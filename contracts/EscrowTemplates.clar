;; Escrow Templates Contract
;; Allows users to create and manage reusable escrow configurations

(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_TEMPLATE_NOT_FOUND (err u201))
(define-constant ERR_TEMPLATE_NAME_EXISTS (err u202))
(define-constant ERR_INVALID_TEMPLATE_DATA (err u203))
(define-constant ERR_MAX_TEMPLATES_REACHED (err u204))
(define-constant ERR_TEMPLATE_NOT_OWNER (err u205))

;; Data variables
(define-data-var template-counter uint u0)

;; Template storage
(define-map escrow-templates
  { template-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    default-arbiter: (optional principal),
    suggested-amount: (optional uint),
    default-duration: uint,
    category: (string-ascii 30),
    description-template: (string-ascii 200),
    is-public: bool,
    created-at: uint,
    usage-count: uint
  }
)

;; User templates tracking
(define-map user-templates
  { owner: principal }
  { template-ids: (list 20 uint) }
)

;; Template name registry for uniqueness per user
(define-map user-template-names
  { owner: principal, name: (string-ascii 50) }
  { template-id: uint }
)

;; Public templates registry
(define-map public-templates-by-category
  { category: (string-ascii 30) }
  { template-ids: (list 50 uint) }
)

;; Create a new escrow template
(define-public (create-template 
  (name (string-ascii 50))
  (default-arbiter (optional principal))
  (suggested-amount (optional uint))
  (default-duration uint)
  (category (string-ascii 30))
  (description-template (string-ascii 200))
  (is-public bool))
  (let
    (
      (template-id (+ (var-get template-counter) u1))
      (current-user-templates (default-to { template-ids: (list) } (map-get? user-templates { owner: tx-sender })))
    )
    ;; Validation checks
    (asserts! (> default-duration u0) ERR_INVALID_TEMPLATE_DATA)
    (asserts! (< (len (get template-ids current-user-templates)) u20) ERR_MAX_TEMPLATES_REACHED)
    (asserts! (is-none (map-get? user-template-names { owner: tx-sender, name: name })) ERR_TEMPLATE_NAME_EXISTS)
    
    ;; Create template
    (map-set escrow-templates
      { template-id: template-id }
      {
        owner: tx-sender,
        name: name,
        default-arbiter: default-arbiter,
        suggested-amount: suggested-amount,
        default-duration: default-duration,
        category: category,
        description-template: description-template,
        is-public: is-public,
        created-at: stacks-block-height,
        usage-count: u0
      }
    )
    
    ;; Update user templates list
    (map-set user-templates
      { owner: tx-sender }
      { template-ids: (unwrap! (as-max-len? (append (get template-ids current-user-templates) template-id) u20) ERR_MAX_TEMPLATES_REACHED) }
    )
    
    ;; Register template name for uniqueness
    (map-set user-template-names
      { owner: tx-sender, name: name }
      { template-id: template-id }
    )
    
    ;; Add to public templates if public
    (if is-public
      (add-to-public-category template-id category)
      true
    )
    
    (var-set template-counter template-id)
    (ok template-id)
  )
)

;; Update an existing template
(define-public (update-template
  (template-id uint)
  (name (string-ascii 50))
  (default-arbiter (optional principal))
  (suggested-amount (optional uint))
  (default-duration uint)
  (category (string-ascii 30))
  (description-template (string-ascii 200))
  (is-public bool))
  (let
    (
      (template-data (unwrap! (map-get? escrow-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
      (old-category (get category template-data))
      (old-is-public (get is-public template-data))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner template-data)) ERR_TEMPLATE_NOT_OWNER)
    (asserts! (> default-duration u0) ERR_INVALID_TEMPLATE_DATA)
    
    ;; Update template
    (map-set escrow-templates
      { template-id: template-id }
      (merge template-data {
        name: name,
        default-arbiter: default-arbiter,
        suggested-amount: suggested-amount,
        default-duration: default-duration,
        category: category,
        description-template: description-template,
        is-public: is-public
      })
    )
    
    ;; Handle public/private visibility changes
    (if (and (not old-is-public) is-public)
      (add-to-public-category template-id category)
      (if (and old-is-public (not is-public))
        (remove-from-public-category template-id old-category)
        (if (and old-is-public is-public (not (is-eq old-category category)))
          (begin
            (remove-from-public-category template-id old-category)
            (add-to-public-category template-id category)
          )
          true
        )
      )
    )
    
    (ok true)
  )
)

;; Use template to create escrow via main contract
(define-public (create-escrow-from-template
  (template-id uint)
  (seller principal)
  (custom-arbiter (optional principal))
  (custom-amount (optional uint))
  (custom-description (optional (string-ascii 256))))
  (let
    (
      (template-data (unwrap! (map-get? escrow-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
      (arbiter (default-to (unwrap! (get default-arbiter template-data) ERR_INVALID_TEMPLATE_DATA) custom-arbiter))
      (amount (default-to (unwrap! (get suggested-amount template-data) ERR_INVALID_TEMPLATE_DATA) custom-amount))
      (description (default-to (get description-template template-data) custom-description))
    )
    ;; Validate template access (owner or public)
    (asserts! (or 
      (is-eq tx-sender (get owner template-data))
      (get is-public template-data)
    ) ERR_UNAUTHORIZED)
    
    ;; Increment usage count
    (map-set escrow-templates
      { template-id: template-id }
      (merge template-data { usage-count: (+ (get usage-count template-data) u1) })
    )
    
    ;; Call main escrow contract (would need to be imported/integrated)
    ;; For now, return the parameters that would be used
    (ok {
      seller: seller,
      arbiter: arbiter,
      amount: amount,
      duration: (get default-duration template-data),
      description: description
    })
  )
)

;; Delete a template
(define-public (delete-template (template-id uint))
  (let
    (
      (template-data (unwrap! (map-get? escrow-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner template-data)) ERR_TEMPLATE_NOT_OWNER)
    
    ;; Remove from public category if public
    (if (get is-public template-data)
      (remove-from-public-category template-id (get category template-data))
      true
    )
    
    ;; Remove template name registration
    (map-delete user-template-names { owner: tx-sender, name: (get name template-data) })
    
    ;; Delete template
    (map-delete escrow-templates { template-id: template-id })
    
    (ok true)
  )
)

;; Private helper to add template to public category
(define-private (add-to-public-category (template-id uint) (category (string-ascii 30)))
  (let
    (
      (current-templates (default-to { template-ids: (list) } (map-get? public-templates-by-category { category: category })))
    )
    (map-set public-templates-by-category
      { category: category }
      { template-ids: (unwrap! (as-max-len? (append (get template-ids current-templates) template-id) u50) false) }
    )
  )
)

;; Private helper to remove template from public category
(define-private (remove-from-public-category (template-id uint) (category (string-ascii 30)))
  (match (map-get? public-templates-by-category { category: category })
    category-data (map-set public-templates-by-category
      { category: category }
      { template-ids: (filter remove-template-id (get template-ids category-data)) }
    )
    true
  )
)

;; Helper function for filtering
(define-private (remove-template-id (id uint))
  (not (is-eq id (var-get template-counter))) ;; This is a placeholder - would need the actual target ID
)

;; Read-only functions
(define-read-only (get-template (template-id uint))
  (map-get? escrow-templates { template-id: template-id })
)

(define-read-only (get-user-templates (owner principal))
  (map-get? user-templates { owner: owner })
)

(define-read-only (get-public-templates-by-category (category (string-ascii 30)))
  (map-get? public-templates-by-category { category: category })
)

(define-read-only (get-template-counter)
  (var-get template-counter)
)

(define-read-only (can-use-template (template-id uint) (caller principal))
  (match (map-get? escrow-templates { template-id: template-id })
    template-data (or 
      (is-eq caller (get owner template-data))
      (get is-public template-data)
    )
    false
  )
)
