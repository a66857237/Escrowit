(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ESCROW_NOT_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_ESCROW_EXPIRED (err u106))
(define-constant ERR_ESCROW_NOT_EXPIRED (err u107))
(define-constant ERR_ALREADY_RELEASED (err u108))
(define-constant ERR_ALREADY_REFUNDED (err u109))
(define-constant ERR_REVIEW_ALREADY_EXISTS (err u110))
(define-constant ERR_INVALID_RATING (err u111))
(define-constant ERR_CANNOT_REVIEW_SELF (err u112))
(define-constant ERR_ESCROW_NOT_COMPLETED (err u113))
(define-constant ERR_NOT_ESCROW_PARTICIPANT (err u114))
(define-constant ERR_REVIEW_PERIOD_EXPIRED (err u115))

(define-data-var escrow-counter uint u0)
(define-data-var platform-fee-rate uint u250)
(define-data-var review-period-blocks uint u1008)

(define-map escrows
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    arbiter: principal,
    amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint,
    description: (string-ascii 256)
  }
)

(define-map escrow-balances
  { escrow-id: uint }
  { balance: uint }
)

(define-map user-escrows
  { user: principal }
  { escrow-ids: (list 100 uint) }
)

(define-map reviews
  { escrow-id: uint, reviewer: principal, reviewee: principal }
  {
    rating: uint,
    comment: (string-ascii 500),
    created-at: uint
  }
)

(define-map user-reputation
  { user: principal }
  {
    total-rating: uint,
    review-count: uint,
    average-rating: uint
  }
)

(define-map escrow-review-status
  { escrow-id: uint }
  {
    buyer-reviewed-seller: bool,
    seller-reviewed-buyer: bool,
    completed-at: uint
  }
)

(define-public (create-escrow (seller principal) (arbiter principal) (amount uint) (duration uint) (description (string-ascii 256)))
  (let
    (
      (escrow-id (+ (var-get escrow-counter) u1))
      (current-block stacks-block-height)
      (expires-at (+ current-block duration))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      {
        buyer: tx-sender,
        seller: seller,
        arbiter: arbiter,
        amount: amount,
        status: "active",
        created-at: current-block,
        expires-at: expires-at,
        description: description
      }
    )
    
    (map-set escrow-balances
      { escrow-id: escrow-id }
      { balance: amount }
    )
    
    (update-user-escrows tx-sender escrow-id)
    (update-user-escrows seller escrow-id)
    (update-user-escrows arbiter escrow-id)
    
    (var-set escrow-counter escrow-id)
    (ok escrow-id)
  )
)

(define-public (release-funds (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (escrow-balance (unwrap! (map-get? escrow-balances { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (amount (get amount escrow-data))
      (platform-fee (/ (* amount (var-get platform-fee-rate)) u10000))
      (seller-amount (- amount platform-fee))
    )
    (asserts! (or 
      (is-eq tx-sender (get buyer escrow-data))
      (is-eq tx-sender (get arbiter escrow-data))
    ) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status escrow-data) "active") ERR_ESCROW_NOT_ACTIVE)
    (asserts! (> (get balance escrow-balance) u0) ERR_ALREADY_RELEASED)
    
    (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller escrow-data))))
    (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow-data { status: "completed" })
    )
    
    (map-set escrow-balances
      { escrow-id: escrow-id }
      { balance: u0 }
    )
    
    (map-set escrow-review-status
      { escrow-id: escrow-id }
      {
        buyer-reviewed-seller: false,
        seller-reviewed-buyer: false,
        completed-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (refund-buyer (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (escrow-balance (unwrap! (map-get? escrow-balances { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (amount (get amount escrow-data))
    )
    (asserts! (or 
      (is-eq tx-sender (get seller escrow-data))
      (is-eq tx-sender (get arbiter escrow-data))
    ) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status escrow-data) "active") ERR_ESCROW_NOT_ACTIVE)
    (asserts! (> (get balance escrow-balance) u0) ERR_ALREADY_REFUNDED)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get buyer escrow-data))))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow-data { status: "refunded" })
    )
    
    (map-set escrow-balances
      { escrow-id: escrow-id }
      { balance: u0 }
    )
    
    (ok true)
  )
)

(define-public (claim-expired-escrow (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (escrow-balance (unwrap! (map-get? escrow-balances { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (amount (get amount escrow-data))
    )
    (asserts! (is-eq tx-sender (get buyer escrow-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status escrow-data) "active") ERR_ESCROW_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get expires-at escrow-data)) ERR_ESCROW_NOT_EXPIRED)
    (asserts! (> (get balance escrow-balance) u0) ERR_ALREADY_REFUNDED)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get buyer escrow-data))))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow-data { status: "expired" })
    )
    
    (map-set escrow-balances
      { escrow-id: escrow-id }
      { balance: u0 }
    )
    
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR_INVALID_AMOUNT)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-private (update-user-escrows (user principal) (escrow-id uint))
  (let
    (
      (current-escrows (default-to { escrow-ids: (list) } (map-get? user-escrows { user: user })))
      (updated-list (unwrap-panic (as-max-len? (append (get escrow-ids current-escrows) escrow-id) u100)))
    )
    (map-set user-escrows
      { user: user }
      { escrow-ids: updated-list }
    )
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-escrow-balance (escrow-id uint))
  (map-get? escrow-balances { escrow-id: escrow-id })
)

(define-read-only (get-user-escrows (user principal))
  (map-get? user-escrows { user: user })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-escrow-counter)
  (var-get escrow-counter)
)

(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (>= stacks-block-height (get expires-at escrow-data))
    false
  )
)

(define-read-only (can-release-funds (escrow-id uint) (caller principal))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (and
      (is-eq (get status escrow-data) "active")
      (or 
        (is-eq caller (get buyer escrow-data))
        (is-eq caller (get arbiter escrow-data))
      )
    )
    false
  )
)

(define-read-only (can-refund (escrow-id uint) (caller principal))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (and
      (is-eq (get status escrow-data) "active")
      (or 
        (is-eq caller (get seller escrow-data))
        (is-eq caller (get arbiter escrow-data))
      )
    )
    false
  )
)

(define-public (submit-review (escrow-id uint) (reviewee principal) (rating uint) (comment (string-ascii 500)))
  (let
    (
      (escrow-data (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (review-status (unwrap! (map-get? escrow-review-status { escrow-id: escrow-id }) ERR_ESCROW_NOT_COMPLETED))
      (review-deadline (+ (get completed-at review-status) (var-get review-period-blocks)))
    )
    (asserts! (is-eq (get status escrow-data) "completed") ERR_ESCROW_NOT_COMPLETED)
    (asserts! (<= stacks-block-height review-deadline) ERR_REVIEW_PERIOD_EXPIRED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (not (is-eq tx-sender reviewee)) ERR_CANNOT_REVIEW_SELF)
    (asserts! (or 
      (is-eq tx-sender (get buyer escrow-data))
      (is-eq tx-sender (get seller escrow-data))
    ) ERR_NOT_ESCROW_PARTICIPANT)
    (asserts! (or 
      (is-eq reviewee (get buyer escrow-data))
      (is-eq reviewee (get seller escrow-data))
    ) ERR_NOT_ESCROW_PARTICIPANT)
    (asserts! (is-none (map-get? reviews { escrow-id: escrow-id, reviewer: tx-sender, reviewee: reviewee })) ERR_REVIEW_ALREADY_EXISTS)
    
    (map-set reviews
      { escrow-id: escrow-id, reviewer: tx-sender, reviewee: reviewee }
      {
        rating: rating,
        comment: comment,
        created-at: stacks-block-height
      }
    )
    
    (update-user-reputation reviewee rating)
    (update-review-status escrow-id tx-sender (get buyer escrow-data) (get seller escrow-data))
    
    (ok true)
  )
)

(define-private (update-user-reputation (user principal) (new-rating uint))
  (let
    (
      (current-rep (default-to { total-rating: u0, review-count: u0, average-rating: u0 } (map-get? user-reputation { user: user })))
      (new-total-rating (+ (get total-rating current-rep) new-rating))
      (new-review-count (+ (get review-count current-rep) u1))
      (new-average-rating (/ new-total-rating new-review-count))
    )
    (map-set user-reputation
      { user: user }
      {
        total-rating: new-total-rating,
        review-count: new-review-count,
        average-rating: new-average-rating
      }
    )
  )
)

(define-private (update-review-status (escrow-id uint) (reviewer principal) (buyer principal) (seller principal))
  (let
    (
      (current-status (unwrap-panic (map-get? escrow-review-status { escrow-id: escrow-id })))
      (buyer-reviewed-seller (if (is-eq reviewer buyer) true (get buyer-reviewed-seller current-status)))
      (seller-reviewed-buyer (if (is-eq reviewer seller) true (get seller-reviewed-buyer current-status)))
    )
    (map-set escrow-review-status
      { escrow-id: escrow-id }
      (merge current-status {
        buyer-reviewed-seller: buyer-reviewed-seller,
        seller-reviewed-buyer: seller-reviewed-buyer
      })
    )
  )
)

(define-public (bulk-submit-reviews (reviews-data (list 10 { escrow-id: uint, reviewee: principal, rating: uint, comment: (string-ascii 500) })))
  (ok (map submit-single-review reviews-data))
)

(define-private (submit-single-review (review-data { escrow-id: uint, reviewee: principal, rating: uint, comment: (string-ascii 500) }))
  (submit-review 
    (get escrow-id review-data)
    (get reviewee review-data)
    (get rating review-data)
    (get comment review-data)
  )
)

(define-public (update-review-period (new-period-blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= new-period-blocks u144) (<= new-period-blocks u14400)) ERR_INVALID_AMOUNT)
    (var-set review-period-blocks new-period-blocks)
    (ok true)
  )
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-user-average-rating (user principal))
  (match (map-get? user-reputation { user: user })
    rep (get average-rating rep)
    u0
  )
)

(define-read-only (get-user-review-count (user principal))
  (match (map-get? user-reputation { user: user })
    rep (get review-count rep)
    u0
  )
)

(define-read-only (get-review (escrow-id uint) (reviewer principal) (reviewee principal))
  (map-get? reviews { escrow-id: escrow-id, reviewer: reviewer, reviewee: reviewee })
)

(define-read-only (get-escrow-reviews (escrow-id uint) (user principal))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (let
      (
        (buyer (get buyer escrow-data))
        (seller (get seller escrow-data))
      )
      (if (is-eq user buyer)
        (map-get? reviews { escrow-id: escrow-id, reviewer: seller, reviewee: buyer })
        (if (is-eq user seller)
          (map-get? reviews { escrow-id: escrow-id, reviewer: buyer, reviewee: seller })
          none
        )
      )
    )
    none
  )
)

(define-read-only (get-review-status (escrow-id uint))
  (map-get? escrow-review-status { escrow-id: escrow-id })
)

(define-read-only (get-review-period)
  (var-get review-period-blocks)
)

(define-read-only (can-submit-review (escrow-id uint) (reviewer principal) (reviewee principal))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (match (map-get? escrow-review-status { escrow-id: escrow-id })
      review-status (let
        (
          (review-deadline (+ (get completed-at review-status) (var-get review-period-blocks)))
        )
        (and
          (is-eq (get status escrow-data) "completed")
          (<= stacks-block-height review-deadline)
          (not (is-eq reviewer reviewee))
          (or 
            (is-eq reviewer (get buyer escrow-data))
            (is-eq reviewer (get seller escrow-data))
          )
          (or 
            (is-eq reviewee (get buyer escrow-data))
            (is-eq reviewee (get seller escrow-data))
          )
          (is-none (map-get? reviews { escrow-id: escrow-id, reviewer: reviewer, reviewee: reviewee }))
        )
      )
      false
    )
    false
  )
)

(define-read-only (get-user-trust-score (user principal))
  (let
    (
      (reputation (default-to { total-rating: u0, review-count: u0, average-rating: u0 } (map-get? user-reputation { user: user })))
      (review-count (get review-count reputation))
      (average-rating (get average-rating reputation))
    )
    (if (is-eq review-count u0)
      u0
      (let
        (
          (base-score (* average-rating u20))
          (volume-bonus (if (>= review-count u5) u10 (/ (* review-count u2) u1)))
          (consistency-bonus (if (>= average-rating u4) u5 u0))
        )
        (+ base-score volume-bonus consistency-bonus)
      )
    )
  )
)