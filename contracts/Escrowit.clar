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

(define-data-var escrow-counter uint u0)
(define-data-var platform-fee-rate uint u250)

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