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
(define-constant ERR_INVALID_FEE_TIER (err u116))
(define-constant ERR_INVALID_DISCOUNT (err u117))
(define-constant ERR_FEE_CONFIGURATION_ERROR (err u118))

(define-data-var escrow-counter uint u0)
(define-data-var platform-fee-rate uint u250)
(define-data-var review-period-blocks uint u1008)
(define-data-var base-fee-rate uint u250)
(define-data-var reputation-discount-enabled bool true)
(define-data-var volume-discount-enabled bool true)
(define-data-var loyalty-bonus-enabled bool true)

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

(define-map user-fee-stats
  { user: principal }
  {
    total-volume: uint,
    escrow-count: uint,
    first-escrow-block: uint,
    last-escrow-block: uint,
    lifetime-fees-paid: uint
  }
)

(define-map fee-tier-configs
  { tier-id: uint }
  {
    name: (string-ascii 50),
    min-volume: uint,
    min-escrows: uint,
    min-reputation: uint,
    fee-discount-bps: uint,
    active: bool
  }
)

(define-map user-custom-discounts
  { user: principal }
  {
    discount-bps: uint,
    expires-at: uint,
    reason: (string-ascii 100)
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
    (update-user-fee-stats tx-sender amount)
    
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
      (buyer (get buyer escrow-data))
      (dynamic-fee-rate (calculate-dynamic-fee-rate buyer amount))
      (platform-fee (/ (* amount dynamic-fee-rate) u10000))
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
    (update-user-fee-payment buyer platform-fee)
    
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

(define-private (update-user-fee-stats (user principal) (amount uint))
  (let
    (
      (current-stats (default-to 
        { total-volume: u0, escrow-count: u0, first-escrow-block: stacks-block-height, last-escrow-block: stacks-block-height, lifetime-fees-paid: u0 }
        (map-get? user-fee-stats { user: user })
      ))
      (new-total-volume (+ (get total-volume current-stats) amount))
      (new-escrow-count (+ (get escrow-count current-stats) u1))
      (first-block (if (is-eq (get escrow-count current-stats) u0) stacks-block-height (get first-escrow-block current-stats)))
    )
    (map-set user-fee-stats
      { user: user }
      {
        total-volume: new-total-volume,
        escrow-count: new-escrow-count,
        first-escrow-block: first-block,
        last-escrow-block: stacks-block-height,
        lifetime-fees-paid: (get lifetime-fees-paid current-stats)
      }
    )
  )
)

(define-private (update-user-fee-payment (user principal) (fee-amount uint))
  (let
    (
      (current-stats (default-to 
        { total-volume: u0, escrow-count: u0, first-escrow-block: stacks-block-height, last-escrow-block: stacks-block-height, lifetime-fees-paid: u0 }
        (map-get? user-fee-stats { user: user })
      ))
      (new-lifetime-fees (+ (get lifetime-fees-paid current-stats) fee-amount))
    )
    (map-set user-fee-stats
      { user: user }
      (merge current-stats { lifetime-fees-paid: new-lifetime-fees })
    )
  )
)

(define-private (calculate-dynamic-fee-rate (user principal) (amount uint))
  (let
    (
      (base-rate (var-get base-fee-rate))
      (reputation-discount (if (var-get reputation-discount-enabled) (calculate-reputation-discount user) u0))
      (volume-discount (if (var-get volume-discount-enabled) (calculate-volume-discount user) u0))
      (loyalty-discount (if (var-get loyalty-bonus-enabled) (calculate-loyalty-discount user) u0))
      (custom-discount (calculate-custom-discount user))
      (tier-discount (calculate-tier-discount user))
      (total-discount (+ reputation-discount volume-discount loyalty-discount custom-discount tier-discount))
      (final-discount (if (> total-discount u200) u200 total-discount))
    )
    (if (> final-discount base-rate)
      u50
      (- base-rate final-discount)
    )
  )
)

(define-private (calculate-reputation-discount (user principal))
  (let
    (
      (user-rating (get-user-average-rating user))
      (review-count (get-user-review-count user))
    )
    (if (and (>= user-rating u4) (>= review-count u3))
      (if (is-eq user-rating u5)
        u50
        (if (>= user-rating u4)
          u25
          u0
        )
      )
      u0
    )
  )
)

(define-private (calculate-volume-discount (user principal))
  (match (map-get? user-fee-stats { user: user })
    stats (let
      (
        (total-volume (get total-volume stats))
        (escrow-count (get escrow-count stats))
      )
      (if (>= total-volume u100000000)
        u40
        (if (>= total-volume u50000000)
          u30
          (if (>= total-volume u10000000)
            u20
            (if (>= escrow-count u10)
              u10
              u0
            )
          )
        )
      )
    )
    u0
  )
)

(define-private (calculate-loyalty-discount (user principal))
  (match (map-get? user-fee-stats { user: user })
    stats (let
      (
        (blocks-since-first (- stacks-block-height (get first-escrow-block stats)))
        (escrow-count (get escrow-count stats))
      )
      (if (and (>= blocks-since-first u14400) (>= escrow-count u5))
        u15
        (if (and (>= blocks-since-first u7200) (>= escrow-count u3))
          u10
          u0
        )
      )
    )
    u0
  )
)

(define-private (calculate-custom-discount (user principal))
  (match (map-get? user-custom-discounts { user: user })
    discount-info (if (> (get expires-at discount-info) stacks-block-height)
      (get discount-bps discount-info)
      u0
    )
    u0
  )
)

(define-private (calculate-tier-discount (user principal))
  (let
    (
      (user-volume (match (map-get? user-fee-stats { user: user }) stats (get total-volume stats) u0))
      (user-escrow-count (match (map-get? user-fee-stats { user: user }) stats (get escrow-count stats) u0))
      (user-rating (get-user-average-rating user))
    )
    (if (and (>= user-volume u100000000) (>= user-escrow-count u20) (>= user-rating u4))
      u60
      (if (and (>= user-volume u50000000) (>= user-escrow-count u10) (>= user-rating u4))
        u40
        (if (and (>= user-volume u10000000) (>= user-escrow-count u5) (>= user-rating u3))
          u20
          u0
        )
      )
    )
  )
)

(define-public (create-fee-tier (tier-id uint) (name (string-ascii 50)) (min-volume uint) (min-escrows uint) (min-reputation uint) (fee-discount-bps uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= fee-discount-bps u200) ERR_INVALID_DISCOUNT)
    (asserts! (is-none (map-get? fee-tier-configs { tier-id: tier-id })) ERR_INVALID_FEE_TIER)
    
    (map-set fee-tier-configs
      { tier-id: tier-id }
      {
        name: name,
        min-volume: min-volume,
        min-escrows: min-escrows,
        min-reputation: min-reputation,
        fee-discount-bps: fee-discount-bps,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (set-custom-discount (user principal) (discount-bps uint) (duration-blocks uint) (reason (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= discount-bps u200) ERR_INVALID_DISCOUNT)
    
    (map-set user-custom-discounts
      { user: user }
      {
        discount-bps: discount-bps,
        expires-at: (+ stacks-block-height duration-blocks),
        reason: reason
      }
    )
    (ok true)
  )
)

(define-public (toggle-discount-system (reputation-enabled bool) (volume-enabled bool) (loyalty-enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set reputation-discount-enabled reputation-enabled)
    (var-set volume-discount-enabled volume-enabled)
    (var-set loyalty-bonus-enabled loyalty-enabled)
    (ok true)
  )
)

(define-public (update-base-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT)
    (var-set base-fee-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-user-fee-stats (user principal))
  (map-get? user-fee-stats { user: user })
)

(define-read-only (get-user-effective-fee-rate (user principal) (amount uint))
  (calculate-dynamic-fee-rate user amount)
)

(define-read-only (get-fee-breakdown (user principal) (amount uint))
  (let
    (
      (base-rate (var-get base-fee-rate))
      (reputation-discount (if (var-get reputation-discount-enabled) (calculate-reputation-discount user) u0))
      (volume-discount (if (var-get volume-discount-enabled) (calculate-volume-discount user) u0))
      (loyalty-discount (if (var-get loyalty-bonus-enabled) (calculate-loyalty-discount user) u0))
      (custom-discount (calculate-custom-discount user))
      (tier-discount (calculate-tier-discount user))
    )
    {
      base-rate: base-rate,
      reputation-discount: reputation-discount,
      volume-discount: volume-discount,
      loyalty-discount: loyalty-discount,
      custom-discount: custom-discount,
      tier-discount: tier-discount,
      final-rate: (calculate-dynamic-fee-rate user amount)
    }
  )
)

(define-read-only (get-fee-tier (tier-id uint))
  (map-get? fee-tier-configs { tier-id: tier-id })
)

(define-read-only (get-user-custom-discount (user principal))
  (map-get? user-custom-discounts { user: user })
)

(define-read-only (get-discount-settings)
  {
    reputation-enabled: (var-get reputation-discount-enabled),
    volume-enabled: (var-get volume-discount-enabled),
    loyalty-enabled: (var-get loyalty-bonus-enabled),
    base-rate: (var-get base-fee-rate)
  }
)

(define-read-only (calculate-fee-for-amount (user principal) (amount uint))
  (let
    (
      (effective-rate (calculate-dynamic-fee-rate user amount))
    )
    (/ (* amount effective-rate) u10000)
  )
)

