
;; title: energy-audit
;; version: 1.0.0
;; summary: Energy Audit Services - Building efficiency system with assessment scheduling, improvement recommendations, contractor referrals, and savings verification
;; description: A comprehensive smart contract for managing energy audits, tracking improvements, and verifying energy savings

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-status (err u105))

;; data vars
(define-data-var next-audit-id uint u1)
(define-data-var next-contractor-id uint u1)
(define-data-var next-recommendation-id uint u1)

;; data maps
(define-map audits
  { audit-id: uint }
  {
    building-owner: principal,
    auditor: principal,
    building-address: (string-ascii 200),
    scheduled-date: uint,
    status: (string-ascii 20), ;; "scheduled", "completed", "cancelled"
    baseline-usage: uint, ;; kWh per month
    audit-score: uint, ;; 0-100
    created-at: uint
  }
)

(define-map recommendations
  { recommendation-id: uint }
  {
    audit-id: uint,
    improvement-type: (string-ascii 100),
    description: (string-ascii 500),
    estimated-cost: uint, ;; in microSTX
    estimated-savings: uint, ;; kWh per month
    priority: (string-ascii 10), ;; "high", "medium", "low"
    implemented: bool,
    created-at: uint
  }
)

(define-map contractors
  { contractor-id: uint }
  {
    contractor-address: principal,
    name: (string-ascii 100),
    specialties: (string-ascii 200),
    rating: uint, ;; 0-100
    completed-jobs: uint,
    verified: bool,
    created-at: uint
  }
)

(define-map savings-verifications
  { audit-id: uint }
  {
    pre-improvement-usage: uint,
    post-improvement-usage: uint,
    verification-date: uint,
    verifier: principal,
    actual-savings: uint,
    verified: bool
  }
)

(define-map audit-payments
  { audit-id: uint }
  {
    amount: uint,
    paid: bool,
    payment-date: uint
  }
)

;; public functions

;; Schedule a new energy audit
(define-public (schedule-audit (building-address (string-ascii 200)) (scheduled-date uint) (auditor principal))
  (let
    (
      (audit-id (var-get next-audit-id))
    )
    (map-set audits
      { audit-id: audit-id }
      {
        building-owner: tx-sender,
        auditor: auditor,
        building-address: building-address,
        scheduled-date: scheduled-date,
        status: "scheduled",
        baseline-usage: u0,
        audit-score: u0,
        created-at: stacks-block-height
      }
    )
    (var-set next-audit-id (+ audit-id u1))
    (ok audit-id)
  )
)

;; Complete an audit with baseline usage and score
(define-public (complete-audit (audit-id uint) (baseline-usage uint) (audit-score uint))
  (let
    (
      (audit (unwrap! (map-get? audits { audit-id: audit-id }) err-not-found))
    )
    (asserts! (is-eq (get auditor audit) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status audit) "scheduled") err-invalid-status)
    (asserts! (<= audit-score u100) err-invalid-amount)
    
    (map-set audits
      { audit-id: audit-id }
      (merge audit {
        status: "completed",
        baseline-usage: baseline-usage,
        audit-score: audit-score
      })
    )
    (ok true)
  )
)

;; Add improvement recommendation
(define-public (add-recommendation
  (audit-id uint)
  (improvement-type (string-ascii 100))
  (description (string-ascii 500))
  (estimated-cost uint)
  (estimated-savings uint)
  (priority (string-ascii 10))
)
  (let
    (
      (recommendation-id (var-get next-recommendation-id))
      (audit (unwrap! (map-get? audits { audit-id: audit-id }) err-not-found))
    )
    (asserts! (is-eq (get auditor audit) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status audit) "completed") err-invalid-status)
    
    (map-set recommendations
      { recommendation-id: recommendation-id }
      {
        audit-id: audit-id,
        improvement-type: improvement-type,
        description: description,
        estimated-cost: estimated-cost,
        estimated-savings: estimated-savings,
        priority: priority,
        implemented: false,
        created-at: stacks-block-height
      }
    )
    (var-set next-recommendation-id (+ recommendation-id u1))
    (ok recommendation-id)
  )
)

;; Register a contractor
(define-public (register-contractor
  (name (string-ascii 100))
  (specialties (string-ascii 200))
)
  (let
    (
      (contractor-id (var-get next-contractor-id))
    )
    (map-set contractors
      { contractor-id: contractor-id }
      {
        contractor-address: tx-sender,
        name: name,
        specialties: specialties,
        rating: u0,
        completed-jobs: u0,
        verified: false,
        created-at: stacks-block-height
      }
    )
    (var-set next-contractor-id (+ contractor-id u1))
    (ok contractor-id)
  )
)

;; Verify contractor (only owner)
(define-public (verify-contractor (contractor-id uint))
  (let
    (
      (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set contractors
      { contractor-id: contractor-id }
      (merge contractor { verified: true })
    )
    (ok true)
  )
)

;; Mark recommendation as implemented
(define-public (mark-recommendation-implemented (recommendation-id uint))
  (let
    (
      (recommendation (unwrap! (map-get? recommendations { recommendation-id: recommendation-id }) err-not-found))
      (audit (unwrap! (map-get? audits { audit-id: (get audit-id recommendation) }) err-not-found))
    )
    (asserts! (is-eq (get building-owner audit) tx-sender) err-unauthorized)
    
    (map-set recommendations
      { recommendation-id: recommendation-id }
      (merge recommendation { implemented: true })
    )
    (ok true)
  )
)

;; Submit savings verification
(define-public (verify-savings
  (audit-id uint)
  (post-improvement-usage uint)
)
  (let
    (
      (audit (unwrap! (map-get? audits { audit-id: audit-id }) err-not-found))
      (pre-usage (get baseline-usage audit))
    )
    (asserts! (is-eq (get auditor audit) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status audit) "completed") err-invalid-status)
    (asserts! (> pre-usage u0) err-invalid-amount)
    
    (map-set savings-verifications
      { audit-id: audit-id }
      {
        pre-improvement-usage: pre-usage,
        post-improvement-usage: post-improvement-usage,
        verification-date: stacks-block-height,
        verifier: tx-sender,
        actual-savings: (if (> pre-usage post-improvement-usage)
                        (- pre-usage post-improvement-usage)
                        u0),
        verified: true
      }
    )
    (ok true)
  )
)

;; Pay for audit
(define-public (pay-for-audit (audit-id uint) (amount uint))
  (let
    (
      (audit (unwrap! (map-get? audits { audit-id: audit-id }) err-not-found))
    )
    (asserts! (is-eq (get building-owner audit) tx-sender) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer STX to auditor
    (try! (stx-transfer? amount tx-sender (get auditor audit)))
    
    (map-set audit-payments
      { audit-id: audit-id }
      {
        amount: amount,
        paid: true,
        payment-date: stacks-block-height
      }
    )
    (ok true)
  )
)

;; read only functions

;; Get audit details
(define-read-only (get-audit (audit-id uint))
  (map-get? audits { audit-id: audit-id })
)

;; Get recommendation details
(define-read-only (get-recommendation (recommendation-id uint))
  (map-get? recommendations { recommendation-id: recommendation-id })
)

;; Get contractor details
(define-read-only (get-contractor (contractor-id uint))
  (map-get? contractors { contractor-id: contractor-id })
)

;; Get savings verification
(define-read-only (get-savings-verification (audit-id uint))
  (map-get? savings-verifications { audit-id: audit-id })
)

;; Get payment details
(define-read-only (get-payment-details (audit-id uint))
  (map-get? audit-payments { audit-id: audit-id })
)

;; Get next audit ID
(define-read-only (get-next-audit-id)
  (var-get next-audit-id)
)

;; Get next contractor ID
(define-read-only (get-next-contractor-id)
  (var-get next-contractor-id)
)

;; Get next recommendation ID
(define-read-only (get-next-recommendation-id)
  (var-get next-recommendation-id)
)

;; Check if contractor is verified
(define-read-only (is-contractor-verified (contractor-id uint))
  (match (map-get? contractors { contractor-id: contractor-id })
    contractor (get verified contractor)
    false
  )
)

;; Calculate potential savings for audit
(define-read-only (get-audit-potential-savings (audit-id uint))
  (let
    (
      (audit (unwrap! (map-get? audits { audit-id: audit-id }) (err u0)))
      (baseline (get baseline-usage audit))
      (score (get audit-score audit))
    )
    (ok (/ (* baseline score) u100))
  )
)
