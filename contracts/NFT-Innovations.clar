;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NFT_EXISTS (err u101))

;; Data vars
(define-data-var last-token-id uint u0)

;; Data maps
(define-map token-owners principal uint)
(define-map activity-levels principal uint)
(define-map evolution-stages principal uint)

;; NFT mint function
(define-public (mint-subscription)
  (let ((token-id (+ (var-get last-token-id) u1)))
    (asserts! (is-none (map-get? token-owners tx-sender)) ERR_NFT_EXISTS)
    (map-set token-owners tx-sender token-id)
    (map-set activity-levels tx-sender u1)
    (map-set evolution-stages tx-sender u1)
    (var-set last-token-id token-id)
    (ok token-id)))

;; Record wallet activity
(define-public (record-activity)
  (let ((current-level (default-to u0 (map-get? activity-levels tx-sender))))
    (map-set activity-levels tx-sender (+ current-level u1))
    ;; (try! (evolve-nft))
    (ok true)))

;; Internal function to evolve NFT
(define-private (evolve-nft)
  (let (
    (activity (default-to u0 (map-get? activity-levels tx-sender)))
    (current-stage (default-to u1 (map-get? evolution-stages tx-sender)))
  )
    (if (and (>= activity (* current-stage u5)) (< current-stage u5))
      (begin
        (map-set evolution-stages tx-sender (+ current-stage u1))
        (ok true))
      (ok false))))

;; Read-only functions
(define-read-only (get-activity-level (owner principal))
  (default-to u0 (map-get? activity-levels owner)))

(define-read-only (get-evolution-stage (owner principal))
  (default-to u0 (map-get? evolution-stages owner)))
