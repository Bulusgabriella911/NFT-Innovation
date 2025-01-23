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



(define-map staked-nfts principal 
  { staked: bool,
    stake-time: uint })

(define-public (stake-nft)
  (let ((token-owner (map-get? token-owners tx-sender)))
    (asserts! (is-some token-owner) ERR_NOT_AUTHORIZED)
    (map-set staked-nfts tx-sender
      { staked: true,
        stake-time: stacks-block-height })
    (ok true)))



(define-map power-ups principal 
  { speed-boost: uint,
    power-up-count: uint })

(define-public (use-power-up)
  (let ((current-powerups (default-to { speed-boost: u0, power-up-count: u0 }
                          (map-get? power-ups tx-sender))))
    (asserts! (> (get power-up-count current-powerups) u0) (err u102))
    (map-set power-ups tx-sender
      { speed-boost: (+ (get speed-boost current-powerups) u1),
        power-up-count: (- (get power-up-count current-powerups) u1) })
    (ok true)))



(define-map user-interactions principal 
  { total-interactions: uint,
    last-interaction: uint })

(define-public (interact-with-nft (target-user principal))
  (let ((current-interactions (default-to { total-interactions: u0, last-interaction: u0 }
                              (map-get? user-interactions tx-sender))))
    (map-set user-interactions tx-sender
      { total-interactions: (+ (get total-interactions current-interactions) u1),
        last-interaction: stacks-block-height })
    (ok true)))



(define-map nft-traits uint 
  { strength: uint,
    speed: uint,
    wisdom: uint })

(define-public (generate-traits (token-id uint))
  (let ((owner (map-get? token-owners tx-sender)))
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set nft-traits token-id
      { strength: (+ u1 (mod stacks-block-height u10)),
        speed: (+ u1 (mod stacks-block-height u8)),
        wisdom: (+ u1 (mod stacks-block-height u12)) })
    (ok true)))

(define-read-only (get-nft-traits (token-id uint))
  (map-get? nft-traits token-id))
