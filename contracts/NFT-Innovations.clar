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


;; New breeding maps
(define-map breeding-pairs 
  { parent1: uint, parent2: uint }
  { child: uint, breed-time: uint })

(define-map breeding-cooldowns principal uint)

;; Breeding function
(define-public (breed-nfts (parent1-id uint) (parent2-id uint))
  (let (
    (token-id (+ (var-get last-token-id) u1))
    (cooldown (default-to u0 (map-get? breeding-cooldowns tx-sender)))
  )
    (asserts! (> stacks-block-height (+ cooldown u144)) (err u103))
    (map-set breeding-pairs { parent1: parent1-id, parent2: parent2-id }
      { child: token-id, breed-time: stacks-block-height })
    (map-set breeding-cooldowns tx-sender stacks-block-height)
    (var-set last-token-id token-id)
    (ok token-id)))



;; Achievement tracking
(define-map user-achievements principal 
  { total-mints: uint,
    evolution-count: uint,
    interaction-score: uint })

(define-public (unlock-achievement (achievement-type uint))
  (let (
    (current-achievements (default-to { total-mints: u0, evolution-count: u0, interaction-score: u0 }
                          (map-get? user-achievements tx-sender)))
  )
    (map-set user-achievements tx-sender
      (if (is-eq achievement-type u1)
        { total-mints: (+ (get total-mints current-achievements) u1),
          evolution-count: (get evolution-count current-achievements),
          interaction-score: (get interaction-score current-achievements) }
        (if (is-eq achievement-type u2)
          { total-mints: (get total-mints current-achievements),
            evolution-count: (+ (get evolution-count current-achievements) u1),
            interaction-score: (get interaction-score current-achievements) }
          { total-mints: (get total-mints current-achievements),
            evolution-count: (get evolution-count current-achievements),
            interaction-score: (+ (get interaction-score current-achievements) u1) })))
    (ok true)))



;; Trading functionality
(define-map trade-offers 
  { seller: principal, token-id: uint }
  { price: uint, active: bool })

(define-public (create-trade-offer (token-id uint) (price uint))
  (let ((owner (map-get? token-owners tx-sender)))
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set trade-offers { seller: tx-sender, token-id: token-id }
      { price: price, active: true })
    (ok true)))

(define-public (accept-trade-offer (seller principal) (token-id uint))
  (let ((offer (map-get? trade-offers { seller: seller, token-id: token-id })))
    (asserts! (is-some offer) (err u104))
    (asserts! (get active (unwrap-panic offer)) (err u105))
    ;; Transfer logic would go here
    (map-set trade-offers { seller: seller, token-id: token-id }
      { price: (get price (unwrap-panic offer)), active: false })
    (ok true)))



;; Element system
(define-map nft-elements uint 
  { primary: uint,
    secondary: uint,
    elemental-power: uint })

(define-public (assign-elements (token-id uint))
  (let (
    (owner (map-get? token-owners tx-sender))
    (block-seed (mod stacks-block-height u5))
  )
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set nft-elements token-id
      { primary: block-seed,
        secondary: (mod (+ block-seed u2) u5),
        elemental-power: (+ u5 (mod stacks-block-height u15)) })
    (ok true)))

(define-read-only (get-nft-elements (token-id uint))
  (map-get? nft-elements token-id))



;; Quest system tracking
(define-map active-quests principal 
  { quest-id: uint,
    progress: uint,
    target: uint,
    reward-claimed: bool })

(define-public (start-quest (quest-id uint))
  (let (
    (current-quest (default-to { quest-id: u0, progress: u0, target: u50, reward-claimed: false }
                    (map-get? active-quests tx-sender)))
  )
    (map-set active-quests tx-sender
      { quest-id: quest-id,
        progress: u0,
        target: u50,
        reward-claimed: false })
    (ok true)))

(define-public (claim-quest-reward)
  (let (
    (quest-data (default-to { quest-id: u0, progress: u0, target: u50, reward-claimed: false }
                 (map-get? active-quests tx-sender)))
  )
    (asserts! (>= (get progress quest-data) (get target quest-data)) (err u106))
    (asserts! (not (get reward-claimed quest-data)) (err u107))
    (map-set active-quests tx-sender
      (merge quest-data { reward-claimed: true }))
    (ok true)))




;; Fusion system maps
(define-map fused-nfts uint 
  { base-nft: uint,
    catalyst-nft: uint,
    fusion-power: uint })

(define-public (fuse-nfts (base-id uint) (catalyst-id uint))
  (let (
    (token-id (+ (var-get last-token-id) u1))
    (base-owner (map-get? token-owners tx-sender))
  )
    (asserts! (is-some base-owner) ERR_NOT_AUTHORIZED)
    (map-set fused-nfts token-id
      { base-nft: base-id,
        catalyst-nft: catalyst-id,
        fusion-power: (+ u10 (mod stacks-block-height u20)) })
    (var-set last-token-id token-id)
    (ok token-id)))


;; Rarity System
(define-map nft-rarity uint 
  { rarity-level: (string-ascii 20),
    rarity-score: uint,
    bonus-multiplier: uint })

(define-public (set-nft-rarity (token-id uint))
  (let ((random-score (mod stacks-block-height u100)))
    (map-set nft-rarity token-id
      (if (< random-score u10)
        { rarity-level: "Legendary", rarity-score: u100, bonus-multiplier: u5 }
        (if (< random-score u30)
          { rarity-level: "Rare", rarity-score: u75, bonus-multiplier: u3 }
          { rarity-level: "Common", rarity-score: u50, bonus-multiplier: u1 })))
    (ok true)))

(define-read-only (get-nft-rarity (token-id uint))
  (map-get? nft-rarity token-id))


(define-map daily-rewards principal 
  { last-claim: uint,
    streak: uint })

(define-public (claim-daily-reward)
  (let (
    (current-data (default-to { last-claim: u0, streak: u0 } 
                   (map-get? daily-rewards tx-sender)))
    (current-height stacks-block-height)
  )
    (asserts! (> current-height (+ (get last-claim current-data) u144)) (err u110))
    (map-set daily-rewards tx-sender
      { last-claim: current-height,
        streak: (+ (get streak current-data) u1) })
    (ok true)))


(define-map crafting-recipes uint 
  { required-items: (list 3 uint),
    result-item: uint })

(define-map player-inventory principal 
  { materials: (list 10 uint),
    crafted-items: (list 5 uint) })

(define-public (craft-item (recipe-id uint))
  (let (
    (recipe (unwrap! (map-get? crafting-recipes recipe-id) (err u111)))
    (inventory (default-to { materials: (list ), crafted-items: (list ) }
                (map-get? player-inventory tx-sender)))
  )
    (map-set player-inventory tx-sender
      { materials: (get materials inventory),
        crafted-items: (unwrap-panic (as-max-len? 
          (append (get crafted-items inventory) (get result-item recipe)) u5)) })
    (ok true)))



(define-map battle-stats uint 
  { attack: uint,
    defense: uint,
    wins: uint,
    losses: uint })

(define-public (initiate-battle (attacker-id uint) (defender-id uint))
  (let (
    (attacker-stats (default-to { attack: u0, defense: u0, wins: u0, losses: u0 }
                     (map-get? battle-stats attacker-id)))
    (defender-stats (default-to { attack: u0, defense: u0, wins: u0, losses: u0 }
                     (map-get? battle-stats defender-id)))
  )
    (if (> (get attack attacker-stats) (get defense defender-stats))
      (map-set battle-stats attacker-id 
        (merge attacker-stats { wins: (+ (get wins attacker-stats) u1) }))
      (map-set battle-stats attacker-id 
        (merge attacker-stats { losses: (+ (get losses attacker-stats) u1) })))
    (ok true)))


(define-map marketplace 
  { token-id: uint }
  { price: uint,
    seller: principal,
    is-listed: bool })

(define-public (list-nft (token-id uint) (price uint))
  (let ((owner (map-get? token-owners tx-sender)))
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set marketplace { token-id: token-id }
      { price: price,
        seller: tx-sender,
        is-listed: true })
    (ok true)))

(define-read-only (get-listing (token-id uint))
  (map-get? marketplace { token-id: token-id }))


(define-map nft-rentals uint 
  { renter: (optional principal),
    rental-end: uint,
    price-per-block: uint })

(define-public (rent-nft (token-id uint) (duration uint))
  (let (
    (rental-info (default-to { renter: none, rental-end: u0, price-per-block: u0 }
                  (map-get? nft-rentals token-id)))
  )
    (asserts! (is-none (get renter rental-info)) (err u112))
    (map-set nft-rentals token-id
      { renter: (some tx-sender),
        rental-end: (+ stacks-block-height duration),
        price-per-block: u10 })
    (ok true)))
