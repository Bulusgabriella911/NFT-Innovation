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


(define-map enchantments uint 
  { enchant-type: (string-ascii 20),
    power-boost: uint,
    durability: uint })

(define-public (enchant-nft (token-id uint) (enchant-type (string-ascii 20)))
  (let (
    (current-block stacks-block-height)
    (power-boost (+ u5 (mod current-block u10)))
  )
    (map-set enchantments token-id
      { enchant-type: enchant-type,
        power-boost: power-boost,
        durability: u100 })
    (ok true)))

(define-read-only (get-enchantment (token-id uint))
  (map-get? enchantments token-id))



(define-map nft-metadata uint 
  { name: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256) })

(define-public (set-nft-metadata (token-id uint) (name (string-ascii 64)) (description (string-ascii 256)) (image-uri (string-ascii 256)))
  (let ((owner (map-get? token-owners tx-sender)))
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set nft-metadata token-id
      { name: name,
        description: description,
        image-uri: image-uri })
    (ok true)))

(define-read-only (get-nft-metadata (token-id uint))
  (map-get? nft-metadata token-id))



(define-map seasonal-events 
  { event-id: uint }
  { name: (string-ascii 64),
    start-block: uint,
    end-block: uint,
    active: bool,
    reward-multiplier: uint })

(define-map event-participation principal 
  { event-id: uint,
    participation-count: uint,
    rewards-earned: uint })

(define-public (create-seasonal-event (event-id uint) (name (string-ascii 64)) (duration uint) (reward-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set seasonal-events { event-id: event-id }
      { name: name,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        active: true,
        reward-multiplier: reward-multiplier })
    (ok true)))

(define-public (participate-in-event (event-id uint))
  (let (
    (event (unwrap! (map-get? seasonal-events { event-id: event-id }) (err u120)))
    (current-participation (default-to { event-id: event-id, participation-count: u0, rewards-earned: u0 }
                           (map-get? event-participation tx-sender)))
  )
    (asserts! (get active event) (err u121))
    (asserts! (<= stacks-block-height (get end-block event)) (err u122))
    (map-set event-participation tx-sender
      { event-id: event-id,
        participation-count: (+ (get participation-count current-participation) u1),
        rewards-earned: (+ (get rewards-earned current-participation) (get reward-multiplier event)) })
    (ok true)))

(define-read-only (get-active-events)
  (ok true))







(define-map nft-levels uint 
  { level: uint,
    experience: uint,
    level-cap: uint })

(define-constant XP_PER_LEVEL u100)

(define-public (gain-experience (token-id uint) (xp-amount uint))
  (let (
    (owner (map-get? token-owners tx-sender))
    (current-levels (default-to { level: u1, experience: u0, level-cap: u50 }
                    (map-get? nft-levels token-id)))
    (new-xp (+ (get experience current-levels) xp-amount))
    (new-level (+ (get level current-levels) (/ new-xp XP_PER_LEVEL)))
    (remaining-xp (mod new-xp XP_PER_LEVEL))
  )
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set nft-levels token-id
      { level: (if (> new-level (get level-cap current-levels))
                 (get level-cap current-levels)
                 new-level),
        experience: remaining-xp,
        level-cap: (get level-cap current-levels) })
    (ok new-level)))

(define-read-only (get-nft-level (token-id uint))
  (default-to { level: u1, experience: u0, level-cap: u50 }
    (map-get? nft-levels token-id)))


(define-map power-scaling uint 
  { base-power: uint,
    holding-start: uint,
    power-multiplier: uint })

(define-constant POWER_SCALE_FACTOR u10)
(define-constant MAX_POWER_MULTIPLIER u5)

(define-public (initialize-power-scaling (token-id uint))
  (let ((owner (map-get? token-owners tx-sender)))
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (map-set power-scaling token-id
      { base-power: u100,
        holding-start: stacks-block-height,
        power-multiplier: u1 })
    (ok true)))

(define-public (calculate-current-power (token-id uint))
  (let (
    (scaling-data (unwrap! (map-get? power-scaling token-id) (err u300)))
    (blocks-held (- stacks-block-height (get holding-start scaling-data)))
    (power-increase (+ u1 (/ blocks-held POWER_SCALE_FACTOR)))
    (new-multiplier (if (> power-increase MAX_POWER_MULTIPLIER)
      MAX_POWER_MULTIPLIER
      power-increase))
  )
    (map-set power-scaling token-id
      (merge scaling-data { power-multiplier: new-multiplier }))
    (ok (* (get base-power scaling-data) new-multiplier))))

  
  (define-map merged-attributes uint 
  { source-nft1: uint,
    source-nft2: uint,
    strength: uint,
    speed: uint,
    wisdom: uint })

(define-constant MERGE_COOLDOWN u100)
(define-map merge-cooldowns principal uint)

(define-public (merge-nft-attributes (nft1-id uint) (nft2-id uint))
  (let (
    (nft1-traits (unwrap! (get-nft-traits nft1-id) (err u200)))
    (nft2-traits (unwrap! (get-nft-traits nft2-id) (err u201)))
    (token-id (+ (var-get last-token-id) u1))
    (last-merge (default-to u0 (map-get? merge-cooldowns tx-sender)))
  )
    (asserts! (> stacks-block-height (+ last-merge MERGE_COOLDOWN)) (err u202))
    (map-set merged-attributes token-id
      { source-nft1: nft1-id,
        source-nft2: nft2-id,
        strength: (/ (+ (get strength nft1-traits) (get strength nft2-traits)) u2),
        speed: (/ (+ (get speed nft1-traits) (get speed nft2-traits)) u2),
        wisdom: (/ (+ (get wisdom nft1-traits) (get wisdom nft2-traits)) u2) })
    (map-set merge-cooldowns tx-sender stacks-block-height)
    (var-set last-token-id token-id)
    (ok token-id)))

  

  (define-map user-reputation principal 
  { score: uint,
    last-update: uint,
    positive-actions: uint,
    negative-actions: uint })

(define-map reputation-history principal 
  { total-score-earned: uint,
    peak-reputation: uint,
    reputation-tier: uint })

(define-constant REPUTATION_DECAY_RATE u1)
(define-constant REPUTATION_DECAY_INTERVAL u1000)
(define-constant MAX_REPUTATION u1000)
(define-constant MIN_REPUTATION u0)

(define-public (initialize-reputation)
  (let ((existing-rep (map-get? user-reputation tx-sender)))
    (if (is-none existing-rep)
      (begin
        (map-set user-reputation tx-sender
          { score: u100,
            last-update: stacks-block-height,
            positive-actions: u0,
            negative-actions: u0 })
        (map-set reputation-history tx-sender
          { total-score-earned: u0,
            peak-reputation: u100,
            reputation-tier: u1 })
        (ok true))
      (ok false))))

(define-public (add-positive-reputation (amount uint))
  (let (
    (current-rep (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
                  (map-get? user-reputation tx-sender)))
    (current-history (default-to { total-score-earned: u0, peak-reputation: u100, reputation-tier: u1 }
                     (map-get? reputation-history tx-sender)))
    (decayed-score (calculate-decayed-reputation (get score current-rep) (get last-update current-rep)))
    (new-score (if (> (+ decayed-score amount) MAX_REPUTATION)
                 MAX_REPUTATION
                 (+ decayed-score amount)))
    (new-tier (calculate-reputation-tier new-score))
  )
    (map-set user-reputation tx-sender
      { score: new-score,
        last-update: stacks-block-height,
        positive-actions: (+ (get positive-actions current-rep) u1),
        negative-actions: (get negative-actions current-rep) })
    (map-set reputation-history tx-sender
      { total-score-earned: (+ (get total-score-earned current-history) amount),
        peak-reputation: (if (> new-score (get peak-reputation current-history))
                          new-score
                          (get peak-reputation current-history)),
        reputation-tier: new-tier })
    (ok new-score)))

(define-public (subtract-negative-reputation (amount uint))
  (let (
    (current-rep (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
                  (map-get? user-reputation tx-sender)))
    (decayed-score (calculate-decayed-reputation (get score current-rep) (get last-update current-rep)))
    (new-score (if (< decayed-score amount)
                 MIN_REPUTATION
                 (- decayed-score amount)))
    (new-tier (calculate-reputation-tier new-score))
  )
    (map-set user-reputation tx-sender
      { score: new-score,
        last-update: stacks-block-height,
        positive-actions: (get positive-actions current-rep),
        negative-actions: (+ (get negative-actions current-rep) u1) })
    (ok new-score)))

(define-private (calculate-decayed-reputation (current-score uint) (last-update uint))
  (let (
    (blocks-passed (- stacks-block-height last-update))
    (decay-periods (/ blocks-passed REPUTATION_DECAY_INTERVAL))
    (total-decay (* decay-periods REPUTATION_DECAY_RATE))
  )
    (if (> total-decay current-score)
      MIN_REPUTATION
      (- current-score total-decay))))

(define-private (calculate-reputation-tier (score uint))
  (if (>= score u800)
    u5
    (if (>= score u600)
      u4
      (if (>= score u400)
        u3
        (if (>= score u200)
          u2
          u1)))))

(define-public (get-reputation-multiplier (user principal))
  (let (
    (rep-data (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
               (map-get? user-reputation user)))
    (current-score (calculate-decayed-reputation (get score rep-data) (get last-update rep-data)))
    (tier (calculate-reputation-tier current-score))
  )
    (ok (if (is-eq tier u5)
          u3
          (if (is-eq tier u4)
            u2
            u1)))))

(define-public (reputation-gated-mint)
  (let (
    (rep-data (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
               (map-get? user-reputation tx-sender)))
    (current-score (calculate-decayed-reputation (get score rep-data) (get last-update rep-data)))
    (token-id (+ (var-get last-token-id) u1))
  )
    (asserts! (>= current-score u300) (err u400))
    (asserts! (is-none (map-get? token-owners tx-sender)) ERR_NFT_EXISTS)
    (map-set token-owners tx-sender token-id)
    (map-set activity-levels tx-sender u1)
    (map-set evolution-stages tx-sender u1)
    (var-set last-token-id token-id)
    (unwrap! (add-positive-reputation u20) (err u401))
    (ok token-id)))

(define-public (reputation-based-trade (token-id uint) (buyer principal) (price uint))
  (let (
    (seller-rep (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
                 (map-get? user-reputation tx-sender)))
    (buyer-rep (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
               (map-get? user-reputation buyer)))
    (seller-score (calculate-decayed-reputation (get score seller-rep) (get last-update seller-rep)))
    (buyer-score (calculate-decayed-reputation (get score buyer-rep) (get last-update buyer-rep)))
  )
    (asserts! (>= seller-score u200) (err u401))
    (asserts! (>= buyer-score u200) (err u402))
    (asserts! (is-some (map-get? token-owners tx-sender)) ERR_NOT_AUTHORIZED)
    (map-set token-owners buyer token-id)
    (unwrap! (add-positive-reputation u10) (err u403))
    (ok true)))

(define-read-only (get-user-reputation (user principal))
  (let (
    (rep-data (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
               (map-get? user-reputation user)))
  )
    (ok { 
      current-score: (calculate-decayed-reputation (get score rep-data) (get last-update rep-data)),
      tier: (calculate-reputation-tier (calculate-decayed-reputation (get score rep-data) (get last-update rep-data))),
      positive-actions: (get positive-actions rep-data),
      negative-actions: (get negative-actions rep-data)
    })))

(define-read-only (get-reputation-history (user principal))
  (ok (map-get? reputation-history user)))

(define-public (report-malicious-behavior (reported-user principal))
  (let (
    (reporter-rep (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
                   (map-get? user-reputation tx-sender)))
    (reporter-score (calculate-decayed-reputation (get score reporter-rep) (get last-update reporter-rep)))
  )
    (asserts! (>= reporter-score u400) (err u403))
    (asserts! (not (is-eq tx-sender reported-user)) (err u404))
    (unwrap! (add-positive-reputation u5) (err u405))
    (ok true)))

(define-map insurance-policies uint
  { insured-nft: uint,
    policy-holder: principal,
    coverage-amount: uint,
    premium-paid: uint,
    policy-start: uint,
    policy-end: uint,
    active: bool })

(define-map insurer-pools principal
  { total-staked: uint,
    available-coverage: uint,
    policies-backed: uint,
    yield-earned: uint })

(define-map insurance-claims uint
  { policy-id: uint,
    claim-amount: uint,
    claim-reason: (string-ascii 100),
    claim-time: uint,
    status: uint,
    validator-votes: uint })

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)

(define-constant INSURANCE_ERR_INSUFFICIENT_COVERAGE (err u500))
(define-constant INSURANCE_ERR_INVALID_POLICY (err u501))
(define-constant INSURANCE_ERR_CLAIM_EXISTS (err u502))
(define-constant INSURANCE_ERR_INVALID_CLAIM (err u503))
(define-constant INSURANCE_ERR_POLICY_EXPIRED (err u504))

(define-public (stake-as-insurer (amount uint))
  (let (
    (current-pool (default-to { total-staked: u0, available-coverage: u0, policies-backed: u0, yield-earned: u0 }
                   (map-get? insurer-pools tx-sender)))
  )
    (map-set insurer-pools tx-sender
      { total-staked: (+ (get total-staked current-pool) amount),
        available-coverage: (+ (get available-coverage current-pool) (* amount u5)),
        policies-backed: (get policies-backed current-pool),
        yield-earned: (get yield-earned current-pool) })
    (ok true)))

(define-public (create-insurance-policy (nft-id uint) (coverage-amount uint) (duration uint))
  (let (
    (policy-id (var-get next-policy-id))
    (premium-rate u10)
    (premium-amount (/ (* coverage-amount premium-rate) u100))
    (owner (map-get? token-owners tx-sender))
  )
    (asserts! (is-some owner) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get-total-available-coverage) coverage-amount) INSURANCE_ERR_INSUFFICIENT_COVERAGE)
    (map-set insurance-policies policy-id
      { insured-nft: nft-id,
        policy-holder: tx-sender,
        coverage-amount: coverage-amount,
        premium-paid: premium-amount,
        policy-start: stacks-block-height,
        policy-end: (+ stacks-block-height duration),
        active: true })
    (var-set next-policy-id (+ policy-id u1))
    (unwrap! (distribute-premium premium-amount) (err u506))
    (ok policy-id)))

(define-public (file-insurance-claim (policy-id uint) (claim-amount uint) (reason (string-ascii 100)))
  (let (
    (policy (unwrap! (map-get? insurance-policies policy-id) INSURANCE_ERR_INVALID_POLICY))
    (claim-id (var-get next-claim-id))
  )
    (asserts! (is-eq (get policy-holder policy) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (get active policy) INSURANCE_ERR_INVALID_POLICY)
    (asserts! (<= stacks-block-height (get policy-end policy)) INSURANCE_ERR_POLICY_EXPIRED)
    (asserts! (<= claim-amount (get coverage-amount policy)) (err u505))
    (map-set insurance-claims claim-id
      { policy-id: policy-id,
        claim-amount: claim-amount,
        claim-reason: reason,
        claim-time: stacks-block-height,
        status: u1,
        validator-votes: u0 })
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)))

(define-public (validate-claim (claim-id uint) (approve bool))
  (let (
    (claim (unwrap! (map-get? insurance-claims claim-id) INSURANCE_ERR_INVALID_CLAIM))
    (validator-rep (default-to { score: u100, last-update: u0, positive-actions: u0, negative-actions: u0 }
                    (map-get? user-reputation tx-sender)))
    (validator-score (calculate-decayed-reputation (get score validator-rep) (get last-update validator-rep)))
  )
    (asserts! (>= validator-score u400) (err u403))
    (asserts! (is-eq (get status claim) u1) INSURANCE_ERR_INVALID_CLAIM)
    (if approve
      (begin
        (map-set insurance-claims claim-id
          (merge claim { validator-votes: (+ (get validator-votes claim) u1) }))
        (if (>= (get validator-votes claim) u3)
          (begin
            (map-set insurance-claims claim-id
              (merge claim { status: u2 }))
            (process-insurance-payout claim-id))
          (ok true)))
      (begin
        (map-set insurance-claims claim-id
          (merge claim { status: u3 }))
        (ok true)))))

(define-private (process-insurance-payout (claim-id uint))
  (let (
    (claim (unwrap! (map-get? insurance-claims claim-id) INSURANCE_ERR_INVALID_CLAIM))
    (policy (unwrap! (map-get? insurance-policies (get policy-id claim)) INSURANCE_ERR_INVALID_POLICY))
  )
    (map-set insurance-policies (get policy-id claim)
      (merge policy { active: false }))
    (ok true)))

(define-private (distribute-premium (premium-amount uint))
  (ok true))

(define-private (get-total-available-coverage)
  u1000000)

(define-public (withdraw-insurer-yield)
  (let (
    (pool-data (unwrap! (map-get? insurer-pools tx-sender) (err u506)))
    (yield-amount (get yield-earned pool-data))
  )
    (asserts! (> yield-amount u0) (err u507))
    (map-set insurer-pools tx-sender
      (merge pool-data { yield-earned: u0 }))
    (ok yield-amount)))

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies policy-id))

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims claim-id))

(define-read-only (get-insurer-pool (insurer principal))
  (map-get? insurer-pools insurer))



