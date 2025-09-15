;; Community Treasury and Governance System
;; Enables NFT holders to propose, vote, and fund community initiatives

;; Error Constants
(define-constant ERR_NOT_AUTHORIZED (err u600))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u601))
(define-constant ERR_VOTING_ENDED (err u602))
(define-constant ERR_ALREADY_VOTED (err u603))
(define-constant ERR_INSUFFICIENT_FUNDS (err u604))
(define-constant ERR_PROPOSAL_NOT_APPROVED (err u605))
(define-constant ERR_FUNDS_ALREADY_RELEASED (err u606))
(define-constant ERR_INVALID_VOTING_POWER (err u607))
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED (err u608))
(define-constant ERR_MINIMUM_THRESHOLD_NOT_MET (err u609))

;; Data Variables
(define-data-var treasury-balance uint u0)
(define-data-var next-proposal-id uint u1)
(define-data-var governance-fee uint u10) ;; Fee percentage for proposal creation
(define-data-var minimum-voting-power uint u50) ;; Minimum evolution stage to vote
(define-data-var proposal-deposit uint u100) ;; Required deposit to create proposal

;; Treasury Management
(define-map treasury-contributions principal uint)
(define-map contribution-history principal 
  { total-contributed: uint,
    contribution-count: uint,
    last-contribution: uint })

;; Proposal System
(define-map governance-proposals uint
  { title: (string-ascii 128),
    description: (string-ascii 512),
    proposer: principal,
    funding-amount: uint,
    proposal-type: uint, ;; 1=funding, 2=governance change, 3=community event
    voting-start: uint,
    voting-end: uint,
    votes-for: uint,
    votes-against: uint,
    total-voting-power: uint,
    status: uint, ;; 1=active, 2=approved, 3=rejected, 4=executed
    execution-block: uint })

;; Voting System
(define-map proposal-votes 
  { proposal-id: uint, voter: principal }
  { vote: bool, ;; true=for, false=against
    voting-power: uint,
    vote-time: uint })

(define-map voter-participation principal
  { proposals-voted: uint,
    governance-score: uint,
    last-vote: uint })

;; Funding Management
(define-map approved-funding uint
  { recipient: principal,
    amount: uint,
    purpose: (string-ascii 256),
    approval-block: uint,
    funds-released: bool,
    milestone-count: uint,
    completed-milestones: uint })

;; Treasury Operations
(define-public (contribute-to-treasury (amount uint))
  (let (
    (current-balance (var-get treasury-balance))
    (current-contribution (default-to u0 (map-get? treasury-contributions tx-sender)))
    (contribution-data (default-to { total-contributed: u0, contribution-count: u0, last-contribution: u0 }
                        (map-get? contribution-history tx-sender)))
  )
    (var-set treasury-balance (+ current-balance amount))
    (map-set treasury-contributions tx-sender (+ current-contribution amount))
    (map-set contribution-history tx-sender
      { total-contributed: (+ (get total-contributed contribution-data) amount),
        contribution-count: (+ (get contribution-count contribution-data) u1),
        last-contribution: stacks-block-height })
    (ok true)))

(define-public (create-governance-proposal 
  (title (string-ascii 128)) 
  (description (string-ascii 512))
  (funding-amount uint)
  (proposal-type uint)
  (voting-duration uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (user-nft-stage (contract-call? .NFT-Innovations get-evolution-stage tx-sender))
    (deposit-amount (var-get proposal-deposit))
  )
    (asserts! (>= user-nft-stage (var-get minimum-voting-power)) ERR_INVALID_VOTING_POWER)
    (asserts! (>= (var-get treasury-balance) funding-amount) ERR_INSUFFICIENT_FUNDS)
    (map-set governance-proposals proposal-id
      { title: title,
        description: description,
        proposer: tx-sender,
        funding-amount: funding-amount,
        proposal-type: proposal-type,
        voting-start: stacks-block-height,
        voting-end: (+ stacks-block-height voting-duration),
        votes-for: u0,
        votes-against: u0,
        total-voting-power: u0,
        status: u1,
        execution-block: u0 })
    (var-set next-proposal-id (+ proposal-id u1))
    (var-set treasury-balance (- (var-get treasury-balance) deposit-amount))
    (ok proposal-id)))

(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (user-nft-stage (contract-call? .NFT-Innovations get-evolution-stage tx-sender))
    (user-reputation (contract-call? .NFT-Innovations get-user-reputation tx-sender))
    (voting-power (calculate-voting-power user-nft-stage user-reputation))
    (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
    (participation-data (default-to { proposals-voted: u0, governance-score: u0, last-vote: u0 }
                         (map-get? voter-participation tx-sender)))
  )
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (<= stacks-block-height (get voting-end proposal)) ERR_VOTING_ENDED)
    (asserts! (>= user-nft-stage (var-get minimum-voting-power)) ERR_INVALID_VOTING_POWER)
    (asserts! (is-eq (get status proposal) u1) ERR_VOTING_ENDED)
    
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for,
        voting-power: voting-power,
        vote-time: stacks-block-height })
    
    (map-set governance-proposals proposal-id
      (if vote-for
        (merge proposal { 
          votes-for: (+ (get votes-for proposal) voting-power),
          total-voting-power: (+ (get total-voting-power proposal) voting-power) })
        (merge proposal { 
          votes-against: (+ (get votes-against proposal) voting-power),
          total-voting-power: (+ (get total-voting-power proposal) voting-power) })))
    
    (map-set voter-participation tx-sender
      { proposals-voted: (+ (get proposals-voted participation-data) u1),
        governance-score: (+ (get governance-score participation-data) voting-power),
        last-vote: stacks-block-height })
    (ok true)))

(define-public (finalize-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (total-votes (get total-voting-power proposal))
    (votes-for (get votes-for proposal))
    (votes-against (get votes-against proposal))
    (approval-threshold (/ (* total-votes u60) u100)) ;; 60% approval needed
  )
    (asserts! (> stacks-block-height (get voting-end proposal)) (err u610))
    (asserts! (is-eq (get status proposal) u1) ERR_PROPOSAL_ALREADY_EXECUTED)
    (asserts! (>= total-votes (var-get minimum-voting-power)) ERR_MINIMUM_THRESHOLD_NOT_MET)
    
    (if (>= votes-for approval-threshold)
      (begin
        (map-set governance-proposals proposal-id
          (merge proposal { status: u2 }))
        (if (is-eq (get proposal-type proposal) u1) ;; Funding proposal
          (approve-funding proposal-id)
          (ok true)))
      (begin
        (map-set governance-proposals proposal-id
          (merge proposal { status: u3 }))
        (var-set treasury-balance (+ (var-get treasury-balance) (var-get proposal-deposit)))
        (ok true)))))

(define-public (execute-approved-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (funding-data (map-get? approved-funding proposal-id))
  )
    (asserts! (is-eq (get status proposal) u2) ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (is-some funding-data) ERR_PROPOSAL_NOT_FOUND)
    (asserts! (not (get funds-released (unwrap-panic funding-data))) ERR_FUNDS_ALREADY_RELEASED)
    
    (map-set approved-funding proposal-id
      (merge (unwrap-panic funding-data) { funds-released: true }))
    (map-set governance-proposals proposal-id
      (merge proposal { status: u4, execution-block: stacks-block-height }))
    (var-set treasury-balance (- (var-get treasury-balance) (get funding-amount proposal)))
    (ok true)))

(define-public (delegate-voting-power (delegate principal) (duration uint))
  (let (
    (user-nft-stage (contract-call? .NFT-Innovations get-evolution-stage tx-sender))
    (delegation-end (+ stacks-block-height duration))
  )
    (asserts! (>= user-nft-stage (var-get minimum-voting-power)) ERR_INVALID_VOTING_POWER)
    (ok true)))

(define-public (emergency-treasury-withdrawal (amount uint) (reason (string-ascii 256)))
  (let (
    (user-reputation (contract-call? .NFT-Innovations get-user-reputation tx-sender))
    (current-balance (var-get treasury-balance))
  )
    (asserts! (>= amount current-balance) ERR_INSUFFICIENT_FUNDS)
    (var-set treasury-balance (- current-balance amount))
    (ok true)))

;; Private Helper Functions
(define-private (calculate-voting-power (nft-stage uint) (reputation-data (response (tuple (current-score uint) (tier uint) (positive-actions uint) (negative-actions uint)) uint)))
  (let (
    (base-power (* nft-stage u10))
    (reputation-bonus (if (is-ok reputation-data)
                       (get tier (unwrap-panic reputation-data))
                       u1))
  )
    (+ base-power (* reputation-bonus u5))))

(define-private (approve-funding (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
  )
    (map-set approved-funding proposal-id
      { recipient: (get proposer proposal),
        amount: (get funding-amount proposal),
        purpose: "Community Funding",
        approval-block: stacks-block-height,
        funds-released: false,
        milestone-count: u3,
        completed-milestones: u0 })
    (ok true)))

;; Read-Only Functions
(define-read-only (get-treasury-balance)
  (var-get treasury-balance))

(define-read-only (get-proposal-details (proposal-id uint))
  (map-get? governance-proposals proposal-id))

(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter }))

(define-read-only (get-voter-participation (voter principal))
  (map-get? voter-participation voter))

(define-read-only (get-contribution-history (contributor principal))
  (map-get? contribution-history contributor))

(define-read-only (get-approved-funding (proposal-id uint))
  (map-get? approved-funding proposal-id))

(define-read-only (get-governance-stats)
  (ok {
    treasury-balance: (var-get treasury-balance),
    next-proposal-id: (var-get next-proposal-id),
    governance-fee: (var-get governance-fee),
    minimum-voting-power: (var-get minimum-voting-power),
    proposal-deposit: (var-get proposal-deposit)
  }))

(define-read-only (calculate-user-voting-power (user principal))
  (let (
    (nft-stage (contract-call? .NFT-Innovations get-evolution-stage user))
    (reputation-data (contract-call? .NFT-Innovations get-user-reputation user))
  )
    (ok (calculate-voting-power nft-stage reputation-data))))

(define-read-only (get-active-proposals)
  (ok {
    total-active: u0,
    proposals-needing-votes: u0,
    proposals-ready-for-execution: u0
  }))

;; Administrative Functions
(define-public (update-governance-parameters (new-fee uint) (new-min-power uint) (new-deposit uint))
  (begin
    (var-set governance-fee new-fee)
    (var-set minimum-voting-power new-min-power)
    (var-set proposal-deposit new-deposit)
    (ok true)))

(define-public (distribute-treasury-yield (yield-amount uint))
  (let (
    (current-balance (var-get treasury-balance))
  )
    (var-set treasury-balance (+ current-balance yield-amount))
    (ok true)))
