;; Dynamic NFT Marketplace with time-based auctions

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_NOT_ACTIVE (err u102))
(define-constant ERR_ENDED (err u103))
(define-constant ERR_BID_LOW (err u104))
(define-constant ERR_REP_LOW (err u105))
(define-constant ERR_NOT_SELLER (err u106))
(define-constant ERR_HAS_BIDS (err u107))
(define-constant ERR_ALREADY_DONE (err u110))
(define-constant ERR_BAD_ARGS (err u111))

;; Pricing constants
(define-constant BPS u10000)
(define-constant BASE_FEE_BPS u250)
(define-constant MIN_FEE_BPS u50)
(define-constant DISC_PER_REP_BPS u2)
(define-constant MAX_DISC_BPS u200)
(define-constant MIN_INC_BPS u100)
(define-constant DEMAND_STEP_BPS u10)
(define-constant MAX_DEMAND_BPS u300)
(define-constant EXTEND_WINDOW u10)
(define-constant MIN_SELLER_REP u10)
(define-constant MIN_BIDDER_REP u5)

;; Data variables
(define-data-var next-id uint u1)

;; Auction data - simplified structure
(define-map auctions uint {
  seller: principal,
  token: uint,
  reserve: uint,
  end: uint,
  highest: uint,
  top: (optional principal),
  bids: uint,
  status: uint
})

;; Auction history
(define-map history uint {
  sold: bool,
  price: uint,
  buyer: (optional principal),
  seller: principal,
  token: uint,
  end: uint
})

;; Helper functions
(define-private (get-user-rep (p principal))
  (let ((evolution-stage (contract-call? .NFT-Innovations get-evolution-stage p)))
    (if (> evolution-stage u0) u50 u0)))

(define-private (calculate-fee (user principal))
  (let ((rep-score (get-user-rep user))
        (discount (if (< (* rep-score u2) u200) (* rep-score u2) u200)))
    (if (> u50 (if (> u250 discount) (- u250 discount) u0)) u50 (if (> u250 discount) (- u250 discount) u0))))

(define-private (calculate-min-bid (auction-data (tuple (seller principal) (token uint) (reserve uint) (end uint) (highest uint) (top (optional principal)) (bids uint) (status uint))))
  (let ((current-bid (get highest auction-data))
        (bid-count (get bids auction-data))
        (reserve-price (get reserve auction-data))
        (demand-factor (if (< (* bid-count u10) u300) (* bid-count u10) u300))
        (increment-factor (+ u100 demand-factor))
        (increment (/ (* current-bid increment-factor) u10000))
        (min-bid (+ current-bid increment)))
    (if (> reserve-price min-bid) reserve-price min-bid)))

;; Public functions

;; Create auction - simplified without NFT transfer for demo
(define-public (create-auction (token uint) (reserve uint) (duration uint))
  (begin
    (asserts! (> duration u0) ERR_BAD_ARGS)
    (asserts! (> token u0) ERR_BAD_ARGS)
    (asserts! (>= (get-user-rep tx-sender) MIN_SELLER_REP) ERR_REP_LOW)
    (let ((auction-id (var-get next-id))
          (end-block (+ stacks-block-height duration)))
      (map-set auctions auction-id {
        seller: tx-sender,
        token: token,
        reserve: reserve,
        end: end-block,
        highest: u0,
        top: none,
        bids: u0,
        status: u0
      })
      (var-set next-id (+ auction-id u1))
      (ok auction-id))))

;; Place bid
(define-public (bid (auction-id uint) (amount uint))
  (let ((auction-data (unwrap! (map-get? auctions auction-id) ERR_NOT_FOUND)))
    (asserts! (is-eq (get status auction-data) u0) ERR_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get end auction-data)) ERR_ENDED)
    (asserts! (>= (get-user-rep tx-sender) MIN_BIDDER_REP) ERR_REP_LOW)
    (let ((min-bid (calculate-min-bid auction-data)))
      (asserts! (>= amount min-bid) ERR_BID_LOW)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      ;; Refund previous bidder
      ;; (match (get top auction-data)
      ;;   some prev-bidder (as-contract (stx-transfer? (get highest auction-data) tx-sender prev-bidder))
      ;;   none (ok true))
      ;; Update auction with new bid and extend if near end
      (let ((time-remaining (- (get end auction-data) stacks-block-height))
            (new-end (if (< time-remaining EXTEND_WINDOW)
                        (+ stacks-block-height EXTEND_WINDOW)
                        (get end auction-data))))
        (map-set auctions auction-id (merge auction-data {
          highest: amount,
          top: (some tx-sender),
          bids: (+ (get bids auction-data) u1),
          end: new-end
        }))
        (ok true)))))

;; Settle auction
(define-public (settle (auction-id uint))
  (let ((auction-data (unwrap! (map-get? auctions auction-id) ERR_NOT_FOUND)))
    (asserts! (is-eq (get status auction-data) u0) ERR_ALREADY_DONE)
    (asserts! (>= stacks-block-height (get end auction-data)) ERR_NOT_ACTIVE)
    (let ((highest-bid (get highest auction-data))
          (reserve (get reserve auction-data))
          (seller (get seller auction-data)))
      (if (and (> highest-bid u0) (>= highest-bid reserve) (is-some (get top auction-data)))
        (let ((buyer (unwrap! (get top auction-data) ERR_NOT_FOUND))
              (fee-bps (calculate-fee seller))
              (fee-amount (/ (* highest-bid fee-bps) BPS))
              (seller-amount (- highest-bid (/ (* highest-bid fee-bps) BPS))))
          ;; Transfer fee to contract owner
          (try! (as-contract (stx-transfer? fee-amount tx-sender CONTRACT-OWNER)))
          ;; Transfer remaining to seller
          (try! (as-contract (stx-transfer? seller-amount tx-sender seller)))
          ;; Transfer NFT to buyer - simplified without actual NFT transfer for demo
          ;; Update auction status and history
          (map-set auctions auction-id (merge auction-data { status: u1 }))
          (map-set history auction-id {
            sold: true,
            price: highest-bid,
            buyer: (some buyer),
            seller: seller,
            token: (get token auction-data),
            end: (get end auction-data)
          })
          (ok true))
        (begin
          ;; (match (get top auction-data)
          ;;   some bidder (as-contract (stx-transfer? highest-bid tx-sender bidder))
          ;;   none (ok true))
          (map-set auctions auction-id (merge auction-data { status: u2 }))
          (map-set history auction-id {
            sold: false,
            price: u0,
            buyer: none,
            seller: seller,
            token: (get token auction-data),
            end: (get end auction-data)
          })
          (ok true))))))

;; Cancel auction (seller only, before any bids)
(define-public (cancel-auction (auction-id uint))
  (let ((auction-data (unwrap! (map-get? auctions auction-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get seller auction-data)) ERR_NOT_SELLER)
    (asserts! (is-eq (get status auction-data) u0) ERR_ALREADY_DONE)
    (asserts! (is-eq (get bids auction-data) u0) ERR_HAS_BIDS)
    (map-set auctions auction-id (merge auction-data { status: u2 }))
    (map-set history auction-id {
      sold: false,
      price: u0,
      buyer: none,
      seller: (get seller auction-data),
      token: (get token auction-data),
      end: (get end auction-data)
    })
    (ok true)))

;; Read-only functions
(define-read-only (get-auction (auction-id uint))
  (map-get? auctions auction-id))

(define-read-only (get-auction-history (auction-id uint))
  (map-get? history auction-id))

(define-read-only (get-next-auction-id)
  (var-get next-id))

;; (define-read-only (get-min-next-bid (auction-id uint))
;;   (match (map-get? auctions auction-id)
;;     some auction-data (calculate-min-bid auction-data)
;;     none u0))

(define-read-only (get-user-reputation (user principal))
  (get-user-rep user))

(define-read-only (get-user-fee-rate (user principal))
  (calculate-fee user))
