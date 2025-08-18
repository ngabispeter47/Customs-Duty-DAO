(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri ((optional uint)) (response (optional (string-utf8 256)) uint))
  )
)

(define-fungible-token customs-token)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-proposal (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-proposal-ended (err u105))
(define-constant err-proposal-active (err u106))
(define-constant err-insufficient-stake (err u107))
(define-constant err-unauthorized (err u108))
(define-constant err-self-delegation (err u109))
(define-constant err-delegation-cycle (err u110))

(define-data-var proposal-counter uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var min-proposal-stake uint u1000000)
(define-data-var voting-period uint u1440)
(define-data-var quorum-threshold uint u5000000)

(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    target-duty-rate: uint,
    commodity-code: (string-ascii 32),
    start-block: uint,
    end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    total-stake: uint,
    executed: bool,
    passed: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool,
    amount: uint,
    block-height: uint
  }
)

(define-map member-stakes
  { member: principal }
  {
    staked-amount: uint,
    voting-power: uint,
    last-claim-block: uint
  }
)

(define-map treasury-contributions
  { contributor: principal }
  {
    total-contributed: uint,
    last-contribution-block: uint
  }
)

(define-map delegations
  { delegator: principal }
  {
    delegate: principal,
    delegated-amount: uint,
    delegation-block: uint
  }
)

(define-map delegation-power
  { delegate: principal }
  {
    total-delegated: uint,
    delegator-count: uint
  }
)

(define-public (initialize (initial-supply uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (ft-mint? customs-token initial-supply tx-sender))
    (ok true)
  )
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender sender) (is-eq contract-caller sender)) err-unauthorized)
    (ft-transfer? customs-token amount sender recipient)
  )
)

(define-public (stake-tokens (amount uint))
  (let
    (
      (current-stake (default-to { staked-amount: u0, voting-power: u0, last-claim-block: u0 }
                                  (map-get? member-stakes { member: tx-sender })))
    )
    (try! (ft-transfer? customs-token amount tx-sender (as-contract tx-sender)))
    (map-set member-stakes
      { member: tx-sender }
      {
        staked-amount: (+ (get staked-amount current-stake) amount),
        voting-power: (+ (get voting-power current-stake) amount),
        last-claim-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (unstake-tokens (amount uint))
  (let
    (
      (current-stake (unwrap! (map-get? member-stakes { member: tx-sender }) err-not-found))
    )
    (asserts! (>= (get staked-amount current-stake) amount) err-insufficient-balance)
    (try! (as-contract (ft-transfer? customs-token amount tx-sender tx-sender)))
    (map-set member-stakes
      { member: tx-sender }
      {
        staked-amount: (- (get staked-amount current-stake) amount),
        voting-power: (- (get voting-power current-stake) amount),
        last-claim-block: (get last-claim-block current-stake)
      }
    )
    (ok true)
  )
)

(define-public (create-proposal (title (string-ascii 128)) (description (string-ascii 512)) 
                               (target-duty-rate uint) (commodity-code (string-ascii 32)))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (member-stake (unwrap! (map-get? member-stakes { member: tx-sender }) err-not-found))
    )
    (asserts! (>= (get staked-amount member-stake) (var-get min-proposal-stake)) err-insufficient-stake)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        target-duty-rate: target-duty-rate,
        commodity-code: commodity-code,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height (var-get voting-period)),
        yes-votes: u0,
        no-votes: u0,
        total-stake: u0,
        executed: false,
        passed: false
      }
    )
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (delegate-voting-power (delegate principal) (amount uint))
  (let
    (
      (current-stake (unwrap! (map-get? member-stakes { member: tx-sender }) err-not-found))
      (existing-delegation (map-get? delegations { delegator: tx-sender }))
      (delegate-power (default-to { total-delegated: u0, delegator-count: u0 }
                                  (map-get? delegation-power { delegate: delegate })))
    )
    (asserts! (not (is-eq tx-sender delegate)) err-self-delegation)
    (asserts! (>= (get staked-amount current-stake) amount) err-insufficient-balance)
    
    (match existing-delegation
      prev-delegation
        (let
          (
            (prev-delegate (get delegate prev-delegation))
            (prev-amount (get delegated-amount prev-delegation))
            (prev-delegate-power (default-to { total-delegated: u0, delegator-count: u0 }
                                            (map-get? delegation-power { delegate: prev-delegate })))
          )
          (map-set delegation-power
            { delegate: prev-delegate }
            {
              total-delegated: (- (get total-delegated prev-delegate-power) prev-amount),
              delegator-count: (- (get delegator-count prev-delegate-power) u1)
            }
          )
        )
      true
    )
    
    (map-set delegations
      { delegator: tx-sender }
      {
        delegate: delegate,
        delegated-amount: amount,
        delegation-block: stacks-block-height
      }
    )
    
    (map-set delegation-power
      { delegate: delegate }
      {
        total-delegated: (+ (get total-delegated delegate-power) amount),
        delegator-count: (+ (get delegator-count delegate-power) u1)
      }
    )
    
    (map-set member-stakes
      { member: tx-sender }
      (merge current-stake { voting-power: (- (get voting-power current-stake) amount) })
    )
    (ok true)
  )
)

(define-public (revoke-delegation)
  (let
    (
      (delegation (unwrap! (map-get? delegations { delegator: tx-sender }) err-not-found))
      (current-stake (unwrap! (map-get? member-stakes { member: tx-sender }) err-not-found))
      (delegate (get delegate delegation))
      (amount (get delegated-amount delegation))
      (delegate-power (unwrap! (map-get? delegation-power { delegate: delegate }) err-not-found))
    )
    (map-delete delegations { delegator: tx-sender })
    
    (map-set delegation-power
      { delegate: delegate }
      {
        total-delegated: (- (get total-delegated delegate-power) amount),
        delegator-count: (- (get delegator-count delegate-power) u1)
      }
    )
    
    (map-set member-stakes
      { member: tx-sender }
      (merge current-stake { voting-power: (+ (get voting-power current-stake) amount) })
    )
    (ok true)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool) (amount uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
      (member-stake (unwrap! (map-get? member-stakes { member: tx-sender }) err-not-found))
      (existing-vote (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
      (delegate-power (default-to { total-delegated: u0, delegator-count: u0 }
                                  (map-get? delegation-power { delegate: tx-sender })))
      (total-voting-power (+ (get voting-power member-stake) (get total-delegated delegate-power)))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (>= total-voting-power amount) err-insufficient-balance)
    (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-ended)
    
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: vote,
        amount: amount,
        block-height: stacks-block-height
      }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal
        {
          yes-votes: (if vote (+ (get yes-votes proposal) amount) (get yes-votes proposal)),
          no-votes: (if vote (get no-votes proposal) (+ (get no-votes proposal) amount)),
          total-stake: (+ (get total-stake proposal) amount)
        }
      )
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
    )
    (asserts! (> stacks-block-height (get end-block proposal)) err-proposal-active)
    (asserts! (not (get executed proposal)) err-invalid-proposal)
    (asserts! (>= (get total-stake proposal) (var-get quorum-threshold)) err-insufficient-stake)
    
    (let
      (
        (passed (> (get yes-votes proposal) (get no-votes proposal)))
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { executed: true, passed: passed })
      )
      (ok passed)
    )
  )
)

(define-public (contribute-to-treasury (amount uint))
  (let
    (
      (current-contribution (default-to { total-contributed: u0, last-contribution-block: u0 }
                                         (map-get? treasury-contributions { contributor: tx-sender })))
    )
    (try! (ft-transfer? customs-token amount tx-sender (as-contract tx-sender)))
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (map-set treasury-contributions
      { contributor: tx-sender }
      {
        total-contributed: (+ (get total-contributed current-contribution) amount),
        last-contribution-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (withdraw-from-treasury (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get treasury-balance)) err-insufficient-balance)
    (try! (as-contract (ft-transfer? customs-token amount tx-sender recipient)))
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-public (update-voting-parameters (new-min-stake uint) (new-voting-period uint) (new-quorum uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-proposal-stake new-min-stake)
    (var-set voting-period new-voting-period)
    (var-set quorum-threshold new-quorum)
    (ok true)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-member-stake (member principal))
  (map-get? member-stakes { member: member })
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (get-voting-parameters)
  {
    min-proposal-stake: (var-get min-proposal-stake),
    voting-period: (var-get voting-period),
    quorum-threshold: (var-get quorum-threshold)
  }
)

(define-read-only (get-token-balance (account principal))
  (ft-get-balance customs-token account)
)

(define-read-only (get-total-supply)
  (ft-get-supply customs-token)
)

(define-read-only (get-name)
  (ok "Customs Token")
)

(define-read-only (get-symbol)
  (ok "CDT")
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-token-uri)
  (ok none)
)

(define-read-only (get-delegation (delegator principal))
  (map-get? delegations { delegator: delegator })
)

(define-read-only (get-delegation-power (delegate principal))
  (map-get? delegation-power { delegate: delegate })
)

(define-read-only (get-effective-voting-power (member principal))
  (let
    (
      (member-stake (default-to { staked-amount: u0, voting-power: u0, last-claim-block: u0 }
                                 (map-get? member-stakes { member: member })))
      (delegate-power (default-to { total-delegated: u0, delegator-count: u0 }
                                  (map-get? delegation-power { delegate: member })))
    )
    (+ (get voting-power member-stake) (get total-delegated delegate-power))
  )
)

(define-private (calculate-voting-power (staked-amount uint) (blocks-staked uint))
  (if (> blocks-staked u0)
    (+ staked-amount (/ (* staked-amount blocks-staked) u10000))
    staked-amount
  )
)
