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
(define-constant err-reputation-penalty (err u111))
(define-constant err-reputation-boost-limit (err u112))
(define-constant err-reward-already-claimed (err u113))
(define-constant err-reward-not-available (err u114))
(define-constant err-insufficient-reward-pool (err u115))
(define-constant err-amendment-window-closed (err u116))
(define-constant err-amendment-limit-reached (err u117))
(define-constant err-not-proposer (err u118))
(define-constant err-amendment-not-found (err u119))

(define-data-var proposal-counter uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var min-proposal-stake uint u1000000)
(define-data-var voting-period uint u1440)
(define-data-var quorum-threshold uint u5000000)
(define-data-var reputation-decay-rate uint u100)
(define-data-var max-reputation-boost uint u5000)
(define-data-var reward-pool-balance uint u0)
(define-data-var proposal-reward-rate uint u10000)
(define-data-var voter-reward-rate uint u1000)
(define-data-var amendment-window uint u720)
(define-data-var max-amendments-per-proposal uint u3)

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

(define-map member-reputation
  { member: principal }
  {
    reputation-score: uint,
    successful-proposals: uint,
    failed-proposals: uint,
    votes-cast: uint,
    last-activity-block: uint,
    reputation-level: uint
  }
)

(define-map proposal-rewards
  { proposal-id: uint }
  {
    total-reward-pool: uint,
    proposer-reward: uint,
    voter-reward-pool: uint,
    rewards-claimed: bool,
    eligible-voters: uint
  }
)

(define-map member-rewards
  { member: principal, proposal-id: uint }
  {
    reward-amount: uint,
    claimed: bool,
    participation-type: (string-ascii 32)
  }
)

(define-map proposal-amendments
  { proposal-id: uint }
  {
    amendment-count: uint,
    last-amendment-block: uint
  }
)

(define-map amendment-details
  { proposal-id: uint, amendment-id: uint }
  {
    new-duty-rate: uint,
    reason: (string-ascii 256),
    amendment-block: uint,
    vote-reset-count: uint
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
      (member-rep (default-to { reputation-score: u1000, successful-proposals: u0, failed-proposals: u0, votes-cast: u0, last-activity-block: u0, reputation-level: u1 }
                              (map-get? member-reputation { member: tx-sender })))
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
      (member-rep (default-to { reputation-score: u1000, successful-proposals: u0, failed-proposals: u0, votes-cast: u0, last-activity-block: u0, reputation-level: u1 }
                              (map-get? member-reputation { member: tx-sender })))
      (reputation-boost (calculate-reputation-boost (get reputation-score member-rep)))
      (base-voting-power (+ (get voting-power member-stake) (get total-delegated delegate-power)))
      (boosted-amount (if (< (+ amount reputation-boost) (* amount u2)) (+ amount reputation-boost) (* amount u2)))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (>= base-voting-power amount) err-insufficient-balance)
    (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-ended)
    
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: vote,
        amount: boosted-amount,
        block-height: stacks-block-height
      }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal
        {
          yes-votes: (if vote (+ (get yes-votes proposal) boosted-amount) (get yes-votes proposal)),
          no-votes: (if vote (get no-votes proposal) (+ (get no-votes proposal) boosted-amount)),
          total-stake: (+ (get total-stake proposal) boosted-amount)
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
        (proposer (get proposer proposal))
      )
      (let () true)
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

(define-public (update-member-reputation (member principal) (successful-proposals uint) (failed-proposals uint) (votes-cast uint))
  (let
    (
      (current-rep (default-to { reputation-score: u1000, successful-proposals: u0, failed-proposals: u0, votes-cast: u0, last-activity-block: u0, reputation-level: u1 }
                               (map-get? member-reputation { member: member })))
      (blocks-since-activity (- stacks-block-height (get last-activity-block current-rep)))
      (decay-amount (if (> blocks-since-activity u1000) (/ (* (get reputation-score current-rep) (var-get reputation-decay-rate)) u10000) u0))
      (base-score (- (get reputation-score current-rep) decay-amount))
      (success-bonus (* successful-proposals u200))
      (failure-penalty (* failed-proposals u100))
      (vote-bonus (* votes-cast u10))
      (new-score (+ (+ (- base-score failure-penalty) success-bonus) vote-bonus))
      (clamped-score (if (< new-score u100) u100 (if (> new-score u10000) u10000 new-score)))
      (new-level (calculate-reputation-level clamped-score))
    )
    (map-set member-reputation
      { member: member }
      {
        reputation-score: clamped-score,
        successful-proposals: (+ (get successful-proposals current-rep) successful-proposals),
        failed-proposals: (+ (get failed-proposals current-rep) failed-proposals),
        votes-cast: (+ (get votes-cast current-rep) votes-cast),
        last-activity-block: stacks-block-height,
        reputation-level: new-level
      }
    )
    (ok true)
  )
)

(define-public (boost-member-reputation (member principal) (boost-amount uint))
  (let
    (
      (current-rep (unwrap! (map-get? member-reputation { member: member }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= boost-amount (var-get max-reputation-boost)) err-reputation-boost-limit)
    (map-set member-reputation
      { member: member }
      (merge current-rep { reputation-score: (if (< (+ (get reputation-score current-rep) boost-amount) u10000) (+ (get reputation-score current-rep) boost-amount) u10000) })
    )
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

(define-read-only (get-member-reputation (member principal))
  (map-get? member-reputation { member: member })
)

(define-read-only (get-reputation-boost-amount (member principal))
  (let
    (
      (member-rep (default-to { reputation-score: u1000, successful-proposals: u0, failed-proposals: u0, votes-cast: u0, last-activity-block: u0, reputation-level: u1 }
                              (map-get? member-reputation { member: member })))
    )
    (calculate-reputation-boost (get reputation-score member-rep))
  )
)

(define-read-only (get-reputation-parameters)
  {
    reputation-decay-rate: (var-get reputation-decay-rate),
    max-reputation-boost: (var-get max-reputation-boost)
  }
)

(define-read-only (get-reward-pool-balance)
  (var-get reward-pool-balance)
)

(define-read-only (get-proposal-reward-data (proposal-id uint))
  (map-get? proposal-rewards { proposal-id: proposal-id })
)

(define-read-only (get-member-reward (member principal) (proposal-id uint))
  (map-get? member-rewards { member: member, proposal-id: proposal-id })
)

(define-read-only (get-reward-rates)
  {
    proposal-reward-rate: (var-get proposal-reward-rate),
    voter-reward-rate: (var-get voter-reward-rate)
  }
)

(define-private (calculate-voting-power (staked-amount uint) (blocks-staked uint))
  (if (> blocks-staked u0)
    (+ staked-amount (/ (* staked-amount blocks-staked) u10000))
    staked-amount
  )
)

(define-private (calculate-reputation-level (reputation-score uint))
  (if (<= reputation-score u2000)
    u1
    (if (<= reputation-score u4000)
      u2
      (if (<= reputation-score u6000)
        u3
        (if (<= reputation-score u8000)
          u4
          u5
        )
      )
    )
  )
)

(define-private (calculate-reputation-boost (reputation-score uint))
  (let
    (
      (base-boost (/ (* reputation-score u50) u10000))
      (max-boost (var-get max-reputation-boost))
    )
    (if (< base-boost max-boost) base-boost max-boost)
  )
)

(define-public (fund-reward-pool (amount uint))
  (begin
    (try! (ft-transfer? customs-token amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
    (ok true)
  )
)

(define-public (distribute-proposal-rewards (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
      (existing-reward (map-get? proposal-rewards { proposal-id: proposal-id }))
      (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
      (proposal-passed (get passed proposal))
      (proposer (get proposer proposal))
    )
    (asserts! (get executed proposal) err-invalid-proposal)
    (asserts! (is-none existing-reward) err-reward-already-claimed)
    (asserts! (> total-votes u0) err-invalid-proposal)
    
    (let
      (
        (base-reward (if proposal-passed (var-get proposal-reward-rate) (/ (var-get proposal-reward-rate) u2)))
        (voter-pool (/ (* total-votes (var-get voter-reward-rate)) u100))
        (total-reward-needed (+ base-reward voter-pool))
      )
      (asserts! (<= total-reward-needed (var-get reward-pool-balance)) err-insufficient-reward-pool)
      
      (map-set proposal-rewards
        { proposal-id: proposal-id }
        {
          total-reward-pool: total-reward-needed,
          proposer-reward: base-reward,
          voter-reward-pool: voter-pool,
          rewards-claimed: false,
          eligible-voters: (count-proposal-voters proposal-id)
        }
      )
      
      (map-set member-rewards
        { member: proposer, proposal-id: proposal-id }
        {
          reward-amount: base-reward,
          claimed: false,
          participation-type: "proposer"
        }
      )
      
      (var-set reward-pool-balance (- (var-get reward-pool-balance) total-reward-needed))
      (ok true)
    )
  )
)

(define-public (claim-proposal-reward (proposal-id uint))
  (let
    (
      (member-reward (unwrap! (map-get? member-rewards { member: tx-sender, proposal-id: proposal-id }) err-not-found))
      (proposal-reward-data (unwrap! (map-get? proposal-rewards { proposal-id: proposal-id }) err-not-found))
    )
    (asserts! (not (get claimed member-reward)) err-reward-already-claimed)
    
    (let
      (
        (reward-amount (get reward-amount member-reward))
      )
      (try! (as-contract (ft-transfer? customs-token reward-amount tx-sender tx-sender)))
      
      (map-set member-rewards
        { member: tx-sender, proposal-id: proposal-id }
        (merge member-reward { claimed: true })
      )
      (ok reward-amount)
    )
  )
)

(define-public (calculate-voter-reward (proposal-id uint) (voter principal))
  (let
    (
      (vote-data (unwrap! (map-get? votes { proposal-id: proposal-id, voter: voter }) err-not-found))
      (proposal-reward-data (unwrap! (map-get? proposal-rewards { proposal-id: proposal-id }) err-not-found))
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
    )
    (let
      (
        (vote-amount (get amount vote-data))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        (voter-pool (get voter-reward-pool proposal-reward-data))
        (voter-share (if (> total-votes u0) (/ (* vote-amount voter-pool) total-votes) u0))
      )
      (map-set member-rewards
        { member: voter, proposal-id: proposal-id }
        {
          reward-amount: voter-share,
          claimed: false,
          participation-type: "voter"
        }
      )
      (ok voter-share)
    )
  )
)

(define-public (batch-calculate-voter-rewards (proposal-id uint) (voters (list 50 principal)))
  (begin
    (fold batch-process-voter voters proposal-id)
    (ok true)
  )
)

(define-private (batch-process-voter (voter principal) (proposal-id-acc uint))
  (begin
    (unwrap-panic (calculate-voter-reward proposal-id-acc voter))
    proposal-id-acc
  )
)

(define-public (update-reward-rates (new-proposal-rate uint) (new-voter-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set proposal-reward-rate new-proposal-rate)
    (var-set voter-reward-rate new-voter-rate)
    (ok true)
  )
)

(define-private (count-proposal-voters (proposal-id uint))
  (let
    (
      (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
    )
    (+ (get yes-votes proposal) (get no-votes proposal))
  )
)

(define-public (amend-proposal (proposal-id uint) (new-duty-rate uint) (reason (string-ascii 256)))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
      (amendments (default-to { amendment-count: u0, last-amendment-block: u0 }
                              (map-get? proposal-amendments { proposal-id: proposal-id })))
      (amendment-id (+ (get amendment-count amendments) u1))
      (blocks-since-start (- stacks-block-height (get start-block proposal)))
    )
    (asserts! (is-eq tx-sender (get proposer proposal)) err-not-proposer)
    (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-ended)
    (asserts! (<= blocks-since-start (var-get amendment-window)) err-amendment-window-closed)
    (asserts! (< (get amendment-count amendments) (var-get max-amendments-per-proposal)) err-amendment-limit-reached)
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { target-duty-rate: new-duty-rate })
    )
    
    (map-set proposal-amendments
      { proposal-id: proposal-id }
      {
        amendment-count: amendment-id,
        last-amendment-block: stacks-block-height
      }
    )
    
    (map-set amendment-details
      { proposal-id: proposal-id, amendment-id: amendment-id }
      {
        new-duty-rate: new-duty-rate,
        reason: reason,
        amendment-block: stacks-block-height,
        vote-reset-count: u0
      }
    )
    
    (ok amendment-id)
  )
)

(define-public (update-amendment-parameters (new-window uint) (new-max-amendments uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set amendment-window new-window)
    (var-set max-amendments-per-proposal new-max-amendments)
    (ok true)
  )
)

(define-read-only (get-proposal-amendments (proposal-id uint))
  (map-get? proposal-amendments { proposal-id: proposal-id })
)

(define-read-only (get-amendment-details (proposal-id uint) (amendment-id uint))
  (map-get? amendment-details { proposal-id: proposal-id, amendment-id: amendment-id })
)

(define-read-only (get-amendment-parameters)
  {
    amendment-window: (var-get amendment-window),
    max-amendments-per-proposal: (var-get max-amendments-per-proposal)
  }
)

(define-read-only (can-amend-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) (err false)))
      (amendments (default-to { amendment-count: u0, last-amendment-block: u0 }
                              (map-get? proposal-amendments { proposal-id: proposal-id })))
      (blocks-since-start (- stacks-block-height (get start-block proposal)))
    )
    (ok (and 
      (is-eq tx-sender (get proposer proposal))
      (<= stacks-block-height (get end-block proposal))
      (<= blocks-since-start (var-get amendment-window))
      (< (get amendment-count amendments) (var-get max-amendments-per-proposal))
    ))
  )
)
