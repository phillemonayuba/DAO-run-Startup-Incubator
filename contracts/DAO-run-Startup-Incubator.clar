(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-STARTUP-EXISTS (err u102))
(define-constant ERR-STARTUP-NOT-FOUND (err u103))
(define-constant ERR-MENTOR-EXISTS (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-MILESTONE-NOT-FOUND (err u106))

(define-data-var dao-owner principal tx-sender)
(define-data-var total-startups uint u0)
(define-data-var total-mentors uint u0)
(define-data-var min-voting-power uint u100)

(define-map Startups 
    { id: uint }
    {
        founder: principal,
        name: (string-ascii 50),
        description: (string-ascii 500),
        funding-requested: uint,
        funding-received: uint,
        status: (string-ascii 20),
        votes: uint,
        milestone-count: uint
    }
)

(define-map Mentors
    { principal: principal }
    {
        name: (string-ascii 50),
        expertise: (string-ascii 100),
        token-balance: uint,
        startups-mentored: uint
    }
)

(define-map Milestones
    { startup-id: uint, milestone-id: uint }
    {
        description: (string-ascii 200),
        target-date: uint,
        funding-amount: uint,
        completed: bool
    }
)

(define-public (register-startup (name (string-ascii 50)) (description (string-ascii 500)) (funding uint))
    (let ((startup-id (var-get total-startups)))
        (asserts! (is-none (get-startup startup-id)) (err ERR-STARTUP-EXISTS))
        (map-set Startups
            { id: startup-id }
            {
                founder: tx-sender,
                name: name,
                description: description,
                funding-requested: funding,
                funding-received: u0,
                status: "pending",
                votes: u0,
                milestone-count: u0
            }
        )
        (var-set total-startups (+ startup-id u1))
        (ok startup-id)
    )
)

(define-public (register-mentor (name (string-ascii 50)) (expertise (string-ascii 100)))
    (begin
        (asserts! (is-none (get-mentor tx-sender)) (err ERR-MENTOR-EXISTS))
        (ok (map-set Mentors
            { principal: tx-sender }
            {
                name: name,
                expertise: expertise,
                token-balance: u100,
                startups-mentored: u0
            }
        ))
    )
)


(define-public (vote-startup (startup-id uint))
    (let ((mentor-data (unwrap! (get-mentor tx-sender) ERR-NOT-AUTHORIZED))
          (startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (current-votes (get votes startup)))
        (asserts! (>= (get token-balance mentor-data) (var-get min-voting-power)) ERR-NOT-AUTHORIZED)
        (map-set Startups
            { id: startup-id }
            (merge startup { votes: (+ current-votes u1) })
        )
        (ok true)
    )
)

(define-public (add-milestone (startup-id uint) (description (string-ascii 200)) (target-date uint) (funding uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (milestone-id (get milestone-count startup)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (map-set Milestones
            { startup-id: startup-id, milestone-id: milestone-id }
            {
                description: description,
                target-date: target-date,
                funding-amount: funding,
                completed: false
            }
        )
        (map-set Startups
            { id: startup-id }
            (merge startup { milestone-count: (+ milestone-id u1) })
        )
        (ok milestone-id)
    )
)

(define-public (complete-milestone (startup-id uint) (milestone-id uint))
    (let ((milestone (unwrap! (get-milestone startup-id milestone-id) ERR-MILESTONE-NOT-FOUND))
          (startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set Milestones
            { startup-id: startup-id, milestone-id: milestone-id }
            (merge milestone { completed: true })
        )
        (map-set Startups
            { id: startup-id }
            (merge startup 
                { funding-received: (+ (get funding-received startup) (get funding-amount milestone)) }
            )
        )
        (ok true)
    )
)

(define-read-only (get-startup (id uint))
    (map-get? Startups { id: id })
)

(define-read-only (get-mentor (who principal))
    (map-get? Mentors { principal: who })
)

(define-read-only (get-milestone (startup-id uint) (milestone-id uint))
    (map-get? Milestones { startup-id: startup-id, milestone-id: milestone-id })
)

(define-constant VOTE-REWARD u10)
(define-constant MENTOR-ASSIGNMENT-REWARD u50)
(define-constant MILESTONE-COMPLETION-REWARD u25)

(define-data-var total-rewards-pool uint u10000)

(define-map MentorRewards
    { mentor: principal }
    {
        pending-rewards: uint,
        total-earned: uint,
        last-claim-block: uint
    }
)

(define-map StartupMentorAssignments
    { startup-id: uint }
    { assigned-mentor: principal }
)

(define-public (assign-mentor-to-startup (startup-id uint) (mentor principal))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (mentor-data (unwrap! (get-mentor mentor) ERR-NOT-AUTHORIZED)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (map-set StartupMentorAssignments
            { startup-id: startup-id }
            { assigned-mentor: mentor }
        )
        (map-set Mentors
            { principal: mentor }
            (merge mentor-data { startups-mentored: (+ (get startups-mentored mentor-data) u1) })
        )
        (add-mentor-reward mentor MENTOR-ASSIGNMENT-REWARD)
        (ok true)
    )
)

(define-private (add-mentor-reward (mentor principal) (amount uint))
    (let ((current-rewards (default-to 
            { pending-rewards: u0, total-earned: u0, last-claim-block: u0 }
            (map-get? MentorRewards { mentor: mentor }))))
        (map-set MentorRewards
            { mentor: mentor }
            {
                pending-rewards: (+ (get pending-rewards current-rewards) amount),
                total-earned: (+ (get total-earned current-rewards) amount),
                last-claim-block: (get last-claim-block current-rewards)
            }
        )
    )
)

(define-public (claim-mentor-rewards)
    (let ((rewards (unwrap! (map-get? MentorRewards { mentor: tx-sender }) ERR-NOT-AUTHORIZED))
          (pending (get pending-rewards rewards)))
        (asserts! (> pending u0) ERR-INVALID-AMOUNT)
        (let ((mentor-data (unwrap! (get-mentor tx-sender) ERR-NOT-AUTHORIZED)))
            (map-set Mentors
                { principal: tx-sender }
                (merge mentor-data { token-balance: (+ (get token-balance mentor-data) pending) })
            )
            (map-set MentorRewards
                { mentor: tx-sender }
                (merge rewards { pending-rewards: u0, last-claim-block: stacks-block-height })
            )
            (var-set total-rewards-pool (- (var-get total-rewards-pool) pending))
            (ok pending)
        )
    )
)

(define-public (enhanced-vote-startup (startup-id uint))
    (let ((mentor-data (unwrap! (get-mentor tx-sender) ERR-NOT-AUTHORIZED))
          (startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (current-votes (get votes startup)))
        (asserts! (>= (get token-balance mentor-data) (var-get min-voting-power)) ERR-NOT-AUTHORIZED)
        (map-set Startups
            { id: startup-id }
            (merge startup { votes: (+ current-votes u1) })
        )
        (add-mentor-reward tx-sender VOTE-REWARD)
        (ok true)
    )
)

(define-public (enhanced-complete-milestone (startup-id uint) (milestone-id uint))
    (let ((milestone (unwrap! (get-milestone startup-id milestone-id) ERR-MILESTONE-NOT-FOUND))
          (startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (assignment (map-get? StartupMentorAssignments { startup-id: startup-id })))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set Milestones
            { startup-id: startup-id, milestone-id: milestone-id }
            (merge milestone { completed: true })
        )
        (map-set Startups
            { id: startup-id }
            (merge startup 
                { funding-received: (+ (get funding-received startup) (get funding-amount milestone)) }
            )
        )
        (match assignment
            mentor-assignment (add-mentor-reward (get assigned-mentor mentor-assignment) MILESTONE-COMPLETION-REWARD)
            true
        )
        (ok true)
    )
)

(define-read-only (get-mentor-rewards (mentor principal))
    (map-get? MentorRewards { mentor: mentor })
)

(define-read-only (get-startup-mentor (startup-id uint))
    (map-get? StartupMentorAssignments { startup-id: startup-id })
)

(define-constant ERR-ROUND-NOT-FOUND (err u107))
(define-constant ERR-ROUND-CLOSED (err u108))
(define-constant ERR-INSUFFICIENT-FUNDS (err u109))
(define-constant ERR-ROUND-ACTIVE (err u110))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u111))
(define-constant ERR-PROPOSAL-CLOSED (err u112))
(define-constant ERR-ALREADY-VOTED (err u113))
(define-constant ERR-PROPOSAL-ACTIVE (err u114))

(define-data-var proposal-id-nonce uint u0)
(define-data-var proposal-voting-period uint u1008)

(define-map Proposals
    { proposal-id: uint }
    {
        proposer: principal,
        proposal-type: (string-ascii 20),
        target-id: uint,
        description: (string-ascii 300),
        voting-deadline: uint,
        votes-for: uint,
        votes-against: uint,
        total-voting-power: uint,
        is-executed: bool,
        is-active: bool
    }
)

(define-map ProposalVotes
    { proposal-id: uint, voter: principal }
    {
        vote: bool,
        voting-power: uint,
        vote-timestamp: uint
    }
)

(define-public (create-proposal 
    (proposal-type (string-ascii 20)) 
    (target-id uint) 
    (description (string-ascii 300)))
    (let ((mentor-data (unwrap! (get-mentor tx-sender) ERR-NOT-AUTHORIZED))
          (proposal-id (var-get proposal-id-nonce))
          (deadline (+ stacks-block-height (var-get proposal-voting-period))))
        (asserts! (>= (get token-balance mentor-data) (var-get min-voting-power)) ERR-NOT-AUTHORIZED)
        (map-set Proposals
            { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                proposal-type: proposal-type,
                target-id: target-id,
                description: description,
                voting-deadline: deadline,
                votes-for: u0,
                votes-against: u0,
                total-voting-power: u0,
                is-executed: false,
                is-active: true
            }
        )
        (var-set proposal-id-nonce (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
    (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (mentor-data (unwrap! (get-mentor tx-sender) ERR-NOT-AUTHORIZED))
          (existing-vote (map-get? ProposalVotes { proposal-id: proposal-id, voter: tx-sender }))
          (voting-power (get token-balance mentor-data)))
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        (asserts! (get is-active proposal) ERR-PROPOSAL-CLOSED)
        (asserts! (< stacks-block-height (get voting-deadline proposal)) ERR-PROPOSAL-CLOSED)
        (asserts! (>= voting-power (var-get min-voting-power)) ERR-NOT-AUTHORIZED)
        (map-set ProposalVotes
            { proposal-id: proposal-id, voter: tx-sender }
            {
                vote: vote,
                voting-power: voting-power,
                vote-timestamp: stacks-block-height
            }
        )
        (map-set Proposals
            { proposal-id: proposal-id }
            (merge proposal {
                votes-for: (if vote (+ (get votes-for proposal) voting-power) (get votes-for proposal)),
                votes-against: (if vote (get votes-against proposal) (+ (get votes-against proposal) voting-power)),
                total-voting-power: (+ (get total-voting-power proposal) voting-power)
            })
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (get is-active proposal) ERR-PROPOSAL-CLOSED)
        (asserts! (>= stacks-block-height (get voting-deadline proposal)) ERR-PROPOSAL-ACTIVE)
        (asserts! (not (get is-executed proposal)) ERR-PROPOSAL-CLOSED)
        (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-NOT-AUTHORIZED)
        (map-set Proposals
            { proposal-id: proposal-id }
            (merge proposal { is-executed: true, is-active: false })
        )
        (if (is-eq (get proposal-type proposal) "approve-startup")
            (approve-startup-via-proposal (get target-id proposal))
            (if (is-eq (get proposal-type proposal) "complete-milestone")
                (complete-milestone-via-proposal (get target-id proposal))
                (ok true)
            )
        )
    )
)

(define-private (approve-startup-via-proposal (startup-id uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND)))
        (map-set Startups
            { id: startup-id }
            (merge startup { status: "approved" })
        )
        (ok true)
    )
)

(define-private (complete-milestone-via-proposal (milestone-info uint))
    (let ((startup-id (/ milestone-info u1000))
          (milestone-id (mod milestone-info u1000)))
        (enhanced-complete-milestone startup-id milestone-id)
    )
)

(define-public (close-proposal (proposal-id uint))
    (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (or (is-eq tx-sender (get proposer proposal)) 
                     (is-eq tx-sender (var-get dao-owner))) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active proposal) ERR-PROPOSAL-CLOSED)
        (map-set Proposals
            { proposal-id: proposal-id }
            (merge proposal { is-active: false })
        )
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? Proposals { proposal-id: proposal-id })
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
    (map-get? ProposalVotes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-status (proposal-id uint))
    (let ((proposal (unwrap! (get-proposal proposal-id) (err "not-found"))))
        (ok {
            is-active: (get is-active proposal),
            is-executed: (get is-executed proposal),
            voting-ended: (>= stacks-block-height (get voting-deadline proposal)),
            result: (if (> (get votes-for proposal) (get votes-against proposal)) "pass" "fail")
        })
    )
)

(define-map FundingRounds
    { startup-id: uint, round-id: uint }
    {
        round-name: (string-ascii 30),
        target-amount: uint,
        raised-amount: uint,
        min-investment: uint,
        max-investment: uint,
        deadline: uint,
        is-active: bool,
        investor-count: uint
    }
)

(define-map RoundInvestments
    { startup-id: uint, round-id: uint, investor: principal }
    {
        amount: uint,
        investment-date: uint
    }
)

(define-map StartupRoundCounts
    { startup-id: uint }
    { round-count: uint }
)

(define-public (create-funding-round 
    (startup-id uint) 
    (round-name (string-ascii 30)) 
    (target-amount uint) 
    (min-investment uint) 
    (max-investment uint) 
    (deadline uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (round-count-data (default-to { round-count: u0 } 
                           (map-get? StartupRoundCounts { startup-id: startup-id })))
          (round-id (get round-count round-count-data)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (asserts! (> target-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> deadline stacks-block-height) ERR-INVALID-AMOUNT)
        (map-set FundingRounds
            { startup-id: startup-id, round-id: round-id }
            {
                round-name: round-name,
                target-amount: target-amount,
                raised-amount: u0,
                min-investment: min-investment,
                max-investment: max-investment,
                deadline: deadline,
                is-active: true,
                investor-count: u0
            }
        )
        (map-set StartupRoundCounts
            { startup-id: startup-id }
            { round-count: (+ round-id u1) }
        )
        (ok round-id)
    )
)

(define-public (invest-in-round (startup-id uint) (round-id uint) (amount uint))
    (let ((round (unwrap! (get-funding-round startup-id round-id) ERR-ROUND-NOT-FOUND))
          (existing-investment (map-get? RoundInvestments 
                               { startup-id: startup-id, round-id: round-id, investor: tx-sender })))
        (asserts! (get is-active round) ERR-ROUND-CLOSED)
        (asserts! (< stacks-block-height (get deadline round)) ERR-ROUND-CLOSED)
        (asserts! (>= amount (get min-investment round)) ERR-INVALID-AMOUNT)
        (asserts! (<= amount (get max-investment round)) ERR-INVALID-AMOUNT)
        (let ((new-total (+ (get raised-amount round) amount))
              (is-new-investor (is-none existing-investment)))
            (asserts! (<= new-total (get target-amount round)) ERR-INVALID-AMOUNT)
            (map-set RoundInvestments
                { startup-id: startup-id, round-id: round-id, investor: tx-sender }
                {
                    amount: (+ amount (default-to u0 (get amount existing-investment))),
                    investment-date: stacks-block-height
                }
            )
            (map-set FundingRounds
                { startup-id: startup-id, round-id: round-id }
                (merge round {
                    raised-amount: new-total,
                    investor-count: (if is-new-investor 
                                   (+ (get investor-count round) u1) 
                                   (get investor-count round))
                })
            )
            (ok true)
        )
    )
)

(define-public (close-funding-round (startup-id uint) (round-id uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (round (unwrap! (get-funding-round startup-id round-id) ERR-ROUND-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active round) ERR-ROUND-CLOSED)
        (map-set FundingRounds
            { startup-id: startup-id, round-id: round-id }
            (merge round { is-active: false })
        )
        (map-set Startups
            { id: startup-id }
            (merge startup { 
                funding-received: (+ (get funding-received startup) (get raised-amount round))
            })
        )
        (ok (get raised-amount round))
    )
)

(define-public (emergency-close-round (startup-id uint) (round-id uint))
    (let ((round (unwrap! (get-funding-round startup-id round-id) ERR-ROUND-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active round) ERR-ROUND-CLOSED)
        (map-set FundingRounds
            { startup-id: startup-id, round-id: round-id }
            (merge round { is-active: false })
        )
        (ok true)
    )
)

(define-read-only (get-funding-round (startup-id uint) (round-id uint))
    (map-get? FundingRounds { startup-id: startup-id, round-id: round-id })
)

(define-read-only (get-investment (startup-id uint) (round-id uint) (investor principal))
    (map-get? RoundInvestments { startup-id: startup-id, round-id: round-id, investor: investor })
)

(define-read-only (get-startup-round-count (startup-id uint))
    (default-to { round-count: u0 } (map-get? StartupRoundCounts { startup-id: startup-id }))
)

(define-constant ERR-INVALID-SCORE (err u115))
(define-constant ERR-INSUFFICIENT-DATA (err u116))
(define-constant ERR-BADGE-EXISTS (err u117))
(define-constant ERR-BADGE-NOT-FOUND (err u118))
(define-constant ERR-REQUIREMENT-NOT-MET (err u119))

(define-data-var performance-update-threshold uint u30)

(define-map StartupPerformance
    { startup-id: uint }
    {
        milestones-completed: uint,
        milestones-overdue: uint,
        total-funding-efficiency: uint,
        mentor-satisfaction-score: uint,
        investor-confidence-score: uint,
        overall-performance-score: uint,
        last-updated: uint,
        performance-trend: (string-ascii 10)
    }
)

(define-map PerformanceMetrics
    { startup-id: uint, metric-type: (string-ascii 20) }
    {
        value: uint,
        recorded-at: uint,
        recorded-by: principal
    }
)

(define-map StartupScoreHistory
    { startup-id: uint, score-id: uint }
    {
        score: uint,
        timestamp: uint,
        factors: (string-ascii 200)
    }
)

(define-map StartupScoreCounts
    { startup-id: uint }
    { score-count: uint }
)

(define-public (update-startup-performance (startup-id uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (current-performance (default-to 
            {
                milestones-completed: u0,
                milestones-overdue: u0,
                total-funding-efficiency: u0,
                mentor-satisfaction-score: u50,
                investor-confidence-score: u50,
                overall-performance-score: u50,
                last-updated: u0,
                performance-trend: "stable"
            }
            (map-get? StartupPerformance { startup-id: startup-id }))))
        (asserts! (or (is-eq tx-sender (get founder startup))
                     (is-eq tx-sender (var-get dao-owner))
                     (is-some (get-mentor tx-sender))) ERR-NOT-AUTHORIZED)
        (let ((milestone-score (calculate-milestone-score startup-id))
              (funding-score (calculate-funding-efficiency startup-id))
              (overall-score (/ (+ milestone-score funding-score 
                                 (get mentor-satisfaction-score current-performance)
                                 (get investor-confidence-score current-performance)) u4)))
            (map-set StartupPerformance
                { startup-id: startup-id }
                {
                    milestones-completed: (get-completed-milestones-count startup-id),
                    milestones-overdue: (get-overdue-milestones-count startup-id),
                    total-funding-efficiency: funding-score,
                    mentor-satisfaction-score: (get mentor-satisfaction-score current-performance),
                    investor-confidence-score: (get investor-confidence-score current-performance),
                    overall-performance-score: overall-score,
                    last-updated: stacks-block-height,
                    performance-trend: (determine-performance-trend overall-score 
                                      (get overall-performance-score current-performance))
                }
            )
            (record-score-history startup-id overall-score)
            (ok overall-score)
        )
    )
)

(define-public (record-performance-metric 
    (startup-id uint) 
    (metric-type (string-ascii 20)) 
    (value uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND)))
        (asserts! (or (is-eq tx-sender (get founder startup))
                     (is-eq tx-sender (var-get dao-owner))
                     (is-some (get-mentor tx-sender))) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= value u0) (<= value u100)) ERR-INVALID-SCORE)
        (map-set PerformanceMetrics
            { startup-id: startup-id, metric-type: metric-type }
            {
                value: value,
                recorded-at: stacks-block-height,
                recorded-by: tx-sender
            }
        )
        (ok true)
    )
)

(define-public (update-mentor-satisfaction (startup-id uint) (score uint))
    (let ((assignment (unwrap! (get-startup-mentor startup-id) ERR-NOT-AUTHORIZED))
          (current-performance (unwrap! (get-startup-performance startup-id) ERR-INSUFFICIENT-DATA)))
        (asserts! (is-eq tx-sender (get assigned-mentor assignment)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= score u0) (<= score u100)) ERR-INVALID-SCORE)
        (map-set StartupPerformance
            { startup-id: startup-id }
            (merge current-performance { mentor-satisfaction-score: score })
        )
        (ok true)
    )
)

(define-public (update-investor-confidence (startup-id uint) (score uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (current-performance (unwrap! (get-startup-performance startup-id) ERR-INSUFFICIENT-DATA)))
        (asserts! (is-some (get-investment startup-id u0 tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= score u0) (<= score u100)) ERR-INVALID-SCORE)
        (map-set StartupPerformance
            { startup-id: startup-id }
            (merge current-performance { investor-confidence-score: score })
        )
        (ok true)
    )
)

(define-private (calculate-milestone-score (startup-id uint))
    (let ((completed (get-completed-milestones-count startup-id))
          (overdue (get-overdue-milestones-count startup-id))
          (total (+ completed overdue)))
        (if (is-eq total u0)
            u50
            (/ (* completed u100) total)
        )
    )
)

(define-private (calculate-funding-efficiency (startup-id uint))
    (match (get-startup startup-id)
        startup (let ((requested (get funding-requested startup))
                     (received (get funding-received startup)))
                    (if (is-eq requested u0)
                        u50
                        (let ((efficiency (/ (* received u100) requested)))
                            (if (> efficiency u100) u100 efficiency)
                        )
                    )
                )
        u50
    )
)

(define-private (get-completed-milestones-count (startup-id uint))
    (match (get-startup startup-id)
        startup (get count (fold count-completed-milestones 
                           (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
                           { startup-id: startup-id, count: u0, total: (get milestone-count startup) }))
        u0
    )
)

(define-private (get-overdue-milestones-count (startup-id uint))
    (match (get-startup startup-id)
        startup (get count (fold count-overdue-milestones 
                           (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
                           { startup-id: startup-id, count: u0, total: (get milestone-count startup) }))
        u0
    )
)

(define-private (count-completed-milestones (milestone-idx uint) (acc { startup-id: uint, count: uint, total: uint }))
    (if (< milestone-idx (get total acc))
        (let ((milestone (get-milestone (get startup-id acc) milestone-idx)))
            (if (and (is-some milestone) (get completed (unwrap-panic milestone)))
                (merge acc { count: (+ (get count acc) u1) })
                acc
            )
        )
        acc
    )
)

(define-private (count-overdue-milestones (milestone-idx uint) (acc { startup-id: uint, count: uint, total: uint }))
    (if (< milestone-idx (get total acc))
        (let ((milestone (get-milestone (get startup-id acc) milestone-idx)))
            (if (and (is-some milestone) 
                    (not (get completed (unwrap-panic milestone)))
                    (< (get target-date (unwrap-panic milestone)) stacks-block-height))
                (merge acc { count: (+ (get count acc) u1) })
                acc
            )
        )
        acc
    )
)

(define-private (determine-performance-trend (current-score uint) (previous-score uint))
    (if (> current-score (+ previous-score u10))
        "improving"
        (if (< current-score (- previous-score u10))
            "declining"
            "stable"
        )
    )
)

(define-private (record-score-history (startup-id uint) (score uint))
    (let ((score-count-data (default-to { score-count: u0 } 
                           (map-get? StartupScoreCounts { startup-id: startup-id })))
          (score-id (get score-count score-count-data)))
        (map-set StartupScoreHistory
            { startup-id: startup-id, score-id: score-id }
            {
                score: score,
                timestamp: stacks-block-height,
                factors: "milestone,funding,mentor,investor"
            }
        )
        (map-set StartupScoreCounts
            { startup-id: startup-id }
            { score-count: (+ score-id u1) }
        )
    )
)

(define-read-only (get-startup-performance (startup-id uint))
    (map-get? StartupPerformance { startup-id: startup-id })
)

(define-read-only (get-performance-metric (startup-id uint) (metric-type (string-ascii 20)))
    (map-get? PerformanceMetrics { startup-id: startup-id, metric-type: metric-type })
)

(define-read-only (get-score-history (startup-id uint) (score-id uint))
    (map-get? StartupScoreHistory { startup-id: startup-id, score-id: score-id })
)

(define-read-only (get-startup-rankings (min-score uint))
    (ok {
        threshold: min-score,
        ranking-criteria: "overall-performance-score",
        last-updated: stacks-block-height
    })
)

(define-read-only (analyze-startup-performance (startup-id uint))
    (let ((performance (unwrap! (get-startup-performance startup-id) (err "no-data")))
          (startup (unwrap! (get-startup startup-id) (err "not-found"))))
        (ok {
            startup-name: (get name startup),
            overall-score: (get overall-performance-score performance),
            milestone-completion-rate: (let ((total-milestones (+ (get milestones-completed performance) 
                                                  (get milestones-overdue performance))))
                                        (if (is-eq total-milestones u0)
                                            u0
                                            (/ (* (get milestones-completed performance) u100) total-milestones))),
            funding-efficiency: (get total-funding-efficiency performance),
            performance-trend: (get performance-trend performance),
            mentor-satisfaction: (get mentor-satisfaction-score performance),
            investor-confidence: (get investor-confidence-score performance),
            last-assessment: (get last-updated performance)
        })
    )
)

(define-map StartupReputation
    { startup-id: uint }
    {
        reputation-score: uint,
        total-badges: uint,
        tier: (string-ascii 20),
        achievements-unlocked: uint,
        last-tier-update: uint
    }
)

(define-map StartupBadges
    { startup-id: uint, badge-type: (string-ascii 30) }
    {
        earned-at: uint,
        badge-level: uint,
        is-active: bool
    }
)

(define-map BadgeDefinitions
    { badge-type: (string-ascii 30) }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        requirement-type: (string-ascii 30),
        requirement-value: uint,
        reputation-points: uint,
        max-level: uint
    }
)

(define-data-var badge-types-initialized bool false)

(define-public (initialize-badge-system)
    (begin
        (asserts! (not (var-get badge-types-initialized)) ERR-BADGE-EXISTS)
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set BadgeDefinitions
            { badge-type: "first-milestone" }
            {
                name: "Milestone Pioneer",
                description: "Complete your first milestone",
                requirement-type: "milestones-completed",
                requirement-value: u1,
                reputation-points: u50,
                max-level: u1
            }
        )
        (map-set BadgeDefinitions
            { badge-type: "funding-champion" }
            {
                name: "Funding Champion",
                description: "Reach 100% funding goal",
                requirement-type: "funding-efficiency",
                requirement-value: u100,
                reputation-points: u100,
                max-level: u3
            }
        )
        (map-set BadgeDefinitions
            { badge-type: "milestone-master" }
            {
                name: "Milestone Master",
                description: "Complete 5 milestones",
                requirement-type: "milestones-completed",
                requirement-value: u5,
                reputation-points: u150,
                max-level: u5
            }
        )
        (map-set BadgeDefinitions
            { badge-type: "high-performer" }
            {
                name: "High Performer",
                description: "Achieve performance score above 80",
                requirement-type: "performance-score",
                requirement-value: u80,
                reputation-points: u200,
                max-level: u3
            }
        )
        (map-set BadgeDefinitions
            { badge-type: "investor-favorite" }
            {
                name: "Investor Favorite",
                description: "Attract 10 or more investors",
                requirement-type: "investor-count",
                requirement-value: u10,
                reputation-points: u120,
                max-level: u3
            }
        )
        (var-set badge-types-initialized true)
        (ok true)
    )
)

(define-public (check-and-award-badges (startup-id uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (performance (get-startup-performance startup-id))
          (current-reputation (default-to 
            {
                reputation-score: u0,
                total-badges: u0,
                tier: "bronze",
                achievements-unlocked: u0,
                last-tier-update: u0
            }
            (map-get? StartupReputation { startup-id: startup-id }))))
        (asserts! (or (is-eq tx-sender (get founder startup))
                     (is-eq tx-sender (var-get dao-owner))) ERR-NOT-AUTHORIZED)
        (let ((badges-awarded (check-all-badge-requirements startup-id startup performance)))
            (update-reputation-tier startup-id current-reputation)
            (ok badges-awarded)
        )
    )
)

(define-private (check-all-badge-requirements 
    (startup-id uint) 
    (startup { founder: principal, name: (string-ascii 50), description: (string-ascii 500), 
               funding-requested: uint, funding-received: uint, status: (string-ascii 20), 
               votes: uint, milestone-count: uint })
    (performance (optional { milestones-completed: uint, milestones-overdue: uint, 
                            total-funding-efficiency: uint, mentor-satisfaction-score: uint, 
                            investor-confidence-score: uint, overall-performance-score: uint, 
                            last-updated: uint, performance-trend: (string-ascii 10) })))
    (let ((perf-data (default-to 
            { milestones-completed: u0, milestones-overdue: u0, total-funding-efficiency: u0, 
              mentor-satisfaction-score: u0, investor-confidence-score: u0, 
              overall-performance-score: u0, last-updated: u0, performance-trend: "stable" } 
            performance))
          (milestones-completed (get milestones-completed perf-data))
          (funding-efficiency (get total-funding-efficiency perf-data))
          (overall-score (get overall-performance-score perf-data)))
        (begin
            (if (>= milestones-completed u1)
                (try! (award-badge startup-id "first-milestone" u1))
                true
            )
            (if (>= funding-efficiency u100)
                (try! (award-badge startup-id "funding-champion" u1))
                true
            )
            (if (>= milestones-completed u5)
                (try! (award-badge startup-id "milestone-master" u1))
                true
            )
            (if (>= overall-score u80)
                (try! (award-badge startup-id "high-performer" u1))
                true
            )
            (ok true)
        )
    )
)

(define-private (award-badge (startup-id uint) (badge-type (string-ascii 30)) (level uint))
    (let ((existing-badge (map-get? StartupBadges { startup-id: startup-id, badge-type: badge-type }))
          (badge-def (unwrap! (get-badge-definition badge-type) ERR-BADGE-NOT-FOUND))
          (current-reputation (default-to 
            {
                reputation-score: u0,
                total-badges: u0,
                tier: "bronze",
                achievements-unlocked: u0,
                last-tier-update: u0
            }
            (map-get? StartupReputation { startup-id: startup-id }))))
        (if (is-none existing-badge)
            (begin
                (map-set StartupBadges
                    { startup-id: startup-id, badge-type: badge-type }
                    {
                        earned-at: stacks-block-height,
                        badge-level: level,
                        is-active: true
                    }
                )
                (map-set StartupReputation
                    { startup-id: startup-id }
                    (merge current-reputation {
                        reputation-score: (+ (get reputation-score current-reputation) 
                                           (get reputation-points badge-def)),
                        total-badges: (+ (get total-badges current-reputation) u1),
                        achievements-unlocked: (+ (get achievements-unlocked current-reputation) u1)
                    })
                )
                (ok true)
            )
            (ok false)
        )
    )
)

(define-private (update-reputation-tier 
    (startup-id uint) 
    (reputation { reputation-score: uint, total-badges: uint, tier: (string-ascii 20), 
                  achievements-unlocked: uint, last-tier-update: uint }))
    (let ((score (get reputation-score reputation))
          (new-tier (if (>= score u500)
                        "platinum"
                        (if (>= score u300)
                            "gold"
                            (if (>= score u150)
                                "silver"
                                "bronze")))))
        (map-set StartupReputation
            { startup-id: startup-id }
            (merge reputation {
                tier: new-tier,
                last-tier-update: stacks-block-height
            })
        )
    )
)

(define-public (revoke-badge (startup-id uint) (badge-type (string-ascii 30)))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (badge (unwrap! (get-startup-badge startup-id badge-type) ERR-BADGE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set StartupBadges
            { startup-id: startup-id, badge-type: badge-type }
            (merge badge { is-active: false })
        )
        (ok true)
    )
)

(define-read-only (get-startup-reputation (startup-id uint))
    (map-get? StartupReputation { startup-id: startup-id })
)

(define-read-only (get-startup-badge (startup-id uint) (badge-type (string-ascii 30)))
    (map-get? StartupBadges { startup-id: startup-id, badge-type: badge-type })
)

(define-read-only (get-badge-definition (badge-type (string-ascii 30)))
    (map-get? BadgeDefinitions { badge-type: badge-type })
)

(define-read-only (get-startup-profile (startup-id uint))
    (let ((startup (unwrap! (get-startup startup-id) (err "not-found")))
          (reputation-data (default-to 
                { reputation-score: u0, total-badges: u0, tier: "bronze", 
                  achievements-unlocked: u0, last-tier-update: u0 } 
                (get-startup-reputation startup-id)))
          (performance-data (default-to 
                { milestones-completed: u0, milestones-overdue: u0, total-funding-efficiency: u0, 
                  mentor-satisfaction-score: u0, investor-confidence-score: u0, 
                  overall-performance-score: u0, last-updated: u0, performance-trend: "stable" } 
                (get-startup-performance startup-id))))
        (ok {
            name: (get name startup),
            status: (get status startup),
            reputation-tier: (get tier reputation-data),
            reputation-score: (get reputation-score reputation-data),
            total-badges: (get total-badges reputation-data),
            performance-score: (get overall-performance-score performance-data),
            funding-received: (get funding-received startup),
            votes: (get votes startup)
        })
    )
)

(define-constant ERR-EXIT-EXISTS (err u120))
(define-constant ERR-EXIT-NOT-FOUND (err u121))
(define-constant ERR-EXIT-NOT-APPROVED (err u122))
(define-constant ERR-ALREADY-DISTRIBUTED (err u123))
(define-constant ERR-NO-INVESTMENT (err u124))

(define-map StartupExits
    { startup-id: uint }
    {
        exit-type: (string-ascii 20),
        exit-valuation: uint,
        exit-date: uint,
        total-proceeds: uint,
        is-approved: bool,
        is-distributed: bool,
        approved-by: (optional principal)
    }
)

(define-map InvestorDistributions
    { startup-id: uint, investor: principal }
    {
        total-invested: uint,
        distribution-amount: uint,
        return-multiple: uint,
        claimed: bool,
        claim-date: (optional uint)
    }
)

(define-public (propose-exit (startup-id uint) (exit-type (string-ascii 20)) (valuation uint) (proceeds uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? StartupExits { startup-id: startup-id })) ERR-EXIT-EXISTS)
        (asserts! (> valuation u0) ERR-INVALID-AMOUNT)
        (asserts! (> proceeds u0) ERR-INVALID-AMOUNT)
        (map-set StartupExits
            { startup-id: startup-id }
            {
                exit-type: exit-type,
                exit-valuation: valuation,
                exit-date: stacks-block-height,
                total-proceeds: proceeds,
                is-approved: false,
                is-distributed: false,
                approved-by: none
            }
        )
        (ok true)
    )
)

(define-public (approve-exit (startup-id uint))
    (let ((exit-data (unwrap! (map-get? StartupExits { startup-id: startup-id }) ERR-EXIT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-approved exit-data)) ERR-EXIT-EXISTS)
        (map-set StartupExits
            { startup-id: startup-id }
            (merge exit-data {
                is-approved: true,
                approved-by: (some tx-sender)
            })
        )
        (map-set Startups
            { id: startup-id }
            (merge (unwrap-panic (get-startup startup-id)) { status: "exited" })
        )
        (ok true)
    )
)

(define-public (calculate-distributions (startup-id uint))
    (let ((exit-data (unwrap! (map-get? StartupExits { startup-id: startup-id }) ERR-EXIT-NOT-FOUND))
          (startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (round-count-data (get-startup-round-count startup-id))
          (total-rounds (get round-count round-count-data)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-approved exit-data) ERR-EXIT-NOT-APPROVED)
        (asserts! (not (get is-distributed exit-data)) ERR-ALREADY-DISTRIBUTED)
        (let ((total-raised (get funding-received startup)))
            (asserts! (> total-raised u0) ERR-INSUFFICIENT-FUNDS)
            (process-all-rounds startup-id total-rounds total-raised (get total-proceeds exit-data))
            (map-set StartupExits
                { startup-id: startup-id }
                (merge exit-data { is-distributed: true })
            )
            (ok true)
        )
    )
)

(define-private (process-all-rounds (startup-id uint) (total-rounds uint) (total-raised uint) (total-proceeds uint))
    (fold process-single-round
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
        { startup-id: startup-id, max-rounds: total-rounds, total-raised: total-raised, 
          total-proceeds: total-proceeds, success: true })
)

(define-private (process-single-round 
    (round-idx uint) 
    (acc { startup-id: uint, max-rounds: uint, total-raised: uint, total-proceeds: uint, success: bool }))
    (if (and (get success acc) (< round-idx (get max-rounds acc)))
        (let ((round-data (get-funding-round (get startup-id acc) round-idx)))
            (match round-data
                round (merge acc { success: true })
                acc
            )
        )
        acc
    )
)

(define-public (claim-distribution (startup-id uint))
    (let ((distribution (unwrap! (map-get? InvestorDistributions 
                                  { startup-id: startup-id, investor: tx-sender }) 
                                  ERR-NO-INVESTMENT))
          (exit-data (unwrap! (map-get? StartupExits { startup-id: startup-id }) ERR-EXIT-NOT-FOUND)))
        (asserts! (get is-distributed exit-data) ERR-EXIT-NOT-APPROVED)
        (asserts! (not (get claimed distribution)) ERR-ALREADY-DISTRIBUTED)
        (map-set InvestorDistributions
            { startup-id: startup-id, investor: tx-sender }
            (merge distribution {
                claimed: true,
                claim-date: (some stacks-block-height)
            })
        )
        (ok (get distribution-amount distribution))
    )
)

(define-public (record-investor-distribution 
    (startup-id uint) 
    (investor principal) 
    (invested uint) 
    (distribution uint))
    (let ((startup (unwrap! (get-startup startup-id) ERR-STARTUP-NOT-FOUND))
          (exit-data (unwrap! (map-get? StartupExits { startup-id: startup-id }) ERR-EXIT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get founder startup)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-approved exit-data) ERR-EXIT-NOT-APPROVED)
        (asserts! (> invested u0) ERR-INVALID-AMOUNT)
        (let ((return-multiple (if (> invested u0) (/ (* distribution u100) invested) u0)))
            (map-set InvestorDistributions
                { startup-id: startup-id, investor: investor }
                {
                    total-invested: invested,
                    distribution-amount: distribution,
                    return-multiple: return-multiple,
                    claimed: false,
                    claim-date: none
                }
            )
            (ok true)
        )
    )
)

(define-read-only (get-exit-details (startup-id uint))
    (map-get? StartupExits { startup-id: startup-id })
)

(define-read-only (get-investor-distribution (startup-id uint) (investor principal))
    (map-get? InvestorDistributions { startup-id: startup-id, investor: investor })
)

(define-read-only (calculate-investor-returns (startup-id uint) (investor principal))
    (let ((total-invested (calculate-total-investment startup-id investor))
          (exit-data (map-get? StartupExits { startup-id: startup-id })))
        (match exit-data
            exit (if (and (get is-approved exit) (> total-invested u0))
                    (let ((startup (unwrap! (get-startup startup-id) (err "not-found")))
                          (total-raised (get funding-received startup))
                          (investor-share (/ (* total-invested u10000) total-raised))
                          (distribution (/ (* (get total-proceeds exit) investor-share) u10000))
                          (return-multiple (/ (* distribution u100) total-invested)))
                        (ok {
                            invested: total-invested,
                            projected-distribution: distribution,
                            return-multiple: return-multiple,
                            ownership-percentage: investor-share
                        })
                    )
                    (err "insufficient-data")
                )
            (err "no-exit")
        )
    )
)

(define-private (calculate-total-investment (startup-id uint) (investor principal))
    (let ((round-count-data (get-startup-round-count startup-id))
          (total-rounds (get round-count round-count-data)))
        (get total (fold sum-investments
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
            { startup-id: startup-id, investor: investor, max-rounds: total-rounds, total: u0 }))
    )
)

(define-private (sum-investments 
    (round-idx uint) 
    (acc { startup-id: uint, investor: principal, max-rounds: uint, total: uint }))
    (if (< round-idx (get max-rounds acc))
        (let ((investment (get-investment (get startup-id acc) round-idx (get investor acc))))
            (match investment
                inv (merge acc { total: (+ (get total acc) (get amount inv)) })
                acc
            )
        )
        acc
    )
)
