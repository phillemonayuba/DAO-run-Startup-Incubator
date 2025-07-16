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