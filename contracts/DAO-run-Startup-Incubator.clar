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
