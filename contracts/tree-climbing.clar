;; Digital Tree Climbing Safety System
;; Core functionality for tree suitability assessment and climber coordination

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-not-found (err u404))
(define-constant err-already-exists (err u409))

;; Tree suitability ratings (1-5 scale)
(define-map trees
  { tree-id: uint }
  {
    location: (string-ascii 100),
    species: (string-ascii 50),
    height: uint,
    safety-rating: uint,
    difficulty-level: uint,
    last-assessed: uint,
    assessor: principal
  }
)

;; Climber skill progression tracking
(define-map climbers
  { climber: principal }
  {
    skill-level: uint,
    climbs-completed: uint,
    safety-certified: bool,
    last-climb: uint
  }
)

;; Equipment sharing registry
(define-map equipment
  { equipment-id: uint }
  {
    owner: principal,
    equipment-type: (string-ascii 30),
    condition: uint,
    available: bool,
    last-inspected: uint
  }
)

(define-data-var next-tree-id uint u1)
(define-data-var next-equipment-id uint u1)

;; Register new tree with safety assessment
(define-public (register-tree (location (string-ascii 100)) (species (string-ascii 50)) (height uint) (safety-rating uint) (difficulty-level uint))
  (let ((tree-id (var-get next-tree-id)))
    (asserts! (<= safety-rating u5) (err u400))
    (asserts! (<= difficulty-level u5) (err u400))
    (asserts! (> height u0) (err u400))

    (map-set trees
      { tree-id: tree-id }
      {
        location: location,
        species: species,
        height: height,
        safety-rating: safety-rating,
        difficulty-level: difficulty-level,
        last-assessed: stacks-block-height,
        assessor: tx-sender
      }
    )
    (var-set next-tree-id (+ tree-id u1))
    (ok tree-id)
  )
)

;; Update climber profile after climb
(define-public (complete-climb (tree-id uint))
  (let ((climber-data (default-to
                        { skill-level: u1, climbs-completed: u0, safety-certified: false, last-climb: u0 }
                        (map-get? climbers { climber: tx-sender })))
        (tree-data (unwrap! (map-get? trees { tree-id: tree-id }) err-not-found)))

    ;; Safety check: climber skill must meet tree difficulty
    (asserts! (>= (get skill-level climber-data) (get difficulty-level tree-data)) (err u403))
    (asserts! (>= (get safety-rating tree-data) u3) (err u402))

    (map-set climbers
      { climber: tx-sender }
      (merge climber-data {
        climbs-completed: (+ (get climbs-completed climber-data) u1),
        last-climb: stacks-block-height,
        skill-level: (if (> (+ (get climbs-completed climber-data) u1) (* (get skill-level climber-data) u3))
                        (if (< (get skill-level climber-data) u5) (+ (get skill-level climber-data) u1) u5)
                        (get skill-level climber-data))
      })
    )
    (ok true)
  )
)

;; Register equipment for sharing
(define-public (register-equipment (equipment-type (string-ascii 30)) (condition uint))
  (let ((equipment-id (var-get next-equipment-id)))
    (asserts! (<= condition u5) (err u400))
    (asserts! (> condition u0) (err u400))

    (map-set equipment
      { equipment-id: equipment-id }
      {
        owner: tx-sender,
        equipment-type: equipment-type,
        condition: condition,
        available: true,
        last-inspected: stacks-block-height
      }
    )
    (var-set next-equipment-id (+ equipment-id u1))
    (ok equipment-id)
  )
)

;; Toggle equipment availability
(define-public (set-equipment-availability (equipment-id uint) (available bool))
  (let ((equipment-data (unwrap! (map-get? equipment { equipment-id: equipment-id }) err-not-found)))
    (asserts! (is-eq (get owner equipment-data) tx-sender) err-unauthorized)

    (map-set equipment
      { equipment-id: equipment-id }
      (merge equipment-data { available: available })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-tree (tree-id uint))
  (map-get? trees { tree-id: tree-id })
)

(define-read-only (get-climber (climber principal))
  (map-get? climbers { climber: climber })
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment { equipment-id: equipment-id })
)

(define-read-only (get-safe-trees-for-skill (skill-level uint))
  (ok skill-level)
)
