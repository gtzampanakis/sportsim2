(use-modules (ice-9 match))
(use-modules (date))
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (util))

(define conf-sim-start-date (date 2023 5 1))
(define conf-sim-end-date (date 2025 5 1))

(define conf-n-countries 3)
(define conf-n-teams-per-country 40)
(define conf-n-players-per-country 2000)
(define conf-n-managers-per-country 200)

(define-class <node> ()
    (id #:init-keyword #:id))

(define-class <country> (<node>)
    (name #:init-keyword #:name)
    (teams #:init-keyword #:teams #:init-value '())
    (players #:init-keyword #:players #:init-value '())
    (managers #:init-keyword #:managers #:init-value '()))

(define-class <team> (<node>)
    (name #:init-keyword #:name)
    (country #:init-keyword #:country))

(define-class <player> (<node>)
    (name #:init-keyword #:name)
    (country #:init-keyword #:country))

(define-class <manager> (<node>)
    (name #:init-keyword #:name)
    (country #:init-keyword #:country))

(define-class <contract> (<node>)
    (team #:init-keyword #:team)
    (date-start #:init-keyword #:date-start)
    (date-end #:init-keyword #:date-end)
    (wage #:init-keyword #:wage)
    (status #:init-keyword #:status))

(define-class <player-contract> (<contract>)
    (player #:init-keyword #:player))

(define-class <manager-contract> (<contract>)
    (manager #:init-keyword #:manager))

(define (make-db)
    (list
        (cons 'nodes '())))

(define (db-nodes db)
    (assoc-ref db 'nodes))

(define (db-insert-node db node)
    (assoc-set! db 'nodes (cons node (db-nodes db))))

(define last-id 0)
(define (get-id)
    (set! last-id (1+ last-id))
    last-id)

(define (connect node other-node one-attr many-attr)
    (slot-set! node one-attr other-node)
    (slot-set! other-node many-attr (cons node (slot-ref other-node many-attr))))

(define (init-team db country)
    (for-each
        (lambda (_)
            (let* (
                    (team
                        (make <team>
                            #:id (get-id))))
                (db-insert-node db team)
                (connect team country 'country 'teams)))
        (range conf-n-teams-per-country)))

(define (init-player db country)
    (for-each
        (lambda (_)
            (let* (
                    (player
                        (make <player>
                            #:id (get-id))))
                (db-insert-node db player)
                (connect player country 'country 'players)))
        (range conf-n-players-per-country)))

(define (init-manager db country)
    (for-each
        (lambda (_)
            (let* (
                    (manager
                        (make <manager>
                            #:id (get-id))))
                (db-insert-node db manager)
                (connect manager country 'country 'managers)))
        (range conf-n-managers-per-country)))

(define (init-country db)
    (for-each
        (lambda (_)
            (let ((country (make <country> #:id (get-id))))
                (db-insert-node db country)
                (init-team db country)
                (init-player db country)
                (init-manager db country)))
        (range conf-n-countries)))

(define (init-sim db)
    (init-country db)
    (init-contract db))

(define (main)
    (define db (make-db))
    (init-sim db)
    (map describe (db-nodes db))
)


(main)
