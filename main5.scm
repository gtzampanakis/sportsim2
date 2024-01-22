(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (date))
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (util))

(define conf-sim-start-date (date 2023 5 1))
(define conf-sim-end-date (date 2025 5 1))

(define conf-n-countries 3)
(define conf-n-teams-per-country 40)
(define conf-n-players-per-team 22)
(define conf-n-players-per-country 2000)
(define conf-n-managers-per-country 200)

(define-class <rec> ()
    (id #:init-keyword #:id))

(define-class <country> (<rec>)
    (name #:init-keyword #:name)
    (team-set #:init-keyword #:team-set #:init-thunk list)
    (player-set #:init-keyword #:player-set #:init-thunk list)
    (manager-set #:init-keyword #:manager-set #:init-thunk list))

(define-class <team> (<rec>)
    (name #:init-keyword #:name)
    (country #:init-keyword #:country))

(define-class <player> (<rec>)
    (name #:init-keyword #:name)
    (country #:init-keyword #:country))

(define-class <manager> (<rec>)
    (name #:init-keyword #:name)
    (country #:init-keyword #:country))

(define-class <contract> (<rec>)
    (team #:init-keyword #:team)
    (date-start #:init-keyword #:date-start)
    (date-end #:init-keyword #:date-end)
    (wage #:init-keyword #:wage)
    (status #:init-keyword #:status))

(define-class <player-contract> (<contract>)
    (player #:init-keyword #:player))

(define-class <manager-contract> (<contract>)
    (manager #:init-keyword #:manager))

(define (make-db) (make-hash-table))

(define (db-insert-rec db rec)
    (define name (class-name (class-of rec)))
    (define table (or (hash-ref db name) '()))
    (set! table (cons rec table))
    (hash-set! db name table))

(define last-id 0)
(define (get-id)
    (set! last-id (1+ last-id))
    last-id)

(define (connect rec other-rec one-attr many-attr)
    (slot-set! rec one-attr other-rec)
    (slot-set! other-rec many-attr
        (cons rec (slot-ref other-rec many-attr))))

(define (init-team db country)
    (for-each
        (lambda (_)
            (let* (
                    (team
                        (make <team>
                            #:id (get-id))))
                (db-insert-rec db team)
                (connect team country 'country 'team-set)))
        (range conf-n-teams-per-country)))

(define (init-player db country)
    (for-each
        (lambda (_)
            (let* (
                    (player
                        (make <player>
                            #:id (get-id))))
                (db-insert-rec db player)
                (connect player country 'country 'player-set)))
        (range conf-n-players-per-country)))

(define (init-manager db country)
    (for-each
        (lambda (_)
            (let* (
                    (manager
                        (make <manager>
                            #:id (get-id))))
                (db-insert-rec db manager)
                (connect manager country 'country 'manager-set)))
        (range conf-n-managers-per-country)))

(define (symbol-append . symbols)
    (string->symbol
        (apply
            string-append
            (map symbol->string symbols))))

(define run-query
    (case-lambda
        ((db tables-in)
            (run-query db tables-in (lambda (rec) #t)))
        ((db tables-in filter-proc)
            (define tables
                (if (not (list? tables-in)) (list tables-in) tables-in))
            (define recs
                (let loop ((tables tables) (first #t) (results '()))
                    (if (null? tables)
                        results
                        (if first
                            (loop
                                (cdr tables)
                                #f
                                (let ((table (car tables)))
                                    (hash-ref db table)))
                            (loop
                                (cdr tables)
                                #f
                                (apply
                                    append
                                    (map
                                        (lambda (rec)
                                            (let (
                                                    (attr
                                                        (symbol-append
                                                            (car tables)
                                                            '-set)))
                                                (slot-ref rec attr)))
                                        results)))))))
            (filter filter-proc recs))))

(define combined-filter-proc
    (lambda filter-procs
        (lambda (rec)
            (let loop ((filter-procs filter-procs))
                (if (null? filter-procs)
                    #t
                    (let ((r ((car filter-procs) rec)))
                        (if r
                            (loop (cdr filter-procs))
                            #f)))))))

(define-syntax rec-spec-or-val
    (syntax-rules ()
        ((_ rec (table attr))
            (slot-ref rec 'attr))
        ((_ rec val)
            val)))

(define-syntax filter-proc-single
    (syntax-rules ()
        ((_ proc arg ...)
            (lambda (rec)
                (proc (rec-spec-or-val rec arg) ...)))))

(define-syntax filter-proc
    (syntax-rules ()
        ((_ (filter-proc-single-proc filter-proc-single-arg ...) ...)
            (combined-filter-proc
                (filter-proc-single
                    filter-proc-single-proc filter-proc-single-arg ...) ...))))

(define (initial-assign-player-to-team db player team)
    (db-insert-rec db
        (make <player-contract>
            #:id (get-id)
            #:player player
            #:team team
            #:date-start conf-sim-start-date
            #:date-end
                (add-days conf-sim-start-date (* 5 365))
            #:wage 1000.0
            #:status 'signed)))

(define (init-contract db country)
    (define teams
        (run-query db '<team>
            (lambda (t) (equal? (slot-ref t 'country) country))))
    (define players
        (run-query db '<player>
            (lambda (p) (equal? (slot-ref p 'country) country))))
    (let loop ((teams teams) (players players))
        (unless (null? teams)
            (for-each
                (lambda (player)
                    (initial-assign-player-to-team db player (car teams)))
                (take players conf-n-players-per-team))
            (loop (cdr teams) (drop players conf-n-players-per-team)))))

(define (init-sim db)
    (for-each
        (lambda (_)
            (let ((country (make <country> #:id (get-id))))
                (db-insert-rec db country)
                (init-team db country)
                (init-player db country)
                (init-manager db country)
                (init-contract db country)))
        (range conf-n-countries)))

(define (main)
    (define db (make-db))
    (init-sim db)
)

(main)
