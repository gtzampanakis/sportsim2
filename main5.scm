(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))
(use-modules (ice-9 match))
(use-modules (date))
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (util))

(define conf-sim-start-year 2023)
(define conf-sim-start-date (date conf-sim-start-year 5 1))
(define conf-sim-end-date (date (+ 2 conf-sim-start-year) 5 1))

(define conf-n-countries 3)
(define conf-n-teams-per-country 40)
(define conf-n-players-per-team 22)
(define conf-n-managers-per-team 1)
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

(define-class <competition> (<rec>)
    (id #:init-keyword #:id)
    (name #:init-keyword #:name)
    (start-month #:init-keyword #:start-month)
    (start-day #:init-keyword #:start-day))

(define-class <competition-instance> (<rec>)
    (id #:init-keyword #:id)
    (competition #:init-keyword #:competition)
    (country #:init-keyword #:country)
    (season #:init-keyword #:season))

(define-class <match> (<rec>)
    (id #:init-keyword #:id)
    (team-home #:init-keyword #:team-home)
    (team-away #:init-keyword #:team-away)
    (score-home #:init-keyword #:score-home)
    (score-away #:init-keyword #:score-away)
    (done #:init-keyword #:done)
    (datetime #:init-keyword #:datetime))

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

(define-syntax col-spec-or-val
    (syntax-rules (col-spec)
        ((_ result-set (col-spec alias col))
            (slot-ref
                (assoc-ref result-set alias)
                col))
        ((_ result-set (elem ...))
            ((col-spec-or-val result-set elem) ...))
        ((_ result-set val)
            val)))

(define-syntax filter-spec-to-proc
    (syntax-rules ()
        ((_ arg ...)
            (lambda (result-set-row)
                (col-spec-or-val result-set-row arg) ...))))

(define (symbol-strip-first-and-last sym)
    (string->symbol
        (let ((str (symbol->string sym)))
            (substring str 1 (1- (string-length str))))))

(define (rec-table-set rec table)
    (let (
            (attr
                (symbol-append
                    (symbol-strip-first-and-last table) '-set)))
        (slot-ref rec attr)))

(define query-results
    (case-lambda
        ((db tables-in)
            (query-results db tables-in #f))
        ((db tables-in filter-proc)
            (query-results db tables-in filter-proc #f))
        ((db tables-in filter-proc order-by)
            (query-results db tables-in filter-proc order-by #f))
        ((db tables-in filter-proc order-by limit)
            (define tables
                (if (not (list? tables-in)) (list tables-in) tables-in))
            (define result-set
                (let loop ((tables tables) (first #t) (results '()))
                    (if (null? tables)
                        results
                        (if first
                            (loop
                                (cdr tables)
                                #f
                                (let ((table (car tables)))
                                    (map
                                        (lambda (rec)
                                            (list (cons table rec)))
                                        (hash-ref db table))))
                            (loop
                                (cdr tables)
                                #f
                                (apply
                                    append
                                    (map
                                        (lambda (result)
                                            (map
                                                (lambda (set-element)
                                                    (cons
                                                        (cons
                                                            (car tables)
                                                            set-element)
                                                        result))
                                                (rec-table-set
                                                    (cdar result)
                                                    (car tables))))
                                        results)))))))
            (let (
                    (filtered
                        (if filter-proc
                            (filter filter-proc result-set)
                            result-set)))
                (let (
                        (ordered
                            (if order-by
                                (sort filtered order-by)
                                filtered)))
                    (let (
                            (limited
                                (if limit (take limit ordered) ordered)))
                        limited))))))

(define (query-exists db tables-in filter-proc)
    (not (null? (query-results db tables-in filter-proc #f 1))))

(define (initial-assign-player-to-team db player team)
    (db-insert-rec db
        (make <player-contract>
            #:id (get-id)
            #:player player
            #:team team
            #:date-start conf-sim-start-date
            ; this should go up to next 
            #:date-end
                (add-years conf-sim-start-date 5)
            #:wage 1000.0
            #:status 'signed)))

(define (initial-assign-manager-to-team db manager team)
    (db-insert-rec db
        (make <manager-contract>
            #:id (get-id)
            #:manager manager
            #:team team
            #:date-start conf-sim-start-date
            #:date-end
                (add-years conf-sim-start-date 5)
            #:wage 1000.0
            #:status 'signed)))

(define (init-player-contract db country)
    (define teams
        (query-results db '<team>
            (filter-spec-to-proc
                (equal? (col-spec '<team> 'country) country))))
    (define players
        (query-results db '<player>
            (filter-spec-to-proc
                (equal? (col-spec '<player> 'country) country))))
    (let loop ((teams teams) (players players))
        (unless (null? teams)
            (for-each
                (lambda (player)
                    (initial-assign-player-to-team db player (car teams)))
                (take players conf-n-players-per-team))
            (loop (cdr teams) (drop players conf-n-players-per-team)))))

(define (init-manager-contract db country)
    (define teams
        (query-results db '<team>
            (filter-spec-to-proc
                (equal? (col-spec '<team> 'country) country))))
    (define managers
        (query-results db '<manager>
            (filter-spec-to-proc
                (equal? (col-spec '<manager> 'country) country))))
    (let loop ((teams teams) (managers managers))
        (unless (null? teams)
            (for-each
                (lambda (manager)
                    (initial-assign-manager-to-team db manager (car teams)))
                (take managers conf-n-managers-per-team))
            (loop (cdr teams) (drop managers conf-n-managers-per-team)))))

(define (init-competition db)
    (db-insert-rec db
        (make <competition>
            #:id (get-id)
            #:name "League"
            #:start-month 8
            #:start-day 1)))

(define (init-competition-instance db country)
    (define competition
        (car
            (query-results db '<competition>
                (lambda (c) (equal? (slot-ref c 'name) "League")))))
    (db-insert-rec db
        (make <competition-instance>
            #:id (get-id)
            #:competition competition
            #:country country
            #:season conf-sim-start-year)))

(define (init-sim db)
    (init-competition db)
    (for-each
        (lambda (_)
            (let ((country (make <country> #:id (get-id))))
                (db-insert-rec db country)
                (init-team db country)
                (init-player db country)
                (init-manager db country)
                (init-player-contract db country)
                (init-manager-contract db country)))
        (range conf-n-countries)))

(define (next-date-with-given-month-day as-of month day)
    (define candidate (date (date-year as-of) month day))
    (if (date>=? candidate as-of)
        candidate
        (add-years candidate 1)))

(define (create-comp-instance-and-schedule-league
                        db season competition country)
    (define existing
        (query-results db
            '<competition-instance>
            (filter-spec-to-proc
                (and
                    (equal?
                        (col-spec
                            '<competition-instance> 'season season))
                    (equal?
                        (col-spec
                            '<competition-instance> 'country country))
                    (equal?
                        (col-spec
                            '<competition-instance> 'competition competition))))))
    (d 'existing existing))

(define (find-leagues-to-schedule db current-date)
    (for-each
        (lambda (result-set-row)
            (define country (assoc-ref result-set-row '<country>))
            (for-each
                (lambda (result-set-row)
                    (define competition
                        (assoc-ref result-set-row '<competition>))
                    (define next-season-start
                        (next-date-with-given-month-day
                            current-date
                            (slot-ref competition 'start-month)
                            (slot-ref competition 'start-day)))
                    (when
                        (<
                            (date-- next-season-start current-date)
                            (* 3 30 24 3600))
                        (create-comp-instance-and-schedule-league
                            db
                            (date-year next-season-start)
                            competition country)))
                (query-results db '<competition>
                    (filter-spec-to-proc
                        (equal? (col-spec '<competition> 'name) "League")))))
        (query-results db '<country>)))

(define (do-day db current-date)
    (find-leagues-to-schedule db current-date))

(define (main)
    (define db (make-db))
    (init-sim db)
    (let loop ((current-date conf-sim-start-date))
        (unless (equal? current-date conf-sim-end-date)
            (d "Current date" (iso-8601-date current-date))
            (do-day db current-date)
            (loop (add-day current-date)))))

(main)
