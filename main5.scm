(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))
(use-modules (ice-9 match))
(use-modules (date))
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (util))

(define conf-sim-start-year 2023)
(define conf-sim-start-date (date conf-sim-start-year 5 1))
(define conf-sim-end-date (date (+ 3 conf-sim-start-year) 5 1))

(define conf-n-countries 3)
(define conf-n-teams-per-country 40)
(define conf-n-teams-per-division 14)
(define conf-n-players-per-team 22)
(define conf-n-players-in-match 11)
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

(define-class <attr> (<rec>)
    (date-start #:init-keyword #:date-start)
    (date-end #:init-keyword #:date-end))

(define-class <player-attr> (<attr>)
    (player #:init-keyword #:player)
    (att #:init-keyword #:att)
    (def #:init-keyword #:def)
    (vel #:init-keyword #:vel))

(define-class <manager-attr> (<attr>)
    (manager #:init-keyword #:manager)
    (jud #:init-keyword #:jud))

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

(define-class <competition-instance-team> (<rec>)
    (id #:init-keyword #:id)
    (competition-instance #:init-keyword #:competition-instance)
    (team #:init-keyword #:team))

(define-class <match> (<rec>)
    (id #:init-keyword #:id)
    (team-home #:init-keyword #:team-home)
    (team-away #:init-keyword #:team-away)
    (score-home #:init-keyword #:score-home)
    (score-away #:init-keyword #:score-away)
    (done #:init-keyword #:done)
    (datetime #:init-keyword #:datetime)
    (competition-instance #:init-keyword #:competition-instance))

(define (db-create-table db class)
    (hash-set! db (class-name class) '()))

(define (db-create-tables db)
    (for-each
        (lambda (class) (db-create-table db class))
        (list
            <country>
            <team>
            <player>
            <manager>
            <player-contract>
            <manager-contract>
            <competition>
            <competition-instance>
            <competition-instance-team>
            <match>
            <player-attr>
            <manager-attr>)))

(define (make-db)
    (define db (make-hash-table))
    (db-create-tables db)
    db)

(define (db-insert-rec db rec)
    (define name (class-name (class-of rec)))
    (define table (hash-ref db name))
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
                (db-insert-rec db
                    (make <player-attr>
                        #:player player
                        #:date-start conf-sim-start-date
                        #:date-end (add-days conf-sim-start-date (* 7 52 10))
                        #:att 1.0
                        #:def 1.0
                        #:vel 1.0))
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
                (db-insert-rec db
                    (make <manager-attr>
                        #:manager manager
                        #:date-start conf-sim-start-date
                        #:date-end (add-days conf-sim-start-date (* 7 4))
                        #:jud 1.0))
                (connect manager country 'country 'manager-set)))
        (range conf-n-managers-per-country)))

(define-syntax query-filter-proc
    (syntax-rules ()
        ((_ obj arg ...)
            (lambda (obj)
                (and arg ...)))))

(define (-query-results db table filter-proc order-by limit)
    (define objs (hash-ref db table))
    (let (
            (filtered
                (if filter-proc
                    (filter filter-proc objs)
                    objs)))
        (let (
                (ordered
                    (if order-by
                        (sort filtered order-by)
                        filtered)))
            (let (
                    (limited
                        (if limit
                            (take-n-or-fewer ordered limit) ordered)))
                limited))))

(define query-results
    (case-lambda
        ((db tables-in)
            (query-results db tables-in #f))
        ((db tables-in filter-proc)
            (query-results db tables-in filter-proc #f))
        ((db tables-in filter-proc order-by)
            (query-results db tables-in filter-proc order-by #f))
        ((db tables-in filter-proc order-by limit)
            (-query-results db tables-in filter-proc order-by limit))))

(define (query-exists db tables-in filter-proc)
    (not (null? (query-results db tables-in filter-proc #f 1))))

(define query-first
    (case-lambda
        ((db tables-in)
            (query-first db tables-in #f))
        ((db tables-in filter-proc)
            (query-first db tables-in filter-proc #f))
        ((db tables-in filter-proc order-by)
            (let ((results
                    (query-results db tables-in filter-proc order-by 1)))
                (if (null? results) results (car results))))))

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
            (query-filter-proc obj
                (equal?
                    (slot-ref obj 'country) country))))
    (define players
        (query-results db '<player>
            (query-filter-proc obj
                (equal? (slot-ref obj 'country) country))))
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
            (query-filter-proc obj
                (equal? (slot-ref obj 'country) country))))
    (define managers
        (query-results db '<manager>
            (query-filter-proc obj
                (equal? (slot-ref obj 'country) country))))
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

(define (gen-round-robin n)
; https://en.wikipedia.org/wiki/Round-robin_tournament#Circle_method
    ; Call cdr to cycle once. This makes the schedule nicer-looking by having the
    ; 0 play the opponents in order.
    (define cycle (cdr (range-cycle 1 n)))
    (define first-round-days
        (let loop-days ((cycle cycle) (i 0) (r '()) (played-home-last-day '()))
            (if (< i (1- n))
                (let ((full (cons 0 cycle)))
                    (let (
                        (day-pairs
                            (map
                                (lambda (k)
                                    (let (
                                            (team0 (list-ref full k))
                                            (team1 (list-ref full (- n 1 k))))
                                        (if
                                            (and
                                                (memq
                                                    team0 played-home-last-day)
                                                (not
                                                    (memq
                                                        team1
                                                        played-home-last-day)))
                                            (list team1 team0)
                                            (list team0 team1))))
                                (range 0 (/ n 2)))))
                        (loop-days
                            (cdr cycle)
                            (1+ i)
                            (cons day-pairs r)
                            (map car day-pairs))))
                r)))
    (append
        first-round-days
        (map
            (lambda (day-pairs)
                (map (lambda (match) (reverse match)) day-pairs))
            first-round-days)))

(define (
        teams-for-new-competition-instance
        db country last-competition-instance)
    (if (null? last-competition-instance)
        (query-results db
            '<team>
            (query-filter-proc obj
                (equal?
                    (slot-ref obj 'country)
                    country))
            #f
            conf-n-teams-per-division)
        (query-results db
            '<competition-instance-team>
            (query-filter-proc obj
                (equal?
                    (slot-ref obj 'competition-instance)
                    last-competition-instance)))))

(define (schedule-league-season db first-date competition-instance)
    (define teams
        (query-results db
            '<competition-instance-team>
            (query-filter-proc obj
                (equal?
                    (slot-ref obj 'competition-instance)
                    competition-instance))))
    (define schedule-days (gen-round-robin (length teams)))
    (for-each
        (lambda (day dayi)
            (for-each
                (lambda (match-teams)
                    (define match
                        (make <match>
                            #:id (get-id)
                            #:team-home
                                (slot-ref
                                    (list-ref teams (car match-teams))
                                    'team)
                            #:team-away
                                (slot-ref
                                    (list-ref teams (cadr match-teams))
                                    'team)
                            #:done 0
                            #:datetime (add-days first-date (* dayi 7))
                            #:competition-instance competition-instance))
                    (db-insert-rec db match))
                day))
        schedule-days
        (range 0 (length schedule-days))))

(define (create-comp-instance-and-schedule-league
                        db first-date competition country)
    (define season (date-year first-date))
    (define existing
        (query-exists db
            '<competition-instance>
            (query-filter-proc obj
                (equal?
                    (slot-ref obj 'season)
                    season)
                (equal?
                    (slot-ref obj 'country)
                    country)
                (equal?
                    (slot-ref obj 'competition)
                    competition))))
    (unless existing
        ; Find previous <competition-instance-team> records and copy them to
        ; this season (promotion and relegation not yet implemented).
        (let* (
            (new-competition-instance
                (make <competition-instance>
                    #:id (get-id)
                    #:season season
                    #:country country
                    #:competition competition))
            (last-competition-instance
                (query-first db
                    '<competition-instance>
                    (query-filter-proc obj
                        (and
                            (equal?
                                (slot-ref obj 'season)
                                (1- season))
                            (equal?
                                (slot-ref obj 'country)
                                country)
                            (equal?
                                (slot-ref obj 'competition)
                                competition)))))
            (teams (
                teams-for-new-competition-instance
                db country last-competition-instance)))
            (db-insert-rec db new-competition-instance)
            (for-each
                (lambda (team)
                    (db-insert-rec db
                        (make <competition-instance-team>
                            #:id (get-id)
                            #:competition-instance new-competition-instance
                            #:team team)))
                teams)
            (schedule-league-season db first-date new-competition-instance))
        5))

(define (next-date-with-given-weekday date weekday)
    (let loop ((date date))
        (if (= (date-week-day date) weekday)
            date
            (loop (add-day date)))))

(define (find-leagues-to-schedule db current-date)
    (for-each
        (lambda (country)
            (for-each
                (lambda (competition)
                    (define next-season-start
                        (next-date-with-given-weekday
                            (next-date-with-given-month-day
                                current-date
                                (slot-ref competition 'start-month)
                                (slot-ref competition 'start-day))
                            0))
                    (when
                        (<
                            (date-- next-season-start current-date)
                            (* 3 30 24 3600))
                        (create-comp-instance-and-schedule-league
                            db
                            next-season-start
                            competition country)))
                (query-results db '<competition>
                    (query-filter-proc obj
                        (equal? (slot-ref obj 'name) "League")))))
        (query-results db '<country>)))

(define (team-players db team datetime)
    (define contracts
        (query-results db '<player-contract>
            (query-filter-proc obj
                (equal? (slot-ref obj 'team) team)
                (equal? (slot-ref obj 'status) 'signed)
                (date>=? datetime (slot-ref obj 'date-start))
                (date<? datetime (slot-ref obj 'date-end)))))
    (map
        (lambda (contract)
            (slot-ref contract 'player))
        contracts))

(define (player-attr db player date)
    (query-first db '<player-attr>
        (query-filter-proc obj
            (equal? (slot-ref obj 'player) player)
            (date>=? date (slot-ref obj 'date-start))
            (date<? date (slot-ref obj 'date-end)))))

(define (get-starters db match team)
    (define match-datetime (slot-ref match 'datetime))
    (define all-players (team-players db team match-datetime))
    (define sorted
        (sort all-players
            (lambda (p1 p2)
                (let (
                        (p1-attr (player-attr db p1 match-datetime))
                        (p2-attr (player-attr db p2 match-datetime)))
                    (>
                        (+
                            (slot-ref p1-attr 'att)
                            (slot-ref p1-attr 'def))
                        (+
                            (slot-ref p2-attr 'att)
                            (slot-ref p2-attr 'def)))))))
    (take-n-or-fewer sorted conf-n-players-in-match))

(define (play-match db match)
    (d "Playing match:" match)
    (d "Home team:" (slot-ref match 'team-home))
    (d "Away team:" (slot-ref match 'team-away))
    (define match-datetime (slot-ref match 'datetime))
    (define team-home (slot-ref match 'team-home))
    (define team-away (slot-ref match 'team-away))
    (define team-home-starters (get-starters db match team-home))
    (define team-away-starters (get-starters db match team-away))
    5)

(define (find-matches-to-play db current-date)
    (define matches
        (query-results db
            '<match>
            (query-filter-proc obj
                (equal?
                    (slot-ref obj 'datetime)
                    current-date)
                (equal?
                    (slot-ref obj 'done)
                    0))))
    (for-each
        (lambda (match) (play-match db match))
        matches))

(define (do-day db current-date)
    (find-leagues-to-schedule db current-date)
    (find-matches-to-play db current-date))

(define (main)
    (define db (make-db))
    (init-sim db)
    (let loop ((current-date conf-sim-start-date))
        (unless (equal? current-date conf-sim-end-date)
            (d "Current date" (iso-8601-date current-date))
            (do-day db current-date)
            (loop (add-day current-date)))))

(main)
