;# vim: tabstop=2 shiftwidth=2
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 textual-ports))

(use-modules (sqlite3))

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

(define n-seconds-per-hour 3600)
(define n-seconds-per-day (* n-seconds-per-hour 24))
(define n-seconds-per-week (* n-seconds-per-day 7))
(define n-seconds-per-month (* n-seconds-per-day 30))
(define n-seconds-per-year (* n-seconds-per-month 12))

(define conf-n-countries 2)
(define conf-n-divisions-per-country 5)
(define conf-n-teams-per-division 14)
(define conf-n-players-per-team 18)
(define conf-n-players-in-match 11)
(define conf-date-start 0)
(define conf-season-start-year-offset (* n-seconds-per-month 7))
(define conf-n-seasons-to-simulate 6)
(define conf-date-end
  (+
    conf-date-start
    conf-season-start-year-offset
    (* n-seconds-per-year conf-n-seasons-to-simulate)))
(define conf-promotion-relegation-enabled #t)
(define conf-logging-level 25)

(define adj-per-week 0.001)
(define sd-per-week 0.002)

(define (logistic-cdf loc sc x)
  (let ((z (/ (- x loc) sc)))
    (/ 1 (+ 1 (exp (- z))))))

(define (rand-binomial p n)
  (let loop ((k 0) (i 0))
    (if (= i n)
      k
      (loop (if (< (random:uniform) p) (1+ k) k) (1+ i)))))

(define (rand-logistic loc sc)
  (let ((r (random:uniform)))
    (let ((z (log10 (/ r (- 1 r)))))
      (+ loc (* sc z)))))

(define (mean-adj age-years)
  (cond
    ((< age-years 16)  1.5)
    ((< age-years 25)  1.0)
    ((< age-years 30)  0.0)
    ((< age-years 38) -1.0)
    (else             -2.0)))

(define (log level msg)
  (if (>= level conf-logging-level)
    (d msg)))

(define (new-id)
  (1+ (random 99999999999)))

(define (range s n)
; List of integers >= s and < n.
  (unless (integer? s)
    (error 'non-integer-s))
  (unless (integer? n)
    (error 'non-integer-n))
  (let loop ((i n) (r '()))
    (if (<= i s)
      r
      (loop (1- i) (cons (1- i) r)))))

(define (range-cycle s n)
; Improper list of integers >= s and < n.
  (unless (integer? s)
    (error 'non-integer-s))
  (unless (integer? n)
    (error 'non-integer-n))
  (let ((ls (range s n)))
    (if (null? ls)
      ls
      (let loop ((pair ls))
        (if (null? (cdr pair))
          (begin
            (set-cdr! pair ls)
            ls)
          (loop (cdr pair)))))))

(define (apply-n-times n proc arg)
  (let loop ((n n) (r arg))
    (if (= n 0)
      r
      (loop (1- n) (proc r)))))

(define sqlite-qmap
  (case-lambda
    ((db sql)
      (sqlite-qmap (lambda (x) x) sql '()))
    ((db sql args)
      (sqlite-qmap db (lambda (x) x) sql args))
    ((db proc sql args)
      (let ((stmt (sqlite-prepare db sql)))
        (apply sqlite-bind-arguments (cons stmt args))
        (sqlite-map proc stmt)))))

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
                  (let
                    (
                      (team0 (list-ref full k))
                      (team1 (list-ref full (- n 1 k))))
                    (if
                      (and
                        (memq team0 played-home-last-day)
                        (not (memq team1 played-home-last-day)))
                      (cons team1 team0)
                      (cons team0 team1))))
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
        (map (lambda (pair) (cons (cdr pair) (car pair))) day-pairs))
      first-round-days)))

(define (get-att-adj age-years mplier)
  (rand-logistic
    (* mplier (mean-adj age-years) adj-per-week)
    (* mplier sd-per-week)))

(define (get-def-adj age-years mplier)
  (rand-logistic
    (* mplier (mean-adj age-years) adj-per-week)
    (* mplier sd-per-week)))

(define (get-vel-adj age-years mplier)
  (rand-logistic 0 (* mplier sd-per-week)))

(define (generate-1 db)
  (for-each
    (lambda (country-i)
      (define country-id (new-id))
      (define country-name (format #f "country-~a" country-i))
      (sqlite-qmap db
        "insert into country values (?, ?)"
        (list country-id country-name))
      (generate-2 db country-id))
    (range 0 conf-n-countries)))

(define (generate-2 db country-id)
  (for-each
    (lambda (division-i)
      (define division-id (new-id))
      (define division-rank (1+ division-i))
      (sqlite-qmap db
        "
        insert into division
        (id, rank, country_id)
        values
        (?, ?, ?)
        "
        (list division-id division-rank country-id))
      (generate-3 db division-id))
    (range 0 conf-n-divisions-per-country)))

(define (generate-3 db division-id)
  (for-each
    (lambda (team-i)
      (define team-id (new-id))
      (define team-name team-id)
      (sqlite-qmap db
        "
        insert into team
        (id, name)
        values
        (?, ?)
        "
        (list team-id team-name))
      (sqlite-qmap db
        "
        insert into teamdivision
        (team_id, division_id, season_id)
        values
        (?, ?, NULL)
        "
        (list team-id division-id))
      (generate-4 db team-id))
    (range 0 conf-n-teams-per-division)))

(define (update-attrs db current-date)
  (log 25 (format #f "Updating attrs on ~a" current-date))
  (define player-id -1)
  (let loop ()
    (define rows
      (sqlite-qmap db
        identity
        "
        select
        pa.player_id, pa.att, pa.def, pa.vel, p.date_of_birth
        from playerattr pa
        join player p on p.id = pa.player_id
        where date = ? - ?
        and pa.player_id > ?
        order by p.id
        limit 1000
        "
        (list current-date n-seconds-per-week player-id)))
    (for-each
      (lambda (row)
        (set! player-id (vector-ref row 0))
        (define age (- current-date (vector-ref row 4)))
        (define age-years (/ age n-seconds-per-year))
        (define att-adj (get-att-adj age-years 1))
        (define def-adj (get-def-adj age-years 1))
        (define vel-adj (get-vel-adj age-years 1))
        (define att (+ (vector-ref row 1) att-adj))
        (define def (+ (vector-ref row 2) def-adj))
        (define vel (+ (vector-ref row 3) vel-adj))
        (sqlite-qmap db
          "insert into
          playerattr
          (player_id, date, att, def, vel)
          values
          (?, ?, ?, ?, ?)"
          (list player-id current-date att def vel)))
      rows)
    (when (not (null? rows))
      (loop))))

(define (generate-4 db team-id)
  (for-each
    (lambda (player-i)
      (define player-id (new-id))
      (define player-name player-id)
      (define player-ability (rand-logistic 0 1))
      (define age-years (+ 16 (* (random:uniform) 19)))
      (define age (* age-years n-seconds-per-year))
      (define date-of-birth (- (truncate age)))
      (define att0 (rand-logistic player-ability 0.1))
      (define def0 (rand-logistic player-ability 0.1))
      (define vel0 (exp (rand-logistic 0 1)))
      (define get-adj
        (lambda (proc)
          (fold
            +
            0.0
            (map
              (lambda (ys) (proc ys 52))
              (range 0 (truncate age-years))))))
      (define att-adj (get-adj get-att-adj))
      (define def-adj (get-adj get-def-adj))
      (define vel-adj (get-adj get-vel-adj))
      (define att (+ att0 att-adj))
      (define def (+ def0 def-adj))
      (define vel (+ vel0 vel-adj))
      (sqlite-qmap db
        "
        insert into player
        (id, name, date_of_birth)
        values
        (?, ?, ?)
        "
        (list player-id player-name date-of-birth))
      (sqlite-qmap db
        "
        insert into playerteam
        (player_id, team_id, date_from, date_to)
        values
        (?, ?, 0, NULL)
        "
        (list player-id team-id))
      (sqlite-qmap db
        "
        insert into playerattr
        (player_id, date, att, def, vel)
        values
        (?, 0, ?, ?, ?)
        "
        (list player-id att def vel)))
    (range 0 conf-n-players-per-team)))

(define (main)
  (define db (sqlite-open "sportsim2.db"))
  (sqlite-exec db "begin")
  (call-with-input-file
    "main.sql"
    (lambda (port)
      (let ((schema-sql (get-string-all port)))
        (sqlite-exec db schema-sql))))

  (generate-1 db)

  (define initialized-teamdivision-records #f)
  (define current-date conf-date-start)
  (define year 0)

  (let loop-days ((current-date conf-date-start))
    (when (= (remainder current-date n-seconds-per-week) 0)
      (update-attrs db current-date))
    (loop-days (+ current-date n-seconds-per-day)))

  (sqlite-exec db "commit"))

(main)
