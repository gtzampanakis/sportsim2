;# vim: tabstop=2 shiftwidth=2
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))

(define n-seconds-per-hour 3600)
(define n-seconds-per-day (* n-seconds-per-hour 24))
(define n-seconds-per-week (* n-seconds-per-day 7))
(define n-seconds-per-month (* n-seconds-per-day 30))
(define n-seconds-per-year (* n-seconds-per-month 12))

(define adj-per-week-base 0.001)
(define sd-per-week-base 0.001)

(define conf-date-start 0)
(define conf-n-divisions-per-country 5)
(define conf-n-teams-per-division 14)
(define conf-n-players-per-team 18)
(define conf-n-players-in-match 11)

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

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

(define (enumerate ls)
  (map
    cons
    ls
    (range 0 (length ls))))

(define (flatten ls)
  (fold append '() ls))

(define (map-twice proc ls)
  (map
    (lambda (ls2)
      (map proc ls2))
    ls))

(define (memoized-proc proc)
  (define cache (make-hash-table))
  (lambda args
    (define cached-result (hash-ref cache args))
    (when (eq? cached-result #f)
      (let ((result (apply proc args)))
        (hash-set! cache args result)
        (set! cached-result result)))
    cached-result))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (name . args) exp exp* ...)
      (define name
        (memoized-proc
          (lambda args exp exp* ...))))))

(define (sum ls)
  (fold + 0 ls))

(define (prod ls)
  (fold * 1 ls))

(define (index ls val)
  (let loop ((ls ls) (r 0))
    (if (null? ls)
      #f
      (if (equal? (car ls) val)
        r
        (loop (cdr ls) (1+ r))))))

(define (is2i as ns)
  ; (=
  ;     (is21 '(1 2 3) '(10 20))
  ;     621)
  ; (=
  ;   (is2i '(444 333 222) '(1000 1000))
  ;   222333444)
  (when (not (= (length as) (1+ (length ns))))
    (error "is2i: wrong input lengths"))
  (let loop ((as as) (ns ns) (s 0) (p 1))
    (when (not (null? ns))
      (when (>= (car as) (car ns))
        (error "is2i: value exceeds dimension")))
    (if (null? ns)
      (+ s (* p (car as)))
      (loop
        (cdr as)
        (cdr ns)
        (+ s (* p (car as)))
        (* p (car ns))))))

(define (i2is i ns)
  (let loop ((ns ns) (i i) (as '()))
    (if (null? ns)
      (reverse (cons i as))
      (loop
        (cdr ns)
        (quotient i (car ns))
        (cons (remainder i (car ns)) as)))))

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

(define get-rs
  (lambda args
    (seed->random-state
      (apply string-append
        (map
          (lambda (arg)
            (cond
              ((number? arg) (number->string arg))
              ((symbol? arg) (symbol->string arg))
              (else arg)))
          (cons 2749828749824 args))))))

(define (s2y s)
  (truncate/ s n-seconds-per-year))

(define (y2s y)
  (* y n-seconds-per-year))

(define (rand-logistic loc sc rs)
  (let ((r (random:uniform rs)))
    (let ((z (log10 (/ r (- 1 r)))))
      (+ loc (* sc z)))))

(define (logistic-cdf loc sc x)
  (let ((z (/ (- x loc) sc)))
    (/ 1 (+ 1 (exp (- z))))))

(define (rand-binomial p n rs)
  (let loop ((k 0) (i 0))
    (if (= i n)
      k
      (loop (if (< (random:uniform rs) p) (1+ k) k) (1+ i)))))

(define (player-name player-id)
  player-id)

(define (player-date-of-birth player-id)
  (define rs (get-rs 'player-date-of-birth player-id))
  (define age-years (+ 16 (* (random:uniform rs) 19)))
  (truncate (- conf-date-start (* age-years n-seconds-per-year))))

(define (adj-per-week-mplier age)
  (define age-years (s2y age))
  (cond
    ((< age-years 16)  1.5)
    ((< age-years 25)  1.0)
    ((< age-years 30)  0.0)
    ((< age-years 38) -1.0)
    (else             -2.0)))

(define-memoized (playerattr-adj attr age rs)
  (let loop ((current-age 0) (r 0))
    (define mplier
      (cond
        ((eq? attr 'att) (adj-per-week-mplier current-age))
        ((eq? attr 'def) (adj-per-week-mplier current-age))
        ((eq? attr 'vel) 0.0)
        (else (error attr))))
    (if (> current-age age)
      r
      (loop
        (+ current-age n-seconds-per-week)
        (+
          r
          (rand-logistic
            (* mplier adj-per-week-base)
            sd-per-week-base
            rs))))))

(define-memoized (playerattr attr player-id date)
  (define rs (get-rs 'playerattr player-id attr))
  (define date-of-birth (player-date-of-birth player-id))
  (define age (- date date-of-birth))
  (playerattr-adj attr age rs))

(define (player-team player-id)
  (truncate/ player-id conf-n-players-per-team))

(define (team-players team-id)
  (let ((s (* team-id conf-n-players-per-team)))
    (range s (+ s conf-n-players-per-team))))

(define-memoized (team-starters team-id date)
  (define players (team-players team-id))
  (define total-proc
    (lambda (player-id)
      (+
        (playerattr 'att player-id date)
        (playerattr 'def player-id date))))
  (define sorted (sort players (lambda (a b) (> (total-proc a) (total-proc b)))))
  (take sorted conf-n-players-in-match))

(define-memoized (team-attr attr team-id date)
  (define starters (team-starters team-id date))
  (sum
    (map
      (lambda (player-id)
        (playerattr attr player-id date))
      starters))) 

(define (division-id division-rank country-id)
  (is2i (list division-rank country-id) (list conf-n-divisions-per-country)))

(define (division-rank-country division)
  (i2is division (list conf-n-divisions-per-country)))

(define (division-rank division)
  (list-ref (division-rank-country division) 0))

(define (division-country division)
  (list-ref (division-rank-country division) 1))

(define (higher-division division)
  (define rank-country (division-rank-country division))
  (define rank (list-ref rank-country 0))
  (define country (list-ref rank-country 1))
  (if (= rank 0)
    '()
    (division-id (1- rank) country)))

(define (lower-division division)
  (define rank-country (division-rank-country division))
  (define rank (list-ref rank-country 0))
  (define country (list-ref rank-country 1))
  (if (= rank (1- conf-n-divisions-per-country))
    '()
    (division-id (1+ rank) country)))

(define-memoized (team-division team season)
  (let loop ((current-season 0) (division (quotient team conf-n-teams-per-division)))
    (if (= season current-season)
      division
      (let* (
          (div-rank (division-rank division))
          (rankings (division-rankings division current-season))
          (team-ranking (index rankings team)))
        (loop
          (1+ current-season)
          (cond
            ((and
                (> div-rank 0)
                (< team-ranking 3))
              (higher-division division))
            ((and
                (< div-rank (1- conf-n-divisions-per-country))
                (>= team-ranking (- conf-n-teams-per-division))
              (lower-division division)))
            (else division)))))))

(define-memoized (division-teams division season)
  (sort
    (let loop (
        (current-season 0)
        (teams
          (let ((s (* division conf-n-teams-per-division)))
            (range s (+ s conf-n-teams-per-division)))))
      (if (= season current-season)
        teams
        (loop
          (1+ current-season)
          (let* (
              (div-rank (division-rank division))
              (higher-div (higher-division division))
              (lower-div (lower-division division))
              (div-rankings
                (division-rankings division current-season))
              (higher-rankings
                (if (null? higher-div) '() (division-rankings higher-div current-season)))
              (lower-rankings
                (if (null? lower-div) '() (division-rankings lower-div current-season))))
            (append
              (if (null? higher-rankings)
                (take div-rankings 3)
                (take-right higher-rankings 3))
              (if (null? lower-rankings)
                (take-right div-rankings 3)
                (take lower-rankings 3))
              (drop (drop-right div-rankings 3) 3))))))
    <))

(define-memoized (match-result teams date)
  (define team-id-1 (car teams))
  (define team-id-2 (cadr teams))
  (define rs (get-rs 'match-result team-id-1 team-id-2 date))
  (define att1 (team-attr 'att team-id-1 date))
  (define att2 (team-attr 'att team-id-2 date))
  (define def1 (team-attr 'def team-id-1 date))
  (define def2 (team-attr 'def team-id-2 date))
  (define vel1 (exp (team-attr 'vel team-id-1 date)))
  (define vel2 (exp (team-attr 'vel team-id-2 date)))
  (define p1 (* 0.52 (logistic-cdf 0 1 (- att1 def2))))
  (define p2 (* 0.42 (logistic-cdf 0 1 (- att2 def1))))
  (define n (truncate (/ 11.5 (+ (/ 1. vel1) (/ 1. vel2)))))
  (define score1 (rand-binomial p1 n rs))
  (define score2 (rand-binomial p2 n rs))
  ;(d vel1 vel2 p1 p2 n score1 score2)
  ;(d "")
  (list score1 score2))

(define-memoized (division-schedule division season)
  (define teams (division-teams division season))
  (define schedule-ords (gen-round-robin conf-n-teams-per-division))
  (define ord-to-team (lambda (ord) (list-ref teams ord)))
  (map
    (lambda (day)
      (map
        (lambda (match)
          (map ord-to-team match))
        day))
    schedule-ords))

(define-memoized (division-results division season)
  (define schedule (division-schedule division season))
  (map
    (lambda (day day-index)
      (map
        (lambda (match)
          (match-result match (* day-index n-seconds-per-week)))
        day))
    schedule
    (range 0 (length schedule))))

(define-memoized (division-points division season)
  (define teams (division-teams division season))
  (define schedule (flatten (division-schedule division season)))
  (define results (flatten (division-results division season)))
  (define points-granular
    (fold
      (lambda (match result acc)
        (define team1 (car match))
        (define team2 (cadr match))
        (define score1 (car result))
        (define score2 (cadr result))
        (append
          (cond
            ((> score1 score2)
              (list (cons team1 3)))
            ((< score1 score2)
              (list (cons team2 3)))
            ((= score1 score2)
              (list (cons team1 1) (cons team2 1))))
          acc))
      '()
      schedule
      results))
  (map
    (lambda (team)
      (fold + 0
        (map cdr
          (filter
            (lambda (pair) (= (car pair) team))
            points-granular))))
    teams))

(define-memoized (division-rankings division season)
  (define teams (division-teams division season))
  (define points (division-points division season))
  (define team-points-pairs (map cons teams points))
  (map car
    (sort
      team-points-pairs
      (lambda (team-points-pair1 team-points-pair2)
        (> (cdr team-points-pair1) (cdr team-points-pair2))))))

(define (main)
  (d (division-teams 0 0))
  (d (division-teams 1 0))
  (d (division-teams 2 0))
  (d (division-teams 3 0))
  (d (division-teams 4 0))
  (d)
  (d (division-teams 0 1))
  (d (division-teams 1 1))
  (d (division-teams 2 1))
  (d (division-teams 3 1))
  (d (division-teams 4 1))
)

;(use-modules (statprof))
;(statprof main)
(main)
