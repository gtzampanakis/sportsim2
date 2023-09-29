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
(define conf-n-countries 2)
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

(define (sum ls)
  (fold + 0 ls))

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

(define (playerattr-adj attr age rs)
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

(define (playerattr attr player-id date)
  (define rs (get-rs 'playerattr player-id attr))
  (define date-of-birth (player-date-of-birth player-id))
  (define age (- date date-of-birth))
  (playerattr-adj attr age rs))

(define (player-team player-id)
  (truncate/ player-id conf-n-players-per-team))

(define (team-players team-id)
  (let ((s (* team-id conf-n-players-per-team)))
    (range s (+ s conf-n-players-per-team))))

(define (team-starters team-id date)
  (define players (team-players team-id))
  (define total-proc
    (lambda (player-id)
      (+
        (playerattr 'att player-id date)
        (playerattr 'def player-id date))))
  (define sorted (sort players (lambda (a b) (> (total-proc a) (total-proc b)))))
  (take sorted conf-n-players-in-match))

(define (team-attr attr team-id date)
  (define starters (team-starters team-id date))
  (sum
    (map
      (lambda (player-id)
        (playerattr attr player-id date))
      starters))) 

(define (match-result team-id-1 team-id-2 date)
  (define rs (get-rs 'match-result team-id-1 team-id-2 date))
  (define att1 (team-attr 'att team-id-1 date))
  (define att2 (team-attr 'att team-id-2 date))
  (define def1 (team-attr 'def team-id-1 date))
  (define def2 (team-attr 'def team-id-2 date))
  (define vel1 (team-attr 'vel team-id-1 date))
  (define vel2 (team-attr 'vel team-id-2 date))
  (define p1 (logistic-cdf 0 1 (* 0.0357 (- att1 def2))))
  (define p2 (logistic-cdf 0 1 (* 0.0263 (- att2 def1))))
  (define n (truncate (/ 11.5 (+ (/ 1. vel1) (/ 1. vel2)))))
  (define score1 (rand-binomial p1 n rs))
  (define score2 (rand-binomial p2 n rs))
  (cons score1 score2))

(define (main)
  (match-result 289 29999999 (y2s 26))
)

;(use-modules (statprof))
;(statprof main)
(main)
