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

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

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

(define (main)
  (d (player-date-of-birth 700990))
  (d (playerattr 'att 700990 (+ (player-date-of-birth 700990) (y2s 24))))
  (d (playerattr 'def 700990 (+ (player-date-of-birth 700990) (y2s 24))))
  (d (playerattr 'vel 700990 (+ (player-date-of-birth 700990) (y2s 24))))
)

(main)
