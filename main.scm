;# vim: tabstop=2 shiftwidth=2
(use-modules (ice-9 format))
(use-modules (ice-9 textual-ports))
(use-modules (sqlite3))

(define d
  (lambda (s)
    (display s)
    (newline)))
; 
; 
; (define stmt (sqlite-prepare db "select * from match order by id limit 5"))
; 
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))
; (d (sqlite-step stmt))

(define n-seconds-per-hour 3600)
(define n-seconds-per-day (* n-seconds-per-hour 24))
(define n-seconds-per-week (* n-seconds-per-day 7))
(define n-seconds-per-month (* n-seconds-per-day 30))
(define n-seconds-per-year (* n-seconds-per-month 12))

(define conf-n-countries 1)
(define conf-n-divisions-per-country 2)
(define conf-n-teams-per-division 6)
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

(define (mean-adj age)
  (let ((age-years (/ age n-seconds-per-year)))
    (cond
      ((< age-years 16)  1.5)
      ((< age-years 25)  1.0)
      ((< age-years 30) -2.0)
      (else             -1.0))))

(define (log level msg)
  (if (>= level conf-logging-level)
    (d msg)))

(define (new-id)
  (1+ (random 99999999999)))

(define (range s n)
; List of integers >= s and < n.
  (let loop ((i n) (r '()))
    (if (<= i s)
      r
      (loop (1- i) (cons (1- i) r)))))

(define (range-cycle s n)
; Improper list of integers >= s and < n.
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

(define (gen-round-robin n)
; https://en.wikipedia.org/wiki/Round-robin_tournament#Circle_method
  (define cycle (range-cycle 1 n))
  (let loop ((cycle cycle) (i 0) (r '()))
    (if (< i (1- n))
      (let ((full (cons 0 cycle)))
        (d full)
        (let
          (
            (day-pairs
              (map
                (lambda (k)
                  (cons (list-ref full k) (list-ref full (- n 1 k))))
                (range 0 (/ n 2)))))
          (loop (cdr cycle) (1+ i) (cons day-pairs r))))
      r)))

(define (main)
  (define db (sqlite-open "sportsim2.db"))
  (call-with-input-file
    "main.sql"
    (lambda (port)
      (let ((schema-sql (get-string-all port)))
        (sqlite-exec db schema-sql))))
  (for-each d (gen-round-robin 4))
)

(main)
