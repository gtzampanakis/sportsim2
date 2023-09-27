;# vim: tabstop=2 shiftwidth=2
(use-modules (ice-9 format))
(use-modules (sqlite3))

(define d
  (lambda (s)
    (display s)
    (newline)))
; 
; (define db (sqlite-open "sportsim2.db"))
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

(define (main)
  (let ((schema-file-port (open-file "main.sql" "r")))
    

(main)
