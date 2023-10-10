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
(define conf-n-teams-per-division 6)
(define conf-n-players-per-team 10)
(define conf-n-players-in-match 5)

(define mean-players-promoted-per-season 1.2)

(define player-attr-names (list 'team 'creation-season 'creation-index))
(define player-attr-names-n (length player-attr-names))

(define retirement-age (* 34 n-seconds-per-year))

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

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

(define (range s n)
; List of integers >= s and < n.
  (let loop ((i n) (r '()))
    (if (<= i s)
      r
      (loop (1- i) (cons (1- i) r)))))

(define (same n l)
  (if (= l 0)
    '()
    (cons n (same n (1- l)))))

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

(define-memoized (pairing-function . ls)
  ; Cantor pairing function. Maps a tuple of nonpositive integers to a unique
  ; nonpositive integer.
  (define (p2 y x)
    (let ((x+y (+ x y)))
      (+ (* (/ 1 2) x+y (+ x+y 1)) y)))
  (reduce p2 -1 ls))

(define-memoized (inverse-pairing-function z n)
  (define (ip2 z)
    (let* (
        (w (floor (* (/ 1 2) (- (sqrt (+ (* 8 z) 1)) 1))))
        (t (* (/ 1 2) (+ (* w w) w)))
        (y (- z t))
        (x (- w y))
        (r (list x y)))
      r))
  (let loop ((z z) (n n) (r '()))
    (if (= n 2) (append (ip2 z) r)
      (let ((h (ip2 z)))
        (loop (car h) (1- n) (cons (cadr h) r))))))

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

(define-memoized (gen-round-robin n)
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

(define-memoized (get-random-seed . args)
  ; Don't memoize this procedure because the object it returns is stateful and
  ; thus we don't want to cache it.
  ; TODO: add a separator between the args otherwise different args lists can
  ; result in the same result
  (apply string-append
    (map
      (lambda (arg)
        (string-append
          (cond
            ((number? arg) (number->string arg))
            ((symbol? arg) (symbol->string arg))
            (else arg))
          "~~~"))
      (cons 2749828749824 args))))

(define (get-random-state . args)
  (seed->random-state (apply get-random-seed args)))

(define (secs-to-years s)
  (truncate/ s n-seconds-per-year))

(define (years-to-secs y)
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

(define (rand-poisson l rs)
; Algorithm by Knuth
  (define L (exp (- l)))
  (let loop ((k 1) (p (random:uniform rs)))
    (if (< p L)
      (1- k)
      (loop (1+ k) (* p (random:uniform rs))))))

(define (player-name player-id) player-id)

(define (player-id team creation-date creation-index)
  (pairing-function team creation-date creation-index))

(define (player-attrs player)
  (inverse-pairing-function player player-attr-names-n))

(define (player-attr player attr)
  (list-ref (player-attrs player) (index player-attr-names attr)))  

(define-memoized (player-date-of-birth player-id)
  (define rs (get-random-state 'player-date-of-birth player-id))
  (define creation-season (player-attr player-id 'creation-season))
  (define age-in-years-at-creation-season (+ 16 (* 17 (random:uniform rs))))
  (define date-of-birth-in-years (- creation-season age-in-years-at-creation-season))
  (define date-of-birth-in-seconds (years-to-secs date-of-birth-in-years))
  date-of-birth-in-seconds)

(define-memoized (player-age player-id date)
  (- date (player-date-of-birth player-id)))

(define (adj-per-week-mplier age)
  (define age-years (secs-to-years age))
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
  (define rs (get-random-state 'playerattr player-id attr))
  (define date-of-birth (player-date-of-birth player-id))
  (define age (- date date-of-birth))
  (playerattr-adj attr age rs))

(define-memoized (player-retired? player-id date)
  (define date-start-of-year (* (truncate/ date n-seconds-per-year) n-seconds-per-year))
  (define age-at-start-of-year (player-age player-id date-start-of-year))
  (>= age-at-start-of-year retirement-age))

(define-memoized (team-initial-players team-id)
  (map player-id
    (same team-id conf-n-players-per-team)
    (same 0 conf-n-players-per-team)
    (range 0 conf-n-players-per-team)))

(define-memoized (player-retirement-season player)
  (define dob (player-date-of-birth player))
  (define date-on-which-reaches-retirement (+ dob retirement-age))
  (truncate/ date-on-which-reaches-retirement n-seconds-per-year))

(define-memoized (team-players team-id date)
  (define season (secs-to-years date))
  (let loop ((season season))
    (if (= season 0)
      (team-initial-players team-id)
      (let (
          (players-not-retired
            (filter
              (lambda (player)
                (> (player-retirement-season player) (1- season)))
              (loop (1- season)))))
        (append players-not-retired
          (map (lambda (i) (player-id team-id season i))
            (range 0 (- conf-n-players-per-team (length players-not-retired)))))))))

(for-each
  (lambda (season)
    (d (team-players 0 (years-to-secs season)))
    (d (map (lambda (player) (secs-to-years (player-age player (years-to-secs season))))
      (team-players 0 (years-to-secs season))))
    (d))
  (range 0 2500))
(exit)

(define-memoized (team-starters team-id date)
  (define players (team-players team-id date))
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
            (drop (drop-right div-rankings 3) 3)))))))

(define-memoized (match-result teams date)
  (define team1 (car teams))
  (define team2 (cadr teams))
  (define rs (get-random-state 'match-result team1 team2 date))
  (define att1 (team-attr 'att team1 date))
  (define att2 (team-attr 'att team2 date))
  (define def1 (team-attr 'def team1 date))
  (define def2 (team-attr 'def team2 date))
  (define vel1 (exp (team-attr 'vel team1 date)))
  (define vel2 (exp (team-attr 'vel team2 date)))
  (define p1 (* 0.52 (logistic-cdf 0 1 (- att1 def2))))
  (define p2 (* 0.42 (logistic-cdf 0 1 (- att2 def1))))
  (define n (truncate (/ 11.5 (+ (/ 1. vel1) (/ 1. vel2)))))
  (define score1 (rand-binomial p1 n rs))
  (define score2 (rand-binomial p2 n rs))
  ;(d vel1 vel2 p1 p2 n score1 score2)
  ;(d "")
  ;(d
  ;  (apply -
  ;    (map
  ;      (lambda (team)
  ;        (+ (team-attr 'att team date) (team-attr 'def team date)))
  ;      teams))
  ;  (list p1 p2)
  ;  (list score1 score2)
  ;)
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
          (match-result match
            (+ (* season n-seconds-per-year) (* day-index n-seconds-per-week))))
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

(define-memoized (division-attr attr division season)
  (sum
    (map
      (lambda (team)
        (team-attr attr team (* season n-seconds-per-year)))
      (division-teams division season))))

(define (main)
  (for-each
    (lambda (season)
      (d season)
      (d (division-rankings 0 season)
        (+ (division-attr 'att 0 season) (division-attr 'def 0 season)))
      (d (division-rankings 1 season)
        (+ (division-attr 'att 1 season) (division-attr 'def 1 season)))
      (d (division-rankings 2 season)
        (+ (division-attr 'att 2 season) (division-attr 'def 2 season)))
      (d (division-rankings 3 season)
        (+ (division-attr 'att 3 season) (division-attr 'def 3 season)))
      (d (division-rankings 4 season)
        (+ (division-attr 'att 4 season) (division-attr 'def 4 season))))
    (range 0 3))
)

;(use-modules (statprof))
;(statprof main)
(main)
