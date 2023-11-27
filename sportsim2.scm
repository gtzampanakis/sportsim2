(define-module (sportsim2))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-43))
(use-modules (ice-9 format))

(use-modules (util))
(use-modules (math))

(define-public n-seconds-per-hour 3600)
(define-public n-seconds-per-day (* n-seconds-per-hour 24))
(define-public n-seconds-per-week (* n-seconds-per-day 7))
(define-public n-seconds-per-month (* n-seconds-per-day 30))
(define-public n-seconds-per-year (* n-seconds-per-month 12))

(define-public adj-period n-seconds-per-year)
(define-public adj-base 0.010)
(define-public sd-base 0.010)

(define-public conf-date-start 0)
(define-public conf-n-divisions-per-country 5)
(define-public conf-n-teams-per-division 8)
(define-public conf-n-players-per-team 4)
(define-public conf-n-players-in-match 2)

(define-public home-advantage-factor 1.1)

(define-public player-prop-names (list 'team 'creation-season 'creation-index))
(define-public player-prop-names-n (length player-prop-names))

(define-public retirement-age (* 34 n-seconds-per-year))

(define-public attr-names (list
  'att 'def 'vel
))

; vel is the preference of a player to play fast or slow, not his
; capability to play fast. So it is uncorrelated to attributes that stem
; from his ability.
(define-public attr-correlations (list
  (cons (list 'att 'def) 0.9)
  (cons (list 'att 'vel) 0.0)
  (cons (list 'def 'vel) 0.0)
))

; TODO:
; * cache strategies to keep one cached entry per season so that we can more
; easily go back as far as we want
; * stadiums
; * player positions to affect their performance
; * salaries
; * finances
; * introduce countries
; * attendances so teams have some income
; * player nationalities to affect their initial and further attributes
; * dependence on country

(define-public (memoized-proc proc)
  (define max-cache-size 50000)
  (define key-to-cache-sublist (make-hash-table))
  (define cache '())
  (define cache-size 0)
  (define cache-last-pair '())
  (lambda args
    (define cache-sublist (hash-ref key-to-cache-sublist args))
    (if (eq? cache-sublist #f)
      (let* (
          (proc-result (apply proc args))
          (cache-entry (cons args proc-result)))
        ; cache grows from the end so we can easily drop the oldest records
        ; from the front
        (if (null? cache)
          (begin
            (set! cache (list cache-entry))
            (set! cache-last-pair cache))
          (begin
            (set-cdr! cache-last-pair (list cache-entry))
            (set! cache-last-pair (cdr cache-last-pair))))
        (set! cache-size (1+ cache-size))
        (when (> cache-size max-cache-size)
          (let ((args-to-forget (caar cache)))
            (set! cache (cdr cache))
            (hash-remove! key-to-cache-sublist args-to-forget)
            (set! cache-size (1- cache-size))))
        (hash-set! key-to-cache-sublist args cache-last-pair)
        (cdr cache-entry))
      (let ((cache-entry (car cache-sublist))) (cdr cache-entry)))))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (name . args) expr expr* ...)
      (define name
        (memoized-proc
          (lambda args expr expr* ...))))))

(define-syntax define-public-memoized
  (syntax-rules ()
    ((_ (name . args) expr expr* ...)
     (begin
       (define-memoized (name . args) expr expr* ...)
       (export name)))))

(define-public attrs-covariance
  (let* (
      (la (length attr-names))
      (sigma (matrix-identity la)))
    (for-each
      (lambda (i)
        (define attr-i (list-ref attr-names i))
        (for-each
          (lambda (j)
            (define attr-j (list-ref attr-names j))
            (define corr (assoc-ref attr-correlations (list attr-i attr-j)))
            (matrix-set! sigma i j corr)
            (matrix-set! sigma j i corr))
          (range (1+ i) la)))
      (range 0 la))
    sigma))


(define-public-memoized (gen-round-robin n)
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

(define-public-memoized (get-random-seed . args)
  ; Don't memoize this procedure because the object it returns is stateful and
  ; thus we don't want to cache it.
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

(define-public (get-random-state . args)
  (seed->random-state (apply get-random-seed args)))

(define-public (secs-to-years s)
  (truncate/ s n-seconds-per-year))

(define-public (years-to-secs y)
  (* y n-seconds-per-year))

(define-public (player-name player-id) player-id)

(define-public (player-id team creation-date creation-index)
  (pairing-function team creation-date creation-index))

(define-public (player-props player)
  (inverse-pairing-function player player-prop-names-n))

(define-public (player-prop player prop)
  (list-ref (player-props player) (index player-prop-names prop)))  

(define-public (player-date-of-birth player-id)
  (define rs (get-random-state 'player-date-of-birth player-id))
  (define creation-season (player-prop player-id 'creation-season))
  (define age-in-years-at-creation-season (+ 16 (* 17 (random:uniform rs))))
  (define date-of-birth-in-years
    (- creation-season age-in-years-at-creation-season))
  (define date-of-birth-in-seconds
    (truncate (years-to-secs date-of-birth-in-years)))
  date-of-birth-in-seconds)

(define-public (player-age player-id date)
  (- date (player-date-of-birth player-id)))

(define-public (adj-mplier age)
  (define age-years (secs-to-years age))
  (cond
    ((< age-years 16)  1.5)
    ((< age-years 25)  1.0)
    ((< age-years 30)  0.0)
    ((< age-years 38) -1.0)
    (else             -2.0)))

(define-public-memoized (player-initial-attrs player-id)
  (define rs (get-random-state 'player-initial-attrs player-id))
  (define vals (rand-mult-normal 0 attrs-covariance rs))
  vals)

; The age is in adj-periods to aid memoization.
(define-public-memoized (player-attrs-by-age-in-adj-periods
                   player-id age-in-adj-periods)
  (if (<= age-in-adj-periods 0)
    (player-initial-attrs player-id)
    (vektor+
      (player-attrs-by-age-in-adj-periods player-id (- age-in-adj-periods 1))
      (vektor+
        0.05
        (rand-mult-normal
          0
          attrs-covariance
          (get-random-state 'player-attrs player-id age-in-adj-periods))))))

(define-public (player-attrs player-id date)
  (player-attrs-by-age-in-adj-periods
    player-id
    (truncate/ (player-age player-id date) adj-period)))

(define-public-memoized (player-attr attr player-id date)
  (define attrs (player-attrs player-id date))
  (list-ref attrs (index attr-names attr)))

(define-public (player-retired? player-id date)
  (define date-start-of-year
    (* (truncate/ date n-seconds-per-year) n-seconds-per-year))
  (define age-at-start-of-year (player-age player-id date-start-of-year))
  (>= age-at-start-of-year retirement-age))

(define-public (team-initial-players team-id)
  (map player-id
    (same team-id conf-n-players-per-team)
    (same 0 conf-n-players-per-team)
    (range 0 conf-n-players-per-team)))

(define-public-memoized (player-retirement-season player)
  (define dob (player-date-of-birth player))
  (define date-on-which-reaches-retirement (+ dob retirement-age))
  (truncate/ date-on-which-reaches-retirement n-seconds-per-year))

(define-public-memoized (team-players team-id date)
  (define season (secs-to-years date))
  (if (= season 0)
    (team-initial-players team-id)
    (let (
        (players-not-retired
          (filter
            (lambda (player)
              (> (player-retirement-season player) (1- season)))
            (team-players team-id (- date n-seconds-per-year)))))
      (append players-not-retired
        (map (lambda (i) (player-id team-id season i))
          (range
            0 (- conf-n-players-per-team (length players-not-retired))))))))

(define-public-memoized (team-starters team-id date)
  (define players (team-players team-id date))
  (define total-proc
    (lambda (player-id)
      (+
        (player-attr 'att player-id date)
        (player-attr 'def player-id date))))
  (define sorted
    (sort players (lambda (a b) (> (total-proc a) (total-proc b)))))
  (take sorted conf-n-players-in-match))

(define-public-memoized (team-starters-attr attr team-id date)
  (define starters (team-starters team-id date))
  (sum
    (map
      (lambda (player-id)
        (player-attr attr player-id date))
      starters))) 

(define-public (division-id division-rank country-id)
  (is2i (list division-rank country-id) (list conf-n-divisions-per-country)))

(define-public (division-rank-country division)
  (i2is division (list conf-n-divisions-per-country)))

(define-public (division-rank division)
  (list-ref (division-rank-country division) 0))

(define-public (division-country division)
  (list-ref (division-rank-country division) 1))

(define-public (higher-division division)
  (define rank-country (division-rank-country division))
  (define rank (list-ref rank-country 0))
  (define country (list-ref rank-country 1))
  (if (= rank 0)
    '()
    (division-id (1- rank) country)))

(define-public (lower-division division)
  (define rank-country (division-rank-country division))
  (define rank (list-ref rank-country 0))
  (define country (list-ref rank-country 1))
  (if (= rank (1- conf-n-divisions-per-country))
    '()
    (division-id (1+ rank) country)))

(define-public-memoized (division-teams division season)
  (let loop (
      (current-season 0)
      (teams
        (let ((s (* division conf-n-teams-per-division)))
          (range s (+ s conf-n-teams-per-division)))))
    (if (= season current-season)
      (sort teams <)
      (loop
        (1+ current-season)
        (let* (
            (div-rank (division-rank division))
            (higher-div (higher-division division))
            (lower-div (lower-division division))
            (div-rankings
              (division-rankings division current-season))
            (higher-rankings
              (if (null? higher-div)
                '() (division-rankings higher-div current-season)))
            (lower-rankings
              (if (null? lower-div)
                '() (division-rankings lower-div current-season))))
          (append
            (if (null? higher-rankings)
              (take div-rankings 3)
              (take-right higher-rankings 3))
            (if (null? lower-rankings)
              (take-right div-rankings 3)
              (take lower-rankings 3))
            (drop (drop-right div-rankings 3) 3)))))))

(define-public-memoized (player-perf-on-date attr player date)
  (define loc (player-attr attr player date))
  (define rs (get-random-state 'player-perf-on-date attr player date))
  (rand-logistic loc 0.05 rs))

(define-public-memoized (team-starters-perf-on-date team attr date)
  (sum
    (map
      (lambda (player) (player-perf-on-date attr player date))
    (team-starters team date))))

(define-public-memoized (match-result teams date)
  (define team1 (car teams))
  (define team2 (cadr teams))
  (define rs (get-random-state 'match-result team1 team2 date))
  (define att1 (team-starters-perf-on-date team1 'att date))
  (define att2 (team-starters-perf-on-date team2 'att date))
  (define def1 (team-starters-perf-on-date team1 'def date))
  (define def2 (team-starters-perf-on-date team2 'def date))
  (define vel1 (team-starters-perf-on-date team1 'vel date))
  (define vel2 (team-starters-perf-on-date team2 'vel date))
  (define p1 (* 0.42 (logistic-cdf 0 1 (- att1 def2)) home-advantage-factor))
  (define p2 (* 0.42 (logistic-cdf 0 1 (- att2 def1))))
  (define n (truncate (/ 11.5 (+ (/ 1. (exp vel1)) (/ 1. (exp vel2))))))
  (define score1 (rand-binomial p1 n rs))
  (define score2 (rand-binomial p2 n rs))
  (list score1 score2))

(define-public-memoized (division-schedule division season)
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

(define-public-memoized (division-results division season)
  (define schedule (division-schedule division season))
  (map
    (lambda (day day-index)
      (map
        (lambda (match)
          (match-result match
            (+
              (* season n-seconds-per-year)
              (* day-index n-seconds-per-week))))
        day))
    schedule
    (range 0 (length schedule))))

(define-public-memoized (division-points division season)
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

(define-public (division-rankings division season)
  (define teams (division-teams division season))
  (define points (division-points division season))
  (define team-points-pairs (map cons teams points))
  (map car
    (sort
      team-points-pairs
      (lambda (team-points-pair1 team-points-pair2)
        (> (cdr team-points-pair1) (cdr team-points-pair2))))))

(define-public-memoized (division-starters-attr attr division season)
  (sum
    (map
      (lambda (team)
        (team-starters-attr attr team (* season n-seconds-per-year)))
      (division-teams division season))))
