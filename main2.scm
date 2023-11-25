(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-43))
(use-modules (ice-9 format))

(define n-seconds-per-hour 3600)
(define n-seconds-per-day (* n-seconds-per-hour 24))
(define n-seconds-per-week (* n-seconds-per-day 7))
(define n-seconds-per-month (* n-seconds-per-day 30))
(define n-seconds-per-year (* n-seconds-per-month 12))

(define adj-period n-seconds-per-year)
(define adj-base 0.010)
(define sd-base 0.010)

(define conf-date-start 0)
(define conf-n-divisions-per-country 5)
(define conf-n-teams-per-division 8)
(define conf-n-players-per-team 4)
(define conf-n-players-in-match 2)

(define home-advantage-factor 1.1)

(define player-prop-names (list 'team 'creation-season 'creation-index))
(define player-prop-names-n (length player-prop-names))

(define retirement-age (* 34 n-seconds-per-year))

(define attr-names (list
  'att 'def 'vel
))

; vel is the preference of a player to play fast or slow, not his
; capability to play fast. So it is uncorrelated to attributes that stem
; from his ability.
(define attr-correlations (list
  (cons (list 'att 'def) 0.9)
  (cons (list 'att 'vel) 0.0)
  (cons (list 'def 'vel) 0.0)
))

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

(define (time)
  (let ((p (gettimeofday))) (+ (car p) (/ (cdr p) 1000000.))))

; TODO:
; * cache strategies to keep one cached entry per season so that we can more
; easily go back as far as we want
; * player positions to affect their performance
; * salaries
; * finances
; * introduce countries
; * player nationalities to affect their initial and further attributes
; * dependence on country

(define (memoized-proc proc)
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

(define (pairing-function . ls)
  ; Cantor pairing function. Maps a tuple of nonpositive integers to a unique
  ; nonpositive integer.
  (define (p2 y x)
    (let ((x+y (+ x y)))
      (+ (* (/ 1 2) x+y (+ x+y 1)) y)))
  ; TODO: reduce uses weird order on the first function call. Check if this
  ; matters here.
  (reduce p2 -1 ls))

(define (inverse-pairing-function z n)
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

(define (flatten ls)
  (fold append '() ls))

(define (minabs-index ls)
  (let loop ((ls ls) (c (inf)) (i 0) (r 0))
    (if (null? ls)
      r
      (let* (
          (a (abs (car ls)))
          (o (< a c)))
        (loop
          (cdr ls)
          (if o a c)
          (1+ i)
          (if o i r))))))

(define (proc-prepend-arg f)
  (lambda args
    (apply f (cdr args))))

(define (sum ls)
  (fold + 0 ls))

(define (prod ls)
  (fold * 1 ls))

; START MATRIX STUFF
(define (vektor-display v)
  (d v)
  (d))

(define (vektor-zeros l)
  (make-list l 0))

(define (vektor-sum v)
  (fold + 0 v))

(define (vektor-prod v)
  (fold * 1 v))

(define (vektor-random l)
	(map (lambda (_) (random:uniform)) (range 0 l)))

(define (vektor-scalar-prod v a)
  (map (lambda (c) (* a c)) v))

(define (vektor-inner-prod . vs)
  (sum (apply map * vs)))

(define (vektor-length v)
  (sqrt (vektor-inner-prod v v)))

(define (vektor-normalize v)
  (vektor-scalar-prod v (/ 1 (vektor-length v))))

(define (vektor-set! v i n)
  (list-set! v i n))

(define (matrix-scalar-prod m a)
  (map
    (lambda (row)
      (vektor-scalar-prod row a))
    m))

(define (matrix-display m)
  (for-each d m)
  (d))

(define (matrix-dim m)
  (values (length m) (length (car m))))

(define (matrix-index m i j)
  (list-ref (list-ref m i) j))

(define (matrix-zeros r c)
  (map
    (lambda (_) (make-list c 0))
    (range 0 r)))

(define (matrix-random r c)
	(map
		(lambda (_) (vektor-random c))
    (range 0 r)))

(define (matrix-symmetric-random d)
  (define m (matrix-zeros d d))
  (for-each
    (lambda (i)
      (for-each
        (lambda (j)
          (if (<= i j)
            (let ((v (random:uniform)))
              (matrix-set! m i j v)
              (matrix-set! m j i v))))
        (range 0 d)))
      (range 0 d))
  m)

(define (matrix-identity d)
  (define m (matrix-zeros d d))
  (for-each
    (lambda (i)
      (matrix-set! m i i 1))
    (range 0 d))
  m)

(define (matrix-diag m)
  (map
    (lambda (row i)
      (matrix-index m i i))
    m
    (range 0 (matrix-dim m))))

(define (matrix-diagonal-with-given-diag diag)
  (define ld (length diag))
  (define m (matrix-zeros ld ld))
  (for-each
    (lambda (i val)
      (matrix-set! m i i val))
    (range 0 ld)
    diag)
  m)

(define (matrix-set! m i j n)
  (list-set! (list-ref m i) j n))

(define (matrix-row m i)
  (list-ref m i ))

(define (matrix-row-set! m i row)
  (list-set! m i row))

(define (broadcast vs)
  (define m
    (apply max
      (map
        (lambda (v)
          (if (number? v)
            1
            (length v)))
        vs)))
  (map
    (lambda (v)
      (if (number? v)
        (make-list m v)
        v))
    vs))
(define (vektor-wise proc . vs)
  (apply map proc (broadcast vs)))
(define (vektor+ . vs)
  (apply vektor-wise + vs))
(define (vektor- . vs)
  (apply vektor-wise - vs))
(define (vektor* . vs)
  (apply vektor-wise * vs))

(define (matrix-wise proc . vs)
  (apply vektor-wise proc vs))
(define (matrix+ . vs)
  (apply matrix-wise vektor+ vs))
(define (matrix- . vs)
  (apply matrix-wise vektor- vs))
(define (matrix* . vs)
  (apply matrix-wise vektor* vs))

(define (matrix-transpose m)
  (apply map list m))

(define (matrix-dot . ms)
  (define (matrix-dot-for-2 m1 m2)
    (define m2t (matrix-transpose m2))
    (map
      (lambda (row)
        (map
          (lambda (col)
            (vektor-inner-prod row col))
          m2t))
      m1))
  (reduce-right matrix-dot-for-2 '() ms))

(define (vektor-proj v v-to)
  (vektor-scalar-prod
    v-to
    (/ (vektor-inner-prod v v-to) (vektor-inner-prod v-to v-to))))

(define (gram-schmidt m)
; This is the "classical" gram-schmidt according to wikipedia, which has some
; numerical instability. See Wikipedia for a simple way to make it numerically
; stable if this need arises.
  (let loop ((m m) (res '()))
    (if (null? m)
      (matrix-transpose (map vektor-normalize (reverse res)))
      (let* (
          (vi (car m))
          (ui
            (if (null? res) vi
              (apply vektor- vi (map (lambda (u) (vektor-proj vi u)) res)))))
        (loop (cdr m) (cons ui res))))))

(define (matrix-subdiagonal-abs-sum m)
  (define-values (r c) (matrix-dim m))
  (define s 0)
  (for-each
    (lambda (j)
      (for-each
        (lambda (i)
          (when (< j i)
            (set! s (+ s (abs (matrix-index m i j))))))
        (range 0 r)))
    (range 0 c))
  s)

(define (matrix-map proc m)
  (map (lambda (row) (map proc row)) m))

(define (qr-decomposition m)
  (define mt (matrix-transpose m))
  (define q (gram-schmidt mt))
  (define r (matrix-transpose (matrix-dot mt q)))
  (values q r))

(define (qr-algorithm m)
; Note that this does not always converge, for example it does not converge for
; ((-1 2 5) (2 8 8) (1 8 9)). Such non-convergence happens when two eigenvalues
; are equal or close to equal. We can introduce shifts in order to overcome
; this. But it appears that the algorithm does converge for most symmetric
; matrices and this is the only ones we care about.
  (let loop ((m m) (q-comp (matrix-identity (matrix-dim m))) (i 0))
    (if (or (> i 50000) (< (matrix-subdiagonal-abs-sum m) 1e-20))
      (list (matrix-diag m) q-comp)
		  (let-values (
		   	 ((q r) (qr-decomposition m)))
        (let* ((new-m (matrix-dot r q)))
          (loop new-m (matrix-dot q-comp q) (1+ i)))))))

(define (solve-linear-system a b)
  (define-values (ma na) (matrix-dim a))
  (define-values (q r) (qr-decomposition a))
  (define c (matrix-dot (matrix-transpose q) (matrix-transpose (list b))))
  (define augm (map (lambda (rrow crow) (append rrow crow)) r c))
  (for-each
    (lambda (this-row-i)
      ; First subtract all lower rows so that all elements right to the
      ; diagonal become 0.
      (for-each
        (lambda (other-row-i)
          (matrix-row-set! augm this-row-i
            (vektor+
              (vektor-scalar-prod
                (matrix-row augm other-row-i)
                (- (matrix-index augm this-row-i other-row-i)))
              (matrix-row augm this-row-i))))
        (range (1+ this-row-i) na))
      ; Now divide the whole row by the diagonal so that the diagonal element
      ; becomes 1.
      (matrix-row-set! augm this-row-i
        (vektor-scalar-prod
          (matrix-row augm this-row-i)
          (/ (matrix-index augm this-row-i this-row-i)))))
    (reverse (range 0 na)))
  (car (reverse (matrix-transpose augm))))

; END MATRIX STUFF

(define attrs-covariance
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

(define (rand-mult-normal mean sigma rs)
  (define z (map (lambda (_) (random:normal rs)) (range 0 (matrix-dim sigma))))
  (define vq (qr-algorithm sigma))
  (define v (car vq))
  (define q (cadr vq))
  (define A (matrix-dot q (matrix-diagonal-with-given-diag (map sqrt v))))
  (define result
    (vektor+
      mean
      (car (matrix-transpose (matrix-dot A (matrix-transpose (list z)))))))
  result)

(define (player-name player-id) player-id)

(define (player-id team creation-date creation-index)
  (pairing-function team creation-date creation-index))

(define (player-props player)
  (inverse-pairing-function player player-prop-names-n))

(define (player-prop player prop)
  (list-ref (player-props player) (index player-prop-names prop)))  

(define (player-date-of-birth player-id)
  (define rs (get-random-state 'player-date-of-birth player-id))
  (define creation-season (player-prop player-id 'creation-season))
  (define age-in-years-at-creation-season (+ 16 (* 17 (random:uniform rs))))
  (define date-of-birth-in-years
    (- creation-season age-in-years-at-creation-season))
  (define date-of-birth-in-seconds
    (truncate (years-to-secs date-of-birth-in-years)))
  date-of-birth-in-seconds)

(define (player-age player-id date)
  (- date (player-date-of-birth player-id)))

(define (adj-mplier age)
  (define age-years (secs-to-years age))
  (cond
    ((< age-years 16)  1.5)
    ((< age-years 25)  1.0)
    ((< age-years 30)  0.0)
    ((< age-years 38) -1.0)
    (else             -2.0)))

(define-memoized (player-initial-attrs player-id)
  (define rs (get-random-state 'player-initial-attrs player-id))
  (define vals (rand-mult-normal 0 attrs-covariance rs))
  vals)

; The age is in adj-periods to aid memoization.
(define-memoized (player-attrs-by-age-in-adj-periods
                   player-id age-in-adj-periods)
  (if (<= age-in-adj-periods 0)
    (player-initial-attrs player-id)
    (matrix+
      (player-attrs-by-age-in-adj-periods player-id (- age-in-adj-periods 1))
      (matrix*
        0.05
        (rand-mult-normal
          0
          attrs-covariance
          (get-random-state 'player-attrs player-id age-in-adj-periods))))))

(define (player-attrs player-id date)
  (player-attrs-by-age-in-adj-periods
    player-id
    (truncate/ (player-age player-id date) adj-period)))

(d (player-attrs 5 (* 0 adj-period)))
(d (player-attrs 5 (* 1 adj-period)))
(d (player-attrs 5 (* 2 adj-period)))
(d (player-attrs 5 (* 3 adj-period)))
(d (player-attrs 5 (* 4 adj-period)))
(exit)

(define-memoized (player-attr attr player-id date)
  (define attrs (player-attrs player-id date))
  (list-ref attrs (index attr-names attr)))

(define (player-retired? player-id date)
  (define date-start-of-year
    (* (truncate/ date n-seconds-per-year) n-seconds-per-year))
  (define age-at-start-of-year (player-age player-id date-start-of-year))
  (>= age-at-start-of-year retirement-age))

(define (team-initial-players team-id)
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

(define-memoized (team-starters team-id date)
  (define players (team-players team-id date))
  (define total-proc
    (lambda (player-id)
      (+
        (player-attr 'att player-id date)
        (player-attr 'def player-id date))))
  (define sorted
    (sort players (lambda (a b) (> (total-proc a) (total-proc b)))))
  (take sorted conf-n-players-in-match))

(define-memoized (team-starters-attr attr team-id date)
  (define starters (team-starters team-id date))
  (sum
    (map
      (lambda (player-id)
        (player-attr attr player-id date))
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

(define-memoized (division-teams division season)
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

(define-memoized (player-perf-on-date attr player date)
  (define loc (player-attr attr player date))
  (define rs (get-random-state 'player-perf-on-date attr player date))
  (rand-logistic loc 0.05 rs))

(define-memoized (team-starters-perf-on-date team attr date)
  (sum
    (map
      (lambda (player) (player-perf-on-date attr player date))
    (team-starters team date))))

(define-memoized (match-result teams date)
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
            (+
              (* season n-seconds-per-year)
              (* day-index n-seconds-per-week))))
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

(define (division-rankings division season)
  (define teams (division-teams division season))
  (define points (division-points division season))
  (define team-points-pairs (map cons teams points))
  (map car
    (sort
      team-points-pairs
      (lambda (team-points-pair1 team-points-pair2)
        (> (cdr team-points-pair1) (cdr team-points-pair2))))))

(define-memoized (division-starters-attr attr division season)
  (sum
    (map
      (lambda (team)
        (team-starters-attr attr team (* season n-seconds-per-year)))
      (division-teams division season))))

(define (main)
  (for-each
    (lambda (season)
      (define t0 (time))
      (d season)
      (d (division-rankings 5 season)
        (+
          (division-starters-attr 'att 5 season)
          (division-starters-attr 'def 5 season)))
      (d (division-rankings 6 season)
        (+
          (division-starters-attr 'att 6 season)
          (division-starters-attr 'def 6 season)))
      (d (division-rankings 7 season)
        (+
          (division-starters-attr 'att 7 season)
          (division-starters-attr 'def 7 season)))
      (d (division-rankings 8 season)
        (+
          (division-starters-attr 'att 8 season)
          (division-starters-attr 'def 8 season)))
      (d (division-rankings 9 season)
        (+
          (division-starters-attr 'att 9 season)
          (division-starters-attr 'def 9 season)))
      (define t1 (time))
      (d "time taken" (- t1 t0)))
    (range 0 100))
)

;(use-modules (statprof))
;(statprof main)
(main)
