(define-module (math))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-43))

(use-modules (util))

(define-public (vektor-display v)
  (d v)
  (d))

(define-public (vektor-zeros l)
  (make-list l 0))

(define-public (vektor-sum v)
  (fold + 0 v))

(define-public (vektor-prod v)
  (fold * 1 v))

(define-public (vektor-random l)
	(map (lambda (_) (random:uniform)) (range 0 l)))

(define-public (vektor-scalar-prod v a)
  (map (lambda (c) (* a c)) v))

(define-public (vektor-inner-prod . vs)
  (sum (apply map * vs)))

(define-public (vektor-length v)
  (sqrt (vektor-inner-prod v v)))

(define-public (vektor-normalize v)
  (vektor-scalar-prod v (/ 1 (vektor-length v))))

(define-public (vektor-set! v i n)
  (list-set! v i n))

(define-public (matrix-scalar-prod m a)
  (map
    (lambda (row)
      (vektor-scalar-prod row a))
    m))

(define-public (matrix-display m)
  (for-each d m)
  (d))

(define-public (matrix-dim m)
  (values (length m) (length (car m))))

(define-public (matrix-index m i j)
  (list-ref (list-ref m i) j))

(define-public (matrix-zeros r c)
  (map
    (lambda (_) (make-list c 0))
    (range 0 r)))

(define-public (matrix-random r c)
	(map
		(lambda (_) (vektor-random c))
    (range 0 r)))

(define-public (matrix-symmetric-random d)
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

(define-public (matrix-identity d)
  (define m (matrix-zeros d d))
  (for-each
    (lambda (i)
      (matrix-set! m i i 1))
    (range 0 d))
  m)

(define-public (matrix-diag m)
  (map
    (lambda (row i)
      (matrix-index m i i))
    m
    (range 0 (matrix-dim m))))

(define-public (matrix-diagonal-with-given-diag diag)
  (define ld (length diag))
  (define m (matrix-zeros ld ld))
  (for-each
    (lambda (i val)
      (matrix-set! m i i val))
    (range 0 ld)
    diag)
  m)

(define-public (matrix-set! m i j n)
  (list-set! (list-ref m i) j n))

(define-public (matrix-row m i)
  (list-ref m i ))

(define-public (matrix-row-set! m i row)
  (list-set! m i row))

(define-public (broadcast vs)
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
(define-public (vektor-wise proc . vs)
  (apply map proc (broadcast vs)))
(define-public (vektor+ . vs)
  (apply vektor-wise + vs))
(define-public (vektor- . vs)
  (apply vektor-wise - vs))
(define-public (vektor* . vs)
  (apply vektor-wise * vs))

(define-public (matrix-wise proc . vs)
  (apply vektor-wise proc vs))
(define-public (matrix+ . vs)
  (apply matrix-wise vektor+ vs))
(define-public (matrix- . vs)
  (apply matrix-wise vektor- vs))
(define-public (matrix* . vs)
  (apply matrix-wise vektor* vs))

(define-public (matrix-transpose m)
  (apply map list m))

(define-public (matrix-dot . ms)
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

(define-public (vektor-proj v v-to)
  (vektor-scalar-prod
    v-to
    (/ (vektor-inner-prod v v-to) (vektor-inner-prod v-to v-to))))

(define-public (gram-schmidt m)
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

(define-public (matrix-subdiagonal-abs-sum m)
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

(define-public (matrix-map proc m)
  (map (lambda (row) (map proc row)) m))

(define-public (qr-decomposition m)
  (define mt (matrix-transpose m))
  (define q (gram-schmidt mt))
  (define r (matrix-transpose (matrix-dot mt q)))
  (values q r))

(define-public (qr-algorithm m)
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

(define-public (solve-linear-system a b)
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
