(define-module (util))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-43))

(define-public d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

(define-public (time)
  (let ((p (gettimeofday))) (+ (car p) (/ (cdr p) 1000000.))))

(define-public (sum ls)
  (fold + 0 ls))

(define-public (prod ls)
  (fold * 1 ls))

(define-public (range s n)
; List of integers >= s and < n.
  (let loop ((i n) (r '()))
    (if (<= i s)
      r
      (loop (1- i) (cons (1- i) r)))))

