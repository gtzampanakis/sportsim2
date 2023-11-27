(use-modules (sportsim2))

(use-modules (util))
(use-modules (math))

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
    (range 0 10))
)

;(use-modules (statprof))
;(statprof main)
(main)
