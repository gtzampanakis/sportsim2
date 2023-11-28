(use-modules (util))
(use-modules (math))
(use-modules (sportsim2))

(for-each
  (lambda (season)
    (define teams (division-rankings 0 season))
    (d teams)
    (d (map (lambda (t) (team-division t season)) teams))
    (d (map (lambda (t) (team-ranking t season)) teams))
    (d (map (lambda (t) (team-rank-across-divisions t season)) teams))
    (d (map (lambda (t) (team-support-factor-based-on-last-season t season)) teams))
    (d))
  (range 0 20))
(exit)

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
    (range 0 2))
)

;(use-modules (statprof))
;(statprof main)
(main)
