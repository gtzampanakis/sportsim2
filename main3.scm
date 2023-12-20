(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))
(use-modules (ice-9 string-fun))
(use-modules (date))
(use-modules (util))
(use-modules (math))
(use-modules (sportsim2))
(use-modules (db sqlite3))

(define db-schema-version 1)

(define conf-n-countries 3)
(define conf-n-teams-per-country 6)
(define conf-n-players-per-team 4)
(define conf-n-players-in-match 2)
(define conf-n-teams-promoted 3)
(define conf-sim-start-date (date 2023 8 1))
(define conf-sim-end-date (date 2025 7 1))

(define-record-type <keyval>
  (make-keyval id key value)
  keyval?
  (id keyval-id)
  (key keyval-key)
  (value keyval-value))

(define-record-type <country>
  (make-country id name)
  country?
  (id country-id)
  (name country-name))

(define-record-type <team>
  (make-team id name country_id)
  team?
  (id team-id)
  (name team-name)
  (country_id team-country_id))

(define-record-type <competition>
  (make-competition id name country_id start_month start_day start_dow)
  competition?
  (id competition-id)
  (name competition-name)
  (country_id competition-country_id)
  (start_month competition-start_month)
  (start_day competition-start_day)
  (start_dow competition-start_dow))

(define-record-type <competition_season>
  (make-competition_season id competition_id season)
  competition_season?
  (id competition_season-id)
  (competition_id competition_season-competition_id)
  (season competition_season-season))

(define-record-type <competition_season_team>
  (make-competition_season_team id competition_season_id team_id)
  competition_season_team?
  (id competition_season_team-id)
  (competition_season_id competition_season_team-competition_season_id)
  (team_id competition_season_team-team_id))

(define-record-type <match>
  (make-match
    id
    competition_season_id
    matchday
    home_team_id
    away_team_id
    home_score
    away_score
    finished)
  match?
  (id match-id)
  (competition_season_id match-competition_season_id)
  (matchday match-matchday)
  (home_team_id match-home_team_id)
  (away_team_id match-away_team_id)
  (home_score match-home_score)
  (away_score match-away_score)
  (finished match-finished))

(define all-types
  (list
    <keyval>
    <country>
    <team>
    <competition>
    <competition_season>
    <competition_season_team>
    <match>
))

(define (rtd-to-table-name rtd)
  (assoc-ref
    (list
      (cons <keyval> 'sim_keyval)
      (cons <country> 'sim_country)
      (cons <team> 'sim_team)
      (cons <competition> 'sim_competition)
      (cons <competition_season> 'sim_competition_season)
      (cons <competition_season_team> 'sim_competition_season_team)
      (cons <match> 'sim_match)
    )
    rtd))

(define load-keyval-value
  (case-lambda
    ((db key)
     (load-keyval-value db key #f))
    ((db key default)
      (define record #f)
      (sqlite3-for-each-by-select
        (lambda (r) (set! record r))
        make-keyval
        db
        (rtd-to-table-name <keyval>)
        (record-type-fields <keyval>)
        (list (cons 'key key))
        1)
      (if (equal? #f record) default (keyval-value record)))))

(define (sql-create-table-col-list rtd)
  (string-append
    "("
    (string-join
      (cons
        "id integer primary key"
        (map symbol->string
          (filter
            (lambda (field) (not (eq? field 'id)))
            (record-type-fields rtd))))
      ", ")
    ")"))

(define (generate-countries db)
  (d "Generating countries...")
  (sqlite3-execute-sql db
    "with recursive cnt(x)
      as (select 1 union all select x+1 from cnt where x<?)
    insert into sim_country
    (name)
    select cnt.x
    from cnt"
    (list conf-n-countries))
  (d "Done generating countries"))

(define (generate-teams db)
  (d "Generating teams...")
  (sqlite3-execute-sql db
      "with recursive cnt(x)
        as (select 1 union all select x+1 from cnt where x<?)
      insert into sim_team
      (name, country_id)
      select cnt.x, c.id
      from cnt
      cross join sim_country c"
      (list conf-n-teams-per-country))
  (d "Done generating teams"))

(define (generate-competitions db)
  (d "Generating competitions...")
  (sqlite3-execute-sql db
    "insert into sim_competition
    (name, country_id, start_month, start_day, start_dow)
    select
    'league', c.id, 8, 1, 0
    from sim_country c"
    )
  (d "Done generating competitions"))

(define (create-tables db)
  (for-each
    (lambda (rtd)
      (sqlite3-execute-sql db
        (string-replace-substring
          (string-replace-substring
            (string-append
              "create table if not exists "
              "{table} "
              "{col-list}")
            "{table}" (rtd-to-table-name rtd))
          "{col-list}" (sql-create-table-col-list rtd))))
    all-types)
  (sqlite3-save-record
    db rtd-to-table-name
    (make-keyval #f "db-schema-version" db-schema-version)))

(define (generate-entities db)
  (when (not (equal? (load-keyval-value db "generate-entities-done") 1))
    (generate-countries db)
    (generate-teams db)
    (generate-competitions db)
    (sqlite3-save-record db rtd-to-table-name
     (make-keyval #f "generate-entities-done" 1))))

(define (schedule-competition db competition date)
  (d "Scheduling..." db competition date))

(define (schedule-seasons db date)
  ; Find competitions that start in 3 months' time.
  (define cs-ids
    (map car
      (sqlite3-execute-sql db
        "
        insert into sim_competition_season
        (competition_id, season)
        select comp.id, cast(strftime('%Y', date(?, '+3 months')) as integer) + 1
        from
        sim_competition comp
        where 1=1
        and start_month = cast(strftime('%m', date(?, '+3 months')) as integer)
        and start_day = cast(strftime('%d', date(?, '+3 months')) as integer)
        returning id
        "
        (list date date date))))
  (for-each
    (lambda (cs-id)
      (define cst-ids
        (map car
          (sqlite3-execute-sql db
            "
            insert into sim_competition_season_team
            (competition_season_id, team_id)
            select
            cs.id, t.id
            from sim_competition_season cs
            join sim_competition comp on comp.id = cs.competition_id
            join sim_team t on t.country_id = comp.country_id
            where cs.id = ?
            returning id
            "
            (list cs-id))))
      (d "foobar" cst-ids)
      5)
    cs-ids)
  ;(for-each
  ;  (lambda (cs-id)
  ;    (sqlite3-execute-sql db
  ;      "
  ;      insert into sim_match
  ;      (competition_season_id, matchday, home_team_id, away_team_id)
  ;      select
  ;      from
  ;      sim_competition_season cs
  ;      where cs.id = ?
  ;      "
  ;      (list cs-id)))
  ;  cs-ids)
  )

(define (do-day db date)
  (d (iso-8601-datetime (current-date)) "Doing day" (iso-8601-date date))
  (schedule-seasons db date)
  (d (iso-8601-datetime (current-date)) "Done day" (iso-8601-date date)))

(define (main)
  (define db (sqlite3-open "db.db"))
  (create-tables db)
  (generate-entities db)

  (let loop ((date conf-sim-start-date))
    (when (date<? date conf-sim-end-date)
      (do-day db date)
      (loop (add-day date))))

  (sqlite3-close db)
)

(main)
