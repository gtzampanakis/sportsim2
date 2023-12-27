(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))
(use-modules (ice-9 ftw))
(use-modules (ice-9 regex))
(use-modules (ice-9 string-fun))
(use-modules (ice-9 textual-ports))
(use-modules (date))
(use-modules (util))
(use-modules (math))
(use-modules (sportsim2))
(use-modules (db sqlite3))

(define conf-n-countries 3)
(define conf-n-teams-per-country 40)
(define conf-n-teams-per-division 8)
(define conf-sim-start-date (date 2023 5 1))
(define conf-sim-end-date (date 2025 5 1))
(define conf-n-players-per-country 2000)

(define db-path "db.db")
(define db (sqlite3-open db-path))

(define get-keyval
  (case-lambda
    ((key)
     (get-keyval key #f))
    ((key default)
      (define rows
        (sqlite3-execute-sql db
          "select value from keyval where key = ?" (list key)))
      (if (null? rows)
        default
        (car (car rows))))))

(define set-keyval
  (case-lambda
    ((key value)
     (sqlite3-execute-sql db
        "insert or replace into keyval (key, value) values (?, ?)"
        (list key value)))))

(define (migrate)
  (define migration-table-exists
    (sqlite3-execute-sql-exists db
      "select 1 from sqlite_master where type='table' and name='migration'"))
  (when (not migration-table-exists)
    (sqlite3-execute-sql db
      "create table migration(id integer primary key, name text)"))
  (define migration-files
    (sort
      (filter
        (lambda (filename)
          (string-match "[0-9][0-9][0-9][0-9].*\\.sql" filename))
        (map car (cddr (file-system-tree "migrations"))))
      string<))
  (for-each
    (lambda (filename)
      (when (not (sqlite3-execute-sql-exists db
                    "select 1 from migration where name = ?" (list filename)))
        (let
          ((file-content
             (get-string-all
               (open-file (string-append "migrations/" filename) "r"))))
          (d "Executing migration" filename)
          (sqlite3-execute-sql db "begin")
          (sqlite3-execute-multiple-sql db file-content)
          (sqlite3-execute-sql db
             "insert into migration (name) values (?)" (list filename))
          (sqlite3-execute-sql db "commit"))))
    migration-files))

(define (init-sim)
  ; country
  (sqlite3-execute-sql db
    "with recursive cnt(x)
      as (select 1 union all select x+1 from cnt where x<?)
    insert into country
    (name)
    select cnt.x
    from cnt"
    (list conf-n-countries))
  ; team
  (sqlite3-execute-sql db
      "with recursive cnt(x)
        as (select 1 union all select x+1 from cnt where x<?)
      insert into team
      (name, country_id)
      select cnt.x, c.id
      from cnt
      cross join country c"
      (list conf-n-teams-per-country))
  ; comp
  (sqlite3-execute-sql db
    "insert into comp
    (name, country_id, start_month, start_day, start_dow)
    select
    'league', id, 8, 1, 0
    from country")
  ; player
  (sqlite3-execute-sql db
    "with recursive cnt(x)
      as (select 1 union all select x+1 from cnt where x<?)
    insert into player
    (name, dob)
    select cnt.x, date(?, '-25 years')
    from cnt"
    (list
      (*
        conf-n-players-per-country
        conf-n-countries)
      conf-sim-start-date))
  ; playerattr
  (sqlite3-execute-sql db
    "insert into playerattr
    (player_id, on_date, rat_att, rat_def, rat_vel)
    select id, ?, 1500., 1500., 1500.
    from player"
    (list conf-sim-start-date))
  ; teamfinance
  (sqlite3-execute-sql db
    "insert into teamfinances
    (team_id, on_date, balance)
    select id, ?, 1000 * 1000
    from team"
    (list conf-sim-start-date)))

(define (schedule-matches ci-id team-ids no-earlier-than)
  (define start-date
    (let loop ((d no-earlier-than))
      (if (= (date-week-day d) 0)
        d
        (loop (add-day d)))))
  (for-each
    (lambda (matchday-schedule matchday-index)
      (for-each
        (lambda (teams)
          (sqlite3-execute-sql db
            "insert into match
            (comp_inst_id, matchday, matchdate,
             home_team_id, away_team_id, finished)
            values
            (?, ?, ?, ?, ?, #f)"
            (list
              ci-id
              matchday-index
              (iso-8601-date (add-days start-date (* 7 matchday-index)))
              (list-ref team-ids (car teams))
              (list-ref team-ids (cadr teams)))))
        matchday-schedule))
    (gen-round-robin (length team-ids))
    (range 0 (length (gen-round-robin (length team-ids))))))

(define (schedule-comp comp day)
  (when (not (sqlite3-execute-sql-exists db
                 "select 1 from comp_inst ci
                 where ci.comp_id = ?
                 and ci.season = ?"
                 (list comp (date-year day))))
    (d "Scheduling comp for season:" (date-year day))
    (let (
      (ci
        (sqlite3-execute-sql-first-flat db
          "insert into comp_inst
          (comp_id, season)
          values
          (?, ?)
          returning id"
          (list comp (date-year day))))
      (ci-prev (sqlite3-execute-sql-first-flat db
        "select ci.id
        from comp_inst ci
        where ci.comp_id = ?
        and ci.season = ?"
        (list comp (1- (date-year day))))))
      (define team-ids
        (if ci-prev
          (sqlite3-execute-sql-flat db
            "insert into comp_inst_team
            (comp_inst_id, team_id)
            select ?, cit.team_id
            from comp_inst_team cit
            where cit.comp_inst_id = ?
            returning team_id"
            (list ci ci-prev))
          (sqlite3-execute-sql-flat db
            "insert into comp_inst_team
            (comp_inst_id, team_id)
            select
            a.comp_inst_id, a.team_id
            from (
              select
              comp_inst.id comp_inst_id, team.id team_id,
              row_number()
                over(partition by team.country_id order by team.name) rn
              from comp_inst
              join comp on comp.id = comp_inst.comp_id
              join team on team.country_id = comp.country_id
              where comp_inst.id = ?
            ) a
            where a.rn <= ?
            returning team_id"
            (list ci conf-n-teams-per-division))))
      (let* (
        (month-day (sqlite3-execute-sql-first db
                           "select start_month, start_day
                           from comp
                           where id = (
                             select comp_id from comp_inst where id = ?)"
                           (list ci)))
        (no-earlier-than
          (date (date-year day) (car month-day) (cadr month-day))))
        (schedule-matches ci team-ids no-earlier-than)))))

(define (do-day day)
  (d "Doing day:" (iso-8601-date day))
  (for-each
    (lambda (comp) (schedule-comp comp day))
    (sqlite3-execute-sql-flat db
      "select comp.id
      from comp
      where date(
         ? || '-01-01',
          (comp.start_month - 1) || ' months',
          (comp.start_day - 1) || ' days'
      ) <= date(?, '3 months')
      and not exists(
         select 1
         from comp_inst ci
         where ci.comp_id = comp.id
         and ci.season = ?
      )"
      (list
        (date-year day)
        (iso-8601-date day)))))

(define (main)
  (migrate)
  (when (not (get-keyval "sim_initialized"))
    (init-sim)
    (set-keyval "sim_initialized" 1))
  (let loop ((day conf-sim-start-date))
    (do-day day)
    (when (date<? (add-day day) conf-sim-end-date)
      (loop (add-day day))))
)

(main)
