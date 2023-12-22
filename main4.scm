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
(define conf-sim-start-date (date 2023 7 1))
(define conf-sim-end-date (date 2024 7 1))

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
  ; comp_inst
  (sqlite3-execute-sql db
    "insert into comp_inst
    (comp_id, season)
    select
    id, ?
    from comp"
    (list (date-year conf-sim-start-date)))
  ; comp_inst_team
  (sqlite3-execute-sql db
    "insert into comp_inst_team
    (comp_inst_id, team_id)
    select
    a.comp_inst_id, a.team_id
    from (
      select
      comp_inst.id comp_inst_id, team.id team_id,
      row_number() over(partition by team.country_id order by team.name) rn
      from comp_inst
      join comp on comp.id = comp_inst.comp_id
      join team on team.country_id = comp.country_id
    ) a
    where a.rn <= ?"
    (list conf-n-teams-per-division))
)

(define (do-day day)
  (d "Doing day:" (iso-8601-date day))
  (sqlite3-execute-sql db
    "insert into comp_inst
    (comp_id, season)
    select
    ci_prev.comp_id, ci_prev.season + 1
    from comp_inst ci_prev
    join comp on comp.id = ci_prev.comp_id
    where ci_prev.season = (? - 1)
    and comp.start_month = mod(? - 1 + 3, 12) + 1
    and comp.start_day = ?
    and not exists(
     /* Don't redo the first season that was created during initialization. */
      select 1
      from comp_inst
      where comp_inst.id = ci_prev.comp_id
      and comp_inst.season = ci_prev.season + 1
    )
    "
    (list (date-year day) (date-month day) (date-day day)))
  (sqlite3-execute-sql db
    "insert into comp_inst_team
      (comp_inst_id, team_id)
      select
      comp_inst.id, comp_inst_team.team_id
      from comp_inst
      join comp on comp.id = comp_inst.comp_id
      join comp_inst comp_inst_prev
            on comp_inst_prev.comp_id = comp_inst.comp_id
            and comp_inst_prev.season = comp_inst.season - 1
      join comp_inst_team on comp_inst_team.comp_inst_id = comp_inst_prev.id
      where comp_inst.season = ?
      and comp.start_month = mod(? - 1 + 3, 12) + 1
      and comp.start_day = ?"
     (list (date-year day) (date-month day) (date-day day)))
)

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
