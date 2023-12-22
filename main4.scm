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

(define db-path "db.db")
(define db (sqlite3-open db-path))

(define (migrate)
  (define migration_table_exists
    (sqlite3-execute-sql-exists db
      "select 1 from sqlite_master where type='table' and name='migration'"))
  (when (not migration_table_exists)
    (sqlite3-execute-sql db
      "create table migration(id integer primary key, name text)"))
  (define migration_files
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
          ((file_content
             (get-string-all
               (open-file (string-append "migrations/" filename) "r"))))
          (d "Executing migration" filename)
          (sqlite3-execute-sql db "begin")
          (sqlite3-execute-multiple-sql db file_content)
          (sqlite3-execute-sql db
             "insert into migration (name) values (?)" (list filename))
          (sqlite3-execute-sql db "commit"))))
    migration_files))

(define (init_sim)
  (sqlite3-execute-sql db
    "with recursive cnt(x)
      as (select 1 union all select x+1 from cnt where x<?)
    insert into country
    (name)
    select cnt.x
    from cnt"
    (list conf-n-countries))
  (sqlite3-execute-sql db
      "with recursive cnt(x)
        as (select 1 union all select x+1 from cnt where x<?)
      insert into team
      (name, country_id)
      select cnt.x, c.id
      from cnt
      cross join country c"
      (list conf-n-teams-per-country))
  (sqlite3-execute-sql db
    "insert into comp
    (name, country_id, start_month, start_day, start_dow)
    select
    'league', id, 8, 1, 0
    from country")
)

(define (main)
  (migrate)
  (init_sim)
)

(main)

;(define (run-sequence procs)
;  (let loop ((procs procs))
;    (let ((proc-impure (car procs)) (proc-pure (cadr procs)))
;      (let ((new-procs (proc-pure (proc-impure))))
;        (if (equal? new-procs 'done)
;          'done
;          (loop new-procs))))))
;
;(define (main _)
;  (lambda () '())kk
;)
;
;(run-sequence
;  (list
;    (lambda () '())
;    main))
