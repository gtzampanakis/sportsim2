(use-modules (srfi srfi-9))
(use-modules (ice-9 string-fun))
(use-modules (date))
(use-modules (util))
(use-modules (math))
(use-modules (sportsim2))
(use-modules (db sqlite3))

(define db-schema-version 1)

(define conf-n-countries 3)
(define conf-n-teams-per-country 18)
(define conf-n-teams-per-division 6)
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
  (make-team id name)
  team?
  (id team-id)
  (name team-name)
  (country_id team-country_id))

(define-record-type <competition>
  (make-competition id name country_id season)
  competition?
  (id competition-id)
  (name competition-name)
  (country_id competition-country_id)
  (season competition-season))

(define all-types
  (list <keyval> <country> <team> <competition>))

(define (rtd-to-table-name rtd)
  (assoc-ref
    (list
      (cons <keyval> 'sim_keyval)
      (cons <country> 'sim_country)
      (cons <team> 'sim_team)
      (cons <competition> 'sim_competition))
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

(define (gen-countries db)
  (d "Generating countries...")
  (for-each
    (lambda (_)
      (let ((record (make-country #f (string-append "country-" (uuid)))))
        (sqlite3-save-record db rtd-to-table-name record)))
    (range 0 conf-n-countries))
  (d "Done generating countries"))

(define (generate-teams-for-country db country)
  (for-each
    (lambda (_)
      (define record
        (make-team #f (string-append "team-" (uuid))))
      (sqlite3-save-record db rtd-to-table-name record))
    (range 0 conf-n-teams-per-country)))

(define (gen-teams db)
  (d "Generating teams...")
  (sqlite3-for-each-by-select
    (lambda (country) (generate-teams-for-country db country))
    make-country
    db
    (rtd-to-table-name <country>)
    (record-type-fields <country>))
  (d "Done generating teams"))

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
    (gen-countries db)
    (gen-teams db)
    (sqlite3-save-record db rtd-to-table-name
     (make-keyval #f "generate-entities-done" 1))))

(define (do-day date)
  (d (iso-8601-date date)))

(define (main)
  (define db (sqlite3-open "db.db"))
  (create-tables db)
  (generate-entities db)

  (let loop ((date conf-sim-start-date))
    (when (date<? date conf-sim-end-date)
      (do-day date)
      (loop (add-day date))))

  (sqlite3-close db)
)

(main)
