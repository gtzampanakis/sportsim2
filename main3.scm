(use-modules (srfi srfi-9))
(use-modules (ice-9 string-fun))
(use-modules (util))
(use-modules (math))
(use-modules (sportsim2))
(use-modules (db sqlite3))

(define conf-n-countries 30)
(define conf-n-teams-per-country 30)
(define conf-n-teams-per-division 6)
(define conf-n-players-per-team 4)
(define conf-n-players-in-match 2)
(define conf-n-teams-promoted 3)

(define-record-type <status>
  (make-status key value)
  status?
  (id status-id)
  (key status-key)
  (value status-value))

(define-record-type <country>
  (make-country name)
  country?
  (id country-id)
  (name country-name))

(define-record-type <team>
  (make-team name)
  team?
  (id team-id)
  (name team-name)
  (country_id team-country))

(define all-types
  (list <status> <country> <team>))

(define (sql-insert-col-list rtd)
  (string-append
    "("
    (string-join
      (map symbol->string (record-type-fields rtd))
      ", ")
    ")"))

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

(define (sql-insert-placeholders-list rtd)
  (string-append
    "("
    (string-join
      (map (lambda (_) "?") (record-type-fields rtd))
      ", ")
    ")"))

(define (pars-list record)
  (let ((rtd (record-type-descriptor record)))
    (map
      (lambda (field)
        ((record-accessor rtd field) record))
      (record-type-fields rtd))))

(define (rtd-to-table-name rtd)
  (assoc-ref
    (list
      (cons <status> "sim_status")
      (cons <country> "sim_country")
      (cons <team> "sim_team"))
    rtd))

(define (save-record db record)
  (define rtd (record-type-descriptor record))
  (define sql
    (string-replace-substring
      (string-replace-substring
        (string-replace-substring
          "insert or replace
          into {table}
          {col-list}
          values
          {placeholders-list}"
          "{table}" (rtd-to-table-name rtd))
        "{col-list}" (sql-insert-col-list rtd))
      "{placeholders-list}" (sql-insert-placeholders-list rtd)))
  (define pars (pars-list record))
  (sqlite3-execute db sql pars))

(define (gen-countries db)
  (for-each
    (lambda (_)
      (let ((record (make-country (string-append "country-" (uuid)))))
        (save-record db record)))
    (range 0 conf-n-countries)))

(define (create-tables db)
  (for-each
    (lambda (rtd)
      (sqlite3-execute db
        (string-replace-substring
          (string-replace-substring
            "create table if not exists
            {table}
            {col-list}"
            "{table}" (rtd-to-table-name rtd))
          "{col-list}" (sql-create-table-col-list rtd))))
    all-types))

(define (generate-entities db)
  (when (not (null?
      (sqlite3-execute db
        "select 1
        from sim_status
        where key = 'generate-entities-done'
        and value = 1
        limit 1")))
    (gen-countries db)
    (save-record db (make-status "generate-entities-done" 1))))

(define (main)
  (define db (sqlite3-open "db.db"))
  (create-tables db)
  (generate-entities db)

  (gen-countries db)

  (sqlite3-close db)
)

(main)
