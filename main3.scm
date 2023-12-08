(use-modules (srfi srfi-9))
(use-modules (ice-9 string-fun))
(use-modules (util))
(use-modules (math))
(use-modules (sportsim2))
(use-modules (db sqlite3))

(define db-schema-version 1)

(define conf-n-countries 30)
(define conf-n-teams-per-country 30)
(define conf-n-teams-per-division 6)
(define conf-n-players-per-team 4)
(define conf-n-players-in-match 2)
(define conf-n-teams-promoted 3)

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
  (country_id team-country))

(define all-types
  (list <keyval> <country> <team>))

(define load-keyval-value
  (case-lambda
    ((db key)
     (load-keyval-value db key #f))
    ((db key default)
      (define records
        (sqlite3-execute-select
          db
          (rtd-to-table-name <keyval>)
          (record-type-fields <keyval>)
          make-keyval
          (list (cons 'key key))
          1))
      (if (null? records) default (keyval-value (car records))))))

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

(define (rtd-to-table-name rtd)
  (assoc-ref
    (list
      (cons <keyval> 'sim_keyval)
      (cons <country> 'sim_country)
      (cons <team> 'sim_team))
    rtd))

(define (gen-countries db)
  (for-each
    (lambda (_)
      (let ((record (make-country #f (string-append "country-" (uuid)))))
        (sqlite3-save-record db rtd-to-table-name record)))
    (range 0 conf-n-countries)))

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
    (sqlite3-save-record db rtd-to-table-name
     (make-keyval #f "generate-entities-done" 1))))

(define (main)
  (define db (sqlite3-open "db.db"))
  (create-tables db)
  (generate-entities db)
  (sqlite3-close db)
)

(main)
