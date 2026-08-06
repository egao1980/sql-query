(in-package #:sql-query/tests)

(deftest postgres-types-datetime-json-array
  (let ((d (sql-query-postgres:make-postgres-dialect)))
    (ok (string= "JSONB" (dialect-type-sql d :jsonb)))
    (ok (string= "TIMESTAMPTZ" (dialect-type-sql d :timestamptz)))
    (ok (string= "INTEGER[]" (dialect-type-sql d '(:array :integer))))
    (ok (string= "BYTEA" (dialect-type-sql d :bson)))
    (let ((sql (%sql (select (columns (typed 1700000000 :timestamptz)
                                      (typed '(1 2 3) :int-array)
                                      (typed "{\"a\":1}" :jsonb))
                             (from :t))
                     d)))
      (%assert-contains sql "to_timestamp" "ARRAY[" "CAST(" "AS JSONB"))))

(deftest postgres-ops-and-helpers
  (let* ((d (sql-query-postgres:make-postgres-dialect))
         (sql (%sql (select
                     (columns (sql-query-postgres:jsonb-text :payload "name")
                              (sql-query-postgres:jsonb-ref :payload "meta")
                              (sql-query-postgres:date-trunc "day" :created)
                              (sql-query-postgres:now)
                              (sql-func :extract (sql-raw "hour") :created)
                              (sql-func :current-date)
                              (array-lit 1 2 3)
                              (sql-query-postgres:array-agg :id))
                     (from :events)
                     (where (sql-query-postgres:jsonb-contains
                             :payload (typed "{\"ok\":true}" :jsonb))))
                    d)))
    (%assert-contains sql
                      "->>" "->" "date_trunc" "now(" "EXTRACT(" "FROM"
                      "CURRENT_DATE" "ARRAY[" "array_agg" "@>" "AS JSONB")))

(deftest postgres-func-registry-rename
  (let ((d (sql-query-postgres:make-postgres-dialect)))
    (ok (find-sql-func d :jsonb-set))
    (ok (string= "jsonb_set" (sql-func-sql-name (find-sql-func d :jsonb-set))))
    (%assert-contains (%sql (select (columns (sql-func :gen-random-uuid)) (from :t)) d)
                      "gen_random_uuid(")))

(deftest sqlite-types-json-datetime
  (let ((d (sql-query-sqlite3:make-sqlite3-dialect)))
    (ok (string= "TEXT" (dialect-type-sql d :json)))
    (ok (string= "TEXT" (dialect-type-sql d :timestamptz)))
    (ok (string= "BLOB" (dialect-type-sql d :bson)))
    (let ((sql (%sql (select
                      (columns (sql-query-sqlite3:json-extract :doc "$.name")
                               (sql-query-sqlite3:strftime "%Y-%m-%d" :created)
                               (sql-query-sqlite3:unixepoch)
                               (sql-query-sqlite3:json-object "a" 1)
                               (ensure-expr '(:->> :doc "$.x")))
                      (from :t))
                     d)))
      (%assert-contains sql
                        "json_extract" "strftime" "unixepoch" "json_object"
                        "->>"))))

(deftest sqlite-func-registry
  (let ((d (sql-query-sqlite3:make-sqlite3-dialect)))
    (ok (find-sql-func d :json-set))
    (%assert-contains (%sql (select (columns (sql-func :json-array-length :doc)) (from :t)) d)
                      "json_array_length(")))
