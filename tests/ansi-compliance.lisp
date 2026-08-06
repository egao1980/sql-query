(in-package #:sql-query/tests)

;;;; First-party ANSI/ISO SQL Foundation compliance suite for ansi-dialect.
;;;;
;;;; Not vendored from NIST / sqltest / sqllogictest. Cases are original Rove
;;;; tests inspired by well-known SQL corner cases and Foundation feature areas
;;;; (query, predicates, expressions, DML, DDL, SQL/PSM, negative vendorisms).
;;;;
;;;; Target: compile AST → ANSI SQL text (+ params). Not engine execution.

;;; ---------------------------------------------------------------------------
;;; Query expression
;;; ---------------------------------------------------------------------------

(deftest ansi-select-list-and-from
  (let ((sql (%sql (select (columns :id (label :name :n) (sql-raw "1"))
                           (from :users "u")))))
    (%assert-contains sql "SELECT" "FROM" "AS")
    (ok (search "\"users\"" sql))))

(deftest ansi-distinct-vs-all
  (let ((sql (%sql (select (distinct) (columns :id) (from :t)))))
    (%assert-contains sql "SELECT DISTINCT")
    (%assert-absent sql "DISTINCT ON")))

(deftest ansi-fetch-first-and-offset-rows
  "SQL:2008-style pagination — not LIMIT/OFFSET vendor form."
  (let* ((both-stmt (select (columns :id) (from :t) (offset 5) (limit 10)))
         (both (%sql both-stmt))
         (fetch-only (%sql (select (columns :id) (from :t) (limit 3))))
         (off-only (%sql (select (columns :id) (from :t) (offset 2)))))
    (%assert-contains both "OFFSET" "ROWS" "FETCH FIRST" "ROWS ONLY")
    (%assert-absent both "LIMIT")
    ;; ANSI emit: OFFSET then FETCH → params in that order
    (ok (equal '(5 10) (%params both-stmt)))
    (%assert-contains fetch-only "FETCH FIRST")
    (%assert-contains off-only "OFFSET" "ROWS")
    (%assert-absent off-only "FETCH")))

(deftest ansi-order-group-having
  (let ((sql (%sql (select (columns :dept (count :*))
                           (from :emp)
                           (group-by :dept)
                           (having (:> (count :*) 1))
                           (order-by '(:salary :desc) :dept)))))
    (%assert-contains sql "GROUP BY" "HAVING" "ORDER BY" "DESC")))

(deftest ansi-for-update-of
  (let ((sql (%sql (select (columns :id) (from :t) (for-update :of '(:id))))))
    (%assert-contains sql "FOR UPDATE" "OF")
    (%assert-absent sql "NOWAIT" "SKIP LOCKED")))

(deftest ansi-set-operations
  (let* ((a (select (columns :id) (from :a)))
         (b (select (columns :id) (from :b))))
    (%assert-contains (%sql (union a b)) "UNION")
    (%assert-contains (%sql (union-all a b)) "UNION ALL")
    (%assert-contains (%sql (intersect a b)) "INTERSECT")
    (%assert-contains (%sql (except a b)) "EXCEPT")
    (%assert-contains (%sql (intersect-all a b)) "INTERSECT ALL")
    (%assert-contains (%sql (except-all a b)) "EXCEPT ALL")))

(deftest ansi-cte-and-recursive-flag
  (let* ((q (select (columns :id) (from :t)))
         (sql (%sql (select (columns :id)
                            (with-cte (cte :x q) (cte :y q :recursive t))
                            (from :x)))))
    (%assert-contains sql "WITH" "RECURSIVE" "AS (")))

(deftest ansi-subquery-exists-unique-pred
  (let ((sql (%sql (select (columns :id)
                           (from :t)
                           (where (sql-and
                                   (exists (select (columns 1) (from :u)
                                                   (where (:= :u.t-id :t.id))))
                                   (unique (select (columns :email) (from :users)))))))))
    (%assert-contains sql "EXISTS (" "UNIQUE (")))

(deftest ansi-lateral-derived-table
  (let ((sql (%sql (select (columns :*)
                           (from :a)
                           (join (lateral (select (columns :x) (from :b)) :lb)
                                 (on (:= :a.id :lb.x)))))))
    (%assert-contains sql "LATERAL")))

;;; ---------------------------------------------------------------------------
;;; Joins
;;; ---------------------------------------------------------------------------

(deftest ansi-join-kinds
  (let ((on (on (:= :a.id :b.a-id))))
    (%assert-contains (%sql (select (columns :*) (from :a) (inner-join :b on)))
                      "INNER JOIN" "ON")
    (%assert-contains (%sql (select (columns :*) (from :a) (left-join :b on)))
                      "LEFT JOIN")
    (%assert-contains (%sql (select (columns :*) (from :a) (right-join :b on)))
                      "RIGHT JOIN")
    (%assert-contains (%sql (select (columns :*) (from :a) (full-join :b on)))
                      "FULL OUTER JOIN")
    (%assert-contains (%sql (select (columns :*) (from :a) (cross-join :b)))
                      "CROSS JOIN")))

(deftest ansi-natural-and-using
  (%assert-contains (%sql (select (columns :*) (from :a) (natural-join :b)))
                    "NATURAL" "INNER JOIN")
  (%assert-contains (%sql (select (columns :*) (from :a) (natural-left-join :b)))
                    "NATURAL" "LEFT JOIN")
  (%assert-contains (%sql (select (columns :*) (from :a) (join :b (using :id :tenant))))
                    "USING ("))

;;; ---------------------------------------------------------------------------
;;; Predicates & search conditions
;;; ---------------------------------------------------------------------------

(deftest ansi-boolean-and-comparisons
  (let* ((stmt (select (columns :id)
                       (from :t)
                       (where (sql-and
                               (:= :a 1)
                               (!= :b 2)
                               (:< :c 3)
                               (:> :d 4)
                               (:<= :e 5)
                               (:>= :f 6)
                               (sql-or (sql-not (:= :g 7)) (:= :h 8))))))
         (sql (%sql stmt)))
    (%assert-contains sql "AND" "OR" "NOT" "=" "<>" "<" ">" "<=" ">=")
    (ok (equal '(1 2 3 4 5 6 7 8) (%params stmt)))))

(deftest ansi-null-and-distinct-predicates
  (let ((sql (%sql (select (columns :id)
                           (from :t)
                           (where (sql-and
                                   (sql-is-null :a)
                                   (sql-is-not-null :b)
                                   (is-distinct-from :c :d)
                                   (is-not-distinct-from :e nil)))))))
    (%assert-contains sql "IS NULL" "IS NOT NULL"
                      "IS DISTINCT FROM" "IS NOT DISTINCT FROM" "NULL")))

(deftest ansi-between-in-like-similar
  (let* ((stmt (select (columns :id)
                       (from :t)
                       (where (sql-and
                               (sql-between :n 1 10)
                               (sql-between :n 1 10 :not t)
                               (sql-in :k 1 2 3)
                               (sql-like :name "a%")
                               (similar-to :code "[A-Z]+")))))
         (sql (%sql stmt)))
    (%assert-contains sql "BETWEEN" "NOT BETWEEN" "IN (" "LIKE" "SIMILAR TO")
    (ok (equal '(1 10 1 10 1 2 3 "a%" "[A-Z]+") (%params stmt)))))

(deftest ansi-quantified-comparisons
  (let* ((sub (select (columns :id) (from :u)))
         (sql (%sql (select (columns :id)
                            (from :t)
                            (where (sql-and
                                    (sql-any :id := sub)
                                    (sql-all :id :< sub)
                                    (sql-some :id :>= sub)))))))
    (%assert-contains sql "ANY" "ALL" "SOME")))

;;; ---------------------------------------------------------------------------
;;; Value expressions
;;; ---------------------------------------------------------------------------

(deftest ansi-case-cast-nullif-coalesce
  (let ((sql (%sql (select (columns
                            (sql-case (:= :a 1) "one" (:= :a 2) "two" :else "other")
                            (sql-cast :n :integer)
                            (nullif :x :y)
                            (coalesce :a :b 0))
                           (from :t)))))
    (%assert-contains sql "CASE" "WHEN" "THEN" "ELSE" "END"
                      "CAST(" "AS" "NULLIF(" "coalesce(")))

(deftest ansi-arithmetic-and-collate
  (let ((sql (%sql (select (columns (:+ :a (:* :b (:- :c (:/ :d 2))))
                                   (collate :name :unicode))
                           (from :t)))))
    (%assert-contains sql "+" "*" "-" "/" "COLLATE")))

(deftest ansi-window-over-frames
  (let ((sql (%sql (select (columns
                            (over (count :*)
                                  :partition-by (list :dept)
                                  :order-by (list '(:salary :desc))
                                  :frame (rows-frame :unbounded-preceding :current-row))
                            (over (sql-func :rank)
                                  :order-by (list :id)
                                  :frame (range-frame '(:preceding 1) '(:following 1))))
                           (from :emp)))))
    (%assert-contains sql "OVER (" "PARTITION BY" "ORDER BY"
                      "ROWS BETWEEN" "UNBOUNDED PRECEDING" "CURRENT ROW"
                      "RANGE BETWEEN")))

(deftest ansi-grouping-sets-rollup-cube
  (%assert-contains (%sql (select (columns :a) (from :t) (group-by (rollup :a :b))))
                    "ROLLUP")
  (%assert-contains (%sql (select (columns :a) (from :t) (group-by (cube :a :b))))
                    "CUBE")
  (%assert-contains (%sql (select (columns :a) (from :t)
                                  (group-by (grouping-sets '(:a) '(:b) '(:a :b)))))
                    "GROUPING SETS"))

(deftest ansi-bindparam-and-fragment-order
  "Corner: fragment ? slots interleave with typed binds in order."
  (let ((stmt (select (columns :id)
                      (from :t)
                      (where (sql-and
                              (:= :a (bindparam :x 1))
                              (sql-fragment "(b > ? AND c < ?)" 2 3)
                              (:= :d 4))))))
    (ok (equal '(1 2 3 4) (%params stmt)))))

;;; ---------------------------------------------------------------------------
;;; DML
;;; ---------------------------------------------------------------------------

(deftest ansi-insert-shapes
  (%assert-contains (%sql (insert-into :t (columns :a :b) (sql-values 1 2)))
                    "INSERT INTO" "VALUES")
  (%assert-contains (%sql (insert-into :t (default-values)))
                    "DEFAULT VALUES")
  (%assert-contains (%sql (insert-into :t (columns :id)
                                       (select (columns :id) (from :src))))
                    "INSERT INTO" "SELECT")
  (%assert-contains (%sql (insert-into :t (columns :a)
                                       (sql-values '(1) '(2))))
                    "VALUES"))

(deftest ansi-update-delete
  (%assert-contains (%sql (update :t (sql-set (:= :a 1) (:= :b 2)) (where (:= :id 9))))
                    "UPDATE" "SET" "WHERE")
  (%assert-contains (%sql (delete-from :t (where (:= :id 9))))
                    "DELETE FROM" "WHERE"))

(deftest ansi-merge
  (let ((sql (%sql (merge-into :tgt
                      :using (values-row '(1 "a") '(2 "b"))
                      :on (:= :tgt.id :src.id)
                      :when-matched (merge-update (list (:= :tgt.n "x")))
                      :when-not-matched (merge-insert '("y") :columns '(:n))))))
    (%assert-contains sql "MERGE INTO" "USING" "ON"
                      "WHEN MATCHED THEN" "UPDATE SET"
                      "WHEN NOT MATCHED THEN" "INSERT" "VALUES")))

(deftest ansi-truncate
  (%assert-contains (%sql (truncate-table '(:a :b) :identity :restart :cascade t))
                    "TRUNCATE TABLE" "RESTART IDENTITY" "CASCADE")
  (%assert-contains (%sql (truncate-table :t :identity :continue))
                    "CONTINUE IDENTITY"))

(deftest ansi-values-selectable
  (%assert-contains (%sql (select (columns :*)
                                  (from (values-row '(1) '(2)) "v")))
                    "VALUES" "AS"))

;;; ---------------------------------------------------------------------------
;;; DDL + constraints
;;; ---------------------------------------------------------------------------

(deftest ansi-create-table-types-and-identity
  (let ((sql (%sql (create-table :users
                      (column :id :type :integer :primary-key t :autoincrement t)
                      (column :name :type '(:varchar 64) :not-null t)
                      (column :bio :type :clob)
                      (column :blob :type :blob)
                      (column :ts :type :timestamptz)
                      (column :flag :type :boolean)))))
    (%assert-contains sql "CREATE TABLE"
                      "GENERATED BY DEFAULT AS IDENTITY"
                      "CHARACTER VARYING(64)"
                      "CHARACTER LARGE OBJECT"
                      "BINARY LARGE OBJECT"
                      "TIMESTAMP WITH TIME ZONE"
                      "BOOLEAN")))

(deftest ansi-table-constraints
  (let ((sql (%sql (create-table :orders
                      (column :id :type :integer)
                      (column :user-id :type :integer)
                      (column :amt :type :numeric)
                      (primary-key :id)
                      (unique-key :user-id :amt)
                      (foreign-key '(:user-id)
                                   :references '(:users :id)
                                   :on-delete :cascade
                                   :on-update :set-null
                                   :match :simple
                                   :name :fk-orders-user)
                      (check (:> :amt 0) :name :chk-amt)))))
    (%assert-contains sql
                      "PRIMARY KEY" "UNIQUE (" "FOREIGN KEY" "REFERENCES"
                      "ON DELETE CASCADE" "ON UPDATE SET NULL" "MATCH SIMPLE"
                      "CONSTRAINT" "CHECK (")))

(deftest ansi-alter-table-actions
  (let ((sql (%sql (alter-table :t
                     (add-column :x :type :integer)
                     (drop-column :y)
                     (add-constraint (unique-key :x))
                     (drop-constraint :uk)
                     (rename-column :a :b)
                     (rename-to :t2)))))
    (%assert-contains sql
                      "ALTER TABLE" "ADD COLUMN" "DROP COLUMN"
                      "ADD " "UNIQUE" "DROP CONSTRAINT"
                      "RENAME COLUMN" "TO" "RENAME TO")))

(deftest ansi-view-schema-sequence-index
  (%assert-contains (%sql (create-view :v (select (columns :id) (from :t))
                                       :columns '(:id)))
                    "CREATE VIEW" "AS")
  (%assert-contains (%sql (drop-view :v :if-exists t :cascade t))
                    "DROP VIEW" "IF EXISTS" "CASCADE")
  (%assert-contains (%sql (create-schema :s :authorization :dbo))
                    "CREATE SCHEMA" "AUTHORIZATION")
  (%assert-contains (%sql (drop-schema :s :if-exists t :cascade t))
                    "DROP SCHEMA")
  (%assert-contains (%sql (create-sequence :seq :start 1 :increment 1
                                           :minvalue 1 :maxvalue 100 :cycle t))
                    "CREATE SEQUENCE" "START WITH" "INCREMENT BY"
                    "MINVALUE" "MAXVALUE" "CYCLE")
  (%assert-contains (%sql (drop-sequence :seq :if-exists t))
                    "DROP SEQUENCE")
  (%assert-contains (%sql (create-index :ix (on :t) (columns :a :b) :unique))
                    "CREATE" "UNIQUE" "INDEX" "ON"))

;;; ---------------------------------------------------------------------------
;;; SQL/PSM
;;; ---------------------------------------------------------------------------

(deftest ansi-sql-psm-procedure-and-call
  (let* ((stmt (create-procedure :bump
                  (params (in :by :integer) (out :outv :integer))
                  (body (sql-fragment "SET outv = by"))))
         (sql (%sql stmt))
         (call (%sql (sql-call :bump 1))))
    (%assert-contains sql "CREATE PROCEDURE" "IN " "OUT " "BEGIN" "END")
    (%assert-contains call "CALL")
    (%assert-absent sql "LANGUAGE" "$$")))

;;; ---------------------------------------------------------------------------
;;; Negative — vendorisms must not silently emit as ANSI
;;; ---------------------------------------------------------------------------

(deftest ansi-rejects-returning
  (ok (signals (compile-sql (insert-into :t (columns :a) (sql-values 1) (returning :a))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

(deftest ansi-rejects-distinct-on
  (ok (signals (compile-sql (select (distinct :id) (columns :id) (from :t))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

(deftest ansi-rejects-for-update-nowait
  (ok (signals (compile-sql (select (columns :id) (from :t)
                                    (for-update :nowait t))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

(deftest ansi-rejects-for-update-skip-locked
  (ok (signals (compile-sql (select (columns :id) (from :t)
                                    (for-update :skip-locked t))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

;;; ---------------------------------------------------------------------------
;;; Identifier / quoting corner cases
;;; ---------------------------------------------------------------------------

(deftest ansi-qualified-names-and-quotes
  (let* ((stmt (select (columns :u.id :orders.user-id)
                       (from :user-data "u")
                       (where (:= :u.id 1))))
         (sql (%sql stmt)))
    (%assert-contains sql "\"user-data\"" ".")
    (ok (search "\"u\"" sql))
    (ok (equal '(1) (%params stmt)))))
