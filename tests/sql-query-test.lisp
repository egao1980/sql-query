(in-package #:sql-query/tests)

(defun %compile (stmt &optional (dialect (make-sqlite3-dialect)))
  (multiple-value-list (compile-sql stmt :dialect dialect)))

(defun %sql (stmt &optional (dialect (make-sqlite3-dialect)))
  (first (%compile stmt dialect)))

(defun %params (stmt &optional (dialect (make-sqlite3-dialect)))
  (second (%compile stmt dialect)))

(deftest select-basic
  (let ((stmt (select (columns :id :name)
                      (from :users)
                      (where (:= :active 1))
                      (order-by :name)
                      (limit 10))))
    (ok (search "SELECT" (%sql stmt)))
    (ok (search "FROM" (%sql stmt)))
    (ok (search "WHERE" (%sql stmt)))
    (ok (search "ORDER BY" (%sql stmt)))
    (ok (search "LIMIT" (%sql stmt)))
    (ok (equal '(1 10) (%params stmt)))))

(deftest select-join-group
  (let ((stmt (select (columns :users.id (sql-fragment "count(*) AS c"))
                      (from :users)
                      (left-join :orders (on (:= :orders.user-id :users.id)))
                      (group-by :users.id)
                      (having (:> (sql-fragment "count(*)") 1)))))
    (ok (search "LEFT JOIN" (%sql stmt)))
    (ok (search "GROUP BY" (%sql stmt)))
    (ok (search "HAVING" (%sql stmt)))
    (ok (equal '(1) (%params stmt)))))

(deftest insert-update-delete
  (let ((ins (insert-into :users
               (columns :name :email)
               (sql-values "ada" "ada@example.com")))
        (upd (update :users
               (sql-set (:= :name "grace"))
               (where (:= :id 1))))
        (del (delete-from :users (where (:= :id 1)))))
    (ok (search "INSERT INTO" (%sql ins)))
    (ok (equal '("ada" "ada@example.com") (%params ins)))
    (ok (search "UPDATE" (%sql upd)))
    (ok (equal '("grace" 1) (%params upd)))
    (ok (search "DELETE FROM" (%sql del)))
    (ok (equal '(1) (%params del)))))

(deftest ddl-sqlite
  (let* ((d (make-sqlite3-dialect))
         (ct (create-table :users
                (column :id :type :integer :primary-key t :autoincrement t)
                (column :name :type '(:varchar 255) :not-null t)
                (column :email :type :text :unique t)))
         (ci (create-index :users-email (on :users) (columns :email)))
         (at (alter-table :users (add-column :bio :type :text)))
         (dt (drop-table :users :if-exists t)))
    (ok (search "CREATE TABLE" (%sql ct d)))
    (ok (search "AUTOINCREMENT" (%sql ct d)))
    (ok (search "CREATE INDEX" (%sql ci d)))
    (ok (search "ALTER TABLE" (%sql at d)))
    (ok (search "DROP TABLE IF EXISTS" (%sql dt d)))))

(deftest ddl-postgres-types
  (let* ((d (make-postgres-dialect))
         (ct (create-table :users
                (column :id :type :integer :primary-key t :autoincrement t)
                (column :email :type '(:varchar 255) :unique t)))
         (sql (%sql ct d)))
    (ok (search "SERIAL PRIMARY KEY" sql))
    (ok (search "VARCHAR(255)" sql))))

(deftest postgres-params-dollar
  (let* ((d (make-postgres-dialect))
         (stmt (select (columns :id) (from :t) (where (:= :id 42))))
         (sql (%sql stmt d))
         (params (%params stmt d)))
    (ok (search "$1" sql))
    (ng (search "?" sql))
    (ok (equal '(42) params))))

(deftest sql-fragment-nesting
  (let ((stmt (select (columns (sql-fragment "count(*) AS c"))
                      (from :users)
                      (where (sql-and
                              (:= :tenant-id 7)
                              (sql-fragment "created_at > ?" "2020-01-01"))))))
    (ok (search "count(*) AS c" (%sql stmt)))
    (ok (equal '(7 "2020-01-01") (%params stmt)))))

(deftest parse-expr-list-form
  (let ((stmt (select (columns :id)
                      (from :t)
                      (where (parse-expr `(:and (:= :a 1) (:= :b 2)))))))
    (ok (search "AND" (%sql stmt)))
    (ok (equal '(1 2) (%params stmt)))))

(deftest compose-and-where
  (let* ((base (select (columns :id) (from :t) (where (:= :a 1))))
         (more (and-where base (:= :b 2)))
         (merged (merge-query base (limit 5))))
    (ok (equal '(1 2) (%params more)))
    (ok (search "LIMIT" (%sql merged)))
    (ok (equal '(1 5) (%params merged)))
    ;; original unchanged
    (ok (equal '(1) (%params base)))))

(deftest procedure-sqlite-unsupported
  (let ((stmt (create-procedure :bump
                 (params (in :by :integer))
                 (body (sql-fragment "UPDATE counters SET n = n + ?" 1)))))
    (ok (signals (compile-sql stmt :dialect (make-sqlite3-dialect))
                 'sql-dialect-unsupported))))

(deftest procedure-postgres
  (let* ((d (make-postgres-dialect))
         (stmt (create-procedure :bump
                  (params (in :by :integer))
                  (body (sql-fragment "UPDATE counters SET n = n + ?" 1))))
         (sql (%sql stmt d)))
    (ok (search "CREATE PROCEDURE" sql))
    (ok (search "LANGUAGE plpgsql" sql))
    (ok (equal '(1) (%params stmt d)))
    (ok (search "CALL" (%sql (sql-call :bump 1) d)))))

(deftest execute-roundtrip-sqlite
  (sql-protocol:with-connection (c :driver :sqlite3 :database-name ":memory:")
    (execute-query c (create-table :users
                        (column :id :type :integer :primary-key t :autoincrement t)
                        (column :name :type :text :not-null t)))
    (execute-query c (insert-into :users (columns :name) (sql-values "ada")))
    (execute-query c (insert-into :users (columns :name) (sql-values "grace")))
    (let ((rows (fetch-all-query
                 c (select (columns :name)
                           (from :users)
                           (where (sql-like :name "%a%"))
                           (order-by :name)))))
      (ok (equal 2 (length rows)))
      (ok (equal "ada" (getf (first rows) :name))))))
