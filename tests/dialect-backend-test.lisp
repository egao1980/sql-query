(in-package #:sql-query/tests)

(deftest ddl-sqlite-backend
  (let* ((d (sql-query-sqlite3:make-sqlite3-dialect))
         (ct (create-table :users
                (column :id :type :integer :primary-key t :autoincrement t)
                (column :name :type '(:varchar 255) :not-null t)))
         (sql (nth-value 0 (compile-sql ct :dialect d))))
    (ok (search "AUTOINCREMENT" sql))
    (ok (search "TEXT" sql))))

(deftest ddl-postgres-backend
  (let* ((d (sql-query-postgres:make-postgres-dialect))
         (ct (create-table :users
                (column :id :type :integer :primary-key t :autoincrement t)
                (column :email :type '(:varchar 255) :unique t)))
         (sql (nth-value 0 (compile-sql ct :dialect d))))
    (ok (search "SERIAL PRIMARY KEY" sql))
    (ok (search "VARCHAR(255)" sql))))

(deftest postgres-params-dollar
  (let* ((d (sql-query-postgres:make-postgres-dialect))
         (stmt (select (columns :id) (from :t)
                       (where (:= :id (bindparam :id 42)))))
         (sql (nth-value 0 (compile-sql stmt :dialect d)))
         (params (nth-value 1 (compile-sql stmt :dialect d))))
    (ok (search "$1" sql))
    (ng (search "?" sql))
    (ok (equal '(42) params)))
  (let* ((d (sql-query-postgres:make-postgres-dialect))
         (sql (nth-value 0 (compile-sql
                            (select (columns :id) (from :t) (where (:= :id 42)))
                            :dialect d))))
    (ok (search "= 42" sql) "bare literal inlined")
    (ng (search "$" sql))))

(deftest procedure-sqlite-unsupported
  (let ((stmt (create-procedure :bump
                 (params (in :by :integer))
                 (body (sql-fragment "UPDATE counters SET n = n + ?" 1)))))
    (ok (signals (compile-sql stmt :dialect (sql-query-sqlite3:make-sqlite3-dialect))
                 'sql-dialect-unsupported))))

(deftest procedure-postgres
  (let* ((d (sql-query-postgres:make-postgres-dialect))
         (stmt (create-procedure :bump
                  (params (in :by :integer))
                  (body (sql-fragment "UPDATE counters SET n = n + ?" 1))))
         (sql (nth-value 0 (compile-sql stmt :dialect d))))
    (ok (search "CREATE PROCEDURE" sql))
    (ok (search "LANGUAGE plpgsql" sql))
    (ok (equal '(1) (nth-value 1 (compile-sql stmt :dialect d))))
    (ok (search "CALL"
                (nth-value 0 (compile-sql (sql-call :bump 1) :dialect d))))))

(deftest execute-roundtrip-sqlite
  (sql-protocol:with-connection (c :driver :sqlite3 :database-name ":memory:")
    (execute-query c (create-table :users
                        (column :id :type :integer :primary-key t :autoincrement t)
                        (column :name :type :text :not-null t))
                   :dialect (sql-query-sqlite3:make-sqlite3-dialect))
    (execute-query c (insert-into :users (columns :name) (sql-values "ada"))
                   :dialect (sql-query-sqlite3:make-sqlite3-dialect))
    (execute-query c (insert-into :users (columns :name) (sql-values "grace"))
                   :dialect (sql-query-sqlite3:make-sqlite3-dialect))
    (let ((rows (fetch-all-query
                 c (select (columns :name)
                           (from :users)
                           (where (sql-like :name "%a%"))
                           (order-by :name))
                 :dialect (sql-query-sqlite3:make-sqlite3-dialect))))
      (ok (equal 2 (length rows)))
      (ok (equal "ada" (getf (first rows) :name))))))

(deftest dialect-registry
  (ok (typep (gethash :sqlite3 sql-query:*sql-dialect-registry*)
             'sql-query-sqlite3:sqlite3-dialect))
  (ok (typep (gethash :postgres sql-query:*sql-dialect-registry*)
             'sql-query-postgres:postgres-dialect))
  (ok (typep (gethash :ansi sql-query:*sql-dialect-registry*)
             'sql-query:ansi-dialect)))
