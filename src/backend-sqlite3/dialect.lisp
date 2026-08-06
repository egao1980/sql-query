(in-package #:sql-query-sqlite3)

(defclass sqlite3-dialect (ansi-dialect) ())

(defun make-sqlite3-dialect ()
  (make-instance 'sqlite3-dialect))

(defmethod dialect-param-style ((dialect sqlite3-dialect)) :question)

(defmethod dialect-boolean ((dialect sqlite3-dialect) value)
  (if value "1" "0"))

(defmethod dialect-type-sql ((dialect sqlite3-dialect) type-spec)
  (cond
    ((null type-spec) "TEXT")
    ((stringp type-spec) type-spec)
    ((keywordp type-spec)
     (case type-spec
       ((:integer :int :bigint :smallint) "INTEGER")
       ((:text :string) "TEXT")
       ((:boolean :bool) "INTEGER")
       ((:real :float :double) "REAL")
       ((:blob :bytea :binary) "BLOB")
       ((:timestamp :timestamptz :date :time) "TEXT")
       ((:numeric :decimal) "NUMERIC")
       (otherwise (call-next-method))))
    ((and (consp type-spec) (member (first type-spec) '(:varchar :char)))
     "TEXT")
    (t (call-next-method))))

(defmethod dialect-autoincrement-pk ((dialect sqlite3-dialect))
  "INTEGER PRIMARY KEY AUTOINCREMENT")

(defmethod dialect-autoincrement-suffix ((dialect sqlite3-dialect))
  " AUTOINCREMENT")

(defmethod emit-create-procedure ((dialect sqlite3-dialect) stmt stream ctx)
  (declare (ignore stmt stream ctx))
  (error 'sql-dialect-unsupported
         :feature :create-procedure
         :dialect dialect
         :message "SQLite has no CREATE PROCEDURE"))

(defmethod emit-call ((dialect sqlite3-dialect) stmt stream ctx)
  (declare (ignore stmt stream ctx))
  (error 'sql-dialect-unsupported
         :feature :call
         :dialect dialect
         :message "SQLite has no CALL"))

(defun use-sqlite3-dialect ()
  "Register :sqlite3 dialect and set *SQL-DIALECT*. Returns the dialect."
  (let ((d (make-sqlite3-dialect)))
    (setf *sql-dialect* d)
    (register-sql-dialect :sqlite3 d)
    d))

(use-sqlite3-dialect)
