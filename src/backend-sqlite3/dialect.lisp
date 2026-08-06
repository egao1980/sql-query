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

(defmethod emit-limit-offset ((dialect sqlite3-dialect) lim off stream ctx)
  (when lim
    (write-string " LIMIT " stream)
    (emit-sql dialect (lit (limit-count lim)) stream ctx))
  (when off
    (write-string " OFFSET " stream)
    (emit-sql dialect (lit (offset-count off)) stream ctx)))

(defmethod emit-returning ((dialect sqlite3-dialect) items stream ctx)
  (write-string " RETURNING " stream)
  (emit-column-list dialect items stream ctx))

(defmethod emit-for-update ((dialect sqlite3-dialect) clause stream ctx)
  ;; SQLite has limited locking; emit plain FOR UPDATE if requested
  (write-string " FOR UPDATE" stream)
  (when (for-update-of clause)
    (write-string " OF " stream)
    (emit-column-list dialect (for-update-of clause) stream ctx)))

(defmethod emit-distinct ((dialect sqlite3-dialect) clause stream ctx)
  (when (distinct-on clause)
    (error 'sql-dialect-unsupported
           :feature :distinct-on
           :dialect dialect
           :message "SQLite has no DISTINCT ON"))
  (write-string "DISTINCT " stream))

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

(defmethod initialize-instance :after ((dialect sqlite3-dialect) &key)
  (register-sqlite3-extensions dialect))

(defun use-sqlite3-dialect ()
  "Register :sqlite3 dialect and set *SQL-DIALECT*. Returns the dialect."
  (let ((d (make-sqlite3-dialect)))
    (setf *sql-dialect* d)
    (register-sql-dialect :sqlite3 d)
    d))

(use-sqlite3-dialect)
