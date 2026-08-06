(in-package #:sql-query)

(defclass sqlite3-dialect (sql-dialect) ())

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
       ((:integer :int :bigint) "INTEGER")
       ((:text :string) "TEXT")
       ((:boolean :bool) "INTEGER")
       ((:real :float :double) "REAL")
       ((:blob :bytea) "BLOB")
       ((:timestamp :timestamptz :date) "TEXT")
       (otherwise (call-next-method))))
    ((and (consp type-spec) (member (first type-spec) '(:varchar :char)))
     "TEXT")
    (t (call-next-method))))

(defmethod dialect-autoincrement-pk ((dialect sqlite3-dialect))
  "INTEGER PRIMARY KEY AUTOINCREMENT")

(defmethod dialect-autoincrement-suffix ((dialect sqlite3-dialect))
  " AUTOINCREMENT")
