(in-package #:sql-query)

(defmethod dialect-for-connection ((connection sql-protocol:sql-connection))
  (or *sql-dialect*
      (let* ((raw (sql-protocol:raw-connection connection))
             (driver (and raw
                          (ignore-errors
                            (funcall (find-symbol "CONNECTION-DRIVER-TYPE" :dbi) raw)))))
        (case driver
          (:postgres (make-postgres-dialect))
          (:sqlite3 (make-sqlite3-dialect))
          (otherwise
           (or (when *sql-dialect* *sql-dialect*)
               (make-sqlite3-dialect)))))))

(defun execute-query (connection statement &key dialect)
  "Compile STATEMENT and execute on CONNECTION. Returns sql-protocol result."
  (let ((d (or dialect (dialect-for-connection connection))))
    (multiple-value-bind (sql params)
        (compile-sql statement :dialect d)
      (sql-protocol:execute connection sql params))))

(defun fetch-query (connection statement &key dialect)
  "Execute STATEMENT and fetch one row (plist) or NIL."
  (sql-protocol:fetch (execute-query connection statement :dialect dialect)))

(defun fetch-all-query (connection statement &key dialect)
  "Execute STATEMENT and fetch all rows."
  (sql-protocol:fetch-all (execute-query connection statement :dialect dialect)))
