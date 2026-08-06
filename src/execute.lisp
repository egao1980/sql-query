(in-package #:sql-query)

(defmethod dialect-for-connection ((connection sql-protocol:sql-connection))
  (or *sql-dialect*
      (let* ((raw (sql-protocol:raw-connection connection))
             (driver (and raw
                          (ignore-errors
                            (funcall (find-symbol "CONNECTION-DRIVER-TYPE" :dbi) raw)))))
        (or (and driver (gethash driver *sql-dialect-registry*))
            (gethash :ansi *sql-dialect-registry*)
            (make-ansi-dialect)))))

(defun execute-query (connection statement &key dialect params)
  "Compile STATEMENT and execute on CONNECTION.
PARAMS when provided replaces the compiled bind list (which already includes
bindparam :default values). Returns sql-protocol result."
  (let ((d (or dialect (dialect-for-connection connection))))
    (multiple-value-bind (sql compiled-params)
        (compile-sql statement :dialect d)
      (sql-protocol:execute connection sql (or params compiled-params)))))

(defun fetch-query (connection statement &key dialect params)
  "Execute STATEMENT and fetch one row (plist) or NIL."
  (sql-protocol:fetch
   (execute-query connection statement :dialect dialect :params params)))

(defun fetch-all-query (connection statement &key dialect params)
  "Execute STATEMENT and fetch all rows."
  (sql-protocol:fetch-all
   (execute-query connection statement :dialect dialect :params params)))
