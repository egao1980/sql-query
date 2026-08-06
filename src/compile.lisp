(in-package #:sql-query)

(defun %default-dialect ()
  (or *sql-dialect* (make-sqlite3-dialect)))

(defun compile-sql (statement &key (dialect nil dialect-p))
  "Compile STATEMENT to SQL string + params list for DIALECT.
   Returns (values sql-string params-list)."
  (check-type statement sql-node)
  (let* ((d (if dialect-p dialect (%default-dialect)))
         (ctx (make-instance 'emit-context :dialect d))
         (sql (with-output-to-string (s)
                (emit-sql d statement s ctx))))
    (values sql (coerce (emit-context-params ctx) 'list))))
