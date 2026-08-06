(in-package #:sql-query)

(defclass sql-dialect () ()
  (:documentation "Dialect protocol. Builtin concrete class = ANSI; vendors in backend systems."))

(defclass emit-context ()
  ((params :initform (make-array 0 :adjustable t :fill-pointer 0)
           :accessor emit-context-params)
   (dialect :initarg :dialect :reader emit-context-dialect)))

(defvar *sql-dialect* nil
  "Default dialect for COMPILE-SQL when :dialect is omitted (ANSI after load).")

(defvar *sql-dialect-registry* (make-hash-table :test #'eq)
  "Map of driver keyword (:sqlite3, :postgres, :ansi, …) → sql-dialect instance.")

(defun register-sql-dialect (driver dialect)
  "Register DIALECT for DRIVER keyword (used by DIALECT-FOR-CONNECTION)."
  (check-type driver keyword)
  (check-type dialect sql-dialect)
  (setf (gethash driver *sql-dialect-registry*) dialect)
  dialect)

(defgeneric dialect-param-style (dialect)
  (:documentation "Return :question (?) or :dollar ($1).")
  (:method ((dialect sql-dialect)) :question))

(defgeneric dialect-quote-char (dialect)
  (:method ((dialect sql-dialect)) #\"))

(defgeneric dialect-boolean (dialect value)
  (:method ((dialect sql-dialect) value)
    (if value "TRUE" "FALSE")))

(defgeneric dialect-type-sql (dialect type-spec)
  (:documentation "Render a column type spec to SQL text (ANSI defaults).")
  (:method ((dialect sql-dialect) type-spec)
    (cond
      ((null type-spec) "CHARACTER VARYING")
      ((stringp type-spec) type-spec)
      ((keywordp type-spec)
       (case type-spec
         ((:integer :int) "INTEGER")
         ((:bigint) "BIGINT")
         ((:smallint) "SMALLINT")
         ((:text :string) "CHARACTER VARYING")
         ((:boolean :bool) "BOOLEAN")
         ((:real :float) "REAL")
         ((:double) "DOUBLE PRECISION")
         ((:blob :bytea :binary) "BINARY LARGE OBJECT")
         ((:timestamp) "TIMESTAMP")
         ((:timestamptz) "TIMESTAMP WITH TIME ZONE")
         ((:date) "DATE")
         ((:time) "TIME")
         ((:numeric :decimal) "NUMERIC")
         (otherwise (string-upcase (symbol-name type-spec)))))
      ((and (consp type-spec) (eq (first type-spec) :varchar))
       (format nil "CHARACTER VARYING(~a)" (second type-spec)))
      ((and (consp type-spec) (eq (first type-spec) :char))
       (format nil "CHARACTER(~a)" (second type-spec)))
      ((and (consp type-spec) (member (first type-spec) '(:numeric :decimal)))
       (format nil "NUMERIC(~{~a~^,~})" (rest type-spec)))
      (t (%err "unsupported type spec ~s" type-spec)))))

(defgeneric dialect-for-connection (connection)
  (:documentation "Infer sql-dialect from a sql-protocol connection/backend.")
  (:method (connection)
    (declare (ignore connection))
    (or *sql-dialect*
        (gethash :ansi *sql-dialect-registry*)
        (make-ansi-dialect))))

(defun ident-string (name)
  (ctypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(defun emit-ident (dialect name stream)
  (let* ((q (dialect-quote-char dialect))
         (s (ident-string name)))
    (write-char q stream)
    (loop for c across s
          do (if (char= c q)
                 (progn (write-char q stream) (write-char q stream))
                 (write-char c stream)))
    (write-char q stream)))

(defun emit-placeholder (dialect stream ctx)
  (ecase (dialect-param-style dialect)
    (:question (write-char #\? stream))
    (:dollar
     (format stream "$~d" (1+ (length (emit-context-params ctx)))))))

(defun push-param (ctx value)
  (vector-push-extend value (emit-context-params ctx)))

(defgeneric emit-sql (dialect node stream ctx)
  (:documentation "Write SQL for NODE to STREAM; accumulate params in CTX."))

(defmethod emit-sql (dialect (node sql-fragment) stream ctx)
  (let* ((template (sql-fragment-template node))
         (args (sql-fragment-args node))
         (parts (uiop:split-string template :separator "?")))
    (unless (= (1- (length parts)) (length args))
      (%err "sql-fragment ? count (~a) != args (~a) in ~s"
            (1- (length parts)) (length args) template))
    (write-string (first parts) stream)
    (mapc (lambda (arg part)
            (emit-placeholder dialect stream ctx)
            (push-param ctx arg)
            (write-string part stream))
          args
          (rest parts))))

(defmethod emit-sql (dialect (node raw-sql) stream ctx)
  (declare (ignore dialect ctx))
  (write-string (raw-sql-text node) stream))

(defmethod emit-sql (dialect (node column-ref) stream ctx)
  (declare (ignore ctx))
  (when (column-ref-table node)
    (emit-ident dialect (column-ref-table node) stream)
    (write-char #\. stream))
  (emit-ident dialect (column-ref-name node) stream))

(defmethod emit-sql (dialect (node literal) stream ctx)
  (declare (ignore dialect))
  (let ((v (literal-value node)))
    (cond
      ((null v) (write-string "NULL" stream))
      (t
       (emit-placeholder (emit-context-dialect ctx) stream ctx)
       (push-param ctx v)))))

(defun %emit-op (dialect op stream)
  (declare (ignore dialect))
  (write-string
   (ecase op
     (:= "=") (:!= "<>") (:< "<") (:> ">") (:<= "<=") (:>= ">=")
     (:+ "+") (:- "-") (:* "*") (:/ "/")
     (:and "AND") (:or "OR") (:not "NOT"))
   stream))

(defmethod emit-sql (dialect (node between-op) stream ctx)
  (emit-sql dialect (between-operand node) stream ctx)
  (write-string (if (between-not-p node) " NOT BETWEEN " " BETWEEN ") stream)
  (emit-sql dialect (between-low node) stream ctx)
  (write-string " AND " stream)
  (emit-sql dialect (between-high node) stream ctx))

(defmethod emit-sql (dialect (node case-expr) stream ctx)
  (write-string "CASE" stream)
  (dolist (w (case-whens node))
    (write-string " WHEN " stream)
    (emit-sql dialect (car w) stream ctx)
    (write-string " THEN " stream)
    (emit-sql dialect (cdr w) stream ctx))
  (when (case-else node)
    (write-string " ELSE " stream)
    (emit-sql dialect (case-else node) stream ctx))
  (write-string " END" stream))

(defmethod emit-sql (dialect (node cast-expr) stream ctx)
  (write-string "CAST(" stream)
  (emit-sql dialect (cast-expr-of node) stream ctx)
  (write-string " AS " stream)
  (write-string (dialect-type-sql dialect (cast-type node)) stream)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node function-call) stream ctx)
  (write-string (ident-string (function-call-name node)) stream)
  (write-char #\( stream)
  (loop for (a . rest) on (function-call-args node)
        do (emit-sql dialect a stream ctx)
           (when rest (write-string ", " stream)))
  (write-char #\) stream))

(defmethod emit-sql (dialect (node exists-op) stream ctx)
  (write-string "EXISTS (" stream)
  (emit-sql dialect (exists-query node) stream ctx)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node labeled-expr) stream ctx)
  (emit-sql dialect (labeled-expr-of node) stream ctx)
  (write-string " AS " stream)
  (emit-ident dialect (labeled-name node) stream))

(defmethod emit-sql (dialect (node bind-param) stream ctx)
  (emit-placeholder dialect stream ctx)
  (push-param ctx (if (bind-param-has-value node)
                      (bind-param-value node)
                      (bind-param-name node))))

(defun emit-from-item (dialect item stream ctx &optional alias)
  (cond
    ((typep item 'subquery)
     (write-char #\( stream)
     (emit-sql dialect (subquery-query item) stream ctx)
     (write-char #\) stream)
     (let ((a (or alias (subquery-alias item))))
       (when a
         (write-string " AS " stream)
         (emit-ident dialect a stream))))
    ((typep item 'cte-node)
     (emit-ident dialect (cte-name item) stream)
     (when alias
       (write-string " AS " stream)
       (emit-ident dialect alias stream)))
    ((typep item 'sql-table)
     (emit-ident dialect (sql-table-name item) stream)
     (when alias
       (write-string " AS " stream)
       (emit-ident dialect alias stream)))
    (t
     (emit-ident dialect item stream)
     (when alias
       (write-string " AS " stream)
       (emit-ident dialect alias stream)))))

(defmethod emit-sql (dialect (node subquery) stream ctx)
  (emit-from-item dialect node stream ctx))

(defmethod emit-sql (dialect (node binary-op) stream ctx)
  (write-char #\( stream)
  (emit-sql dialect (binary-op-left node) stream ctx)
  (write-char #\Space stream)
  (%emit-op dialect (binary-op-op node) stream)
  (write-char #\Space stream)
  (emit-sql dialect (binary-op-right node) stream ctx)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node unary-op) stream ctx)
  (write-char #\( stream)
  (%emit-op dialect (unary-op-op node) stream)
  (write-char #\Space stream)
  (emit-sql dialect (unary-op-operand node) stream ctx)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node nary-op) stream ctx)
  (write-char #\( stream)
  (loop for (op . rest) on (nary-op-operands node)
        for first = t then nil
        do (unless first
             (write-char #\Space stream)
             (%emit-op dialect (nary-op-op node) stream)
             (write-char #\Space stream))
        do (emit-sql dialect op stream ctx))
  (write-char #\) stream))

(defmethod emit-sql (dialect (node in-op) stream ctx)
  (emit-sql dialect (in-op-left node) stream ctx)
  (write-string (if (in-op-not-p node) " NOT IN (" " IN (") stream)
  (loop for (v . rest) on (in-op-values node)
        do (emit-sql dialect v stream ctx)
           (when rest (write-string ", " stream)))
  (write-char #\) stream))

(defmethod emit-sql (dialect (node like-op) stream ctx)
  (emit-sql dialect (like-op-left node) stream ctx)
  (write-string (if (like-op-not-p node) " NOT LIKE " " LIKE ") stream)
  (emit-sql dialect (like-op-pattern node) stream ctx))

(defmethod emit-sql (dialect (node is-null-op) stream ctx)
  (emit-sql dialect (is-null-op-operand node) stream ctx)
  (write-string (if (is-null-op-not-p node) " IS NOT NULL" " IS NULL") stream))

;;; ---- clause helpers ----

(defun %clause (stmt type)
  (%find-clause (statement-clauses stmt) type))

(defun %clauses (stmt type)
  (remove-if-not (lambda (c) (typep c type)) (statement-clauses stmt)))

(defun emit-column-list (dialect items stream ctx)
  (if items
      (loop for (item . rest) on items
            do (emit-sql dialect item stream ctx)
               (when rest (write-string ", " stream)))
      (write-char #\* stream)))

(defun emit-with-cte (dialect stmt stream ctx)
  (let ((w (%clause stmt 'with-cte-clause)))
    (when w
      (write-string "WITH " stream)
      (when (some #'cte-recursive (with-cte-ctes w))
        (write-string "RECURSIVE " stream))
      (loop for (c . rest) on (with-cte-ctes w)
            do (emit-ident dialect (cte-name c) stream)
               (write-string " AS (" stream)
               (emit-sql dialect (cte-query c) stream ctx)
               (write-char #\) stream)
               (when rest (write-string ", " stream)))
      (write-char #\Space stream))))

(defmethod emit-sql (dialect (stmt select-statement) stream ctx)
  (emit-with-cte dialect stmt stream ctx)
  (write-string "SELECT " stream)
  (let ((d (%clause stmt 'distinct-clause)))
    (when d
      (write-string "DISTINCT " stream)
      (when (distinct-on d)
        (write-string "ON (" stream)
        (emit-column-list dialect (distinct-on d) stream ctx)
        (write-string ") " stream))))
  (let ((cols (%clause stmt 'columns-clause)))
    (emit-column-list dialect (and cols (columns-items cols)) stream ctx))
  (let ((f (%clause stmt 'from-clause)))
    (when f
      (write-string " FROM " stream)
      (emit-from-item dialect (from-table f) stream ctx (from-alias f))))
  (dolist (j (%clauses stmt 'join-clause))
    (write-char #\Space stream)
    (write-string (ecase (join-type j)
                    (:inner "INNER JOIN")
                    (:left "LEFT JOIN")
                    (:right "RIGHT JOIN")
                    (:full "FULL OUTER JOIN")
                    (:cross "CROSS JOIN"))
                  stream)
    (write-char #\Space stream)
    (emit-from-item dialect (join-table j) stream ctx (join-alias j))
    (when (and (join-on j) (not (eq (join-type j) :cross)))
      (write-string " ON " stream)
      (emit-sql dialect (join-on j) stream ctx)))
  (let ((w (%clause stmt 'where-clause)))
    (when w
      (write-string " WHERE " stream)
      (emit-sql dialect (where-expr w) stream ctx)))
  (let ((g (%clause stmt 'group-by-clause)))
    (when g
      (write-string " GROUP BY " stream)
      (emit-column-list dialect (group-by-items g) stream ctx)))
  (let ((h (%clause stmt 'having-clause)))
    (when h
      (write-string " HAVING " stream)
      (emit-sql dialect (having-expr h) stream ctx)))
  (let ((o (%clause stmt 'order-by-clause)))
    (when o
      (write-string " ORDER BY " stream)
      (loop for ((expr dir) . rest) on (order-by-items o)
            do (emit-sql dialect expr stream ctx)
               (when (eq dir :desc) (write-string " DESC" stream))
               (when rest (write-string ", " stream)))))
  (let ((lim (%clause stmt 'limit-clause)))
    (when lim
      (write-string " LIMIT " stream)
      (emit-sql dialect (lit (limit-count lim)) stream ctx)))
  (let ((off (%clause stmt 'offset-clause)))
    (when off
      (write-string " OFFSET " stream)
      (emit-sql dialect (lit (offset-count off)) stream ctx)))
  (let ((fu (%clause stmt 'for-update-clause)))
    (when fu
      (write-string " FOR UPDATE" stream)
      (when (for-update-of fu)
        (write-string " OF " stream)
        (emit-column-list dialect (for-update-of fu) stream ctx))
      (when (for-update-nowait fu) (write-string " NOWAIT" stream))
      (when (for-update-skip-locked fu) (write-string " SKIP LOCKED" stream)))))

(defmethod emit-sql (dialect (stmt compound-select-statement) stream ctx)
  (let ((op-sql (ecase (compound-op stmt)
                  (:union "UNION")
                  (:union-all "UNION ALL")
                  (:intersect "INTERSECT")
                  (:intersect-all "INTERSECT ALL")
                  (:except "EXCEPT")
                  (:except-all "EXCEPT ALL"))))
    (loop for (s . rest) on (compound-selects stmt)
          do (write-char #\( stream)
             (emit-sql dialect s stream ctx)
             (write-char #\) stream)
             (when rest
               (write-char #\Space stream)
               (write-string op-sql stream)
               (write-char #\Space stream)))))

(defmethod emit-sql (dialect (stmt insert-statement) stream ctx)
  (write-string "INSERT INTO " stream)
  (emit-ident dialect (insert-table stmt) stream)
  (let ((cols (%clause stmt 'columns-clause)))
    (when cols
      (write-string " (" stream)
      (emit-column-list dialect (columns-items cols) stream ctx)
      (write-char #\) stream)))
  (let ((vals (%clause stmt 'values-clause))
        (src (%clause stmt 'select-source-clause)))
    (cond
      (vals
       (write-string " VALUES " stream)
       (loop for (row . rest) on (values-rows vals)
             do (write-char #\( stream)
                (loop for (cell . r2) on row
                      do (emit-sql dialect cell stream ctx)
                         (when r2 (write-string ", " stream)))
                (write-char #\) stream)
                (when rest (write-string ", " stream))))
      (src
       (write-char #\Space stream)
       (emit-sql dialect (select-source-select src) stream ctx))
      (t (%err "insert-into requires sql-values or select"))))
  (let ((ret (%clause stmt 'returning-clause)))
    (when ret
      (write-string " RETURNING " stream)
      (emit-column-list dialect (returning-items ret) stream ctx))))

(defmethod emit-sql (dialect (stmt update-statement) stream ctx)
  (write-string "UPDATE " stream)
  (emit-ident dialect (update-table stmt) stream)
  (let ((s (%clause stmt 'set-clause)))
    (unless s (%err "update requires sql-set"))
    (write-string " SET " stream)
    (loop for (a . rest) on (set-assignments s)
          do (emit-sql dialect (binary-op-left a) stream ctx)
             (write-string " = " stream)
             (emit-sql dialect (binary-op-right a) stream ctx)
             (when rest (write-string ", " stream))))
  (let ((w (%clause stmt 'where-clause)))
    (when w
      (write-string " WHERE " stream)
      (emit-sql dialect (where-expr w) stream ctx)))
  (let ((ret (%clause stmt 'returning-clause)))
    (when ret
      (write-string " RETURNING " stream)
      (emit-column-list dialect (returning-items ret) stream ctx))))

(defmethod emit-sql (dialect (stmt delete-statement) stream ctx)
  (write-string "DELETE FROM " stream)
  (emit-ident dialect (delete-table stmt) stream)
  (let ((w (%clause stmt 'where-clause)))
    (when w
      (write-string " WHERE " stream)
      (emit-sql dialect (where-expr w) stream ctx))))

(defgeneric emit-column-def (dialect col stream ctx)
  (:method ((dialect sql-dialect) col stream ctx)
    (emit-ident dialect (column-def-name col) stream)
    (write-char #\Space stream)
    (cond
      ((and (column-def-autoincrement col) (column-def-primary-key col))
       (write-string (dialect-autoincrement-pk dialect) stream))
      (t
       (write-string (dialect-type-sql dialect (column-def-type col)) stream)
       (when (column-def-primary-key col) (write-string " PRIMARY KEY" stream))
       (when (column-def-autoincrement col)
         (write-string (dialect-autoincrement-suffix dialect) stream))))
    (when (column-def-not-null col) (write-string " NOT NULL" stream))
    (when (column-def-unique col) (write-string " UNIQUE" stream))
    (when (column-def-default col)
      (write-string " DEFAULT " stream)
      (let ((d (column-def-default col)))
        (emit-sql dialect (if (typep d 'sql-node) d (lit d)) stream ctx)))))

(defgeneric dialect-autoincrement-pk (dialect)
  (:method ((dialect sql-dialect))
    "INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY"))

(defgeneric dialect-autoincrement-suffix (dialect)
  (:method ((dialect sql-dialect))
    " GENERATED BY DEFAULT AS IDENTITY"))

(defmethod emit-sql (dialect (stmt create-table-statement) stream ctx)
  (write-string "CREATE TABLE " stream)
  (when (create-table-if-not-exists stmt)
    (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-table-table stmt) stream)
  (write-string " (" stream)
  (loop for (col . rest) on (create-table-columns stmt)
        do (emit-column-def dialect col stream ctx)
           (when rest (write-string ", " stream)))
  (write-char #\) stream))

(defmethod emit-sql (dialect (stmt drop-table-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP TABLE " stream)
  (when (drop-table-if-exists stmt)
    (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-table-table stmt) stream))

(defmethod emit-sql (dialect (stmt alter-table-statement) stream ctx)
  (loop for (action . rest) on (alter-table-actions stmt)
        do (write-string "ALTER TABLE " stream)
           (emit-ident dialect (alter-table-table stmt) stream)
           (write-char #\Space stream)
           (typecase action
             (add-column-clause
              (write-string "ADD COLUMN " stream)
              (emit-column-def dialect (add-column-column action) stream ctx))
             (drop-column-clause
              (write-string "DROP COLUMN " stream)
              (emit-ident dialect (drop-column-name action) stream))
             (t (%err "unsupported alter-table action ~s" action)))
           (when rest (write-string "; " stream))))

(defmethod emit-sql (dialect (stmt create-index-statement) stream ctx)
  (write-string "CREATE " stream)
  (when (create-index-unique stmt) (write-string "UNIQUE " stream))
  (write-string "INDEX " stream)
  (when (create-index-if-not-exists stmt)
    (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-index-name stmt) stream)
  (write-string " ON " stream)
  (emit-ident dialect (create-index-table stmt) stream)
  (write-string " (" stream)
  (loop for (c . rest) on (create-index-columns stmt)
        do (emit-sql dialect c stream ctx)
           (when rest (write-string ", " stream)))
  (write-char #\) stream))

(defmethod emit-sql (dialect (stmt drop-index-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP INDEX " stream)
  (when (drop-index-if-exists stmt)
    (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-index-name stmt) stream))

(defgeneric emit-create-procedure (dialect stmt stream ctx)
  (:method ((dialect sql-dialect) stmt stream ctx)
    (declare (ignore stream ctx))
    (error 'sql-dialect-unsupported
           :feature :create-procedure
           :dialect dialect
           :message "CREATE PROCEDURE not supported on this dialect")))

(defgeneric emit-call (dialect stmt stream ctx)
  (:method ((dialect sql-dialect) stmt stream ctx)
    (declare (ignore stream ctx))
    (error 'sql-dialect-unsupported
           :feature :call
           :dialect dialect
           :message "CALL not supported on this dialect")))

(defmethod emit-sql (dialect (stmt create-procedure-statement) stream ctx)
  (emit-create-procedure dialect stmt stream ctx))

(defmethod emit-sql (dialect (stmt call-statement) stream ctx)
  (emit-call dialect stmt stream ctx))
