(in-package #:sql-query)

(defclass sql-dialect ()
  ((type-registry :initform (make-hash-table :test #'eq)
                  :accessor dialect-type-registry)
   (op-registry :initform (make-hash-table :test #'eq)
                :accessor dialect-op-registry)
   (func-registry :initform (make-hash-table :test #'eq)
                  :accessor dialect-func-registry))
  (:documentation "Dialect protocol. Builtin concrete class = ANSI; vendors in backend systems.
TYPE/OP/FUNC registries hold extension adapters (register-sql-type/op/func)."))

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
  (:documentation "Render a column type spec to SQL text (ANSI defaults + registry).")
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
  (:documentation "Write SQL for NODE to STREAM; accumulate params in CTX.

Open for dialect packages: specialize on (dialect-class node-class).
Unknown core nodes fall through to EMIT-EXTENSION."))

(defgeneric emit-extension (dialect node stream ctx)
  (:documentation
   "Fallback emit for nodes without a more specific EMIT-SQL method.
    Dialect packages may specialize this for whole families of sql-extension nodes.")
  (:method ((dialect sql-dialect) node stream ctx)
    (declare (ignore stream ctx))
    (error 'sql-dialect-unsupported
           :feature (class-name (class-of node))
           :dialect dialect
           :message (format nil "no emit-sql method for ~s on ~s"
                            (class-name (class-of node))
                            (class-name (class-of dialect))))))

(defmethod emit-sql (dialect (node sql-node) stream ctx)
  "Least-specific fallback (do not specialize DIALECT here — avoids CLOS
ambiguity with methods that only specialize the node). Dialects add
more-specific EMIT-SQL methods or specialize EMIT-EXTENSION."
  (emit-extension dialect node stream ctx))

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

(defun emit-sql-literal (dialect value stream)
  "Write VALUE as SQL literal text according to its Lisp type (never a placeholder).
NIL → NULL, T → TRUE, :FALSE → FALSE, numbers/strings/chars as SQL literals."
  (cond
    ((null value) (write-string "NULL" stream))
    ((eq value t) (write-string (dialect-boolean dialect t) stream))
    ((eq value :false) (write-string (dialect-boolean dialect nil) stream))
    ((integerp value) (format stream "~d" value))
    ((floatp value)
     (write-string (string-trim "( )" (format nil "~f" value)) stream))
    ((stringp value)
     (write-char #\' stream)
     (loop for c across value
           do (when (char= c #\') (write-char #\' stream))
              (write-char c stream))
     (write-char #\' stream))
    ((characterp value)
     (emit-sql-literal dialect (string value) stream))
    ((and (vectorp value) (not (stringp value)))
     (%err "cannot emit vector ~s as SQL literal — register a type or use bindparam" value))
    (t (%err "cannot emit ~s (~a) as SQL literal — use bindparam or a typed literal"
             value (type-of value)))))

(defmethod emit-sql (dialect (node literal) stream ctx)
  "Literals are always inlined. Placeholders come only from BINDPARAM / sql-fragment."
  (let ((v (literal-value node))
        (ty (literal-sql-type node)))
    (if ty
        (emit-typed-value dialect ty v stream ctx :bind nil)
        (emit-sql-literal dialect v stream))))

(defmethod emit-sql (dialect (node typed-value) stream ctx)
  (emit-typed-value dialect (typed-value-sql-type node) (typed-value-value node)
                    stream ctx :bind nil))

(defun %emit-op (dialect op stream)
  (let ((def (find-sql-op dialect op)))
    (if def
        (write-string (sql-op-sql-text def) stream)
        (write-string
         (ecase op
           (:= "=") (:!= "<>") (:< "<") (:> ">") (:<= "<=") (:>= ">=")
           (:+ "+") (:- "-") (:* "*") (:/ "/")
           (:and "AND") (:or "OR") (:not "NOT"))
         stream))))

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
  (let* ((name (function-call-name node))
         (key (cond ((keywordp name) name)
                    ((symbolp name) (intern (symbol-name name) :keyword))
                    (t nil)))
         (def (and key (find-sql-func dialect key))))
    (cond
      ((and def (sql-func-emit-fn def))
       (funcall (sql-func-emit-fn def) dialect node stream ctx))
      (t
       (write-string (if def
                         (sql-func-sql-name def)
                         (ident-string name))
                     stream)
       (write-char #\( stream)
       (loop for (a . rest) on (function-call-args node)
             do (emit-sql dialect a stream ctx)
                (when rest (write-string ", " stream)))
       (write-char #\) stream)))))

(defmethod emit-sql (dialect (node array-literal) stream ctx)
  "ANSI/PG-style ARRAY[…]. Dialects may specialize."
  (write-string "ARRAY[" stream)
  (loop for (a . rest) on (array-literal-items node)
        do (emit-sql dialect a stream ctx)
           (when rest (write-string ", " stream)))
  (write-char #\] stream))

(defmethod emit-sql (dialect (node exists-op) stream ctx)
  (write-string "EXISTS (" stream)
  (emit-sql dialect (exists-query node) stream ctx)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node labeled-expr) stream ctx)
  (emit-sql dialect (labeled-expr-of node) stream ctx)
  (write-string " AS " stream)
  (emit-ident dialect (labeled-name node) stream))

(defun bind-param-effective-value (node)
  "Value sent with prepare/execute: explicit :value, else :default, else name."
  (cond
    ((bind-param-has-value node) (bind-param-value node))
    ((bind-param-has-default node) (bind-param-default node))
    (t (bind-param-name node))))

(defmethod emit-sql (dialect (node bind-param) stream ctx)
  "Explicit placeholder only. :default fills the params vector for execute/prepare
when :value is omitted — it is not inlined into SQL."
  (let* ((ty (bind-param-sql-type node))
         (raw (bind-param-effective-value node)))
    (if ty
        (emit-typed-value dialect ty raw stream ctx :bind t)
        (progn
          (emit-placeholder dialect stream ctx)
          (push-param ctx raw)))))

(defun emit-from-item (dialect item stream ctx &optional alias)
  (cond
    ((typep item 'lateral-subquery)
     (write-string "LATERAL " stream)
     (write-char #\( stream)
     (emit-sql dialect (subquery-query item) stream ctx)
     (write-char #\) stream)
     (let ((a (or alias (subquery-alias item))))
       (when a
         (write-string " AS " stream)
         (emit-ident dialect a stream))))
    ((typep item 'subquery)
     (write-char #\( stream)
     (emit-sql dialect (subquery-query item) stream ctx)
     (write-char #\) stream)
     (let ((a (or alias (subquery-alias item))))
       (when a
         (write-string " AS " stream)
         (emit-ident dialect a stream))))
    ((typep item 'values-selectable)
     (emit-sql dialect item stream ctx)
     (when (or alias (values-selectable-alias item))
       (write-string " AS " stream)
       (emit-ident dialect (or alias (values-selectable-alias item)) stream)))
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

(defmethod emit-sql (dialect (node is-distinct-from-op) stream ctx)
  (emit-sql dialect (is-distinct-from-left node) stream ctx)
  (write-string (if (is-distinct-from-not-p node)
                    " IS NOT DISTINCT FROM "
                    " IS DISTINCT FROM ")
                stream)
  (emit-sql dialect (is-distinct-from-right node) stream ctx))

(defmethod emit-sql (dialect (node similar-to-op) stream ctx)
  (emit-sql dialect (similar-to-left node) stream ctx)
  (write-string (if (similar-to-not-p node) " NOT SIMILAR TO " " SIMILAR TO ") stream)
  (emit-sql dialect (similar-to-pattern node) stream ctx)
  (when (similar-to-escape node)
    (write-string " ESCAPE " stream)
    (emit-sql dialect (similar-to-escape node) stream ctx)))

(defmethod emit-sql (dialect (node quantified-op) stream ctx)
  (emit-sql dialect (quantified-op-left node) stream ctx)
  (write-char #\Space stream)
  (%emit-op dialect (quantified-op-op node) stream)
  (write-char #\Space stream)
  (write-string (ecase (quantified-op-quantifier node)
                  (:any "ANY") (:all "ALL") (:some "SOME"))
                stream)
  (write-string " (" stream)
  (emit-sql dialect (quantified-op-subquery node) stream ctx)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node unique-pred) stream ctx)
  (write-string "UNIQUE (" stream)
  (emit-sql dialect (unique-pred-query node) stream ctx)
  (write-char #\) stream))

(defmethod emit-sql (dialect (node collate-expr) stream ctx)
  (emit-sql dialect (collate-expr-of node) stream ctx)
  (write-string " COLLATE " stream)
  (emit-ident dialect (collate-collation node) stream))

(defmethod emit-sql (dialect (node nullif-expr) stream ctx)
  (write-string "NULLIF(" stream)
  (emit-sql dialect (nullif-left node) stream ctx)
  (write-string ", " stream)
  (emit-sql dialect (nullif-right node) stream ctx)
  (write-char #\) stream))

(defun emit-window-frame-bound (dialect bound stream ctx)
  (cond
    ((eq bound :unbounded-preceding) (write-string "UNBOUNDED PRECEDING" stream))
    ((eq bound :unbounded-following) (write-string "UNBOUNDED FOLLOWING" stream))
    ((eq bound :current-row) (write-string "CURRENT ROW" stream))
    ((and (consp bound) (eq (first bound) :preceding))
     (emit-sql dialect (lit (second bound)) stream ctx)
     (write-string " PRECEDING" stream))
    ((and (consp bound) (eq (first bound) :following))
     (emit-sql dialect (lit (second bound)) stream ctx)
     (write-string " FOLLOWING" stream))
    (t (%err "bad window frame bound ~s" bound))))

(defmethod emit-sql (dialect (node over-expr) stream ctx)
  (emit-sql dialect (over-expr-of node) stream ctx)
  (write-string " OVER (" stream)
  (let ((w (over-window node)))
    (when (window-partition-by w)
      (write-string "PARTITION BY " stream)
      (emit-column-list dialect (window-partition-by w) stream ctx))
    (when (window-order-by w)
      (when (window-partition-by w) (write-char #\Space stream))
      (write-string "ORDER BY " stream)
      (emit-order-item-list dialect (window-order-by w) stream ctx))
    (when (window-frame w)
      (write-char #\Space stream)
      (write-string (ecase (window-frame-unit (window-frame w))
                      (:rows "ROWS") (:range "RANGE") (:groups "GROUPS"))
                    stream)
      (write-char #\Space stream)
      (let* ((fr (window-frame w))
             (start (window-frame-start fr))
             (end (window-frame-end fr)))
        (if end
            (progn
              (write-string "BETWEEN " stream)
              (emit-window-frame-bound dialect start stream ctx)
              (write-string " AND " stream)
              (emit-window-frame-bound dialect end stream ctx))
            (emit-window-frame-bound dialect start stream ctx)))))
  (write-char #\) stream))

(defmethod emit-sql (dialect (node grouping-expr) stream ctx)
  (write-string (ecase (grouping-kind node)
                  (:rollup "ROLLUP")
                  (:cube "CUBE")
                  (:grouping-sets "GROUPING SETS"))
                stream)
  (write-string " (" stream)
  (ecase (grouping-kind node)
    ((:rollup :cube)
     (emit-column-list dialect (grouping-items node) stream ctx))
    (:grouping-sets
     (loop for (set . rest) on (grouping-items node)
           do (write-char #\( stream)
              (emit-column-list dialect set stream ctx)
              (write-char #\) stream)
              (when rest (write-string ", " stream)))))
  (write-char #\) stream))

(defmethod emit-sql (dialect (node subquery) stream ctx)
  (emit-from-item dialect node stream ctx))

(defmethod emit-sql (dialect (node binary-op) stream ctx)
  (let ((def (find-sql-op dialect (binary-op-op node))))
    (if (and def (sql-op-emit-fn def))
        (funcall (sql-op-emit-fn def) dialect node stream ctx)
        (progn
          (write-char #\( stream)
          (emit-sql dialect (binary-op-left node) stream ctx)
          (write-char #\Space stream)
          (%emit-op dialect (binary-op-op node) stream)
          (write-char #\Space stream)
          (emit-sql dialect (binary-op-right node) stream ctx)
          (write-char #\) stream)))))

(defmethod emit-sql (dialect (node unary-op) stream ctx)
  (let ((def (find-sql-op dialect (unary-op-op node))))
    (if (and def (sql-op-emit-fn def))
        (funcall (sql-op-emit-fn def) dialect node stream ctx)
        (progn
          (write-char #\( stream)
          (%emit-op dialect (unary-op-op node) stream)
          (write-char #\Space stream)
          (emit-sql dialect (unary-op-operand node) stream ctx)
          (write-char #\) stream)))))

(defmethod emit-sql (dialect (node nary-op) stream ctx)
  (let ((def (find-sql-op dialect (nary-op-op node))))
    (if (and def (sql-op-emit-fn def))
        (funcall (sql-op-emit-fn def) dialect node stream ctx)
        (progn
          (write-char #\( stream)
          (loop for (op . rest) on (nary-op-operands node)
                for first = t then nil
                do (unless first
                     (write-char #\Space stream)
                     (%emit-op dialect (nary-op-op node) stream)
                     (write-char #\Space stream))
                do (emit-sql dialect op stream ctx))
          (write-char #\) stream)))))

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
  (emit-sql dialect (like-op-pattern node) stream ctx)
  (when (like-op-escape node)
    (write-string " ESCAPE " stream)
    (emit-sql dialect (like-op-escape node) stream ctx)))

(defmethod emit-sql (dialect (node is-null-op) stream ctx)
  (emit-sql dialect (is-null-op-operand node) stream ctx)
  (write-string (if (is-null-op-not-p node) " IS NOT NULL" " IS NULL") stream))

;;; ---- clause helpers ----

(defun %clause (stmt type)
  (%find-clause (statement-clauses stmt) type))

(defun %clauses (stmt type)
  (remove-if-not (lambda (c) (typep c type)) (statement-clauses stmt)))

(defun emit-order-item (dialect item stream ctx)
  "ITEM is (expr dir) or (expr dir nulls) from %NORMALIZE-ORDER-ITEM."
  (destructuring-bind (expr dir &optional nulls) item
    (emit-sql dialect expr stream ctx)
    (when (eq dir :desc) (write-string " DESC" stream))
    (when nulls
      (write-string (ecase nulls
                      (:nulls-first " NULLS FIRST")
                      (:nulls-last " NULLS LAST"))
                    stream))))

(defun emit-order-item-list (dialect items stream ctx)
  (loop for (item . rest) on items
        do (emit-order-item dialect item stream ctx)
           (when rest (write-string ", " stream))))

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

(defgeneric emit-distinct (dialect clause stream ctx)
  (:method (dialect clause stream ctx)
    (write-string "DISTINCT " stream)
    (when (distinct-on clause)
      (write-string "ON (" stream)
      (emit-column-list dialect (distinct-on clause) stream ctx)
      (write-string ") " stream))))

(defgeneric emit-join (dialect join stream ctx)
  (:method (dialect join stream ctx)
    (when (join-natural join)
      (write-string "NATURAL " stream))
    (write-string (ecase (join-type join)
                    (:inner "INNER JOIN")
                    (:left "LEFT JOIN")
                    (:right "RIGHT JOIN")
                    (:full "FULL OUTER JOIN")
                    (:cross "CROSS JOIN"))
                  stream)
    (write-char #\Space stream)
    (emit-from-item dialect (join-table join) stream ctx (join-alias join))
    (cond
      ((join-using join)
       (write-string " USING (" stream)
       (emit-column-list dialect (join-using join) stream ctx)
       (write-char #\) stream))
      ((and (join-on join) (not (eq (join-type join) :cross)) (not (join-natural join)))
       (write-string " ON " stream)
       (emit-sql dialect (join-on join) stream ctx)))))

(defgeneric emit-limit-offset (dialect lim off stream ctx)
  (:documentation "Default = LIMIT/OFFSET (vendor). ANSI specializes to FETCH FIRST.")
  (:method (dialect lim off stream ctx)
    (when lim
      (write-string " LIMIT " stream)
      (emit-sql dialect (lit (limit-count lim)) stream ctx))
    (when off
      (write-string " OFFSET " stream)
      (emit-sql dialect (lit (offset-count off)) stream ctx))))

(defgeneric emit-for-update (dialect clause stream ctx)
  (:method (dialect clause stream ctx)
    (write-string " FOR UPDATE" stream)
    (when (for-update-of clause)
      (write-string " OF " stream)
      (emit-column-list dialect (for-update-of clause) stream ctx))
    (when (for-update-nowait clause) (write-string " NOWAIT" stream))
    (when (for-update-skip-locked clause) (write-string " SKIP LOCKED" stream))))

(defgeneric emit-returning (dialect items stream ctx)
  (:method (dialect items stream ctx)
    (write-string " RETURNING " stream)
    (emit-column-list dialect items stream ctx)))

(defmethod emit-sql (dialect (stmt select-statement) stream ctx)
  (emit-with-cte dialect stmt stream ctx)
  (write-string "SELECT " stream)
  (let ((d (%clause stmt 'distinct-clause)))
    (when d (emit-distinct dialect d stream ctx)))
  (let ((cols (%clause stmt 'columns-clause)))
    (emit-column-list dialect (and cols (columns-items cols)) stream ctx))
  (let ((f (%clause stmt 'from-clause)))
    (when f
      (write-string " FROM " stream)
      (emit-from-item dialect (from-table f) stream ctx (from-alias f))))
  (dolist (j (%clauses stmt 'join-clause))
    (write-char #\Space stream)
    (emit-join dialect j stream ctx))
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
      (emit-order-item-list dialect (order-by-items o) stream ctx)))
  (emit-limit-offset dialect
                     (%clause stmt 'limit-clause)
                     (%clause stmt 'offset-clause)
                     stream ctx)
  (let ((fu (%clause stmt 'for-update-clause)))
    (when fu (emit-for-update dialect fu stream ctx))))

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
        (src (%clause stmt 'select-source-clause))
        (defaults (%clause stmt 'default-values-clause)))
    (cond
      (defaults (write-string " DEFAULT VALUES" stream))
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
      (t (%err "insert-into requires sql-values, default-values, or select"))))
  (let ((ret (%clause stmt 'returning-clause)))
    (when ret
      (emit-returning dialect (returning-items ret) stream ctx))))

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
      (emit-returning dialect (returning-items ret) stream ctx))))

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

(defun emit-referential-action (action stream)
  (write-string (ecase action
                  (:cascade "CASCADE")
                  (:restrict "RESTRICT")
                  (:set-null "SET NULL")
                  (:set-default "SET DEFAULT")
                  (:no-action "NO ACTION"))
                stream))

(defun emit-constraint-deferrable (c stream)
  (let ((d (table-constraint-deferrable c))
        (i (table-constraint-initially c)))
    (cond
      ((eq d :not) (write-string " NOT DEFERRABLE" stream))
      (d (write-string " DEFERRABLE" stream)))
    (when i
      (write-string (ecase i
                      (:deferred " INITIALLY DEFERRED")
                      (:immediate " INITIALLY IMMEDIATE"))
                    stream))))

(defun emit-table-constraint (dialect c stream ctx)
  (when (table-constraint-name c)
    (write-string "CONSTRAINT " stream)
    (emit-ident dialect (table-constraint-name c) stream)
    (write-char #\Space stream))
  (etypecase c
    (primary-key-constraint
     (write-string "PRIMARY KEY (" stream)
     (emit-column-list dialect (primary-key-columns c) stream ctx)
     (write-char #\) stream))
    (unique-constraint
     (write-string "UNIQUE (" stream)
     (emit-column-list dialect (unique-constraint-columns c) stream ctx)
     (write-char #\) stream))
    (check-constraint
     (write-string "CHECK (" stream)
     (emit-sql dialect (check-constraint-expr c) stream ctx)
     (write-char #\) stream))
    (foreign-key-constraint
     (write-string "FOREIGN KEY (" stream)
     (emit-column-list dialect (foreign-key-columns c) stream ctx)
     (write-string ") REFERENCES " stream)
     (emit-ident dialect (foreign-key-ref-table c) stream)
     (when (foreign-key-ref-columns c)
       (write-string " (" stream)
       (emit-column-list dialect (foreign-key-ref-columns c) stream ctx)
       (write-char #\) stream))
     (when (foreign-key-match c)
       (write-string " MATCH " stream)
       (write-string (ecase (foreign-key-match c)
                       (:full "FULL") (:partial "PARTIAL") (:simple "SIMPLE"))
                     stream))
     (when (foreign-key-on-delete c)
       (write-string " ON DELETE " stream)
       (emit-referential-action (foreign-key-on-delete c) stream))
     (when (foreign-key-on-update c)
       (write-string " ON UPDATE " stream)
       (emit-referential-action (foreign-key-on-update c) stream))))
  (emit-constraint-deferrable c stream))

(defgeneric emit-create-table-extra (dialect extra stream ctx)
  (:documentation "Emit a CREATE TABLE extension node after the column list.")
  (:method ((dialect sql-dialect) extra stream ctx)
    (emit-sql dialect extra stream ctx)))

(defmethod emit-sql (dialect (stmt create-table-statement) stream ctx)
  (write-string "CREATE TABLE " stream)
  (when (create-table-if-not-exists stmt)
    (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-table-table stmt) stream)
  (when (create-table-of-type stmt)
    (write-string " OF " stream)
    (emit-ident dialect (create-table-of-type stmt) stream))
  (let ((parts (append (create-table-columns stmt)
                       (create-table-constraints stmt))))
    (when (or parts (null (create-table-of-type stmt)))
      (write-string " (" stream)
      (loop for (p . rest) on parts
            do (if (typep p 'column-def)
                   (emit-column-def dialect p stream ctx)
                   (emit-table-constraint dialect p stream ctx))
               (when rest (write-string ", " stream)))
      (write-char #\) stream)))
  (dolist (extra (create-table-extras stmt))
    (write-char #\Space stream)
    (emit-create-table-extra dialect extra stream ctx)))

(defmethod emit-sql (dialect (stmt drop-table-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP TABLE " stream)
  (when (drop-table-if-exists stmt)
    (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-table-table stmt) stream))

(defgeneric emit-alter-table-action (dialect action stream ctx)
  (:documentation
   "Emit one ALTER TABLE action. Open — dialects add methods for vendor clauses.")
  (:method ((dialect sql-dialect) (action add-column-clause) stream ctx)
    (write-string "ADD COLUMN " stream)
    (emit-column-def dialect (add-column-column action) stream ctx))
  (:method ((dialect sql-dialect) (action drop-column-clause) stream ctx)
    (declare (ignore ctx))
    (write-string "DROP COLUMN " stream)
    (emit-ident dialect (drop-column-name action) stream))
  (:method ((dialect sql-dialect) (action add-constraint-clause) stream ctx)
    (write-string "ADD " stream)
    (emit-table-constraint dialect (add-constraint-constraint action) stream ctx))
  (:method ((dialect sql-dialect) (action drop-constraint-clause) stream ctx)
    (declare (ignore ctx))
    (write-string "DROP CONSTRAINT " stream)
    (emit-ident dialect (drop-constraint-name action) stream))
  (:method ((dialect sql-dialect) (action rename-column-clause) stream ctx)
    (declare (ignore ctx))
    (write-string "RENAME COLUMN " stream)
    (emit-ident dialect (rename-column-old action) stream)
    (write-string " TO " stream)
    (emit-ident dialect (rename-column-new action) stream))
  (:method ((dialect sql-dialect) (action rename-table-clause) stream ctx)
    (declare (ignore ctx))
    (write-string "RENAME TO " stream)
    (emit-ident dialect (rename-table-new action) stream))
  (:method ((dialect sql-dialect) action stream ctx)
    (declare (ignore stream ctx))
    (error 'sql-dialect-unsupported
           :feature (class-name (class-of action))
           :dialect dialect
           :message (format nil "unsupported ALTER TABLE action ~s" action))))

(defmethod emit-sql (dialect (stmt alter-table-statement) stream ctx)
  (loop for (action . rest) on (alter-table-actions stmt)
        do (write-string "ALTER TABLE " stream)
           (emit-ident dialect (alter-table-table stmt) stream)
           (write-char #\Space stream)
           (emit-alter-table-action dialect action stream ctx)
           (when rest (write-string "; " stream))))

(defmethod emit-sql (dialect (stmt values-selectable) stream ctx)
  (write-string "VALUES " stream)
  (loop for (row . rest) on (values-selectable-rows stmt)
        do (write-char #\( stream)
           (loop for (cell . r2) on row
                 do (emit-sql dialect cell stream ctx)
                    (when r2 (write-string ", " stream)))
           (write-char #\) stream)
           (when rest (write-string ", " stream))))

(defmethod emit-sql (dialect (stmt create-view-statement) stream ctx)
  (write-string "CREATE " stream)
  (when (create-view-or-replace stmt) (write-string "OR REPLACE " stream))
  (when (create-view-recursive stmt) (write-string "RECURSIVE " stream))
  (write-string "VIEW " stream)
  (emit-ident dialect (create-view-name stmt) stream)
  (when (create-view-columns stmt)
    (write-string " (" stream)
    (emit-column-list dialect (mapcar #'ensure-expr (create-view-columns stmt)) stream ctx)
    (write-char #\) stream))
  (write-string " AS " stream)
  (emit-sql dialect (create-view-query stmt) stream ctx))

(defmethod emit-sql (dialect (stmt drop-view-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP VIEW " stream)
  (when (drop-view-if-exists stmt) (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-view-name stmt) stream)
  (when (drop-view-cascade stmt) (write-string " CASCADE" stream)))

(defmethod emit-sql (dialect (stmt create-schema-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "CREATE SCHEMA " stream)
  (when (create-schema-if-not-exists stmt) (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-schema-name stmt) stream)
  (when (create-schema-authorization stmt)
    (write-string " AUTHORIZATION " stream)
    (emit-ident dialect (create-schema-authorization stmt) stream)))

(defmethod emit-sql (dialect (stmt drop-schema-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP SCHEMA " stream)
  (when (drop-schema-if-exists stmt) (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-schema-name stmt) stream)
  (when (drop-schema-cascade stmt) (write-string " CASCADE" stream)))

(defmethod emit-sql (dialect (stmt create-sequence-statement) stream ctx)
  (write-string "CREATE SEQUENCE " stream)
  (when (create-sequence-if-not-exists stmt) (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-sequence-name stmt) stream)
  (when (create-sequence-start stmt)
    (write-string " START WITH " stream)
    (emit-sql dialect (lit (create-sequence-start stmt)) stream ctx))
  (when (create-sequence-increment stmt)
    (write-string " INCREMENT BY " stream)
    (emit-sql dialect (lit (create-sequence-increment stmt)) stream ctx))
  (when (create-sequence-minvalue stmt)
    (write-string " MINVALUE " stream)
    (emit-sql dialect (lit (create-sequence-minvalue stmt)) stream ctx))
  (when (create-sequence-maxvalue stmt)
    (write-string " MAXVALUE " stream)
    (emit-sql dialect (lit (create-sequence-maxvalue stmt)) stream ctx))
  (when (create-sequence-cycle stmt) (write-string " CYCLE" stream)))

(defmethod emit-sql (dialect (stmt drop-sequence-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP SEQUENCE " stream)
  (when (drop-sequence-if-exists stmt) (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-sequence-name stmt) stream))

(defmethod emit-sql (dialect (stmt truncate-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "TRUNCATE TABLE " stream)
  (loop for (tname . rest) on (truncate-tables stmt)
        do (emit-ident dialect tname stream)
           (when rest (write-string ", " stream)))
  (when (truncate-identity stmt)
    (write-string (ecase (truncate-identity stmt)
                    (:restart " RESTART IDENTITY")
                    (:continue " CONTINUE IDENTITY"))
                  stream))
  (when (truncate-cascade stmt) (write-string " CASCADE" stream)))

(defun emit-merge-action (dialect action stream ctx)
  (etypecase action
    (merge-update-action
     (write-string "UPDATE SET " stream)
     (loop for (a . rest) on (merge-update-assignments action)
           do (emit-sql dialect (binary-op-left a) stream ctx)
              (write-string " = " stream)
              (emit-sql dialect (binary-op-right a) stream ctx)
              (when rest (write-string ", " stream)))
     (when (merge-update-where action)
       (write-string " WHERE " stream)
       (emit-sql dialect (merge-update-where action) stream ctx)))
    (merge-delete-action
     (write-string "DELETE" stream)
     (when (merge-delete-where action)
       (write-string " WHERE " stream)
       (emit-sql dialect (merge-delete-where action) stream ctx)))
    (merge-insert-action
     (write-string "INSERT " stream)
     (when (merge-insert-columns action)
       (write-char #\( stream)
       (emit-column-list dialect (merge-insert-columns action) stream ctx)
       (write-string ") " stream))
     (write-string "VALUES (" stream)
     (emit-column-list dialect (merge-insert-values action) stream ctx)
     (write-char #\) stream))))

(defmethod emit-sql (dialect (stmt merge-statement) stream ctx)
  (write-string "MERGE INTO " stream)
  (emit-ident dialect (merge-table stmt) stream)
  (write-string " USING " stream)
  (emit-from-item dialect (merge-using stmt) stream ctx)
  (write-string " ON " stream)
  (emit-sql dialect (merge-on stmt) stream ctx)
  (when (merge-when-matched stmt)
    (write-string " WHEN MATCHED THEN " stream)
    (emit-merge-action dialect (merge-when-matched stmt) stream ctx))
  (when (merge-when-not-matched stmt)
    (write-string " WHEN NOT MATCHED THEN " stream)
    (emit-merge-action dialect (merge-when-not-matched stmt) stream ctx)))

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

;;; ---------------------------------------------------------------------------
;;; CREATE / DROP / ALTER TYPE + DOMAIN
;;; ---------------------------------------------------------------------------

(defgeneric emit-create-type-kind (dialect kind stmt stream ctx)
  (:documentation
   "Emit CREATE TYPE for KIND. Open — dialects specialize (eql :enum), (eql :base), …")
  (:method ((dialect sql-dialect) (kind (eql :distinct)) stmt stream ctx)
    (declare (ignore ctx))
    (write-string "CREATE TYPE " stream)
    (when (create-type-if-not-exists stmt) (write-string "IF NOT EXISTS " stream))
    (emit-ident dialect (create-type-name stmt) stream)
    (write-string " AS " stream)
    (write-string (dialect-type-sql dialect (create-type-base-type stmt)) stream))
  (:method ((dialect sql-dialect) (kind (eql :structured)) stmt stream ctx)
    (declare (ignore ctx))
    (write-string "CREATE TYPE " stream)
    (when (create-type-if-not-exists stmt) (write-string "IF NOT EXISTS " stream))
    (emit-ident dialect (create-type-name stmt) stream)
    (write-string " AS (" stream)
    (loop for (attr . rest) on (create-type-attributes stmt)
          do (emit-ident dialect (type-attribute-name attr) stream)
             (write-char #\Space stream)
             (write-string (dialect-type-sql dialect (type-attribute-type attr)) stream)
             (when rest (write-string ", " stream)))
    (write-char #\) stream))
  (:method ((dialect sql-dialect) (kind (eql :enum)) stmt stream ctx)
    (declare (ignore stmt stream ctx))
    (error 'sql-dialect-unsupported
           :feature :create-type-enum
           :dialect dialect
           :message "CREATE TYPE … AS ENUM is not ANSI SQL — use sql-query-postgres"))
  (:method ((dialect sql-dialect) kind stmt stream ctx)
    (declare (ignore stmt stream ctx))
    (error 'sql-dialect-unsupported
           :feature kind
           :dialect dialect
           :message (format nil "CREATE TYPE kind ~s unsupported on ~s"
                            kind (class-name (class-of dialect))))))

(defgeneric emit-create-type (dialect stmt stream ctx)
  (:documentation "Emit CREATE TYPE — default dispatches on CREATE-TYPE-KIND.")
  (:method ((dialect sql-dialect) stmt stream ctx)
    (emit-create-type-kind dialect (create-type-kind stmt) stmt stream ctx)))

(defmethod emit-sql (dialect (stmt create-type-statement) stream ctx)
  (emit-create-type dialect stmt stream ctx))

(defmethod emit-sql (dialect (stmt drop-type-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP TYPE " stream)
  (when (drop-type-if-exists stmt) (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-type-name stmt) stream)
  (when (drop-type-cascade stmt) (write-string " CASCADE" stream)))

(defgeneric emit-alter-type-action (dialect action stream ctx)
  (:method ((dialect sql-dialect) (action add-attribute-clause) stream ctx)
    (declare (ignore ctx))
    (let ((attr (add-attribute-attribute action)))
      (write-string "ADD ATTRIBUTE " stream)
      (emit-ident dialect (type-attribute-name attr) stream)
      (write-char #\Space stream)
      (write-string (dialect-type-sql dialect (type-attribute-type attr)) stream)))
  (:method ((dialect sql-dialect) (action drop-attribute-clause) stream ctx)
    (declare (ignore ctx))
    (write-string "DROP ATTRIBUTE " stream)
    (emit-ident dialect (drop-attribute-name action) stream))
  (:method ((dialect sql-dialect) (action rename-attribute-clause) stream ctx)
    (declare (ignore ctx))
    (write-string "RENAME ATTRIBUTE " stream)
    (emit-ident dialect (rename-attribute-old action) stream)
    (write-string " TO " stream)
    (emit-ident dialect (rename-attribute-new action) stream))
  (:method ((dialect sql-dialect) (action add-enum-value-clause) stream ctx)
    (declare (ignore action stream ctx))
    (error 'sql-dialect-unsupported
           :feature :alter-type-add-value
           :dialect dialect
           :message "ALTER TYPE … ADD VALUE is not ANSI SQL — use sql-query-postgres")))

(defmethod emit-sql (dialect (stmt alter-type-statement) stream ctx)
  (loop for (action . rest) on (alter-type-actions stmt)
        do (write-string "ALTER TYPE " stream)
           (emit-ident dialect (alter-type-name stmt) stream)
           (write-char #\Space stream)
           (emit-alter-type-action dialect action stream ctx)
           (when rest (write-string "; " stream))))

(defmethod emit-sql (dialect (stmt create-domain-statement) stream ctx)
  (write-string "CREATE DOMAIN " stream)
  (when (create-domain-if-not-exists stmt) (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-domain-name stmt) stream)
  (write-string " AS " stream)
  (write-string (dialect-type-sql dialect (create-domain-base-type stmt)) stream)
  (when (create-domain-default stmt)
    (write-string " DEFAULT " stream)
    (emit-sql dialect (ensure-expr (create-domain-default stmt)) stream ctx))
  (when (create-domain-not-null stmt)
    (write-string " NOT NULL" stream))
  (when (create-domain-check stmt)
    (write-string " CHECK (" stream)
    (emit-sql dialect (create-domain-check stmt) stream ctx)
    (write-char #\) stream)))

(defmethod emit-sql (dialect (stmt drop-domain-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP DOMAIN " stream)
  (when (drop-domain-if-exists stmt) (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-domain-name stmt) stream)
  (when (drop-domain-cascade stmt) (write-string " CASCADE" stream)))

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

;;; ---------------------------------------------------------------------------
;;; CREATE TABLE AS + transaction control
;;; ---------------------------------------------------------------------------

(defmethod emit-sql (dialect (stmt create-table-as-statement) stream ctx)
  (write-string "CREATE " stream)
  (when (create-table-as-temporary stmt)
    (write-string "TEMPORARY " stream))
  (write-string "TABLE " stream)
  (when (create-table-as-if-not-exists stmt)
    (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-table-as-table stmt) stream)
  (when (create-table-as-columns stmt)
    (write-string " (" stream)
    (emit-column-list dialect (create-table-as-columns stmt) stream ctx)
    (write-char #\) stream))
  (write-string " AS " stream)
  (emit-sql dialect (create-table-as-query stmt) stream ctx))

(defun emit-transaction-characteristics (stmt stream)
  "Emit ISOLATION / access mode / DEFERRABLE fragments after START|SET TRANSACTION."
  (when (transaction-isolation stmt)
    (write-string " ISOLATION LEVEL " stream)
    (write-string (ecase (transaction-isolation stmt)
                    (:read-uncommitted "READ UNCOMMITTED")
                    (:read-committed "READ COMMITTED")
                    (:repeatable-read "REPEATABLE READ")
                    (:serializable "SERIALIZABLE"))
                  stream))
  (when (transaction-access-mode stmt)
    (write-string (ecase (transaction-access-mode stmt)
                    (:read-only " READ ONLY")
                    (:read-write " READ WRITE"))
                  stream))
  (let ((d (transaction-deferrable stmt)))
    (cond
      ((eq d :not) (write-string " NOT DEFERRABLE" stream))
      (d (write-string " DEFERRABLE" stream)))))

(defmethod emit-sql (dialect (stmt start-transaction-statement) stream ctx)
  (declare (ignore dialect ctx))
  (write-string "START TRANSACTION" stream)
  (emit-transaction-characteristics stmt stream))

(defmethod emit-sql (dialect (stmt set-transaction-statement) stream ctx)
  (declare (ignore dialect ctx))
  (write-string "SET TRANSACTION" stream)
  (emit-transaction-characteristics stmt stream))

(defmethod emit-sql (dialect (stmt commit-statement) stream ctx)
  (declare (ignore dialect stmt ctx))
  (write-string "COMMIT" stream))

(defmethod emit-sql (dialect (stmt rollback-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "ROLLBACK" stream)
  (when (rollback-savepoint stmt)
    (write-string " TO SAVEPOINT " stream)
    (emit-ident dialect (rollback-savepoint stmt) stream)))

(defmethod emit-sql (dialect (stmt savepoint-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "SAVEPOINT " stream)
  (emit-ident dialect (savepoint-name stmt) stream))

(defmethod emit-sql (dialect (stmt release-savepoint-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "RELEASE SAVEPOINT " stream)
  (emit-ident dialect (release-savepoint-name stmt) stream))
