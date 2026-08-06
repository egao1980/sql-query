(in-package #:sql-query)

;;; ---------------------------------------------------------------------------
;;; Expression helpers
;;; ---------------------------------------------------------------------------

(defun col (name &optional table)
  (cond
    (table
     (make-instance 'column-ref :name name :table table))
    ((or (keywordp name) (and (symbolp name) (not (keywordp name))))
     (let* ((s (string-downcase (symbol-name name)))
            (dot (position #\. s)))
       (if dot
           (make-instance 'column-ref
                          :table (subseq s 0 dot)
                          :name (subseq s (1+ dot)))
           (make-instance 'column-ref :name name))))
    (t (make-instance 'column-ref :name name :table table))))

(defun lit (value)
  (make-instance 'literal :value value))

(defun sql-raw (text)
  (check-type text string)
  (make-instance 'raw-sql :text text))

(defun sql-fragment (template &rest args)
  "Build a nestable raw SQL fragment. TEMPLATE uses ? placeholders; ARGS are bind params."
  (apply #'make-sql-fragment template args))

(defun %columnish (x)
  (or (keywordp x)
      (and (symbolp x) (not (member x '(t nil) :test #'eq)))))

(defun ensure-expr (x)
  "Coerce X to an sql-expr node (objects, (:op …) lists, keywords→cols, atoms→lits)."
  (cond
    ((typep x 'sql-node) x)
    ((and (consp x) (symbolp (car x))) (parse-expr x))
    ((eq x t) (lit t))
    ((eq x nil) (lit nil))
    ((%columnish x) (col x))
    (t (lit x))))

(defun parse-expr (form)
  "Parse a list expression such as (:and (:= :a 1) (:= :b 2))."
  (cond
    ((typep form 'sql-node) form)
    ((atom form) (ensure-expr form))
    (t
     (let* ((op (car form))
            (op-key (if (keywordp op)
                        op
                        (intern (symbol-name op) :keyword)))
            (args (cdr form)))
       (case op-key
         (:= (:= (first args) (second args)))
         (:!= (!= (first args) (second args)))
         (:<> (!= (first args) (second args)))
         (:< (:< (first args) (second args)))
         (:> (:> (first args) (second args)))
         (:<= (:<= (first args) (second args)))
         (:>= (:>= (first args) (second args)))
         (:and (apply #'sql-and args))
         (:or (apply #'sql-or args))
         (:not (sql-not (first args)))
         (:in (apply #'sql-in (first args) (rest args)))
         (:like (sql-like (first args) (second args)))
         (:is-null (sql-is-null (first args)))
         (:is-not-null (sql-is-not-null (first args)))
         (:between (sql-between (first args) (second args) (third args)))
         (:not-between (sql-between (first args) (second args) (third args) :not t))
         (:+ (:+ (first args) (second args)))
         (:- (:- (first args) (second args)))
         (:* (:* (first args) (second args)))
         (:/ (:/ (first args) (second args)))
         (:exists (exists (first args)))
         (:func (apply #'sql-func (first args) (rest args)))
         (:case (apply #'sql-case args))
         (:cast (sql-cast (first args) (second args)))
         (otherwise
          (%err "unknown expression operator ~s" op)))))))

(defun := (left right)
  (make-instance 'binary-op :op := :left (ensure-expr left) :right (ensure-expr right)))

(defun != (left right)
  (make-instance 'binary-op :op :!= :left (ensure-expr left) :right (ensure-expr right)))

(defun :< (left right)
  (make-instance 'binary-op :op :< :left (ensure-expr left) :right (ensure-expr right)))

(defun :> (left right)
  (make-instance 'binary-op :op :> :left (ensure-expr left) :right (ensure-expr right)))

(defun :<= (left right)
  (make-instance 'binary-op :op :<= :left (ensure-expr left) :right (ensure-expr right)))

(defun :>= (left right)
  (make-instance 'binary-op :op :>= :left (ensure-expr left) :right (ensure-expr right)))

(defun :+ (left right)
  (make-instance 'binary-op :op :+ :left (ensure-expr left) :right (ensure-expr right)))

(defun :- (left right)
  (make-instance 'binary-op :op :- :left (ensure-expr left) :right (ensure-expr right)))

(defun :* (left right)
  (make-instance 'binary-op :op :* :left (ensure-expr left) :right (ensure-expr right)))

(defun :/ (left right)
  (make-instance 'binary-op :op :/ :left (ensure-expr left) :right (ensure-expr right)))

(defun sql-and (&rest exprs)
  (make-instance 'nary-op :op :and :operands (mapcar #'ensure-expr exprs)))

(defun sql-or (&rest exprs)
  (make-instance 'nary-op :op :or :operands (mapcar #'ensure-expr exprs)))

(defun sql-not (expr)
  (make-instance 'unary-op :op :not :operand (ensure-expr expr)))

(defun sql-in (left &rest values)
  (make-instance 'in-op :left (ensure-expr left)
                        :values (mapcar #'ensure-expr values)))

(defun sql-like (left pattern)
  (make-instance 'like-op :left (ensure-expr left) :pattern (ensure-expr pattern)))

(defun sql-is-null (operand)
  (make-instance 'is-null-op :operand (ensure-expr operand)))

(defun sql-is-not-null (operand)
  (make-instance 'is-null-op :operand (ensure-expr operand) :not-p t))

(defun sql-between (operand low high &key not)
  (make-instance 'between-op
                 :operand (ensure-expr operand)
                 :low (ensure-expr low)
                 :high (ensure-expr high)
                 :not-p not))

(defun sql-case (&rest args)
  "Core case(): (sql-case cond1 val1 cond2 val2 :else default)."
  (let ((whens nil)
        (else nil)
        (rest args))
    (loop while rest
          do (let ((a (pop rest)))
               (cond
                 ((eq a :else) (setf else (ensure-expr (pop rest))))
                 (t (push (cons (ensure-expr a) (ensure-expr (pop rest))) whens)))))
    (make-instance 'case-expr :whens (nreverse whens) :else else)))

(defun sql-cast (expr type)
  (make-instance 'cast-expr :expr (ensure-expr expr) :type type))

(defun sql-func (name &rest args)
  (make-instance 'function-call
                 :name name
                 :args (mapcar (lambda (a)
                                 (if (eq a :*)
                                     (sql-raw "*")
                                     (ensure-expr a)))
                               args)))

(defun count (&optional (expr :*))
  (sql-func :count expr))

(defun coalesce (&rest args)
  (apply #'sql-func :coalesce args))

(defun exists (query)
  (make-instance 'exists-op :query query))

(defun subquery (query &optional alias)
  (make-instance 'subquery :query query :alias alias))

(defun label (expr name)
  (make-instance 'labeled-expr :expr (ensure-expr expr) :name name))

(defun bindparam (name &optional (value nil valuep))
  (make-instance 'bind-param :name name :value value :has-value valuep))

(defun as-cte (query name &key recursive)
  (make-instance 'cte-node :name name :query query :recursive recursive))

(defun make-sql-table (name &rest columns)
  (make-instance 'sql-table :name name :columns columns))

(defun table-column (name &rest keys &key &allow-other-keys)
  (apply #'column name keys))

(defun create-table-from (table &key if-not-exists)
  (check-type table sql-table)
  (apply #'create-table (sql-table-name table)
         (append (when if-not-exists (list :if-not-exists))
                 (sql-table-columns table))))

;;; ---------------------------------------------------------------------------
;;; Clause constructors
;;; ---------------------------------------------------------------------------

(defun columns (&rest items)
  (make-instance 'columns-clause
                 :items (mapcar (lambda (i)
                                  (if (typep i 'sql-node) i (ensure-expr i)))
                                items)))

(defun from (table &optional alias)
  (make-instance 'from-clause :table table :alias alias))

(defun where (expr)
  (make-instance 'where-clause :expr (ensure-expr expr)))

(defun on (expr)
  (make-instance 'on-clause :expr (ensure-expr expr)))

(defun %join (type table &rest rest)
  (let ((alias nil)
        (on-expr nil))
    (loop for x in rest
          do (cond
               ((typep x 'on-clause) (setf on-expr (on-expr x)))
               ((and (symbolp x) (not (typep x 'sql-node))) (setf alias x))
               (t (setf on-expr (ensure-expr x)))))
    (make-instance 'join-clause :type type :table table :alias alias :on on-expr)))

(defun join (table &rest rest)
  (apply #'%join :inner table rest))

(defun inner-join (table &rest rest)
  (apply #'%join :inner table rest))

(defun left-join (table &rest rest)
  (apply #'%join :left table rest))

(defun right-join (table &rest rest)
  (apply #'%join :right table rest))

(defun full-join (table &rest rest)
  (apply #'%join :full table rest))

(defun cross-join (table &rest rest)
  (apply #'%join :cross table rest))

(defun distinct (&rest on-exprs)
  (make-instance 'distinct-clause :on (mapcar #'ensure-expr on-exprs)))

(defun for-update (&key of nowait skip-locked)
  (make-instance 'for-update-clause
                 :of (when of (mapcar #'ensure-expr (if (listp of) of (list of))))
                 :nowait nowait
                 :skip-locked skip-locked))

(defun cte (name query &key recursive)
  (as-cte query name :recursive recursive))

(defun with-cte (&rest ctes)
  (make-instance 'with-cte-clause
                 :ctes (mapcar (lambda (c)
                                 (if (typep c 'cte-node) c
                                     (%err "with-cte expects cte-node, got ~s" c)))
                               ctes)))

(defun group-by (&rest items)
  (make-instance 'group-by-clause :items (mapcar #'ensure-expr items)))

(defun having (expr)
  (make-instance 'having-clause :expr (ensure-expr expr)))

(defun order-by (&rest items)
  "Items are column refs, or (col :asc|:desc)."
  (make-instance 'order-by-clause
                 :items (mapcar (lambda (i)
                                  (if (and (consp i) (not (typep i 'sql-node)))
                                      (list (ensure-expr (first i))
                                            (or (second i) :asc))
                                      (list (ensure-expr i) :asc)))
                                items)))

(defun limit (n)
  (make-instance 'limit-clause :count n))

(defun offset (n)
  (make-instance 'offset-clause :count n))

(defun sql-values (&rest cells)
  "Single VALUES row. For multi-row, pass lists: (sql-values '(1 \"a\") '(2 \"b\"))."
  (let ((rows (if (and cells (every #'listp cells) (not (typep (first cells) 'sql-node)))
                  cells
                  (list cells))))
    (make-instance 'values-clause
                   :rows (mapcar (lambda (row)
                                   (mapcar #'ensure-expr
                                           (if (listp row) row (list row))))
                                 rows))))

(defun sql-set (&rest assignments)
  "Assignments are binary-op from :=, or (:col value) pairs."
  (make-instance 'set-clause
                 :assignments
                 (mapcar (lambda (a)
                           (cond
                             ((typep a 'binary-op) a)
                             ((and (consp a) (= 2 (length a)))
                              (:= (first a) (second a)))
                             (t (%err "sql-set expects := or (col value), got ~s" a))))
                         assignments)))

(defun returning (&rest items)
  (make-instance 'returning-clause
                 :items (mapcar #'ensure-expr items)))

(defun column (name &key type primary-key autoincrement not-null unique default)
  (make-instance 'column-def
                 :name name
                 :type type
                 :primary-key primary-key
                 :autoincrement autoincrement
                 :not-null not-null
                 :unique unique
                 :default default))

(defun add-column (name &key type primary-key autoincrement not-null unique default)
  (make-instance 'add-column-clause
                 :column (column name :type type :primary-key primary-key
                                 :autoincrement autoincrement :not-null not-null
                                 :unique unique :default default)))

(defun drop-column (name)
  (make-instance 'drop-column-clause :name name))

(defun procedure-param (name type &key (mode :in))
  (make-instance 'procedure-param :name name :type type :mode mode))

(defun in (name type)
  (procedure-param name type :mode :in))

(defun out (name type)
  (procedure-param name type :mode :out))

(defun procedure-params (&rest params)
  (make-instance 'procedure-params-clause
                 :params (mapcar (lambda (p)
                                   (if (typep p 'procedure-param)
                                       p
                                       (%err "expected procedure-param, got ~s" p)))
                                 params)))

(defun params (&rest params)
  (apply #'procedure-params params))

(defun body (&rest forms)
  (make-instance 'body-clause
                 :forms (mapcar (lambda (f)
                                  (cond
                                    ((typep f 'sql-node) f)
                                    ((stringp f) (sql-raw f))
                                    (t (ensure-expr f))))
                                forms)))

;;; ---------------------------------------------------------------------------
;;; Statement constructors
;;; ---------------------------------------------------------------------------

(defun select (&rest clauses)
  (make-instance 'select-statement :clauses clauses))

(defun %compound (op &rest selects)
  (make-instance 'compound-select-statement :op op :selects selects))

(defun union (&rest selects) (apply #'%compound :union selects))
(defun union-all (&rest selects) (apply #'%compound :union-all selects))
(defun intersect (&rest selects) (apply #'%compound :intersect selects))
(defun intersect-all (&rest selects) (apply #'%compound :intersect-all selects))
(defun except (&rest selects) (apply #'%compound :except selects))
(defun except-all (&rest selects) (apply #'%compound :except-all selects))

(defun insert-into (table &rest clauses)
  (let ((normalized
          (mapcar (lambda (c)
                    (if (typep c 'select-statement)
                        (make-instance 'select-source-clause :select c)
                        c))
                  clauses)))
    (make-instance 'insert-statement :table table :clauses normalized)))

(defun update (table &rest clauses)
  (make-instance 'update-statement :table table :clauses clauses))

(defun delete-from (table &rest clauses)
  (make-instance 'delete-statement :table table :clauses clauses))

(defun create-table (table &rest args)
  (let ((if-not-exists nil)
        (cols nil))
    (loop for a in args
          do (cond
               ((eq a :if-not-exists) (setf if-not-exists t))
               ((and (keywordp a) (eq a :if-not-exists)) (setf if-not-exists t))
               ((typep a 'column-def) (push a cols))
               (t (%err "create-table: unexpected ~s" a))))
    (make-instance 'create-table-statement
                   :table table
                   :columns (nreverse cols)
                   :if-not-exists if-not-exists)))

(defun drop-table (table &key if-exists)
  (make-instance 'drop-table-statement :table table :if-exists if-exists))

(defun alter-table (table &rest actions)
  (make-instance 'alter-table-statement :table table :actions actions))

(defun create-index (name &rest args)
  (let ((table nil)
        (cols nil)
        (unique nil)
        (if-not-exists nil))
    (loop for a in args
          do (cond
               ((typep a 'from-clause) (setf table (from-table a)))
               ((typep a 'on-clause)
                ;; (on :users) misuse — prefer (from …); allow (on table) as table ref
                (let ((e (on-expr a)))
                  (when (typep e 'column-ref)
                    (setf table (column-ref-name e)))))
               ((and (consp a) (eq (car a) :on))
                (setf table (second a)))
               ((typep a 'columns-clause) (setf cols (columns-items a)))
               ((eq a :unique) (setf unique t))
               ((eq a :if-not-exists) (setf if-not-exists t))
               ((keywordp a) nil)
               ((and (symbolp a) (not (typep a 'sql-node))) (setf table a))
               (t (%err "create-index: unexpected ~s" a))))
    ;; Support brief: (create-index :users-email (on :users) (columns :email))
    ;; where ON was used as table pointer — re-scan
    (unless table
      (dolist (a args)
        (when (typep a 'on-clause)
          (let ((e (on-expr a)))
            (setf table (if (typep e 'column-ref)
                            (column-ref-name e)
                            e))))))
    (make-instance 'create-index-statement
                   :name name
                   :table table
                   :columns cols
                   :unique unique
                   :if-not-exists if-not-exists)))

(defun drop-index (name &key if-exists)
  (make-instance 'drop-index-statement :name name :if-exists if-exists))

(defun create-procedure (name &rest args)
  (let ((params nil)
        (body-forms nil)
        (or-replace nil)
        (language :plpgsql))
    (loop for a in args
          do (cond
               ((typep a 'procedure-params-clause)
                (setf params (procedure-params-list a)))
               ((typep a 'body-clause)
                (setf body-forms (body-forms a)))
               ((eq a :or-replace) (setf or-replace t))
               ((eq a :language) nil)
               ((and (keywordp a) nil))
               (t (%err "create-procedure: unexpected ~s" a))))
    (make-instance 'create-procedure-statement
                   :name name
                   :params params
                   :body body-forms
                   :language language
                   :or-replace or-replace)))

(defun sql-call (name &rest args)
  "CALL procedure. Args are values or (:= :name value) named pairs."
  (make-instance 'call-statement :name name :args args))

;;; Convenience: (on :table) for create-index table binding via keyword table name

(defun %on-table (table)
  "Internal: treat (on :users) in create-index as table specifier."
  (make-instance 'on-clause :expr (col table)))
