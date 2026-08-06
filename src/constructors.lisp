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

(defun lit (value &optional type)
  "SQL literal VALUE (always inlined). Optional TYPE runs encode + typed literal emit.
Use BINDPARAM when you need a placeholder."
  (make-instance 'literal :value value :sql-type type))

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
          (or (parse-registered-op op-key args)
              (%err "unknown expression operator ~s" op))))))))

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

(defun sql-like (left pattern &key escape not)
  (make-instance 'like-op
                 :left (ensure-expr left)
                 :pattern (ensure-expr pattern)
                 :escape (when escape (ensure-expr escape))
                 :not-p not))

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

(defun array-lit (&rest items)
  "ARRAY[e1, e2, …] literal (postgres / dialects that support array constructors)."
  (make-instance 'array-literal :items (mapcar #'ensure-expr items)))

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

(defun bindparam (name &rest args)
  "Explicit placeholder (? / $n) for prepare/execute.

  (bindparam :x)                     ; params get :x (name sentinel)
  (bindparam :x 1)                   ; or :value 1
  (bindparam :x :default 10)         ; params get 10 when value omitted
  (bindparam :x v :default 10 :type :integer)  ; value wins over default

:default is NOT inlined into SQL — it only supplies the bound value passed to
the driver when :value is absent. :type encodes that payload."
  (let ((value nil) (valuep nil) (type nil)
        (default nil) (defaultp nil))
    (when (and args (not (keywordp (first args))))
      (setf value (pop args) valuep t))
    (loop for (k v) on args by #'cddr
          do (ecase k
               (:value (setf value v valuep t))
               (:type (setf type v))
               (:default (setf default v defaultp t))))
    (make-instance 'bind-param
                   :name name
                   :value value
                   :has-value valuep
                   :sql-type type
                   :default default
                   :has-default defaultp)))

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
        (on-expr nil)
        (using-cols nil)
        (natural nil))
    (loop for x in rest
          do (cond
               ((eq x :natural) (setf natural t))
               ((typep x 'on-clause) (setf on-expr (on-expr x)))
               ((typep x 'using-clause) (setf using-cols (using-columns x)))
               ((and (symbolp x) (not (typep x 'sql-node)) (not (keywordp x)))
                (setf alias x))
               ((keywordp x) (setf alias x))
               (t (setf on-expr (ensure-expr x)))))
    (make-instance 'join-clause :type type :table table :alias alias
                               :on on-expr :using using-cols :natural natural)))

(defun using (&rest columns)
  (make-instance 'using-clause :columns (mapcar #'ensure-expr columns)))

(defun natural-join (table &rest rest)
  (apply #'%join :inner table :natural rest))

(defun natural-left-join (table &rest rest)
  (apply #'%join :left table :natural rest))

(defun natural-right-join (table &rest rest)
  (apply #'%join :right table :natural rest))

(defun natural-full-join (table &rest rest)
  (apply #'%join :full table :natural rest))

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

(defun %normalize-order-item (i)
  "Return (list expr dir nulls). NULLS is NIL, :NULLS-FIRST, or :NULLS-LAST.
  Items: column | (col :asc|:desc) | (col :asc|:desc :nulls-first|:nulls-last)."
  (if (and (consp i) (not (typep i 'sql-node)))
      (let ((expr (ensure-expr (first i)))
            (dir :asc)
            (nulls nil))
        (dolist (x (rest i))
          (cond
            ((member x '(:asc :desc) :test #'eq) (setf dir x))
            ((member x '(:nulls-first :nulls-last) :test #'eq) (setf nulls x))
            (t (%err "bad order-by option ~s in ~s" x i))))
        (list expr dir nulls))
      (list (ensure-expr i) :asc nil)))

(defun order-by (&rest items)
  "Items are column refs, or (col :asc|:desc [:nulls-first|:nulls-last])."
  (make-instance 'order-by-clause
                 :items (mapcar #'%normalize-order-item items)))

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

(defun inout (name type)
  (procedure-param name type :mode :inout))

(defun procedure-params (&rest params)
  (make-instance 'procedure-params-clause
                 :params (mapcar (lambda (p)
                                   (if (typep p 'procedure-param)
                                       p
                                       (%err "expected procedure-param, got ~s" p)))
                                 params)))

(defun params (&rest params)
  (apply #'procedure-params params))

;;; BODY is defined in procedure.lisp (lispy IF/LET/SETF/LOOP expander).

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
                    (cond
                      ((typep c 'select-statement)
                       (make-instance 'select-source-clause :select c))
                      ((typep c 'values-selectable)
                       (make-instance 'select-source-clause :select c))
                      (t c)))
                  clauses)))
    (make-instance 'insert-statement :table table :clauses normalized)))

(defun update (table &rest clauses)
  (make-instance 'update-statement :table table :clauses clauses))

(defun delete-from (table &rest clauses)
  (make-instance 'delete-statement :table table :clauses clauses))

(defun create-table (table &rest args)
  "CREATE TABLE.

  Keywords: :IF-NOT-EXISTS, :TEMPORARY, :ON-COMMIT (:preserve|:delete),
  :OF type (typed table), :EXTRAS (list of extension nodes).
  Also accepts COLUMN-DEF, TABLE-CONSTRAINT, and any SQL-NODE as an open extension
  (stored in CREATE-TABLE-EXTRAS — dialects emit via EMIT-CREATE-TABLE-EXTRA)."
  (let ((if-not-exists nil)
        (temporary nil)
        (on-commit nil)
        (of-type nil)
        (cols nil)
        (constraints nil)
        (extras nil)
        (rest args))
    (loop while rest
          do (let ((a (pop rest)))
               (cond
                 ((eq a :if-not-exists) (setf if-not-exists t))
                 ((eq a :temporary) (setf temporary t))
                 ((eq a :on-commit)
                  (setf on-commit (pop rest))
                  (unless (member on-commit '(:preserve :delete))
                    (%err "create-table :on-commit expects :preserve or :delete, got ~s"
                          on-commit)))
                 ((eq a :of)
                  (setf of-type (pop rest))
                  (unless of-type (%err "create-table :of needs a type name")))
                 ((eq a :extras)
                  (let ((xs (pop rest)))
                    (setf extras (append extras (if (listp xs) xs (list xs))))))
                 ((and (consp a) (eq (first a) :of))
                  (setf of-type (second a)))
                 ((typep a 'column-def) (push a cols))
                 ((typep a 'table-constraint) (push a constraints))
                 ((typep a 'sql-node) (push a extras))
                 (t (%err "create-table: unexpected ~s" a)))))
    (make-instance 'create-table-statement
                   :table table
                   :columns (nreverse cols)
                   :constraints (nreverse constraints)
                   :of-type of-type
                   :extras (nreverse extras)
                   :temporary temporary
                   :on-commit on-commit
                   :if-not-exists if-not-exists)))

(defun drop-table (table &key if-exists)
  (make-instance 'drop-table-statement :table table :if-exists if-exists))

(defun alter-table (table &rest actions)
  "ALTER TABLE. ACTIONS are clause nodes — core or dialect extensions.
  Emission is open via EMIT-ALTER-TABLE-ACTION (no closed typecase).
  Nested lists (e.g. from ALTER-COLUMN) are flattened one level."
  (make-instance 'alter-table-statement
                 :table table
                 :actions (loop for a in actions
                                append (if (and (listp a) (not (typep a 'sql-node)))
                                           a
                                           (list a)))))

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

;;; ---- ANSI Foundation constructors ----

(defun is-distinct-from (left right)
  (make-instance 'is-distinct-from-op
                 :left (ensure-expr left) :right (ensure-expr right)))

(defun is-not-distinct-from (left right)
  (make-instance 'is-distinct-from-op
                 :left (ensure-expr left) :right (ensure-expr right) :not-p t))

(defun similar-to (left pattern &key escape not)
  (make-instance 'similar-to-op
                 :left (ensure-expr left)
                 :pattern (ensure-expr pattern)
                 :escape (when escape (ensure-expr escape))
                 :not-p not))

(defun sql-any (left op subquery)
  (make-instance 'quantified-op :op op :quantifier :any
                 :left (ensure-expr left) :subquery subquery))

(defun sql-all (left op subquery)
  (make-instance 'quantified-op :op op :quantifier :all
                 :left (ensure-expr left) :subquery subquery))

(defun sql-some (left op subquery)
  (make-instance 'quantified-op :op op :quantifier :some
                 :left (ensure-expr left) :subquery subquery))

(defun unique (query)
  (make-instance 'unique-pred :query query))

(defun collate (expr collation)
  (make-instance 'collate-expr :expr (ensure-expr expr) :collation collation))

(defun nullif (left right)
  (make-instance 'nullif-expr :left (ensure-expr left) :right (ensure-expr right)))

(defun over (expr &key partition-by order-by frame)
  (make-instance 'over-expr
                 :expr (ensure-expr expr)
                 :window (make-instance 'window-spec
                                       :partition-by (mapcar #'ensure-expr (or partition-by nil))
                                       :order-by (mapcar #'%normalize-order-item (or order-by nil))
                                       :frame frame)))

(defun rows-frame (start &optional end)
  (make-instance 'window-frame :unit :rows :start start :end end))

(defun range-frame (start &optional end)
  (make-instance 'window-frame :unit :range :start start :end end))

(defun rollup (&rest items)
  (make-instance 'grouping-expr :kind :rollup :items (mapcar #'ensure-expr items)))

(defun cube (&rest items)
  (make-instance 'grouping-expr :kind :cube :items (mapcar #'ensure-expr items)))

(defun grouping-sets (&rest sets)
  (make-instance 'grouping-expr :kind :grouping-sets
                 :items (mapcar (lambda (s)
                                  (mapcar #'ensure-expr (if (listp s) s (list s))))
                                sets)))

(defun %constraint-option-keys ()
  '(:name :deferrable :initially))

(defun %take-constraint-options (args)
  "Split ARGS into positional items and a plist of :NAME/:DEFERRABLE/:INITIALLY."
  (let ((positional nil)
        (opts nil)
        (rest args))
    (loop while rest
          do (let ((a (car rest)))
               (if (and (keywordp a)
                        (member a (%constraint-option-keys) :test #'eq)
                        (cdr rest))
                   (progn
                     (push (cadr rest) opts)
                     (push a opts)
                     (setf rest (cddr rest)))
                   (progn
                     (push a positional)
                     (setf rest (cdr rest))))))
    (values (nreverse positional) opts)))

(defun primary-key (&rest args)
  "PRIMARY KEY (cols…). Optional trailing :NAME / :DEFERRABLE / :INITIALLY."
  (multiple-value-bind (cols opts) (%take-constraint-options args)
    (make-instance 'primary-key-constraint
                   :columns (mapcar #'ensure-expr cols)
                   :name (getf opts :name)
                   :deferrable (getf opts :deferrable)
                   :initially (getf opts :initially))))

(defun unique-key (&rest args)
  "UNIQUE (cols…). Optional trailing :NAME / :DEFERRABLE / :INITIALLY."
  (multiple-value-bind (cols opts) (%take-constraint-options args)
    (make-instance 'unique-constraint
                   :columns (mapcar #'ensure-expr cols)
                   :name (getf opts :name)
                   :deferrable (getf opts :deferrable)
                   :initially (getf opts :initially))))

(defun check (expr &key name deferrable initially)
  (make-instance 'check-constraint
                 :name name
                 :expr (ensure-expr expr)
                 :deferrable deferrable
                 :initially initially))

(defun foreign-key (columns &key references on-delete on-update match name
                              deferrable initially)
  (destructuring-bind (ref-table &rest ref-cols) (if (listp references) references (list references))
    (make-instance 'foreign-key-constraint
                   :name name
                   :columns (mapcar #'ensure-expr (if (listp columns) columns (list columns)))
                   :ref-table ref-table
                   :ref-columns (mapcar #'ensure-expr ref-cols)
                   :on-delete on-delete
                   :on-update on-update
                   :match match
                   :deferrable deferrable
                   :initially initially)))

(defun add-constraint (constraint)
  (make-instance 'add-constraint-clause :constraint constraint))

(defun drop-constraint (name)
  (make-instance 'drop-constraint-clause :name name))

(defun rename-column (old new)
  (make-instance 'rename-column-clause :old old :new new))

(defun rename-to (new)
  (make-instance 'rename-table-clause :new new))

(defun values-row (&rest rows)
  "ANSI VALUES selectable. Rows are lists of cells."
  (let ((normalized (if (and rows (every #'listp rows))
                        rows
                        (list rows))))
    (make-instance 'values-selectable
                   :rows (mapcar (lambda (row)
                                   (mapcar #'ensure-expr (if (listp row) row (list row))))
                                 normalized))))

(defun default-values ()
  (make-instance 'default-values-clause))

(defun create-view (name query &key columns or-replace recursive temporary check-option)
  (when check-option
    (unless (or (eq check-option t)
                (member check-option '(:cascaded :local)))
      (%err "create-view :check-option expects T, :cascaded, or :local, got ~s"
            check-option)))
  (make-instance 'create-view-statement
                 :name name :query query :columns columns
                 :or-replace or-replace :recursive recursive
                 :temporary temporary
                 :check-option check-option))

(defun drop-view (name &key if-exists cascade)
  (make-instance 'drop-view-statement :name name :if-exists if-exists :cascade cascade))

(defun create-schema (name &key authorization if-not-exists)
  (make-instance 'create-schema-statement
                 :name name :authorization authorization :if-not-exists if-not-exists))

(defun drop-schema (name &key if-exists cascade)
  (make-instance 'drop-schema-statement :name name :if-exists if-exists :cascade cascade))

(defun create-sequence (name &key start increment minvalue maxvalue cycle if-not-exists)
  (make-instance 'create-sequence-statement
                 :name name :start start :increment increment
                 :minvalue minvalue :maxvalue maxvalue :cycle cycle
                 :if-not-exists if-not-exists))

(defun drop-sequence (name &key if-exists)
  (make-instance 'drop-sequence-statement :name name :if-exists if-exists))

(defun truncate-table (tables &key identity cascade)
  (make-instance 'truncate-statement
                 :tables (if (listp tables) tables (list tables))
                 :identity identity
                 :cascade cascade))

(defun merge-into (table &key using on when-matched when-not-matched)
  (make-instance 'merge-statement
                 :table table
                 :using using
                 :on (ensure-expr on)
                 :when-matched when-matched
                 :when-not-matched when-not-matched))

(defun merge-update (assignments &key where)
  (make-instance 'merge-update-action
                 :assignments (mapcar (lambda (a)
                                        (if (typep a 'binary-op) a
                                            (%err "merge-update expects :=")))
                                      assignments)
                 :where (when where (ensure-expr where))))

(defun merge-delete (&key where)
  (make-instance 'merge-delete-action
                 :where (when where (ensure-expr where))))

(defun merge-insert (values &key columns)
  (make-instance 'merge-insert-action
                 :columns (mapcar #'ensure-expr (or columns nil))
                 :values (mapcar #'ensure-expr values)))

;;; ---------------------------------------------------------------------------
;;; Types / domains
;;; ---------------------------------------------------------------------------

(defun type-attribute (name type)
  "Attribute of a structured UDT: NAME with TYPE (keyword/spec)."
  (make-instance 'type-attribute :name name :type type))

(defun %normalize-type-attributes (attrs)
  (mapcar (lambda (a)
            (cond
              ((typep a 'type-attribute) a)
              ((and (consp a) (= 2 (length a)))
               (type-attribute (first a) (second a)))
              (t (%err "type attribute expects type-attribute or (name type), got ~s" a))))
          attrs))

(defun create-type (name &key as attributes enum if-not-exists kind base-options
                           &allow-other-keys)
  "CREATE TYPE.

  Distinct (ANSI):   (create-type :euros :as :numeric)
  Structured (ANSI): (create-type :addr :attributes '((:city :text) …))
  Enum (postgres):   (create-type :mood :enum '(\"sad\" \"ok\" \"happy\"))
  Base (postgres):   (create-type :complex :kind :base :base-options '(:input … :output …))

  Vendor kinds beyond :distinct/:structured/:enum/:base are allowed — emission is
  open via EMIT-CREATE-TYPE-KIND. Exactly one shape among :AS / :ATTRIBUTES / :ENUM
  / explicit :KIND."
  (let* ((inferred (cond (as :distinct)
                         (attributes :structured)
                         (enum :enum)
                         (kind kind)
                         (t nil)))
         (kind (or kind inferred)))
    (unless kind
      (%err "create-type requires :as, :attributes, :enum, or :kind"))
    (when (and as attributes)
      (%err "create-type: :as and :attributes are mutually exclusive"))
    (make-instance 'create-type-statement
                   :name name
                   :kind kind
                   :base-type as
                   :attributes (when attributes (%normalize-type-attributes attributes))
                   :enum-labels (when enum
                                  (mapcar (lambda (l)
                                            (if (stringp l) l (string l)))
                                          enum))
                   :base-options base-options
                   :if-not-exists if-not-exists)))

(defun drop-type (name &key if-exists cascade)
  (make-instance 'drop-type-statement
                 :name name :if-exists if-exists :cascade cascade))

(defun add-attribute (name type)
  (make-instance 'add-attribute-clause
                 :attribute (type-attribute name type)))

(defun drop-attribute (name)
  (make-instance 'drop-attribute-clause :name name))

(defun rename-attribute (old new)
  (make-instance 'rename-attribute-clause :old old :new new))

(defun add-enum-value (label &key before after if-not-exists)
  "Postgres ALTER TYPE … ADD VALUE (rejected on ANSI)."
  (make-instance 'add-enum-value-clause
                 :label (if (stringp label) label (string label))
                 :before before :after after
                 :if-not-exists if-not-exists))

(defun alter-type (name &rest actions)
  "ALTER TYPE. ACTIONS are open clause nodes; emit via EMIT-ALTER-TYPE-ACTION."
  (make-instance 'alter-type-statement :name name :actions actions))

(defun create-domain (name &key as default check not-null if-not-exists)
  "CREATE DOMAIN name AS type [DEFAULT …] [NOT NULL] [CHECK (…)]."
  (unless as
    (%err "create-domain requires :as base type"))
  (make-instance 'create-domain-statement
                 :name name
                 :base-type as
                 :default default
                 :check (when check (ensure-expr check))
                 :not-null not-null
                 :if-not-exists if-not-exists))

(defun drop-domain (name &key if-exists cascade)
  (make-instance 'drop-domain-statement
                 :name name :if-exists if-exists :cascade cascade))

(defun alter-domain (name &rest actions)
  "ALTER DOMAIN. ACTIONS are open clause nodes; emit via EMIT-ALTER-DOMAIN-ACTION.
  Reuses SET/DROP DEFAULT, SET/DROP NOT NULL, ADD/DROP CONSTRAINT (column slot NIL)."
  (make-instance 'alter-domain-statement :name name :actions actions))

(defun set-default (value &key column)
  "SET DEFAULT. With :COLUMN → ALTER COLUMN … SET DEFAULT; else domain form."
  (make-instance 'set-default-clause :value value :column column))

(defun drop-default (&key column)
  (make-instance 'drop-default-clause :column column))

(defun set-not-null (&key column)
  (make-instance 'set-not-null-clause :column column))

(defun drop-not-null (&key column)
  (make-instance 'drop-not-null-clause :column column))

(defun set-data-type (column type &key using)
  "ALTER COLUMN … SET DATA TYPE / TYPE."
  (make-instance 'set-data-type-clause :column column :type type :using using))

(defun alter-column (column &rest actions)
  "Stamp COLUMN onto SET/DROP DEFAULT|NOT NULL actions for ALTER TABLE.
  Returns a list of clauses (flattened by ALTER-TABLE), e.g.:

    (alter-table :t (alter-column :x (set-default 1) (set-not-null)))
    (alter-table :t (set-data-type :x :integer))"
  (mapcar (lambda (a)
            (cond
              ((typep a 'set-data-type-clause)
               (make-instance 'set-data-type-clause
                              :column column
                              :type (set-data-type-type a)
                              :using (set-data-type-using a)))
              ((typep a 'set-default-clause)
               (make-instance 'set-default-clause
                              :value (set-default-value a)
                              :column column))
              ((typep a 'drop-default-clause)
               (make-instance 'drop-default-clause :column column))
              ((typep a 'set-not-null-clause)
               (make-instance 'set-not-null-clause :column column))
              ((typep a 'drop-not-null-clause)
               (make-instance 'drop-not-null-clause :column column))
              (t (%err "alter-column: unexpected action ~s" a))))
          actions))

(defun create-cast (source target &key with-function without-function with-inout as)
  "CREATE CAST (source AS target) …"
  (when (< 1 (+ (if with-function 1 0)
                (if without-function 1 0)
                (if with-inout 1 0)))
    (%err "create-cast: at most one of :with-function / :without-function / :with-inout"))
  (when as
    (unless (member as '(:assignment :implicit))
      (%err "create-cast :as expects :assignment or :implicit, got ~s" as)))
  (make-instance 'create-cast-statement
                 :source source :target target
                 :with-function with-function
                 :without-function without-function
                 :with-inout with-inout
                 :as as))

(defun drop-cast (source target &key if-exists)
  (make-instance 'drop-cast-statement
                 :source source :target target :if-exists if-exists))

(defun create-function (name &rest args)
  "CREATE FUNCTION (ANSI SQL/PSM–ish).

  Accepts PROCEDURE-PARAMS-CLAUSE / BODY-CLAUSE, plus keywords
  :RETURNS, :LANGUAGE, :OR-REPLACE, :DETERMINISTIC, :BODY, :PARAMS."
  (let ((params nil)
        (body-forms nil)
        (returns nil)
        (language :sql)
        (or-replace nil)
        (deterministic nil)
        (rest args))
    (loop while rest
          do (let ((a (pop rest)))
               (cond
                 ((typep a 'procedure-params-clause)
                  (setf params (procedure-params-list a)))
                 ((typep a 'body-clause)
                  (setf body-forms (body-forms a)))
                 ((eq a :params)
                  (let ((ps (pop rest)))
                    (setf params (mapcar (lambda (p)
                                           (if (typep p 'procedure-param) p
                                               (%err "expected procedure-param, got ~s" p)))
                                         (if (listp ps) ps (list ps))))))
                 ((eq a :body)
                  (let ((b (pop rest)))
                    (setf body-forms (if (listp b) b (list b)))))
                 ((eq a :returns) (setf returns (pop rest)))
                 ((eq a :language) (setf language (pop rest)))
                 ((eq a :or-replace) (setf or-replace t))
                 ((eq a :deterministic) (setf deterministic t))
                 (t (%err "create-function: unexpected ~s" a)))))
    (make-instance 'create-function-statement
                   :name name
                   :params params
                   :returns returns
                   :body body-forms
                   :language language
                   :or-replace or-replace
                   :deterministic deterministic)))

(defun drop-function (name &key if-exists cascade)
  (make-instance 'drop-function-statement
                 :name name :if-exists if-exists :cascade cascade))

(defun create-trigger (name &key timing events table for-each condition
                              function function-args body)
  "CREATE TRIGGER. TIMING is :before|:after|:instead-of; EVENTS list of
  :insert/:update/:delete. BODY is a statement list or raw node; FUNCTION is
  EXECUTE PROCEDURE/FUNCTION name (vendor)."
  (unless timing
    (%err "create-trigger requires :timing"))
  (unless events
    (%err "create-trigger requires :events"))
  (unless table
    (%err "create-trigger requires :table"))
  (unless (member timing '(:before :after :instead-of))
    (%err "create-trigger :timing expects :before/:after/:instead-of, got ~s" timing))
  (make-instance 'create-trigger-statement
                 :name name
                 :timing timing
                 :events (if (listp events) events (list events))
                 :table table
                 :for-each (or for-each :statement)
                 :condition (when condition (ensure-expr condition))
                 :function function
                 :function-args function-args
                 :body (cond
                         ((null body) nil)
                         ((typep body 'body-clause) (body-forms body))
                         ((and (listp body) (not (typep body 'sql-node))) body)
                         (t (list body)))))

(defun drop-trigger (name &key table if-exists cascade)
  (make-instance 'drop-trigger-statement
                 :name name :table table :if-exists if-exists :cascade cascade))

(defun grant (privileges &key on on-kind to with-grant-option)
  "GRANT privileges ON object TO grantee.
  PRIVILEGES is T/:all or a list of privilege keywords/strings."
  (unless on (%err "grant requires :on"))
  (unless to (%err "grant requires :to"))
  (make-instance 'grant-statement
                 :privileges privileges
                 :on on
                 :on-kind (or on-kind :table)
                 :to to
                 :with-grant-option with-grant-option))

(defun revoke (privileges &key on on-kind from cascade grant-option-for)
  "REVOKE privileges ON object FROM grantee."
  (unless on (%err "revoke requires :on"))
  (unless from (%err "revoke requires :from"))
  (make-instance 'revoke-statement
                 :privileges privileges
                 :on on
                 :on-kind (or on-kind :table)
                 :from from
                 :cascade cascade
                 :grant-option-for grant-option-for))

(defun comment-on (kind name comment)
  "COMMENT ON kind name IS '…'. For :column, NAME is (table column)."
  (make-instance 'comment-on-statement
                 :kind kind :name name :comment comment))

(defun lateral (query &optional alias)
  (make-instance 'lateral-subquery :query query :alias alias))

;;; ---------------------------------------------------------------------------
;;; CREATE TABLE AS + transaction control
;;; ---------------------------------------------------------------------------

(defun create-table-as (name query &key temporary if-not-exists columns)
  "CREATE [TEMPORARY] TABLE name [(cols)] AS <query>."
  (make-instance 'create-table-as-statement
                 :table name
                 :query query
                 :temporary temporary
                 :if-not-exists if-not-exists
                 :columns (when columns
                            (mapcar #'ensure-expr
                                    (if (listp columns) columns (list columns))))))

(defun %check-transaction-characteristics (isolation access-mode deferrable)
  (when isolation
    (unless (member isolation '(:read-uncommitted :read-committed
                                :repeatable-read :serializable))
      (%err "bad transaction isolation ~s" isolation)))
  (when access-mode
    (unless (member access-mode '(:read-only :read-write))
      (%err "bad transaction access-mode ~s" access-mode)))
  (when deferrable
    (unless (or (eq deferrable t) (eq deferrable :not))
      (%err "bad transaction deferrable ~s (expect T or :not)" deferrable))))

(defun start-transaction (&key isolation access-mode deferrable)
  "START TRANSACTION [ISOLATION LEVEL …] [READ ONLY|WRITE] [DEFERRABLE]."
  (%check-transaction-characteristics isolation access-mode deferrable)
  (make-instance 'start-transaction-statement
                 :isolation isolation
                 :access-mode access-mode
                 :deferrable deferrable))

(defun set-transaction (&key isolation access-mode deferrable)
  "SET TRANSACTION [ISOLATION LEVEL …] [READ ONLY|WRITE] [DEFERRABLE]."
  (%check-transaction-characteristics isolation access-mode deferrable)
  (make-instance 'set-transaction-statement
                 :isolation isolation
                 :access-mode access-mode
                 :deferrable deferrable))

(defun sql-commit ()
  "COMMIT."
  (make-instance 'commit-statement))

(defun sql-rollback (&key to)
  "ROLLBACK [TO SAVEPOINT name]."
  (make-instance 'rollback-statement :savepoint to))

(defun sql-savepoint (name)
  "SAVEPOINT name."
  (make-instance 'savepoint-statement :name name))

(defun sql-release-savepoint (name)
  "RELEASE SAVEPOINT name."
  (make-instance 'release-savepoint-statement :name name))
