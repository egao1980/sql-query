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
  (let ((if-not-exists nil)
        (cols nil)
        (constraints nil))
    (loop for a in args
          do (cond
               ((eq a :if-not-exists) (setf if-not-exists t))
               ((typep a 'column-def) (push a cols))
               ((typep a 'table-constraint) (push a constraints))
               (t (%err "create-table: unexpected ~s" a))))
    (make-instance 'create-table-statement
                   :table table
                   :columns (nreverse cols)
                   :constraints (nreverse constraints)
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
                                       :order-by (mapcar (lambda (i)
                                                          (if (and (consp i) (not (typep i 'sql-node)))
                                                              (list (ensure-expr (first i)) (or (second i) :asc))
                                                              (list (ensure-expr i) :asc)))
                                                        (or order-by nil))
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

(defun primary-key (&rest columns)
  (make-instance 'primary-key-constraint :columns (mapcar #'ensure-expr columns)))

(defun unique-key (&rest columns)
  (make-instance 'unique-constraint :columns (mapcar #'ensure-expr columns)))

(defun check (expr &key name)
  (make-instance 'check-constraint :name name :expr (ensure-expr expr)))

(defun foreign-key (columns &key references on-delete on-update match name)
  (destructuring-bind (ref-table &rest ref-cols) (if (listp references) references (list references))
    (make-instance 'foreign-key-constraint
                   :name name
                   :columns (mapcar #'ensure-expr (if (listp columns) columns (list columns)))
                   :ref-table ref-table
                   :ref-columns (mapcar #'ensure-expr ref-cols)
                   :on-delete on-delete
                   :on-update on-update
                   :match match)))

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

(defun create-view (name query &key columns or-replace recursive)
  (make-instance 'create-view-statement
                 :name name :query query :columns columns
                 :or-replace or-replace :recursive recursive))

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

(defun lateral (query &optional alias)
  (make-instance 'lateral-subquery :query query :alias alias))
