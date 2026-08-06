(in-package #:sql-query)

;;; ---------------------------------------------------------------------------
;;; AST nodes
;;; ---------------------------------------------------------------------------

(defclass sql-node () ()
  (:documentation "Base class for sql-query AST nodes."))

(defclass sql-statement (sql-node) ()
  (:documentation "Top-level SQL statement node."))

(defclass sql-fragment (sql-node)
  ((template :initarg :template :reader sql-fragment-template)
   (args :initarg :args :reader sql-fragment-args :initform nil))
  (:documentation "Raw SQL fragment with ? placeholders and bound args."))

(defun make-sql-fragment (template &rest args)
  (check-type template string)
  (make-instance 'sql-fragment :template template :args args))

;;; expressions

(defclass sql-expr (sql-node) ())

(defclass column-ref (sql-expr)
  ((name :initarg :name :reader column-ref-name)
   (table :initarg :table :reader column-ref-table :initform nil)))

(defclass literal (sql-expr)
  ((value :initarg :value :reader literal-value)
   (sql-type :initarg :sql-type :reader literal-sql-type :initform nil
             :documentation "Optional registered type — encode + emit as SQL literal text.")))

(defclass typed-value (sql-expr)
  ((value :initarg :value :reader typed-value-value)
   (sql-type :initarg :sql-type :reader typed-value-sql-type))
  (:documentation "Lisp value tagged with a registered SQL type; type adapter writes the expression."))

(defclass binary-op (sql-expr)
  ((op :initarg :op :reader binary-op-op)
   (left :initarg :left :reader binary-op-left)
   (right :initarg :right :reader binary-op-right)))

(defclass unary-op (sql-expr)
  ((op :initarg :op :reader unary-op-op)
   (operand :initarg :operand :reader unary-op-operand)))

(defclass nary-op (sql-expr)
  ((op :initarg :op :reader nary-op-op)
   (operands :initarg :operands :reader nary-op-operands)))

(defclass in-op (sql-expr)
  ((left :initarg :left :reader in-op-left)
   (values :initarg :values :reader in-op-values)
   (not-p :initarg :not-p :reader in-op-not-p :initform nil)))

(defclass like-op (sql-expr)
  ((left :initarg :left :reader like-op-left)
   (pattern :initarg :pattern :reader like-op-pattern)
   (not-p :initarg :not-p :reader like-op-not-p :initform nil)))

(defclass is-null-op (sql-expr)
  ((operand :initarg :operand :reader is-null-op-operand)
   (not-p :initarg :not-p :reader is-null-op-not-p :initform nil)))

(defclass raw-sql (sql-expr)
  ((text :initarg :text :reader raw-sql-text)))

(defclass between-op (sql-expr)
  ((operand :initarg :operand :reader between-operand)
   (low :initarg :low :reader between-low)
   (high :initarg :high :reader between-high)
   (not-p :initarg :not-p :reader between-not-p :initform nil)))

(defclass case-expr (sql-expr)
  ((whens :initarg :whens :reader case-whens) ; list of (condition . result)
   (else :initarg :else :reader case-else :initform nil)))

(defclass cast-expr (sql-expr)
  ((expr :initarg :expr :reader cast-expr-of)
   (type :initarg :type :reader cast-type)))

(defclass function-call (sql-expr)
  ((name :initarg :name :reader function-call-name)
   (args :initarg :args :reader function-call-args :initform nil)))

(defclass exists-op (sql-expr)
  ((query :initarg :query :reader exists-query)))

(defclass labeled-expr (sql-expr)
  ((expr :initarg :expr :reader labeled-expr-of)
   (name :initarg :name :reader labeled-name)))

(defclass bind-param (sql-expr)
  ((name :initarg :name :reader bind-param-name)
   (value :initarg :value :reader bind-param-value :initform nil)
   (has-value :initarg :has-value :reader bind-param-has-value :initform nil)
   (sql-type :initarg :sql-type :reader bind-param-sql-type :initform nil)))

(defclass subquery (sql-expr)
  ((query :initarg :query :reader subquery-query)
   (alias :initarg :alias :reader subquery-alias :initform nil)))

(defclass lateral-subquery (subquery) ())

(defclass cte-node (sql-node)
  ((name :initarg :name :reader cte-name)
   (query :initarg :query :reader cte-query)
   (recursive :initarg :recursive :reader cte-recursive :initform nil)))

;;; clauses

(defclass sql-clause (sql-node) ())

(defclass columns-clause (sql-clause)
  ((items :initarg :items :reader columns-items)))

(defclass from-clause (sql-clause)
  ((table :initarg :table :reader from-table)
   (alias :initarg :alias :reader from-alias :initform nil)))

(defclass where-clause (sql-clause)
  ((expr :initarg :expr :reader where-expr)))

(defclass join-clause (sql-clause)
  ((type :initarg :type :reader join-type) ; :inner :left :right :full :cross :natural-* 
   (table :initarg :table :reader join-table)
   (alias :initarg :alias :reader join-alias :initform nil)
   (on :initarg :on :reader join-on :initform nil)
   (using :initarg :using :reader join-using :initform nil)
   (natural :initarg :natural :reader join-natural :initform nil)))

(defclass on-clause (sql-clause)
  ((expr :initarg :expr :reader on-expr)))

(defclass group-by-clause (sql-clause)
  ((items :initarg :items :reader group-by-items)))

(defclass having-clause (sql-clause)
  ((expr :initarg :expr :reader having-expr)))

(defclass order-by-clause (sql-clause)
  ((items :initarg :items :reader order-by-items)))

(defclass limit-clause (sql-clause)
  ((count :initarg :count :reader limit-count)))

(defclass offset-clause (sql-clause)
  ((count :initarg :count :reader offset-count)))

(defclass values-clause (sql-clause)
  ((rows :initarg :rows :reader values-rows)))

(defclass set-clause (sql-clause)
  ((assignments :initarg :assignments :reader set-assignments)))

(defclass returning-clause (sql-clause)
  ((items :initarg :items :reader returning-items)))

(defclass column-def (sql-clause)
  ((name :initarg :name :reader column-def-name)
   (type :initarg :type :reader column-def-type)
   (primary-key :initarg :primary-key :reader column-def-primary-key :initform nil)
   (autoincrement :initarg :autoincrement :reader column-def-autoincrement :initform nil)
   (not-null :initarg :not-null :reader column-def-not-null :initform nil)
   (unique :initarg :unique :reader column-def-unique :initform nil)
   (default :initarg :default :reader column-def-default :initform nil)))

(defclass add-column-clause (sql-clause)
  ((column :initarg :column :reader add-column-column)))

(defclass drop-column-clause (sql-clause)
  ((name :initarg :name :reader drop-column-name)))

(defclass procedure-params-clause (sql-clause)
  ((params :initarg :params :reader procedure-params-list)))

(defclass procedure-param (sql-node)
  ((name :initarg :name :reader procedure-param-name)
   (type :initarg :type :reader procedure-param-type)
   (mode :initarg :mode :reader procedure-param-mode :initform :in)))

(defclass body-clause (sql-clause)
  ((forms :initarg :forms :reader body-forms)))

;;; Procedural (SQL/PSM / plpgsql) — lispy control flow inside BODY

(defclass proc-node (sql-node) ()
  (:documentation "Procedural statement (IF/LET/SETF/LOOP/…) inside CREATE PROCEDURE."))

(defclass proc-progn (proc-node)
  ((forms :initarg :forms :reader proc-progn-forms :initform nil)))

(defclass proc-let (proc-node)
  ((bindings :initarg :bindings :reader proc-let-bindings)
   ;; each binding: (name type &optional default-expr)
   (forms :initarg :forms :reader proc-let-forms :initform nil)
   (sequential :initarg :sequential :reader proc-let-sequential :initform nil)))

(defclass proc-if (proc-node)
  ((test :initarg :test :reader proc-if-test)
   (then :initarg :then :reader proc-if-then)
   (else :initarg :else :reader proc-if-else :initform nil)))

(defclass proc-cond (proc-node)
  ((clauses :initarg :clauses :reader proc-cond-clauses)))
  ;; each clause: (test . forms) ; test = T for otherwise

(defclass proc-setf (proc-node)
  ((place :initarg :place :reader proc-setf-place)
   (value :initarg :value :reader proc-setf-value)))

(defclass proc-while (proc-node)
  ((test :initarg :test :reader proc-while-test)
   (forms :initarg :forms :reader proc-while-forms :initform nil)
   (until :initarg :until :reader proc-while-until :initform nil)))

(defclass proc-loop (proc-node)
  ((forms :initarg :forms :reader proc-loop-forms :initform nil)
   (label :initarg :label :reader proc-loop-label :initform nil)))

(defclass proc-return (proc-node)
  ((label :initarg :label :reader proc-return-label :initform nil))
  (:documentation "LEAVE (SQL/PSM) / EXIT (plpgsql); optional block/loop label."))

(defclass distinct-clause (sql-clause)
  ((on :initarg :on :reader distinct-on :initform nil))) ; nil = DISTINCT; list = DISTINCT ON (…)

(defclass for-update-clause (sql-clause)
  ((of :initarg :of :reader for-update-of :initform nil)
   (nowait :initarg :nowait :reader for-update-nowait :initform nil)
   (skip-locked :initarg :skip-locked :reader for-update-skip-locked :initform nil)))

(defclass with-cte-clause (sql-clause)
  ((ctes :initarg :ctes :reader with-cte-ctes)))

(defclass select-source-clause (sql-clause)
  ((select :initarg :select :reader select-source-select))
  (:documentation "INSERT … SELECT source (Core insert().from_select)."))

;;; schema (Core Table lite)

(defclass sql-table (sql-node)
  ((name :initarg :name :reader sql-table-name)
   (columns :initarg :columns :reader sql-table-columns :initform nil)))

;;; statements

(defclass select-statement (sql-statement)
  ((clauses :initarg :clauses :accessor statement-clauses :initform nil)))

(defclass compound-select-statement (sql-statement)
  ((op :initarg :op :reader compound-op) ; :union :union-all :intersect …
   (selects :initarg :selects :reader compound-selects)))

(defclass insert-statement (sql-statement)
  ((table :initarg :table :reader insert-table)
   (clauses :initarg :clauses :accessor statement-clauses :initform nil)))

(defclass update-statement (sql-statement)
  ((table :initarg :table :reader update-table)
   (clauses :initarg :clauses :accessor statement-clauses :initform nil)))

(defclass delete-statement (sql-statement)
  ((table :initarg :table :reader delete-table)
   (clauses :initarg :clauses :accessor statement-clauses :initform nil)))

(defclass drop-table-statement (sql-statement)
  ((table :initarg :table :reader drop-table-table)
   (if-exists :initarg :if-exists :reader drop-table-if-exists :initform nil)))

(defclass alter-table-statement (sql-statement)
  ((table :initarg :table :reader alter-table-table)
   (actions :initarg :actions :reader alter-table-actions :initform nil)))

(defclass create-index-statement (sql-statement)
  ((name :initarg :name :reader create-index-name)
   (table :initarg :table :reader create-index-table :initform nil)
   (columns :initarg :columns :reader create-index-columns :initform nil)
   (unique :initarg :unique :reader create-index-unique :initform nil)
   (if-not-exists :initarg :if-not-exists :reader create-index-if-not-exists :initform nil)))

(defclass drop-index-statement (sql-statement)
  ((name :initarg :name :reader drop-index-name)
   (if-exists :initarg :if-exists :reader drop-index-if-exists :initform nil)))

(defclass create-procedure-statement (sql-statement)
  ((name :initarg :name :reader create-procedure-name)
   (params :initarg :params :reader create-procedure-params :initform nil)
   (body :initarg :body :reader create-procedure-body :initform nil)
   (language :initarg :language :reader create-procedure-language :initform :plpgsql)
   (or-replace :initarg :or-replace :reader create-procedure-or-replace :initform nil)))

(defclass call-statement (sql-statement)
  ((name :initarg :name :reader call-name)
   (args :initarg :args :reader call-args :initform nil)))

;;; ---- ANSI Foundation extensions ----

(defclass is-distinct-from-op (sql-expr)
  ((left :initarg :left :reader is-distinct-from-left)
   (right :initarg :right :reader is-distinct-from-right)
   (not-p :initarg :not-p :reader is-distinct-from-not-p :initform nil)))

(defclass similar-to-op (sql-expr)
  ((left :initarg :left :reader similar-to-left)
   (pattern :initarg :pattern :reader similar-to-pattern)
   (escape :initarg :escape :reader similar-to-escape :initform nil)
   (not-p :initarg :not-p :reader similar-to-not-p :initform nil)))

(defclass quantified-op (sql-expr)
  ((op :initarg :op :reader quantified-op-op) ; := :!= :< …
   (quantifier :initarg :quantifier :reader quantified-op-quantifier) ; :any :all :some
   (left :initarg :left :reader quantified-op-left)
   (subquery :initarg :subquery :reader quantified-op-subquery)))

(defclass unique-pred (sql-expr)
  ((query :initarg :query :reader unique-pred-query)))

(defclass collate-expr (sql-expr)
  ((expr :initarg :expr :reader collate-expr-of)
   (collation :initarg :collation :reader collate-collation)))

(defclass nullif-expr (sql-expr)
  ((left :initarg :left :reader nullif-left)
   (right :initarg :right :reader nullif-right)))

(defclass window-spec (sql-node)
  ((partition-by :initarg :partition-by :reader window-partition-by :initform nil)
   (order-by :initarg :order-by :reader window-order-by :initform nil)
   (frame :initarg :frame :reader window-frame :initform nil)))

(defclass window-frame (sql-node)
  ((unit :initarg :unit :reader window-frame-unit :initform :rows) ; :rows :range :groups
   (start :initarg :start :reader window-frame-start) ; (:unbounded-preceding) (:preceding n) (:current-row) …
   (end :initarg :end :reader window-frame-end :initform nil)))

(defclass over-expr (sql-expr)
  ((expr :initarg :expr :reader over-expr-of)
   (window :initarg :window :reader over-window)))

(defclass grouping-expr (sql-expr)
  ((kind :initarg :kind :reader grouping-kind) ; :rollup :cube :grouping-sets
   (items :initarg :items :reader grouping-items)))

(defclass using-clause (sql-clause)
  ((columns :initarg :columns :reader using-columns)))

(defclass values-selectable (sql-statement)
  ((rows :initarg :rows :reader values-selectable-rows)
   (alias :initarg :alias :reader values-selectable-alias :initform nil)
   (column-names :initarg :column-names :reader values-selectable-column-names :initform nil))
  (:documentation "ANSI VALUES as a selectable (FROM values-row(...))."))

(defclass default-values-clause (sql-clause) ())

(defclass table-constraint (sql-node)
  ((name :initarg :name :reader table-constraint-name :initform nil)))

(defclass primary-key-constraint (table-constraint)
  ((columns :initarg :columns :reader primary-key-columns)))

(defclass unique-constraint (table-constraint)
  ((columns :initarg :columns :reader unique-constraint-columns)))

(defclass check-constraint (table-constraint)
  ((expr :initarg :expr :reader check-constraint-expr)))

(defclass foreign-key-constraint (table-constraint)
  ((columns :initarg :columns :reader foreign-key-columns)
   (ref-table :initarg :ref-table :reader foreign-key-ref-table)
   (ref-columns :initarg :ref-columns :reader foreign-key-ref-columns :initform nil)
   (on-delete :initarg :on-delete :reader foreign-key-on-delete :initform nil)
   (on-update :initarg :on-update :reader foreign-key-on-update :initform nil)
   (match :initarg :match :reader foreign-key-match :initform nil)))

(defclass create-table-statement (sql-statement)
  ((table :initarg :table :reader create-table-table)
   (columns :initarg :columns :reader create-table-columns :initform nil)
   (constraints :initarg :constraints :reader create-table-constraints :initform nil)
   (if-not-exists :initarg :if-not-exists :reader create-table-if-not-exists :initform nil)))

(defclass add-constraint-clause (sql-clause)
  ((constraint :initarg :constraint :reader add-constraint-constraint)))

(defclass drop-constraint-clause (sql-clause)
  ((name :initarg :name :reader drop-constraint-name)))

(defclass rename-column-clause (sql-clause)
  ((old :initarg :old :reader rename-column-old)
   (new :initarg :new :reader rename-column-new)))

(defclass rename-table-clause (sql-clause)
  ((new :initarg :new :reader rename-table-new)))

(defclass create-view-statement (sql-statement)
  ((name :initarg :name :reader create-view-name)
   (columns :initarg :columns :reader create-view-columns :initform nil)
   (query :initarg :query :reader create-view-query)
   (or-replace :initarg :or-replace :reader create-view-or-replace :initform nil)
   (recursive :initarg :recursive :reader create-view-recursive :initform nil)))

(defclass drop-view-statement (sql-statement)
  ((name :initarg :name :reader drop-view-name)
   (if-exists :initarg :if-exists :reader drop-view-if-exists :initform nil)
   (cascade :initarg :cascade :reader drop-view-cascade :initform nil)))

(defclass create-schema-statement (sql-statement)
  ((name :initarg :name :reader create-schema-name)
   (authorization :initarg :authorization :reader create-schema-authorization :initform nil)
   (if-not-exists :initarg :if-not-exists :reader create-schema-if-not-exists :initform nil)))

(defclass drop-schema-statement (sql-statement)
  ((name :initarg :name :reader drop-schema-name)
   (if-exists :initarg :if-exists :reader drop-schema-if-exists :initform nil)
   (cascade :initarg :cascade :reader drop-schema-cascade :initform nil)))

(defclass create-sequence-statement (sql-statement)
  ((name :initarg :name :reader create-sequence-name)
   (start :initarg :start :reader create-sequence-start :initform nil)
   (increment :initarg :increment :reader create-sequence-increment :initform nil)
   (minvalue :initarg :minvalue :reader create-sequence-minvalue :initform nil)
   (maxvalue :initarg :maxvalue :reader create-sequence-maxvalue :initform nil)
   (cycle :initarg :cycle :reader create-sequence-cycle :initform nil)
   (if-not-exists :initarg :if-not-exists :reader create-sequence-if-not-exists :initform nil)))

(defclass drop-sequence-statement (sql-statement)
  ((name :initarg :name :reader drop-sequence-name)
   (if-exists :initarg :if-exists :reader drop-sequence-if-exists :initform nil)))

(defclass truncate-statement (sql-statement)
  ((tables :initarg :tables :reader truncate-tables)
   (identity :initarg :identity :reader truncate-identity :initform nil) ; :restart :continue
   (cascade :initarg :cascade :reader truncate-cascade :initform nil)))

(defclass merge-statement (sql-statement)
  ((table :initarg :table :reader merge-table)
   (using :initarg :using :reader merge-using)
   (on :initarg :on :reader merge-on)
   (when-matched :initarg :when-matched :reader merge-when-matched :initform nil)
   (when-not-matched :initarg :when-not-matched :reader merge-when-not-matched :initform nil)))

(defclass merge-update-action (sql-node)
  ((assignments :initarg :assignments :reader merge-update-assignments)
   (where :initarg :where :reader merge-update-where :initform nil)))

(defclass merge-delete-action (sql-node)
  ((where :initarg :where :reader merge-delete-where :initform nil)))

(defclass merge-insert-action (sql-node)
  ((columns :initarg :columns :reader merge-insert-columns :initform nil)
   (values :initarg :values :reader merge-insert-values)))
