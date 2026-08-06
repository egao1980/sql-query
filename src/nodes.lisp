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
  ((value :initarg :value :reader literal-value)))

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
   (has-value :initarg :has-value :reader bind-param-has-value :initform nil)))

(defclass subquery (sql-expr)
  ((query :initarg :query :reader subquery-query)
   (alias :initarg :alias :reader subquery-alias :initform nil)))

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
  ((type :initarg :type :reader join-type) ; :inner :left :right
   (table :initarg :table :reader join-table)
   (alias :initarg :alias :reader join-alias :initform nil)
   (on :initarg :on :reader join-on :initform nil)))

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

(defclass create-table-statement (sql-statement)
  ((table :initarg :table :reader create-table-table)
   (columns :initarg :columns :reader create-table-columns :initform nil)
   (if-not-exists :initarg :if-not-exists :reader create-table-if-not-exists :initform nil)))

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
