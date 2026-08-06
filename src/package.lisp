(defpackage #:sql-query
  (:use #:cl)
  (:nicknames #:stack-sql-query)
  (:shadow #:count #:union)
  (:export
   ;; conditions
   #:sql-query-error
   #:sql-dialect-unsupported
   #:sql-query-error-message
   #:sql-unsupported-feature

   ;; AST base
   #:sql-node
   #:sql-statement
   #:sql-clause
   #:sql-expr
   #:sql-extension
   #:sql-fragment
   #:make-sql-fragment

   ;; statements
   #:select-statement
   #:insert-statement
   #:update-statement
   #:delete-statement
   #:compound-select-statement
   #:create-table-statement
   #:create-table-as-statement
   #:start-transaction-statement
   #:commit-statement
   #:rollback-statement
   #:savepoint-statement
   #:release-savepoint-statement
   #:drop-table-statement
   #:alter-table-statement
   #:create-index-statement
   #:drop-index-statement
   #:create-procedure-statement
   #:call-statement
   #:create-type-statement
   #:drop-type-statement
   #:alter-type-statement
   #:create-domain-statement
   #:drop-domain-statement
   #:alter-domain-statement
   #:create-cast-statement
   #:drop-cast-statement
   #:create-function-statement
   #:drop-function-statement
   #:create-trigger-statement
   #:drop-trigger-statement
   #:grant-statement
   #:revoke-statement
   #:comment-on-statement
   #:create-assertion-statement
   #:drop-assertion-statement
   #:lock-table-statement
   #:create-collation-statement
   #:drop-collation-statement
   #:create-character-set-statement
   #:drop-character-set-statement

   ;; statement constructors (SQLAlchemy Core–shaped)
   #:select
   #:insert-into
   #:update
   #:delete-from
   #:union #:union-all
   #:intersect #:intersect-all
   #:except #:except-all
   #:create-table
   #:create-table-as
   #:start-transaction #:set-transaction
   #:sql-commit #:sql-rollback
   #:sql-savepoint #:sql-release-savepoint
   #:create-table-like #:table-like
   #:drop-table
   #:alter-table
   #:create-index
   #:drop-index
   #:create-procedure
   #:sql-call
   #:create-type
   #:drop-type
   #:alter-type
   #:create-domain
   #:drop-domain
   #:alter-domain
   #:set-default #:drop-default #:set-not-null #:drop-not-null #:set-data-type
   #:alter-column
   #:create-cast #:drop-cast
   #:create-function #:drop-function
   #:create-trigger #:drop-trigger
   #:grant #:revoke
   #:comment-on
   #:create-assertion #:drop-assertion
   #:lock-table
   #:create-collation #:drop-collation
   #:create-character-set #:drop-character-set

   ;; clauses
   #:columns
   #:from
   #:tablesample
   #:where
   #:join #:left-join #:inner-join #:right-join #:full-join #:cross-join
   #:natural-join #:natural-left-join #:natural-right-join #:natural-full-join
   #:on #:using
   #:group-by #:having #:order-by
   #:limit #:offset
   #:distinct #:for-update #:for-share #:for-no-key-update #:for-key-share
   #:cte #:with-cte
   #:sql-values #:default-values #:values-row
   #:sql-set #:returning
   #:column #:add-column #:drop-column
   #:primary-key #:unique-key #:check #:foreign-key
   #:add-constraint #:drop-constraint #:rename-column #:rename-to
   #:procedure-params #:procedure-param #:params #:in #:out #:inout
   ;; procedural SQL — layer 1 (SQL-shaped) + layer 2 (lispy BODY)
   #:make-body #:body #:body*
   #:proc-progn #:proc-let #:proc-if #:proc-cond #:proc-setf
   #:proc-while #:proc-until #:proc-loop #:proc-loop-labeled #:proc-return
   #:proc-node
   #:proc-progn-forms #:proc-let-bindings #:proc-let-forms #:proc-let-sequential
   #:proc-if-test #:proc-if-then #:proc-if-else
   #:proc-cond-clauses #:proc-setf-place #:proc-setf-value
   #:proc-while-test #:proc-while-forms #:proc-while-until
   #:proc-loop-forms #:proc-loop-label #:proc-return-label
   #:create-view #:drop-view
   #:create-schema #:drop-schema
   #:create-sequence #:drop-sequence
   #:truncate-table #:merge-into
   #:merge-update #:merge-delete #:merge-insert
   #:type-attribute #:add-attribute #:drop-attribute #:rename-attribute
   #:add-enum-value
   #:add-attribute-clause #:drop-attribute-clause #:rename-attribute-clause
   #:add-enum-value-clause
   #:add-attribute-attribute #:drop-attribute-name
   #:rename-attribute-old #:rename-attribute-new
   #:add-enum-value-label #:add-enum-value-before #:add-enum-value-after
   #:add-enum-value-if-not-exists
   #:type-attribute-name #:type-attribute-type
   #:create-type-name #:create-type-kind #:create-type-base-type
   #:create-type-attributes #:create-type-enum-labels #:create-type-if-not-exists
   #:create-type-base-options
   #:create-table-of-type #:create-table-extras
   #:create-table-as-table #:create-table-as-query
   #:create-table-as-temporary #:create-table-as-if-not-exists
   #:create-table-as-columns
   #:table-constraint-name #:table-constraint-deferrable #:table-constraint-initially
   #:like-op-escape #:like-op-not-p
   #:drop-type-name #:alter-type-name #:alter-type-actions
   #:create-domain-name #:create-domain-base-type
   #:drop-domain-name
   #:alter-domain-name #:alter-domain-actions
   #:create-table-temporary #:create-table-on-commit
   #:create-view-temporary #:create-view-check-option

   ;; expressions
   #:|=| #:|!=| #:|:<| #:|:>| #:|:<=| #:|:>=|
   #:|:+| #:|:-| #:|:*| #:|:/|
   #:sql-and #:sql-or #:sql-not
   #:sql-in #:sql-between
   #:sql-is-null #:sql-is-not-null
   #:sql-like #:similar-to
   #:is-distinct-from #:is-not-distinct-from
   #:sql-any #:sql-all #:sql-some #:unique
   #:sql-case #:sql-cast #:nullif #:collate
   #:sql-func #:array-lit #:array-literal #:array-literal-items
   #:count #:coalesce
   #:exists #:subquery #:lateral
   #:label #:bindparam #:sql-raw #:typed #:typed-value
   #:function-call #:function-call-name #:function-call-args
   #:function-call-filter #:function-call-within-group
   #:bind-param #:bind-param-name #:bind-param-value #:bind-param-default
   #:bind-param-sql-type #:bind-param-has-value #:bind-param-has-default
   #:bind-param-effective-value
   #:over #:window #:rows-frame #:range-frame
   #:rollup #:cube #:grouping-sets
   #:col #:lit #:ensure-expr #:parse-expr #:as-cte

   ;; schema
   #:sql-table #:sql-table-name #:sql-table-columns
   #:make-sql-table #:table-column #:create-table-from

   #:merge-query #:and-where #:copy-sql

   ;; dialect protocol
   #:sql-dialect #:ansi-dialect #:make-ansi-dialect #:use-ansi-dialect
   #:*sql-dialect* #:*sql-dialect-registry* #:register-sql-dialect
   #:dialect-for-connection
   #:dialect-param-style #:dialect-quote-char #:dialect-boolean
   #:dialect-type-sql #:dialect-autoincrement-pk #:dialect-autoincrement-suffix
   #:emit-sql #:emit-column-def #:emit-create-procedure #:emit-call
   #:emit-ident #:ident-string #:emit-placeholder #:push-param
   #:emit-sql-literal
   #:emit-context #:emit-context-params #:emit-context-dialect
   #:emit-limit-offset #:emit-returning #:emit-for-update #:emit-distinct
   #:emit-insert-prefix #:emit-insert-extras #:emit-trigger-execute
   #:emit-join #:emit-column-list
   #:emit-tablesample #:emit-table-like #:emit-lock-table #:emit-set-transaction
   ;; open dialect extension emit hooks
   #:emit-extension
   #:emit-alter-table-action
   #:emit-create-table-extra
   #:emit-create-type #:emit-create-type-kind
   #:emit-alter-type-action
   #:emit-alter-domain-action

   ;; extension registries (types / ops / funcs / AST)
   #:sql-type-def #:sql-op-def #:sql-func-def
   #:register-sql-type #:register-sql-op #:register-sql-func
   #:find-sql-type #:find-sql-op #:find-sql-func
   #:find-sql-op-catalog #:find-sql-func-catalog
   #:sql-type-write #:sql-type-read #:encode-sql-value
   #:emit-typed-value #:registered-type-sql
   #:*sql-op-catalog* #:*sql-func-catalog*
   #:sql-func-sql-name #:sql-func-emit-fn
   #:sql-extension-def #:sql-extension-name #:sql-extension-constructor
   #:sql-extension-kind #:sql-extension-documentation
   #:*sql-extension-registry*
   #:register-sql-extension #:find-sql-extension
   #:make-sql-extension #:list-sql-extensions

   #:column-def-name #:column-def-type #:column-def-primary-key
   #:column-def-autoincrement #:column-def-not-null #:column-def-unique
   #:column-def-default #:column-def-generated #:column-def-generated-as
   #:column-def-stored
   #:create-procedure-name #:create-procedure-params #:create-procedure-body
   #:create-procedure-language #:create-procedure-or-replace
   #:procedure-param-name #:procedure-param-type #:procedure-param-mode
   #:call-name #:call-args
   #:binary-op #:binary-op-left #:binary-op-right #:binary-op-op
   #:column-ref #:column-ref-name #:column-ref-table
   #:limit-count #:offset-count #:distinct-on
   #:for-update-of #:for-update-nowait #:for-update-skip-locked #:for-update-strength
   #:statement-clauses #:insert-table
   #:create-trigger-name #:create-trigger-timing #:create-trigger-events
   #:create-trigger-table #:create-trigger-for-each #:create-trigger-condition
   #:create-trigger-function #:create-trigger-function-args #:create-trigger-body
   #:drop-trigger-name #:drop-trigger-table #:drop-trigger-if-exists #:drop-trigger-cascade

   #:compile-sql #:execute-query #:fetch-query #:fetch-all-query))

(in-package #:sql-query)
