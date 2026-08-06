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
   #:sql-fragment
   #:make-sql-fragment

   ;; statements
   #:select-statement
   #:insert-statement
   #:update-statement
   #:delete-statement
   #:compound-select-statement
   #:create-table-statement
   #:drop-table-statement
   #:alter-table-statement
   #:create-index-statement
   #:drop-index-statement
   #:create-procedure-statement
   #:call-statement

   ;; statement constructors (SQLAlchemy Core–shaped)
   #:select
   #:insert-into
   #:update
   #:delete-from
   #:union #:union-all
   #:intersect #:intersect-all
   #:except #:except-all
   #:create-table
   #:drop-table
   #:alter-table
   #:create-index
   #:drop-index
   #:create-procedure
   #:sql-call

   ;; clauses
   #:columns
   #:from
   #:where
   #:join #:left-join #:inner-join #:right-join #:full-join #:cross-join
   #:natural-join #:natural-left-join #:natural-right-join #:natural-full-join
   #:on #:using
   #:group-by #:having #:order-by
   #:limit #:offset
   #:distinct #:for-update
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
   #:sql-func #:count #:coalesce
   #:exists #:subquery #:lateral
   #:label #:bindparam #:sql-raw #:typed #:typed-value
   #:bind-param #:bind-param-name #:bind-param-value #:bind-param-default
   #:bind-param-sql-type #:bind-param-has-value #:bind-param-has-default
   #:over #:rows-frame #:range-frame
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
   #:emit-join #:emit-column-list

   ;; extension registries (types own Lisp↔expression conversion)
   #:sql-type-def #:sql-op-def
   #:register-sql-type #:register-sql-op
   #:find-sql-type #:find-sql-op #:find-sql-op-catalog
   #:sql-type-write #:sql-type-read #:encode-sql-value
   #:emit-typed-value #:registered-type-sql
   #:*sql-op-catalog*

   #:column-def-name #:column-def-type #:column-def-primary-key
   #:column-def-autoincrement #:column-def-not-null #:column-def-unique
   #:column-def-default
   #:create-procedure-name #:create-procedure-params #:create-procedure-body
   #:create-procedure-language #:create-procedure-or-replace
   #:procedure-param-name #:procedure-param-type #:procedure-param-mode
   #:call-name #:call-args
   #:binary-op #:binary-op-left #:binary-op-right #:binary-op-op
   #:column-ref #:column-ref-name #:column-ref-table
   #:limit-count #:offset-count #:distinct-on
   #:for-update-of #:for-update-nowait #:for-update-skip-locked

   #:compile-sql #:execute-query #:fetch-query #:fetch-all-query))

(in-package #:sql-query)
