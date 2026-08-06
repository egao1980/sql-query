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
   #:on
   #:group-by
   #:having
   #:order-by
   #:limit
   #:offset
   #:distinct
   #:for-update
   #:cte
   #:with-cte
   #:sql-values
   #:sql-set
   #:returning
   #:column
   #:add-column
   #:drop-column
   #:procedure-params
   #:procedure-param
   #:params
   #:in #:out
   #:body

   ;; expressions (Core sqlelement parity)
   #:|=| #:|!=| #:|:<| #:|:>| #:|:<=| #:|:>=|
   #:|:+| #:|:-| #:|:*| #:|:/|
   #:sql-and #:sql-or #:sql-not
   #:sql-in #:sql-between
   #:sql-is-null #:sql-is-not-null
   #:sql-like
   #:sql-case
   #:sql-cast
   #:sql-func #:count #:coalesce
   #:exists
   #:subquery
   #:label
   #:bindparam
   #:sql-raw
   #:col #:lit
   #:ensure-expr #:parse-expr
   #:as-cte

   ;; schema (Core Table/MetaData lite)
   #:sql-table
   #:sql-table-name
   #:sql-table-columns
   #:make-sql-table
   #:table-column
   #:create-table-from

   ;; composition
   #:merge-query
   #:and-where
   #:copy-sql

   ;; dialect protocol — builtin = ANSI only; backends register vendors
   #:sql-dialect
   #:ansi-dialect
   #:make-ansi-dialect
   #:use-ansi-dialect
   #:*sql-dialect*
   #:*sql-dialect-registry*
   #:register-sql-dialect
   #:dialect-for-connection
   #:dialect-param-style
   #:dialect-quote-char
   #:dialect-boolean
   #:dialect-type-sql
   #:dialect-autoincrement-pk
   #:dialect-autoincrement-suffix
   #:emit-sql
   #:emit-column-def
   #:emit-create-procedure
   #:emit-call
   #:emit-ident
   #:ident-string
   #:emit-placeholder
   #:push-param
   #:emit-context
   #:emit-context-params
   #:emit-context-dialect

   ;; dialect author — node readers
   #:column-def-name #:column-def-type #:column-def-primary-key
   #:column-def-autoincrement #:column-def-not-null #:column-def-unique
   #:column-def-default
   #:create-procedure-name #:create-procedure-params #:create-procedure-body
   #:create-procedure-language #:create-procedure-or-replace
   #:procedure-param-name #:procedure-param-type #:procedure-param-mode
   #:call-name #:call-args
   #:binary-op
   #:binary-op-left #:binary-op-right #:binary-op-op

   #:compile-sql
   #:execute-query
   #:fetch-query
   #:fetch-all-query))

(in-package #:sql-query)
