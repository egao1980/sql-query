(defpackage #:sql-query
  (:use #:cl)
  (:nicknames #:stack-sql-query)
  (:export #:sql-query-error
           #:sql-dialect-unsupported
           #:sql-query-error-message
           #:sql-unsupported-feature

           #:sql-node
           #:sql-statement
           #:sql-fragment
           #:make-sql-fragment

           #:select-statement
           #:insert-statement
           #:update-statement
           #:delete-statement
           #:create-table-statement
           #:drop-table-statement
           #:alter-table-statement
           #:create-index-statement
           #:drop-index-statement
           #:create-procedure-statement
           #:call-statement

           #:select
           #:insert-into
           #:update
           #:delete-from
           #:create-table
           #:drop-table
           #:alter-table
           #:create-index
           #:drop-index
           #:create-procedure
           #:sql-call

           #:columns
           #:from
           #:where
           #:join
           #:left-join
           #:inner-join
           #:on
           #:group-by
           #:having
           #:order-by
           #:limit
           #:offset
           #:sql-values
           #:sql-set
           #:returning
           #:column
           #:add-column
           #:drop-column
           #:procedure-params
           #:procedure-param
           #:params
           #:in
           #:out
           #:body

           #:|=|
           #:|!=|
           #:|:<|
           #:|:>|
           #:|:<=|
           #:|:>=|
           #:sql-and
           #:sql-or
           #:sql-not
           #:sql-in
           #:sql-is-null
           #:sql-is-not-null
           #:sql-like
           #:sql-raw
           #:col
           #:lit
           #:ensure-expr
           #:parse-expr

           #:merge-query
           #:and-where
           #:copy-sql

           #:sql-dialect
           #:sqlite3-dialect
           #:postgres-dialect
           #:*sql-dialect*
           #:dialect-for-connection
           #:make-sqlite3-dialect
           #:make-postgres-dialect

           #:compile-sql
           #:emit-sql

           #:execute-query
           #:fetch-query
           #:fetch-all-query))

(in-package #:sql-query)
