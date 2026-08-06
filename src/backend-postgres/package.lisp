(defpackage #:sql-query-postgres
  (:use #:cl #:sql-query)
  (:shadowing-import-from #:sql-query #:count #:union)
  (:export #:postgres-dialect
           #:make-postgres-dialect
           #:use-postgres-dialect))

(in-package #:sql-query-postgres)
