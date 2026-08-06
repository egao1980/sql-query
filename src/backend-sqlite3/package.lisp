(defpackage #:sql-query-sqlite3
  (:use #:cl #:sql-query)
  (:shadowing-import-from #:sql-query #:count #:union)
  (:export #:sqlite3-dialect
           #:make-sqlite3-dialect
           #:use-sqlite3-dialect))

(in-package #:sql-query-sqlite3)
