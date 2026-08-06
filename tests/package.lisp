(defpackage #:sql-query/tests
  (:use #:cl #:rove #:sql-query)
  (:shadowing-import-from #:sql-query #:count #:union)
  (:export #:%sql #:%params #:%compile #:%has #:%norm
           #:%assert-contains #:%assert-absent))

(in-package #:sql-query/tests)
