(defsystem "sql-query"
  :version "0.1.0"
  :description "Composable CLOS SQL DSL for cl-stack (dialects, DML/DDL/proc, sql-fragment)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop" "sql-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "nodes")
               (:file "constructors")
               (:file "compose")
               (:file "dialect")
               (:file "dialect-sqlite3")
               (:file "dialect-postgres")
               (:file "compile")
               (:file "execute"))
  :in-order-to ((test-op (test-op "sql-query/tests"))))

(defsystem "sql-query/tests"
  :depends-on ("sql-query" "sql-backend-sqlite3" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "sql-query-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
