(defsystem "sql-query"
  :version "0.1.0"
  :description "Composable CLOS SQL DSL for cl-stack — ANSI SQL builtin; vendor dialects are separate projects"
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
               (:file "extensions")
               (:file "procedure")
               (:file "dialect-ansi")
               (:file "compile")
               (:file "execute"))
  :in-order-to ((test-op (test-op "sql-query/tests"))))

(defsystem "sql-query/tests"
  :depends-on ("sql-query" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "sql-query-test")
               (:file "ansi-compliance")
               (:file "extension-registry-test")
               (:file "dialect-extension-test")
               (:file "ddl-foundation-test")
               (:file "procedure-test")
               (:file "expr-txn-gap-test")
               (:file "ansi-gap-rest-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
