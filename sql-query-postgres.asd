(defsystem "sql-query-postgres"
  :version "0.1.0"
  :description "sql-query dialect backend — PostgreSQL"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-query")
  :serial t
  :pathname "src/backend-postgres"
  :components ((:file "package")
               (:file "dialect")))
