(defsystem "sql-query-sqlite3"
  :version "0.1.0"
  :description "sql-query dialect backend — SQLite3"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-query")
  :serial t
  :pathname "src/backend-sqlite3"
  :components ((:file "package")
               (:file "extensions")
               (:file "dialect")))
