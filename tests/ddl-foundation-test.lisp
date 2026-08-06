(in-package #:sql-query/tests)

;;;; ANSI Foundation DDL — GRANT/REVOKE, CAST, ALTER DOMAIN/COLUMN,
;;;; FUNCTION, TRIGGER, COMMENT ON, TEMPORARY table, view CHECK OPTION.

(deftest ddl-temporary-table-and-on-commit
  (let ((sql (%sql (create-table :t
                      (column :id :type :integer)
                      :temporary
                      :on-commit :preserve))))
    (%assert-contains sql "CREATE TEMPORARY TABLE" "ON COMMIT PRESERVE ROWS")))

(deftest ddl-view-check-option
  (%assert-contains (%sql (create-view :v (select (columns :id) (from :t))
                                       :check-option t))
                    "CREATE VIEW" "WITH CHECK OPTION")
  (%assert-contains (%sql (create-view :v (select (columns :id) (from :t))
                                       :check-option :cascaded))
                    "WITH CASCADED CHECK OPTION")
  (%assert-contains (%sql (create-view :v (select (columns :id) (from :t))
                                       :check-option :local
                                       :temporary t))
                    "TEMPORARY VIEW" "WITH LOCAL CHECK OPTION"))

(deftest ddl-alter-column-actions
  (let ((sql (%sql (alter-table :t
                     (set-default 0 :column :x)
                     (drop-default :column :x)
                     (set-not-null :column :x)
                     (drop-not-null :column :x)
                     (set-data-type :x :integer)))))
    (%assert-contains sql
                      "ALTER TABLE" "ALTER COLUMN"
                      "SET DEFAULT" "DROP DEFAULT"
                      "SET NOT NULL" "DROP NOT NULL"
                      "SET DATA TYPE" "INTEGER")))

(deftest ddl-alter-column-helper
  (let ((sql (%sql (alter-table :t
                     (alter-column :x (set-default 1) (set-not-null))
                     (set-data-type :y :text)))))
    (%assert-contains sql "ALTER COLUMN" "SET DEFAULT" "SET NOT NULL"
                      "SET DATA TYPE")))

(deftest ddl-alter-domain-actions
  (let ((sql (%sql (alter-domain :posint
                     (set-default 1)
                     (drop-default)
                     (set-not-null)
                     (drop-not-null)
                     (add-constraint (check (:> (col :value) 0) :name :chk))
                     (drop-constraint :chk)))))
    (%assert-contains sql
                      "ALTER DOMAIN" "SET DEFAULT" "DROP DEFAULT"
                      "SET NOT NULL" "DROP NOT NULL"
                      "ADD" "CHECK" "DROP CONSTRAINT")))

(deftest ddl-create-drop-cast
  (let ((sql (%sql (create-cast :integer :text
                                :with-function :int-to-text
                                :as :assignment)))
        (drop (%sql (drop-cast :integer :text :if-exists t))))
    (%assert-contains sql "CREATE CAST (" "AS" "WITH FUNCTION" "AS ASSIGNMENT")
    (%assert-contains drop "DROP CAST" "IF EXISTS")))

(deftest ddl-create-drop-function
  (let* ((stmt (create-function :add1
                  (params (in :n :integer))
                  :returns :integer
                  :deterministic
                  (body (sql-fragment "RETURN n + 1"))))
         (sql (%sql stmt))
         (drop (%sql (drop-function :add1 :if-exists t :cascade t))))
    (%assert-contains sql "CREATE FUNCTION" "RETURNS" "INTEGER"
                      "LANGUAGE SQL" "DETERMINISTIC" "BEGIN" "END")
    (%assert-contains drop "DROP FUNCTION" "IF EXISTS" "CASCADE")))

(deftest ddl-create-drop-trigger
  (let ((sql (%sql (create-trigger :trg
                                   :timing :after
                                   :events '(:insert :update)
                                   :table :users
                                   :for-each :row
                                   :condition (:> (col :id) 0)
                                   :body (list (sql-fragment "SET x = 1")))))
        (drop (%sql (drop-trigger :trg :table :users :if-exists t :cascade t))))
    (%assert-contains sql "CREATE TRIGGER" "AFTER" "INSERT OR UPDATE"
                      "ON" "FOR EACH ROW" "WHEN (" "BEGIN" "END")
    (%assert-contains drop "DROP TRIGGER" "IF EXISTS" "ON" "CASCADE")))

(deftest ddl-grant-revoke
  (let ((g (%sql (grant '(:select :insert) :on :users :to :alice
                        :with-grant-option t)))
        (r (%sql (revoke t :on :users :from :alice :cascade t
                         :grant-option-for t)))
        (schema (%sql (grant :usage :on :public :to :bob :on-kind :schema)))
        (pub (%sql (grant :select :on :t :to :public))))
    (%assert-contains g "GRANT" "SELECT, INSERT" "ON TABLE" "TO" "WITH GRANT OPTION")
    (%assert-contains r "REVOKE" "GRANT OPTION FOR" "ALL PRIVILEGES" "FROM" "CASCADE")
    (%assert-contains schema "GRANT" "USAGE" "ON SCHEMA")
    (%assert-contains pub "TO PUBLIC")
    (%assert-absent pub "\"public\"")))

(deftest ddl-create-cast-without-function-default
  (%assert-contains (%sql (create-cast :integer :bigint :as :implicit))
                    "CREATE CAST" "WITHOUT FUNCTION" "AS IMPLICIT")
  (%assert-contains (%sql (create-cast :text :integer :with-inout t))
                    "WITH INOUT"))

(deftest ddl-comment-on
  (%assert-contains (%sql (comment-on :table :users "user accounts"))
                    "COMMENT ON TABLE" "IS" "user accounts")
  (%assert-contains (%sql (comment-on :column '(:users :email) "addr"))
                    "COMMENT ON COLUMN" "." "IS")
  (%assert-contains (%sql (comment-on :view :v "a view"))
                    "COMMENT ON VIEW"))
