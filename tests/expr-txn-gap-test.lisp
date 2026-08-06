(in-package #:sql-query/tests)

;;;; Expression / statement gap coverage: LIKE ESCAPE, ORDER BY NULLS,
;;;; DEFERRABLE constraints, CREATE TABLE AS, transaction control.

(deftest like-escape-and-not-like
  (let ((sql (%sql (select (columns :id)
                           (from :t)
                           (where (sql-and
                                   (sql-like :name "a\\_%" :escape "\\")
                                   (sql-like :code "x%" :not t)
                                   (sql-like :tag "%\\%%" :escape "\\" :not t)))))))
    (%assert-contains sql "LIKE" "NOT LIKE" "ESCAPE")
    (ok (%has sql "LIKE 'a\\_%' ESCAPE '\\'"))
    (ok (%has sql "NOT LIKE 'x%'"))
    (ok (%has sql "NOT LIKE '%\\%%' ESCAPE '\\'"))))

(deftest order-by-nulls-first-last
  (let ((sql (%sql (select (columns :id)
                           (from :t)
                           (order-by '(:name :asc :nulls-first)
                                     '(:salary :desc :nulls-last)
                                     :id)))))
    (%assert-contains sql "ORDER BY" "NULLS FIRST" "NULLS LAST" "DESC")
    (ok (%has sql "\"name\" NULLS FIRST"))
    (ok (%has sql "\"salary\" DESC NULLS LAST")))
  (let ((sql (%sql (select (columns (over (sql-func :rank)
                                          :order-by (list '(:score :desc :nulls-last))))
                           (from :t)))))
    (%assert-contains sql "OVER (" "ORDER BY" "NULLS LAST")))

(deftest deferrable-table-constraints
  (let ((sql (%sql (create-table :orders
                      (column :id :type :integer)
                      (column :user-id :type :integer)
                      (primary-key :id :deferrable t :initially :immediate)
                      (unique-key :user-id :name :uq-user :deferrable :not)
                      (foreign-key '(:user-id)
                                   :references '(:users :id)
                                   :name :fk-user
                                   :deferrable t
                                   :initially :deferred)
                      (check (:> :id 0) :name :chk-id
                             :deferrable t :initially :deferred)))))
    (%assert-contains sql
                      "PRIMARY KEY" "DEFERRABLE" "INITIALLY IMMEDIATE"
                      "UNIQUE" "NOT DEFERRABLE"
                      "FOREIGN KEY" "INITIALLY DEFERRED"
                      "CHECK")
    (ok (%has sql "CONSTRAINT \"fk-user\""))
    (ok (%has sql "CONSTRAINT \"chk-id\""))))

(deftest create-table-as-select
  (let* ((q (select (columns :id :name) (from :users) (where (:= :active t))))
         (sql (%sql (create-table-as :active-users q
                                     :temporary t
                                     :if-not-exists t
                                     :columns '(:id :name)))))
    (%assert-contains sql
                      "CREATE TEMPORARY TABLE" "IF NOT EXISTS"
                      "AS" "SELECT" "FROM" "WHERE")
    (ok (%has sql "\"active-users\""))
    (ok (%has sql "(\"id\", \"name\") AS"))))

(deftest transaction-statements
  (%assert-contains (%sql (start-transaction)) "START TRANSACTION")
  (%assert-contains (%sql (sql-commit)) "COMMIT")
  (%assert-contains (%sql (sql-rollback)) "ROLLBACK")
  (%assert-contains (%sql (sql-rollback :to :sp1))
                    "ROLLBACK TO SAVEPOINT" "\"sp1\"")
  (%assert-contains (%sql (sql-savepoint :sp1)) "SAVEPOINT" "\"sp1\"")
  (%assert-contains (%sql (sql-release-savepoint :sp1))
                    "RELEASE SAVEPOINT" "\"sp1\""))
