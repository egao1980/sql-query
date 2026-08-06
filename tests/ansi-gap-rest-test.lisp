(in-package #:sql-query/tests)

;;;; Remaining ANSI/Foundation gaps — assertions, LIKE table, generated cols,
;;;; FILTER / WITHIN GROUP, named WINDOW, TABLESAMPLE, LOCK / SET TRANSACTION,
;;;; collation/charset DDL, extended row-lock strengths.

;;; ---------------------------------------------------------------------------
;;; CREATE / DROP ASSERTION
;;; ---------------------------------------------------------------------------

(deftest assertion-ddl
  (let ((sql (%sql (create-assertion :no-neg
                                     (sql-not (exists (select (columns 1) (from :t)
                                                              (where (:< :n 0)))))))))
    (%assert-contains sql "CREATE ASSERTION" "CHECK (" "EXISTS"))
  (let ((sql (%sql (drop-assertion :no-neg :if-exists t :cascade t))))
    (%assert-contains sql "DROP ASSERTION" "IF EXISTS" "CASCADE")))

;;; ---------------------------------------------------------------------------
;;; CREATE TABLE LIKE
;;; ---------------------------------------------------------------------------

(deftest create-table-like-emit
  (let ((sql (%sql (create-table-like :clone :users
                                      :including '(:defaults :constraints)
                                      :excluding '(:indexes)
                                      :temporary t
                                      :if-not-exists t))))
    (%assert-contains sql "CREATE TEMPORARY TABLE" "IF NOT EXISTS"
                      "LIKE" "INCLUDING DEFAULTS" "INCLUDING CONSTRAINTS"
                      "EXCLUDING INDEXES"))
  (let ((sql (%sql (create-table :mixed
                                 (column :id :type :integer)
                                 (table-like :src :including '(:all))))))
    (%assert-contains sql "CREATE TABLE" "LIKE" "INCLUDING ALL")))

;;; ---------------------------------------------------------------------------
;;; Generated columns
;;; ---------------------------------------------------------------------------

(deftest generated-column-def
  (let ((sql (%sql (create-table :t
                                 (column :a :type :integer)
                                 (column :b :type :integer)
                                 (column :sum :type :integer
                                         :generated :always
                                         :as (:+ :a :b)
                                         :stored t)))))
    (%assert-contains sql "GENERATED ALWAYS AS" "STORED")))

;;; ---------------------------------------------------------------------------
;;; FILTER / WITHIN GROUP
;;; ---------------------------------------------------------------------------

(deftest aggregate-filter-where
  (let ((sql (%sql (select (columns (sql-func :count :* :filter (:> :x 0)))
                           (from :t)))))
    (%assert-contains sql "COUNT(*)" "FILTER (WHERE" "> 0")))

(deftest within-group-ordered-set
  (let ((sql (%sql (select (columns (sql-func :percentile-cont 0.5
                                              :within-group '((:salary :desc))))
                           (from :emp)))))
    (%assert-contains sql "WITHIN GROUP (ORDER BY" "DESC")
    (ok (search "percentile-cont" sql :test #'char-equal))))

;;; ---------------------------------------------------------------------------
;;; Named WINDOW + OVER name
;;; ---------------------------------------------------------------------------

(deftest named-window-clause
  (let ((sql (%sql (select (columns (over (count :*) :window :w))
                           (from :t)
                           (window :w :partition-by '(:dept) :order-by '(:id))))))
    (%assert-contains sql "WINDOW" "AS (" "PARTITION BY" "ORDER BY" "OVER")
    (ok (search "\"w\"" sql))))

;;; ---------------------------------------------------------------------------
;;; TABLESAMPLE
;;; ---------------------------------------------------------------------------

(deftest tablesample-from
  (let ((sql (%sql (select (columns :id)
                           (from :users :tablesample
                                 (tablesample :bernoulli 10 :repeatable 42))))))
    (%assert-contains sql "TABLESAMPLE BERNOULLI" "REPEATABLE")))

;;; ---------------------------------------------------------------------------
;;; LOCK TABLE / SET TRANSACTION
;;; ---------------------------------------------------------------------------

(deftest lock-table-emit
  (let ((sql (%sql (lock-table :users :mode :share :nowait t))))
    (%assert-contains sql "LOCK TABLE" "IN SHARE MODE" "NOWAIT"))
  (let ((sql (%sql (lock-table '(:a :b) :mode :exclusive))))
    (%assert-contains sql "LOCK TABLE" "EXCLUSIVE MODE")))

(deftest set-transaction-emit
  (let ((sql (%sql (set-transaction :isolation :serializable
                                    :access-mode :read-only))))
    (%assert-contains sql "SET TRANSACTION" "ISOLATION LEVEL SERIALIZABLE"
                      "READ ONLY")))

;;; ---------------------------------------------------------------------------
;;; Collation / character set DDL
;;; ---------------------------------------------------------------------------

(deftest collation-ddl
  (let ((sql (%sql (create-collation :mycoll :from :ucs_basic :pad-space t))))
    (%assert-contains sql "CREATE COLLATION" "FROM" "PAD SPACE"))
  (let ((sql (%sql (drop-collation :mycoll :if-exists t))))
    (%assert-contains sql "DROP COLLATION" "IF EXISTS")))

(deftest character-set-ddl-stub
  (let ((sql (%sql (create-character-set :mycs :from :utf8 :collate :ucs_basic))))
    (%assert-contains sql "CREATE CHARACTER SET" "GET" "COLLATE"))
  (let ((sql (%sql (drop-character-set :mycs :if-exists t))))
    (%assert-contains sql "DROP CHARACTER SET" "IF EXISTS")))

;;; ---------------------------------------------------------------------------
;;; FOR SHARE / KEY SHARE / NO KEY UPDATE (base dialect emits; ANSI rejects)
;;; ---------------------------------------------------------------------------

(deftest for-share-strengths-on-base
  "Base sql-dialect emits vendor strengths; use a fresh sql-dialect instance."
  (let* ((d (make-instance 'sql-dialect))
         (share (%sql (select (columns :id) (from :t) (for-share)) d))
         (nku (%sql (select (columns :id) (from :t) (for-no-key-update)) d))
         (ks (%sql (select (columns :id) (from :t) (for-key-share)) d)))
    (%assert-contains share "FOR SHARE")
    (%assert-contains nku "FOR NO KEY UPDATE")
    (%assert-contains ks "FOR KEY SHARE")))

(deftest ansi-rejects-for-share-strength
  (ok (signals (compile-sql (select (columns :id) (from :t) (for-share))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))
