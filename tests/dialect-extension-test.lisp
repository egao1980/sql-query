(in-package #:sql-query/tests)

;;;; Dialect AST extension points — vendor nodes without forking core typecases.

(defclass toy-dialect (ansi-dialect) ())

(defclass toy-inherits (sql-extension)
  ((parent :initarg :parent :reader toy-inherits-parent))
  (:documentation "Fake CREATE TABLE … INHERITS (parent) extension."))

(defclass toy-owner-to (sql-extension sql-clause)
  ((role :initarg :role :reader toy-owner-to-role))
  (:documentation "Fake ALTER TABLE … OWNER TO role."))

(defun make-toy-inherits (parent)
  (make-instance 'toy-inherits :parent parent))

(defun make-toy-owner-to (role)
  (make-instance 'toy-owner-to :role role))

(defmethod emit-create-table-extra ((d toy-dialect) (x toy-inherits) stream ctx)
  (declare (ignore ctx))
  (write-string "INHERITS (" stream)
  (emit-ident d (toy-inherits-parent x) stream)
  (write-char #\) stream))

(defmethod emit-alter-table-action ((d toy-dialect) (a toy-owner-to) stream ctx)
  (declare (ignore ctx))
  (write-string "OWNER TO " stream)
  (emit-ident d (toy-owner-to-role a) stream))

(defmethod emit-create-type-kind ((d toy-dialect) (kind (eql :toy-opaque))
                                  stmt stream ctx)
  (declare (ignore ctx))
  (write-string "CREATE TYPE " stream)
  (emit-ident d (create-type-name stmt) stream)
  (write-string " AS TOY_OPAQUE" stream))

(deftest register-and-build-extension
  (register-sql-extension :toy-inherits #'make-toy-inherits
                          :kind :create-table-extra
                          :documentation "test inherits")
  (ok (find-sql-extension :toy-inherits))
  (ok (member :toy-inherits (list-sql-extensions :create-table-extra)))
  (let ((node (make-sql-extension :toy-inherits :parent)))
    (ok (typep node 'toy-inherits))
    (ok (eq :parent (toy-inherits-parent node)))))

(deftest create-table-extras-open-emit
  (let* ((d (make-instance 'toy-dialect))
         (stmt (create-table :child
                             (column :id :type :integer)
                             (make-toy-inherits :parent)))
         (sql (nth-value 0 (compile-sql stmt :dialect d))))
    (%assert-contains sql "CREATE TABLE" "INHERITS" "parent")
    (ok (typep (first (create-table-extras stmt)) 'toy-inherits))))

(deftest create-table-of-type
  (let ((sql (%sql (create-table :people :of :person-t))))
    (%assert-contains sql "CREATE TABLE" "OF")
    (ok (search "\"person-t\"" sql))))

(deftest alter-table-action-open-emit
  (let* ((d (make-instance 'toy-dialect))
         (sql (nth-value 0 (compile-sql
                            (alter-table :t (make-toy-owner-to :app))
                            :dialect d))))
    (%assert-contains sql "ALTER TABLE" "OWNER TO")
    (ok (search "\"app\"" sql))))

(deftest alter-table-unknown-action-signals
  (ok (signals (compile-sql (alter-table :t (make-toy-owner-to :app))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

(deftest create-type-kind-open-emit
  (let* ((d (make-instance 'toy-dialect))
         (sql (nth-value 0 (compile-sql
                            (create-type :blob :kind :toy-opaque)
                            :dialect d))))
    (%assert-contains sql "CREATE TYPE" "TOY_OPAQUE")))

(deftest emit-extension-fallback
  (ok (signals (compile-sql (make-toy-inherits :p) :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

(deftest for-share-ansi-rejected
  (ok (signals (compile-sql (select (columns :id) (from :t) (for-share))
                            :dialect (make-ansi-dialect))
               'sql-dialect-unsupported)))

(deftest create-trigger-ansi-emit
  (let ((sql (%sql (create-trigger :trg
                     :timing :before
                     :events '(:update)
                     :table :t
                     :for-each :row
                     :body (list (sql-fragment "UPDATE t SET n = n + 1"))))))
    (%assert-contains sql "CREATE TRIGGER" "BEFORE UPDATE" "FOR EACH ROW" "UPDATE")))
