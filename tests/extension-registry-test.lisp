(in-package #:sql-query/tests)

(deftest register-type-ddl-and-cast
  (let ((d (make-ansi-dialect)))
    (register-sql-type :money d :sql "DECIMAL(19,4)")
    (ok (string= "DECIMAL(19,4)" (dialect-type-sql d :money)))
    (let ((sql (%sql (select (columns (sql-cast :amount :money)) (from :t)) d)))
      (%assert-contains sql "CAST(" "AS DECIMAL(19,4)"))))

(deftest type-write-default-cast-encode
  (let ((d (make-ansi-dialect)))
    (register-sql-type :tag d
      :sql "CHARACTER VARYING"
      :encode (lambda (v) (format nil "<~a>" v))
      :decode (lambda (v) (string-trim "<>" v)))
    (multiple-value-bind (sql params)
        (compile-sql (select (columns (typed "x" :tag)) (from :t)) :dialect d)
      (%assert-contains sql "CAST(" "AS CHARACTER VARYING" "'<x>'")
      (ok (null params) "typed literals inline after encode"))
    (ok (string= "x" (sql-type-read d :tag "<x>")))
    (ok (string= "<hi>" (encode-sql-value d :tag "hi")))))

(deftest type-to-expr-custom-write
  (let ((d (make-ansi-dialect)))
    (register-sql-type :point d
      :sql "POINT"
      :to-expr (lambda (dialect value)
                 (declare (ignore dialect))
                 (sql-func "POINT" (first value) (second value))))
    (let ((sql (%sql (select (columns (typed '(1 2) :point)) (from :t)) d)))
      (%assert-contains sql "POINT(" "POINT(1, 2)"))))

(deftest type-emit-value-override
  (let ((d (make-ansi-dialect)))
    (register-sql-type :raw-int d
      :sql "INTEGER"
      :emit-value (lambda (dialect value stream ctx)
                    (declare (ignore dialect ctx))
                    (format stream "~d" value)))
    (let ((sql (%sql (select (columns (lit 42 :raw-int)) (from :t)) d)))
      (ok (%has sql "42"))
      (ok (null (%params (select (columns (lit 42 :raw-int)) (from :t)) d))))))

(deftest lit-and-bindparam-typed
  (let ((d (make-ansi-dialect)))
    (register-sql-type :jsonish d
      :sql "JSON"
      :encode (lambda (v) (format nil "~a" v)))
    (let ((lit-sql (%sql (select (columns (lit "{\"a\":1}" :jsonish)) (from :t)) d)))
      (%assert-contains lit-sql "CAST(" "'{\"a\":1}'" "AS JSON")
      (ok (null (%params (select (columns (lit "{\"a\":1}" :jsonish)) (from :t)) d))))
    (ok (equal '("{\"a\":1}")
               (%params (select (columns (bindparam :j "{\"a\":1}" :type :jsonish))
                                (from :t))
                        d))
        "bindparam is the explicit placeholder path")))

(deftest sql-type-write-helper
  (let ((d (make-ansi-dialect)))
    (register-sql-type :uuid d :sql "UUID"
      :encode #'identity)
    (ok (typep (sql-type-write d :uuid "00000000-0000-0000-0000-000000000000")
               'typed-value))))

(deftest register-op-binary-emit-and-parse
  (let ((d (make-ansi-dialect)))
    (register-sql-op :@@ :binary d :sql "@@")
    (let* ((stmt (select (columns :id) (from :docs)
                         (where (ensure-expr '(:@@ :body "lisp")))))
           (sql (%sql stmt d)))
      (%assert-contains sql "@@" "'lisp'")
      (ok (%has sql "\"body\"")))))

(deftest register-op-custom-emit
  (let ((d (make-ansi-dialect)))
    (register-sql-op :->>> :binary d
      :sql "->>>"
      :emit (lambda (dialect node stream ctx)
              (emit-sql dialect (binary-op-left node) stream ctx)
              (write-string " ->>> " stream)
              (emit-sql dialect (binary-op-right node) stream ctx)))
    (let ((sql (%sql (select (columns (ensure-expr '(:->>> :j "k"))) (from :t)) d)))
      (ok (%has sql "->>>"))
      (ok (%has sql "\"j\""))
      (%assert-contains sql "'k'"))))

(deftest unknown-op-still-errors
  (ok (signals (ensure-expr '(:definitely-not-an-op :a 1))
               'sql-query-error)))

(deftest postgres-seeded-jsonb-and-ops
  (let ((d (sql-query-postgres:make-postgres-dialect)))
    (ok (string= "JSONB" (dialect-type-sql d :jsonb)))
    (ok (string= "JSON" (dialect-type-sql d :json)))
    (ok (string= "INTEGER[]" (dialect-type-sql d '(:array :integer))))
    (multiple-value-bind (sql params)
        (compile-sql
         (select (columns (ensure-expr '(:->> :payload "name")))
                 (from :events)
                 (where (ensure-expr `(:@> :payload ,(typed "{\"ok\":true}" :jsonb)))))
         :dialect d)
      (%assert-contains sql "->>" "@>" "CAST(" "AS JSONB" "'name'" "'{\"ok\":true}'")
      (ok (null params)))))
