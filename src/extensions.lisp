(in-package #:sql-query)

;;; ---------------------------------------------------------------------------
;;; Dialect extension registries — custom SQL types & operators
;;;
;;; Types own Lisp ↔ SQL conversion:
;;;   :sql        type name in DDL / CAST
;;;   :encode     Lisp → bindable wire value
;;;   :decode     wire / result → Lisp
;;;   :to-expr    (dialect value) → sql-node  (preferred write path)
;;;   :emit-value (dialect value stream ctx)  (low-level write override)
;;;
;;; JSON/BSON/arrays stay out of core; backends/extensions register them.
;;; ---------------------------------------------------------------------------

(defclass sql-type-def ()
  ((name :initarg :name :reader sql-type-name)
   (sql :initarg :sql :reader sql-type-sql-text
        :documentation "String or (dialect type-spec) → string.")
   (encode :initarg :encode :initform nil :reader sql-type-encode-fn)
   (decode :initarg :decode :initform nil :reader sql-type-decode-fn)
   (to-expr :initarg :to-expr :initform nil :reader sql-type-to-expr-fn
            :documentation "(dialect value) → sql-node")
   (emit-value :initarg :emit-value :initform nil :reader sql-type-emit-value-fn
               :documentation "(dialect value stream ctx) — full emit control")))

(defclass sql-op-def ()
  ((name :initarg :name :reader sql-op-name)
   (arity :initarg :arity :reader sql-op-arity) ; :binary :unary :nary
   (sql :initarg :sql :initform nil :reader sql-op-sql-text)
   (emit :initarg :emit :initform nil :reader sql-op-emit-fn
         :documentation "(dialect node stream ctx) — full emit control")
   (parse :initarg :parse :initform nil :reader sql-op-parse-fn
          :documentation "(args) → sql-node")))

(defvar *sql-op-catalog* (make-hash-table :test #'eq)
  "Global op name → sql-op-def (arity/parse) so parse-expr works before compile dialect is chosen.")

(defun %type-key (type-spec)
  (cond
    ((keywordp type-spec) type-spec)
    ((and (consp type-spec) (keywordp (first type-spec))) (first type-spec))
    (t nil)))

(defun %resolve-dialect-target (target)
  (ctypecase target
    (sql-dialect target)
    (keyword
     (or (gethash target *sql-dialect-registry*)
         (%err "no dialect registered for ~s — load backend or pass a dialect instance" target)))))

(defun find-sql-type (dialect name)
  "Return sql-type-def for NAME on DIALECT, or NIL."
  (gethash name (dialect-type-registry dialect)))

(defun find-sql-op (dialect name)
  "Return sql-op-def for NAME on DIALECT, or NIL."
  (gethash name (dialect-op-registry dialect)))

(defun find-sql-op-catalog (name)
  "Arity/parse catalog entry for NAME (dialect-agnostic)."
  (gethash name *sql-op-catalog*))

(defun register-sql-type (name target &key sql encode decode to-expr emit-value)
  "Register SQL type NAME on TARGET (dialect instance or driver keyword).

SQL — string or (lambda (dialect type-spec) string) for DDL/CAST text.
ENCODE — (lambda (value) wire) applied when binding untyped placeholders.
DECODE — (lambda (wire) lisp) for result conversion via SQL-TYPE-READ.
TO-EXPR — (lambda (dialect value) sql-node); preferred Lisp→expression write path.
EMIT-VALUE — (lambda (dialect value stream ctx)); overrides TO-EXPR when set."
  (check-type name keyword)
  (unless sql
    (%err "register-sql-type ~s requires :sql" name))
  (let* ((dialect (%resolve-dialect-target target))
         (def (make-instance 'sql-type-def
                             :name name
                             :sql sql
                             :encode encode
                             :decode decode
                             :to-expr to-expr
                             :emit-value emit-value)))
    (setf (gethash name (dialect-type-registry dialect)) def)
    def))

(defun register-sql-op (name arity target &key sql emit parse)
  "Register operator NAME with ARITY (:binary :unary :nary) on TARGET.

SQL — infix/prefix text (default: downcased symbol name, e.g. :->> → \"->>\").
EMIT — (lambda (dialect node stream ctx)) full control.
PARSE — (lambda (args) sql-node); else arity builds binary/unary/nary-op."
  (check-type name keyword)
  (unless (member arity '(:binary :unary :nary) :test #'eq)
    (%err "register-sql-op arity must be :binary, :unary, or :nary (got ~s)" arity))
  (let* ((dialect (%resolve-dialect-target target))
         (sql-text (or sql (string-downcase (symbol-name name))))
         (def (make-instance 'sql-op-def
                             :name name
                             :arity arity
                             :sql sql-text
                             :emit emit
                             :parse parse)))
    (setf (gethash name (dialect-op-registry dialect)) def)
    ;; Catalog keeps last-registered arity/parse for parse-expr.
    (setf (gethash name *sql-op-catalog*) def)
    def))

(defun %render-type-sql (dialect def type-spec)
  (let ((sql (sql-type-sql-text def)))
    (if (functionp sql)
        (funcall sql dialect type-spec)
        sql)))

(defun registered-type-sql (dialect type-spec)
  "If TYPE-SPEC is registered on DIALECT, return its SQL text; else NIL."
  (let* ((key (%type-key type-spec))
         (def (and key (find-sql-type dialect key))))
    (when def
      (%render-type-sql dialect def type-spec))))

(defmethod dialect-type-sql :around ((dialect sql-dialect) type-spec)
  (or (registered-type-sql dialect type-spec)
      (call-next-method)))

(defun encode-sql-value (dialect type-spec value)
  "Encode VALUE for TYPE-SPEC using the type's :encode hook (identity if absent)."
  (let* ((key (%type-key type-spec))
         (def (and key (find-sql-type dialect key)))
         (fn (and def (sql-type-encode-fn def))))
    (if fn (funcall fn value) value)))

(defun sql-type-read (dialect type-spec db-value)
  "Decode DB-VALUE to Lisp via the type's :decode hook (identity if absent)."
  (let* ((key (%type-key type-spec))
         (def (and key (find-sql-type dialect key)))
         (fn (and def (sql-type-decode-fn def))))
    (if fn (funcall fn db-value) db-value)))

(defun sql-type-write (dialect type-spec value)
  "Write Lisp VALUE as an sql-expr under TYPE-SPEC.

Order: :to-expr → else typed-value node (emit uses :emit-value / encode+CAST)."
  (let* ((key (%type-key type-spec))
         (def (and key (find-sql-type dialect key))))
    (unless def
      (%err "no sql type ~s registered on ~a" type-spec
            (class-name (class-of dialect))))
    (let ((to-expr (sql-type-to-expr-fn def)))
      (if to-expr
          (funcall to-expr dialect value)
          (make-instance 'typed-value :value value :sql-type type-spec)))))

(defun typed (value type)
  "Tag VALUE with SQL TYPE. Emit uses the dialect's type adapter at compile time."
  (make-instance 'typed-value :value value :sql-type type))

(defun emit-typed-value (dialect type-spec value stream ctx)
  "Emit VALUE for TYPE-SPEC: :emit-value, else :to-expr, else CAST(? AS type) + encode."
  (let* ((key (%type-key type-spec))
         (def (and key (find-sql-type dialect key))))
    (cond
      ((null def)
       ;; Unregistered: bind raw (same as lit).
       (emit-placeholder dialect stream ctx)
       (push-param ctx value))
      ((sql-type-emit-value-fn def)
       (funcall (sql-type-emit-value-fn def) dialect value stream ctx))
      ((sql-type-to-expr-fn def)
       (emit-sql dialect (funcall (sql-type-to-expr-fn def) dialect value) stream ctx))
      (t
       ;; Default write: CAST(? AS <type>) with optional encode.
       (write-string "CAST(" stream)
       (emit-placeholder dialect stream ctx)
       (push-param ctx (encode-sql-value dialect type-spec value))
       (write-string " AS " stream)
       (write-string (%render-type-sql dialect def type-spec) stream)
       (write-char #\) stream)))))

(defun parse-registered-op (op-key args)
  "Build AST for a catalog-registered operator, or NIL if unknown."
  (let ((def (find-sql-op-catalog op-key)))
    (when def
      (if (sql-op-parse-fn def)
          (funcall (sql-op-parse-fn def) args)
          (ecase (sql-op-arity def)
            (:binary
             (unless (= (length args) 2)
               (%err "operator ~s expects 2 args, got ~a" op-key (length args)))
             (make-instance 'binary-op
                            :op op-key
                            :left (ensure-expr (first args))
                            :right (ensure-expr (second args))))
            (:unary
             (unless (= (length args) 1)
               (%err "operator ~s expects 1 arg, got ~a" op-key (length args)))
             (make-instance 'unary-op
                            :op op-key
                            :operand (ensure-expr (first args))))
            (:nary
             (make-instance 'nary-op
                            :op op-key
                            :operands (mapcar #'ensure-expr args))))))))
