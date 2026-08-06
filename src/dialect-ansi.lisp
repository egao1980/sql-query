(in-package #:sql-query)

(defclass ansi-dialect (sql-dialect) ()
  (:documentation "Full ANSI/ISO SQL Foundation dialect (SQL:2003+).
Vendor extensions live in sql-query-* backend systems."))

(defun make-ansi-dialect ()
  (make-instance 'ansi-dialect))

(defun use-ansi-dialect ()
  "Bind *SQL-DIALECT* to ANSI and register :ansi. Returns the dialect."
  (let ((d (make-ansi-dialect)))
    (setf *sql-dialect* d)
    (register-sql-dialect :ansi d)
    d))

;;; ANSI types — prefer standard names

(defmethod dialect-type-sql ((dialect ansi-dialect) type-spec)
  (cond
    ((null type-spec) "CHARACTER VARYING")
    ((stringp type-spec) type-spec)
    ((keywordp type-spec)
     (case type-spec
       ((:integer :int) "INTEGER")
       ((:bigint) "BIGINT")
       ((:smallint) "SMALLINT")
       ((:text :string :varchar) "CHARACTER VARYING")
       ((:char :character) "CHARACTER")
       ((:boolean :bool) "BOOLEAN")
       ((:real :float) "REAL")
       ((:double :double-precision) "DOUBLE PRECISION")
       ((:numeric :decimal) "NUMERIC")
       ((:blob :binary :bytea) "BINARY LARGE OBJECT")
       ((:clob) "CHARACTER LARGE OBJECT")
       ((:nclob) "NATIONAL CHARACTER LARGE OBJECT")
       ((:nchar :national-char) "NATIONAL CHARACTER")
       ((:nvarchar :national-varchar) "NATIONAL CHARACTER VARYING")
       ((:timestamp) "TIMESTAMP")
       ((:timestamptz) "TIMESTAMP WITH TIME ZONE")
       ((:time) "TIME")
       ((:timetz) "TIME WITH TIME ZONE")
       ((:date) "DATE")
       ((:interval) "INTERVAL")
       ((:xml) "XML")
       ((:boolean) "BOOLEAN")
       (otherwise (call-next-method))))
    ((and (consp type-spec) (eq (first type-spec) :varchar))
     (format nil "CHARACTER VARYING(~a)" (second type-spec)))
    ((and (consp type-spec) (member (first type-spec) '(:char :character)))
     (format nil "CHARACTER(~a)" (second type-spec)))
    ((and (consp type-spec) (eq (first type-spec) :interval))
     (format nil "INTERVAL ~{~a~^ ~}" (mapcar #'ident-string (rest type-spec))))
    ((and (consp type-spec) (member (first type-spec) '(:numeric :decimal)))
     (format nil "NUMERIC(~{~a~^,~})" (rest type-spec)))
    (t (call-next-method))))

;;; Pagination — SQL:2008 FETCH FIRST (not LIMIT)

(defmethod emit-limit-offset ((dialect ansi-dialect) lim off stream ctx)
  (when off
    (write-string " OFFSET " stream)
    (emit-sql dialect (lit (offset-count off)) stream ctx)
    (write-string " ROWS" stream))
  (when lim
    (write-string " FETCH FIRST " stream)
    (emit-sql dialect (lit (limit-count lim)) stream ctx)
    (write-string " ROWS ONLY" stream)))

;;; DISTINCT ON is not ANSI

(defmethod emit-distinct ((dialect ansi-dialect) clause stream ctx)
  (when (distinct-on clause)
    (error 'sql-dialect-unsupported
           :feature :distinct-on
           :dialect dialect
           :message "DISTINCT ON is not ANSI SQL — use sql-query-postgres"))
  (write-string "DISTINCT " stream))

;;; FOR UPDATE without vendor lock wait clauses

(defmethod emit-for-update ((dialect ansi-dialect) clause stream ctx)
  (when (or (for-update-nowait clause) (for-update-skip-locked clause))
    (error 'sql-dialect-unsupported
           :feature :for-update-wait
           :dialect dialect
           :message "NOWAIT/SKIP LOCKED are not ANSI SQL"))
  (when (and (for-update-strength clause)
             (not (eq (for-update-strength clause) :update)))
    (error 'sql-dialect-unsupported
           :feature :for-update-strength
           :dialect dialect
           :message "FOR SHARE / NO KEY UPDATE / KEY SHARE are not ANSI SQL"))
  (write-string " FOR UPDATE" stream)
  (when (for-update-of clause)
    (write-string " OF " stream)
    (emit-column-list dialect (for-update-of clause) stream ctx)))

;;; RETURNING is not ANSI (PostgreSQL / later vendors)

(defmethod emit-returning ((dialect ansi-dialect) items stream ctx)
  (declare (ignore items stream ctx))
  (error 'sql-dialect-unsupported
         :feature :returning
         :dialect dialect
         :message "RETURNING is not ANSI SQL — use a vendor dialect backend"))

;;; IF NOT EXISTS on CREATE TABLE is widespread but not classic ANSI —
;;; keep it (SQL:2023 has it for some objects); no change.

;;; SQL/PSM CREATE PROCEDURE / CALL

(defmethod emit-create-procedure ((dialect ansi-dialect) stmt stream ctx)
  (write-string "CREATE " stream)
  (when (create-procedure-or-replace stmt)
    (write-string "OR REPLACE " stream))
  (write-string "PROCEDURE " stream)
  (emit-ident dialect (create-procedure-name stmt) stream)
  (write-char #\( stream)
  (loop for (p . rest) on (create-procedure-params stmt)
        do (write-string (ecase (procedure-param-mode p)
                           (:in "IN ")
                           (:out "OUT ")
                           (:inout "INOUT "))
                         stream)
           (emit-ident dialect (procedure-param-name p) stream)
           (write-char #\Space stream)
           (write-string (dialect-type-sql dialect (procedure-param-type p)) stream)
           (when rest (write-string ", " stream)))
  (write-string ") BEGIN " stream)
  (dolist (form (create-procedure-body stmt))
    (emit-sql dialect form stream ctx)
    (write-char #\; stream))
  (write-string " END" stream))

(defmethod emit-call ((dialect ansi-dialect) stmt stream ctx)
  (write-string "CALL " stream)
  (emit-ident dialect (call-name stmt) stream)
  (write-char #\( stream)
  (loop for (arg . rest) on (call-args stmt)
        do (cond
             ((typep arg 'binary-op)
              (emit-sql dialect (binary-op-right arg) stream ctx))
             (t (emit-sql dialect (ensure-expr arg) stream ctx)))
           (when rest (write-string ", " stream)))
  (write-char #\) stream))

(use-ansi-dialect)
