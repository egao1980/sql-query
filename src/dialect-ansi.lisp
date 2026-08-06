(in-package #:sql-query)

(defclass ansi-dialect (sql-dialect) ()
  (:documentation "Builtin ANSI/ISO SQL dialect (SQL:2003-ish). Vendor dialects subclass or specialize separately."))

(defun make-ansi-dialect ()
  (make-instance 'ansi-dialect))

(defun use-ansi-dialect ()
  "Bind *SQL-DIALECT* to ANSI and register :ansi. Returns the dialect."
  (let ((d (make-ansi-dialect)))
    (setf *sql-dialect* d)
    (register-sql-dialect :ansi d)
    d))

(use-ansi-dialect)
