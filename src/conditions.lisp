(in-package #:sql-query)

(define-condition sql-query-error (error)
  ((message :initarg :message :reader sql-query-error-message :initform nil))
  (:report (lambda (c s)
             (format s "sql-query error~@[: ~a~]" (sql-query-error-message c)))))

(define-condition sql-dialect-unsupported (sql-query-error)
  ((feature :initarg :feature :reader sql-unsupported-feature)
   (dialect :initarg :dialect :reader sql-unsupported-dialect))
  (:report (lambda (c s)
             (format s "sql-query: ~a unsupported on ~a~@[: ~a~]"
                     (sql-unsupported-feature c)
                     (class-name (class-of (sql-unsupported-dialect c)))
                     (sql-query-error-message c)))))

(defun %err (fmt &rest args)
  (error 'sql-query-error :message (apply #'format nil fmt args)))
