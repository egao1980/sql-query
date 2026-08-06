(in-package #:sql-query)

(defgeneric copy-sql (node)
  (:documentation "Copy of an SQL statement for immutable composition."))

(defmethod copy-sql ((node select-statement))
  (make-instance 'select-statement :clauses (copy-list (statement-clauses node))))

(defmethod copy-sql ((node insert-statement))
  (make-instance 'insert-statement
                 :table (insert-table node)
                 :clauses (copy-list (statement-clauses node))))

(defmethod copy-sql ((node update-statement))
  (make-instance 'update-statement
                 :table (update-table node)
                 :clauses (copy-list (statement-clauses node))))

(defmethod copy-sql ((node delete-statement))
  (make-instance 'delete-statement
                 :table (delete-table node)
                 :clauses (copy-list (statement-clauses node))))

(defun %find-clause (clauses type)
  (find-if (lambda (c) (typep c type)) clauses))

(defun %remove-clause (clauses type)
  (remove-if (lambda (c) (typep c type)) clauses))

(defun and-where (statement expr)
  "Return a copy of STATEMENT with EXPR AND-ed into its WHERE clause."
  (check-type statement sql-statement)
  (let* ((copy (copy-sql statement))
         (clauses (statement-clauses copy))
         (existing (%find-clause clauses 'where-clause))
         (new-expr (ensure-expr expr))
         (combined (if existing
                       (sql-and (where-expr existing) new-expr)
                       new-expr)))
    (setf (statement-clauses copy)
          (append (%remove-clause clauses 'where-clause)
                  (list (where combined))))
    copy))

(defun merge-query (statement &rest clauses)
  "Return a copy of STATEMENT with CLAUSES appended (replacing same-typed clauses)."
  (check-type statement sql-statement)
  (let* ((copy (copy-sql statement))
         (result (copy-list (statement-clauses copy))))
    (dolist (c clauses)
      (let ((replace-type
              (typecase c
                (where-clause 'where-clause)
                (limit-clause 'limit-clause)
                (offset-clause 'offset-clause)
                (columns-clause 'columns-clause)
                (order-by-clause 'order-by-clause)
                (having-clause 'having-clause)
                (group-by-clause 'group-by-clause)
                (t nil))))
        (setf result
              (if replace-type
                  (append (%remove-clause result replace-type) (list c))
                  (append result (list c))))))
    (setf (statement-clauses copy) result)
    copy))
