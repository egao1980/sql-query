(in-package #:sql-query)

;;; ============================================================================
;;; Procedural SQL — two layers
;;;
;;; 1) Lower (1–2–1 with SQL text): PROC-* nodes/constructors + EMIT-SQL
;;;      proc-if → IF … THEN … ELSE … END IF
;;;      proc-setf → SET x = …  /  x := … (postgres)
;;;      proc-while → WHILE … DO … END WHILE
;;;      proc-loop / proc-return → LOOP / LEAVE (EXIT on PG)
;;;      proc-let → DECLARE … BEGIN … END
;;;      proc-progn / proc-cond → BEGIN…END / IF/ELSEIF chain
;;;
;;; 2) Higher (lispy macros): BODY walks CL-shaped forms and expands into (1).
;;;      (body (if …) (let …) (setf …) (loop :while …) (cond …) (return))
;;;
;;; Use (1) for programmatic AST; (2) for readable procedure source.
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Layer 1 — constructors (SQL-shaped)
;;; ---------------------------------------------------------------------------

(defun coerce-proc-form (form)
  "Coerce FORM to a procedural/SQL node for layer-1 builders."
  (cond
    ((typep form 'sql-node) form)
    ((stringp form) (sql-raw form))
    ((and (consp form) (keywordp (car form))) (ensure-expr form))
    (t (ensure-expr form))))

(defun %proc-place (place)
  (ctypecase place
    (column-ref place)
    ((or symbol string) (col place))
    (sql-node place)))

(defun proc-progn (&rest forms)
  "BEGIN … END compound."
  (make-instance 'proc-progn :forms (mapcar #'coerce-proc-form forms)))

(defun proc-let (bindings forms &key sequential)
  "DECLARE bindings then run FORMS.
BINDINGS: ((name type &optional default-expr)…)."
  (make-instance 'proc-let
                 :bindings (mapcar (lambda (b)
                                     (destructuring-bind (name type &optional (default nil defaultp)) b
                                       (if defaultp
                                           (list name type (ensure-expr default))
                                           (list name type))))
                                   bindings)
                 :forms (mapcar #'coerce-proc-form (if (listp forms) forms (list forms)))
                 :sequential sequential))

(defun proc-if (test then &optional else)
  "IF test THEN then [ELSE else] END IF."
  (make-instance 'proc-if
                 :test (ensure-expr test)
                 :then (coerce-proc-form then)
                 :else (and else (coerce-proc-form else))))

(defun proc-cond (&rest clauses)
  "IF/ELSEIF/ELSE from CLAUSES: (test form*)… ; test = T for ELSE."
  (make-instance 'proc-cond
                 :clauses
                 (mapcar (lambda (c)
                           (destructuring-bind (test &rest body) c
                             (cons (if (or (eq test t)
                                           (and (symbolp test)
                                                (string= (symbol-name test) "T")))
                                       t
                                       (ensure-expr test))
                                   (mapcar #'coerce-proc-form body))))
                         clauses)))

(defun proc-setf (place value)
  "Assignment: SET place = value (ANSI) / place := value (postgres)."
  (make-instance 'proc-setf
                 :place (%proc-place place)
                 :value (ensure-expr value)))

(defun proc-while (test &rest forms)
  "WHILE test DO forms END WHILE."
  (make-instance 'proc-while
                 :test (ensure-expr test)
                 :forms (mapcar #'coerce-proc-form forms)))

(defun proc-until (test &rest forms)
  "REPEAT forms UNTIL test END REPEAT (ANSI) / WHILE NOT test LOOP (PG)."
  (make-instance 'proc-while
                 :test (ensure-expr test)
                 :until t
                 :forms (mapcar #'coerce-proc-form forms)))

(defun proc-loop (&rest forms)
  "Infinite LOOP forms END LOOP."
  (make-instance 'proc-loop :forms (mapcar #'coerce-proc-form forms)))

(defun proc-loop-labeled (label &rest forms)
  (make-instance 'proc-loop
                 :label label
                 :forms (mapcar #'coerce-proc-form forms)))

(defun proc-return (&optional label)
  "LEAVE [label] (ANSI) / EXIT [label] (postgres)."
  (make-instance 'proc-return :label label))

(defun make-body (&rest forms)
  "Layer-1 body clause — FORMS are already nodes (or coerced)."
  (make-instance 'body-clause
                 :forms (mapcar #'coerce-proc-form forms)))

(defun body* (&rest forms)
  "Alias for MAKE-BODY (runtime / layer-1)."
  (apply #'make-body forms))

;;; ---------------------------------------------------------------------------
;;; Layer 1 — emit (SQL/PSM). Postgres overrides in backend-postgres.
;;; ---------------------------------------------------------------------------

(defun %emit-proc-seq (dialect forms stream ctx &key (separator "; "))
  (loop for (f . rest) on forms
        do (emit-sql dialect f stream ctx)
           (when rest (write-string separator stream))))

(defmethod emit-sql ((dialect sql-dialect) (node proc-progn) stream ctx)
  (write-string "BEGIN " stream)
  (%emit-proc-seq dialect (proc-progn-forms node) stream ctx)
  (write-string " END" stream))

(defmethod emit-sql ((dialect sql-dialect) (node proc-let) stream ctx)
  (write-string "BEGIN " stream)
  (dolist (b (proc-let-bindings node))
    (destructuring-bind (name type &optional (default nil defaultp)) b
      (write-string "DECLARE " stream)
      (emit-ident dialect name stream)
      (write-char #\Space stream)
      (write-string (dialect-type-sql dialect type) stream)
      (when defaultp
        (write-string " DEFAULT " stream)
        (emit-sql dialect default stream ctx))
      (write-string "; " stream)))
  (%emit-proc-seq dialect (proc-let-forms node) stream ctx)
  (write-string " END" stream))

(defmethod emit-sql ((dialect sql-dialect) (node proc-if) stream ctx)
  (write-string "IF " stream)
  (emit-sql dialect (proc-if-test node) stream ctx)
  (write-string " THEN " stream)
  (emit-sql dialect (proc-if-then node) stream ctx)
  (when (proc-if-else node)
    (write-string " ELSE " stream)
    (emit-sql dialect (proc-if-else node) stream ctx))
  (write-string " END IF" stream))

(defmethod emit-sql ((dialect sql-dialect) (node proc-cond) stream ctx)
  (let ((clauses (proc-cond-clauses node)))
    (unless clauses (%err "cond with no clauses"))
    (loop for (clause . rest) on clauses
          for first = t then nil
          for test = (car clause)
          for forms = (cdr clause)
          do (cond
               ((and (null rest) (eq test t))
                (write-string " ELSE " stream)
                (%emit-proc-seq dialect forms stream ctx))
               (first
                (write-string "IF " stream)
                (emit-sql dialect test stream ctx)
                (write-string " THEN " stream)
                (%emit-proc-seq dialect forms stream ctx))
               (t
                (write-string " ELSEIF " stream)
                (emit-sql dialect test stream ctx)
                (write-string " THEN " stream)
                (%emit-proc-seq dialect forms stream ctx))))
    (write-string " END IF" stream)))

(defmethod emit-sql ((dialect sql-dialect) (node proc-setf) stream ctx)
  (write-string "SET " stream)
  (emit-ident dialect
              (if (typep (proc-setf-place node) 'column-ref)
                  (column-ref-name (proc-setf-place node))
                  (proc-setf-place node))
              stream)
  (write-string " = " stream)
  (emit-sql dialect (proc-setf-value node) stream ctx))

(defmethod emit-sql ((dialect sql-dialect) (node proc-while) stream ctx)
  (if (proc-while-until node)
      (progn
        (write-string "REPEAT " stream)
        (%emit-proc-seq dialect (proc-while-forms node) stream ctx)
        (write-string " UNTIL " stream)
        (emit-sql dialect (proc-while-test node) stream ctx)
        (write-string " END REPEAT" stream))
      (progn
        (write-string "WHILE " stream)
        (emit-sql dialect (proc-while-test node) stream ctx)
        (write-string " DO " stream)
        (%emit-proc-seq dialect (proc-while-forms node) stream ctx)
        (write-string " END WHILE" stream))))

(defmethod emit-sql ((dialect sql-dialect) (node proc-loop) stream ctx)
  (when (proc-loop-label node)
    (emit-ident dialect (proc-loop-label node) stream)
    (write-string ": " stream))
  (write-string "LOOP " stream)
  (%emit-proc-seq dialect (proc-loop-forms node) stream ctx)
  (write-string " END LOOP" stream))

(defmethod emit-sql ((dialect sql-dialect) (node proc-return) stream ctx)
  (declare (ignore ctx))
  (write-string "LEAVE" stream)
  (when (proc-return-label node)
    (write-char #\Space stream)
    (emit-ident dialect (proc-return-label node) stream)))

;;; ---------------------------------------------------------------------------
;;; Layer 2 — lispy macros → expand into layer-1 PROC-* calls
;;;
;;;   (body
;;;     (let ((tmp :integer 0))
;;;       (if (:= :n 0)
;;;           (setf :n :by)
;;;           (setf :n (:+ :n :by)))
;;;       (loop :while (:< :n 100) :do (setf :n (:+ :n 1)))
;;;       (cond ((:= :by 0) (return))
;;;             (t (setf :tmp :by)))))
;;;
;;; Heads matched by symbol-name (any package): IF WHEN UNLESS PROGN LET LET*
;;; SETF SETQ COND LOOP WHILE RETURN RETURN-FROM BLOCK.
;;; Other forms evaluate as normal sql-query calls (UPDATE, sql-fragment, …).
;;; ---------------------------------------------------------------------------

(defun %form-key (form)
  (when (and (consp form) (symbolp (car form)))
    (intern (symbol-name (car form)) :keyword)))

(defun %expand-bindings (bindings)
  `(list
    ,@(mapcar (lambda (b)
                (unless (and (consp b) (car b) (cadr b))
                  (%err "let binding must be (var type &optional default), got ~s" b))
                (destructuring-bind (name type &optional (default nil defaultp)) b
                  (if defaultp
                      `(list ',name ',type ,default)
                      `(list ',name ',type))))
              bindings)))

(defun expand-proc-form (form)
  "Macro-expand a lispy form into code that builds layer-1 PROC-* nodes."
  (cond
    ((typep form 'sql-node) form)
    ((stringp form) `(sql-raw ,form))
    ((atom form) `(coerce-proc-form ',form))
    (t
     (let ((key (%form-key form))
           (args (cdr form)))
       (case key
         (:progn
          `(proc-progn ,@(mapcar #'expand-proc-form args)))
         (:block
          (destructuring-bind (label &rest body) args
            `(proc-loop-labeled ',label ,@(mapcar #'expand-proc-form body))))
         ((:let :let*)
          (destructuring-bind (bindings &rest body) args
            `(proc-let ,(%expand-bindings bindings)
                       (list ,@(mapcar #'expand-proc-form body))
                       :sequential ,(eq key :let*))))
         (:if
          (destructuring-bind (test then &optional (else nil elsep)) args
            `(proc-if ,test
                      ,(expand-proc-form then)
                      ,@(when elsep (list (expand-proc-form else))))))
         (:when
          (destructuring-bind (test &rest body) args
            (let ((forms (mapcar #'expand-proc-form body)))
              `(proc-if ,test ,(if (= (length forms) 1)
                                   (first forms)
                                   `(proc-progn ,@forms))))))
         (:unless
          (destructuring-bind (test &rest body) args
            (let ((forms (mapcar #'expand-proc-form body)))
              `(proc-if (sql-not ,test)
                        ,(if (= (length forms) 1)
                             (first forms)
                             `(proc-progn ,@forms))))))
         ((:setf :setq)
          (unless (evenp (length args))
            (%err "setf needs place/value pairs, got ~s" form))
          (let ((pairs (loop for (p v) on args by #'cddr
                             collect `(proc-setf ,p ,v))))
            (if (= (length pairs) 1)
                (first pairs)
                `(proc-progn ,@pairs))))
         (:cond
          `(proc-cond
            ,@(mapcar (lambda (clause)
                        (destructuring-bind (test &rest body) clause
                          `(list ,(if (or (eq test t)
                                          (and (symbolp test)
                                               (string= (symbol-name test) "T")))
                                      t
                                      test)
                                 ,@(mapcar #'expand-proc-form body))))
                      args)))
         (:while
          (destructuring-bind (test &rest body) args
            `(proc-while ,test ,@(mapcar #'expand-proc-form body))))
         (:loop
          (%expand-loop args))
         (:return
          `(proc-return))
         (:return-from
          (destructuring-bind (label &optional _value) args
            (declare (ignore _value))
            `(proc-return ',label)))
         (otherwise
          form))))))

(defun %expand-loop (args)
  (cond
    ((null args)
     `(proc-loop))
    ((and (keywordp (first args))
          (member (first args) '(:while :until) :test #'eq))
     (let* ((kind (first args))
            (test (second args))
            (rest (cddr args))
            (body (if (and rest (member (first rest) '(:do :then) :test #'eq))
                      (cdr rest)
                      rest)))
       (if (eq kind :until)
           `(proc-until ,test ,@(mapcar #'expand-proc-form body))
           `(proc-while ,test ,@(mapcar #'expand-proc-form body)))))
    ((and (keywordp (first args)) (eq (first args) :do))
     `(proc-loop ,@(mapcar #'expand-proc-form (cdr args))))
    (t
     `(proc-loop ,@(mapcar #'expand-proc-form args)))))

(defmacro body (&body forms)
  "Layer-2 lispy procedure body — expands into MAKE-BODY + PROC-*."
  `(make-body ,@(mapcar #'expand-proc-form forms)))
