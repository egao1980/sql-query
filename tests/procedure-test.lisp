(in-package #:sql-query/tests)

;;;; Layer 1 — SQL-shaped PROC-* (1-2-1 with emit)

(deftest layer1-proc-if-setf
  (let ((sql (%sql (create-procedure :p
                       (params)
                       (make-body (proc-if (:= :a 1)
                                           (proc-setf :b 2)
                                           (proc-setf :b 3)))))))
    (%assert-contains sql "IF " "THEN" "ELSE" "SET " "END IF")))

(deftest layer1-proc-while-loop-return
  (let ((sql (%sql (create-procedure :p
                       (params)
                       (make-body
                        (proc-while (:< :n 10) (proc-setf :n (:+ :n 1)))
                        (proc-loop (proc-if (:> :n 100) (proc-return))
                                   (proc-setf :n (:+ :n 1))))))))
    (%assert-contains sql "WHILE " "END WHILE" "LOOP " "END LOOP" "LEAVE")))

;;;; Layer 2 — lispy BODY expands into layer 1

(deftest layer2-lispy-if-setf-let-ansi
  (let* ((stmt (create-procedure :bump
                  (params (in :by :integer) (inout :n :integer))
                  (body
                   (let ((tmp :integer 0))
                     (if (:= :n 0)
                         (setf :n :by)
                         (setf :n (:+ :n :by)))
                     (setf :tmp :n)))))
         (sql (%sql stmt)))
    (%assert-contains sql
                      "CREATE PROCEDURE" "BEGIN" "DECLARE" "INTEGER"
                      "DEFAULT 0" "IF " "THEN" "ELSE" "END IF" "SET "
                      "END")
    ;; Procedure bodies inline constants — no bind placeholders.
    (ng (find #\? sql :test #'char=))
    (ok (null (%params stmt)))))

(deftest layer2-lispy-cond-while-loop-return
  (let* ((stmt (create-procedure :walk
                  (params (inout :n :integer))
                  (body
                   (cond
                     ((:= :n 0) (setf :n 1))
                     ((:< :n 0) (setf :n 0))
                     (t (setf :n (:+ :n 1))))
                   (loop :while (:< :n 10)
                         :do (setf :n (:+ :n 1)))
                   (loop
                     (when (:> :n 100) (return))
                     (setf :n (:+ :n 1))))))
         (sql (%sql stmt)))
    (%assert-contains sql
                      "IF " "ELSEIF " "ELSE " "END IF"
                      "WHILE " "END WHILE"
                      "LOOP " "END LOOP" "LEAVE"
                      "= 0" "= 1" "< 10" "> 100")
    (ng (find #\? sql :test #'char=))
    (ok (null (%params stmt)))))

(deftest layer2-still-accepts-fragments
  (let ((sql (%sql (create-procedure :bump
                       (params (in :by :integer))
                       (body (sql-fragment "UPDATE counters SET n = n + ?" 1))))))
    (%assert-contains sql "UPDATE counters" "BEGIN" "END")))

(deftest layer2-postgres-plpgsql
  (let* ((d (sql-query-postgres:make-postgres-dialect))
         (stmt (create-procedure :bump
                  (params (in :by :integer) (inout :n :integer))
                  (body
                   (let ((tmp :integer 0))
                     (if (:= :n 0)
                         (setf :n :by)
                         (setf :n (:+ :n :by)))
                     (loop :while (:< :tmp 3)
                           :do (setf :tmp (:+ :tmp 1)))
                     (when (:> :n 1000) (return))))))
         (sql (nth-value 0 (compile-sql stmt :dialect d))))
    (%assert-contains sql
                      "LANGUAGE plpgsql" "AS $$" "DECLARE" ":="
                      "IF " "THEN" "ELSE" "END IF"
                      "WHILE " "LOOP " "END LOOP" "EXIT"
                      "INTEGER := 0")
    (%assert-absent sql "END WHILE" "LEAVE")
    (ng (search "$1" sql) "no bind placeholders ($$ is plpgsql quoting)")
    (ok (null (nth-value 1 (compile-sql stmt :dialect d))))))
