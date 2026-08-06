(in-package #:sql-query-sqlite3)

;;; ---------------------------------------------------------------------------
;;; SQLite type / operator / function seeds
;;; JSON1 extension assumed available for json_* funcs.
;;; ---------------------------------------------------------------------------

(defun %json-encode (value)
  (if (stringp value) value (prin1-to-string value)))

(defun %dt-encode (value)
  (ctypecase value
    (string value)
    (integer value)
    (real value)))

(defun %sqlite-dt-to-expr (dialect value)
  "SQLite stores datetimes as TEXT/NUMERIC — emit quoted ISO or unix integer."
  (declare (ignore dialect))
  (ctypecase value
    (string (lit value))
    (integer (lit value))
    (real (lit value))))

(defun %bson-encode (value)
  (cond
    ((vectorp value) value)
    ((stringp value) value)
    (t (error "bson encode needs string or vector (got ~s)" value))))

(defun register-sqlite3-extensions (dialect)
  ;; ---- types (affinity-oriented names → SQLite storage classes) ----
  (register-sql-type :json dialect :sql "TEXT"
    :encode #'%json-encode :decode #'identity)
  (register-sql-type :jsonb dialect :sql "TEXT"
    :encode #'%json-encode :decode #'identity)
  (register-sql-type :bson dialect :sql "BLOB"
    :encode #'%bson-encode :decode #'identity)
  (register-sql-type :uuid dialect :sql "TEXT")
  (register-sql-type :blob dialect :sql "BLOB")
  (register-sql-type :date dialect :sql "TEXT"
    :encode #'%dt-encode :to-expr #'%sqlite-dt-to-expr)
  (register-sql-type :time dialect :sql "TEXT"
    :encode #'%dt-encode :to-expr #'%sqlite-dt-to-expr)
  (register-sql-type :timestamp dialect :sql "TEXT"
    :encode #'%dt-encode :to-expr #'%sqlite-dt-to-expr)
  (register-sql-type :timestamptz dialect :sql "TEXT"
    :encode #'%dt-encode :to-expr #'%sqlite-dt-to-expr)
  (register-sql-type :datetime dialect :sql "TEXT"
    :encode #'%dt-encode :to-expr #'%sqlite-dt-to-expr)
  (register-sql-type :interval dialect :sql "TEXT")
  ;; arrays — store as JSON text by default
  (register-sql-type :array dialect :sql "TEXT"
    :encode #'%json-encode
    :to-expr (lambda (d v)
               (declare (ignore d))
               (typed (if (stringp v) v (prin1-to-string v)) :json)))
  (register-sql-type :int-array dialect :sql "TEXT"
    :encode #'%json-encode)
  (register-sql-type :text-array dialect :sql "TEXT"
    :encode #'%json-encode)

  ;; ---- operators ----
  (register-sql-op :|||||| :binary dialect :sql "||")   ; concat
  ;; JSON arrow ops (SQLite 3.38+); prefer json-extract helper for older builds
  (register-sql-op :-> :binary dialect :sql "->")
  (register-sql-op :->> :binary dialect :sql "->>")
  (register-sql-op :% :binary dialect :sql "%")
  (register-sql-op :& :binary dialect :sql "&")
  (register-sql-op :|||| :binary dialect :sql "|")
  (register-sql-op :<< :binary dialect :sql "<<")
  (register-sql-op :>> :binary dialect :sql ">>")

  ;; ---- functions ----
  (dolist (pair '((:date . "date")
                  (:time . "time")
                  (:datetime . "datetime")
                  (:julianday . "julianday")
                  (:unixepoch . "unixepoch")
                  (:strftime . "strftime")
                  (:timediff . "timediff")
                  (:json . "json")
                  (:json-array . "json_array")
                  (:json-object . "json_object")
                  (:json-extract . "json_extract")
                  (:json-insert . "json_insert")
                  (:json-replace . "json_replace")
                  (:json-set . "json_set")
                  (:json-remove . "json_remove")
                  (:json-type . "json_type")
                  (:json-valid . "json_valid")
                  (:json-array-length . "json_array_length")
                  (:json-each . "json_each")
                  (:json-tree . "json_tree")
                  (:json-group-array . "json_group_array")
                  (:json-group-object . "json_group_object")
                  (:json-patch . "json_patch")
                  (:json-quote . "json_quote")
                  (:abs . "abs")
                  (:coalesce . "coalesce")
                  (:ifnull . "ifnull")
                  (:nullif . "nullif")
                  (:typeof . "typeof")
                  (:length . "length")
                  (:lower . "lower")
                  (:upper . "upper")
                  (:substr . "substr")
                  (:trim . "trim")
                  (:replace . "replace")
                  (:printf . "printf")
                  (:format . "format")
                  (:hex . "hex")
                  (:random . "random")
                  (:randomblob . "randomblob")
                  (:zeroblob . "zeroblob")
                  (:total-changes . "total_changes")
                  (:last-insert-rowid . "last_insert_rowid")
                  (:sqlite-version . "sqlite_version")))
    (register-sql-func (car pair) dialect :sql (cdr pair)))
  dialect)

;;; Helpers

(defun json-extract (expr path)
  (sql-func :json-extract expr path))

(defun json-object (&rest kvs)
  (apply #'sql-func :json-object kvs))

(defun json-array (&rest elts)
  (apply #'sql-func :json-array elts))

(defun json-set (target path value)
  (sql-func :json-set target path value))

(defun strftime (fmt expr)
  (sql-func :strftime fmt expr))

(defun datetime (&rest args)
  (apply #'sql-func :datetime args))

(defun unixepoch (&optional expr)
  (if expr (sql-func :unixepoch expr) (sql-func :unixepoch)))
