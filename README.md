# sql-query

Composable **CLOS** SQL DSL for [cl-stack](https://github.com/egao1980/cl-stack) — dialects, DML + DDL + procedures, nestable `sql-fragment`.

Nick: **`stack-sql-query`**. Layer 2 over [`sql-protocol`](https://github.com/egao1980/sql-protocol). Brief: [sql.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md).

## Quick use

```lisp
(asdf:load-system "sql-query")
(asdf:load-system "sql-backend-sqlite3")

(use-package :sql-query)

(multiple-value-bind (sql params)
    (compile-sql
     (select (columns :id :name)
             (from :users)
             (where (sql-and (:= :active 1)
                             (sql-fragment "created_at > ?" "2020-01-01")))
             (order-by :name)
             (limit 10))
     :dialect (make-sqlite3-dialect))
  (list sql params))

(sql-protocol:with-connection (c :driver :sqlite3 :database-name ":memory:")
  (execute-query c (create-table :users
                      (column :id :type :integer :primary-key t :autoincrement t)
                      (column :name :type :text)))
  (execute-query c (insert-into :users (columns :name) (sql-values "ada")))
  (fetch-all-query c (select (columns :id :name) (from :users))))
```

Postgres dialect uses `$1` placeholders; `CREATE PROCEDURE` / `CALL` supported. SQLite signals `sql-dialect-unsupported` for procedures.

## API shape

- Constructors return CLOS AST nodes (`select`, `insert-into`, `create-table`, …)
- Compose with `and-where` / `merge-query` (immutable copies)
- `compile-sql` → `(values string params)`
- `execute-query` / `fetch-query` / `fetch-all-query` via sql-protocol
- Raw escape hatch: `(sql-fragment "… ?" arg)` — nestable; params only (no string-interp of values)

SxQL is **not** the public API (Mito interop only at the ORM layer).

## Tests

```bash
ros -e '(asdf:test-system "sql-query")' -q
```

## License

MIT — see [LICENSE](LICENSE).
