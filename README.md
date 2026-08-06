# sql-query

Composable **CLOS** SQL DSL for [cl-stack](https://github.com/egao1980/cl-stack) — **SQLAlchemy Core**–shaped expression language.

Nick: **`stack-sql-query`**. Brief: [sql.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md).

| System | Role |
|--------|------|
| `sql-query` | AST + constructors + **ANSI** dialect (only builtin) |
| `sql-query-sqlite3` | SQLite dialect backend |
| `sql-query-postgres` | PostgreSQL dialect backend |

Same protocol/backend split as `sql-protocol` / `sql-backend-*`.

## Quick use

```lisp
(asdf:load-system "sql-query")
(asdf:load-system "sql-query-sqlite3")
(asdf:load-system "sql-backend-sqlite3")

(multiple-value-bind (sql params)
    (compile-sql
     (select (columns :id (label (count :*) :n))
             (from :users)
             (where (sql-and (:= :active 1)
                             (sql-fragment "created_at > ?" "2020-01-01")))
             (order-by :name)
             (limit 10))
     :dialect (sql-query-sqlite3:make-sqlite3-dialect))
  (list sql params))
```

Default compile dialect is **ANSI** (`?`, `CHARACTER VARYING`, `GENERATED … AS IDENTITY`). Load a backend to register `:sqlite3` / `:postgres` for `dialect-for-connection`.

## Core surface (wave-1)

`select` / `insert-into` / `update` / `delete-from` · joins · `distinct` · `cte`/`with-cte` · `union*`/`intersect*`/`except*` · `exists`/`subquery` · `sql-case` · `sql-cast` · `sql-func`/`count` · `bindparam` · `label` · `sql-between` · arithmetic · `for-update` · DDL · `sql-fragment` · `make-sql-table` / `create-table-from`.

SxQL is **not** the public API.

## Tests

First-party Rove suites (not vendored NIST/sqltest/slt):

- `tests/ansi-compliance.lisp` — ANSI/ISO Foundation **emit** coverage + corner cases + negative vendorisms
- `tests/sql-query-test.lisp` — Core DSL smoke
- `tests/dialect-backend-test.lisp` — sqlite3 / postgres backends

```bash
ros -e '(asdf:test-system "sql-query")' -q
```

## License

MIT — see [LICENSE](LICENSE).
