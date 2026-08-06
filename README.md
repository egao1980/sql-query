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

## Extension registry (types & operators)

Custom SQL types own **Lisp ↔ expression** conversion — not only DDL names:

```lisp
(register-sql-type :money dialect
  :sql "DECIMAL(19,4)"
  :encode #'money-to-wire          ; Lisp → bind value
  :decode #'money-from-wire        ; result → Lisp
  :to-expr (lambda (d v) …)        ; Lisp → sql-node (preferred write)
  :emit-value (lambda (d v s ctx) …)) ; or full emit control

(typed value :money)               ; or (lit value :money) — inlined via type encode
(bindparam :x v :type :money)      ; explicit placeholder (+ encode)
(bindparam :lim :default 10)       ; COALESCE(?, 10) — literal default
(sql-type-write dialect :money v)  ; → sql-expr
(sql-type-read dialect :money db)  ; → Lisp

(register-sql-op :->> :binary dialect :sql "->>")
(ensure-expr '(:->> :payload "name"))
```

**Literals vs params:** `lit` / bare values always emit SQL literal text (typed → encode then literal/`CAST`). Placeholders (`?` / `$n`) only from `bindparam` or `sql-fragment` `?` slots.

## Procedural SQL (two layers)

**Layer 1** — SQL-shaped nodes (`proc-if`, `proc-setf`, `proc-while`, `proc-loop`, `proc-let`, …) map 1–2–1 onto SQL/PSM / plpgsql text via `emit-sql`.

**Layer 2** — lispy `body` macro expands CL-shaped forms into layer 1:

```lisp
(create-procedure :bump
  (params (in :by :integer) (inout :n :integer))
  (body
   (let ((tmp :integer 0))
     (if (:= :n 0)
         (setf :n :by)
         (setf :n (:+ :n :by)))
     (loop :while (:< :tmp 3) :do (setf :tmp (:+ :tmp 1)))
     (when (:> :n 1000) (return)))))
```

Use `make-body` + `proc-*` when building ASTs programmatically. Sqlite: unsupported.

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
