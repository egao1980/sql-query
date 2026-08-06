# sql-query

Composable **CLOS** SQL DSL for [cl-stack](https://github.com/egao1980/cl-stack) — **SQLAlchemy Core** feature checklist (APIs stay Lisp).

**Version:** `0.2.0` (OCI publish pending GitHub Actions minutes).  
Nick: **`stack-sql-query`**. Brief: [sql.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md).

| Project | Role |
|---------|------|
| [`sql-query`](https://github.com/egao1980/sql-query) | AST + constructors + **ANSI** dialect (only builtin) |
| [`sql-query-sqlite3`](https://github.com/egao1980/sql-query-sqlite3) | SQLite dialect backend (separate repo) |
| [`sql-query-postgres`](https://github.com/egao1980/sql-query-postgres) | PostgreSQL dialect backend (separate repo) |

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

Default compile dialect is **ANSI** (`?`, `CHARACTER VARYING`, `GENERATED … AS IDENTITY`). Load a dialect backend project to register `:sqlite3` / `:postgres` for `dialect-for-connection`.

## Dialect AST extension points

Vendor dialects add non-standard AST **without** editing core `typecase`s:

| Hook | Use |
|------|-----|
| subclass `sql-extension` / `sql-clause` / `sql-statement` | define nodes in the dialect package |
| `emit-sql` on `(dialect-class node-class)` | top-level statement / expr emit |
| `emit-alter-table-action` | open `ALTER TABLE` actions |
| `emit-alter-type-action` | open `ALTER TYPE` actions |
| `emit-create-type-kind` | open `CREATE TYPE` kinds (`eql :enum`, `eql :base`, …) |
| `emit-create-table-extra` | trailing `CREATE TABLE` bits (`INHERITS`, …) |
| `emit-insert-prefix` / `emit-insert-extras` | `INSERT OR …` / `ON CONFLICT` hooks |
| `emit-trigger-execute` | `EXECUTE FUNCTION` / `PROCEDURE` / body |
| `for-share` / `for-update :strength` | row lock strengths |
| `emit-extension` | family fallback for a dialect |
| `register-sql-extension` | optional keyword → constructor registry |

```lisp
(defclass pg-inherits (sql-extension)
  ((parent :initarg :parent :reader pg-inherits-parent)))

(defmethod emit-create-table-extra ((d postgres-dialect) (x pg-inherits) stream ctx)
  …)

(create-table :child (column :id :type :integer) (make-instance 'pg-inherits :parent :parent))
(alter-table :t my-vendor-action)   ; emit-alter-table-action
(create-type :t :kind :base :base-options '(:input … :output …))  ; emit-create-type-kind
```

## Extension registry (types, operators, functions)

Custom SQL types own **Lisp ↔ expression** conversion — not only DDL names:

```lisp
(register-sql-type :money dialect
  :sql "DECIMAL(19,4)"
  :encode #'money-to-wire
  :decode #'money-from-wire
  :to-expr (lambda (d v) …)
  :emit-value (lambda (d v s ctx) …))

(typed value :money)
(bindparam :x v :type :money)
(bindparam :lim :default 10)
(register-sql-op :->> :binary dialect :sql "->>")
(register-sql-func :jsonb-set dialect :sql "jsonb_set")
```

**Literals vs params:** `lit` / bare values always emit SQL literal text. Placeholders only from `bindparam` or `sql-fragment`.

Vendor seeds live in the dialect backend repos (jsonb, JSON1, arrays, …). Core also has `array-lit`.

## Procedural SQL (two layers)

**Layer 1** — SQL-shaped nodes (`proc-if`, `proc-setf`, …) · **Layer 2** — lispy `body` macro. Postgres plpgsql overrides live in `sql-query-postgres`. Sqlite: unsupported.

## Types / domains (DDL)

```lisp
;; ANSI distinct + structured UDTs
(create-type :euros :as '(:numeric 10 2))
(create-type :addr :attributes '((:street :text) (:city :text)))
(alter-type :addr (add-attribute :zip :text) (drop-attribute :street))
(drop-type :euros :if-exists t :cascade t)

;; ANSI domains
(create-domain :posint :as :integer :default 1 :not-null t
               :check (:> (col :value) 0))
(drop-domain :posint :if-exists t)

;; Postgres ENUM (sql-query-postgres)
(create-type :mood :enum '("sad" "ok" "happy"))
(alter-type :mood (add-enum-value "meh" :after "ok"))
```

`register-sql-type` remains the Lisp↔value adapter (encode/decode); CREATE TYPE is schema DDL. SQLite rejects type/domain DDL (`sql-dialect-unsupported`).

Typed tables: `(create-table :people :of :person-t)`. Vendor trailing clauses go in `create-table-extras` / open `sql-node` args.

Foundation DDL also includes: cast, alter domain/column, grant/revoke, function/trigger, comment-on, TEMP + `ON COMMIT`, view `CHECK OPTION`, assertions, `CREATE TABLE LIKE`, generated columns, CTAS, collation/character-set stubs, txn stmts (`start`/`set-transaction`/commit/rollback/savepoint), `LOCK TABLE`, `FILTER`/`WITHIN GROUP`, named `WINDOW`, `TABLESAMPLE`, FOR SHARE family.

**Vendor backends** own partitions / matviews / COPY / `ON CONFLICT` (postgres) and `INSERT OR` / `ON CONFLICT` (sqlite3) — not core “still missing”.

Out of scope: embedded/module SQL, FDW, RLS, `VACUUM`/`EXPLAIN`, rich SQL/PSM exception handlers.

## Core surface (0.2.0)

`select` / `insert-into` / `update` / `delete-from` / `merge` / `truncate` · joins · `distinct` · CTE · set ops · window / FILTER / WITHIN GROUP · `sql-case` / `sql-cast` / `sql-func` · `bindparam` · `sql-like` (`:escape`) · NULLS FIRST/LAST · `for-update` / `for-share` · DDL (types/domains/cast/grant/trigger/function/assertion/CTAS/LIKE/TEMP/…) · txn stmts · open dialect AST extensions · `sql-fragment` · `make-sql-table` / `create-table-from`.

SxQL is **not** the public API.

## Tests

First-party Rove suites (not vendored NIST/sqltest/slt):

- `tests/ansi-compliance.lisp` — ANSI emit coverage
- `tests/ansi-gap-rest-test.lisp` — assertions, LIKE table, generated cols, FILTER/WITHIN GROUP, WINDOW, TABLESAMPLE, LOCK/SET TRANSACTION, collation, lock strengths
- `tests/expr-txn-gap-test.lisp` — LIKE ESCAPE, NULLS, DEFERRABLE, CTAS, txn stmts
- `tests/sql-query-test.lisp` — Core DSL smoke
- `tests/extension-registry-test.lisp` — type/op registry
- `tests/dialect-extension-test.lisp` — open AST hooks (`sql-extension`, emit generics)
- `tests/ddl-foundation-test.lisp` — GRANT/CAST/ALTER DOMAIN|COLUMN/FUNCTION/TRIGGER/COMMENT/TEMP/CHECK OPTION
- `tests/procedure-test.lisp` — ANSI SQL/PSM procedural

Dialect backend tests live in `sql-query-sqlite3` / `sql-query-postgres`.

```bash
ros -e '(asdf:test-system "sql-query")' -q
```

## License

MIT — see [LICENSE](LICENSE).
