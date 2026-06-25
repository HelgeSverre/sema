---
name: "db/query-one"
module: "sqlite"
section: "Querying"
---

Run a SELECT and return only the **first** row as a map, or `nil` when no row matches. The convenient form for primary-key / unique lookups. For multiple rows use `db/query`.

```sema
(db/query-one "mydb" "SELECT * FROM users WHERE name = ?" "Alice")
; => {:age 31 :id 1 :name "Alice"}

(db/query-one "mydb" "SELECT * FROM users WHERE name = ?" "Nobody")
; => nil
```
