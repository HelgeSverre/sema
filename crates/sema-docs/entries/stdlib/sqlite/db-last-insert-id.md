---
name: "db/last-insert-id"
module: "sqlite"
section: "Utility"
---

Return the rowid (integer primary key) of the most recent successful INSERT on this connection. Call it immediately after the insert to capture the generated id.

```sema
(db/exec "mydb" "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
(db/exec "mydb" "INSERT INTO users (name) VALUES (?)" "Alice")
(db/last-insert-id "mydb")   ; => 1
```
