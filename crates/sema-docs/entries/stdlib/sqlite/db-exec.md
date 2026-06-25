---
name: "db/exec"
module: "sqlite"
section: "Executing SQL"
---

Run a SQL statement that changes the database (INSERT, UPDATE, DELETE, CREATE TABLE, …) and return the number of rows affected as an integer (0 for DDL like CREATE).

Always pass user data as `?` **placeholders** with the values as trailing arguments rather than building the SQL string yourself — the driver binds them safely, which prevents SQL injection and handles quoting/escaping for you. Use `db/query` / `db/query-one` for SELECTs, and `db/exec-batch` for multi-statement schema scripts.

```sema
(db/exec "mydb" "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)")
; => 0  (DDL affects no rows)

(db/exec "mydb" "INSERT INTO users (name, age) VALUES (?, ?)" "Alice" 30)
; => 1

(db/exec "mydb" "UPDATE users SET age = ? WHERE name = ?" 31 "Alice")
; => 1  (one row matched)
```
