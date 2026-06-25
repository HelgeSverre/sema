---
name: "async/race"
module: "concurrency"
section: "Promises"
---

```sema
(async/race promises) → value
```

Return the value of the **first promise to resolve**, ignoring the rest. Takes a list or vector of promises. The losing tasks are *not* cancelled — they keep running in the background; pair with `async/cancel` if you need to stop them. Contrast with `async/all`, which waits for *every* promise and returns all results.

```sema
;; First reply wins; the slow one is abandoned.
(async/race (list (async (async/sleep 100) :slow)
                  (async (async/sleep 10)  :fast)))  ; => :fast
```
