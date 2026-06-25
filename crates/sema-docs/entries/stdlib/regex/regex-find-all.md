---
name: "regex/find-all"
module: "regex"
section: "Matching"
---

Find all non-overlapping matches of a pattern. Returns a list of the **full matched substrings** (not capture groups), or `()` when nothing matches.

```sema
(regex/find-all #"\d+" "a1b2c3")          ; => ("1" "2" "3")
(regex/find-all #"[A-Z]" "Hello World")   ; => ("H" "W")
(regex/find-all #"\d+" "abc")             ; => ()  (no matches)
```

Each result is the whole match even when the pattern has groups — `(\d)(\d)` over `"12 34"` yields `("12" "34")`, not the individual digits. For capture-group detail, use [`regex/match`](/docs/stdlib/regex/regex-match) per match.
