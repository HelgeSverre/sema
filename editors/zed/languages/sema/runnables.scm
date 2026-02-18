; Run a Sema source file.
; Bind with a task tagged "sema-run" in your tasks.json:
;
;   {
;     "label": "Run: $ZED_RELATIVE_FILE",
;     "command": "sema",
;     "args": ["$ZED_FILE"],
;     "tags": ["sema-run"]
;   }

(source_file) @run (#set! tag "sema-run")
