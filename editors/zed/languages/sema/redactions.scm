; Redact string values passed to sensitive LLM/API configuration calls.
; Hides API keys and secrets during screen sharing / pair programming.

; (llm/configure :api-key "SECRET")
(list
  .
  (symbol) @_f
  .
  (_)*
  .
  (string) @redact
  (#any-of? @_f "llm/configure" "llm/define-provider" "llm/auto-configure")
)
