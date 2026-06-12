# Vendored grammars

`sema.tmLanguage.json` is a **copy** of the canonical TextMate grammar at:

    editors/vscode/sema/syntaxes/sema.tmLanguage.json

It is vendored here so `@sema/ui` can bundle it for in-browser Shiki highlighting
without reaching across package boundaries. This mirrors the existing copy
convention used by `website/` and `pkg/` (see the root `CLAUDE.md`).

**Keep it in sync.** When the canonical grammar changes, re-copy it:

    cp ../../../editors/vscode/sema/syntaxes/sema.tmLanguage.json src/grammars/sema.tmLanguage.json

The Shiki integration (`src/lib/syntax-highlight.ts`) registers it under the
language id `sema` and maps its scope names to `.tok-*` classes.
