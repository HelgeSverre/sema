# internal/

Implementation details of `@sema/ui` — **not** part of the public API. Anything
here (the `SemaElement` base class, reactive controllers, the dedent/highlight
helpers) may change without a major version bump. Components import from here via
relative paths; consumers should depend only on the exported element classes and
documented extension seams (`SemaCode.formatter`, `registerLanguage`).

Convention follows Shoelace (`src/internal/`) and Material Web (`internal/`).
