# @sema/ui — Component Roadmap

Trimmed from a 58-entry inventory sweep down to components that are genuinely
reusable UI primitives. App-specific widgets (notebook cell internals, registry
admin chrome) were dropped — they belong in their app, not the library.

## Guiding Principles

1. **Reusable primitive, not app artifact.** Ship a component only if it's a generic
   building block reused across surfaces (or clearly will be). A notebook "save-state
   flash" or a registry "report card" is app UI — compose it in the app from primitives.
2. **Don't extract Sema-specific marketing layouts** (hero, footer, crate diagram) —
   extract the primitive beneath them (grid, container, card) and compose in place.
3. **Consume design tokens** (`var(--token, #fallback)`); zero hardcoded hex in components.

---

## ✅ Done

| Component | Element(s) | Notes |
|-----------|-----------|-------|
| Button | `sema-button` | variants incl. icon/run/debug/action |
| Tooltip | `sema-tooltip` | CSS anchor positioning |
| Toggle / Segmented | `sema-toggle`, `sema-toggle-group` | |
| Splitter | `sema-splitter` | (#13) |
| Dialog / Modal | `sema-dialog` | **covers Confirm Dialog #15** (no separate confirm) |
| Page | `sema-page` | token/theme host |
| Tree / Sidebar Nav | `sema-tree`, `sema-tree-item` | (#7) |
| Code Block / Snippet | `sema-code` | (#5) dedent + Shiki highlight (sema + json/shell/rust/… lazy + `registerLanguage`) + pluggable format + copy/lines |
| Terminal / Shell Transcript | `sema-terminal` | (#45) `$` prompt / command / `#` comment / output, `prefix` mode |
| Syntax theme | `--syntax-*` + `.tok-*` | (#31) `tokens.css` + `syntax.css` |
| Tokens | `--space-*`, `--radius-*`, `--focus-ring-*` | (L4) |
| CopyController | `internal/controllers/copy.ts` | shared copy + "Copied" feedback |
| Popover / Menu | `sema-popover`, `sema-menu`, `sema-menu-item` | (#37) anchored, outside-click + Esc, roving menu |
| Form controls | `sema-input`, `sema-textarea`, `sema-select`, `sema-field` | (#11) form-associated; **absorbs Search Bar #10** |
| ScrollView | `sema-scroll-area` | themed thin scrollbar; shared `scrollbar.css` (**consolidates #42**) |
| Toast | `sema-toast`, `sema-toaster`, `toast()` | (#17) imperative API + singleton region; variants, stacking, auto-dismiss + pause-on-hover, `dismiss`/`dismissAll` |
| Layout primitives | `sema-container`, `sema-grid`, `sema-sidebar` | (L1–L3) CSS-first, intrinsic responsiveness (no breakpoint props); **`sema-split` renamed → `sema-sidebar`** to avoid confusion with the interactive `sema-splitter` |
| Tabs | `sema-tabs`, `sema-tab`, `sema-tab-panel` | (#4) panel tabs only — segmented control already shipped as `sema-toggle-group`. Light-DOM ARIA (IDREFs resolve), auto/`manual` activation, `hidden="until-found"` find-in-page, opt-in `hash-sync` deep links |

---

## 🔜 Next — priority (ordered)

1. Badge / Tag — `sema-badge` (#9; **absorbs Provider Pill List #23** as a pill variant in a wrap)
2. Drawer / Slide Panel — `sema-drawer` (#16) — **generic & dockable: left / right / top / bottom**, not scoped to the registry admin use-case
3. Spinner / Loading — `sema-spinner` (#2) — **minimal**: inline indicator (size + `role=status`/`aria-label`). Drop the fullscreen "boot splash" mode — that's app composition, not a primitive.
4. Data Table — `sema-table` (#18). Larger build; requirements: **sortable headers** (sort-direction indicator + `aria-sort`), **hoverable rows**, **row/cell click events**, **mobile-responsive horizontal overflow** (`overflow-x:auto` + `overscroll-behavior-x: contain` + momentum scroll).
5. Pagination — `sema-pagination` (#19)
6. Shortcut / KBD — `sema-kbd` (#27)

---

## ⏸ Deferred (sensible, not now)

- **Code Editor composite** — `editor/` module (#38–40): highlighted textarea overlay + **Line Number Gutter (#22)** + debug decorations + read-only viewer. Reuses `sema-code`. (Gutter ships inside this, not standalone.)
- **Card** — `sema-card` (#24 Feature Card / #46 Crate card → one card + the L2 grid). Skipped for now.
- **Cell Divider** — `sema-cell-divider` (#25). Notebook insert UI; compose in the app from `sema-popover` + a divider rather than shipping a component.
- **Empty State (#8)** — likely app-composed (a centered `L1 container` + icon/message/action slots). Reassess whether a thin `<sema-empty-state>` wrapper earns its keep once Layout primitives land.
- **Toast action button** — "Undo"-style action slot + `part` on `sema-toast`. Deferred from Toast v1; add when a real consumer needs it (exit animation polish deferred with it).
- Markdown Renderer (#14, user: defer) · Status Bar (#1) · Toolbar/Header (#3) · Output Console (#12) · Collapsible/Disclosure (#26) · Icon Button / Button Group (#29) · Key-Value / Detail Row → `sema-field` sibling (#30) · Stat Card (#20) · Drop Overlay (#21) · Inline Alert/Banner (#51).

## ⏸ Deferred fixes (from the 2026-06 quality sweep)

- **Packaging/publish portability** — dist + `.d.ts`, exports map, externalized-Lit build, define guards. Latent behind `private: true`; do as one batch when publishing is planned.
- **Event-name consistency** — dialog `sema-dialog-open/close` vs popover `sema-open/close`. Breaking; defer to a deliberate release.
- **CI Node job** — blocked: `brand/ui/` is untracked in git; commit `brand/` first, then add npm ci + playwright + lint + test to ci.yml.
- **Popover positioning** — top-layer/portal for overflow-clipping containers; JS measure-and-flip. Interim: use `native` selects inside scroll containers.
- **Test `waitFor` consolidation** — five duplicated helpers → `tests/_util.ts`; replace `setTimeout(150)` negative assertions in sema-code tests with a `highlightComplete` signal.
- Menu type-ahead (APG SHOULD) · sema-toggle `selected`-is-group-managed JSDoc note.
- Small website primitives: `sema-cta-group` (#43) · `sema-diagram-frame` (#46) · `sema-status-cell` (#48) · `sema-arrow-link` (#49, → Button variant) · `sema-text-divider` (#56).

---

## ❌ Rejected — app concern, not the library

These are application UI; build them in the app from the primitives above.

- **Search Bar (#10)** → use Form controls (`sema-input` + button).
- **Notebook internals:** Cell Container (#32), Execution-Count/Prompt (#33), Output Meta Strip (#34), Inline Edit (#35), Save-State Flash (#36).
- **Registry internals:** Sparkline (#50), TimeAgo (#52), API Token Row (#53), Settings Nav (#54), Settings Section (#55), OAuth button (#56 — use Button + slot), Report Card (#57), Version List (#58).
- **Marketing sections (compose-in-place, no component):** Hero (#43), Crate section (#46), Footer (#47), Feature Matrix table (#48), Reference CTA (#49).

---

## Consolidations applied

- ScrollView ← Themed Scrollbar #42 + all ad-hoc scroll containers
- Search Bar #10 → Form controls
- Confirm Dialog #15 → `sema-dialog` (done)
- Boot Splash #41 → dropped (was briefly folded into Spinner #2 as a fullscreen mode; that mode was cut — app composition, not a primitive)
- Feature Card #24 / Crate card #46 → `sema-card` + L2 grid
- Provider Pill List #23 → `sema-badge` (pill variant)

## Conventions

Extend `SemaElement`; self-register + `HTMLElementTagNameMap`; export from `src/lib/index.ts` → `src/index.ts`; colors via `var(--token, #fallback)` (enforced by `tests/tokens.test.ts`); expose `part`s; paired `stories/*.stories.ts` + `tests/*.test.ts`; non-component code under `src/internal/` (controllers, helpers, base class — not public). Reuse `FocusTrapController` and the `sema-tooltip` anchor-positioning pattern.
