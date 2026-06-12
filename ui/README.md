# @sema/ui

Lit-based Web Components for the Sema design system. Framework-agnostic, consumes DESIGN.md tokens via CSS custom properties.

## Quick Start

### Standalone (notebook, playground, any HTML page)

```html
<link rel="stylesheet" href="tokens.css">
<script type="module" src="sema-ui.js"></script>

<sema-button variant="primary">Get Started</sema-button>
```

`dist/sema-ui.js` is a **self-contained ES module** — Lit is bundled inline, zero runtime dependencies. Drop it into any page alongside `tokens.css`.

### npm consumer (VitePress, Vite, any bundler)

```ts
import '@sema/ui/tokens.css'
import '@sema/ui'
```

When imported this way, lit stays external — your bundler deduplicates it.

## Commands

```sh
npm run dev           # Vite dev server with component showcase
npm run build         # Build standalone library (Vite → Rolldown)
npm run preview       # Preview built output
npm test              # Run tests and quit (Vitest + Playwright, headless)
npm run test:watch    # Run tests in watch mode (re-run on changes)
npm run test:ui       # Run tests with Vitest UI
npm run lint          # ESLint across all source files
npm run storybook     # Start Storybook dev server (http://localhost:6006)
npm run build-storybook  # Build static Storybook
```

## Components

| Component | Tag | Variants | Notes |
|-----------|-----|----------|-------|
| Button | `<sema-button>` | primary, secondary, ghost, icon, pill, run, debug, action | +danger modifier, +shortcut badge, +disabled |
| Tooltip | `<sema-tooltip>` | placement: top/bottom/left/right | CSS Anchor Positioning with fallback, Escape to dismiss |
| Toggle | `<sema-toggle>` | — | Used inside ToggleGroup, aria-checked synced |
| Toggle Group | `<sema-toggle-group>` | — | Arrow key navigation, `sema-change` event |
| Splitter | `<sema-splitter>` | direction: horizontal/vertical | Mouse + touch drag, keyboard arrows, `sema-resize` delta events, configurable step/shiftStep |
| Dialog | `<sema-dialog>` | — | Modal with focus trap, backdrop click, Escape close, scroll lock, header/body/footer slots |
| Tree | `<sema-tree>` / `<sema-tree-item>` | — | Nested items, keyboard arrows, expand/collapse chevron, auto depth calculation, `sema-tree-select` event |

## Architecture

- **Lit 3** — reactive Web Components with decorators
- **CSS custom properties** — all styling from `docs/design/DESIGN.md` tokens, zero hardcoded hex
- **CSS Anchor Positioning** — tooltip popups with `@supports` fallback (zero JS positioning)
- **Vite 6** — Rolldown-powered build, TypeScript compilation, `declaration: true`
- **Vitest 4** — Playwright browser mode, 49 tests, real Chromium rendering
- **Storybook 10** — visual component catalog with a11y and interaction testing
- **ESLint** — `eslint-plugin-lit` + `eslint-plugin-wc` + `eslint-plugin-storybook`
- **54.8 KB** unminified, **13.4 KB** gzipped (Lit + 8 components bundled inline)

## Integration

### Standalone (notebook, playground, any HTML page)
Copy `dist/sema-ui.js` + `src/styles/tokens.css` into your project, add `<link>` + `<script type="module">`. All 8 custom elements auto-register.

### Notebook (Rust binary)
Copy both files into `crates/sema-notebook/src/ui/`, register in `ui.rs` via `include_str!`, add to `index.html` before `notebook.js`.

### Playground (esbuild)
Copy both files to `playground/`, add `<link>` + `<script type="module">` to `index.html`. Components register globally before `dist/app.js` runs.

### Website (VitePress)
```ts
import '@sema/ui/tokens.css'
import '@sema/ui'
```

Vite deduplicates lit automatically.

## Publishing (npm)

```json
{
  "main": "src/index.ts",
  "module": "src/index.ts",
  "types": "src/index.ts",
  "exports": {
    ".": { "import": "./src/index.ts", "types": "./src/index.ts" },
    "./standalone": { "import": "./dist/sema-ui.js" },
    "./tokens.css": "./src/styles/tokens.css"
  }
}
```

All best practices from [Lit's publishing guide](https://lit.dev/docs/tools/publishing/):
- ES2021 target, ESM modules, `.js` extensions on imports
- `HTMLElementTagNameMap` entries on every component
- Self-defining elements, exported classes
- TypeScript declarations generated (`declaration: true`)
- No bundled lit for npm consumers (use `./standalone` for bundled)
