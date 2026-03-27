# @sema-lang/sema-web

Sema as an embedded web scripting language — use Sema to build interactive web UIs with DOM bindings, persistent storage, and `<script type="text/sema">` support.

> Built on top of [`@sema-lang/sema`](https://www.npmjs.com/package/@sema-lang/sema), the WebAssembly-powered Sema interpreter.

## Quick Start

Add Sema to any HTML page:

```html
<!DOCTYPE html>
<html>
<body>
  <div id="app"></div>

  <script type="text/sema">
    ;; Create a greeting element
    (let ((el (dom/create-element "h1")))
      (dom/set-text! el "Hello from Sema!")
      (dom/set-style! el "color" "#6366f1")
      (dom/append-child! (dom/query "#app") el))
  </script>

  <script type="module">
    import { SemaWeb } from "@sema-lang/sema-web";
    await SemaWeb.init();
  </script>
</body>
</html>
```

## Installation

```bash
npm install @sema-lang/sema-web
```

Or use from a CDN:

```html
<script type="module">
  import { SemaWeb } from "https://cdn.jsdelivr.net/npm/@sema-lang/sema-web/dist/index.js";
  await SemaWeb.init();
</script>
```

## Usage

### Auto-load `<script type="text/sema">` tags

The simplest way — `SemaWeb.init()` discovers and evaluates all Sema script tags:

```html
<script type="text/sema">
  (println "Hello from Sema!")
</script>

<script type="text/sema" src="app.sema"></script>

<script type="module">
  import { SemaWeb } from "@sema-lang/sema-web";
  await SemaWeb.init();
</script>
```

### Manual evaluation

Create an instance and evaluate code programmatically:

```js
import { SemaWeb } from "@sema-lang/sema-web";

const web = await SemaWeb.create({ autoLoad: false });

// Evaluate Sema code with DOM access
web.eval('(dom/set-text! (dom/query "#greeting") "Hello!")');

// Register custom JS functions callable from Sema
web.registerFunction("get-timestamp", () => Date.now());
web.eval("(console/log (get-timestamp))");
```

### External `.sema` files

Reference external Sema files with the `src` attribute:

```html
<script type="text/sema" src="counter.sema"></script>
```

## API Namespaces

### `dom/*` — DOM Manipulation

```sema
;; Query elements
(dom/query "#app")              ;; → element handle or nil
(dom/query-all ".item")         ;; → list of element handles
(dom/get-id "my-element")       ;; → element handle or nil

;; Create elements
(dom/create-element "div")      ;; → element handle
(dom/create-text "Hello")       ;; → text node handle

;; Tree manipulation
(dom/append-child! parent child)
(dom/remove-child! parent child)
(dom/remove! element)

;; Attributes
(dom/set-attribute! el "class" "container")
(dom/get-attribute el "href")
(dom/remove-attribute! el "disabled")

;; CSS classes
(dom/add-class! el "active" "visible")
(dom/remove-class! el "hidden")
(dom/toggle-class! el "open")
(dom/has-class? el "active")    ;; → #t or #f

;; Styles
(dom/set-style! el "color" "red")
(dom/get-style el "color")

;; Content
(dom/set-text! el "Hello")
(dom/get-text el)
(dom/set-html! el "<b>Bold</b>")

;; Form values
(dom/set-value! input "text")
(dom/get-value input)

;; Events
(dom/on! el "click" handler-name)
(dom/off! el "click" handler-name)
(dom/prevent-default! event)
```

### `store/*` — Persistent Storage

```sema
;; localStorage
(store/set! "key" "value")
(store/get "key")               ;; → value or nil
(store/remove! "key")
(store/clear!)
(store/keys)                    ;; → list of keys
(store/has? "key")              ;; → #t or #f

;; sessionStorage
(store/session-set! "key" "value")
(store/session-get "key")
(store/session-remove! "key")
(store/session-clear!)
```

### `console/*` — Browser Console

```sema
(console/log "message" value)
(console/warn "warning!")
(console/error "error!")
(console/info "info")
(console/debug "debug")
(console/clear)
(console/time "label")
(console/time-end "label")
```

## Configuration

```js
const web = await SemaWeb.create({
  // Auto-discover <script type="text/sema"> tags (default: true)
  autoLoad: true,

  // Register dom/* functions (default: true)
  dom: true,

  // Register store/* functions (default: true)
  store: true,

  // Register console/* functions (default: true)
  console: true,

  // Custom WASM URL (for CDN deployment)
  wasmUrl: "https://cdn.example.com/sema_wasm_bg.wasm",

  // Sandbox capabilities to deny
  deny: ["network"],
});
```

## Example: Interactive Counter

```sema
;; counter.sema — A simple click counter

;; State
(define count 0)

;; Create UI elements
(let ((container (dom/query "#app"))
      (display (dom/create-element "h1"))
      (btn-inc (dom/create-element "button"))
      (btn-dec (dom/create-element "button")))

  ;; Set initial content
  (dom/set-text! display "0")
  (dom/set-text! btn-inc "+")
  (dom/set-text! btn-dec "−")

  ;; Style
  (dom/set-style! display "font-size" "4rem")
  (dom/set-style! display "text-align" "center")

  ;; Append to container
  (dom/append-child! container display)
  (dom/append-child! container btn-inc)
  (dom/append-child! container btn-dec)

  ;; Store element handles for event handlers
  (define display-el display)
  (define inc-btn btn-inc)
  (define dec-btn btn-dec))

;; Event handlers
(define (on-increment evt)
  (set! count (+ count 1))
  (dom/set-text! display-el (number->string count)))

(define (on-decrement evt)
  (set! count (- count 1))
  (dom/set-text! display-el (number->string count)))

;; Bind events
(dom/on! inc-btn "click" "on-increment")
(dom/on! dec-btn "click" "on-decrement")
```

## Architecture

```
┌─────────────────────────────────────────┐
│  HTML Page                              │
│                                         │
│  <script type="text/sema">              │
│    (dom/set-text! ...)                  │
│  </script>                              │
│                                         │
├─────────────────────────────────────────┤
│  @sema-lang/sema-web                    │
│  ┌──────────┬──────────┬──────────────┐ │
│  │ dom/*    │ store/*  │ console/*    │ │
│  │ bindings │ bindings │ bindings     │ │
│  └──────────┴──────────┴──────────────┘ │
│  ┌────────────────────────────────────┐ │
│  │ Script loader (<script> discovery) │ │
│  └────────────────────────────────────┘ │
├─────────────────────────────────────────┤
│  @sema-lang/sema (interpreter API)      │
├─────────────────────────────────────────┤
│  @sema-lang/sema-wasm (WASM VM)        │
└─────────────────────────────────────────┘
```

`sema-web` uses the `registerFunction` API from `@sema-lang/sema` to bridge JavaScript browser APIs into the Sema interpreter. No Rust code changes are required — all DOM, storage, and console bindings are implemented as JavaScript callbacks registered into the interpreter at initialization.

## License

MIT
