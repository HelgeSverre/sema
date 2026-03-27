/**
 * @sema-lang/sema-web — Sema as a web scripting language
 *
 * Embed Sema in web pages with DOM bindings, persistent storage,
 * and `<script type="text/sema">` support.
 *
 * ## Quick Start
 *
 * Add to your HTML page:
 *
 * ```html
 * <script type="module">
 *   import { SemaWeb } from "@sema-lang/sema-web";
 *   await SemaWeb.init();
 * </script>
 *
 * <script type="text/sema">
 *   (let ((el (dom/create-element "h1")))
 *     (dom/set-text! el "Hello from Sema!")
 *     (dom/append-child! (dom/query "body") el))
 * </script>
 * ```
 *
 * ## Manual Usage
 *
 * ```js
 * import { SemaWeb } from "@sema-lang/sema-web";
 *
 * const web = await SemaWeb.create();
 * web.eval('(dom/set-text! (dom/query "#app") "Hello!")');
 * ```
 *
 * @module
 */

import { SemaInterpreter } from "@sema-lang/sema";
import type { InterpreterOptions } from "@sema-lang/sema";
import { registerDomBindings } from "./dom.js";
import { registerStoreBindings } from "./store.js";
import { loadScripts } from "./loader.js";
import type { LoaderOptions } from "./loader.js";

/** Options for SemaWeb initialization. */
export interface SemaWebOptions extends InterpreterOptions {
  /**
   * Whether to auto-discover and evaluate `<script type="text/sema">` tags.
   * Default: `true`.
   */
  autoLoad?: boolean;

  /**
   * Whether to register `dom/*` namespace functions.
   * Default: `true`.
   */
  dom?: boolean;

  /**
   * Whether to register `store/*` namespace functions.
   * Default: `true`.
   */
  store?: boolean;

  /**
   * Options for the script loader.
   */
  loader?: LoaderOptions;

  /**
   * Whether to register `console/*` namespace functions.
   * Default: `true`.
   */
  console?: boolean;
}

/** Result of evaluating Sema code. */
export interface EvalResult {
  value: string | null;
  output: string[];
  error: string | null;
}

/**
 * Sema web runtime — wraps SemaInterpreter with browser-specific bindings.
 *
 * Provides:
 * - `dom/*` functions for DOM manipulation
 * - `store/*` functions for localStorage/sessionStorage
 * - `console/*` functions for browser console access
 * - Auto-loading of `<script type="text/sema">` tags
 *
 * @example
 * ```js
 * // Auto-init: discovers and runs all <script type="text/sema"> tags
 * await SemaWeb.init();
 *
 * // Manual: create instance and evaluate code
 * const web = await SemaWeb.create({ autoLoad: false });
 * web.eval('(dom/set-text! (dom/query "#greeting") "Hello!")');
 * ```
 */
export class SemaWeb {
  private _interp: SemaInterpreter;

  private constructor(interp: SemaInterpreter) {
    this._interp = interp;
  }

  /**
   * Create a SemaWeb instance with browser bindings registered.
   *
   * @param opts - Configuration options
   * @returns A ready-to-use SemaWeb instance
   */
  static async create(opts?: SemaWebOptions): Promise<SemaWeb> {
    const interp = await SemaInterpreter.create(opts);
    const web = new SemaWeb(interp);

    // Register browser bindings
    if (opts?.dom !== false) {
      registerDomBindings(interp);
    }

    if (opts?.store !== false) {
      registerStoreBindings(interp);
    }

    if (opts?.console !== false) {
      registerConsoleBindings(interp);
    }

    // Auto-discover and evaluate <script type="text/sema"> tags
    if (opts?.autoLoad !== false) {
      await loadScripts(interp, opts?.loader);
    }

    return web;
  }

  /**
   * Convenience: create a SemaWeb instance with default options and auto-load scripts.
   *
   * Equivalent to `SemaWeb.create()` — discovers and evaluates all
   * `<script type="text/sema">` tags in the document.
   *
   * @param opts - Configuration options
   * @returns A ready-to-use SemaWeb instance
   */
  static async init(opts?: SemaWebOptions): Promise<SemaWeb> {
    return SemaWeb.create(opts);
  }

  /**
   * Evaluate a string of Sema code with browser bindings available.
   *
   * @param code - Sema source code
   * @returns The evaluation result
   */
  eval(code: string): EvalResult {
    return this._interp.evalStr(code);
  }

  /**
   * Evaluate Sema code with async HTTP support.
   *
   * @param code - Sema source code
   * @returns The evaluation result
   */
  async evalAsync(code: string): Promise<EvalResult> {
    return this._interp.evalStrAsync(code);
  }

  /**
   * Register a JavaScript function callable from Sema code.
   *
   * @param name - Function name in Sema
   * @param fn - JavaScript function
   */
  registerFunction(name: string, fn: (...args: any[]) => any): void {
    this._interp.registerFunction(name, fn);
  }

  /**
   * Preload a Sema module so that `(import "name")` works.
   *
   * @param name - Module name
   * @param source - Sema source code
   */
  preloadModule(name: string, source: string): void {
    this._interp.preloadModule(name, source);
  }

  /**
   * Get the underlying SemaInterpreter instance.
   *
   * Useful for advanced operations like VFS access.
   */
  get interpreter(): SemaInterpreter {
    return this._interp;
  }

  /**
   * Get the Sema interpreter version.
   */
  version(): string {
    return this._interp.version();
  }

  /**
   * Free the interpreter's WASM memory.
   * The instance cannot be used after calling this method.
   */
  dispose(): void {
    this._interp.dispose();
  }
}

/**
 * Register `console/*` namespace functions.
 */
function registerConsoleBindings(interp: SemaInterpreter): void {
  interp.registerFunction("console/log", (...args: any[]) => {
    console.log(...args);
    return null;
  });

  interp.registerFunction("console/warn", (...args: any[]) => {
    console.warn(...args);
    return null;
  });

  interp.registerFunction("console/error", (...args: any[]) => {
    console.error(...args);
    return null;
  });

  interp.registerFunction("console/info", (...args: any[]) => {
    console.info(...args);
    return null;
  });

  interp.registerFunction("console/debug", (...args: any[]) => {
    console.debug(...args);
    return null;
  });

  interp.registerFunction("console/clear", () => {
    console.clear();
    return null;
  });

  interp.registerFunction("console/time", (label: string) => {
    console.time(label);
    return null;
  });

  interp.registerFunction("console/time-end", (label: string) => {
    console.timeEnd(label);
    return null;
  });
}

// Re-export types
export type { LoaderOptions } from "./loader.js";
export { registerDomBindings } from "./dom.js";
export { registerStoreBindings } from "./store.js";
export { loadScripts } from "./loader.js";
