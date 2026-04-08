/**
 * Reactive state system for Sema Web — powered by @preact/signals-core.
 *
 * Sema API:
 *   (def count (state 0))     ;; create reactive state
 *   @count                     ;; read (auto-tracked in components/computed)
 *   (put! count 42)            ;; set value
 *   (update! count inc)        ;; apply function
 *   (def doubled (computed (* @count 2)))  ;; derived state
 *   (batch (put! a 1) (put! b 2))         ;; coalesce updates
 *   (watch count (fn [old new] ...))       ;; side effects
 *
 * @module
 */

import { signal, computed, effect, batch } from "@preact/signals-core";
import type { Signal, ReadonlySignal } from "@preact/signals-core";
import { SEMA_IDENT_RE } from "./handles.js";
import type { SemaWebContext } from "./context.js";
import { toInvokableCallback, releaseCallback } from "./callbacks.js";

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  invokeGlobal(name: string, ...args: any[]): any;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/**
 * Register all reactive state functions.
 *
 * JS-level functions (__state/* namespace — internal):
 * - `__state/create` — create a new signal, returns numeric ID
 * - `__state/deref` — read signal value (auto-tracked by signals-core)
 * - `__state/put!` — set signal value
 * - `__state/computed-create` — create a computed signal from a named thunk
 * - `__state/next-computed-id` — return a unique name for computed thunks
 * - `__state/batch-run` — run a named thunk inside batch()
 * - `__state/next-batch-id` — return a unique name for batch thunks
 * - `__state/watch` — watch a signal, call Sema fn on change
 *
 * Sema-level wrappers (convenience):
 * - `(state val)` — create reactive state
 * - `(deref ref)` — read state value
 * - `(put! ref val)` — set state value
 * - `(update! ref f . args)` — update state by applying function
 * - `(computed expr)` — macro: derived reactive state
 * - `(batch . body)` — macro: coalesce multiple updates
 * - `(watch ref fn)` — watch state for changes
 */
export function registerReactiveBindings(interp: SemaInterpreterLike, ctx: SemaWebContext): void {
  const getActiveComponent = () => {
    const componentId = ctx.renderContextStack[ctx.renderContextStack.length - 1];
    return componentId != null ? ctx.mountedComponentsById.get(componentId) ?? null : null;
  };

  // __state/create — create a new signal, returns numeric ID
  interp.registerFunction("__state/create", (initialValue: any) => {
    const id = ctx.nextSignalId++;
    ctx.signals.set(id, signal(initialValue));
    return id;
  });

  // __state/deref — read signal value (auto-tracked by signals-core if inside effect/computed)
  interp.registerFunction("__state/deref", (signalId: number) => {
    const s = ctx.signals.get(signalId);
    if (!s) throw new Error(`Unknown state: ${signalId}`);
    return s.value; // Must read .value directly for dependency tracking
  });

  // __state/put! — set signal value
  interp.registerFunction("__state/put!", (signalId: number, newValue: any) => {
    const s = ctx.signals.get(signalId);
    if (!s) throw new Error(`Unknown state: ${signalId}`);
    s.value = newValue;
    return newValue;
  });

  // __state/computed-create — create a computed signal from a named Sema thunk.
  // The thunkName is a zero-arg Sema function whose return value is captured
  // via a registered JS capture function.
  interp.registerFunction("__state/computed-create", (thunkName: string) => {
    if (!SEMA_IDENT_RE.test(thunkName)) {
      throw new Error(`Invalid computed thunk name: ${thunkName}`);
    }
    const id = ctx.nextSignalId++;

    const c = computed(() => {
      try {
        return interp.invokeGlobal(thunkName);
      } catch (e) {
        ctx.onerror(e instanceof Error ? e : new Error(String(e)), `computed:${thunkName}`);
        return undefined;
      }
    });
    ctx.signals.set(id, c as unknown as Signal<any>);
    return id;
  });

  // Counter for generating unique computed thunk names
  let computedCounter = 0;
  interp.registerFunction("__state/next-computed-id", () => {
    return `__cthunk_${computedCounter++}`;
  });

  // __state/batch-run — run a named thunk inside batch()
  interp.registerFunction("__state/batch-run", (thunkName: string) => {
    if (!SEMA_IDENT_RE.test(thunkName)) {
      throw new Error(`Invalid batch thunk name: ${thunkName}`);
    }
    let captured: any = undefined;
    batch(() => {
      try {
        captured = interp.invokeGlobal(thunkName);
      } catch (e) {
        ctx.onerror(e instanceof Error ? e : new Error(String(e)), "batch");
      }
    });
    return captured;
  });

  // Counter for generating unique batch thunk names
  let batchCounter = 0;
  interp.registerFunction("__state/next-batch-id", () => {
    return `__bthunk_${batchCounter++}`;
  });

  // __state/watch — watch a signal for changes, call Sema fn with old + new values.
  // Returns a numeric watch handle that can be disposed with __state/unwatch.
  interp.registerFunction("__state/watch", (signalId: number, callbackValue: any) => {
    const s = ctx.signals.get(signalId);
    if (!s) throw new Error(`Unknown state: ${signalId}`);
    const callback = toInvokableCallback(callbackValue, interp, "watch callback");

    let prev = s.value;

    const watchId = ctx.nextWatchId++;
    const dispose = effect(() => {
      const current = s.value; // track dependency
      if (prev !== current) {
        const oldVal = prev;
        prev = current;
        try {
          callback(oldVal, current);
        } catch (e) {
          ctx.onerror(e instanceof Error ? e : new Error(String(e)), "watch");
        }
      }
    });
    ctx.watchDisposers.set(watchId, { dispose, callback });
    const owner = getActiveComponent();
    if (owner) owner.ownedWatchIds.add(watchId);
    return watchId;
  });

  interp.registerFunction("__state/unwatch", (watchId: number) => {
    const registration = ctx.watchDisposers.get(watchId);
    if (registration) {
      registration.dispose();
      ctx.watchDisposers.delete(watchId);
      releaseCallback(registration.callback);
      for (const component of ctx.mountedComponents.values()) {
        component.ownedWatchIds.delete(watchId);
      }
    }
    return null;
  });

  // --- Sema-side convenience wrappers ---
  // These define user-facing functions and macros that call the __state/* internals.
  const semaResult = interp.evalStr(`
    (define (state val) (__state/create val))
    (define (deref ref) (__state/deref ref))
    (define (put! ref val) (__state/put! ref val))
    (define (update! ref f . args) (put! ref (apply f (cons (deref ref) args))))

    (defmacro computed (expr)
      (let ((name (string->symbol (__state/next-computed-id))))
        \`(begin
           (define (,name) ,expr)
           (__state/computed-create ,(symbol->string name)))))

    (defmacro batch (. body)
      (let ((name (string->symbol (__state/next-batch-id))))
        \`(begin
           (define (,name) ,@body)
           (__state/batch-run ,(symbol->string name)))))

    (define (watch ref fn) (__state/watch ref fn))
    (define (unwatch! watch-id) (__state/unwatch watch-id))
  `);

  if (semaResult.error) {
    throw new Error(`[sema-web] Failed to register reactive wrappers: ${semaResult.error}`);
  }
}
