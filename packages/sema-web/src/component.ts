/**
 * Component system for Sema — reactive mounting of SIP views.
 *
 * Provides `mount!` to bind a Sema component function to a DOM element.
 * The component automatically re-renders when reactive state it depends on changes.
 *
 * Uses morphdom for efficient DOM patching and delegated event handling
 * via `data-sema-on-*` attributes set by the SIP renderer.
 *
 * ## Usage
 *
 * ```sema
 * (def count (state 0))
 *
 * (define (counter-view)
 *   [:div {:class "counter"}
 *     [:p (deref count)]
 *     [:button {:on-click "increment"} "+"]])
 *
 * (define (increment ev) (update! count (lambda (n) (+ n 1))))
 *
 * (mount! "#app" "counter-view")
 * ```
 *
 * @module
 */

import morphdom from "morphdom";
import { signal, effect, batch } from "@preact/signals-core";
import type { SemaWebContext, MountedComponent } from "./context.js";
import { renderSip } from "./sip.js";
import { SEMA_IDENT_RE, storeHandle } from "./handles.js";

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/**
 * Call a Sema component function and capture its structured return value.
 *
 * Uses a unique registered capture function per call to avoid race conditions
 * when multiple components render.
 *
 * Pushes the component function name onto the render context stack before eval
 * and pops it after, so that `local` and `on-mount` can discover which
 * component is currently rendering.
 */
function callComponent(interp: SemaInterpreterLike, fnName: string, ctx: SemaWebContext): any {
  let captured: any = null;
  const captureId = ctx.nextCaptureId++;
  const captureName = `__cc_${captureId}`;
  interp.registerFunction(captureName, (val: any) => { captured = val; return null; });

  // Push render context so local/on-mount know which component is active
  ctx.renderContextStack.push(fnName);
  try {
    const result = interp.evalStr(`(${captureName} (${fnName}))`);
    if (result.error) {
      ctx.onerror(new Error(result.error), `component:${fnName}`);
      return null;
    }
    return captured;
  } finally {
    ctx.renderContextStack.pop();
  }
}

/**
 * Render a mounted component using effect() for automatic dependency tracking
 * and morphdom for efficient DOM patching.
 */
function renderComponent(
  component: MountedComponent,
  selector: string,
  interp: SemaInterpreterLike,
  ctx: SemaWebContext,
): void {
  // Dispose previous effect if any
  if (component.dispose) component.dispose();

  component.dispose = effect(() => {
    const sipData = callComponent(interp, component.componentFn, ctx);
    if (sipData == null) return;

    const clone = component.target.cloneNode(false) as Element;
    const sipNode = renderSip(sipData, interp, ctx);
    clone.appendChild(sipNode);

    morphdom(component.target, clone, {
      childrenOnly: true,
      onBeforeElUpdated(fromEl, toEl) {
        // Preserve focus and cursor position in active input elements
        if (
          fromEl === document.activeElement &&
          (fromEl.tagName === "INPUT" || fromEl.tagName === "TEXTAREA" || fromEl.tagName === "SELECT")
        ) {
          for (const attr of Array.from(toEl.attributes)) {
            if (attr.name !== "value") fromEl.setAttribute(attr.name, attr.value);
          }
          return false;
        }
        return true;
      },
      onNodeDiscarded(_node) {
        // Handle cleanup happens via releaseHandle when Sema code explicitly releases.
        // We don't track inverse mapping from node -> handle, so this is a no-op for now.
      },
    });
  });
}

/**
 * Find a MountedComponent by its function name.
 * Searches all mounted components for one with a matching componentFn.
 */
function findComponentByFn(ctx: SemaWebContext, fnName: string): MountedComponent | null {
  for (const component of ctx.mountedComponents.values()) {
    if (component.componentFn === fnName) return component;
  }
  return null;
}

/**
 * Delegated event handler that intercepts events on the mount target
 * and dispatches to Sema callback functions via `data-sema-on-*` attributes.
 */
class EventDelegator {
  setup(target: Element, interp: SemaInterpreterLike, ctx: SemaWebContext) {
    const bubbling = [
      "click", "dblclick", "contextmenu", "input", "change", "submit",
      "keydown", "keyup", "keypress", "pointerdown", "pointerup", "pointermove",
      "focusin", "focusout",
    ];

    for (const event of bubbling) {
      target.addEventListener(event, (ev) => {
        let el = ev.target as Element | null;
        while (el && target.contains(el)) {
          const attr = `data-sema-on-${event}`;
          if (el.hasAttribute(attr)) {
            const fn = el.getAttribute(attr)!;
            if (SEMA_IDENT_RE.test(fn)) {
              this.dispatchEvent(interp, ctx, fn, ev);
              // Stop walking up if the handler called stopPropagation
              if (ev.cancelBubble || (ev as any).__sema_stop) break;
            }
          }
          el = el.parentElement;
        }
      });
    }

    // mouseenter via mouseover + relatedTarget
    target.addEventListener("mouseover", (ev: Event) => {
      const mev = ev as MouseEvent;
      const el = (mev.target as Element).closest?.("[data-sema-on-mouseenter]");
      if (!el || el.contains(mev.relatedTarget as Node)) return;
      this.dispatchEvent(interp, ctx, el.getAttribute("data-sema-on-mouseenter")!, mev);
    });

    target.addEventListener("mouseout", (ev: Event) => {
      const mev = ev as MouseEvent;
      const el = (mev.target as Element).closest?.("[data-sema-on-mouseleave]");
      if (!el || el.contains(mev.relatedTarget as Node)) return;
      this.dispatchEvent(interp, ctx, el.getAttribute("data-sema-on-mouseleave")!, mev);
    });
  }

  private dispatchEvent(interp: SemaInterpreterLike, ctx: SemaWebContext, callbackName: string, ev: Event) {
    const evHandle = storeHandle(ev, ctx);
    try {
      interp.evalStr(`(${callbackName} ${evHandle})`);
    } catch (e) {
      ctx.onerror(e instanceof Error ? e : new Error(String(e)), `event:${ev.type}:${callbackName}`);
    } finally {
      if (evHandle != null) ctx.handles.delete(evHandle);
    }
  }
}

/**
 * Register `component/*` namespace functions and the `mount!` Sema wrapper.
 *
 * Functions registered:
 * - `component/mount!` — mount a component function to a CSS selector
 * - `component/unmount!` — unmount a component
 * - `component/force-render!` — force re-render of a mounted component
 * - `__component/current-id` — get current component ID from render context stack
 *
 * Sema wrapper:
 * - `(mount! selector fn-name)` — convenience alias for component/mount!
 */
export function registerComponentBindings(interp: SemaInterpreterLike, ctx: SemaWebContext): void {
  // component/mount! — mount a component to a DOM target
  interp.registerFunction(
    "component/mount!",
    (selector: string, componentFn: string) => {
      // Validate component function name
      if (!SEMA_IDENT_RE.test(componentFn)) {
        throw new Error(`Invalid component function name: ${componentFn}`);
      }

      const target = document.querySelector(selector);
      if (!target) throw new Error(`mount! target not found: ${selector}`);

      // Unmount existing component at this selector
      const existing = ctx.mountedComponents.get(selector);
      if (existing) {
        if (existing.dispose) existing.dispose();
      }

      const component: MountedComponent = {
        target,
        componentFn,
        captureId: 0,
        dispose: null,
        localState: new Map(),
        mountCleanup: null,
        renderContextStack: [],
      };

      // Set up delegated event handling on the mount target
      const delegator = new EventDelegator();
      delegator.setup(target, interp, ctx);

      // Store component before rendering so local/on-mount can find it
      ctx.mountedComponents.set(selector, component);

      // Initial render via effect (will auto-track reactive dependencies)
      renderComponent(component, selector, interp, ctx);

      // Handle on-mount callback after first render
      const pendingMount = (component as any).__pendingMount as string | undefined;
      if (pendingMount && SEMA_IDENT_RE.test(pendingMount)) {
        try {
          // Call the mount callback — capture any returned cleanup function
          const cleanupCapName = `__mount_cleanup_${ctx.nextCaptureId++}`;
          let cleanupFnName: string | null = null;
          interp.registerFunction(cleanupCapName, (val: any) => {
            if (typeof val === "string") cleanupFnName = val;
            return null;
          });
          const mountResult = interp.evalStr(`(${cleanupCapName} (${pendingMount}))`);
          if (mountResult.error) {
            ctx.onerror(new Error(mountResult.error), `on-mount:${pendingMount}`);
          } else if (cleanupFnName && SEMA_IDENT_RE.test(cleanupFnName)) {
            // Store cleanup function to call on unmount
            const finalCleanupName = cleanupFnName;
            component.mountCleanup = () => {
              try {
                interp.evalStr(`(${finalCleanupName})`);
              } catch (e) {
                ctx.onerror(e instanceof Error ? e : new Error(String(e)), `unmount-cleanup:${finalCleanupName}`);
              }
            };
          }
        } catch (e) {
          ctx.onerror(e instanceof Error ? e : new Error(String(e)), `on-mount:${pendingMount}`);
        }
        delete (component as any).__pendingMount;
      }

      return null;
    },
  );

  // component/unmount! — unmount a component from a DOM target
  interp.registerFunction("component/unmount!", (selector: string) => {
    const component = ctx.mountedComponents.get(selector);
    if (component) {
      if (component.mountCleanup) component.mountCleanup();
      if (component.dispose) component.dispose();
      component.target.innerHTML = "";
      ctx.mountedComponents.delete(selector);
    }
    return null;
  });

  // component/force-render! — force re-render by disposing and re-creating effect
  interp.registerFunction("component/force-render!", (selector: string) => {
    const component = ctx.mountedComponents.get(selector);
    if (component) {
      renderComponent(component, selector, interp, ctx);
    }
    return null;
  });

  // __component/current-id — get current component from render context stack
  interp.registerFunction("__component/current-id", () => {
    const stack = ctx.renderContextStack;
    return stack.length > 0 ? stack[stack.length - 1] : null;
  });

  // __component/local — component-scoped state (name-based, no call-order dependency)
  interp.registerFunction("__component/local", (name: string, initialValue: any) => {
    const stack = ctx.renderContextStack;
    if (stack.length === 0) {
      throw new Error("(local) called outside of a component render context");
    }
    const componentFnName = stack[stack.length - 1];
    const component = findComponentByFn(ctx, componentFnName);
    if (!component) {
      throw new Error(`(local) no mounted component for "${componentFnName}"`);
    }

    // Key by name within this component's local state
    const existing = component.localState.get(name);
    if (existing) {
      // Return the signal ID for the existing local state
      for (const [id, s] of ctx.signals) {
        if (s === existing) return id;
      }
      // Should not happen, but create a new mapping if needed
      const id = ctx.nextSignalId++;
      ctx.signals.set(id, existing);
      return id;
    }

    // First call: create a new signal
    const id = ctx.nextSignalId++;
    const s = signal(initialValue);
    ctx.signals.set(id, s);
    component.localState.set(name, s);
    return id;
  });

  // __component/on-mount — register lifecycle callback, called once after first render
  interp.registerFunction("__component/on-mount", (callbackName: string) => {
    if (!SEMA_IDENT_RE.test(callbackName)) {
      throw new Error(`Invalid on-mount callback name: ${callbackName}`);
    }
    const stack = ctx.renderContextStack;
    if (stack.length === 0) {
      throw new Error("(on-mount) called outside of a component render context");
    }
    const componentFnName = stack[stack.length - 1];
    const component = findComponentByFn(ctx, componentFnName);
    if (!component) {
      throw new Error(`(on-mount) no mounted component for "${componentFnName}"`);
    }

    // Store the callback name as a string; it will be invoked after first render
    (component as any).__pendingMount = callbackName;
    return null;
  });

  // js/set-interval — browser setInterval wrapper
  interp.registerFunction("js/set-interval", (callbackName: string, ms: number) => {
    if (!SEMA_IDENT_RE.test(callbackName)) {
      throw new Error(`Invalid interval callback name: ${callbackName}`);
    }
    const id = setInterval(() => {
      try {
        interp.evalStr(`(${callbackName})`);
      } catch (e) {
        ctx.onerror(e instanceof Error ? e : new Error(String(e)), `interval:${callbackName}`);
      }
    }, ms);
    return id;
  });

  // js/clear-interval — browser clearInterval wrapper
  interp.registerFunction("js/clear-interval", (id: number) => {
    clearInterval(id);
    return null;
  });

  // --- Sema-side convenience wrappers ---
  const semaResult = interp.evalStr(`
    (defmacro mount! (selector component-name)
      \`(component/mount! ,selector
         ,(if (symbol? component-name)
            (symbol->string component-name)
            component-name)))

    (define (local name initial) (__component/local name initial))

    (define (on-mount fn) (__component/on-mount fn))

    (defmacro defcomponent (name params . body)
      \`(define ,name
         (fn ,params ,@body)))
  `);

  if (semaResult.error) {
    throw new Error(`[sema-web] Failed to register component wrappers: ${semaResult.error}`);
  }
}
