/**
 * Component system for Sema — reactive mounting of hiccup views.
 *
 * Provides `mount!` to bind a Sema component function to a DOM element.
 * The component automatically re-renders when atoms it depends on change.
 *
 * ## Usage
 *
 * ```sema
 * (define count (atom 0))
 *
 * (define (counter-view)
 *   [:div {:class "counter"}
 *     [:p (deref count)]
 *     [:button {:on-click "increment"} "+"]])
 *
 * (define (increment ev) (swap! count (lambda (n) (+ n 1))))
 *
 * (mount! "#app" "counter-view")
 * ```
 *
 * @module
 */

import { renderHiccup } from "./hiccup.js";
import {
  startTracking,
  stopTracking,
  addInternalWatcher,
  removeInternalWatcher,
} from "./reactive.js";

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

interface MountedComponent {
  target: Element;
  componentFn: string;
  deps: Set<number>;
  renderPending: boolean;
  cleanup: () => void;
}

const mountedComponents = new Map<string, MountedComponent>();

/**
 * Call a Sema component function and capture its structured return value.
 *
 * Uses a registered capture function to get the actual JS value
 * (vectors/maps) rather than the string representation from evalStr.
 */
function callComponent(interp: SemaInterpreterLike, fnName: string): any {
  let captured: any = null;
  interp.registerFunction("__component-capture", (val: any) => {
    captured = val;
    return null;
  });
  const result = interp.evalStr(`(__component-capture (${fnName}))`);
  if (result.error) {
    console.error(`[sema-web] Component error (${fnName}): ${result.error}`);
    return null;
  }
  return captured;
}

/**
 * Render a mounted component: track dependencies, build DOM, set up watchers.
 */
function renderComponent(
  component: MountedComponent,
  selector: string,
  interp: SemaInterpreterLike,
): void {
  // Remove old watchers
  component.cleanup();
  component.deps.clear();

  // Start tracking atom dependencies
  startTracking();

  // Call the component function to get hiccup data
  const hiccupData = callComponent(interp, component.componentFn);

  // Stop tracking — get the set of atoms that were deref'd
  component.deps = stopTracking();

  // Render hiccup to DOM
  component.target.innerHTML = "";
  if (hiccupData != null) {
    const node = renderHiccup(hiccupData, interp);
    component.target.appendChild(node);
  }

  // Watch all dependent atoms for changes → re-render
  const watcherKeys: string[] = [];
  component.deps.forEach((atomId) => {
    const key = `__mount:${selector}:${atomId}`;
    watcherKeys.push(key);
    addInternalWatcher(atomId, key, () => {
      // Debounce: schedule re-render on next animation frame
      if (!component.renderPending) {
        component.renderPending = true;
        requestAnimationFrame(() => {
          component.renderPending = false;
          if (mountedComponents.has(selector)) {
            renderComponent(component, selector, interp);
          }
        });
      }
    });
  });

  component.cleanup = () => {
    for (const key of watcherKeys) {
      component.deps.forEach((atomId) => {
        removeInternalWatcher(atomId, key);
      });
    }
  };
}

/**
 * Register `component/*` namespace functions and the `mount!` Sema wrapper.
 *
 * Functions registered:
 * - `component/mount!` — mount a component function to a CSS selector
 * - `component/unmount!` — unmount a component
 * - `component/force-render!` — force re-render of a mounted component
 *
 * Sema wrapper:
 * - `(mount! selector fn-name)` — convenience alias for component/mount!
 */
export function registerComponentBindings(interp: SemaInterpreterLike): void {
  // component/mount! — mount a component to a DOM target
  interp.registerFunction(
    "component/mount!",
    (selector: string, componentFn: string) => {
      // Validate component function name
      if (!/^[a-zA-Z_][a-zA-Z0-9_/\-?!*><=+.]*$/.test(componentFn)) {
        throw new Error(`Invalid component function name: ${componentFn}`);
      }

      const target = document.querySelector(selector);
      if (!target) throw new Error(`mount! target not found: ${selector}`);

      // Unmount existing component at this selector
      const existing = mountedComponents.get(selector);
      if (existing) {
        existing.cleanup();
      }

      const component: MountedComponent = {
        target,
        componentFn,
        deps: new Set(),
        renderPending: false,
        cleanup: () => {},
      };

      // Initial render
      renderComponent(component, selector, interp);
      mountedComponents.set(selector, component);
      return null;
    },
  );

  // component/unmount! — unmount a component from a DOM target
  interp.registerFunction("component/unmount!", (selector: string) => {
    const component = mountedComponents.get(selector);
    if (component) {
      component.cleanup();
      component.target.innerHTML = "";
      mountedComponents.delete(selector);
    }
    return null;
  });

  // component/force-render! — force re-render of a mounted component
  interp.registerFunction("component/force-render!", (selector: string) => {
    const component = mountedComponents.get(selector);
    if (component) {
      renderComponent(component, selector, interp);
    }
    return null;
  });

  // --- Sema-side convenience wrapper ---
  interp.evalStr(`
    (define (mount! selector component-fn)
      (component/mount! selector component-fn))
  `);
}
