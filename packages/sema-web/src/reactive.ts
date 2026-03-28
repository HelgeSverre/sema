/**
 * Reactive atom system for Sema — registers `atom/*` namespace functions.
 *
 * Atoms are mutable reference cells with change notification.
 * They form the foundation of Sema's reactive rendering system.
 *
 * ## Sema API
 *
 * ```sema
 * (define count (atom 0))        ;; create atom
 * (deref count)                  ;; read value
 * (reset! count 42)              ;; set value
 * (swap! count (lambda (n) (+ n 1)))  ;; update with function
 * ```
 *
 * @module
 */

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/** Internal atom storage. */
interface Atom {
  value: any;
  watchers: Map<string, (oldVal: any, newVal: any) => void>;
}

const atoms = new Map<number, Atom>();
let nextAtomId = 1;

// --- Dependency tracking ---
// Stack-based: each render context pushes a new tracking set.
// atom/deref records the atom ID into the top-of-stack set.
const trackingStack: Set<number>[] = [];

/** Start tracking atom reads (called before rendering a component). */
export function startTracking(): void {
  trackingStack.push(new Set());
}

/** Stop tracking and return the set of atom IDs that were read. */
export function stopTracking(): Set<number> {
  return trackingStack.pop() ?? new Set();
}

/** Add an internal watcher (used by the component system, not exposed to Sema). */
export function addInternalWatcher(
  atomId: number,
  key: string,
  fn: (oldVal: any, newVal: any) => void,
): void {
  const atom = atoms.get(atomId);
  if (atom) {
    atom.watchers.set(key, fn);
  }
}

/** Remove an internal watcher. */
export function removeInternalWatcher(atomId: number, key: string): void {
  const atom = atoms.get(atomId);
  if (atom) {
    atom.watchers.delete(key);
  }
}

/**
 * Register all `atom/*` namespace functions and Sema-side convenience wrappers.
 *
 * JS-level functions (atom/* namespace):
 * - `atom/create` — create atom, returns numeric handle
 * - `atom/deref` — read value (tracks dependency in reactive context)
 * - `atom/reset!` — set value, notify watchers
 * - `atom/add-watch` — add named watcher with Sema callback
 * - `atom/remove-watch` — remove named watcher
 *
 * Sema-level wrappers (convenience):
 * - `(atom val)` — create atom
 * - `(deref a)` — read atom
 * - `(reset! a val)` — set atom
 * - `(swap! a f . args)` — update atom by applying function
 */
export function registerReactiveBindings(interp: SemaInterpreterLike): void {
  // atom/create — create a new atom, returns numeric handle
  interp.registerFunction("atom/create", (initialValue: any) => {
    const id = nextAtomId++;
    atoms.set(id, { value: initialValue, watchers: new Map() });
    return id;
  });

  // atom/deref — read atom value, tracks dependency if in tracking context
  interp.registerFunction("atom/deref", (atomId: number) => {
    const atom = atoms.get(atomId);
    if (!atom) throw new Error(`Unknown atom: ${atomId}`);
    // Track this atom as a dependency of the current render context
    if (trackingStack.length > 0) {
      trackingStack[trackingStack.length - 1].add(atomId);
    }
    return atom.value;
  });

  // atom/reset! — set atom value, notify all watchers
  interp.registerFunction("atom/reset!", (atomId: number, newValue: any) => {
    const atom = atoms.get(atomId);
    if (!atom) throw new Error(`Unknown atom: ${atomId}`);
    const oldValue = atom.value;
    atom.value = newValue;
    // Notify watchers (component system and user watchers)
    for (const fn of atom.watchers.values()) {
      try {
        fn(oldValue, newValue);
      } catch (e) {
        console.error("[sema-web] Watcher error:", e);
      }
    }
    return newValue;
  });

  // atom/add-watch — add a named watcher with a Sema callback
  interp.registerFunction(
    "atom/add-watch",
    (atomId: number, key: string, callbackName: string) => {
      const atom = atoms.get(atomId);
      if (!atom) throw new Error(`Unknown atom: ${atomId}`);
      // Validate callback name to prevent code injection
      if (!/^[a-zA-Z_][a-zA-Z0-9_/\-?!*><=+.]*$/.test(callbackName)) {
        throw new Error(`Invalid callback name: ${callbackName}`);
      }
      atom.watchers.set(key, (_oldVal, _newVal) => {
        try {
          interp.evalStr(`(${callbackName})`);
        } catch (e) {
          console.error(`[sema-web] Watch callback error (${callbackName}):`, e);
        }
      });
      return null;
    },
  );

  // atom/remove-watch — remove a named watcher
  interp.registerFunction("atom/remove-watch", (atomId: number, key: string) => {
    const atom = atoms.get(atomId);
    if (atom) {
      atom.watchers.delete(key);
    }
    return null;
  });

  // --- Sema-side convenience wrappers ---
  interp.evalStr(`
    (define (atom val) (atom/create val))
    (define (deref a) (atom/deref a))
    (define (reset! a val) (atom/reset! a val))
    (define (swap! a f . args)
      (reset! a (apply f (cons (deref a) args))))
  `);
}
