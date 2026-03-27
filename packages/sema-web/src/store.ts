/**
 * Store bindings for Sema — registers `store/*` namespace functions.
 *
 * Provides localStorage and sessionStorage access from Sema code.
 *
 * @module
 */

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
}

/**
 * Register all `store/*` namespace functions on the given interpreter.
 *
 * Functions registered:
 * - `store/get` — get value from localStorage
 * - `store/set!` — set value in localStorage
 * - `store/remove!` — remove key from localStorage
 * - `store/clear!` — clear all localStorage
 * - `store/keys` — list all localStorage keys
 * - `store/has?` — check if key exists in localStorage
 * - `store/session-get` — get value from sessionStorage
 * - `store/session-set!` — set value in sessionStorage
 * - `store/session-remove!` — remove key from sessionStorage
 * - `store/session-clear!` — clear all sessionStorage
 *
 * Values are automatically serialized/deserialized as JSON when they
 * are not simple strings.
 */
export function registerStoreBindings(interp: SemaInterpreterLike): void {
  // --- localStorage ---

  interp.registerFunction("store/get", (key: string) => {
    const val = localStorage.getItem(key);
    if (val === null) return null;
    try {
      return JSON.parse(val);
    } catch {
      return val;
    }
  });

  interp.registerFunction("store/set!", (key: string, value: any) => {
    const serialized = typeof value === "string" ? value : JSON.stringify(value);
    localStorage.setItem(key, serialized);
    return null;
  });

  interp.registerFunction("store/remove!", (key: string) => {
    localStorage.removeItem(key);
    return null;
  });

  interp.registerFunction("store/clear!", () => {
    localStorage.clear();
    return null;
  });

  interp.registerFunction("store/keys", () => {
    const keys: string[] = [];
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key !== null) keys.push(key);
    }
    return JSON.stringify(keys);
  });

  interp.registerFunction("store/has?", (key: string) => {
    return localStorage.getItem(key) !== null;
  });

  // --- sessionStorage ---

  interp.registerFunction("store/session-get", (key: string) => {
    const val = sessionStorage.getItem(key);
    if (val === null) return null;
    try {
      return JSON.parse(val);
    } catch {
      return val;
    }
  });

  interp.registerFunction("store/session-set!", (key: string, value: any) => {
    const serialized = typeof value === "string" ? value : JSON.stringify(value);
    sessionStorage.setItem(key, serialized);
    return null;
  });

  interp.registerFunction("store/session-remove!", (key: string) => {
    sessionStorage.removeItem(key);
    return null;
  });

  interp.registerFunction("store/session-clear!", () => {
    sessionStorage.clear();
    return null;
  });
}
