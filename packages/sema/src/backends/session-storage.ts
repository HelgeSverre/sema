import { WebStorageBackend } from "./web-storage.js";
import type { WebStorageBackendOptions } from "./web-storage.js";

/** Options for SessionStorageBackend. */
export type SessionStorageBackendOptions = WebStorageBackendOptions;

/**
 * VFS backend that persists files to sessionStorage.
 *
 * Data persists within the current browser tab/window session only —
 * it is cleared when the tab is closed.  Works well for scratch pads,
 * playground-style editors, or any context where cross-session
 * persistence is unnecessary.
 *
 * Like {@link LocalStorageBackend}, the ~5–10 MB per-origin limit applies.
 *
 * @example
 * ```ts
 * const sema = await SemaInterpreter.create({
 *   vfs: new SessionStorageBackend({ namespace: "playground" }),
 * });
 * ```
 */
export class SessionStorageBackend extends WebStorageBackend {
  constructor(opts?: SessionStorageBackendOptions) {
    super(sessionStorage, opts);
  }
}
