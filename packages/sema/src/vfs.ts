/**
 * Host bridge — methods the backend calls to read/write the in-memory WASM VFS.
 * Provided by SemaInterpreter, not implemented by users.
 */
export interface VFSHost {
  readFile(path: string): string | null;
  writeFile(path: string, content: string): void;
  deleteFile(path: string): boolean;
  mkdir(path: string): void;
  listFiles(dir: string): string[];
  fileExists(path: string): boolean;
  isDirectory(path: string): boolean;
  resetVFS(): void;
}

/**
 * Pluggable VFS storage backend.
 *
 * Implement this interface to persist VFS state across page reloads.
 * The backend runs outside the eval loop, so async is allowed.
 *
 * Built-in implementations:
 * - {@link LocalStorageBackend} — persist to localStorage
 *
 * @example
 * ```ts
 * const sema = await SemaInterpreter.create({
 *   vfs: new LocalStorageBackend({ namespace: "my-app" }),
 * });
 * await sema.evalStrAsync(code);
 * await sema.flushVFS(); // persist changes
 * ```
 */
export interface VFSBackend {
  /** Optional: open DB connections, request permissions, etc. */
  init?(): Promise<void>;

  /**
   * Populate the in-memory WASM VFS from persistent storage.
   * Called once during `SemaInterpreter.create()`.
   */
  hydrate(host: VFSHost): Promise<void>;

  /**
   * Persist current in-memory VFS state to storage.
   * Called explicitly via `sema.flushVFS()`.
   */
  flush(host: VFSHost): Promise<void>;

  /** Optional: clear all persistent data. */
  reset?(): Promise<void>;
}
