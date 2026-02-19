import type { VFSBackend, VFSHost } from "../vfs.js";

/** Options for LocalStorageBackend. */
export interface LocalStorageBackendOptions {
  /**
   * Namespace prefix for localStorage keys.
   * Each file is stored as `${namespace}:f:${path}`.
   * Directories are stored in a manifest key `${namespace}:__dirs__`.
   * @default "sema-vfs"
   */
  namespace?: string;
}

/**
 * VFS backend that persists files to localStorage.
 *
 * Simple and synchronous — good for small projects (&lt; 5 MB).
 * localStorage has a ~5–10 MB limit per origin in most browsers.
 *
 * @example
 * ```ts
 * const sema = await SemaInterpreter.create({
 *   vfs: new LocalStorageBackend({ namespace: "my-project" }),
 * });
 * ```
 */
export class LocalStorageBackend implements VFSBackend {
  private ns: string;
  private filePrefix: string;
  private dirsKey: string;

  constructor(opts?: LocalStorageBackendOptions) {
    this.ns = opts?.namespace ?? "sema-vfs";
    this.filePrefix = this.ns + ":f:";
    this.dirsKey = this.ns + ":__dirs__";
  }

  async hydrate(host: VFSHost): Promise<void> {
    // Restore directories first (so file writes into them work)
    const dirsJson = localStorage.getItem(this.dirsKey);
    if (dirsJson) {
      try {
        const dirs: string[] = JSON.parse(dirsJson);
        for (const dir of dirs) {
          host.mkdir(dir);
        }
      } catch { /* ignore corrupt data */ }
    }

    // Restore files
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key && key.startsWith(this.filePrefix)) {
        const path = key.slice(this.filePrefix.length);
        const content = localStorage.getItem(key);
        if (content !== null) {
          host.writeFile(path, content);
        }
      }
    }
  }

  async flush(host: VFSHost): Promise<void> {
    // Clear old entries for this namespace
    const toRemove: string[] = [];
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key && (key.startsWith(this.filePrefix) || key === this.dirsKey)) {
        toRemove.push(key);
      }
    }
    for (const key of toRemove) {
      localStorage.removeItem(key);
    }

    // Write current files
    const allFiles = this.collectFiles(host, "/");
    for (const path of allFiles) {
      const content = host.readFile(path);
      if (content !== null) {
        localStorage.setItem(this.filePrefix + path, content);
      }
    }

    // Write directory manifest
    const dirs = this.collectDirs(host, "/");
    localStorage.setItem(this.dirsKey, JSON.stringify(dirs));
  }

  async reset(): Promise<void> {
    const toRemove: string[] = [];
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key && (key.startsWith(this.filePrefix) || key === this.dirsKey)) {
        toRemove.push(key);
      }
    }
    for (const key of toRemove) {
      localStorage.removeItem(key);
    }
  }

  /** Recursively collect all file paths. */
  private collectFiles(host: VFSHost, dir: string): string[] {
    const result: string[] = [];
    const entries = host.listFiles(dir);
    for (const name of entries) {
      const full = dir === "/" ? "/" + name : dir + "/" + name;
      if (host.isDirectory(full)) {
        result.push(...this.collectFiles(host, full));
      } else {
        result.push(full);
      }
    }
    return result;
  }

  /** Recursively collect all directory paths. */
  private collectDirs(host: VFSHost, dir: string): string[] {
    const result: string[] = [];
    const entries = host.listFiles(dir);
    for (const name of entries) {
      const full = dir === "/" ? "/" + name : dir + "/" + name;
      if (host.isDirectory(full)) {
        result.push(full);
        result.push(...this.collectDirs(host, full));
      }
    }
    return result;
  }
}
