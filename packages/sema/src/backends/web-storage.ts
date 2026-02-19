import type { VFSBackend, VFSHost } from "../vfs.js";

/** Options for WebStorageBackend and its subclasses. */
export interface WebStorageBackendOptions {
  /**
   * Namespace prefix for storage keys.
   * Each file is stored as `${namespace}:f:${path}`.
   * Directories are stored in a manifest key `${namespace}:__dirs__`.
   * @default "sema-vfs"
   */
  namespace?: string;
}

/**
 * Base class for VFS backends backed by a Web Storage API (`Storage`) object.
 *
 * Handles hydrate/flush/reset using namespace-prefixed keys and a directory
 * manifest.  Subclasses only need to pass the concrete `Storage` instance
 * (e.g. `localStorage` or `sessionStorage`).
 */
export class WebStorageBackend implements VFSBackend {
  private storage: Storage;
  private ns: string;
  private filePrefix: string;
  private dirsKey: string;

  constructor(storage: Storage, opts?: WebStorageBackendOptions) {
    this.storage = storage;
    this.ns = opts?.namespace ?? "sema-vfs";
    this.filePrefix = this.ns + ":f:";
    this.dirsKey = this.ns + ":__dirs__";
  }

  async hydrate(host: VFSHost): Promise<void> {
    // Restore directories first (so file writes into them work)
    const dirsJson = this.storage.getItem(this.dirsKey);
    if (dirsJson) {
      try {
        const dirs: string[] = JSON.parse(dirsJson);
        for (const dir of dirs) {
          host.mkdir(dir);
        }
      } catch { /* ignore corrupt data */ }
    }

    // Restore files
    for (let i = 0; i < this.storage.length; i++) {
      const key = this.storage.key(i);
      if (key && key.startsWith(this.filePrefix)) {
        const path = key.slice(this.filePrefix.length);
        const content = this.storage.getItem(key);
        if (content !== null) {
          host.writeFile(path, content);
        }
      }
    }
  }

  async flush(host: VFSHost): Promise<void> {
    // Clear old entries for this namespace
    const toRemove: string[] = [];
    for (let i = 0; i < this.storage.length; i++) {
      const key = this.storage.key(i);
      if (key && (key.startsWith(this.filePrefix) || key === this.dirsKey)) {
        toRemove.push(key);
      }
    }
    for (const key of toRemove) {
      this.storage.removeItem(key);
    }

    // Write current files
    const allFiles = this.collectFiles(host, "/");
    for (const path of allFiles) {
      const content = host.readFile(path);
      if (content !== null) {
        this.storage.setItem(this.filePrefix + path, content);
      }
    }

    // Write directory manifest
    const dirs = this.collectDirs(host, "/");
    this.storage.setItem(this.dirsKey, JSON.stringify(dirs));
  }

  async reset(): Promise<void> {
    const toRemove: string[] = [];
    for (let i = 0; i < this.storage.length; i++) {
      const key = this.storage.key(i);
      if (key && (key.startsWith(this.filePrefix) || key === this.dirsKey)) {
        toRemove.push(key);
      }
    }
    for (const key of toRemove) {
      this.storage.removeItem(key);
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
