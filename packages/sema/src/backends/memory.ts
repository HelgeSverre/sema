import type { VFSBackend, VFSHost } from "../vfs.js";

/**
 * Ephemeral VFS backend — no persistence.
 *
 * Files exist only in the WASM memory and are lost on page reload.
 * Use this when you don't need persistence, or for testing.
 *
 * @example
 * ```ts
 * import { SemaInterpreter, MemoryBackend } from "@sema-lang/sema";
 *
 * const sema = await SemaInterpreter.create({
 *   vfs: new MemoryBackend(),
 * });
 * ```
 */
export class MemoryBackend implements VFSBackend {
  async hydrate(_host: VFSHost): Promise<void> {}
  async flush(_host: VFSHost): Promise<void> {}
  async reset(): Promise<void> {}
}
