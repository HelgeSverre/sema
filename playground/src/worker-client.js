// Main-thread client for the Sema eval Web Worker (M3).
//
// Enables real wall-clock async/sleep by running eval on a worker that blocks
// on Atomics.wait. Gated: requires cross-origin isolation (for SharedArrayBuffer)
// AND an explicit ?worker opt-in, because VFS/HTTP/debugger are not yet routed
// to the worker (M4/M5/M7). Without the opt-in, the playground uses the
// main-thread interpreter exactly as before.

let worker = null;
let ready = null;
let nextId = 1;
let controlView = null; // Int32Array over the shared control SAB (slot 0)
const pending = new Map();

/** True when the worker eval path is available and opted into. */
export function workerEvalEnabled() {
  return (
    typeof SharedArrayBuffer !== 'undefined' &&
    self.crossOriginIsolated === true &&
    new URLSearchParams(location.search).has('worker')
  );
}

/** Spawn the worker and wait for it to load wasm + install the sleep hook. */
export function initWorker() {
  if (ready) return ready;
  worker = new Worker(new URL('dist/sema-worker.js', document.baseURI), { type: 'module' });
  // 4-byte control buffer: the worker blocks on it (Atomics.wait for sleep) and
  // the main thread stores a cancel flag + Atomics.notify into slot 0 to stop a
  // running program (see cancelWorker).
  const sab = new SharedArrayBuffer(4);
  controlView = new Int32Array(sab);
  worker.addEventListener('message', (e) => {
    const m = e.data;
    if (m && m.type === 'result') {
      const resolve = pending.get(m.id);
      if (resolve) {
        pending.delete(m.id);
        resolve({ result: m.result, vfs: m.vfs });
      }
    }
  });
  ready = new Promise((res) => {
    const onReady = (e) => {
      if (e.data && e.data.type === 'ready') {
        worker.removeEventListener('message', onReady);
        res();
      }
    };
    worker.addEventListener('message', onReady);
    worker.postMessage({ type: 'init', sab });
  });
  return ready;
}

/** Request cancellation of the running eval: set the cancel flag (slot 0) and
 *  wake any in-progress Atomics.wait sleep. The VM loop guard / scheduler abort
 *  shortly after; the worker survives (defines + VFS preserved). */
export function cancelWorker() {
  if (!controlView) return;
  Atomics.store(controlView, 0, 1);
  Atomics.notify(controlView, 0);
}

/** Evaluate `code` on the worker, seeding it with `vfs` (a dumpVfs snapshot).
 *  Resolves to { result: {value,output,error}, vfs: snapshot-after-run }. */
export async function evalViaWorker(code, vfs) {
  await ready;
  const id = nextId++;
  return new Promise((resolve) => {
    pending.set(id, resolve);
    worker.postMessage({ type: 'eval', id, code, vfs });
  });
}
