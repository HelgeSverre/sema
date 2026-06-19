// Sema eval Web Worker (M3).
//
// Runs the WASM VM off the main UI thread so it can block on Atomics.wait for
// REAL wall-clock `async/sleep` (installed via installAtomicsSleep) without
// freezing the page. The main thread talks to it over postMessage:
//   main -> { type:'init', sab }      worker: load wasm, install sleep hook
//   worker -> { type:'ready' }
//   main -> { type:'eval', id, code } worker: run, then post the result
//   worker -> { type:'result', id, result:{value,output,error} }
//
// NOTE (M3 scope): VFS, HTTP, and the debugger are not yet routed here (M4/M5/
// M7). The worker eval path is opt-in (?worker) until those land.
import init, { SemaInterpreter } from '../pkg/sema_wasm.js';

let interp = null;

self.onmessage = async (e) => {
  const msg = e.data;
  try {
    if (msg.type === 'init') {
      await init();
      interp = new SemaInterpreter();
      if (msg.sab) {
        // Build the Int32Array view here; the SAB is shared with the main
        // thread (for future cancel/notify in M6).
        interp.installAtomicsSleep(new Int32Array(msg.sab));
      }
      self.postMessage({ type: 'ready' });
      return;
    }
    if (msg.type === 'eval') {
      // Seed the worker's VFS from the main thread's mirror, run, then return
      // the resulting VFS so the main thread can reflect any file changes.
      if (msg.vfs !== undefined) interp.loadVfs(msg.vfs);
      const result = await interp.evalVMAsync(msg.code);
      const vfs = interp.dumpVfs();
      self.postMessage({ type: 'result', id: msg.id, result, vfs });
      return;
    }
  } catch (err) {
    const message = (err && err.message) ? err.message : String(err);
    self.postMessage({
      type: 'result',
      id: msg && msg.id,
      result: { value: null, output: [], error: message },
    });
  }
};
