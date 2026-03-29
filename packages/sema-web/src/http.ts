/**
 * Browser-specific HTTP bindings for Sema Web.
 *
 * The WASM interpreter already handles `http/get` and `http/post` via
 * the replay-with-cache mechanism. This module adds browser-specific
 * wrappers for SSE (EventSource) connections, which are useful for
 * streaming LLM responses and real-time updates.
 *
 * ## Usage
 *
 * ```sema
 * ;; Create an SSE connection — returns a signal ID
 * (def stream (http/event-source "https://api.example.com/stream"))
 *
 * ;; Read latest message (auto-tracked in effects/computed)
 * @stream  ;; => {:data "..." :done false :error nil}
 *
 * ;; Close when done
 * (http/close-event-source stream)
 * ```
 *
 * @module
 */

import { signal } from "@preact/signals-core";
import type { SemaWebContext } from "./context.js";

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
}

/**
 * Register `http/*` browser-specific namespace functions.
 *
 * Functions registered:
 * - `http/event-source` — create an SSE connection, returns a signal ID
 * - `http/close-event-source` — close an SSE connection
 */
export function registerHttpBindings(interp: SemaInterpreterLike, ctx: SemaWebContext): void {
  // http/event-source — create SSE connection, return signal ID
  interp.registerFunction("http/event-source", (url: string) => {
    const id = ctx.nextSignalId++;
    const s = signal<{ data: string | null; done: boolean; error: string | null }>({
      data: null,
      done: false,
      error: null,
    });
    ctx.signals.set(id, s as any);

    const es = new EventSource(url);
    es.onmessage = (ev) => {
      s.value = { ...s.value, data: ev.data };
    };
    es.onerror = () => {
      s.value = { ...s.value, done: true, error: "EventSource error" };
      es.close();
    };
    // Store reference for cleanup
    (s as any).__eventSource = es;

    return id;
  });

  // http/close-event-source — close an SSE connection
  interp.registerFunction("http/close-event-source", (signalId: number) => {
    const s = ctx.signals.get(signalId);
    if (s && (s as any).__eventSource) {
      (s as any).__eventSource.close();
      (s as any).__eventSource = null;
    }
    return null;
  });
}
