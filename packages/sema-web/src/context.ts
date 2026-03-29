/**
 * Instance-scoped state container for Sema Web.
 *
 * All module-level singletons (handles, signals, mounted components, etc.)
 * are collected here so that multiple SemaWeb instances can coexist
 * without interference.
 *
 * @module
 */

import type { Signal } from "@preact/signals-core";

/** A mounted component managed by the component system. */
export interface MountedComponent {
  target: Element;
  componentFn: string;
  captureId: number;
  dispose: (() => void) | null;
  localState: Map<string, Signal<any>>;
  mountCleanup: (() => void) | null;
  renderContextStack: string[];
}

/** Error handler callback type. */
export type ErrorHandler = (error: Error, context: string) => void;

/**
 * Per-instance state container for SemaWeb.
 *
 * Each `SemaWeb.create()` call produces its own `SemaWebContext`,
 * ensuring complete isolation between instances (handles, signals,
 * mounted components, event listeners, etc.).
 */
export class SemaWebContext {
  /** DOM element/text/event handles */
  handles = new Map<number, Element | Text | Event>();
  nextHandle = 1;

  /** Reactive signals */
  signals = new Map<number, Signal<any>>();
  nextSignalId = 1;

  /** Mounted components */
  mountedComponents = new Map<string, MountedComponent>();

  /** Next capture ID for callComponent */
  nextCaptureId = 1;

  /** Component render context stack (per-instance for multi-instance isolation) */
  renderContextStack: string[] = [];

  /** DOM event listeners registry */
  listeners = new Map<string, EventListener>();

  /** Error handler */
  onerror: ErrorHandler = (error, context) => {
    console.error(`[sema-web] Error in ${context}:`, error);
  };
}
