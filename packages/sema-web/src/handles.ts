/**
 * Shared element handle system for Sema web bindings.
 *
 * Provides a numeric handle map so Sema can reference DOM elements,
 * text nodes, and events by numeric ID across the WASM boundary.
 *
 * @module
 */

/** Handle map storing DOM objects by numeric ID. */
const handles = new Map<number, Element | Text | Event>();
let nextHandle = 1;

/**
 * Store a DOM object and return its numeric handle.
 * Returns null if the object is null/undefined.
 */
export function storeHandle(obj: Element | Text | Event | null): number | null {
  if (obj == null) return null;
  const id = nextHandle++;
  handles.set(id, obj);
  return id;
}

/**
 * Retrieve an Element by handle ID.
 * @throws Error if handle is invalid or not an Element
 */
export function getElement(id: number): Element {
  const el = handles.get(id);
  if (!el || !(el instanceof Element)) {
    throw new Error(`Invalid element handle: ${id}`);
  }
  return el;
}

/**
 * Retrieve an Element or Text node by handle ID.
 * @throws Error if handle is invalid
 */
export function getNode(id: number): Element | Text {
  const node = handles.get(id);
  if (!node || (!(node instanceof Element) && !(node instanceof Text))) {
    throw new Error(`Invalid node handle: ${id}`);
  }
  return node;
}

/**
 * Retrieve an Event by handle ID.
 * @throws Error if handle is invalid or not an Event
 */
export function getEvent(id: number): Event {
  const ev = handles.get(id);
  if (!ev || !(ev instanceof Event)) {
    throw new Error(`Invalid event handle: ${id}`);
  }
  return ev;
}
