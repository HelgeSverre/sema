/**
 * DOM bindings for Sema — registers `dom/*` namespace functions.
 *
 * These functions provide a thin mirror of the browser DOM API,
 * exposed as Sema functions via the interpreter's registerFunction API.
 *
 * @module
 */

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
}

/**
 * Register all `dom/*` namespace functions on the given interpreter.
 *
 * Functions registered:
 * - `dom/query` — querySelector, returns element or nil
 * - `dom/query-all` — querySelectorAll, returns list of elements
 * - `dom/create-element` — createElement
 * - `dom/create-text` — createTextNode
 * - `dom/append-child!` — appendChild
 * - `dom/remove-child!` — removeChild
 * - `dom/remove!` — remove element from DOM
 * - `dom/set-attribute!` — setAttribute
 * - `dom/get-attribute` — getAttribute
 * - `dom/remove-attribute!` — removeAttribute
 * - `dom/add-class!` — add CSS class(es)
 * - `dom/remove-class!` — remove CSS class(es)
 * - `dom/toggle-class!` — toggle a CSS class
 * - `dom/has-class?` — check if element has a CSS class
 * - `dom/set-style!` — set a style property
 * - `dom/get-style` — get a style property
 * - `dom/set-text!` — set textContent
 * - `dom/get-text` — get textContent
 * - `dom/set-html!` — set innerHTML
 * - `dom/get-html` — get innerHTML
 * - `dom/on!` — addEventListener
 * - `dom/off!` — removeEventListener
 * - `dom/prevent-default!` — event.preventDefault()
 * - `dom/set-value!` — set input value
 * - `dom/get-value` — get input value
 * - `dom/get-id` — get element by id
 */
export function registerDomBindings(interp: SemaInterpreterLike): void {
  // --- Element handles ---
  // We store DOM elements in a WeakRef-friendly handle map so Sema
  // can reference them by numeric ID. This avoids passing raw objects
  // through the JSON serialization boundary.
  const handles = new Map<number, Element | Text | Event>();
  let nextHandle = 1;

  function storeHandle(el: Element | Text | Event | null): number | null {
    if (el == null) return null;
    const id = nextHandle++;
    handles.set(id, el);
    return id;
  }

  function getElement(id: number): Element {
    const el = handles.get(id);
    if (!el || !(el instanceof Element)) {
      throw new Error(`Invalid element handle: ${id}`);
    }
    return el;
  }

  function getNode(id: number): Element | Text {
    const el = handles.get(id);
    if (!el || (!(el instanceof Element) && !(el instanceof Text))) {
      throw new Error(`Invalid node handle: ${id}`);
    }
    return el;
  }

  function getEvent(id: number): Event {
    const ev = handles.get(id);
    if (!ev || !(ev instanceof Event)) {
      throw new Error(`Invalid event handle: ${id}`);
    }
    return ev;
  }

  // --- Query ---

  interp.registerFunction("dom/query", (selector: string) => {
    const el = document.querySelector(selector);
    return storeHandle(el);
  });

  interp.registerFunction("dom/query-all", (selector: string) => {
    const els = document.querySelectorAll(selector);
    const ids: number[] = [];
    els.forEach((el) => {
      const id = storeHandle(el);
      if (id != null) ids.push(id);
    });
    return JSON.stringify(ids);
  });

  interp.registerFunction("dom/get-id", (id: string) => {
    const el = document.getElementById(id);
    return storeHandle(el);
  });

  // --- Create ---

  interp.registerFunction("dom/create-element", (tag: string) => {
    const el = document.createElement(tag);
    return storeHandle(el);
  });

  interp.registerFunction("dom/create-text", (content: string) => {
    const node = document.createTextNode(content);
    return storeHandle(node);
  });

  // --- Tree manipulation ---

  interp.registerFunction("dom/append-child!", (parentId: number, childId: number) => {
    const parent = getElement(parentId);
    const child = getNode(childId);
    parent.appendChild(child);
    return childId;
  });

  interp.registerFunction("dom/remove-child!", (parentId: number, childId: number) => {
    const parent = getElement(parentId);
    const child = getNode(childId);
    parent.removeChild(child);
    return childId;
  });

  interp.registerFunction("dom/remove!", (id: number) => {
    const el = getElement(id);
    el.remove();
    return null;
  });

  // --- Attributes ---

  interp.registerFunction("dom/set-attribute!", (id: number, attr: string, val: string) => {
    getElement(id).setAttribute(attr, val);
    return null;
  });

  interp.registerFunction("dom/get-attribute", (id: number, attr: string) => {
    return getElement(id).getAttribute(attr);
  });

  interp.registerFunction("dom/remove-attribute!", (id: number, attr: string) => {
    getElement(id).removeAttribute(attr);
    return null;
  });

  // --- CSS classes ---

  interp.registerFunction("dom/add-class!", (id: number, ...classes: string[]) => {
    getElement(id).classList.add(...classes);
    return null;
  });

  interp.registerFunction("dom/remove-class!", (id: number, ...classes: string[]) => {
    getElement(id).classList.remove(...classes);
    return null;
  });

  interp.registerFunction("dom/toggle-class!", (id: number, cls: string) => {
    return getElement(id).classList.toggle(cls);
  });

  interp.registerFunction("dom/has-class?", (id: number, cls: string) => {
    return getElement(id).classList.contains(cls);
  });

  // --- Styles ---

  interp.registerFunction("dom/set-style!", (id: number, prop: string, val: string) => {
    const el = getElement(id) as HTMLElement;
    el.style.setProperty(prop, val);
    return null;
  });

  interp.registerFunction("dom/get-style", (id: number, prop: string) => {
    const el = getElement(id) as HTMLElement;
    return el.style.getPropertyValue(prop);
  });

  // --- Content ---

  interp.registerFunction("dom/set-text!", (id: number, text: string) => {
    getElement(id).textContent = text;
    return null;
  });

  interp.registerFunction("dom/get-text", (id: number) => {
    return getElement(id).textContent;
  });

  interp.registerFunction("dom/set-html!", (id: number, html: string) => {
    getElement(id).innerHTML = html;
    return null;
  });

  interp.registerFunction("dom/get-html", (id: number) => {
    return getElement(id).innerHTML;
  });

  // --- Form values ---

  interp.registerFunction("dom/set-value!", (id: number, val: string) => {
    const el = getElement(id) as HTMLInputElement;
    el.value = val;
    return null;
  });

  interp.registerFunction("dom/get-value", (id: number) => {
    const el = getElement(id) as HTMLInputElement;
    return el.value;
  });

  // --- Events ---
  // Event listeners are stored so they can be removed with dom/off!.
  const listeners = new Map<string, EventListener>();

  interp.registerFunction("dom/on!", (id: number, event: string, callbackName: string) => {
    const el = getElement(id);
    const key = `${id}:${event}:${callbackName}`;

    const listener = (ev: Event) => {
      const evHandle = storeHandle(ev);
      // Call the Sema callback by evaluating it
      try {
        interp.registerFunction("__dom-current-event", () => evHandle);
        // The callback is a Sema function name — invoke it with the event handle
        (interp as any).evalStr(`(${callbackName} (__dom-current-event))`);
      } catch (_e) {
        // Silently ignore eval errors in event handlers to avoid breaking the page
      }
    };

    listeners.set(key, listener);
    el.addEventListener(event, listener);
    return null;
  });

  interp.registerFunction("dom/off!", (id: number, event: string, callbackName: string) => {
    const key = `${id}:${event}:${callbackName}`;
    const listener = listeners.get(key);
    if (listener) {
      getElement(id).removeEventListener(event, listener);
      listeners.delete(key);
    }
    return null;
  });

  interp.registerFunction("dom/prevent-default!", (evId: number) => {
    getEvent(evId).preventDefault();
    return null;
  });
}
