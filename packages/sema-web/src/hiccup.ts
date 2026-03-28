/**
 * Hiccup-style declarative DOM rendering for Sema.
 *
 * Renders Sema vectors as DOM elements using the hiccup convention:
 *
 * ```sema
 * [:div {:class "container"}
 *   [:h1 "Hello"]
 *   [:p {:style "color: blue"} "World"]]
 * ```
 *
 * After WASM serialization, the JS side receives:
 *   [":div", {":class": "container"}, [":h1", "Hello"], ...]
 *
 * The renderer strips keyword colon prefixes and handles special
 * attributes like `on-*` (event handlers) and `style` (object or string).
 *
 * @module
 */

import { storeHandle } from "./handles.js";

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/**
 * Render a hiccup data structure to a DOM Node.
 *
 * Hiccup format: [tag, attrs?, ...children]
 * - tag: keyword or string (e.g., `:div` serialized as `":div"`)
 * - attrs: optional map of attributes (object with keyword keys)
 * - children: strings, numbers, booleans, or nested hiccup vectors
 *
 * Special attribute handling:
 * - `on-*` attributes are event handlers (value = Sema function name string)
 * - `style` can be a string or a map of CSS properties
 * - `class` sets className
 * - `value`, `checked`, `disabled` set corresponding DOM properties
 */
export function renderHiccup(node: any, interp: SemaInterpreterLike): Node {
  // null/nil → empty text
  if (node === null || node === undefined) {
    return document.createTextNode("");
  }

  // Primitives → text node
  if (typeof node === "string" || typeof node === "number" || typeof node === "boolean") {
    return document.createTextNode(String(node));
  }

  // Array → hiccup element or fragment
  if (Array.isArray(node)) {
    if (node.length === 0) {
      return document.createTextNode("");
    }

    const [tag, ...rest] = node;

    // If first element is not a string, treat as fragment (list of elements)
    if (typeof tag !== "string") {
      const frag = document.createDocumentFragment();
      for (const child of node) {
        frag.appendChild(renderHiccup(child, interp));
      }
      return frag;
    }

    // Strip keyword colon prefix: ":div" → "div"
    const tagName = tag.startsWith(":") ? tag.slice(1) : tag;
    const el = document.createElement(tagName);

    let childStart = 0;

    // Check for attributes map (second element is a plain object, not array)
    if (
      rest.length > 0 &&
      rest[0] !== null &&
      typeof rest[0] === "object" &&
      !Array.isArray(rest[0])
    ) {
      applyAttributes(el, rest[0], interp);
      childStart = 1;
    }

    // Render children
    for (let i = childStart; i < rest.length; i++) {
      el.appendChild(renderHiccup(rest[i], interp));
    }

    return el;
  }

  // Fallback: convert to string
  return document.createTextNode(String(node));
}

/**
 * Apply attributes from a hiccup attrs map to an Element.
 *
 * Handles:
 * - `on-*` → event listeners (value is a Sema function name)
 * - `style` → CSS (string or property map)
 * - `class` → className
 * - `value`, `checked`, `disabled` → DOM properties
 * - Everything else → setAttribute
 */
function applyAttributes(
  el: Element,
  attrs: Record<string, any>,
  interp: SemaInterpreterLike,
): void {
  for (let [key, value] of Object.entries(attrs)) {
    // Strip keyword colon prefix from keys
    if (key.startsWith(":")) {
      key = key.slice(1);
    }

    if (key.startsWith("on-")) {
      // Event handler: value must be a valid Sema function name
      const eventName = key.slice(3);
      if (typeof value === "string") {
        if (!/^[a-zA-Z_][a-zA-Z0-9_/\-?!*><=+.]*$/.test(value)) {
          console.error(`[sema-web] Invalid event handler name: ${value}`);
          continue;
        }
        el.addEventListener(eventName, (ev: Event) => {
          const evHandle = storeHandle(ev);
          try {
            interp.evalStr(`(${value} ${evHandle})`);
          } catch (e) {
            console.error(`[sema-web] Event handler error (${value}):`, e);
          }
        });
      }
    } else if (key === "style") {
      if (typeof value === "string") {
        (el as HTMLElement).setAttribute("style", value);
      } else if (typeof value === "object" && value !== null) {
        // Style map: {":color": "red", ":font-size": "14px"}
        for (let [prop, val] of Object.entries(value)) {
          if (prop.startsWith(":")) prop = prop.slice(1);
          (el as HTMLElement).style.setProperty(prop, String(val));
        }
      }
    } else if (key === "class") {
      el.className = String(value);
    } else if (key === "value") {
      (el as HTMLInputElement).value = String(value);
    } else if (key === "checked") {
      (el as HTMLInputElement).checked = Boolean(value);
    } else if (key === "disabled") {
      if (value) {
        el.setAttribute("disabled", "");
      }
    } else {
      el.setAttribute(key, String(value));
    }
  }
}

/**
 * Register `hiccup/*` namespace functions.
 *
 * Functions registered:
 * - `hiccup/render` — render hiccup data, return element handle
 * - `hiccup/render-into!` — render hiccup into a target element (by CSS selector)
 */
export function registerHiccupBindings(interp: SemaInterpreterLike): void {
  // hiccup/render — render hiccup data and return an element handle
  interp.registerFunction("hiccup/render", (hiccupData: any) => {
    const node = renderHiccup(hiccupData, interp);
    if (node instanceof Element) {
      return storeHandle(node);
    }
    // Wrap non-element nodes in a span for handle compatibility
    const wrapper = document.createElement("span");
    wrapper.appendChild(node);
    return storeHandle(wrapper);
  });

  // hiccup/render-into! — render hiccup into a target element by CSS selector
  interp.registerFunction("hiccup/render-into!", (selector: string, hiccupData: any) => {
    const target = document.querySelector(selector);
    if (!target) throw new Error(`hiccup/render-into!: target not found: ${selector}`);
    target.innerHTML = "";
    const node = renderHiccup(hiccupData, interp);
    target.appendChild(node);
    return null;
  });
}
