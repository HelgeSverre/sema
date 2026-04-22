/**
 * SIP (Sema Interface Protocol) — declarative DOM rendering for Sema.
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

import { storeHandle, SEMA_IDENT_RE } from "./handles.js";
import type { SemaWebContext } from "./context.js";

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/**
 * Render a SIP data structure to a DOM Node.
 *
 * SIP format: [tag, attrs?, ...children]
 * - tag: keyword or string (e.g., `:div` serialized as `":div"`)
 * - attrs: optional map of attributes (object with keyword keys)
 * - children: strings, numbers, booleans, or nested SIP vectors
 *
 * Special attribute handling:
 * - `on-*` attributes are event handlers (value = Sema function name string)
 * - `style` can be a string or a map of CSS properties
 * - `class` sets className
 * - `value`, `checked`, `disabled` set corresponding DOM properties
 */
export function renderSip(node: any, interp: SemaInterpreterLike, ctx: SemaWebContext): Node {
  // null/nil -> empty text
  if (node === null || node === undefined) {
    return document.createTextNode("");
  }

  // Primitives -> text node
  if (typeof node === "string" || typeof node === "number" || typeof node === "boolean") {
    return document.createTextNode(String(node));
  }

  // Array -> SIP element or fragment
  if (Array.isArray(node)) {
    if (node.length === 0) {
      return document.createTextNode("");
    }

    const [tag, ...rest] = node;

    // If first element is not a string, treat as fragment (list of elements)
    if (typeof tag !== "string") {
      const frag = document.createDocumentFragment();
      for (const child of node) {
        frag.appendChild(renderSip(child, interp, ctx));
      }
      return frag;
    }

    // Strip keyword colon prefix: ":div" -> "div"
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
      applyAttributes(el, rest[0], interp, ctx);
      childStart = 1;
    }

    // Render children
    for (let i = childStart; i < rest.length; i++) {
      el.appendChild(renderSip(rest[i], interp, ctx));
    }

    return el;
  }

  // Fallback: convert to string
  return document.createTextNode(String(node));
}

/**
 * Apply attributes from a SIP attrs map to an Element.
 *
 * Handles:
 * - `on-*` -> event listeners (value is a Sema function name)
 * - `style` -> CSS (string or property map)
 * - `class` -> className
 * - `value`, `checked`, `disabled` -> DOM properties
 * - Everything else -> setAttribute
 */
function applyAttributes(
  el: Element,
  attrs: Record<string, any>,
  interp: SemaInterpreterLike,
  ctx: SemaWebContext,
): void {
  for (let [key, value] of Object.entries(attrs)) {
    // Strip keyword colon prefix from keys
    if (key.startsWith(":")) {
      key = key.slice(1);
    }

    if (key.startsWith("on-")) {
      // Event handler: set data attribute for delegated event handling
      const eventName = key.slice(3);
      if (typeof value === "string") {
        if (!SEMA_IDENT_RE.test(value)) {
          console.error(`[sema-web] Invalid event handler name: ${value}`);
          continue;
        }
        el.setAttribute(`data-sema-on-${eventName}`, value);
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
      } else {
        el.removeAttribute("disabled");
      }
    } else {
      el.setAttribute(key, String(value));
    }
  }
}

/**
 * Register `sip/*` namespace functions.
 *
 * Functions registered:
 * - `sip/render` — render SIP data, return element handle
 * - `sip/render-into!` — render SIP into a target element (by CSS selector)
 */
export function registerSipBindings(interp: SemaInterpreterLike, ctx: SemaWebContext): void {
  // sip/render — render SIP data and return an element handle
  interp.registerFunction("sip/render", (sipData: any) => {
    const node = renderSip(sipData, interp, ctx);
    if (node instanceof Element) {
      return storeHandle(node, ctx);
    }
    // Wrap non-element nodes in a span for handle compatibility
    const wrapper = document.createElement("span");
    wrapper.appendChild(node);
    return storeHandle(wrapper, ctx);
  });

  // sip/render-into! — render SIP into a target element by CSS selector
  interp.registerFunction("sip/render-into!", (selector: string, sipData: any) => {
    const target = document.querySelector(selector);
    if (!target) throw new Error(`sip/render-into!: target not found: ${selector}`);
    target.innerHTML = "";
    const node = renderSip(sipData, interp, ctx);
    target.appendChild(node);
    return null;
  });

  // Backward-compatible aliases for the old hiccup/* names
  interp.registerFunction("hiccup/render", (sipData: any) => {
    const node = renderSip(sipData, interp, ctx);
    if (node instanceof Element) {
      return storeHandle(node, ctx);
    }
    const wrapper = document.createElement("span");
    wrapper.appendChild(node);
    return storeHandle(wrapper, ctx);
  });

  interp.registerFunction("hiccup/render-into!", (selector: string, sipData: any) => {
    const target = document.querySelector(selector);
    if (!target) throw new Error(`hiccup/render-into!: target not found: ${selector}`);
    target.innerHTML = "";
    const node = renderSip(sipData, interp, ctx);
    target.appendChild(node);
    return null;
  });
}
