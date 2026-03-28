import { describe, it, expect, vi, beforeEach } from "vitest";
import { renderSip } from "../src/sip.js";
import { SemaWebContext } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

describe("renderSip", () => {
  let interp: ReturnType<typeof createMockInterpreter>;
  let ctx: SemaWebContext;

  beforeEach(() => {
    interp = createMockInterpreter();
    ctx = new SemaWebContext();
  });

  // --- Primitives ---

  it("null returns empty text node", () => {
    const node = renderSip(null, interp, ctx);
    expect(node).toBeInstanceOf(Text);
    expect(node.textContent).toBe("");
  });

  it("string returns text node", () => {
    const node = renderSip("hello", interp, ctx);
    expect(node).toBeInstanceOf(Text);
    expect(node.textContent).toBe("hello");
  });

  it("number returns text node", () => {
    const node = renderSip(42, interp, ctx);
    expect(node).toBeInstanceOf(Text);
    expect(node.textContent).toBe("42");
  });

  it("boolean true returns text node", () => {
    const node = renderSip(true, interp, ctx);
    expect(node).toBeInstanceOf(Text);
    expect(node.textContent).toBe("true");
  });

  // --- Elements ---

  it('[":div"] creates a div element', () => {
    const node = renderSip([":div"], interp, ctx);
    expect(node).toBeInstanceOf(HTMLDivElement);
  });

  it('[":div", {":class": "app"}] sets class attribute', () => {
    const node = renderSip([":div", { ":class": "app" }], interp, ctx) as HTMLElement;
    expect(node.tagName).toBe("DIV");
    expect(node.className).toBe("app");
  });

  it('[":div", {":class": "app"}, "Hello"] sets class and child text', () => {
    const node = renderSip([":div", { ":class": "app" }, "Hello"], interp, ctx) as HTMLElement;
    expect(node.className).toBe("app");
    expect(node.textContent).toBe("Hello");
  });

  it("nested elements render correctly", () => {
    const node = renderSip([":div", [":p", "Hello"]], interp, ctx) as HTMLElement;
    expect(node.tagName).toBe("DIV");
    const p = node.firstChild as HTMLElement;
    expect(p.tagName).toBe("P");
    expect(p.textContent).toBe("Hello");
  });

  // --- Fragment ---

  it("non-string first element produces DocumentFragment", () => {
    const node = renderSip([[":p", "a"], [":p", "b"]], interp, ctx);
    expect(node).toBeInstanceOf(DocumentFragment);
    expect(node.childNodes.length).toBe(2);
    expect((node.childNodes[0] as HTMLElement).tagName).toBe("P");
    expect((node.childNodes[0] as HTMLElement).textContent).toBe("a");
    expect((node.childNodes[1] as HTMLElement).tagName).toBe("P");
    expect((node.childNodes[1] as HTMLElement).textContent).toBe("b");
  });

  it("empty array returns empty text node", () => {
    const node = renderSip([], interp, ctx);
    expect(node).toBeInstanceOf(Text);
    expect(node.textContent).toBe("");
  });

  // --- Event handlers ---

  it("on-click sets data-sema-on-click attribute", () => {
    const node = renderSip([":button", { ":on-click": "handle-click" }], interp, ctx) as HTMLElement;
    expect(node.getAttribute("data-sema-on-click")).toBe("handle-click");
  });

  it("invalid handler name does not set data attribute and logs error", () => {
    const errorSpy = vi.spyOn(console, "error").mockImplementation(() => {});
    const node = renderSip([":button", { ":on-click": "123bad" }], interp, ctx) as HTMLElement;
    expect(node.hasAttribute("data-sema-on-click")).toBe(false);
    expect(errorSpy).toHaveBeenCalledWith(
      expect.stringContaining("Invalid event handler name"),
    );
    errorSpy.mockRestore();
  });

  // --- Style ---

  it("style as string sets style attribute", () => {
    const node = renderSip([":div", { ":style": "color: red" }], interp, ctx) as HTMLElement;
    expect(node.getAttribute("style")).toBe("color: red");
  });

  it("style as map sets inline styles", () => {
    const node = renderSip(
      [":div", { ":style": { ":color": "red", ":font-size": "14px" } }],
      interp,
      ctx,
    ) as HTMLElement;
    expect(node.style.color).toBe("red");
    expect(node.style.fontSize).toBe("14px");
  });

  // --- Boolean attributes ---

  it("disabled true sets attribute", () => {
    const node = renderSip([":button", { ":disabled": true }], interp, ctx) as HTMLElement;
    expect(node.hasAttribute("disabled")).toBe(true);
  });

  it("disabled false removes attribute", () => {
    const node = renderSip([":button", { ":disabled": false }], interp, ctx) as HTMLElement;
    expect(node.hasAttribute("disabled")).toBe(false);
  });

  // --- DOM properties ---

  it("value attribute sets DOM property", () => {
    const node = renderSip([":input", { ":value": "test" }], interp, ctx) as HTMLInputElement;
    expect(node.value).toBe("test");
  });

  it("checked attribute sets DOM property", () => {
    const node = renderSip([":input", { ":checked": true }], interp, ctx) as HTMLInputElement;
    expect(node.checked).toBe(true);
  });

  // --- Edge cases ---

  it("null child in array produces empty text node", () => {
    const node = renderSip([":div", null], interp, ctx) as HTMLElement;
    expect(node.tagName).toBe("DIV");
    expect(node.childNodes.length).toBe(1);
    expect(node.childNodes[0]).toBeInstanceOf(Text);
    expect(node.childNodes[0].textContent).toBe("");
  });

  it("deeply nested (10 levels) does not stack overflow", () => {
    let sip: any = "leaf";
    for (let i = 0; i < 10; i++) {
      sip = [":div", sip];
    }
    const node = renderSip(sip, interp, ctx) as HTMLElement;
    // Walk down 10 levels
    let cur: Node = node;
    for (let i = 0; i < 10; i++) {
      expect((cur as HTMLElement).tagName).toBe("DIV");
      cur = cur.firstChild!;
    }
    expect(cur).toBeInstanceOf(Text);
    expect(cur.textContent).toBe("leaf");
  });
});
