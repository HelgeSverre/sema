import { describe, it, expect, vi, beforeEach } from "vitest";
import { registerDomBindings } from "../src/dom.js";
import { SemaWebContext } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

describe("registerDomBindings", () => {
  let interp: ReturnType<typeof createMockInterpreter>;
  let ctx: SemaWebContext;

  beforeEach(() => {
    interp = createMockInterpreter();
    ctx = new SemaWebContext();
    registerDomBindings(interp, ctx);
    document.body.innerHTML = "";
  });

  // --- Registration ---

  it("registers all expected dom/* functions", () => {
    const expected = [
      "dom/query", "dom/query-all", "dom/get-id",
      "dom/create-element", "dom/create-text",
      "dom/append-child!", "dom/remove-child!", "dom/remove!",
      "dom/set-attribute!", "dom/get-attribute", "dom/remove-attribute!",
      "dom/add-class!", "dom/remove-class!", "dom/toggle-class!", "dom/has-class?",
      "dom/set-style!", "dom/get-style",
      "dom/set-text!", "dom/get-text",
      "dom/set-html!", "dom/get-html",
      "dom/set-value!", "dom/get-value",
      "dom/on!", "dom/off!", "dom/prevent-default!",
      "dom/event-value",
      "dom/render", "dom/render-into!",
    ];
    for (const name of expected) {
      expect(interp.getFunction(name), `${name} should be registered`).toBeDefined();
    }
  });

  // --- create-element ---

  it("dom/create-element returns a numeric handle", () => {
    const fn = interp.getFunction("dom/create-element")!;
    const handle = fn("div");
    expect(typeof handle).toBe("number");
  });

  // --- set-text / get-text ---

  it("dom/set-text! and dom/get-text round-trip", () => {
    const handle = interp.getFunction("dom/create-element")!("div");
    interp.getFunction("dom/set-text!")!(handle, "hello");
    const text = interp.getFunction("dom/get-text")!(handle);
    expect(text).toBe("hello");
  });

  // --- query-all returns array ---

  it("dom/query-all returns an array", () => {
    document.body.innerHTML = "<p>a</p><p>b</p>";
    const result = interp.getFunction("dom/query-all")!("p");
    expect(Array.isArray(result)).toBe(true);
    expect(result.length).toBe(2);
  });

  // --- set-attribute / get-attribute ---

  it("dom/set-attribute! and dom/get-attribute round-trip", () => {
    const handle = interp.getFunction("dom/create-element")!("div");
    interp.getFunction("dom/set-attribute!")!(handle, "class", "foo");
    const val = interp.getFunction("dom/get-attribute")!(handle, "class");
    expect(val).toBe("foo");
  });

  // --- add-class / has-class ---

  it("dom/add-class! and dom/has-class? work", () => {
    const handle = interp.getFunction("dom/create-element")!("div");
    interp.getFunction("dom/add-class!")!(handle, "bar");
    const has = interp.getFunction("dom/has-class?")!(handle, "bar");
    expect(has).toBe(true);
  });

  // --- event-value ---

  it("dom/event-value returns event.target.value", () => {
    // Manually store an event with a target that has .value
    const fakeEvent = new Event("input");
    const input = document.createElement("input");
    input.value = "typed-text";
    Object.defineProperty(fakeEvent, "target", { value: input });

    // Store directly in the handle map
    const evHandle = ctx.nextHandle++;
    ctx.handles.set(evHandle, fakeEvent as any);

    const result = interp.getFunction("dom/event-value")!(evHandle);
    expect(result).toBe("typed-text");
  });

  // --- dom/render returns handle ---

  it("dom/render returns a numeric handle", () => {
    const handle = interp.getFunction("dom/render")!([":div"]);
    expect(typeof handle).toBe("number");
  });

  // --- dom/render-into! ---

  it("dom/render-into! renders into target element", () => {
    document.body.innerHTML = '<div id="app"></div>';
    interp.getFunction("dom/render-into!")!("#app", [":p", "Hello"]);
    const app = document.getElementById("app")!;
    expect(app.innerHTML).toBe("<p>Hello</p>");
  });

  // --- dom/on! event handle auto-release ---

  it("event handle is auto-released after dom/on! handler fires", () => {
    const handle = interp.getFunction("dom/create-element")!("button");
    const el = ctx.handles.get(handle) as Element;
    document.body.appendChild(el);

    // Register handler — evalStr is called with the event handle
    interp.getFunction("dom/on!")!(handle, "click", "my-handler");

    // Track which handle IDs exist before click
    const handlesBefore = new Set(ctx.handles.keys());

    // Simulate click
    el.dispatchEvent(new Event("click"));

    // The event handle that was created during dispatch should have been deleted.
    // Any handle created after our snapshot should be gone.
    for (const id of ctx.handles.keys()) {
      if (!handlesBefore.has(id)) {
        // This handle was created during dispatch — it should have been released
        expect.unreachable("Event handle should have been released");
      }
    }
  });

  // --- dom/on! errors route through ctx.onerror ---

  it("event errors route through ctx.onerror", () => {
    const onerrorSpy = vi.fn();
    ctx.onerror = onerrorSpy;

    const handle = interp.getFunction("dom/create-element")!("button");
    const el = ctx.handles.get(handle) as Element;
    document.body.appendChild(el);

    // Make evalStr throw
    interp.evalStr = () => { throw new Error("boom"); };

    interp.getFunction("dom/on!")!(handle, "click", "bad-handler");
    el.dispatchEvent(new Event("click"));

    expect(onerrorSpy).toHaveBeenCalledWith(
      expect.any(Error),
      expect.stringContaining("event:click:bad-handler"),
    );
  });
});
