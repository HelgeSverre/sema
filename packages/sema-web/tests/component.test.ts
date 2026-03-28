import { describe, it, expect, vi, beforeEach } from "vitest";
import { SemaWebContext } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

// Mock morphdom — it doesn't work in jsdom
vi.mock("morphdom", () => ({
  default: vi.fn((fromNode: Element, toNode: Element, opts?: any) => {
    // Simple replacement: copy children from toNode to fromNode
    if (opts?.childrenOnly) {
      fromNode.innerHTML = toNode.innerHTML;
    }
    return fromNode;
  }),
}));

// Mock @preact/signals-core — effect() runs synchronously in tests
vi.mock("@preact/signals-core", () => ({
  signal: (val: any) => ({ value: val, peek: () => val }),
  effect: (fn: () => void) => {
    fn();
    return () => {};
  },
  batch: (fn: () => void) => fn(),
}));

// Import after mocks are set up
const { registerComponentBindings } = await import("../src/component.js");

describe("registerComponentBindings", () => {
  let interp: ReturnType<typeof createMockInterpreter>;
  let ctx: SemaWebContext;

  beforeEach(() => {
    interp = createMockInterpreter();
    ctx = new SemaWebContext();
    document.body.innerHTML = '<div id="app"></div>';
    registerComponentBindings(interp, ctx);
  });

  // --- Registration ---

  it("registers component/mount! function", () => {
    expect(interp.getFunction("component/mount!")).toBeDefined();
  });

  it("registers component/unmount! function", () => {
    expect(interp.getFunction("component/unmount!")).toBeDefined();
  });

  it("registers __component/current-id function", () => {
    expect(interp.getFunction("__component/current-id")).toBeDefined();
  });

  // --- mount ---

  it("component/mount! with valid selector registers in ctx.mountedComponents", () => {
    // Make evalStr return SIP data when the component function is called
    interp.evalStr = (code: string) => {
      // The capture function call: (__cc_N (my-view))
      const capMatch = code.match(/^\((__cc_\d+)\s/);
      if (capMatch) {
        const capFn = interp.getFunction(capMatch[1]);
        if (capFn) capFn([":div", "hello"]);
      }
      return { value: null, output: [], error: null };
    };

    interp.getFunction("component/mount!")!("#app", "my-view");
    expect(ctx.mountedComponents.has("#app")).toBe(true);
    expect(ctx.mountedComponents.get("#app")!.componentFn).toBe("my-view");
  });

  // --- unmount ---

  it("component/unmount! removes from ctx.mountedComponents and clears target", () => {
    // Mount first
    interp.evalStr = (code: string) => {
      const capMatch = code.match(/^\((__cc_\d+)\s/);
      if (capMatch) {
        const capFn = interp.getFunction(capMatch[1]);
        if (capFn) capFn([":div", "hello"]);
      }
      return { value: null, output: [], error: null };
    };

    interp.getFunction("component/mount!")!("#app", "my-view");
    expect(ctx.mountedComponents.has("#app")).toBe(true);

    interp.getFunction("component/unmount!")!("#app");
    expect(ctx.mountedComponents.has("#app")).toBe(false);
    expect(document.getElementById("app")!.innerHTML).toBe("");
  });

  it("component/unmount! on non-existent selector is no-op", () => {
    expect(() => {
      interp.getFunction("component/unmount!")!("#nonexistent");
    }).not.toThrow();
  });

  // --- __component/current-id ---

  it("__component/current-id returns null when no component is rendering", () => {
    const result = interp.getFunction("__component/current-id")!();
    expect(result).toBeNull();
  });

  // --- defcomponent macro registration ---

  it("evalStr was called with defcomponent macro definition", () => {
    const calls = interp.getEvalCalls();
    const hasDef = calls.some((c: string) => c.includes("defcomponent"));
    expect(hasDef).toBe(true);
  });

  // --- mount! Sema wrapper registration ---

  it("evalStr was called with mount! wrapper definition", () => {
    const calls = interp.getEvalCalls();
    const hasMount = calls.some((c: string) => c.includes("defmacro mount!"));
    expect(hasMount).toBe(true);
  });
});
