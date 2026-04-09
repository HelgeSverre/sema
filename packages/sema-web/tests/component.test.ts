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
  computed: (fn: () => any) => ({
    get value() {
      return fn();
    },
  }),
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
    document.body.innerHTML = '<div id="app"></div><div id="app2"></div>';
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
    interp.invokeGlobal = (name: string) => {
      if (name === "my-view") return [":div", "hello"];
      return null;
    };

    interp.getFunction("component/mount!")!("#app", "my-view");
    expect(ctx.mountedComponents.has("#app")).toBe(true);
    expect(ctx.mountedComponents.get("#app")!.componentFn).toBe("my-view");
  });

  it("mounting the same component function twice keeps instance-local state isolated", () => {
    interp.registerFunction("shared-view", () => {
      const currentId = interp.getFunction("__component/current-id")!();
      const localId = interp.getFunction("__component/local")!("count", 0);
      return [":div", `${currentId}:${localId}`];
    });

    interp.getFunction("component/mount!")!("#app", "shared-view");
    interp.getFunction("component/mount!")!("#app2", "shared-view");

    const first = ctx.mountedComponents.get("#app")!;
    const second = ctx.mountedComponents.get("#app2")!;

    expect(first.instanceId).not.toBe(second.instanceId);
    expect(first.localState.get("count")).not.toBe(second.localState.get("count"));
  });

  it("remounting runs mount cleanup and tears down delegated listeners", () => {
    const target = document.getElementById("app")!;
    const removeSpy = vi.spyOn(target, "removeEventListener");
    let cleanupCalls = 0;

    interp.registerFunction("mount-cleanup-fn", () => {
      cleanupCalls += 1;
      return null;
    });
    interp.registerFunction("mount-hook", () => "mount-cleanup-fn");
    interp.registerFunction("my-view", () => {
      interp.getFunction("__component/on-mount")!("mount-hook");
      return [":div", "hello"];
    });

    interp.getFunction("component/mount!")!("#app", "my-view");
    interp.getFunction("component/mount!")!("#app", "my-view");

    expect(cleanupCalls).toBe(1);
    expect(removeSpy).toHaveBeenCalled();
  });

  it("on-mount accepts direct function callbacks that return direct cleanup functions", () => {
    let mountCalls = 0;
    let cleanupCalls = 0;

    interp.registerFunction("my-view", () => {
      interp.getFunction("__component/on-mount")!(() => {
        mountCalls += 1;
        return () => {
          cleanupCalls += 1;
        };
      });
      return [":div", "hello"];
    });

    interp.getFunction("component/mount!")!("#app", "my-view");
    interp.getFunction("component/unmount!")!("#app");

    expect(mountCalls).toBe(1);
    expect(cleanupCalls).toBe(1);
  });

  // --- unmount ---

  it("component/unmount! removes from ctx.mountedComponents and clears target", () => {
    interp.invokeGlobal = (name: string) => {
      if (name === "my-view") return [":div", "hello"];
      return null;
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
