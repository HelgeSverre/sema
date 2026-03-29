import { describe, it, expect, vi } from "vitest";
import { SemaWebContext } from "../src/context.js";

describe("SemaWebContext", () => {
  it("two instances have independent handles state", () => {
    const ctx1 = new SemaWebContext();
    const ctx2 = new SemaWebContext();

    ctx1.handles.set(1, document.createElement("div"));
    expect(ctx1.handles.size).toBe(1);
    expect(ctx2.handles.size).toBe(0);
  });

  it("handle IDs start at 1 per instance", () => {
    const ctx1 = new SemaWebContext();
    const ctx2 = new SemaWebContext();

    expect(ctx1.nextHandle).toBe(1);
    expect(ctx2.nextHandle).toBe(1);

    ctx1.nextHandle++;
    expect(ctx1.nextHandle).toBe(2);
    expect(ctx2.nextHandle).toBe(1);
  });

  it("signal IDs start at 1 per instance", () => {
    const ctx1 = new SemaWebContext();
    const ctx2 = new SemaWebContext();

    expect(ctx1.nextSignalId).toBe(1);
    expect(ctx2.nextSignalId).toBe(1);

    ctx1.nextSignalId++;
    expect(ctx1.nextSignalId).toBe(2);
    expect(ctx2.nextSignalId).toBe(1);
  });

  it("mountedComponents are independent", () => {
    const ctx1 = new SemaWebContext();
    const ctx2 = new SemaWebContext();

    ctx1.mountedComponents.set("app", {
      target: document.createElement("div"),
      componentFn: "render-app",
      captureId: 1,
      dispose: null,
      localState: new Map(),
      mountCleanup: null,
      renderContextStack: [],
    });

    expect(ctx1.mountedComponents.size).toBe(1);
    expect(ctx2.mountedComponents.size).toBe(0);
  });

  it("default onerror calls console.error", () => {
    const ctx = new SemaWebContext();
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});

    const error = new Error("test error");
    ctx.onerror(error, "test-context");

    expect(spy).toHaveBeenCalledOnce();
    expect(spy).toHaveBeenCalledWith("[sema-web] Error in test-context:", error);

    spy.mockRestore();
  });
});
