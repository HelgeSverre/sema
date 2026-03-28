import { describe, it, expect } from "vitest";
import { signal, effect } from "@preact/signals-core";
import { registerReactiveBindings } from "../src/reactive.js";
import { SemaWebContext } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

function setup() {
  const interp = createMockInterpreter();
  const ctx = new SemaWebContext();
  registerReactiveBindings(interp, ctx);
  return { interp, ctx };
}

describe("registerReactiveBindings", () => {
  it("registers __state/create, __state/deref, __state/put! functions", () => {
    const { interp } = setup();

    expect(interp.getFunction("__state/create")).toBeDefined();
    expect(interp.getFunction("__state/deref")).toBeDefined();
    expect(interp.getFunction("__state/put!")).toBeDefined();
  });

  it("__state/create returns numeric ID, __state/deref reads value", () => {
    const { interp } = setup();

    const create = interp.getFunction("__state/create")!;
    const deref = interp.getFunction("__state/deref")!;

    const id = create(42);
    expect(typeof id).toBe("number");
    expect(id).toBe(1);
    expect(deref(id)).toBe(42);
  });

  it("__state/put! changes value", () => {
    const { interp } = setup();

    const create = interp.getFunction("__state/create")!;
    const deref = interp.getFunction("__state/deref")!;
    const put = interp.getFunction("__state/put!")!;

    const id = create(42);
    put(id, 99);
    expect(deref(id)).toBe(99);
  });

  it("__state/deref throws on unknown ID", () => {
    const { interp } = setup();
    const deref = interp.getFunction("__state/deref")!;

    expect(() => deref(999)).toThrow("Unknown state");
  });

  it("__state/put! throws on unknown ID", () => {
    const { interp } = setup();
    const put = interp.getFunction("__state/put!")!;

    expect(() => put(999, 1)).toThrow("Unknown state");
  });

  it("multiple signals are independent", () => {
    const { interp } = setup();

    const create = interp.getFunction("__state/create")!;
    const deref = interp.getFunction("__state/deref")!;
    const put = interp.getFunction("__state/put!")!;

    const id1 = create("hello");
    const id2 = create("world");

    expect(id1).not.toBe(id2);
    expect(deref(id1)).toBe("hello");
    expect(deref(id2)).toBe("world");

    put(id1, "changed");
    expect(deref(id1)).toBe("changed");
    expect(deref(id2)).toBe("world");
  });

  it("__state/watch calls back when value changes", () => {
    const { interp, ctx } = setup();

    const create = interp.getFunction("__state/create")!;
    const put = interp.getFunction("__state/put!")!;
    const watch = interp.getFunction("__state/watch")!;

    const id = create(10);
    const callsBefore = interp.getEvalCalls().length;

    watch(id, "my-callback");

    // Change the value to trigger the watch effect
    put(id, 20);

    // The watch should have called evalStr with the callback
    const callsAfter = interp.getEvalCalls();
    const watchCalls = callsAfter.slice(callsBefore).filter(
      (c) => c.includes("my-callback")
    );
    expect(watchCalls.length).toBeGreaterThan(0);
  });

  it("__state/batch-run executes the thunk via evalStr", () => {
    const { interp } = setup();

    const batchRun = interp.getFunction("__state/batch-run")!;
    const callsBefore = interp.getEvalCalls().length;

    batchRun("my_thunk");

    const callsAfter = interp.getEvalCalls();
    const batchCalls = callsAfter.slice(callsBefore).filter(
      (c) => c.includes("my_thunk")
    );
    expect(batchCalls.length).toBeGreaterThan(0);
  });

  it("__state/computed-create creates a computed signal via evalStr", () => {
    const { interp, ctx } = setup();

    const computedCreate = interp.getFunction("__state/computed-create")!;
    const deref = interp.getFunction("__state/deref")!;
    const callsBefore = interp.getEvalCalls().length;

    const id = computedCreate("my_thunk");
    expect(typeof id).toBe("number");

    // Computed signals are lazy -- reading the value triggers evaluation
    deref(id);

    const callsAfter = interp.getEvalCalls();
    const computedCalls = callsAfter.slice(callsBefore).filter(
      (c) => c.includes("my_thunk")
    );
    expect(computedCalls.length).toBeGreaterThan(0);
  });

  it("Sema wrappers are registered via evalStr", () => {
    const { interp } = setup();

    const evalCalls = interp.getEvalCalls();
    // The registration should have called evalStr with the wrapper definitions
    const wrapperCall = evalCalls.find(
      (c) => c.includes("state/deref") || c.includes("deref ref")
    );
    expect(wrapperCall).toBeDefined();

    // Check that put! and update! wrappers are included
    const putCall = evalCalls.find((c) => c.includes("put!"));
    expect(putCall).toBeDefined();
  });
});

describe("@preact/signals-core integration", () => {
  it("signal and effect work together", () => {
    const s = signal(0);
    const values: number[] = [];

    const dispose = effect(() => {
      values.push(s.value);
    });

    s.value = 1;
    s.value = 2;

    expect(values).toEqual([0, 1, 2]);
    dispose();
  });
});
