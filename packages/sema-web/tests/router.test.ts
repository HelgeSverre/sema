import { beforeEach, describe, expect, it } from "vitest";
import { registerRouterBindings } from "../src/router.js";
import { SemaWebContext } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

describe("registerRouterBindings", () => {
  let interp: ReturnType<typeof createMockInterpreter>;
  let ctx: SemaWebContext;

  beforeEach(() => {
    interp = createMockInterpreter();
    ctx = new SemaWebContext();
    window.location.hash = "#/";
    registerRouterBindings(interp, ctx);
  });

  it("matches literal route patterns with regex metacharacters", () => {
    interp.getFunction("router/init!")!({
      "/notes/v1.0+draft?": "notes-page",
    });

    interp.getFunction("router/replace!")!("/notes/v1.0+draft?");

    const routeSignalId = interp.getFunction("router/current")!();
    const routeSignal = ctx.signals.get(routeSignalId);
    expect(routeSignal?.value).toEqual({
      path: "/notes/v1.0+draft?",
      params: {},
      handler: "notes-page",
    });
  });

  it("decodes route params from the URL hash", () => {
    interp.getFunction("router/init!")!({
      "/todos/:id": "todo-detail",
    });

    interp.getFunction("router/replace!")!("/todos/hello%20world%2F42");

    const routeSignalId = interp.getFunction("router/current")!();
    const routeSignal = ctx.signals.get(routeSignalId);
    expect(routeSignal?.value).toEqual({
      path: "/todos/hello%20world%2F42",
      params: { id: "hello world/42" },
      handler: "todo-detail",
    });
  });
});
