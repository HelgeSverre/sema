import { beforeEach, describe, expect, it } from "vitest";
import { registerCssBindings } from "../src/css.js";
import { SemaWebContext, disposeContextResources } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

describe("registerCssBindings", () => {
  let interp: ReturnType<typeof createMockInterpreter>;
  let ctx: SemaWebContext;

  beforeEach(() => {
    document.head.querySelectorAll("style[data-sema-css]").forEach((el) => el.remove());
    interp = createMockInterpreter();
    ctx = new SemaWebContext();
    registerCssBindings(interp, ctx);
  });

  it("creates a per-instance style element and injects scoped rules", () => {
    const className = interp.getFunction("css/scoped")!({
      backgroundColor: "red",
      "&:hover": { color: "white" },
    });

    expect(className).toMatch(/^sema-[a-z0-9]+-1$/);
    expect(ctx.styleEl).not.toBeNull();
    const rules = Array.from(ctx.styleEl?.sheet?.cssRules ?? []).map((rule) => rule.cssText);
    expect(rules.some((rule) => rule.includes(`.${className}`))).toBe(true);
    expect(rules.some((rule) => rule.includes(`.${className}:hover`))).toBe(true);
  });

  it("removes instance-owned styles during context disposal", () => {
    interp.getFunction("css/scoped")!({ color: "blue" });

    const styleEl = ctx.styleEl;
    expect(styleEl).not.toBeNull();

    disposeContextResources(ctx);

    expect(ctx.styleEl).toBeNull();
    expect(styleEl?.isConnected).toBe(false);
  });
});
