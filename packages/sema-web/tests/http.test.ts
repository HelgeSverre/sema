import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { registerHttpBindings } from "../src/http.js";
import { SemaWebContext, disposeContextResources } from "../src/context.js";
import { createMockInterpreter } from "./helpers.js";

function makeSseResponse(chunks: string[], init?: ResponseInit): Response {
  return new Response(
    new ReadableStream({
      start(controller) {
        for (const chunk of chunks) {
          controller.enqueue(new TextEncoder().encode(chunk));
        }
        controller.close();
      },
    }),
    {
      status: 200,
      headers: { "Content-Type": "text/event-stream" },
      ...init,
    },
  );
}

async function flushAsyncWork(): Promise<void> {
  await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
}

describe("registerHttpBindings", () => {
  let interp: ReturnType<typeof createMockInterpreter>;
  let ctx: SemaWebContext;

  beforeEach(() => {
    interp = createMockInterpreter();
    ctx = new SemaWebContext();
    registerHttpBindings(interp, ctx);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("streams SSE over fetch with custom headers and POST bodies", async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      makeSseResponse([
        'event: token\n',
        'id: 42\n',
        'data: hello\n\n',
      ]),
    );
    vi.stubGlobal("fetch", fetchMock);

    const signalId = interp.getFunction("http/event-source")!({
      ":url": "/stream",
      ":method": "POST",
      ":headers": { ":authorization": "Bearer token" },
      ":body": "payload",
      ":with-credentials": true,
    });

    await flushAsyncWork();

    expect(fetchMock).toHaveBeenCalledWith("/stream", expect.objectContaining({
      method: "POST",
      body: "payload",
      credentials: "include",
      headers: { authorization: "Bearer token" },
    }));

    const state = ctx.signals.get(signalId)?.value;
    expect(state).toMatchObject({
      data: "hello",
      event: "token",
      id: "42",
      done: true,
      state: "closed",
      error: null,
      status: 200,
    });
  });

  it("closes managed streams on dispose", async () => {
    let aborted = false;
    vi.stubGlobal("fetch", vi.fn().mockImplementation((_url, init?: RequestInit) => {
      init?.signal?.addEventListener("abort", () => {
        aborted = true;
      });
      return Promise.resolve(new Response(
        new ReadableStream({
          start() {
            // Leave open until aborted.
          },
        }),
        {
          status: 200,
          headers: { "Content-Type": "text/event-stream" },
        },
      ));
    }));

    const signalId = interp.getFunction("http/event-source")!("/stream");
    expect(ctx.streams.has(signalId)).toBe(true);

    disposeContextResources(ctx);
    await flushAsyncWork();

    expect(aborted).toBe(true);
    expect(ctx.streams.has(signalId)).toBe(false);
  });

  it("assigns stream ownership from the current execution context", () => {
    vi.stubGlobal("fetch", vi.fn().mockResolvedValue(makeSseResponse([])));

    const component = {
      instanceId: 1,
      target: document.createElement("div"),
      componentFn: "view",
      dispose: null,
      eventCleanup: null,
      localState: new Map(),
      mountCleanup: null,
      pendingMount: null,
      ownedSignalIds: new Set<number>(),
      ownedWatchIds: new Set<number>(),
      ownedIntervalIds: new Set<number>(),
      ownedStreamIds: new Set<number>(),
      ownedListenerKeys: new Set<string>(),
    };

    ctx.mountedComponentsById.set(component.instanceId, component as any);
    ctx.ownerStack.push(component.instanceId);
    const signalId = interp.getFunction("http/event-source")!("/stream");
    ctx.ownerStack.pop();

    expect(component.ownedStreamIds.has(signalId)).toBe(true);
  });
});
