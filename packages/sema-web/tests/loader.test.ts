import { afterEach, describe, expect, it, vi } from "vitest";
import { loadScripts } from "../src/loader.js";

describe("loadScripts", () => {
  afterEach(() => {
    document.body.innerHTML = "";
    vi.restoreAllMocks();
  });

  it("loads external .sema scripts as source", async () => {
    document.body.innerHTML = '<script type="text/sema" src="/app.sema"></script>';

    const evalStrAsync = vi.fn().mockResolvedValue({
      value: "ok",
      output: [],
      error: null,
    });
    vi.stubGlobal(
      "fetch",
      vi.fn().mockResolvedValue({
        ok: true,
        text: () => Promise.resolve('(println "hello")'),
      }),
    );

    const results = await loadScripts({
      evalStr: vi.fn(),
      evalStrAsync,
    });

    expect(evalStrAsync).toHaveBeenCalledWith('(println "hello")');
    expect(results).toHaveLength(1);
    expect(results[0]?.error).toBeNull();
  });

  it("loads external .vfs scripts as compiled archives", async () => {
    document.body.innerHTML = '<script type="text/sema" src="/app.vfs"></script>';

    const archiveBytes = new Uint8Array([0, 1, 2, 3]);
    const loadArchive = vi.fn().mockReturnValue({
      ok: true,
      entryPoint: "__main__.semac",
      fileCount: 2,
      semaVersion: "1.9.0",
      buildTarget: "web",
      buildTimestamp: "0",
      error: null,
    });
    const runEntryAsync = vi.fn().mockResolvedValue({
      value: "done",
      output: [],
      error: null,
    });

    vi.stubGlobal(
      "fetch",
      vi.fn().mockResolvedValue({
        ok: true,
        arrayBuffer: () =>
          Promise.resolve(
            archiveBytes.buffer.slice(
              archiveBytes.byteOffset,
              archiveBytes.byteOffset + archiveBytes.byteLength,
            ),
          ),
      }),
    );

    const results = await loadScripts({
      evalStr: vi.fn(),
      loadArchive,
      runEntryAsync,
    });

    expect(loadArchive).toHaveBeenCalledTimes(1);
    expect(loadArchive.mock.calls[0]?.[0]).toBeInstanceOf(Uint8Array);
    expect(runEntryAsync).toHaveBeenCalledWith("__main__.semac");
    expect(results).toHaveLength(1);
    expect(results[0]?.value).toBe("done");
  });

  it("surfaces archive compatibility failures", async () => {
    document.body.innerHTML = '<script type="text/sema" src="/app.vfs"></script>';

    vi.stubGlobal(
      "fetch",
      vi.fn().mockResolvedValue({
        ok: true,
        arrayBuffer: () => Promise.resolve(new Uint8Array([1, 2, 3]).buffer),
      }),
    );

    const results = await loadScripts({
      evalStr: vi.fn(),
      loadArchive: vi.fn(() => {
        throw new Error("archive version mismatch: built with Sema 0.0.0, runtime is 1.9.0");
      }),
      runEntryAsync: vi.fn(),
    });

    expect(results[0]?.error).toContain("archive version mismatch");
  });
});
