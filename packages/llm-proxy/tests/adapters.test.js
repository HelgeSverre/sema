import assert from "node:assert/strict";
import { afterEach, test } from "node:test";
import { createVercelHandler } from "../dist/adapters/vercel.js";
import { createCloudflareHandler } from "../dist/adapters/cloudflare.js";
import { createNetlifyHandler } from "../dist/adapters/netlify.js";

const originalFetch = globalThis.fetch;

afterEach(() => {
  globalThis.fetch = originalFetch;
});

function mockStreamingFetch(chunks) {
  globalThis.fetch = async () => new Response(
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
    },
  );
}

function makeStreamRequest(url) {
  return new Request(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      messages: [{ role: "user", content: "hello" }],
    }),
  });
}

test("Vercel adapter preserves streaming response bodies", async () => {
  mockStreamingFetch(["data: first\n\n", "data: second\n\n"]);
  const handler = createVercelHandler({ provider: "openai", apiKey: "test-key" });

  const response = await handler.POST(makeStreamRequest("https://example.com/api/llm/stream"));

  assert.equal(response.status, 200);
  assert.equal(response.headers.get("content-type"), "text/event-stream");
  assert.equal(
    await response.text(),
    'data: {"type":"token","text":"first"}\n\n'
      + 'data: {"type":"token","text":"second"}\n\n'
      + 'data: {"type":"done"}\n\n',
  );
});

test("Cloudflare adapter preserves streaming response bodies", async () => {
  mockStreamingFetch(["data: cloud\n\n"]);
  const worker = createCloudflareHandler({ provider: "openai", apiKey: "test-key" });

  const response = await worker.fetch(makeStreamRequest("https://example.com/api/llm/stream"));

  assert.equal(response.status, 200);
  assert.equal(response.headers.get("content-type"), "text/event-stream");
  assert.equal(
    await response.text(),
    'data: {"type":"token","text":"cloud"}\n\n'
      + 'data: {"type":"done"}\n\n',
  );
});

test("Netlify adapter preserves streaming response bodies", async () => {
  mockStreamingFetch(["data: netlify\n\n"]);
  const handler = createNetlifyHandler({ provider: "openai", apiKey: "test-key" });

  const response = await handler(makeStreamRequest("https://example.com/api/llm/stream"));

  assert.equal(response.status, 200);
  assert.equal(response.headers.get("content-type"), "text/event-stream");
  assert.equal(
    await response.text(),
    'data: {"type":"token","text":"netlify"}\n\n'
      + 'data: {"type":"done"}\n\n',
  );
});

test("Vercel adapter returns a structured 400 for invalid JSON", async () => {
  const handler = createVercelHandler({ provider: "openai", apiKey: "test-key" });
  const response = await handler.POST(new Request("https://example.com/api/llm/chat", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: "{",
  }));

  assert.equal(response.status, 400);
  assert.deepEqual(await response.json(), {
    error: "Invalid JSON body",
    code: "INVALID_REQUEST",
  });
});
