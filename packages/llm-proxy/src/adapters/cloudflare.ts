/**
 * Cloudflare Workers adapter for the Sema LLM proxy.
 *
 * ## Usage
 *
 * Create `src/index.ts`:
 *
 * ```ts
 * import { createCloudflareHandler } from "@sema-lang/llm-proxy/cloudflare";
 *
 * export default createCloudflareHandler({
 *   provider: "openai",
 *   apiKey: "your-key", // or use env below
 * });
 * ```
 *
 * Or with environment variables:
 *
 * ```ts
 * import { createCloudflareHandler } from "@sema-lang/llm-proxy/cloudflare";
 *
 * export default {
 *   fetch: (req: Request, env: { OPENAI_API_KEY: string }) =>
 *     createCloudflareHandler({
 *       provider: "openai",
 *       apiKey: env.OPENAI_API_KEY,
 *     }).fetch(req),
 * };
 * ```
 *
 * Then in your frontend:
 * ```js
 * await SemaWeb.create({ llmProxy: "https://my-worker.workers.dev" });
 * ```
 *
 * @module
 */

import { createHandler } from "../handler.js";
import type { ProxyConfig, ProxyRequest } from "../types.js";

/** Cloudflare Workers module export format. */
export interface CloudflareWorker {
  fetch: (req: Request) => Promise<Response>;
}

/**
 * Create a Cloudflare Worker that handles LLM proxy requests.
 *
 * Returns an object with a `fetch` method compatible with Cloudflare Workers.
 */
export function createCloudflareHandler(config: ProxyConfig): CloudflareWorker {
  const handler = createHandler(config);

  return {
    fetch: async (req: Request): Promise<Response> => {
      const url = new URL(req.url);
      const endpoint = extractEndpoint(url.pathname);
      let body: unknown = null;

      if (req.method === "POST") {
        body = await req.json();
      }

      const proxyReq: ProxyRequest = {
        method: req.method,
        endpoint,
        body,
        authHeader: req.headers.get("authorization"),
      };

      const proxyRes = await handler(proxyReq);

      return new Response(proxyRes.body || null, {
        status: proxyRes.status,
        headers: proxyRes.headers,
      });
    },
  };
}

/**
 * Extract the LLM endpoint from the request path.
 * Handles both root-mounted ("/chat") and path-prefixed ("/api/llm/chat").
 */
function extractEndpoint(pathname: string): string {
  const segments = pathname.split("/").filter(Boolean);
  return segments[segments.length - 1] ?? "";
}

// Re-export config types for convenience
export type { ProxyConfig } from "../types.js";
