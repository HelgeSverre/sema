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
import { extractClientIdFromRequestHeaders } from "../client-id.js";
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
  const corsOrigin = config.cors ?? "*";

  return {
    fetch: async (req: Request): Promise<Response> => {
      const url = new URL(req.url);
      const endpoint = extractEndpoint(url.pathname);
      let body: unknown = null;

      if (req.method === "POST") {
        try {
          body = await req.json();
        } catch {
          return new Response(
            JSON.stringify({ error: "Invalid JSON body", code: "INVALID_REQUEST" }),
            {
              status: 400,
              headers: {
                "Content-Type": "application/json",
                "Access-Control-Allow-Origin": corsOrigin,
                "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
                "Access-Control-Allow-Headers": "Content-Type, Authorization",
              },
            },
          );
        }
      }

      const proxyReq: ProxyRequest = {
        method: req.method,
        endpoint,
        body,
        authHeader: req.headers.get("authorization"),
        clientId: extractClientIdFromRequestHeaders(req.headers),
      };

      const proxyRes = await handler(proxyReq);
      const responseBody = proxyRes.stream ?? (proxyRes.body || null);

      return new Response(responseBody, {
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
