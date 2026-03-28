/**
 * Vercel Edge Functions adapter for the Sema LLM proxy.
 *
 * ## Usage (App Router)
 *
 * Create `app/api/llm/[...path]/route.ts`:
 *
 * ```ts
 * import { createVercelHandler } from "@sema-lang/llm-proxy/vercel";
 *
 * export const { GET, POST, OPTIONS } = createVercelHandler({
 *   provider: "openai",
 *   apiKey: process.env.OPENAI_API_KEY!,
 * });
 *
 * export const runtime = "edge";
 * ```
 *
 * Then in your frontend:
 * ```js
 * await SemaWeb.create({ llmProxy: "/api/llm" });
 * ```
 *
 * @module
 */

import { createHandler } from "../handler.js";
import type { ProxyConfig, ProxyRequest } from "../types.js";

/** Route handler functions returned by createVercelHandler. */
export interface VercelHandlers {
  GET: (req: Request) => Promise<Response>;
  POST: (req: Request) => Promise<Response>;
  OPTIONS: (req: Request) => Promise<Response>;
}

/**
 * Create Vercel App Router route handlers for the LLM proxy.
 *
 * Returns `{ GET, POST, OPTIONS }` that can be directly exported
 * from a `route.ts` file.
 */
export function createVercelHandler(config: ProxyConfig): VercelHandlers {
  const handler = createHandler(config);

  async function handle(req: Request): Promise<Response> {
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
  }

  return {
    GET: handle,
    POST: handle,
    OPTIONS: handle,
  };
}

/**
 * Extract the LLM endpoint from a Vercel catch-all route path.
 * e.g. "/api/llm/chat" → "chat", "/api/llm/models" → "models"
 */
function extractEndpoint(pathname: string): string {
  // The last path segment is the endpoint
  const segments = pathname.split("/").filter(Boolean);
  return segments[segments.length - 1] ?? "";
}

// Re-export config types for convenience
export type { ProxyConfig } from "../types.js";
