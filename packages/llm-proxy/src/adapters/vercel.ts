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
import { extractClientIdFromRequestHeaders } from "../client-id.js";
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
  const corsOrigin = config.cors ?? "*";

  async function handle(req: Request): Promise<Response> {
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
