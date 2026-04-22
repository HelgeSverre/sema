/**
 * Netlify Functions adapter for the Sema LLM proxy.
 *
 * ## Usage
 *
 * Create `netlify/functions/llm.ts`:
 *
 * ```ts
 * import { createNetlifyHandler } from "@sema-lang/llm-proxy/netlify";
 *
 * export default createNetlifyHandler({
 *   provider: "openai",
 *   apiKey: process.env.OPENAI_API_KEY!,
 * });
 *
 * export const config = {
 *   path: "/api/llm/*",
 * };
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
import { buildBodyTooLargeResponse, getMaxBodySize, readRequestTextWithLimit } from "../body.js";
import type { ProxyConfig, ProxyRequest } from "../types.js";

/**
 * Netlify Functions v2 handler signature.
 * Compatible with Netlify's modern functions runtime.
 */
export type NetlifyHandler = (
  req: Request,
) => Promise<Response>;

/**
 * Create a Netlify Functions handler for the LLM proxy.
 *
 * Returns a function compatible with Netlify Functions v2 (Web API Request/Response).
 */
export function createNetlifyHandler(config: ProxyConfig): NetlifyHandler {
  const handler = createHandler(config);
  const corsOrigin = config.cors ?? "*";
  const maxBodySize = getMaxBodySize(config);

  return async (req: Request): Promise<Response> => {
    const url = new URL(req.url);
    const endpoint = extractEndpoint(url.pathname);
    let body: unknown = null;

    if (req.method === "POST") {
      try {
        const bodyResult = await readRequestTextWithLimit(req, maxBodySize);
        if (!bodyResult.ok) {
          const tooLarge = buildBodyTooLargeResponse(corsOrigin, maxBodySize, bodyResult.size);
          return new Response(tooLarge.body, {
            status: tooLarge.status,
            headers: tooLarge.headers,
          });
        }
        body = bodyResult.text ? JSON.parse(bodyResult.text) : null;
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
      clientId: extractClientIdFromRequestHeaders(req.headers, config.trustProxyHeaders),
      requestedHeaders: req.headers.get("access-control-request-headers"),
    };

    const proxyRes = await handler(proxyReq);
    const responseBody = proxyRes.stream ?? (proxyRes.body || null);

    return new Response(responseBody, {
      status: proxyRes.status,
      headers: proxyRes.headers,
    });
  };
}

/**
 * Extract the LLM endpoint from the request path.
 * e.g. "/api/llm/chat" → "chat"
 */
function extractEndpoint(pathname: string): string {
  const segments = pathname.split("/").filter(Boolean);
  return segments[segments.length - 1] ?? "";
}

// Re-export config types for convenience
export type { ProxyConfig } from "../types.js";
