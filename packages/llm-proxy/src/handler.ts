/**
 * Core LLM proxy handler — platform-agnostic request processing.
 *
 * This module implements the proxy protocol defined by `@sema-lang/sema-web`.
 * Each adapter (Vercel, Netlify, Cloudflare, Node) converts platform-specific
 * request/response objects to `ProxyRequest`/`ProxyResponse` and delegates here.
 *
 * @module
 */

import type {
  ProxyConfig,
  ProxyRequest,
  ProxyResponse,
  ChatRequest,
  CompleteRequest,
  ExtractRequest,
  ClassifyRequest,
  EmbedRequest,
  ProviderConfig,
  ProxyErrorResponse,
  RateLimitConfig,
} from "./types.js";
import {
  resolveProvider,
  getSpec,
  geminiChatUrl,
  geminiEmbedUrl,
  type ResolvedProvider,
} from "./providers.js";

/** Handler function type — takes a ProxyRequest, returns a ProxyResponse. */
export type HandlerFn = (req: ProxyRequest) => Promise<ProxyResponse>;

// --- Rate Limiter ---

/** Sliding-window in-memory rate limiter. */
class RateLimiter {
  private windows = new Map<string, number[]>();
  private windowMs: number;
  private maxRequests: number;

  constructor(config?: RateLimitConfig) {
    this.windowMs = config?.windowMs ?? 60_000;
    this.maxRequests = config?.maxRequests ?? 60;
  }

  check(key: string): ProxyErrorResponse | null {
    const now = Date.now();
    const timestamps = this.windows.get(key) ?? [];

    // Remove expired entries
    const valid = timestamps.filter(t => now - t < this.windowMs);

    if (valid.length >= this.maxRequests) {
      return {
        error: "Rate limit exceeded",
        code: "RATE_LIMITED",
        details: `Max ${this.maxRequests} requests per ${this.windowMs}ms`,
      };
    }

    valid.push(now);
    this.windows.set(key, valid);

    // Periodic cleanup of stale keys
    if (this.windows.size > 10_000) {
      for (const [k, v] of this.windows) {
        if (v.every(t => now - t >= this.windowMs)) this.windows.delete(k);
      }
    }

    return null;
  }
}

// --- Body size check ---

/** Check request body against maxBodySize. Returns error response or null. */
function checkBodySize(body: string | object, config: ProxyConfig): ProxyErrorResponse | null {
  const maxSize = config.maxBodySize || 1_048_576; // 1MB default
  const size = typeof body === "string" ? body.length : JSON.stringify(body).length;
  if (size > maxSize) {
    return {
      error: `Request body too large (${size} bytes, max ${maxSize})`,
      code: "BODY_TOO_LARGE",
    };
  }
  return null;
}

/**
 * Create a platform-agnostic handler function from a proxy config.
 *
 * The returned function accepts `ProxyRequest` and returns `ProxyResponse`.
 * Adapters wrap this to convert platform-specific types.
 */
export function createHandler(config: ProxyConfig): HandlerFn {
  const resolved = resolveProvider(
    config.provider,
    config.apiKey,
    config.baseUrl,
    config.defaultModel,
  );
  const corsOrigin = config.cors ?? "*";
  const rateLimiter = new RateLimiter(config.rateLimit);

  return async (req: ProxyRequest): Promise<ProxyResponse> => {
    // CORS preflight
    if (req.method === "OPTIONS") {
      return corsResponse(corsOrigin);
    }

    // Auth check
    const authResult = await checkAuth(config.auth, req.authHeader);
    if (authResult) {
      return { ...authResult, headers: { ...authResult.headers, ...corsHeaders(corsOrigin) } };
    }

    // Rate limit check
    const rateLimitKey = req.authHeader || "anonymous";
    const rateLimitError = rateLimiter.check(rateLimitKey);
    if (rateLimitError) {
      return jsonResponse(429, rateLimitError, corsOrigin);
    }

    // Body size check
    if (req.body != null) {
      const bodySizeError = checkBodySize(req.body as string | object, config);
      if (bodySizeError) {
        return jsonResponse(413, bodySizeError, corsOrigin);
      }
    }

    try {
      let result: unknown;

      switch (req.endpoint) {
        case "chat":
          result = await handleChat(resolved, req.body as ChatRequest);
          break;
        case "complete":
          result = await handleComplete(resolved, req.body as CompleteRequest);
          break;
        case "extract":
          result = await handleExtract(resolved, req.body as ExtractRequest);
          break;
        case "classify":
          result = await handleClassify(resolved, req.body as ClassifyRequest);
          break;
        case "embed":
          result = await handleEmbed(resolved, req.body as EmbedRequest);
          break;
        case "models":
          result = await handleModels(resolved);
          break;
        case "stream":
          return await handleStream(resolved, req.body as ChatRequest, corsOrigin);
        default:
          return jsonResponse(404, {
            error: `Unknown endpoint: ${req.endpoint}`,
            code: "INVALID_REQUEST" as const,
          }, corsOrigin);
      }

      return jsonResponse(200, result, corsOrigin);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      return jsonResponse(502, {
        error: message,
        code: "PROVIDER_ERROR" as const,
      }, corsOrigin);
    }
  };
}

// --- Endpoint handlers ---

async function handleChat(
  provider: ResolvedProvider,
  body: ChatRequest,
): Promise<unknown> {
  const spec = getSpec(provider.provider);
  const model = body.model ?? provider.defaultModel;
  const maxTokens = body["max-tokens"];

  const reqBody = spec.formatChatBody(
    body.messages,
    model,
    maxTokens,
    body.temperature,
    body.system,
  );

  const url =
    provider.provider === "gemini"
      ? geminiChatUrl(provider.baseUrl, model, provider.apiKey)
      : spec.chatUrl(provider.baseUrl);

  const data = await llmFetch(url, reqBody, spec.authHeader(provider.apiKey));
  return spec.parseChatResponse(data);
}

async function handleComplete(
  provider: ResolvedProvider,
  body: CompleteRequest,
): Promise<unknown> {
  // Complete is implemented as a single-message chat
  return handleChat(provider, {
    messages: [{ role: "user", content: body.prompt }],
    model: body.model,
    "max-tokens": body["max-tokens"],
    temperature: body.temperature,
    system: body.system,
  });
}

async function handleExtract(
  provider: ResolvedProvider,
  body: ExtractRequest,
): Promise<unknown> {
  const schemaStr = JSON.stringify(body.schema, null, 2);
  const systemPrompt = [
    "Extract structured data from the text below.",
    "Return ONLY a JSON object matching this schema — no extra text:",
    schemaStr,
  ].join("\n");

  const response = await handleChat(provider, {
    messages: [{ role: "user", content: body.text }],
    model: body.model,
    "max-tokens": body["max-tokens"] ?? 1024,
    system: systemPrompt,
  });

  // Try to parse the content as JSON
  const content =
    typeof response === "object" && response !== null && "content" in response
      ? (response as { content: string }).content
      : String(response);

  try {
    return JSON.parse(extractJsonBlock(content));
  } catch {
    return { content, _raw: true };
  }
}

async function handleClassify(
  provider: ResolvedProvider,
  body: ClassifyRequest,
): Promise<unknown> {
  const cats = body.categories.map((c) => `"${c}"`).join(", ");
  const systemPrompt = [
    `Classify the text into exactly ONE of these categories: ${cats}`,
    "Respond with ONLY the category name — no explanation, no quotes.",
  ].join("\n");

  const response = await handleChat(provider, {
    messages: [{ role: "user", content: body.text }],
    model: body.model,
    "max-tokens": 50,
    system: systemPrompt,
  });

  const content =
    typeof response === "object" && response !== null && "content" in response
      ? (response as { content: string }).content.trim()
      : String(response).trim();

  return { category: content };
}

async function handleEmbed(
  provider: ResolvedProvider,
  body: EmbedRequest,
): Promise<unknown> {
  const spec = getSpec(provider.provider);
  const model = body.model ?? spec.embedModel ?? provider.defaultModel;

  const url =
    provider.provider === "gemini"
      ? geminiEmbedUrl(provider.baseUrl, model, provider.apiKey)
      : spec.embedUrl(provider.baseUrl);

  const reqBody = spec.formatEmbedBody(body.text, model);
  const data = await llmFetch(url, reqBody, spec.authHeader(provider.apiKey));
  return spec.parseEmbedResponse(data);
}

async function handleModels(provider: ResolvedProvider): Promise<unknown> {
  const spec = getSpec(provider.provider);

  // For Gemini, model listing uses a different URL format
  if (provider.provider === "gemini") {
    const url = `${provider.baseUrl}/models?key=${provider.apiKey}`;
    const data = await llmFetch(url, null, {});
    const models = data.models as Array<{ name: string }>;
    return {
      models: models?.map((m) => m.name.replace("models/", "")) ?? [],
    };
  }

  // For Ollama and OpenAI-compatible providers
  const url = `${provider.baseUrl}/models`;
  try {
    const data = await llmFetch(url, null, spec.authHeader(provider.apiKey));
    const items = data.data as Array<{ id: string }>;
    return {
      models: items?.map((m) => m.id) ?? [],
    };
  } catch {
    // Fallback: return the default model
    return { models: [provider.defaultModel] };
  }
}

// --- SSE Streaming ---

/** Handle a streaming chat request, returning an SSE response. */
async function handleStream(
  provider: ResolvedProvider,
  body: ChatRequest,
  corsOrigin: string,
): Promise<ProxyResponse> {
  const spec = getSpec(provider.provider);
  const model = body.model ?? provider.defaultModel;
  const messages = body.messages || [{ role: "user", content: "" }];

  const chatBody = spec.formatChatBody(
    messages,
    model,
    body["max-tokens"],
    body.temperature,
    body.system,
  );
  (chatBody as Record<string, unknown>).stream = true;

  const url =
    provider.provider === "gemini"
      ? geminiChatUrl(provider.baseUrl, model, provider.apiKey)
      : spec.chatUrl(provider.baseUrl);

  const headers = { ...spec.authHeader(provider.apiKey), "Content-Type": "application/json" };

  const response = await fetch(url, {
    method: "POST",
    headers,
    body: JSON.stringify(chatBody),
  });

  if (!response.ok || !response.body) {
    const text = await response.text().catch(() => response.statusText);
    const errorBody: ProxyErrorResponse = {
      error: `Provider streaming error (${response.status}): ${text}`,
      code: "PROVIDER_ERROR",
    };
    return jsonResponse(502, errorBody, corsOrigin);
  }

  // Collect the streamed SSE data and return as a single SSE body.
  // Note: ProxyResponse uses a string body, so we buffer the stream.
  // Platform adapters can override this for true streaming if needed.
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";
  let sseOutput = "";

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      buffer += decoder.decode(value, { stream: true });

      const lines = buffer.split("\n");
      buffer = lines.pop() || "";

      for (const line of lines) {
        if (line.startsWith("data: ")) {
          const data = line.slice(6).trim();
          if (data === "[DONE]") {
            sseOutput += "data: [DONE]\n\n";
            continue;
          }
          // Forward the SSE event
          sseOutput += `data: ${data}\n\n`;
        }
      }
    }
  } finally {
    reader.releaseLock();
  }

  return {
    status: 200,
    headers: {
      "Content-Type": "text/event-stream",
      "Cache-Control": "no-cache",
      "Connection": "keep-alive",
      ...corsHeaders(corsOrigin),
    },
    body: sseOutput,
  };
}

// --- Utilities ---

/** Make an HTTP request to an LLM provider API. */
async function llmFetch(
  url: string,
  body: unknown,
  headers: Record<string, string>,
): Promise<Record<string, unknown>> {
  const isGet = body === null;
  const reqHeaders: Record<string, string> = {
    ...headers,
  };

  if (!isGet) {
    reqHeaders["Content-Type"] = "application/json";
  }

  const resp = await fetch(url, {
    method: isGet ? "GET" : "POST",
    headers: reqHeaders,
    body: isGet ? undefined : JSON.stringify(body),
  });

  if (!resp.ok) {
    const text = await resp.text().catch(() => resp.statusText);
    throw new Error(`LLM API error (${resp.status}): ${text}`);
  }

  return (await resp.json()) as Record<string, unknown>;
}

/** Extract a JSON block from a string (handles ```json ... ``` wrapping). */
function extractJsonBlock(s: string): string {
  // Strip markdown code blocks
  const match = s.match(/```(?:json)?\s*\n?([\s\S]*?)\n?\s*```/);
  if (match) return match[1].trim();
  // Try to find a raw JSON object/array
  const idx = s.indexOf("{");
  if (idx >= 0) return s.slice(idx);
  return s;
}

/** Check authorization. Returns an error response if unauthorized, null if OK. */
async function checkAuth(
  auth: ProxyConfig["auth"],
  authHeader: string | null,
): Promise<ProxyResponse | null> {
  if (!auth) return null;

  if (auth.verify) {
    const ok = await auth.verify(authHeader);
    if (!ok) {
      return jsonResponse(401, { error: "Unauthorized", code: "AUTH_FAILED" as const }, "*");
    }
    return null;
  }

  if (auth.token) {
    const expected = `Bearer ${auth.token}`;
    if (authHeader !== expected) {
      return jsonResponse(401, { error: "Unauthorized", code: "AUTH_FAILED" as const }, "*");
    }
  }

  return null;
}

/** Build a JSON response. */
function jsonResponse(
  status: number,
  body: unknown,
  corsOrigin: string,
): ProxyResponse {
  return {
    status,
    headers: {
      "Content-Type": "application/json",
      ...corsHeaders(corsOrigin),
    },
    body: JSON.stringify(body),
  };
}

/** CORS headers. */
function corsHeaders(origin: string): Record<string, string> {
  return {
    "Access-Control-Allow-Origin": origin,
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
  };
}

/** CORS preflight response. */
function corsResponse(origin: string): ProxyResponse {
  return {
    status: 204,
    headers: corsHeaders(origin),
    body: "",
  };
}
