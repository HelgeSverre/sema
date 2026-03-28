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
        default:
          return jsonResponse(404, { error: `Unknown endpoint: ${req.endpoint}` }, corsOrigin);
      }

      return jsonResponse(200, result, corsOrigin);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      return jsonResponse(502, { error: message }, corsOrigin);
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
      return jsonResponse(401, { error: "Unauthorized" }, "*");
    }
    return null;
  }

  if (auth.token) {
    const expected = `Bearer ${auth.token}`;
    if (authHeader !== expected) {
      return jsonResponse(401, { error: "Unauthorized" }, "*");
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
