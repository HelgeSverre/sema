/**
 * LLM proxy bindings for Sema — registers `llm/*` namespace functions.
 *
 * Since LLM API keys cannot be safely stored in the browser, this module
 * bridges the Sema `llm/*` API to a backend proxy server via `http/post`.
 * The proxy holds API keys and forwards requests to the actual LLM providers.
 *
 * ## Architecture
 *
 * ```
 * Browser (Sema code)          Backend proxy server
 * ┌──────────────────┐         ┌──────────────────┐
 * │ (llm/chat ...)   │──POST──▶│ /api/llm/chat    │──▶ OpenAI / Anthropic / etc.
 * │ (llm/complete ..)│──POST──▶│ /api/llm/complete │──▶ ...
 * │ (llm/embed ...)  │──POST──▶│ /api/llm/embed    │──▶ ...
 * └──────────────────┘         └──────────────────┘
 * ```
 *
 * ## Implementation
 *
 * All LLM functions are defined as pure Sema code using `http/post`,
 * `json/encode`, and `json/decode`. This piggybacks on the WASM
 * interpreter's HTTP replay mechanism: when called via `evalAsync()`,
 * `http/post` calls are intercepted, executed via browser `fetch()`,
 * and the results are replayed back transparently.
 *
 * ## Usage
 *
 * ```js
 * const web = await SemaWeb.create({
 *   llmProxy: "https://my-backend.example.com/api/llm",
 * });
 * // Must use evalAsync for HTTP-based LLM calls:
 * await web.evalAsync('(llm/chat (list (message :user "Hi")) {:model "gpt-4o"})');
 * ```
 *
 * @module
 */

interface SemaInterpreterLike {
  registerFunction(name: string, fn: (...args: any[]) => any): void;
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/** Configuration for the LLM proxy. */
export interface LlmProxyOptions {
  /**
   * Base URL of the LLM proxy server.
   * Endpoints are appended as: `{url}/complete`, `{url}/chat`, etc.
   */
  url: string;

  /**
   * Optional authorization header value.
   * Sent as `Authorization: Bearer {token}` on every request.
   * This is for authenticating the browser client to the proxy —
   * NOT the LLM API key (which should be stored server-side).
   */
  token?: string;

  /**
   * Optional custom headers to include in every proxy request.
   */
  headers?: Record<string, string>;

  /**
   * Request timeout in milliseconds.
   * Default: 60000 (60 seconds).
   */
  timeout?: number;
}

/**
 * Register all `llm/*` namespace functions that proxy to a backend server.
 *
 * Functions are defined as Sema code using `http/post` and `json/encode`/
 * `json/decode` — this ensures they work naturally with the WASM async
 * eval loop. When called via `evalAsync()`, HTTP requests are intercepted
 * by the WASM replay mechanism and executed via browser `fetch()`.
 *
 * Functions registered:
 * - `llm/complete` — simple text completion
 * - `llm/chat` — chat with messages list
 * - `llm/send` — send a prompt/messages object
 * - `llm/extract` — structured data extraction
 * - `llm/classify` — classification
 * - `llm/embed` — text embeddings
 * - `llm/list-models` — list available models from the proxy
 * - `llm/proxy-url` — return the configured proxy URL
 * - `message` — helper to build chat messages
 */
export function registerLlmBindings(
  interp: SemaInterpreterLike,
  opts: LlmProxyOptions,
): void {
  const proxyUrl = opts.url.replace(/\/+$/, "");

  // Build the headers map as a Sema literal expression
  const headerPairs: string[] = [];
  headerPairs.push(`"Content-Type" "application/json"`);
  if (opts.token) {
    headerPairs.push(`"Authorization" "Bearer ${escapeSemaString(opts.token)}"`);
  }
  if (opts.headers) {
    for (const [k, v] of Object.entries(opts.headers)) {
      headerPairs.push(`"${escapeSemaString(k)}" "${escapeSemaString(v)}"`);
    }
  }
  const headersMap = `{${headerPairs.join(" ")}}`;

  // Register a simple JS function for the proxy URL (sync, no HTTP needed)
  interp.registerFunction("llm/proxy-url", () => proxyUrl);

  // Define all LLM proxy functions as Sema code.
  // These use http/post which the WASM async loop intercepts for fetch().
  const semaCode = `
;; --- LLM proxy internals ---

(define __llm-proxy-url "${escapeSemaString(proxyUrl)}")
(define __llm-proxy-headers ${headersMap})

;; Helper: POST to the proxy and decode the JSON response body.
(define (__llm-proxy-post endpoint body-map)
  (let ((url (string-append __llm-proxy-url "/" endpoint))
        (resp (http/post url
                {:headers __llm-proxy-headers
                 :body (json/encode body-map)})))
    (if (and (map? resp) (:body resp))
      (json/decode (:body resp))
      resp)))

;; Helper: GET from the proxy.
(define (__llm-proxy-get endpoint)
  (let ((url (string-append __llm-proxy-url "/" endpoint))
        (resp (http/get url {:headers __llm-proxy-headers})))
    (if (and (map? resp) (:body resp))
      (json/decode (:body resp))
      resp)))

;; --- Public API ---

;; (message role content) — build a chat message map
(define (message role content)
  {:role (if (keyword? role) (keyword->string role) (->string role))
   :content content})

;; (llm/complete prompt) or (llm/complete prompt opts)
;; Send a simple prompt for completion.
(define (llm/complete prompt . rest)
  (let ((opts (if (null? rest) {} (car rest))))
    (let ((body (merge {:prompt prompt} (if (map? opts) opts {}))))
      (let ((result (__llm-proxy-post "complete" body)))
        (if (map? result)
          (or (:content result) (:text result) result)
          result)))))

;; (llm/chat messages) or (llm/chat messages opts)
;; Chat with a list of message maps.
(define (llm/chat messages . rest)
  (let ((opts (if (null? rest) {} (car rest))))
    (let ((body (merge {:messages messages} (if (map? opts) opts {}))))
      (let ((result (__llm-proxy-post "chat" body)))
        (if (map? result)
          (or (:content result) (:text result) result)
          result)))))

;; (llm/send prompt) or (llm/send prompt opts)
;; Send a prompt (list of messages or prompt object).
(define (llm/send prompt . rest)
  (let ((opts (if (null? rest) {} (car rest))))
    (let ((messages (if (list? prompt) prompt (list prompt))))
      (let ((body (merge {:messages messages} (if (map? opts) opts {}))))
        (let ((result (__llm-proxy-post "chat" body)))
          (if (map? result)
            (or (:content result) (:text result) result)
            result))))))

;; (llm/extract schema text) or (llm/extract schema text opts)
;; Extract structured data from text.
(define (llm/extract schema text . rest)
  (let ((opts (if (null? rest) {} (car rest))))
    (let ((body (merge {:schema schema :text text} (if (map? opts) opts {}))))
      (__llm-proxy-post "extract" body))))

;; (llm/classify categories text) or (llm/classify categories text opts)
;; Classify text into one of the given categories.
(define (llm/classify categories text . rest)
  (let ((opts (if (null? rest) {} (car rest))))
    (let ((body (merge {:categories categories :text text} (if (map? opts) opts {}))))
      (__llm-proxy-post "classify" body))))

;; (llm/embed text) or (llm/embed text opts)
;; Get text embeddings.
(define (llm/embed text . rest)
  (let ((opts (if (null? rest) {} (car rest))))
    (let ((body (merge {:text text} (if (map? opts) opts {}))))
      (__llm-proxy-post "embed" body))))

;; (llm/list-models)
;; List available models from the proxy.
(define (llm/list-models)
  (__llm-proxy-get "models"))
`;

  const result = interp.evalStr(semaCode);
  if (result.error) {
    throw new Error(`[sema-web] Failed to register LLM bindings: ${result.error}`);
  }
}

/**
 * Escape a string for safe embedding in Sema code.
 * Handles backslashes, double quotes, newlines, carriage returns, and tabs.
 */
function escapeSemaString(s: string): string {
  return s
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"')
    .replace(/\n/g, "\\n")
    .replace(/\r/g, "\\r")
    .replace(/\t/g, "\\t");
}
