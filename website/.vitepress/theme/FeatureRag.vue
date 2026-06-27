<script setup>
import CustomPageLayout from './CustomPageLayout.vue'
</script>

<template>
  <CustomPageLayout active-nav="rag" v-slot="{ copyText }">

    <!-- ============ HERO ============ -->
    <header class="hero">
      <span class="hero-paren l" aria-hidden="true">(</span>
      <span class="hero-paren r" aria-hidden="true">)</span>
      <div class="wrap">
        <p class="eyebrow">Feature<span class="sep">·</span>LLM<span class="sep">·</span>RAG &amp; Vector Store</p>
        <h1>Retrieval is a <em>language feature.</em></h1>
        <p class="lede">
          Four primitives — <code>llm/embed</code>, <code>vector-store/*</code>,
          <code>llm/rerank</code>, <code>llm/complete</code> — compose into the
          full RAG pipeline. <strong>No framework. No vector database to stand
          up. No orchestration library.</strong> Just functions that fit in one
          screen.
        </p>
        <div class="hero-actions">
          <a class="btn btn-gold" href="/docs/llm/rag">Read the docs</a>
          <a class="btn btn-ghost" href="https://sema.run">Try the playground</a>
        </div>
        <div class="hero-actions">
          <span class="install">
            <span class="cmd-text">
              <span class="dollar">$</span>
              <span id="i1">sema examples/llm/rag-docs-search.sema</span>
            </span>
            <button class="copy" @click="copyText('i1', $event)">copy</button>
          </span>
        </div>
        <p class="req">embed → retrieve → rerank → answer · no external DB · disk-persisted</p>
      </div>
    </header>

    <!-- ============ PIPELINE FUNNEL ============ -->
    <section class="pipeline-showcase">
      <div class="wrap">
        <p class="kicker">The pipeline</p>
        <h2>Embed. Retrieve. Rerank. Answer.</h2>
        <p class="sub">
          Bi-encoder search for high recall, cross-encoder reranking for
          precision. The industry-standard pattern, as four function calls.
        </p>

        <div class="funnel">
          <div class="funnel-row">
            <div class="funnel-stage">
              <div class="funnel-fn">llm/embed</div>
              <div class="funnel-desc">text → vectors</div>
              <div class="funnel-box funnel-box-wide">
                <div class="funnel-count">all docs</div>
                <div class="funnel-chips">
                  <span class="fc">Rust</span><span class="fc">Python</span><span class="fc">Lisp</span>
                  <span class="fc">Cooking</span><span class="fc">ML</span><span class="fc">Macros</span>
                  <span class="fc">Gardening</span><span class="fc">Crypto</span><span class="fc">HTTP</span>
                  <span class="fc">Regex</span><span class="fc">JSON</span><span class="fc">PDF</span>
                </div>
              </div>
            </div>

            <div class="funnel-arrow">&darr;</div>

            <div class="funnel-stage">
              <div class="funnel-fn">vector-store/search</div>
              <div class="funnel-desc">cosine nearest-k · high recall</div>
              <div class="funnel-box funnel-box-mid">
                <div class="funnel-count">top 12</div>
                <div class="funnel-chips">
                  <span class="fc fc-hit">Lisp</span><span class="fc fc-hit">Macros</span>
                  <span class="fc fc-hit">Homoiconic</span><span class="fc fc-hit">file/read</span>
                  <span class="fc fc-hit">Regex</span><span class="fc fc-hit">string/split</span>
                  <span class="fc fc-dim">Rust</span><span class="fc fc-dim">Python</span>
                  <span class="fc fc-dim">ML</span><span class="fc fc-dim">HTTP</span>
                  <span class="fc fc-dim">JSON</span><span class="fc fc-dim">PDF</span>
                </div>
              </div>
            </div>

            <div class="funnel-arrow">&darr;</div>

            <div class="funnel-stage">
              <div class="funnel-fn">llm/rerank</div>
              <div class="funnel-desc">cross-encoder · precision</div>
              <div class="funnel-box funnel-box-narrow">
                <div class="funnel-count">top 4</div>
                <div class="funnel-chips">
                  <span class="fc fc-hit">Lisp</span>
                  <span class="fc fc-hit">Macros</span>
                  <span class="fc fc-hit">Homoiconic</span>
                  <span class="fc fc-hit">file/read</span>
                </div>
              </div>
            </div>

            <div class="funnel-arrow">&darr;</div>

            <div class="funnel-stage">
              <div class="funnel-fn">llm/complete</div>
              <div class="funnel-desc">grounded answer</div>
              <div class="funnel-box funnel-box-answer">
                <div class="funnel-answer">"Lisp is homoiconic — code is data."</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: EMBED ============ -->
    <section id="embed">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">Embed</p>
            <h2>Vectors as <em>bytevectors.</em></h2>
            <p class="sub">
              <code>llm/embed</code> takes a string or a list (batch). Returns
              packed f64 bytevectors with a fast path for similarity
              computations — no per-element unboxing overhead.
              It's a first-class value: <code>(map llm/embed ...)</code> just
              works.
            </p>
            <ul class="feature-list">
              <li><strong>Batch embeddings.</strong> <code>(llm/embed ["a" "b" "c"])</code> returns a list of vectors in one network call.</li>
              <li><strong>Seven providers.</strong> Jina, Voyage, Cohere, Nomic, Together AI, Fireworks AI, OpenAI. Auto-configured from env vars. Separate from your chat provider.</li>
              <li><strong>Async-aware.</strong> Inside <code>(async/...)</code>, embeddings offload to the scheduler so sibling tasks overlap.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="code-card">
              <div class="code-card-head">
                <span class="t">embed.sema</span>
                <span class="n">batch + similarity</span>
              </div>
              <pre>(<span class="c-kw">define</span> texts
  (list <span class="c-str">"Rust is a systems language"</span>
        <span class="c-str">"Python is great for ML"</span>
        <span class="c-str">"Lisp is homoiconic"</span>))

<span class="c-com">;; Batch: one network call</span>
(<span class="c-kw">define</span> vecs (llm/embed texts))

<span class="c-com">;; Similarity between two vectors</span>
(llm/similarity
  (first vecs)
  (last vecs))
<span class="c-com">;; => 0.72</span></pre>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: STORE & RETRIEVE ============ -->
    <section id="store">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">Store &amp; retrieve</p>
            <h2>No database. <em>Just a file.</em></h2>
            <p class="sub">
              An in-memory vector store with disk persistence. Index once, query
              forever. <code>vector-store/open</code> loads from disk
              automatically. The JSON format is portable across platforms —
              base64-encoded embeddings, full metadata, diffable in git.
            </p>
            <ul class="feature-list">
              <li><strong>Index once.</strong> <code>vector-store/save</code> writes to disk. Next run loads instantly — no re-embedding.</li>
              <li><strong>Metadata on every doc.</strong> Store source paths, page numbers, timestamps alongside the vector.</li>
              <li><strong>Dimension-mismatch safety.</strong> Mixing embedding models in one store raises a clear error at search time.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="code-card">
              <div class="code-card-head">
                <span class="t">store.sema</span>
                <span class="n">index + search</span>
              </div>
              <pre>(<span class="c-kw">vector-store/open</span> <span class="c-str">"docs"</span> <span class="c-str">"my-docs.json"</span>)

(for-each
  (<span class="c-kw">lambda</span> (text)
    (vector-store/add <span class="c-str">"docs"</span> text
      (llm/embed text)
      {<span class="c-kwd">:text</span> text}))
  texts)

(vector-store/save <span class="c-str">"docs"</span>)

(<span class="c-kw">define</span> hits
  (vector-store/search
    <span class="c-str">"docs"</span>
    (llm/embed <span class="c-str">"Which is homoiconic?"</span>)
    5))

<span class="c-com">;; => ({:id "Lisp"</span>
<span class="c-com">;;     :score 0.94</span>
<span class="c-com">;;     :metadata {:text "Lisp is homoiconic"}}</span>
<span class="c-com">;;    ...)</span></pre>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: RERANK (full-width, not feature-row) ============ -->
    <section id="rerank">
      <div class="wrap">
        <p class="kicker">Rerank</p>
        <h2>Retrieve many. <em>Rerank to a few.</em></h2>
        <p class="sub" style="margin-bottom: 40px">
          Bi-encoders embed query and document independently — fast, but coarse.
          Cross-encoders read them together — slow, but precise. Sema's
          <code>llm/rerank</code> calls a hosted cross-encoder (Cohere, Jina,
          Voyage, Nomic, Together AI, or Fireworks AI) to reorder your candidates. The <code>:index</code> field maps
          back to the original list.
        </p>

        <div class="rerank-flow">
          <div class="rr-stage">
            <div class="rr-label">vector-store/search</div>
            <div class="rr-desc">top-12 by cosine similarity</div>
            <div class="rr-list">
              <div class="rr-item rr-item-dim">file-read-lines <span class="rr-score">0.82</span></div>
              <div class="rr-item rr-item-dim">read-line <span class="rr-score">0.79</span></div>
              <div class="rr-item rr-item-dim">file-for-each-line <span class="rr-score">0.77</span></div>
              <div class="rr-item rr-item-dim">io-read-line <span class="rr-score">0.74</span></div>
              <div class="rr-item rr-item-dim">string/split <span class="rr-score">0.71</span></div>
              <div class="rr-item rr-item-faded">... 7 more</div>
            </div>
          </div>

          <div class="rr-arrow">
            <svg width="28" height="20" viewBox="0 0 28 20" fill="none">
              <path d="M2 10h22m0 0l-6-6m6 6l-6 6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
            <div class="rr-arrow-label">llm/rerank</div>
          </div>

          <div class="rr-stage">
            <div class="rr-label">cross-encoder</div>
            <div class="rr-desc">top-4 by relevance</div>
            <div class="rr-list">
              <div class="rr-item rr-item-hit">file-read-lines <span class="rr-score">0.467</span></div>
              <div class="rr-item rr-item-hit">read-line <span class="rr-score">0.304</span></div>
              <div class="rr-item rr-item-hit">file-for-each-line <span class="rr-score">0.293</span></div>
              <div class="rr-item rr-item-hit">io-read-line <span class="rr-score">0.239</span></div>
            </div>
          </div>
        </div>

        <div class="code-card" style="margin-top: 40px">
          <div class="code-card-head">
            <span class="t">rerank.sema</span>
            <span class="n">three providers, per-call override</span>
          </div>
          <pre>(<span class="c-kw">define</span> candidates
  (vector-store/search <span class="c-str">"docs"</span> (llm/embed question) 12))

(<span class="c-kw">define</span> reranked
  (llm/rerank
    question
    (map (lambda (c) (:text (:metadata c))) candidates)
    {<span class="c-kwd">:top-k</span> 4
     <span class="c-kwd">:provider</span> <span class="c-kwd">:cohere</span>}))

<span class="c-com">;; => ({:index 0 :score 0.467</span>
<span class="c-com">;;     :document "file-read-lines"}</span>
<span class="c-com">;;    {:index 1 :score 0.304</span>
<span class="c-com">;;     :document "read-line"} ...)</span>

<span class="c-com">;; Override per call:</span>
(llm/rerank q docs {:top-k 5 <span class="c-kwd">:provider</span> <span class="c-kwd">:voyage</span>
                    <span class="c-kwd">:model</span> <span class="c-str">"rerank-2.5"</span>})</pre>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: FULL PIPELINE (wide, single code block) ============ -->
    <section id="pipeline">
      <div class="wrap">
        <p class="kicker">The whole pipeline</p>
        <h2>Four functions. <em>One screen.</em></h2>
        <p class="sub" style="margin-bottom: 40px">
          Index a directory of docs, embed the query, retrieve candidates,
          rerank for precision, build context, generate a grounded answer. This
          is the code from <code>examples/llm/rag-docs-search.sema</code> — run
          it with <code>sema</code> and it indexes Sema's own documentation.
        </p>

        <div class="code-card pipeline-card">
          <div class="code-card-head">
            <span class="t">rag-docs-search.sema</span>
            <span class="n">the complete pipeline</span>
          </div>
          <pre><span class="c-com">;; 1. Index (run once, cached to disk)</span>
(vector-store/open <span class="c-str">"docs"</span> <span class="c-str">"/tmp/sema-docs.vec"</span>)
(<span class="c-kw">when</span> (= (vector-store/count <span class="c-str">"docs"</span>) 0)
  (let* ((files (file/glob <span class="c-str">"crates/sema-docs/entries/stdlib/**/*.md"</span>))
         (docs  (map (lambda (p)
                        {:name (path/stem p) :path p
                         :text (string/take (file/read p) 900)})
                      files))
         (vecs  (flat-map llm/embed
                          (list/chunk 64 (map :text docs)))))
    (map (lambda (doc vec)
           (vector-store/add <span class="c-str">"docs"</span> (:name doc) vec doc))
         docs vecs)
    (vector-store/save <span class="c-str">"docs"</span>)))

<span class="c-com">;; 2. Retrieve</span>
(<span class="c-kw">define</span> question <span class="c-str">"How do I read a file and split it into lines?"</span>)
(<span class="c-kw">define</span> hits (vector-store/search <span class="c-str">"docs"</span> (llm/embed question) 12))

<span class="c-com">;; 3. Rerank</span>
(<span class="c-kw">define</span> reranked
  (llm/rerank question
    (map (lambda (c) (:text (:metadata c))) hits)
    {<span class="c-kwd">:top-k</span> 4}))

<span class="c-com">;; 4. Answer</span>
(<span class="c-kw">define</span> context
  (string/join
    (map (lambda (r) (nth (map :text hits) (:index r))) reranked)
    <span class="c-str">"\n\n---\n\n"</span>))

(println (llm/complete
  (prompt (system <span class="c-str">"Answer using only the context."</span>)
          (user (format <span class="c-str">"Context:\n~a\n\nQ: ~a"</span> context question)))
  {<span class="c-kwd">:max-tokens</span> 400}))

<span class="c-com">;; => "Use file/read-lines to read all lines,</span>
<span class="c-com">;;     then string/split or map over the result."</span></pre>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: NO INFRASTRUCTURE (icon grid) ============ -->
    <section id="no-infra">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">No infrastructure</p>
            <h2>No Pinecone. No pgvector. <em>No Chroma.</em></h2>
            <p class="sub">
              The vector store is in-process with disk persistence. No
              connection strings, no Docker compose, no infrastructure to
              maintain. Four embedding providers and three reranker providers,
              all auto-configured from environment variables.
            </p>
            <ul class="feature-list">
              <li><strong>Embedding providers.</strong> Jina, Voyage, Cohere, Nomic, Together AI, Fireworks AI, OpenAI — or any OpenAI-compatible endpoint via <code>:base-url</code>.</li>
              <li><strong>Reranker providers.</strong> Cohere, Jina, Voyage, Nomic, Together AI, Fireworks AI — same API key, per-call override.</li>
              <li><strong>Separate from chat.</strong> <code>llm/configure-embeddings</code> lets you use Voyage for embeddings and Anthropic for chat.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="provider-grid">
              <div class="provider-group">
                <div class="provider-label">embed</div>
                <div class="provider-chips">
                  <span class="chip">Jina</span>
                  <span class="chip">Voyage</span>
                  <span class="chip">Cohere</span>
                  <span class="chip">Nomic</span>
                  <span class="chip">Together</span>
                  <span class="chip">Fireworks</span>
                  <span class="chip">OpenAI</span>
                </div>
              </div>
              <div class="provider-group">
                <div class="provider-label">rerank</div>
                <div class="provider-chips">
                  <span class="chip">Cohere</span>
                  <span class="chip">Jina</span>
                  <span class="chip">Voyage</span>
                  <span class="chip">Nomic</span>
                  <span class="chip">Together</span>
                  <span class="chip">Fireworks</span>
                </div>
              </div>
              <div class="provider-group">
                <div class="provider-label">store</div>
                <div class="provider-chips">
                  <span class="chip chip-gold">in-memory</span>
                  <span class="chip chip-gold">disk (JSON)</span>
                </div>
              </div>
              <div class="provider-group">
                <div class="provider-label">answer</div>
                <div class="provider-chips">
                  <span class="chip">Anthropic</span>
                  <span class="chip">OpenAI</span>
                  <span class="chip">Gemini</span>
                  <span class="chip">Ollama</span>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ COMPARISON ============ -->
    <section id="compare">
      <div class="wrap">
        <p class="kicker">The argument</p>
        <h2>What you'd assemble without it.</h2>
        <p class="sub">
          A typical Python RAG stack: LangChain for orchestration, Chroma or
          Pinecone for vectors, sentence-transformers for embeddings, a separate
          Cohere call for reranking, and prompt templates to glue it together.
          Sema replaces all of it with four function calls.
        </p>
        <div class="compare">
          <div class="pane python">
            <div class="pane-head">
              <span class="t">rag.py</span>
              <span class="n">5 libraries</span>
            </div>
            <pre><span class="c-kw">from</span> langchain.vectorstores <span class="c-kw">import</span> Chroma
<span class="c-kw">from</span> langchain.embeddings <span class="c-kw">import</span> \
    HuggingFaceEmbeddings
<span class="c-kw">from</span> langchain.text_splitter <span class="c-kw">import</span> \
    RecursiveCharacterTextSplitter
<span class="c-kw">from</span> cohere <span class="c-kw">import</span> Client <span class="c-kw">as</span> Cohere
<span class="c-kw">from</span> langchain.<span class="c-fn">openai</span> <span class="c-kw">import</span> ChatOpenAI

embeddings = HuggingFaceEmbeddings()
splitter = RecursiveCharacterTextSplitter()
vectorstore = Chroma.<span class="c-fn">from_documents</span>(
    splitter.<span class="c-fn">split_text</span>(docs), embeddings)
cohere = Cohere(api_key=...)
llm = ChatOpenAI()

<span class="c-kw">def</span> <span class="c-fn">rag</span>(question):
    docs = vectorstore.<span class="c-fn">similarity_search</span>(
        question, k=12)
    results = cohere.<span class="c-fn">rerank</span>(
        query=question, documents=docs, top_k=4)
    context = <span class="c-str">"\n\n"</span>.<span class="c-fn">join</span>(
        docs[r.index].page_content
        <span class="c-kw">for</span> r <span class="c-kw">in</span> results)
    <span class="c-kw">return</span> llm.<span class="c-fn">predict</span>(
        <span class="c-str">f"Context: {context}\nQ: {question}"</span>)</pre>
            <div class="pane-foot">5 imports. 3 objects to configure. Glue code to connect them. The pipeline is hidden behind abstractions.</div>
          </div>
          <div class="pane sema">
            <div class="pane-head">
              <span class="t">rag.sema</span>
              <span class="n">4 functions</span>
            </div>
            <pre>(<span class="c-kw">define</span> hits
  (vector-store/search <span class="c-str">"docs"</span>
    (llm/embed question) 12))

(<span class="c-kw">define</span> reranked
  (llm/rerank question
    (map (lambda (c)
           (:text (:metadata c))) hits)
    {<span class="c-kwd">:top-k</span> 4}))

(<span class="c-kw">define</span> context
  (string/join
    (map (lambda (r)
           (nth (map :text hits)
                (:index r)))
         reranked) <span class="c-str">"\n\n---\n\n"</span>))

(llm/complete
  (prompt
    (system <span class="c-str">"Answer using context."</span>)
    (user (format <span class="c-str">"Context:\n~a\n\nQ: ~a"</span>
                  context question)))
  {<span class="c-kwd">:max-tokens</span> 400})</pre>
            <div class="pane-foot">4 function calls. Zero imports. Zero configuration objects. The pipeline is the code.</div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ CTA ============ -->
    <section class="cta">
      <div class="wrap">
        <h2>Search your first document.</h2>
        <p class="sub">Run the example. It indexes Sema's own docs and answers questions.</p>
        <div class="install-stack">
          <div class="install-row">
            <span class="badge">run</span>
            <span class="install">
              <span class="cmd-text">
                <span class="dollar">$</span>
                <span id="i2">sema examples/llm/rag-docs-search.sema</span>
              </span>
              <button class="copy" @click="copyText('i2', $event)">copy</button>
            </span>
          </div>
          <div class="install-row">
            <span class="badge">install</span>
            <span class="install">
              <span class="cmd-text">
                <span class="dollar">$</span>
                <span id="i3">cargo install sema-lang</span>
              </span>
              <button class="copy" @click="copyText('i3', $event)">copy</button>
            </span>
          </div>
          <div class="hero-actions" style="justify-content:center; margin-top:24px">
            <a class="btn btn-gold" href="/docs/llm/rag">RAG docs</a>
            <a class="btn btn-ghost" href="/docs/llm/vector-store">Vector store docs</a>
            <a class="btn btn-ghost" href="https://sema.run">Open the playground</a>
          </div>
        </div>
      </div>
    </section>

  </CustomPageLayout>
</template>

<style scoped>
/* ---------- hero ---------- */
.hero { padding: 104px 0 56px; }

/* ---------- pipeline funnel (unique to this page) ---------- */
.pipeline-showcase { padding: 0 0 88px; border-top: none; }

.funnel {
  margin-top: 44px;
}

.funnel-row {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0;
}

.funnel-stage {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
}

.funnel-fn {
  font-family: var(--font-mono);
  font-size: 13px;
  font-weight: 500;
  color: var(--gold-bright);
}

.funnel-desc {
  font-size: 12px;
  color: var(--muted);
}

.funnel-box {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 14px 18px;
  margin-top: 4px;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 10px;
}

.funnel-box-wide { width: 100%; max-width: 580px; }
.funnel-box-mid { width: 100%; max-width: 460px; }
.funnel-box-narrow { width: 100%; max-width: 340px; }
.funnel-box-answer {
  width: 100%;
  max-width: 420px;
  border-color: var(--gold-line);
  background: var(--gold-fade);
}

.funnel-count {
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
  text-transform: uppercase;
  letter-spacing: 0.08em;
}

.funnel-chips {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  justify-content: center;
}

.fc {
  font-family: var(--font-mono);
  font-size: 11.5px;
  color: var(--dim);
  background: var(--bg);
  border: 1px solid var(--border-lo);
  border-radius: 5px;
  padding: 3px 8px;
}

.fc-hit {
  color: var(--gold-bright);
  border-color: var(--gold-line);
  background: var(--gold-fade);
}

.fc-dim { opacity: 0.4; }

.funnel-answer {
  font-family: var(--font-display);
  font-style: italic;
  font-size: 16px;
  color: var(--gold-bright);
  text-align: center;
  line-height: 1.5;
}

.funnel-arrow {
  font-size: 22px;
  color: var(--dim);
  padding: 8px 0;
  line-height: 1;
}

/* ---------- feature rows ---------- */
.feature-row {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 56px;
  align-items: center;
  margin-top: 20px;
}
.feature-row.reverse .feature-text { order: 2; }
.feature-row.reverse .feature-visual { order: 1; }

.feature-list { margin-top: 24px; }
.feature-list li {
  padding: 10px 0;
  font-size: 14.5px;
  color: var(--muted);
  line-height: 1.65;
  border-bottom: 1px solid var(--border-lo);
}
.feature-list li:last-child { border-bottom: none; }
.feature-list strong { color: var(--text); font-weight: 500; display: block; margin-bottom: 2px; }
.feature-list code {
  font-family: var(--font-mono);
  font-size: 12.5px;
  color: var(--gold-bright);
  background: var(--gold-fade);
  padding: 1px 5px;
  border-radius: 4px;
}

/* ---------- code card ---------- */
.code-card {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 0 0 1px rgba(200, 168, 85, .04), 0 20px 50px -30px rgba(0, 0, 0, .3);
}
.code-card-head {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 10px;
  padding: 13px 18px;
  border-bottom: 1px solid var(--border-lo);
  font-family: var(--font-mono);
  font-size: 12px;
}
.code-card-head .t { color: var(--gold-bright); }
.code-card-head .n { color: var(--dim); }
.code-card pre {
  font-family: var(--font-mono);
  font-size: 12.5px;
  line-height: 1.62;
  padding: 18px 20px;
  overflow-x: auto;
  color: #c9c2b4;
}

.pipeline-card pre { font-size: 12px; line-height: 1.6; }

/* ---------- rerank flow (unique to this page) ---------- */
.rerank-flow {
  display: flex;
  align-items: stretch;
  gap: 0;
  justify-content: center;
  flex-wrap: wrap;
}

.rr-stage {
  flex: 1;
  min-width: 260px;
  max-width: 340px;
}

.rr-label {
  font-family: var(--font-mono);
  font-size: 13px;
  color: var(--gold-bright);
  font-weight: 500;
  margin-bottom: 4px;
}

.rr-desc {
  font-size: 12px;
  color: var(--muted);
  margin-bottom: 16px;
}

.rr-list {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.rr-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  font-family: var(--font-mono);
  font-size: 12px;
  padding: 8px 14px;
  background: var(--bg-raise);
  border: 1px solid var(--border-lo);
  border-radius: 6px;
  color: var(--muted);
}

.rr-item-dim { color: var(--muted); }

.rr-item-faded {
  color: var(--dim);
  border-style: dashed;
  text-align: center;
  justify-content: center;
}

.rr-item-hit {
  color: var(--text);
  border-color: var(--gold-line);
  background: var(--gold-fade);
}

.rr-score {
  color: var(--dim);
  font-size: 11px;
}

.rr-item-hit .rr-score { color: var(--gold-bright); }

.rr-arrow {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 0 20px;
  color: var(--dim);
}

.rr-arrow-label {
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--gold);
  margin-top: 6px;
}

/* ---------- provider grid (unique to this page) ---------- */
.provider-grid {
  display: flex;
  flex-direction: column;
  gap: 18px;
}

.provider-group {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.provider-label {
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
  text-transform: uppercase;
  letter-spacing: 0.08em;
}

.provider-chips {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

.chip {
  font-family: var(--font-mono);
  font-size: 12px;
  color: var(--text);
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 5px 12px;
}

.chip-gold {
  color: var(--gold-bright);
  border-color: var(--gold-line);
  background: var(--gold-fade);
}

/* ---------- comparison ---------- */
.compare {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 22px;
  margin-top: 46px;
  align-items: start;
}
.pane {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 12px;
  overflow: hidden;
}
.pane.sema {
  border-color: var(--gold-line);
  box-shadow: 0 0 0 1px rgba(200, 168, 85, .08), 0 24px 60px -30px rgba(200, 168, 85, .12);
}
.pane-head {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 10px;
  padding: 13px 18px;
  border-bottom: 1px solid var(--border-lo);
  font-family: var(--font-mono);
  font-size: 12px;
}
.pane-head .t { color: var(--text); }
.pane.sema .pane-head .t { color: var(--gold-bright); }
.pane-head .n { color: var(--dim); }
.pane pre {
  font-family: var(--font-mono);
  font-size: 12px;
  line-height: 1.58;
  padding: 18px 20px;
  overflow-x: auto;
  color: #c9c2b4;
}
.pane-foot {
  padding: 13px 18px;
  border-top: 1px solid var(--border-lo);
  font-size: 13px;
  color: var(--muted);
  line-height: 1.55;
}
.pane.sema .pane-foot { color: var(--text); }

/* ---------- responsive ---------- */
@media (max-width: 880px) {
  .hero { padding: 72px 0 48px; }
  .feature-row, .feature-row.reverse { grid-template-columns: 1fr; }
  .feature-row.reverse .feature-text { order: unset; }
  .feature-row.reverse .feature-visual { order: unset; }
  .compare { grid-template-columns: 1fr; }
  .funnel-box-wide, .funnel-box-mid, .funnel-box-narrow, .funnel-box-answer { max-width: 100%; }
  .rerank-flow { flex-direction: column; align-items: center; }
  .rr-arrow { flex-direction: row; padding: 16px 0; }
  .rr-arrow-label { margin-top: 0; margin-left: 8px; }
  .rr-stage { max-width: 100%; }
}
</style>
