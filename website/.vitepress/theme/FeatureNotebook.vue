<script setup>
import CustomPageLayout from './CustomPageLayout.vue'
</script>

<template>
  <CustomPageLayout active-nav="notebook" v-slot="{ copyText }">

    <!-- ============ HERO ============ -->
    <header class="hero">
      <span class="hero-paren l" aria-hidden="true">(</span>
      <span class="hero-paren r" aria-hidden="true">)</span>
      <div class="wrap">
        <p class="eyebrow">Feature<span class="sep">·</span>Interactive Notebook<span class="sep">·</span>Browser-based</p>
        <h1>Interactive Sema, <em>cell by cell.</em></h1>
        <p class="lede">
          A Jupyter-inspired notebook built into the Sema toolchain. Write code in
          cells, evaluate them individually or all at once, and see results inline —
          including <strong>LLM outputs with timing and token usage</strong>. All in the
          browser, all backed by the same runtime.
        </p>
        <div class="hero-actions">
          <a class="btn btn-gold" href="/docs/notebook">Read the docs</a>
          <a class="btn btn-ghost" href="https://sema.run">Try the playground</a>
        </div>
        <div class="hero-actions">
          <span class="install">
            <span class="cmd-text">
              <span class="dollar">$</span>
              <span id="i1">sema notebook new my-notebook.sema-nb</span>
            </span>
            <button class="copy" @click="copyText('i1', $event)">copy</button>
          </span>
        </div>
        <p class="req">Then: <code>sema notebook serve my-notebook.sema-nb</code> &rarr; http://localhost:8888</p>
      </div>
    </header>

    <!-- ============ NOTEBOOK GUI MOCKUP ============ -->
    <section class="nb-showcase">
      <div class="wrap">
        <div class="nb-gui">
          <div class="nb-toolbar">
            <div class="nb-toolbar-left">
              <span class="nb-file-icon" aria-hidden="true">
                <svg viewBox="0 0 16 16" width="14" height="14"><path d="M2 2h8l4 4v8H2V2zm8 0v4h4" stroke="currentColor" fill="none" stroke-width="1.2" stroke-linejoin="round"/></svg>
              </span>
              <span class="nb-title">sentiment-analysis.sema-nb</span>
              <span class="nb-status">saved</span>
            </div>
            <div class="nb-toolbar-right">
              <button class="nb-btn">+ Code</button>
              <button class="nb-btn">+ Markdown</button>
              <span class="nb-toolbar-sep"></span>
              <button class="nb-btn nb-btn-run">Run All</button>
              <button class="nb-btn">Undo</button>
              <button class="nb-btn">Save</button>
              <button class="nb-btn">Reset</button>
            </div>
          </div>

          <div class="nb-body">
            <!-- Markdown cell -->
            <div class="nb-cell-md">
              <div class="nb-cell-num">[md]</div>
              <div class="nb-md-rendered">
                <h4>Sentiment Analysis</h4>
                <p>Classify text into <span class="md-code">positive</span>, <span class="md-code">negative</span>, or <span class="md-code">neutral</span> using Sema's <span class="md-code">llm/extract</span> primitive.</p>
              </div>
            </div>

            <!-- Cell 1: define data -->
            <div class="nb-cell active">
              <div class="nb-cell-num">[1]</div>
              <div class="nb-cell-editor">
                <span class="c-com">;; Sample texts to classify</span><br>
                (<span class="c-kw">define</span> texts<br>
                &nbsp;&nbsp;[<span class="c-str">"I love this product!"</span><br>
                &nbsp;&nbsp;&nbsp;<span class="c-str">"Terrible experience, would not recommend."</span><br>
                &nbsp;&nbsp;&nbsp;<span class="c-str">"It's okay, nothing special."</span>])
              </div>
            </div>
            <div class="nb-output">
              <div class="out-meta-inline">3 items · 4ms · $0.0000</div>
            </div>

            <!-- Cell 2: define classify function -->
            <div class="nb-cell active">
              <div class="nb-cell-num">[2]</div>
              <div class="nb-cell-editor">
                (<span class="c-kw">define</span> <span class="c-fn">classify</span><br>
                &nbsp;&nbsp;(<span class="c-kw">fn</span> (text)<br>
                &nbsp;&nbsp;&nbsp;&nbsp;(<span class="c-kw">llm/extract</span><br>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{<span class="c-kwd">:sentiment</span> {<span class="c-kwd">:type</span> <span class="c-kwd">:string</span><br>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c-kwd">:enum</span> [<span class="c-str">"positive"</span> <span class="c-str">"negative"</span> <span class="c-str">"neutral"</span>]}}<br>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;f<span class="c-str">"Classify the sentiment of: ${text}"</span>)))
              </div>
            </div>
            <div class="nb-output">
              <div class="out-meta-inline"><span class="c-fn">classify</span> — function defined · 12ms · $0.0000</div>
            </div>

            <!-- Cell 3: run classification (stale) -->
            <div class="nb-cell stale">
              <div class="nb-cell-num">[3*]</div>
              <div class="nb-cell-editor">
                (<span class="c-kw">map</span> <span class="c-fn">classify</span> texts)
              </div>
            </div>
            <div class="nb-output">
              <div class="out-stdout">Classifying 3 texts via llm/extract…</div>
              <div class="out-value">[{<span class="c-kwd">:sentiment</span> <span class="c-str">"positive"</span>} {<span class="c-kwd">:sentiment</span> <span class="c-str">"negative"</span>} {<span class="c-kwd">:sentiment</span> <span class="c-str">"neutral"</span>}]</div>
              <div class="out-meta">847ms · 152 tok · claude-sonnet-4-6</div>
            </div>

            <!-- Insert cell hover indicator -->
            <div class="nb-insert-hint">
              <span class="nb-insert-line"></span>
              <button class="nb-insert-btn">+ Code</button>
              <button class="nb-insert-btn">+ Markdown</button>
              <span class="nb-insert-line"></span>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: SHARED ENVIRONMENT ============ -->
    <section id="shared-env">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">Shared environment</p>
            <h2>Cells remember each other.</h2>
            <p class="sub">
              Definitions in earlier cells are visible in later ones. Define a
              function in cell 1, call it in cell 5. The environment persists across
              evaluations — no re-imports, no re-definitions, no boilerplate.
            </p>
            <ul class="feature-list">
              <li><strong>Persistent scope.</strong> Every cell evaluation extends the same environment. Variables, functions, and macros stick around.</li>
              <li><strong>Incremental workflow.</strong> Re-evaluate one cell after editing — downstream cells are marked stale so you know what to re-run.</li>
              <li><strong>Reset when needed.</strong> One click clears everything and starts fresh.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="nb-gui mini">
              <div class="nb-body">
                <div class="nb-cell active">
                  <div class="nb-cell-num">[1]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">define</span> <span class="c-fn">greet</span><br>
                    &nbsp;&nbsp;(<span class="c-kw">fn</span> (name)<br>
                    &nbsp;&nbsp;&nbsp;&nbsp;(<span class="c-kw">format</span> <span class="c-str">"Hello, ~a!"</span> name)))
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-meta-inline"><span class="c-fn">greet</span> — function defined</div>
                </div>
                <div class="nb-cell active">
                  <div class="nb-cell-num">[2]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-fn">greet</span> <span class="c-str">"Sema"</span>)
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-value">"Hello, Sema!"</div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: MARKDOWN CELLS ============ -->
    <section id="markdown-cells">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">Markdown cells</p>
            <h2>Document as you go.</h2>
            <p class="sub">
              Markdown cells render formatted text — headings, bold, italic, inline
              code, code blocks, lists — right alongside your code. Click to edit,
              press Shift+Enter to re-render. Build a narrative, not just a script.
            </p>
            <ul class="feature-list">
              <li><strong>Rich formatting.</strong> Headings, lists, inline code, and code blocks for documentation that reads like a page, not a comment.</li>
              <li><strong>Click to edit.</strong> Click rendered markdown to drop back into source. Shift+Enter to re-render.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="nb-gui mini">
              <div class="nb-body">
                <div class="nb-cell-md">
                  <div class="nb-cell-num">[md]</div>
                  <div class="nb-md-rendered">
                    <h4>Analysis Notes</h4>
                    <p>The <span class="md-code">classify</span> function uses <span class="md-code">llm/extract</span> with a typed schema. Results come back as <strong>maps</strong>, not strings to re-parse.</p>
                    <ul class="md-list">
                      <li>Positive: 1 text</li>
                      <li>Negative: 1 text</li>
                      <li>Neutral: 1 text</li>
                    </ul>
                  </div>
                </div>
                <div class="nb-cell">
                  <div class="nb-cell-num">[4]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">count</span> results)
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-value">3</div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: UNDO & ROLLBACK ============ -->
    <section id="undo">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">Undo &amp; rollback</p>
            <h2>Mistakes don't stick.</h2>
            <p class="sub">
              After evaluating a cell, click <strong>Undo</strong> to roll back: the
              cell's outputs are restored to their previous state, the interpreter
              environment is rolled back to before the evaluation, and downstream
              stale markers are reverted. Useful when a cell modifies global state
              unexpectedly.
            </p>
            <ul class="feature-list">
              <li><strong>Environment-level undo.</strong> Not just text — the runtime state itself is rewound.</li>
              <li><strong>Inline undo on errors.</strong> Error outputs show an "Undo cell" button right where the failure happened.</li>
              <li><strong>Stale marker cleanup.</strong> Downstream cells that were marked stale by the evaluation are reverted too.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="nb-gui mini">
              <div class="nb-body">
                <div class="nb-cell">
                  <div class="nb-cell-num">[5]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">define</span> counter 0)
                  </div>
                </div>
                <div class="nb-cell stale">
                  <div class="nb-cell-num">[6*]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">set!</span> counter (<span class="c-kw">inc</span> counter))
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-value">1</div>
                  <button class="nb-undo-btn">↶ Undo cell</button>
                </div>
                <div class="nb-cell">
                  <div class="nb-cell-num">[5]</div>
                  <div class="nb-cell-editor nb-editor-hint">
                    <span class="c-com">; counter is back to 0 after undo</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: STALE MARKERS ============ -->
    <section id="stale">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">Stale tracking</p>
            <h2>Know what's out of date.</h2>
            <p class="sub">
              When an upstream cell is re-evaluated, all downstream code cells with
              existing outputs are automatically marked <strong>stale</strong>. A
              dashed gold border and a <code>[*]</code> in the cell number tell you
              exactly which outputs might not reflect the current environment — until
              you re-run them.
            </p>
            <ul class="feature-list">
              <li><strong>Visual indicator.</strong> Dashed gold left border + <code>[N*]</code> cell number — unmistakable, not annoying.</li>
              <li><strong>Previous output preserved.</strong> Stale cells still show their last output, so you don't lose work — you just know it might be outdated.</li>
              <li><strong>Automatic propagation.</strong> No manual tagging — the notebook tracks dependencies for you.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="nb-gui mini">
              <div class="nb-body">
                <div class="nb-cell active">
                  <div class="nb-cell-num">[1]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">define</span> threshold <span class="c-str">0.8</span>)
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-meta-inline">re-evaluated · 2ms</div>
                </div>
                <div class="nb-cell stale">
                  <div class="nb-cell-num">[2*]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">filter</span> (<span class="c-kw">fn</span> (x) (&gt; x threshold)) scores)
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-value" style="opacity:0.6">[0.92 0.85 0.81]</div>
                  <div class="out-meta" style="opacity:0.6">stale — re-evaluate cell [1] changed threshold</div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: HEADLESS & EXPORT ============ -->
    <section id="headless">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">Headless &amp; export</p>
            <h2>Run without a browser.</h2>
            <p class="sub">
              The same notebook you iterate on in the browser runs headless in CI.
              Evaluate all code cells, print stdout to the terminal, and export to
              Markdown for reports. No UI required — the notebook format is just
              JSON.
            </p>
            <ul class="feature-list">
              <li><strong>CI validation.</strong> <code>sema notebook run</code> evaluates every cell in order. Add it to a pipeline.</li>
              <li><strong>Selective execution.</strong> Run specific cells by index with <code>--cells 1,3,5</code>.</li>
              <li><strong>Markdown export.</strong> <code>sema notebook export</code> produces a clean .md with code, outputs, and errors.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="term">
              <div class="head">terminal — CI pipeline</div>
              <div><span class="dollar">$</span> sema notebook run sentiment.sema-nb</div>
              <div class="out">→ evaluating 4 code cells…</div>
              <div class="out">→ [1] 3 items (4ms)</div>
              <div class="out">→ [2] classify defined (12ms)</div>
              <div class="out">→ [3] [{:sentiment "positive"} …] (847ms, 152 tok)</div>
              <div class="out">→ [4] 3 (1ms)</div>
              <div class="ok">✓ all code cells passed</div>
              <div>&nbsp;</div>
              <div><span class="dollar">$</span> sema notebook export sentiment.sema-nb -o report.md</div>
              <div class="out">→ wrote report.md (2.1 KB)</div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: REST API ============ -->
    <section id="rest-api">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">REST API</p>
            <h2>Everything is scriptable.</h2>
            <p class="sub">
              The notebook server exposes a JSON HTTP API on the same port as the
              UI. Everything the browser does — creating cells, evaluating, reordering,
              saving — goes through these endpoints. They're stable enough to script
              against from external tools, CI, or a custom frontend.
            </p>
            <ul class="feature-list">
              <li><strong>Full CRUD.</strong> Create, read, update, delete, and evaluate cells via <code>/api/cells</code>.</li>
              <li><strong>Batch evaluation.</strong> <code>POST /api/eval-all</code> runs every cell, with optional inline source overrides.</li>
              <li><strong>VFS included.</strong> Read, write, and list files alongside the notebook through <code>/vfs/*</code>.</li>
            </ul>
            <p class="sub" style="margin-top:18px">
              <a href="/docs/notebook#rest-api">Full API reference →</a>
            </p>
          </div>
          <div class="feature-visual">
            <div class="term">
              <div class="head">curl — evaluate a cell</div>
              <div><span class="dollar">$</span> curl -X POST localhost:8888/api/cells/c4a3f2b1/eval</div>
              <div class="out">→ 200 OK</div>
              <div class="out">{</div>
              <div class="out">&nbsp;&nbsp;<span class="c-kwd">"id"</span>: <span class="c-str">"c4a3f2b1"</span>,</div>
              <div class="out">&nbsp;&nbsp;<span class="c-kwd">"output"</span>: {</div>
              <div class="out">&nbsp;&nbsp;&nbsp;&nbsp;<span class="c-kwd">"type"</span>: <span class="c-str">"value"</span>,</div>
              <div class="out">&nbsp;&nbsp;&nbsp;&nbsp;<span class="c-kwd">"display"</span>: <span class="c-str">"3"</span>,</div>
              <div class="out">&nbsp;&nbsp;&nbsp;&nbsp;<span class="c-kwd">"duration_ms"</span>: 12</div>
              <div class="out">&nbsp;&nbsp;},</div>
              <div class="out">&nbsp;&nbsp;<span class="c-kwd">"stdout"</span>: <span class="c-str">""</span>,</div>
              <div class="out">&nbsp;&nbsp;<span class="c-kwd">"can_undo"</span>: <span class="c-kwd">true</span></div>
              <div class="out">}</div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ KEYBOARD SHORTCUTS ============ -->
    <section id="shortcuts">
      <div class="wrap">
        <p class="kicker">Keyboard-first</p>
        <h2>Hands stay on the keys.</h2>
        <p class="sub">The notebook is designed for a keyboard-first workflow. No mouse required for the common loop: edit, run, advance.</p>

        <div class="shortcuts-grid">
          <div class="shortcut">
            <kbd>Shift</kbd>+<kbd>Enter</kbd>
            <span>Run cell and advance to next</span>
          </div>
          <div class="shortcut">
            <kbd>Cmd</kbd>/<kbd>Ctrl</kbd>+<kbd>Enter</kbd>
            <span>Run cell and stay focused</span>
          </div>
          <div class="shortcut">
            <kbd>Cmd</kbd>/<kbd>Ctrl</kbd>+<kbd>S</kbd>
            <span>Save notebook</span>
          </div>
          <div class="shortcut">
            <kbd>Tab</kbd>
            <span>Insert 2 spaces</span>
          </div>
          <div class="shortcut">
            <kbd>Esc</kbd>
            <span>Deselect cell</span>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ CTA ============ -->
    <section class="cta">
      <div class="wrap">
        <h2>Start a notebook in seconds.</h2>
        <p class="sub">Create one, serve it, open the browser. That's the whole setup.</p>
        <div class="install-stack">
          <div class="install-row">
            <span class="badge">new</span>
            <span class="install">
              <span class="cmd-text">
                <span class="dollar">$</span>
                <span id="i2">sema notebook new my-notebook.sema-nb</span>
              </span>
              <button class="copy" @click="copyText('i2', $event)">copy</button>
            </span>
          </div>
          <div class="install-row">
            <span class="badge">serve</span>
            <span class="install">
              <span class="cmd-text">
                <span class="dollar">$</span>
                <span id="i3">sema notebook serve my-notebook.sema-nb</span>
              </span>
              <button class="copy" @click="copyText('i3', $event)">copy</button>
            </span>
          </div>
          <div class="hero-actions" style="justify-content:center; margin-top:24px">
            <a class="btn btn-gold" href="/docs/notebook">Notebook docs</a>
            <a class="btn btn-ghost" href="https://sema.run">Open the playground</a>
          </div>
        </div>
      </div>
    </section>

  </CustomPageLayout>
</template>

<style scoped>
/* ---------- hero (page-specific padding) ---------- */
.hero { padding: 104px 0 56px; }
.req code { font-family: var(--font-mono); color: var(--muted); }

/* ---------- notebook GUI ---------- */
.nb-showcase { padding: 0 0 88px; border-top: none; }

.nb-gui {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 14px;
  overflow: hidden;
  box-shadow: 0 0 0 1px rgba(200, 168, 85, .06), 0 40px 80px -40px rgba(200, 168, 85, .15);
}

.nb-gui.mini {
  border-radius: 10px;
  box-shadow: 0 0 0 1px rgba(200, 168, 85, .04), 0 20px 50px -30px rgba(0, 0, 0, .4);
}

.nb-toolbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 12px;
  padding: 10px 16px;
  background: var(--surface);
  border-bottom: 1px solid var(--border-lo);
  font-family: var(--font-mono);
  font-size: 12px;
}

.nb-toolbar-left {
  display: flex;
  align-items: center;
  gap: 8px;
  min-width: 0;
}

.nb-file-icon { color: var(--gold); display: flex; align-items: center; }

.nb-title {
  color: var(--text);
  font-weight: 500;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.nb-status {
  color: var(--dim);
  font-size: 10.5px;
  padding: 2px 7px;
  border: 1px solid var(--border);
  border-radius: 4px;
  flex-shrink: 0;
}

.nb-toolbar-right {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-shrink: 0;
}

.nb-btn {
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--muted);
  background: none;
  border: 1px solid var(--border);
  border-radius: 5px;
  padding: 4px 9px;
  cursor: default;
  white-space: nowrap;
  transition: all .15s;
}

.nb-btn:hover { color: var(--text); border-color: var(--gold-line); }

.nb-btn-run {
  color: var(--gold-bright);
  border-color: var(--gold-line);
  background: var(--gold-fade);
}

.nb-toolbar-sep {
  width: 1px;
  height: 18px;
  background: var(--border-lo);
  margin: 0 4px;
}

.nb-body {
  padding: 18px 20px;
  display: flex;
  flex-direction: column;
  gap: 2px;
}

/* ---------- notebook cells ---------- */
.nb-cell {
  display: flex;
  gap: 0;
  width: 100%;
}

.nb-cell-num {
  width: 40px;
  flex-shrink: 0;
  display: flex;
  align-items: flex-start;
  justify-content: center;
  padding-top: 0.55rem;
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
}

.nb-cell-editor {
  flex-grow: 1;
  font-family: var(--font-mono);
  font-size: 12.5px;
  line-height: 1.65;
  color: var(--text);
  background: var(--bg-editor);
  border: 1px solid var(--border);
  border-radius: 4px;
  padding: 0.5rem 0.8rem;
  white-space: pre;
  overflow-x: auto;
}

.nb-cell.active .nb-cell-editor {
  border-left: 3px solid var(--gold);
  border-top-left-radius: 0;
  border-bottom-left-radius: 0;
}

.nb-cell.stale .nb-cell-editor {
  border-left: 3px dashed rgba(200, 168, 85, 0.4);
  border-top-left-radius: 0;
  border-bottom-left-radius: 0;
  opacity: 0.7;
}

.nb-editor-hint {
  border-left: 3px solid var(--border-lo);
  border-top-left-radius: 0;
  border-bottom-left-radius: 0;
}

/* ---------- markdown cell ---------- */
.nb-cell-md {
  display: flex;
  gap: 0;
  width: 100%;
  margin-bottom: 6px;
}

.nb-cell-md .nb-cell-num {
  width: 40px;
  flex-shrink: 0;
  display: flex;
  align-items: flex-start;
  justify-content: center;
  padding-top: 0.5rem;
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
}

.nb-md-rendered {
  flex-grow: 1;
  padding: 0.65rem 0.8rem;
  font-family: var(--font-body);
  font-size: 14px;
  line-height: 1.6;
  color: var(--text);
}

.nb-md-rendered h4 {
  font-family: var(--font-display);
  font-weight: 500;
  font-size: 17px;
  margin-bottom: 6px;
  color: var(--text);
}

.nb-md-rendered p {
  color: var(--muted);
  margin-bottom: 6px;
}

.md-code {
  font-family: var(--font-mono);
  font-size: 12px;
  color: var(--gold-bright);
  background: var(--gold-fade);
  padding: 1px 5px;
  border-radius: 4px;
}

.md-list {
  list-style: disc !important;
  padding-left: 1.4rem !important;
  margin-top: 4px;
}

.md-list li {
  color: var(--muted);
  padding: 2px 0;
  list-style: disc;
}

/* ---------- output panel ---------- */
.nb-output {
  margin-left: 40px;
  margin-bottom: 8px;
}

.out-meta-inline {
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
  padding: 0.3rem 0.8rem;
}

.out-meta-inline .c-fn { color: var(--gold-bright); }

.nb-output .out-stdout {
  font-family: var(--font-mono);
  font-size: 12px;
  color: var(--muted);
  padding: 0.3rem 0.8rem;
}

.nb-output .out-value {
  font-family: var(--font-mono);
  font-size: 12.5px;
  color: var(--gold-bright);
  background: var(--bg-output);
  border: 1px solid var(--border-lo);
  border-left: 2px solid rgba(200, 168, 85, 0.4);
  border-radius: 0 4px 4px 0;
  padding: 0.45rem 0.8rem;
}

.nb-output .out-value .c-kwd { color: #b8a3d6; }
.nb-output .out-value .c-str { color: #a8b88a; }

.nb-output .out-meta {
  font-family: var(--font-mono);
  font-size: 10.5px;
  color: var(--dim);
  padding: 0.3rem 0.8rem;
  margin-top: 2px;
}

.nb-undo-btn {
  display: inline-block;
  font-family: var(--font-mono);
  font-size: 11px;
  color: #c97b6a;
  background: rgba(201, 123, 106, 0.06);
  border: 1px solid rgba(201, 123, 106, 0.2);
  border-radius: 4px;
  padding: 3px 8px;
  cursor: default;
  margin-top: 6px;
}

/* ---------- insert cell hint ---------- */
.nb-insert-hint {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 0;
  opacity: 0.4;
  transition: opacity .15s;
}

.nb-insert-hint:hover { opacity: 1; }

.nb-insert-line {
  flex-grow: 1;
  height: 1px;
  background: var(--border-lo);
}

.nb-insert-btn {
  font-family: var(--font-mono);
  font-size: 10.5px;
  color: var(--muted);
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 4px;
  padding: 3px 8px;
  cursor: default;
  white-space: nowrap;
}

.nb-insert-btn:hover { color: var(--gold-bright); border-color: var(--gold-line); }

/* ---------- feature rows (alternating left/right) ---------- */
.feature-row {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 56px;
  align-items: center;
  margin-top: 20px;
}

.feature-row.reverse .feature-text { order: 2; }
.feature-row.reverse .feature-visual { order: 1; }

.feature-list {
  margin-top: 24px;
}

.feature-list li {
  padding: 10px 0;
  font-size: 14.5px;
  color: var(--muted);
  line-height: 1.65;
  border-bottom: 1px solid var(--border-lo);
}

.feature-list li:last-child { border-bottom: none; }

.feature-list strong {
  color: var(--text);
  font-weight: 500;
  display: block;
  margin-bottom: 2px;
}

.feature-list code {
  font-family: var(--font-mono);
  font-size: 12.5px;
  color: var(--gold-bright);
  background: var(--gold-fade);
  padding: 1px 5px;
  border-radius: 4px;
}

/* ---------- keyboard shortcuts ---------- */
.shortcuts-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 16px;
  margin-top: 40px;
}

.shortcut {
  display: flex;
  align-items: center;
  gap: 14px;
  padding: 16px 20px;
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 10px;
}

.shortcut kbd {
  font-family: var(--font-mono);
  font-size: 11.5px;
  font-weight: 500;
  color: var(--gold-bright);
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 5px;
  padding: 3px 8px;
  white-space: nowrap;
  box-shadow: 0 2px 0 var(--border-lo);
}

.shortcut span {
  font-size: 13.5px;
  color: var(--muted);
}

/* ---------- responsive ---------- */
@media (max-width: 880px) {
  .hero { padding: 72px 0 48px; }

  .feature-row, .feature-row.reverse {
    grid-template-columns: 1fr;
  }
  .feature-row.reverse .feature-text { order: unset; }
  .feature-row.reverse .feature-visual { order: unset; }

  .nb-showcase { padding: 0 0 64px; }

  .nb-toolbar-right { flex-wrap: wrap; gap: 4px; }
  .nb-btn { font-size: 10px; padding: 3px 7px; }
  .nb-toolbar-sep { display: none; }

  .nb-cell-editor { font-size: 11.5px; }
}
</style>
