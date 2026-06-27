<script setup>
import CustomPageLayout from './CustomPageLayout.vue'
</script>

<template>
  <CustomPageLayout active-nav="cassettes" v-slot="{ copyText }">

    <!-- ============ HERO ============ -->
    <header class="hero">
      <span class="hero-paren l" aria-hidden="true">(</span>
      <span class="hero-paren r" aria-hidden="true">)</span>
      <div class="wrap">
        <p class="eyebrow">Feature<span class="sep">·</span>Cassettes<span class="sep">·</span>Record &amp; Replay</p>
        <h1>Record once. <em>Replay forever.</em></h1>
        <p class="lede">
          A cassette saves the answers from real LLM calls to a file the first
          time you run, then plays them back on every run after — no API key,
          no network, the same output every time. <strong>Deterministic tests
          without secrets. Offline demos that always work.</strong>
        </p>
        <div class="hero-actions">
          <a class="btn btn-gold" href="/docs/llm/cassettes">Read the docs</a>
          <a class="btn btn-ghost" href="https://sema.run">Try the playground</a>
        </div>
        <div class="hero-actions">
          <span class="install">
            <span class="cmd-text">
              <span class="dollar">$</span>
              <span id="i1">cargo install sema-lang</span>
            </span>
            <button class="copy" @click="copyText('i1', $event)">copy</button>
          </span>
        </div>
        <p class="req">NDJSON tapes · safe to commit · works with agents, streaming, embeddings</p>
      </div>
    </header>

    <!-- ============ TAPE DECK MOCKUP ============ -->
    <section class="deck-showcase">
      <div class="wrap">
        <div class="tape-deck">
          <div class="deck-header">
            <div class="deck-label">
              <span class="deck-icon" aria-hidden="true">
                <svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="1.5">
                  <rect x="2" y="6" width="20" height="12" rx="2"/>
                  <circle cx="8" cy="12" r="2.5"/>
                  <circle cx="16" cy="12" r="2.5"/>
                  <path d="M10 12h4" stroke-linecap="round"/>
                </svg>
              </span>
              <span class="deck-title">weather-agent.jsonl</span>
            </div>
            <div class="deck-status">
              <span class="status-dot recording"></span>
              <span class="status-text">RECORDING</span>
            </div>
          </div>

          <div class="deck-body">
            <div class="deck-track">
              <div class="track-num">01</div>
              <div class="track-info">
                <div class="track-kind">complete</div>
                <div class="track-key">a1b2c3…</div>
              </div>
              <div class="track-content">"Hello"</div>
              <div class="track-meta">12 tok · gpt-5-mini</div>
            </div>

            <div class="deck-track">
              <div class="track-num">02</div>
              <div class="track-info">
                <div class="track-kind">complete</div>
                <div class="track-key">d4e5f6…</div>
              </div>
              <div class="track-content">"It's sunny, 22°C."</div>
              <div class="track-meta">28 tok · gpt-5-mini</div>
            </div>

            <div class="deck-track">
              <div class="track-num">03</div>
              <div class="track-info">
                <div class="track-kind">embed</div>
                <div class="track-key">g7h8i9…</div>
              </div>
              <div class="track-content">[0.01, -0.02, 0.03, …]</div>
              <div class="track-meta">1536 dim · text-embedding-3-small</div>
            </div>

            <div class="deck-track deck-track-miss">
              <div class="track-num">04</div>
              <div class="track-info">
                <div class="track-kind">complete</div>
                <div class="track-key">j0k1l2…</div>
              </div>
              <div class="track-content">
                <span class="miss-icon">&#x26a0;</span>
                <span class="miss-text">cassette miss in :replay mode</span>
              </div>
              <div class="track-meta">not recorded</div>
            </div>
          </div>

          <div class="deck-footer">
            <div class="deck-controls">
              <span class="ctrl-btn ctrl-record active">&#x25cf; Record</span>
              <span class="ctrl-btn ctrl-play">&#x25b6; Replay</span>
              <span class="ctrl-btn ctrl-auto">&#x21bb; Auto</span>
            </div>
            <div class="deck-count">3 tracks · 1.2 KB · NDJSON</div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: THREE MODES ============ -->
    <section id="modes">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">Three modes</p>
            <h2>Record, replay, or both.</h2>
            <p class="sub">
              <code>:mode</code> decides what happens on each call. <strong>:auto</strong>
              is the friendly default — records what's missing, replays what it has.
              <strong>:replay</strong> is for CI — never touches the network, and a call
              that isn't on the tape is a hard error. That error is a feature: it tells
              you exactly which call drifted.
            </p>
            <ul class="feature-list">
              <li><strong>:auto (default).</strong> Record new calls, replay recorded ones. Great for writing tapes.</li>
              <li><strong>:replay.</strong> Only play back. A missing call is a hard error that names the request. CI mode.</li>
              <li><strong>:record.</strong> Always call the model and record. Use when you want a fresh tape.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="modes-grid">
              <div class="mode-card">
                <div class="mode-header">
                  <span class="mode-dot mode-dot-auto"></span>
                  <span class="mode-name">:auto</span>
                </div>
                <div class="mode-body">
                  <div class="mode-row"><span class="mode-cond">On tape</span><span class="mode-action">play it back</span></div>
                  <div class="mode-row"><span class="mode-cond">New call</span><span class="mode-action">record it</span></div>
                </div>
                <div class="mode-foot">default · writing tapes</div>
              </div>

              <div class="mode-card">
                <div class="mode-header">
                  <span class="mode-dot mode-dot-replay"></span>
                  <span class="mode-name">:replay</span>
                </div>
                <div class="mode-body">
                  <div class="mode-row"><span class="mode-cond">On tape</span><span class="mode-action">play it back</span></div>
                  <div class="mode-row mode-row-error"><span class="mode-cond">New call</span><span class="mode-action mode-error">error</span></div>
                </div>
                <div class="mode-foot">CI · no network · no key</div>
              </div>

              <div class="mode-card">
                <div class="mode-header">
                  <span class="mode-dot mode-dot-record"></span>
                  <span class="mode-name">:record</span>
                </div>
                <div class="mode-body">
                  <div class="mode-row"><span class="mode-cond">On tape</span><span class="mode-action">call model, re-record</span></div>
                  <div class="mode-row"><span class="mode-cond">New call</span><span class="mode-action">call model, record</span></div>
                </div>
                <div class="mode-foot">fresh tape · re-record</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: CI PATTERN ============ -->
    <section id="ci-pattern">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">CI pattern</p>
            <h2>Record locally. Replay in CI.</h2>
            <p class="sub">
              Run your suite once with an API key to capture tapes. Commit them.
              Then run CI with <code>SEMA_LLM_CASSETTE_MODE=replay</code> — no
              secrets, no network, no flakiness. Any call that isn't on the tape
              fails loudly, so a prompt change can't silently hit a live model.
            </p>
            <ul class="feature-list">
              <li><strong>No API key in CI.</strong> The tape has the answers. Run <code>sema test</code> with zero secrets.</li>
              <li><strong>Deterministic by design.</strong> Same input, same output, every run. LLM tests that pass reliably.</li>
              <li><strong>Drift detection.</strong> Change a prompt or model and the old tape stops matching — the failure names the call.</li>
            </ul>
            <p class="sub" style="margin-top:18px">
              <a href="/docs/llm/cassettes#forcing-replay-across-a-whole-run-ci">CI setup guide &rarr;</a>
            </p>
          </div>
          <div class="feature-visual">
            <div class="term">
              <div class="head">terminal — record &amp; replay</div>
              <div><span class="dollar">$</span> <span class="c-com"># Record: run once with a key</span></div>
              <div><span class="dollar">$</span> SEMA_LLM_CASSETTE=tapes/suite.jsonl \</div>
              <div>&nbsp;&nbsp;semal test/agents.sema</div>
              <div class="out">→ recording 12 LLM calls…</div>
              <div class="ok">✓ wrote tapes/suite.jsonl (4.8 KB)</div>
              <div>&nbsp;</div>
              <div><span class="dollar">$</span> git add tapes/suite.jsonl &amp;&amp; git commit</div>
              <div class="out">→ committed tape</div>
              <div>&nbsp;</div>
              <div><span class="dollar">$</span> <span class="c-com"># CI: replay with no key</span></div>
              <div><span class="dollar">$</span> SEMA_LLM_CASSETTE=tapes/suite.jsonl \</div>
              <div>&nbsp;&nbsp;SEMA_LLM_CASSETTE_MODE=replay \</div>
              <div>&nbsp;&nbsp;semal test/agents.sema</div>
              <div class="out">→ replaying 12 calls from tape…</div>
              <div class="ok">✓ all tests passed (0 API calls)</div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: WHAT GETS SAVED ============ -->
    <section id="whats-saved">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">What's in the file</p>
            <h2>Plain text. Safe to commit.</h2>
            <p class="sub">
              A tape is NDJSON — one JSON object per line, one line per saved call.
              It's diffable, appendable, and reviewable in a pull request. Only the
              <strong>answer</strong> is saved, looked up by a fingerprint of the
              request. Your prompt text, API key, and headers are never written to
              the file.
            </p>
            <ul class="feature-list">
              <li><strong>Only answers.</strong> The model, tokens, and response are saved. Prompts and keys are not.</li>
              <li><strong>NDJSON format.</strong> One line per call. <code>git diff</code> shows exactly what changed when you re-record.</li>
              <li><strong>Versioned.</strong> A <code>"v":1</code> field lets old tapes be migrated if the shape ever changes.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="code-card">
              <div class="code-card-head">
                <span class="t">tapes/suite.jsonl</span>
                <span class="n">3 lines · 1.2 KB</span>
              </div>
              <pre><span class="c-com">{"v":1,"kind":"complete","key":"a1b2…",</span>
<span class="c-com"> </span><span class="c-str">"content"</span>:<span class="c-str">"Hello"</span>,
 <span class="c-str">"model"</span>:<span class="c-str">"gpt-5-mini"</span>,
 <span class="c-str">"prompt_tokens"</span>:12,
 <span class="c-str">"completion_tokens"</span>:1}

{"v":1,"kind":"stream","key":"c3d4…",
 <span class="c-str">"content"</span>:<span class="c-str">"Hi there"</span>,
 <span class="c-str">"chunks"</span>:[<span class="c-str">"Hi"</span>,<span class="c-str">" there"</span>],
 <span class="c-str">"completion_tokens"</span>:2}

{"v":1,"kind":"embed","key":"e5f6…",
 <span class="c-str">"model"</span>:<span class="c-str">"text-embedding-3-small"</span>,
 <span class="c-str">"embeddings"</span>:[[0.01,-0.02,0.03]]}</pre>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: AGENTS & STREAMING ============ -->
    <section id="agents-streaming">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">Agents &amp; streaming</p>
            <h2>Every call type, covered.</h2>
            <p class="sub">
              Cassettes work with <code>llm/complete</code>, <code>llm/chat</code>,
              <code>llm/extract</code>, <code>agent/run</code>, streaming, and
              embeddings. For agents, each turn is saved separately — so a full
              multi-turn run (model &rarr; tool call &rarr; result &rarr; final
              answer) replays exactly. Your tool handlers still run on replay; the
              cassette only stands in for the model's responses.
            </p>
            <ul class="feature-list">
              <li><strong>Agent tool loops.</strong> Each model turn is recorded independently. Tools execute normally on replay — deterministic model output, real tool logic.</li>
              <li><strong>Streaming.</strong> The exact sequence of chunks is saved and replayed in order. A streaming UI behaves identically offline.</li>
              <li><strong>Embeddings.</strong> Vectors are saved byte-for-byte. Similarity scores and vector-store contents are exactly reproducible.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="coverage-grid">
              <div class="coverage-card">
                <div class="coverage-check">&#x2713;</div>
                <div class="coverage-name">llm/complete</div>
                <div class="coverage-desc">answer, model, tokens, finish reason</div>
              </div>
              <div class="coverage-card">
                <div class="coverage-check">&#x2713;</div>
                <div class="coverage-name">llm/chat</div>
                <div class="coverage-desc">multi-turn conversations</div>
              </div>
              <div class="coverage-card">
                <div class="coverage-check">&#x2713;</div>
                <div class="coverage-name">llm/extract</div>
                <div class="coverage-desc">structured results rebuilt from saved answer</div>
              </div>
              <div class="coverage-card">
                <div class="coverage-check">&#x2713;</div>
                <div class="coverage-name">agent/run</div>
                <div class="coverage-desc">each turn saved separately · tools still run</div>
              </div>
              <div class="coverage-card">
                <div class="coverage-check">&#x2713;</div>
                <div class="coverage-name">llm/stream</div>
                <div class="coverage-desc">chunks saved and replayed in order</div>
              </div>
              <div class="coverage-card">
                <div class="coverage-check">&#x2713;</div>
                <div class="coverage-name">llm/embed</div>
                <div class="coverage-desc">vectors saved byte-for-byte</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: HOW IT COMPOSES ============ -->
    <section id="composes">
      <div class="wrap">
        <div class="feature-row">
          <div class="feature-text">
            <p class="kicker">How it composes</p>
            <h2>Slots in, doesn't conflict.</h2>
            <p class="sub">
              A cassette sits just above the real model and below everything else.
              Cost tracking keeps working on replay — the saved answer carries its
              real token counts. Tracing still produces OpenTelemetry spans. The
              response cache is turned off inside a cassette block so the tape
              always answers first.
            </p>
            <ul class="feature-list">
              <li><strong>Cost &amp; budgets.</strong> A replayed call reports real token usage — so budget limits and <code>llm/session-usage</code> behave as if the call happened.</li>
              <li><strong>Tracing.</strong> Replayed calls still produce OTel spans with recorded model and token counts.</li>
              <li><strong>Retries &amp; fallback.</strong> While recording, the normal retry logic wraps the real call. On replay there's nothing to retry.</li>
            </ul>
          </div>
          <div class="feature-visual">
            <div class="compose-diagram">
              <div class="compose-layer compose-layer-app">Your code</div>
              <div class="compose-arrow">&darr;</div>
              <div class="compose-layer compose-layer-cassette">
                <span class="cassette-tag">cassette</span>
                record / replay
              </div>
              <div class="compose-arrow">&darr;</div>
              <div class="compose-layer compose-layer-runtime">
                cost · budgets · tracing · retry · fallback
              </div>
              <div class="compose-arrow">&darr;</div>
              <div class="compose-layer compose-layer-provider">LLM provider (API)</div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ FEATURE: NOTEBOOK INTEGRATION ============ -->
    <section id="notebook-integration">
      <div class="wrap">
        <div class="feature-row reverse">
          <div class="feature-text">
            <p class="kicker">Notebook integration</p>
            <h2>Tapes for notebooks.</h2>
            <p class="sub">
              Record LLM cells once with a key, commit the tape next to the
              <code>.sema-nb</code> file, and the notebook re-runs the same way
              forever — offline, for anyone, in CI. One setup cell turns it on;
              every LLM cell after it records or replays automatically.
            </p>
            <ul class="feature-list">
              <li><strong>One setup cell.</strong> <code>(llm/cassette-load "tapes/nb.jsonl" {:mode :auto})</code> — every cell after it is recorded or replayed.</li>
              <li><strong>Headless replay.</strong> <code>SEMA_LLM_CASSETTE_MODE=replay sema notebook run nb.sema-nb</code> — no key, no network.</li>
              <li><strong>Shareable demos.</strong> Ship the tape with the notebook. Anyone can run it and see the exact same output.</li>
            </ul>
            <p class="sub" style="margin-top:18px">
              <a href="/feature/notebook">Notebook feature page &rarr;</a>
            </p>
          </div>
          <div class="feature-visual">
            <div class="nb-gui mini">
              <div class="nb-body">
                <div class="nb-cell active">
                  <div class="nb-cell-num">[1]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">llm/cassette-load</span><br>
                    &nbsp;&nbsp;<span class="c-str">"tapes/nb.jsonl"</span><br>
                    &nbsp;&nbsp;{<span class="c-kwd">:mode</span> <span class="c-kwd">:auto</span>})
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-meta-inline">cassette loaded · recording</div>
                </div>
                <div class="nb-cell">
                  <div class="nb-cell-num">[2]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">llm/complete</span> <span class="c-str">"Summarize Sema"</span><br>
                    &nbsp;&nbsp;{<span class="c-kwd">:model</span> <span class="c-str">"gpt-5-mini"</span>})
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-value">"A Lisp with LLM primitives in Rust."</div>
                  <div class="out-meta">12 tok · recorded · $0.0002</div>
                </div>
                <div class="nb-cell">
                  <div class="nb-cell-num">[3]</div>
                  <div class="nb-cell-editor">
                    (<span class="c-kw">llm/cassette-save</span>)
                  </div>
                </div>
                <div class="nb-output">
                  <div class="out-meta-inline">wrote tapes/nb.jsonl (0.8 KB)</div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- ============ CTA ============ -->
    <section class="cta">
      <div class="wrap">
        <h2>Record your first tape in seconds.</h2>
        <p class="sub">Wrap your LLM calls in <code>with-cassette</code>, run once, commit the tape.</p>
        <div class="install-stack">
          <div class="install-row">
            <span class="badge">curl</span>
            <span class="install">
              <span class="cmd-text">
                <span class="dollar">$</span>
                <span id="i2">curl -fsSL https://sema-lang.com/install.sh | sh</span>
              </span>
              <button class="copy" @click="copyText('i2', $event)">copy</button>
            </span>
          </div>
          <div class="install-row">
            <span class="badge">cargo</span>
            <span class="install">
              <span class="cmd-text">
                <span class="dollar">$</span>
                <span id="i3">cargo install sema-lang</span>
              </span>
              <button class="copy" @click="copyText('i3', $event)">copy</button>
            </span>
          </div>
          <div class="hero-actions" style="justify-content:center; margin-top:24px">
            <a class="btn btn-gold" href="/docs/llm/cassettes">Cassettes docs</a>
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

/* ---------- tape deck mockup ---------- */
.deck-showcase { padding: 0 0 88px; border-top: none; }

.tape-deck {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 14px;
  overflow: hidden;
  box-shadow: 0 0 0 1px rgba(200, 168, 85, .06), 0 40px 80px -40px rgba(200, 168, 85, .12);
}

.deck-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 12px 18px;
  background: var(--surface);
  border-bottom: 1px solid var(--border-lo);
  font-family: var(--font-mono);
  font-size: 12px;
}

.deck-label {
  display: flex;
  align-items: center;
  gap: 8px;
}

.deck-icon { color: var(--gold); display: flex; align-items: center; }

.deck-title { color: var(--text); font-weight: 500; }

.deck-status {
  display: flex;
  align-items: center;
  gap: 6px;
}

.status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--dim);
}

.status-dot.recording {
  background: #c85555;
  box-shadow: 0 0 6px rgba(200, 85, 85, 0.5);
  animation: pulse 1.5s ease-in-out infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

.status-text {
  font-size: 10.5px;
  color: #c85555;
  font-weight: 500;
  letter-spacing: 0.08em;
}

.deck-body {
  padding: 16px 18px;
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.deck-track {
  display: flex;
  align-items: center;
  gap: 14px;
  padding: 10px 14px;
  background: var(--bg);
  border: 1px solid var(--border-lo);
  border-radius: 8px;
  font-family: var(--font-mono);
  font-size: 12px;
}

.deck-track-miss {
  border-color: rgba(201, 123, 106, 0.25);
  background: rgba(201, 123, 106, 0.03);
}

.track-num {
  color: var(--dim);
  font-size: 11px;
  flex-shrink: 0;
  width: 22px;
}

.track-info {
  display: flex;
  flex-direction: column;
  gap: 1px;
  flex-shrink: 0;
  width: 90px;
}

.track-kind {
  color: var(--gold-bright);
  font-size: 11px;
  font-weight: 500;
}

.track-key {
  color: var(--dim);
  font-size: 10px;
}

.track-content {
  color: var(--text);
  flex-grow: 1;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.track-meta {
  color: var(--dim);
  font-size: 10.5px;
  flex-shrink: 0;
  text-align: right;
}

.miss-icon {
  color: #c97b6a;
  margin-right: 6px;
}

.miss-text {
  color: #c97b6a;
  font-size: 11px;
}

.deck-footer {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 12px 18px;
  background: var(--surface);
  border-top: 1px solid var(--border-lo);
  font-family: var(--font-mono);
  font-size: 11px;
}

.deck-controls {
  display: flex;
  gap: 8px;
}

.ctrl-btn {
  padding: 4px 10px;
  border: 1px solid var(--border);
  border-radius: 5px;
  color: var(--dim);
  font-size: 11px;
}

.ctrl-record.active {
  color: #c85555;
  border-color: rgba(200, 85, 85, 0.3);
  background: rgba(200, 85, 85, 0.06);
}

.ctrl-play { color: #9bb87a; }
.ctrl-auto { color: var(--gold-bright); }

.deck-count { color: var(--dim); }

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

/* ---------- modes grid ---------- */
.modes-grid {
  display: flex;
  flex-direction: column;
  gap: 14px;
}

.mode-card {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 10px;
  overflow: hidden;
}

.mode-header {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 12px 16px;
  border-bottom: 1px solid var(--border-lo);
  font-family: var(--font-mono);
  font-size: 13px;
}

.mode-dot {
  width: 9px;
  height: 9px;
  border-radius: 50%;
  flex-shrink: 0;
}

.mode-dot-auto { background: var(--gold); }
.mode-dot-replay { background: #9bb87a; }
.mode-dot-record { background: #c85555; }

.mode-name { color: var(--text); font-weight: 500; }

.mode-body {
  padding: 8px 16px;
}

.mode-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 6px 0;
  font-family: var(--font-mono);
  font-size: 12px;
  border-bottom: 1px solid var(--border-lo);
}

.mode-row:last-child { border-bottom: none; }

.mode-cond { color: var(--muted); }

.mode-action { color: var(--text); }

.mode-error { color: #c97b6a; }

.mode-foot {
  padding: 8px 16px;
  font-family: var(--font-mono);
  font-size: 10.5px;
  color: var(--dim);
  background: var(--bg);
  border-top: 1px solid var(--border-lo);
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
  font-size: 12px;
  line-height: 1.62;
  padding: 18px 20px;
  overflow-x: auto;
  color: #c9c2b4;
}

/* ---------- coverage grid ---------- */
.coverage-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 12px;
}

.coverage-card {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 16px 18px;
}

.coverage-check {
  color: #9bb87a;
  font-size: 16px;
  margin-bottom: 6px;
}

.coverage-name {
  font-family: var(--font-mono);
  font-size: 13px;
  font-weight: 500;
  color: var(--gold-bright);
  margin-bottom: 4px;
}

.coverage-desc {
  font-size: 12px;
  color: var(--muted);
  line-height: 1.5;
}

/* ---------- compose diagram ---------- */
.compose-diagram {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0;
}

.compose-layer {
  width: 100%;
  text-align: center;
  padding: 16px 20px;
  border-radius: 10px;
  font-family: var(--font-mono);
  font-size: 13px;
  border: 1px solid var(--border);
}

.compose-layer-app {
  background: var(--bg-raise);
  color: var(--text);
  font-weight: 500;
}

.compose-layer-cassette {
  background: var(--gold-fade);
  border-color: var(--gold-line);
  color: var(--gold-bright);
  font-weight: 500;
  position: relative;
}

.cassette-tag {
  display: inline-block;
  font-size: 10px;
  color: var(--gold);
  background: rgba(200, 168, 85, 0.14);
  padding: 2px 7px;
  border-radius: 4px;
  margin-right: 8px;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.compose-layer-runtime {
  background: var(--bg-raise);
  color: var(--muted);
  font-size: 12px;
}

.compose-layer-provider {
  background: var(--bg);
  color: var(--dim);
  font-size: 12px;
  border-style: dashed;
}

.compose-arrow {
  color: var(--dim);
  font-size: 18px;
  padding: 6px 0;
}

/* ---------- notebook mini mockup ---------- */
.nb-gui {
  background: var(--bg-raise);
  border: 1px solid var(--border);
  border-radius: 10px;
  overflow: hidden;
  box-shadow: 0 0 0 1px rgba(200, 168, 85, .04), 0 20px 50px -30px rgba(0, 0, 0, .3);
}

.nb-body {
  padding: 16px 18px;
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.nb-cell {
  display: flex;
  gap: 0;
  width: 100%;
}

.nb-cell-num {
  width: 36px;
  flex-shrink: 0;
  display: flex;
  align-items: flex-start;
  justify-content: center;
  padding-top: 0.5rem;
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
}

.nb-cell-editor {
  flex-grow: 1;
  font-family: var(--font-mono);
  font-size: 12px;
  line-height: 1.6;
  color: var(--text);
  background: var(--bg-editor);
  border: 1px solid var(--border);
  border-radius: 4px;
  padding: 0.45rem 0.75rem;
  white-space: pre;
  overflow-x: auto;
}

.nb-cell.active .nb-cell-editor {
  border-left: 3px solid var(--gold);
  border-top-left-radius: 0;
  border-bottom-left-radius: 0;
}

.nb-output {
  margin-left: 36px;
  margin-bottom: 6px;
}

.out-meta-inline {
  font-family: var(--font-mono);
  font-size: 11px;
  color: var(--dim);
  padding: 0.25rem 0.75rem;
}

.out-value {
  font-family: var(--font-mono);
  font-size: 12px;
  color: var(--gold-bright);
  background: var(--bg-output);
  border: 1px solid var(--border-lo);
  border-left: 2px solid rgba(200, 168, 85, 0.4);
  border-radius: 0 4px 4px 0;
  padding: 0.4rem 0.75rem;
}

.out-meta {
  font-family: var(--font-mono);
  font-size: 10.5px;
  color: var(--dim);
  padding: 0.25rem 0.75rem;
  margin-top: 2px;
}

/* ---------- responsive ---------- */
@media (max-width: 880px) {
  .hero { padding: 72px 0 48px; }

  .feature-row, .feature-row.reverse {
    grid-template-columns: 1fr;
  }
  .feature-row.reverse .feature-text { order: unset; }
  .feature-row.reverse .feature-visual { order: unset; }

  .coverage-grid { grid-template-columns: 1fr; }

  .deck-track {
    flex-wrap: wrap;
    gap: 8px;
  }
  .track-info { width: auto; flex-direction: row; gap: 8px; }
  .track-meta { width: 100%; text-align: left; }
}
</style>
