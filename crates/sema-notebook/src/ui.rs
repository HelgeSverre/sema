//! Embedded browser UI for the notebook.
//!
//! The UI is served as inline HTML/CSS/JS from the binary. This makes
//! deployment simple (single binary) while keeping the UI code in
//! separate logical sections that can be replaced wholesale.
//!
//! To swap the UI: replace the contents of this module (or point the
//! server at a different static asset directory).

/// Return the main HTML page.
pub fn index_html() -> String {
    r##"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Sema Notebook</title>
<link rel="stylesheet" href="/ui/style.css">
</head>
<body>
<div id="app">
  <header id="toolbar">
    <div class="toolbar-left">
      <h1 class="logo">Sema <span class="logo-sub">Notebook</span></h1>
    </div>
    <div class="toolbar-center" id="notebook-title">Untitled</div>
    <div class="toolbar-right">
      <button onclick="Notebook.addCell('code')" title="Add code cell">+ Code</button>
      <button onclick="Notebook.addCell('markdown')" title="Add markdown cell">+ Markdown</button>
      <button onclick="Notebook.evalAll()" title="Run all cells">Run All</button>
      <button onclick="Notebook.save()" title="Save notebook">Save</button>
      <button onclick="Notebook.reset()" title="Reset environment">Reset</button>
    </div>
  </header>
  <main id="cells"></main>
</div>
<script src="/ui/notebook.js"></script>
</body>
</html>"##
        .to_string()
}

/// Serve a UI asset by path. Returns (content, content_type).
pub fn asset(path: &str) -> Option<(String, String)> {
    match path {
        "style.css" => Some((css().to_string(), "text/css".to_string())),
        "notebook.js" => Some((js().to_string(), "application/javascript".to_string())),
        _ => None,
    }
}

fn css() -> &'static str {
    r##"/* Sema Notebook — Stylesheet */
:root {
  --bg: #1a1a2e;
  --bg-cell: #16213e;
  --bg-cell-hover: #1a2744;
  --bg-output: #0f3460;
  --bg-input: #1a1a2e;
  --text: #e0e0e0;
  --text-dim: #8899aa;
  --accent: #e94560;
  --accent2: #0ea5e9;
  --border: #2a3a5a;
  --success: #34d399;
  --error: #f87171;
  --stale: #fbbf24;
  --font-mono: 'JetBrains Mono', 'Fira Code', 'SF Mono', 'Cascadia Code', monospace;
  --font-sans: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
  --radius: 8px;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  background: var(--bg);
  color: var(--text);
  font-family: var(--font-sans);
  line-height: 1.6;
}

#app {
  max-width: 960px;
  margin: 0 auto;
  padding: 0 16px;
}

/* Toolbar */
#toolbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 12px 0;
  border-bottom: 1px solid var(--border);
  margin-bottom: 24px;
  position: sticky;
  top: 0;
  background: var(--bg);
  z-index: 100;
}

.logo {
  font-size: 18px;
  font-weight: 700;
  color: var(--accent);
}
.logo-sub {
  color: var(--text-dim);
  font-weight: 400;
}

.toolbar-center {
  font-size: 14px;
  color: var(--text-dim);
}

.toolbar-right {
  display: flex;
  gap: 8px;
}

button {
  background: var(--bg-cell);
  color: var(--text);
  border: 1px solid var(--border);
  padding: 6px 14px;
  border-radius: var(--radius);
  cursor: pointer;
  font-size: 13px;
  font-family: var(--font-sans);
  transition: background 0.15s, border-color 0.15s;
}
button:hover {
  background: var(--bg-cell-hover);
  border-color: var(--accent2);
}

/* Cells */
#cells {
  padding-bottom: 120px;
}

.cell {
  margin-bottom: 16px;
  border: 1px solid var(--border);
  border-radius: var(--radius);
  overflow: hidden;
  transition: border-color 0.2s;
}
.cell:focus-within {
  border-color: var(--accent2);
}
.cell.stale {
  border-color: var(--stale);
}

.cell-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 6px 12px;
  background: var(--bg-cell);
  border-bottom: 1px solid var(--border);
  font-size: 12px;
  color: var(--text-dim);
}

.cell-badge {
  display: flex;
  align-items: center;
  gap: 6px;
}

.cell-number {
  background: var(--accent2);
  color: #fff;
  padding: 1px 8px;
  border-radius: 4px;
  font-size: 11px;
  font-weight: 600;
  font-family: var(--font-mono);
}

.cell-type {
  text-transform: uppercase;
  font-size: 10px;
  letter-spacing: 0.5px;
}

.cell-actions {
  display: flex;
  gap: 4px;
  opacity: 0;
  transition: opacity 0.15s;
}
.cell:hover .cell-actions {
  opacity: 1;
}

.cell-actions button {
  padding: 2px 8px;
  font-size: 11px;
  border: none;
  background: transparent;
  color: var(--text-dim);
}
.cell-actions button:hover {
  color: var(--text);
  background: var(--bg-cell-hover);
}

.cell-actions button.run-btn {
  color: var(--success);
}

/* Editor */
.cell-editor {
  width: 100%;
  min-height: 60px;
  padding: 12px 16px;
  background: var(--bg-input);
  color: var(--text);
  border: none;
  resize: vertical;
  font-family: var(--font-mono);
  font-size: 14px;
  line-height: 1.5;
  outline: none;
  tab-size: 2;
}

.cell-editor.markdown-editor {
  font-family: var(--font-sans);
}

/* Output */
.cell-output {
  padding: 10px 16px;
  background: var(--bg-output);
  font-family: var(--font-mono);
  font-size: 13px;
  white-space: pre-wrap;
  word-break: break-word;
  border-top: 1px solid var(--border);
}

.cell-output.error {
  color: var(--error);
  background: #1a0a0a;
}

.cell-output.empty {
  display: none;
}

.cell-meta {
  display: flex;
  gap: 12px;
  padding: 4px 16px;
  background: var(--bg-output);
  font-size: 11px;
  color: var(--text-dim);
  border-top: 1px solid rgba(255,255,255,0.05);
}

.stale-badge {
  color: var(--stale);
  font-size: 11px;
  font-weight: 600;
}

/* Markdown rendered */
.markdown-preview {
  padding: 12px 16px;
  line-height: 1.7;
}
.markdown-preview h1, .markdown-preview h2, .markdown-preview h3 {
  margin: 0.5em 0 0.25em;
  color: var(--text);
}
.markdown-preview code {
  background: var(--bg-cell);
  padding: 2px 6px;
  border-radius: 3px;
  font-family: var(--font-mono);
  font-size: 0.9em;
}
.markdown-preview pre code {
  display: block;
  padding: 12px;
  overflow-x: auto;
}

/* Loading spinner */
.cell-loading {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 10px 16px;
  color: var(--accent2);
  font-size: 13px;
}
.spinner {
  width: 14px;
  height: 14px;
  border: 2px solid var(--border);
  border-top-color: var(--accent2);
  border-radius: 50%;
  animation: spin 0.6s linear infinite;
}
@keyframes spin { to { transform: rotate(360deg); } }

/* Add cell button between cells */
.add-cell-divider {
  display: flex;
  justify-content: center;
  padding: 4px 0;
  opacity: 0;
  transition: opacity 0.15s;
}
.add-cell-divider:hover {
  opacity: 1;
}
.add-cell-divider button {
  font-size: 11px;
  padding: 2px 12px;
  border: 1px dashed var(--border);
  background: transparent;
  color: var(--text-dim);
}
.add-cell-divider button:hover {
  border-color: var(--accent2);
  color: var(--accent2);
}

/* Responsive */
@media (max-width: 600px) {
  #toolbar { flex-wrap: wrap; gap: 8px; }
  .toolbar-right { flex-wrap: wrap; }
  #app { padding: 0 8px; }
}
"##
}

fn js() -> &'static str {
    r##"/* Sema Notebook — Client-side JavaScript */
const Notebook = (() => {
  let cells = [];

  async function api(method, path, body) {
    const opts = { method, headers: { 'Content-Type': 'application/json' } };
    if (body !== undefined) opts.body = JSON.stringify(body);
    const res = await fetch(path, opts);
    if (!res.ok) {
      const text = await res.text();
      throw new Error(text || res.statusText);
    }
    return res.json();
  }

  function autoResize(textarea) {
    textarea.style.height = 'auto';
    textarea.style.height = textarea.scrollHeight + 'px';
  }

  function renderOutputHTML(output) {
    if (!output || !output.content) return '';
    const escaped = escapeHtml(output.content);
    const cls = output.output_type === 'error' ? 'cell-output error' : 'cell-output';
    let meta = '';
    if (output.meta) {
      const parts = [];
      if (output.meta.duration_ms != null) parts.push(`${output.meta.duration_ms}ms`);
      if (output.meta.cost_usd != null) parts.push(`$${output.meta.cost_usd.toFixed(4)}`);
      if (output.meta.requires_reeval) parts.push('requires re-eval');
      if (parts.length) meta = `<div class="cell-meta">${parts.join(' · ')}</div>`;
    }
    return `<div class="${cls}">${escaped}</div>${meta}`;
  }

  function escapeHtml(str) {
    return str.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
  }

  function renderCell(cell) {
    const isCode = cell.cell_type === 'code';
    const staleClass = cell.stale ? ' stale' : '';
    const numberBadge = cell.cell_number != null
      ? `<span class="cell-number">In [${cell.cell_number}]</span>` : '';
    const typeLabel = isCode ? 'code' : 'md';

    let outputHTML = '';
    if (cell.rendered_outputs) {
      outputHTML = cell.rendered_outputs.map(renderOutputHTML).join('');
    }

    const staleBadge = cell.stale ? '<span class="stale-badge">stale</span>' : '';

    return `
      <div class="cell${staleClass}" id="cell-${cell.id}" data-id="${cell.id}">
        <div class="cell-header">
          <div class="cell-badge">
            ${numberBadge}
            <span class="cell-type">${typeLabel}</span>
            ${staleBadge}
          </div>
          <div class="cell-actions">
            ${isCode ? `<button class="run-btn" onclick="Notebook.evalCell('${cell.id}')">Run</button>` : ''}
            <button onclick="Notebook.deleteCell('${cell.id}')">Delete</button>
          </div>
        </div>
        <textarea
          class="cell-editor${isCode ? '' : ' markdown-editor'}"
          data-id="${cell.id}"
          spellcheck="false"
          oninput="Notebook.onEdit(this)"
          onkeydown="Notebook.onKeyDown(event, '${cell.id}')"
        >${escapeHtml(cell.source)}</textarea>
        <div class="cell-output-container" id="output-${cell.id}">${outputHTML}</div>
      </div>
    `;
  }

  function renderAllCells() {
    const container = document.getElementById('cells');
    container.innerHTML = cells.map(renderCell).join('');
    // Auto-resize all textareas
    container.querySelectorAll('.cell-editor').forEach(autoResize);
  }

  async function load() {
    try {
      const data = await api('GET', '/api/notebook');
      document.getElementById('notebook-title').textContent = data.title || 'Untitled';
      cells = data.cells || [];
      renderAllCells();
    } catch (e) {
      console.error('Failed to load notebook:', e);
    }
  }

  async function addCell(type, afterId) {
    try {
      const body = { type, source: '' };
      if (afterId) body.after = afterId;
      const data = await api('POST', '/api/cells', body);
      // Reload full state to get correct numbering
      await load();
      // Focus the new cell
      const el = document.querySelector(`#cell-${data.id} .cell-editor`);
      if (el) el.focus();
    } catch (e) {
      console.error('Failed to create cell:', e);
    }
  }

  async function evalCell(id) {
    const outputContainer = document.getElementById(`output-${id}`);
    if (outputContainer) {
      outputContainer.innerHTML = '<div class="cell-loading"><div class="spinner"></div>Evaluating...</div>';
    }

    // First, update the source on the server
    const textarea = document.querySelector(`#cell-${id} .cell-editor`);
    if (textarea) {
      try {
        await api('POST', `/api/cells/${id}`, { source: textarea.value });
      } catch (e) { /* ignore update error, eval will use latest */ }
    }

    try {
      const result = await api('POST', `/api/cells/${id}/eval`);
      // Re-load to get stale markers and updated numbering
      await load();
    } catch (e) {
      if (outputContainer) {
        outputContainer.innerHTML = `<div class="cell-output error">${escapeHtml(e.message)}</div>`;
      }
    }
  }

  async function evalAll() {
    try {
      // First sync all cell sources
      const textareas = document.querySelectorAll('.cell-editor');
      for (const ta of textareas) {
        const id = ta.dataset.id;
        if (id) {
          await api('POST', `/api/cells/${id}`, { source: ta.value });
        }
      }
      await api('POST', '/api/eval-all');
      await load();
    } catch (e) {
      console.error('Eval all failed:', e);
    }
  }

  async function deleteCell(id) {
    try {
      await api('POST', `/api/cells/${id}/delete`);
      await load();
    } catch (e) {
      console.error('Delete failed:', e);
    }
  }

  async function save() {
    try {
      await api('POST', '/api/save');
    } catch (e) {
      alert('Save failed: ' + e.message);
    }
  }

  async function reset() {
    if (!confirm('Reset the environment? All cell outputs will be cleared.')) return;
    try {
      await api('POST', '/api/reset');
      await load();
    } catch (e) {
      console.error('Reset failed:', e);
    }
  }

  function onEdit(textarea) {
    autoResize(textarea);
  }

  function onKeyDown(event, cellId) {
    // Shift+Enter: evaluate cell
    if (event.key === 'Enter' && event.shiftKey) {
      event.preventDefault();
      evalCell(cellId);
      return;
    }
    // Tab: insert 2 spaces
    if (event.key === 'Tab' && !event.shiftKey) {
      event.preventDefault();
      const ta = event.target;
      const start = ta.selectionStart;
      const end = ta.selectionEnd;
      ta.value = ta.value.substring(0, start) + '  ' + ta.value.substring(end);
      ta.selectionStart = ta.selectionEnd = start + 2;
    }
    // Ctrl/Cmd+S: save
    if (event.key === 's' && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      save();
    }
  }

  // Initialize on load
  document.addEventListener('DOMContentLoaded', load);

  return { addCell, evalCell, evalAll, deleteCell, save, reset, onEdit, onKeyDown };
})();
"##
}
