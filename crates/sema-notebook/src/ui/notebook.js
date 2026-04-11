/* Sema Notebook — Client-side JavaScript */
const Notebook = (() => {
  let cells = [];
  let focusedCellId = null;
  let canUndo = false;
  let shiftEnterUsed = localStorage.getItem('sema-nb-shift-enter-used') === 'true';

  function updateUndoState(flag) {
    canUndo = !!flag;
    var btn = document.getElementById('undo-btn');
    if (btn) btn.disabled = !canUndo;
  }

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

  function escapeHtml(str) {
    return str.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
  }

  function autoResize(textarea) {
    textarea.style.height = 'auto';
    textarea.style.height = textarea.scrollHeight + 'px';
  }

  // SVG icons for cell actions
  const icons = {
    run: '<svg viewBox="0 0 16 16" fill="currentColor"><polygon points="4,2 13,8 4,14"/></svg>',
    delete: '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"><polyline points="2,4 14,4"/><path d="M5 4V2h6v2"/><path d="M3 4l1 10h8l1-10"/></svg>',
    moveUp: '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><line x1="8" y1="13" x2="8" y2="3"/><polyline points="4,7 8,3 12,7"/></svg>',
    moveDown: '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><line x1="8" y1="3" x2="8" y2="13"/><polyline points="4,9 8,13 12,9"/></svg>',
  };

  function renderMarkdown(src) {
    let html = escapeHtml(src);
    html = html.replace(/^#### (.+)$/gm, '<h4>$1</h4>');
    html = html.replace(/^### (.+)$/gm, '<h3>$1</h3>');
    html = html.replace(/^## (.+)$/gm, '<h2>$1</h2>');
    html = html.replace(/^# (.+)$/gm, '<h1>$1</h1>');
    html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');
    html = html.replace(/\*(.+?)\*/g, '<em>$1</em>');
    html = html.replace(/`([^`]+)`/g, '<code>$1</code>');
    html = html.replace(/```[\w]*\n([\s\S]*?)```/g, '<pre><code>$1</code></pre>');
    html = html.replace(/^- (.+)$/gm, '<li>$1</li>');
    html = html.replace(/(<li>.*<\/li>\n?)+/g, function(m) { return '<ul>' + m + '</ul>'; });
    html = html.replace(/^(?!<[hup]|<li|<ul|<ol|<pre)(.+)$/gm, '<p>$1</p>');
    return html;
  }

  function renderOutputHTML(output) {
    if (!output) return '';
    const isError = output.output_type === 'error';
    const isStdout = output.output_type === 'stdout';
    const isValue = output.output_type === 'value';

    // Skip empty value outputs (e.g. nil return from println)
    if (isValue && !output.content) return '';

    const cls = isError ? 'cell-output error' : 'cell-output';

    let displayContent = '';
    if (isError) {
      displayContent = escapeHtml(output.content);
    } else if (isStdout) {
      displayContent = escapeHtml(output.content);
    } else if (output.content) {
      displayContent = '<span style="color:var(--gold)">' + escapeHtml(output.content) + '</span>';
    }

    let metaParts = [];
    if (output.meta) {
      if (output.meta.duration_ms != null) metaParts.push(output.meta.duration_ms + 'ms');
      if (output.meta.cost_usd != null) metaParts.push('$' + output.meta.cost_usd.toFixed(4));
    }
    const metaText = metaParts.join(' \u00b7 ');

    const testType = isError ? 'error' : isStdout ? 'stdout' : 'value';
    return '<div class="' + cls + '" data-testid="cell-output-' + testType + '">' +
      '<div class="cell-output-header" onclick="Notebook.toggleOutput(this)">' +
        '<span class="output-chevron" data-testid="output-chevron">\u25bc</span>' +
        '<span class="output-meta" data-testid="output-meta">' + metaText + '</span>' +
      '</div>' +
      '<div class="cell-output-content" data-testid="output-content">' + displayContent +
        (isError && canUndo ? '<button class="undo-inline-btn" onclick="Notebook.undo()">Undo cell</button>' : '') +
      '</div>' +
    '</div>';
  }

  function renderCell(cell) {
    const isCode = cell.cell_type === 'code';
    const isFocused = cell.id === focusedCellId;
    const isStale = cell.stale;

    let classes = 'cell';
    if (isFocused) classes += ' focused';
    if (isStale) classes += ' stale';

    // Gutter
    let gutterContent = '';
    if (!isCode) {
      gutterContent = '<span data-testid="gutter-markdown" style="color:var(--text-dim)">M</span>';
    } else if (cell._loading) {
      gutterContent = '<div class="spinner" data-testid="gutter-spinner"></div>';
    } else if (cell.cell_number != null) {
      const sc = isStale ? ' stale' : '';
      const ss = isStale ? '*' : '';
      gutterContent = '<span class="exec-count' + sc + '" data-testid="gutter-exec-count">[' + cell.cell_number + ss + ']</span>';
    } else {
      gutterContent = '<span data-testid="gutter-empty" style="color:var(--text-dim)">[ ]</span>';
    }

    // Actions
    let actions = '<div class="cell-actions" data-testid="cell-actions">';
    if (isCode) {
      actions += '<button type="button" class="cell-action-btn" title="Run" data-testid="btn-cell-run" onclick="Notebook.evalCell(\'' + cell.id + '\')">' + icons.run + '</button>';
    }
    actions += '<button type="button" class="cell-action-btn" title="Move up" data-testid="btn-cell-move-up" onclick="Notebook.moveCell(\'' + cell.id + '\',-1)">' + icons.moveUp + '</button>';
    actions += '<button type="button" class="cell-action-btn" title="Move down" data-testid="btn-cell-move-down" onclick="Notebook.moveCell(\'' + cell.id + '\',1)">' + icons.moveDown + '</button>';
    actions += '<button type="button" class="cell-action-btn delete" title="Delete" data-testid="btn-cell-delete" onclick="Notebook.deleteCell(\'' + cell.id + '\')">' + icons.delete + '</button>';
    actions += '</div>';

    // Body content
    let bodyContent = '';
    if (!isCode && cell._rendered) {
      bodyContent = '<div class="markdown-rendered" data-testid="markdown-rendered" onclick="Notebook.editMarkdown(\'' + cell.id + '\')">' + renderMarkdown(cell.source) + '</div>';
    } else {
      const rows = Math.max(cell.source.split('\n').length, 1);
      const hint = (!shiftEnterUsed && isFocused) ? '<span class="shift-enter-hint" data-testid="shift-enter-hint">Shift+Enter</span>' : '';
      bodyContent = '<div class="cell-editor" data-testid="cell-editor">' +
        '<textarea rows="' + rows + '" spellcheck="false" data-id="' + cell.id + '"' +
        ' data-testid="cell-textarea"' +
        ' onfocus="Notebook.focusCell(\'' + cell.id + '\')"' +
        ' onkeydown="Notebook.onKeyDown(event,\'' + cell.id + '\')"' +
        ' oninput="Notebook.onEdit(this,\'' + cell.id + '\')"' +
        '>' + escapeHtml(cell.source) + '</textarea>' +
        hint + '</div>';
    }

    // Output
    let outputHTML = '';
    if (cell.rendered_outputs && isCode) {
      outputHTML = cell.rendered_outputs.map(renderOutputHTML).join('');
    }

    return '<div class="' + classes + '" id="cell-' + cell.id + '" data-id="' + cell.id + '" data-testid="cell" data-cell-type="' + cell.cell_type + '">' +
      '<div class="cell-gutter" data-testid="cell-gutter">' + gutterContent + '</div>' +
      '<div class="cell-body" data-testid="cell-body">' + actions + bodyContent + outputHTML + '</div>' +
    '</div>';
  }

  function renderDivider(afterId) {
    return '<div class="cell-divider" data-testid="cell-divider" onmouseenter="this.classList.add(\'visible\')" onmouseleave="Notebook.closeDivider(this)">' +
      '<div class="cell-divider-line"></div>' +
      '<button type="button" class="add-cell-btn" data-testid="btn-add-cell" onclick="Notebook.toggleDropdown(event,\'' + afterId + '\')">+</button>' +
      '<div class="add-cell-dropdown" data-testid="add-cell-dropdown" data-after="' + afterId + '">' +
        '<button type="button" data-testid="btn-insert-code" onclick="Notebook.insertCell(\'code\',\'' + afterId + '\')">Code</button>' +
        '<button type="button" data-testid="btn-insert-markdown" onclick="Notebook.insertCell(\'markdown\',\'' + afterId + '\')">Markdown</button>' +
      '</div>' +
    '</div>';
  }

  function renderAllCells() {
    const container = document.getElementById('cells-inner');

    if (cells.length === 0) {
      container.innerHTML = '<div class="empty-state" data-testid="empty-state">' +
        '<span class="empty-state-text">Empty notebook</span>' +
        '<div class="empty-state-actions">' +
          '<button type="button" class="pill-btn" data-testid="btn-empty-add-code" onclick="Notebook.addCell(\'code\')">+ Code</button>' +
          '<button type="button" class="pill-btn" data-testid="btn-empty-add-markdown" onclick="Notebook.addCell(\'markdown\')">+ Markdown</button>' +
        '</div></div>';
      updateStatus();
      return;
    }

    // Auto-render markdown cells that haven't been edited
    cells.forEach(function(c) {
      if (c.cell_type === 'markdown' && c._rendered === undefined && c.source) {
        c._rendered = true;
      }
    });

    let html = renderDivider('top');
    cells.forEach(function(cell) {
      html += renderCell(cell);
      html += renderDivider(cell.id);
    });
    container.innerHTML = html;

    // Auto-resize all textareas
    container.querySelectorAll('textarea').forEach(autoResize);
    updateStatus();
  }

  function updateStatus() {
    const el = document.getElementById('cell-count');
    if (el) el.textContent = cells.length + ' cell' + (cells.length !== 1 ? 's' : '');
  }

  function focusCell(id) {
    focusedCellId = id;
    renderAllCells();
    const ta = document.querySelector('#cell-' + id + ' textarea');
    if (ta) ta.focus();
  }

  function editMarkdown(id) {
    const cell = cells.find(function(c) { return c.id === id; });
    if (cell) {
      cell._rendered = false;
      focusedCellId = id;
      renderAllCells();
      const ta = document.querySelector('#cell-' + id + ' textarea');
      if (ta) ta.focus();
    }
  }

  async function load() {
    try {
      const data = await api('GET', '/api/notebook');
      document.getElementById('notebook-title').value = data.title || 'Untitled';
      cells = data.cells || [];
      updateUndoState(data.can_undo);
      renderAllCells();
    } catch (e) {
      console.error('Failed to load notebook:', e);
    }
  }

  async function addCell(type, afterId) {
    try {
      const body = { type: type, source: '' };
      if (afterId) body.after = afterId;
      const data = await api('POST', '/api/cells', body);
      await load();
      focusedCellId = data.id;
      renderAllCells();
      const el = document.querySelector('#cell-' + data.id + ' textarea');
      if (el) el.focus();
    } catch (e) {
      console.error('Failed to create cell:', e);
    }
  }

  async function insertCell(type, afterId) {
    closeAllDropdowns();
    const body = { type: type, source: '' };
    if (afterId && afterId !== 'top') body.after = afterId;
    try {
      const data = await api('POST', '/api/cells', body);
      await load();
      focusedCellId = data.id;
      renderAllCells();
      const el = document.querySelector('#cell-' + data.id + ' textarea');
      if (el) el.focus();
    } catch (e) {
      console.error('Failed to insert cell:', e);
    }
  }

  async function evalCell(id) {
    const cell = cells.find(function(c) { return c.id === id; });
    if (!cell) return;

    // Sync source
    const textarea = document.querySelector('#cell-' + id + ' textarea');
    if (textarea) {
      try {
        await api('POST', '/api/cells/' + id, { source: textarea.value });
      } catch (e) { /* ignore */ }
    }

    cell._loading = true;
    renderAllCells();

    try {
      await api('POST', '/api/cells/' + id + '/eval');
      // Advance focus to next cell
      const idx = cells.findIndex(function(c) { return c.id === id; });
      if (idx < cells.length - 1) {
        focusedCellId = cells[idx + 1].id;
      }
      shiftEnterUsed = true;
      localStorage.setItem('sema-nb-shift-enter-used', 'true');
      await load();
    } catch (e) {
      cell._loading = false;
      renderAllCells();
      console.error('Eval failed:', e);
    }
  }

  async function evalCellStay(id) {
    const cell = cells.find(function(c) { return c.id === id; });
    if (!cell) return;

    const textarea = document.querySelector('#cell-' + id + ' textarea');
    if (textarea) {
      try {
        await api('POST', '/api/cells/' + id, { source: textarea.value });
      } catch (e) { /* ignore */ }
    }

    cell._loading = true;
    renderAllCells();

    try {
      await api('POST', '/api/cells/' + id + '/eval');
      focusedCellId = id;
      await load();
    } catch (e) {
      cell._loading = false;
      renderAllCells();
      console.error('Eval failed:', e);
    }
  }

  async function evalAll() {
    const sources = [];
    document.querySelectorAll('.cell-editor textarea').forEach(function(ta) {
      if (ta.dataset.id) sources.push([ta.dataset.id, ta.value]);
    });
    try {
      await api('POST', '/api/eval-all', { sources: sources });
      await load();
    } catch (e) {
      console.error('Eval all failed:', e);
    }
  }

  async function deleteCell(id) {
    try {
      await api('DELETE', '/api/cells/' + id);
      if (focusedCellId === id) focusedCellId = null;
      await load();
    } catch (e) {
      console.error('Delete failed:', e);
    }
  }

  async function moveCell(id, dir) {
    const idx = cells.findIndex(function(c) { return c.id === id; });
    const newIdx = idx + dir;
    if (newIdx < 0 || newIdx >= cells.length) return;

    const ids = cells.map(function(c) { return c.id; });
    const tmp = ids[idx];
    ids[idx] = ids[newIdx];
    ids[newIdx] = tmp;

    try {
      await api('POST', '/api/cells/reorder', { cell_ids: ids });
      await load();
    } catch (e) {
      console.error('Move failed:', e);
    }
  }

  async function save() {
    try {
      await api('POST', '/api/save');
      const btn = document.querySelector('.toolbar-btn[title="Save notebook"]');
      if (btn) {
        btn.style.color = 'var(--success)';
        setTimeout(function() { btn.style.color = ''; }, 600);
      }
    } catch (e) {
      alert('Save failed: ' + e.message);
    }
  }

  async function undo() {
    try {
      var data = await api('POST', '/api/undo');
      updateUndoState(data.can_undo);
      await load();
    } catch (e) {
      console.error('Undo failed:', e);
    }
  }

  async function reset() {
    if (!confirm('Reset the environment? All cell outputs will be cleared.')) return;
    try {
      await api('POST', '/api/reset');
      updateUndoState(false);
      await load();
    } catch (e) {
      console.error('Reset failed:', e);
    }
  }

  function onEdit(textarea, cellId) {
    autoResize(textarea);
    const cell = cells.find(function(c) { return c.id === cellId; });
    if (cell) cell.source = textarea.value;
  }

  function onKeyDown(event, cellId) {
    if (event.key === 'Enter' && event.shiftKey) {
      event.preventDefault();
      const cell = cells.find(function(c) { return c.id === cellId; });
      if (cell && cell.cell_type === 'markdown') {
        cell._rendered = true;
        renderAllCells();
      } else {
        evalCell(cellId);
      }
      return;
    }
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      evalCellStay(cellId);
      return;
    }
    if (event.key === 'Tab' && !event.shiftKey) {
      event.preventDefault();
      const ta = event.target;
      const start = ta.selectionStart;
      ta.value = ta.value.substring(0, start) + '  ' + ta.value.substring(ta.selectionEnd);
      ta.selectionStart = ta.selectionEnd = start + 2;
      const cell = cells.find(function(c) { return c.id === cellId; });
      if (cell) cell.source = ta.value;
      autoResize(ta);
    }
    if (event.key === 's' && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      save();
    }
    if (event.key === 'Escape') {
      event.target.blur();
      focusedCellId = null;
      renderAllCells();
    }
  }

  function toggleOutput(header) {
    const chevron = header.querySelector('.output-chevron');
    const content = header.nextElementSibling;
    if (chevron) chevron.classList.toggle('collapsed');
    if (content) content.classList.toggle('collapsed');
  }

  function toggleDropdown(event, afterId) {
    event.stopPropagation();
    const divider = event.target.closest('.cell-divider');
    const dropdown = divider ? divider.querySelector('.add-cell-dropdown') : null;
    closeAllDropdowns();
    if (dropdown) dropdown.classList.add('open');
  }

  function closeAllDropdowns() {
    document.querySelectorAll('.add-cell-dropdown.open').forEach(function(d) {
      d.classList.remove('open');
    });
  }

  function closeDivider(el) {
    const dropdown = el.querySelector('.add-cell-dropdown');
    if (!dropdown || !dropdown.classList.contains('open')) {
      el.classList.remove('visible');
    }
  }

  // Close dropdowns on outside click
  document.addEventListener('click', function(e) {
    if (!e.target.closest('.add-cell-btn') && !e.target.closest('.add-cell-dropdown')) {
      closeAllDropdowns();
    }
  });

  document.addEventListener('DOMContentLoaded', load);

  return {
    addCell: addCell,
    insertCell: insertCell,
    evalCell: evalCell,
    evalAll: evalAll,
    undo: undo,
    deleteCell: deleteCell,
    moveCell: moveCell,
    save: save,
    reset: reset,
    onEdit: onEdit,
    onKeyDown: onKeyDown,
    focusCell: focusCell,
    editMarkdown: editMarkdown,
    toggleOutput: toggleOutput,
    toggleDropdown: toggleDropdown,
    closeDivider: closeDivider,
  };
})();
