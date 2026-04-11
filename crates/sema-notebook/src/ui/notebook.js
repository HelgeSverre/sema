/* Sema Notebook — Alpine.js Component */
document.addEventListener('alpine:init', () => {
  Alpine.data('notebook', () => ({
    // ── State ──
    cells: [],
    title: 'Untitled',
    focusedCellId: null,
    canUndo: false,
    shiftEnterUsed: localStorage.getItem('sema-nb-shift-enter-used') === 'true',
    openDropdownId: null,
    saveFeedback: false,

    // ── Lifecycle ──
    init() {
      this.load();
      // Close dropdowns on outside click
      document.addEventListener('click', (e) => {
        if (!e.target.closest('.add-cell-btn') && !e.target.closest('.add-cell-dropdown')) {
          this.openDropdownId = null;
        }
      });
    },

    // ── API helper ──
    async api(method, path, body) {
      const opts = { method, headers: { 'Content-Type': 'application/json' } };
      if (body !== undefined) opts.body = JSON.stringify(body);
      const res = await fetch(path, opts);
      if (!res.ok) {
        const text = await res.text();
        throw new Error(text || res.statusText);
      }
      return res.json();
    },

    // ── Data loading ──
    async load() {
      try {
        const data = await this.api('GET', '/api/notebook');
        this.title = data.title || 'Untitled';
        this.canUndo = !!data.can_undo;
        // Preserve _rendered flags for markdown cells
        const oldRendered = {};
        this.cells.forEach(c => { if (c._rendered !== undefined) oldRendered[c.id] = c._rendered; });
        this.cells = (data.cells || []).map(c => {
          // Auto-render markdown cells with content
          if (c.cell_type === 'markdown' && c.source) {
            c._rendered = oldRendered[c.id] !== undefined ? oldRendered[c.id] : true;
          }
          return c;
        });
      } catch (e) {
        console.error('Failed to load notebook:', e);
      }
    },

    // ── Cell evaluation ──
    async evalCell(id) {
      const cell = this.cells.find(c => c.id === id);
      if (!cell) return;
      // Sync source to server
      try { await this.api('POST', '/api/cells/' + id, { source: cell.source }); } catch (e) { /* ignore */ }
      cell._loading = true;
      try {
        await this.api('POST', '/api/cells/' + id + '/eval');
        const idx = this.cells.findIndex(c => c.id === id);
        if (idx < this.cells.length - 1) this.focusedCellId = this.cells[idx + 1].id;
        this.shiftEnterUsed = true;
        localStorage.setItem('sema-nb-shift-enter-used', 'true');
        await this.load();
      } catch (e) {
        cell._loading = false;
        console.error('Eval failed:', e);
      }
    },

    async evalCellStay(id) {
      const cell = this.cells.find(c => c.id === id);
      if (!cell) return;
      try { await this.api('POST', '/api/cells/' + id, { source: cell.source }); } catch (e) { /* ignore */ }
      cell._loading = true;
      try {
        await this.api('POST', '/api/cells/' + id + '/eval');
        this.focusedCellId = id;
        await this.load();
      } catch (e) {
        cell._loading = false;
        console.error('Eval failed:', e);
      }
    },

    async evalAll() {
      const sources = this.cells
        .filter(c => c.cell_type === 'code')
        .map(c => [c.id, c.source]);
      try {
        await this.api('POST', '/api/eval-all', { sources });
        await this.load();
      } catch (e) {
        console.error('Eval all failed:', e);
      }
    },

    // ── Cell management ──
    async addCell(type, afterId) {
      try {
        const body = { type, source: '' };
        if (afterId) body.after = afterId;
        const data = await this.api('POST', '/api/cells', body);
        await this.load();
        this.focusedCellId = data.id;
        this.$nextTick(() => {
          const el = document.querySelector('#cell-' + data.id + ' textarea');
          if (el) el.focus();
        });
      } catch (e) {
        console.error('Failed to create cell:', e);
      }
    },

    async insertCell(type, afterId) {
      this.openDropdownId = null;
      const body = { type, source: '' };
      if (afterId && afterId !== 'top') body.after = afterId;
      try {
        const data = await this.api('POST', '/api/cells', body);
        await this.load();
        this.focusedCellId = data.id;
        this.$nextTick(() => {
          const el = document.querySelector('#cell-' + data.id + ' textarea');
          if (el) el.focus();
        });
      } catch (e) {
        console.error('Failed to insert cell:', e);
      }
    },

    async deleteCell(id) {
      try {
        await this.api('DELETE', '/api/cells/' + id);
        if (this.focusedCellId === id) this.focusedCellId = null;
        await this.load();
      } catch (e) {
        console.error('Delete failed:', e);
      }
    },

    async moveCell(id, dir) {
      const idx = this.cells.findIndex(c => c.id === id);
      const newIdx = idx + dir;
      if (newIdx < 0 || newIdx >= this.cells.length) return;
      const ids = this.cells.map(c => c.id);
      [ids[idx], ids[newIdx]] = [ids[newIdx], ids[idx]];
      try {
        await this.api('POST', '/api/cells/reorder', { cell_ids: ids });
        await this.load();
      } catch (e) {
        console.error('Move failed:', e);
      }
    },

    // ── Save / Undo / Reset ──
    async save() {
      try {
        await this.api('POST', '/api/save');
        this.saveFeedback = true;
        setTimeout(() => { this.saveFeedback = false; }, 600);
      } catch (e) {
        alert('Save failed: ' + e.message);
      }
    },

    async undo() {
      try {
        const data = await this.api('POST', '/api/undo');
        this.canUndo = !!data.can_undo;
        await this.load();
      } catch (e) {
        console.error('Undo failed:', e);
      }
    },

    async reset() {
      if (!confirm('Reset the environment? All cell outputs will be cleared.')) return;
      try {
        await this.api('POST', '/api/reset');
        this.canUndo = false;
        await this.load();
      } catch (e) {
        console.error('Reset failed:', e);
      }
    },

    // ── Markdown ──
    editMarkdown(id) {
      const cell = this.cells.find(c => c.id === id);
      if (cell) {
        cell._rendered = false;
        this.focusedCellId = id;
        this.$nextTick(() => {
          const ta = document.querySelector('#cell-' + id + ' textarea');
          if (ta) ta.focus();
        });
      }
    },

    renderMarkdown(src) {
      let html = this.escapeHtml(src);
      html = html.replace(/^#### (.+)$/gm, '<h4>$1</h4>');
      html = html.replace(/^### (.+)$/gm, '<h3>$1</h3>');
      html = html.replace(/^## (.+)$/gm, '<h2>$1</h2>');
      html = html.replace(/^# (.+)$/gm, '<h1>$1</h1>');
      html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');
      html = html.replace(/\*(.+?)\*/g, '<em>$1</em>');
      html = html.replace(/`([^`]+)`/g, '<code>$1</code>');
      html = html.replace(/```[\w]*\n([\s\S]*?)```/g, '<pre><code>$1</code></pre>');
      html = html.replace(/^- (.+)$/gm, '<li>$1</li>');
      html = html.replace(/(<li>.*<\/li>\n?)+/g, (m) => '<ul>' + m + '</ul>');
      html = html.replace(/^(?!<[hup]|<li|<ul|<ol|<pre)(.+)$/gm, '<p>$1</p>');
      return html;
    },

    // ── Keyboard / Input ──
    handleShiftEnter(cell) {
      if (cell.cell_type === 'markdown') {
        cell._rendered = true;
      } else {
        this.evalCell(cell.id);
      }
    },

    insertTab(el, cell) {
      const start = el.selectionStart;
      el.value = el.value.substring(0, start) + '  ' + el.value.substring(el.selectionEnd);
      el.selectionStart = el.selectionEnd = start + 2;
      cell.source = el.value;
      this.autoResize(el);
    },

    // ── Helpers ──
    escapeHtml(str) {
      return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    },

    autoResize(el) {
      el.style.height = 'auto';
      el.style.height = el.scrollHeight + 'px';
    },

    formatMeta(output) {
      const parts = [];
      if (output.meta) {
        if (output.meta.duration_ms != null) parts.push(output.meta.duration_ms + 'ms');
        if (output.meta.cost_usd != null) parts.push('$' + output.meta.cost_usd.toFixed(4));
      }
      return parts.join(' \u00b7 ');
    },

    toggleOutput(header) {
      const chevron = header.querySelector('.output-chevron');
      const content = header.nextElementSibling;
      if (chevron) chevron.classList.toggle('collapsed');
      if (content) content.classList.toggle('collapsed');
    },
  }));
});
