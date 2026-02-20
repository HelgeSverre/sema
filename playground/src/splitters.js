// Draggable splitter system — persists sizes to localStorage

const STORAGE_KEY = 'sema-playground';

function loadState() {
  try { return JSON.parse(localStorage.getItem(STORAGE_KEY)) || {}; } catch { return {}; }
}

function saveState(patch) {
  const state = { ...loadState(), ...patch };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
}

function draggable(el, axis, onMove, onEnd) {
  el.addEventListener('mousedown', (e) => {
    e.preventDefault();
    const start = axis === 'x' ? e.clientX : e.clientY;
    el.classList.add('active');
    document.body.style.cursor = axis === 'x' ? 'col-resize' : 'row-resize';
    document.body.style.userSelect = 'none';

    const move = (e) => onMove(( axis === 'x' ? e.clientX : e.clientY) - start);
    const up = () => {
      el.classList.remove('active');
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
      document.removeEventListener('mousemove', move);
      document.removeEventListener('mouseup', up);
      if (onEnd) onEnd();
    };
    document.addEventListener('mousemove', move);
    document.addEventListener('mouseup', up);
  });
}

export function initSplitters() {
  const mainEl = document.querySelector('main');
  const rightCol = document.querySelector('.right-column');
  const filesBody = document.getElementById('files-body');
  const saved = loadState();

  // ── 1. Sidebar width ──
  let sidebarW = saved.sidebarW ?? 200;
  mainEl.style.setProperty('--sidebar-w', sidebarW + 'px');

  let sidebarStart;
  draggable(document.getElementById('splitter-sidebar'), 'x',
    (delta) => {
      if (sidebarStart == null) sidebarStart = sidebarW;
      sidebarW = Math.max(120, Math.min(400, sidebarStart + delta));
      mainEl.style.setProperty('--sidebar-w', sidebarW + 'px');
      applyEditorRatio();
    },
    () => { sidebarStart = null; saveState({ sidebarW }); }
  );

  // ── 2. Editor / right-column ratio ──
  let editorRatio = saved.editorRatio ?? 0.55;

  function applyEditorRatio() {
    const available = mainEl.clientWidth - sidebarW - 8;
    const rightW = Math.round(available * (1 - editorRatio));
    mainEl.style.setProperty('--right-col-w', rightW + 'px');
  }
  applyEditorRatio();
  window.addEventListener('resize', applyEditorRatio);

  let editorStart;
  draggable(document.getElementById('splitter-editor'), 'x',
    (delta) => {
      if (editorStart == null) editorStart = editorRatio;
      const available = mainEl.clientWidth - sidebarW - 8;
      const editorW = Math.max(200, Math.min(available - 200, Math.round(available * editorStart) + delta));
      editorRatio = editorW / available;
      applyEditorRatio();
    },
    () => { editorStart = null; saveState({ editorRatio }); }
  );

  // ── 3. Output / files height ──
  let filesH = saved.filesH ?? 200;
  filesBody.style.setProperty('--files-h', filesH + 'px');

  let filesStart;
  draggable(document.getElementById('splitter-output'), 'y',
    (delta) => {
      if (filesStart == null) filesStart = filesH;
      const totalH = rightCol.clientHeight;
      filesH = Math.max(60, Math.min(totalH - 120, filesStart - delta));
      filesBody.style.setProperty('--files-h', filesH + 'px');
    },
    () => { filesStart = null; saveState({ filesH }); }
  );

  // ── 4. File tree width ──
  let filetreeW = saved.filetreeW ?? 200;
  filesBody.style.setProperty('--filetree-w', filetreeW + 'px');

  let ftStart;
  draggable(document.getElementById('splitter-filetree'), 'x',
    (delta) => {
      if (ftStart == null) ftStart = filetreeW;
      filetreeW = Math.max(100, Math.min(400, ftStart + delta));
      filesBody.style.setProperty('--filetree-w', filetreeW + 'px');
    },
    () => { ftStart = null; saveState({ filetreeW }); }
  );
}
