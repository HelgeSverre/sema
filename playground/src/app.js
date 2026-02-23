import init, { SemaInterpreter, formatCode } from '../pkg/sema_wasm.js';
import { examples } from './examples.js';
import { highlightSema } from './highlight.js';
import { TextareaUndo } from './undo.js';
import { makeVfsHost, BACKENDS } from './vfs-backends.js';
import { initSplitters } from './splitters.js';

let interp = null;
let activeBtn = null;
let vfsHost = null;
let vfsBackend = null;
let backendName = 'memory';
let activeFilePath = null;

const STORAGE_KEY = 'sema-playground';

function loadState() {
  try { return JSON.parse(localStorage.getItem(STORAGE_KEY)) || {}; } catch { return {}; }
}

function saveState(patch) {
  const state = { ...loadState(), ...patch };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
}

// ── Files panel collapse ──

const filesPanel = document.getElementById('files-panel');
const filesBody = document.getElementById('files-body');
const collapseBtn = document.getElementById('files-collapse-btn');

collapseBtn.addEventListener('click', () => {
  const collapsed = filesBody.classList.toggle('collapsed');
  collapseBtn.textContent = collapsed ? '▸' : '▾';
  saveState({ filesCollapsed: collapsed });
});

// Restore collapsed state
const savedFilesCollapsed = loadState().filesCollapsed;
if (savedFilesCollapsed) {
  filesBody.classList.add('collapsed');
  collapseBtn.textContent = '▸';
}

// ── Example sidebar ──

const collapsedCategories = new Set();

function buildSidebar() {
  const tree = document.getElementById('sidebar-tree');
  const saved = loadState();
  const savedCollapsed = saved.collapsed || [];

  for (const cat of examples) {
    const catDiv = document.createElement('div');

    const header = document.createElement('div');
    header.className = 'tree-category';
    const chevron = document.createElement('span');
    chevron.className = 'tree-chevron';
    header.appendChild(chevron);
    header.appendChild(document.createTextNode(cat.category));

    const items = document.createElement('div');
    items.className = 'tree-items';

    // Determine initial collapsed state
    const isGettingStarted = cat.category === 'Getting Started';
    let collapsed;
    if (savedCollapsed.length > 0) {
      collapsed = savedCollapsed.includes(cat.category);
    } else {
      collapsed = !isGettingStarted;
    }

    if (collapsed) {
      items.classList.add('collapsed');
      collapsedCategories.add(cat.category);
    }
    chevron.textContent = collapsed ? '▸' : '▾';

    header.onclick = () => {
      items.classList.toggle('collapsed');
      const nowCollapsed = items.classList.contains('collapsed');
      chevron.textContent = nowCollapsed ? '▸' : '▾';
      if (nowCollapsed) collapsedCategories.add(cat.category);
      else collapsedCategories.delete(cat.category);
      saveState({ collapsed: [...collapsedCategories] });
    };

    for (const file of cat.files) {
      const btn = document.createElement('button');
      btn.className = 'tree-file';
      btn.textContent = file.name;
      btn.dataset.exampleId = file.id;
      btn.onclick = () => {
        editorEl.value = file.code;
        if (activeBtn) activeBtn.classList.remove('active');
        btn.classList.add('active');
        activeBtn = btn;
        saveState({ lastExampleId: file.id, editorContent: file.code });
        editorUndo.reset();
        scheduleHighlight();
      };
      items.appendChild(btn);
    }

    catDiv.appendChild(header);
    catDiv.appendChild(items);
    tree.appendChild(catDiv);
  }

  // Restore last selected example or editor content
  if (saved.editorContent) {
    editorEl.value = saved.editorContent;
  }
  if (saved.lastExampleId) {
    const btn = tree.querySelector(`[data-example-id="${CSS.escape(saved.lastExampleId)}"]`);
    if (btn) {
      btn.classList.add('active');
      activeBtn = btn;
      // Expand the parent category if collapsed
      const items = btn.closest('.tree-items');
      if (items && items.classList.contains('collapsed')) {
        items.classList.remove('collapsed');
        const chevron = items.previousElementSibling?.querySelector('.tree-chevron');
        if (chevron) chevron.textContent = '▾';
        const catName = items.previousElementSibling?.textContent?.trim();
        if (catName) collapsedCategories.delete(catName);
      }
    }
  }
}

// ── VFS File Tree ──

const fileTreeEl = document.getElementById('file-tree');
const fileViewerEl = document.getElementById('file-viewer');

function buildVfsTree(dir) {
  let entries;
  try { entries = interp.listFiles(dir); } catch { return []; }
  if (!entries || entries.length === 0) return [];

  const items = [];
  for (const name of entries) {
    const fullPath = dir === '/' ? '/' + name : dir + '/' + name;
    const isDir = interp.isDirectory(fullPath);
    items.push({ name, fullPath, isDir, children: isDir ? buildVfsTree(fullPath) : null });
  }

  items.sort((a, b) => {
    if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
    return a.name.localeCompare(b.name);
  });

  return items;
}

function renderVfsTree(items, depth) {
  const container = document.createDocumentFragment();

  for (const item of items) {
    if (item.isDir) {
      const row = document.createElement('div');
      row.className = 'vfs-tree-dir';
      row.style.paddingLeft = (depth * 14 + 8) + 'px';

      const chevron = document.createElement('span');
      chevron.className = 'tree-chevron';
      chevron.textContent = '▾';
      row.appendChild(chevron);
      row.appendChild(document.createTextNode(item.name + '/'));

      const childContainer = document.createElement('div');
      childContainer.appendChild(renderVfsTree(item.children, depth + 1));

      row.addEventListener('click', () => {
        const hidden = childContainer.style.display === 'none';
        childContainer.style.display = hidden ? '' : 'none';
        chevron.textContent = hidden ? '▾' : '▸';
      });

      container.appendChild(row);
      container.appendChild(childContainer);
    } else {
      const row = document.createElement('div');
      row.className = 'vfs-tree-file' + (item.fullPath === activeFilePath ? ' active' : '');
      row.style.paddingLeft = (depth * 14 + 8) + 'px';
      row.textContent = item.name;
      row.addEventListener('click', () => viewFile(item.fullPath));
      container.appendChild(row);
    }
  }

  return container;
}

function refreshFileTree() {
  if (!interp) return;
  fileTreeEl.innerHTML = '';
  const items = buildVfsTree('/');

  if (items.length === 0) {
    const empty = document.createElement('div');
    empty.className = 'vfs-tree-empty';
    empty.textContent = '(empty — run code to create files)';
    fileTreeEl.appendChild(empty);
    return;
  }

  fileTreeEl.appendChild(renderVfsTree(items, 0));
}

// ── File Viewer ──

function viewFile(path) {
  activeFilePath = path;
  const content = interp.readFile(path);
  fileViewerEl.innerHTML = '';
  fileViewerEl.textContent = content ?? '(empty file)';

  // Expand files panel if collapsed
  if (filesBody.classList.contains('collapsed')) {
    filesBody.classList.remove('collapsed');
    collapseBtn.textContent = '▾';
    saveState({ filesCollapsed: false });
  }

  refreshFileTree();
}

// ── VFS Stats ──

const vfsStatsEl = document.getElementById('vfs-stats');

function formatBytes(bytes) {
  if (bytes < 1024) return bytes + ' B';
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
  return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

function refreshVfsStats() {
  if (!interp) return;
  const s = interp.vfsStats();
  vfsStatsEl.textContent = s.files > 0 ? `${s.files} files · ${formatBytes(s.bytes)}` : '';
}

// ── Upload files into VFS ──

const uploadInput = document.getElementById('vfs-upload');
const uploadBtn = document.getElementById('upload-btn');
const dropOverlay = document.getElementById('drop-overlay');
const clearVfsBtn = document.getElementById('clear-vfs-btn');

uploadBtn.addEventListener('click', () => uploadInput.click());

uploadInput.addEventListener('change', async () => {
  if (uploadInput.files.length > 0) {
    await uploadFiles(uploadInput.files);
    uploadInput.value = '';
  }
});

// Drag and drop on the files panel
let dragCounter = 0;

filesPanel.addEventListener('dragenter', (e) => {
  e.preventDefault();
  dragCounter++;
  dropOverlay.classList.remove('hidden');
});

filesPanel.addEventListener('dragleave', (e) => {
  e.preventDefault();
  dragCounter--;
  if (dragCounter <= 0) {
    dragCounter = 0;
    dropOverlay.classList.add('hidden');
  }
});

filesPanel.addEventListener('dragover', (e) => {
  e.preventDefault();
});

filesPanel.addEventListener('drop', async (e) => {
  e.preventDefault();
  dragCounter = 0;
  dropOverlay.classList.add('hidden');
  if (e.dataTransfer.files.length > 0) {
    await uploadFiles(e.dataTransfer.files);
  }
});

async function uploadFiles(fileList) {
  if (!interp) return;

  interp.mkdir('/uploads');

  let uploaded = 0;
  for (const file of fileList) {
    if (file.size > 1024 * 1024) {
      document.getElementById('status').textContent = `Skipped ${file.name} (>1MB)`;
      continue;
    }
    try {
      const text = await file.text();
      const path = '/uploads/' + file.name;
      interp.writeFile(path, text);
      uploaded++;
    } catch (e) {
      document.getElementById('status').textContent = `Upload failed: ${e.message}`;
    }
  }

  if (uploaded > 0) {
    document.getElementById('status').textContent = `Uploaded ${uploaded} file(s) to /uploads/`;
    document.getElementById('status').className = 'status-text status-ready';

    if (backendName !== 'memory' && vfsBackend) {
      try { await vfsBackend.flush(vfsHost); } catch {}
    }

    // Expand files panel to show uploaded files
    if (filesBody.classList.contains('collapsed')) {
      filesBody.classList.remove('collapsed');
      collapseBtn.textContent = '▾';
      saveState({ filesCollapsed: false });
    }
  }

  refreshFileTree();
  refreshVfsStats();
}

// Clear VFS
clearVfsBtn.addEventListener('click', async () => {
  if (!interp) return;
  interp.resetVFS();
  if (vfsBackend?.reset) await vfsBackend.reset();
  activeFilePath = null;
  fileViewerEl.innerHTML = '<div class="viewer-placeholder">Click a file to preview</div>';
  refreshFileTree();
  refreshVfsStats();
});

// ── Backend toggle ──

const backendToggle = document.getElementById('backend-toggle');

backendToggle.addEventListener('change', async (e) => {
  const newName = e.target.value;
  if (newName === backendName || !interp) return;

  const newBackend = BACKENDS[newName]();
  await newBackend.init?.();
  interp.resetVFS();
  await newBackend.hydrate(vfsHost);

  vfsBackend = newBackend;
  backendName = newName;
  saveState({ backend: newName });

  backendToggle.querySelectorAll('label').forEach(l => {
    l.classList.toggle('active', l.querySelector('input').value === newName);
  });

  activeFilePath = null;
  fileViewerEl.innerHTML = '<div class="viewer-placeholder">Click a file to preview</div>';
  refreshFileTree();
  refreshVfsStats();
});

// ── Engine toggle ──

let useVM = false;
document.getElementById('engine-toggle').addEventListener('change', (e) => {
  useVM = e.target.value === 'vm';
  document.querySelectorAll('#engine-toggle label').forEach(l => {
    l.classList.toggle('active', l.querySelector('input').value === e.target.value);
  });
});

// ── Init ──

async function main() {
  buildSidebar();
  initSplitters();
  await init();
  interp = new SemaInterpreter();
  vfsHost = makeVfsHost(interp);

  // Restore backend preference
  const saved = loadState();
  const storedBackend = saved.backend ?? 'memory';
  if (BACKENDS[storedBackend]) {
    backendName = storedBackend;
    const radio = backendToggle.querySelector(`input[value="${storedBackend}"]`);
    if (radio) {
      radio.checked = true;
      backendToggle.querySelectorAll('label').forEach(l => {
        l.classList.toggle('active', l.querySelector('input').value === storedBackend);
      });
    }
  }

  vfsBackend = BACKENDS[backendName]();
  await vfsBackend.init?.();
  await vfsBackend.hydrate(vfsHost);

  document.getElementById('version').textContent = `v${interp.version()}`;
  document.getElementById('status').textContent = 'Ready';
  document.getElementById('status').className = 'status-text status-ready';
  document.getElementById('run-btn').disabled = false;
  document.getElementById('fmt-btn').disabled = false;
  document.getElementById('output').innerHTML = '<div class="output-welcome">Ready. Write some Sema code and press Run.</div>';

  document.getElementById('loading').classList.add('hidden');
  refreshVfsStats();
}

// ── Run ──

const outputEl = document.getElementById('output');

async function run() {
  if (!interp) return;
  const code = editorEl.value;
  if (!code.trim()) return;

  const engine = useVM ? 'vm' : 'tree';
  const runBtn = document.getElementById('run-btn');
  runBtn.disabled = true;

  const t0 = performance.now();
  const result = useVM ? await interp.evalVMAsync(code) : await interp.evalAsync(code);
  const elapsed = performance.now() - t0;

  runBtn.disabled = false;

  outputEl.innerHTML = '';

  if (result.output && result.output.length > 0) {
    for (const line of result.output) {
      const div = document.createElement('div');
      div.className = 'output-line';
      div.textContent = line;
      outputEl.appendChild(div);
    }
  }

  if (result.error) {
    const div = document.createElement('div');
    div.className = 'output-error';
    div.textContent = result.error;
    outputEl.appendChild(div);
  } else if (result.value !== null) {
    const div = document.createElement('div');
    div.className = 'output-value';
    div.textContent = `=> ${result.value}`;
    outputEl.appendChild(div);
  }

  const timing = document.createElement('div');
  timing.className = 'output-timing';
  timing.textContent = `Evaluated in ${elapsed.toFixed(1)}ms · ${engine === 'vm' ? 'bytecode VM' : 'tree-walker'}`;
  outputEl.appendChild(timing);

  // Refresh VFS state
  refreshFileTree();
  refreshVfsStats();

  // Auto-flush for persistent backends
  if (backendName !== 'memory') {
    try {
      await vfsBackend.flush(vfsHost);
    } catch (e) {
      document.getElementById('status').textContent = `Persist failed: ${e.message}`;
    }
  }

  // Re-read active file in case it changed
  if (activeFilePath && interp.fileExists(activeFilePath)) {
    const content = interp.readFile(activeFilePath);
    fileViewerEl.textContent = content ?? '(empty file)';
  }
}

// Run button
document.getElementById('run-btn').addEventListener('click', run);

// Format button
document.getElementById('fmt-btn').addEventListener('click', () => {
  const code = editorEl.value;
  if (!code.trim()) return;
  const result = formatCode(code, 80, 2, false);
  if (result.error) {
    outputEl.innerHTML = '';
    const div = document.createElement('div');
    div.className = 'output-error';
    div.textContent = `Format error: ${result.error}`;
    outputEl.appendChild(div);
  } else if (result.formatted !== null) {
    editorUndo.transact(() => { editorEl.value = result.formatted; });
    scheduleHighlight();
    debounceSaveEditor();
  }
});

// Clear button
document.getElementById('clear-btn').addEventListener('click', () => {
  outputEl.innerHTML = '';
});

// ── Syntax highlighting ──

const editorEl = document.getElementById('editor');
const hlEl = document.getElementById('editor-highlight');
let hlRaf = 0;

function scheduleHighlight() {
  cancelAnimationFrame(hlRaf);
  hlRaf = requestAnimationFrame(() => {
    hlEl.innerHTML = highlightSema(editorEl.value);
  });
}

function syncScroll() {
  hlEl.scrollTop = editorEl.scrollTop;
  hlEl.scrollLeft = editorEl.scrollLeft;
}

// ── Undo/redo ──

const editorUndo = new TextareaUndo(editorEl, { onChange: scheduleHighlight });

editorEl.addEventListener('input', () => {
  scheduleHighlight();
  debounceSaveEditor();
});
editorEl.addEventListener('scroll', syncScroll);
editorEl.addEventListener('focus', () => hlEl.classList.add('focused'));
editorEl.addEventListener('blur', () => hlEl.classList.remove('focused'));

// Debounced editor content save
let saveTimer = 0;
function debounceSaveEditor() {
  clearTimeout(saveTimer);
  saveTimer = setTimeout(() => {
    saveState({ editorContent: editorEl.value });
  }, 500);
}

// Keyboard shortcut: Cmd/Ctrl+Enter and Tab/Shift+Tab
editorEl.addEventListener('keydown', (e) => {
  if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') {
    e.preventDefault();
    run();
  }
  if (e.key === 'Tab') {
    e.preventDefault();
    editorUndo.transact(() => {
      const ta = editorEl;
      const v = ta.value;
      const start = ta.selectionStart;
      const end = ta.selectionEnd;
      const isDedent = e.shiftKey;
      const ls = v.lastIndexOf('\n', start - 1) + 1;

      if (start === end) {
        if (!isDedent) {
          ta.setRangeText('  ', start, end, 'end');
        } else {
          let rm = v.startsWith('  ', ls) ? 2 : v.charAt(ls) === ' ' ? 1 : 0;
          if (rm) {
            ta.setRangeText('', ls, ls + rm, 'preserve');
            ta.setSelectionRange(Math.max(ls, start - rm), Math.max(ls, start - rm));
          }
        }
      } else {
        const endAdj = (end > start && v[end - 1] === '\n') ? end - 1 : end;
        const le = v.indexOf('\n', endAdj);
        const blockEnd = le === -1 ? v.length : le;
        const block = v.slice(ls, blockEnd);
        const replacement = isDedent
          ? block.replace(/^ {1,2}/gm, '')
          : block.replace(/^/gm, '  ');
        ta.setRangeText(replacement, ls, blockEnd, 'select');
      }
    });
    scheduleHighlight();
  }
});

// Highlight initial content
scheduleHighlight();

main().then(() => { scheduleHighlight(); });
