import init, { SemaInterpreter } from '../pkg/sema_wasm.js';
import { examples } from './examples.js';
import { highlightSema } from './highlight.js';
import { TextareaUndo } from './undo.js';
import { makeVfsHost, BACKENDS } from './vfs-backends.js';

let interp = null;
let activeBtn = null;
let vfsHost = null;
let vfsBackend = null;
let backendName = 'memory';
let activeFilePath = null;

const BACKEND_PREF_KEY = 'sema-playground:backend';

// ── Sidebar tabs ──

const sidebarTabs = document.getElementById('sidebar-tabs');
const sidebarTree = document.getElementById('sidebar-tree');
const fileTreeEl = document.getElementById('file-tree');

sidebarTabs.addEventListener('change', (e) => {
  const tab = e.target.value;
  sidebarTabs.querySelectorAll('label').forEach(l => {
    l.classList.toggle('active', l.querySelector('input').value === tab);
  });
  sidebarTree.classList.toggle('hidden', tab !== 'examples');
  fileTreeEl.classList.toggle('hidden', tab !== 'files');
  if (tab === 'files') refreshFileTree();
});

// ── Output tabs ──

const outputTabs = document.getElementById('output-tabs');
const outputEl = document.getElementById('output');
const fileViewerEl = document.getElementById('file-viewer');

outputTabs.addEventListener('change', (e) => {
  const tab = e.target.value;
  outputTabs.querySelectorAll('label').forEach(l => {
    l.classList.toggle('active', l.querySelector('input').value === tab);
  });
  outputEl.classList.toggle('hidden', tab !== 'output');
  fileViewerEl.classList.toggle('hidden', tab !== 'viewer');
});

// ── Example sidebar ──

function buildSidebar() {
  const tree = document.getElementById('sidebar-tree');
  for (const cat of examples) {
    const catDiv = document.createElement('div');

    const header = document.createElement('div');
    header.className = 'tree-category';
    const chevron = document.createElement('span');
    chevron.className = 'tree-chevron';
    chevron.textContent = '▾';
    header.appendChild(chevron);
    header.appendChild(document.createTextNode(cat.category));

    const items = document.createElement('div');
    items.className = 'tree-items';

    header.onclick = () => {
      items.classList.toggle('collapsed');
      chevron.textContent = items.classList.contains('collapsed') ? '▸' : '▾';
    };

    for (const file of cat.files) {
      const btn = document.createElement('button');
      btn.className = 'tree-file';
      btn.textContent = file.name;
      btn.onclick = () => {
        document.getElementById('editor').value = file.code;
        if (activeBtn) activeBtn.classList.remove('active');
        btn.classList.add('active');
        activeBtn = btn;
      };
      items.appendChild(btn);
    }

    catDiv.appendChild(header);
    catDiv.appendChild(items);
    tree.appendChild(catDiv);
  }
}

// ── VFS File Tree ──

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

  // Switch to File Viewer tab
  const viewerRadio = outputTabs.querySelector('input[value="viewer"]');
  viewerRadio.checked = true;
  viewerRadio.dispatchEvent(new Event('change', { bubbles: true }));

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
  vfsStatsEl.textContent = `${s.files} files · ${formatBytes(s.bytes)}`;
}

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
  localStorage.setItem(BACKEND_PREF_KEY, newName);

  backendToggle.querySelectorAll('label').forEach(l => {
    l.classList.toggle('active', l.querySelector('input').value === newName);
  });

  activeFilePath = null;
  fileViewerEl.innerHTML = '<div class="viewer-placeholder">Click a file in the Files tab to view its contents.</div>';
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
  await init();
  interp = new SemaInterpreter();
  vfsHost = makeVfsHost(interp);

  // Restore backend preference
  const storedBackend = localStorage.getItem(BACKEND_PREF_KEY) ?? 'memory';
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
  outputEl.innerHTML = '<div class="output-welcome">Ready. Write some Sema code and press Run.</div>';

  document.getElementById('loading').classList.add('hidden');
  refreshVfsStats();
}

// ── Run ──

async function run() {
  if (!interp) return;
  const code = document.getElementById('editor').value;
  if (!code.trim()) return;

  const engine = useVM ? 'vm' : 'tree';
  const runBtn = document.getElementById('run-btn');
  runBtn.disabled = true;

  // Switch to output tab
  const outputRadio = outputTabs.querySelector('input[value="output"]');
  outputRadio.checked = true;
  outputRadio.dispatchEvent(new Event('change', { bubbles: true }));

  const t0 = performance.now();
  const result = useVM ? await interp.evalVMAsync(code) : await interp.evalAsync(code);
  const elapsed = performance.now() - t0;

  runBtn.disabled = false;

  const out = outputEl;
  out.innerHTML = '';

  if (result.output && result.output.length > 0) {
    for (const line of result.output) {
      const div = document.createElement('div');
      div.className = 'output-line';
      div.textContent = line;
      out.appendChild(div);
    }
  }

  if (result.error) {
    const div = document.createElement('div');
    div.className = 'output-error';
    div.textContent = result.error;
    out.appendChild(div);
  } else if (result.value !== null) {
    const div = document.createElement('div');
    div.className = 'output-value';
    div.textContent = `=> ${result.value}`;
    out.appendChild(div);
  }

  const timing = document.createElement('div');
  timing.className = 'output-timing';
  timing.textContent = `Evaluated in ${elapsed.toFixed(1)}ms · ${engine === 'vm' ? 'bytecode VM' : 'tree-walker'}`;
  out.appendChild(timing);

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

editorEl.addEventListener('input', scheduleHighlight);
editorEl.addEventListener('scroll', syncScroll);
editorEl.addEventListener('focus', () => hlEl.classList.add('focused'));
editorEl.addEventListener('blur', () => hlEl.classList.remove('focused'));

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

// Re-highlight when loading a file from sidebar
function patchFileLoads() {
  document.querySelectorAll('.tree-file').forEach(btn => {
    const orig = btn.onclick;
    btn.onclick = function() {
      orig?.call(this);
      editorUndo.reset();
      scheduleHighlight();
    };
  });
}

// Highlight initial content
scheduleHighlight();

main().then(() => { patchFileLoads(); scheduleHighlight(); });
