import init, { SemaInterpreter } from '../pkg/sema_wasm.js';

let interp = null;
let activeFilePath = null;

const editorEl = document.getElementById('editor');
const outputEl = document.getElementById('output');
const runBtn = document.getElementById('run-btn');
const clearVfsBtn = document.getElementById('clear-vfs-btn');
const clearOutputBtn = document.getElementById('clear-output-btn');
const fileTreeEl = document.getElementById('file-tree');
const viewerTitle = document.getElementById('viewer-title');
const fileViewerEl = document.getElementById('file-viewer');
const vfsStatsEl = document.getElementById('vfs-stats');
const statusEl = document.getElementById('status-text');
const versionEl = document.getElementById('version');
const loadingEl = document.getElementById('loading');

// --- Initialization ---

async function start() {
  await init();
  interp = new SemaInterpreter();
  versionEl.textContent = `v${interp.version()}`;
  runBtn.disabled = false;
  clearVfsBtn.disabled = false;
  loadingEl.classList.add('hidden');
  statusEl.textContent = 'Ready';
  statusEl.className = 'status-text status-ready';
  outputEl.innerHTML = '<div class="output-welcome">Ready. Press Run to evaluate the script.</div>';
  refreshFileTree();
  refreshStats();
}

start();

// --- Run ---

async function run() {
  if (!interp) return;
  const code = editorEl.value;
  if (!code.trim()) return;

  outputEl.innerHTML = '';
  statusEl.textContent = 'Running…';
  statusEl.className = 'status-text status-loading';
  runBtn.disabled = true;

  try {
    const result = await interp.evalAsync(code);

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
    } else if (result.value != null) {
      const div = document.createElement('div');
      div.className = 'output-value';
      div.textContent = `=> ${result.value}`;
      outputEl.appendChild(div);
    }
  } catch (e) {
    const div = document.createElement('div');
    div.className = 'output-error';
    div.textContent = String(e);
    outputEl.appendChild(div);
  }

  runBtn.disabled = false;
  statusEl.textContent = 'Ready';
  statusEl.className = 'status-text status-ready';
  refreshFileTree();
  refreshStats();

  // Re-read active file in case it changed
  if (activeFilePath && interp.fileExists(activeFilePath)) {
    viewFile(activeFilePath);
  }
}

runBtn.addEventListener('click', run);
clearOutputBtn.addEventListener('click', () => { outputEl.innerHTML = ''; });

editorEl.addEventListener('keydown', (e) => {
  if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') {
    e.preventDefault();
    run();
  }
  if (e.key === 'Tab') {
    e.preventDefault();
    const start = editorEl.selectionStart;
    const end = editorEl.selectionEnd;
    editorEl.value = editorEl.value.substring(0, start) + '  ' + editorEl.value.substring(end);
    editorEl.selectionStart = editorEl.selectionEnd = start + 2;
  }
});

// --- File Tree ---

function buildTree(dir) {
  let entries;
  try { entries = interp.listFiles(dir); } catch { return []; }
  if (!entries || entries.length === 0) return [];

  const items = [];
  for (const name of entries) {
    const fullPath = dir === '/' ? '/' + name : dir + '/' + name;
    const isDir = interp.isDirectory(fullPath);
    items.push({ name, fullPath, isDir, children: isDir ? buildTree(fullPath) : null });
  }

  items.sort((a, b) => {
    if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
    return a.name.localeCompare(b.name);
  });

  return items;
}

function renderTree(items, depth) {
  const container = document.createDocumentFragment();

  for (const item of items) {
    if (item.isDir) {
      const row = document.createElement('div');
      row.className = 'tree-dir';
      row.style.paddingLeft = (depth * 14 + 8) + 'px';

      const chevron = document.createElement('span');
      chevron.className = 'tree-chevron';
      chevron.textContent = '▾';
      row.appendChild(chevron);
      row.appendChild(document.createTextNode(item.name + '/'));

      const childContainer = document.createElement('div');
      childContainer.appendChild(renderTree(item.children, depth + 1));

      row.addEventListener('click', () => {
        const hidden = childContainer.style.display === 'none';
        childContainer.style.display = hidden ? '' : 'none';
        chevron.textContent = hidden ? '▾' : '▸';
      });

      container.appendChild(row);
      container.appendChild(childContainer);
    } else {
      const row = document.createElement('div');
      row.className = 'tree-file' + (item.fullPath === activeFilePath ? ' active' : '');
      row.style.paddingLeft = (depth * 14 + 8) + 'px';
      row.textContent = item.name;
      row.addEventListener('click', () => viewFile(item.fullPath));
      container.appendChild(row);
    }
  }

  return container;
}

function refreshFileTree() {
  fileTreeEl.innerHTML = '';
  const items = buildTree('/');

  if (items.length === 0) {
    const empty = document.createElement('div');
    empty.className = 'tree-empty';
    empty.textContent = '(empty — run code to create files)';
    fileTreeEl.appendChild(empty);
    return;
  }

  fileTreeEl.appendChild(renderTree(items, 0));
}

// --- File Viewer ---

function viewFile(path) {
  activeFilePath = path;
  const content = interp.readFile(path);
  viewerTitle.textContent = path;
  fileViewerEl.textContent = content ?? '(empty file)';
  refreshFileTree(); // re-render to update active highlight
}

// --- VFS Stats ---

function formatBytes(bytes) {
  if (bytes < 1024) return bytes + ' B';
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
  return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

function refreshStats() {
  const s = interp.vfsStats();
  vfsStatsEl.textContent = `${s.files} files · ${formatBytes(s.bytes)} / ${formatBytes(s.maxBytes)} · ${s.maxFiles} max`;
}

// --- Clear VFS ---

clearVfsBtn.addEventListener('click', () => {
  interp.resetVFS();
  activeFilePath = null;
  viewerTitle.textContent = 'File Viewer';
  fileViewerEl.innerHTML = '<span class="viewer-placeholder">Click a file in the explorer to view its contents.</span>';
  refreshFileTree();
  refreshStats();
});
