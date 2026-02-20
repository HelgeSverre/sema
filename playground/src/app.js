import init, { SemaInterpreter } from '../pkg/sema_wasm.js';
import { examples } from './examples.js';
import { highlightSema } from './highlight.js';
import { TextareaUndo } from './undo.js';

let interp = null;
let activeBtn = null;

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

async function main() {
  buildSidebar();
  await init();
  interp = new SemaInterpreter();

  document.getElementById('version').textContent = `v${interp.version()}`;
  document.getElementById('status').textContent = 'Ready';
  document.getElementById('status').className = 'status-text status-ready';
  document.getElementById('run-btn').disabled = false;
  document.getElementById('output').innerHTML = '<div class="output-welcome">Ready. Write some Sema code and press Run.</div>';

  document.getElementById('loading').classList.add('hidden');
}

// Engine toggle
let useVM = false;
document.getElementById('engine-toggle').addEventListener('change', (e) => {
  useVM = e.target.value === 'vm';
  document.querySelectorAll('#engine-toggle label').forEach(l => {
    l.classList.toggle('active', l.querySelector('input').value === e.target.value);
  });
});

async function run() {
  if (!interp) return;
  const code = document.getElementById('editor').value;
  if (!code.trim()) return;

  const engine = useVM ? 'vm' : 'tree';
  const runBtn = document.getElementById('run-btn');
  runBtn.disabled = true;

  const t0 = performance.now();
  const result = useVM ? await interp.evalVMAsync(code) : await interp.evalAsync(code);
  const elapsed = performance.now() - t0;

  runBtn.disabled = false;

  const out = document.getElementById('output');
  out.innerHTML = '';

  // Print output lines
  if (result.output && result.output.length > 0) {
    for (const line of result.output) {
      const div = document.createElement('div');
      div.className = 'output-line';
      div.textContent = line;
      out.appendChild(div);
    }
  }

  // Print result or error
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

  // Timing
  const timing = document.createElement('div');
  timing.className = 'output-timing';
  timing.textContent = `Evaluated in ${elapsed.toFixed(1)}ms · ${engine === 'vm' ? 'bytecode VM' : 'tree-walker'}`;
  out.appendChild(timing);
}

// Run button
document.getElementById('run-btn').addEventListener('click', run);

// Clear button
document.getElementById('clear-btn').addEventListener('click', () => {
  document.getElementById('output').innerHTML = '';
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
