export const SEMA_KEYWORDS = new Set([
  // Special forms
  'define', 'defun', 'lambda', 'fn', 'if', 'cond', 'case', 'when', 'unless',
  'let', 'let*', 'letrec', 'begin', 'do', 'and', 'or', 'not',
  'set!', 'quote', 'quasiquote', 'unquote', 'unquote-splicing',
  'define-record-type', 'defmacro', 'defagent', 'deftool',
  'try', 'catch', 'throw', 'error',
  'import', 'module', 'export', 'load', 'require',
  'delay', 'force', 'eval', 'macroexpand', 'with-budget',
  'else',
  // Threading macros
  '->', '->>', 'as->', 'some->',
  // Higher-order & LLM
  'map', 'filter', 'foldl', 'foldr', 'reduce', 'for-each', 'apply',
  'prompt', 'message', 'conversation/new', 'conversation/say',
  'llm/complete', 'llm/chat', 'llm/stream', 'llm/send',
  'llm/extract', 'llm/classify', 'llm/batch', 'llm/pmap',
  'llm/embed', 'llm/auto-configure', 'llm/configure',
  'llm/set-budget', 'llm/budget-remaining',
  'agent/run',
]);

export function tokenizeSema(code) {
  const tokens = [];
  let i = 0;
  while (i < code.length) {
    // Comments
    if (code[i] === ';') {
      const start = i;
      while (i < code.length && code[i] !== '\n') i++;
      tokens.push({ type: 'comment', text: code.slice(start, i) });
    }
    // Strings
    else if (code[i] === '"') {
      const start = i;
      i++;
      while (i < code.length && code[i] !== '"') {
        if (code[i] === '\\' && i + 1 < code.length) i++;
        i++;
      }
      if (i < code.length) i++; // closing quote
      tokens.push({ type: 'string', text: code.slice(start, i) });
    }
    // Parens
    else if ('()[]{}\'`,'.includes(code[i])) {
      tokens.push({ type: 'paren', text: code[i] });
      i++;
    }
    // Whitespace
    else if (/\s/.test(code[i])) {
      const start = i;
      while (i < code.length && /\s/.test(code[i])) i++;
      tokens.push({ type: 'ws', text: code.slice(start, i) });
    }
    // Words (symbols, keywords, numbers, booleans)
    else {
      const start = i;
      while (i < code.length && !/[\s()[\]{}"`;,]/.test(code[i])) i++;
      const word = code.slice(start, i);
      if (word === '#t' || word === '#f' || word === 'true' || word === 'false' || word === 'nil') {
        tokens.push({ type: 'boolean', text: word });
      } else if (/^-?\d+(\.\d+)?$/.test(word)) {
        tokens.push({ type: 'number', text: word });
      } else if (word.startsWith(':') && word.length > 1) {
        tokens.push({ type: 'keyword-lit', text: word });
      } else if (SEMA_KEYWORDS.has(word)) {
        tokens.push({ type: 'keyword', text: word });
      } else {
        tokens.push({ type: 'plain', text: word });
      }
    }
  }
  return tokens;
}

function escapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

export function highlightSema(code) {
  if (!code) return '\n';
  const tokens = tokenizeSema(code);
  let html = '';
  for (const t of tokens) {
    const escaped = escapeHtml(t.text);
    if (t.type === 'ws' || t.type === 'plain') {
      html += escaped;
    } else {
      html += `<span class="hl-${t.type}">${escaped}</span>`;
    }
  }
  // Trailing newline fix: pre won't render a final empty line
  if (code.endsWith('\n')) html += ' ';
  return html;
}
