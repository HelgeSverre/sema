import { readdirSync, readFileSync, writeFileSync, mkdirSync, existsSync } from 'fs';
import { join, basename } from 'path';
import { build } from 'esbuild';

const EXAMPLES_DIR = 'examples';
const OUTPUT_FILE = 'src/examples.js';
const DIST_DIR = 'dist';

// Category display names
const categoryNames = {
  'getting-started': 'Getting Started',
  'functional': 'Functional',
  'data': 'Data',
  'patterns': 'Patterns',
  'visuals': 'Visuals',
  'math-crypto': 'Math & Crypto',
  'llm-tools': 'LLM Tools',
  'http': 'HTTP',
};

// Category display order
const categoryOrder = [
  'getting-started',
  'functional',
  'data',
  'http',
  'llm-tools',
  'patterns',
  'visuals',
  'math-crypto',
];

// 1. Generate examples.js from .sema files
const dirs = readdirSync(EXAMPLES_DIR, { withFileTypes: true })
  .filter(d => d.isDirectory())
  .map(d => d.name);

// Sort directories by the defined order, unknown dirs go to the end
dirs.sort((a, b) => {
  const ai = categoryOrder.indexOf(a);
  const bi = categoryOrder.indexOf(b);
  return (ai === -1 ? 999 : ai) - (bi === -1 ? 999 : bi);
});

const categories = [];
for (const dir of dirs) {
  const dirPath = join(EXAMPLES_DIR, dir);
  const files = readdirSync(dirPath)
    .filter(f => f.endsWith('.sema'))
    .sort();

  const fileEntries = files.map(f => {
    const code = readFileSync(join(dirPath, f), 'utf-8');
    return `    { name: ${JSON.stringify(f)}, code: ${JSON.stringify(code)} }`;
  });

  const displayName = categoryNames[dir] || dir.replace(/-/g, ' ').replace(/\b\w/g, c => c.toUpperCase());
  categories.push(`  { category: ${JSON.stringify(displayName)}, files: [\n${fileEntries.join(',\n')}\n  ]}`);
}

const output = `export const examples = [\n${categories.join(',\n')}\n];\n`;
writeFileSync(OUTPUT_FILE, output);
console.log(`Generated ${OUTPUT_FILE} (${dirs.length} categories, ${dirs.reduce((n, d) => n + readdirSync(join(EXAMPLES_DIR, d)).filter(f => f.endsWith('.sema')).length, 0)} files)`);

// 2. Bundle with esbuild
if (!existsSync(DIST_DIR)) {
  mkdirSync(DIST_DIR, { recursive: true });
}

await build({
  entryPoints: ['src/app.js'],
  outfile: 'dist/app.js',
  bundle: true,
  format: 'esm',
  minify: false,
  target: 'es2020',
  external: ['../pkg/*'],
});
console.log('Bundled dist/app.js');
