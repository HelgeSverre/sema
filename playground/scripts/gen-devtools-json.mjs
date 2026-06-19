// Generate the Chrome DevTools "Automatic Workspace Folders" descriptor for
// LOCAL DEV. When Chrome opens the playground it fetches
// /.well-known/appspecific/com.chrome.devtools.json; pointing it at this repo's
// playground/ folder makes DevTools auto-add a Workspace, so you can debug the
// real source (via sourcemaps) and edit-to-disk straight from the Sources panel.
//
// The file embeds an absolute, machine-specific path, so it is gitignored and
// only generated for local dev (never for the production build/deploy).
import { writeFileSync, mkdirSync } from 'fs';
import { dirname, resolve, join } from 'path';
import { fileURLToPath } from 'url';

const playgroundRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const outDir = join(playgroundRoot, '.well-known', 'appspecific');
mkdirSync(outDir, { recursive: true });

// Stable UUID so DevTools remembers the workspace mapping across reloads.
const payload = {
  workspace: { root: playgroundRoot, uuid: 'a1b2c3d4-e5f6-4a7b-8c9d-0e1f2a3b4c5d' },
};

writeFileSync(
  join(outDir, 'com.chrome.devtools.json'),
  JSON.stringify(payload, null, 2) + '\n'
);
console.log(`Wrote .well-known/appspecific/com.chrome.devtools.json (workspace root: ${playgroundRoot})`);
