import * as fs from 'fs';
import * as path from 'path';

const E2E_DIR = path.resolve(__dirname, '..', '..');
const DB_PATH = path.join(E2E_DIR, 'e2e-test.db');
const BLOB_DIR = path.join(E2E_DIR, 'e2e-blobs');

/**
 * Delete the test database and blob directory to start fresh.
 * The server will auto-migrate on startup.
 */
export function resetTestData(): void {
  // Remove DB files (SQLite creates -wal and -shm companions)
  for (const suffix of ['', '-wal', '-shm']) {
    const file = DB_PATH + suffix;
    if (fs.existsSync(file)) {
      fs.unlinkSync(file);
    }
  }

  // Remove blob directory
  if (fs.existsSync(BLOB_DIR)) {
    fs.rmSync(BLOB_DIR, { recursive: true, force: true });
  }
  fs.mkdirSync(BLOB_DIR, { recursive: true });
}
