# Backlog

Minor improvements and tech debt items deferred for later sweeps.

## Performance

- **VFS clones on every read** — `vfs_read` returns `Option<Vec<u8>>`, cloning file contents on each call. For large assets or hot paths, return `Cow<[u8]>` or switch to `Arc<HashMap>` with shared references. (Identified in PR #14 review, severity: medium)

## Build System (`sema build`)

- **`file/read-lines` line ending handling** — VFS-served files with `\r\n` line endings will retain `\r` in split results. Consistent with the `fs::read_to_string` fallback but worth normalizing. (Identified in PR #14 review, severity: low)
