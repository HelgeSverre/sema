use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

pub fn blob_path(blob_dir: &str, blob_key: &str) -> PathBuf {
    Path::new(blob_dir).join(&blob_key[..2]).join(blob_key)
}

/// Store bytes to disk, returns (blob_key, sha256_hex, size_bytes).
pub async fn store(blob_dir: &str, data: &[u8]) -> (String, String, usize) {
    let hash = Sha256::digest(data);
    let hex = format!("{hash:x}");
    let key = format!("{hex}.tar.gz");
    let path = blob_path(blob_dir, &key);

    if let Some(parent) = path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .expect("Failed to create blob subdirectory");
    }

    tokio::fs::write(&path, data)
        .await
        .expect("Failed to write blob");
    (key, hex, data.len())
}

/// Read blob bytes from disk.
pub async fn read(blob_dir: &str, blob_key: &str) -> Option<Vec<u8>> {
    let path = blob_path(blob_dir, blob_key);
    tokio::fs::read(path).await.ok()
}
