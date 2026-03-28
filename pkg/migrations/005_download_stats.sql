-- Download tracking for package version downloads
CREATE TABLE IF NOT EXISTS download_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    package_name TEXT NOT NULL,
    version TEXT NOT NULL,
    downloaded_at DATE NOT NULL DEFAULT (date('now'))
);

CREATE INDEX IF NOT EXISTS idx_download_log_package ON download_log(package_name);
CREATE INDEX IF NOT EXISTS idx_download_log_date ON download_log(downloaded_at);
