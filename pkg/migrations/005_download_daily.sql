-- Replace raw per-download event log with daily aggregates per package+version.
-- The UNIQUE constraint enables UPSERT: INSERT ... ON CONFLICT DO UPDATE SET count = count + 1.
DROP TABLE IF EXISTS download_log;

CREATE TABLE IF NOT EXISTS download_daily (
    package_name TEXT NOT NULL,
    version TEXT NOT NULL,
    download_date DATE NOT NULL,
    count INTEGER NOT NULL DEFAULT 0,
    UNIQUE(package_name, version, download_date)
);

CREATE INDEX IF NOT EXISTS idx_download_daily_pkg ON download_daily(package_name);
CREATE INDEX IF NOT EXISTS idx_download_daily_date ON download_daily(download_date);
