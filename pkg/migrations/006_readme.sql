-- Store README content for packages (fetched from GitHub)
ALTER TABLE packages ADD COLUMN readme_raw TEXT;
ALTER TABLE packages ADD COLUMN readme_html TEXT;
