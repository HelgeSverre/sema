-- For GitHub-linked packages, store a redirect URL instead of hosting blobs.
-- When tarball_url is set, the download endpoint returns a 302 redirect
-- instead of serving bytes from blob storage.
ALTER TABLE package_versions ADD COLUMN tarball_url TEXT;
