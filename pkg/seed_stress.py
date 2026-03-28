#!/usr/bin/env python3
"""
Stress-seed script for the Sema package registry.

Populates the database with ~1000 users, ~500 packages, ~2000 versions,
~200 reports, ~5000 audit log entries, and ~10000 downloads (aggregated daily).

This script inserts bulk synthetic data directly via sqlite3.
It does NOT create the admin user (helge) -- run seed.sh first for that.

Usage:
    python3 seed_stress.py [db_path]

    db_path defaults to data/registry.db
"""

import hashlib
import os
import random
import sqlite3
import sys
import time
from datetime import datetime, timedelta, timezone

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

NUM_USERS = 1000
NUM_PACKAGES = 500
NUM_DOWNLOADS = 10000
NUM_REPORTS = 200
NUM_AUDIT_EXTRA = 5000  # extra audit entries beyond auto-generated ones

PREFIXES = [
    "dev", "code", "sys", "net", "data", "api", "cli", "lib", "web", "app",
    "io", "ops", "ml", "pkg", "mod",
]

SUFFIXES = [
    "ninja", "wizard", "builder", "smith", "craft", "works", "labs", "hub",
    "flow", "byte", "bit", "stack", "forge", "dock", "wave",
]

EMAIL_DOMAINS = ["dev.io", "code.org", "example.com", "labs.net", "test.io", "hack.dev"]

ADJECTIVES = [
    "fast", "tiny", "async", "lazy", "smart", "safe", "slim", "bold",
    "quick", "lean", "pure", "deep", "flat", "warm", "cool", "nice",
    "dark", "lite", "mega", "mini",
]

NOUNS = [
    "http", "json", "csv", "sql", "auth", "cache", "log", "queue",
    "hash", "pool", "mail", "file", "task", "pipe", "grid", "tree",
    "heap", "ring", "lock", "stream",
]

REPORT_TYPES = ["spam", "malware", "abuse", "other"]

REPORT_REASONS = [
    "This package contains suspicious binary payloads",
    "Name squatting -- not a real Sema library",
    "Spam content in description",
    "Appears to be a malware distribution vector",
    "Package name is misleading and deceptive",
    "Contains obfuscated code that phones home to unknown servers",
    "Duplicate of an existing package with typosquatted name",
    "License violation -- bundles proprietary code without permission",
    "Description contains advertising links and SEO spam",
    "Maintainer account appears to be compromised",
    "Package installs cryptocurrency mining software",
    "README contains phishing links",
    "No actual Sema code -- just a placeholder to reserve the name",
    "Offensive or inappropriate package name and description",
    "Publishes user data to third-party analytics without consent",
]

AUDIT_ACTIONS = [
    "register", "publish", "yank", "ban_user", "webhook_sync",
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def random_datetime(days_ago_max: int, days_ago_min: int = 0) -> str:
    """Return a random ISO timestamp between days_ago_min and days_ago_max days ago."""
    now = datetime.now(timezone.utc)
    delta = timedelta(
        seconds=random.randint(days_ago_min * 86400, days_ago_max * 86400)
    )
    dt = now - delta
    return dt.strftime("%Y-%m-%d %H:%M:%S")


def random_date(days_ago_max: int, days_ago_min: int = 0) -> str:
    """Return a random date string (YYYY-MM-DD)."""
    now = datetime.now(timezone.utc)
    delta = timedelta(days=random.randint(days_ago_min, days_ago_max))
    dt = now - delta
    return dt.strftime("%Y-%m-%d")


def random_hex(length: int = 64) -> str:
    return hashlib.sha256(random.randbytes(32)).hexdigest()[:length]


def generate_usernames(n: int) -> list[str]:
    """Generate n unique usernames from prefix-suffix combinations."""
    names = set()
    # First generate all bare prefix-suffix combos
    for p in PREFIXES:
        for s in SUFFIXES:
            names.add(f"{p}-{s}")
    # Then add numbered variants until we have enough
    num = 1
    while len(names) < n:
        p = random.choice(PREFIXES)
        s = random.choice(SUFFIXES)
        names.add(f"{p}-{s}{num}")
        num += 1
    return sorted(names)[:n]


def generate_package_names(n: int) -> list[str]:
    """Generate n unique package names from adj-noun combinations."""
    names = set()
    for a in ADJECTIVES:
        for noun in NOUNS:
            names.add(f"sema-{a}-{noun}")
    # Should be 20*20 = 400, add numbered variants for the rest
    num = 2
    while len(names) < n:
        a = random.choice(ADJECTIVES)
        noun = random.choice(NOUNS)
        names.add(f"sema-{a}-{noun}{num}")
        num += 1
    return sorted(names)[:n]


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    db_path = sys.argv[1] if len(sys.argv) > 1 else "data/registry.db"

    if not os.path.exists(db_path):
        print(f"ERROR: Database not found at {db_path}")
        print("Run migrations first, then seed.sh, then this script.")
        sys.exit(1)

    print(f"=== Sema Registry Stress Seed ===")
    print(f"Database: {db_path}")
    print()

    conn = sqlite3.connect(db_path)
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")

    rng = random.Random(42)  # deterministic seed for reproducibility
    random.seed(42)

    t0 = time.time()

    # ------------------------------------------------------------------
    # 1. Users
    # ------------------------------------------------------------------
    print(f"Inserting {NUM_USERS} users...", end=" ", flush=True)

    usernames = generate_usernames(NUM_USERS)
    user_rows = []
    for i, username in enumerate(usernames):
        domain = EMAIL_DOMAINS[i % len(EMAIL_DOMAINS)]
        email = f"{username}@{domain}"
        created_at = random_datetime(180)
        # ~2% of users are banned
        banned_at = random_datetime(90) if random.random() < 0.02 else None
        # password_hash = NULL (GitHub-only users, can't login)
        user_rows.append((username, email, None, None, created_at, 0, banned_at))

    conn.executemany(
        "INSERT INTO users (username, email, password_hash, github_id, created_at, is_admin, banned_at) "
        "VALUES (?, ?, ?, ?, ?, ?, ?)",
        user_rows,
    )
    conn.commit()

    # Fetch all user IDs (including pre-existing ones from seed.sh)
    user_ids = [row[0] for row in conn.execute("SELECT id FROM users").fetchall()]
    # Only bulk-inserted user IDs (for ownership assignment)
    bulk_user_ids = [
        row[0] for row in conn.execute(
            "SELECT id FROM users WHERE password_hash IS NULL"
        ).fetchall()
    ]
    # All user IDs for reports/downloads
    all_user_ids = user_ids

    banned_users = [
        (row[0], row[1]) for row in conn.execute(
            "SELECT id, username FROM users WHERE banned_at IS NOT NULL AND password_hash IS NULL"
        ).fetchall()
    ]

    elapsed = time.time() - t0
    print(f"done ({len(user_rows)} inserted, {len(banned_users)} banned) [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # 2. Packages
    # ------------------------------------------------------------------
    print(f"Inserting {NUM_PACKAGES} packages...", end=" ", flush=True)
    t1 = time.time()

    package_names = generate_package_names(NUM_PACKAGES)
    package_rows = []
    package_meta = []  # (name, adj, noun, owner_id, source, created_at)

    for name in package_names:
        # Parse adj and noun back out of the name
        parts = name.replace("sema-", "", 1).split("-", 1)
        adj = parts[0] if len(parts) > 0 else "fast"
        noun = parts[1] if len(parts) > 1 else "lib"
        owner_id = random.choice(bulk_user_ids) if bulk_user_ids else random.choice(all_user_ids)
        source = "github" if random.random() < 0.2 else "upload"
        created_at = random_datetime(150, 30)
        description = f"A {adj} {noun} library for the Sema programming language"
        repo_url = f"https://github.com/sema-pkg/{name}" if source == "github" else None
        github_repo = f"sema-pkg/{name}" if source == "github" else None

        package_rows.append((name, description, repo_url, created_at, source, github_repo))
        package_meta.append((name, adj, noun, owner_id, source, created_at))

    conn.executemany(
        "INSERT INTO packages (name, description, repository_url, created_at, source, github_repo) "
        "VALUES (?, ?, ?, ?, ?, ?)",
        package_rows,
    )
    conn.commit()

    # Fetch package IDs
    pkg_id_map = {}
    for row in conn.execute("SELECT id, name FROM packages").fetchall():
        pkg_id_map[row[1]] = row[0]

    elapsed = time.time() - t1
    print(f"done ({len(package_rows)} inserted) [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # 3. Owners
    # ------------------------------------------------------------------
    print("Inserting package owners...", end=" ", flush=True)
    t1 = time.time()

    owner_rows = []
    for name, adj, noun, owner_id, source, created_at in package_meta:
        pkg_id = pkg_id_map.get(name)
        if pkg_id is None:
            continue
        # Primary owner
        owner_rows.append((pkg_id, owner_id))
        # 1-2 additional co-owners for ~30% of packages
        if random.random() < 0.3:
            extra = random.randint(1, 2)
            for _ in range(extra):
                co_owner = random.choice(bulk_user_ids) if bulk_user_ids else random.choice(all_user_ids)
                if co_owner != owner_id:
                    owner_rows.append((pkg_id, co_owner))

    # Deduplicate (package_id, user_id) pairs
    owner_rows = list(set(owner_rows))

    conn.executemany(
        "INSERT OR IGNORE INTO owners (package_id, user_id) VALUES (?, ?)",
        owner_rows,
    )
    conn.commit()

    elapsed = time.time() - t1
    print(f"done ({len(owner_rows)} rows) [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # 4. Package Versions
    # ------------------------------------------------------------------
    print("Inserting package versions...", end=" ", flush=True)
    t1 = time.time()

    version_rows = []
    version_meta = []  # for audit log: (package_name, version, published_at, yanked)

    for name, adj, noun, owner_id, source, pkg_created in package_meta:
        pkg_id = pkg_id_map.get(name)
        if pkg_id is None:
            continue
        num_versions = random.randint(1, 8)
        # Generate sorted semver versions
        versions = set()
        while len(versions) < num_versions:
            major = random.randint(0, 3)
            minor = random.randint(0, 15)
            patch = random.randint(0, 20)
            versions.add((major, minor, patch))

        sorted_versions = sorted(versions)
        for i, (major, minor, patch) in enumerate(sorted_versions):
            ver_str = f"{major}.{minor}.{patch}"
            checksum = random_hex(64)
            blob_key = f"{random_hex(16)}.tar.gz"
            size_bytes = random.randint(1000, 500000)
            yanked = 1 if random.random() < 0.10 else 0
            # Later versions get later dates
            days_offset = 120 - int((i / max(len(sorted_versions) - 1, 1)) * 90)
            published_at = random_datetime(days_offset, max(0, days_offset - 30))
            sema_req = f">={random.choice(['0.5', '0.6', '0.7', '0.8', '0.9', '1.0'])}"

            version_rows.append((
                pkg_id, ver_str, checksum, blob_key, size_bytes,
                yanked, sema_req, published_at,
            ))
            version_meta.append((name, ver_str, published_at, yanked, owner_id))

    conn.executemany(
        "INSERT OR IGNORE INTO package_versions "
        "(package_id, version, checksum_sha256, blob_key, size_bytes, yanked, sema_version_req, published_at) "
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        version_rows,
    )
    conn.commit()

    total_versions = conn.execute("SELECT count(*) FROM package_versions").fetchone()[0]
    yanked_count = conn.execute("SELECT count(*) FROM package_versions WHERE yanked = 1").fetchone()[0]

    elapsed = time.time() - t1
    print(f"done ({len(version_rows)} inserted, {yanked_count} yanked) [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # 5. Download Daily
    # ------------------------------------------------------------------
    print(f"Inserting {NUM_DOWNLOADS} downloads (aggregated by day)...", end=" ", flush=True)
    t1 = time.time()

    # Weight downloads toward a few "popular" packages
    popular_count = min(50, len(package_names))
    popular_pkgs = package_names[:popular_count]
    other_pkgs = package_names[popular_count:]

    # Build version lookup for download daily
    pkg_versions = {}
    for row in conn.execute("SELECT p.name, pv.version FROM package_versions pv JOIN packages p ON p.id = pv.package_id").fetchall():
        pkg_versions.setdefault(row[0], []).append(row[1])

    # Aggregate downloads by (package_name, version, date)
    download_counts: dict[tuple[str, str, str], int] = {}
    for _ in range(NUM_DOWNLOADS):
        # 70% downloads go to popular packages
        if random.random() < 0.7 and popular_pkgs:
            pkg_name = random.choice(popular_pkgs)
        elif other_pkgs:
            pkg_name = random.choice(other_pkgs)
        else:
            pkg_name = random.choice(package_names)

        versions = pkg_versions.get(pkg_name, ["1.0.0"])
        version = random.choice(versions)
        download_date = random_date(60)
        key = (pkg_name, version, download_date)
        download_counts[key] = download_counts.get(key, 0) + 1

    download_rows = [
        (pkg_name, version, date, count)
        for (pkg_name, version, date), count in download_counts.items()
    ]

    conn.executemany(
        "INSERT INTO download_daily (package_name, version, download_date, count) VALUES (?, ?, ?, ?)",
        download_rows,
    )
    conn.commit()

    elapsed = time.time() - t1
    print(f"done ({len(download_rows)} rows from {NUM_DOWNLOADS} downloads) [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # 6. Reports
    # ------------------------------------------------------------------
    print(f"Inserting {NUM_REPORTS} reports...", end=" ", flush=True)
    t1 = time.time()

    report_rows = []
    for _ in range(NUM_REPORTS):
        reporter_id = random.choice(all_user_ids)
        # 80% package reports, 20% user reports
        if random.random() < 0.8:
            target_type = "package"
            target_name = random.choice(package_names)
        else:
            target_type = "user"
            target_name = random.choice(usernames)

        report_type = random.choice(REPORT_TYPES)
        reason = random.choice(REPORT_REASONS)

        # Status distribution: 80% open, 10% actioned, 10% dismissed
        r = random.random()
        if r < 0.80:
            status = "open"
            resolved_by = None
            resolved_at = None
        elif r < 0.90:
            status = "actioned"
            resolved_by = random.choice(all_user_ids)
            resolved_at = random_datetime(30)
        else:
            status = "dismissed"
            resolved_by = random.choice(all_user_ids)
            resolved_at = random_datetime(30)

        created_at = random_datetime(90)

        report_rows.append((
            reporter_id, target_type, target_name, report_type,
            reason, status, resolved_by, resolved_at, created_at,
        ))

    conn.executemany(
        "INSERT INTO reports "
        "(reporter_id, target_type, target_name, report_type, reason, status, resolved_by, resolved_at, created_at) "
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        report_rows,
    )
    conn.commit()

    elapsed = time.time() - t1
    print(f"done [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # 7. Audit Log
    # ------------------------------------------------------------------
    print("Inserting audit log entries...", end=" ", flush=True)
    t1 = time.time()

    audit_rows = []

    # Register events for each bulk user
    for username in usernames:
        audit_rows.append((
            username, "register", "user", username, None, random_datetime(180),
        ))

    # Publish events for each version
    for pkg_name, ver_str, published_at, yanked, owner_id in version_meta:
        # Look up username for owner_id
        row = conn.execute("SELECT username FROM users WHERE id = ?", (owner_id,)).fetchone()
        actor = row[0] if row else "unknown"
        audit_rows.append((
            actor, "publish", "package_version", f"{pkg_name}@{ver_str}",
            f'{{"size_bytes": {random.randint(1000, 500000)}}}',
            published_at,
        ))

    # Yank events for yanked versions
    for pkg_name, ver_str, published_at, yanked, owner_id in version_meta:
        if not yanked:
            continue
        row = conn.execute("SELECT username FROM users WHERE id = ?", (owner_id,)).fetchone()
        actor = row[0] if row else "unknown"
        audit_rows.append((
            actor, "yank", "package_version", f"{pkg_name}@{ver_str}", None,
            random_datetime(60),
        ))

    # Ban events for banned users
    for user_id, username in banned_users:
        audit_rows.append((
            "helge", "ban_user", "user", username,
            '{"reason": "Automated stress-test ban"}',
            random_datetime(90),
        ))

    # Fill remaining audit slots with webhook_sync events
    remaining = NUM_AUDIT_EXTRA - len(audit_rows)
    if remaining < 0:
        remaining = 0
    github_pkgs = [name for name, _, _, _, source, _ in package_meta if source == "github"]
    for _ in range(remaining):
        action = random.choice(["webhook_sync", "publish", "register"])
        if action == "webhook_sync" and github_pkgs:
            pkg_name = random.choice(github_pkgs)
            audit_rows.append((
                "github-webhook", "webhook_sync", "package", pkg_name,
                '{"tag": "v' + f'{random.randint(0,3)}.{random.randint(0,15)}.{random.randint(0,20)}' + '"}',
                random_datetime(90),
            ))
        elif action == "publish":
            pkg_name = random.choice(package_names)
            ver = f"{random.randint(0,3)}.{random.randint(0,15)}.{random.randint(0,20)}"
            actor = random.choice(usernames)
            audit_rows.append((
                actor, "publish", "package_version", f"{pkg_name}@{ver}",
                None, random_datetime(90),
            ))
        else:
            actor = random.choice(usernames)
            audit_rows.append((
                actor, "register", "user", actor, None, random_datetime(180),
            ))

    conn.executemany(
        "INSERT INTO audit_log (actor, action, target_type, target_name, detail, created_at) "
        "VALUES (?, ?, ?, ?, ?, ?)",
        audit_rows,
    )
    conn.commit()

    elapsed = time.time() - t1
    print(f"done ({len(audit_rows)} entries) [{elapsed:.1f}s]")

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------
    total_elapsed = time.time() - t0

    counts = {}
    for table in ["users", "packages", "package_versions", "owners", "download_daily", "reports", "audit_log"]:
        counts[table] = conn.execute(f"SELECT count(*) FROM {table}").fetchone()[0]

    conn.close()

    print()
    print(f"=== Stress Seed Complete ({total_elapsed:.1f}s) ===")
    print()
    print(f"  Users:            {counts['users']:>6}")
    print(f"  Packages:         {counts['packages']:>6}")
    print(f"  Versions:         {counts['package_versions']:>6}")
    print(f"  Owners:           {counts['owners']:>6}")
    print(f"  Downloads:        {counts['download_daily']:>6}")
    print(f"  Reports:          {counts['reports']:>6}")
    print(f"  Audit log:        {counts['audit_log']:>6}")
    print()


if __name__ == "__main__":
    main()
