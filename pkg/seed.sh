#!/usr/bin/env bash
set -euo pipefail

# ── Configuration ──
#
# This script seeds TWO things and they must line up:
#   1. The HTTP API at $BASE_URL  (curl)        — users, tokens, packages, reports
#   2. The SQLite database         (db_exec)    — promotes the first admin, which the
#                                                 API cannot do (chicken-and-egg).
#
# SEED_MODE controls how the SQLite step reaches the database:
#   local  (default) — server started with `cargo run`; DB is the local file $DB.
#   docker           — server runs in the `registry` compose service; the SQL runs
#                      *inside* that container so it shares the server's filesystem
#                      (writing to a container-owned WAL DB from the host is unsafe).
#
# Usage:
#   bash seed.sh                 # seed a registry that is already running
#   bash seed.sh --wait          # wait for the server to come up first, then seed
#   SEED_MODE=docker bash seed.sh --wait
BASE="${BASE_URL:-http://localhost:3000}"
DB="${DB_PATH:-data/registry.db}"
SEED_MODE="${SEED_MODE:-local}"
COMPOSE_SERVICE="${COMPOSE_SERVICE:-registry}"
CONTAINER_DB="${CONTAINER_DB:-/app/data/registry.db}"
WAIT_TIMEOUT="${WAIT_TIMEOUT:-180}"

WAIT=0
for arg in "$@"; do
  case "$arg" in
    --wait) WAIT=1 ;;
    *) echo "Unknown argument: $arg" >&2; exit 2 ;;
  esac
done

# Run a SQL statement against the registry database, routed by SEED_MODE.
db_exec() {
  case "$SEED_MODE" in
    docker) docker compose exec -T "$COMPOSE_SERVICE" sqlite3 "$CONTAINER_DB" "$1" ;;
    *)      sqlite3 "$DB" "$1" ;;
  esac
}

echo "=== Sema Package Registry — Dev Seed ==="
echo "Server:   $BASE"
echo "Mode:     $SEED_MODE"
echo "Database: $([ "$SEED_MODE" = docker ] && echo "$COMPOSE_SERVICE:$CONTAINER_DB" || echo "$DB")"
echo ""

# ── Wait for the server (or fail fast) ──
wait_for_server() {
  local deadline=$(( SECONDS + WAIT_TIMEOUT ))
  if curl -sf "$BASE/healthz" > /dev/null 2>&1; then return 0; fi
  [ "$WAIT" -eq 1 ] || return 1
  echo "Waiting for server at $BASE (up to ${WAIT_TIMEOUT}s)..."
  while [ "$SECONDS" -lt "$deadline" ]; do
    sleep 1
    if curl -sf "$BASE/healthz" > /dev/null 2>&1; then
      echo "Server is up."
      return 0
    fi
  done
  return 1
}

if ! wait_for_server; then
  echo "ERROR: Server not running at $BASE"
  if [ "$SEED_MODE" = docker ]; then
    echo "Start it first: docker compose up --build -d   (or: make dev-docker)"
  else
    echo "Start it first: cargo run   (or: make dev)"
  fi
  exit 1
fi

# ── Helper functions ──

register() {
  local user="$1" email="$2" pass="$3"
  local res
  res=$(curl -sf -X POST "$BASE/api/v1/auth/register" \
    -H "Content-Type: application/json" \
    -d "{\"username\":\"$user\",\"email\":\"$email\",\"password\":\"$pass\"}" \
    -D - -o /dev/null 2>/dev/null | grep -i set-cookie | head -1)
  local session
  session=$(echo "$res" | sed -n 's/.*session=\([^;]*\).*/\1/p')
  echo "$session"
}

login() {
  local user="$1" pass="$2"
  local res
  res=$(curl -sf -X POST "$BASE/api/v1/auth/login" \
    -H "Content-Type: application/json" \
    -d "{\"username\":\"$user\",\"password\":\"$pass\"}" \
    -D - -o /dev/null 2>/dev/null | grep -i set-cookie | head -1)
  local session
  session=$(echo "$res" | sed -n 's/.*session=\([^;]*\).*/\1/p')
  echo "$session"
}

create_token() {
  local session="$1" name="$2"
  curl -sf -X POST "$BASE/api/v1/tokens" \
    -H "Content-Type: application/json" \
    -H "Cookie: session=$session" \
    -d "{\"name\":\"$name\"}" | grep -o '"token":"[^"]*"' | head -1 | cut -d'"' -f4
}

publish() {
  local token="$1" name="$2" ver="$3" desc="$4"
  local boundary="seedboundary"
  local tmpfile
  tmpfile=$(mktemp)
  local meta="{\"description\":\"$desc\"}"

  # Build multipart body in a temp file to avoid printf/shell escaping issues
  {
    printf -- "--%s\r\nContent-Disposition: form-data; name=\"metadata\"\r\n\r\n%s\r\n" "$boundary" "$meta"
    printf -- "--%s\r\nContent-Disposition: form-data; name=\"tarball\"; filename=\"pkg.tar.gz\"\r\nContent-Type: application/gzip\r\n\r\nfake-tarball-%s-%s\r\n" "$boundary" "$name" "$ver"
    printf -- "--%s--\r\n" "$boundary"
  } > "$tmpfile"

  curl -sf -X PUT "$BASE/api/v1/packages/$name/$ver" \
    -H "Authorization: Bearer $token" \
    -H "Content-Type: multipart/form-data; boundary=$boundary" \
    --data-binary "@$tmpfile" > /dev/null

  rm -f "$tmpfile"
}

submit_report() {
  local session="$1" target_type="$2" target_name="$3" report_type="$4" reason="$5"
  curl -sf -X POST "$BASE/api/v1/reports" \
    -H "Content-Type: application/json" \
    -H "Cookie: session=$session" \
    -d "{\"target_type\":\"$target_type\",\"target_name\":\"$target_name\",\"report_type\":\"$report_type\",\"reason\":\"$reason\"}" > /dev/null
}

# ── Register users ──

echo "Registering users..."
HELGE_SESSION=$(register "helge" "helge@sema-lang.com" "123123123")
KARI_SESSION=$(register "kari" "kari@example.com" "123123123")
MAGNUS_SESSION=$(register "magnus" "magnus@dev.no" "123123123")
SPAM_SESSION=$(register "spambot" "spam@bad.com" "123123123")
echo "  helge, kari, magnus, spambot"

# ── Promote helge to admin ──

echo "Promoting helge to admin..."
db_exec "UPDATE users SET is_admin = 1 WHERE username = 'helge'"

# ── Create API tokens ──

echo "Creating API tokens..."
HELGE_TOKEN=$(create_token "$HELGE_SESSION" "dev-token")
KARI_TOKEN=$(create_token "$KARI_SESSION" "dev-token")
SPAM_TOKEN=$(create_token "$SPAM_SESSION" "spam-token")
echo "  helge, kari, spambot"

# ── Publish packages ──

echo "Publishing packages..."
publish "$HELGE_TOKEN" "sema-http" "1.0.0" "HTTP client and server library for Sema"
publish "$HELGE_TOKEN" "sema-http" "1.1.0" "HTTP client and server library for Sema"
publish "$HELGE_TOKEN" "sema-http" "2.0.0" "HTTP client and server library for Sema"
publish "$HELGE_TOKEN" "sema-json" "1.0.0" "JSON encode/decode utilities"
publish "$HELGE_TOKEN" "sema-json" "1.1.0" "JSON encode/decode utilities"
publish "$KARI_TOKEN" "sema-csv" "0.5.0" "CSV reader and writer"
publish "$KARI_TOKEN" "sema-xml" "1.0.0" "XML parser and generator"
publish "$SPAM_TOKEN" "free-robux" "9.9.9" "Totally not a scam package"
publish "$SPAM_TOKEN" "bitcoin-miner" "1.0.0" "Definitely legitimate utilities"
echo "  sema-http (3v), sema-json (2v), sema-csv, sema-xml, free-robux, bitcoin-miner"

# ── Yank a version ──

echo "Yanking sema-http v1.0.0..."
curl -sf -X POST "$BASE/api/v1/packages/sema-http/1.0.0/yank" \
  -H "Authorization: Bearer $HELGE_TOKEN" > /dev/null

# ── Ban spambot ──

echo "Banning spambot..."
SPAM_ID=$(db_exec "SELECT id FROM users WHERE username = 'spambot'")
# Need to re-login as admin since sessions are per-registration
HELGE_SESSION=$(login "helge" "123123123")
curl -sf -X POST "$BASE/api/v1/admin/users/$SPAM_ID/ban" \
  -H "Cookie: session=$HELGE_SESSION" \
  -H "Content-Type: application/json" \
  -d '{"reason":"Spam packages and suspicious activity"}' > /dev/null

# ── Submit reports ──

echo "Submitting reports..."
KARI_SESSION=$(login "kari" "123123123")
submit_report "$KARI_SESSION" "package" "free-robux" "spam" "Obvious spam package with no legitimate code"
submit_report "$KARI_SESSION" "package" "bitcoin-miner" "malware" "Package name suggests cryptocurrency mining, suspicious binary payload"
MAGNUS_SESSION=$(login "magnus" "123123123")
submit_report "$MAGNUS_SESSION" "user" "spambot" "abuse" "Multiple spam packages published, appears to be a bot account"

echo ""
echo "=== Seed complete ==="
echo ""
echo "Admin login: helge / 123123123"
echo "Admin panel: $BASE/admin"
echo ""
echo "Users: helge (admin), kari, magnus, spambot (banned)"
echo "Packages: sema-http (3v, 1 yanked), sema-json (2v), sema-csv, sema-xml, free-robux, bitcoin-miner"
echo "Reports: 3 open (2 package, 1 user)"
