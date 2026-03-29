#!/usr/bin/env bash
set -euo pipefail

BASE="${BASE_URL:-http://localhost:3000}"
DB="${DB_PATH:-data/registry.db}"

echo "=== Sema Package Registry — Dev Seed ==="
echo "Server: $BASE"
echo "Database: $DB"
echo ""

# Check server is running
if ! curl -sf "$BASE/healthz" > /dev/null 2>&1; then
  echo "ERROR: Server not running at $BASE"
  echo "Start it first: cargo run"
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
sqlite3 "$DB" "UPDATE users SET is_admin = 1 WHERE username = 'helge'"

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
SPAM_ID=$(sqlite3 "$DB" "SELECT id FROM users WHERE username = 'spambot'")
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
