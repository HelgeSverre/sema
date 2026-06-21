# Cut a Release

Run the Sema release procedure end-to-end. `$ARGUMENTS` may contain the target
version (e.g. `1.21.2`) or a bump kind (`patch`/`minor`/`major`); if absent,
infer the bump from the unreleased changes and confirm before tagging.

This mirrors **Release Procedure** in `CLAUDE.md`. Do the steps in order and stop
on the first failure — never tag or push a red build.

## Step 1 — Pre-flight (must be green)

```bash
cargo test 2>&1 | grep -E 'test result: FAILED|FAILED|error\[|panicked'   # expect no output
make lint                                                                  # fmt-check + clippy -D warnings
git status --short                                                         # know what's uncommitted
```

If tests or lint fail, fix first. Make sure all intended changes are committed
(the release commit should only bump the version + Cargo.lock).

## Step 2 — Choose the version

Read the top of `CHANGELOG.md` and the current `version` in `Cargo.toml`. Decide
the new `X.Y.Z`:
- **patch** — bug fixes only
- **minor** — new user-facing features / additive APIs
- **major** — breaking changes

If `$ARGUMENTS` didn't pin a version, state your choice and the reasoning.

## Step 3 — Bump the version (13 lines: workspace + 12 inter-crate pins)

The #1 release footgun: there are 12 `=OLD` inter-crate pins **plus** the
workspace version. Bump all 13 in one shot, then verify the counts:

```bash
OLD=<current>; NEW=<target>
sed -i '' -e "s/^version = \"$OLD\"/version = \"$NEW\"/" -e "s/version = \"=$OLD\"/version = \"=$NEW\"/g" Cargo.toml
grep -c "$NEW" Cargo.toml   # expect 13
grep -c "$OLD" Cargo.toml   # expect 0
```

## Step 4 — CHANGELOG

Prepend a `## X.Y.Z` section at the top of `CHANGELOG.md`. Group as
`### Fixed` / `### Added` / `### Improved`. Write for users: what changed and why
it matters, not a commit dump. Cite how each item was verified where relevant.

## Step 5 — Build & verify

```bash
cargo build --release 2>&1 | tail -1     # also refreshes Cargo.lock
./target/release/sema --version          # must print the NEW version
```

## Step 6 — Commit, tag, push

```bash
git add Cargo.toml Cargo.lock CHANGELOG.md
git commit -m "release: X.Y.Z"
git tag vX.Y.Z
git push origin main --tags
```

The push triggers 4 CI workflows (CI, Release/binaries, Publish to crates.io,
Publish to npm). Confirm they started:

```bash
sleep 12 && gh run list --limit 4
```

**If "Publish to crates.io" fails**, check whether it's a transient network error
(e.g. `curl failed` / `HTTP2 framing layer` / `download of config.json failed`)
vs. a real problem. The publish script is **idempotent** (skips crates already on
crates.io), so for a transient flake just re-run the failed job — it resumes from
the crate that failed:

```bash
gh run view <run-id> --log-failed | grep -iE 'error|Caused by'   # diagnose
gh run rerun <run-id> --failed                                    # retry transient flakes
```

## Step 7 — Deploy website (only if website content changed)

If this release changed anything under `website/` (docs, homepage, OG), deploy:

```bash
cd website && vercel --prod
```

Then verify the change is actually live (a **failed** Vercel build silently keeps
the previous deploy — production looks unchanged):

```bash
curl -s https://sema-lang.com/<changed-page>.html | grep -o '<expected new text>'
```

Mind the **monorepo deploy gotcha** (see `CLAUDE.md` → Website): the CLI uploads
only `website/`, so any import reaching outside it fails on Vercel.

## Done-when

- `./target/release/sema --version` == new version; `grep -c OLD Cargo.toml` == 0
- Tag pushed; all 4 CI workflows running/green
- If website changed: the new content is confirmed live via `curl`
