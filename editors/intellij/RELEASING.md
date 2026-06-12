# Releasing

How to sign and publish the **Sema** IntelliJ plugin to the
[JetBrains Marketplace](https://plugins.jetbrains.com).

The Gradle project root is `editors/intellij/` inside the [sema](https://github.com/HelgeSverre/sema)
monorepo. Run every command in this document from that directory.

## Decisions

- **Plugin ID** is `com.sema.language`, final. The plugin was never published to the Marketplace, so
  there is no migration concern and no orphaned listing.
- **First public version is `1.0.0`.** Any earlier `0.1.0`/`1.0.0`/`1.1.0` numbering was internal
  testing. Change-notes are sourced from `CHANGELOG.md` (do not hand-edit `<change-notes>` in
  `plugin.xml`).
- **`untilBuild` is intentionally omitted** (`provider { null }` in `build.gradle.kts`) for
  open-ended forward compatibility. Verified compatible from 2024.3 through 2026.x.
- **LSP4IJ experimental-API usage is accepted.** Several LSP4IJ APIs we rely on — the DAP
  descriptor classes (`DebugAdapterDescriptorFactory` / `DebugAdapterDescriptor`) and the client
  feature classes (`LSPClientFeatures`, `LSPCompletionFeature`, `LSPFormattingFeature`, etc.) — are
  marked `@ApiStatus.Experimental` in 0.19.4 with no stable alternative. The verifier reports these
  as non-blocking *experimental API usage* warnings (no compatibility problems).
- **Publishing is manual `workflow_dispatch`, not tag/release-driven** — see the monorepo note below.

`build.gradle.kts` is already wired for signing and publishing. It reads four values from the
environment:

| Variable | Purpose | Secret? |
| --- | --- | --- |
| `CERTIFICATE_CHAIN` | Public signing certificate chain | Public cert |
| `PRIVATE_KEY` | Encrypted RSA signing key | **Secret** |
| `PRIVATE_KEY_PASSWORD` | Passphrase for the key | **Secret** |
| `PUBLISH_TOKEN` | Marketplace upload token | **Secret** |

## Signing keys

The signing identity lives in `editors/intellij/.secrets/`. **The whole folder is gitignored**
(see `.gitignore`) — it must never be committed.

| File | What it is |
| --- | --- |
| `.secrets/private.pem` | 4096-bit RSA key, AES-256 encrypted |
| `.secrets/private_key_password.txt` | The key passphrase |
| `.secrets/chain.crt` | Self-signed certificate |
| `.secrets/signing.env` | Helper that exports the three signing vars for local use |

> **Back up `.secrets/` somewhere safe** (e.g. a password manager). It is gitignored, so it exists
> nowhere else. Losing it means regenerating the key and re-signing future releases.

### Generating the keys (first time, or if lost)

```bash
cd editors/intellij
mkdir -p .secrets && chmod 700 .secrets
openssl rand -base64 32 | tr -d '\n' > .secrets/private_key_password.txt
PW=$(cat .secrets/private_key_password.txt)
openssl genpkey -aes-256-cbc -algorithm RSA -out .secrets/private.pem \
  -pkeyopt rsa_keygen_bits:4096 -pass pass:"$PW"
openssl req -key .secrets/private.pem -passin pass:"$PW" -new -x509 -days 1825 \
  -out .secrets/chain.crt \
  -subj "/CN=Helge Sverre/O=Helge Sverre/C=NO/emailAddress=helge.sverre@gmail.com"
chmod 600 .secrets/*
```

Then create `.secrets/signing.env` so local signing/publishing can source the env vars:

```bash
cat > .secrets/signing.env <<'EOF'
export CERTIFICATE_CHAIN="$(cat "$(dirname "${BASH_SOURCE[0]}")/chain.crt")"
export PRIVATE_KEY="$(cat "$(dirname "${BASH_SOURCE[0]}")/private.pem")"
export PRIVATE_KEY_PASSWORD="$(cat "$(dirname "${BASH_SOURCE[0]}")/private_key_password.txt")"
# export PUBLISH_TOKEN="<your-marketplace-token>"   # add once you have it
EOF
```

See the JetBrains [plugin signing docs](https://plugins.jetbrains.com/docs/intellij/plugin-signing.html).

## GitHub Actions secrets

The release workflow (`.github/workflows/intellij-release.yml`) reads the four secrets from the
repo's **Settings → Secrets and variables → Actions**.

Upload the three signing secrets once the keys exist:

```bash
cd editors/intellij
gh secret set PRIVATE_KEY          < .secrets/private.pem
gh secret set CERTIFICATE_CHAIN    < .secrets/chain.crt
gh secret set PRIVATE_KEY_PASSWORD < .secrets/private_key_password.txt
```

Add the publish token once you have it (from
[plugins.jetbrains.com/author/me/tokens](https://plugins.jetbrains.com/author/me/tokens)):

```bash
gh secret set PUBLISH_TOKEN --body '<your-marketplace-token>'
```

## Local commands

```bash
cd editors/intellij
./gradlew test          # compile + run unit tests
./gradlew verifyPlugin  # IntelliJ Plugin Verifier against current + recommended IDEs
./gradlew buildPlugin   # unsigned ZIP  -> build/distributions/*.zip

# Signing / publishing need the env vars — source the helper first:
source .secrets/signing.env && ./gradlew signPlugin     # signed ZIP -> *-signed.zip
source .secrets/signing.env && ./gradlew publishPlugin  # sign + upload (needs PUBLISH_TOKEN)
```

> The IntelliJ Plugin Verifier reports ~11 *experimental API usage* warnings for the LSP4IJ DAP
> descriptors. These are expected and **not** failures — see
> `src/main/kotlin/com/sema/intellij/dap/SemaDebugAdapterDescriptorFactory.kt`.

## Versioning and changelog

- The plugin version is `pluginVersion` in `gradle.properties`.
- Change-notes are rendered into the plugin **automatically at build time** from `CHANGELOG.md`
  (the `org.jetbrains.changelog` Gradle plugin). Do not edit `<change-notes>` in `plugin.xml`.
- Add notes for the next release under the `## [Unreleased]` section of `CHANGELOG.md`.
- `./gradlew patchChangelog` moves `[Unreleased]` into a dated version section.
- The release channel is derived from the version: a plain version (e.g. `1.0.0`) publishes to the
  **default (stable)** channel; a pre-release suffix like `1.1.0-beta.1` publishes to `beta`.

## Monorepo note — why publishing is manual-dispatch

The sibling standalone plugin repos drive publishing off GitHub Releases / version tags. This
monorepo can't: the shared cargo-dist workflow (`.github/workflows/release.yml`) triggers on **any**
tag matching `**[0-9]+.[0-9]+.[0-9]+*`, and any IntelliJ release tag would match and cross-trigger a
Rust release. So the plugin is published via a manually-dispatched workflow that creates no tags and
no GitHub Releases. The shared Rust CI is left untouched.

## CI workflows

| Workflow | Trigger | Does |
| --- | --- | --- |
| `intellij-build.yml` | push to `main` / PR touching `editors/intellij/**` | Build, test (`check`), `verifyPlugin`; upload artifacts |
| `intellij-release.yml` | manual `workflow_dispatch` | `signPlugin` (dry-run) or `publishPlugin` → Marketplace; upload signed ZIP artifact |

## First release (manual)

The **first upload of a new plugin must be done by hand** and goes through JetBrains moderation
(typically a couple of business days). Automated publishing only works after that.

1. Complete the **manual smoke gate** below on a real IDE.
2. `./gradlew verifyPlugin` — confirm it passes.
3. `source .secrets/signing.env && ./gradlew signPlugin`.
4. Upload `build/distributions/sema-intellij-<version>-signed.zip` at
   [plugins.jetbrains.com → Upload plugin](https://plugins.jetbrains.com/plugin/add). Listing
   metadata (name, description, vendor, change-notes, icon) is taken from the plugin.
5. Wait for approval.

## Subsequent releases (automated)

Once the plugin exists on Marketplace and `PUBLISH_TOKEN` is set:

1. Bump `pluginVersion` in `gradle.properties` and add a `CHANGELOG.md` entry; push to `main`.
2. Run the **Release** workflow from the GitHub Actions tab
   (`intellij-release.yml` → *Run workflow*). Leave `dry_run` unchecked to publish; check it to only
   sign and upload the artifact for inspection.

## Manual smoke gate (run once on a real IDE before the first publish)

These need a real IDE GUI and are not covered by the automated `integrationTest` suite. Launch the
sandbox with `./gradlew runIde`, then verify:

**LSP** — open a `.sema` file and confirm: diagnostics, hover, completion, go-to-definition, folding,
inlay hints, code lenses, and semantic coloring. Trigger a code-lens eval and confirm inline
`sema/evalResult` rendering.

**DAP** — start a debug session for a `.sema` file. Confirm launch parameters include `program`,
`cwd`, and `stopOnEntry`, and that breakpoints, continue, step over/into/out, stack frames, scopes,
and variables work.

**Notebook** — create/open a `.sema-nb` file. Confirm JCEF loads the notebook URL, the server starts
on a loopback port and stops on editor/project disposal, run-all and export-to-Markdown behave
correctly, and the fallback panel works when JCEF is unavailable.
