# Archived plans

Plans whose work has **shipped** or whose investigation **concluded** — kept for
historical context. The authoritative record of what landed is `CHANGELOG.md`,
`docs/adr.md`, and git history; these docs are the design thinking behind it.

Active (still-pending) plans live one level up in `docs/plans/`.

| Plan | Status |
| ---- | ------ |
| `2026-02-13-data-structure-crate-investigation.md` | Decided — `lasso` interning + `hashbrown` env/maps shipped (ADR #43/#44); the tree-walker hot path it targeted was retired. |
| `2026-02-16-compilation-strategy-investigation.md` | Built — recommended the stack bytecode VM, now Sema's sole evaluator (ADRs #50–#55). |
| `2026-05-15-adi-bytecode-verifier.md` | Shipped 1.17.0 — abstract stack-depth verifier (`verify_stack_balance`, ADR #56). |
| `2026-06-12-mcp-server.md` | Shipped 1.17.0 — `sema mcp` / `crates/sema-mcp`. |
| `2026-06-16-vm-module-loading.md` | Shipped — VM-native `load` (1.17.0) + `import` with module isolation (1.18.0). |
| `2026-06-18-retire-tree-walker.md` | Shipped 1.18.0 — bytecode VM is the sole evaluator. |
| `2026-06-18-retire-tree-walker-impl.md` | Shipped 1.18.0 — companion per-phase design. |
| `2026-06-18-c1-vm-hof-in-vm.md` | Shipped — HOF callbacks routed in-VM (commit `b75df47`). |
| `2026-06-09-notebook-output-hook-migration.md` | Shipped — notebook uses `set_stdout_hook`, dropped `gag`. |
| `2026-06-19-vm-web-worker-real-sleep.md` | Shipped 1.19.0 — real `async/sleep` in the playground worker. |
| `2026-06-09-lsp-followups-and-docs-research.md` | Superseded — docs audit replaced by the `sema-docs` crate. Three live stragglers (hover precedence, range formatting, DAP conditional breakpoints) were moved to `docs/wip.md` ("LSP / DAP follow-ups"). |
| `2026-06-09-spur-transmute-cleanup.md` | Shipped (commit `6f141e3`) — centralized `spur_to_bits`/`bits_to_spur` helpers replace all scattered `transmute::<u32, Spur>` sites; round-trip test + `size_of::<Spur>` const guard, no bench regression. |
