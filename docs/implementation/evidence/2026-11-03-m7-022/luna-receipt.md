# M7-022 D-T04 Luna receipt

- Worktree/branch: `/tmp/triomics-m7-022-luna`, `codex/m7-022-luna`.
- Base: `c78555ae50688089075913a1d27c3ff72abc18a1`; final: `36abc874da479b4c6397d598056461db730d9c26`.
- Candidate commits: `608b14f` public D-T04 synthetic fixture; `890cf46` measured-universe ORA implementation; `225529a` native-lineage/test fixture strengthening; `36abc87` strict typed JSON protocol boundary.
- Changed only the packet allowlist: `R/annotation_apply.R`, `R/enrichment_ora.R`, `R/enrichment_family.R`, D-T04 registration, D-T04 ticket, public `inst/examples/data/d-ora/*`, and `tests/testthat/test-D-ora-shared.R`. No CLI, NAMESPACE, manual, lock, or frozen spec edits.
- Focused suite: `Rscript --vanilla -e "testthat::test_local(filter='(D-T04|D-ora-shared)', stop_on_failure=TRUE)"` — 122 PASS, 0 FAIL, 0 WARN, 0 SKIP (98.2 s).
- Native synthetic B→C→D test uses a deterministic D-T04-only signal with original q/effect thresholds. It resolves the real B `score_slope` family and nomination from verified native evidence, plus C `d_t04_concat_pca`; ORA readback gives N/K/n/k=10/3/1/1, P=Q=.3, 3 declared sets, 1 estimable, and valid protocol/mapping/set/universe bindings.
- Protocol tests reject duplicate top-level/claim/set keys; reject non-string identities, non-array claims/set families, and numeric/boolean/null/object/nested member values; accept empty member arrays and two separate claim families. Native evidence route rejects a duplicate-key protocol while preserving the existing D pointer hash.
- Resource tamper proof: a changed source copy succeeds from a valid pinned cache; after cache removal, the source hash mismatch exits 2 and leaves the previous current-D pointer and payload hash unchanged.
- Scoped regressions: D-foundation-shared 80 PASS; D-T01 325 PASS/1 expected `system2` status-2 warning at `tests/testthat/test-D-T01.R:962`; D-T02 87 PASS; D-T03 62 PASS. Aggregate 554 PASS, 0 FAIL, 1 WARN, 0 SKIP (400.3 s).
- `git diff --check c78555ae50688089075913a1d27c3ff72abc18a1..HEAD` passed; worktree clean. No full package suite, push, or scientific-validity claim.
