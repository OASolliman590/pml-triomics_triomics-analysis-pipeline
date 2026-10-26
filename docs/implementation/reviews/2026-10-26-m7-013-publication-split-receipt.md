# M7-013 publication split — blocked, not integrated

## Frozen decision and audit

Base receipt: `55389dd`. M7-013 record commit:
`4e5ad08b608c9787355403df5370d34e57640883`; record SHA-256
`a50af758c28b2bb77dde6d6608d022b7ecb7e85cc32d7ea30d59b4c346df30a7`.
Fresh read-only `/root/m7_013_astra_audit`, requested gpt-6-astra/low, returned
**audit_pass yes**, Critical 0 / Important 0 / Minor 0. No documentation repair
or rereview used. This approves the bounded decision, not implementation
feasibility or acceptance. Receipt: `2026-10-26-m7-013-astra-audit.md`.

## Worker and Sol diff review

One fresh `/root/m7_013_luna`, requested gpt-6-luna/max, no commits or child agents.
Worktree: `/Users/megastore/.codex/worktrees/m7-013-c-publication-split/PML MultiOmics Analysis Pipeline`.
Base remains `425455b481329da95718b522edd879cf8c4793f9`.
Only dirty files are `tests/testthat/test-C-06-shared.R` (+70/-73),
`tests/testthat/test-C-06.R` (+28), and `tests/testthat/test-C-08-shared.R` (+18).
The C-08 edits are additive. There are no R product changes or ticket edits.
Sol inspected the actual diff; it is retained as RED evidence only, not applied
to integration. The replacement missing-B expectation expresses the newly
requested behavior; it is not a passing test or an accepted implementation.

Preserved patch: `evidence/2026-10-26-m7-013/luna-red-tests.patch`.
Patch SHA-256: `121fbfdd8a48b37102d91e875802f64ea9ee88b1d619882d98019eac15774df4`.

## Blocker and bounds

The nested C-02 caller inside `test-C-06-shared.R` explicitly requires D
manifest/evaluation artifacts. Endpoint-only routing would violate those
expectations. Running native B on its unchanged C-02 ticket fails:

```sh
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-02.yaml --stage B --output /tmp/triomics-m7-013-sol/c02-native-b
```

Sol independently reproduced exit 2:
`B_DESIGN_PREPARATION: Stage B reads the declared measurement matrix and does not refit preparation.`
The ticket declares block preparation `c02_fixture` (lines 60, 114) but analysis
preparation `c01_fixture` (line 148). `R/results_b.R:130–141` rejects that identity
mismatch. `inst/examples/tickets/C-02.yaml` and B code are outside the allowlist.
No compatible native predecessor for this unchanged caller was implemented.
The worker stopped; Sol did not relabel the caller, fabricate B evidence, change
its scientific contract, edit the excluded ticket, or add a missing-B bypass.
A new bounded decision must address this existing nested caller/fixture before
claiming the publication split preserves the required suite. This does not
claim native B production itself is broken or that broader solutions are impossible.

The C-06 ticket also uses unsupported `slope_per_score_unit`; its explicitly
allowed correction to `per_unit_beta` was identified but not applied after the
scope stop. No analysis threshold, input matrix or numerical seed changed.

## Independent verification

Sol ran on the returned dirty test snapshot at the worktree above:

- C-08 shared: `Rscript --vanilla -e "testthat::test_local(filter='^C-08-shared$', stop_on_failure=TRUE)"`.
  Exit 1; FAIL 2 / WARN 0 / SKIP 0 / PASS 60. Both existing fresh-root callers
  create `D/handoff-manifest.json`, violating the new additive assertion.
- C-06: full filter, `testthat::set_max_fails(Inf)` and `stop_on_failure=FALSE`
  to collect all failures, followed by an explicit nonzero process exit for
  failures/errors. Exit 1; FAIL 10 / WARN 0 / SKIP 0 / ERROR 0 / PASS 106.
- Native C-02 B command: exit 2 as above. No provenance produced.

Raw logs and the runner are retained under `evidence/2026-10-26-m7-013/`.
Luna's earlier C-06 run stopped at the default failure limit; it is not substituted
for Sol's complete diagnostic count. Old 290/52 results remain historical only.
The separate spec validator was attempted and failed because Python jsonschema
and PyYAML are unavailable; no fresh spec-validation pass is claimed.

No successful B-to-C publication/read-back exists. No new B predecessor or C
provenance payload/envelope was written, so provenance paths and SHA-256 values
are **none**. The independent read-back script was prepared but not executed
because there is no product candidate/frame. A D manifest produced by old code
in a failing test is not a validated provenance handoff.

## Disposition and literal resume

M7-013 is blocked and incomplete. Only documentation/evidence is committed;
no product or test integration. Preserve the dirty worker test diff and the
M7-011/M7-012 receipts. Do not redispatch their workers or treat them as complete.
No run-owned worker remains active; no second implementation attempt opened.
C-06 accepted=false, C-07 not_run, C-09 unqualified. M7, G7 and Stage D stay closed.
No private study data read; no GitHub push or Notion sync. No provenance/schema/D
consumer change; B-T04 bound 0.08 and seeds 1001:1400 unchanged.

Resume from this receipt only after an explicit bounded disposition for the
nested C-02 caller's native predecessor/fixture identity. Do not repeat the
completed M7-013 audit or call RED evidence provenance GREEN.
