# Implementation handover

Run `triomics-20260920-sol-01` continues on branch
`spec/harden-v2-20260920`. G0 through G5 are released. Stages A and B and C-01
through C-05 are accepted on synthetic software evidence. The decisive C-05
clean archive check at `c34d9ff` is `Status: OK`, with testthat FAIL 0, WARN
22, SKIP 0, PASS 4076. Eight warnings are the previously dispositioned B-T07
missing-Git-metadata probes; fourteen are native MOFA small-fixture/QC
diagnostics. The run is not described as warning-free. C-05 is
exploratory-only and explicitly does not support prediction. C-08 is the next
legal packet; C-09 and C-06 depend on it. G6 and every D packet remain closed.
The historical sections below keep their original wording.

## Current evidence

- B-T07 packet commits were integrated as `8cf2d38` and `4a33f01`.
- Same-run matrix retention and CLI/API registration were integrated as
  `f1ed853` and `c5f868f`.
- B-T07 repair 2 was integrated as `1146b5a`; its focused suite passed 124
  expectations and exact CLI AC3 passed. One Sol P1 was transparently retained
  and overruled by Astra as a P2 traceability ambiguity because the frozen
  contract permits verified aggregate unavailable status without the stripped
  optional method fields.
- M4-013 cross-packet composition candidate `57474e7` passed fresh Sol-high
  review and was integrated as `bd7da66`. B-T04/B-T07/B-T08/B-T09 plus the
  composition suite passed 870 focused expectations; four exact CLIs exited 0
  and 34 envelopes validated. Numerical results, B-T04 calibration and clinical
  privacy behavior were unchanged.
- The complete local suite on `bd7da66` passed with zero failures, warnings or
  skips.
- The clean Git-archive package check then failed with one ERROR, two WARNINGs
  and one NOTE: 15 installed B-T07 fixture-path/runner failures, one non-ASCII
  em dash in `R/export_b.R`, one unqualified `setNames()`, and duplicated
  `resume` documentation. Evidence:
  `reviews/M4-013-package-check-gate.md`.

These are packaging/test-infrastructure defects; no scientific regression was
demonstrated. They still cannot be waived. No private study data under the
authorized external data folder was read, fitted, copied, uploaded or committed.
All evidence remains public synthetic software evidence only.

## M4-014 authorization recorded

B-T07 consumed its initial attempt and two repairs. Omar explicitly authorized
the bounded M4-014 exception on 2026-10-16 after Sol restated the exact scope.
The exact record is
`change-records/M4-014-b-t07-package-gate-exception-proposal.md`. It permits only
`tests/testthat/test-B-T07.R`, the two mechanical `R/export_b.R` corrections,
and the Sol-owned duplicate Rd consolidation. It does not reset counters,
authorize a scientific change, or waive any verification gate.

Luna candidate `4acbd71` changed exactly `tests/testthat/test-B-T07.R` and
`R/export_b.R`; source and installed focused runs each passed 124 expectations
with zero failures or skips. Sol-owned manual cleanup is commit `7194b23`.
Fresh Sol-6.1 high review passed with P0/P1/P2 zero, and the candidate was
serially integrated as `01c5e49`. Fresh source verification then exposed a
path-with-spaces defect in the newly changed source CLI helper: Rscript receives
the absolute script argument with spaces transformed to `~+~`, so four source
CLI smokes failed before dispatch. Luna repair `74335d1` changed only the test
helper and passed 124 expectations from a spaced checkout, the integration
snapshot and an installed package, with zero failures or skips. Fresh focused
Sol-6.1 high re-review marked the finding ADDRESSED with no new P0/P1, and the
repair was integrated as `cbe5400`. Fresh integrated evidence at `d2a2231` is:
source B-T07 124/124 with no warnings/skips; installed B-T07 124/124 with no
failures/skips; full suite 3,462/3,462 with no warnings/skips; clean archive
package check `Status: OK` with no ERROR/WARNING/NOTE. The immutable Astra review
returned `changes_required` only because B-T01/B-T02/B-T03 lacked explicit
acceptance records. Sol applied record-only M4-015 after a fresh 749-expectation
combined pass; all three are accepted now, not retroactively. Fresh read-only
reviewer `/root/m4_g4_focused_astra` is active on record-only correction
snapshot `0f4be24` for the single focused rereview of M4-G4-001. That rereview
passed, and Sol accepted B-T07/G4 and released Stage C. No worker is active at
that G4 acceptance boundary; the current C-01 worker is recorded below. Older
completed managed worktrees remain archived.
The user-owned `.gitignore` and AppleDouble files remain untouched. The
git-ignored `.superpowers/` contains review packages only and remains untracked.

## 18 October specification re-evaluation

The documentation-only review in
`reviews/2026-10-18-spec-kit-reevaluation.md` compared all 29 frozen
requirements with the accepted A and B implementation. It found 7 requirements
verified on synthetic software evidence, 15 partially implemented, and 7 with
no accepted implementation. It made no scientific-contract or product-code
change. The separate 17 October local workbook inventory is now recorded as
inventory only. It is not a pipeline fit or biological validation. The two
Notion sync receipts record successful read-back, while GitHub publication
remains unverified and the current CLI token is invalid. Initial Astra review
confirmed all 29 requirement classifications and found one P2 lifecycle-record
issue. The focused rereview passed after correction, with no new P0 or P1.

## Accepted C-01 handoff

Omar explicitly resumed implementation on 2026-10-18. Fresh Luna-max chat
`01a14fdb-c11f-7e13-836e-e87635a89f23` continued the same C-01 initial
attempt in the preserved isolated worktree
`/Users/megastore/.codex/worktrees/c-01-integration-foundation/PML MultiOmics
Analysis Pipeline` at base `945ef8c`. The attempt counter remains 1. Before
dispatch, the worktree contained only the preserved allowlisted file
`tests/testthat/test-C-01.R` (SHA-256
`6eb0aca4f9a6c974d68b1a46f6e17aa0e7509f3dde749d702b51ed63fef881da`). The
worker produced clean allowlisted candidate
`5e43391155643732289a2cfc3d2a27fdbcd3b416`. Fresh Sol-6.1-high review
returned changes required: P0 0, P1 2, P2 2. Luna repair 1 fixed actual
B-resolver identity compatibility, the two-block minimum, complete-view typed
refusal and malformed public-input typing. The single focused rereview passed
with P0/P1/P2 all zero. Sol integrated the candidate as `b5527ef`, repair as
`1cf1632`, and shared registration as `40caa9f`. The exact C-01 suite passed 109
expectations, the exact stage-C CLI exited 0 with a valid complete envelope, and
clean archive package check ended `Status: OK`. C-01-AC1 through AC4 are
accepted on synthetic software evidence. Private study data remain untouched.
Acceptance receipts are commit `38bc7dc`. The three run-owned chats are archived
and the C-01 worktree is archived with a recoverable snapshot. C-02 is released;
C-03/C-04/C-05 remain behind G5.

## Historical C-02 handoff

Fresh Luna-max chat `01a15056-fa00-79b3-867c-538f1a004894` completed C-02
candidate `809190e` in isolated worktree
`/Users/megastore/.codex/worktrees/c-02-supervised-evaluation/PML MultiOmics
Analysis Pipeline` at base `38bc7dc`. The worktree was clean and the C-01
baseline passed 109 expectations before dispatch. `mixOmics 6.36.0` is installed
and the C-02 suite passed 61 expectations with no failure, warning or skip. The
worker reported an invalid `ticket_id` CLI key, partial AC3/AC4 evidence, an
interrupted package check and pending shared registration, so acceptance was not
claimed. Fresh read-only Sol-6.1-high review chat
`01a1506e-9b59-7241-8c33-6a1a6da05423` returned `changes_required`: zero
Critical, eight Important and two Minor findings. The review is
`/private/tmp/triomics-c-02-sol-review-2026-10-18.md` and the review chat is
archived. Targeted repair 1 is active in the original Luna chat. It is confined
to matched metrics, fold counts/subject provenance, generic adapter dispatch,
native-output failure handling, a valid CLI fixture, preparation-failure ledger,
uncertainty disposition and the missing leakage/budget tests. No integration or
G5 release is authorized yet.

## 19 October C-02 registration

Commits `a3a1cfd`, `249e2d0` and `2f4e8e4` closed review findings F01–F08.
Luna high rechecked that tree, changed no files, and reported 17 cases and 131
assertions passing. Astra low had already returned a code-only pass on the same
range and did not rerun tests. Sol then applied the shared registration:
`mixOmics` in DESCRIPTION and `renv.lock`, `evaluate_pipeline` exported,
`concat_plsda` and `concat_pls` registered, and stage C dispatching nested
evaluation. The allowlisted splits helper is commit `028f7c8`.

The integrated C-02 suite passed 165 expectations with no failures, warnings
or skips. A-T05, including the empty-filter guard, passed 127. The command

`Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-02.yaml --stage C --output artifacts/acceptance/C-02`

was run against a fresh temporary directory and exited 0. The synthetic
`concat_plsda` result predicted 12/12 eligible units, with balanced error 0 and
8 inspected fits. One inner fold requested `ncomp=2` at training rank 1 and
failed closed as `C_EVAL_COMPONENT_RANK`. The fixture `min_observed` is 1
because an inner training side can contain two samples. That accuracy is a
property of the tiny synthetic fixture, not a biological result.

C-02 is accepted on synthetic software evidence. The M5 review thread
`01a15362-3c23-7432-8e22-d52aa222658f` returned pass with no Critical or
Important findings and did not rerun tests. G5 is released for C-03, C-04,
and C-05. C-08 and every D packet remain closed.

C-03 code is commit `9758b4a`. Sol reran `test_local(filter='C-03')`: 57
passed, 0 failed, 0 warnings, 0 skips, 7.8 seconds. The stage C fixture
exited 2 with `C_SCORE_BACKEND_UNAVAILABLE` before ingest. Shared registration
is not applied, and C-03 is not accepted. The rank guard currently measures
the first predictor block only; native component mismatch still fails closed.

## Literal resume instruction

Read `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, and this handover. C-01 through C-05 are accepted on
public synthetic software evidence; G5 is released and M6 is in progress.
C-05 acceptance is recorded in
`reviews/2026-10-20-c05-sol-acceptance.md`. Its decisive clean archive check at
`c34d9ff` is `Status: OK`, testthat FAIL 0, WARN 22, SKIP 0, PASS 4076. The
warnings remain visible and must not be called warning-free. No real study or
biological MOFA validation is claimed.

Resume at the clean archive package gate for integrated C-08 snapshot after
commits `c5535fb` and `2170820`. The focused shared rereview passed with no
remaining findings. Fresh integrated tests passed C-08 288, C-01 109, C-03 82
and C-04 132 assertions with zero failures, warnings or skips. The exact Stage-C
CLI exited 0, endpoint JSON and all envelopes validated, and the scheduled B
request bound the persisted endpoint bundle hash. Run
`Rscript --vanilla tools/check-package.R` from a clean Git archive of the
committed snapshot. Accept C-08 only after that package gate passes and every
warning is named and dispositioned. Do not rerun consumed packet repairs.
C-09/C-06/G6/D remain closed.
C-09 and C-06 stay closed until C-08 is accepted; G6 and D stay closed until
their declared gates. Do not loosen the B-T04 0.08 bound, reroll seeds
1001:1400, edit frozen `discovery/spec-v2` content, or read private study data.
