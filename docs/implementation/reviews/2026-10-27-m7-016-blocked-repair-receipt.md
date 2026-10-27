# M7-016 — D publication blocked receipt (not acceptance)

Scope freeze: `92351d658519899d55864ee64617e4f6c71e68c5`.
Fresh Astra-low scope audit: PASS, recorded in `2026-10-27-m7-016-scope-audit.md`.
Initial isolated product candidate: `ac98db3191bf8a5854144f5e017329038e9170e4`.
Repair 1: `f23b5a7c96cec1134b6d098850e90fbbfc420299`, not integrated.

## Verified repair and limits

Sol independently reran the three original counterexamples on repair 1:
public routing without persisted proof returns `D_PROVENANCE_REQUIRED`;
re-sealed conflicting manifest returns `D_PROVENANCE_INVALID`; linked C native
state ancestry returns `D_OUTPUT_PATH_UNSAFE` from both reader and executor.
Fresh focused Sol rereview marks original findings 1–3 addressed, finding 4
not addressed. Its additional installed-package test-bootstrap defect is under
bounded repair 2, saved as `cfe9160`; its narrow rereview is recorded below.

Luna's frozen repair-1 D-T01 run completed with 317 success markers and no
failure, warning, or skip blocks. Command:
`Rscript --vanilla -e 'testthat::test_local(filter="D-T01", reporter="summary", stop_on_failure=FALSE)'`.
The runner did not save structured counts. No exact structured totals or
milestone GREEN are inferred from the shell exit. The log and report are in
`evidence/2026-10-27-m7-016-repair1/`.

## Required native coverage remains blocked before D

Sol independently ran public synthetic B then C in fresh output roots:

- Paired: `/private/tmp/sol-paired-boundary-81e71c7a0cf7`; B exit 0,
  C exit 2 `C_ELIGIBILITY_UNKNOWN_UNIT` for block_1 during integration
  sample-to-unit reconciliation.
- Trajectory: `/private/tmp/sol-trajectory-boundary-81e71e40580b`; B exit 0,
  C exit 2 `C_SCORE_LINEAGE_INVALID`: C-08 requires the declared AnalysisUnitMap
  from analysis resolution.

Neither reaches D. The trajectory fixture requests exploratory concat_pca;
there is no optional endpoint flag to remove. The C caller automatically takes
the C-08 map path for this method and constructs its fallback map only for
levels. Changing that C caller is outside M7-016's D-only shared exception.
Two-group DEqMS metadata mutation coverage does not substitute for native
paired-sign/trajectory-time-origin evidence. No assertion or required gate is
waived. No statistical defect or scientific validity is inferred solely from
these software composition failures.

## Gate disposition

No product integration, full-source gate, clean archive package check, or final
Astra acceptance audit has completed. The original parent focused suite was
stopped as superseded (exit 143); it supplies no total or GREEN. Previously
reported native-generation hashes belong to the initial candidate and are not
promoted to final repaired-snapshot evidence. The final parent B/C/D generation
and pointer SHA-256 receipt is therefore unavailable; no final publication is
claimed.

C-06 accepted remains false; C-07's four controls remain not_run; C-09 remains
unqualified. M7/G7 remain closed. No D-T02, annotation execution, targets, report,
private data access, or GitHub push. The isolated worktree preserves all work.

## Final test-only repair

Repair 2 is isolated commit `cfe9160`, one file `tests/testthat/test-D-T01.R`.
The installed bootstrap defect was reproduced, then the test was made to load
the exact package under test and inject mocks through a private publisher
environment. The extracted actual recovery block ran in source mode and a
fresh temporary candidate installation; both exit 0, all seven expected
outputs checked, including `D_PUBLICATION_STATUS_UNKNOWN`, path rejection,
`D_PUBLICATION_FAILED`, and `old_current_preserved=TRUE`. No full suite or new
B/C fits were run for this test-only repair. Detailed logs/driver are in
`evidence/2026-10-27-m7-016-repair2/`.

Recovery-only current-pointer SHA-256 values (not final milestone readback):

- Source `/private/tmp/repair2-source-root-936b36f7db48/registries/current-D.json`:
  `5389f10bd634a14538a1c1bc296e77d6bf085071d241af42c52707c43f9eacf2`.
- Installed `/private/tmp/repair2-installed-root-936b3c6eb9e3/registries/current-D.json`:
  `01441c0998f87bedc013b4cd0a7a172edec1bcc3d0e04044173ad9f08fe9816a`.

Cumulative product files are exactly nine allowed files: R/evidence_bundle.R,
R/annotation_router.R, R/cli.R (D only), R/stage_registry.R, NAMESPACE,
man/triomics-api.Rd, tests/testthat/test-D-T01.R, inst/examples/tickets/D-T01.yaml,
docs/implementation/registrations/D-T01.json. R/resource_manifest.R is untouched.
All product work stays in the isolated worktree, not the integration branch.

## Close disposition and resume

Final narrow Sol review of `cfe9160`: bootstrap finding ADDRESSED, no new
Critical/Important finding. Original findings 1–3 are addressed; original
finding 4 remains NOT ADDRESSED. Two repair slots used. No run-owned worker
or test process remains active. Scope Astra passed; final Astra did not run
because its verification prerequisites have not passed. M7-016 remains blocked
and incomplete. Only this documentation/evidence receipt is committed to the
integration branch.

Literal resume: read this receipt and preserved candidate `cfe9160` in the
m7-016-d-publication worktree. Do not rerun Luna or consume a third repair.
Scope and audit the C paired/trajectory analysis-unit composition seam before
editing C behavior; preserve the D candidate and all reproduced evidence.
After that seam is resolved, complete the independent B-T07/C-06/D-T01 gates,
actual native B/C/D full-digest readback, full source gate and clean archive
Status: OK, then fresh Astra acceptance review. Do not infer acceptance from
this blocked receipt. M7/G7 and downstream D remain closed.
