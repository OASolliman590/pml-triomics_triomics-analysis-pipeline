# M7-017 Luna evidence

## Scope and initial state

- Worktree: `/Users/megastore/.codex/worktrees/m7-017-c-unit-map/PML MultiOmics Analysis Pipeline`
- Base product: `cfe916059c9e2e3b04f64cb96035d8777520b488`, scope commit `025bc69`, focused endpoint scope correction `627c7da`.
- Focused Astra scope rereview: PASS, `/tmp/triomics-m7-017/astra-scope-rereview.md`.
- Requested settings: Luna max; effective host settings are not exposed.
- Representation ruling: accepted M7-010 distinguishes the uncentered B matrix handed to C from the centered response used by the B-T04 fit. C must retain exact B input values and identity; no new transformation.

## RED/GREEN

Initial focused structured RED completed (session 19376, exit 1; command and serialized output are retained by the run-gate artifacts below). Totals: 328 PASS, 2 FAIL, 0 WARN, 1 ERROR, 0 SKIP, across 18 test blocks. The behavioral failures were:

- Paired native Stage C refused with `C_ELIGIBILITY_UNKNOWN_UNIT`: the CLI passed source sample ids while the once-computed B paired-delta matrices are keyed by subject ids.
- C-08 rejected a visit-level `subject_centered` map as `Unit type and representation id disagree`.
- The first trajectory fixture edit failed in B with `B_DESIGN_UNSUPPORTED` because its helper changed an undeclared visit-column setting; this is a fixture error, not product RED. The helper was corrected to leave `visit_column: null` as declared and has not been rerun as a RED fitting pass, per coordinator direction.

The historical trajectory probe at `/tmp/triomics-m7-016-luna/native-paired-trajectory/trajectory-probe.R` used repository `exec/triomics`, whose `../..` source root resolves above the repository and falls back to an installed package. Its B=0/C=4 `A_ARTIFACT_FINALIZE` result is invalid candidate-source evidence; final candidate checks use `inst/exec/triomics`.

Focused unit-map smoke runner `/tmp/triomics-m7-017-luna/run-unit-map-smoke.R` completed on current product code (session 77487, exit 0): 19 PASS, 0 FAIL, 0 WARN, 0 ERROR, 0 SKIP, 3 test blocks. It exercised subject-centered C-08 validation, named trajectory ordering with shuffled block columns and unchanged matrix values, paired per-block unit joining/attrition, and preservation of signed reference/target names in the normalized map. Structured outputs are `/tmp/triomics-m7-017-luna/small-regression-final.log`, `small-regression.json`, and `small-regression.rds`. A previous run (session 10928) had four assertion failures caused by comparing `AsIs` list columns and named vectors with unclassed expectations; test assertions were corrected and the rerun passed.

## Native B/C/D evidence

Native trajectory flow used the preserved public input layout copied from `/private/tmp/d-t01-trajectory-cli-probe-7ff610834a8e` to `/private/tmp/m7-017-native-trajectory`; all commands used `Rscript --vanilla inst/exec/triomics` and the provided MOFA2/Python paths.

- B: session 45880, exit 0, `registries/run.json` status `complete`; log `/tmp/triomics-m7-017-luna/native-trajectory-B.log`.
- C: session 80347, exit 0; log `/tmp/triomics-m7-017-luna/native-trajectory-C.log`.
- D: session 76258, exit 0; log `/tmp/triomics-m7-017-luna/native-trajectory-D.log`.
- C endpoint readback: 12 source sample units, all `subject_centered`, 4 subjects × times 0/1/2, time unit `synthetic_step`, origin `0`; declared `visit_column` is null and no visit labels were created. C publication also passed the existing native B→C keyed matrix identity validation, which compares the exact same-run matrices.

Native paired flow used inputs copied from `/private/tmp/d-t01-paired-cli-probe-803926a04d4c` to `/private/tmp/m7-017-native-paired-final`.

- B: session 81625, exit 0; log `/tmp/triomics-m7-017-luna/native-paired-final-B.log`.
- C: session 96080, exit 0; log `/tmp/triomics-m7-017-luna/native-paired-final-C.log`.
- D: session 48739, exit 0; log `/tmp/triomics-m7-017-luna/native-paired-final-D.log`.
- C endpoint readback: six `subject_delta` units, named `baseline: -1`, `followup: +1` weights preserved as JSON objects, all block eligibility records present, and exact two-source membership per subject. The C publisher validated the persisted B AnalysisUnitMap digest and exact same-run matrix identities before writing the D handoff.

The earlier intermediate paired root `/private/tmp/m7-017-native-paired/out` is retained as debug evidence only: its endpoint was created before named sign serialization was fixed, so it is not counted as final evidence. Initial structured RED artifacts remain `/tmp/triomics-m7-017-luna/red.log`, `red.json`, and `red.rds`. No fitting job is active.

## Remaining limitations

Focused named gate command was `R_LIBS=/tmp/triomics-mofa-lib-m7-006 Rscript --vanilla /tmp/triomics-m7-017-luna/run-focused-final.R` (session 85390, exit 1). Structured totals from `as.data.frame(testthat_results)`: 748 PASS, 4 FAIL, 2 ERROR, 0 WARN, 0 SKIP, 50 test blocks. Full condition inventory and RDS are `/tmp/triomics-m7-017-luna/focused-final.json` and `.rds`; reporter log is `/tmp/triomics-m7-017-luna/focused-final.log`.

Named focused-gate conditions:

- `test-C-06-shared.R`, “Stage C composes paired and trajectory units from native B resolution”: paired D check failed with `D_LINEAGE_MISMATCH` (“C-06 manifest has an absent or mismatched study_id”); trajectory B fixture failed with `B_DESIGN_UNSUPPORTED` (“Model terms must be declared names, not R expressions”). This test helper fixture differs from the preserved original native fixture roots, where manual B/C/D completed successfully.
- `test-C-08-shared.R`, “Stage C writes immutable C-08 endpoints and schedules, but does not execute, B work”: error `C_SCORE_COVERAGE_MISMATCH`, “The declared AnalysisUnitMap subject identity disagrees with its source samples.”
- `test-C-08-shared.R`, “Stage C refuses an unknown source selection history”: expected `C_SCORE_LINEAGE_INVALID`, observed `C_SCORE_COVERAGE_MISMATCH` at the same subject/source check.
- `test-C-08-shared.R`, “Stage C propagates outcome-selected ancestry as withheld and descriptive-only”: error `C_SCORE_COVERAGE_MISMATCH` at the same subject/source check.
- `test-D-T01.R`, “D-T01 refuses stale paired signs and preserves the old pointer”: no paired root was present in `/tmp/triomics-m7-017-luna/native-roots.txt` when this test ran, confirming its dependency on the order/side effects of another test file.

The coordinator froze the unchanged six-file candidate at `c8b309e` and requested no self-repair pending a consolidated bounded repair. The candidate remains unaccepted and unintegrated.
