# M7-017 Sol task review — c8b309e5

Read-only review of immutable candidate `c8b309e5e0a09887e5939a35db641e3665d4f650` over `627c7da`. Requested Sol/high; effective model/effort are not independently exposed. No repository edits, commits, child agents, fitting, acceptance or milestone release. Reviewed only the six-file additive C repair, approved scope/rulings, relevant existing consumers/producers and synthetic evidence. Repository remained clean at review end.

**Scientific/spec compliance: changes required — Critical 0, Important 2, Minor 0.**
**Code quality/behavior: changes required — Critical 0, Important 2, Minor 0.**
Four distinct findings follow; none is a claim about biological or clinical validity.

## Scientific/spec compliance

### S1 — Important: preserve B-map units excluded from every trajectory block

`R/cli.R:352`, `372–373`, `428–431`, `524–530` derive the output unit universe from the union of block matrix columns. The source B AnalysisUnitMap can legitimately also contain an explicitly excluded sample with `eligible=FALSE` and `exclusion_reason=absent_from_all_analysis_blocks` (existing producer `R/results_b.R:340–383`). The composition validates only that matrix samples are covered, then iterates only those samples. It discards the excluded B row entirely, so C's serialized unit map and target/disposition universe omit known attrition. This violates the M7-017 preservation of eligibility/exclusions and the no-covert-deletion contract.

Non-fitting reproducer: `/tmp/triomics-m7-017/sol-reprobe.R`, run with `Rscript --vanilla /tmp/triomics-m7-017/sol-reprobe.R` from the candidate. Log `/tmp/triomics-m7-017/sol-reprobe.log` reports `TRAJECTORY_INPUT_IDS a0,a1,absent OUTPUT_IDS a0,a1 OUTPUT_MAP_IDS a0,a1`. Retain the exact B map's declared excluded universe, validate its source membership, and serialize the excluded disposition instead of deriving all map rows solely from fitted columns. Add a behavioral exclusion-preservation test.

### S2 — Important: reject foreign secondary paired-map identities before rewriting them

`R/cli.R:642–681`, `725–741` validate secondary paired rows' subject, signs, membership and eligibility, but never validate their `unit_type`, `representation_id`, `study_id`, `analysis_id` or `preparation_context_id` against the resolved/current context. The top-map comparison at `606–615` checks only the first block. The output then replaces the unchecked secondary values with canonical/current labels. A secondary row declaring a different unit type, representation or scientific context silently becomes a valid delta row. A truthful digest of the first-block B map does not validate the secondary records, and matrix hashes do not bind these metadata fields.

Same non-fitting reproducer/log: independently changing each of those five fields in block B to `FOREIGN` yields `ACCEPTED_AND_REWRITTEN` with `subject_delta`, `delta`, `s`, `x` or `p`. Check every supplied per-block identity before composition, allowing source membership/exclusion differences only where contractually valid. Add negative tests for foreign type/representation/context, including a secondary block.

## Code quality and observable behavior

### Q1 — Important: preserve the clinical identity adapter when checking molecular subjects

`R/cli.R:299–307` now compares an existing molecular `record$subject_id` to the clinical link's public `subject_key` and aborts when unequal. These are different namespaces by construction: `triomics_clinical_subject_key` (`R/clinical_linkage.R:1–2`) hashes the study/raw subject into a `sub_...` key. The prior adapter deliberately replaced the unit-map subject identity with that linked key. Valid declared molecular subject `a` linked to its correct public key is now refused with `C_SCORE_COVERAGE_MISMATCH`, before any clinical association can be scheduled.

The reproducer/log above exercises the exact changed helper using `triomics_clinical_subject_key('s','a')`: raw `a`, key `sub_009fa556751b7e96`, result `C_SCORE_COVERAGE_MISMATCH`. Validate raw identity using the authorized linkage evidence before converting to the public key; do not compare raw and hashed identities as though they must be equal. Also review the new repeated branch at `2137–2142`: it bypasses that identity adapter, and trajectory `clinical_source_ids` at `2063–2072` still come from the pre-composition null map. Thus trajectory objectives get a null clinical view; paired objectives retain raw subjects that the existing scheduler (`R/clinical_score_requests.R:97–98`) compares to hashed keys. Add non-fitting linked-subject tests and one authorized native clinical route before claiming preservation of C08 behavior. This finding is confined to the changed composition/caller, not a request to redesign clinical linkage.

### Q2 — Important: remove run-local filesystem and cross-test prerequisites from committed tests

`tests/testthat/test-C-06-shared.R:485–486` writes a hardcoded run-owned `/tmp/triomics-m7-017-luna/native-roots.txt`, without creating its parent. `tests/testthat/test-D-T01.R:883–896` requires that file and surviving fixtures populated by a different test file. A clean machine/archive test run fails C06 when the directory is absent; an isolated D-T01 run fails its first existence assertion even when the package works. A successful development run can mask this through retained state, and an old manifest can select stale roots.

Targeted clean-root simulation extracted only this new D test expression and made that one manifest path absent; it reproduced one failing expectation, without running any CLI or fit. Script `/tmp/triomics-m7-017/sol-clean-test-probe.R`, log `/tmp/triomics-m7-017/sol-clean-test-probe.log`. Use self-contained temporary fixtures scoped to the test (or a deliberate shared helper), with explicit cleanup and no run-owned absolute path. Keep optional run evidence outside product test prerequisites.

## Positive evidence and limits

- Verified clean HEAD and the six-file diff; D implementation changes outside the additive tests were not touched by this candidate.
- Inspected actual retained paired and trajectory B/C/D roots and endpoints. Paired endpoint has six `subject_delta` units with JSON named `baseline:-1, followup:+1`, per-block eligibility/membership and two sources. Trajectory endpoint has twelve `subject_centered` sample units, four subjects, times 0/1/2, unit `synthetic_step`, origin `0`, and no invented visit labels for null visit_column.
- Independently recomputed all 88 `FILE_SHA256` inventory entries in `/tmp/triomics-m7-017-luna/native-sha256-manifest.txt`: zero mismatches. Both D pointers and provenance payloads exist.
- Independently ingested each retained public config and resolved without fitting. All four exact keyed B RDS matrices are identical to current resolution (`sol-native-readback.R/.log`). Separate `sol-c-matrix-readback.R/.log` compares the C result's consumed matrix digests to the actual persisted B matrices. No new centering or second delta is added by the diff. The general trajectory input retains native producer truth; no M7-010 generator ancestry is assumed for these ingested fixtures.
- Including excluded paired targets is not itself a defect: the existing PCA eligibility sidecar explicitly marks missing required views projected_out and all views missing refused, while fit indexes the retained training intersection. The current retained native examples contain complete pairs; they do not independently prove a native attrition case.
- The worker's focused C06/C08shared/D-T01 suite was pending when dispatched. This review does not count it as passing. Full source suite, clean archive `Status: OK`, scoped fresh Astra, and coordinator adjudication remain required.
- No schema/method/threshold/frozen-manifest edits were requested or made. The historical AGENTS/CLAUDE hash mismatch is not assigned to this C repair. C06 remains unaccepted; C07 not_run; C09 unqualified. This bounded phase cannot release canonical M7/G7 or downstream D work.
