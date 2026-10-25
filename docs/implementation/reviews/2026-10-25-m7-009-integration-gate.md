# M7-009 integration gate

- Date: 2026-10-25
- Reviewed candidate: `2eb483ac7bcf5aa1919b8d72b9b8c8b239235861`
- Integrated snapshot: `0f139a49fb5e6471a6529c830e27bd7616431e34`
- Disposition: **focused gates pass; full source gate fails**
- B-T07 accepted: no
- Downstream release: none

## Serial integration and identity

The reviewed candidate was integrated as:

- `0fa320c` — native B-T07 provenance producer;
- `fced7eb` — combined identity-normalization and entity-identity repairs;
- `5ca5012` — M7-009 resume-reason classification; and
- `0f139a4` — read-only resume inspection.

The integrated blobs for `R/export_b.R`, `R/results_b.R`,
`tests/testthat/test-B-T07.R`, and
`docs/implementation/registrations/B-T07.json` equal immutable candidate
`2eb483a` exactly. Unrelated dirty files were not staged or committed.

## Focused integrated gates

Fresh tests from the integrated branch passed:

- exact B-T07: FAIL 0, WARN 0, SKIP 0, PASS 315;
- focused public B-T02 identity CLI: FAIL 0, WARN 0, SKIP 0, PASS 266;
- B-T03: FAIL 0, WARN 0, SKIP 0, PASS 286; and
- A-T08: FAIL 0, WARN 0, SKIP 0, PASS 284.

The B-T03 line `Coefficients not estimable: between_group.unobserved` was an
expected stdout note, not a test warning.

## Full source gate

An initial launch set `R_LIBS_USER` to the isolated MOFA directory and exited
before test discovery because `testthat` was hidden. It is invalid environment
evidence and is not counted as a suite result.

The valid uninterrupted run prepended MOFA2 `1.22.1` to `.libPaths()`, used
mofapy2 `0.7.5` at
`/tmp/triomics-mofa-py-m7-006/bin/python`, and ran:

```text
testthat::test_local(reporter = "summary")
```

It reached `DONE` and exited 1 with three failures and fifteen warnings. No skip
section was reported.

The fourteen C-05 warnings are the existing native MOFA small-fixture
diagnostics. The only new warning and all three failures are the same B-T04
compatibility defect:

1. `test-B-T04.R:707` — the B-T04 CLI command exited 2, producing a command
   warning;
2. `test-B-T04.R:722` — expected CLI status 0, observed 2;
3. `test-M4-005-stage-b-composition.R:281` — expected the historical
   `A_ID_REFERENCE` fallback text but observed `B_EXPORT_INCOMPLETE`; and
4. `test-M4-005-stage-b-composition.R:283` — expected source-composition status
   0, observed 2.

Both focused reproductions give the same error:

```text
triomics B_EXPORT_INCOMPLETE: The Stage B runner did not supply its same-run
native provenance context.
```

## Root cause

`triomics_b04_execute_fixture()` is the only custom Stage-B producer. It
bypasses the generic `triomics_execute_stage_b()` implementation and returns
`resolved_matrices` but no `provenance_context`. The post-stage B-T07 hook is
invoked for every successful Stage-B CLI run and correctly refuses to publish a
complete native handoff without that context.

This is not a repeated-measures numerical or calibration regression. The B-T04
model runs before the handoff refusal. The frozen 0.08 bound and seeds
1001:1400 were unchanged.

The obvious mechanical substitution is scientifically invalid: B-T04's payload
states that its closed-form four-subject trajectory does not use the ticket's
two-visit measurement matrix as its response. Those unused file bytes must not
be presented as derivation ancestors of the generated matrix or fitted result.

## Required next decision

A new bounded record is required before product repair because the M7-009 repair
allowance is consumed. The record must choose and audit one truthful boundary:

- construct a B-T04-specific same-run provenance context rooted in the actual
  immutable synthetic fixture protocol, repository revision, generated
  metadata/feature registry/unit map/resolved matrix, and persisted result; or
- preserve the B-T04 statistical result but publish an explicit fail-closed
  no-handoff disposition, so downstream consumers cannot treat it as proven
  producer evidence.

It must not bind the unused two-visit matrix as response lineage, change the
B-T04 model/calibration, loosen the null bound, reroll seeds, or grant real-study
or biological validity. Public-seam RED/GREEN must cover both the B-T04 CLI and
Stage-B composition test, followed by B-T07 non-regression, two-axis review,
serial integration, and a repeated full source suite.

B-T07 remains integrated but unaccepted. Registration remains
`candidate/review_pending`, `accepted: false`. C-06 provenance, migration,
D verification/publication, D-T01 acceptance, M7/G7 release, and downstream
Stage D remain closed.
