# M4-013 — Stage B handoff composition correction

- Date: 2026-10-16
- Trigger: integrated M4-011 adjacent B-T04/B-T08/B-T09 regression gate
- Status: approved exclusive shared-integration unit; implementation pending
- Packet accounting: this is a cross-cutting registration/contract seam exposed
  by B-T07 integration, not another numerical-method attempt for accepted
  B-T04, B-T08 or B-T09 and not B-T07 repair 3.

## Reproduced failures

The package-aware adjacent suite reported six failures after B-T07 registration:

1. B-T08 CLI exited 2 because its verified clinical payload omitted
   `schema_version`, while the B-T07 public artifact contract requires `2.0`.
2. B-T09 produced both its root packet receipt and the new nested B-T07 handoff
   receipt; an older test recursively assumed exactly one acceptance record.
3. B-T04's accepted fixture executor returned no `resolved_matrices`, so the
   registered B-T07 hook failed closed even though that executor already holds
   the exact resolved matrix used by the fit.

The B-T07 focused suite and exact B-T07 CLI are green. These failures do not
change a model, statistic, threshold, calibration, family or clinical estimate.

## Exclusive implementation boundary

One fresh Luna-max worker in a fresh isolated worktree may change only:

- `R/design_repeated.R`;
- `R/clinical_association.R`;
- `R/clinical_longitudinal.R`;
- `tests/testthat/test-B-T08.R`;
- `tests/testthat/test-B-T09.R` if needed for the same artifact assertion;
- `tests/testthat/test-M4-005-stage-b-composition.R`.

The worker must use the reproduced six-failure adjacent suite as RED evidence.
The minimal implementation is:

- add `schema_version = "2.0"` to the serialized B-T08 and B-T09 clinical
  association payloads before envelope creation;
- return `resolved_matrices = list(block_1 = resolved$blocks$block_1$assay)`
  from the existing B-T04 fixture executor without serializing it or changing
  the B-T04 public result/calibration/acceptance artifacts;
- update B-T08/B-T09 tests to select and validate their root packet receipt while
  separately acknowledging the nested B-T07 handoff receipt;
- assert the B-T04 returned matrix is the same resolved matrix bound by its
  public source hashes and that the source CLI now completes its B-T07 handoff.

No exporter weakening, reconstructed matrix, fixture substitution, acceptance
threshold change, clinical refit change, schema version other than `2.0`, or
private-data access is authorized.

## Gates

Run the focused B-T04, B-T07, B-T08, B-T09 and Stage B composition suites plus
their exact public CLI slices. Fresh Sol-high review must verify the five/six-file
allowlist, unchanged numerical outputs and exact same-run provenance. Then run
the full local suite and clean-archive package check before immutable M4 Astra
review. B-T07 and G4 remain unaccepted until every gate passes.
