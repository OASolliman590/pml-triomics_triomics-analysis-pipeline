# M8-D08-S02 — respect the configured test temporary directory

Status: proposed bounded verification-location repair; no implementation before the fresh read-only scope audit passes. The user's standing bounded-cycle authority applies to this separate test-only seam. It does not expand the D-T08 or S01 workers' allowlists, change a scientific contract, or reopen M7.

## Evidence

Sol's clean8a5c762 focused run recorded1376 passes,0 failures/errors/skips and1 existing named D-T01 stale-sign warning. C-06-shared and D-T01 hard-code `/private/tmp` for native fixtures and outputs, so TMPDIR did not control those paths. Preserve that behavioral receipt but do not use it as the requested no-tmp runtime qualification. The four affected existing test files have the same blobs at the scope snapshot and8a5c762. A bounded search found the same output-location convention in B-T05 and D-target-shared, which would affect the later full gate. No product-code defect or scientific change is inferred from this location issue.

## Exact scope and behavior

One fresh Luna-max worker may edit only:

- tests/testthat/test-B-T05.R
- tests/testthat/test-C-06-shared.R
- tests/testthat/test-D-T01.R
- tests/testthat/test-D-target-shared.R

Replace hard-coded temporary output/fixture locations with the running R session's `tempdir()`. Apply the same replacement inside generated child-R scripts. Child processes inherit the configured TMPDIR; the verification runner sets it to the internal APFS proof directory before starting R. Do not introduce a new environment API, global tempfile override, harness source rewriting, mount mapping, or product helper. The runner must check the effective tempdir and filesystem, not merely assume that an environment variable took effect.

Preserve every test identity, expectation, model, fixture value, seed, numerical bound, failure injection, publication order, and command except temporary locations. In particular, external symlink attack targets must remain outside the tested output root. The generated D-T01 fault-injection script currently links to `/private/tmp`; its replacement must remain a real external directory and continue testing rejection and prior-generation preservation. Do not turn an unsafe external path into a legal descendant. Existing no-symlink/rollback assertions and expected nonzero exits remain unchanged. Do not add skips or suppress warnings.

These tests may still run on other hosts through their normal tempdir. This run's qualification remains synthetic macOS/internal APFS. No change is made to product filesystem guarantees or power-loss behavior. M7/G7 remain historically accepted. Conditional D06/D07/D14, C07 not_run, C09 unqualified and pending frozen-kit audit are unchanged.

## Proof and integration

Record exact before/after file diff and ensure the edit is location-only. A fresh child R probe must resolve its tempdir under the configured APFS proof root. Run the four affected suites with uncapped structured counts and every warning named; zero skipped required checks. Verify native fixture/output/log paths shown by the tests and warnings are under the configured root. Keep the earlier disqualified-path run, do not overwrite its evidence.

Sol reruns the required combined D08/S01 regressions and untouched native chain on the final combined candidate after the final report repair. The report's known missing-path error classification and privacy/nested-CV findings are a separate D-T08 repair and are not fixed here. No acceptance or G8/G9 release follows merely from this test-location repair.

One initial implementation plus at most two targeted repairs. One initial scope audit plus at most one focused documentation rereview. Any need outside these four files stops with evidence. No private data, canonical spec edit or push.
