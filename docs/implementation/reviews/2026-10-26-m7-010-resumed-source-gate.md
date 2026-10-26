# M7-010 resumed source gate

## Scope

Omar resumed the paused M7-010 verification on 26 October 2026. Source snapshot:
`2a8e5d50f8fd165a79f3767d1a42d11bdb92bfa3`; its product files are unchanged from
reviewed integration `0afe2c533c49684f914f62d2a93169f122b73053`. The completed
Luna work and Astra rereviews were not repeated. No product code, test assertion
or scientific contract was changed. No private study data was read or fitted.

## Interrupted-run diagnostics

The prior partial log showed three numbered A-T01 diagnostics without their
names or types. A fresh exact A-T01 run returned FAIL 0 / WARN 0 / SKIP 0 /
ERROR 0 / PASS 61 across 12 test blocks, exit 0. It used the unchanged
five-second CLI timeout assertions. The historical diagnostics did not recur;
their cause is unproven, not retroactively classified as harmless or repaired.

## Full source gate

The one fresh full source run completed in coordinator session 19228, exit 0
after `DONE`: **FAIL 0 / WARN 14 / SKIP 0 / ERROR 0 / PASS 5893**, across 352
test blocks in 33 files. These are testthat expectations, not independent
scientific studies. Within this run A-T01 passed 61, B-T04 passed 430, B-T07
passed 315, and Stage-B composition passed 94; none had warnings or failures.
The run used
`testthat::test_local(filter = NULL, reporter = "summary", stop_on_failure = FALSE)`.
The runner records exact totals and each warning/failure/error/skip by file and
test name after completion. No additional warning suppression is applied.

MOFA2 1.22.1 is selected by prepending `/tmp/triomics-mofa-lib-m7-006` to
`.libPaths()`. Python is `/tmp/triomics-mofa-py-m7-006/bin/python`, verified as
mofapy2 0.7.5. Original runner and evidence:
`/private/tmp/triomics-m7-010-resume.Onzcpx`.

Durable exact log, JSON counts/conditions, A-T01 diagnostic evidence and runner:
`evidence/2026-10-26-m7-010-resumed/`. No test suite was rerun to recover counts.
This source gate ran in the existing checkout, with unrelated dirty
`.Rbuildignore`, `.gitignore`, `renv/activate.R` and untracked files preserved.
Product-file comparison to `0afe2c5` was empty. This is not a Git-archive
package gate and no new `Status: OK` package result is claimed.

## Named warnings

All fourteen are `expectation_warning` from `test-C-05.R`:

| Exact test name | Count | Diagnostics |
| --- | ---: | --- |
| C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes | 11 | Low feature count (3); factor-1/average-expression correlation (4); `no non-missing arguments to max; returning -Inf` (4) |
| C-05 real MOFA2 fits and extracts a missing sample-view capability cell | 3 | Low feature count (1); inactive factors (2) |

Here `real MOFA2 fit` in the historical test name means execution of the native
backend on a public synthetic fixture, not fitting private real-study data.
The current condition list matches the previously dispositioned M7-006 source
baseline by file, exact test name, message with whitespace normalized, and
multiplicity. `baseline-warnings.txt` preserves the prior summary and
`warning-comparison.json` records the comparison. The exact current messages
are retained in `full.json` and `full.log`. No new warning name or message
appeared; no warning was newly suppressed. Archive-only B-T07 Git-metadata
warnings are not part of this source-checkout baseline.

## Separate specification tooling limitation

The specification validator could not start because its required `jsonschema`
and PyYAML dependency set is unavailable in the checked Python environments
(system Python, isolated MOFA Python and bundled workspace Python). Each attempt
exited 1 with the explicit missing-dependencies message. No installation was
attempted. This is not a specification-validation pass or a product test failure.
Git comparison confirms frozen specification files are unchanged from `0afe2c5`.

## Disposition

M7-010 is reviewed, integrated and source-verified on public synthetic fixtures.
The authorized verification cycle is complete; no worker or test process remains
active. Completed Luna repairs and Astra reviews were retained without repeats.
B-T07 remains `candidate/review_pending`, `accepted=false`; B-T07/M7 acceptance,
G7 and Stage D remain closed. No package gate, milestone acceptance, real-study
validation, private-data run or GitHub push is claimed. A subsequent producer
provenance/migration seam requires its own bounded authority and applicable audit.
