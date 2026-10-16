# M4-008 — reconcile Stage B composition tests with built-package execution

- Date: 2026-10-16
- Trigger: the post-review M4-005B coordinator full suite passed, but the required
  `Rscript --vanilla tools/check-package.R` gate failed inside `R CMD check`.
- Candidate: `0c84428` in `.worktrees/m4-005b-stage-b-composition`
- Status: pending independent Astra-low audit

## Evidence and diagnosis

The repository-local suite passed 3,169 assertions with zero failures, warnings
or skips. `R CMD check` then passed installation, namespace loading, examples and
code checks, but its installed test run reported 3 failures and 3,156 passes:

1. the M4-005B installed B-T04 test called the production executor with the raw
   B-T04 ticket. That ticket intentionally points at `discovery/spec-v2` source
   fixtures, which are excluded from the built package. The test therefore
   produced `A_SOURCE_MISSING` before exercising composition. The existing B-T04
   packet test already solves this correctly by rewriting a temporary config to
   the installed synthetic resources;
2. the global idempotence subprocess treated `testthat::test_path("..", "..")`
   as a repository root. During `R CMD check` that path is the check tests area,
   not a source checkout, so no `R/*.R` files were sourced and the subprocess
   correctly reported a missing global installer;
3. `.superpowers/` was staged into the source tarball because `.Rbuildignore`
   does not exclude it, producing the package-structure NOTE.

The preserved diagnostic check is under
`/private/tmp/triomics-rcheck-m4007.TgZRzH/triomics.Rcheck` and contains no
private study input. This is packaging/test-harness evidence, not a statistical
failure or biological validation.

## Bounded decision

Create a separate packaging reconciliation rather than silently consuming a
second M4-005B repair. M4-008 does not reset any M4-005, B-T05 or B-T06 counter
and may modify only:

- `tests/testthat/test-M4-005-stage-b-composition.R` by one fresh Luna-max worker;
- `.Rbuildignore` by Sol, adding only the `.superpowers/` exclusion.

The test repair must:

- make the installed B-T04 composition test use the existing temporary config
  rewrite to package-installed synthetic metadata and proteomics, and clean it;
- retain a real global-dispatch idempotence assertion. In a source checkout it
  must continue sourcing `R/*.R`; in built-package execution it may construct a
  source-style global function environment from the installed triomics namespace
  and run the same global installer twice. It must not skip either context;
- leave all expected B-T04 statistics, calibration controls, B-T05 provenance,
  B-T06 family/rank behavior and production R files unchanged.

No threshold, seed, estimand, fixture values, package API, CLI behavior, runtime
dependency, private path or study input may change. The repair gets one initial
attempt and at most one targeted repair. It receives one fresh Sol-high review
and at most one focused rereview.

## Required acceptance

1. `M4-005-stage-b-composition` passes locally with no skips.
2. The complete local suite passes with no failures, warnings or skips.
3. `Rscript --vanilla tools/check-package.R` reports `Status: OK`; the built
   source contains no `.superpowers/` directory or package-structure NOTE.
4. Fresh source and installed B-T04/B-T05/B-T06 probes pass and preserve the
   already reviewed outputs.
5. A fresh Sol-high reviewer accepts the exact bounded diff.
6. Only after all gates pass may the candidate integrate and proceed to the
   still-required M4-005 shared Astra audit. B-T05, B-T06 and B-T07 remain blocked
   until that audit is accepted.

