# M4-010 — B-T08 installed-test path exception

- Date: 2026-10-16
- Trigger: Omar explicitly authorized one narrowly scoped exception to the
  two-repair ceiling after the M4-009 clean-archive package check exposed an
  installed-test-only fixture-path defect.
- Prior gate: `docs/implementation/reviews/M4-009-package-check-gate.md`
- Initial audit: `docs/implementation/reviews/M4-010-astra-audit.md`
- Focused rereview: `docs/implementation/reviews/M4-010-astra-rereview.md`
- Sol review: `docs/implementation/reviews/M4-010-sol-review.md`
- Status: reviewed candidate integrated as `92760cb`; full-suite and clean-package
  gates pending
- Counter disposition: this is one exceptional test-harness correction. It does
  not reset, erase or renumber B-T08's initial attempt or two consumed targeted
  repairs, and it authorizes no further correction automatically.

## Frozen authority and scientific boundary

M4-009's production and scientific candidate remains unchanged. Constant
continuous responses fail closed; duplicate family members abort; valid members
of mixed families retain family-adjusted q-values; the six public APIs and the
fixture-restricted CLI route remain as reviewed. The historical B-T04 diagnostic,
all statistical thresholds, seeds, estimands, models, multiplicity rules,
eligibility rules and claims remain frozen.

The clean-archive package check failed only because the new B-T08 CLI regression
helper resolves `inst/examples/tickets` through a source-checkout path. That path
exists under `test_local()` but not when testthat runs the installed package.
The package already provides `triomics_test_resource()` to resolve the same
resource from either a source checkout or `system.file()` in an installation.

## Authorized correction

One fresh Luna-max worker in one fresh isolated worktree may modify only:

- `tests/testthat/test-B-T08.R`.

The worker must reproduce the installed-context failure before editing. It may
change only `b08_copy_ticket_examples()`, `b08_run_source_cli()` (renaming it to
describe context-aware behavior is allowed), and, if strictly necessary for
those helpers' test-path behavior, assertions or labels in the three B-T08 CLI
regression tests that call them. The fixture helper must resolve packaged ticket
examples in both source and installed test contexts using the existing
installed-aware resource mechanism, copy only public synthetic fixtures, and
preserve the temporary fixture layout expected by the CLI probes.

The CLI helper must retain the existing checkout runner when the source checkout
and `inst/exec/triomics` are present. In installed-test context, where
`.Rbuildignore` intentionally excludes `inst/exec`, it may invoke only the same
installed package's internal `triomics_main()` in a clean child R process using
the package library paths supplied by the test environment. Both paths must pass
the same CLI arguments and return the actual exit status and output. The helper
may not simulate outputs, call the association function directly, or bypass CLI
dispatch and error mapping.

No file under `R/`, `NAMESPACE`, `man/`, `inst/`, `discovery/`, package metadata,
dependency lock, configuration contract, acceptance threshold or private-data
location may change. The worker may not weaken, skip, delete or conditionally
bypass any required assertion. No private study data may be read or copied.

## Required evidence and gates

1. RED: the existing clean installed-package context deterministically produces
   the three documented fixture-open errors before the correction. The initial
   Astra audit additionally established that fixing only those paths would expose
   the checkout-only `pkgload::load_all()` / `inst/exec/triomics` assumption.
2. GREEN: the B-T08 focused suite passes locally and in installed-package context
   with no failure, warning or skip; the B-T09 coexistence and nonfixture refusal
   checks remain intact.
3. A fresh Sol-high reviewer accepts the bounded diff for scope, test integrity,
   source/installed parity and privacy.
4. Sol integrates the reviewed worker commit serially, reruns the complete local
   suite, and runs a fresh clean-archive package build/check. Required tests may
   not count through skips and the package check must report no ERROR, WARNING or
   NOTE.
5. The already-required focused M4-005 Astra rereview evaluates the immutable
   integrated snapshot before B-T08 acceptance and B-T07 release.

Any failure requiring a production change, a second exception correction, a
scientific-contract change or weakened test is outside this authorization and
returns B-T08 to blocked status.
