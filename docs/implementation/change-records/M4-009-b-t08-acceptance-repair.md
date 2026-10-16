# M4-009 — B-T08 acceptance and installed-registration repair

- Date: 2026-10-16
- Trigger: fresh acceptance review of integrated B-T08 found three P1 defects
- Review: `docs/implementation/reviews/B-T08-acceptance-review.md`
- Status: pending independent Astra-low audit
- Counter: B-T08 initial implementation 1; this authorizes targeted repair 1 of at most 2

## Frozen authority

B-T08 AC2 already requires unsupported, censored, constant and non-estimable
fields to remain explicit rather than fall back to a correlation. AC3 already
requires a fixed endpoint family. The packet registration request already names
six exports and requires an explicit installed Stage B clinical-association call.
M4-009 implements those frozen requirements; it does not change the scientific
contract, method, threshold, family definition or scope.

## Bounded repair

One fresh Luna-max worker in a fresh isolated worktree may modify only:

- `R/clinical_association.R`;
- `tests/testthat/test-B-T08.R`.

It must first add behavioral tests showing that a constant continuous response
fails closed with `estimation_status = failed`, reason `constant_response`, no
effect, SE, p or q, and no published family q. It must add a negative fixed-family
test requiring duplicate `member_ids` to abort before fitting with the existing
`B_FAMILY_UNDECLARED` family-contract error. It must retain every existing AC1,
ancestry, missingness and privacy assertion. It may then implement only those two
behaviors. No fallback correlation or silent deduplication is allowed.

Sol alone owns the shared registration repair and may modify only:

- `NAMESPACE`, adding the six exports already listed in the B-T08 registration;
- the Stage B branch of `R/cli.R`, calling
  `triomics_execute_clinical_associations(config_path, output)` after successful
  Stage B execution when `clinical.enabled` is true, without disrupting B-T09;
- only the source-only `triomics_main` hook at the end of
  `R/clinical_objectives.R`, removing it once explicit shared dispatch exists.

The registered path must execute exactly once in source and installed layouts.
It must preserve B-T09's registered longitudinal call and fail closed through the
existing CLI error mapping. No other B-T08, shared package, fixture, dependency,
specification or private-data file may change.

## Required gates

1. RED evidence isolates constant-response, duplicate-family and installed-CLI
   registration failures.
2. B-T08 tests pass with no required skip; source and installed B-T08 CLI runs
   both write clinical results and the B-T08 acceptance record.
3. B-T09 and Stage B composition regression suites remain green.
4. A fresh Sol-high reviewer accepts the complete diff.
5. The complete suite and fresh package check pass with no warning/skip/NOTE.
6. The one available focused M4-005 Astra rereview confirms B-T08 acceptance and
   B-T07 dependency legality before B-T07 dispatch.

Repair 1 receives at most one targeted correction before repair 2 would be
needed. No repair changes the failed historical ordinary B-T04 diagnostic or
constitutes biological validation.

