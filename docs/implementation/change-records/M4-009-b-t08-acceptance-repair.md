# M4-009 — B-T08 acceptance and installed-registration repair

- Date: 2026-10-16
- Trigger: fresh acceptance review of integrated B-T08 found three P1 defects
- Review: `docs/implementation/reviews/B-T08-acceptance-review.md`
- Status: blocked at final package check after both permitted repairs were consumed
- Counter: B-T08 initial implementation 1; the Luna implementation below consumes
  targeted repair 1 of the packet's existing maximum of 2. Any later
  implementation correction consumes targeted repair 2; there is no uncounted
  third attempt.

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
behaviors. A mixed-family regression must prove that valid members may retain
their family-adjusted q-values while a constant member remains in the internal
family accounting at p = 1 and cannot publish q. No fallback correlation or
silent deduplication is allowed.

Sol alone owns the shared registration repair and may modify only:

- `NAMESPACE`, adding the six exports already listed in the B-T08 registration;
- `man/triomics-api.Rd`, documenting those six exported interfaces without
  changing their scientific contract;
- the Stage B branch of `R/cli.R`, preparing the declared B-T08 fixture before
  Stage B and calling `triomics_execute_clinical_associations(config_path,
  output)` exactly once after successful Stage B execution only when the B-T08
  route is eligible, without disrupting B-T09;
- only the source-only `triomics_main` hook at the end of
  `R/clinical_objectives.R`, removing it once explicit shared dispatch exists.

Eligibility must remain restricted to the declared
`triomics_b_t08_fixture` with `clinical.enabled = true`; `clinical.enabled`
alone is not a dispatch trigger. The registered path must execute exactly once
in source and installed layouts. It must preserve B-T09's separate registered
longitudinal call, must not construct B-T08 synthetic endpoints for a B-T09 or
other configuration, and must fail closed through the existing CLI error mapping
when a nonfixture configuration requests a clinical objective without an
eligible registered endpoint bundle. No other B-T08, shared package, fixture,
dependency, specification or private-data file may change.

## Required gates

1. RED evidence isolates constant-response, duplicate-family and installed-CLI
   registration failures.
2. B-T08 tests pass with no required skip; source and installed B-T08 CLI runs
   both write clinical results and the B-T08 acceptance record exactly once.
3. Source and installed regressions prove that B-T09 executes only its own
   route, a nonfixture clinical request refuses without constructing B-T08
   synthetic endpoints, and Stage B composition remains green.
4. A fresh Sol-high reviewer accepts the complete diff.
5. The complete suite and fresh package check pass with no warning/skip/NOTE.
6. The one available focused M4-005 Astra rereview confirms B-T08 acceptance and
   B-T07 dependency legality before B-T07 dispatch.

The implementation dispatched under this record is targeted repair 1. If its
review or verification requires a further implementation change, that change
is targeted repair 2 and exhausts the packet's repair allowance. No repair
changes the failed historical ordinary B-T04 diagnostic or constitutes
biological validation.

