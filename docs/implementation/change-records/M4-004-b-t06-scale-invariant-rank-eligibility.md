# M4-004 — reopen B-T06 with scale-invariant rank-test eligibility

- Date: 2026-10-15
- Trigger: the focused review found that an absolute tolerance admits a
  treatment-mean contrast as if it were target minus reference.
- Status: **proposed; not accepted**
- Evidence:
  `docs/implementation/investigations/B-T06-scale-invariant-rank-eligibility.md`
- Independent audit:
  `docs/implementation/reviews/B-T06-reopening-astra-audit.md`
- Focused rereview: **PASS** on corrected snapshot `0dfe88d`

## Proposed decision

Preserve B-T06's scientific contract and replace the scale-dependent eligibility
test with the structural contrast invariant. An unadjusted two-group Wilcoxon
sensitivity may run only when the registered contrast has exactly the two
declared group columns, finite weights, a nonzero target coefficient and weights
that are exact opposites. Direction continues to follow the target coefficient.

This accepts all nonzero scalar multiples of target minus reference while
rejecting group means and other incompatible contrasts at every scale.
The existing `target_weight - reference_weight` finiteness gate must be removed:
that subtraction can overflow even when both registered coefficients are finite.
Direction is derived directly from the sign of the target coefficient.

## Contract effect

This does not change the primary limma result, the Wilcoxon procedure, or its
ties/zeros/exact policies. It implements B-T06-AC4's restriction to an eligible
unadjusted two-group question. Because eligibility is structural, an absolute
numerical tolerance is scientifically inappropriate here.

## Preserved behavior

- Wilcoxon remains sensitivity analysis, never a substitute for moderated
  inference.
- `exact=FALSE`, continuity correction, tie and zero reporting remain unchanged.
- Adjusted, repeated/multivisit and incompatible estimands remain unavailable.
- Fixed-family accounting, q values and nomination behavior remain unchanged.
- No real-study inputs or empirical tuning may be used.

## Authorized work only if Omar accepts

Acceptance authorizes the packet's second and final targeted repair in a fresh
isolated Luna max worktree. Sol must begin from the then-current integration
snapshot and restore all six packet-owned files from the unintegrated candidate
commits
`033e0f6ea637cc7f9cc14643592d7453f2a8e643` and
`9b85bbd616a797c03f133fb43a5f0cce1260bb7d` unchanged:

- `R/multiplicity.R`
- `R/nominations.R`
- `R/rank_sensitivity.R`
- `tests/testthat/test-B-T06.R`
- `inst/examples/tickets/B-T06.yaml`
- `docs/implementation/registrations/B-T06.json`

After that restoration, the Luna repair may modify only:

- `R/rank_sensitivity.R`
- `tests/testthat/test-B-T06.R`

The worker must first add the four scale/orientation regressions in the
investigation plus finite extreme weights `(-1e308, 1e308)`. Tests must assert
that positive scaling preserves orientation, negative scaling reverses direction
without changing the two-sided rank-test p-value, and extreme finite coefficients
do not fail because their subtraction overflows. The worker then makes the
smallest eligibility repair, runs the B-T06 suite and synthetic CLI, and stops
with a concise receipt.

The 2026-10-14 initial review and focused re-review remain exhausted historical
events; renaming a review does not reset their counters. This record, only if
Omar accepts it, explicitly authorizes exactly one additional Sol 6.1 high
review of repair 2. That reviewer must inspect the complete packet diff against
the current integration snapshot and rerun the packet/adversarial tests. Sol may
integrate only after review acceptance and fresh adjacent/full/package checks.
If repair 2 or this one-time additional review leaves an unresolved blocker,
B-T06 returns to blocked with no further packet repair or re-review allowance
under this record. The independent Astra M4 milestone gate remains required
before any consumer is released.

## User decision

Pending Omar's explicit acceptance. Proposal and audit do not authorize code.
