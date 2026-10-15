# M4-004 — reopen B-T06 with scale-invariant rank-test eligibility

- Date: 2026-10-15
- Trigger: the focused review found that an absolute tolerance admits a
  treatment-mean contrast as if it were target minus reference.
- Status: **proposed; not accepted**
- Evidence:
  `docs/implementation/investigations/B-T06-scale-invariant-rank-eligibility.md`
- Independent audit: pending

## Proposed decision

Preserve B-T06's scientific contract and replace the scale-dependent eligibility
test with the structural contrast invariant. An unadjusted two-group Wilcoxon
sensitivity may run only when the registered contrast has exactly the two
declared group columns, finite weights, a nonzero target coefficient and weights
that are exact opposites. Direction continues to follow the target coefficient.

This accepts all nonzero scalar multiples of target minus reference while
rejecting group means and other incompatible contrasts at every scale.

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

Acceptance authorizes one bounded second targeted repair in a fresh isolated
Luna max worktree. The worker may reuse the unintegrated candidate commits
`033e0f6ea637cc7f9cc14643592d7453f2a8e643` and
`9b85bbd616a797c03f133fb43a5f0cce1260bb7d` as historical packet content, but
must begin from the then-current integration snapshot and modify only:

- `R/rank_sensitivity.R`
- `tests/testthat/test-B-T06.R`

The worker must first add the four scale/orientation regressions in the
investigation, then make the smallest eligibility repair. It must run the B-T06
suite and synthetic CLI and stop with a concise receipt.

A fresh Sol 6.1 high reviewer must inspect the complete packet diff against the
current integration snapshot and rerun the packet/adversarial tests. Sol may
integrate only after review acceptance and fresh adjacent/full/package checks.
This is a new accepted reopening record, not a second focused re-review of the
2026-10-14 candidate.

## User decision

Pending Omar's explicit acceptance. Proposal and audit do not authorize code.
