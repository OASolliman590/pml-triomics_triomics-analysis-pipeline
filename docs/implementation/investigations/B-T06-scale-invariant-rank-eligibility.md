# B-T06 scale-invariant rank-eligibility investigation

- Date: 2026-10-15
- Candidate inspected: `9b85bbd616a797c03f133fb43a5f0cce1260bb7d`
- Scope: public synthetic fixtures and repository code only
- Status: root cause reproduced; no implementation performed

## Observed failure

The candidate packet suite passes 52/52 assertions, with no failures, warnings or
skips. The reviewer adversarial contrast with weights
`condition.control = 0` and `condition.treatment = 1e-9` nevertheless returns a
completed `wilcoxon_rank_sum` sensitivity result. That contrast estimates a tiny
treatment mean, not treatment minus control, so B-T06-AC4 requires the rank test
to be unavailable.

## Root cause

`triomics_b06_two_group_rank()` accepts the contrast when
`abs(reference_weight + target_weight)` is below
`1e-8 * max(1, abs(reference_weight), abs(target_weight))`. The fixed lower
scale of one makes the predicate non-invariant to rescaling. Consequently
`(0, 1e-9)` is accepted even though `(0, 1)` is rejected.

## Repair invariant

An unadjusted two-group Wilcoxon sensitivity is eligible only when the registered
contrast contains exactly the two declared condition columns, both weights are
finite, the target coefficient is nonzero, and the coefficients are exact
opposites. This accepts any nonzero scalar multiple of target minus reference,
including `(-1e-9, 1e-9)` and reversed orientation `(1, -1)`, while rejecting
the treatment-mean contrast `(0, 1e-9)`. No absolute tolerance or scale floor is
needed for this structural eligibility decision.

## Required regression probes

1. Canonical `(-1, 1)`: eligible and oriented target minus reference.
2. Tiny valid difference `(-1e-9, 1e-9)`: eligible.
3. Tiny treatment mean `(0, 1e-9)`: unavailable with the existing incompatible
   contrast reason.
4. Reversed `(1, -1)`: eligible with reversed direction.
5. Extreme finite `(-1e308, 1e308)`: eligible without computing an overflowing
   coefficient difference; direction follows the target weight.
6. Positive/negative scalar multiples retain the same two-sided Wilcoxon p-value
   while direction changes only with the target coefficient's sign.
7. Extra/missing/nonfinite weights and adjusted designs: unavailable.
8. Existing ties, zeros and `exact=FALSE` policy assertions remain unchanged.

The private study directory was not read, fitted or uploaded.
