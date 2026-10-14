# M2 / G2 Astra review

Date: 2026-10-14
Reviewer role: fresh Astra low, read-only
Final immutable snapshot: `8b0754c1ea581ba18c4bb2e7eb862c26eeb625a7`
Integrated branch commit: `e691ab9c9d798e650a73802e98bac78b9598bfd9`

## Initial review

Verdict: `CHANGES_REQUIRED`.

1. A-T03 apply compared incomplete operation definitions, allowing changed
   preprocessing parameters to reuse a fit state.
2. A-T03 imposed transform-before-normalise globally, blocking a valid
   method-specific normalise-before-transform route.
3. A-T09 could approve an exact/nearest clinical link despite contradictory
   explicit sample identity.

The frozen specification, seeds and acceptance thresholds were not changed.

## Bounded repairs and independent review

- A-T03 candidate `901e39ba8adf324aa9bdc352cf8e1b1f63d133b5`;
  fresh Sol 6.1 high focused re-review: `ACCEPTED`.
- A-T09 candidate `9c4929aab3717a12d2a8f477c46e471a1280f0e4`;
  fresh Sol 6.1 high focused re-review: `ACCEPTED`.
- Integrated branch full suite: 2,296 passed, 0 failed, 0 warned, 0 skipped.
- Integrated branch package build/check: passed.
- Final snapshot focused suite: 512 passed, 0 failed, 0 warned, 0 skipped.
- Final snapshot package check: `Status: OK`.

## Focused Astra re-review

Verdict: `ACCEPTED`.

Astra confirmed that frozen parameters and unavailable capabilities cannot be
widened at apply time; valid method-specific order is preserved while sequencing
invariants remain enforced; and contradictory sample identities fail closed while
sample-scoped exact linkage without a visit remains valid. Astra independently
reran all 512 assertions from an exact-commit temporary copy. The spec hash was
unchanged.

## Sol disposition

G2 is accepted for the specified synthetic software behavior. This does not
constitute biological validation, real-study execution, or acceptance of G3/G4.
