# G3-001 — metabolomics advanced imputation remains unavailable in v2

- Date: 2026-10-14
- Trigger: A-T05 exhausted its frozen repair budget because caller-authored
  metadata and current code fingerprints could self-authorize kNN/RF execution.
- User decision: Omar explicitly selected and approved Option 1 after a
  scientific-literature and public-GitHub review.
- Evidence summary:
  `reports/2026-10-14-metabolomics-imputation-validity-survey.md`.

## Decision

For triomics v2, metabolomic kNN and random-forest imputation remain recognized
conditional methods but are always unavailable in production preparation. No
caller-supplied qualification object, API label, implementation fingerprint, or
self-authored manifest can activate them. The named unavailability reason is
`qualification_unavailable_in_v2`.

The executable metabolomic imputation routes remain:

- `none`, for downstream analyses that can validly retain incomplete values;
- `training_median`, as an explicitly labelled baseline/sensitivity route fitted
  from training observations only and applied unchanged to held-out data.

Training median is not a biological default and does not claim to reconstruct a
true metabolite concentration. Analyses that require a complete matrix must not
silently accept incompatible remaining missing values.

## Contract effect

This is an accepted change record in the authority chain defined by `AGENTS.md`.
It does not edit or weaken the frozen v2 specification, seeds, thresholds, or
packet identities. It supplies the explicit disposition that the frozen text
already permits when no qualified inductive contract is available:

- A-T05-AC3 is satisfied in v2 by fail-closed kNN/RF behavior plus adversarial
  proof that caller-authored evidence cannot activate either route.
- Existing internal experimental helpers may remain private, but no production
  operation may dispatch them.
- kNN/RF are not described as scientifically invalid. Future activation requires
  a separately approved change record, an authoritative qualification source,
  leakage-safe fit/apply behavior, study-representative validation, and affected
  acceptance review.

## Authorized implementation and review

The two exhausted repairs remain rejected history. This decision authorizes one
new bounded change implementation, not a third attempt under the rejected
qualification design. The A-T05 allowlist remains unchanged. The worker must use
test-first development and prove:

1. copied/current fingerprints and apparently pinned artifacts cannot activate
   kNN or RF;
2. both methods return `unavailable / qualification_unavailable_in_v2`;
3. `prepare_fit` refuses both without mutating the input;
4. `none` and fold-local training median continue to behave as specified; and
5. the exact A-T05 suite and Stage A CLI complete without skipped acceptance.

A fresh Sol 6.1 high reviewer must accept the packet diff. Sol then performs
integrated verification and creates an immutable M3 snapshot for the one allowed
focused Astra low re-review. G3 and its consumers remain closed until that review
is accepted.
