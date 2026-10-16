# M4/G4 immutable Astra-low review

- Date: 2026-10-16
- Reviewer: `/root/m4_g4_astra_review` (fresh Astra low, read-only)
- Reviewed commit: `20e2a35fc442c877874ecf797a2073970955e157`
- Frozen spec hash: `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`
- Verdict: **changes_required**

## Blocking finding

`M4-G4-001` (P1, acceptance accounting): B-T01, B-T02 and B-T03 remained
`sol_verified` with empty acceptance references and stale pre-integration
blockers. The implementation and later integrated tests provide substantial
evidence, but the frozen M4 criterion requires every required packet to be
explicitly accepted. Sol confirmed that no prior immutable acceptance record
existed. The correction may be record-only: preserve history and counters,
record an evidence-backed Sol acceptance disposition, clear stale blockers and
submit the documentation diff for the single focused M4 rereview.

## Substantive disposition

No new software or scientific-contract blocker was demonstrated. The review
confirmed that B-T04's ordinary 23/400 diagnostic remains failed and separate
from the qualified moderated publication route; fixed-family and unavailable
row accounting remain intact; clinical negative/privacy tests remain bounded;
same-run matrices and dependency hashes remain checked; and no required test
was skipped. Package log hashes matched and the check log ended `Status: OK`.

Nonblocking qualifications: retain M4-012's P2 unavailable-method traceability
ambiguity; do not claim a fresh spec-validator pass because the current system
Python lacks its dependencies; and treat all M4 evidence as synthetic software
evidence, not biological validation. B-T07, G4 and Stage C remain unreleased
pending the record correction, focused rereview and Sol disposition.
