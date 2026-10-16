# M4-012 B-T07 repair 2 — fresh Sol-high review

- Date: 2026-10-16
- Reviewer: `/root/b_t07_repair2_sol_review`
- Base: `c5f868f`
- Candidate: `a893475`
- Verdict: **FAIL as reviewed**
- Findings: P0 0, P1 1, P2 0

The exact package-aware B-T07 suite passed 124 expectations with zero failures,
warnings or skips. The exact CLI on a clean candidate archive exited 0, created
one same-run matrix handoff, marked AC3 pass and validated seven of seven
envelopes. The diff changed exactly `R/export_b.R` and
`tests/testthat/test-B-T07.R`; the worktree was clean.

The reviewer found one P1. After removing `requested_method`, `method`,
`deqms_status` and `unavailable_reason` from an otherwise unavailable payload and
rehashing its envelope, the no-handoff guard still returned
`no_exportable_public_claims` using the aggregate unavailable status. Wrong
paths, partial or complete empty payloads, malformed blocks, missing matrices and
corrupt hashes otherwise failed closed; complete claims still exported and no
skipped case wrote a C handoff.

Because repair 2 exhausted the packet budget, the coordinator dispatched a
read-only Astra contract adjudication rather than changing code or granting an
unrecorded third repair. B-T07 remained unaccepted.
