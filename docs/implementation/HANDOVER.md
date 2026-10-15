# Implementation handover

Run `triomics-20260920-sol-01` is active on branch
`spec/harden-v2-20260920`. G3 is accepted. M4-005 execution is active: Unit A
is integrated and verified, and Unit B is the next dependency-legal packet.
M4 is not yet eligible for G4 review.

G0 and G1 remain accepted. G2 is accepted on immutable milestone snapshot
`8b0754c1ea581ba18c4bb2e7eb862c26eeb625a7` after its one permitted focused
Astra re-review. The accepted repair candidates were A-T03 `901e39b` and A-T09
`9c4929a`; they were integrated as `f671254` and `e691ab9` respectively.

Fresh coordinator evidence after accepted B-T04 integration and its installed
package repair:

- full local suite: 2,706 passed, 0 failed, 0 warned, 0 skipped;
- package build/check in `artifacts/package-check-b-t04-af69a9e`:
  `Status: OK`;
- build/check log SHA-256:
  `9923a71b7a38fddfe501ed620738b8392647702de6ddde9e337e8bccf645a8a0` /
  `6baea0de66a2c4012af2a5097797d0aab734a2d29adc9a2df1b9baa944dbf63f`.

This is synthetic software evidence only. None of Omar's fasting,
diabetic-treatment, or organophosphate study data was read, fitted, uploaded, or
scientifically validated.

## Current programme state

- M2/G2: accepted; review is in `reviews/M2-astra-review.md`.
- M3/G3: accepted. A-T08 and A-T06 repairs were independently
  accepted and integrated as `610a732` and `62b2e7d`. After Omar approved
  change record G3-001, A-T05 kNN/RF were made fail-closed in v2; `none` and
  fold-local training median remain executable. Candidate `6845d5c` was accepted
  by fresh Sol 6.1 high review and integrated as `a194a6b`. The integrated full
  suite passed 2,321 assertions and package check reported `Status: OK`. Astra's
  focused review found G3-01, G3-02 and G3-03 addressed and returned `PASS`.
- B-T02, B-T03, and B-T08 remain Sol-verified.
- B-T04 is accepted under change record M4-002. The ordinary diagnostic remains
  failed at 23/400 and upper bound 0.08048; the unchanged production-aligned
  moderated route passes all five declared cells. Candidate `bb0181b` and
  targeted installed-package repair `9e5f08e` were independently accepted and
  integrated as `753ae47` and `af69a9e`. Review: `reviews/B-T04-review.md`.
- B-T05's M4-003 cycle remains historically exhausted, but accepted change
  record M4-005 authorized a bounded shared-integration reopening. Unit A is now
  integrated through `d76e564`: method/unavailability/fallback provenance is
  retained, false DEqMS claims are excluded, its exact optional dependency
  closure is locked, targeted/adjacent tests pass, and package check is
  `Status: OK`. B-T05 is still not accepted pending Unit B and the shared Astra
  M4-005 audit.
- B-T06 is blocked after its final M4-004 repair/review cycle. Candidate
  `086d604` fixed scale-invariant Wilcoxon eligibility and passed 66 assertions,
  but its restored startup wrapper removes the integrated B-T04 dispatch/stage
  chain. M4-004 permits no further repair/review; the candidate remains
  unintegrated. Review: `reviews/B-T06-final-repair-review.md`.
- B-T09 is accepted and integrated. It passed 83 packet assertions, 139 B-T08
  regression assertions, 13 cross-cutting CLI probes, source/installed fixture
  execution, schema checks and package check. Unsupported real-study
  longitudinal objectives fail closed until eligible inputs exist.
- B-T07 is unstarted because B-T05 and B-T06 remain blocked. G4, all C and all
  D remain unreleased.
- The final blocked-cycle Astra audit passed after correcting B-T07's stale
  dependency reason. Review: `reviews/M4-final-cycle-astra-audit.md`. Neither
  blocked candidate is integrated.
- M4-005 defines the bounded reopening: the completed B-T05 result-boundary Unit
  A followed by centralized B-T02-through-B-T06 dispatch composition Unit B.
  Omar accepted the record and plans. Unit A passed focused Sol review,
  registration review and coordinator verification; M4-006's independent Astra
  audit accepted the one-value stale-test reconciliation. Unit B now starts from
  the integrated Unit A tree. No run-owned agent is active at this checkpoint.

## Literal resume instruction

Read root `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, `reviews/M2-astra-review.md`,
`reviews/M3-astra-review.md`, `milestones/M4.md`, `reviews/B-T04-review.md`, and
`reviews/B-T09-review.md`, then M4-003/M4-004, M4-005 and their audit receipts.
Verify HEAD and dirty state. Do not repeat accepted G2/G3/B-T09 work or any
accepted B-T04 work, or any exhausted B-T05/B-T06 attempt or review. M4-003 and
M4-004 final cycles are exhausted and blocked; do not repair either candidate
under those old records. M4-005 is independently audited and accepted for
execution. Unit A is already integrated; resume at
`plans/M4-005B-stage-b-dispatch-composition.md`, restoring the six B-T06 files
only inside its fresh worktree. B-T07 remains illegal until Unit B and the
bounded M4-005 shared-integration audit pass. Preserve B-T04's frozen controls.

## Orchestration correction

The coordinator missed two completed Luna turns and then two completed Sol review
turns until Omar prompted it. `ORCHESTRATION_CONTROL.md` now defines the mandatory
completion-to-next-action protocol. On resume, a coordinator may not return a
final answer while a run-owned chat remains active unless Omar explicitly orders
a pause. Every completed chat must be reconciled into Git/ledger state and either
trigger its authorized next gate or be recorded as stopped before the coordinator
reports completion.
