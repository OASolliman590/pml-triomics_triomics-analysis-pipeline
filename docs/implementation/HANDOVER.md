# Implementation handover

Run `triomics-20260920-sol-01` is active on branch
`spec/harden-v2-20260920`. G3 is accepted on immutable snapshot `af323f8` after
the one permitted focused Astra low re-review passed.

G0 and G1 remain accepted. G2 is accepted on immutable milestone snapshot
`8b0754c1ea581ba18c4bb2e7eb862c26eeb625a7` after its one permitted focused
Astra re-review. The accepted repair candidates were A-T03 `901e39b` and A-T09
`9c4929a`; they were integrated as `f671254` and `e691ab9` respectively.

Fresh coordinator evidence on the integrated branch:

- full local suite: 2,296 passed, 0 failed, 0 warned, 0 skipped;
- package build/check in `artifacts/package-check-g2-e691ab9`: passed;
- immutable G2 snapshot: 512 focused A-T02/A-T03/A-T09 assertions passed;
- immutable snapshot package check: `Status: OK`.

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
- B-T04 remains blocked: 23/400 false discoveries for the one-cell-removed null
  pattern; upper 95% bound 0.08048 exceeds the frozen 0.08 limit.
- B-T05, B-T06, B-T09, B-T07, all C, and all D remain unstarted.
- G3-dependent work may now proceed subject to each packet's own dependency and
  file-boundary gates. B-T04 remains independently blocked and its frozen seeds
  and 0.08 bound must not change.

## Literal resume instruction

Read root `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, `reviews/M2-astra-review.md`, and
`reviews/M3-astra-review.md`, and both A-T05 G3 repair reviews. Verify HEAD and
dirty state. Do not repeat G2, either G3 review, either exhausted A-T05 repair,
or the accepted G3-001 implementation/review. Continue from the accepted G3 gate
with the next dependency-legal Stage B packet. Preserve B-T04's seeds and 0.08
bound.

## Orchestration correction

The coordinator missed two completed Luna turns and then two completed Sol review
turns until Omar prompted it. `ORCHESTRATION_CONTROL.md` now defines the mandatory
completion-to-next-action protocol. On resume, a coordinator may not return a
final answer while a run-owned chat remains active unless Omar explicitly orders
a pause. Every completed chat must be reconciled into Git/ledger state and either
trigger its authorized next gate or be recorded as stopped before the coordinator
reports completion.
