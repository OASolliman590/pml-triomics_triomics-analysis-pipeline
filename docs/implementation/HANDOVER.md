# Implementation handover

Run `triomics-20260920-sol-01` is paused on branch
`spec/harden-v2-20260920` at integration commit
`e691ab9c9d798e650a73802e98bac78b9598bfd9`.

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
- M3/G3: implementation is present and Sol-verified, but the independent
  milestone review has not been run. This is the next legal gate.
- B-T02, B-T03, and B-T08 remain Sol-verified.
- B-T04 remains blocked: 23/400 false discoveries for the one-cell-removed null
  pattern; upper 95% bound 0.08048 exceeds the frozen 0.08 limit.
- B-T05, B-T06, B-T09, B-T07, all C, and all D remain unstarted.
- No worker, reviewer, auditor, polling loop, or implementation packet is active.

## Literal resume instruction

Read root `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, and `reviews/M2-astra-review.md`. Verify
HEAD and dirty state. Do not repeat G2. Construct an immutable M3 snapshot for
A-T04 through A-T08, run the required tests and package check, obtain the fresh
Astra-low G3 review, and record Sol's disposition. Do not restart B-T05 until the
required prior review gate is satisfied. Preserve B-T04's frozen seeds and 0.08
bound.

## Orchestration correction

The coordinator missed two completed Luna turns and then two completed Sol review
turns until Omar prompted it. `ORCHESTRATION_CONTROL.md` now defines the mandatory
completion-to-next-action protocol. On resume, a coordinator may not return a
final answer while a run-owned chat remains active unless Omar explicitly orders
a pause. Every completed chat must be reconciled into Git/ledger state and either
trigger its authorized next gate or be recorded as stopped before the coordinator
reports completion.
