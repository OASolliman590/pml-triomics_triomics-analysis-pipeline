# Implementation handover

Run `triomics-20260920-sol-01` is stopped blocked on branch
`spec/harden-v2-20260920`. No run-owned worker, reviewer, or polling loop is
active.

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
- M3/G3: blocked. A-T08 and A-T06 repairs were independently accepted and
  integrated as `610a732` and `62b2e7d`; their combined integrated suite passed
  455 assertions. A-T05 exhausted both repairs. A caller-authored artifact with
  current API fingerprints can still self-qualify kNN/RF without authoritative
  approval. Candidates `4c51fcc` and `d21f6b0` were not integrated.
- B-T02, B-T03, and B-T08 remain Sol-verified.
- B-T04 remains blocked: 23/400 false discoveries for the one-cell-removed null
  pattern; upper 95% bound 0.08048 exceeds the frozen 0.08 limit.
- B-T05, B-T06, B-T09, B-T07, all C, and all D remain unstarted.
- B-T04, B-T05, B-T06, and B-T09 all require G3. No downstream implementation
  packet is currently legal. The focused Astra G3 re-review was not run.

## Literal resume instruction

Read root `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, `reviews/M2-astra-review.md`, and
`reviews/M3-astra-review.md`, and both A-T05 G3 repair reviews. Verify HEAD and
dirty state. Do not repeat G2, the initial G3 review, or either exhausted A-T05
repair. Resume only after an authorized contract-compliant source of
`MethodQualification` approval is available or an explicit accepted change
record alters the affected scientific contract. Then use a newly authorized
repair path, rerun M3 verification, and obtain the focused Astra re-review. Do
not restart B-T05 until G3 is accepted. Preserve B-T04's seeds and 0.08 bound.

## Orchestration correction

The coordinator missed two completed Luna turns and then two completed Sol review
turns until Omar prompted it. `ORCHESTRATION_CONTROL.md` now defines the mandatory
completion-to-next-action protocol. On resume, a coordinator may not return a
final answer while a run-owned chat remains active unless Omar explicitly orders
a pause. Every completed chat must be reconciled into Git/ledger state and either
trigger its authorized next gate or be recorded as stopped before the coordinator
reports completion.
