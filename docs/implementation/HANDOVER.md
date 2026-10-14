# Implementation handover

Run `triomics-20260920-sol-01` is active on branch
`spec/harden-v2-20260920`.

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
- M3/G3: the initial independent Astra review returned `CHANGES_REQUIRED` on
  snapshot `d96ca310cea5715f40e2cae72d59a29d411713bb`. One isolated Luna-max
  `repair_1` worker is active for each finding in A-T05, A-T06, and A-T08.
  A-T04 and A-T07 remain Sol-verified but are not accepted while G3 is open.
- B-T02, B-T03, and B-T08 remain Sol-verified.
- B-T04 remains blocked: 23/400 false discoveries for the one-cell-removed null
  pattern; upper 95% bound 0.08048 exceeds the frozen 0.08 limit.
- B-T05, B-T06, B-T09, B-T07, all C, and all D remain unstarted.
- Active repair chats and worktrees are recorded in `execution-ledger.json`.
  Each completion must transition immediately to a fresh Sol 6.1 high packet
  review. After all accepted repairs are integrated and verified, construct a
  new immutable M3 snapshot for the one permitted focused Astra re-review.

## Literal resume instruction

Read root `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, `reviews/M2-astra-review.md`, and
`reviews/M3-astra-review.md`. Verify HEAD and dirty state. Do not repeat G2 or
the initial G3 review. Reconcile the three active repair chats by their stable
thread IDs; inspect each commit, dispatch fresh Sol 6.1 high packet review, and
integrate accepted commits serially. Then rerun the M3 suite, CLIs, package check,
and focused Astra re-review. Do not restart B-T05 until G3 is accepted. Preserve
B-T04's frozen seeds and 0.08 bound.

## Orchestration correction

The coordinator missed two completed Luna turns and then two completed Sol review
turns until Omar prompted it. `ORCHESTRATION_CONTROL.md` now defines the mandatory
completion-to-next-action protocol. On resume, a coordinator may not return a
final answer while a run-owned chat remains active unless Omar explicitly orders
a pause. Every completed chat must be reconciled into Git/ledger state and either
trigger its authorized next gate or be recorded as stopped before the coordinator
reports completion.
