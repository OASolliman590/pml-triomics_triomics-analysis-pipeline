# Implementation handover

Run `triomics-20260920-sol-01` is active on branch
`spec/harden-v2-20260920`. G3 is accepted. M4 is blocked and not eligible for
G4 review; no dependency-legal Stage B, C or D packet remains dispatchable.

G0 and G1 remain accepted. G2 is accepted on immutable milestone snapshot
`8b0754c1ea581ba18c4bb2e7eb862c26eeb625a7` after its one permitted focused
Astra re-review. The accepted repair candidates were A-T03 `901e39b` and A-T09
`9c4929a`; they were integrated as `f671254` and `e691ab9` respectively.

Fresh coordinator evidence after accepted B-T09 integration and registration:

- full local suite: 2,404 passed, 0 failed, 0 warned, 0 skipped;
- package build/check in `artifacts/package-check-stage-b-7e11c47-docs`:
  `Status: OK`;
- build/check log SHA-256:
  `d8b558aa37e27eca723b52ece7974568c614682c5a24af4ae8297b9f5d4d1ab4` /
  `c92dad89ffdb5a342bd8247c7f177d5da21dbdd09520833b2751a54a460b644e`.

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
- B-T05 is blocked after its focused review: missing counts still bypass the
  method-specific unavailable/fallback path. Its candidates are unintegrated.
- B-T06 is blocked after its focused review: the Wilcoxon contrast check remains
  scale-dependent for weights 0 and 1e-9. Its candidates are unintegrated.
- B-T09 is accepted and integrated. It passed 83 packet assertions, 139 B-T08
  regression assertions, 13 cross-cutting CLI probes, source/installed fixture
  execution, schema checks and package check. Unsupported real-study
  longitudinal objectives fail closed until eligible inputs exist.
- B-T07 is unstarted because it depends on B-T04. G4, all C and all D remain
  unreleased.

## Literal resume instruction

Read root `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, `reviews/M2-astra-review.md`, and
`reviews/M3-astra-review.md`, `milestones/M4.md`, and `reviews/B-T09-review.md`.
Verify HEAD and dirty state. Do not repeat accepted G2/G3/B-T09 work or any
exhausted B-T04/B-T05/B-T06 attempt or review. There is no legal downstream
packet until an explicit accepted change record reopens an affected blocker.
Preserve B-T04's seeds and 0.08 bound.

## Orchestration correction

The coordinator missed two completed Luna turns and then two completed Sol review
turns until Omar prompted it. `ORCHESTRATION_CONTROL.md` now defines the mandatory
completion-to-next-action protocol. On resume, a coordinator may not return a
final answer while a run-owned chat remains active unless Omar explicitly orders
a pause. Every completed chat must be reconciled into Git/ledger state and either
trigger its authorized next gate or be recorded as stopped before the coordinator
reports completion.
