# C-08 repair-1 focused Sol-high rereview

Date: 2026-10-20. Candidate:
`53acd32ac6859b0ffcef8d8c7d610b195c8f545f`. Initial candidate:
`809fc8933d84f1b716205a829a717643c8c5e15f`. Reviewer:
`/root/c08_sol_review`, requested model `gpt-6.1-sol`, effort high. The rereview
was read-only and inspected no private data.

Verdict: **findings remain**. Critical 0, Important 3, Minor 0. The single
focused rereview allowance is consumed.

## Original finding status

1. Adapter shapes: addressed locally and correctly blocked on the missing
   upstream qualified handoff. PCA, DIABLO and block.spls score reproduction
   was probed; MOFA's accepted object shape matches without claiming a new
   native run.
2. Fabricated state identity: addressed. The result-derived fallback is gone.
3. Unbound lineage: addressed locally. Scores are reproduced and input, Y,
   selection and unit-map hashes must match the qualified handoff.
4. Unit/time compatibility: incomplete.

## Final bounded repair findings

1. `R/integration_endpoints.R:150-156` requires numeric `visit_weights`, but
   accepted mean maps from `R/design_levels.R` store named lists. A production
   representation probe fails. Preserve and validate the actual dependency
   representation.
2. Endpoint sample rows omit `visit_id` and `time`. The scheduler retains links
   separately but passes endpoints unchanged, while accepted B-T08/B-T09
   consumers read those fields from endpoint samples. Direct consumption of a
   successfully scheduled request returned unavailable/endpoint-absent.
   Establish an immutable B-compatible endpoint handoff without pretending a
   delta is two independent visit-level scores.
3. Between-person, effect-modification and prediction scheduling accepts
   nonblank but invalid collection dates. Validate dates before accepting these
   modes.

The reviewer independently reproduced 11 blocks and 206 passing assertions with
zero failure, warning, or skip. These tests miss the three production seams.
The exact CLI remains legacy PCA output and does not prove endpoint/request
composition. The registration request honestly records the still-missing shared
correctness repair.

Repair 2 is the final permitted packet repair and is confined to these three
findings in the existing allowlist. No second focused rereview is authorized;
Sol must inspect and verify directly. C-08 remains unintegrated and unaccepted,
and C-09/C-06/G6/D remain closed.
