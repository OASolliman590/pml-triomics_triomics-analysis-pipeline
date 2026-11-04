# M8-D08-S01 initial scope audit

Fresh read-only Astra-low role `/root/d08_s01_scope_astra` reviewed archive b3674f33fc1932a61c87b56f7c504aa59b5be1f3, SHA-25653445215a6e4f396e39d5df30a0bbeec1f821511533df44e4fcf205fb3b66f39. Verdict: audit_pass no; filename isolation is appropriate but publication semantics need refinement. Effective model/effort were not independently exposed.

1. P1: distinguish producer-artifact completion from the complete stage; the repeated B writer publishes its record before later calibration/acceptance and possible abort.
2. P1: define canonical/legacy order, retained state on second-write failure, and fault-injection assertions preventing partial/orphan metadata from counting as stage completion.
3. P2: explicitly reject existing canonical records including identical bytes, unsupported legacy stages, dangling links and directories; bind A identity through the A envelope/payload and current configuration.
4. P2: exclude C-09's remaining legacy run.json writer and preserve its unqualified status.
5. Name existing focused regressions for A-T09, B-T05 integration, C-06-shared and D-T01, plus both B call paths and same-root preservation.

Sol made one documentation revision covering these points. A fresh focused rereview is next; no implementation is authorized by this audit or document revision. This is not the M8 milestone review and accepts no packet or gate.
