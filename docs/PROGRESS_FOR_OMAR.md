# Tri-omics progress

**Goal:** Build a reproducible analysis pipeline for quantified proteomics,
metabolomics and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** G3 and the accepted Stage B packets remain stable. B-T04 provides
the qualified repeated-visit route. M4-005A is now integrated: B-T05 results keep
the requested method, the method actually used, named unavailability reasons and
explicit fallback provenance. It cannot falsely label fallback output as DEqMS.

**Software evidence:** The M4-005A target passed 302 assertions; its adjacent
B-T01/B-T03/B-T04 run passed 818. Both had zero failures, warnings or skips. A
clean package build/check returned `Status: OK`, the archive installed cleanly,
and an installed B-T05 fixture produced a valid unavailable result with the
correct reason. A fresh Sol reviewer accepted the correction and dependency
closure.

**Scientific evidence:** This remains synthetic fixture and calibration
evidence. None of the three private studies has been fitted, uploaded or used for
biological claims. B-T04's ordinary 23/400 diagnostic remains failed; only its
qualified production-aligned route passed its declared calibration.

**Next:** Execute M4-005B to compose B-T02 through B-T06 without disconnecting
B-T04. Then a fresh Astra audit must accept the combined snapshot before B-T05,
B-T06 or B-T07 can advance. No decision is currently needed from Omar.

**Sync:** Local repository documentation updated. No Notion sync or external
publication was attempted.

<details>
<summary>Technical checkpoint</summary>

Unit A integration commits: `09d33f6`, `03b9ab0`, `45eb7e5`, `cc21eb5`,
`d76e564`. Reviews:
`docs/implementation/reviews/M4-005A-focused-rereview.md` and
`docs/implementation/reviews/M4-005A-registration-review.md`.

Restart at `docs/implementation/plans/M4-005B-stage-b-dispatch-composition.md`.
Preserve B-T04 controls and exclude private study data.

</details>
