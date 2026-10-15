# Tri-omics — current progress

**Goal:** Build a local, reproducible pipeline for quantified proteomics,
metabolomics and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** G3 and five Stage B analysis packets are accepted on synthetic
fixtures. B-T04 now supports qualified fixed-subject multivisit and trajectory
analysis; the integrated branch passed 2,706 assertions and package check.

**Changed:** B-T04's qualification now tests the same limma-moderated statistic
that the pipeline publishes. The failed ordinary diagnostic remains visible:
23 discoveries in 400 simulations, upper bound 0.08048. No seed, generator or
0.08 limit was loosened. The moderated route passed all five declared layouts.

**Not established:** This is fixture-based software/calibration evidence, not
biological validation. None of the fasting, diabetic-treatment or
organophosphate studies has been fitted. Additional covariance, missingness and
robust-moderation routes remain unavailable. B-T05 and B-T06 remain blocked.

**Next milestone:** Resolve B-T05 and B-T06, then implement B-T07 and submit the
complete M4 snapshot to G4 review.

**Needs Omar:** Nothing now.

**Sync:** Local repository documentation updated. No Notion sync or external
publication was attempted.

<details>
<summary>Technical evidence and restart</summary>

Integrated B-T04 code HEAD: `af69a9e`.
Review: `docs/implementation/reviews/B-T04-review.md`.

Software evidence: B-T04 302/0/0/0; full suite 2,706/0/0/0; package check
`Status: OK`; CLI AC1–AC4 pass. Real-data scientific evidence: not run. No
required test was skipped.

Restart: read `docs/implementation/HANDOVER.md`, verify Git state, and do not
repeat accepted B-T04. No downstream packet is legal until B-T05 and B-T06 are
reopened by accepted records. Do not loosen B-T04's seeds, generator or bound.

</details>
