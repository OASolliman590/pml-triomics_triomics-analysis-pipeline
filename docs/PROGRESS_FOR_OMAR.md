# Tri-omics progress

**Goal:** Build a local, reproducible pipeline for quantified proteomics,
metabolomics and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** G3 and five Stage B analysis packets are accepted on synthetic
fixtures. B-T04 supports qualified fixed-subject multivisit and trajectory
analysis; the integrated branch passed 2,706 assertions and package check.

**Changed:** Both final repairs fixed their narrow defects and passed packet
tests, but neither candidate is integrated. B-T05 still needs shared CLI
provenance/status work. B-T06's restored startup wrapper removes the accepted
B-T04 repeated-analysis dispatch chain. Their authorized repair cycles are now
exhausted.

**Not established:** This is fixture-based software/calibration evidence, not
biological validation. None of the fasting, diabetic-treatment or
organophosphate studies has been fitted. B-T04's ordinary 23/400 diagnostic
remains failed even though its qualified moderated route passed. B-T05 and B-T06
remain blocked with no authorized repair remaining.

**Next milestone:** M4 cannot reach G4. Preserve both blocked candidates and
decide later whether a new audited shared-integration record is warranted.

**Needs Omar:** Nothing now. Reopening later would require a new explicit,
independently audited shared-integration record.

**Sync:** Local repository documentation updated. No Notion sync or external
publication was attempted.

<details>
<summary>Technical evidence and restart</summary>

Integrated B-T04 code HEAD: `af69a9e`.
Review: `docs/implementation/reviews/B-T04-review.md`.

Software evidence: B-T04 302/0/0/0; full suite 2,706/0/0/0; package check
`Status: OK`; CLI AC1–AC4 pass. Real-data scientific evidence: not run. No
required test was skipped.

Restart: read `docs/implementation/HANDOVER.md` and verify Git state. The
M4-003/M4-004 cycles are exhausted; do not integrate either candidate. No
downstream packet is legal. Preserve B-T04 controls.

</details>
