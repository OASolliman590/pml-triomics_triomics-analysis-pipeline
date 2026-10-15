# Tri-omics — current progress

**Goal:** Build a local, reproducible pipeline for quantified proteomics,
metabolomics and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** G3 and five Stage B analysis packets are accepted on synthetic
fixtures. B-T04 supports qualified fixed-subject multivisit and trajectory
analysis; the integrated branch passed 2,706 assertions and package check.

**Changed:** The remaining B-T05 and B-T06 blockers were reproduced. Two bounded
repair records now passed focused Astra review: preserve DEqMS identity/fallback
when counts are missing, and make Wilcoxon eligibility scale-invariant. No code
from either blocked candidate is integrated yet.

**Not established:** This is fixture-based software/calibration evidence, not
biological validation. None of the fasting, diabetic-treatment or
organophosphate studies has been fitted. B-T04's ordinary 23/400 diagnostic
remains failed even though its qualified moderated route passed. B-T05 and B-T06
remain blocked until their audited records are explicitly accepted and repaired.

**Next milestone:** Resolve B-T05 and B-T06, then implement B-T07 and submit the
complete M4 snapshot to G4 review.

**Needs Omar:** Accept or reject M4-003 and M4-004. Acceptance permits
one final repair and one additional review per packet; it does not pre-accept code.

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
repeat accepted B-T04. M4-003/M4-004 passed focused audit but remain unaccepted.
No downstream packet is legal. Do not loosen B-T04's frozen controls.

</details>
