# Tri-omics — current progress

**Goal:** Build a local, reproducible pipeline for quantified proteomics,
metabolomics and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** G2 is accepted for synthetic ingest, identity preservation,
preparation-state freezing and typed clinical linkage. A repaired immutable
snapshot passed 512 focused assertions and package build/check. The integrated
branch passed 2,296 assertions with no failures, warnings or skips.

**Changed:** Preprocessing apply can no longer change fit-time parameters or
re-enable a method frozen as unavailable. Method-specific normalization before
transformation is supported without relaxing sequencing safeguards. Clinical
links now refuse contradictory explicit sample identities.

**Not established:** These are fixture-based software results, not biological
validation. None of the three real studies has been fitted. B-T04 remains blocked
because one null scenario produced 23 discoveries in 400 simulations, giving a
95% upper bound of 0.08048 against the frozen 0.08 maximum.

**Next milestone:** G3 must independently review the three assay profiles and
immutable preparation behavior before additional Stage B work is released.

**Needs Omar:** Nothing during this pause.

**Sync:** Local repository documentation updated. No Notion sync or external
publication was attempted.

<details>
<summary>Technical evidence and restart</summary>

Integrated HEAD: `e691ab9c9d798e650a73802e98bac78b9598bfd9`.
Accepted G2 snapshot: `8b0754c1ea581ba18c4bb2e7eb862c26eeb625a7`.
Review: `docs/implementation/reviews/M2-astra-review.md`.

Software evidence: full suite 2,296/0/0/0; integrated and snapshot package checks
reported `Status: OK`; snapshot focused suite 512/0/0/0. Real-data scientific
evidence: not run. No required test was skipped.

Restart: read `docs/implementation/HANDOVER.md`, verify Git state, then run the
M3/G3 immutable-snapshot review. Do not repeat G2 or loosen B-T04's seeds/bound.

</details>
