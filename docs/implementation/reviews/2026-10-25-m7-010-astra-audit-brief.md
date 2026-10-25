# M7-010 fresh read-only amendment audit brief

Audit the immutable M7-010 snapshot named in the dispatch receipt. The primary
document is:

- `docs/implementation/change-records/M7-010-b-t04-synthetic-producer-lineage.md`

Compare it with:

- accepted M7-002 and M7-003 provenance amendments;
- `docs/implementation/reviews/2026-10-25-m7-009-integration-gate.md`;
- `R/design_repeated.R`, especially `triomics_b04_execute_fixture()`,
  `triomics_b04_resolve()` and `triomics_b04_fit()`;
- `R/results_b.R`, especially the generic same-run context and unit-map helper;
- `R/export_b.R`, especially context validation and native profile building;
- exact B-T04, Stage-B composition and B-T07 tests; and
- the frozen B-T04 ticket and calibration controls.

The reviewer is read-only. Do not edit, implement, rerun private data, accept a
packet or release downstream work.

Report Critical/Important/Minor findings and `audit_pass: yes|no`. Specifically
determine whether:

1. the retained synthetic protocol and generated domain artifacts prove every
   material B-T04 input and transformation;
2. the configured paired-example TSVs are unambiguously non-ancestral in both
   provenance and the block-results envelope;
3. the contract truthfully distinguishes the uncentered generated matrix handed
   downstream from the subject-centered response fitted by limma;
4. missing or conflicting evidence always fails closed;
5. the existing closed role/schema vocabulary can express the design without a
   schema change; and
6. the allowlist and tests are sufficient to prevent a false lineage or a
   statistical/calibration change.

This audit approves or blocks the design only. It does not establish product
correctness or authorize any action outside M7-010.
