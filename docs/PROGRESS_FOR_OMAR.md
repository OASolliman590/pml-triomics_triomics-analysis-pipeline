# Tri-omics progress

**Goal:** Build a reproducible pipeline for quantified proteomics, metabolomics
and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** Stage B’s independent, paired, repeated-visit, DEqMS/fallback,
multiplicity, rank-sensitivity and clinical-association lanes coexist in one
dispatch path. B-T04’s qualified repeated route remains connected; B-T05 reports
what method actually ran; B-T06 preserves fixed-family accounting.

**Changed:** B-T08 now handles constant outcomes and duplicate testing-family
entries safely, exports its six public interfaces, runs only for its declared
fixture, leaves B-T09 separate, and refuses unsupported nonfixture clinical
execution. Fresh Sol review found no defect. The complete local suite passed
3,298 assertions with no failures, warnings or skips.

**Not established:** This is tested on public synthetic fixtures only—not
biological validation. None of the three private studies was read or fitted.
B-T04’s ordinary 23/400 diagnostic remains failed; only its frozen
production-aligned route passed calibration.

**Next:** B-T08 is not accepted yet. Its clean installed-package check stopped
on three test-setup errors because the new CLI test helper looks for ticket files
using a source-tree path that does not exist after installation. This is a test
harness defect, not a failed metabolomics statistic, but the mandatory gate still
failed. B-T07 therefore cannot start.

**Needs Omar:** Decide whether to permit one explicit exception to the frozen
two-repair ceiling for this single installed-test path correction. No change is
authorized until that decision is recorded.

**Sync:** Local repository documentation updated; no Notion sync or publication
was attempted.

<details>
<summary>Technical checkpoint</summary>

Candidate tip before the blocked-gate receipt: `ed94035`. Verification:
`docs/implementation/reviews/M4-009-package-check-gate.md`.

Literal restart: read `AGENTS.md`, the spec-v2 README, execution ledger and
handover; verify HEAD/dirty state; do not change B-T08 until Omar decides the
repair-ceiling exception. Keep the existing user `.gitignore` modification
untouched.

</details>
