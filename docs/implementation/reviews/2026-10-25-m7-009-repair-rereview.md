# M7-009 focused repair rereview

- Date: 2026-10-25
- Repair fixed point: `762a0a09104fb70c920784f3c0897f3e96565567`
- Repair candidate: `2eb483ac7bcf5aa1919b8d72b9b8c8b239235861`
- Reviewers: `/root/m7_009_repair_standards_rereview`,
  `/root/m7_009_repair_spec_rereview`
- Reviewer edits: none
- Overall verdict: **pass**
- Integration or acceptance granted by this receipt: no

## Repair evidence

The pre-repair public-seam regression reproduced the defect with FAIL 2,
WARN 1, SKIP 0, PASS 10. The warning was the expected denied write beneath the
read-only handoff directory. After the repair, the same focused regression
passed with FAIL 0, WARN 0, SKIP 0, PASS 17.

The exact B-T07 suite passed 315 expectations. The focused B-T02 identity CLI,
B-T03 and A-T08 checks passed 266, 286 and 284 expectations respectively, all
with zero failures, test warnings or skips. The B-T03 stdout note
`Coefficients not estimable: between_group.unobserved` is not a test warning.

Sol independently reran the named public B-T07 regression from immutable commit
`2eb483a`; it passed 17 expectations with no failure, warning or skip. The
worktree remained clean and the repair changes exactly:

- `R/export_b.R`
- `tests/testthat/test-B-T07.R`
- `docs/implementation/registrations/B-T07.json`

Registration remains `candidate/review_pending`, `accepted: false`.

## Standards axis

Verdict: **pass**. Critical 0, Important 0, Minor 0.

The original finding is fully addressed. Resume inspection creates its schema
probe only inside a unique process-temporary directory, uses controlled
basenames, rejects artifact symlinks as before, and removes the probe directory
with `on.exit()`. It neither writes nor attempts to write inside the finalized
handoff tree.

The regression begins with a valid envelope, changes the still-readable JSON
payload bytes, makes the handoff directory read-only, exercises exported
`triomics_b07_export_handoff(..., resume = TRUE)`, observes the real public
decision as `hash_mismatch`, and proves that no probe targets the handoff tree.
It restores tracing, options, permissions and the original bytes on exit.

## Spec axis

Verdict: **pass**. Critical 0, Important 0, Minor 0.

The repair does not change the seven-rule refusal precedence.
`hash_mismatch` remains ahead of incomplete required evidence. Declared-partial
diagnostic publication and complete-claim enforcement remain unchanged. The
new regression exercises the exported resume path, and the repair stays within
the exact three-file allowlist.

## Disposition

M7-009's authorized contract definition, Astra audit, bounded B-T07 repair and
focused verification are complete. Candidate `2eb483a` is ready for a later
serial integration decision, but this cycle does not integrate or accept it.
C-06 provenance, migration, D verification/publication, D-T01 acceptance, M7,
G7 and every downstream Stage-D packet remain closed.
