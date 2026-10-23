# M7-002 Astra-low amendment audit brief

## Role and boundary

You are a fresh read-only Astra-low reviewer. Review the immutable commit named
in the dispatch message. Do not edit files, accept the amendment, release M7,
dispatch implementation, or read private study data.

## Required reading

1. `docs/implementation/change-records/M7-002-stage-handoff-provenance-amendment.md`
2. `docs/implementation/reviews/2026-10-23-m7-001-focused-sol-rereview.md`
3. `docs/implementation/investigations/2026-10-23-stage-handoff-scientific-provenance-research.md`
4. `docs/implementation/investigations/2026-10-23-canonical-digests-safe-publication-research.md`
5. `GLOSSARY.md`
6. `docs/adr/0003-stage-handoff-provenance-and-generation-publication.md`
7. `discovery/spec-v2/DATA_CONTRACTS.md`
8. `discovery/spec-v2/D_ANNOTATION_TARGET_REPORT.md`
9. `discovery/spec-v2/VALIDATION.md`
10. B-T07, C-06, and D-T01 entries/cards in `discovery/spec-v2`.

## Audit questions

- Does the amendment close every reproduced M7-001 defect: FeatureRegistry
  drift, paired/repeated metadata drift, DEqMS identity disagreement, recursive
  `model_hash` collision, and registry mutation before dangling-symlink failure?
- Is scientific identity producer-owned and exhaustive without D refitting or
  reconstructing B/C logic?
- Are the legacy proof, deterministic migration, and regeneration rules
  fail-closed and scientifically honest?
- Are canonicalization, digest kinds, schemas, arrays, and member roles precise
  enough to implement and test without two plausible incompatible readings?
- Does the publication state machine actually preserve the prior current
  generation under every promised process failure, while avoiding a false
  hostile-race or crash-durability claim?
- Are reopened seams, attempt budgets, ownership, and acceptance gates bounded
  without erasing M7-001 history or creating a 42nd packet?
- Does any clause alter statistical methods, thresholds, seeds, estimands,
  families, biological claims, or private-data policy?

## Output

Return `pass` or `changes_required`, with Critical, Important, and Minor findings
and exact file/section references. State whether implementation may proceed under
the proposed route. Keep the receipt concise; write no repository files.
