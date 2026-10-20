# C-05 repair-1 focused Sol-high rereview

Date: 2026-10-20. Candidate:
`9eb420702c854b7e7e6de7a8303d21b9a6393c8b`. Base: `8dcd78d`.
Reviewer: `/root/c05_repair1_sol_rereview`, requested model
`gpt-6.1-sol`, effort high. The review was read-only and did not rerun fits or
inspect private data.

Verdict: **changes required**. Critical 0, Important 1, Minor 0.

The repair closes the native percentage conversion defect, declares variance
semantics, covers the four frozen missingness cells, implements descriptive
independent-seed/sign/permutation/subspace sensitivity, and produces
version-bound native-model and transformed-data hashes. Unequal retained factor
dimensions retain explicit matched and unmatched identities. The public route
remains disabled and no predictive or biological claim is made.

The remaining Important finding is confined to the annotation boundary.
`triomics_mofa_enrichment_input_sidecar()` accepts and hashes any nonempty R
object, including a scalar string or a mapping to a feature absent from the
fitted model, then marks the handoff ready. This is not the `MappingEdge`
contract in `discovery/spec-v2/DATA_CONTRACTS.md`: measured feature identity,
namespaced molecular entity, relation/resolution, taxon/context, resource
release/evidence, and ambiguity group must be validated. Malformed or
mismatched input requires a typed refusal; absent mappings must keep the
explicit not-supplied disposition. No enrichment execution is requested.

The 14 native MOFA diagnostics from the tiny synthetic fixtures are disposed,
not suppressed: five low-feature-count power warnings, four
factor/average-expression cautions, three single-factor correlation-QC
`max(..., na.rm=TRUE)` warnings, and two all-low-variance factor-retention
warnings. They do not expose a demonstrated adapter correctness defect. The
run remains synthetic software evidence, not scientific validation.

Repair 2 is the final permitted packet repair. After it, Sol must inspect and
verify directly; the single focused rereview allowance is consumed.
