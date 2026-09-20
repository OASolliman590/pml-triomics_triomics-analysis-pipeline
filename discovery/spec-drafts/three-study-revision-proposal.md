# Revised pipeline design for confirmation

Shared design/statistical choices are now [settled through Opus–Sol/Astra](../operations/contract-settlement.md) under the user's delegation. This closes the bounded design frontier, not language confirmation, scientific validation, provisioning or original publication gates.

Clinical-association extension: **scope approved, 2026-09-06**; see [approval record](../operations/clinical-scope-approval.md). The earlier general architecture confirmation status below is separate.

Date: 2026-09-06. Status: **revised after Claude Opus 5 review; owner confirmation pending**. All four actual A–D kits now contain dated source/design amendments. See the [Opus report](../reviews/opus-three-study-design-review.md), [response](../reviews/opus-review-response.md), and [shared source contract](../operations/source-contract-proposal.md). This is an architecture proposal, not completed implementation or analysis.

## Proposed design

One generic pipeline processes each project as a separate Study, with authoritative sample/subject metadata, N assay Blocks and named Analyses for explicit contrasts or prediction targets. Worksheet regions are source artifacts, not automatically Blocks or Analyses. Cross-study codes never join participants or pool statistics.

The full path remains validated measurements → A profiles → B statistics and C integration using the same approved representation per Analysis → both D annotation routes → report. All A–D scope, three integration backends, N-block behavior, per-Block FDR and protected interfaces remain.

| Kit | Concrete revision |
|---|---|
| [A: ingest/profiles](A-ingest-profiles.md) | Validate workbook/sheet/region manifests and audited conversion before TSV/CSV ingest; declare headers, visibility, column roles, selection, aliases, sample maps and overlaps. |
| [B: design/statistics](B-design-statistics.md) | Explicit group-by-visit estimands, projection non-estimability and incomplete-visit coverage; gate q/significance on justified families and selection evidence. |
| [C: integration](C-integration.md) | Enforce per-Study/Analysis/backend source, identity, target, selection-leakage and deployment eligibility. |
| [D: annotation/report](D-annotation-report.md) | Preserve aliases without extra observations; separate imported/recomputed evidence; require valid enrichment universes and expose coverage/unavailable states. |

## What the three projects establish

The local audit inspected nine workbooks and 49 sheets read-only; root verified that count from the local [structural record](../research/three-project-structure.json). Opus did not independently recheck that record. These provide candidate design scenarios, not validated scientific inputs or test constants.

| Project | Observed structure and candidate design | Required before analysis |
|---|---|---|
| Egypt Organophosphate | Mixed contrast tables may contain usable measurement columns alongside supplied statistics; curated/subset sheets also occur. Candidate three-group design. | Authoritative sample/group/repeat metadata, column roles, selection history, overlapping-export concordance and measurement provenance. Neither a validated usable matrix nor summary-only status is established. |
| Egypt Diabetic | Multi-header All-Lipids/All-Metabolites sheets and a hidden matrix-like protein sheet. Candidate group-by-visit design. | Subject/visit/group map, selection/coverage, estimands/covariates, missing-visit policy and approved common representation/model. Hidden does not mean complete original universe. |
| Fasting | Main candidate measurement spans plus separate averages/statistics; formatting differences across Block headers. Candidate paired design. | Authoritative identity/pairing/occasion map, approved crosswalk, processing provenance and supplement roles. Matching column counts or normalized codes cannot establish pairs. |

No study measurements or identifiers were sent to Opus. It reviewed sanitized structural observations and design/evidence documents. Source workbooks remain unchanged. The [local audit](../research/three-project-design-audit.md) contains study-specific metadata that is not automatically publishable.

## Five proposed decisions

1. **Source boundary:** mandatory manifest and verified reproducible external conversion to canonical TSV/CSV first. A native XLSX reader remains a separately researched/approved option. Conversion requires tool/version, hashes, maps and extraction checks.
2. **Source roles and selection:** mandatory distinct measurement/result/list/mapping/dictionary/unresolved roles. Claim eligibility is bound to a hashed authorized scientific decision and its exact source/family/evaluation scope, not a self-certified flag. Unknown provenance restricts claims; justified panels/filters are not banned merely for being subsets.
3. **Group-by-visit coverage:** preserve within-group change, differences in change and requested between-group contrasts as distinct targets. Reject removed signals. Any alternative retaining subject means remains an explicit protected representation/model decision; no silent target removal or B/C matrix divergence.
4. **Incomplete visits and external evidence:** default missing-pair error, with explicitly approved complete-pairs-per-contrast and per-group attrition reporting. Initially use imported results for display/concordance only; no replacement reanalysis or new candidate branch.
5. **Feature identity:** stable measured keys plus typed aliases, including proposed PubChem CID registration; preserve adduct/protein-group rows and uncertainty. Resource/API activation remains separately evidence-gated.

Approval accepts the architecture direction and spec amendments. It does not choose a repeated model, change protected score rows, establish metadata, install dependencies or make the kits implementation-ready. R still needs explicit confirmation. Dependency, historical handover, repository/Notion/host and final readiness requirements remain.

## Subsequent target-investigation proposal

The user requested a general capability to investigate arbitrary targets. The [new D extension](D-target-investigation-extension.md) covers assess, compare and exploratory discover modes, with target/context identity, evidence for and against mechanisms, alternatives and explicit separation from physical-binding evidence. Its [separate Opus review and root response](../reviews/opus-target-extension-response.md) are now recorded. It is additive, unimplemented and not adopted; it was not part of the preceding three-study checkpoint. The original five decisions remain visible; this addition retains its own scope/contract/resource gates.

## Subsequent clinical metadata association proposal

The user requested association of patient clinical metadata with molecular findings. The [shared clinical extension](clinical-molecular-association-extension.md) adds typed variables, patient/visit linkage, granular objective cards, between/within/baseline-to-later associations, integration-score associations and separately evaluated prediction. The [local audit](../research/clinical-metadata-design-audit.md) found five additional Fasting clinical workbooks; its original/hidden-sheet reinspection has not located validated Organophosphate/Diabetic clinical tables beyond group/visit labels. Source mappings, codebooks and units remain explicit prerequisites. This addition is outside prior review checkpoints and requires its own coordinated review before adoption; no actual patient association has been computed.

## Validation status

The shared contract adds eleven synthetic acceptance scenarios to existing tickets: source roles, headers/hidden sheets, authorized maps/collisions, selection validity, aliases, group-by-visit estimability, Study isolation, partial coverage, assembly conflicts, external-only reporting and evaluation leakage. These are planned tests, not executed results. Scientific/API/runtime checks follow the existing gates.
