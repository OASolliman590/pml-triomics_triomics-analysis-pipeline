# Three-project workbook audit for design revision

Inspected 2026-09-06, read-only, from the user-supplied folder E:/Triple_Layer_Omics_PML. This is structural evidence for a design review, not a biological analysis or validated import. Source workbooks remain unchanged and outside the pipeline workspace. No participant identifier, feature list, measurement row or individual result is reproduced here. Detailed sheet dimensions, header roles and source hashes are in three-project-structure.json. These study-specific audit records are local review material; do not publish them without a separate disclosure review.

Root integrity recheck: the local structural record contains nine workbooks and 49 sheet records; source SHA-256 comparisons remain unchanged. This aggregate count was computed from three-project-structure.json, which Opus did not independently inspect. The completed Opus reviews and root dispositions are in ../reviews/opus-three-study-design-review.md, opus-three-study-design-recheck.md and opus-review-response.md; the original proposals below remain historical review inputs.

## Observed project coverage

| Project | Observed workbook organization | Design implication / uncertainty |
|---|---|---|
| Egypt Organophosphate | Three assay workbooks. Lipid/metabolite workbooks have three contrast sheets; proteins have larger contrast tables plus up/down subsets and intersection lists. Comparison labels refer to control, chronic and acute groups. | Candidate independent multi-group design, not proven from sheet names. Repeated specimens, covariates, batch and contrast direction require metadata. Pairwise exports must not be stacked as independent studies or duplicate samples. |
| Egypt Diabetic | Three assay workbooks. Lipid/metabolite workbooks contain baseline-group and follow-up comparisons, plus All Lipids and All Metabolites sheets with two header rows. Protein workbook has visible significant-result sheets and a hidden Ctrl vs Met vs Met Novel matrix-like sheet. | Candidate group-by-visit design. Treatment-group contrasts, within-group change and differences in change are distinct estimands. Hidden sheet is an input candidate, not evidence of complete longitudinal protein coverage. Subject/visit/treatment metadata and complete tested universe remain necessary. |
| Fasting | Three assay workbooks with main feature-by-column tables; pre/post group-average headings; supplemental protein/metabolite sheets with other orientations and reduced feature inventories. | Candidate paired two-occasion design. Pre/post labels and matching sample-like codes do not prove pairing. A subject/sample map is required. Auxiliary transposed/subset sheets cannot be treated as independent assays. |

There are nine xlsx files and no separately supplied sample metadata, protocol, README, AGENTS.md or architecture file in this folder. Absence here is not proof such records do not exist elsewhere. These three projects are separate Studies, each containing multiple Blocks; they are not three Blocks of one Study. Generic production code must not embed these project names or sample counts.

## Concrete observations changing the proposed import contract

1. **Workbook and sheet are not the analysis unit.** Sheets mix quantities, chemical/feature descriptors, sample-like columns, group summaries, FC/log2FC, original and adjusted p-value columns, expression labels and curated lists. Explicit per-sheet roles and column ranges are required. A numeric column cannot be assumed to be a sample.
2. **Selection bias is an input property.** Visible diabetes protein sheets explicitly identify significant results, yet the workbook also contains a hidden 506-by-77 matrix-like sheet including its header. Inventory must include hidden sheets and report provenance. Do not declare the workbook summary-only from its filename, and do not declare the hidden sheet a complete original universe from its dimensions. Reanalysis eligibility needs a documented selection history and authoritative matrix selection.
3. **Header handling needs evidence.** Diabetes All Lipids is 453-by-176 and All Metabolites is 400-by-162; row 1 contains group bands and row 2 field/sample-like headings. Duplicate Class headings occur in lipid exports; repeated Name headings occur in the metabolite assembly. Preserve source positions and original text; do not let automatic name repair silently choose field meaning.
4. **Comparison exports may overlap.** Organophosphate pairwise lipid and metabolite sheets have repeated feature inventories with differing column sets. Protein sheets have both broad and selected tables. Shared sample/feature identities must be reconciled through an explicit map; consistency of repeated values has not been checked. Conflicting repeated measurements must block automatic coalescing.
5. **Identifier namespaces extend beyond the original five examples.** Several workbooks explicitly include PubChem CID/ID alongside lipid shorthand or metabolite names. Protein sources include accessions, descriptions and gene-name annotations. PubChem mapping must be proposed explicitly; retaining a supplied CID does not verify its chemical identity or justify upgrading lipid structural resolution. Multiple namespace aliases belong to one measured feature, not duplicate observations.
6. **Upstream processing remains unknown.** Main tables contain finite numeric measurements but also existing hypothesis-test outputs. Units, scaling, normalization, imputation masks and the exact statistical procedure are not established by headers such as p value (U) or p value (BH). Completeness of numeric cells is not evidence of complete original observations. No normalization or imputation is approved by this audit.

## Fasting alignment diagnostic

Main-sheet candidate measurement spans were inspected locally, without exporting their values:

| Workbook / sheet | Candidate region | Structural finding |
|---|---|---|
| Fasting Lipidomics.xlsx / Sheet1 | K2:BN717 | 716 feature rows, 56 candidate columns; inspected cells numeric and nonblank |
| Fasting Metabolomics.xlsx / Sheet1 | G2:BJ2002 | 2001 feature rows, 56 candidate columns; inspected cells numeric and nonblank |
| Fasting Proteomics-gene names.xlsx / Sheet1 | C2:BF464 | 463 feature rows, 56 candidate columns; inspected cells numeric and nonblank |

Exact three-way candidate-header intersection is 25; outer-whitespace trimming gives 55; removing all whitespace for diagnosis gives 56. No duplicate header is created within these candidate spans under the three compared rules. The metabolite headers include outer whitespace and one protein header includes internal whitespace. This establishes a plausible formatting mismatch, not a biological identity map or proof of 28 paired subjects. All-whitespace deletion is **not** an adopted normalization policy. Preserve original headers, propose a reversible crosswalk, test uniqueness and require the authoritative sample/subject map before fitting or centering.

The spans are candidates based on sheet structure and surrounding metadata/summary columns; acquisition units and scientific meaning remain unverified. No zero, distribution, missingness-mechanism, effect, p-value, pairing or recovery validation is claimed. Supplemental sheets remain unselected pending provenance.

## Proposed design changes for Opus to challenge

- Study → Analysis → Block → source-artifact registry; one project/run boundary per study, multiple explicit estimands, no automatic cross-study pooling.
- A workbook-import adapter selected by manifest: sheet, header rows, orientation, feature keys/aliases, numeric measurement columns, metadata/statistic columns, source-selection history and processing provenance. Import preview before analysis. Excel support needs an approved dependency or explicit external conversion boundary.
- Separate matrix, imported-result, selected-list, mapping and dictionary roles. Only eligible matrices enter A/B/C. Imported results may feed a separately declared interpretation/comparison path if approved, never substitute for independent measurement rows or masquerade as new inference.
- Group-by-visit design as a distinct B resolver capability to specify and validate, preserving all existing resolver designs. Fasting and organophosphate are additional acceptance scenarios, not hardcoded paths.
- C eligibility report per study/analysis/backend: matched independent units, relevant outcome and leakage-safe source universe. No significance-selected feature set for confirmatory prediction, no integration across independent study folders as though samples matched, and no reconstruction of sample covariance from FC/p tables.
- D supports namespace aliases and proposed PubChem routes with resource-specific rights/coverage, and reports source role, availability, original versus recomputed statistics and failed/unknown provenance.

All are proposed revisions, pending the user's confirmation after the requested Opus review. The protected BlockProfile and IntegrationResult signatures, separate-per-block FDR, distinct assay profiles, N-block capability and no-real-data-in-Git rule remain in force. Source-dependent biological design, pairing, normalization and missingness are unresolved. R confirmation is still required before pipeline code. No Opus review is claimed until a successfully authenticated run and returned report are verified.
