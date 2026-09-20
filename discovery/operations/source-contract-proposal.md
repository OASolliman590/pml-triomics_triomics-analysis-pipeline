# Source and study contract — three-study revision

## Settlement update — 2026-09-06

[The adopted contract settlement](contract-settlement.md) and [Astra arbitration](../reviews/astra-design-arbitration.md) resolve the shared design/statistical alternatives below. DEC-02–12 and CL-ADR01–04 have bounded design dispositions; older pending labels are historical. Language confirmation, real-study/user parameters, rights, empirical tests and provisioning remain separate gates.

Date: 2026-09-06. Status: **proposed for owner confirmation**, following the [Opus review](../reviews/opus-three-study-design-review.md) and [root response](../reviews/opus-review-response.md). This is the single shared schema proposal for the new source boundary. It supplements the existing shared contract under X-R01 and A-D01/D06; it does not change BlockProfile or IntegrationResult signatures. The dated amendments in A–D refer here rather than defining competing source schemas.

## Identity and cardinality

Require `study_id` in configuration and every artifact envelope, alongside `run_id`, `schema_version` and, for analysis artifacts, `analysis_id`. Existing local block/sample/feature keys resolve within that Study. Cross-study code matches never create a join. `analyses[]` declares independently named targets within one Study. A batch contains separate Study runs; cross-study synthesis remains a separate design decision.

`sources[]` contains N source artifacts; `blocks[]` references one or more measurement sources by `source_refs[]`. N artifacts need not equal the number of Blocks. A may validate one Block; multi-block assembly/integration retains the existing minimum of two and backend-specific eligibility. There is no fixed upper block count or three-assay schema. Each source belongs to exactly one Study. Canonical `blocks[].file`/`.format` still identifies a feature-by-sample TSV/CSV consumed by the existing A parser.

## SourceArtifact fields

| Field | Required meaning |
|---|---|
| `artifact_id`, `study_id`, `source_ref`, `source_sha256` | Stable manifest identity, local original location and SHA-256. These local references/hashes are not automatically publishable. |
| `format`, `sheet_id`, `sheet_visibility`, `header_rows`, `data_range`, `orientation` | Actual source format; exact sheet/visibility for workbooks, explicit not-applicable for text; header row positions with roles; selected range; `features_by_samples`, `samples_by_features`, or `not_applicable` for nonmeasurement artifacts. No inferred orientation or default first sheet. |
| `source_role` | `measurement_matrix`, `imported_result`, `selected_list`, `mapping_table`, `dictionary`, `clinical_metadata`, or `unresolved`. The clinical role is a new proposed extension with its own review; roles describe regions, not entire filenames. |
| `columns[]` | Explicit source position and original heading, unique canonical field name, declared type and role: `feature_id`, `source_row_id`, `alias`, `measurement`, `feature_metadata`, `upstream_statistic`, `qc`, or `excluded`. Exclusion requires a reason. All selected-region columns must be accounted for. |
| `sample_map_ref` | Hashed approved mapping from artifact/column position/original heading to canonical `sample_id`. For transposed sources, the corresponding explicit source row locator is required. |
| `source_selection` | Structured record below; absence is an error, an explicit unknown is retained. |
| `processing_provenance` | Supplied scale/units, prior transformations/normalization/imputation and observation-mask availability, with explicit unknowns. No inference from finite cells. |
| `conversion` | Mode `identity` or `external`; converter identity/version, complete parameters, source and output digests, output orientation/field map, reproducible instruction and verification record. Identity is only valid for an unchanged canonical text source. |
| `assembly` | Required if multiple sources contribute to one Block: exact joins, source precedence forbidden for conflicting measurements, declared absolute/relative numeric tolerance and overlap verification evidence. |

A mixed worksheet is represented by multiple typed region artifacts sharing the original file hash. A measurement region can include explicitly excluded statistic/QC fields in its column accounting; separate imported evidence artifacts reference their own columns. Summary-only FC/p/q regions and selected lists cannot become assays even when rectangular and numeric. Declaring a role is necessary but is not proof that columns are biological observations: sample membership must also be validated against authoritative metadata. Workbook group bands are proposed metadata only until confirmed.

Original duplicate headings are allowed only when each chosen field is disambiguated by position plus original heading and assigned an explicit unique canonical field name. Duplicate canonical names or unresolved positional selection fail. No reader auto-suffix is a scientific mapping. Group averages, group totals and numeric QC metrics never become sample observations.

Formula cells require formula/cache provenance and a declared cache-validity verification policy; unknown/stale caches are ineligible as measurements. The conversion must not execute macros, refresh links or invent results by recalculation. Re-deriving a conversion verifies faithful extraction, not the source's scientific validity. For external conversion, verification records input/output dimensions, field/ID maps, value equality under a declared tolerance, missing tokens and exclusion accounting. A manifest with a missing check is unverified, not self-certifying. A validates that evidence and exact hashes at ingest. Dependency/API review of any eventual converter remains required; this draft installs none.

For multi-artifact assembly, equal repeated cells may be included once with all contributing provenance. Unequal cells raise `A_SOURCE_CONFLICT`. Disjoint complements require an explicit join and coverage policy. Pairwise result exports cannot be stacked as new independent samples; differing column subsets neither establish nor disprove valid recoverable measurements.

### Clinical source extension — proposed

The [clinical association contract](../spec-drafts/clinical-molecular-association-extension.md) defines variable dictionaries, subject/visit/clinical observations and explicit observation-to-sample links for `clinical_metadata`. Such regions are never eligible omics assay Blocks. Their column roles additionally distinguish clinical identity, visit/time, observed clinical value, coded questionnaire, derived index, reference range, collection/administrative flag, summary and excluded fields; typed dictionary references settle names, units, codes and missingness. These are coordinated schema additions, not an already accepted parser behavior. Existing `metadata.file` canonical views must derive from the approved source/dictionary lineage rather than a second manually maintained truth. The original source-role rules below continue to exclude clinical tables from assays and external q from B.

## Source selection and three different universes

`source_selection` requires `history` (`none_declared`, `independent_filter`, `outcome_selected`, `curated_subset`, `unknown`), selection rule/statistic/threshold when applicable, selecting population and data reference, selection time, source-universe reference/digest when available, `source_universe_complete` (`true`, `false`, `unknown`), and supporting evidence. `none_declared` is a provider assertion until verified. Keep separate:

1. The source feature inventory and original observation-mask provenance.
2. B's declared eligible hypothesis family (`family_id`, member digest/count, filtering rationale, `selection_validity`, `selection_evidence_ref`).
3. D's route-specific eligible enrichment background/rank universe and mapping coverage.

Completeness of an exported worksheet proves none of these by itself. Conversely, a valid prespecified targeted panel or justified filter need not contain every feature ever measured. `selection_validity` is `approved_for_claim`, `unverified`, or `ineligible` and requires a reviewed rationale, not a boolean inferred from row count. Unknown or outcome-dependent preselection does not qualify for ordinary BH/FDR claims unless an independently reviewed selection-aware analysis establishes validity. This proposal approves no such new method. B withholds q and significance nominations with a recorded reason when validity is unresolved; any displayed effect/SE/p must separately satisfy its method/selection validity and be labelled conditional/descriptive where appropriate. Withholding q does not repair selection bias in effect estimates.

Per-feature prevalence uses eligible sample observations as its denominator. Feature selection changes the population of features described; it does not turn that denominator into a feature count. Unknown original missingness prevents an original-observation prevalence claim even when the supplied matrix is fully numeric.

C separately assesses feature selection relative to its evaluation population. An externally fixed panel can be eligible if its construction is independent of the evaluation observations and the information available at deployment is declared. Unknown provenance or selection using held-out outcomes cannot support valid performance claims. Exploration can remain eligible and labelled. Learned preprocessing and selection must be fitted within the appropriate training partitions. D requires a justified background for each enrichment method; a selected candidate list is never its own background by default.

Evidence qualification: independent filtering can preserve valid testing under specific filter/test conditions; it is not a blanket exemption ([Bourgon et al., 2010](https://pmc.ncbi.nlm.nih.gov/articles/PMC2906865/), abstract and methodological conditions). Selection used to train a predictor must be included inside its validation procedure ([Ambroise and McLachlan, 2002](https://pmc.ncbi.nlm.nih.gov/articles/PMC124442/), cross-validation discussion). These primary sources support the distinctions, not a validation of these studies or implementation defaults.

### Binding reviewed selection decisions

`selection_validity` defaults to `unverified`. A claim may use `approved_for_claim` only through a `SelectionDecision` record referenced by `selection_decision_ref` and `selection_decision_sha256`. Require decision ID/version, owner or designated scientific reviewer identity, approval date, rationale/method evidence, exact Study/artifact IDs and source hashes, known source/eligible-family inventory digests, selection rule and selecting-population digest, claim/analysis scope, and evaluation population/split-plan identity where relevant. Unknown original inventories remain explicitly unknown; an approved independent targeted panel uses its exact panel digest and justified claim scope rather than inventing a complete source inventory.

B/C/D verify the approval record and bindings, not a config-author assertion or the presence of a file. Require an accepted reviewer/decision registry and evidence of authorization; an arbitrary approver string is insufficient. Reuse is permitted for an unchanged authorized scope without repeated approval. Any source, panel, family, selection rule, claim or evaluation-context change invalidates that eligibility decision until reviewed for the changed scope. A generated fold may reuse a decision covering that verified fold-generation procedure; it does not require a new human approval per fold. Missing/mismatched evidence yields `B_SELECTION_UNVERIFIED` or the corresponding C/D ineligible state; missing declaration yields `A_SELECTION_UNDECLARED`. Source-history assertions remain provider provenance and cannot self-promote to approval.

## Sample identity and aliases

Strict original-header matching remains the default. Alignment diagnostics may propose candidate maps and report collisions but cannot apply them. An owner-approved map can be applied mechanically and reused by its hash; no repeated approval is needed for the same authorized mapping. Preserve original values and canonical values so the mapping history is recoverable. Similar strings do not establish specimen identity, subject identity, visit or pairing. Those relationships require authoritative metadata.

The approved map record binds its digest and approval evidence to `study_id`, `artifact_id`, `source_sha256`, selected region/header-role definition and authoritative sample-metadata digest. A checks those bindings and complete declared measurement-column coverage before application; stale-but-matching headers still fail with `A_CONVERSION_UNVERIFIED` reason `stale_sample_map`. A changed source or metadata requires review of the changed scope, not silent reuse. Nonmeasurement columns are explicitly accounted for outside the map.

`FeatureAliases` is one-to-many: `study_id`, `feature_key`, `alias_namespace`, `alias_value`, `alias_source` (`as_supplied` or `derived_by_route`), `asserted_by`, `verification_status` (`unverified`, `verified`, `conflicting`), `evidence_ref`. Verification defaults to unverified and needs provenance. Aliases are neither new measured rows nor primary keys. Register `pubchem_cid` as a proposed namespace; a supplied identifier does not enable an API route or prove chemical identity.

Separate measurement granularity (`measurement_level`, e.g. ion/adduct/compound/protein_group/unknown) from identification specificity (`identification_resolution`, namespace-appropriate vocabulary including unknown). Neither upgrades the other. Source-row discrimination preserves distinct measured adducts/protein groups. D mapping ambiguity is distinct from an explicitly approved aggregation relationship; shared aliases alone authorize neither collapsing observations nor summing them.

## Shared status and external evidence

Every result index entry records `study_id`, analysis scope, source references and statistic origin: `recomputed_in_run` or `imported_unverified`. External supplied evidence uses `stage=external_upstream`; the origin stays external even if its metadata is later reconciled. Preserve provider method strings, units, contrast and universe declarations including unknowns. Never infer a test from an abbreviation, recompute BH on imported p values, or inject external q into B's families.

The proposed initial external path is display and explicitly defined concordance only. It creates no B/C CandidateNominations, integration fit, enrichment test or completed reanalysis. CandidateNominations keeps branches B/C, both feeding both approved namespace routes. External-list annotation beyond display remains a separate D-ADR01 scope/contract decision, with no branch extension adopted here.

Reports carry per-Study/Analysis/Block coverage and denominators, eligibility with reasons and the existing separate input/analysis/rendering states. A structurally valid external-only bundle can render completely while requested B/C analyses remain not_started and ineligible. It cannot satisfy the full-pipeline milestone.

## Shared synthetic acceptance obligations — planned, not run

| ID | Fixture and acceptance |
|---|---|
| TS-01 | Arbitrary independent 2/3/5-block studies, with extra dictionaries and multiple source regions: N artifacts are not N Blocks; rename display labels without changing processing. One-Block A ingest remains valid independently of integration eligibility. |
| TS-02 | Two header rows, a hidden candidate table, duplicate original headings and numeric averages/QC: explicit field mapping works; unresolved headings, undeclared columns and summary-as-sample claims fail. Hidden visibility implies neither inclusion nor exclusion. |
| TS-03 | Permuted headers with outer/internal whitespace plus a collision case: strict mode diagnoses only; authorized collision-free map restores keyed values; collision is rejected; no pairing inferred. |
| TS-04 | Matrix, imported statistics and selected list together: only validated measurement region enters assays; adding supplied BH columns cannot change B family/q or C input hash for measurements. D origins remain separate. |
| TS-05 | Same eligible values with known versus unknown selection: numeric extraction unchanged; claim eligibility and q availability change. Include a justified fixed panel/independent filter and an outcome-selected panel to prevent a blanket all-subsets ban. |
| TS-06 | Aliases plus distinct adduct rows: stable measured row count/keys, no automatic coalescing, no structural-resolution promotion and separate ambiguity versus aggregation records. |
| TS-07 | Synthetic group-by-two-visit design with incomplete visits: hand-check retained-subject deltas and group difference in deltas under approved assumptions; reject a projected-out main-effect request; per-group attrition counts correct. Approval/model/covariance checks remain prerequisites, not test assertions. |
| TS-08 | Same sample text in two Studies: all cross-study joins and a mixed-study integration request fail. |
| TS-09 | Partial block coverage and multi-artifact overlaps: strict/explicit union follow approved policies; whole-assay gaps are not feature imputation; equal overlaps appear once, unequal overlaps fail; no silently preferred cell. |
| TS-10 | External-only report, missing universe, stale conversion output and unsupported converter: honest unavailable states, no negative biological conclusion, hash/provenance refusal and no fake completed analysis. |
| TS-11 | Nested evaluation with an externally fixed feature panel versus held-out-outcome-selected input: panel provenance and fold isolation decide eligibility, not mere inventory size. No successful performance object for the leaking input. |

Extend TS-03 with a stale source/metadata hash despite identical headers: reject the old map. Extend TS-05/11 with missing/forged approval evidence, changed panel/family/source/claim and changed evaluation scope: no eligible q/performance claim. An unchanged authorized scope reuses approval; a covered fold-generation procedure does not prompt once per fold. Extend TS-07/10 so D explicitly states the declared/changed analysis population and applied incomplete-visit policy, not counts alone.

All fixture labels, sizes and values must be invented independently; no real sample/feature identifiers, workbook cells or study-specific numerical constants belong in Git or delegate prompts. These are acceptance specifications, not implemented or executed tests.
