# A — ingest, configuration and block profiles

## Adopted design settlement — 2026-09-06

The user delegated unresolved choices to Opus/Sol and Astra. The [settlement](../operations/contract-settlement.md), [Astra arbitration](../reviews/astra-design-arbitration.md), [command contract](../operations/implementation-command-contract.md) and [individual tickets](../tickets/README.md) now supersede conflicting pending alternatives in the older draft below. Design is settled within these bounds; runtime, scientific validation and original G0 are still open; R core plus Python is explicitly approved. A owns the common schema module through A-T01/02 and the concrete SE metadata fit/apply protocol through A-T07. A-T09 clinical ingest now precedes A-T08 complete handoff. No protected profile or simulator signature changes. A-T01 also owns tools/check-package.R and the single stage-aware CLI.


**Status: independently reviewed provisional draft; not implementation-ready.** Date: 2026-09-05. The [draft review](../reviews/A-spec-review.md) and [revision response](../reviews/A-spec-review-response.md) do not constitute final readiness approval. Read the [shared contract proposal](../operations/shared-contract-proposal.md) alongside local field proposals. Language confirmation, historical handover reconciliation, dependency approval, several scientific/contract decisions and executable verification are outstanding. No production code, package installation or R execution is represented by this file. It contains reviewable schema/ticket proposals, not approved changes to the owner contracts.

## 0. Three-study revision — 2026-09-06, proposed for confirmation

The [Opus review](../reviews/opus-three-study-design-review.md) is incorporated with the [root response](../reviews/opus-review-response.md). The [source contract](../operations/source-contract-proposal.md) defines the single shared schema for these additions under A-D01/D06 and X-R01. Existing protected interfaces remain unchanged; historical reviews apply to earlier snapshots.

A owns original-source manifest and conversion-evidence validation before canonical ingest. Recommended first boundary: externally converted TSV/CSV plus mandatory workbook/sheet/region provenance. An in-process XLSX reader remains an optional dependency decision. Validate sheet selection/visibility, header roles, orientation, formula/cache status, measurement/QC/statistic columns, selection history, exact hashes and reproducible extraction checks. Multiple source header rows are compatible with the single-header canonical output rule. Evidence of faithful conversion is distinct from evidence of scientific validity.

Require `study_id`, `analyses[]`, `sources[]`, `blocks[].source_refs[]`, and the exact column/sample maps specified in the source contract. All local keys are Study-scoped. Only eligible measurement regions become SE assays; numeric rectangularity is insufficient. A can validate one Block independently; assembled multi-block/integration paths retain a minimum of two plus backend constraints. N source artifacts need not equal M Blocks, and neither count is fixed to three.

Add the proposed FeatureAliases sidecar and `pubchem_cid` namespace registration. Preserve stable measured-feature keys and source-row discrimination through all downstream joins. Aliases never create/merge observations or upgrade identification specificity. Duplicate original headings require explicit position/name mapping to unique canonical fields; no reader auto-suffix is accepted as a semantic decision. Numeric group averages, QC and supplied statistics must be assigned nonmeasurement roles.

Strict sample alignment stays default. Diagnostic maps are inert until authorized; an approved map can be applied and reused by its hash, preserving original/canonical values. Sample-to-subject/group/visit relationships require authoritative metadata. Code similarity, group bands and equal counts prove no identity or pairing. As-supplied inventories and original measured universes are distinct. Per-feature prevalence uses eligible samples as its denominator, with original-observation mask knownness reported. Whole-assay gaps are availability gaps, not feature imputation.

| Added failure | Proposed error and response |
|---|---|
| Missing manifest or selection declaration | `A_ARTIFACT_MANIFEST_MISSING` / `A_SELECTION_UNDECLARED`; explicit unknown selection is allowed but gates downstream claims. |
| Missing header roles or unresolved canonical duplicate headings | `A_HEADER_ROW_UNDECLARED` / `A_COLUMN_NAME_DUPLICATE`; show original positions, never auto-suffix. |
| Unassigned numeric column or invalid observation membership | `A_UNDECLARED_NUMERIC_COLUMN` / existing `A_SAMPLE_UNMAPPED`; require roles and authoritative mapping. |
| Summary/list/unresolved region requested as assay | `A_SOURCE_ROLE_INELIGIBLE`; fail before SE construction. |
| Hash mismatch, unverifiable conversion, stale/unknown formula cache | `A_CONVERSION_UNVERIFIED`; name failed extraction check. |
| Overlap disagreement in declared multi-artifact assembly | `A_SOURCE_CONFLICT`; report join/cells/tolerance, never average or prefer first. |
| Mixed Study references | `A_CROSS_STUDY_SOURCE`; fail before assembly. |

A-D09 also owns converter/reader dependency approval. A-T01 validates manifest configuration shape. A-T02 (milestone 2) gains manifest → conversion verification → canonical ingest → source/coverage diagnostics within its existing vertical deliverable. A-T06 preserves measured/derived identity through aliases; A-T07 preserves selection evidence and immutable pre-fit values for fold preparation. These are amendments to existing tickets, not a new component or early report implementation.

Add source-contract fixtures TS-01–06 and TS-08–10 to A-T02/06/07 as applicable: explicit hidden-sheet handling, multiple headers, authorized mapping/collision refusal, numeric-summary refusal, extraction re-derivation and overlap conflicts. Existing profile, N-block and leakage tests remain. All new tests are planned, not run; no dependency or production code has been added.

## 0.1 Clinical metadata association extension — proposed

The [shared clinical extension](clinical-molecular-association-extension.md) adds first-class variable dictionaries, subject/visit registries, typed observations and hashed clinical-to-sample links, with a proposed `clinical_metadata` source role. A validates meaning, units/codes, measured/derived/reference/administrative roles, source versions, populated coverage and visit/cardinality conflicts. Clinical fields do not become omics assays by default. A-T09 extends ingest/preparation at milestones 2–3; no new protected profile signature or dependency is adopted. Source-specific observations/objectives are in the local clinical metadata audit; missing definitions and placeholder-only columns remain unavailable.

## 1. Problem statement, scope and claim limits

A Study can arrive as heterogeneous source artifacts. A validates their manifest and audited conversion before accepting N quantified feature-by-sample Blocks and Sample metadata at its canonical boundary. Each Block has measurement properties that affect filtering, transformation, normalization, missingness handling and interpretation. A must make these properties explicit, preserve sample and feature identity, and deliver traceable matrices to B. It must not assume that a familiar Block name determines assay technology, abundance scale, missingness mechanism or annotation coverage.

A owns configuration parsing/validation, matrix and metadata ingest, one validated SummarizedExperiment (SE) per Block, Study assembly using MultiAssayExperiment (MAE), per-Block preprocessing and the synthetic Study generator. B owns Design resolution and Decomposition. C owns integration and predictive validation, with an A/C seam needed for preprocessing within resampling folds. D owns external identifier resolution, enrichment and reporting; A supplies provenance/QC/feature lineage to it. A does not compute q-values or choose statistical contrasts.

The three built-in profiles remain proteomics, lipidomics and metabolomics. A fourth profile must be possible without altering stage orchestration or dispatching on display names. Separate policies are retained; reusable low-level identity/provenance checks do not imply one shared scientific preprocessing policy.

Out of scope: raw files, peak detection or identification, new imputation/statistical methods, unapproved dependencies, annotation-service uploads, prototype repair, study-specific constants and real study data in version control. Mechanism assessment may require upstream metadata; A cannot reconstruct an acquisition process from a matrix alone.

**Closest precedents:** lipidr provides lipid parsing, preprocessing and enrichment; MultiOmicsIntegrator documents lipidr-enabled class interpretation, and OmicsAnalyst documents lipid data analysis. See `../research/D-resources-precedents.md` and `../reviews/D-evidence-review.md`, P-Q02/P-Q04 and D-R01/D-R02. Candidate engineering contribution is explicit identity, measurement/provenance and Design-independent stage contracts with reproducible decisions. This is not evidence that no existing tool combines three omics types or that triomics improves inference.

**Proceed/narrow/defer criteria:** proceed with approved, independently reviewed ingest and profile work only after the global all-four-specifications gate. Unsupported normalization/imputation combinations must remain visibly blocked. A narrow milestone may implement a reviewed subset while retaining other promised profiles as incomplete; it must not count A complete. Deferring a profile or contract requirement requires an explicit maintainer disposition. No method is accepted solely because it recovers a planted factor once.

**Architecture crosswalk:** A maps to execution prompt A and architecture-context §§5 and 10, implementation milestones 1–3. Older numbering/history is unknown. Shared terminology comes from `../../CONTEXT.md`; this draft adds operational definitions below, all proposed rather than silently canonicalized.

## 2. Solution and user stories

The proposed user experience is a CLI which validates an explicit configuration before processing, reports file/key-specific failures, and produces an inspectable A-stage result plus a provenance record. Scientific policies without defensible defaults remain explicit decisions printed in the eventual report.

1. As an analyst, I want an invalid configuration rejected before matrix mutation, so that an omitted policy cannot silently choose my estimand.
2. As an analyst, I want feature/sample identities preserved exactly, so that punctuation, leading zeros and duplicate molecular labels cannot change an alignment.
3. As an analyst, I want sample-to-assay maps checked by identity, so that reordered columns produce the same Study.
4. As an analyst, I want repeated observations retained as separate Samples, so that B can resolve the declared Design.
5. As an analyst, I want independent data accepted without a subject identifier, so that the first consumer's paired Design does not become a pipeline assumption.
6. As an analyst, I want quantified intensity distinguished from concentration, composition and already-transformed input, so that values are not logged twice or mislabeled mol%.
7. As an analyst, I want each Block's upstream processing recorded, so that already-imputed values are not presented as observed data.
8. As an analyst, I want separate profile policies, so that one assay's missingness assumption cannot silently govern another.
9. As an analyst, I want missingness filters applied before imputation with loss reasons, so that reconstructed entries cannot establish prevalence.
10. As an analyst, I want lipid class totals and composition endpoints distinguished, so that a total increase is not erased without disclosure.
11. As an analyst, I want unresolved lipid names preserved with explicit annotation gaps, so that downstream absence of annotation is not mistaken for absent biology.
12. As an analyst, I want deterministic preprocessing replay with seeds and parameters, so that a restart does not silently create different completed data.
13. As a methods reviewer, I want training-only fitted preprocessing during validation, so that held-out Samples cannot influence feature eligibility or learned references.
14. As a maintainer, I want a fourth Block registered through contracts, so that adding an assay does not require editing name-specific branches.
15. As a reviewer, I want synthetic truth separated from measurement evidence, so that a software smoke test is not claimed as assay validation.
16. As an analyst, I want all scientific changes, including no-op/skipped stages, recorded, so that B/C results can be traced to the same authorized source matrices.
17. As a maintainer, I want interrupted artifacts distinguished from validated artifacts, so that resumption does not consume partial output.
18. As a data custodian, I want all study-derived matrices and identifying logs kept out of the repository, so that documentation does not become a covert data export.

## 3. Owner interfaces and proposed input/output schemas

### 3.1 Preserved BlockProfile contract

The supplied interface is retained verbatim in meaning: a named list with `validate`, `filter`, `transform`, `normalise`, `impute`, and optional `derive`, each with `(se, params)` and returning an SE or stopping with a clear error. No additional function argument, native package object or alternative return type is adopted here. It is an interface declaration, not executable code in this draft.

Proposed invariants for every operation:

- Preserve original IDs and Sample relationships. Filtering records each removed Feature; derivation records every parent Feature. No other stage may drop/reorder by unrecorded heuristics.
- SE validity and assay/rowData/colData correspondence hold after every operation. All assays inside a given SE share its dimensions and dimnames. Matrices before a shape-changing stage are retained as separate checkpoint artifacts, not malformed extra assays.
- A profile may explicitly keep missing values when its selected policy allows them; neither finite output nor complete imputation is universally presumed. Every adapter-specific requirement is checked at dispatch.
- Repeating a non-idempotent stage is rejected using its declared input/output scale and completed-stage record, unless a reviewed reprocessing path explicitly restores the earlier checkpoint.
- Profile methods read only their input object, resolved params and approved fitted-state mechanism; they do not reach into B/C/D internals or global analysis variables.

### 3.2 Proposed configuration schema (pending A-D01)

Existing keys `blocks[].name`, `file`, `id_namespace`, `normalisation`, `annotation_route` and `design.{subject_id,condition,condition_type,covariates,repeated}` retain their meaning. Additional keys below are proposals necessary to make processing reviewable. Unknown keys fail validation rather than being ignored. No arbitrary R expression/function name is evaluated from YAML.

| Key | Proposed type and constraint | Default/decision status |
|---|---|---|
| `schema_version` | Required scalar version string identifying an approved schema | Proposed value `1`; no current accepted schema |
| `metadata.file` | Required local TSV/CSV path resolved relative to configuration location | No discovery by filename |
| `metadata.sample_id` | Required existing column; nonempty unique text values | No guessed first column |
| `metadata.column_types` | Map of used metadata columns to `string`, `factor`, `numeric`, or `logical`; factor levels explicit when order matters | Proposed strict typed metadata; B validates model semantics |
| `blocks` | Ordered list, at least two for assembled multi-block Study; profile testing may operate on one SE | No fixed maximum in schema; supported size constrained by declared resources |
| `blocks[].name` | Unique nonempty display/container label | Never dispatch scientific behavior on its value |
| `blocks[].file` | Required canonical quantified matrix path linked to validated `source_refs[]` | Section 0 source/conversion evidence required; no raw instrument parser |
| `blocks[].format` | `tsv` or `csv` | Explicit when extension ambiguous; supported parsing details fixed below |
| `blocks[].feature_id_column` | Required column name containing original Feature IDs | IDs parsed as text |
| `blocks[].id_namespace` | Supplied `uniprot`, `gene_symbol`, `hmdb`, `kegg_compound`, `lipid_shorthand`; proposed addition `pubchem_cid` | Section 0 registry/alias proposal needs approval; registration is not external mapping or route entitlement |
| `blocks[].annotation_route` | Registered route key compatible with namespace | `reaction_network` already supplied; exact lipid route key/allowed combinations require D approval |
| `blocks[].feature_metadata` | Optional local table keyed to exact Feature row identity | Required when the chosen method requires standards/counts/calibration/structural properties |
| `blocks[].sample_map` | Optional local table of `assay_column`, `sample_id` | Absent means exact header-to-sample_id matching; not positional matching |
| `blocks[].measurement.quantity` | `intensity`, `concentration`, `fraction`, or `molar_fraction` | Required; values are proposed enumeration |
| `blocks[].measurement.scale` | `linear`, `log2`, `ln`, `clr_ln`, `clr_log2`, or `other_declared` | Required; `other_declared` may ingest but cannot preprocess until supported |
| `blocks[].measurement.units` | Nonempty unit string, including documented arbitrary intensity units | No inference of molarity from positive values |
| `blocks[].measurement.acquisition` | Structured provenance: platform, targeted/profiling, quantification level, upstream software/version | Unknowns represented explicitly; missing required evidence blocks incompatible policy |
| `blocks[].measurement.upstream_steps` | Ordered records of operations already applied, parameters/software, and original missingness availability | Required declaration, including explicit none/unknown |
| `blocks[].measurement.denominator` | Fixed composition scope/member list or sample-amount normalization description | Required for fractions; the part set is not inferred from current filtered matrix |
| `blocks[].missing_tokens` | Explicit list of input missing tokens, e.g. `NA` and empty cell | Zero is a value unless separately declared; arbitrary text never coerced silently |
| `blocks[].missingness.assumption` | `mnar_left_censored`, `mar`, `mcar`, `mixed`, or `unknown`; rationale and source | No assay-name auto-diagnosis; pending settled-policy qualification A-D02 |
| `blocks[].filter.prevalence` | Numeric in [0,1] | Supplied 0.70 retained as proposed operating point, not evidence-backed universal default |
| `blocks[].filter.scope` | `global` or named training-metadata strata; explicit eligibility/count denominator | Choice unresolved; no automatic outcome stratification |
| `blocks[].filter.min_observed` | Nonnegative integer requirement in addition to proportion | No universal value selected; may be method-dependent |
| `blocks[].filter.qc` | Explicit blank/QC/sample-amount thresholds and columns when used | No silent sample exclusion; unsupported rule rejected |
| `blocks[].transform.method` | Registered transform compatible with declared quantity/scale | Supplied log2 for built-ins; explicit compositional alternative only |
| `blocks[].transform.reference_set` | Ordered/hashed fixed Feature set and `block` or `class` scope for CLR | Required for CLR; base always explicit |
| `blocks[].transform.zero_policy` | Registered positive replacement policy with units and parameter, or `error` | Proposed `error` until scientifically justified; no automatic pseudocount 1 |
| `blocks[].normalisation` | Existing scalar registered method key | Existing spelling preserved; params separated below |
| `blocks[].normalisation_params` | Method-specific mapping: reference, standards map, input scale, sample amount and missing-standard handling | All chosen scientific params printed; unresolved operation errors |
| `blocks[].imputation.method` | Registered method key or `none` | Existing profile options retained; dependencies/assumptions govern availability |
| `blocks[].imputation.params` | Method-specific params and required RNG seed | No treating MinProb and QRILC return values as interchangeable |
| `blocks[].derive` | Explicit list of endpoint definitions, weighting quantity, fixed eligible parent universe, missing-parent policy, coverage rule and output scale | Lipid endpoints retained as required capability; representation and aggregation choice pending A-D04; no implicit NA-as-zero |
| `sample_alignment.policy` | `strict`, `intersection`, or `union` | Proposed `strict`; intersection/union are explicit choices awaiting A/B/C agreement |
| `resampling` | Reference to owner-approved C fold plan; no fold generation in A | A receives partition identity; split semantics remain C/B-owned |
| `execution.seed` | Required integer when stochastic stages run; per-stage derivation declared | No arbitrary seed selected in this document |
| `execution.output_dir` | Explicit writable destination, governed by data classification | Real data/derived outputs prohibited from version control |
| `execution.memory_limit_mb` | Positive resource budget for preflight; conversion/method allocations checked | No unbenchmarked claim of safe universal memory setting |

A accepts and preserves `design` but does not infer a two-level model, center subjects or convert continuous predictors. A validates referenced columns exist and scalar/list types are well formed; B validates estimability, factor contrasts, duplicate visits and covariate semantics. A cannot claim a Study is statistically valid merely because it ingests.

**Parsing rules proposed:** UTF-8 text; a single header row; explicit separator and decimal point; no thousands-separator guessing; no whitespace/case/punctuation modification of IDs; any requested canonicalization creates an auditable original-to-canonical map first and rechecks collisions. Numeric assay cells must be finite numeric values or declared missing tokens. ±Inf and NaN are distinct invalid entries at ingest, unless an approved importer explicitly records their conversion to missing. Row/sample counts must be positive. Extra annotation columns are allowed only when explicitly excluded from matrix columns or supplied in feature metadata. Input lists and duplicate YAML keys need parser behavior verification; duplicate keys must not resolve by last-write-wins silently.

### 3.3 Sample and Feature identity

Canonical Study colData has one row per **Sample**, keyed by `metadata.sample_id`; repeated Samples can share a Subject column. It must not use Subject as the primary row key, which would collapse visits. Each Block's mapped assay columns correspond one-to-one to Samples under the initial proposal. Technical replicates mapping to the same Sample require a separately approved aggregation policy and remain blocked; they are not automatically averaged.

MAE `sampleMap` is constructed explicitly: `assay` is the Block container key, `primary` is canonical Sample ID, and `colname` is the assay column identifier. Its three-column relationship is documented in the [MAE Quick Start Guide, sampleMap and constructor sections](https://bioconductor.org/packages/release/bioc/vignettes/MultiAssayExperiment/inst/doc/QuickStartMultiAssay.html), dated 2026-04-28. That documentation also demonstrates automatic harmonization/drop behavior. **Proposed invariant:** validate and compare maps before/after construction and refuse unexpected dropped entries; do not rely on a constructor to repair scientific input. Exact installed constructor signatures/behavior remain unverified.

Strict alignment requires all declared analysis Samples in every Block. Intersection reports exact removed Sample identities/reasons and rechecks Design eligibility in B. Union retains assay-specific measured subsets, plus an explicit Block availability map; missing entire assays must not be imputed as ordinary single-feature gaps. No automatically padded NA matrix is claimed equivalent to MAE's absent-assay-column representation. C must specify its extraction contract. Blank/QC Samples are retained or excluded only by explicit roles, with analysis eligibility distinct from original assay availability.

Each Feature is keyed by the tuple `(originating Block identity, identifier namespace, original Feature ID, source row discriminator when explicitly supplied)`. Exact duplicate IDs within a Block fail by default; distinct features sharing a molecular ID need a separate explicit unique row key and provenance. Identical strings in different namespaces or Blocks are not merged. Internal row keys must use an injective encoding or tuple table, not naïve delimiter concatenation. A Block rename updates its display/container mapping without altering numeric preprocessing behavior. Namespace/route rules may reject incompatible metadata but cannot equate ID namespace with measurement provenance.

**Coordinated identity approval gate (A-SR01):** this tuple is an A proposal, not a frozen shared contract. A/B/C/D must approve one Feature registry that distinguishes unique measured/derived Feature identity from the original molecular ID and preserves any discriminator through SE rows, B tables, C selection/loading outputs and D nominations. Registry keys, field names, encoding and round-trip fixtures remain unresolved under A-D01/D04/D06. Repeated molecular identifiers must neither collapse nor lose their unique key in a downstream join; no local A wording change approves a competing schema.

### 3.4 Proposed SE/MAE output schema

For each validated/processed SE:

| Location | Required proposed contents/invariants |
|---|---|
| Assay `analysis` | Numeric feature-by-Sample values at explicitly declared output scale; NA allowed only under declared policy; dimnames match rowData/colData |
| Assay `observed_mask` | Logical dimensions matching `analysis`; true only for originally observed values retained through stage lineage |
| Assay `imputed_mask` | Logical dimensions matching `analysis`; disjoint from originally observed entries unless an explicitly audited replacement operation exists |
| rowData | Internal row key, original ID, namespace, originating Block, measured/derived kind, units/scale where heterogeneous, source row, parse status, optional class/chain/counts metadata |
| colData | Canonical Sample ID, original assay column, sample role, declared metadata joined by identity; no silently dropped covariates |
| metadata | Resolved policy/version, ordered completed operations, input/output scales, fitted-state reference if approved, missingness declaration, input/output/checkpoint hashes, software/RNG, loss and lineage tables |

The `analysis`/mask names and field set are proposed additions, not present in the original owner contract. Immutable source matrices before filtering are separate artifacts with all original dimensions. Upstream-imputed exports with unknown original masks must represent mask provenance as unknown, rather than setting every finite value as observed; this requires a tri-state provenance convention or separate knownness mask (A-D05).

MAE stores the named SE collection, canonical Sample colData, explicit sampleMap, Block availability and Study-level provenance. A hands B the processed MAE and evidence of the source matrix chosen for each Block. B creates Analysis-unit identity and decomposition lineage; A does not relabel subject differences as Samples. The common-matrix requirement for B/C is retained; any secondary sensitivity representation is labeled with a distinct analysis ID and cannot silently replace the primary source.

**Derived Features unresolved representation:** SE assays cannot have different row counts. Proposed option A is to append derived rows to the processed SE with `feature_kind`, parent lineage, endpoint units and internal namespace; original source rows are recoverable from checkpoint plus lineage. Option B is a separately described derived-endpoint collection tied to the parent Block and multiplicity family. Option B changes assembly/dispatch semantics; option A needs derived-namespace and heterogeneous-scale agreement with B/C/D. Neither is adopted. No dummy NA source rows or molecule IDs are invented to evade this decision.

**Derived observation-completeness contract proposed under A-D04 (A-SR03):** define and hash one eligible parent universe per endpoint and analysis before Sample-level aggregation. Eligibility rules must name standards, duplicate adduct handling, structural unknowns and any training-only selection for predictive use. A Sample's missing parent does not silently shrink that universe. Changing the universe creates a separately identified endpoint/analysis, with its changed estimand explicit.

For a parent with missing abundance, the owner must approve an endpoint-specific policy: (a) report an explicitly labeled observed partial sum, (b) estimate the defined eligible sum after an approved imputation policy, or (c) return a missing/unestimable endpoint with a reason. These are alternatives, not selected defaults; no policy implies replacing NA with zero. A partial sum cannot be labeled a complete class total, and imputation does not turn a parent into an observed measurement. Coverage thresholds and treatment of unknown upstream masks are also unresolved choices. An estimated full eligible sum is not a claim to measure all biological class members, including those outside the assay.

Each endpoint-by-Sample record must carry the parent-universe reference, aggregation-policy/version, expected/observed/imputed/unavailable/unknown-provenance parent counts, completeness status and excluded-parent reasons. Count coverage uses the defined eligible-parent denominator; count coverage is not molar coverage. For additive sums, retain separate observed and imputed contributions with units where those contributions are known. For weighted chain/unsaturation endpoints, retain numerator/denominator definitions and their contribution provenance instead of assuming an additive endpoint decomposition. Original source masks stay parent-level: the derived output needs this completeness record and must not be marked simply observed because its aggregate is finite. B's effect labels and D's report must carry the endpoint type (partial observed sum, estimated eligible sum, or unavailable), universe and missing-parent policy; exact shared keys need approval.

### 3.5 Failure contract

Errors have a stable proposed code, stage, configuration key or file, reason and corrective requirement; sensitive values are minimized in publishable logs. Full sample-level diagnostics remain only in the protected run directory. Nonzero exit means the A stage did not complete; no successful completion marker is written.

| Error code | Concrete trigger | Required error context |
|---|---|---|
| `A_CONFIG_UNKNOWN_KEY` | Misspelled `blocks[2].normalization` | Exact key and accepted `normalisation` spelling |
| `A_INPUT_NONNUMERIC` | Assay cell contains undeclared text | Block file, row/column location, missing-token policy |
| `A_ID_DUPLICATE` | Duplicate Feature key or Sample ID | Identity domain and relevant file; no automatic suffixing |
| `A_SAMPLE_UNMAPPED` | Assay header absent from map/metadata | Block file, `sample_map` or `metadata.sample_id` |
| `A_SAMPLE_MULTIMAP` | Multiple assay columns map to one Sample without replicate policy | Map rows and required decision |
| `A_ALIGNMENT_LOSS` | Constructor or intersection drops an unapproved Sample | `sample_alignment.policy`, recorded losses |
| `A_SCALE_CONFLICT` | Input `log2` plus requested log2 again; mol% requested from uncalibrated intensity | Quantity/scale and operation keys |
| `A_MISSING_POLICY` | Unknown mechanism paired with required left-censored-only assumption | Missingness/imputation keys and absent rationale |
| `A_PROFILE_UNSUPPORTED` | Namespace/route/operation combination not registered | Actual keys and missing capability |
| `A_REFERENCE_INVALID` | CLR reference empty, lacks positive parts, or changes across compared samples | Reference set/zero policy |
| `A_STANDARD_UNAVAILABLE` | Required lipid-class internal standard absent | Standard-mapping policy; no hidden fallback |
| `A_NO_ELIGIBLE_FEATURES` | Filtering leaves empty Block | Counts, filter params, downstream incomplete status |
| `A_IMPUTE_FAILED` | All-missing eligible Feature, insufficient neighbors or package return mismatch | Method, constraints and actual stage |
| `A_DERIVE_UNRESOLVED` | Endpoint lacks weighting, fixed parent universe, missing-parent/coverage policy, lineage or approved representation | `derive` item and A-D04; a configured missing/unestimable endpoint is a recorded endpoint status, not this configuration error |
| `A_RESOURCE_LIMIT` | Estimated allocations exceed declared budget | Dimensions, estimated lower bound, operation |
| `A_CHECKPOINT_MISMATCH` | Resume inputs/config/software hashes differ | Changed provenance fields; no cache reuse |

An unexpected external-library error is wrapped with stage/method context and retained cause; it is never converted into an empty successful result. No statistical non-significance is an A failure condition.

## 4. Profile policies, stage ordering and cross-validation seam

### 4.1 All three profiles remain in scope

| Built-in profile label | Preserved requested options | Evidence-backed qualification / unresolved decision |
|---|---|---|
| Proteomics | Median scaling or VSN; log2 where applicable; MinProb/QRILC left-censoring options; 70% configurable prevalence; no requested derived endpoints | A-Q01/A-Q03: mixtures occur; platform and upstream aggregation matter. No universal MNAR diagnosis. VSN is a joint normalization/transformation whose output scale and sequencing need adapter verification; do not blindly log twice. Peptide/PSM counts preserved for B without inventing them. |
| Lipidomics | Within-class policy, explicit log2/compositional option, class-aware missingness/imputation, class sums/chain/unsaturation endpoints | A-Q04–08: define class-aware operation; class standards, sample-amount normalization, closure and variance scaling differ. Class-aware imputation is not established by lipidr wrapper documentation. Derived formula/scale/representation unresolved. |
| Metabolomics | PQN, log2 where applicable, kNN or RF, configurable 70% prevalence; no requested derived endpoints | A-Q02: targeted LC/MS can be censored; RF/QRILC evidence is mechanism-dependent, not a universal winner or MinProb comparison. PQN reference definition and raw-versus-log application need explicit semantics and fitted reference. |

Registry proposal: namespace/annotation-route capabilities resolve the appropriate profile family and allowed policies; explicit measurement and method parameters govern operations. Registry rules never use `blocks[].name`. Distinct acquisitions sharing a namespace may need different params; a namespace does not determine a unique valid scientific policy. Whether adding an explicit profile/capability key is permitted remains A-D06, because the supplied three-declaration rule does not fully specify ambiguous resolution.

Fourth-Block conformance example: a fourth quantified assay with an already supported namespace/route and different display name can reuse a compatible profile with explicit measurement/processing params. A genuinely new namespace/chemistry requires a registered validator/profile and D route disposition, accepted through the same six functions and result invariants; no fork of the Study loop. Both scenarios must be tested. Unsupported chemistry is rejected clearly, not processed as metabolomics by default. Two Blocks with the same namespace remain distinct.

### 4.2 Ordering proposal and scientific guardrails

The function-list order is not assumed to be universally executable order. Proposed planner derives a validated operation sequence from the selected profile, input scale and method preconditions, then records it before execution. This is A-D07 because sequencing/derived endpoints need owner review.

Minimum invariant: input validation and original missingness/QC assessment precede prevalence filtering; prevalence is based on observed availability, never on completed data. Normalization, transformation and imputation order is explicit per method. Missing values may be retained during a supported reference fit; a method requiring complete input must not fill them implicitly. Class concentration sums are derived from authorized linear abundance before closure/log/centering; this is an endpoint definition, not permission for uncontrolled branch divergence.

That ordering does not select a missing-parent rule. The §3.4 fixed universe, completeness ledger and approved partial/imputed/unestimable policy apply before a class sum or weighted endpoint is declared available. The planner must expose where any approved imputation contributes to that endpoint; it cannot use a changing observed subset as a complete class total.

Lipid scientific choices: class-total closure erases uniform class increases; it remains a legitimate **declared compositional endpoint**, not a universal concentration correction. CLR uses a fixed set of positive reference parts, selected log base and explicit zero policy. Natural-log CLR documentation does not mean base-2 output. Derived weighted chain/unsaturation measures require molecular calibration/weight semantics and cannot infer individual chains from a sum composition. Unknown structural entries are excluded only under explicit coverage policy with counts.

lipidr reuse remains conditional: unpinned inspected source replaces nonfinite entries with the whole-assay `min(...,na.rm=TRUE)`, not necessarily a finite minimum, and clamps subunit values only when requesting log conversion on an assay not flagged logged. Missing matching standards use divisor 1, while docs promise all-standard fallback. Absence of any standards raises an error. These are static concerns awaiting release pin/execution, not a claim about installed 2.26.0. A wrapper may not rely on a normalization call to carry out unaudited imputation. See A-REV03.

### 4.3 Proposed high-level fit/apply test seam (A-D08)

The highest useful public seam is “prepare this Study under this resolved policy and partition,” returning the traceable processed Study. Existing six profile methods remain the inner owner interface. Most behavioral tests should use this Study-level seam, with a few package-adapter numerical edge probes where necessary.

Cross-validation cannot call a full-data preprocessing routine before generating folds. Proposed fit/apply semantics: train-time state contains eligible Feature keys, learned scaling/reference quantities, training-only neighbor/model state if supported, fixed CLR part set, params and training Sample fingerprint. Apply-time processing accepts new Samples without their outcomes and does not refit from held-out observations. In a repeated Design, partition membership is subject-grouped by C; A verifies the supplied separation, it does not generate folds.

The current `(se,params)->se` contract has no explicit returned fitted-state object. Options for approval: (1) a specified `params` execution mode plus typed state in SE metadata, with an outer orchestrator serializing that state; or (2) an explicit fitted-profile interface amendment. Neither is presumed sufficient before audit. kNN/RF and some distribution-based imputation wrappers may not offer inductive apply operations; if so, a predictive path must remain unsupported or use an explicitly approved alternative. Using test features to refit an unsupervised method is still a different transductive analysis and must not be silently scored as held-out prediction.

Training-unseen missingness and absent reference parts need declared failure/degraded behavior. Any filter using condition strata may use only training conditions; held-out outcomes cannot choose processing. An independent null synthetic validation must check that changing held-out values/outcomes cannot change training-fitted parameters or training outputs.

## 5. Evidence and dependency-by-question manifest

No dependency is approved or installed by this draft. The following facts are documentation/static-inspection facts. The detailed DOI/section ledger, uncertainty and failed access attempts are in `../research/A-preprocessing.md`; qualifications are in `../reviews/A-evidence-review.md`. These files are part of the eventual evidence package, not replaced by this shorter matrix.

| Question / decision coverage | Source/dependency and exact anchor | Known version/terms | Verification needed before acceptance |
|---|---|---|---|
| A-Q01 / A-D02 mechanism | Lazar 2016, DOI 10.1021/acs.jproteome.5b00981, §5 Conclusions | Version of record; article evidence | Acquisition-specific assessment, selected method behavior and measurement benchmark |
| A-Q02,Q09 / A-D02 filter/method | Wei 2018, DOI 10.1038/s41598-017-19120-0, Discussion/Fig.4/Methods | R 3.4.2 in study; benchmark-specific | Does not settle triomics threshold or MinProb-versus-QRILC winner |
| A-Q03 uncertainty | Chion 2022, DOI 10.1371/journal.pcbi.1010420, Introduction | Primary methods paper | Imputation sensitivity and interval/FDR validation in B; no automatic multiple-imputation adoption |
| A-Q04,Q05 / A-D03 class policy | Burla 2018, DOI 10.1194/jlr.S087163, Quantification step iii; LSI quantification headings | Plasma/serum scope; LSI page date unknown | Requested endpoint and calibration provenance; preserve legitimate composition option |
| A-Q06 / A-D03 transform | compositions reference, CLR Details | 2.0-9 full reference; GPL >=2, reference only | No new dependency needed merely to cite math; chosen implementation/signature/base unapproved |
| A-Q07 / A-D09 imputation wrappers | imputeLCMD MinProb/QRILC official Usage/Value | 2.1 documented; license/current source hash unverified | Orientation, NA constraints, all-missing/small-n, return shapes and RNG; full license |
| A-Q07,Q08 / A-D03,D04,D09 lipidr | `normalize_istd`, `normalize_pqn`, `impute_na`, `annotate_lipids`, `gen_lipidsets`; upstream normalize.R | Release page 2.26.0, BioC3.23, MIT + LICENSE; website 2.15.1; source master unpinned | Exact released function formals/source hash; license file; implicit mutation and parser probes |
| A-Q10 / A-D01,D05 container shape | [SummarizedExperiment release page](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html), Description/Details | Inspected 2026-09-05: 1.42.0, BioC3.23, Artistic-2.0; matrix-like assays | Constructor/accessor formals, dimname/rowData preservation and validity checks in actual approved environment |
| A-Q11 / A-D01,D05 sample map | [MultiAssayExperiment release page](https://bioconductor.org/packages/release/bioc/html/MultiAssayExperiment.html), Details; Quick Start sampleMap/constructor | Inspected 2026-09-05: 1.38.0, BioC3.23, Artistic-2.0; Depends R>=4.5.0 | Installed constructor/accessor formals, harmonization/drop/union semantics and serialization round trip |
| A-Q12 / A-D09 parsing/CLI | yaml/optparse, named in supplied technology choice | Exact versions, formals and licences not verified | Duplicate keys, coercion, path resolution, CLI exit behavior; approve dependency set |
| A-Q13 / A-D09 transforms | Median scaling/VSN/PQN/kNN/RF selected implementations | No selected VSN/RF package/version/signature; PQN candidate lipidr noted | Method definitions, fit/apply ability, missing requirements, licenses and concrete APIs |
| A-Q14 / A-D08 leakage seam | A/C contract inference and adversarial test design | No package establishes this pipeline seam | Cross-component ADR plus held-out independence probes |
| A-Q15 reproducibility | renv/testthat and R, requested technology | Exact versions, lock and execution host unknown | Restore approved lock, report session/platform/RNG, execute substantive checks |

A-Q10–15 are new draft engineering questions: proposed answers are the schema/seam choices above, classified **design choices**, supported by the cited container docs where factual. Their runtime behavior remains unknown. All exact package constructor formal defaults are to be captured with `formals()`/help/source on the selected version; quoted examples in a vignette are not a complete API audit. Quantification-profile outcome types are not a package outcome classifier: A must accept factor/continuous/repeated metadata without selecting B's model.

## 6. Validation strategy and two hand-worked Designs

### 6.1 Ground truth and fixture construction

Synthetic generator remains a first-class exported capability. Preserve supplied parameters: `n_subjects`, named `n_features`, `design`, `effect_size`, `prop_affected`, `missingness`, `shared_factor_strength`. Proposed additions such as sample/visit schedules, Block profiles, noise parameters, seed and output schema require A-D10. Existing sample `prot/lip/met` names are illustrative defaults, never dispatch conditions. B/C determine which generated factors are identifiable under each Design; effect size and noise must be separate controls.

**Complete resolver fixture coverage (A-SR04):** retain the supplied `design = c("paired", "two_group", "continuous", "timeseries")` choices. Do not silently widen that public enum. The proposed coverage below uses direct generator modes for those four choices and explicit, versioned synthetic fixture construction for the two additional resolver rows. A-D10 may instead approve a generalized level/visit schema or additional modes; until then those remain proposals and all six coverage obligations remain. Generated data support an eligibility or documented unsupported-path check when B's method is unresolved; fixture availability does not validate that model.

| B resolver row | Proposed construction without adopting an enum change | Required fixture oracle and A/B handoff |
|---|---|---|
| Repeated, two factor levels | Direct `paired` mode | Explicit two Samples per Subject and level order; truth for subject intercepts and paired effects; missing pair variant with exact loss/eligibility expectation |
| Repeated, >2 factor levels | Explicit synthetic fixture with at least three declared factor levels and Subject/Sample map; optional extended generator schedule awaits A-D10 | Known subject and level terms; balanced and unbalanced/missing-visit fixtures; A preserves factor levels and repeated IDs, B checks approved route or precise unsupported status |
| Independent, two groups | Direct `two_group` mode | Explicit two-level contrast order, independent Sample IDs and no required Subject column; known group means and null alternative |
| Independent, >2 groups | Explicit synthetic fixture with at least three groups; generalized group-count generator option awaits A-D10 | Known group terms and declared scalar contrasts; separate omnibus eligibility case; no invented pairing or factor-to-number coercion |
| Continuous outcome/predictor | Direct `continuous` mode | Numeric predictor/units, known slope and independent Samples; a repeated-continuous negative fixture must not route as independent merely because values are numeric |
| Time series / trajectory | Direct `timeseries` mode; exact time/basis/covariance fixture parameters await A-D10 and B model decision | Explicit actual times/units and Subject map, independently documented generating mean/covariance; repeated and irregular/missing-time cases with rank/eligibility expectations, not a claim that centering validates spline covariance |

For every row, version the fixture definition, seed if stochastic, input hashes, Sample/Subject mapping, distinct truth artifact and the expected valid-or-explicitly-unsupported outcome. Each row also needs a malformed configuration or mapping variant that names the failing field. A-T02 owns generating/ingesting all six fixture families; B's corresponding tickets own resolver/model validation. All fixture-definition and extended schema choices remain subject to approval, and the two hand-worked examples below are illustrations rather than substitutes for this coverage matrix.

Generator output proposal: input-compatible Blocks and metadata, plus a separate truth record describing complete pre-missing values, mechanism, affected Features, Design coefficients, factor scores/loadings, seed and formula version. Truth stays separate from fitting inputs to prevent leakage. Every fixture is visibly `synthetic`; no public or private measured dataset is copied into fixtures. Null effects and weak/unidentifiable factors are valid generated outcomes. No absolute “failed factor recovery proves a bug” assertion is retained.

Ground truth for identity checks is the explicitly constructed permutation/map, for arithmetic checks it is small analytically computed values, and for stochastic checks it is the generator mechanism over repeated predeclared seeds. A truth table can verify what was generated; it cannot guarantee recovery, correct standard errors or biological relevance.

Measurement validation remains separate: an approved confidential or appropriately licensed benchmark with assay provenance, standards or known mixtures is needed to assess normalization/missingness assumptions. It is not committed. Benchmark protocol must define units, missingness provenance, perturbation truth, metrics, uncertainty and where outputs can be stored. Source-data absence blocks that validation rather than changing it into a synthetic pass.

### 6.2 Worked paired Design (illustration, not executable result)

Metadata has Samples `s01a,s01b,s02a,s02b`; Subjects `u01,u01,u02,u02`; factor visits `a,b,a,b`. Those labels are arbitrary example text, never package constants. One Block file lists columns `s02b,s01a,s02a,s01b` with Feature P values `32,8,16,16`. Exact identity mapping restores `(8,16,16,32)`. Under an explicitly selected log2 transformation and already-completed upstream normalization, A outputs `(3,4,4,5)` with all values observed. A does not calculate the paired differences. B would receive both visit Samples and could, if its approved resolver chooses differences b−a, obtain `(1,1)` on Subject Analysis units.

A lipid class has two species `PC(32:0),PC(34:1)` with subject-1 visit abundances `(10,30)` and `(20,60)` in calibrated common molar units. Proposed concentration totals are `40,80`; within-class molar fractions are `(0.25,0.75)` at both visits. The former shows a doubling, the latter stable composition. That is an estimand distinction, not a normalization success/failure test. The selected representation/derived storage is blocked on A-D03/D04; A must not pick whichever produces significance. Subject 2 could have no change, demonstrating that per-subject consistency belongs in B's result and is not an A filter.

If Feature Q is observed in only two of four Samples, a global 70% rule drops it because `2/4 < 0.70`; it cannot reappear through imputation. The lost-row record cites Q and the rule. A group-wise rule might differ and therefore requires explicit scope. In subject-grouped validation, all visits of u02 are held out; no u02 values may define u01's normalization reference or feature filter. Two Subjects are far too few here for a useful prediction study: this is an identity/scale example only.

### 6.3 Worked independent continuous Design (illustration, not executable result)

Samples `r1,r2,r3` have numeric predictor values `0,1,2`, with no Subject column. A second Block file lists `r3,r1,r2`; assay values of a Feature are `16,4,8`. The validated canonical ordering is `(4,8,16)` and explicit log2 output is `(2,3,4)`. A preserves the numeric predictor and does not manufacture factor groups. B could later fit its approved continuous model; A reports no p-value, q-value or inference from three points.

Assume an approved illustrative training-reference scaling operation learns `g=2` from training Samples r1/r2 and applies division by g on linear scale. Their processed values are `(2,4)` and held-out r3 is `8`. Changing held-out r3 to `1600` changes only its applied value to `800`; g and training output stay unchanged. These arithmetic choices illustrate the fit/apply contract, not a definition of PQN, VSN or the selected production method. A method that refits a whole-matrix reference on all three fails this validation contract even if it ignores the outcome.

If a fourth Block has only `r1,r2`, strict alignment produces `A_ALIGNMENT_LOSS`/missing-Sample diagnostic. Explicit union could preserve that Block availability for C if its approved adapter accepts it. A must not invent an r3 measurement or silently remove r3 from other Blocks. In all cases, no subject_id requirement appears merely because the first example was paired.

### 6.4 Substantive test plan

| Risk | Proposed external-behavior check and oracle | Gate |
|---|---|---|
| Cross-Block sample mismatch | Permute each Block independently; resulting keyed values and metadata agree with hand truth; ambiguous map rejected | A ingest |
| Namespace/key collision | Same string in two namespaces/Blocks stays separate; delimiter-like IDs cannot collide; duplicate within-Block keys fail | A ingest/D handoff |
| Silent Design coercion | Independent continuous metadata preserves numeric type and absent Subject; factor level order remains explicit | A/B handoff |
| Filtering after imputation | Sparse observed Feature stays in loss record even when an imputer could fill it; compare eligibility to original mask | A profile |
| Outcome leakage | Perturb held-out values/labels; fitted training state and outputs unchanged; repeat within subject-group partitions | A/C seam |
| Sign/estimand reversal | Known positive linear abundance change remains positive after authorized monotone transform; class closure example reported as composition; CLR reference-change effects labeled | A/B interpretation |
| Implicit package mutation | NA/Inf/subunit/already-logged/standards fixture checks exact allowed changes and failure conditions against release source | Dependency adapter |
| Derived loss/double count | Parent list excludes standards/duplicate adduct entries as declared; class totals equal hand sums; undefined denominator fails | A derive/B family |
| Missing-parent estimand drift | Fixed parents P/Q with `(10,30)` versus `(10,NA)`: complete first sum 40; second is partial 10 only under labeled partial policy, an estimated sum only with approved reconstructed Q, or unavailable under that policy. Assert universe unchanged, no NA-zero substitution, correct observed/imputed contribution/coverage, and B endpoint labels. Include unknown upstream mask and all-parent-missing cases | A derive/B/D handoff; A-D04 decision required |
| Incomplete Design coverage | All six §6.1 resolver-row fixture families and their malformed variants ingest with the declared types/maps or fail specifically; B validates model or explicit unsupported path separately | A-T02/B contract gate |
| Seed dependence | Same seed/state replays; distinct seeds retain declared stochastic variability; report sensitivity rather than enforce identical different-seed results | A impute |
| Hidden Feature loss | Stage event ledger accounts for all input keys; unmatched annotation not removed silently | A/D |
| Pooled multiplicity | A carries originating Block/family provenance through derived endpoints; B's separate-block correction checked in cross-component gate | B-owned, A support |
| Partial restart | Interrupted stage has no completion marker; unchanged approved checkpoint resumes; changed source/config/lock invalidates | A execution |
| N-Block extensibility | Same contract checks for 2,3,4,5 Blocks and renamed labels; a registered additional chemistry needs no stage-loop edits | A extension |

Tests target outputs/failures/provenance, not exact internal function call counts. No repository prior-art tests or audited reusable prototype were found in this bounded draft context. Test tolerances for numerical packages require a declared scientific/numerical justification; there is no arbitrary benchmark acceptance threshold here.

## 7. Decision log, glossary and citation packaging

Every row is unresolved unless explicitly identified as a supplied constraint. These are draft decision IDs, not committed ADRs or maintainer approvals.

| Decision | Proposed choice to review | Classification/status and material alternative |
|---|---|---|
| A-D00 technology/history | R package with requested tooling; reconcile older handover before coding | Supplied default, maintainer confirmation missing; no Python migration proposed |
| A-D01 schema/sample alignment | Typed config, explicit Sample map, strict alignment by default | Design choice; union/intersection need B/C coordination; constructor semantics unrun |
| A-D02 missingness/filter | Retain distinct profiles with evidence-qualified mechanism declaration and explicit method/filter scope | Challenges settled wording; approval needed; do not silently substitute algorithms |
| A-D03 lipid normalization/composition | Distinguish standards/sample amount/closure/scaling; preserve concentration and declared composition estimands | Evidence-backed challenge to unconditional class-total scaling; no blanket prohibition of composition |
| A-D04 derived representation | Choose appended typed rows or separate linked collection; define weighting/formulas/multiplicity, fixed parent universe and explicit missing-parent/completeness policy | Contract/namespace/estimand issue; owner ADR needed; partial versus imputed versus unavailable alternatives not selected |
| A-D05 provenance shape | Named masks + unknownness, immutable source checkpoint, stage lineage | Schema choice; upstream original missingness may be irrecoverably unknown |
| A-D06 profile extension | Namespace/route capability registry with explicit method/measurement params | Existing no-name-dispatch preserved; optional profile key would require approved amendment |
| A-D07 operation planning | Per-method input/output scale and prerequisite sequence | No one universal order; preserve filter-before-impute; VSN/class sums require inspection |
| A-D08 fold-local state | Add approved fitted-state semantics around unchanged functions or amend interface explicitly | Cross-component seam/ticket approval required; some methods may lack inductive support |
| A-D09 dependencies | Select complete compatible R/BioC/package set and adapter APIs | No new package approved; source/licenses/runtime checks pending |
| A-D10 synthetic generator | Separate fit inputs and planted truth; four supplied modes plus explicit fixtures for the other two resolver rows; consider generalized group/visit schema | All six resolver rows retained; exact fixture schema and any extended signature/result schema need approval |
| A-D11 publication | Reviewed milestones only to verified private repository; synchronize exact Notion record | Supplied policy; destination/history reconciliation handled by orchestrator |

Operational glossary additions proposed: **measurement quantity** is what an assay number represents; **measurement scale** is the mathematical transformation already applied; **fitted preprocessing state** is learned training-only information needed to apply a policy; **observed availability** distinguishes measured entries from upstream/downstream reconstruction; **Feature lineage** records measured parents of a derived quantity; **composition reference set** defines the parts used as a denominator. Retain Study/Sample/Subject/Analysis unit distinctions in CONTEXT.md.

Citation library proposal: reuse full primary citations and section anchors from the A evidence note, store DOI/URL, title, version, inspected date and verification type in the future package citation manifest. New container records: SummarizedExperiment DOI 10.18129/B9.bioc.SummarizedExperiment, release 1.42.0; MultiAssayExperiment DOI 10.18129/B9.bioc.MultiAssayExperiment, release 1.38.0; official MAE Quick Start dated 2026-04-28. These release pages were inspected while drafting. Do not use a cached manual's version as proof of current release behavior.

## 8. Software architecture, environment and reproduction

Proposed A graph: configuration/file preflight → typed metadata and matrices → identity/mapping validation → immutable ingest checkpoint → resolved per-Block operation plan → profile validation/filter/authorized processing/derive → SE/MAE validity and lineage reconciliation → A-stage artifact/provenance handoff to B and QC summary to D. In C validation, the same approved preparation logic is fitted on each training partition and applied according to A-D08; it is not a second undisclosed primary pipeline.

Modules have one responsibility and communicate through these documented values. No module reads another module's private state. Production packaging/CLI organization follows the supplied R package/tidyverse/styler/namespaced-call decisions if approved. No `library()` inside functions. New plotting, serialization, hashing or validation dependencies require explicit approval; they are not smuggled in as incidental tooling.

**Environment requirement:** choose an exact R patch, Bioconductor release and complete compatible lock. Current documented BioC3.23 pages recommend R4.6; MAE1.38.0 itself declares R>=4.5.0. The supplied floor R>=4.3 is not sufficient to install arbitrary current versions. Exact R, optparse, yaml, renv, testthat, VSN/RF implementation, dependency tree and system libraries remain unknown/unselected. Container image/tag/digest is unknown; no Dockerfile, renv.lock or container is claimed to exist. All dependency/source licenses must be inventoried, including lipidr's LICENSE file and imputation transitive requirements.

**Provenance schema:** run ID; configuration byte hash plus canonical resolved-policy hash; relative/private input location reference, content hash and byte size; input/classification declarations; row/column key hashes; source revision and dirty-tree status; lock hash; R/platform/session information; package versions/source hashes; RNG kind/master/per-stage seed; stage input/output scale/counts/masks; fitted-state and partition fingerprint; exact checkpoint/output SHA-256; warnings/errors and reviewed/unreviewed validation status. Hashes are integrity/provenance identifiers, not anonymization; sensitive manifests stay outside Git/Notion. Published synthetic logs may contain synthetic names only.

**Checkpoint/restart rule proposed:** write stage outputs and their manifest to a temporary run-local location; verify sizes/hashes and invariants, then write a final completion marker by atomic replacement where supported. Resume only when source/config/lock/state/seed match. Recompute a partial stage; never repair it silently in place. Reuse all unaffected verified checkpoints; changed scientific params invalidate dependent stages. Exact cross-platform atomicity and serialization/version compatibility need implementation tests.

**Resource requirement:** dense double matrices alone need at least `8 × sum(features_b × samples_b)` bytes; working copies, masks, derived arrays and statistical packages increase this. Preflight must report that lower bound and per-method estimated allocations. No n-based universal runtime is asserted. RF/kNN fitting and repeated resampling need empirical profiling after API choice; disk budget includes immutable checkpoints. If an operation cannot fit the declared budget, fail before allocation or propose an explicitly approved backing-store dependency.

**Proposed commands, all unrun:** after R/host/dependency confirmation, capture `R --version`, restore the approved lock, capture `sessionInfo()` and selected package `formals()`/source hashes; then `R CMD build .`, `R CMD check <built-package.tar.gz>`, focused `testthat` suites, and CLI smoke runs on two synthetic Design fixtures. Exact installed binary paths, restore command flags and package filename are pending the actual environment. Proposed future CLI shape is `Rscript inst/exec/triomics --config <synthetic-config> --stage A --output <run-dir>`; `--stage`/resume flags are schema proposals, not an existing runnable command. Quarto rendering belongs to D; A publishes machine-readable summaries for it.

Expected protected run artifacts: validated resolved config, ingest and processed SE/MAE checkpoint, input/Feature/Sample lineage, Block availability, filter losses, missingness summary, scale/operation manifest, fitted-state references, RNG/session info, validation log and completion status. Synthetic demonstration includes separate truth files; real Study artifacts cannot be committed. A reproduction guide must distinguish local replay from a verified private-repository push and Notion synchronization.

## 9. Proposed bounded implementation tickets

These are **draft ticket bodies within the requested single file**, not published tickets and not `ready-for-agent`. The to-spec and to-tickets skills were actually read and their synthesis/test-seam/vertical-slice workflow applied. Their generic publish-ready defaults conflict with the user's requirement for all four complete independently reviewed specs, unresolved decisions and explicit dependency/R gates, so they are not followed here. User-required concrete owned paths also take precedence over skill advice to avoid paths. Seam/ticket-granularity approval is not claimed. Paths below are proposed future ownership, not existing files.

Global blocker G-A: locate/reconcile historical handover, obtain technology and actual dependency/fleet approval, resolve relevant A/B/C/D contract ADRs, and independently review all four complete specification packages. Every ticket is blocked by G-A plus listed prerequisites. No ticket frontier is executable now. Each delivered slice includes a CLI-visible status/artifact and meaningful validation; none is merely an isolated skeleton claimed as implementation.

### A-T01 — Validate and explain a synthetic Study configuration

**Blocked by:** G-A, A-D01/D09. **Owns:** proposed `DESCRIPTION`, `NAMESPACE`, `R/config.R`, `R/cli.R`, `inst/exec/triomics`, `inst/schema/config.yaml`, `tests/testthat/test-config.R`, approved lock/environment files. **Deliverable:** CLI config-validation mode reads a tiny synthetic config, resolves paths/types and either prints a structured validation report or fails before processing.

**Acceptance:** unknown/duplicate keys, incompatible quantity/scale, bad list lengths and absent referenced files name the exact key/file; user-design metadata preserved; no credentials/data echo; `--help` and nonzero failure exit behavior verified. Dependency additions limited to approved manifest. **Smoke:** valid synthetic config then one misspelled key and duplicated key. **Restart:** no processing checkpoint created by validation-only run. **Review:** independent standards+spec review with source diff and actually captured outputs; build/check result recorded, not assumed.

### A-T02 — Generate and ingest explicitly keyed synthetic multi-Block data

**Blocked by:** A-T01, A-D05/D10. **Owns:** `R/simulate-study.R`, `R/ingest.R`, `tests/testthat/test-ingest.R`, `tests/testthat/test-simulate-study.R`, `inst/examples/synthetic-ingest/`, `man/simulate_study.Rd` through approved documentation workflow. **Deliverable:** CLI generates a labeled small Study and truth separately, reads quantified matrices and produces validated per-Block SE plus identity audit.

**Acceptance:** all six §6.1 fixture families and one malformed variant per resolver row are present, with paired/two_group/continuous/timeseries direct modes and explicit repeated >2/independent >2 fixtures unless A-D10 approves an extension; exact types/maps and missing-visit cases preserved. Numeric text/NA handling exact; permutation invariant; duplicate keys reject under the approved registry; original masks/unknown provenance distinct; true latent factors/effects/missingness recorded outside fitting objects. B validates each model or documented unsupported route; A fixture generation does not claim inferential support. **Smoke:** three-block example with intentionally scrambled headers plus inventory verifying all six fixture families. **Restart:** same seed/config produces same semantic synthetic artifact; avoid timestamp-dependent content hashes where defined. **Review:** review generator truth against hand cases, resolver-row coverage and no-data-in-repo guard; no factor-recovery guarantee.

### A-T03 — Assemble a Study without silently changing its Samples

**Blocked by:** A-T02, A-D01/D05, agreed B/C availability contract. **Owns:** `R/assemble-study.R`, `R/sample-map.R`, `tests/testthat/test-sample-alignment.R`, `tests/testthat/test-containers.R`, synthetic map examples. **Deliverable:** CLI A-ingest output includes MAE, explicit sampleMap and availability/loss record.

**Acceptance:** strict mismatch fails, approved intersection/union behavior explicit; repeated visits stay separate Samples; no Subject requirement for independent data; constructor drops detected; 2/4/5 Blocks preserve values; identical feature strings across namespaces remain distinct. **Smoke:** remove one assay Sample and exercise each approved alignment policy. **Restart:** verified ingest checkpoint reused only for matching input/maps/lock. **Review:** independent container/API source and external-behavior check, including installed signatures and round trip.

### A-T04 — Run the approved proteomic policy with complete provenance

**Blocked by:** A-T03, A-D02/D07/D09; selected median/VSN and imputation API decisions. **Owns:** `R/profile-protein.R`, `R/profile-registry.R`, `R/profile-execution.R`, `tests/testthat/test-profile-protein.R`, `tests/testthat/test-profile-contract.R`, synthetic protein policy example. **Deliverable:** configured policy from quantified input through processed SE and audit, including legitimate no-op/previously-processed declarations.

**Acceptance:** observed prevalence precedes reconstruction; unsupported assumptions error; approved MinProb/QRILC wrapper shape/seed validated; counts preserved for B; no double log or hardcoded Block name; filtering losses exact; optional VSN is complete only after joint-transform semantics verified. **Smoke:** known left-censored synthetic case and technically missing sensitivity case, with uncertainty rather than superiority claims. **Restart:** stochastic stage state/seed immutable under replay. **Review:** scientific reviewer assesses assumptions, standards/spec reviewer checks contract; unvalidated method options stay visibly incomplete.

### A-T05 — Run the approved metabolomic policy with an explicit reference

**Blocked by:** A-T03 and accepted shared execution part of A-T04, A-D02/D07/D09. **Owns:** `R/profile-metabolite.R`, `tests/testthat/test-profile-metabolite.R`, synthetic targeted/profiling policy examples; registry additions scoped and reviewed. **Deliverable:** selected PQN/log/missingness policy produces traceable SE and reference artifact.

**Acceptance:** targeted MNAR and profiling MAR policies are explicit; no method chosen from display name; selected RF/kNN APIs and fallback/failure constraints verified; PQN reference/log order unambiguous; known all-missing/insufficient-neighbor cases fail clearly. **Smoke:** positive simple dilution fixture for the approved exact formula, plus a null fixture. **Restart:** learned reference hash included; no reuse after changed reference membership. **Review:** reference calculation and method limits checked independently; no undocumented package introduced.

### A-T06 — Preserve lipid chemistry and endpoint meaning end to end

**Blocked by:** A-T03 and shared execution from A-T04, A-D03/D04/D07/D09 plus B/D derived namespace/family decisions. **Owns:** `R/profile-lipid.R`, `R/derive-lipids.R`, `tests/testthat/test-profile-lipid.R`, `tests/testthat/test-derived-lipids.R`, synthetic lipid input/standards examples. **Deliverable:** configured class-aware processing and derived endpoints produce an approved representation with structural/provenance/coverage records.

**Acceptance:** standard matching behavior verified on pinned source; no hidden lipidr replacement/clamping; class totals/closure/CLR treated distinctly; parser unknowns retained; explicit DBI/chain formulas/units; missing parent annotations/zero denominators/duplicate adducts addressed; finite concentration below one not silently floored. For missing parent abundances, the approved §3.4 policy fixes the eligible universe, distinguishes partial/estimated/unavailable endpoints, carries completeness and observed/imputed contributions, and propagates labels to B/D. No implicit NA-as-zero and no finite-derived-value-as-observed shortcut. **Smoke:** hand doubling-class fixture plus missing matching standard, unparseable lipid and §6.4 missing-parent cases. **Restart:** parser/calibration/reference/parent-universe/aggregation-policy versions invalidate dependent endpoints. **Review:** independent lipid scientific reviewer and contract reviewer; class-aware imputation and missing-parent aggregation cannot be marked implemented without approved definitions and tests.

### A-T07 — Apply approved preprocessing inside a held-out split

**Blocked by:** A-T04/A-T05/A-T06 for methods enabled in the test, A-D08, C fold/analysis-unit contract. **Owns:** `R/preprocessing-state.R`, `tests/testthat/test-preprocessing-resampling.R`, synthetic train/apply example; outer seam edits require orchestrator ownership review. **Deliverable:** CLI or agreed public Study seam fits training-only state and applies it to held-out Samples, serializing state/provenance.

**Acceptance:** held-out perturbations do not alter training state; repeated Subjects never span splits; no held-out outcomes accepted by apply; missing held-out reference/donor limits explicit; unsupported non-inductive methods fail before predictive scoring. **Smoke:** independent continuous example plus paired subject-group split. **Restart:** fold/state hash prevents cross-fold reuse. **Review:** C reviewer independently challenges leakage with adversarial data, not call-count mocks; primary B/C common-matrix contract retained.

### A-T08 — Reproduce, extend and hand off a complete A stage

**Blocked by:** A-T01–07 and A-T09, closure of all promised profile capabilities or explicit maintainer disposition. **Owns:** `R/provenance.R`, `R/checkpoint.R`, `tests/testthat/test-a-end-to-end.R`, `tests/testthat/test-resume.R`, `docs/reproduction/A-ingest-profiles.md`, `inst/examples/synthetic-a-cli/`, and scoped manifest exports. **Deliverable:** full A-stage CLI for two Designs, reproduction Markdown and synthetic CLI fixture, checkpoint replay and fourth-Block extension evidence ready for B/C/D consumers. These are proposed owned artifacts, not files claimed to exist.

**Acceptance:** two-Design smoke checks, 2/3/4/5 Block loops, namespace collision, exact losses, interruption/resume and environment mismatch behavior verified; all outputs/provenance produced from approved source and lock; private-data guard reviewed; dependency and pending-validation ledger complete. **Smoke:** execute proposed reproduction commands in clean approved environment. **Restart:** interruption fault injected during output write produces no successful artifact; resume reuses only verified ancestors. **Review:** independent standards/spec review and scientific gate, then orchestrator records exact commit/checks and verifies authorized private push/Notion update. No tag “validated” if required checks are deferred.

**Milestone 9 handoff (A-SR02):** reserve `vignettes/ingest-profiles.qmd` for the supplied vignette milestone 9, after annotation/report, using then-validated A artifacts and the A reproduction guide. It is not an A-T08 deliverable or prerequisite. The later vignette ticket must own Quarto rendering/inspection and cross-component examples in the global milestone plan; no earlier order change is adopted here.

Ticket boundaries can be merged/split only through documented review; horizontal shared-executor work is kept within the first runnable profile slice, and later slices reuse only its accepted interface. Any necessary cross-component edits receive explicit ownership instead of overlapping delegates.

## 10. Readiness checklist and unrun-validation queue

| Required kit category | Concrete artifact in this draft | State |
|---|---|---|
| Scope, mapping, precedent, claim limits | §1, profiles retained, comparison links | Drafted; history and exact novelty comparison incomplete |
| Dependency/question coverage, versions, terms, namespaces | §5 plus A evidence/review | Partial facts verified; APIs/source/licenses/run behavior pending |
| Input/output/invariants/errors/config | §3–4 | Proposed; owner contracts not amended |
| Validation/ground truth/real-data limits | §6.1/6.4 | Planned only |
| Two different Design examples | §6.2/6.3 | Hand-worked, not executed or statistical validation |
| Decisions/glossary/citations/unknowns | §7 and external ledgers | Proposed decisions pending |
| Architecture/environment/provenance/resources/runs | §8 | Design stated; lock/container/host not established |
| Bounded tickets/dependencies/ownership/acceptance/restart/review | §9 | Draft, unapproved and unpublished |
| Substantive failure tests | §6.4 and per-ticket acceptance | All unrun |
| Reproduction artifacts/readiness | §8 and this section | Not ready; no hidden pass claims |

Blocking queue: (1) locate and reconcile historical architecture/handover; (2) maintainer R confirmation and fleet/dependency decisions; (3) schema/namespace/sample/derived/fitted-state ADRs A-D01–10; (4) A/B/C same-matrix, resampling and derived-endpoint agreements; (5) exact approved environment and external package API/license/source pinning; (6) required package checks and synthetic probes; (7) measurement validation protocol/data access where necessary; (8) independent full A specification review and all-four-packages gate; (9) verified repository milestone publication and exact Notion synchronization by orchestrator.

For each future deferred execution record: ticket/decision ID, source commit (currently no verified implementation commit), exact environment/command, expected artifacts, actual blocking dependency, responsible role and next action. A failed/unavailable host does not turn unrun checks into passes. Scaffolding does not complete A. This draft can support documented decisions and review while all blocked work remains explicit.
