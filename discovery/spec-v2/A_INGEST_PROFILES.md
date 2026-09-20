# A — configuration, ingest, preparation and clinical linkage

## 1. Boundary and ownership

A implements `prepare_fit(study, preparation_spec)` and
`prepare_apply(study, preparation_spec, state_refs)`, and the shared schema module.
The six BlockProfile methods retain `(se, params) -> SummarizedExperiment`:
`validate`, `filter`, `transform`, `normalise`, `impute`, `derive`. Derivation is
optional. SE assay dimensions always agree with rowData/colData; shape-changing
checkpoints are separate artifacts, not differently shaped assays in one SE.
A assembles MultiAssayExperiment without positional joins. One block supports A/B;
C validates its own minimum of two. Two and five arbitrary-named blocks are tests.

## 2. Inputs and conversion

Canonical inputs are UTF-8 TSV/CSV, a single declared header, unique measured row
keys and explicit sample-column mapping. IDs are strings; preserve leading zeros,
punctuation and original identifiers. Strict numeric parsing rejects stray text,
Inf/NaN and unknown missing tokens. Zero is a measurement unless declared otherwise.
A source manifest records original file hash, region/sheet/header roles, selection
history, converter/version/options and canonical hash. Native canonical files
need no fictional workbook fields. Formula-derived XLSX input requires checked
cached values or reproducible recalculation; it cannot silently use stale caches.
Native XLSX ingest is an optional later adapter, not a new required raw-data layer.

Every numeric column is a measurement, QC, blank, standard, summary, imported
statistic or exclusion. Group means, p/q/FC and selected lists cannot become assay
samples. Imported results are D display/concordance inputs only. Overlapping
source regions must agree within an explicitly declared tolerance; no average or
first-file wins. Source selection unknown is allowed to be recorded, but gates
inference/evaluation rather than supplying a fictitious full measured universe.

Feature keys are Study+Block+source-row identities, with namespace/original ID as
attributes. Aliases may be many-to-many and cannot merge observations. Sample maps
must be one-to-one unless an explicit technical-replicate aggregation contract
supplies scale, weights and lineage. Biological replicates are never averaged as
technical replicates. Group/visit/subject relationships are not inferred from ID
patterns. Mixed studies fail before fitting; a single study can have partial views.
Whole absent assays are structural missingness, not cell-level imputation targets.

## 3. Preparation order and retained states

1. Validate source identity, numeric roles, quantity/scale and original mask.
2. Remove declared contaminants/QC failures and apply the declared prevalence
   filter using original observations, not imputed values. Record all losses.
3. On eligible quantitative **linear** parents, produce requested derived lipid
   quantities before closure/log/centering. Use a frozen parent universe and
   report retained, missing and incompatible parents. Never sum logarithms.
4. Apply configured transform and normalization in the method registry's order.
   VSN owns its combined transform/normalization and cannot receive a second log.
5. Impute only when requested and scientifically/methodologically eligible.
6. Save immutable prepared assay, mask, derivation and operation ledgers.

Transformation and normalization order is method-specific, not a universal list.
The resolved operation list and input/output scale are frozen before execution.
Re-running a non-idempotent operation on its own output is refused unless the
pipeline restores an earlier checkpoint under a new preparation context.

## 4. Initial registered methods

`methods.json` fixes the finite initial method set, ownership, parameter schemas,
fit/apply availability and scientific qualifications. Registry choices are not
selected automatically from plots, outcome separation or block names.

- All profiles: explicit `none`; `log2` for positive linear inputs with a declared
  zero policy; global prevalence with a declared minimum observed count. A
  group-anchored filter is allowed only with a valid declared selection policy;
  predictive versions use training labels only and their evaluation consequences.
- Proteomic policy: `median_shift` in log2 space; conditional VSN; optional
  MinProb/QRILC only with justified left-censoring and verified implementation.
  Valid peptide/PSM metadata is retained for B, not invented from abundance.
- Metabolomic policy: PQN with a positive frozen reference and valid feature ratios;
  optional MAR methods are explicitly configured. PQN quotients are computed in
  positive linear space, then logarithms are applied. No reference refit on apply.
- Lipid policy: explicit sample-amount/internal-standard corrections when validated
  metadata exist; `none` is meaningful for already normalized concentrations.
  Class sums, abundance-weighted total carbon and double-bond indices are declared
  endpoints. For compatible parents, index = sum(abundance * property)/sum(abundance).
  Zero denominator, unresolved chain property or incompatible units gives an
  unavailable endpoint, not zero. Sum composition supports total carbons/unsaturation,
  not invented sn positions or individual chains. mol%/CLR are separate analyses.
- A simple training-feature-median imputer is an explicitly labelled baseline,
  not a mechanistic default. kNN/RF may open only after an inductive fit/apply
  contract is qualified; transductive methods cannot be advertised as deployable.

For CLR, freeze the part/reference set, log base, zero rule and any replacement
parameters. Structural absence and feature censoring are different. No automatic
pseudocount from global or held-out data. Do not compare CLR effects to raw/log2
abundance effects without declaring their different estimands.

## 5. Fit/apply protocol and fold safety

Fit calls use `params$mode = "fit"`; profile output stores provisional typed
`metadata(se)$triomics$state_payload`. A validates and atomically externalizes it,
then writes `metadata(se)$triomics$state_ref`. Apply passes the resolved immutable
state through `params$fitted_state`, with `mode="apply"`. State contains training
unit IDs, feature universe, masks, references, learned parameters, input scale,
operation order, package/RNG versions and hashes. Apply cannot reselect features,
reestimate a median/PQN reference or use outcome labels from held-out units.
No-op transforms still record provenance. Unsupported apply yields unavailable
predictive capability, not a silent whole-dataset fit.

## 6. Clinical inputs

A owns SubjectRegistry, VisitRegistry, ClinicalVariableDictionary,
ClinicalObservation, ClinicalSampleLink and the analysis-specific clinical view.
Clinical values do not become an omics block by default. Dictionaries record type,
units, allowed model roles, code/label/missing maps, measurement versus derived or
device-estimated role, verification and privacy. Static conflicts, duplicate visit
matches and unknown codes fail or remain explicitly nonmodelable. Censored values
retain comparator/limit; multi-response values remain sets. No unit inference,
nearest-visit matching, administrative predictors or medical interpretation.

A source may supply subject-static and time-specific variables. Joining requires
explicit cardinality, reference time, match window and tie rule. Derived indices
carry input IDs/units/formula provenance; a reference range is not a second patient.
Export aggregate coverage with approved privacy; never put patient values in Git.

## 7. Simulator and acceptance

Preserve `simulate_study(n_subjects, n_features = c(prot=800, lip=400, met=600),
design = c("paired", "two_group", "continuous", "timeseries"), effect_size,
prop_affected, missingness, shared_factor_strength)`. No signature/default change
without an ADR. Record seed through the execution context; save planted truth as a
separate synthetic artifact. N-block support follows names supplied in n_features.

A-T01/02/03 establish contracts, generator, ingest and common preparation machinery.
A-T04/05/06 implement disjoint policies; A-T07 owns train/apply mutation tests;
A-T09 clinical linkage can run beside profiles; A-T08 closes A. Required adversarial
cases: ID reorder, duplicate IDs, summaries-as-data, unknown selected universe,
zero/log mismatch, already-imputed mask uncertainty, all-missing view, altered
held-out rows, parent mass-balance, static clinical conflicts and leaked raw values.
