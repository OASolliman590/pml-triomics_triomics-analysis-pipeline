# Shared data contracts — schema version 2.0

All schemas belong to A-T01/02's single in-package schema module. Names below are
specified artifacts/services, not claims that implementations already exist.
JSON is metadata/envelope interchange; assay matrices remain typed SE/MAE/RDS
artifacts and TSV exports where appropriate. Numerical NAs serialize as null
with a reason/status; strings such as "NA" are not numeric values.

## 1. Envelope and atomic provenance

Required common envelope: `schema_version`, `artifact_type`, `artifact_id`,
`study_id`, `run_id`, `status`, `created_utc`, `config_sha256`, `inputs` (ordered
artifact_id/sha256 pairs), `payload_sha256`, `producer` (package/version/commit),
`reason_codes` and `privacy_class`. `analysis_id`, `preparation_context_id` and
`representation_id` are required on applicable analysis artifacts, null only for
study-wide artifacts. SHA256 is 64 lowercase hex over actual bytes, never a title.
Metadata/content digests are distinct from Git commit IDs. No wall-clock path is
used as a scientific identity. Stale/unknown schemas/hashes fail before numerics.

Statuses: `not_requested`, `not_started`, `running`, `complete`, `partial`,
`unavailable`, `failed`. `not_requested` differs from a failed requested route.
Completion policy is separate `required`/`optional`. Scientific inference status
is separate `eligible`, `descriptive_only`, `not_estimable`, `unsupported`, or
`unverified`. A completed writer does not set a statistical status to eligible.

Write payload to a temporary file within the output root, verify checksum/schema,
then atomically finalize payload and manifest. A manifest is complete only when
all referenced payloads exist and hashes match. Interrupted outputs are not
resume candidates. Cache key binds inputs, effective config, methods/versions,
resource hashes and preparation context. Changed input/seed/model invalidates the
appropriate descendants. Validate paths against an allowed local root; reject
traversal/symlink escapes. Logs contain IDs/codes, not secret clinical values.

## 2. Registries and identity

| Artifact | Required columns / keys |
|---|---|
| SourceManifest | source_id, study_id, role, local_ref, format, sha256, original_ref/hash when converted, sheet/region/header/column roles when applicable, conversion evidence, upstream steps, selection_history, selection_evidence_ref |
| SampleRegistry | study_id, sample_id, sample_role, optional subject_id/visit_id, specimen/context, source_ref; sample_id unique within Study |
| AnalysisUnitMap | study_id, analysis_id, preparation_context_id, unit_id, unit_type, source_sample_ids, subject_id if applicable, visit weights/sign, eligibility/exclusion and representation_id |
| FeatureRegistry | study_id, block_id, feature_key, original_id, id_namespace, source_id/source_row, measured_or_derived, quantity/scale/units, structural_resolution, alias_refs, parent_refs, annotation_route |
| FeatureAliases | feature_key, alias_namespace, alias_id, relation, asserted_by, resource/evidence/hash, resolution; aliases never add measured rows |
| SampleMap | block_id, assay_column, sample_id, source_column; exact identity and explicit technical-replicate aggregation refs |
| FeatureFlow | feature_key, context/stage, retained, reason, original/observed/imputed/missing counts, parent coverage and prior checkpoint refs |
| ClinicalVariableDictionary | variable_id, label, type, units, codes/missing map, measured/derived/device/reference/admin role, allowed_model_roles, time semantics, privacy, verification/evidence |
| ClinicalObservation | observation_id, subject/sample/visit/static scope, variable_id, typed value, comparator/limit, missing reason, multi-response set, specimen/time, source_ref/hash |
| ClinicalSampleLink | link_id, observation/visit -> sample/unit IDs, cardinality, time window/ties, source hashes, approved/unmatched/ambiguous status |

Technical QC/blank/standard roles are not model subjects. Primary IDs have explicit
scope; identical codes in two studies cannot join. Sample permutation must change
neither matched values nor effects. Duplicate measured molecular labels require
unique feature keys, not automatic name suffixing as a biological decision.

## 3. Preparation and resolved analysis

PreparedStudy payload: ordered block collection, SE assays and original masks,
MAE sample map, registries, context specification, `state_refs`, feature-flow and
operation ledgers. Assays use features x samples, unique named dimensions; original
masks distinguish known-observed, known-missing, known-imputed and unknown history.

PreprocessingState: state_id/hash, mode, training_unit_ids/hash, ordered feature
keys, reference values/scales, learned parameters, input/output quantity, original
mask lineage, operation order, RNG seed/kind, package/runtime versions and fit
input hashes. Apply output refers to the same immutable fit state.

ResolvedAnalysis: common context matrices, AnalysisUnitMap, feature registry,
representation specification (levels/delta/mean/subject_centered), outcome/covariate
view, ModelPlan (terms, column map, explicit contrasts, residual assumptions),
eligibility/coverage, hypothesis-family declarations and input hashes. Missing
views retain their per-block availability map; no hidden global intersection.

## 4. Statistical/clinical results

BlockResult row: result_id, feature_key, block_id, analysis_id, contrast_id,
family_id, effect, effect_unit, ordinary_se, moderated_se (when applicable),
statistic/name, ordinary_df, effective_test_df, p, q, inferential_status,
n_samples, n_subjects, n_rows_used, n_pairs, direction_numerator/denominator,
direction_zero_count, direction_status, estimation_status/reason, count_prior_ref,
model/ref/source/representation hashes. Zero is a valid estimate; unavailable is
null plus status. Method-inapplicable quantities are null with reason, not omitted
silently. No p value is reverse engineered from a supplied q.

FamilyLedger: family_id, claim_scope, member_ids/hash, declared/eligible/evaluated/
failed counts, correction method, selection/model/dependence justification,
internal-placeholder policy and effective thresholds. Sum of final dispositions
must equal declared members. CandidateNominations: nomination_id, feature_key,
source_branch, result/factor_ref, executed selection rule/value, family context.
Multiple nominations cannot become multiple measured observations.

ObjectiveRegistry: objective_id/version, question, mode, response/predictor refs,
analysis population, time/horizon, covariates/roles, missing policy, hypothesis
status and disclosure, family definition, selection/deployment lineage.
AssociationEndpoint: endpoint_id, unit values/map, type/units, producing fit,
training IDs/outcome, orientation/scale, selection ancestry, available_at_time,
dependency group, matrix/source hashes. ClinicalAssociationResult adds objective,
clinical variable, endpoint, effect/SE/interval/statistic/p/q/status, adjustment,
counts/exclusions, model and all construction lineages to the statistical pattern.

## 5. Integration and evaluation sidecars

IntegrationResult retains six fields in C_INTEGRATION.md. `params` refers to an
IntegrationRun sidecar containing native_method, versions, training state,
required views/units/features, eligibility reasons, score convention, variance
metric/units/denominator, convergence, selection rule and native diagnostic refs.
Native opaque model objects cross seams only as typed version-bound sidecars.

SplitPlan: outer/inner IDs, training/test unit IDs and grouped-subject IDs, target,
stratification/temporal rules, seeds and creation hash. No sample in a train/test
partition may share the forbidden generalization unit. EvaluationResult records
pipeline candidate, fold, metric definition/value, n eligible/predicted, failures,
uncertainty method, tuning/selection history and complete preprocessor state refs.
A failed fold cannot be removed from the denominator without disposition.

## 6. D, target evidence and report bundle

MappingEdge: measured feature -> namespaced molecular entity, relation/resolution,
taxon/context, resource release/evidence and ambiguity group. EnrichmentResult:
set/contrast/family, method, eligible universe/rank/model refs, counts/statistics/
p/q/status, resource and selection provenance. ResourceManifest binds exact
release/snapshot, rights, operation, network/cache permissions and actual hashes.

TargetRequest/TargetUniverse/TargetContext: target/drug/entity/taxon/context IDs,
mode, bounded members, selection/hypothesis history and comparison rules.
ExternalTargetEvidence: exact compound/target construct, evidence axis, assay,
endpoint/value/units/relation, experiment/publication group, controls/limitations,
source/ref/hash and external-versus-this-study status. TargetEvidence: observed
feature/result -> typed sourced mechanistic chain -> target, expected versus
observed direction or unknown, support/conflict/ambiguous/uninformative status,
coverage and dependency group. TargetAssessment has per-axis assessability,
alternatives, comparison validity and no inferred binding probability.

ReportBundle: immutable manifest of A/B/C/D/clinical/target artifacts, available
and unavailable sections, audience/disclosure policy, citation library and
rendering config. Renderer is a consumer, not an estimator.

## 7. Error taxonomy

Machine errors contain code, module, stage, config_key or source_ref, safe message,
remediation and dependency/requiredness; never raw patient values. Prefixes:
`A_CONFIG_*`, `A_SOURCE_*`, `A_ID_*`, `A_SCALE_*`, `A_STATE_*`, `A_CLINICAL_*`;
`B_DESIGN_*`, `B_NONESTIMABLE`, `B_SELECTION_*`, `B_FAMILY_*`;
`C_ELIGIBILITY_*`, `C_NA_CAPABILITY`, `C_SCORE_*`, `C_VARIANCE_*`, `C_LEAKAGE_*`;
`D_RESOURCE_*`, `D_MAPPING_*`, `D_UNIVERSE_*`, `D_TARGET_*`, `D_PRIVACY_*`.
CLI exit 0 complete under declared optionality; 2 invalid/unsupported request;
3 missing execution/dependency/rights; 4 execution failed or required incomplete.
Actual member statuses always remain visible regardless of aggregate exit code.
