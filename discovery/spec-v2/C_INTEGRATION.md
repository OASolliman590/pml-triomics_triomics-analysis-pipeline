# C — integration and leakage-safe evaluation

## 1. Input eligibility

`integrate_blocks(resolved_analysis, integration_spec)` accepts exactly B's keyed
molecular values and representation for this context. Record the sorted unit and
feature maps, matrix hashes, source roles/selection, outcome eligibility, required
views and whether the task is exploratory or predictive. Summaries or selected
FC/p/q exports cannot reconstruct covariance. No unknown sample intersections,
covert row deletion, whole-view imputation or second withinVariation call.

A delta representation has one subject-contrast row, not two visit labels. It may
support a declared group-from-change task, but not classify a vanished occasion
label. Baseline deployment may not consume follow-up delta. All-missing samples
across every view are ineligible. A wholly unobserved view cannot be estimated.
Cell-level and per-sample missing views have distinct capability tests.

## 2. Protected IntegrationResult

Keep exactly six top-level fields:
- `scores`: analysis units x components, with unique stable row/column names;
- `loadings`: named list of feature x component matrices for predictor blocks;
- `variance`: predictor blocks x components, verified proportions with a sidecar
  declaring assay space, denominator and incremental/cumulative semantics;
- `selected`: named list of feature-key vectors under an executed selection rule;
- `backend`: scalar adapter family name;
- `params`: effective settings, method identity, state/version and provenance refs.

Status, predictions, block-native scores, model objects, fold assignments and
variance definitions belong in typed sidecars/envelopes. A successful common
object has finite scores/loadings/variance for its declared retained dimensions.
Native sparse zeros are valid; unavailable values are not fabricated as zeros.
Unusable/pruned components have explicit disposition. Dense selection is explicit
(`none`, an explicit top-k rule, or a validated threshold rule); no significance
is inferred from loading size. Sparse selection is the executed nonzero support.

## 3. Concatenation baseline: C-01 and C-02

PCA uses `stats::prcomp` on units x features with training-only feature centering
and optional scaling. Default scaling must be explicit in config. Optional block
weight `1/sqrt(p_b)` is a declared engineering convention, not biological calibration.
Zero-variance features are recorded and omitted using training observations only.
Apply uses stored feature order, means/SDs, block weights and native rotation.

For PCA's common per-block variance, this kit explicitly specifies the projection
metric, rather than pretending prcomp returns native block fractions. Let X_b be
the centered/scaled/weighted training block and t_k, p_bk the native score and that
block's loading portion. Export `||t_k p_bk'||_F^2 / ||X_b||_F^2`; denominator must
be positive. With orthogonal training scores this is the incremental block
reconstruction fraction. Verify against SSE reduction for each component; label
`metric=orthogonal_pca_block_projection`, not backend-native consensus variance.
The global prcomp variance remains a separate diagnostic. This reviewed definition
is a bounded clarification of the settlement's permitted explicit reconstruction
route; it must pass the numeric oracle before C-01 acceptance.

Supervised baseline: concatenate the same prepared/scaled predictor blocks, use
mixOmics PLS-DA for categorical Y or PLS regression for a scalar continuous Y.
Use native predictions and the same nested split/evaluation service as multiblock
methods. Baseline feature counts, tuning options and compute are recorded; do not
make a deliberately weak baseline to improve the sophisticated method's ranking.

## 4. Categorical DIABLO: C-03

Use `mixOmics::block.splsda` for categorical supervised integration, with explicit
block design, components, keepX/tuning grid, distance/vote prediction rule and
CV plan. Check a square, named, symmetric predictor-block design in [0,1] with
zero diagonal at the wrapper boundary; preserve the exact requested values and
record any documented native expansion involving Y. A 0/1 default is not silently
chosen. Do not describe the design as a universally correct correlation setting.

Require `score_definition=consensus_mean` for the common-score route. For each
component choose the lexicographically first stable block_key as anchor. Orient
it so its largest-absolute loading (tie: feature_key) is positive. Align each
other block's scores/loadings to positive correlation with that anchor on the
predeclared common training units. Reject undefined/near-zero correlation or
zero/near-zero SD using recorded tolerances. Default numerical correlation
tolerance is sqrt(.Machine$double.eps), an engineering convention, not evidence
of biological concordance. Freeze signs, training means/SDs, equal 1/N weights,
required views, alignment units and native prediction state.

Common score = mean of the signed, training-standardized native block variates.
Apply calls the verified native scoring/prediction API and uses stored transforms;
never reorient on test data or reconstruct scores by assuming X %*% loadings.
Missing required views make this common score unavailable; do not average fewer
views. Prediction outputs remain native predictions, not the consensus score
thresholded as a new classifier. Native block diagnostics are retained.

The common variance uses verified native predictor-block/component proportions,
not variance explained by consensus scores. Native diagnostic absence or undefined
semantics makes common adaptation unavailable with the native model preserved;
no clamping, zero filling or arbitrary eigenvalue conversion.

## 5. Continuous multiblock integration: C-04

Use `mixOmics::block.spls` in declared regression mode for one scalar continuous
response, supplied as a named n x 1 matrix. Fix response scaling/back-transform,
keepY behavior, tuning metric and prediction extraction against the installed
API. This is continuous block.spls, not categorical DIABLO. Multivariate clinical
response matrices require a separate scope decision and are not silently enabled.

Native response-block variates/loadings are diagnostic only. Common loadings,
variance and selected contain only the original predictor assay blocks. Held-out
Y must not participate in predictor scores, sign orientation, preprocessing or
prediction. The consensus training convention, native-variance qualification and
missing-value capability checks apply to this adapter as specified above.

## 6. MOFA2: C-05

Use the R MOFA2 interface and isolated compatible mofapy2 environment. Supply
Gaussian continuous views on explicitly transformed/scaled data, with factor
count, convergence mode, seed and training options recorded. Do not treat group
labels as supervised outcomes. MOFA multi-group modeling, when used, is a declared
statistical group structure, not a convenience label copied from every condition.

Use the pinned native factor/weight getters and variance-explained output, with
verified units and view/component correspondence. Preserve per-view missingness;
MOFA's missing-data likelihood is not synthetic imputation evidence. Verify fit,
save, reload and output agreement. Record optimization/convergence, factors
removed by native rules, model dimension and sensitivity across declared seeds.
Compare factors after sign/permutation alignment, and use subspace comparisons
when individual factors are not identifiable. Never force a stable biological
name onto unstable components.

Core MOFA is exploratory structure discovery. New-subject score inference or
predictive comparison requires an independently qualified inductive adapter;
full-cohort factors followed by endpoint CV are not valid held-out evaluation.
No general MOFA predictive capability is asserted merely because training works.
Native model/data/annotation sidecars are required for D's native enrichment route.

## 7. Evaluation and clinical handoff

C-02 implements subject/group-disjoint outer splits and nested inner tuning.
When rows are independent samples, the grouping unit is the sample. Each inner
and outer training split invokes A fit/apply and B representation resolution as
needed. Feature eligibility, normalization, imputation, scaling and tuning are
fit on that split's training data. Stratification cannot violate subject grouping.
Require both classes and model support in applicable folds; infeasible requested
folds fail explicitly rather than silently reducing folds or dropping a class.

Categorical evaluation: fixed native distance/vote rule, balanced error/accuracy,
confusion matrix, per-class counts and failure/uncertainty ledger. Report AUROC
only for a valid continuous decision score with orientation; discrete labels are
not probabilities. Continuous evaluation: RMSE, MAE and held-out R-squared against
a training-only intercept baseline. Repeated CV folds are not independent patients;
uncertainty/resampling unit and dependence limitations must be stated.

Permutation/null tests preserve the exchangeability implied by the study design,
including subject/exposure structure. Re-run the full learned pipeline including
tuning for null comparisons. Runtime/cost budgets and all inspected candidate
pipelines are logged. Test-set reuse cannot be reset by a new filename.

C-08 exports immutable per-unit endpoint artifacts and schedules B-owned clinical
association requests. C-09 is conditional matched clinical-only, molecular-only
and combined prediction for one declared endpoint, using identical eligible
patients and splits; method eligibility and temporal availability are mandatory.
C-06 compares outputs on declared comparable contexts, coverage and metrics,
not blindly comparable variance numbers. C-07 remains evaluation evidence within
C-06/VALIDATION.md, not a forty-second ticket. No guaranteed benefit is promised.


## 8. Shared adapter and evaluation seams frozen before parallel backends

C-01 owns the internal BackendAdapter registry and common consensus utility;
C-02 owns tuning/evaluation orchestration. A registered adapter provides:
`validate(resolved_analysis, params) -> Eligibility`,
`fit(resolved_analysis, params) -> BackendFit`,
`predict(backend_fit, new_resolved_analysis, params) -> PredictionArtifact`, and
`extract(backend_fit, context, params) -> common result plus typed sidecars`.
Predict may return a typed unsupported capability for a genuinely non-inductive
method; that method cannot enter predictive evaluation. Native response values
are never read by a new-unit prediction path. BackendFit contains only native
state plus explicit version/hash/role metadata, not a second project schema.

`make_consensus_state(native_scores, native_loadings, training_units, params)`
and `apply_consensus_state(native_scores, state)` live in
R/integration_consensus.R, owned by C-01. Synthetic native-score matrices test the
exact anchor, sign, training standardization, required-view and tolerance rules
before C-03 and C-04 run in parallel. Neither backend owns or duplicates this seam.

`evaluate_pipeline(study, analysis_spec, integration_spec, split_plan)` lives in
C-02. It drives each training-only preprocessing/fit and new-unit prediction.
Wrapper-controlled grid search is the default tuning implementation: every inner
split repeats all learned stages. A native tune helper may be used only when its
fold and preprocessing contracts are proven equivalent. Native helper existence
is not assumed for every continuous/multiblock method, and its default resampling
must not override subject grouping. Grid/tuning behavior is tested separately
from the native fit/predict API.

For a supervised concatenation common result, split native X loadings/weights by
original predictor block and declare their role; common scores are native X
variates, not predicted labels. Where no native block variance exists, use the
explicit nested **training-score subspace** metric: Q_k is an orthonormal basis
for the first k training X-score columns; block fraction for component k is
(||(I-Q_(k-1)Q_(k-1)')X_b||_F^2 - ||(I-Q_k Q_k')X_b||_F^2)/||X_b||_F^2.
Set Q_0 empty. Rank-degenerate columns are unavailable with disposition. Label
this projection metric and verify nested SSE exactly; it is not native PLS
variance or predictive variance. DIABLO/MOFA native variance rules are unchanged.
