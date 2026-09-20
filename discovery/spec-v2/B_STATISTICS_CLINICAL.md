# B — design resolution, inference and clinical associations

## 1. Public seams and immutable requests

`resolve_analysis(prepared_study, analysis_spec)` returns ResolvedAnalysis;
`fit_block_models(resolved_analysis, statistics_spec)` returns complete per-block
results and family ledgers; `associate_endpoints(endpoint_bundle, clinical_view,
objective_spec)` fits eligible clinical associations. B owns all these fits.
C/D may create new immutable endpoints; the orchestrator schedules new B requests.
B never calls a producer to construct its own response and never refits upstream
preprocessing. Input hashes and keyed AnalysisUnitMaps must match before dispatch.

## 2. Supported design matrix construction

Formula construction is restricted to declared metadata terms, not arbitrary R
code from YAML. Every term has a stable model-column ID and a printed R formula;
contrasts are named coefficient weights over those IDs. Full rank/estimability is
checked for each feature's actual usable rows, not once for the whole matrix.

| Design / estimand | Representation and model |
|---|---|
| Independent two/multiple groups | Levels; `~0 + condition + declared covariates`; declared scalar contrasts, including treatment minus control. |
| Continuous molecular response association | Levels; intercept + declared continuous predictor + covariates; coefficient has predictor-unit interpretation. |
| Exactly two visits: within-person change | Delta target minus reference, one row per complete subject; intercept-only moderated one-sample test or a declared modifier model. |
| Exactly two visits: group difference in change | Subject deltas; `~0 + between_group + delta_covariates + declared modifiers`, then group contrast. |
| Between-person level / subject mean | Separate mean-retaining Analysis with explicit visit/mean weights; never claim it from centered/delta data. |
| >2 visits / trajectory | Qualified centered response with full subject fixed-effect design and explicit residual assumptions; linear time or an explicit natural-spline basis. Actual time units/origin, knots/boundaries and contrast weights required. |

The repeated fixed-subject lane uses ordinary residual df `n_observed - rank(X)`
where applicable. An offset removed by subject centering does not justify dropping
subject terms/df or adding ordinary duplicateCorrelation as a second correction.
Per-feature residual support and covariance assumptions must pass the repeated
calibration protocol. Unsupported random slopes/covariance return unsupported,
not an unnoticed independent-observation model. Irregular visits are not rejected
merely for being irregular; only eligible, validated patterns may run.

For two-visit deltas, a static covariate's association with delta is a modifier of
change in the original repeated model. It cannot be labelled an estimated
within-person static main effect. Time-varying adjustments use their declared
change or model form. Do not infer confounder/mediator status from correlation.
Missing visits default to error; `complete_pairs_per_contrast` explicitly changes
the analysis population and reports per-group attrition. No missing-visit imputation.

## 3. limma outputs and uncertainty

For eligible continuous responses, use `limma::lmFit`, explicit contrasts and
`eBayes(trend=FALSE)` initially; robust moderation is an explicit parameter with
its actual dependency/behavior checked. Preserve ordinary residual variance and
moderated posterior variance. For a scalar contrast c:
- effect = c' beta;
- ordinary SE = sqrt(sigma_squared * c'(X'X)^-1 c), generalized appropriately for
  the documented weights/covariance actually used;
- moderated SE uses the corresponding posterior variance and unscaled SE;
- moderated t, posterior/prior df and limma's effective total df come from the
  fitted object, not a guessed n-2;
- confidence intervals use the matching statistic/SE/df, never an ordinary SE
  paired with a moderated p without labeling.

Report effect unit: log2 fold change only for a contrast of compatible log2
abundances; per-unit beta, delta difference, CLR contrast and standardized beta
are labelled separately. Preserve p/q availability, n samples, n subjects, rows
used, complete pairs, failure code and residual diagnostics.

DEqMS requires aligned positive peptide/PSM counts, acquisition/count provenance,
verified native API/output fields and a justified count summary for the fitted
representation. Count-prior qualification is particularly explicit for deltas.
On a requested but invalid DEqMS fit, return unavailable; fallback to limma is
permitted only when explicitly configured, recorded and separately interpreted.

## 4. Multiplicity and nomination

Declare eligible family membership before fitting; hash full membership and keep
every disposition. Default family = study/analysis/block/scalar contrast/claim.
Include failed/non-estimable members internally at p=1 for an otherwise valid
family; publish their p/q as unavailable plus the reason. Invalid-null p values,
selection leakage or unqualified dependence invalidate inferential q; do not hide
this by publishing a nominal q as validated. Source-selected panels need a
method-specific valid scope, not automatic exclusion or automatic acceptance.

Candidate nomination requires valid q <= user threshold and the user-specified
effect reporting threshold in declared units. A post-hoc threshold is not a new
minimum-effect test. Clinical endpoints, integration factors and pathway scores
have their own fixed hypothesis families; no omnibus all-omics q is implied.
Preserve original and transformed values and imported-result isolation.

Directional consistency is defined only for interpretable paired changes:
number of eligible nonzero subject deltas having the sign of the contrast's
estimated change divided by the number of eligible nonzero deltas; also report
zero and unavailable counts. For difference-in-change, report within-group change
consistency separately, not a fictitious per-subject difference across groups.
For independent/continuous designs it is not applicable, with an explicit reason.

## 5. Rank sensitivity and diagnostics

Optional Wilcoxon sensitivity is restricted to unadjusted independent two-group
or complete paired/one-sample delta questions. Use R's documented approximation
(`exact=FALSE`, explicit continuity correction), record ties/zero differences and
sample counts; direction follows the registered contrast. This is not a substitute
for a covariate-adjusted moderated effect or identical hypothesis. With adjustments,
multiple visits or an incompatible estimand it is unavailable, not silently run.
Report disagreements; do not vote tests into a result. Dispersion, residual/influence,
missingness/coverage and batch/group confounding diagnostics never choose an
unregistered model or secretly delete subjects.

## 6. Clinical objective registry

Each objective fixes response, predictor, estimand, eligible population, time window,
adjustments with rationale, missingness, construction/selection lineage, family,
exploratory/confirmatory status and deployment information when predictive.
The initial executable association lane is continuous molecular/eligible-score
**response** against verified clinical predictors, using the compatible B models.
A binary clinical predictor is a factor, not a binary-response regression.
Unsupported binary/ordinal/count/survival responses stay typed and unavailable
until a separate qualified method; never coerce them into continuous regression.

`between_person`, `within_person`, `baseline_to_later` and `effect_modification`
are explicit modes. Baseline prediction of later levels/change requires horizon
and baseline-adjustment choice; those are different estimands. Interaction tests
are explicit coefficients, not comparison of one significant subgroup with another.
Clinical complete-case handling is an explicit policy with excluded-subject counts;
no reuse of assay MinProb/QRILC on clinical variables.

Use the same service for later MOFA/PCA/DIABLO endpoints. Scores carry training Y,
training IDs, selection, orientation and unit map. Association with a DIABLO
training outcome is descriptive and cannot release an independent-validation q.
Unsupervised factors still require a declared full factor/endpoint family and
selection-aware claims if chosen after inspecting clinical outcomes. A pathway
contrast enrichment is not an individual endpoint; a valid per-unit score artifact
is required first. B-T08 uses synthetic endpoints before C exists; no cyclic build
or runtime dependency is necessary.

## 7. Acceptance gates

B-T01/02/03 oracle effect/sign/SE/df/rank tests; B-T04 repeated/time calibration;
B-T05 aligned counts and qualified DEqMS output; B-T06 full family accounting and
imported p isolation; B-T08/09 clinical types/time/selection/unit checks. B-T07
exports complete bundles and a Markdown reproduction record, not the final HTML.
All ordinary/moderated expectations and null simulations must be explicit in the
validation record. A summary file or a test that merely mocks limma is insufficient.
