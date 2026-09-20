# C — integration adapters: provisional specification package

## Adopted design settlement — 2026-09-06

The user delegated unresolved choices to Opus/Sol and Astra. The [settlement](../operations/contract-settlement.md), [Astra arbitration](../reviews/astra-design-arbitration.md), [command contract](../operations/implementation-command-contract.md) and [individual tickets](../tickets/README.md) now supersede conflicting pending alternatives in the older draft below. Design is settled within these bounds; runtime, scientific validation and original G0 are still open; R core plus Python is explicitly approved. C consumes the same per-Analysis/context matrix as B. DIABLO consensus requires explicit configuration and frozen training anchor/sign/scale/native-prediction state; no implicit common-score fallback. Verify native variance units/meaning and NA fit/tune/predict capabilities before successful common outputs. Dense selection requires an explicit rule. C-08 clinical score handoff precedes C-06 complete handoff; C-09 incremental prediction remains conditionally gated.


Status: **independently reviewed provisional draft; not implementation-ready**. Drafted 2026-09-05 using the requested to-spec synthesis and to-tickets slicing disciplines. See the [draft review](../reviews/C-spec-review.md), [revision response](../reviews/C-spec-review-response.md) and [shared contract proposal](../operations/shared-contract-proposal.md). Historical handover, technology confirmation, material contract decisions, executable dependency checks and final readiness review remain outstanding. No automatic `ready-for-agent` label or tracker publication is authorized by this draft. The user explicitly requires conditional packages to remain conditional.

## 0. Three-study revision — 2026-09-06, proposed for confirmation

The [source contract](../operations/source-contract-proposal.md) and [root response](../reviews/opus-review-response.md) incorporate the [Opus review](../reviews/opus-three-study-design-review.md). They add eligibility/provenance artifacts without changing protected IntegrationResult fields. Score-row, variance and fold-preparation decisions remain open.

Input envelopes require `study_id`, `analysis_id`, source references/roles, selection evidence, Feature registry/aliases, coverage and approved representation/unit map. Only A-validated measurements become Blocks. Every cache/artifact is Study-scoped; equal codes across projects never create a join. C cannot reconstruct covariance from FC/p/q or treat repeated pairwise exports as independent observations.

Before every requested backend/context, produce an `IntegrationEligibility` record keyed by Study, Analysis, backend and preparation context: role validity, same-Study identity, matched units, overlap/missing-view pattern, retained-signal/outcome compatibility, source-selection validity, fold-preparation availability, deployment information set and failed prerequisites. This is an enforcement gate. Ineligible requests yield unavailable/failed records under declared completeness policy, never a successful IntegrationResult or performance estimate.

`C_CROSS_STUDY_BLOCKS` rejects mixed Studies; `C_INELIGIBLE_SOURCE_ROLE` rejects summary/list inputs; `C_SELECTION_UNVERIFIED` / `C_SELECTION_LEAKAGE` block unsupported evaluation; `C_REPRESENTATION_TARGET_MISMATCH` names removed/unavailable target information. Existing backend-specific missing-view/coverage errors remain. NA imputation and string similarity cannot manufacture matched assays.

Source completeness alone does not determine evaluation validity. An externally prespecified panel can support evaluation on independent observations with adequate construction provenance. Unknown selection or held-out-outcome selection cannot. Even complete source matrices need all learned preprocessing/tuning/selection inside appropriate training partitions. Exploration on a selected measurement set can be eligible with conditional interpretation; it is not validated prediction or causal evidence.

Group-by-visit integration consumes B's approved representation and units without a second decomposition. Delta rows cannot supervise an occasion label that no longer varies per unit or recover removed subject means. Predicting group from changes is a separately declared task with subject-level folds and deployment inputs. Within every Analysis/fold, B/C must reference the same approved values and unit map.

Existing C decisions gain this eligibility contract alongside score/variance and preparation-context definitions. C-01 adds Study/role/provenance enforcement; C-02 adds upstream-selection leakage/fixed-panel evaluation; C-03/04/05 consume this gate plus their own missingness/outcome checks; C-06 exports per-backend eligibility, selection and coverage to D. No backend or milestone is removed. Shared TS-04/05/07/08/09/11 cover mixed-study/summary refusal, target mismatch, leaking selection, independent panels and partial views. Include deliberately whole-data-selected negative controls and cache invalidation when Study/source changes. All are planned, not executed.

## 0.1 Clinical associations of integration scores — proposed

The [shared clinical extension](clinical-molecular-association-extension.md) adds score construction/training-outcome lineage and a handoff to the B-owned clinical association service. MOFA factor correlations are not a universal adjusted repeated model; DIABLO association with its training outcome is descriptive fit. C-08 handles clinical-score handoff after eligible backends; conditional C-09 evaluates one endpoint with matched clinical-only/molecular-only/combined nested prediction. Clinical endpoints stay outside assay Blocks by default. Unit/representation, selection and temporal leakage gates remain; protected IntegrationResult fields are unchanged.

## 1. Problem, scope and architecture mapping

The consumer needs interpretable cross-block structure with an honest comparison against simpler analysis, without introducing a different unrecorded preprocessing history from per-block inference. C receives resolver-owned processed/decomposed blocks and returns a common IntegrationResult to D. A owns assay processing; B owns design, decomposition and outcome eligibility. C owns backend eligibility, model fitting/tuning, result adaptation and integration-specific diagnostics. C must not infer scientific behavior from a block's display name or perform a second within-subject decomposition.

Build order remains concat_baseline, mixomics_diablo, mofa2. Continuous supervised integration uses the documented block.spls family and must be labelled as such, rather than describing a regression outcome as discriminant analysis. PCA/PLS and MOFA are established methods; triomics' proposed contribution is explicit contracts, matched provenance and reproducible comparison, not new statistical methodology or guaranteed superiority.

Closest precedents are mixOmics multilevel analysis, DIABLO, MOFA/MOFA+, and MultiOmicsIntegrator's orchestration. The latter already documents lipid class/enrichment/saturation outputs. See `../research/BC-methods.md` B-03/C-01–07 and `../research/D-resources-precedents.md` P-Q04–07 plus its independent review. No exhaustive novelty claim is supported.

Proceed when representation, score semantics, environment, CV behavior and adapter contracts have been reviewed and exercised. Narrowing or deferring any backend is a maintainer decision; this package preserves all three. A requested backend that lacks an eligible outcome reports the incompatibility, rather than silently substituting unsupervised PCA. An explicitly requested set of analyses can include eligible unsupervised results and visibly unavailable supervised results, with run completeness policy declared.

### User stories

1. As an analyst, I can compare an integration result with concatenation on the same eligible units and features.
2. As an analyst, I can supply two or five blocks with arbitrary display names without rewriting dispatch logic.
3. As an analyst, I know which transformations and decomposition generated every fitted input.
4. As an analyst, I receive a clear error when my outcome cannot supervise the selected method.
5. As a reviewer, I can inspect explicit DIABLO connections, tuning grids, folds and selection instability.
6. As a reviewer, I can distinguish training fit, held-out performance and exploratory associations.
7. As an analyst, I can use documented missing-view support only for patterns verified by the pinned adapter.
8. As a reviewer, I can inspect MOFA seed dependence after accounting for factor sign and order.
9. As an analyst, I can restart an interrupted fit without mistaking a partial result for a completed model.
10. As a downstream consumer, I can interpret common scores, loadings, variance and selected IDs without reading native backend internals.
11. As a reviewer, I can see null and weak-signal outcomes, including when integration adds nothing demonstrable.
12. As a maintainer, I can reproduce software checks without committing participant-level data.

## 2. Evidence, dependencies and coverage

Evidence entries distinguish documented fact, static source inspection, mathematical inference, policy choice and unknown. None is runtime validation. The linked ledger contains source sections, versions, limitations and verification status rather than relying on agreement between agents.

| Question / interface | Dependency and inspected evidence | Observed version / licence | Remaining verification |
| --- | --- | --- | --- |
| C-API01 baseline decomposition | R stats PCA/SVD contract; specific prcomp documentation/source still to inspect | Chosen R release unresolved | Signature, rank handling, projection, licence notices in distribution |
| C-API02 supervised baseline | Existing R PLS functionality to be selected from already proposed dependencies | No extra package approved | Exact method and prediction API; outcome encoding and probabilities |
| C-01/C-02 categorical and repeated | mixOmics block.splsda source; release withinVariation manual | 6.36.0, GPL >=2; R >=4.4 observed | Installed signatures, missingness grid, repeated decomposition cases |
| C-API03 continuous supervision | mixOmics block.spls | 6.36.0 catalogue; exact signature not frozen | Y shape/scale, tuning, predict and native output contracts |
| C-03/C-04 connections and CV | DIABLO guide; tune.block.splsda source | Release versus moving source distinction unresolved | Grouped folds and all learned steps in nested resampling |
| C-05/C-06 factor training | MOFA2 preparation/training/FAQ; MOFA and MOFA+ papers | 1.22.0, LGPL-3 | mofapy2/Python bridge versions, defaults, API signatures, overlap behavior |
| C-07 incremental utility | Matched baseline, univariate summary and planted/null controls | Evaluation protocol, not a package claim | Prespecified metrics, recovery tolerances and measured uncertainty |
| Input containers | SummarizedExperiment/MultiAssayExperiment through A/B contracts | See A dependency manifest | Concrete resolved data/metadata API and score row map |

Current versions in this table are observed catalogue entries, not an installable lock. No package was installed. R >=4.3 alone does not establish compatibility. In particular, the observed mixOmics release requires R >=4.4; actual R/Bioconductor selection must be reviewed. MOFA's Python backend and its transitive dependencies require their own explicit environment/permission review. The R licence label does not cover mofapy2 or every packaged dependency. No external identifier database is a C dependency; C retains namespace-qualified feature provenance for D.

Primary-source references: [block.splsda source](https://raw.githubusercontent.com/mixOmicsTeam/mixOmics/master/R/block.splsda.R), documentation lines 10–19, native output descriptions and formal arguments; [MOFA FAQ](https://biofam.github.io/MOFA2/faq.html), missing values and factor interpretation. The broader reference library and exact methodological sections are in B/C ledger. Root independently reopened these two sources; multilevel and DIABLO guide retrieval returned errors in this pass, so the earlier researcher's inspections are not claimed as an additional successful root check.

## 3. Owned input/output contract

### Input envelope — proposed clarification, not an accepted new interface

The caller supplies named numeric block matrices with exact feature and analysis-unit identity; a resolver record specifying design, decomposition, sign convention and unit map; a common input fingerprint; original/observed/imputed masks; namespace-qualified feature records; and requested adapter parameters. Each block is transposed for a samples/units-by-features backend only under a recorded orientation conversion. C accepts the resolver output, not arbitrary new sample intersections.

The envelope must make explicit whether rows are original samples or subject/contrast units. A change from the supplied `scores: samples × components` meaning requires owner approval. All corresponding rows are aligned by identity and verified, not trusted by position. Blocks with a different sample set must have a resolver-approved missing-view alignment policy or fail. Structural missingness is distinct from a completed imputed value. Features dropped for zero variance/rank problems remain in a reasoned attrition record.

**Predictive context correction (C-R01, proposed contract):** the completed resolver result is sufficient only for explicitly exploratory fitting. Predictive evaluation additionally requires access through a reviewed public A/B preparation service to immutable pre-fit inputs, eligible metadata, profile policy and design specification. C owns partition generation and evaluation; A owns train-only filter/normalization/imputation fit and apply; B owns fold-valid decomposition/model metadata. Per-fold B/C consume the same authorized fold representation. A whole-study filter cannot be undone from the processed matrix, and an input hash is not a replay service. The evaluation context must declare generalization unit, prediction horizon and which repeated measurements of a held-out subject are available at prediction time. Retrospective whole-pair analysis and prediction of a future visit have different information sets. This service supplements the six-function profile contract only after approved seam/ADR review; its callable signature is not yet frozen. Until then, all predictive-performance claims and C-02 onward evaluation acceptance remain blocked. Exploration must not be relabelled validation.

**Feature identity correction:** the final A/B/C/D registry must distinguish an internal unique feature key from original molecular ID. If A permits duplicate molecular IDs with a source-row discriminator, loadings and selected names must retain that unique key and link through the registry. A naive `(block, namespace, original_id)` join is insufficient in that case. This is a pending shared contract decision, not an adopted extension of the protected fields.

### Protected IntegrationResult — retained fields

| Field | Required shape and meaning | Invariants / unresolved details |
| --- | --- | --- |
| scores | numeric matrix, original contract samples × components | Unique row/component names; exact unit map; paired delta row meaning awaits approval |
| loadings | named list of feature × component numeric matrices, one per input block | Feature names match that block, component names aligned; any dropped feature recorded |
| variance | numeric matrix, blocks × components, proportion explained | Named blocks/components; units are proportions, not percentages; definitions must be recorded |
| selected | named list of character feature vectors | Within-block IDs only, no duplicates; selection rule recorded; join through block+namespace+ID |
| backend | scalar character | concat_baseline, mixomics_diablo or mofa2; actual method also recorded |
| params | list | Effective parameters, versions, seeds, preprocessing and source fingerprints sufficient to reproduce the run |

No new protected top-level field is adopted. A separate proposed run manifest can hold statuses, artifacts and provenance without turning failed fits into valid IntegrationResults. Native objects may be saved as versioned backend artifacts for reproduction; D cannot reach into their internals through the common interface.

**Continuous-response correction (C-R02, source fact plus proposed policy):** inspected upstream [block.spls source](https://raw.githubusercontent.com/mixOmicsTeam/mixOmics/master/R/block.spls.R), documentation lines 43–50 and formals/output sections, expects a continuous response matrix and exposes `keepY` and `mode`. Its native result includes a response block identified by `indY`. The proposed initial scalar continuous target must be converted to a named n-by-1 response matrix; multi-response support is not claimed and requires a separately reviewed scope decision. Configuration explicitly records regression mode, response scaling/back-transform and keepY policy. Extraction must return exactly the original predictor assay blocks in loadings, variance and selected; response loadings/variates belong only in separately labelled diagnostics. Predictor scores and predictions cannot use held-out Y. Pin and execute this call/extraction contract before C-04 acceptance; moving-source inspection is not installed behavior.

**Common-value correction (C-R03, proposed eligibility policy, pending approval):** a successful common scores/loadings/variance object contains finite numeric values. NA, NaN and Inf cannot stand for an unavailable backend or undefined variance. Missing predictors may be eligible for a verified native method, but output eligibility is checked independently. Native zero loadings for retained unselected sparse features remain zero with that meaning; missing values are never filled with zero to pass validation. Eligible retained feature rows are declared in the registry; excluded rows have attrition reasons rather than fabricated loading values. Pruned factors are omitted consistently from all component dimensions and recorded in the run manifest. A zero variance denominator, non-converged fit or adapter output not satisfying the selected success policy yields an unavailable/failed run record rather than an NA-filled success object. Empty selection is valid only when an executed explicit rule yields none; unavailable selection is not encoded as an empty successful list. The finite-success policy itself requires owner review, since the original numeric-matrix fields did not settle this issue. Variance definitions must also state observed versus completed input denominator and whether they describe native block scores or exported common scores.

**Score-semantics blocker:** mixOmics returns variates per block; the common contract supplies one score matrix. Choosing one reference block or aggregating aligned scores changes interpretation. Require an explicit, reviewed score-definition policy; neither averaging nor selecting the first block is a defensible silent default. Keep native block scores available in the integration diagnostic artifact. The policy must be applicable to held-out prediction using training-derived quantities only.

**Variance-semantics blocker:** native per-component explained-variance metrics from different methods may use different denominators and need not be additive for nonorthogonal components. Record definition, denominator, per-component versus cumulative meaning and any scale conversion. Do not compare them as identical metrics or silently clip invalid output into [0,1]. If a backend cannot supply the stated proportion contract, fail adaptation with a reason until a reviewed definition is available.

**Selection-semantics blocker:** sparse loadings support a native nonzero-set definition. Dense PCA/MOFA loadings do not imply a statistical selected set. Require an explicit rule such as all eligible features or a declared loading/ranking threshold; threshold values are choices requiring justification. Empty selection is valid when the configured rule yields none. Selected does not mean significant, causal or independently validated.

### Proposed configuration keys

These keys form a draft schema, not executable configuration accepted by existing software.

| Keys | Required meaning / validation |
| --- | --- |
| integration.backends; integration.completeness_policy | Requested nonempty backend list; explicit fail-request versus report-unavailable policy |
| integration.analysis_target; integration.outcome | Exploratory covariance, categorical prediction or continuous prediction; valid resolver-linked outcome where supervised |
| integration.input_representation | Must exactly match approved resolver record, not initiate a transformation |
| integration.randomization.seed_manifest | Explicit context-keyed seeds/RNG settings for partitions, tuning fits and model restarts; no order-dependent shared stream |
| integration.adapters.concat_baseline.method; integration.adapters.concat_baseline.ncomp; integration.adapters.concat_baseline.center; integration.adapters.concat_baseline.feature_scale; integration.adapters.concat_baseline.block_scale | PCA or approved PLS/PLS-DA method; legal rank; explicit centering and scaling policies/parameters |
| integration.adapters.mixomics_diablo.design | Explicit named connection matrix; order checked against blocks; no inferred default |
| integration.adapters.mixomics_diablo.ncomp; integration.adapters.mixomics_diablo.keepX_grid; integration.adapters.mixomics_diablo.score_definition; integration.adapters.mixomics_diablo.missingness_policy | Configurable dimension/sparsity/score representation; backend pattern eligibility verified |
| integration.adapters.mixomics_diablo.regression.mode; integration.adapters.mixomics_diablo.regression.keepY; integration.adapters.mixomics_diablo.regression.response_scale | Continuous response-matrix and predictor-only extraction policy; explicit inverse transformation |
| integration.cv.unit; integration.cv.folds; integration.cv.repeats; integration.cv.inner_folds; integration.cv.metric | Unit equals declared generalization unit; class/subject feasibility checked; partition seeds referenced from seed_manifest; explicit nested tuning and scoring policy |
| integration.adapters.mofa2.factors; integration.adapters.mofa2.convergence_mode; integration.adapters.mofa2.max_iterations; integration.adapters.mofa2.drop_factor_threshold; integration.adapters.mofa2.likelihoods | Explicit valid training options and likelihood per view; restart seeds referenced from seed_manifest; no Poisson model on signed differences |
| integration.adapters.<backend>.selection.rule; integration.adapters.<backend>.selection.threshold | Separate dense/sparse selection policy for each configured backend; threshold required only when its rule uses one |
| integration.comparison.metrics; null_scheme | Appropriate to target; label/subject/block permutation restrictions stated |

`<backend>` expands to one of the three declared adapter keys; parameters are never broadcast across adapters. Final machine schema must spell every expanded path unambiguously. No generic selection rule or seed silently overrides a backend value. The resolved seed manifest identifies a split/tuning/restart context by stable IDs and records the effective integer seeds plus R/Python/backend RNG settings. If a master-seed derivation is later supported, its deterministic mapping must be frozen and tested before use; iteration order must not affect it. Record every effective backend centering/scaling operation and the incoming A/B scale, avoiding a second standardization unless explicitly configured. A change to one backend's seed/selection invalidates its dependent artifacts, while partition seeds and other fits remain unchanged. Values lacking a scientifically correct default remain explicit and printed in D's report. Species/feature namespaces govern downstream meaning; C can iterate blocks generically without name-dependent branches.

### Errors and attrition

Errors identify the exact config key, block file or offending IDs. Required conditions: sample-order mismatch; duplicate/ambiguous unit identity; incompatible matrix dimensions; one-level supervised Y; continuous/factor mismatch; unsupported missingness pattern; zero observations for a view/sample; disconnected cross-view overlap requiring a reviewed policy; nonpositive residual training size; infeasible grouped folds; absent explicit DIABLO design; invalid sparsity grid; invalid components; unknown preprocessing state; failure/convergence exhaustion; native output unable to satisfy the common contract. A null biological result is not an error. No silently invented labels, pair completion, reordering without a recorded map, or automatic imputation at dispatch.

## 4. Methodological decisions and frontier

| Decision | Proposed answer and classification | Status / dependency |
| --- | --- | --- |
| Shared matrices versus equal scientific signal | Retain common preprocessing/decomposition provenance; mean change, covariance and prediction are different estimands (B-03 inference) | Clarify exact shared-matrix wording with owner before adopting |
| Paired deltas versus sample-level within representation | Either common contrast-unit matrices with only eligible subject-level supervision, or common within matrices with a valid paired inferential model; compare explicitly | Material owner decision, linked B gate |
| Repeated CV | Whole-subject splits with training-only learned processing for new-subject claims | Proposed policy; actual tuning API remains unverified |
| Missing predictors in mixOmics | Official source permits some NAs; a stricter complete-input rule is a pipeline choice | Factual correction established, eligibility policy not adopted |
| DIABLO connections | Preserve mandatory explicit configuration | User decision retained; effective matrix behavior to test |
| View-size balancing | Compare declared policies, e.g. feature standardization with or without block-energy scaling; no universal optimum asserted | Value/operation not selected; cost and interpretability documented in report |
| MOFA multi-seed stability | Align signs/order or compare subspaces; report variability | Number of runs/tolerances unset and must be approved/bounded |
| Planted factor recovery | Distinguish deterministic invariants, high-signal controls and recovery distributions under weak/null/adversarial signals | Newer uncertainty guardrail takes precedence; no guaranteed recovery claim |
| Incremental utility | Compare matched inputs, held-out metrics where applicable and feature/factor overlap; preserve no-improvement findings | Metric and null schemes to freeze before experiments |
| Common scores and variance | Do not flatten native multi-block semantics silently | Protected-contract clarification needed |

No default numerical FDR, loading cutoff, seed count, factor count or CV repetition is declared scientifically established. C does not pool B's p values and does not create p values for loadings by renaming them. Univariate preselection used inside prediction must be trained within folds; selecting features once from all labels is prohibited.

## 5. Worked examples — two designs, illustrative arithmetic only

### Paired two-occasion study

Synthetic subject IDs u1,u2,u3 and declared contrast followup minus baseline. Two blocks each contain one displayed feature with deltas (-1,0,1). The mean delta is 0; those matching deviations can still form shared covariance. A second illustrative feature has deltas (2,2,2): mean change is 2, but centering subject-level deltas removes all variance. These examples explain distinct objectives; no fitted factor or moderated p value is claimed and three subjects are not a recommended study size.

Under a contrast-unit option, the common matrix has three subject rows; no two-level timepoint Y survives. Requesting timepoint DIABLO must return an eligible-outcome error. A real categorical subject-level outcome, if declared and sufficiently represented, would define a different supervised question. Under a within-sample option, centered pairs have values ±delta/2 across six rows and legitimate sample timepoint labels, but inference must account for pair dependence and CV must hold subjects out. The project has not selected one option. Both retain per-block profile differences and expose uncertainty.

### Independent two-group study

Four synthetic samples a1,a2,b1,b2, with explicit categorical outcome A,A,B,B and no subject column. Two feature columns in different blocks have values (0,1,3,4). The equal ranking offers a high-signal illustration for a shared component; sample count is deliberately tiny, so a nested two-fold exercise would leave too few training examples for meaningful tuning. The validator must report fold infeasibility rather than quietly reduce folds. Exploratory PCA can still be shown with rank and uncertainty caveats if explicitly requested; it is not cross-validated prediction. An actual validation fixture uses feasible class counts declared by the test protocol, not these four rows as scientific validation.

A five-block variant changes only the named block collection and explicit connection matrix. It must not trigger a different scientific policy because one display name changes. A continuous-outcome fixture replaces categorical Y with a declared numeric variable and dispatches the approved regression adapter; no numeric-to-factor coercion is allowed.

## 6. Validation and test strategy

The public adapter boundary and CLI integration stage are the main test seams; fixtures test scientific invariants, not implementation structure. Tests use labelled synthetic matrices and independently derived expected quantities. Hold measured data outside the repository; a separate approved measurement benchmark is needed for actual assay validity, biological interpretation and comparative utility.

| Substantive risk | Test / oracle | Required retained evidence |
| --- | --- | --- |
| Sign reversal | Reverse an explicit paired contrast; verify declared change signs, then account for arbitrary factor sign | Contrast map, original and aligned results |
| Sample mismatch | Permute only one block's columns; require keyed realignment with provenance or an error per approved policy | Unit order before/after and error artifact |
| Namespace collision | Reuse the same text ID across two namespaces and blocks | Two distinct feature records through selected/loading export |
| Silent design coercion | One-level factor, continuous Y and repeated singleton cases | Exact key-specific rejection; no fake labels |
| Leakage | Alter held-out outcomes/values; training transforms, feature selection and model fit must remain invariant where the declared deployment target requires it | Split manifest and training fingerprints |
| Imputation/filter order | A fold fixture with held-out-only prevalence and imputed values | A provenance check refuses the wrong processing order |
| Unrecorded attrition | Constant/all-missing feature and failed native fit | Every source feature accounted for by retained/drop reason |
| Seed dependence | Refit approved seed list, align factor signs/order, compare subspaces | Raw per-seed runs, ELBO/convergence and variability |
| Pooled multiplicity | Pass B tables with separate families through C/report | Unchanged q values/family IDs; no C recomputation |
| Missing views | Entry NA versus absent sample/view versus wholly empty block | Per-function eligibility grid and expected errors |
| Response leakage in block.spls | Fit one response/multiple assays; adapted predictor blocks equal original names; replace held-out Y with fixed predictors/model | Predictor scores/predictions unchanged; response diagnostics separately labelled |
| Unavailable versus zero | Isolated input NA, all-NA feature, sparse unselected feature, pruned factor and zero denominator | Exact finite-success/failed-run policy, no zero/NA placeholder repair |
| RNG/selection coupling | Reorder backend requests; change only one backend's seed or selection rule | Identical folds/other models; distinct effective per-backend rules |
| No true signal | Null labels/shared-factor strength zero and block-shuffled controls | Empirical false structure/performance distribution, not compulsory discovery |
| Incremental value | Matched baseline/univariate/integration comparison under prespecified objectives | Out-of-fold paired comparison or factor recovery, uncertainty and null results |

Recovery thresholds and replicate counts are protocol choices to justify and freeze; no thresholds are invented solely to let tests pass. Runtime benchmarks, peak memory and wall time must be measured on the actual host before concurrency is increased. Documentation checks cannot substitute for this table.

## 7. Environment, workflow, artifacts and reproduction

Proposed exploratory workflow: A profile processing → B validated decomposition/unit mapping → C eligibility → requested fits → native-to-common validation → candidate/provenance export → D. Proposed predictive workflow: immutable pre-fit input plus approved A/B public service → C unit-preserving partitions → train-only A fit and permitted held-out apply → B fold-valid representation → C inner tuning/outer scoring → common diagnostics. The latter is blocked until the service and deployment information set are approved and executable. In implementation, baseline precedes DIABLO, which precedes MOFA. Run selection can request a subset only when explicitly configured; build order does not imply every analysis must run every backend.

Environment needs a reviewed R/Bioconductor pair, committed renv lock with exact package sources, Quarto for downstream report, and an explicitly pinned Python/mofapy2 environment for MOFA. A fabricated renv lock is not a reproduction artifact. Source manifests include package release, source URL/hash, operating system, BLAS/thread settings, R/Python versions and RNG modes. User data and generated participant-level scores/models stay in a configured output directory outside Git; versioned fixtures are synthetic only.

Per-run artifacts: resolved config, unit/feature maps, immutable input fingerprints, preprocessing/decomposition manifest, selection/drop reasons, common result, native artifact where permitted, folds, tuning results, per-seed diagnostics, warnings/status manifest and SHA-256 inventory. Checksums detect changed content; they do not authorize public disclosure of data or identifiers. Atomic completion records are written only after required outputs and contract validation succeed.

Restart: cache identity includes input and metadata content, config, transformation/contrast, environment, backend, fold and seed. A partial model is not loaded as successful. Resume native training only when the selected package explicitly supports it and compatibility is verified; otherwise restart that bounded fit from its declared seed. Preserve failure logs and elapsed resource measurements. Publish only code/synthetic evidence and reviewed metadata safe for the private repository.

Planned commands (not currently runnable): `Rscript -e 'sessionInfo()'` for environment evidence; approved testthat entry points for C tickets; package CLI `triomics run --config <synthetic-config> --output <validation-dir>` after A's CLI specification is frozen. The exact executable path and installed command must be verified before ticket acceptance; no CLI exists yet. Record actual commands and exit statuses, rather than treating this notation as a successful smoke check.

## 8. Bounded implementation tickets — proposed, blocked

File ownership is supplied because the execution prompt explicitly requires it, overriding the generic to-tickets preference to omit paths. These are proposed repository-relative paths and must be reconciled with the verified checkout. No issue is published or ready for dispatch.

| Ticket | Dependencies / owned files | Deliverable and acceptance | Tests / restart / review |
| --- | --- | --- | --- |
| C-01 common result and baseline PCA slice | All four reviewed kits, A/B executable contracts; R/integration-result.R, R/integration-baseline.R, tests/testthat/test-integration-baseline.R | CLI produces a valid baseline result and attrition/provenance on synthetic independent and approved paired representations; 2/5 blocks and names independent | Known rank/sign/subspace, order/collision/missingness errors; atomic artifact restart; independent standards+spec review |
| C-02 supervised baseline and evaluation | C-01 plus approved/executable A/B fold-preparation seam; R/integration-baseline.R, R/integration-validation.R, tests/testthat/test-integration-validation.R | Approved PLS/PLS-DA API and common eligible-fold evaluation; train/test outputs separated | Whole-path held-out outlier/prevalence mutation leaves train processing unchanged; deliberately whole-data-fitted control fails; group isolation/infeasible folds; cache includes split; independent statistical review |
| C-03 DIABLO categorical slice | C-02 and verified grouped tuning/score policy; R/integration-diablo.R, tests/testthat/test-integration-diablo.R | Explicit design, legal tuning grid, common score/loadings/variance definitions, selection stability exported | Native-versus-adapter numeric check, outcome mismatch and NA grid; retain each fold result; standards+spec/method review |
| C-04 continuous block.spls slice | C-03 and pinned/verified regression response-matrix/mode/keepY/extraction contract; same adapter with tests/testthat/test-integration-continuous.R | Named n-by-1 scalar response matrix, predictor-only common block outputs; correct held-out metrics and provenance | Response-block exclusion, held-out Y independence, scaling/back-transform, response position and coercion controls; complete-fold resume; independent review |
| C-05 MOFA slice | C-04 and approved Python backend environment; R/integration-mofa.R, tests/testthat/test-integration-mofa.R | Missing-view eligibility, explicit likelihood/factors/seeds, per-run convergence and common result | Known high-signal, weak/null and overlap-degenerate controls; seed-level restart; statistical/source/licence review |
| C-06 comparison and D handoff slice | C-01–05/C-08 and approved D input contract; R/integration-comparison.R, tests/testthat/test-integration-handoff.R | Matched comparison artifacts and all candidates/universes required by D, retaining nulls and invalid-route reasons | No q-value pooling, complete feature accounting and cross-design smoke; input-fingerprint invalidation; final independent review |

Each ticket requires exact test commands, environment and expected file checksums frozen after checkout/environment discovery. C production work starts only after B's validated milestone and proceeds sequentially. Shared-file tickets are sequential, never simultaneously assigned. The orchestrator re-runs gates, reviews diffs, resolves findings, commits/pushes a reviewed branch and updates the exact Notion page. None of those actions is claimed here.

## 9. Decision records, glossary and citation library

Canonical definitions are in root CONTEXT.md. Evidence and citations are in B/C research ledger; A's processing and D's enrichment evidence are linked dependencies. Proposed ADR topics: analysis-unit/shared-matrix semantics; centered repeated inference; common DIABLO score/variance meanings; fold-local preprocessing state; candidate versus enrichment evidence contract. No accepted ADR is invented while these tradeoffs remain unresolved.

C's selected feature names remain scoped to the originating block and namespace. A factor is a model-dependent variation axis, not an effect-size estimate or pathway. A prediction target, a contrast and an analysis unit must remain separate terms in all reports.

## 10. Readiness checklist and limits

- [x] Scope, build order, protected fields, known scientific conflicts and proposed seams documented.
- [x] All three backends retained; two study-design examples explicitly labelled illustrative and unrun.
- [x] Bounded implementation tickets and substantive risk tests proposed without false pass claims.
- [ ] Recover/reconcile historical architecture and exact private repository/Notion/host references.
- [ ] Maintainer confirms R stack and approves additional dependency graph where needed.
- [ ] Resolve analysis-unit/shared-matrix, common score/variance and CV/selection policies through reviewed decisions.
- [ ] Inspect all current entry-point signatures/licences and pin compatible source/environment artifacts.
- [ ] Execute API missingness, grouped-fold and result-contract checks; attach logs and checksums.
- [ ] Independently review the complete package and resolve findings; approve ticket boundaries and seams.
- [ ] Commit/push the ready package to verified private repository and synchronize Notion with verified links.

Final disposition: a concrete provisional design for further grilling, with material blockers. It is neither a validated integration pipeline nor a completed implementation-ready specification kit.
