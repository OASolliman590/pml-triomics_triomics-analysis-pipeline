# Sol second-round rebuttal to Opus

Date: 2026-09-06. This responds to [Opus’s first-round position](opus-design-position.md). It is a settlement recommendation, not a claim that the user has adopted it. My first-round decisions stand except where refined below.

## Agreements that should close now

I agree with Opus on provenance-defined profiles; typed, reversible identity maps; separation of clinical and omics missingness; abundance versus compositional lipid Analyses; fixed parent universes for derived endpoints; B-owned clinical fits; no universal correlation fallback; provenance-based DEqMS dispatch; per-Block FDR; both B/C nominations reaching both compatible D routes; local/pinned resource gates; and honest unavailable states. I also agree that naive subject-centering plus ordinary `duplicateCorrelation` should not be a production default.

The protected six BlockProfile signatures and IntegrationResult top-level fields remain unchanged. The necessary semantic decisions are the per-Analysis representation, fit/apply state transport and exact meanings of DIABLO scores/variance/selection.

## 1. D1 overgeneralizes the analysis unit

Generality does not prove that every repeated Analysis should have `nk` sample-coordinate rows. The protected rule requires one common B/C representation **per Analysis and preparation context**, so different estimands may correctly use different Analyses.

For a two-occasion within-person change or group difference in change, signed subject deltas are the sufficient, transparent common representation: one row per independent subject, direct direction, direct complete-pair attrition and an ordinary one-sample/regression model. They do not need to represent a level-at-visit estimand; that is a separate mean-retaining Analysis. They are reproducible for a new subject when both measurements are part of the declared deployment information set. They are correctly ineligible for future-visit prediction when the second measurement is unavailable.

**Settlement:** two-occasion change uses n-row deltas; between-person/level questions use a distinct mean-retaining Analysis; >2-visit within-person integration may use centered sample rows. The executable equivalence check against an `nk` fixed-subject fit is valuable validation, not a reason to discard the cleaner primary representation.

## 2. D2 needs a narrower support claim

The centered-response/fixed-subject algebra is useful, but Opus states the residual-df formula too broadly. `(n-1)(k-1)` holds for a complete balanced simple occasion design. Unequal visit counts, additional estimable covariates/interactions, weights and feature-specific missingness change rank and degrees of freedom. Fixed subject columns also do not solve heteroskedasticity, serial correlation or random slopes.

**Settlement:** use delta inference for two occasions. For >2 visits, allow centered sample rows plus fixed subject columns only when the exact design matrix is full rank for that feature and the declared residual model passes calibration. Compute df from the fitted design, never from the shortcut formula. Irregular time or unsupported covariance returns `B_REPEATED_MODEL_UNSUPPORTED`; it remains an explicit experimental branch to reopen with a specified model rather than canceling trajectory scope.

## 3. D4’s state flow is incomplete as written

An input `params` object cannot itself return newly fitted state. The contract-preserving mechanism must say where the state emerges.

**Settlement:** in fit mode, a stateful `(se, params) -> SE` operation places a typed provisional state payload in reserved SE metadata. The outer A preparation service validates it, atomically writes the content-addressed `PreprocessingState` sidecar, and replaces the payload with its digest/reference. In apply mode, the service resolves the sidecar and passes the immutable state through `params`; the operation may not refit. Filters, normalization references, CLR parts, scaling and supported imputation models all use the same protocol. Methods without a valid inductive apply route remain exploratory-only.

This is an explicit semantic amendment even though the function signatures stay unchanged.

## 4. D5’s example sign rule can corrupt cross-block direction

Flipping each block independently so its largest loading is positive can reverse biologically meaningful relative directions between blocks. A DIABLO consensus needs joint orientation, training-only centering/scaling and a reproducible prediction formula; it is not a native mixOmics quantity.

**Settlement:** choose the anchor by stable `block_key`, not list position. Orient the anchor component by its largest-magnitude loading, then flip every other block’s variate and loadings together so its training correlation with the anchor is nonnegative. Undefined or zero cross-block correlation makes the consensus unavailable for that component. Store the signs, training mean/SD, required-view set and weights. The exported consensus is the equal-weight mean of these standardized variates. A held-out score uses only stored loadings/signs/scales; native block variates remain diagnostics. This rule must pass block-permutation and held-out reconstruction checks before C-03 acceptance.

For `selected`, sparse DIABLO uses executed nonzero support. Dense PCA/MOFA does **not** silently redefine all inputs as selected; an explicit rule is required, and `none` yields an empty selected list and no C nominations. Loading cutoffs remain configured choices, not biological defaults.

## 5. D6 promotes an unverified NA behavior to a default

Static source acceptance of isolated NAs is not evidence that DIABLO tuning, prediction and common-result adaptation support that pattern. Opus correctly names this as the largest factual gap but simultaneously defaults it to permitted.

**Settlement:** preserve native-NA capability in the design, but fail closed for each unverified runtime path. C-03 first executes the release-pinned pattern grid for direct fit, tuning and prediction. Only passing cells become supported capabilities. No pipeline imputation, zero-fill or automatic complete-case deletion is introduced at dispatch.

## 6. D8 does not require another package

I agree on one versioned shared schema and B ownership. If triomics is one R package, a separate “common contract package” adds release and dependency work without preventing cycles. A versioned internal schema module plus serialized public artifacts is sufficient. B must not import or invoke C/D; the orchestrator schedules the later score-association request.

## 7. Multiplicity can be bounded more precisely

Opus’s `all_tests` default is defensible only after its claim scope is fixed. Pooling every feature × every contrast within a Block can answer a broader question than the scientific objective and can make adding an unrelated contrast alter existing q values.

**Settlement:** the initial family is all eligible features within one `(Study, Analysis, Block, scalar_contrast, claim_scope)`. Multiple contrasts are separate predeclared families unless an approved joint claim explicitly groups them. Their candidate union carries no combined FDR claim. Adding another Block never changes existing q; adding a contrast creates a new family rather than retroactively changing one.

Opus leaves failed-fit handling unresolved. Adopt the concrete rule from my first round: for an otherwise valid fixed family, BH uses valid p-values for estimable members and an internal p-value of 1 for failed/non-estimable members. Failed rows publish q=NA plus `adjustment_placeholder=1`; all declared/evaluated/failed counts remain. With unresolved selection, family or model validity, release no q.

## 8. Resource and language corrections

The evidence ledger supports the statement that LION/web forbids confidential/proprietary uploads. It does not establish the same terms claim for BioPAN. BioPAN hosted use is excluded here by the project’s no-upload rule and unverified source/data rights, not by an uncited assertion that its terms contain the same prohibition. The local BioPAN route remains conditional on source/data licensing and a compatible quantitative design.

R remains the best core choice because the selected method graph is R/Bioconductor-native. Python could bridge to R packages rather than reimplement them, so “Python necessarily invents estimators” is too strong. It would, however, create more cross-language container, error, state and equivalence boundaries while MOFA already supplies the one necessary Python core. Therefore retain the R-core recommendation with isolated release-pinned Python MOFA. User confirmation remains pending.

## 9. Ticket and command settlement

Use existing IDs: A-T01/A-T02 for schemas and ingest; A-T07 for fit/apply; A-T08 for reproduction/handoff; A-T09 for clinical ingest; B-T03/B-T04 for repeated designs; B-T06 for per-Block families; B-T08/B-T09 for clinical families; C-03 for DIABLO; C-08/C-09 for clinical score/prediction; D-T04 for enrichment evidence; D-T15 for the clinical report. B-T07 produces a Markdown method/reproduction record at milestone 4; cross-cutting Quarto vignettes remain milestone 9.

Freeze the future CLI as `Rscript inst/exec/triomics --config <path> --stage <A|B|C|D|all> --output <path>`, with `--validate-only` for non-computing schema/source/capability/estimand validation. Compute mode runs only the requested eligible stage. Invalid input, unsupported/unavailable capability, partial completion and execution failure must be distinguishable. These are acceptance commands for scripts/tests the implementer must create; no command or test is claimed to exist or have run now.

## Result of the exchange

The decisions can now be adopted without another proposal cycle: analysis-specific representations; delta for two-occasion change; guarded fixed-subject centered route for >2 visits; concrete state sidecars; jointly oriented, training-scaled DIABLO consensus; fail-closed NA capability grid; typed source/feature/clinical contracts; fixed per-contrast per-Block families with conservative failed-fit accounting; full D evidence; and conditional local resources. Runtime/API/calibration gates remain validation work, while language, institutional entitlements, clinical meanings/linkage, objectives/adjustments and disclosure threshold remain user or institution facts.
