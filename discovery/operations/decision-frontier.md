# Grilling decision frontier

## Settlement update — 2026-09-06

[The adopted contract settlement](contract-settlement.md) and [Astra arbitration](../reviews/astra-design-arbitration.md) resolve the shared design/statistical alternatives below. DEC-02–12 and CL-ADR01–04 have bounded design dispositions; older pending labels are historical. R core plus Python is approved. Real-study/user parameters, rights, empirical tests and provisioning remain separate gates.

## Clinical scope approved — 2026-09-06

The user confirmed the revised clinical-association scope with “yes”. This scope is accepted and must not be requested again. See [approval record](clinical-scope-approval.md). Exact shared contracts/methods, clinical meaning/linkage and original implementation prerequisites remain open; this approval is not a model/test result.


Status: proposals and unresolved questions, not accepted ADRs. Evidence date 2026-09-05. Source facts are in the A, B/C and D ledgers; their classifications and limits govern these summaries. No questionnaire substitutes for research. Questions answerable by source/API inspection remain autonomous work; only material tradeoffs, institutional entitlements and missing references go to the maintainer.

## Three-study review decisions — 2026-09-06

Claude Opus 5 reviewed the prior drafts against the source audit. The [revised proposal](../spec-drafts/three-study-revision-proposal.md) is the concrete confirmation surface; the [response](../reviews/opus-review-response.md) qualifies the raw review. Proposed choices: mandatory manifest with audited external conversion first; source/selection claim gates; explicit group-by-visit estimands and protected representation choices; missing-pair refusal default with opt-in complete-pair attrition reporting; typed aliases without new routes. They extend A-D01/D06/D09, ADR-B-01/02/04/05, C eligibility decisions, D-ADR01/03/04/05 and X-R01; none is adopted. New D-Q12 is PubChem research, not approval. The supplied source folder establishes study-data location only; R and other prerequisites remain open.

## General target investigation — additional proposal

User requested target-agnostic investigation. Proposed D-ADR07 adds named-target assessment, candidate comparison and bounded exploratory discovery; D-ADR01/03/04 own evidence, identity/context and any target-level inference. See ../spec-drafts/D-target-investigation-extension.md. Descriptive evidence profiles require no invented binding probability; quantitative ranking/testing remains a separate reviewed method. No need to name one drug/target to design the generic service. Scope adoption, resources and implementation readiness remain open; existing five three-study decisions and all original prerequisites remain.

## Clinical metadata association — additional proposal

User requested granular objectives from each Study's metadata and clinical–molecular association. Proposed CL-ADR01–04 cover dictionaries/linkage/time, objective/model/representation and service ownership, selection/multiplicity/missingness, and score/prediction/report context. See ../spec-drafts/clinical-molecular-association-extension.md and the local audit. Source-specific requirements are a verified clinical–omics crosswalk, row-level visit resolution, variable units/codebook, source version/derived-index provenance and primary objective/adjustment choices. Organophosphate/Diabetic clinical tables remain unlocated after an embedded/hidden-sheet scan; group/visit labels alone cannot supply absent clinical endpoints. No default causal effect, all-column covariate set or universal correlation method is accepted.

## Prerequisite tree

1. Restore prior handover and exact repository/Notion/host → reconcile historical numbering, accepted decisions and execution resources.
2. Confirm R stack → resolve a compatible R/Bioconductor and MOFA Python environment → approve additions → run critical API checks.
3. Clarify measurement scale and processing policy → explicit BlockProfile behavior and fold-local processing contract.
4. Clarify shared-matrix/analysis-unit contract → valid repeated-design model → eligible supervised/unsupervised targets and common scores.
5. Clarify candidate versus enrichment evidence → exact resource-specific input, licensing and unavailable-route design.
6. Complete and independently review all four kits → approve actionable tickets/fleet → push ready specs → sequential production milestones.

## Material decisions with evidence ready for discussion

| ID | Proposed answer | Evidence classification and consequence | Status |
| --- | --- | --- | --- |
| DEC-01 technology | Propose R 4.6.1/Bioconductor 3.23 as the first reference environment; released MOFA2 1.22.0 declares Python 3.12.12/mofapy2 0.7.3 | User default; current dependencies have higher floors. runtime-compatibility.md separates inspected release pins from development/PyPI versions. Metadata and upstream builds do not establish local installed compatibility or a full lock. | Explicit confirmation requested, unanswered; no dependency provisioned |
| DEC-02 missingness | Keep distinct profiles, but make missingness mechanism and acquisition provenance explicit instead of assigning it universally by assay label | A-Q01–03: primary studies show multiple mechanisms; imputation bias direction depends on imbalance and assumptions | Qualification to settled rationale proposed, not adopted |
| DEC-03 lipid normalization | Distinguish class-standard correction, sample-amount normalization, class-total closure and variance scaling; preserve concentration totals for abundance questions; closure/CLR are explicit compositional endpoints | A-Q04–06 and independent A review. Plasma guidance discourages class-total normalization for concentration reporting; class-total closure mathematically erases uniform class abundance change | Material policy clarification pending |
| DEC-04 profile learned state | Declare how training-fitted preprocessing state is carried/applied in CV without changing held-out data into training evidence | A-Q09 / C-04 inference; current six-function return contract does not explicitly carry fitted state | Interface clarification pending |
| DEC-05 shared matrices | Retain one authoritative transformation/provenance path; define whether common rows are samples or contrasts and how statistical model projection operates | B-01–03. Mean changes, covariance and prediction are not equivalent targets; paired deltas have no varying occasion label | Protected semantic decision pending |
| DEC-06 repeated inference | Investigate fixed-subject designs on the common centered matrix as one alternative to naive centered-response duplicateCorrelation; contrast-coordinate modeling is another | B-02 algebra/source concern. Requires correct design rank, transformed covariates and covariance; no executable equivalence check yet | No alternative accepted |
| DEC-07 NA eligibility | Correct the claim that mixOmics universally needs complete matrices; choose a verified per-pattern support policy, possibly stricter than native support | C-01 static source permits predictor NAs; all-NA views/tuning/prediction remain untested | Factual rationale correction; policy pending |
| DEC-08 common result meanings | Explicitly define a single DIABLO score matrix and backend-specific variance/selection semantics | C native variates are per block; dense loading cutoffs are choices | Contract clarification pending |
| DEC-09 enrichment evidence | Retain eligible universes/ranks and declared quantitative inputs where required; keep native backend internals out of D unless an approved contract explicitly exposes a service/artifact | D-R03: MOFA native enrichment needs model/data/annotations, BioPAN needs compatible quantitative comparison | Interface extension or route limitation pending |
| DEC-10 resource entitlement | Open each annotation route only with exact applicable terms, identity/coverage and executable checks; D still reports route status | D-Q01–11. Public availability does not imply arbitrary redistribution or confidential uploads | Institutional KEGG/HMDB entitlement unknown; local LION/BioPAN artefacts unpinned |
| DEC-11 novelty | Compare documented capabilities at a dated snapshot; claim engineering objectives rather than absence of all prior lipid-aware workflows | D precedent ledger and independent review establish MOI lipid interpretation and OmicsAnalyst lipid examples; scope nuances retained | Correction supported, no architecture reduction |
| DEC-12 synthetic recovery | Separate deterministic invariants, strong positive controls and weak/null recovery distributions | C-07 inference plus explicit user uncertainty guardrail; source inputs internally conflict on guaranteed recovery | Do not inherit the guarantee as a test oracle |

## Defaults that must not be silently chosen

Prevalence scope/threshold; normalization/transform and CLR reference/zero policy; imputation distribution and uncertainty policy; DEqMS count aggregation; contrasts/covariates and incomplete-pair handling; moderated SE/trend/robust settings; within-block multiplicity families; directional consistency for independent/continuous designs; effect-size floor semantics; feature/block scaling; CV target/folds/repeats/metric; DIABLO design/keepX; MOFA factors/seeds/pruning/convergence; loading selection; enrichment background/statistic/set size; missing/ambiguous mapping behavior; output completeness policy. Each draft must name its configuration keys, proposed justification and unresolved status.

## Pending factual work, not user preferences

Pin current function signatures/source hashes and licences; run adversarial small checks once environment/code gate permits; verify grouped tuning; resolve resource versions/coverage and predecessor behavior; inspect source/licence for optional local adapters. Continue this work where accessible without requiring the user to supply methodological facts.
