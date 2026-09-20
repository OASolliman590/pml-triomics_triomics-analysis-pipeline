# Independent review of B design/statistics draft

Reviewed 2026-09-05. Reviewer: annotation/report researcher, not the author of B. Target read in full: `discovery/spec-drafts/B-design-statistics.md`, SHA-256 **47A1AFF5D47AAF5C2009F93BAA73BBC4B6A567EE36DF419F0E796E65E8921564**. Basis: both preserved user inputs, root `CONTEXT.md`, B/C evidence ledger, discovery README, pending validation and current decision frontier; C's shared-unit contract was cross-checked. The B draft was not edited.

**Disposition: retain as a conditional draft; not implementation-ready.** All ten requested categories are present as draft sections. This is a complete-document review, not a finding that every required artefact or scientific decision is complete. Two P1 readiness gates are already honestly acknowledged. Four P2 contract clarifications require action. No hard algebraic error was found in the paired examples or the centered-response fixed-subject proposal under its stated assumptions. No runtime statistical validation, package install, code review of an implemented B module, or licence completion is claimed.

## A. Requirement and contract coverage

### Coverage against all ten requested kit categories

| User category | B location | Review result |
|---|---|---|
| 1. Scope, mapping, precedent, interfaces, claim/proceed limits | §1 | Present. Stable B scope and original contracts preserved; no novelty guarantee. Comparisons are primarily to statistical dependencies, so the eventual integrated pipeline comparison should also link D's corrected MOI/GraphOmics analysis. |
| 2. Dependency/questions, manifest, licence, namespaces, verification artefacts | §2 and B/C ledger | Draft matrix present. Current catalogue versions are correctly distinguished from a locked environment. Executed verification artefacts, full licences/transitive dependency review and compatible lock remain absent and explicit. |
| 3. Exact input/output/config/error contract | §3–4 | Detailed proposal, not exact approved contract. See BR-03 through BR-06 for unresolved output/sensitivity/count semantics. Shared representation is the P1 gate BR-01. |
| 4. Ground truth and scientific validation limits | §5 and §9 | Present. Synthetic invariants/calibration are distinct from real measurements. No planted-factor success guarantee inherited. |
| 5. At least two worked designs with uncertainty | §5 | Paired and independent hand examples present and arithmetically correct. No fabricated moderated p/q. Planned full synthetic blocks must eventually populate real method outputs. |
| 6. Decisions/ADRs, evidence, glossary, citations, unknowns | §6; B/C ledger | Present and statuses explicit. Frontier DEC-05/06 and B's ADR-B-01/02 agree. Mandatory sensitivity's allowed-disable rule needs an explicit disposition. |
| 7. Architecture, environment, hashes, graph, resources, run instructions | §7 | Workflow and provenance/restart design present. Environment lock/container specification, numeric measured resource envelope and real command sequence pending. No runnable guide implied. |
| 8. Bounded tickets/ownership/acceptance/tests/restart/review | §8 | Seven vertical slices present; all blocked. Shared-file sequencing acknowledged. Scientific probe execution and package work must be scheduled separately if probes precede production approval. B-T07's C/D “interface fixtures” should remain contract fixtures, not require later C/D production milestones. |
| 9. Substantive risk plan | §9 | All nine user-named risks covered, plus covariance, SE and pseudoreplication. Direction reversal oracle needs the BR-06 clarification. |
| 10. Reproduction, artefacts, readiness | §10 | Present and honestly incomplete. Independent review can now be recorded, but unresolved findings, environment and tests remain unchecked. |

The `BlockProfile` and `IntegrationResult` signatures are not silently changed. N-block support, namespace identity, separate block FDR and both-branch/both-annotation routing are retained. Missing history, technology confirmation, dependency approvals and publication destinations remain visible. No requirement to put real study data in Git is introduced. A blocked resolver path is not counted as implemented design support.

### BR-01 — P1 readiness gate: shared representation and repeated model still need an adopted contract

**Targets:** §3 “Resolved analysis and result proposal”; §4 “Proposed ADR-B-01”; §8 B-T03/04; frontier DEC-05/06.

The protected requirement is the same decomposed matrices for B/C. The draft deliberately leaves n delta units versus nk sample units unresolved, and correctly notes that the options change model or score semantics. This is an existing gate, not a hidden reversal or new implementation bug. Its significance is that a B-only mathematically valid fit still does not settle C's outcome labels, score rows and CV target.

**Required correction before readiness:** adopt an orchestrator-owned representation/analysis-unit ADR with matrix dimensions, column/row identity, original-to-analysis mapping, C eligible outcomes, exact model/covariance and allowed deterministic coordinate views. Link each supported resolver row and B-T03/04 to that ADR. Run BC-V02/V03 under the chosen environment. Neither this review nor the existence of Option B selects it.

### BR-02 — P1 readiness gate: no locked executable dependency/calibration evidence yet

**Targets:** §2 “Remaining verification”; §5 calibration; §7 environment prerequisites; §10 checklist.

The package cannot be treated as ready while limma/DEqMS/container APIs and licences are unpinned, time-series/rank-test methods unresolved, and all BC-V01–04 checks unrun. The draft says this correctly. A numerical catalogue version or static release-branch source is not an installation/compatibility result. R >=4.3 is not a complete supported stack.

**Required action:** approved host/technology and dependency inventory; resolved R/Bioconductor versions and lock; actual signatures/source/licence hashes; reference calculations, NA/count/design checks and prespecified null-calibration evidence. Keep scientific validation probes visibly separate from production implementation. A “reviewed draft” label must not enable the tickets before these applicable gates are satisfied.

### BR-03 — P2 requirement gap: disabling a required rank sensitivity needs explicit policy

**Targets:** §3 configuration `statistics.sensitivity`; §3 “Required sensitivity incompatible with design”; §8 B-T06; §6 ADR-B-05.

The supplied execution prompt requires rank-based metabolomics sensitivity and reporting disagreements. The draft admits `Disabled` while saying the profile-required policy is pending, and lists either an error or an approved unavailable status. This is properly proposed but not sufficiently determinate for an implementer: a disabled setting could silently bypass a required built-in profile behaviour.

**Suggested correction:** define a `required_by_profile` capability, valid methods by estimand/design, and the exact disabled/unavailable/error decision. If sensitivity cannot preserve the covariate or repeated-design estimand, report that limitation and require the already specified narrowing/ADR rather than selecting an unadjusted test. Ordinary user configuration must not masquerade as an accepted removal of a required analysis. Pin the method, null, tie/zero behaviour and disagreement definition before B-T06 starts.

## B. Scientific and evidence findings

### Confirmed mathematics with bounded interpretation

For §5 Example 1, deltas [1,2,3,2] have mean 2, SSE 2, variance 2/3, ordinary SE sqrt(1/6) and df 3. The fixed-subject fit's variance 1/3 times unscaled coefficient variance 1/2 yields the same SE. Example 2 has effect 3, pooled variance 1, SE sqrt(2/3) and df 4. These were independently checked algebraically; no R execution occurred.

For §4 Option B, write the subtracted subject vector as Xc with c nonzero only on subject columns. Fitting y-Xc with the same full-rank X and fixed weights changes fitted coefficients from beta-hat to beta-hat-c and leaves residuals and the non-subject coefficients unchanged. The stated ordinary SE/df invariance follows for the same design, weights and residual variance calculation. This supports the proposed inference without endorsing arbitrary residual correlation, feature-dependent eligibility or a different weighted fit. The subject columns must stay in the fitted model. The B/C ledger's statement that response centering alone does not justify arbitrary untransformed covariates is compatible with this exception because Option B retains subject nuisance columns.

Primary source recheck: [limma RELEASE_3_23 lmfit.R](https://raw.githubusercontent.com/bioc/limma/RELEASE_3_23/R/lmfit.R), `lm.series` common versus feature-specific QR paths and `gls.series`; [ebayes.R](https://raw.githubusercontent.com/bioc/limma/RELEASE_3_23/R/ebayes.R), `.ebayes`. They implement distinct OLS/weighted/GLS paths, feature-specific residual df where needed, and variance moderation. The draft's posterior-SE denominator and trend caveat are consistent with these inspected sources. They remain release-branch static evidence, not checksum-pinned installed behaviour.

The df `(n−1)(k−1)` expression applies to complete balanced visits and full occasion contrasts under the specified fixed-subject model. It is not a generic df for arbitrary trajectory bases, missing visits, covariates or random slopes. The draft qualifies this adequately. No error found requiring reinstatement of naive subject-centering plus duplicateCorrelation.

### BR-04 — P2 schema gap: scalar contrasts and multi-df omnibus tests need different result semantics

**Targets:** §3 per-feature/contrast result table and `SE detail`; §4 independent >2-group row; §8 B-T02.

The draft mentions distinguishing multi-df omnibus tests, but its only result schema requires scalar `effect`, `SE`, directional consistency and a candidate rule combining effect floor with q. An omnibus test of a vector of coefficients has no unique signed scalar effect/SE or direction. The planned acceptance criterion “not mislabeled directional effect” does not supply a schema.

**Suggested correction:** either constrain this release to explicitly declared one-df contrasts and mark omnibus unsupported, or propose a discriminated `test_type` with null/coefficient-set identity, statistic/distribution and numerator/denominator df; omnibus rows need explicitly inapplicable scalar effect/SE/direction and a separately reviewed candidate rule. This is a design decision, not licence to fabricate a representative fold change.

### BR-05 — P2 schema ambiguity: separate actual observations, analysis rows and independent units

**Targets:** §3 result field `n_used`; §4 missing/covariate eligibility; §9 pseudoreplication test.

`n_used means independent/resolved analysis units` conflates two quantities. Under Option B there can be 2n fitted sample rows but n independent subjects; under a delta representation there are n rows. Feature-wise missingness can change observed rows, complete contrasts and design rank. A single slash-separated definition lets adapters report different quantities under the same column.

**Suggested correction:** define `n_used` once (with a `n_used_definition`/unit tag), and independently require `n_analysis_rows`, `n_observed_input_values`, `n_independent_units`, relevant complete pairs and imputed counts for each feature/test. Existing `n_samples_used` and `n_subjects_used` can implement this without redundant new columns if their definitions are exact. Imputed values counted in a fit must not be described as additional observed measurements. Distinguish unavailable counts (NA) from actual zero. Tie sample inclusion to the exact fit mask and count eligibility, not only the study-wide map.

### BR-06 — P2 metric/test ambiguity: directional agreement needs a scale/mask and reversal invariant

**Targets:** §4 “Covariates, direction and lipid-feature families”; §3 directional fields; §9 Sign reversal.

The draft appropriately proposes NA outside meaningful within-subject contrasts and labels covariate-adjusted comparisons as unadjusted paired sign agreement. It has not defined whether “raw eligible subject differences” means raw measured abundance, A-processed noncentered data or completed/imputed values. With imputation or compositional transforms, these count different scientific objects. The declared zero policy also needs an effect-equals-zero case.

**Suggested correction:** bind the metric to the approved scale/representation and an observed/imputed contrast mask, reporting observed-only and imputation-involving counts separately or selecting one explicitly. Define all-zero/no-eligible cases. In the reversal test, reversing both contrast differences and effect should swap positive/negative counts while leaving sign-*agreement fraction* unchanged; it should not flip the fraction itself. Reword “effect/direction flips” accordingly. NA outside paired designs is a proposed requirement interpretation needing explicit maintainer/ADR disposition, not a numeric compliance workaround.

### Multiplicity, SE and count-dependent inference

The draft correctly preserves separate blocks and warns that partitioning within-block families does not control FDR over their union. The family choice, inclusion of non-estimable hypotheses, primary versus sensitivity tests, and multiple contrasts still need the promised policy; no family is selected by this review. Report q as the chosen adjusted-p measure with family scope rather than inferring global FDR over all displayed findings.

The proposed limma moderated SE matches the test denominator in inspected `.ebayes`; ordinary SE is separately named, and zero-coefficient division is rightly rejected. Multi-df results need BR-04, and DEqMS SE must be verified against its actual count-adjusted outputs before use. The DEqMS delta prior is correctly conditional: algebra of response centering does not establish validity of a count-dependent variance prior after a changed response representation. Metadata alignment, invalid/partial counts and count aggregation remain unresolved scientific/API gates, not opportunities to fill counts with one.

## Review completion and remaining disposition

The ten-category draft coverage is sufficient for documented grilling and targeted revision. It is not sufficient to mark a production ticket ready. BR-01/02 remain P1 readiness gates; BR-03–06 are concrete P2 refinements. No production changes, tests, benchmark outcomes, environment restoration, repository push or Notion synchronization occurred in this review.

After the author/orchestrator records responses, recheck the changed contract sections and the accepted representation with C and downstream eligibility/ranking needs with D. Do not repeat unrelated checks without a change or unresolved concern. Keep the target's original SHA with this review; later edits need a new revision/hash and an explicit findings disposition.
