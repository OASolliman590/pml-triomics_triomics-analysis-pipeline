# Independent artifact review — C integration draft

Review date: 2026-09-05. Reviewed artifact: `discovery/spec-drafts/C-integration.md`, **SHA-256 1CE5C5C7FEB2E2E0A9EFF7ADAD71AB2EA5FE19F0D21B2D9D0CFE88DBD55A938F**. Hash was measured before and after reading; both matched. References below use that snapshot's one-based line numbers. Reviewer did not modify the target.

This is a complete document/specification review with no Git baseline. It is not a code-review CLI execution, implementation review or runtime-validation report. No package was installed, no model fitted and no statistical test executed. The whole C document was read, alongside the preserved user requirements, root glossary, decision frontier and B/C evidence ledger. A first-party `block.spls` source file was additionally inspected as described below. No review approval or ready-for-agent status is inferred from this artifact.

**Disposition:** structurally full across the ten requested kit categories, with material blockers accurately disclosed; **not implementation-ready**. The four corrections below should be incorporated before the next complete-package review. Two affect scientific validity if implemented without an explicit resolution, and two affect the common contract/reproducibility. The review does not select a representation, score aggregation, variance formula or preprocessing API on the maintainer's behalf.

## A. Requirement and protected-contract coverage

| User kit category | Location / coverage | Review result |
|---|---|---|
| 1. Scope, architecture, precedent, interfaces, claim limits and proceed/narrow/defer | §1 | Present. All three backends and required build order retained. Configured run subsets are distinguished from abandoning implementation scope. Engineering claims appropriately limited. |
| 2. Dependency/question matrix, version/licence manifest, namespaces and verification artifacts | §2, §7 | Present as a provisional manifest. Current catalogue versions are not misrepresented as installed/locked. Continuous adapter and baseline entry-point inspection remain factual work, not user preferences. C-R02 adds source-resolvable detail. |
| 3. Exact input/output/config/invariants/errors | §3 | Protected six fields retained. Common scores, variance and selection are explicitly blocked. Input and output details still need C-R01–R04; therefore structural coverage is not an exact approved machine contract. |
| 4. Validation and synthetic/real-data limits | §6 | Present. Planted structure is not guaranteed recovery; synthetic evidence cannot establish real assay validity. Nulls and weak-signal outcomes are retained. |
| 5. Two study designs with uncertainty | §5 | Present. Arithmetic illustrations make the mean-change/covariance difference clear and do not pretend to be executed models. Paired labels are handled correctly. |
| 6. Decision log, ADRs, evidence, glossary and open questions | §4, §9 | Present and linked to decision frontier. No accepted ADR is fabricated. Score/variance/decomposition decisions remain proposals. |
| 7. Architecture, versions/environment, provenance, graph, resource/run requirements | §7 | Present provisionally as a textual workflow and environment/resource requirements. No actual environment lock or benchmark exists; correctly a readiness blocker. Exact host resource limits and command semantics must be filled after discovery. |
| 8. Bounded tickets, dependencies, ownership, criteria/tests/restart/review | §8 | Six sequential slices have owned files and useful acceptance scopes. C-R01 and C-R02 need named ticket gates/oracles. No tickets are published or falsely ready. |
| 9. Substantive-risk test plan | §6 | All user-listed risks covered. The held-out mutation test is valuable but must execute through a fold-replay contract, not only inspect an already transformed matrix. Add response-block and output-NA tests below. |
| 10. Reproduction guide, expected artifacts and readiness | §7, §10 | Present, honestly pending. `triomics run ...` is explicitly hypothetical; it is not a verified CLI. Do not mark the reproduction gate complete until actual commands/output paths/exit statuses exist. |

Protected architecture checks pass at the proposal level: same authoritative decomposition remains subject to explicit semantic clarification; no second within decomposition; no fabricated timepoint labels for subject-level deltas; no display-name dispatch; N-block support; block/namespace-qualified feature joins; no C recomputation/pooling of B's q values; D cannot reach into native backend objects; no real participant data in Git. The draft correctly distinguishes source-advertised predictor NA support from a verified missingness-pattern policy.

## B. Actionable scientific/evidence corrections

### C-R01 — [P1] The predictive-evaluation input contract cannot replay A/B processing within folds

**Target:** lines 51–55 (input envelope), 106 and 117 (CV rules), 139–140 (leakage/order tests), 154 (workflow), C-02/C-03 tickets. **Type:** scientific validity and cross-component contract gap. **Status:** unresolved; DEC-04/DEC-05 already acknowledge the issue, but the C input contract has not made the required execution context concrete.

The envelope supplies already processed/decomposed matrices, masks and fingerprints. Those can audit a whole-study transformation but cannot reconstruct normalization/imputation/feature-filter parameters fitted on training observations alone. Running a second C scaling step within folds cannot remove information already learned by A/B from held-out observations. The workflow currently places completed A/B processing before resampling, while its statistical rule requires learned operations within resampling. This is a consequential gap if a developer treats the proposed envelope as sufficient for a prediction claim.

**Correction:** define two explicit proposed contexts: exploratory fitting on the completed authoritative resolver result, and predictive evaluation requiring a reviewed public A/B fold-processing/replay seam plus eligible pre-fit inputs/state. Specify who owns fit/apply, how per-fold common matrices reach B/C, which operations use the held-out subject's available repeated measurements, and what the declared deployment target permits. Until the seam is accepted and implemented, mark predictive evaluation ineligible; do not imply that a provenance fingerprint alone makes it valid. Preserve the existing BlockProfile signatures until an ADR explicitly changes or supplements their contract.

**Acceptance evidence:** C-02 must be blocked on the accepted fold-local contract. An end-to-end held-out-only outlier/prevalence fixture must leave training filtering, imputation, normalization, scaling and model artifacts unchanged when the deployment assumptions require that invariance. The same test must demonstrate that a deliberately whole-data-fitted preprocessing path fails. This is a proposed test, not an executed finding about existing software.

### C-R02 — [P1] Continuous block.spls needs explicit response shape and predictor-only result extraction

**Target:** line 39, input/output table lines 61–65, score discussion line 70, C-04 ticket line 173. **Type:** documented API/source fact plus scientific-contract implication. **Status:** source-answerable detail now established; installed behavior remains pending.

New inspection: [mixOmics maintainer source, `R/block.spls.R`](https://raw.githubusercontent.com/mixOmicsTeam/mixOmics/master/R/block.spls.R), moving `master`, read 2026-09-05. Documentation lines 43–50 describe a continuous **matrix** response and `keepY`; signature lines 93–106 exposes `Y`/`indY`, `keepY` and `mode`. Lines 135–155 retain `result$A`, `variates` and `loadings` with a recorded `indY`. The response is therefore a distinct internal block whose role must be identified, rather than blindly treating every returned block as a predictor assay. This source has not been pinned to the catalogue release or executed.

The current condition “Y stays numeric” is too weak to guide this adapter. A developer could reuse categorical extraction assumptions and accidentally include response-derived variates in common score aggregation, export Y loadings as an assay, or count Y in explained-variance/selected outputs. That would make the common representation depend on a response that is unavailable for prediction and violate the original per-input-assay contract.

**Correction:** document the proposed single-response n×1 matrix conversion and named response dimension; explicitly address whether multi-response is in or out of the approved scope. Record chosen regression `mode`, outcome scaling/back-transform and `keepY` policy. Use the verified response position/name to extract predictor-only scores/loadings/variance/selection; retain response diagnostics separately. Common scores for prediction must be computed using available predictors and training-derived quantities. Do not use held-out Y to construct the exported predictor score. Call shape and output extraction must be verified against the installed pinned version before acceptance.

**Acceptance evidence:** C-04 gains a fixture with one response and multiple assay blocks; its adapted output must contain exactly the original assay names, never the response block. With fixed fitted model and predictor rows, replacing held-out Y must not alter predictor-derived common scores or predictions. Include matrix-shape, outcome-scale, `mode` and response-position tests. The current unverified-signature warning remains until these run.

## C. Actionable common-contract and reproducibility corrections

### C-R03 — [P2] Define per-field nonfinite/NA eligibility and the meaning of unavailable versus zero

**Target:** lines 61–68 and 72–74; errors paragraph in §3; missing-view tests in §6. **Type:** schema completeness. **Status:** open.

The draft distinguishes failed runs from successful IntegrationResults, but “numeric matrix” allows NA/NaN/Inf in R and does not define whether missing-input support permits missing adapted scores/loadings/variance. Structural missingness, zero loading for an unselected retained feature, undefined variance from a zero denominator, a pruned component, and a non-converged fit need different representations. A general adaptation-failure sentence does not tell a downstream reader whether NA is valid scientific unavailability or an adapter bug, or whether a placeholder zero was inserted.

**Correction:** add a field-by-field policy for finite values, dropped/pruned components, omitted rows versus full retained-feature rows, zero versus NA, and required reason/status artifacts. Choose/review whether any successful result permits unavailable entries; otherwise fail adaptation explicitly. Keep unavailable backends in the run manifest, not a success object filled with NA. Selection vectors must be subsets of eligible feature keys and distinguish empty valid selection from unavailable selection.

**Acceptance evidence:** round-trip fixtures for isolated input NA, all-NA feature, unselected sparse feature, pruned factor and zero-variance denominator; verify exact common output and reason ledger. Tests must not impute or clip these outputs merely to satisfy dimensions.

### C-R04 — [P2] Scope configuration and effective-parameter provenance per backend and per resampling context

**Target:** configuration table lines 81–92; `params` row line 66; restart definition line 160. **Type:** reproducibility/schema completeness. **Status:** open.

The schema allows several backends but provides one generic `integration.selection.rule/threshold`; dense PCA/MOFA selection and sparse nonzero selection may require different rules in the same run. Similarly, global seed/RNG, CV split seed and MOFA seed list are present without an explicit precedence/scope rule. The table notes that shorthand must be expanded, but it does not yet state whether a single generic value is broadcast, rejected on incompatibility or overridden. That leaves two implementations able to interpret the same config differently.

**Correction:** propose a fully qualified per-backend/method parameter mapping or an explicit reviewed override/inheritance rule. Define seed domains (split, tuning, model restart) and keep effective values with each native/common artifact. Declare how a block/view scaling choice interacts with backend-native centering/scaling; save effective operations to prevent accidental double standardization. Do not choose numeric defaults during this correction.

**Acceptance evidence:** one configured multi-backend run using dense and sparse selection must preserve distinct effective rules; changing only one backend's selection/seed policy must invalidate the appropriate artifacts without silently changing other fitted inputs or fold identities. Reordered backend list must not change random splits through implicit shared-RNG consumption.

## D. Scientific topics reviewed without additional defect findings

- **Score aggregation:** the draft correctly blocks silent first-block selection or averaging. A complete future policy must align component signs/order, specify weights/reference, and admit a predictor-only held-out computation. C-R02 supplies an additional regression-specific constraint. No aggregation method is approved here.
- **Variance interpretation:** the draft correctly warns about denominators, percentages versus proportions and nonadditivity. It does not pretend native metrics are directly comparable or clip output. A future formula must state whether variance is defined on imputed/completed or observed entries and whether it describes native per-block scores or the exported common score; these belong in the already declared score/variance ADR and C-R03 acceptance checks.
- **Paired labels:** subject-delta rows cannot be assigned fabricated baseline/follow-up labels; the worked example enforces this. Sample-level within rows may have real timepoint labels but require subject-held-out evaluation and valid B inference. No contrary claim was found.
- **Scope:** all three backends, categorical and continuous mixOmics paths, and the baseline comparison are retained. The supervised baseline should explicitly retain the required PLS-DA categorical objective when its API is selected; “PLS” alone is not a future license to drop that behavior. Currently this is disclosed selection work, not an accepted scope reduction.
- **Synthetic recovery:** strong, weak, null and adversarial scenarios are distinguished. The draft preserves no-improvement findings and does not convert an absent effect into automatic software failure.
- **Tickets/commands:** ownership, sequencing and review are concrete enough for a draft; unverified exact commands and thresholds are visibly outstanding. No nonexistent CLI is claimed to have run. Ticket acceptance must remain blocked until those protocol details and runtime evidence are attached.

## E. Re-review requirements

1. Revise only the affected schema/context/ticket sections to address C-R01–R04, linking unresolved owner decisions rather than accepting new contracts silently.
2. Pin the next document hash and record each finding as resolved-in-document, pending-owner-decision or pending-execution; these are separate statuses.
3. Independently source-check the amended regression adapter semantics and cross-check the fold-local proposal with A/B ownership.
4. Re-run complete-package review before readiness. No document revision can substitute for grouped-fold, missingness, adapter-number and convergence checks in the pinned environment.

The reviewed snapshot is a useful, structurally complete provisional package with honest limitations. It remains blocked; neither the ten-category structure nor this independent review establishes implementation readiness.
