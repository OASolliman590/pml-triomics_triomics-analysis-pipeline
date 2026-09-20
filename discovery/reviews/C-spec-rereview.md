# Independent targeted re-review — revised C integration draft

Date: 2026-09-05. Reviewer: original independent C artifact reviewer. This re-review checks C-R01–C-R04 against the actual revised contract, configuration, tests, workflow and tickets, together with the relevant fold-service, identity and run-state proposals. It is not a runtime check, code review, adopted contract or implementation-readiness approval.

## Reviewed snapshots

| Artifact | SHA-256 |
|---|---|
| Revised `discovery/spec-drafts/C-integration.md`, final status-updated snapshot | **A5BA3CCD73740D9B192B4B0BD5A3FF7CA0E95FEC21F2BF998D8E88B44FA2127F** |
| Supporting `discovery/operations/shared-contract-proposal.md` | **C91D7B2B45A475E7769B7704AB16A8013BC990767755B99FDFC6977A6ACA7423** |
| Original C snapshot reviewed in `C-spec-review.md` | **1CE5C5C7FEB2E2E0A9EFF7ADAD71AB2EA5FE19F0D21B2D9D0CFE88DBD55A938F** |

The method/config/test/ticket text was reviewed at hash **B4AB6E614FD097A4117CBB6C4488E197BA1E904BEA37D2EFCB4AC73F98F62338**, measured before and after reading with a match. The first combined text read was truncated; the intervening decision/example/test/workflow section was read separately to complete inspection. Root then reported an opening-status-only edit; the reviewer reread that opening and measured the final C hash shown above. It now correctly says independently reviewed provisional draft, with final readiness still outstanding, and links the review/response/shared proposal. The supporting proposal hash was rechecked and remained unchanged. The reviewer did not edit the target or supporting proposal.

**Disposition:** all four requested corrections are **addressed at the provisional-document level**. Their proposed resolution is concrete enough for owner/ADR discussion and bounded future checks. None is considered owner-approved or runtime-verified. C remains structurally full but **not implementation-ready**.

## Finding-by-finding disposition

| Finding | Actual revision inspected | Independent disposition | Still required |
|---|---|---|---|
| C-R01 — P1 fold-local A/B replay context | §3 Predictive context correction distinguishes exploratory completed matrices from immutable pre-fit inputs plus public A/B preparation service; C partitions, A fits/applies and B resolves fold representation. §7 now separates exploratory/predictive workflows. C-02 explicitly depends on approved/executable seam and tests held-out outlier/prevalence invariance plus a deliberately invalid whole-data-fitted control. | **Addressed in draft; approval/runtime gate retained.** A fingerprint is no longer treated as a replay capability, and unavailable preparation blocks predictive claims. | Approve callable public seam, exact state/input masks and deployment information set; ensure fit/apply repeats at every applicable outer and inner training split, including feature selection; verify the grouped-tuning path and execute the whole-path negative control. |
| C-R02 — P1 block.spls response shape and predictor-only extraction | §3 Continuous-response correction specifies named n×1 scalar response, `mode`, `keepY`, response scaling/back-transform and response position; multi-response scope explicitly unclaimed. Common predictor assays exclude response variates/loadings; held-out Y cannot construct scores/predictions. Configuration and C-04 encode these gates; test table includes response leakage/assay-name equality. | **Addressed in draft; source-to-installed verification pending.** Numeric-vector ambiguity and accidental response-as-assay adaptation are explicitly prevented by the proposed policy. | Pin source/release, inspect actual native names/positions and callable signature, run shape/extraction/prediction tests and resolve common score policy. Scalar-only proposal remains subject to scope review rather than an accepted narrowing. |
| C-R03 — P2 nonfinite/NA/zero eligibility | §3 Common-value correction proposes finite successful matrices; input NA eligibility is separate from output validity. It distinguishes legitimate sparse zero, excluded feature rows, consistently omitted pruned factors, undefined variance and unavailable selection. §6 includes isolated NA/all-NA/sparse-zero/pruned-factor/zero-denominator cases. | **Addressed in draft; finite-success policy explicitly pending approval.** Failed/unavailable backends cannot be encoded as NA-filled valid results or empty successful selection. | Approve finite-success/attrition/completeness semantics, freeze retained-row/component policy and variance definition, and run all boundary cases on each actual backend. A legitimate null/empty selection must remain distinguishable from estimation failure. |
| C-R04 — P2 parameter and RNG scoping | Configuration now uses `integration.adapters.<backend>.*`, per-backend selection and context-keyed `integration.randomization.seed_manifest`. No broadcasting or implicit overrides; stable context IDs and effective R/Python/backend settings are recorded. Scaling provenance and targeted cache invalidation are explicit. §6 adds request-order and one-backend-change isolation tests. | **Addressed in draft; no numerical defaults chosen.** Multi-backend configuration no longer implies one silently shared selection rule or iteration-dependent RNG stream. | Approve expanded machine schema, context-ID/seed mapping and exact backend RNG behavior; test serial/parallel request-order invariance as applicable and effective-operation/cache provenance. |

## Shared-proposal consistency

The supporting proposal's **Preparation contexts** section agrees with C-R01: immutable pre-filter inputs, public A/B service ownership, explicit held-out information set, and all data-dependent selection inside evaluation partitions. Neither document adopts a new BlockProfile signature or permits C to inspect A/B internals. This is a shared design direction awaiting an actual reviewed service contract.

The **Identity crosswalk** proposes stable feature keys linked to original IDs, namespaces and optional source-row discriminators. C now acknowledges that duplicate original IDs cannot be joined safely by a naive block/namespace/original-ID triple. Its protected output table retains the original semantics while the proposed crosswalk is under review; before a machine schema is frozen, that table must resolve its `selected`/loading names to the single accepted key convention. This is an explicit remaining cross-component approval dependency, not a silently accepted extension.

The **Run state and output-root proposal** distinguishes analysis completion from rendering success and keeps requested unavailable analyses visible. That is consistent with C's unavailable/failed run records and refusal to create a valid IntegrationResult filled with NA. Exact per-backend-to-aggregate status mapping and completeness policy remain to be approved. Rendering an explanatory HTML report does not satisfy a failed mandatory analysis or accept a permanent backend deferral.

The global milestone proposal retains baseline, mixOmics and MOFA and requires B's validated milestone before C production work. Nothing in these revisions silently drops a required backend or turns independent review into a test pass.

## Remaining blockers and claim limits

1. Historical reconciliation, explicit technology confirmation, verified repository/Notion/host and required dependency/fleet approvals remain open.
2. Protected shared-matrix/analysis-unit semantics and B's valid repeated model remain unresolved. Subject-delta rows still cannot receive fabricated timepoint labels.
3. Common DIABLO score aggregation, predictor-only held-out calculation, variance denominators/native-versus-common score interpretation and dense-feature selection policy remain owner/ADR decisions. No policy is selected by this re-review.
4. The fold-preparation service and all nested resampling boundaries need callable signatures, data/state schemas and executable leakage checks. Per-fold wording is not runtime evidence of inner/outer isolation.
5. Current source/manifests must be pinned to a compatible R/Bioconductor and Python/mofapy2 environment; actual missingness, call-shape, grouping, convergence and output contracts have not run.
6. Exact CLI commands, expected artifacts, validation tolerances/replicate protocols, resource envelope and reviewed results remain absent by design at this stage. Synthetic illustrations remain hand-worked, not model output.
7. All complete packages require final independent cross-component review and findings disposition before production readiness; publication/synchronization must be evidenced separately.

No new correction is required to close the **wording/coverage** aspects of C-R01–C-R04 at these snapshots. Their approval and execution portions remain pending as listed. This re-review wrote only this file and performed read/hash inspection; it did not install dependencies, run models/tests, edit other artifacts, publish, or validate implementation.
