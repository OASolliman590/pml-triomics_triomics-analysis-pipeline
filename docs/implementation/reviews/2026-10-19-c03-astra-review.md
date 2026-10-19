# C-03 Astra review — 2026-10-19

Read-only review of adapter commit `9758b4a`. Thread `01a15375-51b5-7940-880d-e90c14552e73`. Model `gpt-6-astra`, effort low. The reviewer did not rerun tests and did not edit the adapter.

Verdict: `changes_required`. Critical findings: none.

Important:

- AC2. Common loadings are the native loadings. The frozen consensus signs are applied to scores only.
- AC3. Prediction returns native class labels. Held-out block variates are not passed through the stored consensus transform.
- AC4. Variance is copied from `prop_expl_var` without a recomputed assay-space, denominator, and per-component semantics check.
- AC1. The suite records a tuning label. It does not show that the adapter fits the supplied candidate once and does not call `tune.block.splsda`.

Note: the centered-rank guard uses the first predictor block only. Later component-map checks still fail closed. This review does not accept C-03 and does not authorize `mixomics_diablo` registration.

Sol's earlier rerun remains the test evidence: 57 passed, and the stage C fixture exited 2 with `C_SCORE_BACKEND_UNAVAILABLE` before ingest.
