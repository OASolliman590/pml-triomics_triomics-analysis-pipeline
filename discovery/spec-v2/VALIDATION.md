# Validation and acceptance protocol

## Three different evidential levels

1. **Specification checks now:** JSON/schema consistency, all 41 identities,
   acyclic dependencies and gates, ownership conflicts, requirement coverage,
   source bytes/hashes for the synthetic examples and bundle integrity.
2. **Implementation acceptance later:** real installed R/native dependency calls,
   behavioral unit tests, numeric oracles, nested-preparation invariants, CLI,
   package build/check and safe offline rendering on actual merged commits.
3. **Scientific/user-data validation:** representative measurements and prespecified
   null/uncertainty/coverage/robustness protocols on the qualified methods. Synthetic
   fixtures and review consensus do not establish this level.

The drafting assistant performed only the specification checks reported in the
bundle validation report. The R command examples below are acceptance requirements,
not recorded successes. Independent Astra review of this new kit is still pending.

## Reference and adversarial test register

| ID / owners | Exact behavior required |
|---|---|
| V01 A01 | Reject duplicate/unknown YAML keys, absent paths, wrong types and requested unsupported stages without numerical dispatch. |
| V02 A02/03 | Permuting columns yields identical keyed values; duplicate sample keys fail; duplicate molecular IDs retain separate measured-feature keys. |
| V03 A02 | Numeric summary/FC/p/q/QC columns cannot be assay samples; native/converted hashes and selection roles are checked. |
| V04 A03/07 | Fit once on training IDs; replace held-out values/outcomes with extreme values, permute test rows and reapply; training state hashes/parameters remain identical. |
| V05 A04/05/06 | Original-observation filtering precedes imputation; unknown masks/mechanisms stay unknown. Double log and wrong-scale normalization fail. |
| V06 A06 | Linear parents [2,3] sum to 5, not log2(2)+log2(3). Weighted property [16,18] gives 17.2. Missing parent changes coverage; zero denominator is unavailable. |
| V07 B01/02 | Independent groups [1,2,3] vs [3,4,5]: target-control=2, ordinary pooled SE=sqrt(2/3), residual df=4. Model exactly the declared scale. Reverse contrast preserves two-sided p. |
| V08 B03 | Subject deltas [1,2,3,4] have mean 2.5, ordinary SE=sqrt(5/12), df=3. Changing source-column order must not change deltas. |
| V09 B01/04 | Compare ordinary fit/coefficient/SE/residuals to independently constructed QR/linear-model calculations; moderated values to direct pinned limma reference, actual per-feature rank/df. |
| V10 B06 | p=[.01,.04,failed] -> internal [.01,.04,1]; BH [.03,.06,1]. Publish failed p/q null, member_count=3. Add 1,000 features in another block; first block's q unchanged. |
| V11 B05 | Permuted, zero, missing and unrelated peptide/PSM counts fail or have explicit qualification; real DEqMS reference outputs execute. |
| V12 C01 | PCA block projection fractions equal nested SSE reductions; sign/column permutation invariance and rank-zero rejection. |
| V13 C01/03/04 | Synthetic native block variates plus flipped signs reproduce the exact frozen consensus; test-set signs/scales do not change state. Missing required views fail, not average fewer. |
| V14 C02/03/04 | Outer and inner splits have no prohibited subject overlap. Folds/feature selection/imputers/scalers are trained locally; native predictions never access held-out Y. |
| V15 C03/04/05 | Execute fit/wrapper-tune/predict/extract capability cells for complete, scattered NA, missing sample-view, all-missing feature and all-view-missing sample cases. Distinguish documented support from tested support. |
| V16 C05 | MOFA fit/save/reload matches within declared numerical tolerance; sign/permutation/subspace-aware seed comparison, actual Python/R lock. |
| V17 B08/09/C08 | Censored/coded/static-conflict/time-misaligned clinical data cannot silently become numeric; training-Y score association is not independent validation. |
| V18 D02/03/04 | Ambiguous isomers/IDs cannot multiply observations; ORA N=10,K=3,n=2,k=2 gives 3/45=1/15. Selected-only background fails. |
| V19 D10/11/12 | FCN3-like nonenzymatic synthetic target has no fabricated substrate reaction; duplicated pathways/adducts/publications do not add independence; no observed Kd is generated. |
| V20 D05/08/13/15 | Optional/required failures differ; all-off report works; malicious HTML labels/path traversal/private fields are safely blocked/suppressed. |
| V21 all | Kill a synthetic run between payload and manifest, resume; partial output is never accepted. Change source/config/resource hash and recompute affected descendants. |
| V22 D09 | Both independent/paired CLI walkthroughs, complete required tests, R CMD build/check, offline Quarto and final requirement reconciliation execute on one final commit. |

Numbers in V06–V10/V18 are hand-checkable arithmetic software oracles, not biological
benchmarks. Include at least one test deliberately injecting each prohibited
condition. A schema-existence assertion is not enough to satisfy those tests.

## Numerical tolerances and calibration

For deterministic algebra/reference checks use atol=1e-8, rtol=1e-6 unless the
pinned API requires an explicitly justified different tolerance. These are
engineering choices, not study thresholds. Compare matching ordinary/moderated
quantities; do not compare ordinary and moderated SE as if identical. Do not
clamp implausible outputs to make the tolerance test pass.

A default **qualification protocol to implement**, not an executed result:
400 independent simulated studies per supported design cell, 100 features, fixed
seed list 1001..1400, complete Gaussian null followed by declared correlated/null,
paired, missingness and confounding cases. At q=.05 under a justified all-null BH
setting, report the fraction of studies with any rejection and its one-sided 95%
binomial upper bound. An engineering target upper bound <=.08 is a prespecified
.03 tolerance margin for this finite simulation protocol, not a theorem that FDR
is controlled at .05. Failure is diagnostic; an inconclusive estimate is not a
pass. Report interval coverage similarly. Other assumptions need their own cells,
not extrapolation from the easy Gaussian case.

Strong-signal controls use one identifiable planted shared factor with frozen
noise/SNR and 10 fixed seeds; an absolute aligned score correlation >=.9 on at
least 9 seeds is an engineering recovery check only when that positive-control
construction is declared before running. Null/weak/collinear/adversarial regimes
have no forced positive recovery threshold: report stability, uncertainty and
failure modes. Do not change the generator, seed or acceptance threshold after
seeing failure to create a pass. Full methodological benchmarking needs measured
data, source/selection provenance and a separately prespecified analysis protocol.

## Expensive work and cache

Unit tests use tiny deterministic fixtures. Native adapter smoke tests run small
models; full calibration is a milestone qualification job with an explicit fit
budget, hardware, runtime and evidence location. One heavy-fit lease by default;
parallel code editing does not authorize uncontrolled training concurrency. Cache
only content-matching accepted evidence. Stop at budget limits and record exactly
which commands/cells remain unrun. A skipped qualification is not a passed one.

## Acceptance receipt

For every acceptance line store command, working-directory/ref, environment hash,
start/end status, exit code, test count, artifact paths/hashes, verdict and gap.
Agent prose and exit status alone are insufficient. Sol reruns targeted tests on
integration and native package checks per milestone; Astra reviews relevant diffs
and concise evidence, with narrowly requested failure excerpts if needed. Review
cannot certify unexecuted scientific methods. No real participant data go into
Git, delegated briefs or the public report fixtures.
