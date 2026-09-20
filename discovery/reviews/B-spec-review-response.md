# B draft response to independent review

Date: 2026-09-05. Author response to `discovery/reviews/B-spec-review.md`; no independent re-review or executable validation claimed.

Target: `discovery/spec-drafts/B-design-statistics.md`.

- Before SHA-256: **47A1AFF5D47AAF5C2009F93BAA73BBC4B6A567EE36DF419F0E796E65E8921564** — matched the reviewed snapshot before editing.
- After SHA-256: **94D727FE345F6B1EB762FC734F8C98CAA0E4ACA3771EE50F3225933B3FBE4972** — measured after editing.

| Finding | Document response | Status |
|---|---|---|
| BR-03 required sensitivity | Added proposed profile-requirement capability; ordinary configuration cannot disable mandatory sensitivity. `unavailable_required` leaves the requirement incomplete and differs from a maintainer-accepted, referenced waiver and from actual completion. Methods/null/ties/disagreement remain gates; B-T06 and errors updated. | Addressed in draft; method, metadata/completeness policy and any waiver remain unapproved. |
| BR-04 omnibus versus scalar result | Proposed discriminated `test_type`, joint-null/coefficient-set/statistic/df fields; omnibus scalar effect/SE/direction explicitly inapplicable. No scalar effect-floor nomination or fictitious fold change. Requests remain blocked until method/schema/candidate policy approved; multi-group scalar contrasts retained. | Addressed in draft; no omnibus implementation/scope decision accepted. |
| BR-05 observation counts | Proposed `n_used` consistently counts independent experimental units with unit tag. Separate actual fit rows, source specimens, subjects, complete pairs and observed/imputed source-cell counts reference per-feature/test masks. Added mixed observed/imputed delta example and stronger pseudoreplication oracle. | Addressed in draft; resolved representation/model and exact schema still require approval. |
| BR-06 directional metric | Bound proposed contrast values to the approved common decomposition's processed-feature scale and observation/imputation masks; required declared contrast-mask policy and split counts. Defined zero-effect/all-zero/no-eligible unavailable statuses. Reversal swaps positive/negative counts and effect sign while preserving sign-agreement fraction and denominator. | Addressed in draft; numerical zero/denominator policies and nonpaired NA interpretation remain reviewable. |

BR-01 and BR-02 remain **P1 readiness gates**: shared representation/analysis-unit and valid repeated-model ADR; confirmed technology, compatible locked dependencies/source/licences and actual BC-V01–04/model-calibration evidence. No model option, BlockProfile/IntegrationResult amendment, rank method, sensitivity waiver, multiplicity family, omnibus policy or directional default has been accepted by this revision.

Only the B draft and this response were written. Verification consisted of reading the review, checking before/after hashes and inspecting revised text. No production code, dependency addition, model fitting, test execution, commit/push or Notion synchronization occurred. The B package remains **not implementation-ready**. Next action: targeted independent recheck of BR-03–06, cross-component schema reconciliation, then owner decisions and execution gates.
