# Independent targeted re-review of revised B

Reviewed 2026-09-05 by the original independent B reviewer, not the B author. Compared actual revised [B draft](../spec-drafts/B-design-statistics.md) sections 2–4, 6, 8–10 with BR-03–06 in the [original review](B-spec-review.md), and read the [shared contract proposal](../operations/shared-contract-proposal.md). The author response was not substituted for inspection. This is a targeted document re-review, not a second complete scientific literature review, an adopted schema or executable validation.

| Inspected artifact | SHA-256 |
|---|---|
| Revised B draft, including root's review-status/link corrections | `49A1AC488F25416249313BA7406CE78418FCBF0532197FD07853B58B4065014B` |
| Shared contract proposal, including explicit two-group vignette wording | `01AA4B27A08900F56737B775BCD74069D82963F7299FD79EF1EA4D0B565555CA` |

**Disposition: BR-03–06 are addressed as conditional draft clarifications. B remains not adopted and not implementation-ready.** This closes the four requests to make proposed behavior explicit; it does not accept the proposed behavior or certify a runtime outcome. BR-01/02 remain P1 readiness gates.

The final opening and section 9 review-role paragraph were re-read after root's status edits: they correctly acknowledge the completed independent draft review while retaining final readiness and implementation review gates. The shared proposal was re-read after its final wording change; its identity/count definitions remain as reviewed below.

## Requirement and contract findings

### BR-03 — addressed in the conditional design; sensitivity decisions remain gated

Actual locations: section 3 configuration, “Required sensitivity proposal (BR-03 clarification)” and failure table; section 6 ADR-B-05; section 8 B-T06. The required metabolomics sensitivity is now protected through a proposed capability rather than a display-name branch. Disabling it without an accepted versioned waiver raises `B_SENSITIVITY_REQUIRED`; `unavailable_required` cannot count as fulfillment, and a waiver differs from completion. The draft expressly prohibits substituting an unadjusted rank test for an adjusted/paired estimand.

The method/null, tie/zero treatment, disagreement definition, capability storage and exact fail-versus-partial completeness policy still need approval before B-T06. The shared proposal separates requested-analysis completion from rendering, which is compatible with this requirement but does not itself finalize B's error/exit policy. No waiver has been granted by either document or this review.

### Shared identity/count coordination — coherent proposal, not an accepted crosswalk

Actual locations: B section 2 “Identifier handling,” section 3 result key/count definitions; shared proposal “Identity crosswalk proposed for approval.” The shared proposal preserves measurement identity independently of display names, distinguishes sample from analysis-unit identity, and links each nomination to a feature and source analysis. Its `(analysis_id, analysis_unit_id)` map avoids conflating multiple contrasts from one subject. Counts agree with revised B's separation of independent units, fitted rows and observed source values.

An explicit reconciliation gate remains: B currently describes `(block_id, id_namespace, feature_id)`, whereas the shared proposal uses canonical `feature_key`, `block_key`, `original_id` and an optional `source_row_id` discriminator. Both can be made consistent, but repeated original IDs and derived-ID collisions cannot be resolved by independently selecting different ingest/join policies. Before implementation, owners must accept one typed registry/crosswalk, specify whether repeated source IDs are rejected or discriminated, and bind B/C/D joins to it without losing visible original IDs. This review does not silently replace the protected IntegrationResult feature/score semantics. The proposal itself correctly leaves that acceptance open.

The exact fit mask must refer through the accepted unit map to source cells, including derived-feature lineage and A's observation/imputation provenance. The shared proposal's parent-universe and missingness policy requirement is appropriate; it does not constitute an implemented mask export. No additional factual conflict was found in its stated count semantics.

## Scientific and evidence findings

### BR-04 — addressed in the conditional design; omnibus remains unsupported until approved

Actual locations: section 3 “Scalar versus omnibus proposal (BR-04 clarification)” and failure table; section 6 ADR-B-03; section 8 B-T02. `test_type` now distinguishes scalar contrasts from a named joint null. Omnibus rows carry actual statistic/distribution and numerator/denominator df; scalar effect, SE and direction are inapplicable. The draft disallows a representative fold change and scalar effect-floor nomination, supplies an unresolved candidate-rule status, and rejects requests with `B_OMNIBUS_UNVERIFIED` before approval/verification. Thus the generic result-table shorthand about q/effect candidates is constrained by the explicit discriminated rule.

The omnibus test, multiplicity family and eventual nomination policy are not approved by this correction. Scalar multi-group contrasts remain the stated supported proposal. Pinned API checks must eventually confirm all method-specific statistic/df/SE semantics, including DEqMS; no runtime claim follows from the schema.

### BR-05 — addressed in the conditional design; independent-unit and provenance checks still required

Actual locations: section 3 result table and “Observation-count proposal (BR-05 clarification)”; section 9 pseudoreplication test. `n_used` now has one declared meaning: contributing independent experimental units, tagged as subjects or independent samples. It is distinct from `n_analysis_rows`, original specimens, repeated-study subject counts, complete pairs, observed input cells and imputed input cells. Counts are feature/test-specific and reference the fit/eligibility mask. The one-observed/one-imputed delta example correctly separates two source cells from one delta row and one subject. NA with a reason differs from a known zero.

This is not an assertion that n subjects equal residual df or effective information. The independent-unit definition is an eligibility gate when the chosen model cannot supply it. Representation, covariance, incomplete pairs, replicate handling and approved A mask/derived-lineage exports remain unresolved where the draft says so. The software must later demonstrate those definitions on both n-row and 2n-row fixtures; this re-review executed none.

### BR-06 — addressed in the conditional design; metric choices require adoption

Actual locations: section 3 directional configuration; section 4 “Covariates, direction and lipid-feature families”; section 8 B-T06; section 9 sign-reversal test. The metric is bound to the common decomposition and declared A-processed scale; “unadjusted” no longer implies acquisition-scale data. Observed-only versus completed contrast masks are explicit choices. Completed contrasts retain separate observed-only and imputation-involving positive/negative/zero/NA counts; missing source masks produce unavailable status. Adjusted-effect comparisons are accurately labeled as unadjusted paired sign agreement with adjusted effect direction.

Zero effect, all-zero contrasts and no eligible contrasts have named states and NA agreement. Simultaneously reversing subject contrasts and effect swaps positive/negative counts while preserving sign-agreement fraction, eligible denominator and zero counts under the stated symmetric zero policy. The revised test therefore has the correct invariant. Exact tolerance/denominator choice and precedence of overlapping exceptional states should be frozen in the final schema. NA outside meaningful paired/scalar designs remains an explicitly proposed interpretation requiring maintainer disposition, not an adopted relaxation of the user requirement.

## Remaining P1 gates and validation limits

BR-01 remains open: approve the common representation, analysis-unit identities and score-row semantics jointly with C, then select a scientifically justified repeated model/covariance strategy. Centered-response fixed-subject algebra remains a proposed alternative under stated assumptions; it does not authorize switching the requested model or establish arbitrary residual covariance validity. Matrix dimensions, unit/observation mapping, permitted coordinate views, eligible C outcomes and leakage boundaries require the shared ADR and BC-V02/V03 evidence.

BR-02 remains open: confirm technology/execution host and dependencies, pin compatible R/Bioconductor and exact APIs/source/licences, and execute the applicable BC-V01–04 reference/count/NA/design and prespecified calibration checks. DEqMS count-prior validity on the chosen representation, rank-sensitivity methods and longitudinal methods remain evidence gates. Historical reconciliation, all-component approval, environment, actual reproduction instructions and publication prerequisites are not closed by these edits.

No new hard scientific error was found in the reviewed changes. Remaining policy and crosswalk choices are documented and block readiness; agreement between documents or reviewers is not scientific evidence. No production files were edited, no tests or statistical code ran, and no implementation, publication or whole-goal completion is certified.
