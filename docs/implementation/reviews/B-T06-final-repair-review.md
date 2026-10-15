# B-T06 final repair 2 review

- Date: 2026-10-15
- Reviewer: `/root/bt06_final_review`
- Requested role: Sol 6.1 high, read-only
- Integration base: `94f3580`
- Candidate: `086d604de8ba481e437d3c9109a1780839104810`
- Verdict: **not accepted for integration**

The reviewer found that the final two-file scale repair satisfies M4-004:
eligibility now requires finite exact-opposite coefficients and a nonzero target,
direction follows the target coefficient, and regressions cover canonical, tiny
incompatible, tiny valid, reversed and extreme-finite contrasts. The worker
reported 66 passing packet assertions with no failures, warnings or skips and a
successful synthetic CLI run.

The complete restored packet nevertheless introduces an important adjacent
integration regression. `R/multiplicity.R` replaces startup installation with a
chain that omits B-T04's fit dispatch and stage hook; its source CLI wrapper also
captures the older B-T01 main function. A focused reviewer probe showed the same
qualified B-T04 trajectory succeeds through `triomics_b04_fit()` but fails when
routed through the candidate's `triomics_fit_block_models()`. Supported
repeated/trajectory requests can therefore reach the wrong fitter.

Resolving this finding requires a foreign packet file outside M4-004's final
two-file repair allowlist. The record permits no additional repair or review.
B-T06 returns to blocked and candidate `086d604` remains unintegrated. Private
study data were not read or used.
