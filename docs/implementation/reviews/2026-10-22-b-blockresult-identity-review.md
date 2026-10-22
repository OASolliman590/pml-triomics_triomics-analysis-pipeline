# Stage-B BlockResult identity repair review

Date: 2026-10-22  
Base: `a50547f`  
Candidate: `05fc87c1bb66148a08f1fbb4db1dbde9a3622e0f`  
Integration: `9efdf19`  
Verdict: `ready_to_integrate: yes`

The fresh read-only Sol-high review found no Critical, Important, or Minor
defect. The candidate changed exactly `R/results_b.R`,
`tests/testthat/test-B-T01.R`, and `tests/testthat/test-B-T07.R`; its worktree
was clean and `git diff --check` passed.

The review verified deterministic scalar and joint identities, explicit joint
claim/member semantics, idempotence, and fail-closed rejection for all supplied
row-context or `result_id` conflicts. Eighteen direct adversarial probes passed.
Fresh targeted suites passed B-T01 581/581, B-T02 208/208, and B-T07 216/216,
with no failure, error, warning, or skip in the final valid runs. A recursive
base-versus-candidate B-T02 public-payload comparison found every pre-existing
serialized field identical, including statistics, q-values, statuses, families,
and selection metadata. The repair adds identity and context after fitting; it
does not alter the fitted scientific result.

The identifiers are scoped to the declared study, analysis, block, claim,
family, feature, and representation identities. They are not content-addressed
across model changes; consumers must interpret them with immutable artifact
provenance. The frozen contract does not require model/source hashes to be
members of `result_id`, so this is a disclosed limitation rather than a review
finding.

The D-T01 fixture's `slope_per_score_unit` Stage-B failure is separate from this
repair: it occurs before finalization. The already-authorized D-T01 shared
repair changes that fixture to the accepted `per_unit_beta` vocabulary and must
still pass the exact integrated B-to-C-to-D gate. This review does not accept
D-T01 or release any successor. No private study data were read.
