# M4-011 Unit A — fresh Sol-high review

- Date: 2026-10-16
- Reviewer: `/root/m4_011_unit_a_sol_review`
- Base: `f5c4b58`
- Candidate: `814964b5f525f2104153777a5580f0a84f917ea4`
- Scope: read-only review of `R/results_b.R` and `tests/testthat/test-B-T07.R`
- Verdict: **PASS**
- Findings: P0 0, P1 0, P2 0

The returned matrices come directly from the same `resolved` object used by the
Stage B fit and are keyed by the configured block identifiers. Independent
runtime inspection confirmed exact equality with those resolved assays. The
focused tests compare every returned matrix hash with the public Stage B
`source_hash`, prove that a value mutation changes the hash and is rejected with
typed `B_EXPORT_MATRIX`, and preserve the Stage B payload and envelope fields.
No matrix or native model object is added to serialized Stage B artifacts.

The reviewer replayed the RED condition against the base and observed the
expected missing-matrix failure. On the candidate, the B-T07 suite reported 103
passing expectations, zero failures and zero skips; eight warnings were limited
to absent Git metadata in a temporary archive. `git diff --check` passed and the
candidate changed exactly the two authorized files. No private study data was
accessed. B-T07 remains unaccepted pending Unit B and the remaining M4 gates.

Next authorized transition: serially integrate Unit A, implement the bounded
Sol-owned CLI/namespace/manual registration, then run the exact integrated
B-T07 CLI and regression gates.
