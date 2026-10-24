# M7-008 B-T07 native provenance producer blocker

Date: 2026-10-24  
Authorization: bounded M7-008 producer-provenance or migration cycle  
Base: `0d96c578a2144c58cf298905aa87c10f78bf3eea`  
Final candidate: `8745f22a2fddaef9a98a49ce646efe57b1f40245`

## Disposition

M7-008 is blocked after its initial implementation and both permitted targeted
repairs. The candidate is not integrated, reviewed, accepted, or released.
The independent Standards and Spec reviews were not dispatched because the
focused B-T07 gate is still red.

## Evidence

- Pre-implementation RED: FAIL 2, WARN 8, SKIP 0, PASS 218. The exact public
  CLI emitted neither a native provenance payload nor its envelope.
- Final focused constructor regression: FAIL 0, WARN 0, SKIP 0, PASS 1.
- Final exact B-T07: FAIL 1, WARN 0, SKIP 0, PASS 261.
- Both public B-T02 identity CLI runs exit 0.
- B-T03: FAIL 0, WARN 0, SKIP 0, PASS 286.
- The worktree is clean, `git diff --check` passes, and the complete candidate
  changes only `R/results_b.R`, `R/export_b.R`, `tests/testthat/test-B-T07.R`,
  and `docs/implementation/registrations/B-T07.json`.

The final candidate successfully emits a native B StageHandoffProvenance
payload and envelope. The public validators accept them. Required roles,
ordered matrix and method identities, predecessor bytes, missing or tampered
evidence refusal, post-fit source-mutation refusal, ordinary `model_hash`
contrast handling, and unchanged block-result serialization pass.

The sole remaining failure is B-T07 AC3. A direct export given
`caller_matrix_unverified` has no same-run `provenance_context`, remains
non-reusable, and reports the fail-closed reason `partial`; the frozen test
expects `incomplete`. The worker correctly did not relax the assertion because
that scenario never constructs a valid same-run profile capable of proving the
expected state label wrong.

## Boundary

The distinction between `partial` and `incomplete` now requires an explicit
contract/state-vocabulary adjudication before another implementation cycle.
It must not be resolved by silently changing a test or by spending a third
repair in M7-008. C-06 provenance, migration execution, D verification or
publication, D-T01, M7 acceptance, G7 release, and every Stage-D successor
remain closed. No private study data was read, and nothing was pushed.

Literal resume instruction:

> Reconcile M7-008 blocker `8745f22`: decide the B-T07 AC3 `partial` versus
> `incomplete` contract in an explicit authorized change record before any new
> implementation. Do not integrate the candidate or start C-06/Stage D first.
