# C-08 shared correctness review

Date: 2026-10-20  
Candidate: `b579bfd214618957adb6fcd7193284758913aa27`  
Base: `b45f31a422349c6fd4e5a702c30d7df1c4570e6d`  
Reviewer: fresh Sol 6.1 high, read-only  
Verdict: `findings_remain`

## Findings

1. **Important — source selection ancestry is discarded.**
   `R/cli.R` asks the integration request for `block_ids`, although those IDs
   are declared on the analysis. The resulting empty source-selection record
   silently becomes `prespecified` and `qualified`, including for adversarial
   `unknown` and `outcome_selected` source histories. The exact fixture output
   also contains an empty `source_selection`. The repair must pass the fitted
   block IDs explicitly and prove Stage C refuses or propagates adverse source
   histories.
2. **Important — the persisted endpoint bundle invalidates its own hash.**
   Stage C adds `schema_version` after the endpoint bundle hash is finalized.
   Read-back validation fails with `C_SCORE_ENDPOINT_HASH`. The repair must
   finalize the serialized body before hashing and validate the written payload
   on read-back.

## Reproduced evidence

- C-08: PASS 277, FAIL 0, WARN 0, SKIP 0.
- C-08-shared: PASS 41, FAIL 0, WARN 0, SKIP 0.
- C-01/C-03/C-04 plus shared: PASS 364 total, no failures, warnings or skips.
- Exact synthetic Stage-C command exited 0 and kept the request B-owned,
  `scheduled/not_started`, empty-result and non-recursive, but the two findings
  above prevent acceptance.
- Fit instrumentation observed adapter validate/fit/extract once each. The
  existing PCA fitter retains its internal eligibility revalidation; no second
  fit was observed and the public IntegrationResult remains six fields.
- C-05 reproduced the disclosed baseline-identical 18 failures / 87 passes:
  two stale registration assertions and sixteen unavailable-MOFA/cascade
  failures. This is not a green C-05 compatibility result and is not caused by
  the shared candidate.
- Changed files are exactly the nine authorized paths; no private data,
  stage-registry change or out-of-scope file was found. Worktree is clean and
  `git diff --check` passes.

## Disposition

C-08 remains unaccepted and C-09/C-06 remain closed. One targeted continuation
of the exclusive shared repair may address only these two findings and their
behavioral tests. This does not reopen or rerun the consumed packet repairs.
