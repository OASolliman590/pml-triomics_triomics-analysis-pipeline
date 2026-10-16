# B-T07 initial Sol-high review

- Date: 2026-10-16
- Base: `b204b53`
- Candidate: `b7a9455bc14a4c2a062a92ad2d8a68e9c2d2dccd`
- Role: fresh Sol 6.1 high, read-only
- Verdict: **FAIL**
- Spec compliance: fail
- Code/test quality: fail
- Findings: P0 0, P1 3, P2 1

## Findings

1. **P1 — exporter errors satisfy success tests.** Four tests assert that the
   caught value has R type `list` and return early when it is an error, but a
   `triomics_error` is itself list-valued. Direct AC1 execution raises
   `B_EXPORT_INCOMPLETE`; the remaining AC1–AC4 success assertions do not run.
   Some hidden assertions also expect flattened family fields although the
   implementation nests them under `$family`.
2. **P1 — matrix binding does not verify the fitted source hash.** Changing a
   keyed synthetic matrix value while retaining the original public result
   artifact still returns a complete bundle and AC3 pass. The code must compare
   each applicable public result `source_hash` with the actual keyed matrix hash;
   a provenance statement is not verification.
3. **P1 — interrupted late writes remain reusable.** Bundle members exclude the
   reproduction and acceptance payload/envelope pairs, and the bundle envelope
   finalizes before they are written. Deleting those four artifacts still yields
   `reuse=TRUE` and AC4 pass with a nonexistent reproduction path. Final
   completeness and resume must cover every required handoff artifact.
4. **P2 — reproduction text makes unconditional synthetic-data claims.** The
   public API accepts general configurations and preserves upstream privacy
   classes. Synthetic/no-real-data statements must be derived from verified
   context or the API must be explicitly restricted to synthetic input.

AC1 and AC4 are unproven; AC2 predecessor smokes execute but export acceptance is
unproven; AC3 fails. The exact CLI proves Stage B only and creates no B-T07
handoff. The missing same-run matrix return and post-B hook remain legitimate
Sol-owned integration work, not a scientific-spec defect, but integration cannot
begin until these candidate defects pass bounded repair 1 and focused rereview.

No private data or native model object was inspected. The candidate remains
unintegrated.
