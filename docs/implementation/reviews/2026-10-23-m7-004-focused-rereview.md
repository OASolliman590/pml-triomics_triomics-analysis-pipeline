# M7-004 focused Spec rereview

- Date: 2026-10-23
- Fixed point: `221df65a20e6a53bf523a4f5592a9ffe1e006ec8`
- Initial candidate: `65662bcaf67f59bd3fbf5440984dc7711444daca`
- Repair 1: `6c437d1`
- Reviewer: `/root/m7_004_spec_review`
- Verdict: **changes required**; Critical 0, Important 1, Minor 0
- Ready to integrate: **no**

## Resolved

The original missing feature-registry block and missing C method-state probes now
reject with `D_PROVENANCE_INVALID`. Reverse evidence-to-route matching by route
kind, route ID, and full declaration scope is also enforced: surplus clinical
evidence that reuses a native route ID rejects. Valid native-route,
multiple-method, multiple-preprocessing-state, and multiple-block fixtures pass.

## Remaining Important finding

Required-scope coverage remains incomplete outside the two repaired groups. The
reviewer independently refreshed identity hashes and reproduced both paths
through the public validator:

1. changing B's sole `method_state.method_id` from the producer/activity method
   `limma` to `unrelated-method` still passes; and
2. changing the sole `normalized_metadata.analysis_id` from `analysis-1` to
   `unrelated-analysis` still passes while the other descriptors remain scoped
   to `analysis-1`.

M7-003 requires metadata exactly once per analysis and method state exactly once
per `(analysis_id, method_id, preparation_context_id, representation_id)`; zero
for a required scope is missing. M7-002 also requires the consumed
method-specific state and settings. An unrelated group cannot satisfy either
requirement.

## Disposition

The complete candidate remains within the original four-file allowlist,
`git diff --check` passes, and the worktree is clean. Repair 2/2 is limited to
first codifying the two remaining paths as RED tests and then enforcing complete
required method-state and normalized-metadata analysis coverage. No full suite,
integration, private-data access, M7 acceptance, or downstream release occurs
before that focused gate and the remaining review path pass.
