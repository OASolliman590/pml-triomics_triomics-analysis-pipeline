# M7-009 two-axis code review

- Date: 2026-10-25
- Product fixed point: `8745f22a2fddaef9a98a49ce646efe57b1f40245`
- Candidate commit: `762a0a09104fb70c920784f3c0897f3e96565567`
- Reviewers: `/root/m7_009_standards_review`, `/root/m7_009_spec_review`
- Product edits by reviewers: none
- Overall disposition: **changes required**

## Standards

Verdict: **changes required**. Critical 0, Important 1, Minor 1.

The resume inspector derives structural envelope validity during a payload hash
mismatch by writing a modified temporary envelope beside the finalized
artifact. A readable but non-writable artifact directory can therefore turn a
true `hash_mismatch` into `incomplete`, and read-only inspection mutates the
artifact directory. The focused tests use a writable directory and do not
exercise this boundary.

The nonblocking Minor is that rewriting an envelope solely to reuse the
combined schema/hash validator gives the inspector unnecessary filesystem
responsibility.

## Spec

Verdict: **pass**. Critical 0, Important 0, Minor 0.

The candidate implements the seven ordered outcomes, keeps `hash_mismatch`
ahead of late-evidence failure, separates physical presence, readability, base
validity, payload binding and required-evidence completeness, permits only the
declared-partial diagnostic publication, and requires full evidence for a
complete claim. Existing AC3 still expects `incomplete`; public filesystem
mutations and the complete-without-provenance post-write refusal are tested.
Only the three allowlisted files changed and registration remains
`candidate/review_pending`, `accepted: false`.

Independent exact B-T07 verification completed with FAIL 0, WARN 0, SKIP 0,
PASS 298.

## Disposition

The candidate is not ready to integrate because both review axes must pass.
The same Luna worker may perform one bounded repair within the original
three-file allowlist: resume inspection must not write into the artifact tree,
and a readable structurally valid envelope that binds different payload bytes
must remain `hash_mismatch` when its directory is non-writable. The repair must
be proven through the public B-T07 seam and receive focused rereview before any
integration. This receipt grants no acceptance or downstream authority.
