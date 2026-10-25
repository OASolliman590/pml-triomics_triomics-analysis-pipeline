# M7-009 — B-T07 resume-reason vocabulary

Date: 2026-10-25  
Status: proposed; implementation prohibited until a fresh read-only audit passes  
Authority: Omar authorized M7-009 to define `partial` versus `incomplete`, audit
the change, and only after approval repair and verify the remaining B-T07 AC3
expectation.

## Problem

M7-008 candidate `8745f22a2fddaef9a98a49ce646efe57b1f40245`
successfully writes and validates native Stage-B provenance for the ordinary
same-run CLI path. Its exact B-T07 suite has one remaining failure. A direct
export using `caller_matrix_unverified` writes `bundle.json` and its envelope,
declares the bundle `status = "partial"`, correctly refuses reuse, but reports
resume reason `partial`; the established AC3 assertion expects `incomplete`.

The shared resume decision function already distinguishes these logical cases:

- `partial` is returned when a payload exists but its companion envelope is
  absent, representing an interrupted publication pair.
- `incomplete` is returned when the payload/envelope pair exists but the
  candidate's declared status is not `complete`.

B-T07 currently collapses full-candidate validation into the field named
`envelope_present`. Missing late/provenance evidence therefore looks like a
physically absent envelope and changes the reason from `incomplete` to
`partial`, even when the envelope file exists. This is a bounded B-T07
classification clarification. Stage A's file reader also derives its current
flag from validation rather than raw filesystem presence; M7-009 does not alter
Stage A or claim that every existing reader already implements the clarified
filesystem classification.

## Decision

Artifact status and resume-refusal reason remain separate concepts.

1. `partial` means the publication pair is structurally interrupted: a payload
   exists but its companion envelope file is physically absent. It does not
   mean that a present, finalized candidate merely has partial scientific or
   provenance content.
2. `incomplete` means both payload and envelope files are physically present,
   but the candidate is not eligible as a complete reusable handoff because its
   payload is unreadable, its declared status is not `complete`, its base
   envelope is unreadable/invalid, or its required member/late/provenance
   evidence is missing or invalid.
3. Both reasons are fail-closed: `reuse = FALSE`. Neither grants scientific
   validity, provenance eligibility, or permission to consume the handoff.
4. `absent`, `invalidated`, and `hash_mismatch` retain their established
   meanings. M7-009 does not change cache identity, hashes, validators, artifact
   statuses, or publication atomicity.
5. For the M7-008 AC3 fixture, the correct reason is `incomplete`: both bundle
   files exist, the bundle declares `status = "partial"`, and the same-run
   provenance frame is unavailable. The existing test expectation is therefore
   retained; product classification must be repaired.

For B-T07, overlapping refusal facts are resolved in this order:

1. no payload file -> `absent`;
2. payload file present and companion envelope file absent -> `partial`;
3. unreadable payload, non-`complete` declared status, or unreadable/invalid
   base envelope -> `incomplete`;
4. complete readable candidate with a different cache key -> `invalidated`;
5. matching cache identity whose readable base envelope binds a different
   payload SHA-256 -> `hash_mismatch`;
6. matching base payload/envelope hashes but missing or invalid required
   member, late, or provenance evidence -> `incomplete`;
7. only a complete, matching candidate whose base envelope and every required
   evidence check pass -> `complete_match`.

Thus ordinary bundle-payload tampering with a readable envelope remains
`hash_mismatch`; it is not relabelled `incomplete`. A physically present but
unreadable/invalid envelope cannot prove its own hash binding and is
`incomplete`. Every non-final case refuses reuse.

## Authorized implementation after audit pass

The repair is limited to the restored M7-008 candidate and these files:

- `R/export_b.R`
- `tests/testthat/test-B-T07.R`
- `docs/implementation/registrations/B-T07.json`

The implementation must keep at least three independent facts: physical
envelope presence, base-envelope/hash validity, and complete required-evidence
validation. `envelope_present` must not stand in for the latter two. Both the
resume decision and the post-write finalization guard must consume the explicit
validation facts; a physically present but incomplete/invalid candidate must
never become reusable or pass finalization. The public B-T07 seam must prove at
least:

- payload present plus envelope absent -> `partial`, reuse refused;
- payload and envelope present plus declared partial status -> `incomplete`,
  reuse refused;
- payload and envelope present plus missing required provenance/late evidence ->
  `incomplete`, reuse refused;
- payload and envelope present plus invalid required provenance/late evidence ->
  `incomplete`, reuse refused, and post-write finalization raises the existing
  `B_EXPORT_INCOMPLETE` error rather than treating physical presence as proof;
- complete, matching, fully validated candidate -> `complete_match`, reuse
  permitted;
- changed cache identity remains `invalidated`; readable-envelope payload hash
  mismatch remains `hash_mismatch`; both refuse reuse.

No assertion may be weakened merely to obtain GREEN. No frozen spec-v2 file,
schema, Stage-C/Stage-D code, private study input, B-T04 calibration bound, or
seed is changed. M7-008 remains unintegrated until the repaired candidate passes
focused verification and subsequent review.

## Audit question

Does this vocabulary faithfully preserve the existing shared resume contract,
process-failure atomicity, and fail-closed provenance behavior without creating
a path that reuses a present but invalid B handoff?
