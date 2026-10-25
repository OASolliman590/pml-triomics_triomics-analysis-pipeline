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

The shared resume contract already distinguishes these cases:

- `partial` is returned when a payload exists but its companion envelope is
  absent, representing an interrupted publication pair.
- `incomplete` is returned when the payload/envelope pair exists but the
  candidate's declared status is not `complete`.

B-T07 currently collapses full-candidate validation into the field named
`envelope_present`. Missing late/provenance evidence therefore looks like a
physically absent envelope and changes the reason from `incomplete` to
`partial`, even when the envelope file exists.

## Decision

Artifact status and resume-refusal reason remain separate concepts.

1. `partial` means the publication pair is structurally interrupted: a payload
   exists but its companion envelope file is physically absent. It does not
   mean that a present, finalized candidate merely has partial scientific or
   provenance content.
2. `incomplete` means both payload and envelope files are physically present,
   but the candidate is not eligible as a complete reusable handoff because its
   declared status is not `complete` or its required member/late/provenance
   evidence is incomplete or invalid.
3. Both reasons are fail-closed: `reuse = FALSE`. Neither grants scientific
   validity, provenance eligibility, or permission to consume the handoff.
4. `absent`, `invalidated`, and `hash_mismatch` retain their established
   meanings. M7-009 does not change cache identity, hashes, validators, artifact
   statuses, or publication atomicity.
5. For the M7-008 AC3 fixture, the correct reason is `incomplete`: both bundle
   files exist, the bundle declares `status = "partial"`, and the same-run
   provenance frame is unavailable. The existing test expectation is therefore
   retained; product classification must be repaired.

## Authorized implementation after audit pass

The repair is limited to the restored M7-008 candidate and these files:

- `R/export_b.R`
- `tests/testthat/test-B-T07.R`
- `docs/implementation/registrations/B-T07.json`

The implementation must keep physical envelope presence separate from complete
candidate validation. A physically present but incomplete/invalid candidate
must never become reusable. The public B-T07 seam must prove at least:

- payload present plus envelope absent -> `partial`, reuse refused;
- payload and envelope present plus declared partial status -> `incomplete`,
  reuse refused;
- payload and envelope present plus missing required provenance/late evidence ->
  `incomplete`, reuse refused;
- complete, matching, fully validated candidate -> `complete_match`, reuse
  permitted;
- changed cache identity and payload/envelope hash mismatch remain refused with
  their existing reasons.

No assertion may be weakened merely to obtain GREEN. No frozen spec-v2 file,
schema, Stage-C/Stage-D code, private study input, B-T04 calibration bound, or
seed is changed. M7-008 remains unintegrated until the repaired candidate passes
focused verification and subsequent review.

## Audit question

Does this vocabulary faithfully preserve the existing shared resume contract,
process-failure atomicity, and fail-closed provenance behavior without creating
a path that reuses a present but invalid B handoff?
