# M4-003 — reopen B-T05 for method-specific unavailable and fallback dispatch

- Date: 2026-10-15
- Trigger: the focused review found that missing counts bypass B-T05 before it
  can emit the method-specific AC4 outcome.
- Status: **proposed; not accepted**
- Evidence: `docs/implementation/investigations/B-T05-dispatch-reopening.md`
- Independent audit: pending

## Proposed decision

Preserve B-T05's scientific contract and repair only its routing boundary. Every
DEqMS request in the B-T01 level or B-T03 delta lanes must enter B-T05 even when
count metadata are missing or invalid. `triomics_b05_fit()` remains responsible
for validating count identity/provenance and for returning either named DEqMS
unavailability or a separately identified, explicitly requested limma fallback.

The presence of valid count metadata must not be used as a prerequisite for
entering the component that is required to report invalid count metadata.

## Contract effect

This does not add a method, estimand or supported analysis lane. It implements
the existing B contract and B-T05-AC4 requirement that unavailable DEqMS have a
named status and that a declared limma fallback carry separate provenance. It
does not make DEqMS available for B-T02 or another unsupported lane.

## Preserved behavior

- DEqMS version qualification remains pinned to 1.30.0.
- Positive aligned peptide/PSM counts, acquisition and provenance remain
  mandatory for a DEqMS result.
- Delta count summaries remain feature-specific and use only retained complete
  pairs.
- Fallback remains opt-in and is never labelled DEqMS.
- Effect units and all existing AC1-AC3 behavior remain unchanged.
- No real-study inputs or empirical tuning may be used.

## Authorized work only if Omar accepts

Acceptance authorizes one bounded second targeted repair in a fresh isolated
Luna max worktree. The worker may reuse the unintegrated candidate commits
`b1e753d3b9004737eaf68ecc9916da472f3f5754` and
`efd830884b894d79441af5a948fa3b600ff8e4db` as historical packet content, but
must begin from the then-current integration snapshot and modify only:

- `R/statistics_deqms.R`
- `tests/testthat/test-B-T05.R`

The worker must first add failing regressions for missing-count level requests
with fallback `none` and `explicit_limma`, then make the smallest routing repair.
It must run the B-T05 suite and synthetic CLI and stop with a concise receipt.

A fresh Sol 6.1 high reviewer must inspect the complete packet diff against the
current integration snapshot and rerun the packet/adversarial tests. Sol may
integrate only after review acceptance and fresh adjacent/full/package checks.
This is a new accepted reopening record, not a reinterpretation of the exhausted
2026-10-14 focused re-review.

## User decision

Pending Omar's explicit acceptance. Proposal and audit do not authorize code.
