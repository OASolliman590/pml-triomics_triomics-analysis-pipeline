# M4-003 — reopen B-T05 for method-specific unavailable and fallback dispatch

- Date: 2026-10-15
- Trigger: the focused review found that missing counts bypass B-T05 before it
  can emit the method-specific AC4 outcome.
- Status: **proposed; not accepted**
- Evidence: `docs/implementation/investigations/B-T05-dispatch-reopening.md`
- Independent audit:
  `docs/implementation/reviews/B-T05-reopening-astra-audit.md`
- Focused rereview: **PASS** on corrected snapshot `0dfe88d`

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

Acceptance authorizes the packet's second and final targeted repair in a fresh
isolated Luna max worktree. Sol must begin from the then-current integration
snapshot and restore all four packet-owned files from the unintegrated candidate
commits
`b1e753d3b9004737eaf68ecc9916da472f3f5754` and
`efd830884b894d79441af5a948fa3b600ff8e4db` unchanged:

- `R/statistics_deqms.R`
- `tests/testthat/test-B-T05.R`
- `inst/examples/tickets/B-T05.yaml`
- `docs/implementation/registrations/B-T05.json`

After that restoration, the Luna repair may modify only:

- `R/statistics_deqms.R`
- `tests/testthat/test-B-T05.R`

The worker must replace the candidate regression that expects a missing-count
level request to bypass B-T05. It must first add failing regressions for both
`NULL` and nonexistent-path count inputs, each with fallback `none` and
`explicit_limma`. The assertions must cover named reason, requested method,
actual method and fallback provenance. Existing delta and unsupported-lane
dispatch checks remain. Source and installed CLI paths must demonstrate the same
AC4 behavior. The worker then makes the smallest routing repair, runs the B-T05
suite and synthetic CLI, and stops with a concise receipt.

The 2026-10-14 initial review and focused re-review remain exhausted historical
events; their counters are not reset. This accepted reopening would authorize
exactly one fresh Sol 6.1 high reopening review of the complete packet diff
against the current integration snapshot. That reviewer must rerun the packet
and adversarial tests. Sol may integrate only after review acceptance and fresh
adjacent/full/package checks. If repair 2 or this single reopening review leaves
an unresolved blocker, B-T05 returns to blocked with no further packet repair or
reopening review allowance under this record. The independent Astra M4 milestone
gate remains required before any consumer is released.

## User decision

Pending Omar's explicit acceptance. Proposal and audit do not authorize code.
