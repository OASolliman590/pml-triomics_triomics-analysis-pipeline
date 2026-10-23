# M7-006 — provenance completeness repair

- Date: 2026-10-24
- Status: **authorized by Omar**
- Base implementation snapshot: `5dcc25f`
- Blocker receipt:
  `docs/implementation/reviews/2026-10-24-m7-004-astra-acceptance-audit.md`
- Scientific contract: unchanged from accepted M7-002 plus M7-003

## Purpose

This bounded cycle repairs exactly the two Important completeness defects found
by the fresh M7-004 Astra acceptance audit:

1. `M7-004-ASTRA-01`: represented-matrix/preprocessing contexts are not required
   to join the corresponding analysis-unit, resolved-analysis, design, and
   method-state contexts.
2. `M7-004-ASTRA-02`: the migration proof set omits the mandatory
   `handoff_payload` role.

This is a product-contract repair, not a new statistical method, scientific
claim, migration policy, or Stage-D implementation.

## Pre-agreed public seam and allowlist

Tests exercise `triomics_validate_stage_handoff_provenance()` through public
synthetic profiles and require `D_PROVENANCE_INVALID` for malformed profiles.
One fresh Luna-max worker in one fresh isolated worktree may edit only:

- `R/provenance_contract.R`;
- `tests/testthat/test-M7-003-provenance-contract.R`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json`.

The frozen schema, specification, package/CLI registration, DESCRIPTION,
NAMESPACE, ledger, and handover are Sol-owned or closed.

## TDD requirements

The worker must use vertical red-to-green slices:

1. Add a valid B-profile baseline, move the represented matrix and its matching
   preprocessing state to a different preparation context while leaving the
   unit/design/method descriptors behind, refresh the identity digest, and prove
   that the current validator accepts it before implementing the rejection.
2. Add a valid migration-profile baseline whose complete proof object includes
   every mandatory role including `handoff_payload = "proven"`; prove the current
   validator rejects that complete proof and separately permits the malformed
   omission before implementing the correction.

After each RED, implement only enough validation to close that behavior. Preserve
valid multiple-block, multiple-method, multiple-preprocessing-state,
multiple-route, and complete-migration profiles. The focused M7-003 contract
suite must finish with zero failures, warnings, and skips. Record commands and
counts in the registration receipt and return one clean commit.

## Attempts and review

One implementation plus at most two targeted repairs is allowed. Sol inspects
the candidate and runs the two-axis code-review workflow against fixed point
`5dcc25f`. Both axes must pass before serial integration. Sol then runs the full
source suite once, without suppressing warnings, and creates an immutable
snapshot for exactly one fresh read-only Astra-low acceptance audit. There is no
automatic audit rereview in this cycle: a remaining Critical or Important
finding blocks acceptance and requires new user authority.

## Closed scope

Do not start B-T07/C-06 provenance producer wiring, D verification or
publication, migration execution, D-T01 acceptance, M7 release, or any Stage-D
successor. Do not read or disclose private study data, change the B-T04 bound or
seeds, edit frozen `discovery/spec-v2`, suppress warnings, or push to GitHub.
