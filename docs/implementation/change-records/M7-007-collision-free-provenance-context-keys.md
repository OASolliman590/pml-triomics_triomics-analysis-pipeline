# M7-007 — collision-free provenance context keys

- Date: 2026-10-24
- Status: **authorized by Omar's instruction to continue**
- Product base: `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Blocker receipt:
  `docs/implementation/reviews/2026-10-24-m7-006-astra-acceptance-audit.md`
- Scientific contract: unchanged from accepted M7-002 plus M7-003

## Purpose

This bounded successor cycle repairs exactly `M7-006-ASTRA-01`. The current
validator joins tuple components using raw U+001C. Because JSON identifiers may
contain that escaped character, two distinct context tuples can serialize to
the same comparison key and leave represented-matrix/preprocessing context
closure partially fail-open.

This is an engineering identity-encoding repair. It does not add a scientific
method, restrict previously legal identifier content, change the closed schema,
alter migration policy, or implement any Stage-D producer or consumer seam.

## Pre-agreed seam and allowlist

Tests exercise the public
`triomics_validate_stage_handoff_provenance()` boundary with public synthetic
profiles. One fresh isolated Luna-max worker may edit only:

- `R/provenance_contract.R`;
- `tests/testthat/test-M7-003-provenance-contract.R`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json`.

The schema, frozen specification, package/CLI registration, DESCRIPTION,
NAMESPACE, ledger, handover, producer seams, migration execution, and Stage-D
files remain closed or Sol-owned.

## TDD requirements

Use one vertical red-to-green slice:

1. Start with a valid B or C public synthetic profile. Assign one complete
   unit/design/method context preparation `p\u001cq`, representation `r`.
   Assign represented-matrix and matching preprocessing descriptors the
   distinct context preparation `p`, representation `q\u001cr`, preserving all
   other required relationships and refreshing the identity digest. Prove the
   current public validator accepts this malformed profile before product code
   changes.
2. Replace delimiter-based tuple serialization with a collision-free encoding
   that preserves exact component boundaries and values. Do not solve the defect
   by banning U+001C or narrowing the accepted schema.
3. Prove the adversarial profile rejects with `D_PROVENANCE_INVALID`. Preserve a
   positive fully covered multi-context profile containing escaped U+001C so
   legal identifiers and valid context multiplicity remain accepted.

Run the focused M7-003 provenance-contract suite to zero failures, warnings and
skips. Record RED and GREEN evidence in the registration request and return one
clean commit.

## Attempts and review route

One implementation plus at most two targeted repairs is allowed. Sol inspects
the candidate and runs the two-axis Standards/Spec review against product fixed
point `e4aed59`. Both axes must pass before serial integration. Sol then reruns
the focused suite and one full source suite, without suppressing warnings, and
creates an immutable snapshot for exactly one fresh read-only Astra-low
acceptance audit. There is no automatic audit rereview in this cycle.

## Closed scope

Do not accept M7-004, M7-006, M7-007, D-T01 or M7 inside a worker or reviewer.
Do not start B-T07/C-06 producer provenance, migration, D
verification/publication, or any downstream Stage-D packet. Do not read private
study data, loosen the B-T04 bound, reroll seeds, edit frozen `discovery/spec-v2`,
suppress warnings, or push to GitHub.
