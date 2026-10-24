# M7-004 — Contract-foundation completion cycle

- Date: 2026-10-23
- Status: **authorized; implementation not yet accepted**
- User authority: Omar invoked the `implement` workflow and requested that the
  accepted specification be implemented and tested to a passing gate.
- Scientific contract: unchanged from accepted M7-002 plus M7-003.
- Predecessor blocker:
  `docs/implementation/reviews/2026-10-23-m7-003-contract-foundation-blocker.md`.

## Purpose

M7-004 is a bounded implementation exception, not a new scientific amendment.
It reopens only the contract-foundation phase that exhausted M7-003's ordinary
attempt allowance. The existing uncommitted three-file result may be preserved
as an explicitly failing, non-acceptance seed; it must not be integrated or
treated as evidence of correctness.

The two known starting failures are:

1. empty `derived_from_entity_ids` reaches `sort(NULL, method = "radix")`; and
2. one embedded-JSON synthetic fixture removes `size_bytes` instead of retaining
   the schema-required JSON null member.

## Allowed files and seams

One fresh Luna-max worker in a fresh isolated worktree may create or edit only:

- `R/provenance_contract.R`;
- `inst/schema/stage-handoff-provenance.schema.json`;
- `tests/testthat/test-M7-003-provenance-contract.R`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json`.

The pre-agreed behavioral seams are the R RFC 8785 canonicalization/identity
boundary and the closed Stage Handoff Provenance JSON Schema plus validator.
Tests must observe contract behavior through these seams, not private helper
implementation details.

## Execution and gates

The worker receives one implementation attempt and at most two targeted repairs.
It must first reproduce the focused RED from the failing seed, then work in
vertical test-first slices. The focused M7-003 suite must finish with zero
failures, warnings, or skips. The mandatory native-route `method_id` mismatch
probe must reject with `D_PROVENANCE_INVALID`.

After the focused gate passes, run the complete test suite once without warning
suppression. Verify the exact four-file allowlist and `git diff --check`, create
the packet-owned registration request, and commit an immutable candidate.

The candidate then receives the `code-review` workflow against the accepted
base, with independent Standards and Spec axes. Any correctness finding returns
only a bounded targeted repair to the same implementer. Sol alone may integrate
a reviewed candidate and apply a reviewed shared dependency registration.

## Closed scope

This record does not authorize B-T07 or C-06 producer wiring, D verification or
publication, legacy migration, shared CLI/NAMESPACE registration, D-T01
acceptance, M7 release, or a Stage-D successor. Those phases remain closed until
the contract foundation is integrated and their own dependency/review gate is
released. No private study data or paths may be read, copied, fitted, delegated,
or published. The B-T04 threshold and seeds remain frozen.
