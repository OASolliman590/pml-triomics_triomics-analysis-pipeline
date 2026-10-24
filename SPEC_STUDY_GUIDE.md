# triomics specification study guide

This branch is a public, documentation-only view of the triomics 2.0.0-rc1
specification. It is intended for reading and discussion with a chat assistant.
It excludes implementation history, private study inputs, local audit records,
large analysis outputs, credentials, and uncommitted workstation files.

## Recommended reading order

1. [`discovery/spec-v2/README.md`](discovery/spec-v2/README.md) — authority,
   scope, and how the specification is organized.
2. [`discovery/spec-v2/SCIENCE_CONTRACT.md`](discovery/spec-v2/SCIENCE_CONTRACT.md)
   — the scientific promises and limits.
3. [`discovery/spec-v2/DATA_CONTRACTS.md`](discovery/spec-v2/DATA_CONTRACTS.md)
   — the identities and evidence objects that prevent samples, subjects,
   features, visits, and analyses from being confused.
4. Stage documents:
   [`A_INGEST_PROFILES.md`](discovery/spec-v2/A_INGEST_PROFILES.md),
   [`B_STATISTICS_CLINICAL.md`](discovery/spec-v2/B_STATISTICS_CLINICAL.md),
   [`C_INTEGRATION.md`](discovery/spec-v2/C_INTEGRATION.md), and
   [`D_ANNOTATION_TARGET_REPORT.md`](discovery/spec-v2/D_ANNOTATION_TARGET_REPORT.md).
5. [`discovery/spec-v2/VALIDATION.md`](discovery/spec-v2/VALIDATION.md) — what
   must be demonstrated before a method or milestone is accepted.
6. [`discovery/spec-v2/IMPLEMENTATION_PLAN.md`](discovery/spec-v2/IMPLEMENTATION_PLAN.md)
   and [`discovery/spec-v2/packets.json`](discovery/spec-v2/packets.json) — the
   staged construction plan and all 41 packet identities.

## Provenance amendment

The frozen rc1 design is supplemented by the accepted provenance decision and
change records:

- [`docs/adr/0003-stage-handoff-provenance-and-generation-publication.md`](docs/adr/0003-stage-handoff-provenance-and-generation-publication.md)
- [`docs/implementation/change-records/M7-002-stage-handoff-provenance-amendment.md`](docs/implementation/change-records/M7-002-stage-handoff-provenance-amendment.md)
- [`docs/implementation/change-records/M7-003-provenance-schema-closure.md`](docs/implementation/change-records/M7-003-provenance-schema-closure.md)
- [`docs/implementation/reviews/2026-10-23-m7-003-sol-acceptance.md`](docs/implementation/reviews/2026-10-23-m7-003-sol-acceptance.md)

Later M7 records document implementation hardening and are included for
traceability. They do not silently expand the scientific claims.

## Current boundary

The synthetic provenance contract foundation through M7-007 is accepted. The
next native Stage-B producer candidate, M7-008, is not integrated or accepted:
261 B-T07 expectations pass, but one AC3 assertion still disagrees over the
fail-closed label `partial` versus `incomplete`. See
[`docs/implementation/reviews/2026-10-24-m7-008-b-t07-producer-blocker.md`](docs/implementation/reviews/2026-10-24-m7-008-b-t07-producer-blocker.md).

This branch is suitable for studying the design. It is not evidence that the
pipeline is clinically validated, biologically validated, or ready for use on
real study data.
