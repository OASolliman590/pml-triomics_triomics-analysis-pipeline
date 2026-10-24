# M7-007 fresh Astra acceptance-audit brief

- Date: 2026-10-24
- Reviewer: one fresh read-only Astra-low agent
- Immutable repaired product snapshot:
  `b29c3aa5693b29ac232f26344987877e4e79511d`
- Prior rejected snapshot: `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Candidate: `b34df9f5d93ca60d74f690a2e86e5f416547fec6`
- Sol acceptance authority: retained by the coordinator

## Authorization and scope

Omar authorized bounded M7-007 by instructing Sol to continue from the recorded
M7-006 blocker. Inspect immutable Git objects, not unrelated dirty checkout
content. Do not edit or commit files, run tests, read private study data, accept
any milestone, release a gate, or start producer, migration, D-T01, or
downstream Stage-D work.

Read the accepted M7-002/M7-003 amendments, prior M7-006 Astra receipt, M7-007
change record, two-axis review receipt, integration-verification receipt, and
the product delta `e4aed59..b29c3aa`.

## Finding that must be closed

`M7-006-ASTRA-01`: distinct ordered context tuples must never compare equal
merely because identifier values contain U+001C or another delimiter-like byte
sequence. Confirm the new count plus UTF-8 byte-length encoding is injective for
exact component sequences and preserves component boundaries without narrowing
the schema or normalizing values.

Confirm the public negative fixture reproduces the cited collision and rejects
with `D_PROVENANCE_INVALID`; the positive fixture retains legal escaped U+001C
values with all context roles covered; prior context, migration-proof,
role/type/scope, cardinality, route, inventory, graph, canonicalization, and
digest controls are not weakened; and registration remains
`candidate/review_pending`, `accepted: false`.

## Evidence supplied, not to be rerun

`docs/implementation/reviews/2026-10-24-m7-007-integration-verification.md`
records exact candidate-blob equality, focused exit 0/`DONE`, and uninterrupted
full source-suite exit 0/`DONE` with failures 0, errors 0, skips 0, and exactly
the same fourteen named C-05 native MOFA small-fixture warnings. The summary
reporter emitted no numeric PASS total. Treat this as software evidence only.

## Required verdict

Return `pass` or `changes_required`, Critical/Important/Minor counts, and
`ready_for_sol_acceptance: yes|no`. Cite immutable file/line locations or a
reproducible reasoning path for every finding. This is the only Astra audit
authorized for M7-007; do not request or perform an automatic rereview. A pass
permits Sol to accept only the repaired contract foundation in this run. It does
not accept D-T01 or M7 and does not authorize downstream Stage-D work.
