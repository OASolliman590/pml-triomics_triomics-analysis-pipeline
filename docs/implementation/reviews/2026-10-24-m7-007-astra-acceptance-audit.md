# M7-007 fresh Astra acceptance audit

- Date: 2026-10-24
- Reviewer: `/root/m7_007_astra_acceptance_audit`
- Requested role/model: fresh read-only Astra-low
- Immutable product snapshot:
  `b29c3aa5693b29ac232f26344987877e4e79511d`
- Dispatch-record commit: `dd0acd3`
- Verdict: **pass**
- Findings: Critical 0, Important 0, Minor 0
- `ready_for_sol_acceptance`: **yes**

## Finding closure

`M7-006-ASTRA-01` is closed. `triomics_provenance_group_key()` now encodes the
ordered component count and each exact UTF-8 value with a byte-length prefix.
Component boundaries therefore remain unambiguous when values contain U+001C,
digits, colons, or multibyte characters. Empty identifier strings are forbidden
by the schema and required-scope checks; the encoding would still represent an
empty component unambiguously as `0:`.

The public-validator negative fixture reproduces the former U+001C collision,
refreshes the identity digest, confirms schema validity, and now requires
`D_PROVENANCE_INVALID`. The positive fixture retains a legal U+001C identifier,
supplies all five required context roles, refreshes identity, and remains valid.

The executable delta changes only the tuple-grouping helper. The reviewer found
no regression in the existing provenance, migration-proof, role/type/scope,
cardinality, route, inventory, graph, canonicalization, or digest controls. The
registration remains the audited candidate handoff with
`candidate/review_pending` and `accepted: false`; only Sol may record acceptance.

## Evidence boundary

The reviewer inspected the recorded focused and full-source-suite evidence and
did not rerun tests. The full suite retains fourteen known C-05 native MOFA
small-fixture warnings and no numeric PASS total. No files were edited and no
private data were accessed. This is public synthetic software-contract evidence,
not real-study, biological, migration-execution, D-T01, M7, or clinical evidence.
The audit itself accepts nothing and releases no downstream gate.
