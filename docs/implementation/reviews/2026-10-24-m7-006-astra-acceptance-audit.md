# M7-006 fresh Astra acceptance audit

- Date: 2026-10-24
- Reviewer: `/root/m7_006_astra_acceptance_audit`
- Requested role/model: fresh read-only Astra-low
- Immutable repaired product snapshot:
  `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Evidence/brief commit: `4083de5`
- Verdict: **changes_required**
- Findings: Critical 0, Important 1, Minor 0
- `ready_for_sol_acceptance`: **no**

## Closed finding

`M7-004-ASTRA-02` is closed. `handoff_payload` is now a mandatory role used by
the exact migration-proof role comparison. The public suite contains positive
complete-proof and negative omitted-handoff-proof cases. The registration
remains `candidate/review_pending` with `accepted: false`.

## Important finding: M7-006-ASTRA-01

The context-completeness repair is still partially fail-open because
`triomics_provenance_group_key()` serializes tuple fields by joining their raw
values with U+001C (`R/provenance_contract.R:386-388`). The new context checks
reuse those keys (`R/provenance_contract.R:647-665`), while the schema permits
escaped U+001C in `preparation_context_id` and `representation_id`
(`inst/schema/stage-handoff-provenance.schema.json:245-246`).

A collision is therefore possible without changing the apparent joined key.
For example, unit/design/method descriptors can use preparation
`p\u001cq`, representation `r`, while represented-matrix and preprocessing
descriptors use preparation `p`, representation `q\u001cr`. These are distinct
tuples but both join to the same key. With the identity digest refreshed and
the other role, pair, graph, and method constraints maintained, the validator
cannot distinguish the orphaned represented/preprocessing context.

The reviewer did not execute this probe, as required by the read-only/no-test
brief. Sol independently inspected the cited immutable lines and confirmed the
delimiter construction and permissive string schema. A collision-free tuple
encoding plus public regression fixtures is required before acceptance.

## Evidence disposition

The supplied focused and full-suite evidence remains valid software evidence
for snapshot `e4aed59`: zero failures, errors, or skips, with only the fourteen
known C-05 native small-fixture warnings. It does not cover the newly identified
adversarial identifier collision and therefore cannot override the audit.

M7-004 and M7-006 are not accepted. No automatic rereview is authorized in this
cycle. B-T07/C-06 producer provenance, migration, D verification/publication,
D-T01 acceptance, M7 release, and every downstream Stage-D packet remain
closed. No private study data were read or delegated.
