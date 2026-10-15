# M4-005 shared-integration record Astra audit

- Date: 2026-10-15
- Auditor: `/root/m4_005_astra_audit`
- Requested role: Astra low, read-only
- Snapshot: `cfa0be4b80cfafa3a736769c687797e6e0262708`
- Initial verdict: **PASS WITH REQUIRED CHANGES**
- Corrected snapshot: `279ecc430fa9cc76e93a3d2a8dd898ef1c5637a0`
- Focused rereview: **PASS**

The auditor found no P0 safety or scientific blocker. Separating generic artifact
completion from analytical method outcome is consistent with the frozen data
contract, the proposed provenance fields can represent B-T05 AC4, and the record
preserves exhausted packet counters, private-data restrictions, B-T04 controls
and Sol ownership of shared registration.

Two P1 corrections were required before execution acceptance:

1. The restored B-T05 CLI test explicitly required the known invalid fallback
   outcome: exit 2, `A_ARTIFACT_INVALID` and payload status
   `fallback_complete`. M4-005 now permits only that named test block to change
   and requires exit 0, valid persisted artifact/run statuses, generic artifact
   `complete` or `partial`, retained method status and explicit limma provenance.
   A separate partial-fallback regression is also required.
2. The draft incorrectly required normal G4 acceptance before B-T07 even though
   B-T07 is itself a required M4 packet. The record now adds one bounded
   immutable M4-005 shared-integration review before B-T07 release. The normal
   M4/G4 review remains after B-T07 and retains its own frozen allowance.

The focused rereview confirmed both P1 blockers resolved without a new
contradiction: only the stale CLI test may change, complete and partial persisted
fallback states are required, the bounded M4-005 review precedes B-T07, and the
normal G4 review follows it with its allowance unchanged.

The audit did not authorize implementation, accept M4-005 for Omar, release
B-T07, or provide scientific validation. Omar's explicit execution acceptance is
still required.
