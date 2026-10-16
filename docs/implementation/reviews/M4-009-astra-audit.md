# M4-009 Astra audit

- Date: 2026-10-16
- Auditor: `/root/m4_009_astra_audit` (`gpt-6-astra`, low)
- Immutable snapshot: `c774737`
- Verdict: **FAIL**; P0 0, P1 2, P2 1
- Literature review: not required; the frozen contract and registration request
  fully determine this repair

## Confirmed findings

The three B-T08 acceptance findings are supported: constant responses can reach
an `estimated` result with non-finite inference, duplicate fixed-family members
are fitted and counted twice, and the six requested exports plus installed CLI
dispatch are absent. Marking a constant response `failed/constant_response`,
retaining it internally at p = 1, suppressing its published q-value, and
rejecting duplicate family members with `B_FAMILY_UNDECLARED` are compatible
with the frozen contract.

## Required record corrections

1. `clinical.enabled` alone is unsafe because the current B-T08 executor
   constructs synthetic endpoints unconditionally after ingestion. Dispatch
   must remain restricted to the declared B-T08 fixture/eligible route; B-T09
   and unsupported nonfixture configurations must never receive that fixture.
2. Sol's registration allowlist must include bounded changes to
   `man/triomics-api.Rd` so the six new exports do not break the warning-free
   package-check gate.
3. The record must not imply an additional uncounted correction: the Luna
   implementation consumes repair 1 and any later implementation correction
   consumes repair 2, the existing maximum.
4. Tests must cover mixed-family publication, exactly-once B-T08 source and
   installed dispatch, B-T09 exclusion/coexistence, and nonfixture refusal.

## Disposition

Sol must correct M4-009 and obtain the one focused Astra rereview before
dispatching repair 1. B-T08 remains blocked, and B-T07 remains dependency-illegal.
No private data, external upload, threshold change or biological claim is
authorized.
