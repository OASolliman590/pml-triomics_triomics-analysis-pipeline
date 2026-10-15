# B-T05 reopening Astra audit

- Date: 2026-10-15
- Auditor: `/root/bt05_reopen_audit`
- Requested role: Astra low, read-only
- Snapshot: `1acef834aeed4090dae7cc9e28416587c388a5ad`
- Verdict: **PASS WITH REQUIRED CHANGES**
- Focused rereview: **PASS** on `0dfe88d91ab98d2dd0045f6bab5ea78ce4270065`

The audit found the candidate code demonstrates the dispatch defect and that
routing supported DEqMS lanes independently of count validity implements AC4
without broadening methods or lanes.

Required before acceptance:

1. Replace the candidate tests that require missing-count level requests to
   bypass B-T05. Cover `NULL` and nonexistent paths with both fallback settings,
   named reasons and method provenance; preserve delta/unsupported-lane checks
   and verify source and installed CLI behavior.
2. State that all four absent packet files are restored unchanged before the
   new repair edits only the R/test pair.
3. Preserve the exhausted historical review and attempt counters. Authorize
   repair 2, no further repair, exactly one reopening review, and retain the
   later independent M4 Astra gate.

Private study data were not needed. The audit does not authorize implementation
or accept M4-003 on Omar's behalf. The required wording was applied for one
focused re-review. That rereview confirmed all three required changes resolved:
the adversarial matrix, four-file restoration with two-file repair scope, and
the bounded repair/review authorization. M4-003 still needs Omar's acceptance.
