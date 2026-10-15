# B-T06 reopening Astra audit

- Date: 2026-10-15
- Auditor: `/root/bt06_reopen_audit`
- Requested role: Astra low, read-only
- Snapshot: `1acef834aeed4090dae7cc9e28416587c388a5ad`
- Verdict: **PASS WITH REQUIRED CHANGES**
- Focused rereview: **PASS** on `0dfe88d91ab98d2dd0045f6bab5ea78ce4270065`

The audit independently reproduced completed Wilcoxon sensitivity for the
incompatible `(0, 1e-9)` contrast. It found that finite, nonzero, exact-opposite
coefficients correctly identify scalar multiples of the two-group difference;
negative scaling reverses orientation while preserving the two-sided p-value.

Required before acceptance:

1. Remove the subtraction-based finiteness gate and derive direction from the
   target coefficient. Add an extreme finite `(-1e308, 1e308)` regression because
   subtraction currently overflows and rejects that valid structural contrast.
2. State that all six absent packet files are restored unchanged before the new
   repair edits only the R/test pair.
3. Preserve exhausted historical counters. Omar must explicitly authorize the
   one-time additional review of repair 2; no further repair or re-review is
   available, and the independent M4 Astra gate remains mandatory.

Private study data were not needed or accessed. The audit does not authorize
implementation or accept M4-004 on Omar's behalf. The required wording was
applied for one focused re-review. That rereview confirmed all three required
changes resolved: overflow-safe eligibility/tests, six-file restoration with
two-file repair scope, and bounded one-time review authorization. M4-004 still
needs Omar's acceptance.
