# M7-004 fresh Astra acceptance audit

- Date: 2026-10-24
- Implementation snapshot: `5dcc25f`
- Integration receipt commit: `76e16bb7c55843f68382051a89e124645c95448e`
- Audit brief commit: `c3444ef`
- Reviewer: `/root/m7_004_astra_acceptance_audit` (fresh read-only Astra-low)
- Verdict: **changes required**
- Findings: Critical 0, Important 2, Minor 0
- Ready for Sol acceptance: **no**

## Confirmed closures

The auditor confirmed that the five previously reproduced fail-open paths have
explicit rejection checks and regression fixtures. Positive multiple-block,
multiple-method, multiple-preprocessing-state, and multiple-route fixtures remain
present. The auditor also verified exact equality of all four packet-owned blobs
between candidate `647192f` and integrated snapshot `5dcc25f`, plus the Sol-owned
`V8` Imports registration.

Closed-schema validation, canonical hashing, graph resolution, predecessor-pair
mapping, and current-provenance-frame exclusion showed no additional blocker in
the bounded inspection.

## Important findings

### M7-004-ASTRA-01 — represented-context coverage is not joined to analysis context

At immutable `R/provenance_contract.R:638–645`, unit-map groups are compared to
resolved-analysis groups, while `:599–608` derives method-state requirements
from unit maps. Neither check requires each represented-matrix context to be
covered by the unit/design/method context.

Static reasoning path: in a valid B profile, move both the represented matrix
and its matching preprocessing state from `prep-1` to `prep-2`, retain the
unit/design/method descriptors at `prep-1`, and refresh the identity digest. The
implemented pairwise comparisons remain satisfied even though the represented
context lacks its required analysis/method evidence. This path was not executed
because the audit prohibited test runs.

### M7-004-ASTRA-02 — migration proof set omits mandatory handoff payload

At immutable `R/provenance_contract.R:738–740`, expected migration proofs are
built from `required_by_role`; its definition at `:568–573` omits the mandatory
`handoff_payload`. A complete proof object that includes
`handoff_payload = "proven"` is therefore treated as surplus, while an otherwise
matching object without that proof can pass this check. The existing migration
fixture at `tests/testthat/test-M7-003-provenance-contract.R:400–417` uses an
unknown proof field and does not establish valid complete-proof acceptance.

## Disposition

M7-004 is not accepted and no gate is released. The implementation is integrated
and its recorded focused/full-suite gates are green, but those results do not
cover these two static completeness paths. The initial implementation and both
permitted repairs are consumed. A new product repair requires Omar's explicit
authorization of a bounded change record, with RED fixtures for both findings,
focused GREEN, and a fresh read-only acceptance audit of the repaired immutable
snapshot. Do not rerun the existing Luna worker or start downstream Stage D.

No tests, edits, private-data access, biological interpretation, clinical claim,
acceptance, or gate change occurred in this audit.
