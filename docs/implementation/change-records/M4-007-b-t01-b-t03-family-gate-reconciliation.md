# M4-007 — reconcile predecessor row status and q publication with B-T06

- Date: 2026-10-16
- Trigger: M4-005B's post-review complete suite passed 3,130 assertions but
  failed six predecessor assertions after the B-T06 dispatcher became active.
- Status: **proposed bounded acceptance reconciliation; Astra audit required**
- Candidate: `ada3a17adebc388850321190bfdbcc9fb64b1152`
- Evidence: `docs/implementation/reviews/M4-005B-coordinator-gate.md`

## Finding

The M4-005B targeted matrix and fresh Sol review passed, but the required full
suite exposed behavior that could not be waived:

1. B-T06-AC3 requires missing or invalid selection inventory to withhold
   validated q publication. Three older B-T01/B-T03 assertions still expect q
   values from direct synthetic fits that deliberately provide no selection
   inventory. Those expectations predate B-T06 and conflict with its frozen
   fail-closed acceptance criterion. Internal p/q family accounting must remain
   available; published row q must remain null.
2. `triomics_b06_process_family()` currently replaces every row's
   `inferential_status` with `withheld` whenever family validation fails. That
   erases more specific established statuses: a failed fit is `not_estimable`,
   a reporting-only fit is `descriptive_only`, and an estimated predecessor fit
   without validated selection is `unverified`. Family-level `validation.status
   = withheld` and null published q already express the B-T06 gate. The blanket
   row-status overwrite is not required by B-T06-AC3.

This is an internal contract reconciliation, not a new statistical method or a
change to the family, FDR, Wilcoxon, estimand, threshold or B-T04 calibration.
No literature search is needed because the controlling rules are the frozen
B-T06 acceptance contract and existing typed row-status contract.

## Authorized correction only after an Astra pass

One fresh Luna-max worker, without child agents, may modify only:

- `tests/testthat/test-B-T01.R`
- `tests/testthat/test-B-T03.R`
- `tests/testthat/test-B-T06.R`

The worker first adds/updates behavioral assertions so that:

- missing selection inventory leaves published row q null while retaining the
  already calculated internal family q and the named
  `selection_inventory_missing` reason;
- adding another block still cannot change the original block's internal q;
- failed, descriptive-only and unverified predecessor row statuses are retained
  while the family validation remains withheld.

Sol alone may then change only the row-status publication loop in
`R/multiplicity.R`. The correction removes the blanket overwrite of
`inferential_status`; it must not change family validation, q withholding,
internal p/q calculation, nominations, rank sensitivity, dispatch, context or
any other packet code.

## Gates and limits

- One tests-only Luna implementation and one Sol-owned one-line production
  correction; together they are one M4-007 implementation attempt.
- One fresh Sol 6.1 high complete-diff review and at most one focused repair and
  focused rereview.
- Required GREEN suites: `B-T01|B-T03|B-T06|M4-005-stage-b-composition`, then
  the complete local suite, clean package build/check and source/installed
  B-T04/B-T05/B-T06 probes.
- M4-005B remains unintegrated until all gates pass. Its prior Sol review is
  retained as evidence but does not override the failed full-suite gate.
- The M4-005 shared Astra integration review remains required after integration.
- Private studies remain excluded. Synthetic fixtures only.

Any required change outside these four files, any scientific-contract change,
or any unresolved full-suite failure blocks M4-007 and M4-005. This record does
not reset M4-005A/B counters, accept B-T05/B-T06, release B-T07, or replace G4.
