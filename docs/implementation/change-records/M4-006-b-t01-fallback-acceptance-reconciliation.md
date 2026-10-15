# M4-006 — reconcile the stale B-T01 explicit-fallback acceptance case

- Date: 2026-10-15
- Trigger: the fresh M4-005A Sol review found three mandatory adjacent-test
  failures in a B-T01 case that contradicts the already accepted B-T05 fallback
  contract.
- Status: **accepted for bounded execution under Omar's one-shot instruction after Astra-low PASS**
- Review evidence: `docs/implementation/reviews/M4-005A-sol-review.md`
- Audit: `docs/implementation/reviews/M4-006-astra-audit.md`
- Candidate preserved: `97e7c28715581b8dd5c070e41c52f5f4c0bf2523`

## Root cause

`tests/testthat/test-B-T01.R` asks for DEqMS with missing counts and
`fallback = "explicit_limma"`, but expects status `unavailable`, no applied
fallback and zero result blocks. Accepted M4-003 requires every supported DEqMS
request to enter B-T05 even when counts are absent and requires B-T05 to return
either named unavailability or the explicitly requested limma fallback. B-T05
AC4 and M4-005 require that fallback to be separately labeled and serialized;
they do not permit silently ignoring it.

The M4-005A candidate follows those contracts. Its target suite, fresh install
and five source/installed parity cases passed. Only the contradictory B-T01
assertions fail. Changing production behavior to satisfy them would regress the
accepted method contract.

## Decision

Authorize one acceptance-test-only reconciliation after a fresh Astra-low audit:

- A fresh Luna max worker may modify only the test block named
  `DEqMS is unavailable and counts do not change the limma fit` in
  `tests/testthat/test-B-T01.R`.
- In that block, change only the unavailable-route request from
  `fallback = "explicit_limma"` to `fallback = "none"`.
- Keep its existing unavailable, no-fallback, zero-block and null-effects
  assertions. They then test the intended no-fallback route.
- Do not change production code, the restored B-T05 candidate, the M4-005A
  adapter, thresholds, estimands, method labels, B-T04 controls, canonical spec,
  package/CLI registration or any other test.

This choice preserves the B-T01 test's unavailable-route purpose. Complete and
partial explicit-fallback behavior remains covered by the dedicated B-T05 and
M4-005A tests, including method provenance and absence of false DEqMS claims.

## Verification and review

The worker starts from M4-005A candidate `97e7c28`, records the existing three
failures as RED evidence, makes the one requested test-input correction, and
runs:

1. B-T01 alone;
2. M4-005A plus B-T05;
3. the B-T01, B-T03 and B-T04 adjacent set.

No skipped required test may count as passed. The worker commits only the one
test file and stops. M4-005A's original Sol reviewer then uses its one allowed
focused rereview to verdict only the stale-test blocker and the correction diff.
If the rereview passes, Sol still performs the M4-005A coordinator installed
verification and package check before serial integration. If the correction or
rereview exposes another load-bearing defect, M4-006 and M4-005A remain blocked;
no second correction attempt is authorized.

## Scope and counters

M4-006 does not reopen or reset B-T01, B-T05, B-T06 or M4-005 implementation
counters. It does not consume M4-005A's unused in-allowlist repair because the
reviewer found no Unit A implementation defect. It authorizes one separate
test-only correction and uses M4-005A's already reserved focused rereview.

This record changes no scientific contract, statistical method or product
scope. It reconciles one obsolete test input with contracts already accepted by
Omar. Repository synthetic fixtures only are in scope; private study data remain
excluded.

## Execution gate

One fresh Astra-low reviewer must confirm that this record preserves the frozen
fallback semantics, does not hide a B-T01 regression, has sufficient tests, and
does not improperly reset attempts or reviews. Astra may pass, require a focused
record correction, or fail. It may not implement or accept the code. Execution
may start only after a pass.

## Audit disposition

A fresh Astra-low audit passed immutable snapshot
`3e755a4baf0b7068a04b9075eafa7d6fe5e8cd10` without required changes. Omar's
2026-10-15 instruction approved continuous one-shot execution of the accepted
plan and directed blocker review through the bounded reviewer/Astra process.
The audit pass therefore activates only the one test-input correction and
focused rereview defined here. It does not pre-accept M4-005A or release any
consumer.
