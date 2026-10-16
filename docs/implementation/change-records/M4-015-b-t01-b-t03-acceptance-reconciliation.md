# M4-015 — reconcile B-T01/B-T02/B-T03 acceptance accounting

- Date: 2026-10-16
- Trigger: `M4-G4-001` in `reviews/M4-G4-astra-review.md`
- Scope: record-only; no production, test, fixture, threshold, seed, method,
  schema, attempt counter or historical event change
- Status: correction applied; focused Astra rereview required

B-T01, B-T02 and B-T03 were implemented, integrated and Sol-verified, but their
ledger entries were never converted from `sol_verified` to explicit acceptance.
Their old blockers described the earlier absence of M4 review or isolated runs
without unfinished B-T04. Those statements are retained in historical events
but are no longer current blockers after later composition, family, full-suite
and package gates.

Sol now records acceptance on the existing evidence only. A fresh combined
B-T01/B-T02/B-T03 run passed 749 expectations with zero failures, errors,
warnings or skips. B-T01 and B-T03 also passed the M4-007 focused review (255 and
286 expectations) with P0/P1/P2 zero. The complete integrated suite passed
3,462 expectations without failure/warning/skip, and the clean archive package
check ended `Status: OK`. The implementation and integration commits remain
`ddc0ae5`/`2e4971e` for B-T01 and `bded42a`/`9cd36fe` for B-T02/B-T03.

This is acceptance on public synthetic software evidence. It does not establish
real-study performance or biological validity. B-T07 and G4 remain unaccepted
until the focused Astra rereview passes and Sol records the milestone decision.
