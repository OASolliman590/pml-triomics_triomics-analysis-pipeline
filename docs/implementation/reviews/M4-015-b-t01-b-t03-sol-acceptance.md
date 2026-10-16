# M4-015 — Sol acceptance of B-T01, B-T02 and B-T03

- Date: 2026-10-16
- Authority: Sol coordinator, correcting `M4-G4-001`
- Disposition: **B-T01, B-T02 and B-T03 accepted now**
- Scope: record-only; not a retroactive claim of earlier acceptance

The three packet implementations and integrations are present at their recorded
commits. Their acceptance criteria are exercised by their named suites,
including hand-derived/sign-reversal and native-limma checks; multigroup,
continuous, alias and mean-retaining checks; and paired delta,
difference-in-change, attrition, covariate and identical signed-map checks.

Fresh combined verification passed 749 expectations with zero failures, errors,
warnings or skips (B-T01 255, B-T02 208, B-T03 286). M4-007 independently
reviewed the reconciled B-T01/B-T03 family behavior with P0/P1/P2 zero. The
integrated full suite passed 3,462 expectations with no failures, warnings or
skips, and the clean installed-package check ended `Status: OK`.

The historical `sol_verified` states and earlier isolated-run notes remain in
Git and orchestration events. Current stale blockers are cleared. Acceptance is
limited to the frozen synthetic software contracts and makes no real-data or
biological claim.
