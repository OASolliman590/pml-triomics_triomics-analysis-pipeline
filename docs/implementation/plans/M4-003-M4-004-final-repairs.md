# M4-003 and M4-004 final repairs

Spec authority: `discovery/spec-v2/README.md`, B-T05/B-T06 packet cards and the
accepted change records M4-003 and M4-004. Execute tasks sequentially. These are
the second and final targeted repairs for each packet.

## Global constraints

- Private study inputs are excluded. Use only repository synthetic fixtures.
- Sol restores exact historical packet content onto the current integration
  snapshot before dispatch. Luna changes only the two repair files named below.
- Use TDD: add the specified regression first, run it and capture the expected
  failure, then make the minimal production change and rerun focused plus packet
  tests. Required tests may not be skipped.
- Each task gets exactly one fresh Sol 6.1 high review. If repair or review leaves
  a blocker, stop that packet as blocked; there is no further repair allowance.
- Sol alone integrates, edits the shared ledger/registration surfaces and runs
  adjacent, full-suite, CLI and package verification.
- Do not broaden methods, designs, estimands, thresholds, dependencies or scope.

## Task 1: B-T05 final dispatch repair

Authority: `docs/implementation/change-records/M4-003-b-t05-dispatch-reopening.md`.

Worktree starts from the current integration snapshot with all four files from
candidate commits `b1e753d3b9004737eaf68ecc9916da472f3f5754` and
`efd830884b894d79441af5a948fa3b600ff8e4db` restored unchanged. New edits are
restricted to:

- `R/statistics_deqms.R`
- `tests/testthat/test-B-T05.R`

Replace the regression that expects missing-count level requests to bypass
B-T05. Add real-behavior cases for both `NULL` and a nonexistent count path,
each with fallback `none` and `explicit_limma`. Assert named reason, requested
method, actual method and fallback provenance. Preserve valid-count DEqMS,
delta, non-DEqMS and unsupported-lane behavior. Demonstrate source and installed
CLI behavior. Route supported B-T01 level and B-T03 delta DEqMS requests into
B-T05 regardless of count validity; let B-T05 validation return the typed result.
Do not add support for B-T02 or another lane.

Run the B-T05 suite and synthetic CLI. Commit only the two repair files and write
the requested SDD report with RED/GREEN evidence. Stop after the commit.

## Task 2: B-T06 final scale-invariant eligibility repair

Authority:
`docs/implementation/change-records/M4-004-b-t06-scale-invariant-rank-eligibility.md`.

Worktree starts from the then-current integration snapshot with all six files
from candidate commits `033e0f6ea637cc7f9cc14643592d7453f2a8e643` and
`9b85bbd616a797c03f133fb43a5f0cce1260bb7d` restored unchanged. New edits are
restricted to:

- `R/rank_sensitivity.R`
- `tests/testthat/test-B-T06.R`

Add real-behavior regressions for canonical `(-1,1)`, incompatible `(0,1e-9)`,
tiny valid `(-1e-9,1e-9)`, reversed `(1,-1)` and extreme finite
`(-1e308,1e308)` contrasts. Assert positive/negative scaling preserves the
two-sided Wilcoxon p-value while direction follows the target coefficient.
Remove subtraction-based finiteness and the absolute tolerance. Eligibility
requires exactly the two declared condition columns, finite weights, nonzero
target coefficient and exact-opposite coefficients. Preserve ties, zeros,
`exact=FALSE`, continuity correction and unsupported-design behavior.

Run the B-T06 suite and synthetic CLI. Commit only the two repair files and write
the requested SDD report with RED/GREEN evidence. Stop after the commit.
