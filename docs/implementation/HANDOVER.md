# Implementation handover

Run `triomics-20260920-sol-01` continues on branch
`spec/harden-v2-20260920`. G0 through G4 are accepted. All nine required Stage B
packets are accepted. Stage C dependency gates are released; C-01 is the only
immediately legal packet, while C-02 waits on C-01 and C-03/C-04/C-05 remain
behind G5.

## Current evidence

- B-T07 packet commits were integrated as `8cf2d38` and `4a33f01`.
- Same-run matrix retention and CLI/API registration were integrated as
  `f1ed853` and `c5f868f`.
- B-T07 repair 2 was integrated as `1146b5a`; its focused suite passed 124
  expectations and exact CLI AC3 passed. One Sol P1 was transparently retained
  and overruled by Astra as a P2 traceability ambiguity because the frozen
  contract permits verified aggregate unavailable status without the stripped
  optional method fields.
- M4-013 cross-packet composition candidate `57474e7` passed fresh Sol-high
  review and was integrated as `bd7da66`. B-T04/B-T07/B-T08/B-T09 plus the
  composition suite passed 870 focused expectations; four exact CLIs exited 0
  and 34 envelopes validated. Numerical results, B-T04 calibration and clinical
  privacy behavior were unchanged.
- The complete local suite on `bd7da66` passed with zero failures, warnings or
  skips.
- The clean Git-archive package check then failed with one ERROR, two WARNINGs
  and one NOTE: 15 installed B-T07 fixture-path/runner failures, one non-ASCII
  em dash in `R/export_b.R`, one unqualified `setNames()`, and duplicated
  `resume` documentation. Evidence:
  `reviews/M4-013-package-check-gate.md`.

These are packaging/test-infrastructure defects; no scientific regression was
demonstrated. They still cannot be waived. No private study data under the
authorized external data folder was read, fitted, copied, uploaded or committed.
All evidence remains public synthetic software evidence only.

## M4-014 authorization recorded

B-T07 consumed its initial attempt and two repairs. Omar explicitly authorized
the bounded M4-014 exception on 2026-10-16 after Sol restated the exact scope.
The exact record is
`change-records/M4-014-b-t07-package-gate-exception-proposal.md`. It permits only
`tests/testthat/test-B-T07.R`, the two mechanical `R/export_b.R` corrections,
and the Sol-owned duplicate Rd consolidation. It does not reset counters,
authorize a scientific change, or waive any verification gate.

Luna candidate `4acbd71` changed exactly `tests/testthat/test-B-T07.R` and
`R/export_b.R`; source and installed focused runs each passed 124 expectations
with zero failures or skips. Sol-owned manual cleanup is commit `7194b23`.
Fresh Sol-6.1 high review passed with P0/P1/P2 zero, and the candidate was
serially integrated as `01c5e49`. Fresh source verification then exposed a
path-with-spaces defect in the newly changed source CLI helper: Rscript receives
the absolute script argument with spaces transformed to `~+~`, so four source
CLI smokes failed before dispatch. Luna repair `74335d1` changed only the test
helper and passed 124 expectations from a spaced checkout, the integration
snapshot and an installed package, with zero failures or skips. Fresh focused
Sol-6.1 high re-review marked the finding ADDRESSED with no new P0/P1, and the
repair was integrated as `cbe5400`. Fresh integrated evidence at `d2a2231` is:
source B-T07 124/124 with no warnings/skips; installed B-T07 124/124 with no
failures/skips; full suite 3,462/3,462 with no warnings/skips; clean archive
package check `Status: OK` with no ERROR/WARNING/NOTE. The immutable Astra review
returned `changes_required` only because B-T01/B-T02/B-T03 lacked explicit
acceptance records. Sol applied record-only M4-015 after a fresh 749-expectation
combined pass; all three are accepted now, not retroactively. Fresh read-only
reviewer `/root/m4_g4_focused_astra` is active on record-only correction
snapshot `0f4be24` for the single focused rereview of M4-G4-001. That rereview
passed, and Sol accepted B-T07/G4 and released Stage C. No worker is active at
this exact handover point. Older completed managed worktrees remain archived.
The user-owned `.gitignore` and AppleDouble files remain untouched. The
git-ignored `.superpowers/` contains review packages only and remains untracked.

## Literal resume instruction

Read `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, the M4 milestone and M4-G4 review
receipts. Verify HEAD and dirty state. G4 is accepted. Dispatch one fresh
isolated Luna-max worker for C-01 only, using its packet card and relevant C
contracts. Inspect the candidate, obtain fresh Sol-high review, integrate and
run the named gates. Do not dispatch C-02 before C-01 acceptance, and do not
dispatch C-03/C-04/C-05 before G5.
