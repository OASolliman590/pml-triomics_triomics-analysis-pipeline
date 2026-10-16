# Implementation handover

Run `triomics-20260920-sol-01` is paused on branch
`spec/harden-v2-20260920` at `11b288c`. G0 through G3 remain accepted. Stage B
packets B-T01 through B-T06, B-T08 and B-T09 are accepted. B-T07 is implemented
and integrated through M4-011, M4-012 and M4-013, but remains unaccepted because
the mandatory installed-package gate failed. Therefore G4 and Stage C remain
unreleased.

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

The fresh Luna-max worker `/root/m4_014_bt07_luna` is active in the isolated
`m4-014-bt07-package` worktree against snapshot `fe9e870`, with only
`tests/testthat/test-B-T07.R` and `R/export_b.R` allowlisted. Its next legal
transition is candidate inspection and fresh Sol-high review. Older completed
managed worktrees remain archived.
The user-owned `.gitignore`, AppleDouble files and `.superpowers/`
remain untouched.

## Literal resume instruction

Read `AGENTS.md`, `discovery/spec-v2/README.md`, the master Sol prompt,
`execution-ledger.json`, this handover, M4-011 through M4-014, and the M4-013
package-gate/Astra receipts. Verify HEAD and dirty state. M4-014 is authorized:
dispatch one fresh isolated Luna-max worker for the exact B-T07 source/test
scope, have Sol apply the manual-only edit, obtain fresh Sol-high review, run
source/installed B-T07 parity, the complete local suite and a clean archive
package check with no ERROR/WARNING/NOTE/required skip, then request the
immutable M4 Astra review. Accept B-T07 and G4 and release Stage C only if every
gate passes.
