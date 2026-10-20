# C-06 shared integration verification and pause receipt

Date: 2026-10-21  
Authority: Sol coordinator  
Scope: C-06 shared Stage-C CLI/D-handoff cycle only

## Integrated changes

- Packet commits were integrated as `7d29da4` and `634c29f`.
- Shared candidates `1cc3504`, `be2a4e4`, and final repair `29e1e19` were
  integrated serially as `32ab4d1`, `a658398`, and `3753be0`.
- The final repair changed only `R/cli.R`, `R/integration_comparison.R`,
  `tests/testthat/test-C-06-shared.R`, and the C-06 registration record.
- Direct inspection found a clean candidate worktree, the expected four-file
  diff, and no whitespace errors.

## Fresh integrated evidence

- `testthat::test_local(filter='C-06', stop_on_failure=TRUE)` passed 290
  expectations with 0 failures, 0 warnings, and 0 skips.
- The exact C-06 Stage-C CLI exited 0. It wrote a `partial` D handoff because
  C-07 controls were explicitly not run, not because an attempted analysis
  silently failed.
- The handoff has five payload/envelope pairs plus its manifest envelope. Every
  recorded SHA-256 matched the fresh file bytes. Manifest SHA-256:
  `ecc9b0ffd794c902aa2644e5e7fbacc74cfbee7fc562ae5ad0100cc2e1cc1fbb`;
  envelope SHA-256:
  `5053e22e173ef3aaa893a5a37f7a95f629a9ee9c6c9bc5f558807db20b7eb680`.
- The primary-statistics branch is `not_requested`. All four C-07 controls are
  `not_run`, carry no metrics, and use `split_plan_status=not_run` with a null
  split-plan hash. C-09 is `not_started` and conditional.
- The exact accepted C-02 fixture was rerun only as a regression check after
  integration. It exited 0, predicted 12/12 eligible synthetic units, retained
  the correct root study/analysis identities, and left the single-fit D export
  unavailable without downgrading its complete nested evaluation. Run registry
  status was complete. Manifest SHA-256:
  `56d720c76c687fc7a088a54321d618eedd773ec5811f64d6caeacf472d877647`;
  envelope SHA-256:
  `6b1030f3d3d0ee8503db1a6220bdc11f601737caf90cd9f15b2b563b46a45ecf`.
  Six visible mixOmics warnings reflect the tiny synthetic study folds with
  fewer than five samples; they are not biological evidence.

## Disposition

C-06 shared integration is implemented and verified on public synthetic
software evidence. It is **not yet accepted at M6**: no clean package gate or
immutable Astra milestone audit was run in this cycle. C-09 shared registration
is still pending, G6/Stage D remain closed, and no private study data were read
or fitted. The run is paused at Omar's request after this cycle.
