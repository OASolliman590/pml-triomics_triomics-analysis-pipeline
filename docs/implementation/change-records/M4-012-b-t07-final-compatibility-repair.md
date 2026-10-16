# M4-012 — B-T07 final compatibility repair

- Date: 2026-10-16
- Trigger: first integrated M4-011 Unit B run
- Status: approved bounded repair 2; B-T07 remains unaccepted
- Attempt accounting: consumes B-T07 targeted repair 2 of 2

## Observed integrated failure

The exact B-T07 CLI completed with exit 0, wrote the verified matrix handoff,
reproduction record and acceptance record, and marked AC3 pass. The package-aware
B-T07 suite then exposed two integration assumptions:

1. The B-T05 unavailable-DEqMS smoke wrote its typed unavailable Stage B result,
   but the unconditional B-T07 post-stage hook tried to export family-backed
   claims that intentionally do not exist and changed the CLI exit from 0 to 2
   (`B_EXPORT_INCOMPLETE`).
2. AC2 and AC3 tests invoked the CLI and then attempted a second export into the
   same output/cache identity. Before M4-011 the CLI did not create a B-T07
   handoff; after registration this correctly fails closed as an existing bundle.

These are runner/export lifecycle seams. No model, threshold, family definition,
matrix hash, acceptance criterion or scientific interpretation changes.

## Frozen repair boundary

The same B-T07 Luna implementer may modify only `R/export_b.R` and
`tests/testthat/test-B-T07.R` in the isolated packet worktree rebased to the
integrated `c5f868f` snapshot.

- `triomics_b07_cli_after_stage_b()` may return an explicit non-error unavailable
  disposition only when the completed Stage B result itself is typed unavailable
  and contains no exportable public block claims. The Stage B unavailable result
  and method disposition remain on disk; no empty or invented C handoff is made.
- Partial/complete Stage B results with claims must still run the B-T07 exporter;
  malformed or missing same-run matrices must still fail closed.
- AC2 must still execute independent, paired and unavailable-method CLI commands,
  assert signed effects and the visible unavailable disposition, and construct
  its consolidated receipt in a fresh output using same-run matrices rather than
  overwriting a CLI-created bundle.
- The caller-supplied/unverified AC3 test must use a fresh Stage B result without
  colliding with the integrated CLI handoff. It must retain mutation rejection,
  cache-dependency checks and the blocked unverified provenance verdict.
- Add or retain an integrated CLI assertion that B-T07 produces a verified
  same-run handoff with AC3 pass. Do not weaken restart, hash, reproduction,
  unavailable-method or private-data protections.

## Gates

Run the exact package-aware B-T07 suite and exact B-T07 CLI. A fresh Sol-high
review must confirm the guard is limited to the typed unavailable/no-claims
case and that AC2/AC3 are still substantively tested. No further B-T07 repair is
available after this attempt; unresolved correctness work becomes blocked.
