# M4-011 — B-T07 same-run matrix and post-B registration

- Date: 2026-10-16
- Trigger: accepted B-T07 registration request and repair-1 focused rereview
- Packet registration: `docs/implementation/registrations/B-T07.json`
- Packet review: `docs/implementation/reviews/B-T07-repair1-rereview.md`
- Status: approved shared integration; implementation pending
- Attempt accounting: this is the packet's declared Sol-owned shared registration
  boundary. It does not consume B-T07 targeted repair 2 unless the accepted
  packet-owned implementation itself must change again.

## Frozen boundary

B-T07 must hand Stage C the exact keyed representation matrices used by the same
Stage B fit. It may not re-ingest, re-resolve, reconstruct from result rows,
synthesize or inspect private/native fitted objects. Existing Stage B result JSON
and envelope remain unchanged; the matrices are retained only in the in-memory
return long enough for the post-B exporter to write its typed, hashed handoff.
Existing B-T09 and B-T08 hooks keep their current behavior and ordering.

## Unit A — exclusive Luna shared-behavior implementation

One fresh Luna-max worker in an isolated worktree may modify only:

- `R/results_b.R`;
- `tests/testthat/test-B-T07.R`.

It must first add a failing behavioral test proving that
`triomics_execute_stage_b()` currently omits the exact same-run named matrices.
The test must compare each returned keyed matrix hash with the corresponding
public result `source_hash`, must prove a value mutation changes the hash, and
must confirm that the on-disk Stage B result artifact does not serialize the
matrices or native model objects. The implementation may then return only a named
`resolved_matrices` list sourced directly from
`resolved$blocks[[block_id]]$assay` used by that fit. No public artifact schema,
method, threshold, model or dependency changes.

## Unit B — Sol-owned mechanical registration

After Unit A passes fresh Sol-high review and is integrated, Sol may modify only:

- `R/cli.R`, assigning the Stage B return and invoking
  `triomics_b07_cli_after_stage_b(args, stage_b_result)` after the existing B-T09
  and B-T08 hooks;
- `NAMESPACE`, exporting `triomics_b07_export_handoff` and
  `triomics_b07_cli_after_stage_b`;
- `man/triomics-api.Rd`, documenting the two exported interfaces and arguments.

No stage, schema, dependency, lockfile, fixture or scientific-contract change is
authorized. The hook must run only after successful non-validation Stage B.

## Required gates

1. Unit A RED/GREEN evidence and fresh Sol-high task review.
2. Serial integration followed by the exact B-T07 focused suite and exact CLI.
   The CLI must create a verified handoff, matrix manifest/values, reproduction
   record and acceptance record; AC3 must pass on same-run provenance. Existing
   B-T08/B-T09 and Stage B composition tests must remain green.
3. Fresh Sol-high review of the complete shared diff, then complete local suite
   and clean-archive package check with no failure, required skip, package ERROR,
   WARNING or NOTE.
4. Immutable M4 Astra review before B-T07 acceptance, G4 or Stage C release.

Any need to change packet export semantics, reconstruct matrices, weaken tests or
alter science exceeds this record and returns to the remaining packet repair
budget or a new material change record as appropriate.
