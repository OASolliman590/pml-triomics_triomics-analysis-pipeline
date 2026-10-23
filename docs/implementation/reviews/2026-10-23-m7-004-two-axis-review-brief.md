# M7-004 two-axis review brief

- Candidate: `65662bcaf67f59bd3fbf5440984dc7711444daca`
- Accepted review base: `221df65a20e6a53bf523a4f5592a9ffe1e006ec8`
- Diff: `git diff 221df65a20e6a53bf523a4f5592a9ffe1e006ec8...65662bcaf67f59bd3fbf5440984dc7711444daca`
- Commits: failing seed `8bc460b`, completion `65662bc`
- Product scope: M7-002 plus M7-003 contract foundation only
- Review workflow: independent Standards and Spec axes

## Evidence

The fresh Luna-max worker reproduced the seed RED with eight issues, then reached
focused GREEN with zero failures, warnings, or skips. The required native-route
descriptor with a non-null `method_id` different from the declared route is a
negative fixture and rejects with `D_PROVENANCE_INVALID`.

The worker's full suite was invalidated by restricted-worktree write permission.
Sol reran the immutable candidate with write permission. It traversed the whole
suite and the new M7-003 file remained green, but the source-tree run failed on
two pre-existing C-05 assertions that still demand an unaccepted registration.
Tracked `docs/implementation/registrations/C-05.json` has been accepted since
commit `cad313b`; neither C-05 file differs between the review base and candidate.
With the approved isolated MOFA2 runtime, the focused C-05 diagnostic reduces to
those same two stale assertions plus the fourteen already dispositioned native
small-fixture warnings. This baseline issue blocks overall-suite acceptance but
is outside the M7-004 diff and must not be silently waived or repaired by a
reviewer.

## Standards axis

Review only; do not edit. Apply `AGENTS.md`, `CLAUDE.md`, `DESCRIPTION` testthat
edition, and the fixed smell baseline supplied in the reviewer prompt. Report
hard documented-standard violations separately from judgement-call smells.

## Spec axis

Review only; do not edit. Compare the candidate with:

- `docs/implementation/change-records/M7-002-stage-handoff-provenance-amendment.md`;
- `docs/implementation/change-records/M7-003-provenance-schema-closure.md`;
- `docs/implementation/change-records/M7-004-contract-foundation-completion.md`;
- `docs/implementation/reviews/2026-10-23-m7-003-focused-astra-rereview.md`; and
- `docs/implementation/reviews/2026-10-23-m7-003-sol-acceptance.md`.

Report missing/partial requirements, scope creep, and apparently implemented but
incorrect behavior. Preserve every closed downstream phase.

Neither reviewer accepts the packet, edits files, fixes C-05, integrates,
dispatches, or releases a gate. Each returns Critical/Important/Minor counts and
`pass` or `changes_required` for its axis.
