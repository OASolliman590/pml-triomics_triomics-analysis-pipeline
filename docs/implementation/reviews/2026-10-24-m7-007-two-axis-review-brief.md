# M7-007 two-axis code-review brief

- Date: 2026-10-24
- Product fixed point: `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Authorization base: `dfceea7`
- Implementation commit: `0b1fcbdbb321a8e6caa002d19e769d693d53d2bd`
- Registration repair 1: `b34df9f5d93ca60d74f690a2e86e5f416547fec6`
- Candidate HEAD: `b34df9f5d93ca60d74f690a2e86e5f416547fec6`
- Review axes: one fresh read-only Standards reviewer and one fresh read-only
  Spec reviewer

## Authorized scope

Review the M7-007 candidate against
`docs/implementation/change-records/M7-007-collision-free-provenance-context-keys.md`
and the prior blocker
`docs/implementation/reviews/2026-10-24-m7-006-astra-acceptance-audit.md`.
The product change may touch only:

- `R/provenance_contract.R`;
- `tests/testthat/test-M7-003-provenance-contract.R`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json`.

The complete candidate consists of the two commits after authorization base
`dfceea7`; the product comparison is `e4aed59...b34df9f` and therefore also
contains coordinator documentation. Distinguish coordinator records from the
three-file worker allowlist.

## Evidence supplied, not to be rerun

- RED: focused M7-003 suite exited 1 with exactly one failure, zero errors,
  warnings, or skips because the public validator accepted the schema-valid
  escaped-U+001C collision profile.
- GREEN: the same focused suite exited 0 with `DONE`, zero failures, warnings,
  or skips. The collision profile rejects and a valid fully covered escaped
  identifier/multiple-context profile passes.
- `git diff --check` and registration JSON parsing passed; worktree is clean.
- Registration repair 1 changed only the stale root `next_gate`; tests were not
  rerun because product code and tests were unchanged.

## Standards axis

Review exact allowlist compliance, repository rules, public-seam behavioral test
quality, warning/skip handling, registration truthfulness, privacy, closed
Stage-D scope, and baseline code smells. Do not edit files or run tests.

## Spec axis

Decide whether length-prefixed UTF-8 byte encoding is collision-free for exact
tuple values and field boundaries without banning U+001C or narrowing schema
content. Check the negative collision fixture, positive legal escaped-identifier
multi-context fixture, preservation of earlier context/migration/route checks,
and `candidate/review_pending`/`accepted: false`. Do not edit files or run tests.

Each reviewer returns `pass` or `changes_required`, Critical/Important/Minor
counts, and `ready_to_integrate: yes|no`. Review does not accept any milestone
or authorize downstream Stage D.
