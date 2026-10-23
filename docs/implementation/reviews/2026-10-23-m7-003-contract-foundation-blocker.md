# M7-003 contract-foundation implementation blocker

- Date: 2026-10-23
- Coordinator: Sol
- Worker: `/root/m7_003_contract_luna`
- Requested role: Luna max
- Accepted base and worktree HEAD: `221df65a20e6a53bf523a4f5592a9ffe1e006ec8`
- Candidate commit: **none**
- Status: **blocked; initial implementation plus two targeted repairs consumed**

## Authorized scope

The worker was limited to four new files: the R provenance contract helper, the
Stage Handoff Provenance JSON Schema, its focused behavioral test file, and a
packet-owned registration request. Producer seams, D verification/publication,
migration, shared registration, D-T01 acceptance, M7 release, and every Stage-D
successor remained closed. Only public synthetic fixtures were authorized.

## Test-first evidence

The pre-implementation focused command was:

```sh
Rscript --vanilla -e 'testthat::test_local(filter = "M7-003-provenance-contract", reporter = "summary")'
```

It exited 1 at testthat's ten-failure cap because the canonicalizer, identity
hash, validator, and schema did not yet exist. This was a meaningful RED.

The same focused command exited 1 after each of the three permitted
implementation passes:

1. Initial implementation: seven issues (one failure, six errors), including an
   incorrect key-order expectation, a bad schema-helper path, malformed fixture
   scope, and a missing integration-result fixture lookup.
2. Targeted repair 1: eight issues (two failures, six errors); valid B/C fixtures
   lacked the required handoff payload digest.
3. Targeted repair 2: eight issues (two failures, six errors); valid profiles
   reach `sort(NULL, method = "radix")` for empty
   `derived_from_entity_ids`, and an embedded-JSON fixture removes `size_bytes`
   instead of retaining it as JSON null. One mutation consequently surfaces the
   base R error instead of `D_PROVENANCE_INVALID`.

The mandatory focused-Astra Minor probe exists in the uncommitted test file: a
native-sidecar descriptor whose non-null `method_id` differs from its declared
route must fail `D_PROVENANCE_INVALID`. It was not established GREEN because the
focused file remains failing.

## Disposition

Sol stopped further edits under the one-initial-plus-two-repair limit. The full
suite was not run because the focused gate already failed; an immediately
started run was interrupted before producing a result. No registration request
was created, no commit was made, and no Sol review was dispatched. The isolated
worktree contains exactly three uncommitted allowlisted files and remains at the
accepted base commit. `git diff --check` reported no tracked-file whitespace
error, but this is not acceptance evidence for untracked files.

M7-003 remains accepted as a specification amendment, but its contract
foundation is not implemented or integrated. D-T01, M7, and every Stage-D
successor remain closed. Further implementation requires Omar's explicit
authorization of another bounded repair record; it must not be treated as a
continuation of the exhausted packet.
