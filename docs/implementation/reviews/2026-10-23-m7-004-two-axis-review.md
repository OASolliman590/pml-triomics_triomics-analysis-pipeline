# M7-004 two-axis code review

- Date: 2026-10-23
- Candidate: `65662bcaf67f59bd3fbf5440984dc7711444daca`
- Fixed point: `221df65a20e6a53bf523a4f5592a9ffe1e006ec8`
- Reviewers: `/root/m7_004_standards_review`, `/root/m7_004_spec_review`
- Product edits by reviewers: none
- Overall disposition: **changes_required**

## Standards

Verdict: **pass**. Critical 0, Important 0, Minor 1.

No hard violation of `AGENTS.md`, `CLAUDE.md`, or DESCRIPTION's testthat edition
was found. No credential, clinical identifier, private path, upload, or unsafe
execution was introduced, and no existing test was removed or weakened. The
registration request correctly leaves shared dependency registration to Sol and
does not claim acceptance.

Minor judgement call: `tests/testthat/test-M7-003-provenance-contract.R` has an
identical `if (stage == "B") ... else ...` branch when appending the synthetic
handoff payload. The factory already receives `stage`, so the branch is possible
Duplicated Code. This is nonblocking.

## Spec

Verdict: **changes_required**. Critical 0, Important 2, Minor 0.

1. Required scope groups are not proven complete. The public validator accepts a
   `block-1` represented matrix after moving the only feature registry to
   `unrelated-block`; it also accepts a C `block.spls` integration result/state
   pair after removing its corresponding `method_state`. M7-003 requires exact
   per-block and per-analysis/method coverage; zero is missing even when another
   valid group exists.
2. Extra evidence of the wrong route kind is accepted. A valid native route plus
   native sidecar still validates after adding a `clinical_evidence` descriptor
   with the same native route ID. M7-003 requires every descriptor to match
   exactly one declaration by kind, ID, and complete scope.

Both paths were reproduced through
`triomics_validate_stage_handoff_provenance()` with refreshed valid identity
hashes. The candidate otherwise stays within scope and preserves downstream
closures. The unrelated C-05 baseline gate remains separate.

## Required repair 1

The same Luna-max implementer must first add the three negative fixtures above,
observe RED, then enforce complete required-scope coverage and reverse
route-kind/ID/scope mapping. It may change only the existing four-file M7-004
allowlist. After focused GREEN and a clean repair commit, the same Spec reviewer
gets the single focused rereview. No full suite or integration occurs before that
rereview passes.
