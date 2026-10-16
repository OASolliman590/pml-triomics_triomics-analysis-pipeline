# M4-009 coordinator package-check gate

- Date: 2026-10-16
- Candidate tip: `ed94035`
- Complete local suite: 3,298 passed, 0 failed, 0 warned, 0 skipped
- Clean-archive package check: **FAIL**, `Status: 1 ERROR`
- Installed tests: 3,204 passed; 3 errors and their 3 file-open warnings;
  0 skips
- Clean check root:
  `/private/tmp/triomics-m4-009-archive-check.3mHlNS`

The three errors are all in the new B-T08 CLI regression harness.
`b08_copy_ticket_examples()` assumes that
`testthat::test_path("..", "..")/inst/examples/tickets` exists. That is true
for `test_local()` but false when tests execute against the installed package,
so B-T08, B-T09 and nonfixture setup cannot open their ticket YAML files.
This is an installed-test path/context defect, not a failed statistical result.

The production candidate previously passed 268 B-T08 assertions, 145 adjacent
B-T09/composition assertions, all 3,298 local assertions, six installed exports,
and direct source/installed B-T08/B-T09/nonfixture probes. Fresh Sol review also
passed with no finding. Nevertheless, a required package check is not green and
cannot count as acceptance.

An initial build directly from the managed checkout also included `.git` after
the sandbox denied R permission to delete its temporary copied Git objects,
causing one warning and one note. Rebuilding from a clean `git archive` removed
both artifacts and isolated the genuine status above: one installed-test error
only.

Both B-T08 repair attempts are consumed. Under the frozen orchestration ceiling,
no further code change or focused Astra rereview is authorized in this run.
B-T08 is blocked at the package gate; B-T07 and all C/D work remain unreleased.
No private study data were read, fitted or uploaded.
