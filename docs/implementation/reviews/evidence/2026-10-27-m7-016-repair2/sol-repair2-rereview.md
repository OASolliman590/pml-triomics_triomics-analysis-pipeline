# M7-016 repair 2 narrow rereview

Frozen candidate: `cfe916059c9e2e3b04f64cb96035d8777520b488`.
Parent: `f23b5a7c96cec1134b6d098850e90fbbfc420299`.

**Repair-1 finding 5: ADDRESSED.** No new Important/Critical finding in this
test-only fix. **Original finding 4: NOT ADDRESSED; acceptance remains blocked.**

Reviewed only the final diff for `tests/testthat/test-D-T01.R`, repair-2 worker
report, retained RED, extracted source/installed probe logs, and extraction
driver. No broader scan, suite execution, new test run, product edit, commit,
dispatch, integration, or acceptance. Git status was clean at the pinned SHA.

## Installed bootstrap disposition

The change at `tests/testthat/test-D-T01.R:1348` onward tests for actual source
files before choosing the source layout. When sources are absent it derives the
installed package root using the correct one-parent layout and explicitly loads
that package namespace with `lib.loc=dirname(package_root)`. Required internal
bindings are made available to the subprocess.

The installed branch copies the real publisher closure into a private overlay
whose parent is that namespace. Validator, generation-path, `tempfile`, and
`file.rename` replacements are assigned/restored in the publisher's environment.
This closes both parts of the original defect: unavailable internal names and
injections that otherwise would not affect namespace-bound nested calls. The
source branch retains the original source execution pattern. A new assertion
checks that the expected bootstrap branch actually ran. No production function,
schema, dependency, or shared-library modification is part of the repair.

Evidence inspected:

- `/tmp/triomics-m7-016-luna/repair2-installed-bootstrap-red.log` retains the
  previous zero-source/no-namespace/undefined-`triomics_read_config` failure.
- `/tmp/triomics-m7-016-luna/repair2-run-probe.R` extracts the actual new
  `writeLines()` recovery-script expression from the native D test. It does not
  substitute a separately authored approximation of that script.
- `/tmp/triomics-m7-016-luna/repair2-source-probe.log` reports exit 0 and
  `probe_bootstrap=source`.
- `/tmp/triomics-m7-016-luna/repair2-installed-probe.log` reports exit 0 and
  `probe_bootstrap=installed`, loading the isolated candidate installation.
- Both logs show the seven expected bootstrap/recovery/path/failure outputs:
  correct bootstrap, `D_PUBLICATION_STATUS_UNKNOWN` for ambiguous readback,
  `D_OUTPUT_PATH_UNSAFE` for planned generation leaf and both temporary ancestry
  hazards, `D_PUBLICATION_FAILED` for failed pointer rename, and
  `old_current_preserved=TRUE`. Both also identify a complete current generation
  after the ambiguous outcome.

These are targeted extracted-block proofs for the bootstrap repair. They are
not a claim that the full D suite or clean archive package check was rerun on
this final candidate.

## Remaining disposition

Original findings 1-3 retain their repair-1 ADDRESSED dispositions. Original
finding 4 remains **NOT ADDRESSED**: native paired and trajectory compositions
fail in Stage C before D, as recorded in
`/tmp/triomics-m7-016/parent-c-boundary.log`. Two-group DEqMS metadata and group
contrast tests do not establish native paired-sign/repeated-time-origin proof.
The test-only bootstrap repair neither changes those boundaries nor waives the
accepted requirement.

The bounded repairs are exhausted. Preserve this candidate and evidence without
integration or acceptance. D-T01/M7/G7 remain blocked; C-06 remains unaccepted,
C-07 controls `not_run`, C-09 unqualified, and downstream work closed. This
review is a narrow code-review receipt, not a milestone acceptance audit.
