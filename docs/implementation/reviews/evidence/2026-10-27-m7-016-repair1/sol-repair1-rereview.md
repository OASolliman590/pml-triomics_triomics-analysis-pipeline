# M7-016 repair 1 focused rereview

Candidate: `f23b5a7c96cec1134b6d098850e90fbbfc420299`.
Repair base: `ac98db3191bf8a5854144f5e017329038e9170e4`.
Scope: the four original findings and new Important/Critical defects in the
repair diff only. No product edits, commits, agents, whole-suite rerun, private
data access, integration, or acceptance. Repository status remained clean.

**Spec verdict: changes required — Critical 0, Important 2 remaining.**
**Quality verdict: changes required — Critical 0, Important 1 new.**
The two remaining unique matters are original finding 4 (acceptance evidence)
and new finding 5 (installed-package recovery-test bootstrap).

## Original finding dispositions

| Original finding | Disposition | Evidence |
|---|---|---|
| 1. Native C sidecar ancestry escapes same-root boundary | **ADDRESSED** | `R/evidence_bundle.R:623-658` validates original directory ancestry before enumeration and resolves both original leaf paths through `triomics_d_child_path`; line 52 now rejects dangling links without conditioning on `file.exists`. Parent's retained ancestry probe rejects both native consumption and the D executor with `D_OUTPUT_PATH_UNSAFE`. Native tests include parent, directory, payload/envelope, and dangling links. |
| 2. Exported API accepts missing producer proof | **ADDRESSED** | `R/annotation_router.R:1154-1183` requires the readback context/request, re-reads the persisted native B/C evidence, and rejects changed in-memory evidence; line 1202 calls the guard before routing. Parent's profileless API counterexample now returns `D_PROVENANCE_REQUIRED`. Added tests exercise missing context/profile references and in-memory modification. |
| 3. Generation validator accepts semantic manifest conflicts | **ADDRESSED** | `R/annotation_router.R:407-469` provides one typed provenance projection used by publisher and reader. Lines 610-643 compare manifest projection and envelope inputs against payload immutable references and enforce exact payload/envelope types, roles, and pairing. Parent's re-sealed conflicting-manifest counterexample now returns `D_PROVENANCE_INVALID`. Added tests independently cover re-sealed descriptor/provenance and envelope-input conflicts. |
| 4. Required native D-boundary acceptance evidence incomplete | **NOT ADDRESSED** | Repair materially improves coverage, but native paired and trajectory handoffs still fail in C before D. Fresh parent evidence is retained in `/tmp/triomics-m7-016/parent-c-boundary.log`. This requirement is not waived. |

The counterexample readbacks above are in
`/tmp/triomics-m7-016/parent-repair1-ancestry.log` and
`/tmp/triomics-m7-016/parent-repair1-readback.log`; I inspected those logs and
the fix code rather than rerunning the same native consumers again.

## Remaining original Important finding 4

Repair adds actual qualified native DEqMS B/C/D execution, current count-table
drift, persisted prior mutation, real measurement-byte mutation, nonempty
subject/visit/time metadata drift, represented/prepared-state changes,
publication path hazards, failed rename with old-current proof, and unknown
postcommit readback. The source test's two-group contrast reversal and added
time columns do **not** establish native paired-sign or repeated-time-origin
coverage. The worker's appended report now states that distinction explicitly.

The retained fresh compositions show:

- Paired: B exit 0; C exit 2, `C_ELIGIBILITY_UNKNOWN_UNIT`, `block_1`, matrix
  sample IDs outside the declared unit map.
- Trajectory: B exit 0; C exit 2, `C_SCORE_LINEAGE_INVALID`, C-08 missing the
  declared AnalysisUnitMap from resolution.

These are C boundaries outside this D-only worker's allowed caller scope.
Passing the two-group case, schema/hash unit tests, or the remaining source
suite cannot substitute for those required native handoffs. Keep original
finding 4 open; do not integrate this candidate or claim D-T01/M7/G7 acceptance
with the gap. Resolving it requires a coordinator scope disposition; this
rereview does not authorize changing B/C product code or inventing a proof.

## New finding 5 — Important: recovery probe cannot bootstrap an installed package

Location: `tests/testthat/test-D-T01.R:1348-1354` (new repair code).

The new subprocess computes `repository_root` as two parents above the executable
directory and sources `repository_root/R/*.R`. That matches source layout
`<repo>/inst/exec/triomics`. The shared resource helper also supports installed
layout `<library>/triomics/exec/triomics`. In that case two parents resolve to
`<library>`, not the package directory. Furthermore, an installed R package
provides lazy-loaded code rather than the original `R/*.R` source tree. A fresh
`Rscript --vanilla` therefore sources zero files, loads no `triomics` namespace,
and immediately fails on the unqualified internal `triomics_read_config()` call.

This is material to the explicitly required clean archive package check. Its
installed-package tests use `helper-installed-paths.R` when source `inst/`
resources are absent. The recovery subprocess is unconditional in the native
D test, so this would turn the newly added recovery assertions into failures
in that context even though the source test passed.

Bounded reproduction, without a suite run:

```
Rscript --vanilla /tmp/triomics-m7-016/sol-repair1-installed-bootstrap.R
repository_root=/Users/megastore/Library/R/x86_64/4.6/library
R_source_count=0
triomics_namespace_loaded=FALSE
next_call=could not find function "triomics_read_config"
```

Captured evidence: `/tmp/triomics-m7-016/sol-repair1-installed-bootstrap.log`.
The script executes the same root calculation/source bootstrap through an
installed executable and catches the next call's error. It does not run
publication or depend on the installed package matching the repair; the defect
is the absence of any namespace/code bootstrap before that call.

Minimal required fix, within the existing test-file allowlist: retain source
loading only when the actual source tree is available; add an explicit
installed-package bootstrap for this subprocess. The probe deliberately
replaces internal validator/path/tempfile/rename bindings, so the installed
branch must execute package functions in a private test environment where those
injections resolve (for example copy the package bindings there and rebind the
package closures to that environment). Merely attaching exports with
`library(triomics)` leaves internal names unavailable, and merely fetching a
namespace closure does not make its nested calls use global test replacements.
Verify both bootstrap branches with the existing small recovery scenarios.
Do not change product/B/C code, suppress assertions, or skip installed tests.

## Verification accounting

Inspected repair diff (four allowlisted files only), parent counterexample logs,
parent C-boundary evidence, appended worker report, and the focused test log.
The log `/tmp/triomics-m7-016-luna/repair1-focused-final.log` contains `DONE`, 317
success markers, and no failure/warning/skip blocks; there is no structured
test-results object, so this rereview does not invent exact testthat totals.
No full suite or native B/C/D rerun was performed by this reviewer.

No other new Important/Critical finding was identified in the bounded fix diff.
The three original product counterexamples are closed for this repair snapshot;
the native paired/trajectory acceptance evidence and the new test bootstrap
defect remain open. C-06 stays unaccepted, C-07 controls `not_run`, C-09
unqualified, and M7/G7 closed.
