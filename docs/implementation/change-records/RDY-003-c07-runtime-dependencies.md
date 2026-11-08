# RDY-003 — Explicit dependencies for the C-07 runner

Status: proposed bounded coordinator-owned seam; no implementation or fit
authorized by this record before its own fresh Astra review passes.

## Trigger and preserved history

RDY-002's initial Luna attempt stopped before writing its runner. Its only
uncommitted change is the new test file, expanded to eight absent-runner errors.
The first four RED errors were independently reproduced by Sol. There is no
runner commit, native fit or successful C-07 result. The implementation remains
incomplete, with zero targeted repairs used. Its protocol initial audit and one
focused packaging rereview remain recorded and exhausted; this record does not
grant another review or reset an attempt on that protocol.

The runner needs explicit child-process supervision and identity checks.
`DESCRIPTION` declares neither `processx` nor `ps`. Both are already present in
the accepted `renv.lock` and observed runtime: processx 3.9.0, ps 1.9.3. Existing
installation is not a declaration of the package's direct dependencies.

## Exact proposed change

After this seam passes audit, Sol adds only these entries to `DESCRIPTION`'s
`Suggests`, preserving every existing entry:

```text
    processx (== 3.9.0),
    ps (== 1.9.3),
```

No install, lockfile edit, namespace export, production R/CLI edit, statistical
method or numerical protocol change is permitted. The already audited RDY-002
worker scope stays exactly `inst/qualification/run-c07-qualification.R` and
`tests/testthat/test-RDY-002-c07-qualification.R`. After Sol resolves this shared
seam, the same Luna initial attempt may continue; it cannot edit DESCRIPTION.
The frozen protocol is supplied explicitly and retains its current content hash.
Its environment record already requires all dependency versions/hashes; the
runner records and verifies these two additional runtime requirements there.

## Required process behavior

- Use explicit run-owned process handles, supervision and cleanup. Do not rely
  on garbage collection, an agent exit or an elapsed timeout as proof of cleanup.
- Reserve fits durably before launch. Before any native work, a child waits for
  a release handshake issued only after its run/cell identity, PID and creation
  time are durably recorded. A missing handshake cannot start a fit.
- Bind execution to a single writer and the unchanged output root, protocol,
  code/runtime identity and original start/deadline. Interruption does not reset
  the time cap or refund a reservation. Do not rerun an interrupted cell.
- After a timeout, verify run-owned processes have stopped before another cell
  can start. On resume, compare PID plus creation time and run identity; a PID
  alone is insufficient and must not authorize signalling. If orphan identity,
  liveness or cleanup cannot be established, refuse further execution and leave
  remaining rows unrun with a reason. Never kill an unrelated process.
- Test child and descendant cleanup, abrupt coordinator death, no-release
  behavior, a changed/reused identity, an unrelated live process and failure of
  inspection. These are no-fit supervisor tests; no model fitting is allowed to
  test process management. Record the actual qualified host and limitations.
- Retain the smaller of the per-cell and remaining global timeout. Preserve
  actual exit, warning, timeout and cleanup evidence, including uncertainty.
  Library availability alone is insufficient to qualify supervision.

The [processx documentation](https://processx.r-lib.org/reference/process.html)
describes supervision and process-tree cleanup, including limitations on macOS
environment inspection. Consequently the runner must test the host and fail
closed when ownership or cleanup is uncertain; it must not claim an unrestricted
cross-platform guarantee. [ps handles](https://ps.r-lib.org/reference/ps_handle.html)
include PID and creation time. These facilities support the required checks but
do not themselves prove the runner has implemented them correctly.

Base R's [system2 documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/system2.html)
states that its timeout uses whole seconds and termination is not guaranteed for
all commands. Sol will not replace the reviewed process requirements with a
weaker undocumented assumption to avoid declaring a dependency.

## Review and acceptance boundary

This is a new, narrowly bounded shared-dependency decision under the authorized
readiness programme. One fresh Astra-low initial scope audit and at most one
focused correction/rereview apply to RDY-003 only. It neither re-audits the
scientific protocol nor accepts RDY-002. Sol applies the exact declaration after
audit_pass yes, inspects the resulting diff and read-backs declared and installed
versions. The eventual runner must pass independent no-fit tests and the clean
archive package check with no required skips before acceptance. A failed process
probe blocks native execution; it does not expand implementation scope.

All 41 packet identities, accepted M1–M9/G7–G9 states, conditional limitations,
B-T04 bound 0.08 and seeds 1001:1400 remain unchanged. No private data, external
publication, GitHub push, new method, endpoint or interpretation is authorized.
