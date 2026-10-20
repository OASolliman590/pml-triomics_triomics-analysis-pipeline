# C-08 integrated package check

Date: 2026-10-20  
Snapshot: `329f3a6`  
Clean archive: `/private/tmp/triomics-c08-pkg3.9vWQmp`  
Result: `Status: 1 ERROR`

The check used both verified R library roots and the isolated accepted MOFA
runtime (`MOFA2 1.22.1`, `reticulate`, and the local mofapy2 Python). Dependency,
build, install, load, static-code, documentation and example checks passed.

The installed test suite stopped with FAIL 3, WARN 24, SKIP 0, PASS 4346. All
three errors are in new `test-C-08-shared.R` tests: they resolve
`inst/examples/tickets/C-08.yaml` only through a source-checkout path, which is
absent from the installed-package test context. Two of the 24 warnings are the
associated failed `file()` opens. The remaining 22 match the previously named
inventory: eight B-T07 missing-Git-metadata probes and fourteen native MOFA
small-fixture/QC diagnostics.

This is a package test-fixture lookup defect. It is not evidence of a C-08
scientific, endpoint-integrity or model-execution failure, but it cannot be
waived. One final narrow shared-repair continuation may change only the C-08
shared test/helper and registration evidence so the same public fixture resolves
in both source and installed contexts. No product code, frozen spec, threshold,
packet repair reset or new review round is authorized. Sol will verify directly
and rerun the clean package gate.

Evidence hashes:

- source archive: `897e65f7d66c772ba456a3d3c6ac0add549747fe9366907d9a60bf084620d047`
- `00check.log`: `d8b80ccfca9e769f341f71ab5faabc80bd2207aac514abe3421dd0a8e48929dd`
- `testthat.Rout.fail`: `f8d0527b5db944c0164c99066f93125677fb65e4e65dafb3d6d4d3dae01b4405`
- `check.log`: `0d3dd5f04f222d2cf9d125d396016a004ec01ce6825c82d413079b573a908a1e`
