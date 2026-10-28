# M7-018 Luna receipt

- Checkout: clean at `6456055d394057baf029eba7321c45ad90a3ec21` before edit; only `R/cli.R` changed.
- Edit: `R/cli.R:700` now emits a named `time = NULL` only when `absent_all_missing_time` is true; finite retained/represented time and score guards are untouched.
- Boundary evidence: `/tmp/triomics-m7-017/sol-repair2-review.md` identifies the persisted `time:"NA"` defect and verifies named NULL is accepted; parent reports RED reproduction at `/tmp/triomics-m7-018-terra`.
- Probe: `jsonlite::toJSON` then `fromJSON(..., simplifyVector=FALSE)` emitted `{"unit_map":[{"unit_id":"SYN_12","time":null}]}` and retained the `time` key with NULL value.
- Regression: `Rscript --vanilla -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-C-08-shared.R", reporter="summary")'` exited 0 (`DONE`).
- `git diff --check` passed; no tests, helpers, writer, schemas, or D consumer were edited. Full native B/C/D proof and focused parent gates remain pending.
- Effective model/effort settings were not exposed for independent verification.
