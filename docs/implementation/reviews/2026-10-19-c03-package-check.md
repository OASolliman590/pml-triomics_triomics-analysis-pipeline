# Package check of d5037ec

The clean archive check of `d5037ec` finished with `Status: 1 ERROR`.
The testthat summary was `FAIL 7`, `WARN 8`, `SKIP 0`, `PASS 3803`.
This check does not include `6ea8d20`. C-03 and C-04 stay unaccepted.

Sol reproduced the expectation failures on `6ea8d20` with `pkgload::load_all`:

- `triomics_integration_backend_names()` is `concat_pca`, `concat_plsda`,
  `concat_pls`, `mixomics_diablo`, `mixomics_block_spls`.
  `tests/testthat/test-C-01.R` still expects only the three concatenation names.
- `triomics_integration_backend("mixomics_diablo")` returns a categorical
  `BackendAdapter`. The C-01 test still expects `C_SCORE_BACKEND_UNAVAILABLE`.
- `integrate_blocks` with that method on a C-01 spec aborts
  `C_SCORE_PARAMETER` because `score_definition` is absent. The test still
  expects `C_SCORE_BACKEND_UNAVAILABLE`.
- `triomics_evaluation_backend("mixomics_diablo", "categorical")` returns a
  `BackendAdapter`. `tests/testthat/test-C-02.R` line 439 still expects
  `C_SCORE_BACKEND_UNAVAILABLE`.
- `tests/testthat/test-C-03.R` line 302 calls `normalizePath` on
  `discovery/spec-v2/schemas/config.schema.json`. The built package omits
  `discovery/` and `docs/` through `.Rbuildignore`, so the installed test
  errors. The C-03 and C-04 registration JSON paths have the same shape.
- An unregistered name, `mixomics_unregistered`, still aborts
  `C_SCORE_BACKEND_UNAVAILABLE`.

`inst/schema/config.schema.json` is not the frozen schema. Their sha256
prefixes are `179cc30d5650c71a` and `25277ed4a9d8d2a5`. The repair must copy
the frozen file into the package and keep validating the fixture against
those bytes.

The eight testthat warnings did not by themselves fail `test_check` in
testthat 3.3.2. They remain undisposed until the next full check.
