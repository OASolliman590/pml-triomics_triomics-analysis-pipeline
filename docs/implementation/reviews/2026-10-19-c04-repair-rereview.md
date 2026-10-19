# C-04 repair rereview

Astra low, read-only, reviewed `6ea8d20` on 2026-10-19. Thread artifact
`/tmp/triomics-astra-c04-rereview-run`. Verdict: pass. No new Critical or
Important finding. This review does not accept C-04.

The three Important findings from the `a3b9713` review are closed:

- Variance qualification failure keeps the native `BackendFit`, response
  diagnostics, and an unavailable variance status. Extraction still aborts
  `C_VARIANCE_UNAVAILABLE` at `R/integration_block_spls.R:650`.
- DIABLO continuous evaluation expects `C_EVAL_CAPABILITY_UNAVAILABLE` at
  `tests/testthat/test-C-04.R:504`.
- The synthetic score lives in `inst/examples/data/c04/metadata.tsv`. The
  shared independent metadata was not given a response column.

Sol checked those lines after the review. Astra did not rerun tests. The
fixture CLI metrics are not a biological result. The failed package check of
`d5037ec` still blocks acceptance, and that check does not cover `6ea8d20`.
