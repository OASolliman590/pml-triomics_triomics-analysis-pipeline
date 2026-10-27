.libPaths(c("/tmp/triomics-mofa-lib-m7-006", .libPaths()))
stopifnot(as.character(utils::packageVersion("MOFA2")) == "1.22.1")
testthat::set_max_fails(Inf)
results <- testthat::test_local(
  filter = "^(C-06-shared|C-08-shared)$",
  reporter = "summary",
  stop_on_failure = FALSE
)
tab <- as.data.frame(results)
stopifnot(nrow(tab) > 0L)
totals <- list(
  fail = sum(tab$failed), warn = sum(tab$warning), skip = sum(tab$skipped),
  error = sum(tab$error), pass = sum(tab$passed), test_blocks = nrow(tab)
)
cat("\nEXACT_TOTALS:", jsonlite::toJSON(totals, auto_unbox = TRUE), "\n")
conditions <- list()
for (test in results) {
  for (expectation in test$results) {
    if (inherits(expectation, "expectation_warning") ||
        inherits(expectation, "expectation_failure") ||
        inherits(expectation, "expectation_error") ||
        inherits(expectation, "expectation_skip")) {
      item <- list(
        file = test$file, test = test$test,
        type = class(expectation)[[1L]], message = conditionMessage(expectation)
      )
      conditions[[length(conditions) + 1L]] <- item
      cat("NAMED_CONDITION:", jsonlite::toJSON(item, auto_unbox = TRUE), "\n")
    }
  }
}
jsonlite::write_json(
  list(totals = totals, conditions = conditions),
  "/tmp/triomics-m7-017-luna/red.json", auto_unbox = TRUE, pretty = TRUE
)
saveRDS(results, "/tmp/triomics-m7-017-luna/red.rds")
quit(status = if (totals$fail + totals$error + totals$skip > 0L) 1L else 0L)
