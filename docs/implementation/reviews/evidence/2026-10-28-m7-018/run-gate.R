args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[[1L]] %in% c("focused", "full", "diagnostic"))
.libPaths(c("/tmp/triomics-mofa-lib-m7-006", .libPaths()))
stopifnot(as.character(utils::packageVersion("MOFA2")) == "1.22.1")
cat("GATE:", args[[1L]], "\n")
cat("COMMIT:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
cat("MOFA2:", as.character(utils::packageVersion("MOFA2")), "\n")
cat("PYTHON:", Sys.getenv("TRIOMICS_MOFA_PYTHON"), "\n")
selection <- if (identical(args[[1L]], "focused")) "^(B-T07|C-06|C-06-shared|C-08-shared|D-T01)$" else if (identical(args[[1L]], "diagnostic")) "^A-T01$" else NULL
testthat::set_max_fails(Inf)
results <- testthat::test_local(filter = selection, reporter = "summary", stop_on_failure = FALSE)
tab <- as.data.frame(results)
stopifnot(nrow(tab) > 0L)
totals <- list(fail = sum(tab$failed), warn = sum(tab$warning), skip = sum(tab$skipped),
               error = sum(tab$error), pass = sum(tab$passed), test_blocks = nrow(tab))
cat("\nEXACT_TOTALS:", jsonlite::toJSON(totals, auto_unbox = TRUE), "\n")
inventory <- list()
for (i in seq_along(results)) {
  test <- results[[i]]
  for (expectation in test$results) {
    if (inherits(expectation, "expectation_warning") ||
        inherits(expectation, "expectation_failure") ||
        inherits(expectation, "expectation_error") ||
        inherits(expectation, "expectation_skip")) {
      item <- list(file = test$file, test = test$test,
                   type = class(expectation)[[1L]], message = conditionMessage(expectation))
      inventory[[length(inventory) + 1L]] <- item
      cat("NAMED_CONDITION:", jsonlite::toJSON(item, auto_unbox = TRUE), "\n")
    }
  }
}
jsonlite::write_json(list(totals = totals, conditions = inventory), args[[2L]],
                     auto_unbox = TRUE, pretty = TRUE)
saveRDS(results, paste0(args[[2L]], ".rds"))
quit(status = if (totals$fail + totals$error + totals$skip > 0L) 1L else 0L)
