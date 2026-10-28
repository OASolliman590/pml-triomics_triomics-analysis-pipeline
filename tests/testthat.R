library(testthat)

triomics_warning_reporter <- testthat::CheckReporter$new()
triomics_write_warning_inventory <- function(reporter) {
  records <- reporter$warnings$as_list()
  warnings <- lapply(records, function(record) {
    test_name <- record$test
    if (!is.character(test_name) || length(test_name) != 1L || is.na(test_name)) {
      test_name <- "<unknown test>"
    }
    list(test = test_name, message = as.character(record$message))
  })
  jsonlite::write_json(
    list(warnings = warnings),
    "triomics-warning-inventory.json",
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
}

tryCatch(
  test_check("triomics", reporter = triomics_warning_reporter),
  finally = triomics_write_warning_inventory(triomics_warning_reporter)
)
