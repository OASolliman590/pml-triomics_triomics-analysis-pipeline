testthat::test_that("the report renderer rejects a bundle without an immutable source manifest", {
  renderer <- get0("render_report", envir = asNamespace("triomics"), inherits = FALSE)
  testthat::expect_true(is.function(renderer))
  if (!is.function(renderer)) return(invisible(NULL))

  error <- tryCatch(renderer(list(), list()), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_BUNDLE_INVALID")
})
