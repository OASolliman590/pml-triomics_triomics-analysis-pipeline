testthat::test_that("the report renderer rejects a bundle without an immutable source manifest", {
  renderer <- get0("render_report", envir = asNamespace("triomics"), inherits = FALSE)
  testthat::expect_true(is.function(renderer))
  if (!is.function(renderer)) return(invisible(NULL))

  error <- tryCatch(renderer(list(), list()), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_BUNDLE_INVALID")
})

testthat::test_that("report bundle construction refuses a missing producer root", {
  build_bundle <- get0("triomics_report_bundle_from_root", envir = asNamespace("triomics"),
                       inherits = FALSE)
  testthat::expect_true(is.function(build_bundle))
  if (!is.function(build_bundle)) return(invisible(NULL))

  missing_root <- file.path(tempdir(), "d-t08-missing-producer-root")
  error <- tryCatch(build_bundle(missing_root, list()), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_REQUIRED_MISSING")
})

testthat::test_that("the report resolves the source-checkout Quarto template", {
  template_path <- get0("triomics_report_template_path", envir = asNamespace("triomics"),
                        inherits = FALSE)
  testthat::expect_true(is.function(template_path))
  if (!is.function(template_path)) return(invisible(NULL))

  resolved <- normalizePath(template_path(), winslash = "/", mustWork = TRUE)
  expected <- normalizePath(triomics_test_resource("report", "triomics.qmd"),
                            winslash = "/", mustWork = TRUE)

  testthat::expect_identical(resolved, expected)
})
