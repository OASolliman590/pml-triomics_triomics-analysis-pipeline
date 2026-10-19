c05_error <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

# Injected probe: the missing-package path is tested without depending on what
# this machine has installed.
c05_probe <- function(loaded) {
  function(package) {
    is_loaded <- isTRUE(loaded[[package]])
    list(
      package = package, loaded = is_loaded,
      version = if (is_loaded) "0.0.0-test" else NULL,
      reason = if (is_loaded) NULL else "r_package_not_loadable"
    )
  }
}

testthat::test_that("C-05 missing-package path names each absent runtime and is available only when both load", {
  both_missing <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = FALSE, mofapy2 = FALSE)))
  testthat::expect_false(both_missing$available)
  testthat::expect_identical(both_missing$missing, c("MOFA2", "mofapy2"))

  mofapy_missing <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = FALSE)))
  testthat::expect_false(mofapy_missing$available)
  testthat::expect_identical(mofapy_missing$missing, "mofapy2")

  mofa_missing <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = FALSE, mofapy2 = TRUE)))
  testthat::expect_false(mofa_missing$available)
  testthat::expect_identical(mofa_missing$missing, "MOFA2")

  both_loaded <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = TRUE)))
  testthat::expect_true(both_loaded$available)
  testthat::expect_identical(both_loaded$missing, character())
})

testthat::test_that("C-05 missing-package path returns an unavailable state with no factors, weights, variance or reload agreement", {
  status <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = FALSE, mofapy2 = FALSE)))
  state <- triomics_mofa_fit(list(), list(method = "mofa2"), status = status)

  testthat::expect_identical(state$status, "unavailable")
  testthat::expect_false(state$trained)
  testthat::expect_identical(state$missing_packages, c("MOFA2", "mofapy2"))
  testthat::expect_false(any(c("factors", "weights", "variance", "scores", "loadings") %in% names(state)))
  testthat::expect_identical(state$reload$status, "not_evaluated")
  testthat::expect_false("agreement" %in% names(state$reload))
})

testthat::test_that("C-05 missing-package path refuses when only one runtime is absent", {
  status <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = FALSE)))
  state <- triomics_mofa_fit(list(), list(method = "mofa2"), status = status)

  testthat::expect_identical(state$status, "unavailable")
  testthat::expect_false(state$trained)
  testthat::expect_identical(state$missing_packages, "mofapy2")
  testthat::expect_false("factors" %in% names(state))
})

testthat::test_that("C-05 does not report training when both runtimes load but the training path is absent", {
  status <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = TRUE)))
  err <- c05_error(
    triomics_mofa_fit(list(), list(method = "mofa2"), status = status),
    "C_SCORE_BACKEND_UNAVAILABLE"
  )
  testthat::expect_match(conditionMessage(err), "no factors, weights or variance were produced")
})

testthat::test_that("C-05 python status refuses an unconfigured or unlaunchable mofapy2 runtime", {
  unconfigured <- triomics_mofa_python_status("")
  testthat::expect_false(unconfigured$loaded)
  testthat::expect_identical(unconfigured$reason, "python_not_configured")

  missing_binary <- triomics_mofa_python_status(file.path(tempdir(), "c05-no-such-python"))
  testthat::expect_false(missing_binary$loaded)
  testthat::expect_identical(missing_binary$reason, "mofapy2_import_failed")
})

testthat::test_that("C-05 live runtime status agrees with the R namespace and the default probe", {
  live <- triomics_mofa_backend_status()

  testthat::expect_identical(live$packages$MOFA2$loaded, requireNamespace("MOFA2", quietly = TRUE))
  testthat::expect_identical(
    live$available,
    all(vapply(live$packages, function(status) isTRUE(status$loaded), logical(1L)))
  )
})

testthat::test_that("C-05 environment check reports both runtimes as not loaded from an empty library and exits non-zero", {
  script <- testthat::test_path("..", "..", "tools", "check-mofa-environment.R")
  source_checkout <- file.exists(testthat::test_path(
    "..", "..", "discovery", "spec-v2", "schemas", "config.schema.json"
  ))
  if (!source_checkout) {
    # tools/ is excluded from the built package. The runtime probe remains the installed check.
    live <- triomics_mofa_backend_status()
    if (isTRUE(live$available)) {
      c05_error(
        triomics_mofa_fit(list(), list(method = "mofa2"), status = live),
        "C_SCORE_BACKEND_UNAVAILABLE"
      )
    } else {
      state <- triomics_mofa_fit(list(), list(method = "mofa2"), status = live)
      testthat::expect_identical(state$status, "unavailable")
      testthat::expect_false(state$trained)
    }
    return()
  }
  testthat::expect_true(file.exists(script))
  library <- tempfile("c05-empty-library-")
  dir.create(library)
  output <- suppressWarnings(system2(
    "Rscript", c("--vanilla", script, "--library", library),
    stdout = TRUE, stderr = TRUE
  ))

  testthat::expect_identical(attr(output, "status"), 1L)
  testthat::expect_true(any(grepl("^MOFA2: not loaded", output)))
  testthat::expect_true(any(grepl("^mofapy2: not loaded", output)))
  testthat::expect_true(any(grepl("No model was fitted", output, fixed = TRUE)))
})

testthat::test_that("C-05 ticket validates under the existing config schema and requests mofa2 without an outcome", {
  previous <- options(triomics.source_root = normalizePath(
    testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE
  ))
  on.exit(options(previous), add = TRUE)
  config <- triomics_read_config(triomics_test_resource("examples", "tickets", "C-05.yaml"))
  request <- config$analyses[[1L]]$integration[[1L]]

  testthat::expect_identical(request$method, "mofa2")
  testthat::expect_null(request$outcome)
  testthat::expect_identical(request$mofa_convergence, "medium")
  testthat::expect_identical(request$evaluation$mode, "exploratory")
})

testthat::test_that("C-05 public registry refuses the unregistered mofa2 name", {
  c05_error(triomics_integration_backend("mofa2"), "C_SCORE_BACKEND_UNAVAILABLE")
})

testthat::test_that("C-05 registration request records an unaccepted, unapplied and CLI-disabled route", {
  registration_path <- testthat::test_path(
    "..", "..", "docs", "implementation", "registrations", "C-05.json"
  )
  source_checkout <- file.exists(testthat::test_path(
    "..", "..", "discovery", "spec-v2", "schemas", "config.schema.json"
  ))
  if (!source_checkout) {
    # docs/ is excluded from the built package. The public registry must still refuse the method.
    c05_error(triomics_integration_backend("mofa2"), "C_SCORE_BACKEND_UNAVAILABLE")
    return()
  }
  testthat::expect_true(file.exists(registration_path))
  registration <- jsonlite::read_json(registration_path, simplifyVector = TRUE)

  testthat::expect_identical(registration$packet_id, "C-05")
  testthat::expect_false(registration$accepted)
  testthat::expect_false(registration$applied_to_shared_files)
  testthat::expect_false(registration$cli_registration$enabled)
  testthat::expect_identical(registration$integration_backends$method, "mofa2")
  testthat::expect_identical(registration$dependencies$package, c("MOFA2", "mofapy2"))
  testthat::expect_true(any(grepl(
    "AC1 training, save and reload evidence is absent", registration$known_gaps, fixed = TRUE
  )))
})
