status_code <- function(result) {
  status <- attr(result, "status")
  if (is.null(status)) 0L else status
}

triomics_cli <- function(args, timeout = 5) {
  cli <- testthat::test_path("..", "..", "inst", "exec", "triomics")
  rscript <- file.path(R.home("bin"), "Rscript")
  args <- as.character(args)
  path_options <- which(args %in% c("--config", "--output")) + 1L
  path_options <- path_options[path_options <= length(args)]
  args[path_options] <- shQuote(args[path_options])
  suppressWarnings(system2(rscript, c("--vanilla", shQuote(cli), args), stdout = TRUE,
                            stderr = TRUE, timeout = timeout))
}

new_check_fixture <- function(warn_on_load = FALSE) {
  root <- tempfile("triomics-check-fixture-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c(
    "Package: fixturecheck",
    "Title: Minimal Package Check Fixture",
    "Version: 0.1.0",
    "Authors@R: person(\"Fixture\", \"Author\", email = \"fixture@example.test\", role = c(\"aut\", \"cre\"))",
    "Description: A tiny synthetic package used to exercise the native check wrapper.",
    "License: GPL-3",
    "Encoding: UTF-8"
  ), file.path(root, "DESCRIPTION"))
  writeLines(character(), file.path(root, "NAMESPACE"))
  writeLines("fixture_value <- function() 1L", file.path(root, "R", "fixture.R"))
  if (warn_on_load) {
    writeLines(
      '.onLoad <- function(libname, pkgname) warning("intentional check warning fixture")',
      file.path(root, "R", "zzz.R")
    )
  }
  root
}

run_package_check <- function(root, output) {
  script <- testthat::test_path("..", "..", "tools", "check-package.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  suppressWarnings(system2(rscript, c("--vanilla", shQuote(script), "--root", shQuote(root),
                            "--output", shQuote(output)),
                            stdout = TRUE, stderr = TRUE, timeout = 120))
}

testthat::test_that("strict configuration rejects duplicate keys before mutation", {
  config <- tempfile(fileext = ".yaml")
  output <- tempfile()
  dir.create(output)
  writeLines(c(
    "schema_version: '2.0'",
    "schema_version: '2.0'"
  ), config)

  err <- testthat::capture_error(triomics_read_config(config))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, "A_CONFIG_DUPLICATE_KEY")
  testthat::expect_match(conditionMessage(err), "schema_version", fixed = TRUE)
  testthat::expect_identical(list.files(output, all.files = TRUE, no.. = TRUE), character())

  malformed <- tempfile(fileext = ".yaml")
  writeLines("schema_version: [unterminated", malformed)
  err_malformed <- testthat::capture_error(triomics_read_config(malformed))
  testthat::expect_identical(err_malformed$code, "A_CONFIG_PARSE")
  testthat::expect_identical(err_malformed$config_key, "config")
  testthat::expect_match(conditionMessage(err_malformed), "malformed", ignore.case = TRUE)
})

testthat::test_that("strict configuration rejects unknown keys and wrong types", {
  unknown <- tempfile(fileext = ".yaml")
  writeLines(c("schema_version: '2.0'", "unknown_key: true"), unknown)
  err_unknown <- testthat::capture_error(triomics_read_config(unknown))
  testthat::expect_identical(err_unknown$code, "A_CONFIG_UNKNOWN_KEY")
  testthat::expect_match(conditionMessage(err_unknown), "unknown_key", fixed = TRUE)

  wrong_type <- tempfile(fileext = ".yaml")
  writeLines(c("schema_version: 2", "study_id: test"), wrong_type)
  err_type <- testthat::capture_error(triomics_read_config(wrong_type))
  testthat::expect_identical(err_type$code, "A_CONFIG_TYPE")
  testthat::expect_match(conditionMessage(err_type), "schema_version", fixed = TRUE)
})

testthat::test_that("CLI help and unsupported stages return documented exit codes", {
  help <- triomics_cli("--help")
  testthat::expect_equal(status_code(help), 0L)
  testthat::expect_true(any(grepl("Usage:", help, fixed = TRUE)))

  bad <- triomics_cli(c("--stage", "Z"))
  testthat::expect_equal(status_code(bad), 2L)
  testthat::expect_true(any(grepl("stage", bad, ignore.case = TRUE)))
})

testthat::test_that("invalid CLI configuration creates no output and names its key", {
  config <- tempfile(fileext = ".yaml")
  output <- tempfile("triomics-invalid-output-")
  writeLines(c("schema_version: 2", "unknown_key: true"), config)
  result <- triomics_cli(c("--config", config, "--stage", "A", "--output", output,
                           "--validate-only"))
  testthat::expect_equal(status_code(result), 2L)
  testthat::expect_true(any(grepl("unknown_key", result, fixed = TRUE)),
                        info = paste(result, collapse = "\n"))
  testthat::expect_false(file.exists(output))
})

testthat::test_that("validation-only checks stage A without analysis checkpoints", {
  fixture <- testthat::test_path("..", "..", "inst", "examples", "tickets", "A-T01.yaml")
  output <- tempfile("triomics-a-t01-")
  rscript <- file.path(R.home("bin"), "Rscript")
  cli <- testthat::test_path("..", "..", "inst", "exec", "triomics")
  result <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(cli), "--config", shQuote(fixture),
                                                  "--stage", "A", "--output", shQuote(output),
                                                  "--validate-only"), stdout = TRUE, stderr = TRUE,
                               timeout = 5))
  testthat::expect_equal(status_code(result), 0L, info = paste(result, collapse = "\n"))
  testthat::expect_true(file.exists(file.path(output, "validation-summary.json")))
  testthat::expect_false(any(grepl("checkpoint", list.files(output, recursive = TRUE),
                                  ignore.case = TRUE)))
})

testthat::test_that("native package checking derives names and preserves logs", {
  root <- new_check_fixture()
  output <- tempfile("triomics-check-output-")
  on.exit(unlink(c(root, output), recursive = TRUE), add = TRUE)
  script <- testthat::test_path("..", "..", "tools", "check-package.R")

  if (!file.exists(script)) {
    testthat::expect_true(file.exists(script))
  } else {
    result <- run_package_check(root, output)
    build_log <- if (file.exists(file.path(output, "build.log"))) {
      readLines(file.path(output, "build.log"), warn = FALSE)
    } else character()
    testthat::expect_equal(status_code(result), 0L,
                           info = paste(c(result, build_log), collapse = "\n"))
    testthat::expect_true(file.exists(file.path(output, "fixturecheck_0.1.0.tar.gz")))
    testthat::expect_true(file.exists(file.path(output, "build.log")))
    testthat::expect_true(file.exists(file.path(output, "check.log")))
  }
})

testthat::test_that("native package checking fails on an undisposed package warning", {
  root <- new_check_fixture(warn_on_load = TRUE)
  output <- tempfile("triomics-warning-output-")
  on.exit(unlink(c(root, output), recursive = TRUE), add = TRUE)
  script <- testthat::test_path("..", "..", "tools", "check-package.R")

  if (!file.exists(script)) {
    testthat::expect_true(file.exists(script))
  } else {
    result <- run_package_check(root, output)
    testthat::expect_false(identical(status_code(result), 0L))
    testthat::expect_true(file.exists(file.path(output, "build.log")))
    testthat::expect_true(file.exists(file.path(output, "check.log")))
    if (file.exists(file.path(output, "check.log"))) {
      check_log <- paste(readLines(file.path(output, "check.log"), warn = FALSE), collapse = "\n")
      testthat::expect_match(check_log, "intentional check warning fixture|WARNING")
    }
  }
})

testthat::test_that("environment capture records actual R, Bioconductor, and installed package state", {
  project <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  output <- tempfile("triomics-environment-")
  dir.create(output)
  script <- testthat::test_path("..", "..", "tools", "capture-environment.R")
  rscript <- file.path(R.home("bin"), "Rscript")

  if (!file.exists(script)) {
    testthat::expect_true(file.exists(script))
  } else {
    result <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(script), "--project", shQuote(project),
                                                    "--output", shQuote(output)), stdout = TRUE, stderr = TRUE,
                                         timeout = 30))
    testthat::expect_equal(status_code(result), 0L, info = paste(result, collapse = "\n"))
    if (identical(status_code(result), 0L)) {
      lock <- jsonlite::read_json(file.path(project, "renv.lock"), simplifyVector = TRUE)
      testthat::expect_identical(lock$R$Version, as.character(getRversion()))
      testthat::expect_identical(lock$Bioconductor$Version, "3.23")
      installed <- utils::installed.packages(lib.loc = .libPaths()[1L])
      manifest <- jsonlite::read_json(file.path(output, "installation-manifest.json"),
                                      simplifyVector = TRUE)
      testthat::expect_identical(manifest$packages$BiocManager$Version,
                                 unname(installed["BiocManager", "Version"]))
      session <- readLines(file.path(output, "session-manifest.txt"), warn = FALSE)
      testthat::expect_true(any(grepl(R.version.string, session, fixed = TRUE)))
    }
  }
})

testthat::test_that("artifact writer finalizes a verified envelope atomically", {
  output <- tempfile("triomics-envelope-")
  dir.create(output)
  payload <- file.path(output, "payload.json")
  manifest <- file.path(output, "manifest.json")
  writeLines('{"ok":true}', payload, useBytes = TRUE)
  envelope <- triomics_artifact_envelope(
    artifact_type = "validation_summary",
    artifact_id = "artifact-1",
    study_id = "synthetic",
    run_id = "run-1",
    status = "complete",
    config_sha256 = paste(rep("a", 64), collapse = ""),
    payload = payload,
    producer_commit = "test"
  )
  triomics_write_envelope(envelope, manifest)
  testthat::expect_true(file.exists(manifest))
  testthat::expect_true(triomics_validate_envelope(manifest))
  testthat::expect_false(file.exists(paste0(manifest, ".tmp")))
})
