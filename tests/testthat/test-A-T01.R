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

package_check_evidence_dir <- function(name) {
  root <- testthat::test_path("..", "..", "artifacts", "acceptance", "A-T01",
                              "repair-package-check")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  tempfile(paste0(name, "-"), tmpdir = root)
}

write_test_envelope <- function(envelope, path) {
  jsonlite::write_json(envelope, path, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
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

testthat::test_that("strict configuration rejects scalar values for array fields by exact key", {
  fixture <- testthat::test_path("..", "..", "inst", "examples", "tickets", "A-T01.yaml")
  source <- paste(readLines(fixture, warn = FALSE), collapse = "\n")
  malformed_arrays <- list(
    list(text = sub("    block_ids:\n      - block_1", "    block_ids: block_1",
                     source, fixed = TRUE), key = "analyses[1].block_ids"),
    list(text = sub("    source_refs:\n      - block_1", "    source_refs: block_1",
                     source, fixed = TRUE), key = "blocks[1].source_refs")
  )

  for (case in malformed_arrays) {
    config <- tempfile(fileext = ".yaml")
    writeLines(case$text, config, useBytes = TRUE)
    err <- testthat::capture_error(triomics_read_config(config))
    testthat::expect_s3_class(err, "triomics_error")
    testthat::expect_identical(err$code, "A_CONFIG_TYPE")
    testthat::expect_identical(err$config_key, case$key)
    testthat::expect_match(conditionMessage(err), case$key, fixed = TRUE)
  }
})

testthat::test_that("stage A validates each declared source, metadata, and block input path", {
  fixture <- testthat::test_path("..", "..", "inst", "examples", "tickets", "A-T01.yaml")
  config <- triomics_read_config(fixture)
  cases <- list(
    list(key = "sources[1].file", mutate = function(value) {
      value$sources[[1L]]$file <- "missing-source.tsv"
      value
    }),
    list(key = "metadata.file", mutate = function(value) {
      value$metadata$file <- "missing-metadata.tsv"
      value
    }),
    list(key = "blocks[1].file", mutate = function(value) {
      value$blocks[[1L]]$file <- "missing-block.tsv"
      value
    })
  )

  for (case in cases) {
    err <- testthat::capture_error(
      triomics_validate_config_paths(case$mutate(config), fixture)
    )
    testthat::expect_s3_class(err, "triomics_error")
    testthat::expect_identical(err$code, "A_SOURCE_MISSING")
    testthat::expect_identical(err$config_key, case$key)
    testthat::expect_match(conditionMessage(err), case$key, fixed = TRUE)
  }
})

testthat::test_that("CLI help and unsupported stages return documented exit codes", {
  help <- triomics_cli("--help")
  testthat::expect_equal(status_code(help), 0L)
  testthat::expect_true(any(grepl("Usage:", help, fixed = TRUE)))

  bad <- triomics_cli(c("--stage", "Z"))
  testthat::expect_equal(status_code(bad), 2L)
  testthat::expect_true(any(grepl("stage", bad, ignore.case = TRUE)))

  fixture <- testthat::test_path("..", "..", "inst", "examples", "tickets", "A-T01.yaml")
  unavailable <- triomics_cli(c("--config", fixture, "--stage", "B", "--output",
                                tempfile("triomics-stage-b-"), "--validate-only"))
  testthat::expect_equal(status_code(unavailable), 3L,
                         info = paste(unavailable, collapse = "\n"))
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

testthat::test_that("CLI finalization failures return execution exit code 4", {
  fixture <- testthat::test_path("..", "..", "inst", "examples", "tickets", "A-T01.yaml")
  output <- tempfile("triomics-finalize-failure-")
  dir.create(output)
  summary <- file.path(output, "validation-summary.json")
  writeLines("pre-existing immutable output", summary)

  result <- triomics_cli(c("--config", fixture, "--stage", "A", "--output", output,
                           "--validate-only"))
  testthat::expect_equal(status_code(result), 4L,
                         info = paste(result, collapse = "\n"))
  testthat::expect_identical(readLines(summary, warn = FALSE), "pre-existing immutable output")
  testthat::expect_false(file.exists(paste0(summary, ".tmp")))
})

testthat::test_that("native package checking derives names and preserves logs", {
  root <- new_check_fixture()
  output <- package_check_evidence_dir("clean")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- testthat::test_path("..", "..", "tools", "check-package.R")

  if (!file.exists(script)) {
    testthat::expect_true(file.exists(script))
  } else {
    result <- run_package_check(root, output)
    build_log <- if (file.exists(file.path(output, "build.log"))) {
      readLines(file.path(output, "build.log"), warn = FALSE)
    } else character()
    check_log <- if (file.exists(file.path(output, "check.log"))) {
      readLines(file.path(output, "check.log"), warn = FALSE)
    } else character()
    testthat::expect_equal(status_code(result), 0L,
                           info = paste(c(result, build_log, check_log), collapse = "\n"))
    testthat::expect_true(file.exists(file.path(output, "fixturecheck_0.1.0.tar.gz")))
    testthat::expect_true(file.exists(file.path(output, "build.log")))
    testthat::expect_true(file.exists(file.path(output, "check.log")))
  }
})

testthat::test_that("native package checking fails on an undisposed package warning", {
  root <- new_check_fixture(warn_on_load = TRUE)
  output <- package_check_evidence_dir("warning")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
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

testthat::test_that("artifact envelope enforces the frozen schema, statuses, producer, and input hashes", {
  root <- tempfile("triomics-envelope-shape-")
  dir.create(root)
  payload <- file.path(root, "payload.json")
  manifest <- file.path(root, "manifest.json")
  writeLines("payload", payload, useBytes = TRUE)
  envelope <- triomics_artifact_envelope(
    artifact_type = "validation_summary", artifact_id = "artifact-1",
    study_id = "synthetic", run_id = "run-1", status = "complete",
    config_sha256 = paste(rep("a", 64), collapse = ""), payload = payload,
    producer_commit = "test"
  )
  envelope$inputs <- list(list(artifact_id = "prior-1", sha256 = paste(rep("b", 64), collapse = "")))
  write_test_envelope(envelope, manifest)
  testthat::expect_true(triomics_validate_envelope(manifest))

  invalid <- list(
    schema = within(envelope, schema_version <- "9.0"),
    status = within(envelope, status <- "queued"),
    producer = within(envelope, producer$commit <- NULL),
    input_hash = within(envelope, inputs[[1L]]$sha256 <- paste(rep("B", 64), collapse = "")),
    input_record = within(envelope, inputs[[1L]]$artifact_id <- NULL),
    config_hash = within(envelope, config_sha256 <- "not-a-sha256"),
    unknown_field = within(envelope, unexpected <- TRUE)
  )
  for (case in names(invalid)) {
    write_test_envelope(invalid[[case]], manifest)
    testthat::expect_false(triomics_validate_envelope(manifest), info = case)
  }
})

testthat::test_that("artifact envelope rejects traversal and symlink payload escapes", {
  root <- tempfile("triomics-envelope-root-")
  outside <- tempfile("triomics-envelope-outside-")
  dir.create(root)
  dir.create(outside)
  payload <- file.path(outside, "payload.json")
  manifest <- file.path(root, "manifest.json")
  writeLines("payload", payload, useBytes = TRUE)
  envelope <- triomics_artifact_envelope(
    artifact_type = "validation_summary", artifact_id = "artifact-1",
    study_id = "synthetic", run_id = "run-1", status = "complete",
    config_sha256 = paste(rep("a", 64), collapse = ""), payload = payload,
    producer_commit = "test"
  )

  envelope$payload_file <- "../triomics-envelope-outside-payload.json"
  outside_payload <- file.path(dirname(root), "triomics-envelope-outside-payload.json")
  writeLines("payload", outside_payload, useBytes = TRUE)
  envelope$payload_sha256 <- triomics_sha256(outside_payload)
  write_test_envelope(envelope, manifest)
  testthat::expect_false(triomics_validate_envelope(manifest))
  unlink(outside_payload)

  link <- file.path(root, "outside-link")
  linked <- suppressWarnings(file.symlink(outside, link))
  if (!linked && .Platform$OS.type == "windows") {
    tryCatch(
      system2("cmd.exe", c("/c", "mklink", "/J", shQuote(link), shQuote(outside)),
               stdout = FALSE, stderr = FALSE),
      error = function(e) invisible(NULL)
    )
    linked <- dir.exists(link)
  }
  testthat::expect_true(linked, info = "This test requires a local directory symlink or junction.")
  if (linked) {
    envelope$payload_file <- "outside-link/payload.json"
    envelope$payload_sha256 <- triomics_sha256(payload)
    write_test_envelope(envelope, manifest)
    testthat::expect_false(triomics_validate_envelope(manifest))
    unlink(link, recursive = FALSE)
  }
  unlink(c(root, outside), recursive = TRUE)
})
