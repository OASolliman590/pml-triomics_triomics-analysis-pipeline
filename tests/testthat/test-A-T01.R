status_code <- function(result) {
  status <- attr(result, "status")
  if (is.null(status)) 0L else status
}

triomics_cli <- function(args, timeout = 5) {
  cli <- triomics_test_resource("exec", "triomics")
  old_wd <- setwd(dirname(cli))
  on.exit(setwd(old_wd), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  args <- as.character(args)
  path_options <- which(args %in% c("--config", "--output")) + 1L
  path_options <- path_options[path_options <= length(args)]
  args[path_options] <- shQuote(args[path_options])
  suppressWarnings(system2(rscript, c("--vanilla", shQuote(basename(cli)), args), stdout = TRUE,
                            stderr = TRUE, timeout = timeout))
}

write_test_envelope <- function(envelope, path) {
  jsonlite::write_json(envelope, path, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
}

testthat::test_that("package resources resolve from source and installed layouts", {
  ticket <- triomics_test_resource("examples", "tickets", "A-T01.yaml")
  cli <- triomics_test_resource("exec", "triomics")
  testthat::expect_true(file.exists(ticket))
  testthat::expect_true(file.exists(cli))
})

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
  fixture <- triomics_test_resource("examples", "tickets", "A-T01.yaml")
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
  fixture <- triomics_test_resource("examples", "tickets", "A-T01.yaml")
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

  fixture <- triomics_test_resource("examples", "tickets", "A-T01.yaml")
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
  fixture <- triomics_test_resource("examples", "tickets", "A-T01.yaml")
  output <- tempfile("triomics-a-t01-")
  result <- triomics_cli(c("--config", fixture, "--stage", "A", "--output", output,
                           "--validate-only"), timeout = 5)
  testthat::expect_equal(status_code(result), 0L, info = paste(result, collapse = "\n"))
  testthat::expect_true(file.exists(file.path(output, "validation-summary.json")))
  testthat::expect_false(any(grepl("checkpoint", list.files(output, recursive = TRUE),
                                  ignore.case = TRUE)))
})

testthat::test_that("CLI finalization failures return execution exit code 4", {
  fixture <- triomics_test_resource("examples", "tickets", "A-T01.yaml")
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
