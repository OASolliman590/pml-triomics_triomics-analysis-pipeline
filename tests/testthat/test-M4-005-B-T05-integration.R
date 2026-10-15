m4005_b05_fixture <- function() {
  triomics_test_resource("examples", "tickets", "B-T05.yaml")
}

m4005_b05_config <- function(fallback = "none", method = "deqms", count_path = NULL,
                             assay_path = NULL) {
  fixture <- m4005_b05_fixture()
  source <- paste(readLines(fixture, warn = FALSE), collapse = "\n")
  if (!is.null(count_path)) {
    value <- as.character(jsonlite::toJSON(count_path, auto_unbox = TRUE))
    source <- sub("      count_metadata: null", paste0("      count_metadata: ", value),
                  source, fixed = TRUE)
  }
  source <- sub("      fallback: none", paste0("      fallback: ", fallback), source, fixed = TRUE)
  if (!identical(method, "deqms")) {
    source <- sub("      method: deqms", paste0("      method: ", method), source, fixed = TRUE)
  }
  if (!is.null(assay_path)) {
    value <- as.character(jsonlite::toJSON(assay_path, auto_unbox = TRUE))
    source <- gsub("file: ../data/paired/proteomics.tsv", paste0("file: ", value),
                   source, fixed = TRUE)
  }
  path <- tempfile("m4-005-b-t05-", tmpdir = dirname(fixture), fileext = ".yaml")
  writeLines(source, path, useBytes = TRUE)
  path
}

m4005_b05_count_file <- function() {
  fixture <- m4005_b05_fixture()
  config <- triomics_read_config(fixture)
  ingested <- triomics_ingest_configured_study(config, fixture)
  feature_keys <- as.character(ingested$feature_registry$feature_key)
  sample_ids <- as.character(ingested$sample_metadata$sample_id)
  counts <- expand.grid(feature_key = feature_keys, sample_id = sample_ids,
                        stringsAsFactors = FALSE)
  counts$psm_count <- match(counts$feature_key, feature_keys) +
    match(counts$sample_id, sample_ids) + 1L
  counts$acquisition <- "synthetic_m4_005_acquisition"
  counts$provenance <- "synthetic public M4-005 count fixture"
  path <- tempfile("m4-005-b-t05-counts-", fileext = ".tsv")
  utils::write.table(counts, path, sep = "\t", row.names = FALSE, quote = TRUE)
  path
}

m4005_b05_partial_assay <- function() {
  fixture <- m4005_b05_fixture()
  source_path <- file.path(dirname(fixture), "../data/paired/proteomics.tsv")
  assay <- utils::read.delim(source_path, check.names = FALSE, stringsAsFactors = FALSE,
                             na.strings = c("", "NA"))
  assay[1L, -1L] <- NA_real_
  path <- tempfile("m4-005-b-t05-partial-", tmpdir = dirname(fixture), fileext = ".tsv")
  utils::write.table(assay, path, sep = "\t", row.names = FALSE, quote = FALSE, na = "NA")
  path
}

m4005_b05_execute <- function(fallback = "none", method = "deqms", count_path = NULL,
                              assay_path = NULL) {
  fixture <- m4005_b05_config(fallback, method, count_path, assay_path)
  output <- tempfile("m4-005-b-t05-output-")
  result <- tryCatch(
    triomics_execute_stage_b(fixture, output),
    error = function(error) {
      unlink(c(fixture, output), recursive = TRUE)
      stop(error)
    }
  )
  payload_path <- file.path(output, "B", "block-results.json")
  envelope_path <- file.path(output, "B", "block-results.envelope.json")
  run_path <- file.path(output, "registries", "run.json")
  list(
    result = result,
    payload = jsonlite::read_json(payload_path, simplifyVector = FALSE),
    envelope = jsonlite::read_json(envelope_path, simplifyVector = FALSE),
    envelope_path = envelope_path,
    run = jsonlite::read_json(run_path, simplifyVector = FALSE),
    cleanup = c(fixture, output)
  )
}

m4005_expect_artifact_status <- function(run, status) {
  testthat::expect_identical(run$result$status, status)
  testthat::expect_identical(run$result$method_status, run$payload$method_status)
  testthat::expect_identical(run$payload$status, status)
  testthat::expect_identical(run$envelope$status, status)
  testthat::expect_identical(run$run$status, status)
  testthat::expect_true(triomics_validate_envelope(run$envelope_path))
}

m4005_expect_fallback_rows <- function(payload) {
  rows <- payload$blocks$block_1$contrasts$target_minus_reference$rows
  testthat::expect_gt(length(rows), 0L)
  for (row in rows) {
    testthat::expect_identical(row$method_id, "limma_explicit_fallback")
    testthat::expect_null(row$count_prior_ref)
    testthat::expect_null(row$deqms_sca_t)
    testthat::expect_null(row$deqms_sca_p)
    testthat::expect_null(row$method_statistic_field)
    testthat::expect_null(row$method_p_field)
  }
}

testthat::test_that("M4-005A serializes unavailable DEqMS without fallback", {
  run <- m4005_b05_execute()
  on.exit(unlink(run$cleanup, recursive = TRUE), add = TRUE)

  m4005_expect_artifact_status(run, "unavailable")
  testthat::expect_identical(run$payload$method_status, "unavailable")
  testthat::expect_identical(run$payload$requested_method, "DEqMS")
  testthat::expect_identical(run$payload$method, "DEqMS")
  testthat::expect_identical(run$payload$unavailable_reason, "count_metadata_missing")
  testthat::expect_identical(run$payload$deqms_status$status, "unavailable")
  testthat::expect_identical(run$payload$deqms_status$reason, "count_metadata_missing")
  testthat::expect_false(isTRUE(run$payload$fallback_applied))
  testthat::expect_identical(run$envelope$reason_codes, "count_metadata_missing")
})

testthat::test_that("M4-005A serializes complete explicit limma fallback", {
  run <- m4005_b05_execute(fallback = "explicit_limma")
  on.exit(unlink(run$cleanup, recursive = TRUE), add = TRUE)

  m4005_expect_artifact_status(run, "complete")
  testthat::expect_identical(run$payload$method_status, "fallback_complete")
  testthat::expect_identical(run$payload$requested_method, "DEqMS")
  testthat::expect_identical(run$payload$method, "limma_explicit_fallback")
  testthat::expect_true(isTRUE(run$payload$fallback_applied))
  testthat::expect_identical(run$payload$deqms_status$status, "unavailable")
  testthat::expect_identical(run$payload$deqms_status$reason, "count_metadata_missing")
  testthat::expect_identical(run$payload$fallback_provenance$source_method, "limma")
  testthat::expect_identical(run$payload$fallback_provenance$status, "applied")
  testthat::expect_identical(run$payload$fallback_provenance$trigger_reason,
                             "count_metadata_missing")
  testthat::expect_true(isTRUE(run$payload$fallback_provenance$user_declared))
  testthat::expect_identical(run$envelope$reason_codes, "count_metadata_missing")
  m4005_expect_fallback_rows(run$payload)
})

testthat::test_that("M4-005A serializes partial explicit limma fallback", {
  assay_path <- m4005_b05_partial_assay()
  on.exit(unlink(assay_path), add = TRUE)
  run <- m4005_b05_execute(fallback = "explicit_limma", assay_path = assay_path)
  on.exit(unlink(c(run$cleanup, assay_path), recursive = TRUE), add = TRUE)

  m4005_expect_artifact_status(run, "partial")
  testthat::expect_identical(run$payload$method_status, "fallback_partial")
  testthat::expect_identical(run$payload$requested_method, "DEqMS")
  testthat::expect_identical(run$payload$method, "limma_explicit_fallback")
  testthat::expect_true(isTRUE(run$payload$fallback_applied))
  testthat::expect_identical(run$payload$deqms_status$reason, "count_metadata_missing")
  testthat::expect_identical(run$payload$fallback_provenance$trigger_reason,
                             "count_metadata_missing")
  testthat::expect_identical(run$envelope$reason_codes, "count_metadata_missing")
  m4005_expect_fallback_rows(run$payload)
})

testthat::test_that("M4-005A leaves ordinary limma and DEqMS provenance clean", {
  ordinary <- m4005_b05_execute(method = "limma")
  on.exit(unlink(ordinary$cleanup, recursive = TRUE), add = TRUE)
  m4005_expect_artifact_status(ordinary, "complete")
  testthat::expect_identical(ordinary$payload$method_status, "complete")
  testthat::expect_length(ordinary$envelope$reason_codes, 0L)
  testthat::expect_length(intersect(
    c("requested_method", "method", "unavailable_reason", "deqms_status", "deqms_version",
      "fallback_applied", "fallback_provenance"), names(ordinary$payload)
  ), 0L)

  count_path <- m4005_b05_count_file()
  on.exit(unlink(count_path), add = TRUE)
  deqms <- m4005_b05_execute(count_path = count_path)
  on.exit(unlink(c(deqms$cleanup, count_path), recursive = TRUE), add = TRUE)
  m4005_expect_artifact_status(deqms, "complete")
  testthat::expect_identical(deqms$payload$method_status, "complete")
  testthat::expect_identical(deqms$payload$requested_method, "DEqMS")
  testthat::expect_identical(deqms$payload$method, "DEqMS")
  testthat::expect_identical(deqms$payload$deqms_version, "1.30.0")
  testthat::expect_false(isTRUE(deqms$payload$fallback_applied))
  testthat::expect_null(deqms$payload$unavailable_reason)
  testthat::expect_null(deqms$payload$fallback_provenance)
  testthat::expect_length(deqms$envelope$reason_codes, 0L)
})

testthat::test_that("M4-005A rejects statuses outside the Stage B result contract", {
  error <- tryCatch(triomics_b_artifact_status("fallback_failed"), error = identity)
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "B_RESULT_CONTRACT")
})
