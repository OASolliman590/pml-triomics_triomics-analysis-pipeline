c06_shared_cli_run <- function(output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  config <- triomics_test_resource("examples", "tickets", "C-06.yaml")
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config),
      "--stage", "C", "--output", shQuote(output_root)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = output)
}

testthat::test_that("Stage C writes a lineage-bound, honestly partial C-06 handoff", {
  output_root <- tempfile("triomics-c06-shared-cli-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)

  cli <- c06_shared_cli_run(output_root)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))

  d_root <- file.path(output_root, "D")
  expected_files <- c(
    "candidate-nominations.json", "candidate-nominations.envelope.json",
    "integration-evidence.json", "integration-evidence.envelope.json",
    "integration-comparisons.json", "integration-comparisons.envelope.json",
    "evaluation-evidence.json", "evaluation-evidence.envelope.json",
    "stage-status.json", "stage-status.envelope.json",
    "handoff-manifest.json", "handoff-manifest.envelope.json"
  )
  has_handoff <- all(file.exists(file.path(d_root, expected_files)))
  testthat::expect_true(has_handoff)
  testthat::skip_if_not(has_handoff, "C-06 D handoff is not wired into the Stage-C CLI yet.")

  manifest_path <- file.path(d_root, "handoff-manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  testthat::expect_identical(manifest$status, "partial")
  testthat::expect_length(manifest$artifacts, 5L)
  for (artifact in manifest$artifacts) {
    payload_path <- file.path(d_root, artifact$payload_file)
    envelope_path <- file.path(d_root, artifact$envelope_file)
    testthat::expect_identical(triomics_sha256(payload_path), artifact$payload_sha256)
    testthat::expect_identical(triomics_sha256(envelope_path), artifact$envelope_sha256)
    testthat::expect_true(triomics_validate_envelope(envelope_path))
    envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)
    testthat::expect_identical(envelope$privacy_class, "synthetic")
    testthat::expect_identical(envelope$preparation_context_id, "c06_fixture")
    testthat::expect_identical(envelope$representation_id, "levels")
  }
  testthat::expect_identical(triomics_sha256(manifest_path),
                             jsonlite::read_json(file.path(d_root, "handoff-manifest.envelope.json"),
                                                 simplifyVector = FALSE)$payload_sha256)
  testthat::expect_true(triomics_validate_envelope(file.path(d_root, "handoff-manifest.envelope.json")))

  candidates <- jsonlite::read_json(file.path(d_root, "candidate-nominations.json"), simplifyVector = FALSE)
  testthat::expect_identical(candidates$branches$primary_statistics$status, "not_requested")
  testthat::expect_identical(candidates$branches$integration_selection$status, "complete")
  testthat::expect_gt(candidates$n_nominations, 0L)

  c_payload_path <- file.path(output_root, "C", "integration-results.json")
  testthat::expect_true(file.exists(c_payload_path))
  testthat::expect_true(triomics_validate_envelope(file.path(output_root, "C", "integration-results.envelope.json")))
  testthat::expect_true(file.exists(file.path(output_root, "C", "score-endpoints", "integration-01.json")))
  integration_bundle <- jsonlite::read_json(file.path(d_root, "integration-evidence.json"), simplifyVector = FALSE)
  testthat::expect_length(integration_bundle$integrations, 1L)
  integration <- integration_bundle$integrations[[1L]]
  testthat::expect_identical(integration$status, "complete")
  testthat::expect_identical(integration$backend, "concat_pca")
  native_path <- file.path(output_root, "C", "native-states", "integration-01.json")
  native_envelope_path <- file.path(output_root, "C", "native-states", "integration-01.envelope.json")
  has_native_sidecar <- file.exists(native_path) && file.exists(native_envelope_path)
  testthat::expect_true(has_native_sidecar)
  testthat::skip_if_not(has_native_sidecar, "C-06 CLI has not persisted the native integration sidecar yet.")
  native_payload <- jsonlite::read_json(native_path, simplifyVector = FALSE)
  native_envelope <- jsonlite::read_json(native_envelope_path, simplifyVector = FALSE)
  testthat::expect_identical(integration$native_artifact_ref$artifact_id, native_payload$artifact_id)
  testthat::expect_identical(integration$native_artifact_ref$sha256, triomics_sha256(native_path))
  testthat::expect_identical(native_envelope$artifact_type, "integration_native_state")
  testthat::expect_identical(native_envelope$privacy_class, "synthetic")
  testthat::expect_identical(native_envelope$preparation_context_id, "c06_fixture")
  testthat::expect_true(triomics_validate_envelope(native_envelope_path))
  integration_envelope <- jsonlite::read_json(
    file.path(d_root, "integration-evidence.envelope.json"), simplifyVector = FALSE
  )
  testthat::expect_true(any(vapply(integration_envelope$inputs, function(input) {
    identical(input$artifact_id, integration$native_artifact_ref$artifact_id) &&
      identical(input$sha256, integration$native_artifact_ref$sha256)
  }, logical(1))))
  testthat::expect_identical(native_payload$native$state_sha256, integration$native$state_sha256)
  testthat::expect_identical(integration$eligibility$status, "validated")
  testthat::expect_true(length(integration$native$scores$values) > 0L)
  testthat::expect_true(length(integration$common$scores$values) > 0L)
  testthat::expect_true(length(integration$selection$selected) > 0L)
  testthat::expect_identical(integration$variance$native_definition$metric,
                             "stats::prcomp_global_variance_fraction")
  testthat::expect_identical(integration$variance$common_definition$metric,
                             "orthogonal_pca_block_projection")

  evaluation <- jsonlite::read_json(file.path(d_root, "evaluation-evidence.json"), simplifyVector = FALSE)
  testthat::expect_identical(evaluation$status, "partial")
  testthat::expect_identical(vapply(evaluation$controls, `[[`, character(1), "control"),
                             c("null", "weak_signal", "strong_signal", "adversarial"))
  testthat::expect_true(all(vapply(evaluation$controls, function(control) {
    identical(control$status, "not_run") &&
      identical(control$candidate$status, "not_run") &&
      identical(control$baseline$status, "not_run") &&
      identical(control$candidate$method, "not_selected") &&
      identical(control$baseline$method, "concat_pca") &&
      identical(control$candidate$match_context, control$baseline$match_context) &&
      is.null(control$candidate$metrics) && is.null(control$baseline$metrics) &&
      grepl("not run", control$candidate$reason, ignore.case = TRUE) &&
      grepl("not run", control$baseline$reason, ignore.case = TRUE)
  }, logical(1))))
  testthat::expect_identical(evaluation$controls[[1L]]$candidate$match_context$preparation_context_id,
                             "c06_fixture")
  testthat::expect_identical(evaluation$controls[[1L]]$candidate$match_context$representation_id,
                             "levels")

  status_payload <- jsonlite::read_json(file.path(d_root, "stage-status.json"), simplifyVector = FALSE)
  testthat::expect_identical(status_payload$status, "partial")
  testthat::expect_identical(status_payload$backend_statuses$concat_pca$status, "complete")
  testthat::expect_true(all(vapply(status_payload$backend_statuses[setdiff(names(status_payload$backend_statuses),
                                                                           "concat_pca")],
                                   function(x) identical(x$status, "not_requested"), logical(1))))
  testthat::expect_identical(status_payload$clinical_prediction$status, "not_started")
  testthat::expect_identical(status_payload$clinical_prediction$completion_policy, "conditional")
  testthat::expect_identical(status_payload$clinical_prediction$packet_id, "C-09")
  run_record <- jsonlite::read_json(file.path(output_root, "registries", "run.json"), simplifyVector = FALSE)
  testthat::expect_identical(run_record$status, status_payload$status)
})
