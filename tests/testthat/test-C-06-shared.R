c06_shared_cli_run <- function(output_root, stage = "C", config_path = NULL) {
  cli <- triomics_test_resource("exec", "triomics")
  if (is.null(config_path)) {
    config_path <- triomics_test_resource("examples", "tickets", "C-06.yaml")
  }
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", shQuote(stage), "--output", shQuote(output_root)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = output)
}

c06_seed_primary_nominations <- function(output_root) {
  config_path <- triomics_test_resource("examples", "tickets", "C-06.yaml")
  config <- triomics_read_config(config_path)
  analysis <- config$analyses[[1L]]
  b_root <- file.path(output_root, "B")
  block_results_path <- file.path(b_root, "block-results.json")
  block_results <- list(
    schema_version = "2.0",
    artifact_type = "block_results",
    study_id = config$study_id,
    analysis_id = analysis$analysis_id,
    preparation_context_id = analysis$preparation_context_id,
    representation_id = analysis$design$representation,
    status = "complete",
    config_sha256 = attr(config, "config_sha256"),
    blocks = list()
  )
  triomics_write_json_atomic(block_results, block_results_path)
  block_results_envelope <- triomics_artifact_envelope(
    artifact_type = "block_results",
    artifact_id = "stage-b-block-results",
    study_id = config$study_id,
    run_id = "b-t01",
    status = "complete",
    config_sha256 = attr(config, "config_sha256"),
    payload = block_results_path,
    producer_commit = "synthetic-test-input",
    inputs = list(),
    reason_codes = character(),
    privacy_class = "synthetic"
  )
  block_results_envelope$analysis_id <- analysis$analysis_id
  block_results_envelope$preparation_context_id <- analysis$preparation_context_id
  block_results_envelope$representation_id <- analysis$design$representation
  triomics_write_envelope(
    block_results_envelope,
    file.path(b_root, "block-results.envelope.json")
  )
  nomination_payload <- list(
    schema_version = "2.0",
    artifact_type = "candidate_nominations",
    study_id = config$study_id,
    analysis_id = analysis$analysis_id,
    status = "complete",
    source_artifact_id = "stage-b-block-results",
    n_nominations = 0L,
    nominations = list()
  )
  nomination_path <- file.path(
    b_root, "handoffs", "synthetic-c06", "candidate-nominations.json"
  )
  triomics_b07_write_enveloped_json(
    nomination_payload,
    nomination_path,
    artifact_type = "candidate_nominations",
    artifact_id = "b-t07-candidate-nominations",
    study_id = config$study_id,
    config_sha256 = attr(config, "config_sha256"),
    inputs = list(list(
      artifact_id = "stage-b-block-results",
      sha256 = triomics_sha256(block_results_path)
    )),
    status = "complete",
    producer_commit = "synthetic-test-input",
    privacy_class = "synthetic"
  )
  nomination_path
}

testthat::test_that("Stage C consumes the verified B nomination artifact and preserves its lineage", {
  output_root <- tempfile("triomics-c06-shared-cli-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)

  nomination_path <- c06_seed_primary_nominations(output_root)
  nomination_paths <- list.files(
    file.path(output_root, "B", "handoffs"),
    pattern = "^candidate-nominations[.]json$",
    recursive = TRUE,
    full.names = TRUE
  )
  testthat::expect_length(nomination_paths, 1L)
  testthat::expect_identical(nomination_paths, nomination_path)
  nomination_envelope_path <- sub("[.]json$", ".envelope.json", nomination_path)
  testthat::expect_true(triomics_validate_envelope(nomination_envelope_path))
  nomination_payload <- jsonlite::read_json(nomination_path, simplifyVector = FALSE)
  nomination_envelope <- jsonlite::read_json(nomination_envelope_path, simplifyVector = FALSE)
  block_results <- jsonlite::read_json(
    file.path(output_root, "B", "block-results.json"), simplifyVector = FALSE
  )

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
  primary <- candidates$branches$primary_statistics
  testthat::expect_identical(primary$status, nomination_payload$status)
  testthat::expect_identical(primary$source_artifact_id, nomination_envelope$artifact_id)
  testthat::expect_identical(primary$source_artifact_ref$sha256, triomics_sha256(nomination_path))
  testthat::expect_identical(candidates$study_id, nomination_payload$study_id)
  testthat::expect_identical(candidates$analysis_id, nomination_payload$analysis_id)
  testthat::expect_identical(primary$preparation_context_id, block_results$preparation_context_id)
  testthat::expect_identical(primary$representation_id, block_results$representation_id)
  testthat::expect_identical(primary$nominations, nomination_payload$nominations)
  testthat::expect_identical(primary$status, "complete")
  testthat::expect_length(primary$nominations, 0L)
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
  for (block_id in c("block_1", "block_2")) {
    roles <- integration$eligibility$source_roles[[block_id]]
    selection <- integration$eligibility$source_selection[[block_id]]
    testthat::expect_identical(roles$status, "supplied")
    testthat::expect_identical(unlist(roles$value, use.names = FALSE), "measurement_matrix")
    testthat::expect_identical(selection$status, "supplied")
    testthat::expect_identical(selection$value$selection_history, "full_unselected")
    testthat::expect_identical(selection$value$inference_gate, "declared_unselected")
  }
  state_hashes <- integration$eligibility$preparation_state_hashes
  testthat::expect_identical(sort(names(state_hashes)), c("block_1", "block_2"))
  testthat::expect_true(all(vapply(state_hashes, triomics_sha256_text, logical(1L))))
  state_dir <- file.path(output_root, "C", "preparation-states")
  for (block_id in names(state_hashes)) {
    state_ref <- list(
      state_id = paste0("st_", substr(state_hashes[[block_id]], 1L, 20L)),
      state_hash = state_hashes[[block_id]],
      path = paste0("st_", substr(state_hashes[[block_id]], 1L, 20L), ".json")
    )
    state <- triomics_read_state(state_ref, state_dir)
    testthat::expect_identical(state$block_id, block_id)
    testthat::expect_identical(state$state_hash, state_hashes[[block_id]])
  }
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
  testthat::expect_identical(native_payload$preparation_state_hashes, state_hashes)
  testthat::expect_identical(sort(names(native_payload$preparation_state_refs)),
                             c("block_1", "block_2"))
  for (block_id in names(state_hashes)) {
    state_ref <- native_payload$preparation_state_refs[[block_id]]
    state_path <- file.path(output_root, state_ref$path)
    testthat::expect_identical(state_ref$state_hash, state_hashes[[block_id]])
    testthat::expect_identical(state_ref$payload_sha256, triomics_sha256(state_path))
    testthat::expect_true(any(vapply(native_envelope$inputs, function(input) {
      identical(input$artifact_id, paste("preparation-state", block_id, sep = "::")) &&
        identical(input$sha256, triomics_sha256(state_path))
    }, logical(1L))))
    testthat::expect_false(identical(state_ref$payload_sha256, state_ref$state_hash))
  }
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
  split_plan_path <- file.path(output_root, "C", "evaluation-plans", "c06-control-split-plan.json")
  split_plan_envelope_path <- sub("[.]json$", ".envelope.json", split_plan_path)
  testthat::expect_false(file.exists(split_plan_path))
  testthat::expect_false(file.exists(split_plan_envelope_path))
  testthat::expect_identical(
    evaluation$controls[[1L]]$candidate$match_context$split_plan_status,
    "not_run"
  )
  testthat::expect_null(evaluation$controls[[1L]]$candidate$match_context$split_plan_sha256)

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
  testthat::expect_identical(run_record$status, "complete")
})

testthat::test_that("Stage C keeps an absent primary nomination source explicitly not requested", {
  output_root <- tempfile("triomics-c06-no-primary-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)

  cli <- c06_shared_cli_run(output_root)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))
  payload <- jsonlite::read_json(
    file.path(output_root, "D", "candidate-nominations.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(payload$branches$primary_statistics$status, "not_requested")
  testthat::expect_true(is.null(payload$branches$primary_statistics$source_artifact_ref))
})

testthat::test_that("nested C-02 Stage-C evaluation retains verified units for its unrun D controls", {
  output_root <- tempfile("triomics-c02-nested-cli-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  config <- triomics_test_resource("examples", "tickets", "C-02.yaml")

  cli <- c06_shared_cli_run(output_root, config_path = config)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))
  testthat::expect_false(any(grepl("C_EVAL_EVIDENCE_INVALID", cli$output, fixed = TRUE)))
  has_d_evidence <- file.exists(file.path(output_root, "D", "evaluation-evidence.json"))
  testthat::expect_true(has_d_evidence)
  testthat::skip_if_not(has_d_evidence, "The C-02 handoff aborted before D control read-back.")
  evaluation <- jsonlite::read_json(
    file.path(output_root, "D", "evaluation-evidence.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(evaluation$status, "partial")
  context <- evaluation$controls[[1L]]$candidate$match_context
  testthat::expect_identical(sort(names(context$feature_keys)), c("block_1", "block_2"))
  testthat::expect_true(all(vapply(context$feature_keys, length, integer(1L)) > 0L))
  config_values <- triomics_read_config(config)
  analysis <- config_values$analyses[[1L]]
  c_integration <- jsonlite::read_json(
    file.path(output_root, "C", "integration-results.json"), simplifyVector = FALSE
  )
  predictions <- c_integration$integrations[[1L]]$evaluation$predictions
  predicted_units <- if (is.list(predictions) && !is.null(predictions$unit_id)) {
    as.character(unlist(predictions$unit_id, use.names = FALSE))
  } else {
    vapply(predictions, `[[`, character(1L), "unit_id")
  }
  testthat::expect_setequal(unlist(context$sample_ids, use.names = FALSE), unique(predicted_units))
  testthat::expect_false(is.null(context$split_plan_sha256))
  testthat::expect_identical(
    context$split_plan_sha256,
    c_integration$integrations[[1L]]$evaluation$split_plan_hash
  )
  testthat::expect_false(file.exists(file.path(
    output_root, "C", "evaluation-plans", "c06-control-split-plan.json"
  )))
  testthat::expect_identical(context$split_plan_status, "executed")
  d_root <- file.path(output_root, "D")
  expected_files <- c(
    "candidate-nominations.json", "candidate-nominations.envelope.json",
    "integration-evidence.json", "integration-evidence.envelope.json",
    "integration-comparisons.json", "integration-comparisons.envelope.json",
    "evaluation-evidence.json", "evaluation-evidence.envelope.json",
    "stage-status.json", "stage-status.envelope.json",
    "handoff-manifest.json", "handoff-manifest.envelope.json"
  )
  has_complete_handoff <- all(file.exists(file.path(d_root, expected_files)))
  testthat::expect_true(has_complete_handoff)
  testthat::skip_if_not(has_complete_handoff, "The C-02 handoff is missing one or more D artifacts.")
  manifest <- jsonlite::read_json(
    file.path(output_root, "D", "handoff-manifest.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(manifest$status, "unavailable")
  testthat::expect_length(manifest$artifacts, 5L)
  for (artifact in manifest$artifacts) {
    payload_path <- file.path(d_root, artifact$payload_file)
    envelope_path <- file.path(d_root, artifact$envelope_file)
    testthat::expect_identical(triomics_sha256(payload_path), artifact$payload_sha256)
    testthat::expect_identical(triomics_sha256(envelope_path), artifact$envelope_sha256)
    testthat::expect_true(triomics_validate_envelope(envelope_path))
    envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)
    testthat::expect_identical(envelope$privacy_class, "synthetic")
    testthat::expect_identical(envelope$study_id, config_values$study_id)
    testthat::expect_identical(envelope$analysis_id, analysis$analysis_id)
    testthat::expect_identical(envelope$preparation_context_id, analysis$preparation_context_id)
    testthat::expect_identical(envelope$representation_id, analysis$design$representation)
  }
  manifest_envelope_path <- file.path(d_root, "handoff-manifest.envelope.json")
  manifest_envelope <- jsonlite::read_json(manifest_envelope_path, simplifyVector = FALSE)
  testthat::expect_identical(triomics_sha256(file.path(d_root, "handoff-manifest.json")),
                             manifest_envelope$payload_sha256)
  testthat::expect_true(triomics_validate_envelope(manifest_envelope_path))
  testthat::expect_identical(manifest$study_id, config_values$study_id)
  testthat::expect_identical(manifest$analysis_id, analysis$analysis_id)
  candidates <- jsonlite::read_json(
    file.path(d_root, "candidate-nominations.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(candidates$branches$primary_statistics$status, "not_requested")
  integration_evidence <- jsonlite::read_json(
    file.path(d_root, "integration-evidence.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(integration_evidence$status, "unavailable")
  testthat::expect_identical(integration_evidence$integrations[[1L]]$status, "unavailable")
  testthat::expect_identical(integration_evidence$study_id, config_values$study_id)
  testthat::expect_identical(integration_evidence$analysis_id, analysis$analysis_id)
  status_payload <- jsonlite::read_json(
    file.path(d_root, "stage-status.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(status_payload$status, "unavailable")
  testthat::expect_identical(status_payload$clinical_prediction$status, "not_started")
  run_record <- jsonlite::read_json(
    file.path(output_root, "registries", "run.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(run_record$status, "complete")
  nested <- c_integration$integrations[[1L]]$evaluation
  testthat::expect_identical(nested$n_predicted, 12L)
  testthat::expect_identical(nested$total_denominator, 12L)
  testthat::expect_identical(nested$failed_prediction_count, 0L)
  testthat::expect_identical(c_integration$study_id, config_values$study_id)
  testthat::expect_identical(c_integration$analysis_id, analysis$analysis_id)
})

testthat::test_that("unrun C-07 controls can declare no split plan while complete controls require one", {
  no_plan_context <- list(
    sample_ids = c("u1", "u2"),
    feature_keys = list(block_1 = "f1", block_2 = "f2"),
    preparation_context_id = "prep-no-plan",
    representation_id = "levels",
    split_plan_status = "not_run",
    split_plan_sha256 = NULL
  )
  unrun <- function(method) list(
    method = method,
    status = "not_run",
    artifact_ref = NULL,
    artifact_sha256 = NULL,
    metrics = NULL,
    reason = "C-07 control and split plan were not run.",
    match_context = no_plan_context
  )
  unrun_controls <- lapply(triomics_c06_control_names(), function(name) list(
    control = name,
    candidate = unrun("not_selected"),
    baseline = unrun("concat_pca")
  ))
  validated <- triomics_c06_evaluation_evidence(unrun_controls)
  testthat::expect_true(all(vapply(validated$controls, function(control) {
    identical(control$candidate$status, "not_run") &&
      identical(control$baseline$status, "not_run") &&
      identical(control$candidate$match_context$split_plan_status, "not_run") &&
      is.null(control$candidate$match_context$split_plan_sha256)
  }, logical(1L))))

  incomplete_run <- structure(list(
    method = "concat_pca",
    split_plan_hash = NULL,
    predictions = data.frame(unit_id = c("u1", "u2"), prediction = c(1, 2)),
    metrics = list(model = list(score = 1), baseline = list(score = 0)),
    n_predicted = 2L,
    total_denominator = 2L,
    failed_prediction_count = 0L,
    inspected_fit_count = 1L,
    max_fits = 1L
  ), class = "EvaluationResult")
  complete <- list(
    method = "concat_pca",
    status = "complete",
    artifact_ref = "c07-complete-control",
    artifact_sha256 = paste(rep("a", 64L), collapse = ""),
    metrics = NULL,
    reason = NULL,
    evaluation_result = incomplete_run,
    match_context = no_plan_context
  )
  complete_control <- list(
    control = "null",
    candidate = complete,
    baseline = complete
  )
  testthat::expect_error(
    triomics_c06_evaluation_evidence(c(
      list(complete_control), unrun_controls[-1L]
    )),
    class = "triomics_error"
  )
})
