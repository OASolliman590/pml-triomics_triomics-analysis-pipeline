c06_shared_cli_run <- function(output_root, stage = "C", config_path = NULL,
                               prepare_b = identical(stage, "C")) {
  cli <- triomics_test_resource("exec", "triomics")
  if (is.null(config_path)) {
    config_path <- triomics_test_resource("examples", "tickets", "C-06.yaml")
  }
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  run_cli <- function(requested_stage) {
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
        "--stage", shQuote(requested_stage), "--output", shQuote(output_root)),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    list(status = as.integer(status), output = output)
  }
  if (isTRUE(prepare_b) && identical(stage, "C")) {
    b_run <- run_cli("B")
    if (b_run$status != 0L) return(b_run)
    bundle_paths <- list.files(
      file.path(output_root, "B", "handoffs"), pattern = "^bundle[.]json$",
      recursive = TRUE, full.names = TRUE, all.files = FALSE
    )
    if (length(bundle_paths) == 1L) {
      bundle_path <- bundle_paths[[1L]]
      b_provenance_path <- file.path(dirname(bundle_path), "stage-handoff-provenance.json")
      b_paths <- c(
        bundle_path,
        sub("[.]json$", ".envelope.json", bundle_path),
        b_provenance_path,
        sub("[.]json$", ".envelope.json", b_provenance_path)
      )
      predecessor_hashes <- vapply(b_paths, triomics_sha256, character(1L))
    } else {
      predecessor_hashes <- character()
    }
    b_run_record <- file.path(output_root, "registries", "run.json")
    b_run_record_sha256 <- if (file.exists(b_run_record)) triomics_sha256(b_run_record) else NULL
    c_run <- run_cli(stage)
    c_run$predecessor_hashes <- predecessor_hashes
    c_run$b_run_record_sha256 <- b_run_record_sha256
    return(c_run)
  }
  run_cli(stage)
}

c06_shared_repeated_fixture <- function(kind) {
  source_config <- triomics_test_resource("examples", "tickets", "D-T01.yaml")
  fixture_root <- tempfile(paste0("c06-shared-", kind, "-fixture-"), tmpdir = "/private/tmp")
  dir.create(file.path(fixture_root, "tickets"), recursive = TRUE)
  testthat::expect_true(file.copy(source_config, file.path(fixture_root, "tickets")))
  source_data <- normalizePath(file.path(dirname(source_config), "..", "data"),
                               winslash = "/", mustWork = TRUE)
  testthat::expect_true(file.copy(source_data, fixture_root, recursive = TRUE))
  config_path <- file.path(fixture_root, "tickets", basename(source_config))
  metadata_path <- file.path(fixture_root, "data", "c04", "metadata.tsv")
  metadata <- utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE)

  if (identical(kind, "paired")) {
    subjects <- paste0("pair-", seq_len(nrow(metadata) / 2L))
    metadata$subject <- rep(subjects, each = 2L)
    metadata$visit <- rep(c("baseline", "followup"), times = length(subjects))
    metadata$time <- rep(c(0, 1), times = length(subjects))
    visit_levels <- c("baseline", "followup")
  } else if (identical(kind, "trajectory")) {
    subjects <- paste0("sub", seq_len(nrow(metadata) / 3L))
    metadata$subject <- rep(subjects, each = 3L)
    metadata$visit <- rep(c("v0", "v1", "v2"), times = length(subjects))
    metadata$time <- rep(0:2, times = length(subjects))
    visit_levels <- c("v0", "v1", "v2")
  } else {
    stop("Unsupported repeated fixture kind: ", kind)
  }
  utils::write.table(metadata, metadata_path, sep = "\t", row.names = FALSE, quote = FALSE)

  config_text <- paste(readLines(config_path, warn = FALSE), collapse = "\n")
  config_text <- sub(
    "    score: numeric",
    "    score: numeric\n    subject: string\n    visit: factor\n    time: numeric",
    config_text, fixed = TRUE
  )
  levels_text <- paste0("  factor_levels:\n    visit: [", paste(visit_levels, collapse = ", "), "]")
  config_text <- sub("  factor_levels: {}", levels_text, config_text, fixed = TRUE)
  config_text <- sub("  subject_id: null", "  subject_id: subject", config_text, fixed = TRUE)
  config_text <- sub("      subject_id: null", "      subject_id: subject", config_text, fixed = TRUE)
  config_text <- sub("      repeated: false", "      repeated: true", config_text, fixed = TRUE)

  if (identical(kind, "paired")) {
    config_text <- sub("      kind: independent", "      kind: paired", config_text, fixed = TRUE)
    config_text <- sub("      condition: score", "      condition: visit", config_text, fixed = TRUE)
    config_text <- sub("      condition_type: continuous", "      condition_type: factor", config_text, fixed = TRUE)
    config_text <- sub("      representation: levels", "      representation: delta", config_text, fixed = TRUE)
    config_text <- sub("      reference_level: null", "      reference_level: baseline", config_text, fixed = TRUE)
    config_text <- sub("      target_level: null", "      target_level: followup", config_text, fixed = TRUE)
    config_text <- sub("      visit_column: null", "      visit_column: visit", config_text, fixed = TRUE)
    config_text <- sub("      reference_visit: null", "      reference_visit: baseline", config_text, fixed = TRUE)
    config_text <- sub("      target_visit: null", "      target_visit: followup", config_text, fixed = TRUE)
    config_text <- sub("contrast_id: score_slope", "contrast_id: target_minus_reference", config_text, fixed = TRUE)
    config_text <- sub("weights: {intercept: 0, continuous.score: 1}",
                       "weights: {intercept: 1}", config_text, fixed = TRUE)
    config_text <- sub("effect_unit: per_unit_beta", "effect_unit: delta_difference", config_text, fixed = TRUE)
  } else {
    config_text <- sub("      kind: independent", "      kind: trajectory", config_text, fixed = TRUE)
    config_text <- sub("      condition: score", "      condition: time", config_text, fixed = TRUE)
    config_text <- sub("      representation: levels", "      representation: subject_centered", config_text, fixed = TRUE)
    config_text <- sub("      time_column: null", "      time_column: time", config_text, fixed = TRUE)
    config_text <- sub("      time_unit: null", "      time_unit: synthetic_step", config_text, fixed = TRUE)
    config_text <- sub("      time_origin: null", "      time_origin: '0'", config_text, fixed = TRUE)
    config_text <- sub("      residual_contract_ref: null",
                       "      residual_contract_ref: fixed_subject_ordinary_independent",
                       config_text, fixed = TRUE)
    config_text <- sub("contrast_id: score_slope", "contrast_id: time_slope", config_text, fixed = TRUE)
    trajectory_weights <- paste(
      c(paste0("subject.", subjects, ": 0"), "time.linear: 1"),
      collapse = ", "
    )
    config_text <- sub("weights: {intercept: 0, continuous.score: 1}",
                       paste0("weights: {", trajectory_weights, "}"), config_text, fixed = TRUE)
  }
  writeLines(config_text, config_path, useBytes = TRUE)
  list(root = fixture_root, config = config_path)
}

c06_shared_paired_attrition_fixture <- function() {
  fixture <- c06_shared_repeated_fixture("paired")
  config_text <- paste(readLines(fixture$config, warn = FALSE), collapse = "\n")
  config_text <- sub("      missing_visits: error",
                     "      missing_visits: complete_pairs_per_contrast",
                     config_text, fixed = TRUE)
  writeLines(config_text, fixture$config, useBytes = TRUE)
  config <- yaml::read_yaml(fixture$config)

  metadata_path <- file.path(fixture$root, "data", "c04", "metadata.tsv")
  metadata <- utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE)
  subjects <- unique(as.character(metadata$subject))
  if (length(subjects) < 2L) stop("The public fixture needs two paired attrition units.", call. = FALSE)
  excluded_subjects <- stats::setNames(subjects[(length(subjects) - 1L):length(subjects)],
                                       c("block_1", "block_2"))
  excluded_targets <- vapply(names(excluded_subjects), function(block_id) {
    subject <- excluded_subjects[[block_id]]
    target <- as.character(metadata$sample_id[metadata$subject == subject & metadata$visit == "followup"])
    if (length(target) != 1L) stop("The synthetic attrition target is not unique.", call. = FALSE)
    target
  }, character(1L))
  for (block_index in seq_along(config$blocks)) {
    block <- config$blocks[[block_index]]
    target <- excluded_targets[[block$block_id]]
    if (is.null(target)) next
    matrix_path <- normalizePath(file.path(dirname(fixture$config), block$file),
                                 winslash = "/", mustWork = TRUE)
    assay <- utils::read.delim(matrix_path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!target %in% names(assay)) stop("The declared attrition target is absent from its block.", call. = FALSE)
    assay <- assay[, names(assay) != target, drop = FALSE]
    utils::write.table(assay, matrix_path, sep = "\t", row.names = FALSE, quote = FALSE)
  }
  fixture$excluded_subjects <- excluded_subjects
  fixture$excluded_targets <- excluded_targets
  fixture$metadata <- metadata
  fixture
}

c06_shared_trajectory_missing_time_fixture <- function() {
  fixture <- c06_shared_repeated_fixture("trajectory")
  metadata_path <- file.path(fixture$root, "data", "c04", "metadata.tsv")
  metadata <- utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE)
  excluded_sample <- tail(as.character(metadata$sample_id), 1L)
  metadata$time[metadata$sample_id == excluded_sample] <- NA_real_
  utils::write.table(metadata, metadata_path, sep = "\t", row.names = FALSE,
                     quote = FALSE, na = "NA")
  config <- triomics_read_config(fixture$config)
  for (block in config$blocks) {
    matrix_path <- normalizePath(file.path(dirname(fixture$config), block$file),
                                 winslash = "/", mustWork = TRUE)
    assay <- utils::read.delim(matrix_path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!excluded_sample %in% names(assay)) {
      stop("The declared absent-all trajectory sample is missing from a source block.",
           call. = FALSE)
    }
    assay <- assay[, names(assay) != excluded_sample, drop = FALSE]
    utils::write.table(assay, matrix_path, sep = "\t", row.names = FALSE, quote = FALSE)
  }
  fixture$excluded_sample <- excluded_sample
  fixture
}

c06_shared_native_input_identity <- function(output_root, config_path) {
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  analysis <- config$analyses[[1L]]
  block_ids <- as.character(unlist(analysis$block_ids, use.names = FALSE))
  unit_ids <- as.character(ingested$sample_metadata$sample_id)
  analysis_unit_map <- NULL
  if (identical(analysis$design$representation, "levels")) {
    analysis_unit_map <- data.frame(
      unit_id = unit_ids,
      unit_type = rep("sample", length(unit_ids)),
      representation_id = rep("levels", length(unit_ids)),
      stringsAsFactors = FALSE
    )
    analysis_unit_map$source_sample_ids <- I(lapply(unit_ids, identity))
  }
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = analysis_unit_map
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  resolved <- triomics_c06_cli_ingest_lineage(resolved, ingested, config, block_ids)
  resolved$unit_ids <- unit_ids
  resolved$preparation_context_id <- analysis$preparation_context_id
  resolved$representation_id <- analysis$design$representation
  source_resolved <- resolved
  if (any(!vapply(config$analyses[[1L]]$integration, function(request) {
    identical(request$evaluation$mode, "nested_cv")
  }, logical(1L))) && identical(analysis$design$representation, "levels")) {
    resolved <- triomics_c06_cli_prepare_resolved(
      resolved, ingested, config, analysis, block_ids, output_root
    )
  }
  source_inputs <- unname(lapply(ingested$source_manifests, function(source) {
    list(artifact_id = source$source_id, sha256 = source$sha256)
  }))
  identity <- triomics_c06_cli_input_identity(
    resolved, ingested, analysis, analysis$design$representation,
    block_ids, source_inputs, config, source_resolved
  )
  list(config = config, ingested = ingested, analysis = analysis,
       resolved = resolved, block_ids = block_ids, identity = identity)
}

testthat::test_that("Stage C consumes the verified B nomination artifact and preserves its lineage", {
  output_root <- tempfile("triomics-c06-shared-cli-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)

  cli <- c06_shared_cli_run(output_root)
  nomination_paths <- list.files(
    file.path(output_root, "B", "handoffs"),
    pattern = "^candidate-nominations[.]json$",
    recursive = TRUE,
    full.names = TRUE
  )
  testthat::expect_length(nomination_paths, 1L)
  nomination_path <- nomination_paths[[1L]]
  testthat::expect_identical(nomination_paths, nomination_path)
  nomination_envelope_path <- sub("[.]json$", ".envelope.json", nomination_path)
  testthat::expect_true(triomics_validate_envelope(nomination_envelope_path))
  nomination_payload <- jsonlite::read_json(nomination_path, simplifyVector = FALSE)
  nomination_envelope <- jsonlite::read_json(nomination_envelope_path, simplifyVector = FALSE)
  block_results <- jsonlite::read_json(
    file.path(output_root, "B", "block-results.json"), simplifyVector = FALSE
  )

  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))

  bundle_paths <- list.files(
    file.path(output_root, "B", "handoffs"), pattern = "^bundle[.]json$",
    recursive = TRUE, full.names = TRUE
  )
  testthat::expect_length(bundle_paths, 1L)
  bundle_path <- bundle_paths[[1L]]
  bundle_envelope_path <- sub("[.]json$", ".envelope.json", bundle_path)
  b_provenance_path <- file.path(dirname(bundle_path), "stage-handoff-provenance.json")
  b_provenance_envelope_path <- sub("[.]json$", ".envelope.json", b_provenance_path)
  b_paths <- c(bundle_path, bundle_envelope_path, b_provenance_path, b_provenance_envelope_path)
  b_hashes_before <- cli$predecessor_hashes
  b_bundle <- jsonlite::read_json(bundle_path, simplifyVector = FALSE)
  b_profile <- jsonlite::read_json(b_provenance_path, simplifyVector = FALSE)
  testthat::expect_true(triomics_b07_resume_candidate(bundle_path)$required_evidence_complete)
  testthat::expect_true(triomics_b07_validate_provenance_artifacts(
    b_bundle, bundle_path, bundle_envelope_path
  ))
  testthat::expect_true(triomics_validate_stage_handoff_provenance(
    b_profile, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
  ))

  d_root <- file.path(output_root, "D")
  expected_files <- c(
    "candidate-nominations.json", "candidate-nominations.envelope.json",
    "integration-evidence.json", "integration-evidence.envelope.json",
    "integration-comparisons.json", "integration-comparisons.envelope.json",
    "evaluation-evidence.json", "evaluation-evidence.envelope.json",
    "stage-status.json", "stage-status.envelope.json",
    "handoff-manifest.json", "handoff-manifest.envelope.json",
    "stage-handoff-provenance.json", "stage-handoff-provenance.envelope.json"
  )
  has_handoff <- all(file.exists(file.path(d_root, expected_files)))
  testthat::expect_true(has_handoff)

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
  c_provenance_path <- file.path(output_root, "D", "stage-handoff-provenance.json")
  c_provenance_envelope_path <- file.path(output_root, "D", "stage-handoff-provenance.envelope.json")
  testthat::expect_true(file.exists(c_provenance_path))
  testthat::expect_true(file.exists(c_provenance_envelope_path))
  c_profile <- jsonlite::read_json(c_provenance_path, simplifyVector = FALSE)
  c_envelope <- jsonlite::read_json(c_provenance_envelope_path, simplifyVector = FALSE)
  testthat::expect_true(triomics_validate_envelope(c_provenance_envelope_path))
  testthat::expect_true(triomics_validate_stage_handoff_provenance(
    c_profile, c_envelope$artifact_id, paste0(c_envelope$artifact_id, "-envelope")
  ))
  predecessor_payloads <- Filter(function(entity) {
    identical(entity$scientific_role, "predecessor_payload")
  }, c_profile$identity$entities)
  predecessor_envelopes <- Filter(function(entity) {
    identical(entity$scientific_role, "predecessor_envelope")
  }, c_profile$identity$entities)
  testthat::expect_length(predecessor_payloads, 2L)
  testthat::expect_length(predecessor_envelopes, 2L)
  predecessor_hashes <- vapply(predecessor_payloads, `[[`, character(1L), "digest")
  testthat::expect_setequal(unname(predecessor_hashes), unname(b_hashes_before[c(1L, 3L)]))
  predecessor_envelope_hashes <- vapply(predecessor_envelopes, `[[`, character(1L), "digest")
  testthat::expect_setequal(unname(predecessor_envelope_hashes), unname(b_hashes_before[c(2L, 4L)]))
  for (predecessor in predecessor_payloads) {
    parent <- Filter(function(entity) {
      identical(as.character(entity$entity_id), as.character(predecessor$member_of_entity_id))
    }, predecessor_envelopes)
    testthat::expect_length(parent, 1L)
    testthat::expect_identical(predecessor$digest, parent[[1L]]$artifact_payload_sha256)
  }
  testthat::expect_identical(vapply(b_paths, triomics_sha256, character(1L)), b_hashes_before)
  testthat::expect_identical(c_profile$producer_stage, "C")
  testthat::expect_identical(unlist(c_profile$producer$method_ids, use.names = FALSE), "concat_pca")
  c_preparation_entities <- Filter(function(entity) {
    identical(entity$scientific_role, "preprocessing_state") &&
      identical(entity$generated_by_activity_id, "c06-stage-c-preparation")
  }, c_profile$identity$entities)
  testthat::expect_setequal(
    vapply(c_preparation_entities, `[[`, character(1), "block_id"),
    c("block_1", "block_2")
  )
  integration_bundle <- jsonlite::read_json(file.path(d_root, "integration-evidence.json"), simplifyVector = FALSE)
  testthat::expect_length(integration_bundle$integrations, 1L)
  integration <- integration_bundle$integrations[[1L]]
  testthat::expect_identical(integration$status, "complete")
  testthat::expect_identical(integration$backend, "concat_pca")
  c_integration_entity <- Filter(function(entity) {
    identical(entity$scientific_role, "integration_result") &&
      identical(entity$method_id, "concat_pca")
  }, c_profile$identity$entities)
  testthat::expect_length(c_integration_entity, 1L)
  testthat::expect_identical(
    c_integration_entity[[1L]]$digest,
    triomics_b07_prov_value_sha256(integration)
  )
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
  native_payload <- jsonlite::read_json(native_path, simplifyVector = FALSE)
  native_envelope <- jsonlite::read_json(native_envelope_path, simplifyVector = FALSE)
  c_runtime <- c_profile$producer$runtime_backend_versions
  native_runtime <- Filter(function(item) identical(item$component, "stats::prcomp"), c_runtime)
  testthat::expect_length(native_runtime, 1L)
  testthat::expect_identical(native_runtime[[1L]]$version, native_payload$native$native_version)
  testthat::expect_identical(integration$native_artifact_ref$artifact_id, native_payload$artifact_id)
  testthat::expect_identical(integration$native_artifact_ref$sha256, triomics_sha256(native_path))
  c_state_entity <- Filter(function(entity) {
    identical(entity$scientific_role, "integration_state") &&
      identical(entity$method_id, integration$backend)
  }, c_profile$identity$entities)
  expected_state_value <- list(
    integration_id = integration$integration_id,
    method_id = integration$backend,
    status = integration$status,
    reason_code = integration$reason_code %||% NULL,
    reason = integration$reason %||% NULL,
    native_artifact_ref = integration$native_artifact_ref %||% NULL,
    native_state = native_payload
  )
  testthat::expect_length(c_state_entity, 1L)
  testthat::expect_identical(
    c_state_entity[[1L]]$digest,
    triomics_b07_prov_value_sha256(expected_state_value)
  )
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
  for (block_id in names(integration$identity$matrix_sha256)) {
    testthat::expect_true(any(vapply(native_envelope$inputs, function(input) {
      identical(input$artifact_id, paste(integration$integration_id, block_id, sep = "::")) &&
        identical(input$sha256, integration$identity$matrix_sha256[[block_id]])
    }, logical(1L))))
  }
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
  testthat::expect_identical(triomics_sha256(file.path(output_root, "registries", "run.json")),
                             cli$b_run_record_sha256)
  c_run_record <- jsonlite::read_json(file.path(output_root, "registries", "run-C.json"), simplifyVector = FALSE)
  testthat::expect_identical(c_run_record$stage, "C")
  testthat::expect_identical(c_run_record$status, "complete")
})

testthat::test_that("C-06 Stage C rejects a fresh root without native B evidence", {
  output_root <- tempfile("triomics-c06-no-b-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  config_path <- triomics_test_resource("examples", "tickets", "C-06.yaml")

  error <- testthat::capture_error(triomics_execute_stage_c(config_path, output_root))

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_EXPORT_PREDECESSOR_MISSING")
  testthat::expect_false(file.exists(file.path(output_root, "D", "handoff-manifest.json")))
  testthat::expect_false(file.exists(file.path(output_root, "D", "stage-handoff-provenance.json")))
  testthat::expect_false(file.exists(file.path(output_root, "D", "stage-handoff-provenance.envelope.json")))
})

testthat::test_that("Stage C composes paired and trajectory units from native B resolution", {
  for (kind in c("paired", "trajectory")) {
    fixture <- c06_shared_repeated_fixture(kind)
    output_root <- tempfile(paste0("c06-shared-", kind, "-output-"), tmpdir = "/private/tmp")
    on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(unlink(output_root, recursive = TRUE, force = TRUE), add = TRUE)

    c_run <- c06_shared_cli_run(output_root, config_path = fixture$config, prepare_b = TRUE)

    testthat::expect_identical(c_run$status, 0L, info = paste(c_run$output, collapse = "\n"))
    if (c_run$status == 0L) {
      testthat::expect_true(file.exists(file.path(output_root, "C", "integration-results.json")))
      testthat::expect_true(file.exists(file.path(output_root, "D", "stage-handoff-provenance.json")))
      endpoint_path <- file.path(output_root, "C", "score-endpoints", "integration-01.json")
      endpoint <- jsonlite::read_json(endpoint_path, simplifyVector = FALSE)
      testthat::expect_true(length(endpoint$endpoints) > 0L)
      unit_map <- endpoint$endpoints[[1L]]$unit_map
      testthat::expect_true(length(unit_map) > 0L)
      if (identical(kind, "paired")) {
        testthat::expect_true(all(vapply(unit_map, function(record) {
          identical(record$representation_id, "delta")
        }, logical(1L))))
        testthat::expect_true(all(vapply(unit_map, function(record) {
          weights <- record$visit_weights
          is.list(weights) && identical(names(weights), c("baseline", "followup")) &&
            identical(as.numeric(unlist(weights)), c(-1, 1))
        }, logical(1L))))
      } else {
        testthat::expect_true(all(vapply(unit_map, function(record) {
          identical(record$representation_id, "subject_centered")
        }, logical(1L))))
        testthat::expect_true(all(vapply(unit_map, function(record) {
          value <- record$subject_id
          is.character(value) && length(value) == 1L && nzchar(value)
        }, logical(1L))))
        testthat::expect_true(all(vapply(unit_map, function(record) {
          value <- record$time
          is.numeric(value) && length(value) == 1L && is.finite(value)
        }, logical(1L))))
        testthat::expect_true(all(vapply(unit_map, function(record) {
          value <- record$time_unit
          is.character(value) && length(value) == 1L && nzchar(value)
        }, logical(1L))))
        testthat::expect_true(all(vapply(unit_map, function(record) length(record$time_origin) == 1L,
                                         logical(1L))))
      }
      d_run <- c06_shared_cli_run(
        output_root, stage = "D", config_path = fixture$config, prepare_b = FALSE
      )
      testthat::expect_identical(d_run$status, 0L, info = paste(d_run$output, collapse = "\n"))
    }
  }
})

testthat::test_that("native paired C retains per-block complete-pair exclusions through D", {
  fixture <- c06_shared_paired_attrition_fixture()
  output_root <- tempfile("c06-paired-attrition-output-", tmpdir = "/private/tmp")
  on.exit(unlink(c(fixture$root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  b_run <- c06_shared_cli_run(output_root, stage = "B", config_path = fixture$config,
                              prepare_b = FALSE)
  testthat::expect_identical(b_run$status, 0L, info = paste(b_run$output, collapse = "\n"))
  if (b_run$status != 0L) return(invisible(NULL))
  c_run <- c06_shared_cli_run(output_root, stage = "C", config_path = fixture$config,
                              prepare_b = FALSE)
  testthat::expect_identical(c_run$status, 0L, info = paste(c_run$output, collapse = "\n"))
  if (c_run$status != 0L) return(invisible(NULL))
  d_run <- c06_shared_cli_run(output_root, stage = "D", config_path = fixture$config,
                              prepare_b = FALSE)
  testthat::expect_identical(d_run$status, 0L, info = paste(d_run$output, collapse = "\n"))
  if (d_run$status != 0L) return(invisible(NULL))

  endpoint <- jsonlite::read_json(
    file.path(output_root, "C", "score-endpoints", "integration-01.json"),
    simplifyVector = FALSE
  )$endpoints[[1L]]
  unit_ids <- vapply(endpoint$unit_map, `[[`, character(1L), "unit_id")
  unit_map <- stats::setNames(endpoint$unit_map, unit_ids)
  excluded <- fixture$excluded_subjects
  testthat::expect_setequal(unit_ids, unique(as.character(
    vapply(endpoint$unit_map, `[[`, character(1L), "subject_id")
  )))
  for (block_id in names(excluded)) {
    subject <- excluded[[block_id]]
    record <- unit_map[[subject]]
    testthat::expect_identical(record$eligibility, "excluded")
    testthat::expect_identical(record$block_eligibility[[block_id]], "excluded")
    testthat::expect_match(record$exclusion_reason, paste0("ineligible_in_blocks:", block_id))
    testthat::expect_length(record$block_source_sample_ids[[block_id]], 1L)
    other_block <- setdiff(c("block_1", "block_2"), block_id)[[1L]]
    testthat::expect_identical(record$block_eligibility[[other_block]], "retained")
    testthat::expect_length(record$block_source_sample_ids[[other_block]], 2L)
    subject_samples <- as.character(fixture$metadata$sample_id[
      fixture$metadata$subject == subject
    ])
    testthat::expect_setequal(record$source_sample_ids, subject_samples)
  }
  expected_training <- setdiff(unit_ids, unname(excluded))
  testthat::expect_setequal(unlist(endpoint$training_unit_ids, use.names = FALSE), expected_training)
  endpoint_samples <- endpoint$samples
  sample_units <- vapply(endpoint_samples, `[[`, character(1L), "unit_id")
  testthat::expect_false(any(sample_units %in% unname(excluded)))

  d_evidence <- jsonlite::read_json(file.path(output_root, "D", "integration-evidence.json"),
                                    simplifyVector = FALSE)$integrations[[1L]]
  testthat::expect_setequal(unlist(d_evidence$eligibility$training_unit_ids, use.names = FALSE),
                            expected_training)
})

testthat::test_that("native trajectory C retains an absent-all B exclusion with unavailable time", {
  fixture <- c06_shared_trajectory_missing_time_fixture()
  output_root <- tempfile("c06-trajectory-missing-time-output-", tmpdir = "/private/tmp")
  on.exit(unlink(c(fixture$root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  b_run <- c06_shared_cli_run(output_root, stage = "B", config_path = fixture$config,
                              prepare_b = FALSE)
  testthat::expect_identical(b_run$status, 0L, info = paste(b_run$output, collapse = "\n"))
  if (b_run$status != 0L) return(invisible(NULL))
  c_run <- c06_shared_cli_run(output_root, stage = "C", config_path = fixture$config,
                              prepare_b = FALSE)
  testthat::expect_identical(c_run$status, 0L, info = paste(c_run$output, collapse = "\n"))
  if (c_run$status != 0L) return(invisible(NULL))

  endpoint_path <- file.path(output_root, "C", "score-endpoints", "integration-01.json")
  endpoint_bundle <- triomics_c08_cli_validate_endpoint_readback(endpoint_path)
  testthat::expect_no_error(triomics_c08_validate_endpoint_bundle(endpoint_bundle))
  endpoint <- endpoint_bundle$endpoints[[1L]]
  unit_map <- endpoint$unit_map
  unit_ids <- vapply(unit_map, `[[`, character(1L), "unit_id")
  excluded <- unit_map[[match(fixture$excluded_sample, unit_ids)]]
  testthat::expect_identical(excluded$eligibility, "excluded")
  testthat::expect_identical(excluded$exclusion_reason, "absent_from_all_analysis_blocks")
  testthat::expect_identical(unlist(excluded$source_sample_ids, use.names = FALSE),
                             fixture$excluded_sample)
  testthat::expect_length(excluded$represented_blocks, 0L)
  testthat::expect_null(excluded$time)
  testthat::expect_false(fixture$excluded_sample %in% unlist(endpoint$training_unit_ids,
                                                              use.names = FALSE))
  testthat::expect_false(any(vapply(endpoint$samples, function(sample) {
    identical(sample$unit_id, fixture$excluded_sample)
  }, logical(1L))))

  d_run <- c06_shared_cli_run(output_root, stage = "D", config_path = fixture$config,
                              prepare_b = FALSE)
  testthat::expect_identical(d_run$status, 0L, info = paste(d_run$output, collapse = "\n"))
  if (d_run$status == 0L) {
    d_evidence <- jsonlite::read_json(file.path(output_root, "D", "integration-evidence.json"),
                                      simplifyVector = FALSE)$integrations[[1L]]
    testthat::expect_false(fixture$excluded_sample %in% unlist(
      d_evidence$eligibility$training_unit_ids, use.names = FALSE
    ))
  }
})

testthat::test_that("paired C unit composition joins per-block maps by subject and records attrition", {
  sample_ids <- c("s1-baseline", "s1-followup", "s2-baseline", "s2-followup")
  metadata <- data.frame(
    sample_id = sample_ids,
    subject = c("s1", "s1", "s2", "s2"),
    visit = c("baseline", "followup", "baseline", "followup"),
    stringsAsFactors = FALSE
  )
  weights <- stats::setNames(c(-1, 1), c("baseline", "followup"))
  make_record <- function(subject, eligibility = "retained") {
    list(
      unit_id = subject, unit_type = "subject_delta", representation_id = "delta",
      eligibility = eligibility,
      exclusion_reason = if (eligibility == "retained") NULL else "missing_visit",
      source_sample_ids = c(paste0(subject, "-baseline"), paste0(subject, "-followup")),
      subject_id = subject, visit_weights = weights,
      study_id = "paired-map-study", analysis_id = "paired-map-analysis",
      preparation_context_id = "paired-map-prep"
    )
  }
  matrix_for <- function(ids) {
    matrix(seq_len(length(ids)), nrow = 1L, dimnames = list("feature", ids))
  }
  block <- function(id, ids, records) {
    assay <- matrix_for(ids)
    list(
      block_id = id, assay = assay, sample_ids = ids, all_subjects = c("s1", "s2"),
      signed_delta_map = assay, centered = FALSE,
      decomposition = "target_minus_reference_once", decomposition_count = 1L,
      unit_map = records
    )
  }
  resolved <- list(
    model_plan = list(reference_visit = "baseline", target_visit = "followup"),
    blocks = list(
      block_a = block("block_a", c("s1", "s2"), list(make_record("s2"), make_record("s1"))),
      block_b = block("block_b", c("s1"), list(make_record("s1"), make_record("s2", "excluded")))
    )
  )
  ingested <- list(sample_metadata = metadata, study_id = "paired-map-study")
  analysis <- list(
    analysis_id = "paired-map-analysis", preparation_context_id = "paired-map-prep",
    design = list(representation = "delta", subject_id = "subject", visit_column = "visit")
  )

  composed <- triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, c("block_a", "block_b"),
    source_unit_map = resolved$blocks$block_a$unit_map
  )

  testthat::expect_identical(composed$unit_ids, c("s1", "s2"))
  testthat::expect_identical(composed$training_unit_ids, "s1")
  testthat::expect_identical(composed$unit_map$s2$eligibility, "excluded")
  testthat::expect_match(composed$unit_map$s2$exclusion_reason, "block_b")
  testthat::expect_named(composed$unit_map$s2$block_eligibility, c("block_a", "block_b"))
  testthat::expect_identical(composed$unit_map$s2$block_eligibility$block_b, "excluded")
  testthat::expect_setequal(composed$unit_map$s2$source_sample_ids,
                            c("s2-baseline", "s2-followup"))

  shuffled_sign_names <- resolved
  shuffled_sign_names$blocks$block_b$unit_map[[1L]]$visit_weights <-
    stats::setNames(c(1, -1), c("followup", "baseline"))
  testthat::expect_silent(triomics_c06_cli_compose_unit_map(
    shuffled_sign_names, ingested, analysis, c("block_a", "block_b"),
    source_unit_map = resolved$blocks$block_a$unit_map
  ))

  wrong_sign <- resolved
  wrong_sign$blocks$block_b$unit_map[[2L]]$visit_weights <-
    stats::setNames(c(1, -1), c("baseline", "followup"))
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    wrong_sign, ingested, analysis, c("block_a", "block_b"),
    source_unit_map = resolved$blocks$block_a$unit_map
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  wrong_source <- resolved
  wrong_source$blocks$block_b$unit_map[[2L]]$source_sample_ids <-
    c("s1-baseline", "s1-followup")
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    wrong_source, ingested, analysis, c("block_a", "block_b"),
    source_unit_map = resolved$blocks$block_a$unit_map
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  for (field in c("unit_type", "representation_id", "study_id", "analysis_id",
                  "preparation_context_id")) {
    foreign_identity <- resolved
    foreign_identity$blocks$block_b$unit_map[[1L]][[field]] <- "foreign-context"
    error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
      foreign_identity, ingested, analysis, c("block_a", "block_b"),
      source_unit_map = resolved$blocks$block_a$unit_map
    ))
    testthat::expect_s3_class(error, "triomics_error")
    testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID", info = field)
  }
})

testthat::test_that("trajectory C units retain source-time identity without inventing visits", {
  metadata <- data.frame(
    sample_id = c("sample-a", "sample-b", "sample-excluded"),
    subject = c("subject-a", "subject-a", "subject-b"),
    time = c(0, 2, 0), stringsAsFactors = FALSE
  )
  matrix_a <- matrix(c(1, 3), nrow = 1L, dimnames = list("feature", c("sample-b", "sample-a")))
  matrix_b <- matrix(c(4, 2), nrow = 1L, dimnames = list("feature", c("sample-a", "sample-b")))
  resolved <- list(blocks = list(
    block_a = list(assay = matrix_a, sample_ids = colnames(matrix_a)),
    block_b = list(assay = matrix_b, sample_ids = colnames(matrix_b))
  ))
  source_map <- data.frame(
    unit_id = c("sample-a", "sample-b", "sample-excluded"),
    representation_id = rep("subject_centered", 3L),
    subject_id = c("subject-a", "subject-a", "subject-b"),
    visit_id = rep(NA_character_, 3L),
    condition = c("0", "2", "0"), eligible = c(TRUE, TRUE, FALSE),
    time = c(0, 2, 0), time_unit = rep("day", 3L), time_origin = rep("0", 3L),
    exclusion_reason = c(NA_character_, NA_character_, "absent_from_all_analysis_blocks"),
    source_sample_ids = I(list("sample-a", "sample-b", "sample-excluded")),
    represented_blocks = I(list(c("block_a", "block_b"), c("block_a", "block_b"), character())),
    stringsAsFactors = FALSE
  )
  ingested <- list(sample_metadata = metadata, study_id = "trajectory-map-study")
  analysis <- list(
    analysis_id = "trajectory-map-analysis", preparation_context_id = "trajectory-map-prep",
    design = list(
      representation = "subject_centered", subject_id = "subject", condition = "time",
      time_column = "time", time_unit = "day", time_origin = "0", visit_column = NULL
    )
  )
  before <- resolved$blocks

  composed <- triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, c("block_a", "block_b"), source_map
  )

  testthat::expect_identical(composed$unit_ids, c("sample-a", "sample-b", "sample-excluded"))
  testthat::expect_identical(composed$training_unit_ids, c("sample-a", "sample-b"))
  testthat::expect_identical(composed$unit_map[["sample-excluded"]]$eligibility, "excluded")
  testthat::expect_identical(composed$unit_map[["sample-excluded"]]$exclusion_reason,
                             "absent_from_all_analysis_blocks")
  testthat::expect_identical(composed$unit_map[["sample-excluded"]]$source_sample_ids,
                             "sample-excluded")
  testthat::expect_identical(composed$unit_map[["sample-excluded"]]$represented_blocks,
                             character())
  testthat::expect_true(all(vapply(composed$unit_map, function(record) {
    identical(record$unit_type, "sample") && is.null(record$visit_id)
  }, logical(1L))))
  testthat::expect_identical(unname(vapply(composed$unit_map[1:2], `[[`, numeric(1L), "time")), c(0, 2))
  testthat::expect_identical(resolved$blocks, before)

  declared_visit <- analysis
  declared_visit$design$visit_column <- "visit"
  ingested$sample_metadata$visit <- c("v0", "v2", "v0")
  source_map$visit_id <- c("v0", "v2", "v0")
  with_visit <- triomics_c06_cli_compose_unit_map(
    resolved, ingested, declared_visit, c("block_a", "block_b"), source_map
  )
  testthat::expect_true(all(vapply(with_visit$unit_map, function(record) {
    identical(record$unit_type, "visit_level") && nzchar(record$visit_id)
  }, logical(1L))))

  wrong_visit <- source_map
  wrong_visit$visit_id <- c("wrong-v0", "v2", "v0")
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    resolved, ingested, declared_visit, c("block_a", "block_b"), wrong_visit
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  wrong_time <- source_map
  wrong_time$time[[1L]] <- 3
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, c("block_a", "block_b"), wrong_time
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  wrong_time_unit <- source_map
  wrong_time_unit$time_unit[[1L]] <- "hour"
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, c("block_a", "block_b"), wrong_time_unit
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  wrong_origin <- source_map
  wrong_origin$time_origin[[1L]] <- "1"
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, c("block_a", "block_b"), wrong_origin
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  wrong_subject <- source_map
  wrong_subject$subject_id[[1L]] <- "other-subject"
  error <- testthat::capture_error(triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, c("block_a", "block_b"), wrong_subject
  ))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")
})

testthat::test_that("C-06 validates same-root B identities and cleans failed D publication", {
  output_root <- tempfile("triomics-c06-provenance-identity-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  config_path <- triomics_test_resource("examples", "tickets", "C-06.yaml")
  b_run <- c06_shared_cli_run(output_root, stage = "B", config_path = config_path,
                              prepare_b = FALSE)
  testthat::expect_identical(b_run$status, 0L, info = paste(b_run$output, collapse = "\n"))
  b_paths <- triomics_c06_cli_native_b_predecessors(output_root)
  b_hashes <- vapply(b_paths, triomics_sha256, character(1L))
  inputs <- c06_shared_native_input_identity(output_root, config_path)
  handoff_identity <- list(
    study_id = inputs$ingested$study_id,
    analysis_id = inputs$analysis$analysis_id,
    preparation_context_id = inputs$analysis$preparation_context_id,
    representation_id = inputs$analysis$design$representation,
    integration_evidence = list()
  )
  verified <- triomics_c06_validate_b_predecessors(
    output_root, handoff_identity, attr(inputs$config, "config_sha256"),
    b_paths, inputs$identity
  )
  testthat::expect_identical(vapply(verified$paths, triomics_sha256, character(1L)), b_hashes)

  changed_matrix <- inputs$identity
  changed_matrix$blocks$block_1$matrix[1L, 1L] <-
    changed_matrix$blocks$block_1$matrix[1L, 1L] + 1
  matrix_error <- testthat::capture_error(triomics_c06_validate_b_predecessors(
    output_root, handoff_identity, attr(inputs$config, "config_sha256"),
    b_paths, changed_matrix
  ))
  testthat::expect_identical(matrix_error$code, "C_EXPORT_IDENTITY_MISMATCH")

  changed_metadata <- inputs$identity
  changed_metadata$normalized_metadata$sample_metadata$score[[1L]] <-
    changed_metadata$normalized_metadata$sample_metadata$score[[1L]] + 1
  metadata_error <- testthat::capture_error(triomics_c06_validate_b_predecessors(
    output_root, handoff_identity, attr(inputs$config, "config_sha256"),
    b_paths, changed_metadata
  ))
  testthat::expect_identical(metadata_error$code, "C_EXPORT_IDENTITY_MISMATCH")

  controls <- triomics_c06_cli_unrun_controls(list(), inputs$resolved, inputs$analysis, list())
  unavailable_runs <- list(
    list(integration_id = "unavailable-a", method = "concat_pca", required = TRUE,
         status = "unavailable", reason_code = "C06_INTEGRATION_RESULT_UNAVAILABLE",
         reason = "The native result is unavailable for this software-only writer cleanup check."),
    list(integration_id = "unavailable-b", method = "concat_pca", required = TRUE,
         status = "unavailable", reason_code = "C06_INTEGRATION_RESULT_UNAVAILABLE",
         reason = "The native result is unavailable for this software-only writer cleanup check.")
  )
  handoff <- triomics_c06_build_handoff(
    primary_nominations = NULL,
    integration_runs = unavailable_runs,
    evaluation_evidence = list(controls = controls),
    backend_statuses = list(concat_pca = list(status = "unavailable")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional",
                               packet_id = "C-09")
  )
  handoff$study_id <- inputs$ingested$study_id
  handoff$analysis_id <- inputs$analysis$analysis_id
  cleanup_error <- testthat::capture_error(triomics_c06_write_handoff(
    handoff, output_root,
    config_sha256 = attr(inputs$config, "config_sha256"),
    producer_commit = triomics_b07_git_ref(getwd()),
    privacy_class = "synthetic",
    b_predecessor_paths = b_paths,
    c_input_identity = inputs$identity
  ))
  testthat::expect_identical(cleanup_error$code, "C_EXPORT_PROVENANCE_INVALID")
  testthat::expect_false(dir.exists(file.path(output_root, "D")))
  testthat::expect_identical(vapply(b_paths, triomics_sha256, character(1L)), b_hashes)
})

testthat::test_that("native C-06 writer propagates sensitive privacy and binds D lineage", {
  original_config <- triomics_test_resource("examples", "tickets", "C-06.yaml")
  config_lines <- readLines(original_config, warn = FALSE)
  matched <- grepl("^data_class: synthetic$", config_lines)
  testthat::expect_identical(sum(matched), 1L)
  config_lines[matched] <- "data_class: real"
  sensitive_config <- tempfile("c06-sensitive-", tmpdir = tempdir(), fileext = ".yaml")
  data_root <- normalizePath(
    file.path(dirname(original_config), "..", "data"),
    winslash = "/", mustWork = TRUE
  )
  config_lines <- sub(
    "^([[:space:]]*file: )[.][.]/data/",
    paste0("\\1", data_root, "/"),
    config_lines
  )
  writeLines(config_lines, sensitive_config, useBytes = TRUE)
  output_root <- tempfile("triomics-c06-sensitive-cli-")
  on.exit(unlink(c(sensitive_config, output_root), recursive = TRUE), add = TRUE)

  cli <- c06_shared_cli_run(output_root, config_path = sensitive_config)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))
  config <- triomics_read_config(sensitive_config)
  analysis <- config$analyses[[1L]]
  d_root <- file.path(output_root, "D")
  envelope_paths <- list.files(d_root, pattern = "[.]envelope[.]json$", full.names = TRUE)
  testthat::expect_length(envelope_paths, 7L)
  envelopes <- lapply(envelope_paths, jsonlite::read_json, simplifyVector = FALSE)
  testthat::expect_true(all(vapply(envelopes, function(envelope) {
    identical(envelope$privacy_class, "sensitive") &&
      identical(envelope$analysis_id, analysis$analysis_id) &&
      identical(envelope$preparation_context_id, analysis$preparation_context_id) &&
      identical(envelope$representation_id, analysis$design$representation)
  }, logical(1L))))

  candidate_path <- file.path(d_root, "candidate-nominations.json")
  candidate_envelope_path <- file.path(d_root, "candidate-nominations.envelope.json")
  candidate_envelope <- jsonlite::read_json(candidate_envelope_path, simplifyVector = FALSE)
  nomination_path <- list.files(file.path(output_root, "B", "handoffs"),
                                pattern = "^candidate-nominations[.]json$",
                                recursive = TRUE, full.names = TRUE)
  testthat::expect_length(nomination_path, 1L)
  integration <- jsonlite::read_json(file.path(d_root, "integration-evidence.json"),
                                     simplifyVector = FALSE)$integrations[[1L]]
  evaluation_path <- file.path(d_root, "evaluation-evidence.json")
  contains_input <- function(artifact_id, sha256) any(vapply(candidate_envelope$inputs, function(input) {
    identical(input$artifact_id, artifact_id) && identical(input$sha256, sha256)
  }, logical(1L)))
  testthat::expect_true(contains_input("b-t07-candidate-nominations", triomics_sha256(nomination_path)))
  testthat::expect_true(contains_input(integration$native_artifact_ref$artifact_id,
                                       integration$native_artifact_ref$sha256))
  testthat::expect_true(contains_input("c06-evaluation-evidence", triomics_sha256(evaluation_path)))
  testthat::expect_true(triomics_validate_envelope(candidate_envelope_path))
  testthat::expect_identical(triomics_sha256(candidate_path), candidate_envelope$payload_sha256)
})

testthat::test_that("Stage C preserves the verified native B primary nomination source", {
  output_root <- tempfile("triomics-c06-no-primary-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)

  cli <- c06_shared_cli_run(output_root)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))
  payload <- jsonlite::read_json(
    file.path(output_root, "D", "candidate-nominations.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(payload$branches$primary_statistics$status, "complete")
  testthat::expect_true(is.list(payload$branches$primary_statistics$source_artifact_ref))
  testthat::expect_identical(
    payload$branches$primary_statistics$source_artifact_ref$artifact_id,
    "b-t07-candidate-nominations"
  )
})

testthat::test_that("nested C-02 Stage-C evaluation retains verified units for its unrun D controls", {
  output_root <- tempfile("triomics-c02-nested-cli-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  config <- triomics_test_resource("examples", "tickets", "C-02.yaml")

  cli <- c06_shared_cli_run(output_root, config_path = config)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))
  b_bundles <- list.files(file.path(output_root, "B", "handoffs"), pattern = "^bundle[.]json$",
                          recursive = TRUE, full.names = TRUE)
  testthat::expect_length(b_bundles, 1L)
  b_profile_path <- file.path(dirname(b_bundles[[1L]]), "stage-handoff-provenance.json")
  b_profile <- jsonlite::read_json(b_profile_path, simplifyVector = FALSE)
  testthat::expect_true(triomics_validate_stage_handoff_provenance(
    b_profile, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
  ))
  testthat::expect_false(any(grepl("C_EVAL_EVIDENCE_INVALID", cli$output, fixed = TRUE)))
  has_d_evidence <- file.exists(file.path(output_root, "D", "evaluation-evidence.json"))
  testthat::expect_true(has_d_evidence)
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
    "handoff-manifest.json", "handoff-manifest.envelope.json",
    "stage-handoff-provenance.json", "stage-handoff-provenance.envelope.json"
  )
  has_complete_handoff <- all(file.exists(file.path(d_root, expected_files)))
  testthat::expect_true(has_complete_handoff)
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
  testthat::expect_identical(candidates$branches$primary_statistics$status, "complete")
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
  testthat::expect_identical(triomics_sha256(file.path(output_root, "registries", "run.json")),
                             cli$b_run_record_sha256)
  c_run_record <- jsonlite::read_json(
    file.path(output_root, "registries", "run-C.json"), simplifyVector = FALSE
  )
  testthat::expect_identical(c_run_record$status, "complete")
  nested <- c_integration$integrations[[1L]]$evaluation
  testthat::expect_identical(nested$n_predicted, 12L)
  testthat::expect_identical(nested$total_denominator, 12L)
  testthat::expect_identical(nested$failed_prediction_count, 0L)
  testthat::expect_identical(c_integration$study_id, config_values$study_id)
  testthat::expect_identical(c_integration$analysis_id, analysis$analysis_id)
  c_provenance_path <- file.path(output_root, "D", "stage-handoff-provenance.json")
  c_provenance_envelope_path <- file.path(output_root, "D", "stage-handoff-provenance.envelope.json")
  testthat::expect_true(file.exists(c_provenance_path))
  testthat::expect_true(file.exists(c_provenance_envelope_path))
  c_profile <- jsonlite::read_json(c_provenance_path, simplifyVector = FALSE)
  c_envelope <- jsonlite::read_json(c_provenance_envelope_path, simplifyVector = FALSE)
  testthat::expect_true(triomics_validate_envelope(c_provenance_envelope_path))
  testthat::expect_true(triomics_validate_stage_handoff_provenance(
    c_profile, c_envelope$artifact_id, paste0(c_envelope$artifact_id, "-envelope")
  ))
  testthat::expect_identical(c_profile$producer_stage, "C")
  nested_results <- Filter(function(entity) identical(entity$scientific_role, "integration_result"),
                           c_profile$identity$entities)
  nested_states <- Filter(function(entity) identical(entity$scientific_role, "integration_state"),
                          c_profile$identity$entities)
  testthat::expect_length(nested_results, 1L)
  testthat::expect_length(nested_states, 1L)
  testthat::expect_identical(nested_states[[1L]]$member_of_entity_id, nested_results[[1L]]$entity_id)
  testthat::expect_true(triomics_sha256_text(nested_states[[1L]]$digest))
  nested_integration <- jsonlite::read_json(
    file.path(output_root, "D", "integration-evidence.json"), simplifyVector = FALSE
  )$integrations[[1L]]
  testthat::expect_identical(nested_integration$status, "unavailable")
  testthat::expect_length(c_profile$identity$declared_routes, 0L)
  b_current_hashes <- c(
    b_bundles[[1L]], sub("[.]json$", ".envelope.json", b_bundles[[1L]]),
    b_profile_path, sub("[.]json$", ".envelope.json", b_profile_path)
  )
  testthat::expect_identical(vapply(b_current_hashes, triomics_sha256, character(1L)),
                             cli$predecessor_hashes)
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
