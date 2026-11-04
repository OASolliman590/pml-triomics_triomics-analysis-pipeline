triomics_report_config <- function(config_path) {
  if (!triomics_d_nonempty_string(config_path)) {
    triomics_report_abort("D_REPORT_REQUEST_INVALID", "The report needs the producing configuration file.",
                          "config")
  }
  config_path <- tryCatch(
    triomics_d_secure_path(config_path, "D_REPORT_PATH_UNSAFE",
                           "The report configuration path is unsafe.", "config"),
    error = function(error) triomics_report_abort("D_REPORT_REQUEST_INVALID",
                                                   "The report configuration file cannot be read safely.",
                                                   "config")
  )
  if (!file.exists(config_path) || dir.exists(config_path) || triomics_is_symlink(config_path)) {
    triomics_report_abort("D_REPORT_REQUIRED_MISSING", "The producing configuration file is unavailable.",
                          "config")
  }
  config <- tryCatch(triomics_read_config(config_path), error = function(error) NULL)
  config_sha256 <- attr(config, "config_sha256")
  if (!is.list(config) || !triomics_sha256_text(config_sha256) ||
      !identical(triomics_sha256(config_path), config_sha256)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The report configuration identity is invalid or changed.",
                          "config")
  }
  analyses <- config$analyses
  if (!is.list(analyses) || length(analyses) != 1L ||
      !triomics_d_nonempty_string(analyses[[1L]]$analysis_id)) {
    triomics_report_abort("D_REPORT_REQUEST_INVALID", "The report requires one declared analysis identity.",
                          "analyses")
  }
  audience <- as.character(config$execution$audience %||% "")
  privacy_class <- if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  if (!audience %in% c("synthetic_demo", "private_research", "approved_aggregate") ||
      (identical(audience, "synthetic_demo") && !identical(privacy_class, "synthetic"))) {
    triomics_report_abort("D_REPORT_REQUEST_INVALID", "The configured audience is not valid for this study class.",
                          "execution.audience")
  }
  if (identical(audience, "approved_aggregate") && isTRUE(config$clinical$enabled)) {
    triomics_report_abort(
      "D_REPORT_DISCLOSURE_UNQUALIFIED",
      "Clinical aggregate release is unavailable without a qualified minimum-cell and complementary-suppression policy.",
      "execution.disclosure_policy_ref",
      "Use a private local research report or wait for an institution-approved disclosure policy implementation."
    )
  }
  if (!is.list(config$blocks) || !length(config$blocks)) {
    triomics_report_abort("D_REPORT_REQUEST_INVALID", "The report configuration has no measurement blocks.",
                          "blocks")
  }
  analysis <- analyses[[1L]]
  requests <- config$annotation$requests %||% list()
  integrations <- analysis$integration %||% list()
  objectives <- config$clinical$objectives %||% list()
  list(
    path = config_path,
    config = config,
    config_sha256 = as.character(config_sha256),
    study_id = triomics_report_identifier_value(config$study_id, "study_id"),
    analysis_id = triomics_report_identifier_value(analysis$analysis_id, "analysis_id"),
    privacy_class = privacy_class,
    audience = audience,
    source_count = length(config$sources %||% list()),
    clinical_enabled = isTRUE(config$clinical$enabled),
    clinical_required = isTRUE(config$clinical$enabled),
    target_enabled = isTRUE(config$target_investigation$enabled),
    target_required = isTRUE(config$target_investigation$enabled) &&
      isTRUE(config$target_investigation$required),
    route_requirements = setNames(lapply(requests, function(request) isTRUE(request$required)),
                                  vapply(requests, function(request) {
                                    triomics_report_identifier(request, "route")
                                  }, character(1))),
    integration_requirements = setNames(lapply(integrations, function(integration) isTRUE(integration$required)),
                                        vapply(integrations, function(integration) {
                                          triomics_report_identifier(integration, "integration_id")
                                        }, character(1))),
    blocks = lapply(seq_along(config$blocks), function(index) {
      block <- config$blocks[[index]]
      list(
      block_id = triomics_report_identifier(block, "block_id"),
      name = paste("Block", index),
      profile = triomics_report_identifier_value(as.character(block$profile %||% "not_reported"), "blocks.profile"),
      quantity = triomics_report_identifier_value(as.character(block$measurement$quantity %||% "not_reported"), "blocks.measurement.quantity"),
      scale = triomics_report_identifier_value(as.character(block$measurement$scale %||% "not_reported"), "blocks.measurement.scale"),
      units = triomics_report_unit(as.character(block$measurement$units %||% "not_reported"), "blocks.measurement.units"),
      preparation_context_id = triomics_report_identifier_value(as.character(
        block$preprocessing$context_id %||% analysis$preparation_context_id %||% "not_reported"),
        "blocks.preparation_context_id"),
      transform = triomics_report_identifier_value(as.character(block$preprocessing$transform$method %||% "not_reported"), "blocks.transform"),
      normalization = triomics_report_identifier_value(as.character(block$preprocessing$normalisation$method %||% "not_reported"), "blocks.normalization"),
      imputation = triomics_report_identifier_value(as.character(block$preprocessing$imputation$method %||% "not_reported"), "blocks.imputation")
    )
    }),
    analysis = list(
      preparation_context_id = triomics_report_identifier_value(as.character(analysis$preparation_context_id %||% "not_reported"), "analysis.preparation_context_id"),
      design_kind = triomics_report_identifier_value(as.character(analysis$design$kind %||% "not_reported"), "analysis.design.kind"),
      condition_type = triomics_report_identifier_value(as.character(analysis$design$condition_type %||% "not_reported"), "analysis.design.condition_type"),
      representation = triomics_report_identifier_value(as.character(analysis$design$representation %||% "not_reported"), "analysis.design.representation"),
      statistics_method = triomics_report_identifier_value(as.character(analysis$statistics$method %||% "not_reported"), "analysis.statistics.method"),
      contrasts = lapply(analysis$statistics$contrasts %||% list(), function(contrast) list(
        contrast_id = triomics_report_identifier_value(as.character(contrast$contrast_id %||% "not_reported"), "analysis.statistics.contrast_id"),
        effect_unit = triomics_report_unit(as.character(analysis$statistics$effect_unit %||% "not_reported"), "analysis.statistics.effect_unit")
      ))
    ),
    objective_count = length(objectives)
  )
}

triomics_report_read_json <- function(path, key) {
  value <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(value) || is.null(names(value))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A verified report artifact is not readable JSON.", key)
  }
  value
}

triomics_report_artifact <- function(root, relative_path, artifact_type, artifact_id,
                                     study_id, config_sha256, analysis_id = NULL,
                                     parse_payload = TRUE) {
  envelope_relative <- sub("[.]json$", ".envelope.json", relative_path)
  payload_path <- triomics_report_safe_child(root, relative_path, relative_path, must_exist = FALSE)
  envelope_path <- triomics_report_safe_child(root, envelope_relative, envelope_relative,
                                               must_exist = FALSE)
  if (!file.exists(payload_path) && !file.exists(envelope_path)) {
    triomics_report_abort("D_REPORT_REQUIRED_MISSING", "A required versioned producer artifact is missing.",
                          relative_path)
  }
  if (!file.exists(payload_path) || dir.exists(payload_path) || triomics_is_symlink(payload_path) ||
      !file.exists(envelope_path) || dir.exists(envelope_path) || triomics_is_symlink(envelope_path) ||
      !triomics_validate_envelope(envelope_path)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A producer payload or envelope is incomplete or tampered.",
                          relative_path)
  }
  envelope <- triomics_report_read_json(envelope_path, envelope_relative)
  if (!identical(as.character(envelope$artifact_type), artifact_type) ||
      !identical(as.character(envelope$artifact_id), artifact_id) ||
      !identical(as.character(envelope$study_id), study_id) ||
      !identical(as.character(envelope$config_sha256), config_sha256) ||
      !envelope$status %in% triomics_envelope_statuses() ||
      !identical(as.character(envelope$payload_file), basename(relative_path)) ||
      !identical(as.character(envelope$payload_sha256), triomics_sha256(payload_path))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A producer envelope does not match the report run.",
                          relative_path)
  }
  payload <- if (isTRUE(parse_payload)) triomics_report_read_json(payload_path, relative_path) else NULL
  if (isTRUE(parse_payload) &&
      (!identical(as.character(payload$schema_version), "2.0") ||
       !identical(as.character(payload$study_id), study_id) ||
       (!is.null(payload$status) && !identical(as.character(payload$status), as.character(envelope$status))) ||
       (!is.null(payload$analysis_id) && !identical(as.character(payload$analysis_id), analysis_id)))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A producer payload identity differs from its envelope.",
                          relative_path)
  }
  if (!is.null(analysis_id) && !is.null(envelope$analysis_id) &&
      !identical(as.character(envelope$analysis_id), analysis_id)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A producer envelope names a different analysis.",
                          relative_path)
  }
  list(
    reference = list(
      path = relative_path,
      envelope_path = envelope_relative,
      artifact_type = artifact_type,
      artifact_id = artifact_id,
      status = as.character(envelope$status),
      payload_sha256 = triomics_sha256(payload_path),
      envelope_sha256 = triomics_sha256(envelope_path),
      run_id = as.character(envelope$run_id),
      producer = envelope$producer
    ),
    payload = payload,
    envelope = envelope
  )
}

triomics_report_empty_frame <- function(columns, numeric_columns = character(), logical_columns = character()) {
  values <- setNames(lapply(columns, function(name) {
    if (name %in% numeric_columns) return(base::numeric())
    if (name %in% logical_columns) return(base::logical())
    character()
  }), columns)
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_optional_artifact <- function(root, relative_path, artifact_type, artifact_id,
                                              study_id, config_sha256, analysis_id) {
  envelope_relative <- sub("[.]json$", ".envelope.json", relative_path)
  payload_path <- triomics_report_safe_child(root, relative_path, relative_path, must_exist = FALSE)
  envelope_path <- triomics_report_safe_child(root, envelope_relative, envelope_relative,
                                               must_exist = FALSE)
  payload_exists <- file.exists(payload_path) || dir.exists(payload_path)
  envelope_exists <- file.exists(envelope_path) || dir.exists(envelope_path)
  if (!payload_exists && !envelope_exists) return(NULL)
  triomics_report_artifact(root, relative_path, artifact_type, artifact_id, study_id,
                           config_sha256, analysis_id, parse_payload = TRUE)
}

triomics_report_d_artifact <- function(root, config) {
  pointer_relative <- file.path("registries", "current-D.json")
  pointer_path <- triomics_report_safe_child(root, pointer_relative, pointer_relative,
                                              must_exist = FALSE)
  if (!file.exists(pointer_path) || dir.exists(pointer_path)) {
    triomics_report_abort("D_REPORT_REQUIRED_MISSING", "The current D generation pointer is missing.",
                          pointer_relative)
  }
  current <- tryCatch(triomics_d_validate_current_pointer(root, pointer_path),
                      error = function(error) NULL)
  if (!is.list(current) || !is.list(current$generation)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The D pointer or immutable generation failed validation.",
                          pointer_relative)
  }
  generation <- current$generation
  payload <- generation$payload
  if (!identical(as.character(payload$study_id), config$study_id) ||
      !identical(as.character(payload$analysis_id), config$analysis_id) ||
      !identical(as.character(payload$config_sha256), config$config_sha256) ||
      !identical(as.character(payload$privacy_class), config$privacy_class)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The D generation identity differs from the report configuration.",
                          "D.generation")
  }
  generation_id <- as.character(current$pointer$generation_id)
  generation_relative <- file.path("D", "generations", generation_id)
  payload_relative <- file.path(generation_relative, "evidence-routing.json")
  envelope_relative <- file.path(generation_relative, "evidence-routing.envelope.json")
  manifest_relative <- file.path(generation_relative, "generation-manifest.json")
  list(
    reference = list(
      pointer_path = pointer_relative,
      pointer_sha256 = as.character(current$pointer_sha256),
      generation_id = generation_id,
      manifest_path = manifest_relative,
      manifest_sha256 = as.character(generation$manifest_sha256),
      payload_path = payload_relative,
      payload_sha256 = triomics_sha256(triomics_report_safe_child(root, payload_relative,
                                                                   payload_relative)),
      envelope_path = envelope_relative,
      envelope_sha256 = triomics_sha256(triomics_report_safe_child(root, envelope_relative,
                                                                    envelope_relative)),
      producer = generation$manifest$producer,
      status = as.character(payload$status),
      run_id = as.character(current$pointer$run_id)
    ),
    payload = payload,
    manifest = generation$manifest
  )
}

triomics_report_load_d_reference <- function(root, reference, config) {
  required <- c("pointer_path", "pointer_sha256", "generation_id", "manifest_path",
                "manifest_sha256", "payload_path", "payload_sha256", "envelope_path",
                "envelope_sha256", "producer", "status", "run_id")
  if (!is.list(reference) || is.null(names(reference)) ||
      !identical(sort(names(reference)), sort(required)) ||
      !identical(reference$pointer_path, file.path("registries", "current-D.json")) ||
      !triomics_sha256_text(reference$pointer_sha256) ||
      !triomics_sha256_text(reference$generation_id) ||
      !triomics_sha256_text(reference$manifest_sha256)) {
    triomics_report_abort("D_REPORT_BUNDLE_INVALID", "The ReportBundle D reference is malformed.", "D")
  }
  expected_directory <- file.path("D", "generations", reference$generation_id)
  expected <- list(
    manifest_path = file.path(expected_directory, "generation-manifest.json"),
    payload_path = file.path(expected_directory, "evidence-routing.json"),
    envelope_path = file.path(expected_directory, "evidence-routing.envelope.json")
  )
  if (!identical(reference$manifest_path, expected$manifest_path) ||
      !identical(reference$payload_path, expected$payload_path) ||
      !identical(reference$envelope_path, expected$envelope_path)) {
    triomics_report_abort("D_REPORT_PATH_UNSAFE", "The ReportBundle D reference escapes its fixed generation paths.",
                          "D")
  }
  pointer_path <- triomics_report_safe_child(root, reference$pointer_path, "D.pointer")
  if (!identical(triomics_sha256(pointer_path), reference$pointer_sha256)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The D current-generation pointer changed after bundling.",
                          "D.pointer")
  }
  current <- tryCatch(triomics_d_validate_current_pointer(root, pointer_path),
                      error = function(error) NULL)
  if (!is.list(current) || !is.list(current$generation) ||
      !identical(as.character(current$pointer$generation_id), reference$generation_id) ||
      !identical(as.character(current$generation$manifest_sha256), reference$manifest_sha256) ||
      !identical(as.character(current$generation$payload$status), reference$status) ||
      !identical(current$generation$manifest$producer, reference$producer) ||
      !identical(as.character(current$pointer$run_id), reference$run_id)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The D generation no longer matches the ReportBundle.", "D")
  }
  payload_path <- triomics_report_safe_child(root, reference$payload_path, "D.payload")
  envelope_path <- triomics_report_safe_child(root, reference$envelope_path, "D.envelope")
  if (!identical(triomics_sha256(payload_path), reference$payload_sha256) ||
      !identical(triomics_sha256(envelope_path), reference$envelope_sha256)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The D generation payload or envelope changed after bundling.",
                          "D")
  }
  payload <- current$generation$payload
  if (!identical(as.character(payload$study_id), config$study_id) ||
      !identical(as.character(payload$analysis_id), config$analysis_id) ||
      !identical(as.character(payload$config_sha256), config$config_sha256) ||
      !identical(as.character(payload$privacy_class), config$privacy_class)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The D generation identity differs from the configuration.",
                          "D")
  }
  current$generation
}

triomics_report_bundle_from_root <- function(output_root, config_path) {
  if (!triomics_d_nonempty_string(output_root)) {
    triomics_report_abort("D_REPORT_REQUEST_INVALID", "The report output root is invalid.", "output")
  }
  root <- tryCatch(triomics_d_secure_path(output_root, "D_REPORT_PATH_UNSAFE",
                                          "The report output root is unsafe.", "output", directory = TRUE),
                   error = function(error) NULL)
  if (is.null(root) || !dir.exists(root)) {
    triomics_report_abort("D_REPORT_REQUIRED_MISSING", "The producing output root is unavailable.", "output")
  }
  config <- triomics_report_config(config_path)
  artifacts <- list(
    a = triomics_report_artifact(root, file.path("A", "assays.json"), "assays",
                                 "stage-a-assays", config$study_id, config$config_sha256,
                                 parse_payload = FALSE)$reference,
    b = triomics_report_artifact(root, file.path("B", "block-results.json"), "block_results",
                                 "stage-b-block-results", config$study_id, config$config_sha256,
                                 config$analysis_id)$reference,
    c = triomics_report_artifact(root, file.path("C", "integration-results.json"), "integration_results",
                                 "stage-c-integration-results", config$study_id, config$config_sha256,
                                 config$analysis_id)$reference,
    d = triomics_report_d_artifact(root, config)$reference,
    clinical = if (config$clinical_enabled) {
      optional <- triomics_report_optional_artifact(
        root, file.path("B", "clinical-associations.json"),
        "clinical_association_results", "b-t08-clinical-associations",
        config$study_id, config$config_sha256, config$analysis_id
      )
      if (is.null(optional)) NULL else optional$reference
    } else NULL
  )
  bundle <- structure(list(
    schema_version = "1.0",
    root = root,
    config_path = config$path,
    config_sha256 = config$config_sha256,
    study_id = config$study_id,
    analysis_id = config$analysis_id,
    privacy_class = config$privacy_class,
    audience = config$audience,
    artifacts = artifacts
  ), class = "triomics_report_bundle")
  triomics_report_verify_bundle(bundle, config)
  bundle
}

triomics_report_evaluation_status <- function(evaluation) {
  if (!is.list(evaluation)) return("not_reported")
  count_names <- c("n_eligible", "n_predicted", "failed_prediction_count",
                   "n_ineligible", "total_denominator")
  values <- lapply(count_names, function(name) evaluation[[name]])
  names(values) <- count_names
  if (any(!vapply(values, function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) &&
      value >= 0 && value == floor(value)
  }, logical(1)))) return("not_reported")
  counts <- vapply(values, as.numeric, numeric(1))
  if (counts[["n_predicted"]] > counts[["n_eligible"]] ||
      counts[["n_eligible"]] != counts[["n_predicted"]] + counts[["failed_prediction_count"]] ||
      counts[["n_eligible"]] > counts[["total_denominator"]] ||
      counts[["n_ineligible"]] > counts[["total_denominator"]] ||
      counts[["n_eligible"]] + counts[["n_ineligible"]] != counts[["total_denominator"]] ||
      counts[["failed_prediction_count"]] > counts[["n_eligible"]] - counts[["n_predicted"]]) {
    return("not_reported")
  }
  if (counts[["n_predicted"]] > 0 &&
      counts[["n_predicted"]] == counts[["total_denominator"]] &&
      counts[["n_eligible"]] == counts[["total_denominator"]] &&
      counts[["failed_prediction_count"]] == 0 && counts[["n_ineligible"]] == 0) {
    return("complete")
  }
  if (counts[["n_predicted"]] > 0) "partial" else "failed"
}

triomics_report_c_evaluation_only <- function(c_item, evidence) {
  is.list(c_item) && is.list(evidence) &&
    identical(as.character(c_item$status), "complete") &&
    is.null(c_item$result) && is.null(evidence$native_artifact_ref) &&
    identical(as.character(evidence$status), "unavailable") &&
    identical(as.character(evidence$reason_code), "C06_INTEGRATION_RESULT_UNAVAILABLE") &&
    triomics_report_evaluation_status(c_item$evaluation) %in% c("complete", "partial", "failed")
}

triomics_report_verify_d_source_lineage <- function(b_reference, c_payload, c_envelope, d_payload) {
  immutable <- d_payload$immutable_inputs
  b_source <- immutable$b_artifacts$block_results
  required_b <- c("artifact_type", "artifact_id", "path", "envelope_path", "sha256", "envelope_sha256")
  if (!is.null(b_reference)) {
    if (!is.list(b_source) || !all(required_b %in% names(b_source)) ||
        !identical(as.character(b_source$artifact_type), as.character(b_reference$artifact_type)) ||
        !identical(as.character(b_source$artifact_id), as.character(b_reference$artifact_id)) ||
        !identical(as.character(b_source$path), as.character(b_reference$path)) ||
        !identical(as.character(b_source$envelope_path), as.character(b_reference$envelope_path)) ||
        !identical(as.character(b_source$sha256), as.character(b_reference$payload_sha256)) ||
        !identical(as.character(b_source$envelope_sha256), as.character(b_reference$envelope_sha256))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID",
                            "The displayed B result bytes differ from the D generation's immutable B reference.",
                            "D.immutable_inputs.b_artifacts.block_results")
    }
  }

  c_source <- immutable$c_artifacts$integration_evidence
  required_c <- c("artifact_type", "artifact_id", "payload_file", "payload_sha256",
                  "envelope_file", "envelope_sha256")
  if (!is.list(c_source) || !all(required_c %in% names(c_source)) ||
      !identical(as.character(c_source$artifact_type), "integration_evidence_bundle") ||
      !identical(as.character(c_source$artifact_id), "c06-integration-evidence") ||
      !triomics_sha256_text(as.character(c_source$payload_sha256)) ||
      !triomics_sha256_text(as.character(c_source$envelope_sha256))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID",
                          "The D generation lacks its hash-bound C integration evidence reference.",
                          "D.immutable_inputs.c_artifacts.integration_evidence")
  }
  c_integrations <- c_payload$integrations %||% list()
  d_integrations <- d_payload$inherited_evidence$integration_evidence$integrations %||% list()
  inputs <- c_envelope$inputs %||% list()
  if (!is.list(c_integrations) || !is.list(d_integrations) || !is.list(inputs)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C integration lineage records are malformed.",
                          "C.integration_lineage")
  }
  c_ids <- vapply(seq_along(c_integrations), function(index) {
    triomics_report_identifier(c_integrations[[index]], "integration_id")
  }, character(1))
  d_ids <- vapply(seq_along(d_integrations), function(index) {
    triomics_report_identifier(d_integrations[[index]], "integration_id")
  }, character(1))
  if (anyDuplicated(c_ids) || anyDuplicated(d_ids) || !setequal(c_ids, d_ids)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID",
                          "C result integrations do not match the D hash-bound integration evidence identities.",
                          "C.integration_lineage.integration_id")
  }
  input_pairs <- lapply(inputs, function(input) {
    if (!is.list(input) || is.null(names(input)) ||
        !triomics_d_nonempty_string(input$artifact_id) ||
        !triomics_sha256_text(as.character(input$sha256))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A C envelope input reference is malformed.",
                            "C.envelope.inputs")
    }
    list(artifact_id = as.character(input$artifact_id), sha256 = as.character(input$sha256))
  })
  for (id in c_ids) {
    evidence <- d_integrations[[match(id, d_ids)]]
    reference <- evidence$native_artifact_ref
    c_item <- c_integrations[[match(id, c_ids)]]
    if (identical(as.character(c_item$status), "complete")) {
      if (triomics_report_c_evaluation_only(c_item, evidence)) next
      if (!is.list(reference) || !triomics_d_nonempty_string(reference$artifact_id) ||
          !triomics_sha256_text(as.character(reference$sha256)) ||
          !any(vapply(input_pairs, function(input) {
            identical(input$artifact_id, as.character(reference$artifact_id)) &&
              identical(input$sha256, as.character(reference$sha256))
          }, logical(1)))) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID",
                              "A displayed C result is not bound to the native evidence referenced by D.",
                              paste0("C.integration_lineage.", id))
      }
    }
  }
  invisible(TRUE)
}

triomics_report_verify_d_public_c_evidence <- function(root, d_payload, config) {
  source <- d_payload$immutable_inputs$c_artifacts$integration_evidence
  required <- c("artifact_type", "artifact_id", "payload_file", "payload_sha256",
                "envelope_file", "envelope_sha256")
  if (!is.list(source) || !all(required %in% names(source)) ||
      !identical(as.character(source$artifact_type), "integration_evidence_bundle") ||
      !identical(as.character(source$artifact_id), "c06-integration-evidence") ||
      !identical(as.character(source$payload_file), "integration-evidence.json") ||
      !identical(as.character(source$envelope_file), "integration-evidence.envelope.json") ||
      !triomics_sha256_text(as.character(source$payload_sha256)) ||
      !triomics_sha256_text(as.character(source$envelope_sha256))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID",
                          "The D generation lacks its fixed hash-bound C integration evidence artifact.",
                          "D.immutable_inputs.c_artifacts.integration_evidence")
  }
  artifact <- triomics_report_artifact(
    root, file.path("D", source$payload_file), source$artifact_type, source$artifact_id,
    config$study_id, config$config_sha256, config$analysis_id, parse_payload = TRUE
  )
  if (!identical(artifact$reference$payload_sha256, as.character(source$payload_sha256)) ||
      !identical(artifact$reference$envelope_sha256, as.character(source$envelope_sha256))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID",
                          "The public C integration evidence bytes differ from D's immutable reference.",
                          "D.immutable_inputs.c_artifacts.integration_evidence")
  }
  inherited <- d_payload$inherited_evidence$integration_evidence
  if (!is.list(inherited) ||
      !identical(triomics_canonical_json(artifact$payload), triomics_canonical_json(inherited))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID",
                          "D's inherited C evidence differs from the hash-bound public C artifact.",
                          "D.inherited_evidence.integration_evidence")
  }
  invisible(artifact$payload)
}

triomics_report_matrix_signature <- function(value, row_key, column_key, path) {
  if (!is.list(value) || !is.list(value$rows) || !is.list(value$columns) ||
      !is.list(value$values)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A C variance matrix has an invalid typed shape.", path)
  }
  rows <- if (!is.null(names(value$rows)) && row_key %in% names(value$rows)) value$rows[[row_key]] else value$rows
  columns <- if (!is.null(names(value$columns)) && column_key %in% names(value$columns)) value$columns[[column_key]] else value$columns
  if (!is.list(rows) || !is.list(columns) || length(value$values) != length(rows)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C variance matrix dimensions do not align.", path)
  }
  rows <- vapply(seq_along(rows), function(index) {
    triomics_report_identifier_value(rows[[index]], paste0(path, ".rows[", index, "]"))
  }, character(1))
  columns <- vapply(seq_along(columns), function(index) {
    triomics_report_identifier_value(columns[[index]], paste0(path, ".columns[", index, "]"))
  }, character(1))
  if (anyDuplicated(rows) || anyDuplicated(columns)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C variance matrix identifiers are duplicated.", path)
  }
  matrix_values <- lapply(value$values, function(row) {
    if (!is.list(row) || length(row) != length(columns) ||
        any(!vapply(row, function(cell) is.numeric(cell) && length(cell) == 1L &&
                    is.finite(cell), logical(1)))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A C variance matrix row is malformed.", path)
    }
    as.numeric(unlist(row, use.names = FALSE))
  })
  list(rows = unlist(rows, use.names = FALSE), columns = unlist(columns, use.names = FALSE),
       values = matrix_values)
}

triomics_report_verify_c_projection <- function(c_payload, c_envelope, d_payload) {
  triomics_report_verify_d_source_lineage(NULL, c_payload, c_envelope, d_payload)
  d_c_evidence <- d_payload$inherited_evidence$integration_evidence
  c_integrations <- c_payload$integrations %||% list()
  d_integrations <- d_c_evidence$integrations %||% list()
  d_ids <- vapply(seq_along(d_integrations), function(index) {
    as.character(d_integrations[[index]]$integration_id)
  }, character(1))
  for (field in c("preparation_context_id", "representation_id")) {
    c_value <- triomics_report_identifier_value(c_payload[[field]], paste0("C.", field))
    d_value <- triomics_report_identifier_value(d_c_evidence[[field]], paste0("D.C.", field))
    if (!identical(c_value, d_value)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID",
                            "C preparation or representation context differs from D's hash-bound evidence.",
                            paste0("C.", field))
    }
  }
  for (integration in c_integrations) {
    id <- triomics_report_identifier(integration, "integration_id")
    evidence <- d_integrations[[match(id, d_ids)]]
    evaluation_only <- triomics_report_c_evaluation_only(integration, evidence)
    status_matches <- identical(as.character(integration$status), as.character(evidence$status)) ||
      evaluation_only
    if (!status_matches ||
        !identical(as.character(integration$method), as.character(evidence$backend))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID",
                            "A C integration status or method differs from the D hash-bound evidence.",
                            paste0("C.integration_lineage.", id))
    }
    result <- integration$result %||% list()
    common <- evidence$common %||% list()
    parameters <- common$parameters %||% list()
    if (identical(as.character(integration$status), "complete") && !evaluation_only) {
      d_context <- evidence$context %||% list()
      if (!identical(triomics_report_identifier_value(d_context$preparation_context_id,
                                                      paste0("D.", id, ".preparation_context_id")),
                     triomics_report_identifier_value(c_payload$preparation_context_id,
                                                      paste0("C.", id, ".preparation_context_id"))) ||
          !identical(triomics_report_identifier_value(d_context$representation_id,
                                                      paste0("D.", id, ".representation_id")),
                     triomics_report_identifier_value(c_payload$representation_id,
                                                      paste0("C.", id, ".representation_id")))) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID",
                              "C integration context differs from D's hash-bound evidence.",
                              paste0("C.integration_lineage.", id, ".context"))
      }
    }
    if (!identical(triomics_canonical_json(result$selected %||% list()),
                   triomics_canonical_json(common$selected %||% list())) ||
        !identical(triomics_canonical_json(result$params$rank_lineage %||% list()),
                   triomics_canonical_json(parameters$rank_lineage %||% list())) ||
        !identical(triomics_canonical_json(result$params$feature_flow %||% list()),
                   triomics_canonical_json(parameters$feature_flow %||% list())) ||
        !identical(as.character(result$params$retained_n_components %||% ""),
                   as.character(parameters$retained_n_components %||% "")) ||
        !identical(as.character(result$params$effective$selection$rule %||% ""),
                   as.character(evidence$selection$rule$rule %||% ""))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID",
                            "Displayed C rank, selection, or retained-feature values differ from D evidence.",
                            paste0("C.integration_lineage.", id))
    }
    c_variance <- result$variance
    d_variance <- evidence$variance$common_values
    if (is.null(c_variance) != is.null(d_variance)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID",
                            "Displayed C variance is not present in the D hash-bound evidence.",
                            paste0("C.integration_lineage.", id, ".variance"))
    }
    if (!is.null(c_variance)) {
      c_matrix <- triomics_report_matrix_signature(c_variance, "block_ids", "component_ids",
                                                    paste0("C.", id, ".variance"))
      d_matrix <- triomics_report_matrix_signature(d_variance, "rows", "columns",
                                                    paste0("D.", id, ".variance"))
      d_definition <- evidence$variance$common_definition %||% list()
      definition_fields <- c("metric", "assay_space", "denominator", "semantics")
      definitions_match <- all(vapply(definition_fields, function(field) {
        identical(as.character(c_variance[[field]] %||% ""),
                  as.character(d_definition[[field]] %||% ""))
      }, logical(1)))
      if (!identical(c_matrix$rows, d_matrix$rows) ||
          !identical(c_matrix$columns, d_matrix$columns) ||
          !identical(triomics_canonical_json(c_matrix$values), triomics_canonical_json(d_matrix$values)) ||
          !definitions_match) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID",
                              "Displayed C variance differs from the D hash-bound common evidence.",
                              paste0("C.integration_lineage.", id, ".variance"))
      }
    }
  }
  invisible(TRUE)
}

triomics_report_verify_bundle <- function(bundle, config) {
  fields <- c("schema_version", "root", "config_path", "config_sha256", "study_id",
              "analysis_id", "privacy_class", "audience", "artifacts")
  if (!is.list(bundle) || is.null(names(bundle)) ||
      !identical(sort(names(bundle)), sort(fields)) ||
      !identical(bundle$schema_version, "1.0") ||
      !identical(bundle$config_sha256, config$config_sha256) ||
      !identical(bundle$study_id, config$study_id) ||
      !identical(bundle$analysis_id, config$analysis_id) ||
      !identical(bundle$privacy_class, config$privacy_class) ||
      !identical(bundle$audience, config$audience) ||
      !is.list(bundle$artifacts) ||
      !identical(sort(names(bundle$artifacts)), sort(c("a", "b", "c", "d", "clinical")))) {
    triomics_report_abort("D_REPORT_BUNDLE_INVALID", "The ReportBundle does not match its configuration manifest.",
                          "report_bundle")
  }
  root <- tryCatch(triomics_d_secure_path(bundle$root, "D_REPORT_PATH_UNSAFE",
                                          "The ReportBundle root is unsafe.", "report_bundle.root",
                                          directory = TRUE),
                   error = function(error) NULL)
  if (is.null(root) || !dir.exists(root) || !identical(root, bundle$root)) {
    triomics_report_abort("D_REPORT_PATH_UNSAFE", "The ReportBundle root changed or crosses a symbolic link.",
                          "report_bundle.root")
  }
  config_path <- tryCatch(triomics_d_secure_path(bundle$config_path, "D_REPORT_PATH_UNSAFE",
                                                 "The ReportBundle configuration path is unsafe.",
                                                 "report_bundle.config_path"),
                          error = function(error) NULL)
  if (is.null(config_path) || !file.exists(config_path) || dir.exists(config_path) ||
      !identical(config_path, bundle$config_path) ||
      !identical(triomics_sha256(config_path), bundle$config_sha256)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The producing configuration bytes changed after bundling.",
                          "config")
  }
  expected_artifacts <- list(
    a = c(path = file.path("A", "assays.json"), type = "assays", id = "stage-a-assays"),
    b = c(path = file.path("B", "block-results.json"), type = "block_results", id = "stage-b-block-results"),
    c = c(path = file.path("C", "integration-results.json"), type = "integration_results", id = "stage-c-integration-results")
  )
  payloads <- list()
  envelopes <- list()
  for (name in names(expected_artifacts)) {
    reference <- bundle$artifacts[[name]]
    values <- expected_artifacts[[name]]
    if (!is.list(reference) || !identical(reference$path, unname(values[["path"]])) ||
        !identical(reference$artifact_type, unname(values[["type"]])) ||
        !identical(reference$artifact_id, unname(values[["id"]]))) {
      triomics_report_abort("D_REPORT_BUNDLE_INVALID", "A ReportBundle source path or artifact identity is invalid.",
                            name)
    }
    artifact <- triomics_report_artifact(
      root, reference$path, reference$artifact_type, reference$artifact_id,
      config$study_id, config$config_sha256,
      if (name == "a") NULL else config$analysis_id,
      parse_payload = TRUE
    )
    if (!identical(artifact$reference, reference)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A producer artifact changed after bundle creation.", name)
    }
    payloads[[name]] <- artifact$payload
    envelopes[[name]] <- artifact$envelope
  }
  d_generation <- triomics_report_load_d_reference(root, bundle$artifacts$d, config)
  if (is.null(bundle$artifacts$clinical)) {
    payloads$clinical <- NULL
    envelopes$clinical <- NULL
  } else {
    clinical_ref <- bundle$artifacts$clinical
    if (!identical(clinical_ref$path, file.path("B", "clinical-associations.json")) ||
        !identical(clinical_ref$artifact_type, "clinical_association_results") ||
        !identical(clinical_ref$artifact_id, "b-t08-clinical-associations")) {
      triomics_report_abort("D_REPORT_BUNDLE_INVALID", "The clinical source reference is not a registered export.",
                            "clinical")
    }
    clinical <- triomics_report_artifact(
      root, clinical_ref$path, clinical_ref$artifact_type, clinical_ref$artifact_id,
      config$study_id, config$config_sha256, config$analysis_id, parse_payload = TRUE
    )
    if (!identical(clinical$reference, clinical_ref)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "The clinical results changed after bundle creation.",
                            "clinical")
    }
    payloads$clinical <- clinical$payload
    envelopes$clinical <- clinical$envelope
  }
  d <- d_generation$payload
  triomics_report_verify_d_public_c_evidence(root, d, config)
  triomics_report_verify_d_source_lineage(bundle$artifacts$b, payloads$c, envelopes$c, d)
  triomics_report_verify_c_projection(payloads$c, envelopes$c, d)
  if (!identical(as.character(payloads$b$analysis_id), config$analysis_id) ||
      !identical(as.character(payloads$c$analysis_id), config$analysis_id) ||
      !is.list(payloads$b$blocks) || !is.list(payloads$c$integrations) ||
      !identical(as.character(d$analysis_id), config$analysis_id)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "B, C, and D analysis identities or payload shapes differ.",
                          "analysis")
  }
  list(root = root, config = config, payloads = payloads, envelopes = envelopes,
       d_generation = d_generation, d = d, artifacts = bundle$artifacts)
}

triomics_report_text <- function(record, key, default = "") {
  value <- if (is.list(record)) record[[key]] else NULL
  if (is.null(value)) {
    if (is.null(default) || length(default) != 1L || is.na(default)) return("")
    return(triomics_report_clean_text(default))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A report label has an invalid scalar type.", key)
  }
  triomics_report_clean_text(value)
}

triomics_report_identifier_value <- function(value, key, default = "") {
  if (is.null(value)) return(default)
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A scientific identifier has an invalid scalar type.", key)
  }
  if (!nzchar(value)) return(default)
  if (triomics_report_contains_private_text(value) || grepl("[[:cntrl:]]", value, perl = TRUE)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A scientific identifier contains unsafe private text.", key)
  }
  value
}

triomics_report_identifier <- function(record, key, default = "") {
  value <- if (is.list(record)) record[[key]] else NULL
  triomics_report_identifier_value(value, key, default)
}

triomics_report_unit <- function(value, key = "effect_unit") {
  if (is.list(value)) value <- value[[key]]
  if (is.null(value)) return("")
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A scientific unit has an invalid scalar type.", key)
  }
  if (triomics_report_contains_private_text(value) || grepl("[[:cntrl:]]", value, perl = TRUE)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A scientific unit contains unsafe private text.", key)
  }
  value
}

triomics_report_identifier_collection <- function(value, key, default = "") {
  if (is.null(value) || !length(value)) return(default)
  values <- if (is.list(value)) value else as.list(value)
  identifiers <- vapply(seq_along(values), function(index) {
    triomics_report_identifier_value(values[[index]], paste0(key, "[", index, "]"))
  }, character(1))
  paste(identifiers, collapse = ";")
}

triomics_report_requirement <- function(requirements, name) {
  if (!is.list(requirements) && !is.atomic(requirements)) return(FALSE)
  requirement_names <- names(requirements)
  if (is.null(requirement_names) || !name %in% requirement_names) return(FALSE)
  isTRUE(requirements[[name]])
}

triomics_report_reason <- function(record, key, default = "") {
  value <- triomics_report_text(record, key)
  if (!nzchar(value)) return(default)
  if (grepl("^[A-Za-z][A-Za-z0-9_.:-]{0,127}$", value)) return(value)
  "source_reason_withheld"
}

triomics_report_number <- function(record, key) {
  value <- if (is.list(record)) record[[key]] else NULL
  if (is.null(value)) return(NA_real_)
  if (!is.numeric(value) || length(value) != 1L || is.nan(value) || is.infinite(value)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A report statistic has an invalid numeric type.", key)
  }
  as.numeric(value)
}

triomics_report_rows <- function(block_results, blocks) {
  columns <- c(
    "block_id", "block_name", "feature_key", "analysis_id", "preparation_context_id",
    "representation_id", "claim_type", "claim_id", "contrast_id", "family_id",
    "family_member_hash", "family_claim_scope", "family_correction_method",
    "family_n_declared", "family_n_eligible", "family_n_evaluated", "family_n_failed",
    "effect", "effect_unit", "ordinary_se", "moderated_se", "uncertainty_status",
    "interval_lower", "interval_upper", "interval_method", "interval_status",
    "statistic", "statistic_name", "ordinary_df", "effective_test_df", "p", "q",
    "inferential_status", "estimation_status", "estimation_reason", "n_samples",
    "n_subjects", "n_rows_used", "n_pairs", "direction_numerator",
    "direction_denominator", "direction_zero_count", "direction_status",
    "direction_reason", "count_prior_count", "count_prior_type", "count_prior_metadata_sha256",
    "count_prior_status", "source_hash", "model_hash",
    "representation_hash", "contrast_hash", "result_id"
  )
  records <- list()
  block_names <- vapply(seq_along(blocks), function(index) {
    triomics_report_identifier(blocks[[index]], "block_id")
  }, character(1))
  block_labels <- vapply(seq_along(blocks), function(index) paste("Block", index), character(1))
  append_claim <- function(block_id, claim_id, claim, claim_type) {
    rows <- claim$rows %||% list()
    if (!is.list(rows)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A B claim does not contain a typed result-row array.",
                            paste(block_id, claim_id, sep = "."))
    }
    family <- claim$family %||% list()
    block_index <- match(block_id, block_names)
    block_name <- if (is.na(block_index)) block_id else block_labels[[block_index]]
    for (row in rows) {
      if (!is.list(row) || is.null(names(row))) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "A B result row is not a named record.", claim_id)
      }
      effect_unit_value <- if (!is.null(row$effect_unit)) row$effect_unit else claim$effect_unit
      effect_unit <- triomics_report_unit(effect_unit_value)
      interval_lower <- if (!is.null(row$interval_lower)) triomics_report_number(row, "interval_lower") else
        if (!is.null(row$ci_lower)) triomics_report_number(row, "ci_lower") else NA_real_
      interval_upper <- if (!is.null(row$interval_upper)) triomics_report_number(row, "interval_upper") else
        if (!is.null(row$ci_upper)) triomics_report_number(row, "ci_upper") else NA_real_
      ordinary_se <- triomics_report_number(row, "ordinary_se")
      moderated_se <- triomics_report_number(row, "moderated_se")
      interval_status <- if (is.finite(interval_lower) && is.finite(interval_upper)) {
        "source_reported"
      } else "not_exported"
      uncertainty_status <- if (is.finite(ordinary_se) || is.finite(moderated_se)) {
        "source_reported_se"
      } else "not_reported"
      count_ref <- row$count_prior_ref
      if (is.null(count_ref)) count_ref <- list()
      if (!is.list(count_ref) || (!is.null(names(count_ref)) && any(!nzchar(names(count_ref))))) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "A B count reference is not a typed record.", claim_id)
      }
      count_prior_count <- triomics_report_number(count_ref, "count")
      count_prior_type <- triomics_report_text(count_ref, "count_type")
      count_prior_metadata <- triomics_report_text(count_ref, "metadata_sha256")
      count_prior_status <- if (is.finite(count_prior_count) && nzchar(count_prior_type)) {
        "source_reported"
      } else if (length(count_ref)) "source_reference_withheld" else "not_reported"
      values <- list(
        block_id = triomics_report_identifier_value(block_id, "B.block_id"), block_name = block_name,
        feature_key = triomics_report_identifier(row, "feature_key"),
        analysis_id = triomics_report_identifier(row, "analysis_id",
                                                 triomics_report_identifier_value(block_results$analysis_id, "B.analysis_id")),
        preparation_context_id = triomics_report_identifier_value(
          as.character(block_results$preparation_context_id %||% "not_reported"), "B.preparation_context_id"),
        representation_id = triomics_report_identifier(row, "representation_id",
          triomics_report_identifier_value(as.character(block_results$representation_id %||% "not_reported"),
                                           "B.representation_id")),
        claim_type = triomics_report_identifier(row, "claim_type", claim_type),
        claim_id = triomics_report_identifier_value(claim_id, "B.claim_id"),
        contrast_id = triomics_report_identifier(row, "contrast_id", claim_id),
        family_id = triomics_report_identifier(row, "family_id", triomics_report_identifier(family, "family_id")),
        family_member_hash = triomics_report_identifier(family, "member_hash"),
        family_claim_scope = triomics_report_identifier(family, "claim_scope"),
        family_correction_method = triomics_report_identifier(family, "correction_method"),
        family_n_declared = triomics_report_number(family, "n_declared"),
        family_n_eligible = triomics_report_number(family, "n_eligible"),
        family_n_evaluated = triomics_report_number(family, "n_evaluated"),
        family_n_failed = triomics_report_number(family, "n_failed"),
        effect = triomics_report_number(row, "effect"),
        effect_unit = effect_unit,
        ordinary_se = ordinary_se,
        moderated_se = moderated_se,
        uncertainty_status = uncertainty_status,
        interval_lower = interval_lower,
        interval_upper = interval_upper,
        interval_method = triomics_report_identifier(row, "interval_method"),
        interval_status = interval_status,
        statistic = triomics_report_number(row, "statistic"),
        statistic_name = triomics_report_identifier(row, "statistic_name"),
        ordinary_df = triomics_report_number(row, "ordinary_df"),
        effective_test_df = triomics_report_number(row, "effective_test_df"),
        p = triomics_report_number(row, "p"), q = triomics_report_number(row, "q"),
        inferential_status = triomics_report_identifier(row, "inferential_status"),
        estimation_status = triomics_report_identifier(row, "estimation_status"),
        estimation_reason = triomics_report_reason(row, "estimation_reason"),
        n_samples = triomics_report_number(row, "n_samples"),
        n_subjects = triomics_report_number(row, "n_subjects"),
        n_rows_used = triomics_report_number(row, "n_rows_used"),
        n_pairs = triomics_report_number(row, "n_pairs"),
        direction_numerator = triomics_report_number(row, "direction_numerator"),
        direction_denominator = triomics_report_number(row, "direction_denominator"),
        direction_zero_count = triomics_report_number(row, "direction_zero_count"),
        direction_status = triomics_report_identifier(row, "direction_status"),
        direction_reason = triomics_report_reason(row, "direction_reason"),
        count_prior_count = count_prior_count,
        count_prior_type = triomics_report_identifier_value(count_prior_type, "B.count_prior_type"),
        count_prior_metadata_sha256 = triomics_report_identifier_value(count_prior_metadata,
                                                                        "B.count_prior_metadata_sha256"),
        count_prior_status = count_prior_status,
        source_hash = triomics_report_identifier(row, "source_hash"),
        model_hash = triomics_report_identifier(row, "model_hash"),
        representation_hash = triomics_report_identifier(row, "representation_hash"),
        contrast_hash = triomics_report_identifier(row, "contrast_hash"),
        result_id = triomics_report_identifier(row, "result_id")
      )
      values <- lapply(values, function(value) {
        if (is.null(value) || !length(value)) return(NA_character_)
        if (length(value) != 1L) {
          triomics_report_abort("D_REPORT_SOURCE_INVALID", "A projected B field is not scalar.", claim_id)
        }
        value
      })
      records[[length(records) + 1L]] <<- as.data.frame(values, stringsAsFactors = FALSE,
                                                         check.names = FALSE)
    }
  }
  for (block_name in names(block_results$blocks)) {
    block <- block_results$blocks[[block_name]]
    block_id <- as.character(block$block_id %||% block_name)
    for (group_name in c("contrasts", "joint_claims")) {
      group <- block[[group_name]] %||% list()
      if (!is.list(group)) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "B claim groups are not typed records.", block_id)
      }
      for (claim_id in names(group)) {
        claim <- group[[claim_id]]
        if (is.list(claim) && !is.null(claim$rows)) {
          append_claim(block_id, claim_id, claim,
                       triomics_report_text(claim, "claim_type", group_name))
        }
      }
    }
  }
  if (!length(records)) return(triomics_report_empty_frame(
    columns,
    numeric_columns = c("family_n_declared", "family_n_eligible", "family_n_evaluated", "family_n_failed",
                        "effect", "ordinary_se", "moderated_se", "interval_lower", "interval_upper",
                        "statistic", "ordinary_df", "effective_test_df", "p", "q", "n_samples",
                        "n_subjects", "n_rows_used", "n_pairs", "direction_numerator",
                        "direction_denominator", "direction_zero_count", "count_prior_count")
  ))
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_assay_summary <- function(payload, config) {
  if (!is.list(payload) || !identical(as.character(payload$schema_version), "2.0") ||
      !identical(as.character(payload$study_id), config$study_id) || !is.list(payload$blocks)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "The A assay export has an invalid report shape.", "A.assays")
  }
  block_ids <- vapply(config$blocks, function(block) as.character(block$block_id), character(1))
  assay_ids <- vapply(payload$blocks, function(block) {
    if (!is.list(block) || !triomics_d_nonempty_string(as.character(block$block_id %||% ""))) "" else
      as.character(block$block_id)
  }, character(1))
  if (any(!nzchar(assay_ids)) || anyDuplicated(assay_ids) ||
      !identical(assay_ids, block_ids) || length(payload$blocks) != length(config$blocks)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A assay block identities do not match the configuration.",
                          "A.assays.blocks")
  }
  records <- lapply(seq_along(payload$blocks), function(index) {
    block <- payload$blocks[[index]]
    features <- block$feature_keys %||% list()
    samples <- block$sample_ids %||% list()
    rows <- block$values %||% list()
    if (!is.list(features) || !is.list(samples) || !is.list(rows) || !length(features) ||
        !length(samples) || length(rows) != length(features)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A assay dimensions and value rows do not align.",
                            "A.assays.values")
    }
    if (any(!vapply(c(features, samples), function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1)))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A assay identifiers are malformed.", "A.assays.ids")
    }
    if (anyDuplicated(unlist(features, use.names = FALSE)) ||
        anyDuplicated(unlist(samples, use.names = FALSE))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A assay dimensions contain duplicate identifiers.",
                            "A.assays.ids")
    }
    observed <- 0L
    for (row in rows) {
      if (!is.list(row) || length(row) != length(samples)) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "An A assay row does not match its sample dimension.",
                              "A.assays.values")
      }
      for (cell in row) {
        if (is.null(cell)) next
        if (!is.numeric(cell) || length(cell) != 1L || is.nan(cell) || is.infinite(cell)) {
          triomics_report_abort("D_REPORT_SOURCE_INVALID", "An A assay cell is not a finite number or null.",
                                "A.assays.values")
        }
        observed <- observed + 1L
      }
    }
    total <- as.double(length(features)) * length(samples)
    list(
      block_id = block_ids[[index]], block_name = paste("Block", index),
      source_feature_count = as.integer(length(features)),
      source_sample_count = as.integer(length(samples)),
      observed_cell_count = as.integer(observed),
      missing_cell_count = as.integer(total - observed)
    )
  })
  as.data.frame(do.call(rbind, lapply(records, function(record) as.data.frame(record,
                                                                               stringsAsFactors = FALSE))),
                stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_clinical_adjustments <- function(row) {
  adjustments <- row$adjustment %||% list()
  if (!is.list(adjustments)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "Clinical adjustments are not a typed record array.",
                          "clinical.adjustment")
  }
  if (!length(adjustments)) {
    return(list(count = 0L, variables = "", roles = "", forms = "", status = "none_declared"))
  }
  if (any(!vapply(adjustments, function(adjustment) {
    is.list(adjustment) && !is.null(names(adjustment))
  }, logical(1)))) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A clinical adjustment is not a named record.",
                          "clinical.adjustment")
  }
  variable <- vapply(seq_along(adjustments), function(index) {
    triomics_report_identifier(adjustments[[index]], "variable")
  }, character(1))
  role <- vapply(seq_along(adjustments), function(index) {
    triomics_report_identifier(adjustments[[index]], "role", "not_reported")
  }, character(1))
  form <- vapply(seq_along(adjustments), function(index) {
    triomics_report_identifier(adjustments[[index]], "form", "not_reported")
  }, character(1))
  list(count = as.integer(length(adjustments)), variables = paste(variable, collapse = ";"),
       roles = paste(role, collapse = ";"), forms = paste(form, collapse = ";"),
       status = "source_reported")
}

triomics_report_clinical_rows <- function(payload) {
  columns <- c("objective_id", "endpoint_id", "clinical_variable", "effect", "effect_unit",
               "ordinary_se", "moderated_se", "uncertainty_status", "interval_lower",
               "interval_upper", "interval_method", "interval_status", "statistic",
               "statistic_name", "ordinary_df", "effective_test_df", "p", "q", "status",
               "adjustment_count", "adjustment_variables", "adjustment_roles", "adjustment_forms",
               "adjustment_status", "n_subjects", "n_rows_used", "family_id", "source_hash",
               "model_hash", "result_id")
  rows <- payload$results %||% list()
  if (!is.list(rows)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "Clinical results are not a typed record array.",
                          "clinical.results")
  }
  records <- lapply(seq_along(rows), function(index) {
    row <- rows[[index]]
    if (!is.list(row) || is.null(names(row))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A clinical association result is not a named record.",
                            "clinical.results")
    }
    lower <- if (!is.null(row$interval_low)) triomics_report_number(row, "interval_low") else
      if (!is.null(row$interval_lower)) triomics_report_number(row, "interval_lower") else NA_real_
    upper <- if (!is.null(row$interval_high)) triomics_report_number(row, "interval_high") else
      if (!is.null(row$interval_upper)) triomics_report_number(row, "interval_upper") else NA_real_
    ordinary_se <- triomics_report_number(row, "ordinary_se")
    moderated_se <- triomics_report_number(row, "moderated_se")
    adjustment <- triomics_report_clinical_adjustments(row)
    values <- list(
      objective_id = triomics_report_identifier(row, "objective_id", "not_reported"),
      endpoint_id = triomics_report_identifier(row, "endpoint_id", "not_reported"),
      clinical_variable = triomics_report_identifier(row, "predictor_ref", "not_reported"),
      effect = triomics_report_number(row, "effect"),
      effect_unit = triomics_report_unit(row, "effect_unit"),
      ordinary_se = ordinary_se, moderated_se = moderated_se,
      uncertainty_status = if (is.finite(ordinary_se) || is.finite(moderated_se)) "source_reported_se" else "not_reported",
      interval_lower = lower, interval_upper = upper,
      interval_method = triomics_report_identifier(row, "interval_method", "not_reported"),
      interval_status = if (is.finite(lower) && is.finite(upper)) "source_reported" else "not_exported",
      statistic = triomics_report_number(row, "statistic"),
      statistic_name = triomics_report_identifier(row, "statistic_name", "not_reported"),
      ordinary_df = triomics_report_number(row, "ordinary_df"),
      effective_test_df = triomics_report_number(row, "effective_test_df"),
      p = triomics_report_number(row, "p"), q = triomics_report_number(row, "q"),
      status = triomics_report_reason(row, "status",
                 triomics_report_reason(row, "estimation_status",
                   triomics_report_reason(row, "inferential_status", "not_reported"))),
      adjustment_count = adjustment$count,
      adjustment_variables = adjustment$variables,
      adjustment_roles = adjustment$roles,
      adjustment_forms = adjustment$forms,
      adjustment_status = adjustment$status,
      n_subjects = triomics_report_number(row, "n_subjects"),
      n_rows_used = triomics_report_number(row, "n_rows_used"),
      family_id = triomics_report_identifier(row, "family_id", "not_reported"),
      source_hash = triomics_report_identifier(row, "source_hash", "not_reported"),
      model_hash = triomics_report_identifier(row, "model_hash", "not_reported"),
      result_id = triomics_report_identifier(row, "result_id", "not_reported")
    )
    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  })
  if (!length(records)) return(triomics_report_empty_frame(
    columns,
    numeric_columns = c("effect", "ordinary_se", "moderated_se", "interval_lower", "interval_upper",
                        "statistic", "ordinary_df", "effective_test_df", "p", "q", "n_subjects",
                        "n_rows_used", "adjustment_count")
  ))
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_integration_rows <- function(payload, config, d = list(), c_payload_sha256 = "") {
  integrations <- payload$integrations %||% list()
  if (!is.list(integrations)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C integration results are not a typed record array.",
                          "C.integrations")
  }
  d_integrations <- d$inherited_evidence$integration_evidence$integrations %||% list()
  d_ids <- if (is.list(d_integrations)) vapply(seq_along(d_integrations), function(index) {
    triomics_report_identifier(d_integrations[[index]], "integration_id")
  }, character(1)) else character()
  records <- lapply(integrations, function(integration) {
    result <- integration$result %||% list()
    variance <- result$variance %||% list()
    params <- result$params %||% list()
    rank <- params$rank_lineage %||% list()
    selection <- params$effective$selection %||% list()
    selected <- result$selected %||% list()
    selected_count <- sum(vapply(selected, function(items) if (is.list(items)) length(items) else 0L, integer(1)))
    integration_id <- triomics_report_identifier(integration, "integration_id")
    evidence_index <- match(integration_id, d_ids)
    evidence <- if (is.na(evidence_index)) NULL else d_integrations[[evidence_index]]
    execution_status <- triomics_report_identifier(integration, "status", "unavailable")
    single_fit_status <- triomics_report_identifier(
      evidence, "status",
      if (!is.null(integration$result)) execution_status else "unavailable"
    )
    comparisons <- d$inherited_evidence$integration_comparisons %||% d$integration_comparisons %||% list()
    comparison_status <- as.character(comparisons$status %||% comparisons$comparisons$status %||% "unavailable")
    pairs <- comparisons$pairs %||% comparisons$comparisons$pairs %||% list()
    rank_available <- length(rank) > 0L
    selected_from_rank <- triomics_report_number(rank, "selected_feature_count")
    list(
      integration_id = integration_id,
      method = triomics_report_identifier(integration, "method", "not_reported"),
      status = single_fit_status,
      execution_status = execution_status,
      evaluation_status = triomics_report_evaluation_status(integration$evaluation),
      evaluation_source_path = "C/integration-results.json",
      evaluation_source_sha256 = if (triomics_sha256_text(as.character(c_payload_sha256))) {
        as.character(c_payload_sha256)
      } else "not_reported",
      single_fit_status = single_fit_status,
      single_fit_reason_code = triomics_report_identifier(evidence, "reason_code", "not_reported"),
      required = triomics_report_requirement(config$integration_requirements, integration_id),
      eligibility_status = if (rank_available && identical(single_fit_status, "complete")) {
        "source_reported_eligible"
      } else "not_reported",
      training_unit_count = triomics_report_number(rank, "training_unit_count"),
      maximum_centered_rank = triomics_report_number(rank, "maximum_centered_rank"),
      numerical_rank = triomics_report_number(rank, "numerical_rank"),
      requested_components = triomics_report_number(rank, "requested_components"),
      rank_disposition = triomics_report_text(rank, "disposition", "not_reported"),
      preparation_context_id = triomics_report_identifier_value(
        payload$preparation_context_id %||% "not_reported", "C.preparation_context_id"),
      representation_id = triomics_report_identifier_value(
        payload$representation_id %||% "not_reported", "C.representation_id"),
      selection_rule = triomics_report_identifier(selection, "rule", "not_reported"),
      selected_feature_count = if (is.finite(selected_from_rank)) as.integer(selected_from_rank) else
        as.integer(selected_count),
      retained_components = triomics_report_number(params, "retained_n_components"),
      stability_status = comparison_status,
      stability_comparison_count = as.integer(length(pairs)),
      stability_reason_code = triomics_report_reason(comparisons, "reason"),
      variance_metric = triomics_report_identifier(variance, "metric", "not_reported"),
      variance_assay_space = triomics_report_identifier(variance, "assay_space", "not_reported"),
      variance_denominator = triomics_report_identifier(variance, "denominator", "not_reported"),
      variance_semantics = triomics_report_identifier(variance, "semantics", "not_reported")
    )
  })
  if (!length(records)) {
    return(triomics_report_empty_frame(
      c("integration_id", "method", "status", "execution_status", "evaluation_status",
        "evaluation_source_path", "evaluation_source_sha256", "single_fit_status",
        "single_fit_reason_code", "required", "eligibility_status",
        "training_unit_count", "maximum_centered_rank", "numerical_rank", "requested_components",
        "rank_disposition", "preparation_context_id", "representation_id", "selection_rule",
        "selected_feature_count", "retained_components", "stability_status",
        "stability_comparison_count", "stability_reason_code", "variance_metric",
        "variance_assay_space", "variance_denominator", "variance_semantics"),
      numeric_columns = c("training_unit_count", "maximum_centered_rank", "numerical_rank",
                          "requested_components", "selected_feature_count", "retained_components",
                          "stability_comparison_count"),
      logical_columns = "required"
    ))
  }
  result <- as.data.frame(do.call(rbind, lapply(records, function(record) as.data.frame(record,
                                                                                         stringsAsFactors = FALSE))),
                          stringsAsFactors = FALSE, check.names = FALSE)
  rownames(result) <- NULL
  result
}

triomics_report_integration_variance_rows <- function(payload) {
  integrations <- payload$integrations %||% list()
  if (!is.list(integrations)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C integration results are not a typed record array.",
                          "C.integrations")
  }
  records <- list()
  for (integration in integrations) {
    integration_id <- triomics_report_identifier(integration, "integration_id")
    result <- integration$result %||% list()
    variance <- result$variance %||% list()
    row_ids <- variance$rows$block_ids %||% list()
    column_ids <- variance$columns$component_ids %||% list()
    values <- variance$values %||% list()
    if (!length(row_ids) && !length(column_ids) && !length(values)) next
    if (!is.list(row_ids) || !is.list(column_ids) || !is.list(values) ||
        length(row_ids) != length(values) || any(lengths(values) != length(column_ids))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "C variance dimensions and values do not align.",
                            "C.variance")
    }
    for (row_index in seq_along(row_ids)) {
      for (column_index in seq_along(column_ids)) {
        value <- values[[row_index]][[column_index]]
        variance_value <- if (is.null(value)) NA_real_ else {
          if (!is.numeric(value) || length(value) != 1L || is.nan(value) || is.infinite(value)) {
            triomics_report_abort("D_REPORT_SOURCE_INVALID", "A C variance value is not a finite number or null.",
                                  "C.variance.values")
          }
          as.numeric(value)
        }
        records[[length(records) + 1L]] <- data.frame(
          integration_id = integration_id,
          method = triomics_report_identifier(integration, "method", "not_reported"),
          block_id = triomics_report_identifier_value(row_ids[[row_index]], "C.variance.block_id"),
          component_id = triomics_report_identifier_value(column_ids[[column_index]], "C.variance.component_id"),
          variance_value = variance_value,
          variance_metric = triomics_report_identifier(variance, "metric", "not_reported"),
          variance_assay_space = triomics_report_identifier(variance, "assay_space", "not_reported"),
          variance_denominator = triomics_report_identifier(variance, "denominator", "not_reported"),
          variance_semantics = triomics_report_identifier(variance, "semantics", "not_reported"),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    }
  }
  if (!length(records)) {
    return(triomics_report_empty_frame(
      c("integration_id", "method", "block_id", "component_id", "variance_value",
        "variance_metric", "variance_assay_space", "variance_denominator", "variance_semantics"),
      numeric_columns = "variance_value"
    ))
  }
  frame <- do.call(rbind, records)
  rownames(frame) <- NULL
  frame
}

triomics_report_integration_eligibility_rows <- function(payload) {
  integrations <- payload$integrations %||% list()
  if (!is.list(integrations)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C integration results are not a typed record array.",
                          "C.integrations")
  }
  records <- list()
  for (integration in integrations) {
    params <- integration$result$params %||% list()
    feature_flow <- params$feature_flow %||% list()
    if (!is.list(feature_flow)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "C feature eligibility flow is not a typed record map.",
                            "C.feature_flow")
    }
    for (index in seq_along(feature_flow)) {
      block <- feature_flow[[index]]
      if (!is.list(block)) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "C block eligibility flow is not a typed record.",
                              "C.feature_flow")
      }
      input <- block$input %||% list()
      selected <- block$selected %||% list()
      retained <- block$retained %||% selected
      zero_variance <- block$zero_variance %||% list()
      excluded <- block$excluded_by_selection %||% list()
      if (!is.list(input) || !is.list(selected) || !is.list(retained) ||
          !is.list(zero_variance) || !is.list(excluded)) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "C feature eligibility counts are malformed.",
                              "C.feature_flow")
      }
      records[[length(records) + 1L]] <- data.frame(
        integration_id = triomics_report_identifier(integration, "integration_id"),
        method = triomics_report_identifier(integration, "method", "not_reported"),
        block_id = triomics_report_identifier_value(names(feature_flow)[[index]], "C.feature_flow.block_id"),
        eligibility_status = "source_reported",
        input_feature_count = as.integer(length(input)),
        selected_feature_count = as.integer(length(selected)),
        retained_feature_count = as.integer(length(retained)),
        zero_variance_feature_count = as.integer(length(zero_variance)),
        excluded_by_selection_count = as.integer(length(excluded)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  columns <- c("integration_id", "method", "block_id", "eligibility_status", "input_feature_count",
               "selected_feature_count", "retained_feature_count", "zero_variance_feature_count",
               "excluded_by_selection_count")
  if (!length(records)) {
    return(triomics_report_empty_frame(columns,
      numeric_columns = columns[5:9]
    ))
  }
  frame <- do.call(rbind, records)
  rownames(frame) <- NULL
  frame[, columns, drop = FALSE]
}

triomics_report_route_rows <- function(d, config) {
  routes <- d$routes %||% list()
  if (!is.list(routes)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "D route statuses are not a typed record map.", "D.routes")
  }
  route_names <- unique(c(triomics_d_route_names(), names(routes)))
  records <- lapply(route_names, function(name) {
    route <- routes[[name]] %||% list(status = "not_requested", reason_code = "D_ROUTE_DISABLED")
    nominations <- route$nominations %||% list()
    list(route = triomics_report_clean_text(name), status = as.character(route$status %||% "unavailable"),
         required = triomics_report_requirement(config$route_requirements, name),
         reason_code = triomics_report_reason(route, "reason_code"),
         candidate_count = triomics_report_number(route, "candidate_count"),
         nominated_count = as.integer(length(nominations)),
         mapped_entity_count = NA_real_, coverage_status = "mapping_detail_not_exported")
  })
  as.data.frame(do.call(rbind, lapply(records, function(record) as.data.frame(record,
                                                                               stringsAsFactors = FALSE))),
                stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_enrichment_rows <- function(d) {
  enrichment <- d$enrichment %||% list()
  summaries <- enrichment$results %||% list()
  if (!is.list(summaries)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "D enrichment summaries are not a typed record array.",
                          "D.enrichment.results")
  }
  route_claims <- d$routes$ora$claims %||% list()
  if (!is.list(route_claims)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "D ORA claim detail is not a typed record array.",
                          "D.routes.ora.claims")
  }
  identity_fields <- c("study_id", "analysis_id", "preparation_context_id", "representation_id",
                       "block_id", "source_branch", "claim_id", "integration_id", "family_id")
  identity_key <- function(identity, key) {
    if (!is.list(identity) || is.null(names(identity))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "An enrichment claim identity is malformed.", key)
    }
    safe <- lapply(identity_fields, function(field) {
      triomics_report_identifier(identity, field, if (identical(field, "integration_id")) "not_applicable" else "not_reported")
    })
    names(safe) <- identity_fields
    triomics_canonical_json(safe)
  }
  records <- list()
  for (summary in summaries) {
    if (!is.list(summary) || is.null(names(summary))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A D enrichment summary is not a named record.",
                            "D.enrichment.results")
    }
    identity <- summary$identity
    key <- identity_key(identity, "D.enrichment.identity")
    details <- Filter(function(claim) {
      is.list(claim) && !is.null(names(claim)) &&
        identical(identity_key(claim$identity, "D.routes.ora.claims.identity"), key)
    }, route_claims)
    if (length(details) > 1L) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "An enrichment claim has duplicate source detail.",
                            "D.routes.ora.claims")
    }
    detail <- if (length(details)) details[[1L]] else NULL
    detail_bindings <- detail$bindings %||% list()
    protocol <- detail$protocol %||% list()
    mapping <- detail$mapping %||% list()
    universe <- detail$measured_universe %||% list()
    set_family <- detail$set_family %||% list()
    identity_values <- lapply(identity_fields, function(field) {
      triomics_report_identifier(identity, field,
        if (identical(field, "integration_id")) "not_applicable" else "not_reported")
    })
    names(identity_values) <- identity_fields
    source_ids <- triomics_report_identifier_collection(identity$source_ids, "D.enrichment.source_ids")
    nomination_ids <- triomics_report_identifier_collection(identity$nomination_ids,
                                                             "D.enrichment.nomination_ids")
    protocol_sha <- triomics_report_identifier(protocol, "sha256",
      triomics_report_identifier(detail_bindings, "protocol_sha256", "not_reported"))
    mapping_sha <- triomics_report_identifier(detail_bindings, "mapping_sha256", "not_reported")
    universe_sha <- triomics_report_identifier(universe, "sha256",
      triomics_report_identifier(detail_bindings, "measured_universe_sha256", "not_reported"))
    set_family_sha <- triomics_report_identifier(set_family, "sha256",
      triomics_report_identifier(detail_bindings, "set_family_sha256", "not_reported"))
    mapping_resource <- mapping$resource %||% list()
    mapping_resource_id <- triomics_report_identifier(mapping_resource, "resource_id",
      triomics_report_identifier(mapping_resource, "artifact_id", "not_reported"))
    mapping_resource_version <- triomics_report_identifier(mapping_resource, "version", "not_reported")
    protocol_resource_id <- triomics_report_identifier(protocol, "resource_id", "not_reported")
    mapping_rule <- triomics_report_identifier(protocol, "mapping_rule", "not_reported")
    aggregation_rule <- triomics_report_identifier(protocol, "aggregation_rule", "not_reported")
    ambiguity_rule <- triomics_report_identifier(protocol, "ambiguity_rule", "not_reported")
    universe_source <- triomics_report_identifier(universe, "source", "not_reported")
    interpretation <- triomics_report_identifier_value(
      detail$interpretation %||% summary$interpretation,
      "D.enrichment.interpretation", "not_reported"
    )
    claim_context_sha <- if (!is.null(detail)) triomics_hash_text(triomics_canonical_json(list(
      identity = identity_values, protocol_sha256 = protocol_sha,
      mapping_sha256 = mapping_sha, measured_universe_sha256 = universe_sha,
      set_family_sha256 = set_family_sha
    ))) else ""
    family_size <- triomics_report_number(summary, "family_size")
    tests_in_bh <- triomics_report_number(summary, "tests_in_bh")
    sets <- summary$sets %||% list()
    if (!is.list(sets)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "D enrichment sets are not a typed record array.",
                            "D.enrichment.sets")
    }
    for (set in sets) {
      if (!is.list(set) || is.null(names(set))) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "A D enrichment result is not a named record.",
                              "D.enrichment.sets")
      }
      records[[length(records) + 1L]] <- data.frame(
        study_id = identity_values$study_id,
        analysis_id = identity_values$analysis_id,
        preparation_context_id = identity_values$preparation_context_id,
        representation_id = identity_values$representation_id,
        block_id = identity_values$block_id,
        source_branch = identity_values$source_branch,
        claim_id = identity_values$claim_id,
        integration_id = identity_values$integration_id,
        family_id = identity_values$family_id,
        source_ids = source_ids,
        nomination_ids = nomination_ids,
        protocol_resource_id = protocol_resource_id,
        protocol_sha256 = protocol_sha,
        mapping_resource_id = mapping_resource_id,
        mapping_resource_version = mapping_resource_version,
        mapping_sha256 = mapping_sha,
        mapping_rule = mapping_rule,
        aggregation_rule = aggregation_rule,
        ambiguity_rule = ambiguity_rule,
        universe_source = universe_source,
        measured_universe_sha256 = universe_sha,
        set_family_sha256 = set_family_sha,
        interpretation = interpretation,
        claim_context_sha256 = claim_context_sha,
        lineage_status = if (is.null(detail)) "detail_not_reported" else "source_reported",
        set_id = triomics_report_identifier(set, "set_id", "not_reported"),
        status = triomics_report_reason(set, "status", "not_reported"),
        population_N = triomics_report_number(set, "N"),
        set_members_K = triomics_report_number(set, "K"),
        nominated_n = triomics_report_number(set, "n"),
        overlap_k = triomics_report_number(set, "k"),
        p = triomics_report_number(set, "p"),
        q = triomics_report_number(set, "q"),
        family_size = family_size,
        tests_in_bh = tests_in_bh,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  columns <- c("study_id", "analysis_id", "preparation_context_id", "representation_id", "block_id",
               "source_branch", "claim_id", "integration_id", "family_id", "source_ids",
               "nomination_ids", "protocol_resource_id", "protocol_sha256", "mapping_resource_id",
               "mapping_resource_version", "mapping_sha256", "mapping_rule", "aggregation_rule",
               "ambiguity_rule", "universe_source",
               "measured_universe_sha256", "set_family_sha256", "interpretation", "claim_context_sha256",
               "lineage_status", "set_id", "status", "population_N", "set_members_K", "nominated_n",
               "overlap_k", "p", "q", "family_size", "tests_in_bh")
  if (!length(records)) {
    return(triomics_report_empty_frame(columns, numeric_columns = c("population_N", "set_members_K",
      "nominated_n", "overlap_k", "p", "q", "family_size", "tests_in_bh")))
  }
  frame <- do.call(rbind, records)
  rownames(frame) <- NULL
  frame[, columns, drop = FALSE]
}

triomics_report_flow_rows <- function(config, results, assay_summary) {
  records <- lapply(seq_along(config$blocks), function(index) {
    block <- config$blocks[[index]]
    matches <- if (nrow(results)) results[results$block_id == block$block_id, , drop = FALSE] else results
    sample_counts <- if (nrow(matches)) unique(matches$n_samples[is.finite(matches$n_samples)]) else numeric()
    summary <- assay_summary[assay_summary$block_id == block$block_id, , drop = FALSE]
    list(
      block_id = as.character(block$block_id),
      block_name = paste("Block", index),
      profile = block$profile,
      quantity = block$quantity,
      scale = block$scale,
      units = block$units,
      preparation_context_id = block$preparation_context_id,
      transform = block$transform,
      normalization = block$normalization,
      imputation = block$imputation,
      source_sample_count = as.integer(summary$source_sample_count[[1L]]),
      source_feature_count = as.integer(summary$source_feature_count[[1L]]),
      observed_cell_count = as.integer(summary$observed_cell_count[[1L]]),
      missing_cell_count = as.integer(summary$missing_cell_count[[1L]]),
      tested_result_rows = as.integer(nrow(matches)),
      tested_feature_count = as.integer(length(unique(matches$feature_key))),
      tested_model_sample_count_min = if (length(sample_counts)) min(sample_counts) else NA_real_,
      tested_model_sample_count_max = if (length(sample_counts)) max(sample_counts) else NA_real_
    )
  })
  if (!length(records)) return(data.frame())
  as.data.frame(do.call(rbind, lapply(records, function(record) as.data.frame(record,
                                                                               stringsAsFactors = FALSE))),
                stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_section <- function(id, status, required = FALSE, reason_code = "") {
  list(section_id = as.character(id), status = as.character(status),
       required = isTRUE(required), reason_code = as.character(reason_code %||% ""))
}

triomics_report_optional_sections <- function(loaded, config) {
  definitions <- list(
    targets = list(function_name = "triomics_report_targets_section", enabled = config$target_enabled,
                   unavailable_reason = "D_REPORT_TARGET_SECTION_NOT_IMPLEMENTED"),
    clinical = list(function_name = "triomics_report_clinical_section", enabled = config$clinical_enabled,
                    unavailable_reason = "D_REPORT_CLINICAL_SECTION_NOT_IMPLEMENTED")
  )
  result <- lapply(names(definitions), function(section_name) {
    definition <- definitions[[section_name]]
    fallback <- list(
      status = if (definition$enabled) "unavailable" else "not_requested",
      reason_code = if (definition$enabled) definition$unavailable_reason else "",
      tables = list()
    )
    provider <- get0(definition$function_name,
                     envir = environment(),
                     mode = "function", inherits = TRUE)
    if (!is.function(provider)) return(fallback)
    value <- tryCatch(provider(loaded, config), error = function(error) NULL)
    required <- c("status", "reason_code", "tables")
    if (!is.list(value) || is.null(names(value)) ||
        !identical(sort(names(value)), sort(required)) ||
        !is.character(value$status) || length(value$status) != 1L || is.na(value$status) ||
        !value$status %in% c("complete", "partial", "unavailable", "not_requested") ||
        !is.character(value$reason_code) || length(value$reason_code) != 1L || is.na(value$reason_code) ||
        !is.list(value$tables) || (!is.null(names(value$tables)) && any(!nzchar(names(value$tables)))) ||
        anyDuplicated(names(value$tables))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A trusted report section provider returned an invalid table contract.",
                            paste0("report.sections.", section_name))
    }
    if (length(value$tables) && any(!vapply(value$tables, is.data.frame, logical(1)))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A trusted report section provider returned a non-table value.",
                            paste0("report.sections.", section_name))
    }
    value$tables <- lapply(value$tables, function(frame) {
      if (anyDuplicated(names(frame)) || any(!nzchar(names(frame)))) {
        triomics_report_abort("D_REPORT_SOURCE_INVALID", "A report section table has invalid column names.",
                              paste0("report.sections.", section_name))
      }
      frame
    })
    value$reason_code <- triomics_report_reason(value, "reason_code")
    value
  })
  names(result) <- names(definitions)
  result
}

triomics_report_optional_section_html <- function(section) {
  if (!is.list(section) || !is.list(section$tables)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "A report section projection is malformed.", "report.sections")
  }
  if (!length(section$tables)) {
    return(paste0("<p>Status: <code>", triomics_report_html_escape(section$status), "</code>. ",
                  triomics_report_html_escape(section$reason_code), "</p>"))
  }
  parts <- unlist(lapply(names(section$tables), function(name) {
    frame <- section$tables[[name]]
    c(paste0("<h3>", triomics_report_html_escape(name), "</h3>"),
      triomics_report_html_table(frame))
  }), use.names = FALSE)
  paste0("<p>Status: <code>", triomics_report_html_escape(section$status), "</code>.</p>",
         paste(parts, collapse = "\n"))
}

triomics_report_conditional_capabilities <- function(routes, c07_status, c09,
                                                    extended_sections = NULL) {
  targets <- extended_sections$targets %||% list(status = "unavailable")
  clinical <- extended_sections$clinical %||% list(status = "unavailable")
  c09_status <- as.character(c09$status %||% "unavailable")
  if (isTRUE(c09$benefit_guaranteed)) {
    triomics_report_abort("D_REPORT_SOURCE_INVALID", "C-09 cannot claim guaranteed benefit.", "C09.benefit_guaranteed")
  }
  data.frame(
    capability = c("MOFA native", "BioPAN", "C-07 controls", "C-09 clinical prediction",
                   "D-T13 target dossier", "D-T14 quantitative target method", "D-T15 clinical report"),
    status = c(routes$status[match("mofa_native", routes$route)],
               routes$status[match("biopan", routes$route)], c07_status, c09_status,
               as.character(targets$status), "conditional_unapproved", as.character(clinical$status)),
    scope = c("D-T06", "D-T07", "C-07", as.character(c09$status_scope %||% c09$scope %||%
                                                         "synthetic_fixture_only"),
              "D-T13", "D-T14", "D-T15"),
    completion_policy = c("conditional", "conditional", "not_applicable",
                          as.character(c09$completion_policy %||% "conditional"),
                          "conditional", "conditional", "conditional"),
    qualification_status = c("unqualified", "unqualified", "not_applicable",
                             as.character(c09$qualification_status %||% "unqualified"),
                             "not_evaluated", "unapproved", "not_evaluated"),
    benefit_guaranteed = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    model_execution = c("not_reported", "not_reported", "not_applicable",
                        as.character(c09$model_execution %||% c09$status %||% "not_started"),
                        "not_applicable", "not_started", "not_applicable"),
    project_acceptance_status = rep("not_assessed", 7L),
    stringsAsFactors = FALSE
  )
}

triomics_report_optional_section_notes <- function(extended_sections) {
  notes <- character()
  for (entry in list(
    list(packet = "D-T13", name = "target dossier", value = extended_sections$targets),
    list(packet = "D-T15", name = "clinical report", value = extended_sections$clinical)
  )) {
    section <- entry$value %||% list(status = "unavailable", reason_code = "")
    status <- triomics_report_identifier(section, "status", "unavailable")
    if (status %in% c("unavailable", "partial")) {
      reason <- triomics_report_reason(section, "reason_code")
      suffix <- if (nzchar(reason)) paste0("; reason code ", reason) else ""
      notes <- c(notes, paste0(entry$packet, " ", entry$name, " report section status: ",
                               status, suffix, ". Project acceptance is not assessed."))
    }
  }
  notes
}

triomics_report_sections <- function(config, loaded, results, clinical, integrations, routes, flow,
                                     extended_sections = NULL) {
  d <- loaded$d
  clinical_status <- if (!config$clinical_enabled) "not_requested" else if (is.null(loaded$payloads$clinical)) {
    "unavailable"
  } else as.character(loaded$payloads$clinical$status %||% "unavailable")
  target <- d$target_investigation %||% list()
  target_status <- if (!config$target_enabled) "not_requested" else as.character(target$status %||% "unavailable")
  route_required <- any(routes$required)
  route_status <- if (!nrow(routes) || all(routes$status == "not_requested")) "not_requested" else if (
    any(routes$status %in% c("unavailable", "failed", "partial"))) "partial" else "complete"
  integration_required <- any(integrations$required)
  integration_status <- if (!nrow(integrations) || all(integrations$status == "not_requested")) {
    "not_requested"
  } else if (any(integrations$required & integrations$status != "complete")) {
    "unavailable"
  } else if (any(integrations$status %in% c("unavailable", "failed", "partial"))) {
    "partial"
  } else "complete"
  d_status <- if (identical(as.character(d$route_status$run_completeness), "incomplete")) "partial" else
    as.character(d$status %||% "unavailable")
  stage_statuses <- list(
    A = as.character(loaded$envelopes$a$status),
    B = as.character(loaded$payloads$b$status %||% "unavailable"),
    C = as.character(loaded$payloads$c$status %||% "unavailable"),
    D = d_status,
    clinical = clinical_status,
    target = target_status
  )
  c07 <- d$inherited_evidence$evaluation_evidence$controls %||% list()
  c07_status <- if (length(c07)) {
    if (all(vapply(c07, function(control) identical(as.character(control$status), "not_run"), logical(1)))) {
      "not_run"
    } else "reported"
  } else "unavailable"
  c09 <- d$inherited_evidence$stage_status$clinical_prediction %||% list()
  c09_status <- as.character(c09$status %||% "unavailable")
  flow_status <- if (nrow(flow) == length(config$blocks) && nrow(flow) > 0L &&
                     all(is.finite(flow$source_sample_count)) &&
                     all(is.finite(flow$source_feature_count)) &&
                     all(flow$source_sample_count > 0L) &&
                     all(flow$source_feature_count > 0L)) "complete" else "partial"
  extended_sections <- extended_sections %||% list(
    targets = list(status = if (config$target_enabled) "unavailable" else "not_requested",
                   reason_code = if (config$target_enabled) "D_REPORT_TARGET_SECTION_NOT_IMPLEMENTED" else "",
                   tables = list()),
    clinical = list(status = if (config$clinical_enabled) "unavailable" else "not_requested",
                    reason_code = if (config$clinical_enabled) "D_REPORT_CLINICAL_SECTION_NOT_IMPLEMENTED" else "",
                    tables = list())
  )
  required_sections <- list(
    triomics_report_section("run_config_environment", "complete", TRUE),
    triomics_report_section("A", stage_statuses$A, TRUE),
    triomics_report_section("B", stage_statuses$B, TRUE),
    triomics_report_section("C", stage_statuses$C, TRUE),
    triomics_report_section("D", stage_statuses$D, TRUE,
                            if (identical(d_status, "partial")) "D_REQUIRED_ROUTE_INCOMPLETE" else ""),
    triomics_report_section("source_sample_feature_flow",
                            flow_status, TRUE,
                            if (flow_status == "complete") "" else "D_REPORT_ASSAY_DIMENSIONS_ABSENT"),
    triomics_report_section("assay_qc_preparation", "unavailable", FALSE,
                            "D_REPORT_QC_EXPORT_ABSENT"),
    triomics_report_section("estimands_contrasts",
                            if (length(config$analysis$contrasts)) "complete" else "unavailable", TRUE,
                            if (length(config$analysis$contrasts)) "" else "D_REPORT_ESTIMANDS_ABSENT"),
    triomics_report_section("full_block_results",
                            if (nrow(results)) stage_statuses$B else "unavailable", TRUE,
                            if (nrow(results)) "" else "D_REPORT_RESULTS_ABSENT"),
    triomics_report_section("integration_eligibility_variance_selection_stability",
                            integration_status, integration_required,
                            if (integration_status %in% c("unavailable", "partial")) "D_REPORT_INTEGRATION_INCOMPLETE" else ""),
    triomics_report_section("evaluation", "partial", FALSE, "D_REPORT_CONDITIONAL_EVALUATION"),
    triomics_report_section("clinical_objectives", clinical_status, config$clinical_required,
                            if (clinical_status == "unavailable") "D_REPORT_CLINICAL_EXPORT_MISSING" else ""),
    triomics_report_section("clinical_report_details", extended_sections$clinical$status,
                            config$clinical_required,
                            extended_sections$clinical$reason_code),
    triomics_report_section("mapping_enrichment_coverage", route_status, route_required,
                            if (route_status == "partial") "D_REPORT_ROUTE_UNAVAILABLE" else ""),
    triomics_report_section("target_evidence_alternatives", target_status, config$target_required,
                            if (target_status == "unavailable") "D_REPORT_TARGET_UNAVAILABLE" else ""),
    triomics_report_section("target_dossier_details", extended_sections$targets$status,
                            config$target_required,
                            extended_sections$targets$reason_code),
    triomics_report_section("unavailable_work", "complete", FALSE),
    triomics_report_section("limitations_reproducible_commands", "complete", FALSE)
  )
  list(stage_statuses = stage_statuses, sections = required_sections,
       conditional_statuses = list(C07 = c07_status, C09 = c09_status),
       extended_sections = extended_sections)
}

triomics_report_svg_panel <- function(rows, panel_index) {
  effects <- rows$effect
  se_column <- if (any(is.finite(rows$ordinary_se))) "ordinary_se" else
    if (any(is.finite(rows$moderated_se))) "moderated_se" else ""
  errors <- if (nzchar(se_column)) rows[[se_column]] else rep(NA_real_, length(effects))
  finite <- is.finite(effects)
  effects <- effects[finite]
  errors <- errors[finite]
  blocks <- rows$block_id[finite]
  if (!length(effects)) return("")
  lower <- effects - ifelse(is.finite(errors), errors, 0)
  upper <- effects + ifelse(is.finite(errors), errors, 0)
  span <- range(c(lower, upper), finite = TRUE)
  padding <- max(diff(span) * 0.08, abs(span[[1L]]) * 0.05, 1e-6)
  span <- span + c(-padding, padding)
  x_left <- 60; x_right <- 690; y_top <- 38; y_bottom <- 330
  x <- if (length(effects) == 1L) (x_left + x_right) / 2 else
    seq(x_left + 8, x_right - 8, length.out = length(effects))
  y <- y_bottom - (effects - span[[1L]]) / diff(span) * (y_bottom - y_top)
  y_low <- y_bottom - (lower - span[[1L]]) / diff(span) * (y_bottom - y_top)
  y_high <- y_bottom - (upper - span[[1L]]) / diff(span) * (y_bottom - y_top)
  ticks <- pretty(span, n = 4)
  tick_y <- y_bottom - (ticks - span[[1L]]) / diff(span) * (y_bottom - y_top)
  palette <- c("#245b8f", "#9b4f2e", "#357253", "#7a5791", "#8a721c", "#4b6974")
  block_levels <- unique(blocks)
  color_index <- match(blocks, block_levels)
  elements <- c(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 740 390" role="img" aria-label="Source-reported effect estimates with standard-error bars">',
    '<title>Source-reported effect estimates. Row order matches results.tsv.</title>',
    '<rect width="740" height="390" fill="#ffffff"/>',
    paste0('<text x="60" y="20" font-size="14" font-weight="600">Effect unit group ', panel_index, '</text>'),
    paste0('<line x1="', x_left, '" y1="', y_bottom, '" x2="', x_right,
           '" y2="', y_bottom, '" stroke="#58636b"/>'),
    paste0('<line x1="', x_left, '" y1="', y_top, '" x2="', x_left,
           '" y2="', y_bottom, '" stroke="#58636b"/>'),
    paste0('<text x="375" y="377" text-anchor="middle" font-size="12">Result row index</text>'),
    paste0('<text x="16" y="185" text-anchor="middle" font-size="12" transform="rotate(-90 16 185)">Effect on source-reported scale</text>')
  )
  for (i in seq_along(ticks)) {
    if (is.finite(tick_y[[i]]) && tick_y[[i]] >= y_top && tick_y[[i]] <= y_bottom) {
      elements <- c(elements,
        paste0('<line x1="', x_left - 4, '" y1="', tick_y[[i]], '" x2="', x_right,
               '" y2="', tick_y[[i]], '" stroke="#e3e7e9"/>'),
        paste0('<text x="', x_left - 8, '" y="', tick_y[[i]] + 4,
               '" text-anchor="end" font-size="10">', format(ticks[[i]], digits = 4), '</text>')
      )
    }
  }
  for (i in seq_along(effects)) {
    color <- palette[((color_index[[i]] - 1L) %% length(palette)) + 1L]
    if (is.finite(errors[[i]])) {
      elements <- c(elements,
        paste0('<line x1="', x[[i]], '" y1="', y_low[[i]], '" x2="', x[[i]],
               '" y2="', y_high[[i]], '" stroke="', color, '" stroke-width="1.4"/>'),
        paste0('<line x1="', x[[i]] - 3, '" y1="', y_low[[i]], '" x2="', x[[i]] + 3,
               '" y2="', y_low[[i]], '" stroke="', color, '"/>'),
        paste0('<line x1="', x[[i]] - 3, '" y1="', y_high[[i]], '" x2="', x[[i]] + 3,
               '" y2="', y_high[[i]], '" stroke="', color, '"/>')
      )
    }
    elements <- c(elements, paste0('<circle cx="', x[[i]], '" cy="', y[[i]],
                                    '" r="3.2" fill="', color, '"/>'))
  }
  legend_y <- 348
  for (i in seq_along(block_levels)) {
    x_legend <- x_left + (i - 1L) * 130
    color <- palette[((i - 1L) %% length(palette)) + 1L]
    elements <- c(elements,
      paste0('<circle cx="', x_legend, '" cy="', legend_y, '" r="3.2" fill="', color, '"/>'),
      paste0('<text x="', x_legend + 8, '" y="', legend_y + 4,
             '" font-size="10">Block ', i, '</text>')
    )
  }
  paste0(paste(elements, collapse = ""), "</svg>")
}

triomics_report_plots <- function(results) {
  if (!nrow(results)) return(list(html = "<p>No result rows were exported, so no effect plot is available.</p>",
                                  count = 0L))
  finite <- is.finite(results$effect) & nzchar(results$effect_unit)
  if (!any(finite)) return(list(html = "<p>No finite source-reported effects are available for plotting.</p>",
                                count = 0L))
  rows <- results[finite, , drop = FALSE]
  units <- unique(rows$effect_unit)
  panels <- vapply(seq_along(units), function(index) {
    panel_rows <- rows[rows$effect_unit == units[[index]], , drop = FALSE]
    se_label <- if (any(is.finite(panel_rows$ordinary_se))) "ordinary standard error" else
      if (any(is.finite(panel_rows$moderated_se))) "moderated standard error" else "no source-reported SE"
    paste0("<h4>Effect unit group ", index, "</h4><p>Source effect_unit: <code>",
           triomics_report_html_escape(units[[index]]), "</code>. Error bars use ",
           se_label, " when present. No confidence intervals are inferred.</p>",
           triomics_report_svg_panel(panel_rows, index))
  }, character(1))
  list(html = paste(panels, collapse = "\n"), count = length(units))
}

triomics_report_section_frame <- function(sections) {
  as.data.frame(do.call(rbind, lapply(sections, function(section) {
    data.frame(section_id = section$section_id, status = section$status,
               required = section$required, reason_code = section$reason_code,
               stringsAsFactors = FALSE)
  })), stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_target_summary <- function(d, enabled) {
  target <- d$target_investigation %||% list()
  status <- if (!enabled) "not_requested" else as.character(target$status %||% "unavailable")
  data.frame(section = "target_investigation", status = status,
             reason_code = triomics_report_reason(target, "reason_code"),
             evidence_record_count = if (is.list(target$results)) length(target$results) else 0L,
             stringsAsFactors = FALSE)
}

triomics_report_runtime <- function(quarto_path) {
  version <- tryCatch(system2(quarto_path, "--version", stdout = TRUE, stderr = TRUE,
                              env = paste0("TMPDIR=", Sys.getenv("TMPDIR"))),
                      error = function(error) NULL)
  status <- if (is.null(version)) 1L else {
    value <- attr(version, "status")
    if (is.null(value)) 0L else as.integer(value)
  }
  if (!identical(status, 0L) || !length(version) || !grepl("^[0-9]+[.][0-9]+", version[[1L]])) {
    triomics_report_abort("D_REPORT_RUNTIME_MISSING", "The local Quarto runtime did not report a version.",
                          "QUARTO_PATH")
  }
  package_version <- tryCatch(as.character(utils::packageVersion("triomics")), error = function(error) "unknown")
  list(R = as.character(getRversion()), package = package_version,
       quarto = as.character(version[[1L]]), platform = as.character(R.version$platform))
}

triomics_report_project <- function(loaded, config, quarto_version) {
  assay_summary <- triomics_report_assay_summary(loaded$payloads$a, config)
  results <- triomics_report_rows(loaded$payloads$b, config$blocks)
  clinical <- if (is.null(loaded$payloads$clinical)) {
    triomics_report_clinical_rows(list(results = list()))
  } else triomics_report_clinical_rows(loaded$payloads$clinical)
  integrations <- triomics_report_integration_rows(
    loaded$payloads$c, config, loaded$d, loaded$artifacts$c$payload_sha256
  )
  integration_variance <- triomics_report_integration_variance_rows(loaded$payloads$c)
  integration_eligibility <- triomics_report_integration_eligibility_rows(loaded$payloads$c)
  routes <- triomics_report_route_rows(loaded$d, config)
  enrichment <- triomics_report_enrichment_rows(loaded$d)
  flow <- triomics_report_flow_rows(config, results, assay_summary)
  extended_sections <- triomics_report_optional_sections(loaded, config)
  status <- triomics_report_sections(config, loaded, results, clinical, integrations, routes,
                                     flow, extended_sections)
  status_frame <- triomics_report_section_frame(status$sections)
  required_incomplete <- any(status_frame$required & status_frame$status != "complete")
  report_status <- if (required_incomplete) "partial" else "complete"
  c07 <- status$conditional_statuses$C07
  c09 <- status$conditional_statuses$C09
  unavailable <- character()
  if (any(routes$status %in% c("unavailable", "failed", "partial"))) {
    unavailable <- c(unavailable, "One or more requested D annotation routes are unavailable or partial.")
  }
  if (identical(status$stage_statuses$clinical, "unavailable")) {
    unavailable <- c(unavailable, "Clinical objectives were requested but no typed clinical association export is available.")
  }
  if (identical(status$stage_statuses$target, "unavailable")) {
    unavailable <- c(unavailable, "The requested target investigation is unavailable.")
  }
  unavailable <- c(unavailable,
                   paste0("C-07 controls status: ", c07, "."),
                   paste0("C-09 clinical prediction status: ", c09, "."),
                   triomics_report_optional_section_notes(extended_sections),
                   "D-T14 quantitative target method remains conditional and unapproved.")
  c09_record <- loaded$d$inherited_evidence$stage_status$clinical_prediction %||% list()
  conditional <- triomics_report_conditional_capabilities(routes, c07, c09_record, extended_sections)
  if (anyNA(conditional$status)) conditional$status[is.na(conditional$status)] <- "unavailable"
  if (anyNA(conditional$status)) conditional$status[is.na(conditional$status)] <- "unavailable"
  notice <- switch(config$audience,
    synthetic_demo = "Synthetic demonstration report. It contains no participant data.",
    private_research = "Private local research report. Keep this output on the authorized research host.",
    approved_aggregate = "Aggregate report. No clinical records are included.",
    "Report audience is not reported."
  )
  commands <- c(
    "Rscript --vanilla inst/exec/triomics --config CONFIG.yaml --stage A --output OUTPUT",
    "Rscript --vanilla inst/exec/triomics --config CONFIG.yaml --stage B --output OUTPUT",
    "Rscript --vanilla inst/exec/triomics --config CONFIG.yaml --stage C --output OUTPUT",
    "Rscript --vanilla inst/exec/triomics --config CONFIG.yaml --stage D --output OUTPUT",
    "Rscript --vanilla inst/exec/triomics --config CONFIG.yaml --stage report --output OUTPUT"
  )
  lineage <- list(
    A = loaded$artifacts$a,
    B = loaded$artifacts$b,
    C = loaded$artifacts$c,
    D = loaded$artifacts$d,
    clinical = loaded$artifacts$clinical
  )
  list(
    schema_version = "1.0",
    study_id = config$study_id,
    report_status = report_status,
    audience = config$audience,
    audience_notice = notice,
    config_sha256 = config$config_sha256,
    stage_statuses = status$stage_statuses,
    sections = status$sections,
    source_flow = list(source_count = as.integer(config$source_count),
                       block_count = as.integer(length(config$blocks)), blocks = flow,
                       assay_summary = assay_summary),
    analysis_context = config$analysis,
    block_results = results,
    clinical_results = clinical,
    integrations = integrations,
    integration_variance = integration_variance,
    integration_eligibility = integration_eligibility,
    routes = routes,
    enrichment = enrichment,
    target = triomics_report_target_summary(loaded$d, config$target_enabled),
    extended_sections = extended_sections,
    conditional_capabilities = conditional,
    unavailable_work = unique(unavailable),
    limitations = c("No model is refitted by the report renderer.",
                    "D evidence is descriptive or unavailable where its producer status says so.",
                    "No missing binding, clinical, pathway, or integration evidence is biological absence."),
    reproducible_commands = commands,
    lineage = lineage,
    environment = quarto_version
  )
}

triomics_report_data_frame_records <- function(frame) {
  lapply(seq_len(nrow(frame)), function(index) {
    record <- lapply(frame, function(column) {
      value <- column[[index]]
      if (length(value) == 1L && is.na(value)) NULL else unname(value)
    })
    names(record) <- names(frame)
    record
  })
}

triomics_report_html_table <- function(frame, columns = names(frame)) {
  if (!is.data.frame(frame) || !length(columns) || !all(columns %in% names(frame))) {
    return("<p>No typed rows are available for this section.</p>")
  }
  frame <- frame[, columns, drop = FALSE]
  if (!nrow(frame)) return("<p>No rows were exported for this section.</p>")
  headers <- paste0("<th scope=\"col\">", triomics_report_html_escape(names(frame)), "</th>",
                    collapse = "")
  rows <- vapply(seq_len(nrow(frame)), function(row_index) {
    cells <- vapply(seq_along(frame), function(column_index) {
      value <- frame[[column_index]][[row_index]]
      if (length(value) == 1L && is.na(value)) value <- "Not reported"
      if (is.logical(value)) value <- if (isTRUE(value)) "yes" else "no"
      if (is.numeric(value)) value <- format(value, digits = 7, trim = TRUE, scientific = FALSE)
      paste0("<td>", triomics_report_html_escape(value), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0("<div class=\"table-scroll\"><table><thead><tr>", headers,
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

triomics_report_render_content <- function(data, plot_html) {
  section_table <- triomics_report_section_frame(data$sections)
  flow <- data$source_flow$blocks
  context <- data$analysis_context
  commands <- paste0("<li><code>", triomics_report_html_escape(data$reproducible_commands), "</code></li>",
                     collapse = "")
  unavailable <- paste0("<li>", triomics_report_html_escape(data$unavailable_work), "</li>",
                        collapse = "")
  limitations <- paste0("<li>", triomics_report_html_escape(data$limitations), "</li>", collapse = "")
  stage <- as.data.frame(do.call(rbind, lapply(names(data$stage_statuses), function(name) {
    data.frame(stage = name, status = data$stage_statuses[[name]], stringsAsFactors = FALSE)
  })), stringsAsFactors = FALSE)
  estimands <- data.frame(
    analysis_context = c("design_kind", "condition_type", "representation", "preparation_context_id", "statistics_method"),
    value = c(context$design_kind, context$condition_type, context$representation,
              context$preparation_context_id, context$statistics_method),
    stringsAsFactors = FALSE
  )
  contrasts <- as.data.frame(do.call(rbind, lapply(context$contrasts, function(contrast) {
    data.frame(contrast_id = contrast$contrast_id, effect_unit = contrast$effect_unit,
               stringsAsFactors = FALSE)
  })), stringsAsFactors = FALSE)
  if (!length(context$contrasts)) contrasts <- data.frame(contrast_id = character(), effect_unit = character())
  result_columns <- c("block_id", "block_name", "feature_key", "claim_type", "claim_id",
                      "contrast_id", "family_id", "family_member_hash", "family_claim_scope",
                      "family_correction_method", "family_n_declared", "family_n_eligible",
                      "family_n_evaluated", "family_n_failed", "effect", "effect_unit",
                      "ordinary_se", "moderated_se", "uncertainty_status", "interval_lower",
                      "interval_upper", "interval_method", "interval_status", "statistic",
                      "statistic_name", "ordinary_df", "effective_test_df", "p", "q",
                      "inferential_status", "estimation_status", "estimation_reason", "n_samples",
                      "n_subjects", "n_rows_used", "n_pairs", "direction_numerator",
                      "direction_denominator", "direction_zero_count", "direction_status",
                      "direction_reason", "count_prior_count", "count_prior_type",
                      "count_prior_metadata_sha256", "count_prior_status", "preparation_context_id",
                      "representation_id", "source_hash", "model_hash", "representation_hash",
                      "contrast_hash", "result_id")
  clinical_columns <- names(data$clinical_results)
  integration_columns <- names(data$integrations)
  route_columns <- names(data$routes)
  conditional_columns <- names(data$conditional_capabilities)
  target_columns <- names(data$target)
  paste0(
    "<h1>triomics analysis report</h1><p class=\"audience-notice\">",
    triomics_report_html_escape(data$audience_notice), "</p>",
    "<section><h2>Run, configuration, and environment</h2><p>Configuration SHA-256 <code>",
    triomics_report_html_escape(data$config_sha256), "</code></p>",
    triomics_report_html_table(stage),
    "<p>R ", triomics_report_html_escape(data$environment$R), "; triomics ",
    triomics_report_html_escape(data$environment$package), "; Quarto ",
    triomics_report_html_escape(data$environment$quarto), "; platform ",
    triomics_report_html_escape(data$environment$platform), ".</p></section>",
    "<section><h2>Section status</h2>", triomics_report_html_table(section_table), "</section>",
    "<section><h2>Source, sample, and feature flow</h2><p>Source records ",
    data$source_flow$source_count, "; configured blocks ", data$source_flow$block_count,
    ". A sample/feature dimensions and cell missingness come from the byte-verified saved assay export. B model sample counts are reported separately. Sample identifiers, feature identifiers, and assay values are not included in this flow table.</p>",
    triomics_report_html_table(flow), "</section>",
    "<section><h2>Assay QC and preparation</h2><p>The A payload is byte-verified and inspected in memory only for matrix dimensions and missingness. Preparation settings and measured units come from the configuration. Sample/feature identifiers and source-cell values are excluded. Additional QC diagnostics are unavailable without a versioned QC export.</p></section>",
    "<section><h2>Estimands and contrasts</h2>", triomics_report_html_table(estimands),
    triomics_report_html_table(contrasts), "</section>",
    "<section><h2>Full per-block results</h2><p>All exported B rows are included. Empty uncertainty and interval fields remain explicitly unavailable; the report does not derive confidence intervals.</p>",
    triomics_report_html_table(data$block_results, result_columns), "</section>",
    "<section><h2>Source-reported effect plot</h2><p>Panels separate effect units. Error bars show source-reported standard errors when available and are not confidence intervals. The results table carries family, context, sample counts, and byte-hash lineage.</p>",
    plot_html, "</section>",
    "<section><h2>Integration eligibility, variance, selection, and stability</h2>",
    "<p>Evaluation status is derived from the saved C result export and its listed payload SHA. Evaluation execution and single-fit integration evidence are reported separately; an evaluation can complete while single-fit results and variance remain unavailable.</p>",
    triomics_report_html_table(data$integrations, integration_columns),
    triomics_report_html_table(data$integration_eligibility),
    triomics_report_html_table(data$integration_variance),
    "<p>Stability comparisons are shown only when a saved comparison artifact reports them; no missing stability result is inferred.</p></section>",
    "<section><h2>Evaluation</h2><p>C-07 status ", triomics_report_html_escape(data$conditional_capabilities$status[[3L]]),
    ". C-09 status ", triomics_report_html_escape(data$conditional_capabilities$status[[4L]]),
    ", qualification ", triomics_report_html_escape(data$conditional_capabilities$qualification_status[[4L]]),
    ", scope ", triomics_report_html_escape(data$conditional_capabilities$scope[[4L]]),
    ", model execution ", triomics_report_html_escape(data$conditional_capabilities$model_execution[[4L]]),
    ", benefit guaranteed: no. Conditional capabilities remain visible and are not treated as completed methods.</p>",
    triomics_report_html_table(data$conditional_capabilities, conditional_columns), "</section>",
    "<section><h2>Clinical objectives</h2><p>Clinical rows contain typed aggregate effects only. Raw clinical observations, subject/sample identifiers, exclusions, and free-text questions are excluded.</p>",
    triomics_report_html_table(data$clinical_results, clinical_columns), "</section>",
    "<section><h2>Mapping and enrichment coverage</h2>",
    triomics_report_html_table(data$routes, route_columns),
    triomics_report_html_table(data$enrichment), "</section>",
    "<section><h2>Target evidence and alternatives</h2>",
    triomics_report_html_table(data$target, target_columns),
    "<p>Missing target evidence is not evidence of no binding, no effect, or biological absence.</p></section>",
    "<section><h2>Unavailable work</h2><ul>", unavailable, "</ul></section>",
    "<section><h2>Limitations and reproducible commands</h2><ul>", limitations,
    "</ul><ol>", commands, "</ol></section>",
    "<p class=\"machine-files\">Machine-readable files are written beside this HTML as report_data.json, results.tsv, clinical_results.tsv, integration_eligibility.tsv, integration_variance.tsv, enrichment.tsv, section_statuses.tsv, and report_manifest.json.</p>"
  )
}

triomics_report_sanitize_value <- function(value) {
  if (is.data.frame(value)) {
    return(as.data.frame(lapply(value, triomics_report_sanitize_value),
                         stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (is.list(value)) return(lapply(value, triomics_report_sanitize_value))
  if (is.character(value)) return(triomics_report_clean_text(value))
  value
}

triomics_report_stage_section_template <- function(section, staging) {
  if (!section %in% c("targets", "clinical")) {
    triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "The requested report section is not registered.",
                          "report.section")
  }
  dir.create(file.path(staging, "sections"), recursive = TRUE, showWarnings = FALSE)
  source <- triomics_report_section_template_path(section)
  filename <- file.path(staging, "sections", paste0(section, ".qmd"))
  if (nzchar(source)) {
    content <- paste(readLines(source, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    heading <- if (identical(section, "targets")) "Target dossier details" else "Clinical report details"
    content <- paste0("## ", heading, "\n\n{{< include ", section, "-content.html >}}\n")
  }
  if (grepl("(?m)^\\s*```\\s*\\{", content, perl = TRUE) ||
      grepl("(?is)<\\s*(?:script|iframe|object|embed)\\b|javascript:", content, perl = TRUE)) {
    triomics_report_abort("D_REPORT_TEMPLATE_UNSAFE", "A packaged report section contains executable content.",
                          paste0("report.sections.", section))
  }
  include_matches <- regmatches(content, gregexpr("(?s)\\{\\{<\\s*include\\s+([^ >]+)\\s*>\\}\\}",
                                                   content, perl = TRUE))[[1L]]
  expected_include <- paste0(section, "-content.html")
  if (!length(include_matches) || !any(grepl(paste0("include\\s+", expected_include, "\\s*>"),
                                              include_matches, perl = TRUE)) ||
      any(!grepl(paste0("include\\s+", expected_include, "\\s*>"), include_matches, perl = TRUE))) {
    triomics_report_abort("D_REPORT_TEMPLATE_UNSAFE", "A packaged report section must use its fixed generated-content include.",
                          paste0("report.sections.", section))
  }
  triomics_report_write_text(content, filename)
  invisible(filename)
}

triomics_report_write_tsv <- function(frame, path) {
  if (!is.data.frame(frame)) {
    triomics_report_abort("D_REPORT_RENDER_FAILED", "A report export is not a typed table.", basename(path))
  }
  for (name in names(frame)) {
    if (is.character(frame[[name]])) frame[[name]] <- triomics_report_tsv_text(frame[[name]])
  }
  utils::write.table(frame, file = path, sep = "\t", row.names = FALSE, col.names = TRUE,
                     quote = TRUE, na = "", fileEncoding = "UTF-8")
  if (!file.exists(path) || dir.exists(path) || file.info(path)$size <= 0) {
    triomics_report_abort("D_REPORT_RENDER_FAILED", "A machine-readable report table was not written.",
                          basename(path))
  }
  invisible(path)
}

triomics_report_write_text <- function(value, path) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(enc2utf8(value)), connection)
  invisible(path)
}

triomics_report_manifest_sources <- function(bundle) {
  refs <- bundle$artifacts
  names <- c("a", "b", "c", "d", "clinical")
  manifest_producer <- function(producer, key) {
    if (!is.list(producer) || is.null(names(producer)) ||
        !identical(sort(names(producer)), sort(c("package", "version", "commit")))) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "A source producer identity is malformed.", key)
    }
    list(
      package = triomics_report_identifier(producer, "package"),
      version = triomics_report_identifier(producer, "version"),
      commit = triomics_report_identifier(producer, "commit")
    )
  }
  records <- list()
  for (name in names) {
    reference <- refs[[name]]
    if (is.null(reference)) next
    if (identical(name, "d")) {
      records[[length(records) + 1L]] <- list(
        stage = "D", pointer_path = triomics_report_identifier(reference, "pointer_path"),
        pointer_sha256 = triomics_report_identifier(reference, "pointer_sha256"),
        generation_id = triomics_report_identifier(reference, "generation_id"),
        generation_manifest_path = triomics_report_identifier(reference, "manifest_path"),
        generation_manifest_sha256 = triomics_report_identifier(reference, "manifest_sha256"),
        payload_path = triomics_report_identifier(reference, "payload_path"),
        payload_sha256 = triomics_report_identifier(reference, "payload_sha256"),
        envelope_path = triomics_report_identifier(reference, "envelope_path"),
        envelope_sha256 = triomics_report_identifier(reference, "envelope_sha256"),
        producer = manifest_producer(reference$producer, "D.producer"),
        status = triomics_report_identifier(reference, "status")
      )
    } else {
      records[[length(records) + 1L]] <- list(
        stage = toupper(name), path = triomics_report_identifier(reference, "path"),
        artifact_type = triomics_report_identifier(reference, "artifact_type"),
        artifact_id = triomics_report_identifier(reference, "artifact_id"),
        payload_sha256 = triomics_report_identifier(reference, "payload_sha256"),
        envelope_path = triomics_report_identifier(reference, "envelope_path"),
        envelope_sha256 = triomics_report_identifier(reference, "envelope_sha256"),
        producer = manifest_producer(reference$producer, paste0(toupper(name), ".producer")),
        status = triomics_report_identifier(reference, "status")
      )
    }
  }
  records
}

triomics_report_envelope_inputs <- function(bundle, report_data_sha256 = NULL) {
  refs <- bundle$artifacts
  records <- list()
  for (name in c("a", "b", "c")) {
    reference <- refs[[name]]
    if (is.null(reference)) next
    stage <- toupper(name)
    records[[length(records) + 1L]] <- list(
      artifact_id = paste0(stage, ":payload"), sha256 = reference$payload_sha256
    )
    records[[length(records) + 1L]] <- list(
      artifact_id = paste0(stage, ":envelope"), sha256 = reference$envelope_sha256
    )
  }
  d <- refs$d
  if (!is.null(d)) {
    records[[length(records) + 1L]] <- list(artifact_id = "D:current-pointer", sha256 = d$pointer_sha256)
    records[[length(records) + 1L]] <- list(artifact_id = "D:generation-manifest",
                                             sha256 = d$manifest_sha256)
    records[[length(records) + 1L]] <- list(artifact_id = "D:payload", sha256 = d$payload_sha256)
    records[[length(records) + 1L]] <- list(artifact_id = "D:envelope", sha256 = d$envelope_sha256)
  }
  clinical <- refs$clinical
  if (!is.null(clinical)) {
    records[[length(records) + 1L]] <- list(artifact_id = "clinical:payload",
                                             sha256 = clinical$payload_sha256)
    records[[length(records) + 1L]] <- list(artifact_id = "clinical:envelope",
                                             sha256 = clinical$envelope_sha256)
  }
  if (!is.null(report_data_sha256)) {
    records[[length(records) + 1L]] <- list(artifact_id = "report-data:payload",
                                             sha256 = report_data_sha256)
  }
  unname(records)
}

triomics_report_producer_revision <- function() {
  revision <- tryCatch(triomics_source_revision(), error = function(error) "unavailable")
  if (is.character(revision) && length(revision) == 1L &&
      grepl("^(?:[0-9a-f]{40}|[0-9a-f]{64})$", revision)) revision else "unavailable"
}

triomics_report_render_identity <- function(template_path, styles_path, section_paths, runtime) {
  if (!is.list(section_paths) || !identical(sort(names(section_paths)), c("clinical", "targets")) ||
      any(!file.exists(unlist(section_paths, use.names = FALSE)))) {
    triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "A fixed report section template is unavailable.",
                          "report.sections")
  }
  list(
    schema_version = "1.0",
    template_sha256 = triomics_sha256(template_path),
    stylesheet_sha256 = triomics_sha256(styles_path),
    section_template_sha256 = lapply(section_paths, triomics_sha256),
    rendering = list(
      output_format = "html", embed_resources = TRUE, html_math_method = "plain",
      execute_code = FALSE, network_resources = "forbidden"
    ),
    effective_versions = list(
      R = runtime$R, triomics = runtime$package, Quarto = runtime$quarto,
      platform = runtime$platform, source_revision = triomics_report_producer_revision()
    )
  )
}

render_report <- function(report_bundle, report_spec) {
  required_bundle_fields <- c("schema_version", "root", "config_path", "config_sha256",
                              "study_id", "analysis_id", "privacy_class", "audience", "artifacts")
  if (!is.list(report_bundle) || is.null(names(report_bundle)) ||
      !identical(sort(names(report_bundle)), sort(required_bundle_fields))) {
    triomics_report_abort("D_REPORT_BUNDLE_INVALID",
                          "The report needs an immutable manifest of verified producer artifacts.",
                          "report_bundle")
  }
  if (!is.list(report_spec) || is.null(names(report_spec)) ||
      !identical(names(report_spec), "output_relative_path") ||
      !identical(report_spec$output_relative_path, "report")) {
    triomics_report_abort("D_REPORT_REQUEST_INVALID", "The report output must use the fixed local report directory.",
                          "report.output")
  }
  config <- triomics_report_config(report_bundle$config_path)
  loaded <- triomics_report_verify_bundle(report_bundle, config)
  root <- loaded$root
  final_relative <- report_spec$output_relative_path
  final_path <- triomics_report_safe_child(root, final_relative, "report.output", must_exist = FALSE)
  if (file.exists(final_path) || dir.exists(final_path)) {
    triomics_report_abort("D_REPORT_OUTPUT_EXISTS", "The report output already exists; render into a fresh run root.",
                          "report.output")
  }
  quarto_path <- triomics_report_quarto_path()
  runtime <- triomics_report_runtime(quarto_path)
  data <- triomics_report_project(loaded, config, runtime)
  plot <- triomics_report_plots(data$block_results)
  content <- triomics_report_render_content(data, plot$html)
  staging <- tempfile(".report-staging-", tmpdir = root)
  if (!dir.create(staging, recursive = FALSE, showWarnings = FALSE)) {
    triomics_report_abort("D_REPORT_RENDER_FAILED", "A local report staging directory could not be created.",
                          "report.output")
  }
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  staging <- normalizePath(staging, winslash = "/", mustWork = TRUE)
  template <- triomics_report_template_path()
  styles <- triomics_report_styles_path()
  if (!file.copy(template, file.path(staging, "triomics.qmd"), overwrite = FALSE) ||
      !file.copy(styles, file.path(staging, "styles.css"), overwrite = FALSE)) {
    triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "The fixed report template resources could not be copied.",
                          "report.template")
  }
  triomics_report_stage_section_template("targets", staging)
  triomics_report_stage_section_template("clinical", staging)
  section_paths <- list(
    targets = file.path(staging, "sections", "targets.qmd"),
    clinical = file.path(staging, "sections", "clinical.qmd")
  )
  render_identity <- triomics_report_render_identity(template, styles, section_paths, runtime)
  report_id <- triomics_hash_text(triomics_canonical_json(list(
    config_sha256 = config$config_sha256,
    sources = triomics_report_manifest_sources(report_bundle),
    render_identity = render_identity
  )))
  data$render_identity <- render_identity
  data$report_id <- report_id
  triomics_report_write_text(content, file.path(staging, "report-content.html"))
  triomics_report_write_text(triomics_report_optional_section_html(data$extended_sections$targets),
                             file.path(staging, "targets-content.html"))
  triomics_report_write_text(triomics_report_optional_section_html(data$extended_sections$clinical),
                             file.path(staging, "clinical-content.html"))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging)
  quarto_output <- tryCatch(system2(
    quarto_path,
    args = c("render", "triomics.qmd", "--to", "html", "--output", "report.html"),
    stdout = TRUE, stderr = TRUE,
    env = paste0("TMPDIR=", Sys.getenv("TMPDIR"))
  ), error = function(error) NULL)
  quarto_status <- if (is.null(quarto_output)) 1L else {
    value <- attr(quarto_output, "status")
    if (is.null(value)) 0L else as.integer(value)
  }
  html_path <- file.path(staging, "report.html")
  if (!identical(quarto_status, 0L) || !file.exists(html_path) || dir.exists(html_path) ||
      file.info(html_path)$size <= 0) {
    triomics_report_abort("D_REPORT_RENDER_FAILED", "Quarto failed to produce the offline report HTML.",
                          "report.render")
  }
  html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (triomics_report_external_reference(html)) {
    triomics_report_abort("D_REPORT_RENDER_UNSAFE", "The rendered report contains a runtime network reference.",
                          "report.html")
  }
  unlink(c(file.path(staging, "triomics.qmd"), file.path(staging, "styles.css"),
           file.path(staging, "report-content.html"), file.path(staging, "targets-content.html"),
           file.path(staging, "clinical-content.html"), file.path(staging, "sections")),
         recursive = TRUE, force = TRUE)
  triomics_report_write_tsv(data$block_results, file.path(staging, "results.tsv"))
  triomics_report_write_tsv(data$clinical_results, file.path(staging, "clinical_results.tsv"))
  triomics_report_write_tsv(data$integration_eligibility,
                            file.path(staging, "integration_eligibility.tsv"))
  triomics_report_write_tsv(data$integration_variance, file.path(staging, "integration_variance.tsv"))
  triomics_report_write_tsv(data$enrichment, file.path(staging, "enrichment.tsv"))
  triomics_report_write_tsv(triomics_report_section_frame(data$sections),
                            file.path(staging, "section_statuses.tsv"))
  report_json <- list(
    schema_version = data$schema_version,
    study_id = data$study_id,
    report_id = data$report_id,
    report_status = data$report_status,
    audience = data$audience,
    audience_notice = data$audience_notice,
    config_sha256 = data$config_sha256,
    stage_statuses = data$stage_statuses,
    sections = data$sections,
    source_flow = list(source_count = data$source_flow$source_count,
                       block_count = data$source_flow$block_count,
                       blocks = triomics_report_data_frame_records(data$source_flow$blocks)),
    analysis_context = data$analysis_context,
    block_results = triomics_report_data_frame_records(data$block_results),
    clinical_results = triomics_report_data_frame_records(data$clinical_results),
    integrations = triomics_report_data_frame_records(data$integrations),
    integration_eligibility = triomics_report_data_frame_records(data$integration_eligibility),
    integration_variance = triomics_report_data_frame_records(data$integration_variance),
    routes = triomics_report_data_frame_records(data$routes),
    enrichment = triomics_report_data_frame_records(data$enrichment),
    target = triomics_report_data_frame_records(data$target),
    conditional_capabilities = triomics_report_data_frame_records(data$conditional_capabilities),
    extended_sections = data$extended_sections,
    unavailable_work = data$unavailable_work,
    limitations = data$limitations,
    reproducible_commands = data$reproducible_commands,
    environment = data$environment,
    render_identity = data$render_identity,
    sources = triomics_report_manifest_sources(report_bundle)
  )
  report_json <- triomics_report_sanitize_value(report_json)
  triomics_write_json_atomic(report_json, file.path(staging, "report_data.json"))
  report_reason_codes <- vapply(data$sections, function(section) section$reason_code, character(1))
  report_reason_codes <- unique(report_reason_codes[nzchar(report_reason_codes)])
  report_data_envelope <- triomics_artifact_envelope(
    artifact_type = "triomics.report-data", artifact_id = paste0("report-data-", substr(report_id, 1L, 16L)),
    study_id = data$study_id,
    run_id = report_id, status = data$report_status, config_sha256 = data$config_sha256,
    payload = file.path(staging, "report_data.json"), producer_commit =
      triomics_report_producer_revision(),
    inputs = triomics_report_envelope_inputs(report_bundle),
    reason_codes = report_reason_codes,
    privacy_class = config$privacy_class
  )
  triomics_write_envelope(report_data_envelope, file.path(staging, "report_data.envelope.json"))
  output_names <- c("report.html", "report_data.json", "results.tsv",
                    "clinical_results.tsv", "integration_eligibility.tsv",
                    "integration_variance.tsv", "enrichment.tsv",
                    "section_statuses.tsv", "report_data.envelope.json")
  outputs <- lapply(output_names, function(name) {
    path <- file.path(staging, name)
    list(path = name, size_bytes = as.numeric(file.info(path)$size),
         digest_algorithm = "sha256", digest = triomics_sha256(path))
  })
  report_manifest <- list(
    schema_version = "1.0",
    artifact_type = "triomics.report-manifest",
    status = data$report_status,
    report_id = report_id,
    config_sha256 = data$config_sha256,
    audience = data$audience,
    privacy_class = config$privacy_class,
    render_identity = render_identity,
    stage_statuses = data$stage_statuses,
    section_statuses = data$sections,
    sources = triomics_report_manifest_sources(report_bundle),
    environment = data$environment,
    outputs = outputs,
    figure_count = as.integer(plot$count),
    disclosure_status = if (identical(data$audience, "approved_aggregate")) "clinical_not_included" else
      if (identical(data$audience, "private_research")) "private_local" else "synthetic_or_nonclinical"
  )
  report_manifest <- triomics_report_sanitize_value(report_manifest)
  triomics_write_json_atomic(report_manifest, file.path(staging, "report_manifest.json"))
  manifest_envelope <- triomics_artifact_envelope(
    artifact_type = "triomics.report-manifest", artifact_id = paste0("report-manifest-", substr(report_id, 1L, 16L)),
    study_id = report_data_envelope$study_id, run_id = report_id,
    status = data$report_status, config_sha256 = data$config_sha256,
    payload = file.path(staging, "report_manifest.json"),
    producer_commit = triomics_report_producer_revision(),
    inputs = triomics_report_envelope_inputs(report_bundle, triomics_sha256(file.path(staging,
                                                                                       "report_data.json"))),
    reason_codes = report_data_envelope$reason_codes,
    privacy_class = config$privacy_class
  )
  triomics_write_envelope(manifest_envelope, file.path(staging, "report_manifest.envelope.json"))
  if (!isTRUE(triomics_validate_envelope(file.path(staging, "report_data.envelope.json"))) ||
      !isTRUE(triomics_validate_envelope(file.path(staging, "report_manifest.envelope.json")))) {
    triomics_report_abort("D_REPORT_RENDER_FAILED", "The report artifact envelope failed its read-back validation.",
                          "report.envelope")
  }
  final_inventory <- sort(list.files(staging, all.files = TRUE, no.. = TRUE))
  if (!identical(final_inventory, sort(c(output_names, "report_manifest.json",
                                         "report_manifest.envelope.json")))) {
    triomics_report_abort("D_REPORT_RENDER_UNSAFE", "The report staging directory contains an unregistered output asset.",
                          "report.output_inventory")
  }
  if (file.exists(final_path) || dir.exists(final_path) || !file.rename(staging, final_path)) {
    triomics_report_abort("D_REPORT_RENDER_FAILED", "The complete report directory could not be published atomically.",
                          "report.output")
  }
  staging <- ""
  incomplete <- any(vapply(data$sections, function(section) {
    isTRUE(section$required) && !identical(section$status, "complete")
  }, logical(1)))
  list(status = if (incomplete) "partial" else "complete",
       exit_code = if (incomplete) 4L else 0L,
       output_relative_path = final_relative,
       manifest_sha256 = triomics_sha256(file.path(final_path, "report_manifest.json")),
       html_sha256 = triomics_sha256(file.path(final_path, "report.html")),
       results_rows = as.integer(nrow(data$block_results)),
       clinical_rows = as.integer(nrow(data$clinical_results)),
       stage_statuses = data$stage_statuses,
       section_statuses = data$sections)
}
