triomics_b_required_effect_unit <- function(scale) {
  if (identical(scale, "log2")) return("log2_fold_change")
  if (scale %in% c("clr_ln", "clr_log2")) return("clr_contrast")
  if (scale %in% c("linear", "ln", "variance_stabilized")) return("group_difference")
  ""
}

triomics_b_check_effect_unit <- function(scales, effect_unit) {
  required <- unique(vapply(scales, triomics_b_required_effect_unit, character(1)))
  if (length(required) != 1L || !nzchar(required[[1L]]) || !identical(required[[1L]], effect_unit)) {
    triomics_b_abort(
      "B_DESIGN_EFFECT_UNIT",
      "The effect unit does not match the declared measurement scale.",
      key = "statistics.effect_unit"
    )
  }
  required[[1L]]
}

triomics_b_inferential_status <- function(estimated, selection_validity) {
  if (!estimated) return("not_estimable")
  if (identical(selection_validity, "descriptive_only")) return("descriptive_only")
  "unverified"
}

triomics_b_family <- function(study_id, analysis_id, block_id, contrast_id, member_ids,
                              estimated, p_values, publish_q) {
  n <- length(member_ids)
  internal_p <- rep(1, n)
  if (any(estimated)) internal_p[estimated] <- as.numeric(p_values[estimated])
  internal_p <- unname(as.numeric(internal_p))
  internal_q <- unname(as.numeric(stats::p.adjust(internal_p, method = "BH")))
  n_estimated <- as.integer(sum(estimated))
  n_failed <- as.integer(n - n_estimated)
  list(
    family_id = triomics_b_hash(list(
      study_id = study_id,
      analysis_id = analysis_id,
      block_id = block_id,
      contrast_id = contrast_id,
      grain = "study_analysis_block_scalar_contrast"
    )),
    member_ids = as.character(member_ids),
    member_hash = triomics_b_hash(as.character(member_ids)),
    n_declared = as.integer(n),
    n_estimated = n_estimated,
    n_failed = n_failed,
    n_eligible = n_estimated,
    n_evaluated = as.integer(n),
    dispositions = c(estimated = n_estimated, failed = n_failed),
    internal_p = internal_p,
    internal_q = internal_q,
    correction_method = "BH",
    q_published = isTRUE(publish_q)
  )
}

triomics_b_direction_fields <- function() {
  list(
    direction_status = "not_applicable",
    direction_reason = "independent_samples_have_no_within_person_delta",
    direction_numerator = NULL,
    direction_denominator = NULL,
    direction_zero_count = NULL,
    n_pairs = NULL
  )
}

triomics_b_optional_number <- function(value) {
  if (length(value) != 1L || !is.finite(value)) NULL else as.numeric(value)
}

triomics_b_n_subjects <- function(subjects, sample_ids, used) {
  n_used <- as.integer(sum(used))
  if (is.null(subjects)) return(n_used)
  chosen <- subjects[sample_ids[used]]
  chosen <- chosen[!is.na(chosen) & nzchar(chosen)]
  if (length(chosen) == 0L) n_used else as.integer(length(unique(chosen)))
}

triomics_b_artifact_status <- function(method_status) {
  if (!is.character(method_status) || length(method_status) != 1L ||
      is.na(method_status) || !nzchar(method_status)) {
    triomics_b_abort(
      "B_RESULT_CONTRACT",
      "Stage B method status is outside the artifact result contract.",
      key = "status"
    )
  }
  if (identical(method_status, "fallback_complete")) return("complete")
  if (identical(method_status, "fallback_partial")) return("partial")
  if (method_status %in% triomics_envelope_statuses()) return(method_status)
  triomics_b_abort(
    "B_RESULT_CONTRACT",
    "Stage B method status is outside the artifact result contract.",
    key = "status"
  )
}

triomics_b_method_metadata <- function(fit) {
  fields <- c(
    "requested_method", "method", "unavailable_reason", "deqms_status",
    "deqms_version", "fallback_applied", "fallback_provenance"
  )
  metadata <- list(method_status = fit$status)
  for (field in fields) {
    if (field %in% names(fit)) metadata[[field]] <- fit[[field]]
  }
  metadata
}

triomics_b_reason_codes <- function(fit) {
  deqms_reason <- if (is.list(fit$deqms_status)) fit$deqms_status$reason else NULL
  reasons <- unlist(list(fit$unavailable_reason, deqms_reason), use.names = FALSE)
  reasons <- as.character(reasons)
  unique(reasons[!is.na(reasons) & nzchar(reasons)])
}

triomics_b_identity_preparation <- function(config) {
  analyses <- config$analyses
  if (length(analyses) == 0L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Stage B needs an analysis.", key = "analyses")
  }
  for (analysis in analyses) {
    block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
    context <- analysis$preparation_context_id
    for (block in config$blocks) {
      if (!block$block_id %in% block_ids) next
      prep <- block$preprocessing
      identity <- !is.null(prep) &&
        identical(prep$context_id, context) &&
        identical(prep$transform$method, "none") &&
        identical(prep$normalisation$method, "none") &&
        identical(prep$imputation$method, "none") &&
        triomics_b_empty(prep$derive)
      if (!identity) {
        triomics_b_abort(
          "B_DESIGN_PREPARATION",
          "Stage B reads the declared measurement matrix and does not refit preparation.",
          key = "preprocessing"
        )
      }
    }
  }
  invisible(TRUE)
}

triomics_execute_stage_b <- function(config_path, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Stage B output must be a directory path.", key = "output")
  }
  config <- triomics_read_config(config_path)
  triomics_b_identity_preparation(config)
  ingested <- triomics_ingest_configured_study(config, config_path)
  n_fits <- 0L
  for (analysis in config$analyses) {
    block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
    n_features <- sum(vapply(block_ids, function(block_id) {
      nrow(ingested$assays[[block_id]])
    }, integer(1)))
    n_fits <- n_fits + as.integer(n_features) * as.integer(length(analysis$statistics$contrasts))
  }
  max_fits <- config$execution$max_fits
  if (!is.numeric(max_fits) || length(max_fits) != 1L || is.na(max_fits) || n_fits > max_fits) {
    triomics_b_abort("B_DESIGN_BUDGET", "The declared feature-contrast fits exceed max_fits.",
                     key = "execution.max_fits")
  }
  if (length(config$analyses) != 1L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane writes one analysis envelope.", key = "analyses")
  }
  analysis <- config$analyses[[1L]]
  block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  fit <- triomics_fit_block_models(resolved, analysis$statistics)
  artifact_status <- triomics_b_artifact_status(fit$status)
  method_metadata <- triomics_b_method_metadata(fit)
  payload <- list(
    schema_version = "2.0",
    study_id = ingested$study_id,
    analysis_id = analysis$analysis_id,
    preparation_context_id = analysis$preparation_context_id,
    representation_id = analysis$design$representation,
    status = artifact_status,
    limma_version = fit$limma_version,
    config_sha256 = attr(config, "config_sha256"),
    preparation_refit = FALSE,
    matrix_origin = "declared_measurement_matrix",
    filter_applied = FALSE,
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      limma_version = fit$limma_version
    ),
    blocks = fit$blocks
  )
  payload <- c(payload, method_metadata)
  payload_path <- file.path(output, "B", "block-results.json")
  triomics_write_json_atomic(payload, payload_path)
  inputs <- unname(lapply(ingested$source_manifests, function(source) {
    list(artifact_id = source$source_id, sha256 = source$sha256)
  }))
  envelope <- triomics_artifact_envelope(
    artifact_type = "block_results",
    artifact_id = "stage-b-block-results",
    study_id = ingested$study_id,
    run_id = "b-t01",
    status = artifact_status,
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = triomics_b_reason_codes(fit),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  triomics_write_envelope(envelope, file.path(output, "B", "block-results.envelope.json"))
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "b-t01",
    study_id = ingested$study_id,
    stage = "B",
    status = artifact_status
  ), file.path(output, "registries", "run.json"))
  list(status = artifact_status, method_status = fit$status,
       payload_path = payload_path, limma_version = fit$limma_version)
}
