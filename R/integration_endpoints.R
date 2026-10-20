triomics_c08_abort <- function(code, message, key = NULL) {
  triomics_integration_abort(code, message, key = key)
}

triomics_c08_sha256 <- function(value) {
  triomics_hash_text(triomics_canonical_json(value))
}

triomics_c08_valid_sha256 <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) &&
    grepl("^[0-9a-f]{64}$", value)
}

triomics_c08_method_contract <- function(method) {
  contracts <- list(
    concat_pca = list(
      native_method = "stats::prcomp",
      score_definition = "native_pca_scores",
      scale = "native_pca_score",
      orientation = "native_component_sign",
      uses_y = FALSE
    ),
    mixomics_diablo = list(
      native_method = "mixOmics::block.splsda",
      score_definition = "consensus_mean",
      scale = "training_standardized_native_score_mean",
      orientation = "frozen_consensus_training_anchor",
      uses_y = TRUE
    ),
    mixomics_block_spls = list(
      native_method = "mixOmics::block.spls",
      score_definition = "equal_weight_mean_of_signed_training_standardized_native_scores",
      scale = "training_standardized_native_score_mean",
      orientation = "frozen_consensus_training_anchor",
      uses_y = TRUE
    ),
    mofa2 = list(
      native_method = "MOFA2::run_mofa",
      score_definition = "native_mofa_factor_values",
      scale = "native_mofa_factor_value",
      orientation = "native_factor_sign_unaligned",
      uses_y = FALSE
    )
  )
  contract <- contracts[[method]]
  if (is.null(contract)) {
    triomics_c08_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "C-08 score endpoints are available only for PCA, DIABLO, block.spls, and MOFA.",
      key = "integration.method"
    )
  }
  contract
}

triomics_c08_named_ids <- function(value, label) {
  if (!is.character(value) || !length(value) || anyNA(value) ||
      any(!nzchar(trimws(value))) || anyDuplicated(value)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", paste0(label, " must be unique non-blank ids."), label)
  }
  value
}

triomics_c08_result_training_ids <- function(result) {
  ids <- result$params$training_unit_ids
  if (is.null(ids) && is.list(result$params$state)) ids <- result$params$state$training_unit_ids
  triomics_c08_named_ids(ids, "result training units")
}

triomics_c08_validate_unit_map <- function(unit_map, score_unit_ids) {
  required <- c("unit_id", "sample_id", "subject_key", "unit_type", "visit_id", "time")
  if (!is.data.frame(unit_map) || any(!required %in% names(unit_map))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "C-08 needs the complete public analysis unit map.", "unit_map")
  }
  values <- lapply(required, function(field) unit_map[[field]])
  names(values) <- required
  if (any(!vapply(values, is.character, logical(1))) ||
      any(vapply(values, function(x) anyNA(x) || any(!nzchar(trimws(x))), logical(1)))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Unit, sample, subject, visit, and time ids must be explicit character values.", "unit_map")
  }
  if (anyDuplicated(unit_map$unit_id) || anyDuplicated(unit_map$sample_id) ||
      !setequal(unit_map$unit_id, score_unit_ids) || length(unit_map$unit_id) != length(score_unit_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "The score rows and analysis unit map do not have exact one-to-one coverage.", "unit_map")
  }
  unit_map[match(score_unit_ids, unit_map$unit_id), required, drop = FALSE]
}

triomics_c08_validate_input_hashes <- function(input_hashes, block_ids) {
  if (!is.list(input_hashes) || is.null(names(input_hashes)) ||
      !identical(sort(names(input_hashes)), sort(block_ids)) || anyDuplicated(names(input_hashes)) ||
      !all(vapply(input_hashes, triomics_c08_valid_sha256, logical(1)))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Input hashes must be SHA-256 values for every and only fitted block.", "input_hashes")
  }
  input_hashes[block_ids]
}

triomics_c08_fit_identity <- function(result, backend_fit, contract) {
  if (!inherits(backend_fit, "BackendFit") || !is.list(backend_fit$result_params) ||
      !is.list(backend_fit$metadata)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "A public BackendFit sidecar is required for score provenance.", "backend_fit")
  }
  metadata <- backend_fit$metadata
  params <- backend_fit$result_params
  method <- result$backend
  training_ids <- triomics_c08_result_training_ids(result)
  fit_training_ids <- metadata$training_unit_ids
  if (is.null(fit_training_ids)) fit_training_ids <- params$training_unit_ids
  if (is.null(fit_training_ids) && is.list(params$state)) fit_training_ids <- params$state$training_unit_ids
  if (!identical(metadata$backend, method) ||
      !identical(params$method, method) ||
      !identical(fit_training_ids, training_ids)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Fit, IntegrationResult, and training-unit identities disagree.", "training_unit_ids")
  }
  native_method <- metadata$native_method
  if (is.null(native_method)) native_method <- params$native_method
  if (is.null(native_method)) native_method <- contract$native_method
  if (!identical(native_method, contract$native_method)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "The producing native method does not match the registered backend contract.", "native_method")
  }
  score_definition <- params$score_definition
  if (!is.null(score_definition) && !identical(score_definition, contract$score_definition)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Score definition differs from the registered backend contract.", "score_definition")
  }
  fit_state_hash <- metadata$state_hash
  if (is.null(fit_state_hash)) fit_state_hash <- params$training_state_hash
  if (is.null(fit_state_hash) && is.list(params$state)) fit_state_hash <- params$state$state_hash
  if (!triomics_c08_valid_sha256(fit_state_hash)) {
    fit_state_hash <- triomics_c08_sha256(list(
      result_params = params,
      scores = triomics_matrix_payload(result$scores),
      selected = result$selected
    ))
  }
  list(
    fit_id = if (is.character(metadata$fit_id) && length(metadata$fit_id) == 1L && nzchar(metadata$fit_id)) {
      metadata$fit_id
    } else {
      paste0("fit-", substr(fit_state_hash, 1L, 16L))
    },
    state_sha256 = fit_state_hash,
    method = method,
    native_method = native_method,
    native_version = if (is.null(metadata$native_version)) metadata$backend_version else metadata$native_version,
    model_role = metadata$model_role,
    preparation_context_id = metadata$preparation_context_id
  )
}

triomics_c08_validate_training_y <- function(training_y, training_unit_ids, unit_map, used_by_fit) {
  subject_map <- stats::setNames(unit_map$subject_key, unit_map$unit_id)
  if (is.null(training_y) && !isTRUE(used_by_fit)) {
    return(list(
      variable_id = NULL,
      used_by_fit = FALSE,
      training_unit_ids = training_unit_ids,
      training_subjects = unname(subject_map[training_unit_ids]),
      values_sha256 = NULL
    ))
  }
  if (!is.list(training_y) || !is.character(training_y$variable_id) ||
      length(training_y$variable_id) != 1L || is.na(training_y$variable_id) ||
      !nzchar(trimws(training_y$variable_id)) || !is.atomic(training_y$values) ||
      is.null(names(training_y$values))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Training Y identity and unit-keyed values are required when Y is used or supplied.", "training_y")
  }
  values <- training_y$values
  if (anyDuplicated(names(values)) || !identical(sort(names(values)), sort(training_unit_ids)) ||
      anyNA(values)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Training Y must cover the fitted units exactly once.", "training_y")
  }
  y_payload <- list(
    variable_id = training_y$variable_id,
    values = as.list(values[training_unit_ids]),
    unit_ids = as.list(training_unit_ids),
    subjects = as.list(unname(subject_map[training_unit_ids]))
  )
  list(
    variable_id = training_y$variable_id,
    used_by_fit = isTRUE(used_by_fit),
    training_unit_ids = training_unit_ids,
    training_subjects = unname(subject_map[training_unit_ids]),
    values_sha256 = triomics_c08_sha256(y_payload)
  )
}

triomics_c08_matrix_hash <- function(endpoint) {
  samples <- endpoint$samples
  rows <- lapply(samples, function(sample) {
    list(
      sample_id = as.character(sample$sample_id),
      value = as.numeric(sample$value)
    )
  })
  rows <- rows[order(vapply(rows, `[[`, character(1), "sample_id"))]
  triomics_b_hash(list(endpoint_id = endpoint$endpoint_id, rows = rows))
}

triomics_c08_endpoint_hash <- function(endpoint) {
  body <- endpoint
  body$endpoint_sha256 <- NULL
  triomics_c08_sha256(body)
}

triomics_c08_bundle_hash <- function(bundle) {
  body <- bundle
  body$payload_sha256 <- NULL
  triomics_c08_sha256(body)
}

triomics_integration_score_endpoints <- function(result, backend_fit, context) {
  result <- triomics_validate_integration_result(result)
  if (!is.list(context) || !is.character(context$study_id) || length(context$study_id) != 1L ||
      !nzchar(context$study_id) || !is.character(context$analysis_id) || length(context$analysis_id) != 1L ||
      !nzchar(context$analysis_id) || !is.character(context$integration_id) || length(context$integration_id) != 1L ||
      !nzchar(context$integration_id) || !is.character(context$preparation_context_id) ||
      length(context$preparation_context_id) != 1L || !nzchar(context$preparation_context_id) ||
      !is.character(context$representation_id) || length(context$representation_id) != 1L ||
      !nzchar(context$representation_id)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Study, analysis, integration, preparation, and representation identities are required.", "context")
  }
  contract <- triomics_c08_method_contract(result$backend)
  score_units <- triomics_c08_named_ids(rownames(result$scores), "score unit ids")
  component_ids <- triomics_c08_named_ids(colnames(result$scores), "score component ids")
  unit_map <- triomics_c08_validate_unit_map(context$unit_map, score_units)
  if (!identical(as.character(unit_map$unit_id), score_units)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "The public unit map cannot be aligned exactly to score rows.", "unit_map")
  }
  training_unit_ids <- triomics_c08_result_training_ids(result)
  if (!setequal(score_units, training_unit_ids) || length(score_units) != length(training_unit_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Score rows must exactly cover the producing fit's training units.", "training_unit_ids")
  }
  input_hashes <- triomics_c08_validate_input_hashes(context$input_hashes, names(result$loadings))
  fit <- triomics_c08_fit_identity(result, backend_fit, contract)
  if (!identical(fit$preparation_context_id, context$preparation_context_id)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "The fit and endpoint request use different preparation contexts.", "preparation_context_id")
  }
  if (!is.list(context$selection) ||
      !is.character(context$selection$ancestry) || length(context$selection$ancestry) != 1L ||
      is.na(context$selection$ancestry) || !nzchar(context$selection$ancestry)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Explicit selection ancestry is required.", "selection")
  }
  if (!is.character(context$available_at_time) || length(context$available_at_time) != 1L ||
      is.na(context$available_at_time) || !nzchar(context$available_at_time) ||
      !is.character(context$dependency_group) || length(context$dependency_group) != 1L ||
      is.na(context$dependency_group) || !nzchar(context$dependency_group)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Availability and dependency group are required.", "availability")
  }
  training_y <- triomics_c08_validate_training_y(context$training_y, training_unit_ids, unit_map, contract$uses_y)
  if (contract$uses_y && !training_y$used_by_fit) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Supervised integration requires the declared training Y.", "training_y")
  }
  input_source_hash <- triomics_c08_sha256(list(
    input_hashes = input_hashes,
    fit_state_sha256 = fit$state_sha256,
    training_y_sha256 = training_y$values_sha256,
    training_unit_ids = as.list(training_unit_ids),
    preparation_context_id = context$preparation_context_id,
    representation_id = context$representation_id
  ))
  endpoints <- lapply(seq_along(component_ids), function(component_index) {
    component_id <- component_ids[[component_index]]
    endpoint_id <- paste0("score_", substr(triomics_c08_sha256(list(
      study_id = context$study_id, analysis_id = context$analysis_id,
      integration_id = context$integration_id, component_id = component_id
    )), 1L, 20L))
    samples <- lapply(seq_along(score_units), function(i) list(
      unit_id = score_units[[i]],
      unit_type = as.character(unit_map$unit_type[[i]]),
      sample_id = as.character(unit_map$sample_id[[i]]),
      subject_key = as.character(unit_map$subject_key[[i]]),
      visit_id = as.character(unit_map$visit_id[[i]]),
      time = as.character(unit_map$time[[i]]),
      value = as.numeric(result$scores[i, component_index])
    ))
    endpoint <- list(
      artifact_type = "integration_score_endpoint",
      endpoint_id = endpoint_id,
      study_id = context$study_id,
      analysis_id = context$analysis_id,
      integration_id = context$integration_id,
      component_id = component_id,
      component_index = as.integer(component_index),
      response_type = "continuous",
      unit = "score_unit",
      units = "score_unit",
      scale = contract$scale,
      orientation = contract$orientation,
      samples = samples,
      producing_fit = fit,
      training_unit_ids = training_unit_ids,
      training_ids = unique(unit_map$subject_key[match(training_unit_ids, unit_map$unit_id)]),
      training_y = training_y,
      training_outcome = if (contract$uses_y) training_y$variable_id else NULL,
      selection = context$selection,
      inference_status = if (contract$uses_y ||
                            identical(context$selection$ancestry, "trained_on_outcome") ||
                            identical(context$selection$ancestry, "selected_after_clinical_outcome") ||
                            !is.null(context$selection$inspected_outcome)) {
        "descriptive_only"
      } else if (identical(result$backend, "mofa2")) {
        "exploratory"
      } else {
        "qualified"
      },
      available_at_time = context$available_at_time,
      dependency_group = context$dependency_group,
      preparation_context_id = context$preparation_context_id,
      representation_id = context$representation_id,
      input_hashes = input_hashes,
      source_hash = input_source_hash
    )
    endpoint$matrix_hash <- triomics_c08_matrix_hash(endpoint)
    endpoint$endpoint_sha256 <- triomics_c08_endpoint_hash(endpoint)
    endpoint
  })
  names(endpoints) <- component_ids
  bundle <- list(
    artifact_type = "integration_score_endpoint_bundle",
    study_id = context$study_id,
    analysis_id = context$analysis_id,
    integration_id = context$integration_id,
    unit_map_hash = triomics_c08_sha256(unit_map),
    source_hash = input_source_hash,
    endpoints = endpoints
  )
  class(bundle) <- c("IntegrationScoreEndpointBundle", "list")
  bundle$payload_sha256 <- triomics_c08_bundle_hash(bundle)
  bundle
}
