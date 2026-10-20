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

triomics_c08_unit_records <- function(unit_map) {
  if (is.data.frame(unit_map)) {
    required <- c("unit_id", "unit_type", "source_sample_ids", "representation_id")
    if (any(!required %in% names(unit_map))) return(NULL)
    return(lapply(seq_len(nrow(unit_map)), function(i) {
      record <- lapply(names(unit_map), function(name) {
        value <- unit_map[[name]]
        if (is.list(value) || is.data.frame(value)) value[[i]] else value[[i]]
      })
      names(record) <- names(unit_map)
      record
    }))
  }
  if (!is.list(unit_map) || !length(unit_map) ||
      !all(vapply(unit_map, is.list, logical(1)))) return(NULL)
  unit_map
}

triomics_c08_validate_unit_map <- function(unit_map, score_unit_ids, context) {
  records <- triomics_c08_unit_records(unit_map)
  if (is.null(records) || !length(records)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "C-08 requires the public AnalysisUnitMap fields, including source_sample_ids.", "unit_map")
  }
  ids <- vapply(records, function(record) {
    value <- record$unit_id
    if (!is.character(value) || length(value) != 1L || is.na(value)) "" else value
  }, character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Analysis unit ids must be unique non-blank strings.", "unit_map.unit_id")
  }
  eligibility <- vapply(records, function(record) {
    value <- record$eligibility
    if (is.null(value)) "retained" else if (is.character(value) && length(value) == 1L && !is.na(value)) value else "invalid"
  }, character(1))
  excluded <- eligibility == "excluded"
  if (any(!eligibility %in% c("retained", "excluded"))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "AnalysisUnitMap eligibility must be retained or excluded.", "unit_map.eligibility")
  }
  reasons <- lapply(records, function(record) record$exclusion_reason)
  for (i in which(excluded)) {
    reason <- reasons[[i]]
    if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(trimws(reason))) {
      triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Excluded AnalysisUnitMap rows need an explicit exclusion_reason.", "unit_map.exclusion_reason")
    }
  }
  if (!setequal(ids[!excluded], score_unit_ids) || length(ids[!excluded]) != length(score_unit_ids) ||
      any(ids[excluded] %in% score_unit_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Retained AnalysisUnitMap rows and score rows must cover the same units exactly once.", "unit_map")
  }
  source_ids_all <- unlist(lapply(records, `[[`, "source_sample_ids"), use.names = FALSE)
  if (anyDuplicated(source_ids_all)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "AnalysisUnitMap source samples must belong to one unit only.", "unit_map.source_sample_ids")
  }
  valid_types <- c("sample", "visit_level", "subject_mean", "subject_delta")
  for (record in records) {
    unit_type <- record$unit_type
    unit_representation <- record$representation_id
    source_ids <- record$source_sample_ids
    if (!is.character(unit_type) || length(unit_type) != 1L || is.na(unit_type) ||
        !unit_type %in% valid_types || !is.character(unit_representation) ||
        length(unit_representation) != 1L || is.na(unit_representation) ||
        !identical(unit_representation, context$representation_id)) {
      triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "AnalysisUnitMap unit types and representation ids must be declared and mutually consistent.", "unit_map.unit_type")
    }
    if (!is.character(source_ids) || !length(source_ids) || anyNA(source_ids) ||
        any(!nzchar(trimws(source_ids))) || anyDuplicated(source_ids)) {
      triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Each analysis unit needs unique source_sample_ids.", "unit_map.source_sample_ids")
    }
    subject_id <- record$subject_id
    if (!is.null(subject_id) && (!is.character(subject_id) || length(subject_id) != 1L ||
        is.na(subject_id) || !nzchar(trimws(subject_id)))) {
      triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "A supplied subject_id must be one non-blank string.", "unit_map.subject_id")
    }
    is_excluded <- identical(record$eligibility, "excluded")
    weights <- record$visit_weights
    if (unit_type %in% c("sample", "visit_level")) {
      expected_representation <- "levels"
      if (length(source_ids) != 1L || !is.null(weights)) {
        triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Sample and visit-level units require one source sample and no derived visit weights.", "unit_map.source_sample_ids")
      }
    } else if (identical(unit_type, "subject_mean")) {
      expected_representation <- "mean"
      if (!is.character(subject_id) || length(source_ids) < 1L ||
          !is.numeric(weights) || !length(weights) || is.null(names(weights)) ||
          anyNA(weights) || any(!is.finite(weights)) || anyDuplicated(names(weights)) ||
          any(!nzchar(names(weights))) || sum(weights) == 0) {
        triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Subject means require a subject, source samples and named finite visit weights.", "unit_map.visit_weights")
      }
    } else {
      expected_representation <- "delta"
      if (!is.character(subject_id) || (!is_excluded && length(source_ids) != 2L) ||
          (is_excluded && !length(source_ids)) ||
          !is.numeric(weights) || length(weights) != 2L || is.null(names(weights)) ||
          anyNA(weights) || any(!is.finite(weights)) || anyDuplicated(names(weights)) ||
          !identical(as.numeric(weights), c(-1, 1)) || any(!nzchar(names(weights))) ||
          anyDuplicated(names(weights))) {
        triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Subject deltas require two source samples and named reference/target weights of -1 and +1.", "unit_map.visit_weights")
      }
    }
    if (!identical(unit_representation, expected_representation)) {
      triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Unit type and representation id disagree.", "unit_map.representation_id")
    }
    for (field in c("study_id", "analysis_id", "preparation_context_id")) {
      value <- record[[field]]
      if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
          is.na(value) || !nzchar(trimws(value)))) {
        triomics_c08_abort("C_SCORE_LINEAGE_INVALID", paste0("Supplied AnalysisUnitMap ", field, " must be a non-blank string."), paste0("unit_map.", field))
      }
      if (!is.null(value) && !identical(value, context[[field]])) {
        triomics_c08_abort("C_SCORE_LINEAGE_INVALID", paste0("AnalysisUnitMap ", field, " differs from the producing IntegrationResult."), paste0("unit_map.", field))
      }
    }
  }
  result <- data.frame(
    unit_id = vapply(records, `[[`, character(1), "unit_id"),
    unit_type = vapply(records, `[[`, character(1), "unit_type"),
    representation_id = vapply(records, `[[`, character(1), "representation_id"),
    stringsAsFactors = FALSE
  )
  result$source_sample_ids <- I(lapply(records, `[[`, "source_sample_ids"))
  result$subject_id <- I(lapply(records, function(record) record$subject_id))
  result$visit_weights <- I(lapply(records, function(record) record$visit_weights))
  result$eligibility <- I(lapply(records, function(record) record$eligibility))
  result$exclusion_reason <- I(lapply(records, function(record) record$exclusion_reason))
  for (field in c("study_id", "analysis_id", "preparation_context_id")) {
    result[[field]] <- I(lapply(records, function(record) record[[field]]))
  }
  result
}

triomics_c08_validate_input_hashes <- function(input_hashes, block_ids) {
  if (!is.list(input_hashes) || is.null(names(input_hashes)) ||
      !identical(sort(names(input_hashes)), sort(block_ids)) || anyDuplicated(names(input_hashes)) ||
      !all(vapply(input_hashes, triomics_c08_valid_sha256, logical(1)))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Input hashes must be SHA-256 values for every and only fitted block.", "input_hashes")
  }
  input_hashes[block_ids]
}

triomics_c08_y_binding_hash <- function(training_y, training_unit_ids) {
  if (is.null(training_y)) return(NULL)
  triomics_c08_sha256(list(
    variable_id = training_y$variable_id,
    unit_ids = as.list(training_unit_ids),
    values = as.list(training_y$values[training_unit_ids])
  ))
}

triomics_c08_fit_scores <- function(result, backend_fit) {
  state <- backend_fit$native_state
  scores <- switch(result$backend,
    concat_pca = state$training_scores,
    mixomics_diablo = if (is.list(state$native_scores) && inherits(state$consensus_state, "ConsensusState")) {
      apply_consensus_state(state$native_scores, state$consensus_state)
    } else NULL,
    mixomics_block_spls = if (is.list(state$training_scores) && inherits(state$consensus_state, "ConsensusState")) {
      apply_consensus_state(state$training_scores, state$consensus_state)
    } else NULL,
    mofa2 = state$factors,
    NULL
  )
  if (!is.matrix(scores) || !is.numeric(scores) || any(!is.finite(scores)) ||
      !identical(dimnames(scores), dimnames(result$scores)) ||
      !isTRUE(all.equal(scores, result$scores, tolerance = 0, check.attributes = TRUE))) {
    triomics_c08_abort(
      "C_SCORE_FIT_BINDING_MISMATCH",
      "IntegrationResult scores do not reproduce from the accepted producing-fit state.",
      "scores"
    )
  }
  scores
}

triomics_c08_fit_identity <- function(result, backend_fit, contract, context,
                                      input_hashes, training_y, fit_scores) {
  if (!inherits(backend_fit, "BackendFit") || !is.list(backend_fit$result_params) ||
      !is.list(backend_fit$metadata) || !is.list(backend_fit$native_state)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "A public BackendFit sidecar is required for score provenance.", "backend_fit")
  }
  metadata <- backend_fit$metadata
  params <- backend_fit$result_params
  method <- result$backend
  training_ids <- triomics_c08_result_training_ids(result)
  result_provenance <- result$params$provenance
  result_params <- result$params
  result_params$provenance <- NULL
  fit_training_ids <- metadata$training_unit_ids
  if (is.null(fit_training_ids)) fit_training_ids <- params$training_unit_ids
  if (is.null(fit_training_ids) && is.list(params$state)) fit_training_ids <- params$state$training_unit_ids
  if (!identical(metadata$backend, method) ||
      (!is.null(params$method) && !identical(params$method, method)) ||
      !identical(fit_training_ids, training_ids) ||
      !identical(result_params, params) ||
      !is.list(result_provenance)) {
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
  handoff <- metadata$c08_handoff
  if (!is.list(handoff) ||
      !identical(handoff$schema_version, "C-08-fit-handoff-1") ||
      !identical(handoff$qualification, "same_fit_adapter_handoff") ||
      !triomics_c08_valid_sha256(handoff$handoff_sha256)) {
    triomics_c08_abort(
      "C_SCORE_LINEAGE_INVALID",
      "The accepted adapter does not provide a qualified C-08 fit handoff.",
      "c08_handoff"
    )
  }
  handoff_body <- handoff
  handoff_body$handoff_sha256 <- NULL
  if (!identical(handoff$handoff_sha256, triomics_c08_sha256(handoff_body))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "The C-08 dependency handoff hash is invalid.", "c08_handoff")
  }
  if (!triomics_c08_valid_sha256(fit_state_hash)) fit_state_hash <- handoff$fit_state_sha256
  if (!triomics_c08_valid_sha256(fit_state_hash) ||
      !identical(handoff$fit_state_sha256, fit_state_hash) ||
      (!is.null(metadata$state_hash) && !identical(metadata$state_hash, fit_state_hash)) ||
      (!is.null(params$training_state_hash) && !identical(params$training_state_hash, fit_state_hash))) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "An authoritative producing-fit state identity is unavailable or inconsistent.", "state_sha256")
  }
  expected_y_hash <- triomics_c08_y_binding_hash(
    if (contract$uses_y) context$training_y else NULL, training_ids
  )
  expected_handoff <- list(
    training_scores_sha256 = triomics_c08_sha256(triomics_matrix_payload(fit_scores)),
    training_unit_ids = as.list(training_ids),
    input_hashes = input_hashes,
    training_y_sha256 = expected_y_hash,
    selection_sha256 = triomics_c08_sha256(context$selection),
    unit_map_sha256 = triomics_c08_sha256(context$unit_map),
    representation_id = context$representation_id
  )
  for (field in names(expected_handoff)) {
    if (!identical(handoff[[field]], expected_handoff[[field]])) {
      triomics_c08_abort(
        "C_SCORE_FIT_BINDING_MISMATCH",
        paste0("The C-08 fit handoff does not bind ", field, " to this endpoint request."),
        field
      )
    }
  }
  if (!identical(result_provenance$study_id, context$study_id) ||
      !identical(result_provenance$analysis_id, context$analysis_id) ||
      !identical(result_provenance$preparation_context_id, context$preparation_context_id) ||
      !identical(result_provenance$representation_id, context$representation_id) ||
      !identical(result_provenance$unit_map, training_ids) ||
      !identical(result_provenance$matrix_sha256, input_hashes)) {
    triomics_c08_abort("C_SCORE_FIT_BINDING_MISMATCH", "Input hashes or analysis identity differ from the fit-produced IntegrationResult lineage.", "provenance")
  }
  state_preparation_context <- metadata$preparation_context_id
  if (is.null(state_preparation_context) && is.list(params$state)) {
    state_preparation_context <- params$state$preparation_context_id
  }
  if (is.null(state_preparation_context)) state_preparation_context <- result_provenance$preparation_context_id
  if (!identical(state_preparation_context, context$preparation_context_id)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "The fit and endpoint request use different preparation contexts.", "preparation_context_id")
  }
  fit_id <- if (is.character(metadata$fit_id) && length(metadata$fit_id) == 1L &&
                !is.na(metadata$fit_id) && nzchar(metadata$fit_id)) metadata$fit_id else {
    paste0("fit-", substr(fit_state_hash, 1L, 16L))
  }
  list(
    fit_id = fit_id,
    state_sha256 = fit_state_hash,
    method = method,
    native_method = native_method,
    native_version = if (is.null(metadata$native_version)) metadata$backend_version else metadata$native_version,
    model_role = metadata$model_role,
    preparation_context_id = state_preparation_context,
    handoff_sha256 = handoff$handoff_sha256,
    input_hashes_sha256 = triomics_c08_sha256(input_hashes),
    training_y_sha256 = expected_y_hash,
    selection_sha256 = expected_handoff$selection_sha256
  )
}

triomics_c08_validate_training_y <- function(training_y, training_unit_ids, unit_map, used_by_fit) {
  subject_ids <- vapply(seq_len(nrow(unit_map)), function(i) {
    value <- unit_map$subject_id[[i]]
    if (is.null(value) || !length(value)) NA_character_ else as.character(value)
  }, character(1))
  subject_map <- stats::setNames(subject_ids, unit_map$unit_id)
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
  unit_map <- triomics_c08_validate_unit_map(context$unit_map, score_units, context)
  score_unit_map <- unit_map[match(score_units, unit_map$unit_id), , drop = FALSE]
  training_unit_ids <- triomics_c08_result_training_ids(result)
  if (!setequal(score_units, training_unit_ids) || length(score_units) != length(training_unit_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Score rows must exactly cover the producing fit's training units.", "training_unit_ids")
  }
  input_hashes <- triomics_c08_validate_input_hashes(context$input_hashes, names(result$loadings))
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
  training_y <- triomics_c08_validate_training_y(context$training_y, training_unit_ids, score_unit_map, contract$uses_y)
  if (contract$uses_y && !training_y$used_by_fit) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Supervised integration requires the declared training Y.", "training_y")
  }
  if (!contract$uses_y && !is.null(context$training_y)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "Unsupervised integration cannot attach an unrelated training Y.", "training_y")
  }
  fit_scores <- triomics_c08_fit_scores(result, backend_fit)
  fit <- triomics_c08_fit_identity(
    result, backend_fit, contract, context, input_hashes,
    if (contract$uses_y) context$training_y else NULL, fit_scores
  )
  if (!identical(fit$preparation_context_id, context$preparation_context_id)) {
    triomics_c08_abort("C_SCORE_LINEAGE_INVALID", "The fit and endpoint request use different preparation contexts.", "preparation_context_id")
  }
  input_source_hash <- triomics_c08_sha256(list(
    input_hashes = input_hashes,
    fit_state_sha256 = fit$state_sha256,
    fit_handoff_sha256 = fit$handoff_sha256,
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
    samples <- lapply(seq_along(score_units), function(i) {
      subject_id <- score_unit_map$subject_id[[i]]
      if (is.null(subject_id) || !length(subject_id)) subject_id <- NULL
      sample <- list(
        sample_id = score_units[[i]],
        unit_id = score_units[[i]],
        unit_type = as.character(score_unit_map$unit_type[[i]]),
        source_sample_ids = as.character(score_unit_map$source_sample_ids[[i]]),
        subject_id = if (is.null(subject_id)) NULL else as.character(subject_id),
        subject_key = if (is.null(subject_id)) NULL else as.character(subject_id),
        representation_id = as.character(score_unit_map$representation_id[[i]]),
        value = as.numeric(result$scores[i, component_index])
      )
      if (length(score_unit_map$visit_weights[[i]])) sample$visit_weights <- score_unit_map$visit_weights[[i]]
      if (length(score_unit_map$eligibility[[i]])) sample$eligibility <- score_unit_map$eligibility[[i]]
      if (length(score_unit_map$exclusion_reason[[i]])) sample$exclusion_reason <- score_unit_map$exclusion_reason[[i]]
      for (field in c("study_id", "analysis_id", "preparation_context_id")) {
        value <- score_unit_map[[field]][[i]]
        if (length(value)) sample[[field]] <- value
      }
      sample
    })
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
      training_ids = unique(stats::na.omit(vapply(training_unit_ids, function(unit_id) {
        score_unit_map$subject_id[[match(unit_id, score_unit_map$unit_id)]] %||% NA_character_
      }, character(1)))),
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
      unit_map = unit_map,
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
