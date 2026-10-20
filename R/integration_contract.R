triomics_integration_component_names <- function(value, label, error_code) {
  if (is.null(value) || anyNA(value) || any(!nzchar(trimws(value))) || anyDuplicated(value)) {
    triomics_integration_abort(
      error_code, paste0(label, " must be unique, non-blank names."), key = label
    )
  }
  value
}

triomics_integration_finite_matrix <- function(value, label) {
  if (!is.matrix(value) || !is.numeric(value) || is.complex(value) ||
      length(dim(value)) != 2L || any(dim(value) < 1L)) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", paste0(label, " must be a non-empty numeric matrix."),
      key = label
    )
  }
  row_ids <- triomics_integration_component_names(
    rownames(value), paste0(label, " rows"), "C_SCORE_RESULT_INVALID"
  )
  column_ids <- triomics_integration_component_names(
    colnames(value), paste0(label, " columns"), "C_SCORE_RESULT_INVALID"
  )
  if (any(!is.finite(value))) {
    triomics_integration_abort(
      "C_SCORE_NONFINITE", paste0(label, " must contain only finite values."), key = label
    )
  }
  list(rows = row_ids, columns = column_ids)
}

triomics_validate_integration_result <- function(result) {
  expected_fields <- c("scores", "loadings", "variance", "selected", "backend", "params")
  if (!inherits(result, "IntegrationResult") || !is.list(result) ||
      !identical(names(result), expected_fields)) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID",
      "IntegrationResult must have exactly the six protected fields in contract order."
    )
  }

  score_map <- triomics_integration_finite_matrix(result$scores, "scores")
  if (!is.list(result$loadings) || is.null(names(result$loadings)) ||
      !identical(names(result$loadings), names(result$selected)) ||
      anyDuplicated(names(result$loadings))) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", "Loadings and selected features must share a stable block map."
    )
  }
  block_ids <- triomics_integration_component_names(
    names(result$loadings), "loading block ids", "C_SCORE_RESULT_INVALID"
  )
  if (!is.list(result$selected) || is.null(names(result$selected)) ||
      !identical(names(result$selected), block_ids)) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", "Selected features must be a named list matching loading blocks."
    )
  }
  for (block_id in block_ids) {
    loading_map <- triomics_integration_finite_matrix(
      result$loadings[[block_id]], paste0("loadings.", block_id)
    )
    if (!identical(loading_map$columns, score_map$columns) ||
        !is.character(result$selected[[block_id]]) || anyNA(result$selected[[block_id]]) ||
        anyDuplicated(result$selected[[block_id]]) ||
        !identical(loading_map$rows, result$selected[[block_id]])) {
      triomics_integration_abort(
        "C_SCORE_RESULT_INVALID",
        "Each loading row map must exactly match its executed selected-feature map.",
        key = block_id
      )
    }
  }

  variance_map <- triomics_integration_finite_matrix(result$variance, "variance")
  if (!identical(variance_map$rows, block_ids) ||
      !identical(variance_map$columns, score_map$columns) ||
      any(result$variance < 0 | result$variance > 1)) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID",
      "Variance must be finite proportions keyed by predictor block and retained component."
    )
  }
  if (!is.character(result$backend) || length(result$backend) != 1L ||
      is.na(result$backend) || !nzchar(trimws(result$backend)) || !is.list(result$params)) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", "Backend must be one name and params must be a list."
    )
  }
  result
}

triomics_new_integration_result <- function(scores, loadings, variance, selected,
                                            backend, params) {
  triomics_validate_integration_result(structure(
    list(
      scores = scores,
      loadings = loadings,
      variance = variance,
      selected = selected,
      backend = backend,
      params = params
    ),
    class = "IntegrationResult"
  ))
}

triomics_integration_backend_names <- function() {
  c(
    "concat_pca", "concat_plsda", "concat_pls", "mixomics_diablo",
    "mixomics_block_spls", "mofa2"
  )
}

triomics_integration_backend <- function(method) {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% triomics_integration_backend_names()) {
    triomics_integration_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "The requested integration backend is not registered as implemented.",
      key = "integration.method",
      remediation = "Select the registered concat_pca route or wait for the backend packet and acceptance review."
    )
  }
  if (method %in% c("concat_plsda", "concat_pls")) {
    return(triomics_supervised_backend(method))
  }
  if (identical(method, "mixomics_diablo")) {
    return(triomics_diablo_backend(method))
  }
  if (identical(method, "mixomics_block_spls")) {
    return(triomics_block_spls_backend())
  }
  if (identical(method, "mofa2")) {
    return(structure(
      list(
        validate = function(resolved_analysis, params) {
          triomics_mofa_validate(resolved_analysis, params)
        },
        fit = function(resolved_analysis, params) {
          state <- triomics_mofa_fit(resolved_analysis, params)
          if (!inherits(state, "BackendFit")) {
            triomics_integration_abort(
              "C_SCORE_BACKEND_UNAVAILABLE",
              "The registered MOFA2 route requires loadable MOFA2 and mofapy2 runtimes.",
              key = "integration.method",
              remediation = "Run with the verified isolated MOFA2 1.22.1 and mofapy2 0.7.5 environment."
            )
          }
          state
        },
        predict = function(backend_fit, new_resolved_analysis, params) {
          triomics_integration_abort(
            "C_EVAL_PREDICTION_UNSUPPORTED",
            "Core MOFA2 is exploratory and has no qualified unseen-subject prediction route.",
            key = "integration.method"
          )
        },
        extract = function(backend_fit, context, params) {
          triomics_mofa_extract(backend_fit, context, params)
        },
        evaluation_modes = "exploratory"
      ),
      class = "BackendAdapter"
    ))
  }
  structure(
    list(
      validate = function(resolved_analysis, params) {
        triomics_integration_eligibility(resolved_analysis, params)
      },
      fit = function(resolved_analysis, params) {
        triomics_integration_pca_backend_fit(resolved_analysis, params)
      },
      predict = function(backend_fit, new_resolved_analysis, params) {
        triomics_integration_pca_backend_predict(backend_fit, new_resolved_analysis, params)
      },
      extract = function(backend_fit, context, params) {
        triomics_integration_pca_backend_extract(backend_fit, context, params)
      },
      evaluation_modes = character()
    ),
    class = "BackendAdapter"
  )
}

triomics_integration_matrix_hashes <- function(resolved_analysis, block_ids) {
  stats::setNames(lapply(block_ids, function(block_id) {
    digest::digest(
      resolved_analysis$blocks[[block_id]]$assay,
      algo = "sha256", serialize = TRUE
    )
  }), block_ids)
}

triomics_integration_c08_fit_state <- function(backend_fit, method, input_hashes,
                                                training_unit_ids) {
  metadata <- backend_fit$metadata
  if (!is.list(metadata)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "C-08 requires adapter fit metadata.", "backend_fit.metadata"
    )
  }
  state_hash <- metadata$state_hash
  if (is.null(state_hash)) state_hash <- backend_fit$result_params$training_state_hash
  if (is.null(state_hash) && is.list(backend_fit$result_params$state)) {
    state_hash <- backend_fit$result_params$state$state_hash
  }
  if (is.null(state_hash) && identical(method, "concat_pca")) {
    state <- backend_fit$native_state$state
    if (!is.list(state) || !identical(state$training_unit_ids, training_unit_ids) ||
        is.null(state$rotation) || is.null(state$feature_means) ||
        is.null(state$feature_scales) || is.null(state$prcomp_center)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "PCA cannot establish fit identity without its immutable fitted native state.",
        "backend_fit.native_state.state"
      )
    }
    contract <- triomics_c08_method_contract(method)
    version <- metadata$backend_version
    if (is.null(version)) version <- R.version.string
    state_hash <- digest::digest(list(
      schema_version = "C-08-native-fit-state-1",
      method = method,
      native_method = contract$native_method,
      native_version = version,
      training_unit_ids = training_unit_ids,
      input_hashes = input_hashes,
      fitted_native_state = state
    ), algo = "sha256", serialize = TRUE)
    backend_fit$metadata$state_hash <- state_hash
  }
  if (!triomics_c08_valid_sha256(state_hash)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "The adapter fit has no authoritative native-state identity for C-08.",
      "backend_fit.metadata.state_hash"
    )
  }
  backend_fit
}

triomics_integration_c08_training_y <- function(resolved_analysis, integration_spec,
                                                c08_context, backend_fit,
                                                training_unit_ids, contract) {
  if (!isTRUE(contract$uses_y)) return(NULL)
  variable_id <- c08_context$training_y_variable_id
  if (is.null(variable_id) && is.list(c08_context$training_y)) {
    variable_id <- c08_context$training_y$variable_id
  }
  if (is.null(variable_id)) variable_id <- integration_spec$outcome
  if (is.null(variable_id)) variable_id <- integration_spec$response_block
  if (is.null(variable_id)) variable_id <- backend_fit$native_state$response_block
  if (!is.character(variable_id) || length(variable_id) != 1L || is.na(variable_id) ||
      !nzchar(trimws(variable_id))) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "A supervised C-08 fit requires the declared identity of its training Y.",
      "training_y.variable_id"
    )
  }
  values <- resolved_analysis$outcome
  if (is.matrix(values)) {
    if (ncol(values) != 1L) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "C-08 supports only the fitted scalar training Y.", "training_y"
      )
    }
    row_ids <- rownames(values)
    if (is.null(row_ids)) row_ids <- names(resolved_analysis$outcome)
    if (is.null(row_ids)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Training Y matrix rows must identify fitted units.", "training_y"
      )
    }
    values <- stats::setNames(values[, 1L], row_ids)
  }
  if (!is.atomic(values) || !length(values)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "The adapter's resolved training Y is unavailable.", "training_y"
    )
  }
  value_ids <- names(values)
  if (is.null(value_ids)) {
    resolved_ids <- resolved_analysis$unit_ids
    if (!identical(resolved_ids, training_unit_ids) || length(values) != length(training_unit_ids)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Unkeyed training Y cannot be aligned to the fitted units.", "training_y"
      )
    }
    names(values) <- training_unit_ids
  }
  if (anyDuplicated(names(values)) || !setequal(names(values), training_unit_ids) ||
      length(values) != length(training_unit_ids)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Training Y must cover the fitted units exactly once.", "training_y"
    )
  }
  list(variable_id = variable_id, values = values[training_unit_ids])
}

triomics_integration_bind_c08_handoff <- function(backend_fit, result, context) {
  result <- triomics_validate_integration_result(result)
  if (!inherits(backend_fit, "BackendFit") || !is.list(context) ||
      !is.list(context$input_hashes) || !is.list(context$selection) ||
      is.null(context$unit_map)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Same-fit C-08 binding requires a BackendFit and complete public context."
    )
  }
  contract <- triomics_c08_method_contract(result$backend)
  training_unit_ids <- triomics_c08_result_training_ids(result)
  input_hashes <- triomics_c08_validate_input_hashes(context$input_hashes, names(result$loadings))
  backend_fit <- triomics_integration_c08_fit_state(
    backend_fit, result$backend, input_hashes, training_unit_ids
  )
  if (!identical(backend_fit$metadata$backend, result$backend) ||
      !identical(backend_fit$metadata$training_unit_ids, training_unit_ids) ||
      !is.list(backend_fit$result_params)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Adapter metadata and result training identities disagree."
    )
  }
  result_params <- result$params
  result_provenance <- result_params$provenance
  result_params$provenance <- NULL
  if (!identical(result_params, backend_fit$result_params) || !is.list(result_provenance) ||
      !identical(result_provenance$matrix_sha256, input_hashes) ||
      !identical(result_provenance$unit_map, training_unit_ids) ||
      !identical(result_provenance$study_id, context$study_id) ||
      !identical(result_provenance$analysis_id, context$analysis_id) ||
      !identical(result_provenance$preparation_context_id, context$preparation_context_id) ||
      !identical(result_provenance$representation_id, context$representation_id)) {
    triomics_integration_abort(
      "C_SCORE_FIT_BINDING_MISMATCH",
      "C-08 context does not match the exact same-fit IntegrationResult provenance."
    )
  }
  if (!is.character(context$selection$ancestry) || length(context$selection$ancestry) != 1L ||
      is.na(context$selection$ancestry) || !nzchar(trimws(context$selection$ancestry))) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Explicit selection ancestry is required.", "selection"
    )
  }
  if (isTRUE(contract$uses_y) && !identical(context$selection$ancestry, "trained_on_outcome")) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "A supervised integration must record trained_on_outcome selection ancestry.",
      "selection.ancestry"
    )
  }
  training_y <- if (isTRUE(contract$uses_y)) context$training_y else NULL
  if (isTRUE(contract$uses_y) && is.null(training_y)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Supervised integration requires its same-fit training Y.", "training_y"
    )
  }
  fit_scores <- triomics_c08_fit_scores(result, backend_fit)
  handoff <- list(
    schema_version = "C-08-fit-handoff-1",
    qualification = "same_fit_adapter_handoff",
    fit_state_sha256 = backend_fit$metadata$state_hash,
    training_scores_sha256 = triomics_c08_sha256(triomics_matrix_payload(fit_scores)),
    training_unit_ids = as.list(training_unit_ids),
    input_hashes = input_hashes,
    training_y_sha256 = triomics_c08_y_binding_hash(training_y, training_unit_ids),
    selection_sha256 = triomics_c08_sha256(context$selection),
    unit_map_sha256 = triomics_c08_sha256(context$unit_map),
    representation_id = context$representation_id
  )
  handoff$handoff_sha256 <- triomics_c08_sha256(handoff)
  backend_fit$metadata$c08_handoff <- handoff
  list(backend_fit = backend_fit, context = context)
}

triomics_integration_fit_once <- function(resolved_analysis, integration_spec,
                                          c08_context = NULL, adapter = NULL) {
  if (!is.list(resolved_analysis) || !is.list(integration_spec)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_INPUT_INVALID",
      "Resolved analysis and integration specification must be lists."
    )
  }
  if (is.list(integration_spec$block_ids) && length(integration_spec$block_ids) &&
      all(vapply(integration_spec$block_ids, function(block_id) {
        is.character(block_id) && length(block_id) == 1L && !is.na(block_id)
      }, logical(1)))) {
    integration_spec$block_ids <- unlist(integration_spec$block_ids, use.names = FALSE)
  }
  method <- integration_spec$method
  if (!is.character(method) || length(method) != 1L || is.na(method) || !nzchar(method)) {
    triomics_integration_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "An explicit registered integration method is required.", key = "method"
    )
  }
  if (is.null(adapter)) adapter <- triomics_integration_backend(method)
  if (!inherits(adapter, "BackendAdapter") || !is.function(adapter$validate) ||
      !is.function(adapter$fit) || !is.function(adapter$extract)) {
    triomics_integration_abort(
      "C_SCORE_BACKEND_UNAVAILABLE", "A registered BackendAdapter with validate, fit and extract is required."
    )
  }
  eligibility <- adapter$validate(resolved_analysis, integration_spec)
  backend_fit <- adapter$fit(resolved_analysis, integration_spec)
  matrix_hashes <- triomics_integration_matrix_hashes(resolved_analysis, eligibility$block_ids)
  study_id <- eligibility$study_id
  if (is.null(study_id)) study_id <- resolved_analysis$study_id
  preparation_context_id <- eligibility$preparation_context_id
  if (is.null(preparation_context_id)) preparation_context_id <- resolved_analysis$preparation_context_id
  representation_id <- eligibility$representation_id
  if (is.null(representation_id)) representation_id <- resolved_analysis$representation_id
  if (is.null(representation_id)) representation_id <- resolved_analysis$representation
  training_unit_ids <- eligibility$training_unit_ids
  if (is.null(training_unit_ids)) training_unit_ids <- eligibility$unit_ids
  if (is.null(training_unit_ids)) training_unit_ids <- eligibility$units
  if (is.null(training_unit_ids)) training_unit_ids <- backend_fit$metadata$training_unit_ids
  if (is.null(training_unit_ids)) training_unit_ids <- backend_fit$result_params$training_unit_ids
  if (is.null(training_unit_ids) && is.list(backend_fit$native_state$state)) {
    training_unit_ids <- backend_fit$native_state$state$training_unit_ids
  }
  required_views <- eligibility$required_views
  if (is.null(required_views)) required_views <- integration_spec$required_views
  if (is.null(required_views)) required_views <- eligibility$block_ids
  if (!is.null(c08_context)) {
    if (!is.character(training_unit_ids) || !length(training_unit_ids)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "C-08 fit identity requires explicit fitted training units.",
        "training_unit_ids"
      )
    }
    backend_fit <- triomics_integration_c08_fit_state(
      backend_fit, method, matrix_hashes, training_unit_ids
    )
  }
  extraction_context <- list(
    study_id = study_id,
    analysis_id = resolved_analysis$analysis_id,
    preparation_context_id = preparation_context_id,
    representation_id = representation_id
  )
  extracted <- adapter$extract(
    backend_fit,
    context = extraction_context,
    params = integration_spec
  )
  result <- triomics_validate_integration_result(extracted$result)

  source_roles <- stats::setNames(lapply(eligibility$block_ids, function(block_id) {
    block <- resolved_analysis$blocks[[block_id]]
    value <- if (!is.null(block$source_roles)) block$source_roles else block$source_role
    if (is.null(value)) list(status = "not_supplied_by_resolved_analysis") else {
      list(status = "supplied", value = value)
    }
  }), eligibility$block_ids)
  source_selection <- stats::setNames(lapply(eligibility$block_ids, function(block_id) {
    block <- resolved_analysis$blocks[[block_id]]
    value <- if (!is.null(block$source_selection)) block$source_selection else block$selection_history
    if (is.null(value)) list(status = "not_supplied_by_resolved_analysis") else {
      list(status = "supplied", value = value)
    }
  }), eligibility$block_ids)
  outcome_eligibility <- resolved_analysis$outcome_eligibility
  if (is.null(outcome_eligibility)) {
    outcome_eligibility <- list(status = "not_supplied_by_resolved_analysis")
  }
  task_mode <- integration_spec$task_mode
  if (is.null(task_mode)) task_mode <- integration_spec$evaluation$mode
  if (is.null(task_mode)) task_mode <- "exploratory"
  result$params$provenance <- list(
    study_id = study_id,
    analysis_id = resolved_analysis$analysis_id,
    preparation_context_id = preparation_context_id,
    representation_id = representation_id,
    unit_map = training_unit_ids,
    feature_map = eligibility$block_feature_keys,
    matrix_sha256 = matrix_hashes,
    source_roles = source_roles,
    source_selection = source_selection,
    outcome_eligibility = outcome_eligibility,
    task_mode = task_mode,
    required_views = required_views
  )
  result <- triomics_validate_integration_result(result)
  context <- NULL
  if (!is.null(c08_context)) {
    context <- c08_context
    context$study_id <- study_id
    context$analysis_id <- resolved_analysis$analysis_id
    context$preparation_context_id <- preparation_context_id
    context$representation_id <- representation_id
    context$input_hashes <- matrix_hashes
    context$integration_id <- if (is.null(context$integration_id)) {
      integration_spec$integration_id
    } else context$integration_id
    if (is.null(context$integration_id)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "C-08 requires an integration identity.", "integration_id"
      )
    }
    contract <- triomics_c08_method_contract(method)
    training_unit_ids <- triomics_c08_result_training_ids(result)
    context$training_y <- triomics_integration_c08_training_y(
      resolved_analysis, integration_spec, context, backend_fit,
      training_unit_ids, contract
    )
    if (isTRUE(contract$uses_y)) {
      context$selection$ancestry <- "trained_on_outcome"
      context$selection$inspected_outcome <- context$training_y$variable_id
    }
    if (is.null(context$available_at_time)) context$available_at_time <- "after_integration_fit"
    if (is.null(context$dependency_group)) {
      context$dependency_group <- paste(context$study_id, context$integration_id, sep = "::")
    }
    bound <- triomics_integration_bind_c08_handoff(backend_fit, result, context)
    backend_fit <- bound$backend_fit
    context <- bound$context
  }
  list(
    result = result,
    backend_fit = backend_fit,
    context = context,
    eligibility = eligibility,
    sidecars = extracted$sidecars
  )
}

integrate_blocks <- function(resolved_analysis, integration_spec) {
  triomics_integration_fit_once(resolved_analysis, integration_spec)$result
}
