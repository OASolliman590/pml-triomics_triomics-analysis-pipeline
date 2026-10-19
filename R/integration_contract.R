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
  c("concat_pca", "concat_plsda", "concat_pls", "mixomics_diablo", "mixomics_block_spls")
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

integrate_blocks <- function(resolved_analysis, integration_spec) {
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
  adapter <- triomics_integration_backend(method)
  eligibility <- adapter$validate(resolved_analysis, integration_spec)
  backend_fit <- adapter$fit(resolved_analysis, integration_spec)
  extracted <- adapter$extract(
    backend_fit,
    context = list(
      study_id = eligibility$study_id,
      analysis_id = resolved_analysis$analysis_id,
      preparation_context_id = eligibility$preparation_context_id,
      representation_id = eligibility$representation_id
    ),
    params = integration_spec
  )
  result <- triomics_validate_integration_result(extracted$result)

  matrix_hashes <- stats::setNames(lapply(eligibility$block_ids, function(block_id) {
    digest::digest(resolved_analysis$blocks[[block_id]]$assay, algo = "sha256", serialize = TRUE)
  }), eligibility$block_ids)
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
    study_id = eligibility$study_id,
    analysis_id = resolved_analysis$analysis_id,
    preparation_context_id = eligibility$preparation_context_id,
    representation_id = eligibility$representation_id,
    unit_map = eligibility$unit_ids,
    feature_map = eligibility$block_feature_keys,
    matrix_sha256 = matrix_hashes,
    source_roles = source_roles,
    source_selection = source_selection,
    outcome_eligibility = outcome_eligibility,
    task_mode = task_mode,
    required_views = eligibility$required_views
  )
  triomics_validate_integration_result(result)
}
