triomics_block_spls_abort <- function(code, message, key = NULL,
                                      remediation = "Review the continuous block.spls inputs and frozen contract.") {
  triomics_abort(
    code, message, key = key, module = "evaluation", stage = "C",
    remediation = remediation
  )
}

triomics_block_spls_component_ids <- function(n_components) {
  paste0("comp", seq_len(n_components))
}

triomics_block_spls_ids <- function(value, label, error_code) {
  if (is.list(value)) value <- unlist(value, use.names = FALSE)
  if (!is.character(value) || !length(value) || anyNA(value) ||
      any(!nzchar(trimws(value))) || anyDuplicated(value)) {
    triomics_block_spls_abort(error_code, paste0(label, " must be unique, non-blank names."), key = label)
  }
  value
}

triomics_block_spls_response <- function(response, units, response_block) {
  if (is.null(response)) {
    triomics_block_spls_abort("C_EVAL_RESPONSE_INVALID", "A training response is required.", key = response_block)
  }
  if (is.factor(response) || is.character(response) || is.logical(response)) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID",
      paste0("Continuous block.spls regression requires a scalar numeric response; categorical responses ",
             "belong to the categorical DIABLO route (mixomics_diablo), not this regression adapter."),
      key = response_block
    )
  }
  response_names <- names(response)
  if (is.matrix(response)) {
    if (ncol(response) != 1L) {
      triomics_block_spls_abort(
        "C_EVAL_RESPONSE_INVALID",
        paste0("Continuous block.spls requires one scalar response column, not ", ncol(response),
               "; multivariate responses need a separate scope decision."),
        key = response_block
      )
    }
    response_names <- rownames(response)
    response <- response[, 1L]
  }
  if (!is.numeric(response) || is.complex(response) || length(response) != length(units)) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID", "Continuous block.spls requires one numeric response per training unit.",
      key = response_block
    )
  }
  if (!is.null(response_names)) {
    if (anyDuplicated(response_names) || !setequal(response_names, units)) {
      triomics_block_spls_abort(
        "C_EVAL_RESPONSE_INVALID", "Response names must match the training units.", key = response_block
      )
    }
    response <- response[units]
  }
  if (anyNA(response) || any(!is.finite(response))) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID", "Continuous block.spls requires finite training responses.", key = response_block
    )
  }
  if (!stats::sd(response) > .Machine$double.eps) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID", "A constant training response has no regression signal.", key = response_block
    )
  }
  stats::setNames(as.numeric(response), units)
}

triomics_block_spls_predictors <- function(resolved_analysis, block_ids, units) {
  if (!is.list(resolved_analysis$blocks) || !identical(names(resolved_analysis$blocks), block_ids)) {
    triomics_block_spls_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING",
      "Every declared predictor block must be supplied in the declared stable order.",
      key = "blocks"
    )
  }
  stats::setNames(lapply(block_ids, function(block_id) {
    block <- resolved_analysis$blocks[[block_id]]
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        length(dim(assay)) != 2L || nrow(assay) < 1L || ncol(assay) < 1L ||
        is.null(rownames(assay)) || is.null(colnames(assay)) ||
        anyNA(rownames(assay)) || anyNA(colnames(assay)) ||
        anyDuplicated(rownames(assay)) || anyDuplicated(colnames(assay))) {
      triomics_block_spls_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING", paste0("Block '", block_id, "' needs a keyed numeric feature-by-unit matrix."),
        key = block_id
      )
    }
    if (!identical(as.character(block$feature_keys), rownames(assay)) ||
        !identical(as.character(block$sample_ids), colnames(assay)) ||
        !setequal(colnames(assay), units)) {
      triomics_block_spls_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING",
        paste0("Block '", block_id, "' feature or unit identity does not match the training map."),
        key = block_id
      )
    }
    assay <- assay[, units, drop = FALSE]
    if (any(!is.finite(assay))) {
      triomics_block_spls_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING",
        paste0("Block '", block_id, "' has a non-finite training predictor; values are not imputed."),
        key = block_id
      )
    }
    assay
  }), block_ids)
}

triomics_block_spls_design <- function(design, block_ids, response_block) {
  ids <- c(block_ids, response_block)
  if (is.list(design) && !is.data.frame(design)) {
    if (is.null(names(design)) || anyDuplicated(names(design))) {
      triomics_block_spls_abort("C_SCORE_PARAMETER", "A block design map must have unique row names.", key = "design_matrix")
    }
    row_ids <- names(design)
    columns <- names(design[[1L]])
    rows <- lapply(row_ids, function(row_id) {
      row <- design[[row_id]]
      if (!is.list(row) && !is.numeric(row)) {
        triomics_block_spls_abort("C_SCORE_PARAMETER", "Each design row must be a named numeric map.", key = "design_matrix")
      }
      if (!setequal(names(row), columns)) {
        triomics_block_spls_abort("C_SCORE_PARAMETER", "Design rows must share one column map.", key = "design_matrix")
      }
      as.numeric(unlist(row[columns], use.names = FALSE))
    })
    design <- do.call(rbind, rows)
    dimnames(design) <- list(row_ids, columns)
  }
  if (!is.matrix(design) || !is.numeric(design) || is.null(rownames(design)) ||
      is.null(colnames(design)) || !setequal(rownames(design), ids) || !setequal(colnames(design), ids)) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER",
      "The block.spls design must be a named square matrix over predictor blocks and the response.",
      key = "design_matrix"
    )
  }
  design <- design[ids, ids, drop = FALSE]
  if (anyNA(design) || any(design < 0 | design > 1) || any(diag(design) != 0) ||
      any(design != t(design))) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER",
      "The block.spls design must be symmetric, in [0, 1], with a zero diagonal.",
      key = "design_matrix"
    )
  }
  design
}

triomics_block_spls_keepX <- function(keepX, block_ids, response_block, ncomp, feature_counts) {
  if (!is.list(keepX) || is.null(names(keepX))) {
    triomics_block_spls_abort("C_SCORE_PARAMETER", "keepX must be a named map of predictor blocks.", key = "keepX")
  }
  if (response_block %in% names(keepX)) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID",
      "The response block cannot receive predictor keepX; response sparsity is declared through keepY.",
      key = response_block
    )
  }
  if (!setequal(names(keepX), block_ids)) {
    triomics_block_spls_abort("C_SCORE_PARAMETER", "keepX must name every predictor block exactly once.", key = "keepX")
  }
  stats::setNames(lapply(block_ids, function(block_id) {
    value <- keepX[[block_id]]
    if (is.numeric(value) && length(value) == 1L) value <- rep(value, ncomp)
    if (!is.numeric(value) || length(value) != ncomp || anyNA(value) ||
        any(value < 1) || any(value != trunc(value)) || any(value > feature_counts[[block_id]])) {
      triomics_block_spls_abort(
        "C_SCORE_PARAMETER",
        paste0("keepX for '", block_id, "' must give one count per component (or one count recycled), ",
               "each an integer within the retained features."),
        key = block_id
      )
    }
    as.integer(value)
  }), block_ids)
}

triomics_block_spls_validate <- function(resolved_analysis, params) {
  if (!is.list(params)) {
    triomics_block_spls_abort("C_SCORE_PARAMETER", "Continuous block.spls needs an explicit parameter list.", key = "integration")
  }
  response_block <- if (is.null(params[["response_block"]])) params[["outcome"]] else params[["response_block"]]
  if (!is.character(response_block) || length(response_block) != 1L || is.na(response_block) ||
      !nzchar(trimws(response_block))) {
    triomics_block_spls_abort("C_EVAL_RESPONSE_INVALID", "A declared response block name is required.", key = "response_block")
  }
  block_ids <- triomics_block_spls_ids(params[["block_ids"]], "block_ids", "C_EVAL_REQUIRED_VIEW_MISSING")
  if (response_block %in% block_ids) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID",
      "The response block cannot also be a predictor block; common loadings, variance and selection use predictors only.",
      key = response_block
    )
  }
  mode <- if (is.null(params[["mode"]])) "regression" else params[["mode"]]
  if (!identical(mode, "regression")) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER", "block.spls is declared in regression mode for one continuous response.", key = "mode"
    )
  }
  if (!is.logical(params[["scale_features"]]) || length(params[["scale_features"]]) != 1L ||
      is.na(params[["scale_features"]])) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER", "Scaling must be an explicit logical setting shared by predictors and response.",
      key = "scale_features"
    )
  }
  if (!identical(params[["block_scaling"]], "none")) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER", "Only block_scaling = 'none' is supported for continuous block.spls.", key = "block_scaling"
    )
  }
  if (!identical(params[["prediction_rule"]], "AveragedPredict")) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER", "The declared block.spls prediction rule must be the native AveragedPredict.",
      key = "prediction_rule"
    )
  }
  if (!is.null(params[["score_definition"]]) && !identical(params[["score_definition"]], "consensus_mean")) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER", "Continuous block.spls only supports the consensus_mean common score.",
      key = "score_definition"
    )
  }
  keep_y <- params[["keepY"]]
  if (!is.numeric(keep_y) || length(keep_y) != 1L || is.na(keep_y) || keep_y != 1) {
    triomics_block_spls_abort(
      "C_SCORE_PARAMETER",
      "keepY must be declared as 1 for the single continuous response column.",
      key = "keepY"
    )
  }
  ncomp <- if (is.null(params[["ncomp"]])) params[["n_components"]] else params[["ncomp"]]
  if (!is.numeric(ncomp) || length(ncomp) != 1L || is.na(ncomp) ||
      !is.finite(ncomp) || ncomp < 1 || ncomp != trunc(ncomp)) {
    triomics_block_spls_abort(
      "C_EVAL_COMPONENT_RANK", "Requested component count must be a positive integer.", key = "ncomp"
    )
  }
  if (!is.list(resolved_analysis) || !is.character(resolved_analysis$study_id) ||
      length(resolved_analysis$study_id) != 1L || !nzchar(resolved_analysis$study_id) ||
      !is.character(resolved_analysis$preparation_context_id) ||
      length(resolved_analysis$preparation_context_id) != 1L ||
      !nzchar(resolved_analysis$preparation_context_id)) {
    triomics_block_spls_abort(
      "C_EVAL_RESPONSE_INVALID", "Study and preparation context identity are required.",
      key = "preparation_context_id"
    )
  }
  units <- triomics_block_spls_ids(resolved_analysis$unit_ids, "unit_ids", "C_EVAL_RESPONSE_INVALID")
  response <- triomics_block_spls_response(resolved_analysis$outcome, units, response_block)
  predictors <- triomics_block_spls_predictors(resolved_analysis, block_ids, units)

  input_keys <- list()
  retained <- list()
  excluded <- list()
  x_blocks <- list()
  for (block_id in block_ids) {
    values <- t(predictors[[block_id]])
    feature_sd <- apply(values, 2L, stats::sd)
    keep <- is.finite(feature_sd) & feature_sd > .Machine$double.eps
    if (!any(keep)) {
      triomics_block_spls_abort(
        "C_EVAL_COMPONENT_RANK", paste0("Block '", block_id, "' has no non-constant training features."),
        key = block_id
      )
    }
    input_keys[[block_id]] <- colnames(values)
    retained[[block_id]] <- colnames(values)[keep]
    excluded[[block_id]] <- colnames(values)[!keep]
    x_blocks[[block_id]] <- values[, keep, drop = FALSE]
  }
  available_rank <- vapply(x_blocks, function(values) {
    qr(scale(values, center = TRUE, scale = FALSE), tol = 1e-10)$rank
  }, integer(1))
  if (ncomp > min(available_rank)) {
    triomics_block_spls_abort(
      "C_EVAL_COMPONENT_RANK",
      paste0("Requested ncomp=", as.integer(ncomp), " exceeds the smallest training block rank ",
             min(available_rank), "; components are not silently reduced."),
      key = "ncomp"
    )
  }
  feature_counts <- vapply(retained, length, integer(1))
  keepX <- triomics_block_spls_keepX(params[["keepX"]], block_ids, response_block, as.integer(ncomp), feature_counts)
  design_input <- if (is.null(params[["design"]])) params[["design_matrix"]] else params[["design"]]
  design <- triomics_block_spls_design(design_input, block_ids, response_block)
  native_design <- design
  dimnames(native_design) <- list(c(block_ids, "Y"), c(block_ids, "Y"))

  list(
    block_ids = block_ids, response_block = response_block, unit_ids = units,
    response = response, x = x_blocks, input_keys = input_keys, retained = retained,
    excluded = excluded, ncomp = as.integer(ncomp), keepX = keepX, keepY = 1L,
    scale_features = params[["scale_features"]], design = design, native_design = native_design,
    study_id = resolved_analysis$study_id,
    preparation_context_id = resolved_analysis$preparation_context_id,
    preparation_state_hashes = resolved_analysis$preparation_state_hashes
  )
}

triomics_block_spls_named_scores <- function(value, unit_ids, component_ids) {
  rownames(value) <- unit_ids
  colnames(value) <- component_ids
  value
}

triomics_block_spls_variance <- function(model, block_ids, n_components) {
  component_ids <- triomics_block_spls_component_ids(n_components)
  values <- matrix(
    NA_real_, nrow = length(block_ids), ncol = n_components,
    dimnames = list(block_ids, component_ids)
  )
  for (block_id in block_ids) {
    block_x <- model$X[[block_id]]
    scores <- model$variates[[block_id]]
    total <- sum(block_x^2)
    if (!is.finite(total) || total <= 0) {
      triomics_block_spls_abort(
        "C_VARIANCE_DENOMINATOR", paste0("Block '", block_id, "' has no positive training denominator."),
        key = block_id
      )
    }
    recomputed <- vapply(seq_len(n_components), function(h) {
      projected <- crossprod(scores[, h, drop = FALSE], block_x)
      sum(projected^2) / sum(scores[, h]^2) / total
    }, numeric(1))
    native <- as.numeric(model$prop_expl_var[[block_id]])
    if (length(native) != n_components || any(!is.finite(native)) ||
        any(native < 0 | native > 1) || any(!is.finite(recomputed)) ||
        max(abs(native - recomputed)) > 1e-10) {
      triomics_block_spls_abort(
        "C_VARIANCE_UNAVAILABLE",
        paste0("Native block.spls proportions for '", block_id,
               "' do not verify against the training block; no value is clamped or filled."),
        key = block_id
      )
    }
    values[block_id, ] <- native
  }
  attr(values, "metric") <- "native_block_projection_on_variate"
  attr(values, "units") <- "proportion"
  attr(values, "semantics") <- "per_component_projection_not_nested"
  values
}

triomics_block_spls_prediction_disposition <- function(predictions) {
  invalid <- !is.finite(predictions)
  predictions[invalid] <- NA_real_
  status <- stats::setNames(ifelse(invalid, "failed", "predicted"), names(predictions))
  reason_code <- stats::setNames(
    ifelse(invalid, "nonfinite_native_prediction", NA_character_), names(predictions)
  )
  list(predictions = predictions, status = status, reason_code = reason_code)
}

triomics_block_spls_fit <- function(resolved_analysis, params) {
  eligibility <- triomics_block_spls_validate(resolved_analysis, params)
  block_ids <- eligibility$block_ids
  units <- eligibility$unit_ids
  ncomp <- eligibility$ncomp
  component_ids <- triomics_block_spls_component_ids(ncomp)
  response <- matrix(
    eligibility$response, ncol = 1L,
    dimnames = list(units, eligibility$response_block)
  )
  native_fit <- tryCatch(
    mixOmics::block.spls(
      X = eligibility$x, Y = response, ncomp = ncomp,
      keepX = eligibility$keepX, keepY = rep(eligibility$keepY, ncomp),
      design = eligibility$native_design, mode = "regression",
      scale = eligibility$scale_features, near.zero.var = FALSE, all.outputs = TRUE
    ),
    error = function(error) {
      triomics_block_spls_abort(
        "C_EVAL_NATIVE_FIT", paste0("mixOmics native block.spls fit failed: ", conditionMessage(error)),
        key = "ncomp"
      )
    }
  )
  variates <- stats::setNames(lapply(block_ids, function(block_id) {
    value <- native_fit$variates[[block_id]]
    if (!is.matrix(value) || any(!is.finite(value))) {
      triomics_block_spls_abort("C_EVAL_NATIVE_FIT", "Native block.spls returned non-finite predictor variates.", key = block_id)
    }
    triomics_block_spls_named_scores(value, units, component_ids)
  }), block_ids)
  loadings <- stats::setNames(lapply(block_ids, function(block_id) {
    value <- native_fit$loadings[[block_id]]
    if (!is.matrix(value) || any(!is.finite(value)) || nrow(value) != length(eligibility$retained[[block_id]])) {
      triomics_block_spls_abort("C_EVAL_NATIVE_FIT", "Native block.spls returned unusable predictor loadings.", key = block_id)
    }
    rownames(value) <- eligibility$retained[[block_id]]
    colnames(value) <- component_ids
    value
  }), block_ids)
  variance <- triomics_block_spls_variance(native_fit, block_ids, ncomp)
  consensus_state <- make_consensus_state(
    variates, loadings, units, params = list(required_views = block_ids)
  )
  selected <- stats::setNames(lapply(block_ids, function(block_id) {
    value <- loadings[[block_id]]
    rownames(value)[rowSums(value != 0) > 0]
  }), block_ids)

  response_diagnostics <- list(
    block = eligibility$response_block,
    native_block = "Y",
    loadings = {
      value <- native_fit$loadings$Y
      rownames(value) <- eligibility$response_block
      colnames(value) <- component_ids
      value
    },
    variates = triomics_block_spls_named_scores(native_fit$variates$Y, units, component_ids),
    prop_expl_var = stats::setNames(as.numeric(native_fit$prop_expl_var$Y), component_ids),
    keepY = eligibility$keepY
  )
  native_version <- as.character(utils::packageVersion("mixOmics"))
  training_transform <- list(
    center = stats::setNames(lapply(block_ids, function(block_id) {
      attr(native_fit$X[[block_id]], "scaled:center")
    }), block_ids),
    scale = stats::setNames(lapply(block_ids, function(block_id) {
      attr(native_fit$X[[block_id]], "scaled:scale")
    }), block_ids),
    response_center = attr(native_fit$X$Y, "scaled:center"),
    response_scale = attr(native_fit$X$Y, "scaled:scale"),
    scale_features = eligibility$scale_features,
    training_unit_ids = units
  )
  state_hash <- digest::digest(
    list(
      method = "mixomics_block_spls", native_version = native_version,
      training_unit_ids = units, response = unname(eligibility$response),
      input_keys = eligibility$input_keys, retained = eligibility$retained,
      training_transform = training_transform, keepX = eligibility$keepX,
      keepY = eligibility$keepY, design = eligibility$design, ncomp = ncomp,
      variates = variates, loadings = loadings, variance = variance,
      signs = consensus_state$signs, preprocessing_state_hashes = eligibility$preparation_state_hashes
    ),
    algo = "sha256", serialize = TRUE
  )
  feature_flow <- stats::setNames(lapply(block_ids, function(block_id) {
    list(
      input = unname(eligibility$input_keys[[block_id]]),
      retained = unname(eligibility$retained[[block_id]]),
      excluded_zero_variance = unname(eligibility$excluded[[block_id]]),
      rule = "fold_prepared_nonconstant_features"
    )
  }), block_ids)
  result_params <- list(
    native_method = "mixOmics::block.spls",
    task = "continuous_regression",
    classification = FALSE,
    mode = "regression",
    response_block = eligibility$response_block,
    keepX = eligibility$keepX,
    keepY = eligibility$keepY,
    scale_features = eligibility$scale_features,
    back_transform = "native_prediction_on_original_response_scale",
    block_scaling = "none",
    design = eligibility$design,
    prediction_rule = "AveragedPredict",
    score_definition = "equal_weight_mean_of_signed_training_standardized_native_scores",
    loading_role = "native_predictor_loadings_sign_aligned_to_consensus",
    selection_rule = "executed_native_nonzero_support",
    consensus_anchor_view = consensus_state$anchor_view,
    training_unit_ids = units,
    training_state_hash = state_hash,
    feature_flow = feature_flow,
    requested_n_components = ncomp,
    retained_n_components = ncomp,
    variance = list(
      metric = "native_block_projection_on_variate", units = "proportion",
      denominator = "scaled_training_block_frobenius_sum_of_squares",
      semantics = "per_component_projection_not_nested"
    ),
    response_diagnostics = list(
      block = eligibility$response_block,
      status = "native_retained_outside_common_object"
    ),
    versions = list(
      mixOmics = native_version,
      R = paste(R.version$major, R.version$minor, sep = ".")
    )
  )
  structure(
    list(
      native_state = list(
        model = native_fit,
        block_ids = block_ids,
        response_block = eligibility$response_block,
        ncomp = ncomp,
        component_ids = component_ids,
        unit_ids = units,
        input_keys = eligibility$input_keys,
        retained = eligibility$retained,
        training_scores = variates,
        block_loadings = loadings,
        selected = selected,
        consensus_state = consensus_state,
        response_diagnostics = response_diagnostics,
        training_transform = training_transform,
        keepX = eligibility$keepX,
        keepY = eligibility$keepY,
        preparation_state_hashes = eligibility$preparation_state_hashes
      ),
      selected = selected,
      result_params = result_params,
      metadata = list(
        backend = "mixomics_block_spls", native_method = "mixOmics::block.spls",
        native_version = native_version, model_role = "continuous_regression_prediction",
        training_unit_ids = units,
        preparation_context_id = eligibility$preparation_context_id,
        state_hash = state_hash
      )
    ),
    class = "BackendFit"
  )
}

triomics_block_spls_predict <- function(backend_fit, new_resolved_analysis, params) {
  if (!inherits(backend_fit, "BackendFit") || is.null(backend_fit$native_state$model)) {
    triomics_block_spls_abort(
      "C_EVAL_PREDICTION_UNSUPPORTED", "Prediction requires a fitted continuous block.spls BackendFit.",
      key = "backend_fit"
    )
  }
  state <- backend_fit$native_state
  block_ids <- state$block_ids
  if (!is.list(new_resolved_analysis) || !is.list(new_resolved_analysis$blocks) ||
      !identical(names(new_resolved_analysis$blocks), block_ids)) {
    triomics_block_spls_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING", "Prediction requires every fitted predictor block in fitted order.",
      key = "blocks"
    )
  }
  units <- new_resolved_analysis$unit_ids
  if (!is.character(units) || !length(units) || anyNA(units) || anyDuplicated(units)) {
    triomics_block_spls_abort("C_EVAL_PREDICTION_UNSUPPORTED", "Prediction unit ids must be unique.", key = "unit_ids")
  }
  newdata <- stats::setNames(lapply(block_ids, function(block_id) {
    block <- new_resolved_analysis$blocks[[block_id]]
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        !identical(as.character(block$feature_keys), rownames(assay)) ||
        !identical(as.character(block$sample_ids), colnames(assay)) ||
        !setequal(colnames(assay), units)) {
      triomics_block_spls_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED",
        paste0("Prediction input for block '", block_id, "' does not match the frozen unit/feature map."),
        key = block_id
      )
    }
    if (!identical(rownames(assay), state$input_keys[[block_id]])) {
      triomics_block_spls_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED",
        paste0("Prediction feature order for block '", block_id, "' differs from its training state."),
        key = block_id
      )
    }
    values <- t(assay[state$retained[[block_id]], units, drop = FALSE])
    if (any(!is.finite(values))) {
      triomics_block_spls_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING",
        paste0("Prediction input for block '", block_id, "' contains a non-finite predictor; values are not imputed."),
        key = block_id
      )
    }
    values
  }), block_ids)
  native <- tryCatch(
    stats::predict(state$model, newdata = newdata),
    error = function(error) {
      triomics_block_spls_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED",
        paste0("mixOmics native block.spls prediction failed: ", conditionMessage(error)),
        key = "prediction"
      )
    }
  )
  averaged <- native$AveragedPredict
  if (length(dim(averaged)) != 3L || dim(averaged)[[1L]] != length(units) ||
      dim(averaged)[[2L]] != 1L || dim(averaged)[[3L]] < state$ncomp) {
    triomics_block_spls_abort(
      "C_EVAL_PREDICTION_UNSUPPORTED", "Native block.spls prediction omitted the scalar response or requested component.",
      key = "prediction"
    )
  }
  predictions <- stats::setNames(as.numeric(averaged[, 1L, state$ncomp]), units)
  disposition <- triomics_block_spls_prediction_disposition(predictions)
  predictor_scores <- stats::setNames(lapply(block_ids, function(block_id) {
    triomics_block_spls_named_scores(native$variates[[block_id]], units, state$component_ids)
  }), block_ids)
  structure(
    list(
      unit_ids = unname(units),
      predictions = disposition$predictions,
      status = disposition$status,
      reason_code = disposition$reason_code,
      prediction_rule = "AveragedPredict",
      prediction_kind = "continuous_response",
      back_transform = "native_prediction_on_original_response_scale",
      native_method = backend_fit$metadata$native_method,
      native_version = backend_fit$metadata$native_version,
      training_state_hash = backend_fit$metadata$state_hash,
      predictor_scores = predictor_scores,
      consensus_scores = apply_consensus_state(predictor_scores, state$consensus_state),
      block_predictions = native$predict[block_ids]
    ),
    class = "PredictionArtifact"
  )
}

triomics_block_spls_extract <- function(backend_fit, context, params) {
  if (!inherits(backend_fit, "BackendFit")) {
    triomics_block_spls_abort("C_EVAL_NATIVE_FIT", "Extraction requires a BackendFit.", key = "backend_fit")
  }
  state <- backend_fit$native_state
  variance <- triomics_block_spls_variance(state$model, state$block_ids, state$ncomp)
  scores <- apply_consensus_state(state$training_scores, state$consensus_state)
  loadings <- stats::setNames(lapply(state$block_ids, function(block_id) {
    signed <- sweep(state$block_loadings[[block_id]], 2L,
                    state$consensus_state$signs[block_id, ], "*")
    signed[state$selected[[block_id]], , drop = FALSE]
  }), state$block_ids)
  result <- triomics_new_integration_result(
    scores = scores,
    loadings = loadings,
    variance = variance,
    selected = state$selected,
    backend = backend_fit$metadata$backend,
    params = backend_fit$result_params
  )
  list(
    result = triomics_validate_integration_result(result),
    sidecars = list(
      native_state = state,
      metadata = backend_fit$metadata,
      context = context,
      params = params
    )
  )
}

triomics_block_spls_backend <- function() {
  structure(
    list(
      evaluation_modes = "continuous",
      validate = function(resolved_analysis, params) {
        triomics_block_spls_validate(resolved_analysis, params)
      },
      fit = function(resolved_analysis, params) {
        triomics_block_spls_fit(resolved_analysis, params)
      },
      predict = function(backend_fit, new_resolved_analysis, params) {
        triomics_block_spls_predict(backend_fit, new_resolved_analysis, params)
      },
      extract = function(backend_fit, context, params) {
        triomics_block_spls_extract(backend_fit, context, params)
      }
    ),
    class = "BackendAdapter"
  )
}
