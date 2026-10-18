triomics_supervised_abort <- function(code, message, key = NULL,
                                      remediation = "Review the supervised evaluation inputs and frozen contract.") {
  triomics_abort(
    code, message, key = key, module = "evaluation", stage = "C",
    remediation = remediation
  )
}

triomics_supervised_method_mode <- function(method) {
  switch(
    method,
    concat_plsda = "categorical",
    concat_pls = "continuous",
    triomics_supervised_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "The supervised concatenation method is not registered in the C-02 adapter.",
      key = "integration.method",
      remediation = "Request C-02 adapter registration before selecting this method."
    )
  )
}

triomics_supervised_validate <- function(resolved_analysis, params) {
  if (!is.list(params) || is.null(params$method)) {
    triomics_supervised_abort("C_EVAL_RESPONSE_INVALID", "A supervised method is required.", key = "method")
  }
  mode <- triomics_supervised_method_mode(as.character(params$method[[1L]]))
  block_ids <- params$block_ids
  if (is.list(block_ids)) block_ids <- unlist(block_ids, use.names = FALSE)
  if (!is.character(block_ids) || !length(block_ids) || anyNA(block_ids) ||
      any(!nzchar(block_ids)) || anyDuplicated(block_ids)) {
    triomics_supervised_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING", "Required predictor block ids must be unique and explicit.",
      key = "block_ids"
    )
  }
  if (!is.list(resolved_analysis) || !is.list(resolved_analysis$blocks) ||
      !identical(names(resolved_analysis$blocks), block_ids)) {
    triomics_supervised_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING",
      "Every required predictor block must be supplied in the declared stable order.",
      key = "blocks"
    )
  }
  units <- resolved_analysis$unit_ids
  if (!is.character(units) || !length(units) || anyNA(units) ||
      any(!nzchar(units)) || anyDuplicated(units)) {
    triomics_supervised_abort("C_EVAL_RESPONSE_INVALID", "Training unit ids must be unique and non-blank.", key = "unit_ids")
  }
  if (!is.character(resolved_analysis$study_id) || length(resolved_analysis$study_id) != 1L ||
      !nzchar(resolved_analysis$study_id) ||
      !is.character(resolved_analysis$preparation_context_id) ||
      length(resolved_analysis$preparation_context_id) != 1L ||
      !nzchar(resolved_analysis$preparation_context_id)) {
    triomics_supervised_abort(
      "C_EVAL_RESPONSE_INVALID", "Study and preparation context identity are required.",
      key = "preparation_context_id"
    )
  }
  if (!is.logical(params$scale_features) || length(params$scale_features) != 1L ||
      is.na(params$scale_features)) {
    triomics_supervised_abort(
      "C_EVAL_RESPONSE_INVALID", "Feature scaling must be an explicit logical setting.",
      key = "scale_features"
    )
  }
  ncomp <- params$ncomp
  if (!is.numeric(ncomp) || length(ncomp) != 1L || is.na(ncomp) ||
      !is.finite(ncomp) || ncomp < 1 || ncomp != trunc(ncomp)) {
    triomics_supervised_abort("C_EVAL_COMPONENT_RANK", "Requested component count must be a positive integer.", key = "ncomp")
  }

  if (mode == "categorical") {
    response <- resolved_analysis$outcome
    if (!(is.factor(response) || is.character(response)) || length(response) != length(units) ||
        anyNA(response)) {
      triomics_supervised_abort(
        "C_EVAL_RESPONSE_INVALID", "PLS-DA requires one non-missing categorical response per training unit.",
        key = "outcome"
      )
    }
    declared <- params$class_levels
    if (is.null(declared)) declared <- if (is.factor(response)) levels(response) else sort(unique(response))
    if (!is.character(declared) || length(declared) < 2L || anyNA(declared) ||
        any(!nzchar(declared)) || anyDuplicated(declared) || any(!as.character(response) %in% declared)) {
      triomics_supervised_abort(
        "C_EVAL_RESPONSE_INVALID", "Declared categorical levels must contain the observed training classes.",
        key = "class_levels"
      )
    }
    observed <- unique(as.character(response))
    if (length(observed) < 2L || any(!declared %in% observed)) {
      triomics_supervised_abort(
        "C_EVAL_CLASS_SUPPORT", "Every declared class must have training support in this split.",
        key = "outcome"
      )
    }
    response <- factor(as.character(response), levels = declared)
  } else {
    response <- resolved_analysis$outcome
    if (!is.numeric(response) || is.matrix(response) || length(response) != length(units) ||
        anyNA(response) || any(!is.finite(response))) {
      triomics_supervised_abort(
        "C_EVAL_RESPONSE_INVALID", "PLS regression requires one finite scalar numeric response per training unit.",
        key = "outcome"
      )
    }
    declared <- NULL
    response <- as.numeric(response)
  }

  block_rows <- vector("list", length(block_ids))
  names(block_rows) <- block_ids
  for (block_id in block_ids) {
    block <- resolved_analysis$blocks[[block_id]]
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        length(dim(assay)) != 2L || nrow(assay) < 1L || ncol(assay) < 1L ||
        is.null(rownames(assay)) || is.null(colnames(assay)) ||
        anyNA(rownames(assay)) || anyNA(colnames(assay)) ||
        anyDuplicated(rownames(assay)) || anyDuplicated(colnames(assay))) {
      triomics_supervised_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING", paste0("Block '", block_id, "' needs a keyed numeric feature-by-unit matrix."),
        key = block_id
      )
    }
    if (!identical(as.character(block$feature_keys), rownames(assay)) ||
        !identical(as.character(block$sample_ids), colnames(assay)) ||
        !setequal(colnames(assay), units)) {
      triomics_supervised_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING",
        paste0("Block '", block_id, "' feature or unit identity does not match the training map."),
        key = block_id
      )
    }
    assay <- assay[, units, drop = FALSE]
    if (any(!is.finite(assay))) {
      triomics_supervised_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING",
        paste0("Block '", block_id, "' has a non-finite prepared training predictor."),
        key = block_id
      )
    }
    block_rows[[block_id]] <- assay
  }
  input_feature_keys <- stats::setNames(lapply(block_ids, function(block_id) {
    rownames(block_rows[[block_id]])
  }), block_ids)
  x <- do.call(cbind, lapply(block_ids, function(block_id) t(block_rows[[block_id]])))
  feature_map <- do.call(rbind, lapply(seq_along(block_ids), function(i) {
    block_id <- block_ids[[i]]
    features <- rownames(block_rows[[block_id]])
    data.frame(
      block_id = block_id,
      feature_key = features,
      model_column = paste0("triomics_feature_", sprintf("%05d", seq_len(length(features)) +
        sum(vapply(block_rows[seq_len(i - 1L)], nrow, integer(1))))),
      input_order = seq_along(features),
      stringsAsFactors = FALSE
    )
  }))
  colnames(x) <- feature_map$model_column
  rownames(x) <- units
  feature_sd <- apply(x, 2L, stats::sd)
  keep <- is.finite(feature_sd) & feature_sd > .Machine$double.eps
  if (!any(keep)) {
    triomics_supervised_abort(
      "C_EVAL_COMPONENT_RANK", "No non-constant prepared predictor features remain in this training split.",
      key = "features"
    )
  }
  feature_map$selected <- keep
  x <- x[, keep, drop = FALSE]
  feature_map <- feature_map[keep, , drop = FALSE]
  feature_map <- feature_map[order(match(feature_map$block_id, block_ids), feature_map$input_order), , drop = FALSE]
  if (any(!block_ids %in% feature_map$block_id)) {
    triomics_supervised_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING", "A required block has no non-constant training features.",
      key = "block_ids"
    )
  }
  centered <- scale(x, center = TRUE, scale = FALSE)
  available_rank <- min(nrow(x) - 1L, qr(centered, tol = 1e-10)$rank)
  if (as.integer(ncomp) > available_rank) {
    triomics_supervised_abort(
      "C_EVAL_COMPONENT_RANK",
      paste0("Requested ncomp=", as.integer(ncomp), " exceeds training X rank ", available_rank,
             "; components are not silently reduced."),
      key = "ncomp"
    )
  }
  names(response) <- units
  list(
    mode = mode, method = as.character(params$method[[1L]]), block_ids = block_ids,
    unit_ids = units, outcome = response, class_levels = declared,
    x = x, feature_map = feature_map, available_rank = as.integer(available_rank),
    input_feature_keys = input_feature_keys,
    ncomp = as.integer(ncomp)
  )
}

triomics_supervised_variance <- function(training_x, scores, feature_map, block_ids) {
  n <- nrow(scores)
  requested <- ncol(scores)
  disposition <- lapply(seq_len(requested), function(k) {
    list(component = colnames(scores)[[k]], status = "unavailable_rank_degenerate",
         reason = "native_training_score_did_not_add_rank")
  })
  retained <- integer()
  previous_rank <- 0L
  for (k in seq_len(requested)) {
    current_rank <- qr(scores[, seq_len(k), drop = FALSE], tol = 1e-10)$rank
    if (current_rank <= previous_rank) break
    retained <- c(retained, k)
    disposition[[k]] <- list(component = colnames(scores)[[k]], status = "retained", reason = NULL)
    previous_rank <- current_rank
  }
  if (!length(retained)) {
    triomics_supervised_abort(
      "C_EVAL_COMPONENT_RANK", "Native training X variates have zero usable rank.", key = "ncomp"
    )
  }
  scores <- scores[, retained, drop = FALSE]
  q_previous <- matrix(numeric(), nrow = n, ncol = 0L)
  fractions <- matrix(
    NA_real_, nrow = length(block_ids), ncol = length(retained),
    dimnames = list(block_ids, colnames(scores))
  )
  for (j in seq_along(retained)) {
    q_current <- qr.Q(qr(scores[, seq_len(j), drop = FALSE], tol = 1e-10))[, seq_len(j), drop = FALSE]
    for (block_id in block_ids) {
      columns <- feature_map$model_column[feature_map$block_id == block_id]
      block_x <- training_x[, columns, drop = FALSE]
      denominator <- sum(block_x^2)
      if (!is.finite(denominator) || denominator <= 0) {
        triomics_supervised_abort(
          "C_EVAL_REQUIRED_VIEW_MISSING",
          paste0("Block '", block_id, "' has no positive training-score variance denominator."),
          key = block_id
        )
      }
      before <- if (!ncol(q_previous)) block_x else {
        block_x - q_previous %*% crossprod(q_previous, block_x)
      }
      after <- block_x - q_current %*% crossprod(q_current, block_x)
      sse_before <- sum(before^2)
      sse_after <- sum(after^2)
      reduction <- sse_before - sse_after
      tolerance <- 1e-10 * max(1, denominator)
      if (reduction < -tolerance) {
        triomics_supervised_abort(
          "C_EVAL_VARIANCE_INVALID", "Nested training-score projection increased block SSE.",
          key = block_id
        )
      }
      fractions[block_id, j] <- min(1, max(0, reduction) / denominator)
    }
    q_previous <- q_current
  }
  attr(fractions, "metric") <- "nested_training_score_subspace"
  attr(fractions, "units") <- "proportion"
  list(
    scores = scores,
    variance = fractions,
    retained = retained,
    disposition = disposition
  )
}

triomics_supervised_fit <- function(resolved_analysis, params) {
  eligibility <- triomics_supervised_validate(resolved_analysis, params)
  x <- eligibility$x
  y <- eligibility$outcome
  native_fit <- tryCatch({
    if (identical(eligibility$mode, "categorical")) {
      mixOmics::plsda(
        X = x, Y = y, ncomp = eligibility$ncomp,
        scale = params$scale_features, near.zero.var = FALSE, all.outputs = TRUE
      )
    } else {
      mixOmics::pls(
        X = x, Y = matrix(y, ncol = 1L, dimnames = list(eligibility$unit_ids, "outcome")),
        ncomp = eligibility$ncomp, scale = params$scale_features,
        mode = "regression", near.zero.var = FALSE, all.outputs = TRUE
      )
    }
  }, error = function(error) {
    triomics_supervised_abort(
      "C_EVAL_NATIVE_FIT", paste0("mixOmics native fit failed: ", conditionMessage(error)),
      key = "ncomp"
    )
  })
  native_method <- if (identical(eligibility$mode, "categorical")) "mixOmics::plsda" else "mixOmics::pls"
  native_version <- as.character(utils::packageVersion("mixOmics"))
  scores <- native_fit$variates$X
  if (!is.matrix(scores) || nrow(scores) != length(eligibility$unit_ids) ||
      !ncol(scores) || any(!is.finite(scores))) {
    triomics_supervised_abort(
      "C_EVAL_COMPONENT_RANK", "Native model did not return finite training X variates.",
      key = "ncomp"
    )
  }
  if (is.null(rownames(scores))) rownames(scores) <- eligibility$unit_ids
  if (is.null(colnames(scores))) colnames(scores) <- paste0("comp", seq_len(ncol(scores)))
  if (!identical(rownames(scores), eligibility$unit_ids)) {
    scores <- scores[eligibility$unit_ids, , drop = FALSE]
  }
  native_loadings <- native_fit$loadings$X
  if (!is.matrix(native_loadings) || any(!is.finite(native_loadings))) {
    triomics_supervised_abort(
      "C_EVAL_NATIVE_FIT", "Native model did not return finite X loadings.", key = "loadings"
    )
  }
  if (is.null(rownames(native_loadings))) rownames(native_loadings) <- colnames(x)
  if (is.null(colnames(native_loadings))) colnames(native_loadings) <- colnames(scores)[seq_len(ncol(native_loadings))]
  native_loadings <- native_loadings[colnames(x), colnames(scores), drop = FALSE]
  variance <- triomics_supervised_variance(native_fit$X, scores, eligibility$feature_map,
                                           eligibility$block_ids)
  scores <- variance$scores
  retained_components <- colnames(scores)
  selected <- stats::setNames(lapply(eligibility$block_ids, function(block_id) {
    eligibility$feature_map$feature_key[eligibility$feature_map$block_id == block_id]
  }), eligibility$block_ids)
  loadings <- stats::setNames(lapply(eligibility$block_ids, function(block_id) {
    map <- eligibility$feature_map[eligibility$feature_map$block_id == block_id, , drop = FALSE]
    value <- native_loadings[map$model_column, retained_components, drop = FALSE]
    rownames(value) <- map$feature_key
    value
  }), eligibility$block_ids)
  model_x_center <- attr(native_fit$X, "scaled:center")
  model_x_scale <- attr(native_fit$X, "scaled:scale")
  if (is.null(model_x_center)) model_x_center <- stats::setNames(rep(0, ncol(x)), colnames(x))
  if (is.null(model_x_scale)) model_x_scale <- stats::setNames(rep(1, ncol(x)), colnames(x))
  response_state <- if (identical(eligibility$mode, "categorical")) {
    list(type = "categorical", class_levels = eligibility$class_levels,
         training_class_counts = as.list(table(factor(y, levels = eligibility$class_levels))))
  } else {
    list(
      type = "continuous",
      center = attr(native_fit$Y, "scaled:center"),
      scale = attr(native_fit$Y, "scaled:scale"),
      training_intercept = mean(y),
      prediction_scale = "original_response_scale"
    )
  }
  training_transform <- list(
    center = model_x_center, scale = model_x_scale,
    scale_features = isTRUE(params$scale_features),
    training_unit_ids = eligibility$unit_ids
  )
  state_hash <- digest::digest(
    list(
      method = eligibility$method, native_version = native_version,
      training_unit_ids = eligibility$unit_ids,
      outcome = if (is.factor(y)) list(values = as.character(y), levels = levels(y)) else unname(y),
      feature_map = eligibility$feature_map,
      preprocessing_state_hashes = resolved_analysis$preparation_state_hashes,
      training_transform = training_transform,
      response_state = response_state,
      scores = scores, loadings = native_loadings[, retained_components, drop = FALSE],
      ncomp = eligibility$ncomp
    ),
    algo = "sha256", serialize = TRUE
  )
  names(training_transform$center) <- names(training_transform$scale) <- colnames(x)
  feature_flow <- lapply(eligibility$block_ids, function(block_id) {
    map <- eligibility$feature_map[eligibility$feature_map$block_id == block_id, , drop = FALSE]
    list(
      input = unname(map$feature_key), selected = unname(map$feature_key),
      excluded_zero_variance = setdiff(
        rownames(resolved_analysis$blocks[[block_id]]$assay), map$feature_key
      ), rule = "fold_prepared_nonconstant_features"
    )
  })
  names(feature_flow) <- eligibility$block_ids
  metadata <- list(
    backend = eligibility$method, native_method = native_method,
    native_version = native_version, model_role = "supervised_prediction",
    training_unit_ids = eligibility$unit_ids,
    preparation_context_id = resolved_analysis$preparation_context_id,
    state_hash = state_hash
  )
  response_extraction <- if (identical(eligibility$mode, "categorical")) {
    list(
      output = "mixOmics_native_class", prediction_rule = params$prediction_rule,
      class_levels = eligibility$class_levels,
      probabilities = FALSE, auroc = "not_reported_without_oriented_continuous_decision_score"
    )
  } else {
    list(
      output = "predict$predict[, 1, ncomp]",
      back_transform = "native_prediction_on_original_response_scale",
      training_intercept = mean(y)
    )
  }
  result_params <- list(
    native_method = native_method,
    versions = list(mixOmics = native_version, R = paste(R.version$major, R.version$minor, sep = ".")),
    training_unit_ids = eligibility$unit_ids,
    training_state_hash = state_hash,
    training_state = list(
      centering = training_transform$center,
      scaling = training_transform$scale,
      selection = feature_flow,
      response = response_state,
      preprocessing_state_hashes = resolved_analysis$preparation_state_hashes
    ),
    feature_flow = feature_flow,
    requested_n_components = eligibility$ncomp,
    retained_n_components = length(retained_components),
    component_disposition = variance$disposition,
    loading_role = "native_X_loadings_split_by_original_predictor_block",
    prediction_rule = if (identical(eligibility$mode, "categorical")) params$prediction_rule else "native_response_extraction",
    response_extraction = response_extraction,
    variance = list(
      metric = "nested_training_score_subspace", units = "proportion",
      denominator = "training_block_frobenius_sum_of_squares", semantics = "incremental"
    ),
    diagnostics = list(native_prediction_scale = response_state$type)
  )
  structure(
    list(
      native_state = list(
        model = native_fit, feature_map = eligibility$feature_map,
        input_feature_keys = eligibility$input_feature_keys,
        training_matrix = x, training_response = y,
        training_transform = training_transform,
        response_state = response_state,
        training_scores = scores,
        block_loadings = loadings,
        block_projection_fraction = variance$variance,
        block_ids = eligibility$block_ids,
        class_levels = eligibility$class_levels,
        prediction_rule = params$prediction_rule,
        requested_ncomp = eligibility$ncomp,
        component_disposition = variance$disposition,
        preparation_state_hashes = resolved_analysis$preparation_state_hashes,
        preparation_state_refs = resolved_analysis$preparation_state_refs
      ),
      selected = selected,
      result_params = result_params,
      metadata = metadata
    ),
    class = "BackendFit"
  )
}

triomics_supervised_predict <- function(backend_fit, new_resolved_analysis, params) {
  if (!inherits(backend_fit, "BackendFit") || is.null(backend_fit$native_state$model)) {
    triomics_supervised_abort("C_EVAL_PREDICTION_UNSUPPORTED", "Prediction requires a fitted BackendFit.", key = "backend_fit")
  }
  state <- backend_fit$native_state
  block_ids <- state$block_ids
  if (!is.list(new_resolved_analysis) || !is.list(new_resolved_analysis$blocks) ||
      !identical(names(new_resolved_analysis$blocks), block_ids)) {
    triomics_supervised_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING", "Prediction requires every fitted predictor block in fitted order.",
      key = "blocks"
    )
  }
  units <- new_resolved_analysis$unit_ids
  if (!is.character(units) || !length(units) || anyNA(units) || anyDuplicated(units)) {
    triomics_supervised_abort("C_EVAL_PREDICTION_UNSUPPORTED", "Prediction unit ids must be unique.", key = "unit_ids")
  }
  feature_map <- state$feature_map
  new_x <- matrix(NA_real_, nrow = length(units), ncol = nrow(feature_map),
                  dimnames = list(units, feature_map$model_column))
  for (block_id in block_ids) {
    block <- new_resolved_analysis$blocks[[block_id]]
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        !identical(as.character(block$feature_keys), rownames(assay)) ||
        !identical(as.character(block$sample_ids), colnames(assay)) ||
        !setequal(colnames(assay), units)) {
      triomics_supervised_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED",
        paste0("Prediction input for block '", block_id, "' does not match the frozen unit/feature map."),
        key = block_id
      )
    }
    block_map <- feature_map[feature_map$block_id == block_id, , drop = FALSE]
    expected_input <- state$input_feature_keys[[block_id]]
    if (!identical(rownames(assay), expected_input)) {
      triomics_supervised_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED",
        paste0("Prediction feature order for block '", block_id, "' differs from its training state."),
        key = block_id
      )
    }
    values <- t(assay[block_map$feature_key, units, drop = FALSE])
    if (any(!is.finite(values))) {
      triomics_supervised_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING",
        paste0("Prediction input for block '", block_id, "' contains a non-finite predictor."),
        key = block_id
      )
    }
    new_x[, block_map$model_column] <- values
  }
  model <- state$model
  if (identical(backend_fit$metadata$native_method, "mixOmics::plsda")) {
    rule <- state$prediction_rule
    if (!is.character(rule) || length(rule) != 1L ||
        !rule %in% c("max.dist", "centroids.dist", "mahalanobis.dist")) {
      triomics_supervised_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED", "PLS-DA requires a declared native distance prediction rule.",
        key = "prediction_rule"
      )
    }
    native <- tryCatch(
      stats::predict(model, newdata = new_x, dist = rule),
      error = function(error) {
        triomics_supervised_abort(
          "C_EVAL_PREDICTION_UNSUPPORTED",
          paste0("mixOmics native PLS-DA prediction failed: ", conditionMessage(error)),
          key = "prediction"
        )
      }
    )
    class_values <- native$class[[rule]]
    if (!is.matrix(class_values) || nrow(class_values) != length(units) ||
        ncol(class_values) < state$requested_ncomp) {
      triomics_supervised_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED", "Native PLS-DA prediction omitted the requested component.",
        key = "prediction"
      )
    }
    predictions <- as.character(class_values[, state$requested_ncomp])
    names(predictions) <- units
    structure(
      list(
        unit_ids = unname(units), predictions = predictions,
        prediction_rule = rule, prediction_kind = "categorical_label",
        native_method = backend_fit$metadata$native_method,
        native_version = backend_fit$metadata$native_version,
        training_state_hash = backend_fit$metadata$state_hash
      ),
      class = "PredictionArtifact"
    )
  } else {
    native <- tryCatch(
      stats::predict(model, newdata = new_x),
      error = function(error) {
        triomics_supervised_abort(
          "C_EVAL_PREDICTION_UNSUPPORTED",
          paste0("mixOmics native PLS prediction failed: ", conditionMessage(error)),
          key = "prediction"
        )
      }
    )
    values <- native$predict
    if (length(dim(values)) != 3L || dim(values)[[1L]] != length(units) ||
        dim(values)[[2L]] != 1L || dim(values)[[3L]] < state$requested_ncomp) {
      triomics_supervised_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED", "Native PLS prediction omitted the scalar response or requested component.",
        key = "prediction"
      )
    }
    predictions <- as.numeric(values[, 1L, state$requested_ncomp])
    names(predictions) <- units
    structure(
      list(
        unit_ids = unname(units), predictions = predictions,
        prediction_rule = "native_response_extraction",
        prediction_kind = "continuous_response",
        native_method = backend_fit$metadata$native_method,
        native_version = backend_fit$metadata$native_version,
        training_state_hash = backend_fit$metadata$state_hash,
        back_transform = "native_prediction_on_original_response_scale"
      ),
      class = "PredictionArtifact"
    )
  }
}

triomics_supervised_extract <- function(backend_fit, context, params) {
  if (!inherits(backend_fit, "BackendFit")) {
    triomics_supervised_abort("C_EVAL_NATIVE_FIT", "Extraction requires a BackendFit.", key = "backend_fit")
  }
  result <- triomics_new_integration_result(
    scores = backend_fit$native_state$training_scores,
    loadings = backend_fit$native_state$block_loadings,
    variance = backend_fit$native_state$block_projection_fraction,
    selected = backend_fit$selected,
    backend = backend_fit$metadata$backend,
    params = backend_fit$result_params
  )
  list(
    result = triomics_validate_integration_result(result),
    sidecars = list(
      native_state = backend_fit$native_state,
      metadata = backend_fit$metadata,
      context = context,
      params = params
    )
  )
}

triomics_supervised_backend <- function(method) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    triomics_supervised_abort("C_SCORE_BACKEND_UNAVAILABLE", "One supervised method id is required.", key = "method")
  }
  triomics_supervised_method_mode(method)
  structure(
    list(
      validate = function(resolved_analysis, params) {
        params$method <- method
        triomics_supervised_validate(resolved_analysis, params)
      },
      fit = function(resolved_analysis, params) {
        params$method <- method
        triomics_supervised_fit(resolved_analysis, params)
      },
      predict = function(backend_fit, new_resolved_analysis, params) {
        params$method <- method
        triomics_supervised_predict(backend_fit, new_resolved_analysis, params)
      },
      extract = function(backend_fit, context, params) {
        params$method <- method
        triomics_supervised_extract(backend_fit, context, params)
      }
    ),
    class = "BackendAdapter"
  )
}
