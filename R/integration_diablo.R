triomics_diablo_abort <- function(code, message, key = NULL,
                                   remediation = "Review the DIABLO input and its declared capability.") {
  triomics_abort(
    code, message, key = key, module = "integration", stage = "C",
    remediation = remediation
  )
}

triomics_diablo_ids <- function(value, label, code = "C_SCORE_RESULT_INVALID") {
  if (!is.character(value) || !length(value) || anyNA(value) ||
      any(!nzchar(value)) || anyDuplicated(value)) {
    triomics_diablo_abort(code, paste0(label, " must be unique, non-blank character ids."), key = label)
  }
  value
}

triomics_diablo_design <- function(value, block_ids) {
  if (is.list(value) && !is.matrix(value)) {
    row_ids <- names(value)
    if (is.null(row_ids)) {
      triomics_diablo_abort("C_SCORE_DESIGN_INVALID", "DIABLO design must have named rows.", key = "design_matrix")
    }
    row_ids <- triomics_diablo_ids(row_ids, "design_matrix rows", "C_SCORE_DESIGN_INVALID")
    column_ids <- unique(unlist(lapply(value, names), use.names = FALSE))
    if (!length(column_ids) || anyNA(column_ids) || any(!nzchar(column_ids)) || anyDuplicated(column_ids)) {
      triomics_diablo_abort("C_SCORE_DESIGN_INVALID", "DIABLO design must have named columns.", key = "design_matrix")
    }
    output <- matrix(NA_real_, nrow = length(row_ids), ncol = length(column_ids),
                     dimnames = list(row_ids, column_ids))
    for (row_id in row_ids) {
      row <- value[[row_id]]
      if (!is.list(row) || is.null(names(row))) {
        triomics_diablo_abort("C_SCORE_DESIGN_INVALID", "Each DIABLO design row must be a named numeric row.", key = row_id)
      }
      output[row_id, names(row)] <- unlist(row, use.names = FALSE)
    }
    value <- output
  }
  if (!is.matrix(value) || !is.numeric(value) || is.complex(value) ||
      nrow(value) != length(block_ids) || ncol(value) != length(block_ids) ||
      is.null(rownames(value)) || is.null(colnames(value)) ||
      !identical(rownames(value), block_ids) || !identical(colnames(value), block_ids) ||
      any(!is.finite(value)) || any(value < 0 | value > 1) ||
      any(diag(value) != 0) || !isTRUE(all.equal(value, t(value), check.attributes = FALSE))) {
    triomics_diablo_abort(
      "C_SCORE_DESIGN_INVALID",
      "DIABLO design must be a named symmetric predictor-block matrix in [0, 1] with zero diagonal.",
      key = "design_matrix"
    )
  }
  value
}

triomics_diablo_ncomp <- function(params) {
  values <- params[c("ncomp", "n_components")]
  values <- values[!vapply(values, is.null, logical(1L))]
  if (!length(values) || any(vapply(values, function(value) {
    !is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 1 || value != trunc(value)
  }, logical(1L))) || length(unique(as.integer(unlist(values)))) != 1L) {
    triomics_diablo_abort("C_SCORE_COMPONENT_RANK", "DIABLO components must be one positive integer.", key = "n_components")
  }
  as.integer(unlist(values)[[1L]])
}

triomics_diablo_keepx <- function(value, block_ids, ncomp, feature_counts) {
  if (!is.list(value) || is.null(names(value)) ||
      !identical(sort(names(value), method = "radix"), sort(block_ids, method = "radix"))) {
    triomics_diablo_abort("C_SCORE_PARAMETER", "DIABLO keepX must be named for every predictor block.", key = "keepX")
  }
  output <- stats::setNames(vector("list", length(block_ids)), block_ids)
  for (block_id in block_ids) {
    current <- value[[block_id]]
    if (!is.numeric(current) || length(current) != ncomp || anyNA(current) ||
        any(!is.finite(current)) || any(current < 1) || any(current != trunc(current)) ||
        any(current > feature_counts[[block_id]])) {
      triomics_diablo_abort(
        "C_SCORE_PARAMETER",
        paste0("DIABLO keepX.", block_id, " must contain one valid feature count per component."),
        key = paste0("keepX.", block_id)
      )
    }
    output[[block_id]] <- as.integer(current)
  }
  output
}

triomics_diablo_validate <- function(resolved_analysis, params) {
  if (!is.list(params) || !identical(as.character(params$method), "mixomics_diablo")) {
    triomics_diablo_abort("C_SCORE_BACKEND_UNAVAILABLE", "The DIABLO adapter requires method mixomics_diablo.", key = "method")
  }
  block_ids <- triomics_diablo_ids(params$block_ids, "block_ids", "C_ELIGIBILITY_BLOCK_MAP")
  if (length(block_ids) < 2L) {
    triomics_diablo_abort("C_ELIGIBILITY_BLOCK_COUNT", "DIABLO requires at least two predictor blocks.", key = "block_ids")
  }
  if (!is.list(resolved_analysis) || !is.list(resolved_analysis$blocks) ||
      !identical(names(resolved_analysis$blocks), block_ids)) {
    triomics_diablo_abort(
      "C_ELIGIBILITY_BLOCK_MAP",
      "DIABLO predictor blocks must match the declared stable block order.", key = "blocks"
    )
  }
  if (!identical(params$score_definition, "consensus_mean")) {
    triomics_diablo_abort(
      "C_SCORE_PARAMETER", "DIABLO requires score_definition=consensus_mean.", key = "score_definition"
    )
  }
  if (!is.logical(params$scale_features) || length(params$scale_features) != 1L ||
      is.na(params$scale_features)) {
    triomics_diablo_abort("C_SCORE_PARAMETER", "DIABLO feature scaling must be explicitly TRUE or FALSE.", key = "scale_features")
  }
  if (!is.character(params$tuning_grid_ref) || length(params$tuning_grid_ref) != 1L ||
      is.na(params$tuning_grid_ref) || !nzchar(params$tuning_grid_ref)) {
    triomics_diablo_abort("C_SCORE_PARAMETER", "DIABLO requires a named wrapper-controlled tuning grid.", key = "tuning_grid_ref")
  }
  if (!identical(params$na_capability, "complete_predictor_blocks")) {
    triomics_diablo_abort(
      "C_NA_CAPABILITY",
      "DIABLO requires the pinned complete_predictor_blocks NA capability declaration.",
      key = "na_capability",
      remediation = "Use a predecessor-produced complete predictor representation; DIABLO does not impute cell-level missing values."
    )
  }
  outcome <- resolved_analysis$outcome
  units <- triomics_diablo_ids(resolved_analysis$unit_ids, "unit_ids", "C_ELIGIBILITY_UNIT_MAP")
  if (!(is.factor(outcome) || is.character(outcome)) || length(outcome) != length(units) || anyNA(outcome)) {
    triomics_diablo_abort(
      "C_EVAL_RESPONSE_INVALID",
      "DIABLO requires one non-missing categorical outcome for every training unit.", key = "outcome"
    )
  }
  declared_levels <- params$class_levels
  if (is.null(declared_levels)) declared_levels <- if (is.factor(outcome)) levels(outcome) else sort(unique(as.character(outcome)))
  if (!is.character(declared_levels) || length(declared_levels) < 2L || anyNA(declared_levels) ||
      any(!nzchar(declared_levels)) || anyDuplicated(declared_levels) ||
      any(!as.character(outcome) %in% declared_levels) ||
      any(!declared_levels %in% unique(as.character(outcome)))) {
    triomics_diablo_abort(
      "C_EVAL_RESPONSE_INVALID", "Declared DIABLO outcome levels must exactly cover observed training classes.",
      key = "class_levels"
    )
  }
  outcome <- factor(as.character(outcome), levels = declared_levels)
  design <- triomics_diablo_design(params$design_matrix, block_ids)
  ncomp <- triomics_diablo_ncomp(params)
  assays <- stats::setNames(vector("list", length(block_ids)), block_ids)
  feature_counts <- stats::setNames(integer(length(block_ids)), block_ids)
  for (block_id in block_ids) {
    block <- resolved_analysis$blocks[[block_id]]
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        is.null(rownames(assay)) || is.null(colnames(assay)) ||
        !identical(as.character(block$feature_keys), rownames(assay)) ||
        !identical(as.character(block$sample_ids), colnames(assay)) ||
        !setequal(colnames(assay), units)) {
      triomics_diablo_abort(
        "C_ELIGIBILITY_SAMPLE_MAP",
        paste0("DIABLO block '", block_id, "' must have keyed feature-by-unit values matching the training map."),
        key = block_id
      )
    }
    if (any(!is.finite(assay)) || anyNA(assay)) {
      triomics_diablo_abort(
        "C_NA_CAPABILITY",
        paste0("DIABLO block '", block_id, "' contains unsupported cell-level missing or non-finite values."),
        key = block_id,
        remediation = "Provide a complete predictor block with an explicit predecessor missingness policy."
      )
    }
    ordered <- assay[, units, drop = FALSE]
    if (any(apply(ordered, 1L, stats::sd) <= .Machine$double.eps)) {
      triomics_diablo_abort(
        "C_SCORE_COMPONENT_RANK", paste0("DIABLO block '", block_id, "' contains a constant predictor feature."), key = block_id
      )
    }
    assays[[block_id]] <- t(ordered)
    rownames(assays[[block_id]]) <- units
    feature_counts[[block_id]] <- ncol(assays[[block_id]])
  }
  keepX <- triomics_diablo_keepx(params$keepX, block_ids, ncomp, feature_counts)
  if (!is.character(params$prediction_rule) || length(params$prediction_rule) != 1L ||
      is.na(params$prediction_rule) || !params$prediction_rule %in% c("max.dist", "centroids.dist", "mahalanobis.dist")) {
    triomics_diablo_abort(
      "C_SCORE_PARAMETER", "DIABLO requires a supported native distance prediction rule.", key = "prediction_rule"
    )
  }
  centered_ranks <- vapply(assays, function(value) {
    min(nrow(value) - 1L,
        qr(scale(value, center = TRUE, scale = FALSE), tol = 1e-10)$rank)
  }, integer(1L))
  available_rank <- min(centered_ranks)
  if (ncomp > available_rank) {
    triomics_diablo_abort(
      "C_SCORE_COMPONENT_RANK",
      paste0("DIABLO components exceed the minimum predictor-block training rank ", available_rank, "."),
      key = "n_components"
    )
  }
  list(
    block_ids = block_ids, units = units, assays = assays, outcome = factor(outcome, levels = declared_levels),
    class_levels = declared_levels, design = design, ncomp = ncomp, keepX = keepX,
    prediction_rule = params$prediction_rule, available_rank = as.integer(available_rank)
  )
}

triomics_diablo_native_matrix <- function(value, block_id, label, units = NULL, components = NULL) {
  if (!is.matrix(value) || !is.numeric(value) || is.complex(value) ||
      is.null(rownames(value)) || is.null(colnames(value)) || any(!is.finite(value))) {
    triomics_diablo_abort("C_SCORE_RESULT_INVALID", paste0("Native ", label, " for ", block_id, " is invalid."), key = block_id)
  }
  if (!is.null(units) && !setequal(rownames(value), units)) {
    triomics_diablo_abort("C_SCORE_RESULT_INVALID", paste0("Native ", label, " has the wrong training units."), key = block_id)
  }
  if (!is.null(components) && !setequal(colnames(value), components)) {
    triomics_diablo_abort("C_SCORE_RESULT_INVALID", paste0("Native ", label, " has the wrong component map."), key = block_id)
  }
  value
}

triomics_diablo_native_variance <- function(model, block_ids, components) {
  values <- model$prop_expl_var
  if (!is.list(model$X) || is.null(names(model$X)) || !is.list(model$variates) ||
      is.null(names(model$variates)) || !is.list(values) || is.null(names(values)) ||
      any(!block_ids %in% names(model$X)) || any(!block_ids %in% names(model$variates)) ||
      any(!block_ids %in% names(values))) {
    triomics_diablo_abort(
      "C_SCORE_VARIANCE_UNAVAILABLE",
      "Native DIABLO predictor-block variance inputs are absent or not keyed by predictor block.", key = "prop_expl_var"
    )
  }
  output <- matrix(NA_real_, nrow = length(block_ids), ncol = length(components),
                   dimnames = list(block_ids, components))
  for (block_id in block_ids) {
    current <- values[[block_id]]
    if (!is.numeric(current) || is.null(names(current)) || any(!components %in% names(current))) {
      triomics_diablo_abort(
        "C_SCORE_VARIANCE_UNAVAILABLE",
        paste0("Native DIABLO variance for predictor block '", block_id, "' is not component-keyed."), key = block_id
      )
    }
    native_values <- as.numeric(current[components])
    if (any(!is.finite(native_values)) || any(native_values < 0 | native_values > 1)) {
      triomics_diablo_abort(
        "C_SCORE_VARIANCE_UNAVAILABLE",
        paste0("Native DIABLO variance for predictor block '", block_id, "' is non-finite or outside [0, 1]."),
        key = block_id
      )
    }
    x <- model$X[[block_id]]
    scores <- model$variates[[block_id]]
    if (!is.matrix(x) || !is.numeric(x) || is.complex(x) ||
        !is.matrix(scores) || !is.numeric(scores) || is.complex(scores) ||
        is.null(colnames(scores)) || any(!components %in% colnames(scores)) ||
        nrow(x) != nrow(scores) || any(!is.finite(x)) || any(!is.finite(scores))) {
      triomics_diablo_abort(
        "C_SCORE_VARIANCE_UNAVAILABLE",
        paste0("Native DIABLO variance cannot be recomputed for predictor block '", block_id, "'."),
        key = block_id
      )
    }
    block_denominator <- sum(x^2)
    if (!is.finite(block_denominator) || block_denominator <= 0) {
      triomics_diablo_abort(
        "C_SCORE_VARIANCE_UNAVAILABLE",
        paste0("Predictor block '", block_id, "' has no positive fitted Frobenius denominator."), key = block_id
      )
    }
    recomputed <- vapply(components, function(component) {
      t <- scores[, component]
      score_denominator <- sum(t^2)
      if (!is.finite(score_denominator) || score_denominator <= 0) return(NA_real_)
      sum(crossprod(x, t)^2) / (score_denominator * block_denominator)
    }, numeric(1L))
    if (any(!is.finite(recomputed)) || any(recomputed < 0 | recomputed > 1) ||
        any(abs(recomputed - native_values) > 1e-8 * pmax(1, abs(recomputed), abs(native_values)))) {
      triomics_diablo_abort(
        "C_SCORE_VARIANCE_UNAVAILABLE",
        paste0("Native DIABLO variance disagrees with the verified predictor-block formula for '", block_id, "'."),
        key = block_id
      )
    }
    output[block_id, ] <- native_values
  }
  attr(output, "metric") <- "native_block_splsda_proportion"
  attr(output, "units") <- "proportion"
  attr(output, "semantics") <- "predictor_block_native"
  output
}

triomics_diablo_support <- function(native_loadings, block_ids, components) {
  loadings <- stats::setNames(vector("list", length(block_ids)), block_ids)
  selected <- stats::setNames(vector("list", length(block_ids)), block_ids)
  for (block_id in block_ids) {
    current <- native_loadings[[block_id]]
    support <- apply(abs(current[, components, drop = FALSE]) > 0, 1L, any)
    if (!any(support)) {
      triomics_diablo_abort(
        "C_SCORE_SPARSE_SUPPORT", paste0("Native DIABLO block '", block_id, "' has no sparse loading support."), key = block_id
      )
    }
    selected[[block_id]] <- rownames(current)[support]
    value <- current[support, components, drop = FALSE]
    rownames(value) <- selected[[block_id]]
    loadings[[block_id]] <- value
  }
  list(loadings = loadings, selected = selected)
}

triomics_diablo_oriented_loadings <- function(native_loadings, consensus_state, block_ids, components) {
  oriented <- stats::setNames(vector("list", length(block_ids)), block_ids)
  for (block_id in block_ids) {
    current <- native_loadings[[block_id]][, components, drop = FALSE]
    oriented[[block_id]] <- sweep(
      current, 2L, consensus_state$signs[block_id, components], "*"
    )
  }
  oriented
}

triomics_diablo_validate_state <- function(state) {
  if (!is.list(state) || !inherits(state$consensus_state, "ConsensusState") ||
      !is.list(state$native_scores) || !is.list(state$native_loadings)) {
    triomics_diablo_abort("C_SCORE_RESULT_INVALID", "DIABLO native state is incomplete.", key = "native_state")
  }
  block_ids <- state$block_ids
  components <- state$components
  for (block_id in block_ids) {
    scores <- triomics_diablo_native_matrix(state$native_scores[[block_id]], block_id, "scores",
                                            state$training_unit_ids, components)
    triomics_diablo_native_matrix(state$native_loadings[[block_id]], block_id, "loadings", NULL, components)
    if (any(vapply(seq_len(ncol(scores)), function(i) stats::sd(scores[, i]) <= state$consensus_state$sd_tolerance, logical(1)))) {
      triomics_diablo_abort("C_SCORE_RANK_ZERO", "A native DIABLO score is zero or near-zero on training units.", key = block_id)
    }
  }
  invisible(TRUE)
}

triomics_diablo_fit <- function(resolved_analysis, params) {
  eligibility <- triomics_diablo_validate(resolved_analysis, params)
  native_fit <- tryCatch(
    mixOmics::block.splsda(
      X = eligibility$assays, Y = eligibility$outcome, ncomp = eligibility$ncomp,
      keepX = eligibility$keepX, design = eligibility$design,
      scale = params$scale_features,
      near.zero.var = FALSE, all.outputs = TRUE
    ),
    error = function(error) {
      triomics_diablo_abort("C_EVAL_NATIVE_FIT", paste0("mixOmics native DIABLO fit failed: ", conditionMessage(error)), key = "n_components")
    }
  )
  components <- paste0("comp", seq_len(eligibility$ncomp))
  native_scores <- stats::setNames(vector("list", length(eligibility$block_ids)), eligibility$block_ids)
  native_loadings <- stats::setNames(vector("list", length(eligibility$block_ids)), eligibility$block_ids)
  for (block_id in eligibility$block_ids) {
    native_scores[[block_id]] <- triomics_diablo_native_matrix(
      native_fit$variates[[block_id]], block_id, "scores", eligibility$units, components
    )[eligibility$units, components, drop = FALSE]
    native_loadings[[block_id]] <- triomics_diablo_native_matrix(
      native_fit$loadings[[block_id]], block_id, "loadings", NULL, components
    )
  }
  consensus_state <- make_consensus_state(
    native_scores, native_loadings, eligibility$units,
    list(required_views = eligibility$block_ids, score_definition = "consensus_mean")
  )
  variance <- triomics_diablo_native_variance(native_fit, eligibility$block_ids, components)
  oriented_loadings <- triomics_diablo_oriented_loadings(
    native_loadings, consensus_state, eligibility$block_ids, components
  )
  support <- triomics_diablo_support(oriented_loadings, eligibility$block_ids, components)
  training_transforms <- lapply(eligibility$block_ids, function(block_id) {
    x <- native_fit$X[[block_id]]
    list(
      center = attr(x, "scaled:center"), scale = attr(x, "scaled:scale"),
      scale_features = isTRUE(params$scale_features), feature_keys = colnames(x)
    )
  })
  names(training_transforms) <- eligibility$block_ids
  state_hash <- digest::digest(
    list(
      method = "mixomics_diablo", native_version = as.character(utils::packageVersion("mixOmics")),
      training_unit_ids = eligibility$units, class_levels = eligibility$class_levels,
      block_ids = eligibility$block_ids, design = eligibility$design, keepX = eligibility$keepX,
      ncomp = eligibility$ncomp, training_transforms = training_transforms,
      native_scores = native_scores, native_loadings = native_loadings
    ), algo = "sha256", serialize = TRUE
  )
  native_design <- native_fit$design
  if (!is.matrix(native_design) || !identical(rownames(native_design), c(eligibility$block_ids, "Y")) ||
      !identical(colnames(native_design), c(eligibility$block_ids, "Y"))) {
    triomics_diablo_abort("C_SCORE_RESULT_INVALID", "Native DIABLO design did not preserve predictor and Y identities.", key = "design_matrix")
  }
  structure(
    list(
      native_state = list(
        model = native_fit, block_ids = eligibility$block_ids, training_unit_ids = eligibility$units,
        class_levels = eligibility$class_levels, components = components, native_scores = native_scores,
        native_loadings = native_loadings, oriented_loadings = oriented_loadings,
        consensus_state = consensus_state,
        training_transform = training_transforms, prediction_state = list(uses_outcome = FALSE),
        requested_ncomp = eligibility$ncomp, keepX = eligibility$keepX,
        prediction_rule = eligibility$prediction_rule
      ),
      selected = support$selected,
      result_params = list(
        native_method = "mixOmics::block.splsda",
        native_version = as.character(utils::packageVersion("mixOmics")),
        tuning = "wrapper_controlled_grid", tuning_state = "candidate_params_are_fit_once_per_evaluation_fold",
        requested_n_components = eligibility$ncomp, retained_n_components = length(components),
        available_training_rank = eligibility$available_rank,
        keepX = eligibility$keepX, score_definition = "consensus_mean",
        loading_orientation = "frozen_consensus_state_signs",
        design = eligibility$design, native_design = native_design,
        native_design_expansion = "mixOmics_added_Y_with_unit_link_for_categorical_response",
        na_capability = "complete_predictor_blocks",
        prediction_rule = eligibility$prediction_rule,
        variance = list(metric = "native_block_splsda_proportion", units = "proportion",
                        semantics = "predictor_block_native", source = "native_fit$prop_expl_var",
                        assay_space = "mixomics_fitted_block_after_declared_scaling",
                        denominator = "squared_frobenius_norm_of_fitted_block",
                        accumulation = "per_component_against_original_block_not_cumulative"),
        training_unit_ids = eligibility$units, training_state_hash = state_hash,
        consensus_state_version = consensus_state$state_version,
        sparse_selection = "union_of_nonzero_native_predictor_loadings"
      ),
      metadata = list(
        backend = "mixomics_diablo", native_method = "mixOmics::block.splsda",
        native_version = as.character(utils::packageVersion("mixOmics")),
        model_role = "supervised_prediction", training_unit_ids = eligibility$units,
        preparation_context_id = resolved_analysis$preparation_context_id, state_hash = state_hash
      )
    ),
    class = "BackendFit"
  )
}

triomics_diablo_predict <- function(backend_fit, new_resolved_analysis, params) {
  if (!inherits(backend_fit, "BackendFit") || !is.list(backend_fit$native_state)) {
    triomics_diablo_abort("C_EVAL_PREDICTION_UNSUPPORTED", "Prediction requires a fitted DIABLO BackendFit.", key = "backend_fit")
  }
  state <- backend_fit$native_state
  triomics_diablo_validate_state(state)
  if (!is.list(new_resolved_analysis) || !is.list(new_resolved_analysis$blocks) ||
      !identical(names(new_resolved_analysis$blocks), state$block_ids)) {
    triomics_diablo_abort("C_EVAL_REQUIRED_VIEW_MISSING", "DIABLO prediction requires every fitted predictor block in fitted order.", key = "blocks")
  }
  units <- triomics_diablo_ids(new_resolved_analysis$unit_ids, "unit_ids", "C_EVAL_PREDICTION_UNSUPPORTED")
  newdata <- stats::setNames(vector("list", length(state$block_ids)), state$block_ids)
  for (block_id in state$block_ids) {
    block <- new_resolved_analysis$blocks[[block_id]]
    assay <- block$assay
    expected_features <- colnames(state$model$X[[block_id]])
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        is.null(rownames(assay)) || is.null(colnames(assay)) ||
        !identical(as.character(block$feature_keys), rownames(assay)) ||
        !identical(as.character(block$sample_ids), colnames(assay)) || !setequal(colnames(assay), units) ||
        !identical(rownames(assay), expected_features)) {
      triomics_diablo_abort("C_EVAL_PREDICTION_UNSUPPORTED", paste0("Prediction input for block '", block_id, "' differs from the fitted feature map."), key = block_id)
    }
    if (any(!is.finite(assay)) || anyNA(assay)) {
      triomics_diablo_abort("C_NA_CAPABILITY", paste0("Prediction block '", block_id, "' contains unsupported missing or non-finite values."), key = block_id)
    }
    newdata[[block_id]] <- t(assay[, units, drop = FALSE])
    rownames(newdata[[block_id]]) <- units
  }
  native <- tryCatch(
    stats::predict(state$model, newdata = newdata, dist = state$prediction_rule),
    error = function(error) {
      triomics_diablo_abort("C_EVAL_PREDICTION_UNSUPPORTED", paste0("mixOmics native DIABLO prediction failed: ", conditionMessage(error)), key = "prediction")
    }
  )
  native_scores <- stats::setNames(vector("list", length(state$block_ids)), state$block_ids)
  for (block_id in state$block_ids) {
    value <- triomics_diablo_native_matrix(
      native$variates[[block_id]], block_id, "held-out scores", units, NULL
    )
    if (ncol(value) < state$requested_ncomp) {
      triomics_diablo_abort(
        "C_EVAL_PREDICTION_UNSUPPORTED", "Native DIABLO prediction omitted the requested score component.", key = "prediction"
      )
    }
    value <- value[units, seq_len(state$requested_ncomp), drop = FALSE]
    colnames(value) <- state$components
    native_scores[[block_id]] <- value
  }
  common_scores <- apply_consensus_state(native_scores, state$consensus_state)
  classes <- native$MajorityVote[[state$prediction_rule]]
  if (!is.matrix(classes) || nrow(classes) != length(units) || ncol(classes) < state$requested_ncomp) {
    triomics_diablo_abort("C_EVAL_PREDICTION_UNSUPPORTED", "Native DIABLO prediction omitted the requested component.", key = "prediction")
  }
  predictions <- as.character(classes[, state$requested_ncomp])
  names(predictions) <- units
  invalid <- is.na(predictions) | !predictions %in% state$class_levels
  status <- stats::setNames(rep("predicted", length(units)), units)
  reason <- stats::setNames(rep(NA_character_, length(units)), units)
  status[invalid] <- "failed"
  reason[invalid] <- "invalid_native_majority_vote"
  predictions[invalid] <- NA_character_
  structure(
    list(
      unit_ids = units, predictions = predictions, status = status, reason_code = reason,
      scores = common_scores, native_scores = native_scores,
      prediction_rule = state$prediction_rule, prediction_kind = "categorical_label",
      prediction_aggregation = "native_MajorityVote", native_method = backend_fit$metadata$native_method,
      native_version = backend_fit$metadata$native_version, training_state_hash = backend_fit$metadata$state_hash,
      uses_held_out_outcome = FALSE
    ),
    class = "PredictionArtifact"
  )
}

triomics_diablo_extract <- function(backend_fit, context, params) {
  if (!inherits(backend_fit, "BackendFit")) {
    triomics_diablo_abort("C_EVAL_NATIVE_FIT", "Extraction requires a DIABLO BackendFit.", key = "backend_fit")
  }
  state <- backend_fit$native_state
  triomics_diablo_validate_state(state)
  components <- state$components
  variance <- triomics_diablo_native_variance(state$model, state$block_ids, components)
  oriented_loadings <- triomics_diablo_oriented_loadings(
    state$native_loadings, state$consensus_state, state$block_ids, components
  )
  support <- triomics_diablo_support(oriented_loadings, state$block_ids, components)
  scores <- apply_consensus_state(state$native_scores, state$consensus_state)
  if (!is.matrix(scores) || !identical(colnames(scores), components) || any(!is.finite(scores))) {
    triomics_diablo_abort("C_SCORE_RESULT_INVALID", "DIABLO common scores are missing or non-finite.", key = "scores")
  }
  result <- triomics_new_integration_result(
    scores = scores, loadings = support$loadings, variance = variance,
    selected = support$selected, backend = "mixomics_diablo", params = backend_fit$result_params
  )
  list(
    result = triomics_validate_integration_result(result),
    sidecars = list(native_state = state, metadata = backend_fit$metadata, context = context, params = params)
  )
}

triomics_diablo_backend <- function(method = "mixomics_diablo") {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !identical(method, "mixomics_diablo")) {
    triomics_diablo_abort("C_SCORE_BACKEND_UNAVAILABLE", "One DIABLO method id, mixomics_diablo, is required.", key = "method")
  }
  structure(
    list(
      evaluation_modes = "categorical",
      validate = function(resolved_analysis, params) {
        params$method <- method
        triomics_diablo_validate(resolved_analysis, params)
      },
      fit = function(resolved_analysis, params) {
        params$method <- method
        triomics_diablo_fit(resolved_analysis, params)
      },
      predict = function(backend_fit, new_resolved_analysis, params) {
        params$method <- method
        triomics_diablo_predict(backend_fit, new_resolved_analysis, params)
      },
      extract = function(backend_fit, context, params) {
        params$method <- method
        triomics_diablo_extract(backend_fit, context, params)
      }
    ),
    class = "BackendAdapter"
  )
}
