triomics_c01_component_ids <- function(n) {
  paste0("PC", seq_len(n))
}

triomics_c01_validate_pca_spec <- function(integration_spec) {
  n_components <- integration_spec$n_components
  if (!is.numeric(n_components) || length(n_components) != 1L || is.na(n_components) ||
      !is.finite(n_components) || n_components < 1 || n_components != floor(n_components)) {
    triomics_integration_abort(
      "C_SCORE_PARAMETER", "n_components must be one positive integer.", key = "n_components"
    )
  }
  scale_features <- integration_spec$scale_features
  if (!is.logical(scale_features) || length(scale_features) != 1L || is.na(scale_features)) {
    triomics_integration_abort(
      "C_SCORE_PARAMETER", "scale_features must be explicitly TRUE or FALSE.", key = "scale_features"
    )
  }
  block_scaling <- integration_spec$block_scaling
  if (!is.character(block_scaling) || length(block_scaling) != 1L || is.na(block_scaling) ||
      !block_scaling %in% c("none", "inverse_sqrt_p")) {
    triomics_integration_abort(
      "C_SCORE_PARAMETER", "block_scaling must be none or inverse_sqrt_p.", key = "block_scaling"
    )
  }
  selection <- integration_spec$selection
  if (!is.list(selection)) {
    triomics_integration_abort(
      "C_SCORE_PARAMETER", "selection must explicitly name a dense selection rule.", key = "selection"
    )
  }
  rule <- selection$rule
  top_k <- selection$top_k
  if (is.null(rule) && !is.null(selection$method)) {
    rule <- selection$method
    if (identical(rule, "top_k")) top_k <- selection$value
  }
  if (!is.character(rule) || length(rule) != 1L || is.na(rule)) {
    triomics_integration_abort(
      "C_SCORE_PARAMETER", "selection must name an explicit dense selection rule.", key = "selection"
    )
  }
  if (!rule %in% c("none", "top_k")) {
    triomics_integration_abort(
      "C_SCORE_SELECTION_UNSUPPORTED",
      "C-01 supports the explicit none and top_k dense selection rules.", key = "selection.rule"
    )
  }
  if (identical(rule, "top_k")) {
    if (!is.numeric(top_k) || length(top_k) != 1L || is.na(top_k) ||
        !is.finite(top_k) || top_k < 1 || top_k != floor(top_k)) {
      triomics_integration_abort(
        "C_SCORE_PARAMETER", "top_k selection requires one positive integer.", key = "selection.top_k"
      )
    }
  }
  list(
    requested_n_components = as.integer(n_components),
    scale_features = scale_features,
    block_scaling = block_scaling,
    selection = list(
      rule = rule,
      top_k = if (is.null(top_k)) NULL else as.integer(top_k),
      criterion = if (identical(rule, "top_k")) {
        "training_variance_descending_then_feature_key"
      } else {
        "retain_all_training_nonzero_variance_features"
      }
    )
  )
}

triomics_c01_select_features <- function(feature_keys, training_matrix, selection) {
  variances <- apply(training_matrix, 2L, stats::var)
  names(variances) <- feature_keys
  if (identical(selection$rule, "none")) return(feature_keys)
  ranked <- names(sort(variances, decreasing = TRUE))
  ranked <- ranked[order(-variances[ranked], ranked, method = "radix")]
  head(ranked, min(selection$top_k, length(ranked)))
}

triomics_c01_fit_block <- function(block, block_id, training_unit_ids, pca_spec) {
  input_features <- sort(block$feature_keys, method = "radix")
  raw_training <- t(block$assay[input_features, training_unit_ids, drop = FALSE])
  feature_means_all <- colMeans(raw_training)
  feature_sds_all <- apply(raw_training, 2L, stats::sd)
  names(feature_means_all) <- input_features
  names(feature_sds_all) <- input_features
  zero_variance <- input_features[!is.finite(feature_sds_all) | feature_sds_all == 0]
  variable_features <- setdiff(input_features, zero_variance)

  if (!length(variable_features)) {
    return(list(
      block_id = block_id,
      input_features = input_features,
      selected_features = character(),
      zero_variance = zero_variance,
      feature_variance = numeric(),
      means = numeric(),
      scales = numeric(),
      matrix = matrix(numeric(), nrow = length(training_unit_ids), ncol = 0L,
                      dimnames = list(training_unit_ids, character()))
    ))
  }

  variable_training <- raw_training[, variable_features, drop = FALSE]
  feature_variance <- apply(variable_training, 2L, stats::var)
  names(feature_variance) <- variable_features
  selected_features <- triomics_c01_select_features(
    variable_features, variable_training, pca_spec$selection
  )
  selected_features <- sort(selected_features, method = "radix")
  means <- feature_means_all[selected_features]
  scales <- if (isTRUE(pca_spec$scale_features)) {
    feature_sds_all[selected_features]
  } else {
    stats::setNames(rep(1, length(selected_features)), selected_features)
  }
  prepared <- sweep(raw_training[, selected_features, drop = FALSE], 2L, means, "-")
  prepared <- sweep(prepared, 2L, scales, "/")
  dimnames(prepared) <- list(training_unit_ids, selected_features)

  list(
    block_id = block_id,
    input_features = input_features,
    selected_features = selected_features,
    zero_variance = zero_variance,
    feature_variance = feature_variance,
    means = means,
    scales = scales,
    matrix = prepared
  )
}

triomics_integration_pca_fit <- function(resolved_analysis, integration_spec) {
  pca_spec <- triomics_c01_validate_pca_spec(integration_spec)
  eligibility <- triomics_integration_eligibility(resolved_analysis, integration_spec)
  block_ids <- eligibility$block_ids
  training_unit_ids <- eligibility$training_unit_ids
  if (length(training_unit_ids) < 2L) {
    triomics_integration_abort(
      "C_SCORE_RANK_ZERO", "PCA needs at least two eligible training units.", key = "training_unit_ids"
    )
  }

  block_fits <- lapply(block_ids, function(block_id) {
    triomics_c01_fit_block(
      resolved_analysis$blocks[[block_id]], block_id, training_unit_ids, pca_spec
    )
  })
  names(block_fits) <- block_ids
  selected <- lapply(block_fits, `[[`, "selected_features")
  if (any(lengths(selected) == 0L)) {
    triomics_integration_abort(
      "C_SCORE_RANK_ZERO", "Every requested PCA block must retain at least one variable feature."
    )
  }

  retained_counts <- lengths(selected)
  block_weights <- if (identical(pca_spec$block_scaling, "inverse_sqrt_p")) {
    stats::setNames(1 / sqrt(retained_counts), block_ids)
  } else {
    stats::setNames(rep(1, length(block_ids)), block_ids)
  }
  raw_matrices <- lapply(block_fits, `[[`, "matrix")
  weighted_matrices <- Map(function(x, weight) x * weight, raw_matrices, block_weights)
  all_feature_keys <- unlist(selected, use.names = FALSE)
  column_ids <- sprintf("C01F%05d", seq_along(all_feature_keys))
  feature_map <- do.call(rbind, lapply(seq_along(block_ids), function(i) {
    block_id <- block_ids[[i]]
    keys <- selected[[block_id]]
    first_column <- if (i == 1L) 1L else sum(retained_counts[seq_len(i - 1L)]) + 1L
    last_column <- first_column + length(keys) - 1L
    data.frame(
      block_id = block_id,
      feature_key = keys,
      column_id = column_ids[first_column:last_column],
      stringsAsFactors = FALSE
    )
  }))
  colnames_list <- feature_map$column_id
  training_x <- do.call(cbind, weighted_matrices)
  storage.mode(training_x) <- "double"
  colnames(training_x) <- colnames_list
  rownames(training_x) <- training_unit_ids

  max_rank <- min(length(training_unit_ids) - 1L, ncol(training_x))
  if (max_rank < 1L) {
    triomics_integration_abort("C_SCORE_RANK_ZERO", "The training matrix has no possible PCA component.")
  }
  requested_n_components <- pca_spec$requested_n_components
  native_fit <- tryCatch(
    stats::prcomp(training_x, center = TRUE, scale. = FALSE, rank. = max_rank),
    error = function(error) {
      triomics_integration_abort(
        "C_SCORE_FIT_FAILED", "stats::prcomp could not fit the declared training matrix.",
        key = "integration", remediation = "Inspect the keyed training values and declared PCA dimensions."
      )
    }
  )
  if (!length(native_fit$sdev) || !all(is.finite(native_fit$sdev))) {
    triomics_integration_abort("C_SCORE_RANK_ZERO", "PCA returned no finite singular values.")
  }
  singular_values <- native_fit$sdev * sqrt(length(training_unit_ids) - 1L)
  rank_tolerance <- max(singular_values) * max(dim(training_x)) * .Machine$double.eps
  numerical_rank <- sum(singular_values > rank_tolerance)
  retained_n_components <- min(requested_n_components, numerical_rank, length(native_fit$sdev))
  if (retained_n_components < 1L) {
    triomics_integration_abort(
      "C_SCORE_RANK_ZERO", "The centered training matrix has numerical rank zero."
    )
  }
  component_ids <- triomics_c01_component_ids(retained_n_components)
  scores <- native_fit$x[, seq_len(retained_n_components), drop = FALSE]
  rotation <- native_fit$rotation[, seq_len(retained_n_components), drop = FALSE]
  colnames(scores) <- component_ids
  colnames(rotation) <- component_ids

  loadings <- lapply(block_ids, function(block_id) {
    rows <- feature_map$block_id == block_id
    values <- rotation[feature_map$column_id[rows], , drop = FALSE]
    rownames(values) <- feature_map$feature_key[rows]
    values
  })
  names(loadings) <- block_ids

  variance <- matrix(
    NA_real_, nrow = length(block_ids), ncol = retained_n_components,
    dimnames = list(block_ids, component_ids)
  )
  block_train_matrices <- Map(function(x, weight) x * weight, raw_matrices, block_weights)
  names(block_train_matrices) <- block_ids
  for (block_id in block_ids) {
    map_rows <- feature_map$block_id == block_id
    column_ids_block <- feature_map$column_id[map_rows]
    centered_block <- sweep(
      block_train_matrices[[block_id]], 2L,
      native_fit$center[column_ids_block], "-"
    )
    denominator <- sum(centered_block^2)
    if (!is.finite(denominator) || denominator <= 0) {
      triomics_integration_abort(
        "C_VARIANCE_DENOMINATOR", "A retained PCA block must have positive training sum of squares.",
        key = block_id
      )
    }
    for (component in seq_len(retained_n_components)) {
      reconstructed_component <- tcrossprod(
        scores[, component], loadings[[block_id]][, component]
      )
      variance[block_id, component] <- sum(reconstructed_component^2) / denominator
    }
  }
  attr(variance, "metric") <- "orthogonal_pca_block_projection"
  attr(variance, "assay_space") <- "centered_scaled_block_weighted_training"
  attr(variance, "denominator") <- "block_training_frobenius_sum_of_squares"
  attr(variance, "semantics") <- "incremental"

  feature_flow <- lapply(block_fits, function(block_fit) {
    list(
      input = block_fit$input_features,
      selected = block_fit$selected_features,
      retained = block_fit$selected_features,
      zero_variance = block_fit$zero_variance,
      training_variance = block_fit$feature_variance,
      excluded_by_selection = setdiff(
        setdiff(block_fit$input_features, block_fit$zero_variance),
        block_fit$selected_features
      )
    )
  })
  names(feature_flow) <- block_ids
  means <- unlist(lapply(block_fits, `[[`, "means"), use.names = FALSE)
  scales <- unlist(lapply(block_fits, `[[`, "scales"), use.names = FALSE)
  names(means) <- colnames_list
  names(scales) <- colnames_list
  state <- list(
    state_version = "C-01-pca-1",
    block_ids = block_ids,
    input_feature_keys = lapply(block_fits, `[[`, "input_features"),
    selected_feature_keys = selected,
    feature_map = feature_map,
    feature_means = means,
    feature_scales = scales,
    block_weights = block_weights,
    prcomp_center = native_fit$center,
    rotation = rotation,
    training_unit_ids = training_unit_ids,
    training_unit_map = training_unit_ids,
    study_id = eligibility$study_id,
    preparation_context_id = eligibility$preparation_context_id,
    representation_id = eligibility$representation_id,
    scale_features = pca_spec$scale_features,
    block_scaling = pca_spec$block_scaling,
    selection = pca_spec$selection,
    numerical_rank = numerical_rank,
    rank_tolerance = rank_tolerance
  )
  params <- list(
    method = "concat_pca",
    effective = list(
      n_components = retained_n_components,
      scale_features = pca_spec$scale_features,
      block_scaling = pca_spec$block_scaling,
      selection = pca_spec$selection
    ),
    requested_n_components = requested_n_components,
    retained_n_components = retained_n_components,
    rank_truncated = retained_n_components < requested_n_components,
    rank_lineage = list(
      training_unit_count = length(training_unit_ids),
      selected_feature_count = ncol(training_x),
      maximum_centered_rank = max_rank,
      numerical_rank = numerical_rank,
      tolerance = rank_tolerance,
      requested_components = requested_n_components,
      retained_components = retained_n_components,
      omitted_component_ids = if (retained_n_components < requested_n_components) {
        paste0("PC", seq.int(retained_n_components + 1L, requested_n_components))
      } else {
        character()
      },
      disposition = if (retained_n_components < requested_n_components) {
        "truncated_to_training_numeric_rank"
      } else {
        "all_requested_components_retained"
      }
    ),
    feature_flow = feature_flow,
    variance = list(
      metric = "orthogonal_pca_block_projection",
      assay_space = "centered_scaled_block_weighted_training",
      denominator = "block_training_frobenius_sum_of_squares",
      semantics = "incremental"
    ),
    diagnostics = list(
      global_prcomp_variance = native_fit$sdev^2 / sum(native_fit$sdev^2),
      global_prcomp_sdev = native_fit$sdev
    ),
    eligibility = list(
      block_availability = eligibility$block_availability,
      unit_status = eligibility$unit_status
    ),
    state = state
  )
  triomics_new_integration_result(
    scores = scores,
    loadings = loadings,
    variance = variance,
    selected = selected,
    backend = "concat_pca",
    params = params
  )
}

triomics_integration_pca_apply <- function(fit, resolved_analysis, target_unit_ids = NULL) {
  fit <- triomics_validate_integration_result(fit)
  if (!identical(fit$backend, "concat_pca") || is.null(fit$params$state)) {
    triomics_integration_abort(
      "C_SCORE_BACKEND_UNAVAILABLE", "Apply requires a fitted concatenation PCA state."
    )
  }
  state <- fit$params$state
  if (is.null(target_unit_ids)) target_unit_ids <- resolved_analysis$unit_ids
  if (is.null(target_unit_ids)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_TARGET_MAP", "Apply requires explicit target unit ids.", key = "target_unit_ids"
    )
  }
  apply_unit_ids <- resolved_analysis$unit_ids
  if (is.null(apply_unit_ids)) apply_unit_ids <- target_unit_ids
  apply_spec <- list(
    block_ids = state$block_ids,
    unit_ids = apply_unit_ids,
    training_unit_ids = character(),
    target_unit_ids = target_unit_ids,
    required_views = state$block_ids
  )
  eligibility <- triomics_integration_eligibility(resolved_analysis, apply_spec)
  if (!identical(eligibility$study_id, state$study_id) ||
      !identical(eligibility$preparation_context_id, state$preparation_context_id) ||
      !identical(eligibility$representation_id, state$representation_id)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_CONTEXT_MISMATCH", "Apply data do not match the fitted study context and representation."
    )
  }
  for (block_id in state$block_ids) {
    observed <- sort(eligibility$block_feature_keys[[block_id]], method = "radix")
    expected <- sort(state$input_feature_keys[[block_id]], method = "radix")
    if (!identical(observed, expected)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_FEATURE_MAP",
        "Apply feature keys must exactly match the frozen input feature map.", key = block_id
      )
    }
  }
  eligible_units <- eligibility$unit_status$unit_id[
    eligibility$unit_status$status == "eligible"
  ]
  projected_units <- sort(eligible_units, method = "radix")
  if (!length(projected_units)) {
    triomics_integration_abort(
      "C_SCORE_REQUIRED_VIEW_MISSING", "No target unit has every required PCA view."
    )
  }

  prepared_blocks <- lapply(state$block_ids, function(block_id) {
    feature_keys <- state$selected_feature_keys[[block_id]]
    raw <- t(resolved_analysis$blocks[[block_id]]$assay[
      feature_keys, projected_units, drop = FALSE
    ])
    feature_rows <- state$feature_map[state$feature_map$block_id == block_id, , drop = FALSE]
    column_ids <- feature_rows$column_id
    means <- state$feature_means[column_ids]
    scales <- state$feature_scales[column_ids]
    transformed <- sweep(raw, 2L, means, "-")
    transformed <- sweep(transformed, 2L, scales, "/")
    transformed <- transformed * state$block_weights[[block_id]]
    colnames(transformed) <- column_ids
    rownames(transformed) <- projected_units
    transformed
  })
  names(prepared_blocks) <- state$block_ids
  new_x <- do.call(cbind, prepared_blocks)
  native_centered <- sweep(new_x, 2L, state$prcomp_center[colnames(new_x)], "-")
  scores <- native_centered %*% state$rotation
  rownames(scores) <- projected_units
  colnames(scores) <- colnames(state$rotation)
  params <- fit$params
  params$applied_unit_status <- eligibility$unit_status
  triomics_new_integration_result(
    scores = scores,
    loadings = fit$loadings,
    variance = fit$variance,
    selected = fit$selected,
    backend = fit$backend,
    params = params
  )
}

triomics_integration_pca_backend_fit <- function(resolved_analysis, params) {
  common <- triomics_integration_pca_fit(resolved_analysis, params)
  structure(
    list(
      native_state = list(
        state = common$params$state,
        training_scores = common$scores,
        block_loadings = common$loadings,
        block_projection_fraction = common$variance
      ),
      selected = common$selected,
      result_params = common$params,
      metadata = list(
        backend = common$backend,
        backend_version = R.version.string,
        model_role = "unsupervised_predictor_structure",
        training_unit_ids = common$params$state$training_unit_ids,
        preparation_context_id = common$params$state$preparation_context_id
      )
    ),
    class = "BackendFit"
  )
}

triomics_c01_common_result_from_backend_fit <- function(backend_fit) {
  if (!inherits(backend_fit, "BackendFit")) {
    triomics_integration_abort("C_SCORE_BACKEND_UNAVAILABLE", "A BackendFit value is required.")
  }
  triomics_new_integration_result(
    scores = backend_fit$native_state$training_scores,
    loadings = backend_fit$native_state$block_loadings,
    variance = backend_fit$native_state$block_projection_fraction,
    selected = backend_fit$selected,
    backend = backend_fit$metadata$backend,
    params = backend_fit$result_params
  )
}

triomics_integration_pca_backend_predict <- function(backend_fit, new_resolved_analysis, params) {
  triomics_integration_pca_apply(
    triomics_c01_common_result_from_backend_fit(backend_fit), new_resolved_analysis,
    target_unit_ids = params$target_unit_ids
  )
}

triomics_integration_pca_backend_extract <- function(backend_fit, context, params) {
  list(
    result = triomics_validate_integration_result(
      triomics_c01_common_result_from_backend_fit(backend_fit)
    ),
    sidecars = list(
      native_state = backend_fit$native_state,
      context = context,
      params = params
    )
  )
}
