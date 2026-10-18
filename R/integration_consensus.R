triomics_c01_score_matrix <- function(value, view_id) {
  if (!is.matrix(value) || !is.numeric(value) || is.complex(value) ||
      length(dim(value)) != 2L || any(dim(value) < 1L)) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", "Native block scores must be non-empty numeric matrices.",
      key = view_id
    )
  }
  row_ids <- triomics_integration_component_names(
    rownames(value), paste0(view_id, " score units"), "C_SCORE_RESULT_INVALID"
  )
  component_ids <- triomics_integration_component_names(
    colnames(value), paste0(view_id, " score components"), "C_SCORE_RESULT_INVALID"
  )
  if (any(!is.finite(value))) {
    triomics_integration_abort(
      "C_SCORE_NONFINITE", "Native block scores must be finite.", key = view_id
    )
  }
  list(rows = row_ids, columns = component_ids)
}

triomics_c01_loading_matrix <- function(value, view_id) {
  map <- triomics_integration_finite_matrix(value, paste0("native_loadings.", view_id))
  list(features = map$rows, components = map$columns)
}

make_consensus_state <- function(native_scores, native_loadings, training_units, params = list()) {
  if (!is.list(native_scores) || is.null(names(native_scores)) ||
      anyDuplicated(names(native_scores)) || !is.list(native_loadings) ||
      is.null(names(native_loadings)) || anyDuplicated(names(native_loadings))) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", "Native scores and loadings must be named block lists."
    )
  }
  required_views <- params$required_views
  if (is.null(required_views)) required_views <- names(native_scores)
  required_views <- sort(triomics_c01_ids(
    required_views, "required_views", "C_SCORE_REQUIRED_VIEW_MISSING"
  ), method = "radix")
  if (length(setdiff(required_views, names(native_scores))) ||
      length(setdiff(required_views, names(native_loadings)))) {
    triomics_integration_abort(
      "C_SCORE_REQUIRED_VIEW_MISSING",
      "Every required view needs native scores and native loadings."
    )
  }
  training_units <- sort(triomics_c01_ids(
    training_units, "training_units", "C_ELIGIBILITY_TRAINING_MAP"
  ), method = "radix")
  correlation_tolerance <- params$correlation_tolerance
  if (is.null(correlation_tolerance)) correlation_tolerance <- sqrt(.Machine$double.eps)
  sd_tolerance <- params$sd_tolerance
  if (is.null(sd_tolerance)) sd_tolerance <- sqrt(.Machine$double.eps)
  if (!is.numeric(correlation_tolerance) || length(correlation_tolerance) != 1L ||
      is.na(correlation_tolerance) || !is.finite(correlation_tolerance) ||
      correlation_tolerance < 0 || correlation_tolerance >= 1 ||
      !is.numeric(sd_tolerance) || length(sd_tolerance) != 1L ||
      is.na(sd_tolerance) || !is.finite(sd_tolerance) || sd_tolerance < 0) {
    triomics_integration_abort(
      "C_SCORE_PARAMETER", "Consensus correlation and SD tolerances must be finite and non-negative."
    )
  }

  score_maps <- lapply(required_views, function(view_id) {
    triomics_c01_score_matrix(native_scores[[view_id]], view_id)
  })
  names(score_maps) <- required_views
  anchor_view <- required_views[[1L]]
  component_ids <- score_maps[[anchor_view]]$columns
  for (view_id in required_views) {
    score_map <- score_maps[[view_id]]
    loading_map <- triomics_c01_loading_matrix(native_loadings[[view_id]], view_id)
    if (length(setdiff(training_units, score_map$rows)) ||
        !setequal(score_map$columns, component_ids) ||
        !setequal(loading_map$components, component_ids)) {
      triomics_integration_abort(
        "C_SCORE_RESULT_INVALID",
        "Every required view must cover the declared training units and component map.",
        key = view_id
      )
    }
  }

  anchor_loadings <- native_loadings[[anchor_view]][, component_ids, drop = FALSE]
  signs <- matrix(
    1, nrow = length(required_views), ncol = length(component_ids),
    dimnames = list(required_views, component_ids)
  )
  anchor_features <- stats::setNames(character(length(component_ids)), component_ids)
  means <- matrix(
    NA_real_, nrow = length(required_views), ncol = length(component_ids),
    dimnames = list(required_views, component_ids)
  )
  sds <- means

  for (component_id in component_ids) {
    anchor_values <- anchor_loadings[, component_id]
    largest <- max(abs(anchor_values))
    if (!is.finite(largest) || largest <= 0) {
      triomics_integration_abort(
        "C_SCORE_RANK_ZERO", "The anchor loading vector has no orientable feature.",
        key = paste(anchor_view, component_id, sep = ".")
      )
    }
    tied_features <- names(anchor_values)[abs(anchor_values) == largest]
    anchor_feature <- sort(tied_features, method = "radix")[[1L]]
    anchor_features[[component_id]] <- anchor_feature
    signs[anchor_view, component_id] <- if (anchor_values[[anchor_feature]] < 0) -1 else 1

    anchor_training <- native_scores[[anchor_view]][training_units, component_id] *
      signs[anchor_view, component_id]
    anchor_sd <- stats::sd(anchor_training)
    if (!is.finite(anchor_sd) || anchor_sd <= sd_tolerance) {
      triomics_integration_abort(
        "C_SCORE_RANK_ZERO", "The anchor score has zero or near-zero training SD.",
        key = paste(anchor_view, component_id, sep = ".")
      )
    }
    for (view_id in setdiff(required_views, anchor_view)) {
      view_training <- native_scores[[view_id]][training_units, component_id]
      view_sd <- stats::sd(view_training)
      if (!is.finite(view_sd) || view_sd <= sd_tolerance) {
        triomics_integration_abort(
          "C_SCORE_RANK_ZERO", "A required native score has zero or near-zero training SD.",
          key = paste(view_id, component_id, sep = ".")
        )
      }
      alignment <- suppressWarnings(stats::cor(anchor_training, view_training))
      if (!is.finite(alignment) || abs(alignment) <= correlation_tolerance) {
        triomics_integration_abort(
          "C_SCORE_ALIGNMENT_UNDEFINED",
          "A required block score cannot be aligned to the anchor on training units.",
          key = paste(view_id, component_id, sep = ".")
        )
      }
      signs[view_id, component_id] <- if (alignment < 0) -1 else 1
    }

    for (view_id in required_views) {
      signed_training <- native_scores[[view_id]][training_units, component_id] *
        signs[view_id, component_id]
      training_sd <- stats::sd(signed_training)
      if (!is.finite(training_sd) || training_sd <= sd_tolerance) {
        triomics_integration_abort(
          "C_SCORE_RANK_ZERO", "A required native score has zero or near-zero training SD.",
          key = paste(view_id, component_id, sep = ".")
        )
      }
      means[view_id, component_id] <- mean(signed_training)
      sds[view_id, component_id] <- training_sd
    }
  }

  structure(
    list(
      state_version = "C-01-consensus-1",
      anchor_view = anchor_view,
      anchor_features = anchor_features,
      required_views = required_views,
      component_ids = component_ids,
      training_units = training_units,
      alignment_units = training_units,
      signs = signs,
      means = means,
      sds = sds,
      weights = stats::setNames(rep(1 / length(required_views), length(required_views)), required_views),
      correlation_tolerance = correlation_tolerance,
      sd_tolerance = sd_tolerance,
      score_definition = "equal_weight_mean_of_signed_training_standardized_native_scores"
    ),
    class = "ConsensusState"
  )
}

apply_consensus_state <- function(native_scores, state) {
  if (!inherits(state, "ConsensusState") || !is.list(native_scores) ||
      is.null(names(native_scores)) || anyDuplicated(names(native_scores))) {
    triomics_integration_abort(
      "C_SCORE_RESULT_INVALID", "Consensus apply needs a named score list and frozen ConsensusState."
    )
  }
  required_views <- state$required_views
  if (length(setdiff(required_views, names(native_scores)))) {
    triomics_integration_abort(
      "C_SCORE_REQUIRED_VIEW_MISSING",
      "A frozen consensus score cannot be computed without every required view."
    )
  }
  score_maps <- lapply(required_views, function(view_id) {
    triomics_c01_score_matrix(native_scores[[view_id]], view_id)
  })
  names(score_maps) <- required_views
  component_ids <- state$component_ids
  for (view_id in required_views) {
    if (length(setdiff(component_ids, score_maps[[view_id]]$columns)) ||
        length(setdiff(score_maps[[view_id]]$columns, component_ids))) {
      triomics_integration_abort(
        "C_SCORE_RESULT_INVALID", "Native score components must match the frozen state.",
        key = view_id
      )
    }
  }
  first_units <- score_maps[[required_views[[1L]]]]$rows
  same_units <- vapply(score_maps, function(map) setequal(map$rows, first_units), logical(1))
  if (!all(same_units)) {
    triomics_integration_abort(
      "C_SCORE_REQUIRED_VIEW_MISSING",
      "Every target unit must have every required view; consensus never averages fewer views."
    )
  }
  unit_ids <- sort(first_units, method = "radix")
  standardized <- lapply(required_views, function(view_id) {
    score <- native_scores[[view_id]][unit_ids, component_ids, drop = FALSE]
    signed <- sweep(score, 2L, state$signs[view_id, component_ids], "*")
    centered <- sweep(signed, 2L, state$means[view_id, component_ids], "-")
    sweep(centered, 2L, state$sds[view_id, component_ids], "/")
  })
  names(standardized) <- required_views
  consensus <- Reduce(`+`, Map(function(values, view_id) {
    values * state$weights[[view_id]]
  }, standardized, required_views))
  rownames(consensus) <- unit_ids
  colnames(consensus) <- component_ids
  consensus
}
