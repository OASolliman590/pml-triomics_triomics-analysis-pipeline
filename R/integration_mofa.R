# Packet C-05: MOFA2 adapter gate. The public registry still refuses "mofa2"
# (see triomics_integration_backend_names()); this adapter is exercised directly
# until Sol applies the shared registration.

triomics_mofa_abort <- function(code, message, key = NULL,
                                remediation = "Install MOFA2 and mofapy2 in an isolated environment, then rerun the C-05 checks.") {
  triomics_abort(
    code, message, key = key, module = "evaluation", stage = "C",
    remediation = remediation
  )
}

triomics_mofa_ids <- function(value, label, code = "C_ELIGIBILITY_INPUT_INVALID") {
  if (!is.character(value) || length(value) == 0L || anyNA(value) ||
      any(!nzchar(value)) || anyDuplicated(value)) {
    triomics_mofa_abort(code, paste0(label, " must be unique, non-blank character ids."), key = label)
  }
  value
}

triomics_mofa_n_components <- function(params) {
  values <- params[c("n_components", "ncomp")]
  values <- values[!vapply(values, is.null, logical(1L))]
  if (!length(values)) return(2L)
  valid <- vapply(values, function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value >= 1 && value == trunc(value)
  }, logical(1L))
  if (any(!valid)) {
    triomics_mofa_abort("C_SCORE_COMPONENT_RANK", "MOFA2 components must be one positive integer.", key = "n_components")
  }
  integer_values <- vapply(values, as.integer, integer(1L))
  if (length(integer_values) > 1L && any(integer_values != integer_values[[1L]])) {
    triomics_mofa_abort("C_SCORE_PARAMETER", "MOFA2 component declarations disagree.", key = "n_components")
  }
  integer_values[[1L]]
}

triomics_mofa_seed <- function(params) {
  value <- params$seed
  if (is.null(value)) value <- params$execution_seed
  if (is.null(value)) value <- 1L
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value != trunc(value)) {
    triomics_mofa_abort("C_SCORE_PARAMETER", "MOFA2 seed must be one finite integer.", key = "seed")
  }
  as.integer(value)
}

triomics_mofa_convergence <- function(params) {
  value <- params$mofa_convergence
  if (is.null(value)) value <- "medium"
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !value %in% c("fast", "medium", "slow")) {
    triomics_mofa_abort(
      "C_SCORE_PARAMETER", "MOFA2 convergence mode must be fast, medium, or slow.",
      key = "mofa_convergence"
    )
  }
  value
}

triomics_mofa_seed_repeats <- function(params) {
  value <- params$mofa_seed_repeats
  if (is.null(value)) value <- 1L
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 1 || value != trunc(value) ||
      value > .Machine$integer.max) {
    triomics_mofa_abort(
      "C_SCORE_PARAMETER", "MOFA2 seed repeats must be one positive integer.",
      key = "mofa_seed_repeats"
    )
  }
  as.integer(value)
}

triomics_mofa_validate <- function(resolved_analysis, params) {
  if (!is.list(resolved_analysis) || !is.list(params) ||
      !identical(as.character(params$method), "mofa2")) {
    triomics_mofa_abort(
      "C_ELIGIBILITY_INPUT_INVALID",
      "MOFA2 requires a named resolved analysis and method=mofa2.",
      key = "method"
    )
  }
  blocks <- resolved_analysis$blocks
  if (!is.list(blocks) || is.null(names(blocks)) || length(blocks) < 2L ||
      anyNA(names(blocks)) || any(!nzchar(names(blocks))) || anyDuplicated(names(blocks))) {
    triomics_mofa_abort(
      "C_ELIGIBILITY_BLOCK_MAP",
      "MOFA2 requires at least two uniquely named predictor views.",
      key = "blocks"
    )
  }

  block_ids <- params$block_ids
  if (is.null(block_ids)) block_ids <- names(blocks)
  block_ids <- triomics_mofa_ids(block_ids, "block_ids", "C_ELIGIBILITY_BLOCK_MAP")
  if (length(block_ids) < 2L || !identical(sort(block_ids, method = "radix"),
                                            sort(names(blocks), method = "radix"))) {
    triomics_mofa_abort(
      "C_ELIGIBILITY_BLOCK_MAP",
      "MOFA2 block_ids must name every resolved predictor view exactly once.",
      key = "block_ids"
    )
  }

  block_data <- stats::setNames(vector("list", length(block_ids)), block_ids)
  missingness_masks <- stats::setNames(vector("list", length(block_ids)), block_ids)
  all_units <- character()
  for (block_id in block_ids) {
    block <- blocks[[block_id]]
    if (!is.list(block) || is.null(block$assay)) {
      triomics_mofa_abort(
        "C_ELIGIBILITY_INPUT_INVALID",
        paste0("MOFA2 view '", block_id, "' must supply keyed assay values."),
        key = block_id
      )
    }
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        length(dim(assay)) != 2L || any(dim(assay) < 1L) ||
        any(is.infinite(assay) | is.nan(assay), na.rm = TRUE) ||
        is.null(rownames(assay)) || is.null(colnames(assay))) {
      triomics_mofa_abort(
        "C_ELIGIBILITY_ASSAY_INVALID",
        paste0("MOFA2 view '", block_id, "' must be a non-empty finite-keyed numeric matrix."),
        key = block_id
      )
    }
    feature_keys <- block$feature_keys
    if (is.null(feature_keys)) feature_keys <- rownames(assay)
    sample_ids <- block$sample_ids
    if (is.null(sample_ids)) sample_ids <- colnames(assay)
    feature_keys <- triomics_mofa_ids(feature_keys, paste0(block_id, ".feature_keys"), "C_ELIGIBILITY_FEATURE_MAP")
    sample_ids <- triomics_mofa_ids(sample_ids, paste0(block_id, ".sample_ids"), "C_ELIGIBILITY_SAMPLE_MAP")
    if (!identical(feature_keys, rownames(assay)) || !identical(sample_ids, colnames(assay)) ||
        length(feature_keys) != nrow(assay) || length(sample_ids) != ncol(assay)) {
      triomics_mofa_abort(
        "C_ELIGIBILITY_SAMPLE_MAP",
        paste0("MOFA2 view '", block_id, "' assay names do not match its feature/sample maps."),
        key = block_id
      )
    }
    if (!any(!is.na(assay))) {
      triomics_mofa_abort(
        "C_NA_CAPABILITY",
        paste0("MOFA2 view '", block_id, "' is wholly unobserved and cannot be estimated."),
        key = block_id
      )
    }
    block_data[[block_id]] <- list(
      assay = assay, feature_keys = feature_keys, sample_ids = sample_ids
    )
    all_units <- c(all_units, sample_ids)
  }

  declared_units <- resolved_analysis$unit_ids
  if (is.null(declared_units)) declared_units <- unique(all_units)
  declared_units <- triomics_mofa_ids(declared_units, "unit_ids", "C_ELIGIBILITY_UNIT_MAP")
  if (length(setdiff(all_units, declared_units))) {
    triomics_mofa_abort(
      "C_ELIGIBILITY_UNIT_MAP",
      "A MOFA2 view contains a sample outside the resolved unit map.",
      key = "unit_ids"
    )
  }
  units <- sort(declared_units, method = "radix")
  if (length(units) < 2L) {
    triomics_mofa_abort("C_ELIGIBILITY_UNIT_MAP", "MOFA2 requires at least two training units.", key = "unit_ids")
  }

  data <- stats::setNames(vector("list", length(block_ids)), block_ids)
  transforms <- stats::setNames(vector("list", length(block_ids)), block_ids)
  scale_features <- params$scale_features
  if (is.null(scale_features)) scale_features <- TRUE
  if (!is.logical(scale_features) || length(scale_features) != 1L || is.na(scale_features)) {
    triomics_mofa_abort("C_SCORE_PARAMETER", "MOFA2 feature scaling must be explicitly TRUE or FALSE.", key = "scale_features")
  }
  for (block_id in block_ids) {
    current <- block_data[[block_id]]
    assay <- matrix(NA_real_, nrow = length(current$feature_keys), ncol = length(units),
                    dimnames = list(current$feature_keys, units))
    assay[, match(current$sample_ids, units)] <- current$assay
    missing_features <- which(rowSums(!is.na(assay)) == 0L)
    if (length(missing_features)) {
      triomics_mofa_abort(
        "C_NA_CAPABILITY",
        paste0("MOFA2 view '", block_id, "' contains an all-missing feature."),
        key = current$feature_keys[[missing_features[[1L]]]]
      )
    }
    missingness_masks[[block_id]] <- is.na(assay)
    if (isTRUE(scale_features)) {
      center <- rowMeans(assay, na.rm = TRUE)
      counts <- rowSums(!is.na(assay))
      spread <- vapply(seq_len(nrow(assay)), function(i) {
        values <- assay[i, !is.na(assay[i, ])]
        if (length(values) < 2L) return(NA_real_)
        stats::sd(values)
      }, numeric(1L))
      if (any(!is.finite(center)) || any(!is.finite(spread)) || any(spread <= .Machine$double.eps)) {
        triomics_mofa_abort(
          "C_SCORE_COMPONENT_RANK",
          paste0("MOFA2 view '", block_id, "' contains a feature that cannot be scaled."),
          key = block_id
        )
      }
      assay <- sweep(assay, 1L, center, "-")
      assay <- sweep(assay, 1L, spread, "/")
      transforms[[block_id]] <- list(
        center = stats::setNames(center, current$feature_keys),
        scale = stats::setNames(spread, current$feature_keys),
        observed_count = stats::setNames(counts, current$feature_keys),
        method = "feature_z_score_observed_values_only"
      )
    } else {
      transforms[[block_id]] <- list(
        center = NULL, scale = NULL,
        observed_count = stats::setNames(rowSums(!is.na(assay)), current$feature_keys),
        method = "none"
      )
    }
    data[[block_id]] <- assay
  }
  observation_by_unit <- Reduce("+", lapply(data, function(value) colSums(!is.na(value))))
  if (any(observation_by_unit == 0L)) {
    triomics_mofa_abort(
      "C_NA_CAPABILITY",
      "MOFA2 training units with no observed value in any view are ineligible.",
      key = "unit_ids"
    )
  }

  list(
    block_ids = block_ids, units = units, data = data, transforms = transforms,
    missingness_mask = missingness_masks,
    scale_features = isTRUE(scale_features), n_components = triomics_mofa_n_components(params),
    seed = triomics_mofa_seed(params), seed_repeats = triomics_mofa_seed_repeats(params),
    convergence = triomics_mofa_convergence(params),
    study_id = if (is.null(resolved_analysis$study_id)) NA_character_ else resolved_analysis$study_id,
    preparation_context_id = if (is.null(resolved_analysis$preparation_context_id)) NA_character_ else resolved_analysis$preparation_context_id,
    representation_id = if (!is.null(resolved_analysis$representation_id)) resolved_analysis$representation_id else if (is.null(resolved_analysis$representation)) NA_character_ else resolved_analysis$representation
  )
}

`%||%` <- function(left, right) if (is.null(left)) right else left

triomics_mofa_prepare_options <- function(model, eligibility, params) {
  data_options <- MOFA2::get_default_data_options(model)
  model_options <- MOFA2::get_default_model_options(model)
  training_options <- MOFA2::get_default_training_options(model)
  supplied_data <- params$mofa_data_options %||% list()
  supplied_model <- params$mofa_model_options %||% list()
  supplied_training <- params$mofa_training_options %||% params$training_options %||% list()
  if (!is.list(supplied_data) || !is.list(supplied_model) || !is.list(supplied_training)) {
    triomics_mofa_abort("C_SCORE_PARAMETER", "MOFA2 native options must be lists.", key = "native_options")
  }
  data_options <- utils::modifyList(data_options, supplied_data)
  model_options <- utils::modifyList(model_options, supplied_model)
  training_options <- utils::modifyList(training_options, supplied_training)

  # Scaling is performed above so the exact transformed values and missingness
  # mask are under this adapter's control. Group labels/outcomes are never passed.
  data_options$scale_views <- FALSE
  data_options$scale_groups <- FALSE
  data_options$center_groups <- FALSE
  data_options$views <- eligibility$block_ids
  model_options$num_factors <- eligibility$n_components
  model_options$likelihoods <- stats::setNames(rep("gaussian", length(eligibility$block_ids)), eligibility$block_ids)
  training_options$convergence_mode <- eligibility$convergence
  training_options$seed <- eligibility$seed
  list(data = data_options, model = model_options, training = training_options)
}

triomics_mofa_restore_python <- function() {
  python <- Sys.getenv("TRIOMICS_MOFA_PYTHON", unset = "")
  if (!nzchar(python)) return(FALSE)
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    triomics_mofa_abort(
      "C_EVAL_NATIVE_FIT", "MOFA2 is loadable but reticulate is unavailable for the isolated mofapy2 runtime.",
      key = "TRIOMICS_MOFA_PYTHON"
    )
  }
  tryCatch(
    reticulate::use_python(python, required = TRUE),
    error = function(error) {
      triomics_mofa_abort(
        "C_EVAL_NATIVE_FIT", paste0("The isolated mofapy2 Python runtime could not be selected: ", conditionMessage(error)),
        key = "TRIOMICS_MOFA_PYTHON"
      )
    }
  )
  TRUE
}

triomics_mofa_native_matrix <- function(value, label, row_ids = NULL, column_ids = NULL) {
  if (!is.matrix(value) || !is.numeric(value) || is.complex(value) ||
      any(!is.finite(value)) || nrow(value) < 1L || ncol(value) < 1L) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", paste0("Native MOFA2 ", label, " is invalid."), key = label)
  }
  if (!is.null(row_ids)) {
    if (is.null(rownames(value))) rownames(value) <- row_ids
    if (!setequal(rownames(value), row_ids)) {
      triomics_mofa_abort("C_SCORE_RESULT_INVALID", paste0("Native MOFA2 ", label, " has an unexpected row map."), key = label)
    }
    value <- value[row_ids, , drop = FALSE]
  }
  if (!is.null(column_ids)) {
    if (is.null(colnames(value))) colnames(value) <- column_ids
    if (!setequal(colnames(value), column_ids)) {
      triomics_mofa_abort("C_SCORE_RESULT_INVALID", paste0("Native MOFA2 ", label, " has an unexpected factor map."), key = label)
    }
    value <- value[, column_ids, drop = FALSE]
  }
  value
}

triomics_mofa_variance_semantics <- function() {
  list(
    metric = "MOFA2::get_variance_explained$r2_per_factor",
    assay_space = "adapter_transformed_training_views",
    numerator = "observed_residual_sse_reduction_for_single_factor_reconstruction",
    denominator = "sum_of_squared_observed_transformed_values_about_zero_per_view",
    native_unit = "percentage",
    output_unit = "proportion",
    conversion = "divide_by_100_unconditionally",
    accumulation = "single_factor_per_view_against_zero_baseline_not_cumulative",
    missing_observations = "excluded_by_native_na_rm",
    interpretation = "exploratory_variance_diagnostic_not_predictive_variance"
  )
}

triomics_mofa_variance_proportions <- function(per_factor, factor_ids, block_ids) {
  if (!is.matrix(per_factor) || !is.numeric(per_factor) || is.complex(per_factor) ||
      is.null(rownames(per_factor)) || is.null(colnames(per_factor)) ||
      !setequal(rownames(per_factor), factor_ids) || !setequal(colnames(per_factor), block_ids)) {
    triomics_mofa_abort(
      "C_SCORE_VARIANCE_UNAVAILABLE", "MOFA2 per-factor variance has an unexpected map.",
      key = "variance"
    )
  }
  native <- per_factor[factor_ids, block_ids, drop = FALSE]
  if (any(!is.finite(native)) || any(native < 0 | native > 100)) {
    triomics_mofa_abort(
      "C_SCORE_VARIANCE_UNAVAILABLE", "MOFA2 native variance percentages must be finite values from 0 to 100.",
      key = "variance"
    )
  }
  variance <- t(native) / 100
  rownames(variance) <- block_ids
  colnames(variance) <- factor_ids
  list(
    variance = variance,
    native_variance_percentage = native,
    variance_units = "proportion_from_native_percentage",
    semantics = triomics_mofa_variance_semantics()
  )
}

triomics_mofa_native_outputs <- function(model, eligibility) {
  factors <- tryCatch(MOFA2::get_factors(model), error = function(error) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", paste0("MOFA2 factor extraction failed: ", conditionMessage(error)), key = "factors")
  })
  if (!is.list(factors) || length(factors) != 1L) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", "MOFA2 must return one ungrouped factor matrix.", key = "factors")
  }
  factors <- factors[[1L]]
  if (!is.matrix(factors)) factors <- as.matrix(factors)
  if (is.null(rownames(factors))) rownames(factors) <- eligibility$units
  factor_ids <- colnames(factors)
  if (is.null(factor_ids)) factor_ids <- paste0("Factor", seq_len(ncol(factors)))
  colnames(factors) <- factor_ids
  factors <- triomics_mofa_native_matrix(factors, "factors", eligibility$units)

  weights <- tryCatch(MOFA2::get_weights(model), error = function(error) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", paste0("MOFA2 weight extraction failed: ", conditionMessage(error)), key = "weights")
  })
  if (!is.list(weights)) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", "MOFA2 weights must be a named list of view matrices.", key = "weights")
  }
  if (is.null(names(weights))) names(weights) <- eligibility$block_ids
  if (!identical(sort(names(weights), method = "radix"), sort(eligibility$block_ids, method = "radix"))) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", "MOFA2 weights did not preserve the view map.", key = "weights")
  }
  weights <- weights[eligibility$block_ids]
  weights <- lapply(seq_along(weights), function(i) {
    block_id <- eligibility$block_ids[[i]]
    current <- weights[[i]]
    if (!is.matrix(current)) current <- as.matrix(current)
    if (is.null(rownames(current))) rownames(current) <- rownames(eligibility$data[[block_id]])
    if (is.null(colnames(current))) colnames(current) <- colnames(factors)
    triomics_mofa_native_matrix(
      current, paste0("weights.", block_id),
      row_ids = rownames(eligibility$data[[block_id]]), column_ids = colnames(factors)
    )
  })
  names(weights) <- eligibility$block_ids

  variance <- tryCatch(MOFA2::get_variance_explained(model), error = function(error) {
    triomics_mofa_abort("C_SCORE_VARIANCE_UNAVAILABLE", paste0("MOFA2 variance extraction failed: ", conditionMessage(error)), key = "variance")
  })
  per_factor <- variance$r2_per_factor
  if (!is.list(per_factor) || length(per_factor) != 1L) {
    triomics_mofa_abort("C_SCORE_VARIANCE_UNAVAILABLE", "MOFA2 variance output did not preserve the ungrouped model.", key = "variance")
  }
  per_factor <- per_factor[[1L]]
  converted_variance <- triomics_mofa_variance_proportions(
    per_factor, colnames(factors), eligibility$block_ids
  )
  list(
    factors = factors, weights = weights,
    variance = converted_variance$variance,
    native_variance_percentage = converted_variance$native_variance_percentage,
    variance_units = converted_variance$variance_units,
    variance_semantics = converted_variance$semantics
  )
}

triomics_mofa_outputs_agree <- function(fitted, reloaded) {
  same_matrix <- function(left, right) {
    identical(dim(left), dim(right)) && identical(dimnames(left), dimnames(right)) &&
      isTRUE(all.equal(left, right, check.attributes = FALSE, tolerance = 1e-7))
  }
  same_list <- function(left, right) {
    identical(names(left), names(right)) && length(left) == length(right) &&
      all(vapply(seq_along(left), function(i) same_matrix(left[[i]], right[[i]]), logical(1L)))
  }
  c(
    factors = same_matrix(fitted$factors, reloaded$factors),
    weights = same_list(fitted$weights, reloaded$weights),
    variance = same_matrix(fitted$variance, reloaded$variance),
    native_variance_percentage = same_matrix(
      fitted$native_variance_percentage, reloaded$native_variance_percentage
    )
  )
}

triomics_mofa_align_reloaded_outputs <- function(fitted, reloaded) {
  if (!is.matrix(fitted$factors) || !is.matrix(reloaded$factors) ||
      !identical(dim(fitted$factors), dim(reloaded$factors))) {
    triomics_mofa_abort("C_SCORE_RESULT_INVALID", "MOFA2 reload changed the factor dimensions.", key = "reload")
  }
  n_factors <- ncol(fitted$factors)
  remaining <- seq_len(n_factors)
  permutation <- integer(n_factors)
  signs <- rep(1, n_factors)
  for (i in seq_len(n_factors)) {
    correlations <- vapply(remaining, function(j) {
      left <- fitted$factors[, i]
      right <- reloaded$factors[, j]
      if (stats::sd(left) <= .Machine$double.eps || stats::sd(right) <= .Machine$double.eps) return(0)
      abs(stats::cor(left, right))
    }, numeric(1L))
    pick <- remaining[[which.max(correlations)]]
    permutation[[i]] <- pick
    left <- fitted$factors[, i]
    right <- reloaded$factors[, pick]
    signs[[i]] <- if (sum(left * right) < 0) -1 else 1
    remaining <- setdiff(remaining, pick)
  }
  aligned <- reloaded
  aligned$factors <- sweep(reloaded$factors[, permutation, drop = FALSE], 2L, signs, "*")
  colnames(aligned$factors) <- colnames(fitted$factors)
  aligned$weights <- lapply(reloaded$weights, function(value) {
    output <- sweep(value[, permutation, drop = FALSE], 2L, signs, "*")
    colnames(output) <- colnames(fitted$factors)
    output
  })
  aligned$variance <- reloaded$variance[, permutation, drop = FALSE]
  colnames(aligned$variance) <- colnames(fitted$factors)
  aligned$native_variance_percentage <- reloaded$native_variance_percentage[
    permutation, , drop = FALSE
  ]
  rownames(aligned$native_variance_percentage) <- colnames(fitted$factors)
  aligned
}

triomics_mofa_align_seed_factors <- function(reference, candidate) {
  if (!is.matrix(reference) || !is.numeric(reference) || any(!is.finite(reference)) ||
      !is.matrix(candidate) || !is.numeric(candidate) || any(!is.finite(candidate)) ||
      nrow(reference) != nrow(candidate) || is.null(rownames(reference)) ||
      is.null(rownames(candidate)) || !identical(rownames(reference), rownames(candidate)) ||
      ncol(reference) < 1L || ncol(candidate) < 1L) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "Seed factor matrices must share a finite keyed sample map.",
      key = "seed_sensitivity"
    )
  }
  correlations <- matrix(0, nrow = ncol(reference), ncol = ncol(candidate))
  for (i in seq_len(ncol(reference))) {
    for (j in seq_len(ncol(candidate))) {
      left <- reference[, i]
      right <- candidate[, j]
      if (stats::sd(left) > .Machine$double.eps && stats::sd(right) > .Machine$double.eps) {
        value <- suppressWarnings(stats::cor(left, right))
        if (is.finite(value)) correlations[i, j] <- value
      }
    }
  }
  permutation <- rep(NA_integer_, ncol(reference))
  signs <- rep(NA_real_, ncol(reference))
  aligned_correlations <- rep(NA_real_, ncol(reference))
  remaining_rows <- seq_len(nrow(correlations))
  remaining_columns <- seq_len(ncol(correlations))
  for (unused in seq_len(min(nrow(correlations), ncol(correlations)))) {
    available <- abs(correlations[remaining_rows, remaining_columns, drop = FALSE])
    selected <- which(available == max(available), arr.ind = TRUE)[1L, ]
    row <- remaining_rows[[selected[[1L]]]]
    column <- remaining_columns[[selected[[2L]]]]
    value <- correlations[row, column]
    permutation[[row]] <- column
    signs[[row]] <- if (value < 0) -1 else 1
    aligned_correlations[[row]] <- abs(value)
    remaining_rows <- setdiff(remaining_rows, row)
    remaining_columns <- setdiff(remaining_columns, column)
  }
  matched <- which(!is.na(permutation))
  scores <- sweep(
    candidate[, permutation[matched], drop = FALSE], 2L, signs[matched], "*"
  )
  colnames(scores) <- colnames(reference)[matched]
  rownames(scores) <- rownames(reference)
  names(permutation) <- colnames(reference)
  names(signs) <- colnames(reference)
  names(aligned_correlations) <- colnames(reference)
  list(
    scores = scores, permutation = permutation, signs = signs,
    aligned_correlations = aligned_correlations[matched],
    matched_reference_factors = colnames(reference)[matched],
    unmatched_reference_factors = colnames(reference)[is.na(permutation)],
    unmatched_candidate_factors = colnames(candidate)[setdiff(seq_len(ncol(candidate)), permutation[matched])]
  )
}

triomics_mofa_subspace_similarity <- function(left, right) {
  if (!is.matrix(left) || !is.numeric(left) || any(!is.finite(left)) ||
      !is.matrix(right) || !is.numeric(right) || any(!is.finite(right)) ||
      nrow(left) != nrow(right) || is.null(rownames(left)) || is.null(rownames(right)) ||
      !identical(rownames(left), rownames(right))) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "Seed factor subspaces must share a finite keyed sample map.",
      key = "seed_sensitivity"
    )
  }
  orthonormal_basis <- function(value) {
    value <- scale(value, center = TRUE, scale = FALSE)
    decomposition <- qr(value, tol = 1e-10)
    rank <- decomposition$rank
    if (rank < 1L) return(list(basis = matrix(numeric(), nrow(value), 0L), rank = 0L))
    list(basis = qr.Q(decomposition, complete = FALSE)[, seq_len(rank), drop = FALSE], rank = rank)
  }
  left_basis <- orthonormal_basis(left)
  right_basis <- orthonormal_basis(right)
  if (left_basis$rank < 1L || right_basis$rank < 1L) {
    return(list(similarity = NA_real_, left_rank = left_basis$rank, right_rank = right_basis$rank))
  }
  overlap <- crossprod(left_basis$basis, right_basis$basis)
  similarity <- sum(overlap^2) / sqrt(left_basis$rank * right_basis$rank)
  if (!is.finite(similarity) || similarity < -1e-12 || similarity > 1 + 1e-12) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "MOFA2 subspace similarity is outside its mathematical range.",
      key = "seed_sensitivity"
    )
  }
  similarity <- min(1, max(0, similarity))
  list(similarity = similarity, left_rank = left_basis$rank, right_rank = right_basis$rank)
}

triomics_mofa_seed_sensitivity <- function(runs, control_regime = "unspecified") {
  if (!is.list(runs) || !length(runs) || !is.character(control_regime) ||
      length(control_regime) != 1L || is.na(control_regime) || !nzchar(control_regime)) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "Seed sensitivity requires non-empty run records and a control label.",
      key = "seed_sensitivity"
    )
  }
  complete <- which(vapply(runs, function(run) identical(run$status, "complete"), logical(1L)))
  failures <- which(vapply(runs, function(run) identical(run$status, "software_failure"), logical(1L)))
  comparisons <- list()
  if (length(complete) > 1L) {
    reference_index <- complete[[1L]]
    for (index in complete[-1L]) {
      reference <- runs[[reference_index]]$outputs$factors
      candidate <- runs[[index]]$outputs$factors
      aligned <- triomics_mofa_align_seed_factors(reference, candidate)
      subspace <- triomics_mofa_subspace_similarity(reference, candidate)
      comparisons[[length(comparisons) + 1L]] <- list(
        reference_seed = runs[[reference_index]]$seed,
        seed = runs[[index]]$seed,
        fit_status = "complete",
        alignment = list(
          permutation = aligned$permutation,
          signs = aligned$signs,
          matched_reference_factors = aligned$matched_reference_factors,
          unmatched_reference_factors = aligned$unmatched_reference_factors,
          unmatched_candidate_factors = aligned$unmatched_candidate_factors
        ),
        aligned_correlations = aligned$aligned_correlations,
        subspace_similarity = subspace$similarity,
        subspace_ranks = c(reference = subspace$left_rank, candidate = subspace$right_rank)
      )
    }
  }
  correlations <- unlist(lapply(comparisons, `[[`, "aligned_correlations"), use.names = FALSE)
  subspace_values <- vapply(comparisons, `[[`, numeric(1L), "subspace_similarity")
  finite_subspace_values <- subspace_values[is.finite(subspace_values)]
  run_summaries <- lapply(runs, function(run) {
    list(
      seed = run$seed,
      status = run$status,
      error_code = run$error_code %||% NULL,
      error_message = run$error_message %||% NULL
    )
  })
  structure(
    list(
      requested_repeats = length(runs),
      seeds = vapply(runs, `[[`, integer(1L), "seed"),
      status = if (length(failures)) "software_failure" else if (length(comparisons)) "assessed" else "not_assessed_single_seed",
      software_status = if (length(failures)) "partial_failure" else "complete",
      stability_status = if (!length(comparisons)) "not_estimable" else if (all(is.finite(subspace_values))) {
        "quantified"
      } else {
        "partially_quantified"
      },
      control_regime = control_regime,
      runs = run_summaries,
      comparisons = comparisons,
      uncertainty = list(
        aligned_correlation_min = if (length(correlations)) min(correlations) else NULL,
        aligned_correlation_max = if (length(correlations)) max(correlations) else NULL,
        subspace_similarity_min = if (length(finite_subspace_values)) min(finite_subspace_values) else NULL,
        subspace_similarity_max = if (length(finite_subspace_values)) max(finite_subspace_values) else NULL
      ),
      recovery_threshold = NULL,
      factor_naming = "native_generic_ids_only",
      interpretation = "descriptive_stability_uncertainty_no_forced_recovery"
    ),
    class = "MofaSeedSensitivity"
  )
}

triomics_mofa_hash_object <- function(value) {
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

triomics_mofa_hash_training_inputs <- function(data, missingness_mask) {
  list(
    transformed_data_sha256 = triomics_mofa_hash_object(data),
    missingness_mask_sha256 = triomics_mofa_hash_object(missingness_mask),
    transformed_data_and_masks_sha256 = triomics_mofa_hash_object(list(
      transformed_data = data,
      missingness_mask = missingness_mask
    ))
  )
}

triomics_mofa_hash_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path) ||
      is.na(file.info(path)$size) || file.info(path)$size <= 0) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "The saved native MOFA2 model is missing or empty.",
      key = "model_path"
    )
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

triomics_mofa_mapping_edge_columns <- function() {
  c(
    "block_id", "feature_key", "target_namespace", "target_entity_id",
    "relation", "resolution", "taxon", "context", "resource_release",
    "evidence", "ambiguity_group"
  )
}

triomics_mofa_mapping_invalid <- function(message, key = "annotation_mappings") {
  triomics_abort(
    "D_MAPPING_INVALID", message, key = key, module = "annotation", stage = "D",
    remediation = "Provide a typed MappingEdge collection keyed to fitted features, or omit mappings explicitly."
  )
}

triomics_mofa_canonical_mapping_edges <- function(mappings, state) {
  columns <- triomics_mofa_mapping_edge_columns()
  empty <- as.data.frame(
    stats::setNames(rep(list(character()), length(columns)), columns),
    stringsAsFactors = FALSE
  )
  if (is.null(mappings) || (is.list(mappings) && length(mappings) == 0L) ||
      (is.data.frame(mappings) && nrow(mappings) == 0L)) {
    return(empty)
  }

  if (is.data.frame(mappings)) {
    if (anyDuplicated(names(mappings)) || !setequal(names(mappings), columns)) {
      triomics_mofa_mapping_invalid("MappingEdge tables must contain exactly the supported fields.")
    }
    edges <- mappings[, columns, drop = FALSE]
  } else if (is.list(mappings)) {
    rows <- lapply(mappings, function(edge) {
      if (!is.list(edge) || is.null(names(edge)) || anyDuplicated(names(edge)) ||
          !setequal(names(edge), columns)) {
        triomics_mofa_mapping_invalid("Each MappingEdge must be a named record with exactly the supported fields.")
      }
      valid <- vapply(columns, function(field) {
        value <- edge[[field]]
        is.character(value) && length(value) == 1L &&
          (is.na(value) && field %in% c("taxon", "context", "ambiguity_group") ||
             (!is.na(value) && nzchar(trimws(value))))
      }, logical(1L))
      if (!all(valid)) {
        triomics_mofa_mapping_invalid("MappingEdge fields must be scalar character values; required values cannot be blank or missing.")
      }
      edge[columns]
    })
    values <- lapply(columns, function(field) {
      vapply(rows, function(edge) edge[[field]], character(1L), USE.NAMES = FALSE)
    })
    edges <- as.data.frame(stats::setNames(values, columns), stringsAsFactors = FALSE)
  } else {
    triomics_mofa_mapping_invalid("MappingEdge collections must be data frames or lists of edge records.")
  }

  nullable <- c("taxon", "context", "ambiguity_group")
  valid_values <- vapply(columns, function(field) {
    value <- edges[[field]]
    is.character(value) && length(value) == nrow(edges) &&
      !any(!is.na(value) & !nzchar(trimws(value))) &&
      (field %in% nullable || !anyNA(value))
  }, logical(1L))
  if (!all(valid_values)) {
    triomics_mofa_mapping_invalid("MappingEdge fields must be character values; required values cannot be blank or missing.")
  }

  identity_columns <- setdiff(columns, "evidence")
  if (anyDuplicated(edges[, identity_columns, drop = FALSE])) {
    triomics_mofa_mapping_invalid("MappingEdge identities must be unique.", key = "mapping_edge_identity")
  }

  weights <- state$weights
  block_ids <- state$block_ids
  if (!is.list(weights) || is.null(names(weights)) || anyDuplicated(names(weights)) ||
      !is.character(block_ids) || !length(block_ids) || anyNA(block_ids) ||
      any(!nzchar(block_ids)) || anyDuplicated(block_ids) ||
      !setequal(names(weights), block_ids)) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "The fitted MOFA2 feature map is invalid.", key = "weights"
    )
  }
  fitted_features <- do.call(rbind, lapply(block_ids, function(block_id) {
    feature_keys <- rownames(weights[[block_id]])
    if (!is.character(feature_keys) || !length(feature_keys) || anyNA(feature_keys) ||
        any(!nzchar(feature_keys)) || anyDuplicated(feature_keys)) {
      triomics_mofa_abort(
        "C_SCORE_RESULT_INVALID", "The fitted MOFA2 feature map is invalid.", key = "weights"
      )
    }
    data.frame(block_id = block_id, feature_key = feature_keys, stringsAsFactors = FALSE)
  }))
  rownames(fitted_features) <- NULL
  feature_matches <- merge(
    data.frame(
      edge_index = seq_len(nrow(edges)), block_id = edges$block_id,
      feature_key = edges$feature_key, stringsAsFactors = FALSE
    ),
    fitted_features, by = c("block_id", "feature_key"), sort = FALSE
  )
  if (nrow(feature_matches) != nrow(edges)) {
    triomics_mofa_mapping_invalid(
      "Every MappingEdge measured feature must belong to the fitted MOFA2 feature map.",
      key = "feature_key"
    )
  }

  order_index <- do.call(
    order,
    c(unname(as.list(edges[, columns, drop = FALSE])), list(na.last = TRUE, method = "radix"))
  )
  edges <- edges[order_index, columns, drop = FALSE]
  rownames(edges) <- NULL
  edges
}

triomics_mofa_fit_once <- function(eligibility, params, model_path, reload = FALSE) {
  model <- MOFA2::create_mofa(eligibility$data)
  options <- triomics_mofa_prepare_options(model, eligibility, params)
  model <- MOFA2::prepare_mofa(
    object = model, data_options = options$data,
    model_options = options$model, training_options = options$training
  )
  fitted <- MOFA2::run_mofa(model, outfile = model_path, use_basilisk = FALSE)
  if (is.null(fitted)) fitted <- model
  native_model_sha256 <- triomics_mofa_hash_file(model_path)
  fitted_outputs <- triomics_mofa_native_outputs(fitted, eligibility)
  reloaded <- NULL
  agreement <- c(factors = NA, weights = NA, variance = NA, native_variance_percentage = NA)
  if (isTRUE(reload)) {
    reloaded <- MOFA2::load_model(
      model_path, sort_factors = FALSE, remove_inactive_factors = TRUE
    )
    reloaded_outputs <- triomics_mofa_native_outputs(reloaded, eligibility)
    reloaded_outputs <- triomics_mofa_align_reloaded_outputs(fitted_outputs, reloaded_outputs)
    agreement <- triomics_mofa_outputs_agree(fitted_outputs, reloaded_outputs)
    if (!all(agreement)) {
      stop(
        paste0("MOFA2 save/reload output disagreement: ", paste(names(agreement)[!agreement], collapse = ", ")),
        call. = FALSE
      )
    }
  }
  list(
    model = fitted, reloaded_model = reloaded, outputs = fitted_outputs,
    agreement = agreement, options = options,
    model_path = model_path, native_model_sha256 = native_model_sha256
  )
}

triomics_mofa_enrichment_input_sidecar <- function(state, context = list(), params = list()) {
  model_sha256 <- triomics_mofa_hash_file(state$model_path)
  if (!identical(model_sha256, state$native_model_sha256)) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "The saved MOFA2 model bytes changed after the fitted state was created.",
      key = "model_sha256"
    )
  }
  if (!is.list(context) || !is.list(params)) {
    triomics_mofa_mapping_invalid("MappingEdge inputs must be supplied through named context or parameter lists.")
  }
  mappings <- context$annotation_mappings %||% context$annotation_mapping %||%
    params$annotation_mappings %||% params$annotation_mapping %||% list()
  mappings <- triomics_mofa_canonical_mapping_edges(mappings, state)
  mapping_count <- nrow(mappings)
  mapping_sha256 <- triomics_mofa_hash_object(mappings)
  training_hashes <- state$training_input_hashes
  if (!is.list(training_hashes) ||
      !all(vapply(training_hashes, triomics_sha256_text, logical(1L)))) {
    triomics_mofa_abort(
      "C_SCORE_RESULT_INVALID", "MOFA2 transformed training data and missingness masks lack valid hashes.",
      key = "training_input_hashes"
    )
  }
  hashes <- list(
    native_model_sha256 = model_sha256,
    transformed_data_sha256 = training_hashes$transformed_data_sha256,
    missingness_mask_sha256 = training_hashes$missingness_mask_sha256,
    transformed_data_and_masks_sha256 = training_hashes$transformed_data_and_masks_sha256,
    annotation_mappings_sha256 = mapping_sha256
  )
  artifact_id <- paste0(
    "mofa2-enrichment-inputs-",
    substr(triomics_mofa_hash_object(hashes), 1L, 20L)
  )
  structure(
    list(
      schema_version = "2.0",
      artifact_type = "mofa2_enrichment_input_handoff",
      artifact_id = artifact_id,
      status = if (mapping_count > 0L) "ready_for_downstream_handoff" else "annotation_mapping_not_supplied",
      adapter_version = "C-05/2.0.0-rc1",
      native_method = "MOFA2",
      native_version = state$native_version,
      python_backend = "mofapy2",
      python_backend_version = state$python_backend_version,
      training_state_hash = state$training_state_hash,
      factor_ids = state$components,
      view_ids = state$block_ids,
      factors = state$factors,
      weights = state$weights,
      variance = state$variance,
      variance_semantics = state$variance_semantics,
      annotation_mappings = mappings,
      annotation_mapping_status = if (mapping_count > 0L) "supplied" else "not_supplied",
      inputs = list(
        native_model = list(artifact_type = "mofa2_native_model", sha256 = model_sha256),
        transformed_data_and_masks = list(
          artifact_type = "mofa2_transformed_training_data_and_missingness_masks",
          sha256 = training_hashes$transformed_data_and_masks_sha256
        ),
        annotation_mappings = list(
          artifact_type = "annotation_mapping_collection",
          sha256 = mapping_sha256,
          status = if (mapping_count > 0L) "supplied" else "not_supplied"
        )
      ),
      hashes = hashes,
      enrichment_execution = "not_performed",
      prediction_claim = "none; exploratory structure discovery only"
    ),
    class = "MofaEnrichmentInputSidecar"
  )
}

triomics_mofa_fit <- function(resolved_analysis, params, status = triomics_mofa_backend_status()) {
  if (!isTRUE(status$available)) {
    return(triomics_mofa_unavailable_state(status))
  }

  # The caller-supplied probe is only a gate. Re-check both native namespaces
  # before touching data so an injected/lying status object cannot train.
  real <- triomics_mofa_real_runtime_status()
  if (!isTRUE(real$MOFA2$loaded) || !isTRUE(real$mofapy2$loaded)) {
    triomics_mofa_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "MOFA2 runtime status was available but the native runtime was not actually loadable; no factors, weights or variance were produced.",
      key = "integration.method"
    )
  }

  eligibility <- triomics_mofa_validate(resolved_analysis, params)
  triomics_mofa_restore_python()
  options <- NULL
  model_path <- params$model_path %||% params$save_path
  if (is.null(model_path)) model_path <- tempfile("triomics-mofa-", fileext = ".hdf5")
  if (!is.character(model_path) || length(model_path) != 1L || is.na(model_path) || !nzchar(model_path)) {
    triomics_mofa_abort("C_SCORE_PARAMETER", "MOFA2 model_path must be one non-blank path.", key = "model_path")
  }
  model_path <- normalizePath(model_path, winslash = "/", mustWork = FALSE)
  if (dir.exists(dirname(model_path)) == FALSE) {
    triomics_mofa_abort("C_SCORE_PARAMETER", "MOFA2 model_path parent directory does not exist.", key = "model_path")
  }

  seed_values <- as.double(eligibility$seed) + seq_len(eligibility$seed_repeats) - 1
  if (any(!is.finite(seed_values) | seed_values > .Machine$integer.max |
          seed_values < -.Machine$integer.max)) {
    triomics_mofa_abort(
      "C_SCORE_PARAMETER", "MOFA2 repeated seeds exceed the supported integer range.",
      key = "mofa_seed_repeats"
    )
  }
  seed_values <- as.integer(seed_values)
  fit_started <- proc.time()[["elapsed"]]
  primary <- tryCatch(
    triomics_mofa_fit_once(eligibility, params, model_path, reload = TRUE),
    error = function(error) {
    if (inherits(error, "triomics_error")) stop(error)
    triomics_mofa_abort(
      "C_EVAL_NATIVE_FIT",
      paste0("MOFA2 training, save or reload failed: ", conditionMessage(error)),
      key = "mofa2"
    )
    }
  )

  seed_runs <- list(list(
    seed = seed_values[[1L]], status = "complete", outputs = primary$outputs
  ))
  if (length(seed_values) > 1L) {
    for (seed in seed_values[-1L]) {
      repeat_path <- tempfile(
        pattern = paste0("triomics-mofa-seed-", seed, "-"),
        tmpdir = dirname(model_path), fileext = ".hdf5"
      )
      repeat_eligibility <- eligibility
      repeat_eligibility$seed <- seed
      repeat_params <- params
      repeat_params$seed <- seed
      repeat_run <- tryCatch({
        value <- triomics_mofa_fit_once(
          repeat_eligibility, repeat_params, repeat_path, reload = FALSE
        )
        unlink(repeat_path)
        list(seed = seed, status = "complete", outputs = value$outputs)
      }, error = function(error) {
        unlink(repeat_path)
        list(
          seed = seed, status = "software_failure",
          error_code = if (inherits(error, "triomics_error")) error$code else "C_EVAL_NATIVE_FIT",
          error_message = conditionMessage(error)
        )
      })
      seed_runs[[length(seed_runs) + 1L]] <- repeat_run
    }
  }
  seed_sensitivity <- triomics_mofa_seed_sensitivity(
    seed_runs, control_regime = params$mofa_control_regime %||% "unspecified"
  )
  fit_runtime_seconds <- max(0, proc.time()[["elapsed"]] - fit_started)
  seed_sensitivity$fit_count <- length(seed_values)
  seed_sensitivity$runtime_seconds <- fit_runtime_seconds

  trained <- primary
  options <- trained$options
  outputs <- trained$outputs
  components <- colnames(outputs$factors)
  native_version <- as.character(utils::packageVersion("MOFA2"))
  python_backend_version <- real$mofapy2$version
  training_input_hashes <- triomics_mofa_hash_training_inputs(
    eligibility$data, eligibility$missingness_mask
  )
  state_hash <- digest::digest(
    list(
      method = "mofa2", native_version = native_version,
      python_backend_version = python_backend_version,
      training_unit_ids = eligibility$units, block_ids = eligibility$block_ids,
      components = components, transforms = eligibility$transforms,
      training_input_hashes = training_input_hashes,
      native_model_sha256 = trained$native_model_sha256,
      factors = outputs$factors, weights = outputs$weights,
      variance = outputs$variance,
      seed_sensitivity = list(
        seeds = seed_sensitivity$seeds,
        status = seed_sensitivity$status,
        comparisons = seed_sensitivity$comparisons
      )
    ), algo = "sha256", serialize = TRUE
  )
  result_params <- list(
    native_method = "MOFA2::run_mofa",
    native_version = native_version,
    python_backend = "mofapy2",
    python_backend_version = python_backend_version,
    likelihoods = stats::setNames(rep("gaussian", length(eligibility$block_ids)), eligibility$block_ids),
    requested_n_components = eligibility$n_components,
    retained_n_components = length(components),
    removed_factors = setdiff(paste0("Factor", seq_len(eligibility$n_components)), components),
    convergence_mode = eligibility$convergence,
    seed = eligibility$seed,
    training_options = options$training,
    data_options = options$data,
    model_options = options$model,
    feature_transforms = eligibility$transforms,
    missingness = "native_MOFA_likelihood_preserved; no imputation evidence",
    variance_units = outputs$variance_units,
    variance = outputs$variance_semantics,
    training_input_hashes = training_input_hashes,
    native_model_sha256 = trained$native_model_sha256,
    mofa_seed_repeats = eligibility$seed_repeats,
    seed_sensitivity = seed_sensitivity,
    fit_count = seed_sensitivity$fit_count,
    fit_runtime_seconds = fit_runtime_seconds,
    groups = list(
      model_structure = "single_group",
      native_group_ids = options$data$groups,
      labels_not_from_outcome = TRUE
    ),
    model_path = model_path,
    save_reload = list(saved = TRUE, reloaded = TRUE, agreement = trained$agreement),
    training_unit_ids = eligibility$units, training_state_hash = state_hash,
    score_definition = "native_mofa_factor_values",
    exploratory_only = TRUE
  )
  structure(
    list(
      trained = TRUE, native_state = list(
        model = trained$model, reloaded_model = trained$reloaded_model,
        block_ids = eligibility$block_ids, training_unit_ids = eligibility$units,
        components = components, factors = outputs$factors, weights = outputs$weights,
        variance = outputs$variance,
        native_variance_percentage = outputs$native_variance_percentage,
        variance_semantics = outputs$variance_semantics,
        reload_agreement = trained$agreement,
        model_path = trained$model_path,
        native_model_sha256 = trained$native_model_sha256,
        native_version = native_version,
        python_backend_version = python_backend_version,
        training_state_hash = state_hash,
        training_input_hashes = training_input_hashes,
        missingness_mask = eligibility$missingness_mask,
        seed_sensitivity = seed_sensitivity,
        training_transform = eligibility$transforms, prediction_state = list(uses_outcome = FALSE)
      ),
      selected = stats::setNames(lapply(eligibility$data, rownames), eligibility$block_ids),
      result_params = result_params,
      metadata = list(
        backend = "mofa2", native_method = "MOFA2::run_mofa",
        native_version = result_params$native_version,
        python_backend_version = python_backend_version,
        model_role = "exploratory_structure_discovery",
        training_unit_ids = eligibility$units, state_hash = state_hash,
        save_reload = result_params$save_reload,
        training_input_hashes = training_input_hashes,
        native_model_sha256 = trained$native_model_sha256
      )
    ),
    class = "BackendFit"
  )
}

triomics_mofa_extract <- function(backend_fit, context, params) {
  if (!inherits(backend_fit, "BackendFit") || !isTRUE(backend_fit$trained) ||
      !is.list(backend_fit$native_state)) {
    triomics_mofa_abort("C_EVAL_NATIVE_FIT", "Extraction requires a successfully trained MOFA2 BackendFit.", key = "backend_fit")
  }
  state <- backend_fit$native_state
  enrichment_inputs <- triomics_mofa_enrichment_input_sidecar(state, context, params)
  result <- triomics_new_integration_result(
    scores = state$factors, loadings = state$weights, variance = state$variance,
    selected = backend_fit$selected, backend = "mofa2", params = backend_fit$result_params
  )
  list(
    result = triomics_validate_integration_result(result),
    sidecars = list(
      native_state = state,
      metadata = backend_fit$metadata,
      context = context,
      params = params,
      enrichment_inputs = enrichment_inputs
    )
  )
}
