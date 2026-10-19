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
    scale_features = isTRUE(scale_features), n_components = triomics_mofa_n_components(params),
    seed = triomics_mofa_seed(params), convergence = triomics_mofa_convergence(params),
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
  if (!is.matrix(per_factor) || is.null(rownames(per_factor)) || is.null(colnames(per_factor)) ||
      !setequal(rownames(per_factor), colnames(factors)) ||
      !setequal(colnames(per_factor), eligibility$block_ids)) {
    triomics_mofa_abort("C_SCORE_VARIANCE_UNAVAILABLE", "MOFA2 per-factor variance has an unexpected map.", key = "variance")
  }
  variance <- t(per_factor[colnames(factors), eligibility$block_ids, drop = FALSE])
  colnames(variance) <- colnames(factors)
  rownames(variance) <- eligibility$block_ids
  variance_units <- "proportion"
  if (all(is.finite(variance)) && max(variance) > 1 && max(variance) <= 100) {
    variance <- variance / 100
    variance_units <- "proportion_from_native_percentage"
  }
  if (any(!is.finite(variance)) || any(variance < 0 | variance > 1)) {
    triomics_mofa_abort("C_SCORE_VARIANCE_UNAVAILABLE", "MOFA2 variance is not a finite proportion.", key = "variance")
  }
  list(factors = factors, weights = weights, variance = variance, variance_units = variance_units)
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
    variance = same_matrix(fitted$variance, reloaded$variance)
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
  aligned
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

  trained <- tryCatch({
    model <- MOFA2::create_mofa(eligibility$data)
    options <- triomics_mofa_prepare_options(model, eligibility, params)
    model <- MOFA2::prepare_mofa(
      object = model, data_options = options$data,
      model_options = options$model, training_options = options$training
    )
    fitted <- MOFA2::run_mofa(model, outfile = model_path, use_basilisk = FALSE)
    if (is.null(fitted)) fitted <- model
    # run_mofa writes the native HDF5 model to outfile as part of the fit.
    # MOFA2 has no separate R-side save_model API in the supported interface.
    if (!file.exists(model_path) || file.info(model_path)$size <= 0) {
      stop("MOFA2 did not create a non-empty saved model.", call. = FALSE)
    }
    reloaded <- MOFA2::load_model(
      model_path, sort_factors = FALSE, remove_inactive_factors = TRUE
    )
    fitted_outputs <- triomics_mofa_native_outputs(fitted, eligibility)
    reloaded_outputs <- triomics_mofa_native_outputs(reloaded, eligibility)
    reloaded_outputs <- triomics_mofa_align_reloaded_outputs(fitted_outputs, reloaded_outputs)
    agreement <- triomics_mofa_outputs_agree(fitted_outputs, reloaded_outputs)
    if (!all(agreement)) {
      stop(
        paste0("MOFA2 save/reload output disagreement: ", paste(names(agreement)[!agreement], collapse = ", ")),
        call. = FALSE
      )
    }
    list(
      model = fitted, reloaded_model = reloaded, outputs = fitted_outputs,
      agreement = agreement
    )
  }, error = function(error) {
    if (inherits(error, "triomics_error")) stop(error)
    triomics_mofa_abort(
      "C_EVAL_NATIVE_FIT",
      paste0("MOFA2 training, save or reload failed: ", conditionMessage(error)),
      key = "mofa2"
    )
  })

  outputs <- trained$outputs
  components <- colnames(outputs$factors)
  state_hash <- digest::digest(
    list(
      method = "mofa2", native_version = as.character(utils::packageVersion("MOFA2")),
      training_unit_ids = eligibility$units, block_ids = eligibility$block_ids,
      components = components, transforms = eligibility$transforms,
      factors = outputs$factors, weights = outputs$weights, variance = outputs$variance
    ), algo = "sha256", serialize = TRUE
  )
  result_params <- list(
    native_method = "MOFA2::run_mofa",
    native_version = as.character(utils::packageVersion("MOFA2")),
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
    variance_units = "proportion",
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
        variance = outputs$variance, reload_agreement = trained$agreement,
        training_transform = eligibility$transforms, prediction_state = list(uses_outcome = FALSE)
      ),
      selected = stats::setNames(lapply(eligibility$data, rownames), eligibility$block_ids),
      result_params = result_params,
      metadata = list(
        backend = "mofa2", native_method = "MOFA2::run_mofa",
        native_version = result_params$native_version, model_role = "exploratory_structure_discovery",
        training_unit_ids = eligibility$units, state_hash = state_hash,
        save_reload = result_params$save_reload
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
  result <- triomics_new_integration_result(
    scores = state$factors, loadings = state$weights, variance = state$variance,
    selected = backend_fit$selected, backend = "mofa2", params = backend_fit$result_params
  )
  list(
    result = triomics_validate_integration_result(result),
    sidecars = list(native_state = state, metadata = backend_fit$metadata, context = context, params = params)
  )
}
