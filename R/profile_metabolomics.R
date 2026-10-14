# Metabolomic profile. PQN quotients are positive linear ratios against a frozen
# reference; log2, when requested, runs after PQN even though the shared stage
# list names transform before normalisation. kNN and RF remain conditional method
# names but are unavailable in v2 until an authoritative qualification route exists.

triomics_metabolomics_supported_methods <- function() {
  c("identity", "none", "prevalence", "log2", "pqn", "training_median")
}

triomics_metabolomics_imputation_description <- function(method) {
  if (identical(method, "training_median")) {
    return(list(
      method = "training_median", role = "explicit_baseline",
      biological_default = FALSE, mechanism = "unspecified"
    ))
  }
  if (is.null(method) || identical(method, "none")) {
    return(list(
      method = "none", role = "not_applied",
      biological_default = FALSE, mechanism = "unspecified"
    ))
  }
  if (is.character(method) && length(method) == 1L && method %in% c("knn", "rf")) {
    return(list(
      method = method, role = "conditional_unavailable",
      biological_default = FALSE, mechanism = "mar"
    ))
  }
  list(
    method = if (is.character(method) && length(method) == 1L) method else "unknown",
    role = "unsupported", biological_default = FALSE, mechanism = "unspecified"
  )
}

triomics_metabolomics_assumption <- function(operation) {
  assumption <- if (is.list(operation)) operation$missingness_assumption else NULL
  if (is.null(assumption) || length(assumption) != 1L || is.na(assumption) || !nzchar(as.character(assumption))) {
    "unknown"
  } else {
    as.character(assumption)
  }
}

triomics_metabolomics_lane_decision <- function(operation) {
  method <- if (is.list(operation)) operation$method else NULL
  if (is.null(method) || !is.character(method) || length(method) != 1L || is.na(method)) {
    return(list(status = "unavailable", reason = "method_not_registered"))
  }
  if (method %in% c("knn", "rf")) {
    return(list(status = "unavailable", reason = "qualification_unavailable_in_v2"))
  }
  if (method %in% triomics_metabolomics_supported_methods()) {
    return(list(status = "fit_apply", reason = "supported"))
  }
  if (method %in% c(
    "median_shift", "vsn", "minprob", "qrilc", "clr", "sample_amount",
    "internal_standard", "class_sum", "weighted_carbons", "weighted_double_bonds",
    "molar_fraction", "saturated_unsaturated_ratio"
  )) {
    return(list(status = "unavailable", reason = "not_metabolomic_policy"))
  }
  list(status = "unavailable", reason = "method_not_registered")
}

triomics_metabolomics_predictive_eligibility <- function(operation) {
  triomics_metabolomics_lane_decision(operation)$status
}

triomics_metabolomics_refuse_operation <- function(operation, decision = NULL) {
  if (is.null(decision)) decision <- triomics_metabolomics_lane_decision(operation)
  method <- if (is.list(operation) && is.character(operation$method)) operation$method else "unknown"
  if (identical(decision$reason, "unknown_missingness_is_not_mar")) {
    message <- "Unknown missingness does not select a MAR method (unknown_missingness_is_not_mar)."
  } else if (identical(decision$reason, "missingness_does_not_select_mar")) {
    message <- paste0(
      "Missingness assumption '", triomics_metabolomics_assumption(operation),
      "' does not select a MAR method (missingness_does_not_select_mar)."
    )
  } else {
    message <- paste0("Metabolomic lane '", method, "' is unavailable (", decision$reason, ").")
  }
  triomics_abort(
    "A_CONFIG_METHOD", message, key = method, module = "profile_metabolomics", stage = "A",
    remediation = "Choose an explicit metabolomic method. Do not treat unknown missingness as MAR or fall back to another imputer."
  )
}

triomics_metabolomics_operations <- function(filter_method = "none", transform = "none",
                                             normalise = "none", impute = "none",
                                             min_observed = NULL, prevalence = NULL,
                                             zero_policy = "error", zero_value = NULL,
                                             reference = NULL, reference_file = NULL,
                                             scope = "global", stratum = NULL,
                                             selection_policy = NULL,
                                             selection_justification = NULL,
                                             missingness_assumption = "unknown",
                                             missingness_evidence = NULL,
                                             qualification = NULL, k = NULL, trees = NULL,
                                             derive = "none") {
  defer <- identical(transform, "log2") && identical(normalise, "pqn")
  ops <- list(
    list(
      stage = "validate", method = "identity",
      missingness_assumption = missingness_assumption,
      missingness_evidence = missingness_evidence
    ),
    list(
      stage = "filter", method = filter_method, min_observed = min_observed,
      prevalence = prevalence, scope = scope, stratum = stratum,
      selection_policy = selection_policy, selection_justification = selection_justification
    ),
    list(stage = "derive", method = derive),
    list(
      stage = "transform", method = transform, zero_policy = zero_policy,
      zero_value = zero_value, defer_until_pqn = defer
    ),
    list(
      stage = "normalise", method = normalise, reference = reference,
      reference_file = reference_file, then_log2 = defer,
      zero_policy = zero_policy, zero_value = zero_value
    ),
    list(
      stage = "impute", method = impute, qualification = qualification,
      missingness_assumption = missingness_assumption,
      missingness_evidence = missingness_evidence, k = k, trees = trees
    )
  )
  eligibility <- vapply(ops, triomics_metabolomics_predictive_eligibility, character(1))
  if (any(eligibility == "unavailable")) {
    ops <- lapply(ops, function(op) {
      op$apply <- "unavailable"
      op
    })
  }
  ops
}

triomics_metabolomics_number <- function(value) {
  if (is.null(value) || length(value) == 0L) return(NA_real_)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (!length(value) || all(is.na(value))) return(NA_real_)
  as.numeric(value[[1L]])
}

triomics_metabolomics_hash_json <- function(value) {
  triomics_hash_text(triomics_canonical_json(value))
}

triomics_metabolomics_as_rows <- function(value) {
  if (is.null(value) || (is.list(value) && !length(value))) return(list())
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(i) as.list(value[i, , drop = FALSE])))
  }
  if (is.list(value) && !is.null(names(value))) {
    atomic <- !any(vapply(value, is.list, logical(1)))
    lens <- vapply(value, length, integer(1))
    if (atomic && length(lens) && all(lens == lens[[1L]]) && lens[[1L]] > 1L) {
      fields <- names(value)
      return(lapply(seq_len(lens[[1L]]), function(i) {
        row <- lapply(fields, function(field) value[[field]][[i]])
        names(row) <- fields
        row
      }))
    }
    if (!is.null(value$feature_key) || !is.null(value$sample_id) || !is.null(value$leaf)) {
      return(list(value))
    }
  }
  if (is.list(value)) {
    return(lapply(value, function(row) if (is.list(row)) row else as.list(row)))
  }
  list()
}

triomics_metabolomics_positive_count <- function(value, label) {
  if (is.null(value) || length(value) != 1L || is.na(value) || !is.numeric(value) ||
      !is.finite(value) || value < 1 || value != trunc(value)) {
    triomics_abort(
      "A_CONFIG_METHOD", paste0(label, " requires a positive integer."),
      key = label, module = "profile_metabolomics", stage = "A",
      remediation = "Set the declared count. There is no hidden neighbor or tree default."
    )
  }
  as.integer(value)
}

triomics_metabolomics_validate <- function(se, params) {
  se <- triomics_merge_learned(se, list(
    missingness_assumption = triomics_metabolomics_assumption(params$operation),
    missingness_rewritten = FALSE
  ))
  triomics_shared_validate(se, params)
}

triomics_metabolomics_loss_row <- function(se, index, params, reason) {
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  training <- params$training_unit_ids
  cols <- if (is.character(training) && length(training) && all(training %in% colnames(se))) {
    training
  } else {
    colnames(se)
  }
  counts <- triomics_count_mask(mask[index, cols], analysis[index, cols])
  data.frame(
    feature_key = rownames(se)[[index]],
    block_id = if (is.null(params$block_id)) NA_character_ else params$block_id,
    stage = "filter",
    retained = FALSE,
    reason = reason,
    n_observed = counts$n_observed,
    n_missing = counts$n_missing,
    n_imputed = counts$n_imputed,
    n_unknown = counts$n_unknown,
    stringsAsFactors = FALSE
  )
}

triomics_metabolomics_drop_declared <- function(se, params) {
  row_data <- SummarizedExperiment::rowData(se)
  if (!"declared_exclusion" %in% names(row_data)) return(se)
  labels <- as.character(row_data$declared_exclusion)
  labels[is.na(labels)] <- ""
  unknown <- !labels %in% c("", "contaminant", "qc_failure")
  if (any(unknown)) {
    triomics_abort(
      "A_CONFIG_OPERATION",
      "declared_exclusion must be contaminant, qc_failure, or empty.",
      key = "declared_exclusion", module = "profile_metabolomics", stage = "A",
      remediation = "Declare contaminant or qc_failure. Do not infer exclusions from feature identifiers."
    )
  }
  drop <- labels %in% c("contaminant", "qc_failure")
  if (!any(drop)) return(se)
  flow <- do.call(rbind, lapply(which(drop), function(index) {
    reason <- if (identical(labels[[index]], "contaminant")) "declared_contaminant" else "declared_qc_failure"
    triomics_metabolomics_loss_row(se, index, params, reason)
  }))
  se <- se[!drop, , drop = FALSE]
  if (!nrow(se)) {
    triomics_abort(
      "A_STATE_PARTIAL", "Declared exclusions removed every feature.",
      key = "declared_exclusion", module = "profile_metabolomics", stage = "A",
      remediation = "Keep at least one measured feature after declared exclusions."
    )
  }
  triomics <- triomics_meta(se)
  triomics$declared_flow <- flow
  triomics_set_triomics(se, triomics)
}

triomics_metabolomics_combine_flow <- function(se) {
  triomics <- triomics_meta(se)
  declared <- triomics$declared_flow
  if (is.null(declared) || !nrow(declared)) return(se)
  shared_flow <- triomics$feature_flow
  triomics$feature_flow <- if (is.data.frame(shared_flow) && nrow(shared_flow)) {
    rbind(declared, shared_flow)
  } else {
    declared
  }
  triomics$declared_flow <- NULL
  triomics_set_triomics(se, triomics)
}

triomics_metabolomics_filter_stratum <- function(se, params) {
  policy <- params$operation$selection_policy
  justification <- params$operation$selection_justification
  stratum <- params$operation$stratum
  if (!is.character(policy) || length(policy) != 1L ||
      !policy %in% c("training_min_in_each_group", "training_min_in_any_group") ||
      !is.character(justification) || length(justification) != 1L ||
      is.na(justification) || !nzchar(justification) ||
      !is.character(stratum) || length(stratum) != 1L || is.na(stratum) || !nzchar(stratum)) {
    triomics_abort(
      "A_CONFIG_OPERATION",
      "A group-anchored filter needs a declared selection policy, justification, and stratum.",
      key = "selection_policy", module = "profile_metabolomics", stage = "A",
      remediation = "Declare the training-label policy. Do not infer groups from sample identifiers."
    )
  }
  col_data <- SummarizedExperiment::colData(se)
  if (!stratum %in% names(col_data)) {
    triomics_abort(
      "A_CONFIG_OPERATION", "The declared stratum column is not in sample metadata.",
      key = "stratum", module = "profile_metabolomics", stage = "A",
      remediation = "Name a colData column. Do not parse groups out of sample ids."
    )
  }
  training <- params$training_unit_ids
  if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
    triomics_abort(
      "A_STATE_PARTIAL", "Group-anchored filtering requires the declared training samples.",
      key = "training_unit_ids", module = "profile_metabolomics", stage = "A",
      remediation = "Count original observations on training units only."
    )
  }
  labels <- as.character(col_data[[stratum]])
  names(labels) <- colnames(se)
  train_labels <- labels[training]
  if (anyNA(train_labels) || any(!nzchar(train_labels))) {
    triomics_abort(
      "A_CONFIG_OPERATION", "Training samples need explicit stratum labels.",
      key = "stratum", module = "profile_metabolomics", stage = "A",
      remediation = "Do not fill missing group labels from held-out samples."
    )
  }
  groups <- unique(train_labels)
  minimum <- params$operation$min_observed
  prevalence <- params$operation$prevalence
  if (is.null(minimum) && is.null(prevalence)) {
    triomics_abort(
      "A_CONFIG_OPERATION", "Prevalence filtering requires a declared minimum.",
      key = "filter", module = "profile_metabolomics", stage = "A",
      remediation = "Set min_observed or prevalence. There is no hidden default."
    )
  }
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  keep <- logical(nrow(se))
  flow <- vector("list", nrow(se))
  for (i in seq_len(nrow(se))) {
    group_ok <- logical(length(groups))
    for (g in seq_along(groups)) {
      cols <- training[train_labels == groups[[g]]]
      counts <- triomics_count_mask(mask[i, cols], analysis[i, cols])
      ok <- TRUE
      if (!is.null(minimum)) ok <- ok && counts$n_observed >= minimum
      if (!is.null(prevalence) && length(cols)) ok <- ok && (counts$n_observed / length(cols)) >= prevalence
      if (!length(cols)) ok <- FALSE
      group_ok[[g]] <- ok
    }
    counts <- triomics_count_mask(mask[i, training], analysis[i, training])
    ok <- if (identical(policy, "training_min_in_each_group")) all(group_ok) else any(group_ok)
    keep[[i]] <- ok
    flow[[i]] <- data.frame(
      feature_key = rownames(se)[[i]],
      block_id = params$block_id,
      stage = "filter",
      retained = ok,
      reason = if (ok) "retained" else "below_min_observed",
      n_observed = counts$n_observed,
      n_missing = counts$n_missing,
      n_imputed = counts$n_imputed,
      n_unknown = counts$n_unknown,
      stringsAsFactors = FALSE
    )
  }
  se <- se[keep, , drop = FALSE]
  triomics <- triomics_meta(se)
  triomics$feature_flow <- do.call(rbind, flow)
  se <- triomics_set_triomics(se, triomics)
  se <- triomics_metabolomics_combine_flow(se)
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_metabolomics_filter <- function(se, params) {
  if (identical(params$mode, "apply")) {
    keys <- as.character(params$fitted_state$feature_keys)
    if (!length(keys) || any(!keys %in% rownames(se))) {
      triomics_abort(
        "A_STATE_FEATURES", "Apply data is missing a feature from the frozen fit universe.",
        key = "feature_key", module = "profile_metabolomics", stage = "A",
        remediation = "Do not reselect features on apply. Supply the fit feature keys."
      )
    }
    se <- se[keys, , drop = FALSE]
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  method <- params$operation$method
  if (!is.character(method) || length(method) != 1L || !method %in% c("none", "prevalence")) {
    triomics_metabolomics_refuse_operation(params$operation)
  }
  se <- triomics_metabolomics_drop_declared(se, params)
  if (identical(method, "none")) {
    se <- triomics_metabolomics_combine_flow(se)
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  scope <- params$operation$scope
  if (is.null(scope) || identical(scope, "global")) {
    se <- triomics_shared_filter(se, params)
    return(triomics_metabolomics_combine_flow(se))
  }
  if (!identical(scope, "stratum")) {
    triomics_abort(
      "A_CONFIG_OPERATION", "Metabolomic prevalence scope must be global or stratum.",
      key = "scope", module = "profile_metabolomics", stage = "A",
      remediation = "Use global or a declared stratum. Do not invent a grouping."
    )
  }
  triomics_metabolomics_filter_stratum(se, params)
}

triomics_metabolomics_transform <- function(se, params) {
  method <- params$operation$method
  if (identical(method, "none")) return(triomics_shared_transform(se, params))
  if (!is.character(method) || length(method) != 1L || !identical(method, "log2")) {
    triomics_metabolomics_refuse_operation(params$operation)
  }
  defer <- isTRUE(params$operation$defer_until_pqn)
  fitted <- params$fitted_state
  if (!defer && is.list(fitted) && isTRUE(fitted$learned_parameters$pqn$then_log2)) defer <- TRUE
  if (!defer) return(triomics_shared_transform(se, params))
  scale <- triomics_meta(se)$scale
  if (identical(scale, "log2")) {
    triomics_abort(
      "A_SCALE_DOUBLE_LOG", "log2 cannot be applied to data that is already on the log2 scale.",
      key = "transform", module = "profile_metabolomics", stage = "A",
      remediation = "Keep the declared input scale. PQN log2 runs once, after the linear quotient."
    )
  }
  if (!identical(scale, "linear")) {
    triomics_abort(
      "A_SCALE_INPUT", "PQN plus log2 requires positive linear input.",
      key = "transform", module = "profile_metabolomics", stage = "A",
      remediation = "Do not log before PQN and do not log an unknown scale."
    )
  }
  se <- triomics_merge_learned(se, list(pending_log2 = list(
    zero_policy = if (is.null(params$operation$zero_policy)) "error" else params$operation$zero_policy,
    zero_value = params$operation$zero_value,
    applied_after = "pqn"
  )))
  se <- triomics_record_operation(se, params$operation, "linear", "linear")
  triomics <- triomics_meta(se)
  last <- length(triomics$completed)
  triomics$completed[[last]]$deferred <- TRUE
  triomics$completed[[last]]$applied_after <- "pqn"
  triomics_set_triomics(se, triomics)
}

triomics_metabolomics_file_set <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
}

triomics_metabolomics_read_reference <- function(path, feature_keys) {
  if (!triomics_metabolomics_file_set(path) || !file.exists(path) || dir.exists(path)) {
    triomics_abort(
      "A_SCALE_REFERENCE", "Supplied PQN reference file is missing.",
      key = "reference_file", module = "profile_metabolomics", stage = "A",
      remediation = "Provide a reference table with feature_key and reference. Do not invent a spectrum."
    )
  }
  table <- tryCatch(
    utils::read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                      check.names = FALSE, comment.char = "", quote = "",
                      colClasses = "character"),
    error = function(error) NULL
  )
  if (is.null(table) || !all(c("feature_key", "reference") %in% names(table))) {
    triomics_abort(
      "A_SCALE_REFERENCE", "PQN reference mismatch: the file needs feature_key and reference columns.",
      key = "reference_file", module = "profile_metabolomics", stage = "A",
      remediation = "Use one row per feature key. Do not align the reference by position."
    )
  }
  keys <- as.character(table$feature_key)
  values <- suppressWarnings(as.numeric(table$reference))
  if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys) || any(!is.finite(values)) ||
      !setequal(keys, feature_keys)) {
    triomics_abort(
      "A_SCALE_REFERENCE", "PQN reference mismatch: feature keys do not match the frozen feature universe.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "Supply one positive reference for every retained feature and no extra keys."
    )
  }
  stats::setNames(values[match(feature_keys, keys)], feature_keys)
}

triomics_metabolomics_reference_from_state <- function(state, feature_keys) {
  learned <- if (is.list(state)) state$learned_parameters$pqn$reference_vector else NULL
  rows <- triomics_metabolomics_as_rows(learned)
  if (!length(rows)) {
    triomics_abort(
      "A_SCALE_REFERENCE", "PQN reference mismatch: the fit state has no reference vector.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "Apply the frozen reference. Do not refit it."
    )
  }
  keys <- vapply(rows, function(row) as.character(row$feature_key)[[1L]], character(1))
  values <- vapply(rows, function(row) triomics_metabolomics_number(row$value), numeric(1))
  if (anyDuplicated(keys) || !setequal(keys, feature_keys) || any(!is.finite(values))) {
    triomics_abort(
      "A_SCALE_REFERENCE", "PQN reference mismatch: stored keys do not match the apply features.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "Do not reestimate or reorder the reference against held-out rows."
    )
  }
  stats::setNames(values[match(feature_keys, keys)], feature_keys)
}

triomics_metabolomics_training_reference <- function(analysis, mask, training) {
  reference <- numeric(nrow(analysis))
  names(reference) <- rownames(analysis)
  for (i in seq_len(nrow(analysis))) {
    observed <- as.character(mask[i, training]) == "known_observed" & is.finite(analysis[i, training])
    if (!any(observed)) {
      triomics_abort(
        "A_SCALE_ALL_MISSING",
        paste0("Feature '", rownames(analysis)[[i]], "' has no training observation for the PQN reference."),
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "Do not invent a reference for an all-missing feature."
      )
    }
    values <- analysis[i, training][observed]
    if (any(values == 0)) {
      triomics_abort(
        "A_SCALE_ZERO", "PQN found a zero in the training reference.",
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "PQN uses positive linear values. Do not replace a zero with a pseudocount."
      )
    }
    if (any(values < 0)) {
      triomics_abort(
        "A_SCALE_NONPOSITIVE", "PQN found a negative training value.",
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "Remove negative measurements before PQN."
      )
    }
    reference[[i]] <- stats::median(values)
  }
  reference
}

triomics_metabolomics_check_reference_signs <- function(reference) {
  if (any(!is.finite(reference))) {
    triomics_abort(
      "A_SCALE_REFERENCE", "PQN reference mismatch: a reference value is not finite.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "Supply a positive finite reference for every feature."
    )
  }
  if (any(reference == 0)) {
    triomics_abort(
      "A_SCALE_ZERO", "PQN reference denominator is zero.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "A quotient denominator must be positive. Do not divide by zero."
    )
  }
  if (any(reference < 0)) {
    triomics_abort(
      "A_SCALE_NONPOSITIVE", "PQN reference is negative.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "Reference values must be positive linear abundances."
    )
  }
  invisible(TRUE)
}

triomics_metabolomics_finish_log2 <- function(se, zero_policy, zero_value) {
  if (is.null(zero_policy) || !is.character(zero_policy) || !nzchar(zero_policy)) zero_policy <- "error"
  matrix <- SummarizedExperiment::assay(se, "analysis")
  finite <- is.finite(matrix)
  if (any(matrix[finite] < 0)) {
    triomics_abort(
      "A_SCALE_NONPOSITIVE", "log2 cannot transform negative values.",
      key = "transform", module = "profile_metabolomics", stage = "A",
      remediation = "PQN output must stay positive before log2."
    )
  }
  zeros <- finite & matrix == 0
  if (any(zeros)) {
    if (!identical(zero_policy, "declared_replacement") || !is.numeric(zero_value) ||
        length(zero_value) != 1L || !is.finite(zero_value) || zero_value <= 0) {
      triomics_abort(
        "A_SCALE_ZERO", "log2 found a zero without a declared positive replacement.",
        key = "zero_policy", module = "profile_metabolomics", stage = "A",
        remediation = "Use zero_policy error or a declared positive zero_value."
      )
    }
    matrix[zeros] <- zero_value
  }
  matrix[is.finite(matrix)] <- log2(matrix[is.finite(matrix)])
  SummarizedExperiment::assay(se, "analysis") <- matrix
  triomics <- triomics_meta(se)
  patched <- FALSE
  for (i in seq_along(triomics$completed)) {
    entry <- triomics$completed[[i]]
    if (identical(entry$stage, "transform") && identical(entry$method, "log2")) {
      entry$output_scale <- "log2"
      entry$deferred <- FALSE
      entry$applied_after <- "pqn"
      triomics$completed[[i]] <- entry
      patched <- TRUE
    }
  }
  if (!patched) {
    triomics_abort(
      "A_CONFIG_OPERATION", "log2 after PQN was requested without a deferred log2 transform.",
      key = "transform", module = "profile_metabolomics", stage = "A",
      remediation = "Request log2 with PQN so the logarithm is applied to the linear quotient."
    )
  }
  triomics$scale <- "log2"
  se <- triomics_set_triomics(se, triomics)
  row_data <- SummarizedExperiment::rowData(se)
  if ("scale" %in% names(row_data)) {
    row_data$scale <- rep("log2", nrow(row_data))
    SummarizedExperiment::rowData(se) <- row_data
  }
  learned <- list(zero_policy = zero_policy)
  if (identical(zero_policy, "declared_replacement")) learned$zero_value <- as.numeric(zero_value)
  triomics_merge_learned(se, learned)
}

triomics_metabolomics_pqn <- function(se, params) {
  scale <- triomics_meta(se)$scale
  if (!identical(scale, "linear")) {
    triomics_abort(
      "A_SCALE_INPUT", "PQN requires positive linear input before log2.",
      key = "normalise", module = "profile_metabolomics", stage = "A",
      remediation = "Compute quotients on the linear scale. Do not run PQN after log2."
    )
  }
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- matrix(as.character(SummarizedExperiment::assay(se, "original_mask")),
                 nrow = nrow(analysis), dimnames = dimnames(analysis))
  feature_keys <- rownames(analysis)
  if (identical(params$mode, "apply")) {
    reference <- triomics_metabolomics_reference_from_state(params$fitted_state, feature_keys)
  } else if (identical(params$operation$reference, "training_median")) {
    if (triomics_metabolomics_file_set(params$operation$reference_file)) {
      triomics_abort(
        "A_CONFIG_METHOD", "training_median PQN cannot also take a reference file.",
        key = "reference_file", module = "profile_metabolomics", stage = "A",
        remediation = "Use reference training_median or supplied, not both."
      )
    }
    training <- params$training_unit_ids
    if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
      triomics_abort(
        "A_STATE_PARTIAL", "PQN requires the declared training samples.",
        key = "training_unit_ids", module = "profile_metabolomics", stage = "A",
        remediation = "Freeze the reference from training rows only."
      )
    }
    reference <- triomics_metabolomics_training_reference(analysis, mask, training)
  } else if (identical(params$operation$reference, "supplied")) {
    reference <- triomics_metabolomics_read_reference(params$operation$reference_file, feature_keys)
  } else {
    triomics_abort(
      "A_CONFIG_METHOD", "PQN requires reference 'training_median' or 'supplied'.",
      key = "reference", module = "profile_metabolomics", stage = "A",
      remediation = "Name the frozen reference. There is no hidden spectrum."
    )
  }
  triomics_metabolomics_check_reference_signs(reference)
  for (j in seq_len(ncol(analysis))) {
    observed <- mask[, j] == "known_observed" & is.finite(analysis[, j])
    if (any(observed & analysis[, j] == 0)) {
      triomics_abort(
        "A_SCALE_ZERO", "PQN found a zero.",
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "Positive linear ratios cannot use a zero abundance."
      )
    }
    if (any(observed & analysis[, j] < 0)) {
      triomics_abort(
        "A_SCALE_NONPOSITIVE", "PQN found a negative value.",
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "Remove negative measurements before PQN."
      )
    }
  }
  factors <- numeric(ncol(analysis))
  names(factors) <- colnames(analysis)
  normalized <- analysis
  for (j in seq_len(ncol(analysis))) {
    observed <- mask[, j] == "known_observed" & is.finite(analysis[, j])
    if (!any(observed)) {
      triomics_abort(
        "A_SCALE_ALL_MISSING",
        paste0("Sample '", colnames(analysis)[[j]], "' has no valid positive PQN ratio."),
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "A sample needs at least one positive original observation."
      )
    }
    ratios <- analysis[observed, j] / reference[observed]
    factors[[j]] <- stats::median(ratios)
    if (!is.finite(factors[[j]]) || factors[[j]] <= 0) {
      triomics_abort(
        "A_SCALE_ZERO", "PQN dilution factor is not a positive denominator.",
        key = "normalise", module = "profile_metabolomics", stage = "A",
        remediation = "Do not divide by a zero or non-finite quotient."
      )
    }
    scale_cell <- mask[, j] %in% c("known_observed", "known_imputed") & is.finite(normalized[, j])
    normalized[scale_cell, j] <- normalized[scale_cell, j] / factors[[j]]
  }
  SummarizedExperiment::assay(se, "analysis") <- normalized
  reference_vector <- lapply(feature_keys, function(key) {
    list(feature_key = key, value = unname(reference[[key]]))
  })
  then_log2 <- isTRUE(params$operation$then_log2) ||
    isTRUE(triomics_meta(se)$state_payload$learned_parameters$pending_log2$applied_after == "pqn") ||
    (is.list(params$fitted_state) && isTRUE(params$fitted_state$learned_parameters$pqn$then_log2))
  if (identical(params$mode, "apply") && is.list(params$fitted_state)) {
    then_log2 <- isTRUE(params$fitted_state$learned_parameters$pqn$then_log2)
  }
  se <- triomics_merge_learned(se, list(pqn = list(
    reference = if (identical(params$mode, "apply")) {
      as.character(params$fitted_state$learned_parameters$pqn$reference)
    } else {
      as.character(params$operation$reference)
    },
    reference_scale = "linear",
    numeric_order = if (then_log2) c("pqn", "log2") else "pqn",
    then_log2 = then_log2,
    reference_hash = triomics_metabolomics_hash_json(reference_vector),
    reference_vector = reference_vector,
    fit_dilution_factors = lapply(colnames(analysis), function(id) {
      list(sample_id = id, factor = unname(factors[[id]]))
    })
  )))
  se <- triomics_record_operation(se, params$operation, "linear", "linear")
  if (!then_log2) return(se)
  pending <- triomics_meta(se)$state_payload$learned_parameters$pending_log2
  zero_policy <- if (is.list(pending) && !is.null(pending$zero_policy)) {
    pending$zero_policy
  } else {
    params$operation$zero_policy
  }
  zero_value <- if (is.list(pending)) pending$zero_value else params$operation$zero_value
  if (identical(params$mode, "apply") && is.list(params$fitted_state$learned_parameters$pending_log2)) {
    state_pending <- params$fitted_state$learned_parameters$pending_log2
    if (!is.null(state_pending$zero_policy)) zero_policy <- state_pending$zero_policy
    zero_value <- state_pending$zero_value
  }
  triomics_metabolomics_finish_log2(se, zero_policy, zero_value)
}

triomics_metabolomics_normalise <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_normalise(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "pqn")) {
    return(triomics_metabolomics_pqn(se, params))
  }
  triomics_metabolomics_refuse_operation(params$operation)
}

triomics_metabolomics_require_known_mask <- function(se) {
  mask <- as.character(SummarizedExperiment::assay(se, "original_mask"))
  if (any(mask == "unknown")) {
    triomics_abort(
      "A_SOURCE_MASK", "Unknown mask history cannot be imputed or treated as MAR.",
      key = "original_mask", module = "profile_metabolomics", stage = "A",
      remediation = "Leave unknown cells unknown. Do not fill them with a baseline or a MAR model."
    )
  }
  invisible(TRUE)
}

triomics_metabolomics_lineage_row <- function(feature_key, sample_id, method, value, role) {
  list(
    feature_key = feature_key,
    sample_id = sample_id,
    method = method,
    value = unname(as.numeric(value)),
    role = role
  )
}

triomics_metabolomics_collect_lineage <- function(before_mask, after_mask, after_values, method, role) {
  lineage <- list()
  for (i in seq_len(nrow(after_values))) {
    for (j in seq_len(ncol(after_values))) {
      if (identical(as.character(before_mask[i, j]), "known_missing") &&
          identical(as.character(after_mask[i, j]), "known_imputed")) {
        lineage[[length(lineage) + 1L]] <- triomics_metabolomics_lineage_row(
          rownames(after_values)[[i]], colnames(after_values)[[j]], method, after_values[i, j], role
        )
      }
    }
  }
  lineage
}

triomics_metabolomics_training_median <- function(se, params) {
  triomics_metabolomics_require_known_mask(se)
  before_mask <- SummarizedExperiment::assay(se, "original_mask")
  se <- triomics_shared_impute(se, params)
  description <- triomics_metabolomics_imputation_description("training_median")
  lineage <- triomics_metabolomics_collect_lineage(
    before_mask,
    SummarizedExperiment::assay(se, "original_mask"),
    SummarizedExperiment::assay(se, "analysis"),
    "training_median",
    description$role
  )
  triomics_merge_learned(se, list(
    imputation_role = description,
    imputation_lineage = lineage
  ))
}

triomics_metabolomics_character_ids <- function(value) {
  if (is.null(value) || !length(value)) return(character())
  if (is.list(value) && !is.null(value[[1L]]) && is.list(value[[1L]])) {
    return(vapply(value, function(item) as.character(item[[1L]]), character(1)))
  }
  as.character(unlist(value, use.names = FALSE))
}

triomics_metabolomics_knn_distance <- function(query, donor) {
  shared <- is.finite(query) & is.finite(donor)
  if (!any(shared)) return(NA_real_)
  sqrt(mean((query[shared] - donor[shared])^2))
}

triomics_metabolomics_donor_matrix <- function(feature_keys, sample_ids, cells) {
  matrix_values <- matrix(NA_real_, length(feature_keys), length(sample_ids),
                          dimnames = list(feature_keys, sample_ids))
  for (cell in cells) {
    key <- as.character(cell$feature_key)[[1L]]
    sample_id <- as.character(cell$sample_id)[[1L]]
    if (key %in% feature_keys && sample_id %in% sample_ids) {
      matrix_values[key, sample_id] <- triomics_metabolomics_number(cell$value)
    }
  }
  matrix_values
}

triomics_metabolomics_fit_donors <- function(analysis, mask, training) {
  cells <- list()
  for (sample_id in training) {
    for (key in rownames(analysis)) {
      if (identical(as.character(mask[key, sample_id]), "known_observed") && is.finite(analysis[key, sample_id])) {
        cells[[length(cells) + 1L]] <- list(
          sample_id = sample_id,
          feature_key = key,
          value = unname(as.numeric(analysis[key, sample_id]))
        )
      }
    }
  }
  list(sample_ids = as.character(training), cells = cells)
}

triomics_metabolomics_impute_from_donors <- function(analysis, mask, donors, k) {
  feature_keys <- rownames(analysis)
  donor_ids <- as.character(donors$sample_ids)
  donor_values <- triomics_metabolomics_donor_matrix(feature_keys, donor_ids, donors$cells)
  output <- analysis
  output_mask <- matrix(as.character(mask), nrow = nrow(mask), dimnames = dimnames(mask))
  lineage <- list()
  queries <- colnames(analysis)
  for (sample_id in queries) {
    query <- ifelse(output_mask[, sample_id] == "known_observed" & is.finite(analysis[, sample_id]),
                    analysis[, sample_id], NA_real_)
    missing <- which(output_mask[, sample_id] == "known_missing")
    for (index in missing) {
      eligible <- donor_ids[is.finite(donor_values[index, ]) & donor_ids != sample_id]
      if (length(eligible) < k) {
        triomics_abort(
          "A_SCALE_NEIGHBORS",
          paste0("kNN has fewer than ", k, " training donors for feature '", feature_keys[[index]], "'."),
          key = "k", module = "profile_metabolomics", stage = "A",
          remediation = "Do not shrink k and do not recruit held-out rows as donors."
        )
      }
      distances <- vapply(eligible, function(id) {
        triomics_metabolomics_knn_distance(query, donor_values[, id])
      }, numeric(1))
      ok <- is.finite(distances)
      if (sum(ok) < k) {
        triomics_abort(
          "A_SCALE_NEIGHBORS",
          paste0("kNN found fewer than ", k, " donors with a shared original observation."),
          key = "k", module = "profile_metabolomics", stage = "A",
          remediation = "A query needs original overlap with training donors. Held-out samples are not donors."
        )
      }
      eligible <- eligible[ok]
      distances <- distances[ok]
      values <- donor_values[index, eligible]
      ord <- order(distances, eligible)
      picked <- seq_len(k)
      picked_dist <- distances[ord][picked]
      picked_values <- values[ord][picked]
      picked_ids <- eligible[ord][picked]
      if (any(picked_dist == 0)) {
        weights <- ifelse(picked_dist == 0, 1, 0)
      } else {
        weights <- 1 / picked_dist
      }
      weights <- weights / sum(weights)
      output[index, sample_id] <- sum(weights * picked_values)
      output_mask[index, sample_id] <- "known_imputed"
      lineage[[length(lineage) + 1L]] <- c(
        triomics_metabolomics_lineage_row(feature_keys[[index]], sample_id, "knn", output[index, sample_id], "qualified_mar"),
        list(donor_sample_ids = as.character(picked_ids), weights = as.numeric(weights))
      )
    }
  }
  list(values = output, mask = output_mask, lineage = lineage)
}

triomics_metabolomics_knn <- function(se, params) {
  triomics_metabolomics_require_known_mask(se)
  k <- triomics_metabolomics_positive_count(params$operation$k, "knn k")
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  if (identical(params$mode, "apply")) {
    model <- params$fitted_state$learned_parameters$knn
    donors <- list(
      sample_ids = triomics_metabolomics_character_ids(model$donor_sample_ids),
      cells = triomics_metabolomics_as_rows(model$donor_cells)
    )
  } else {
    training <- params$training_unit_ids
    if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
      triomics_abort(
        "A_STATE_PARTIAL", "kNN requires the declared training samples.",
        key = "training_unit_ids", module = "profile_metabolomics", stage = "A",
        remediation = "Fit donors on training rows only."
      )
    }
    donors <- triomics_metabolomics_fit_donors(analysis, mask, training)
  }
  imputed <- triomics_metabolomics_impute_from_donors(analysis, mask, donors, k)
  SummarizedExperiment::assay(se, "analysis") <- imputed$values
  SummarizedExperiment::assay(se, "original_mask") <- imputed$mask
  body <- list(sample_ids = as.character(donors$sample_ids), cells = donors$cells, k = k)
  if (!identical(params$mode, "apply")) {
    se <- triomics_merge_learned(se, list(
      knn = list(
        k = k,
        distance = "rms_shared_observed",
        weights = "inverse_distance",
        donors = "training_samples_only",
        donor_sample_ids = as.character(donors$sample_ids),
        donor_cells = donors$cells,
        donor_hash = triomics_metabolomics_hash_json(body)
      ),
      imputation_role = triomics_metabolomics_imputation_description("knn"),
      imputation_lineage = imputed$lineage
    ))
  }
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_metabolomics_restore_rng <- function(saved_kind, had_seed, saved_seed) {
  if (length(saved_kind) >= 3L) {
    RNGkind(saved_kind[[1L]], saved_kind[[2L]], saved_kind[[3L]])
  }
  if (had_seed) {
    assign(".Random.seed", saved_seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(TRUE)
}

triomics_metabolomics_stump <- function(train, target_index, boot, predictors) {
  y <- train[target_index, boot]
  node <- mean(y)
  best <- NULL
  best_sse <- Inf
  feature_names <- rownames(train)
  for (predictor in predictors) {
    x <- train[predictor, boot]
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2L) next
    unique_x <- sort(unique(x[ok]))
    if (length(unique_x) < 2L) next
    for (t in seq_len(length(unique_x) - 1L)) {
      threshold <- (unique_x[[t]] + unique_x[[t + 1L]]) / 2
      left <- ok & x <= threshold
      right <- ok & x > threshold
      if (!any(left) || !any(right)) next
      sse <- sum((y[left] - mean(y[left]))^2) + sum((y[right] - mean(y[right]))^2)
      if (sse < best_sse - 1e-12) {
        best_sse <- sse
        best <- list(
          split_feature = feature_names[[predictor]],
          threshold = unname(threshold),
          left_mean = unname(mean(y[left])),
          right_mean = unname(mean(y[right]))
        )
      }
    }
  }
  if (is.null(best)) {
    return(list(
      leaf = TRUE, split_feature = "", threshold = 0,
      left_mean = unname(node), right_mean = unname(node), node_mean = unname(node)
    ))
  }
  list(
    leaf = FALSE, split_feature = best$split_feature, threshold = best$threshold,
    left_mean = best$left_mean, right_mean = best$right_mean, node_mean = unname(node)
  )
}

triomics_metabolomics_fit_forest_rows <- function(train, trees, seed) {
  rows <- list()
  n_features <- nrow(train)
  for (feature in seq_len(n_features)) {
    observed <- which(is.finite(train[feature, ]))
    if (length(observed) < 2L) next
    predictors <- setdiff(seq_len(n_features), feature)
    for (tree_id in seq_len(trees)) {
      set.seed(as.integer(seed) + feature * 10007L + tree_id * 101L)
      boot <- observed[sample.int(length(observed), length(observed), replace = TRUE)]
      if (!length(predictors)) {
        chosen <- integer()
      } else if (length(predictors) == 1L) {
        chosen <- predictors
      } else {
        mtry <- max(1L, as.integer(floor(sqrt(length(predictors)))))
        chosen <- sample(predictors, mtry)
      }
      stump <- triomics_metabolomics_stump(train, feature, boot, chosen)
      rows[[length(rows) + 1L]] <- c(
        list(feature_key = rownames(train)[[feature]], tree_id = as.integer(tree_id)),
        stump
      )
    }
  }
  rows
}

triomics_metabolomics_predict_stump <- function(row, query) {
  node <- triomics_metabolomics_number(row$node_mean)
  leaf <- row$leaf
  if (is.list(leaf)) leaf <- unlist(leaf, use.names = FALSE)
  if (isTRUE(as.logical(leaf)[[1L]]) || !nzchar(as.character(row$split_feature)[[1L]])) return(node)
  feature <- as.character(row$split_feature)[[1L]]
  value <- query[[feature]]
  if (is.null(value) || length(value) != 1L || !is.finite(value)) return(node)
  if (value <= triomics_metabolomics_number(row$threshold)) {
    triomics_metabolomics_number(row$left_mean)
  } else {
    triomics_metabolomics_number(row$right_mean)
  }
}

triomics_metabolomics_group_rows <- function(rows, key) {
  keys <- vapply(rows, function(row) as.character(row[[key]])[[1L]], character(1))
  split(rows, factor(keys, levels = unique(keys)))
}

triomics_metabolomics_rf <- function(se, params) {
  triomics_metabolomics_require_known_mask(se)
  trees <- triomics_metabolomics_positive_count(params$operation$trees, "rf trees")
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- matrix(as.character(SummarizedExperiment::assay(se, "original_mask")),
                 nrow = nrow(analysis), dimnames = dimnames(analysis))
  if (identical(params$mode, "apply")) {
    model <- params$fitted_state$learned_parameters$rf
    forest_rows <- triomics_metabolomics_as_rows(model$forest_rows)
    training <- triomics_metabolomics_character_ids(model$donor_sample_ids)
  } else {
    training <- params$training_unit_ids
    if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
      triomics_abort(
        "A_STATE_PARTIAL", "RF imputation requires the declared training samples.",
        key = "training_unit_ids", module = "profile_metabolomics", stage = "A",
        remediation = "Fit forests on training rows only."
      )
    }
    train <- analysis[, training, drop = FALSE]
    train_mask <- mask[, training, drop = FALSE]
    for (i in seq_len(nrow(train))) {
      observed <- train_mask[i, ] == "known_observed" & is.finite(train[i, ])
      missing_any <- any(mask[i, ] == "known_missing")
      if (!any(observed)) {
        triomics_abort(
          "A_SCALE_ALL_MISSING",
          paste0("Feature '", rownames(train)[[i]], "' has no training observation for RF imputation."),
          key = "impute", module = "profile_metabolomics", stage = "A",
          remediation = "Do not grow a forest for an all-missing feature."
        )
      }
      if (sum(observed) < 2L && missing_any) {
        triomics_abort(
          "A_SCALE_NEIGHBORS",
          paste0("Feature '", rownames(train)[[i]], "' does not have two training observations for a forest."),
          key = "trees", module = "profile_metabolomics", stage = "A",
          remediation = "Do not fit a forest on a single training row or on held-out rows."
        )
      }
      train[i, !observed] <- NA_real_
    }
    saved_kind <- RNGkind()
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    saved_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit(triomics_metabolomics_restore_rng(saved_kind, had_seed, saved_seed), add = TRUE)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    seed <- params$seed
    if (is.null(seed) || !is.numeric(seed) || !is.finite(seed)) seed <- 1L
    forest_rows <- triomics_metabolomics_fit_forest_rows(train, trees, seed)
  }
  grouped <- if (length(forest_rows)) triomics_metabolomics_group_rows(forest_rows, "feature_key") else list()
  output <- analysis
  before_mask <- matrix(as.character(mask), nrow = nrow(mask), dimnames = dimnames(mask))
  for (j in seq_len(ncol(output))) {
    query <- stats::setNames(rep(NA_real_, nrow(output)), rownames(output))
    observed <- mask[, j] == "known_observed" & is.finite(analysis[, j])
    query[observed] <- analysis[observed, j]
    missing <- which(mask[, j] == "known_missing")
    for (index in missing) {
      key <- rownames(output)[[index]]
      rows <- grouped[[key]]
      if (is.null(rows) || !length(rows)) {
        triomics_abort(
          "A_STATE_PARTIAL",
          paste0("No fitted forest is stored for feature '", key, "'."),
          key = "impute", module = "profile_metabolomics", stage = "A",
          remediation = "Apply the stored forest. Do not refit on held-out rows."
        )
      }
      predictions <- vapply(rows, function(row) triomics_metabolomics_predict_stump(row, query), numeric(1))
      output[index, j] <- mean(predictions)
      mask[index, j] <- "known_imputed"
    }
  }
  SummarizedExperiment::assay(se, "analysis") <- output
  SummarizedExperiment::assay(se, "original_mask") <- mask
  if (!identical(params$mode, "apply")) {
    body <- list(
      seed = if (is.null(params$seed)) 1L else as.integer(params$seed),
      trees = trees,
      donor_sample_ids = as.character(training),
      forest_rows = forest_rows
    )
    se <- triomics_merge_learned(se, list(
      rf = list(
        trees = trees,
        forest = "bootstrap_stumps",
        donors = "training_rows_observed_target",
        donor_sample_ids = as.character(training),
        forest_rows = forest_rows,
        forest_hash = triomics_metabolomics_hash_json(body)
      ),
      imputation_role = triomics_metabolomics_imputation_description("rf"),
      imputation_lineage = triomics_metabolomics_collect_lineage(
        before_mask,
        mask,
        output,
        "rf",
        "qualified_mar"
      )
    ))
  }
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_metabolomics_impute <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_impute(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "training_median")) {
    return(triomics_metabolomics_training_median(se, params))
  }
  decision <- triomics_metabolomics_lane_decision(params$operation)
  triomics_metabolomics_refuse_operation(params$operation, decision)
}

triomics_metabolomics_derive <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_derive(se, params))
  }
  triomics_metabolomics_refuse_operation(params$operation)
}

triomics_metabolomics_profile <- function() {
  list(
    validate = triomics_metabolomics_validate,
    filter = triomics_metabolomics_filter,
    transform = triomics_metabolomics_transform,
    normalise = triomics_metabolomics_normalise,
    impute = triomics_metabolomics_impute,
    derive = triomics_metabolomics_derive
  )
}

triomics_register_metabolomics_profile <- function() {
  triomics_register_profile("metabolomics", triomics_metabolomics_profile())
  invisible("metabolomics")
}

addTaskCallback(local({
  done <- FALSE
  function(...) {
    if (done) return(FALSE)
    result <- try(triomics_register_metabolomics_profile(), silent = TRUE)
    if (inherits(result, "try-error")) return(TRUE)
    done <<- TRUE
    FALSE
  }
}))
