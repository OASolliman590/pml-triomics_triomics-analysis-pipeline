# Proteomic profile. Qualified MinProb, QRILC and VSN lanes stay unavailable
# until a pinned contract matches an executed fit/apply runtime. Do not impute
# those lanes with a local approximation.

triomics_proteomics_supported_methods <- function() {
  c("identity", "none", "prevalence", "log2", "median_shift", "training_median")
}

triomics_proteomics_lane_contracts <- function() {
  list(
    minprob = list(
      package = "imputeLCMD", native_fit = "impute.MinProb", native_apply = NULL,
      fit_apply = FALSE, inductive = FALSE
    ),
    qrilc = list(
      package = "imputeLCMD", native_fit = "impute.QRILC", native_apply = NULL,
      fit_apply = FALSE, inductive = FALSE
    ),
    vsn = list(
      package = "vsn", native_fit = "vsn2", native_apply = "predict",
      fit_apply = TRUE, inductive = TRUE
    )
  )
}

triomics_proteomics_same_marker <- function(left, right) {
  if (is.null(left) && is.null(right)) return(TRUE)
  identical(left, right)
}

triomics_proteomics_qualification_pinned <- function(qualification, method, contract) {
  if (!is.list(qualification) || is.null(names(qualification))) return(FALSE)
  if (!identical(qualification$status, "pinned")) return(FALSE)
  if (!identical(qualification$method_id, method)) return(FALSE)
  if (!identical(qualification$package, contract$package)) return(FALSE)
  version <- qualification$version
  if (!is.character(version) || length(version) != 1L || is.na(version) || !nzchar(version)) return(FALSE)
  if (!identical(qualification$native_fit, contract$native_fit)) return(FALSE)
  if (!triomics_proteomics_same_marker(qualification$native_apply, contract$native_apply)) return(FALSE)
  if (!identical(isTRUE(qualification$fit_apply), isTRUE(contract$fit_apply))) return(FALSE)
  contexts <- qualification$contexts
  if (is.null(contexts) || !any(as.character(contexts) == "proteomics")) return(FALSE)
  TRUE
}

triomics_proteomics_lane_decision <- function(operation) {
  method <- operation$method
  if (is.null(method) || !is.character(method) || length(method) != 1L) {
    return(list(status = "unavailable", reason = "method_not_registered"))
  }
  if (method %in% triomics_proteomics_supported_methods()) {
    return(list(status = "fit_apply", reason = "supported"))
  }
  if (method %in% c("knn", "rf", "pqn", "clr", "class_sum", "sample_amount", "internal_standard")) {
    return(list(status = "unavailable", reason = "not_proteomic_policy"))
  }
  contracts <- triomics_proteomics_lane_contracts()
  if (!method %in% names(contracts)) {
    return(list(status = "unavailable", reason = "method_not_registered"))
  }
  if (method %in% c("minprob", "qrilc")) {
    assumption <- operation$missingness_assumption
    if (is.null(assumption) || !identical(assumption, "mnar_left_censored")) {
      return(list(status = "unavailable", reason = "missingness_does_not_select_censoring"))
    }
    evidence <- operation$missingness_evidence
    if (!is.character(evidence) || length(evidence) != 1L || is.na(evidence) || !nzchar(evidence)) {
      return(list(status = "unavailable", reason = "left_censoring_evidence_missing"))
    }
  }
  contract <- contracts[[method]]
  qualification <- operation$qualification
  if (is.null(qualification)) {
    return(list(status = "unavailable", reason = "qualification_missing"))
  }
  if (!triomics_proteomics_qualification_pinned(qualification, method, contract)) {
    return(list(status = "unavailable", reason = "qualification_not_pinned"))
  }
  if (!isTRUE(contract$inductive)) {
    return(list(status = "unavailable", reason = "inductive_fit_apply_not_supported"))
  }
  # No executed native fit/apply runtime is pinned for this build.
  list(status = "unavailable", reason = "native_fit_apply_not_runtime_qualified")
}

triomics_proteomics_predictive_eligibility <- function(operation) {
  triomics_proteomics_lane_decision(operation)$status
}

triomics_proteomics_refuse_operation <- function(operation, decision = NULL) {
  if (is.null(decision)) decision <- triomics_proteomics_lane_decision(operation)
  method <- if (is.character(operation$method)) operation$method else "unknown"
  if (identical(decision$reason, "missingness_does_not_select_censoring")) {
    assumption <- operation$missingness_assumption
    sentence <- if (is.null(assumption) || identical(assumption, "unknown")) {
      "Unknown missingness does not select a censoring model."
    } else {
      paste0("Missingness assumption '", assumption, "' does not select a censoring model.")
    }
    message <- paste0(sentence, " (", decision$reason, ").")
  } else {
    message <- paste0("Proteomic lane '", method, "' is unavailable (", decision$reason, ").")
  }
  triomics_abort(
    "A_CONFIG_METHOD", message, key = method, module = "profile_proteomics", stage = "A",
    remediation = "Choose an explicit supported proteomic method. Do not fall back to another imputer or normaliser."
  )
}

triomics_proteomics_operations <- function(filter_method = "none", transform = "none",
                                           normalise = "none", impute = "none",
                                           min_observed = NULL, prevalence = NULL,
                                           zero_policy = "error", zero_value = NULL,
                                           reference = NULL, scope = "global",
                                           stratum = NULL, selection_policy = NULL,
                                           selection_justification = NULL,
                                           missingness_assumption = "unknown",
                                           missingness_evidence = NULL,
                                           qualification = NULL, count_metadata = NULL,
                                           derive = "none", tail_probability = NULL,
                                           scale_multiplier = NULL) {
  ops <- list(
    list(
      stage = "validate", method = "identity", count_metadata = count_metadata,
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
      stage = "transform", method = transform, zero_policy = zero_policy, zero_value = zero_value
    ),
    list(
      stage = "normalise", method = normalise, reference = reference, qualification = qualification
    ),
    list(
      stage = "impute", method = impute, qualification = qualification,
      missingness_assumption = missingness_assumption,
      missingness_evidence = missingness_evidence,
      tail_probability = tail_probability, scale_multiplier = scale_multiplier
    )
  )
  eligibility <- vapply(ops, triomics_proteomics_predictive_eligibility, character(1))
  if (any(eligibility == "unavailable")) {
    ops <- lapply(ops, function(op) {
      op$apply <- "unavailable"
      op
    })
  }
  ops
}

triomics_proteomics_json_number <- function(value) {
  if (is.null(value) || length(value) == 0L) return(NA_real_)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (!length(value) || all(is.na(value))) return(NA_real_)
  as.numeric(value[[1L]])
}

triomics_proteomics_count_records <- function(value) {
  if (is.null(value) || !length(value)) return(list())
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(i) as.list(value[i, , drop = FALSE])))
  }
  if (is.list(value) && !is.null(value$feature_key) && !is.list(value$feature_key)) {
    return(list(value))
  }
  value
}

triomics_proteomics_whole_count <- function(values, label) {
  present <- !is.na(values)
  if (!any(present)) return(invisible(TRUE))
  bad <- !is.finite(values[present]) | values[present] <= 0 |
    abs(values[present] - round(values[present])) > 1e-8
  if (any(bad)) {
    triomics_abort(
      "A_SOURCE_CONTRACT",
      paste0(label, " counts must be positive whole numbers."),
      key = "count_metadata", module = "profile_proteomics", stage = "A",
      remediation = "Supply positive peptide or PSM counts. Do not invent them from abundance."
    )
  }
  invisible(TRUE)
}

triomics_proteomics_attach_counts <- function(se, table) {
  if (is.null(table)) return(se)
  if (!is.data.frame(table)) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Peptide/PSM metadata must be a table keyed by feature_key.",
      key = "feature_key", module = "profile_proteomics", stage = "A",
      remediation = "Join counts by feature key. Do not bind them by row position."
    )
  }
  if (!all(c("feature_key", "acquisition", "provenance") %in% names(table)) ||
      (!"peptide_count" %in% names(table) && !"psm_count" %in% names(table))) {
    triomics_abort(
      "A_SOURCE_CONTRACT",
      "Peptide/PSM metadata must include feature_key, acquisition, and provenance.",
      key = "feature_key", module = "profile_proteomics", stage = "A",
      remediation = "Do not attach a positional count vector."
    )
  }
  keys <- as.character(table$feature_key)
  if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Peptide/PSM feature keys must be unique and non-blank.",
      key = "feature_key", module = "profile_proteomics", stage = "A",
      remediation = "Keep one count record for each measured feature key."
    )
  }
  assay_keys <- rownames(se)
  if (!setequal(keys, assay_keys)) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Peptide/PSM feature keys must match the assay feature keys exactly.",
      key = "feature_key", module = "profile_proteomics", stage = "A",
      remediation = "Do not add unrelated counts or drop a measured feature from the count table."
    )
  }
  provenance <- as.character(table$provenance)
  acquisition <- as.character(table$acquisition)
  if (anyNA(provenance) || any(!nzchar(provenance)) || anyNA(acquisition) || any(!nzchar(acquisition))) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Peptide/PSM metadata needs non-blank provenance and acquisition.",
      key = "provenance", module = "profile_proteomics", stage = "A",
      remediation = "Record where the peptide or PSM count came from. Do not infer it from intensity."
    )
  }
  index <- match(assay_keys, keys)
  peptide <- if ("peptide_count" %in% names(table)) as.numeric(table$peptide_count[index]) else {
    rep(NA_real_, length(assay_keys))
  }
  psm <- if ("psm_count" %in% names(table)) as.numeric(table$psm_count[index]) else {
    rep(NA_real_, length(assay_keys))
  }
  triomics_proteomics_whole_count(peptide, "Peptide")
  triomics_proteomics_whole_count(psm, "PSM")
  row_data <- SummarizedExperiment::rowData(se)
  row_data$peptide_count <- peptide
  row_data$psm_count <- psm
  row_data$count_acquisition <- acquisition[index]
  row_data$count_provenance <- provenance[index]
  SummarizedExperiment::rowData(se) <- row_data
  stored <- lapply(seq_along(assay_keys), function(i) {
    list(
      feature_key = assay_keys[[i]],
      peptide_count = if (is.na(peptide[[i]])) NULL else unname(peptide[[i]]),
      psm_count = if (is.na(psm[[i]])) NULL else unname(psm[[i]]),
      acquisition = acquisition[index][[i]],
      provenance = provenance[index][[i]]
    )
  })
  triomics_proteomics_store_counts(se, stored)
}

triomics_proteomics_store_counts <- function(se, stored) {
  triomics_merge_learned(se, list(
    count_metadata = stored,
    count_hash = triomics_hash_text(triomics_canonical_json(stored))
  ))
}

triomics_proteomics_refresh_counts <- function(se) {
  triomics <- triomics_meta(se)
  existing <- triomics_proteomics_count_records(triomics$state_payload$learned_parameters$count_metadata)
  if (!length(existing)) return(se)
  keys <- rownames(se)
  kept <- lapply(keys, function(key) {
    for (item in existing) {
      if (identical(as.character(item$feature_key), key)) return(item)
    }
    NULL
  })
  if (any(vapply(kept, is.null, logical(1L)))) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Retained features lost their peptide/PSM provenance.",
      key = "feature_key", module = "profile_proteomics", stage = "A",
      remediation = "Carry count records with the feature key when rows are filtered."
    )
  }
  triomics_proteomics_store_counts(se, kept)
}

triomics_proteomics_align_counts_apply <- function(se, params) {
  learned <- params$fitted_state$learned_parameters$count_metadata
  records <- triomics_proteomics_count_records(learned)
  if (!length(records)) return(se)
  records <- lapply(records, function(row) {
    list(
      feature_key = as.character(row$feature_key),
      peptide_count = triomics_proteomics_json_number(row$peptide_count),
      psm_count = triomics_proteomics_json_number(row$psm_count),
      acquisition = as.character(row$acquisition),
      provenance = as.character(row$provenance)
    )
  })
  row_data <- SummarizedExperiment::rowData(se)
  has_cols <- any(c("peptide_count", "psm_count", "count_acquisition", "count_provenance") %in% names(row_data))
  mismatch <- function(got, expected) {
    if (is.na(got) && is.na(expected)) return(FALSE)
    if (is.na(got) || is.na(expected)) return(TRUE)
    if (is.numeric(got) || is.numeric(expected)) {
      return(!is.finite(as.numeric(got)) || abs(as.numeric(got) - as.numeric(expected)) > 1e-8)
    }
    !identical(as.character(got), as.character(expected))
  }
  if (has_cols) {
    for (record in records) {
      key <- record$feature_key
      if (!key %in% rownames(row_data)) next
      if ("peptide_count" %in% names(row_data) &&
          mismatch(triomics_proteomics_json_number(row_data[key, "peptide_count"]), record$peptide_count)) {
        triomics_abort(
          "A_SOURCE_CONTRACT", "Peptide/PSM counts disagree with fit provenance.",
          key = "provenance", module = "profile_proteomics", stage = "A",
          remediation = "Do not replace fit peptide or PSM counts on apply."
        )
      }
      if ("psm_count" %in% names(row_data) &&
          mismatch(triomics_proteomics_json_number(row_data[key, "psm_count"]), record$psm_count)) {
        triomics_abort(
          "A_SOURCE_CONTRACT", "Peptide/PSM counts disagree with fit provenance.",
          key = "provenance", module = "profile_proteomics", stage = "A",
          remediation = "Do not replace fit peptide or PSM counts on apply."
        )
      }
    }
    return(se)
  }
  peptide <- rep(NA_real_, nrow(se))
  psm <- rep(NA_real_, nrow(se))
  acquisition <- rep(NA_character_, nrow(se))
  provenance <- rep(NA_character_, nrow(se))
  for (record in records) {
    hit <- match(record$feature_key, rownames(se))
    if (is.na(hit)) next
    peptide[[hit]] <- record$peptide_count
    psm[[hit]] <- record$psm_count
    acquisition[[hit]] <- record$acquisition
    provenance[[hit]] <- record$provenance
  }
  row_data$peptide_count <- peptide
  row_data$psm_count <- psm
  row_data$count_acquisition <- acquisition
  row_data$count_provenance <- provenance
  SummarizedExperiment::rowData(se) <- row_data
  se
}

triomics_proteomics_validate <- function(se, params) {
  if (identical(params$mode, "apply")) {
    se <- triomics_proteomics_align_counts_apply(se, params)
  } else if (!is.null(params$operation$count_metadata)) {
    se <- triomics_proteomics_attach_counts(se, params$operation$count_metadata)
  }
  triomics_shared_validate(se, params)
}

triomics_proteomics_loss_row <- function(se, index, params, reason) {
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

triomics_proteomics_drop_declared <- function(se, params) {
  row_data <- SummarizedExperiment::rowData(se)
  if (!"declared_exclusion" %in% names(row_data)) return(se)
  labels <- as.character(row_data$declared_exclusion)
  labels[is.na(labels)] <- ""
  unknown <- !labels %in% c("", "contaminant", "qc_failure")
  if (any(unknown)) {
    triomics_abort(
      "A_CONFIG_OPERATION",
      "declared_exclusion must be contaminant, qc_failure, or empty.",
      key = "declared_exclusion", module = "profile_proteomics", stage = "A",
      remediation = "Declare contaminant or qc_failure. Do not infer exclusions from feature identifiers."
    )
  }
  drop <- labels %in% c("contaminant", "qc_failure")
  if (!any(drop)) return(se)
  flow <- do.call(rbind, lapply(which(drop), function(index) {
    reason <- if (identical(labels[[index]], "contaminant")) "declared_contaminant" else "declared_qc_failure"
    triomics_proteomics_loss_row(se, index, params, reason)
  }))
  se <- se[!drop, , drop = FALSE]
  if (!nrow(se)) {
    triomics_abort(
      "A_STATE_PARTIAL", "Declared exclusions removed every feature.",
      key = "declared_exclusion", module = "profile_proteomics", stage = "A",
      remediation = "Keep at least one measured feature after declared exclusions."
    )
  }
  triomics <- triomics_meta(se)
  triomics$declared_flow <- flow
  triomics_set_triomics(se, triomics)
}

triomics_proteomics_combine_flow <- function(se) {
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

triomics_proteomics_filter_stratum <- function(se, params) {
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
      key = "selection_policy", module = "profile_proteomics", stage = "A",
      remediation = "Declare the training-label policy. Do not infer groups from sample identifiers."
    )
  }
  col_data <- SummarizedExperiment::colData(se)
  if (!stratum %in% names(col_data)) {
    triomics_abort(
      "A_CONFIG_OPERATION", "The declared stratum column is not in sample metadata.",
      key = "stratum", module = "profile_proteomics", stage = "A",
      remediation = "Name a colData column. Do not parse groups out of sample ids."
    )
  }
  training <- params$training_unit_ids
  if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
    triomics_abort(
      "A_STATE_PARTIAL", "Group-anchored filtering requires the declared training samples.",
      key = "training_unit_ids", module = "profile_proteomics", stage = "A",
      remediation = "Count original observations on training units only."
    )
  }
  labels <- as.character(col_data[[stratum]])
  names(labels) <- colnames(se)
  train_labels <- labels[training]
  if (anyNA(train_labels) || any(!nzchar(train_labels))) {
    triomics_abort(
      "A_CONFIG_OPERATION", "Training samples need explicit stratum labels.",
      key = "stratum", module = "profile_proteomics", stage = "A",
      remediation = "Do not fill missing group labels from held-out samples."
    )
  }
  groups <- unique(train_labels)
  minimum <- params$operation$min_observed
  prevalence <- params$operation$prevalence
  if (is.null(minimum) && is.null(prevalence)) {
    triomics_abort(
      "A_CONFIG_OPERATION", "Prevalence filtering requires a declared minimum.",
      key = "filter", module = "profile_proteomics", stage = "A",
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
  if (!any(keep)) {
    triomics_abort(
      "A_STATE_PARTIAL", "Prevalence filtering removed every feature.",
      key = "filter", module = "profile_proteomics", stage = "A",
      remediation = "Declare a minimum the training samples can meet, and keep at least one observed feature."
    )
  }
  se <- se[keep, , drop = FALSE]
  triomics <- triomics_meta(se)
  triomics$feature_flow <- do.call(rbind, flow)
  se <- triomics_set_triomics(se, triomics)
  se <- triomics_proteomics_combine_flow(se)
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_proteomics_filter <- function(se, params) {
  method <- params$operation$method
  if (identical(params$mode, "apply")) {
    keys <- as.character(params$fitted_state$feature_keys)
    if (!length(keys) || any(!keys %in% rownames(se))) {
      triomics_abort(
        "A_STATE_FEATURES", "Apply data is missing a feature from the frozen fit universe.",
        key = "feature_key", module = "profile_proteomics", stage = "A",
        remediation = "Do not reselect features on apply. Supply the fit feature keys."
      )
    }
    se <- se[keys, , drop = FALSE]
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  if (!is.character(method) || length(method) != 1L || !method %in% c("none", "prevalence")) {
    triomics_proteomics_refuse_operation(params$operation)
  }
  se <- triomics_proteomics_drop_declared(se, params)
  scope <- params$operation$scope
  if (identical(method, "none")) {
    se <- triomics_proteomics_combine_flow(se)
    scale <- triomics_meta(se)$scale
    se <- triomics_record_operation(se, params$operation, scale, scale)
    return(triomics_proteomics_refresh_counts(se))
  }
  if (is.null(scope) || identical(scope, "global")) {
    se <- triomics_shared_filter(se, params)
    se <- triomics_proteomics_combine_flow(se)
    return(triomics_proteomics_refresh_counts(se))
  }
  if (!identical(scope, "stratum")) {
    triomics_abort(
      "A_CONFIG_OPERATION", "Proteomic prevalence scope must be global or stratum.",
      key = "scope", module = "profile_proteomics", stage = "A",
      remediation = "Use global or a declared stratum. Do not invent a grouping."
    )
  }
  se <- triomics_proteomics_filter_stratum(se, params)
  triomics_proteomics_refresh_counts(se)
}

triomics_proteomics_transform <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && method %in% c("none", "log2")) {
    return(triomics_shared_transform(se, params))
  }
  triomics_proteomics_refuse_operation(params$operation)
}

triomics_proteomics_sample_median <- function(values, mask) {
  observed <- mask == "known_observed" & is.finite(values)
  if (!any(observed)) return(NA_real_)
  stats::median(values[observed])
}

triomics_proteomics_shift_matrix <- function(analysis, mask, training_median) {
  for (id in colnames(analysis)) {
    sample_median <- triomics_proteomics_sample_median(analysis[, id], mask[, id])
    if (!is.finite(sample_median)) next
    offset <- sample_median - training_median
    shift <- mask[, id] %in% c("known_observed", "known_imputed") & is.finite(analysis[, id])
    analysis[shift, id] <- analysis[shift, id] - offset
  }
  analysis
}

triomics_proteomics_median_shift <- function(se, params) {
  if (!identical(params$operation$reference, "training_median")) {
    triomics_abort(
      "A_CONFIG_METHOD", "median_shift requires reference 'training_median'.",
      key = "reference", module = "profile_proteomics", stage = "A",
      remediation = "Set reference to training_median. There is no hidden reference."
    )
  }
  scale <- triomics_meta(se)$scale
  if (!identical(scale, "log2")) {
    triomics_abort(
      "A_SCALE_INPUT", "median_shift requires data that are already on the log2 scale.",
      key = "normalise", module = "profile_proteomics", stage = "A",
      remediation = "Apply log2 to positive linear data first, or declare an input scale of log2."
    )
  }
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  if (identical(params$mode, "apply")) {
    learned <- params$fitted_state$learned_parameters$median_shift
    training_median <- triomics_proteomics_json_number(if (is.null(learned)) NULL else learned$training_median)
    if (!is.finite(training_median)) {
      triomics_abort(
        "A_STATE_PARTIAL", "Median-shift state has no frozen training median.",
        key = "learned_parameters", module = "profile_proteomics", stage = "A",
        remediation = "Apply the fit state. Do not reestimate the training median."
      )
    }
  } else {
    training <- params$training_unit_ids
    if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
      triomics_abort(
        "A_STATE_PARTIAL", "median_shift requires the declared training samples.",
        key = "training_unit_ids", module = "profile_proteomics", stage = "A",
        remediation = "Freeze the training median from training samples only."
      )
    }
    sample_medians <- vapply(training, function(id) {
      triomics_proteomics_sample_median(analysis[, id], mask[, id])
    }, numeric(1))
    if (any(!is.finite(sample_medians))) {
      triomics_abort(
        "A_SCALE_ALL_MISSING",
        "median_shift found a training sample with no original observations.",
        key = "normalise", module = "profile_proteomics", stage = "A",
        remediation = "Do not invent a zero offset for an unobserved training sample."
      )
    }
    training_median <- stats::median(sample_medians)
    se <- triomics_merge_learned(se, list(median_shift = list(
      reference = "training_median",
      training_median = unname(training_median),
      training_sample_ids = as.character(training),
      training_sample_medians = unname(as.numeric(sample_medians))
    )))
  }
  SummarizedExperiment::assay(se, "analysis") <- triomics_proteomics_shift_matrix(analysis, mask, training_median)
  triomics_record_operation(se, params$operation, "log2", "log2")
}

triomics_proteomics_normalise <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_normalise(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "median_shift")) {
    return(triomics_proteomics_median_shift(se, params))
  }
  triomics_proteomics_refuse_operation(params$operation)
}

triomics_proteomics_impute <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && method %in% c("none", "training_median")) {
    return(triomics_shared_impute(se, params))
  }
  triomics_proteomics_refuse_operation(params$operation)
}

triomics_proteomics_derive <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_derive(se, params))
  }
  triomics_proteomics_refuse_operation(params$operation)
}

triomics_proteomics_profile <- function() {
  list(
    validate = triomics_proteomics_validate,
    filter = triomics_proteomics_filter,
    transform = triomics_proteomics_transform,
    normalise = triomics_proteomics_normalise,
    impute = triomics_proteomics_impute,
    derive = triomics_proteomics_derive
  )
}

triomics_register_proteomics_profile <- function() {
  triomics_register_profile("proteomics", triomics_proteomics_profile())
  invisible("proteomics")
}

addTaskCallback(local({
  done <- FALSE
  function(...) {
    if (done) return(FALSE)
    result <- try(triomics_register_proteomics_profile(), silent = TRUE)
    if (inherits(result, "try-error")) return(TRUE)
    done <<- TRUE
    FALSE
  }
}))
