# Lipidomics profile. Policy is the profile id, not the block display name.
# Sample-amount and internal-standard corrections stay in linear space.

triomics_lipidomics_supported_methods <- function() {
  c(
    "identity", "none", "prevalence", "log2", "clr",
    "sample_amount", "internal_standard", "training_median",
    "class_sum", "weighted_carbons", "weighted_double_bonds",
    "molar_fraction", "within_class_fraction", "saturated_unsaturated_ratio",
    "lipid_derive"
  )
}

triomics_lipidomics_policy_identity <- function(block_id, display_name) {
  # block_id and display_name do not select or alter lipid policy.
  methods <- unname(triomics_lipidomics_supported_methods())
  list(
    profile_id = "lipidomics",
    methods = methods,
    policy_hash = triomics_hash_text(triomics_canonical_json(methods))
  )
}

triomics_lipidomics_lane_decision <- function(operation) {
  method <- if (is.list(operation)) operation$method else NULL
  if (is.null(method) || !is.character(method) || length(method) != 1L || is.na(method)) {
    return(list(status = "unavailable", reason = "method_not_registered"))
  }
  if (method %in% triomics_lipidomics_supported_methods()) {
    return(list(status = "fit_apply", reason = "supported"))
  }
  if (method %in% c("median_shift", "pqn", "vsn", "minprob", "qrilc", "knn", "rf")) {
    return(list(status = "unavailable", reason = "not_lipid_policy"))
  }
  list(status = "unavailable", reason = "method_not_registered")
}

triomics_lipidomics_predictive_eligibility <- function(operation) {
  triomics_lipidomics_lane_decision(operation)$status
}

triomics_lipidomics_refuse_operation <- function(operation, decision = NULL) {
  if (is.null(decision)) decision <- triomics_lipidomics_lane_decision(operation)
  method <- if (is.list(operation) && is.character(operation$method)) operation$method else "unknown"
  triomics_abort(
    "A_CONFIG_METHOD",
    paste0("Lipidomic lane '", method, "' is unavailable (", decision$reason, ")."),
    key = method, module = "profile_lipidomics", stage = "A",
    remediation = "Choose an explicit lipid method. Renaming the block does not change this policy."
  )
}

triomics_lipidomics_operations <- function(filter_method = "none", transform = "none",
                                           normalise = "none", impute = "none",
                                           min_observed = NULL, prevalence = NULL,
                                           zero_policy = NULL, zero_value = NULL,
                                           reference_set = NULL, reference_features = NULL,
                                           log_base = NULL, scope = "global",
                                           metadata_column = NULL, amount_units = NULL,
                                           output_quantity = NULL, standards_map = NULL,
                                           derive = "none",
                                           missingness_assumption = "unknown") {
  if (is.list(derive)) {
    derive_op <- derive
    derive_op$stage <- "derive"
    if (is.null(derive_op$method) || !is.character(derive_op$method)) {
      derive_op$method <- "lipid_derive"
    }
  } else {
    derive_op <- list(stage = "derive", method = derive)
  }
  list(
    list(
      stage = "validate", method = "identity",
      missingness_assumption = missingness_assumption
    ),
    list(
      stage = "filter", method = filter_method, min_observed = min_observed,
      prevalence = prevalence, scope = scope
    ),
    derive_op,
    list(
      stage = "transform", method = transform, zero_policy = zero_policy,
      zero_value = zero_value, reference_set = reference_set,
      reference_features = reference_features, log_base = log_base
    ),
    list(
      stage = "normalise", method = normalise, metadata_column = metadata_column,
      amount_units = amount_units, output_quantity = output_quantity,
      standards_map = standards_map
    ),
    list(
      stage = "impute", method = impute,
      missingness_assumption = missingness_assumption
    )
  )
}

triomics_lipid_analysis_matrix <- function(se) {
  assay <- SummarizedExperiment::assay(se, "analysis")
  matrix(as.numeric(assay), nrow = nrow(assay), dimnames = dimnames(assay))
}

triomics_lipid_mask_matrix <- function(se) {
  mask <- SummarizedExperiment::assay(se, "original_mask")
  matrix(as.character(mask), nrow = nrow(mask), dimnames = dimnames(mask))
}

triomics_lipid_endpoints <- function(se) {
  endpoints <- triomics_meta(se)$lipid_endpoints
  if (is.null(endpoints)) list() else endpoints
}

triomics_lipid_endpoint_sample <- function(se, feature_key, sample_id) {
  for (endpoint in triomics_lipid_endpoints(se)) {
    if (!identical(as.character(endpoint$feature_key), as.character(feature_key))) next
    for (sample in endpoint$samples) {
      if (identical(as.character(sample$sample_id), as.character(sample_id))) {
        return(list(sample = sample, endpoint = endpoint))
      }
    }
  }
  NULL
}

triomics_lipid_clr_record <- function(se) triomics_meta(se)$lipid_clr

triomics_lipid_amount_record <- function(se) triomics_meta(se)$lipid_amount

triomics_lipidomics_validate <- function(se, params) {
  triomics_shared_validate(se, params)
}

triomics_lipidomics_filter <- function(se, params) {
  scope <- params$operation$scope
  if (identical(scope, "stratum")) {
    triomics_abort(
      "A_CONFIG_OPERATION",
      "A group-anchored lipid filter is not selected by the block name.",
      key = "scope", module = "profile_lipidomics", stage = "A",
      remediation = "Use a global prevalence filter or an explicit none. Do not infer a stratum from the display name."
    )
  }
  triomics_shared_filter(se, params)
}

triomics_lipidomics_store <- function(se, field, value) {
  triomics <- triomics_meta(se)
  triomics[[field]] <- value
  triomics_set_triomics(se, triomics)
}

triomics_lipidomics_derive <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_derive(se, params))
  }
  decision <- triomics_lipidomics_lane_decision(params$operation)
  if (!identical(decision$status, "fit_apply")) {
    triomics_lipidomics_refuse_operation(params$operation, decision)
  }
  assay <- triomics_lipid_analysis_matrix(se)
  mask <- triomics_lipid_mask_matrix(se)
  result <- triomics_lipid_derivation_result(assay, mask, params$operation, triomics_meta(se)$scale)
  if (identical(params$mode, "apply")) {
    previous <- params$fitted_state$learned_parameters$lipid_derivation$universe_hash
    if (!identical(as.character(previous)[[1L]], result$learned$universe_hash)) {
      triomics_lipid_abort(
        "A_STATE_STALE",
        "The lipid parent universe does not match the fit state.",
        "parent_set_ref"
      )
    }
  }
  se <- triomics_lipidomics_store(se, "lipid_endpoints", result$endpoints)
  se <- triomics_merge_learned(se, list(lipid_derivation = result$learned))
  triomics_record_operation(se, params$operation, "linear", "linear")
}

triomics_lipidomics_clr <- function(se, params) {
  scale <- triomics_meta(se)$scale
  if (!identical(scale, "linear")) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "CLR requires positive linear parts. Refusing to transform logarithms.",
      "transform"
    )
  }
  operation <- params$operation
  reference_features <- as.character(operation$reference_features)
  log_base <- operation$log_base
  zero_policy <- operation$zero_policy
  zero_value <- operation$zero_value
  triomics_lipid_require_text(operation$reference_set, "reference_set")
  assay <- triomics_lipid_analysis_matrix(se)
  transformed <- triomics_lipid_clr_values(assay, reference_features, log_base, zero_policy, zero_value)
  if (identical(params$mode, "apply")) {
    previous <- params$fitted_state$learned_parameters$lipid_clr$reference_hash
    if (!identical(as.character(previous)[[1L]], transformed$reference_hash)) {
      triomics_lipid_abort("A_STATE_STALE", "The CLR reference set does not match the fit state.", "reference_set")
    }
  }
  SummarizedExperiment::assay(se, "analysis") <- transformed$values
  record <- list(
    estimand = "clr_log_ratio",
    abundance_estimand = FALSE,
    class_total_erased_from_assay = TRUE,
    abundance_retained_in_state = TRUE,
    disclosure = "CLR is a log-ratio estimand, not an abundance. Linear abundances and their class total are retained in state. Do not compare CLR effects with raw or log2 abundance effects.",
    log_base = unname(as.numeric(log_base)),
    zero_policy = as.character(zero_policy),
    output_scale = transformed$output_scale,
    reference_set = as.character(operation$reference_set),
    abundance_before = transformed$abundance_before,
    class_total = transformed$class_total
  )
  se <- triomics_lipidomics_store(se, "lipid_clr", record)
  row_data <- SummarizedExperiment::rowData(se)
  row_data$estimand <- rep("clr_log_ratio", nrow(row_data))
  SummarizedExperiment::rowData(se) <- row_data
  learned <- list(
    reference_hash = transformed$reference_hash,
    reference_set = as.character(operation$reference_set),
    reference_features = lapply(reference_features, function(key) list(feature_key = key)),
    log_base = unname(as.numeric(log_base)),
    zero_policy = as.character(zero_policy),
    zero_value = triomics_lipid_json_number(zero_value),
    output_scale = transformed$output_scale,
    estimand = "clr_log_ratio",
    abundance_estimand = FALSE,
    class_total_erased_from_assay = TRUE,
    abundance_retained_in_state = TRUE,
    class_total = lapply(names(transformed$class_total), function(sample_id) {
      list(sample_id = sample_id, value = unname(as.numeric(transformed$class_total[[sample_id]])))
    }),
    abundance_before = lapply(rownames(assay), function(key) {
      list(
        feature_key = key,
        samples = lapply(colnames(assay), function(sample_id) {
          list(sample_id = sample_id, value = unname(as.numeric(assay[key, sample_id])))
        })
      )
    })
  )
  se <- triomics_merge_learned(se, list(lipid_clr = learned))
  triomics_record_operation(se, operation, "linear", transformed$output_scale)
}

triomics_lipidomics_transform <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_transform(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "log2")) {
    return(triomics_shared_transform(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "clr")) {
    return(triomics_lipidomics_clr(se, params))
  }
  triomics_lipidomics_refuse_operation(params$operation)
}

triomics_lipid_amounts <- function(se, column) {
  coldata <- SummarizedExperiment::colData(se)
  if (!is.character(column) || length(column) != 1L || is.na(column) || !nzchar(column) ||
      !column %in% names(coldata)) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      "Sample-amount normalisation needs a verified positive amount column.",
      "metadata_column"
    )
  }
  values <- coldata[[column]]
  sample_ids <- colnames(se)
  if ("sample_id" %in% names(coldata)) {
    index <- match(sample_ids, as.character(coldata$sample_id))
    if (anyNA(index)) {
      triomics_lipid_abort("A_ID_UNKNOWN_SAMPLE", "A sample amount is not aligned by sample id.", "metadata_column")
    }
    values <- values[index]
  }
  amounts <- suppressWarnings(as.numeric(values))
  if (length(amounts) != length(sample_ids) || any(!is.finite(amounts)) || any(amounts <= 0)) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "Sample amounts must be positive and finite for every sample.",
      "metadata_column"
    )
  }
  stats::setNames(unname(amounts), sample_ids)
}

triomics_lipidomics_sample_amount <- function(se, params) {
  if (!identical(triomics_meta(se)$scale, "linear")) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "Sample-amount correction requires linear abundances and cannot divide logarithms.",
      "normalise"
    )
  }
  operation <- params$operation
  requested <- operation$output_quantity
  if (is.character(requested) && length(requested) == 1L && !is.na(requested) &&
      requested %in% c("concentration", "molar_fraction")) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "Sample-amount correction cannot produce a molar concentration from intensity.",
      "output_quantity"
    )
  }
  amount_units <- triomics_lipid_require_text(operation$amount_units, "amount_units")
  amounts <- triomics_lipid_amounts(se, operation$metadata_column)
  if (identical(params$mode, "apply")) {
    previous <- params$fitted_state$learned_parameters$lipid_amount$metadata_column
    if (!identical(as.character(previous)[[1L]], as.character(operation$metadata_column))) {
      triomics_lipid_abort("A_STATE_STALE", "Apply changed the sample-amount column.", "metadata_column")
    }
  }
  analysis <- triomics_lipid_analysis_matrix(se)
  for (sample_id in colnames(analysis)) {
    finite <- is.finite(analysis[, sample_id])
    if (any(analysis[finite, sample_id] < 0)) {
      triomics_lipid_abort("A_SCALE_NONPOSITIVE", "Sample-amount correction found a negative abundance.", "normalise")
    }
    analysis[finite, sample_id] <- analysis[finite, sample_id] / amounts[[sample_id]]
  }
  SummarizedExperiment::assay(se, "analysis") <- analysis
  row_data <- SummarizedExperiment::rowData(se)
  input_units <- if ("units" %in% names(row_data)) unique(as.character(row_data$units)) else "unspecified"
  if (length(input_units) != 1L) {
    triomics_lipid_abort("A_SCALE_INPUT", "Sample-amount correction requires one input unit.", "units")
  }
  output_units <- paste0(input_units, "/", amount_units)
  if ("units" %in% names(row_data)) row_data$units <- rep(output_units, nrow(row_data))
  SummarizedExperiment::rowData(se) <- row_data
  quantity <- triomics_meta(se)$quantity
  if (is.null(quantity) || !nzchar(as.character(quantity))) quantity <- "intensity"
  record <- list(
    metadata_column = as.character(operation$metadata_column),
    amount_units = amount_units,
    output_units = output_units,
    output_quantity = as.character(quantity),
    absolute_concentration = FALSE,
    disclosure = "Sample-amount correction divides linear abundance by a declared positive amount. It does not create a molar concentration."
  )
  se <- triomics_lipidomics_store(se, "lipid_amount", record)
  se <- triomics_merge_learned(se, list(lipid_amount = list(
    metadata_column = record$metadata_column,
    amount_units = amount_units,
    output_units = output_units,
    output_quantity = record$output_quantity,
    absolute_concentration = FALSE
  )))
  triomics_record_operation(se, operation, "linear", "linear")
}

triomics_lipidomics_internal_standard <- function(se, params) {
  if (!identical(triomics_meta(se)$scale, "linear")) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "Internal-standard correction requires linear abundances and cannot rescale logarithms.",
      "normalise"
    )
  }
  operation <- params$operation
  items <- lapply(triomics_lipid_standard_items(operation$standards_map), triomics_lipid_resolve_standard)
  analysis <- triomics_lipid_analysis_matrix(se)
  for (item in items) {
    if (!item$feature_key %in% rownames(analysis) || !item$standard_key %in% rownames(analysis)) {
      triomics_lipid_abort("A_ID_REFERENCE", "An internal standard or analyte is not in the block.", "standards_map")
    }
    standard <- as.numeric(analysis[item$standard_key, ])
    analyte <- as.numeric(analysis[item$feature_key, ])
    if (any(!is.finite(standard)) || any(standard <= 0)) {
      triomics_lipid_abort(
        "A_SCALE_ZERO",
        "The internal standard is not a positive linear denominator.",
        "standards_map"
      )
    }
    if (any(is.finite(analyte) & analyte < 0)) {
      triomics_lipid_abort("A_SCALE_NONPOSITIVE", "An analyte abundance is negative.", "standards_map")
    }
  }
  if (identical(params$mode, "apply")) {
    previous <- params$fitted_state$learned_parameters$lipid_internal_standard$standards
    previous_factor <- if (is.list(previous) && length(previous)) previous[[1L]]$calibration_factor else NULL
    if (!identical(triomics_lipid_json_number(previous_factor), items[[1L]]$calibration_factor)) {
      triomics_lipid_abort("A_STATE_STALE", "Apply changed the internal-standard calibration.", "standards_map")
    }
  }
  for (item in items) {
    standard <- as.numeric(analysis[item$standard_key, ])
    analyte <- as.numeric(analysis[item$feature_key, ])
    finite <- is.finite(analyte)
    analysis[item$feature_key, finite] <- analyte[finite] / standard[finite] * item$calibration_factor
  }
  SummarizedExperiment::assay(se, "analysis") <- analysis
  row_data <- SummarizedExperiment::rowData(se)
  quantity <- if ("quantity" %in% names(row_data)) as.character(row_data$quantity) else rep("intensity", nrow(se))
  units <- if ("units" %in% names(row_data)) as.character(row_data$units) else rep("unspecified", nrow(se))
  for (item in items) {
    index <- match(item$feature_key, rownames(se))
    quantity[[index]] <- item$calibrated_quantity
    units[[index]] <- item$calibrated_units
  }
  row_data$quantity <- quantity
  row_data$units <- units
  SummarizedExperiment::rowData(se) <- row_data
  learned_rows <- lapply(items, function(item) {
    list(
      feature_key = item$feature_key,
      standard_key = item$standard_key,
      calibration_factor = item$calibration_factor,
      calibration_status = item$calibration_status,
      calibrated_quantity = item$calibrated_quantity,
      calibrated_units = item$calibrated_units
    )
  })
  se <- triomics_merge_learned(se, list(lipid_internal_standard = list(
    standards = learned_rows,
    absolute_concentration = any(vapply(items, function(item) isTRUE(item$absolute_concentration), logical(1L)))
  )))
  triomics_record_operation(se, operation, "linear", "linear")
}

triomics_lipidomics_normalise <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && identical(method, "none")) {
    return(triomics_shared_normalise(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "sample_amount")) {
    return(triomics_lipidomics_sample_amount(se, params))
  }
  if (is.character(method) && length(method) == 1L && identical(method, "internal_standard")) {
    return(triomics_lipidomics_internal_standard(se, params))
  }
  triomics_lipidomics_refuse_operation(params$operation)
}

triomics_lipidomics_impute <- function(se, params) {
  method <- params$operation$method
  if (is.character(method) && length(method) == 1L && method %in% c("none", "training_median")) {
    return(triomics_shared_impute(se, params))
  }
  triomics_lipidomics_refuse_operation(params$operation)
}

triomics_lipidomics_profile <- function() {
  list(
    validate = triomics_lipidomics_validate,
    filter = triomics_lipidomics_filter,
    transform = triomics_lipidomics_transform,
    normalise = triomics_lipidomics_normalise,
    impute = triomics_lipidomics_impute,
    derive = triomics_lipidomics_derive
  )
}

triomics_register_lipidomics_profile <- function() {
  triomics_register_profile("lipidomics", triomics_lipidomics_profile())
  invisible("lipidomics")
}

addTaskCallback(local({
  done <- FALSE
  function(...) {
    if (done) return(FALSE)
    result <- try(triomics_register_lipidomics_profile(), silent = TRUE)
    if (inherits(result, "try-error")) return(TRUE)
    done <<- TRUE
    FALSE
  }
}))
