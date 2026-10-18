triomics_integration_abort <- function(code, message, key = NULL,
                                       remediation = "Review the integration input and its declared identity maps.") {
  triomics_abort(
    code, message, key = key, module = "integration", stage = "C",
    remediation = remediation
  )
}

triomics_c01_ids <- function(value, label, code, allow_empty = FALSE) {
  if (!is.character(value) || anyNA(value) || any(!nzchar(trimws(value))) ||
      anyDuplicated(value) || (!allow_empty && length(value) == 0L)) {
    triomics_integration_abort(
      code, paste0(label, " must be a non-empty, unique vector of non-blank character ids."),
      key = label
    )
  }
  value
}

triomics_c01_pick_ids <- function(primary, fallback) {
  if (!is.null(primary)) primary else fallback
}

triomics_c01_optional_identity <- function(block, fields, code, block_id) {
  present_fields <- fields[fields %in% names(block)]
  if (!length(present_fields)) return(NA_character_)
  values <- lapply(present_fields, function(field) block[[field]])
  valid_values <- vapply(values, function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }, logical(1))
  if (!all(valid_values)) {
    triomics_integration_abort(
      code, "Block identity evidence must be one non-blank character value when supplied.",
      key = block_id
    )
  }
  values <- unlist(values, use.names = FALSE)
  if (any(values != values[[1L]])) {
    triomics_integration_abort(
      code, "Block identity evidence conflicts across supplied identity fields.",
      key = block_id
    )
  }
  values[[1L]]
}

triomics_integration_eligibility <- function(resolved_analysis, integration_spec) {
  if (!is.list(resolved_analysis) || !is.list(integration_spec)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_INPUT_INVALID",
      "Resolved analysis and integration specification must be named lists."
    )
  }

  block_collection <- resolved_analysis$blocks
  if (!is.list(block_collection) || is.null(names(block_collection)) ||
      anyNA(names(block_collection)) || any(!nzchar(names(block_collection))) ||
      anyDuplicated(names(block_collection))) {
    triomics_integration_abort(
      "C_ELIGIBILITY_BLOCK_MAP", "Resolved analysis blocks need unique stable block ids."
    )
  }
  available_block_ids <- names(block_collection)
  requested_block_ids <- triomics_c01_pick_ids(integration_spec$block_ids, available_block_ids)
  requested_block_ids <- triomics_c01_ids(
    requested_block_ids, "block_ids", "C_ELIGIBILITY_BLOCK_MAP"
  )
  if (length(requested_block_ids) < 2L) {
    triomics_integration_abort(
      "C_ELIGIBILITY_BLOCK_COUNT",
      "C-01 integration requires at least two requested predictor blocks.",
      key = "block_ids"
    )
  }
  missing_blocks <- setdiff(requested_block_ids, available_block_ids)
  if (length(missing_blocks)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_BLOCK_MISSING",
      "A requested integration block is absent from the resolved analysis.",
      key = "block_ids"
    )
  }

  explicit_unit_ids <- integration_spec$unit_ids
  root_unit_ids <- resolved_analysis$unit_ids
  block_sample_sets <- vector("list", length(requested_block_ids))
  names(block_sample_sets) <- requested_block_ids
  block_features <- vector("list", length(requested_block_ids))
  names(block_features) <- requested_block_ids
  block_studies <- character(length(requested_block_ids))
  names(block_studies) <- requested_block_ids
  block_contexts <- character(length(requested_block_ids))
  names(block_contexts) <- requested_block_ids
  block_representations <- character(length(requested_block_ids))
  names(block_representations) <- requested_block_ids

  root_study <- resolved_analysis$study_id
  root_context <- resolved_analysis$preparation_context_id
  root_representation <- triomics_c01_pick_ids(
    resolved_analysis$representation_id, resolved_analysis$representation
  )
  for (block_id in requested_block_ids) {
    block <- block_collection[[block_id]]
    if (!is.list(block) || !identical(block$block_id, block_id)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_BLOCK_MAP", "Block identity does not match its collection key.",
        key = block_id
      )
    }
    if (is.null(block$assay)) {
      code <- if (!is.null(block$summary)) "C_ELIGIBILITY_SUMMARY_ONLY" else "C_ELIGIBILITY_ASSAY_MISSING"
      triomics_integration_abort(
        code, "Integration requires keyed molecular values; summaries cannot reconstruct covariance.",
        key = block_id
      )
    }
    assay <- block$assay
    if (!is.matrix(assay) || !is.numeric(assay) || is.complex(assay) ||
        length(dim(assay)) != 2L || any(dim(assay) < 1L)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_ASSAY_INVALID", "Each integration assay must be a non-empty numeric matrix.",
        key = block_id
      )
    }

    feature_keys <- triomics_c01_ids(
      block$feature_keys, paste0(block_id, ".feature_keys"), "C_ELIGIBILITY_FEATURE_MAP"
    )
    sample_ids <- triomics_c01_ids(
      block$sample_ids, paste0(block_id, ".sample_ids"), "C_ELIGIBILITY_SAMPLE_MAP"
    )
    assay_features <- rownames(assay)
    assay_samples <- colnames(assay)
    if (is.null(assay_features) || is.null(assay_samples) ||
        !identical(assay_features, feature_keys) || !identical(assay_samples, sample_ids) ||
        nrow(assay) != length(feature_keys) || ncol(assay) != length(sample_ids)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_SAMPLE_MAP",
        "Assay row/column names must exactly match the declared feature and sample maps.",
        key = block_id
      )
    }
    if (any(is.infinite(assay) | is.nan(assay), na.rm = TRUE)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_NONFINITE", "Integration assay values must not contain Inf or NaN.",
        key = block_id
      )
    }
    if (anyNA(assay)) {
      triomics_integration_abort(
        "C_NA_CAPABILITY",
        "Concatenation PCA does not impute or estimate cell-level missing values.",
        key = block_id,
        remediation = "Provide a predecessor-produced representation with an explicit supported missingness policy."
      )
    }

    block_sample_sets[[block_id]] <- sample_ids
    block_features[[block_id]] <- feature_keys
    block_studies[[block_id]] <- triomics_c01_optional_identity(
      block, "study_id", "C_ELIGIBILITY_STUDY_MISMATCH", block_id
    )
    block_contexts[[block_id]] <- triomics_c01_optional_identity(
      block, "preparation_context_id", "C_ELIGIBILITY_CONTEXT_MISMATCH", block_id
    )
    block_representations[[block_id]] <- triomics_c01_optional_identity(
      block, c("representation_id", "representation"),
      "C_ELIGIBILITY_REPRESENTATION_MISMATCH", block_id
    )
  }

  if (is.null(explicit_unit_ids)) {
    if (length(block_sample_sets) > 1L &&
        !all(vapply(block_sample_sets[-1L], setequal, logical(1), block_sample_sets[[1L]]))) {
      triomics_integration_abort(
        "C_ELIGIBILITY_UNKNOWN_INTERSECTION",
        "Unequal block sample maps require an explicit integration unit map.",
        key = "unit_ids"
      )
    }
    explicit_unit_ids <- if (!is.null(root_unit_ids)) root_unit_ids else block_sample_sets[[1L]]
  }
  unit_ids <- sort(triomics_c01_ids(
    explicit_unit_ids, "unit_ids", "C_ELIGIBILITY_UNIT_MAP"
  ))
  if (!is.null(root_unit_ids)) {
    root_unit_ids <- triomics_c01_ids(root_unit_ids, "resolved_analysis.unit_ids", "C_ELIGIBILITY_UNIT_MAP")
    if (!setequal(root_unit_ids, unit_ids)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_UNIT_MAP", "Integration and resolved-analysis unit maps do not agree.",
        key = "unit_ids"
      )
    }
  }
  for (block_id in requested_block_ids) {
    unknown_samples <- setdiff(block_sample_sets[[block_id]], unit_ids)
    if (length(unknown_samples)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_UNKNOWN_UNIT", "A block contains sample ids outside the declared unit map.",
        key = block_id
      )
    }
  }

  study_id <- root_study
  if (!is.character(study_id) || length(study_id) != 1L || is.na(study_id) || !nzchar(study_id)) {
    triomics_integration_abort("C_ELIGIBILITY_STUDY_MISMATCH", "Resolved analysis needs one study id.")
  }
  if (any(!is.na(block_studies) & block_studies != study_id)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_STUDY_MISMATCH", "All integration blocks must belong to the resolved study.",
      key = "study_id"
    )
  }
  if (!is.character(root_context) || length(root_context) != 1L || is.na(root_context) ||
      !nzchar(root_context)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_CONTEXT_MISMATCH",
      "Resolved analysis needs one preparation context id.", key = "preparation_context_id"
    )
  }
  if (any(!is.na(block_contexts) & block_contexts != root_context)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_CONTEXT_MISMATCH",
      "A supplied block preparation context conflicts with the resolved context.",
      key = "preparation_context_id"
    )
  }
  if (!is.character(root_representation) || length(root_representation) != 1L ||
      is.na(root_representation) || !nzchar(root_representation) ||
      any(!is.na(block_representations) & block_representations != root_representation)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_REPRESENTATION_MISMATCH",
      "All integration blocks must use the same declared representation.", key = "representation"
    )
  }

  required_views <- integration_spec$required_views
  if (is.null(required_views)) required_views <- requested_block_ids
  required_views <- triomics_c01_ids(
    required_views, "required_views", "C_ELIGIBILITY_BLOCK_MAP"
  )
  if (length(setdiff(required_views, requested_block_ids))) {
    triomics_integration_abort(
      "C_ELIGIBILITY_BLOCK_MAP", "Required views must be among the requested integration blocks.",
      key = "required_views"
    )
  }

  block_availability <- lapply(requested_block_ids, function(block_id) {
    stats::setNames(unit_ids %in% block_sample_sets[[block_id]], unit_ids)
  })
  names(block_availability) <- requested_block_ids

  training_unit_ids <- integration_spec$training_unit_ids
  if (is.null(training_unit_ids)) training_unit_ids <- unit_ids
  training_unit_ids <- sort(triomics_c01_ids(
    training_unit_ids, "training_unit_ids", "C_ELIGIBILITY_TRAINING_MAP", allow_empty = TRUE
  ))
  if (length(setdiff(training_unit_ids, unit_ids))) {
    triomics_integration_abort(
      "C_ELIGIBILITY_TRAINING_MAP", "Training ids must belong to the declared integration unit map.",
      key = "training_unit_ids"
    )
  }

  target_unit_ids <- triomics_c01_pick_ids(integration_spec$target_unit_ids, unit_ids)
  target_unit_ids <- triomics_c01_ids(
    target_unit_ids, "target_unit_ids", "C_ELIGIBILITY_TARGET_MAP"
  )
  required_availability <- block_availability[required_views]
  unit_status <- lapply(target_unit_ids, function(unit_id) {
    if (!(unit_id %in% unit_ids)) {
      return(c(status = "refused", reason_code = "unknown_target_unit"))
    }
    present <- vapply(required_availability, function(view) isTRUE(view[[unit_id]]), logical(1))
    if (!any(present)) {
      return(c(status = "refused", reason_code = "all_views_missing"))
    }
    if (!all(present)) {
      return(c(status = "projected_out", reason_code = "required_view_missing"))
    }
    c(status = "eligible", reason_code = "")
  })
  status_matrix <- do.call(rbind, unit_status)
  unit_status <- data.frame(
    unit_id = target_unit_ids,
    status = unname(status_matrix[, "status"]),
    reason_code = unname(status_matrix[, "reason_code"]),
    stringsAsFactors = FALSE
  )
  rownames(unit_status) <- NULL

  training_missing <- vapply(training_unit_ids, function(unit_id) {
    !all(vapply(block_availability[requested_block_ids], function(view) {
      isTRUE(view[[unit_id]])
    }, logical(1)))
  }, logical(1))
  if (any(training_missing)) {
    triomics_integration_abort(
      "C_ELIGIBILITY_TRAINING_MISSING",
      "Every PCA training unit must have every requested predictor block; the integration stage does not intersect rows.",
      key = "training_unit_ids"
    )
  }

  structure(
    list(
      block_ids = requested_block_ids,
      unit_ids = unit_ids,
      training_unit_ids = training_unit_ids,
      target_unit_ids = target_unit_ids,
      required_views = required_views,
      block_feature_keys = block_features,
      block_availability = block_availability,
      unit_status = unit_status,
      study_id = study_id,
      preparation_context_id = root_context,
      representation_id = root_representation
    ),
    class = "IntegrationEligibility"
  )
}
