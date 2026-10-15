triomics_b06_wilcoxon_p <- function(expr) {
  tested <- tryCatch(suppressWarnings(force(expr)), error = function(error) NULL)
  if (is.null(tested) || !is.numeric(tested$p.value) || length(tested$p.value) != 1L ||
      is.na(tested$p.value) || !is.finite(tested$p.value)) {
    return(NULL)
  }
  unname(as.numeric(tested$p.value))
}

triomics_b06_paired_rank <- function(block, weights) {
  orientation <- 1
  if ("intercept" %in% names(weights) && is.finite(weights[["intercept"]]) &&
      weights[["intercept"]] < 0) orientation <- -1
  features <- lapply(seq_along(block$feature_keys), function(index) {
    values <- as.numeric(block$assay[index, ])
    values <- values[is.finite(values)]
    oriented <- orientation * values
    zero <- oriented == 0
    nonzero <- oriented[!zero]
    absolute <- abs(nonzero)
    tied_values <- unique(absolute[duplicated(absolute)])
    p <- if (length(nonzero)) {
      triomics_b06_wilcoxon_p(stats::wilcox.test(
        nonzero, mu = 0, exact = FALSE, correct = TRUE, alternative = "two.sided"
      ))
    } else {
      NULL
    }
    list(
      feature_key = block$feature_keys[[index]],
      p = p,
      n = as.integer(length(values)),
      n_zero_differences = as.integer(sum(zero)),
      n_tied_nonzero_absolute = as.integer(sum(duplicated(absolute))),
      n_ties = as.integer(sum(duplicated(absolute))),
      n_tied_absolute_groups = as.integer(length(tied_values))
    )
  })
  list(
    status = "complete",
    method = "wilcoxon_signed_rank",
    exact = FALSE,
    continuity_correction = TRUE,
    zero_policy = "omit exact zero differences from signed ranks and report their count",
    tie_policy = "use asymptotic tie correction and report tied nonzero absolute differences",
    direction = "registered_contrast",
    substitutes_for_primary = FALSE,
    equivalent_to_moderated_test = FALSE,
    features = features
  )
}

triomics_b06_two_group_rank <- function(block, plan, contrast) {
  reference <- plan$reference_level
  target <- plan$target_level
  if (is.null(reference) || is.null(target) || identical(reference, target) ||
      length(plan$levels) != 2L || !setequal(as.character(plan$levels), c(reference, target)) ||
      is.null(block$groups) || length(block$groups) != ncol(block$assay) ||
      !is.list(contrast) || !is.list(contrast$weights) && !is.numeric(contrast$weights)) {
    return(NULL)
  }
  weights <- contrast$weights
  if (is.list(weights)) weights <- unlist(weights, use.names = TRUE)
  expected_columns <- c(paste0("condition.", reference), paste0("condition.", target))
  if (is.null(names(weights)) || anyDuplicated(names(weights)) ||
      length(weights) != length(expected_columns) || !setequal(names(weights), expected_columns) ||
      any(!is.finite(as.numeric(weights)))) {
    return(NULL)
  }
  reference_weight <- as.numeric(weights[[paste0("condition.", reference)]])
  target_weight <- as.numeric(weights[[paste0("condition.", target)]])
  if (target_weight == 0 || reference_weight != -target_weight) return(NULL)
  features <- lapply(seq_along(block$feature_keys), function(index) {
    values <- as.numeric(block$assay[index, ])
    groups <- as.character(block$groups)
    reference_values <- values[groups == reference & is.finite(values)]
    target_values <- values[groups == target & is.finite(values)]
    pooled <- c(reference_values, target_values)
    value_counts <- if (length(pooled)) table(pooled) else integer()
    p <- if (length(reference_values) && length(target_values)) {
      triomics_b06_wilcoxon_p(stats::wilcox.test(
        target_values, reference_values, exact = FALSE, correct = TRUE,
        alternative = "two.sided"
      ))
    } else {
      NULL
    }
    list(
      feature_key = block$feature_keys[[index]],
      p = p,
      n_reference = as.integer(length(reference_values)),
      n_target = as.integer(length(target_values)),
      n_ties = as.integer(sum(duplicated(pooled))),
      n_tied_observations = as.integer(sum(value_counts[value_counts > 1L])),
      n_zero_reference = as.integer(sum(reference_values == 0)),
      n_zero_target = as.integer(sum(target_values == 0))
    )
  })
  list(
    status = "complete",
    method = "wilcoxon_rank_sum",
    exact = FALSE,
    continuity_correction = TRUE,
    zero_policy = "report group zeros; do not drop observations",
    tie_policy = "use asymptotic tie correction and report pooled ties",
    direction = "registered_target_minus_reference",
    direction_sign = if (target_weight > 0) "positive_target_minus_reference" else "negative_target_minus_reference",
    substitutes_for_primary = FALSE,
    equivalent_to_moderated_test = FALSE,
    features = features
  )
}

triomics_b06_unavailable_rank <- function(reason) {
  list(
    status = "unavailable",
    method = NULL,
    reason = reason,
    substitutes_for_primary = FALSE,
    equivalent_to_moderated_test = FALSE,
    features = NULL
  )
}

triomics_b06_rank_sensitivity <- function(resolved_analysis, statistics_spec, claim_id,
                                           block_id, current = NULL) {
  if (!isTRUE(statistics_spec$rank_sensitivity)) return(list(status = "not_requested"))
  block <- resolved_analysis$blocks[[block_id]]
  plan <- resolved_analysis$model_plan
  contrasts <- statistics_spec$contrasts
  contrast <- Filter(function(item) identical(item$contrast_id, claim_id), contrasts)
  if (length(contrast) != 1L) {
    return(triomics_b06_unavailable_rank("rank_sensitivity_contrast_not_unique"))
  }
  contrast <- contrast[[1L]]

  if (identical(resolved_analysis$lane, "b_t03")) {
    eligible <- identical(plan$estimand, "within_person_change") &&
      identical(resolved_analysis$representation_id, "delta") &&
      length(plan$covariates) == 0L &&
      identical(plan$column_ids, "intercept")
    if (!eligible) {
      return(triomics_b06_unavailable_rank(
        "rank_sensitivity_is_unavailable_for_adjusted_or_difference_in_change"
      ))
    }
    weights <- triomics_b02_weight_map(contrast, plan$column_ids)
    return(triomics_b06_paired_rank(block, weights))
  }

  independent_two_group <- is.null(resolved_analysis$lane) &&
    identical(resolved_analysis$representation, "levels") &&
    identical(plan$formula, "~0 + condition") &&
    length(plan$levels) == 2L && is.null(plan$covariates)
  if (identical(resolved_analysis$lane, "b_t02")) {
    return(triomics_b06_unavailable_rank(
      "rank_sensitivity_is_unavailable_for_adjusted_or_non_two_group_designs"
    ))
  }
  if (!independent_two_group) {
    return(triomics_b06_unavailable_rank("rank_sensitivity_is_unavailable_for_unsupported_design"))
  }
  sensitivity <- triomics_b06_two_group_rank(block, plan, contrast)
  if (is.null(sensitivity)) {
    return(triomics_b06_unavailable_rank("rank_sensitivity_contrast_is_not_an_unadjusted_two_group_test"))
  }
  sensitivity
}
