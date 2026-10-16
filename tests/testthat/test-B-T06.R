b06_two_group_study <- function(two_blocks = FALSE, selection = "declared_unselected") {
  samples <- c("c1", "c2", "c3", "t1", "t2", "t3")
  metadata <- data.frame(
    sample_id = samples,
    group = rep(c("control", "treatment"), each = 3L),
    stringsAsFactors = FALSE
  )
  block_1 <- rbind(
    strong = c(1, 1, 1, 8, 8, 8),
    weak = c(1, 2, 1, 1, 2, 1),
    failed = rep(NA_real_, length(samples))
  )
  colnames(block_1) <- samples
  block_2 <- rbind(
    other_a = c(1, 2, 1, 2, 3, 2),
    other_b = c(2, 1, 2, 1, 2, 1)
  )
  colnames(block_2) <- samples
  assays <- list(block_1 = block_1)
  if (isTRUE(two_blocks)) assays$block_2 <- block_2
  features <- do.call(rbind, lapply(names(assays), function(block_id) {
    data.frame(
      feature_key = rownames(assays[[block_id]]),
      block_id = block_id,
      scale = "linear",
      stringsAsFactors = FALSE
    )
  }))
  study <- list(
    study_id = "synthetic_b_t06",
    preparation_context_id = "b_t06",
    sample_metadata = metadata,
    sample_registry = data.frame(
      sample_id = samples,
      subject_id = NA_character_,
      visit_id = NA_character_,
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(group = c("control", "treatment")),
    feature_registry = features,
    assays = assays,
    assay_hashes = NULL,
    analysis_unit_map = NULL,
    imported_pq = data.frame(feature_key = "strong", p = 0, q = 0)
  )
  if (identical(selection, "declared_unselected")) {
    study$selection <- data.frame(
      block_id = names(assays),
      source_id = paste0(names(assays), "_source"),
      selection_history = "full_unselected",
      inference_gate = "declared_unselected",
      stringsAsFactors = FALSE
    )
  } else if (!is.null(selection)) {
    study$selection <- selection
  }
  study
}

b06_analysis <- function(block_ids = "block_1") {
  list(
    analysis_id = "b_t06_primary",
    block_ids = block_ids,
    preparation_context_id = "b_t06",
    design = list(
      kind = "independent",
      condition = "group",
      condition_type = "factor",
      subject_id = NULL,
      repeated = FALSE,
      representation = "levels",
      reference_level = "control",
      target_level = "treatment",
      between_group = NULL,
      visit_column = NULL,
      reference_visit = NULL,
      target_visit = NULL,
      covariates = list(),
      missing_visits = "error",
      time_column = NULL,
      time_unit = NULL,
      time_origin = NULL,
      spline_knots = list(),
      spline_boundary = list(),
      residual_contract_ref = NULL,
      mean_weights = NULL,
      interactions = list()
    )
  )
}

b06_statistics <- function(rank_sensitivity = FALSE, selection_validity = "qualified",
                           family_scope = "one_scalar_contrast_all_retained_features",
                           dependence_justification = "Synthetic fixture; no scientific dependence claim.",
                           effect_threshold = 0.5) {
  list(
    method = "limma",
    contrasts = list(list(
      contrast_id = "treatment_minus_control",
      weights = c("condition.control" = -1, "condition.treatment" = 1),
      interpretation = "Synthetic treatment minus control."
    )),
    trend = FALSE,
    robust = FALSE,
    q_threshold = 0.05,
    effect_threshold = effect_threshold,
    effect_unit = "group_difference",
    family_scope = family_scope,
    selection_validity = selection_validity,
    selection_justification = "Synthetic fixture; inference is not validated.",
    dependence_justification = dependence_justification,
    rank_sensitivity = rank_sensitivity,
    count_metadata = NULL,
    fallback = "none"
  )
}

b06_fit <- function(study = b06_two_group_study(), analysis = b06_analysis(),
                    statistics = b06_statistics()) {
  resolved <- triomics_resolve_analysis(study, analysis)
  list(resolved = resolved, fit = triomics_fit_block_models(resolved, statistics))
}

b06_paired_study <- function() {
  subjects <- c("A", "B", "C", "D")
  samples <- as.vector(rbind(paste0(subjects, "_b"), paste0(subjects, "_f")))
  baseline <- c(0, 0, 0, 0)
  followup <- c(0, 1, 1, 2)
  assay <- rbind(
    tied_delta = as.vector(rbind(baseline, followup)),
    other_delta = as.vector(rbind(c(0, 1, 1, 2), c(2, 2, 3, 5)))
  )
  colnames(assay) <- samples
  metadata <- data.frame(
    sample_id = samples,
    subject = rep(subjects, each = 2L),
    visit = rep(c("baseline", "followup"), length(subjects)),
    age = rep(c(20, 30, 40, 50), each = 2L),
    stringsAsFactors = FALSE
  )
  list(
    study_id = "synthetic_b_t06_paired",
    preparation_context_id = "b_t06_paired",
    sample_metadata = metadata,
    sample_registry = data.frame(
      sample_id = samples,
      subject_id = metadata$subject,
      visit_id = metadata$visit,
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(visit = c("baseline", "followup")),
    feature_registry = data.frame(
      feature_key = rownames(assay),
      block_id = "block_1",
      scale = "linear",
      stringsAsFactors = FALSE
    ),
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = NULL,
    selection = data.frame(
      block_id = "block_1",
      source_id = "paired_measurements",
      selection_history = "full_unselected",
      inference_gate = "declared_unselected",
      stringsAsFactors = FALSE
    )
  )
}

b06_paired_analysis <- function(adjusted = FALSE) {
  covariates <- if (adjusted) list(list(variable = "age", form = "between")) else list()
  list(
    analysis_id = "b_t06_paired",
    block_ids = "block_1",
    preparation_context_id = "b_t06_paired",
    design = list(
      kind = "paired",
      condition = "visit",
      condition_type = "factor",
      subject_id = "subject",
      repeated = TRUE,
      representation = "delta",
      reference_level = "baseline",
      target_level = "followup",
      between_group = NULL,
      visit_column = "visit",
      reference_visit = "baseline",
      target_visit = "followup",
      covariates = covariates,
      missing_visits = "error",
      time_column = NULL,
      time_unit = NULL,
      time_origin = NULL,
      spline_knots = list(),
      spline_boundary = list(),
      residual_contract_ref = NULL,
      mean_weights = NULL,
      interactions = list()
    )
  )
}

b06_paired_statistics <- function(adjusted = FALSE) {
  weights <- if (adjusted) list(intercept = 1, modifier.age = 0) else list(intercept = 1)
  list(
    method = "limma",
    contrasts = list(list(
      contrast_id = "followup_minus_baseline",
      weights = weights,
      interpretation = "Synthetic subject delta."
    )),
    trend = FALSE,
    robust = FALSE,
    q_threshold = 0.05,
    effect_threshold = 0.5,
    effect_unit = "delta_difference",
    family_scope = "one_scalar_contrast_all_retained_features",
    selection_validity = "qualified",
    selection_justification = "Synthetic fixture; inference is not validated.",
    dependence_justification = "Synthetic fixture; no scientific dependence claim.",
    rank_sensitivity = TRUE,
    count_metadata = NULL,
    fallback = "none"
  )
}

testthat::test_that("B-T06-AC1 complete families retain failed members internally", {
  fitted <- b06_fit()
  result <- fitted$fit$blocks$block_1$contrasts$treatment_minus_control
  family <- result$family
  failed <- Filter(function(row) identical(row$feature_key, "failed"), result$rows)[[1L]]

  testthat::expect_identical(family$n_declared, 3L)
  testthat::expect_identical(family$n_failed, 1L)
  testthat::expect_identical(family$n_evaluated, 3L)
  testthat::expect_identical(sum(family$dispositions), family$n_declared)
  testthat::expect_identical(family$internal_p[[match("failed", family$member_ids)]], 1)
  testthat::expect_null(failed$p)
  testthat::expect_null(failed$q)
})

testthat::test_that("B-T06-AC2 blocks and imported p/q sidecars do not change a fixed family", {
  one_block <- b06_fit()
  two_blocks <- b06_fit(
    study = b06_two_group_study(two_blocks = TRUE),
    analysis = b06_analysis(c("block_1", "block_2"))
  )
  withheld_second_block <- data.frame(
    block_id = c("block_1", "block_2"),
    source_id = c("block_1_source", "block_2_source"),
    selection_history = c("full_unselected", "unknown"),
    inference_gate = c("declared_unselected", "withheld"),
    stringsAsFactors = FALSE
  )
  mixed_inventory <- b06_fit(
    study = b06_two_group_study(two_blocks = TRUE, selection = withheld_second_block),
    analysis = b06_analysis(c("block_1", "block_2"))
  )
  q_one <- one_block$fit$blocks$block_1$contrasts$treatment_minus_control$family$internal_q
  q_two <- two_blocks$fit$blocks$block_1$contrasts$treatment_minus_control$family$internal_q

  testthat::expect_equal(q_one, q_two)
  testthat::expect_identical(
    one_block$fit$blocks$block_1$contrasts$treatment_minus_control$family$member_ids,
    two_blocks$fit$blocks$block_1$contrasts$treatment_minus_control$family$member_ids
  )
  block_1_mixed <- mixed_inventory$fit$blocks$block_1$contrasts$treatment_minus_control
  block_2_mixed <- mixed_inventory$fit$blocks$block_2$contrasts$treatment_minus_control
  testthat::expect_true(block_1_mixed$family$q_published)
  testthat::expect_equal(q_one, block_1_mixed$family$internal_q)
  testthat::expect_false(block_2_mixed$family$q_published)
  testthat::expect_true(all(vapply(block_2_mixed$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_null(one_block$fit$blocks$block_1$contrasts$treatment_minus_control$rows[[1L]]$imported_p)
  testthat::expect_null(one_block$fit$blocks$block_1$contrasts$treatment_minus_control$rows[[1L]]$imported_q)
})

testthat::test_that("B-T06-AC3 invalid inventory, scope, or dependence withholds q", {
  unknown_inventory <- data.frame(
    block_id = c("block_1", "block_1"),
    source_id = c("outcome_selected_measurements", "unknown_measurements"),
    selection_history = c("outcome_selected", "unknown"),
    inference_gate = c("withheld", "withheld"),
    stringsAsFactors = FALSE
  )
  withheld <- b06_fit(
    study = b06_two_group_study(selection = unknown_inventory),
    statistics = b06_statistics()
  )$fit$blocks$block_1$contrasts$treatment_minus_control
  missing_inventory_study <- b06_two_group_study()
  missing_inventory_study$selection <- NULL
  missing_inventory <- b06_fit(study = missing_inventory_study)$fit$
    blocks$block_1$contrasts$treatment_minus_control
  invalid_scope <- b06_fit(statistics = b06_statistics(family_scope = "selected_hits_only"))$fit$
    blocks$block_1$contrasts$treatment_minus_control
  invalid_dependence <- b06_fit(
    statistics = b06_statistics(dependence_justification = "unqualified")
  )$fit$blocks$block_1$contrasts$treatment_minus_control
  invalid_selection <- b06_fit(
    statistics = b06_statistics(selection_validity = "unverified")
  )$fit$blocks$block_1$contrasts$treatment_minus_control
  descriptive <- b06_fit(
    study = missing_inventory_study,
    statistics = b06_statistics(selection_validity = "descriptive_only")
  )$fit$blocks$block_1$contrasts$treatment_minus_control
  invalid_model <- b06_statistics()
  invalid_model$method <- "unsupported_model"
  model_error <- testthat::capture_error(b06_fit(statistics = invalid_model))

  testthat::expect_false(withheld$family$validation$eligible)
  testthat::expect_true(all(vapply(withheld$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_false(missing_inventory$family$validation$eligible)
  testthat::expect_identical(missing_inventory$family$validation$status, "withheld")
  testthat::expect_true("selection_inventory_missing" %in% missing_inventory$family$validation$reasons)
  testthat::expect_true(all(vapply(missing_inventory$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_false(missing_inventory$family$q_published)
  failed_row <- Filter(function(row) identical(row$feature_key, "failed"), missing_inventory$rows)[[1L]]
  testthat::expect_identical(failed_row$inferential_status, "not_estimable")
  testthat::expect_null(failed_row$q)
  descriptive_row <- Filter(function(row) identical(row$feature_key, "strong"), descriptive$rows)[[1L]]
  testthat::expect_identical(descriptive$family$validation$status, "withheld")
  testthat::expect_true("selection_inventory_missing" %in% descriptive$family$validation$reasons)
  testthat::expect_identical(descriptive_row$inferential_status, "descriptive_only")
  testthat::expect_null(descriptive_row$q)
  testthat::expect_false(invalid_scope$family$validation$eligible)
  testthat::expect_true(all(vapply(invalid_scope$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_false(invalid_dependence$family$validation$eligible)
  testthat::expect_true(all(vapply(invalid_dependence$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_false(invalid_selection$family$validation$eligible)
  testthat::expect_true(all(vapply(invalid_selection$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_s3_class(model_error, "triomics_error")
  testthat::expect_identical(model_error$code, "B_DESIGN_UNSUPPORTED")
})

testthat::test_that("B-T06 effect threshold filters nominations without changing q", {
  low_threshold <- b06_fit(statistics = b06_statistics(effect_threshold = 0.5))$
    fit$blocks$block_1$contrasts$treatment_minus_control
  high_threshold <- b06_fit(statistics = b06_statistics(effect_threshold = 9))$
    fit$blocks$block_1$contrasts$treatment_minus_control

  testthat::expect_equal(low_threshold$family$internal_q, high_threshold$family$internal_q)
  testthat::expect_setequal(vapply(low_threshold$nominations, `[[`, character(1), "feature_key"), "strong")
  testthat::expect_length(high_threshold$nominations, 0L)
  testthat::expect_identical(low_threshold$nominations[[1L]]$filter_role, "reporting_only")
})

testthat::test_that("B-T06-AC4 rank sensitivity records ties and explicit approximation policies", {
  study <- b06_two_group_study()
  assay <- study$assays$block_1
  assay["strong", ] <- c(1, 1, 2, 8, 8, 9)
  assay["weak", ] <- c(0, 0, 1, 0, 1, 1)
  study$assays$block_1 <- assay
  sensitivity <- b06_fit(study, statistics = b06_statistics(rank_sensitivity = TRUE))$
    fit$blocks$block_1$contrasts$treatment_minus_control$sensitivity
  strong <- Filter(function(row) identical(row$feature_key, "strong"), sensitivity$features)[[1L]]
  direct <- suppressWarnings(stats::wilcox.test(
    c(8, 8, 9), c(1, 1, 2), exact = FALSE, correct = TRUE, alternative = "two.sided"
  ))

  testthat::expect_identical(sensitivity$status, "complete")
  testthat::expect_identical(sensitivity$exact, FALSE)
  testthat::expect_identical(sensitivity$continuity_correction, TRUE)
  testthat::expect_equal(strong$p, direct$p.value)
  testthat::expect_equal(strong$n_reference, 3L)
  testthat::expect_equal(strong$n_target, 3L)
  testthat::expect_identical(strong$n_ties, 2L)
  testthat::expect_identical(sensitivity$zero_policy, "report group zeros; do not drop observations")
  testthat::expect_identical(sensitivity$substitutes_for_primary, FALSE)

  treatment_mean <- b06_statistics(rank_sensitivity = TRUE)
  treatment_mean$contrasts[[1L]]$weights <- c("condition.control" = 0, "condition.treatment" = 1)
  incompatible <- b06_fit(statistics = treatment_mean)$fit$
    blocks$block_1$contrasts$treatment_minus_control$sensitivity
  testthat::expect_identical(incompatible$status, "unavailable")
  testthat::expect_identical(
    incompatible$reason,
    "rank_sensitivity_contrast_is_not_an_unadjusted_two_group_test"
  )
})

testthat::test_that("B-T06 two-group rank eligibility is invariant to finite contrast scale", {
  block <- list(
    feature_keys = "feature",
    assay = matrix(c(1, 1, 1, 8, 8, 8), nrow = 1L, dimnames = list("feature", NULL)),
    groups = c("control", "control", "control", "treatment", "treatment", "treatment")
  )
  plan <- list(
    reference_level = "control",
    target_level = "treatment",
    levels = c("control", "treatment")
  )
  rank_for <- function(weights) triomics_b06_two_group_rank(block, plan, list(weights = weights))
  canonical <- rank_for(c("condition.control" = -1, "condition.treatment" = 1))
  incompatible <- rank_for(c("condition.control" = 0, "condition.treatment" = 1e-9))
  tiny_valid <- rank_for(c("condition.control" = -1e-9, "condition.treatment" = 1e-9))
  reversed <- rank_for(c("condition.control" = 1, "condition.treatment" = -1))
  extreme <- rank_for(c("condition.control" = -1e308, "condition.treatment" = 1e308))
  direct <- stats::wilcox.test(
    c(8, 8, 8), c(1, 1, 1), exact = FALSE, correct = TRUE, alternative = "two.sided"
  )

  testthat::expect_identical(canonical$status, "complete")
  testthat::expect_identical(canonical$direction_sign, "positive_target_minus_reference")
  testthat::expect_equal(canonical$features[[1L]]$p, direct$p.value)
  testthat::expect_null(incompatible)
  testthat::expect_identical(tiny_valid$status, "complete")
  testthat::expect_identical(tiny_valid$direction_sign, "positive_target_minus_reference")
  testthat::expect_equal(tiny_valid$features[[1L]]$p, canonical$features[[1L]]$p)
  testthat::expect_identical(reversed$status, "complete")
  testthat::expect_identical(reversed$direction_sign, "negative_target_minus_reference")
  testthat::expect_equal(reversed$features[[1L]]$p, canonical$features[[1L]]$p)
  testthat::expect_false(is.null(extreme))
  if (!is.null(extreme)) {
    testthat::expect_identical(extreme$status, "complete")
    testthat::expect_identical(extreme$direction_sign, "positive_target_minus_reference")
    testthat::expect_equal(extreme$features[[1L]]$p, canonical$features[[1L]]$p)
  }
})

testthat::test_that("B-T06 paired rank sensitivity reports omitted zeros and refuses adjusted refits", {
  paired <- b06_fit(
    study = b06_paired_study(),
    analysis = b06_paired_analysis(),
    statistics = b06_paired_statistics()
  )$fit$blocks$block_1$contrasts$followup_minus_baseline$sensitivity
  tied <- Filter(function(row) identical(row$feature_key, "tied_delta"), paired$features)[[1L]]
  direct <- suppressWarnings(stats::wilcox.test(c(1, 1, 2), exact = FALSE, correct = TRUE))
  adjusted <- b06_fit(
    study = b06_paired_study(),
    analysis = b06_paired_analysis(adjusted = TRUE),
    statistics = b06_paired_statistics(adjusted = TRUE)
  )$fit$blocks$block_1$contrasts$followup_minus_baseline$sensitivity

  testthat::expect_identical(paired$method, "wilcoxon_signed_rank")
  testthat::expect_identical(paired$exact, FALSE)
  testthat::expect_identical(paired$continuity_correction, TRUE)
  testthat::expect_equal(tied$p, direct$p.value)
  testthat::expect_identical(tied$n_zero_differences, 1L)
  testthat::expect_identical(tied$n_ties, 1L)
  testthat::expect_match(paired$zero_policy, "omit exact zero differences")
  testthat::expect_identical(adjusted$status, "unavailable")
  testthat::expect_null(adjusted$features)
})
