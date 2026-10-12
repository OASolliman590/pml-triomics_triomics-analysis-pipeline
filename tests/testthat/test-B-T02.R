expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

expect_close <- function(actual, expected) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  testthat::expect_identical(length(actual), length(expected))
  ok <- abs(actual - expected) <= 1e-8 + 1e-6 * abs(expected)
  testthat::expect_true(all(ok), info = paste(actual, expected, sep = " != "))
}

b02_study <- function(assay, metadata, levels = list(), scale = "linear") {
  if (is.null(rownames(assay))) rownames(assay) <- paste0("F", seq_len(nrow(assay)))
  testthat::expect_false(is.null(colnames(assay)))
  list(
    study_id = "synthetic_b_t02",
    preparation_context_id = "b_t02",
    sample_metadata = metadata,
    sample_registry = data.frame(
      sample_id = as.character(metadata$sample_id),
      subject_id = if ("subject" %in% names(metadata)) as.character(metadata$subject) else NA_character_,
      visit_id = if ("visit" %in% names(metadata)) as.character(metadata$visit) else NA_character_,
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = levels,
    feature_registry = data.frame(
      feature_key = rownames(assay),
      block_id = rep("block_1", nrow(assay)),
      scale = rep(scale, nrow(assay)),
      stringsAsFactors = FALSE
    ),
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
}

b02_analysis <- function(condition, condition_type, representation, covariates = list(),
                         subject_id = NULL, visit_column = NULL, mean_weights = NULL,
                         reference_level = NULL, target_level = NULL, kind = "independent",
                         repeated = FALSE, block_ids = "block_1") {
  list(
    analysis_id = "b_t02_primary",
    block_ids = block_ids,
    preparation_context_id = "b_t02",
    design = list(
      kind = kind,
      condition = condition,
      condition_type = condition_type,
      subject_id = subject_id,
      repeated = repeated,
      representation = representation,
      reference_level = reference_level,
      target_level = target_level,
      between_group = NULL,
      visit_column = visit_column,
      reference_visit = NULL,
      target_visit = NULL,
      covariates = covariates,
      missing_visits = "error",
      time_column = NULL,
      time_unit = NULL,
      time_origin = NULL,
      spline_knots = list(),
      spline_boundary = list(),
      residual_contract_ref = NULL,
      mean_weights = mean_weights,
      interactions = list()
    )
  )
}

b02_statistics <- function(contrasts, effect_unit = "group_difference", robust = FALSE,
                           rank_sensitivity = FALSE, selection_validity = "qualified",
                           claims = NULL, method = "limma") {
  stats <- list(
    method = method,
    contrasts = contrasts,
    trend = FALSE,
    robust = robust,
    q_threshold = 0.05,
    effect_threshold = 0.5,
    effect_unit = effect_unit,
    family_scope = "one_scalar_contrast_all_retained_features",
    selection_validity = selection_validity,
    selection_justification = "Synthetic fixture; inference is not validated.",
    dependence_justification = "Synthetic fixture; no scientific dependence claim.",
    rank_sensitivity = rank_sensitivity,
    count_metadata = NULL,
    fallback = "none"
  )
  if (!is.null(claims)) stats$claims <- claims
  stats
}

b02_contrast <- function(contrast_id, weights, interpretation = "Declared column weights.") {
  list(contrast_id = contrast_id, weights = weights, interpretation = interpretation)
}

b02_fit <- function(study, analysis, statistics) {
  resolved <- triomics_resolve_analysis(study, analysis)
  list(resolved = resolved, fit = triomics_fit_block_models(resolved, statistics))
}

b02_rows <- function(fit, contrast, block = "block_1") {
  fit$blocks[[block]]$contrasts[[contrast]]$rows
}

b02_row <- function(fit, feature, contrast, block = "block_1") {
  rows <- b02_rows(fit, contrast, block)
  hit <- which(vapply(rows, function(row) identical(row$feature_key, feature), logical(1)))
  testthat::expect_identical(length(hit), 1L)
  rows[[hit]]
}

b02_groups <- function() {
  groups <- rep(c("low", "mid", "high"), each = 3L)
  samples <- paste0("S", seq_along(groups))
  list(
    groups = groups,
    samples = samples,
    metadata = data.frame(sample_id = samples, group = groups, stringsAsFactors = FALSE),
    levels = list(group = c("low", "mid", "high"))
  )
}

testthat::test_that("three-group contrasts use declared model-column weights", {
  spec <- b02_groups()
  assay <- matrix(c(1, 2, 3, 3, 4, 5, 5, 6, 7), nrow = 1L, dimnames = list("F", spec$samples))
  study <- b02_study(assay, spec$metadata, spec$levels)
  analysis <- b02_analysis("group", "factor", "levels", reference_level = "low", target_level = "high")
  contrasts <- list(
    b02_contrast("mid_minus_low", list(condition.low = -1, condition.mid = 1, condition.high = 0)),
    b02_contrast("high_and_mid_versus_low", list(condition.low = -1, condition.mid = 0.5, condition.high = 0.5),
                 interpretation = "stop('evaluated')")
  )
  fitted <- b02_fit(study, analysis, b02_statistics(contrasts))
  testthat::expect_identical(fitted$resolved$lane, "b_t02")
  testthat::expect_identical(
    fitted$resolved$model_plan$column_ids,
    c("condition.low", "condition.mid", "condition.high")
  )
  testthat::expect_false(any(grepl("control|treatment", fitted$resolved$model_plan$column_ids)))
  testthat::expect_match(fitted$resolved$model_plan$formula, "condition.low", fixed = TRUE)
  testthat::expect_false(grepl("eval|parse|control|treatment", fitted$resolved$model_plan$formula))

  mid <- b02_row(fitted$fit, "F", "mid_minus_low")
  blend <- b02_row(fitted$fit, "F", "high_and_mid_versus_low")
  expect_close(mid$effect, 2)
  expect_close(mid$ordinary_se, sqrt(2 / 3))
  testthat::expect_equal(mid$ordinary_df, 6)
  expect_close(blend$effect, 3)
  expect_close(blend$ordinary_se, sqrt(0.5))
  testthat::expect_equal(blend$ordinary_df, 6)
  testthat::expect_identical(mid$effect_unit, "group_difference")
  testthat::expect_identical(blend$reference_direction_status, "declared_weight_direction")
  expect_close(
    unlist(fitted$fit$blocks$block_1$contrasts$high_and_mid_versus_low$reference_direction$weights),
    c(condition.low = -1, condition.mid = 0.5, condition.high = 0.5)
  )
  testthat::expect_false(identical(mid$effect, blend$effect))

  expressed <- b02_statistics(list(b02_contrast(
    "bad",
    list(condition.low = "stop('evaluated')", condition.mid = 1, condition.high = 0)
  )))
  err <- expect_code(b02_fit(study, analysis, expressed), "B_DESIGN_CONTRAST")
  testthat::expect_false(grepl("evaluated", conditionMessage(err), fixed = TRUE))

  set.seed(11)
  values <- rbind(F = c(1, 2, 3, 3, 4, 5, 5, 6, 7), matrix(stats::rnorm(12L * 9L, sd = 0.4), nrow = 12L))
  rownames(values) <- c("F", paste0("N", seq_len(12L)))
  colnames(values) <- spec$samples
  moderated <- b02_fit(b02_study(values, spec$metadata, spec$levels), analysis, b02_statistics(contrasts))
  design <- stats::model.matrix(~ 0 + factor(spec$groups, levels = c("low", "mid", "high")))
  colnames(design) <- c("condition.low", "condition.mid", "condition.high")
  contrast <- matrix(c(-1, 1, 0), ncol = 1L, dimnames = list(colnames(design), "mid_minus_low"))
  reference <- limma::eBayes(
    limma::contrasts.fit(limma::lmFit(values, design), contrast),
    trend = FALSE, robust = FALSE
  )
  rows <- b02_rows(moderated$fit, "mid_minus_low")
  expect_close(vapply(rows, function(row) row$moderated_se, numeric(1)), as.numeric(sqrt(reference$s2.post) * reference$stdev.unscaled))
  expect_close(vapply(rows, function(row) row$statistic, numeric(1)), as.numeric(reference$t))
  expect_close(vapply(rows, function(row) row$p, numeric(1)), as.numeric(reference$p.value))
  testthat::expect_false(any(vapply(rows, function(row) identical(row$inferential_status, "eligible"), logical(1))))
})

testthat::test_that("continuous terms keep predictor-unit weights and are not coerced", {
  samples <- paste0("S", 1:4)
  metadata <- data.frame(sample_id = samples, dose = c(0, 1, 2, 3), stringsAsFactors = FALSE)
  assay <- matrix(c(1, 2, 5, 6), nrow = 1L, dimnames = list("F", samples))
  study <- b02_study(assay, metadata, scale = "log2")
  analysis <- b02_analysis("dose", "continuous", "levels")
  slope <- b02_contrast("dose_slope", list(intercept = 0, continuous.dose = 1))
  doubled <- b02_contrast("twice_dose_slope", list(intercept = 0, continuous.dose = 2))
  fitted <- b02_fit(study, analysis, b02_statistics(list(slope, doubled), effect_unit = "per_unit_beta"))
  testthat::expect_identical(
    fitted$resolved$model_plan$column_ids,
    c("intercept", "continuous.dose")
  )
  testthat::expect_identical(fitted$resolved$model_plan$coefficient_kind, "continuous")
  expect_close(b02_row(fitted$fit, "F", "dose_slope")$effect, 1.8)
  expect_close(b02_row(fitted$fit, "F", "dose_slope")$ordinary_se, sqrt(0.08))
  testthat::expect_equal(b02_row(fitted$fit, "F", "dose_slope")$ordinary_df, 2)
  expect_close(b02_row(fitted$fit, "F", "twice_dose_slope")$effect, 3.6)
  testthat::expect_identical(b02_row(fitted$fit, "F", "dose_slope")$effect_unit, "per_unit_beta")
  testthat::expect_identical(b02_row(fitted$fit, "F", "dose_slope")$direction_status, "not_applicable")
  testthat::expect_match(b02_row(fitted$fit, "F", "dose_slope")$direction_reason, "continuous")
  expect_code(
    b02_fit(study, analysis, b02_statistics(list(slope), effect_unit = "log2_fold_change")),
    "B_DESIGN_EFFECT_UNIT"
  )

  labels <- data.frame(sample_id = samples, dose = c("control", "treatment", "control", "treatment"), stringsAsFactors = FALSE)
  labelled <- b02_study(assay, labels, levels = list(dose = c("control", "treatment")))
  expect_code(
    triomics_resolve_analysis(labelled, b02_analysis("dose", "continuous", "levels")),
    "B_DESIGN_UNSUPPORTED"
  )
  numeric_groups <- data.frame(sample_id = samples, dose = c(0, 1, 2, 3), stringsAsFactors = FALSE)
  expect_code(
    triomics_resolve_analysis(
      b02_study(assay, numeric_groups, levels = list(dose = c("0", "1", "2", "3"))),
      b02_analysis("dose", "factor", "levels", reference_level = "0", target_level = "3")
    ),
    "B_DESIGN_UNSUPPORTED"
  )

  adjusted <- data.frame(
    sample_id = paste0("S", 1:6),
    dose = c(0, 1, 2, 3, 4, 5),
    age = c(1, 0, 1, 2, 0, 1),
    stringsAsFactors = FALSE
  )
  response <- 1 + 2 * adjusted$dose + 3 * adjusted$age + c(0.2, -0.1, 0.3, -0.2, 0.1, -0.3)
  adjusted_assay <- matrix(response, nrow = 1L, dimnames = list("F", adjusted$sample_id))
  adjusted_study <- b02_study(adjusted_assay, adjusted)
  adjusted_analysis <- b02_analysis(
    "dose", "continuous", "levels",
    covariates = list(list(variable = "age", role = "confounder", form = "level", rationale = "Declared adjustment."))
  )
  adjusted_fit <- b02_fit(
    adjusted_study, adjusted_analysis,
    b02_statistics(list(b02_contrast(
      "dose_slope",
      list(intercept = 0, continuous.dose = 1, covariate.age = 0)
    )), effect_unit = "per_unit_beta")
  )
  oracle <- stats::lm(response ~ adjusted$dose + adjusted$age)
  expect_close(b02_row(adjusted_fit$fit, "F", "dose_slope")$effect, unname(stats::coef(oracle)[[2L]]))
  expect_close(b02_row(adjusted_fit$fit, "F", "dose_slope")$ordinary_se, unname(summary(oracle)$coefficients[2L, 2L]))
  testthat::expect_identical(
    adjusted_fit$resolved$model_plan$column_ids,
    c("intercept", "continuous.dose", "covariate.age")
  )
})

testthat::test_that("aliased covariates and absent levels fail without dropping contrasts", {
  groups <- rep(c("A", "B", "C"), each = 3L)
  samples <- paste0("S", seq_along(groups))
  batch <- as.numeric(groups == "B")
  metadata <- data.frame(sample_id = samples, group = groups, batch = batch, stringsAsFactors = FALSE)
  assay <- matrix(c(1, 2, 3, 5, 6, 7, 2, 3, 4), nrow = 1L, dimnames = list("F", samples))
  study <- b02_study(assay, metadata, list(group = c("A", "B", "C")))
  analysis <- b02_analysis(
    "group", "factor", "levels",
    covariates = list(list(variable = "batch", role = "technical_nuisance", form = "level", rationale = "Aliased with B.")),
    reference_level = "A", target_level = "B"
  )
  contrasts <- list(
    b02_contrast("B_minus_A", list(condition.A = -1, condition.B = 1, condition.C = 0, covariate.batch = 0)),
    b02_contrast("C_minus_A", list(condition.A = -1, condition.B = 0, condition.C = 1, covariate.batch = 0))
  )
  fitted <- b02_fit(study, analysis, b02_statistics(contrasts))
  testthat::expect_false(is.null(fitted$fit$blocks$block_1$contrasts$B_minus_A))
  aliased <- b02_row(fitted$fit, "F", "B_minus_A")
  kept <- b02_row(fitted$fit, "F", "C_minus_A")
  testthat::expect_identical(aliased$estimation_status, "failed")
  testthat::expect_identical(aliased$estimation_reason, "contrast_not_estimable")
  testthat::expect_null(aliased$effect)
  testthat::expect_null(aliased$p)
  testthat::expect_false(identical(aliased$effect, 4))
  expect_close(kept$effect, 1)
  expect_close(kept$ordinary_se, sqrt(2 / 3))
  testthat::expect_equal(kept$ordinary_df, 6)
  testthat::expect_identical(kept$estimation_status, "estimated")

  absent_groups <- rep(c("low", "mid"), each = 3L)
  absent_samples <- paste0("S", seq_along(absent_groups))
  absent_meta <- data.frame(sample_id = absent_samples, group = absent_groups, stringsAsFactors = FALSE)
  absent_assay <- matrix(c(1, 2, 3, 3, 4, 5), nrow = 1L, dimnames = list("F", absent_samples))
  absent_study <- b02_study(absent_assay, absent_meta, list(group = c("low", "mid", "high")))
  absent_analysis <- b02_analysis("group", "factor", "levels", reference_level = "low", target_level = "mid")
  absent_contrasts <- list(
    b02_contrast("mid_minus_low", list(condition.low = -1, condition.mid = 1, condition.high = 0)),
    b02_contrast("high_minus_low", list(condition.low = -1, condition.mid = 0, condition.high = 1))
  )
  absent <- b02_fit(absent_study, absent_analysis, b02_statistics(absent_contrasts))
  testthat::expect_false(is.null(absent$fit$blocks$block_1$contrasts$high_minus_low))
  present <- b02_row(absent$fit, "F", "mid_minus_low")
  missing_level <- b02_row(absent$fit, "F", "high_minus_low")
  expect_close(present$effect, 2)
  expect_close(present$ordinary_se, sqrt(2 / 3))
  testthat::expect_equal(present$ordinary_df, 4)
  testthat::expect_identical(missing_level$estimation_status, "failed")
  testthat::expect_identical(missing_level$estimation_reason, "contrast_not_estimable")
  testthat::expect_null(missing_level$effect)
  testthat::expect_identical(absent$fit$status, "partial")
  family <- absent$fit$blocks$block_1$contrasts$high_minus_low$family
  testthat::expect_identical(family$n_declared, 1L)
  testthat::expect_identical(family$n_failed, 1L)
  testthat::expect_identical(family$internal_p[[1L]], 1)

  partial_values <- rbind(
    complete = c(1, 2, 3, 3, 4, 5, 5, 6, 7),
    no_high = c(1, 2, 3, 3, 4, 5, NA, NA, NA)
  )
  spec <- b02_groups()
  colnames(partial_values) <- spec$samples
  partial <- b02_fit(
    b02_study(partial_values, spec$metadata, spec$levels),
    b02_analysis("group", "factor", "levels"),
    b02_statistics(list(
      b02_contrast("mid_minus_low", list(condition.low = -1, condition.mid = 1, condition.high = 0)),
      b02_contrast("high_minus_low", list(condition.low = -1, condition.mid = 0, condition.high = 1))
    ))
  )
  expect_close(b02_row(partial$fit, "no_high", "mid_minus_low")$effect, 2)
  testthat::expect_identical(b02_row(partial$fit, "no_high", "high_minus_low")$estimation_status, "failed")
  testthat::expect_null(b02_row(partial$fit, "no_high", "high_minus_low")$effect)

  dropped <- b02_statistics(list(b02_contrast(
    "mid_minus_low", list(condition.low = -1, condition.mid = 1)
  )))
  expect_code(b02_fit(study, b02_analysis("group", "factor", "levels"), dropped), "B_DESIGN_CONTRAST")

  age <- batch
  age[[1L]] <- NA_real_
  missing_covariate <- data.frame(sample_id = samples, group = groups, age = age, stringsAsFactors = FALSE)
  expect_code(
    triomics_resolve_analysis(
      b02_study(assay, missing_covariate, list(group = c("A", "B", "C"))),
      b02_analysis("group", "factor", "levels", covariates = list(list(
        variable = "age", role = "confounder", form = "level", rationale = "Missing covariate."
      )))
    ),
    "B_DESIGN_UNSUPPORTED"
  )
})

testthat::test_that("subject-mean and visit-level analyses keep between-person signal and ids", {
  samples <- c("A_b", "A_f", "B_b", "B_f", "C_b", "C_f", "D_b", "D_f")
  metadata <- data.frame(
    sample_id = samples,
    subject = c("A", "A", "B", "B", "C", "C", "D", "D"),
    visit = rep(c("baseline", "followup"), 4L),
    group = rep(c("control", "treatment"), each = 4L),
    stringsAsFactors = FALSE
  )
  values <- c(0, 10, 2, 12, 10, 0, 14, 4)
  assay <- matrix(values, nrow = 1L, dimnames = list("F", samples))
  levels <- list(group = c("control", "treatment"), visit = c("baseline", "followup"))
  study <- b02_study(assay, metadata, levels)
  mean_analysis <- b02_analysis(
    "group", "factor", "mean",
    subject_id = "subject", visit_column = "visit",
    mean_weights = list(baseline = 1, followup = 1),
    reference_level = "control", target_level = "treatment"
  )
  follow_analysis <- mean_analysis
  follow_analysis$design$mean_weights <- list(baseline = 0, followup = 1)
  visit_analysis <- b02_analysis(
    "group", "factor", "levels",
    subject_id = "subject", visit_column = "visit",
    reference_level = "control", target_level = "treatment"
  )
  weights <- list(condition.control = -1, condition.treatment = 1)
  statistics <- b02_statistics(list(b02_contrast("treatment_minus_control", weights)))
  mean_fit <- b02_fit(study, mean_analysis, statistics)
  follow_fit <- b02_fit(study, follow_analysis, statistics)
  visit_fit <- b02_fit(study, visit_analysis, statistics)

  testthat::expect_identical(mean_fit$resolved$representation_id, "mean")
  testthat::expect_identical(visit_fit$resolved$representation_id, "levels")
  testthat::expect_false(identical(mean_fit$resolved$representation_id, visit_fit$resolved$representation_id))
  testthat::expect_identical(mean_fit$resolved$unit_map[[1L]]$unit_type, "subject_mean")
  testthat::expect_identical(visit_fit$resolved$unit_map[[1L]]$unit_type, "visit_level")
  testthat::expect_identical(mean_fit$resolved$model_plan$mean_weights, list(baseline = 1, followup = 1))
  expect_close(mean_fit$resolved$blocks$block_1$assay["F", "A"], 5)
  expect_close(b02_row(mean_fit$fit, "F", "treatment_minus_control")$effect, 1)
  expect_close(b02_row(follow_fit$fit, "F", "treatment_minus_control")$effect, -9)
  expect_close(b02_row(visit_fit$fit, "F", "treatment_minus_control")$effect, 1)
  testthat::expect_equal(b02_row(mean_fit$fit, "F", "treatment_minus_control")$n_rows_used, 4L)
  testthat::expect_equal(b02_row(visit_fit$fit, "F", "treatment_minus_control")$n_rows_used, 8L)
  testthat::expect_equal(b02_row(mean_fit$fit, "F", "treatment_minus_control")$n_subjects, 4L)
  testthat::expect_equal(b02_row(visit_fit$fit, "F", "treatment_minus_control")$n_subjects, 4L)
  testthat::expect_false(identical(
    b02_row(mean_fit$fit, "F", "treatment_minus_control")$representation_hash,
    b02_row(visit_fit$fit, "F", "treatment_minus_control")$representation_hash
  ))
  testthat::expect_false(abs(b02_row(mean_fit$fit, "F", "treatment_minus_control")$effect) < 1e-8)
  centered <- values - rep(c(5, 7, 5, 9), each = 2L)
  testthat::expect_equal(mean(centered[metadata$group == "treatment"]) - mean(centered[metadata$group == "control"]), 0)
  testthat::expect_identical(mean_fit$resolved$blocks$block_1$unit_ids, c("A", "B", "C", "D"))

  incomplete <- study
  keep <- metadata$sample_id != "D_f"
  incomplete$sample_metadata <- metadata[keep, , drop = FALSE]
  incomplete$sample_registry <- incomplete$sample_registry[keep, , drop = FALSE]
  incomplete$assays$block_1 <- assay[, keep, drop = FALSE]
  expect_code(triomics_resolve_analysis(incomplete, mean_analysis), "B_DESIGN_LEVELS")

  centered_request <- mean_analysis
  centered_request$design$representation <- "subject_centered"
  expect_code(triomics_resolve_analysis(study, centered_request), "B_DESIGN_UNSUPPORTED")
  delta_request <- mean_analysis
  delta_request$design$representation <- "delta"
  delta_request$design$kind <- "paired"
  expect_code(triomics_resolve_analysis(study, delta_request), "B_DESIGN_UNSUPPORTED")

  contradicting <- study
  contradicting$analysis_unit_map <- data.frame(
    unit_id = samples,
    representation_id = rep("subject_centered", length(samples)),
    stringsAsFactors = FALSE
  )
  expect_code(triomics_resolve_analysis(contradicting, mean_analysis), "B_DESIGN_UNIT_MAP")
})

testthat::test_that("joint-contrast claims stay distinct from scalar families", {
  spec <- b02_groups()
  values <- rbind(
    F = c(1, 2, 3, 3, 4, 5, 5, 6, 7),
    G = c(0, 0, 1, 2, 2, 3, 1, 1, 2)
  )
  colnames(values) <- spec$samples
  study <- b02_study(values, spec$metadata, spec$levels, scale = "log2")
  analysis <- b02_analysis("group", "factor", "levels", reference_level = "low", target_level = "high")
  contrasts <- list(
    b02_contrast("mid_minus_low", list(condition.low = -1, condition.mid = 1, condition.high = 0)),
    b02_contrast("high_minus_low", list(condition.low = -1, condition.mid = 0, condition.high = 1))
  )
  claim <- list(list(
    claim_id = "groups_joint",
    claim_type = "joint_contrast",
    contrast_ids = c("mid_minus_low", "high_minus_low"),
    effect_unit = "log2_fold_change"
  ))
  alone <- b02_fit(study, analysis, b02_statistics(contrasts, effect_unit = "log2_fold_change"))
  joint <- b02_fit(study, analysis, b02_statistics(contrasts, effect_unit = "log2_fold_change", claims = claim))
  scalar <- joint$fit$blocks$block_1$contrasts$mid_minus_low
  high <- joint$fit$blocks$block_1$contrasts$high_minus_low
  claim_result <- joint$fit$blocks$block_1$joint_claims$groups_joint
  testthat::expect_identical(scalar$claim_scope, "scalar_contrast")
  testthat::expect_identical(claim_result$claim_scope, "joint_contrast")
  testthat::expect_false(identical(scalar$family$family_id, high$family$family_id))
  testthat::expect_false(identical(scalar$family$family_id, claim_result$family$family_id))
  testthat::expect_identical(claim_result$family$grain, "study_analysis_block_joint_contrast")
  testthat::expect_identical(scalar$family$grain, "study_analysis_block_scalar_contrast")
  joint_row <- claim_result$rows[[1L]]
  testthat::expect_null(joint_row$effect)
  testthat::expect_null(joint_row$ordinary_se)
  testthat::expect_identical(joint_row$effect_unit, "log2_fold_change")
  testthat::expect_identical(joint_row$reference_direction_status, "not_a_scalar_direction")
  testthat::expect_identical(joint_row$direction_status, "not_applicable")
  testthat::expect_match(joint_row$direction_reason, "joint_contrast")
  testthat::expect_identical(claim_result$reference_direction$status, "not_a_scalar_direction")
  testthat::expect_identical(claim_result$numerator_df, 2L)
  expect_close(b02_row(joint$fit, "F", "mid_minus_low")$effect, 2)
  expect_close(b02_row(joint$fit, "F", "mid_minus_low")$q, b02_row(alone$fit, "F", "mid_minus_low")$q)
  expect_close(b02_row(joint$fit, "G", "high_minus_low")$q, b02_row(alone$fit, "G", "high_minus_low")$q)

  design <- stats::model.matrix(~ 0 + factor(spec$groups, levels = c("low", "mid", "high")))
  colnames(design) <- c("condition.low", "condition.mid", "condition.high")
  contrast <- cbind(mid_minus_low = c(-1, 1, 0), high_minus_low = c(-1, 0, 1))
  rownames(contrast) <- colnames(design)
  reference <- limma::topTable(
    limma::eBayes(limma::contrasts.fit(limma::lmFit(values, design), contrast), trend = FALSE),
    coef = 1:2, number = Inf, sort.by = "none"
  )
  expect_close(vapply(claim_result$rows, function(row) row$statistic, numeric(1)), reference$F)
  expect_close(vapply(claim_result$rows, function(row) row$p, numeric(1)), reference$P.Value)
  testthat::expect_true(all(vapply(claim_result$rows, function(row) is.null(row$effect), logical(1))))

  reversed <- b02_fit(study, analysis, b02_statistics(list(
    b02_contrast("mid_minus_low", list(condition.low = 1, condition.mid = -1, condition.high = 0)),
    contrasts[[2L]]
  ), effect_unit = "log2_fold_change"))
  expect_close(b02_row(reversed$fit, "F", "mid_minus_low")$effect, -2)
  testthat::expect_identical(
    b02_row(reversed$fit, "F", "mid_minus_low")$p,
    b02_row(joint$fit, "F", "mid_minus_low")$p
  )
  testthat::expect_false(identical(
    reversed$fit$blocks$block_1$contrasts$mid_minus_low$reference_direction$weights,
    joint$fit$blocks$block_1$contrasts$mid_minus_low$reference_direction$weights
  ))

  sensitivity <- b02_fit(
    study, analysis,
    b02_statistics(contrasts, effect_unit = "log2_fold_change", rank_sensitivity = TRUE)
  )
  testthat::expect_identical(
    sensitivity$fit$blocks$block_1$contrasts$mid_minus_low$sensitivity$status,
    "unavailable"
  )
  testthat::expect_null(sensitivity$fit$blocks$block_1$contrasts$mid_minus_low$sensitivity$features)
})

testthat::test_that("the two-group levels lane remains the predecessor resolver", {
  samples <- paste0("S", 1:6)
  metadata <- data.frame(
    sample_id = samples,
    group = rep(c("control", "treatment"), each = 3L),
    stringsAsFactors = FALSE
  )
  assay <- matrix(c(1, 2, 3, 3, 4, 5), nrow = 1L, dimnames = list("F", samples))
  study <- b02_study(assay, metadata, list(group = c("control", "treatment")))
  analysis <- b02_analysis("group", "factor", "levels", reference_level = "control", target_level = "treatment")
  fitted <- b02_fit(study, analysis, b02_statistics(list(b02_contrast(
    "target_minus_reference",
    list(condition.control = -1, condition.treatment = 1)
  ))))
  testthat::expect_null(fitted$resolved$lane)
  expect_close(b02_row(fitted$fit, "F", "target_minus_reference")$effect, 2)
  expect_close(b02_row(fitted$fit, "F", "target_minus_reference")$ordinary_se, sqrt(2 / 3))
  testthat::expect_equal(b02_row(fitted$fit, "F", "target_minus_reference")$ordinary_df, 4)
  expect_code(
    triomics_resolve_analysis(study, b02_analysis(
      "group", "factor", "levels",
      covariates = list(list(variable = "age", role = "confounder", form = "level", rationale = "Kept out of the two-group lane.")),
      reference_level = "control", target_level = "treatment"
    )),
    "B_DESIGN_UNSUPPORTED"
  )
})
