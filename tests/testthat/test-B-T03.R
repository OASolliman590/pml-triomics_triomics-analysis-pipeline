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

b03_oracle <- function(y, design, weights) {
  ok <- is.finite(y)
  x <- design[ok, , drop = FALSE]
  yy <- as.numeric(y[ok])
  weights <- as.numeric(weights[colnames(x)])
  keep <- apply(x, 2L, function(column) any(column != 0))
  x <- x[, keep, drop = FALSE]
  weights <- weights[keep]
  decomposition <- qr(x)
  coefficients <- qr.coef(decomposition, yy)
  coefficients[!is.finite(coefficients)] <- 0
  residual <- yy - as.numeric(qr.fitted(decomposition, yy))
  df <- sum(ok) - decomposition$rank
  sigma2 <- sum(residual^2) / df
  variance <- sum(weights * solve(crossprod(x), weights))
  c(effect = sum(weights * coefficients), se = sqrt(sigma2 * variance), df = df)
}

b03_study <- function(baseline, followup, groups = NULL, covariate_values = NULL,
                      scramble = FALSE, drop_samples = character(),
                      duplicate_baseline = FALSE, blank_subject = FALSE,
                      scale = "linear", visit_levels = c("baseline", "followup"),
                      group_levels = NULL, unit_map = NULL) {
  if (is.null(dim(baseline))) baseline <- matrix(baseline, nrow = 1L)
  if (is.null(dim(followup))) followup <- matrix(followup, nrow = 1L)
  subjects <- colnames(baseline)
  if (is.null(subjects)) subjects <- LETTERS[seq_len(ncol(baseline))]
  colnames(baseline) <- subjects
  colnames(followup) <- subjects
  if (is.null(rownames(baseline))) rownames(baseline) <- paste0("F", seq_len(nrow(baseline)))
  rownames(followup) <- rownames(baseline)
  samples <- as.vector(rbind(paste0(subjects, "_b"), paste0(subjects, "_f")))
  assay <- matrix(NA_real_, nrow(baseline), length(samples),
                  dimnames = list(rownames(baseline), samples))
  for (subject in subjects) {
    assay[, paste0(subject, "_b")] <- baseline[, subject]
    assay[, paste0(subject, "_f")] <- followup[, subject]
  }
  metadata <- data.frame(
    sample_id = samples,
    subject = rep(subjects, each = 2L),
    visit = rep(c("baseline", "followup"), length(subjects)),
    stringsAsFactors = FALSE
  )
  if (!is.null(groups)) {
    metadata$group <- rep(groups, each = 2L)
    if (is.null(group_levels)) group_levels <- unique(groups)
  }
  if (!is.null(covariate_values)) {
    for (cov in covariate_values) {
      values <- rep(NA_real_, nrow(metadata))
      for (i in seq_along(subjects)) {
        values[metadata$subject == subjects[[i]] & metadata$visit == "baseline"] <- cov$baseline[[i]]
        values[metadata$subject == subjects[[i]] & metadata$visit == "followup"] <- cov$followup[[i]]
      }
      metadata[[cov$variable]] <- values
    }
  }
  if (isTRUE(duplicate_baseline)) {
    extra <- matrix(99, nrow(assay), 1L, dimnames = list(rownames(assay), "A_b_dup"))
    assay <- cbind(assay, extra)
    extra_row <- metadata[metadata$sample_id == "A_b", , drop = FALSE]
    extra_row$sample_id <- "A_b_dup"
    metadata <- rbind(metadata, extra_row)
  }
  if (length(drop_samples) > 0L) {
    assay <- assay[, !colnames(assay) %in% drop_samples, drop = FALSE]
    metadata <- metadata[!metadata$sample_id %in% drop_samples, , drop = FALSE]
  }
  if (isTRUE(blank_subject)) metadata$subject <- ""
  if (isTRUE(scramble)) {
    ids <- colnames(assay)
    ord <- c(ids[grepl("_b", ids)], ids[grepl("_f", ids)])
    assay <- assay[, ord, drop = FALSE]
    metadata <- metadata[match(ord, metadata$sample_id), , drop = FALSE]
    rownames(metadata) <- NULL
  }
  levels <- list(visit = visit_levels)
  if (!is.null(group_levels)) levels$group <- group_levels
  list(
    study_id = "synthetic_b_t03",
    preparation_context_id = "b_t03",
    sample_metadata = metadata,
    sample_registry = data.frame(
      sample_id = metadata$sample_id,
      subject_id = metadata$subject,
      visit_id = metadata$visit,
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
    analysis_unit_map = unit_map
  )
}

b03_analysis <- function(between_group = NULL, covariates = list(),
                         missing_visits = "error", representation = "delta",
                         repeated = TRUE, reference_visit = "baseline",
                         target_visit = "followup", reference_level = NULL,
                         target_level = NULL, condition = NULL,
                         subject_id = "subject", visit_column = "visit",
                         mean_weights = NULL, kind = "paired") {
  if (is.null(between_group)) {
    if (is.null(reference_level)) reference_level <- reference_visit
    if (is.null(target_level)) target_level <- target_visit
    if (is.null(condition)) condition <- visit_column
  } else {
    if (is.null(reference_level)) reference_level <- "control"
    if (is.null(target_level)) target_level <- "treatment"
    if (is.null(condition)) condition <- between_group
  }
  list(
    analysis_id = "b_t03_primary",
    block_ids = "block_1",
    preparation_context_id = "b_t03",
    design = list(
      kind = kind,
      condition = condition,
      condition_type = "factor",
      subject_id = subject_id,
      repeated = repeated,
      representation = representation,
      reference_level = reference_level,
      target_level = target_level,
      between_group = between_group,
      visit_column = visit_column,
      reference_visit = reference_visit,
      target_visit = target_visit,
      covariates = covariates,
      missing_visits = missing_visits,
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

b03_statistics <- function(weights = list(intercept = 1), effect_unit = "delta_difference",
                           contrast_id = "target_minus_reference", contrasts = NULL,
                           rank_sensitivity = FALSE, method = "limma", robust = FALSE,
                           selection_validity = "qualified") {
  if (is.null(contrasts)) {
    contrasts <- list(list(
      contrast_id = contrast_id,
      weights = weights,
      interpretation = "Synthetic paired contrast."
    ))
  }
  list(
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
}

b03_contrast <- function(contrast_id, weights) {
  list(contrast_id = contrast_id, weights = weights, interpretation = "Declared paired weights.")
}

b03_run <- function(study, analysis = NULL, statistics = NULL) {
  if (is.null(analysis)) analysis <- b03_analysis()
  if (is.null(statistics)) statistics <- b03_statistics()
  resolved <- triomics_resolve_analysis(study, analysis)
  list(resolved = resolved, fit = triomics_fit_block_models(resolved, statistics))
}

b03_row <- function(fit, feature, contrast = "target_minus_reference") {
  rows <- fit$blocks$block_1$contrasts[[contrast]]$rows
  hit <- Filter(function(row) identical(row$feature_key, feature), rows)
  testthat::expect_length(hit, 1L)
  hit[[1L]]
}

b03_change_study <- function(scramble = FALSE) {
  subjects <- c("A", "B", "C", "D")
  baseline <- rbind(
    V = c(0, 0, 0, 0),
    P = c(5, 7, 4, 8)
  )
  followup <- rbind(
    V = c(1, 2, 3, 4),
    P = c(6, 9, 7, 10)
  )
  colnames(baseline) <- subjects
  colnames(followup) <- subjects
  b03_study(baseline, followup, scramble = scramble)
}

testthat::test_that("B-T03-AC1 delta is target minus reference and recovers the hand change", {
  fitted <- b03_run(b03_change_study())
  resolved <- fitted$resolved
  block <- resolved$blocks$block_1
  testthat::expect_identical(resolved$lane, "b_t03")
  testthat::expect_identical(resolved$representation_id, "delta")
  testthat::expect_identical(resolved$model_plan$formula, "~1")
  testthat::expect_identical(resolved$model_plan$column_ids, "intercept")
  testthat::expect_identical(resolved$model_plan$estimand, "within_person_change")
  testthat::expect_identical(colnames(block$assay), c("A", "B", "C", "D"))
  testthat::expect_equal(block$n_analysis_rows, 4L)
  testthat::expect_equal(block$n_source_samples, 8L)
  testthat::expect_equal(nrow(block$design), 4L)
  expect_close(block$assay["V", ], c(1, 2, 3, 4))
  expect_close(block$assay["P", ], c(1, 2, 3, 2))
  testthat::expect_identical(block$signed_delta_map, block$assay)
  testthat::expect_identical(block$centered, FALSE)
  testthat::expect_identical(block$decomposition, "target_minus_reference_once")
  testthat::expect_identical(block$decomposition_count, 1L)
  testthat::expect_identical(resolved$c_input$maps$block_1, block$assay)
  testthat::expect_identical(resolved$c_input$centered, FALSE)
  testthat::expect_false(isTRUE(all.equal(
    as.numeric(block$assay["V", ]),
    as.numeric(block$assay["V", ]) - mean(as.numeric(block$assay["V", ]))
  )))
  signs <- block$unit_map[[1L]]$visit_weights
  testthat::expect_equal(unname(signs[c("baseline", "followup")]), c(-1, 1))
  testthat::expect_identical(block$unit_map[[1L]]$representation_id, "delta")
  testthat::expect_identical(block$unit_map[[1L]]$unit_type, "subject_delta")

  scrambled <- b03_run(b03_change_study(scramble = TRUE))
  testthat::expect_equal(scrambled$resolved$blocks$block_1$assay, block$assay)
  testthat::expect_identical(
    scrambled$resolved$blocks$block_1$source_hash,
    block$source_hash
  )

  design <- block$design
  weights <- c(intercept = 1)
  oracle_v <- b03_oracle(block$assay["V", ], design, weights)
  oracle_p <- b03_oracle(block$assay["P", ], design, weights)
  row_v <- b03_row(fitted$fit, "V")
  row_p <- b03_row(fitted$fit, "P")
  expect_close(row_v$effect, 2.5)
  expect_close(row_v$ordinary_se, sqrt(5 / 12))
  testthat::expect_equal(row_v$ordinary_df, 3)
  expect_close(row_v$effect, oracle_v[["effect"]])
  expect_close(row_v$ordinary_se, oracle_v[["se"]])
  expect_close(row_p$effect, 2)
  expect_close(row_p$ordinary_se, sqrt(1 / 6))
  testthat::expect_equal(row_p$ordinary_df, 3)
  testthat::expect_identical(row_v$effect_unit, "delta_difference")
  testthat::expect_identical(row_v$statistic_name, "moderated_t")
  testthat::expect_identical(row_v$coefficient_interpretation, "within_person_change")
  testthat::expect_identical(row_v$inferential_status, "unverified")
  testthat::expect_equal(row_v$n_samples, 8L)
  testthat::expect_equal(row_v$n_subjects, 4L)
  testthat::expect_equal(row_v$n_rows_used, 4L)
  testthat::expect_equal(row_v$n_pairs, 4L)
  testthat::expect_equal(row_v$n_analysis_rows, 4L)
  testthat::expect_identical(fitted$fit$blocks$block_1$signed_delta_map, block$assay)
  testthat::expect_identical(fitted$fit$blocks$block_1$centered, FALSE)

  eb <- limma::eBayes(
    limma::contrasts.fit(
      limma::lmFit(block$assay, design),
      matrix(1, nrow = 1L, ncol = 1L)
    ),
    trend = FALSE,
    robust = FALSE
  )
  expect_close(row_v$p, eb$p.value["V", 1L])
  expect_close(row_p$moderated_se, sqrt(eb$s2.post["P"]) * eb$stdev.unscaled["P", 1L])
  expect_close(row_v$effective_test_df, eb$df.total[[match("V", rownames(block$assay))]])
})

testthat::test_that("B-T03-AC1 difference-in-change matches the hand group contrast", {
  subjects <- c("A", "B", "C", "D")
  baseline <- matrix(c(0, 0, 0, 0), nrow = 1L, dimnames = list("F", subjects))
  followup <- matrix(c(1, 3, 5, 7), nrow = 1L, dimnames = list("F", subjects))
  study <- b03_study(baseline, followup, groups = c("control", "control", "treatment", "treatment"),
                     group_levels = c("control", "treatment"))
  weights <- list(between_group.control = -1, between_group.treatment = 1)
  reverse <- list(between_group.control = 1, between_group.treatment = -1)
  statistics <- b03_statistics(contrasts = list(
    b03_contrast("treatment_minus_control", weights),
    b03_contrast("control_minus_treatment", reverse)
  ))
  fitted <- b03_run(study, b03_analysis(between_group = "group"), statistics)
  testthat::expect_identical(fitted$resolved$model_plan$estimand, "difference_in_change")
  testthat::expect_identical(
    fitted$resolved$model_plan$formula,
    "~0 + between_group.control + between_group.treatment"
  )
  block <- fitted$resolved$blocks$block_1
  expect_close(block$assay["F", ], c(1, 3, 5, 7))
  oracle <- b03_oracle(block$assay["F", ], block$design, c(between_group.control = -1, between_group.treatment = 1))
  forward <- b03_row(fitted$fit, "F", "treatment_minus_control")
  backward <- b03_row(fitted$fit, "F", "control_minus_treatment")
  expect_close(forward$effect, 4)
  expect_close(forward$ordinary_se, sqrt(2))
  testthat::expect_equal(forward$ordinary_df, 2)
  expect_close(forward$effect, oracle[["effect"]])
  expect_close(forward$ordinary_se, oracle[["se"]])
  expect_close(backward$effect, -4)
  testthat::expect_equal(forward$p, backward$p)
  testthat::expect_identical(forward$coefficient_interpretation, "difference_in_change")
  testthat::expect_identical(
    fitted$fit$blocks$block_1$contrasts$treatment_minus_control$estimand,
    "difference_in_change"
  )
  testthat::expect_equal(forward$direction_groups$control$direction_numerator, backward$direction_groups$control$direction_numerator)
  testthat::expect_null(forward$direction_numerator)
})

testthat::test_that("B-T03-AC2 complete-pair policies report attrition and do not impute or guess", {
  subjects <- c("A", "B", "C", "D")
  baseline <- matrix(c(0, 0, 0, 0), nrow = 1L, dimnames = list("F", subjects))
  followup <- matrix(c(1, 3, 5, 9), nrow = 1L, dimnames = list("F", subjects))
  groups <- c("control", "control", "treatment", "treatment")
  dropped <- b03_study(baseline, followup, groups = groups, drop_samples = "D_f",
                       group_levels = c("control", "treatment"))
  analysis <- b03_analysis(between_group = "group", missing_visits = "complete_pairs_per_contrast")
  weights <- list(between_group.control = -1, between_group.treatment = 1)
  fitted <- b03_run(dropped, analysis, b03_statistics(weights, contrast_id = "treatment_minus_control"))
  block <- fitted$resolved$blocks$block_1
  testthat::expect_false("D" %in% colnames(block$assay))
  expect_close(block$assay["F", c("A", "B", "C")], c(1, 3, 5))
  attrition <- block$attrition
  testthat::expect_identical(attrition$policy, "complete_pairs_per_contrast")
  testthat::expect_identical(attrition$population_changed, TRUE)
  testthat::expect_identical(attrition$imputed, FALSE)
  testthat::expect_identical(attrition$pairing, "subject_id_and_declared_visits")
  testthat::expect_equal(attrition$n_subjects_starting, 4L)
  testthat::expect_equal(attrition$n_subjects_retained, 3L)
  testthat::expect_equal(attrition$n_subjects_excluded, 1L)
  testthat::expect_equal(attrition$per_group$control$n_excluded, 0L)
  testthat::expect_equal(attrition$per_group$treatment$n_excluded, 1L)
  testthat::expect_equal(attrition$per_group$treatment$n_retained, 1L)
  excluded <- Filter(function(unit) identical(unit$unit_id, "D"), block$unit_map)
  testthat::expect_length(excluded, 1L)
  testthat::expect_identical(excluded[[1L]]$eligibility, "excluded")
  testthat::expect_identical(excluded[[1L]]$exclusion_reason, "missing_target_visit")
  row <- b03_row(fitted$fit, "F", "treatment_minus_control")
  expect_close(row$effect, 3)
  testthat::expect_equal(row$ordinary_df, 1)
  testthat::expect_identical(
    fitted$fit$blocks$block_1$contrasts$treatment_minus_control$attrition$n_subjects_excluded,
    1L
  )

  strict <- b03_analysis(between_group = "group", missing_visits = "error")
  err <- expect_code(
    triomics_resolve_analysis(dropped, strict),
    "B_DESIGN_PAIRING"
  )
  testthat::expect_match(conditionMessage(err), "not imputed")
  testthat::expect_match(conditionMessage(err), "not guessed")
  testthat::expect_match(conditionMessage(err), "population")

  duplicated <- b03_study(baseline, followup, duplicate_baseline = TRUE)
  dup <- expect_code(triomics_resolve_analysis(duplicated, b03_analysis()), "B_DESIGN_PAIRING")
  testthat::expect_match(conditionMessage(dup), "not guessed")

  blank <- b03_study(baseline, followup, blank_subject = TRUE)
  guessed <- expect_code(triomics_resolve_analysis(blank, b03_analysis()), "B_DESIGN_PAIRING")
  testthat::expect_match(conditionMessage(guessed), "not guessed")

  partial <- rbind(
    F = c(0, 0, 0, 0),
    G = c(0, 0, 0, 0)
  )
  partial_follow <- rbind(
    F = c(1, 2, 3, 4),
    G = c(4, 0, 0, NA)
  )
  colnames(partial) <- subjects
  colnames(partial_follow) <- subjects
  valued <- b03_run(b03_study(partial, partial_follow))
  expect_close(b03_row(valued$fit, "F")$effect, 2.5)
  expect_close(b03_row(valued$fit, "G")$effect, 4 / 3)
  testthat::expect_equal(b03_row(valued$fit, "G")$n_pairs, 3L)
  testthat::expect_equal(b03_row(valued$fit, "F")$n_pairs, 4L)
  testthat::expect_false(isTRUE(all.equal(b03_row(valued$fit, "G")$effect, 1)))
  testthat::expect_identical(valued$resolved$blocks$block_1$attrition$population_changed, FALSE)
  testthat::expect_equal(valued$resolved$blocks$block_1$attrition$n_subjects_excluded, 0L)

  absent_levels <- c("control", "treatment", "unobserved")
  absent_study <- b03_study(baseline, followup, groups = groups, group_levels = absent_levels)
  absent_stats <- b03_statistics(contrasts = list(
    b03_contrast("treatment_minus_control", list(
      between_group.control = -1, between_group.treatment = 1, between_group.unobserved = 0
    )),
    b03_contrast("unobserved_minus_control", list(
      between_group.control = -1, between_group.treatment = 0, between_group.unobserved = 1
    ))
  ))
  absent <- b03_run(absent_study, b03_analysis(between_group = "group"), absent_stats)
  kept <- b03_row(absent$fit, "F", "treatment_minus_control")
  missing_level <- b03_row(absent$fit, "F", "unobserved_minus_control")
  expect_close(kept$effect, 5)
  testthat::expect_identical(missing_level$estimation_status, "failed")
  testthat::expect_identical(missing_level$estimation_reason, "contrast_not_estimable")
  testthat::expect_null(missing_level$effect)
  testthat::expect_null(missing_level$p)
  testthat::expect_identical(absent$fit$status, "partial")
  testthat::expect_identical(absent$fit$blocks$block_1$contrasts$unobserved_minus_control$family$n_failed, 1L)
  testthat::expect_identical(absent$fit$blocks$block_1$contrasts$unobserved_minus_control$family$internal_p[[1L]], 1)
})

testthat::test_that("B-T03-AC3 modifiers use declared transforms and projected-out levels fail", {
  subjects <- c("A", "B", "C", "D")
  baseline <- matrix(0, nrow = 1L, ncol = length(subjects), dimnames = list("F", subjects))
  followup <- matrix(c(1, 2, 3, 4), nrow = 1L, dimnames = list("F", subjects))
  age <- list(variable = "age", baseline = c(0, 0, 1, 1), followup = c(0, 0, 1, 1))
  study <- b03_study(baseline, followup, covariate_values = list(age))
  covariates <- list(list(
    variable = "age", role = "confounder", form = "level", rationale = "Static modifier of change."
  ))
  statistics <- b03_statistics(contrasts = list(
    b03_contrast("change", list(intercept = 1, modifier.age = 0)),
    b03_contrast("age_modifier", list(intercept = 0, modifier.age = 1))
  ))
  fitted <- b03_run(study, b03_analysis(covariates = covariates), statistics)
  plan <- fitted$resolved$model_plan$covariates[[1L]]
  testthat::expect_identical(plan$interpretation, "modifier_of_change")
  testthat::expect_identical(plan$within_person_main_effect, FALSE)
  testthat::expect_identical(plan$role, "confounder")
  testthat::expect_identical(plan$time_varying, FALSE)
  testthat::expect_identical(plan$transformation, "static_level")
  testthat::expect_identical(fitted$resolved$model_plan$formula, "~1 + modifier.age")
  design <- fitted$resolved$blocks$block_1$design
  expect_close(design[, "modifier.age"], c(0, 0, 1, 1))
  change <- b03_row(fitted$fit, "F", "change")
  modifier <- b03_row(fitted$fit, "F", "age_modifier")
  expect_close(change$effect, 1.5)
  expect_close(modifier$effect, 2)
  testthat::expect_equal(change$ordinary_df, 2)
  testthat::expect_identical(change$coefficient_interpretation, "within_person_change")
  testthat::expect_identical(modifier$coefficient_interpretation, "modifier_of_change")
  testthat::expect_false(identical(modifier$coefficient_interpretation, "within_person_main_effect"))
  oracle <- b03_oracle(
    fitted$resolved$blocks$block_1$assay["F", ],
    design,
    c(intercept = 0, modifier.age = 1)
  )
  expect_close(modifier$effect, oracle[["effect"]])
  expect_close(modifier$ordinary_se, oracle[["se"]])

  between <- covariates
  between[[1L]]$form <- "between"
  between_fit <- triomics_resolve_analysis(study, b03_analysis(covariates = between))
  testthat::expect_identical(between_fit$model_plan$covariates[[1L]]$transformation, "static_between")
  expect_close(between_fit$blocks$block_1$design[, "modifier.age"], c(0, 0, 1, 1))
  testthat::expect_false(identical(
    between_fit$model_plan$covariates[[1L]]$transformation,
    "static_level"
  ))

  varying <- list(variable = "cov", baseline = c(0, 1, 2, 0), followup = c(2, 1, 6, 0))
  varying_study <- b03_study(baseline, followup, covariate_values = list(varying))
  expected_delta <- varying$followup - varying$baseline
  expected_between <- (varying$baseline + varying$followup) / 2
  expected_within <- varying$followup - expected_between
  for (form in c("delta", "within", "between")) {
    spec <- list(list(variable = "cov", role = "modifier", form = form, rationale = "Declared transform."))
    resolved <- triomics_resolve_analysis(varying_study, b03_analysis(covariates = spec))
    column <- as.numeric(resolved$blocks$block_1$design[, "modifier.cov"])
    if (identical(form, "delta")) {
      expect_close(column, expected_delta)
      testthat::expect_identical(resolved$model_plan$covariates[[1L]]$transformation, "delta")
    } else if (identical(form, "within")) {
      expect_close(column, expected_within)
      testthat::expect_identical(
        resolved$model_plan$covariates[[1L]]$transformation,
        "within_target_minus_subject_mean"
      )
    } else {
      expect_close(column, expected_between)
      testthat::expect_identical(resolved$model_plan$covariates[[1L]]$transformation, "between_subject_mean")
    }
    testthat::expect_identical(resolved$model_plan$covariates[[1L]]$time_varying, TRUE)
  }
  delta_column <- triomics_resolve_analysis(
    varying_study,
    b03_analysis(covariates = list(list(variable = "cov", role = "modifier", form = "delta", rationale = "Delta.")))
  )$blocks$block_1$design[, "modifier.cov"]
  within_column <- triomics_resolve_analysis(
    varying_study,
    b03_analysis(covariates = list(list(variable = "cov", role = "modifier", form = "within", rationale = "Within.")))
  )$blocks$block_1$design[, "modifier.cov"]
  testthat::expect_false(isTRUE(all.equal(as.numeric(delta_column), as.numeric(within_column))))

  level_form <- list(list(variable = "cov", role = "modifier", form = "level", rationale = "Projected level."))
  projected <- expect_code(
    triomics_resolve_analysis(varying_study, b03_analysis(covariates = level_form)),
    "B_DESIGN_LEVELS"
  )
  testthat::expect_match(conditionMessage(projected), "projected")

  static_delta <- list(list(variable = "age", role = "confounder", form = "delta", rationale = "Not a within effect."))
  static_err <- expect_code(
    triomics_resolve_analysis(study, b03_analysis(covariates = static_delta)),
    "B_DESIGN_UNSUPPORTED"
  )
  testthat::expect_match(conditionMessage(static_err), "modifier of change")

  visit_weights <- list(condition.baseline = -1, condition.followup = 1)
  level_contrast <- expect_code(
    b03_run(b03_change_study(), statistics = b03_statistics(visit_weights)),
    "B_DESIGN_LEVELS"
  )
  testthat::expect_match(conditionMessage(level_contrast), "Projected-out")

  mean_request <- b03_analysis(mean_weights = list(baseline = 1, followup = 1))
  mean_err <- expect_code(triomics_resolve_analysis(b03_change_study(), mean_request), "B_DESIGN_LEVELS")
  testthat::expect_match(conditionMessage(mean_err), "delta")

  removed <- b03_analysis(target_visit = "followup", reference_visit = "baseline")
  removed_study <- b03_change_study()
  removed_study$declared_factor_levels$visit <- "baseline"
  removed_err <- expect_code(triomics_resolve_analysis(removed_study, removed), "B_DESIGN_LEVELS")
  testthat::expect_match(conditionMessage(removed_err), "removed")

  levels_request <- b03_analysis(representation = "levels")
  expect_code(triomics_resolve_analysis(b03_change_study(), levels_request), "B_DESIGN_UNSUPPORTED")
})

testthat::test_that("B-T03-AC4 directional counts keep zeros and the signed delta is not recentered", {
  subjects <- c("A", "B", "C", "D")
  baseline <- matrix(0, nrow = 1L, ncol = length(subjects), dimnames = list("Z", subjects))
  followup <- matrix(c(4, 0, 0, 0), nrow = 1L, dimnames = list("Z", subjects))
  fitted <- b03_run(b03_study(baseline, followup), statistics = b03_statistics(rank_sensitivity = TRUE))
  row <- b03_row(fitted$fit, "Z")
  expect_close(row$effect, 1)
  testthat::expect_equal(row$direction_numerator, 1)
  testthat::expect_equal(row$direction_denominator, 1)
  testthat::expect_equal(row$direction_zero_count, 3)
  testthat::expect_equal(row$direction_unavailable_count, 0)
  testthat::expect_identical(row$direction_status, "paired_sign_agreement")
  testthat::expect_null(row$direction_groups)
  testthat::expect_identical(fitted$resolved$c_input$decomposition_count, 1L)
  testthat::expect_identical(fitted$fit$blocks$block_1$signed_delta_map, fitted$resolved$blocks$block_1$assay)
  sensitivity <- fitted$fit$blocks$block_1$contrasts$target_minus_reference$sensitivity
  testthat::expect_identical(sensitivity$status, "complete")
  testthat::expect_identical(sensitivity$exact, FALSE)
  testthat::expect_identical(sensitivity$continuity_correction, TRUE)
  testthat::expect_identical(sensitivity$equivalent_to_moderated_test, FALSE)
  testthat::expect_identical(sensitivity$substitutes_for_primary, FALSE)
  direct <- suppressWarnings(stats::wilcox.test(c(4, 0, 0, 0), exact = FALSE, correct = TRUE))
  expect_close(sensitivity$features[[1L]]$p, direct$p.value)
  testthat::expect_equal(sensitivity$features[[1L]]$n_zero_differences, 3)

  groups <- c("control", "control", "treatment", "treatment")
  base <- matrix(0, nrow = 1L, ncol = length(subjects), dimnames = list("F", subjects))
  follow <- matrix(c(1, 0, 4, 5), nrow = 1L, dimnames = list("F", subjects))
  dropped <- b03_study(base, follow, groups = groups, drop_samples = "D_f",
                       group_levels = c("control", "treatment"))
  group_fit <- b03_run(
    dropped,
    b03_analysis(between_group = "group", missing_visits = "complete_pairs_per_contrast"),
    b03_statistics(list(between_group.control = -1, between_group.treatment = 1),
                   contrast_id = "treatment_minus_control")
  )
  grouped <- b03_row(group_fit$fit, "F", "treatment_minus_control")
  expect_close(grouped$effect, 3.5)
  testthat::expect_null(grouped$direction_numerator)
  testthat::expect_null(grouped$direction_denominator)
  testthat::expect_equal(grouped$direction_zero_count, 1)
  testthat::expect_equal(grouped$direction_unavailable_count, 1)
  testthat::expect_identical(grouped$direction_status, "within_group_change")
  testthat::expect_identical(
    grouped$direction_reason,
    "difference_in_change_is_not_a_per_subject_cross_group_delta"
  )
  testthat::expect_equal(grouped$direction_groups$control$direction_numerator, 1)
  testthat::expect_equal(grouped$direction_groups$control$direction_denominator, 1)
  testthat::expect_equal(grouped$direction_groups$control$direction_zero_count, 1)
  testthat::expect_equal(grouped$direction_groups$control$direction_unavailable_count, 0)
  testthat::expect_equal(grouped$direction_groups$treatment$direction_numerator, 1)
  testthat::expect_equal(grouped$direction_groups$treatment$direction_denominator, 1)
  testthat::expect_equal(grouped$direction_groups$treatment$direction_unavailable_count, 1)

  age <- list(variable = "age", baseline = c(0, 0, 1, 1), followup = c(0, 0, 1, 1))
  adjusted_study <- b03_study(
    matrix(0, 1, 4, dimnames = list("F", subjects)),
    matrix(c(1, 2, 3, 4), 1, 4, dimnames = list("F", subjects)),
    covariate_values = list(age)
  )
  adjusted <- b03_run(
    adjusted_study,
    b03_analysis(covariates = list(list(
      variable = "age", role = "confounder", form = "level", rationale = "Modifier."
    ))),
    b03_statistics(
      contrasts = list(b03_contrast("change", list(intercept = 1, modifier.age = 0))),
      rank_sensitivity = TRUE
    )
  )
  testthat::expect_identical(
    adjusted$fit$blocks$block_1$contrasts$change$sensitivity$status,
    "unavailable"
  )
  testthat::expect_null(adjusted$fit$blocks$block_1$contrasts$change$sensitivity$features)
  expect_close(b03_row(adjusted$fit, "F", "change")$effect, 1.5)
  testthat::expect_identical(
    b03_row(adjusted$fit, "F", "change")$direction_reason,
    "unadjusted_paired_sign_agreement_with_adjusted_effect"
  )
})

testthat::test_that("B-T03 effect units, failed-family members, and DEqMS stay explicit", {
  logged <- b03_change_study()
  logged$feature_registry$scale <- "log2"
  log_fit <- b03_run(logged, statistics = b03_statistics(effect_unit = "log2_fold_change"))
  testthat::expect_identical(b03_row(log_fit$fit, "V")$effect_unit, "log2_fold_change")
  expect_close(b03_row(log_fit$fit, "V")$effect, 2.5)
  expect_code(b03_run(logged, statistics = b03_statistics(effect_unit = "delta_difference")), "B_DESIGN_EFFECT_UNIT")
  expect_code(b03_run(b03_change_study(), statistics = b03_statistics(effect_unit = "log2_fold_change")), "B_DESIGN_EFFECT_UNIT")
  expect_code(b03_run(b03_change_study(), statistics = b03_statistics(effect_unit = "group_difference")), "B_DESIGN_EFFECT_UNIT")

  subjects <- c("A", "B", "C", "D")
  baseline <- rbind(good = c(0, 0, 0, 0), bad = c(NA, NA, NA, NA))
  followup <- rbind(good = c(1, 2, 3, 4), bad = c(NA, NA, NA, NA))
  colnames(baseline) <- subjects
  colnames(followup) <- subjects
  failed <- b03_run(b03_study(baseline, followup))
  bad <- b03_row(failed$fit, "bad")
  good <- b03_row(failed$fit, "good")
  testthat::expect_null(bad$effect)
  testthat::expect_null(bad$p)
  testthat::expect_null(bad$q)
  testthat::expect_identical(bad$estimation_status, "failed")
  testthat::expect_identical(bad$inferential_status, "not_estimable")
  family <- failed$fit$blocks$block_1$contrasts$target_minus_reference$family
  testthat::expect_identical(family$n_declared, 2L)
  testthat::expect_identical(family$n_failed, 1L)
  testthat::expect_identical(family$internal_p[[which(family$member_ids == "bad")]], 1)
  testthat::expect_identical(good$inferential_status, "unverified")
  testthat::expect_identical(family$validation$status, "withheld")
  testthat::expect_false(family$q_published)
  testthat::expect_true("selection_inventory_missing" %in% family$validation$reasons)
  expect_close(family$internal_q[[which(family$member_ids == "good")]],
               stats::p.adjust(c(good$p, 1), method = "BH")[[1L]])
  testthat::expect_null(good$q)

  alone <- b03_run(b03_change_study())
  both <- b03_change_study()
  extra <- rbind(both$assays$block_1, EXTRA = both$assays$block_1["V", ])
  both$assays$block_2 <- extra
  both$feature_registry <- rbind(
    both$feature_registry,
    data.frame(feature_key = c("V", "P", "EXTRA"), block_id = "block_2", scale = "linear", stringsAsFactors = FALSE)
  )
  analysis <- b03_analysis()
  analysis$block_ids <- c("block_1", "block_2")
  widened <- b03_run(both, analysis)
  alone_result <- alone$fit$blocks$block_1$contrasts$target_minus_reference
  widened_result <- widened$fit$blocks$block_1$contrasts$target_minus_reference
  testthat::expect_equal(alone_result$family$internal_q, widened_result$family$internal_q)
  testthat::expect_identical(alone_result$family$validation$status, "withheld")
  testthat::expect_false(alone_result$family$q_published)
  testthat::expect_true("selection_inventory_missing" %in% alone_result$family$validation$reasons)
  testthat::expect_true(all(vapply(alone_result$rows, function(row) is.null(row$q), logical(1))))
  testthat::expect_true(all(vapply(widened_result$rows, function(row) is.null(row$q), logical(1))))

  resolved <- triomics_resolve_analysis(b03_change_study(), b03_analysis())
  deqms <- triomics_fit_block_models(resolved, b03_statistics(method = "deqms"))
  testthat::expect_identical(deqms$status, "unavailable")
  testthat::expect_false(isTRUE(deqms$fallback_applied))
})

testthat::test_that("B-T03 the paired fixture recovers subject deltas from predecessor matrices", {
  config_path <- triomics_test_resource("examples", "tickets", "B-T03.yaml")
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  analysis <- config$analyses[[1L]]
  block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
  analysis$block_ids <- block_ids
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  fitted <- b03_run(study, analysis, analysis$statistics)
  metadata <- utils::read.delim(
    triomics_test_resource("examples", "data", "paired", "metadata.tsv"),
    stringsAsFactors = FALSE
  )
  table <- utils::read.delim(
    triomics_test_resource("examples", "data", "paired", "proteomics.tsv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  subjects <- unique(metadata$subject)
  hand <- vapply(seq_len(nrow(table)), function(i) {
    values <- as.numeric(table[i, metadata$sample_id])
    names(values) <- metadata$sample_id
    deltas <- vapply(subjects, function(subject) {
      rows <- metadata[metadata$subject == subject, ]
      target <- rows$sample_id[rows$visit == "followup"]
      reference <- rows$sample_id[rows$visit == "baseline"]
      values[[target]] - values[[reference]]
    }, numeric(1))
    mean(deltas)
  }, numeric(1))
  effects <- vapply(fitted$fit$blocks$block_1$contrasts$target_minus_reference$rows, function(row) {
    row$effect
  }, numeric(1))
  expect_close(effects, hand)
  testthat::expect_identical(fitted$resolved$representation_id, "delta")
  testthat::expect_equal(b03_row(fitted$fit, fitted$fit$blocks$block_1$contrasts$target_minus_reference$rows[[1L]]$feature_key)$ordinary_df, 5)
  output <- file.path(tempdir(), paste0("b-t03-ticket-", as.integer(Sys.time()), "-", Sys.getpid()))
  ran <- triomics_execute_stage_b(config_path, output)
  testthat::expect_identical(ran$status, "complete")
  payload <- jsonlite::fromJSON(file.path(output, "B", "block-results.json"), simplifyVector = FALSE)
  testthat::expect_identical(payload$representation_id, "delta")
  testthat::expect_identical(payload$blocks$block_1$centered, FALSE)
  testthat::expect_identical(payload$blocks$block_1$decomposition, "target_minus_reference_once")
})

testthat::test_that("B-T03 does not capture independent levels or an incomplete paired request", {
  groups <- rep(c("control", "treatment"), each = 3L)
  samples <- paste0("S", seq_along(groups))
  assay <- matrix(c(1, 2, 3, 3, 4, 5), nrow = 1L, dimnames = list("F", samples))
  study <- list(
    study_id = "synthetic_b_t01",
    preparation_context_id = "b_t01_levels",
    sample_metadata = data.frame(sample_id = samples, group = groups, stringsAsFactors = FALSE),
    sample_registry = NULL,
    declared_factor_levels = list(group = c("control", "treatment")),
    feature_registry = data.frame(feature_key = "F", block_id = "block_1", scale = "linear", stringsAsFactors = FALSE),
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  analysis <- list(
    analysis_id = "primary",
    block_ids = "block_1",
    preparation_context_id = "b_t01_levels",
    design = list(
      kind = "independent",
      condition = "group",
      condition_type = "factor",
      subject_id = NULL,
      repeated = FALSE,
      representation = "levels",
      reference_level = "control",
      target_level = "treatment",
      covariates = list(),
      missing_visits = "error"
    )
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  testthat::expect_identical(resolved$model_plan$formula, "~0 + condition")
  testthat::expect_false(identical(resolved$lane, "b_t03"))
  fit <- triomics_fit_block_models(resolved, b03_statistics(
    list(condition.control = -1, condition.treatment = 1),
    effect_unit = "group_difference"
  ))
  expect_close(fit$blocks$block_1$contrasts$target_minus_reference$rows[[1L]]$effect, 2)

  incomplete <- analysis
  incomplete$design$kind <- "paired"
  incomplete$design$representation <- "delta"
  expect_code(triomics_resolve_analysis(study, incomplete), "B_DESIGN_UNSUPPORTED")
})
