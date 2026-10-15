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

b04_oracle <- function(y, subjects, design, weights) {
  centered <- as.numeric(y)
  for (subject in unique(subjects)) {
    index <- which(subjects == subject & is.finite(y))
    if (length(index) > 0L) centered[index] <- y[index] - mean(y[index])
  }
  ok <- is.finite(y)
  x <- design[ok, , drop = FALSE]
  yy <- centered[ok]
  decomposition <- qr(x)
  coefficients <- qr.coef(decomposition, yy)
  coefficients[!is.finite(coefficients)] <- 0
  residual <- yy - as.numeric(qr.fitted(decomposition, yy))
  df <- sum(ok) - decomposition$rank
  sigma2 <- sum(residual^2) / df
  variance <- sum(weights * solve(crossprod(x), weights))
  raw <- qr.coef(qr(x), as.numeric(y[ok]))
  raw[!is.finite(raw)] <- 0
  c(
    effect = sum(weights * coefficients),
    se = sqrt(sigma2 * variance),
    df = df,
    rank = decomposition$rank,
    raw_effect = sum(weights * raw)
  )
}

b04_visit_study <- function(features, visits = c("v1", "v2", "v3"), subjects = NULL,
                            scale = "linear", scramble = FALSE) {
  if (is.null(dim(features))) features <- matrix(features, nrow = 1L)
  if (is.null(subjects)) subjects <- LETTERS[seq_len(ncol(features) / length(visits))]
  if (is.null(colnames(features))) {
    colnames(features) <- as.vector(t(outer(subjects, visits, paste, sep = "_")))
  }
  if (is.null(rownames(features))) rownames(features) <- paste0("F", seq_len(nrow(features)))
  samples <- colnames(features)
  metadata <- data.frame(
    sample_id = samples,
    subject = rep(subjects, each = length(visits)),
    visit = rep(visits, times = length(subjects)),
    stringsAsFactors = FALSE
  )
  if (isTRUE(scramble)) {
    ord <- sample(samples)
    features <- features[, ord, drop = FALSE]
    metadata <- metadata[match(ord, metadata$sample_id), , drop = FALSE]
    rownames(metadata) <- NULL
  }
  list(
    study_id = "synthetic_b_t04",
    preparation_context_id = "b_t04",
    sample_metadata = metadata,
    sample_registry = NULL,
    declared_factor_levels = list(visit = visits),
    feature_registry = data.frame(
      feature_key = rownames(features),
      block_id = "block_1",
      scale = scale,
      stringsAsFactors = FALSE
    ),
    assays = list(block_1 = features),
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
}

b04_time_study <- function(times_by_subject, values, origin_clock = NULL, scale = "linear") {
  if (is.null(dim(values))) values <- matrix(values, nrow = 1L)
  subjects <- rep(names(times_by_subject), times = vapply(times_by_subject, length, integer(1)))
  times <- unlist(times_by_subject, use.names = FALSE)
  samples <- paste0(subjects, "_t", seq_along(times))
  colnames(values) <- samples
  if (is.null(rownames(values))) rownames(values) <- paste0("F", seq_len(nrow(values)))
  metadata <- data.frame(
    sample_id = samples,
    subject = subjects,
    visit = paste0("t", seq_along(times)),
    time = times,
    stringsAsFactors = FALSE
  )
  list(
    study_id = "synthetic_b_t04",
    preparation_context_id = "b_t04",
    sample_metadata = metadata,
    sample_registry = NULL,
    declared_factor_levels = list(visit = unique(metadata$visit)),
    feature_registry = data.frame(
      feature_key = rownames(values),
      block_id = "block_1",
      scale = scale,
      stringsAsFactors = FALSE
    ),
    assays = list(block_1 = values),
    assay_hashes = NULL,
    analysis_unit_map = NULL,
    origin_clock = origin_clock
  )
}

b04_design <- function(kind = "multivisit", residual = "fixed_subject_ordinary_independent",
                       representation = "subject_centered", time_column = NULL, time_unit = NULL,
                       time_origin = NULL, spline_knots = list(), spline_boundary = list(),
                       reference_level = "v1", target_level = "v3", covariates = list(),
                       between_group = NULL, missing_visits = "error", condition = NULL,
                       condition_type = NULL, visit_column = "visit") {
  if (identical(kind, "trajectory")) {
    if (is.null(condition)) condition <- "time"
    if (is.null(condition_type)) condition_type <- "continuous"
    if (is.null(time_column)) time_column <- "time"
    reference_level <- NULL
    target_level <- NULL
  } else {
    if (is.null(condition)) condition <- visit_column
    if (is.null(condition_type)) condition_type <- "factor"
  }
  list(
    analysis_id = "b_t04_primary",
    block_ids = "block_1",
    preparation_context_id = "b_t04",
    design = list(
      kind = kind,
      condition = condition,
      condition_type = condition_type,
      subject_id = "subject",
      repeated = TRUE,
      representation = representation,
      reference_level = reference_level,
      target_level = target_level,
      between_group = between_group,
      visit_column = visit_column,
      reference_visit = reference_level,
      target_visit = target_level,
      covariates = covariates,
      missing_visits = missing_visits,
      time_column = time_column,
      time_unit = time_unit,
      time_origin = time_origin,
      spline_knots = spline_knots,
      spline_boundary = spline_boundary,
      residual_contract_ref = residual,
      mean_weights = NULL,
      interactions = list()
    )
  )
}

b04_statistics <- function(column_ids, nonzero, effect_unit, contrast_id = "within",
                           rank_sensitivity = FALSE, method = "limma",
                           selection_validity = "qualified") {
  weights <- stats::setNames(rep(0, length(column_ids)), column_ids)
  for (name in names(nonzero)) weights[[name]] <- nonzero[[name]]
  list(
    method = method,
    contrasts = list(list(
      contrast_id = contrast_id,
      weights = as.list(weights),
      interpretation = "Synthetic fixed-subject contrast."
    )),
    trend = FALSE,
    robust = FALSE,
    q_threshold = 0.05,
    effect_threshold = 0,
    effect_unit = effect_unit,
    family_scope = "one_scalar_contrast_all_retained_features",
    selection_validity = selection_validity,
    selection_justification = "Synthetic fixture; inference is not a study claim.",
    dependence_justification = "Synthetic fixture; no scientific dependence claim.",
    rank_sensitivity = rank_sensitivity,
    count_metadata = NULL,
    fallback = "none"
  )
}

b04_row <- function(fit, feature) {
  rows <- fit$blocks$block_1$contrasts[[1L]]$rows
  hit <- Filter(function(row) identical(row$feature_key, feature), rows)
  hit[[1L]]
}

b04_fit <- function(study, analysis, nonzero, effect_unit) {
  resolved <- triomics_resolve_analysis(study, analysis)
  column_ids <- resolved$model_plan$column_ids
  fit <- triomics_fit_block_models(resolved, b04_statistics(column_ids, nonzero, effect_unit))
  list(resolved = resolved, fit = fit)
}

testthat::test_that("B-T04 unqualified repeated capabilities stay unavailable after refusal", {
  record <- triomics_b04_capability("centered_duplicate_correlation")
  testthat::expect_false(isTRUE(record$implemented))
  testthat::expect_false(isTRUE(record$qualified))
  testthat::expect_identical(record$status, "unavailable")
  testthat::expect_false(isTRUE(record$refusal_marks_implemented))
  slope <- triomics_b04_capability("random_slope")
  testthat::expect_false(isTRUE(slope$implemented))
  testthat::expect_identical(slope$status, "unavailable")
  visits <- c("v1", "v2", "v3")
  y <- rep(0, 12)
  study <- b04_visit_study(y, visits)
  refused <- b04_design(residual = "random_slope")
  expect_code(triomics_resolve_analysis(study, refused), "B_REPEATED_MODEL_UNSUPPORTED")
  after <- triomics_b04_capability("random_slope")
  testthat::expect_false(isTRUE(after$implemented))
  testthat::expect_identical(after$status, "unavailable")
  testthat::expect_false(isTRUE(after$refusal_marks_implemented))
  ordinary <- triomics_b04_capability("fixed_subject_ordinary_independent")
  testthat::expect_true(isTRUE(ordinary$implemented))
  testthat::expect_false(identical(ordinary$evidence, "refusal_only"))
})

testthat::test_that("B-T04 fixed-subject rank and df match the raw ordinary reference", {
  subjects <- LETTERS[1:4]
  visits <- c("v1", "v2", "v3")
  set.seed(17)
  base <- 2 * (visits == "v3") + rnorm(12, sd = 0.3)
  intercept <- rep(c(4, 1, -2, 3), each = 3)
  feature_1 <- intercept + base
  feature_2 <- feature_1
  feature_2[3] <- NA_real_
  feature_3 <- feature_1
  feature_3[c(3, 6)] <- NA_real_
  assay <- rbind(feature_1, feature_2, feature_3)
  rownames(assay) <- c("complete", "one_cell", "two_cells")
  study <- b04_visit_study(assay, visits, subjects)
  analysis <- b04_design()
  called <- FALSE
  traced <- FALSE
  on.exit(if (isTRUE(traced)) suppressWarnings(suppressMessages(
    untrace(limma::duplicateCorrelation, where = asNamespace("limma"))
  )), add = TRUE)
  suppressWarnings(suppressMessages(trace(
    limma::duplicateCorrelation,
    tracer = function() called <<- TRUE,
    print = FALSE,
    where = asNamespace("limma")
  )))
  traced <- TRUE
  ran <- b04_fit(study, analysis, c(visit.v3 = 1), "within_person_difference")
  testthat::expect_false(isTRUE(called))
  resolved <- ran$resolved
  testthat::expect_identical(resolved$lane, "b_t04")
  testthat::expect_identical(resolved$representation_id, "subject_centered")
  testthat::expect_true(isTRUE(resolved$model_plan$subject_terms_retained))
  testthat::expect_identical(resolved$model_plan$duplicate_correlation_applied, FALSE)
  testthat::expect_true(all(paste0("subject.", subjects) %in% resolved$model_plan$column_ids))
  design <- resolved$blocks$block_1$design
  subject_columns <- grepl("^subject[.]", colnames(design))
  testthat::expect_identical(sum(subject_columns), 4L)
  aligned_subjects <- as.character(study$sample_metadata$subject)
  names(aligned_subjects) <- study$sample_metadata$sample_id
  aligned_subjects <- aligned_subjects[colnames(study$assays$block_1)]
  weights <- rep(0, ncol(design))
  names(weights) <- colnames(design)
  weights[["visit.v3"]] <- 1
  complete <- b04_oracle(assay[1, ], unname(aligned_subjects), design, weights)
  one_cell <- b04_oracle(assay[2, ], unname(aligned_subjects), design, weights)
  testthat::expect_equal(complete[["df"]], 6)
  testthat::expect_equal(complete[["rank"]], 6)
  testthat::expect_equal(one_cell[["df"]], 5)
  testthat::expect_equal(one_cell[["rank"]], 6)
  shortcut <- design[, !subject_columns, drop = FALSE]
  shortcut_df <- 12 - qr(shortcut)$rank
  testthat::expect_equal(shortcut_df, 10)
  testthat::expect_true(complete[["df"]] < shortcut_df)
  row_1 <- b04_row(ran$fit, "complete")
  row_2 <- b04_row(ran$fit, "one_cell")
  row_3 <- b04_row(ran$fit, "two_cells")
  expect_close(row_1$effect, complete[["effect"]])
  expect_close(row_1$ordinary_se, complete[["se"]])
  expect_close(row_1$effect, complete[["raw_effect"]])
  testthat::expect_equal(row_1$ordinary_df, complete[["df"]])
  testthat::expect_equal(row_1$design_rank, complete[["rank"]])
  expect_close(row_2$effect, one_cell[["effect"]])
  testthat::expect_equal(row_2$ordinary_df, one_cell[["df"]])
  testthat::expect_equal(row_2$n_rows_used, 11L)
  testthat::expect_identical(row_2$feature_pattern, "balanced_visits_one_cell_removed")
  testthat::expect_identical(row_1$duplicate_correlation_applied, FALSE)
  testthat::expect_null(row_3$effect)
  testthat::expect_identical(row_3$estimation_status, "failed")
  testthat::expect_identical(row_3$estimation_reason, "unsupported_missing_pattern")
  testthat::expect_null(row_3$p)
  testthat::expect_null(row_3$q)
  family <- ran$fit$blocks$block_1$contrasts[[1L]]$family
  testthat::expect_identical(family$n_declared, 3L)
  testthat::expect_identical(family$n_failed, 1L)
  testthat::expect_equal(family$internal_p[[3L]], 1)
  scrambled <- b04_visit_study(assay, visits, subjects, scramble = TRUE)
  scrambled_fit <- b04_fit(scrambled, analysis, c(visit.v3 = 1), "within_person_difference")
  expect_close(b04_row(scrambled_fit$fit, "complete")$effect, row_1$effect)
  testthat::expect_equal(b04_row(scrambled_fit$fit, "complete")$ordinary_df, row_1$ordinary_df)
})

testthat::test_that("B-T04 irregular time keeps subject terms when intercepts confound the clock", {
  times <- list(A = c(0, 1), B = c(10, 11))
  set.seed(7)
  noise <- rnorm(4, sd = 0.05)
  y <- c(0, 0, 100, 100) + unlist(times, use.names = FALSE) + noise
  study <- b04_time_study(times, y)
  analysis <- b04_design(
    kind = "trajectory", time_unit = "day", time_origin = "0"
  )
  ran <- b04_fit(study, analysis, c(time.linear = 1), "per_unit_beta")
  design <- ran$resolved$blocks$block_1$design
  subjects <- study$sample_metadata$subject
  weights <- rep(0, ncol(design))
  names(weights) <- colnames(design)
  weights[["time.linear"]] <- 1
  oracle <- b04_oracle(y, subjects, design, weights)
  row <- b04_row(ran$fit, "F1")
  expect_close(row$effect, oracle[["effect"]])
  expect_close(row$effect, oracle[["raw_effect"]])
  testthat::expect_equal(row$ordinary_df, 1)
  testthat::expect_equal(row$design_rank, 3)
  reduced <- qr.coef(qr(cbind(1, study$sample_metadata$time)), y)
  testthat::expect_true(abs(row$effect - reduced[[2L]]) > 1)
  testthat::expect_identical(ran$resolved$model_plan$pattern, "irregular_linear_complete")
  testthat::expect_identical(ran$resolved$model_plan$time_unit, "day")
  testthat::expect_identical(ran$resolved$model_plan$time_origin, "0")
})

testthat::test_that("B-T04 time origin, spline boundaries and unsupported covariance are explicit", {
  times <- list(A = c(10, 11, 12), B = c(10, 11, 12), C = c(10, 11, 12), D = c(10, 11, 12))
  y <- rep(c(5, 7, 9, 11), each = 3) + 1.5 * unlist(times, use.names = FALSE) + rep(c(0.2, -0.4, 0.2), 4)
  shifted <- b04_time_study(times, y)
  raw_clock <- times
  raw_clock[] <- lapply(times, function(value) value - 10)
  y_raw <- rep(c(5, 7, 9, 11), each = 3) + 1.5 * unlist(raw_clock, use.names = FALSE) + rep(c(0.2, -0.4, 0.2), 4)
  origin_zero <- b04_time_study(raw_clock, y_raw)
  fit_shift <- b04_fit(
    shifted,
    b04_design(kind = "trajectory", time_unit = "day", time_origin = "10"),
    c(time.linear = 1),
    "per_unit_beta"
  )
  fit_zero <- b04_fit(
    origin_zero,
    b04_design(kind = "trajectory", time_unit = "day", time_origin = "0"),
    c(time.linear = 1),
    "per_unit_beta"
  )
  expect_close(b04_row(fit_shift$fit, "F1")$effect, 1.5)
  expect_close(b04_row(fit_shift$fit, "F1")$effect, b04_row(fit_zero$fit, "F1")$effect)
  testthat::expect_equal(b04_row(fit_shift$fit, "F1")$ordinary_df, 7)
  missing_origin <- b04_design(kind = "trajectory", time_unit = "day", time_origin = NULL)
  expect_code(triomics_resolve_analysis(shifted, missing_origin), "B_DESIGN_UNSUPPORTED")
  partial_spline <- b04_design(
    kind = "trajectory", time_unit = "day", time_origin = "10",
    spline_knots = list(2), spline_boundary = list()
  )
  expect_code(triomics_resolve_analysis(shifted, partial_spline), "B_DESIGN_UNSUPPORTED")
  outside <- b04_design(
    kind = "trajectory", time_unit = "day", time_origin = "10",
    spline_knots = list(10.5), spline_boundary = c(10, 11)
  )
  expect_code(triomics_resolve_analysis(shifted, outside), "B_REPEATED_MODEL_UNSUPPORTED")
  visits <- c("baseline", "followup")
  paired_like <- b04_visit_study(c(1, 2, 1, 3, 2, 4), visits, c("A", "B", "C"))
  expect_code(
    triomics_resolve_analysis(paired_like, b04_design(reference_level = "baseline", target_level = "followup")),
    "B_DESIGN_UNSUPPORTED"
  )
})

testthat::test_that("B-T04 natural spline contrasts use the declared knots and boundaries", {
  times <- list(A = 0:4, B = 0:4, C = 0:4, D = 0:4)
  set.seed(11)
  y <- rep(c(1, 2, 4, 8), each = 5) + rnorm(20, sd = 0.2)
  study <- b04_time_study(times, y)
  analysis <- b04_design(
    kind = "trajectory", time_unit = "day", time_origin = "0",
    spline_knots = list(2), spline_boundary = c(0, 4)
  )
  ran <- b04_fit(study, analysis, c(time.ns1 = 1), "per_unit_beta")
  basis <- splines::ns(unlist(times), knots = 2, Boundary.knots = c(0, 4))
  subjects <- study$sample_metadata$subject
  subject_matrix <- sapply(unique(subjects), function(subject) as.numeric(subjects == subject))
  design <- cbind(subject_matrix, basis)
  oracle <- as.numeric(qr.coef(qr(design), y)[5])
  expect_close(b04_row(ran$fit, "F1")$effect, oracle)
  testthat::expect_identical(ran$resolved$model_plan$basis, "natural_spline")
  testthat::expect_identical(ran$resolved$model_plan$spline_knots, 2)
  testthat::expect_identical(ran$resolved$model_plan$spline_boundary, c(0, 4))
  moved <- b04_design(
    kind = "trajectory", time_unit = "day", time_origin = "0",
    spline_knots = list(1.5), spline_boundary = c(0, 4)
  )
  moved_resolved <- triomics_resolve_analysis(study, moved)
  testthat::expect_false(identical(
    ran$resolved$model_plan$basis_hash,
    moved_resolved$model_plan$basis_hash
  ))
  testthat::expect_equal(b04_row(ran$fit, "F1")$ordinary_df, 20 - (4 + 2))
})

testthat::test_that("B-T04 calibration executes the prespecified ordinary and null protocol", {
  result <- triomics_b04_calibrate()
  testthat::expect_identical(result$status, "pass")
  testthat::expect_true(isTRUE(result$executed))
  testthat::expect_false(isTRUE(result$skipped))
  testthat::expect_identical(result$protocol$n_studies, 400L)
  testthat::expect_identical(result$protocol$n_features, 100L)
  testthat::expect_identical(result$protocol$seeds[[1L]], 1001L)
  testthat::expect_identical(result$protocol$seeds[[400L]], 1400L)
  testthat::expect_identical(result$protocol$rejection_upper_bound, 0.08)
  patterns <- c(
    "balanced_complete_visits",
    "balanced_visits_one_cell_removed",
    "balanced_linear_complete",
    "irregular_linear_complete",
    "natural_spline_complete"
  )
  testthat::expect_identical(names(result$patterns), patterns)
  for (pattern in patterns) {
    cell <- result$patterns[[pattern]]
    testthat::expect_true(isTRUE(cell$executed))
    testthat::expect_false(isTRUE(cell$skipped))
    testthat::expect_identical(cell$ordinary_reference$status, "pass")
    testthat::expect_true(cell$ordinary_reference$max_abs_effect <= 1e-8)
    testthat::expect_true(cell$ordinary_reference$centered_matches_raw)
    testthat::expect_identical(cell$null$status, "pass")
    testthat::expect_identical(cell$null$n_studies, 400L)
    testthat::expect_identical(cell$null$n_features, 100L)
    testthat::expect_identical(cell$null$seed_first, 1001L)
    testthat::expect_identical(cell$null$seed_last, 1400L)
    testthat::expect_true(cell$null$rejection_upper <= 0.08)
    testthat::expect_true(cell$null$coverage_lower >= 0.90)
    testthat::expect_true(cell$null$n_intervals == 40000L)
  }
  subjects <- rep(paste0("S", 1:5), each = 3)
  visits <- rep(c("v1", "v2", "v3"), times = 5)
  subject_matrix <- sapply(paste0("S", 1:5), function(subject) as.numeric(subjects == subject))
  visit_matrix <- cbind(as.numeric(visits == "v2"), as.numeric(visits == "v3"))
  design <- cbind(subject_matrix, visit_matrix)
  weights <- c(rep(0, 5), 0, 1)
  previous <- RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  on.exit(RNGkind(previous[[1L]], previous[[2L]], previous[[3L]]), add = TRUE)
  set.seed(1001)
  generated <- matrix(stats::rnorm(15 * 100), nrow = 15)
  centered <- generated
  for (subject in unique(subjects)) {
    index <- which(subjects == subject)
    centered[index, ] <- sweep(centered[index, , drop = FALSE], 2L, colMeans(centered[index, , drop = FALSE]), "-")
  }
  decomposition <- qr(design)
  coefficients <- qr.coef(decomposition, centered)
  residual <- centered - qr.fitted(decomposition, centered)
  df <- nrow(design) - decomposition$rank
  sigma2 <- colSums(residual^2) / df
  variance <- sum(weights * solve(crossprod(design), weights))
  effect <- sum(weights * coefficients[, 1L])
  se <- sqrt(sigma2[[1L]] * variance)
  stored <- result$patterns$balanced_complete_visits$ordinary_null$first_feature
  expect_close(stored$effect, effect)
  expect_close(stored$ordinary_se, se)
  testthat::expect_equal(stored$df, df)
  testthat::expect_equal(stored$rank, decomposition$rank)
  testthat::expect_equal(stored$p, 2 * stats::pt(-abs(effect / se), df))
})

testthat::test_that("B-T04 calibration and publication share moderated method, contrast and statistics", {
  result <- triomics_b04_calibrate()
  testthat::expect_identical(result$status, "pass")
  removed <- result$patterns$balanced_visits_one_cell_removed$ordinary_null
  testthat::expect_identical(removed$status, "fail")
  testthat::expect_identical(removed$n_rejections, 23L)
  testthat::expect_equal(removed$rejection_upper, 0.0804819, tolerance = 5e-7)
  testthat::expect_identical(result$patterns$balanced_visits_one_cell_removed$null$n_rejections, 15L)

  patterns <- names(result$patterns)
  for (pattern in patterns) {
    cell <- result$patterns[[pattern]]
    ordinary <- cell$ordinary_null
    moderated <- cell$null
    testthat::expect_identical(ordinary$method, "ordinary_fixed_subject_qr")
    testthat::expect_length(ordinary$feature_support$rank, 100L)
    testthat::expect_equal(ordinary$feature_support$residual_df,
                           ordinary$feature_support$n_observed - ordinary$feature_support$rank)
    testthat::expect_true(all(is.finite(ordinary$feature_support$effect)))
    testthat::expect_true(all(is.finite(ordinary$feature_support$ordinary_se)))
    testthat::expect_true(all(is.finite(ordinary$feature_support$p)))
    testthat::expect_identical(moderated$provenance$method, "limma")
    testthat::expect_identical(moderated$provenance$fit, "limma::lmFit")
    testthat::expect_identical(moderated$provenance$contrast, "limma::contrasts.fit")
    testthat::expect_identical(moderated$provenance$moderation, "limma::eBayes")
    testthat::expect_identical(moderated$provenance$trend, FALSE)
    testthat::expect_identical(moderated$provenance$robust, FALSE)
    testthat::expect_true(moderated$rejection_upper <= 0.08)
    testthat::expect_true(moderated$coverage_lower >= 0.90)
    testthat::expect_identical(moderated$coverage_lower_method,
                               "one_sided_t_over_independent_study_coverage_proportions")
    testthat::expect_identical(moderated$n_study_coverage, 400L)
    testthat::expect_identical(moderated$interval_coverage_bound_status, "descriptive_only")
  }

  if (!identical(result$status, "pass")) return(invisible(NULL))
  subjects <- rep(paste0("S", 1:5), each = 3)
  visits <- rep(c("v1", "v2", "v3"), times = 5)
  previous <- RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  on.exit(RNGkind(previous[[1L]], previous[[2L]], previous[[3L]]), add = TRUE)
  set.seed(1001)
  generated <- matrix(stats::rnorm(15 * 100), nrow = 15)
  centered <- generated
  for (subject in unique(subjects)) {
    index <- which(subjects == subject)
    centered[index, ] <- sweep(centered[index, , drop = FALSE], 2L,
                               colMeans(centered[index, , drop = FALSE]), "-")
  }
  layout <- triomics_b04_layout("balanced_complete_visits")
  contrast <- matrix(layout$weights, ncol = 1L,
                     dimnames = list(names(layout$weights), "within"))
  reference <- suppressWarnings(limma::eBayes(
    limma::contrasts.fit(limma::lmFit(t(centered), layout$design), contrast),
    trend = FALSE,
    robust = FALSE
  ))
  calibrated <- result$patterns$balanced_complete_visits$null$first_feature
  reference_se <- sqrt(reference$s2.post[[1L]]) * reference$stdev.unscaled[1L, 1L]
  testthat::expect_identical(result$patterns$balanced_complete_visits$null$provenance$contrast_id, "within")
  expect_close(calibrated$effect, reference$coefficients[1L, 1L])
  expect_close(calibrated$moderated_se, reference_se)
  testthat::expect_equal(unname(calibrated$p), unname(reference$p.value[1L, 1L]))
  testthat::expect_equal(calibrated$effective_df, reference$df.total[[1L]])

  study <- b04_visit_study(t(generated), c("v1", "v2", "v3"), paste0("S", 1:5))
  published <- b04_fit(study, b04_design(), c(visit.v3 = 1), "within_person_difference")
  published_contrast <- published$fit$blocks$block_1$contrasts$within
  row <- b04_row(published$fit, "F1")
  testthat::expect_identical(published_contrast$contrast_id,
                             result$patterns$balanced_complete_visits$null$provenance$contrast_id)
  expect_close(row$p, calibrated$p)
  expect_close(row$moderated_se, calibrated$moderated_se)
  testthat::expect_equal(row$effective_test_df, calibrated$effective_df)
  testthat::expect_identical(row$statistic_name, "moderated_t")
})

testthat::test_that("B-T04 refuses robust moderation without a separate qualification", {
  study <- b04_visit_study(rep(c(1, 2, 4), times = 4))
  resolved <- triomics_resolve_analysis(study, b04_design())
  statistics <- b04_statistics(
    resolved$model_plan$column_ids,
    c(visit.v3 = 1),
    "within_person_difference"
  )
  statistics$robust <- TRUE
  expect_code(triomics_fit_block_models(resolved, statistics), "B_DESIGN_UNSUPPORTED")
})

testthat::test_that("B-T04 refuses two-visit relabelling, covariates, between-person terms and rank sensitivity", {
  subjects <- LETTERS[1:4]
  visits <- c("v1", "v2", "v3")
  y <- rep(c(1, 2, 3, 4), each = 3) + rep(c(0, 0.2, 0.4), 4)
  study <- b04_visit_study(y, visits, subjects)
  expect_code(
    triomics_resolve_analysis(study, b04_design(covariates = list(list(
      variable = "age", role = "modifier", form = "delta", rationale = "not this lane"
    )))),
    "B_DESIGN_UNSUPPORTED"
  )
  expect_code(
    triomics_resolve_analysis(study, b04_design(between_group = "group")),
    "B_DESIGN_UNSUPPORTED"
  )
  expect_code(
    triomics_resolve_analysis(study, b04_design(representation = "levels")),
    "B_DESIGN_UNSUPPORTED"
  )
  ran <- b04_fit(study, b04_design(), c(visit.v3 = 1), "within_person_difference")
  sensitive <- triomics_fit_block_models(
    ran$resolved,
    b04_statistics(ran$resolved$model_plan$column_ids, c(visit.v3 = 1), "within_person_difference",
                   rank_sensitivity = TRUE)
  )
  testthat::expect_identical(
    sensitive$blocks$block_1$contrasts[[1L]]$sensitivity$status,
    "unavailable"
  )
  testthat::expect_false(is.null(b04_row(sensitive, "F1")$effect))
  wrong_unit <- b04_statistics(
    ran$resolved$model_plan$column_ids, c(visit.v3 = 1), "log2_fold_change"
  )
  expect_code(triomics_fit_block_models(ran$resolved, wrong_unit), "B_DESIGN_EFFECT_UNIT")
  deqms <- triomics_fit_block_models(
    ran$resolved,
    b04_statistics(ran$resolved$model_plan$column_ids, c(visit.v3 = 1), "within_person_difference",
                   method = "deqms")
  )
  testthat::expect_identical(deqms$status, "unavailable")
  testthat::expect_false(isTRUE(deqms$fallback_applied))
  log_study <- b04_visit_study(y, visits, subjects, scale = "log2")
  log_fit <- b04_fit(log_study, b04_design(), c(visit.v3 = 1), "log2_fold_change")
  testthat::expect_identical(b04_row(log_fit$fit, "F1")$effect_unit, "log2_fold_change")
})

testthat::test_that("B-T04 does not capture the two-visit paired lane", {
  samples <- c("A_b", "A_f", "B_b", "B_f")
  assay <- matrix(c(1, 3, 2, 6), nrow = 1L, dimnames = list("F", samples))
  study <- list(
    study_id = "synthetic_b_t03",
    preparation_context_id = "b_t03",
    sample_metadata = data.frame(
      sample_id = samples,
      subject = rep(c("A", "B"), each = 2L),
      visit = rep(c("baseline", "followup"), 2L),
      stringsAsFactors = FALSE
    ),
    sample_registry = NULL,
    declared_factor_levels = list(visit = c("baseline", "followup")),
    feature_registry = data.frame(
      feature_key = "F", block_id = "block_1", scale = "linear", stringsAsFactors = FALSE
    ),
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  analysis <- list(
    analysis_id = "paired",
    block_ids = "block_1",
    preparation_context_id = "b_t03",
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
  testthat::expect_false(isTRUE(triomics_b04_owns(study, analysis)))
  resolved <- triomics_resolve_analysis(study, analysis)
  testthat::expect_identical(resolved$lane, "b_t03")
  testthat::expect_identical(resolved$representation_id, "delta")
})

testthat::test_that("B-T04 CLI fixture fits the qualified linear trajectory", {
  root <- testthat::test_path("..", "..")
  output <- file.path(tempdir(), paste0("b-t04-", Sys.getpid()))
  unlink(output, recursive = TRUE)
  old_wd <- getwd()
  setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  lines <- system2(
    "Rscript",
    c(
      "--vanilla", "inst/exec/triomics",
      "--config", "inst/examples/tickets/B-T04.yaml",
      "--stage", "B",
      "--output", output
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status_code <- attr(lines, "status")
  if (is.null(status_code)) status_code <- 0L
  if (!identical(status_code, 0L)) cat(paste(lines, collapse = "\n"), "\n")
  testthat::expect_identical(status_code, 0L)
  payload <- jsonlite::fromJSON(file.path(output, "B", "block-results.json"), simplifyVector = FALSE)
  row <- payload$blocks$block_1$contrasts$per_synthetic_step$rows[[1L]]
  expect_close(row$effect, 1.5)
  testthat::expect_equal(row$ordinary_df, 7)
  testthat::expect_equal(row$design_rank, 5)
  testthat::expect_identical(row$duplicate_correlation_applied, FALSE)
  testthat::expect_identical(payload$representation_id, "subject_centered")
  calibration <- jsonlite::fromJSON(file.path(output, "calibration.json"), simplifyVector = FALSE)
  testthat::expect_identical(calibration$status, "pass")
  testthat::expect_true(isTRUE(calibration$executed))
  testthat::expect_false(isTRUE(calibration$skipped))
  acceptance <- jsonlite::fromJSON(file.path(output, "acceptance-record.json"), simplifyVector = FALSE)
  verdicts <- vapply(acceptance$acceptance, function(item) item$verdict, character(1))
  testthat::expect_true(all(verdicts == "pass"))
})
