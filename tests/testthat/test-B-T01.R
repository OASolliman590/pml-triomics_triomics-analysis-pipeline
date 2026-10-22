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

b_groups <- function() rep(c("control", "treatment"), each = 3L)

b_statistics <- function(weights = NULL, effect_unit = "group_difference", robust = FALSE,
                         rank_sensitivity = FALSE, method = "limma", count_metadata = NULL,
                         fallback = "none", selection_validity = "qualified",
                         contrasts = NULL) {
  if (is.null(contrasts)) {
    if (is.null(weights)) {
      weights <- list(condition.control = -1, condition.treatment = 1)
    }
    contrasts <- list(list(
      contrast_id = "target_minus_reference",
      weights = weights,
      interpretation = "Synthetic target minus reference."
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
    count_metadata = count_metadata,
    fallback = fallback
  )
}

b_analysis <- function(block_ids = "block_1", kind = "independent", representation = "levels",
                       repeated = FALSE, covariates = list(), subject_id = NULL) {
  list(
    analysis_id = "primary",
    block_ids = block_ids,
    preparation_context_id = "b_t01_levels",
    design = list(
      kind = kind,
      condition = "group",
      condition_type = "factor",
      subject_id = subject_id,
      repeated = repeated,
      representation = representation,
      reference_level = "control",
      target_level = "treatment",
      covariates = covariates,
      missing_visits = "error"
    )
  )
}

b_study <- function(assays, groups = b_groups(), scales = NULL, level_order = c("control", "treatment"),
                    subjects = NULL, assay_hashes = NULL, unit_map = NULL) {
  if (is.matrix(assays)) assays <- list(block_1 = assays)
  else if (is.numeric(assays)) assays <- list(block_1 = matrix(assays, nrow = 1L))
  block_ids <- names(assays)
  if (is.null(scales)) scales <- stats::setNames(rep("linear", length(block_ids)), block_ids)
  sample_ids <- paste0("S", seq_along(groups))
  registry <- list()
  stored <- list()
  for (block_id in block_ids) {
    assay <- assays[[block_id]]
    if (is.null(dim(assay))) assay <- matrix(assay, nrow = 1L)
    if (is.null(colnames(assay))) colnames(assay) <- sample_ids
    if (is.null(rownames(assay))) rownames(assay) <- paste0(block_id, "_F", seq_len(nrow(assay)))
    stored[[block_id]] <- assay
    registry[[block_id]] <- data.frame(
      feature_key = rownames(assay),
      block_id = block_id,
      original_id = rownames(assay),
      scale = scales[[block_id]],
      quantity = "intensity",
      units = "synthetic_arbitrary_units",
      stringsAsFactors = FALSE
    )
  }
  metadata <- data.frame(sample_id = sample_ids, group = groups, stringsAsFactors = FALSE)
  if (!is.null(subjects)) metadata$subject_id <- subjects
  list(
    study_id = "synthetic_b_t01",
    preparation_context_id = "b_t01_levels",
    sample_metadata = metadata,
    sample_registry = data.frame(
      sample_id = sample_ids,
      subject_id = if (is.null(subjects)) rep(NA_character_, length(sample_ids)) else subjects,
      visit_id = rep(NA_character_, length(sample_ids)),
      sample_role = rep("primary", length(sample_ids)),
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(group = level_order),
    feature_registry = do.call(rbind, registry),
    assays = stored,
    assay_hashes = assay_hashes,
    analysis_unit_map = unit_map
  )
}

b_fit <- function(assays, statistics = b_statistics(), analysis = NULL, ...) {
  study <- if (is.list(assays) && !is.null(assays$assays)) assays else b_study(assays, ...)
  if (is.null(analysis)) analysis <- b_analysis(names(study$assays))
  resolved <- triomics_resolve_analysis(study, analysis)
  list(resolved = resolved, fit = triomics_fit_block_models(resolved, statistics))
}

b_rows <- function(fit, block = "block_1", contrast = "target_minus_reference") {
  fit$blocks[[block]]$contrasts[[contrast]]$rows
}

b_row <- function(fit, feature, block = "block_1", contrast = "target_minus_reference") {
  rows <- b_rows(fit, block, contrast)
  hit <- Filter(function(row) identical(row$feature_key, feature), rows)
  testthat::expect_length(hit, 1L)
  hit[[1L]]
}

b_stage_cli_smoke <- function(config_path, output) {
  source_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  cli <- file.path(source_root, "inst", "exec", "triomics")
  rscript <- file.path(R.home("bin"), "Rscript")
  original_wd <- getwd()
  setwd(dirname(cli))
  on.exit(setwd(original_wd), add = TRUE)
  lines <- system2(
    rscript,
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", "B", "--output", shQuote(output)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(lines, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = lines)
}

b_qr <- function(y, groups, weights = c(control = -1, treatment = 1)) {
  ok <- is.finite(y)
  present <- factor(groups[ok], levels = c("control", "treatment"))
  design <- stats::model.matrix(~ 0 + present)
  colnames(design) <- paste0("condition.", levels(present))
  contrast <- unname(weights[c("control", "treatment")])
  decomposition <- qr(design)
  coefficients <- qr.coef(decomposition, y[ok])
  coefficients[is.na(coefficients)] <- 0
  residual <- y[ok] - as.numeric(qr.fitted(decomposition, y[ok]))
  df <- sum(ok) - decomposition$rank
  sigma2 <- sum(residual^2) / df
  variance <- sum(contrast * solve(crossprod(design), contrast))
  c(effect = sum(contrast * coefficients), se = sqrt(sigma2 * variance), df = df)
}

oracle_p <- function() {
  2 * stats::pt(-abs(2 / sqrt(2 / 3)), 4)
}

testthat::test_that("explicit two-group contrasts match the hand-derived effect, SE, and df", {
  fitted <- b_fit(c(1, 2, 3, 3, 4, 5))
  testthat::expect_identical(fitted$resolved$model_plan$formula, "~0 + condition")
  testthat::expect_identical(
    fitted$resolved$model_plan$column_ids,
    c("condition.control", "condition.treatment")
  )
  row <- b_row(fitted$fit, "block_1_F1")
  expect_close(row$effect, 2)
  expect_close(row$ordinary_se, sqrt(2 / 3))
  testthat::expect_equal(row$ordinary_df, 4)
  expect_close(row$p, oracle_p())
  testthat::expect_identical(row$statistic_name, "moderated_t")
  testthat::expect_false(identical(row$inferential_status, "eligible"))
  testthat::expect_false(identical(row$inferential_status, "complete"))

  reversed_levels <- b_study(c(1, 2, 3, 3, 4, 5), level_order = c("treatment", "control"))
  refit <- b_fit(reversed_levels)
  expect_close(b_row(refit$fit, "block_1_F1")$effect, 2)
  testthat::expect_identical(
    refit$resolved$model_plan$column_ids,
    c("condition.treatment", "condition.control")
  )
})

testthat::test_that("reversing a contrast reverses the effect but not the two-sided p", {
  contrasts <- list(
    list(contrast_id = "target_minus_reference",
         weights = list(condition.control = -1, condition.treatment = 1),
         interpretation = "Forward synthetic contrast."),
    list(contrast_id = "reference_minus_target",
         weights = list(condition.control = 1, condition.treatment = -1),
         interpretation = "Reversed synthetic contrast.")
  )
  fitted <- b_fit(c(1, 2, 3, 3, 4, 5), statistics = b_statistics(contrasts = contrasts))
  forward <- b_row(fitted$fit, "block_1_F1", contrast = "target_minus_reference")
  reverse <- b_row(fitted$fit, "block_1_F1", contrast = "reference_minus_target")
  expect_close(forward$effect, 2)
  expect_close(reverse$effect, -2)
  expect_close(forward$ordinary_se, reverse$ordinary_se)
  testthat::expect_equal(forward$ordinary_df, reverse$ordinary_df)
  testthat::expect_identical(forward$p, reverse$p)
  testthat::expect_identical(forward$ordinary_se, reverse$ordinary_se)

  scaled <- b_fit(c(1, 2, 3, 3, 4, 5), statistics = b_statistics(
    weights = list(condition.control = -2, condition.treatment = 2)
  ))
  scaled_row <- b_row(scaled$fit, "block_1_F1")
  expect_close(scaled_row$effect, 4)
  expect_close(scaled_row$ordinary_se, 2 * sqrt(2 / 3))
  testthat::expect_equal(scaled_row$ordinary_df, 4)
})

testthat::test_that("unsupported designs and malformed contrasts fail closed", {
  study <- b_study(c(1, 2, 3, 3, 4, 5))
  paired <- b_analysis(kind = "paired", representation = "delta")
  expect_code(triomics_resolve_analysis(study, paired), "B_DESIGN_UNSUPPORTED")
  covariate <- b_analysis(covariates = list(list(
    variable = "age", role = "confounder", form = "level", rationale = "Must not be dropped."
  )))
  expect_code(triomics_resolve_analysis(study, covariate), "B_DESIGN_UNSUPPORTED")
  repeated <- b_analysis(subject_id = "subject_id")
  duplicated <- b_study(c(1, 2, 3, 3, 4, 5), subjects = c("A", "A", "B", "C", "D", "E"))
  expect_code(triomics_resolve_analysis(duplicated, repeated), "B_DESIGN_UNSUPPORTED")

  resolved <- triomics_resolve_analysis(study, b_analysis())
  expect_code(triomics_fit_block_models(resolved, b_statistics(
    weights = list(group.control = -1, group.treatment = 1)
  )), "B_DESIGN_CONTRAST")
  expect_code(triomics_fit_block_models(resolved, b_statistics(
    weights = list(condition.treatment = 1)
  )), "B_DESIGN_CONTRAST")
  expect_code(triomics_fit_block_models(resolved, b_statistics(
    weights = list(condition.control = "treatment-control", condition.treatment = 1)
  )), "B_DESIGN_CONTRAST")
  bad_trend <- b_statistics()
  bad_trend$trend <- TRUE
  expect_code(triomics_fit_block_models(resolved, bad_trend), "B_DESIGN_UNSUPPORTED")
  expect_code(triomics_fit_block_models(resolved, b_statistics(effect_unit = "log2_fold_change")),
              "B_DESIGN_EFFECT_UNIT")
  expect_code(triomics_fit_block_models(resolved, b_statistics(effect_unit = "delta_difference")),
              "B_DESIGN_EFFECT_UNIT")

  logged <- b_study(c(1, 2, 3, 3, 4, 5), scales = c(block_1 = "log2"))
  logged_fit <- b_fit(logged, statistics = b_statistics(effect_unit = "log2_fold_change"))
  logged_row <- b_row(logged_fit$fit, "block_1_F1")
  expect_close(logged_row$effect, 2)
  testthat::expect_identical(logged_row$effect_unit, "log2_fold_change")
})

testthat::test_that("moderated statistics match installed limma rather than a mocked success", {
  testthat::expect_true(requireNamespace("limma", quietly = TRUE))
  set.seed(1)
  values <- rbind(
    c(1, 2, 3, 3, 4, 5),
    c(0, 0, 0, 10, 10, 10),
    matrix(stats::rnorm(8L * 6L, sd = 0.2), nrow = 8L)
  )
  groups <- b_groups()
  reference <- function(robust) {
    design <- stats::model.matrix(~ 0 + factor(groups, levels = c("control", "treatment")))
    colnames(design) <- c("condition.control", "condition.treatment")
    contrast <- matrix(c(-1, 1), ncol = 1L, dimnames = list(colnames(design), "target_minus_reference"))
    fit <- limma::eBayes(
      limma::contrasts.fit(limma::lmFit(values, design), contrast),
      trend = FALSE,
      robust = robust
    )
    unscaled <- fit$stdev.unscaled
    if (is.null(dim(unscaled))) unscaled <- matrix(unscaled, ncol = 1L)
    list(
      moderated_se = as.numeric(sqrt(fit$s2.post) * unscaled[, 1L]),
      statistic = as.numeric(fit$t),
      df_total = as.numeric(fit$df.total),
      df_prior = as.numeric(if (length(fit$df.prior) == 1L) rep(fit$df.prior, nrow(values)) else fit$df.prior),
      ordinary_se = as.numeric(fit$sigma * unscaled[, 1L]),
      p = as.numeric(fit$p.value)
    )
  }
  plain <- reference(FALSE)
  testthat::expect_true(any(abs(plain$moderated_se - plain$ordinary_se) > 1e-6))
  fitted <- b_fit(values)
  rows <- b_rows(fitted$fit)
  expect_close(vapply(rows, function(row) row$moderated_se, numeric(1)), plain$moderated_se)
  expect_close(vapply(rows, function(row) row$statistic, numeric(1)), plain$statistic)
  expect_close(vapply(rows, function(row) row$effective_test_df, numeric(1)), plain$df_total)
  expect_close(vapply(rows, function(row) row$df_prior, numeric(1)), plain$df_prior)
  expect_close(vapply(rows, function(row) row$ordinary_se, numeric(1)), plain$ordinary_se)
  expect_close(vapply(rows, function(row) row$p, numeric(1)), plain$p)
  testthat::expect_identical(fitted$fit$limma_version, as.character(packageVersion("limma")))

  robust_reference <- reference(TRUE)
  robust_fit <- b_fit(values, statistics = b_statistics(robust = TRUE))
  robust_rows <- b_rows(robust_fit$fit)
  expect_close(vapply(robust_rows, function(row) row$moderated_se, numeric(1)), robust_reference$moderated_se)
  expect_close(vapply(robust_rows, function(row) row$statistic, numeric(1)), robust_reference$statistic)
  expect_close(vapply(robust_rows, function(row) row$effective_test_df, numeric(1)), robust_reference$df_total)
  testthat::expect_true(any(abs(
    vapply(robust_rows, function(row) row$statistic, numeric(1)) -
      vapply(rows, function(row) row$statistic, numeric(1))
  ) > 1e-6))
})

testthat::test_that("missing rows, rank, and zero residual support are accounted per feature", {
  groups <- b_groups()
  values <- rbind(
    complete = c(1, 2, 3, 3, 4, 5),
    partial = c(1, 2, NA, 3, 4, 5),
    missing_group = c(NA, NA, NA, 3, 4, 5),
    zero_df = c(1, NA, NA, 5, NA, NA)
  )
  fitted <- b_fit(values)
  complete <- b_row(fitted$fit, "complete")
  partial <- b_row(fitted$fit, "partial")
  missing_group <- b_row(fitted$fit, "missing_group")
  zero_df <- b_row(fitted$fit, "zero_df")
  expect_close(complete$effect, 2)
  expect_close(complete$ordinary_se, sqrt(2 / 3))
  testthat::expect_equal(complete$ordinary_df, 4)
  qr_partial <- b_qr(values["partial", ], groups)
  expect_close(partial$effect, unname(qr_partial[["effect"]]))
  expect_close(partial$ordinary_se, unname(qr_partial[["se"]]))
  testthat::expect_equal(partial$ordinary_df, unname(qr_partial[["df"]]))
  testthat::expect_identical(partial$estimation_status, "estimated")

  testthat::expect_null(missing_group$effect)
  testthat::expect_null(missing_group$ordinary_se)
  testthat::expect_null(missing_group$p)
  testthat::expect_null(missing_group$q)
  testthat::expect_identical(missing_group$estimation_status, "failed")
  testthat::expect_identical(missing_group$estimation_reason, "contrast_not_estimable")
  testthat::expect_identical(missing_group$inferential_status, "not_estimable")

  expect_close(zero_df$effect, 4)
  testthat::expect_null(zero_df$ordinary_se)
  testthat::expect_null(zero_df$moderated_se)
  testthat::expect_null(zero_df$p)
  testthat::expect_null(zero_df$q)
  testthat::expect_identical(zero_df$inferential_status, "not_estimable")
  testthat::expect_identical(zero_df$estimation_reason, "zero_residual_support")
  borrowed_design <- stats::model.matrix(~ 0 + factor(groups, levels = c("control", "treatment")))
  colnames(borrowed_design) <- c("condition.control", "condition.treatment")
  borrowed_contrast <- matrix(
    c(-1, 1), ncol = 1L,
    dimnames = list(colnames(borrowed_design), "target_minus_reference")
  )
  set.seed(1)
  witnessed <- rbind(
    values[c("complete", "zero_df"), , drop = FALSE],
    matrix(stats::rnorm(8L * 6L, sd = 0.2), nrow = 8L)
  )
  rownames(witnessed) <- c("complete", "zero_df", paste0("extra", seq_len(nrow(witnessed) - 2L)))
  borrowed <- suppressWarnings(limma::eBayes(limma::contrasts.fit(limma::lmFit(
    witnessed, borrowed_design
  ), borrowed_contrast), trend = FALSE))
  testthat::expect_true(is.finite(borrowed$p.value["zero_df", 1L]))
  witnessed_fit <- b_fit(witnessed)
  testthat::expect_null(b_row(witnessed_fit$fit, "zero_df")$p)
  testthat::expect_null(b_row(witnessed_fit$fit, "zero_df")$ordinary_se)
  expect_close(b_row(witnessed_fit$fit, "zero_df")$effect, 4)
  testthat::expect_null(zero_df$p)

  family <- fitted$fit$blocks$block_1$contrasts$target_minus_reference$family
  testthat::expect_identical(family$n_declared, 4L)
  testthat::expect_identical(family$n_estimated + family$n_failed, family$n_declared)
  testthat::expect_identical(family$n_failed, 2L)
  testthat::expect_identical(family$internal_p[match(c("missing_group", "zero_df"), family$member_ids)], c(1, 1))
  testthat::expect_identical(sum(family$dispositions), family$n_declared)
  testthat::expect_identical(family$validation$status, "withheld")
  testthat::expect_false(family$q_published)
  testthat::expect_true("selection_inventory_missing" %in% family$validation$reasons)
})

testthat::test_that("failed members enter BH at internal p=1 and another block cannot change q", {
  groups <- b_groups()
  alone <- b_fit(c(1, 2, 3, 3, 4, 5))
  mixed <- b_fit(rbind(c(1, 2, 3, 3, 4, 5), c(NA, NA, NA, 3, 4, 5)))
  p_value <- b_row(alone$fit, "block_1_F1")$p
  expect_close(b_row(mixed$fit, "block_1_F1")$p, p_value)
  expected_q <- stats::p.adjust(c(p_value, 1), method = "BH")
  alone_result <- alone$fit$blocks$block_1$contrasts$target_minus_reference
  mixed_result <- mixed$fit$blocks$block_1$contrasts$target_minus_reference
  expect_close(alone_result$family$internal_q[[1L]], p_value)
  expect_close(mixed_result$family$internal_q[[1L]], expected_q[[1L]])
  testthat::expect_false(alone_result$family$q_published)
  testthat::expect_false(mixed_result$family$q_published)
  testthat::expect_true("selection_inventory_missing" %in% alone_result$family$validation$reasons)
  testthat::expect_true("selection_inventory_missing" %in% mixed_result$family$validation$reasons)
  testthat::expect_null(b_row(alone$fit, "block_1_F1")$q)
  testthat::expect_null(b_row(mixed$fit, "block_1_F1")$q)
  testthat::expect_null(b_row(mixed$fit, "block_1_F2")$q)
  testthat::expect_identical(
    mixed$fit$blocks$block_1$contrasts$target_minus_reference$family$internal_p[[2L]],
    1
  )

  block_2 <- matrix(c(0, 0, 0, 8, 8, 8, 1, 1, 1, 1, 1.2, 1.4), nrow = 2L, byrow = TRUE)
  both <- b_fit(list(block_1 = matrix(c(1, 2, 3, 3, 4, 5), nrow = 1L), block_2 = block_2))
  both_result <- both$fit$blocks$block_1$contrasts$target_minus_reference
  expect_close(both_result$family$internal_q, alone_result$family$internal_q)
  testthat::expect_null(b_row(both$fit, "block_1_F1", block = "block_1")$q)
  testthat::expect_true("selection_inventory_missing" %in% both_result$family$validation$reasons)
  testthat::expect_false(identical(
    both$fit$blocks$block_1$contrasts$target_minus_reference$family$family_id,
    both$fit$blocks$block_2$contrasts$target_minus_reference$family$family_id
  ))

  descriptive <- b_fit(c(1, 2, 3, 3, 4, 5), statistics = b_statistics(selection_validity = "descriptive_only"))
  descriptive_row <- b_row(descriptive$fit, "block_1_F1")
  testthat::expect_false(is.null(descriptive_row$p))
  testthat::expect_null(descriptive_row$q)
  testthat::expect_identical(descriptive_row$inferential_status, "descriptive_only")
  descriptive_family <- descriptive$fit$blocks$block_1$contrasts$target_minus_reference$family
  testthat::expect_identical(descriptive_family$validation$status, "withheld")
  testthat::expect_false(descriptive_family$q_published)
  testthat::expect_true("selection_inventory_missing" %in% descriptive_family$validation$reasons)
  testthat::expect_false(is.null(
    descriptive$fit$blocks$block_1$contrasts$target_minus_reference$family$internal_q
  ))
})

testthat::test_that("effect unit and independent directional consistency are explicit and hashed", {
  fitted <- b_fit(c(1, 2, 3, 3, 4, 5))
  row <- b_row(fitted$fit, "block_1_F1")
  testthat::expect_identical(row$effect_unit, "group_difference")
  testthat::expect_identical(row$direction_status, "not_applicable")
  testthat::expect_identical(row$direction_reason, "independent_samples_have_no_within_person_delta")
  testthat::expect_null(row$direction_numerator)
  testthat::expect_null(row$direction_denominator)
  testthat::expect_null(row$direction_zero_count)
  testthat::expect_null(row$n_pairs)
  testthat::expect_match(row$source_hash, "^[0-9a-f]{64}$")
  testthat::expect_match(row$model_hash, "^[0-9a-f]{64}$")
  testthat::expect_match(row$representation_hash, "^[0-9a-f]{64}$")
  testthat::expect_match(row$contrast_hash, "^[0-9a-f]{64}$")

  ordered <- b_study(matrix(c(1, 2, 3, 3, 4, 5), nrow = 1L, dimnames = list("F", paste0("S", 1:6))))
  permuted_assay <- ordered$assays$block_1[, c(4:6, 1:3), drop = FALSE]
  permuted <- ordered
  permuted$assays$block_1 <- permuted_assay
  permuted$sample_metadata <- ordered$sample_metadata[c(4:6, 1:3), , drop = FALSE]
  permuted$sample_registry <- ordered$sample_registry[c(4:6, 1:3), , drop = FALSE]
  original_fit <- b_fit(ordered)
  permuted_fit <- b_fit(permuted)
  testthat::expect_identical(b_row(original_fit$fit, "F")$source_hash, b_row(permuted_fit$fit, "F")$source_hash)
  expect_close(b_row(original_fit$fit, "F")$effect, b_row(permuted_fit$fit, "F")$effect)

  shifted <- ordered
  shifted$assays$block_1[1L, 1L] <- shifted$assays$block_1[1L, 1L] + 1
  shifted_fit <- b_fit(shifted)
  testthat::expect_false(identical(b_row(original_fit$fit, "F")$source_hash, b_row(shifted_fit$fit, "F")$source_hash))
  testthat::expect_false(abs(b_row(original_fit$fit, "F")$effect - b_row(shifted_fit$fit, "F")$effect) < 1e-8)

  mislabeled <- ordered
  mislabeled$assays$block_1 <- ordered$assays$block_1[, rev(seq_len(6L)), drop = FALSE]
  mislabeled_fit <- b_fit(mislabeled)
  expect_close(b_row(mislabeled_fit$fit, "F")$effect, 2)

  full <- matrix(c(1, 2, 3, 3, 4, 5), nrow = 1L, dimnames = list("F", paste0("S", 1:6)))
  dropped <- full[, -1L, drop = FALSE]
  separated <- b_fit(list(block_1 = full, block_2 = dropped))
  expect_close(b_row(separated$fit, "F", block = "block_1")$effect, 2)
  expect_close(b_row(separated$fit, "F", block = "block_1")$ordinary_se, sqrt(2 / 3))
  dropped_qr <- b_qr(
    as.numeric(dropped),
    c("control", "control", "treatment", "treatment", "treatment")
  )
  expect_close(b_row(separated$fit, "F", block = "block_2")$effect, unname(dropped_qr[["effect"]]))
  expect_close(b_row(separated$fit, "F", block = "block_2")$ordinary_se, unname(dropped_qr[["se"]]))

  clr <- b_study(c(1, 2, 3, 3, 4, 5), scales = c(block_1 = "clr_log2"))
  clr_fit <- b_fit(clr, statistics = b_statistics(effect_unit = "clr_contrast"))
  testthat::expect_identical(b_row(clr_fit$fit, "block_1_F1")$effect_unit, "clr_contrast")
  expect_close(b_row(clr_fit$fit, "block_1_F1")$effect, 2)
})

testthat::test_that("declared hashes and unit maps are checked before numeric fitting", {
  study <- b_study(c(1, 2, 3, 3, 4, 5))
  study$assay_hashes <- list(block_1 = paste(rep("ab", 32L), collapse = ""))
  expect_code(triomics_resolve_analysis(study, b_analysis()), "B_DESIGN_HASH")
  mismatched <- b_study(c(1, 2, 3, 3, 4, 5))
  mismatched$analysis_unit_map <- data.frame(
    unit_id = c(paste0("S", 2:6), "OTHER"),
    representation_id = rep("levels", 6L),
    stringsAsFactors = FALSE
  )
  expect_code(triomics_resolve_analysis(mismatched, b_analysis()), "B_DESIGN_UNIT_MAP")
})

testthat::test_that("DEqMS is unavailable and counts do not change the limma fit", {
  plain <- b_fit(c(1, 2, 3, 3, 4, 5))
  counted <- b_fit(c(1, 2, 3, 3, 4, 5), statistics = b_statistics(count_metadata = "missing-counts.tsv"))
  testthat::expect_identical(b_row(plain$fit, "block_1_F1")$effect, b_row(counted$fit, "block_1_F1")$effect)
  testthat::expect_identical(b_row(plain$fit, "block_1_F1")$p, b_row(counted$fit, "block_1_F1")$p)
  testthat::expect_null(b_row(counted$fit, "block_1_F1")$count_prior_ref)
  testthat::expect_identical(
    counted$fit$blocks$block_1$contrasts$target_minus_reference$count_prior_status,
    "not_used_no_automatic_deqms"
  )
  unavailable <- b_fit(c(1, 2, 3, 3, 4, 5), statistics = b_statistics(
    method = "deqms", fallback = "none", count_metadata = "counts.tsv"
  ))
  testthat::expect_identical(unavailable$fit$status, "unavailable")
  testthat::expect_false(isTRUE(unavailable$fit$fallback_applied))
  testthat::expect_length(unavailable$fit$blocks, 0L)
  testthat::expect_null(unavailable$fit$effects)
})

testthat::test_that("rank sensitivity stays a separate Wilcoxon result", {
  values <- rbind(c(1, 2, 3, 3, 4, 5), c(0, 0, 1, 2, 4, 9))
  plain <- b_fit(values)
  ranked <- b_fit(values, statistics = b_statistics(rank_sensitivity = TRUE))
  testthat::expect_identical(b_rows(plain$fit)[[1L]]$p, b_rows(ranked$fit)[[1L]]$p)
  testthat::expect_identical(b_rows(plain$fit)[[1L]]$effect, b_rows(ranked$fit)[[1L]]$effect)
  sensitivity <- ranked$fit$blocks$block_1$contrasts$target_minus_reference$sensitivity
  testthat::expect_false(isTRUE(sensitivity$substitutes_for_primary))
  testthat::expect_false(isTRUE(sensitivity$equivalent_to_moderated_test))
  testthat::expect_identical(sensitivity$method, "wilcoxon_rank_sum")
  testthat::expect_false(isTRUE(sensitivity$exact))
  testthat::expect_true(isTRUE(sensitivity$continuity_correction))
  reference <- stats::wilcox.test(c(3, 4, 5), c(1, 2, 3), exact = FALSE, correct = TRUE)
  expect_close(sensitivity$features[[1L]]$p, unname(reference$p.value))
  testthat::expect_false(identical(sensitivity$features[[1L]]$p, b_rows(ranked$fit)[[1L]]$p))
  testthat::expect_identical(
    plain$fit$blocks$block_1$contrasts$target_minus_reference$sensitivity$status,
    "not_requested"
  )
})

testthat::test_that("the stage B fixture envelope matches limma on predecessor matrices", {
  config_path <- triomics_test_resource("examples", "tickets", "B-T01.yaml")
  output <- tempfile("b-t01-stage-")
  dir.create(output)
  written <- triomics_execute_stage_b(config_path, output)
  payload_path <- file.path(output, "B", "block-results.json")
  envelope_path <- file.path(output, "B", "block-results.envelope.json")
  testthat::expect_true(file.exists(payload_path))
  testthat::expect_true(triomics_validate_envelope(envelope_path))
  payload <- jsonlite::read_json(payload_path, simplifyVector = FALSE)
  testthat::expect_identical(payload$preparation_refit, FALSE)
  testthat::expect_identical(payload$filter_applied, FALSE)
  testthat::expect_identical(payload$limma_version, as.character(packageVersion("limma")))
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  for (block_id in c("block_1", "block_2")) {
    block <- payload$blocks[[block_id]]
    assay <- ingested$assays[[block_id]]
    groups <- ingested$sample_metadata$group[match(colnames(assay), ingested$sample_metadata$sample_id)]
    design <- stats::model.matrix(~ 0 + factor(groups, levels = c("control", "treatment")))
    colnames(design) <- c("condition.control", "condition.treatment")
    contrast <- matrix(c(-1, 1), ncol = 1L, dimnames = list(colnames(design), "target_minus_reference"))
    reference <- limma::eBayes(limma::contrasts.fit(limma::lmFit(assay, design), contrast), trend = FALSE, robust = FALSE)
    rows <- block$contrasts$target_minus_reference$rows
    testthat::expect_identical(length(rows), nrow(assay))
    expect_close(vapply(rows, function(row) row$effect, numeric(1)), as.numeric(reference$coefficients))
    hashes <- vapply(rows, function(row) row$source_hash, character(1))
    testthat::expect_true(all(hashes == hashes[[1L]]))
    testthat::expect_true(all(vapply(rows, function(row) identical(row$effect_unit, "group_difference"), logical(1))))
    testthat::expect_true(all(vapply(rows, function(row) {
      identical(row$direction_status, "not_applicable") &&
        identical(row$direction_reason, "independent_samples_have_no_within_person_delta") &&
        is.null(row$direction_numerator)
    }, logical(1))))
  }
  testthat::expect_false(identical(
    payload$blocks$block_1$contrasts$target_minus_reference$rows[[1L]]$source_hash,
    payload$blocks$block_2$contrasts$target_minus_reference$rows[[1L]]$source_hash
  ))
  testthat::expect_identical(written$status, "complete")

  ticket_dir <- dirname(config_path)
  refused_config <- tempfile("b-t01-refused-", tmpdir = ticket_dir, fileext = ".yaml")
  budget_config <- tempfile("b-t01-budget-", tmpdir = ticket_dir, fileext = ".yaml")
  on.exit(unlink(c(refused_config, budget_config)), add = TRUE)
  config_text <- paste(readLines(config_path), collapse = "\n")
  refused_text <- sub(
    "transform:\n        method: none",
    "transform:\n        method: log2",
    config_text,
    fixed = TRUE
  )
  writeLines(refused_text, refused_config)
  refused_output <- tempfile("b-t01-refused-out-")
  dir.create(refused_output)
  expect_code(triomics_execute_stage_b(refused_config, refused_output), "B_DESIGN_PREPARATION")
  testthat::expect_false(file.exists(file.path(refused_output, "B", "block-results.envelope.json")))

  budget_text <- sub("max_fits: 40", "max_fits: 1", config_text, fixed = TRUE)
  writeLines(budget_text, budget_config)
  budget_output <- tempfile("b-t01-budget-out-")
  dir.create(budget_output)
  expect_code(triomics_execute_stage_b(budget_config, budget_output), "B_DESIGN_BUDGET")
})

testthat::test_that("Stage B CLI writes unique, deterministic BlockResult identities across contrasts", {
  config_path <- triomics_test_resource("examples", "tickets", "B-T02.yaml")
  first_output <- tempfile("b-t02-identity-first-")
  second_output <- tempfile("b-t02-identity-second-")
  dir.create(first_output)
  dir.create(second_output)
  on.exit(unlink(c(first_output, second_output), recursive = TRUE), add = TRUE)

  first_run <- b_stage_cli_smoke(config_path, first_output)
  second_run <- b_stage_cli_smoke(config_path, second_output)
  testthat::expect_identical(first_run$status, 0L, info = paste(first_run$output, collapse = "\n"))
  testthat::expect_identical(second_run$status, 0L, info = paste(second_run$output, collapse = "\n"))
  first <- jsonlite::read_json(file.path(first_output, "B", "block-results.json"), simplifyVector = FALSE)
  second <- jsonlite::read_json(file.path(second_output, "B", "block-results.json"), simplifyVector = FALSE)
  claims <- first$blocks$block_1$contrasts
  claim_ids <- names(claims)
  testthat::expect_length(claim_ids, 2L)

  ids <- character()
  numeric_fields <- list()
  scientific_fields <- c(
    "feature_key", "family_id", "effect", "effect_unit", "ordinary_se",
    "moderated_se", "statistic", "statistic_name", "ordinary_df",
    "effective_test_df", "df_prior", "p", "q", "inferential_status",
    "estimation_status", "estimation_reason", "n_samples", "n_subjects",
    "n_rows_used", "count_prior_ref", "source_hash", "model_hash",
    "representation_hash", "contrast_hash", "reference_direction_status",
    "direction_status", "direction_reason", "direction_numerator",
    "direction_denominator", "direction_zero_count", "n_pairs"
  )
  serialized_science <- list()
  second_science <- list()
  for (claim_id in claim_ids) {
    claim <- claims[[claim_id]]
    second_claim <- second$blocks$block_1$contrasts[[claim_id]]
    for (index in seq_along(claim$rows)) {
      row <- claim$rows[[index]]
      testthat::expect_true(all(scientific_fields %in% names(row)))
      projection_key <- paste(claim_id, index, sep = ":")
      serialized_science[[projection_key]] <- row[scientific_fields]
      second_science[[projection_key]] <- second_claim$rows[[index]][scientific_fields]
      testthat::expect_true(is.character(row$result_id) && length(row$result_id) == 1L)
      testthat::expect_match(if (is.null(row$result_id)) "" else as.character(row$result_id),
                             "^[a-f0-9]{64}$")
      testthat::expect_identical(as.character(row$study_id), "triomics_b_t02_fixture")
      testthat::expect_identical(as.character(row$analysis_id), "synthetic_primary")
      testthat::expect_identical(as.character(row$block_id), "block_1")
      testthat::expect_identical(as.character(row$claim_type), "scalar_contrast")
      testthat::expect_identical(as.character(row$claim_id), claim_id)
      testthat::expect_identical(as.character(row$contrast_id), claim_id)
      testthat::expect_identical(as.character(row$representation_id), "levels")
      identity <- list(
        study_id = first$study_id,
        analysis_id = first$analysis_id,
        block_id = "block_1",
        claim_type = "scalar_contrast",
        claim_id = claim_id,
        family_id = claim$family$family_id,
        feature_key = row$feature_key,
        representation_id = first$representation_id,
        contrast_id = claim_id
      )
      expected_id <- digest::digest(
        triomics_canonical_json(identity), algo = "sha256", serialize = FALSE
      )
      testthat::expect_identical(row$result_id, expected_id)
      testthat::expect_identical(
        row$result_id, as.character(second_claim$rows[[index]]$result_id)
      )
      ids <- c(ids, row$result_id)
      numeric_fields[[length(numeric_fields) + 1L]] <- row[c(
        "effect", "effect_unit", "ordinary_se", "moderated_se", "statistic",
        "effective_test_df", "p", "q", "estimation_status", "estimation_reason"
      )]
      testthat::expect_identical(
        numeric_fields[[length(numeric_fields)]],
        second_claim$rows[[index]][c(
          "effect", "effect_unit", "ordinary_se", "moderated_se", "statistic",
          "effective_test_df", "p", "q", "estimation_status", "estimation_reason"
        )]
      )
    }
  }
  testthat::expect_gt(length(ids), 0L)
  testthat::expect_length(unique(ids), length(ids))
  testthat::expect_identical(triomics_b_hash(serialized_science), triomics_b_hash(second_science))
})

testthat::test_that("public identity finalization preserves the fitted scientific rows exactly", {
  config_path <- triomics_test_resource("examples", "tickets", "B-T02.yaml")
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  analysis <- config$analyses[[1L]]
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[as.character(analysis$block_ids)],
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  fitted <- triomics_fit_block_models(resolved, analysis$statistics)
  fitted_blocks_before <- fitted$blocks
  scientific_fields <- c(
    "feature_key", "family_id", "effect", "effect_unit", "ordinary_se",
    "moderated_se", "statistic", "statistic_name", "ordinary_df",
    "effective_test_df", "df_prior", "p", "q", "inferential_status",
    "estimation_status", "estimation_reason", "n_samples", "n_subjects",
    "n_rows_used", "count_prior_ref", "source_hash", "model_hash",
    "representation_hash", "contrast_hash", "reference_direction_status",
    "direction_status", "direction_reason", "direction_numerator",
    "direction_denominator", "direction_zero_count", "n_pairs"
  )
  fitted_projection <- list()
  for (claim_id in names(fitted_blocks_before$block_1$contrasts)) {
    fitted_rows <- fitted_blocks_before$block_1$contrasts[[claim_id]]$rows
    for (index in seq_along(fitted_rows)) {
      key <- paste(claim_id, index, sep = ":")
      fitted_projection[[key]] <- fitted_rows[[index]][scientific_fields]
    }
  }
  public_payload <- triomics_b_finalize_public_result_identity(list(
    study_id = ingested$study_id,
    analysis_id = analysis$analysis_id,
    representation_id = analysis$design$representation,
    blocks = fitted$blocks
  ))
  finalized_projection <- list()
  for (claim_id in names(public_payload$blocks$block_1$contrasts)) {
    finalized_rows <- public_payload$blocks$block_1$contrasts[[claim_id]]$rows
    for (index in seq_along(finalized_rows)) {
      key <- paste(claim_id, index, sep = ":")
      finalized_projection[[key]] <- finalized_rows[[index]][scientific_fields]
    }
  }
  testthat::expect_identical(finalized_projection, fitted_projection)
  testthat::expect_identical(fitted$blocks, fitted_blocks_before)
  testthat::expect_identical(triomics_b_hash(finalized_projection), triomics_b_hash(fitted_projection))
  testthat::expect_identical(
    triomics_b_finalize_public_result_identity(public_payload), public_payload
  )
})

testthat::test_that("Stage B serializes joint claim identity without choosing a member contrast", {
  source_config <- triomics_test_resource("examples", "tickets", "B-T02.yaml")
  output <- tempfile("b-t02-joint-identity-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  original_fit <- getFromNamespace("triomics_fit_block_models", "triomics")
  testthat::local_mocked_bindings(
    triomics_fit_block_models = function(resolved_analysis, statistics_spec) {
      statistics_spec$claims <- list(list(
        claim_id = "all_declared_groups_joint",
        claim_type = "joint_contrast",
        contrast_ids = c("treatment_minus_control", "unobserved_minus_control"),
        effect_unit = statistics_spec$effect_unit
      ))
      original_fit(resolved_analysis, statistics_spec)
    },
    .package = "triomics"
  )
  triomics_execute_stage_b(source_config, output)
  payload <- jsonlite::read_json(file.path(output, "B", "block-results.json"), simplifyVector = FALSE)
  claim <- payload$blocks$block_1$joint_claims$all_declared_groups_joint
  members <- c("treatment_minus_control", "unobserved_minus_control")
  testthat::expect_identical(as.character(claim$claim_id), "all_declared_groups_joint")
  testthat::expect_identical(unlist(claim$contrast_ids, use.names = FALSE), members)
  for (row in claim$rows) {
    testthat::expect_identical(as.character(row$claim_id), "all_declared_groups_joint")
    testthat::expect_identical(as.character(row$claim_type), "joint_contrast")
    testthat::expect_identical(as.character(row$contrast_id), "joint:all_declared_groups_joint")
    testthat::expect_identical(unlist(row$contrast_ids, use.names = FALSE), members)
    identity <- list(
      study_id = payload$study_id,
      analysis_id = payload$analysis_id,
      block_id = "block_1",
      claim_type = "joint_contrast",
      claim_id = "all_declared_groups_joint",
      family_id = claim$family$family_id,
      feature_key = row$feature_key,
      representation_id = payload$representation_id,
      contrast_id = "joint:all_declared_groups_joint",
      contrast_ids = as.list(members)
    )
    expected_id <- digest::digest(
      triomics_canonical_json(identity), algo = "sha256", serialize = FALSE
    )
    testthat::expect_identical(row$result_id, expected_id)
  }
})

testthat::test_that("Stage B rejects a supplied result context that conflicts with its enclosing claim", {
  config_path <- triomics_test_resource("examples", "tickets", "B-T01.yaml")
  output <- tempfile("b-t01-conflicting-result-context-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  original_fit <- getFromNamespace("triomics_fit_block_models", "triomics")

  testthat::local_mocked_bindings(
    triomics_fit_block_models = function(resolved_analysis, statistics_spec) {
      fit <- original_fit(resolved_analysis, statistics_spec)
      rows <- fit$blocks$block_1$contrasts$target_minus_reference$rows
      rows[[1L]]$analysis_id <- "another_analysis"
      fit$blocks$block_1$contrasts$target_minus_reference$rows <- rows
      fit
    },
    .package = "triomics"
  )

  expect_code(triomics_execute_stage_b(config_path, output), "B_RESULT_CONTRACT")
})

testthat::test_that("Stage B rejects a supplied result ID that disagrees with its canonical identity", {
  config_path <- triomics_test_resource("examples", "tickets", "B-T01.yaml")
  output <- tempfile("b-t01-conflicting-result-id-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  original_fit <- getFromNamespace("triomics_fit_block_models", "triomics")

  testthat::local_mocked_bindings(
    triomics_fit_block_models = function(resolved_analysis, statistics_spec) {
      fit <- original_fit(resolved_analysis, statistics_spec)
      rows <- fit$blocks$block_1$contrasts$target_minus_reference$rows
      rows[[1L]]$result_id <- paste(rep("a", 64L), collapse = "")
      fit$blocks$block_1$contrasts$target_minus_reference$rows <- rows
      fit
    },
    .package = "triomics"
  )

  expect_code(triomics_execute_stage_b(config_path, output), "B_RESULT_CONTRACT")
})
