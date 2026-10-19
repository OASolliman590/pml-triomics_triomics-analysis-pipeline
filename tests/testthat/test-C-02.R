c02_error <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

c02_evaluate_pipeline <- function(study, analysis_spec, integration_spec, split_plan) {
  adapter <- triomics_supervised_backend(integration_spec$method)
  triomics_evaluation_core(study, analysis_spec, integration_spec, split_plan, adapter)
}

c02_noop_operations <- function() {
  lapply(c("validate", "filter", "derive", "transform", "normalise", "impute"), function(stage) {
    list(stage = stage, method = "none")
  })
}

c02_feature_rows <- function(study_id, block_id, keys) {
  n <- length(keys)
  data.frame(
    study_id = rep(study_id, n), block_id = rep(block_id, n), feature_key = keys,
    original_id = keys, id_namespace = rep("synthetic", n), source_id = rep(block_id, n),
    source_row = as.character(seq_len(n)), measured_or_derived = rep("measured", n),
    quantity = rep("intensity", n), scale = rep("linear", n),
    units = rep("synthetic_arbitrary_units", n), structural_resolution = rep("unknown", n),
    alias_refs = rep("", n), parent_refs = rep("", n), annotation_route = rep("none", n),
    stringsAsFactors = FALSE
  )
}

c02_study <- function(ids, outcome, blocks, subject_ids = ids) {
  block_ids <- names(blocks)
  registry <- data.frame(
    sample_id = ids, subject_id = subject_ids, outcome = outcome,
    sample_role = rep("undeclared", length(ids)), stringsAsFactors = FALSE
  )
  masks <- lapply(blocks, function(x) {
    mask <- matrix("known_observed", nrow(x), ncol(x), dimnames = dimnames(x))
    mask[is.na(x)] <- "known_missing"
    mask
  })
  raw <- list(
    study_id = "study-c02-synthetic",
    sample_registry = registry,
    feature_registry = do.call(rbind, lapply(block_ids, function(id) {
      c02_feature_rows("study-c02-synthetic", id, rownames(blocks[[id]]))
    })),
    sample_maps = stats::setNames(lapply(block_ids, function(id) {
      data.frame(block_id = id, assay_column = ids, sample_id = ids,
                 source_column = ids, stringsAsFactors = FALSE)
    }), block_ids),
    assays = blocks,
    original_masks = masks,
    availability = do.call(rbind, lapply(block_ids, function(id) {
      data.frame(
        study_id = "study-c02-synthetic", block_id = id, sample_id = ids,
        presence = "present", cell_imputation_eligible = TRUE, stringsAsFactors = FALSE
      )
    })),
    source_manifests = list(), selection = data.frame(), column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = stats::setNames(replicate(length(block_ids), character(), FALSE), block_ids)
  )
  triomics_reset_profiles()
  triomics_register_profile("baseline", triomics_baseline_profile())
  triomics_assemble_study(raw, alignment = "strict")
}

c02_resolved <- function(blocks, response, units, mode) {
  list(
    study_id = "study-c02-synthetic", analysis_id = "analysis-c02-synthetic",
    preparation_context_id = "fold-c02-synthetic", representation_id = "fold_prepared",
    unit_ids = unname(units), outcome = response, task_mode = mode,
    blocks = stats::setNames(lapply(names(blocks), function(block_id) {
      assay <- blocks[[block_id]]
      list(
        block_id = block_id, study_id = "study-c02-synthetic",
        preparation_context_id = "fold-c02-synthetic", representation_id = "fold_prepared",
        assay = assay, sample_ids = colnames(assay), feature_keys = rownames(assay)
      )
    }), names(blocks))
  )
}

c02_backend_spec <- function(method, block_ids, ncomp = 1L) {
  list(
    method = method, block_ids = block_ids, ncomp = ncomp,
    scale_features = TRUE, prediction_rule = "centroids.dist"
  )
}

c02_eval_fixture <- function(mode = "categorical", state_dir = tempfile("c02-state-")) {
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  ids <- sprintf("unit-%02d", seq_len(32L))
  if (identical(mode, "categorical")) {
    outcome <- factor(rep(c("control", "case"), each = 16L), levels = c("control", "case"))
    x_signal <- c(rep(-2.5, 16L), rep(2.5, 16L))
    stratification <- as.character(outcome)
    outcome_spec <- list(column = "outcome", type = "categorical", levels = levels(outcome))
    method <- "concat_plsda"
  } else {
    outcome <- seq(2, 64, by = 2)
    x_signal <- outcome / 5
    stratification <- NULL
    outcome_spec <- list(column = "outcome", type = "continuous")
    method <- "concat_pls"
  }
  x_alpha <- rbind(
    signal = x_signal + rep(c(-0.15, 0.15), length(ids) / 2L),
    trend = seq_along(ids) / 7,
    ripple = sin(seq_along(ids)),
    flat = rep(3, length(ids))
  )
  x_beta <- rbind(
    signal_b = x_signal + cos(seq_along(ids)) / 10,
    trend_b = cos(seq_along(ids) / 3),
    ripple_b = sin(seq_along(ids) / 2)
  )
  colnames(x_alpha) <- colnames(x_beta) <- ids
  blocks <- list(alpha = x_alpha, beta = x_beta)
  subject_ids <- rep(sprintf("subject-%02d", seq_len(16L)), each = 2L)
  study <- c02_study(ids, outcome, blocks, subject_ids)
  plan <- triomics_nested_split_plan(
    ids, subject_ids = subject_ids, n_outer = 2L, n_inner = 2L, seed = 19L,
    stratification = stratification, target = "outcome"
  )
  operations <- c02_noop_operations()
  block_specs <- lapply(names(blocks), function(block_id) {
    list(block_id = block_id, profile_id = "baseline", operations = operations)
  })
  names(block_specs) <- NULL
  analysis_spec <- list(
    outcome = outcome_spec,
    predictive_spec = list(
      preparation_context_id = "C02-predictive", seed = 19L, state_dir = state_dir,
      blocks = block_specs
    ),
    descriptive_spec = list(
      preparation_context_id = "C02-descriptive", training_unit_ids = ids,
      seed = 19L, state_dir = state_dir, blocks = block_specs
    )
  )
  integration_spec <- list(
    method = method, block_ids = names(blocks), scale_features = TRUE,
    prediction_rule = "centroids.dist",
    candidates = list(list(id = "one-component", ncomp = 1L),
                      list(id = "two-components", ncomp = 2L)),
    max_fits = 20L
  )
  list(study = study, plan = plan, analysis_spec = analysis_spec,
       integration_spec = integration_spec, ids = ids, blocks = blocks, outcome = outcome)
}

testthat::test_that("native PLS-DA keeps block identity, declared centroids predictions, and exact subspace fractions", {
  train_ids <- paste0("train-", seq_len(8L))
  y <- factor(rep(c("control", "case"), each = 4L), levels = c("control", "case"))
  alpha <- rbind(
    a_signal = c(-2.2, -1.8, -2.0, -1.9, 2.0, 1.8, 2.2, 1.9),
    a_trend = c(1, 2, 1, 3, 4, 3, 5, 4)
  )
  beta <- rbind(b_signal = c(-1.5, -1.8, -1.6, -1.7, 1.7, 1.6, 1.9, 1.8))
  colnames(alpha) <- colnames(beta) <- train_ids
  training <- c02_resolved(list(alpha = alpha, beta = beta), y, train_ids, "categorical")
  spec <- c02_backend_spec("concat_plsda", c("alpha", "beta"), 1L)
  adapter <- triomics_supervised_backend(spec$method)
  fit <- adapter$fit(training, spec)
  common <- adapter$extract(fit, list(study_id = training$study_id), spec)$result

  new_ids <- c("new-control", "new-case")
  new_alpha <- rbind(a_signal = c(-2.1, 2.1), a_trend = c(2, 4))
  new_beta <- rbind(b_signal = c(-1.6, 1.8))
  colnames(new_alpha) <- colnames(new_beta) <- new_ids
  new_data <- c02_resolved(list(alpha = new_alpha, beta = new_beta), NULL, new_ids, "categorical")
  prediction <- adapter$predict(fit, new_data, spec)

  testthat::expect_s3_class(fit, "BackendFit")
  testthat::expect_s3_class(common, "IntegrationResult")
  testthat::expect_identical(names(common), c("scores", "loadings", "variance", "selected", "backend", "params"))
  testthat::expect_identical(names(common$loadings), c("alpha", "beta"))
  testthat::expect_identical(common$selected$alpha, c("a_signal", "a_trend"))
  testthat::expect_identical(common$selected$beta, "b_signal")
  testthat::expect_identical(fit$metadata$native_method, "mixOmics::plsda")
  testthat::expect_identical(fit$metadata$native_version, as.character(utils::packageVersion("mixOmics")))
  testthat::expect_identical(fit$native_state$training_transform$center,
                             attr(fit$native_state$model$X, "scaled:center"))
  testthat::expect_identical(prediction$prediction_rule, "centroids.dist")
  testthat::expect_identical(unname(prediction$predictions), c("control", "case"))
  testthat::expect_false("probability" %in% names(prediction))
  testthat::expect_identical(prediction$unit_ids, new_ids)

  native_scores <- fit$native_state$model$variates$X
  q <- qr.Q(qr(native_scores[, 1L, drop = FALSE]))
  native_x <- fit$native_state$model$X
  feature_map <- fit$native_state$feature_map
  for (block_id in c("alpha", "beta")) {
    columns <- feature_map$model_column[feature_map$block_id == block_id]
    block_x <- native_x[, columns, drop = FALSE]
    sse_before <- sum(block_x^2)
    residual <- block_x - q %*% crossprod(q, block_x)
    want <- (sse_before - sum(residual^2)) / sse_before
    testthat::expect_equal(unname(common$variance[block_id, "comp1"]), want, tolerance = 1e-10)
  }
  testthat::expect_identical(attr(common$variance, "metric"), "nested_training_score_subspace")
})

testthat::test_that("native scalar PLS regression returns native predictions on the declared response scale", {
  ids <- paste0("train-", seq_len(8L))
  y <- c(2, 4, 3, 5, 8, 10, 9, 11)
  alpha <- rbind(a = y + c(-0.2, 0.1, 0.2, -0.1, 0.1, -0.2, 0.2, -0.1),
                 b = seq_along(ids))
  colnames(alpha) <- ids
  training <- c02_resolved(list(alpha = alpha), y, ids, "continuous")
  spec <- c02_backend_spec("concat_pls", "alpha", 1L)
  adapter <- triomics_supervised_backend(spec$method)
  fit <- adapter$fit(training, spec)
  new_x <- rbind(a = c(3, 9), b = c(2, 8))
  colnames(new_x) <- c("new-1", "new-2")
  new_data <- c02_resolved(list(alpha = new_x), NULL, colnames(new_x), "continuous")
  actual <- adapter$predict(fit, new_data, spec)$predictions
  native_new_x <- t(new_x)
  colnames(native_new_x) <- fit$native_state$feature_map$model_column
  native <- stats::predict(fit$native_state$model, native_new_x)$predict[, 1L, 1L]

  testthat::expect_type(actual, "double")
  testthat::expect_equal(unname(actual), unname(native), tolerance = 1e-12)
  testthat::expect_identical(fit$metadata$native_method, "mixOmics::pls")
  testthat::expect_identical(fit$result_params$response_extraction$back_transform,
                             "native_prediction_on_original_response_scale")
  testthat::expect_s3_class(adapter$extract(fit, list(), spec)$result, "IntegrationResult")

  invalid <- training
  invalid$outcome <- cbind(first = y, second = y * 2)
  c02_error(adapter$validate(invalid, spec), "C_EVAL_RESPONSE_INVALID")
})

testthat::test_that("supervised integration rejects missing required predictor blocks", {
  ids <- paste0("u", seq_len(6L))
  alpha <- rbind(a = c(-2, -1, -3, 2, 1, 3))
  colnames(alpha) <- ids
  resolved <- c02_resolved(
    list(alpha = alpha), factor(rep(c("control", "case"), each = 3L)), ids, "categorical"
  )
  spec <- c02_backend_spec("concat_plsda", c("alpha", "beta"), 1L)
  adapter <- triomics_supervised_backend(spec$method)
  c02_error(adapter$validate(resolved, spec), "C_EVAL_REQUIRED_VIEW_MISSING")
})

testthat::test_that("categorical metric includes failed cases in class error and confusion denominators", {
  metric <- triomics_evaluation_metrics(
    "categorical", truth = c("A", "A", "A", "B", "B", "B"),
    prediction = c("A", "B", NA, "B", "B", "A"),
    status = c("predicted", "predicted", "failed", "predicted", "predicted", "predicted"),
    declared_levels = c("A", "B")
  )

  testthat::expect_equal(metric$balanced_error, 0.5, tolerance = 1e-12)
  testthat::expect_equal(metric$accuracy, 0.5, tolerance = 1e-12)
  testthat::expect_equal(unname(as.matrix(metric$confusion)),
                         matrix(c(1L, 1L, 1L, 1L, 2L, 0L), nrow = 2L, byrow = TRUE))
  testthat::expect_equal(metric$n_eligible, 6L)
  testthat::expect_equal(metric$n_predicted, 5L)
  testthat::expect_equal(metric$failed_prediction_count, 1L)
  testthat::expect_equal(metric$total_denominator, 6L)
})

testthat::test_that("continuous R-squared uses the training intercept, not the held-out mean", {
  metric <- triomics_evaluation_metrics(
    "continuous", truth = c(2, 4, 6), prediction = c(3, 3, 5),
    status = rep("predicted", 3L), training_intercept = 2
  )

  testthat::expect_equal(metric$rmse, 1, tolerance = 1e-12)
  testthat::expect_equal(metric$mae, 1, tolerance = 1e-12)
  testthat::expect_equal(metric$r_squared, 0.85, tolerance = 1e-12)
  testthat::expect_identical(metric$r_squared_baseline, "training_intercept")
  testthat::expect_equal(metric$r_squared_denominator, 20, tolerance = 1e-12)
})

testthat::test_that("nested evaluation tunes inside outer training and ignores held-out mutations", {
  fixture <- c02_eval_fixture("continuous")
  result <- c02_evaluate_pipeline(fixture$study, fixture$analysis_spec,
                                 fixture$integration_spec, fixture$plan)
  first <- result$outer_folds[[1L]]
  test_units <- fixture$plan$folds[[1L]]$test_unit_ids
  mutated <- fixture$study
  for (block_id in names(fixture$blocks)) {
    se <- mutated$experiments[[block_id]]
    values <- SummarizedExperiment::assay(se, "analysis")
    values[, test_units] <- 1e12
    SummarizedExperiment::assay(se, "analysis") <- values
    mutated$experiments[[block_id]] <- se
  }
  mutated$sample_registry$outcome[match(test_units, mutated$sample_registry$sample_id)] <- 1e15
  changed <- c02_evaluate_pipeline(mutated, fixture$analysis_spec,
                                  fixture$integration_spec, fixture$plan)$outer_folds[[1L]]

  testthat::expect_s3_class(result, "EvaluationResult")
  testthat::expect_length(result$outer_folds, 2L)
  testthat::expect_length(first$inner_results, 4L)
  testthat::expect_length(first$candidate_dispositions, 2L)
  testthat::expect_equal(result$inspected_fit_count, 10L)
  testthat::expect_identical(first$fold_id, "outer_01")
  testthat::expect_identical(first$preprocessing_state_hashes,
                             changed$preprocessing_state_hashes)
  testthat::expect_identical(first$chosen_candidate$id, changed$chosen_candidate$id)
  testthat::expect_identical(first$training_transforms, changed$training_transforms)
  testthat::expect_identical(first$feature_support, changed$feature_support)
  testthat::expect_identical(first$inner_results, changed$inner_results)
  testthat::expect_true(all(vapply(first$inner_results, function(x) {
    length(x$training_subject_ids) > 0L && length(x$test_subject_ids) > 0L &&
      identical(x$training_subject_ids, unique(fixture$plan$unit_subject$subject_id[
        match(x$training_unit_ids, fixture$plan$unit_subject$unit_id)])) &&
      identical(x$test_subject_ids, unique(fixture$plan$unit_subject$subject_id[
        match(x$test_unit_ids, fixture$plan$unit_subject$unit_id)])) &&
      length(intersect(x$training_subject_ids, x$test_subject_ids)) == 0L
  }, logical(1))))
  testthat::expect_equal(result$n_eligible,
                         result$n_predicted + result$failed_prediction_count)
  testthat::expect_equal(result$total_denominator,
                         result$n_eligible + result$n_ineligible)
  testthat::expect_equal(result$metrics$model$n_predicted,
                         result$metrics$baseline$n_predicted)
  testthat::expect_equal(result$metrics$model$total_denominator,
                         result$total_denominator)
  testthat::expect_identical(result$baseline_comparison$coverage, "all_eligible_units")
  testthat::expect_equal(result$baseline_comparison$n_matched, result$n_eligible)
  testthat::expect_true(all(result$requested_folds$status %in% c("eligible", "failed")))
  testthat::expect_true(all(vapply(result$outer_folds, function(x) {
    x$total_denominator == length(x$test_unit_ids) &&
      x$n_eligible + x$n_ineligible == x$total_denominator
  }, logical(1))))
  for (fold_id in names(result$outer_folds)) {
    outer <- result$outer_folds[[fold_id]]
    fold_plan <- fixture$plan$folds[[match(fold_id, vapply(fixture$plan$folds, `[[`, character(1), "fold_id"))]]
    testthat::expect_identical(outer$training_subject_ids, fold_plan$training_subject_ids)
    testthat::expect_identical(outer$test_subject_ids, fold_plan$test_subject_ids)
    testthat::expect_equal(outer$n_predicted + outer$failed_prediction_count +
                             outer$n_ineligible, outer$total_denominator)
  }
})

testthat::test_that("infeasible native rank, missing class support, and fit budgets fail explicitly", {
  ids <- paste0("u", seq_len(4L))
  x <- rbind(f1 = c(1, 2, 3, 4), f2 = c(4, 3, 2, 1))
  colnames(x) <- ids
  resolved <- c02_resolved(list(alpha = x), factor(rep("only", 4L)), ids, "categorical")
  spec <- c02_backend_spec("concat_plsda", "alpha", 1L)
  spec$class_levels <- c("only", "other")
  adapter <- triomics_supervised_backend(spec$method)
  c02_error(adapter$fit(resolved, spec), "C_EVAL_CLASS_SUPPORT")
  resolved$outcome <- factor(c("A", "B", "A", "B"), levels = c("A", "B"))
  impossible <- c02_backend_spec("concat_plsda", "alpha", 4L)
  c02_error(adapter$fit(resolved, impossible), "C_EVAL_COMPONENT_RANK")

  fixture <- c02_eval_fixture("categorical")
  fixture$integration_spec$max_fits <- 1L
  budget_error <- c02_error(
    c02_evaluate_pipeline(fixture$study, fixture$analysis_spec,
                          fixture$integration_spec, fixture$plan),
    "C_EVAL_BUDGET"
  )
  testthat::expect_match(conditionMessage(budget_error), "1 attempted native fit")
})

testthat::test_that("evaluation rejects a tampered subject-overlapping SplitPlan", {
  fixture <- c02_eval_fixture("categorical")
  bad <- fixture$plan
  bad$folds[[1L]]$test_subject_ids[[1L]] <- bad$folds[[1L]]$training_subject_ids[[1L]]
  bad$creation_hash <- NULL
  bad$creation_hash <- triomics_hash_text(triomics_canonical_json(bad))
  err <- c02_error(
    c02_evaluate_pipeline(fixture$study, fixture$analysis_spec,
                          fixture$integration_spec, bad),
    "A_STATE_SPLIT"
  )
  testthat::expect_match(conditionMessage(err), "shares a subject")
})

testthat::test_that("continuous model and baseline metrics share the same successful prediction set", {
  comparison <- triomics_evaluation_compare_metrics(
    mode = "continuous", truth = c(2, 100),
    model_prediction = c(2, NA_real_), baseline_prediction = c(0, 0),
    status = c("predicted", "failed"), training_intercept = 0,
    unit_ids = c("unit-01", "unit-02")
  )

  testthat::expect_identical(comparison$coverage, "successful_predictions_only")
  testthat::expect_identical(comparison$n_matched, 1L)
  testthat::expect_equal(comparison$model$rmse, 0)
  testthat::expect_equal(comparison$baseline$rmse, 2)
  testthat::expect_equal(comparison$model$failed_prediction_count, 1L)
  testthat::expect_equal(comparison$baseline$failed_prediction_count, 1L)
  testthat::expect_identical(comparison$failed_unit_ids, "unit-02")
})

testthat::test_that("categorical baseline uses its own valid predictions with the eligible denominator", {
  comparison <- triomics_evaluation_compare_metrics(
    mode = "categorical", truth = c("A", "B"),
    model_prediction = c("A", NA_character_), baseline_prediction = c("B", "B"),
    status = c("predicted", "failed"), declared_levels = c("A", "B"),
    unit_ids = c("unit-01", "unit-02")
  )

  testthat::expect_equal(comparison$model$accuracy, 0.5)
  testthat::expect_equal(comparison$baseline$accuracy, 0.5)
  testthat::expect_equal(comparison$model$n_eligible, 2L)
  testthat::expect_equal(comparison$baseline$n_eligible, 2L)
  testthat::expect_equal(comparison$model$failed_prediction_count, 1L)
  testthat::expect_equal(comparison$baseline$failed_prediction_count, 0L)
  testthat::expect_equal(comparison$baseline$n_predicted, 2L)
  testthat::expect_identical(comparison$baseline_failed_unit_ids, character())
})

testthat::test_that("candidate adapter parameters survive evaluation candidate expansion", {
  integration <- list(method = "mixomics_diablo", block_ids = c("alpha", "beta"),
                      scale_features = TRUE, prediction_rule = "centroids.dist")
  candidate <- list(id = "sparse", ncomp = 2L,
                    keepX = list(alpha = c(3L, 2L), beta = c(2L)),
                    backend_parameters = list(tolerance = 1e-7))
  params <- triomics_evaluation_candidate_params(integration, candidate, NULL)

  testthat::expect_identical(params$keepX, candidate$keepX)
  testthat::expect_identical(params$backend_parameters, candidate$backend_parameters)
  testthat::expect_identical(params$candidate_id, "sparse")
})

testthat::test_that("public evaluation enforces registration while packet adapters remain directly testable", {
  c02_error(triomics_evaluation_backend("concat_pca", "categorical"),
            "C_EVAL_CAPABILITY_UNAVAILABLE")
  registered <- triomics_evaluation_backend("concat_plsda", "categorical")
  testthat::expect_s3_class(registered, "BackendAdapter")
  testthat::expect_identical(registered$evaluation_modes, "categorical")
  c02_error(evaluate_pipeline(NULL, NULL, list(method = "concat_plsda"), NULL),
            "A_STATE_SPLIT")
  adapter <- triomics_supervised_backend("concat_plsda")
  testthat::expect_s3_class(adapter, "BackendAdapter")
  testthat::expect_identical(adapter$evaluation_modes, "categorical")
  c02_error(triomics_evaluation_backend("mixomics_diablo", "categorical"),
            "C_SCORE_BACKEND_UNAVAILABLE")
})

testthat::test_that("native predictions retain per-unit failure dispositions", {
  ids <- paste0("train-", seq_len(8L))
  y <- c(200, 400, 300, 500, 800, 1000, 900, 1100)
  alpha <- rbind(a = y + c(-20, 10, 20, -10, 10, -20, 20, -10), b = seq_along(ids))
  colnames(alpha) <- ids
  training <- c02_resolved(list(alpha = alpha), y, ids, "continuous")
  spec <- c02_backend_spec("concat_pls", "alpha", 1L)
  adapter <- triomics_supervised_backend(spec$method)
  fit <- adapter$fit(training, spec)
  new_x <- rbind(a = c(3, 1e308), b = c(2, 1e308))
  colnames(new_x) <- c("ordinary", "overflow")
  prediction <- adapter$predict(
    fit, c02_resolved(list(alpha = new_x), NULL, colnames(new_x), "continuous"), spec
  )

  testthat::expect_identical(prediction$status[c("ordinary", "overflow")],
                             c(ordinary = "predicted", overflow = "failed"))
  testthat::expect_true(is.finite(prediction$predictions[["ordinary"]]))
  testthat::expect_true(is.na(prediction$predictions[["overflow"]]))
  testthat::expect_identical(prediction$reason_code[["overflow"]],
                             "nonfinite_native_prediction")
  categorical <- triomics_supervised_prediction_disposition(
    c(control = "control", missing = NA_character_, unknown = "other"),
    "categorical", c("control", "case")
  )
  testthat::expect_identical(categorical$status,
                             c(control = "predicted", missing = "failed", unknown = "failed"))
  testthat::expect_identical(categorical$reason_code[["unknown"]],
                             "invalid_native_class_prediction")
})

testthat::test_that("a fold-local preparation failure remains in the evaluation ledger", {
  fixture <- c02_eval_fixture("continuous")
  first_inner <- fixture$plan$folds[[1L]]$inner[[1L]]
  selected <- first_inner$training_unit_ids
  alpha <- fixture$blocks$alpha
  alpha <- rbind(alpha, fold_failure = rep(12, length(fixture$ids)))
  alpha["fold_failure", selected] <- NA_real_
  colnames(alpha) <- fixture$ids
  blocks <- fixture$blocks
  blocks$alpha <- alpha
  fixture$study <- c02_study(fixture$ids, fixture$outcome, blocks,
                             fixture$plan$unit_subject$subject_id)
  operations <- c02_noop_operations()
  operations[[6L]]$method <- "training_median"
  fixture$analysis_spec$predictive_spec$blocks[[1L]]$operations <- operations
  descriptive_operations <- c02_noop_operations()
  fixture$analysis_spec$descriptive_spec$blocks[[1L]]$operations <- descriptive_operations

  result <- c02_evaluate_pipeline(
    fixture$study, fixture$analysis_spec,
    fixture$integration_spec, fixture$plan
  )

  failed <- result$outer_folds[["outer_01"]]$inner_results[
    vapply(result$outer_folds[["outer_01"]]$inner_results, function(x) {
      identical(x$fold_id, first_inner$fold_id)
    }, logical(1))
  ]
  testthat::expect_length(result$outer_folds, 2L)
  testthat::expect_true(length(failed) > 0L)
  testthat::expect_true(all(vapply(failed, function(x) {
    identical(x$status, "preparation_failed") &&
      identical(x$failure$code, "A_SCALE_ALL_MISSING")
  }, logical(1))))
  testthat::expect_equal(result$total_denominator, nrow(result$predictions))
  testthat::expect_true(all(result$requested_folds$status %in% c("eligible", "failed")))
})

testthat::test_that("evaluation records uncertainty disposition and subject dependence limits", {
  fixture <- c02_eval_fixture("categorical")
  result <- c02_evaluate_pipeline(
    fixture$study, fixture$analysis_spec,
    fixture$integration_spec, fixture$plan
  )

  testthat::expect_identical(result$uncertainty$method, "not_estimated")
  testthat::expect_identical(result$uncertainty$disposition, "not_estimated")
  testthat::expect_identical(result$uncertainty$resampling_unit, "subject")
  testthat::expect_match(result$uncertainty$dependence_limitations, "not independent")
})

testthat::test_that("a failed native fit consumes the fit budget", {
  ids <- paste0("native-failure-", seq_len(8L))
  x <- rbind(signal = c(1, 2, 4, 3, 5, 8, 7, 9), trend = seq_along(ids))
  colnames(x) <- ids
  resolved <- c02_resolved(list(alpha = x), rep(4, length(ids)), ids, "continuous")
  spec <- c02_backend_spec("concat_pls", "alpha", 1L)
  adapter <- triomics_supervised_backend(spec$method)
  budget <- new.env(parent = emptyenv())
  budget$count <- 0L
  budget$maximum <- 1L

  first <- c02_error(
    triomics_evaluation_budget_fit(adapter, resolved, spec, budget), "C_EVAL_NATIVE_FIT"
  )
  next_fit <- c02_error(
    triomics_evaluation_budget_fit(adapter, resolved, spec, budget), "C_EVAL_BUDGET"
  )

  testthat::expect_match(conditionMessage(first), "constant|variance|native", ignore.case = TRUE)
  testthat::expect_equal(budget$count, 1L)
  testthat::expect_match(conditionMessage(next_fit), "1 attempted native fit")
  testthat::expect_equal(budget$count, 1L)
})

testthat::test_that("inner-held-out mutation leaves learned preparation state and support frozen", {
  fixture <- c02_eval_fixture("continuous")
  outer_plan <- fixture$plan$folds[[1L]]
  inner_plan <- outer_plan$inner[[1L]]
  blocks <- lapply(fixture$blocks, function(x) x + 20)
  alpha <- blocks$alpha
  alpha <- rbind(alpha, gap = rep(24, length(fixture$ids)),
                 heldout_only = rep(NA_real_, length(fixture$ids)))
  gap_missing <- inner_plan$training_unit_ids[[1L]]
  heldout_only_units <- inner_plan$test_unit_ids
  alpha["gap", gap_missing] <- NA_real_
  alpha["heldout_only", heldout_only_units] <- 25
  colnames(alpha) <- fixture$ids
  blocks$alpha <- alpha
  study <- c02_study(fixture$ids, fixture$outcome, blocks,
                     fixture$plan$unit_subject$subject_id)
  triomics_register_metabolomics_profile()
  operations <- triomics_metabolomics_operations(
    filter_method = "prevalence", min_observed = 1L, prevalence = 0.5,
    transform = "log2", normalise = "pqn", reference = "training_median",
    impute = "training_median", scope = "global"
  )
  descriptive_operations <- lapply(operations, function(op) {
    if (identical(op$stage, "impute")) op$method <- "none"
    op
  })
  predictive_spec <- list(
    preparation_context_id = "C02-leakage-predictive", seed = 19L,
    state_dir = tempfile("c02-leakage-before-"),
    blocks = lapply(names(blocks), function(block_id) {
      list(block_id = block_id, profile_id = "metabolomics", operations = operations)
    })
  )
  descriptive_spec <- list(
    preparation_context_id = "C02-leakage-descriptive",
    training_unit_ids = fixture$ids, seed = 19L,
    state_dir = tempfile("c02-leakage-descriptive-before-"),
    blocks = lapply(names(blocks), function(block_id) {
      list(block_id = block_id, profile_id = "metabolomics", operations = descriptive_operations)
    })
  )
  original <- triomics_fold_prepare(study, predictive_spec, fixture$plan, descriptive_spec)
  mutated_blocks <- blocks
  mutated_blocks$alpha["heldout_only", heldout_only_units] <- 250
  mutated_study <- c02_study(fixture$ids, fixture$outcome, mutated_blocks,
                             fixture$plan$unit_subject$subject_id)
  triomics_register_metabolomics_profile()
  mutated_predictive <- predictive_spec
  mutated_predictive$state_dir <- tempfile("c02-leakage-after-")
  mutated_descriptive <- descriptive_spec
  mutated_descriptive$state_dir <- tempfile("c02-leakage-descriptive-after-")
  changed <- triomics_fold_prepare(mutated_study, mutated_predictive,
                                   fixture$plan, mutated_descriptive)
  original_inner <- original$folds[[1L]]$inner[[1L]]$blocks$alpha
  changed_inner <- changed$folds[[1L]]$inner[[1L]]$blocks$alpha

  testthat::expect_identical(original_inner$state_hash, changed_inner$state_hash)
  testthat::expect_identical(original_inner$feature_keys, changed_inner$feature_keys)
  testthat::expect_false("heldout_only" %in% original_inner$feature_keys)
  testthat::expect_true("gap" %in% original_inner$feature_keys)
  testthat::expect_true(is.finite(original_inner$medians[["gap"]]))
  testthat::expect_identical(original_inner$output_scale, "log2")
  testthat::expect_identical(original_inner$pqn_reference_scale,
                             changed_inner$pqn_reference_scale)
})

# C02-SPLITS

c02_split_plan <- function() {
  ids <- sprintf("unit-%02d", seq_len(16L))
  subject_ids <- rep(sprintf("subject-%02d", seq_len(8L)), each = 2L)
  triomics_nested_split_plan(
    ids, subject_ids = subject_ids, n_outer = 2L, n_inner = 2L, seed = 19L, target = "outcome"
  )
}

testthat::test_that("C02 split provenance returns nonempty disjoint subjects from the validated plan", {
  plan <- c02_split_plan()
  for (fold in plan$folds) {
    sides <- triomics_c02_subjects_from_split(plan, fold$fold_id)
    testthat::expect_named(sides, c("train", "test"))
    testthat::expect_type(sides$train, "character")
    testthat::expect_type(sides$test, "character")
    testthat::expect_true(length(sides$train) > 0L && length(sides$test) > 0L)
    testthat::expect_length(intersect(sides$train, sides$test), 0L)
    testthat::expect_setequal(sides$train, fold$training_subject_ids)
    testthat::expect_setequal(sides$test, fold$test_subject_ids)
  }
  inner <- plan$folds[[1L]]$inner[[1L]]
  inner_sides <- triomics_c02_subjects_from_split(plan, inner$fold_id)
  testthat::expect_setequal(inner_sides$train, inner$training_subject_ids)
  testthat::expect_setequal(inner_sides$test, inner$test_subject_ids)
  testthat::expect_length(intersect(inner_sides$train, inner_sides$test), 0L)
})

testthat::test_that("C02 split provenance collapses a subject repeated within one side only", {
  plan <- c02_split_plan()
  fold <- plan$folds[[1L]]
  sides <- triomics_c02_subjects_from_split(plan, fold$fold_id)
  units <- plan$unit_subject
  repeated_train <- units$subject_id[units$unit_id %in% fold$training_unit_ids]
  repeated_test <- units$subject_id[units$unit_id %in% fold$test_unit_ids]

  testthat::expect_true(anyDuplicated(repeated_train) > 0L)
  testthat::expect_true(anyDuplicated(repeated_test) > 0L)
  testthat::expect_identical(anyDuplicated(sides$train), 0L)
  testthat::expect_identical(anyDuplicated(sides$test), 0L)
  testthat::expect_setequal(sides$train, unique(repeated_train))
  testthat::expect_setequal(sides$test, unique(repeated_test))
  testthat::expect_length(sides$train, length(fold$training_unit_ids) / 2L)
  testthat::expect_length(intersect(sides$train, sides$test), 0L)
})

testthat::test_that("C02 split provenance fails explicitly for a missing fold and an empty side", {
  plan <- c02_split_plan()
  err <- c02_error(triomics_c02_subjects_from_split(plan, "outer_99"), "C02_SPLIT_PROVENANCE")
  testthat::expect_match(conditionMessage(err), "outer_99")
  c02_error(triomics_c02_subjects_from_split(plan, NA_character_), "C02_SPLIT_PROVENANCE")

  for (side in c("training_unit_ids", "test_unit_ids")) {
    empty <- plan
    empty$folds[[1L]][[side]] <- character()
    c02_error(triomics_c02_subjects_from_split(empty, "outer_01"), "C02_SPLIT_PROVENANCE")
  }
})
