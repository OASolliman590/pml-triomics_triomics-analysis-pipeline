c02_error <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
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

c02_study <- function(ids, outcome, blocks) {
  block_ids <- names(blocks)
  registry <- data.frame(
    sample_id = ids, subject_id = ids, outcome = outcome,
    sample_role = rep("undeclared", length(ids)), stringsAsFactors = FALSE
  )
  masks <- lapply(blocks, function(x) {
    matrix("known_observed", nrow(x), ncol(x), dimnames = dimnames(x))
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
  ids <- sprintf("unit-%02d", seq_len(24L))
  if (identical(mode, "categorical")) {
    outcome <- factor(rep(c("control", "case"), each = 12L), levels = c("control", "case"))
    x_signal <- c(rep(-2.5, 12L), rep(2.5, 12L))
    stratification <- as.character(outcome)
    outcome_spec <- list(column = "outcome", type = "categorical", levels = levels(outcome))
    method <- "concat_plsda"
  } else {
    outcome <- seq(2, 48, by = 2)
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
  study <- c02_study(ids, outcome, blocks)
  plan <- triomics_nested_split_plan(
    ids, subject_ids = ids, n_outer = 2L, n_inner = 2L, seed = 19L,
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
  result <- evaluate_pipeline(fixture$study, fixture$analysis_spec,
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
  changed <- evaluate_pipeline(mutated, fixture$analysis_spec,
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
    length(intersect(x$training_subject_ids, x$test_subject_ids)) == 0L
  }, logical(1))))
  testthat::expect_equal(result$n_eligible,
                         result$n_predicted + result$failed_prediction_count)
  testthat::expect_equal(result$total_denominator,
                         result$n_eligible + result$n_ineligible)
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
    evaluate_pipeline(fixture$study, fixture$analysis_spec,
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
    evaluate_pipeline(fixture$study, fixture$analysis_spec,
                      fixture$integration_spec, bad),
    "A_STATE_SPLIT"
  )
  testthat::expect_match(conditionMessage(err), "shares a subject")
})
