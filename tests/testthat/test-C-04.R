c04_error <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

c04_evaluate_pipeline <- function(study, analysis_spec, integration_spec, split_plan) {
  adapter <- triomics_block_spls_backend()
  triomics_evaluation_core(study, analysis_spec, integration_spec, split_plan, adapter)
}

# Public synthetic units: every value is a deterministic function of the unit index.
c04_block_values <- function(index) {
  latent <- cos(index * 0.9)
  alpha <- rbind(
    a_signal = latent + 0.1 * sin(3 * index),
    a_trend = index / 40,
    a_ripple = sin(index * 2.3),
    a_mix = 0.5 * cos(index * 1.1) + 0.3 * latent
  )
  beta <- rbind(
    b_signal = 0.8 * latent + 0.15 * cos(index * 0.8),
    b_ripple = cos(index * 1.9),
    b_trend = (40 - index) / 40
  )
  list(alpha = alpha, beta = beta, response = 3 + 2 * latent + 0.2 * sin(index * 1.7))
}

c04_resolved <- function(blocks, response, units) {
  units <- unname(units)
  list(
    study_id = "study-c04-synthetic", analysis_id = "analysis-c04-synthetic",
    preparation_context_id = "fold-c04-synthetic", representation_id = "fold_prepared",
    unit_ids = units, outcome = response, task_mode = "continuous",
    blocks = stats::setNames(lapply(names(blocks), function(block_id) {
      assay <- blocks[[block_id]]
      list(
        block_id = block_id, study_id = "study-c04-synthetic",
        preparation_context_id = "fold-c04-synthetic", representation_id = "fold_prepared",
        assay = assay, sample_ids = colnames(assay), feature_keys = rownames(assay)
      )
    }), names(blocks))
  )
}

c04_training <- function(index = seq_len(12L), flat = FALSE) {
  ids <- sprintf("train-%02d", index)
  values <- c04_block_values(index)
  alpha <- values$alpha
  if (flat) alpha <- rbind(alpha, a_flat = rep(3, length(index)))
  colnames(alpha) <- ids
  beta <- values$beta
  colnames(beta) <- ids
  c04_resolved(list(alpha = alpha, beta = beta), stats::setNames(values$response, ids), ids)
}

c04_new_units <- function(index = 33:36) {
  ids <- sprintf("new-%02d", index)
  values <- c04_block_values(index)
  alpha <- values$alpha
  beta <- values$beta
  colnames(alpha) <- colnames(beta) <- ids
  c04_resolved(list(alpha = alpha, beta = beta), NULL, ids)
}

c04_design <- function(block_ids, response_block = "outcome") {
  ids <- c(block_ids, response_block)
  design <- matrix(1, length(ids), length(ids), dimnames = list(ids, ids))
  diag(design) <- 0
  design
}

c04_spec <- function(ncomp = 2L, keepX = NULL, keepY = 1L, block_ids = c("alpha", "beta")) {
  if (is.null(keepX)) {
    keepX <- list(alpha = rep(2L, ncomp), beta = rep(2L, ncomp))
  }
  list(
    method = "mixomics_block_spls", block_ids = block_ids, response_block = "outcome",
    mode = "regression", ncomp = as.integer(ncomp), keepX = keepX, keepY = keepY,
    design = c04_design(block_ids), scale_features = TRUE, block_scaling = "none",
    prediction_rule = "AveragedPredict"
  )
}

c04_feature_rows <- function(study_id, block_id, keys) {
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

c04_study <- function(ids, outcome, blocks, subject_ids = ids) {
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
    study_id = "study-c04-synthetic",
    sample_registry = registry,
    feature_registry = do.call(rbind, lapply(block_ids, function(id) {
      c04_feature_rows("study-c04-synthetic", id, rownames(blocks[[id]]))
    })),
    sample_maps = stats::setNames(lapply(block_ids, function(id) {
      data.frame(block_id = id, assay_column = ids, sample_id = ids,
                 source_column = ids, stringsAsFactors = FALSE)
    }), block_ids),
    assays = blocks,
    original_masks = masks,
    availability = do.call(rbind, lapply(block_ids, function(id) {
      data.frame(
        study_id = "study-c04-synthetic", block_id = id, sample_id = ids,
        presence = "present", cell_imputation_eligible = TRUE, stringsAsFactors = FALSE
      )
    })),
    source_manifests = list(), selection = data.frame(), column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = stats::setNames(rep(list(character()), length(block_ids)), block_ids)
  )
  triomics_reset_profiles()
  triomics_register_profile("baseline", triomics_baseline_profile())
  triomics_assemble_study(raw, alignment = "strict")
}

c04_noop_operations <- function() {
  lapply(c("validate", "filter", "derive", "transform", "normalise", "impute"), function(stage) {
    list(stage = stage, method = "none")
  })
}

c04_eval_fixture <- function(state_dir = tempfile("c04-state-")) {
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  index <- seq_len(32L)
  ids <- sprintf("unit-%02d", index)
  values <- c04_block_values(index)
  blocks <- list(alpha = values$alpha, beta = values$beta)
  for (block_id in names(blocks)) colnames(blocks[[block_id]]) <- ids
  outcome <- stats::setNames(values$response, ids)
  subject_ids <- rep(sprintf("subject-%02d", seq_len(16L)), each = 2L)
  study <- c04_study(ids, outcome, blocks, subject_ids)
  plan <- triomics_nested_split_plan(
    ids, subject_ids = subject_ids, n_outer = 2L, n_inner = 2L, seed = 19L, target = "outcome"
  )
  operations <- c04_noop_operations()
  block_specs <- lapply(names(blocks), function(block_id) {
    list(block_id = block_id, profile_id = "baseline", operations = operations)
  })
  names(block_specs) <- NULL
  analysis_spec <- list(
    outcome = list(column = "outcome", type = "continuous"),
    predictive_spec = list(
      preparation_context_id = "C04-predictive", seed = 19L, state_dir = state_dir,
      blocks = block_specs
    ),
    descriptive_spec = list(
      preparation_context_id = "C04-descriptive", training_unit_ids = ids,
      seed = 19L, state_dir = state_dir, blocks = block_specs
    )
  )
  integration_spec <- c04_spec(ncomp = 2L)
  integration_spec$candidates <- list(
    list(id = "one-component", ncomp = 1L, keepX = list(alpha = 2L, beta = 2L)),
    list(id = "two-components", ncomp = 2L, keepX = list(alpha = c(2L, 2L), beta = c(2L, 2L)))
  )
  integration_spec$max_fits <- 20L
  list(study = study, plan = plan, analysis_spec = analysis_spec,
       integration_spec = integration_spec, ids = ids, blocks = blocks, outcome = outcome)
}

c04_native_scores <- function(fit, block_ids, n_components) {
  stats::setNames(lapply(block_ids, function(block_id) {
    scores <- fit$native_state$model$variates[[block_id]]
    colnames(scores) <- paste0("comp", seq_len(n_components))
    scores
  }), block_ids)
}

c04_native_loadings <- function(fit, block_ids, n_components) {
  stats::setNames(lapply(block_ids, function(block_id) {
    loadings <- fit$native_state$model$loadings[[block_id]]
    colnames(loadings) <- paste0("comp", seq_len(n_components))
    loadings
  }), block_ids)
}

testthat::test_that("C-04-AC1 scalar continuous Y is a native n-by-1 response under declared regression settings", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()
  spec <- c04_spec(ncomp = 2L)
  fit <- adapter$fit(training, spec)
  model <- fit$native_state$model
  new_units <- c04_new_units()
  prediction <- adapter$predict(fit, new_units, spec)

  testthat::expect_s3_class(fit, "BackendFit")
  testthat::expect_s3_class(model, "block.spls")
  testthat::expect_identical(dim(model$X$Y), c(12L, 1L))
  testthat::expect_identical(colnames(model$X$Y), "outcome")
  testthat::expect_identical(model$mode, "regression")
  testthat::expect_identical(fit$metadata$native_method, "mixOmics::block.spls")
  testthat::expect_identical(fit$metadata$native_version, as.character(utils::packageVersion("mixOmics")))
  testthat::expect_identical(fit$result_params$mode, "regression")
  testthat::expect_identical(fit$result_params$keepY, 1L)
  testthat::expect_true(fit$result_params$scale_features)
  testthat::expect_identical(fit$result_params$back_transform,
                             "native_prediction_on_original_response_scale")
  testthat::expect_identical(fit$result_params$task, "continuous_regression")
  testthat::expect_false(fit$result_params$classification)

  # A 1-column matrix is the same native response as the vector form.
  as_matrix <- training
  as_matrix$outcome <- matrix(training$outcome, ncol = 1L,
                              dimnames = list(names(training$outcome), "outcome"))
  testthat::expect_identical(adapter$fit(as_matrix, spec)$result_params$training_state_hash,
                             fit$result_params$training_state_hash)

  # Predictions are the native AveragedPredict on the original response scale.
  new_x <- lapply(c("alpha", "beta"), function(block_id) t(new_units$blocks[[block_id]]$assay))
  names(new_x) <- c("alpha", "beta")
  native <- stats::predict(model, newdata = new_x)$AveragedPredict[, 1L, 2L]
  testthat::expect_equal(unname(prediction$predictions), unname(native), tolerance = 1e-12)
  testthat::expect_identical(prediction$prediction_rule, "AveragedPredict")
  testthat::expect_identical(prediction$prediction_kind, "continuous_response")

  # Back-transform check: an affine change of Y maps predictions by the same affine change.
  shifted <- training
  shifted$outcome <- 10 * training$outcome + 5
  shifted_fit <- adapter$fit(shifted, spec)
  shifted_prediction <- adapter$predict(shifted_fit, new_units, spec)
  testthat::expect_equal(unname(shifted_prediction$predictions),
                         unname(10 * prediction$predictions + 5), tolerance = 1e-8)
})

testthat::test_that("C-04-AC1 negative: multivariate responses and undeclared regression settings fail explicitly", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()
  spec <- c04_spec(ncomp = 2L)

  two_columns <- training
  two_columns$outcome <- cbind(first = training$outcome, second = 2 * training$outcome)
  c04_error(adapter$validate(two_columns, spec), "C_EVAL_RESPONSE_INVALID")

  no_keepY <- spec
  no_keepY$keepY <- NULL
  c04_error(adapter$validate(training, no_keepY), "C_SCORE_PARAMETER")

  two_keepY <- spec
  two_keepY$keepY <- 2L
  c04_error(adapter$validate(training, two_keepY), "C_SCORE_PARAMETER")

  canonical <- spec
  canonical$mode <- "canonical"
  c04_error(adapter$validate(training, canonical), "C_SCORE_PARAMETER")

  unscaled_undeclared <- spec
  unscaled_undeclared$scale_features <- NULL
  c04_error(adapter$validate(training, unscaled_undeclared), "C_SCORE_PARAMETER")

  over_rank <- c04_spec(ncomp = 20L)
  c04_error(adapter$validate(training, over_rank), "C_EVAL_COMPONENT_RANK")
})

testthat::test_that("C-04-AC1 wrapper-controlled tuning selects through the native fit and enforces the fit budget", {
  fixture <- c04_eval_fixture()
  result <- c04_evaluate_pipeline(fixture$study, fixture$analysis_spec,
                                 fixture$integration_spec, fixture$plan)

  testthat::expect_s3_class(result, "EvaluationResult")
  testthat::expect_identical(result$method, "mixomics_block_spls")
  testthat::expect_identical(result$mode, "continuous")
  testthat::expect_length(result$outer_folds, 2L)
  testthat::expect_true(all(vapply(result$outer_folds, function(fold) {
    fold$chosen_candidate$id %in% c("one-component", "two-components")
  }, logical(1))))
  # 2 outer folds x (2 inner folds x 2 candidates + 1 outer refit) = 10 native fits.
  testthat::expect_equal(result$inspected_fit_count, 10L)
  testthat::expect_equal(result$failed_prediction_count, 0L)
  testthat::expect_equal(result$n_predicted, length(fixture$ids))

  fixture$integration_spec$max_fits <- 1L
  budget_error <- c04_error(
    c04_evaluate_pipeline(fixture$study, fixture$analysis_spec,
                          fixture$integration_spec, fixture$plan),
    "C_EVAL_BUDGET"
  )
  testthat::expect_match(conditionMessage(budget_error), "1 attempted native fit")
})

testthat::test_that("C-04-AC2 response block is excluded from common loadings, variance and selection but retained natively", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training(flat = TRUE)
  spec <- c04_spec(ncomp = 2L)
  fit <- adapter$fit(training, spec)
  common <- adapter$extract(fit, list(study_id = training$study_id), spec)$result

  testthat::expect_s3_class(common, "IntegrationResult")
  testthat::expect_identical(names(common$loadings), c("alpha", "beta"))
  testthat::expect_identical(names(common$selected), c("alpha", "beta"))
  testthat::expect_identical(rownames(common$variance), c("alpha", "beta"))
  testthat::expect_false("outcome" %in% unlist(common$selected))
  testthat::expect_false("outcome" %in% names(common$loadings))

  native_alpha <- fit$native_state$model$loadings$alpha
  expected_selected <- rownames(native_alpha)[rowSums(native_alpha != 0) > 0]
  testthat::expect_identical(common$selected$alpha, expected_selected)
  testthat::expect_identical(rownames(common$loadings$alpha), common$selected$alpha)
  testthat::expect_false("a_flat" %in% common$selected$alpha)

  diagnostics <- fit$native_state$response_diagnostics
  testthat::expect_identical(rownames(diagnostics$loadings), "outcome")
  testthat::expect_identical(dim(diagnostics$loadings), c(1L, 2L))
  testthat::expect_identical(dim(diagnostics$variates), c(12L, 2L))
  testthat::expect_length(diagnostics$prop_expl_var, 2L)
  testthat::expect_identical(common$params$response_diagnostics$status, "native_retained_outside_common_object")
})

testthat::test_that("C-04-AC2 negative: a response named as a predictor or given predictor keepX is rejected", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()

  as_predictor <- c04_spec(ncomp = 2L, block_ids = c("alpha", "outcome"))
  c04_error(adapter$validate(training, as_predictor), "C_EVAL_RESPONSE_INVALID")

  response_keepX <- c04_spec(ncomp = 2L)
  response_keepX$keepX$outcome <- c(1L, 1L)
  c04_error(adapter$validate(training, response_keepX), "C_EVAL_RESPONSE_INVALID")

  missing_keepX <- c04_spec(ncomp = 2L)
  missing_keepX$keepX$beta <- NULL
  c04_error(adapter$validate(training, missing_keepX), "C_SCORE_PARAMETER")
})

testthat::test_that("C-04-AC3 held-out Y mutations leave the evaluation fold preprocessing, scores and predictions unchanged", {
  fixture <- c04_eval_fixture()
  result <- c04_evaluate_pipeline(fixture$study, fixture$analysis_spec,
                                 fixture$integration_spec, fixture$plan)
  first <- result$outer_folds[[1L]]
  test_units <- fixture$plan$folds[[1L]]$test_unit_ids
  mutated <- fixture$study
  mutated$sample_registry$outcome[match(test_units, mutated$sample_registry$sample_id)] <-
    1e9 + seq_along(test_units)
  changed <- c04_evaluate_pipeline(mutated, fixture$analysis_spec,
                                  fixture$integration_spec, fixture$plan)
  changed_first <- changed$outer_folds[[1L]]
  fold_rows <- result$predictions$fold_id == "outer_01"

  testthat::expect_identical(first$preprocessing_state_hashes, changed_first$preprocessing_state_hashes)
  testthat::expect_identical(first$training_transforms, changed_first$training_transforms)
  testthat::expect_identical(first$feature_support, changed_first$feature_support)
  testthat::expect_identical(first$chosen_candidate$id, changed_first$chosen_candidate$id)
  testthat::expect_identical(first$inner_results, changed_first$inner_results)
  testthat::expect_identical(result$predictions$prediction[fold_rows],
                             changed$predictions$prediction[fold_rows])
  # The mutation reached the held-out truth ledger, so the invariance is not vacuous.
  testthat::expect_false(identical(result$predictions$truth[fold_rows],
                                   changed$predictions$truth[fold_rows]))
})

testthat::test_that("C-04-AC3 held-out Y is ignored by predictor scores and predictions while training Y changes them", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()
  spec <- c04_spec(ncomp = 2L)
  fit <- adapter$fit(training, spec)
  new_units <- c04_new_units()
  baseline <- adapter$predict(fit, new_units, spec)

  heldout_y <- new_units
  heldout_y$outcome <- stats::setNames(c(1e6, -1e6, 42, 0), new_units$unit_ids)
  mutated <- adapter$predict(fit, heldout_y, spec)
  testthat::expect_identical(mutated$predictions, baseline$predictions)
  testthat::expect_identical(mutated$predictor_scores, baseline$predictor_scores)
  testthat::expect_identical(mutated$consensus_scores, baseline$consensus_scores)
  testthat::expect_identical(mutated$training_state_hash, baseline$training_state_hash)

  # Negative controls: training Y and held-out predictors both move the output.
  training_changed <- training
  training_changed$outcome <- training$outcome + 5 * sin(seq_along(training$outcome))
  changed_fit <- adapter$fit(training_changed, spec)
  testthat::expect_false(isTRUE(all.equal(
    unname(adapter$predict(changed_fit, new_units, spec)$predictions),
    unname(baseline$predictions)
  )))
  moved_x <- new_units
  moved_x$blocks$alpha$assay["a_signal", ] <- moved_x$blocks$alpha$assay["a_signal", ] + 2
  testthat::expect_false(isTRUE(all.equal(
    unname(adapter$predict(fit, moved_x, spec)$predictions),
    unname(baseline$predictions)
  )))
})

testthat::test_that("C-04-AC4 common consensus, sign-aligned loadings and training-standardized variance follow the declared contract", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training(flat = TRUE)
  spec <- c04_spec(ncomp = 2L)
  fit <- adapter$fit(training, spec)
  common <- adapter$extract(fit, list(study_id = training$study_id), spec)$result

  native_scores <- c04_native_scores(fit, c("alpha", "beta"), 2L)
  native_loadings <- c04_native_loadings(fit, c("alpha", "beta"), 2L)
  expected_state <- make_consensus_state(
    native_scores, native_loadings, training$unit_ids,
    params = list(required_views = c("alpha", "beta"))
  )
  expected_scores <- apply_consensus_state(native_scores, expected_state)

  testthat::expect_identical(common$backend, "mixomics_block_spls")
  testthat::expect_equal(common$scores, expected_scores, tolerance = 1e-12)
  testthat::expect_equal(unname(colMeans(common$scores)), c(0, 0), tolerance = 1e-10)
  testthat::expect_identical(common$params$score_definition,
                             "equal_weight_mean_of_signed_training_standardized_native_scores")
  testthat::expect_equal(common$loadings$alpha,
                         sweep(native_loadings$alpha, 2L, expected_state$signs["alpha", ], "*")[
                           common$selected$alpha, , drop = FALSE], tolerance = 1e-12)

  recomputed <- vapply(c("alpha", "beta"), function(block_id) {
    block_x <- fit$native_state$model$X[[block_id]]
    scores <- fit$native_state$model$variates[[block_id]]
    total <- sum(block_x^2)
    vapply(seq_len(ncol(scores)), function(h) {
      projected <- crossprod(scores[, h, drop = FALSE], block_x)
      sum(projected^2) / sum(scores[, h]^2) / total
    }, numeric(1))
  }, numeric(2))
  testthat::expect_identical(dim(common$variance), c(2L, 2L))
  testthat::expect_equal(as.vector(common$variance), as.vector(t(recomputed)), tolerance = 1e-12)
  testthat::expect_identical(rownames(common$variance), c("alpha", "beta"))
  testthat::expect_identical(attr(common$variance, "metric"), "native_block_projection_on_variate")
  testthat::expect_true(all(common$variance >= 0 & common$variance <= 1))

  testthat::expect_identical(common$params$feature_flow$alpha$excluded_zero_variance, "a_flat")
  testthat::expect_identical(common$params$response_diagnostics$block, "outcome")
})

testthat::test_that("C-04-AC4 labels separate continuous block.spls regression from categorical DIABLO", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()
  spec <- c04_spec(ncomp = 2L)
  fit <- adapter$fit(training, spec)
  common <- adapter$extract(fit, list(), spec)$result

  testthat::expect_identical(adapter$evaluation_modes, "continuous")
  testthat::expect_identical(common$params$task, "continuous_regression")
  testthat::expect_identical(common$params$native_method, "mixOmics::block.spls")
  testthat::expect_false(common$params$classification)
  testthat::expect_identical(common$backend, "mixomics_block_spls")
  testthat::expect_false(identical(common$backend, "mixomics_diablo"))
  testthat::expect_false(identical(common$params$native_method, "mixOmics::block.splsda"))
})

testthat::test_that("C-04-AC4 negative: categorical responses, missing predictors and invalid variance are rejected without imputation", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()
  spec <- c04_spec(ncomp = 2L)

  categorical <- training
  categorical$outcome <- factor(rep(c("control", "case"), 6L))
  categorical_error <- c04_error(adapter$validate(categorical, spec), "C_EVAL_RESPONSE_INVALID")
  testthat::expect_match(conditionMessage(categorical_error), "DIABLO")

  constant_response <- training
  constant_response$outcome <- rep(1, 12L)
  c04_error(adapter$validate(constant_response, spec), "C_EVAL_RESPONSE_INVALID")

  missing_training_predictor <- training
  missing_training_predictor$blocks$alpha$assay["a_signal", 1L] <- NA_real_
  c04_error(adapter$fit(missing_training_predictor, spec), "C_EVAL_REQUIRED_VIEW_MISSING")

  fit <- adapter$fit(training, spec)
  missing_new_predictor <- c04_new_units()
  missing_new_predictor$blocks$beta$assay["b_signal", "new-33"] <- NA_real_
  c04_error(adapter$predict(fit, missing_new_predictor, spec), "C_EVAL_REQUIRED_VIEW_MISSING")

  native_scores <- c04_native_scores(fit, c("alpha", "beta"), 2L)
  native_loadings <- c04_native_loadings(fit, c("alpha", "beta"), 2L)
  state <- make_consensus_state(native_scores, native_loadings, training$unit_ids,
                                params = list(required_views = c("alpha", "beta")))
  c04_error(apply_consensus_state(native_scores["alpha"], state), "C_SCORE_REQUIRED_VIEW_MISSING")

  tampered <- fit
  tampered$native_state$model$prop_expl_var$alpha[1L] <- 1.5
  c04_error(adapter$extract(tampered, list(), spec), "C_VARIANCE_UNAVAILABLE")

  c04_error(triomics_evaluation_require_adapter(adapter, "mixomics_block_spls", "categorical"),
            "C_EVAL_CAPABILITY_UNAVAILABLE")
})

testthat::test_that("C-04 registration boundary: the public registry does not enable block.spls before shared registration", {
  # Sol's shared registration changes this expectation; until then the public path must refuse the method.
  adapter <- triomics_integration_backend("mixomics_block_spls")
  testthat::expect_s3_class(adapter, "BackendAdapter")
  testthat::expect_identical(adapter$evaluation_modes, "continuous")
  c04_error(triomics_evaluation_backend("mixomics_diablo", "continuous"),
            "C_EVAL_CAPABILITY_UNAVAILABLE")
})

testthat::test_that("variance qualification failure keeps the native BackendFit", {
  adapter <- triomics_block_spls_backend()
  training <- c04_training()
  spec <- c04_spec(ncomp = 2L)
  box <- new.env(parent = emptyenv())
  box$n <- 0L
  trace(
    triomics_block_spls_variance,
    tracer = function() {
      box$n <- box$n + 1L
      if (identical(box$n, 1L)) stop(simpleError("forced variance failure"))
    },
    where = asNamespace("triomics"), print = FALSE
  )
  on.exit(untrace(triomics_block_spls_variance, where = asNamespace("triomics")), add = TRUE)
  fit <- adapter$fit(training, spec)
  untrace(triomics_block_spls_variance, where = asNamespace("triomics"))
  testthat::expect_s3_class(fit, "BackendFit")
  testthat::expect_s3_class(fit$native_state$model, "block.spls")
  testthat::expect_false(is.null(fit$native_state$response_diagnostics))
  testthat::expect_identical(fit$native_state$variance_failure$code, "C_VARIANCE_UNAVAILABLE")
  testthat::expect_identical(fit$result_params$variance$status, "unavailable")
  c04_error(adapter$extract(fit, list(), spec), "C_VARIANCE_UNAVAILABLE")
})

testthat::test_that("C-04 YAML fixture validates against the existing config schema with a supported block.spls request", {
  previous <- options(triomics.source_root = normalizePath(
    testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE
  ))
  on.exit(options(previous), add = TRUE)
  config <- triomics_read_config(triomics_test_resource("examples", "tickets", "C-04.yaml"))
  request <- config$analyses[[1L]]$integration[[1L]]

  testthat::expect_identical(request$method, "mixomics_block_spls")
  testthat::expect_identical(request$score_definition, "consensus_mean")
  testthat::expect_identical(request$block_scaling, "none")
  testthat::expect_identical(request$keepY, 1L)
  testthat::expect_identical(names(request$keepX), c("block_1", "block_2"))
  testthat::expect_identical(names(request$design_matrix), c("block_1", "block_2", request$outcome))
})

testthat::test_that("C-04 registration request records the applied shared contract change", {
  discovery_schema_path <- testthat::test_path(
    "..", "..", "discovery", "spec-v2", "schemas", "config.schema.json"
  )
  if (file.exists(discovery_schema_path)) {
    registration_path <- testthat::test_path(
      "..", "..", "docs", "implementation", "registrations", "C-04.json"
    )
    testthat::expect_true(
      file.exists(registration_path),
      info = "The source registration record for C-04 is missing."
    )
    registration <- jsonlite::read_json(registration_path, simplifyVector = TRUE)

    testthat::expect_identical(registration$packet_id, "C-04")
    testthat::expect_true(registration$applied_to_shared_files)
    testthat::expect_identical(registration$integration_backends$method, "mixomics_block_spls")
    testthat::expect_identical(registration$shared_changes_for_sol$file, "R/integration_contract.R")
  }
})
