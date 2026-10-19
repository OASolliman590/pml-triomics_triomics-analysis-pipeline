c03_error <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

c03_resolved <- function(blocks, outcome = NULL, units = colnames(blocks[[1L]])) {
  if (!is.null(outcome)) names(outcome) <- units
  list(
    study_id = "study-c03-synthetic",
    analysis_id = "analysis-c03-synthetic",
    preparation_context_id = "fold-c03-synthetic",
    representation_id = "fold_prepared",
    unit_ids = unname(units),
    outcome = outcome,
    task_mode = "categorical",
    blocks = stats::setNames(lapply(names(blocks), function(block_id) {
      assay <- blocks[[block_id]]
      list(
        block_id = block_id,
        study_id = "study-c03-synthetic",
        preparation_context_id = "fold-c03-synthetic",
        representation_id = "fold_prepared",
        assay = assay,
        sample_ids = colnames(assay),
        feature_keys = rownames(assay)
      )
    }), names(blocks))
  )
}

c03_blocks <- function() {
  units <- paste0("unit-", seq_len(12L))
  alpha <- rbind(
    alpha_signal = c(rep(-2, 6L), rep(2, 6L)) + c(-.1, .1, -.2, .2, 0, .1, -.1, .2, 0, -.2, .1, 0),
    alpha_trend = seq(-1, 1, length.out = 12L),
    alpha_noise = c(.2, -.4, .3, -.1, .5, -.2, -.3, .1, -.5, .4, -.2, .3)
  )
  beta <- rbind(
    beta_signal = c(rep(-1.5, 6L), rep(1.5, 6L)) + c(.1, -.1, .2, -.2, 0, .1, -.1, 0, .2, -.2, .1, 0),
    beta_trend = cos(seq_len(12L) / 2)
  )
  colnames(alpha) <- colnames(beta) <- units
  list(alpha = alpha, beta = beta)
}

c03_spec <- function(block_ids = c("alpha", "beta")) {
  design <- matrix(
    c(0, .5, .5, 0), nrow = 2L, ncol = 2L,
    dimnames = list(block_ids, block_ids)
  )
  list(
    method = "mixomics_diablo",
    block_ids = block_ids,
    ncomp = 2L,
    scale_features = TRUE,
    score_definition = "consensus_mean",
    design_matrix = design,
    keepX = stats::setNames(list(c(2L, 2L), c(2L, 2L)), block_ids),
    tuning_grid_ref = "c03_test_grid",
    prediction_rule = "centroids.dist",
    na_capability = "complete_predictor_blocks",
    class_levels = c("control", "case")
  )
}

c03_training <- function(block_order = c("alpha", "beta")) {
  blocks <- c03_blocks()[block_order]
  y <- factor(rep(c("control", "case"), each = 6L), levels = c("control", "case"))
  list(
    resolved = c03_resolved(blocks, y),
    params = c03_spec(block_order),
    blocks = blocks,
    outcome = y
  )
}

testthat::test_that("categorical DIABLO fits native named design with sparse support", {
  fixture <- c03_training()
  adapter <- triomics_diablo_backend("mixomics_diablo")
  fit <- adapter$fit(fixture$resolved, fixture$params)
  extracted <- adapter$extract(fit, list(study_id = fixture$resolved$study_id), fixture$params)
  result <- extracted$result

  testthat::expect_s3_class(adapter, "BackendAdapter")
  testthat::expect_identical(adapter$evaluation_modes, "categorical")
  testthat::expect_s3_class(fit, "BackendFit")
  testthat::expect_identical(fit$metadata$native_method, "mixOmics::block.splsda")
  testthat::expect_identical(fit$metadata$native_version, "6.36.0")
  testthat::expect_s3_class(result, "IntegrationResult")
  testthat::expect_identical(result$backend, "mixomics_diablo")
  testthat::expect_identical(names(result$loadings), c("alpha", "beta"))
  testthat::expect_identical(rownames(fit$native_state$model$design), c("alpha", "beta", "Y"))
  testthat::expect_identical(colnames(fit$native_state$model$design), c("alpha", "beta", "Y"))
  testthat::expect_equal(fit$native_state$model$design[c("alpha", "beta"), c("alpha", "beta")],
                         fixture$params$design_matrix, tolerance = 0)
  testthat::expect_identical(unname(fit$native_state$model$ncomp[c("alpha", "beta")]), c(2, 2))
  testthat::expect_identical(fit$native_state$model$keepX, fixture$params$keepX)
  testthat::expect_identical(fit$result_params$tuning, "wrapper_controlled_grid")
  testthat::expect_identical(fit$result_params$na_capability, "complete_predictor_blocks")
  anchor_features <- fit$native_state$consensus_state$anchor_features
  testthat::expect_true(all(vapply(seq_along(anchor_features), function(j) {
    result$loadings$alpha[anchor_features[[j]], names(anchor_features)[[j]]] >= 0
  }, logical(1))))
  oriented_expected <- lapply(names(fit$native_state$native_loadings), function(block_id) {
    value <- fit$native_state$native_loadings[[block_id]]
    sweep(value, 2L, fit$native_state$consensus_state$signs[block_id, colnames(value)], "*")
  })
  names(oriented_expected) <- names(result$loadings)
  testthat::expect_equal(
    lapply(result$loadings, function(value) value),
    stats::setNames(lapply(names(result$loadings), function(block_id) {
      oriented_expected[[block_id]][result$selected[[block_id]], , drop = FALSE]
    }), names(result$loadings)),
    tolerance = 1e-12
  )
  testthat::expect_true(all(vapply(result$selected, length, integer(1)) > 0L))
  testthat::expect_true(all(vapply(seq_along(result$selected), function(i) {
    all(result$selected[[i]] %in% rownames(result$loadings[[i]]))
  }, logical(1))))
  testthat::expect_true(all(vapply(result$loadings, function(x) any(abs(x) > 0), logical(1))))
})

testthat::test_that("DIABLO rejects non-categorical or unpinned inputs at the adapter boundary", {
  fixture <- c03_training()
  adapter <- triomics_diablo_backend("mixomics_diablo")

  bad_design <- fixture$params
  bad_design$design_matrix <- matrix(c(0, .5, .2, 0), 2L, dimnames = list(c("alpha", "beta"), c("alpha", "beta")))
  c03_error(adapter$validate(fixture$resolved, bad_design), "C_SCORE_DESIGN_INVALID")

  missing_pin <- fixture$params
  missing_pin$na_capability <- NULL
  c03_error(adapter$validate(fixture$resolved, missing_pin), "C_NA_CAPABILITY")

  missing_values <- fixture$resolved
  missing_values$blocks$alpha$assay[1L, 1L] <- NA_real_
  c03_error(adapter$validate(missing_values, fixture$params), "C_NA_CAPABILITY")

  numeric_outcome <- fixture$resolved
  numeric_outcome$outcome <- seq_along(numeric_outcome$unit_ids)
  c03_error(adapter$validate(numeric_outcome, fixture$params), "C_EVAL_RESPONSE_INVALID")

  low_rank <- fixture$resolved
  low_rank$blocks$beta$assay[2L, ] <- low_rank$blocks$beta$assay[1L, ]
  c03_error(adapter$validate(low_rank, fixture$params), "C_SCORE_COMPONENT_RANK")
})

testthat::test_that("consensus uses the frozen anchor/sign/standardization convention", {
  fixture <- c03_training()
  fit <- triomics_diablo_backend("mixomics_diablo")$fit(fixture$resolved, fixture$params)
  native_scores <- fit$native_state$native_scores
  native_loadings <- fit$native_state$native_loadings
  training_units <- fixture$resolved$unit_ids
  state <- make_consensus_state(native_scores, native_loadings, training_units,
                                list(required_views = c("alpha", "beta")))
  expected <- apply_consensus_state(native_scores, state)

  reordered <- make_consensus_state(
    native_scores[c("beta", "alpha")], native_loadings[c("beta", "alpha")],
    training_units, list(required_views = c("beta", "alpha"))
  )
  testthat::expect_identical(reordered$anchor_view, "alpha")
  testthat::expect_equal(apply_consensus_state(native_scores[c("beta", "alpha")], reordered),
                         expected, tolerance = 1e-10)

  signs <- c(alpha = -1, beta = 1)
  perturbed_scores <- Map(function(x, sign) x * sign, native_scores, signs[names(native_scores)])
  perturbed_loadings <- Map(function(x, sign) x * sign, native_loadings, signs[names(native_loadings)])
  perturbed <- make_consensus_state(perturbed_scores, perturbed_loadings, training_units,
                                    list(required_views = c("alpha", "beta")))
  testthat::expect_equal(apply_consensus_state(perturbed_scores, perturbed),
                         expected, tolerance = 1e-10)
  orient <- function(loadings, state) {
    stats::setNames(lapply(names(loadings), function(block_id) {
      sweep(loadings[[block_id]], 2L, state$signs[block_id, colnames(loadings[[block_id]])], "*")
    }), names(loadings))
  }
  testthat::expect_equal(orient(native_loadings, state), orient(perturbed_loadings, perturbed),
                         tolerance = 1e-12)
  testthat::expect_identical(state$weights, c(alpha = 0.5, beta = 0.5))

  degenerate <- native_scores
  degenerate$beta[, 1L] <- 1
  c03_error(make_consensus_state(degenerate, native_loadings, training_units,
                                 list(required_views = c("alpha", "beta"))),
            "C_SCORE_RANK_ZERO")
})

testthat::test_that("held-out prediction and scores use native APIs and stored transforms", {
  fixture <- c03_training()
  adapter <- triomics_diablo_backend("mixomics_diablo")
  fit <- adapter$fit(fixture$resolved, fixture$params)
  new_blocks <- lapply(fixture$blocks, function(x) x[, c(1L, 7L), drop = FALSE])
  colnames(new_blocks$alpha) <- colnames(new_blocks$beta) <- c("held-control", "held-case")
  new_resolved <- c03_resolved(new_blocks, outcome = NULL)
  prediction <- adapter$predict(fit, new_resolved, fixture$params)
  native <- stats::predict(
    fit$native_state$model,
    newdata = lapply(new_blocks, function(x) t(x)),
    dist = fixture$params$prediction_rule
  )
  expected_native <- as.character(native$MajorityVote[[fixture$params$prediction_rule]][, fixture$params$ncomp])
  expected_scores <- lapply(native$variates, function(value) {
    value <- value[, seq_len(fixture$params$ncomp), drop = FALSE]
    colnames(value) <- fit$native_state$components
    value
  })
  names(expected_scores) <- names(fit$native_state$native_scores)
  expected_scores <- apply_consensus_state(expected_scores, fit$native_state$consensus_state)

  testthat::expect_identical(prediction$prediction_kind, "categorical_label")
  testthat::expect_identical(unname(prediction$predictions), expected_native)
  testthat::expect_equal(prediction$scores, expected_scores, tolerance = 1e-10)
  testthat::expect_equal(prediction$native_scores, lapply(native$variates, function(value) {
    value <- value[, seq_len(fixture$params$ncomp), drop = FALSE]
    colnames(value) <- fit$native_state$components
    value
  }), tolerance = 1e-10)
  testthat::expect_identical(prediction$unit_ids, c("held-control", "held-case"))
  testthat::expect_identical(prediction$training_state_hash, fit$metadata$state_hash)
  testthat::expect_true(all(vapply(fit$native_state$training_transform,
                                   function(x) identical(x$scale_features, TRUE), logical(1))))
  testthat::expect_identical(fit$native_state$prediction_state$uses_outcome, FALSE)

  changed_outcome <- c03_resolved(new_blocks, factor(c("case", "control"), levels = c("control", "case")))
  changed_prediction <- adapter$predict(fit, changed_outcome, fixture$params)
  testthat::expect_identical(changed_prediction$predictions, prediction$predictions)
  testthat::expect_equal(changed_prediction$scores, prediction$scores, tolerance = 0)

  common <- adapter$extract(fit, list(), fixture$params)$result
  expected_common <- apply_consensus_state(fit$native_state$native_scores,
                                           fit$native_state$consensus_state)
  testthat::expect_equal(common$scores, expected_common, tolerance = 1e-10)

  missing_block <- new_resolved
  missing_block$blocks$beta <- NULL
  c03_error(adapter$predict(fit, missing_block, fixture$params), "C_EVAL_REQUIRED_VIEW_MISSING")
})

testthat::test_that("native predictor variance is verified and common degeneracy fails closed", {
  fixture <- c03_training()
  adapter <- triomics_diablo_backend("mixomics_diablo")
  fit <- adapter$fit(fixture$resolved, fixture$params)
  result <- adapter$extract(fit, list(), fixture$params)$result

  expected <- vapply(c("alpha", "beta"), function(block_id) {
    x <- fit$native_state$model$X[[block_id]]
    t <- fit$native_state$model$variates[[block_id]][, "comp1"]
    sum(crossprod(x, t)^2) / (sum(t^2) * sum(x^2))
  }, numeric(1))
  testthat::expect_equal(unname(result$variance[, "comp1"]), unname(expected), tolerance = 1e-12)
  testthat::expect_identical(attr(result$variance, "metric"), "native_block_splsda_proportion")
  testthat::expect_identical(attr(result$variance, "semantics"), "predictor_block_native")
  testthat::expect_identical(fit$result_params$variance$assay_space,
                             "mixomics_fitted_block_after_declared_scaling")
  testthat::expect_identical(fit$result_params$variance$denominator,
                             "squared_frobenius_norm_of_fitted_block")
  testthat::expect_identical(fit$result_params$variance$accumulation,
                             "per_component_against_original_block_not_cumulative")
  testthat::expect_false("Y" %in% rownames(result$variance))
  testthat::expect_false("Y" %in% names(result$loadings))
  testthat::expect_true(all(vapply(result$selected, function(keys) length(keys) > 0L, logical(1))))

  no_variance <- fit
  no_variance$native_state$model$prop_expl_var <- NULL
  c03_error(adapter$extract(no_variance, list(), fixture$params), "C_SCORE_VARIANCE_UNAVAILABLE")

  mismatched_variance <- fit
  mismatched_variance$native_state$model$prop_expl_var$alpha[["comp1"]] <-
    mismatched_variance$native_state$model$prop_expl_var$alpha[["comp1"]] + 0.01
  c03_error(adapter$extract(mismatched_variance, list(), fixture$params), "C_SCORE_VARIANCE_UNAVAILABLE")

  no_common <- fit
  no_common$native_state$native_scores$alpha[, 1L] <- 0
  c03_error(adapter$extract(no_common, list(), fixture$params), "C_SCORE_RANK_ZERO")
})

testthat::test_that("DIABLO fits supplied candidates once without native tuning", {
  fixture <- c03_training()
  adapter <- triomics_diablo_backend("mixomics_diablo")
  trace("tune.block.splsda", tracer = quote(stop("native tuner must not be called")),
        where = asNamespace("mixOmics"))
  on.exit(untrace("tune.block.splsda", where = asNamespace("mixOmics")), add = TRUE)

  candidates <- list(
    utils::modifyList(c03_spec(), list(keepX = list(alpha = c(1L, 1L), beta = c(1L, 1L)))),
    utils::modifyList(c03_spec(), list(keepX = list(alpha = c(3L, 2L), beta = c(2L, 1L))))
  )
  fits <- lapply(candidates, function(params) adapter$fit(fixture$resolved, params))
  for (i in seq_along(fits)) {
    testthat::expect_identical(fits[[i]]$native_state$model$keepX, candidates[[i]]$keepX)
    testthat::expect_identical(unname(fits[[i]]$native_state$model$ncomp[c("alpha", "beta")]), c(2, 2))
    testthat::expect_identical(fits[[i]]$result_params$requested_n_components, 2L)
  }
})

testthat::test_that("C-03 fixture is schema-valid and shared registration remains coordinator-owned", {
  fixture_path <- triomics_test_resource("examples", "tickets", "C-03.yaml")
  config <- yaml::read_yaml(fixture_path)
  schema_path <- normalizePath(
    testthat::test_path("..", "..", "discovery", "spec-v2", "schemas", "config.schema.json"),
    winslash = "/", mustWork = TRUE
  )
  testthat::expect_true(jsonvalidate::json_validate(
    jsonlite::toJSON(config, auto_unbox = TRUE, null = "null"),
    schema_path, engine = "ajv", verbose = TRUE
  ))
  registration <- jsonlite::read_json(
    testthat::test_path("..", "..", "docs", "implementation", "registrations", "C-03.json"),
    simplifyVector = FALSE
  )
  testthat::expect_identical(registration$applied_to_shared_files, TRUE)
  testthat::expect_true(length(registration$coordinator_owned_files_to_update) > 0L)
  testthat::expect_true("R/cli.R" %in% unlist(registration$coordinator_owned_files_to_update))
  adapter <- triomics_integration_backend("mixomics_diablo")
  testthat::expect_s3_class(adapter, "BackendAdapter")
  testthat::expect_identical(adapter$evaluation_modes, "categorical")
  c03_error(
    evaluate_pipeline(NULL, NULL, list(method = "mixomics_diablo_unregistered"), NULL),
    "C_SCORE_BACKEND_UNAVAILABLE"
  )
})
