triomics_evaluation_abort <- function(code, message, key = NULL,
                                      remediation = "Review the split, response, or integration specification.") {
  triomics_abort(code, message, key = key, module = "evaluation", stage = "C", remediation = remediation)
}

triomics_evaluation_metrics <- function(mode, truth, prediction, status,
                                       declared_levels = NULL, training_intercept = NULL) {
  if (length(truth) != length(prediction) || length(truth) != length(status)) {
    triomics_evaluation_abort("C_EVAL_METRIC_INPUT", "Truth, prediction, and status lengths must match.", key = "metrics")
  }
  eligible <- status %in% c("predicted", "failed")
  predicted <- status == "predicted" & eligible
  failed <- status == "failed" & eligible
  n_eligible <- sum(eligible)
  n_predicted <- sum(predicted)
  failed_count <- sum(failed)
  if (identical(mode, "categorical")) {
    if (!is.character(declared_levels) || length(declared_levels) < 2L ||
        anyNA(declared_levels) || anyDuplicated(declared_levels)) {
      triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Categorical metrics require declared class levels.", key = "class_levels")
    }
    truth <- as.character(truth)
    if (anyNA(truth[eligible]) || any(!truth[eligible] %in% declared_levels)) {
      triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Eligible truth values must use declared classes.", key = "outcome")
    }
    predicted_values <- rep("<failed>", length(status))
    predicted_values[predicted] <- as.character(prediction[predicted])
    predicted_levels <- c(declared_levels, "<failed>")
    confusion <- table(
      factor(truth[eligible], levels = declared_levels),
      factor(predicted_values[eligible], levels = predicted_levels),
      dnn = c("truth", "prediction")
    )
    class_counts <- rowSums(confusion)
    class_errors <- vapply(seq_along(declared_levels), function(i) {
      if (class_counts[[i]] == 0L) return(NA_real_)
      1 - confusion[i, declared_levels[[i]]] / class_counts[[i]]
    }, numeric(1))
    names(class_errors) <- declared_levels
    balanced_error <- if (all(class_counts > 0L)) mean(class_errors) else NA_real_
    accuracy <- if (n_eligible) sum(diag(confusion[, declared_levels, drop = FALSE])) / n_eligible else NA_real_
    return(list(
      balanced_error = unname(balanced_error), accuracy = unname(accuracy),
      confusion = confusion,
      per_class = data.frame(
        class = declared_levels, n_eligible = as.integer(class_counts),
        error_rate = unname(class_errors), stringsAsFactors = FALSE
      ),
      n_eligible = as.integer(n_eligible), n_predicted = as.integer(n_predicted),
      failed_prediction_count = as.integer(failed_count),
      total_denominator = as.integer(n_eligible),
      auroc = NULL
    ))
  }
  if (!identical(mode, "continuous")) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Metric mode must be categorical or continuous.", key = "mode")
  }
  truth <- as.numeric(truth)
  prediction <- as.numeric(prediction)
  if (anyNA(truth[eligible]) || any(!is.finite(truth[eligible]))) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Eligible continuous truth values must be finite.", key = "outcome")
  }
  if (any(!is.finite(prediction[predicted]))) {
    triomics_evaluation_abort("C_EVAL_METRIC_INPUT", "Predicted continuous values must be finite.", key = "prediction")
  }
  if (length(training_intercept) == 1L) training_intercept <- rep(training_intercept, length(truth))
  if (length(training_intercept) != length(truth) ||
      any(!is.finite(training_intercept[predicted]))) {
    triomics_evaluation_abort(
      "C_EVAL_METRIC_INPUT", "Continuous R-squared requires the training-only intercept for every prediction.",
      key = "training_intercept"
    )
  }
  if (!n_predicted) {
    rmse <- mae <- r_squared <- denominator <- NA_real_
  } else {
    residual <- truth[predicted] - prediction[predicted]
    rmse <- sqrt(mean(residual^2))
    mae <- mean(abs(residual))
    denominator <- sum((truth[predicted] - training_intercept[predicted])^2)
    r_squared <- if (denominator > 0) 1 - sum(residual^2) / denominator else NA_real_
  }
  list(
    rmse = unname(rmse), mae = unname(mae), r_squared = unname(r_squared),
    r_squared_baseline = "training_intercept",
    r_squared_denominator = unname(denominator),
    n_eligible = as.integer(n_eligible), n_predicted = as.integer(n_predicted),
    failed_prediction_count = as.integer(failed_count),
    total_denominator = as.integer(n_eligible)
  )
}

triomics_evaluation_response <- function(study, outcome_spec, unit_ids) {
  if (!is.list(outcome_spec) || !is.character(outcome_spec$column) ||
      length(outcome_spec$column) != 1L || !nzchar(outcome_spec$column) ||
      !outcome_spec$type %in% c("categorical", "continuous")) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Outcome column and type must be explicit.", key = "outcome")
  }
  registry <- study$sample_registry
  if (!is.data.frame(registry) || !"sample_id" %in% names(registry) ||
      !outcome_spec$column %in% names(registry)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "The declared outcome column is absent from the sample registry.", key = outcome_spec$column)
  }
  matched <- match(unit_ids, as.character(registry$sample_id))
  if (anyNA(matched)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "The response map is missing a SplitPlan unit.", key = "sample_id")
  }
  raw <- registry[[outcome_spec$column]][matched]
  names(raw) <- unit_ids
  if (identical(outcome_spec$type, "categorical")) {
    if (!(is.factor(raw) || is.character(raw))) {
      triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Categorical outcome values must be factor or character.", key = outcome_spec$column)
    }
    levels <- outcome_spec$levels
    if (is.null(levels) && is.factor(raw)) levels <- levels(raw)
    if (is.null(levels)) levels <- sort(unique(as.character(raw[!is.na(raw)])))
    if (!is.character(levels) || length(levels) < 2L || anyNA(levels) ||
        any(!nzchar(levels)) || anyDuplicated(levels)) {
      triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Categorical outcomes need two or more declared levels.", key = "levels")
    }
    values <- as.character(raw)
    invalid <- !is.na(values) & !values %in% levels
    if (any(invalid)) {
      values[invalid] <- NA_character_
    }
    response <- factor(values, levels = levels)
    names(response) <- unit_ids
    return(list(mode = "categorical", values = response, levels = levels))
  }
  if (!is.numeric(raw) || is.matrix(raw)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Continuous outcome must be a scalar numeric registry column.", key = outcome_spec$column)
  }
  values <- as.numeric(raw)
  names(values) <- unit_ids
  list(mode = "continuous", values = values, levels = NULL)
}

triomics_evaluation_prepared_input <- function(prepared_fold, requested_units, study,
                                              analysis_spec, block_ids, response = NULL) {
  requested_units <- unname(as.character(requested_units))
  reasons <- stats::setNames(rep(NA_character_, length(requested_units)), requested_units)
  contexts <- prepared_fold$contexts$B$blocks
  if (!is.list(contexts) || !identical(names(contexts), block_ids)) {
    triomics_evaluation_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING", "Fold preparation omitted a required predictor block.",
      key = "contexts.B.blocks"
    )
  }
  valid <- stats::setNames(rep(TRUE, length(requested_units)), requested_units)
  for (block_id in block_ids) {
    context <- contexts[[block_id]]
    values <- context$values
    if (!is.matrix(values) || !is.numeric(values) || is.complex(values) ||
        is.null(rownames(values)) || is.null(colnames(values))) {
      triomics_evaluation_abort(
        "C_EVAL_REQUIRED_VIEW_MISSING", paste0("Prepared block '", block_id, "' has no keyed numeric values."),
        key = block_id
      )
    }
    for (unit_id in requested_units) {
      if (!unit_id %in% colnames(values)) {
        valid[[unit_id]] <- FALSE
        if (is.na(reasons[[unit_id]])) reasons[[unit_id]] <- paste0("required_view_missing:", block_id)
      } else if (any(!is.finite(values[, unit_id]))) {
        valid[[unit_id]] <- FALSE
        if (is.na(reasons[[unit_id]])) reasons[[unit_id]] <- paste0("nonfinite_predictor:", block_id)
      }
    }
  }
  complete_units <- requested_units[valid]
  output <- list(
    complete_units = complete_units, reasons = reasons,
    resolved = NULL
  )
  if (!length(complete_units)) return(output)
  blocks <- stats::setNames(lapply(block_ids, function(block_id) {
    values <- contexts[[block_id]]$values[, complete_units, drop = FALSE]
    list(
      block_id = block_id,
      study_id = study$study_id,
      preparation_context_id = prepared_fold$contexts$B$preparation_context_id,
      representation_id = "fold_prepared",
      assay = values,
      sample_ids = colnames(values),
      feature_keys = rownames(values)
    )
  }), block_ids)
  state_refs <- stats::setNames(lapply(block_ids, function(block_id) {
    prepared_fold$blocks[[block_id]]$state_ref
  }), block_ids)
  state_hashes <- stats::setNames(vapply(block_ids, function(block_id) {
    as.character(prepared_fold$blocks[[block_id]]$state_hash)
  }, character(1)), block_ids)
  response_values <- NULL
  if (!is.null(response)) response_values <- response[complete_units]
  resolved <- list(
    study_id = study$study_id,
    analysis_id = if (is.character(analysis_spec$analysis_id) && length(analysis_spec$analysis_id)) {
      analysis_spec$analysis_id[[1L]]
    } else {
      "supervised-evaluation"
    },
    preparation_context_id = prepared_fold$contexts$B$preparation_context_id,
    representation_id = "fold_prepared",
    unit_ids = complete_units,
    blocks = blocks,
    outcome = response_values,
    preparation_state_refs = state_refs,
    preparation_state_hashes = state_hashes
  )
  output$resolved <- resolved
  output
}

triomics_evaluation_candidate_params <- function(integration_spec, candidate, levels) {
  params <- integration_spec
  params$candidates <- NULL
  params$max_fits <- NULL
  params$ncomp <- candidate$ncomp
  params$candidate_id <- candidate$id
  if (!is.null(candidate$scale_features)) params$scale_features <- candidate$scale_features
  if (!is.null(candidate$prediction_rule)) params$prediction_rule <- candidate$prediction_rule
  if (!is.null(levels)) params$class_levels <- levels
  params
}

triomics_evaluation_budget_fit <- function(adapter, resolved, params, budget) {
  adapter$validate(resolved, params)
  if (budget$count >= budget$maximum) {
    triomics_evaluation_abort(
      "C_EVAL_BUDGET",
      paste0("max_fits=", budget$maximum, " reached after ", budget$count,
             " attempted native fit", if (budget$count == 1L) "" else "s",
             "; the next fit was not started."),
      key = "max_fits"
    )
  }
  budget$count <- budget$count + 1L
  adapter$fit(resolved, params)
}

triomics_evaluation_native_error <- function(error) {
  if (inherits(error, "triomics_error") && identical(error$code, "C_EVAL_BUDGET")) stop(error)
  list(
    code = if (inherits(error, "triomics_error")) error$code else "C_EVAL_NATIVE_FAILURE",
    message = conditionMessage(error)
  )
}

triomics_evaluation_baseline <- function(mode, training_response, levels, n) {
  if (identical(mode, "categorical")) {
    counts <- table(factor(as.character(training_response), levels = levels))
    value <- levels[[which.max(counts)]]
    return(rep(value, n))
  }
  rep(mean(training_response), n)
}

triomics_evaluation_outer_rows <- function(fold_id, unit_ids, truth, prediction,
                                           prediction_error, predictor_reasons,
                                           candidate_id, baseline, training_intercept) {
  n <- length(unit_ids)
  status <- rep("predicted", n)
  reason <- rep(NA_character_, n)
  pred <- prediction
  for (i in seq_len(n)) {
    if (is.na(truth[[i]])) {
      status[[i]] <- "ineligible"
      reason[[i]] <- "missing_or_undeclared_outcome"
      pred[[i]] <- if (is.numeric(pred)) NA_real_ else NA_character_
    } else if (!is.na(predictor_reasons[[unit_ids[[i]]]])) {
      status[[i]] <- "ineligible"
      reason[[i]] <- predictor_reasons[[unit_ids[[i]]]]
      pred[[i]] <- if (is.numeric(pred)) NA_real_ else NA_character_
    } else if (!is.null(prediction_error)) {
      status[[i]] <- "failed"
      reason[[i]] <- prediction_error$code
      pred[[i]] <- if (is.numeric(pred)) NA_real_ else NA_character_
    }
  }
  data.frame(
    fold_id = rep(fold_id, n), unit_id = unit_ids,
    truth = if (is.factor(truth)) as.character(truth) else as.numeric(truth),
    prediction = if (is.factor(truth) || is.character(truth)) as.character(pred) else as.numeric(pred),
    status = status, reason_code = reason, candidate_id = rep(candidate_id, n),
    baseline_prediction = baseline,
    training_intercept = rep(training_intercept, n),
    stringsAsFactors = FALSE
  )
}

triomics_evaluation_candidate <- function(outer_fold, candidate, candidate_params,
                                          prepared, study, analysis_spec, integration_spec,
                                          response, mode, levels, adapter, budget) {
  inner_results <- list()
  all_truth <- list()
  all_prediction <- list()
  all_status <- list()
  all_intercepts <- list()
  fit_failed <- FALSE
  for (inner_fold in outer_fold$inner) {
    train_input <- triomics_evaluation_prepared_input(
      inner_fold, inner_fold$training_unit_ids, study, analysis_spec,
      integration_spec$block_ids, response
    )
    test_input <- triomics_evaluation_prepared_input(
      inner_fold, inner_fold$test_unit_ids, study, analysis_spec,
      integration_spec$block_ids
    )
    fit_error <- NULL
    backend_fit <- tryCatch({
      if (length(train_input$complete_units) != length(inner_fold$training_unit_ids)) {
        triomics_evaluation_abort(
          "C_EVAL_REQUIRED_VIEW_MISSING",
          "An inner training partition lacks a required predictor view; the fold was not silently reduced.",
          key = inner_fold$fold_id
        )
      }
      triomics_evaluation_budget_fit(adapter, train_input$resolved, candidate_params, budget)
    }, error = function(error) {
      fit_error <<- triomics_evaluation_native_error(error)
      NULL
    })
    if (!is.null(fit_error) && identical(fit_error$code, "C_EVAL_BUDGET")) {
      triomics_evaluation_abort("C_EVAL_BUDGET", fit_error$message, key = "max_fits")
    }
    test_prediction <- NULL
    prediction_error <- NULL
    if (is.null(fit_error) && length(test_input$complete_units)) {
      test_prediction <- tryCatch(
        adapter$predict(backend_fit, test_input$resolved, candidate_params),
        error = function(error) {
          prediction_error <<- triomics_evaluation_native_error(error)
          NULL
        }
      )
    }
    test_ids <- inner_fold$test_unit_ids
    test_truth <- response[test_ids]
    pred_values <- if (identical(mode, "categorical")) {
      stats::setNames(rep(NA_character_, length(test_ids)), test_ids)
    } else {
      stats::setNames(rep(NA_real_, length(test_ids)), test_ids)
    }
    test_status <- stats::setNames(rep("ineligible", length(test_ids)), test_ids)
    reason_codes <- test_input$reasons
    for (unit_id in test_ids) {
      if (is.na(test_truth[[unit_id]])) {
        reason_codes[[unit_id]] <- "missing_or_undeclared_outcome"
      } else if (!is.na(test_input$reasons[[unit_id]])) {
        reason_codes[[unit_id]] <- test_input$reasons[[unit_id]]
      } else if (!is.null(fit_error)) {
        test_status[[unit_id]] <- "failed"
        reason_codes[[unit_id]] <- fit_error$code
      } else if (!is.null(prediction_error)) {
        test_status[[unit_id]] <- "failed"
        reason_codes[[unit_id]] <- prediction_error$code
      } else if (!is.null(test_prediction) && unit_id %in% test_prediction$unit_ids) {
        test_status[[unit_id]] <- "predicted"
        pred_values[[unit_id]] <- test_prediction$predictions[[unit_id]]
        reason_codes[[unit_id]] <- NA_character_
      }
    }
    eligible_status <- test_status %in% c("predicted", "failed")
    inner_metric <- if (any(eligible_status)) {
      if (identical(mode, "categorical")) {
        triomics_evaluation_metrics(
          mode, as.character(test_truth), as.character(pred_values), test_status,
          declared_levels = levels
        )
      } else {
        triomics_evaluation_metrics(
          mode, as.numeric(test_truth), as.numeric(pred_values), test_status,
          training_intercept = mean(train_input$resolved$outcome)
        )
      }
    } else {
      NULL
    }
    inner_status <- if (!is.null(fit_error)) "fit_failed" else if (!is.null(prediction_error)) {
      "prediction_failed"
    } else if (is.null(inner_metric)) {
      "no_eligible_units"
    } else {
      "complete"
    }
    if (inner_status == "fit_failed") fit_failed <- TRUE
    record <- list(
      candidate_id = candidate$id, fold_id = inner_fold$fold_id,
      status = inner_status,
      failure = if (!is.null(fit_error)) fit_error else prediction_error,
      training_unit_ids = inner_fold$training_unit_ids,
      test_unit_ids = inner_fold$test_unit_ids,
      training_subject_ids = inner_fold$training_subject_ids,
      test_subject_ids = inner_fold$test_subject_ids,
      preparation_state_refs = train_input$resolved$preparation_state_refs,
      preparation_state_hashes = train_input$resolved$preparation_state_hashes,
      fitted_state_hash = if (is.null(backend_fit)) NULL else backend_fit$metadata$state_hash,
      tuning_metric = inner_metric,
      ineligible_reasons = reason_codes
    )
    inner_results[[length(inner_results) + 1L]] <- record
    all_truth[[length(all_truth) + 1L]] <- as.character(test_truth)
    all_prediction[[length(all_prediction) + 1L]] <- as.character(pred_values)
    all_status[[length(all_status) + 1L]] <- unname(test_status)
    all_intercepts[[length(all_intercepts) + 1L]] <- rep(
      if (identical(mode, "continuous") && !is.null(train_input$resolved)) mean(train_input$resolved$outcome) else NA_real_,
      length(test_ids)
    )
  }
  scores <- vapply(inner_results, function(record) {
    if (is.null(record$tuning_metric)) return(NA_real_)
    if (identical(mode, "categorical")) record$tuning_metric$balanced_error else record$tuning_metric$rmse
  }, numeric(1))
  eligible_candidate <- !fit_failed && length(scores) > 0L && all(is.finite(scores))
  score <- if (eligible_candidate) mean(scores) else NA_real_
  list(
    id = candidate$id, candidate = candidate,
    status = if (eligible_candidate) "eligible" else "failed",
    score = score,
    score_definition = if (identical(mode, "categorical")) "mean_inner_balanced_error" else "mean_inner_rmse",
    inner_fold_ids = vapply(inner_results, `[[`, character(1), "fold_id"),
    inner_scores = scores,
    inner_results = inner_results
  )
}

evaluate_pipeline <- function(study, analysis_spec, integration_spec, split_plan) {
  triomics_validate_split_plan(split_plan, TRUE)
  if (!is.list(study) || !is.data.frame(study$sample_registry) ||
      !is.character(study$study_id) || length(study$study_id) != 1L ||
      !nzchar(study$study_id)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Evaluation needs an assembled study and sample registry.", key = "study")
  }
  if (!is.list(analysis_spec) || !is.list(analysis_spec$predictive_spec) ||
      !is.list(analysis_spec$descriptive_spec) || !is.list(integration_spec)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Predictive, descriptive, and integration specifications are required.", key = "analysis_spec")
  }
  unit_ids <- as.character(split_plan$unit_subject$unit_id)
  registry_ids <- as.character(study$sample_registry$sample_id)
  if (anyDuplicated(registry_ids) || !setequal(unit_ids, registry_ids)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "SplitPlan units must match the assembled study registry.", key = "unit_subject")
  }
  response <- triomics_evaluation_response(study, analysis_spec$outcome, unit_ids)
  mode <- response$mode
  expected_method <- if (identical(mode, "categorical")) "concat_plsda" else "concat_pls"
  if (!identical(integration_spec$method, expected_method)) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "Integration method does not match the declared response type.", key = "method")
  }
  block_ids <- integration_spec$block_ids
  if (is.list(block_ids)) block_ids <- unlist(block_ids, use.names = FALSE)
  prep_ids <- vapply(analysis_spec$predictive_spec$blocks, function(block) {
    as.character(block$block_id[[1L]])
  }, character(1))
  if (!is.character(block_ids) || !identical(block_ids, prep_ids)) {
    triomics_evaluation_abort(
      "C_EVAL_REQUIRED_VIEW_MISSING", "Integration block order must match the fold-preparation block order.",
      key = "block_ids"
    )
  }
  candidates <- integration_spec$candidates
  if (!is.list(candidates) || !length(candidates)) {
    if (is.null(integration_spec$ncomp)) {
      triomics_evaluation_abort("C_EVAL_COMPONENT_RANK", "At least one component candidate is required.", key = "candidates")
    }
    candidates <- list(list(id = "default", ncomp = integration_spec$ncomp))
  }
  candidate_ids <- vapply(seq_along(candidates), function(i) {
    candidate <- candidates[[i]]
    if (is.null(candidate$id)) paste0("candidate-", i) else as.character(candidate$id[[1L]])
  }, character(1))
  if (anyNA(candidate_ids) || any(!nzchar(candidate_ids)) || anyDuplicated(candidate_ids)) {
    triomics_evaluation_abort("C_EVAL_COMPONENT_RANK", "Candidate ids must be unique non-blank strings.", key = "candidates")
  }
  for (i in seq_along(candidates)) candidates[[i]]$id <- candidate_ids[[i]]
  maximum <- integration_spec$max_fits
  if (!is.numeric(maximum) || length(maximum) != 1L || is.na(maximum) ||
      !is.finite(maximum) || maximum < 1 || maximum != trunc(maximum)) {
    triomics_evaluation_abort("C_EVAL_BUDGET", "A positive integer max_fits budget is required.", key = "max_fits")
  }
  if (split_plan$target != analysis_spec$outcome$column) {
    triomics_evaluation_abort("C_EVAL_RESPONSE_INVALID", "SplitPlan target must match the declared response column.", key = "target")
  }
  prepared <- triomics_fold_prepare(
    study, analysis_spec$predictive_spec, split_plan, analysis_spec$descriptive_spec
  )
  adapter <- triomics_supervised_backend(integration_spec$method)
  budget <- new.env(parent = emptyenv())
  budget$count <- 0L
  budget$maximum <- as.integer(maximum)
  outer_records <- list()
  prediction_rows <- list()
  for (i in seq_along(prepared$folds)) {
    outer_fold <- prepared$folds[[i]]
    candidate_records <- list()
    flat_inner <- list()
    for (candidate in candidates) {
      candidate_params <- triomics_evaluation_candidate_params(
        integration_spec, candidate, response$levels
      )
      candidate_result <- triomics_evaluation_candidate(
        outer_fold, candidate, candidate_params, prepared, study, analysis_spec,
        integration_spec, response$values, mode, response$levels, adapter, budget
      )
      candidate_records[[candidate$id]] <- candidate_result[setdiff(names(candidate_result), c("inner_results", "candidate"))]
      flat_inner <- c(flat_inner, candidate_result$inner_results)
    }
    score_vector <- vapply(candidate_records, function(record) record$score, numeric(1))
    if (!any(is.finite(score_vector))) {
      candidate_id <- NA_character_
      chosen <- NULL
      outer_fit <- NULL
      outer_prediction <- NULL
      outer_fit_error <- list(code = "C_EVAL_NO_CANDIDATE", message = "No candidate completed every required inner split.")
      outer_prediction_error <- NULL
      train_input <- triomics_evaluation_prepared_input(
        outer_fold, outer_fold$training_unit_ids, study, analysis_spec,
        integration_spec$block_ids, response$values
      )
    } else {
      chosen_index <- which(is.finite(score_vector))[which.min(score_vector[is.finite(score_vector)])]
      chosen <- candidates[[match(names(score_vector)[chosen_index], candidate_ids)]]
      candidate_id <- chosen$id
      chosen_params <- triomics_evaluation_candidate_params(integration_spec, chosen, response$levels)
      train_input <- triomics_evaluation_prepared_input(
        outer_fold, outer_fold$training_unit_ids, study, analysis_spec,
        integration_spec$block_ids, response$values
      )
      test_input <- triomics_evaluation_prepared_input(
        outer_fold, outer_fold$test_unit_ids, study, analysis_spec,
        integration_spec$block_ids
      )
      outer_fit_error <- NULL
      outer_fit <- tryCatch({
        if (length(train_input$complete_units) != length(outer_fold$training_unit_ids)) {
          triomics_evaluation_abort(
            "C_EVAL_REQUIRED_VIEW_MISSING",
            "An outer training partition lacks a required predictor view; the partition was not silently reduced.",
            key = outer_fold$fold_id
          )
        }
        triomics_evaluation_budget_fit(adapter, train_input$resolved, chosen_params, budget)
      }, error = function(error) {
        outer_fit_error <<- triomics_evaluation_native_error(error)
        NULL
      })
      if (!is.null(outer_fit_error) && identical(outer_fit_error$code, "C_EVAL_BUDGET")) {
        triomics_evaluation_abort("C_EVAL_BUDGET", outer_fit_error$message, key = "max_fits")
      }
      outer_prediction_error <- NULL
      outer_prediction <- NULL
      if (is.null(outer_fit_error) && length(test_input$complete_units)) {
        outer_prediction <- tryCatch(
          adapter$predict(outer_fit, test_input$resolved, chosen_params),
          error = function(error) {
            outer_prediction_error <<- triomics_evaluation_native_error(error)
            NULL
          }
        )
      }
    }
    test_ids <- outer_fold$test_unit_ids
    test_input <- triomics_evaluation_prepared_input(
      outer_fold, test_ids, study, analysis_spec, integration_spec$block_ids
    )
    test_truth <- response$values[test_ids]
    prediction_values <- if (identical(mode, "categorical")) {
      stats::setNames(rep(NA_character_, length(test_ids)), test_ids)
    } else {
      stats::setNames(rep(NA_real_, length(test_ids)), test_ids)
    }
    if (!is.null(outer_prediction)) {
      prediction_values[outer_prediction$unit_ids] <- outer_prediction$predictions[outer_prediction$unit_ids]
    }
    training_response <- train_input$resolved$outcome
    baseline <- triomics_evaluation_baseline(
      mode, training_response, response$levels, length(test_ids)
    )
    names(baseline) <- test_ids
    intercept <- if (identical(mode, "continuous")) mean(training_response) else NA_real_
    failure <- outer_prediction_error
    if (is.null(failure) && !is.null(outer_fit_error)) failure <- outer_fit_error
    rows <- triomics_evaluation_outer_rows(
      outer_fold$fold_id, test_ids, test_truth, prediction_values, failure,
      test_input$reasons, candidate_id, baseline, intercept
    )
    prediction_rows[[outer_fold$fold_id]] <- rows
    prep_refs <- stats::setNames(lapply(integration_spec$block_ids, function(block_id) {
      outer_fold$blocks[[block_id]]$state_ref
    }), integration_spec$block_ids)
    prep_hashes <- stats::setNames(vapply(integration_spec$block_ids, function(block_id) {
      as.character(outer_fold$blocks[[block_id]]$state_hash)
    }, character(1)), integration_spec$block_ids)
    common_result <- if (is.null(outer_fit)) NULL else {
      adapter$extract(outer_fit, list(fold_id = outer_fold$fold_id), chosen_params)$result
    }
    outer_records[[outer_fold$fold_id]] <- list(
      fold_id = outer_fold$fold_id,
      training_unit_ids = outer_fold$training_unit_ids,
      test_unit_ids = outer_fold$test_unit_ids,
      training_subject_ids = outer_fold$training_subject_ids,
      test_subject_ids = outer_fold$test_subject_ids,
      candidate_dispositions = candidate_records,
      inner_results = flat_inner,
      tuning_history = lapply(candidate_records, function(record) {
        list(candidate_id = record$id, score = record$score,
             score_definition = record$score_definition,
             inner_fold_ids = record$inner_fold_ids, inner_scores = record$inner_scores,
             status = record$status)
      }),
      chosen_candidate = if (is.null(chosen)) NULL else chosen,
      preprocessing_state_refs = prep_refs,
      preprocessing_state_hashes = prep_hashes,
      backend_fit = outer_fit,
      common_result = common_result,
      training_transforms = if (is.null(outer_fit)) NULL else outer_fit$native_state$training_transform,
      feature_support = if (is.null(outer_fit)) NULL else outer_fit$selected,
      fit_failure = outer_fit_error,
      prediction_failure = outer_prediction_error,
      n_eligible = sum(rows$status %in% c("predicted", "failed")),
      n_predicted = sum(rows$status == "predicted"),
      failed_prediction_count = sum(rows$status == "failed"),
      total_denominator = length(rows)
    )
  }
  ledger <- do.call(rbind, prediction_rows)
  eligible <- ledger$status %in% c("predicted", "failed")
  if (identical(mode, "categorical")) {
    model_metric <- triomics_evaluation_metrics(
      mode, as.character(ledger$truth), as.character(ledger$prediction), ledger$status,
      declared_levels = response$levels
    )
    baseline_status <- ifelse(eligible, "predicted", "ineligible")
    baseline_metric <- triomics_evaluation_metrics(
      mode, as.character(ledger$truth), as.character(ledger$baseline_prediction), baseline_status,
      declared_levels = response$levels
    )
  } else {
    model_metric <- triomics_evaluation_metrics(
      mode, as.numeric(ledger$truth), as.numeric(ledger$prediction), ledger$status,
      training_intercept = as.numeric(ledger$training_intercept)
    )
    baseline_status <- ifelse(eligible, "predicted", "ineligible")
    baseline_metric <- triomics_evaluation_metrics(
      mode, as.numeric(ledger$truth), as.numeric(ledger$baseline_prediction), baseline_status,
      training_intercept = as.numeric(ledger$training_intercept)
    )
  }
  total_denominator <- nrow(ledger)
  n_eligible <- sum(eligible)
  n_predicted <- sum(ledger$status == "predicted")
  failed_count <- sum(ledger$status == "failed")
  ineligible_count <- sum(ledger$status == "ineligible")
  model_metric$total_denominator <- baseline_metric$total_denominator <- as.integer(total_denominator)
  model_metric$n_ineligible <- baseline_metric$n_ineligible <- as.integer(ineligible_count)
  failures <- ledger[ledger$status %in% c("failed", "ineligible"), , drop = FALSE]
  structure(
    list(
      mode = mode,
      method = integration_spec$method,
      native_version = as.character(utils::packageVersion("mixOmics")),
      split_plan_hash = split_plan$creation_hash,
      outer_folds = outer_records,
      predictions = ledger,
      metrics = list(model = model_metric, baseline = baseline_metric),
      failures = failures,
      n_eligible = as.integer(n_eligible),
      n_predicted = as.integer(n_predicted),
      failed_prediction_count = as.integer(failed_count),
      n_ineligible = as.integer(ineligible_count),
      total_denominator = as.integer(total_denominator),
      inspected_fit_count = as.integer(budget$count),
      max_fits = as.integer(budget$maximum)
    ),
    class = "EvaluationResult"
  )
}
