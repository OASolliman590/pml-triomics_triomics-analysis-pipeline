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

triomics_evaluation_compare_metrics <- function(mode, truth, model_prediction,
                                               baseline_prediction, status,
                                               training_intercept = NULL,
                                               declared_levels = NULL,
                                               unit_ids = paste0("unit-", seq_along(status))) {
  if (length(unit_ids) != length(status)) {
    triomics_evaluation_abort("C_EVAL_METRIC_INPUT", "Comparison unit ids and statuses must match.", key = "unit_ids")
  }
  model <- triomics_evaluation_metrics(
    mode, truth, model_prediction, status, declared_levels, training_intercept
  )
  baseline <- triomics_evaluation_metrics(
    mode, truth, baseline_prediction, status, declared_levels, training_intercept
  )
  eligible <- status %in% c("predicted", "failed")
  failed <- status == "failed" & eligible
  matched <- if (identical(mode, "continuous")) status == "predicted" else eligible
  coverage <- if (identical(mode, "continuous") && any(failed)) {
    "successful_predictions_only"
  } else {
    "all_eligible_units"
  }
  list(
    model = model, baseline = baseline, coverage = coverage,
    disposition = if (any(failed)) "failures_retained_and_matched" else "complete_eligible_set",
    n_matched = as.integer(sum(matched)),
    failed_unit_ids = unname(as.character(unit_ids[failed]))
  )
}

triomics_evaluation_subject_ids <- function(split_plan, unit_ids) {
  map <- split_plan$unit_subject
  matched <- match(as.character(unit_ids), as.character(map$unit_id))
  if (anyNA(matched)) {
    triomics_evaluation_abort("C_EVAL_SPLIT_INVALID", "Fold units are absent from the validated subject map.", key = "unit_subject")
  }
  unique(as.character(map$subject_id[matched]))
}

triomics_evaluation_prepare_one_fold <- function(study, predictive_spec, split_plan,
                                                  descriptive_spec, fold) {
  fold_request <- fold
  fold_request$inner <- list()
  fold_plan <- split_plan
  fold_plan$folds <- list(fold_request)
  fold_plan$creation_hash <- NULL
  fold_plan <- triomics_finalize_split_plan(fold_plan)
  triomics_fold_prepare(study, predictive_spec, fold_plan, descriptive_spec)$folds[[1L]]
}

triomics_evaluation_validate_descriptive <- function(study, predictive_spec,
                                                     descriptive_spec, split_plan) {
  triomics_fold_reject_noninductive(predictive_spec)
  if (!is.list(descriptive_spec) ||
      !is.character(descriptive_spec$preparation_context_id) ||
      !is.character(predictive_spec$preparation_context_id) ||
      identical(descriptive_spec$preparation_context_id, predictive_spec$preparation_context_id)) {
    triomics_evaluation_abort("C_EVAL_PREPARATION_INVALID", "Descriptive and predictive contexts need distinct ids.", key = "preparation_context_id")
  }
  if (!is.character(descriptive_spec$training_unit_ids) ||
      !length(descriptive_spec$training_unit_ids) || anyNA(descriptive_spec$training_unit_ids) ||
      any(!nzchar(descriptive_spec$training_unit_ids)) ||
      anyDuplicated(descriptive_spec$training_unit_ids) ||
      !is.list(descriptive_spec$blocks) || !length(descriptive_spec$blocks)) {
    triomics_evaluation_abort("C_EVAL_PREPARATION_INVALID", "Descriptive units and blocks must be explicit and unique.", key = "descriptive_spec")
  }
  known <- as.character(study$sample_registry$sample_id)
  requested <- unique(c(split_plan$unit_subject$unit_id, descriptive_spec$training_unit_ids))
  if (any(!requested %in% known)) {
    triomics_evaluation_abort("C_EVAL_PREPARATION_INVALID", "A split or descriptive unit is absent from the registry.", key = "sample_id")
  }
  for (block_spec in descriptive_spec$blocks) {
    if (!identical(triomics_fold_impute_method(block_spec$operations), "none")) {
      triomics_evaluation_abort("C_EVAL_PREPARATION_INVALID", "The descriptive context cannot impute values.", key = "impute")
    }
    block_id <- as.character(block_spec$block_id[[1L]])
    presence <- triomics_fold_presence(study, block_id, descriptive_spec$training_unit_ids)
    se <- triomics_fold_copy_se(study$experiments[[block_id]])
    se <- se[, presence$present, drop = FALSE]
    sub <- study
    sub$experiments <- stats::setNames(list(se), block_id)
    sub$mae <- triomics_mae_from_experiments(sub)
    local_spec <- descriptive_spec
    local_spec$training_unit_ids <- presence$present
    local_spec$blocks <- list(block_spec)
    prepare_fit(sub, local_spec)
  }
  invisible(TRUE)
}

triomics_evaluation_backend <- function(method, mode) {
  registry_error <- NULL
  adapter <- tryCatch(triomics_integration_backend(method), error = function(error) {
    registry_error <<- error
    NULL
  })
  if (inherits(adapter, "BackendAdapter")) {
    supported_modes <- adapter$evaluation_modes
    if (!is.character(supported_modes) || !mode %in% supported_modes ||
        !all(c("validate", "fit", "predict", "extract") %in% names(adapter))) {
      triomics_evaluation_abort(
        "C_EVAL_CAPABILITY_UNAVAILABLE",
        paste0("Registered backend '", method, "' does not declare the required supervised evaluation capability."),
        key = "integration.method",
        remediation = "Register the backend's supervised task modes and validate/fit/predict/extract methods before evaluation."
      )
    }
    return(adapter)
  }
  if (method %in% c("concat_plsda", "concat_pls")) {
    expected_mode <- if (identical(method, "concat_plsda")) "categorical" else "continuous"
    if (!identical(mode, expected_mode)) {
      triomics_evaluation_abort("C_EVAL_CAPABILITY_UNAVAILABLE", "The supervised backend does not support the declared response mode.", key = "mode")
    }
    return(triomics_supervised_backend(method))
  }
  if (inherits(registry_error, "triomics_error") &&
      identical(registry_error$code, "C_SCORE_BACKEND_UNAVAILABLE")) {
    triomics_evaluation_abort(
      "C_EVAL_BACKEND_UNAVAILABLE",
      paste0("Integration backend '", method, "' is not registered for evaluation."),
      key = "integration.method",
      remediation = "Wait for the backend packet and accepted shared registration before requesting evaluation."
    )
  }
  if (!is.null(registry_error)) stop(registry_error)
  triomics_evaluation_abort("C_EVAL_BACKEND_UNAVAILABLE", "No evaluation backend is available.", key = "integration.method")
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
  params$candidate_id <- candidate$id
  overrides <- candidate[setdiff(names(candidate), "id")]
  params <- utils::modifyList(params, overrides)
  params$candidate_id <- candidate$id
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
                                           candidate_id, baseline, training_intercept,
                                           prediction_status = NULL, prediction_reasons = NULL) {
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
    } else if (!is.null(prediction_status) &&
               identical(prediction_status[[unit_ids[[i]]]], "failed")) {
      status[[i]] <- "failed"
      reason[[i]] <- prediction_reasons[[unit_ids[[i]]]]
      pred[[i]] <- if (is.numeric(pred)) NA_real_ else NA_character_
    } else if (is.null(prediction_status) ||
               !identical(prediction_status[[unit_ids[[i]]]], "predicted")) {
      status[[i]] <- "failed"
      reason[[i]] <- if (is.null(prediction_reasons)) {
        "C_EVAL_PREDICTION_MISSING"
      } else {
        prediction_reasons[[unit_ids[[i]]]]
      }
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

triomics_evaluation_prediction_records <- function(prediction, unit_ids, mode, levels) {
  values <- if (identical(mode, "categorical")) {
    stats::setNames(rep(NA_character_, length(unit_ids)), unit_ids)
  } else {
    stats::setNames(rep(NA_real_, length(unit_ids)), unit_ids)
  }
  status <- stats::setNames(rep("failed", length(unit_ids)), unit_ids)
  reasons <- stats::setNames(rep("C_EVAL_PREDICTION_MISSING", length(unit_ids)), unit_ids)
  if (is.null(prediction)) return(list(predictions = values, status = status, reason_code = reasons))
  matched <- match(unit_ids, as.character(prediction$unit_ids))
  for (i in which(!is.na(matched))) {
    unit_id <- unit_ids[[i]]
    index <- matched[[i]]
    value <- prediction$predictions[[index]]
    invalid <- if (identical(mode, "categorical")) {
      length(value) != 1L || is.na(value) || !as.character(value) %in% levels
    } else {
      length(value) != 1L || !is.numeric(value) || !is.finite(value)
    }
    declared_status <- if (is.null(prediction$status)) NULL else prediction$status[[index]]
    failed <- isTRUE(invalid) || identical(declared_status, "failed")
    status[[unit_id]] <- if (failed) "failed" else "predicted"
    if (failed) {
      reason <- if (is.null(prediction$reason_code)) NA_character_ else prediction$reason_code[[index]]
      reasons[[unit_id]] <- if (!is.na(reason)) {
        as.character(reason)
      } else if (isTRUE(invalid)) {
        "C_EVAL_PREDICTION_INVALID"
      } else {
        "C_EVAL_PREDICTION_FAILED"
      }
    } else {
      values[[unit_id]] <- if (identical(mode, "categorical")) as.character(value) else as.numeric(value)
      reasons[[unit_id]] <- NA_character_
    }
  }
  list(predictions = values, status = status, reason_code = reasons)
}

triomics_evaluation_candidate <- function(outer_fold, candidate, candidate_params,
                                          inner_preparations, study, analysis_spec,
                                          integration_spec, response, mode, levels,
                                          adapter, budget) {
  inner_results <- list()
  fit_failed <- FALSE
  for (inner_fold in outer_fold$inner) {
    preparation <- inner_preparations[[inner_fold$fold_id]]
    test_ids <- inner_fold$test_unit_ids
    test_truth <- response[test_ids]
    if (!is.null(preparation$failure)) {
      prep_error <- preparation$failure
      prep_failure <- triomics_evaluation_native_error(prep_error)
      test_status <- stats::setNames(ifelse(is.na(test_truth), "ineligible", "failed"), test_ids)
      reasons <- stats::setNames(ifelse(is.na(test_truth), "missing_or_undeclared_outcome", prep_failure$code), test_ids)
      failed_metric <- if (any(test_status %in% c("predicted", "failed"))) {
        triomics_evaluation_metrics(
          mode, if (identical(mode, "categorical")) as.character(test_truth) else as.numeric(test_truth),
          if (identical(mode, "categorical")) rep(NA_character_, length(test_ids)) else rep(NA_real_, length(test_ids)),
          test_status, declared_levels = levels,
          training_intercept = if (identical(mode, "continuous")) rep(NA_real_, length(test_ids)) else NULL
        )
      } else NULL
      inner_results[[length(inner_results) + 1L]] <- list(
        candidate_id = candidate$id, fold_id = inner_fold$fold_id,
        status = "preparation_failed", failure = prep_failure,
        training_unit_ids = inner_fold$training_unit_ids,
        test_unit_ids = inner_fold$test_unit_ids,
        training_subject_ids = inner_fold$training_subject_ids,
        test_subject_ids = inner_fold$test_subject_ids,
        preparation_state_refs = NULL, preparation_state_hashes = NULL,
        fitted_state_hash = NULL, tuning_metric = failed_metric,
        ineligible_reasons = reasons
      )
      fit_failed <- TRUE
      next
    }
    inner_prepared <- preparation$fold
    train_input <- triomics_evaluation_prepared_input(
      inner_prepared, inner_fold$training_unit_ids, study, analysis_spec,
      integration_spec$block_ids, response
    )
    test_input <- triomics_evaluation_prepared_input(
      inner_prepared, inner_fold$test_unit_ids, study, analysis_spec,
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
    prediction_records <- triomics_evaluation_prediction_records(
      test_prediction, test_ids, mode, levels
    )
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
      } else {
        test_status[[unit_id]] <- prediction_records$status[[unit_id]]
        reason_codes[[unit_id]] <- prediction_records$reason_code[[unit_id]]
      }
    }
    eligible_status <- test_status %in% c("predicted", "failed")
    inner_metric <- if (any(eligible_status)) {
      if (identical(mode, "categorical")) {
        triomics_evaluation_metrics(
          mode, as.character(test_truth), as.character(prediction_records$predictions), test_status,
          declared_levels = levels
        )
      } else {
        triomics_evaluation_metrics(
          mode, as.numeric(test_truth), as.numeric(prediction_records$predictions), test_status,
          training_intercept = mean(train_input$resolved$outcome)
        )
      }
    } else {
      NULL
    }
    inner_status <- if (!is.null(fit_error)) "fit_failed" else if (!is.null(prediction_error)) {
      "prediction_failed"
    } else if (any(test_status == "failed")) {
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
  if (!is.character(integration_spec$method) || length(integration_spec$method) != 1L ||
      is.na(integration_spec$method) || !nzchar(integration_spec$method)) {
    triomics_evaluation_abort("C_EVAL_BACKEND_UNAVAILABLE", "One integration method id is required.", key = "integration.method")
  }
  adapter <- triomics_evaluation_backend(integration_spec$method, mode)
  request_ledger <- expand.grid(
    fold_id = vapply(split_plan$folds, `[[`, character(1), "fold_id"),
    candidate_id = candidate_ids, stringsAsFactors = FALSE
  )
  triomics_evaluation_validate_descriptive(
    study, analysis_spec$predictive_spec, analysis_spec$descriptive_spec, split_plan
  )
  budget <- new.env(parent = emptyenv())
  budget$count <- 0L
  budget$maximum <- as.integer(maximum)
  outer_records <- stats::setNames(vector("list", length(split_plan$folds)),
                                   vapply(split_plan$folds, `[[`, character(1), "fold_id"))
  prediction_rows <- stats::setNames(vector("list", length(split_plan$folds)), names(outer_records))
  request_ledger$status <- "requested"

  for (outer_plan in split_plan$folds) {
    fold_id <- outer_plan$fold_id
    outer_plan$training_subject_ids <- triomics_evaluation_subject_ids(split_plan, outer_plan$training_unit_ids)
    outer_plan$test_subject_ids <- triomics_evaluation_subject_ids(split_plan, outer_plan$test_unit_ids)
    outer_plan$inner <- lapply(outer_plan$inner, function(inner) {
      inner$training_subject_ids <- triomics_evaluation_subject_ids(split_plan, inner$training_unit_ids)
      inner$test_subject_ids <- triomics_evaluation_subject_ids(split_plan, inner$test_unit_ids)
      inner
    })

    outer_preparation <- tryCatch(
      list(fold = triomics_evaluation_prepare_one_fold(
        study, analysis_spec$predictive_spec, split_plan, analysis_spec$descriptive_spec, outer_plan
      )),
      error = function(error) list(failure = error)
    )
    candidate_records <- list()
    flat_inner <- list()
    inner_preparations <- stats::setNames(vector("list", length(outer_plan$inner)),
                                          vapply(outer_plan$inner, `[[`, character(1), "fold_id"))
    if (is.null(outer_preparation$failure)) {
      outer_fold <- outer_plan
      prepared_outer <- outer_preparation$fold
      outer_fold$blocks <- prepared_outer$blocks
      outer_fold$contexts <- prepared_outer$contexts
      outer_fold$preparation_context_id <- prepared_outer$preparation_context_id
      for (inner_plan in outer_plan$inner) {
        inner_preparations[[inner_plan$fold_id]] <- tryCatch(
          list(fold = triomics_evaluation_prepare_one_fold(
            study, analysis_spec$predictive_spec, split_plan,
            analysis_spec$descriptive_spec, inner_plan
          )),
          error = function(error) list(failure = error)
        )
      }
      for (candidate in candidates) {
        candidate_params <- triomics_evaluation_candidate_params(
          integration_spec, candidate, response$levels
        )
        candidate_params$task_mode <- mode
        candidate_result <- triomics_evaluation_candidate(
          outer_fold, candidate, candidate_params, inner_preparations, study,
          analysis_spec, integration_spec, response$values, mode,
          response$levels, adapter, budget
        )
        candidate_records[[candidate$id]] <- candidate_result[
          setdiff(names(candidate_result), c("inner_results", "candidate"))
        ]
        flat_inner <- c(flat_inner, candidate_result$inner_results)
      }
    } else {
      preparation_failure <- triomics_evaluation_native_error(outer_preparation$failure)
      for (candidate in candidates) {
        failed_inner <- lapply(outer_plan$inner, function(inner) {
          outcome <- response$values[inner$test_unit_ids]
          list(
            candidate_id = candidate$id, fold_id = inner$fold_id,
            status = "preparation_failed", failure = preparation_failure,
            training_unit_ids = inner$training_unit_ids,
            test_unit_ids = inner$test_unit_ids,
            training_subject_ids = inner$training_subject_ids,
            test_subject_ids = inner$test_subject_ids,
            preparation_state_refs = NULL, preparation_state_hashes = NULL,
            fitted_state_hash = NULL, tuning_metric = NULL,
            ineligible_reasons = stats::setNames(
              ifelse(is.na(outcome), "missing_or_undeclared_outcome", preparation_failure$code),
              inner$test_unit_ids
            )
          )
        })
        names(failed_inner) <- vapply(outer_plan$inner, `[[`, character(1), "fold_id")
        candidate_records[[candidate$id]] <- list(
          id = candidate$id, status = "failed", score = NA_real_,
          score_definition = if (identical(mode, "categorical")) "mean_inner_balanced_error" else "mean_inner_rmse",
          inner_fold_ids = names(failed_inner), inner_scores = rep(NA_real_, length(failed_inner)),
          failure = preparation_failure
        )
        flat_inner <- c(flat_inner, failed_inner)
      }
      outer_fold <- outer_plan
    }

    for (requested_candidate in candidate_ids) {
      request_index <- which(request_ledger$fold_id == fold_id &
                             request_ledger$candidate_id == requested_candidate)
      request_ledger$status[request_index] <- candidate_records[[requested_candidate]]$status
    }

    test_ids <- outer_plan$test_unit_ids
    test_truth <- response$values[test_ids]
    candidate_id <- NA_character_
    chosen <- NULL
    outer_fit <- NULL
    outer_prediction <- NULL
    outer_fit_error <- NULL
    outer_prediction_error <- NULL
    outer_preparation_error <- NULL
    prediction_reasons <- stats::setNames(rep(NA_character_, length(test_ids)), test_ids)
    if (!is.null(outer_preparation$failure)) {
      outer_preparation_error <- triomics_evaluation_native_error(outer_preparation$failure)
      train_response <- response$values[outer_plan$training_unit_ids]
      training_intercept <- if (identical(mode, "continuous")) {
        mean(as.numeric(train_response), na.rm = TRUE)
      } else NA_real_
      if (identical(mode, "continuous") && !is.finite(training_intercept)) training_intercept <- NA_real_
      baseline <- triomics_evaluation_baseline(mode, train_response, response$levels, length(test_ids))
      names(baseline) <- test_ids
      prediction_values <- if (identical(mode, "categorical")) {
        stats::setNames(rep(NA_character_, length(test_ids)), test_ids)
      } else stats::setNames(rep(NA_real_, length(test_ids)), test_ids)
      rows <- triomics_evaluation_outer_rows(
        fold_id, test_ids, test_truth, prediction_values, outer_preparation_error,
        prediction_reasons, candidate_id, baseline, training_intercept
      )
      outer_fold$blocks <- NULL
      outer_fold$contexts <- NULL
      outer_fold$preparation_context_id <- NULL
    } else {
      score_vector <- vapply(candidate_records, function(record) record$score, numeric(1))
      train_input <- triomics_evaluation_prepared_input(
        outer_fold, outer_plan$training_unit_ids, study, analysis_spec,
        integration_spec$block_ids, response$values
      )
      test_input <- triomics_evaluation_prepared_input(
        outer_fold, test_ids, study, analysis_spec, integration_spec$block_ids
      )
      if (!any(is.finite(score_vector))) {
        outer_fit_error <- list(code = "C_EVAL_NO_CANDIDATE", message = "No candidate completed every required inner split.")
      } else {
        chosen_index <- which(is.finite(score_vector))[which.min(score_vector[is.finite(score_vector)])]
        chosen <- candidates[[match(names(score_vector)[chosen_index], candidate_ids)]]
        candidate_id <- chosen$id
        chosen_params <- triomics_evaluation_candidate_params(integration_spec, chosen, response$levels)
        chosen_params$task_mode <- mode
        outer_fit <- tryCatch({
          if (length(train_input$complete_units) != length(outer_plan$training_unit_ids)) {
            triomics_evaluation_abort(
              "C_EVAL_REQUIRED_VIEW_MISSING",
              "An outer training partition lacks a required predictor view; the partition was not silently reduced.",
              key = fold_id
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
      test_input <- triomics_evaluation_prepared_input(
        outer_fold, test_ids, study, analysis_spec, integration_spec$block_ids
      )
      prediction_records <- triomics_evaluation_prediction_records(
        outer_prediction, test_ids, mode, response$levels
      )
      prediction_values <- prediction_records$predictions
      prediction_reasons <- prediction_records$reason_code
      training_response <- train_input$resolved$outcome
      training_intercept <- if (identical(mode, "continuous")) mean(training_response) else NA_real_
      baseline <- triomics_evaluation_baseline(mode, training_response, response$levels, length(test_ids))
      names(baseline) <- test_ids
      prediction_failure <- if (!is.null(outer_prediction_error)) outer_prediction_error else outer_fit_error
      rows <- triomics_evaluation_outer_rows(
        fold_id, test_ids, test_truth, prediction_values, prediction_failure,
        test_input$reasons, candidate_id, baseline, training_intercept,
        prediction_records$status, prediction_reasons
      )
    }
    prediction_rows[[fold_id]] <- rows
    outer_records[[fold_id]] <- list(
      fold_id = fold_id,
      training_unit_ids = outer_plan$training_unit_ids,
      test_unit_ids = outer_plan$test_unit_ids,
      training_subject_ids = outer_plan$training_subject_ids,
      test_subject_ids = outer_plan$test_subject_ids,
      candidate_dispositions = candidate_records,
      inner_results = flat_inner,
      tuning_history = lapply(candidate_records, function(record) {
        list(candidate_id = record$id, score = record$score,
             score_definition = record$score_definition,
             inner_fold_ids = record$inner_fold_ids, inner_scores = record$inner_scores,
             status = record$status)
      }),
      chosen_candidate = if (is.null(chosen)) NULL else chosen,
      preprocessing_state_refs = if (is.null(outer_preparation$failure)) {
        stats::setNames(lapply(integration_spec$block_ids, function(block_id) {
          outer_fold$blocks[[block_id]]$state_ref
        }), integration_spec$block_ids)
      } else NULL,
      preprocessing_state_hashes = if (is.null(outer_preparation$failure)) {
        stats::setNames(vapply(integration_spec$block_ids, function(block_id) {
          as.character(outer_fold$blocks[[block_id]]$state_hash)
        }, character(1)), integration_spec$block_ids)
      } else NULL,
      backend_fit = outer_fit,
      common_result = if (is.null(outer_fit)) NULL else adapter$extract(
        outer_fit, list(fold_id = fold_id),
        triomics_evaluation_candidate_params(integration_spec, chosen, response$levels)
      )$result,
      training_transforms = if (is.null(outer_fit)) NULL else outer_fit$native_state$training_transform,
      feature_support = if (is.null(outer_fit)) NULL else outer_fit$selected,
      fit_failure = outer_fit_error,
      prediction_failure = outer_prediction_error,
      preparation_failure = outer_preparation_error,
      n_eligible = sum(rows$status %in% c("predicted", "failed")),
      n_predicted = sum(rows$status == "predicted"),
      failed_prediction_count = sum(rows$status == "failed"),
      n_ineligible = sum(rows$status == "ineligible"),
      total_denominator = nrow(rows)
    )
  }
  ledger <- do.call(rbind, prediction_rows)
  comparison <- triomics_evaluation_compare_metrics(
    mode, if (identical(mode, "categorical")) as.character(ledger$truth) else as.numeric(ledger$truth),
    if (identical(mode, "categorical")) as.character(ledger$prediction) else as.numeric(ledger$prediction),
    if (identical(mode, "categorical")) as.character(ledger$baseline_prediction) else as.numeric(ledger$baseline_prediction),
    ledger$status, training_intercept = if (identical(mode, "continuous")) ledger$training_intercept else NULL,
    declared_levels = response$levels, unit_ids = ledger$unit_id
  )
  total_denominator <- nrow(ledger)
  n_eligible <- sum(ledger$status %in% c("predicted", "failed"))
  n_predicted <- sum(ledger$status == "predicted")
  failed_count <- sum(ledger$status == "failed")
  ineligible_count <- sum(ledger$status == "ineligible")
  comparison$model$total_denominator <- comparison$baseline$total_denominator <- as.integer(total_denominator)
  comparison$model$n_ineligible <- comparison$baseline$n_ineligible <- as.integer(ineligible_count)
  structure(
    list(
      mode = mode, method = integration_spec$method,
      native_version = as.character(utils::packageVersion("mixOmics")),
      split_plan_hash = split_plan$creation_hash,
      outer_folds = outer_records, predictions = ledger,
      metrics = list(model = comparison$model, baseline = comparison$baseline),
      baseline_comparison = comparison[setdiff(names(comparison), c("model", "baseline"))],
      uncertainty = list(
        method = "not_estimated", disposition = "not_estimated",
        resampling_unit = as.character(split_plan$grouping_unit),
        dependence_limitations = paste0(
          "Outer-fold estimates are not independent because their training sets overlap; resampling groups by ",
          split_plan$grouping_unit, "."
        )
      ),
      failures = ledger[ledger$status %in% c("failed", "ineligible"), , drop = FALSE],
      requested_folds = request_ledger,
      n_eligible = as.integer(n_eligible), n_predicted = as.integer(n_predicted),
      failed_prediction_count = as.integer(failed_count), n_ineligible = as.integer(ineligible_count),
      total_denominator = as.integer(total_denominator),
      inspected_fit_count = as.integer(budget$count), max_fits = as.integer(budget$maximum)
    ),
    class = "EvaluationResult"
  )
}
