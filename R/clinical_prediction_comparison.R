triomics_c09_abort <- function(code, message, key = NULL) {
  triomics_abort(
    code, message, key = key, module = "clinical_prediction", stage = "C",
    remediation = "Review the declared C-09 protocol, C-08 lineage, and C-02 split/evaluation inputs."
  )
}

triomics_c09_scalar_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
}

triomics_c09_validate_protocol <- function(protocol, split_plan) {
  if (!is.list(protocol) || !triomics_c09_scalar_text(protocol$protocol_id) ||
      !identical(protocol$approval_scope, "synthetic_fixture_only") ||
      !triomics_c09_scalar_text(protocol$endpoint_id)) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "C-09 requires one explicitly identified synthetic fixture protocol; real endpoint approval is not present.",
      "protocol"
    )
  }
  model <- protocol$response_model
  if (!is.list(model) || !identical(model$type, "continuous") ||
      !identical(model$algorithm, "concat_pls") ||
      !identical(model$baseline, "training_intercept") ||
      !identical(model$metric_names, c("rmse", "mae", "r_squared"))) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "The synthetic C-09 route supports scalar continuous concat_pls with the training-intercept baseline and fixed RMSE/MAE/R-squared metrics.",
      "response_model"
    )
  }
  horizon <- protocol$horizon
  if (!is.list(horizon) || !identical(horizon$anchor, "V0") ||
      !identical(horizon$outcome_visit, "V30") || !is.numeric(horizon$value) ||
      length(horizon$value) != 1L || is.na(horizon$value) ||
      !is.finite(horizon$value) || horizon$value != 30 ||
      !identical(horizon$unit, "day")) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "The public software fixture declares a 30-day response from baseline V0 to outcome visit V30.",
      "horizon"
    )
  }
  information <- protocol$information_set
  if (!is.list(information) || !identical(information$prediction_time, "V0") ||
      !identical(information$availability_rule, "available_by_prediction_time") ||
      !isTRUE(information$excludes_future) ||
      !is.character(information$clinical_feature_ids) || !length(information$clinical_feature_ids) ||
      anyNA(information$clinical_feature_ids) || anyDuplicated(information$clinical_feature_ids) ||
      !is.character(information$molecular_feature_ids) || !length(information$molecular_feature_ids) ||
      anyNA(information$molecular_feature_ids) || anyDuplicated(information$molecular_feature_ids) ||
      !identical(names(information$candidate_block_ids), c("clinical_only", "molecular_only", "combined")) ||
      !identical(information$candidate_block_ids$clinical_only, "clinical") ||
      !identical(information$candidate_block_ids$molecular_only, "molecular") ||
      !identical(information$candidate_block_ids$combined, c("clinical", "molecular"))) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "The C-09 information set must name baseline clinical and molecular features and the fixed three-candidate block map.",
      "information_set"
    )
  }
  deployment <- protocol$deployment
  if (!is.list(deployment) ||
      !identical(deployment$scope, "synthetic_research_comparison") ||
      !identical(deployment$real_deployment_status, "not_qualified")) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "The fixture is research-only software evidence; real clinical deployment remains unqualified.",
      "deployment"
    )
  }
  uncertainty <- protocol$uncertainty
  if (!is.list(uncertainty) ||
      !identical(uncertainty$method, "paired_subject_cluster_bootstrap") ||
      !identical(uncertainty$resampling_unit, "subject") ||
      !is.numeric(uncertainty$replicates) || length(uncertainty$replicates) != 1L ||
      is.na(uncertainty$replicates) || uncertainty$replicates < 100 ||
      uncertainty$replicates != as.integer(uncertainty$replicates) ||
      !is.numeric(uncertainty$confidence_level) || length(uncertainty$confidence_level) != 1L ||
      is.na(uncertainty$confidence_level) || uncertainty$confidence_level <= 0 ||
      uncertainty$confidence_level >= 1 || !is.numeric(uncertainty$seed) ||
      length(uncertainty$seed) != 1L || is.na(uncertainty$seed) ||
      uncertainty$seed < 0 || uncertainty$seed != as.integer(uncertainty$seed)) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "C-09 uncertainty must declare at least 100 paired subject-cluster bootstrap replicates, a confidence level, and a fixed seed.",
      "uncertainty"
    )
  }
  if (!identical(split_plan$target, protocol$endpoint_id) ||
      !identical(split_plan$grouping_unit, "subject")) {
    triomics_c09_abort(
      "C_C09_SPLIT_INVALID",
      "The C-02 SplitPlan must target the declared endpoint and group held-out evaluation by subject.",
      "split_plan"
    )
  }
  invisible(TRUE)
}

triomics_c09_validate_c08 <- function(endpoint_bundle, clinical_score_request, split_plan) {
  tryCatch(
    triomics_c08_validate_endpoint_bundle(endpoint_bundle),
    error = function(error) triomics_c09_abort(
      "C_C09_ENDPOINT_INVALID", paste0("The C-08 endpoint bundle failed validation: ", conditionMessage(error)),
      "endpoint_bundle"
    )
  )
  if (!inherits(clinical_score_request, "ClinicalScoreAssociationRequest") ||
      !identical(clinical_score_request$artifact_type, "clinical_score_association_request") ||
      !identical(clinical_score_request$request_type, "clinical_score_association") ||
      !identical(clinical_score_request$owner_stage, "B") ||
      !identical(clinical_score_request$status, "scheduled") ||
      !identical(clinical_score_request$model_execution, "not_started") ||
      !identical(clinical_score_request$recurses_to_stage_c, FALSE) ||
      length(clinical_score_request$results) != 0L ||
      !identical(clinical_score_request$source_endpoint_bundle_sha256, endpoint_bundle$payload_sha256) ||
      !identical(clinical_score_request$endpoint_bundle$source_payload_sha256, endpoint_bundle$payload_sha256) ||
      !identical(clinical_score_request$endpoint_bundle_sha256,
                 clinical_score_request$endpoint_bundle$payload_sha256)) {
    triomics_c09_abort(
      "C_C09_ENDPOINT_INVALID",
      "C-09 requires the immutable C-08 endpoint bundle and its scheduled, non-recursive B request.",
      "clinical_score_request"
    )
  }
  request_body <- clinical_score_request
  request_hash <- clinical_score_request$request_sha256
  request_body$request_sha256 <- NULL
  if (!triomics_c09_scalar_text(request_hash) ||
      !identical(request_hash, triomics_c08_sha256(request_body))) {
    triomics_c09_abort("C_C09_ENDPOINT_INVALID", "The C-08 score request checksum is invalid.", "request_sha256")
  }
  tryCatch(
    triomics_c08_validate_endpoint_bundle(clinical_score_request$endpoint_bundle),
    error = function(error) triomics_c09_abort(
      "C_C09_ENDPOINT_INVALID", paste0("The persisted C-08 request endpoint failed validation: ", conditionMessage(error)),
      "clinical_score_request.endpoint_bundle"
    )
  )
  expected_units <- as.character(split_plan$unit_subject$unit_id)
  endpoint <- endpoint_bundle$endpoints[[1L]]
  endpoint_units <- vapply(endpoint$samples, function(sample) as.character(sample$unit_id), character(1))
  if (!setequal(endpoint_units, expected_units) || length(endpoint_units) != length(expected_units)) {
    triomics_c09_abort(
      "C_C09_COHORT_MISMATCH",
      "The C-08 endpoint units must exactly match the C-02 evaluation cohort.",
      "endpoint_bundle.endpoints"
    )
  }
  invisible(TRUE)
}

triomics_c09_reject_unqualified_mofa <- function(endpoint_bundle, split_plan) {
  held_out <- unique(unlist(lapply(split_plan$folds, `[[`, "test_unit_ids"), use.names = FALSE))
  for (endpoint in endpoint_bundle$endpoints) {
    fit <- endpoint$producing_fit
    method <- if (is.list(fit)) fit$native_method else NULL
    is_mofa <- identical(method, "MOFA2::run_mofa") ||
      identical(endpoint$integration_id, "mofa2") ||
      identical(endpoint$backend, "mofa2")
    if (!is_mofa) next
    training_units <- as.character(endpoint$training_unit_ids)
    overlaps_held_out <- length(intersect(training_units, held_out)) > 0L
    has_fold_local_inference <- is.list(fit) &&
      identical(fit$predictive_qualification, "fold_local_new_subject_inference") &&
      identical(fit$predictive_status, "qualified") &&
      isTRUE(endpoint$inference_status == "qualified")
    if (overlaps_held_out || !has_fold_local_inference) {
      triomics_c09_abort(
        "C_C09_MOFA_NONINDUCTIVE",
        "A full-cohort MOFA representation cannot precede held-out evaluation; the C-08 handoff has no qualified fold-local new-subject inference.",
        "endpoint_bundle.mofa2"
      )
    }
  }
  invisible(TRUE)
}

triomics_c09_validate_candidate_inputs <- function(protocol, candidates, split_plan) {
  required_names <- c("clinical_only", "molecular_only", "combined")
  if (!is.list(candidates) || !identical(names(candidates), required_names)) {
    triomics_c09_abort(
      "C_C09_CANDIDATE_MISMATCH",
      "C-09 requires clinical-only, molecular-only, and combined candidates in the declared order.",
      "candidates"
    )
  }
  unit_map <- split_plan$unit_subject
  unit_ids <- as.character(unit_map$unit_id)
  expected_blocks <- protocol$information_set$candidate_block_ids
  feature_ids <- list(
    clinical_only = protocol$information_set$clinical_feature_ids,
    molecular_only = protocol$information_set$molecular_feature_ids,
    combined = c(protocol$information_set$clinical_feature_ids,
                 protocol$information_set$molecular_feature_ids)
  )
  reference_outcome <- NULL
  reference_subjects <- NULL
  reference_candidate_grid <- NULL
  for (candidate_name in required_names) {
    candidate <- candidates[[candidate_name]]
    if (!is.list(candidate) || !is.list(candidate$study) ||
        !is.data.frame(candidate$study$sample_registry) ||
        !is.list(candidate$analysis_spec) || !is.list(candidate$integration_spec)) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        paste0("Candidate '", candidate_name, "' needs an assembled study and C-02 analysis/integration specifications."),
        candidate_name
      )
    }
    analysis_outcome <- candidate$analysis_spec$outcome
    if (!is.list(analysis_outcome) ||
        !identical(analysis_outcome$column, protocol$endpoint_id) ||
        !identical(analysis_outcome$type, protocol$response_model$type) ||
        !identical(candidate$integration_spec$method, protocol$response_model$algorithm) ||
        !identical(as.character(candidate$integration_spec$block_ids), expected_blocks[[candidate_name]]) ||
        !identical(candidate$integration_spec$candidates, reference_candidate_grid %||% candidate$integration_spec$candidates)) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "All candidate inputs must use the same declared response, model, nested candidate grid, and their protocol-defined information set.",
        candidate_name
      )
    }
    if (!is.list(candidate$analysis_spec$predictive_spec) ||
        !is.list(candidate$analysis_spec$descriptive_spec) ||
        !identical(vapply(candidate$analysis_spec$predictive_spec$blocks, function(block) {
          as.character(block$block_id[[1L]])
        }, character(1)), expected_blocks[[candidate_name]])) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "Predictive preparation blocks must match the declared candidate information set.",
        candidate_name
      )
    }
    registry <- candidate$study$sample_registry
    registry_ids <- as.character(registry$sample_id)
    if (anyDuplicated(registry_ids) || !setequal(registry_ids, unit_ids) ||
        length(registry_ids) != length(unit_ids) || !protocol$endpoint_id %in% names(registry)) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "Candidate studies must contain the same declared response and exactly the SplitPlan units.",
        candidate_name
      )
    }
    matched <- match(unit_ids, registry_ids)
    subjects <- as.character(registry$subject_id[matched])
    outcome <- as.numeric(registry[[protocol$endpoint_id]][matched])
    if (!identical(subjects, as.character(unit_map$subject_id)) ||
        (!is.null(reference_subjects) && !identical(subjects, reference_subjects)) ||
        (!is.null(reference_outcome) && !identical(outcome, reference_outcome))) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "All candidates must use the same subject map and same declared response values for the same held-out patients.",
        candidate_name
      )
    }
    registry_features <- candidate$study$feature_registry
    if (!is.data.frame(registry_features) ||
        !setequal(as.character(registry_features$block_id), expected_blocks[[candidate_name]]) ||
        !setequal(as.character(registry_features$feature_key), feature_ids[[candidate_name]])) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "Candidate feature keys must match the clinical and molecular features declared by the synthetic protocol.",
        paste0(candidate_name, ".feature_registry")
      )
    }
    grid <- candidate$integration_spec$candidates
    if (!is.list(grid) || length(grid) < 2L) {
      triomics_c09_abort(
        "C_C09_NESTED_SELECTION_INVALID",
        "Each candidate must expose at least two declared tuning candidates for nested selection.",
        paste0(candidate_name, ".integration_spec.candidates")
      )
    }
    if (is.null(reference_candidate_grid)) reference_candidate_grid <- grid
    if (is.null(reference_subjects)) reference_subjects <- subjects
    if (is.null(reference_outcome)) reference_outcome <- outcome
  }
  invisible(TRUE)
}

triomics_c09_validate_candidate_results <- function(evaluations, split_plan, protocol) {
  required_names <- c("clinical_only", "molecular_only", "combined")
  if (!is.list(evaluations) || !identical(names(evaluations), required_names)) {
    triomics_c09_abort("C_C09_CANDIDATE_MISMATCH", "C-02 must return all three named candidate evaluations.", "evaluations")
  }
  fold_ids <- vapply(split_plan$folds, `[[`, character(1), "fold_id")
  expected <- do.call(rbind, lapply(split_plan$folds, function(fold) {
    data.frame(fold_id = fold$fold_id, unit_id = as.character(fold$test_unit_ids), stringsAsFactors = FALSE)
  }))
  reference_truth <- NULL
  reference_status <- NULL
  for (candidate_name in required_names) {
    evaluation <- evaluations[[candidate_name]]
    if (!inherits(evaluation, "EvaluationResult") ||
        !identical(evaluation$split_plan_hash, split_plan$creation_hash) ||
        !identical(evaluation$mode, protocol$response_model$type) ||
        !identical(evaluation$method, protocol$response_model$algorithm) ||
        !identical(names(evaluation$outer_folds), fold_ids) ||
        !is.data.frame(evaluation$predictions) ||
        !all(c("fold_id", "unit_id", "truth", "prediction", "status", "training_intercept") %in%
             names(evaluation$predictions))) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        paste0("Candidate '", candidate_name, "' is not a compatible C-02 EvaluationResult on the declared SplitPlan."),
        candidate_name
      )
    }
    ledger <- evaluation$predictions
    observed <- data.frame(
      fold_id = as.character(ledger$fold_id), unit_id = as.character(ledger$unit_id),
      stringsAsFactors = FALSE
    )
    if (nrow(ledger) != nrow(expected) || !identical(observed, expected)) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "Clinical-only, molecular-only, and combined results must contain the same patients in the same outer folds.",
        paste0(candidate_name, ".predictions")
      )
    }
    truth <- as.numeric(ledger$truth)
    status <- as.character(ledger$status)
    if (any(!status %in% c("predicted", "failed", "ineligible")) ||
        (!is.null(reference_truth) && !identical(truth, reference_truth)) ||
        (!is.null(reference_status) && !identical(status, reference_status))) {
      triomics_c09_abort(
        "C_C09_CANDIDATE_MISMATCH",
        "Matched incremental metrics require identical outcomes and failure/ineligibility dispositions for every held-out patient.",
        paste0(candidate_name, ".predictions")
      )
    }
    for (fold_id in fold_ids) {
      fold <- evaluation$outer_folds[[fold_id]]
      plan_fold <- split_plan$folds[[match(fold_id, fold_ids)]]
      expected_inner_ids <- vapply(plan_fold$inner, `[[`, character(1), "fold_id")
      dispositions <- fold$candidate_dispositions
      disposition_ids <- if (is.list(dispositions)) {
        vapply(dispositions, function(disposition) {
          if (is.list(disposition) && triomics_c09_scalar_text(disposition$id)) {
            disposition$id
          } else NA_character_
        }, character(1))
      } else character()
      tuning_ids <- if (is.list(fold$tuning_history)) {
        vapply(fold$tuning_history, function(record) {
          if (is.list(record) && triomics_c09_scalar_text(record$candidate_id)) {
            record$candidate_id
          } else NA_character_
        }, character(1))
      } else character()
      inner_fold_ids <- if (is.list(fold$inner_results)) {
        vapply(fold$inner_results, function(inner) {
          if (is.list(inner) && triomics_c09_scalar_text(inner$fold_id)) inner$fold_id else NA_character_
        }, character(1))
      } else character()
      inner_candidate_ids <- if (is.list(fold$inner_results)) {
        vapply(fold$inner_results, function(inner) {
          if (is.list(inner) && triomics_c09_scalar_text(inner$candidate_id)) inner$candidate_id else NA_character_
        }, character(1))
      } else character()
      chosen_id <- if (is.list(fold$chosen_candidate) &&
                       triomics_c09_scalar_text(fold$chosen_candidate$id)) {
        fold$chosen_candidate$id
      } else NULL
      preparation_failure_history <- length(inner_candidate_ids) > 0L &&
        all(is.na(inner_candidate_ids)) && all(vapply(dispositions, function(disposition) {
          identical(disposition$status, "failed")
        }, logical(1)))
      valid_inner_records <- is.list(fold$inner_results) && length(fold$inner_results) > 0L &&
        !anyNA(inner_fold_ids) && all(inner_fold_ids %in% expected_inner_ids) &&
        length(inner_fold_ids) == length(expected_inner_ids) * length(disposition_ids) &&
        all(vapply(fold$inner_results, function(inner) {
          is.list(inner) && triomics_c09_scalar_text(inner$fold_id) &&
            triomics_c09_scalar_text(inner$status)
        }, logical(1))) &&
        (all(!is.na(inner_candidate_ids) & inner_candidate_ids %in% disposition_ids) ||
           preparation_failure_history)
      if (valid_inner_records && all(!is.na(inner_candidate_ids))) {
        expected_pairs <- as.vector(outer(unname(disposition_ids), unname(expected_inner_ids), paste, sep = "\r"))
        observed_pairs <- paste(inner_candidate_ids, inner_fold_ids, sep = "\r")
        valid_inner_records <- setequal(observed_pairs, expected_pairs) &&
          length(observed_pairs) == length(expected_pairs)
      } else if (valid_inner_records && preparation_failure_history) {
        valid_inner_records <- all(tabulate(match(inner_fold_ids, expected_inner_ids),
                                            nbins = length(expected_inner_ids)) == length(disposition_ids))
      }
      valid_history <- length(disposition_ids) > 0L && !anyNA(disposition_ids) &&
        !anyDuplicated(disposition_ids) &&
        identical(as.character(names(dispositions)), unname(disposition_ids)) &&
        identical(unname(tuning_ids), unname(disposition_ids)) &&
        all(vapply(fold$tuning_history, function(record) {
          identical(unname(as.character(record$inner_fold_ids)), unname(expected_inner_ids)) &&
            triomics_c09_scalar_text(record$status)
        }, logical(1))) && valid_inner_records &&
        (is.null(chosen_id) || chosen_id %in% disposition_ids) &&
        (!is.null(chosen_id) || all(vapply(dispositions, function(disposition) {
          identical(disposition$status, "failed")
        }, logical(1)))) &&
        identical(unname(as.character(fold$training_unit_ids)),
                  unname(as.character(plan_fold$training_unit_ids))) &&
        identical(unname(as.character(fold$test_unit_ids)),
                  unname(as.character(plan_fold$test_unit_ids)))
      if (!valid_history) {
        triomics_c09_abort(
          "C_C09_NESTED_SELECTION_INVALID",
          paste0("Candidate '", candidate_name, "' is missing C-02 inner-fold and tested-candidate history for ", fold_id, "."),
          paste0(candidate_name, ".outer_folds.", fold_id)
        )
      }
    }
    if (is.null(reference_truth)) reference_truth <- truth
    if (is.null(reference_status)) reference_status <- status
  }
  invisible(TRUE)
}

triomics_c09_metric_values <- function(evaluation, protocol) {
  ledger <- evaluation$predictions
  metrics <- triomics_evaluation_metrics(
    "continuous", as.numeric(ledger$truth), as.numeric(ledger$prediction),
    as.character(ledger$status), training_intercept = as.numeric(ledger$training_intercept)
  )
  metrics[protocol$response_model$metric_names]
}

triomics_c09_metric_value <- function(name, truth, prediction, intercept) {
  predicted <- is.finite(truth) & is.finite(prediction)
  if (!any(predicted)) return(NA_real_)
  error <- truth[predicted] - prediction[predicted]
  switch(name,
    rmse = sqrt(mean(error^2)),
    mae = mean(abs(error)),
    r_squared = {
      denominator <- sum((truth[predicted] - intercept[predicted])^2)
      if (denominator <= 0) NA_real_ else 1 - sum(error^2) / denominator
    },
    NA_real_
  )
}

triomics_c09_bootstrap_delta <- function(metric, combined, reference, split_plan, protocol) {
  combined_ledger <- combined$predictions
  reference_ledger <- reference$predictions
  predicted <- combined_ledger$status == "predicted"
  if (!any(predicted)) {
    return(list(
      estimate = NA_real_, interval = c(NA_real_, NA_real_),
      uncertainty_method = "not_estimable", resampling_unit = "subject",
      confidence_level = protocol$uncertainty$confidence_level,
      requested_replicates = as.integer(protocol$uncertainty$replicates),
      completed_replicates = 0L, failed_replicates = as.integer(protocol$uncertainty$replicates)
    ))
  }
  rows <- which(predicted)
  truth <- as.numeric(combined_ledger$truth[rows])
  left <- as.numeric(combined_ledger$prediction[rows])
  right <- as.numeric(reference_ledger$prediction[rows])
  intercept <- as.numeric(combined_ledger$training_intercept[rows])
  unit_ids <- as.character(combined_ledger$unit_id[rows])
  subject_ids <- as.character(split_plan$unit_subject$subject_id[
    match(unit_ids, as.character(split_plan$unit_subject$unit_id))
  ])
  subject_clusters <- unique(subject_ids)
  point <- triomics_c09_metric_value(metric, truth, left, intercept) -
    triomics_c09_metric_value(metric, truth, right, intercept)
  if (length(subject_clusters) < 2L || !is.finite(point)) {
    return(list(
      estimate = unname(point), interval = c(NA_real_, NA_real_),
      uncertainty_method = "not_estimable", resampling_unit = "subject",
      confidence_level = protocol$uncertainty$confidence_level,
      requested_replicates = as.integer(protocol$uncertainty$replicates),
      completed_replicates = 0L, failed_replicates = as.integer(protocol$uncertainty$replicates)
    ))
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(protocol$uncertainty$seed))
  replicates <- as.integer(protocol$uncertainty$replicates)
  samples <- replicate(replicates, {
    selected_clusters <- sample(subject_clusters, length(subject_clusters), replace = TRUE)
    sampled_rows <- unlist(lapply(selected_clusters, function(subject_id) {
      which(subject_ids == subject_id)
    }), use.names = FALSE)
    triomics_c09_metric_value(metric, truth[sampled_rows], left[sampled_rows], intercept[sampled_rows]) -
      triomics_c09_metric_value(metric, truth[sampled_rows], right[sampled_rows], intercept[sampled_rows])
  }, simplify = TRUE)
  finite <- samples[is.finite(samples)]
  confidence <- protocol$uncertainty$confidence_level
  interval <- if (length(finite)) {
    unname(as.numeric(stats::quantile(
      finite, probs = c((1 - confidence) / 2, 1 - (1 - confidence) / 2),
      names = FALSE, type = 7
    )))
  } else {
    c(NA_real_, NA_real_)
  }
  list(
    estimate = unname(point), interval = interval,
    uncertainty_method = "paired_subject_cluster_bootstrap_percentile_type7",
    resampling_unit = "subject", confidence_level = confidence,
    requested_replicates = replicates, completed_replicates = as.integer(length(finite)),
    failed_replicates = as.integer(replicates - length(finite)),
    dependence_limitation = "Conditional on the fixed nested SplitPlan; resampling subjects does not remove overlap among training sets."
  )
}

triomics_c09_candidate_history <- function(evaluation) {
  lapply(evaluation$outer_folds, function(fold) {
    list(
      fold_id = fold$fold_id,
      training_unit_ids = fold$training_unit_ids,
      test_unit_ids = fold$test_unit_ids,
      chosen_candidate_id = if (is.null(fold$chosen_candidate)) NULL else fold$chosen_candidate$id,
      candidate_tuning = fold$tuning_history,
      inner_selection = lapply(fold$inner_results, function(inner) {
        list(
          candidate_id = inner$candidate_id, inner_fold_id = inner$fold_id,
          status = inner$status,
          metric = if (is.null(inner$tuning_metric)) NULL else inner$tuning_metric$rmse,
          failure = if (is.null(inner$failure)) NULL else inner$failure$code
        )
      })
    )
  })
}

triomics_c09_candidate_denominator <- function(evaluation) {
  ledger <- evaluation$predictions
  status <- as.character(ledger$status)
  failure_reasons <- table(factor(
    as.character(ledger$reason_code[status == "failed"]),
    levels = sort(unique(as.character(ledger$reason_code[status == "failed"])))
  ))
  list(
    total = as.integer(nrow(ledger)),
    eligible = as.integer(sum(status %in% c("predicted", "failed"))),
    predicted = as.integer(sum(status == "predicted")),
    failed = as.integer(sum(status == "failed")),
    ineligible = as.integer(sum(status == "ineligible")),
    failure_reasons = as.list(as.integer(failure_reasons))
  )
}

run_clinical_prediction_comparison <- function(protocol, endpoint_bundle,
                                              clinical_score_request, split_plan,
                                              candidates) {
  tryCatch(
    triomics_validate_split_plan(split_plan, TRUE),
    error = function(error) triomics_c09_abort(
      "C_C09_SPLIT_INVALID", paste0("The C-02 SplitPlan failed validation: ", conditionMessage(error)),
      "split_plan"
    )
  )
  triomics_c09_validate_protocol(protocol, split_plan)
  triomics_c09_validate_c08(endpoint_bundle, clinical_score_request, split_plan)
  triomics_c09_reject_unqualified_mofa(endpoint_bundle, split_plan)
  triomics_c09_validate_candidate_inputs(protocol, candidates, split_plan)

  evaluations <- stats::setNames(vector("list", length(candidates)), names(candidates))
  for (candidate_name in names(candidates)) {
    candidate <- candidates[[candidate_name]]
    evaluations[[candidate_name]] <- evaluate_pipeline(
      candidate$study, candidate$analysis_spec, candidate$integration_spec, split_plan
    )
  }
  triomics_c09_validate_candidate_results(evaluations, split_plan, protocol)

  metric_values <- lapply(evaluations, triomics_c09_metric_values, protocol = protocol)
  comparisons <- list(
    combined_vs_clinical_only = "clinical_only",
    combined_vs_molecular_only = "molecular_only"
  )
  incremental <- lapply(comparisons, function(reference_name) {
    stats::setNames(lapply(protocol$response_model$metric_names, function(metric) {
      result <- triomics_c09_bootstrap_delta(
        metric, evaluations$combined, evaluations[[reference_name]], split_plan, protocol
      )
      result$reference_candidate <- reference_name
      result$combined_candidate <- "combined"
      result$delta_definition <- "combined_metric_minus_reference_metric"
      result$direction <- if (metric %in% c("rmse", "mae")) "negative_favors_combined" else "positive_favors_combined"
      result
    }), protocol$response_model$metric_names)
  })
  names(incremental) <- names(comparisons)
  candidate_summaries <- stats::setNames(lapply(names(evaluations), function(candidate_name) {
    evaluation <- evaluations[[candidate_name]]
    list(
      method = evaluation$method,
      metric_definitions = list(
        rmse = "Root mean squared error over successful held-out predictions.",
        mae = "Mean absolute error over successful held-out predictions.",
        r_squared = "Held-out R-squared against each fold's training-only intercept."
      ),
      metrics = metric_values[[candidate_name]],
      denominator = triomics_c09_candidate_denominator(evaluation)
    )
  }), names(evaluations))
  history <- stats::setNames(lapply(evaluations, triomics_c09_candidate_history), names(evaluations))

  structure(
    list(
      artifact_type = "clinical_prediction_comparison",
      protocol = protocol,
      split_plan_hash = split_plan$creation_hash,
      c08_source = list(
        endpoint_bundle_sha256 = endpoint_bundle$payload_sha256,
        request_sha256 = clinical_score_request$request_sha256,
        request_status = clinical_score_request$status,
        owner_stage = clinical_score_request$owner_stage,
        recurses_to_stage_c = clinical_score_request$recurses_to_stage_c
      ),
      candidates = candidate_summaries,
      incremental = incremental,
      tested_candidate_history = history,
      qualification = list(
        evidence_scope = "synthetic_software_only",
        scientific_validity = "not_assessed_by_synthetic_fixture",
        real_endpoint_protocol = "not_supplied",
        real_deployment = "not_qualified",
        independent_validation = "not_performed",
        benefit_guaranteed = FALSE,
        uncertainty_limitations = "Intervals are conditional on the fixed nested splits and do not establish external performance or remove training-set dependence."
      ),
      benefit_guaranteed = FALSE
    ),
    class = c("ClinicalPredictionComparison", "list")
  )
}
