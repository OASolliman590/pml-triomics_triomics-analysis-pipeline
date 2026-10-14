triomics_b09_variable <- function(clinical_view, variable_id) {
  variable <- triomics_b08_dictionary(clinical_view)[[variable_id]]
  if (is.null(variable)) return(list(ok = FALSE, reason = "predictor_unresolved"))
  if (variable$privacy %in% c("identifier", "free_text", "sensitive")) {
    return(list(ok = FALSE, reason = "privacy_excluded"))
  }
  if (isFALSE(variable$modelable)) return(list(ok = FALSE, reason = "not_modelable"))
  type <- triomics_b08_variable_type(variable)
  if (identical(type, "ordinal")) return(list(ok = FALSE, reason = "ordinal_not_numeric_distance"))
  if (!type %in% c("continuous", "count", "binary")) {
    return(list(ok = FALSE, reason = "unsupported_predictor_type"))
  }
  if (type %in% c("continuous", "count") &&
      (!identical(variable$units_verification, "verified") || is.null(variable$units) ||
       !nzchar(variable$units) || identical(variable$units, "inapplicable"))) {
    return(list(ok = FALSE, reason = "units_unverified"))
  }
  if (identical(type, "binary") && length(variable$codebook) != 2L) {
    return(list(ok = FALSE, reason = "unsupported_predictor_type"))
  }
  list(ok = TRUE, value = variable)
}

triomics_b09_observation_value <- function(clinical_view, variable, subject_key, visit_id) {
  static <- identical(variable$time_semantics, "subject_static")
  hits <- Filter(function(obs) {
    identical(obs$variable_id, variable$variable_id) &&
      identical(obs$subject_key, subject_key) &&
      (static || identical(obs$visit_id, visit_id))
  }, clinical_view$observations)
  if (!length(hits)) return(list(state = "missing", reason = "clinical_observation_absent"))
  if (length(hits) > 1L) return(list(state = "block", reason = "duplicate_clinical_observation"))
  triomics_b08_coded_value(hits[[1L]], variable)
}

triomics_b09_sample_subject <- function(sample, links) {
  key <- sample$subject_key
  if (is.null(key) || !is.character(key) || length(key) != 1L || !nzchar(key)) {
    key <- triomics_b08_subject_for_sample(links, sample$sample_id)
  }
  if (is.null(key) || !is.character(key) || length(key) != 1L || !nzchar(key)) return(NA_character_)
  key
}

triomics_b09_endpoint_rows <- function(endpoint, clinical_view) {
  samples <- endpoint$samples
  if (is.null(samples)) samples <- list()
  lapply(samples, function(sample) {
    value <- sample$value
    list(
      subject_key = triomics_b09_sample_subject(sample, clinical_view$links),
      visit_id = if (is.null(sample$visit_id)) "" else as.character(sample$visit_id),
      sample_id = if (is.null(sample$sample_id)) "" else as.character(sample$sample_id),
      time = if (is.null(sample$time)) NULL else as.character(sample$time),
      value = if (is.null(value) || length(value) != 1L || !is.numeric(value) || !is.finite(value)) {
        NA_real_
      } else {
        as.numeric(value)
      }
    )
  })
}

triomics_b09_sample_time <- function(sample, clinical_view, subject_key, visit_id) {
  time <- sample$time
  if (is.character(time) && length(time) == 1L && nzchar(time)) return(time)
  hits <- Filter(function(obs) {
    identical(obs$subject_key, subject_key) && identical(obs$visit_id, visit_id) &&
      is.character(obs$time) && length(obs$time) == 1L && nzchar(obs$time)
  }, clinical_view$observations)
  times <- unique(vapply(hits, function(obs) obs$time, character(1)))
  if (length(times) == 1L) times[[1L]] else NULL
}

triomics_b09_horizon <- function(start_time, end_time) {
  if (!is.character(start_time) || length(start_time) != 1L || !nzchar(start_time) ||
      !is.character(end_time) || length(end_time) != 1L || !nzchar(end_time)) {
    return(list(ok = FALSE, reason = "horizon_unverified", days = NULL))
  }
  start <- suppressWarnings(as.Date(start_time, format = "%Y-%m-%d"))
  end <- suppressWarnings(as.Date(end_time, format = "%Y-%m-%d"))
  if (is.na(start) || is.na(end) || format(start, "%Y-%m-%d") != start_time ||
      format(end, "%Y-%m-%d") != end_time) {
    return(list(ok = FALSE, reason = "horizon_unverified", days = NULL))
  }
  days <- as.integer(end - start)
  list(ok = days > 0L, reason = if (days > 0L) NULL else "horizon_not_forward", days = days)
}

triomics_b09_audit_shell <- function(objective) {
  mode <- objective$mode
  unit <- switch(mode,
    between_person = "subject",
    within_person = "subject_pair",
    baseline_to_later = "subject",
    effect_modification = "subject",
    "unresolved"
  )
  visits <- if (identical(mode, "within_person") || identical(mode, "baseline_to_later")) {
    c(objective$baseline_visit_ref, objective$later_visit_ref)
  } else if (is.null(objective$time_window_ref)) {
    character()
  } else {
    as.character(objective$time_window_ref)
  }
  list(
    objective_id = objective$objective_id,
    mode = mode,
    unit_map = list(
      input_record_unit = "sample_visit",
      endpoint_unit = "sample_visit",
      clinical_unit = "subject_visit",
      analysis_unit = unit,
      repeated_records_as_independent = FALSE
    ),
    time_window = list(
      visit_ids = visits,
      predictor_visit = if (identical(mode, "baseline_to_later")) objective$baseline_visit_ref else NULL,
      baseline_visit = objective$baseline_visit_ref,
      later_visit = objective$later_visit_ref,
      declared_horizon_days = objective$horizon_days,
      observed_horizon_days = numeric(),
      baseline_adjustment = objective$baseline_adjustment
    ),
    missingness = list(
      policy = objective$missingness_policy,
      n_candidate = 0L,
      n_included = 0L,
      n_excluded_clinical = 0L,
      n_excluded_endpoint = 0L,
      reasons = list()
    )
  )
}

triomics_b09_record_exclusion <- function(audit, source, reason = NULL) {
  if (identical(source, "clinical")) {
    audit$missingness$n_excluded_clinical <- audit$missingness$n_excluded_clinical + 1L
    if (is.null(reason)) reason <- "clinical_complete_case"
  } else {
    audit$missingness$n_excluded_endpoint <- audit$missingness$n_excluded_endpoint + 1L
    if (is.null(reason)) reason <- "endpoint_unobserved"
  }
  current <- audit$missingness$reasons[[reason]]
  if (is.null(current)) current <- 0L
  audit$missingness$reasons[[reason]] <- as.integer(current + 1L)
  audit
}

triomics_b09_adjustment_variables <- function(objective, clinical_view, mode) {
  adjustments <- objective$adjustments
  if (is.null(adjustments)) adjustments <- list()
  out <- list()
  for (adjustment in adjustments) {
    if (is.null(adjustment$variable) || is.null(adjustment$rationale) ||
        !is.character(adjustment$rationale) || !nzchar(adjustment$rationale)) {
      return(list(ok = FALSE, reason = "adjustment_undeclared"))
    }
    expected_form <- switch(mode,
      within_person = "change",
      baseline_to_later = "level",
      "level"
    )
    if (!identical(adjustment$form, expected_form)) {
      return(list(ok = FALSE, reason = "adjustment_form_unresolved"))
    }
    variable <- triomics_b09_variable(clinical_view, adjustment$variable)
    if (!isTRUE(variable$ok)) return(variable)
    if (identical(mode, "within_person") &&
        identical(variable$value$time_semantics, "subject_static")) {
      return(list(ok = FALSE, reason = "static_covariate_not_within_person_change"))
    }
    out[[adjustment$variable]] <- variable$value
  }
  list(ok = TRUE, values = out)
}

triomics_b09_adjustment_values <- function(clinical_view, variables, subject_key,
                                            visit_ids, mode) {
  values <- numeric()
  for (name in names(variables)) {
    variable <- variables[[name]]
    if (identical(mode, "within_person")) {
      before <- triomics_b09_observation_value(clinical_view, variable, subject_key, visit_ids[[1L]])
      after <- triomics_b09_observation_value(clinical_view, variable, subject_key, visit_ids[[2L]])
      if (identical(before$state, "block")) return(before)
      if (identical(after$state, "block")) return(after)
      if (!identical(before$state, "value") || !identical(after$state, "value")) {
        return(list(state = "missing", reason = "clinical_adjustment_missing"))
      }
      values[[paste0("adjustment::", name)]] <- after$value - before$value
    } else {
      visit <- if (identical(mode, "baseline_to_later")) visit_ids[[1L]] else visit_ids[[1L]]
      value <- triomics_b09_observation_value(clinical_view, variable, subject_key, visit)
      if (!identical(value$state, "value")) return(value)
      values[[paste0("adjustment::", name)]] <- value$value
    }
  }
  list(state = "value", values = values)
}

triomics_b09_single_sample <- function(rows, subject_key, visit_id) {
  hits <- Filter(function(row) identical(row$subject_key, subject_key) &&
                   identical(row$visit_id, visit_id), rows)
  if (!length(hits)) return(list(state = "missing"))
  if (length(hits) > 1L) return(list(state = "block", reason = "duplicate_endpoint_unit"))
  list(state = "value", row = hits[[1L]])
}

triomics_b09_collect_rows <- function(endpoint, clinical_view, objective, predictor,
                                       modifier = NULL, adjustments = list()) {
  mode <- objective$mode
  audit <- triomics_b09_audit_shell(objective)
  rows <- triomics_b09_endpoint_rows(endpoint, clinical_view)
  if (!length(rows)) return(list(ok = FALSE, reason = "endpoint_absent", rows = list(), audit = audit))
  if (any(vapply(rows, function(row) is.na(row$subject_key), logical(1)))) {
    return(list(ok = FALSE, reason = "subject_unaligned", rows = list(), audit = audit))
  }
  subjects <- unique(vapply(rows, function(row) row$subject_key, character(1)))
  audit$missingness$n_candidate <- as.integer(length(subjects))
  if (identical(mode, "between_person") || identical(mode, "effect_modification")) {
    visit <- objective$time_window_ref
    if (!is.character(visit) || length(visit) != 1L || !nzchar(visit)) {
      return(list(ok = FALSE, reason = "time_window_unresolved", rows = list(), audit = audit))
    }
    audit$time_window$visit_ids <- visit
    candidates <- subjects
    collected <- list()
    for (subject in candidates) {
      selected <- triomics_b09_single_sample(rows, subject, visit)
      if (identical(selected$state, "block")) {
        return(list(ok = FALSE, reason = selected$reason, rows = list(), audit = audit))
      }
      if (!identical(selected$state, "value")) {
        audit <- triomics_b09_record_exclusion(audit, "endpoint", "endpoint_visit_missing")
        next
      }
      if (!is.finite(selected$row$value)) {
        audit <- triomics_b09_record_exclusion(audit, "endpoint")
        next
      }
      x <- triomics_b09_observation_value(clinical_view, predictor, subject, visit)
      if (identical(x$state, "block")) return(list(ok = FALSE, reason = x$reason, rows = list(), audit = audit))
      if (!identical(x$state, "value")) {
        audit <- triomics_b09_record_exclusion(audit, "clinical")
        next
      }
      row <- list(response = selected$row$value, predictor = x$value, covariates = numeric(),
                  modifier = NULL, n_samples = 1L)
      adjustment <- triomics_b09_adjustment_values(clinical_view, adjustments, subject, visit, mode)
      if (!identical(adjustment$state, "value")) {
        if (identical(adjustment$state, "block")) return(list(ok = FALSE, reason = adjustment$reason,
                                                               rows = list(), audit = audit))
        audit <- triomics_b09_record_exclusion(audit, "clinical")
        next
      }
      row$covariates <- adjustment$values
      if (identical(mode, "effect_modification")) {
        m <- triomics_b09_observation_value(clinical_view, modifier, subject, visit)
        if (identical(m$state, "block")) return(list(ok = FALSE, reason = m$reason, rows = list(), audit = audit))
        if (!identical(m$state, "value")) {
          audit <- triomics_b09_record_exclusion(audit, "clinical")
          next
        }
        row$modifier <- m$value
      }
      collected[[length(collected) + 1L]] <- row
    }
    audit$missingness$n_included <- as.integer(length(collected))
    return(list(ok = TRUE, reason = NULL, rows = collected, audit = audit))
  }

  baseline_visit <- objective$baseline_visit_ref
  later_visit <- objective$later_visit_ref
  if (!is.character(baseline_visit) || length(baseline_visit) != 1L || !nzchar(baseline_visit) ||
      !is.character(later_visit) || length(later_visit) != 1L || !nzchar(later_visit) ||
      identical(baseline_visit, later_visit)) {
    return(list(ok = FALSE, reason = "time_window_unresolved", rows = list(), audit = audit))
  }
  audit$time_window$visit_ids <- c(baseline_visit, later_visit)
  collected <- list()
  horizons <- integer()
  horizon_failure <- NULL
  for (subject in subjects) {
    before <- triomics_b09_single_sample(rows, subject, baseline_visit)
    after <- triomics_b09_single_sample(rows, subject, later_visit)
    if (identical(before$state, "block")) return(list(ok = FALSE, reason = before$reason,
                                                        rows = list(), audit = audit))
    if (identical(after$state, "block")) return(list(ok = FALSE, reason = after$reason,
                                                      rows = list(), audit = audit))
    if (!identical(before$state, "value") || !identical(after$state, "value")) {
      audit <- triomics_b09_record_exclusion(audit, "endpoint")
      next
    }
    if (!is.finite(before$row$value) || !is.finite(after$row$value)) {
      audit <- triomics_b09_record_exclusion(audit, "endpoint")
      next
    }
    x_before <- triomics_b09_observation_value(clinical_view, predictor, subject, baseline_visit)
    if (identical(x_before$state, "block")) {
      return(list(ok = FALSE, reason = x_before$reason, rows = list(), audit = audit))
    }
    if (!identical(x_before$state, "value")) {
      audit <- triomics_b09_record_exclusion(audit, "clinical")
      next
    }
    adjustment_visit_ids <- if (identical(mode, "within_person")) {
      c(baseline_visit, later_visit)
    } else {
      baseline_visit
    }
    adjustment <- triomics_b09_adjustment_values(clinical_view, adjustments, subject,
                                                  adjustment_visit_ids, mode)
    if (!identical(adjustment$state, "value")) {
      if (identical(adjustment$state, "block")) return(list(ok = FALSE, reason = adjustment$reason,
                                                             rows = list(), audit = audit))
      audit <- triomics_b09_record_exclusion(audit, "clinical")
      next
    }
    if (identical(mode, "within_person")) {
      x_after <- triomics_b09_observation_value(clinical_view, predictor, subject, later_visit)
      if (identical(x_after$state, "block")) return(list(ok = FALSE, reason = x_after$reason,
                                                          rows = list(), audit = audit))
      if (!identical(x_after$state, "value")) {
        audit <- triomics_b09_record_exclusion(audit, "clinical")
        next
      }
      collected[[length(collected) + 1L]] <- list(
        response = after$row$value - before$row$value,
        predictor = x_after$value - x_before$value,
        covariates = adjustment$values,
        modifier = NULL, n_samples = 2L
      )
      next
    }
    if (identical(mode, "baseline_to_later")) {
      start_time <- x_before$time
      if (is.null(start_time) || !nzchar(start_time)) {
        start_time <- triomics_b09_sample_time(before$row, clinical_view, subject, baseline_visit)
      }
      end_time <- triomics_b09_sample_time(after$row, clinical_view, subject, later_visit)
      horizon <- triomics_b09_horizon(start_time, end_time)
      if (!isTRUE(horizon$ok)) {
        audit <- triomics_b09_record_exclusion(audit, "endpoint", horizon$reason)
        horizon_failure <- horizon$reason
        next
      }
      horizons <- c(horizons, horizon$days)
      if (!identical(as.numeric(horizon$days), as.numeric(objective$horizon_days))) {
        audit <- triomics_b09_record_exclusion(audit, "endpoint", "horizon_mismatch")
        horizon_failure <- "horizon_mismatch"
        next
      }
      response <- switch(objective$outcome_estimand,
        later_level = after$row$value,
        change = after$row$value - before$row$value,
        NULL
      )
      if (is.null(response)) return(list(ok = FALSE, reason = "outcome_estimand_unresolved",
                                         rows = list(), audit = audit))
      covariates <- adjustment$values
      if (isTRUE(objective$baseline_adjustment)) {
        covariates <- c(covariates, baseline_endpoint = before$row$value)
      }
      collected[[length(collected) + 1L]] <- list(
        response = response, predictor = x_before$value,
        covariates = covariates, modifier = NULL, n_samples = 2L
      )
    }
  }
  audit$time_window$observed_horizon_days <- sort(unique(horizons))
  audit$missingness$n_included <- as.integer(length(collected))
  if (identical(mode, "baseline_to_later") && !length(collected) &&
      !is.null(horizon_failure)) {
    return(list(ok = FALSE, reason = horizon_failure, rows = collected, audit = audit))
  }
  list(ok = TRUE, reason = NULL, rows = collected, audit = audit)
}

triomics_b09_result_shell <- function(objective, endpoint_id, response_type = NULL,
                                      status = "unavailable", reason = NULL) {
  list(
    objective_id = objective$objective_id,
    objective_version = if (is.null(objective$objective_version)) "1" else objective$objective_version,
    analysis_id = objective$analysis_id,
    endpoint_id = endpoint_id,
    predictor_ref = objective$predictor_ref,
    mode = objective$mode,
    estimand = NULL,
    response_type = response_type,
    predictor_type = NULL,
    effect = NULL,
    effect_unit = NULL,
    effect_scale = NULL,
    ordinary_se = NULL,
    moderated_se = NULL,
    interval_low = NULL,
    interval_high = NULL,
    interval_level = 0.95,
    interval_method = NULL,
    statistic = NULL,
    statistic_name = NULL,
    ordinary_df = NULL,
    effective_test_df = NULL,
    df_prior = NULL,
    p = NULL,
    q = NULL,
    inferential_status = "not_estimable",
    estimation_status = status,
    estimation_reason = reason,
    independent_validation_released = FALSE,
    hypothesis_status = objective$hypothesis_status,
    adjustment = objective$adjustments,
    n_subjects = 0L,
    n_samples = 0L,
    n_rows_used = 0L,
    n_excluded_clinical = 0L,
    n_excluded_endpoint = 0L,
    predictor_units = NULL,
    response_units = NULL,
    formula = NULL,
    tested_term = NULL,
    contrast_weights = NULL,
    objective_audit = triomics_b09_audit_shell(objective),
    selection = NULL
  )
}

triomics_b09_fit_objective <- function(endpoint, clinical_view, objective, robust) {
  response_type <- endpoint$response_type
  if (!identical(response_type, "continuous")) {
    return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                     reason = "unsupported_response_type"))
  }
  if (!identical(objective$missingness_policy, "complete_case")) {
    return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                     reason = "clinical_imputation_refused"))
  }
  mode <- objective$mode
  if (!mode %in% c("between_person", "within_person", "baseline_to_later", "effect_modification")) {
    return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                     reason = "mode_not_executable"))
  }
  predictor <- triomics_b09_variable(clinical_view, objective$predictor_ref)
  if (!isTRUE(predictor$ok)) {
    return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                     reason = predictor$reason))
  }
  if (identical(mode, "within_person") &&
      identical(predictor$value$time_semantics, "subject_static")) {
    return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                     reason = "static_predictor_not_within_person_change"))
  }
  if (identical(mode, "baseline_to_later")) {
    if (!is.numeric(objective$horizon_days) || length(objective$horizon_days) != 1L ||
        !is.finite(objective$horizon_days) || objective$horizon_days <= 0 ||
        objective$horizon_days != as.integer(objective$horizon_days)) {
      return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                       reason = "baseline_horizon_undeclared"))
    }
    if (!is.logical(objective$baseline_adjustment) || length(objective$baseline_adjustment) != 1L ||
        is.na(objective$baseline_adjustment)) {
      return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                       reason = "baseline_adjustment_choice_undeclared"))
    }
    if (!objective$outcome_estimand %in% c("later_level", "change")) {
      return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                       reason = "outcome_estimand_unresolved"))
    }
  }
  modifier <- NULL
  if (identical(mode, "effect_modification")) {
    modifier_result <- triomics_b09_variable(clinical_view, objective$modifier_ref)
    if (!isTRUE(modifier_result$ok)) {
      return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                       reason = if (is.null(objective$modifier_ref)) {
                                         "modifier_unresolved"
                                       } else modifier_result$reason))
    }
    modifier <- modifier_result$value
  }
  adjustment_vars <- triomics_b09_adjustment_variables(objective, clinical_view, mode)
  if (!isTRUE(adjustment_vars$ok)) {
    return(triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type,
                                     reason = adjustment_vars$reason))
  }
  collected <- triomics_b09_collect_rows(endpoint, clinical_view, objective, predictor$value,
                                         modifier, adjustment_vars$values)
  result <- triomics_b09_result_shell(objective, endpoint$endpoint_id, response_type)
  result$objective_audit <- collected$audit
  result$n_subjects <- as.integer(length(collected$rows))
  result$n_samples <- as.integer(sum(vapply(collected$rows, function(row) row$n_samples, integer(1))))
  result$n_rows_used <- result$n_subjects
  result$n_excluded_clinical <- collected$audit$missingness$n_excluded_clinical
  result$n_excluded_endpoint <- collected$audit$missingness$n_excluded_endpoint
  result$predictor_type <- triomics_b08_variable_type(predictor$value)
  result$predictor_units <- predictor$value$units
  result$response_units <- endpoint$units
  result$selection <- endpoint$selection
  if (!isTRUE(collected$ok)) {
    result$estimation_reason <- collected$reason
    return(result)
  }
  if (!length(collected$rows)) {
    result$estimation_status <- "failed"
    result$estimation_reason <- "no_complete_cases"
    return(result)
  }

  predictors <- vapply(collected$rows, function(row) row$predictor, numeric(1))
  responses <- vapply(collected$rows, function(row) row$response, numeric(1))
  if (length(unique(predictors)) < 2L) {
    result$estimation_reason <- "constant_predictor"
    return(result)
  }
  design <- cbind(intercept = rep(1, length(responses)))
  predictor_term <- switch(mode,
    between_person = "predictor",
    within_person = "delta_predictor",
    baseline_to_later = "baseline_predictor",
    effect_modification = "predictor"
  )
  design <- cbind(design, stats::setNames(data.frame(predictors), predictor_term))
  covariate_names <- names(collected$rows[[1L]]$covariates)
  if (length(covariate_names)) {
    covariate_matrix <- do.call(rbind, lapply(collected$rows, function(row) row$covariates[covariate_names]))
    colnames(covariate_matrix) <- covariate_names
    design <- cbind(design, covariate_matrix)
  }
  tested_term <- predictor_term
  if (identical(mode, "effect_modification")) {
    modifiers <- vapply(collected$rows, function(row) row$modifier, numeric(1))
    modifier_term <- "modifier"
    interaction_term <- "predictor_x_modifier"
    design <- cbind(design, stats::setNames(data.frame(modifiers), modifier_term),
                    stats::setNames(data.frame(predictors * modifiers), interaction_term))
    tested_term <- paste(objective$predictor_ref, objective$modifier_ref, sep = ":")
  }
  design <- as.matrix(design)
  storage.mode(design) <- "double"
  weights <- rep(0, ncol(design))
  effect_column <- if (identical(mode, "effect_modification")) "predictor_x_modifier" else predictor_term
  effect_index <- match(effect_column, colnames(design))
  weights[[effect_index]] <- 1
  support <- triomics_b_feature_support(responses, design, list(weights))
  result$ordinary_df <- as.numeric(support$df)
  result$formula <- paste(endpoint$endpoint_id, "~", paste(colnames(design), collapse = " + "))
  result$tested_term <- tested_term
  result$contrast_weights <- stats::setNames(as.list(weights), colnames(design))
  result$effect_scale <- if (identical(predictor$value$declared_type, "binary")) {
    "level_difference"
  } else if (identical(mode, "within_person")) {
    "per_predictor_change_unit"
  } else {
    "per_predictor_unit"
  }
  result$effect_unit <- if (identical(predictor$value$declared_type, "binary")) {
    endpoint$units
  } else {
    paste(endpoint$units, "per", predictor$value$units)
  }
  result$estimand <- switch(mode,
    between_person = "between_person_coefficient",
    within_person = "within_person_change_association",
    baseline_to_later = paste0("baseline_to_later_", objective$outcome_estimand),
    effect_modification = "predictor_modifier_interaction_coefficient"
  )
  if (qr(design)$rank < ncol(design)) {
    result$estimation_reason <- "rank_deficient"
    return(result)
  }
  if (!isTRUE(support$eligible)) {
    result$estimation_reason <- if (support$df < 1) "zero_residual_support" else "contrast_not_estimable"
    if (isTRUE(support$estimable[[1L]]) && identical(result$estimation_reason, "zero_residual_support")) {
      result$effect <- as.numeric(support$effects[[1L]])
    }
    return(result)
  }
  if (!requireNamespace("limma", quietly = TRUE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma is not installed.", key = "statistics.method",
                     remediation = "Install limma from Bioconductor before fitting.")
  }
  contrast <- matrix(weights, ncol = 1L, dimnames = list(colnames(design), "objective"))
  moderated <- limma::eBayes(
    limma::contrasts.fit(limma::lmFit(matrix(responses, nrow = 1L), design), contrast),
    trend = FALSE, robust = robust
  )
  effect <- as.numeric(moderated$coefficients)[[1L]]
  unscaled <- as.numeric(moderated$stdev.unscaled)[[1L]]
  sigma <- as.numeric(moderated$sigma)[[1L]]
  if (!triomics_b_close(support$effects[[1L]], effect) ||
      !triomics_b_close(support$ordinary_se[[1L]], sigma * unscaled)) {
    triomics_b_abort("B_FIT_DIVERGENCE", "Ordinary association results diverged from limma.",
                     key = "objective_id")
  }
  df_total <- as.numeric(moderated$df.total)[[1L]]
  result$effect <- effect
  result$ordinary_se <- as.numeric(support$ordinary_se[[1L]])
  result$moderated_se <- sqrt(as.numeric(moderated$s2.post)[[1L]]) * unscaled
  result$statistic <- as.numeric(moderated$t)[[1L]]
  result$statistic_name <- "moderated_t"
  result$effective_test_df <- df_total
  result$df_prior <- as.numeric(moderated$df.prior)[[1L]]
  result$p <- as.numeric(moderated$p.value)[[1L]]
  result$estimation_status <- "estimated"
  result$estimation_reason <- NULL
  result$inferential_status <- if (identical(objective$hypothesis_status, "exploratory")) {
    "exploratory"
  } else {
    "unverified"
  }
  if (is.finite(df_total) && df_total >= 1) {
    critical <- stats::qt(0.975, df_total)
    result$interval_low <- result$effect - critical * result$moderated_se
    result$interval_high <- result$effect + critical * result$moderated_se
    result$interval_method <- "moderated_t"
  }
  result
}

triomics_b09_leak <- function(row, endpoint) {
  selection <- endpoint$selection
  ancestry <- if (is.null(selection)) NULL else selection$ancestry
  inspected <- if (is.null(selection)) NULL else selection$inspected_outcome
  !is.null(endpoint$training_outcome) || identical(ancestry, "trained_on_outcome") ||
    identical(ancestry, "selected_after_clinical_outcome") || !is.null(inspected) ||
    identical(row$inferential_status, "descriptive_only")
}

triomics_associate_longitudinal_endpoints <- function(endpoint_bundle, clinical_view, objective_spec) {
  if (!is.list(endpoint_bundle) || !is.list(clinical_view) || !is.list(objective_spec)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Association inputs must be typed lists.",
                     key = "endpoint_bundle")
  }
  if (!is.null(endpoint_bundle$study_id) && !is.null(clinical_view$study_id) &&
      !identical(endpoint_bundle$study_id, clinical_view$study_id)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The endpoint bundle and clinical view name different studies.",
                     key = "study_id")
  }
  triomics_b08_check_hashes(endpoint_bundle)
  spec <- triomics_b08_normalize_spec(objective_spec)
  if (!identical(spec$trend, FALSE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Clinical association keeps limma trend false.", key = "trend")
  }
  if (!is.logical(spec$robust) || length(spec$robust) != 1L || is.na(spec$robust)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma robust must be true or false.", key = "robust")
  }
  objectives <- spec$objectives
  ids <- vapply(objectives, function(objective) as.character(objective$objective_id), character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Objective ids must be non-blank and unique.",
                     key = "objective_id")
  }
  endpoint_ids <- vapply(objectives, function(objective) as.character(objective$endpoint_ref), character(1))
  if (any(!nzchar(endpoint_ids)) || anyDuplicated(endpoint_ids)) {
    triomics_b_abort("B_FAMILY_UNDECLARED", "One longitudinal objective per endpoint is allowed in a family.",
                     key = "endpoint_ref")
  }
  family <- spec$family
  family_declared <- is.list(family) && !is.null(family$member_ids) && length(family$member_ids) > 0L
  member_ids <- if (family_declared) as.character(unlist(family$member_ids, use.names = FALSE)) else endpoint_ids
  if (family_declared && (anyDuplicated(member_ids) || any(!endpoint_ids %in% member_ids))) {
    triomics_b_abort("B_FAMILY_UNDECLARED", "Declare each longitudinal objective exactly once in its family.",
                     key = "family_ref")
  }
  endpoint_index <- triomics_b08_index_endpoints(endpoint_bundle)
  objective_index <- stats::setNames(objectives, endpoint_ids)
  results <- vector("list", length(member_ids))
  estimated <- rep(FALSE, length(member_ids))
  leak <- FALSE
  qualified <- TRUE
  endpoints <- vector("list", length(member_ids))
  for (i in seq_along(member_ids)) {
    objective <- objective_index[[member_ids[[i]]]]
    endpoint <- if (is.null(objective)) NULL else endpoint_index[[objective$endpoint_ref]]
    endpoints[[i]] <- endpoint
    if (is.null(objective) || is.null(endpoint)) {
      placeholder <- if (is.null(objective)) list(objective_id = member_ids[[i]], mode = "unresolved") else objective
      results[[i]] <- triomics_b09_result_shell(placeholder,
        if (is.null(objective)) member_ids[[i]] else objective$endpoint_ref,
        status = "failed", reason = "endpoint_absent")
      next
    }
    if (!family_declared) {
      results[[i]] <- triomics_b09_result_shell(objective, endpoint$endpoint_id,
                                               reason = "family_undeclared")
      next
    }
    if (!is.null(spec$imputation) && !identical(spec$imputation, "none")) {
      results[[i]] <- triomics_b09_result_shell(objective, endpoint$endpoint_id,
                                               reason = "clinical_imputation_refused")
      next
    }
    results[[i]] <- triomics_b09_fit_objective(endpoint, clinical_view, objective, spec$robust)
    estimated[[i]] <- identical(results[[i]]$estimation_status, "estimated")
    leak <- leak || triomics_b09_leak(results[[i]], endpoint)
    validity <- if (is.null(endpoint$selection)) NULL else endpoint$selection$validity
    if (isTRUE(estimated[[i]]) && !identical(validity, "qualified")) qualified <- FALSE
  }
  internal_p <- rep(1, length(member_ids))
  for (i in seq_along(member_ids)) {
    if (isTRUE(estimated[[i]]) && is.finite(results[[i]]$p)) internal_p[[i]] <- results[[i]]$p
  }
  internal_q <- if (length(internal_p)) unname(stats::p.adjust(internal_p, method = "BH")) else numeric()
  q_published <- family_declared && any(estimated) && !leak && qualified
  for (i in seq_along(results)) {
    if (isTRUE(estimated[[i]]) && q_published) {
      results[[i]]$q <- internal_q[[i]]
      if (identical(results[[i]]$hypothesis_status, "independent_validation")) {
        results[[i]]$inferential_status <- "independent_validation"
        results[[i]]$independent_validation_released <- TRUE
      }
    } else {
      results[[i]]$q <- NULL
      if (isTRUE(estimated[[i]]) && leak) {
        results[[i]]$inferential_status <- "descriptive_only"
        results[[i]]$estimation_reason <- "selection_after_outcome"
        results[[i]]$independent_validation_released <- FALSE
      }
    }
  }
  family_out <- if (!family_declared) NULL else list(
    family_id = family$family_id,
    claim_scope = family$claim_scope,
    member_ids = member_ids,
    member_hash = triomics_b_hash(member_ids),
    n_declared = as.integer(length(member_ids)),
    n_estimated = as.integer(sum(estimated)),
    n_failed = as.integer(length(member_ids) - sum(estimated)),
    correction_method = "BH",
    q_published = q_published,
    q_withheld_reason = if (!q_published && leak) "selection_or_training_lineage" else
      if (!q_published && !qualified) "unqualified_selection" else NULL,
    internal_p = as.numeric(internal_p),
    internal_q = as.numeric(internal_q)
  )
  list(
    status = if (length(member_ids) && all(estimated)) "complete" else if (any(estimated)) "partial" else "unavailable",
    method = if (any(estimated)) "limma" else "none",
    fallback_applied = FALSE,
    requested_fallback = spec$fallback,
    limma_version = if (any(estimated)) as.character(utils::packageVersion("limma")) else NULL,
    study_id = clinical_view$study_id,
    analysis_id = endpoint_bundle$analysis_id,
    objectives = lapply(results, function(row) row$objective_audit),
    family = family_out,
    results = results,
    imputation = "none",
    assay_imputation_reused = FALSE
  )
}

associate_longitudinal_endpoints <- triomics_associate_longitudinal_endpoints

triomics_b09_write_fixture_tables <- function(config, config_path) {
  triomics_b08_write_fixture_tables(config, config_path)
  invisible(TRUE)
}

triomics_b09_fixture_bundle <- function(clinical_view) {
  frame <- triomics_b08_fixture_frame()
  observations <- clinical_view$observations
  find_observation <- function(sample_id, variable_id) {
    hits <- Filter(function(obs) identical(obs$sample_id, sample_id) &&
                     identical(obs$variable_id, variable_id), observations)
    if (length(hits)) hits[[1L]] else NULL
  }
  endpoint_samples <- lapply(seq_len(frame$n), function(i) {
    sample_id <- sprintf("SYN_%02d", i)
    observation <- find_observation(sample_id, "glucose")
    x <- frame$glucose[[i]]
    value <- if (is.na(x)) NA_real_ else 2 * x + c(-0.3, 0.2, 0.1, -0.2, 0.3, -0.1)[[(i - 1L) %% 6L + 1L]]
    list(
      sample_id = sample_id,
      subject_key = if (is.null(observation)) NA_character_ else observation$subject_key,
      visit_id = "V1",
      time = if (is.null(observation)) NULL else observation$time,
      value = value
    )
  })
  list(
    study_id = clinical_view$study_id,
    analysis_id = "synthetic_primary",
    endpoints = list(list(
      endpoint_id = "synthetic_longitudinal_score",
      endpoint_class = "score",
      response_type = "continuous",
      units = "score_unit",
      samples = endpoint_samples,
      training_outcome = NULL,
      selection = list(ancestry = "prespecified", validity = "qualified", inspected_outcome = NULL)
    ))
  )
}

triomics_b09_cli_spec <- function(config) {
  objectives <- lapply(config$clinical$objectives, function(objective) {
    objective$response_ref <- "synthetic_longitudinal_score"
    objective$endpoint_ref <- "synthetic_longitudinal_score"
    objective$predictor_ref <- "glucose"
    objective$mode <- "between_person"
    objective$time_window_ref <- "V1"
    objective$objective_version <- "1"
    objective
  })
  member_ids <- vapply(objectives, function(objective) objective$endpoint_ref, character(1))
  list(
    objectives = objectives,
    family = list(
      family_id = objectives[[1L]]$family_ref,
      member_ids = member_ids,
      claim_scope = "synthetic_between_person_score_association"
    ),
    trend = FALSE, robust = FALSE, fallback = "none", imputation = "none"
  )
}

triomics_b09_execute_longitudinal_associations <- function(config_path, output) {
  config <- triomics_read_config(config_path)
  if (!isTRUE(config$clinical$enabled)) return(list(status = "not_requested"))
  if (identical(config$study_id, "triomics_b_t09_fixture")) {
    triomics_b09_write_fixture_tables(config, config_path)
  }
  ingested <- triomics_ingest_configured_study(config, config_path)
  clinical <- triomics_ingest_clinical(config, config_path, ingested$sample_registry)
  bundle <- triomics_b09_fixture_bundle(clinical)
  fit <- triomics_associate_longitudinal_endpoints(bundle, clinical, triomics_b09_cli_spec(config))
  exported <- triomics_clinical_association_export(fit)
  payload <- file.path(output, "B", "clinical-longitudinal-associations.json")
  triomics_write_json_atomic(exported, payload)
  envelope <- triomics_artifact_envelope(
    artifact_type = "clinical_association_results",
    artifact_id = "b-t09-longitudinal-clinical-associations",
    study_id = config$study_id,
    run_id = "b-t09",
    status = fit$status,
    config_sha256 = attr(config, "config_sha256"),
    payload = payload,
    producer_commit = "not-recorded",
    inputs = list(
      list(artifact_id = "clinical-dictionary-source", sha256 = clinical$dictionary_sha256),
      list(artifact_id = "clinical-observations-source", sha256 = clinical$observations_sha256),
      list(artifact_id = "clinical-links-source", sha256 = clinical$links_sha256)
    ),
    reason_codes = if (identical(fit$status, "complete")) character() else "partial_family",
    privacy_class = "synthetic"
  )
  envelope_path <- file.path(output, "B", "clinical-longitudinal-associations.envelope.json")
  triomics_write_envelope(envelope, envelope_path)
  primary <- fit$results[[1L]]
  ac1 <- identical(fit$status, "complete") && identical(primary$estimation_status, "estimated") &&
    identical(primary$n_subjects, 11L) && identical(primary$n_samples, 11L) &&
    is.finite(primary$effect) && abs(primary$effect - 2) < 0.1 &&
    identical(primary$objective_audit$unit_map$repeated_records_as_independent, FALSE)
  record <- list(
    packet_id = "B-T09",
    study_id = config$study_id,
    command = "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T09.yaml --stage B --output artifacts/acceptance/B-T09",
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      limma_version = fit$limma_version,
      platform = R.version$platform
    ),
    status = if (ac1) "software_fixture_pass" else "failed",
    acceptance = list(
      list(id = "B-T09-AC1", verdict = if (ac1) "pass" else "fail"),
      list(id = "B-T09-AC2", verdict = "covered_by_testthat"),
      list(id = "B-T09-AC3", verdict = "covered_by_testthat"),
      list(id = "B-T09-AC4", verdict = "covered_by_testthat")
    ),
    effect = primary$effect,
    n_subjects = primary$n_subjects,
    n_samples = primary$n_samples,
    payload_sha256 = triomics_sha256(payload),
    envelope_sha256 = triomics_sha256(envelope_path)
  )
  triomics_write_json_atomic(record, file.path(output, "acceptance-record.json"))
  if (!ac1) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The synthetic longitudinal association did not meet its oracle.",
                     key = "clinical.objectives")
  }
  invisible(fit)
}

triomics_b09_prepare_cli <- function(args) {
  options <- triomics_parse_cli(args)
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) || isTRUE(options$help) ||
      is.null(options$config) || !file.exists(options$config)) return(invisible(FALSE))
  config <- triomics_read_config(options$config)
  if (!identical(config$study_id, "triomics_b_t09_fixture") || !isTRUE(config$clinical$enabled)) {
    return(invisible(FALSE))
  }
  triomics_b09_write_fixture_tables(config, options$config)
  invisible(TRUE)
}

triomics_b09_cli_after_stage_b <- function(args) {
  options <- triomics_parse_cli(args)
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) || is.null(options$config) ||
      is.null(options$output)) return(invisible(FALSE))
  config <- triomics_read_config(options$config)
  if (!identical(config$study_id, "triomics_b_t09_fixture") || !isTRUE(config$clinical$enabled)) {
    return(invisible(FALSE))
  }
  triomics_b09_execute_longitudinal_associations(options$config, options$output)
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_b09_current_main <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  if (!isTRUE(attr(triomics_b09_current_main, "b_t09_hook"))) {
    triomics_b09_main_before <- triomics_b09_current_main
    triomics_b09_hooked_main <- function(args) {
      prepared <- tryCatch(triomics_b09_prepare_cli(args), triomics_error = function(error) error,
                           error = function(error) error)
      if (inherits(prepared, "triomics_error")) {
        cat(paste0("triomics ", prepared$code, ": ", conditionMessage(prepared), "\n"), file = stderr())
        return(triomics_cli_exit(prepared))
      }
      if (inherits(prepared, "error")) {
        cat(paste0("triomics A_INTERNAL: ", conditionMessage(prepared), "\n"), file = stderr())
        return(1L)
      }
      status <- triomics_b09_main_before(args)
      if (length(status) != 1L || is.na(status) || !identical(as.integer(status), 0L)) return(status)
      ran <- tryCatch(triomics_b09_cli_after_stage_b(args), triomics_error = function(error) error,
                      error = function(error) error)
      if (inherits(ran, "triomics_error")) {
        cat(paste0("triomics ", ran$code, ": ", conditionMessage(ran), "\n"), file = stderr())
        return(triomics_cli_exit(ran))
      }
      if (inherits(ran, "error")) {
        cat(paste0("triomics A_INTERNAL: ", conditionMessage(ran), "\n"), file = stderr())
        return(1L)
      }
      status
    }
    attr(triomics_b09_hooked_main, "b_t09_hook") <- TRUE
    assign("triomics_main", triomics_b09_hooked_main, envir = .GlobalEnv)
  }
}
