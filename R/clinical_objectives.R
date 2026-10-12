triomics_b08_deny_keys <- function() {
  c("subject_id", "mrn", "patient_id", "patient_name", "direct_identifier",
    "free_text", "text_value", "note", "raw_identifier", "sample_id",
    "identifier", "secret", "private")
}

triomics_b08_scrub <- function(value) {
  if (is.null(value) || is.atomic(value)) return(value)
  if (!is.list(value)) return(value)
  deny <- triomics_b08_deny_keys()
  names_value <- names(value)
  if (is.null(names_value)) {
    return(lapply(value, triomics_b08_scrub))
  }
  out <- list()
  for (i in seq_along(value)) {
    name <- names_value[[i]]
    if (!is.na(name) && name %in% deny) next
    out[[name]] <- triomics_b08_scrub(value[[i]])
  }
  out
}

triomics_clinical_association_export <- function(result) {
  triomics_b08_scrub(result)
}

triomics_b08_dictionary <- function(clinical_view) {
  dictionary <- clinical_view$dictionary
  out <- list()
  if (is.null(dictionary)) return(out)
  for (variable in dictionary) {
    if (is.null(variable$variable_id) || !nzchar(variable$variable_id)) next
    out[[variable$variable_id]] <- variable
  }
  out
}

triomics_b08_variable_type <- function(variable) {
  type <- variable$declared_type
  if (is.null(type) || !nzchar(type)) type <- variable$type
  type
}

triomics_b08_normalize_spec <- function(objective_spec) {
  spec <- objective_spec
  if (is.null(spec)) spec <- list()
  if (!is.null(spec$objective_id)) {
    family <- spec$family
    spec$family <- NULL
    spec <- list(objectives = list(spec), family = family)
  }
  if (is.null(spec$objectives)) spec <- c(spec, list(objectives = list()))
  if (is.null(spec$trend)) spec$trend <- FALSE
  if (is.null(spec$robust)) spec$robust <- FALSE
  if (is.null(spec$fallback) || !nzchar(spec$fallback)) spec$fallback <- "none"
  spec
}

triomics_b08_objective_block <- function(objective, dictionary) {
  mode <- objective$mode
  known <- c("between_person", "within_person", "baseline_to_later", "effect_modification", "prediction")
  if (!is.character(mode) || length(mode) != 1L || !mode %in% known) {
    return("mode_not_executable")
  }
  if (!identical(mode, "between_person")) return("mode_not_executable")
  if (!identical(objective$response_ref, objective$endpoint_ref)) return("response_endpoint_mismatch")
  policy <- objective$missingness_policy
  if (identical(policy, "qualified_method")) return("clinical_imputation_refused")
  if (!identical(policy, "complete_case")) return("missingness_policy_unresolved")
  predictor <- dictionary[[objective$predictor_ref]]
  if (is.null(predictor)) return("predictor_unresolved")
  if (predictor$privacy %in% c("identifier", "free_text", "sensitive")) return("privacy_excluded")
  if (isFALSE(predictor$modelable)) return("not_modelable")
  if (!identical(predictor$verification, "verified") && !isTRUE(predictor$modelable)) return("predictor_unverified")
  type <- triomics_b08_variable_type(predictor)
  if (identical(type, "ordinal")) return("ordinal_not_numeric_distance")
  if (!type %in% c("continuous", "count", "binary")) return("unsupported_predictor_type")
  if (type %in% c("continuous", "count")) {
    if (!identical(predictor$units_verification, "verified") || !nzchar(predictor$units) ||
        identical(predictor$units, "inapplicable")) {
      return("units_unverified")
    }
  }
  if (identical(type, "binary") && length(predictor$codebook) != 2L) return("unsupported_predictor_type")
  adjustments <- objective$adjustments
  if (is.null(adjustments)) adjustments <- list()
  for (adjustment in adjustments) {
    if (is.null(adjustment$variable) || is.null(adjustment$rationale) || !nzchar(adjustment$rationale)) {
      return("adjustment_undeclared")
    }
    if (!identical(adjustment$form, "level")) return("adjustment_form_not_between_person")
    covariate <- dictionary[[adjustment$variable]]
    if (is.null(covariate)) return("adjustment_unresolved")
    if (covariate$privacy %in% c("identifier", "free_text", "sensitive")) return("privacy_excluded")
    covariate_type <- triomics_b08_variable_type(covariate)
    if (identical(covariate_type, "ordinal")) return("ordinal_not_numeric_distance")
    if (!covariate_type %in% c("continuous", "count", "binary")) return("unsupported_predictor_type")
  }
  NULL
}

triomics_resolve_clinical_objectives <- function(clinical_view, objective_spec) {
  spec <- triomics_b08_normalize_spec(objective_spec)
  dictionary <- triomics_b08_dictionary(clinical_view)
  family <- spec$family
  lapply(spec$objectives, function(objective) {
    reason <- triomics_b08_objective_block(objective, dictionary)
    list(
      objective_id = objective$objective_id,
      objective_version = if (is.null(objective$objective_version)) "1" else objective$objective_version,
      question = if (is.null(objective$question)) {
        "Declared between-person association of a continuous endpoint with a clinical predictor."
      } else {
        objective$question
      },
      mode = objective$mode,
      response_ref = objective$response_ref,
      predictor_ref = objective$predictor_ref,
      endpoint_ref = objective$endpoint_ref,
      analysis_id = objective$analysis_id,
      population = list(visit_id = objective$time_window_ref, missingness_policy = objective$missingness_policy),
      time_window_ref = objective$time_window_ref,
      adjustments = objective$adjustments,
      missingness_policy = objective$missingness_policy,
      hypothesis_status = objective$hypothesis_status,
      family_ref = objective$family_ref,
      family_id = if (is.null(family)) NULL else family$family_id,
      protocol_ref = objective$protocol_ref,
      status = if (is.null(reason)) "eligible" else "unavailable",
      reason = reason,
      fitted = FALSE,
      imputation = "none",
      assay_imputation_reused = FALSE
    )
  })
}

triomics_b08_blank_result <- function(objective, endpoint_id, status, reason, response_type = NULL) {
  list(
    objective_id = if (is.null(objective)) NULL else objective$objective_id,
    objective_version = if (is.null(objective) || is.null(objective$objective_version)) "1" else objective$objective_version,
    analysis_id = if (is.null(objective)) NULL else objective$analysis_id,
    endpoint_id = endpoint_id,
    predictor_ref = if (is.null(objective)) NULL else objective$predictor_ref,
    response_role = "endpoint",
    predictor_role = "clinical",
    mode = if (is.null(objective)) NULL else objective$mode,
    estimand = "adjusted_between_person_coefficient",
    response_type = response_type,
    predictor_type = NULL,
    effect = NULL,
    effect_unit = NULL,
    effect_scale = NULL,
    predictor_units = NULL,
    response_units = NULL,
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
    correlation = NULL,
    inferential_status = "not_estimable",
    estimation_status = status,
    estimation_reason = reason,
    independent_validation_released = FALSE,
    hypothesis_status = if (is.null(objective)) NULL else objective$hypothesis_status,
    adjustment = if (is.null(objective)) list() else objective$adjustments,
    n_subjects = 0L,
    n_samples = 0L,
    n_rows_used = 0L,
    n_pairs = NULL,
    n_excluded_clinical = 0L,
    n_excluded_endpoint = 0L,
    exclusions = list(),
    missingness_policy = if (is.null(objective)) NULL else objective$missingness_policy,
    imputation = "none",
    assay_imputation_reused = FALSE,
    formula = NULL,
    contrast_weights = NULL,
    selection = NULL
  )
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_b08_current_main <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  if (!isTRUE(attr(triomics_b08_current_main, "b_t08_hook"))) {
    triomics_main_before_b08 <- triomics_b08_current_main
    assign("triomics_main_before_b08", triomics_main_before_b08, envir = .GlobalEnv)
    triomics_b08_hooked_main <- function(args) {
      prepared <- tryCatch(triomics_b08_prepare_cli(args), triomics_error = function(error) error,
                           error = function(error) error)
      if (inherits(prepared, "triomics_error")) {
        cat(paste0("triomics ", prepared$code, ": ", conditionMessage(prepared), "\n"), file = stderr())
        return(triomics_cli_exit(prepared))
      }
      if (inherits(prepared, "error")) {
        cat(paste0("triomics A_INTERNAL: ", conditionMessage(prepared), "\n"), file = stderr())
        return(1L)
      }
      status <- triomics_main_before_b08(args)
      if (length(status) != 1L || is.na(status) || !identical(as.integer(status), 0L)) return(status)
      ran <- tryCatch(triomics_b08_cli_after_stage_b(args), triomics_error = function(error) error,
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
    attr(triomics_b08_hooked_main, "b_t08_hook") <- TRUE
    assign("triomics_main", triomics_b08_hooked_main, envir = .GlobalEnv)
  }
}
