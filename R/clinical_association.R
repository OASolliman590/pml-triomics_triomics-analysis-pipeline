triomics_b08_endpoint_hash <- function(endpoint) {
  samples <- endpoint$samples
  if (is.null(samples)) samples <- list()
  rows <- lapply(samples, function(sample) {
    value <- sample$value
    list(
      sample_id = if (is.null(sample$sample_id)) "" else as.character(sample$sample_id),
      value = if (is.null(value) || length(value) != 1L || !is.finite(value)) NULL else as.numeric(value)
    )
  })
  if (length(rows)) {
    order_ids <- vapply(rows, function(row) row$sample_id, character(1))
    rows <- rows[order(order_ids)]
  }
  triomics_b_hash(list(endpoint_id = endpoint$endpoint_id, rows = rows))
}

triomics_b08_check_hashes <- function(endpoint_bundle) {
  endpoints <- endpoint_bundle$endpoints
  if (is.null(endpoints)) return(invisible(TRUE))
  for (endpoint in endpoints) {
    declared <- endpoint$matrix_hash
    if (is.null(declared)) next
    if (!identical(declared, triomics_b08_endpoint_hash(endpoint))) {
      triomics_b_abort(
        "B_DESIGN_HASH",
        "The endpoint matrix hash does not match the declared values.",
        key = "matrix_hash",
        remediation = "Pass the endpoint artifact unchanged. Do not refit it from a stale hash."
      )
    }
  }
  invisible(TRUE)
}

triomics_b08_index_endpoints <- function(endpoint_bundle) {
  out <- list()
  endpoints <- endpoint_bundle$endpoints
  if (is.null(endpoints)) return(out)
  for (endpoint in endpoints) {
    if (is.null(endpoint$endpoint_id) || !nzchar(endpoint$endpoint_id)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "Each endpoint needs an endpoint id.", key = "endpoint_id")
    }
    if (!is.null(out[[endpoint$endpoint_id]])) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "Endpoint ids must be unique.", key = "endpoint_id")
    }
    out[[endpoint$endpoint_id]] <- endpoint
  }
  out
}

triomics_b08_subject_for_sample <- function(links, sample_id) {
  if (is.null(links) || is.null(sample_id)) return(NULL)
  for (link in links) {
    if (identical(link$status, "approved") && identical(link$sample_id, sample_id) &&
        !is.null(link$subject_key) && nzchar(link$subject_key)) {
      return(link$subject_key)
    }
  }
  NULL
}

triomics_b08_population <- function(endpoint, objective, links) {
  window <- objective$time_window_ref
  if (is.character(window) && length(window) == 1L && !nzchar(window)) window <- NULL
  rows <- list()
  samples <- endpoint$samples
  if (is.null(samples)) samples <- list()
  for (sample in samples) {
    visit <- sample$visit_id
    if (!is.null(window) && !identical(visit, window)) next
    subject_key <- sample$subject_key
    if (is.null(subject_key) || !nzchar(subject_key)) {
      subject_key <- triomics_b08_subject_for_sample(links, sample$sample_id)
    }
    value <- sample$value
    rows[[length(rows) + 1L]] <- list(
      subject_key = if (is.null(subject_key)) "" else as.character(subject_key),
      visit_id = visit,
      value = if (is.null(value) || length(value) != 1L || !is.finite(value)) NA_real_ else as.numeric(value)
    )
  }
  if (!length(rows)) return(list(ok = FALSE, reason = "endpoint_absent", rows = rows))
  keys <- vapply(rows, function(row) row$subject_key, character(1))
  if (any(!nzchar(keys))) return(list(ok = FALSE, reason = "subject_unaligned", rows = rows))
  if (any(duplicated(keys))) return(list(ok = FALSE, reason = "repeated_visits_not_independent", rows = rows))
  list(ok = TRUE, reason = NULL, rows = rows)
}

triomics_b08_observations_for <- function(observations, variable, subject_key, visit) {
  static <- identical(variable$time_semantics, "subject_static")
  hits <- list()
  if (is.null(observations)) return(hits)
  for (obs in observations) {
    if (!identical(obs$variable_id, variable$variable_id)) next
    if (!identical(obs$subject_key, subject_key)) next
    if (!static && !is.null(visit) && !identical(obs$visit_id, visit)) next
    hits[[length(hits) + 1L]] <- obs
  }
  hits
}

triomics_b08_coded_value <- function(obs, variable) {
  type <- triomics_b08_variable_type(variable)
  if (identical(obs$value_kind, "censored")) return(list(state = "block", reason = "censored_values_retained"))
  if (identical(type, "ordinal")) return(list(state = "block", reason = "ordinal_not_numeric_distance"))
  if (!type %in% c("continuous", "count", "binary")) {
    return(list(state = "block", reason = "unsupported_predictor_type"))
  }
  if (obs$value_kind %in% c("missing", "placeholder", "unavailable")) return(list(state = "missing"))
  if (identical(type, "binary")) {
    if (!identical(obs$value_kind, "code")) return(list(state = "block", reason = "unknown_coding"))
    code <- obs$code_value
    levels <- names(variable$codebook)
    if (is.null(code) || !nzchar(code) || !code %in% levels) {
      return(list(state = "block", reason = "unknown_coding"))
    }
    return(list(state = "value", coding = "factor", reference = levels[[1L]], target = levels[[2L]],
                value = if (identical(code, levels[[2L]])) 1 else 0))
  }
  if (!identical(obs$value_kind, "numeric") || length(obs$numeric_value) != 1L || !is.finite(obs$numeric_value)) {
    return(list(state = "block", reason = "unknown_coding"))
  }
  list(state = "value", coding = "numeric", value = as.numeric(obs$numeric_value))
}

triomics_b08_column_value <- function(observations, variable, subject_key, visit) {
  hits <- triomics_b08_observations_for(observations, variable, subject_key, visit)
  if (length(hits) > 1L) return(list(state = "block", reason = "repeated_visits_not_independent"))
  if (length(hits) == 0L) return(list(state = "missing"))
  triomics_b08_coded_value(hits[[1L]], variable)
}

triomics_b08_fit_endpoint <- function(endpoint, objective, clinical_view, robust) {
  dictionary <- triomics_b08_dictionary(clinical_view)
  response_type <- endpoint$response_type
  if (identical(endpoint$endpoint_class, "pathway_enrichment")) {
    return(triomics_b08_blank_result(objective, endpoint$endpoint_id, "unavailable",
                                    "pathway_enrichment_not_patient_score", response_type))
  }
  if (!identical(response_type, "continuous")) {
    return(triomics_b08_blank_result(objective, endpoint$endpoint_id, "unavailable",
                                    "unsupported_response_type", response_type))
  }
  population <- triomics_b08_population(endpoint, objective, clinical_view$links)
  if (!isTRUE(population$ok)) {
    return(triomics_b08_blank_result(objective, endpoint$endpoint_id, "unavailable",
                                    population$reason, response_type))
  }
  predictor <- dictionary[[objective$predictor_ref]]
  adjustments <- objective$adjustments
  if (is.null(adjustments)) adjustments <- list()
  covariates <- lapply(adjustments, function(adjustment) dictionary[[adjustment$variable]])
  exclusions <- list()
  predictor_values <- numeric()
  covariate_values <- list()
  response_values <- numeric()
  coding <- NULL
  reference <- NULL
  target <- NULL
  n_clinical <- 0L
  n_endpoint <- 0L
  for (row in population$rows) {
    interpreted <- triomics_b08_column_value(clinical_view$observations, predictor, row$subject_key, row$visit_id)
    if (identical(interpreted$state, "block")) {
      return(triomics_b08_blank_result(objective, endpoint$endpoint_id, "unavailable",
                                      interpreted$reason, response_type))
    }
    covariate_row <- numeric()
    covariate_block <- NULL
    covariate_missing <- FALSE
    for (covariate in covariates) {
      drawn <- triomics_b08_column_value(clinical_view$observations, covariate, row$subject_key, row$visit_id)
      if (identical(drawn$state, "block")) {
        covariate_block <- drawn$reason
        break
      }
      if (identical(drawn$state, "missing")) covariate_missing <- TRUE
      covariate_row <- c(covariate_row, if (identical(drawn$state, "value")) drawn$value else NA_real_)
    }
    if (!is.null(covariate_block)) {
      return(triomics_b08_blank_result(objective, endpoint$endpoint_id, "unavailable",
                                      covariate_block, response_type))
    }
    endpoint_missing <- !is.finite(row$value)
    clinical_missing <- identical(interpreted$state, "missing") || covariate_missing
    if (clinical_missing) {
      n_clinical <- n_clinical + 1L
      exclusions[[length(exclusions) + 1L]] <- list(
        subject_key = row$subject_key, reason = "clinical_complete_case", source = "clinical"
      )
    }
    if (endpoint_missing) {
      n_endpoint <- n_endpoint + 1L
      exclusions[[length(exclusions) + 1L]] <- list(
        subject_key = row$subject_key, reason = "endpoint_unobserved", source = "endpoint"
      )
    }
    if (clinical_missing || endpoint_missing) next
    if (is.null(coding)) coding <- interpreted$coding
    reference <- interpreted$reference
    target <- interpreted$target
    predictor_values <- c(predictor_values, interpreted$value)
    response_values <- c(response_values, row$value)
    covariate_values[[length(covariate_values) + 1L]] <- covariate_row
  }
  result <- triomics_b08_blank_result(objective, endpoint$endpoint_id, "failed", "no_complete_cases", response_type)
  result$predictor_type <- triomics_b08_variable_type(predictor)
  result$predictor_units <- predictor$units
  result$response_units <- endpoint$units
  result$n_excluded_clinical <- n_clinical
  result$n_excluded_endpoint <- n_endpoint
  result$exclusions <- exclusions
  result$selection <- endpoint$selection
  if (!length(response_values)) return(result)
  if (length(unique(response_values)) < 2L) {
    result$estimation_reason <- "constant_response"
    result$n_subjects <- as.integer(length(response_values))
    result$n_samples <- result$n_subjects
    return(result)
  }
  design <- cbind(intercept = 1, predictor_values)
  names_design <- c("intercept", objective$predictor_ref)
  if (length(covariates)) {
    extra <- do.call(rbind, lapply(covariate_values, function(values) matrix(values, nrow = 1L)))
    design <- cbind(design, extra)
    names_design <- c(names_design, vapply(adjustments, function(adjustment) adjustment$variable, character(1)))
  }
  colnames(design) <- names_design
  if (length(unique(predictor_values)) < 2L) {
    result$estimation_reason <- "constant_predictor"
    result$n_subjects <- as.integer(nrow(design))
    return(result)
  }
  rank <- qr(design)$rank
  if (rank < ncol(design)) {
    result$estimation_reason <- "rank_deficient"
    result$n_subjects <- as.integer(nrow(design))
    return(result)
  }
  weights <- rep(0, ncol(design))
  weights[[2L]] <- 1
  support <- triomics_b_feature_support(response_values, design, list(weights))
  result$n_subjects <- as.integer(nrow(design))
  result$n_samples <- result$n_subjects
  result$ordinary_df <- as.numeric(support$df)
  result$effect_scale <- if (identical(coding, "factor")) "level_difference" else "per_predictor_unit"
  result$effect_unit <- if (identical(coding, "factor")) {
    endpoint$units
  } else {
    paste(endpoint$units, "per", predictor$units)
  }
  result$formula <- paste(endpoint$endpoint_id, "~", paste(colnames(design), collapse = " + "))
  result$contrast_weights <- stats::setNames(as.list(weights), colnames(design))
  result$reference_level <- reference
  result$target_level <- target
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
  contrast <- matrix(weights, ncol = 1L, dimnames = list(colnames(design), "predictor"))
  moderated <- limma::eBayes(
    limma::contrasts.fit(limma::lmFit(matrix(response_values, nrow = 1L), design), contrast),
    trend = FALSE, robust = robust
  )
  limma_effect <- as.numeric(moderated$coefficients)[[1L]]
  limma_unscaled <- as.numeric(moderated$stdev.unscaled)[[1L]]
  limma_sigma <- as.numeric(moderated$sigma)[[1L]]
  if (!triomics_b_close(support$effects[[1L]], limma_effect) ||
      !triomics_b_close(support$ordinary_se[[1L]], limma_sigma * limma_unscaled)) {
    triomics_b_abort("B_FIT_DIVERGENCE", "Ordinary association results diverged from limma.",
                     key = "endpoint_id")
  }
  moderated_se <- sqrt(as.numeric(moderated$s2.post)[[1L]]) * limma_unscaled
  df_total <- as.numeric(moderated$df.total)[[1L]]
  result$effect <- limma_effect
  result$ordinary_se <- as.numeric(support$ordinary_se[[1L]])
  result$moderated_se <- as.numeric(moderated_se)
  result$statistic <- as.numeric(moderated$t)[[1L]]
  result$statistic_name <- "moderated_t"
  result$effective_test_df <- df_total
  result$df_prior <- as.numeric(moderated$df.prior)[[1L]]
  result$p <- as.numeric(moderated$p.value)[[1L]]
  result$n_rows_used <- result$n_subjects
  result$estimation_status <- "estimated"
  result$estimation_reason <- NULL
  result$inferential_status <- if (identical(objective$hypothesis_status, "exploratory")) {
    "exploratory"
  } else {
    "unverified"
  }
  if (is.finite(df_total) && df_total >= 1) {
    crit <- stats::qt(0.975, df_total)
    result$interval_low <- result$effect - crit * result$moderated_se
    result$interval_high <- result$effect + crit * result$moderated_se
    result$interval_method <- "moderated_t"
  }
  ancestry <- if (is.null(endpoint$selection)) NULL else endpoint$selection$ancestry
  inspected <- if (is.null(endpoint$selection)) NULL else endpoint$selection$inspected_outcome
  if (!is.null(endpoint$training_outcome) || identical(ancestry, "trained_on_outcome")) {
    result$inferential_status <- "descriptive_only"
    result$independent_validation_released <- FALSE
  }
  if (identical(ancestry, "selected_after_clinical_outcome") || !is.null(inspected)) {
    result$inferential_status <- "descriptive_only"
    result$estimation_reason <- "selection_after_outcome"
    result$independent_validation_released <- FALSE
  }
  result$limma_used <- TRUE
  result
}

triomics_b08_leak <- function(row, endpoint) {
  if (is.null(endpoint)) return(FALSE)
  ancestry <- if (is.null(endpoint$selection)) NULL else endpoint$selection$ancestry
  inspected <- if (is.null(endpoint$selection)) NULL else endpoint$selection$inspected_outcome
  !is.null(endpoint$training_outcome) || identical(ancestry, "trained_on_outcome") ||
    identical(ancestry, "selected_after_clinical_outcome") || !is.null(inspected) ||
    identical(row$inferential_status, "descriptive_only")
}

triomics_associate_endpoints <- function(endpoint_bundle, clinical_view, objective_spec) {
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
  registry <- triomics_resolve_clinical_objectives(clinical_view, spec)
  endpoints <- triomics_b08_index_endpoints(endpoint_bundle)
  imputation <- spec$imputation
  refused_imputation <- !is.null(imputation) && !identical(imputation, "none")
  family <- spec$family
  family_declared <- is.list(family) && !is.null(family$member_ids) && length(family$member_ids) > 0L
  objectives <- spec$objectives
  objective_for <- list()
  for (objective in objectives) {
    if (is.null(objective$endpoint_ref) || !nzchar(objective$endpoint_ref)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "Each objective needs an endpoint ref.", key = "endpoint_ref")
    }
    if (!is.null(objective_for[[objective$endpoint_ref]])) {
      triomics_b_abort("B_FAMILY_UNDECLARED", "Two objectives name the same endpoint.",
                       key = "endpoint_ref",
                       remediation = "Keep one objective per endpoint inside the declared family.")
    }
    objective_for[[objective$endpoint_ref]] <- objective
  }
  member_ids <- if (family_declared) as.character(unlist(family$member_ids, use.names = FALSE)) else character()
  if (family_declared) {
    if (anyDuplicated(member_ids)) {
      triomics_b_abort(
        "B_FAMILY_UNDECLARED",
        "Declared family member ids must be unique.",
        key = "family_ref",
        remediation = "List each endpoint once in the fixed family."
      )
    }
    for (objective in objectives) {
      if (!objective$endpoint_ref %in% member_ids) {
        triomics_b_abort(
          "B_FAMILY_UNDECLARED",
          "An objective endpoint is outside the declared family.",
          key = "family_ref",
          remediation = "Declare every endpoint in the family before fitting."
        )
      }
    }
  } else {
    member_ids <- unique(vapply(objectives, function(objective) objective$endpoint_ref, character(1)))
  }
  results <- vector("list", length(member_ids))
  estimated <- rep(FALSE, length(member_ids))
  limma_used <- FALSE
  for (i in seq_along(member_ids)) {
    endpoint_id <- member_ids[[i]]
    objective <- objective_for[[endpoint_id]]
    endpoint <- endpoints[[endpoint_id]]
    if (is.null(objective) || is.null(endpoint)) {
      results[[i]] <- triomics_b08_blank_result(objective, endpoint_id, "failed", "endpoint_absent",
                                               if (is.null(endpoint)) NULL else endpoint$response_type)
      next
    }
    if (refused_imputation) {
      results[[i]] <- triomics_b08_blank_result(objective, endpoint_id, "unavailable",
                                               "clinical_imputation_refused", endpoint$response_type)
      next
    }
    if (!family_declared) {
      results[[i]] <- triomics_b08_blank_result(objective, endpoint_id, "unavailable",
                                               "family_undeclared", endpoint$response_type)
      next
    }
    block <- triomics_b08_objective_block(objective, triomics_b08_dictionary(clinical_view))
    if (!is.null(block)) {
      results[[i]] <- triomics_b08_blank_result(objective, endpoint_id, "unavailable", block, endpoint$response_type)
      next
    }
    results[[i]] <- triomics_b08_fit_endpoint(endpoint, objective, clinical_view, spec$robust)
    estimated[[i]] <- identical(results[[i]]$estimation_status, "estimated")
    limma_used <- limma_used || isTRUE(results[[i]]$limma_used)
    results[[i]]$limma_used <- NULL
  }
  internal_p <- rep(1, length(member_ids))
  leak <- FALSE
  qualified <- TRUE
  for (i in seq_along(member_ids)) {
    if (isTRUE(estimated[[i]]) && is.finite(results[[i]]$p)) internal_p[[i]] <- results[[i]]$p
    endpoint <- endpoints[[member_ids[[i]]]]
    if (triomics_b08_leak(results[[i]], endpoint)) leak <- TRUE
    validity <- if (is.null(endpoint) || is.null(endpoint$selection)) NULL else endpoint$selection$validity
    if (isTRUE(estimated[[i]]) && !identical(validity, "qualified")) qualified <- FALSE
  }
  internal_q <- if (length(internal_p)) unname(stats::p.adjust(internal_p, method = "BH")) else numeric()
  q_published <- family_declared && any(estimated) && !leak && qualified
  for (i in seq_along(results)) {
    if (isTRUE(estimated[[i]]) && q_published) {
      results[[i]]$q <- internal_q[[i]]
      if (identical(results[[i]]$hypothesis_status, "independent_validation") &&
          !identical(results[[i]]$inferential_status, "descriptive_only")) {
        results[[i]]$inferential_status <- "independent_validation"
        results[[i]]$independent_validation_released <- TRUE
      }
    } else {
      results[[i]]$q <- NULL
    }
  }
  for (i in seq_along(registry)) {
    matched <- Filter(function(row) identical(row$objective_id, registry[[i]]$objective_id), results)
    if (length(matched)) {
      registry[[i]]$fitted <- any(vapply(matched, function(row) identical(row$estimation_status, "estimated"), logical(1)))
      registry[[i]]$status <- if (isTRUE(registry[[i]]$fitted)) "fitted" else "unavailable"
      registry[[i]]$reason <- matched[[1L]]$estimation_reason
      registry[[i]]$effect <- matched[[1L]]$effect
      registry[[i]]$p <- matched[[1L]]$p
    }
  }
  n_estimated <- as.integer(sum(estimated))
  n_failed <- as.integer(length(member_ids) - n_estimated)
  family_out <- NULL
  if (family_declared) {
    family_out <- list(
      family_id = family$family_id,
      claim_scope = family$claim_scope,
      member_ids = member_ids,
      member_hash = triomics_b_hash(member_ids),
      n_declared = as.integer(length(member_ids)),
      n_estimated = n_estimated,
      n_failed = n_failed,
      correction_method = "BH",
      q_published = q_published,
      q_withheld_reason = if (!q_published && leak) "selection_or_training_lineage" else NULL,
      internal_p = as.numeric(internal_p),
      internal_q = as.numeric(internal_q)
    )
  }
  list(
    status = if (length(member_ids) && all(estimated)) "complete" else if (any(estimated)) "partial" else "unavailable",
    method = if (limma_used) "limma" else "none",
    fallback_applied = FALSE,
    requested_fallback = spec$fallback,
    limma_version = if (limma_used) as.character(packageVersion("limma")) else NULL,
    study_id = clinical_view$study_id,
    analysis_id = endpoint_bundle$analysis_id,
    objectives = registry,
    family = family_out,
    results = results,
    imputation = "none",
    assay_imputation_reused = FALSE
  )
}

associate_endpoints <- triomics_associate_endpoints

triomics_b08_fixture_frame <- function() {
  glucose <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, NA, 15)
  age <- c(40, 42, 41, 55, 50, 48, 60, 58, 70, 66, 72, 71)
  list(glucose = glucose, age = age, n = length(glucose))
}

triomics_b08_write_tsv <- function(path, header, rows) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(paste(header, collapse = "\t"), vapply(rows, function(row) {
    paste(vapply(header, function(name) row[[name]], character(1)), collapse = "\t")
  }, character(1)))
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

triomics_b08_write_fixture_tables <- function(config, config_path) {
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  resolve <- function(path) if (grepl("^/", path)) path else file.path(root, path)
  frame <- triomics_b08_fixture_frame()
  dictionary_header <- c(
    "variable_id", "label", "type", "units", "units_verification", "role",
    "allowed_model_roles", "time_semantics", "privacy", "verification", "evidence",
    "codebook", "missing_map", "censoring", "value_separator", "time_format"
  )
  evidence <- "Synthetic fixture; no participant data."
  dictionary <- list(
    list(variable_id = "glucose", label = "Glucose", type = "continuous", units = "mmol/L",
         units_verification = "verified", role = "measured", allowed_model_roles = "predictor",
         time_semantics = "visit", privacy = "ordinary", verification = "verified", evidence = evidence,
         codebook = "", missing_map = ".=placeholder", censoring = "forbidden", value_separator = "",
         time_format = "iso8601_date"),
    list(variable_id = "age", label = "Age", type = "continuous", units = "year",
         units_verification = "verified", role = "measured", allowed_model_roles = "predictor",
         time_semantics = "visit", privacy = "ordinary", verification = "verified", evidence = evidence,
         codebook = "", missing_map = ".=placeholder", censoring = "forbidden", value_separator = "",
         time_format = "iso8601_date"),
    list(variable_id = "mrn", label = "Record", type = "text", units = "inapplicable",
         units_verification = "inapplicable", role = "administrative", allowed_model_roles = "excluded",
         time_semantics = "subject_static", privacy = "identifier", verification = "verified",
         evidence = evidence, codebook = "", missing_map = "", censoring = "forbidden",
         value_separator = "", time_format = "none")
  )
  triomics_b08_write_tsv(resolve(config$clinical$dictionary_file), dictionary_header, dictionary)
  observation_header <- c("subject_id", "visit_id", "sample_id", "time", "time_unit", "glucose", "age", "mrn")
  observations <- lapply(seq_len(frame$n), function(i) {
    list(
      subject_id = sprintf("SYN-SUBJ-%02d", i),
      visit_id = "V1",
      sample_id = sprintf("SYN_%02d", i),
      time = sprintf("2020-01-%02d", i),
      time_unit = "day",
      glucose = if (is.na(frame$glucose[[i]])) "." else format(frame$glucose[[i]], trim = TRUE, scientific = FALSE),
      age = format(frame$age[[i]], trim = TRUE, scientific = FALSE),
      mrn = sprintf("SYN-MRN-%02d", i)
    )
  })
  triomics_b08_write_tsv(resolve(config$clinical$observations_file), observation_header, observations)
  link_header <- c("link_id", "subject_id", "visit_id", "sample_id", "cardinality", "match_rule",
                   "reference_time", "window_days", "tie_rule")
  links <- lapply(seq_len(frame$n), function(i) {
    list(link_id = sprintf("L%02d", i), subject_id = sprintf("SYN-SUBJ-%02d", i), visit_id = "V1",
         sample_id = sprintf("SYN_%02d", i), cardinality = "one_to_one", match_rule = "exact_sample",
         reference_time = "", window_days = "", tie_rule = "")
  })
  triomics_b08_write_tsv(resolve(config$clinical$links_file), link_header, links)
  invisible(TRUE)
}

triomics_b08_fixture_bundle <- function(clinical_bundle) {
  frame <- triomics_b08_fixture_frame()
  subject_key <- function(index, variable_id) {
    sample_id <- sprintf("SYN_%02d", index)
    hits <- Filter(function(obs) {
      identical(obs$variable_id, variable_id) && identical(obs$sample_id, sample_id)
    }, clinical_bundle$observations)
    hits[[1L]]$subject_key
  }
  score_of <- function(index, slope) {
    if (index == 12L || is.na(frame$glucose[[index]])) return(NA_real_)
    slope * frame$glucose[[index]] + if (slope == 2) 0.1 * frame$age[[index]] else 0
  }
  make_endpoint <- function(endpoint_id, slope) {
    list(
      endpoint_id = endpoint_id,
      endpoint_class = "score",
      response_type = "continuous",
      units = "score_unit",
      scale = "linear",
      samples = lapply(seq_len(frame$n), function(i) {
        list(sample_id = sprintf("SYN_%02d", i), subject_key = subject_key(i, "glucose"),
             visit_id = "V1", value = score_of(i, slope))
      }),
      producing_fit = "synthetic-b-t08",
      training_ids = NULL,
      training_outcome = NULL,
      orientation = "native",
      selection = list(ancestry = "prespecified", validity = "qualified", inspected_outcome = NULL),
      available_at_time = "V1",
      dependency_group = "synthetic",
      matrix_hash = NULL,
      source_hash = clinical_bundle$observations_sha256
    )
  }
  list(
    study_id = clinical_bundle$study_id,
    analysis_id = "synthetic_primary",
    endpoints = list(make_endpoint("synthetic_score", 2), make_endpoint("held_score", 1))
  )
}

triomics_b08_cli_spec <- function(config) {
  objectives <- lapply(config$clinical$objectives, function(objective) {
    objective$objective_version <- "1"
    if (is.null(objective$question)) {
      objective$question <- "Synthetic between-person score association. Not a clinical claim."
    }
    objective
  })
  if (!any(vapply(objectives, function(objective) identical(objective$endpoint_ref, "held_score"), logical(1)))) {
    held <- objectives[[1L]]
    held$objective_id <- "held_between"
    held$response_ref <- "held_score"
    held$endpoint_ref <- "held_score"
    held$question <- "Second prespecified synthetic score in the same family."
    objectives[[length(objectives) + 1L]] <- held
  }
  list(
    objectives = objectives,
    family = list(
      family_id = objectives[[1L]]$family_ref,
      member_ids = c("synthetic_score", "held_score", "unsupplied_score"),
      claim_scope = "between_person_synthetic_scores"
    ),
    trend = FALSE,
    robust = FALSE,
    fallback = "none",
    imputation = "none"
  )
}

triomics_execute_clinical_associations <- function(config_path, output) {
  config <- triomics_read_config(config_path)
  if (!isTRUE(config$clinical$enabled)) {
    return(list(status = "not_requested"))
  }
  if (identical(config$study_id, "triomics_b_t08_fixture")) {
    triomics_b08_write_fixture_tables(config, config_path)
  }
  ingested <- triomics_ingest_configured_study(config, config_path)
  clinical <- triomics_ingest_clinical(config, config_path, ingested$sample_registry)
  bundle <- triomics_b08_fixture_bundle(clinical)
  fit <- triomics_associate_endpoints(bundle, clinical, triomics_b08_cli_spec(config))
  exported <- c(list(schema_version = "2.0"), triomics_clinical_association_export(fit))
  payload <- file.path(output, "B", "clinical-associations.json")
  triomics_write_json_atomic(exported, payload)
  inputs <- list(
    list(artifact_id = "clinical-dictionary-source", sha256 = clinical$dictionary_sha256),
    list(artifact_id = "clinical-observations-source", sha256 = clinical$observations_sha256),
    list(artifact_id = "clinical-links-source", sha256 = clinical$links_sha256)
  )
  envelope <- triomics_artifact_envelope(
    artifact_type = "clinical_association_results",
    artifact_id = "b-t08-clinical-associations",
    study_id = config$study_id,
    run_id = "b-t08",
    status = fit$status,
    config_sha256 = attr(config, "config_sha256"),
    payload = payload,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = if (identical(fit$status, "complete")) character() else "partial_family",
    privacy_class = "synthetic"
  )
  triomics_write_envelope(envelope, file.path(output, "B", "clinical-associations.envelope.json"))
  primary <- Filter(function(row) identical(row$endpoint_id, "synthetic_score"), exported$results)[[1L]]
  held <- Filter(function(row) identical(row$endpoint_id, "held_score"), exported$results)[[1L]]
  absent <- Filter(function(row) identical(row$endpoint_id, "unsupplied_score"), exported$results)[[1L]]
  text <- paste(readLines(payload, warn = FALSE), collapse = "\n")
  reasons <- vapply(primary$exclusions, function(item) item$reason, character(1))
  ac1 <- is.finite(primary$effect) && abs(primary$effect - 2) <= 1e-6 &&
    identical(primary$effect_unit, "score_unit per mmol/L") && identical(primary$n_subjects, 10L) &&
    is.finite(primary$ordinary_se) && is.finite(primary$moderated_se) &&
    identical(primary$interval_method, "moderated_t") && is.finite(held$effect) && abs(held$effect - 1) <= 1e-6
  ac3 <- identical(exported$family$n_declared, 3L) && identical(exported$family$n_failed, 1L) &&
    identical(absent$estimation_reason, "endpoint_absent") && is.null(absent$q) && is.finite(primary$q)
  ac4 <- !grepl("SYN-SUBJ-", text, fixed = TRUE) && !grepl("SYN-MRN-", text, fixed = TRUE) &&
    !grepl("\"subject_id\"", text, fixed = TRUE) &&
    "clinical_complete_case" %in% reasons && "endpoint_unobserved" %in% reasons &&
    identical(primary$imputation, "none") && isFALSE(primary$assay_imputation_reused)
  record <- list(
    packet_id = "B-T08",
    study_id = config$study_id,
    command = "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T08.yaml --stage B --output artifacts/acceptance/B-T08",
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      limma_version = fit$limma_version,
      platform = R.version$platform
    ),
    status = if (ac1 && ac3 && ac4) "complete" else "failed",
    acceptance = list(
      list(id = "B-T08-AC1", verdict = if (ac1) "pass" else "fail"),
      list(id = "B-T08-AC2", verdict = "covered_by_testthat",
           detail = "Unsupported, censored, unknown and constant cases are refused without Pearson."),
      list(id = "B-T08-AC3", verdict = if (ac3) "pass" else "fail"),
      list(id = "B-T08-AC4", verdict = if (ac4) "pass" else "fail")
    ),
    payload_sha256 = triomics_sha256(payload),
    envelope_sha256 = triomics_sha256(file.path(output, "B", "clinical-associations.envelope.json"))
  )
  triomics_write_json_atomic(record, file.path(output, "acceptance-record.json"))
  if (!(ac1 && ac3 && ac4)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The synthetic clinical association did not meet its oracle.",
                     key = "clinical.objectives",
                     remediation = "Do not publish a Pearson fallback or an imputed clinical value.")
  }
  invisible(fit)
}

triomics_b08_prepare_cli <- function(args) {
  options <- triomics_parse_cli(args)
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) || isTRUE(options$help) ||
      is.null(options$config) || !file.exists(options$config)) {
    return(invisible(FALSE))
  }
  config <- triomics_read_config(options$config)
  if (!identical(config$study_id, "triomics_b_t08_fixture") || !isTRUE(config$clinical$enabled)) {
    return(invisible(FALSE))
  }
  triomics_b08_write_fixture_tables(config, options$config)
  invisible(TRUE)
}

triomics_b08_cli_after_stage_b <- function(args) {
  options <- triomics_parse_cli(args)
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) || is.null(options$config) ||
      is.null(options$output)) {
    return(invisible(FALSE))
  }
  config <- triomics_read_config(options$config)
  if (!identical(config$study_id, "triomics_b_t08_fixture") || !isTRUE(config$clinical$enabled)) {
    return(invisible(FALSE))
  }
  triomics_execute_clinical_associations(options$config, options$output)
}
