c08_shared_unit_map <- function(unit_ids, subjects = paste0("subject-", unit_ids),
                                representation_id = "levels") {
  lapply(seq_along(unit_ids), function(i) {
    list(
      unit_id = unit_ids[[i]],
      unit_type = "visit_level",
      source_sample_ids = unit_ids[[i]],
      subject_id = subjects[[i]],
      representation_id = representation_id
    )
  })
}

c08_shared_resolved <- function(unit_ids = sprintf("sample-%02d", seq_len(8L)),
                                subjects = paste0("subject-", unit_ids),
                                outcome = NULL, representation_id = "levels") {
  index <- seq_along(unit_ids)
  alpha <- rbind(
    alpha_1 = index + 0.2 * sin(index),
    alpha_2 = index^2 + cos(index),
    alpha_3 = sin(index * 0.8),
    alpha_4 = cos(index * 0.3) + index / 5
  )
  beta <- rbind(
    beta_1 = cos(index * 0.4),
    beta_2 = sin(index * 1.2) + index / 7,
    beta_3 = index^2 / 20 + cos(index),
    beta_4 = sin(index * 0.7) - index / 9
  )
  colnames(alpha) <- colnames(beta) <- unit_ids
  blocks <- list(alpha = alpha, beta = beta)
  list(
    study_id = "study-c08-shared-synthetic",
    analysis_id = "analysis-c08-shared-synthetic",
    preparation_context_id = "context-c08-shared-synthetic",
    representation_id = representation_id,
    unit_ids = unit_ids,
    subjects = stats::setNames(subjects, unit_ids),
    outcome = if (is.null(outcome)) NULL else stats::setNames(outcome, unit_ids),
    unit_map = c08_shared_unit_map(unit_ids, subjects, representation_id),
    blocks = stats::setNames(lapply(names(blocks), function(block_id) {
      list(
        block_id = block_id,
        assay = blocks[[block_id]],
        sample_ids = unit_ids,
        feature_keys = rownames(blocks[[block_id]]),
        source_selection = list(
          selection_history = "full_unselected",
          selection_evidence = "Public deterministic C-08 software fixture."
        )
      )
    }), names(blocks))
  )
}

c08_shared_spec <- function(method = "concat_pca", unit_ids,
                            outcome = NULL, n_components = 2L) {
  list(
    method = method,
    outcome = outcome,
    block_ids = c("alpha", "beta"),
    unit_ids = unit_ids,
    training_unit_ids = unit_ids,
    target_unit_ids = unit_ids,
    required_views = c("alpha", "beta"),
    n_components = as.integer(n_components),
    ncomp = as.integer(n_components),
    scale_features = TRUE,
    block_scaling = "none",
    selection = list(rule = "none"),
    score_definition = if (identical(method, "concat_pca")) "native" else "consensus_mean"
  )
}

c08_shared_context <- function(resolved, integration_id = "shared-fit",
                               ancestry = "prespecified", inspected_outcome = NULL) {
  list(
    integration_id = integration_id,
    unit_map = resolved$unit_map,
    selection = list(
      ancestry = ancestry,
      inspected_outcome = inspected_outcome,
      source_selection = lapply(resolved$blocks, `[[`, "source_selection")
    ),
    available_at_time = "after_integration_fit",
    dependency_group = paste(resolved$study_id, integration_id, sep = "::")
  )
}

c08_shared_run_once <- function(resolved, spec, context) {
  adapter <- triomics_integration_backend(spec$method)
  fit_count <- new.env(parent = emptyenv())
  fit_count$value <- 0L
  fit <- adapter$fit
  adapter$fit <- function(...) {
    fit_count$value <- fit_count$value + 1L
    fit(...)
  }
  output <- triomics_integration_fit_once(
    resolved, spec, c08_context = context, adapter = adapter
  )
  output$instrumented_fit_count <- fit_count$value
  output
}

c08_shared_clinical_view <- function(unit_map, visits, times) {
  ids <- vapply(unit_map, `[[`, character(1), "unit_id")
  subjects <- vapply(unit_map, `[[`, character(1), "subject_id")
  links <- lapply(seq_along(ids), function(i) list(
    link_id = paste0("link-", ids[[i]]),
    sample_id = ids[[i]],
    subject_id = subjects[[i]],
    subject_key = subjects[[i]],
    visit_id = visits[[i]],
    reference_time = times[[i]],
    status = "approved",
    cardinality = "one_to_one"
  ))
  observations <- lapply(seq_along(ids), function(i) list(
    observation_id = paste0("glucose-", ids[[i]]),
    variable_id = "glucose",
    subject_key = subjects[[i]],
    visit_id = visits[[i]],
    sample_id = ids[[i]],
    time = times[[i]],
    value_kind = "numeric",
    numeric_value = as.numeric(i + if (identical(visits[[i]], "V1")) 0.5 else 0),
    units = "mmol/L",
    time_semantics = "visit"
  ))
  list(
    study_id = "study-c08-shared-synthetic",
    links = links,
    dictionary = list(list(
      variable_id = "glucose",
      declared_type = "continuous",
      type = "continuous",
      units = "mmol/L",
      units_verification = "verified",
      role = "measured",
      allowed_model_roles = "predictor",
      time_semantics = "visit",
      privacy = "ordinary",
      verification = "verified",
      modelable = TRUE
    )),
    observations = observations
  )
}

c08_shared_objective <- function(endpoint_id, analysis_id, mode, visit = "V1") {
  objective <- list(
    objective_id = paste0("objective-", mode),
    objective_version = "1",
    analysis_id = analysis_id,
    mode = mode,
    response_ref = endpoint_id,
    predictor_ref = "glucose",
    endpoint_ref = endpoint_id,
    adjustments = list(),
    time_window_ref = visit,
    missingness_policy = "complete_case",
    family_ref = "synthetic-score-family",
    hypothesis_status = "exploratory",
    protocol_ref = "synthetic-c08-shared"
  )
  if (identical(mode, "baseline_to_later")) {
    objective$baseline_visit_ref <- "V0"
    objective$later_visit_ref <- "V1"
    objective$horizon_days <- 30L
    objective$baseline_adjustment <- FALSE
    objective$outcome_estimand <- "change"
  }
  list(
    objectives = list(objective),
    family = list(
      family_id = "synthetic-score-family",
      member_ids = endpoint_id,
      claim_scope = "synthetic_score_association"
    ),
    trend = FALSE,
    robust = FALSE,
    fallback = "none",
    imputation = "none"
  )
}

c08_shared_fixture_path <- function() {
  triomics_test_resource("examples", "tickets", "C-08.yaml")
}

c08_shared_config_with_selection <- function(selection_by_block) {
  source_path <- c08_shared_fixture_path()
  config <- yaml::read_yaml(source_path)
  config_root <- dirname(source_path)
  resolve_fixture_path <- function(path) {
    if (is.null(path)) return(NULL)
    normalizePath(file.path(config_root, path), winslash = "/", mustWork = TRUE)
  }
  for (i in seq_along(config$sources)) {
    config$sources[[i]]$file <- resolve_fixture_path(config$sources[[i]]$file)
  }
  config$metadata$file <- resolve_fixture_path(config$metadata$file)
  for (i in seq_along(config$blocks)) {
    config$blocks[[i]]$file <- resolve_fixture_path(config$blocks[[i]]$file)
    config$blocks[[i]]$source_refs <- as.list(config$blocks[[i]]$source_refs)
  }
  for (field in c("dictionary_file", "observations_file", "links_file")) {
    config$clinical[[field]] <- resolve_fixture_path(config$clinical[[field]])
  }
  block_ids <- vapply(config$blocks, `[[`, character(1L), "block_id")
  source_ids <- vapply(config$sources, `[[`, character(1L), "source_id")
  for (block_id in names(selection_by_block)) {
    block_index <- match(block_id, block_ids)
    source_refs <- unlist(config$blocks[[block_index]]$source_refs, use.names = FALSE)
    for (source_ref in source_refs) {
      source_index <- match(source_ref, source_ids)
      config$sources[[source_index]]$selection_history <- selection_by_block[[block_id]]
    }
  }
  path <- tempfile("triomics-c08-selection-", fileext = ".yaml")
  yaml::write_yaml(config, path)
  path
}

testthat::test_that("the shared fit seam retains one fit and emits a qualified PCA handoff", {
  ids <- sprintf("sample-%02d", seq_len(8L))
  resolved <- c08_shared_resolved(ids)
  spec <- c08_shared_spec(unit_ids = ids)
  output <- c08_shared_run_once(
    resolved, spec, c08_shared_context(resolved, "pca-shared")
  )

  testthat::expect_identical(output$instrumented_fit_count, 1L)
  testthat::expect_s3_class(output$backend_fit, "BackendFit")
  testthat::expect_s3_class(output$result, "IntegrationResult")
  testthat::expect_identical(
    names(output$result), c("scores", "loadings", "variance", "selected", "backend", "params")
  )
  handoff <- output$backend_fit$metadata$c08_handoff
  testthat::expect_identical(handoff$schema_version, "C-08-fit-handoff-1")
  testthat::expect_identical(handoff$qualification, "same_fit_adapter_handoff")
  testthat::expect_true(triomics_c08_valid_sha256(handoff$fit_state_sha256))
  testthat::expect_identical(output$backend_fit$metadata$state_hash, handoff$fit_state_sha256)
  testthat::expect_identical(handoff$training_unit_ids, as.list(ids))
  testthat::expect_identical(
    handoff$input_hashes,
    output$result$params$provenance$matrix_sha256
  )
  testthat::expect_true(triomics_c08_valid_sha256(handoff$training_scores_sha256))
  testthat::expect_true(triomics_c08_valid_sha256(handoff$handoff_sha256))

  bundle <- triomics_integration_score_endpoints(
    output$result, output$backend_fit, output$context
  )
  testthat::expect_identical(bundle$endpoints[[1L]]$producing_fit$state_sha256,
                             handoff$fit_state_sha256)
  testthat::expect_identical(bundle$endpoints[[1L]]$selection$ancestry, "prespecified")

  changed_result <- output$result
  changed_result$scores[1L, 1L] <- changed_result$scores[1L, 1L] + 1
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    changed_result, output$backend_fit, output$context
  ))
  testthat::expect_identical(error$code, "C_SCORE_FIT_BINDING_MISMATCH")
})

testthat::test_that("public integrate_blocks keeps the protected six-field result", {
  ids <- sprintf("sample-%02d", seq_len(8L))
  resolved <- c08_shared_resolved(ids)
  result <- integrate_blocks(resolved, c08_shared_spec(unit_ids = ids))

  testthat::expect_s3_class(result, "IntegrationResult")
  testthat::expect_identical(
    names(result), c("scores", "loadings", "variance", "selected", "backend", "params")
  )
})

testthat::test_that("C-08 preserves visit-level subject-centered unit identity", {
  ids <- c("SYN_01", "SYN_02", "SYN_03")
  unit_map <- lapply(seq_along(ids), function(index) list(
    unit_id = ids[[index]],
    unit_type = "visit_level",
    source_sample_ids = ids[[index]],
    subject_id = paste0("subject-", ceiling(index / 3)),
    visit_id = paste0("visit-", index),
    time = as.numeric(index - 1L),
    time_unit = "synthetic_step",
    time_origin = "0",
    representation_id = "subject_centered",
    eligibility = "retained",
    exclusion_reason = NULL,
    study_id = "study-c08-trajectory",
    analysis_id = "analysis-c08-trajectory",
    preparation_context_id = "context-c08-trajectory"
  ))
  context <- list(
    study_id = "study-c08-trajectory",
    analysis_id = "analysis-c08-trajectory",
    preparation_context_id = "context-c08-trajectory",
    representation_id = "subject_centered"
  )

  mapped <- triomics_c08_validate_unit_map(unit_map, ids, context)

  testthat::expect_identical(mapped$unit_type, rep("visit_level", length(ids)))
  testthat::expect_identical(mapped$representation_id, rep("subject_centered", length(ids)))
  testthat::expect_identical(unclass(mapped$source_sample_ids), lapply(ids, identity))
  testthat::expect_identical(unclass(mapped$visit_id), lapply(seq_along(ids), function(i) paste0("visit-", i)))
  testthat::expect_identical(unclass(mapped$time), lapply(seq_along(ids), function(i) as.numeric(i - 1L)))
  testthat::expect_identical(unclass(mapped$time_unit), as.list(rep("synthetic_step", length(ids))))
  testthat::expect_identical(unclass(mapped$time_origin), as.list(rep("0", length(ids))))

  for (field in c("subject_id", "visit_id", "time_unit", "time_origin")) {
    changed <- unit_map
    changed[[1L]][[field]] <- ""
    error <- testthat::capture_error(triomics_c08_validate_unit_map(changed, ids, context))
    testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")
  }
  changed <- unit_map
  changed[[1L]]$time <- Inf
  error <- testthat::capture_error(triomics_c08_validate_unit_map(changed, ids, context))
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")
})

testthat::test_that("C-08 preserves paired sign names and leaves mean weights numeric", {
  paired <- list(list(
    unit_id = "subject-a", unit_type = "subject_delta", representation_id = "delta",
    source_sample_ids = c("a-v0", "a-v1"), subject_id = "subject-a",
    visit_weights = stats::setNames(c(-1, 1), c("baseline", "followup")),
    eligibility = "retained"
  ))
  paired_context <- list(
    study_id = "study", analysis_id = "paired", preparation_context_id = "prep",
    representation_id = "delta"
  )
  paired_map <- triomics_c08_validate_unit_map(paired, "subject-a", paired_context)
  testthat::expect_identical(names(paired_map$visit_weights[[1L]]), c("baseline", "followup"))
  testthat::expect_identical(as.numeric(unlist(paired_map$visit_weights[[1L]])), c(-1, 1))

  mean_map <- list(list(
    unit_id = "subject-b", unit_type = "subject_mean", representation_id = "mean",
    source_sample_ids = c("b-v0", "b-v1"), subject_id = "subject-b",
    visit_weights = stats::setNames(c(0.25, 0.75), c("v0", "v1")),
    eligibility = "retained"
  ))
  mean_context <- list(
    study_id = "study", analysis_id = "mean", preparation_context_id = "prep",
    representation_id = "mean"
  )
  mean_result <- triomics_c08_validate_unit_map(mean_map, "subject-b", mean_context)
  testthat::expect_identical(mean_result$visit_weights[[1L]],
                             stats::setNames(c(0.25, 0.75), c("v0", "v1")))
})

testthat::test_that("actual DIABLO and block.spls fits receive same-fit handoffs", {
  ids <- sprintf("sample-%02d", seq_len(12L))
  subjects <- paste0("subject-", ids)
  group <- factor(rep(c("control", "case"), each = 6L), levels = c("control", "case"))
  resolved <- c08_shared_resolved(ids, subjects, group)
  diablo_spec <- c08_shared_spec("mixomics_diablo", ids, outcome = "group", n_components = 1L)
  diablo_spec$scale_features <- TRUE
  diablo_spec$tuning_grid_ref <- "public-c08-diablo-grid"
  diablo_spec$prediction_rule <- "centroids.dist"
  diablo_spec$na_capability <- "complete_predictor_blocks"
  diablo_spec$class_levels <- c("control", "case")
  diablo_spec$design_matrix <- matrix(
    c(0, 0.5, 0.5, 0), 2L, dimnames = list(c("alpha", "beta"), c("alpha", "beta"))
  )
  diablo_spec$keepX <- list(alpha = 2L, beta = 2L)
  diablo_context <- c08_shared_context(
    resolved, "diablo-shared", ancestry = "trained_on_outcome", inspected_outcome = "group"
  )
  diablo <- c08_shared_run_once(resolved, diablo_spec, diablo_context)
  diablo_bundle <- triomics_integration_score_endpoints(
    diablo$result, diablo$backend_fit, diablo$context
  )

  response <- stats::setNames(2 + sin(seq_along(ids)) + seq_along(ids) / 4, ids)
  continuous <- c08_shared_resolved(ids, subjects, response)
  block_spls_spec <- c08_shared_spec(
    "mixomics_block_spls", ids, outcome = "glucose", n_components = 1L
  )
  block_spls_spec$response_block <- "outcome"
  block_spls_spec$mode <- "regression"
  block_spls_spec$keepX <- list(alpha = 2L, beta = 2L)
  block_spls_spec$keepY <- 1L
  block_spls_spec$design <- matrix(
    c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3L,
    dimnames = list(c("alpha", "beta", "outcome"), c("alpha", "beta", "outcome"))
  )
  block_spls_spec$prediction_rule <- "AveragedPredict"
  block_spls_spec$scale_features <- TRUE
  block_spls_spec$block_scaling <- "none"
  block_spls_context <- c08_shared_context(
    continuous, "block-spls-shared", ancestry = "trained_on_outcome", inspected_outcome = "glucose"
  )
  block_spls <- c08_shared_run_once(continuous, block_spls_spec, block_spls_context)
  block_spls_bundle <- triomics_integration_score_endpoints(
    block_spls$result, block_spls$backend_fit, block_spls$context
  )

  testthat::expect_identical(diablo$instrumented_fit_count, 1L)
  testthat::expect_identical(block_spls$instrumented_fit_count, 1L)
  testthat::expect_identical(diablo_bundle$endpoints[[1L]]$inference_status, "descriptive_only")
  testthat::expect_identical(block_spls_bundle$endpoints[[1L]]$inference_status, "descriptive_only")
  testthat::expect_true(triomics_c08_valid_sha256(diablo$backend_fit$metadata$c08_handoff$handoff_sha256))
  testthat::expect_true(triomics_c08_valid_sha256(block_spls$backend_fit$metadata$c08_handoff$handoff_sha256))
})

testthat::test_that("the accepted MOFA BackendFit shape binds its factors without a native run", {
  ids <- sprintf("sample-%02d", seq_len(6L))
  resolved <- c08_shared_resolved(ids)
  spec <- c08_shared_spec("mofa2", ids, n_components = 2L)
  factor_scores <- cbind(
    Factor1 = seq_along(ids) + sin(seq_along(ids)),
    Factor2 = cos(seq_along(ids)) + seq_along(ids) / 3
  )
  rownames(factor_scores) <- ids
  loadings <- list(
    alpha = matrix(c(0.7, 0.3, 0.2, 0.8), 2L, dimnames = list(c("a1", "a2"), colnames(factor_scores))),
    beta = matrix(c(0.6, 0.4, 0.5, 0.5), 2L, dimnames = list(c("b1", "b2"), colnames(factor_scores)))
  )
  variance <- matrix(
    c(0.2, 0.3, 0.4, 0.5), 2L,
    dimnames = list(c("alpha", "beta"), colnames(factor_scores))
  )
  selected <- lapply(loadings, rownames)
  state_hash <- paste(rep("d", 64L), collapse = "")
  fit_params <- list(
    native_method = "MOFA2::run_mofa",
    native_version = "1.22.1",
    python_backend = "mofapy2",
    python_backend_version = "0.7.5",
    training_unit_ids = ids,
    training_state_hash = state_hash,
    score_definition = "native_mofa_factor_values",
    exploratory_only = TRUE
  )
  fit <- structure(list(
    trained = TRUE,
    native_state = list(
      model = structure(list(), class = "MOFA"),
      reloaded_model = structure(list(), class = "MOFA"),
      block_ids = c("alpha", "beta"),
      training_unit_ids = ids,
      components = colnames(factor_scores),
      factors = factor_scores,
      weights = loadings,
      variance = variance,
      native_model_sha256 = paste(rep("e", 64L), collapse = ""),
      native_version = "1.22.1",
      python_backend_version = "0.7.5",
      training_state_hash = state_hash,
      training_input_hashes = list(alpha = paste(rep("a", 64L), collapse = ""),
                                   beta = paste(rep("b", 64L), collapse = "")),
      prediction_state = list(uses_outcome = FALSE)
    ),
    selected = selected,
    result_params = fit_params,
    metadata = list(
      backend = "mofa2",
      native_method = "MOFA2::run_mofa",
      native_version = "1.22.1",
      python_backend_version = "0.7.5",
      model_role = "exploratory_structure_discovery",
      training_unit_ids = ids,
      state_hash = state_hash,
      native_model_sha256 = paste(rep("e", 64L), collapse = ""),
      training_input_hashes = list(alpha = paste(rep("a", 64L), collapse = ""),
                                   beta = paste(rep("b", 64L), collapse = ""))
    )
  ), class = "BackendFit")
  result <- triomics_new_integration_result(
    factor_scores, loadings, variance, selected, "mofa2", fit_params
  )
  input_hashes <- stats::setNames(lapply(c("alpha", "beta"), function(block_id) {
    digest::digest(resolved$blocks[[block_id]]$assay, algo = "sha256", serialize = TRUE)
  }), c("alpha", "beta"))
  result$params$provenance <- list(
    study_id = resolved$study_id,
    analysis_id = resolved$analysis_id,
    preparation_context_id = resolved$preparation_context_id,
    representation_id = resolved$representation_id,
    unit_map = ids,
    matrix_sha256 = input_hashes
  )
  context <- c08_shared_context(resolved, "mofa-shape")
  context$study_id <- resolved$study_id
  context$analysis_id <- resolved$analysis_id
  context$preparation_context_id <- resolved$preparation_context_id
  context$representation_id <- resolved$representation_id
  context$input_hashes <- input_hashes
  context$training_y <- NULL
  bound <- triomics_integration_bind_c08_handoff(fit, result, context)
  bundle <- triomics_integration_score_endpoints(
    result, bound$backend_fit, bound$context
  )

  testthat::expect_true(triomics_c08_valid_sha256(
    bound$backend_fit$metadata$c08_handoff$handoff_sha256
  ))
  testthat::expect_identical(bundle$endpoints[[1L]]$scale, "native_mofa_factor_value")
  testthat::expect_identical(bundle$endpoints[[1L]]$inference_status, "exploratory")
})

testthat::test_that("scheduled PCA level endpoints are directly accepted by B-T08 and B-T09", {
  ids <- paste0("sample-", rep(LETTERS[1:4], each = 2L), "-", rep(c("V0", "V1"), 4L))
  subjects <- paste0("subject-", rep(LETTERS[1:4], each = 2L))
  visits <- rep(c("V0", "V1"), 4L)
  times <- rep(c("2020-01-01", "2020-01-31"), 4L)
  resolved <- c08_shared_resolved(ids, subjects)
  spec <- c08_shared_spec(unit_ids = ids)
  output <- c08_shared_run_once(
    resolved, spec, c08_shared_context(resolved, "consumer-fit")
  )
  bundle <- triomics_integration_score_endpoints(
    output$result, output$backend_fit, output$context
  )
  view <- c08_shared_clinical_view(output$context$unit_map, visits, times)

  later_indices <- which(visits == "V1")
  later_ids <- ids[later_indices]
  later_subjects <- subjects[later_indices]
  later_resolved <- c08_shared_resolved(later_ids, later_subjects)
  later_spec <- c08_shared_spec(unit_ids = later_ids)
  later_output <- c08_shared_run_once(
    later_resolved, later_spec, c08_shared_context(later_resolved, "consumer-fit")
  )
  later_bundle <- triomics_integration_score_endpoints(
    later_output$result, later_output$backend_fit, later_output$context
  )
  later_view <- c08_shared_clinical_view(
    later_output$context$unit_map, visits[later_indices], times[later_indices]
  )
  endpoint_id <- later_bundle$endpoints[[1L]]$endpoint_id

  between_request <- triomics_schedule_clinical_score_request(
    later_bundle,
    c08_shared_objective(endpoint_id, resolved$analysis_id, "between_person", "V1"),
    later_view
  )
  b08 <- triomics_associate_endpoints(
    between_request$endpoint_bundle, later_view, between_request$objective_spec
  )

  repeated_request <- triomics_schedule_clinical_score_request(
    bundle,
    c08_shared_objective(endpoint_id, resolved$analysis_id, "baseline_to_later"),
    view
  )
  b09 <- triomics_associate_longitudinal_endpoints(
    repeated_request$endpoint_bundle, view, repeated_request$objective_spec
  )

  testthat::expect_identical(b08$results[[1L]]$estimation_status, "estimated")
  testthat::expect_identical(b08$results[[1L]]$n_subjects, 4L)
  testthat::expect_identical(b09$results[[1L]]$estimation_status, "estimated")
  testthat::expect_identical(b09$results[[1L]]$n_subjects, 4L)
  testthat::expect_identical(b09$results[[1L]]$n_samples, 8L)
})

testthat::test_that("Stage C writes immutable C-08 endpoints and schedules, but does not execute, B work", {
  output_dir <- tempfile("triomics-c08-shared-cli-")
  config_path <- c08_shared_fixture_path()
  result <- triomics_execute_stage_c(
    config_path, output_dir
  )
  integration <- result$results[[1L]]
  endpoint_path <- file.path(output_dir, integration$score_endpoint_bundle$payload_file)
  endpoint_envelope_path <- file.path(output_dir, integration$score_endpoint_bundle$envelope_file)
  request_path <- file.path(output_dir, integration$clinical_score_request$payload_file)
  request_envelope_path <- file.path(output_dir, integration$clinical_score_request$envelope_file)

  testthat::expect_true(file.exists(endpoint_path))
  testthat::expect_true(file.exists(request_path))
  testthat::expect_true(triomics_validate_envelope(endpoint_envelope_path))
  testthat::expect_true(triomics_validate_envelope(request_envelope_path))
  endpoint_bundle <- jsonlite::read_json(endpoint_path, simplifyVector = FALSE)
  class(endpoint_bundle) <- c("IntegrationScoreEndpointBundle", "list")
  testthat::expect_no_error(triomics_c08_validate_endpoint_bundle(endpoint_bundle))
  testthat::expect_identical(endpoint_bundle$schema_version, "2.0")
  source_selection <- endpoint_bundle$endpoints[[1L]]$selection$source_selection
  testthat::expect_setequal(names(source_selection), c("block_1", "block_2"))
  testthat::expect_true(all(vapply(source_selection, function(source) {
    length(source$selection_history) == 1L && nzchar(source$selection_history)
  }, logical(1L))))
  request <- jsonlite::read_json(
    request_path, simplifyVector = FALSE
  )
  testthat::expect_identical(request$status, "scheduled")
  testthat::expect_identical(request$owner_stage, "B")
  testthat::expect_identical(request$model_execution, "not_started")
  testthat::expect_length(request$results, 0L)
  testthat::expect_false(isTRUE(request$recurses_to_stage_c))
  testthat::expect_identical(
    request$source_endpoint_bundle_sha256, endpoint_bundle$payload_sha256
  )
  testthat::expect_false(file.exists(file.path(output_dir, "D", "handoff-manifest.json")))
  testthat::expect_false(file.exists(file.path(output_dir, "D", "stage-handoff-provenance.json")))
  testthat::expect_false(file.exists(file.path(output_dir, "D", "stage-handoff-provenance.envelope.json")))
  testthat::expect_false(file.exists(file.path(output_dir, "D", "stage-status.json")))
  testthat::expect_false(file.exists(file.path(output_dir, "B", "block-results.json")))
  testthat::expect_false(file.exists(file.path(output_dir, "registries", "run.json")))
  c_run_record <- jsonlite::read_json(file.path(output_dir, "registries", "run-C.json"),
                                     simplifyVector = FALSE)
  testthat::expect_identical(c_run_record$stage, "C")
  testthat::expect_identical(c_run_record$status, "complete")
  testthat::expect_error(
    triomics_execute_stage_c(config_path, output_dir),
    class = "triomics_error"
  )
})

testthat::test_that("endpoint-only C refuses an existing D publication before writing C", {
  config_path <- c08_shared_fixture_path()
  output_dir <- tempfile("triomics-c08-existing-d-")
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  d_root <- file.path(output_dir, "D")
  dir.create(d_root, recursive = TRUE)
  marker <- file.path(d_root, "handoff-manifest.json")
  writeLines("stale D publication", marker, useBytes = TRUE)

  error <- testthat::capture_error(triomics_execute_stage_c(config_path, output_dir))

  testthat::expect_identical(error$code, "C_EXPORT_ENDPOINT_CONFLICT")
  testthat::expect_identical(readLines(marker, warn = FALSE), "stale D publication")
  testthat::expect_false(file.exists(file.path(output_dir, "C", "integration-results.json")))
  testthat::expect_false(file.exists(file.path(output_dir, "registries", "run-C.json")))
})

testthat::test_that("Stage C refuses an unknown source selection history", {
  config_path <- c08_shared_config_with_selection(c(block_1 = "unknown"))
  output_dir <- tempfile("triomics-c08-unknown-selection-")
  on.exit(unlink(c(config_path, output_dir), recursive = TRUE), add = TRUE)

  error <- testthat::capture_error(triomics_execute_stage_c(config_path, output_dir))

  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")
  testthat::expect_false(file.exists(file.path(output_dir, "C", "score-endpoints", "integration-01.json")))
})

testthat::test_that("Stage C propagates outcome-selected ancestry as withheld and descriptive-only", {
  config_path <- c08_shared_config_with_selection(c(block_1 = "outcome_selected"))
  output_dir <- tempfile("triomics-c08-outcome-selected-")
  on.exit(unlink(c(config_path, output_dir), recursive = TRUE), add = TRUE)

  result <- triomics_execute_stage_c(config_path, output_dir)
  endpoint_path <- file.path(
    output_dir, result$results[[1L]]$score_endpoint_bundle$payload_file
  )
  endpoint_bundle <- jsonlite::read_json(endpoint_path, simplifyVector = FALSE)
  endpoint <- endpoint_bundle$endpoints[[1L]]

  testthat::expect_identical(endpoint$selection$ancestry, "selected_after_clinical_outcome")
  testthat::expect_identical(endpoint$selection$validity, "withheld")
  testthat::expect_identical(endpoint$inference_status, "descriptive_only")
  testthat::expect_identical(endpoint$selection$source_selection$block_1$inference_gate, "withheld")
})
