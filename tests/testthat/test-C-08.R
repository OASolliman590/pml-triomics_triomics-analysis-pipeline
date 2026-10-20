c08_abort <- function(expr, code) {
  error <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, code)
  error
}

c08_methods <- function() {
  list(
    concat_pca = list(
      native_method = "stats::prcomp",
      score_definition = "native_pca_scores",
      scale = "native_pca_score",
      orientation = "native_component_sign",
      uses_y = FALSE
    ),
    mixomics_diablo = list(
      native_method = "mixOmics::block.splsda",
      score_definition = "consensus_mean",
      scale = "training_standardized_native_score_mean",
      orientation = "frozen_consensus_training_anchor",
      uses_y = TRUE
    ),
    mixomics_block_spls = list(
      native_method = "mixOmics::block.spls",
      score_definition = "equal_weight_mean_of_signed_training_standardized_native_scores",
      scale = "training_standardized_native_score_mean",
      orientation = "frozen_consensus_training_anchor",
      uses_y = TRUE
    ),
    mofa2 = list(
      native_method = "MOFA2::run_mofa",
      score_definition = "native_mofa_factor_values",
      scale = "native_mofa_factor_value",
      orientation = "native_factor_sign_unaligned",
      uses_y = FALSE
    )
  )
}

c08_inputs <- function(method = "mixomics_diablo", visits = rep("V1", 4L),
                       times = rep("2020-01-01", 4L),
                       subjects = paste0("subject-u", seq_len(4L)),
                       selection = list(ancestry = "prespecified", validity = "qualified",
                                        inspected_outcome = NULL)) {
  units <- paste0("u", seq_len(4L))
  method_spec <- c08_methods()[[method]]
  scores <- matrix(
    c(1, 10, 2, 20, 3, 30, 4, 40), nrow = 4L, byrow = TRUE,
    dimnames = list(units, c("component_1", "component_2"))
  )
  loadings <- stats::setNames(list(
    matrix(c(.6, .8, .8, .6), nrow = 2L,
           dimnames = list(c("feature_a", "feature_b"), colnames(scores))),
    matrix(c(.5, .5, .6, .8), nrow = 2L,
           dimnames = list(c("feature_c", "feature_d"), colnames(scores)))
  ), c("alpha", "beta"))
  variance <- matrix(
    c(.2, .3, .4, .5), nrow = 2L,
    dimnames = list(c("alpha", "beta"), colnames(scores))
  )
  fit_hash <- paste(rep("a", 64L), collapse = "")
  result_params <- list(
    method = method,
    native_method = method_spec$native_method,
    training_unit_ids = units,
    training_state_hash = fit_hash,
    score_definition = method_spec$score_definition,
    versions = list(R = "4.5.1")
  )
  if (identical(method, "concat_pca")) {
    result_params$state <- list(
      state_version = "C-01-pca-1", training_unit_ids = units,
      preparation_context_id = "ctx-c08"
    )
    result_params$training_unit_ids <- NULL
    result_params$training_state_hash <- NULL
    result_params$score_definition <- NULL
  }
  result <- triomics_new_integration_result(
    scores = scores, loadings = loadings, variance = variance,
    selected = lapply(loadings, rownames), backend = method, params = result_params
  )
  result_hash <- if (identical(method, "concat_pca")) NULL else fit_hash
  fit_metadata <- list(
    backend = method,
    native_method = method_spec$native_method,
    native_version = "fixture-native-1",
    model_role = if (method_spec$uses_y) "supervised_prediction" else "exploratory_structure_discovery",
    training_unit_ids = units,
    state_hash = result_hash,
    preparation_context_id = "ctx-c08"
  )
  fit <- structure(list(result_params = result_params, metadata = fit_metadata), class = "BackendFit")
  unit_map <- data.frame(
    unit_id = units,
    sample_id = paste0("sample-", units),
    subject_key = as.character(subjects),
    unit_type = rep("sample_visit", length(units)),
    visit_id = as.character(visits),
    time = as.character(times),
    stringsAsFactors = FALSE
  )
  input_hashes <- stats::setNames(
    as.list(c(paste(rep("b", 64L), collapse = ""), paste(rep("c", 64L), collapse = ""))),
    c("alpha", "beta")
  )
  training_y <- list(
    variable_id = "clinical_group",
    values = stats::setNames(c("control", "control", "case", "case"), units)
  )
  context <- list(
    study_id = "study-c08-synthetic",
    analysis_id = "analysis-c08-synthetic",
    integration_id = paste0("fit-", method),
    preparation_context_id = "ctx-c08",
    representation_id = "levels",
    unit_map = unit_map,
    input_hashes = input_hashes,
    training_y = training_y,
    selection = selection,
    available_at_time = "V1",
    dependency_group = paste0("dependency-", method)
  )
  list(result = result, fit = fit, context = context, scores = scores,
       unit_map = unit_map, method_spec = method_spec, fit_hash = fit_hash)
}

c08_bundle <- function(method = "mixomics_diablo", ...) {
  input <- c08_inputs(method, ...)
  bundle <- triomics_integration_score_endpoints(input$result, input$fit, input$context)
  list(input = input, bundle = bundle)
}

c08_clinical_view <- function(unit_map) {
  list(
    study_id = "study-c08-synthetic",
    links = lapply(seq_len(nrow(unit_map)), function(i) list(
      link_id = paste0("link-", unit_map$unit_id[[i]]),
      sample_id = unit_map$sample_id[[i]],
      subject_key = unit_map$subject_key[[i]],
      visit_id = unit_map$visit_id[[i]],
      status = "approved",
      cardinality = "one_to_one",
      reference_time = unit_map$time[[i]]
    ))
  )
}

c08_objective <- function(endpoint_id, mode = "between_person", visit = "V1") {
  list(
    objectives = list(list(
      objective_id = "score_glucose",
      objective_version = "1",
      analysis_id = "analysis-c08-synthetic",
      mode = mode,
      response_ref = endpoint_id,
      predictor_ref = "glucose",
      endpoint_ref = endpoint_id,
      adjustments = list(),
      time_window_ref = visit,
      missingness_policy = "complete_case",
      family_ref = "score_family",
      hypothesis_status = "exploratory",
      protocol_ref = "synthetic-c08"
    )),
    family = list(
      family_id = "score_family",
      member_ids = endpoint_id,
      claim_scope = "synthetic_score_association"
    ),
    trend = FALSE,
    robust = FALSE,
    fallback = "none",
    imputation = "none"
  )
}

testthat::test_that("C-08-AC1 emits per-unit lineage for PCA, DIABLO, block.spls and MOFA", {
  expected <- c08_methods()
  for (method in names(expected)) {
    fixture <- c08_bundle(method)
    endpoint <- fixture$bundle$endpoints[[1L]]
    spec <- expected[[method]]

    testthat::expect_length(fixture$bundle$endpoints, 2L)
    testthat::expect_identical(endpoint$response_type, "continuous")
    testthat::expect_identical(endpoint$unit, "score_unit")
    testthat::expect_identical(endpoint$units, "score_unit")
    testthat::expect_identical(endpoint$scale, spec$scale)
    testthat::expect_identical(endpoint$orientation, spec$orientation)
    testthat::expect_identical(endpoint$producing_fit$native_method, spec$native_method)
    testthat::expect_identical(endpoint$producing_fit$fit_id,
                               paste0("fit-", substr(endpoint$producing_fit$state_sha256, 1L, 16L)))
    testthat::expect_identical(endpoint$training_unit_ids, paste0("u", 1:4))
    testthat::expect_identical(endpoint$training_ids, paste0("subject-u", 1:4))
    testthat::expect_identical(endpoint$training_y$training_unit_ids, paste0("u", 1:4))
    testthat::expect_identical(endpoint$training_y$training_subjects, paste0("subject-u", 1:4))
    testthat::expect_match(endpoint$training_y$values_sha256, "^[0-9a-f]{64}$")
    testthat::expect_identical(endpoint$training_y$variable_id, "clinical_group")
    testthat::expect_identical(endpoint$training_y$used_by_fit, spec$uses_y)
    testthat::expect_identical(endpoint$training_outcome,
                               if (spec$uses_y) "clinical_group" else NULL)
    testthat::expect_identical(endpoint$samples[[3L]]$unit_id, "u3")
    testthat::expect_equal(endpoint$samples[[3L]]$value,
                           fixture$input$scores["u3", "component_1"])
    testthat::expect_identical(endpoint$samples[[3L]]$unit_type, "sample_visit")
    testthat::expect_identical(endpoint$samples[[3L]]$subject_key, "subject-u3")
    testthat::expect_identical(endpoint$samples[[3L]]$visit_id, "V1")
    testthat::expect_identical(names(endpoint$input_hashes), c("alpha", "beta"))
    testthat::expect_identical(endpoint$available_at_time, "V1")
    testthat::expect_identical(endpoint$dependency_group, paste0("dependency-", method))
    testthat::expect_match(endpoint$matrix_hash, "^[0-9a-f]{64}$")
    testthat::expect_identical(endpoint$matrix_hash, triomics_b08_endpoint_hash(endpoint))
    testthat::expect_match(endpoint$source_hash, "^[0-9a-f]{64}$")
    testthat::expect_identical(endpoint$selection$ancestry, "prespecified")
  }
})

testthat::test_that("C-08-AC1 records unsupervised training subjects when no training Y exists", {
  fixture <- c08_inputs("concat_pca")
  fixture$context$training_y <- NULL
  endpoint <- triomics_integration_score_endpoints(
    fixture$result, fixture$fit, fixture$context
  )$endpoints[[1L]]

  testthat::expect_null(endpoint$training_outcome)
  testthat::expect_null(endpoint$training_y$variable_id)
  testthat::expect_false(endpoint$training_y$used_by_fit)
  testthat::expect_identical(endpoint$training_y$training_unit_ids, paste0("u", 1:4))
  testthat::expect_identical(endpoint$training_y$training_subjects, paste0("subject-u", 1:4))
  testthat::expect_null(endpoint$training_y$values_sha256)
})

testthat::test_that("C-08-AC1 rejects fit, training-unit and score-map lineage disagreement", {
  fixture <- c08_inputs("mixomics_diablo")
  fixture$fit$metadata$training_unit_ids[[4L]] <- "different-unit"
  c08_abort(
    triomics_integration_score_endpoints(fixture$result, fixture$fit, fixture$context),
    "C_SCORE_LINEAGE_INVALID"
  )

  fixture <- c08_inputs("mixomics_diablo")
  fixture$context$unit_map <- fixture$context$unit_map[-1L, , drop = FALSE]
  c08_abort(
    triomics_integration_score_endpoints(fixture$result, fixture$fit, fixture$context),
    "C_SCORE_COVERAGE_MISMATCH"
  )
})

testthat::test_that("C-08-AC2 schedules an immutable B-owned score request without model results", {
  fixture <- c08_bundle("concat_pca")
  endpoint <- fixture$bundle$endpoints[[1L]]
  request <- triomics_schedule_clinical_score_request(
    fixture$bundle,
    c08_objective(endpoint$endpoint_id),
    c08_clinical_view(fixture$input$unit_map)
  )

  testthat::expect_s3_class(request, "ClinicalScoreAssociationRequest")
  testthat::expect_identical(request$request_type, "clinical_score_association")
  testthat::expect_identical(request$owner_stage, "B")
  testthat::expect_identical(request$status, "scheduled")
  testthat::expect_identical(request$model_execution, "not_started")
  testthat::expect_identical(request$endpoint_bundle_sha256, fixture$bundle$payload_sha256)
  testthat::expect_match(request$request_sha256, "^[0-9a-f]{64}$")
  testthat::expect_match(request$clinical_linkage$links_sha256, "^[0-9a-f]{64}$")
  testthat::expect_identical(request$objective_spec$objectives[[1L]]$endpoint_ref,
                             endpoint$endpoint_id)
  testthat::expect_length(request$results, 0L)
  testthat::expect_false(isTRUE(request$recurses_to_stage_c))

  changed <- fixture$bundle
  changed$endpoints[[1L]]$samples[[1L]]$value <- -999
  c08_abort(
    triomics_schedule_clinical_score_request(
      changed, c08_objective(endpoint$endpoint_id),
      c08_clinical_view(fixture$input$unit_map)
    ),
    "C_SCORE_ENDPOINT_HASH"
  )
})

testthat::test_that("C-08-AC3 keeps DIABLO training-Y and selected MOFA associations descriptive", {
  diablo <- c08_bundle("mixomics_diablo")
  diablo_endpoint <- diablo$bundle$endpoints[[1L]]
  diablo_request <- triomics_schedule_clinical_score_request(
    diablo$bundle, c08_objective(diablo_endpoint$endpoint_id),
    c08_clinical_view(diablo$input$unit_map)
  )
  testthat::expect_identical(diablo_request$endpoint_bundle$endpoints[[1L]]$training_outcome,
                             "clinical_group")
  testthat::expect_identical(diablo_request$endpoint_bundle$endpoints[[1L]]$inference_status,
                             "descriptive_only")

  selection <- list(
    ancestry = "selected_after_clinical_outcome",
    validity = "descriptive_only",
    inspected_outcome = "glucose"
  )
  mofa <- c08_bundle("mofa2", selection = selection)
  mofa_endpoint <- mofa$bundle$endpoints[[1L]]
  mofa_request <- triomics_schedule_clinical_score_request(
    mofa$bundle, c08_objective(mofa_endpoint$endpoint_id),
    c08_clinical_view(mofa$input$unit_map)
  )
  scheduled <- mofa_request$endpoint_bundle$endpoints[[1L]]
  testthat::expect_identical(scheduled$training_outcome, NULL)
  testthat::expect_identical(scheduled$selection$ancestry,
                             "selected_after_clinical_outcome")
  testthat::expect_identical(scheduled$selection$inspected_outcome, "glucose")
  testthat::expect_identical(scheduled$inference_status, "descriptive_only")
})

testthat::test_that("C-08-AC4 refuses score-to-clinical unit or visit mismatches", {
  fixture <- c08_bundle("concat_pca")
  endpoint <- fixture$bundle$endpoints[[1L]]
  view <- c08_clinical_view(fixture$input$unit_map)
  view$links <- view$links[-1L]
  c08_abort(
    triomics_schedule_clinical_score_request(
      fixture$bundle, c08_objective(endpoint$endpoint_id), view
    ),
    "C_SCORE_COVERAGE_MISMATCH"
  )

  fixture <- c08_bundle("concat_pca")
  endpoint <- fixture$bundle$endpoints[[1L]]
  view <- c08_clinical_view(fixture$input$unit_map)
  view$links[[1L]]$visit_id <- "V2"
  c08_abort(
    triomics_schedule_clinical_score_request(
      fixture$bundle, c08_objective(endpoint$endpoint_id), view
    ),
    "C_SCORE_TIME_MISMATCH"
  )
})

testthat::test_that("C-08-AC4 refuses repeated-visit time horizons that do not match the objective", {
  visits <- c("V0", "V1", "V0", "V1")
  times <- c("2020-01-01", "2020-01-31", "2020-01-01", "2020-02-01")
  fixture <- c08_bundle("mofa2", visits = visits, times = times,
                        subjects = c("subject-a", "subject-a", "subject-b", "subject-b"))
  endpoint <- fixture$bundle$endpoints[[1L]]
  objective <- c08_objective(endpoint$endpoint_id, mode = "baseline_to_later", visit = "V0")
  objective$objectives[[1L]]$baseline_visit_ref <- "V0"
  objective$objectives[[1L]]$later_visit_ref <- "V1"
  objective$objectives[[1L]]$horizon_days <- 30L
  objective$objectives[[1L]]$outcome_estimand <- "later_level"
  objective$objectives[[1L]]$baseline_adjustment <- FALSE
  c08_abort(
    triomics_schedule_clinical_score_request(
      fixture$bundle, objective, c08_clinical_view(fixture$input$unit_map)
    ),
    "C_SCORE_TIME_MISMATCH"
  )
})

testthat::test_that("C-08-AC4 rejects feature-association artifacts at the score-request seam", {
  fixture <- c08_bundle("concat_pca")
  feature_results <- list(
    artifact_type = "block_results",
    study_id = fixture$bundle$study_id,
    analysis_id = fixture$bundle$analysis_id,
    results = list(list(feature_key = "feature_a", effect = 2))
  )
  c08_abort(
    triomics_schedule_clinical_score_request(
      feature_results,
      c08_objective(fixture$bundle$endpoints[[1L]]$endpoint_id),
      c08_clinical_view(fixture$input$unit_map)
    ),
    "C_SCORE_REQUEST_INVALID"
  )
})
