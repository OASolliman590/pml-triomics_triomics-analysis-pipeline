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

c08_sha <- function(value) {
  triomics_hash_text(triomics_canonical_json(value))
}

c08_training_y_hash <- function(training_y, training_unit_ids) {
  if (is.null(training_y)) return(NULL)
  c08_sha(list(
    variable_id = training_y$variable_id,
    unit_ids = as.list(training_unit_ids),
    values = as.list(training_y$values[training_unit_ids])
  ))
}

c08_score_hash <- function(scores) {
  c08_sha(triomics_matrix_payload(scores))
}

c08_handoff <- function(fit_hash, scores, input_hashes, training_y, training_unit_ids,
                        selection, unit_map, representation_id) {
  fields <- list(
    schema_version = "C-08-fit-handoff-1",
    qualification = "same_fit_adapter_handoff",
    fit_state_sha256 = fit_hash,
    training_scores_sha256 = c08_score_hash(scores),
    training_unit_ids = as.list(training_unit_ids),
    input_hashes = input_hashes,
    training_y_sha256 = c08_training_y_hash(training_y, training_unit_ids),
    selection_sha256 = c08_sha(selection),
    unit_map_sha256 = c08_sha(unit_map),
    representation_id = representation_id
  )
  fields$handoff_sha256 <- c08_sha(fields)
  fields
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
  input_hashes <- stats::setNames(
    as.list(c(paste(rep("b", 64L), collapse = ""), paste(rep("c", 64L), collapse = ""))),
    c("alpha", "beta")
  )
  training_y <- if (method_spec$uses_y) list(
      variable_id = "clinical_group",
      values = stats::setNames(c("control", "control", "case", "case"), units)
    ) else NULL
  unit_map <- data.frame(
    unit_id = units,
    unit_type = rep("visit_level", length(units)),
    subject_id = as.character(subjects),
    representation_id = rep("levels", length(units)),
    eligibility = rep("retained", length(units)),
    stringsAsFactors = FALSE
  )
  unit_map$source_sample_ids <- I(lapply(units, function(unit) paste0("sample-", unit)))

  native_state <- NULL
  result_scores <- scores
  result_params <- switch(method,
    concat_pca = list(
      method = "concat_pca",
      state = list(
        state_version = "C-01-pca-1", training_unit_ids = units,
        preparation_context_id = "ctx-c08", representation_id = "levels"
      )
    ),
    mixomics_diablo = list(
      native_method = "mixOmics::block.splsda", native_version = "6.36.0",
      training_unit_ids = units, training_state_hash = fit_hash,
      score_definition = "consensus_mean"
    ),
    mixomics_block_spls = list(
      native_method = "mixOmics::block.spls", native_version = "6.36.0",
      task = "continuous_regression", training_unit_ids = units,
      training_state_hash = fit_hash,
      score_definition = "equal_weight_mean_of_signed_training_standardized_native_scores"
    ),
    mofa2 = list(
      native_method = "MOFA2::run_mofa", native_version = "1.18.0",
      training_unit_ids = units, training_state_hash = fit_hash,
      score_definition = "native_mofa_factor_values",
      training_input_hashes = input_hashes
    )
  )
  if (method %in% c("mixomics_diablo", "mixomics_block_spls")) {
    native_scores <- list(
      alpha = scores,
      beta = sweep(scores, 2L, c(1, 2), `+`)
    )
    native_loadings <- loadings
    consensus_state <- make_consensus_state(
      native_scores, native_loadings, units,
      list(required_views = names(native_scores), score_definition = method_spec$score_definition)
    )
    result_scores <- apply_consensus_state(native_scores, consensus_state)
    native_state <- if (identical(method, "mixomics_diablo")) list(
      block_ids = names(native_scores), training_unit_ids = units,
      class_levels = c("control", "case"), components = colnames(scores),
      native_scores = native_scores, native_loadings = native_loadings,
      consensus_state = consensus_state
    ) else list(
      block_ids = names(native_scores), unit_ids = units,
      component_ids = colnames(scores), training_scores = native_scores,
      block_loadings = native_loadings, selected = lapply(native_loadings, rownames),
      consensus_state = consensus_state
    )
  } else if (identical(method, "concat_pca")) {
    native_state <- list(
      state = result_params$state, training_scores = scores,
      block_loadings = loadings, block_projection_fraction = variance
    )
  } else {
    native_state <- list(
      block_ids = names(loadings), training_unit_ids = units,
      components = colnames(scores), factors = scores
    )
  }
  result_params$provenance <- list(
    study_id = "study-c08-synthetic", analysis_id = "analysis-c08-synthetic",
    preparation_context_id = "ctx-c08", representation_id = "levels",
    unit_map = units, matrix_sha256 = input_hashes,
    source_selection = stats::setNames(lapply(names(input_hashes), function(block_id) {
      list(status = "supplied", value = "full_unselected")
    }), names(input_hashes)),
    outcome_eligibility = list(status = if (is.null(training_y)) "not_applicable" else "supplied")
  )
  result <- triomics_new_integration_result(
    scores = result_scores, loadings = loadings, variance = variance,
    selected = lapply(loadings, rownames), backend = method, params = result_params
  )
  result_params <- result$params
  handoff <- c08_handoff(fit_hash, result_scores, input_hashes, training_y, units,
                         selection, unit_map, "levels")
  fit_metadata <- switch(method,
    concat_pca = list(
      backend = method, backend_version = R.version.string,
      model_role = "unsupervised_predictor_structure",
      training_unit_ids = units, preparation_context_id = "ctx-c08"
    ),
    mixomics_diablo = list(
      backend = "mixomics_diablo", native_method = "mixOmics::block.splsda",
      native_version = "6.36.0", model_role = "supervised_prediction",
      training_unit_ids = units, preparation_context_id = "ctx-c08", state_hash = fit_hash
    ),
    mixomics_block_spls = list(
      backend = "mixomics_block_spls", native_method = "mixOmics::block.spls",
      native_version = "6.36.0", model_role = "continuous_regression_prediction",
      training_unit_ids = units, preparation_context_id = "ctx-c08", state_hash = fit_hash
    ),
    mofa2 = list(
      backend = "mofa2", native_method = "MOFA2::run_mofa",
      native_version = "1.18.0", python_backend_version = "1.2.0",
      model_role = "exploratory_structure_discovery", training_unit_ids = units,
      state_hash = fit_hash, training_input_hashes = input_hashes
    )
  )
  fit_metadata$c08_handoff <- handoff
  fit_result_params <- result_params
  fit_result_params$provenance <- NULL
  fit <- structure(
    list(native_state = native_state, result_params = fit_result_params, metadata = fit_metadata),
    class = "BackendFit"
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
  links <- lapply(seq_along(units), function(i) list(
    link_id = paste0("link-", units[[i]]),
    sample_id = paste0("sample-", units[[i]]),
    subject_key = as.character(subjects[[i]]),
    visit_id = as.character(visits[[i]]),
    status = "approved", cardinality = "one_to_one",
    reference_time = as.character(times[[i]])
  ))
  list(result = result, fit = fit, context = context, scores = result_scores,
       unit_map = unit_map, links = links, method_spec = method_spec, fit_hash = fit_hash)
}

c08_bundle <- function(method = "mixomics_diablo", ...) {
  input <- c08_inputs(method, ...)
  bundle <- triomics_integration_score_endpoints(input$result, input$fit, input$context)
  list(input = input, bundle = bundle)
}

c08_clinical_view <- function(unit_map, links = NULL) {
  if (!is.null(links)) return(list(study_id = "study-c08-synthetic", links = links))
  list(
    study_id = "study-c08-synthetic",
    links = lapply(seq_len(nrow(unit_map)), function(i) list(
      link_id = paste0("link-", unit_map$unit_id[[i]]),
      sample_id = unit_map$source_sample_ids[[i]][[1L]],
      subject_key = unit_map$subject_id[[i]],
      visit_id = "V1",
      status = "approved",
      cardinality = "one_to_one",
      reference_time = "2020-01-01"
    ))
  )
}

c08_set_representation <- function(input, representation = c("mean", "delta")) {
  representation <- match.arg(representation)
  units <- input$context$unit_map$unit_id
  source_ids <- lapply(units, function(unit) paste0("sample-", unit, "-", c("V0", "V1")))
  unit_map <- data.frame(
    unit_id = units,
    unit_type = if (identical(representation, "mean")) "subject_mean" else "subject_delta",
    subject_id = paste0("subject-", units),
    representation_id = representation,
    eligibility = "retained",
    stringsAsFactors = FALSE
  )
  unit_map$source_sample_ids <- I(source_ids)
  weights <- if (identical(representation, "mean")) {
    stats::setNames(c(.5, .5), c("V0", "V1"))
  } else {
    stats::setNames(c(-1, 1), c("V0", "V1"))
  }
  unit_map$visit_weights <- I(rep(list(weights), length(units)))
  input$context$unit_map <- unit_map
  input$context$representation_id <- representation
  input$result$params$provenance$representation_id <- representation
  input$links <- unlist(lapply(seq_along(units), function(i) list(
    list(
      link_id = paste0("link-", units[[i]], "-V0"), sample_id = source_ids[[i]][[1L]],
      subject_key = unit_map$subject_id[[i]], visit_id = "V0", status = "approved",
      cardinality = "one_to_one", reference_time = "2020-01-01"
    ),
    list(
      link_id = paste0("link-", units[[i]], "-V1"), sample_id = source_ids[[i]][[2L]],
      subject_key = unit_map$subject_id[[i]], visit_id = "V1", status = "approved",
      cardinality = "one_to_one", reference_time = "2020-01-31"
    )
  )), recursive = FALSE)
  handoff <- input$fit$metadata$c08_handoff
  handoff$unit_map_sha256 <- c08_sha(unit_map)
  handoff$representation_id <- representation
  handoff$handoff_sha256 <- NULL
  handoff$handoff_sha256 <- c08_sha(handoff)
  input$fit$metadata$c08_handoff <- handoff
  input
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
    if (spec$uses_y) {
      testthat::expect_match(endpoint$training_y$values_sha256, "^[0-9a-f]{64}$")
      testthat::expect_identical(endpoint$training_y$variable_id, "clinical_group")
    } else {
      testthat::expect_null(endpoint$training_y$values_sha256)
      testthat::expect_null(endpoint$training_y$variable_id)
    }
    testthat::expect_identical(endpoint$training_y$used_by_fit, spec$uses_y)
    testthat::expect_identical(endpoint$training_outcome,
                               if (spec$uses_y) "clinical_group" else NULL)
    testthat::expect_identical(endpoint$samples[[3L]]$unit_id, "u3")
    testthat::expect_equal(endpoint$samples[[3L]]$value,
                           fixture$input$scores["u3", "component_1"])
    testthat::expect_identical(endpoint$samples[[3L]]$unit_type, "visit_level")
    testthat::expect_identical(endpoint$samples[[3L]]$source_sample_ids, "sample-u3")
    testthat::expect_identical(endpoint$samples[[3L]]$subject_id, "subject-u3")
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
    c08_clinical_view(fixture$input$unit_map, fixture$input$links)
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
      c08_clinical_view(fixture$input$unit_map, fixture$input$links)
    ),
    "C_SCORE_ENDPOINT_HASH"
  )

  changed_lineage <- fixture$bundle
  changed_lineage$endpoints[[2L]]$samples[[1L]]$source_sample_ids <- "unrelated-source"
  changed_lineage$endpoints[[2L]]$endpoint_sha256 <-
    triomics_c08_endpoint_hash(changed_lineage$endpoints[[2L]])
  changed_lineage$payload_sha256 <- triomics_c08_bundle_hash(changed_lineage)
  c08_abort(
    triomics_schedule_clinical_score_request(
      changed_lineage, c08_objective(changed_lineage$endpoints[[2L]]$endpoint_id),
      c08_clinical_view(fixture$input$unit_map, fixture$input$links)
    ),
    "C_SCORE_COVERAGE_MISMATCH"
  )
})

testthat::test_that("C-08-AC3 keeps DIABLO training-Y and selected MOFA associations descriptive", {
  diablo <- c08_bundle("mixomics_diablo")
  diablo_endpoint <- diablo$bundle$endpoints[[1L]]
  diablo_request <- triomics_schedule_clinical_score_request(
    diablo$bundle, c08_objective(diablo_endpoint$endpoint_id),
    c08_clinical_view(diablo$input$unit_map, diablo$input$links)
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
    c08_clinical_view(mofa$input$unit_map, mofa$input$links)
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
  view <- c08_clinical_view(fixture$input$unit_map, fixture$input$links)
  view$links <- view$links[-1L]
  c08_abort(
    triomics_schedule_clinical_score_request(
      fixture$bundle, c08_objective(endpoint$endpoint_id), view
    ),
    "C_SCORE_COVERAGE_MISMATCH"
  )

  fixture <- c08_bundle("concat_pca")
  endpoint <- fixture$bundle$endpoints[[1L]]
  view <- c08_clinical_view(fixture$input$unit_map, fixture$input$links)
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
      fixture$bundle, objective,
      c08_clinical_view(fixture$input$unit_map, fixture$input$links)
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
      c08_clinical_view(fixture$input$unit_map, fixture$input$links)
    ),
    "C_SCORE_REQUEST_INVALID"
  )
})

testthat::test_that("C-08 probes accepted adapter shapes and refuses absent handoff identity", {
  pca <- c08_inputs("concat_pca")
  testthat::expect_identical(pca$fit$metadata$backend, "concat_pca")
  testthat::expect_null(pca$fit$metadata$state_hash)
  testthat::expect_identical(pca$fit$native_state$training_scores, pca$result$scores)

  diablo <- c08_inputs("mixomics_diablo")
  testthat::expect_null(diablo$fit$result_params$method)
  testthat::expect_identical(diablo$fit$metadata$native_method, "mixOmics::block.splsda")
  testthat::expect_true(inherits(diablo$fit$native_state$consensus_state, "ConsensusState"))
  testthat::expect_equal(
    apply_consensus_state(diablo$fit$native_state$native_scores,
                          diablo$fit$native_state$consensus_state),
    diablo$result$scores
  )

  block_spls <- c08_inputs("mixomics_block_spls")
  testthat::expect_null(block_spls$fit$result_params$method)
  testthat::expect_identical(block_spls$fit$metadata$native_method, "mixOmics::block.spls")
  testthat::expect_true(inherits(block_spls$fit$native_state$consensus_state, "ConsensusState"))
  testthat::expect_equal(
    apply_consensus_state(block_spls$fit$native_state$training_scores,
                          block_spls$fit$native_state$consensus_state),
    block_spls$result$scores
  )

  mofa <- c08_inputs("mofa2")
  testthat::expect_null(mofa$fit$result_params$method)
  testthat::expect_null(mofa$fit$metadata$preparation_context_id)
  testthat::expect_identical(mofa$fit$metadata$native_method, "MOFA2::run_mofa")
  testthat::expect_identical(mofa$fit$native_state$factors, mofa$result$scores)

  for (fixture in list(pca, diablo, block_spls, mofa)) {
    fixture$fit$metadata$c08_handoff <- NULL
    error <- testthat::capture_error(triomics_integration_score_endpoints(
      fixture$result, fixture$fit, fixture$context
    ))
    testthat::expect_s3_class(error, "triomics_error")
    testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")
    testthat::expect_match(conditionMessage(error), "handoff|state identity")
  }
})

testthat::test_that("C-08 binds scores, source inputs, Y, selection and AnalysisUnitMap to the fit handoff", {
  fixture <- c08_inputs("mofa2")
  changed_scores <- fixture
  changed_scores$result$scores[1L, 1L] <- changed_scores$result$scores[1L, 1L] + 1
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    changed_scores$result, changed_scores$fit, changed_scores$context
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_FIT_BINDING_MISMATCH")

  changed_inputs <- fixture
  changed_inputs$context$input_hashes$alpha <- paste(rep("d", 64L), collapse = "")
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    changed_inputs$result, changed_inputs$fit, changed_inputs$context
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_FIT_BINDING_MISMATCH")

  supervised <- c08_inputs("mixomics_diablo")
  supervised$context$training_y$values[["u4"]] <- "control"
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    supervised$result, supervised$fit, supervised$context
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_FIT_BINDING_MISMATCH")

  changed_selection <- fixture
  changed_selection$context$selection$ancestry <- "selected_after_clinical_outcome"
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    changed_selection$result, changed_selection$fit, changed_selection$context
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_FIT_BINDING_MISMATCH")

  changed_map <- fixture
  changed_map$context$unit_map$source_sample_ids[[1L]] <- "unrelated-sample"
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    changed_map$result, changed_map$fit, changed_map$context
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_FIT_BINDING_MISMATCH")
})

testthat::test_that("C-08 preserves mean and delta AnalysisUnitMap lineage and rejects incompatible times", {
  mean_input <- c08_set_representation(c08_inputs("mofa2"), "mean")
  mean_endpoint <- triomics_integration_score_endpoints(
    mean_input$result, mean_input$fit, mean_input$context
  )$endpoints[[1L]]
  testthat::expect_identical(mean_endpoint$representation_id, "mean")
  testthat::expect_identical(mean_endpoint$samples[[1L]]$unit_type, "subject_mean")
  testthat::expect_identical(mean_endpoint$samples[[1L]]$source_sample_ids,
                             c("sample-u1-V0", "sample-u1-V1"))
  testthat::expect_identical(mean_endpoint$samples[[1L]]$visit_weights,
                             stats::setNames(c(.5, .5), c("V0", "V1")))
  mean_bundle <- triomics_integration_score_endpoints(
    mean_input$result, mean_input$fit, mean_input$context
  )
  mean_objective <- c08_objective(mean_bundle$endpoints[[1L]]$endpoint_id, visit = "V0")
  error <- testthat::capture_error(triomics_schedule_clinical_score_request(
    mean_bundle, mean_objective,
    c08_clinical_view(mean_input$context$unit_map, mean_input$links)
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_TIME_MISMATCH")

  delta_input <- c08_set_representation(c08_inputs("mofa2"), "delta")
  delta_bundle <- triomics_integration_score_endpoints(
    delta_input$result, delta_input$fit, delta_input$context
  )
  delta_endpoint <- delta_bundle$endpoints[[1L]]
  testthat::expect_identical(delta_endpoint$representation_id, "delta")
  testthat::expect_identical(delta_endpoint$samples[[1L]]$unit_type, "subject_delta")
  testthat::expect_identical(delta_endpoint$samples[[1L]]$source_sample_ids,
                             c("sample-u1-V0", "sample-u1-V1"))
  testthat::expect_identical(delta_endpoint$samples[[1L]]$visit_weights,
                             stats::setNames(c(-1, 1), c("V0", "V1")))
  delta_objective <- c08_objective(delta_endpoint$endpoint_id, mode = "baseline_to_later", visit = "V0")
  delta_objective$objectives[[1L]]$baseline_visit_ref <- "V0"
  delta_objective$objectives[[1L]]$later_visit_ref <- "V1"
  delta_objective$objectives[[1L]]$horizon_days <- 30L
  testthat::expect_s3_class(triomics_schedule_clinical_score_request(
    delta_bundle, delta_objective,
    c08_clinical_view(delta_input$context$unit_map, delta_input$links)
  ), "ClinicalScoreAssociationRequest")

  reversed_links <- delta_input$links
  reversed_links[[1L]]$visit_id <- "V1"
  reversed_links[[2L]]$visit_id <- "V0"
  error <- testthat::capture_error(triomics_schedule_clinical_score_request(
    delta_bundle, delta_objective,
    c08_clinical_view(delta_input$context$unit_map, reversed_links)
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_TIME_MISMATCH")

  invalid <- c08_inputs("mofa2")
  invalid$context$unit_map$unit_type[[1L]] <- "invented_unit"
  error <- testthat::capture_error(triomics_integration_score_endpoints(
    invalid$result, invalid$fit, invalid$context
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_SCORE_LINEAGE_INVALID")

  with_exclusion <- c08_inputs("mofa2")
  records <- lapply(seq_len(nrow(with_exclusion$context$unit_map)), function(i) {
    lapply(names(with_exclusion$context$unit_map), function(name) {
      with_exclusion$context$unit_map[[name]][[i]]
    })
  })
  for (i in seq_along(records)) names(records[[i]]) <- names(with_exclusion$context$unit_map)
  records[[5L]] <- list(
    unit_id = "u5", unit_type = "visit_level", source_sample_ids = "sample-u5",
    subject_id = "subject-u5", representation_id = "levels", eligibility = "excluded",
    exclusion_reason = "not_present_in_producing_fit"
  )
  with_exclusion$context$unit_map <- records
  handoff <- with_exclusion$fit$metadata$c08_handoff
  handoff$unit_map_sha256 <- c08_sha(records)
  handoff$handoff_sha256 <- NULL
  handoff$handoff_sha256 <- c08_sha(handoff)
  with_exclusion$fit$metadata$c08_handoff <- handoff
  excluded_bundle <- triomics_integration_score_endpoints(
    with_exclusion$result, with_exclusion$fit, with_exclusion$context
  )
  testthat::expect_length(excluded_bundle$endpoints[[1L]]$samples, 4L)
  testthat::expect_identical(excluded_bundle$endpoints[[1L]]$unit_map$eligibility[[5L]], "excluded")
  testthat::expect_identical(excluded_bundle$endpoints[[1L]]$unit_map$exclusion_reason[[5L]],
                             "not_present_in_producing_fit")
})
