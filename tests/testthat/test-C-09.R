c09_error <- function(expr, code) {
  error <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, code)
  error
}

c09_feature_rows <- function(study_id, block_id, keys) {
  data.frame(
    study_id = study_id, block_id = block_id, feature_key = keys,
    original_id = keys, id_namespace = "synthetic", source_id = block_id,
    source_row = as.character(seq_along(keys)), measured_or_derived = "measured",
    quantity = "intensity", scale = "linear", units = "synthetic_arbitrary_units",
    structural_resolution = "unknown", alias_refs = "", parent_refs = "",
    annotation_route = "none", stringsAsFactors = FALSE
  )
}

c09_study <- function(blocks, outcome, state_dir) {
  ids <- names(outcome)
  block_ids <- names(blocks)
  study_id <- "study-c09-synthetic"
  registry <- data.frame(
    sample_id = ids, subject_id = ids, synthetic_response_30d = unname(outcome),
    sample_role = "undeclared", stringsAsFactors = FALSE
  )
  raw <- list(
    study_id = study_id,
    sample_registry = registry,
    feature_registry = do.call(rbind, lapply(block_ids, function(block_id) {
      c09_feature_rows(study_id, block_id, rownames(blocks[[block_id]]))
    })),
    sample_maps = stats::setNames(lapply(block_ids, function(block_id) {
      data.frame(block_id = block_id, assay_column = ids, sample_id = ids,
                 source_column = ids, stringsAsFactors = FALSE)
    }), block_ids),
    assays = blocks,
    original_masks = lapply(blocks, function(x) {
      mask <- matrix("known_observed", nrow(x), ncol(x), dimnames = dimnames(x))
      mask[is.na(x)] <- "known_missing"
      mask
    }),
    availability = do.call(rbind, lapply(block_ids, function(block_id) {
      data.frame(
        study_id = study_id, block_id = block_id, sample_id = ids,
        presence = "present", cell_imputation_eligible = TRUE,
        stringsAsFactors = FALSE
      )
    })),
    source_manifests = list(), selection = data.frame(), column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = stats::setNames(replicate(length(block_ids), character(), FALSE), block_ids)
  )
  triomics_reset_profiles()
  triomics_register_profile("baseline", triomics_baseline_profile())
  study <- triomics_assemble_study(raw, alignment = "strict")

  operations <- lapply(c("validate", "filter", "derive", "transform", "normalise", "impute"), function(stage) {
    list(stage = stage, method = "none")
  })
  block_specs <- lapply(block_ids, function(block_id) {
    list(block_id = block_id, profile_id = "baseline", operations = operations)
  })
  list(
    study = study,
    analysis_spec = list(
      outcome = list(column = "synthetic_response_30d", type = "continuous"),
      predictive_spec = list(
        preparation_context_id = paste0("C09-predictive-", paste(block_ids, collapse = "-")),
        seed = 19L, state_dir = state_dir, blocks = block_specs
      ),
      descriptive_spec = list(
        preparation_context_id = paste0("C09-descriptive-", paste(block_ids, collapse = "-")),
        training_unit_ids = ids, seed = 19L, state_dir = state_dir, blocks = block_specs
      )
    ),
    integration_spec = list(
      method = "concat_pls", block_ids = block_ids, scale_features = TRUE,
      candidates = list(list(id = "one_component", ncomp = 1L),
                        list(id = "two_components", ncomp = 2L)),
      max_fits = 20L
    )
  )
}

c09_endpoint_request <- function(unit_ids, method = "stats::prcomp") {
  endpoint_id <- "score_c09_synthetic_pc1"
  subjects <- unit_ids
  samples <- lapply(seq_along(unit_ids), function(i) {
    list(
      sample_id = unit_ids[[i]], unit_id = unit_ids[[i]], unit_type = "visit_level",
      source_sample_ids = paste0("source-", unit_ids[[i]]),
      subject_id = subjects[[i]], subject_key = subjects[[i]],
      representation_id = "levels", value = as.numeric(i)
    )
  })
  endpoint <- list(
    artifact_type = "integration_score_endpoint", endpoint_id = endpoint_id,
    study_id = "study-c09-synthetic", analysis_id = "analysis-c09-synthetic",
    integration_id = "integration-c09-synthetic", component_id = "component_1",
    component_index = 1L, response_type = "continuous", unit = "score_unit",
    units = "score_unit", scale = "native_pca_score", orientation = "native_component_sign",
    samples = samples,
    producing_fit = list(native_method = method, training_unit_ids = unit_ids),
    training_unit_ids = unit_ids, training_ids = subjects, training_y = list(used_by_fit = FALSE),
    training_outcome = NULL,
    selection = list(ancestry = "prespecified", validity = "qualified", inspected_outcome = NULL),
    inference_status = if (identical(method, "MOFA2::run_mofa")) "exploratory" else "qualified",
    available_at_time = "V0", dependency_group = "synthetic-c09", preparation_context_id = "ctx-c09",
    representation_id = "levels", input_hashes = list(), source_hash = paste(rep("a", 64L), collapse = "")
  )
  endpoint$matrix_hash <- triomics_c08_matrix_hash(endpoint)
  endpoint$endpoint_sha256 <- triomics_c08_endpoint_hash(endpoint)
  bundle <- structure(
    list(
      artifact_type = "integration_score_endpoint_bundle",
      study_id = endpoint$study_id, analysis_id = endpoint$analysis_id,
      integration_id = endpoint$integration_id, unit_map_hash = paste(rep("b", 64L), collapse = ""),
      source_hash = endpoint$source_hash, endpoints = stats::setNames(list(endpoint), "component_1")
    ),
    class = c("IntegrationScoreEndpointBundle", "list")
  )
  bundle$payload_sha256 <- triomics_c08_bundle_hash(bundle)
  links <- lapply(seq_along(unit_ids), function(i) list(
    link_id = paste0("link-", unit_ids[[i]]),
    sample_id = paste0("source-", unit_ids[[i]]),
    subject_key = subjects[[i]], visit_id = "V0", status = "approved",
    cardinality = "one_to_one", reference_time = "2026-01-01"
  ))
  objective <- list(objectives = list(list(
    objective_id = "c09-synthetic-score-request", analysis_id = bundle$analysis_id,
    mode = "prediction", response_ref = endpoint_id, endpoint_ref = endpoint_id,
    predictor_ref = "synthetic_response_30d", time_window_ref = "V0"
  )))
  request <- triomics_schedule_clinical_score_request(
    bundle, objective,
    list(study_id = bundle$study_id, links = links)
  )
  list(bundle = bundle, request = request)
}

c09_protocol <- function(bundle_hash) {
  list(
    protocol_id = "C-09-SYNTH-30D-v1", approval_scope = "synthetic_fixture_only",
    endpoint_id = "synthetic_response_30d",
    response_model = list(
      type = "continuous", algorithm = "concat_pls", baseline = "training_intercept",
      metric_names = c("rmse", "mae", "r_squared")
    ),
    horizon = list(anchor = "V0", outcome_visit = "V30", value = 30L, unit = "day"),
    information_set = list(
      prediction_time = "V0",
      clinical_feature_ids = c("baseline_clinical_signal", "baseline_clinical_trend", "baseline_clinical_noise"),
      molecular_feature_ids = c("baseline_molecular_signal", "baseline_molecular_trend", "baseline_molecular_noise"),
      availability_rule = "available_by_prediction_time", excludes_future = TRUE,
      c08_endpoint_bundle_sha256 = bundle_hash,
      candidate_block_ids = list(
        clinical_only = "clinical", molecular_only = "molecular",
        combined = c("clinical", "molecular")
      )
    ),
    deployment = list(scope = "synthetic_research_comparison", real_deployment_status = "not_qualified"),
    uncertainty = list(
      method = "paired_subject_cluster_bootstrap", resampling_unit = "subject",
      replicates = 200L, confidence_level = 0.95, seed = 90210L
    )
  )
}

c09_minimal_evaluations <- function(split_plan, protocol) {
  fold_ids <- vapply(split_plan$folds, `[[`, character(1), "fold_id")
  predictions <- do.call(rbind, lapply(split_plan$folds, function(fold) {
    data.frame(
      fold_id = fold$fold_id, unit_id = as.character(fold$test_unit_ids),
      truth = seq_along(fold$test_unit_ids), prediction = seq_along(fold$test_unit_ids),
      status = "predicted", training_intercept = 0, stringsAsFactors = FALSE
    )
  }))
  stats::setNames(lapply(c("clinical_only", "molecular_only", "combined"), function(candidate_name) {
    outer_folds <- stats::setNames(lapply(seq_along(split_plan$folds), function(i) {
      fold <- split_plan$folds[[i]]
      inner <- lapply(fold$inner, function(inner_fold) {
        list(candidate_id = "one_component", fold_id = inner_fold$fold_id, status = "complete")
      })
      list(
        fold_id = fold$fold_id, training_unit_ids = fold$training_unit_ids,
        test_unit_ids = fold$test_unit_ids,
        inner_results = inner,
        tuning_history = list(list(candidate_id = "one_component", status = "eligible")),
        chosen_candidate = list(id = "one_component")
      )
    }), fold_ids)
    structure(
      list(
        mode = protocol$response_model$type, method = protocol$response_model$algorithm,
        split_plan_hash = split_plan$creation_hash, outer_folds = outer_folds,
        predictions = predictions
      ),
      class = "EvaluationResult"
    )
  }), c("clinical_only", "molecular_only", "combined"))
}

c09_fixture <- function(endpoint_method = "stats::prcomp", missing_outcome = FALSE) {
  unit_ids <- sprintf("unit-%02d", seq_len(32L))
  index <- seq_along(unit_ids)
  outcome <- 1.5 + index / 4 + sin(index / 3)
  names(outcome) <- unit_ids
  if (missing_outcome) outcome[[unit_ids[[1L]]]] <- NA_real_
  clinical <- rbind(
    baseline_clinical_signal = outcome + cos(index / 2),
    baseline_clinical_trend = index / 9,
    baseline_clinical_noise = sin(index)
  )
  molecular <- rbind(
    baseline_molecular_signal = outcome + sin(index / 2),
    baseline_molecular_trend = cos(index / 3),
    baseline_molecular_noise = sin(index / 4)
  )
  colnames(clinical) <- colnames(molecular) <- unit_ids
  split_plan <- triomics_nested_split_plan(
    unit_ids, subject_ids = unit_ids, n_outer = 2L, n_inner = 2L,
    seed = 90210L, target = "synthetic_response_30d"
  )
  c08 <- c09_endpoint_request(unit_ids, method = endpoint_method)
  common <- list(
    clinical = c09_study(list(clinical = clinical), outcome, tempfile("c09-state-clinical-")),
    molecular = c09_study(list(molecular = molecular), outcome, tempfile("c09-state-molecular-")),
    combined = c09_study(list(clinical = clinical, molecular = molecular), outcome,
                         tempfile("c09-state-combined-"))
  )
  list(
    protocol = c09_protocol(c08$bundle$payload_sha256), endpoint_bundle = c08$bundle,
    clinical_score_request = c08$request, split_plan = split_plan,
    candidates = stats::setNames(lapply(names(common), function(name) common[[name]]),
                                 c("clinical_only", "molecular_only", "combined")),
    unit_ids = unit_ids
  )
}

testthat::test_that("C-09 evaluates matched nested candidates and reports uncertainty and failure denominators", {
  fixture <- c09_fixture(missing_outcome = TRUE)
  result <- run_clinical_prediction_comparison(
    fixture$protocol, fixture$endpoint_bundle, fixture$clinical_score_request,
    fixture$split_plan, fixture$candidates
  )

  testthat::expect_s3_class(result, "ClinicalPredictionComparison")
  testthat::expect_identical(result$protocol$approval_scope, "synthetic_fixture_only")
  testthat::expect_identical(result$split_plan_hash, fixture$split_plan$creation_hash)
  testthat::expect_identical(names(result$candidates), c("clinical_only", "molecular_only", "combined"))
  testthat::expect_true(all(vapply(result$candidates, function(candidate) {
    candidate$denominator$total == 32L && candidate$denominator$ineligible == 1L &&
      candidate$denominator$predicted + candidate$denominator$failed + candidate$denominator$ineligible ==
        candidate$denominator$total
  }, logical(1))))
  testthat::expect_true(all(vapply(result$tested_candidate_history, function(history) {
    length(history) == 2L && all(vapply(history, function(fold) {
      candidate_ids <- vapply(fold$candidate_tuning, `[[`, character(1), "candidate_id")
      length(candidate_ids) == 2L && length(fold$inner_selection) == 4L &&
        all(vapply(fold$inner_selection, function(inner) {
          is.character(inner$status) && length(inner$status) == 1L
        }, logical(1))) &&
        (is.null(fold$chosen_candidate_id) || fold$chosen_candidate_id %in% candidate_ids)
    }, logical(1)))
  }, logical(1))))
  delta <- result$incremental$combined_vs_clinical_only$rmse
  testthat::expect_true(is.finite(delta$estimate))
  testthat::expect_true(delta$interval[[1L]] <= delta$estimate && delta$estimate <= delta$interval[[2L]])
  testthat::expect_identical(delta$uncertainty_method, "paired_subject_cluster_bootstrap_percentile_type7")
  testthat::expect_false(result$benefit_guaranteed)
  testthat::expect_identical(result$qualification$real_deployment, "not_qualified")
  testthat::expect_identical(result$qualification$scientific_validity, "not_assessed_by_synthetic_fixture")
  testthat::expect_identical(result$c08_source$request_status, "scheduled")
  testthat::expect_false(result$c08_source$recurses_to_stage_c)
})

testthat::test_that("C-09 rejects a full-cohort MOFA endpoint before invoking held-out evaluation", {
  fixture <- c09_fixture(endpoint_method = "MOFA2::run_mofa")
  fixture$candidates <- list(clinical_only = NULL, molecular_only = NULL, combined = NULL)

  error <- c09_error(run_clinical_prediction_comparison(
    fixture$protocol, fixture$endpoint_bundle, fixture$clinical_score_request,
    fixture$split_plan, fixture$candidates
  ), "C_C09_MOFA_NONINDUCTIVE")

  testthat::expect_match(conditionMessage(error), "fold-local new-subject inference")
})

testthat::test_that("C-09 refuses mismatched response or split identities before comparing metrics", {
  fixture <- c09_fixture()
  fixture$candidates$molecular_only$analysis_spec$outcome$column <- "another_response"
  error <- c09_error(run_clinical_prediction_comparison(
    fixture$protocol, fixture$endpoint_bundle, fixture$clinical_score_request,
    fixture$split_plan, fixture$candidates
  ), "C_C09_CANDIDATE_MISMATCH")

  testthat::expect_match(conditionMessage(error), "same declared response")
})

testthat::test_that("C-09 rejects corrupted C-08 requests and incomplete nested candidate history", {
  fixture <- c09_fixture()
  corrupted <- fixture$clinical_score_request
  corrupted$source_endpoint_bundle_sha256 <- paste(rep("f", 64L), collapse = "")
  c09_error(run_clinical_prediction_comparison(
    fixture$protocol, fixture$endpoint_bundle, corrupted,
    fixture$split_plan, fixture$candidates
  ), "C_C09_ENDPOINT_INVALID")

  fixture <- c09_fixture()
  evaluations <- c09_minimal_evaluations(fixture$split_plan, fixture$protocol)
  evaluations$clinical_only$outer_folds[[1L]]$inner_results <- list()
  c09_error(triomics_c09_validate_candidate_results(
    evaluations, fixture$split_plan, fixture$protocol
  ), "C_C09_NESTED_SELECTION_INVALID")
})
