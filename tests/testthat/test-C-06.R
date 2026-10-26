c06_ids <- c("u1", "u2", "u3")
c06_blocks <- c("proteome", "metabolome")
c06_features <- list(
  proteome = c("p1", "p2"),
  metabolome = c("m1", "m2")
)
c06_hashes <- list(
  proteome = paste(rep("a", 64), collapse = ""),
  metabolome = paste(rep("b", 64), collapse = "")
)

c06_matrix <- function(values, rows, columns) {
  matrix(values, nrow = length(rows), ncol = length(columns),
         dimnames = list(rows, columns))
}

c06_run <- function(method = "mixomics_diablo", preparation = "prep-1",
                    representation = "levels", features = c06_features,
                    units = c06_ids, metric = "native_block_splsda_proportion",
                    denominator = "squared_frobenius_norm_of_fitted_block",
                    run_status = "complete", include_native = TRUE) {
  component_ids <- c("comp1", "comp2")
  common_scores <- c06_matrix(c(1, 2, 3, 2, 3, 4), units, component_ids)
  common_loadings <- stats::setNames(lapply(c06_blocks, function(block_id) {
    c06_matrix(c(0.8, 0.2, 0.3, 0.7), features[[block_id]], component_ids)
  }), c06_blocks)
  common_variance <- c06_matrix(c(0.4, 0.2, 0.3, 0.5), c06_blocks, component_ids)
  variance_semantics <- if (identical(method, "mofa2")) {
    list(
      metric = "MOFA2::get_variance_explained$r2_per_factor",
      assay_space = "adapter_transformed_training_views",
      numerator = "observed_residual_sse_reduction_for_single_factor_reconstruction",
      denominator = "sum_of_squared_observed_transformed_values_about_zero_per_view",
      native_unit = "percentage",
      output_unit = "proportion",
      conversion = "divide_by_100_unconditionally",
      accumulation = "single_factor_per_view_against_zero_baseline_not_cumulative",
      missing_observations = "excluded_by_native_na_rm",
      interpretation = "exploratory_variance_diagnostic_not_predictive_variance"
    )
  } else {
    list(
      metric = metric,
      units = "proportion",
      semantics = "per_component_against_original_block_not_cumulative",
      denominator = denominator,
      assay_space = "mixomics_fitted_block_after_declared_scaling"
    )
  }
  provenance <- list(
    study_id = "synthetic-c06",
    analysis_id = "analysis-1",
    preparation_context_id = preparation,
    representation_id = representation,
    unit_map = units,
    feature_map = features,
    matrix_sha256 = c06_hashes,
    source_roles = list(proteome = list(status = "supplied"), metabolome = list(status = "supplied")),
    source_selection = list(proteome = list(value = "full_unselected"), metabolome = list(value = "full_unselected")),
    outcome_eligibility = list(status = "eligible"),
    task_mode = "exploratory",
    required_views = c06_blocks
  )
  params <- list(
    native_method = switch(
      method,
      mixomics_diablo = "mixOmics::block.splsda",
      mixomics_block_spls = "mixOmics::block.spls",
      mofa2 = "MOFA2::run_mofa",
      "stats::prcomp"
    ),
    native_version = "synthetic-1",
    training_unit_ids = units,
    training_state_hash = paste(rep("c", 64), collapse = ""),
    score_definition = if (identical(method, "mofa2")) "native_mofa_factor_values" else "consensus_mean",
    selection_rule = if (identical(method, "mofa2")) "all_retained_features_are_native_factor_weights" else "union_of_nonzero_native_predictor_loadings",
    variance = variance_semantics,
    diagnostics = if (identical(method, "concat_pca")) {
      list(global_prcomp_variance = c(comp1 = 0.65, comp2 = 0.35))
    } else NULL,
    provenance = provenance
  )
  selected <- stats::setNames(features, c06_blocks)
  result <- triomics_new_integration_result(
    scores = common_scores,
    loadings = common_loadings,
    variance = common_variance,
    selected = selected,
    backend = method,
    params = params
  )
  eligibility <- list(
    block_ids = c06_blocks,
    unit_ids = units,
    training_unit_ids = units,
    required_views = c06_blocks,
    block_feature_keys = features,
    input_keys = features,
    retained = features,
    excluded = stats::setNames(lapply(c06_blocks, function(block_id) character()), c06_blocks),
    outcome_eligibility = provenance$outcome_eligibility,
    status = "eligible"
  )
  sidecars <- NULL
  if (isTRUE(include_native)) {
    native_scores <- stats::setNames(lapply(c06_blocks, function(block_id) {
      c06_matrix(c(11, 12, 13, 21, 22, 23), units, component_ids)
    }), c06_blocks)
    native_loadings <- stats::setNames(lapply(c06_blocks, function(block_id) {
      c06_matrix(c(0.6, 0.4, 0.3, 0.7), features[[block_id]], component_ids)
    }), c06_blocks)
    native_state <- if (identical(method, "concat_pca")) {
      list(
        state = list(training_unit_ids = units),
        training_scores = common_scores,
        block_loadings = common_loadings,
        block_projection_fraction = common_variance
      )
    } else if (identical(method, "mofa2")) {
      list(
        factors = common_scores,
        weights = common_loadings,
        variance = common_variance,
        native_variance_percentage = common_variance * 100,
        variance_semantics = variance_semantics,
        block_ids = c06_blocks,
        training_unit_ids = units
      )
    } else {
      list(
        native_scores = native_scores,
        native_loadings = native_loadings,
        block_ids = c06_blocks,
        training_unit_ids = units,
        model = list(prop_expl_var = stats::setNames(
          lapply(seq_along(c06_blocks), function(i) c(0.4, 0.3)), c06_blocks
        ))
      )
    }
    sidecars <- list(
      native_state = native_state,
      metadata = list(
        backend = method,
        native_method = params$native_method,
        native_version = params$native_version,
        state_hash = params$training_state_hash,
        model_role = if (identical(method, "mofa2")) "exploratory_structure_discovery" else "supervised_prediction"
      )
    )
  }
  list(
    integration_id = paste0("integration-", method),
    method = method,
    required = TRUE,
    status = run_status,
    native_artifact_ref = if (identical(run_status, "complete") && isTRUE(include_native)) {
      list(
        artifact_id = paste0("native-state-", method),
        sha256 = paste(rep("f", 64), collapse = "")
      )
    } else NULL,
    result = result,
    eligibility = eligibility,
    sidecars = sidecars
  )
}

c06_primary_nominations <- function() {
  list(
    schema_version = "2.0",
    artifact_type = "candidate_nominations",
    study_id = "synthetic-c06",
    analysis_id = "analysis-1",
    status = "complete",
    source_artifact_id = "stage-b-block-results",
    source_artifact_ref = list(
      artifact_id = "b-t07-candidate-nominations",
      sha256 = paste(rep("1", 64), collapse = "")
    ),
    preparation_context_id = "prep-1",
    representation_id = "levels",
    nominations = list(list(
      nomination_id = paste(rep("d", 64), collapse = ""),
      feature_key = "p1",
      source_branch = "primary_statistics",
      result_ref = list(block_id = "proteome", claim_id = "contrast-1", family_id = "family-1"),
      selection_rule = "valid_q_and_effect_reporting_thresholds",
      selection_value = list(q = 0.01, effect = 2),
      q_threshold = 0.05,
      effect_threshold = 1
    ))
  )
}

c06_control_context <- function(preparation = "prep-1") {
  list(
    sample_ids = c06_ids,
    feature_keys = c06_features,
    preparation_context_id = preparation,
    representation_id = "levels",
    split_plan_sha256 = paste(rep("e", 64), collapse = "")
  )
}

c06_control <- function(name, status = "not_run", preparation = "prep-1") {
  unrun <- function(method) list(
    method = method,
    status = status,
    artifact_ref = NULL,
    metrics = NULL,
    reason = if (identical(status, "not_run")) paste(name, method, "control not run") else NULL,
    match_context = c06_control_context(preparation)
  )
  list(control = name, candidate = unrun("mixomics_diablo"), baseline = unrun("matched_concat_plsda"))
}

c06_unrun_controls <- function() {
  lapply(c("null", "weak_signal", "strong_signal", "adversarial"), c06_control)
}

c06_c01_fit_run <- function() {
  units <- paste0("U", seq_len(6L))
  proteome <- c06_matrix(
    c(1, 2, 4, 3, 5, 8,
      8, 7, 6, 5, 3, 2,
      2, 4, 1, 5, 3, 6,
      rep(9, 6)),
    c("p1", "p2", "p3", "p0"), units
  )
  metabolome <- c06_matrix(
    c(3, 1, 4, 2, 8, 5,
      4, 6, 3, 7, 1, 5,
      5, 2, 7, 3, 9, 4),
    c("m1", "m2", "m3"), units
  )
  resolved <- list(
    study_id = "synthetic-c06",
    analysis_id = "analysis-1",
    preparation_context_id = "prep-1",
    representation = "levels",
    unit_ids = units,
    blocks = list(
      proteome = list(
        block_id = "proteome", assay = proteome, sample_ids = units,
        feature_keys = rownames(proteome), source_selection = "full_unselected"
      ),
      metabolome = list(
        block_id = "metabolome", assay = metabolome, sample_ids = units,
        feature_keys = rownames(metabolome), source_selection = "full_unselected"
      )
    )
  )
  spec <- list(
    method = "concat_pca",
    block_ids = c("proteome", "metabolome"),
    unit_ids = units,
    training_unit_ids = units,
    target_unit_ids = units,
    n_components = 2L,
    scale_features = FALSE,
    block_scaling = "none",
    selection = list(rule = "none")
  )
  fit <- triomics_integration_fit_once(resolved, spec)
  payload_path <- tempfile("c06-c01-result-", fileext = ".json")
  triomics_write_json_atomic(triomics_c01_result_payload(fit$result), payload_path)
  native_state_sha256 <- triomics_hash_text(
    triomics_canonical_json(fit$backend_fit$native_state$state)
  )
  run <- list(
    integration_id = "c01-actual-pca-fit",
    method = "concat_pca",
    required = TRUE,
    status = "complete",
    native_artifact_ref = list(
      artifact_id = "stage-c-integration-results",
      sha256 = triomics_sha256(payload_path)
    ),
    result = fit$result,
    eligibility = fit$eligibility,
    sidecars = c(fit$sidecars, list(metadata = fit$backend_fit$metadata))
  )
  list(run = run, fit = fit, payload_path = payload_path,
       native_state_sha256 = native_state_sha256)
}

testthat::test_that("C-06 exports both nomination branches and complete keyed integration evidence", {
  run <- c06_run()
  evidence <- triomics_c06_export_integration(run)
  testthat::expect_identical(evidence$status, "complete")
  testthat::expect_identical(unlist(evidence$eligibility$training_unit_ids), c06_ids)
  testthat::expect_identical(unlist(evidence$eligibility$required_views), c06_blocks)
  testthat::expect_identical(unlist(evidence$eligibility$details$retained$proteome), c("p1", "p2"))
  testthat::expect_length(unlist(evidence$eligibility$details$excluded$proteome), 0L)
  testthat::expect_equal(evidence$native$scores_by_view$proteome$values[[1]][[1]], 11)
  testthat::expect_equal(evidence$common$scores$values[[1]][[1]], 1)
  testthat::expect_identical(evidence$selection$selected$proteome, c("p1", "p2"))
  testthat::expect_identical(evidence$variance$definition$metric, "native_block_splsda_proportion")
  testthat::expect_equal(evidence$variance$common_values$values[[1]][[1]], 0.4)

  handoff <- triomics_c06_build_handoff(
    primary_nominations = c06_primary_nominations(),
    integration_runs = list(run),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mixomics_diablo = list(status = "complete")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  testthat::expect_identical(handoff$nomination_branches$primary_statistics$status, "complete")
  testthat::expect_identical(handoff$nomination_branches$integration_selection$status, "complete")
  testthat::expect_true(any(vapply(handoff$nominations, function(x) identical(x$source_branch, "primary_statistics"), logical(1))))
  testthat::expect_true(any(vapply(handoff$nominations, function(x) identical(x$source_branch, "integration_selection"), logical(1))))
  testthat::expect_identical(length(handoff$nominations), 5L)
  testthat::expect_identical(handoff$clinical_prediction$completion_policy, "conditional")

  output_root <- tempfile("triomics-c06-output-")
  error <- testthat::capture_error(triomics_c06_write_handoff(
    handoff, output_root,
    config_sha256 = paste(rep("f", 64), collapse = ""),
    producer_commit = "ef7da20",
    privacy_class = "synthetic"
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_EXPORT_PREDECESSOR_MISSING")
  testthat::expect_false(file.exists(file.path(output_root, "D")))
  unlink(output_root, recursive = TRUE)
})

testthat::test_that("C-06 preserves native PCA scores/loadings and separates global from block variance", {
  pca <- c06_run(
    method = "concat_pca", metric = "orthogonal_pca_block_projection",
    denominator = "block_training_frobenius_sum_of_squares"
  )
  evidence <- triomics_c06_export_integration(pca)
  testthat::expect_identical(
    evidence$native$variance_definition$metric,
    "stats::prcomp_global_variance_fraction"
  )
  testthat::expect_identical(
    evidence$variance$common_definition$metric,
    "orthogonal_pca_block_projection"
  )
  testthat::expect_equal(evidence$native$variance_values[[1]], 0.65)
  testthat::expect_equal(evidence$native$scores$values[[1]][[1]], 1)
  testthat::expect_equal(evidence$native$loadings_by_view$proteome$values[[1]][[1]], 0.8)

  mofa <- c06_run(method = "mofa2", metric = "MOFA2::get_variance_explained$r2_per_factor",
                  denominator = "native_view_total_sum_of_squares")
  mofa_evidence <- triomics_c06_export_integration(mofa)
  testthat::expect_equal(mofa_evidence$native$scores$values[[1]][[1]], 1)
  testthat::expect_equal(mofa_evidence$native$loadings$proteome$values[[1]][[1]], 0.8)
  testthat::expect_identical(mofa_evidence$native$variance_definition$metric,
                             "MOFA2::get_variance_explained$r2_per_factor")
  testthat::expect_identical(mofa_evidence$native$variance_definition$units, "percent")
  testthat::expect_identical(mofa_evidence$native$variance_definition$native_unit, "percentage")
  testthat::expect_identical(mofa_evidence$native$variance_definition$conversion,
                             "divide_by_100_unconditionally")
  testthat::expect_equal(mofa_evidence$native$variance_values$values[[1]][[1]], 40)
  testthat::expect_identical(mofa_evidence$variance$common_definition$units, "proportion")
  testthat::expect_equal(mofa_evidence$variance$common_values$values[[1]][[1]], 0.4)
})

testthat::test_that("C-06 exports a real accepted C-01 fit without invented PCA sidecars", {
  actual <- c06_c01_fit_run()
  on.exit(unlink(actual$payload_path), add = TRUE)
  testthat::expect_s3_class(actual$fit$result, "IntegrationResult")
  testthat::expect_identical(actual$fit$result$backend, "concat_pca")

  evidence <- triomics_c06_export_integration(actual$run)

  testthat::expect_identical(evidence$context$preparation_context_id, "prep-1")
  testthat::expect_identical(evidence$context$representation_id, "levels")
  testthat::expect_identical(
    evidence$variance$common_definition$metric,
    "orthogonal_pca_block_projection"
  )
  testthat::expect_identical(
    evidence$native$variance_definition$source,
    "stats::prcomp$sdev"
  )
  testthat::expect_identical(
    evidence$native$native_version,
    actual$fit$backend_fit$metadata$backend_version
  )
  testthat::expect_equal(
    evidence$native$variance_values[[1]],
    actual$fit$result$params$diagnostics$global_prcomp_variance[[1]]
  )
  testthat::expect_equal(
    evidence$native$scores$values[[1]][[1]],
    actual$fit$result$scores[1, 1]
  )
  testthat::expect_equal(
    evidence$native$loadings_by_view$proteome$values[[1]][[1]],
    actual$fit$result$loadings$proteome[1, 1]
  )
  testthat::expect_identical(
    evidence$native$state_sha256,
    actual$native_state_sha256
  )
})

testthat::test_that("C-06 preserves a legitimate complete-empty primary nomination branch", {
  primary <- c06_primary_nominations()
  primary$nominations <- list()
  handoff <- triomics_c06_build_handoff(
    primary_nominations = primary,
    integration_runs = list(c06_run()),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mixomics_diablo = list(status = "complete")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  testthat::expect_identical(handoff$nomination_branches$primary_statistics$status, "complete")
  testthat::expect_identical(handoff$nomination_branches$primary_statistics$source_artifact_id,
                             "stage-b-block-results")
  testthat::expect_length(handoff$nomination_branches$primary_statistics$nominations, 0L)
  testthat::expect_true(any(vapply(handoff$nominations, function(x) {
    identical(x$source_branch, "integration_selection")
  }, logical(1))))
})

testthat::test_that("C-06 preserves an explicitly unrequested primary nomination branch", {
  handoff <- triomics_c06_build_handoff(
    primary_nominations = NULL,
    integration_runs = list(c06_run()),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mixomics_diablo = list(status = "complete")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  testthat::expect_identical(handoff$nomination_branches$primary_statistics$status, "not_requested")
  testthat::expect_null(handoff$nomination_branches$primary_statistics$source_artifact_ref)
  testthat::expect_length(handoff$lineage_inputs, 1L)
})

testthat::test_that("C-06 writer requires the authoritative privacy class and native B", {
  handoff <- triomics_c06_build_handoff(
    primary_nominations = c06_primary_nominations(),
    integration_runs = list(c06_run()),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mixomics_diablo = list(status = "complete")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  output_root <- tempfile("triomics-c06-sensitive-")
  error <- testthat::capture_error(triomics_c06_write_handoff(
    handoff, output_root,
    config_sha256 = paste(rep("e", 64), collapse = ""),
    producer_commit = "c06-test",
    privacy_class = "sensitive"
  ))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "C_EXPORT_PREDECESSOR_MISSING")
  testthat::expect_false(file.exists(file.path(output_root, "D")))
  no_privacy <- testthat::capture_error(
    triomics_c06_write_handoff(
      handoff, tempfile("triomics-c06-no-privacy-"),
      config_sha256 = paste(rep("e", 64), collapse = ""),
      producer_commit = "c06-test"
    )
  )
  testthat::expect_s3_class(no_privacy, "triomics_error")
  testthat::expect_identical(no_privacy$code, "C_EXPORT_INPUT_INVALID")
  unlink(output_root, recursive = TRUE)
})

testthat::test_that("C-06 writer revalidates context and lineage before requiring native B", {
  handoff <- triomics_c06_build_handoff(
    primary_nominations = c06_primary_nominations(),
    integration_runs = list(c06_run()),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mixomics_diablo = list(status = "complete")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  testthat::expect_identical(handoff$preparation_context_id, "prep-1")
  testthat::expect_identical(handoff$representation_id, "levels")
  output_root <- tempfile("triomics-c06-lineage-")
  missing_native <- c06_run()
  missing_native$native_artifact_ref <- NULL
  testthat::expect_error(
    triomics_c06_build_handoff(
      primary_nominations = c06_primary_nominations(),
      integration_runs = list(missing_native),
      evaluation_evidence = list(controls = c06_unrun_controls()),
      backend_statuses = list(mixomics_diablo = list(status = "complete")),
      clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
    ),
    class = "triomics_error"
  )
  bad_context <- handoff
  bad_context$preparation_context_id <- "changed-after-fit"
  testthat::expect_error(
    triomics_c06_write_handoff(
      bad_context, tempfile("triomics-c06-context-mismatch-"),
      config_sha256 = paste(rep("a", 64), collapse = ""),
      producer_commit = "c06-test",
      privacy_class = "synthetic"
    ),
    class = "triomics_error"
  )
  missing_lineage <- handoff
  missing_lineage$lineage_inputs <- list()
  missing_lineage_root <- tempfile("triomics-c06-lineage-missing-")
  missing_lineage_error <- tryCatch(
    triomics_c06_write_handoff(
      missing_lineage, missing_lineage_root,
      config_sha256 = paste(rep("b", 64), collapse = ""),
      producer_commit = "c06-test",
      privacy_class = "synthetic"
    ),
    error = identity
  )
  testthat::expect_s3_class(missing_lineage_error, "triomics_error")
  testthat::expect_identical(missing_lineage_error$code, "C_EXPORT_LINEAGE_MISMATCH")
  unlink(missing_lineage_root, recursive = TRUE)
  fabricated_evaluation_source <- handoff
  fabricated_ref <- list(
    artifact_id = "fabricated-c07-source",
    sha256 = paste(rep("c", 64), collapse = "")
  )
  fabricated_evaluation_source$evaluation_evidence$source_artifacts <- list(fabricated_ref)
  fabricated_evaluation_source$lineage_inputs <- c(
    fabricated_evaluation_source$lineage_inputs, list(fabricated_ref)
  )
  testthat::expect_error(
    triomics_c06_write_handoff(
      fabricated_evaluation_source, tempfile("triomics-c06-evaluation-lineage-"),
      config_sha256 = paste(rep("c", 64), collapse = ""),
      producer_commit = "c06-test",
      privacy_class = "synthetic"
    ),
    class = "triomics_error"
  )
  unlink(output_root, recursive = TRUE)
})

testthat::test_that("C-06 comparisons bind sample, feature, preparation and representation identity", {
  left <- c06_run(method = "mixomics_diablo")
  right <- c06_run(method = "mofa2", metric = "MOFA2::get_variance_explained$r2_per_factor",
                   denominator = "native_view_total_sum_of_squares")
  comparison <- triomics_c06_compare_integrations(list(left, right))
  pair <- comparison$pairs[[1]]
  testthat::expect_identical(pair$context_status, "matched")
  testthat::expect_identical(pair$variance_comparison$status, "unlike_definitions")
  testthat::expect_identical(pair$variance_comparison$ranking_status, "withheld_unlike_definitions")
  testthat::expect_null(pair$variance_comparison$ranking)
  testthat::expect_identical(pair$variance_comparison$definitions[[1]]$metric, "native_block_splsda_proportion")
  testthat::expect_identical(pair$variance_comparison$definitions[[2]]$metric, "MOFA2::get_variance_explained$r2_per_factor")

  left_full <- c06_run(method = "mixomics_diablo")
  right_full <- c06_run(method = "mofa2")
  left_full$result$params$provenance$feature_map <- NULL
  left_full$eligibility$block_feature_keys <- NULL
  left_full$result$selected$proteome <- "p1"
  left_full$result$loadings$proteome <- left_full$result$loadings$proteome["p1", , drop = FALSE]
  right_full$result$params$provenance$feature_map <- NULL
  right_full$eligibility$block_feature_keys <- NULL
  right_full$result$selected$proteome <- "p2"
  right_full$result$loadings$proteome <- right_full$result$loadings$proteome["p2", , drop = FALSE]
  full_map_comparison <- triomics_c06_compare_integrations(list(left_full, right_full))
  testthat::expect_identical(full_map_comparison$pairs[[1]]$context_status, "matched")

  for (mismatch in list(
    c06_run(method = "mofa2", units = c("u1", "u2", "u4")),
    c06_run(method = "mofa2", features = list(proteome = c("p1", "p3"), metabolome = c("m1", "m2"))),
    c06_run(method = "mofa2", preparation = "prep-2"),
    c06_run(method = "mofa2", representation = "delta")
  )) {
    testthat::expect_error(
      triomics_c06_compare_integrations(list(left, mismatch)), class = "triomics_error"
    )
  }
})

testthat::test_that("C-06 C-07 evidence contains every control and only matched actual runs or explicit unrun records", {
  controls <- triomics_c06_evaluation_evidence(c06_unrun_controls())
  testthat::expect_identical(
    vapply(controls$controls, `[[`, character(1), "control"),
    c("null", "weak_signal", "strong_signal", "adversarial")
  )
  testthat::expect_true(all(vapply(controls$controls, function(x) {
    identical(x$candidate$status, "not_run") && identical(x$baseline$status, "not_run") &&
      identical(x$candidate$match_context, x$baseline$match_context) &&
      !length(x$candidate$metrics) && !length(x$baseline$metrics)
  }, logical(1))))

  actual <- c06_control("null", status = "complete")
  for (side in c("candidate", "baseline")) {
    actual[[side]]$artifact_ref <- paste0("synthetic-run-", side)
    actual[[side]]$metrics <- list(balanced_error = 0.25)
    actual[[side]]$reason <- NULL
  }
  testthat::expect_error(
    triomics_c06_evaluation_evidence(c(list(actual), c06_unrun_controls()[-1])),
    class = "triomics_error"
  )

  testthat::expect_error(
    triomics_c06_evaluation_evidence(c06_unrun_controls()[-4]), class = "triomics_error"
  )
  mismatched <- c06_control("null")
  mismatched$baseline$match_context$split_plan_sha256 <- paste(rep("f", 64), collapse = "")
  testthat::expect_error(
    triomics_c06_evaluation_evidence(c(list(mismatched), c06_unrun_controls()[-1])),
    class = "triomics_error"
  )
  fabricated <- c06_control("null")
  fabricated$candidate$metrics <- list(balanced_error = 0.5)
  testthat::expect_error(
    triomics_c06_evaluation_evidence(c(list(fabricated), c06_unrun_controls()[-1])),
    class = "triomics_error"
  )
})

testthat::test_that("C-06 keeps unavailable routes visible and cannot complete with an empty adapter", {
  empty <- triomics_c06_build_handoff(
    primary_nominations = c06_primary_nominations(),
    integration_runs = list(),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mofa2 = list(status = "unavailable", reason = "MOFA2 runtime unavailable")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  testthat::expect_false(identical(empty$status, "complete"))
  testthat::expect_identical(empty$backend_statuses$mofa2$status, "unavailable")
  testthat::expect_identical(empty$clinical_prediction$status, "not_started")

  unavailable_run <- c06_run(method = "mofa2", run_status = "unavailable", include_native = FALSE)
  unavailable <- triomics_c06_build_handoff(
    primary_nominations = c06_primary_nominations(),
    integration_runs = list(unavailable_run),
    evaluation_evidence = list(controls = c06_unrun_controls()),
    backend_statuses = list(mofa2 = list(status = "unavailable", reason = "MOFA2 runtime unavailable")),
    clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
  )
  testthat::expect_false(identical(unavailable$status, "complete"))
  testthat::expect_identical(unavailable$integration_evidence[[1]]$status, "unavailable")

  testthat::expect_error(
    triomics_c06_build_handoff(
      primary_nominations = c06_primary_nominations(), integration_runs = list(),
      evaluation_evidence = list(controls = c06_unrun_controls()),
      backend_statuses = list(unknown_backend = list(status = "complete")),
      clinical_prediction = list(status = "not_started", completion_policy = "conditional", packet_id = "C-09")
    ),
    class = "triomics_error"
  )

  testthat::expect_error(
    triomics_c06_export_integration(c06_run(include_native = FALSE)), class = "triomics_error"
  )
})
