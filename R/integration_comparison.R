triomics_c06_context_identity <- function(integration_run) {
  if (!is.list(integration_run) || !inherits(integration_run$result, "IntegrationResult")) {
    triomics_integration_abort(
      "C_COMPARE_INPUT_INVALID", "Comparison requires a completed IntegrationResult."
    )
  }
  result <- triomics_validate_integration_result(integration_run$result)
  provenance <- result$params$provenance
  if (!is.list(provenance)) {
    triomics_integration_abort(
      "C_COMPARE_CONTEXT_MISMATCH", "Integration provenance is required for a cross-backend comparison.",
      key = "result.params.provenance"
    )
  }
  eligibility <- integration_run$eligibility
  if (!is.list(eligibility)) eligibility <- list()
  native_state <- triomics_c06_native_state(integration_run, result)
  feature_map <- triomics_c06_feature_map(eligibility, provenance, result, native_state)
  block_ids <- names(result$selected)
  unit_ids <- provenance$unit_map
  if (is.list(unit_ids)) unit_ids <- unlist(unit_ids, use.names = FALSE)
  if (is.null(unit_ids)) unit_ids <- result$params$training_unit_ids
  if (!is.character(unit_ids) || !length(unit_ids) || anyNA(unit_ids) ||
      any(!nzchar(trimws(unit_ids))) || anyDuplicated(unit_ids) ||
      !setequal(unit_ids, rownames(result$scores)) || length(unit_ids) != nrow(result$scores)) {
    triomics_integration_abort(
      "C_COMPARE_CONTEXT_MISMATCH", "Comparison training units must exactly cover the common score rows.",
      key = "unit_map"
    )
  }
  matrix_hashes <- provenance$matrix_sha256
  if (!is.list(matrix_hashes) || is.null(names(matrix_hashes)) ||
      !identical(names(matrix_hashes), block_ids) || any(vapply(matrix_hashes, function(value) {
        !triomics_sha256_text(value)
      }, logical(1)))) {
    triomics_integration_abort(
      "C_COMPARE_CONTEXT_MISMATCH", "Comparison requires an input matrix hash for every predictor block.",
      key = "matrix_sha256"
    )
  }
  list(
    study_id = provenance$study_id,
    analysis_id = provenance$analysis_id,
    preparation_context_id = provenance$preparation_context_id,
    representation_id = provenance$representation_id,
    unit_ids = sort(unit_ids, method = "radix"),
    feature_map = stats::setNames(lapply(block_ids, function(block_id) {
      sort(feature_map[[block_id]], method = "radix")
    }), block_ids),
    matrix_sha256 = matrix_hashes[block_ids]
  )
}

triomics_c06_variance_definition <- function(result) {
  variance <- result$params$variance
  if (!is.list(variance)) variance <- list()
  metric <- variance$metric %||% attr(result$variance, "metric")
  units <- variance$units %||% attr(result$variance, "units")
  if (is.null(units) && identical(result$backend, "concat_pca") &&
      identical(metric, "orthogonal_pca_block_projection")) {
    units <- "proportion"
  }
  if (is.null(units)) units <- variance$output_unit
  if (is.null(units)) units <- result$params$variance_units
  denominator <- variance$denominator %||% attr(result$variance, "denominator")
  semantics <- variance$semantics %||% attr(result$variance, "semantics")
  if (is.null(semantics)) semantics <- variance$accumulation
  required <- list(metric = metric, units = units, denominator = denominator, semantics = semantics)
  if (any(vapply(required, function(value) {
    !is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))
  }, logical(1)))) {
    triomics_integration_abort(
      "C_VARIANCE_EVIDENCE_MISSING", "Variance comparison needs metric, units, denominator and semantics.",
      key = "result.params.variance"
    )
  }
  definition <- variance
  definition$metric <- metric
  definition$assay_space <- definition$assay_space %||% attr(result$variance, "assay_space")
  definition$units <- units
  definition$denominator <- denominator
  definition$semantics <- semantics
  definition
}

triomics_c06_compare_integrations <- function(integration_runs) {
  if (!is.list(integration_runs) || length(integration_runs) < 2L) {
    triomics_integration_abort(
      "C_COMPARE_INPUT_INVALID", "At least two integration runs are required for comparison."
    )
  }
  methods <- vapply(integration_runs, function(run) {
    value <- if (is.list(run$result)) run$result$backend else NULL
    if (!is.character(value) || length(value) != 1L || is.na(value)) "" else value
  }, character(1))
  if (any(!nzchar(methods)) || anyDuplicated(methods)) {
    triomics_integration_abort(
      "C_COMPARE_INPUT_INVALID", "Each comparison run needs one distinct registered backend identity.",
      key = "integration_runs"
    )
  }
  identities <- lapply(integration_runs, triomics_c06_context_identity)
  reference <- identities[[1L]]
  same_identity <- vapply(identities[-1L], identical, logical(1), y = reference)
  if (!all(same_identity)) {
    triomics_integration_abort(
      "C_COMPARE_CONTEXT_MISMATCH",
      "Cross-backend comparisons require identical study, analysis, samples, features, matrices, preparation and representation.",
      key = "integration_runs"
    )
  }
  definitions <- lapply(integration_runs, function(run) {
    triomics_c06_variance_definition(triomics_validate_integration_result(run$result))
  })
  same_variance <- vapply(definitions[-1L], identical, logical(1), y = definitions[[1L]])
  variance_status <- if (all(same_variance)) "same_definition" else "unlike_definitions"
  list(
    status = "complete",
    methods = as.list(methods),
    matched_context = reference,
    pairs = lapply(seq_len(length(integration_runs) - 1L), function(i) {
      lapply(seq.int(i + 1L, length(integration_runs)), function(j) {
        unlike <- !identical(definitions[[i]], definitions[[j]])
        list(
          left = methods[[i]],
          right = methods[[j]],
          context_status = "matched",
          variance_comparison = list(
            status = if (unlike) "unlike_definitions" else "same_definition",
            definitions = list(definitions[[i]], definitions[[j]]),
            ranking_status = if (unlike) "withheld_unlike_definitions" else "not_performed",
            ranking = NULL
          )
        )
      })
    }) |> unlist(recursive = FALSE),
    variance_definition_status = variance_status
  )
}

triomics_c06_control_names <- function() {
  c("null", "weak_signal", "strong_signal", "adversarial")
}

triomics_c06_validate_control_context <- function(context, label) {
  required <- c("sample_ids", "feature_keys", "preparation_context_id",
                "representation_id")
  if (!is.list(context) || !all(required %in% names(context))) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", paste0(label, " must declare matched samples, features, preparation and representation."),
      key = label
    )
  }
  split_plan_status <- context$split_plan_status
  split_plan_sha256 <- context$split_plan_sha256
  if (is.null(split_plan_status) && triomics_sha256_text(split_plan_sha256)) {
    split_plan_status <- "executed"
  }
  valid_plan_statuses <- c("executed", "not_run", "unavailable", "failed")
  sample_ids <- context$sample_ids
  feature_keys <- context$feature_keys
  if (!is.character(sample_ids) || !length(sample_ids) || anyNA(sample_ids) ||
      any(!nzchar(trimws(sample_ids))) || anyDuplicated(sample_ids) ||
      !is.list(feature_keys) || is.null(names(feature_keys)) || !length(feature_keys) ||
      anyDuplicated(names(feature_keys)) || any(!nzchar(names(feature_keys))) ||
      !triomics_nonempty_string(context$preparation_context_id) ||
      !triomics_nonempty_string(context$representation_id) ||
      !is.character(split_plan_status) || length(split_plan_status) != 1L ||
      is.na(split_plan_status) || !split_plan_status %in% valid_plan_statuses ||
      (identical(split_plan_status, "executed") && !triomics_sha256_text(split_plan_sha256)) ||
      (!identical(split_plan_status, "executed") && !is.null(split_plan_sha256))) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", paste0(label, " has invalid or incomplete match identity."), key = label
    )
  }
  context$split_plan_status <- split_plan_status
  for (block_id in names(feature_keys)) {
    features <- feature_keys[[block_id]]
    if (!is.character(features) || !length(features) || anyNA(features) ||
        any(!nzchar(trimws(features))) || anyDuplicated(features)) {
      triomics_integration_abort(
        "C_EVAL_EVIDENCE_INVALID", paste0(label, " has an invalid feature map."), key = block_id
      )
    }
  }
  context
}

triomics_c06_validate_control_observation <- function(observation, label) {
  valid_statuses <- c("complete", "not_run", "unavailable", "failed")
  if (!is.list(observation) || !triomics_nonempty_string(observation$method) ||
      !is.character(observation$status) || length(observation$status) != 1L ||
      is.na(observation$status) || !observation$status %in% valid_statuses) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", paste0(label, " needs an explicit method and execution status."), key = label
    )
  }
  observation$match_context <- triomics_c06_validate_control_context(
    observation$match_context, paste0(label, ".match_context")
  )
  if (identical(observation$status, "complete")) {
    run <- observation$evaluation_result
    required_run_fields <- c(
      "method", "split_plan_hash", "predictions", "metrics", "n_predicted",
      "total_denominator", "failed_prediction_count", "inspected_fit_count", "max_fits"
    )
    if (!inherits(run, "EvaluationResult") || any(!required_run_fields %in% names(run)) ||
        !triomics_nonempty_string(observation$artifact_ref) ||
        !triomics_sha256_text(observation$artifact_sha256) ||
        !identical(observation$match_context$split_plan_status, "executed") ||
        !identical(run$method, observation$method) ||
        !triomics_sha256_text(run$split_plan_hash) ||
        !identical(run$split_plan_hash, observation$match_context$split_plan_sha256) ||
        !is.data.frame(run$predictions) || !"unit_id" %in% names(run$predictions) ||
        !setequal(as.character(run$predictions$unit_id), observation$match_context$sample_ids) ||
        length(unique(as.character(run$predictions$unit_id))) != length(observation$match_context$sample_ids) ||
        !is.list(run$metrics) || !is.list(run$metrics$model) || !length(run$metrics$model) ||
        !is.list(run$metrics$baseline) || !length(run$metrics$baseline) ||
        !is.numeric(run$n_predicted) || length(run$n_predicted) != 1L ||
        is.na(run$n_predicted) || run$n_predicted < 1L ||
        !is.numeric(run$total_denominator) || length(run$total_denominator) != 1L ||
        is.na(run$total_denominator) || run$total_denominator < run$n_predicted ||
        !is.numeric(run$failed_prediction_count) || length(run$failed_prediction_count) != 1L ||
        is.na(run$failed_prediction_count) || run$failed_prediction_count != 0L ||
        !is.null(observation$metrics)) {
      triomics_integration_abort(
        "C_EVAL_EVIDENCE_INVALID", paste0(label, " is marked complete without a matched EvaluationResult and its actual model/baseline run evidence."),
        key = label
      )
    }
    observation$metrics <- triomics_c06_json_value(run$metrics)
    observation$run_summary <- list(
      method = run$method,
      split_plan_sha256 = run$split_plan_hash,
      n_predicted = run$n_predicted,
      total_denominator = run$total_denominator,
      failed_prediction_count = run$failed_prediction_count,
      inspected_fit_count = run$inspected_fit_count,
      max_fits = run$max_fits,
      baseline_comparison = triomics_c06_json_value(run$baseline_comparison),
      uncertainty = triomics_c06_json_value(run$uncertainty)
    )
    observation$evaluation_result <- NULL
    if (!is.null(observation$reason)) {
      triomics_integration_abort(
        "C_EVAL_EVIDENCE_INVALID", paste0(label, " cannot carry an unrun reason when an actual run is complete."),
        key = label
      )
    }
  } else {
    if (!triomics_nonempty_string(observation$reason) ||
        !is.null(observation$artifact_ref) || !is.null(observation$metrics) ||
        !is.null(observation$artifact_sha256) ||
        !is.null(observation$evaluation_result)) {
      triomics_integration_abort(
        "C_EVAL_EVIDENCE_INVALID", paste0(label, " must explicitly explain an unrun/failed route and contain no fabricated result."),
        key = label
      )
    }
  }
  observation
}

triomics_c06_evaluation_evidence <- function(controls) {
  if (is.list(controls) && !is.null(controls$controls)) controls <- controls$controls
  if (!is.list(controls) || length(controls) != length(triomics_c06_control_names())) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", "C-07 evidence must include null, weak, strong and adversarial controls."
    )
  }
  names_seen <- vapply(controls, function(control) {
    if (is.list(control) && triomics_nonempty_string(control$control)) control$control else ""
  }, character(1))
  required_names <- triomics_c06_control_names()
  if (any(!nzchar(names_seen)) || anyDuplicated(names_seen) || !setequal(names_seen, required_names)) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", "C-07 controls must contain exactly one record for each required control regime.",
      key = "controls"
    )
  }
  controls <- stats::setNames(controls, names_seen)[required_names]
  normalized <- lapply(required_names, function(control_name) {
    control <- controls[[control_name]]
    if (!is.list(control) || !is.list(control$candidate) || !is.list(control$baseline)) {
      triomics_integration_abort(
        "C_EVAL_EVIDENCE_INVALID", "Every C-07 control requires a candidate and matched baseline record.",
        key = control_name
      )
    }
    candidate <- triomics_c06_validate_control_observation(
      control$candidate, paste0(control_name, ".candidate")
    )
    baseline <- triomics_c06_validate_control_observation(
      control$baseline, paste0(control_name, ".baseline")
    )
    if (!identical(candidate$match_context, baseline$match_context)) {
      triomics_integration_abort(
        "C_EVAL_CONTEXT_MISMATCH", "Candidate and baseline evidence must use identical samples, features, preparation, representation and splits.",
        key = control_name
      )
    }
    list(
      control = control_name,
      status = if (identical(candidate$status, "complete") && identical(baseline$status, "complete")) {
        "complete"
      } else if (identical(candidate$status, "not_run") && identical(baseline$status, "not_run")) {
        "not_run"
      } else {
        "partial"
      },
      candidate = candidate,
      baseline = baseline,
      matched = TRUE
    )
  })
  names(normalized) <- required_names
  reference_context <- normalized[[required_names[[1L]]]]$candidate$match_context
  if (any(!vapply(normalized, function(item) {
    identical(item$candidate$match_context, reference_context)
  }, logical(1)))) {
    triomics_integration_abort(
      "C_EVAL_CONTEXT_MISMATCH", "All C-07 control regimes must share one immutable evaluation context.",
      key = "controls"
    )
  }
  source_artifacts <- unlist(lapply(normalized, function(item) {
    lapply(list(item$candidate, item$baseline), function(observation) {
      if (!identical(observation$status, "complete")) return(NULL)
      list(artifact_id = observation$artifact_ref, sha256 = observation$artifact_sha256)
    })
  }), recursive = FALSE)
  source_artifacts <- Filter(function(artifact) !is.null(artifact), source_artifacts)
  list(
    schema_version = "2.0",
    artifact_type = "integration_evaluation_evidence",
    status = if (all(vapply(normalized, function(item) identical(item$status, "complete"), logical(1)))) {
      "complete"
    } else {
      "partial"
    },
    controls = unname(normalized),
    source_artifacts = unname(source_artifacts)
  )
}
