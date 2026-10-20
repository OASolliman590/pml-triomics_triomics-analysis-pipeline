triomics_c06_nonempty_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
}

triomics_c06_envelope_status <- function(status) {
  if (is.character(status) && length(status) == 1L && !is.na(status) &&
      status %in% triomics_envelope_statuses()) {
    return(status)
  }
  if (identical(status, "not_comparable")) return("partial")
  triomics_integration_abort(
    "C_EXPORT_EVIDENCE_INVALID", "A handoff artifact has a status that cannot be represented by the D envelope contract.",
    key = "status"
  )
}

triomics_c06_ids <- function(value, label, allow_empty = FALSE) {
  if (!is.character(value) || anyNA(value) || any(!nzchar(trimws(value))) ||
      anyDuplicated(value) || (!allow_empty && !length(value))) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", paste0(label, " must be unique non-blank character ids."), key = label
    )
  }
  value
}

triomics_c06_matrix_payload <- function(value, label) {
  if (!is.matrix(value) || !is.numeric(value) || is.complex(value) ||
      length(dim(value)) != 2L || any(dim(value) < 1L) || any(!is.finite(value)) ||
      is.null(rownames(value)) || is.null(colnames(value)) ||
      anyDuplicated(rownames(value)) || anyDuplicated(colnames(value)) ||
      any(!nzchar(rownames(value))) || any(!nzchar(colnames(value)))) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", paste0(label, " must be a finite keyed numeric matrix."), key = label
    )
  }
  list(
    rows = as.list(rownames(value)),
    columns = as.list(colnames(value)),
    values = lapply(seq_len(nrow(value)), function(i) {
      as.list(unname(as.numeric(value[i, , drop = TRUE])))
    })
  )
}

triomics_c06_matrix_list_payload <- function(values, label) {
  if (!is.list(values) || is.null(names(values)) || !length(values) ||
      anyDuplicated(names(values))) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", paste0(label, " must be a named collection of matrices."), key = label
    )
  }
  stats::setNames(lapply(names(values), function(name) {
    triomics_c06_matrix_payload(values[[name]], paste0(label, ".", name))
  }), names(values))
}

triomics_c06_json_value <- function(value) {
  if (inherits(value, "table")) {
    table_value <- unclass(value)
    return(list(
      rows = as.list(rownames(table_value)),
      columns = as.list(colnames(table_value)),
      values = lapply(seq_len(nrow(table_value)), function(i) {
        as.list(unname(as.integer(table_value[i, , drop = TRUE])))
      })
    ))
  }
  if (is.matrix(value)) return(triomics_c06_matrix_payload(value, "json_matrix"))
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(i) {
      lapply(names(value), function(name) triomics_c06_json_value(value[[name]][i])) |>
        stats::setNames(names(value))
    }))
  }
  if (is.list(value)) {
    blocked <- c("model", "reloaded_model", "backend_fit", "training_matrix", "training_response",
                 "assay", "data", "x", "response", "model_path", "path")
    keep <- setdiff(names(value), blocked)
    if (is.null(names(value))) keep <- seq_along(value)
    return(lapply(keep, function(name) triomics_c06_json_value(value[[name]])) |>
             stats::setNames(if (is.null(names(value))) NULL else keep))
  }
  if (is.atomic(value) && is.numeric(value) && any(!is.finite(value), na.rm = TRUE)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", "Serialized numerical evidence must be finite or explicitly unavailable.",
      key = "json_value"
    )
  }
  value
}

triomics_c06_feature_map <- function(eligibility, provenance, result, native_state) {
  map <- provenance$feature_map
  if (is.null(map)) map <- eligibility$block_feature_keys
  if (is.null(map)) map <- eligibility$input_keys
  if (is.null(map)) map <- eligibility$retained
  if (is.null(map) && is.list(eligibility$assays)) {
    map <- lapply(eligibility$assays, colnames)
  }
  if (is.null(map) && is.list(eligibility$data)) {
    map <- lapply(eligibility$data, rownames)
  }
  if (is.null(map) && is.list(native_state$native_loadings)) {
    map <- lapply(native_state$native_loadings, rownames)
  }
  if (is.null(map) && is.list(native_state$block_loadings)) {
    map <- lapply(native_state$block_loadings, rownames)
  }
  if (is.null(map) && is.list(native_state$weights)) {
    map <- lapply(native_state$weights, rownames)
  }
  if (is.null(map) && identical(result$backend, "concat_pca")) map <- result$selected
  if (is.null(map)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "The fitted input feature map is required; selected features are not a substitute.",
      key = "feature_map"
    )
  }
  block_ids <- names(result$selected)
  if (!is.list(map) || is.null(names(map)) || !setequal(names(map), block_ids)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Eligibility evidence must retain a feature map for every predictor block.",
      key = "feature_map"
    )
  }
  map <- map[block_ids]
  stats::setNames(lapply(block_ids, function(block_id) {
    features <- map[[block_id]]
    if (!is.character(features)) features <- as.character(unlist(features, use.names = FALSE))
    triomics_c06_ids(features, paste0("feature_map.", block_id))
  }), block_ids)
}

triomics_c06_native_state <- function(integration_run, result) {
  state <- integration_run$sidecars$native_state
  if (is.null(state) && identical(result$backend, "concat_pca")) state <- result$params$state
  if (!is.list(state)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "A completed integration requires a typed native-state sidecar.",
      key = "sidecars.native_state"
    )
  }
  state
}

triomics_c06_native_payload <- function(integration_run, result, variance_definition) {
  state <- triomics_c06_native_state(integration_run, result)
  metadata <- integration_run$sidecars$metadata
  if (!is.list(metadata)) metadata <- list()
  method <- result$backend
  block_ids <- names(result$selected)
  native_scores <- state$native_scores
  if (is.null(native_scores)) native_scores <- state$training_scores
  if (is.null(native_scores)) native_scores <- state$variates
  native_loadings <- state$native_loadings
  if (is.null(native_loadings)) native_loadings <- state$block_loadings
  if (is.null(native_loadings)) native_loadings <- state$weights
  if (identical(method, "mofa2")) {
    native_scores <- NULL
    native_loadings <- NULL
  }
  native_variance <- NULL
  if (is.list(state$model) && !is.null(state$model$prop_expl_var)) {
    native_variance <- state$model$prop_expl_var
  }
  if (is.null(native_variance)) native_variance <- state$native_variance_percentage
  if (is.null(native_variance) && identical(method, "mofa2")) native_variance <- state$variance
  if (is.null(native_variance) && identical(method, "concat_pca")) {
    native_variance <- result$params$diagnostics$global_prcomp_variance
  }
  if (is.null(native_variance)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Native variance evidence is required for a completed integration.",
      key = "native_variance"
    )
  }
  payload <- list(
    method = method,
    native_method = metadata$native_method %||% result$params$native_method,
    native_version = metadata$native_version %||% result$params$native_version,
    state_sha256 = metadata$state_hash %||% result$params$training_state_hash,
    model_role = metadata$model_role %||% if (identical(method, "mofa2")) "exploratory_structure_discovery" else "integration_fit",
    training_unit_ids = result$params$training_unit_ids %||% result$params$provenance$unit_map,
    scores_by_view = if (is.list(native_scores)) {
      triomics_c06_matrix_list_payload(native_scores[intersect(block_ids, names(native_scores))], "native_scores")
    } else NULL,
    scores = if (is.matrix(state$factors)) {
      triomics_c06_matrix_payload(state$factors, "native_factors")
    } else if (identical(method, "concat_pca") && is.matrix(state$training_scores)) {
      triomics_c06_matrix_payload(state$training_scores, "native_pca_scores")
    } else NULL,
    loadings_by_view = if (is.list(native_loadings)) {
      triomics_c06_matrix_list_payload(native_loadings[intersect(block_ids, names(native_loadings))], "native_loadings")
    } else NULL,
    loadings = if (is.list(state$weights) && is.null(native_loadings)) {
      triomics_c06_matrix_list_payload(state$weights, "native_weights")
    } else NULL,
    variance_values = triomics_c06_json_value(native_variance),
    variance_definition = variance_definition,
    alignment_state = triomics_c06_json_value(state$consensus_state),
    seed_sensitivity = triomics_c06_json_value(state$seed_sensitivity),
    save_reload = triomics_c06_json_value(metadata$save_reload %||% result$params$save_reload)
  )
  if (is.null(payload$native_method) || !triomics_c06_nonempty_string(payload$native_method) ||
      is.null(payload$native_version) || !triomics_c06_nonempty_string(payload$native_version) ||
      is.null(payload$state_sha256) || !triomics_sha256_text(payload$state_sha256)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Native method, version and fitted-state hash are required.",
      key = "native_metadata"
    )
  }
  if (is.null(payload$scores_by_view) && is.null(payload$scores) && !identical(method, "concat_pca")) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Native training scores or factors are absent from the sidecar.",
      key = "native_scores"
    )
  }
  if (is.null(payload$loadings_by_view) && is.null(payload$loadings) &&
      !identical(method, "concat_pca")) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Native loadings or weights are absent from the sidecar.",
      key = "native_loadings"
    )
  }
  payload
}

triomics_c06_native_variance_definition <- function(result) {
  if (identical(result$backend, "concat_pca")) {
    return(list(
      metric = "stats::prcomp_global_variance_fraction",
      units = "proportion",
      denominator = "sum_of_squared_singular_values",
      semantics = "per_component_of_total_weighted_centered_training_matrix",
      assay_space = "weighted_centered_training_concatenation",
      source = "stats::prcomp$sdev"
    ))
  }
  triomics_c06_variance_definition(result)
}

triomics_c06_eligibility_details <- function(eligibility, unit_ids, required_views,
                                            feature_map, provenance) {
  fields <- c(
    "status", "block_ids", "unit_ids", "training_unit_ids", "required_views",
    "input_keys", "retained", "excluded", "feature_counts", "available_rank",
    "unit_status", "block_availability", "outcome_eligibility",
    "preparation_state_hashes"
  )
  details <- eligibility[intersect(fields, names(eligibility))]
  details$unit_ids <- as.list(unit_ids)
  details$training_unit_ids <- as.list(unit_ids)
  details$required_views <- as.list(required_views)
  details$feature_map <- triomics_c06_json_value(feature_map)
  details$outcome_eligibility <- triomics_c06_json_value(
    eligibility$outcome_eligibility %||% provenance$outcome_eligibility
  )
  triomics_c06_json_value(details)
}

triomics_c06_selection_definition <- function(result) {
  params <- result$params
  selection <- params$selection
  if (!is.null(params$selection_rule)) {
    return(list(rule = params$selection_rule, value = params$keepX %||% NULL))
  }
  if (!is.null(params$sparse_selection)) {
    return(list(rule = params$sparse_selection, value = params$keepX %||% NULL))
  }
  if (is.list(params$effective$selection)) {
    return(list(
      rule = params$effective$selection$rule,
      criterion = params$effective$selection$criterion,
      value = params$effective$selection$top_k
    ))
  }
  if (is.list(selection) && !is.null(selection$method)) {
    return(list(rule = selection$method, value = selection$value))
  }
  if (identical(result$backend, "mofa2")) {
    return(list(rule = "all_retained_features_as_native_factor_weights", value = NULL))
  }
  triomics_integration_abort(
    "C_EXPORT_SELECTION_UNVERIFIED", "Integration selections require the executed native/dense rule and its value.",
    key = "result.params.selection"
  )
}

triomics_c06_export_integration <- function(integration_run) {
  if (!is.list(integration_run) || !identical(integration_run$status, "complete") ||
      !inherits(integration_run$result, "IntegrationResult") || !is.list(integration_run$eligibility)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", "Only a completed IntegrationResult with eligibility evidence can be exported."
    )
  }
  result <- triomics_validate_integration_result(integration_run$result)
  provenance <- result$params$provenance
  if (!is.list(provenance)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Integration provenance is required for D export.",
      key = "result.params.provenance"
    )
  }
  eligibility <- integration_run$eligibility
  native_state <- triomics_c06_native_state(integration_run, result)
  block_ids <- names(result$selected)
  feature_map <- triomics_c06_feature_map(eligibility, provenance, result, native_state)
  unit_ids <- eligibility$training_unit_ids
  if (is.null(unit_ids)) unit_ids <- eligibility$unit_ids
  if (is.null(unit_ids)) unit_ids <- eligibility$units
  if (is.null(unit_ids)) unit_ids <- result$params$training_unit_ids
  if (is.null(unit_ids)) unit_ids <- provenance$unit_map
  if (is.list(unit_ids)) unit_ids <- unlist(unit_ids, use.names = FALSE)
  unit_ids <- triomics_c06_ids(unit_ids, "training_unit_ids")
  if (!setequal(unit_ids, rownames(result$scores)) || length(unit_ids) != nrow(result$scores)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", "Eligibility training units must exactly cover common score rows.",
      key = "training_unit_ids"
    )
  }
  required_views <- eligibility$required_views %||% provenance$required_views %||% block_ids
  required_views <- triomics_c06_ids(as.character(unlist(required_views, use.names = FALSE)), "required_views")
  if (!setequal(required_views, block_ids) || length(required_views) != length(block_ids)) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_INVALID", "Required integration views must exactly match result blocks.",
      key = "required_views"
    )
  }
  context <- list(
    study_id = provenance$study_id,
    analysis_id = provenance$analysis_id,
    preparation_context_id = provenance$preparation_context_id,
    representation_id = provenance$representation_id
  )
  if (any(!vapply(context, triomics_c06_nonempty_string, logical(1)))) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Study, analysis, preparation and representation identities are required.",
      key = "provenance"
    )
  }
  matrix_hashes <- provenance$matrix_sha256
  if (!is.list(matrix_hashes) || is.null(names(matrix_hashes)) ||
      !setequal(names(matrix_hashes), block_ids) || any(vapply(matrix_hashes[block_ids], function(value) {
        !is.character(value) || length(value) != 1L || is.na(value) || !grepl("^[0-9a-f]{64}$", value)
      }, logical(1)))) {
    triomics_integration_abort(
      "C_EXPORT_EVIDENCE_MISSING", "Each predictor matrix needs a verified SHA256 identity.",
      key = "matrix_sha256"
    )
  }
  variance_definition <- triomics_c06_variance_definition(result)
  selection_definition <- triomics_c06_selection_definition(result)
  selection_rule <- selection_definition$rule
  if (!triomics_c06_nonempty_string(selection_rule)) {
    triomics_integration_abort(
      "C_EXPORT_SELECTION_UNVERIFIED", "The executed integration selection rule is missing.",
      key = "selection_rule"
    )
  }
  native <- triomics_c06_native_payload(integration_run, result, variance_definition)
  native_variance_definition <- triomics_c06_native_variance_definition(result)
  native$variance_definition <- native_variance_definition
  identity <- list(
    study_id = context$study_id,
    analysis_id = context$analysis_id,
    preparation_context_id = context$preparation_context_id,
    representation_id = context$representation_id,
    unit_ids = sort(unit_ids, method = "radix"),
    feature_map = stats::setNames(lapply(block_ids, function(block_id) {
      sort(feature_map[[block_id]], method = "radix")
    }), block_ids),
    matrix_sha256 = matrix_hashes[block_ids]
  )
  common <- list(
    backend = result$backend,
    scores = triomics_c06_matrix_payload(result$scores, "common_scores"),
    loadings = triomics_c06_matrix_list_payload(result$loadings, "common_loadings"),
    variance = triomics_c06_matrix_payload(result$variance, "common_variance"),
    selected = triomics_c06_json_value(result$selected),
    parameters = triomics_c06_json_value(result$params[setdiff(names(result$params), "provenance")])
  )
  selected_evidence <- lapply(block_ids, function(block_id) {
    features <- result$selected[[block_id]]
    values <- result$loadings[[block_id]]
    stats::setNames(lapply(features, function(feature_key) {
      loading <- values[feature_key, , drop = TRUE]
      list(
        feature_key = feature_key,
        component_loadings = as.list(stats::setNames(as.numeric(loading), names(loading)))
      )
    }), features)
  })
  names(selected_evidence) <- block_ids
  list(
    schema_version = "2.0",
    artifact_type = "integration_evidence",
    integration_id = integration_run$integration_id %||% paste0("integration-", result$backend),
    status = "complete",
    backend = result$backend,
    context = context,
    identity_sha256 = triomics_hash_text(triomics_canonical_json(identity)),
    identity = triomics_c06_json_value(identity),
    eligibility = list(
      status = eligibility$status %||% "validated",
      block_ids = as.list(block_ids),
      required_views = as.list(required_views),
      training_unit_ids = as.list(unit_ids),
      feature_map = triomics_c06_json_value(feature_map),
      matrix_sha256 = triomics_c06_json_value(matrix_hashes[block_ids]),
      source_roles = triomics_c06_json_value(provenance$source_roles),
      source_selection = triomics_c06_json_value(provenance$source_selection),
      outcome_eligibility = triomics_c06_json_value(provenance$outcome_eligibility),
      preparation_state_hashes = triomics_c06_json_value(eligibility$preparation_state_hashes),
      details = triomics_c06_eligibility_details(
        eligibility, unit_ids, required_views, feature_map, provenance
      )
    ),
    native = native,
    common = common,
    selection = list(
      rule = selection_definition,
      selected = triomics_c06_json_value(result$selected),
      selected_values = triomics_c06_json_value(selected_evidence)
    ),
    variance = list(
      definition = variance_definition,
      native_definition = native_variance_definition,
      common_definition = variance_definition,
      native_values = native$variance_values,
      common_values = common$variance,
      comparison_policy = "compare_only_identical_metric_units_denominator_and_semantics"
    )
  )
}

triomics_c06_primary_branch <- function(primary_nominations, study_id, analysis_id) {
  if (is.null(primary_nominations)) {
    return(list(status = "not_requested", source_artifact_id = NULL, nominations = list()))
  }
  if (!is.list(primary_nominations) ||
      !identical(primary_nominations$artifact_type, "candidate_nominations") ||
      !is.list(primary_nominations$nominations) ||
      !triomics_c06_nonempty_string(primary_nominations$source_artifact_id) ||
      !identical(as.character(primary_nominations$study_id), study_id) ||
      !identical(as.character(primary_nominations$analysis_id), analysis_id)) {
    triomics_integration_abort(
      "C_EXPORT_NOMINATION_INVALID", "The primary-statistics nomination artifact must match the D handoff study and analysis.",
      key = "primary_nominations"
    )
  }
  valid_statuses <- c("not_requested", "not_started", "running", "complete", "partial", "unavailable", "failed")
  if (!is.character(primary_nominations$status) || length(primary_nominations$status) != 1L ||
      is.na(primary_nominations$status) || !primary_nominations$status %in% valid_statuses ||
      (identical(primary_nominations$status, "complete") && !length(primary_nominations$nominations))) {
    triomics_integration_abort(
      "C_EXPORT_NOMINATION_INVALID", "Primary-statistics nomination status must match its immutable source artifact.",
      key = "primary_nominations.status"
    )
  }
  required_fields <- c("nomination_id", "feature_key", "source_branch", "result_ref",
                       "selection_rule", "selection_value", "q_threshold", "effect_threshold")
  for (nomination in primary_nominations$nominations) {
    if (!is.list(nomination) || any(!required_fields %in% names(nomination)) ||
        !triomics_sha256_text(nomination$nomination_id) ||
        !triomics_c06_nonempty_string(nomination$feature_key) ||
        !is.list(nomination$result_ref) || !triomics_c06_nonempty_string(nomination$source_branch)) {
      triomics_integration_abort(
        "C_EXPORT_NOMINATION_INVALID", "A primary-statistics nomination lacks its family and selection lineage.",
        key = "primary_nominations"
      )
    }
  }
  list(
    status = as.character(primary_nominations$status),
    source_artifact_id = primary_nominations$source_artifact_id %||% NULL,
    nominations = primary_nominations$nominations
  )
}

triomics_c06_integration_nominations <- function(integration_evidence) {
  nominations <- list()
  for (evidence in integration_evidence) {
    if (!identical(evidence$status, "complete")) next
    selected <- evidence$selection$selected
    for (block_id in names(selected)) {
      for (feature_key in selected[[block_id]]) {
        selected_value <- evidence$selection$selected_values[[block_id]][[feature_key]]
        nomination_context <- list(
          integration_id = evidence$integration_id,
          backend = evidence$backend,
          block_id = block_id,
          feature_key = feature_key,
          selection_rule = evidence$selection$rule,
          loading = selected_value$component_loadings
        )
        nominations[[length(nominations) + 1L]] <- list(
          nomination_id = triomics_hash_text(triomics_canonical_json(nomination_context)),
          feature_key = feature_key,
          source_branch = "integration_selection",
          result_ref = list(
            integration_id = evidence$integration_id,
            backend = evidence$backend,
            block_id = block_id,
            component_ids = evidence$common$scores$columns
          ),
          selection_rule = evidence$selection$rule,
          selection_value = list(
            block_id = block_id,
            component_loadings = selected_value$component_loadings
          ),
          q_threshold = NULL,
          effect_threshold = NULL,
          filter_role = "integration_selection_only"
        )
      }
    }
  }
  nominations
}

triomics_c06_backend_statuses <- function(statuses, integration_runs) {
  methods <- c("concat_pca", "concat_plsda", "concat_pls", "mixomics_diablo",
               "mixomics_block_spls", "mofa2")
  if (is.null(statuses)) statuses <- list()
  if (!is.list(statuses)) {
    triomics_integration_abort(
      "C_EXPORT_STATUS_INVALID", "Backend dispositions must be a named list.", key = "backend_statuses"
    )
  }
  if (length(statuses) && (is.null(names(statuses)) || anyDuplicated(names(statuses)) ||
                           any(!names(statuses) %in% methods))) {
    triomics_integration_abort(
      "C_EXPORT_STATUS_INVALID", "Backend dispositions must use unique registered method ids.",
      key = "backend_statuses"
    )
  }
  run_methods <- vapply(integration_runs, function(run) {
    if (!is.character(run$method) || length(run$method) != 1L) "" else run$method
  }, character(1))
  if (any(!run_methods %in% methods)) {
    triomics_integration_abort(
      "C_EXPORT_STATUS_INVALID", "Integration runs must use an existing C backend identity.",
      key = "integration_runs.method"
    )
  }
  output <- stats::setNames(vector("list", length(methods)), methods)
  for (method in methods) {
    supplied <- statuses[[method]]
    run_indexes <- which(run_methods == method)
    if (is.null(supplied)) {
      current <- if (length(run_indexes)) as.character(integration_runs[[run_indexes[[1L]]]]$status) else "not_requested"
      supplied <- list(status = current)
    }
    valid <- c("not_requested", "not_started", "running", "complete", "partial", "unavailable", "failed")
    if (!is.list(supplied) || !is.character(supplied$status) || length(supplied$status) != 1L ||
        is.na(supplied$status) || !supplied$status %in% valid) {
      triomics_integration_abort(
        "C_EXPORT_STATUS_INVALID", paste0("Backend '", method, "' needs an explicit valid disposition."),
        key = method
      )
    }
    if (identical(supplied$status, "complete") &&
        (!length(run_indexes) || any(vapply(integration_runs[run_indexes], function(run) {
          !identical(run$status, "complete")
        }, logical(1))))) {
      triomics_integration_abort(
        "C_EXPORT_STATUS_INVALID", paste0("Backend '", method, "' cannot be complete without completed integration evidence."),
        key = method
      )
    }
    output[[method]] <- supplied
  }
  output
}

triomics_c06_stage_status <- function(integration_runs, integration_evidence) {
  if (!length(integration_runs) || !length(integration_evidence)) return("unavailable")
  required <- vapply(integration_runs, function(run) isTRUE(run$required), logical(1))
  run_statuses <- vapply(integration_runs, function(run) {
    if (!is.character(run$status) || length(run$status) != 1L) "failed" else run$status
  }, character(1))
  if (any(required & run_statuses != "complete")) {
    if (all(run_statuses[required] %in% c("unavailable", "failed"))) return("unavailable")
    return("partial")
  }
  if (!any(vapply(integration_evidence, function(item) identical(item$status, "complete"), logical(1)))) {
    return("unavailable")
  }
  "complete"
}

triomics_c06_build_handoff <- function(primary_nominations = NULL,
                                       integration_runs = list(),
                                       evaluation_evidence,
                                       backend_statuses = list(),
                                       clinical_prediction = NULL) {
  if (!is.list(integration_runs)) {
    triomics_integration_abort("C_EXPORT_INPUT_INVALID", "Integration runs must be a list.", key = "integration_runs")
  }
  evidence <- lapply(integration_runs, function(run) {
    if (identical(run$status, "complete")) {
      triomics_c06_export_integration(run)
    } else {
      list(
        schema_version = "2.0", artifact_type = "integration_evidence",
        integration_id = run$integration_id %||% run$method %||% "unknown",
        backend = run$method %||% "unknown", status = run$status %||% "failed",
        reason_code = run$reason_code %||% NULL, reason = run$reason %||% NULL
      )
    }
  })
  if (is.null(evaluation_evidence)) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", "C-07 evaluation evidence or explicit unrun controls are required.",
      key = "evaluation_evidence"
    )
  }
  evaluation <- triomics_c06_evaluation_evidence(evaluation_evidence)
  if (length(evidence)) {
    study_ids <- unique(vapply(evidence, function(item) {
      if (!is.null(item$context$study_id)) item$context$study_id else ""
    }, character(1)))
    analysis_ids <- unique(vapply(evidence, function(item) {
      if (!is.null(item$context$analysis_id)) item$context$analysis_id else ""
    }, character(1)))
    study_values <- study_ids[nzchar(study_ids)]
    analysis_values <- analysis_ids[nzchar(analysis_ids)]
    study_id <- if (length(study_values)) study_values[[1L]] else primary_nominations$study_id
    analysis_id <- if (length(analysis_values)) analysis_values[[1L]] else primary_nominations$analysis_id
    if (is.null(study_id)) study_id <- "unknown"
    if (is.null(analysis_id)) analysis_id <- "unknown"
    if (length(study_ids[nzchar(study_ids)]) > 1L || length(analysis_ids[nzchar(analysis_ids)]) > 1L) {
      triomics_integration_abort(
        "C_EXPORT_CONTEXT_MISMATCH", "One D handoff cannot combine different studies or analyses.", key = "integration_runs"
      )
    }
  } else {
    study_id <- if (!is.null(primary_nominations$study_id)) primary_nominations$study_id else "unknown"
    analysis_id <- if (!is.null(primary_nominations$analysis_id)) primary_nominations$analysis_id else "unknown"
  }
  primary <- triomics_c06_primary_branch(primary_nominations, study_id, analysis_id)
  integration_nominations <- triomics_c06_integration_nominations(evidence)
  integration_branch <- list(
    status = if (length(integration_nominations)) "complete" else "unavailable",
    source_artifact_ids = as.list(vapply(evidence, `[[`, character(1), "integration_id")),
    nominations = integration_nominations
  )
  combined_nominations <- c(primary$nominations, integration_nominations)
  nomination_status <- if (length(combined_nominations)) "complete" else "unavailable"
  backend_statuses <- triomics_c06_backend_statuses(backend_statuses, integration_runs)
  if (is.null(clinical_prediction)) {
    clinical_prediction <- list(
      status = "not_started",
      completion_policy = "conditional",
      packet_id = "C-09",
      model_execution = "not_started",
      reason = "Conditional matched clinical prediction has not been executed."
    )
  }
  clinical_valid <- c("not_requested", "not_started", "running", "complete", "partial", "unavailable", "failed")
  if (!is.list(clinical_prediction) || !is.character(clinical_prediction$status) ||
      length(clinical_prediction$status) != 1L || is.na(clinical_prediction$status) ||
      !clinical_prediction$status %in% clinical_valid ||
      !identical(clinical_prediction$completion_policy, "conditional")) {
    triomics_integration_abort(
      "C_EXPORT_STATUS_INVALID", "C-09 clinical prediction status and conditional policy must remain explicit.",
      key = "clinical_prediction"
    )
  }
  if (identical(clinical_prediction$status, "complete") &&
      !triomics_c06_nonempty_string(clinical_prediction$artifact_ref)) {
    triomics_integration_abort(
      "C_EXPORT_STATUS_INVALID", "A complete conditional clinical prediction needs an actual evidence artifact reference.",
      key = "clinical_prediction.artifact_ref"
    )
  }
  status <- triomics_c06_stage_status(integration_runs, evidence)
  comparisons <- if (length(evidence) >= 2L && all(vapply(evidence, function(item) identical(item$status, "complete"), logical(1)))) {
    triomics_c06_compare_integrations(integration_runs)
  } else {
    list(status = "not_comparable", reason = if (length(evidence) < 2L) "fewer_than_two_completed_integrations" else "incomplete_integration_evidence", pairs = list())
  }
  list(
    schema_version = "2.0",
    artifact_type = "stage_c_d_handoff",
    status = status,
    study_id = study_id,
    analysis_id = analysis_id,
    nomination_status = nomination_status,
    nomination_branches = list(
      primary_statistics = primary,
      integration_selection = integration_branch
    ),
    nominations = combined_nominations,
    integration_evidence = evidence,
    comparisons = comparisons,
    evaluation_evidence = evaluation,
    backend_statuses = backend_statuses,
    clinical_prediction = clinical_prediction
  )
}

triomics_c06_write_handoff <- function(handoff, output_root, config_sha256,
                                       producer_commit = "not-recorded") {
  if (!is.list(handoff) || !identical(handoff$artifact_type, "stage_c_d_handoff") ||
      !triomics_c06_nonempty_string(output_root) ||
      !triomics_sha256_text(config_sha256) || !triomics_c06_nonempty_string(producer_commit)) {
    triomics_integration_abort(
      "C_EXPORT_INPUT_INVALID", "A validated C handoff, local output root and configuration SHA256 are required."
    )
  }
  root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
  d_root <- file.path(root, "D")
  dir.create(d_root, recursive = TRUE, showWarnings = FALSE)
  candidate_payload <- list(
    schema_version = "2.0",
    artifact_type = "candidate_nominations",
    study_id = handoff$study_id,
    analysis_id = handoff$analysis_id,
    status = handoff$nomination_status,
    source_artifact_id = "stage-c-d-handoff",
    n_nominations = as.integer(length(handoff$nominations)),
    branches = handoff$nomination_branches,
    nominations = handoff$nominations
  )
  integration_payload <- list(
    schema_version = "2.0", artifact_type = "integration_evidence_bundle",
    study_id = handoff$study_id, analysis_id = handoff$analysis_id,
    status = handoff$status, integrations = handoff$integration_evidence
  )
  comparison_payload <- list(
    schema_version = "2.0", artifact_type = "integration_comparisons",
    study_id = handoff$study_id, analysis_id = handoff$analysis_id,
    status = handoff$comparisons$status, comparisons = handoff$comparisons
  )
  evaluation_payload <- handoff$evaluation_evidence
  evaluation_payload$study_id <- handoff$study_id
  evaluation_payload$analysis_id <- handoff$analysis_id
  status_payload <- list(
    schema_version = "2.0", artifact_type = "stage_c_status",
    study_id = handoff$study_id, analysis_id = handoff$analysis_id,
    status = handoff$status, backend_statuses = handoff$backend_statuses,
    clinical_prediction = handoff$clinical_prediction
  )
  payloads <- list(
    list(name = "candidate-nominations.json", type = "candidate_nominations", value = candidate_payload, status = candidate_payload$status),
    list(name = "integration-evidence.json", type = "integration_evidence_bundle", value = integration_payload, status = integration_payload$status),
    list(name = "integration-comparisons.json", type = "integration_comparisons", value = comparison_payload, status = comparison_payload$status),
    list(name = "evaluation-evidence.json", type = "integration_evaluation_evidence", value = evaluation_payload, status = evaluation_payload$status),
    list(name = "stage-status.json", type = "stage_c_status", value = status_payload, status = status_payload$status)
  )
  inputs <- list()
  for (evidence in handoff$integration_evidence) {
    hashes <- evidence$eligibility$matrix_sha256
    if (is.list(hashes)) {
      for (block_id in names(hashes)) {
        value <- hashes[[block_id]]
        if (is.character(value) && length(value) == 1L && triomics_sha256_text(value)) {
          inputs[[length(inputs) + 1L]] <- list(
            artifact_id = paste(evidence$integration_id, block_id, sep = "::"),
            sha256 = value
          )
        }
      }
    }
  }
  artifacts <- lapply(payloads, function(item) {
    path <- file.path(d_root, item$name)
    envelope_path <- file.path(d_root, sub("\\.json$", ".envelope.json", item$name))
    triomics_write_json_atomic(item$value, path)
    envelope <- triomics_artifact_envelope(
      artifact_type = item$type,
      artifact_id = paste0("c06-", sub("\\.json$", "", item$name)),
      study_id = handoff$study_id,
      run_id = "c-06",
      status = triomics_c06_envelope_status(item$status),
      config_sha256 = config_sha256,
      payload = path,
      producer_commit = producer_commit,
      inputs = inputs,
      reason_codes = character(),
      privacy_class = "synthetic"
    )
    envelope$analysis_id <- handoff$analysis_id
    triomics_write_envelope(envelope, envelope_path)
    list(
      artifact_type = item$type,
      payload_file = item$name,
      payload_sha256 = triomics_sha256(path),
      envelope_file = basename(envelope_path),
      envelope_sha256 = triomics_sha256(envelope_path)
    )
  })
  manifest <- list(
    schema_version = "2.0",
    artifact_type = "stage_c_handoff_manifest",
    study_id = handoff$study_id,
    analysis_id = handoff$analysis_id,
    status = handoff$status,
    artifacts = artifacts
  )
  manifest_path <- file.path(d_root, "handoff-manifest.json")
  triomics_write_json_atomic(manifest, manifest_path)
  manifest_envelope <- triomics_artifact_envelope(
    artifact_type = "stage_c_handoff_manifest",
    artifact_id = "c06-handoff-manifest",
    study_id = handoff$study_id,
    run_id = "c-06",
    status = triomics_c06_envelope_status(handoff$status),
    config_sha256 = config_sha256,
    payload = manifest_path,
    producer_commit = producer_commit,
    inputs = inputs,
    reason_codes = character(),
    privacy_class = "synthetic"
  )
  manifest_envelope$analysis_id <- handoff$analysis_id
  triomics_write_envelope(manifest_envelope, file.path(d_root, "handoff-manifest.envelope.json"))
  list(
    directory = d_root,
    manifest = manifest_path,
    manifest_sha256 = triomics_sha256(manifest_path),
    artifacts = artifacts
  )
}
