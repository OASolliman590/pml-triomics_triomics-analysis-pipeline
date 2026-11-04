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

triomics_c06_artifact_ref <- function(value, label) {
  if (!is.list(value) || !triomics_c06_nonempty_string(value$artifact_id) ||
      !triomics_sha256_text(value$sha256)) {
    triomics_integration_abort(
      "C_EXPORT_LINEAGE_INVALID",
      paste0(label, " must identify an immutable artifact by artifact_id and payload SHA256."),
      key = label
    )
  }
  list(artifact_id = value$artifact_id, sha256 = value$sha256)
}

triomics_c06_lineage_inputs <- function(inputs, label = "lineage_inputs") {
  if (!is.list(inputs)) {
    triomics_integration_abort(
      "C_EXPORT_LINEAGE_INVALID", "Artifact lineage inputs must be a list.", key = label
    )
  }
  checked <- lapply(seq_along(inputs), function(index) {
    triomics_c06_artifact_ref(inputs[[index]], paste0(label, "[", index, "]"))
  })
  ids <- vapply(checked, `[[`, character(1), "artifact_id")
  if (anyDuplicated(ids)) {
    duplicates <- unique(ids[duplicated(ids)])
    for (artifact_id in duplicates) {
      hashes <- vapply(checked[ids == artifact_id], `[[`, character(1), "sha256")
      if (length(unique(hashes)) != 1L) {
        triomics_integration_abort(
          "C_EXPORT_LINEAGE_INVALID",
          paste0("Artifact lineage '", artifact_id, "' is bound to conflicting hashes."),
          key = label
        )
      }
    }
    checked <- checked[!duplicated(ids)]
  }
  unname(checked)
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
  native_pca_scores <- if (identical(method, "concat_pca")) {
    if (is.matrix(state$training_scores)) state$training_scores else result$scores
  } else {
    NULL
  }
  native_loadings <- state$native_loadings
  if (is.null(native_loadings)) native_loadings <- state$block_loadings
  if (is.null(native_loadings)) native_loadings <- state$weights
  if (is.null(native_loadings) && identical(method, "concat_pca")) native_loadings <- result$loadings
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
  state_sha256 <- metadata$state_hash %||% result$params$training_state_hash %||%
    result$params$state_sha256
  if (is.null(state_sha256) && identical(method, "concat_pca")) {
    immutable_state <- state$state %||% state
    state_sha256 <- triomics_hash_text(triomics_canonical_json(immutable_state))
  }
  native_method <- metadata$native_method %||% result$params$native_method
  if (is.null(native_method) && identical(method, "concat_pca")) native_method <- "stats::prcomp"
  native_version <- metadata$native_version %||% metadata$backend_version %||%
    result$params$native_version
  payload <- list(
    method = method,
    native_method = native_method,
    native_version = native_version,
    state_sha256 = state_sha256,
    model_role = metadata$model_role %||% if (identical(method, "mofa2")) "exploratory_structure_discovery" else "integration_fit",
    training_unit_ids = result$params$training_unit_ids %||% result$params$provenance$unit_map,
    scores_by_view = if (is.list(native_scores)) {
      triomics_c06_matrix_list_payload(native_scores[intersect(block_ids, names(native_scores))], "native_scores")
    } else NULL,
    scores = if (is.matrix(state$factors)) {
      triomics_c06_matrix_payload(state$factors, "native_factors")
    } else if (identical(method, "concat_pca") && is.matrix(native_pca_scores)) {
      triomics_c06_matrix_payload(native_pca_scores, "native_pca_scores")
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
  definition <- triomics_c06_variance_definition(result)
  if (identical(result$backend, "mofa2")) {
    definition$metric <- "MOFA2::get_variance_explained$r2_per_factor"
    definition$units <- "percent"
    definition$source <- "MOFA2::get_variance_explained(r2_per_factor)"
  }
  definition
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
  native_artifact_ref <- triomics_c06_artifact_ref(
    integration_run$native_artifact_ref, "integration_run.native_artifact_ref"
  )
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
    native_artifact_ref = native_artifact_ref,
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
      is.null(primary_nominations$source_artifact_ref) ||
      !identical(as.character(primary_nominations$study_id), study_id) ||
      !identical(as.character(primary_nominations$analysis_id), analysis_id)) {
    triomics_integration_abort(
      "C_EXPORT_NOMINATION_INVALID", "The primary-statistics nomination artifact must match the D handoff study and analysis.",
      key = "primary_nominations"
    )
  }
  valid_statuses <- c("not_requested", "not_started", "running", "complete", "partial", "unavailable", "failed")
  if (!is.character(primary_nominations$status) || length(primary_nominations$status) != 1L ||
      is.na(primary_nominations$status) || !primary_nominations$status %in% valid_statuses) {
    triomics_integration_abort(
      "C_EXPORT_NOMINATION_INVALID", "Primary-statistics nomination status must match its immutable source artifact.",
      key = "primary_nominations.status"
    )
  }
  source_artifact_ref <- triomics_c06_artifact_ref(
    primary_nominations$source_artifact_ref, "primary_nominations.source_artifact_ref"
  )
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
    source_artifact_ref = source_artifact_ref,
    preparation_context_id = primary_nominations$preparation_context_id %||% NULL,
    representation_id = primary_nominations$representation_id %||% NULL,
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

triomics_c06_handoff_context <- function(integration_evidence, primary_nominations, evaluation) {
  contexts <- lapply(integration_evidence, function(item) {
    if (identical(item$status, "complete")) item$context else NULL
  })
  contexts <- Filter(function(context) !is.null(context), contexts)
  if (is.list(primary_nominations)) {
    primary_context <- list(
      preparation_context_id = primary_nominations$preparation_context_id,
      representation_id = primary_nominations$representation_id
    )
    if (any(vapply(primary_context, triomics_c06_nonempty_string, logical(1)))) {
      contexts <- c(contexts, list(primary_context))
    }
  }
  evaluation_contexts <- lapply(evaluation$controls, function(control) {
    control$candidate$match_context[c("preparation_context_id", "representation_id")]
  })
  contexts <- c(contexts, evaluation_contexts)
  if (!length(contexts)) {
    triomics_integration_abort(
      "C_EXPORT_CONTEXT_MISSING",
      "The D handoff needs immutable preparation and representation identity.",
      key = "handoff.context"
    )
  }
  preparation_ids <- vapply(contexts, function(context) {
    value <- context$preparation_context_id
    if (!triomics_c06_nonempty_string(value)) "" else value
  }, character(1))
  representation_ids <- vapply(contexts, function(context) {
    value <- context$representation_id
    if (!triomics_c06_nonempty_string(value)) "" else value
  }, character(1))
  if (any(!nzchar(preparation_ids)) || length(unique(preparation_ids)) != 1L ||
      any(!nzchar(representation_ids)) || length(unique(representation_ids)) != 1L) {
    triomics_integration_abort(
      "C_EXPORT_CONTEXT_MISMATCH",
      "Primary nominations, integrations and C-07 evaluation must share preparation and representation identity.",
      key = "handoff.context"
    )
  }
  list(
    preparation_context_id = preparation_ids[[1L]],
    representation_id = representation_ids[[1L]]
  )
}

triomics_c06_evaluation_lineage <- function(evaluation) {
  required_names <- triomics_c06_control_names()
  controls <- evaluation$controls
  if (!is.list(evaluation) || !is.list(controls) ||
      length(controls) != length(required_names) ||
      !is.list(evaluation$source_artifacts)) {
    triomics_integration_abort(
      "C_EXPORT_LINEAGE_INVALID",
      "Evaluation lineage requires all four control records and their declared source artifacts.",
      key = "evaluation_evidence"
    )
  }
  observed <- list()
  for (index in seq_along(required_names)) {
    control <- controls[[index]]
    if (!is.list(control) || !identical(control$control, required_names[[index]])) {
      triomics_integration_abort(
        "C_EXPORT_LINEAGE_INVALID",
        "Evaluation controls must retain their canonical order and identity.",
        key = "evaluation_evidence.controls"
      )
    }
    for (side in c("candidate", "baseline")) {
      observation <- control[[side]]
      if (!is.list(observation)) {
        triomics_integration_abort(
          "C_EXPORT_LINEAGE_INVALID",
          "Each evaluation control must preserve candidate and baseline observations.",
          key = paste0("evaluation_evidence.controls.", required_names[[index]], ".", side)
        )
      }
      status <- observation$status
      if (!is.character(status) || length(status) != 1L || is.na(status)) {
        triomics_integration_abort(
          "C_EXPORT_LINEAGE_INVALID",
          "Evaluation observation status must be explicit before lineage can be verified.",
          key = paste0("evaluation_evidence.controls.", required_names[[index]], ".", side)
        )
      }
      if (identical(status, "complete")) {
        observed[[length(observed) + 1L]] <- triomics_c06_artifact_ref(
          list(artifact_id = observation$artifact_ref, sha256 = observation$artifact_sha256),
          paste0("evaluation_evidence.controls.", required_names[[index]], ".", side)
        )
      } else if (!status %in% c("not_run", "unavailable", "failed") ||
                 !is.null(observation$artifact_ref) || !is.null(observation$artifact_sha256)) {
        triomics_integration_abort(
          "C_EXPORT_LINEAGE_INVALID",
          "Only complete evaluation records may bind source artifact references.",
          key = paste0("evaluation_evidence.controls.", required_names[[index]], ".", side)
        )
      }
    }
  }
  observed <- triomics_c06_lineage_inputs(observed, "evaluation_evidence.controls")
  declared <- triomics_c06_lineage_inputs(
    evaluation$source_artifacts, "evaluation_evidence.source_artifacts"
  )
  if (!identical(observed, declared)) {
    triomics_integration_abort(
      "C_EXPORT_LINEAGE_MISMATCH",
      "Evaluation source references do not match the complete candidate/baseline control records.",
      key = "evaluation_evidence.source_artifacts"
    )
  }
  observed
}

triomics_c06_handoff_lineage <- function(primary_nominations, integration_runs, evaluation) {
  inputs <- list()
  if (is.list(primary_nominations) && !is.null(primary_nominations$source_artifact_ref)) {
    inputs <- c(inputs, list(primary_nominations$source_artifact_ref))
  }
  for (run in integration_runs) {
    if (identical(run$status, "complete")) {
      inputs <- c(inputs, list(run$native_artifact_ref))
    }
  }
  inputs <- c(inputs, triomics_c06_evaluation_lineage(evaluation))
  triomics_c06_lineage_inputs(inputs, "handoff.lineage_inputs")
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
  handoff_context <- triomics_c06_handoff_context(evidence, primary, evaluation)
  lineage_inputs <- triomics_c06_handoff_lineage(primary, integration_runs, evaluation)
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
    preparation_context_id = handoff_context$preparation_context_id,
    representation_id = handoff_context$representation_id,
    nomination_status = nomination_status,
    nomination_branches = list(
      primary_statistics = primary,
      integration_selection = integration_branch
    ),
    nominations = combined_nominations,
    integration_evidence = evidence,
    lineage_inputs = lineage_inputs,
    comparisons = comparisons,
    evaluation_evidence = evaluation,
    backend_statuses = backend_statuses,
    clinical_prediction = clinical_prediction
  )
}

triomics_c06_attach_primary_nominations <- function(handoff, primary_nominations) {
  if (!is.list(handoff) || !identical(handoff$artifact_type, "stage_c_d_handoff") ||
      !is.list(handoff$integration_evidence) || !is.list(handoff$evaluation_evidence)) {
    triomics_integration_abort(
      "C_EXPORT_INPUT_INVALID", "A validated saved C integration handoff is required for B nomination attachment.",
      key = "handoff"
    )
  }
  primary <- triomics_c06_primary_branch(
    primary_nominations, as.character(handoff$study_id), as.character(handoff$analysis_id)
  )
  context <- triomics_c06_handoff_context(
    handoff$integration_evidence, primary, handoff$evaluation_evidence
  )
  integration_nominations <- triomics_c06_integration_nominations(handoff$integration_evidence)
  source_runs <- lapply(handoff$integration_evidence, function(evidence) {
    list(status = evidence$status, native_artifact_ref = evidence$native_artifact_ref %||% NULL)
  })
  handoff$preparation_context_id <- context$preparation_context_id
  handoff$representation_id <- context$representation_id
  handoff$nomination_branches$primary_statistics <- primary
  handoff$nomination_branches$integration_selection <- list(
    status = if (length(integration_nominations)) "complete" else "unavailable",
    source_artifact_ids = lapply(handoff$integration_evidence, `[[`, "integration_id"),
    nominations = integration_nominations
  )
  handoff$nominations <- c(primary$nominations, integration_nominations)
  handoff$nomination_status <- if (length(handoff$nominations)) "complete" else "unavailable"
  handoff$lineage_inputs <- triomics_c06_handoff_lineage(
    primary, source_runs, handoff$evaluation_evidence
  )
  handoff
}

triomics_c06_provenance_abort <- function(code, message, key = "provenance") {
  triomics_integration_abort(code, message, key = key)
}

triomics_c06_validate_b_predecessors <- function(output_root, handoff, config_sha256,
                                                  b_predecessor_paths, c_input_identity) {
  fail <- function(code, message, key = "B/handoffs") {
    triomics_c06_provenance_abort(code, message, key)
  }
  root <- tryCatch(triomics_b07_output_root(output_root, create = FALSE),
                   error = function(error) NULL)
  required_names <- c("bundle_path", "bundle_envelope_path", "provenance_path",
                      "provenance_envelope_path")
  if (is.null(root) || !is.list(b_predecessor_paths) ||
      !identical(sort(names(b_predecessor_paths)), sort(required_names)) ||
      !is.list(c_input_identity)) {
    fail("C_EXPORT_PREDECESSOR_MISSING",
         "C-06 publication requires one native B handoff and the same-run resolved C input identity.")
  }
  safe_path <- function(path) {
    if (!triomics_c06_nonempty_string(path)) return(NULL)
    relative <- tryCatch(triomics_b07_relative(root, path), error = function(error) NULL)
    if (is.null(relative)) return(NULL)
    tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
  }
  paths <- lapply(b_predecessor_paths, safe_path)
  if (any(vapply(paths, is.null, logical(1L))) ||
      any(!vapply(paths, file.exists, logical(1L))) ||
      any(vapply(paths, dir.exists, logical(1L))) ||
      any(vapply(paths, triomics_is_symlink, logical(1L)))) {
    fail("C_EXPORT_PREDECESSOR_MISSING",
         "C-06 publication requires four regular, immutable B handoff and provenance files.")
  }
  bundle_path <- paths$bundle_path
  expected <- list(
    bundle_path = bundle_path,
    bundle_envelope_path = sub("[.]json$", ".envelope.json", bundle_path),
    provenance_path = file.path(dirname(bundle_path), "stage-handoff-provenance.json"),
    provenance_envelope_path = file.path(dirname(bundle_path), "stage-handoff-provenance.envelope.json")
  )
  expected_match <- vapply(required_names, function(name) {
    identical(paths[[name]], expected[[name]])
  }, logical(1L))
  if (!all(expected_match)) {
    fail("C_EXPORT_PREDECESSOR_PATH",
         "The four B predecessor paths must be the bundle payload, its envelope, and its provenance pair.")
  }
  handoff_root <- tryCatch(triomics_b07_child_path(root, file.path("B", "handoffs")),
                           error = function(error) NULL)
  bundle_paths <- if (!is.null(handoff_root) && dir.exists(handoff_root)) {
    list.files(handoff_root, pattern = "^bundle[.]json$", recursive = TRUE,
               full.names = TRUE, all.files = FALSE)
  } else character()
  if (length(bundle_paths) != 1L ||
      !identical(normalizePath(bundle_paths[[1L]], winslash = "/", mustWork = TRUE), bundle_path)) {
    fail("C_EXPORT_PREDECESSOR_AMBIGUOUS",
         "C-06 publication requires exactly one native B handoff in this output root.")
  }
  candidate <- tryCatch(triomics_b07_resume_candidate(bundle_path), error = function(error) NULL)
  bundle <- tryCatch(jsonlite::read_json(bundle_path, simplifyVector = FALSE), error = function(error) NULL)
  bundle_envelope_path <- paths$bundle_envelope_path
  bundle_envelope <- tryCatch(jsonlite::read_json(bundle_envelope_path, simplifyVector = FALSE),
                              error = function(error) NULL)
  profile_path <- paths$provenance_path
  profile <- tryCatch(jsonlite::read_json(profile_path, simplifyVector = FALSE), error = function(error) NULL)
  provenance_envelope_path <- paths$provenance_envelope_path
  provenance_envelope <- tryCatch(jsonlite::read_json(provenance_envelope_path, simplifyVector = FALSE),
                                  error = function(error) NULL)
  if (!is.list(candidate) || !identical(candidate$status, "complete") ||
      !isTRUE(candidate$payload_present) || !isTRUE(candidate$envelope_present) ||
      !isTRUE(candidate$base_envelope_valid) || !isTRUE(candidate$payload_hash_matches) ||
      !isTRUE(candidate$required_evidence_complete) ||
      !is.list(bundle) || !is.list(bundle_envelope) || !is.list(profile) ||
      !is.list(provenance_envelope) ||
      !isTRUE(triomics_validate_envelope(bundle_envelope_path)) ||
      !isTRUE(triomics_validate_envelope(provenance_envelope_path)) ||
      !isTRUE(triomics_b07_validate_provenance_artifacts(bundle, bundle_path, bundle_envelope_path)) ||
      !isTRUE(tryCatch(triomics_validate_stage_handoff_provenance(
        profile, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
      ), error = function(error) FALSE))) {
    fail("C_EXPORT_PREDECESSOR_INVALID",
         "The native B handoff or its provenance failed existing byte, identity, or lineage validation.")
  }
  analysis_id <- c_input_identity$analysis_id
  preparation_id <- c_input_identity$preparation_context_id
  representation_id <- c_input_identity$representation_id
  identity_values <- list(
    study_id = c_input_identity$study_id,
    analysis_id = analysis_id,
    preparation_context_id = preparation_id,
    representation_id = representation_id
  )
  if (!identical(as.character(bundle$config_sha256), as.character(config_sha256)) ||
      !identical(as.character(profile$producer$effective_configuration_sha256), as.character(config_sha256)) ||
      !identical(as.character(profile$study_id), as.character(identity_values$study_id)) ||
      !identical(as.character(bundle$study_id), as.character(identity_values$study_id)) ||
      !identical(as.character(bundle$analysis_id), as.character(analysis_id)) ||
      !identical(as.character(bundle$preparation_context_id), as.character(preparation_id)) ||
      !identical(as.character(bundle$representation_id), as.character(representation_id)) ||
      !identical(as.character(handoff$study_id), as.character(identity_values$study_id)) ||
      !identical(as.character(handoff$analysis_id), as.character(analysis_id)) ||
      !identical(as.character(handoff$preparation_context_id), as.character(preparation_id)) ||
      !identical(as.character(handoff$representation_id), as.character(representation_id))) {
    fail("C_EXPORT_IDENTITY_MISMATCH",
         "Native B and C handoff study, analysis, preparation, representation, or config identity differs.")
  }
  source_inputs <- c_input_identity$source_inputs
  source_entities <- triomics_b07_provenance_entities(profile, "source_manifest")
  expected_source_ids <- if (is.list(source_inputs)) vapply(source_inputs, function(input) {
    triomics_b07_prov_entity_id("source_manifest", list(
      source_id = as.character(input$artifact_id), sha256 = as.character(input$sha256)
    ))
  }, character(1L)) else character()
  actual_source_ids <- vapply(source_entities, `[[`, character(1L), "entity_id")
  normalized_metadata_entities <- triomics_b07_provenance_entities(profile, "normalized_metadata")
  analysis_unit_map_entities <- triomics_b07_provenance_entities(profile, "analysis_unit_map")
  current_metadata_digest <- triomics_b07_prov_value_sha256(c_input_identity$normalized_metadata)
  current_unit_map_digest <- triomics_b07_prov_value_sha256(c_input_identity$analysis_unit_map)
  if (!length(expected_source_ids) || any(!vapply(source_inputs, function(input) {
    is.list(input) && triomics_c06_nonempty_string(input$artifact_id) &&
      triomics_sha256_text(input$sha256)
  }, logical(1L))) || any(!expected_source_ids %in% actual_source_ids) ||
      length(source_entities) != as.integer(c_input_identity$expected_source_entity_count) ||
      length(normalized_metadata_entities) != 1L ||
      !identical(as.character(normalized_metadata_entities[[1L]]$digest), current_metadata_digest) ||
      length(analysis_unit_map_entities) != 1L ||
      !triomics_sha256_text(c_input_identity$analysis_unit_map_sha256) ||
      !identical(as.character(c_input_identity$analysis_unit_map_sha256), current_unit_map_digest) ||
      !identical(as.character(analysis_unit_map_entities[[1L]]$digest), current_unit_map_digest)) {
    fail("C_EXPORT_IDENTITY_MISMATCH",
         "C source metadata and AnalysisUnitMap do not match the validated native B identities.")
  }
  expected_blocks <- c_input_identity$blocks
  source_blocks <- c_input_identity$source_blocks
  b_matrix_member <- Filter(function(member) {
    identical(as.character(member$artifact_id), "stage_c_matrix_values")
  }, bundle$members)
  b_manifest_member <- Filter(function(member) {
    identical(as.character(member$artifact_id), "stage_c_matrices")
  }, bundle$members)
  if (!is.list(expected_blocks) || !length(expected_blocks) ||
      !is.list(source_blocks) || !setequal(names(source_blocks), names(expected_blocks)) ||
      length(b_matrix_member) != 1L || length(b_manifest_member) != 1L) {
    fail("C_EXPORT_IDENTITY_MISMATCH", "The same-run B and C resolved block identities are incomplete.")
  }
  matrix_path <- tryCatch(triomics_b07_child_path(root, as.character(b_matrix_member[[1L]]$path)),
                          error = function(error) NULL)
  manifest_path <- tryCatch(triomics_b07_child_path(root, as.character(b_manifest_member[[1L]]$path)),
                            error = function(error) NULL)
  b_matrices <- if (!is.null(matrix_path)) tryCatch(readRDS(matrix_path), error = function(error) NULL) else NULL
  b_manifest <- if (!is.null(manifest_path)) {
    tryCatch(jsonlite::read_json(manifest_path, simplifyVector = FALSE), error = function(error) NULL)
  } else NULL
  represented <- triomics_b07_provenance_entities(profile, "represented_matrix")
  if (!is.list(b_matrices) || !is.list(b_manifest) ||
      !setequal(names(expected_blocks), names(b_matrices)) ||
      !setequal(names(expected_blocks), names(b_manifest$blocks)) ||
      !setequal(names(expected_blocks), vapply(represented, `[[`, character(1L), "block_id"))) {
    fail("C_EXPORT_IDENTITY_MISMATCH", "The B matrix block set differs from the resolved C block set.")
  }
  for (block_id in names(expected_blocks)) {
    current <- expected_blocks[[block_id]]
    consumed_source <- source_blocks[[block_id]]
    prior <- b_matrices[[block_id]]
    b_declared <- b_manifest$blocks[[block_id]]
    matrix_entity <- Filter(function(entity) identical(as.character(entity$block_id), block_id), represented)
    if (!is.matrix(prior) || !is.matrix(current$matrix) || !is.list(consumed_source) ||
        !is.matrix(consumed_source$matrix) || length(matrix_entity) != 1L ||
        !identical(as.character(b_declared$matrix_hash), triomics_b07_matrix_hash(block_id, prior)) ||
        !identical(as.character(current$matrix_sha256),
                   digest::digest(current$matrix, algo = "sha256", serialize = TRUE)) ||
        !identical(as.character(consumed_source$matrix_sha256),
                   triomics_b07_matrix_hash(block_id, consumed_source$matrix)) ||
        !identical(as.character(consumed_source$matrix_sha256),
                   triomics_b07_matrix_hash(block_id, prior)) ||
        !identical(as.character(current$feature_ids), as.character(rownames(prior))) ||
        !identical(as.character(current$unit_ids), as.character(colnames(prior))) ||
        !identical(as.character(consumed_source$feature_ids), as.character(rownames(prior))) ||
        !identical(as.character(consumed_source$unit_ids), as.character(colnames(prior))) ||
        !identical(as.character(matrix_entity[[1L]]$row_identity_sha256),
                   triomics_b07_prov_ordered_ids_sha256(rownames(prior))) ||
        !identical(as.character(matrix_entity[[1L]]$column_identity_sha256),
                   triomics_b07_prov_ordered_ids_sha256(colnames(prior)))) {
      fail("C_EXPORT_IDENTITY_MISMATCH",
           paste0("Resolved C matrix identity differs from native B for block ", block_id, "."),
           paste0("blocks.", block_id))
    }
  }
  for (evidence in handoff$integration_evidence) {
    if (!identical(evidence$status, "complete")) next
    matrix_hashes <- evidence$identity$matrix_sha256
    if (!is.list(matrix_hashes) || !setequal(names(matrix_hashes), names(expected_blocks)) ||
        any(!vapply(names(expected_blocks), function(block_id) {
          identical(as.character(matrix_hashes[[block_id]]),
                    as.character(expected_blocks[[block_id]]$matrix_sha256))
        }, logical(1L)))) {
      fail("C_EXPORT_IDENTITY_MISMATCH",
           "A completed C integration result does not use the verified B matrix identities.",
           paste0("integration.", evidence$integration_id))
    }
  }
  list(
    root = root,
    paths = paths,
    bundle = bundle,
    bundle_envelope = bundle_envelope,
    profile = profile,
    profile_envelope = provenance_envelope,
    predecessor_hashes = stats::setNames(vapply(paths, triomics_sha256, character(1L)), required_names),
    source_entities = source_entities,
    analysis_unit_map_entity = analysis_unit_map_entities[[1L]],
    baseline_entities = Filter(function(entity) entity$scientific_role %in% c(
      "normalized_metadata", "feature_registry", "analysis_unit_map", "resolved_analysis",
      "represented_matrix", "preprocessing_state"
    ), profile$identity$entities),
    b_matrices = b_matrices
  )
}

triomics_c06_provenance_entity <- function(role, entity_id, value, study_id,
                                           scope = list(), activity_id,
                                           derived_from = character(), member_of = NULL) {
  entity <- triomics_b07_prov_embedded_entity(
    "method_state", entity_id, value, study_id, scope,
    generated_by_activity_id = activity_id,
    derived_from_entity_ids = derived_from,
    schema_uri = paste0("urn:triomics:", gsub("_", "-", role), ":identity:1.0")
  )
  entity$scientific_role <- role
  entity$entity_type <- paste0("triomics.", role)
  entity["member_of_entity_id"] <- list(member_of)
  entity
}

triomics_c06_provenance_artifact_entity <- function(role, entity_id, payload_path,
                                                    envelope_path, study_id,
                                                    scope = list(), activity_id,
                                                    derived_from = character(),
                                                    storage_form = "enveloped_artifact",
                                                    member_of = NULL) {
  entity <- triomics_b07_prov_artifact_entity(
    "predecessor_envelope", entity_id, payload_path, envelope_path, study_id,
    scope = scope, generated_by_activity_id = activity_id,
    derived_from_entity_ids = derived_from, storage_form = storage_form,
    member_of_entity_id = member_of
  )
  entity$scientific_role <- role
  entity$entity_type <- paste0("triomics.", role)
  entity["member_of_entity_id"] <- list(member_of)
  entity
}

triomics_c06_validate_native_preparation_inputs <- function(evidence, native_payload,
                                                            native_envelope, root) {
  fail <- function(message) {
    triomics_c06_provenance_abort("C_EXPORT_PROVENANCE_INVALID", message)
  }
  matrix_hashes <- evidence$identity$matrix_sha256
  envelope_inputs <- native_envelope$inputs
  has_input <- function(artifact_id, sha256) {
    is.list(envelope_inputs) && any(vapply(envelope_inputs, function(input) {
      is.list(input) && identical(as.character(input$artifact_id), artifact_id) &&
        identical(as.character(input$sha256), sha256)
    }, logical(1L)))
  }
  if (!is.list(matrix_hashes) || is.null(names(matrix_hashes)) || !length(matrix_hashes) ||
      any(!vapply(matrix_hashes, triomics_sha256_text, logical(1L))) ||
      any(!vapply(names(matrix_hashes), function(block_id) {
        has_input(paste(evidence$integration_id, block_id, sep = "::"),
                  as.character(matrix_hashes[[block_id]]))
      }, logical(1L)))) {
    fail("The C native sidecar envelope does not bind every current integration matrix hash.")
  }
  state_hashes <- native_payload$preparation_state_hashes %||% list()
  state_refs <- native_payload$preparation_state_refs %||% list()
  if (!is.list(state_hashes) || !is.list(state_refs) ||
      !identical(sort(names(state_hashes)), sort(names(state_refs))) ||
      any(!names(state_hashes) %in% names(matrix_hashes))) {
    fail("C native sidecar preparation states do not align with its fitted matrix identities.")
  }
  if (!length(state_refs)) return(list())
  state_directory <- file.path(root, "C", "preparation-states")
  states <- stats::setNames(lapply(names(state_refs), function(block_id) {
    reference <- state_refs[[block_id]]
    if (!is.list(reference) || !triomics_c06_nonempty_string(reference$path) ||
        !triomics_c06_nonempty_string(reference$state_id) ||
        !identical(as.character(reference$state_hash), as.character(state_hashes[[block_id]]))) {
      fail(paste0("C native sidecar has an invalid preparation reference for ", block_id, "."))
    }
    path <- tryCatch(triomics_b07_child_path(root, as.character(reference$path)),
                     error = function(error) NULL)
    if (is.null(path) || !file.exists(path) || dir.exists(path) || triomics_is_symlink(path) ||
        !identical(triomics_sha256(path), as.character(reference$payload_sha256)) ||
        !has_input(paste("preparation-state", block_id, sep = "::"),
                   as.character(reference$payload_sha256))) {
      fail(paste0("C native sidecar envelope does not bind the persisted preparation state for ", block_id, "."))
    }
    local_reference <- reference
    local_reference$path <- basename(path)
    state <- tryCatch(triomics_read_state(local_reference, state_directory),
                      error = function(error) NULL)
    if (!is.list(state) || !identical(as.character(state$block_id), block_id) ||
        !identical(as.character(state$state_hash), as.character(reference$state_hash))) {
      fail(paste0("Persisted C preparation state failed its native read-back checks for ", block_id, "."))
    }
    list(
      reference = reference,
      path = path,
      state = state
    )
  }), names(state_refs))
  states
}

triomics_c06_provenance_runtime_versions <- function(integration_evidence, root) {
  version <- function(component) {
    if (identical(component, "R")) {
      paste(R.version$major, R.version$minor, sep = ".")
    } else {
      tryCatch(as.character(utils::packageVersion(component)), error = function(error) "not-installed")
    }
  }
  components <- c("R", "V8", "digest", "jsonlite", "jsonvalidate", "triomics")
  methods <- unique(vapply(integration_evidence, function(evidence) {
    as.character(evidence$backend %||% evidence$method %||% "")
  }, character(1L)))
  backend_components <- vapply(methods, function(method) {
    switch(method,
      concat_pca = "stats",
      concat_plsda = "mixOmics",
      concat_pls = "mixOmics",
      mixomics_diablo = "mixOmics",
      mixomics_block_spls = "mixOmics",
      mofa2 = "MOFA2",
      "stats"
    )
  }, character(1L))
  components <- unique(c(components, backend_components))
  runtime <- lapply(components, function(component) {
    list(component = component, version = version(component))
  })
  for (evidence in integration_evidence) {
    reference <- evidence$native_artifact_ref
    if (!is.list(reference) || !triomics_c06_nonempty_string(reference$artifact_id)) next
    native_directory <- file.path(root, "C", "native-states")
    candidates <- if (dir.exists(native_directory)) {
      list.files(native_directory, pattern = "^integration-[0-9]+[.]json$",
                 full.names = TRUE, all.files = FALSE)
    } else character()
    hits <- candidates[vapply(candidates, function(path) {
      payload <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) NULL)
      is.list(payload) && identical(as.character(payload$artifact_id), as.character(reference$artifact_id)) &&
        identical(triomics_sha256(path), as.character(reference$sha256))
    }, logical(1L))]
    if (length(hits) != 1L) next
    payload <- jsonlite::read_json(hits[[1L]], simplifyVector = FALSE)
    native <- payload$native
    if (is.list(native) && triomics_c06_nonempty_string(native$native_method) &&
        triomics_c06_nonempty_string(native$native_version)) {
      runtime[[length(runtime) + 1L]] <- list(
        component = as.character(native$native_method),
        version = as.character(native$native_version)
      )
    }
  }
  keys <- vapply(runtime, function(item) item$component, character(1L))
  runtime[!duplicated(keys)]
}

triomics_c06_build_provenance_profile <- function(handoff, d_root, config_sha256,
                                                   producer_commit, b_predecessors) {
  study_id <- as.character(handoff$study_id)
  analysis_id <- as.character(handoff$analysis_id)
  preparation_id <- as.character(handoff$preparation_context_id)
  representation_id <- as.character(handoff$representation_id)
  scope <- triomics_b07_prov_scope(study_id, analysis_id = analysis_id,
                                   preparation_context_id = preparation_id,
                                   representation_id = representation_id)
  base_roles <- c("source_manifest", "normalized_metadata", "feature_registry",
                  "analysis_unit_map", "resolved_analysis", "represented_matrix",
                  "preprocessing_state")
  entities <- Filter(function(entity) entity$scientific_role %in% base_roles,
                     c(b_predecessors$source_entities, b_predecessors$baseline_entities))
  entities <- lapply(entities, function(entity) {
    entity["generated_by_activity_id"] <- list(NULL)
    entity
  })
  ids_by_role <- function(role) vapply(Filter(function(entity) {
    identical(entity$scientific_role, role)
  }, entities), `[[`, character(1L), "entity_id")
  source_ids <- ids_by_role("source_manifest")
  unit_ids <- ids_by_role("analysis_unit_map")
  matrix_ids <- ids_by_role("represented_matrix")
  resolved_ids <- ids_by_role("resolved_analysis")
  if (!length(source_ids) || length(unit_ids) != 1L || length(resolved_ids) != 1L ||
      !length(matrix_ids)) {
    triomics_c06_provenance_abort(
      "C_EXPORT_PROVENANCE_INVALID", "Validated B provenance lacks C-required upstream identity entities."
    )
  }
  used_upstream <- sort(unique(vapply(entities, `[[`, character(1L), "entity_id")), method = "radix")
  b_bundle_path <- b_predecessors$paths$bundle_path
  b_bundle_envelope_path <- b_predecessors$paths$bundle_envelope_path
  b_provenance_path <- b_predecessors$paths$provenance_path
  b_provenance_envelope_path <- b_predecessors$paths$provenance_envelope_path
  predecessor_specs <- list(
    list(id = "b-bundle", payload = b_bundle_path, envelope = b_bundle_envelope_path),
    list(id = "b-provenance", payload = b_provenance_path, envelope = b_provenance_envelope_path)
  )
  predecessor_payload_ids <- character()
  for (item in predecessor_specs) {
    envelope_json <- tryCatch(jsonlite::read_json(item$envelope, simplifyVector = FALSE),
                              error = function(error) NULL)
    if (!is.list(envelope_json)) {
      triomics_c06_provenance_abort("C_EXPORT_PREDECESSOR_INVALID",
                                    "A verified B predecessor envelope could not be read.")
    }
    artifact_id <- triomics_b07_prov_entity_id("predecessor_envelope", item$id)
    payload_id <- triomics_b07_prov_entity_id("predecessor_payload", item$id)
    entities[[length(entities) + 1L]] <- triomics_c06_provenance_artifact_entity(
      "predecessor_envelope", artifact_id, item$payload, item$envelope, study_id,
      scope = scope, activity_id = NULL
    )
    entities[[length(entities) + 1L]] <- triomics_c06_provenance_artifact_entity(
      "predecessor_payload", payload_id, item$payload, item$envelope, study_id,
      scope = scope, activity_id = NULL, storage_form = "file", member_of = artifact_id
    )
    predecessor_payload_ids <- c(predecessor_payload_ids, payload_id)
  }
  base_ids <- sort(unique(vapply(entities, `[[`, character(1L), "entity_id")), method = "radix")
  c_fit_activities <- list()
  c_export_activity <- "c06-stage-handoff-export"
  cfit_entity_ids <- character()
  c_preparation_activity_id <- "c06-stage-c-preparation"
  c_preparation_entity_ids <- character()
  c_preparation_state_refs <- NULL
  method_ids <- character()
  route_declarations <- list()
  upstream_matrix_ids <- matrix_ids
  for (evidence in handoff$integration_evidence) {
    method_id <- as.character(evidence$backend %||% evidence$method %||% "")
    integration_id <- as.character(evidence$integration_id %||% "")
    if (!triomics_c06_nonempty_string(method_id) || !triomics_c06_nonempty_string(integration_id)) {
      triomics_c06_provenance_abort("C_EXPORT_PROVENANCE_INVALID",
                                    "Every C integration result needs its requested method and integration identity.")
    }
    method_ids <- c(method_ids, method_id)
    fit_activity <- paste0("c06-integration-", integration_id)
    result_id <- triomics_b07_prov_entity_id("integration_result", list(
      analysis_id = analysis_id, method_id = method_id,
      preparation_context_id = preparation_id, representation_id = representation_id
    ))
    result_value <- evidence
    entities[[length(entities) + 1L]] <- triomics_c06_provenance_entity(
      "integration_result", result_id, result_value, study_id,
      triomics_b07_prov_scope(study_id, analysis_id, method_id = method_id,
                              preparation_context_id = preparation_id,
                              representation_id = representation_id),
      activity_id = fit_activity,
      derived_from = sort(unique(upstream_matrix_ids), method = "radix")
    )
    result_entity_index <- length(entities)
    state_id <- triomics_b07_prov_entity_id("integration_state", list(
      analysis_id = analysis_id, method_id = method_id,
      preparation_context_id = preparation_id, representation_id = representation_id
    ))
    state_value <- list(
      integration_id = integration_id,
      method_id = method_id,
      status = as.character(evidence$status),
      reason_code = evidence$reason_code %||% NULL,
      reason = evidence$reason %||% NULL,
      native_artifact_ref = evidence$native_artifact_ref %||% NULL
    )
    native_ref <- evidence$native_artifact_ref
    native_generated <- character()
    if (identical(evidence$status, "complete") && is.list(native_ref)) {
      native_directory <- file.path(b_predecessors$root, "C", "native-states")
      native_payload_candidates <- if (dir.exists(native_directory)) {
        list.files(native_directory, pattern = "^integration-[0-9]+[.]json$",
                   full.names = TRUE, all.files = FALSE)
      } else character()
      native_hits <- native_payload_candidates[vapply(native_payload_candidates, function(path) {
        payload <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) NULL)
        is.list(payload) && identical(as.character(payload$artifact_id), as.character(native_ref$artifact_id)) &&
          identical(triomics_sha256(path), as.character(native_ref$sha256))
      }, logical(1L))]
      native_payload_path <- if (length(native_hits) == 1L) native_hits[[1L]] else NULL
      native_envelope_path <- if (is.null(native_payload_path)) NULL else
        sub("[.]json$", ".envelope.json", native_payload_path)
      if (is.null(native_payload_path) || !file.exists(native_envelope_path) ||
          triomics_is_symlink(native_payload_path) || triomics_is_symlink(native_envelope_path) ||
          !isTRUE(triomics_validate_envelope(native_envelope_path))) {
        triomics_c06_provenance_abort("C_EXPORT_PROVENANCE_INVALID",
                                      "A completed C integration native sidecar differs from its immutable reference.")
      }
      sidecar_envelope <- jsonlite::read_json(native_envelope_path, simplifyVector = FALSE)
      if (!identical(as.character(sidecar_envelope$payload_sha256), as.character(native_ref$sha256)) ||
          !identical(as.character(sidecar_envelope$artifact_id), as.character(native_ref$artifact_id))) {
        triomics_c06_provenance_abort("C_EXPORT_PROVENANCE_INVALID",
                                      "A completed C integration native sidecar failed envelope identity checks.")
      }
      native_payload <- jsonlite::read_json(native_payload_path, simplifyVector = FALSE)
      state_value$native_state <- native_payload
      preparation_states <- triomics_c06_validate_native_preparation_inputs(
        evidence, native_payload, sidecar_envelope, b_predecessors$root
      )
      if (length(preparation_states)) {
        current_state_refs <- lapply(preparation_states, function(item) {
          list(state_id = item$reference$state_id,
               state_hash = item$reference$state_hash,
               payload_sha256 = item$reference$payload_sha256)
        })
        if (is.null(c_preparation_state_refs)) {
          c_preparation_state_refs <- current_state_refs
          for (block_id in names(preparation_states)) {
            item <- preparation_states[[block_id]]
            matrix_entity <- Filter(function(entity) {
              identical(entity$scientific_role, "represented_matrix") &&
                identical(as.character(entity$block_id), block_id)
            }, entities)
            b_prep_entity <- Filter(function(entity) {
              identical(entity$scientific_role, "preprocessing_state") &&
                identical(as.character(entity$block_id), block_id) &&
                is.null(entity$generated_by_activity_id)
            }, entities)
            if (length(matrix_entity) != 1L || length(b_prep_entity) != 1L ||
                is.null(b_predecessors$b_matrices[[block_id]])) {
              triomics_c06_provenance_abort(
                "C_EXPORT_PROVENANCE_INVALID",
                paste0("C preparation state has no unique verified B matrix and preprocessing predecessor for ", block_id, ".")
              )
            }
            matrix_hash <- as.character(evidence$identity$matrix_sha256[[block_id]])
            state <- item$state
            preparation_value <- list(
              producer_stage = "C",
              integration_id = integration_id,
              block_id = block_id,
              preparation_context_id = preparation_id,
              representation_id = representation_id,
              state_id = as.character(item$reference$state_id),
              state_hash = as.character(item$reference$state_hash),
              state_payload_sha256 = as.character(item$reference$payload_sha256),
              state_path = as.character(item$reference$path),
              input_matrix_sha256 = triomics_b07_matrix_hash(
                block_id, b_predecessors$b_matrices[[block_id]]
              ),
              prepared_matrix_sha256 = matrix_hash,
              profile_id = as.character(state$profile_id),
              operation_order = state$operation_order,
              operation_hash = as.character(state$operation_hash),
              training_unit_hash = as.character(state$training_unit_hash),
              feature_keys_sha256 = triomics_b07_prov_ordered_ids_sha256(state$feature_keys)
            )
            prep_entity_id <- triomics_b07_prov_entity_id("preprocessing_state", list(
              producer_stage = "C", analysis_id = analysis_id, block_id = block_id,
              preparation_context_id = preparation_id,
              representation_id = representation_id,
              state_hash = item$reference$state_hash
            ))
            entities[[length(entities) + 1L]] <- triomics_c06_provenance_entity(
              "preprocessing_state", prep_entity_id, preparation_value, study_id,
              triomics_b07_prov_scope(
                study_id, analysis_id = analysis_id, block_id = block_id,
                preparation_context_id = preparation_id,
                representation_id = representation_id
              ),
              activity_id = c_preparation_activity_id,
              derived_from = c(matrix_entity[[1L]]$entity_id,
                               b_prep_entity[[1L]]$entity_id)
            )
            c_preparation_entity_ids <- c(c_preparation_entity_ids, prep_entity_id)
          }
        } else if (!identical(current_state_refs, c_preparation_state_refs)) {
          triomics_c06_provenance_abort(
            "C_EXPORT_PROVENANCE_INVALID",
            "Completed C integrations do not share the same immutable preparation state references."
          )
        }
        result_entity <- entities[[result_entity_index]]
        result_entity$derived_from_entity_ids <- as.list(sort(unique(c(
          upstream_matrix_ids, c_preparation_entity_ids
        )), method = "radix"))
        entities[[result_entity_index]] <- result_entity
      }
      route_id <- integration_id
      native_entity_id <- triomics_b07_prov_entity_id("native_sidecar", route_id)
      entities[[length(entities) + 1L]] <- triomics_c06_provenance_artifact_entity(
        "native_sidecar", native_entity_id, native_payload_path, native_envelope_path, study_id,
        scope = triomics_b07_prov_scope(study_id, analysis_id, method_id = method_id,
                                        preparation_context_id = preparation_id,
                                        representation_id = representation_id, route_id = route_id),
        activity_id = fit_activity,
        derived_from = c(result_id, c_preparation_entity_ids)
      )
      native_generated <- native_entity_id
      route_declarations[[length(route_declarations) + 1L]] <- list(
        route_id = route_id, route_kind = "native", analysis_id = analysis_id,
        preparation_context_id = preparation_id, representation_id = representation_id,
        method_id = method_id
      )
    }
    entities[[length(entities) + 1L]] <- triomics_c06_provenance_entity(
      "integration_state", state_id, state_value, study_id,
      triomics_b07_prov_scope(study_id, analysis_id, method_id = method_id,
                              preparation_context_id = preparation_id,
                              representation_id = representation_id),
      activity_id = fit_activity,
      derived_from = c(result_id, native_generated, c_preparation_entity_ids),
      member_of = result_id
    )
    method_state_id <- triomics_b07_prov_entity_id("method_state", list(
      analysis_id = analysis_id, method_id = method_id,
      preparation_context_id = preparation_id, representation_id = representation_id
    ))
    entities[[length(entities) + 1L]] <- triomics_c06_provenance_entity(
      "method_state", method_state_id,
      list(integration_id = integration_id, method_id = method_id,
           status = as.character(evidence$status), backend = as.character(evidence$backend),
           native_state_ref = native_ref %||% NULL), study_id,
      triomics_b07_prov_scope(study_id, analysis_id, method_id = method_id,
                              preparation_context_id = preparation_id,
                              representation_id = representation_id),
      activity_id = fit_activity,
      derived_from = c(result_id, native_generated, upstream_matrix_ids,
                       c_preparation_entity_ids)
    )
    fit_generated <- c(result_id, state_id, method_state_id, native_generated)
    fit_used <- sort(unique(c(used_upstream, c_preparation_entity_ids)), method = "radix")
    c_fit_activities[[length(c_fit_activities) + 1L]] <- list(
      activity_id = fit_activity, stage = "C", activity_type = "integration",
      method_id = method_id, used_entity_ids = as.list(fit_used),
      generated_entity_ids = as.list(sort(fit_generated, method = "radix"))
    )
    cfit_entity_ids <- c(cfit_entity_ids, fit_generated)
  }
  if (!length(method_ids) || anyDuplicated(method_ids)) {
    triomics_c06_provenance_abort("C_EXPORT_PROVENANCE_INVALID",
                                  "C provenance requires one integration result per declared method.")
  }
  method_ids <- sort(unique(method_ids), method = "radix")
  nomination_path <- file.path(d_root, "candidate-nominations.json")
  nomination_envelope_path <- file.path(d_root, "candidate-nominations.envelope.json")
  evaluation_path <- file.path(d_root, "evaluation-evidence.json")
  evaluation_envelope_path <- file.path(d_root, "evaluation-evidence.envelope.json")
  manifest_path <- file.path(d_root, "handoff-manifest.json")
  manifest_envelope_path <- file.path(d_root, "handoff-manifest.envelope.json")
  export_generated <- character()
  for (spec in list(
    list(role = "candidate_nominations", id = "c06-candidate-nominations",
         payload = nomination_path, envelope = nomination_envelope_path),
    list(role = "evaluation_evidence", id = "c06-evaluation-evidence",
         payload = evaluation_path, envelope = evaluation_envelope_path)
  )) {
    entity_id <- triomics_b07_prov_entity_id(spec$role, spec$id)
    entities[[length(entities) + 1L]] <- triomics_c06_provenance_artifact_entity(
      spec$role, entity_id, spec$payload, spec$envelope, study_id,
      scope = scope, activity_id = c_export_activity,
      derived_from = c(cfit_entity_ids, predecessor_payload_ids)
    )
    export_generated <- c(export_generated, entity_id)
  }
  handoff_id <- "c06-handoff-manifest"
  handoff_entity_id <- triomics_b07_prov_entity_id("handoff_payload", handoff_id)
  entities[[length(entities) + 1L]] <- triomics_c06_provenance_artifact_entity(
    "handoff_payload", handoff_entity_id, manifest_path, manifest_envelope_path,
    study_id, scope = scope, activity_id = c_export_activity,
    derived_from = c(export_generated, cfit_entity_ids, predecessor_payload_ids)
  )
  export_generated <- c(export_generated, handoff_entity_id)
  all_ids <- sort(unique(vapply(entities, `[[`, character(1L), "entity_id")), method = "radix")
  used_export <- sort(setdiff(all_ids, c(export_generated, source_ids)), method = "radix")
  export_activity <- list(
    activity_id = c_export_activity, stage = "C", activity_type = "handoff_export",
    method_id = method_ids[[1L]],
    used_entity_ids = as.list(sort(unique(c(used_export, source_ids)), method = "radix")),
    generated_entity_ids = as.list(sort(export_generated, method = "radix"))
  )
  c_preparation_activity <- if (length(c_preparation_entity_ids)) list(list(
    activity_id = c_preparation_activity_id,
    stage = "C",
    activity_type = "analysis",
    method_id = "prepare_fit",
    used_entity_ids = as.list(sort(unique(used_upstream), method = "radix")),
    generated_entity_ids = as.list(sort(unique(c_preparation_entity_ids), method = "radix"))
  )) else list()
  activities <- c(c_preparation_activity, c_fit_activities, list(export_activity))
  entities <- entities[order(
    vapply(entities, `[[`, character(1L), "scientific_role"),
    vapply(entities, `[[`, character(1L), "entity_type"),
    vapply(entities, `[[`, character(1L), "entity_id"), method = "radix"
  )]
  profile <- list(
    profile = "triomics.stage-handoff-provenance",
    profile_version = "1.0",
    producer_stage = "C",
    study_id = study_id,
    run_id = "c-06",
    handoff_artifact_id = handoff_id,
    producer_activity_id = c_export_activity,
    identity = list(
      schema_uri = "urn:triomics:stage-handoff-provenance:identity",
      schema_version = "1.0",
      handoff_role = "stage-c-handoff",
      activities = activities,
      entities = entities,
      declared_routes = route_declarations[order(
        vapply(route_declarations, `[[`, character(1L), "route_kind"),
        vapply(route_declarations, `[[`, character(1L), "route_id"), method = "radix"
      )]
    ),
    producer = list(
      package_version = as.character(utils::packageVersion("triomics")),
      repository_commit = as.character(producer_commit),
      module_id = "C-06",
      method_ids = as.list(method_ids),
      effective_configuration_sha256 = as.character(config_sha256),
      runtime_backend_versions = triomics_c06_provenance_runtime_versions(
        handoff$integration_evidence, b_predecessors$root
      )
    ),
    limitations = list(
      "Native C same-root handoff provenance; synthetic software evidence is not scientific validation.",
      "The profile binds C-06 output to a validated immutable B handoff and inventories the C integration state."
    ),
    migration = NULL
  )
  profile$identity_sha256 <- triomics_provenance_identity_sha256(profile$identity)
  profile
}

triomics_c06_write_handoff <- function(handoff, output_root, config_sha256,
                                       privacy_class = NULL, producer_commit = "not-recorded",
                                       b_predecessor_paths = NULL, c_input_identity = NULL) {
  if (!is.list(handoff) || !identical(handoff$artifact_type, "stage_c_d_handoff") ||
      !triomics_c06_nonempty_string(output_root) ||
      !triomics_sha256_text(config_sha256) || !triomics_c06_nonempty_string(producer_commit) ||
      !is.character(privacy_class) || length(privacy_class) != 1L || is.na(privacy_class) ||
      !privacy_class %in% c("synthetic", "sensitive")) {
    triomics_integration_abort(
      "C_EXPORT_INPUT_INVALID",
      "A validated C handoff, local output root, configuration SHA256 and authoritative privacy class are required."
    )
  }
  expected_context <- triomics_c06_handoff_context(
    handoff$integration_evidence,
    handoff$nomination_branches$primary_statistics,
    handoff$evaluation_evidence
  )
  if (!triomics_c06_nonempty_string(handoff$study_id) ||
      !triomics_c06_nonempty_string(handoff$analysis_id) ||
      !identical(handoff$preparation_context_id, expected_context$preparation_context_id) ||
      !identical(handoff$representation_id, expected_context$representation_id)) {
    triomics_integration_abort(
      "C_EXPORT_CONTEXT_MISMATCH",
      "The D handoff context no longer matches its integration, nomination and evaluation evidence.",
      key = "handoff.context"
    )
  }
  lineage_inputs <- triomics_c06_lineage_inputs(
    handoff$lineage_inputs, "handoff.lineage_inputs"
  )
  expected_lineage <- triomics_c06_handoff_lineage(
    handoff$nomination_branches$primary_statistics,
    handoff$integration_evidence,
    handoff$evaluation_evidence
  )
  if (!identical(lineage_inputs, expected_lineage)) {
    triomics_integration_abort(
      "C_EXPORT_LINEAGE_MISMATCH",
      "The D handoff lineage no longer matches its primary, integration and evaluation evidence.",
      key = "handoff.lineage_inputs"
    )
  }
  b_predecessors <- triomics_c06_validate_b_predecessors(
    output_root, handoff, config_sha256, b_predecessor_paths, c_input_identity
  )
  if (!grepl("^[0-9a-f]{40,64}$", as.character(producer_commit))) {
    triomics_c06_provenance_abort(
      "C_EXPORT_PROVENANCE_INVALID",
      "Native C provenance requires the repository revision used by the Stage-C producer."
    )
  }
  root <- b_predecessors$root
  d_root <- file.path(root, "D")
  if (triomics_is_symlink(d_root) || (file.exists(d_root) && !dir.exists(d_root))) {
    triomics_integration_abort(
      "C_EXPORT_ARTIFACT_COLLISION", "The C-06 D publication path is not a safe directory.",
      key = "D"
    )
  }
  expected_outputs <- file.path(d_root, c(
    "candidate-nominations.json", "candidate-nominations.envelope.json",
    "integration-evidence.json", "integration-evidence.envelope.json",
    "integration-comparisons.json", "integration-comparisons.envelope.json",
    "evaluation-evidence.json", "evaluation-evidence.envelope.json",
    "stage-status.json", "stage-status.envelope.json",
    "handoff-manifest.json", "handoff-manifest.envelope.json",
    "stage-handoff-provenance.json", "stage-handoff-provenance.envelope.json"
  ))
  if (any(file.exists(expected_outputs)) || any(dir.exists(expected_outputs)) ||
      any(vapply(expected_outputs, triomics_is_symlink, logical(1L)))) {
    triomics_integration_abort(
      "C_EXPORT_ARTIFACT_COLLISION",
      "A C-06 D publication already exists; immutable outputs cannot be replaced.",
      key = "D"
    )
  }
  d_root_created <- !dir.exists(d_root)
  written_paths <- character()
  finalized <- FALSE
  mark_written <- function(path) {
    written_paths <<- unique(c(written_paths, path))
    invisible(path)
  }
  on.exit({
    if (!finalized) {
      if (length(written_paths)) unlink(written_paths, force = TRUE)
      if (d_root_created && dir.exists(d_root) && !length(list.files(d_root, all.files = TRUE, no.. = TRUE))) {
        unlink(d_root, recursive = TRUE, force = TRUE)
      }
    }
  }, add = TRUE)
  dir.create(d_root, recursive = TRUE, showWarnings = FALSE)
  candidate_payload <- list(
    schema_version = "2.0",
    artifact_type = "candidate_nominations",
    study_id = handoff$study_id,
    analysis_id = handoff$analysis_id,
    preparation_context_id = handoff$preparation_context_id,
    representation_id = handoff$representation_id,
    status = handoff$nomination_status,
    source_artifact_id = "stage-c-d-handoff",
    n_nominations = as.integer(length(handoff$nominations)),
    branches = handoff$nomination_branches,
    nominations = handoff$nominations
  )
  integration_payload <- list(
    schema_version = "2.0", artifact_type = "integration_evidence_bundle",
    study_id = handoff$study_id, analysis_id = handoff$analysis_id,
    preparation_context_id = handoff$preparation_context_id,
    representation_id = handoff$representation_id,
    status = handoff$status, integrations = handoff$integration_evidence
  )
  comparison_payload <- list(
    schema_version = "2.0", artifact_type = "integration_comparisons",
    study_id = handoff$study_id, analysis_id = handoff$analysis_id,
    preparation_context_id = handoff$preparation_context_id,
    representation_id = handoff$representation_id,
    status = handoff$comparisons$status, comparisons = handoff$comparisons
  )
  evaluation_payload <- handoff$evaluation_evidence
  evaluation_payload$study_id <- handoff$study_id
  evaluation_payload$analysis_id <- handoff$analysis_id
  evaluation_payload$preparation_context_id <- handoff$preparation_context_id
  evaluation_payload$representation_id <- handoff$representation_id
  evaluation_payload$lineage_inputs <- lineage_inputs
  status_payload <- list(
    schema_version = "2.0", artifact_type = "stage_c_status",
    study_id = handoff$study_id, analysis_id = handoff$analysis_id,
    preparation_context_id = handoff$preparation_context_id,
    representation_id = handoff$representation_id,
    status = handoff$status, backend_statuses = handoff$backend_statuses,
    clinical_prediction = handoff$clinical_prediction,
    lineage_inputs = lineage_inputs
  )
  payloads <- list(
    list(name = "candidate-nominations.json", type = "candidate_nominations", value = candidate_payload, status = candidate_payload$status),
    list(name = "integration-evidence.json", type = "integration_evidence_bundle", value = integration_payload, status = integration_payload$status),
    list(name = "integration-comparisons.json", type = "integration_comparisons", value = comparison_payload, status = comparison_payload$status),
    list(name = "evaluation-evidence.json", type = "integration_evaluation_evidence", value = evaluation_payload, status = evaluation_payload$status),
    list(name = "stage-status.json", type = "stage_c_status", value = status_payload, status = status_payload$status)
  )
  matrix_inputs <- list()
  for (evidence in handoff$integration_evidence) {
    hashes <- evidence$eligibility$matrix_sha256
    if (is.list(hashes)) {
      for (block_id in names(hashes)) {
        value <- hashes[[block_id]]
        if (is.character(value) && length(value) == 1L && triomics_sha256_text(value)) {
          matrix_inputs[[length(matrix_inputs) + 1L]] <- list(
            artifact_id = paste(evidence$integration_id, block_id, sep = "::"),
            sha256 = value
          )
        }
      }
    }
  }
  inputs <- triomics_c06_lineage_inputs(c(lineage_inputs, matrix_inputs), "handoff.inputs")
  payload_records <- lapply(payloads, function(item) {
    path <- file.path(d_root, item$name)
    triomics_write_json_atomic(item$value, path)
    mark_written(path)
    list(item = item, path = path, payload_sha256 = triomics_sha256(path))
  })
  evaluation_record <- payload_records[[which(vapply(payload_records, function(record) {
    identical(record$item$type, "integration_evaluation_evidence")
  }, logical(1)))]]
  artifacts <- lapply(payload_records, function(record) {
    item <- record$item
    path <- record$path
    envelope_path <- file.path(d_root, sub("\\.json$", ".envelope.json", item$name))
    artifact_inputs <- inputs
    if (!identical(item$type, "integration_evaluation_evidence")) {
      artifact_inputs <- c(artifact_inputs, list(list(
        artifact_id = "c06-evaluation-evidence",
        sha256 = evaluation_record$payload_sha256
      )))
    }
    artifact_inputs <- triomics_c06_lineage_inputs(
      artifact_inputs, paste0("envelope.inputs.", item$type)
    )
    envelope <- triomics_artifact_envelope(
      artifact_type = item$type,
      artifact_id = paste0("c06-", sub("\\.json$", "", item$name)),
      study_id = handoff$study_id,
      run_id = "c-06",
      status = triomics_c06_envelope_status(item$status),
      config_sha256 = config_sha256,
      payload = path,
      producer_commit = producer_commit,
      inputs = artifact_inputs,
      reason_codes = character(),
      privacy_class = privacy_class
    )
    envelope$analysis_id <- handoff$analysis_id
    envelope$preparation_context_id <- handoff$preparation_context_id
    envelope$representation_id <- handoff$representation_id
    triomics_write_envelope(envelope, envelope_path)
    mark_written(envelope_path)
    list(
      artifact_type = item$type,
      payload_file = item$name,
      payload_sha256 = record$payload_sha256,
      envelope_file = basename(envelope_path),
      envelope_sha256 = triomics_sha256(envelope_path)
    )
  })
  manifest <- list(
    schema_version = "2.0",
    artifact_type = "stage_c_handoff_manifest",
    study_id = handoff$study_id,
    analysis_id = handoff$analysis_id,
    preparation_context_id = handoff$preparation_context_id,
    representation_id = handoff$representation_id,
    status = handoff$status,
    artifacts = artifacts
  )
  manifest_path <- file.path(d_root, "handoff-manifest.json")
  triomics_write_json_atomic(manifest, manifest_path)
  mark_written(manifest_path)
  manifest_inputs <- c(inputs, lapply(artifacts, function(artifact) {
    list(
      artifact_id = paste0("c06-", sub("\\.json$", "", artifact$payload_file)),
      sha256 = artifact$payload_sha256
    )
  }))
  manifest_inputs <- triomics_c06_lineage_inputs(manifest_inputs, "manifest.inputs")
  manifest_envelope <- triomics_artifact_envelope(
    artifact_type = "stage_c_handoff_manifest",
    artifact_id = "c06-handoff-manifest",
    study_id = handoff$study_id,
    run_id = "c-06",
    status = triomics_c06_envelope_status(handoff$status),
    config_sha256 = config_sha256,
    payload = manifest_path,
    producer_commit = producer_commit,
    inputs = manifest_inputs,
    reason_codes = character(),
    privacy_class = privacy_class
  )
  manifest_envelope$analysis_id <- handoff$analysis_id
  manifest_envelope$preparation_context_id <- handoff$preparation_context_id
  manifest_envelope$representation_id <- handoff$representation_id
  manifest_envelope_path <- file.path(d_root, "handoff-manifest.envelope.json")
  triomics_write_envelope(manifest_envelope, manifest_envelope_path)
  mark_written(manifest_envelope_path)
  profile <- triomics_c06_build_provenance_profile(
    handoff, d_root, config_sha256, producer_commit, b_predecessors
  )
  profile_path <- file.path(d_root, "stage-handoff-provenance.json")
  profile_envelope_path <- file.path(d_root, "stage-handoff-provenance.envelope.json")
  validation <- tryCatch(isTRUE(triomics_validate_stage_handoff_provenance(
    profile, "c06-stage-handoff-provenance", "c06-stage-handoff-provenance-envelope"
  )), error = function(error) error)
  if (inherits(validation, "error") || !isTRUE(validation)) {
    reason <- if (inherits(validation, "error")) conditionMessage(validation) else
      "validator returned false"
    triomics_c06_provenance_abort(
      "C_EXPORT_PROVENANCE_INVALID",
      paste0("The native C provenance profile failed the existing public validator: ", reason)
    )
  }
  triomics_write_json_atomic(profile, profile_path)
  mark_written(profile_path)
  profile_envelope <- triomics_artifact_envelope(
    artifact_type = "stage_handoff_provenance",
    artifact_id = "c06-stage-handoff-provenance",
    study_id = as.character(profile$study_id),
    run_id = as.character(profile$run_id),
    status = "complete",
    config_sha256 = as.character(config_sha256),
    payload = profile_path,
    producer_commit = as.character(producer_commit),
    inputs = list(list(artifact_id = as.character(profile$handoff_artifact_id),
                       sha256 = triomics_sha256(manifest_path))),
    reason_codes = character(),
    privacy_class = as.character(privacy_class)
  )
  profile_envelope$analysis_id <- as.character(handoff$analysis_id)
  profile_envelope$preparation_context_id <- as.character(handoff$preparation_context_id)
  profile_envelope$representation_id <- as.character(handoff$representation_id)
  triomics_write_envelope(profile_envelope, profile_envelope_path)
  mark_written(profile_envelope_path)
  readback <- tryCatch(jsonlite::read_json(profile_path, simplifyVector = FALSE),
                       error = function(error) NULL)
  if (!isTRUE(triomics_validate_envelope(profile_envelope_path)) ||
      !is.list(readback) || !isTRUE(tryCatch(triomics_validate_stage_handoff_provenance(
        readback, "c06-stage-handoff-provenance", "c06-stage-handoff-provenance-envelope"
      ), error = function(error) FALSE)) ||
      !identical(triomics_sha256(profile_path), as.character(profile_envelope$payload_sha256)) ||
      !identical(vapply(b_predecessors$paths, triomics_sha256, character(1L)),
                 b_predecessors$predecessor_hashes)) {
    triomics_c06_provenance_abort(
      "C_EXPORT_PROVENANCE_INVALID",
      "C provenance read-back or exact B predecessor-byte validation failed."
    )
  }
  finalized <- TRUE
  list(
    directory = d_root,
    manifest = manifest_path,
    manifest_sha256 = triomics_sha256(manifest_path),
    artifacts = artifacts,
    provenance = profile_path,
    provenance_sha256 = triomics_sha256(profile_path),
    provenance_envelope = profile_envelope_path,
    provenance_envelope_sha256 = triomics_sha256(profile_envelope_path),
    predecessor_hashes = b_predecessors$predecessor_hashes
  )
}
