triomics_cnh_abort <- function(code, message, key = "clinical_score_request", stage = "B") {
  triomics_abort(
    code, message, key = key, module = "clinical_native_handoff", stage = stage,
    remediation = "Preserve the declared request and rerun the accepted C producer into a clean same-root output."
  )
}

triomics_cnh_r_value_abort <- function(message, key = "C/publication-context.json") {
  triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", message, key, stage = "C")
}

triomics_cnh_encode_atomic <- function(value) {
  storage <- typeof(value)
  if (!storage %in% c("character", "integer", "double", "logical")) {
    triomics_cnh_r_value_abort("The C publication context contains an unsupported atomic storage type.")
  }
  missing <- which(is.na(value))
  if (storage == "double" && any(!is.finite(value) & !is.na(value))) {
    triomics_cnh_r_value_abort("The C publication context contains non-finite numeric metadata.")
  }
  values <- value
  if (length(missing)) {
    if (storage == "character") values[missing] <- ""
    if (storage == "integer") values[missing] <- 0L
    if (storage == "double") values[missing] <- 0
    if (storage == "logical") values[missing] <- FALSE
  }
  list(
    storage = storage,
    values = lapply(seq_along(values), function(index) unname(values[[index]])),
    missing = as.list(as.integer(missing)),
    has_names = !is.null(names(value)),
    names = if (is.null(names(value))) NULL else as.list(names(value))
  )
}

triomics_cnh_encode_r_value <- function(value) {
  if (is.null(value)) return(list(type = "null"))
  if (is.matrix(value)) {
    if (!typeof(value) %in% c("integer", "double") || is.complex(value) ||
        length(dim(value)) != 2L || any(dim(value) < 0L)) {
      triomics_cnh_r_value_abort("Only two-dimensional integer and numeric matrices can be retained in a C context.")
    }
    return(list(
      type = "matrix", storage = typeof(value), dimensions = as.list(as.integer(dim(value))),
      dimnames = if (is.null(dimnames(value))) NULL else lapply(dimnames(value), function(item) {
        if (is.null(item)) NULL else as.list(item)
      }),
      values = triomics_cnh_encode_atomic(as.vector(value))
    ))
  }
  if (is.data.frame(value)) {
    if (anyDuplicated(names(value)) || anyNA(names(value))) {
      triomics_cnh_r_value_abort("C context data frames must have unique named columns.")
    }
    return(list(
      type = "data.frame", column_names = as.list(names(value)),
      columns = lapply(value, triomics_cnh_encode_r_value),
      row_names = triomics_cnh_encode_r_value(attr(value, "row.names", exact = TRUE))
    ))
  }
  if (is.factor(value)) {
    codes <- unclass(value)
    missing <- which(is.na(codes))
    codes[missing] <- 0L
    return(list(
      type = "factor", ordered = is.ordered(value), levels = as.list(levels(value)),
      codes = as.list(as.integer(codes)), missing = as.list(as.integer(missing)),
      has_names = !is.null(names(value)),
      names = if (is.null(names(value))) NULL else as.list(names(value))
    ))
  }
  if (is.list(value)) {
    classes <- attr(value, "class", exact = TRUE)
    if (!is.null(classes) && !identical(classes, "AsIs")) {
      triomics_cnh_r_value_abort("The C publication context contains an unsupported list class.")
    }
    return(list(
      type = "list", class = if (is.null(classes)) NULL else as.list(classes),
      has_names = !is.null(names(value)),
      names = if (is.null(names(value))) NULL else as.list(names(value)),
      values = lapply(value, triomics_cnh_encode_r_value)
    ))
  }
  if (is.atomic(value)) {
    classes <- attr(value, "class", exact = TRUE)
    if (is.null(classes)) classes <- character()
    if (length(classes) && !identical(classes, "Date")) {
      triomics_cnh_r_value_abort("The C publication context contains an unsupported atomic class.")
    }
    atomic <- triomics_cnh_encode_atomic(value)
    return(c(list(type = "atomic", class = if (!length(classes)) NULL else as.list(classes)), atomic))
  }
  triomics_cnh_r_value_abort("The C publication context contains a function or unsupported R object.")
}

triomics_cnh_decode_atomic <- function(node) {
  storage <- as.character(node$storage %||% "")
  values <- unlist(node$values, recursive = FALSE, use.names = FALSE)
  if (storage == "character") values <- as.character(values)
  if (storage == "integer") values <- as.integer(values)
  if (storage == "double") values <- as.double(values)
  if (storage == "logical") values <- as.logical(values)
  missing <- as.integer(unlist(node$missing, use.names = FALSE))
  if (length(missing)) values[missing] <- NA
  if (isTRUE(node$has_names)) names(values) <- as.character(unlist(node$names, use.names = FALSE))
  values
}

triomics_cnh_decode_r_value <- function(node) {
  if (!is.list(node) || !triomics_nonempty_string(as.character(node$type %||% ""))) {
    triomics_cnh_r_value_abort("The saved C publication context has an unknown typed-value node.")
  }
  type <- as.character(node$type)
  if (identical(type, "null")) return(NULL)
  if (identical(type, "atomic")) {
    value <- triomics_cnh_decode_atomic(node)
    classes <- as.character(unlist(node$class, use.names = FALSE))
    if (length(classes)) class(value) <- classes
    return(value)
  }
  if (identical(type, "factor")) {
    codes <- as.integer(unlist(node$codes, use.names = FALSE))
    missing <- as.integer(unlist(node$missing, use.names = FALSE))
    if (length(missing)) codes[missing] <- NA_integer_
    value <- structure(codes, levels = as.character(unlist(node$levels, use.names = FALSE)),
                       class = if (isTRUE(node$ordered)) c("ordered", "factor") else "factor")
    if (isTRUE(node$has_names)) names(value) <- as.character(unlist(node$names, use.names = FALSE))
    return(value)
  }
  if (identical(type, "list")) {
    values <- lapply(node$values, triomics_cnh_decode_r_value)
    if (isTRUE(node$has_names)) names(values) <- as.character(unlist(node$names, use.names = FALSE))
    classes <- as.character(unlist(node$class, use.names = FALSE))
    if (length(classes)) class(values) <- classes
    return(values)
  }
  if (identical(type, "data.frame")) {
    names <- as.character(unlist(node$column_names, use.names = FALSE))
    columns <- lapply(node$columns, triomics_cnh_decode_r_value)
    names(columns) <- names
    row_names <- triomics_cnh_decode_r_value(node$row_names)
    result <- structure(columns, class = "data.frame", row.names = row_names)
    return(result)
  }
  if (identical(type, "matrix")) {
    value <- triomics_cnh_decode_atomic(node$values)
    dimensions <- as.integer(unlist(node$dimensions, use.names = FALSE))
    if (length(dimensions) != 2L || prod(dimensions) != length(value)) {
      triomics_cnh_r_value_abort("The saved C matrix dimensions do not match its typed values.")
    }
    dim(value) <- dimensions
    if (is.list(node$dimnames)) {
      dimnames(value) <- lapply(node$dimnames, function(item) {
        if (is.null(item)) NULL else as.character(unlist(item, use.names = FALSE))
      })
    }
    return(value)
  }
  triomics_cnh_r_value_abort("The saved C publication context has an unsupported typed-value node.")
}

triomics_cnh_same_json <- function(left, right) {
  left_text <- tryCatch(triomics_canonical_json(left), error = function(error) NULL)
  right_text <- tryCatch(triomics_canonical_json(right), error = function(error) NULL)
  is.character(left_text) && length(left_text) == 1L &&
    is.character(right_text) && length(right_text) == 1L &&
    identical(left_text, right_text)
}

triomics_cnh_read_json <- function(path, key) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path) || dir.exists(path) || triomics_is_symlink(path)) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", paste0("The saved ", key, " is missing or unsafe."), key)
  }
  value <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(value) || is.null(names(value))) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", paste0("The saved ", key, " is not a JSON object."), key)
  }
  value
}

triomics_cnh_path <- function(root, relative, expected_sha256, key) {
  if (!triomics_nonempty_string(relative) || !triomics_sha256_text(expected_sha256)) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", paste0("The saved ", key, " reference is incomplete."), key)
  }
  path <- tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
  if (is.null(path) || !file.exists(path) || dir.exists(path) || triomics_is_symlink(path) ||
      !identical(triomics_sha256(path), expected_sha256)) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", paste0("The saved ", key, " failed same-root byte validation."), key)
  }
  path
}

triomics_cnh_validate_envelope <- function(path, expected, key) {
  if (!isTRUE(triomics_validate_envelope(path))) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", paste0("The saved ", key, " envelope failed validation."), key)
  }
  envelope <- triomics_cnh_read_json(path, paste0(key, " envelope"))
  for (field in names(expected)) {
    if (!identical(envelope[[field]], expected[[field]])) {
      triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", paste0("The saved ", key, " has a mismatched ", field, "."), key)
    }
  }
  envelope
}

triomics_cnh_source_inputs <- function(ingested) {
  sources <- ingested$source_manifests
  if (!is.list(sources) || !length(sources)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The C request has no authenticated molecular source manifests.", "source_manifests")
  }
  lapply(sources, function(source) {
    if (!is.list(source) || !triomics_nonempty_string(source$source_id) ||
        !triomics_sha256_text(source$sha256)) {
      triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "A molecular source manifest is incomplete.", "source_manifests")
    }
    list(artifact_id = as.character(source$source_id), sha256 = as.character(source$sha256))
  })
}

triomics_cnh_reject_prior_b <- function(root) {
  paths <- c(
    file.path(root, "registries", "run-B.json"),
    file.path(root, "B", "block-results.json"),
    file.path(root, "B", "block-results.envelope.json"),
    file.path(root, "B", "clinical-associations.json"),
    file.path(root, "B", "clinical-associations.envelope.json"),
    file.path(root, "B", "clinical-longitudinal-associations.json"),
    file.path(root, "B", "clinical-longitudinal-associations.envelope.json")
  )
  if (any(file.exists(paths)) || any(vapply(paths, triomics_is_symlink, logical(1L)))) {
    triomics_cnh_abort(
      "B_SCORE_REQUEST_CONFLICT",
      "A Stage B result already exists; the saved C request cannot be executed as a retry.",
      "B"
    )
  }
  handoff_dir <- file.path(root, "B", "handoffs")
  if (triomics_is_symlink(handoff_dir) || (dir.exists(handoff_dir) &&
      length(list.files(handoff_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)))) {
    triomics_cnh_abort(
      "B_SCORE_REQUEST_CONFLICT",
      "A Stage B handoff already exists; the saved C request cannot be executed as a retry.",
      "B/handoffs"
    )
  }
  request_dir <- file.path(root, "B", "clinical-score-association-requests")
  if (dir.exists(request_dir)) {
    files <- list.files(request_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    if (any(!files %in% c("request-01.json", "request-01.envelope.json"))) {
      triomics_cnh_abort(
        "B_SCORE_REQUEST_AMBIGUOUS",
        "The output root contains more than one scheduled clinical request.",
        "B/clinical-score-association-requests"
      )
    }
  }
  invisible(TRUE)
}

triomics_cnh_load_stage_b_context <- function(config_path, output, allow_b_started = FALSE) {
  config <- triomics_read_config(config_path)
  if (!isTRUE(config$clinical$enabled)) return(NULL)
  objectives <- config$clinical$objectives
  if (!is.list(objectives) || !length(objectives)) return(NULL)
  if (config$study_id %in% c("triomics_b_t08_fixture", "triomics_b_t09_fixture")) return(NULL)
  modes <- vapply(objectives, function(objective) {
    if (is.null(objective$mode)) "" else as.character(objective$mode)
  }, character(1L))
  if (any(modes != "between_person")) {
    triomics_cnh_abort(
      "B_DESIGN_UNSUPPORTED",
      "The native C score-request route accepts between-person objectives only.",
      "clinical.objectives.mode"
    )
  }
  if (length(config$analyses) != 1L) {
    triomics_cnh_abort("B_DESIGN_UNSUPPORTED", "The native score-request route requires one analysis.", "analyses")
  }
  analysis <- config$analyses[[1L]]
  integrations <- analysis$integration
  if (!is.list(integrations) || length(integrations) != 1L ||
      !as.character(integrations[[1L]]$method) %in%
        c("concat_pca", "mixomics_diablo", "mixomics_block_spls", "mofa2")) {
    triomics_cnh_abort(
      "B_DESIGN_UNSUPPORTED",
      "The native score-request route requires one registered C-08 integration request.",
      "analyses.integration"
    )
  }
  if (any(vapply(objectives, function(objective) {
    !triomics_nonempty_string(as.character(objective$time_window_ref %||% ""))
  }, logical(1L)))) {
    triomics_cnh_abort(
      "B_DESIGN_UNSUPPORTED", "Every between-person score request needs an explicit visit window.",
      "clinical.objectives.time_window_ref"
    )
  }

  root <- tryCatch(triomics_b07_output_root(output, create = FALSE), error = function(error) NULL)
  if (is.null(root)) {
    triomics_cnh_abort("B_SCORE_REQUEST_MISSING", "The native B request needs an existing same-root C output.", "output")
  }
  if (!isTRUE(allow_b_started)) triomics_cnh_reject_prior_b(root)
  config_sha256 <- as.character(attr(config, "config_sha256"))
  source_identity <- list(
    study_id = as.character(config$study_id),
    analysis_id = as.character(analysis$analysis_id),
    preparation_context_id = as.character(analysis$preparation_context_id),
    representation_id = as.character(analysis$design$representation)
  )
  if (any(!vapply(source_identity, triomics_nonempty_string, logical(1L))) ||
      !triomics_sha256_text(config_sha256)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The native request lacks a complete C identity.", "identity")
  }

  c_payload_path <- triomics_cnh_path(
    root, file.path("C", "integration-results.json"),
    expected_sha256 = triomics_sha256(file.path(root, "C", "integration-results.json")),
    key = "C integration results"
  )
  c_envelope_path <- file.path(root, "C", "integration-results.envelope.json")
  c_envelope <- triomics_cnh_validate_envelope(c_envelope_path, c(
    list(artifact_type = "integration_results", artifact_id = "stage-c-integration-results",
         study_id = source_identity$study_id, config_sha256 = config_sha256,
         analysis_id = source_identity$analysis_id,
         preparation_context_id = source_identity$preparation_context_id,
         representation_id = source_identity$representation_id),
    list(payload_sha256 = triomics_sha256(c_payload_path))
  ), "C integration results")
  c_payload <- triomics_cnh_read_json(c_payload_path, "C integration results")
  if (!identical(c_payload$status, "complete") ||
      !triomics_cnh_same_json(c_payload[names(source_identity)], source_identity) ||
      !is.list(c_payload$integrations) || length(c_payload$integrations) != 1L ||
      !identical(c_payload$integrations[[1L]]$status, "complete")) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The saved C integration result is incomplete or mismatched.", "C/integration-results.json")
  }
  run_path <- file.path(root, "registries", "run-C.json")
  run_record <- triomics_cnh_read_json(run_path, "C run record")
  if (!identical(run_record$stage, "C") || !identical(run_record$study_id, source_identity$study_id) ||
      !identical(run_record$status, "complete")) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The saved C run record is incomplete or mismatched.", "registries/run-C.json")
  }

  ingested <- triomics_ingest_configured_study(config, config_path)
  if (!identical(as.character(ingested$study_id), source_identity$study_id)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The current molecular sources name a different study.", "study_id")
  }
  clinical <- triomics_ingest_clinical(config, config_path, ingested$sample_registry)
  if (!is.list(clinical$public) || !identical(as.character(clinical$study_id), source_identity$study_id)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The current clinical inputs do not match the saved C study.", "clinical")
  }
  source_inputs <- triomics_cnh_source_inputs(ingested)

  item <- c_payload$integrations[[1L]]
  integration_request <- integrations[[1L]]
  if (!identical(as.character(item$integration_id), as.character(integration_request$integration_id)) ||
      !identical(as.character(item$method), as.character(integration_request$method)) ||
      !is.list(item$score_endpoint_bundle) || !is.list(item$clinical_score_request)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The saved C result does not contain the single configured score request.", "C/integration-results.json")
  }
  endpoint_ref <- item$score_endpoint_bundle
  request_ref <- item$clinical_score_request
  endpoint_rel <- file.path("C", "score-endpoints", "integration-01.json")
  endpoint_envelope_rel <- file.path("C", "score-endpoints", "integration-01.envelope.json")
  request_rel <- file.path("B", "clinical-score-association-requests", "request-01.json")
  request_envelope_rel <- file.path("B", "clinical-score-association-requests", "request-01.envelope.json")
  if (!identical(endpoint_ref$payload_file, endpoint_rel) ||
      !identical(endpoint_ref$envelope_file, endpoint_envelope_rel) ||
      !identical(request_ref$payload_file, request_rel) ||
      !identical(request_ref$envelope_file, request_envelope_rel) ||
      !identical(request_ref$status, "scheduled") ||
      !identical(request_ref$owner_stage, "B") ||
      !identical(request_ref$model_execution, "not_started")) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", "The saved C request uses an unsupported artifact reference or state.", "clinical_score_request")
  }
  endpoint_path <- triomics_cnh_path(root, endpoint_ref$payload_file,
                                     endpoint_ref$payload_sha256, "C score endpoint")
  endpoint_envelope_path <- triomics_cnh_path(root, endpoint_ref$envelope_file,
                                              endpoint_ref$envelope_sha256, "C score endpoint envelope")
  endpoint_envelope <- triomics_cnh_validate_envelope(endpoint_envelope_path, c(
    list(artifact_type = "integration_score_endpoint_bundle",
         artifact_id = as.character(endpoint_ref$artifact_id),
         study_id = source_identity$study_id, config_sha256 = config_sha256,
         analysis_id = source_identity$analysis_id,
         preparation_context_id = source_identity$preparation_context_id,
         representation_id = source_identity$representation_id,
         payload_sha256 = triomics_sha256(endpoint_path)),
    list(inputs = source_inputs)
  ), "C score endpoint")
  endpoint_bundle <- triomics_cnh_read_json(endpoint_path, "C score endpoint")
  class(endpoint_bundle) <- c("IntegrationScoreEndpointBundle", "list")
  valid_endpoint <- tryCatch({
    triomics_c08_validate_endpoint_bundle(endpoint_bundle)
    TRUE
  }, error = function(error) FALSE)
  if (!valid_endpoint ||
      !identical(endpoint_bundle$study_id, source_identity$study_id) ||
      !identical(endpoint_bundle$analysis_id, source_identity$analysis_id) ||
      !identical(endpoint_bundle$integration_id, as.character(integration_request$integration_id)) ||
      !identical(as.character(endpoint_ref$artifact_id), "c08-score-endpoint-bundle-1")) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The saved C score endpoint failed its identity or semantic hash checks.", "C/score-endpoints/integration-01.json")
  }
  endpoint_ids <- vapply(endpoint_bundle$endpoints, function(endpoint) as.character(endpoint$endpoint_id), character(1L))
  if (!triomics_cnh_same_json(endpoint_ref$endpoint_ids, as.list(endpoint_ids))) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The C result's endpoint IDs differ from the authenticated endpoint bundle.", "score_endpoint_bundle.endpoint_ids")
  }
  result_provenance <- item$result$params$provenance
  endpoint_hashes <- endpoint_bundle$endpoints[[1L]]$input_hashes
  if (!is.list(result_provenance$matrix_sha256) ||
      !triomics_cnh_same_json(result_provenance$matrix_sha256, endpoint_hashes) ||
      !identical(as.character(result_provenance$study_id), source_identity$study_id) ||
      !identical(as.character(result_provenance$analysis_id), source_identity$analysis_id) ||
      !identical(as.character(result_provenance$preparation_context_id), source_identity$preparation_context_id) ||
      !identical(as.character(result_provenance$representation_id), source_identity$representation_id)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The score endpoint matrix or fit identity differs from the C integration result.", "C/score-endpoints/integration-01.json")
  }

  endpoint_samples <- endpoint_bundle$endpoints[[1L]]$samples
  source_sample_ids <- unique(unlist(lapply(endpoint_samples, function(sample) {
    as.character(unlist(sample$source_sample_ids, use.names = FALSE))
  }), use.names = FALSE))
  if (!length(source_sample_ids) || anyNA(source_sample_ids) || any(!nzchar(source_sample_ids))) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The C score endpoint has no validated source-sample linkage.", "score_endpoint_bundle.unit_map")
  }
  clinical_view <- tryCatch(
    triomics_c08_cli_clinical_view(clinical, source_sample_ids),
    error = function(error) NULL
  )
  if (!is.list(clinical_view) || !identical(clinical_view$study_id, source_identity$study_id)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The clinical view cannot reproduce the C score links.", "clinical.links_file")
  }
  objective_spec <- triomics_c08_cli_objective_spec(config, analysis)
  expected_request <- tryCatch(
    triomics_schedule_clinical_score_request(endpoint_bundle, objective_spec, clinical_view),
    error = function(error) error
  )
  if (inherits(expected_request, "error")) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", "The current C endpoint and clinical view cannot recreate the declared request.", "clinical_score_request")
  }
  request_path <- triomics_cnh_path(root, request_ref$payload_file,
                                    request_ref$payload_sha256, "B clinical score request")
  request_envelope_path <- triomics_cnh_path(root, request_ref$envelope_file,
                                             request_ref$envelope_sha256, "B clinical score request envelope")
  request_inputs <- c(
    list(list(artifact_id = as.character(endpoint_ref$artifact_id),
             sha256 = as.character(endpoint_ref$payload_sha256))),
    list(
      list(artifact_id = "clinical-dictionary-source", sha256 = clinical$dictionary_sha256),
      list(artifact_id = "clinical-observations-source", sha256 = clinical$observations_sha256),
      list(artifact_id = "clinical-links-source", sha256 = clinical$links_sha256)
    )
  )
  request_envelope <- triomics_cnh_validate_envelope(request_envelope_path, c(
    list(artifact_type = "clinical_score_association_request",
         artifact_id = as.character(request_ref$artifact_id),
         study_id = source_identity$study_id, config_sha256 = config_sha256,
         analysis_id = source_identity$analysis_id,
         preparation_context_id = source_identity$preparation_context_id,
         representation_id = source_identity$representation_id,
         status = "not_started", payload_sha256 = triomics_sha256(request_path),
         inputs = request_inputs)
  ), "B clinical score request")
  request <- triomics_cnh_read_json(request_path, "B clinical score request")
  request_body <- request
  request_body$request_sha256 <- NULL
  recomputed_request_sha256 <- triomics_c08_sha256(request_body)
  if (!identical(request$request_sha256, expected_request$request_sha256) ||
      !identical(request$request_sha256, recomputed_request_sha256) ||
      !triomics_cnh_same_json(request, unclass(expected_request)) ||
      !identical(request_ref$request_sha256, expected_request$request_sha256) ||
      !identical(request$status, "scheduled") || !identical(request$owner_stage, "B") ||
      !identical(request$model_execution, "not_started") || !identical(request$recurses_to_stage_c, FALSE) ||
      length(request$results) != 0L) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", "The saved B request differs from the request rebuilt from current C inputs.", "clinical_score_request")
  }
  if (!is.list(request$objective_spec$family) ||
      !length(request$objective_spec$family$member_ids) ||
      anyDuplicated(as.character(unlist(request$objective_spec$family$member_ids, use.names = FALSE)))) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", "The saved clinical score family is incomplete or repeated.", "clinical_score_request.objective_spec.family")
  }

  expected_request_hashes <- stats::setNames(vapply(c(
    c_payload_path, c_envelope_path, endpoint_path, endpoint_envelope_path,
    request_path, request_envelope_path
  ), triomics_sha256, character(1L)), c(
    "c_payload", "c_envelope", "endpoint_payload", "endpoint_envelope",
    "request_payload", "request_envelope"
  ))
  context <- list(
    config = config,
    config_path = normalizePath(config_path, winslash = "/", mustWork = TRUE),
    config_sha256 = config_sha256,
    analysis = analysis,
    output_root = root,
    study_id = source_identity$study_id,
    analysis_id = source_identity$analysis_id,
    preparation_context_id = source_identity$preparation_context_id,
    representation_id = source_identity$representation_id,
    ingested = ingested,
    clinical = clinical,
    clinical_view = clinical_view,
    objective_spec = objective_spec,
    endpoint_bundle = endpoint_bundle,
    request = request,
    endpoint_reference = endpoint_ref,
    request_reference = request_ref,
    endpoint_path = endpoint_path,
    endpoint_envelope_path = endpoint_envelope_path,
    request_path = request_path,
    request_envelope_path = request_envelope_path,
    source_inputs = source_inputs,
    binding = expected_request_hashes
  )
  class(context) <- c("ClinicalNativeHandoffContext", "list")
  context
}

triomics_cnh_prepare_stage_b <- function(config_path, output) {
  triomics_cnh_load_stage_b_context(config_path, output, allow_b_started = FALSE)
}

triomics_cnh_public_dictionary_coverage <- function(clinical) {
  dictionary <- clinical$public$dictionary
  coverage <- clinical$public$coverage
  if (!is.list(dictionary) || !is.list(coverage)) {
    triomics_cnh_abort("B_SCORE_PRIVACY_INVALID", "Clinical dictionary and coverage inputs are required.", "clinical_dictionary")
  }
  excluded_privacy <- c("identifier", "free_text", "sensitive")
  dictionary_by_id <- stats::setNames(dictionary, vapply(dictionary, function(variable) {
    as.character(variable$variable_id %||% "")
  }, character(1L)))
  visible <- Filter(function(variable) {
    !as.character(variable$privacy %||% "") %in% excluded_privacy
  }, dictionary)
  privacy_categories <- excluded_privacy
  exclusion_counts <- stats::setNames(lapply(privacy_categories, function(privacy) {
    variables <- Filter(function(variable) identical(as.character(variable$privacy), privacy), dictionary)
    rows <- Filter(function(record) {
      variable <- dictionary_by_id[[as.character(record$variable_id %||% "")]]
      is.list(variable) && identical(as.character(variable$privacy), privacy)
    }, coverage)
    count_fields <- c("n_subjects", "n_observed", "n_missing", "n_placeholder", "n_censored", "n_multiselect")
    counts <- stats::setNames(lapply(count_fields, function(field) {
      as.integer(sum(vapply(rows, function(record) as.integer(record[[field]] %||% 0L), integer(1L))))
    }), count_fields)
    list(n_variables = as.integer(length(variables)), coverage_counts = counts)
  }), privacy_categories)
  safe_dispositions <- c("placeholder", "not_applicable", "unavailable", "below_detection",
                         "censored", "missing", "not_collected")
  variable_projection <- lapply(visible, function(variable) {
    dispositions <- as.character(unlist(variable$missing_map, use.names = FALSE))
    dispositions <- sort(unique(dispositions[dispositions %in% safe_dispositions]), method = "radix")
    unsupported_count <- as.integer(length(setdiff(unique(dispositions), safe_dispositions)))
    missingness_rules <- if (length(variable$missing_map)) {
      list(
        mapped_dispositions = as.list(dispositions),
        unsupported_disposition_count = unsupported_count,
        censoring = as.character(variable$censoring %||% "unresolved"),
        imputation = "none"
      )
    } else NULL
    list(
      variable_id = as.character(variable$variable_id),
      declared_type = as.character(variable$declared_type %||% variable$type %||% "unresolved"),
      unit = as.character(variable$units %||% "unresolved"),
      units_verification = as.character(variable$units_verification %||% "unresolved"),
      role = as.character(variable$role %||% "unresolved"),
      time_semantics = as.character(variable$time_semantics %||% "unresolved"),
      availability = as.character(variable$availability %||% "unresolved"),
      verification = as.character(variable$verification %||% "unresolved"),
      modelable = if (is.logical(variable$modelable) && length(variable$modelable) == 1L && !is.na(variable$modelable)) variable$modelable else NULL,
      missingness_rules = missingness_rules
    )
  })
  names(variable_projection) <- NULL
  coverage_projection <- lapply(coverage, function(record) {
    variable <- dictionary_by_id[[as.character(record$variable_id %||% "")]]
    if (is.null(variable) || as.character(variable$privacy %||% "") %in% excluded_privacy) return(NULL)
    list(
      variable_id = as.character(record$variable_id),
      visit_id = record$visit_id %||% NULL,
      scope = as.character(record$scope %||% "unresolved"),
      availability = as.character(record$availability %||% variable$availability %||% "unresolved"),
      n_subjects = as.integer(record$n_subjects %||% 0L),
      n_observed = as.integer(record$n_observed %||% 0L),
      n_missing = as.integer(record$n_missing %||% 0L),
      n_placeholder = as.integer(record$n_placeholder %||% 0L),
      n_censored = as.integer(record$n_censored %||% 0L),
      n_multiselect = as.integer(record$n_multiselect %||% 0L)
    )
  })
  coverage_projection <- Filter(Negate(is.null), coverage_projection)
  list(
    schema_version = "1.0",
    variables = variable_projection,
    coverage = coverage_projection,
    excluded = exclusion_counts
  )
}

triomics_cnh_publish_stage_b <- function(context, config_path, output) {
  if (!inherits(context, "ClinicalNativeHandoffContext") ||
      !triomics_nonempty_string(config_path) || !triomics_nonempty_string(output)) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", "A validated native request context and its original paths are required.", "context")
  }
  current_config_path <- tryCatch(normalizePath(config_path, winslash = "/", mustWork = TRUE),
                                  error = function(error) NULL)
  current_root <- tryCatch(triomics_b07_output_root(output, create = FALSE), error = function(error) NULL)
  if (is.null(current_config_path) || !identical(current_config_path, context$config_path) ||
      is.null(current_root) || !identical(current_root, context$output_root)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The native request must publish under its original config and output root.", "output")
  }
  fresh <- triomics_cnh_load_stage_b_context(config_path, output, allow_b_started = TRUE)
  if (!inherits(fresh, "ClinicalNativeHandoffContext") ||
      !identical(fresh$config_sha256, context$config_sha256) ||
      !identical(fresh$binding, context$binding) ||
      !triomics_cnh_same_json(fresh$request, context$request) ||
      !triomics_cnh_same_json(fresh$endpoint_bundle, context$endpoint_bundle)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The C request or its inputs changed after B preflight.", "clinical_score_request")
  }

  b_payload_path <- file.path(current_root, "B", "block-results.json")
  b_envelope_path <- file.path(current_root, "B", "block-results.envelope.json")
  if (!file.exists(b_payload_path) || !file.exists(b_envelope_path)) {
    triomics_cnh_abort("B_SCORE_REQUEST_INVALID", "The native clinical result must follow the ordinary Stage B feature producer.", "B/block-results.json")
  }
  b_payload <- triomics_cnh_read_json(b_payload_path, "Stage B block results")
  b_envelope <- triomics_cnh_validate_envelope(b_envelope_path, c(
    list(artifact_type = "block_results", artifact_id = "stage-b-block-results",
         study_id = fresh$study_id, config_sha256 = fresh$config_sha256,
         payload_sha256 = triomics_sha256(b_payload_path), inputs = fresh$source_inputs)
  ), "Stage B block results")
  if (!identical(as.character(b_payload$study_id), fresh$study_id) ||
      !identical(as.character(b_payload$analysis_id), fresh$analysis_id) ||
      !identical(as.character(b_payload$preparation_context_id), fresh$preparation_context_id) ||
      !identical(as.character(b_payload$representation_id), fresh$representation_id) ||
      !identical(as.character(b_payload$config_sha256), fresh$config_sha256)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The Stage B feature result has a different config or analysis identity.", "B/block-results.json")
  }
  output_path <- file.path(current_root, "B", "clinical-associations.json")
  output_envelope_path <- file.path(current_root, "B", "clinical-associations.envelope.json")
  if (file.exists(output_path) || file.exists(output_envelope_path) ||
      triomics_is_symlink(output_path) || triomics_is_symlink(output_envelope_path)) {
    triomics_cnh_abort("B_SCORE_REQUEST_CONFLICT", "A clinical association result already exists; retries cannot replace it.", "B/clinical-associations.json")
  }

  fit <- triomics_associate_endpoints(
    fresh$request$endpoint_bundle, fresh$clinical_view, fresh$request$objective_spec
  )
  exported <- c(list(schema_version = "2.0"), triomics_clinical_association_export(fit))
  exported$clinical_dictionary_coverage <- triomics_cnh_public_dictionary_coverage(fresh$clinical)
  exported$native_transport <- list(
    request = list(
      artifact_id = as.character(fresh$request_reference$artifact_id),
      payload_file = as.character(fresh$request_reference$payload_file),
      payload_sha256 = as.character(fresh$request_reference$payload_sha256),
      envelope_file = as.character(fresh$request_reference$envelope_file),
      envelope_sha256 = as.character(fresh$request_reference$envelope_sha256),
      request_sha256 = as.character(fresh$request_reference$request_sha256)
    ),
    endpoint_bundle = list(
      artifact_id = as.character(fresh$endpoint_reference$artifact_id),
      payload_file = as.character(fresh$endpoint_reference$payload_file),
      payload_sha256 = as.character(fresh$endpoint_reference$payload_sha256),
      envelope_file = as.character(fresh$endpoint_reference$envelope_file),
      envelope_sha256 = as.character(fresh$endpoint_reference$envelope_sha256)
    ),
    clinical_source_sha256 = list(
      dictionary = fresh$clinical$dictionary_sha256,
      observations = fresh$clinical$observations_sha256,
      links = fresh$clinical$links_sha256
    )
  )
  inputs <- list(
    list(artifact_id = as.character(fresh$endpoint_reference$artifact_id),
         sha256 = as.character(fresh$endpoint_reference$payload_sha256)),
    list(artifact_id = paste0(fresh$endpoint_reference$artifact_id, ":envelope"),
         sha256 = as.character(fresh$endpoint_reference$envelope_sha256)),
    list(artifact_id = as.character(fresh$request_reference$artifact_id),
         sha256 = as.character(fresh$request_reference$payload_sha256)),
    list(artifact_id = paste0(fresh$request_reference$artifact_id, ":envelope"),
         sha256 = as.character(fresh$request_reference$envelope_sha256)),
    list(artifact_id = "clinical-dictionary-source", sha256 = fresh$clinical$dictionary_sha256),
    list(artifact_id = "clinical-observations-source", sha256 = fresh$clinical$observations_sha256),
    list(artifact_id = "clinical-links-source", sha256 = fresh$clinical$links_sha256)
  )
  privacy_class <- if (identical(fresh$config$data_class, "synthetic")) "synthetic" else "sensitive"
  written <- character()
  finalized <- FALSE
  on.exit({
    if (!finalized && length(written)) unlink(written, force = TRUE)
  }, add = TRUE)
  triomics_write_json_atomic(exported, output_path)
  written <- c(written, output_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "clinical_association_results",
    artifact_id = "b-t08-clinical-associations",
    study_id = fresh$study_id,
    run_id = "b-t08",
    status = fit$status,
    config_sha256 = fresh$config_sha256,
    payload = output_path,
    producer_commit = triomics_b07_git_ref(getwd()),
    inputs = inputs,
    reason_codes = if (identical(fit$status, "complete")) character() else "partial_family",
    privacy_class = privacy_class
  )
  envelope$analysis_id <- fresh$analysis_id
  envelope$preparation_context_id <- fresh$preparation_context_id
  envelope$representation_id <- fresh$representation_id
  triomics_write_envelope(envelope, output_envelope_path)
  written <- c(written, output_envelope_path)
  payload_readback <- triomics_cnh_read_json(output_path, "native clinical association result")
  envelope_readback <- triomics_cnh_validate_envelope(output_envelope_path, c(
    list(artifact_type = "clinical_association_results", artifact_id = "b-t08-clinical-associations",
         study_id = fresh$study_id, config_sha256 = fresh$config_sha256,
         analysis_id = fresh$analysis_id,
         preparation_context_id = fresh$preparation_context_id,
         representation_id = fresh$representation_id,
         status = fit$status, inputs = inputs, privacy_class = privacy_class,
         payload_sha256 = triomics_sha256(output_path))
  ), "native clinical association result")
  if (!triomics_cnh_same_json(payload_readback$results, triomics_clinical_association_export(fit)$results) ||
      !identical(triomics_sha256(output_path), envelope_readback$payload_sha256)) {
    triomics_cnh_abort("B_SCORE_PUBLICATION_FAILED", "The native clinical result failed exact result or envelope read-back.", "B/clinical-associations.json")
  }
  finalized <- TRUE
  list(
    status = fit$status,
    artifact_ref = list(artifact_id = "b-t08-clinical-associations",
                        path = file.path("B", "clinical-associations.json"),
                        sha256 = triomics_sha256(output_path)),
    payload_path = output_path,
    envelope_path = output_envelope_path,
    fit = fit
  )
}
