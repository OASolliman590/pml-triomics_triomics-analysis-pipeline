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
  if (!storage %in% c("character", "integer", "double", "logical")) {
    triomics_cnh_r_value_abort("The saved C publication context has an unsupported atomic storage type.")
  }
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
    if (length(classes) && !identical(classes, "Date")) {
      triomics_cnh_r_value_abort("The saved C publication context declares an unsupported atomic class.")
    }
    if (length(classes)) class(value) <- classes
    return(value)
  }
  if (identical(type, "factor")) {
    codes <- as.integer(unlist(node$codes, use.names = FALSE))
    missing <- as.integer(unlist(node$missing, use.names = FALSE))
    levels <- as.character(unlist(node$levels, use.names = FALSE))
    if (anyNA(codes) || any(codes < 0L) || any(codes > length(levels)) ||
        any(missing < 1L | missing > length(codes))) {
      triomics_cnh_r_value_abort("The saved C factor codes or levels are invalid.")
    }
    if (length(missing)) codes[missing] <- NA_integer_
    value <- structure(codes, levels = levels,
                       class = if (isTRUE(node$ordered)) c("ordered", "factor") else "factor")
    if (isTRUE(node$has_names)) names(value) <- as.character(unlist(node$names, use.names = FALSE))
    return(value)
  }
  if (identical(type, "list")) {
    values <- lapply(node$values, triomics_cnh_decode_r_value)
    if (isTRUE(node$has_names)) names(values) <- as.character(unlist(node$names, use.names = FALSE))
    classes <- as.character(unlist(node$class, use.names = FALSE))
    if (length(classes) && !identical(classes, "AsIs")) {
      triomics_cnh_r_value_abort("The saved C publication context declares an unsupported list class.")
    }
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
    storage <- as.character(node$storage %||% "")
    if (!storage %in% c("integer", "double") || typeof(value) != storage ||
        length(dimensions) != 2L || anyNA(dimensions) || any(dimensions < 0L) ||
        prod(dimensions) != length(value)) {
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

triomics_cnh_declared_source_files <- function(config) {
  config_path <- attr(config, "config_path")
  if (!triomics_nonempty_string(as.character(config_path %||% "")) ||
      !file.exists(as.character(config_path))) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "Declared source binding needs the validated configuration path.",
      "config_path", stage = "C"
    )
  }
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  declarations <- list("metadata.file" = config$metadata$file)
  if (isTRUE(config$clinical$enabled)) {
    declarations <- c(declarations, list(
      "clinical.dictionary_file" = config$clinical$dictionary_file,
      "clinical.observations_file" = config$clinical$observations_file,
      "clinical.links_file" = config$clinical$links_file
    ))
  }
  lapply(names(declarations), function(key) {
    declared_path <- declarations[[key]]
    resolved_path <- tryCatch(
      triomics_resolve_declared_input(declared_path, key, root),
      error = function(error) NULL
    )
    if (!triomics_nonempty_string(as.character(declared_path %||% "")) ||
        is.null(resolved_path) || !file.exists(resolved_path) || dir.exists(resolved_path) ||
        triomics_is_symlink(resolved_path)) {
      triomics_cnh_abort(
        "C_PUBLICATION_CONTEXT_INVALID", paste0("The declared source file is missing or unsafe: ", key),
        key, stage = "C"
      )
    }
    digest <- tryCatch(triomics_sha256(resolved_path), error = function(error) NULL)
    if (!triomics_sha256_text(digest)) {
      triomics_cnh_abort(
        "C_PUBLICATION_CONTEXT_INVALID", paste0("The declared source file cannot be hashed: ", key),
        key, stage = "C"
      )
    }
    list(
      key = key,
      declared_path = as.character(declared_path),
      resolved_path = resolved_path,
      sha256 = digest
    )
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
  run_relative <- file.path("registries", "run-C.json")
  run_candidate <- tryCatch(
    triomics_b07_child_path(root, run_relative), error = function(error) NULL
  )
  if (is.null(run_candidate) || !file.exists(run_candidate) || dir.exists(run_candidate) ||
      triomics_is_symlink(run_candidate)) {
    triomics_cnh_abort(
      "B_SCORE_LINEAGE_INVALID", "The C completion registry path is missing or escapes the output root.",
      run_relative, stage = "B"
    )
  }
  run_path <- triomics_cnh_path(root, run_relative, triomics_sha256(run_candidate), "C run record")
  run_record <- triomics_cnh_read_json(run_path, "C run record")
  if (!identical(run_record$stage, "C") || !identical(run_record$status, "complete")) {
    triomics_cnh_abort(
      "B_SCORE_LINEAGE_INVALID", "The saved C run record is incomplete or mismatched.",
      run_relative, stage = "B"
    )
  }
  saved_context <- tryCatch(
    triomics_cnh_read_saved_c_context(config, root),
    error = function(error) error
  )
  if (inherits(saved_context, "error")) {
    triomics_cnh_abort(
      "B_SCORE_LINEAGE_INVALID",
      paste0("The saved C context failed source and producer identity validation: ",
             conditionMessage(saved_context)),
      "C/native-publication-context.json", stage = "B"
    )
  }
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

  c_payload_relative <- file.path("C", "integration-results.json")
  c_payload_candidate <- tryCatch(
    triomics_b07_child_path(root, c_payload_relative), error = function(error) NULL
  )
  if (is.null(c_payload_candidate) || !file.exists(c_payload_candidate) ||
      dir.exists(c_payload_candidate) || triomics_is_symlink(c_payload_candidate)) {
    triomics_cnh_abort(
      "B_SCORE_LINEAGE_INVALID", "The saved C integration payload path is missing or unsafe.",
      c_payload_relative, stage = "B"
    )
  }
  c_payload_path <- triomics_cnh_path(
    root, c_payload_relative, expected_sha256 = triomics_sha256(c_payload_candidate),
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
  if (!identical(run_record$study_id, source_identity$study_id)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The saved C run record is incomplete or mismatched.", "registries/run-C.json")
  }

  ingested <- triomics_ingest_configured_study(config, config_path)
  if (!identical(as.character(ingested$study_id), source_identity$study_id)) {
    triomics_cnh_abort("B_SCORE_LINEAGE_INVALID", "The current molecular sources name a different study.", "study_id")
  }
  normalized_metadata <- list(
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels
  )
  if (!triomics_cnh_same_json(saved_context$identity$normalized_metadata, normalized_metadata)) {
    triomics_cnh_abort(
      "B_SCORE_LINEAGE_INVALID", "The saved C normalized metadata differs from current declared source bytes.",
      "metadata.file", stage = "B"
    )
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
    missing_map <- variable$missing_map
    declared_dispositions <- as.character(unlist(lapply(missing_map, function(value) {
      if (is.null(value)) NA_character_ else value
    }), recursive = TRUE, use.names = FALSE))
    unsupported_dispositions <- unique(declared_dispositions[
      is.na(declared_dispositions) | !declared_dispositions %in% safe_dispositions
    ])
    dispositions <- sort(unique(declared_dispositions[
      !is.na(declared_dispositions) & declared_dispositions %in% safe_dispositions
    ]), method = "radix")
    unsupported_count <- as.integer(length(unsupported_dispositions))
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

triomics_cnh_native_c_route <- function(config) {
  if (!is.list(config) || !isTRUE(config$clinical$enabled) ||
      !is.list(config$clinical$objectives) || !length(config$clinical$objectives) ||
      config$study_id %in% c("triomics_b_t08_fixture", "triomics_b_t09_fixture")) {
    return(NULL)
  }
  modes <- vapply(config$clinical$objectives, function(objective) {
    as.character(objective$mode %||% "")
  }, character(1L))
  if (!any(modes == "between_person")) return(NULL)
  if (any(modes != "between_person")) {
    triomics_cnh_abort(
      "C_PUBLICATION_ROUTE_UNSUPPORTED",
      "The saved native C publication route accepts between-person objectives only.",
      "clinical.objectives.mode", stage = "C"
    )
  }
  if (length(config$analyses) != 1L) {
    triomics_cnh_abort(
      "C_PUBLICATION_ROUTE_UNSUPPORTED", "The saved native C route requires one analysis.",
      "analyses", stage = "C"
    )
  }
  analysis <- config$analyses[[1L]]
  integrations <- analysis$integration
  supported <- c("concat_pca", "mixomics_diablo", "mixomics_block_spls", "mofa2")
  if (!is.list(integrations) || length(integrations) != 1L ||
      !as.character(integrations[[1L]]$method %||% "") %in% supported) {
    triomics_cnh_abort(
      "C_PUBLICATION_ROUTE_UNSUPPORTED",
      "The saved native C route requires one registered C-08 integration request.",
      "analyses.integration", stage = "C"
    )
  }
  if (any(vapply(config$clinical$objectives, function(objective) {
    !triomics_nonempty_string(as.character(objective$time_window_ref %||% ""))
  }, logical(1L)))) {
    triomics_cnh_abort(
      "C_PUBLICATION_ROUTE_UNSUPPORTED",
      "Every between-person objective requires an explicit visit window.",
      "clinical.objectives.time_window_ref", stage = "C"
    )
  }
  list(analysis = analysis, integration = integrations[[1L]])
}

triomics_cnh_context_paths <- function(root) {
  list(
    payload = file.path(root, "C", "native-publication-context.json"),
    envelope = file.path(root, "C", "native-publication-context.envelope.json")
  )
}

triomics_cnh_c_file_references <- function(root) {
  required <- c(
    "C/integration-results.json", "C/integration-results.envelope.json",
    "C/native-states/integration-01.json", "C/native-states/integration-01.envelope.json",
    "C/score-endpoints/integration-01.json", "C/score-endpoints/integration-01.envelope.json",
    "B/clinical-score-association-requests/request-01.json",
    "B/clinical-score-association-requests/request-01.envelope.json"
  )
  paths <- vapply(required, function(relative) {
    path <- tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
    if (is.null(path) || !file.exists(path) || dir.exists(path) || triomics_is_symlink(path)) {
      triomics_cnh_abort(
        "C_PUBLICATION_CONTEXT_INVALID",
        paste0("The initial native C output is missing or unsafe: ", relative), relative, stage = "C"
      )
    }
    path
  }, character(1L), USE.NAMES = FALSE)
  preparation_root <- tryCatch(
    triomics_b07_child_path(root, "C/preparation-states"), error = function(error) NULL
  )
  preparation_files <- character()
  if (is.null(preparation_root)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The C preparation-state path is unsafe.",
      "C/preparation-states", stage = "C"
    )
  }
  if (file.exists(preparation_root)) {
    if (!dir.exists(preparation_root) || triomics_is_symlink(preparation_root)) {
      triomics_cnh_abort(
        "C_PUBLICATION_CONTEXT_INVALID", "The C preparation-state directory is unsafe.",
        "C/preparation-states", stage = "C"
      )
    }
    candidates <- list.files(preparation_root, recursive = TRUE, full.names = TRUE,
                             all.files = TRUE, no.. = TRUE)
    for (candidate in candidates) {
      relative <- substring(normalizePath(candidate, winslash = "/", mustWork = TRUE),
                            nchar(root) + 2L)
      safe <- tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
      if (is.null(safe) || triomics_is_symlink(candidate)) {
        triomics_cnh_abort(
          "C_PUBLICATION_CONTEXT_INVALID", "A C preparation-state member is unsafe.",
          relative, stage = "C"
        )
      }
      info <- file.info(candidate)
      if (isTRUE(info$isdir)) next
      if (grepl("[.]tmp$", basename(candidate))) {
        triomics_cnh_abort(
          "C_PUBLICATION_CONTEXT_INVALID", "A partial preparation-state write is present.",
          relative, stage = "C"
        )
      }
      preparation_files <- c(preparation_files, safe)
    }
  }
  paths <- unique(c(unname(paths), preparation_files))
  paths <- paths[order(vapply(paths, function(path) {
    substring(path, nchar(root) + 2L)
  }, character(1L)), method = "radix")]
  lapply(paths, function(path) {
    list(
      path = substring(path, nchar(root) + 2L),
      sha256 = triomics_sha256(path)
    )
  })
}

triomics_cnh_context_envelope_inputs <- function(source_inputs, file_references,
                                                  declared_source_files = list()) {
  declared_inputs <- lapply(declared_source_files, function(source) {
    list(artifact_id = paste0("declared-source-file:", source$key), sha256 = source$sha256)
  })
  file_inputs <- lapply(file_references, function(reference) {
    list(artifact_id = paste0("c-stage-file:", reference$path), sha256 = reference$sha256)
  })
  inputs <- c(source_inputs, declared_inputs, file_inputs)
  ids <- vapply(inputs, function(input) as.character(input$artifact_id), character(1L))
  if (anyDuplicated(ids)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "C publication context input artifact ids are duplicated.",
      "inputs", stage = "C"
    )
  }
  inputs
}

triomics_cnh_validate_saved_identity_matrices <- function(root, c_input_identity) {
  invalid <- function(message) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", message,
      "c_input_identity.blocks", stage = "C"
    )
  }
  relative <- file.path("C", "integration-results.json")
  path <- tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
  if (is.null(path) || !file.exists(path) || dir.exists(path) || triomics_is_symlink(path)) {
    invalid("The C integration payload needed to authenticate typed matrices is missing or unsafe.")
  }
  payload <- tryCatch(triomics_cnh_read_json(path, "C integration results"), error = function(error) NULL)
  if (!is.list(payload$integrations) || length(payload$integrations) != 1L) {
    invalid("The C integration payload does not contain one completed native identity.")
  }
  integration <- payload$integrations[[1L]]
  provenance <- integration$result$params$provenance
  expected_hashes <- provenance$matrix_sha256
  blocks <- c_input_identity$blocks
  if (!is.list(blocks) || !length(blocks) || is.null(names(blocks)) ||
      !is.list(expected_hashes) || is.null(names(expected_hashes)) ||
      !setequal(names(blocks), names(expected_hashes))) {
    invalid("The typed C matrix identities do not match the independent integration result.")
  }
  for (block_id in names(blocks)) {
    block <- blocks[[block_id]]
    matrix <- block$matrix
    feature_ids <- as.character(unlist(block$feature_ids, use.names = FALSE))
    unit_ids <- as.character(unlist(block$unit_ids, use.names = FALSE))
    matrix_sha256 <- if (is.matrix(matrix)) {
      digest::digest(matrix, algo = "sha256", serialize = TRUE)
    } else ""
    if (!is.matrix(matrix) || is.null(rownames(matrix)) || is.null(colnames(matrix)) ||
        !identical(rownames(matrix), feature_ids) || !identical(colnames(matrix), unit_ids) ||
        !identical(as.character(matrix_sha256), as.character(block$matrix_sha256)) ||
        !identical(as.character(matrix_sha256), as.character(expected_hashes[[block_id]])) ||
        !identical(dim(matrix), c(length(feature_ids), length(unit_ids)))) {
      invalid(paste0("The saved typed matrix differs from the exact C producer matrix for ", block_id, "."))
    }
  }
  invisible(list(payload = payload, integration = integration, provenance = provenance))
}

triomics_cnh_c01_matrix <- function(payload, row_key, column_key) {
  rows <- as.character(unlist(payload$rows[[row_key]], use.names = FALSE))
  columns <- as.character(unlist(payload$columns[[column_key]], use.names = FALSE))
  values <- lapply(payload$values, function(row) {
    as.numeric(unlist(row, recursive = FALSE, use.names = FALSE))
  })
  if (!length(rows) || !length(columns) || length(values) != length(rows) ||
      any(vapply(values, length, integer(1L)) != length(columns))) return(NULL)
  matrix_value <- do.call(rbind, values)
  dimnames(matrix_value) <- list(rows, columns)
  matrix_value
}

triomics_cnh_expected_selected_values <- function(loadings, selected) {
  stats::setNames(lapply(names(selected), function(block_id) {
    features <- selected[[block_id]]
    stats::setNames(lapply(features, function(feature_key) {
      loading <- loadings[[block_id]][feature_key, , drop = TRUE]
      list(
        feature_key = feature_key,
        component_loadings = as.list(stats::setNames(as.numeric(loading), names(loading)))
      )
    }), features)
  }), names(selected))
}

triomics_cnh_c01_common_parameters <- function(params, score_matrix, backend,
                                                required_views, block_ids) {
  parameters <- params[setdiff(names(params), "provenance")]
  state <- parameters$state
  required_views <- as.character(unlist(required_views, use.names = FALSE))
  block_ids <- as.character(unlist(block_ids, use.names = FALSE))
  for (field in c("rotation", "design", "native_design")) {
    value <- if (identical(field, "rotation") && is.list(state)) state$rotation else parameters[[field]]
    if (!is.list(value) || !length(value) || !all(vapply(value, is.list, logical(1L)))) next
    rows <- lapply(value, function(row) unlist(row, recursive = FALSE, use.names = FALSE))
    numeric_rows <- vapply(rows, function(row) is.numeric(row) && all(is.finite(row)), logical(1L))
    if (!any(numeric_rows)) next
    column_ids <- switch(field,
      rotation = colnames(score_matrix),
      design = if (identical(backend, "mixomics_block_spls")) c(block_ids, "Y") else required_views,
      native_design = c(block_ids, "Y")
    )
    row_ids <- switch(field,
      rotation = {
        feature_map <- if (is.list(state)) state$feature_map else NULL
        if (is.list(feature_map) && length(feature_map) && all(vapply(feature_map, function(item) {
          is.list(item) && triomics_nonempty_string(as.character(item$column_id %||% ""))
        }, logical(1L)))) {
          vapply(feature_map, function(item) as.character(item$column_id), character(1L))
        } else character()
      },
      design = column_ids,
      native_design = column_ids
    )
    if (!all(numeric_rows) || length(row_ids) != length(rows) || length(column_ids) < 1L ||
        any(vapply(rows, length, integer(1L)) != length(column_ids))) {
      triomics_cnh_abort(
        "C_PUBLICATION_CONTEXT_INVALID",
        paste0("The C producer omitted dimensions needed to authenticate result.params.", field),
        paste0("C/integration-results.json.result.params.", field), stage = "C"
      )
    }
    matrix_value <- do.call(rbind, lapply(rows, as.numeric))
    dimnames(matrix_value) <- list(row_ids, column_ids)
    if (identical(field, "rotation")) {
      parameters$state$rotation <- triomics_c06_matrix_payload(
        matrix_value, "result.params.state.rotation"
      )
    } else {
      parameters[[field]] <- triomics_c06_matrix_payload(matrix_value, paste0("result.params.", field))
    }
  }
  triomics_c06_json_value(parameters)
}

triomics_cnh_validate_saved_handoff_producer <- function(root, identity, source_inputs,
                                                          handoff, config_sha256) {
  invalid <- function(message, key = "C/integration-results.json") {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", message, key, stage = "C")
  }
  producer <- triomics_cnh_validate_saved_identity_matrices(root, identity)
  result_item <- producer$integration
  result_payload <- result_item$result
  provenance <- result_payload$params$provenance
  integration_id <- as.character(result_item$integration_id %||% result_item$method %||% "")
  if (!identical(result_item$status, "complete") ||
      !triomics_nonempty_string(integration_id) ||
      !identical(as.character(result_payload$backend), as.character(result_item$method)) ||
      !triomics_cnh_same_json(provenance[c("study_id", "analysis_id", "preparation_context_id",
                                           "representation_id")],
                              identity[c("study_id", "analysis_id", "preparation_context_id",
                                         "representation_id")])) {
    invalid("The completed integration result does not reproduce the saved C handoff identity.")
  }

  score_matrix <- triomics_cnh_c01_matrix(result_payload$scores, "unit_ids", "component_ids")
  loadings <- stats::setNames(lapply(result_payload$loadings, function(item) {
    triomics_cnh_c01_matrix(item, "feature_keys", "component_ids")
  }), vapply(result_payload$loadings, function(item) as.character(item$block_id), character(1L)))
  variance_matrix <- triomics_cnh_c01_matrix(result_payload$variance, "block_ids", "component_ids")
  selected <- stats::setNames(lapply(result_payload$selected, function(value) {
    as.character(unlist(value, recursive = FALSE, use.names = FALSE))
  }), names(result_payload$selected))
  if (is.null(score_matrix) || is.null(variance_matrix) || !length(loadings) ||
      any(vapply(loadings, is.null, logical(1L))) || is.null(names(selected)) ||
      !identical(names(loadings), names(selected))) {
    invalid("The C integration result cannot reproduce its keyed score, loading, variance, and selection facts.")
  }
  result <- structure(list(
    scores = score_matrix,
    loadings = loadings,
    variance = variance_matrix,
    selected = selected,
    backend = as.character(result_payload$backend),
    params = result_payload$params
  ), class = "IntegrationResult")
  for (field in c("metric", "assay_space", "denominator", "semantics")) {
    attr(result$variance, field) <- result_payload$variance[[field]]
  }
  result <- tryCatch(triomics_validate_integration_result(result), error = function(error) NULL)
  if (is.null(result)) invalid("The C result matrices do not satisfy the IntegrationResult contract.")

  block_ids <- names(result$selected)
  unit_ids <- as.character(unlist(provenance$unit_map %||% result$params$training_unit_ids,
                                  use.names = FALSE))
  feature_map <- provenance$feature_map
  matrix_hashes <- provenance$matrix_sha256
  required_views <- as.character(unlist(provenance$required_views %||% block_ids,
                                        use.names = FALSE))
  if (!length(unit_ids) || is.null(names(feature_map)) ||
      !identical(names(feature_map), block_ids) || is.null(names(matrix_hashes)) ||
      !identical(names(matrix_hashes), block_ids) ||
      !setequal(unit_ids, rownames(score_matrix)) || length(unit_ids) != nrow(score_matrix) ||
      !setequal(required_views, block_ids) || length(required_views) != length(block_ids)) {
    invalid("The C result provenance cannot reproduce its training units, views, or block identities.")
  }
  if (!is.data.frame(identity$analysis_unit_map) ||
      !identical(as.character(triomics_b07_prov_value_sha256(identity$analysis_unit_map)),
                 as.character(identity$analysis_unit_map_sha256))) {
    invalid("The typed C analysis-unit map failed its producer self-identity check.",
            "c_input_identity.analysis_unit_map_sha256")
  }
  eligible_identity_units <- as.character(unlist(
    identity$analysis_unit_map$unit_id[identity$analysis_unit_map$eligible %in% TRUE],
    use.names = FALSE
  ))
  if (!length(eligible_identity_units) || any(!unit_ids %in% eligible_identity_units)) {
    invalid("C result training units are not covered by the typed C analysis-unit identity.",
            "c_input_identity.analysis_unit_map")
  }
  for (block_id in block_ids) {
    source_features <- as.character(unlist(identity$blocks[[block_id]]$feature_ids,
                                           use.names = FALSE))
    fitted_features <- as.character(unlist(feature_map[[block_id]], use.names = FALSE))
    if (!identical(fitted_features, source_features)) {
      invalid(paste0("C feature map differs from the typed source block: ", block_id),
              paste0("c_input_identity.blocks.", block_id, ".feature_ids"))
    }
  }

  identity_expected <- list(
    study_id = provenance$study_id,
    analysis_id = provenance$analysis_id,
    preparation_context_id = provenance$preparation_context_id,
    representation_id = provenance$representation_id,
    unit_ids = sort(unit_ids, method = "radix"),
    feature_map = stats::setNames(lapply(block_ids, function(block_id) {
      sort(as.character(unlist(feature_map[[block_id]], use.names = FALSE)), method = "radix")
    }), block_ids),
    matrix_sha256 = matrix_hashes[block_ids]
  )
  identity_sha256 <- triomics_hash_text(triomics_canonical_json(identity_expected))
  context_expected <- identity_expected[c("study_id", "analysis_id", "preparation_context_id",
                                          "representation_id")]

  native_relative <- file.path("C", "native-states", "integration-01.json")
  native_candidate <- tryCatch(triomics_b07_child_path(root, native_relative), error = function(error) NULL)
  if (is.null(native_candidate) || !file.exists(native_candidate) || dir.exists(native_candidate) ||
      triomics_is_symlink(native_candidate)) {
    invalid("The bound C native-state producer payload is missing or unsafe.", native_relative)
  }
  native_path <- triomics_cnh_path(root, native_relative, triomics_sha256(native_candidate),
                                   "C native state")
  native_envelope_relative <- file.path("C", "native-states", "integration-01.envelope.json")
  native_envelope_candidate <- tryCatch(
    triomics_b07_child_path(root, native_envelope_relative), error = function(error) NULL
  )
  if (is.null(native_envelope_candidate) || !file.exists(native_envelope_candidate) ||
      dir.exists(native_envelope_candidate) || triomics_is_symlink(native_envelope_candidate)) {
    invalid("The bound C native-state envelope is missing or unsafe.", native_envelope_relative)
  }
  native_envelope_path <- triomics_cnh_path(
    root, native_envelope_relative, triomics_sha256(native_envelope_candidate), "C native state envelope"
  )
  native <- triomics_cnh_read_json(native_path, "C native state")
  native_payload <- native$native
  native_ref <- list(
    artifact_id = as.character(native$artifact_id),
    sha256 = triomics_sha256(native_path)
  )
  result_relative <- file.path("C", "integration-results.json")
  result_path <- tryCatch(triomics_b07_child_path(root, result_relative), error = function(error) NULL)
  result_envelope_relative <- file.path("C", "integration-results.envelope.json")
  result_envelope_candidate <- tryCatch(
    triomics_b07_child_path(root, result_envelope_relative), error = function(error) NULL
  )
  if (is.null(result_path) || !file.exists(result_path) || dir.exists(result_path) ||
      triomics_is_symlink(result_path) || is.null(result_envelope_candidate) ||
      !file.exists(result_envelope_candidate) || dir.exists(result_envelope_candidate) ||
      triomics_is_symlink(result_envelope_candidate)) {
    invalid("The C integration result producer payload or envelope is missing or unsafe.")
  }
  result_envelope_path <- triomics_cnh_path(
    root, result_envelope_relative, triomics_sha256(result_envelope_candidate),
    "C integration results envelope"
  )
  triomics_cnh_validate_envelope(result_envelope_path, c(
    list(artifact_type = "integration_results", artifact_id = "stage-c-integration-results",
         study_id = as.character(identity$study_id), config_sha256 = config_sha256,
         analysis_id = as.character(identity$analysis_id),
         preparation_context_id = as.character(identity$preparation_context_id),
         representation_id = as.character(identity$representation_id), status = "complete",
         payload_sha256 = triomics_sha256(result_path),
         inputs = c(source_inputs, list(native_ref)))
  ), "C integration results")
  native_inputs <- lapply(block_ids, function(block_id) {
    list(artifact_id = paste(integration_id, block_id, sep = "::"),
         sha256 = as.character(matrix_hashes[[block_id]]))
  })
  names(native_inputs) <- NULL
  preparation_hashes <- native$preparation_state_hashes %||% list()
  preparation_refs <- native$preparation_state_refs %||% list()
  if (!is.list(preparation_hashes) || !is.list(preparation_refs) ||
      !identical(sort(names(preparation_hashes)), sort(names(preparation_refs))) ||
      !triomics_cnh_same_json(preparation_hashes, provenance$preparation_state_hashes %||% list())) {
    invalid("The C native-state preparation hashes do not match C result producer lineage.",
            "C/native-states/integration-01.json")
  }
  preparation_inputs <- lapply(names(preparation_refs), function(block_id) {
    reference <- preparation_refs[[block_id]]
    path <- if (is.list(reference)) tryCatch(
      triomics_b07_child_path(root, as.character(reference$path %||% "")), error = function(error) NULL
    ) else NULL
    if (is.null(path) || !file.exists(path) || dir.exists(path) || triomics_is_symlink(path) ||
        !identical(triomics_sha256(path), as.character(reference$payload_sha256)) ||
        !identical(as.character(reference$state_hash), as.character(preparation_hashes[[block_id]]))) {
      invalid(paste0("The immutable C preparation state failed its producer binding: ", block_id),
              "C/native-states/integration-01.json")
    }
    list(artifact_id = paste("preparation-state", block_id, sep = "::"),
         sha256 = as.character(reference$payload_sha256))
  })
  expected_native_inputs <- c(source_inputs, native_inputs, preparation_inputs)
  triomics_cnh_validate_envelope(native_envelope_path, c(
    list(artifact_type = "integration_native_state", artifact_id = native_ref$artifact_id,
         study_id = as.character(identity$study_id), config_sha256 = config_sha256,
         analysis_id = as.character(identity$analysis_id),
         preparation_context_id = as.character(identity$preparation_context_id),
         representation_id = as.character(identity$representation_id), status = "complete",
         payload_sha256 = triomics_sha256(native_path), inputs = expected_native_inputs)
  ), "C native state")
  native_identity <- list(
    study_id = native$study_id, analysis_id = native$analysis_id,
    preparation_context_id = native$preparation_context_id,
    representation_id = native$representation_id
  )
  if (!identical(as.character(native$integration_id), integration_id) ||
      !identical(as.character(native$backend), as.character(result$backend)) ||
      !triomics_cnh_same_json(native_identity, context_expected) ||
      !triomics_sha256_text(native_ref$sha256) || !is.list(native_payload)) {
    invalid("The C native-state producer does not match the fitted result identity.",
            "C/native-states/integration-01.json")
  }

  common_expected <- list(
    backend = result$backend,
    scores = triomics_c06_matrix_payload(result$scores, "saved_common_scores"),
    loadings = triomics_c06_matrix_list_payload(result$loadings, "saved_common_loadings"),
    variance = triomics_c06_matrix_payload(result$variance, "saved_common_variance"),
    selected = triomics_c06_json_value(result$selected),
    parameters = triomics_cnh_c01_common_parameters(
      result$params, result$scores, result$backend, required_views, block_ids
    )
  )
  eligibility <- result$params$eligibility
  if (!is.list(eligibility)) eligibility <- list()
  if (identical(result$backend, "mixomics_diablo")) {
    eligibility$available_rank <- result$params$available_training_rank
  }
  if (identical(result$backend, "mixomics_block_spls")) {
    feature_flow <- result$params$feature_flow
    if (!is.list(feature_flow) || is.null(names(feature_flow)) ||
        !identical(names(feature_flow), block_ids)) {
      invalid("The block.spls producer omitted the feature-flow map needed to authenticate eligibility details.",
              "C/integration-results.json.result.params.feature_flow")
    }
    input_keys <- stats::setNames(lapply(block_ids, function(block_id) {
      as.character(unlist(feature_flow[[block_id]]$input, use.names = FALSE))
    }), block_ids)
    retained <- stats::setNames(lapply(block_ids, function(block_id) {
      as.character(unlist(feature_flow[[block_id]]$retained, use.names = FALSE))
    }), block_ids)
    excluded <- stats::setNames(lapply(block_ids, function(block_id) {
      as.character(unlist(feature_flow[[block_id]]$excluded_zero_variance, use.names = FALSE))
    }), block_ids)
    available_rank <- stats::setNames(vapply(block_ids, function(block_id) {
      matrix_value <- identity$blocks[[block_id]]$matrix
      source_features <- as.character(unlist(identity$blocks[[block_id]]$feature_ids,
                                              use.names = FALSE))
      if (!is.matrix(matrix_value) || !identical(rownames(matrix_value), source_features) ||
          !setequal(colnames(matrix_value), unit_ids)) {
        invalid(paste0("The block.spls source matrix cannot reproduce eligibility rank for ", block_id),
                paste0("c_input_identity.blocks.", block_id))
      }
      if (!identical(input_keys[[block_id]], source_features)) {
        invalid(paste0("The block.spls feature-flow input map differs from source identity for ", block_id),
                paste0("C/integration-results.json.result.params.feature_flow.", block_id))
      }
      feature_sd <- apply(matrix_value[, unit_ids, drop = FALSE], 1L, stats::sd)
      expected_retained <- source_features[is.finite(feature_sd) & feature_sd > .Machine$double.eps]
      expected_excluded <- source_features[!(is.finite(feature_sd) & feature_sd > .Machine$double.eps)]
      if (!identical(retained[[block_id]], expected_retained) ||
          !identical(excluded[[block_id]], expected_excluded)) {
        invalid(paste0("The block.spls feature-flow retention differs from source matrices for ", block_id),
                paste0("C/integration-results.json.result.params.feature_flow.", block_id))
      }
      values <- t(matrix_value[retained[[block_id]], unit_ids, drop = FALSE])
      qr(scale(values, center = TRUE, scale = FALSE), tol = 1e-10)$rank
    }, integer(1L)), block_ids)
    eligibility$input_keys <- input_keys
    eligibility$retained <- retained
    eligibility$excluded <- excluded
    eligibility$available_rank <- available_rank
  }
  eligibility_expected <- list(
    status = eligibility$status %||% "validated",
    block_ids = as.list(block_ids),
    required_views = as.list(required_views),
    training_unit_ids = as.list(unit_ids),
    feature_map = triomics_c06_json_value(feature_map),
    matrix_sha256 = triomics_c06_json_value(matrix_hashes[block_ids]),
    source_roles = triomics_c06_json_value(provenance$source_roles),
    source_selection = triomics_c06_json_value(provenance$source_selection),
    outcome_eligibility = triomics_c06_json_value(provenance$outcome_eligibility),
    preparation_state_hashes = triomics_c06_json_value(preparation_hashes)
  )
  eligibility$block_ids <- as.list(block_ids)
  eligibility$unit_ids <- as.list(unit_ids)
  eligibility$training_unit_ids <- as.list(unit_ids)
  eligibility$required_views <- as.list(required_views)
  eligibility$preparation_state_hashes <- preparation_hashes
  eligibility_expected$details <- triomics_c06_eligibility_details(
    eligibility, unit_ids, required_views, feature_map, provenance
  )
  selection_rule <- triomics_c06_selection_definition(result)
  selected_values <- triomics_cnh_expected_selected_values(result$loadings, selected)
  selection_expected <- list(
    rule = selection_rule,
    selected = triomics_c06_json_value(selected),
    selected_values = triomics_c06_json_value(selected_values)
  )
  variance_definition <- triomics_c06_variance_definition(result)
  native_variance_definition <- triomics_c06_native_variance_definition(result)
  variance_expected <- list(
    definition = variance_definition,
    native_definition = native_variance_definition,
    common_definition = variance_definition,
    native_values = native_payload$variance_values,
    common_values = common_expected$variance,
    comparison_policy = "compare_only_identical_metric_units_denominator_and_semantics"
  )
  evidence_expected <- list(
    schema_version = "2.0", artifact_type = "integration_evidence",
    integration_id = integration_id, status = "complete", backend = result$backend,
    native_artifact_ref = native_ref, context = context_expected,
    identity_sha256 = identity_sha256, identity = triomics_c06_json_value(identity_expected),
    eligibility = eligibility_expected, native = native_payload, common = common_expected,
    selection = selection_expected, variance = variance_expected
  )
  evidence_saved <- handoff$integration_evidence[[1L]]
  if (length(handoff$integration_evidence) != 1L || !is.list(evidence_saved)) {
    invalid("The initial handoff must preserve exactly one producer-backed integration record.",
            "initial_handoff.integration_evidence")
  }
  for (field in names(identity_expected)) {
    if (!triomics_cnh_same_json(identity_expected[[field]], evidence_saved$identity[[field]])) {
      invalid(paste0("Saved integration identity field '", field,
                     "' does not reproduce from C result provenance."),
              paste0("initial_handoff.integration_evidence.identity.", field))
    }
  }
  for (field in names(evidence_expected)) {
    if (!triomics_cnh_same_json(evidence_expected[[field]], evidence_saved[[field]])) {
      if (identical(field, "eligibility") && is.list(evidence_saved[[field]])) {
        for (eligibility_field in names(evidence_expected[[field]])) {
          if (!triomics_cnh_same_json(evidence_expected[[field]][[eligibility_field]],
                                      evidence_saved[[field]][[eligibility_field]])) {
            if (identical(eligibility_field, "details") &&
                is.list(evidence_expected[[field]]$details) &&
                is.list(evidence_saved[[field]]$details)) {
              detail_fields <- union(names(evidence_expected[[field]]$details),
                                     names(evidence_saved[[field]]$details))
              detail_mismatch <- detail_fields[!vapply(detail_fields, function(detail_field) {
                triomics_cnh_same_json(evidence_expected[[field]]$details[[detail_field]],
                                       evidence_saved[[field]]$details[[detail_field]])
              }, logical(1L))]
              if (length(detail_mismatch)) {
                invalid(paste0("Saved eligibility detail '", detail_mismatch[[1L]],
                               "' differs from producer facts."),
                        paste0("initial_handoff.integration_evidence.eligibility.details.",
                               detail_mismatch[[1L]]))
              }
            }
            invalid(paste0("Saved eligibility field '", eligibility_field,
                           "' differs from producer facts."),
                    paste0("initial_handoff.integration_evidence.eligibility.", eligibility_field))
          }
        }
      }
      if (identical(field, "common") && is.list(evidence_saved[[field]])) {
        for (common_field in names(evidence_expected[[field]])) {
          if (!triomics_cnh_same_json(evidence_expected[[field]][[common_field]],
                                      evidence_saved[[field]][[common_field]])) {
            invalid(paste0("Saved common result field '", common_field,
                           "' differs from C integration results."),
                    paste0("initial_handoff.integration_evidence.common.", common_field))
          }
        }
      }
      invalid(paste0("Saved integration evidence field '", field,
                     "' differs from its independently bound C producer."),
              paste0("initial_handoff.integration_evidence.", field))
    }
  }

  controls_result <- result
  controls_result$params$provenance$unit_map <- unit_ids
  controls_result$params$provenance$feature_map <- stats::setNames(lapply(block_ids, function(block_id) {
    as.character(unlist(feature_map[[block_id]], use.names = FALSE))
  }), block_ids)
  run_for_controls <- list(status = "complete", method = result$backend, result = controls_result,
                           eligibility = eligibility)
  unrun_controls <- triomics_c06_cli_unrun_controls(
    list(run_for_controls), list(), list(), list(result_item)
  )
  evaluation_expected <- triomics_c06_evaluation_evidence(list(controls = unrun_controls))
  primary_expected <- triomics_c06_primary_branch(NULL, identity$study_id, identity$analysis_id)
  integration_nominations <- triomics_c06_integration_nominations(list(evidence_expected))
  integration_branch <- list(
    status = if (length(integration_nominations)) "complete" else "unavailable",
    source_artifact_ids = as.list(integration_id),
    nominations = integration_nominations
  )
  all_nominations <- c(primary_expected$nominations, integration_nominations)
  lineage_run <- list(status = "complete", native_artifact_ref = native_ref)
  handoff_expected <- list(
    schema_version = "2.0", artifact_type = "stage_c_d_handoff",
    status = if (identical(triomics_c06_stage_status(list(run_for_controls), list(evidence_expected)),
                           "complete") && !identical(evaluation_expected$status, "complete")) {
      "partial"
    } else triomics_c06_stage_status(list(run_for_controls), list(evidence_expected)),
    study_id = as.character(identity$study_id), analysis_id = as.character(identity$analysis_id),
    preparation_context_id = as.character(identity$preparation_context_id),
    representation_id = as.character(identity$representation_id),
    nomination_status = if (length(all_nominations)) "complete" else "unavailable",
    nomination_branches = list(primary_statistics = primary_expected,
                               integration_selection = integration_branch),
    nominations = all_nominations,
    integration_evidence = list(evidence_expected),
    lineage_inputs = triomics_c06_handoff_lineage(primary_expected, list(lineage_run),
                                                 evaluation_expected),
    comparisons = list(status = "not_comparable",
                       reason = "fewer_than_two_completed_integrations", pairs = list()),
    evaluation_evidence = evaluation_expected,
    backend_statuses = triomics_c06_backend_statuses(list(), list(run_for_controls)),
    clinical_prediction = list(
      status = "not_started", completion_policy = "conditional", packet_id = "C-09",
      model_execution = "not_started",
      reason = "Conditional matched clinical prediction has not been executed."
    )
  )
  if (!triomics_cnh_same_json(handoff_expected, handoff)) {
    invalid("Saved nominations, comparison status, lineage, C-07 evidence, or backend dispositions differ from C producer facts.",
            "initial_handoff")
  }
  invisible(TRUE)
}

triomics_cnh_validate_initial_handoff_contract <- function(handoff) {
  expected_clinical_prediction <- list(
    status = "not_started", completion_policy = "conditional", packet_id = "C-09",
    model_execution = "not_started",
    reason = "Conditional matched clinical prediction has not been executed."
  )
  evaluation <- handoff$evaluation_evidence
  control_names <- triomics_c06_control_names()
  controls <- evaluation$controls
  valid_controls <- is.list(controls) && length(controls) == length(control_names) &&
    all(vapply(seq_along(control_names), function(index) {
      control <- controls[[index]]
      if (!is.list(control) || !identical(control$control, control_names[[index]]) ||
          !identical(control$status, "not_run") || !identical(control$matched, TRUE)) {
        return(FALSE)
      }
      all(vapply(c("candidate", "baseline"), function(side) {
        observation <- control[[side]]
        is.list(observation) && identical(observation$status, "not_run") &&
          triomics_nonempty_string(as.character(observation$reason %||% "")) &&
          is.null(observation$artifact_ref) && is.null(observation$artifact_sha256) &&
          is.null(observation$metrics) && is.null(observation$evaluation_result) &&
          is.list(observation$match_context)
      }, logical(1L)))
    }, logical(1L)))
  if (!is.list(handoff$clinical_prediction) ||
      !triomics_cnh_same_json(handoff$clinical_prediction, expected_clinical_prediction) ||
      !is.list(evaluation) || !identical(evaluation$schema_version, "2.0") ||
      !identical(evaluation$artifact_type, "integration_evaluation_evidence") ||
      !identical(evaluation$status, "partial") || !is.list(evaluation$source_artifacts) ||
      length(evaluation$source_artifacts) != 0L || !isTRUE(valid_controls)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID",
      "The saved initial context changes the conditional C-09 or unrun C-07 evidence contract.",
      "initial_handoff", stage = "C"
    )
  }
  invisible(TRUE)
}

triomics_cnh_validate_saved_c_outputs <- function(root, config, c_input_identity,
                                                   source_inputs, integration_runs,
                                                   initial_handoff) {
  config_sha256 <- as.character(attr(config, "config_sha256"))
  identity <- c_input_identity[c(
    "study_id", "analysis_id", "preparation_context_id", "representation_id"
  )]
  if (any(!vapply(identity, triomics_nonempty_string, logical(1L))) ||
      !triomics_sha256_text(config_sha256) ||
      !triomics_cnh_same_json(c_input_identity$source_inputs, source_inputs)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The C input identity or source manifests are incomplete.",
      "c_input_identity", stage = "C"
    )
  }
  c_payload_path <- file.path(root, "C", "integration-results.json")
  c_envelope_path <- file.path(root, "C", "integration-results.envelope.json")
  c_payload <- triomics_cnh_read_json(c_payload_path, "C integration results")
  c_envelope <- triomics_cnh_validate_envelope(c_envelope_path, c(
    list(artifact_type = "integration_results", artifact_id = "stage-c-integration-results",
         study_id = identity$study_id, config_sha256 = config_sha256,
         analysis_id = identity$analysis_id,
         preparation_context_id = identity$preparation_context_id,
         representation_id = identity$representation_id,
         status = "complete", payload_sha256 = triomics_sha256(c_payload_path)),
    list(inputs = c(source_inputs, lapply(integration_runs, `[[`, "native_artifact_ref")))
  ), "C integration results")
  integration <- if (is.list(c_payload$integrations) && length(c_payload$integrations) == 1L) {
    c_payload$integrations[[1L]]
  } else NULL
  triomics_cnh_validate_saved_handoff_producer(
    root, c_input_identity, source_inputs, initial_handoff, config_sha256
  )
  run <- if (is.list(integration_runs) && length(integration_runs) == 1L) {
    integration_runs[[1L]]
  } else NULL
  if (!triomics_cnh_same_json(c_payload[names(identity)], identity) ||
      !identical(c_payload$status, "complete") || !is.list(integration) ||
      !identical(integration$status, "complete") || !is.list(run) ||
      !identical(run$status, "complete") || !inherits(run$result, "IntegrationResult") ||
      !identical(as.character(integration$integration_id), as.character(run$integration_id)) ||
      !identical(as.character(integration$method), as.character(run$method)) ||
      !triomics_cnh_same_json(initial_handoff$integration_evidence[[1L]]$native_artifact_ref,
                              run$native_artifact_ref)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID",
      "The C result, in-memory fit, native sidecar reference, and saved handoff differ.",
      "C/integration-results.json", stage = "C"
    )
  }
  native_path <- file.path(root, "C", "native-states", "integration-01.json")
  native_envelope_path <- file.path(root, "C", "native-states", "integration-01.envelope.json")
  native <- triomics_cnh_read_json(native_path, "C native state")
  matrix_inputs <- lapply(names(c_input_identity$blocks), function(block_id) {
    list(
      artifact_id = paste(run$integration_id, block_id, sep = "::"),
      sha256 = as.character(c_input_identity$blocks[[block_id]]$matrix_sha256)
    )
  })
  preparation_inputs <- lapply(names(native$preparation_state_refs), function(block_id) {
    reference <- native$preparation_state_refs[[block_id]]
    if (!is.list(reference) || !triomics_nonempty_string(as.character(reference$path %||% "")) ||
        !triomics_sha256_text(as.character(reference$payload_sha256 %||% ""))) {
      triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "A C preparation-state reference is incomplete.",
                         "preparation_state_refs", stage = "C")
    }
    state_path <- triomics_cnh_path(root, as.character(reference$path),
                                    as.character(reference$payload_sha256),
                                    paste0("C preparation state ", block_id))
    list(artifact_id = paste("preparation-state", block_id, sep = "::"),
         sha256 = triomics_sha256(state_path))
  })
  triomics_cnh_validate_envelope(native_envelope_path, c(
    list(artifact_type = "integration_native_state",
         artifact_id = as.character(run$native_artifact_ref$artifact_id),
         study_id = identity$study_id, config_sha256 = config_sha256,
         analysis_id = identity$analysis_id,
         preparation_context_id = identity$preparation_context_id,
         representation_id = identity$representation_id,
         status = "complete", payload_sha256 = triomics_sha256(native_path)),
    list(inputs = c(source_inputs, matrix_inputs, preparation_inputs))
  ), "C native state")
  if (!identical(as.character(native$artifact_id), as.character(run$native_artifact_ref$artifact_id)) ||
      !identical(triomics_sha256(native_path), as.character(run$native_artifact_ref$sha256))) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "The C native state reference differs from its file.",
                       "C/native-states/integration-01.json", stage = "C")
  }
  endpoint <- integration$score_endpoint_bundle
  request <- integration$clinical_score_request
  if (!is.list(endpoint) || !is.list(request) ||
      !identical(endpoint$payload_file, file.path("C", "score-endpoints", "integration-01.json")) ||
      !identical(endpoint$envelope_file, file.path("C", "score-endpoints", "integration-01.envelope.json")) ||
      !identical(request$payload_file, file.path("B", "clinical-score-association-requests", "request-01.json")) ||
      !identical(request$envelope_file, file.path("B", "clinical-score-association-requests", "request-01.envelope.json")) ||
      !identical(request$status, "scheduled") || !identical(request$owner_stage, "B") ||
      !identical(request$model_execution, "not_started")) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The C score endpoint or B request reference is unsupported.",
      "clinical_score_request", stage = "C"
    )
  }
  endpoint_path <- file.path(root, endpoint$payload_file)
  endpoint_envelope_path <- file.path(root, endpoint$envelope_file)
  request_path <- file.path(root, request$payload_file)
  request_envelope_path <- file.path(root, request$envelope_file)
  for (path in c(endpoint_path, endpoint_envelope_path, request_path, request_envelope_path)) {
    triomics_cnh_path(root, substring(path, nchar(root) + 2L), triomics_sha256(path), basename(path))
  }
  if (!isTRUE(triomics_validate_envelope(endpoint_envelope_path)) ||
      !isTRUE(triomics_validate_envelope(request_envelope_path)) ||
      !identical(triomics_sha256(endpoint_path), as.character(endpoint$payload_sha256)) ||
      !identical(triomics_sha256(endpoint_envelope_path), as.character(endpoint$envelope_sha256)) ||
      !identical(triomics_sha256(request_path), as.character(request$payload_sha256)) ||
      !identical(triomics_sha256(request_envelope_path), as.character(request$envelope_sha256))) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The endpoint or request artifact bytes failed validation.",
      "clinical_score_request", stage = "C"
    )
  }
  invisible(c_envelope)
}

triomics_cnh_save_initial_c <- function(integration_runs, evaluation_controls,
                                        c_input_identity, config, output,
                                        source_inputs, run_status) {
  route <- triomics_cnh_native_c_route(config)
  if (is.null(route)) return(NULL)
  root <- tryCatch(triomics_b07_output_root(output, create = FALSE), error = function(error) NULL)
  if (is.null(root)) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "The C output root must already exist.",
                       "output", stage = "C")
  }
  if (!identical(run_status, "complete") || !is.list(integration_runs) ||
      length(integration_runs) != 1L || !is.list(evaluation_controls)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The initial C context requires one complete integration and its evaluation controls.",
      "integration_runs", stage = "C"
    )
  }
  context_paths <- triomics_cnh_context_paths(root)
  registry_path <- file.path(root, "registries", "run-C.json")
  if (any(file.exists(unlist(context_paths, use.names = FALSE))) ||
      any(vapply(unlist(context_paths, use.names = FALSE), triomics_is_symlink, logical(1L))) ||
      file.exists(registry_path) || triomics_is_symlink(registry_path)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_CONFLICT",
      "Initial C publication context must be saved once, before the C completion registry.",
      "C/native-publication-context.json", stage = "C"
    )
  }
  triomics_cnh_reject_prior_b(root)
  d_root <- file.path(root, "D")
  if (file.exists(d_root) || triomics_is_symlink(d_root)) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_CONFLICT", "A D publication already exists for this C run.",
                       "D", stage = "C")
  }
  if (!triomics_cnh_same_json(c_input_identity$source_inputs, source_inputs) ||
      !identical(as.character(c_input_identity$study_id), as.character(config$study_id)) ||
      !identical(as.character(c_input_identity$analysis_id), as.character(route$analysis$analysis_id))) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The C input identity differs from the configured analysis or sources.",
      "c_input_identity", stage = "C"
    )
  }
  handoff <- triomics_c06_build_handoff(
    primary_nominations = NULL,
    integration_runs = integration_runs,
    evaluation_evidence = list(controls = evaluation_controls),
    clinical_prediction = list(
      status = "not_started", completion_policy = "conditional", packet_id = "C-09",
      model_execution = "not_started",
      reason = "Conditional matched clinical prediction has not been executed."
    )
  )
  handoff$study_id <- as.character(c_input_identity$study_id)
  handoff$analysis_id <- as.character(c_input_identity$analysis_id)
  if (identical(handoff$status, "complete") &&
      !identical(handoff$evaluation_evidence$status, "complete")) {
    handoff$status <- "partial"
  }
  if (!triomics_cnh_same_json(handoff$integration_evidence[[1L]]$context, list(
    study_id = c_input_identity$study_id, analysis_id = c_input_identity$analysis_id,
    preparation_context_id = c_input_identity$preparation_context_id,
    representation_id = c_input_identity$representation_id
  ))) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "The C integration handoff identity is inconsistent.",
                       "handoff", stage = "C")
  }
  file_references <- triomics_cnh_c_file_references(root)
  declared_source_files <- triomics_cnh_declared_source_files(config)
  triomics_cnh_validate_saved_c_outputs(
    root, config, c_input_identity, source_inputs, integration_runs, handoff
  )
  payload <- list(
    schema_version = "1.0",
    artifact_type = "stage_c_native_publication_context",
    study_id = as.character(c_input_identity$study_id),
    analysis_id = as.character(c_input_identity$analysis_id),
    preparation_context_id = as.character(c_input_identity$preparation_context_id),
    representation_id = as.character(c_input_identity$representation_id),
    config_sha256 = as.character(attr(config, "config_sha256")),
    run_status = run_status,
    c_input_identity_sha256 = triomics_b07_prov_value_sha256(c_input_identity),
    source_inputs = source_inputs,
    declared_source_files = declared_source_files,
    input_files = file_references,
    c_input_identity = triomics_cnh_encode_r_value(c_input_identity),
    initial_handoff = triomics_cnh_encode_r_value(handoff)
  )
  envelope_inputs <- triomics_cnh_context_envelope_inputs(
    source_inputs, file_references, declared_source_files
  )
  payload_path <- context_paths$payload
  envelope_path <- context_paths$envelope
  written <- character()
  finalized <- FALSE
  on.exit({
    if (!finalized && length(written)) unlink(written, force = TRUE)
  }, add = TRUE)
  triomics_write_json_atomic(payload, payload_path)
  written <- c(written, payload_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "stage_c_native_publication_context",
    artifact_id = "c06-native-publication-context",
    study_id = payload$study_id,
    run_id = "c-01",
    status = "complete",
    config_sha256 = payload$config_sha256,
    payload = payload_path,
    producer_commit = triomics_b07_git_ref(getwd()),
    inputs = envelope_inputs,
    reason_codes = character(),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  envelope$analysis_id <- payload$analysis_id
  envelope$preparation_context_id <- payload$preparation_context_id
  envelope$representation_id <- payload$representation_id
  triomics_write_envelope(envelope, envelope_path)
  written <- c(written, envelope_path)
  readback <- triomics_cnh_read_json(payload_path, "C publication context")
  triomics_cnh_validate_envelope(envelope_path, c(
    list(artifact_type = "stage_c_native_publication_context",
         artifact_id = "c06-native-publication-context", study_id = payload$study_id,
         config_sha256 = payload$config_sha256, status = "complete",
         payload_sha256 = triomics_sha256(payload_path), inputs = envelope_inputs,
         analysis_id = payload$analysis_id,
         preparation_context_id = payload$preparation_context_id,
         representation_id = payload$representation_id)
  ), "C publication context")
  if (!triomics_cnh_same_json(readback, payload) ||
      !identical(triomics_sha256(payload_path), as.character(envelope$payload_sha256))) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "The initial C context failed exact JSON read-back.",
                       "C/native-publication-context.json", stage = "C")
  }
  finalized <- TRUE
  invisible(list(
    status = "saved", payload_path = payload_path, envelope_path = envelope_path,
    payload_sha256 = triomics_sha256(payload_path), envelope_sha256 = triomics_sha256(envelope_path)
  ))
}

triomics_cnh_has_saved_c_state <- function(root) {
  files <- c(
    "registries/run-C.json", "registries/run-B.json",
    "C/integration-results.json", "C/integration-results.envelope.json",
    "C/native-publication-context.json", "C/native-publication-context.envelope.json",
    "B/block-results.json", "B/block-results.envelope.json",
    "B/clinical-associations.json", "B/clinical-associations.envelope.json",
    "B/clinical-longitudinal-associations.json",
    "B/clinical-longitudinal-associations.envelope.json"
  )
  for (relative in files) {
    path <- tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
    if (is.null(path) || file.exists(path) || triomics_is_symlink(path)) return(TRUE)
  }
  for (relative in c("C/native-states", "C/score-endpoints", "C/preparation-states",
                     "B/clinical-score-association-requests", "B/handoffs", "D")) {
    path <- tryCatch(triomics_b07_child_path(root, relative), error = function(error) NULL)
    if (is.null(path) || triomics_is_symlink(path)) return(TRUE)
    if (dir.exists(path) && length(list.files(path, recursive = TRUE, all.files = TRUE,
                                              no.. = TRUE))) return(TRUE)
  }
  FALSE
}

triomics_cnh_read_saved_c_context <- function(config, root) {
  context_paths <- triomics_cnh_context_paths(root)
  existing <- file.exists(unlist(context_paths, use.names = FALSE)) |
    vapply(unlist(context_paths, use.names = FALSE), triomics_is_symlink, logical(1L))
  if (!all(existing)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The saved C publication context payload and envelope must exist together.",
      "C/native-publication-context.json", stage = "C"
    )
  }
  context_path <- triomics_cnh_path(
    root, file.path("C", "native-publication-context.json"),
    triomics_sha256(context_paths$payload), "C publication context"
  )
  envelope_path <- triomics_cnh_path(
    root, file.path("C", "native-publication-context.envelope.json"),
    triomics_sha256(context_paths$envelope), "C publication context envelope"
  )
  payload <- triomics_cnh_read_json(context_path, "C publication context")
  config_sha256 <- as.character(attr(config, "config_sha256"))
  file_references <- triomics_cnh_c_file_references(root)
  declared_source_files <- triomics_cnh_declared_source_files(config)
  if (!triomics_cnh_same_json(payload$input_files, file_references) ||
      !triomics_cnh_same_json(payload$declared_source_files, declared_source_files) ||
      !identical(payload$schema_version, "1.0") ||
      !identical(payload$artifact_type, "stage_c_native_publication_context") ||
      !identical(payload$run_status, "complete") ||
      !identical(as.character(payload$config_sha256), config_sha256) ||
      !identical(as.character(payload$study_id), as.character(config$study_id)) ||
      !triomics_sha256_text(config_sha256)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID",
      "The saved C context no longer matches the current configuration or exact input-file bytes.",
      "C/native-publication-context.json", stage = "C"
    )
  }
  source_inputs <- payload$source_inputs
  if (!is.list(source_inputs) || !length(source_inputs) || any(!vapply(source_inputs, function(input) {
    is.list(input) && triomics_nonempty_string(as.character(input$artifact_id %||% "")) &&
      triomics_sha256_text(as.character(input$sha256 %||% ""))
  }, logical(1L)))) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "Saved C source manifest hashes are incomplete.",
                       "source_inputs", stage = "C")
  }
  expected_inputs <- triomics_cnh_context_envelope_inputs(
    source_inputs, file_references, declared_source_files
  )
  privacy_class <- if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  envelope <- triomics_cnh_validate_envelope(envelope_path, c(
    list(artifact_type = "stage_c_native_publication_context",
         artifact_id = "c06-native-publication-context",
         study_id = as.character(payload$study_id), config_sha256 = config_sha256,
         status = "complete", payload_sha256 = triomics_sha256(context_path),
         inputs = expected_inputs, privacy_class = privacy_class,
         analysis_id = as.character(payload$analysis_id),
         preparation_context_id = as.character(payload$preparation_context_id),
         representation_id = as.character(payload$representation_id))
  ), "C publication context")
  identity <- triomics_cnh_decode_r_value(payload$c_input_identity)
  handoff <- triomics_cnh_decode_r_value(payload$initial_handoff)
  identity_fields <- list(
    study_id = payload$study_id, analysis_id = payload$analysis_id,
    preparation_context_id = payload$preparation_context_id,
    representation_id = payload$representation_id
  )
  if (!is.list(identity) || !is.list(handoff) ||
      !identical(as.character(triomics_b07_prov_value_sha256(identity)),
                 as.character(payload$c_input_identity_sha256)) ||
      !triomics_cnh_same_json(identity[c("study_id", "analysis_id", "preparation_context_id",
                                         "representation_id")], identity_fields) ||
      !identical(handoff$artifact_type, "stage_c_d_handoff") ||
      !identical(as.character(handoff$study_id), as.character(payload$study_id)) ||
      !identical(as.character(handoff$analysis_id), as.character(payload$analysis_id)) ||
      !is.list(handoff$nomination_branches$primary_statistics) ||
      !identical(handoff$nomination_branches$primary_statistics$status, "not_requested") ||
      length(handoff$nomination_branches$primary_statistics$nominations) != 0L) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The typed C identity or saved pre-B handoff failed validation.",
      "C/native-publication-context.json", stage = "C"
    )
  }
  triomics_cnh_validate_initial_handoff_contract(handoff)
  triomics_cnh_validate_saved_handoff_producer(
    root, identity, source_inputs, handoff, config_sha256
  )
  registry_relative <- file.path("registries", "run-C.json")
  registry_candidate <- tryCatch(
    triomics_b07_child_path(root, registry_relative), error = function(error) NULL
  )
  if (is.null(registry_candidate) || !file.exists(registry_candidate) ||
      dir.exists(registry_candidate) || triomics_is_symlink(registry_candidate)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The C completion registry is missing or unsafe.",
      registry_relative, stage = "C"
    )
  }
  registry_path <- triomics_cnh_path(
    root, registry_relative, triomics_sha256(registry_candidate), "C completion registry"
  )
  registry <- triomics_cnh_read_json(registry_path, "C completion registry")
  if (!identical(registry$stage, "C") || !identical(registry$run_id, "c-01") ||
      !identical(registry$status, payload$run_status) ||
      !identical(registry$study_id, payload$study_id)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID",
      "The C completion registry is missing or disagrees with the saved initial publication.",
      "registries/run-C.json", stage = "C"
    )
  }
  list(
    payload = payload, envelope = envelope, identity = identity,
    handoff = handoff, registry = registry, file_references = file_references
  )
}

triomics_cnh_publish_saved_c <- function(config_path, output) {
  if (!triomics_nonempty_string(config_path) || !triomics_nonempty_string(output)) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "Config and output paths are required.",
                       "output", stage = "C")
  }
  config <- triomics_read_config(config_path)
  root <- tryCatch(triomics_b07_output_root(output, create = FALSE), error = function(error) NULL)
  if (is.null(root)) return(NULL)
  context_paths <- triomics_cnh_context_paths(root)
  context_present <- any(file.exists(unlist(context_paths, use.names = FALSE))) ||
    any(vapply(unlist(context_paths, use.names = FALSE), triomics_is_symlink, logical(1L)))
  route <- triomics_cnh_native_c_route(config)
  if (is.null(route)) {
    if (context_present) {
      triomics_cnh_abort(
        "C_PUBLICATION_CONTEXT_INVALID", "The saved native C context no longer matches an applicable route.",
        "clinical.objectives", stage = "C"
      )
    }
    return(NULL)
  }
  if (!context_present) {
    if (!triomics_cnh_has_saved_c_state(root)) return(NULL)
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_MISSING",
      "This output contains prior C/B publication state but no initial native C context; it cannot be refit or resumed.",
      "C/native-publication-context.json", stage = "C"
    )
  }
  saved <- triomics_cnh_read_saved_c_context(config, root)
  identity <- saved$identity
  if (!identical(as.character(identity$analysis_id), as.character(route$analysis$analysis_id)) ||
      !identical(as.character(identity$preparation_context_id),
                 as.character(route$analysis$preparation_context_id)) ||
      !identical(as.character(identity$representation_id),
                 as.character(route$analysis$design$representation))) {
    triomics_cnh_abort("C_PUBLICATION_CONTEXT_INVALID", "The saved C identity differs from the configured analysis.",
                       "c_input_identity", stage = "C")
  }
  fresh_b_context <- triomics_cnh_load_stage_b_context(config_path, output, allow_b_started = TRUE)
  if (!inherits(fresh_b_context, "ClinicalNativeHandoffContext") ||
      !identical(fresh_b_context$config_sha256, saved$payload$config_sha256) ||
      !identical(fresh_b_context$study_id, saved$payload$study_id) ||
      !identical(fresh_b_context$analysis_id, saved$payload$analysis_id) ||
      !triomics_cnh_same_json(fresh_b_context$source_inputs, saved$payload$source_inputs)) {
    triomics_cnh_abort(
      "C_PUBLICATION_CONTEXT_INVALID", "The current B request does not match the saved typed C context.",
      "C/native-publication-context.json", stage = "C"
    )
  }
  primary_nominations <- triomics_c06_cli_primary_nominations(output, config, route$analysis)
  handoff <- triomics_c06_attach_primary_nominations(saved$handoff, primary_nominations)
  if (identical(handoff$status, "complete") &&
      !identical(handoff$evaluation_evidence$status, "complete")) {
    handoff$status <- "partial"
  }
  predecessor_paths <- triomics_c06_cli_native_b_predecessors(output)
  publication <- triomics_c06_write_handoff(
    handoff, output,
    config_sha256 = as.character(saved$payload$config_sha256),
    producer_commit = triomics_b07_git_ref(getwd()),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive",
    b_predecessor_paths = predecessor_paths,
    c_input_identity = identity
  )
  list(
    status = handoff$status,
    run_status = as.character(saved$registry$status),
    payload_path = file.path(root, "C", "integration-results.json"),
    results = saved$handoff$integration_evidence,
    required_complete = identical(saved$registry$status, "complete"),
    publication = publication
  )
}
