triomics_report_clinical_abort <- function(message, key) {
  triomics_report_abort("D_REPORT_SOURCE_INVALID", message, key)
}

triomics_report_clinical_records <- function(value, key) {
  if (!is.list(value) || (length(value) > 0L && any(!vapply(value, function(record) {
    is.list(record) && !is.null(names(record))
  }, logical(1L))))) {
    triomics_report_clinical_abort("Clinical evidence is not a typed record array.", key)
  }
  value
}

triomics_report_clinical_count <- function(record, key) {
  value <- triomics_report_number(record, key)
  if (!is.finite(value) || value < 0 || value != floor(value) || value > .Machine$integer.max) {
    triomics_report_clinical_abort("A clinical coverage count is not a nonnegative integer.", key)
  }
  as.integer(value)
}

triomics_report_clinical_optional_count <- function(record, key) {
  if (is.null(record[[key]])) return(NA_integer_)
  triomics_report_clinical_count(record, key)
}

triomics_report_clinical_boolean <- function(record, key) {
  value <- if (is.list(record)) record[[key]] else NULL
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    triomics_report_clinical_abort("A clinical evidence flag is not a scalar boolean.", key)
  }
  value
}

triomics_report_clinical_scalar <- function(record, key, default = "") {
  triomics_report_identifier(record, key, default)
}

triomics_report_clinical_array_count <- function(value, key) {
  if (!(is.list(value) || is.character(value)) || any(vapply(seq_along(value), function(index) {
    !is.character(value[[index]]) || length(value[[index]]) != 1L || is.na(value[[index]])
  }, logical(1L)))) {
    triomics_report_clinical_abort("A clinical lineage array is not a string record array.", key)
  }
  as.integer(length(value))
}

triomics_report_clinical_empty_frame <- function(columns, types = list()) {
  values <- setNames(lapply(columns, function(name) {
    if (name %in% names(types)) return(types[[name]])
    character()
  }), columns)
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_clinical_dictionary_rows <- function(coverage) {
  variables <- triomics_report_clinical_records(coverage$variables, "clinical_dictionary_coverage.variables")
  columns <- c("variable_id", "declared_type", "unit", "units_verification", "role",
               "time_semantics", "availability", "verification", "modelable",
               "missingness_rule_status", "mapped_dispositions",
               "unsupported_disposition_count", "censoring", "imputation")
  records <- lapply(seq_along(variables), function(index) {
    variable <- variables[[index]]
    rules <- variable$missingness_rules
    if (is.null(rules)) {
      rules_status <- "not_reported"
      dispositions <- ""
      unsupported_count <- NA_integer_
      censoring <- "not_reported"
      imputation <- "not_reported"
    } else {
      if (!is.list(rules) || is.null(names(rules))) {
        triomics_report_clinical_abort(
          "A clinical dictionary missingness record is malformed.",
          paste0("clinical_dictionary_coverage.variables[", index, "].missingness_rules")
        )
      }
      rules_status <- "source_reported"
      dispositions <- triomics_report_identifier_collection(
        rules$mapped_dispositions, paste0("clinical_dictionary_coverage.variables[", index, "].mapped_dispositions")
      )
      unsupported_count <- triomics_report_clinical_optional_count(
        rules, "unsupported_disposition_count"
      )
      censoring <- triomics_report_clinical_scalar(rules, "censoring", "not_reported")
      imputation <- triomics_report_clinical_scalar(rules, "imputation", "not_reported")
    }
    data.frame(
      variable_id = triomics_report_clinical_scalar(variable, "variable_id"),
      declared_type = triomics_report_clinical_scalar(variable, "declared_type", "not_reported"),
      unit = triomics_report_unit(variable$unit, paste0("clinical_dictionary_coverage.variables[", index, "].unit")),
      units_verification = triomics_report_clinical_scalar(variable, "units_verification", "not_reported"),
      role = triomics_report_clinical_scalar(variable, "role", "not_reported"),
      time_semantics = triomics_report_clinical_scalar(variable, "time_semantics", "not_reported"),
      availability = triomics_report_clinical_scalar(variable, "availability", "not_reported"),
      verification = triomics_report_clinical_scalar(variable, "verification", "not_reported"),
      modelable = triomics_report_clinical_boolean(variable, "modelable"),
      missingness_rule_status = rules_status,
      mapped_dispositions = dispositions,
      unsupported_disposition_count = unsupported_count,
      censoring = censoring,
      imputation = imputation,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  if (!length(records)) {
    return(triomics_report_clinical_empty_frame(
      columns, list(modelable = logical(), unsupported_disposition_count = integer())
    ))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_visit <- function(value, key) {
  visit <- triomics_report_identifier_value(value, key, "not_reported")
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}(?:[T ][0-9:.-]+)?$", visit, perl = TRUE)) {
    return("visit timing withheld")
  }
  visit
}

triomics_report_clinical_coverage_rows <- function(coverage, safe_variable_ids) {
  rows <- triomics_report_clinical_records(coverage$coverage, "clinical_dictionary_coverage.coverage")
  columns <- c("variable_id", "visit_id", "scope", "availability", "n_subjects",
               "n_observed", "n_missing", "n_placeholder", "n_censored", "n_multiselect")
  records <- lapply(seq_along(rows), function(index) {
    row <- rows[[index]]
    variable_id <- triomics_report_clinical_scalar(row, "variable_id")
    if (!variable_id %in% safe_variable_ids) {
      triomics_report_clinical_abort(
        "Clinical coverage refers to a variable absent from the safe dictionary projection.",
        paste0("clinical_dictionary_coverage.coverage[", index, "].variable_id")
      )
    }
    data.frame(
      variable_id = variable_id,
      visit_id = triomics_report_clinical_visit(row$visit_id, "clinical_dictionary_coverage.coverage.visit_id"),
      scope = triomics_report_clinical_scalar(row, "scope", "not_reported"),
      availability = triomics_report_clinical_scalar(row, "availability", "not_reported"),
      n_subjects = triomics_report_clinical_count(row, "n_subjects"),
      n_observed = triomics_report_clinical_count(row, "n_observed"),
      n_missing = triomics_report_clinical_count(row, "n_missing"),
      n_placeholder = triomics_report_clinical_count(row, "n_placeholder"),
      n_censored = triomics_report_clinical_count(row, "n_censored"),
      n_multiselect = triomics_report_clinical_count(row, "n_multiselect"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  if (!length(records)) {
    return(triomics_report_clinical_empty_frame(
      columns, list(n_subjects = integer(), n_observed = integer(), n_missing = integer(),
                    n_placeholder = integer(), n_censored = integer(), n_multiselect = integer())
    ))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_exclusion_rows <- function(coverage) {
  excluded <- coverage$excluded
  groups <- c("identifier", "free_text", "sensitive")
  columns <- c("excluded_class", "n_variables", "n_subjects", "n_observed",
               "n_missing", "n_placeholder", "n_censored", "n_multiselect")
  if (!is.list(excluded) || is.null(names(excluded))) {
    return(triomics_report_clinical_empty_frame(
      columns, list(n_variables = integer(), n_subjects = integer(), n_observed = integer(),
                    n_missing = integer(), n_placeholder = integer(), n_censored = integer(),
                    n_multiselect = integer())
    ))
  }
  records <- lapply(groups[groups %in% names(excluded)], function(group) {
    value <- excluded[[group]]
    counts <- value$coverage_counts
    if (!is.list(value) || !is.list(counts)) {
      triomics_report_clinical_abort("Clinical exclusion counts are malformed.",
                                     paste0("clinical_dictionary_coverage.excluded.", group))
    }
    data.frame(
      excluded_class = group,
      n_variables = triomics_report_clinical_count(value, "n_variables"),
      n_subjects = triomics_report_clinical_count(counts, "n_subjects"),
      n_observed = triomics_report_clinical_count(counts, "n_observed"),
      n_missing = triomics_report_clinical_count(counts, "n_missing"),
      n_placeholder = triomics_report_clinical_count(counts, "n_placeholder"),
      n_censored = triomics_report_clinical_count(counts, "n_censored"),
      n_multiselect = triomics_report_clinical_count(counts, "n_multiselect"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  if (!length(records)) {
    return(triomics_report_clinical_empty_frame(
      columns, list(n_variables = integer(), n_subjects = integer(), n_observed = integer(),
                    n_missing = integer(), n_placeholder = integer(), n_censored = integer(),
                    n_multiselect = integer())
    ))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_saved_endpoints <- function(loaded, config) {
  integrations <- loaded$payloads$c$integrations %||% list()
  integrations <- triomics_report_clinical_records(integrations, "C.integrations")
  endpoints <- list()
  for (integration_index in seq_along(integrations)) {
    integration <- integrations[[integration_index]]
    reference <- integration$score_endpoint_bundle
    if (is.null(reference)) next
    required <- c("artifact_id", "payload_file", "envelope_file",
                  "payload_sha256", "envelope_sha256")
    if (!is.list(reference) || is.null(names(reference)) || !all(required %in% names(reference))) {
      triomics_report_clinical_abort("The C score endpoint reference is malformed.",
                                     paste0("C.integrations[", integration_index, "].score_endpoint_bundle"))
    }
    expected_envelope <- sub("[.]json$", ".envelope.json", as.character(reference$payload_file))
    if (!identical(as.character(reference$payload_file),
                   file.path("C", "score-endpoints", basename(as.character(reference$payload_file))) ) ||
        !identical(as.character(reference$envelope_file), expected_envelope) ||
        !triomics_sha256_text(as.character(reference$payload_sha256)) ||
        !triomics_sha256_text(as.character(reference$envelope_sha256))) {
      triomics_report_clinical_abort("The C score endpoint reference has an unsupported path or hash.",
                                     paste0("C.integrations[", integration_index, "].score_endpoint_bundle"))
    }
    artifact <- triomics_report_artifact(
      loaded$root, as.character(reference$payload_file),
      "integration_score_endpoint_bundle", as.character(reference$artifact_id),
      config$study_id, config$config_sha256, config$analysis_id, parse_payload = TRUE
    )
    if (!identical(artifact$reference$payload_sha256, as.character(reference$payload_sha256)) ||
        !identical(artifact$reference$envelope_sha256, as.character(reference$envelope_sha256)) ||
        !identical(artifact$reference$envelope_path, as.character(reference$envelope_file))) {
      triomics_report_clinical_abort(
        "The saved C score endpoint bytes differ from the verified integration result.",
        paste0("C.integrations[", integration_index, "].score_endpoint_bundle")
      )
    }
    bundle <- artifact$payload
    if (!identical(as.character(bundle$artifact_type), "integration_score_endpoint_bundle") ||
        !identical(as.character(bundle$integration_id), triomics_report_identifier(integration, "integration_id")) ||
        !identical(as.character(bundle$study_id), config$study_id) ||
        !identical(as.character(bundle$analysis_id), config$analysis_id) ||
        !triomics_sha256_text(as.character(bundle$unit_map_hash))) {
      triomics_report_clinical_abort("The saved C score endpoint bundle identity is inconsistent.",
                                     paste0("C.integrations[", integration_index, "].score_endpoint_bundle"))
    }
    bundle_endpoints <- triomics_report_clinical_records(bundle$endpoints, "C.score_endpoint_bundle.endpoints")
    for (endpoint_index in seq_along(bundle_endpoints)) {
      endpoint <- bundle_endpoints[[endpoint_index]]
      endpoint_id <- triomics_report_identifier(endpoint, "endpoint_id")
      component_id <- triomics_report_identifier(endpoint, "component_id")
      component_key <- names(bundle$endpoints)[[endpoint_index]]
      if (!nzchar(endpoint_id) || !identical(as.character(endpoint$artifact_type), "integration_score_endpoint") ||
          !identical(as.character(endpoint$study_id), config$study_id) ||
          !identical(as.character(endpoint$analysis_id), config$analysis_id) ||
          !identical(as.character(endpoint$integration_id), as.character(bundle$integration_id)) ||
          !nzchar(component_id) || !identical(component_key, component_id) ||
          !triomics_sha256_text(as.character(endpoint$source_hash)) ||
          !triomics_sha256_text(as.character(endpoint$endpoint_sha256))) {
        triomics_report_clinical_abort("The saved C score endpoint bundle identity is inconsistent.",
                                       paste0("C.score_endpoint_bundle.endpoints.", endpoint_index))
      }
      if (endpoint_id %in% names(endpoints)) {
        triomics_report_clinical_abort("A saved endpoint identity is duplicated.",
                                       "C.score_endpoint_bundle.endpoints")
      }
      fit <- endpoint$producing_fit
      selection <- endpoint$selection
      training_y <- endpoint$training_y
      if (!is.list(fit) || is.null(names(fit)) || !is.list(selection) ||
          is.null(names(selection)) || !is.list(training_y) || is.null(names(training_y))) {
        triomics_report_clinical_abort("A saved C endpoint lacks typed construction lineage.",
                                       paste0("C.score_endpoint_bundle.endpoints.", endpoint_id))
      }
      used_by_fit <- triomics_report_clinical_boolean(training_y, "used_by_fit")
      endpoints[[endpoint_id]] <- list(
        endpoint_id = endpoint_id,
        integration_id = triomics_report_identifier(integration, "integration_id"),
        component_id = triomics_report_identifier(endpoint, "component_id"),
        method = triomics_report_identifier(integration, "method", "not_reported"),
        native_method = triomics_report_identifier(fit, "native_method",
                         triomics_report_identifier(fit, "method", "not_reported")),
        response_type = triomics_report_identifier(endpoint, "response_type", "not_reported"),
        unit = triomics_report_unit(endpoint$unit, "C.score_endpoint.unit"),
        scale = triomics_report_identifier(endpoint, "scale", "not_reported"),
        orientation = triomics_report_identifier(endpoint, "orientation", "not_reported"),
        available_at_time = triomics_report_clinical_visit(endpoint$available_at_time,
                                                           "C.score_endpoint.available_at_time"),
        training_unit_count = triomics_report_clinical_array_count(
          endpoint$training_unit_ids, "C.score_endpoint.training_unit_ids"
        ),
        training_subject_count = triomics_report_clinical_array_count(
          endpoint$training_ids, "C.score_endpoint.training_ids"
        ),
        training_y_used_by_fit = used_by_fit,
        training_outcome_present = !is.null(endpoint$training_outcome),
        selection_ancestry = triomics_report_identifier(selection, "ancestry", "not_reported"),
        selection_validity = triomics_report_identifier(selection, "validity", "not_reported"),
        endpoint_source_hash = as.character(endpoint$source_hash),
        endpoint_sha256 = as.character(endpoint$endpoint_sha256),
        endpoint_bundle_artifact_id = artifact$reference$artifact_id,
        endpoint_bundle_payload_file = artifact$reference$path,
        endpoint_bundle_envelope_file = artifact$reference$envelope_path,
        endpoint_bundle_payload_sha256 = artifact$reference$payload_sha256,
        endpoint_bundle_envelope_sha256 = artifact$reference$envelope_sha256,
        unit_map_hash = as.character(bundle$unit_map_hash),
        dependency_group_present = is.character(endpoint$dependency_group) &&
          length(endpoint$dependency_group) == 1L && !is.na(endpoint$dependency_group) &&
          nzchar(endpoint$dependency_group)
      )
    }
  }
  endpoints
}

triomics_report_clinical_b_bundle_member <- function(loaded, config) {
  unverified <- function() list(
    integrity_status = "unverified", availability_status = "unverified", member = NULL
  )
  if (!is.list(loaded) || !is.list(loaded$d) || !is.list(config)) return(unverified())
  immutable <- loaded$d$immutable_inputs
  family <- if (is.list(immutable) && is.list(immutable$b_artifacts)) {
    immutable$b_artifacts$family_ledger
  } else NULL
  manifest_bundle <- loaded$d_generation$manifest$provenance$b$bundle
  d_bundle_fields <- c("b_bundle_artifact_id", "b_bundle_sha256", "b_bundle_envelope_sha256")
  family_fields <- c("artifact_type", "artifact_id", "path", "envelope_path", "sha256", "envelope_sha256")
  manifest_fields <- c("artifact_id", "payload_sha256", "envelope_sha256")
  if (!is.list(immutable) || !all(d_bundle_fields %in% names(immutable)) ||
      !is.list(family) || !all(family_fields %in% names(family)) ||
      !is.list(manifest_bundle) || !all(manifest_fields %in% names(manifest_bundle))) {
    return(unverified())
  }
  d_bundle_id <- as.character(immutable$b_bundle_artifact_id)
  d_bundle_sha <- as.character(immutable$b_bundle_sha256)
  d_bundle_envelope_sha <- as.character(immutable$b_bundle_envelope_sha256)
  if (length(d_bundle_id) != 1L || is.na(d_bundle_id) || !nzchar(d_bundle_id) ||
      !triomics_sha256_text(d_bundle_sha) || !triomics_sha256_text(d_bundle_envelope_sha) ||
      !identical(d_bundle_id, as.character(manifest_bundle$artifact_id)) ||
      !identical(d_bundle_sha, as.character(manifest_bundle$payload_sha256)) ||
      !identical(d_bundle_envelope_sha, as.character(manifest_bundle$envelope_sha256))) {
    triomics_report_clinical_abort(
      "D's immutable B bundle identities disagree with its generation manifest.",
      "D.immutable_inputs.b_bundle"
    )
  }
  family_path <- as.character(family$path)
  if (length(family_path) != 1L || is.na(family_path) || !nzchar(family_path)) {
    return(unverified())
  }
  path_parts <- strsplit(gsub("\\\\", "/", family_path), "/", fixed = TRUE)[[1L]]
  if (length(path_parts) != 4L || !identical(path_parts[[1L]], "B") ||
      !identical(path_parts[[2L]], "handoffs") ||
      !grepl("^[a-f0-9]{64}$", path_parts[[3L]]) ||
      !identical(path_parts[[4L]], "family-ledger.json") ||
      !identical(as.character(family$envelope_path),
                 sub("[.]json$", ".envelope.json", family_path))) {
    return(unverified())
  }
  family_artifact <- tryCatch(
    triomics_report_artifact(
      loaded$root, family_path, as.character(family$artifact_type),
      as.character(family$artifact_id), config$study_id, config$config_sha256,
      config$analysis_id, parse_payload = TRUE
    ),
    triomics_error = function(error) {
      triomics_report_clinical_abort(
        "D's pinned B family ledger cannot be read from its verified handoff directory.",
        "D.immutable_inputs.b_artifacts.family_ledger"
      )
    }
  )
  if (!identical(family_artifact$reference$payload_sha256, as.character(family$sha256)) ||
      !identical(family_artifact$reference$envelope_sha256, as.character(family$envelope_sha256)) ||
      !identical(family_artifact$reference$envelope_path,
                 file.path(dirname(family_path), "family-ledger.envelope.json"))) {
    triomics_report_clinical_abort(
      "D's pinned B family ledger differs from its saved artifact bytes.",
      "D.immutable_inputs.b_artifacts.family_ledger"
    )
  }
  bundle_path <- file.path(dirname(family_path), "bundle.json")
  bundle_artifact <- tryCatch(
    triomics_report_artifact(
      loaded$root, bundle_path, "stage_b_public_bundle", d_bundle_id,
      config$study_id, config$config_sha256, config$analysis_id, parse_payload = TRUE
    ),
    triomics_error = function(error) {
      triomics_report_clinical_abort(
        "D's pinned B public bundle cannot be read from its verified handoff directory.",
        "D.immutable_inputs.b_bundle"
      )
    }
  )
  if (!identical(bundle_artifact$reference$payload_sha256, d_bundle_sha) ||
      !identical(bundle_artifact$reference$envelope_sha256, d_bundle_envelope_sha)) {
    triomics_report_clinical_abort(
      "The saved B public bundle differs from D's immutable B bundle reference.",
      "D.immutable_inputs.b_bundle"
    )
  }
  members <- triomics_report_clinical_records(bundle_artifact$payload$members,
                                               "B.public_bundle.members")
  matches <- which(vapply(members, function(member) {
    identical(as.character(member$artifact_id %||% ""), "clinical_associations")
  }, logical(1L)))
  if (!length(matches)) {
    return(list(integrity_status = "absent", availability_status = "unavailable", member = NULL))
  }
  if (length(matches) != 1L) {
    triomics_report_clinical_abort(
      "The pinned B public bundle repeats its clinical association member.",
      "B.public_bundle.members.clinical_associations"
    )
  }
  member <- members[[matches[[1L]]]]
  member_fields <- c("artifact_id", "status", "path", "sha256", "envelope_path", "envelope_sha256")
  if (!all(member_fields %in% names(member))) {
    triomics_report_clinical_abort(
      "The pinned B clinical association member reference is malformed.",
      "B.public_bundle.members.clinical_associations"
    )
  }
  member_status <- member$status
  if (!is.character(member_status) || length(member_status) != 1L ||
      is.na(member_status) || !member_status %in% triomics_envelope_statuses()) {
    triomics_report_clinical_abort(
      "The pinned B clinical association member status is malformed.",
      "B.public_bundle.members.clinical_associations.status"
    )
  }
  reference_fields <- c("path", "sha256", "envelope_path", "envelope_sha256")
  absent_reference <- vapply(reference_fields, function(field) is.null(member[[field]]), logical(1L))
  if (all(absent_reference)) {
    if (member_status %in% c("complete", "partial")) {
      triomics_report_clinical_abort(
        "A complete or partial B clinical association member is missing its saved artifact references.",
        "B.public_bundle.members.clinical_associations"
      )
    }
    return(list(
      integrity_status = "absent", availability_status = as.character(member_status), member = member
    ))
  }
  if (any(absent_reference) ||
      !identical(as.character(member$path), file.path("B", "clinical-associations.json")) ||
      !identical(as.character(member$envelope_path), file.path("B", "clinical-associations.envelope.json")) ||
      !triomics_sha256_text(as.character(member$sha256)) ||
      !triomics_sha256_text(as.character(member$envelope_sha256))) {
    triomics_report_clinical_abort(
      "The pinned B clinical association member reference is malformed.",
      "B.public_bundle.members.clinical_associations"
    )
  }
  clinical_reference <- loaded$artifacts$clinical
  if (!is.list(clinical_reference) ||
      !identical(as.character(clinical_reference$path), as.character(member$path)) ||
      !identical(as.character(clinical_reference$envelope_path), as.character(member$envelope_path)) ||
      !identical(as.character(clinical_reference$artifact_type), "clinical_association_results") ||
      !identical(as.character(clinical_reference$artifact_id), "b-t08-clinical-associations") ||
      !identical(as.character(clinical_reference$status), as.character(member_status)) ||
      !identical(as.character(clinical_reference$payload_sha256), as.character(member$sha256)) ||
      !identical(as.character(clinical_reference$envelope_sha256), as.character(member$envelope_sha256))) {
    triomics_report_clinical_abort(
      "The loaded B clinical payload or envelope differs from D's pinned B bundle member.",
      "D.immutable_inputs.b_bundle.clinical_associations"
    )
  }
  list(integrity_status = "verified", availability_status = as.character(member_status), member = member)
}

triomics_report_clinical_endpoint_transport <- function(payload, endpoints) {
  unverified <- function() list(status = "unverified", endpoint_ids = character())
  native_transport <- if (is.list(payload)) payload$native_transport else NULL
  reference <- if (is.list(native_transport)) native_transport$endpoint_bundle else NULL
  required <- c("artifact_id", "payload_file", "payload_sha256", "envelope_file", "envelope_sha256")
  if (!is.list(reference) || is.null(names(reference)) || !all(required %in% names(reference))) {
    return(unverified())
  }
  text_fields <- c("artifact_id", "payload_file", "envelope_file")
  if (any(!vapply(text_fields, function(field) {
    value <- reference[[field]]
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }, logical(1L))) ||
      !triomics_sha256_text(as.character(reference$payload_sha256)) ||
      !triomics_sha256_text(as.character(reference$envelope_sha256))) {
    triomics_report_clinical_abort(
      "The B native transport endpoint bundle identity is malformed.",
      "clinical.native_transport.endpoint_bundle"
    )
  }
  matches <- names(endpoints)[vapply(endpoints, function(endpoint) {
    identical(as.character(reference$artifact_id), endpoint$endpoint_bundle_artifact_id) &&
      identical(as.character(reference$payload_file), endpoint$endpoint_bundle_payload_file) &&
      identical(as.character(reference$payload_sha256), endpoint$endpoint_bundle_payload_sha256) &&
      identical(as.character(reference$envelope_file), endpoint$endpoint_bundle_envelope_file) &&
      identical(as.character(reference$envelope_sha256), endpoint$endpoint_bundle_envelope_sha256)
  }, logical(1L))]
  if (!length(matches)) {
    triomics_report_clinical_abort(
      "The B native transport endpoint bundle does not match a verified C endpoint bundle.",
      "clinical.native_transport.endpoint_bundle"
    )
  }
  list(status = "verified", endpoint_ids = matches)
}

triomics_report_clinical_validate_adjustments <- function(records, field, dictionary_ids, key) {
  for (record_index in seq_along(records)) {
    adjustments <- records[[record_index]][[field]] %||% list()
    if (!is.list(adjustments)) {
      triomics_report_clinical_abort("Clinical adjustments are not a typed record array.",
                                     paste0(key, "[", record_index, "]"))
    }
    for (adjustment_index in seq_along(adjustments)) {
      adjustment <- adjustments[[adjustment_index]]
      if (!is.list(adjustment) || is.null(names(adjustment))) {
        triomics_report_clinical_abort("A clinical adjustment is not a named record.",
                                       paste0(key, "[", record_index, "][", adjustment_index, "]"))
      }
      variable <- triomics_report_identifier(adjustment, "variable")
      if (!nzchar(variable) || !variable %in% dictionary_ids) {
        triomics_report_clinical_abort(
          "A clinical adjustment variable is absent from the safe dictionary projection.",
          paste0(key, "[", record_index, "][", adjustment_index, "].variable")
        )
      }
    }
  }
  invisible(TRUE)
}

triomics_report_clinical_objective_rows <- function(payload, dictionary_ids) {
  objectives <- triomics_report_clinical_records(payload$objectives, "clinical.objectives")
  results <- triomics_report_clinical_records(payload$results, "clinical.results")
  triomics_report_clinical_validate_adjustments(
    objectives, "adjustments", dictionary_ids, "clinical.objectives.adjustments"
  )
  ids <- vapply(seq_along(objectives), function(index) {
    triomics_report_identifier(objectives[[index]], "objective_id")
  }, character(1L))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    triomics_report_clinical_abort("Registered clinical objective identifiers are missing or duplicated.",
                                   "clinical.objectives")
  }
  result_ids <- vapply(seq_along(results), function(index) {
    triomics_report_identifier(results[[index]], "objective_id")
  }, character(1L))
  if (any(!nzchar(result_ids)) || any(!result_ids %in% ids)) {
    triomics_report_clinical_abort("A clinical result has no registered objective.",
                                   "clinical.results.objective_id")
  }
  rows <- triomics_report_clinical_rows(payload)
  columns <- c("objective_id", "mode", "response_ref", "predictor_ref",
               "hypothesis_status", "protocol_ref", "family_id", "time_window",
               "missingness_policy", "registered_adjustment_count",
               "registered_adjustments", "model_formula", "result_status", "result_reason")
  records <- list()
  for (objective_index in seq_along(objectives)) {
    objective <- objectives[[objective_index]]
    objective_id <- ids[[objective_index]]
    matching <- which(result_ids == objective_id)
    if (!length(matching)) matching <- NA_integer_
    for (result_index in matching) {
      result <- if (is.na(result_index)) NULL else results[[result_index]]
      if (is.null(result)) {
        projected <- NULL
        result_status <- "unavailable"
        result_reason <- "D_REPORT_CLINICAL_RESULT_ABSENT"
        formula <- "not_reported"
      } else {
        projected <- rows[result_index, , drop = FALSE]
        result_status <- as.character(projected$status[[1L]])
        result_reason <- triomics_report_reason(result, "estimation_reason")
        formula <- triomics_report_identifier_value(result$formula %||% "not_reported",
                                                      "clinical.results.formula", "not_reported")
      }
      predictor <- triomics_report_identifier(objective, "predictor_ref", "not_reported")
      if (predictor != "not_reported" && !predictor %in% dictionary_ids) {
        triomics_report_clinical_abort(
          "A clinical objective predictor is absent from the safe dictionary projection.",
          paste0("clinical.objectives[", objective_index, "].predictor_ref")
        )
      }
      adjustments <- triomics_report_clinical_adjustments(
        list(adjustment = objective$adjustments %||% list())
      )
      adjustment_text <- if (!nzchar(adjustments$variables)) "" else {
        entries <- strsplit(adjustments$variables, ";", fixed = TRUE)[[1L]]
        roles <- strsplit(adjustments$roles, ";", fixed = TRUE)[[1L]]
        forms <- strsplit(adjustments$forms, ";", fixed = TRUE)[[1L]]
        paste(paste(entries, roles, forms, sep = ":"), collapse = ";")
      }
      records[[length(records) + 1L]] <- data.frame(
        objective_id = objective_id,
        mode = triomics_report_identifier(objective, "mode", "not_reported"),
        response_ref = triomics_report_identifier(objective, "response_ref", "not_reported"),
        predictor_ref = predictor,
        hypothesis_status = triomics_report_identifier(objective, "hypothesis_status", "not_reported"),
        protocol_ref = triomics_report_identifier(objective, "protocol_ref", "not_reported"),
        family_id = triomics_report_identifier(objective, "family_id",
                      triomics_report_identifier(objective, "family_ref", "not_reported")),
        time_window = triomics_report_clinical_visit(objective$time_window_ref,
                                                     paste0("clinical.objectives[", objective_index, "].time_window_ref")),
        missingness_policy = triomics_report_identifier(objective, "missingness_policy", "not_reported"),
        registered_adjustment_count = adjustments$count,
        registered_adjustments = adjustment_text,
        model_formula = formula,
        result_status = result_status,
        result_reason = result_reason,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  if (!length(records)) {
    return(triomics_report_clinical_empty_frame(
      columns, list(registered_adjustment_count = integer())
    ))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_family_rows <- function(payload) {
  family <- payload$family
  if (is.null(family)) {
    return(triomics_report_clinical_empty_frame(
      c("family_id", "claim_scope", "member_hash", "n_declared", "n_estimated",
        "n_failed", "correction_method", "q_published"),
      list(n_declared = integer(), n_estimated = integer(), n_failed = integer(),
           q_published = logical())
    ))
  }
  families <- if (is.list(family) && "family_id" %in% names(family)) list(family) else
    triomics_report_clinical_records(family, "clinical.family")
  columns <- c("family_id", "claim_scope", "member_hash", "n_declared", "n_estimated",
               "n_failed", "correction_method", "q_published")
  records <- lapply(seq_along(families), function(index) {
    value <- families[[index]]
    data.frame(
      family_id = triomics_report_identifier(value, "family_id"),
      claim_scope = triomics_report_identifier(value, "claim_scope", "not_reported"),
      member_hash = triomics_report_identifier(value, "member_hash", "not_reported"),
      n_declared = triomics_report_clinical_count(value, "n_declared"),
      n_estimated = triomics_report_clinical_count(value, "n_estimated"),
      n_failed = triomics_report_clinical_count(value, "n_failed"),
      correction_method = triomics_report_identifier(value, "correction_method", "not_reported"),
      q_published = triomics_report_clinical_boolean(value, "q_published"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  if (!length(records)) {
    return(triomics_report_clinical_empty_frame(
      columns, list(n_declared = integer(), n_estimated = integer(), n_failed = integer(),
                    q_published = logical())
    ))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_association_rows <- function(payload, endpoints, dictionary_ids,
                                                       b_bundle, endpoint_transport) {
  results <- triomics_report_clinical_records(payload$results, "clinical.results")
  triomics_report_clinical_validate_adjustments(
    results, "adjustment", dictionary_ids, "clinical.results.adjustment"
  )
  rows <- triomics_report_clinical_rows(payload)
  columns <- c(names(rows), "response_role", "predictor_role", "estimand", "effect_scale",
               "predictor_units", "response_units", "inferential_status", "hypothesis_status",
               "selection_ancestry", "selection_validity", "independent_validation_released",
               "endpoint_lineage_status", "evidence_class", "n_samples",
               "n_excluded_clinical", "n_excluded_endpoint", "result_reason")
  records <- lapply(seq_along(results), function(index) {
    result <- results[[index]]
    predictor <- triomics_report_identifier(result, "predictor_ref", "not_reported")
    if (predictor != "not_reported" && !predictor %in% dictionary_ids) {
      triomics_report_clinical_abort(
        "A clinical result predictor is absent from the safe dictionary projection.",
        paste0("clinical.results[", index, "].predictor_ref")
      )
    }
    endpoint_id <- triomics_report_identifier(result, "endpoint_id", "not_reported")
    endpoint <- endpoints[[endpoint_id]]
    selection <- result$selection
    if (is.null(selection)) {
      selection_ancestry <- "not_reported"
      selection_validity <- "not_reported"
      inspected_outcome <- FALSE
    } else if (!is.list(selection) || is.null(names(selection))) {
      triomics_report_clinical_abort("Clinical result selection lineage is malformed.",
                                     paste0("clinical.results[", index, "].selection"))
    } else {
      selection_ancestry <- triomics_report_identifier(selection, "ancestry", "not_reported")
      selection_validity <- triomics_report_identifier(selection, "validity", "not_reported")
      inspected_outcome <- !is.null(selection$inspected_outcome)
    }
    released_value <- result$independent_validation_released
    if (!is.null(released_value) &&
        (!is.logical(released_value) || length(released_value) != 1L || is.na(released_value))) {
      triomics_report_clinical_abort(
        "A clinical independent-validation release flag is not a scalar boolean.",
        paste0("clinical.results[", index, "].independent_validation_released")
      )
    }
    independent_validation_released <- if (is.null(released_value)) NA else released_value
    q_value <- triomics_report_number(result, "q")
    endpoint_lineage_status <- "not_in_saved_C_endpoint_bundle"
    evidence_class <- "association_endpoint_lineage_unavailable"
    if (!is.null(endpoint)) {
      if (!identical(b_bundle$integrity_status, "verified")) {
        endpoint_lineage_status <- "B_clinical_payload_unverified"
      } else if (!identical(endpoint_transport$status, "verified")) {
        endpoint_lineage_status <- "B_endpoint_bundle_unverified"
      } else if (!endpoint_id %in% endpoint_transport$endpoint_ids) {
        triomics_report_clinical_abort(
          "A B clinical result names an endpoint outside its verified C endpoint bundle.",
          paste0("clinical.results[", index, "].endpoint_id")
        )
      } else endpoint_lineage_status <- "saved_C_endpoint_verified"

      endpoint_ancestry <- endpoint$selection_ancestry
      if (selection_ancestry == "not_reported") selection_ancestry <- endpoint_ancestry
      training_outcome <- isTRUE(endpoint$training_y_used_by_fit) ||
        isTRUE(endpoint$training_outcome_present) ||
        selection_ancestry == "trained_on_outcome" || endpoint_ancestry == "trained_on_outcome"
      outcome_selected <- inspected_outcome ||
        selection_ancestry == "selected_after_clinical_outcome" ||
        endpoint_ancestry == "selected_after_clinical_outcome"
      endpoint_verified <- identical(endpoint_lineage_status, "saved_C_endpoint_verified")
      independent_qualified <- endpoint_verified &&
        identical(triomics_report_identifier(result, "estimation_status", "not_reported"), "estimated") &&
        identical(triomics_report_identifier(result, "inferential_status", "not_reported"),
                  "independent_validation") &&
        identical(triomics_report_identifier(result, "hypothesis_status", "not_reported"),
                  "independent_validation") &&
        isTRUE(independent_validation_released) &&
        is.finite(q_value) && q_value >= 0 && q_value <= 1 &&
        identical(selection_ancestry, "prespecified") &&
        identical(selection_validity, "qualified") && !inspected_outcome &&
        identical(endpoint$selection_ancestry, "prespecified") &&
        identical(endpoint$selection_validity, "qualified") &&
        !isTRUE(endpoint$training_y_used_by_fit) &&
        !isTRUE(endpoint$training_outcome_present)
      evidence_class <- if (!endpoint_verified) {
        "association_endpoint_lineage_unavailable"
      } else if (training_outcome) {
        "training_outcome_descriptive"
      } else if (outcome_selected) {
        "outcome_selected_descriptive"
      } else if (independent_qualified) {
        "producer_qualified_independent_B_association"
      } else "descriptive_score_association"
    }
    base <- rows[index, , drop = FALSE]
    data.frame(
      base,
      response_role = triomics_report_identifier(result, "response_role", "not_reported"),
      predictor_role = triomics_report_identifier(result, "predictor_role", "not_reported"),
      estimand = triomics_report_identifier(result, "estimand", "not_reported"),
      effect_scale = triomics_report_identifier(result, "effect_scale", "not_reported"),
      predictor_units = triomics_report_unit(result$predictor_units, "clinical.results.predictor_units"),
      response_units = triomics_report_unit(result$response_units, "clinical.results.response_units"),
      inferential_status = triomics_report_identifier(result, "inferential_status", "not_reported"),
      hypothesis_status = triomics_report_identifier(result, "hypothesis_status", "not_reported"),
      selection_ancestry = selection_ancestry,
      selection_validity = selection_validity,
      independent_validation_released = independent_validation_released,
      endpoint_lineage_status = endpoint_lineage_status,
      evidence_class = evidence_class,
      n_samples = if (is.null(result$n_samples)) NA_integer_ else {
        triomics_report_clinical_count(result, "n_samples")
      },
      n_excluded_clinical = if (is.null(result$n_excluded_clinical)) NA_integer_ else {
        triomics_report_clinical_count(result, "n_excluded_clinical")
      },
      n_excluded_endpoint = if (is.null(result$n_excluded_endpoint)) NA_integer_ else {
        triomics_report_clinical_count(result, "n_excluded_endpoint")
      },
      result_reason = triomics_report_reason(result, "estimation_reason"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  if (!length(records)) {
    types <- list(
      effect = numeric(), ordinary_se = numeric(), moderated_se = numeric(),
      interval_lower = numeric(), interval_upper = numeric(), statistic = numeric(),
      ordinary_df = numeric(), effective_test_df = numeric(), p = numeric(), q = numeric(),
      n_subjects = numeric(), n_rows_used = numeric(), adjustment_count = integer(),
      n_samples = integer(), n_excluded_clinical = integer(), n_excluded_endpoint = integer(),
      independent_validation_released = logical()
    )
    return(triomics_report_clinical_empty_frame(columns, types))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_endpoint_lineage_rows <- function(endpoints, clinical_results) {
  endpoint_ids <- unique(vapply(seq_along(clinical_results), function(index) {
    triomics_report_identifier(clinical_results[[index]], "endpoint_id", "not_reported")
  }, character(1L)))
  endpoint_ids <- endpoint_ids[endpoint_ids %in% names(endpoints)]
  columns <- c("endpoint_id", "integration_id", "component_id", "method", "native_method",
               "response_type", "unit", "scale", "orientation", "available_at_time",
               "training_unit_count", "training_subject_count", "training_outcome_used_by_fit",
               "training_outcome_status", "selection_ancestry", "selection_validity",
               "dependency_group_present", "unit_map_hash", "endpoint_source_hash",
               "endpoint_bundle_artifact_id", "endpoint_bundle_payload_file",
               "endpoint_bundle_envelope_file", "endpoint_sha256", "endpoint_bundle_payload_sha256",
               "endpoint_bundle_envelope_sha256")
  records <- lapply(endpoint_ids, function(endpoint_id) {
    endpoint <- endpoints[[endpoint_id]]
    data.frame(
      endpoint_id = endpoint$endpoint_id,
      integration_id = endpoint$integration_id,
      component_id = endpoint$component_id,
      method = endpoint$method,
      native_method = endpoint$native_method,
      response_type = endpoint$response_type,
      unit = endpoint$unit,
      scale = endpoint$scale,
      orientation = endpoint$orientation,
      available_at_time = endpoint$available_at_time,
      training_unit_count = endpoint$training_unit_count,
      training_subject_count = endpoint$training_subject_count,
      training_outcome_used_by_fit = endpoint$training_y_used_by_fit,
      training_outcome_status = if (endpoint$training_y_used_by_fit) "used_by_fit" else
        if (endpoint$training_outcome_present) "source_reported" else "not_used_by_fit",
      selection_ancestry = endpoint$selection_ancestry,
      selection_validity = endpoint$selection_validity,
      dependency_group_present = endpoint$dependency_group_present,
      unit_map_hash = endpoint$unit_map_hash,
      endpoint_source_hash = endpoint$endpoint_source_hash,
      endpoint_bundle_artifact_id = endpoint$endpoint_bundle_artifact_id,
      endpoint_bundle_payload_file = endpoint$endpoint_bundle_payload_file,
      endpoint_bundle_envelope_file = endpoint$endpoint_bundle_envelope_file,
      endpoint_sha256 = endpoint$endpoint_sha256,
      endpoint_bundle_payload_sha256 = endpoint$endpoint_bundle_payload_sha256,
      endpoint_bundle_envelope_sha256 = endpoint$endpoint_bundle_envelope_sha256,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  if (!length(records)) {
    return(triomics_report_clinical_empty_frame(
      columns, list(training_unit_count = integer(), training_subject_count = integer(),
                    training_outcome_used_by_fit = logical(), dependency_group_present = logical())
    ))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result[, columns, drop = FALSE]
}

triomics_report_clinical_held_out_record <- function(values = list()) {
  record <- list(
    comparison_type = "not_reported", evidence_class = "unavailable", status = "unavailable",
    integration_id = "not_reported", source_outcome_label = "not_reported",
    clinical_variable_id = "not_reported",
    endpoint_link_status = "unverified", link_scope = "not_applicable",
    metric = "", metric_definition = "", metric_status = "not_reported",
    model_value = NA_real_, baseline_value = NA_real_, unit = "not_reported",
    source_artifact_sha256 = "not_reported", split_plan_sha256 = "not_reported",
    outer_fold_count = NA_integer_, resampling_unit = "not_reported",
    n_eligible = NA_integer_, n_predicted = NA_integer_,
    failed_prediction_count = NA_integer_, n_ineligible = NA_integer_,
    total_denominator = NA_integer_, n_matched = NA_integer_,
    comparison_coverage = "not_reported", baseline_definition = "not_reported",
    dependence_limitations = "not_reported", completion_policy = "not_applicable",
    evidence_scope = "not_reported", qualification_status = "unqualified",
    model_execution = "not_reported", independent_validation = "not_claimed",
    benefit_guaranteed = FALSE, reason_code = ""
  )
  for (name in intersect(names(values), names(record))) record[[name]] <- values[[name]]
  as.data.frame(record, stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_clinical_c02_spec <- function(config, integration_id) {
  raw <- if (is.list(config)) config$config else NULL
  analyses <- if (is.list(raw)) raw$analyses else NULL
  if (!is.list(analyses) || length(analyses) != 1L || !is.list(analyses[[1L]])) return(NULL)
  analysis <- analyses[[1L]]
  if (!identical(as.character(analysis$analysis_id %||% ""), as.character(config$analysis_id %||% ""))) {
    return(NULL)
  }
  integrations <- analysis$integration
  if (!is.list(integrations)) return(NULL)
  matches <- which(vapply(integrations, function(spec) {
    is.list(spec) && identical(as.character(spec$integration_id %||% ""), integration_id)
  }, logical(1L)))
  if (length(matches) != 1L) return(NULL)
  integrations[[matches[[1L]]]]
}

triomics_report_clinical_c02_fold_metadata <- function(evaluation, spec) {
  folds <- evaluation$outer_folds
  fold_count <- if (is.list(folds)) length(folds) else 0L
  grouping <- if (is.list(spec$evaluation)) spec$evaluation$generalization_unit else NULL
  grouping <- if (is.character(grouping) && length(grouping) == 1L &&
                  grouping %in% c("subject", "sample") &&
                  identical(as.character(evaluation$uncertainty$resampling_unit %||% ""), grouping)) {
    grouping
  } else "not_reported"
  list(available = fold_count > 0L && grouping != "not_reported",
       count = as.integer(fold_count), grouping = grouping)
}

triomics_report_clinical_c02_metric_value <- function(metrics, metric, key) {
  value <- if (is.list(metrics)) metrics[[metric]] else NULL
  if (is.null(value) || (is.numeric(value) && length(value) == 1L && is.na(value))) return(NA_real_)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    triomics_report_clinical_abort("A C-02 aggregate metric is not a finite scalar or unavailable value.", key)
  }
  as.numeric(value)
}

triomics_report_clinical_c02_rows <- function(loaded, config) {
  integrations <- loaded$payloads$c$integrations %||% list()
  integrations <- triomics_report_clinical_records(integrations, "C.integrations")
  if (!length(integrations)) {
    return(list(triomics_report_clinical_held_out_record(list(
      comparison_type = "C-02 nested evaluation",
      reason_code = "C02_EVALUATION_SOURCE_ABSENT"
    ))))
  }
  source_sha256 <- loaded$artifacts$c$payload_sha256 %||% "not_reported"
  if (!triomics_sha256_text(as.character(source_sha256))) source_sha256 <- "not_reported"
  records <- list()
  for (index in seq_along(integrations)) {
    integration <- integrations[[index]]
    integration_id <- triomics_report_identifier(integration, "integration_id", "not_reported")
    method <- triomics_report_identifier(integration, "method", "not_reported")
    evaluation <- integration$evaluation
    if (is.null(evaluation)) {
      records[[length(records) + 1L]] <- triomics_report_clinical_held_out_record(list(
        comparison_type = "C-02 nested evaluation", integration_id = integration_id,
        source_artifact_sha256 = as.character(source_sha256),
        reason_code = "C02_EVALUATION_ARTIFACT_ABSENT"
      ))
      next
    }
    if (!is.list(evaluation) || is.null(names(evaluation))) {
      triomics_report_clinical_abort("A saved C integration evaluation is not a typed record.",
                                     paste0("C.integrations[", index, "].evaluation"))
    }
    status <- triomics_report_evaluation_status(evaluation)
    spec <- triomics_report_clinical_c02_spec(config, integration_id)
    mode <- as.character(evaluation$mode %||% "")
    method_matches <- identical(as.character(evaluation$method %||% ""), method) &&
      !is.null(spec) && identical(as.character(spec$method %||% ""), method)
    split_hash <- as.character(evaluation$split_plan_hash %||% "not_reported")
    split_hash_valid <- triomics_sha256_text(split_hash)
    folds <- if (is.null(spec)) list(available = FALSE, count = 0L, grouping = "not_reported") else {
      triomics_report_clinical_c02_fold_metadata(evaluation, spec)
    }
    outcome <- if (is.list(spec) && is.character(spec$outcome) && length(spec$outcome) == 1L &&
                   !is.na(spec$outcome)) spec$outcome else ""
    source_outcome_label <- if (nzchar(outcome)) outcome else "not_reported"
    link_status <- "unverified"
    clinical_variable_id <- "not_reported"
    n_predicted <- triomics_report_number(evaluation, "n_predicted")
    required_fields <- c(
      "mode", "method", "native_version", "split_plan_hash", "outer_folds",
      "predictions", "metrics", "baseline_comparison", "uncertainty", "failures",
      "requested_folds", "n_eligible", "n_predicted", "failed_prediction_count",
      "n_ineligible", "total_denominator", "inspected_fit_count", "max_fits"
    )
    producer_shape <- all(required_fields %in% names(evaluation))
    lineage_valid <- producer_shape && method_matches && split_hash_valid && folds$available &&
      status %in% c("complete", "partial") && is.finite(n_predicted) && n_predicted > 0
    model_metrics <- evaluation$metrics$model %||% list()
    baseline_metrics <- evaluation$metrics$baseline %||% list()
    metric_names <- if (identical(mode, "continuous")) c("rmse", "mae", "r_squared") else
      if (identical(mode, "categorical")) c("balanced_error", "accuracy") else character()
    definitions <- c(
      rmse = "Root mean squared error on predicted outer-fold units",
      mae = "Mean absolute error on predicted outer-fold units",
      r_squared = "Held-out R-squared against a training-only intercept baseline",
      balanced_error = "Balanced classification error on eligible outer-fold units",
      accuracy = "Accuracy on eligible outer-fold units"
    )
    metric_rows <- lapply(metric_names, function(metric) {
      model_value <- triomics_report_clinical_c02_metric_value(
        model_metrics, metric, paste0("C.integrations[", index, "].evaluation.metrics.model.", metric)
      )
      baseline_value <- triomics_report_clinical_c02_metric_value(
        baseline_metrics, metric, paste0("C.integrations[", index, "].evaluation.metrics.baseline.", metric)
      )
      unit <- if (metric %in% c("rmse", "mae")) "not_reported" else
        if (metric %in% c("balanced_error", "accuracy")) "proportion" else "unitless"
      triomics_report_clinical_held_out_record(list(
        comparison_type = "C-02 nested evaluation",
        evidence_class = if (lineage_valid && (is.finite(model_value) || is.finite(baseline_value))) {
          "held_out_model_evaluation"
        } else "unqualified_evaluation",
        status = if (status %in% c("complete", "partial", "failed")) status else "unavailable",
        integration_id = integration_id, source_outcome_label = source_outcome_label,
        clinical_variable_id = clinical_variable_id,
        endpoint_link_status = link_status, link_scope = "integration_level_only",
        metric = metric, metric_definition = unname(definitions[[metric]]),
        metric_status = if (is.finite(model_value) && is.finite(baseline_value)) "reported" else "unavailable",
        model_value = model_value, baseline_value = baseline_value, unit = unit,
        source_artifact_sha256 = as.character(source_sha256),
        split_plan_sha256 = if (split_hash_valid) split_hash else "not_reported",
        outer_fold_count = folds$count, resampling_unit = folds$grouping,
        n_eligible = triomics_report_clinical_optional_count(evaluation, "n_eligible"),
        n_predicted = triomics_report_clinical_optional_count(evaluation, "n_predicted"),
        failed_prediction_count = triomics_report_clinical_optional_count(evaluation, "failed_prediction_count"),
        n_ineligible = triomics_report_clinical_optional_count(evaluation, "n_ineligible"),
        total_denominator = triomics_report_clinical_optional_count(evaluation, "total_denominator"),
        n_matched = triomics_report_clinical_optional_count(evaluation$baseline_comparison, "n_matched"),
        comparison_coverage = triomics_report_identifier(
          evaluation$baseline_comparison, "coverage", "not_reported"
        ),
        baseline_definition = if (identical(mode, "continuous")) "training-only intercept" else
          "training-majority class",
        dependence_limitations = paste0(
          "Outer-fold training sets overlap; folds are not independent ", folds$grouping, "s."
        ),
        evidence_scope = "C-02 nested cross-validation",
        qualification_status = if (lineage_valid) "C02_nested_evaluation_source" else "unqualified",
        model_execution = if (lineage_valid) "reported" else "not_established",
        independent_validation = "not_claimed",
        reason_code = if (lineage_valid) "C02_HELD_OUT_EVALUATION_REPORTED" else
          "C02_HELD_OUT_LINEAGE_UNVERIFIED"
      ))
    })
    if (!length(metric_rows)) {
      metric_rows <- list(triomics_report_clinical_held_out_record(list(
        comparison_type = "C-02 nested evaluation", integration_id = integration_id,
        status = if (status %in% c("complete", "partial", "failed")) status else "unavailable",
        source_artifact_sha256 = as.character(source_sha256),
        split_plan_sha256 = if (split_hash_valid) split_hash else "not_reported",
        outer_fold_count = folds$count, resampling_unit = folds$grouping,
        reason_code = "C02_EVALUATION_METRICS_UNAVAILABLE"
      )))
    }
    records <- c(records, metric_rows)
  }
  records
}

triomics_report_clinical_held_out_status <- function(loaded, config) {
  c09 <- loaded$d$inherited_evidence$stage_status$clinical_prediction
  if (is.null(c09)) {
    c09_row <- triomics_report_clinical_held_out_record(list(
      comparison_type = "C-09 matched clinical prediction",
      evidence_class = "unavailable",
      status = "unavailable",
      completion_policy = "conditional",
      evidence_scope = "synthetic_fixture_only",
      qualification_status = "unqualified",
      model_execution = "not_reported",
      independent_validation = "not_performed",
      benefit_guaranteed = FALSE,
      reason_code = "C09_STAGE_STATUS_ABSENT"
    ))
  } else {
    if (!is.list(c09) || is.null(names(c09)) ||
        (!is.null(c09$packet_id) && !identical(as.character(c09$packet_id), "C-09"))) {
      triomics_report_clinical_abort("The saved C-09 stage status is malformed.",
                                     "D.stage_status.clinical_prediction")
    }
    if (isTRUE(c09$benefit_guaranteed)) {
      triomics_report_abort("D_REPORT_SOURCE_INVALID", "C-09 cannot claim guaranteed benefit.",
                            "D.stage_status.clinical_prediction.benefit_guaranteed")
    }
    status <- triomics_report_identifier(c09, "status", "unavailable")
    c09_row <- triomics_report_clinical_held_out_record(list(
      comparison_type = "C-09 matched clinical prediction",
      evidence_class = if (identical(status, "complete")) "held_out_comparison_status_only" else "unavailable",
      status = status,
      completion_policy = triomics_report_identifier(c09, "completion_policy", "conditional"),
      evidence_scope = triomics_report_identifier(c09, "status_scope", "synthetic_fixture_only"),
      qualification_status = triomics_report_identifier(c09, "qualification_status", "unqualified"),
      model_execution = triomics_report_identifier(c09, "model_execution", status),
      independent_validation = triomics_report_identifier(c09, "independent_validation", "not_performed"),
      benefit_guaranteed = isTRUE(c09$benefit_guaranteed),
      reason_code = if (identical(status, "not_started")) "C09_COMPARISON_ARTIFACT_ABSENT" else
        "C09_STAGE_STATUS_REPORTED"
    ))
  }
  c02_rows <- triomics_report_clinical_c02_rows(loaded, config)
  do.call(rbind, c(list(c09_row), c02_rows))
}

triomics_report_clinical_limit_rows <- function(loaded) {
  controls <- loaded$d$inherited_evidence$evaluation_evidence$controls %||% list()
  control_status <- if (length(controls) == 4L && all(vapply(controls, function(control) {
    is.list(control) && identical(as.character(control$status), "not_run")
  }, logical(1L)))) "four_controls_not_run" else "status_not_established"
  data.frame(
    evidence_class = c("interpretation", "B-association", "lineage", "C-07",
                       "D-06/D-07/D-14", "qualification"),
    statement = c(
      "Individual molecular score associations are not contrast enrichment, patient-level pathway scores, or target binding.",
      "A producer-qualified B association is a released B association category; it does not establish external replication, held-out clinical prediction, biological confirmation, or guaranteed benefit.",
      "Shared score construction and subject linkage are retained by saved source hashes; unit identifiers and observation records are withheld.",
      paste0("C-07 control status: ", control_status, "."),
      "D-06 and D-07 retain their conditional evidence limits; D-14 remains conditional-unapproved and receives no completion credit from this section.",
      "The frozen-kit independent audit remains pending; synthetic software output does not establish clinical or biological validity."
    ),
    stringsAsFactors = FALSE
  )
}

triomics_report_clinical_section <- function(loaded, config) {
  if (!is.list(config) || !isTRUE(config$clinical_enabled)) {
    return(list(status = "not_requested", reason_code = "", tables = list()))
  }
  audience <- as.character(config$audience %||% "")
  if (identical(audience, "approved_aggregate")) {
    triomics_report_abort(
      "D_REPORT_DISCLOSURE_UNQUALIFIED",
      "Clinical aggregate release is unavailable without a qualified minimum-cell and complementary-suppression policy.",
      "execution.disclosure_policy_ref"
    )
  }
  payload <- if (is.list(loaded)) loaded$payloads$clinical else NULL
  loaded_clinical_reference <- if (is.list(loaded) && is.list(loaded$artifacts)) {
    loaded$artifacts$clinical
  } else NULL
  if (is.null(payload) && is.null(loaded_clinical_reference)) {
    return(list(status = "unavailable", reason_code = "D_REPORT_CLINICAL_EXPORT_MISSING",
                tables = list()))
  }
  b_bundle <- triomics_report_clinical_b_bundle_member(loaded, config)
  if (identical(b_bundle$integrity_status, "absent")) {
    triomics_report_clinical_abort(
      "Loaded clinical evidence is absent from D's immutable B bundle.",
      "D.immutable_inputs.b_bundle.clinical_associations"
    )
  }
  if (is.null(payload)) {
    return(list(status = "unavailable", reason_code = "D_REPORT_CLINICAL_EXPORT_MISSING",
                tables = list()))
  }
  if (!is.list(payload) || is.null(names(payload)) ||
      !identical(as.character(payload$schema_version), "2.0")) {
    triomics_report_clinical_abort("The saved clinical result has an unsupported payload shape.",
                                   "clinical")
  }
  dictionary <- payload$clinical_dictionary_coverage
  if (!is.list(dictionary) || is.null(names(dictionary)) ||
      !identical(as.character(dictionary$schema_version), "1.0") ||
      is.null(dictionary$variables) || is.null(dictionary$coverage)) {
    return(list(
      status = "partial",
      reason_code = "D_REPORT_CLINICAL_COVERAGE_UNSUPPORTED",
      tables = list()
    ))
  }
  if (!is.character(payload$status) || length(payload$status) != 1L ||
      is.na(payload$status) || !payload$status %in% c("complete", "partial", "unavailable")) {
    triomics_report_clinical_abort("The saved clinical result has an invalid status.", "clinical.status")
  }
  dictionary_rows <- triomics_report_clinical_dictionary_rows(dictionary)
  safe_variable_ids <- dictionary_rows$variable_id
  coverage_rows <- triomics_report_clinical_coverage_rows(dictionary, safe_variable_ids)
  exclusion_rows <- triomics_report_clinical_exclusion_rows(dictionary)
  endpoints <- triomics_report_clinical_saved_endpoints(loaded, config)
  objective_rows <- triomics_report_clinical_objective_rows(payload, safe_variable_ids)
  endpoint_transport <- triomics_report_clinical_endpoint_transport(payload, endpoints)
  association_rows <- triomics_report_clinical_association_rows(
    payload, endpoints, safe_variable_ids, b_bundle, endpoint_transport
  )
  family_rows <- triomics_report_clinical_family_rows(payload)
  result_records <- triomics_report_clinical_records(payload$results, "clinical.results")
  endpoint_lineage <- triomics_report_clinical_endpoint_lineage_rows(endpoints, result_records)
  held_out <- triomics_report_clinical_held_out_status(loaded, config)
  limits <- triomics_report_clinical_limit_rows(loaded)
  lineage_missing <- nrow(association_rows) > 0L &&
    any(association_rows$endpoint_lineage_status != "saved_C_endpoint_verified")
  status <- if (identical(payload$status, "unavailable")) "unavailable" else if (
    identical(payload$status, "partial") || lineage_missing) "partial" else "complete"
  reason_code <- if (status == "complete") "" else if (lineage_missing) {
    "D_REPORT_CLINICAL_ENDPOINT_LINEAGE_INCOMPLETE"
  } else "D_REPORT_CLINICAL_EXPORT_PARTIAL"
  list(
    status = status,
    reason_code = reason_code,
    tables = list(
      objectives = objective_rows,
      associations = association_rows,
      families = family_rows,
      dictionary_coverage = dictionary_rows,
      variable_coverage = coverage_rows,
      disclosure_exclusions = exclusion_rows,
      endpoint_lineage = endpoint_lineage,
      held_out_comparisons = held_out,
      interpretation_limits = limits
    )
  )
}
