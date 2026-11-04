triomics_target_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

triomics_target_taxon <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x > 0 && x == floor(x)
}

triomics_target_constructs <- function(x) {
  is.list(x) && !is.data.frame(x) && is.null(names(x)) &&
    all(vapply(x, triomics_target_string, logical(1))) &&
    !anyDuplicated(unlist(x, use.names = FALSE))
}

triomics_target_request <- function(identity, mode, selection, context = NULL, drug = NULL, universe = NULL) {
  invalid_identity <- !is.list(identity) || is.data.frame(identity) ||
    !triomics_target_string(identity$namespace) || !triomics_target_string(identity$id) ||
    !triomics_target_taxon(identity$taxon_id) || !triomics_target_string(identity$entity_type) ||
    !identity$entity_type %in% c("protein", "complex", "pathway", "other_declared") ||
    (!is.null(identity$constructs) && !triomics_target_constructs(identity$constructs))
  if (invalid_identity) {
    triomics_d_abort("D_TARGET_IDENTITY_REQUIRED",
                     "A namespaced target, positive integer taxon and entity type are required.", "identity")
  }
  constructs <- identity$constructs %||% list()
  if (!triomics_target_string(mode) || !mode %in% c("assess", "compare", "discover")) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Target mode is invalid.", "mode")
  }
  if (!is.null(context) && (!is.list(context) || is.data.frame(context) ||
      (!is.null(context$context_id) && !triomics_target_string(context$context_id)))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Target context needs one declared context identifier.", "context")
  }
  if (!is.null(drug) && (!is.list(drug) || is.data.frame(drug) ||
      length(drug) != 3L || !setequal(names(drug), c("namespace", "drug_id", "label")) ||
      !triomics_target_string(drug$namespace) || !triomics_target_string(drug$drug_id) ||
      !triomics_target_string(drug$label))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Drug must declare a namespace, ID and label.", "drug")
  }
  if (mode == "discover" && (!is.list(universe) || !triomics_target_string(universe$universe_id) ||
      !triomics_target_string(universe$version) || !identical(universe$enumeration, "unranked"))) {
    triomics_d_abort("D_TARGET_UNIVERSE_REQUIRED", "Discovery requires a versioned unranked universe.", "universe")
  }
  if (!is.list(selection) || is.data.frame(selection) || !is.null(names(selection))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Selection must be an array of records.", "selection")
  }
  valid_selection <- vapply(selection, function(item) {
    is.list(item) && !is.data.frame(item) && triomics_target_string(item$selection_id) &&
      triomics_target_string(item$hypothesis_status) && item$hypothesis_status %in% c("exploratory", "confirmed") &&
      is.logical(item$selected_with_observations) && length(item$selected_with_observations) == 1L &&
      !is.na(item$selected_with_observations)
  }, logical(1))
  selection_ids <- if (length(selection)) {
    vapply(selection, function(item) as.character(item$selection_id), character(1))
  } else character()
  if (!all(valid_selection) || anyDuplicated(selection_ids)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Selection records must be valid and uniquely identified.", "selection")
  }
  selection <- lapply(selection, function(item) {
    if (item$hypothesis_status == "confirmed" && (item$selected_with_observations ||
        !triomics_target_string(context$confirmation_protocol))) {
      item$hypothesis_status <- "exploratory"
    }
    item
  })
  list(
    request_id = paste0("target:", identity$namespace, ":", identity$id, ":taxon:", identity$taxon_id),
    namespace = identity$namespace,
    id = identity$id,
    taxon_id = identity$taxon_id,
    entity_type = identity$entity_type,
    constructs = constructs,
    identity_status = if (length(constructs) == 1L) "unique" else if (length(constructs) > 1L) "ambiguous" else "unresolved",
    binding_target = identity$entity_type != "pathway",
    mode = mode,
    selection = selection,
    context = context,
    drug = drug,
    universe = universe
  )
}

triomics_target_external_evidence <- function(request, records) {
  if (!is.list(request) || !triomics_target_string(request$request_id) ||
      !triomics_target_string(request$namespace) || !triomics_target_string(request$id) ||
      !triomics_target_taxon(request$taxon_id) || !triomics_target_string(request$entity_type) ||
      !is.list(records) || is.data.frame(records) || !is.null(names(records))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "A target request and array of assay records are required.", "records")
  }
  collapsed <- function(fields) {
    fields <- tolower(fields)
    any(fields %in% c("composite_score", "support_score", "potency_score", "binding_probability") |
      grepl("^kd($|_)", fields))
  }
  if (collapsed(names(records)) || any(vapply(records, function(record) {
    is.list(record) && collapsed(names(record))
  }, logical(1)))) {
    triomics_d_abort("D_TARGET_AXIS_COLLAPSED", "Collapsed assay fields are not permitted.", "records")
  }
  collapsed_endpoint <- any(vapply(records, function(record) {
    is.list(record) && triomics_target_string(record$endpoint) &&
      tolower(record$endpoint) %in% c("binding_probability", "causal_mediation", "flux")
  }, logical(1)))
  if (collapsed_endpoint) {
    triomics_d_abort("D_TARGET_AXIS_COLLAPSED", "A mechanistic claim label cannot masquerade as an assay endpoint.", "records")
  }
  required <- c("evidence_id", "axis", "endpoint", "units", "relation", "source_id",
                "publication_group", "measured_by_pipeline", "value")
  optional_strings <- c("target_namespace", "target_id", "target_entity_type", "context_id",
                        "construct", "compound_id", "compound_namespace")
  assay_fields <- c("assay", "assay_limitations")
  valid <- vapply(records, function(record) {
    if (!is.list(record) || is.data.frame(record) || is.null(names(record)) ||
        anyDuplicated(names(record)) || !all(required %in% names(record))) return(FALSE)
    strings <- c("evidence_id", "axis", "endpoint", "units", "relation", "source_id", "publication_group")
    assay_fields_valid <- all(vapply(assay_fields[assay_fields %in% names(record)], function(field) {
      triomics_target_string(record[[field]])
    }, logical(1)))
    all(vapply(record[strings], triomics_target_string, logical(1))) &&
      all(vapply(optional_strings[optional_strings %in% names(record)], function(field) {
        triomics_target_string(record[[field]])
      }, logical(1))) &&
      assay_fields_valid &&
      record$axis %in% c("binding", "function", "engagement", "association") &&
      is.logical(record$measured_by_pipeline) && length(record$measured_by_pipeline) == 1L &&
      !is.na(record$measured_by_pipeline) &&
      (is.null(record$value) || (is.numeric(record$value) && length(record$value) == 1L &&
        !is.na(record$value) && is.finite(record$value))) &&
      (is.null(record$taxon_id) || triomics_target_taxon(record$taxon_id))
  }, logical(1))
  if (!all(valid) || anyDuplicated(vapply(records, function(record) as.character(record$evidence_id), character(1)))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Assay records need valid identities and unique evidence IDs.", "records")
  }
  if (any(vapply(records, `[[`, logical(1), "measured_by_pipeline"))) {
    triomics_d_abort("D_TARGET_EXTERNAL_ONLY", "Only external assay records are accepted.", "measured_by_pipeline")
  }

  attach_record <- function(record) {
    target_fields <- c("target_namespace", "target_id", "target_entity_type")
    target_values <- c(request$namespace, request$id, request$entity_type)
    target_mismatch <- any(vapply(seq_along(target_fields), function(index) {
      field <- target_fields[[index]]
      !is.null(record[[field]]) && !identical(record[[field]], target_values[[index]])
    }, logical(1)))
    taxon_mismatch <- !is.null(record$taxon_id) && !identical(as.numeric(record$taxon_id), as.numeric(request$taxon_id))
    context_mismatch <- !is.null(record$context_id) &&
      (is.null(request$context$context_id) || !identical(record$context_id, request$context$context_id))
    construct_mismatch <- !is.null(record$construct) &&
      !record$construct %in% unlist(request$constructs, use.names = FALSE)
    compound_mismatch <- !is.null(record$compound_namespace) && !is.null(request$drug) &&
      !identical(record$compound_namespace, request$drug$namespace) ||
      !is.null(record$compound_id) && !is.null(request$drug) &&
      !identical(record$compound_id, request$drug$drug_id)
    complete_identity <- !any(vapply(target_fields, function(field) !triomics_target_string(record[[field]]), logical(1))) &&
      !is.null(record$taxon_id) && !is.null(record$context_id) &&
      !is.null(request$context$context_id) &&
      length(request$constructs) == 1L && !is.null(record$construct) &&
      !is.null(request$drug) && triomics_target_string(record$compound_namespace) &&
      triomics_target_string(record$compound_id)
    mismatch_status <- if (target_mismatch) "target_mismatch" else if (taxon_mismatch) "taxon_mismatch" else
      if (construct_mismatch) "construct_mismatch" else if (compound_mismatch) "compound_mismatch" else
        if (context_mismatch) "context_mismatch" else NULL
    attachment <- if (record$axis == "binding" && !isTRUE(request$binding_target)) {
      "not_binding_target"
    } else if (!is.null(mismatch_status)) {
      mismatch_status
    } else if (!complete_identity || identical(request$identity_status, "ambiguous")) {
      "unresolved"
    } else {
      "request"
    }
    record$attachment <- attachment
    record$target_identity_status <- if (identical(attachment, "request")) "matched" else
      if (grepl("mismatch$", attachment)) "mismatched" else "unresolved"
    record$dependency_group <- paste0("publication:", record$publication_group)
    record$independent_replication <- FALSE
    record$availability <- if (is.null(record$value)) "unavailable" else "available"
    assay_description_supplied <- triomics_target_string(record$assay)
    assay_limitations_supplied <- triomics_target_string(record$assay_limitations)
    record$assay_qualification <- if (assay_description_supplied && assay_limitations_supplied) {
      "qualified"
    } else {
      "unresolved"
    }
    record$assay_qualification_reason <- NULL
    if (!assay_description_supplied && !assay_limitations_supplied) {
      record$assay_qualification_reason <- "assay_description_and_limitations_missing"
    } else if (!assay_description_supplied) {
      record$assay_qualification_reason <- "assay_description_missing"
    } else if (!assay_limitations_supplied) {
      record$assay_qualification_reason <- "assay_limitations_missing"
    }
    record
  }
  attached <- lapply(records, attach_record)
  group_ids <- if (length(attached)) unique(vapply(attached, `[[`, character(1), "dependency_group")) else character()
  list(request_id = request$request_id, records = attached,
       independent_group_count = as.integer(length(group_ids)))
}
