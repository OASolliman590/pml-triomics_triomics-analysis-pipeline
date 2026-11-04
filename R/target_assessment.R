triomics_target_expected_status <- function(effect, expected_direction) {
  if (!is.numeric(effect) || length(effect) != 1L || is.na(effect) || !is.finite(effect) ||
      !triomics_target_string(expected_direction) ||
      !expected_direction %in% c("positive", "negative")) return("uninformative")
  if (effect == 0) return("uninformative")
  observed <- if (effect > 0) "positive" else "negative"
  if (identical(observed, expected_direction)) "support" else "conflict"
}

triomics_target_taxon_token <- function(value) {
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    if (!identical(value, trimws(value)) || !grepl("^[1-9][0-9]*$", value)) return(NULL)
    return(value)
  }
  if (is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) &&
      value > 0 && value == floor(value) && value <= 9007199254740991) {
    return(format(value, scientific = FALSE, trim = TRUE, digits = 22L))
  }
  NULL
}

triomics_target_safe_taxon_value <- function(value) {
  if (is.null(value) || (is.atomic(value) && length(value) == 1L && is.na(value))) NULL else value
}

triomics_target_feature_taxon <- function(feature, request_taxon_id) {
  routing_metadata <- if (is.list(feature$provenance)) feature$provenance$routing_metadata else NULL
  declaration <- if (is.list(routing_metadata)) routing_metadata$taxon else NULL
  declaration <- if (is.list(declaration)) declaration else NULL
  declaration_status <- if (is.list(declaration)) declaration$status else NULL
  adapted_status <- feature$taxon_status
  declaration_copy <- declaration
  if (is.list(declaration_copy) && "value" %in% names(declaration_copy)) {
    declaration_copy["value"] <- list(triomics_target_safe_taxon_value(declaration_copy$value))
  }
  declaration_status_valid <- triomics_target_string(declaration_status) &&
    declaration_status %in% c("declared", "unknown")
  provenance_ref <- if (is.list(declaration) && triomics_target_string(declaration$provenance_ref)) {
    declaration$provenance_ref
  } else NULL
  feature_taxon_id <- if (identical(declaration_status, "declared") && is.list(declaration)) {
    triomics_target_safe_taxon_value(declaration$value)
  } else NULL
  result <- list(
    applicability = "unassessable", reason_code = "feature_taxon_unknown",
    status = if (declaration_status_valid) declaration_status else "unknown",
    taxon_id = feature_taxon_id, declaration = declaration_copy,
    provenance_ref = provenance_ref
  )

  if (is.null(declaration)) return(result)
  if (!declaration_status_valid) {
    result$reason_code <- "feature_taxon_malformed"
    return(result)
  }
  if (identical(declaration_status, "unknown") || identical(adapted_status, "unknown")) return(result)
  if (!is.null(adapted_status) && !identical(adapted_status, "declared")) {
    result$reason_code <- "feature_taxon_malformed"
    return(result)
  }
  if (!identical(declaration$value, feature$taxon_id)) {
    result$reason_code <- "feature_taxon_malformed"
    return(result)
  }

  feature_token <- triomics_target_taxon_token(declaration$value)
  if (is.null(feature_token)) {
    unknown_value <- is.null(declaration$value) ||
      (is.atomic(declaration$value) && length(declaration$value) == 1L && is.na(declaration$value)) ||
      (is.character(declaration$value) && length(declaration$value) == 1L &&
       identical(tolower(trimws(declaration$value)), "unknown"))
    result$reason_code <- if (unknown_value) "feature_taxon_unknown" else "feature_taxon_malformed"
    return(result)
  }
  request_token <- triomics_target_taxon_token(request_taxon_id)
  if (is.null(request_token)) {
    result$reason_code <- "feature_taxon_malformed"
    return(result)
  }
  if (!identical(feature_token, request_token)) {
    result$applicability <- "inapplicable"
    result$reason_code <- "feature_taxon_mismatch"
    return(result)
  }
  result$applicability <- "matched"
  result$reason_code <- NULL
  result
}

triomics_target_resolve_observations <- function(request, links, evidence) {
  if (!is.list(request) || !triomics_target_string(request$request_id) ||
      !is.list(links) || is.data.frame(links) || !is.list(evidence) ||
      !identical(evidence$artifact_type, "evidence_bundle") ||
      !triomics_target_string(evidence$study_id) || !triomics_target_string(evidence$analysis_id) ||
      !is.list(evidence$feature_registry) || !is.list(evidence$source_by_id) ||
      !is.list(evidence$tested_results) || !is.list(evidence$nominations) ||
      !triomics_sha256_text(as.character(evidence$immutable_inputs$b_bundle_sha256)) ||
      !triomics_sha256_text(as.character(evidence$immutable_inputs$c_manifest_sha256))) {
    triomics_d_abort("D_TARGET_OBSERVATION_LINEAGE_INVALID",
                     "Target observations require a verified same-run B/C evidence bundle.", "evidence")
  }
  observations <- list()
  for (link in links) {
    if (!is.list(link) || !triomics_target_string(link$link_id) ||
        !triomics_target_string(link$feature_namespace) || !triomics_target_string(link$feature_id)) {
      triomics_d_abort("D_TARGET_PATH_UNDOCUMENTED",
                       "A target observation path needs an exact measured feature identity.", "links")
    }
    feature_candidate_record <- function(feature) {
      feature_taxon <- triomics_target_feature_taxon(feature, request$taxon_id)
      source_id <- if (triomics_target_string(feature$source_id)) as.character(feature$source_id) else ""
      source <- evidence$source_by_id[[source_id]]
      feature_key <- if (triomics_target_string(feature$feature_key)) as.character(feature$feature_key) else ""
      list(
        feature_key = feature_key, feature_namespace = as.character(feature$id_namespace),
        feature_id = as.character(feature$original_id), source_id = source_id,
        source_sha256 = if (is.list(source)) source$sha256 else NULL,
        source_selection_history = if (is.list(source)) source$selection_history %||% NULL else NULL,
        source_selection_evidence = if (is.list(source)) source$selection_evidence %||% NULL else NULL,
        request_taxon_id = request$taxon_id,
        feature_taxon_id = feature_taxon$taxon_id,
        feature_taxon_status = feature_taxon$status,
        feature_taxon_declaration = feature_taxon$declaration,
        feature_taxon_provenance_ref = feature_taxon$provenance_ref,
        taxon_applicability = feature_taxon$applicability,
        taxon_reason_code = feature_taxon$reason_code,
        dependency_group = paste0("molecular_feature:", feature_key)
      )
    }
    add_unmeasured <- function(status, reason, features = list()) {
      candidates <- lapply(features, feature_candidate_record)
      candidate <- if (length(candidates) == 1L) candidates[[1L]] else NULL
      observations[[length(observations) + 1L]] <<- list(
        observation_id = paste0("unmeasured:", link$link_id), link_id = link$link_id,
        endpoint = "molecular_measurement", value = NULL, effect = NULL, effect_unit = NULL,
        status = status, reason = reason, feature_identity_status = status,
        feature_key = if (is.null(candidate)) NULL else candidate$feature_key,
        feature_namespace = if (is.null(candidate)) NULL else candidate$feature_namespace,
        feature_id = if (is.null(candidate)) NULL else candidate$feature_id,
        source_id = if (is.null(candidate)) NULL else candidate$source_id,
        source_sha256 = if (is.null(candidate)) NULL else candidate$source_sha256,
        source_selection_history = if (is.null(candidate)) NULL else candidate$source_selection_history,
        source_selection_evidence = if (is.null(candidate)) NULL else candidate$source_selection_evidence,
        request_taxon_id = request$taxon_id,
        feature_taxon_id = if (is.null(candidate)) NULL else candidate$feature_taxon_id,
        feature_taxon_status = if (is.null(candidate)) NULL else candidate$feature_taxon_status,
        feature_taxon_declaration = if (is.null(candidate)) NULL else candidate$feature_taxon_declaration,
        feature_taxon_provenance_ref = if (is.null(candidate)) NULL else candidate$feature_taxon_provenance_ref,
        taxon_applicability = if (is.null(candidate)) {
          if (length(candidates)) "unassessable" else NULL
        } else candidate$taxon_applicability,
        taxon_reason_code = if (is.null(candidate)) {
          if (length(candidates)) "duplicate_molecular_identity" else NULL
        } else candidate$taxon_reason_code,
        feature_candidates = candidates,
        dependency_group = if (is.null(candidate)) paste0("link:", link$link_id) else candidate$dependency_group,
        source_branch = "none", analysis_id = evidence$analysis_id, study_id = evidence$study_id,
        path_context = link$context_id, path_sign = link$sign,
        interpretation = "descriptive_compatibility_only"
      )
    }
    if (!identical(as.character(link$context_id), as.character(request$context$context_id))) {
      add_unmeasured("unmeasured", "context_mismatch")
      next
    }
    matching_features <- Filter(function(feature) {
      identical(as.character(feature$id_namespace), as.character(link$feature_namespace)) &&
        identical(as.character(feature$original_id), as.character(link$feature_id))
    }, evidence$feature_registry)
    if (!length(matching_features)) {
      add_unmeasured("unmeasured", "feature_not_measured")
      next
    }
    if (length(matching_features) != 1L) {
      add_unmeasured("ambiguous", "duplicate_molecular_identity", matching_features)
      next
    }
    feature <- matching_features[[1L]]
    feature_key <- as.character(feature$feature_key)
    source <- evidence$source_by_id[[as.character(feature$source_id)]]
    if (!triomics_target_string(feature_key) || !is.list(source) ||
        !identical(as.character(source$source_id), as.character(feature$source_id)) ||
        !triomics_sha256_text(as.character(source$sha256))) {
      triomics_d_abort("D_TARGET_OBSERVATION_LINEAGE_INVALID",
                       "A target observation feature lacks its immutable source identity.", feature_key)
    }
    feature_taxon <- triomics_target_feature_taxon(feature, request$taxon_id)
    taxon_fields <- list(
      request_taxon_id = request$taxon_id,
      feature_taxon_id = feature_taxon$taxon_id,
      feature_taxon_status = feature_taxon$status,
      feature_taxon_declaration = feature_taxon$declaration,
      feature_taxon_provenance_ref = feature_taxon$provenance_ref,
      taxon_applicability = feature_taxon$applicability,
      taxon_reason_code = feature_taxon$reason_code
    )
    matched_rows <- Filter(function(item) {
      is.list(item$row) && identical(as.character(item$row$feature_key), feature_key)
    }, evidence$tested_results)
    for (item in matched_rows) {
      row <- item$row
      result_id <- as.character(row$result_id %||% "")
      effect_available <- identical(as.character(row$estimation_status), "estimated") &&
        is.numeric(row$effect) && length(row$effect) == 1L && !is.na(row$effect) && is.finite(row$effect)
      value <- if (effect_available) row$effect else NULL
      observations[[length(observations) + 1L]] <- c(list(
        observation_id = paste0("B:", result_id), result_id = result_id,
        source_branch = "primary_statistics", analysis_id = evidence$analysis_id,
        study_id = evidence$study_id, block_id = as.character(item$block_id),
        claim_id = as.character(item$claim_id), contrast_id = as.character(row$contrast_id %||% ""),
        family_id = as.character(row$family_id %||% ""), feature_key = feature_key,
        feature_namespace = as.character(feature$id_namespace), feature_id = as.character(feature$original_id),
        source_id = as.character(feature$source_id), source_sha256 = as.character(source$sha256),
        source_selection_history = source$selection_history %||% NULL,
        source_selection_evidence = source$selection_evidence %||% NULL,
        endpoint = "statistical_effect", effect = value, value = value,
        effect_unit = as.character(row$effect_unit %||% ""),
        status = if (!effect_available) "unmeasured" else if (!identical(feature_taxon$applicability, "matched")) {
          "uninformative"
        } else triomics_target_expected_status(value, link$expected_direction),
        dependency_group = paste0("molecular_feature:", feature_key),
        link_id = link$link_id, path_context = link$context_id, path_sign = link$sign,
        interpretation = "descriptive_compatibility_only"
      ), taxon_fields)
    }
    c_nominations <- Filter(function(nomination) {
      identical(as.character(nomination$source_branch), "integration_selection") &&
        identical(as.character(nomination$feature_key), feature_key)
    }, evidence$nominations)
    for (nomination in c_nominations) {
      resolved <- triomics_d_resolve_integration_nomination(evidence$c$integration_evidence, nomination)
      loadings <- resolved$selected_value$component_loadings
      if (!is.list(loadings) || !length(loadings)) {
        triomics_d_abort("D_TARGET_OBSERVATION_LINEAGE_INVALID",
                         "A C-06 target nomination lacks its verified component-loading values.",
                         as.character(nomination$nomination_id))
      }
      for (component_id in names(loadings)) {
        loading <- loadings[[component_id]]
        if (!is.numeric(loading) || length(loading) != 1L || is.na(loading) || !is.finite(loading)) next
        observations[[length(observations) + 1L]] <- c(list(
          observation_id = paste0("C:", as.character(nomination$nomination_id), ":", component_id),
          source_branch = "integration_selection", analysis_id = evidence$analysis_id,
          study_id = evidence$study_id, nomination_id = as.character(nomination$nomination_id),
          integration_id = as.character(nomination$result_ref$integration_id),
          component_id = component_id, feature_key = feature_key,
          feature_namespace = as.character(feature$id_namespace), feature_id = as.character(feature$original_id),
          source_id = as.character(feature$source_id), source_sha256 = as.character(source$sha256),
          source_selection_history = source$selection_history %||% NULL,
          source_selection_evidence = source$selection_evidence %||% NULL,
          endpoint = "integration_component_loading", effect = NULL, value = loading,
          effect_unit = "loading", status = "uninformative",
          selection_rule = nomination$selection_rule,
          selection_value = nomination$selection_value,
          dependency_group = paste0("molecular_feature:", feature_key),
          link_id = link$link_id, path_context = link$context_id, path_sign = link$sign,
          interpretation = "descriptive_compatibility_only"
        ), taxon_fields)
      }
    }
    if (!length(matched_rows) && !length(c_nominations)) {
      add_unmeasured("unmeasured", "no_same_run_B_or_C_result", list(feature))
    }
  }
  unname(observations)
}

triomics_target_external_axis <- function(records, axis) {
  matched <- Filter(function(record) identical(as.character(record$axis), axis), records)
  publication_groups <- if (length(matched)) {
    vapply(matched, function(record) {
      as.character(record$dependency_group %||% record$publication_group %||% "")
    }, character(1))
  } else character()
  coverage <- list(
    expected_records = as.integer(length(matched)),
    measured_records = 0L,
    unmeasured_records = 0L,
    unresolved_records = 0L,
    independent_publication_groups = as.integer(length(unique(publication_groups[nzchar(publication_groups)])))
  )
  if (!length(matched)) {
    return(list(status = "unmeasured", reason_code = "D_TARGET_EVIDENCE_ABSENT",
                coverage = coverage, evidence = list()))
  }
  attached <- vapply(matched, function(record) identical(record$attachment, "request"), logical(1))
  assay_qualified <- vapply(matched, function(record) {
    identical(record$assay_qualification, "qualified")
  }, logical(1))
  available <- vapply(matched, function(record) identical(record$availability, "available"), logical(1))
  applicable <- attached & assay_qualified
  coverage$measured_records <- as.integer(sum(applicable & available))
  coverage$unmeasured_records <- as.integer(sum(applicable & !available))
  coverage$unresolved_records <- as.integer(sum(!applicable))
  status <- if (any(applicable & available)) "measured" else if (any(!applicable)) "unresolved" else "unmeasured"
  reason_code <- if (!identical(status, "unresolved")) NULL else if (any(!attached)) {
    "D_TARGET_IDENTITY_UNRESOLVED"
  } else {
    "D_TARGET_ASSAY_QUALIFICATION_UNRESOLVED"
  }
  list(status = status, reason_code = reason_code,
       coverage = coverage, evidence = matched)
}

triomics_target_profile <- function(request, links, observations, external_evidence = NULL) {
  invalid <- function(message, field) triomics_d_abort("D_TARGET_INPUT_INVALID", message, field)
  if (!is.list(request) || !triomics_target_string(request$request_id) ||
      !triomics_target_string(request$namespace) || !triomics_target_string(request$id) ||
      !triomics_target_taxon(request$taxon_id) || !triomics_target_string(request$entity_type)) {
    invalid("A valid target request is required.", "request")
  }
  mechanism_class <- request$mechanism_class %||% "unknown"
  mechanism_status <- if (mechanism_class %in% c("non_enzymatic", "enzymatic")) "assessable" else "unassessable"
  if (!is.list(links) || is.data.frame(links) || !is.list(observations) || is.data.frame(observations)) {
    invalid("Links and observations must be arrays of records.", "links")
  }
  collapsed <- function(x) {
    if (!is.list(x)) return(FALSE)
    any(tolower(names(x)) %in% c("kd", "binding_probability", "flux", "causal_mediation", "binding", "activity")) ||
      any(vapply(x, collapsed, logical(1)))
  }
  if (collapsed(request) || collapsed(links) || collapsed(observations)) {
    triomics_d_abort("D_TARGET_CLAIM_COLLAPSED", "Inferred binding, flux or causal claims are not permitted.", "observations")
  }
  link_ids <- vapply(links, function(link) {
    if (!is.list(link) || !triomics_target_string(link$link_id) ||
        !triomics_target_string(link$relation) ||
        !link$relation %in% c("regulatory", "perturbational", "enzymatic_reaction") ||
        !triomics_target_string(link$sign) ||
        !link$sign %in% c("positive", "negative", "unsigned", "unknown") ||
        !triomics_target_string(link$context_id) || !triomics_target_string(link$source_id)) {
      triomics_d_abort("D_TARGET_PATH_UNDOCUMENTED", "Each path needs a relation, sign, context and source.", "links")
    }
    if (identical(mechanism_class, "non_enzymatic") && link$relation == "enzymatic_reaction") {
      triomics_d_abort("D_TARGET_REACTION_INVENTED", "A non-enzymatic target cannot have an enzymatic reaction path.", "links")
    }
    link$link_id
  }, character(1))
  if (anyDuplicated(link_ids)) invalid("Link identifiers must be unique.", "links")
  status_values <- c("support", "conflict", "ambiguous", "uninformative", "unmeasured")
  taxon_applicability_values <- c("matched", "unassessable", "inapplicable")
  rows <- lapply(observations, function(observation) {
    if (!is.list(observation) || !triomics_target_string(observation$link_id) ||
        !observation$link_id %in% link_ids) {
      triomics_d_abort("D_TARGET_PATH_UNDOCUMENTED", "Each observation requires a matching documented path.", "observations")
    }
    if (!triomics_target_string(observation$observation_id) || !triomics_target_string(observation$endpoint) ||
        !triomics_target_string(observation$dependency_group) || !triomics_target_string(observation$status) ||
        !observation$status %in% status_values ||
        !(is.null(observation$value) || (is.numeric(observation$value) && length(observation$value) == 1L &&
          !is.na(observation$value) && is.finite(observation$value)))) {
      invalid("Each observation must resolve to one documented path and retain a typed value/status.", "observations")
    }
    if (tolower(observation$endpoint) %in% c("binding", "flux", "activity", "causal_mediation", "kd", "binding_probability")) {
      triomics_d_abort("D_TARGET_CLAIM_COLLAPSED", "Observation endpoints cannot be converted to mechanistic claims.", "observations")
    }
    link <- links[[match(observation$link_id, link_ids)]]
    if (!is.null(observation$taxon_applicability) &&
        (!triomics_target_string(observation$taxon_applicability) ||
         !observation$taxon_applicability %in% taxon_applicability_values)) {
      invalid("Feature taxon applicability must be matched, unassessable, or inapplicable.", "observations")
    }
    if (triomics_target_string(observation$feature_key) && is.null(observation$taxon_applicability)) {
      observation$request_taxon_id <- request$taxon_id
      observation$feature_taxon_id <- NULL
      observation$feature_taxon_status <- "unknown"
      observation$feature_taxon_declaration <- NULL
      observation$feature_taxon_provenance_ref <- NULL
      observation$taxon_applicability <- "unassessable"
      observation$taxon_reason_code <- "feature_taxon_unknown"
    }
    observation$path_sign <- link$sign
    observation$path_context <- link$context_id
    observation$coverage <- if (identical(observation$taxon_applicability, "inapplicable")) {
      "inapplicable"
    } else if (identical(observation$taxon_applicability, "unassessable")) {
      "unassessable"
    } else if (is.null(observation$value)) {
      "unmeasured"
    } else {
      "measured"
    }
    observation
  })
  external_records <- list()
  if (!is.null(external_evidence)) {
    if (!is.list(external_evidence) || is.data.frame(external_evidence) ||
        !identical(as.character(external_evidence$request_id), as.character(request$request_id)) ||
        !is.list(external_evidence$records) || is.data.frame(external_evidence$records) ||
        !is.null(names(external_evidence$records))) {
      invalid("External assay evidence must be a typed ledger for the actual target request.", "external_evidence")
    }
    external_ledger <- triomics_target_external_evidence(request, external_evidence$records)
    external_records <- external_ledger$records
  }
  if (length(external_records) && !all(vapply(external_records, function(record) {
    is.list(record) && triomics_target_string(record$axis) &&
      record$axis %in% c("binding", "function", "engagement", "association")
  }, logical(1)))) {
    invalid("External evidence must preserve its declared assay axis.", "external_evidence")
  }
  by_feature <- vapply(rows, function(observation) {
    if (triomics_target_string(observation$feature_key)) return(observation$feature_key)
    paste0("link:", observation$link_id)
  }, character(1))
  applicable_measurements <- vapply(rows, function(observation) {
    !is.null(observation$value) &&
      (is.null(observation$taxon_applicability) || identical(observation$taxon_applicability, "matched"))
  }, logical(1))
  taxon_unassessable <- any(vapply(rows, function(observation) {
    !is.null(observation$taxon_applicability) &&
      observation$taxon_applicability %in% c("unassessable", "inapplicable")
  }, logical(1)))
  measured_feature_count <- length(unique(by_feature[applicable_measurements]))
  unmeasured_feature_count <- length(unique(by_feature[!applicable_measurements]))
  dependency_groups <- if (length(rows)) unique(vapply(rows, `[[`, character(1), "dependency_group")) else character()
  compatibility_status <- if (identical(mechanism_status, "unassessable")) "unassessable" else
    if (taxon_unassessable) "unassessable" else
      if (!length(links) || !length(rows) || measured_feature_count == 0L) "unmeasured" else "measured"
  binding <- triomics_target_external_axis(external_records, "binding")
  function_axis <- triomics_target_external_axis(external_records, "function")
  engagement <- triomics_target_external_axis(external_records, "engagement")
  axes <- list(
    metabolic_multiomics_compatibility = list(
      status = compatibility_status,
      reason_code = if (!identical(compatibility_status, "unassessable")) NULL else if (identical(mechanism_status, "unassessable")) {
        "D_TARGET_MECHANISM_UNASSESSABLE"
      } else {
        "D_TARGET_FEATURE_TAXON_UNASSESSABLE"
      },
      coverage = list(expected_links = as.integer(length(links)),
                      measured_features = as.integer(measured_feature_count),
                      unmeasured_features = as.integer(unmeasured_feature_count)),
      links = links, observations = rows,
      independent_group_count = as.integer(length(dependency_groups))
    ),
    functional_perturbation = function_axis,
    cellular_tissue_engagement = engagement,
    direct_binding = binding
  )
  list(
    request_id = request$request_id, target_id = request$id, request = request,
    mechanism_class = if (mechanism_status == "assessable") mechanism_class else NULL,
    mechanism_status = mechanism_status, links = links, observations = rows,
    external_evidence = external_records,
    external_associations = Filter(function(record) identical(record$axis, "association"), external_records),
    axes = names(axes), evidence_axes = axes,
    coverage = if (identical(compatibility_status, "measured")) "measured" else "unmeasured",
    independent_count = as.integer(length(dependency_groups)), selected_target = NULL
  )
}

triomics_target_compose_assessment <- function(existing, investigation) {
  if (!is.list(existing)) existing <- list()
  investigation <- investigation %||% list()
  if (!isTRUE(investigation$enabled)) return(existing)
  records <- existing$results %||% list()
  if (!is.list(records) || is.data.frame(records)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Prepared target evidence must remain an array of target records.", "results")
  }
  profiles <- lapply(records, function(record) {
    if (!is.list(record) || !is.list(record$request) || !is.list(record$external_evidence)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "A prepared target record lacks its request or assay ledger.", "results")
    }
    profile <- triomics_target_profile(record$request, record$links %||% list(),
                                       record$observations %||% list(),
                                       external_evidence = record$external_evidence)
    profile$mechanism_declaration_status <- as.character(record$mechanism_status %||% "unassessable")
    profile$mechanism_declarations <- record$mechanism_declarations %||% list()
    profile$input_provenance <- record$input_provenance %||% list()
    profile
  })
  alternatives <- if (length(profiles) >= 2L) triomics_target_alternatives(profiles) else NULL
  existing$profiles <- profiles
  existing$alternatives <- alternatives
  existing$selected_target <- NULL
  existing$tie_status <- if (is.null(alternatives)) NULL else alternatives$tie_status
  existing$mechanism_status <- if (length(profiles) &&
      all(vapply(profiles, function(profile) identical(profile$mechanism_status, "assessable"), logical(1)))) {
    "supplied"
  } else if (any(vapply(profiles, function(profile) identical(profile$mechanism_status, "assessable"), logical(1)))) {
    "partially_assessable"
  } else "unassessable"
  existing
}

triomics_target_merge_protein_metabolites <- function(report, target_investigation) {
  if (!is.list(report) || !is.list(report$proteins) || !is.list(target_investigation)) return(report)
  profiles <- target_investigation$profiles %||% list()
  for (index in seq_along(report$proteins)) {
    protein <- report$proteins[[index]]
    matching <- Filter(function(profile) {
      identity <- profile$request
      identical(as.character(identity$namespace), as.character(protein$namespace)) &&
        identical(as.character(identity$id), as.character(protein$id)) &&
        identical(as.numeric(identity$taxon_id), as.numeric(protein$taxon_id)) &&
        identical(as.character(identity$entity_type), as.character(protein$entity_type))
    }, profiles)
    if (length(matching) != 1L) next
    records <- Filter(function(record) identical(record$axis, "binding"), matching[[1L]]$external_evidence)
    if (!length(records)) next
    attached <- any(vapply(records, function(record) identical(record$attachment, "request"), logical(1)))
    protein$binding_status <- if (attached) "external_evidence_recorded" else "unresolved"
    protein$binding_reason <- if (attached) NULL else "D_TARGET_IDENTITY_UNRESOLVED"
    protein$binding_evidence <- records
    report$proteins[[index]] <- protein
  }
  report
}
