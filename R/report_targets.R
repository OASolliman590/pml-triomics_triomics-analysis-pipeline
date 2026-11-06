triomics_report_target_field <- function(record, name) {
  if (!is.list(record) || is.data.frame(record)) return(NULL)
  record[[name, exact = TRUE]]
}

triomics_report_target_records <- function(value) {
  if (is.null(value)) return(list())
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(index) {
      as.list(value[index, , drop = FALSE])
    }))
  }
  if (is.list(value)) return(unname(value))
  list()
}

triomics_report_target_text <- function(value, default = "") {
  if (is.null(value) || !length(value) || !is.null(dim(value))) return(default)
  if (is.list(value)) {
    scalar_items <- vapply(value, function(item) {
      is.atomic(item) && length(item) == 1L && is.null(dim(item))
    }, logical(1))
    if (!all(scalar_items)) return(default)
    values <- vapply(value, function(item) {
      if (is.na(item)) NA_character_ else as.character(item)
    }, character(1))
  } else if (is.atomic(value)) {
    values <- as.character(value)
  } else {
    return(default)
  }
  values <- values[!is.na(values)]
  if (!length(values)) return(default)
  paste(triomics_report_clean_text(values), collapse = "; ")
}

triomics_report_target_safe_association_token <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) return("")
  value <- trimws(value)
  if (!nzchar(value) || nchar(value, type = "bytes") > 128L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]{0,127}$", value, perl = TRUE) ||
      triomics_report_contains_private_text(value)) return("")
  value
}

triomics_report_target_safe_association_prose <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) return("")
  value <- trimws(value)
  identifier_separator <- "[[:space:]_,.;:/#=()'’\"-]"
  identifier_value <- "[[:alnum:]][[:alnum:]_.-]*"
  # Treat identifier connectors as token content: the S in SubjectS-7 is attached,
  # while the final S in SUBJECTS count is a complete plural label.
  identifier_token_character <- "[[:alnum:]_.:/#=-]"
  plural_label_suffix <- paste0(
    "(?!(?i:s)(?!", identifier_token_character, "))"
  )
  person_label <- paste0(
    "(?i:(?:\\b|_)(?:subject|patient|participant|mrn)", plural_label_suffix,
    "(?=(?:",
    identifier_separator,
    "|id|identifier|number|no\\.?|[[:digit:]]|(?-i:[A-Z]))))"
  )
  medical_record_label <- paste0(
    "(?i:(?:\\b|_)medical[[:space:]_-]*record", plural_label_suffix,
    "(?=(?:",
    identifier_separator,
    "|id|identifier|number|no\\.?|[[:digit:]]|(?-i:[A-Z]))))"
  )
  sample_label <- paste0(
    "(?i:(?:\\b|_)sample", plural_label_suffix, "(?=(?:",
    identifier_separator,
    "|id|identifier|number|no\\.?|[[:digit:]]|(?-i:[A-Z]))))"
  )
  lineage_descriptor <- paste0(
    "(?!(?i:(?:used|in|by|for|with|from|of|the|same|and|or|to|at|as|is|are|was|were))\\b)",
    "[[:alpha:]][[:alnum:]_-]*"
  )
  sample_operation_lineage <- paste0(
    "(?i:(?:", lineage_descriptor, "[[:space:]]+)*",
    "[[:alpha:]][[:alnum:]_-]*(?:ing|tion|sion)",
    "[[:space:]]+of[[:space:]]+[[:digit:]]+",
    "[[:space:]]+(?:molecular[[:space:]]+)?(?:measurements?|features?))"
  )
  sample_score_construction <- paste0(
    "(?i:(?:[[:alpha:]][[:alpha:]_-]*[[:space:]]+)*",
    "score[[:space:]]+constructed[[:space:]]+from[[:space:]]+[[:digit:]]+",
    "[[:space:]]+(?:molecular[[:space:]]+)?features?)"
  )
  sample_top_selection <- paste0(
    "(?i:top[[:space:]]+[[:digit:]]+[[:space:]]+features?",
    "[[:space:]]+selected[[:space:]]+in[[:space:]]+(?:the[[:space:]]+)?same[[:space:]]+study)"
  )
  sample_scientific_description <- paste0(
    "(?:", sample_operation_lineage, "|",
    sample_score_construction, "|", sample_top_selection, ")"
  )
  identifier_label <- paste0("(?:", person_label, "|", medical_record_label, ")")
  qualified_identifier <- paste0(
    identifier_separator, "*(?i:id|identifier|number|no\\.?)",
    "(?:", identifier_separator, "+", identifier_value,
    "|(?=[[:digit:]]|(?-i:[A-Z]))", identifier_value, ")"
  )
  attached_identifier <- paste0(
    "(?=[[:digit:]]|(?-i:[A-Z]))", identifier_value
  )
  numeric_identifier <- paste0(
    identifier_separator, "*(?=[[:alnum:]_.-]*[[:digit:]])", identifier_value
  )
  punctuated_identifier <- paste0(
    "(?:[[:space:]]*[:=#/._-][[:space:]]*|[[:space:]]+['’\"()_-][[:space:]]*)",
    "(?=[[:digit:]]|(?-i:[A-Z]))",
    identifier_value
  )
  sample_punctuated_identifier <- paste0(
    "(?:[[:space:]]*[:=#/._-][[:space:]]*|[[:space:]]+['’\"()_-][[:space:]]*)",
    # Exempt only this local description shape; identifier matches still scan all of value.
    "(?!(?:", sample_scientific_description, ")(?![[:alnum:]_.-]))",
    "(?!(?i:(?:level|wise|based))(?![[:alnum:]_.-]))",
    "(?!(?i:(?:median|mean|quantile)[[:space:]]+normalization)(?![[:alnum:]_.-]))",
    "(?=[[:digit:]]|(?-i:[A-Z]))",
    identifier_value
  )
  strong_identifier_value <- paste0(
    "(?i:(?:\\b|_)(?:mrn|medical[[:space:]_-]*record)", plural_label_suffix, "\\b)",
    identifier_separator, "+", identifier_value
  )
  identifier_pattern <- paste0(
    "(?:", identifier_label, "(?:",
    qualified_identifier, "|", attached_identifier, "|", numeric_identifier, "|",
    punctuated_identifier,
    ")|", sample_label, "(?:",
    qualified_identifier, "|", attached_identifier, "|", numeric_identifier, "|",
    sample_punctuated_identifier,
    ")|", strong_identifier_value, ")"
  )
  # Embedded labels cannot use the plural exception: the left side is part of the
  # same identifier token, even when the suffix is followed by whitespace or end.
  legacy_identifier_pattern <- paste0(
    "(?i:subject|patient|participant)[_.:-][A-Za-z0-9]+|",
    identifier_token_character, "(?i:subject|patient|participant)(?=[A-Z0-9])|",
    "(?i:(?:subject|patient|participant)[[:space:]]+(?:id[[:space:]_.:-]*)?[0-9]+)"
  )
  if (!nzchar(value) || nchar(value, type = "bytes") > 512L ||
      grepl("[[:cntrl:]<>`]", value, perl = TRUE) ||
      triomics_report_contains_private_text(value) ||
      grepl(identifier_pattern, value, perl = TRUE) ||
      grepl(legacy_identifier_pattern, value, perl = TRUE)) {
    return("")
  }
  token <- triomics_report_target_safe_association_token(value)
  if (nzchar(token)) return(token)
  if (!grepl("^[[:alpha:]][[:alpha:][:digit:][:space:],.;:'’():-]*$", value, perl = TRUE)) return("")
  triomics_report_clean_text(value)
}

triomics_report_target_number <- function(value) {
  if (is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value)) {
    return(as.numeric(value))
  }
  NA_real_
}

triomics_report_target_logical <- function(value) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
  NA
}

triomics_report_target_frame <- function(rows, columns,
                                         numeric_columns = character(),
                                         logical_columns = character()) {
  if (!length(rows)) {
    return(triomics_report_empty_frame(columns, numeric_columns = numeric_columns,
                                       logical_columns = logical_columns))
  }
  output <- setNames(lapply(columns, function(column) {
    if (column %in% numeric_columns) {
      return(vapply(rows, function(row) triomics_report_target_number(row[[column]]), numeric(1)))
    }
    if (column %in% logical_columns) {
      return(vapply(rows, function(row) triomics_report_target_logical(row[[column]]), logical(1)))
    }
    vapply(rows, function(row) triomics_report_target_text(row[[column]]), character(1))
  }), columns)
  as.data.frame(output, stringsAsFactors = FALSE, check.names = FALSE)
}

triomics_report_target_axis_definitions <- function() {
  list(
    metabolic_multiomics_compatibility = list(
      label = "Metabolic and multi-omics compatibility",
      note = "Descriptive compatibility does not establish activity, flux, mediation, or binding."
    ),
    functional_perturbation = list(
      label = "Functional perturbation",
      note = "A perturbation endpoint is distinct from target engagement and direct binding."
    ),
    cellular_tissue_engagement = list(
      label = "Cellular and tissue target engagement",
      note = "Assay context and limitations govern what engagement evidence supports."
    ),
    direct_binding = list(
      label = "Direct binding",
      note = "Source-reported quantities are retained as given; no affinity conversion is made."
    )
  )
}

triomics_report_target_identity <- function(profile) {
  request <- triomics_report_target_field(profile, "request")
  list(
    target_id = triomics_report_target_text(
      triomics_report_target_field(profile, "target_id") %||%
        triomics_report_target_field(request, "id")
    ),
    target_namespace = triomics_report_target_text(triomics_report_target_field(request, "namespace")),
    target_taxon_id = triomics_report_target_field(request, "taxon_id"),
    target_entity_type = triomics_report_target_text(triomics_report_target_field(request, "entity_type")),
    mode = triomics_report_target_text(triomics_report_target_field(request, "mode")),
    mechanism_class = triomics_report_target_text(
      triomics_report_target_field(profile, "mechanism_class") %||%
        triomics_report_target_field(request, "mechanism_class"), "unknown"
    ),
    mechanism_status = triomics_report_target_text(
      triomics_report_target_field(profile, "mechanism_status"), "unassessable"
    ),
    mechanism_declaration_status = triomics_report_target_text(
      triomics_report_target_field(profile, "mechanism_declaration_status"), "unassessable"
    )
  )
}

triomics_report_target_profile_rows <- function(profiles) {
  lapply(profiles, function(profile) {
    identity <- triomics_report_target_identity(profile)
    request <- triomics_report_target_field(profile, "request")
    drug <- triomics_report_target_field(request, "drug")
    context <- triomics_report_target_field(request, "context")
    observations <- triomics_report_target_records(
      triomics_report_target_field(profile, "observations")
    )
    observed_studies <- unique(Filter(nzchar, vapply(observations, function(observation) {
      triomics_report_target_text(triomics_report_target_field(observation, "study_id"))
    }, character(1))))
    external_evidence <- triomics_report_target_records(
      triomics_report_target_field(profile, "external_evidence")
    )
    associations <- Filter(function(record) {
      identical(triomics_report_target_field(record, "axis"), "association")
    }, external_evidence)
    association_studies <- unique(Filter(nzchar, vapply(associations, function(record) {
      triomics_report_target_safe_association_token(
        triomics_report_target_field(record, "study_id")
      )
    }, character(1))))
    observed_studies <- unique(c(observed_studies, association_studies))
    groups <- unique(Filter(nzchar, vapply(observations, function(observation) {
      triomics_report_target_text(triomics_report_target_field(observation, "dependency_group"))
    }, character(1))))
    discovery <- triomics_report_target_field(profile, "discovery")
    overlap <- triomics_report_target_safe_association_token(
      triomics_report_target_field(profile, "subject_overlap_status") %||%
        triomics_report_target_field(triomics_report_target_field(profile, "overlap"), "status")
    )
    if (!nzchar(overlap)) {
      overlap <- unique(Filter(nzchar, vapply(c(observations, associations), function(record) {
        triomics_report_target_safe_association_token(
          triomics_report_target_field(record, "subject_overlap_status")
        )
      }, character(1))))
      overlap <- if (length(overlap)) paste(overlap, collapse = "; ") else "unknown_not_recorded"
    }
    c(identity, list(
      drug_namespace = triomics_report_target_field(drug, "namespace"),
      drug_id = triomics_report_target_field(drug, "drug_id"),
      drug_label = triomics_report_target_field(drug, "label"),
      declared_constructs = triomics_report_target_text(
        triomics_report_target_field(request, "constructs")
      ),
      request_context_id = triomics_report_target_field(context, "context_id"),
      coverage = triomics_report_target_text(triomics_report_target_field(profile, "coverage"), "unmeasured"),
      study_ids = paste(observed_studies, collapse = "; "),
      study_count = length(observed_studies),
      subject_overlap_status = overlap,
      dependency_group_count = length(groups),
      discovery_status = triomics_report_target_text(
        triomics_report_target_field(discovery, "status"), "not_applicable"
      ),
      discovery_enumeration = triomics_report_target_text(
        triomics_report_target_field(discovery, "enumeration"), "not_applicable"
      ),
      reference_overlap_status = triomics_report_target_text(
        triomics_report_target_field(
          triomics_report_target_field(discovery, "reference_overlap"), "status"
        ), "not_recorded"
      )
    ))
  })
}

triomics_report_target_axis_rows <- function(profiles) {
  definitions <- triomics_report_target_axis_definitions()
  unlist(lapply(profiles, function(profile) {
    identity <- triomics_report_target_identity(profile)
    axes <- triomics_report_target_field(profile, "evidence_axes")
    lapply(names(definitions), function(axis_name) {
      axis <- triomics_report_target_field(axes, axis_name)
      coverage <- triomics_report_target_field(axis, "coverage")
      compat <- identical(axis_name, "metabolic_multiomics_compatibility")
      list(
        target_id = identity$target_id,
        axis = axis_name,
        axis_label = definitions[[axis_name]]$label,
        status = triomics_report_target_text(
          triomics_report_target_field(axis, "status"), "unavailable"
        ),
        reason_code = triomics_report_target_text(
          triomics_report_target_field(axis, "reason_code"),
          if (is.null(axis)) "D_REPORT_TARGET_AXIS_MISSING" else ""
        ),
        expected_links = if (compat) triomics_report_target_field(coverage, "expected_links") else NULL,
        measured_features = if (compat) triomics_report_target_field(coverage, "measured_features") else NULL,
        unmeasured_features = if (compat) triomics_report_target_field(coverage, "unmeasured_features") else NULL,
        expected_records = if (!compat) triomics_report_target_field(coverage, "expected_records") else NULL,
        measured_records = if (!compat) triomics_report_target_field(coverage, "measured_records") else NULL,
        unmeasured_records = if (!compat) triomics_report_target_field(coverage, "unmeasured_records") else NULL,
        unresolved_records = if (!compat) triomics_report_target_field(coverage, "unresolved_records") else NULL,
        publication_group_count = if (!compat) {
          triomics_report_target_field(coverage, "independent_publication_groups")
        } else NULL,
        assessment_note = if (is.null(axis) ||
                              triomics_report_target_text(triomics_report_target_field(axis, "status")) %in%
                                c("unmeasured", "unavailable", "unassessable", "unresolved")) {
          "Missing or unresolved evidence is not evidence of no effect or no binding."
        } else definitions[[axis_name]]$note
      )
    })
  }), recursive = FALSE)
}

triomics_report_target_link_rows <- function(profiles) {
  rows <- list()
  for (profile in profiles) {
    identity <- triomics_report_target_identity(profile)
    links <- triomics_report_target_records(triomics_report_target_field(profile, "links"))
    if (!length(links)) {
      compatibility <- triomics_report_target_field(
        triomics_report_target_field(profile, "evidence_axes"),
        "metabolic_multiomics_compatibility"
      )
      links <- triomics_report_target_records(triomics_report_target_field(compatibility, "links"))
    }
    for (link in links) {
      intermediate <- triomics_report_target_field(link, "intermediate_nodes") %||%
        triomics_report_target_field(link, "intermediate_node_ids")
      rows[[length(rows) + 1L]] <- list(
        target_id = identity$target_id,
        link_id = triomics_report_target_field(link, "link_id"),
        relation = triomics_report_target_field(link, "relation"),
        sign = triomics_report_target_field(link, "sign"),
        expected_direction = triomics_report_target_field(link, "expected_direction"),
        context_id = triomics_report_target_field(link, "context_id"),
        source_id = triomics_report_target_field(link, "source_id"),
        intermediate_context = triomics_report_target_text(intermediate),
        feature_namespace = triomics_report_target_field(link, "feature_namespace"),
        feature_id = triomics_report_target_field(link, "feature_id")
      )
    }
  }
  rows
}

triomics_report_target_observation_rows <- function(profiles) {
  rows <- list()
  for (profile in profiles) {
    identity <- triomics_report_target_identity(profile)
    observations <- triomics_report_target_records(triomics_report_target_field(profile, "observations"))
    if (!length(observations)) {
      compatibility <- triomics_report_target_field(
        triomics_report_target_field(profile, "evidence_axes"),
        "metabolic_multiomics_compatibility"
      )
      observations <- triomics_report_target_records(
        triomics_report_target_field(compatibility, "observations")
      )
    }
    links <- triomics_report_target_records(triomics_report_target_field(profile, "links"))
    if (!length(links)) {
      compatibility <- triomics_report_target_field(
        triomics_report_target_field(profile, "evidence_axes"),
        "metabolic_multiomics_compatibility"
      )
      links <- triomics_report_target_records(triomics_report_target_field(compatibility, "links"))
    }
    for (observation in observations) {
      link_id <- triomics_report_target_text(triomics_report_target_field(observation, "link_id"))
      link <- Filter(function(item) {
        identical(triomics_report_target_text(triomics_report_target_field(item, "link_id")), link_id)
      }, links)
      link <- if (length(link)) link[[1L]] else list()
      rows[[length(rows) + 1L]] <- list(
        target_id = identity$target_id,
        evidence_id = triomics_report_target_field(observation, "observation_id"),
        source_branch = triomics_report_target_field(observation, "source_branch"),
        study_id = triomics_report_target_field(observation, "study_id"),
        analysis_id = triomics_report_target_field(observation, "analysis_id"),
        block_id = triomics_report_target_field(observation, "block_id"),
        claim_id = triomics_report_target_field(observation, "claim_id"),
        contrast_id = triomics_report_target_field(observation, "contrast_id"),
        feature_key = triomics_report_target_field(observation, "feature_key"),
        feature_namespace = triomics_report_target_field(observation, "feature_namespace"),
        feature_id = triomics_report_target_field(observation, "feature_id"),
        endpoint = triomics_report_target_field(observation, "endpoint"),
        value = triomics_report_target_field(observation, "value"),
        effect = triomics_report_target_field(observation, "effect"),
        effect_unit = triomics_report_target_field(observation, "effect_unit"),
        status = triomics_report_target_field(observation, "status"),
        reason = triomics_report_target_field(observation, "reason"),
        expected_direction = triomics_report_target_field(observation, "expected_direction") %||%
          triomics_report_target_field(link, "expected_direction"),
        observed_direction = triomics_report_target_field(observation, "observed_direction") %||%
          triomics_report_target_field(observation, "direction"),
        path_sign = triomics_report_target_field(observation, "path_sign") %||%
          triomics_report_target_field(link, "sign"),
        path_relation = triomics_report_target_field(link, "relation"),
        path_context = triomics_report_target_field(observation, "path_context") %||%
          triomics_report_target_field(link, "context_id"),
        coverage = triomics_report_target_field(observation, "coverage"),
        taxon_applicability = triomics_report_target_field(observation, "taxon_applicability"),
        source_id = triomics_report_target_field(observation, "source_id"),
        source_sha256 = triomics_report_target_field(observation, "source_sha256"),
        source_selection_history = triomics_report_target_field(observation, "source_selection_history"),
        dependency_group = triomics_report_target_field(observation, "dependency_group"),
        subject_overlap_status = triomics_report_target_text(
          triomics_report_target_field(observation, "subject_overlap_status"),
          "unknown_not_recorded"
        ),
        selection_rule = triomics_report_target_field(
          triomics_report_target_field(observation, "selection_rule"), "rule"
        ),
        selection_criterion = triomics_report_target_field(
          triomics_report_target_field(observation, "selection_rule"), "criterion"
        ),
        interpretation = triomics_report_target_field(observation, "interpretation")
      )
    }
  }
  rows
}

triomics_report_target_input_hash <- function(profile, target, name) {
  profile_provenance <- triomics_report_target_field(profile, "input_provenance")
  target_provenance <- triomics_report_target_field(target, "input_provenance")
  profile_entry <- triomics_report_target_field(profile_provenance, name)
  target_entry <- triomics_report_target_field(target_provenance, name)
  triomics_report_target_field(profile_entry, "sha256") %||%
    triomics_report_target_field(target_entry, "sha256")
}

triomics_report_target_external_rows <- function(profiles, target) {
  rows <- list()
  for (profile in profiles) {
    identity <- triomics_report_target_identity(profile)
    records <- triomics_report_target_records(triomics_report_target_field(profile, "external_evidence"))
    source_hash <- triomics_report_target_input_hash(profile, target, "external_evidence_file")
    for (record in records) {
      is_association <- identical(
        triomics_report_target_field(record, "axis"), "association"
      )
      raw_value <- triomics_report_target_field(record, "value")
      availability <- triomics_report_target_text(
        triomics_report_target_field(record, "availability"),
        if (is.null(raw_value)) "unavailable" else "available"
      )
      rows[[length(rows) + 1L]] <- list(
        target_id = identity$target_id,
        evidence_id = triomics_report_target_field(record, "evidence_id"),
        axis = triomics_report_target_field(record, "axis"),
        origin = if (isTRUE(triomics_report_target_field(record, "measured_by_pipeline"))) {
          "pipeline"
        } else "external",
        measured_by_pipeline = triomics_report_target_field(record, "measured_by_pipeline"),
        target_namespace = triomics_report_target_field(record, "target_namespace") %||% identity$target_namespace,
        target_identity = triomics_report_target_field(record, "target_id") %||% identity$target_id,
        target_entity_type = triomics_report_target_field(record, "target_entity_type") %||% identity$target_entity_type,
        taxon_id = triomics_report_target_field(record, "taxon_id") %||% triomics_report_target_field(identity, "target_taxon_id"),
        compound_namespace = triomics_report_target_field(record, "compound_namespace"),
        compound_id = triomics_report_target_field(record, "compound_id"),
        construct = triomics_report_target_field(record, "construct"),
        assay = triomics_report_target_field(record, "assay"),
        endpoint = triomics_report_target_field(record, "endpoint"),
        relation = triomics_report_target_field(record, "relation"),
        units = triomics_report_target_field(record, "units"),
        value = raw_value,
        value_display = if (is.null(raw_value)) "Not available (source value is null)" else
          triomics_report_target_text(raw_value),
        value_status = availability,
        value_reason = triomics_report_target_text(
          triomics_report_target_field(record, "value_reason") %||%
            triomics_report_target_field(record, "assay_qualification_reason"),
          if (is.null(raw_value)) "source_value_null" else ""
        ),
        study_id = if (is_association) triomics_report_target_safe_association_token(
          triomics_report_target_field(record, "study_id")
        ) else "",
        analysis_id = if (is_association) triomics_report_target_safe_association_token(
          triomics_report_target_field(record, "analysis_id")
        ) else "",
        subject_overlap_status = if (is_association) triomics_report_target_safe_association_token(
          triomics_report_target_field(record, "subject_overlap_status")
        ) else "",
        construction_lineage = if (is_association) triomics_report_target_safe_association_prose(
          triomics_report_target_field(record, "construction_lineage")
        ) else "",
        source_selection_history = if (is_association) triomics_report_target_safe_association_prose(
          triomics_report_target_field(record, "source_selection_history")
        ) else "",
        source_id = triomics_report_target_field(record, "source_id"),
        source_sha256 = triomics_report_target_field(record, "source_sha256") %||% source_hash,
        publication_group = triomics_report_target_field(record, "publication_group"),
        dependency_group = triomics_report_target_field(record, "dependency_group"),
        attachment = triomics_report_target_field(record, "attachment"),
        target_identity_status = triomics_report_target_field(record, "target_identity_status"),
        assay_qualification = triomics_report_target_field(record, "assay_qualification"),
        context_id = triomics_report_target_field(record, "context_id"),
        assay_limitations = triomics_report_target_field(record, "assay_limitations"),
        independent_replication = triomics_report_target_field(record, "independent_replication"),
        replication_interpretation = "Publication groups preserve provenance and do not establish independent replication."
      )
    }
  }
  rows
}

triomics_report_target_alternative_rows <- function(target) {
  alternatives <- triomics_report_target_field(target, "alternatives")
  if (is.null(alternatives)) {
    return(list(list(
      target_id = "", comparability = "not_compared", comparison_axes = "",
      tie_status = "not_assessed", selected_target = "not_selected"
    )))
  }
  profiles <- triomics_report_target_records(triomics_report_target_field(alternatives, "profiles"))
  comparability <- triomics_report_target_text(
    triomics_report_target_field(alternatives, "comparability"), "unknown"
  )
  comparison_axes <- triomics_report_target_text(
    triomics_report_target_field(alternatives, "comparison_axes")
  )
  tie_status <- triomics_report_target_text(
    triomics_report_target_field(alternatives, "tie_status"), "unresolved"
  )
  selected_target <- triomics_report_target_text(
    triomics_report_target_field(alternatives, "selected_target"), "not_selected"
  )
  if (!length(profiles)) profiles <- list(list())
  lapply(profiles, function(profile) list(
    target_id = triomics_report_target_text(
      triomics_report_target_field(profile, "target_id") %||%
        triomics_report_target_field(triomics_report_target_field(profile, "request"), "id")
    ),
    comparability = comparability,
    comparison_axes = comparison_axes,
    tie_status = tie_status,
    selected_target = selected_target
  ))
}

triomics_report_target_discovery_rows <- function(target, profiles) {
  discovery <- triomics_report_target_field(target, "discovery")
  members <- triomics_report_target_records(triomics_report_target_field(discovery, "members"))
  if (!length(members)) {
    members <- unlist(lapply(profiles, function(profile) {
      member <- triomics_report_target_field(profile, "discovery")
      if (is.null(member)) return(list())
      list(c(member, list(target_id = triomics_report_target_identity(profile)$target_id)))
    }), recursive = FALSE)
  }
  if (!length(members)) return(list())
  universe <- triomics_report_target_field(discovery, "universe")
  protocol <- triomics_report_target_field(discovery, "protocol")
  lapply(members, function(member) {
    overlap <- triomics_report_target_field(member, "reference_overlap")
    history <- triomics_report_target_records(
      triomics_report_target_field(member, "prior_selection_history")
    )
    list(
      target_id = triomics_report_target_text(
        triomics_report_target_field(triomics_report_target_field(member, "identity"), "id") %||%
          triomics_report_target_field(member, "target_id")
      ),
      member_id = triomics_report_target_field(member, "member_id"),
      member_sha256 = triomics_report_target_field(member, "member_sha256"),
      inclusion_status = triomics_report_target_field(member, "inclusion_status"),
      reason = triomics_report_target_field(member, "reason"),
      universe_id = triomics_report_target_field(universe, "universe_id") %||%
        triomics_report_target_field(triomics_report_target_field(member, "universe"), "universe_id"),
      universe_version = triomics_report_target_field(universe, "version") %||%
        triomics_report_target_field(triomics_report_target_field(member, "universe"), "version"),
      universe_sha256 = triomics_report_target_field(universe, "sha256") %||%
        triomics_report_target_field(triomics_report_target_field(member, "universe"), "sha256"),
      protocol_id = triomics_report_target_field(protocol, "protocol_id"),
      protocol_sha256 = triomics_report_target_field(protocol, "sha256"),
      enumeration = triomics_report_target_field(discovery, "enumeration") %||%
        triomics_report_target_field(triomics_report_target_field(member, "universe"), "enumeration"),
      discovery_status = triomics_report_target_field(member, "status") %||%
        "exploratory_unranked",
      reference_overlap_status = triomics_report_target_field(overlap, "status"),
      reference_overlap_reason_code = triomics_report_target_field(overlap, "reason_code"),
      reference_overlap_evidence_count = length(triomics_report_target_records(
        triomics_report_target_field(overlap, "evidence")
      )),
      prior_selection_count = length(history)
    )
  })
}

triomics_report_target_selection_history_rows <- function(target, profiles) {
  discovery <- triomics_report_target_field(target, "discovery")
  members <- triomics_report_target_records(triomics_report_target_field(discovery, "members"))
  if (!length(members)) {
    members <- unlist(lapply(profiles, function(profile) {
      member_discovery <- triomics_report_target_field(profile, "discovery")
      if (is.null(member_discovery)) return(list())
      list(c(member_discovery, list(
        target_id = triomics_report_target_identity(profile)$target_id
      )))
    }), recursive = FALSE)
  }
  rows <- list()
  for (member in members) {
    history <- triomics_report_target_records(
      triomics_report_target_field(member, "prior_selection_history")
    )
    for (selection in history) {
      rows[[length(rows) + 1L]] <- list(
        target_id = triomics_report_target_text(
          triomics_report_target_field(triomics_report_target_field(member, "identity"), "id") %||%
            triomics_report_target_field(member, "target_id")
        ),
        member_id = triomics_report_target_field(member, "member_id"),
        selection_record_scope = "discovery_member_prior",
        selection_id = triomics_report_target_field(selection, "selection_id"),
        hypothesis_status = triomics_report_target_field(selection, "hypothesis_status"),
        recorded_hypothesis_status = triomics_report_target_field(selection, "recorded_hypothesis_status") %||%
          triomics_report_target_field(selection, "hypothesis_status"),
        selected_with_observations = triomics_report_target_field(selection, "selected_with_observations"),
        selection_source = triomics_report_target_field(selection, "selection_source"),
        selection_evidence_ref = triomics_report_target_field(selection, "selection_evidence_ref")
      )
    }
  }
  for (profile in profiles) {
    identity <- triomics_report_target_identity(profile)
    request <- triomics_report_target_field(profile, "request")
    selections <- triomics_report_target_records(triomics_report_target_field(request, "selection"))
    for (selection in selections) {
      rows[[length(rows) + 1L]] <- list(
        target_id = identity$target_id,
        member_id = "",
        selection_record_scope = "target_request",
        selection_id = triomics_report_target_field(selection, "selection_id"),
        hypothesis_status = triomics_report_target_field(selection, "hypothesis_status"),
        recorded_hypothesis_status = triomics_report_target_field(selection, "recorded_hypothesis_status") %||%
          triomics_report_target_field(selection, "hypothesis_status"),
        selected_with_observations = triomics_report_target_field(selection, "selected_with_observations"),
        selection_source = triomics_report_target_field(selection, "selection_source"),
        selection_evidence_ref = triomics_report_target_field(selection, "selection_evidence_ref")
      )
    }
  }
  rows
}

triomics_report_target_provenance_rows <- function(target, profiles) {
  rows <- list()
  append_entries <- function(provenance, target_id = "") {
    if (!is.list(provenance)) return(invisible(NULL))
    for (entry_name in names(provenance)) {
      entry <- provenance[[entry_name, exact = TRUE]]
      rows[[length(rows) + 1L]] <<- list(
        target_id = target_id,
        provenance_item = entry_name,
        status = triomics_report_target_text(triomics_report_target_field(entry, "status")),
        sha256 = triomics_report_target_field(entry, "sha256")
      )
    }
    invisible(NULL)
  }
  append_entries(triomics_report_target_field(target, "input_provenance"))
  for (profile in profiles) {
    append_entries(triomics_report_target_field(profile, "input_provenance"),
                   triomics_report_target_identity(profile)$target_id)
  }
  rows
}

triomics_report_target_lineage_rows <- function(profiles) {
  observations <- triomics_report_target_observation_rows(profiles)
  if (!length(observations)) return(list())
  lapply(observations, function(observation) {
    observation[c(
      "target_id", "study_id", "analysis_id", "source_branch", "block_id", "source_id",
      "source_sha256", "source_selection_history", "dependency_group", "subject_overlap_status"
    )]
  })
}

triomics_report_target_limit_rows <- function() {
  lapply(c(
    "A complete descriptive dossier is not scientific qualification or project acceptance.",
    "Unavailable assays and missing coverage do not mean there is no binding or no target effect.",
    "Association, abundance, docking, and IC50 are not converted to Kd or binding probability.",
    "Source-reported assay values and units retain their original endpoint and applicability limits.",
    "Repeated analyses sharing a study, source hash, or dependency group are not independent replication.",
    "D-T14 quantitative target method remains conditional-unapproved and not completed."
  ), function(note) list(limit = note))
}

triomics_report_targets_section <- function(loaded, config) {
  empty <- list(status = "not_requested", reason_code = "", tables = list())
  if (!isTRUE(config$target_enabled)) return(empty)
  d <- triomics_report_target_field(loaded, "d")
  target <- triomics_report_target_field(d, "target_investigation")
  if (is.null(target)) {
    return(list(status = "unavailable", reason_code = "D_REPORT_TARGET_DOSSIER_MISSING", tables = list()))
  }
  producer_status <- triomics_report_target_text(triomics_report_target_field(target, "status"), "unavailable")
  if (identical(producer_status, "not_requested")) return(empty)
  profiles <- triomics_report_target_records(triomics_report_target_field(target, "profiles"))
  reason_code <- triomics_report_target_text(triomics_report_target_field(target, "reason_code"))
  section_status <- switch(producer_status,
    recorded = "complete",
    complete = "complete",
    partial = "partial",
    unavailable = "unavailable",
    failed = "unavailable",
    not_started = "unavailable",
    "unavailable"
  )
  if (identical(section_status, "partial") && !nzchar(reason_code)) {
    reason_code <- "D_REPORT_TARGET_DOSSIER_PARTIAL"
  }
  if (identical(section_status, "unavailable") && !nzchar(reason_code)) {
    reason_code <- "D_REPORT_TARGET_DOSSIER_UNAVAILABLE"
  }
  dossier <- list(list(
    producer_status = producer_status,
    reason_code = reason_code,
    mode = triomics_report_target_text(triomics_report_target_field(target, "mode"), "unknown"),
    profile_count = length(profiles),
    result_count = length(triomics_report_target_records(triomics_report_target_field(target, "results"))),
    selected_target = triomics_report_target_text(
      triomics_report_target_field(target, "selected_target"), "not_selected"
    ),
    tie_status = triomics_report_target_text(triomics_report_target_field(target, "tie_status"), "not_assessed"),
    quantitative_method_status = "conditional_unapproved",
    d14_status = "not_completed",
    project_acceptance_status = "not_assessed"
  ))
  axes <- triomics_report_target_axis_rows(profiles)
  observations <- triomics_report_target_observation_rows(profiles)
  external <- triomics_report_target_external_rows(profiles, target)
  tables <- list(
    dossier = triomics_report_target_frame(dossier, c(
      "producer_status", "reason_code", "mode", "profile_count", "result_count", "selected_target",
      "tie_status", "quantitative_method_status", "d14_status", "project_acceptance_status"
    ), numeric_columns = c("profile_count", "result_count")),
    profiles = triomics_report_target_frame(triomics_report_target_profile_rows(profiles), c(
      "target_id", "target_namespace", "target_taxon_id", "target_entity_type", "mode",
      "drug_namespace", "drug_id", "drug_label", "declared_constructs", "request_context_id",
      "mechanism_class", "mechanism_status", "mechanism_declaration_status", "coverage",
      "study_ids", "study_count", "subject_overlap_status", "dependency_group_count",
      "discovery_status", "discovery_enumeration", "reference_overlap_status"
    ), numeric_columns = c("target_taxon_id", "study_count", "dependency_group_count")),
    evidence_axes = triomics_report_target_frame(axes, c(
      "target_id", "axis", "axis_label", "status", "reason_code", "expected_links",
      "measured_features", "unmeasured_features", "expected_records", "measured_records",
      "unmeasured_records", "unresolved_records", "publication_group_count", "assessment_note"
    ), numeric_columns = c("expected_links", "measured_features", "unmeasured_features",
                           "expected_records", "measured_records", "unmeasured_records",
                           "unresolved_records", "publication_group_count")),
    mechanistic_context = triomics_report_target_frame(triomics_report_target_link_rows(profiles), c(
      "target_id", "link_id", "relation", "sign", "expected_direction", "context_id", "source_id",
      "intermediate_context", "feature_namespace", "feature_id"
    )),
    observations = triomics_report_target_frame(observations, c(
      "target_id", "evidence_id", "source_branch", "study_id", "analysis_id", "block_id", "claim_id",
      "contrast_id", "feature_key", "feature_namespace", "feature_id", "endpoint", "value", "effect",
      "effect_unit", "status", "reason", "expected_direction", "observed_direction", "path_sign",
      "path_relation", "path_context", "coverage", "taxon_applicability", "source_id", "source_sha256",
      "source_selection_history", "dependency_group", "subject_overlap_status", "selection_rule",
      "selection_criterion", "interpretation"
    ), numeric_columns = c("value", "effect")),
    source_lineage = triomics_report_target_frame(triomics_report_target_lineage_rows(profiles), c(
      "target_id", "study_id", "analysis_id", "source_branch", "block_id", "source_id", "source_sha256",
      "source_selection_history", "dependency_group", "subject_overlap_status"
    )),
    external_evidence = triomics_report_target_frame(external, c(
      "target_id", "evidence_id", "axis", "origin", "measured_by_pipeline", "target_namespace",
      "target_identity", "target_entity_type", "taxon_id", "compound_namespace", "compound_id", "construct",
      "assay", "endpoint", "relation", "units", "value", "value_display", "value_status", "value_reason",
      "study_id", "analysis_id", "subject_overlap_status", "construction_lineage",
      "source_selection_history", "source_id", "source_sha256", "publication_group",
      "dependency_group", "attachment",
      "target_identity_status", "assay_qualification", "context_id", "assay_limitations",
      "independent_replication", "replication_interpretation"
    ), numeric_columns = c("taxon_id", "value"),
    logical_columns = c("measured_by_pipeline", "independent_replication")),
    alternatives = triomics_report_target_frame(triomics_report_target_alternative_rows(target), c(
      "target_id", "comparability", "comparison_axes", "tie_status", "selected_target"
    )),
    discovery = triomics_report_target_frame(triomics_report_target_discovery_rows(target, profiles), c(
      "target_id", "member_id", "member_sha256", "inclusion_status", "reason", "universe_id",
      "universe_version", "universe_sha256", "protocol_id", "protocol_sha256", "enumeration",
      "discovery_status", "reference_overlap_status", "reference_overlap_reason_code",
      "reference_overlap_evidence_count", "prior_selection_count"
    ), numeric_columns = c("reference_overlap_evidence_count", "prior_selection_count")),
    selection_history = triomics_report_target_frame(
      triomics_report_target_selection_history_rows(target, profiles),
      c("target_id", "member_id", "selection_record_scope", "selection_id", "hypothesis_status",
        "recorded_hypothesis_status", "selected_with_observations", "selection_source",
        "selection_evidence_ref"),
      logical_columns = "selected_with_observations"
    ),
    provenance = triomics_report_target_frame(triomics_report_target_provenance_rows(target, profiles), c(
      "target_id", "provenance_item", "status", "sha256"
    )),
    limitations = triomics_report_target_frame(triomics_report_target_limit_rows(), "limit")
  )
  list(status = section_status, reason_code = reason_code, tables = tables)
}
