triomics_d_target_discovery_abort <- function(code, message, key = "target_investigation") {
  triomics_d_abort(code, message, key)
}

triomics_d_target_discovery_array <- function(value) {
  is.list(value) && !is.data.frame(value) && is.null(names(value))
}

triomics_d_target_discovery_object <- function(value, required, key, allowed = required) {
  if (!is.list(value) || is.data.frame(value) || is.null(names(value)) || anyDuplicated(names(value)) ||
      !all(required %in% names(value)) || any(!names(value) %in% allowed)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "Discovery input has an invalid typed object or unexpected fields.", key)
  }
  invisible(value)
}

triomics_d_target_discovery_selection <- function(history, key) {
  if (!triomics_d_target_discovery_array(history)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "Every universe member needs an explicit selection-history array.", key)
  }
  required <- c("selection_id", "hypothesis_status", "selected_with_observations")
  allowed <- c(required, "selection_source", "selection_evidence_ref")
  for (record in history) {
    triomics_d_target_discovery_object(record, required, key, allowed)
    if (!triomics_target_string(record$selection_id) ||
        !triomics_target_string(record$hypothesis_status) ||
        !record$hypothesis_status %in% c("exploratory", "confirmed") ||
        !is.logical(record$selected_with_observations) || length(record$selected_with_observations) != 1L ||
        is.na(record$selected_with_observations) ||
        (!is.null(record$selection_source) && !triomics_target_string(record$selection_source)) ||
        (!is.null(record$selection_evidence_ref) && !triomics_target_string(record$selection_evidence_ref))) {
      triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                        "A universe member has invalid prior selection history.", key)
    }
  }
  ids <- if (length(history)) vapply(history, `[[`, character(1), "selection_id") else character()
  if (anyDuplicated(ids)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "A universe member repeats a selection-history identifier.", key)
  }
  history
}

triomics_d_target_discovery_identity <- function(value, key) {
  if (!is.list(value) || is.data.frame(value) || is.null(names(value)) || anyDuplicated(names(value))) {
    triomics_d_target_discovery_abort("D_TARGET_IDENTITY_REQUIRED",
                                      "Each universe member needs a typed namespaced identity.", key)
  }
  has_constructs <- "constructs" %in% names(value)
  triomics_d_target_identity_record(value, key, allow_constructs = has_constructs)
}

triomics_d_target_discovery_reference_sources <- function(value, key) {
  if (!triomics_d_target_discovery_array(value)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "Reference signatures need an explicit source array.", key)
  }
  sources <- lapply(value, function(source) {
    triomics_d_target_discovery_object(source, c("source_id", "sha256"), key)
    if (!triomics_target_string(source$source_id) ||
        !triomics_sha256_text(as.character(source$sha256))) {
      triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                        "Reference sources need actual source identifiers and SHA-256 hashes.", key)
    }
    source
  })
  ids <- if (length(sources)) vapply(sources, `[[`, character(1), "source_id") else character()
  if (anyDuplicated(ids)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "A member's reference sources must have unique source identifiers.", key)
  }
  unname(sources)
}

triomics_d_target_discovery_members <- function(universe) {
  members <- universe$members
  if (!triomics_d_target_discovery_array(members) || !length(members)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "The bounded universe must enumerate at least one member.",
                                      "target_investigation.universe_ref.members")
  }
  validated <- lapply(seq_along(members), function(index) {
    member <- members[[index]]
    key <- paste0("target_investigation.universe_ref.members[", index, "]")
    triomics_d_target_discovery_object(
      member,
      c("member_id", "disposition", "reason", "identity", "mechanism_class", "links",
        "selection_history", "reference_sources"),
      key
    )
    if (!triomics_target_string(member$member_id) ||
        !triomics_target_string(member$disposition) ||
        !member$disposition %in% c("included", "excluded") ||
        !triomics_target_string(member$reason)) {
      triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                        "Every universe member needs a disposition and a reason.", key)
    }
    identity <- triomics_d_target_discovery_identity(member$identity, paste0(key, ".identity"))
    mechanism_class <- member$mechanism_class
    if (is.null(mechanism_class)) mechanism_class <- "unknown"
    if (!triomics_target_string(mechanism_class) ||
        !mechanism_class %in% c("enzymatic", "non_enzymatic", "unknown")) {
      triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                        "A member mechanism class must be a declared category.", key)
    }
    if (!triomics_d_target_discovery_array(member$links)) {
      triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                        "Every universe member needs an explicit path array.", key)
    }
    history <- triomics_d_target_discovery_selection(member$selection_history,
                                                      paste0(key, ".selection_history"))
    sources <- triomics_d_target_discovery_reference_sources(member$reference_sources,
                                                              paste0(key, ".reference_sources"))
    list(member = member, identity = identity, mechanism_class = mechanism_class,
         links = member$links, history = history, reference_sources = sources,
         member_sha256 = triomics_hash_text(triomics_canonical_json(member)))
  })
  ids <- vapply(validated, function(entry) as.character(entry$member$member_id), character(1))
  request_ids <- vapply(validated, function(entry) paste0("target:", entry$identity$namespace, ":",
                                                          entry$identity$id, ":taxon:", entry$identity$taxon_id),
                        character(1))
  if (anyDuplicated(ids) || anyDuplicated(request_ids)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "Member IDs and namespaced target identities must be unique.",
                                      "target_investigation.universe_ref.members")
  }
  unname(validated)
}

triomics_d_target_discovery_protocol <- function(protocol, investigation, universe_input) {
  required <- c("format", "format_version", "protocol_id", "version", "context_ref", "drug_ref",
                "drug_identity", "universe_sha256", "enumeration_rule")
  triomics_d_target_discovery_object(protocol, required,
                                     "target_investigation.protocol_ref")
  rule <- protocol$enumeration_rule
  triomics_d_target_discovery_object(rule, c("rule_id", "method", "ordering", "ranking"),
                                     "target_investigation.protocol_ref.enumeration_rule")
  valid <- identical(protocol$format, "triomics.target-discovery-protocol") &&
    identical(protocol$format_version, "1.0") &&
    triomics_target_string(protocol$protocol_id) && triomics_target_string(protocol$version) &&
    identical(as.character(protocol$context_ref), as.character(investigation$context_ref)) &&
    identical(protocol$drug_ref, investigation$drug_ref %||% NULL) &&
    triomics_sha256_text(as.character(protocol$universe_sha256)) &&
    identical(as.character(protocol$universe_sha256), as.character(universe_input$sha256)) &&
    triomics_target_string(universe_input$value$enumeration_rule_id) &&
    identical(as.character(rule$rule_id), as.character(universe_input$value$enumeration_rule_id)) &&
    identical(rule$method, "declared-disposition") &&
    identical(rule$ordering, "input-order") && identical(rule$ranking, "none")
  if (!valid) {
    code <- if (!identical(as.character(protocol$universe_sha256), as.character(universe_input$sha256))) {
      "D_TARGET_UNIVERSE_HASH_MISMATCH"
    } else "D_TARGET_PROTOCOL_INVALID"
    triomics_d_target_discovery_abort(code,
                                      "The discovery protocol must pin the universe bytes and declare its unranked enumeration rule.",
                                      "target_investigation.protocol_ref")
  }
  drug <- NULL
  if (is.null(investigation$drug_ref)) {
    if (!is.null(protocol$drug_identity)) {
      triomics_d_target_discovery_abort("D_TARGET_PROTOCOL_INVALID",
                                        "A drug identity cannot be supplied without drug_ref.",
                                        "target_investigation.protocol_ref.drug_identity")
    }
  } else {
    value <- protocol$drug_identity
    triomics_d_target_discovery_object(value, c("namespace", "id", "label"),
                                       "target_investigation.protocol_ref.drug_identity")
    if (!triomics_target_string(value$namespace) || !triomics_target_string(value$id) ||
        !triomics_target_string(value$label)) {
      triomics_d_target_discovery_abort("D_TARGET_PROTOCOL_INVALID",
                                        "The discovery protocol drug identity is incomplete.",
                                        "target_investigation.protocol_ref.drug_identity")
    }
    drug <- list(namespace = value$namespace, drug_id = value$id, label = value$label)
  }
  list(value = protocol, drug = drug)
}

triomics_d_target_discovery_source_overlap <- function(reference_sources, evidence, member_id) {
  if (!length(reference_sources)) {
    return(list(status = "not_applicable", evidence = list(), reason_code = NULL))
  }
  current <- evidence$source_by_id
  inventory_complete <- is.list(current) && !is.data.frame(current)
  current_sources <- if (inventory_complete) Filter(function(source) {
    is.list(source) && triomics_target_string(source$source_id) &&
      triomics_sha256_text(as.character(source$sha256))
  }, current) else list()
  current_ids <- if (length(current_sources)) {
    vapply(current_sources, `[[`, character(1), "source_id")
  } else character()
  inventory_complete <- inventory_complete && length(current_sources) == length(current) &&
    !anyDuplicated(current_ids)
  overlap_unknown <- TRUE
  comparison <- lapply(reference_sources, function(reference) {
    same_bytes <- Filter(function(source) {
      identical(as.character(source$sha256), as.character(reference$sha256))
    }, current_sources)
    if (length(same_bytes)) {
      triomics_d_target_discovery_abort(
        "D_TARGET_REFERENCE_LEAKAGE",
        "A reference signature source is byte-identical to a source in the current B/C evaluation bundle.",
        paste0("target_investigation.universe_ref.members.", member_id, ".reference_sources")
      )
    }
    same_id <- Filter(function(source) {
      identical(as.character(source$source_id), as.character(reference$source_id))
    }, current_sources)
    list(source_id = reference$source_id, reference_sha256 = reference$sha256,
         current_source_status = if (length(same_id)) "same_id_different_bytes" else
           if (inventory_complete) "no_matching_id_or_bytes" else "inventory_incomplete",
         current_sha256 = if (length(same_id)) as.character(same_id[[1L]]$sha256) else NULL,
         overlap = "unknown")
  })
  list(status = "unknown",
       evidence = unname(comparison),
       reason_code = "D_TARGET_REFERENCE_OVERLAP_UNKNOWN")
}

triomics_d_target_discovery_evidence_bundle <- function(evidence) {
  valid <- is.list(evidence) && identical(evidence$artifact_type, "evidence_bundle") &&
    triomics_target_string(evidence$study_id) && triomics_target_string(evidence$analysis_id) &&
    is.list(evidence$immutable_inputs) &&
    triomics_sha256_text(as.character(evidence$immutable_inputs$b_bundle_sha256)) &&
    triomics_sha256_text(as.character(evidence$immutable_inputs$c_manifest_sha256)) &&
    is.list(evidence$feature_registry) && is.list(evidence$source_by_id) &&
    is.list(evidence$tested_results) && is.list(evidence$nominations)
  if (!valid) {
    triomics_d_target_discovery_abort("D_TARGET_OBSERVATION_LINEAGE_INVALID",
                                      "Discovery requires the verified same-run B/C evidence bundle.",
                                      "target_investigation")
  }
  invisible(evidence)
}

triomics_d_target_discovery_profile <- function(entry, protocol, universe, evidence,
                                                external_records, input_provenance) {
  member <- entry$member
  identity <- entry$identity
  history_for_request <- lapply(entry$history, function(selection) {
    selection$recorded_hypothesis_status <- selection$hypothesis_status
    selection
  })
  request <- triomics_target_request(
    identity = identity, mode = "discover", selection = history_for_request,
    context = list(context_id = as.character(protocol$value$context_ref),
                   confirmation_protocol = NULL),
    drug = protocol$drug,
    universe = list(universe_id = as.character(universe$universe_id),
                    version = as.character(universe$version), enumeration = "unranked",
                    sha256 = as.character(input_provenance$universe_ref$sha256))
  )
  request$mechanism_class <- entry$mechanism_class
  observations <- triomics_target_resolve_observations(request, entry$links, evidence)
  external <- triomics_target_external_evidence(request, external_records)
  external_ledger <- list(request_id = request$request_id, records = external$records,
                          independent_group_count = external$independent_group_count)
  mechanism_status <- if (entry$mechanism_class %in% c("enzymatic", "non_enzymatic")) {
    "assessable"
  } else "unassessable"
  profile <- triomics_target_profile(request, entry$links, observations, external_evidence = external_ledger)
  profile$mechanism_declaration_status <- mechanism_status
  profile$mechanism_declarations <- list()
  profile$input_provenance <- input_provenance
  overlap <- triomics_d_target_discovery_source_overlap(entry$reference_sources, evidence,
                                                        as.character(member$member_id))
  profile$discovery <- list(
    status = "exploratory_unranked",
    universe_id = as.character(universe$universe_id),
    universe_version = as.character(universe$version),
    enumeration = "unranked",
    member_id = as.character(member$member_id),
    member_sha256 = entry$member_sha256,
    inclusion_status = "included",
    reason = as.character(member$reason),
    prior_selection_history = entry$history,
    reference_sources = entry$reference_sources,
    reference_overlap = overlap
  )
  list(profile = profile,
       result = list(request = request, mechanism_status = mechanism_status,
                     mechanism_declarations = list(), links = entry$links,
                     observations = observations, external_evidence = external_ledger,
                     input_provenance = input_provenance))
}

triomics_d_target_discovery_qualify_profile <- function(profile) {
  if (!identical(profile$discovery$reference_overlap$status, "unknown")) return(profile)
  axis <- profile$evidence_axes$metabolic_multiomics_compatibility
  axis$status <- "unassessable"
  axis$reason_code <- profile$discovery$reference_overlap$reason_code
  profile$evidence_axes$metabolic_multiomics_compatibility <- axis
  profile
}

triomics_d_target_discovery_slot <- function(investigation, config_path, evidence = NULL) {
  if (!is.list(investigation) || !isTRUE(investigation$enabled) ||
      !identical(investigation$mode, "discover") ||
      !triomics_target_string(investigation$context_ref) ||
      !triomics_target_string(investigation$protocol_ref) ||
      !triomics_target_string(investigation$universe_ref)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_REQUIRED",
                                      "Discovery needs local universe and protocol references plus a declared context.",
                                      "target_investigation")
  }
  targets <- investigation$targets %||% list()
  if (!triomics_d_target_discovery_array(targets) || length(targets) > 0L) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "Discover mode enumerates its pinned universe and cannot add declared targets.",
                                      "target_investigation.targets")
  }
  universe_input <- triomics_d_target_read_json_input(
    investigation$universe_ref, config_path, "target_investigation.universe_ref"
  )
  if (!identical(universe_input$status, "available")) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_REQUIRED",
                                      "The referenced local target universe is unavailable.",
                                      "target_investigation.universe_ref")
  }
  protocol_input <- triomics_d_target_read_json_input(
    investigation$protocol_ref, config_path, "target_investigation.protocol_ref"
  )
  if (!identical(protocol_input$status, "available")) {
    triomics_d_target_discovery_abort("D_TARGET_PROTOCOL_INVALID",
                                      "The referenced local discovery protocol is unavailable.",
                                      "target_investigation.protocol_ref")
  }
  universe <- universe_input$value
  required_universe <- c("format", "format_version", "universe_id", "version",
                         "enumeration_rule_id", "members")
  triomics_d_target_discovery_object(universe, required_universe,
                                     "target_investigation.universe_ref")
  if (!identical(universe$format, "triomics.target-discovery-universe") ||
      !identical(universe$format_version, "1.0") ||
      !triomics_target_string(universe$universe_id) || !triomics_target_string(universe$version)) {
    triomics_d_target_discovery_abort("D_TARGET_UNIVERSE_INVALID",
                                      "The target universe needs a supported versioned format.",
                                      "target_investigation.universe_ref")
  }
  protocol <- triomics_d_target_discovery_protocol(protocol_input$value, investigation, universe_input)
  members <- triomics_d_target_discovery_members(universe)
  triomics_d_target_discovery_evidence_bundle(evidence)
  external_input <- if (triomics_d_nonempty_string(investigation$external_evidence_file)) {
    triomics_d_target_read_json_input(investigation$external_evidence_file, config_path,
                                      "target_investigation.external_evidence_file")
  } else list(status = "unavailable", value = NULL, sha256 = NULL)
  external_records <- list()
  if (identical(external_input$status, "available")) {
    document <- external_input$value
    allowed <- c("format", "format_version", "records")
    if (!setequal(names(document), "records") &&
        !(setequal(names(document), allowed) && identical(document$format, "triomics.external-target-evidence") &&
          identical(document$format_version, "1.0"))) {
      triomics_d_target_discovery_abort("D_TARGET_INPUT_INVALID",
                                        "External assay input has an invalid format.",
                                        "target_investigation.external_evidence_file")
    }
    external_records <- document$records
    if (!triomics_d_target_discovery_array(external_records)) {
      triomics_d_target_discovery_abort("D_TARGET_INPUT_INVALID",
                                        "External assay records must remain an array.",
                                        "target_investigation.external_evidence_file.records")
    }
  }
  input_provenance <- list(
    universe_ref = list(status = "consumed", reference = as.character(investigation$universe_ref),
                        sha256 = as.character(universe_input$sha256)),
    protocol_ref = list(status = "consumed", reference = as.character(investigation$protocol_ref),
                        sha256 = as.character(protocol_input$sha256)),
    external_evidence_file = list(status = external_input$status,
                                  reference = investigation$external_evidence_file %||% NULL,
                                  sha256 = external_input$sha256 %||% NULL)
  )
  included <- Filter(function(entry) identical(entry$member$disposition, "included"), members)
  built <- lapply(included, triomics_d_target_discovery_profile, protocol = protocol,
                  universe = universe, evidence = evidence, external_records = external_records,
                  input_provenance = input_provenance)
  base_profiles <- lapply(built, `[[`, "profile")
  results <- lapply(built, `[[`, "result")
  alternatives <- if (length(base_profiles) >= 2L) triomics_target_alternatives(base_profiles) else NULL
  unknown_overlap <- vapply(base_profiles, function(profile) {
    identical(profile$discovery$reference_overlap$status, "unknown")
  }, logical(1))
  profiles <- lapply(base_profiles, triomics_d_target_discovery_qualify_profile)
  if (!is.null(alternatives)) {
    alternatives$profiles <- profiles
    alternatives$selected_target <- NULL
    if (any(unknown_overlap)) alternatives$comparability <- "noncomparable"
  }
  accounted <- lapply(members, function(entry) {
    member <- entry$member
    overlap <- if (identical(member$disposition, "included")) {
      profiles[[match(member$member_id, vapply(profiles, function(profile) profile$discovery$member_id,
                                               character(1)))]]$discovery$reference_overlap
    } else if (length(entry$reference_sources)) {
      triomics_d_target_discovery_source_overlap(entry$reference_sources, evidence,
                                                  as.character(member$member_id))
    } else list(status = "not_applicable", evidence = list(), reason_code = NULL)
    list(member_id = as.character(member$member_id), member_sha256 = entry$member_sha256,
         inclusion_status = as.character(member$disposition), reason = as.character(member$reason),
         identity = entry$identity, prior_selection_history = entry$history,
         reference_sources = entry$reference_sources, reference_overlap = overlap)
  })
  included_count <- as.integer(sum(vapply(members, function(entry) identical(entry$member$disposition, "included"),
                                         logical(1))))
  excluded_count <- as.integer(length(members) - included_count)
  universe_summary <- list(universe_id = as.character(universe$universe_id),
                           version = as.character(universe$version),
                           sha256 = as.character(universe_input$sha256),
                           member_count = as.integer(length(members)),
                           included_count = included_count, excluded_count = excluded_count)
  list(status = "recorded", reason_code = NULL, mode = "discover",
       discovery = list(format = "triomics.target-discovery", format_version = "1.0",
                        mode = "discover", enumeration = "unranked", universe = universe_summary,
                        protocol = list(protocol_id = as.character(protocol$value$protocol_id),
                                        version = as.character(protocol$value$version),
                                        sha256 = as.character(protocol_input$sha256),
                                        enumeration_rule = protocol$value$enumeration_rule),
                        members = unname(accounted)),
       input_provenance = input_provenance, results = unname(results), profiles = unname(profiles),
       alternatives = alternatives, selected_target = NULL,
       tie_status = if (is.null(alternatives)) NULL else alternatives$tie_status,
       mechanism_status = if (length(profiles) &&
         all(vapply(profiles, function(profile) identical(profile$mechanism_status, "assessable"), logical(1)))) {
         "supplied"
       } else if (any(vapply(profiles, function(profile) identical(profile$mechanism_status, "assessable"), logical(1)))) {
         "partially_assessable"
       } else "unassessable")
}
