triomics_ora_upper_tail <- function(N, K, n, k) {
  counts <- list(N, K, n, k)
  valid <- all(vapply(counts, function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 0 && x == floor(x), logical(1)))
  if (!valid || K > N || n > N || k > K || k > n || k < max(0, n - (N - K))) {
    triomics_d_abort('D_ORA_COUNT_INVALID', 'Hypergeometric counts must describe a possible draw.', 'counts')
  }
  stats::phyper(k - 1, K, N - K, n, lower.tail = FALSE)
}

triomics_ora_abort <- function(code, message, key = "ora") {
  triomics_d_abort(code, message, key)
}

triomics_ora_characters <- function(value) {
  if (is.null(value)) return(character())
  if (is.character(value)) return(value)
  if (is.list(value) || is.atomic(value)) return(as.character(unlist(value, use.names = FALSE)))
  character()
}

triomics_ora_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
}

triomics_ora_inventory_status <- function(source_ids, source_by_id) {
  source_ids <- unique(triomics_ora_characters(source_ids))
  source_ids <- source_ids[!is.na(source_ids) & nzchar(source_ids)]
  records <- lapply(source_ids, function(source_id) {
    source <- if (is.list(source_by_id)) source_by_id[[source_id]] else NULL
    history <- as.character(source$selection_history %||% "unknown")
    if (length(history) != 1L || is.na(history) ||
        !history %in% c("full_unselected", "prespecified_panel", "outcome_selected", "unknown") ||
        !identical(as.character(source$role %||% ""), "measurement_matrix")) {
      history <- "unknown"
    }
    list(source_id = source_id,
         role = as.character(source$role %||% "unknown"),
         selection_history = history,
         source_sha256 = as.character(source$sha256 %||% ""))
  })
  names(records) <- source_ids
  histories <- if (length(records)) {
    vapply(records, `[[`, character(1), "selection_history")
  } else {
    "unknown"
  }
  status <- if (any(histories == "outcome_selected")) {
    "outcome_selected"
  } else if (any(histories == "unknown")) {
    "unknown"
  } else if (any(histories == "prespecified_panel")) {
    "prespecified_panel"
  } else {
    "full_unselected"
  }
  list(status = status, qualified = status %in% c("full_unselected", "prespecified_panel"),
       sources = unname(records))
}

triomics_ora_mapping_compatibility <- function(claim, mapping_contract, feature_rows = list()) {
  required <- c("source_namespace", "taxon_id", "source_resolution", "target_namespace")
  if (!is.list(claim) || any(!vapply(required, function(field) triomics_ora_text(claim[[field]]), logical(1))) ||
      !is.list(mapping_contract)) {
    return(list(status = "incompatible", reason_code = "D_ORA_MAPPING_INCOMPATIBLE",
                reason = "The protocol and mapping resource do not declare one compatible molecular identity."))
  }
  actual_fields <- c(source_namespace = "source_namespace", taxon_id = "taxon_id",
                     source_resolution = "resolution")
  for (field in names(actual_fields)) {
    observed <- if (length(feature_rows)) {
      unique(vapply(feature_rows, function(row) as.character(row[[actual_fields[[field]]]] %||% ""), character(1)))
    } else {
      character()
    }
    if (length(observed) > 1L || (length(observed) == 1L && !identical(observed[[1L]], claim[[field]]))) {
      return(list(status = "incompatible", reason_code = "D_ORA_MAPPING_INCOMPATIBLE",
                  reason = paste0("FeatureRegistry ", actual_fields[[field]], " values disagree with the frozen protocol.")))
    }
  }
  contract_fields <- c(source_namespace = "namespaces", taxon_id = "organisms",
                       source_resolution = "resolutions", target_namespace = "target_namespaces")
  for (field in names(contract_fields)) {
    supported <- triomics_ora_characters(mapping_contract[[contract_fields[[field]]]])
    if (!claim[[field]] %in% supported) {
      return(list(status = "incompatible", reason_code = "D_ORA_MAPPING_INCOMPATIBLE",
                  reason = paste0("The pinned mapping resource does not support the declared ", field, ".")))
    }
  }
  list(status = "compatible", reason_code = NULL, reason = NULL)
}

triomics_ora_unique_entities <- function(feature_rows, mapping, target_namespace, taxon_id,
                                         nomination_feature_keys) {
  if (!is.list(feature_rows) || (length(feature_rows) &&
      !all(vapply(feature_rows, function(row) is.list(row) && !is.data.frame(row), logical(1)))) ||
      !is.list(mapping) || !identical(mapping$status, "available") ||
      !triomics_ora_text(target_namespace) || !triomics_ora_text(taxon_id)) {
    triomics_ora_abort("D_ORA_MAPPING_INVALID", "The measured feature rows and available mapping result are required.")
  }
  feature_keys <- vapply(feature_rows, function(row) as.character(row$feature_key %||% ""), character(1))
  if (any(!nzchar(feature_keys)) || anyDuplicated(feature_keys)) {
    triomics_ora_abort("D_ORA_MAPPING_INVALID", "Measured feature keys must be unique and non-empty.")
  }
  nominated <- unique(triomics_ora_characters(nomination_feature_keys))
  if (anyNA(nominated) || any(!nzchar(nominated)) || any(!nominated %in% feature_keys)) {
    triomics_ora_abort("D_ORA_LINEAGE_INVALID", "A nomination does not belong to the complete measured feature background.")
  }
  observations <- mapping$observations
  if (!is.list(observations) || length(observations) != length(feature_keys)) {
    triomics_ora_abort("D_ORA_MAPPING_INVALID", "The D02/D03 mapper must return one observation for every measured feature.")
  }
  observation_keys <- vapply(observations, function(row) as.character(row$feature_key %||% ""), character(1))
  if (anyDuplicated(observation_keys) || !setequal(observation_keys, feature_keys)) {
    triomics_ora_abort("D_ORA_MAPPING_INVALID", "Mapper observations do not cover the complete measured feature background exactly once.")
  }
  edges <- mapping$edges %||% list()
  if (!is.list(edges) || (length(edges) && !all(vapply(edges, function(edge) {
    is.list(edge) && triomics_ora_text(edge$feature_key) && edge$feature_key %in% feature_keys &&
      triomics_ora_text(edge$target_namespace) && triomics_ora_text(edge$target_entity_id) &&
      triomics_ora_text(as.character(edge$taxon %||% ""))
  }, logical(1))))) {
    triomics_ora_abort("D_ORA_MAPPING_INVALID", "The D02/D03 mapper returned an invalid edge record.")
  }
  included <- list()
  excluded <- list()
  entity_of_feature <- stats::setNames(vector("list", length(feature_keys)), feature_keys)
  for (index in seq_along(feature_rows)) {
    row <- feature_rows[[index]]
    key <- feature_keys[[index]]
    source_edges <- Filter(function(edge) identical(as.character(edge$feature_key), key), edges)
    target_edges <- Filter(function(edge) identical(as.character(edge$target_namespace), target_namespace) &&
                             identical(as.character(edge$taxon), taxon_id), source_edges)
    target_ids <- sort(unique(vapply(target_edges, function(edge) as.character(edge$target_entity_id), character(1))),
                       method = "radix")
    record <- list(feature_key = key,
                   original_id = as.character(row$original_id %||% row$source_id %||% ""),
                   source_id = as.character(row$source_id %||% ""),
                   source_namespace = as.character(row$source_namespace %||% ""),
                   taxon_id = as.character(row$taxon_id %||% ""))
    if (length(target_ids) == 1L) {
      entity_of_feature[[key]] <- target_ids[[1L]]
      included[[length(included) + 1L]] <- c(record, list(
        target_namespace = target_namespace, entity_id = target_ids[[1L]],
        reason = "unique_resolved_entity"
      ))
    } else {
      observation <- observations[[match(key, observation_keys)]]
      reason <- if (length(target_ids) > 1L) {
        "ambiguous_target_entity"
      } else if (length(source_edges) && !length(target_edges)) {
        "target_namespace_unmapped"
      } else if (identical(as.character(observation$mapping_status %||% ""), "unmapped")) {
        "unmapped"
      } else {
        "target_namespace_unmapped"
      }
      entity_of_feature[[key]] <- NULL
      excluded[[length(excluded) + 1L]] <- c(record, list(
        target_namespace = target_namespace, candidate_entity_ids = as.list(target_ids), reason = reason
      ))
    }
  }
  included_keys <- vapply(included, `[[`, character(1), "feature_key")
  included_ids <- vapply(included, `[[`, character(1), "entity_id")
  universe_ids <- sort(unique(included_ids), method = "radix")
  selected_ids <- sort(unique(vapply(included[vapply(included, function(record) {
    record$feature_key %in% nominated
  }, logical(1))], `[[`, character(1), "entity_id")), method = "radix")
  nominated_excluded <- Filter(function(record) record$feature_key %in% nominated, excluded)
  duplicate_features <- included_keys[duplicated(included_ids)]
  universe <- list(
    ids = universe_ids, source = "measured", target_namespace = target_namespace,
    taxon_id = taxon_id, mapping_rule = "unique_resolved_entity",
    included = unname(included), excluded = unname(excluded),
    mapping_counts = list(
      feature_count = as.integer(length(feature_rows)),
      uniquely_resolved_feature_count = as.integer(length(included)),
      unique_entity_count = as.integer(length(universe_ids)),
      ambiguous_feature_count = as.integer(sum(vapply(excluded, function(record) {
        identical(record$reason, "ambiguous_target_entity")
      }, logical(1)))),
      unmapped_feature_count = as.integer(sum(vapply(excluded, function(record) {
        record$reason %in% c("unmapped", "target_namespace_unmapped")
      }, logical(1)))),
      duplicate_entity_feature_count = as.integer(length(duplicate_features))
    )
  )
  list(universe = universe, selected_ids = selected_ids,
       nominated_feature_keys = as.list(nominated), nominated_excluded = unname(nominated_excluded))
}

triomics_ora_resolve_claim <- function(evidence_bundle, claim_spec) {
  abort_lineage <- function(message, key = "claim") {
    triomics_ora_abort("D_ORA_LINEAGE_INVALID", message, key)
  }
  if (!is.list(evidence_bundle) || !is.list(claim_spec) ||
      !triomics_ora_text(evidence_bundle$analysis_id) ||
      !identical(as.character(claim_spec$analysis_id), as.character(evidence_bundle$analysis_id)) ||
      !triomics_ora_text(claim_spec$block_id) ||
      !triomics_ora_text(claim_spec$source_branch) ||
      !as.character(claim_spec$source_branch) %in% c("primary_statistics", "integration_selection") ||
      !triomics_ora_text(claim_spec$claim_id)) {
    abort_lineage("The ORA protocol claim does not match one verified analysis and branch identity.")
  }
  branch <- as.character(claim_spec$source_branch)
  block_id <- as.character(claim_spec$block_id)
  claim_id <- as.character(claim_spec$claim_id)
  feature_keys <- character()
  producer_rows <- list()
  family_id <- NULL
  if (identical(branch, "primary_statistics")) {
    blocks <- evidence_bundle$b$block_results$blocks
    block <- if (is.list(blocks)) blocks[[block_id]] else NULL
    if (!is.list(block)) abort_lineage("The protocol block is absent from verified B results.", block_id)
    candidates <- Filter(function(group) is.list(group) && !is.null(group[[claim_id]]),
                         list(block$contrasts, block$joint_claims))
    if (length(candidates) != 1L) abort_lineage("The protocol claim does not resolve to exactly one B claim.", claim_id)
    producer_claim <- candidates[[1L]][[claim_id]]
    family <- producer_claim$family
    rows <- producer_claim$rows
    if (!is.list(family) || !triomics_ora_text(as.character(family$family_id)) || !is.list(rows)) {
      abort_lineage("The B claim lacks a complete family and tested rows.", claim_id)
    }
    family_members <- triomics_ora_characters(family$member_ids)
    row_keys <- vapply(rows, function(row) as.character(row$feature_key %||% ""), character(1))
    if (any(!nzchar(family_members)) || anyDuplicated(family_members) || any(!nzchar(row_keys)) ||
        anyDuplicated(row_keys) || !setequal(family_members, row_keys) ||
        length(family_members) != length(row_keys) ||
        (is.numeric(family$n_declared) && length(family$n_declared) == 1L &&
         !is.na(family$n_declared) && family$n_declared != length(family_members))) {
      abort_lineage("The complete B claim rows do not match its frozen family membership.", claim_id)
    }
    ledger <- evidence_bundle$b$family_ledger$families
    ledger_matches <- Filter(function(record) identical(as.character(record$block_id), block_id) &&
                               identical(as.character(record$claim_id), claim_id), ledger)
    if (length(ledger_matches) != 1L ||
        !identical(triomics_canonical_json(ledger_matches[[1L]]$family), triomics_canonical_json(family))) {
      abort_lineage("The B family ledger does not match the requested claim.", claim_id)
    }
    feature_keys <- family_members
    family_id <- as.character(family$family_id)
    producer_rows <- stats::setNames(rows, row_keys)
  } else {
    integration_id <- as.character(claim_spec$integration_id %||% "")
    if (!triomics_ora_text(integration_id) || !identical(integration_id, claim_id)) {
      abort_lineage("An integration-selection claim must identify its exact C integration.", claim_id)
    }
    integrations <- evidence_bundle$c$integration_evidence$integrations
    matches <- Filter(function(item) identical(as.character(item$integration_id), integration_id), integrations)
    if (length(matches) != 1L || !identical(as.character(matches[[1L]]$status), "complete")) {
      abort_lineage("The protocol integration is absent or incomplete in verified C evidence.", integration_id)
    }
    integration <- matches[[1L]]
    feature_map <- integration$eligibility$feature_map
    if (!is.list(feature_map) || is.null(names(feature_map)) ||
        !block_id %in% names(feature_map)) {
      abort_lineage("The C integration does not retain the full input feature map for this block.", block_id)
    }
    feature_keys <- triomics_ora_characters(feature_map[[block_id]])
    if (anyNA(feature_keys) || any(!nzchar(feature_keys)) || anyDuplicated(feature_keys)) {
      abort_lineage("The C input feature map must contain unique feature keys.", block_id)
    }
    producer_rows <- stats::setNames(lapply(feature_keys, function(feature_key) {
      list(feature_key = feature_key, estimation_status = "verified_integration_input")
    }), feature_keys)
  }
  if (!length(feature_keys)) abort_lineage("The producer claim has no full measured feature background.", claim_id)
  registry <- evidence_bundle$feature_registry
  feature_records <- lapply(feature_keys, function(feature_key) {
    feature <- registry[[feature_key]]
    if (!is.list(feature) || !identical(as.character(feature$block_id), block_id)) {
      abort_lineage("A producer feature is absent from the FeatureRegistry block.", feature_key)
    }
    source_id <- as.character(feature$source_id %||% "")
    source <- evidence_bundle$source_by_id[[source_id]]
    if (!is.list(source)) abort_lineage("A producer feature has no verified SourceManifest.", source_id)
    producer <- producer_rows[[feature_key]]
    list(
      feature_key = feature_key, original_id = as.character(feature$original_id %||% ""),
      source_id = source_id, source_role = as.character(source$role %||% ""),
      source_namespace = as.character(feature$id_namespace %||% ""),
      taxon_id = as.character(feature$taxon_id %||% ""),
      taxon_status = as.character(feature$taxon_status %||% "unknown"),
      resolution = as.character(feature$structural_resolution %||% ""),
      resolution_status = as.character(feature$resolution_status %||% "unknown"),
      capability_status = as.character(feature$capability_status %||% "unknown"),
      capabilities = as.list(triomics_ora_characters(feature$capabilities)),
      measured_or_derived = as.character(feature$measured_or_derived %||% ""),
      producer_status = as.character(producer$estimation_status %||% "")
    )
  })
  nominations <- evidence_bundle$c$candidate_nominations$branches[[branch]]$nominations
  if (!is.list(nominations)) abort_lineage("The referenced C nomination branch is absent.", branch)
  matching_nominations <- list()
  for (nomination in nominations) {
    if (!is.list(nomination) || !identical(as.character(nomination$source_branch), branch) ||
        !is.list(nomination$result_ref)) {
      abort_lineage("A branch nomination does not retain its producer identity.", branch)
    }
    reference <- nomination$result_ref
    matches_identity <- identical(as.character(reference$block_id %||% ""), block_id) &&
      if (identical(branch, "primary_statistics")) {
        identical(as.character(reference$claim_id %||% ""), claim_id)
      } else {
        identical(as.character(reference$integration_id %||% ""), claim_id)
      }
    if (!matches_identity) next
    if (identical(branch, "primary_statistics")) {
      triomics_d_resolve_primary_nomination(
        evidence_bundle$b$block_results, evidence_bundle$b$family_ledger,
        nomination, evidence_bundle$analysis_id
      )
    } else {
      triomics_d_resolve_integration_nomination(evidence_bundle$c$integration_evidence, nomination)
    }
    matching_nominations[[length(matching_nominations) + 1L]] <- nomination
  }
  nomination_feature_keys <- unique(vapply(matching_nominations, function(nomination) {
    as.character(nomination$feature_key)
  }, character(1)))
  if (any(!nomination_feature_keys %in% feature_keys)) {
    abort_lineage("A verified nomination is not a member of the complete producer feature background.", claim_id)
  }
  list(
    analysis_id = as.character(evidence_bundle$analysis_id),
    preparation_context_id = as.character(evidence_bundle$preparation_context_id),
    representation_id = as.character(evidence_bundle$representation_id),
    block_id = block_id, source_branch = branch, claim_id = claim_id,
    integration_id = if (identical(branch, "integration_selection")) claim_id else NULL,
    family_id = family_id, feature_keys = feature_keys,
    feature_rows = feature_records, producer_rows = producer_rows,
    nominations = matching_nominations,
    nomination_feature_keys = nomination_feature_keys
  )
}
