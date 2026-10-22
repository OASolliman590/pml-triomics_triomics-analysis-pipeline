triomics_d_route_names <- function() {
  c("gene_reaction", "lipid_ontology", "ora", "mofa_native", "biopan")
}

triomics_d_route_character_values <- function(value) {
  if (is.null(value)) return(character())
  result <- as.character(unlist(value, use.names = FALSE))
  result[!is.na(result) & nzchar(result)]
}

triomics_d_validate_route_specs <- function(route_specs) {
  if (is.null(route_specs)) route_specs <- list()
  if (!is.list(route_specs) || (length(route_specs) &&
      (is.null(names(route_specs)) || any(!nzchar(names(route_specs))) || anyDuplicated(names(route_specs))))) {
    triomics_d_abort("D_ROUTE_INVALID", "D route declarations must be a uniquely named list.", "route_specs")
  }
  unknown <- setdiff(names(route_specs), triomics_d_route_names())
  if (length(unknown)) {
    triomics_d_abort("D_ROUTE_INVALID", "D received an unregistered annotation route.", unknown[[1L]])
  }
  for (route_name in names(route_specs)) {
    route <- route_specs[[route_name]]
    if (!is.list(route) || !is.logical(route$enabled) || length(route$enabled) != 1L || is.na(route$enabled)) {
      triomics_d_abort("D_ROUTE_INVALID", "Each D route needs one explicit enabled flag.", route_name)
    }
    if (isTRUE(route$enabled) && route_name %in% c("gene_reaction", "lipid_ontology")) {
      required <- c("namespaces", "organisms", "resolutions", "capabilities")
      if (any(!required %in% names(route)) ||
          any(vapply(required, function(field) !length(triomics_d_route_character_values(route[[field]])), logical(1)))) {
        triomics_d_abort("D_ROUTE_INVALID", "An enabled molecular route must declare namespace, organism, resolution, and capabilities.", route_name)
      }
    }
  }
  route_specs
}

triomics_d_route_compatibility <- function(feature, source, route) {
  if (!identical(as.character(source$role), "measurement_matrix")) {
    return(list(compatible = FALSE, reason = "source_role_not_measurement_matrix"))
  }
  if (!identical(as.character(feature$taxon_status), "declared")) {
    return(list(compatible = FALSE, reason = "taxon_unknown"))
  }
  if (!as.character(feature$id_namespace) %in% triomics_d_route_character_values(route$namespaces)) {
    return(list(compatible = FALSE, reason = "namespace_mismatch"))
  }
  if (!as.character(feature$taxon_id) %in% triomics_d_route_character_values(route$organisms)) {
    return(list(compatible = FALSE, reason = "organism_mismatch"))
  }
  if (!identical(as.character(feature$capability_status), "declared")) {
    return(list(compatible = FALSE, reason = "capability_unknown"))
  }
  if (!as.character(feature$structural_resolution) %in% triomics_d_route_character_values(route$resolutions)) {
    return(list(compatible = FALSE, reason = if (identical(as.character(feature$resolution_status), "unknown")) {
      "molecular_resolution_unknown"
    } else {
      "molecular_resolution_mismatch"
    }))
  }
  feature_capabilities <- triomics_d_route_character_values(feature$capabilities)
  required_capabilities <- triomics_d_route_character_values(route$capabilities)
  if (!length(required_capabilities) || any(!required_capabilities %in% feature_capabilities)) {
    return(list(compatible = FALSE, reason = "capability_not_declared"))
  }
  list(compatible = TRUE, reason = NULL)
}

triomics_d_find_b_claim <- function(bundle, nomination) {
  resolved <- triomics_d_resolve_primary_nomination(
    bundle$b$block_results, bundle$b$family_ledger, nomination, bundle$analysis_id
  )
  list(tested_rows = resolved$claim$rows, family = resolved$family_record$family,
       family_record = resolved$family_record)
}

triomics_d_find_c_selection <- function(bundle, nomination) {
  triomics_d_resolve_integration_nomination(bundle$c$integration_evidence, nomination)
}

triomics_d_nomination_evidence <- function(bundle, nomination) {
  feature <- bundle$feature_registry[[as.character(nomination$feature_key)]]
  source <- bundle$source_by_id[[as.character(feature$source_id)]]
  selection <- list(
    nomination = nomination,
    source_manifest = source,
    source_selection = list(
      selection_history = source$selection_history %||% NULL,
      selection_evidence = source$selection_evidence %||% NULL
    )
  )
  if (identical(as.character(nomination$source_branch), "primary_statistics")) {
    tested <- triomics_d_find_b_claim(bundle, nomination)
    selection$tested_rows <- tested$tested_rows
    selection$family <- tested$family
    selection$family_record <- tested$family_record
  } else {
    selected <- triomics_d_find_c_selection(bundle, nomination)
    selection$selection <- selected$selection
    selection$integration <- selected$integration
  }
  selection
}

triomics_d_route_evidence <- function(evidence_bundle, route_specs = list()) {
  if (!is.list(evidence_bundle) || !identical(evidence_bundle$artifact_type, "evidence_bundle") ||
      !triomics_d_nonempty_string(evidence_bundle$study_id) ||
      !triomics_d_nonempty_string(evidence_bundle$analysis_id)) {
    triomics_d_abort("D_EVIDENCE_INPUT_INVALID", "The annotation router needs a validated D evidence bundle.", "evidence_bundle")
  }
  route_specs <- triomics_d_validate_route_specs(route_specs)
  routes <- list()
  disabled <- 0L
  routed_ids <- character()
  incompatible <- list()
  for (route_name in names(route_specs)) {
    route <- route_specs[[route_name]]
    if (!isTRUE(route$enabled)) {
      disabled <- disabled + 1L
      routes[[route_name]] <- list(
        status = "not_requested", reason_code = "D_ROUTE_DISABLED", nominations = list(),
        candidate_count = 0L
      )
      next
    }
    if (!route_name %in% c("gene_reaction", "lipid_ontology")) {
      routes[[route_name]] <- list(
        status = "unavailable", reason_code = "D_ROUTE_CAPABILITY_NOT_IMPLEMENTED",
        nominations = list(), candidate_count = 0L
      )
      next
    }
    route_nominations <- list()
    for (nomination in evidence_bundle$nominations) {
      feature <- evidence_bundle$feature_registry[[as.character(nomination$feature_key)]]
      source <- evidence_bundle$source_by_id[[as.character(feature$source_id)]]
      compatibility <- triomics_d_route_compatibility(feature, source, route)
      if (!isTRUE(compatibility$compatible)) {
        incompatible[[length(incompatible) + 1L]] <- list(
          nomination_id = nomination$nomination_id,
          route = route_name,
          reason_code = compatibility$reason
        )
        next
      }
      evidence <- triomics_d_nomination_evidence(evidence_bundle, nomination)
      route_nominations[[length(route_nominations) + 1L]] <- list(
        nomination_id = nomination$nomination_id,
        feature_key = nomination$feature_key,
        source_branch = nomination$source_branch,
        namespace = feature$id_namespace,
        organism = feature$taxon_id,
        molecular_resolution = feature$structural_resolution,
        capabilities = feature$capabilities,
        evidence = evidence
      )
      routed_ids <- c(routed_ids, as.character(nomination$nomination_id))
    }
    routes[[route_name]] <- list(
      status = "complete", reason_code = NULL, nominations = route_nominations,
      candidate_count = as.integer(length(route_nominations))
    )
  }
  routed_unique <- unique(routed_ids)
  list(
    schema_version = "2.0", artifact_type = "annotation_routing_result",
    status = "complete", study_id = evidence_bundle$study_id,
    analysis_id = evidence_bundle$analysis_id,
    preparation_context_id = evidence_bundle$preparation_context_id,
    representation_id = evidence_bundle$representation_id,
    interpretation = "no_biological_absence_inferred",
    immutable_inputs = evidence_bundle$immutable_inputs,
    routes = routes,
    imported_evidence = evidence_bundle$imported_evidence,
    inherited_evidence = list(
      b = evidence_bundle$b,
      source_manifests = evidence_bundle$source_manifests,
      feature_registry = evidence_bundle$feature_registry,
      tested_claims = evidence_bundle$tested_claims,
      measured_rows = evidence_bundle$measured_rows,
      integration_evidence = evidence_bundle$c$integration_evidence,
      integration_comparisons = evidence_bundle$c$integration_comparisons,
      evaluation_evidence = evidence_bundle$c$evaluation_evidence,
      stage_status = evidence_bundle$c$stage_status
    ),
    incompatible_nominations = incompatible,
    enrichment = list(
      status = "not_requested",
      reason_code = if (disabled == length(route_specs)) "D_ROUTES_DISABLED" else "D_T01_ENRICHMENT_DEFERRED",
      test_count = 0L,
      results = list()
    ),
    accounting = c(
      evidence_bundle$accounting,
      list(
        routes_disabled = as.integer(disabled),
        routed_nomination_count = as.integer(length(routed_unique)),
        unrouted_nomination_count = as.integer(length(setdiff(
          vapply(evidence_bundle$nominations, function(item) as.character(item$nomination_id), character(1)),
          routed_unique
        ))),
        imported_correction_inputs = 0L,
        imported_enrichment_inputs = 0L
      )
    )
  )
}

triomics_d_output_input_pairs <- function(provenance) {
  required_b <- c("block_results", "family_ledger", "candidate_nominations")
  if (!is.list(provenance) ||
      !triomics_d_nonempty_string(provenance$b_bundle_artifact_id) ||
      !triomics_sha256_text(as.character(provenance$b_bundle_sha256)) ||
      !triomics_sha256_text(as.character(provenance$b_bundle_envelope_sha256)) ||
      !is.list(provenance$b_artifacts) || !all(required_b %in% names(provenance$b_artifacts)) ||
      !identical(as.character(provenance$c_manifest_artifact_id), "c06-handoff-manifest") ||
      !triomics_sha256_text(as.character(provenance$c_manifest_sha256)) ||
      !triomics_sha256_text(as.character(provenance$c_manifest_envelope_sha256))) {
    triomics_d_abort("D_HANDOFF_INVALID", "D output requires complete immutable B/C payload and envelope references.",
                     "immutable_inputs")
  }
  inputs <- list(
    list(artifact_id = as.character(provenance$b_bundle_artifact_id),
         sha256 = as.character(provenance$b_bundle_sha256))
  )
  for (name in required_b) {
    artifact <- provenance$b_artifacts[[name]]
    if (!is.list(artifact) || !triomics_d_nonempty_string(artifact$artifact_id) ||
        !triomics_sha256_text(as.character(artifact$sha256)) ||
        !triomics_sha256_text(as.character(artifact$envelope_sha256))) {
      triomics_d_abort("D_HANDOFF_INVALID", "A B-T07 member lacks its immutable payload or envelope reference.",
                       paste0("immutable_inputs.b_artifacts.", name))
    }
    inputs[[length(inputs) + 1L]] <- list(
      artifact_id = as.character(artifact$artifact_id), sha256 = as.character(artifact$sha256)
    )
  }
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = as.character(provenance$c_manifest_artifact_id),
    sha256 = as.character(provenance$c_manifest_sha256)
  )
  unname(inputs)
}

triomics_d_write_evidence_result <- function(result, output_root, config_sha256,
                                            producer_commit, privacy_class,
                                            run_id = "d-t01") {
  output_parts <- if (triomics_d_nonempty_string(output_root)) {
    strsplit(output_root, "[/\\\\]+")[[1L]]
  } else {
    character()
  }
  if (!is.list(result) || !identical(result$artifact_type, "annotation_routing_result") ||
      !triomics_d_nonempty_string(output_root) ||
      any(output_parts == "..") ||
      !triomics_sha256_text(as.character(config_sha256)) ||
      !triomics_d_nonempty_string(producer_commit) ||
      !triomics_d_nonempty_string(privacy_class) ||
      !privacy_class %in% c("synthetic", "sensitive") ||
      !triomics_d_nonempty_string(run_id)) {
    triomics_d_abort("D_OUTPUT_INVALID", "D output needs a typed result, local root, exact config hash, producer, and privacy class.",
                     "output_root")
  }
  inputs <- triomics_d_output_input_pairs(result$immutable_inputs)
  root <- triomics_d_secure_path(
    output_root, "D_OUTPUT_INVALID", "D output root is invalid or crosses a symlink.",
    "output_root", directory = TRUE
  )
  root <- triomics_d_create_directory_safely(
    root, "D_OUTPUT_INVALID", "D output root could not be created safely.", "output_root"
  )
  d_root <- file.path(root, "D")
  d_root <- triomics_d_create_directory_safely(
    d_root, "D_OUTPUT_INVALID", "D output directory could not be created safely.", "output_root"
  )
  if (!dir.exists(d_root)) {
    triomics_d_abort("D_OUTPUT_INVALID", "The local D output directory could not be created.", "output_root")
  }
  payload <- result
  payload$config_sha256 <- as.character(config_sha256)
  payload$privacy_class <- as.character(privacy_class)
  payload_path <- triomics_d_secure_path(
    file.path(d_root, "evidence-routing.json"), "D_OUTPUT_INVALID",
    "D output payload path is invalid or crosses a symlink.", "output_root"
  )
  envelope_path <- triomics_d_secure_path(
    file.path(d_root, "evidence-routing.envelope.json"), "D_OUTPUT_INVALID",
    "D output envelope path is invalid or crosses a symlink.", "output_root"
  )
  if (dir.exists(payload_path) || dir.exists(envelope_path)) {
    triomics_d_abort("D_OUTPUT_INVALID", "D output payload and envelope paths must be files.", "output_root")
  }
  triomics_write_json_atomic(payload, payload_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "annotation_routing_result",
    artifact_id = "d-evidence-routing",
    study_id = as.character(result$study_id),
    run_id = run_id,
    status = as.character(result$status),
    config_sha256 = as.character(config_sha256),
    payload = payload_path,
    producer_commit = producer_commit,
    inputs = inputs,
    reason_codes = if (is.list(result$enrichment) && !is.null(result$enrichment$reason_code)) {
      as.character(result$enrichment$reason_code)
    } else {
      character()
    },
    privacy_class = as.character(privacy_class)
  )
  envelope$analysis_id <- as.character(result$analysis_id)
  envelope$preparation_context_id <- as.character(result$preparation_context_id)
  envelope$representation_id <- as.character(result$representation_id)
  triomics_write_envelope(envelope, envelope_path)
  list(
    payload_path = payload_path,
    envelope_path = envelope_path,
    payload_sha256 = triomics_sha256(payload_path),
    envelope_sha256 = triomics_sha256(envelope_path),
    status = as.character(result$status)
  )
}

triomics_annotate_evidence <- function(evidence_bundle, annotation_spec = list()) {
  provenance <- if (is.list(evidence_bundle)) evidence_bundle$immutable_inputs else NULL
  required_b <- c("block_results", "family_ledger", "candidate_nominations")
  b_refs <- if (is.list(provenance)) provenance$b_artifacts else NULL
  if (!is.list(provenance) ||
      !triomics_sha256_text(as.character(provenance$b_bundle_sha256)) ||
      !triomics_sha256_text(as.character(provenance$c_manifest_sha256)) ||
      !triomics_sha256_text(as.character(provenance$c_manifest_envelope_sha256)) ||
      !is.list(b_refs) || !all(required_b %in% names(b_refs)) ||
      any(!vapply(b_refs[required_b], function(ref) {
        is.list(ref) && triomics_d_nonempty_string(ref$artifact_id) &&
          triomics_sha256_text(as.character(ref$sha256))
      }, logical(1)))) {
    triomics_d_abort("D_HANDOFF_INVALID", "The annotation entry point accepts only B/C evidence loaded from hash-verified immutable handoffs.",
                     "evidence_bundle.immutable_inputs")
  }
  if (!is.list(annotation_spec)) {
    triomics_d_abort("D_ROUTE_INVALID", "The annotation request must be a typed list.", "annotation_spec")
  }
  if (!is.null(annotation_spec$routes)) {
    return(triomics_d_route_evidence(evidence_bundle, annotation_spec$routes))
  }
  requests <- annotation_spec$requests %||% list()
  if (!is.list(requests) || length(requests)) {
    triomics_d_abort(
      "D_ROUTE_CAPABILITY_NOT_IMPLEMENTED",
      "Configured annotation requests need an explicit packet-qualified namespace, organism, resolution, and capability contract.",
      "annotation_spec.requests"
    )
  }
  disabled_routes <- stats::setNames(lapply(triomics_d_route_names(), function(route_name) {
    list(enabled = FALSE)
  }), triomics_d_route_names())
  triomics_d_route_evidence(evidence_bundle, disabled_routes)
}

annotate_evidence <- triomics_annotate_evidence
