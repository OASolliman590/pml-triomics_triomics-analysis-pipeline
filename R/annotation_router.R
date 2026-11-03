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
    publication_context = evidence_bundle$publication_context,
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
  required_c <- c("candidate_nominations", "integration_evidence", "integration_comparisons",
                  "evaluation_evidence", "stage_status")
  if (!is.list(provenance) ||
      !triomics_d_nonempty_string(provenance$b_bundle_artifact_id) ||
      !triomics_sha256_text(as.character(provenance$b_bundle_sha256)) ||
      !triomics_sha256_text(as.character(provenance$b_bundle_envelope_sha256)) ||
      !is.list(provenance$b_artifacts) || !all(required_b %in% names(provenance$b_artifacts)) ||
      !identical(as.character(provenance$c_manifest_artifact_id), "c06-handoff-manifest") ||
      !triomics_sha256_text(as.character(provenance$c_manifest_sha256)) ||
      !triomics_sha256_text(as.character(provenance$c_manifest_envelope_sha256)) ||
      !identical(as.character(provenance$b_profile_artifact_id), "b-t07-stage-handoff-provenance") ||
      !triomics_sha256_text(as.character(provenance$b_profile_sha256)) ||
      !triomics_sha256_text(as.character(provenance$b_profile_envelope_sha256)) ||
      !identical(as.character(provenance$c_profile_artifact_id), "c06-stage-handoff-provenance") ||
      !triomics_sha256_text(as.character(provenance$c_profile_sha256)) ||
      !triomics_sha256_text(as.character(provenance$c_profile_envelope_sha256)) ||
      !is.list(provenance$c_artifacts) || !all(required_c %in% names(provenance$c_artifacts))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "D output requires complete immutable B/C payload and envelope references.",
                     "immutable_inputs")
  }
  inputs <- list(
    list(artifact_id = as.character(provenance$b_bundle_artifact_id),
         sha256 = as.character(provenance$b_bundle_sha256)),
    list(artifact_id = paste0(as.character(provenance$b_bundle_artifact_id), "::envelope"),
         sha256 = as.character(provenance$b_bundle_envelope_sha256))
  )
  for (name in required_b) {
    artifact <- provenance$b_artifacts[[name]]
    if (!is.list(artifact) || !triomics_d_nonempty_string(artifact$artifact_id) ||
        !triomics_sha256_text(as.character(artifact$sha256)) ||
        !triomics_sha256_text(as.character(artifact$envelope_sha256))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A B-T07 member lacks its immutable payload or envelope reference.",
                       paste0("immutable_inputs.b_artifacts.", name))
    }
    inputs[[length(inputs) + 1L]] <- list(
      artifact_id = as.character(artifact$artifact_id), sha256 = as.character(artifact$sha256)
    )
    inputs[[length(inputs) + 1L]] <- list(
      artifact_id = paste0(as.character(artifact$artifact_id), "::envelope"),
      sha256 = as.character(artifact$envelope_sha256)
    )
  }
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = as.character(provenance$c_manifest_artifact_id),
    sha256 = as.character(provenance$c_manifest_sha256)
  )
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = paste0(as.character(provenance$c_manifest_artifact_id), "::envelope"),
    sha256 = as.character(provenance$c_manifest_envelope_sha256)
  )
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = as.character(provenance$b_profile_artifact_id),
    sha256 = as.character(provenance$b_profile_sha256)
  )
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = paste0(as.character(provenance$b_profile_artifact_id), "::envelope"),
    sha256 = as.character(provenance$b_profile_envelope_sha256)
  )
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = as.character(provenance$c_profile_artifact_id),
    sha256 = as.character(provenance$c_profile_sha256)
  )
  inputs[[length(inputs) + 1L]] <- list(
    artifact_id = paste0(as.character(provenance$c_profile_artifact_id), "::envelope"),
    sha256 = as.character(provenance$c_profile_envelope_sha256)
  )
  for (name in required_c) {
    artifact <- provenance$c_artifacts[[name]]
    if (!is.list(artifact) ||
        !identical(as.character(artifact$artifact_type), switch(
          name,
          candidate_nominations = "candidate_nominations",
          integration_evidence = "integration_evidence_bundle",
          integration_comparisons = "integration_comparisons",
          evaluation_evidence = "integration_evaluation_evidence",
          stage_status = "stage_c_status"
        )) ||
        !triomics_d_nonempty_string(artifact$artifact_id) ||
        !triomics_sha256_text(as.character(artifact$payload_sha256)) ||
        !triomics_sha256_text(as.character(artifact$envelope_sha256))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C-06 handoff member lacks its exact payload and envelope descriptor.",
                       paste0("immutable_inputs.c_artifacts.", name))
    }
    inputs[[length(inputs) + 1L]] <- list(
      artifact_id = as.character(artifact$artifact_id),
      sha256 = as.character(artifact$payload_sha256)
    )
    inputs[[length(inputs) + 1L]] <- list(
      artifact_id = paste0(as.character(artifact$artifact_id), "::envelope"),
      sha256 = as.character(artifact$envelope_sha256)
    )
  }
  unname(inputs)
}

triomics_d_publication_failpoint <- function(name) {
  configured <- getOption("triomics.d.publication.failpoint")
  if (is.character(configured) && length(configured) == 1L &&
      !is.na(configured) && identical(configured, name)) {
    triomics_d_abort("D_PUBLICATION_FAILED",
                     paste0("D publication failure was injected at ", name, "."),
                     paste0("publication.failpoint.", name))
  }
  invisible(TRUE)
}

triomics_d_read_raw_text <- function(path) {
  tryCatch(readChar(path, nchars = file.info(path)$size, useBytes = TRUE),
           error = function(error) NULL)
}

triomics_d_read_raw_bytes <- function(path) {
  tryCatch(readBin(path, what = "raw", n = file.info(path)$size),
           error = function(error) NULL)
}

triomics_d_write_raw_file <- function(path, value) {
  if (file.exists(path) || dir.exists(path) || triomics_is_symlink(path)) {
    triomics_d_abort("D_PUBLICATION_FAILED", "A D publication temporary path already exists.",
                     "publication.temporary")
  }
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(enc2utf8(value)), connection)
  invisible(path)
}

triomics_d_closed_object <- function(value, fields) {
  is.list(value) && !is.null(names(value)) && !anyDuplicated(names(value)) &&
    identical(sort(names(value), method = "radix"), sort(fields, method = "radix"))
}

triomics_d_generation_paths <- function(root, generation_id) {
  if (!triomics_sha256_text(as.character(generation_id))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "D generation identity must be a full SHA-256 digest.",
                     "generation_id")
  }
  generation <- file.path(root, "D", "generations", as.character(generation_id))
  list(
    generation = generation,
    manifest = file.path(generation, "generation-manifest.json"),
    payload = file.path(generation, "evidence-routing.json"),
    envelope = file.path(generation, "evidence-routing.envelope.json")
  )
}

triomics_d_valid_generation_member_path <- function(path) {
  if (!triomics_d_nonempty_string(path) || grepl("^(?:/|[A-Za-z]:)", path) ||
      grepl(intToUtf8(92L), path, fixed = TRUE) || grepl("//", path, fixed = TRUE) ||
      endsWith(path, "/")) {
    return(FALSE)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  length(parts) > 0L && all(nzchar(parts)) && !any(parts %in% c(".", ".."))
}

triomics_d_generation_inventory <- function(generation) {
  if (!dir.exists(generation) || triomics_is_symlink(generation)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "The immutable D generation directory is unsafe.",
                     "generation.directory")
  }
  files <- character()
  directories <- character()
  visit <- function(directory, prefix = "") {
    children <- list.files(directory, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    for (child in children) {
      relative <- if (nzchar(prefix)) paste0(prefix, "/", basename(child)) else basename(child)
      if (triomics_is_symlink(child)) {
        triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A D generation contains a symbolic link.", relative)
      }
      info <- file.info(child)
      if (is.na(info$isdir)) {
        triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A D generation contains an unreadable path.", relative)
      }
      if (isTRUE(info$isdir)) {
        directories <<- c(directories, relative)
        visit(child, relative)
      } else {
        files <<- c(files, relative)
      }
    }
  }
  visit(generation)
  list(files = sort(files, method = "radix"), directories = sort(directories, method = "radix"))
}

triomics_d_valid_artifact_descriptor <- function(value, has_type = FALSE) {
  fields <- c("artifact_id", "payload_sha256", "envelope_sha256")
  if (isTRUE(has_type)) fields <- c(fields, "artifact_type")
  triomics_d_closed_object(value, fields) &&
    triomics_d_nonempty_string(value$artifact_id) &&
    (!isTRUE(has_type) || triomics_d_nonempty_string(value$artifact_type)) &&
    triomics_sha256_text(as.character(value$payload_sha256)) &&
    triomics_sha256_text(as.character(value$envelope_sha256))
}

triomics_d_generation_provenance <- function(provenance) {
  triomics_d_output_input_pairs(provenance)
  b_names <- c("block_results", "family_ledger", "candidate_nominations")
  c_names <- c("candidate_nominations", "integration_evidence", "integration_comparisons",
               "evaluation_evidence", "stage_status")
  c_types <- c(
    candidate_nominations = "candidate_nominations",
    integration_evidence = "integration_evidence_bundle",
    integration_comparisons = "integration_comparisons",
    evaluation_evidence = "integration_evaluation_evidence",
    stage_status = "stage_c_status"
  )
  if (!is.list(provenance$b_artifacts) || !all(b_names %in% names(provenance$b_artifacts)) ||
      !is.list(provenance$c_artifacts) || !all(c_names %in% names(provenance$c_artifacts)) ||
      any(!vapply(b_names, function(name) {
        identical(as.character(provenance$b_artifacts[[name]]$artifact_type), name)
      }, logical(1))) ||
      any(!vapply(c_names, function(name) {
        identical(as.character(provenance$c_artifacts[[name]]$artifact_type), c_types[[name]])
      }, logical(1)))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "D output provenance has an unexpected B/C member type.",
                     "immutable_inputs")
  }
  list(
    b = list(
      bundle = list(
        artifact_id = as.character(provenance$b_bundle_artifact_id),
        payload_sha256 = as.character(provenance$b_bundle_sha256),
        envelope_sha256 = as.character(provenance$b_bundle_envelope_sha256)
      ),
      profile = list(
        artifact_id = as.character(provenance$b_profile_artifact_id),
        payload_sha256 = as.character(provenance$b_profile_sha256),
        envelope_sha256 = as.character(provenance$b_profile_envelope_sha256)
      ),
      members = lapply(provenance$b_artifacts[b_names], function(ref) {
        list(artifact_id = as.character(ref$artifact_id),
             artifact_type = as.character(ref$artifact_type),
             payload_sha256 = as.character(ref$sha256),
             envelope_sha256 = as.character(ref$envelope_sha256))
      })
    ),
    c = list(
      manifest = list(
        artifact_id = as.character(provenance$c_manifest_artifact_id),
        payload_sha256 = as.character(provenance$c_manifest_sha256),
        envelope_sha256 = as.character(provenance$c_manifest_envelope_sha256)
      ),
      profile = list(
        artifact_id = as.character(provenance$c_profile_artifact_id),
        payload_sha256 = as.character(provenance$c_profile_sha256),
        envelope_sha256 = as.character(provenance$c_profile_envelope_sha256)
      ),
      members = lapply(provenance$c_artifacts[c_names], function(ref) {
        list(artifact_id = as.character(ref$artifact_id),
             artifact_type = as.character(ref$artifact_type),
             payload_sha256 = as.character(ref$payload_sha256),
             envelope_sha256 = as.character(ref$envelope_sha256))
      })
    )
  )
}

triomics_d_validate_generation <- function(root, generation_id) {
  paths <- triomics_d_generation_paths(root, generation_id)
  manifest_path <- triomics_d_secure_path(
    paths$manifest, "D_OUTPUT_PATH_UNSAFE",
    "The D generation manifest path is unsafe or crosses a symlink.",
    "generation.manifest"
  )
  if (!file.exists(manifest_path) || dir.exists(manifest_path) ||
      !identical(triomics_sha256(manifest_path), generation_id)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D generation manifest does not match its immutable directory identity.",
                     "generation.manifest")
  }
  manifest_json <- triomics_d_read_raw_text(manifest_path)
  manifest_json_valid <- is.character(manifest_json) && isTRUE(tryCatch({
    triomics_provenance_json_scan(manifest_json)
    TRUE
  }, error = function(error) FALSE))
  manifest <- if (manifest_json_valid) {
    tryCatch(jsonlite::fromJSON(manifest_json, simplifyVector = FALSE),
             error = function(error) NULL)
  } else NULL
  manifest_fields <- c(
    "profile", "profile_version", "study_id", "run_id", "provenance",
    "producer", "status", "limitations", "members"
  )
  if (!is.list(manifest) || !triomics_d_closed_object(manifest, manifest_fields) ||
      !identical(manifest$profile, "triomics.output-generation-manifest") ||
      !identical(manifest$profile_version, "1.0") || !identical(manifest$status, "complete") ||
      !triomics_d_nonempty_string(manifest$study_id) ||
      !triomics_d_nonempty_string(manifest$run_id) ||
      !is.list(manifest$limitations) || !length(manifest$limitations) ||
      any(!vapply(manifest$limitations, triomics_d_nonempty_string, logical(1))) ||
      !identical(manifest_json, triomics_b07_prov_canonical_value(manifest))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D generation manifest is not a closed canonical complete manifest.",
                     "generation.manifest")
  }
  if (!triomics_d_closed_object(manifest$producer, c("package", "version", "commit")) ||
      !identical(manifest$producer$package, "triomics") ||
      !triomics_d_nonempty_string(manifest$producer$version) ||
      !triomics_d_nonempty_string(manifest$producer$commit) ||
      !triomics_d_closed_object(manifest$provenance, c("b", "c")) ||
      !triomics_d_closed_object(manifest$provenance$b, c("bundle", "profile", "members")) ||
      !triomics_d_closed_object(manifest$provenance$c, c("manifest", "profile", "members")) ||
      !triomics_d_valid_artifact_descriptor(manifest$provenance$b$bundle) ||
      !triomics_d_valid_artifact_descriptor(manifest$provenance$b$profile) ||
      !triomics_d_valid_artifact_descriptor(manifest$provenance$c$manifest) ||
      !triomics_d_valid_artifact_descriptor(manifest$provenance$c$profile)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D manifest omits required producer descriptors.",
                     "generation.provenance")
  }
  for (stage in c("b", "c")) {
    members <- manifest$provenance[[stage]]$members
    expected_member_names <- if (identical(stage, "b")) {
      c("block_results", "family_ledger", "candidate_nominations")
    } else {
      c("candidate_nominations", "integration_evidence", "integration_comparisons",
        "evaluation_evidence", "stage_status")
    }
    if (!is.list(members) || !setequal(names(members), expected_member_names) ||
        length(members) != length(expected_member_names) ||
        any(!vapply(members, triomics_d_valid_artifact_descriptor, logical(1), has_type = TRUE))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "The D manifest has incomplete B/C member descriptors.",
                       paste0("generation.provenance.", stage, ".members"))
    }
  }
  acceptance_limitations <- c(
    "This publication is not a C-06, M7, or G7 acceptance receipt.",
    "C-06 remains unaccepted."
  )
  required_limitations <- c(
    "C-07 controls remain not_run.",
    "C-09 remains unqualified and conditional."
  )
  published_limitations <- unlist(manifest$limitations, use.names = FALSE)
  if (!any(acceptance_limitations %in% published_limitations) ||
      !all(required_limitations %in% published_limitations)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D generation omits the required C-06/C-07/C-09 publication limitations.",
                     "generation.limitations")
  }
  member_fields <- c(
    "path", "scientific_role", "artifact_type", "media_type", "status",
    "size_bytes", "digest_algorithm", "digest"
  )
  if (!is.list(manifest$members) || length(manifest$members) != 2L) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D generation must contain exactly one payload and one envelope member.",
                     "generation.members")
  }
  expected_paths <- c("evidence-routing.envelope.json", "evidence-routing.json")
  member_paths <- vapply(manifest$members, function(member) as.character(member$path), character(1))
  if (!identical(member_paths, sort(unique(member_paths), method = "radix")) ||
      !identical(sort(member_paths, method = "radix"), expected_paths)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D generation member paths are incomplete or not canonical.",
                     "generation.members")
  }
  for (member in manifest$members) {
    allowed_fields <- if ("envelope_for_artifact_id" %in% names(member)) {
      c(member_fields, "envelope_for_artifact_id")
    } else member_fields
    if (!triomics_d_closed_object(member, allowed_fields) ||
        !triomics_d_valid_generation_member_path(as.character(member$path)) ||
        !triomics_d_nonempty_string(member$scientific_role) ||
        !triomics_d_nonempty_string(member$artifact_type) ||
        !identical(member$media_type, "application/json") ||
        !identical(member$status, "complete") ||
        !identical(member$digest_algorithm, "sha256") ||
        !triomics_sha256_text(as.character(member$digest)) ||
        !is.numeric(member$size_bytes) || length(member$size_bytes) != 1L ||
        is.na(member$size_bytes) || member$size_bytes < 0 ||
        member$size_bytes != trunc(member$size_bytes)) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A D generation member descriptor is incomplete or has an invalid type.",
                       "generation.members")
    }
    member_path <- triomics_d_secure_path(
      file.path(paths$generation, as.character(member$path)),
      "D_OUTPUT_PATH_UNSAFE", "A D generation member path is unsafe.",
      paste0("generation.members.", member$path)
    )
    if (!file.exists(member_path) || dir.exists(member_path) ||
        !identical(as.numeric(file.info(member_path)$size), as.numeric(member$size_bytes)) ||
        !identical(triomics_sha256(member_path), as.character(member$digest))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A D generation member failed exact size or SHA-256 read-back.",
                       paste0("generation.members.", member$path))
    }
  }
  inventory <- triomics_d_generation_inventory(paths$generation)
  expected_files <- sort(c("generation-manifest.json", member_paths), method = "radix")
  if (length(inventory$directories) || !identical(inventory$files, expected_files)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D generation contains an undisclosed file or directory.",
                     "generation.members")
  }
  payload_path <- triomics_d_secure_path(
    paths$payload, "D_OUTPUT_PATH_UNSAFE", "The D routing payload path is unsafe.",
    "generation.payload"
  )
  envelope_path <- triomics_d_secure_path(
    paths$envelope, "D_OUTPUT_PATH_UNSAFE", "The D routing envelope path is unsafe.",
    "generation.envelope"
  )
  payload <- tryCatch(jsonlite::read_json(payload_path, simplifyVector = FALSE),
                      error = function(error) NULL)
  envelope <- tryCatch(jsonlite::read_json(envelope_path, simplifyVector = FALSE),
                       error = function(error) NULL)
  payload_member <- Filter(function(member) identical(member$path, "evidence-routing.json"), manifest$members)
  envelope_member <- Filter(function(member) identical(member$path, "evidence-routing.envelope.json"),
                            manifest$members)
  expected_provenance <- tryCatch(triomics_d_generation_provenance(payload$immutable_inputs),
                                 error = function(error) NULL)
  expected_inputs <- tryCatch(triomics_d_output_input_pairs(payload$immutable_inputs),
                              error = function(error) NULL)
  if (length(payload_member) != 1L || !is.list(payload) || !is.list(envelope) ||
      !identical(payload$artifact_type, "annotation_routing_result") ||
      !identical(payload$artifact_id, "d-evidence-routing") ||
      !identical(payload$status, "complete") ||
      !identical(as.character(payload$study_id), as.character(manifest$study_id)) ||
      !triomics_d_nonempty_string(payload$analysis_id) ||
      !triomics_sha256_text(as.character(payload$config_sha256)) ||
      !is.character(payload$privacy_class) ||
      !payload$privacy_class %in% c("synthetic", "sensitive") ||
      !identical(envelope$artifact_type, "annotation_routing_result") ||
      !identical(envelope$artifact_id, "d-evidence-routing") ||
      !identical(as.character(envelope$run_id), as.character(manifest$run_id)) ||
      !identical(as.character(envelope$study_id), as.character(manifest$study_id)) ||
      !identical(as.character(envelope$analysis_id), as.character(payload$analysis_id)) ||
      !identical(as.character(envelope$config_sha256), as.character(payload$config_sha256)) ||
      !identical(as.character(envelope$payload_sha256), as.character(payload_member[[1L]]$digest)) ||
      is.null(expected_provenance) || is.null(expected_inputs) ||
      !identical(triomics_b07_prov_canonical_value(manifest$provenance),
                 triomics_b07_prov_canonical_value(expected_provenance)) ||
      !identical(triomics_b07_prov_canonical_value(envelope$inputs),
                 triomics_b07_prov_canonical_value(expected_inputs)) ||
      !identical(as.character(manifest$study_id), as.character(payload$study_id)) ||
      length(envelope_member) != 1L ||
      !identical(payload_member[[1L]]$artifact_type, "annotation_routing_result") ||
      !identical(payload_member[[1L]]$scientific_role, "annotation_routing_result") ||
      "envelope_for_artifact_id" %in% names(payload_member[[1L]]) ||
      !identical(envelope_member[[1L]]$artifact_type, "artifact_envelope") ||
      !identical(envelope_member[[1L]]$scientific_role, "evidence_envelope") ||
      !identical(envelope_member[[1L]]$envelope_for_artifact_id, "d-evidence-routing") ||
      !"envelope_for_artifact_id" %in% names(envelope_member[[1L]]) ||
      !isTRUE(triomics_validate_envelope(envelope_path))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D payload and envelope do not match their complete generation descriptor.",
                     "generation.envelope")
  }
  route_names <- triomics_d_route_names()
  if (!is.list(payload$routes) || !setequal(names(payload$routes), route_names) ||
      length(payload$routes) != length(route_names) ||
      !is.list(payload$inherited_evidence$evaluation_evidence$controls) ||
      any(!vapply(payload$inherited_evidence$evaluation_evidence$controls, function(control) {
        identical(as.character(control$status), "not_run")
      }, logical(1))) ||
      !identical(as.character(payload$inherited_evidence$stage_status$clinical_prediction$completion_policy),
                 "conditional")) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D routing generation omits route accounting or changes C-07/C-09 scientific status.",
                     "generation.payload")
  }
  list(manifest = manifest, payload = payload, envelope = envelope, paths = paths,
       manifest_sha256 = as.character(generation_id))
}

triomics_d_validate_current_pointer <- function(root, pointer_path) {
  pointer_path <- triomics_d_secure_path(
    pointer_path, "D_OUTPUT_PATH_UNSAFE",
    "The D current-generation pointer path is unsafe or crosses a symlink.",
    "publication.pointer"
  )
  if (!file.exists(pointer_path)) return(NULL)
  if (dir.exists(pointer_path) || triomics_is_symlink(pointer_path)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "The D current-generation pointer must be a regular file.",
                     "publication.pointer")
  }
  pointer_json <- triomics_d_read_raw_text(pointer_path)
  pointer_json_valid <- is.character(pointer_json) && isTRUE(tryCatch({
    triomics_provenance_json_scan(pointer_json)
    TRUE
  }, error = function(error) FALSE))
  pointer <- if (pointer_json_valid) tryCatch(
    jsonlite::fromJSON(pointer_json, simplifyVector = FALSE), error = function(error) NULL
  ) else NULL
  pointer_fields <- c("schema_version", "study_id", "run_id", "generation_id", "generation_manifest")
  if (!triomics_d_closed_object(pointer, pointer_fields) ||
      !identical(pointer$schema_version, "1.0") ||
      !triomics_d_nonempty_string(pointer$study_id) ||
      !triomics_d_nonempty_string(pointer$run_id) ||
      !triomics_sha256_text(as.character(pointer$generation_id)) ||
      !triomics_d_closed_object(pointer$generation_manifest,
                                c("path", "size_bytes", "digest_algorithm", "digest")) ||
      !identical(pointer$generation_manifest$path, file.path("D", "generations",
                                                 as.character(pointer$generation_id),
                                                 "generation-manifest.json")) ||
      !identical(pointer$generation_manifest$digest_algorithm, "sha256") ||
      !identical(as.character(pointer$generation_manifest$digest), as.character(pointer$generation_id)) ||
      !is.numeric(pointer$generation_manifest$size_bytes) ||
      length(pointer$generation_manifest$size_bytes) != 1L ||
      is.na(pointer$generation_manifest$size_bytes) ||
      pointer$generation_manifest$size_bytes < 0 ||
      pointer$generation_manifest$size_bytes != trunc(pointer$generation_manifest$size_bytes)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D current-generation pointer is not a closed manifest reference.",
                     "publication.pointer")
  }
  manifest_path <- triomics_d_secure_path(
    file.path(root, pointer$generation_manifest$path), "D_OUTPUT_PATH_UNSAFE",
    "The D pointer manifest path is unsafe.", "publication.pointer.manifest"
  )
  if (!file.exists(manifest_path) || dir.exists(manifest_path) ||
      !identical(as.numeric(file.info(manifest_path)$size),
                 as.numeric(pointer$generation_manifest$size_bytes)) ||
      !identical(triomics_sha256(manifest_path), as.character(pointer$generation_manifest$digest))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The current pointer does not bind its exact generation manifest bytes.",
                     "publication.pointer.manifest")
  }
  generation <- triomics_d_validate_generation(root, as.character(pointer$generation_id))
  if (!identical(as.character(generation$manifest$study_id), as.character(pointer$study_id)) ||
      !identical(as.character(generation$manifest$run_id), as.character(pointer$run_id))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The D pointer identity differs from its complete generation.",
                     "publication.pointer")
  }
  list(
    pointer = pointer,
    pointer_sha256 = triomics_sha256(pointer_path),
    generation = generation
  )
}

triomics_d_validate_publication_context <- function(result, root, config_sha256,
                                                    privacy_class, run_id) {
  context <- result$publication_context
  if (!is.list(context) || !identical(as.character(context$root), root) ||
      !identical(as.character(context$current_config_sha256), as.character(config_sha256)) ||
      !identical(as.character(result$study_id), as.character(context$b_profile$profile$study_id)) ||
      !identical(as.character(result$analysis_id),
                 as.character(context$c_profile$envelope$analysis_id)) ||
      !triomics_sha256_text(as.character(config_sha256)) ||
      !identical(as.character(privacy_class), as.character(result$privacy_class %||% privacy_class)) ||
      !triomics_d_nonempty_string(run_id)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "D publication needs the exact verified same-root B/C readback context.",
                     "publication.provenance")
  }
  immutable <- result$immutable_inputs
  if (!is.list(immutable) ||
      !identical(as.character(immutable$b_bundle_sha256), as.character(context$b_bundle_sha256)) ||
      !identical(as.character(immutable$b_bundle_envelope_sha256), as.character(context$b_bundle_envelope_sha256)) ||
      !identical(as.character(immutable$b_profile_sha256), as.character(context$b_profile$payload_sha256)) ||
      !identical(as.character(immutable$b_profile_envelope_sha256), as.character(context$b_profile$envelope_sha256)) ||
      !identical(as.character(immutable$c_profile_sha256), as.character(context$c_profile$payload_sha256)) ||
      !identical(as.character(immutable$c_profile_envelope_sha256), as.character(context$c_profile$envelope_sha256))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "D output references do not match the verified B/C producer context.",
                     "publication.provenance")
  }
  checks <- list(
    list(context$b_bundle_path, context$b_bundle_sha256, "B.bundle"),
    list(context$b_bundle_envelope_path, context$b_bundle_envelope_sha256, "B.bundle.envelope"),
    list(context$b_profile$path, context$b_profile$payload_sha256, "B.provenance"),
    list(context$b_profile$envelope_path, context$b_profile$envelope_sha256, "B.provenance.envelope"),
    list(context$c_manifest_path, immutable$c_manifest_sha256, "C.manifest"),
    list(context$c_manifest_envelope_path, immutable$c_manifest_envelope_sha256, "C.manifest.envelope"),
    list(context$c_profile$path, context$c_profile$payload_sha256, "C.provenance"),
    list(context$c_profile$envelope_path, context$c_profile$envelope_sha256, "C.provenance.envelope")
  )
  for (check in checks) {
    path <- triomics_d_secure_path(
      check[[1L]], "D_OUTPUT_PATH_UNSAFE",
      "An exact B/C source path became unsafe before D publication.", check[[3L]]
    )
    if (!file.exists(path) || dir.exists(path) ||
        !identical(triomics_sha256(path), as.character(check[[2L]]))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A B/C source changed after D readback.",
                       check[[3L]])
    }
  }
  for (name in c("block_results", "family_ledger", "candidate_nominations")) {
    ref <- context$b_artifacts[[name]]
    payload_path <- triomics_d_child_path(root, as.character(ref$path), paste0("B.", name))
    envelope_path <- triomics_d_child_path(root, as.character(ref$envelope_path),
                                           paste0("B.", name, ".envelope"))
    if (is.null(payload_path) || is.null(envelope_path) ||
        !identical(triomics_sha256(payload_path), as.character(ref$sha256)) ||
        !identical(triomics_sha256(envelope_path), as.character(ref$envelope_sha256)) ||
        !isTRUE(triomics_validate_envelope(envelope_path))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A B-T07 member changed after D readback.",
                       paste0("B.", name))
    }
  }
  for (name in names(immutable$c_artifacts)) {
    ref <- immutable$c_artifacts[[name]]
    payload_path <- triomics_d_child_path(file.path(root, "D"), as.character(ref$payload_file),
                                          paste0("C.", name))
    envelope_path <- triomics_d_child_path(file.path(root, "D"), as.character(ref$envelope_file),
                                           paste0("C.", name, ".envelope"))
    payload <- if (is.null(payload_path)) NULL else tryCatch(
      jsonlite::read_json(payload_path, simplifyVector = FALSE), error = function(error) NULL
    )
    envelope <- if (is.null(envelope_path)) NULL else tryCatch(
      jsonlite::read_json(envelope_path, simplifyVector = FALSE), error = function(error) NULL
    )
    if (is.null(payload_path) || is.null(envelope_path) || !is.list(payload) || !is.list(envelope) ||
        !identical(as.character(payload$artifact_type), as.character(ref$artifact_type)) ||
        !identical(as.character(envelope$artifact_type), as.character(ref$artifact_type)) ||
        !identical(as.character(envelope$artifact_id), as.character(ref$artifact_id)) ||
        !identical(triomics_sha256(payload_path), as.character(ref$payload_sha256)) ||
        !identical(triomics_sha256(envelope_path), as.character(ref$envelope_sha256)) ||
        !identical(as.character(envelope$payload_sha256), as.character(ref$payload_sha256)) ||
        !isTRUE(triomics_validate_envelope(envelope_path))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C-06 handoff member changed after D readback.",
                       paste0("C.", name))
    }
  }
  b_raw <- triomics_d_read_raw_text(context$b_profile$path)
  c_raw <- triomics_d_read_raw_text(context$c_profile$path)
  if (!is.character(b_raw) || !is.character(c_raw) ||
      !isTRUE(tryCatch(triomics_validate_stage_handoff_provenance(
        b_raw, "b-t07-stage-handoff-provenance",
        "b-t07-stage-handoff-provenance-envelope"
      ), error = function(error) FALSE)) ||
      !isTRUE(tryCatch(triomics_validate_stage_handoff_provenance(
        c_raw, "c06-stage-handoff-provenance",
        "c06-stage-handoff-provenance-envelope"
      ), error = function(error) FALSE))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "A B/C provenance frame failed exact raw-JSON readback.",
                     "publication.provenance")
  }
  invisible(TRUE)
}

triomics_d_publish_evidence_generation <- function(result, output_root, config_sha256,
                                                    producer_commit, privacy_class,
                                                    run_id = "d-t01",
                                                    lock_held = FALSE) {
  if (!is.list(result) || !identical(result$artifact_type, "annotation_routing_result") ||
      !triomics_d_nonempty_string(output_root) ||
      !triomics_d_nonempty_string(producer_commit) ||
      !triomics_d_nonempty_string(privacy_class) ||
      !privacy_class %in% c("synthetic", "sensitive") ||
      !triomics_d_nonempty_string(run_id)) {
    triomics_d_abort("D_PUBLICATION_FAILED", "D output needs a verified routing result, local root, producer, and privacy class.",
                     "output_root")
  }
  root <- triomics_d_secure_path(
    output_root, "D_OUTPUT_PATH_UNSAFE", "D output root is invalid or crosses a symlink.",
    "output_root", directory = TRUE
  )
  if (!dir.exists(root)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "D publication requires the existing same-root B/C output directory.",
                     "output_root")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  d_root <- file.path(root, "D")
  generation_root <- file.path(d_root, "generations")
  registry <- file.path(root, "registries")
  pointer_path <- file.path(registry, "current-D.json")
  lock_path <- file.path(registry, ".D-publication.lock")
  for (path in c(registry, lock_path)) {
    triomics_d_secure_path(path, "D_OUTPUT_PATH_UNSAFE",
                           "The D publication lock path is unsafe or crosses a symlink.",
                           "publication.lock", directory = identical(path, registry))
  }
  if (!dir.exists(registry)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "The same-root B/C run registry is required for D publication.",
                     "publication.registry")
  }
  if (isTRUE(lock_held) &&
      (!dir.exists(lock_path) || triomics_is_symlink(lock_path))) {
    triomics_d_abort("D_PUBLICATION_FAILED", "D was asked to publish without holding its exclusive lock.",
                     "publication.lock")
  }
  acquired_lock <- FALSE
  if (!isTRUE(lock_held)) {
    if (file.exists(lock_path) || dir.exists(lock_path) || triomics_is_symlink(lock_path) ||
        !dir.create(lock_path, recursive = FALSE, showWarnings = FALSE)) {
      triomics_d_abort("D_PUBLICATION_FAILED", "Another D publisher holds the exclusive publication lock.",
                       "publication.lock")
    }
    acquired_lock <- TRUE
    on.exit(unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
  }
  if (!identical(as.character(result$config_sha256 %||% config_sha256), as.character(config_sha256))) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "D publication configuration hash differs from its verified request.",
                     "config_sha256")
  }
  triomics_d_validate_publication_context(result, root, config_sha256, privacy_class, run_id)
  inputs <- triomics_d_output_input_pairs(result$immutable_inputs)
  if (!identical(as.character(result$status), "complete") ||
      !identical(as.character(result$inherited_evidence$stage_status$clinical_prediction$completion_policy),
                 "conditional") ||
      any(!vapply(result$inherited_evidence$evaluation_evidence$controls, function(control) {
        identical(as.character(control$status), "not_run")
      }, logical(1)))) {
    triomics_d_abort("D_PUBLICATION_FAILED", "D-T01 publication requires complete routing accounting and preserved partial C status.",
                     "result.status")
  }
  for (path in c(d_root, generation_root, registry, pointer_path, lock_path)) {
    triomics_d_secure_path(
      path, "D_OUTPUT_PATH_UNSAFE",
      "A D publication path is unsafe or crosses a symlink.", "publication.paths",
      directory = path %in% c(d_root, generation_root, registry, lock_path)
    )
  }
  if (!dir.exists(d_root) ||
      (file.exists(generation_root) && !dir.exists(generation_root)) ||
      !dir.exists(registry) ||
      (file.exists(pointer_path) && dir.exists(pointer_path))) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A D publication directory or pointer has an incompatible type.",
                     "publication.paths")
  }
  old_current <- triomics_d_validate_current_pointer(root, pointer_path)
  old_pointer_sha256 <- if (is.null(old_current)) NULL else old_current$pointer_sha256
  triomics_d_create_directory_safely(
    generation_root, "D_OUTPUT_PATH_UNSAFE",
    "The D generation directory could not be created safely.", "publication.generations"
  )
  temporary_generation <- tempfile(pattern = ".D-generation-", tmpdir = generation_root)
  temporary_pointer <- tempfile(pattern = ".current-D-", tmpdir = registry, fileext = ".tmp")
  payload_path <- file.path(temporary_generation, "evidence-routing.json")
  envelope_path <- file.path(temporary_generation, "evidence-routing.envelope.json")
  manifest_path <- file.path(temporary_generation, "generation-manifest.json")
  for (path in c(temporary_generation, payload_path, envelope_path, manifest_path, temporary_pointer)) {
    triomics_d_secure_path(path, "D_OUTPUT_PATH_UNSAFE",
                           "A planned D generation or pointer path is unsafe.",
                           "publication.planned_paths",
                           directory = identical(path, temporary_generation))
  }
  if (any(vapply(c(temporary_generation, payload_path, envelope_path, manifest_path, temporary_pointer),
                 function(path) file.exists(path) || dir.exists(path) || triomics_is_symlink(path),
                 logical(1)))) {
    triomics_d_abort("D_PUBLICATION_FAILED", "A planned D temporary path already exists.",
                     "publication.planned_paths")
  }
  on.exit({
    if (dir.exists(temporary_generation)) unlink(temporary_generation, recursive = TRUE, force = TRUE)
    if (!is.null(temporary_pointer) && file.exists(temporary_pointer)) unlink(temporary_pointer)
  }, add = TRUE)
  if (!dir.create(temporary_generation, recursive = FALSE, showWarnings = FALSE)) {
    triomics_d_abort("D_PUBLICATION_FAILED", "The exclusive temporary D generation could not be created.",
                     "publication.temporary_generation")
  }
  triomics_d_publication_failpoint("temporary_generation_created")

  payload <- result
  payload$publication_context <- NULL
  payload$artifact_id <- "d-evidence-routing"
  payload$config_sha256 <- as.character(config_sha256)
  payload$privacy_class <- as.character(privacy_class)
  triomics_write_json_atomic(payload, payload_path)
  triomics_d_publication_failpoint("payload_written")
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
    } else character(),
    privacy_class = as.character(privacy_class)
  )
  envelope$analysis_id <- as.character(result$analysis_id)
  envelope$preparation_context_id <- as.character(result$preparation_context_id)
  envelope$representation_id <- as.character(result$representation_id)
  triomics_write_envelope(envelope, envelope_path)
  if (!isTRUE(triomics_validate_envelope(envelope_path))) {
    triomics_d_abort("D_PUBLICATION_FAILED", "The D evidence envelope failed its exact temporary readback.",
                     "publication.envelope")
  }
  triomics_d_publication_failpoint("envelope_written")

  member <- function(path, artifact_type, scientific_role,
                     envelope_for_artifact_id = NULL) {
    value <- list(
      path = basename(path),
      artifact_type = artifact_type,
      scientific_role = scientific_role,
      media_type = "application/json",
      status = "complete",
      size_bytes = as.numeric(file.info(path)$size),
      digest_algorithm = "sha256",
      digest = triomics_sha256(path)
    )
    if (!is.null(envelope_for_artifact_id)) {
      value$envelope_for_artifact_id <- envelope_for_artifact_id
    }
    value
  }
  members <- list(
    member(payload_path, "annotation_routing_result", "annotation_routing_result"),
    member(envelope_path, "artifact_envelope",
           "evidence_envelope", "d-evidence-routing")
  )
  members <- members[order(vapply(members, function(item) item$path, character(1)),
                           method = "radix")]
  provenance <- result$immutable_inputs
  profile <- list(
    profile = "triomics.output-generation-manifest",
    profile_version = "1.0",
    study_id = as.character(result$study_id),
    run_id = as.character(run_id),
    status = "complete",
    producer = list(
      package = "triomics",
      version = as.character(utils::packageVersion("triomics")),
      commit = as.character(producer_commit)
    ),
    provenance = triomics_d_generation_provenance(provenance),
    limitations = list(
      "This publication is not a C-06, M7, or G7 acceptance receipt.",
      "C-07 controls remain not_run.",
      "C-09 remains unqualified and conditional.",
      "Disabled annotation routes do not make biological absence claims."
    ),
    members = members
  )
  manifest_path <- file.path(temporary_generation, "generation-manifest.json")
  triomics_d_write_raw_file(manifest_path, triomics_b07_prov_canonical_value(profile))
  generation_id <- triomics_sha256(manifest_path)
  triomics_d_publication_failpoint("manifest_sealed")

  final_paths <- triomics_d_generation_paths(root, generation_id)
  triomics_d_secure_path(
    final_paths$generation, "D_OUTPUT_PATH_UNSAFE",
    "The immutable D generation path is unsafe.", "publication.generation",
    directory = TRUE
  )
  if (dir.exists(final_paths$generation)) {
    existing <- triomics_d_validate_generation(root, generation_id)
    existing_inventory <- triomics_d_generation_inventory(final_paths$generation)
    proposed_inventory <- triomics_d_generation_inventory(temporary_generation)
    same_inventory <- identical(existing_inventory$files, proposed_inventory$files) &&
      identical(existing_inventory$directories, proposed_inventory$directories)
    same_bytes <- same_inventory && all(vapply(existing_inventory$files, function(relative) {
      old_bytes <- triomics_d_read_raw_bytes(file.path(final_paths$generation, relative))
      new_bytes <- triomics_d_read_raw_bytes(file.path(temporary_generation, relative))
      !is.null(old_bytes) && !is.null(new_bytes) && identical(old_bytes, new_bytes)
    }, logical(1)))
    if (!isTRUE(same_bytes)) {
      triomics_d_abort("D_PUBLICATION_FAILED", "An existing generation with this identity has different bytes.",
                       "publication.generation")
    }
    unlink(temporary_generation, recursive = TRUE, force = TRUE)
  } else {
    if (file.exists(final_paths$generation) ||
        !file.rename(temporary_generation, final_paths$generation)) {
      triomics_d_abort("D_PUBLICATION_FAILED", "The immutable D generation could not be finalized atomically.",
                       "publication.generation")
    }
    renamed_generation <- TRUE
  }
  triomics_d_validate_generation(root, generation_id)
  triomics_d_publication_failpoint("generation_renamed")

  final_manifest <- final_paths$manifest
  pointer <- list(
    schema_version = "1.0",
    study_id = as.character(result$study_id),
    run_id = as.character(run_id),
    generation_id = generation_id,
    generation_manifest = list(
      path = file.path("D", "generations", generation_id, "generation-manifest.json"),
      size_bytes = as.numeric(file.info(final_manifest)$size),
      digest_algorithm = "sha256",
      digest = generation_id
    )
  )
  pointer_json <- triomics_b07_prov_canonical_value(pointer)
  triomics_d_secure_path(
    temporary_pointer, "D_OUTPUT_PATH_UNSAFE",
    "The temporary D pointer path is unsafe.", "publication.temporary_pointer"
  )
  triomics_d_write_raw_file(temporary_pointer, pointer_json)
  triomics_d_validate_generation(root, generation_id)
  triomics_d_publication_failpoint("pointer_prepared")
  triomics_d_publication_failpoint("before_pointer_rename")
  if (!file.rename(temporary_pointer, pointer_path)) {
    current <- tryCatch(triomics_d_validate_current_pointer(root, pointer_path),
                        error = function(error) NULL)
    if (!is.null(current) && identical(as.character(current$pointer$generation_id), generation_id)) {
      return(list(
        status = "complete", recovered = TRUE, generation_id = generation_id,
        generation_path = final_paths$generation, manifest_path = final_manifest,
        pointer_path = pointer_path, payload_path = final_paths$payload,
        envelope_path = final_paths$envelope
      ))
    }
    triomics_d_abort(
      if (identical(is.null(current), is.null(old_current)) &&
          (is.null(old_current) || identical(current$pointer_sha256, old_pointer_sha256))) {
        "D_PUBLICATION_FAILED"
      } else {
        "D_PUBLICATION_STATUS_UNKNOWN"
      },
      "The D pointer replacement could not be confirmed from a complete old or new publication state.",
      "publication.pointer"
    )
  }
  temporary_pointer <- NULL
  after_rename_error <- tryCatch({
    triomics_d_publication_failpoint("after_pointer_rename")
    NULL
  }, error = function(error) error)
  current <- tryCatch(triomics_d_validate_current_pointer(root, pointer_path),
                      error = function(error) NULL)
  if (!is.null(after_rename_error)) {
    if (!is.null(current) && identical(as.character(current$pointer$generation_id), generation_id)) {
      return(list(
        status = "complete", recovered = TRUE, generation_id = generation_id,
        generation_path = final_paths$generation, manifest_path = final_manifest,
        pointer_path = pointer_path, payload_path = final_paths$payload,
        envelope_path = final_paths$envelope
      ))
    }
    if (is.null(current) && is.null(old_current)) {
      triomics_d_abort("D_PUBLICATION_STATUS_UNKNOWN",
                       "Pointer acknowledgement failed and neither complete commit state could be established.",
                       "publication.pointer")
    }
    if (!is.null(current) && !is.null(old_current) &&
        identical(current$pointer_sha256, old_pointer_sha256)) {
      triomics_d_abort("D_PUBLICATION_FAILED",
                       "The old complete D pointer remains current; the new generation was not committed.",
                       "publication.pointer")
    }
    triomics_d_abort("D_PUBLICATION_STATUS_UNKNOWN",
                     "Pointer acknowledgement failed and the current D publication state is ambiguous.",
                     "publication.pointer")
  }
  if (is.null(current) || !identical(as.character(current$pointer$generation_id), generation_id)) {
    triomics_d_abort("D_PUBLICATION_STATUS_UNKNOWN",
                     "The new D pointer could not be read back as a complete committed generation.",
                     "publication.pointer")
  }
  list(
    status = "complete", recovered = FALSE, generation_id = generation_id,
    generation_path = final_paths$generation, manifest_path = final_manifest,
    pointer_path = pointer_path, payload_path = final_paths$payload,
    envelope_path = final_paths$envelope,
    payload_sha256 = triomics_sha256(final_paths$payload),
    envelope_sha256 = triomics_sha256(final_paths$envelope)
  )
}

triomics_d_write_evidence_result <- function(result, output_root, config_sha256,
                                            producer_commit, privacy_class,
                                            run_id = "d-t01") {
  triomics_d_publish_evidence_generation(
    result, output_root, config_sha256, producer_commit, privacy_class, run_id
  )
}

triomics_d_validate_route_evidence_proof <- function(evidence_bundle) {
  context <- if (is.list(evidence_bundle)) evidence_bundle$publication_context else NULL
  request <- if (is.list(context)) context$verified_request else NULL
  if (!is.list(context) || !triomics_d_nonempty_string(context$root) ||
      !triomics_sha256_text(as.character(context$current_config_sha256)) ||
      !is.list(request) || !is.list(request$source_manifests) ||
      !is.data.frame(request$feature_registry) || !is.list(request$imported_statistics) ||
      !all(c("source_manifests", "feature_registry", "current_config_sha256",
             "current_metadata", "current_count_metadata", "imported_statistics",
             "routing_metadata") %in% names(request)) ||
      (!is.null(request$current_metadata) && !is.list(request$current_metadata))) {
    triomics_d_abort("D_PROVENANCE_REQUIRED",
                     "The exported annotation API requires a native same-root B/C readback context.",
                     "publication_context")
  }
  verified <- triomics_d_read_evidence_bundle(
    context$root, as.character(evidence_bundle$study_id), as.character(evidence_bundle$analysis_id),
    request$source_manifests, request$feature_registry,
    imported_statistics = request$imported_statistics,
    routing_metadata = request$routing_metadata,
    current_config_sha256 = as.character(context$current_config_sha256),
    current_metadata = request$current_metadata,
    current_count_metadata = request$current_count_metadata
  )
  if (!identical(evidence_bundle, verified)) {
    triomics_d_abort("D_PROVENANCE_INVALID",
                     "The in-memory annotation input differs from its re-read same-root producer evidence.",
                     "evidence_bundle")
  }
  invisible(TRUE)
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
    triomics_d_abort("D_PROVENANCE_REQUIRED", "The annotation entry point accepts only B/C evidence loaded from hash-verified immutable handoffs.",
                     "evidence_bundle.immutable_inputs")
  }
  triomics_d_validate_route_evidence_proof(evidence_bundle)
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
