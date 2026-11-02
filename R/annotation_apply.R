triomics_d_sequence <- function(value) {
  is.list(value) && !is.data.frame(value) && (length(value) == 0L || is.null(names(value)))
}

triomics_d_one_string <- function(value, code, message, key) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    triomics_d_abort(code, message, key)
  }
  value
}

triomics_d_read_feature_metadata <- function(path, key) {
  table <- tryCatch(
    utils::read.delim(path, colClasses = "character", check.names = FALSE,
                      quote = "", comment.char = "", na.strings = character()),
    error = function(error) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                       "Feature metadata must be a readable tab-separated table.", key)
    }
  )
  required <- c("feature_id", "taxon_id", "resolution", "capabilities", "provenance_ref")
  if (anyDuplicated(names(table)) || !all(required %in% names(table)) ||
      any(!vapply(table[required], function(column) all(!is.na(column) & nzchar(column)), logical(1)))) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                     "Feature metadata needs feature_id, taxon_id, resolution, capabilities, and provenance_ref.",
                     key)
  }
  if (anyDuplicated(table$feature_id)) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                     "Feature metadata identifiers must be unique within a block.", key)
  }
  table
}

triomics_d_routing_metadata <- function(config, config_path, feature_registry) {
  if (!is.data.frame(feature_registry) || !all(c("block_id", "feature_key", "original_id") %in% names(feature_registry))) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                     "Stage D routing needs the ingested feature registry.", "feature_registry")
  }
  requests <- triomics_d_request_list(config$annotation %||% list())
  requested <- vapply(requests, function(request) {
    triomics_d_one_string(request$route, "D_ROUTE_INVALID", "Each annotation request needs one route.", "annotation.requests")
  }, character(1))
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  blocks <- config$blocks %||% list()
  if (!triomics_d_sequence(blocks)) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "Blocks must be a list.", "blocks")
  }
  metadata <- list()
  for (index in seq_along(blocks)) {
    block <- blocks[[index]]
    route <- as.character(block$annotation_route %||% "")
    key <- sprintf("blocks[%d].feature_metadata", index)
    meta_path <- block$feature_metadata
    if (length(route) == 1L && route %in% c("gene_reaction", "lipid_ontology") && route %in% requested &&
        (is.null(meta_path) || !triomics_d_nonempty_string(meta_path))) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                       "A requested molecular block needs a feature metadata table.", key)
    }
    if (is.null(meta_path) || !triomics_d_nonempty_string(meta_path)) next
    resolved <- triomics_resolve_declared_input(meta_path, key, root)
    table <- triomics_d_read_feature_metadata(resolved, key)
    rows <- feature_registry[feature_registry$block_id == as.character(block$block_id), , drop = FALSE]
    for (row_index in seq_len(nrow(table))) {
      feature_id <- table$feature_id[[row_index]]
      matched <- which(as.character(rows$original_id) == feature_id)
      if (length(matched) != 1L) {
        triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                         "A feature metadata identifier must match one feature in its block.", key)
      }
      feature_key <- as.character(rows$feature_key[[matched]])
      if (!is.null(metadata[[feature_key]])) {
        triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                         "Feature metadata resolved the same feature key twice.", feature_key)
      }
      capabilities <- trimws(strsplit(table$capabilities[[row_index]], "|", fixed = TRUE)[[1L]])
      capabilities <- capabilities[nzchar(capabilities)]
      if (!length(capabilities) || anyDuplicated(capabilities)) {
        triomics_d_abort("D_FEATURE_REGISTRY_INVALID",
                         "Declared capabilities must be unique pipe-separated values.", key)
      }
      provenance_ref <- table$provenance_ref[[row_index]]
      metadata[[feature_key]] <- list(
        taxon = list(status = "declared", value = table$taxon_id[[row_index]], provenance_ref = provenance_ref),
        capabilities = list(status = "declared", value = capabilities, provenance_ref = provenance_ref),
        resolution = list(status = "declared", value = table$resolution[[row_index]], provenance_ref = provenance_ref)
      )
    }
  }
  if (!length(metadata)) stats::setNames(list(), character()) else metadata
}

triomics_d_request_list <- function(annotation) {
  requests <- if (is.list(annotation)) annotation$requests %||% list() else list()
  if (!triomics_d_sequence(requests)) {
    triomics_d_abort("D_ROUTE_INVALID", "Annotation requests must be a list.", "annotation.requests")
  }
  requests
}

triomics_d_index_resources <- function(annotation) {
  resources <- if (is.list(annotation)) annotation$resources %||% list() else list()
  if (!triomics_d_sequence(resources)) {
    triomics_d_abort("D_RESOURCE_REQUEST_INVALID", "Annotation resources must be a list.", "annotation.resources")
  }
  index <- list()
  for (resource in resources) {
    resource_id <- triomics_d_one_string(resource$resource_id, "D_RESOURCE_REQUEST_INVALID",
                                         "Each resource needs one identifier.", "annotation.resources")
    if (!is.null(index[[resource_id]])) {
      triomics_d_abort("D_RESOURCE_REQUEST_INVALID", "Resource identifiers must be unique.", resource_id)
    }
    index[[resource_id]] <- resource
  }
  index
}

triomics_d_mapping_contract <- function(resource, route, cache_dir) {
  access <- triomics_d_resource_access(
    resource, route, as.character(resource$version), as.character(resource$sha256),
    cache_dir, network_policy = "offline"
  )
  if (!identical(access$status, "available")) return(NULL)
  code <- if (identical(route, "lipid_ontology")) "D_LIPID_TABLE_INVALID" else "D_REACTION_TABLE_INVALID"
  table <- tryCatch(
    utils::read.delim(access$path, colClasses = "character", check.names = FALSE,
                      quote = "", comment.char = "", na.strings = character()),
    error = function(error) triomics_d_abort(code, "The local mapping table cannot be parsed.", resource$resource_id)
  )
  values <- function(column) {
    if (!column %in% names(table)) return(character())
    kept <- unique(table[[column]])
    kept[!is.na(kept) & nzchar(kept)]
  }
  contract <- list(namespaces = values("source_namespace"), organisms = values("taxon_id"),
                   resolutions = values("resolution"))
  if (!length(contract$namespaces) || !length(contract$organisms) || !length(contract$resolutions)) {
    triomics_d_abort(code, "The local mapping table lacks namespace, taxon, or resolution.",
                     as.character(resource$resource_id))
  }
  contract
}

triomics_d_measured_rows <- function(route_name, nominations, registry) {
  if (!is.list(nominations)) nominations <- list()
  rows <- list()
  for (nomination in nominations) {
    feature_key <- triomics_d_one_string(nomination$feature_key, "D_FEATURE_REGISTRY_INVALID",
                                         "A routed nomination lacks its feature key.", route_name)
    feature <- registry[[feature_key]]
    if (!is.list(feature)) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A routed feature is absent from the registry.", feature_key)
    }
    if (is.null(rows[[feature_key]])) {
      rows[[feature_key]] <- list(
        feature_key = feature_key,
        # Mapper source id is the measured original identifier, not the source-manifest id.
        source_id = triomics_d_one_string(feature$original_id, "D_FEATURE_REGISTRY_INVALID",
                                          "A routed feature lacks its measured identifier.", feature_key),
        source_namespace = triomics_d_one_string(feature$id_namespace, "D_FEATURE_REGISTRY_INVALID",
                                                 "A routed feature lacks its identifier namespace.", feature_key),
        taxon_id = triomics_d_one_string(feature$taxon_id, "D_FEATURE_REGISTRY_INVALID",
                                         "A routed feature lacks its declared taxon.", feature_key),
        resolution = triomics_d_one_string(feature$structural_resolution, "D_FEATURE_REGISTRY_INVALID",
                                           "A routed feature lacks its declared resolution.", feature_key)
      )
      if (identical(route_name, "gene_reaction")) rows[[feature_key]]$nominations <- list()
    }
    if (identical(route_name, "gene_reaction")) {
      slot <- rows[[feature_key]]$nominations
      slot[[length(slot) + 1L]] <- list(
        nomination_id = triomics_d_one_string(nomination$nomination_id, "D_REACTION_INPUT_INVALID",
                                              "A routed nomination lacks its identifier.", feature_key),
        source_branch = triomics_d_one_string(nomination$source_branch, "D_REACTION_INPUT_INVALID",
                                              "A routed nomination lacks its source branch.", feature_key)
      )
      rows[[feature_key]]$nominations <- slot
    }
  }
  unname(rows)
}

triomics_d_target_slot <- function(investigation, config_path) {
  investigation <- investigation %||% list()
  if (!isTRUE(investigation$enabled)) {
    return(list(status = "not_requested", reason_code = NULL, results = list()))
  }
  if (triomics_d_nonempty_string(investigation$quantitative_method_ref)) {
    triomics_d_abort("D_ROUTE_CAPABILITY_NOT_IMPLEMENTED",
                     "The quantitative target route is not approved for this command.",
                     "target_investigation.quantitative_method_ref")
  }
  if (identical(as.character(investigation$mode %||% ""), "discover")) {
    triomics_d_abort("D_TARGET_UNIVERSE_REQUIRED",
                     "This command does not build a discovery universe.", "target_investigation.mode")
  }
  if (!triomics_d_nonempty_string(investigation$context_ref) ||
      !triomics_d_nonempty_string(investigation$protocol_ref)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID",
                     "An enabled target investigation needs a context and a protocol reference.",
                     "target_investigation")
  }
  evidence_file <- investigation$external_evidence_file
  if (!triomics_d_nonempty_string(evidence_file)) {
    return(list(status = "unavailable", reason_code = "D_TARGET_EVIDENCE_ABSENT", results = list()))
  }
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  resolved <- if (grepl("^(?:[A-Za-z]:[/\\\\]|/)", evidence_file)) evidence_file else file.path(root, evidence_file)
  if (!file.exists(resolved) || dir.exists(resolved)) {
    return(list(status = "unavailable", reason_code = "D_TARGET_EVIDENCE_ABSENT", results = list()))
  }
  records <- tryCatch(jsonlite::read_json(resolved, simplifyVector = FALSE),
                      error = function(error) {
                        triomics_d_abort("D_TARGET_INPUT_INVALID",
                                         "The external target evidence file is not valid JSON.",
                                         "target_investigation.external_evidence_file")
                      })
  if (is.list(records) && !is.null(records$records) && is.list(records$records)) records <- records$records
  targets <- investigation$targets %||% list()
  if (!triomics_d_sequence(targets) || !length(targets)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID",
                     "An enabled target investigation needs at least one declared target.",
                     "target_investigation.targets")
  }
  mode <- triomics_d_one_string(investigation$mode, "D_TARGET_INPUT_INVALID",
                                "Target mode must be one value.", "target_investigation.mode")
  results <- lapply(targets, function(target) {
    request <- triomics_target_request(
      identity = list(
        namespace = triomics_d_one_string(target$namespace, "D_TARGET_IDENTITY_REQUIRED",
                                          "A target needs a namespace.", "target_investigation.targets"),
        id = triomics_d_one_string(target$id, "D_TARGET_IDENTITY_REQUIRED",
                                   "A target needs an identifier.", "target_investigation.targets"),
        taxon_id = as.numeric(target$taxon_id),
        entity_type = triomics_d_one_string(target$entity_type, "D_TARGET_IDENTITY_REQUIRED",
                                            "A target needs an entity type.", "target_investigation.targets"),
        constructs = list()
      ),
      mode = mode,
      selection = list(list(
        selection_id = paste0("declared:", target$namespace, ":", target$id),
        hypothesis_status = "exploratory",
        selected_with_observations = FALSE
      )),
      context = list(context_id = as.character(investigation$context_ref),
                     confirmation_protocol = as.character(investigation$protocol_ref)),
      drug = NULL
    )
    triomics_target_external_evidence(request, records)
  })
  list(status = "recorded", reason_code = NULL, drug_ref = investigation$drug_ref %||% NULL,
       mechanism_status = if (triomics_d_nonempty_string(investigation$mechanism_file)) "not_applied" else "not_supplied",
       results = results)
}

triomics_d_annotate_local_resources <- function(evidence, annotation, cache_dir,
                                                     target_investigation, config_path) {
  annotation <- annotation %||% list()
  requests <- triomics_d_request_list(annotation)
  resources <- triomics_d_index_resources(annotation)
  by_route <- list()
  for (request in requests) {
    route <- triomics_d_one_string(request$route, "D_ROUTE_INVALID",
                                   "Each annotation request needs one route.", "annotation.requests")
    if (!route %in% triomics_d_route_names()) {
      triomics_d_abort("D_ROUTE_INVALID", "D received an unregistered annotation route.", route)
    }
    if (!is.null(by_route[[route]])) {
      triomics_d_abort("D_ROUTE_INVALID", "Each annotation route may be requested once.", route)
    }
    if (!is.logical(request$required) || length(request$required) != 1L || is.na(request$required)) {
      triomics_d_abort("D_ROUTE_INVALID", "Each annotation request needs one required flag.", route)
    }
    ids <- as.character(unlist(request$resource_ids %||% list(), use.names = FALSE))
    if (any(!nzchar(ids)) || anyDuplicated(ids) || any(!ids %in% names(resources))) {
      triomics_d_abort("D_RESOURCE_REQUEST_INVALID",
                       "An annotation request names a missing or repeated resource.", route)
    }
    if (route %in% c("gene_reaction", "lipid_ontology") && !length(ids)) {
      triomics_d_abort("D_RESOURCE_REQUEST_INVALID",
                       "A molecular annotation request needs at least one local resource.", route)
    }
    by_route[[route]] <- list(request = request, resource_ids = ids)
  }
  contracts <- list()
  for (route in intersect(names(by_route), c("gene_reaction", "lipid_ontology"))) {
    for (resource_id in by_route[[route]]$resource_ids) {
      contracts[[resource_id]] <- triomics_d_mapping_contract(resources[[resource_id]], route, cache_dir)
    }
  }
  specs <- stats::setNames(lapply(triomics_d_route_names(), function(route) {
    request <- by_route[[route]]
    if (is.null(request)) return(list(enabled = FALSE))
    if (!route %in% c("gene_reaction", "lipid_ontology")) return(list(enabled = TRUE))
    parts <- contracts[request$resource_ids]
    parts <- parts[vapply(parts, is.list, logical(1))]
    if (!length(parts)) return(list(enabled = FALSE))
    list(
      enabled = TRUE,
      namespaces = unique(unlist(lapply(parts, `[[`, "namespaces"), use.names = FALSE)),
      organisms = unique(unlist(lapply(parts, `[[`, "organisms"), use.names = FALSE)),
      resolutions = unique(unlist(lapply(parts, `[[`, "resolutions"), use.names = FALSE)),
      capabilities = route
    )
  }), triomics_d_route_names())
  routed <- triomics_annotate_evidence(evidence, list(routes = specs))
  registry <- routed$inherited_evidence$feature_registry
  for (route in intersect(names(by_route), c("gene_reaction", "lipid_ontology"))) {
    measured <- triomics_d_measured_rows(route, routed$routes[[route]]$nominations %||% list(), registry)
    mappings <- list()
    for (resource_id in by_route[[route]]$resource_ids) {
      resource <- resources[[resource_id]]
      mappings[[resource_id]] <- if (identical(route, "lipid_ontology")) {
        triomics_map_lipids(measured, resource, route, as.character(resource$version),
                            as.character(resource$sha256), cache_dir)
      } else {
        triomics_map_reactions(measured, resource, route, as.character(resource$version),
                               as.character(resource$sha256), cache_dir)
      }
    }
    routed$routes[[route]]$mappings <- mappings
  }
  provenance <- evidence$immutable_inputs
  predecessor <- list(
    b_sha256 = as.character(provenance$b_bundle_sha256),
    c_sha256 = as.character(provenance$c_manifest_sha256)
  )
  status_routes <- stats::setNames(lapply(triomics_d_route_names(), function(route) {
    list(enabled = FALSE, required = FALSE, status = "not_run")
  }), triomics_d_route_names())
  for (route in names(by_route)) {
    status_routes[[route]]$enabled <- TRUE
    status_routes[[route]]$required <- isTRUE(by_route[[route]]$request$required)
    if (!route %in% c("gene_reaction", "lipid_ontology")) {
      # These routes stay unavailable: no measured enrichment universe is declared,
      # and the conditional MOFA and BioPAN adapters are not approved.
      status_routes[[route]]$status <- "unavailable"
      status_routes[[route]]$reason_code <- "D_ROUTE_CAPABILITY_NOT_IMPLEMENTED"
      next
    }
    mappings <- routed$routes[[route]]$mappings %||% list()
    unavailable <- mappings[vapply(mappings, function(mapping) !identical(mapping$status, "available"), logical(1))]
    if (!length(mappings) || length(unavailable)) {
      reason_code <- if (length(unavailable)) unavailable[[1L]]$reason_code %||% "D_RESOURCE_MISSING" else "D_RESOURCE_MISSING"
      status_routes[[route]]$status <- "unavailable"
      status_routes[[route]]$reason_code <- reason_code
      routed$routes[[route]]$status <- "unavailable"
      routed$routes[[route]]$reason_code <- reason_code
    } else {
      status_routes[[route]]$status <- "complete"
    }
  }
  route_status <- triomics_d_route_status(status_routes, list(), predecessor, list())
  target <- triomics_d_target_slot(target_investigation %||% list(), config_path)
  if (isTRUE((target_investigation %||% list())$required) && identical(target$status, "unavailable")) {
    route_status$exit_code <- 2L
  }
  routed$target_investigation <- target
  routed$route_status <- route_status
  routed
}
