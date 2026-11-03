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
  access <- triomics_d_annotation_resource_access(
    resource, route, as.character(resource$version), as.character(resource$sha256),
    cache_dir
  )
  if (!identical(access$status, "available")) return(access)
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
  contract <- list(status = "available", namespaces = values("source_namespace"), organisms = values("taxon_id"),
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

triomics_d_target_read_json_input <- function(path, config_path, key) {
  if (!triomics_d_nonempty_string(path) || grepl("^(?:https?|ftp)://", path, ignore.case = TRUE)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "A target evidence input must be a local JSON file.", key)
  }
  config_root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  candidate <- if (grepl("^(?:[A-Za-z]:[/\\\\]|/)", path)) path else file.path(config_root, path)
  if (!file.exists(candidate) || dir.exists(candidate)) {
    return(list(status = "unavailable", value = NULL, sha256 = NULL))
  }
  components <- strsplit(candidate, "[/\\\\]+", perl = TRUE)[[1L]]
  current <- if (grepl("^(?:[A-Za-z]:[/\\\\]|/)", candidate)) {
    if (startsWith(candidate, "/")) "/" else paste0(components[[1L]], "/")
  } else ""
  walk <- if (grepl("^[A-Za-z]:[/\\\\]", candidate)) components[-1L] else components
  for (component in walk[nzchar(walk)]) {
    if (identical(component, ".")) next
    if (identical(component, "..")) {
      current <- dirname(current)
      next
    }
    current <- if (identical(current, "/") || grepl("^[A-Za-z]:[/\\\\]$", current)) {
      paste0(current, component)
    } else file.path(current, component)
    if (triomics_is_symlink(current)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "A target evidence path crosses a symbolic link.", key)
    }
  }
  absolute <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
  text <- tryCatch(readChar(absolute, nchars = file.info(absolute)$size, useBytes = TRUE),
                   error = function(error) NULL)
  valid_json <- !is.null(text) && tryCatch({
    triomics_d_ora_scan_unique_json_keys(text)
    TRUE
  }, error = function(error) FALSE)
  if (!valid_json) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Target evidence JSON is malformed or repeats an object field.", key)
  }
  value <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(value) || is.data.frame(value) || is.null(names(value)) || anyDuplicated(names(value))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Target evidence JSON must contain one typed object.", key)
  }
  list(status = "available", value = value, sha256 = triomics_sha256(absolute))
}

triomics_d_target_identity_record <- function(value, key, allow_constructs = FALSE) {
  fields <- if (allow_constructs) c("namespace", "id", "taxon_id", "entity_type", "constructs") else
    c("namespace", "id", "taxon_id", "entity_type")
  if (!is.list(value) || is.data.frame(value) || is.null(names(value)) || anyDuplicated(names(value)) ||
      !setequal(names(value), fields) || !triomics_target_string(value$namespace) ||
      !triomics_target_string(value$id) || !triomics_target_taxon(value$taxon_id) ||
      !triomics_target_string(value$entity_type) ||
      !value$entity_type %in% c("protein", "complex", "pathway", "other_declared")) {
    triomics_d_abort("D_TARGET_IDENTITY_REQUIRED", "Target identity must be a typed namespace, ID, integer taxon and entity type.", key)
  }
  identity <- value[c("namespace", "id", "taxon_id", "entity_type")]
  if (allow_constructs) {
    if (!triomics_target_constructs(value$constructs)) {
      triomics_d_abort("D_TARGET_IDENTITY_REQUIRED", "Target constructs must be unique declared identifiers.", key)
    }
    identity$constructs <- value$constructs
  }
  identity
}

triomics_d_target_mechanism_document <- function(input, investigation) {
  if (!identical(input$status, "available")) return(list(status = "not_supplied", declarations = list(), drug = NULL))
  document <- input$value
  expected_fields <- c("format", "format_version", "protocol_ref", "context_ref", "drug_ref", "drug_identity", "targets")
  if (!identical(sort(names(document), method = "radix"), sort(expected_fields, method = "radix")) ||
      !identical(document$format, "triomics.target-mechanism") ||
      !identical(document$format_version, "1.0") ||
      !triomics_target_string(document$protocol_ref) ||
      !triomics_target_string(document$context_ref) ||
      !identical(document$protocol_ref, investigation$protocol_ref) ||
      !identical(document$context_ref, investigation$context_ref) ||
      !identical(document$drug_ref, investigation$drug_ref) ||
      !is.list(document$targets) || is.data.frame(document$targets) || !is.null(names(document$targets))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "The mechanism file has an invalid version or mismatched protocol, drug or context reference.",
                     "target_investigation.mechanism_file")
  }
  drug <- NULL
  if (is.null(investigation$drug_ref)) {
    if (!is.null(document$drug_identity)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "A drug identity cannot be declared without drug_ref.",
                       "target_investigation.mechanism_file.drug_identity")
    }
  } else {
    value <- document$drug_identity
    if (!is.list(value) || is.data.frame(value) || is.null(names(value)) ||
        anyDuplicated(names(value)) || !setequal(names(value), c("namespace", "id", "label")) ||
        !triomics_target_string(value$namespace) || !triomics_target_string(value$id) ||
        !triomics_target_string(value$label)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "drug_ref needs a typed drug namespace, ID and label in the mechanism file.",
                       "target_investigation.mechanism_file.drug_identity")
    }
    drug <- list(namespace = value$namespace, drug_id = value$id, label = value$label)
  }
  declarations <- lapply(document$targets, function(declaration) {
    allowed <- c("identity", "mechanism_class", "links", "selection_history")
    if (!is.list(declaration) || is.data.frame(declaration) || is.null(names(declaration)) ||
        anyDuplicated(names(declaration)) || any(!names(declaration) %in% allowed) ||
        !all(c("identity", "mechanism_class", "links") %in% names(declaration))) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Each mechanism declaration must name a typed target, class and links.",
                       "target_investigation.mechanism_file.targets")
    }
    identity <- triomics_d_target_identity_record(declaration$identity,
                                                  "target_investigation.mechanism_file.targets.identity",
                                                  allow_constructs = TRUE)
    mechanism_class <- declaration$mechanism_class
    if (!is.null(mechanism_class) && !triomics_target_string(mechanism_class)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "A mechanism class must be one string or null.",
                       "target_investigation.mechanism_file.targets.mechanism_class")
    }
    links <- declaration$links
    if (!is.list(links) || is.data.frame(links) || !is.null(names(links))) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Mechanism links must be an array.",
                       "target_investigation.mechanism_file.targets.links")
    }
    links <- lapply(links, function(link) {
      required <- c("link_id", "relation", "sign", "context_id", "source_id", "feature_namespace", "feature_id")
      allowed_link_fields <- c(required, "expected_direction")
      if (!is.list(link) || is.data.frame(link) || is.null(names(link)) || anyDuplicated(names(link)) ||
          any(!names(link) %in% allowed_link_fields) || !all(required %in% names(link)) ||
          !all(vapply(link[required], triomics_target_string, logical(1))) ||
          (!is.null(link$expected_direction) && (!triomics_target_string(link$expected_direction) ||
            !link$expected_direction %in% c("positive", "negative", "unknown", "unsigned")))) {
        triomics_d_abort("D_TARGET_INPUT_INVALID", "A mechanism link has an invalid path or measured feature identity.",
                         "target_investigation.mechanism_file.targets.links")
      }
      link
    })
    link_ids <- if (length(links)) vapply(links, `[[`, character(1), "link_id") else character()
    if (anyDuplicated(link_ids)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Mechanism link identifiers must be unique per target.",
                       "target_investigation.mechanism_file.targets.links")
    }
    history <- declaration$selection_history %||% list()
    if (!is.list(history) || is.data.frame(history) || !is.null(names(history))) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Target selection history must be an array.",
                       "target_investigation.mechanism_file.targets.selection_history")
    }
    history <- lapply(history, function(selection) {
      required_selection <- c("selection_id", "hypothesis_status", "selected_with_observations")
      allowed_selection <- c(required_selection, "selection_source", "selection_evidence_ref")
      if (!is.list(selection) || is.data.frame(selection) || is.null(names(selection)) ||
          anyDuplicated(names(selection)) || any(!names(selection) %in% allowed_selection) ||
          !all(required_selection %in% names(selection)) ||
          !triomics_target_string(selection$selection_id) ||
          !triomics_target_string(selection$hypothesis_status) ||
          !selection$hypothesis_status %in% c("exploratory", "confirmed") ||
          !is.logical(selection$selected_with_observations) ||
          length(selection$selected_with_observations) != 1L || is.na(selection$selected_with_observations) ||
          (!is.null(selection$selection_source) && !triomics_target_string(selection$selection_source)) ||
          (!is.null(selection$selection_evidence_ref) && !triomics_target_string(selection$selection_evidence_ref))) {
        triomics_d_abort("D_TARGET_INPUT_INVALID", "Target selection history has invalid typed fields.",
                         "target_investigation.mechanism_file.targets.selection_history")
      }
      selection
    })
    selection_ids <- if (length(history)) vapply(history, `[[`, character(1), "selection_id") else character()
    if (anyDuplicated(selection_ids)) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Target selection history identifiers must be unique.",
                       "target_investigation.mechanism_file.targets.selection_history")
    }
    list(identity = identity, mechanism_class = mechanism_class, links = links,
         selection_history = history)
  })
  list(status = "supplied", declarations = declarations, drug = drug)
}

triomics_d_target_selection_history <- function(target, declaration = NULL) {
  history <- if (is.list(declaration)) declaration$selection_history else list()
  if (!length(history)) {
    return(list(list(selection_id = paste0("declared:", target$namespace, ":", target$id),
                     hypothesis_status = "exploratory", selected_with_observations = TRUE,
                     selection_source = "configuration_request", independent_confirmation = FALSE)))
  }
  lapply(history, function(selection) {
    selection$recorded_hypothesis_status <- selection$hypothesis_status
    selection$hypothesis_status <- "exploratory"
    selection$independent_confirmation <- FALSE
    selection
  })
}

triomics_d_target_slot <- function(investigation, config_path, evidence = NULL) {
  investigation <- investigation %||% list()
  if (!isTRUE(investigation$enabled)) {
    return(list(status = "not_requested", reason_code = NULL, results = list()))
  }
  if (triomics_d_nonempty_string(investigation$quantitative_method_ref)) {
    triomics_d_abort("D_ROUTE_CAPABILITY_NOT_IMPLEMENTED",
                     "The quantitative target route is not approved for this command.",
                     "target_investigation.quantitative_method_ref")
  }
  if (identical(investigation$mode, "discover")) {
    triomics_d_abort("D_TARGET_UNIVERSE_REQUIRED", "This command does not build a discovery universe.",
                     "target_investigation.mode")
  }
  if (!triomics_target_string(investigation$context_ref) || !triomics_target_string(investigation$protocol_ref)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID",
                     "An enabled target investigation needs a context and a protocol reference.",
                     "target_investigation")
  }
  targets <- investigation$targets %||% list()
  if (!is.list(targets) || is.data.frame(targets) || !is.null(names(targets))) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Declared targets must be an array.", "target_investigation.targets")
  }
  if (!length(targets)) {
    return(list(status = "unavailable", reason_code = "D_TARGET_EVIDENCE_ABSENT", results = list(),
                input_provenance = list()))
  }
  identities <- lapply(targets, triomics_d_target_identity_record,
                       key = "target_investigation.targets", allow_constructs = FALSE)
  mode <- investigation$mode
  if (!triomics_target_string(mode) || !mode %in% c("assess", "compare")) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Target mode must be assess or compare for this command.",
                     "target_investigation.mode")
  }
  config_root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  mechanism_input <- if (triomics_d_nonempty_string(investigation$mechanism_file)) {
    triomics_d_target_read_json_input(investigation$mechanism_file, config_path,
                                      "target_investigation.mechanism_file")
  } else list(status = "not_supplied", value = NULL, sha256 = NULL)
  mechanism <- triomics_d_target_mechanism_document(mechanism_input, investigation)
  external_input <- if (triomics_d_nonempty_string(investigation$external_evidence_file)) {
    triomics_d_target_read_json_input(investigation$external_evidence_file, config_path,
                                      "target_investigation.external_evidence_file")
  } else list(status = "unavailable", value = NULL, sha256 = NULL)
  external_records <- list()
  if (identical(external_input$status, "available")) {
    allowed <- c("format", "format_version", "records")
    document <- external_input$value
    if (!setequal(names(document), "records") &&
        !(setequal(names(document), allowed) && identical(document$format, "triomics.external-target-evidence") &&
          identical(document$format_version, "1.0"))) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "External assay input must contain one records array and optional format version.",
                       "target_investigation.external_evidence_file")
    }
    external_records <- document$records
    if (!is.list(external_records) || is.data.frame(external_records) || !is.null(names(external_records))) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "External assay records must be an array.",
                       "target_investigation.external_evidence_file.records")
    }
  }
  input_provenance <- list()
  if (identical(mechanism_input$status, "available")) {
    input_provenance[["mechanism_file"]] <- list(status = "consumed", sha256 = mechanism_input$sha256)
  }
  if (identical(external_input$status, "available")) {
    input_provenance[["external_evidence_file"]] <- list(status = "consumed", sha256 = external_input$sha256)
  }
  results <- lapply(identities, function(identity) {
    matches <- Filter(function(declaration) {
      identical(declaration$identity$namespace, identity$namespace) &&
        identical(declaration$identity$id, identity$id) &&
        identical(as.numeric(declaration$identity$taxon_id), as.numeric(identity$taxon_id)) &&
        identical(declaration$identity$entity_type, identity$entity_type)
    }, mechanism$declarations)
    declaration <- if (length(matches) == 1L) matches[[1L]] else NULL
    request_identity <- identity
    request_identity$constructs <- if (is.null(declaration)) list() else declaration$identity$constructs
    selection <- triomics_d_target_selection_history(identity, declaration)
    request <- triomics_target_request(
      identity = request_identity, mode = mode, selection = selection,
      context = list(context_id = investigation$context_ref,
                     confirmation_protocol = investigation$protocol_ref),
      drug = mechanism$drug
    )
    mechanism_class <- if (length(matches) == 1L && triomics_target_string(declaration$mechanism_class)) {
      declaration$mechanism_class
    } else "unknown"
    request$mechanism_class <- mechanism_class
    links <- if (length(matches) == 1L) declaration$links else list()
    observations <- if (identical(mechanism_class, "enzymatic") || identical(mechanism_class, "non_enzymatic")) {
      if (is.null(evidence)) list() else triomics_target_resolve_observations(request, links, evidence)
    } else list()
    external <- triomics_target_external_evidence(request, external_records)
    list(request = request, mechanism_status = if (length(matches) == 0L) "not_declared" else
      if (length(matches) > 1L) "ambiguous" else if (mechanism_class %in% c("enzymatic", "non_enzymatic")) "assessable" else "unassessable",
      mechanism_declarations = matches, links = links, observations = observations,
      external_evidence = external, input_provenance = input_provenance)
  })
  status <- if (identical(external_input$status, "available")) "recorded" else "unavailable"
  list(status = status,
       reason_code = if (identical(status, "unavailable")) "D_TARGET_EVIDENCE_ABSENT" else NULL,
       drug_ref = investigation$drug_ref %||% NULL,
       mechanism_status = if (identical(mechanism$status, "supplied")) "supplied" else "not_supplied",
       input_provenance = input_provenance, results = results)
}

triomics_d_resolve_resource_path <- function(path, config_root) {
  if (!triomics_d_nonempty_string(path) ||
      grepl("^(?:https?|ftp)://", path, ignore.case = TRUE) ||
      grepl("^(?:[A-Za-z]:[/\\\\]|/)", path)) {
    return(path)
  }
  file.path(config_root, path)
}

triomics_d_ora_scan_unique_json_keys <- function(text) {
  characters <- strsplit(enc2utf8(text), "", fixed = TRUE)[[1L]]
  position <- 1L
  size <- length(characters)
  skip_whitespace <- function() {
    while (position <= size && characters[[position]] %in% c(" ", "\t", "\r", "\n")) {
      position <<- position + 1L
    }
  }
  read_string <- function() {
    if (position > size || !identical(characters[[position]], '"')) stop("Expected a JSON string.", call. = FALSE)
    start <- position
    position <<- position + 1L
    escaped <- FALSE
    while (position <= size) {
      character <- characters[[position]]
      if (escaped) {
        escaped <- FALSE
        position <<- position + 1L
        next
      }
      if (identical(character, "\\")) {
        escaped <- TRUE
        position <<- position + 1L
        next
      }
      if (identical(character, '"')) {
        token <- paste0(characters[start:position], collapse = "")
        position <<- position + 1L
        decoded <- jsonlite::fromJSON(token, simplifyVector = FALSE)
        if (!is.character(decoded) || length(decoded) != 1L) stop("Invalid JSON string.", call. = FALSE)
        return(decoded)
      }
      position <<- position + 1L
    }
    stop("Unterminated JSON string.", call. = FALSE)
  }
  parse_value <- NULL
  parse_array <- function() {
    position <<- position + 1L
    skip_whitespace()
    if (position <= size && identical(characters[[position]], "]")) {
      position <<- position + 1L
      return(invisible(NULL))
    }
    repeat {
      parse_value()
      skip_whitespace()
      if (position > size) stop("Unterminated JSON array.", call. = FALSE)
      if (identical(characters[[position]], "]")) {
        position <<- position + 1L
        break
      }
      if (!identical(characters[[position]], ",")) stop("Invalid JSON array separator.", call. = FALSE)
      position <<- position + 1L
    }
    invisible(NULL)
  }
  parse_object <- function() {
    position <<- position + 1L
    skip_whitespace()
    if (position <= size && identical(characters[[position]], "}")) {
      position <<- position + 1L
      return(invisible(NULL))
    }
    keys <- character()
    repeat {
      skip_whitespace()
      key <- read_string()
      if (key %in% keys) stop("A JSON object repeats a key.", call. = FALSE)
      keys <- c(keys, key)
      skip_whitespace()
      if (position > size || !identical(characters[[position]], ":")) {
        stop("Invalid JSON object member.", call. = FALSE)
      }
      position <<- position + 1L
      parse_value()
      skip_whitespace()
      if (position > size) stop("Unterminated JSON object.", call. = FALSE)
      if (identical(characters[[position]], "}")) {
        position <<- position + 1L
        break
      }
      if (!identical(characters[[position]], ",")) stop("Invalid JSON object separator.", call. = FALSE)
      position <<- position + 1L
    }
    invisible(NULL)
  }
  parse_value <- function() {
    skip_whitespace()
    if (position > size) stop("Missing JSON value.", call. = FALSE)
    current <- characters[[position]]
    if (identical(current, "{")) return(parse_object())
    if (identical(current, "[")) return(parse_array())
    if (identical(current, '"')) {
      read_string()
      return(invisible(NULL))
    }
    start <- position
    while (position <= size && !(characters[[position]] %in% c(",", "]", "}", " ", "\t", "\r", "\n"))) {
      position <<- position + 1L
    }
    if (start == position) stop("Invalid JSON value.", call. = FALSE)
    invisible(NULL)
  }
  parse_value()
  skip_whitespace()
  if (position <= size) stop("Unexpected trailing JSON content.", call. = FALSE)
  invisible(TRUE)
}

triomics_d_ora_parse_protocol_text <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    triomics_d_abort("D_ORA_PROTOCOL_INVALID", "The pinned ORA protocol must be one UTF-8 JSON document.",
                     "annotation.requests.ora.protocol_ref")
  }
  tryCatch(
    triomics_d_ora_scan_unique_json_keys(text),
    error = function(error) {
      triomics_d_abort("D_ORA_PROTOCOL_INVALID",
                       "The pinned ORA protocol contains invalid JSON or a repeated object key.",
                       "annotation.requests.ora.protocol_ref")
    }
  )
  protocol <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(protocol)) {
    triomics_d_abort("D_ORA_PROTOCOL_INVALID", "The pinned ORA protocol must have a JSON object at its root.",
                     "annotation.requests.ora.protocol_ref")
  }
  protocol
}

triomics_d_ora_json_array <- function(value) {
  is.list(value) && !is.data.frame(value) && is.null(names(value))
}

triomics_d_ora_json_object <- function(value, fields = NULL) {
  is.list(value) && !is.data.frame(value) && !is.null(names(value)) &&
    !anyDuplicated(names(value)) &&
    (is.null(fields) || identical(sort(names(value), method = "radix"), sort(fields, method = "radix")))
}

triomics_d_ora_json_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
}

triomics_d_ora_validate_protocol <- function(protocol, protocol_ref, analysis_id, mapping_resource_ids) {
  abort <- function(message, key = "annotation.requests.ora.protocol_ref") {
    triomics_d_abort("D_ORA_PROTOCOL_INVALID", message, key)
  }
  top_fields <- c("schema_version", "artifact_type", "protocol_id", "mapping_rule",
                  "aggregation_rule", "ambiguity_rule", "claims")
  if (!triomics_d_ora_json_object(protocol, top_fields) ||
      !identical(protocol$schema_version, "1.0") ||
      !identical(protocol$artifact_type, "declared_universe_ora_protocol") ||
      !identical(protocol$protocol_id, protocol_ref) ||
      !identical(protocol$mapping_rule, "unique_resolved_entity") ||
      !identical(protocol$aggregation_rule, "unique_target_entity_across_measured_features") ||
      !identical(protocol$ambiguity_rule, "exclude_unmapped_and_multi_entity_rows") ||
      !triomics_d_ora_json_array(protocol$claims) || !length(protocol$claims)) {
    abort("The pinned ORA protocol does not match the supported typed protocol contract.")
  }
  claims <- lapply(protocol$claims, function(claim) {
    if (!is.list(claim) || is.data.frame(claim) || is.null(names(claim)) || anyDuplicated(names(claim))) {
      abort("Every ORA claim must be a JSON object with unique keys.")
    }
    branch <- claim$source_branch
    required <- c("analysis_id", "block_id", "source_branch", "claim_id", "mapping_resource_id",
                  "mapping_operation", "source_namespace", "taxon_id", "source_resolution",
                  "target_namespace", "set_family")
    if (identical(branch, "integration_selection")) required <- c(required, "integration_id")
    if (!triomics_d_ora_json_object(claim, required) ||
        !all(vapply(c("analysis_id", "block_id", "source_branch", "claim_id", "mapping_resource_id",
                      "mapping_operation", "source_namespace", "taxon_id", "source_resolution",
                      "target_namespace"), function(field) triomics_d_ora_json_string(claim[[field]]),
                    logical(1))) ||
        !identical(claim$analysis_id, analysis_id) ||
        !branch %in% c("primary_statistics", "integration_selection") ||
        !claim$mapping_operation %in% c("gene_reaction", "lipid_ontology") ||
        !claim$mapping_resource_id %in% mapping_resource_ids) {
      abort("An ORA claim has a missing, unsupported, or mismatched analysis, branch, or resource identity.")
    }
    if (identical(branch, "integration_selection")) {
      if (!triomics_d_ora_json_string(claim$integration_id) ||
          !identical(claim$integration_id, claim$claim_id)) {
        abort("An integration-selection claim must name its exact integration as claim_id and integration_id.")
      }
    }
    sets <- claim$set_family
    if (!triomics_d_ora_json_array(sets) || !length(sets)) {
      abort("Every ORA claim must predeclare a non-empty complete set-family array.")
    }
    normalized_sets <- lapply(sets, function(set) {
      set_fields <- c("set_id", "members", "eligible")
      if (!triomics_d_ora_json_object(set, set_fields) ||
          !triomics_d_ora_json_string(set$set_id) || !triomics_d_ora_json_array(set$members) ||
          !all(vapply(set$members, triomics_d_ora_json_string, logical(1)))) {
        abort("Each predeclared ORA set needs a unique ID, a flat string member array, and explicit eligibility.")
      }
      members <- vapply(set$members, identity, character(1))
      if (anyDuplicated(members) ||
          !is.logical(set$eligible) || length(set$eligible) != 1L || is.na(set$eligible)) {
        abort("Each predeclared ORA set needs a unique ID, exact members, and explicit eligibility.")
      }
      list(set_id = as.character(set$set_id), members = members, eligible = set$eligible)
    })
    set_ids <- vapply(normalized_sets, `[[`, character(1), "set_id")
    if (anyDuplicated(set_ids)) abort("The ORA set family repeats a set identifier.")
    claim$set_family <- normalized_sets
    claim
  })
  claim_keys <- vapply(claims, function(claim) paste(
    claim$analysis_id, claim$source_branch, claim$block_id, claim$claim_id, sep = "\r"
  ), character(1))
  if (anyDuplicated(claim_keys)) abort("The ORA protocol repeats one analysis, block, branch, and claim family.")
  claims
}

triomics_d_ora_candidate_rows <- function(resolved_claim, mapping_operation) {
  eligible <- list()
  excluded <- list()
  identity_incompatible <- FALSE
  for (row in resolved_claim$feature_rows) {
    reason <- NULL
    if (!identical(as.character(row$source_role), "measurement_matrix")) {
      reason <- "source_role_not_measurement_matrix"
    } else if (!identical(as.character(row$measured_or_derived), "measured")) {
      reason <- "feature_not_measured"
    } else if (identical(as.character(resolved_claim$source_branch), "primary_statistics") &&
               !identical(as.character(row$producer_status), "estimated")) {
      reason <- "producer_row_not_estimated"
    } else if (!identical(as.character(row$taxon_status), "declared") ||
               !identical(as.character(row$resolution_status), "declared") ||
               !identical(as.character(row$capability_status), "declared") ||
               !triomics_d_nonempty_string(row$original_id) ||
               !triomics_d_nonempty_string(row$source_namespace) ||
               !triomics_d_nonempty_string(row$taxon_id) ||
               !triomics_d_nonempty_string(row$resolution)) {
      reason <- "feature_mapping_identity_incomplete"
      identity_incompatible <- TRUE
    } else if (!mapping_operation %in% triomics_ora_characters(row$capabilities)) {
      reason <- "mapping_capability_not_declared"
      identity_incompatible <- TRUE
    }
    if (is.null(reason)) {
      eligible[[length(eligible) + 1L]] <- row
    } else {
      excluded[[length(excluded) + 1L]] <- list(
        feature_key = as.character(row$feature_key), original_id = as.character(row$original_id),
        source_id = as.character(row$source_id), reason = reason
      )
    }
  }
  list(rows = unname(eligible), excluded = unname(excluded),
       identity_incompatible = identity_incompatible)
}

triomics_d_ora_mapper_rows <- function(feature_rows, nominations) {
  lapply(feature_rows, function(row) {
    matched <- Filter(function(nomination) identical(as.character(nomination$feature_key),
                                                     as.character(row$feature_key)), nominations)
    mapper_nominations <- lapply(matched, function(nomination) {
      list(nomination_id = as.character(nomination$nomination_id),
           source_branch = as.character(nomination$source_branch))
    })
    list(
      feature_key = as.character(row$feature_key), source_id = as.character(row$original_id),
      source_namespace = as.character(row$source_namespace), taxon_id = as.character(row$taxon_id),
      resolution = as.character(row$resolution), nominations = mapper_nominations, aliases = list()
    )
  })
}

triomics_d_ora_mapping_contract <- function(path) {
  table <- tryCatch(utils::read.delim(
    path, colClasses = "character", check.names = FALSE,
    quote = "", comment.char = "", na.strings = character()
  ), error = function(error) {
    triomics_d_abort("D_ORA_MAPPING_INVALID", "The pinned ORA mapping table cannot be parsed.", "mapping_resource")
  })
  required <- c("source_namespace", "source_id", "taxon_id", "target_namespace", "target_id",
                "relation", "resolution", "evidence_ref", "resource_id")
  if (anyDuplicated(names(table)) || !all(required %in% names(table)) ||
      any(!vapply(table[required], function(column) all(!is.na(column) & nzchar(trimws(column))), logical(1)))) {
    triomics_d_abort("D_ORA_MAPPING_INVALID", "The pinned ORA mapping table lacks valid D02/D03 mapping fields.",
                     "mapping_resource")
  }
  list(
    namespaces = unique(table$source_namespace), organisms = unique(table$taxon_id),
    resolutions = unique(table$resolution), target_namespaces = unique(table$target_namespace),
    table_resource_ids = unique(table$resource_id)
  )
}

triomics_d_ora_empty_universe <- function(feature_rows, claim, reason) {
  excluded <- lapply(feature_rows, function(row) list(
    feature_key = as.character(row$feature_key), original_id = as.character(row$original_id),
    source_id = as.character(row$source_id), reason = reason
  ))
  list(
    universe = list(ids = character(), source = "measured", target_namespace = as.character(claim$target_namespace),
                    taxon_id = as.character(claim$taxon_id), mapping_rule = "unique_resolved_entity",
                    included = list(), excluded = unname(excluded),
                    mapping_counts = list(feature_count = as.integer(length(feature_rows)),
                                          uniquely_resolved_feature_count = 0L, unique_entity_count = 0L,
                                          ambiguous_feature_count = 0L, unmapped_feature_count = 0L,
                                          duplicate_entity_feature_count = 0L)),
    selected_ids = character(), nominated_feature_keys = list(), nominated_excluded = list()
  )
}

triomics_d_ora_run_claim <- function(evidence, protocol_claim, protocol, protocol_sha256,
                                     resources, cache_dir) {
  resolved <- triomics_ora_resolve_claim(evidence, protocol_claim)
  inventory <- triomics_ora_inventory_status(
    vapply(resolved$feature_rows, `[[`, character(1), "source_id"), evidence$source_by_id
  )
  candidate <- triomics_d_ora_candidate_rows(resolved, protocol_claim$mapping_operation)
  mapping_resource <- resources[[as.character(protocol_claim$mapping_resource_id)]]
  access <- triomics_d_annotation_resource_access(
    mapping_resource, as.character(protocol_claim$mapping_operation),
    as.character(mapping_resource$version), as.character(mapping_resource$sha256),
    cache_dir
  )
  mapping_result <- NULL
  compatibility <- list(status = "incompatible", reason_code = "D_ORA_MAPPING_INCOMPATIBLE",
                         reason = "The local mapping resource is unavailable.")
  if (identical(access$status, "available")) {
    contract <- triomics_d_ora_mapping_contract(access$path)
    if (length(contract$table_resource_ids) != 1L ||
        !identical(contract$table_resource_ids[[1L]], as.character(protocol_claim$mapping_resource_id))) {
      triomics_d_abort("D_ORA_MAPPING_INVALID", "The mapping table resource_id disagrees with its ResourceManifest.",
                       as.character(protocol_claim$mapping_resource_id))
    }
    compatibility <- triomics_ora_mapping_compatibility(protocol_claim, contract, candidate$rows)
    if (isTRUE(candidate$identity_incompatible)) {
      compatibility <- list(status = "incompatible", reason_code = "D_ORA_MAPPING_INCOMPATIBLE",
                            reason = "At least one producer feature lacks a complete declared mapping identity.")
    }
    mapper_rows <- triomics_d_ora_mapper_rows(candidate$rows, resolved$nominations)
    mapping_result <- triomics_d_annotation_resource_attempt(function() {
      if (identical(as.character(protocol_claim$mapping_operation), "lipid_ontology")) {
        triomics_map_lipids(mapper_rows, mapping_resource, as.character(protocol_claim$mapping_operation),
                            as.character(mapping_resource$version), as.character(mapping_resource$sha256), cache_dir)
      } else {
        triomics_map_reactions(mapper_rows, mapping_resource, as.character(protocol_claim$mapping_operation),
                               as.character(mapping_resource$version), as.character(mapping_resource$sha256), cache_dir)
      }
    }, mapping_resource, as.character(protocol_claim$mapping_operation))
    if (!identical(mapping_result$status, "available")) {
      compatibility <- list(status = "incompatible", reason_code = mapping_result$reason_code %||% "D_RESOURCE_MISSING",
                            reason = mapping_result$reason %||% "The local mapping resource is unavailable.")
    }
  } else {
    compatibility <- list(status = "incompatible",
                          reason_code = access$reason_code %||% "D_RESOURCE_MISSING",
                          reason = access$reason %||% "The local mapping resource is unavailable.")
  }
  if (is.list(mapping_result) && identical(mapping_result$status, "available")) {
    resolved_entities <- triomics_ora_unique_entities(
      candidate$rows, mapping_result, as.character(protocol_claim$target_namespace),
      as.character(protocol_claim$taxon_id), intersect(resolved$nomination_feature_keys,
                                                       vapply(candidate$rows, `[[`, character(1), "feature_key"))
    )
  } else {
    resolved_entities <- triomics_d_ora_empty_universe(candidate$rows, protocol_claim,
                                                       "mapping_resource_unavailable")
  }
  producer_excluded <- candidate$excluded
  selected_excluded <- Filter(function(row) row$feature_key %in% resolved$nomination_feature_keys,
                              producer_excluded)
  resolved_entities$universe$excluded <- c(resolved_entities$universe$excluded, producer_excluded)
  resolved_entities$nominated_excluded <- c(resolved_entities$nominated_excluded, selected_excluded)
  resolved_entities$universe$producer_feature_count <- as.integer(length(resolved$feature_rows))
  resolved_entities$universe$eligible_measured_feature_count <- as.integer(length(candidate$rows))
  resolved_entities$universe$source_inventory <- inventory$sources
  mapping_status <- if (identical(compatibility$status, "compatible") &&
                        is.list(mapping_result) && identical(mapping_result$status, "available")) {
    "compatible"
  } else {
    "incompatible"
  }
  family_result <- triomics_ora_family(
    resolved_entities$universe, protocol_claim$set_family, resolved_entities$selected_ids,
    inventory, mapping = list(rule = as.character(protocol$mapping_rule), status = mapping_status)
  )
  mapping_reason <- if (!identical(mapping_status, "compatible")) compatibility$reason_code else NULL
  inventory_reason <- if (!isTRUE(inventory$qualified)) "D_ORA_INVENTORY_UNQUALIFIED" else NULL
  claim_status <- if (identical(mapping_status, "compatible") && isTRUE(inventory$qualified)) {
    "complete"
  } else {
    "unavailable"
  }
  universe_artifact <- list(
    identity = list(study_id = as.character(evidence$study_id), analysis_id = resolved$analysis_id,
                    preparation_context_id = resolved$preparation_context_id,
                    representation_id = resolved$representation_id, block_id = resolved$block_id,
                    source_branch = resolved$source_branch, claim_id = resolved$claim_id,
                    family_id = resolved$family_id),
    source = resolved_entities$universe$source,
    source_inventory = inventory,
    mapping_rule = as.character(protocol$mapping_rule),
    target_namespace = as.character(protocol_claim$target_namespace),
    taxon_id = as.character(protocol_claim$taxon_id),
    ids = resolved_entities$universe$ids,
    included = resolved_entities$universe$included,
    excluded = resolved_entities$universe$excluded,
    mapping_counts = resolved_entities$universe$mapping_counts
  )
  universe_sha256 <- triomics_hash_text(triomics_canonical_json(universe_artifact))
  set_family_sha256 <- triomics_hash_text(triomics_canonical_json(protocol_claim$set_family))
  mapping_sha256 <- as.character((mapping_result$resource %||% access)$content_sha256 %||% mapping_resource$sha256)
  list(
    status = claim_status, reason_code = mapping_reason %||% inventory_reason,
    identity = list(
      study_id = as.character(evidence$study_id), analysis_id = resolved$analysis_id,
      preparation_context_id = resolved$preparation_context_id,
      representation_id = resolved$representation_id,
      block_id = resolved$block_id, source_branch = resolved$source_branch,
      claim_id = resolved$claim_id, integration_id = resolved$integration_id,
      family_id = resolved$family_id,
      source_ids = as.list(unique(vapply(resolved$feature_rows, `[[`, character(1), "source_id"))),
      nomination_ids = as.list(vapply(resolved$nominations, `[[`, character(1), "nomination_id"))
    ),
    protocol = list(resource_id = as.character(protocol$protocol_id), sha256 = protocol_sha256,
                    mapping_rule = as.character(protocol$mapping_rule),
                    aggregation_rule = as.character(protocol$aggregation_rule),
                    ambiguity_rule = as.character(protocol$ambiguity_rule)),
    mapping = list(status = mapping_status, reason_code = compatibility$reason_code,
                   reason = compatibility$reason, resource = mapping_result$resource %||% access,
                   observations = mapping_result$observations %||% list(),
                   edges = mapping_result$edges %||% list()),
    inventory = inventory,
    measured_universe = c(universe_artifact, list(sha256 = universe_sha256)),
    selected_entities = list(feature_keys = as.list(resolved$nomination_feature_keys),
                             entity_ids = as.list(resolved_entities$selected_ids),
                             excluded = resolved_entities$nominated_excluded),
    set_family = list(sha256 = set_family_sha256,
                      declared = protocol_claim$set_family,
                      declared_count = as.integer(family_result$full_family_size),
                      result = family_result),
    bindings = list(protocol_sha256 = protocol_sha256, mapping_sha256 = mapping_sha256,
                    set_family_sha256 = set_family_sha256,
                    measured_universe_sha256 = universe_sha256),
    interpretation = "over_representation_not_pathway_activation_or_patient_score"
  )
}

triomics_d_ora_run <- function(evidence, request, resources, cache_dir) {
  protocol_ref <- as.character(request$protocol_ref %||% "")
  if (!triomics_d_nonempty_string(protocol_ref) || is.null(resources[[protocol_ref]])) {
    triomics_d_abort("D_ORA_PROTOCOL_INVALID", "annotation request.protocol_ref must identify a local ORA protocol ResourceManifest.",
                     "annotation.requests.protocol_ref")
  }
  request_resource_ids <- as.character(unlist(request$resource_ids %||% list(), use.names = FALSE))
  protocol_resource <- resources[[protocol_ref]]
  protocol_access <- tryCatch(
    triomics_d_resource_access(
      protocol_resource, "ora_protocol", as.character(protocol_resource$version),
      as.character(protocol_resource$sha256), cache_dir, network_policy = "offline"
    ),
    error = function(error) {
      if (inherits(error, "triomics_error") &&
          identical(as.character(error$code), "D_RESOURCE_RIGHTS")) {
        return(triomics_d_resource_status_from_error(error, protocol_resource, "ora_protocol"))
      }
      stop(error)
    }
  )
  if (!identical(protocol_access$status, "available")) {
    return(list(
      route = list(status = "unavailable", reason_code = protocol_access$reason_code,
                   protocol_resource = protocol_access, claims = list(),
                   interpretation = "not_pathway_activation"),
      enrichment = list(status = "unavailable", reason_code = protocol_access$reason_code,
                        test_count = 0L, results = list())
    ))
  }
  protocol_text <- tryCatch(paste(readLines(protocol_access$path, warn = FALSE), collapse = "\n"),
                            error = function(error) NA_character_)
  protocol <- triomics_d_ora_parse_protocol_text(protocol_text)
  protocol_sha256 <- triomics_sha256(protocol_access$path)
  normalized_claims <- triomics_d_ora_validate_protocol(
    protocol, protocol_ref, as.character(evidence$analysis_id), request_resource_ids
  )
  claims <- lapply(normalized_claims, function(claim) {
    triomics_d_ora_run_claim(evidence, claim, protocol, protocol_sha256, resources, cache_dir)
  })
  claim_statuses <- vapply(claims, `[[`, character(1), "status")
  route_status <- if (all(claim_statuses == "complete")) "complete" else "unavailable"
  route_reason <- if (identical(route_status, "complete")) NULL else {
    first <- which(claim_statuses != "complete")[[1L]]
    claims[[first]]$reason_code %||% "D_ORA_CLAIM_UNAVAILABLE"
  }
  summaries <- lapply(claims, function(claim) list(
    identity = claim$identity,
    status = claim$status,
    family_size = claim$set_family$result$full_family_size,
    tests_in_bh = claim$set_family$result$tests_in_bh,
    estimable_count = claim$set_family$result$estimable_count,
    sets = claim$set_family$result$sets,
    interpretation = "not_pathway_activation"
  ))
  list(
    route = list(status = route_status, reason_code = route_reason,
                 protocol_resource = protocol_access, claims = claims,
                 family_scope = "BH is applied separately within each predeclared claim family",
                 interpretation = "not_pathway_activation"),
    enrichment = list(
      status = route_status, reason_code = route_reason,
      test_count = as.integer(sum(vapply(claims, function(claim) claim$set_family$result$tests_in_bh,
                                        integer(1)))),
      results = unname(summaries)
    )
  )
}

triomics_d_annotate_local_resources <- function(evidence, annotation, cache_dir,
                                                     target_investigation, config_path) {
  annotation <- annotation %||% list()
  requests <- triomics_d_request_list(annotation)
  resources <- triomics_d_index_resources(annotation)
  config_root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  for (resource_id in names(resources)) {
    source_path <- as.character(resources[[resource_id]]$local_file %||% "")
    # Preserve the leaf path so resource access can reject a symlink before hashing it.
    resources[[resource_id]]$local_file <- triomics_d_resolve_resource_path(source_path, config_root)
  }
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
    parts <- parts[vapply(parts, function(part) is.list(part) &&
                            identical(as.character(part$status %||% "available"), "available"),
                          logical(1))]
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
      mappings[[resource_id]] <- triomics_d_annotation_resource_attempt(function() {
        if (identical(route, "lipid_ontology")) {
          triomics_map_lipids(measured, resource, route, as.character(resource$version),
                              as.character(resource$sha256), cache_dir)
        } else {
          triomics_map_reactions(measured, resource, route, as.character(resource$version),
                                 as.character(resource$sha256), cache_dir)
        }
      }, resource, route)
    }
    routed$routes[[route]]$mappings <- mappings
  }
  if (!is.null(by_route$ora)) {
    ora <- triomics_d_ora_run(evidence, by_route$ora$request, resources, cache_dir)
    routed$routes$ora <- ora$route
    routed$enrichment <- ora$enrichment
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
    if (identical(route, "ora")) {
      status_routes[[route]]$status <- as.character(routed$routes$ora$status)
      status_routes[[route]]$reason_code <- routed$routes$ora$reason_code %||% NULL
      next
    }
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
  target <- triomics_d_target_slot(target_investigation %||% list(), config_path, evidence = evidence)
  if (isTRUE((target_investigation %||% list())$required) && identical(target$status, "unavailable")) {
    route_status$exit_code <- 2L
  }
  routed$target_investigation <- target
  routed$route_status <- route_status
  routed
}
