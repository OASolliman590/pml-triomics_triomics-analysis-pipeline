triomics_map_reactions <- function(measured_rows, resource, operation, expected_version,
                                   expected_sha256, cache_dir) {
  abort <- function(code, message) triomics_d_abort(code, message, "reaction_mapping")
  if (!is.list(measured_rows) || is.data.frame(measured_rows) ||
      (length(measured_rows) && !all(vapply(measured_rows, function(x) is.list(x) && !is.data.frame(x), logical(1))))) {
    abort("D_REACTION_INPUT_INVALID", "Measured rows must be an explicit list of rows.")
  }
  if (!identical(operation, "gene_reaction")) {
    abort("D_REACTION_INPUT_INVALID", "The operation must be gene_reaction.")
  }
  if (!is.list(resource) || !triomics_d_nonempty_string(resource$resource_id)) {
    abort("D_REACTION_IDENTITY_REQUIRED", "A resource identity is required.")
  }
  keys <- character()
  for (row in measured_rows) {
    if (!triomics_d_nonempty_string(row$source_namespace) || !triomics_d_nonempty_string(row$taxon_id)) {
      abort("D_REACTION_IDENTITY_REQUIRED", "Each measured row requires a namespace and taxon.")
    }
    if (!triomics_d_nonempty_string(row$feature_key) || !triomics_d_nonempty_string(row$source_id) ||
        row$feature_key %in% keys || !is.list(row$nominations) || is.data.frame(row$nominations) ||
        any(!vapply(row$nominations, function(n) {
          is.list(n) && triomics_d_nonempty_string(n$nomination_id) &&
            triomics_d_nonempty_string(n$source_branch) && n$source_branch %in% c("primary_statistics", "integration")
        }, logical(1)))) {
      abort("D_REACTION_INPUT_INVALID", "Each row needs a unique feature, source ID, and valid nomination list.")
    }
    nomination_ids <- vapply(row$nominations, `[[`, "", "nomination_id")
    branches <- vapply(row$nominations, `[[`, "", "source_branch")
    if (anyDuplicated(nomination_ids) || anyDuplicated(branches)) {
      abort("D_REACTION_INPUT_INVALID", "Nomination IDs and source branches must be unique per feature.")
    }
    keys <- c(keys, row$feature_key)
  }
  access <- triomics_d_resource_access(resource, operation, expected_version, expected_sha256,
                                      cache_dir, network_policy = "offline")
  if (identical(access$status, "unavailable")) return(access)
  required <- c("source_namespace", "source_id", "taxon_id", "target_namespace", "target_id",
                "relation", "resolution", "evidence_ref", "resource_id")
  table <- tryCatch(utils::read.delim(access$path, colClasses = "character", check.names = FALSE,
                                     quote = "", comment.char = "", na.strings = character()),
                    error = function(e) abort("D_REACTION_TABLE_INVALID", "The local mapping table cannot be parsed."))
  if (anyDuplicated(names(table)) || !all(required %in% names(table))) {
    abort("D_REACTION_TABLE_INVALID", "The local mapping table lacks required columns.")
  }
  identity <- c("source_namespace", "taxon_id", "target_namespace", "resource_id")
  if (any(!vapply(table[identity], function(x) all(!is.na(x) & nzchar(trimws(x))), logical(1)))) {
    abort("D_REACTION_IDENTITY_REQUIRED", "Mapping rows require namespace, taxon, and resource identity.")
  }
  derived_type <- ifelse(table$target_namespace == "GO", "go_function", "reaction")
  if ("edge_type" %in% names(table) &&
      any(is.na(table$edge_type) | table$edge_type != derived_type)) {
    abort("D_REACTION_TABLE_INVALID", "The edge type contradicts its target namespace.")
  }
  table$edge_type <- derived_type
  if (any(!vapply(table[required], function(x) all(!is.na(x) & nzchar(trimws(x))), logical(1))) ||
      any(table$resource_id != resource$resource_id)) {
    abort("D_REACTION_TABLE_INVALID", "The local mapping table has invalid fields, resource identity, or edge type.")
  }
  observations <- list(); edges <- list(); groups <- list()
  for (row in measured_rows) {
    candidates <- table[table$source_namespace == row$source_namespace & table$source_id == row$source_id &
                        table$taxon_id == row$taxon_id, , drop = FALSE]
    lineage <- paste0("feature:", row$feature_key, ";nominations:",
                      paste(vapply(row$nominations, `[[`, "", "nomination_id"), collapse = ","))
    target_keys <- unique(candidates[c("target_namespace", "target_id", "edge_type")])
    counts <- table(target_keys$edge_type)
    for (type in names(counts)[counts > 1L]) {
      groups[[length(groups) + 1L]] <- list(feature_key = row$feature_key, edge_type = type,
                                           ambiguity_group = paste0("ambiguity:", type, ":", row$feature_key))
    }
    observations[[length(observations) + 1L]] <- list(feature_key = row$feature_key, source_id = row$source_id,
      source_namespace = row$source_namespace, taxon_id = row$taxon_id, nominations = row$nominations,
      mapping_status = if (nrow(candidates)) "mapped" else "unmapped", evidence_lineage = lineage,
      independent_corroboration = FALSE)
    for (i in seq_len(nrow(target_keys))) {
      target <- target_keys[i, , drop = FALSE]
      selected <- candidates[candidates$target_namespace == target$target_namespace & candidates$target_id == target$target_id &
                             candidates$edge_type == target$edge_type, , drop = FALSE]
      if (nrow(unique(selected[c("relation", "resolution", "evidence_ref")])) != 1L) {
        abort("D_REACTION_TABLE_INVALID", "Duplicate target edges disagree on relation or evidence.")
      }
      edges[[length(edges) + 1L]] <- list(feature_key = row$feature_key, target_namespace = target$target_namespace[[1L]],
        target_entity_id = target$target_id[[1L]], relation = selected$relation[[1L]], resolution = selected$resolution[[1L]], edge_type = target$edge_type[[1L]],
        taxon = row$taxon_id, resource_release = access$release, evidence = selected$evidence_ref[[1L]],
        ambiguity_group = if (counts[[target$edge_type[[1L]]]] > 1L) paste0("ambiguity:", target$edge_type[[1L]], ":", row$feature_key) else NULL, evidence_lineage = lineage)
    }
  }
  list(status = "available", resource = access, observations = observations, edges = edges, ambiguity_groups = groups)
}
