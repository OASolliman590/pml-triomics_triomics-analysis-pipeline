triomics_map_lipids <- function(measured_rows, resource, operation, expected_version,
                                expected_sha256, cache_dir) {
  access <- triomics_d_resource_access(resource, operation, expected_version,
                                      expected_sha256, cache_dir, network_policy = "offline")
  if (identical(access$status, "unavailable")) return(access)
  abort <- function(code, message) triomics_d_abort(code, message, "lipid_mapping")
  required <- c("source_namespace", "source_id", "taxon_id", "target_namespace", "target_id",
                "relation", "resolution", "evidence_ref", "resource_id")
  table <- utils::read.delim(access$path, colClasses = "character", check.names = FALSE,
                             quote = "", comment.char = "", na.strings = character())
  if (anyDuplicated(names(table)) || !all(required %in% names(table)) ||
      any(!vapply(table[required], function(x) all(!is.na(x) & nzchar(x)), logical(1))) ||
      any(table$resource_id != resource$resource_id)) {
    abort("D_LIPID_TABLE_INVALID", "The local lipid table has invalid required fields or resource identity.")
  }
  ranks <- c(unresolved = 0L, class = 1L, sum_composition = 2L, molecular_species = 3L, sn_resolved = 4L)
  if (any(!table$resolution %in% names(ranks))) {
    abort("D_LIPID_TABLE_INVALID", "The local lipid table has an unsupported resolution.")
  }
  if (!is.list(measured_rows) || is.data.frame(measured_rows) ||
      (length(measured_rows) && !all(vapply(measured_rows, is.list, logical(1))))) {
    abort("D_LIPID_INPUT_INVALID", "Measured lipids must be a list of measured rows.")
  }
  parse_sum <- function(value) {
    match <- regexec('^([A-Za-z][A-Za-z0-9-]*)(?:\\(([0-9]+):([0-9]+)\\)| +([0-9]+):([0-9]+))$', value, perl = TRUE)
    parts <- regmatches(value, match)[[1L]]
    if (!length(parts)) return(NULL)
    list(class = parts[[2L]], carbons = as.integer(if (nzchar(parts[[3L]])) parts[[3L]] else parts[[5L]]),
         unsaturations = as.integer(if (nzchar(parts[[4L]])) parts[[4L]] else parts[[6L]]))
  }
  has_structure <- function(value, source_rank) {
    value <- trimws(value)
    if (!nzchar(value) || tolower(value) %in% c('unresolved', 'unknown', 'na', 'none', 'not_applicable')) return(FALSE)
    if (grepl('sn[[:space:]-]*[12]', value, ignore.case = TRUE, perl = TRUE)) return(TRUE)
    sum <- parse_sum(value)
    if (!is.null(sum)) return(source_rank < ranks[['sum_composition']])
    grepl('(?:sn[[:space:]-]*[12]|[0-9]+:[0-9]+)', value, ignore.case = TRUE, perl = TRUE)
  }
  observations <- list(); edges <- list(); groups <- list(); keys <- character()
  for (row in measured_rows) {
    fields <- c("feature_key", "source_id", "source_namespace", "taxon_id", "resolution")
    if (any(!vapply(fields, function(field) triomics_d_nonempty_string(row[[field]]), logical(1))) ||
        !row$resolution %in% names(ranks) || row$feature_key %in% keys) {
      abort("D_LIPID_INPUT_INVALID", "Each measured row needs a unique feature key and declared lipid identity and resolution.")
    }
    keys <- c(keys, row$feature_key)
    parsed <- parse_sum(row$source_id)
    if (!is.null(parsed) && ranks[[row$resolution]] > ranks[['sum_composition']]) {
      abort("D_LIPID_RESOLUTION_CONFLICT", "A sum-composition source cannot declare finer resolution.")
    }
    if (is.null(parsed) && grepl('^[A-Za-z][A-Za-z0-9-]*$', row$source_id) &&
        ranks[[row$resolution]] > ranks[['class']]) {
      abort("D_LIPID_RESOLUTION_CONFLICT", "A class source cannot declare finer resolution.")
    }
    aliases <- row$aliases %||% list()
    if (!is.list(aliases) || any(!vapply(aliases, function(a) {
      is.list(a) && all(vapply(c("source_id", "source_namespace", "provenance_ref"),
                                function(field) triomics_d_nonempty_string(a[[field]]), logical(1)))
    }, logical(1)))) abort("D_LIPID_INPUT_INVALID", "Aliases require an identity and approval provenance.")
    identities <- c(list(list(source_id = row$source_id, source_namespace = row$source_namespace)), aliases)
    hits <- integer()
    for (identity in identities) {
      hits <- union(hits, which(table$source_id == identity$source_id &
                                table$source_namespace == identity$source_namespace & table$taxon_id == row$taxon_id))
    }
    candidates <- table[hits, , drop = FALSE]
    if (nrow(candidates)) {
      if (any(ranks[candidates$resolution] > ranks[[row$resolution]])) {
        abort("D_LIPID_RESOLUTION_CONFLICT", "A mapping asserts structure finer than its measured source.")
      }
      if (ranks[[row$resolution]] <= ranks[['sum_composition']] &&
          any(vapply(unlist(candidates, use.names = FALSE), has_structure, logical(1), source_rank = ranks[[row$resolution]]))) {
        abort("D_LIPID_RESOLUTION_CONFLICT", "A mapping cell asserts structure finer than its measured source.")
      }
    }
    target_keys <- unique(paste(candidates$target_namespace, candidates$target_id, sep = '\r'))
    group_id <- if (length(target_keys) > 1L) paste0('lipid:', row$feature_key) else NULL
    if (!is.null(group_id)) groups[[length(groups) + 1L]] <- list(feature_key = row$feature_key, ambiguity_group = group_id, targets = target_keys)
    observations[[length(observations) + 1L]] <- list(
      feature_key = row$feature_key, source_id = row$source_id,
      source_namespace = row$source_namespace, taxon_id = row$taxon_id,
      resolution = row$resolution, class = if (!is.null(parsed)) parsed$class else if (row$resolution == 'class') row$source_id else NULL,
      carbons = if (!is.null(parsed)) parsed$carbons else NULL,
      unsaturations = if (!is.null(parsed)) parsed$unsaturations else NULL,
      chains = NULL, sn_positions = NULL, isomer = NULL, aliases = aliases,
      mapping_status = if (nrow(candidates)) 'mapped' else 'unmapped')
    for (target in target_keys) {
      selected <- candidates[paste(candidates$target_namespace, candidates$target_id, sep = '\r') == target, , drop = FALSE]
      if (nrow(unique(selected[c('relation', 'resolution', 'evidence_ref')])) != 1L) {
        abort("D_LIPID_TABLE_INVALID", "Rows for one target disagree on relation, resolution, or evidence.")
      }
      edges[[length(edges) + 1L]] <- list(feature_key = row$feature_key,
        target_namespace = selected$target_namespace[[1L]], target_entity_id = selected$target_id[[1L]],
        relation = selected$relation[[1L]], resolution = selected$resolution[[1L]], taxon = row$taxon_id,
        context = NULL, resource_release = access$release, evidence = selected$evidence_ref[[1L]],
        ambiguity_group = group_id)
    }
  }
  list(status = "available", resource = access, observations = observations, edges = edges, ambiguity_groups = groups)
}
