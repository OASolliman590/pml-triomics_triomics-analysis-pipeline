triomics_as_characters <- function(value, key) {
  if (is.null(value) || length(value) == 0L) return(character())
  if (is.character(value)) return(value)
  if (!is.list(value)) {
    triomics_abort("A_SOURCE_CONTRACT", paste0("'", key, "' must be a list of strings."),
                   key = key, module = "ingest", stage = "A",
                   remediation = "Declare the field as a sequence of strings.")
  }
  vapply(value, function(item) {
    if (!is.character(item) || length(item) != 1L || is.na(item)) {
      triomics_abort("A_SOURCE_CONTRACT", paste0("'", key, "' must be a list of strings."),
                     key = key, module = "ingest", stage = "A",
                     remediation = "Declare the field as a sequence of strings.")
    }
    item
  }, character(1))
}

triomics_mask_code <- function(label) {
  if (identical(label, "observed")) return("known_observed")
  if (identical(label, "missing")) return("known_missing")
  if (identical(label, "imputed")) return("known_imputed")
  if (identical(label, "unknown")) return("unknown")
  triomics_abort("A_SOURCE_MASK", "Cell mask code is not observed, missing, imputed, or unknown.",
                 key = "cell_mask", module = "ingest", stage = "A",
                 remediation = "Use the four cell-mask codes.")
}

triomics_inference_gate <- function(history) {
  if (identical(history, "full_unselected")) return("declared_unselected")
  if (identical(history, "prespecified_panel")) return("panel_only")
  if (identical(history, "unknown") || identical(history, "outcome_selected")) return("withheld")
  triomics_abort("A_SOURCE_SELECTION", "Selection history is not a known gate.",
                 key = "selection_history", module = "ingest", stage = "A",
                 remediation = "Use full_unselected, prespecified_panel, outcome_selected, or unknown.")
}

triomics_tolerances_agree <- function(left, right) {
  if (is.null(left) && is.null(right)) return(TRUE)
  if (is.numeric(left) && is.numeric(right) && length(left) == 1L && length(right) == 1L) {
    return(isTRUE(all.equal(unname(left), unname(right))))
  }
  FALSE
}

triomics_overlap_conflict <- function(left_token, right_token, tolerance) {
  pattern <- triomics_measurement_pattern()
  left_num <- isTRUE(grepl(pattern, left_token, perl = TRUE))
  right_num <- isTRUE(grepl(pattern, right_token, perl = TRUE))
  if (left_num && right_num) {
    limit <- if (is.null(tolerance)) 0 else tolerance
    return(abs(as.numeric(left_token) - as.numeric(right_token)) > limit)
  }
  !identical(left_token, right_token)
}

# Each block keeps its own cell. Agreement never replaces either value with a mean.
triomics_check_region_overlaps <- function(entries) {
  if (length(entries) < 2L) return(invisible(NULL))
  for (i in seq_len(length(entries) - 1L)) {
    for (j in seq.int(i + 1L, length(entries))) {
      left <- entries[[i]]
      right <- entries[[j]]
      if (is.null(left$original_hash) || is.null(right$original_hash)) next
      if (!identical(left$original_hash, right$original_hash)) next
      if (is.null(left$sheet) || is.null(right$sheet) || !identical(left$sheet, right$sheet)) next
      if (!triomics_tolerances_agree(left$tolerance, right$tolerance)) {
        triomics_abort("A_SOURCE_OVERLAP",
                       "Overlapping workbook regions declare conflicting tolerances and were not averaged.",
                       key = "region", module = "ingest", stage = "A",
                       remediation = "Declare one shared tolerance. This ingest does not average cells.")
      }
      left_rows <- seq.int(left$row_start, left$row_start + nrow(left$cells) - 1L)
      right_rows <- seq.int(right$row_start, right$row_start + nrow(right$cells) - 1L)
      left_cols <- seq.int(left$col_start, left$col_start + ncol(left$cells) - 1L)
      right_cols <- seq.int(right$col_start, right$col_start + ncol(right$cells) - 1L)
      rows <- intersect(left_rows, right_rows)
      cols <- intersect(left_cols, right_cols)
      if (length(rows) == 0L || length(cols) == 0L) next
      for (sheet_row in rows) {
        for (sheet_col in cols) {
          left_token <- left$cells[sheet_row - left$row_start + 1L, sheet_col - left$col_start + 1L]
          right_token <- right$cells[sheet_row - right$row_start + 1L, sheet_col - right$col_start + 1L]
          if (triomics_overlap_conflict(left_token, right_token, left$tolerance)) {
            triomics_abort("A_SOURCE_OVERLAP",
                           "Overlapping workbook regions disagree and were not averaged.",
                           key = "region", module = "ingest", stage = "A",
                           remediation = "Keep each block's own cell. This ingest does not average or prefer the first file.")
          }
        }
      }
    }
  }
  invisible(NULL)
}

triomics_assign_column_roles <- function(header, registry_ids, feature_id_column, declared) {
  if (anyDuplicated(header)) {
    duplicated_names <- unique(header[duplicated(header)])
    triomics_abort("A_ID_DUPLICATE_SAMPLE",
                   paste0("Sample column '", duplicated_names[[1L]], "' is duplicated."),
                   key = "sample_id", module = "ingest", stage = "A",
                   remediation = "Keep one column per sample. This ingest does not average duplicates.")
  }
  if (is.null(declared)) {
    roles <- character(length(header))
    for (i in seq_along(header)) {
      name <- header[[i]]
      if (identical(name, feature_id_column)) {
        roles[[i]] <- "feature_id"
      } else if (name %in% registry_ids) {
        if (isTRUE(triomics_is_summary_column(name))) {
          triomics_abort("A_SOURCE_SUMMARY_AS_MEASUREMENT",
                         paste0("Summary column '", name, "' cannot be a measurement."),
                         key = name, module = "ingest", stage = "A",
                         remediation = "Remove summary and statistic columns from the sample registry.")
        }
        roles[[i]] <- "measurement"
      } else {
        triomics_abort("A_SOURCE_ROLE",
                       paste0("Column '", name, "' has no declared source role."),
                       key = name, module = "ingest", stage = "A",
                       remediation = "Declare a role for every column that is not a registered sample.")
      }
    }
    return(roles)
  }
  if (any(!header %in% names(declared)) || any(!names(declared) %in% header)) {
    triomics_abort("A_SOURCE_ROLE", "Column roles do not match the source header.",
                   key = "column_roles", module = "ingest", stage = "A",
                   remediation = "Declare exactly one role for every header and no extra names.")
  }
  unname(declared[header])
}

triomics_reject_summary_measurements <- function(header, roles) {
  for (i in seq_along(header)) {
    if (identical(roles[[i]], "measurement") && isTRUE(triomics_is_summary_column(header[[i]]))) {
      triomics_abort("A_SOURCE_SUMMARY_AS_MEASUREMENT",
                     paste0("Summary column '", header[[i]], "' cannot be a measurement."),
                     key = header[[i]], module = "ingest", stage = "A",
                     remediation = "Declare summary and statistic columns as summary or imported_statistic.")
    }
  }
  invisible(roles)
}

triomics_sample_for_column <- function(column_name, registry_ids, sample_map) {
  if (is.null(sample_map)) {
    if (!column_name %in% registry_ids) {
      triomics_abort("A_ID_UNKNOWN_SAMPLE",
                     paste0("Sample column '", column_name, "' is not in the sample registry."),
                     key = column_name, module = "ingest", stage = "A",
                     remediation = "Do not invent samples for undeclared columns.")
    }
    return(column_name)
  }
  if (!is.data.frame(sample_map) || !all(c("source_column", "sample_id") %in% names(sample_map))) {
    triomics_abort("A_SOURCE_CONTRACT", "Sample map must have source_column and sample_id.",
                   key = "sample_map", module = "ingest", stage = "A",
                   remediation = "Provide a one-to-one source-column map.")
  }
  hits <- which(sample_map$source_column == column_name)
  if (length(hits) != 1L) {
    triomics_abort("A_ID_UNKNOWN_SAMPLE",
                   paste0("Sample column '", column_name, "' has no one-to-one sample map entry."),
                   key = column_name, module = "ingest", stage = "A",
                   remediation = "Map each measurement column to one registered sample.")
  }
  sample_id <- sample_map$sample_id[[hits[[1L]]]]
  if (!is.character(sample_id) || length(sample_id) != 1L || is.na(sample_id) ||
      !sample_id %in% registry_ids) {
    triomics_abort("A_ID_UNKNOWN_SAMPLE",
                   paste0("Mapped sample '", sample_id, "' is not in the sample registry."),
                   key = "sample_map", module = "ingest", stage = "A",
                   remediation = "Map measurement columns only to registered samples.")
  }
  sample_id
}

triomics_codes_from_cell_mask <- function(cell_mask, feature_id_column, original_ids, sample_ids) {
  header <- cell_mask$header
  columns <- cell_mask$columns
  if (!is.character(header) || !is.list(columns) || length(header) != length(columns)) {
    triomics_abort("A_SOURCE_MASK", "Cell mask header and columns do not align.",
                   key = "cell_mask", module = "ingest", stage = "A",
                   remediation = "Supply one mask column per header, including the feature id.")
  }
  feature_pos <- match(feature_id_column, header)
  if (is.na(feature_pos) || !identical(columns[[feature_pos]], original_ids)) {
    triomics_abort("A_SOURCE_MASK", "Cell mask feature ids do not match the source rows.",
                   key = "cell_mask", module = "ingest", stage = "A",
                   remediation = "Repeat the source feature ids in the cell mask.")
  }
  codes <- vector("list", length(sample_ids))
  names(codes) <- sample_ids
  for (sample_id in sample_ids) {
    pos <- match(sample_id, header)
    raw <- if (is.na(pos)) NULL else columns[[pos]]
    if (is.null(raw) || length(raw) != length(original_ids)) {
      triomics_abort("A_SOURCE_MASK", "Cell mask does not cover every assay sample.",
                     key = "cell_mask", module = "ingest", stage = "A",
                     remediation = "Provide a mask code for every measured sample and row.")
    }
    codes[[sample_id]] <- vapply(raw, triomics_mask_code, character(1))
  }
  codes
}

triomics_build_original_mask <- function(status, cell_mask, feature_id_column, original_ids,
                                        assay) {
  if (!status %in% c("known", "partly_known", "unknown")) {
    triomics_abort("A_SOURCE_MASK", "Original mask status must be known, partly_known, or unknown.",
                   key = "original_mask_status", module = "ingest", stage = "A",
                   remediation = "Declare the original mask status. Do not infer it from upstream text.")
  }
  mask <- matrix(NA_character_, nrow(assay), ncol(assay), dimnames = dimnames(assay))
  sample_ids <- colnames(assay)
  if (identical(status, "partly_known") && is.null(cell_mask)) {
    triomics_abort("A_SOURCE_MASK", "A partly known mask needs a cell mask.",
                   key = "cell_mask", module = "ingest", stage = "A",
                   remediation = "Supply observed, missing, imputed, or unknown for every cell.")
  }
  if (!is.null(cell_mask)) {
    codes <- triomics_codes_from_cell_mask(cell_mask, feature_id_column, original_ids, sample_ids)
    for (sample_id in sample_ids) mask[, sample_id] <- codes[[sample_id]]
  } else if (identical(status, "unknown")) {
    mask[] <- "unknown"
  } else {
    mask[] <- "known_observed"
    mask[is.na(assay)] <- "known_missing"
  }
  if (identical(status, "unknown") && any(mask != "unknown")) {
    triomics_abort("A_SOURCE_MASK", "An unknown original mask cannot be upgraded to observed.",
                   key = "cell_mask", module = "ingest", stage = "A",
                   remediation = "Leave unknown history unknown. A cell mask cannot invent observations.")
  }
  codes <- as.vector(mask)
  values <- as.vector(assay)
  if (any(codes == "known_missing" & !is.na(values)) ||
      any(codes %in% c("known_observed", "known_imputed") & is.na(values))) {
    triomics_abort("A_SOURCE_MASK", "Cell mask codes disagree with missingness.",
                   key = "cell_mask", module = "ingest", stage = "A",
                   remediation = "Use missing only for NA cells and observed or imputed only for values.")
  }
  mask
}

triomics_column_tokens <- function(rows, index) {
  if (length(rows) == 0L) return(character())
  vapply(rows, function(row) row[[index]], character(1))
}

triomics_character_matrix <- function(rows, n_cols) {
  n_rows <- length(rows)
  cells <- matrix(NA_character_, n_rows, n_cols)
  if (n_rows == 0L) return(cells)
  for (j in seq_len(n_cols)) cells[, j] <- triomics_column_tokens(rows, j)
  cells
}

triomics_ingest_block <- function(study_id, registry_ids, block, source) {
  if (!is.null(block$technical_replicate_spec_ref)) {
    triomics_abort("A_SOURCE_REPLICATE",
                   "Technical replicates are not averaged by source ingest.",
                   key = "technical_replicate_spec_ref", module = "ingest", stage = "A",
                   remediation = "Leave replicate aggregation to its own contract. Do not average samples here.")
  }
  table <- triomics_read_delimited(source$local_ref, source$format)
  header <- table$header
  roles <- triomics_assign_column_roles(header, registry_ids, block$feature_id_column, source$column_roles)
  triomics_reject_summary_measurements(header, roles)
  if (sum(roles == "feature_id") != 1L) {
    triomics_abort("A_SOURCE_ROLE", "Each measurement table needs exactly one feature-id column.",
                   key = "feature_id_column", module = "ingest", stage = "A",
                   remediation = "Mark one column as feature_id.")
  }
  feature_index <- which(roles == "feature_id")[[1L]]
  n <- length(table$rows)
  original_ids <- triomics_column_tokens(table$rows, feature_index)
  if (n > 0L) original_ids <- triomics_preserve_identifier(original_ids, key = "feature_id")
  source_rows <- as.character(seq_len(n))
  keys <- if (n == 0L) character() else vapply(source_rows, function(source_row) {
    triomics_feature_key(study_id, block$block_id, source_row)
  }, character(1))
  measurement_index <- which(roles == "measurement")
  resolved <- character(length(measurement_index))
  parsed <- vector("list", length(measurement_index))
  source_of <- character()
  for (k in seq_along(measurement_index)) {
    index <- measurement_index[[k]]
    sample_id <- triomics_sample_for_column(header[[index]], registry_ids, block$sample_map)
    resolved[[k]] <- sample_id
    parsed[[k]] <- triomics_parse_measurements(triomics_column_tokens(table$rows, index), block$missing_tokens)
    source_of[[sample_id]] <- header[[index]]
  }
  if (anyDuplicated(resolved)) {
    triomics_abort("A_ID_DUPLICATE_SAMPLE", "Two measurement columns map to the same sample.",
                   key = "sample_id", module = "ingest", stage = "A",
                   remediation = "Keep the sample map one-to-one. This ingest does not average replicates.")
  }
  present <- registry_ids[registry_ids %in% resolved]
  assay <- matrix(NA_real_, n, length(present), dimnames = list(keys, present))
  for (k in seq_along(resolved)) assay[, resolved[[k]]] <- parsed[[k]]
  mask <- triomics_build_original_mask(
    block$measurement$original_mask_status, block$cell_mask, block$feature_id_column, original_ids, assay
  )
  alias_index <- which(roles == "alias")
  alias_refs <- if (length(alias_index) == 0L || n == 0L) {
    rep("", n)
  } else {
    apply(triomics_character_matrix(table$rows, length(header))[, alias_index, drop = FALSE], 1L, paste, collapse = "|")
  }
  feature_registry <- data.frame(
    study_id = rep(study_id, n),
    block_id = rep(block$block_id, n),
    feature_key = keys,
    original_id = original_ids,
    id_namespace = rep(block$id_namespace, n),
    source_id = rep(source$source_id, n),
    source_row = source_rows,
    measured_or_derived = rep("measured", n),
    quantity = rep(block$measurement$quantity, n),
    scale = rep(block$measurement$scale, n),
    units = rep(block$measurement$units, n),
    structural_resolution = rep("unknown", n),
    alias_refs = alias_refs,
    parent_refs = rep("", n),
    annotation_route = rep(block$annotation_route, n),
    stringsAsFactors = FALSE
  )
  presence <- ifelse(registry_ids %in% colnames(assay), "present", "structural_absence")
  availability <- data.frame(
    study_id = rep(study_id, length(registry_ids)),
    block_id = rep(block$block_id, length(registry_ids)),
    sample_id = registry_ids,
    presence = presence,
    cell_imputation_eligible = presence == "present",
    stringsAsFactors = FALSE
  )
  sample_map <- data.frame(
    block_id = rep(block$block_id, length(present)),
    assay_column = present,
    sample_id = present,
    source_column = unname(source_of[present]),
    stringsAsFactors = FALSE
  )
  column_accounting <- data.frame(
    block_id = rep(block$block_id, length(header)),
    source_id = rep(source$source_id, length(header)),
    position = seq_along(header),
    column = header,
    role = roles,
    sample_id = NA_character_,
    included_in_assay = roles == "measurement",
    stringsAsFactors = FALSE
  )
  for (k in seq_along(measurement_index)) {
    column_accounting$sample_id[[measurement_index[[k]]]] <- resolved[[k]]
  }
  steps <- block$measurement$upstream_steps
  if (is.null(steps)) steps <- character()
  if (!is.character(steps)) steps <- triomics_as_characters(steps, "upstream_steps")
  region <- source$region
  list(
    feature_registry = feature_registry,
    assay = assay,
    mask = mask,
    availability = availability,
    sample_map = sample_map,
    column_accounting = column_accounting,
    upstream = steps,
    selection = data.frame(
      block_id = block$block_id,
      source_id = source$source_id,
      selection_history = source$selection_history,
      verified_universe_complete = FALSE,
      inference_gate = triomics_inference_gate(source$selection_history),
      stringsAsFactors = FALSE
    ),
    overlap = list(
      original_hash = source$original_hash,
      sheet = source$sheet,
      tolerance = source$tolerance,
      row_start = if (is.null(region)) 1L else region$row_start,
      col_start = if (is.null(region)) 1L else region$col_start,
      cells = triomics_character_matrix(table$rows, length(header))
    )
  )
}

triomics_bind_rows <- function(frames) {
  if (length(frames) == 0L) return(data.frame())
  do.call(rbind, c(frames, list(make.row.names = FALSE)))
}

triomics_ingest_study_tables <- function(study_id, sample_registry, blocks, sources) {
  if (!triomics_manifest_string(study_id)) {
    triomics_abort("A_ID_INVALID", "Study id must be one non-blank string.",
                   key = "study_id", module = "ingest", stage = "A",
                   remediation = "Pass the configured study id.")
  }
  if (!is.data.frame(sample_registry) || !"sample_id" %in% names(sample_registry)) {
    triomics_abort("A_SOURCE_CONTRACT", "Sample registry must include sample_id.",
                   key = "sample_registry", module = "ingest", stage = "A",
                   remediation = "Build the sample registry before ingesting assays.")
  }
  registry_ids <- triomics_preserve_identifier(sample_registry$sample_id)
  if (anyDuplicated(registry_ids)) {
    triomics_abort("A_ID_DUPLICATE_SAMPLE", "Sample identifiers must be unique.",
                   key = "sample_id", module = "ingest", stage = "A",
                   remediation = "Remove duplicated sample ids before ingest.")
  }
  if (!is.list(blocks) || !is.list(sources)) {
    triomics_abort("A_SOURCE_CONTRACT", "Blocks and sources must be lists.",
                   key = "blocks", module = "ingest", stage = "A",
                   remediation = "Pass one source record per measurement block.")
  }
  source_ids <- vapply(sources, function(source) source$source_id, character(1))
  prepared <- vector("list", length(blocks))
  block_ids <- character(length(blocks))
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    hit <- match(block$source_id, source_ids)
    if (is.na(hit)) {
      triomics_abort("A_ID_REFERENCE", "A block points at a source that was not supplied.",
                     key = "source_id", module = "ingest", stage = "A",
                     remediation = "Supply the measurement source named by the block.")
    }
    if (!is.character(block$missing_tokens)) {
      block$missing_tokens <- triomics_as_characters(block$missing_tokens, "missing_tokens")
    }
    prepared[[i]] <- triomics_ingest_block(study_id, registry_ids, block, sources[[hit]])
    block_ids[[i]] <- block$block_id
  }
  triomics_check_region_overlaps(lapply(prepared, function(item) item$overlap))
  names(prepared) <- block_ids
  n <- length(registry_ids)
  subject <- if ("subject_id" %in% names(sample_registry)) sample_registry$subject_id else rep(NA_character_, n)
  visit <- if ("visit_id" %in% names(sample_registry)) sample_registry$visit_id else rep(NA_character_, n)
  role <- if ("sample_role" %in% names(sample_registry)) as.character(sample_registry$sample_role) else rep("undeclared", n)
  result <- list(
    study_id = study_id,
    sample_registry = data.frame(
      sample_id = registry_ids,
      subject_id = subject,
      visit_id = visit,
      sample_role = role,
      stringsAsFactors = FALSE
    ),
    feature_registry = triomics_bind_rows(lapply(prepared, function(item) item$feature_registry)),
    sample_maps = lapply(prepared, function(item) item$sample_map),
    assays = lapply(prepared, function(item) item$assay),
    original_masks = lapply(prepared, function(item) item$mask),
    availability = triomics_bind_rows(lapply(prepared, function(item) item$availability)),
    source_manifests = unname(sources),
    selection = triomics_bind_rows(lapply(prepared, function(item) item$selection)),
    column_accounting = triomics_bind_rows(lapply(prepared, function(item) item$column_accounting)),
    clinical_observations = list(),
    upstream_steps = lapply(prepared, function(item) item$upstream)
  )
  triomics_validate_ingest_contract(result)
  result
}

triomics_metadata_column <- function(rows, header, name) {
  index <- match(name, header)
  if (is.na(index)) {
    triomics_abort("A_ID_REFERENCE", paste0("Metadata column '", name, "' is not in the metadata file."),
                   key = name, module = "ingest", stage = "A",
                   remediation = "Use a column that exists in the metadata table.")
  }
  triomics_column_tokens(rows, index)
}

triomics_read_sample_map <- function(path) {
  format <- if (grepl("[.]csv$", path, ignore.case = TRUE)) "csv" else "tsv"
  table <- triomics_read_delimited(path, format)
  if (!all(c("source_column", "sample_id") %in% table$header)) {
    triomics_abort("A_SOURCE_CONTRACT", "Sample map file must contain source_column and sample_id.",
                   key = "sample_map", module = "ingest", stage = "A",
                   remediation = "Use those two character columns.")
  }
  data.frame(
    source_column = triomics_preserve_identifier(triomics_metadata_column(table$rows, table$header, "source_column")),
    sample_id = triomics_preserve_identifier(triomics_metadata_column(table$rows, table$header, "sample_id")),
    stringsAsFactors = FALSE
  )
}

triomics_prepare_sample_metadata <- function(config, metadata_path) {
  source <- NULL
  for (item in config$sources) {
    if (identical(item$role, "sample_metadata")) source <- item
  }
  format <- if (is.null(source)) "tsv" else source$format
  table <- triomics_read_delimited(metadata_path, format)
  header <- table$header
  types <- config$metadata$column_types
  if (any(!header %in% names(types))) {
    missing <- header[!header %in% names(types)]
    triomics_abort("A_SOURCE_ROLE",
                   paste0("Metadata column '", missing[[1L]], "' has no declared role."),
                   key = missing[[1L]], module = "ingest", stage = "A",
                   remediation = "Declare every metadata column in column_types. Do not invent clinical fields.")
  }
  for (analysis in config$analyses) {
    if (!analysis$design$condition %in% header) {
      triomics_abort("A_ID_REFERENCE", "The design condition column is absent from metadata.",
                     key = "design.condition", module = "ingest", stage = "A",
                     remediation = "Include the declared condition column in the metadata file.")
    }
  }
  sample_ids <- triomics_preserve_identifier(triomics_metadata_column(table$rows, header, config$metadata$sample_id))
  if (anyDuplicated(sample_ids)) {
    triomics_abort("A_ID_DUPLICATE_SAMPLE", "Sample identifiers must be unique.",
                   key = "sample_id", module = "ingest", stage = "A",
                   remediation = "Remove duplicated sample ids. Do not split them into subject and visit.")
  }
  declared_levels <- lapply(config$metadata$factor_levels, function(levels) {
    triomics_as_characters(levels, "factor_levels")
  })
  columns <- vector("list", length(header))
  names(columns) <- header
  for (name in header) {
    raw <- triomics_metadata_column(table$rows, header, name)
    type <- types[[name]]
    if (identical(type, "factor")) {
      levels <- declared_levels[[name]]
      if (is.null(levels) || any(!raw %in% levels)) {
        triomics_abort("A_ID_REFERENCE",
                       paste0("Factor column '", name, "' contains a value outside its declared levels."),
                       key = name, module = "ingest", stage = "A",
                       remediation = "Keep the declared levels, including levels that are not observed.")
      }
      columns[[name]] <- raw
    } else if (identical(type, "string")) {
      columns[[name]] <- raw
    } else if (identical(type, "numeric")) {
      columns[[name]] <- triomics_parse_measurements(raw, c("NA", ""))
    } else if (identical(type, "logical")) {
      if (any(!raw %in% c("TRUE", "FALSE", "NA", ""))) {
        triomics_abort("A_ID_REFERENCE", paste0("Logical column '", name, "' contains an undeclared token."),
                       key = name, module = "ingest", stage = "A",
                       remediation = "Use TRUE, FALSE, or a declared missing token.")
      }
      columns[[name]] <- ifelse(raw %in% c("NA", ""), NA, raw == "TRUE")
    } else {
      triomics_abort("A_SOURCE_ROLE", paste0("Metadata column '", name, "' has an unknown type."),
                     key = name, module = "ingest", stage = "A",
                     remediation = "Use string, factor, numeric, or logical.")
    }
  }
  sample_metadata <- as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)
  subject_name <- config$metadata$subject_id
  subject <- if (is.null(subject_name)) {
    rep(NA_character_, length(sample_ids))
  } else {
    triomics_preserve_identifier(triomics_metadata_column(table$rows, header, subject_name), key = "subject_id")
  }
  visit_values <- lapply(config$analyses, function(analysis) analysis$design$visit_column)
  agreed <- length(visit_values) > 0L && all(vapply(visit_values, function(item) {
    identical(item, visit_values[[1L]])
  }, logical(1)))
  visit_name <- if (agreed) visit_values[[1L]] else NULL
  visit <- if (!is.character(visit_name) || length(visit_name) != 1L || !nzchar(visit_name)) {
    rep(NA_character_, length(sample_ids))
  } else {
    triomics_metadata_column(table$rows, header, visit_name)
  }
  role_name <- config$metadata$sample_role_column
  sample_role <- if (is.null(role_name)) {
    rep("undeclared", length(sample_ids))
  } else {
    triomics_metadata_column(table$rows, header, role_name)
  }
  list(
    sample_metadata = sample_metadata,
    declared_factor_levels = declared_levels,
    sample_registry = data.frame(
      sample_id = sample_ids,
      subject_id = subject,
      visit_id = visit,
      sample_role = sample_role,
      stringsAsFactors = FALSE
    )
  )
}

triomics_configured_blocks <- function(config, config_path, resolved_sources) {
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  source_ids <- vapply(config$sources, function(source) source$source_id, character(1))
  blocks <- vector("list", length(config$blocks))
  records <- list()
  for (i in seq_along(config$blocks)) {
    block <- config$blocks[[i]]
    block_path <- triomics_resolve_declared_input(block$file, sprintf("blocks[%d].file", i), root)
    same <- resolved_sources[vapply(resolved_sources, identical, logical(1), block_path)]
    measurement_id <- NULL
    for (source_id in names(same)) {
      source <- config$sources[[match(source_id, source_ids)]]
      if (identical(source$role, "measurement_matrix")) measurement_id <- source_id
    }
    source <- config$sources[[match(measurement_id, source_ids)]]
    conversion <- source$conversion_manifest
    if (!is.null(conversion)) {
      conversion <- triomics_resolve_declared_input(conversion, "conversion_manifest", root)
    }
    sample_map <- block$sample_map
    if (!is.null(sample_map)) {
      sample_map <- triomics_read_sample_map(triomics_resolve_declared_input(sample_map, "sample_map", root))
    }
    records[[length(records) + 1L]] <- triomics_source_record(
      source_id = source$source_id,
      study_id = config$study_id,
      path = same[[measurement_id]],
      format = source$format,
      role = source$role,
      selection_history = source$selection_history,
      selection_evidence = source$selection_evidence,
      sha256 = source$sha256,
      conversion_manifest = conversion
    )
    blocks[[i]] <- list(
      block_id = block$block_id,
      source_id = source$source_id,
      feature_id_column = block$feature_id_column,
      id_namespace = block$id_namespace,
      missing_tokens = triomics_as_characters(block$missing_tokens, "missing_tokens"),
      sample_map = sample_map,
      cell_mask = NULL,
      annotation_route = block$annotation_route,
      technical_replicate_spec_ref = block$technical_replicate_spec_ref,
      measurement = list(
        quantity = block$measurement$quantity,
        scale = block$measurement$scale,
        units = block$measurement$units,
        original_mask_status = block$measurement$original_mask_status,
        upstream_steps = triomics_as_characters(block$measurement$upstream_steps, "upstream_steps")
      )
    )
  }
  list(blocks = blocks, sources = records)
}

triomics_ingest_configured_study <- function(config, config_path) {
  references <- triomics_check_config_references(config, config_path)
  prepared <- triomics_prepare_sample_metadata(config, references$metadata)
  linked <- triomics_configured_blocks(config, config_path, references$sources)
  ingested <- triomics_ingest_study_tables(
    study_id = config$study_id,
    sample_registry = prepared$sample_registry,
    blocks = linked$blocks,
    sources = linked$sources
  )
  ingested$sample_metadata <- prepared$sample_metadata
  ingested$declared_factor_levels <- prepared$declared_factor_levels
  ingested$clinical_observations <- list()
  ingested
}

triomics_assay_payload_block <- function(block_id, assay) {
  values <- lapply(seq_len(nrow(assay)), function(i) {
    unname(lapply(seq_len(ncol(assay)), function(j) {
      value <- assay[i, j]
      if (is.na(value)) NULL else as.numeric(value)
    }))
  })
  list(
    block_id = block_id,
    feature_keys = rownames(assay),
    sample_ids = colnames(assay),
    values = values
  )
}

triomics_execute_stage_a <- function(config_path, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_abort("A_OUTPUT_INVALID", "Stage A output must be a directory path.",
                   key = "output", module = "ingest", stage = "A",
                   remediation = "Choose a new local output directory.")
  }
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  if (isTRUE(config$clinical$enabled)) {
    clinical_bundle <- triomics_ingest_clinical(config, config_path, ingested$sample_registry)
    triomics_write_clinical_artifacts(clinical_bundle, output)
  }
  payload <- list(
    schema_version = "2.0",
    study_id = config$study_id,
    blocks = unname(lapply(config$blocks, function(block) {
      triomics_assay_payload_block(block$block_id, ingested$assays[[block$block_id]])
    }))
  )
  payload_path <- file.path(output, "A", "assays.json")
  triomics_write_json_atomic(payload, payload_path)
  inputs <- unname(lapply(ingested$source_manifests, function(source) {
    list(artifact_id = source$source_id, sha256 = source$sha256)
  }))
  envelope <- triomics_artifact_envelope(
    artifact_type = "assays",
    artifact_id = "stage-a-assays",
    study_id = config$study_id,
    run_id = "a-t02",
    status = "complete",
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = character(),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  triomics_write_envelope(envelope, file.path(output, "A", "assays.envelope.json"))
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "a-t02",
    study_id = config$study_id,
    stage = "A",
    status = "complete"
  ), file.path(output, "registries", "run.json"))
  invisible(output)
}
