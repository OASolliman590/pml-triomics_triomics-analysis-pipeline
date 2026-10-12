triomics_require_bioc <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    triomics_abort("A_SOURCE_CONTRACT", paste0("Package '", package, "' is required for study assembly."),
                   key = package, module = "study", stage = "A",
                   remediation = "Install the Bioconductor package named by the study contract.")
  }
  invisible(TRUE)
}

triomics_validate_se_dimensions <- function(se) {
  triomics_require_bioc("SummarizedExperiment")
  if (!methods::is(se, "SummarizedExperiment")) {
    triomics_abort("A_SOURCE_CONTRACT", "A block experiment must be a SummarizedExperiment.",
                   key = "experiments", module = "study", stage = "A",
                   remediation = "Assemble each block so assay, rowData, and colData share one shape.")
  }
  assay_names <- SummarizedExperiment::assayNames(se)
  if (!all(c("analysis", "original_mask") %in% assay_names)) {
    triomics_abort("A_SOURCE_CONTRACT", "Each block SE needs analysis and original_mask assays.",
                   key = "assays", module = "study", stage = "A",
                   remediation = "Keep the mask beside the analysis assay at the same dimensions.")
  }
  collected <- SummarizedExperiment::assays(se)
  dims <- lapply(collected, dim)
  dimnames_ok <- lapply(collected, dimnames)
  if (length(dims) == 0L || !all(vapply(dims, function(dim) identical(dim, dims[[1L]]), logical(1L))) ||
      !all(vapply(dimnames_ok, function(names) identical(names, dimnames_ok[[1L]]), logical(1L)))) {
    triomics_abort("A_SOURCE_CONTRACT", "SE assays must share dimensions and dimnames.",
                   key = "assays", module = "study", stage = "A",
                   remediation = "Store a shape-changing matrix as a checkpoint, not as another assay.")
  }
  if (!identical(rownames(se), rownames(collected[[1L]])) || !identical(colnames(se), colnames(collected[[1L]]))) {
    triomics_abort("A_SOURCE_CONTRACT", "SE dimnames drifted from the assays.",
                   key = "dimnames", module = "study", stage = "A",
                   remediation = "Keep assay dimnames identical to the SE dimnames.")
  }
  row_data <- SummarizedExperiment::rowData(se)
  col_data <- SummarizedExperiment::colData(se)
  if (!identical(nrow(row_data), nrow(collected[[1L]])) || !identical(rownames(row_data), rownames(se)) ||
      !identical(nrow(col_data), ncol(collected[[1L]])) || !identical(rownames(col_data), colnames(se))) {
    triomics_abort("A_SOURCE_CONTRACT", "rowData or colData does not match the assay shape.",
                   key = "rowData", module = "study", stage = "A",
                   remediation = "Validate dimensions again after every shape change.")
  }
  if ("feature_key" %in% names(row_data) && !identical(as.character(row_data$feature_key), rownames(se))) {
    triomics_abort("A_ID_INVALID", "rowData feature keys are not the assay rownames.",
                   key = "feature_key", module = "study", stage = "A",
                   remediation = "Join feature metadata by feature key, not by row position.")
  }
  if ("sample_id" %in% names(col_data) && !identical(as.character(col_data$sample_id), colnames(se))) {
    triomics_abort("A_ID_INVALID", "colData sample ids are not the assay colnames.",
                   key = "sample_id", module = "study", stage = "A",
                   remediation = "Join sample metadata by sample id, not by column position.")
  }
  invisible(TRUE)
}

triomics_copy_named_matrix <- function(matrix, columns, new_colnames) {
  copied <- matrix(matrix[, columns, drop = FALSE], nrow = nrow(matrix),
                   dimnames = list(rownames(matrix), new_colnames))
  if (is.character(matrix)) mode(copied) <- "character"
  copied
}

triomics_alignment_records <- function(block_ids, registry_ids, kept, alignment) {
  rows <- list()
  for (block_id in block_ids) {
    for (sample_id in registry_ids) {
      status <- if (sample_id %in% kept[[block_id]]) {
        "kept"
      } else if (identical(alignment, "intersection")) {
        "removed"
      } else {
        "absent"
      }
      rows[[length(rows) + 1L]] <- data.frame(
        block_id = block_id,
        sample_id = sample_id,
        status = status,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

triomics_align_block <- function(study, block_id, sample_ids) {
  assay <- study$assays[[block_id]]
  mask <- study$original_masks[[block_id]]
  map <- study$sample_maps[[block_id]]
  if (!is.matrix(assay) || !is.numeric(assay) || !is.matrix(mask)) {
    triomics_abort("A_SOURCE_CONTRACT", "Each block needs a numeric assay and a mask matrix.",
                   key = "assays", module = "study", stage = "A",
                   remediation = "Assemble from the ingested matrices. Do not merge blocks by position.")
  }
  if (!identical(dim(assay), dim(mask)) || !identical(colnames(assay), colnames(mask)) ||
      !identical(rownames(assay), rownames(mask))) {
    triomics_abort("A_SOURCE_MASK", "The original mask does not match the assay dimensions.",
                   key = "original_mask", module = "study", stage = "A",
                   remediation = "Keep mask dimnames identical to the assay before alignment.")
  }
  if (anyDuplicated(colnames(assay)) || anyDuplicated(rownames(assay)) || anyDuplicated(sample_ids)) {
    triomics_abort("A_ID_DUPLICATE_SAMPLE", "Assay columns and sample ids must be unique.",
                   key = "sample_id", module = "study", stage = "A",
                   remediation = "Do not average or positionally stack duplicate columns.")
  }
  if (!is.data.frame(map) || !all(c("assay_column", "sample_id") %in% names(map))) {
    triomics_abort("A_SAMPLE_MAP", "A sample map needs assay_column and sample_id.",
                   key = "sample_map", module = "study", stage = "A",
                   remediation = "Map each assay column onto one sample id.")
  }
  if (anyDuplicated(map$sample_id) || anyDuplicated(map$assay_column)) {
    triomics_abort("A_SAMPLE_MAP", "Sample map entries must be one-to-one.",
                   key = "sample_map", module = "study", stage = "A",
                   remediation = "Do not map two columns onto one sample in this contract.")
  }
  if (!setequal(as.character(map$assay_column), colnames(assay)) || nrow(map) != ncol(assay)) {
    triomics_abort("A_SAMPLE_MAP", paste0("Block '", block_id, "' sample map does not cover the assay columns."),
                   key = "sample_map", module = "study", stage = "A",
                   remediation = "Refuse unmapped columns instead of letting a constructor drop them.")
  }
  if (any(!sample_ids %in% map$sample_id)) {
    triomics_abort("A_ID_UNKNOWN_SAMPLE", "A kept sample is not in the block sample map.",
                   key = "sample_id", module = "study", stage = "A",
                   remediation = "Choose samples from the explicit map.")
  }
  columns <- character(length(sample_ids))
  source_columns <- character(length(sample_ids))
  for (i in seq_along(sample_ids)) {
    hit <- match(sample_ids[[i]], map$sample_id)
    columns[[i]] <- as.character(map$assay_column[[hit]])
    source_columns[[i]] <- if ("source_column" %in% names(map)) {
      as.character(map$source_column[[hit]])
    } else {
      columns[[i]]
    }
  }
  aligned_assay <- triomics_copy_named_matrix(assay, columns, sample_ids)
  aligned_mask <- triomics_copy_named_matrix(mask, columns, sample_ids)
  allowed <- c("known_observed", "known_missing", "known_imputed", "unknown")
  if (any(!as.vector(aligned_mask) %in% allowed)) {
    triomics_abort("A_SOURCE_MASK", "Original mask codes must stay the four observation statuses.",
                   key = "original_mask", module = "study", stage = "A",
                   remediation = "Use known_observed, known_missing, known_imputed, or unknown.")
  }
  values <- as.vector(aligned_assay)
  codes <- as.vector(aligned_mask)
  if (any(codes == "known_missing" & !is.na(values)) ||
      any(codes %in% c("known_observed", "known_imputed") & is.na(values))) {
    triomics_abort("A_SOURCE_MASK", "Mask codes do not match the observed cells.",
                   key = "original_mask", module = "study", stage = "A",
                   remediation = "Do not attach a mask to a sample by column position.")
  }
  list(
    assay = aligned_assay,
    mask = aligned_mask,
    sample_ids = sample_ids,
    source_columns = source_columns
  )
}

triomics_se_from_aligned <- function(study, block_id, aligned, display_name) {
  triomics_require_bioc("SummarizedExperiment")
  keys <- rownames(aligned$assay)
  registry <- study$feature_registry
  if (!is.data.frame(registry) || !all(c("feature_key", "block_id", "original_id", "scale", "quantity") %in% names(registry))) {
    triomics_abort("A_SOURCE_CONTRACT", "Feature registry is missing identity or scale columns.",
                   key = "feature_registry", module = "study", stage = "A",
                   remediation = "Carry feature_key, block_id, original_id, quantity, and scale.")
  }
  rows <- registry[registry$block_id == block_id, , drop = FALSE]
  index <- match(keys, rows$feature_key)
  if (anyNA(index) || anyDuplicated(keys)) {
    triomics_abort("A_ID_DUPLICATE_FEATURE", "Assay rows must match unique feature keys in the block.",
                   key = "feature_key", module = "study", stage = "A",
                   remediation = "Do not merge identical molecular labels across blocks or rows.")
  }
  rows <- rows[index, , drop = FALSE]
  scales <- unique(as.character(rows$scale))
  quantities <- unique(as.character(rows$quantity))
  if (length(scales) != 1L || length(quantities) != 1L) {
    triomics_abort("A_SCALE_INPUT", "A block cannot mix quantity or scale.",
                   key = "scale", module = "study", stage = "A",
                   remediation = "Split mixed scales before assembly.")
  }
  row_frame <- data.frame(
    feature_key = as.character(rows$feature_key),
    original_id = as.character(rows$original_id),
    id_namespace = as.character(rows$id_namespace),
    block_id = as.character(rows$block_id),
    source_row = as.character(rows$source_row),
    measured_or_derived = as.character(rows$measured_or_derived),
    quantity = quantities,
    scale = scales,
    units = as.character(rows$units),
    stringsAsFactors = FALSE
  )
  rownames(row_frame) <- keys
  samples <- study$sample_registry
  sample_index <- match(aligned$sample_ids, samples$sample_id)
  if (anyNA(sample_index)) {
    triomics_abort("A_ID_UNKNOWN_SAMPLE", "An aligned sample is not in the sample registry.",
                   key = "sample_id", module = "study", stage = "A",
                   remediation = "Do not invent samples during assembly.")
  }
  picked <- samples[sample_index, , drop = FALSE]
  picked$sample_id <- aligned$sample_ids
  picked$source_column <- aligned$source_columns
  rownames(picked) <- aligned$sample_ids
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(analysis = aligned$assay, original_mask = aligned$mask),
    rowData = S4Vectors::DataFrame(row_frame),
    colData = S4Vectors::DataFrame(picked),
    metadata = list(triomics = list(
      block_id = block_id,
      display_name = display_name,
      quantity = quantities[[1L]],
      scale = scales[[1L]],
      completed = list(),
      state_payload = NULL,
      state_ref = NULL
    ))
  )
  triomics_validate_se_dimensions(se)
  se
}

triomics_bind_mae <- function(experiments, col_data, sample_map) {
  triomics_require_bioc("MultiAssayExperiment")
  if (!is.list(experiments) || is.null(names(experiments)) || anyDuplicated(names(experiments)) ||
      any(!nzchar(names(experiments)))) {
    triomics_abort("A_SOURCE_CONTRACT", "Experiments must be a named block list.",
                   key = "experiments", module = "study", stage = "A",
                   remediation = "Name experiments by block id, not by display name.")
  }
  map <- as.data.frame(sample_map, stringsAsFactors = FALSE)
  if (!all(c("assay", "primary", "colname") %in% names(map))) {
    triomics_abort("A_SAMPLE_MAP", "MAE sample map needs assay, primary, and colname.",
                   key = "sample_map", module = "study", stage = "A",
                   remediation = "Build the sample map explicitly before calling the constructor.")
  }
  map$assay <- as.character(map$assay)
  map$primary <- as.character(map$primary)
  map$colname <- as.character(map$colname)
  for (block_id in names(experiments)) {
    triomics_validate_se_dimensions(experiments[[block_id]])
    wanted <- map$colname[map$assay == block_id]
    if (length(wanted) != ncol(experiments[[block_id]]) || !setequal(wanted, colnames(experiments[[block_id]]))) {
      triomics_abort("A_SAMPLE_MAP", paste0("Sample map for '", block_id, "' would drop or invent assay columns."),
                     key = "sample_map", module = "study", stage = "A",
                     remediation = "Refuse a positional or partial map before the constructor runs.")
    }
  }
  col_frame <- as.data.frame(col_data, stringsAsFactors = FALSE)
  if (is.null(rownames(col_frame)) || anyDuplicated(rownames(col_frame))) {
    triomics_abort("A_SAMPLE_MAP", "Canonical colData rows must be unique sample ids.",
                   key = "colData", module = "study", stage = "A",
                   remediation = "Key colData by sample id.")
  }
  if (length(setdiff(unique(map$primary), rownames(col_frame)))) {
    triomics_abort("A_SAMPLE_MAP", "Sample map primary ids are missing from colData.",
                   key = "primary", module = "study", stage = "A",
                   remediation = "Do not point the sample map at an unknown sample.")
  }
  intended_rows <- rownames(col_frame)
  intended_cols <- lapply(experiments, colnames)
  experiment_list <- do.call(MultiAssayExperiment::ExperimentList, experiments)
  mae <- suppressMessages(suppressWarnings(MultiAssayExperiment::MultiAssayExperiment(
    experiments = experiment_list,
    colData = S4Vectors::DataFrame(col_frame, row.names = intended_rows),
    sampleMap = S4Vectors::DataFrame(
      assay = factor(map$assay, levels = unique(map$assay)),
      primary = map$primary,
      colname = map$colname
    )
  )))
  if (!identical(rownames(SummarizedExperiment::colData(mae)), intended_rows)) {
    triomics_abort("A_SAMPLE_MAP_DROP", "MultiAssayExperiment dropped colData rows.",
                   key = "colData", module = "study", stage = "A",
                   remediation = "Do not accept a constructor intersection or harmonized drop.")
  }
  got <- MultiAssayExperiment::experiments(mae)
  for (block_id in names(experiments)) {
    if (!identical(colnames(got[[block_id]]), intended_cols[[block_id]])) {
      triomics_abort("A_SAMPLE_MAP_DROP", "MultiAssayExperiment dropped assay columns.",
                     key = "sample_map", module = "study", stage = "A",
                     remediation = "Compare maps before and after construction and refuse dropped entries.")
    }
  }
  if (nrow(MultiAssayExperiment::sampleMap(mae)) != nrow(map)) {
    triomics_abort("A_SAMPLE_MAP_DROP", "MultiAssayExperiment dropped sample map rows.",
                   key = "sample_map", module = "study", stage = "A",
                   remediation = "Keep every explicit assay/primary/colname triple.")
  }
  mae
}

triomics_mae_from_experiments <- function(study) {
  block_ids <- names(study$experiments)
  map_rows <- lapply(block_ids, function(block_id) {
    cols <- colnames(study$experiments[[block_id]])
    data.frame(assay = block_id, primary = cols, colname = cols, stringsAsFactors = FALSE)
  })
  map <- do.call(rbind, map_rows)
  present <- study$sample_registry$sample_id[study$sample_registry$sample_id %in% map$primary]
  col <- study$sample_registry[match(present, study$sample_registry$sample_id), , drop = FALSE]
  rownames(col) <- present
  triomics_bind_mae(study$experiments, col, map)
}

triomics_assemble_study <- function(study, alignment = "strict", display_names = NULL) {
  if (!is.list(study) || is.null(names(study$assays)) || !length(study$assays)) {
    triomics_abort("A_SOURCE_CONTRACT", "Study assembly needs named block assays.",
                   key = "assays", module = "study", stage = "A",
                   remediation = "Ingest blocks before assembling them.")
  }
  if (!is.character(alignment) || length(alignment) != 1L ||
      !alignment %in% c("strict", "intersection", "union")) {
    triomics_abort("A_SAMPLE_ALIGNMENT", "Alignment must be strict, intersection, or union.",
                   key = "alignment", module = "study", stage = "A",
                   remediation = "Do not silently intersect blocks. Name the alignment policy.")
  }
  block_ids <- names(study$assays)
  if (anyDuplicated(block_ids) || any(!nzchar(block_ids))) {
    triomics_abort("A_ID_INVALID", "Block ids must be unique and non-blank.",
                   key = "block_id", module = "study", stage = "A",
                   remediation = "Keep the block id stable when the display name changes.")
  }
  registry_ids <- triomics_preserve_identifier(study$sample_registry$sample_id)
  if (anyDuplicated(registry_ids)) {
    triomics_abort("A_ID_DUPLICATE_SAMPLE", "Sample identifiers must be unique.",
                   key = "sample_id", module = "study", stage = "A",
                   remediation = "Do not collapse visits that share a subject.")
  }
  present <- lapply(block_ids, function(block_id) {
    map <- study$sample_maps[[block_id]]
    if (!is.data.frame(map) || !"sample_id" %in% names(map)) {
      triomics_abort("A_SAMPLE_MAP", paste0("Block '", block_id, "' is missing a sample map."),
                     key = "sample_map", module = "study", stage = "A",
                     remediation = "Supply the explicit sample map from ingest.")
    }
    as.character(map$sample_id)
  })
  names(present) <- block_ids
  if (identical(alignment, "strict")) {
    for (block_id in block_ids) {
      missing <- setdiff(registry_ids, present[[block_id]])
      extra <- setdiff(present[[block_id]], registry_ids)
      if (length(missing) || length(extra)) {
        triomics_abort("A_SAMPLE_ALIGNMENT",
                       paste0("Block '", block_id, "' does not match the study samples: ",
                              paste(c(missing, extra), collapse = ", "), "."),
                       key = "sample_map", module = "study", stage = "A",
                       remediation = "Use an explicit intersection or union. Do not drop samples silently.")
      }
    }
    kept <- lapply(block_ids, function(block_id) registry_ids)
  } else if (identical(alignment, "intersection")) {
    common <- registry_ids
    for (block_id in block_ids) common <- intersect(common, present[[block_id]])
    common <- registry_ids[registry_ids %in% common]
    kept <- lapply(block_ids, function(block_id) common)
  } else {
    kept <- lapply(block_ids, function(block_id) registry_ids[registry_ids %in% present[[block_id]]])
  }
  names(kept) <- block_ids
  experiments <- list()
  assays <- list()
  masks <- list()
  maps <- list()
  for (block_id in block_ids) {
    aligned <- triomics_align_block(study, block_id, kept[[block_id]])
    display <- block_id
    if (!is.null(display_names) && !is.null(display_names[[block_id]])) {
      display <- as.character(display_names[[block_id]])
    }
    experiments[[block_id]] <- triomics_se_from_aligned(study, block_id, aligned, display)
    assays[[block_id]] <- aligned$assay
    masks[[block_id]] <- aligned$mask
    maps[[block_id]] <- data.frame(
      block_id = block_id,
      assay_column = kept[[block_id]],
      sample_id = kept[[block_id]],
      source_column = aligned$source_columns,
      stringsAsFactors = FALSE
    )
  }
  assembled <- study
  assembled$sample_registry$sample_id <- registry_ids
  assembled$assays <- assays
  assembled$original_masks <- masks
  assembled$sample_maps <- maps
  assembled$experiments <- experiments
  assembled$mae <- triomics_mae_from_experiments(assembled)
  assembled$alignment <- list(
    policy = alignment,
    records = triomics_alignment_records(block_ids, registry_ids, kept, alignment),
    unmapped_sample_ids = registry_ids[!registry_ids %in% unique(unlist(present, use.names = FALSE))]
  )
  assembled$checkpoints <- list()
  assembled$state_refs <- list()
  assembled$display_names <- stats::setNames(
    vapply(block_ids, function(block_id) S4Vectors::metadata(experiments[[block_id]])$triomics$display_name, character(1)),
    block_ids
  )
  assembled$feature_flow <- data.frame()
  assembled$operation_ledger <- list()
  assembled
}
