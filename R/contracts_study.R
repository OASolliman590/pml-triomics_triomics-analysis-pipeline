triomics_validate_ingest_contract <- function(study) {
  clinical <- study$clinical_observations
  if (is.null(clinical)) {
    triomics_abort("A_SOURCE_CONTRACT", "Ingest output is missing clinical_observations.",
                   key = "clinical_observations", module = "contracts", stage = "A",
                   remediation = "Return an empty clinical observation list. Do not invent observations.")
  }
  if (!is.list(clinical) || is.data.frame(clinical) || length(clinical) > 0L ||
      (!is.null(names(clinical)) && any(nzchar(names(clinical))))) {
    triomics_abort("A_CLINICAL_FABRICATED",
                   "Clinical observations were not supplied by a source and cannot be fabricated.",
                   key = "clinical_observations", module = "contracts", stage = "A",
                   remediation = "Leave clinical observations empty until a clinical source is linked.")
  }
  required <- c(
    "study_id", "sample_registry", "feature_registry", "sample_maps", "assays",
    "original_masks", "availability", "source_manifests", "selection",
    "column_accounting", "clinical_observations", "upstream_steps"
  )
  if (!is.list(study) || !all(required %in% names(study))) {
    triomics_abort("A_SOURCE_CONTRACT", "Ingest output is missing a required registry.",
                   key = "study", module = "contracts", stage = "A",
                   remediation = "Return sample, feature, mask, availability, and source registries together.")
  }
  if (!is.data.frame(study$sample_registry) || anyDuplicated(study$sample_registry$sample_id)) {
    triomics_abort("A_ID_DUPLICATE_SAMPLE", "Sample identifiers must be unique.",
                   key = "sample_id", module = "contracts", stage = "A",
                   remediation = "Keep one registry row per sample.")
  }
  registry <- study$feature_registry
  if (!is.data.frame(registry) || !all(c("feature_key", "block_id", "source_row") %in% names(registry))) {
    triomics_abort("A_SOURCE_CONTRACT", "Feature registry is incomplete.",
                   key = "feature_registry", module = "contracts", stage = "A",
                   remediation = "Record feature_key, block_id, and source_row for every measured row.")
  }
  if (nrow(registry) > 0L) {
    expected <- vapply(seq_len(nrow(registry)), function(i) {
      triomics_feature_key(study$study_id, registry$block_id[[i]], registry$source_row[[i]])
    }, character(1))
    if (anyDuplicated(registry$feature_key)) {
      triomics_abort("A_ID_DUPLICATE_FEATURE", "Feature keys must be unique.",
                     key = "feature_key", module = "contracts", stage = "A",
                     remediation = "Key features by study, block, and source row.")
    }
    if (!identical(registry$feature_key, expected)) {
      triomics_abort("A_ID_INVALID", "Feature keys must be study, block, and source-row identities.",
                     key = "feature_key", module = "contracts", stage = "A",
                     remediation = "Do not use molecular labels as feature keys.")
    }
  }
  allowed_mask <- c("known_observed", "known_missing", "known_imputed", "unknown")
  for (block_id in names(study$assays)) {
    assay <- study$assays[[block_id]]
    mask <- study$original_masks[[block_id]]
    if (!is.matrix(assay) || !is.matrix(mask) || !identical(dim(mask), dim(assay)) ||
        !identical(dimnames(mask), dimnames(assay)) || any(!as.vector(mask) %in% allowed_mask)) {
      triomics_abort("A_SOURCE_MASK", "Original mask codes must stay the four observation statuses.",
                     key = "original_mask", module = "contracts", stage = "A",
                     remediation = "Use known_observed, known_missing, known_imputed, or unknown.")
    }
    codes <- as.vector(mask)
    values <- as.vector(assay)
    if (any(codes == "known_missing" & !is.na(values)) ||
        any(codes %in% c("known_observed", "known_imputed") & is.na(values))) {
      triomics_abort("A_SOURCE_MASK", "Mask codes do not match the observed cells.",
                     key = "original_mask", module = "contracts", stage = "A",
                     remediation = "Use known_missing only for NA cells.")
    }
    mapped <- study$sample_maps[[block_id]]
    if (!is.data.frame(mapped) || !identical(mapped$sample_id, colnames(assay))) {
      triomics_abort("A_SOURCE_CONTRACT", "Sample map order must match the assay columns.",
                     key = "sample_map", module = "contracts", stage = "A",
                     remediation = "Order the sample map by the assay columns.")
    }
  }
  if (!is.data.frame(study$availability) ||
      !all(c("block_id", "sample_id", "presence", "cell_imputation_eligible") %in% names(study$availability))) {
    triomics_abort("A_SOURCE_CONTRACT", "Availability registry is incomplete.",
                   key = "availability", module = "contracts", stage = "A",
                   remediation = "Record presence separately from cell masks.")
  }
  absence <- study$availability$presence == "structural_absence"
  if (any(study$availability$cell_imputation_eligible[absence] %in% TRUE)) {
    triomics_abort("A_SOURCE_ABSENCE", "A structurally absent assay is not eligible for cell imputation.",
                   key = "availability", module = "contracts", stage = "A",
                   remediation = "Keep whole-assay absence out of the assay and out of imputation.")
  }
  absent_blocks <- unique(study$availability$block_id[absence])
  for (block_id in absent_blocks) {
    absent_ids <- study$availability$sample_id[absence & study$availability$block_id == block_id]
    if (any(absent_ids %in% colnames(study$assays[[block_id]]))) {
      triomics_abort("A_SOURCE_ABSENCE", "A structurally absent sample was copied into the assay.",
                     key = "availability", module = "contracts", stage = "A",
                     remediation = "Omit structurally absent samples instead of filling them with NA.")
    }
  }
  if (!is.data.frame(study$selection) || any(study$selection$verified_universe_complete %in% TRUE)) {
    triomics_abort("A_SOURCE_SELECTION", "A source cannot claim a verified complete feature universe.",
                   key = "selection", module = "contracts", stage = "A",
                   remediation = "Leave verified_universe_complete false and gate inference from selection history.")
  }
  account <- study$column_accounting
  if (!is.data.frame(account) || !all(c("column", "role", "included_in_assay") %in% names(account))) {
    triomics_abort("A_SOURCE_CONTRACT", "Column accounting is incomplete.",
                   key = "column_accounting", module = "contracts", stage = "A",
                   remediation = "Record every source column role and whether it entered the assay.")
  }
  included <- account$included_in_assay %in% TRUE
  if (any(included & triomics_is_summary_column(account$column))) {
    triomics_abort("A_SOURCE_SUMMARY_AS_MEASUREMENT", "A summary column was included as a measurement.",
                   key = "column_accounting", module = "contracts", stage = "A",
                   remediation = "Keep summary and statistic columns out of the assay.")
  }
  if (any(included & account$role != "measurement")) {
    triomics_abort("A_SOURCE_ROLE", "Only measurement columns can enter the assay.",
                   key = "column_accounting", module = "contracts", stage = "A",
                   remediation = "Exclude metadata, summary, QC, and statistic columns.")
  }
  invisible(TRUE)
}

triomics_check_config_references <- function(config, config_path) {
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  if (!triomics_is_yaml_sequence(config$sources) || !triomics_is_yaml_sequence(config$blocks) ||
      !triomics_is_yaml_sequence(config$analyses)) {
    triomics_abort("A_SOURCE_CONTRACT", "Sources, blocks, and analyses must be sequences.",
                   key = "config", module = "ingest", stage = "A",
                   remediation = "Use YAML lists for sources, blocks, and analyses.")
  }
  source_ids <- vapply(config$sources, function(source) source$source_id, character(1))
  block_ids <- vapply(config$blocks, function(block) block$block_id, character(1))
  analysis_ids <- vapply(config$analyses, function(analysis) analysis$analysis_id, character(1))
  if (anyDuplicated(source_ids) || anyDuplicated(block_ids) || anyDuplicated(analysis_ids)) {
    triomics_abort("A_SOURCE_CONTRACT", "Source, block, and analysis identifiers must be unique.",
                   key = "source_id", module = "ingest", stage = "A",
                   remediation = "Remove duplicated identifiers.")
  }
  resolved <- lapply(seq_along(config$sources), function(i) {
    triomics_resolve_declared_input(config$sources[[i]]$file, sprintf("sources[%d].file", i), root)
  })
  names(resolved) <- source_ids
  for (block in config$blocks) {
    refs <- triomics_as_characters(block$source_refs, "source_refs")
    if (any(!refs %in% source_ids)) {
      triomics_abort("A_ID_REFERENCE", "A block source reference does not exist.",
                     key = "source_refs", module = "ingest", stage = "A",
                     remediation = "Point source_refs at declared source ids.")
    }
  }
  type_names <- names(config$metadata$column_types)
  for (analysis in config$analyses) {
    ids <- triomics_as_characters(analysis$block_ids, "block_ids")
    if (any(!ids %in% block_ids)) {
      triomics_abort("A_ID_REFERENCE", "An analysis names a block that does not exist.",
                     key = "block_ids", module = "ingest", stage = "A",
                     remediation = "List only declared block ids.")
    }
    condition <- analysis$design$condition
    if (!is.character(condition) || length(condition) != 1L || !condition %in% type_names) {
      triomics_abort("A_ID_REFERENCE", "The design condition is not a declared metadata column.",
                     key = "design.condition", module = "ingest", stage = "A",
                     remediation = "Declare the condition column and its type.")
    }
  }
  for (i in seq_along(config$blocks)) {
    block <- config$blocks[[i]]
    block_path <- triomics_resolve_declared_input(block$file, sprintf("blocks[%d].file", i), root)
    same <- resolved[vapply(resolved, identical, logical(1), block_path)]
    matched <- config$sources[match(names(same), source_ids)]
    roles <- if (length(matched) == 0L) character() else vapply(matched, function(source) source$role, character(1))
    measurement <- names(same)[roles == "measurement_matrix"]
    if (any(roles == "imported_result") && length(measurement) != 1L) {
      triomics_abort("A_SOURCE_CONTRACT", "An imported result cannot be used as an assay.",
                     key = "role", module = "ingest", stage = "A",
                     remediation = "Point the block at a measurement matrix.")
    }
    if (length(measurement) != 1L || !measurement[[1L]] %in% triomics_as_characters(block$source_refs, "source_refs")) {
      triomics_abort("A_ID_REFERENCE", "Block file must match exactly one referenced measurement matrix.",
                     key = "file", module = "ingest", stage = "A",
                     remediation = "Give the block and one referenced measurement source the same file path.")
    }
  }
  meta_path <- triomics_resolve_declared_input(config$metadata$file, "metadata.file", root)
  invisible(list(root = root, sources = resolved, metadata = meta_path))
}
