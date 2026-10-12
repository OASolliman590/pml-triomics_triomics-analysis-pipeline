triomics_b_abort <- function(code, message, key = NULL,
                               remediation = "Review the stage B design before fitting.") {
  triomics_abort(code, message, key = key, module = "statistics_b", stage = "B", remediation = remediation)
}

triomics_b_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

triomics_b_empty <- function(value) {
  is.null(value) || length(value) == 0L
}

triomics_b_characters <- function(value, key) {
  if (is.character(value)) return(value)
  triomics_as_characters(value, key)
}

triomics_b_hash <- function(value) {
  triomics_hash_text(triomics_canonical_json(value))
}

triomics_b_assay_hash <- function(block_id, assay) {
  features <- sort(rownames(assay))
  samples <- sort(colnames(assay))
  records <- vector("list", length(features) * length(samples))
  index <- 0L
  for (feature in features) {
    for (sample_id in samples) {
      index <- index + 1L
      value <- assay[feature, sample_id]
      records[[index]] <- list(
        feature_key = feature,
        sample_id = sample_id,
        value = if (length(value) != 1L || !is.finite(value)) NULL else as.numeric(value)
      )
    }
  }
  triomics_b_hash(list(block_id = block_id, records = records))
}

triomics_b_scale <- function(registry, block_id, feature_key) {
  if (is.null(registry) || !all(c("block_id", "feature_key", "scale") %in% names(registry))) {
    return("")
  }
  hit <- which(as.character(registry$block_id) == block_id &
                 as.character(registry$feature_key) == feature_key)
  if (length(hit) != 1L) return("")
  as.character(registry$scale[[hit[[1L]]]])
}

triomics_resolve_analysis <- function(prepared_study, analysis_spec) {
  design <- analysis_spec$design
  if (is.null(design) ||
      !identical(design$kind, "independent") ||
      !identical(design$representation, "levels") ||
      !identical(design$condition_type, "factor") ||
      !identical(design$repeated, FALSE)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Only an independent two-level factor on the levels representation is supported.",
      key = "design"
    )
  }
  if (!triomics_b_empty(design$covariates)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Declared covariates are kept out of this two-group lane and are not dropped.",
      key = "design.covariates"
    )
  }
  condition <- design$condition
  metadata <- prepared_study$sample_metadata
  if (!triomics_b_string(condition) || is.null(metadata) || !condition %in% names(metadata)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The condition column is not in the sample metadata.",
                     key = "design.condition")
  }
  declared <- prepared_study$declared_factor_levels[[condition]]
  if (is.null(declared)) {
    triomics_b_abort("B_DESIGN_LEVELS", "The condition has no declared factor levels.", key = "factor_levels")
  }
  levels <- triomics_b_characters(declared, "factor_levels")
  reference <- design$reference_level
  target <- design$target_level
  if (length(levels) != 2L || anyDuplicated(levels) ||
      !triomics_b_string(reference) || !triomics_b_string(target) ||
      identical(reference, target) || !setequal(levels, c(reference, target))) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "An independent two-group contrast needs exactly the declared reference and target levels.",
      key = "factor_levels"
    )
  }
  block_ids <- triomics_b_characters(analysis_spec$block_ids, "block_ids")
  assays <- prepared_study$assays
  if (length(block_ids) == 0L || is.null(assays) || any(!block_ids %in% names(assays))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Every analysis block must already be in the prepared study.",
                     key = "block_ids")
  }
  sample_ids <- as.character(metadata$sample_id)
  if (anyNA(sample_ids) || any(!nzchar(sample_ids)) || anyDuplicated(sample_ids)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "Sample identifiers must be unique before design resolution.",
                     key = "sample_id")
  }
  groups <- as.character(metadata[[condition]])
  names(groups) <- sample_ids
  column_ids <- paste0("condition.", levels)
  registry <- prepared_study$feature_registry
  hashes <- prepared_study$assay_hashes

  blocks <- vector("list", length(block_ids))
  names(blocks) <- block_ids
  for (block_id in block_ids) {
    assay <- assays[[block_id]]
    if (!is.matrix(assay) || is.null(colnames(assay)) || is.null(rownames(assay)) ||
        anyDuplicated(colnames(assay)) || anyDuplicated(rownames(assay))) {
      triomics_b_abort("B_DESIGN_UNIT_MAP", "Each assay needs unique sample and feature identifiers.",
                       key = "assays")
    }
    if (length(setdiff(colnames(assay), sample_ids)) > 0L) {
      triomics_b_abort("B_DESIGN_UNIT_MAP", "An assay column is outside the sample metadata.",
                       key = "sample_id")
    }
    present <- colnames(assay)
    present_groups <- unname(groups[present])
    if (any(!present_groups %in% levels)) {
      triomics_b_abort("B_DESIGN_LEVELS", "A sample condition is outside the declared two levels.",
                       key = "design.condition")
    }
    actual_hash <- triomics_b_assay_hash(block_id, assay)
    declared_hash <- if (is.null(hashes)) NULL else hashes[[block_id]]
    if (!is.null(declared_hash) && !identical(as.character(declared_hash), actual_hash)) {
      triomics_b_abort("B_DESIGN_HASH", "The declared assay hash does not match the measurement matrix.",
                       key = "assay_hashes")
    }
    feature_keys <- rownames(assay)
    scales <- vapply(feature_keys, triomics_b_scale, character(1), registry = registry, block_id = block_id)
    blocks[[block_id]] <- list(
      block_id = block_id,
      assay = assay,
      sample_ids = present,
      groups = present_groups,
      feature_keys = feature_keys,
      scales = unname(scales),
      source_hash = actual_hash
    )
  }

  subject_name <- design$subject_id
  if (!is.null(subject_name)) {
    if (!triomics_b_string(subject_name) || !subject_name %in% names(metadata)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "The declared subject column is absent.",
                       key = "design.subject_id")
    }
    subjects <- as.character(metadata[[subject_name]])
    names(subjects) <- sample_ids
    used <- unique(unlist(lapply(blocks, function(block) block$sample_ids), use.names = FALSE))
    observed <- subjects[used]
    observed <- observed[!is.na(observed) & nzchar(observed)]
    if (anyDuplicated(observed)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED",
                       "Duplicate subjects are not an independent-sample design.",
                       key = "design.subject_id")
    }
  }

  unit_map <- prepared_study$analysis_unit_map
  if (!is.null(unit_map)) {
    if (!is.data.frame(unit_map) || !all(c("unit_id", "representation_id") %in% names(unit_map))) {
      triomics_b_abort("B_DESIGN_UNIT_MAP", "An analysis unit map needs unit_id and representation_id.",
                       key = "analysis_unit_map")
    }
    unit_ids <- as.character(unit_map$unit_id)
    if (anyNA(unit_ids) || anyDuplicated(unit_ids) || !setequal(unit_ids, sample_ids)) {
      triomics_b_abort("B_DESIGN_UNIT_MAP", "Analysis unit ids do not match the sample ids.",
                       key = "analysis_unit_map")
    }
    if (any(as.character(unit_map$representation_id) != "levels")) {
      triomics_b_abort("B_DESIGN_UNIT_MAP", "This lane only accepts the levels representation.",
                       key = "analysis_unit_map")
    }
  }

  subjects <- NULL
  registry_samples <- prepared_study$sample_registry
  if (!is.null(registry_samples) && "subject_id" %in% names(registry_samples)) {
    subjects <- as.character(registry_samples$subject_id)[match(sample_ids, as.character(registry_samples$sample_id))]
    names(subjects) <- sample_ids
  }

  list(
    study_id = prepared_study$study_id,
    analysis_id = analysis_spec$analysis_id,
    preparation_context_id = analysis_spec$preparation_context_id,
    representation = "levels",
    model_plan = list(
      formula = "~0 + condition",
      column_ids = column_ids,
      levels = levels,
      reference_level = reference,
      target_level = target,
      condition = condition
    ),
    blocks = blocks,
    subjects = subjects
  )
}

resolve_analysis <- triomics_resolve_analysis
