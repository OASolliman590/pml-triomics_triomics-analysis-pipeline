# Qualified multivisit and trajectory fixed-subject inference.
# The response is subject-centered and the subject columns stay in X.
# Residual df is n_observed - rank(X). duplicateCorrelation is not fit.

.triomics_b04_qualification <- new.env(parent = emptyenv())
.triomics_b04_qualification$sealed <- FALSE
.triomics_b04_qualification$result <- NULL

triomics_b04_protocol <- function() {
  seeds <- 1001:1400
  list(
    n_studies = length(seeds),
    n_features = 100L,
    seeds = seeds,
    q = 0.05,
    multiple_testing_method = "BH",
    rejection_upper_bound = 0.08,
    coverage_lower_bound = 0.90
  )
}

triomics_b04_synthetic_protocol <- function(config_sha256) {
  subjects <- sprintf("sub%04d", 1:4)
  times <- c(0, 1, 2)
  features <- c("F1", "F2")
  list(
    protocol_id = "b-t04-synthetic-protocol-v1",
    protocol_version = "1.0.0",
    generator_id = "triomics_b04_execute_fixture",
    generator_protocol_version = "1.0.0",
    study_id = "triomics_b_t04_fixture",
    ordered_subjects = as.list(subjects),
    ordered_times = list(
      values = as.list(times),
      visit_labels = as.list(paste0("t", times)),
      unit = "synthetic_step",
      origin = 0
    ),
    feature_ids = as.list(features),
    subject_intercepts = stats::setNames(as.list(c(5, 7, 9, 11)), subjects),
    feature_slopes = stats::setNames(as.list(c(1.5, -0.25)), features),
    residual_pattern = list(
      time_values = as.list(times),
      values = as.list(c(0.2, -0.4, 0.2)),
      repeated_per_subject = TRUE
    ),
    measurement = list(
      quantity = "intensity",
      scale = "linear",
      units = "synthetic_arbitrary_units"
    ),
    generation = list(
      method = "deterministic_closed_form",
      randomness = "none",
      uses_rng = FALSE
    ),
    effective_configuration_sha256 = as.character(config_sha256),
    non_ancestral_source_declaration = paste(
      "The configured two-visit TSVs are ticket schema fixtures only.",
      "Their bytes, digests, and source IDs do not generate this response, design, fit, or result."
    )
  )
}

triomics_b04_write_synthetic_protocol <- function(protocol, output) {
  path <- file.path(output, "B", "b-t04-synthetic-protocol.json")
  if (file.exists(path) || dir.exists(path)) {
    triomics_b_abort(
      "B_EXPORT_INCOMPLETE",
      "The immutable B-T04 synthetic protocol already exists in this output root.",
      "synthetic_protocol",
      "Use a fresh output root and preserve the protocol bytes from one producer invocation."
    )
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  bytes <- triomics_b07_prov_canonical_value(protocol)
  temporary <- tempfile(pattern = ".b-t04-synthetic-protocol-", tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeBin(charToRaw(enc2utf8(bytes)), temporary)
  if (!file.rename(temporary, path)) {
    triomics_b_abort(
      "B_EXPORT_INCOMPLETE",
      "The canonical B-T04 synthetic protocol could not be finalized.",
      "synthetic_protocol",
      "Use a local output directory that supports atomic file renames."
    )
  }
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    bytes = bytes,
    sha256 = triomics_sha256(path),
    value = protocol
  )
}

triomics_b04_synthetic_context_sha256 <- function(context) {
  synthetic <- context$synthetic_producer
  resolved_blocks <- lapply(names(context$resolved_analysis$blocks), function(block_id) {
    block <- context$resolved_analysis$blocks[[block_id]]
    list(
      block_id = block_id,
      assay = block$assay,
      sample_ids = as.list(as.character(block$sample_ids)),
      subject_ids = as.list(as.character(block$subject_ids)),
      feature_keys = as.list(as.character(block$feature_keys)),
      scales = as.list(as.character(block$scales)),
      source_hash = as.character(block$source_hash),
      design = block$design
    )
  })
  triomics_b07_prov_value_sha256(list(
    effective_configuration_sha256 = as.character(attr(context$config, "config_sha256")),
    source_manifest_ids = lapply(context$source_manifests, function(source) {
      list(source_id = as.character(source$source_id), sha256 = as.character(source$sha256))
    }),
    source_file_ids = lapply(context$source_files, function(source) {
      list(source_id = as.character(source$source_id), sha256 = as.character(source$sha256),
           local_ref = as.character(source$local_ref))
    }),
    protocol = list(
      value = synthetic$protocol$value,
      bytes = synthetic$protocol$bytes,
      sha256 = synthetic$protocol$sha256
    ),
    sample_metadata = context$sample_metadata,
    sample_registry = context$sample_registry,
    declared_factor_levels = context$declared_factor_levels,
    feature_registry = context$feature_registry,
    analysis_unit_map = context$analysis_unit_map,
    resolved_analysis = list(
      lane = as.character(context$resolved_analysis$lane),
      study_id = as.character(context$resolved_analysis$study_id),
      analysis_id = as.character(context$resolved_analysis$analysis_id),
      preparation_context_id = as.character(context$resolved_analysis$preparation_context_id),
      representation_id = as.character(context$resolved_analysis$representation_id),
      model_plan = context$resolved_analysis$model_plan,
      unit_map = context$resolved_analysis$unit_map,
      blocks = resolved_blocks
    ),
    statistics_spec = context$statistics_spec,
    method_state = context$method_state,
    raw_response = synthetic$raw_response,
    centered_response = synthetic$centered_response,
    ordered_design = synthetic$ordered_design,
    subject_time_identity = synthetic$subject_time_identity,
    centering_rule = synthetic$centering_rule,
    synthetic_method_state = synthetic$method_state,
    calibration = synthetic$calibration[c(
      "status", "executed", "skipped", "protocol_sha256", "result_sha256", "payload_sha256"
    )],
    block_results_sha256 = as.character(context$block_results_sha256),
    block_results_envelope_sha256 = as.character(context$block_results_envelope_sha256),
    run_id = as.character(context$run_id),
    producer_method_ids = as.list(as.character(context$producer_method_ids))
  ))
}

triomics_b04_supported_patterns <- function() {
  c(
    "balanced_complete_visits",
    "balanced_visits_one_cell_removed",
    "balanced_linear_complete",
    "irregular_linear_complete",
    "natural_spline_complete"
  )
}

triomics_b04_capability <- function(contract_id) {
  ordinary <- "fixed_subject_ordinary_independent"
  if (identical(contract_id, ordinary)) {
    sealed <- isTRUE(.triomics_b04_qualification$sealed) &&
      is.list(.triomics_b04_qualification$result) &&
      identical(.triomics_b04_qualification$result$status, "pass")
    return(list(
      implemented = TRUE,
      qualified = sealed,
      status = if (sealed) "qualified" else "not_qualified",
      evidence = "subject_columns_retained_ordinary_least_squares",
      refusal_marks_implemented = FALSE
    ))
  }
  list(
    implemented = FALSE,
    qualified = FALSE,
    status = "unavailable",
    evidence = "prohibited_shortcut_not_implemented",
    refusal_marks_implemented = FALSE
  )
}

triomics_b04_require_contract <- function(contract_id) {
  if (!identical(contract_id, "fixed_subject_ordinary_independent")) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      "An unsupported residual contract is not replaced by an independent model.",
      key = "design.residual_contract_ref"
    )
  }
  invisible(contract_id)
}

triomics_b04_owns <- function(prepared_study, analysis_spec) {
  design <- if (is.list(analysis_spec)) analysis_spec$design else NULL
  if (is.null(design) || !is.list(design)) return(FALSE)
  isTRUE(design$repeated) && isTRUE(design$kind %in% c("multivisit", "trajectory"))
}

triomics_b04_center_vector <- function(y, subjects) {
  centered <- as.numeric(y)
  for (subject in unique(subjects)) {
    index <- which(subjects == subject & is.finite(y))
    if (length(index) > 0L) centered[index] <- y[index] - mean(y[index])
  }
  centered
}

triomics_b04_feature_support <- function(y, subjects, design, weight_list, check = TRUE) {
  if (length(y) != length(subjects) || length(y) != nrow(design)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "The repeated response is not aligned to the design.", key = "assays")
  }
  centered <- triomics_b04_center_vector(y, subjects)
  centered_support <- triomics_b_feature_support(centered, design, weight_list)
  if (isTRUE(check)) {
    raw_support <- triomics_b_feature_support(y, design, weight_list)
    for (j in seq_along(weight_list)) {
      if (!isTRUE(centered_support$estimable[[j]]) || !isTRUE(raw_support$estimable[[j]])) next
      same_effect <- triomics_b_close(centered_support$effects[[j]], raw_support$effects[[j]])
      same_se <- !is.finite(centered_support$ordinary_se[[j]]) ||
        triomics_b_close(centered_support$ordinary_se[[j]], raw_support$ordinary_se[[j]])
      same_df <- identical(as.integer(centered_support$df), as.integer(raw_support$df))
      if (!same_effect || !same_se || !same_df) {
        triomics_b_abort(
          "B_FIT_DIVERGENCE",
          "Centered fixed-subject contrasts diverged from the raw ordinary reference.",
          key = "design"
        )
      }
    }
  }
  centered_support
}

triomics_b04_bind_design <- function(sample_ids, subjects, extra_ids, extra) {
  subjects <- as.character(subjects)
  if (length(subjects) != length(sample_ids)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "Subject ids are not aligned to assay columns.", key = "design.subject_id")
  }
  subject_levels <- sort(unique(subjects))
  subject_levels <- vapply(subject_levels, triomics_b02_token, character(1), key = "design.subject_id")
  column_ids <- c(paste0("subject.", subject_levels), extra_ids)
  columns <- stats::setNames(vector("list", length(column_ids)), column_ids)
  for (subject in subject_levels) {
    columns[[paste0("subject.", subject)]] <- as.numeric(subjects == subject)
  }
  for (id in extra_ids) columns[[id]] <- as.numeric(extra[[id]])
  design <- triomics_b02_design_matrix(sample_ids, column_ids, columns)
  if (qr(design)$rank < ncol(design)) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      "The fixed-subject design is rank deficient and columns are not dropped to reclaim degrees of freedom.",
      key = "design"
    )
  }
  list(design = design, column_ids = column_ids, subject_levels = subject_levels)
}

triomics_b04_visit_basis <- function(design, metadata, sample_ids, prepared_study) {
  if (!identical(design$condition_type, "factor") || !identical(design$condition, design$visit_column)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A multivisit condition is the declared visit column.", key = "design.condition")
  }
  if (triomics_b_string(design$time_column) || triomics_b_string(design$time_unit) ||
      triomics_b_string(design$time_origin) || !triomics_b_empty(design$spline_knots) ||
      !triomics_b_empty(design$spline_boundary)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A visit factor does not carry a time basis.", key = "design.time_column")
  }
  levels <- triomics_b02_levels(prepared_study, design$visit_column)
  if (length(levels) < 3L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Exactly two visit levels stay on the paired delta lane.", key = "design.visit_column")
  }
  reference <- design$reference_level
  target <- design$target_level
  if (!triomics_b_string(reference) || !triomics_b_string(target) || identical(reference, target) ||
      !reference %in% levels || !target %in% levels) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "The visit contrast needs distinct declared reference and target levels.",
      key = "design.reference_level"
    )
  }
  if (!is.null(design$reference_visit) && !identical(design$reference_visit, reference)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "reference_visit must match reference_level.", key = "design.reference_visit")
  }
  visits <- as.character(triomics_b02_align(metadata, sample_ids, design$visit_column, "design.visit_column"))
  subjects <- as.character(triomics_b02_align(metadata, sample_ids, design$subject_id, "design.subject_id"))
  if (any(!nzchar(subjects) | is.na(subjects))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Every repeated row needs a subject id.", key = "design.subject_id")
  }
  if (any(!visits %in% levels)) {
    triomics_b_abort("B_DESIGN_LEVELS", "A sample visit is outside the declared levels.", key = "design.visit_column")
  }
  if (anyDuplicated(paste(subjects, visits, sep = "\r"))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A duplicate subject visit is not averaged.", key = "design.visit_column")
  }
  for (subject in unique(subjects)) {
    got <- visits[subjects == subject]
    if (length(got) != length(levels) || anyDuplicated(got) || !setequal(got, levels)) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "A missing or irregular visit is not imputed. Irregular clocks use the trajectory lane.",
        key = "design.visit_column"
      )
    }
  }
  kept <- levels[levels != reference]
  extra_ids <- paste0("visit.", kept)
  extra <- lapply(kept, function(level) as.numeric(visits == level))
  names(extra) <- extra_ids
  list(
    extra_ids = extra_ids,
    extra = extra,
    pattern = "balanced_complete_visits",
    basis = "visit_factor",
    subjects = subjects,
    time_unit = NULL,
    time_origin = NULL,
    spline_knots = numeric(),
    spline_boundary = numeric(),
    formula = "~0 + subject + visit",
    basis_hash = triomics_b_hash(list(
      basis = "visit_factor",
      levels = levels,
      reference = reference,
      columns = extra_ids
    ))
  )
}

triomics_b04_resolve <- function(prepared_study, analysis_spec) {
  design <- analysis_spec$design
  triomics_b04_require_contract(design$residual_contract_ref)
  if (!identical(design$representation, "subject_centered")) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Repeated fixed-subject inference uses the subject_centered representation.",
      key = "design.representation"
    )
  }
  if (!triomics_b_empty(design$covariates)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Declared covariates are kept out of this repeated lane and are not dropped.",
      key = "design.covariates"
    )
  }
  if (!is.null(design$between_group)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "A between-person factor is collinear with subject fixed effects and is not estimated from centered rows.",
      key = "design.between_group"
    )
  }
  if (!identical(design$missing_visits, "error")) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Missing visits stay an error in this lane and are not imputed.",
      key = "design.missing_visits"
    )
  }
  if (!triomics_b_empty(design$interactions)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Interactions are not expanded from names or evaluated as R code.",
      key = "design.interactions"
    )
  }
  if (!triomics_b_empty(design$mean_weights)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "A level-retaining mean weight is not a subject-centered repeated estimand.",
      key = "design.mean_weights"
    )
  }
  if (!triomics_b_string(analysis_spec$analysis_id)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The analysis id is required.", key = "analysis_id")
  }
  triomics_b02_check_map(prepared_study, "subject_centered")
  block_ids <- triomics_b_characters(analysis_spec$block_ids, "block_ids")
  assays <- prepared_study$assays
  if (length(block_ids) == 0L || is.null(assays) || any(!block_ids %in% names(assays))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Every analysis block must already be in the prepared study.", key = "block_ids")
  }
  metadata <- prepared_study$sample_metadata
  registry <- prepared_study$feature_registry
  blocks <- vector("list", length(block_ids))
  names(blocks) <- block_ids
  basis_info <- NULL
  column_ids <- NULL
  for (block_id in block_ids) {
    assay <- triomics_b02_validate_assay(assays[[block_id]])
    source_hash <- triomics_b02_hash_check(block_id, assay, prepared_study$assay_hashes)
    sample_ids <- colnames(assay)
    if (identical(design$kind, "multivisit")) {
      basis <- triomics_b04_visit_basis(design, metadata, sample_ids, prepared_study)
    } else if (identical(design$kind, "trajectory")) {
      basis <- triomics_b04_time_basis(design, metadata, sample_ids)
    } else {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane resolves multivisit and trajectory designs.", key = "design.kind")
    }
    bound <- triomics_b04_bind_design(sample_ids, basis$subjects, basis$extra_ids, basis$extra)
    if (is.null(column_ids)) {
      column_ids <- bound$column_ids
      basis_info <- basis
    } else if (!identical(bound$column_ids, column_ids)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "Repeated blocks do not share one fixed-subject column set.", key = "block_ids")
    }
    feature_keys <- rownames(assay)
    scales <- vapply(feature_keys, triomics_b_scale, character(1), registry = registry, block_id = block_id)
    blocks[[block_id]] <- list(
      block_id = block_id,
      assay = assay,
      sample_ids = sample_ids,
      subject_ids = as.character(basis$subjects),
      feature_keys = feature_keys,
      scales = unname(scales),
      source_hash = source_hash,
      design = bound$design
    )
  }
  named_subjects <- stats::setNames(character(), character())
  for (block in blocks) named_subjects[block$sample_ids] <- block$subject_ids
  list(
    lane = "b_t04",
    study_id = prepared_study$study_id,
    analysis_id = analysis_spec$analysis_id,
    preparation_context_id = analysis_spec$preparation_context_id,
    representation = "subject_centered",
    representation_id = "subject_centered",
    model_plan = list(
      formula = basis_info$formula,
      column_ids = column_ids,
      subject_terms_retained = TRUE,
      duplicate_correlation_applied = FALSE,
      pattern = basis_info$pattern,
      time_unit = basis_info$time_unit,
      time_origin = basis_info$time_origin,
      basis = basis_info$basis,
      basis_hash = basis_info$basis_hash,
      spline_knots = basis_info$spline_knots,
      spline_boundary = basis_info$spline_boundary,
      kind = design$kind,
      residual_contract_ref = design$residual_contract_ref,
      estimand = if (identical(design$kind, "trajectory")) {
        "fixed_subject_trajectory_contrast"
      } else {
        "fixed_subject_within_visit_contrast"
      }
    ),
    blocks = blocks,
    subjects = named_subjects
  )
}

triomics_b04_plain_design <- function(design) {
  plain <- matrix(as.numeric(design), nrow = nrow(design), ncol = ncol(design))
  plain
}

triomics_b04_moderated_fit <- function(assay, design, contrast_matrix) {
  fitted <- limma::lmFit(assay, design)
  contrasted <- limma::contrasts.fit(fitted, contrast_matrix)
  suppressWarnings(limma::eBayes(contrasted, trend = FALSE, robust = FALSE))
}

triomics_b04_moderated_provenance <- function(layout, protocol) {
  list(
    method = "limma",
    fit = "limma::lmFit",
    contrast = "limma::contrasts.fit",
    moderation = "limma::eBayes",
    trend = FALSE,
    robust = FALSE,
    contrast_id = layout$contrast_id,
    contrast_weights = as.list(layout$weights),
    multiple_testing = protocol$multiple_testing_method,
    p_value = "moderated$p.value",
    standard_error = "sqrt(s2.post) * stdev.unscaled",
    effective_df = "moderated$df.total"
  )
}

triomics_b04_visit_layout <- function() {
  subjects <- rep(paste0("S", 1:5), each = 3)
  visits <- rep(c("v1", "v2", "v3"), times = 5)
  subject_ids <- paste0("S", 1:5)
  columns <- lapply(subject_ids, function(subject) as.numeric(subjects == subject))
  names(columns) <- paste0("subject.", subject_ids)
  columns$visit.v2 <- as.numeric(visits == "v2")
  columns$visit.v3 <- as.numeric(visits == "v3")
  column_ids <- names(columns)
  design <- triomics_b02_design_matrix(paste0("row", seq_along(subjects)), column_ids, columns)
  weights <- stats::setNames(rep(0, length(column_ids)), column_ids)
  weights[["visit.v3"]] <- 1
  list(
    design = design,
    subjects = subjects,
    weights = weights,
    target_index = which(column_ids == "visit.v3"),
    contrast_id = "within"
  )
}

triomics_b04_subject_time_layout <- function(subjects, times, weight_name, extra,
                                             contrast_id = "per_synthetic_step") {
  subject_ids <- sort(unique(subjects))
  columns <- lapply(subject_ids, function(subject) as.numeric(subjects == subject))
  names(columns) <- paste0("subject.", subject_ids)
  for (name in names(extra)) columns[[name]] <- as.numeric(extra[[name]])
  column_ids <- names(columns)
  design <- triomics_b02_design_matrix(paste0("row", seq_along(subjects)), column_ids, columns)
  weights <- stats::setNames(rep(0, length(column_ids)), column_ids)
  weights[[weight_name]] <- 1
  list(
    design = design,
    subjects = as.character(subjects),
    weights = weights,
    target_index = which(column_ids == weight_name),
    contrast_id = contrast_id
  )
}

triomics_b04_layout <- function(pattern) {
  if (identical(pattern, "balanced_complete_visits")) {
    built <- triomics_b04_visit_layout()
  } else if (identical(pattern, "balanced_visits_one_cell_removed")) {
    built <- triomics_b04_visit_layout()
    built$design <- built$design[-3L, , drop = FALSE]
    built$subjects <- built$subjects[-3L]
  } else if (identical(pattern, "balanced_linear_complete")) {
    subjects <- rep(c("A", "B", "C", "D"), each = 3)
    times <- rep(c(0, 1, 2), times = 4)
    built <- triomics_b04_subject_time_layout(subjects, times, "time.linear", list(time.linear = times))
  } else if (identical(pattern, "irregular_linear_complete")) {
    subjects <- rep(c("A", "B", "C", "D"), each = 3)
    times <- c(0, 1, 4, 0, 2, 5, 1, 2, 6, 0, 3, 6)
    built <- triomics_b04_subject_time_layout(subjects, times, "time.linear", list(time.linear = times))
  } else if (identical(pattern, "natural_spline_complete")) {
    subjects <- rep(c("A", "B", "C", "D"), each = 5)
    times <- rep(0:4, times = 4)
    basis <- splines::ns(times, knots = 2, Boundary.knots = c(0, 4))
    extra <- list(time.ns1 = as.numeric(basis[, 1L]), time.ns2 = as.numeric(basis[, 2L]))
    built <- triomics_b04_subject_time_layout(subjects, times, "time.ns1", extra)
  } else {
    triomics_b_abort("B_REPEATED_MODEL_UNSUPPORTED", "The calibration pattern is not declared.", key = "design")
  }
  if (qr(built$design)$rank != ncol(built$design) || length(built$target_index) != 1L) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      paste0("Calibration layout ", pattern, " is rank deficient."),
      key = "design"
    )
  }
  built
}

triomics_b04_ordinary_reference <- function(layout) {
  design <- layout$design
  weights <- as.numeric(layout$weights)
  beta <- rep(0, ncol(design))
  beta[[layout$target_index]] <- 1.25
  set.seed(4242)
  noise <- stats::rnorm(nrow(design))
  noise <- noise - as.numeric(qr.fitted(qr(design), noise))
  y <- as.numeric(design %*% beta) + noise
  support <- triomics_b04_feature_support(y, layout$subjects, design, list(weights), check = TRUE)
  max_abs <- abs(support$effects[[1L]] - sum(weights * beta))
  list(
    status = if (isTRUE(max_abs <= 1e-8) && isTRUE(support$eligible)) "pass" else "fail",
    max_abs_effect = max_abs,
    centered_matches_raw = TRUE
  )
}

triomics_b04_null_cell <- function(layout, protocol, store_first) {
  design <- triomics_b04_plain_design(layout$design)
  subjects <- layout$subjects
  named_weights <- layout$weights
  weights <- as.numeric(named_weights)
  n <- nrow(design)
  n_features <- protocol$n_features
  decomposition <- qr(design)
  rank <- decomposition$rank
  df <- n - rank
  variance <- sum(weights * solve(crossprod(design), weights))
  ordinary_tcrit <- stats::qt(0.975, df)
  ordinary_rejections <- 0L
  ordinary_covers <- 0L
  moderated_rejections <- 0L
  moderated_covers <- 0L
  n_intervals <- 0L
  study_coverage <- numeric(protocol$n_studies)
  first_ordinary <- NULL
  first_moderated <- NULL
  first_raw <- NULL
  contrast_matrix <- matrix(
    named_weights,
    ncol = 1L,
    dimnames = list(names(named_weights), layout$contrast_id)
  )
  for (study_index in seq_along(protocol$seeds)) {
    seed <- protocol$seeds[[study_index]]
    set.seed(seed)
    generated <- matrix(stats::rnorm(n * n_features), nrow = n)
    centered <- generated
    for (subject in unique(subjects)) {
      index <- which(subjects == subject)
      centered[index, ] <- sweep(
        centered[index, , drop = FALSE], 2L,
        colMeans(centered[index, , drop = FALSE]), "-"
      )
    }

    coefficients <- qr.coef(decomposition, centered)
    residual <- centered - qr.fitted(decomposition, centered)
    sigma2 <- colSums(residual^2) / df
    ordinary_effects <- as.numeric(crossprod(weights, coefficients))
    ordinary_ses <- sqrt(sigma2 * variance)
    ordinary_p <- 2 * stats::pt(-abs(ordinary_effects / ordinary_ses), df)
    ordinary_q <- stats::p.adjust(ordinary_p, method = protocol$multiple_testing_method)
    if (any(ordinary_q < protocol$q)) ordinary_rejections <- ordinary_rejections + 1L
    ordinary_covered <- abs(ordinary_effects) <= ordinary_tcrit * ordinary_ses
    ordinary_covers <- ordinary_covers + sum(ordinary_covered)

    moderated <- triomics_b04_moderated_fit(t(centered), layout$design, contrast_matrix)
    moderated_effects <- as.numeric(moderated$coefficients[, 1L])
    moderated_ses <- sqrt(as.numeric(moderated$s2.post)) *
      as.numeric(moderated$stdev.unscaled[, 1L])
    moderated_df <- as.numeric(moderated$df.total)
    moderated_p <- as.numeric(moderated$p.value[, 1L])
    moderated_q <- stats::p.adjust(moderated_p, method = protocol$multiple_testing_method)
    if (any(moderated_q < protocol$q)) moderated_rejections <- moderated_rejections + 1L
    moderated_tcrit <- stats::qt(0.975, moderated_df)
    moderated_covered <- abs(moderated_effects) <= moderated_tcrit * moderated_ses
    moderated_covers <- moderated_covers + sum(moderated_covered)
    n_intervals <- n_intervals + length(moderated_effects)
    study_coverage[[study_index]] <- mean(moderated_covered)

    if (isTRUE(store_first) && identical(as.integer(seed), 1001L)) {
      first_raw <- generated[, 1L]
      first_ordinary <- list(
        n_observed = as.integer(n),
        rank = as.integer(rank),
        df = as.integer(df),
        effect = ordinary_effects[[1L]],
        ordinary_se = ordinary_ses[[1L]],
        p = ordinary_p[[1L]]
      )
      first_ordinary$feature_support <- list(
        n_observed = rep.int(as.integer(n), n_features),
        rank = rep.int(as.integer(rank), n_features),
        residual_df = rep.int(as.integer(df), n_features),
        effect = ordinary_effects,
        ordinary_se = ordinary_ses,
        p = ordinary_p
      )
      first_moderated <- list(
        effect = moderated_effects[[1L]],
        moderated_se = moderated_ses[[1L]],
        effective_df = moderated_df[[1L]],
        p = moderated_p[[1L]],
        q = moderated_q[[1L]],
        statistic = as.numeric(moderated$t[1L, 1L])
      )
    }
  }
  helper_ok <- TRUE
  if (!is.null(first_raw)) {
    support <- triomics_b04_feature_support(first_raw, subjects, design, list(weights), check = TRUE)
    helper_p <- 2 * stats::pt(-abs(support$effects[[1L]] / support$ordinary_se[[1L]]), support$df)
    helper_ok <- triomics_b_close(support$effects[[1L]], first_ordinary$effect) &&
      triomics_b_close(support$ordinary_se[[1L]], first_ordinary$ordinary_se) &&
      identical(as.integer(support$df), as.integer(first_ordinary$df)) &&
      identical(as.integer(support$rank), as.integer(first_ordinary$rank)) &&
      triomics_b_close(helper_p, first_ordinary$p)
  }

  n_studies <- protocol$n_studies
  ordinary_rejection_upper <- stats::qbeta(
    0.95, ordinary_rejections + 1L, n_studies - ordinary_rejections
  )
  ordinary_interval_coverage_lower <- stats::qbeta(
    0.05, ordinary_covers, n_intervals - ordinary_covers + 1L
  )
  ordinary_pass <- isTRUE(ordinary_rejection_upper <= protocol$rejection_upper_bound) &&
    isTRUE(ordinary_interval_coverage_lower >= protocol$coverage_lower_bound) &&
    identical(as.integer(n_intervals), as.integer(n_studies * n_features)) &&
    isTRUE(helper_ok)

  moderated_rejection_upper <- stats::qbeta(
    0.95, moderated_rejections + 1L, n_studies - moderated_rejections
  )
  study_coverage_lower <- mean(study_coverage) -
    stats::qt(0.95, df = n_studies - 1L) * stats::sd(study_coverage) / sqrt(n_studies)
  moderated_interval_coverage_lower <- stats::qbeta(
    0.05, moderated_covers, n_intervals - moderated_covers + 1L
  )
  moderated_pass <- isTRUE(moderated_rejection_upper <= protocol$rejection_upper_bound) &&
    isTRUE(study_coverage_lower >= protocol$coverage_lower_bound) &&
    identical(as.integer(n_intervals), as.integer(n_studies * n_features)) &&
    isTRUE(helper_ok)

  list(
    ordinary_null = list(
      status = if (ordinary_pass) "pass" else "fail",
      method = "ordinary_fixed_subject_qr",
      n_studies = n_studies,
      n_features = n_features,
      seed_first = protocol$seeds[[1L]],
      seed_last = protocol$seeds[[length(protocol$seeds)]],
      rejection_upper = ordinary_rejection_upper,
      interval_coverage_lower = ordinary_interval_coverage_lower,
      n_intervals = as.integer(n_intervals),
      n_rejections = as.integer(ordinary_rejections),
      n_covers = as.integer(ordinary_covers),
      helper_matches_reference = helper_ok,
      first_feature = first_ordinary,
      feature_support = if (is.null(first_ordinary)) NULL else first_ordinary$feature_support
    ),
    moderated_null = list(
      status = if (moderated_pass) "pass" else "fail",
      provenance = triomics_b04_moderated_provenance(layout, protocol),
      n_studies = n_studies,
      n_features = n_features,
      seed_first = protocol$seeds[[1L]],
      seed_last = protocol$seeds[[length(protocol$seeds)]],
      rejection_upper = moderated_rejection_upper,
      coverage_lower = study_coverage_lower,
      coverage_lower_method = "one_sided_t_over_independent_study_coverage_proportions",
      n_study_coverage = as.integer(length(study_coverage)),
      study_coverage_proportions = as.list(study_coverage),
      coverage_mean = mean(study_coverage),
      coverage_sd = stats::sd(study_coverage),
      interval_coverage_lower = moderated_interval_coverage_lower,
      interval_coverage_bound_status = "descriptive_only",
      n_intervals = as.integer(n_intervals),
      n_rejections = as.integer(moderated_rejections),
      n_covers = as.integer(moderated_covers),
      helper_matches_reference = helper_ok,
      first_feature = first_moderated
    )
  )
}

triomics_b04_calibrate_pattern <- function(pattern, protocol) {
  layout <- triomics_b04_layout(pattern)
  ordinary <- triomics_b04_ordinary_reference(layout)
  null <- triomics_b04_null_cell(
    layout, protocol, store_first = TRUE
  )
  list(
    executed = TRUE,
    skipped = FALSE,
    ordinary_reference = ordinary,
    ordinary_null = null$ordinary_null,
    null = null$moderated_null
  )
}

triomics_b04_calibrate <- function() {
  if (isTRUE(.triomics_b04_qualification$sealed)) return(.triomics_b04_qualification$result)
  cached <- .triomics_b04_qualification$result
  if (is.list(cached) && identical(cached$status, "fail")) return(cached)
  protocol <- triomics_b04_protocol()
  previous_kind <- RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    RNGkind(previous_kind[[1L]], previous_kind[[2L]], previous_kind[[3L]])
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  patterns <- list()
  for (pattern in triomics_b04_supported_patterns()) {
    patterns[[pattern]] <- triomics_b04_calibrate_pattern(pattern, protocol)
  }
  failed <- character()
  notes <- character()
  for (pattern in names(patterns)) {
    cell <- patterns[[pattern]]
    ordinary_pass <- identical(cell$ordinary_reference$status, "pass")
    null_pass <- identical(cell$null$status, "pass")
    if (!ordinary_pass || !null_pass) {
      failed <- c(failed, pattern)
      notes <- c(notes, paste0(
        pattern, " ordinary=", cell$ordinary_reference$status,
        " max_abs=", format(cell$ordinary_reference$max_abs_effect, digits = 8),
        " ordinary_null=", cell$ordinary_null$status,
        " ordinary_reject=", format(cell$ordinary_null$rejection_upper, digits = 6),
        " moderated_null=", cell$null$status,
        " reject=", format(cell$null$rejection_upper, digits = 6),
        " cover=", format(cell$null$coverage_lower, digits = 6)
      ))
    }
  }
  passed_patterns <- names(patterns)[vapply(patterns, function(cell) {
    identical(cell$ordinary_reference$status, "pass") && identical(cell$null$status, "pass")
  }, logical(1))]
  result <- list(
    status = if (length(failed) == 0L) "pass" else "fail",
    executed = TRUE,
    skipped = FALSE,
    protocol = protocol,
    patterns = patterns,
    passed_patterns = as.character(passed_patterns),
    failure_summary = paste(notes, collapse = "; "),
    seed_policy = "Protocol seeds 1001:1400 are not the execution seed.",
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      limma_version = as.character(utils::packageVersion("limma")),
      splines_version = as.character(utils::packageVersion("splines"))
    )
  )
  assign("result", result, envir = .triomics_b04_qualification)
  if (identical(result$status, "pass")) {
    assign("sealed", TRUE, envir = .triomics_b04_qualification)
  }
  result
}

triomics_b04_required_unit <- function(scale, kind) {
  if (identical(kind, "trajectory")) return("per_unit_beta")
  if (identical(scale, "log2")) return("log2_fold_change")
  if (scale %in% c("clr_ln", "clr_log2")) return("clr_contrast")
  if (scale %in% c("linear", "ln", "variance_stabilized")) return("within_person_difference")
  ""
}

triomics_b04_check_effect_unit <- function(resolved_analysis, statistics_spec) {
  kind <- resolved_analysis$model_plan$kind
  scales <- unlist(lapply(resolved_analysis$blocks, function(block) block$scales), use.names = FALSE)
  required <- unique(vapply(scales, triomics_b04_required_unit, character(1), kind = kind))
  if (length(required) != 1L || !nzchar(required[[1L]]) || !identical(required[[1L]], statistics_spec$effect_unit)) {
    triomics_b_abort(
      "B_DESIGN_EFFECT_UNIT",
      "The effect unit does not match the declared measurement scale.",
      key = "statistics.effect_unit"
    )
  }
  required[[1L]]
}

triomics_b04_feature_pattern <- function(y, schedule, design) {
  missing <- !is.finite(y)
  if (!any(missing)) return(schedule)
  if (identical(schedule, "balanced_complete_visits") && sum(missing) == 1L) {
    used <- design[!missing, , drop = FALSE]
    if (nrow(used) > 0L && qr(used)$rank == ncol(design)) return("balanced_visits_one_cell_removed")
  }
  "unsupported_missing_pattern"
}

triomics_b04_fit <- function(resolved_analysis, statistics_spec) {
  if (identical(statistics_spec$method, "deqms")) {
    version <- if (requireNamespace("limma", quietly = TRUE)) as.character(utils::packageVersion("limma")) else NULL
    return(list(
      status = "unavailable",
      fallback_applied = FALSE,
      blocks = list(),
      effects = NULL,
      limma_version = version
    ))
  }
  if (!identical(resolved_analysis$lane, "b_t04")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The repeated fitter received another lane.", key = "design")
  }
  if (!identical(statistics_spec$method, "limma")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane fits limma or reports DEqMS as unavailable.", key = "statistics.method")
  }
  if (!identical(statistics_spec$trend, FALSE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma trend stays false in this lane.", key = "statistics.trend")
  }
  if (!is.logical(statistics_spec$robust) || length(statistics_spec$robust) != 1L || is.na(statistics_spec$robust)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma robust must be true or false.", key = "statistics.robust")
  }
  if (!identical(statistics_spec$robust, FALSE)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "B-T04 only qualifies limma robust=FALSE; robust=TRUE needs its own calibration.",
      key = "statistics.robust"
    )
  }
  if (!is.logical(statistics_spec$rank_sensitivity) || length(statistics_spec$rank_sensitivity) != 1L ||
      is.na(statistics_spec$rank_sensitivity)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Rank sensitivity must be true or false.", key = "statistics.rank_sensitivity")
  }
  if (!statistics_spec$selection_validity %in% c("qualified", "descriptive_only", "unverified")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Selection validity is not a known family status.", key = "statistics.selection_validity")
  }
  if (!requireNamespace("limma", quietly = TRUE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma is not installed.", key = "statistics.method",
                     remediation = "Install limma from Bioconductor before fitting.")
  }
  triomics_b04_require_contract(resolved_analysis$model_plan$residual_contract_ref)
  qualification <- triomics_b04_calibrate()
  if (!identical(qualification$status, "pass")) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      paste0("Repeated qualification did not pass. ", qualification$failure_summary),
      key = "design.residual_contract_ref"
    )
  }
  plan <- resolved_analysis$model_plan
  if (!plan$pattern %in% qualification$passed_patterns) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      "This repeated schedule is outside the qualified patterns.",
      key = "design"
    )
  }
  effect_unit <- triomics_b04_check_effect_unit(resolved_analysis, statistics_spec)
  column_ids <- plan$column_ids
  contrasts <- statistics_spec$contrasts
  if (length(contrasts) == 0L) {
    triomics_b_abort("B_DESIGN_CONTRAST", "At least one named contrast is required.", key = "statistics.contrasts")
  }
  contrast_ids <- vapply(contrasts, function(contrast) {
    if (!triomics_b_string(contrast$contrast_id)) {
      triomics_b_abort("B_DESIGN_CONTRAST", "Each contrast needs a contrast id.", key = "statistics.contrasts")
    }
    contrast$contrast_id
  }, character(1))
  if (anyDuplicated(contrast_ids)) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Contrast ids must be unique.", key = "statistics.contrasts")
  }
  weight_list <- lapply(contrasts, triomics_b_contrast_weights, column_ids = column_ids)
  contrast_matrix <- matrix(0, length(column_ids), length(contrast_ids), dimnames = list(column_ids, contrast_ids))
  for (j in seq_along(weight_list)) contrast_matrix[, j] <- weight_list[[j]]
  publish_q <- identical(statistics_spec$selection_validity, "qualified")
  blocks <- vector("list", length(resolved_analysis$blocks))
  names(blocks) <- names(resolved_analysis$blocks)
  all_estimated <- TRUE
  for (block in resolved_analysis$blocks) {
    n_features <- length(block$feature_keys)
    raw_rows <- lapply(seq_len(n_features), function(i) as.numeric(block$assay[i, ]))
    feature_patterns <- vapply(raw_rows, triomics_b04_feature_pattern, character(1),
                               schedule = plan$pattern, design = block$design)
    supports <- lapply(raw_rows, function(y) {
      triomics_b04_feature_support(y, block$subject_ids, block$design, weight_list, check = TRUE)
    })
    qualified_feature <- feature_patterns %in% qualification$passed_patterns
    eligible <- qualified_feature & vapply(supports, function(support) isTRUE(support$eligible), logical(1))
    centered_assay <- block$assay
    for (i in seq_len(n_features)) centered_assay[i, ] <- triomics_b04_center_vector(raw_rows[[i]], block$subject_ids)
    moderated <- NULL
    n_eligible <- sum(eligible)
    if (n_eligible > 0L) {
      moderated <- triomics_b04_moderated_fit(
        centered_assay[eligible, , drop = FALSE], block$design, contrast_matrix
      )
      local <- 0L
      for (feature in which(eligible)) {
        local <- local + 1L
        sigma <- triomics_b_gene_value(moderated$sigma, local)
        for (j in seq_along(weight_list)) {
          unscaled <- triomics_b_at(moderated$stdev.unscaled, local, j, n_eligible, length(weight_list))
          limma_effect <- triomics_b_at(moderated$coefficients, local, j, n_eligible, length(weight_list))
          if (!triomics_b_close(supports[[feature]]$ordinary_se[[j]], sigma * unscaled) ||
              !triomics_b_close(supports[[feature]]$effects[[j]], limma_effect)) {
            triomics_b_abort(
              "B_FIT_DIVERGENCE",
              paste0("Ordinary contrast results diverged from limma for ", block$feature_keys[[feature]], "."),
              key = "statistics.contrasts"
            )
          }
        }
      }
    }
    local_of <- rep(NA_integer_, n_features)
    if (n_eligible > 0L) local_of[which(eligible)] <- seq_len(n_eligible)
    model_hash <- triomics_b_hash(list(
      formula = plan$formula,
      column_ids = column_ids,
      pattern = plan$pattern,
      samples = lapply(seq_along(block$sample_ids), function(index) {
        list(sample_id = block$sample_ids[[index]], subject = block$subject_ids[[index]])
      })
    ))
    contrast_results <- vector("list", length(contrast_ids))
    names(contrast_results) <- contrast_ids
    for (j in seq_along(contrast_ids)) {
      weights <- weight_list[[j]]
      contrast_hash <- triomics_b_hash(list(contrast_id = contrast_ids[[j]], weights = as.list(weights)))
      p_values <- rep(NA_real_, n_features)
      estimated <- logical(n_features)
      rows <- vector("list", n_features)
      moderated_values <- vector("list", n_features)
      for (i in seq_len(n_features)) {
        support <- supports[[i]]
        local <- local_of[[i]]
        if (!is.na(local)) {
          estimated[[i]] <- TRUE
          unscaled <- triomics_b_at(moderated$stdev.unscaled, local, j, n_eligible, length(weight_list))
          s2_post <- triomics_b_gene_value(moderated$s2.post, local)
          p_values[[i]] <- triomics_b_at(moderated$p.value, local, j, n_eligible, length(weight_list))
          moderated_values[[i]] <- list(
            moderated_se = sqrt(s2_post) * unscaled,
            statistic = triomics_b_at(moderated$t, local, j, n_eligible, length(weight_list)),
            effective_df = triomics_b_gene_value(moderated$df.total, local),
            df_prior = triomics_b_gene_value(moderated$df.prior, local),
            p = p_values[[i]],
            reason = NULL
          )
        } else if (!feature_patterns[[i]] %in% qualification$passed_patterns) {
          moderated_values[[i]] <- list(reason = "unsupported_missing_pattern")
        } else if (!isTRUE(support$estimable[[j]])) {
          moderated_values[[i]] <- list(reason = "contrast_not_estimable")
        } else if (support$df < 1) {
          moderated_values[[i]] <- list(reason = "zero_residual_support")
        } else {
          moderated_values[[i]] <- list(reason = "unsupported_missing_pattern")
        }
      }
      if (!all(estimated)) all_estimated <- FALSE
      family <- triomics_b_family(
        resolved_analysis$study_id, resolved_analysis$analysis_id, block$block_id,
        contrast_ids[[j]], block$feature_keys, estimated, p_values, publish_q
      )
      for (i in seq_len(n_features)) {
        drawn <- moderated_values[[i]]
        q_value <- if (estimated[[i]] && publish_q) family$internal_q[[i]] else NULL
        row <- triomics_b_result_row(
          feature_key = block$feature_keys[[i]],
          scale = block$scales[[i]],
          support = supports[[i]],
          contrast_index = j,
          estimated = estimated[[i]],
          reason = if (estimated[[i]]) NULL else drawn$reason,
          moderated_se = if (estimated[[i]]) drawn$moderated_se else NULL,
          statistic = if (estimated[[i]]) drawn$statistic else NULL,
          effective_df = if (estimated[[i]]) drawn$effective_df else NULL,
          df_prior = if (estimated[[i]]) drawn$df_prior else NULL,
          p_value = if (estimated[[i]]) drawn$p else NULL,
          q_value = q_value,
          effect_unit = effect_unit,
          selection_validity = statistics_spec$selection_validity,
          sample_ids = block$sample_ids,
          subjects = resolved_analysis$subjects,
          source_hash = block$source_hash,
          model_hash = model_hash,
          contrast_hash = contrast_hash
        )
        row$design_rank <- as.integer(supports[[i]]$rank)
        row$feature_pattern <- feature_patterns[[i]]
        row$duplicate_correlation_applied <- FALSE
        row$direction_status <- "not_applicable"
        row$direction_reason <- "fixed_subject_repeated_effect_is_not_a_subject_delta_sign_count"
        row$direction_numerator <- NULL
        row$direction_denominator <- NULL
        row$direction_zero_count <- NULL
        row$n_pairs <- NULL
        row$representation_hash <- triomics_b_hash(list(
          representation = "subject_centered",
          scale = block$scales[[i]],
          feature_key = block$feature_keys[[i]]
        ))
        rows[[i]] <- row
      }
      sensitivity <- if (isTRUE(statistics_spec$rank_sensitivity)) {
        list(status = "unavailable", substitutes_for_primary = FALSE, equivalent_to_moderated_test = FALSE)
      } else {
        list(status = "not_requested")
      }
      contrast_results[[contrast_ids[[j]]]] <- list(
        contrast_id = contrast_ids[[j]],
        rows = rows,
        family = family,
        count_prior_status = "not_used_no_automatic_deqms",
        sensitivity = sensitivity
      )
    }
    blocks[[block$block_id]] <- list(block_id = block$block_id, contrasts = contrast_results)
  }
  list(
    status = if (all_estimated) "complete" else "partial",
    limma_version = as.character(utils::packageVersion("limma")),
    blocks = blocks,
    effects = NULL
  )
}

triomics_b04_execute_fixture <- function(config_path, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Stage B output must be a directory path.", key = "output")
  }
  config <- triomics_read_config(config_path)
  triomics_b_identity_preparation(config)
  if (length(config$analyses) != 1L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane writes one analysis envelope.", key = "analyses")
  }
  analysis <- config$analyses[[1L]]
  design <- analysis$design
  splines_empty <- triomics_b_empty(design$spline_knots) && triomics_b_empty(design$spline_boundary)
  ticket_ok <- identical(config$study_id, "triomics_b_t04_fixture") &&
    identical(design$kind, "trajectory") &&
    identical(design$representation, "subject_centered") &&
    isTRUE(design$repeated) &&
    identical(design$subject_id, "subject") &&
    identical(design$condition, "time") &&
    identical(design$condition_type, "continuous") &&
    identical(design$time_column, "time") &&
    identical(design$time_unit, "synthetic_step") &&
    identical(design$time_origin, "0") &&
    isTRUE(splines_empty) &&
    identical(design$residual_contract_ref, "fixed_subject_ordinary_independent") &&
    identical(design$missing_visits, "error") &&
    is.null(design$reference_level) &&
    is.null(design$target_level) &&
    triomics_b_empty(design$covariates) &&
    is.null(design$between_group) &&
    identical(analysis$statistics$method, "limma") &&
    identical(analysis$statistics$effect_unit, "per_unit_beta") &&
    length(analysis$statistics$contrasts) == 1L &&
    identical(analysis$statistics$contrasts[[1L]]$contrast_id, "per_synthetic_step")
  if (!ticket_ok) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "The B-T04 fixture ticket does not request the qualified four-subject linear trajectory.",
      key = "design"
    )
  }
  block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
  if (!identical(block_ids, "block_1")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The B-T04 fixture uses one block.", key = "block_ids")
  }
  n_features <- 2L
  n_fits <- n_features * length(analysis$statistics$contrasts)
  max_fits <- config$execution$max_fits
  if (!is.numeric(max_fits) || length(max_fits) != 1L || is.na(max_fits) || n_fits > max_fits) {
    triomics_b_abort("B_DESIGN_BUDGET", "The declared feature-contrast fits exceed max_fits.", key = "execution.max_fits")
  }
  config_sha256 <- as.character(attr(config, "config_sha256"))
  protocol <- triomics_b04_synthetic_protocol(config_sha256)
  protocol_artifact <- triomics_b04_write_synthetic_protocol(protocol, output)
  protocol_id <- as.character(protocol$protocol_id)
  subjects <- unlist(protocol$ordered_subjects, use.names = FALSE)
  times <- as.numeric(unlist(protocol$ordered_times$values, use.names = FALSE))
  visit_levels <- unlist(protocol$ordered_times$visit_labels, use.names = FALSE)
  feature_ids <- unlist(protocol$feature_ids, use.names = FALSE)
  subject_intercepts <- as.numeric(unlist(protocol$subject_intercepts[subjects], use.names = FALSE))
  feature_slopes <- as.numeric(unlist(protocol$feature_slopes[feature_ids], use.names = FALSE))
  names(feature_slopes) <- feature_ids
  residual_pattern <- as.numeric(unlist(protocol$residual_pattern$values, use.names = FALSE))
  sample_ids <- unlist(lapply(subjects, function(subject) paste0(subject, "_", visit_levels)),
                       use.names = FALSE)
  subject_col <- rep(subjects, each = length(times))
  time_col <- rep(times, times = length(subjects))
  visits <- rep(visit_levels, times = length(subjects))
  intercept <- rep(subject_intercepts, each = length(times))
  residual <- rep(residual_pattern, times = length(subjects))
  assay <- do.call(rbind, lapply(feature_ids, function(feature_id) {
    intercept + feature_slopes[[feature_id]] * time_col + residual
  }))
  rownames(assay) <- feature_ids
  colnames(assay) <- sample_ids
  study_id <- as.character(protocol$study_id)
  time_unit <- as.character(protocol$ordered_times$unit)
  time_origin <- as.numeric(protocol$ordered_times$origin)
  measurement <- protocol$measurement
  sample_metadata <- data.frame(
    sample_id = sample_ids,
    subject = subject_col,
    visit = visits,
    time = time_col,
    time_unit = rep(time_unit, length(sample_ids)),
    time_origin = rep(time_origin, length(sample_ids)),
    stringsAsFactors = FALSE
  )
  sample_registry <- data.frame(
    study_id = rep(study_id, length(sample_ids)),
    sample_id = sample_ids,
    subject_id = subject_col,
    visit_id = visits,
    time = time_col,
    time_unit = rep(time_unit, length(sample_ids)),
    time_origin = rep(time_origin, length(sample_ids)),
    source_id = rep(protocol_id, length(sample_ids)),
    source_row = as.character(seq_along(sample_ids)),
    sample_role = rep("synthetic_generated", length(sample_ids)),
    stringsAsFactors = FALSE
  )
  feature_registry <- data.frame(
    study_id = rep(study_id, length(feature_ids)),
    block_id = rep("block_1", length(feature_ids)),
    feature_key = feature_ids,
    canonical_id = feature_ids,
    original_id = feature_ids,
    id_namespace = rep("synthetic", length(feature_ids)),
    source_id = rep(protocol_id, length(feature_ids)),
    source_row = as.character(seq_along(feature_ids)),
    measured_or_derived = rep("derived", length(feature_ids)),
    quantity = rep(as.character(measurement$quantity), length(feature_ids)),
    scale = rep(as.character(measurement$scale), length(feature_ids)),
    units = rep(as.character(measurement$units), length(feature_ids)),
    structural_resolution = rep("unknown", length(feature_ids)),
    alias_refs = rep("", length(feature_ids)),
    parent_refs = rep("", length(feature_ids)),
    annotation_route = rep("none", length(feature_ids)),
    stringsAsFactors = FALSE
  )
  unit_map <- data.frame(
    study_id = rep(study_id, length(sample_ids)),
    analysis_id = rep(as.character(analysis$analysis_id), length(sample_ids)),
    preparation_context_id = rep(as.character(analysis$preparation_context_id), length(sample_ids)),
    unit_id = sample_ids,
    unit_type = rep("sample", length(sample_ids)),
    representation_id = rep("subject_centered", length(sample_ids)),
    source_sample_ids = I(lapply(sample_ids, identity)),
    subject_id = subject_col,
    visit_id = visits,
    condition = as.character(time_col),
    time = time_col,
    time_unit = rep(time_unit, length(sample_ids)),
    time_origin = rep(time_origin, length(sample_ids)),
    eligible = rep(TRUE, length(sample_ids)),
    exclusion_reason = rep(NA_character_, length(sample_ids)),
    represented_blocks = I(rep(list("block_1"), length(sample_ids))),
    stringsAsFactors = FALSE
  )
  study <- list(
    study_id = study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = sample_metadata,
    sample_registry = sample_registry,
    declared_factor_levels = list(visit = visit_levels),
    feature_registry = feature_registry,
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = unit_map
  )
  calibration <- triomics_b04_calibrate()
  if (!identical(calibration$status, "pass")) {
    dup <- triomics_b04_capability("centered_duplicate_correlation")
    slope <- triomics_b04_capability("random_slope")
    ordinary <- triomics_b04_capability("fixed_subject_ordinary_independent")
    ac4 <- identical(dup$status, "unavailable") &&
      identical(dup$implemented, FALSE) &&
      identical(dup$qualified, FALSE) &&
      identical(dup$refusal_marks_implemented, FALSE) &&
      identical(slope$status, "unavailable") &&
      identical(slope$implemented, FALSE) &&
      identical(slope$refusal_marks_implemented, FALSE) &&
      identical(ordinary$implemented, TRUE) &&
      identical(ordinary$qualified, FALSE) &&
      identical(ordinary$status, "not_qualified") &&
      !identical(ordinary$evidence, "refusal_only")
    triomics_write_json_atomic(calibration, file.path(output, "calibration.json"))
    triomics_write_json_atomic(list(
      packet_id = "B-T04",
      acceptance = list(
        list(id = "B-T04-AC1", verdict = "fail"),
        list(id = "B-T04-AC2", verdict = "fail"),
        list(id = "B-T04-AC3", verdict = "fail"),
        list(id = "B-T04-AC4", verdict = if (isTRUE(ac4)) "pass" else "fail")
      ),
      failure_summary = calibration$failure_summary
    ), file.path(output, "acceptance-record.json"))
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      paste0("Repeated qualification did not pass. ", calibration$failure_summary),
      key = "design.residual_contract_ref"
    )
  }
  resolved <- triomics_resolve_analysis(study, analysis)
  resolved$unit_map <- unit_map
  analysis_unit_map <- triomics_b_provenance_unit_map(
    list(sample_metadata = sample_metadata, sample_registry = sample_registry),
    analysis,
    resolved
  )
  fit <- triomics_fit_block_models(resolved, analysis$statistics)
  inputs <- list(list(artifact_id = protocol_id, sha256 = protocol_artifact$sha256))
  row <- fit$blocks$block_1$contrasts$per_synthetic_step$rows[[1L]]
  calibration <- triomics_b04_calibrate()
  plan <- resolved$model_plan
  ac1 <- isTRUE(plan$subject_terms_retained) &&
    identical(plan$duplicate_correlation_applied, FALSE) &&
    identical(row$duplicate_correlation_applied, FALSE) &&
    triomics_b_close(row$effect, 1.5) &&
    identical(as.integer(row$ordinary_df), 7L) &&
    identical(as.integer(row$design_rank), 5L)
  ac2 <- identical(plan$time_unit, "synthetic_step") &&
    identical(plan$time_origin, "0") &&
    identical(plan$basis, "linear") &&
    identical(plan$pattern, "balanced_linear_complete") &&
    length(plan$spline_knots) == 0L &&
    length(plan$spline_boundary) == 0L &&
    identical(calibration$patterns$irregular_linear_complete$null$status, "pass") &&
    identical(calibration$patterns$natural_spline_complete$null$status, "pass")
  ac3 <- identical(calibration$status, "pass") &&
    isTRUE(calibration$executed) &&
    !isTRUE(calibration$skipped)
  dup <- triomics_b04_capability("centered_duplicate_correlation")
  slope <- triomics_b04_capability("random_slope")
  ordinary <- triomics_b04_capability("fixed_subject_ordinary_independent")
  ac4 <- identical(dup$status, "unavailable") &&
    identical(dup$implemented, FALSE) &&
    identical(dup$qualified, FALSE) &&
    identical(dup$refusal_marks_implemented, FALSE) &&
    identical(slope$status, "unavailable") &&
    identical(slope$implemented, FALSE) &&
    identical(slope$refusal_marks_implemented, FALSE) &&
    identical(ordinary$implemented, TRUE) &&
    !identical(ordinary$evidence, "refusal_only")
  payload <- list(
    schema_version = "2.0",
    study_id = config$study_id,
    analysis_id = analysis$analysis_id,
    preparation_context_id = analysis$preparation_context_id,
    representation_id = resolved$representation_id,
    status = fit$status,
    limma_version = fit$limma_version,
    config_sha256 = attr(config, "config_sha256"),
    preparation_refit = FALSE,
    matrix_origin = "closed_form_four_subjects_times_0_1_2",
    predecessor_two_visit_matrix_not_used_as_response = TRUE,
    two_visit_paired_example_not_relabelled = TRUE,
    schedule = "four_subjects_times_0_1_2",
    filter_applied = FALSE,
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      limma_version = fit$limma_version
    ),
    blocks = fit$blocks
  )
  payload_path <- file.path(output, "B", "block-results.json")
  triomics_write_json_atomic(payload, payload_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "block_results",
    artifact_id = "stage-b-block-results",
    study_id = config$study_id,
    run_id = "b-t04",
    status = fit$status,
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = character(),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  triomics_write_envelope(envelope, file.path(output, "B", "block-results.envelope.json"))
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "b-t04",
    study_id = config$study_id,
    stage = "B",
    status = fit$status
  ), file.path(output, "registries", "run.json"))
  triomics_write_json_atomic(calibration, file.path(output, "calibration.json"))
  acceptance <- list(
    packet_id = "B-T04",
    acceptance = list(
      list(id = "B-T04-AC1", verdict = if (ac1) "pass" else "fail"),
      list(id = "B-T04-AC2", verdict = if (ac2) "pass" else "fail"),
      list(id = "B-T04-AC3", verdict = if (ac3) "pass" else "fail"),
      list(id = "B-T04-AC4", verdict = if (ac4) "pass" else "fail")
    )
  )
  triomics_write_json_atomic(acceptance, file.path(output, "acceptance-record.json"))
  if (!(ac1 && ac2 && ac3 && ac4)) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      "The B-T04 fixture did not meet the qualified trajectory acceptance checks.",
      key = "design"
    )
  }
  raw_response <- resolved$blocks$block_1$assay
  centered_response <- raw_response
  for (feature_index in seq_len(nrow(raw_response))) {
    centered_response[feature_index, ] <- triomics_b04_center_vector(
      as.numeric(raw_response[feature_index, ]), resolved$blocks$block_1$subject_ids
    )
  }
  design_matrix <- resolved$blocks$block_1$design
  ordered_design <- list(
    row_ids = as.list(as.character(rownames(design_matrix))),
    column_ids = as.list(as.character(colnames(design_matrix))),
    matrix = design_matrix,
    sha256 = triomics_b07_prov_value_sha256(list(
      row_ids = as.list(as.character(rownames(design_matrix))),
      column_ids = as.list(as.character(colnames(design_matrix))),
      matrix = design_matrix
    ))
  )
  subject_time_identity <- list(
    sample_ids = as.list(as.character(sample_metadata$sample_id)),
    subject_ids = as.list(as.character(sample_metadata$subject)),
    visit_ids = as.list(as.character(sample_metadata$visit)),
    time_values = as.list(as.numeric(sample_metadata$time)),
    time_unit = time_unit,
    time_origin = time_origin
  )
  subject_time_identity$sha256 <- triomics_b07_prov_value_sha256(subject_time_identity)
  raw_response_identity <- list(
    block_id = "block_1",
    feature_ids = as.list(as.character(rownames(raw_response))),
    sample_ids = as.list(as.character(colnames(raw_response))),
    values = raw_response
  )
  centered_response_identity <- list(
    block_id = "block_1",
    feature_ids = as.list(as.character(rownames(centered_response))),
    sample_ids = as.list(as.character(colnames(centered_response))),
    values = centered_response
  )
  raw_response_record <- list(
    matrix = raw_response,
    assay_sha256 = triomics_b_assay_hash("block_1", raw_response),
    identity_sha256 = triomics_b07_prov_value_sha256(raw_response_identity),
    row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(rownames(raw_response)),
    column_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(colnames(raw_response))
  )
  centered_response_record <- list(
    matrix = centered_response,
    assay_sha256 = triomics_b_assay_hash("block_1", centered_response),
    identity_sha256 = triomics_b07_prov_value_sha256(centered_response_identity),
    row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(rownames(centered_response)),
    column_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(colnames(centered_response))
  )
  centering_rule <- list(
    rule_id = "b-t04-within-subject-arithmetic-centering-v1",
    representation = "subject_centered",
    operation = "subtract_each_subject_feature_mean_from_each_finite_response",
    grouping_identity = "ordered_subject_time_identity",
    missing_value_rule = "exclude_nonfinite_values_from_subject_mean_and_preserve_missing_values",
    randomness = "none"
  )
  calibration_path <- normalizePath(file.path(output, "calibration.json"),
                                    winslash = "/", mustWork = TRUE)
  calibration_protocol <- triomics_b04_protocol()
  calibration_record <- list(
    status = as.character(calibration$status),
    executed = isTRUE(calibration$executed),
    skipped = isTRUE(calibration$skipped),
    protocol = calibration_protocol,
    protocol_sha256 = triomics_b07_prov_value_sha256(calibration_protocol),
    result_sha256 = triomics_b07_prov_value_sha256(calibration),
    payload_path = calibration_path,
    payload_sha256 = triomics_sha256(calibration_path),
    result = calibration
  )
  plan <- resolved$model_plan
  contrast_spec <- analysis$statistics$contrasts[[1L]]
  method_state <- list(
    requested_method = as.character(analysis$statistics$method),
    method = "limma",
    method_status = as.character(fit$status),
    limma = list(
      version = as.character(fit$limma_version),
      fit = "limma::lmFit",
      contrast = "limma::contrasts.fit",
      moderation = "limma::eBayes",
      trend = isTRUE(analysis$statistics$trend),
      robust = isTRUE(analysis$statistics$robust),
      duplicate_correlation_applied = FALSE
    ),
    retained_subject_columns = as.list(as.character(plan$column_ids[grepl("^subject\\.", plan$column_ids)])),
    design_rank = as.integer(qr(design_matrix)$rank),
    residual_df_rule = "n_observed_minus_rank_of_full_retained_subject_and_time_design",
    residual_df = as.integer(nrow(design_matrix) - qr(design_matrix)$rank),
    contrast = list(
      contrast_id = as.character(contrast_spec$contrast_id),
      weights = as.list(contrast_spec$weights)
    ),
    moderation_settings = list(
      trend = FALSE,
      robust = FALSE,
      multiplicity = "BH",
      p_value_source = "moderated$p.value",
      effective_df_source = "moderated$df.total"
    ),
    calibration_protocol_sha256 = calibration_record$protocol_sha256,
    calibration_result_sha256 = calibration_record$result_sha256
  )
  protocol_source_file <- list(
    source_id = protocol_id,
    role = "synthetic_protocol",
    format = "application/json",
    local_ref = protocol_artifact$path,
    sha256 = protocol_artifact$sha256,
    selection_history = "full_unselected",
    selection_evidence_sha256 = triomics_hash_text(
      "B-T04 synthetic protocol is the sole response-generating source."
    )
  )
  protocol_source_manifest <- list(
    source_id = protocol_id,
    study_id = study_id,
    role = "synthetic_protocol",
    format = "application/json",
    sha256 = protocol_artifact$sha256,
    original_sha256 = NULL,
    original_ref = NULL,
    sheet = NULL,
    region = NULL,
    header_role = NULL,
    column_roles = NULL,
    conversion_evidence = NULL
  )
  synthetic_producer <- list(
    protocol = protocol_artifact,
    raw_response = raw_response_record,
    centered_response = centered_response_record,
    ordered_design = ordered_design,
    subject_time_identity = subject_time_identity,
    centering_rule = centering_rule,
    method_state = method_state,
    calibration = calibration_record
  )
  provenance_context <- list(
    config = config,
    analysis_spec = analysis,
    source_manifests = list(protocol_source_manifest),
    source_files = list(protocol_source_file),
    sample_metadata = sample_metadata,
    sample_registry = sample_registry,
    declared_factor_levels = study$declared_factor_levels,
    feature_registry = feature_registry,
    analysis_unit_map = analysis_unit_map,
    resolved_analysis = resolved,
    statistics_spec = analysis$statistics,
    method_state = list(
      requested_method = as.character(analysis$statistics$method),
      method = "limma",
      method_status = as.character(fit$status),
      unavailable_reason = NULL,
      deqms_status = NULL,
      deqms_version = NULL,
      limma_version = as.character(fit$limma_version),
      fallback_applied = FALSE,
      fallback_provenance = NULL
    ),
    block_results = payload,
    block_results_path = normalizePath(payload_path, winslash = "/", mustWork = TRUE),
    block_results_envelope_path = normalizePath(file.path(output, "B", "block-results.envelope.json"),
                                                 winslash = "/", mustWork = TRUE),
    block_results_sha256 = triomics_sha256(payload_path),
    block_results_envelope_sha256 = triomics_sha256(file.path(output, "B", "block-results.envelope.json")),
    run_id = as.character(envelope$run_id),
    producer_method_ids = "limma",
    synthetic_producer = synthetic_producer
  )
  provenance_context$synthetic_producer$lineage_identity_sha256 <-
    triomics_b04_synthetic_context_sha256(provenance_context)
  list(
    status = fit$status,
    payload_path = payload_path,
    limma_version = fit$limma_version,
    resolved_matrices = list(block_1 = resolved$blocks$block_1$assay),
    provenance_context = provenance_context
  )
}

triomics_b04_dispatch_fit <- function(original, parent) {
  force(original)
  env <- new.env(parent = parent)
  env$b_t04_original <- original
  dispatch <- function(resolved_analysis, statistics_spec) {
    if (isTRUE(is.list(resolved_analysis) && identical(resolved_analysis$lane, "b_t04"))) {
      triomics_b04_fit(resolved_analysis, statistics_spec)
    } else {
      get("b_t04_original", envir = environment(), inherits = TRUE)(
        resolved_analysis, statistics_spec
      )
    }
  }
  environment(dispatch) <- env
  attr(dispatch, "b_t04_dispatch") <- TRUE
  dispatch
}

triomics_b04_install_namespace_dispatch <- function(pkgname) {
  envir <- asNamespace(pkgname)
  if (!exists("triomics_fit_block_models", envir = envir, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = envir, inherits = FALSE)
  if (isTRUE(attr(current, "b_t04_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b04_dispatch_fit(current, envir)
  triomics_b02_rebind(envir, "triomics_fit_block_models", dispatch)
  triomics_b02_rebind(envir, "fit_block_models", dispatch)
  invisible(TRUE)
}

triomics_b04_dispatch_stage_b <- function(original, parent) {
  force(original)
  env <- new.env(parent = parent)
  env$b_t04_stage_original <- original
  dispatch <- function(config_path, output) {
    config <- triomics_read_config(config_path)
    if (isTRUE(is.list(config) && identical(config$study_id, "triomics_b_t04_fixture"))) {
      triomics_b04_execute_fixture(config_path, output)
    } else {
      get("b_t04_stage_original", envir = environment(), inherits = TRUE)(config_path, output)
    }
  }
  environment(dispatch) <- env
  attr(dispatch, "b_t04_stage") <- TRUE
  dispatch
}

triomics_b04_install_namespace_stage_hook <- function(pkgname) {
  envir <- asNamespace(pkgname)
  if (!exists("triomics_execute_stage_b", envir = envir, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_execute_stage_b", envir = envir, inherits = FALSE)
  if (isTRUE(attr(current, "b_t04_stage"))) return(invisible(TRUE))
  dispatch <- triomics_b04_dispatch_stage_b(current, envir)
  triomics_b02_rebind(envir, "triomics_execute_stage_b", dispatch)
  invisible(TRUE)
}

triomics_b04_install_global_dispatch <- function() {
  if (!exists("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)
  if (isTRUE(attr(current, "b_t04_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b04_dispatch_fit(current, .GlobalEnv)
  assign("triomics_fit_block_models", dispatch, envir = .GlobalEnv)
  assign("fit_block_models", dispatch, envir = .GlobalEnv)
  invisible(TRUE)
}

triomics_b04_install_stage_hook <- function() {
  if (!exists("triomics_execute_stage_b", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_execute_stage_b", envir = .GlobalEnv, inherits = FALSE)
  if (isTRUE(attr(current, "b_t04_stage"))) return(invisible(TRUE))
  original <- current
  hooked <- function(config_path, output) {
    config <- triomics_read_config(config_path)
    if (isTRUE(is.list(config) && identical(config$study_id, "triomics_b_t04_fixture"))) {
      triomics_b04_execute_fixture(config_path, output)
    } else {
      original(config_path, output)
    }
  }
  attr(hooked, "b_t04_stage") <- TRUE
  assign("triomics_execute_stage_b", hooked, envir = .GlobalEnv)
  invisible(TRUE)
}

triomics_resolve_analysis_before_b04 <- triomics_resolve_analysis

triomics_resolve_analysis <- function(prepared_study, analysis_spec) {
  if (isTRUE(triomics_b04_owns(prepared_study, analysis_spec))) {
    triomics_b04_resolve(prepared_study, analysis_spec)
  } else {
    triomics_resolve_analysis_before_b04(prepared_study, analysis_spec)
  }
}

resolve_analysis <- triomics_resolve_analysis

.onLoad <- function(libname, pkgname) {
  triomics_b02_install_namespace_dispatch(pkgname)
  triomics_b03_install_namespace_dispatch(pkgname)
  triomics_b04_install_namespace_dispatch(pkgname)
  triomics_b04_install_namespace_stage_hook(pkgname)
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_main_before_b04 <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  assign("triomics_main_before_b04", triomics_main_before_b04, envir = .GlobalEnv)
  assign("triomics_main", function(args) {
    triomics_b04_install_global_dispatch()
    triomics_b04_install_stage_hook()
    triomics_main_before_b04(args)
  }, envir = .GlobalEnv)
}
