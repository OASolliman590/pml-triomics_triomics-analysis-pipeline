# Paired two-visit delta. Fit dispatch is installed from .onLoad and the CLI
# entry, not at source time: statistics_limma.R is sourced later and assigns
# triomics_fit_block_models. This file replaces design_levels.R's .onLoad.

triomics_b03_owns <- function(prepared_study, analysis_spec) {
  design <- if (is.list(analysis_spec)) analysis_spec$design else NULL
  if (is.null(design) || !is.list(design)) return(FALSE)
  if (!identical(design$kind, "paired") || !isTRUE(design$repeated)) return(FALSE)
  if (!identical(design$representation, "delta")) return(FALSE)
  if (!triomics_b_string(design$subject_id) || !triomics_b_string(design$visit_column)) return(FALSE)
  if (!triomics_b_string(design$reference_visit) || !triomics_b_string(design$target_visit)) return(FALSE)
  TRUE
}

triomics_b03_declared_levels <- function(prepared_study, column, key) {
  declared <- if (is.list(prepared_study$declared_factor_levels)) {
    prepared_study$declared_factor_levels[[column]]
  } else {
    NULL
  }
  if (is.null(declared)) {
    triomics_b_abort("B_DESIGN_LEVELS", "The factor has no declared levels.", key = key)
  }
  levels <- triomics_b_characters(declared, "factor_levels")
  if (length(levels) == 0L || anyDuplicated(levels) || any(!nzchar(levels)) || anyNA(levels)) {
    triomics_b_abort("B_DESIGN_LEVELS", "Declared factor levels must be unique and non-empty.", key = key)
  }
  vapply(levels, triomics_b02_token, character(1), key = key)
}

triomics_b03_required_unit <- function(scale) {
  if (identical(scale, "log2")) return("log2_fold_change")
  if (scale %in% c("clr_ln", "clr_log2")) return("clr_contrast")
  if (scale %in% c("linear", "ln", "variance_stabilized")) return("delta_difference")
  ""
}

triomics_b03_pair_block <- function(block_id, assay, prepared_study, design, visit_pair, group_levels) {
  original <- triomics_b02_validate_assay(assay)
  triomics_b02_hash_check(block_id, original, prepared_study$assay_hashes)
  present <- colnames(original)
  metadata <- prepared_study$sample_metadata
  subjects <- as.character(triomics_b02_align(metadata, present, design$subject_id, "design.subject_id"))
  visits <- as.character(triomics_b02_align(metadata, present, design$visit_column, "design.visit_column"))
  names(subjects) <- present
  names(visits) <- present
  if (any(is.na(subjects) | !nzchar(subjects))) {
    triomics_b_abort(
      "B_DESIGN_PAIRING",
      "Pairing is not guessed from column order.",
      key = "design.subject_id"
    )
  }
  if (any(is.na(visits) | !nzchar(visits))) {
    triomics_b_abort(
      "B_DESIGN_PAIRING",
      "A missing visit is not guessed from column order.",
      key = "design.visit_column"
    )
  }
  if (anyDuplicated(paste(subjects, visits, sep = "\r")) > 0L) {
    triomics_b_abort(
      "B_DESIGN_PAIRING",
      "A duplicate visit is not guessed or averaged.",
      key = "design.visit_column"
    )
  }
  allowed <- c(visit_pair$reference, visit_pair$target)
  if (any(!visits %in% allowed)) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "A visit outside the declared reference and target is not dropped by proximity.",
      key = "design.visit_column"
    )
  }
  group_values <- NULL
  if (!is.null(group_levels)) {
    group_values <- as.character(triomics_b02_align(
      metadata, present, design$between_group, "design.between_group"
    ))
    names(group_values) <- present
    if (any(is.na(group_values) | !nzchar(group_values)) || any(!group_values %in% group_levels)) {
      triomics_b_abort(
        "B_DESIGN_LEVELS",
        "A sample group is outside the declared between-group levels.",
        key = "design.between_group"
      )
    }
  }
  all_subjects <- sort(unique(subjects))
  ref_sample <- stats::setNames(rep(NA_character_, length(all_subjects)), all_subjects)
  tgt_sample <- ref_sample
  reasons <- stats::setNames(rep(NA_character_, length(all_subjects)), all_subjects)
  group_by_subject <- if (is.null(group_levels)) {
    NULL
  } else {
    stats::setNames(rep(NA_character_, length(all_subjects)), all_subjects)
  }
  for (subject in all_subjects) {
    chosen <- present[subjects == subject]
    chosen_visits <- visits[chosen]
    ref_hits <- chosen[chosen_visits == visit_pair$reference]
    tgt_hits <- chosen[chosen_visits == visit_pair$target]
    if (length(ref_hits) == 1L) ref_sample[[subject]] <- ref_hits
    if (length(tgt_hits) == 1L) tgt_sample[[subject]] <- tgt_hits
    if (length(ref_hits) == 1L && length(tgt_hits) == 1L) {
      reasons[[subject]] <- NA_character_
    } else if (length(tgt_hits) == 0L) {
      reasons[[subject]] <- "missing_target_visit"
    } else {
      reasons[[subject]] <- "missing_reference_visit"
    }
    if (!is.null(group_by_subject)) {
      groups <- unique(group_values[chosen])
      if (length(groups) != 1L) {
        triomics_b_abort(
          "B_DESIGN_PAIRING",
          "A within-subject group change is not guessed into one group.",
          key = "design.between_group"
        )
      }
      group_by_subject[[subject]] <- groups[[1L]]
    }
  }
  retained <- all_subjects[is.na(reasons)]
  excluded <- all_subjects[!is.na(reasons)]
  if (identical(design$missing_visits, "error") && length(excluded) > 0L) {
    triomics_b_abort(
      "B_DESIGN_PAIRING",
      paste0(
        "An incomplete pair would change the population from ",
        length(all_subjects), " subjects to ", length(retained),
        " subjects. Missing visits are not imputed and pairing is not guessed."
      ),
      key = "design.missing_visits"
    )
  }
  if (length(retained) == 0L) {
    triomics_b_abort(
      "B_DESIGN_PAIRING",
      paste0(
        "No complete pairs remain. The population would change from ",
        length(all_subjects),
        " subjects to 0 subjects. Missing visits are not imputed and pairing is not guessed."
      ),
      key = "design.missing_visits"
    )
  }
  delta <- matrix(
    NA_real_, nrow(original), length(retained),
    dimnames = list(rownames(original), retained)
  )
  for (subject in retained) {
    delta[, subject] <- as.numeric(original[, tgt_sample[[subject]]]) -
      as.numeric(original[, ref_sample[[subject]]])
  }
  list(
    block_id = block_id,
    delta = delta,
    present = present,
    all_subjects = all_subjects,
    retained = retained,
    excluded = excluded,
    reasons = reasons,
    ref_sample = ref_sample,
    tgt_sample = tgt_sample,
    group_by_subject = group_by_subject
  )
}

triomics_b03_covariate_column <- function(item, paired, metadata_values) {
  form <- item$form
  if (!form %in% c("level", "between", "delta", "within")) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Covariate form is not a declared transformation.",
      key = "design.covariates"
    )
  }
  per_block <- vector("list", length(paired))
  names(per_block) <- vapply(paired, function(block) block$block_id, character(1))
  time_varying <- FALSE
  for (block in paired) {
    values <- metadata_values
    reference <- numeric(length(block$retained))
    target <- reference
    names(reference) <- block$retained
    names(target) <- block$retained
    for (subject in block$retained) {
      reference[[subject]] <- values[[block$ref_sample[[subject]]]]
      target[[subject]] <- values[[block$tgt_sample[[subject]]]]
    }
    if (any(!is.finite(reference)) || any(!is.finite(target))) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "Missing covariates are an error and are not imputed.",
        key = "design.covariates"
      )
    }
    if (any(reference != target)) time_varying <- TRUE
    per_block[[block$block_id]] <- list(reference = reference, target = target)
  }
  columns <- vector("list", length(paired))
  names(columns) <- names(per_block)
  if (!time_varying) {
    if (form %in% c("delta", "within")) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "A static covariate is a modifier of change, not a within-person main effect.",
        key = "design.covariates"
      )
    }
    transformation <- if (identical(form, "between")) "static_between" else "static_level"
    for (block in paired) {
      columns[[block$block_id]] <- per_block[[block$block_id]]$reference
    }
  } else if (identical(form, "level")) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "A time-varying level is a projected-out level, not a declared transformation.",
      key = "design.covariates"
    )
  } else {
    transformation <- if (identical(form, "delta")) {
      "delta"
    } else if (identical(form, "within")) {
      "within_target_minus_subject_mean"
    } else {
      "between_subject_mean"
    }
    for (block in paired) {
      reference <- per_block[[block$block_id]]$reference
      target <- per_block[[block$block_id]]$target
      subject_mean <- (reference + target) / 2
      columns[[block$block_id]] <- if (identical(form, "delta")) {
        target - reference
      } else if (identical(form, "within")) {
        target - subject_mean
      } else {
        subject_mean
      }
    }
  }
  list(
    record = list(
      variable = item$variable,
      role = if (triomics_b_string(item$role)) item$role else item$role,
      form = form,
      interpretation = "modifier_of_change",
      within_person_main_effect = FALSE,
      time_varying = time_varying,
      transformation = transformation
    ),
    columns = columns
  )
}

triomics_b03_resolve <- function(prepared_study, analysis_spec) {
  design <- analysis_spec$design
  if (!triomics_b_empty(design$interactions)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Interactions are not expanded from names or evaluated as R code.",
      key = "design.interactions"
    )
  }
  if (triomics_b_string(design$time_column) ||
      !triomics_b_empty(design$spline_knots) ||
      !triomics_b_empty(design$spline_boundary)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "A time column or spline is not this two-visit delta.",
      key = "design.time_column"
    )
  }
  if (!triomics_b_empty(design$mean_weights)) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "A level-retaining mean weight is not recoverable from a subject delta.",
      key = "design.mean_weights"
    )
  }
  if (!identical(design$condition_type, "factor")) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Paired delta uses a declared visit factor.",
      key = "design.condition_type"
    )
  }
  if (!design$missing_visits %in% c("error", "complete_pairs_per_contrast")) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Missing visits are either an error or an explicit complete-pair population.",
      key = "design.missing_visits"
    )
  }
  reference_visit <- triomics_b02_token(design$reference_visit, "design.reference_visit")
  target_visit <- triomics_b02_token(design$target_visit, "design.target_visit")
  if (identical(reference_visit, target_visit)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "The reference and target visits are identical, so a subject delta is not defined.",
      key = "design.target_visit"
    )
  }
  visit_levels <- triomics_b03_declared_levels(prepared_study, design$visit_column, "design.visit_column")
  if (!reference_visit %in% visit_levels || !target_visit %in% visit_levels) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "The delta target requiring a removed level is not estimable.",
      key = "design.target_visit"
    )
  }
  if (length(visit_levels) != 2L || !setequal(visit_levels, c(reference_visit, target_visit))) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "More than two declared visits are a trajectory, not this paired delta.",
      key = "design.visit_column"
    )
  }
  between <- if (triomics_b_string(design$between_group)) design$between_group else NULL
  group_levels <- NULL
  if (is.null(between)) {
    if (!identical(design$condition, design$visit_column) ||
        !identical(design$reference_level, reference_visit) ||
        !identical(design$target_level, target_visit)) {
      triomics_b_abort(
        "B_DESIGN_LEVELS",
        "The within-person contrast must use the declared visit endpoints on a subject delta.",
        key = "design.condition"
      )
    }
    estimand <- "within_person_change"
  } else {
    between <- triomics_b02_token(between, "design.between_group")
    if (!identical(design$condition, between) && !identical(design$condition, design$visit_column)) {
      triomics_b_abort(
        "B_DESIGN_LEVELS",
        "The difference-in-change condition must be the between-group factor or the visit column.",
        key = "design.condition"
      )
    }
    group_levels <- triomics_b03_declared_levels(prepared_study, between, "design.between_group")
    if (!triomics_b_string(design$reference_level) || !triomics_b_string(design$target_level) ||
        !design$reference_level %in% group_levels || !design$target_level %in% group_levels) {
      triomics_b_abort(
        "B_DESIGN_LEVELS",
        "The delta target requiring a removed level is not estimable.",
        key = "design.target_level"
      )
    }
    estimand <- "difference_in_change"
  }
  triomics_b02_check_map(prepared_study, "delta")
  if (!triomics_b_string(analysis_spec$analysis_id)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The analysis id is required.", key = "analysis_id")
  }
  block_ids <- triomics_b_characters(analysis_spec$block_ids, "block_ids")
  assays <- prepared_study$assays
  if (length(block_ids) == 0L || is.null(assays) || any(!block_ids %in% names(assays))) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Every analysis block must already be in the prepared study.",
      key = "block_ids"
    )
  }
  visit_pair <- list(reference = reference_visit, target = target_visit)
  paired <- lapply(block_ids, function(block_id) {
    triomics_b03_pair_block(
      block_id, assays[[block_id]], prepared_study, design, visit_pair, group_levels
    )
  })
  names(paired) <- block_ids
  raw_covariates <- design$covariates
  if (triomics_b_empty(raw_covariates)) raw_covariates <- list()
  if (!is.list(raw_covariates)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Covariates must be declared records.", key = "design.covariates")
  }
  covariate_records <- vector("list", length(raw_covariates))
  covariate_columns <- vector("list", length(raw_covariates))
  covariate_names <- character(length(raw_covariates))
  for (i in seq_along(raw_covariates)) {
    item <- raw_covariates[[i]]
    if (!is.list(item) || !triomics_b_string(item$variable) || !triomics_b_string(item$form)) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "Each covariate needs a variable and a form.",
        key = "design.covariates"
      )
    }
    variable <- triomics_b02_token(item$variable, "design.covariates")
    item$variable <- variable
    covariate_names[[i]] <- variable
    covariate_records[[i]] <- item
  }
  if (anyDuplicated(covariate_names)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Covariate names must be unique.", key = "design.covariates")
  }
  if (length(covariate_names) > 0L) {
    for (i in seq_along(covariate_records)) {
      variable <- covariate_names[[i]]
      per_block_values <- lapply(paired, function(block) {
        aligned <- triomics_b02_align(
          prepared_study$sample_metadata, block$present, variable, "design.covariates"
        )
        if (is.character(aligned) || is.factor(aligned)) {
          triomics_b_abort(
            "B_DESIGN_UNSUPPORTED",
            "A factor or label is not coerced into a continuous term.",
            key = "design.covariates"
          )
        }
        values <- suppressWarnings(as.numeric(aligned))
        stats::setNames(values, block$present)
      })
      record_and_columns <- triomics_b03_covariate_values(covariate_records[[i]], paired, per_block_values)
      covariate_records[[i]] <- record_and_columns$record
      covariate_columns[[i]] <- record_and_columns$columns
    }
  }
  modifier_ids <- if (length(covariate_names) == 0L) character() else paste0("modifier.", covariate_names)
  if (is.null(between)) {
    column_ids <- c("intercept", modifier_ids)
    formula <- if (length(modifier_ids) == 0L) {
      "~1"
    } else {
      paste0("~1 + ", paste(modifier_ids, collapse = " + "))
    }
  } else {
    column_ids <- c(paste0("between_group.", group_levels), modifier_ids)
    formula <- paste0("~0 + ", paste(column_ids, collapse = " + "))
  }
  visit_weights <- stats::setNames(c(-1, 1), c(reference_visit, target_visit))
  registry <- prepared_study$feature_registry
  blocks <- vector("list", length(block_ids))
  names(blocks) <- block_ids
  for (block_id in block_ids) {
    block <- paired[[block_id]]
    columns <- list()
    if (is.null(between)) {
      columns$intercept <- rep(1, length(block$retained))
    } else {
      retained_groups <- block$group_by_subject[block$retained]
      for (level in group_levels) {
        columns[[paste0("between_group.", level)]] <- as.numeric(retained_groups == level)
      }
    }
    for (i in seq_along(modifier_ids)) {
      columns[[modifier_ids[[i]]]] <- as.numeric(covariate_columns[[i]][[block_id]][block$retained])
    }
    per_group <- NULL
    if (!is.null(group_levels)) {
      per_group <- lapply(stats::setNames(group_levels, group_levels), function(level) {
        members <- names(block$group_by_subject)[block$group_by_subject == level]
        list(
          n_starting = as.integer(length(members)),
          n_retained = as.integer(sum(members %in% block$retained)),
          n_excluded = as.integer(sum(members %in% block$excluded)),
          excluded_subject_ids = as.character(intersect(block$excluded, members))
        )
      })
    }
    attrition <- list(
      policy = design$missing_visits,
      population_changed = length(block$excluded) > 0L,
      imputed = FALSE,
      pairing = "subject_id_and_declared_visits",
      n_subjects_starting = as.integer(length(block$all_subjects)),
      n_subjects_retained = as.integer(length(block$retained)),
      n_subjects_excluded = as.integer(length(block$excluded)),
      excluded_subject_ids = as.character(block$excluded),
      n_complete_pairs = as.integer(length(block$retained)),
      n_observations_starting = as.integer(length(block$present)),
      n_observations_retained = as.integer(2L * length(block$retained)),
      per_group = per_group
    )
    unit_map <- lapply(block$all_subjects, function(subject) {
      retained_flag <- subject %in% block$retained
      samples <- c(block$ref_sample[[subject]], block$tgt_sample[[subject]])
      samples <- samples[!is.na(samples) & nzchar(samples)]
      list(
        unit_id = subject,
        unit_type = "subject_delta",
        representation_id = "delta",
        eligibility = if (retained_flag) "retained" else "excluded",
        exclusion_reason = if (retained_flag) NULL else unname(block$reasons[[subject]]),
        source_sample_ids = as.character(samples),
        subject_id = subject,
        study_id = prepared_study$study_id,
        analysis_id = analysis_spec$analysis_id,
        preparation_context_id = analysis_spec$preparation_context_id,
        visit_weights = visit_weights
      )
    })
    feature_keys <- rownames(block$delta)
    scales <- vapply(feature_keys, triomics_b_scale, character(1), registry = registry, block_id = block_id)
    blocks[[block_id]] <- list(
      block_id = block_id,
      assay = block$delta,
      signed_delta_map = block$delta,
      centered = FALSE,
      decomposition = "target_minus_reference_once",
      decomposition_count = 1L,
      sample_ids = colnames(block$delta),
      unit_ids = colnames(block$delta),
      subjects = block$retained,
      group_by_subject = block$group_by_subject,
      excluded_subjects = block$excluded,
      feature_keys = feature_keys,
      scales = unname(scales),
      source_hash = triomics_b_assay_hash(block_id, block$delta),
      design = triomics_b02_design_matrix(colnames(block$delta), column_ids, columns),
      n_analysis_rows = as.integer(length(block$retained)),
      n_source_samples = as.integer(2L * length(block$retained)),
      unit_map = unit_map,
      attrition = attrition
    )
  }
  maps <- lapply(blocks, function(block) block$assay)
  list(
    lane = "b_t03",
    study_id = prepared_study$study_id,
    analysis_id = analysis_spec$analysis_id,
    preparation_context_id = analysis_spec$preparation_context_id,
    representation = "delta",
    representation_id = "delta",
    model_plan = list(
      formula = formula,
      column_ids = column_ids,
      estimand = estimand,
      covariates = covariate_records,
      visit_column = design$visit_column,
      visit_levels = c(reference_visit, target_visit),
      reference_visit = reference_visit,
      target_visit = target_visit,
      reference_level = design$reference_level,
      target_level = design$target_level,
      between_group = between,
      group_levels = group_levels
    ),
    blocks = blocks,
    subjects = blocks[[block_ids[[1L]]]]$subjects,
    unit_map = blocks[[block_ids[[1L]]]]$unit_map,
    c_input = list(
      centered = FALSE,
      decomposition = "target_minus_reference_once",
      decomposition_count = 1L,
      maps = maps
    )
  )
}

triomics_b03_covariate_values <- function(item, paired, per_block_values) {
  form <- item$form
  if (!form %in% c("level", "between", "delta", "within")) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Covariate form is not a declared transformation.",
      key = "design.covariates"
    )
  }
  time_varying <- FALSE
  checked <- vector("list", length(paired))
  names(checked) <- names(per_block_values)
  for (block in paired) {
    values <- per_block_values[[block$block_id]]
    reference <- numeric(length(block$retained))
    target <- reference
    names(reference) <- block$retained
    names(target) <- block$retained
    for (subject in block$retained) {
      reference[[subject]] <- values[[block$ref_sample[[subject]]]]
      target[[subject]] <- values[[block$tgt_sample[[subject]]]]
    }
    if (any(!is.finite(reference)) || any(!is.finite(target))) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "Missing covariates are an error and are not imputed.",
        key = "design.covariates"
      )
    }
    if (any(reference != target)) time_varying <- TRUE
    checked[[block$block_id]] <- list(reference = reference, target = target)
  }
  columns <- vector("list", length(paired))
  names(columns) <- names(checked)
  if (!time_varying) {
    if (form %in% c("delta", "within")) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "A static covariate is a modifier of change, not a within-person main effect.",
        key = "design.covariates"
      )
    }
    transformation <- if (identical(form, "between")) "static_between" else "static_level"
    for (block in paired) columns[[block$block_id]] <- checked[[block$block_id]]$reference
  } else if (identical(form, "level")) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "A time-varying level is a projected-out level, not a declared transformation.",
      key = "design.covariates"
    )
  } else {
    transformation <- if (identical(form, "delta")) {
      "delta"
    } else if (identical(form, "within")) {
      "within_target_minus_subject_mean"
    } else {
      "between_subject_mean"
    }
    for (block in paired) {
      reference <- checked[[block$block_id]]$reference
      target <- checked[[block$block_id]]$target
      subject_mean <- (reference + target) / 2
      columns[[block$block_id]] <- if (identical(form, "delta")) {
        target - reference
      } else if (identical(form, "within")) {
        target - subject_mean
      } else {
        subject_mean
      }
    }
  }
  list(
    record = list(
      variable = item$variable,
      role = item$role,
      form = form,
      interpretation = "modifier_of_change",
      within_person_main_effect = FALSE,
      time_varying = time_varying,
      transformation = transformation
    ),
    columns = columns
  )
}

triomics_b03_reject_projected <- function(contrast, plan) {
  weights <- contrast$weights
  if (is.null(weights) || is.null(names(weights))) return(invisible(TRUE))
  visits <- plan$visit_levels
  forbidden <- unique(c(
    visits,
    paste0("condition.", visits),
    paste0("visit.", visits),
    if (triomics_b_string(plan$visit_column)) paste0(plan$visit_column, ".", visits) else character()
  ))
  if (any(names(weights) %in% forbidden & !names(weights) %in% plan$column_ids)) {
    triomics_b_abort(
      "B_DESIGN_LEVELS",
      "Projected-out level targets are not estimable from a subject delta.",
      key = "statistics.contrasts"
    )
  }
  invisible(TRUE)
}

triomics_b03_contrast_kind <- function(weights) {
  nonzero <- abs(weights) > 1e-8
  if (any(nonzero & startsWith(names(weights), "between_group."))) return("difference_in_change")
  if (any(nonzero & names(weights) == "intercept")) return("within_person_change")
  "modifier_of_change"
}

triomics_b03_sign_counts <- function(values, effect, estimated) {
  finite <- is.finite(values)
  nonzero <- finite & abs(values) > 1e-8
  zeros <- sum(finite & abs(values) <= 1e-8)
  if (!any(finite)) {
    return(list(status = "no_eligible_contrasts", numerator = NULL, denominator = NULL, zeros = zeros))
  }
  if (!isTRUE(estimated) || length(effect) != 1L || !is.finite(effect)) {
    return(list(status = "not_estimable", numerator = NULL, denominator = NULL, zeros = zeros))
  }
  if (!any(nonzero)) {
    return(list(status = "all_contrasts_zero", numerator = NULL, denominator = NULL, zeros = zeros))
  }
  if (abs(effect) <= 1e-8) {
    return(list(status = "no_effect_direction", numerator = NULL, denominator = NULL, zeros = zeros))
  }
  chosen <- values[nonzero]
  list(
    status = "paired_sign_agreement",
    numerator = as.integer(sum(sign(chosen) == sign(effect))),
    denominator = as.integer(sum(nonzero)),
    zeros = as.integer(zeros)
  )
}

triomics_b03_direction <- function(y, block, plan, kind, effect, estimated, adjusted) {
  retained <- colnames(block$assay)
  names(y) <- retained
  finite <- is.finite(y)
  n_zero <- as.integer(sum(finite & abs(y) <= 1e-8))
  n_unavailable <- as.integer(sum(!finite) + length(block$excluded_subjects))
  if (identical(kind, "modifier_of_change")) {
    return(list(
      direction_status = "not_applicable",
      direction_reason = "modifier_coefficient_is_not_a_paired_change_direction",
      direction_numerator = NULL,
      direction_denominator = NULL,
      direction_zero_count = n_zero,
      direction_unavailable_count = n_unavailable,
      direction_groups = NULL
    ))
  }
  if (identical(kind, "difference_in_change")) {
    groups <- list()
    levels <- plan$group_levels
    if (is.null(levels)) levels <- character()
    for (level in levels) {
      members <- names(block$group_by_subject)[block$group_by_subject == level]
      retained_members <- intersect(retained, members)
      excluded_members <- intersect(block$excluded_subjects, members)
      member_values <- y[retained_members]
      finite_values <- member_values[is.finite(member_values)]
      unavailable <- as.integer(sum(!is.finite(member_values)) + length(excluded_members))
      zeros <- as.integer(sum(is.finite(member_values) & abs(member_values) <= 1e-8))
      if (length(finite_values) == 0L) {
        groups[[level]] <- list(
          direction_status = "no_eligible_contrasts",
          direction_numerator = NULL,
          direction_denominator = NULL,
          direction_zero_count = zeros,
          direction_unavailable_count = unavailable
        )
      } else {
        center <- mean(finite_values)
        nonzero <- finite_values[abs(finite_values) > 1e-8]
        if (length(nonzero) == 0L || !is.finite(center) || abs(center) <= 1e-8) {
          groups[[level]] <- list(
            direction_status = if (length(nonzero) == 0L) "all_contrasts_zero" else "no_effect_direction",
            direction_numerator = NULL,
            direction_denominator = NULL,
            direction_zero_count = zeros,
            direction_unavailable_count = unavailable
          )
        } else {
          groups[[level]] <- list(
            direction_status = "paired_sign_agreement",
            direction_numerator = as.integer(sum(sign(nonzero) == sign(center))),
            direction_denominator = as.integer(length(nonzero)),
            direction_zero_count = zeros,
            direction_unavailable_count = unavailable
          )
        }
      }
    }
    return(list(
      direction_status = "within_group_change",
      direction_reason = "difference_in_change_is_not_a_per_subject_cross_group_delta",
      direction_numerator = NULL,
      direction_denominator = NULL,
      direction_zero_count = n_zero,
      direction_unavailable_count = n_unavailable,
      direction_groups = groups
    ))
  }
  counted <- triomics_b03_sign_counts(y, effect, estimated)
  reason <- if (!identical(counted$status, "paired_sign_agreement")) {
    if (identical(counted$status, "no_eligible_contrasts")) {
      "no_finite_subject_delta"
    } else if (identical(counted$status, "not_estimable")) {
      "failed_contrast_has_no_estimated_sign"
    } else if (identical(counted$status, "all_contrasts_zero")) {
      "all_finite_subject_deltas_are_zero"
    } else {
      "estimated_change_has_no_sign"
    }
  } else if (adjusted) {
    "unadjusted_paired_sign_agreement_with_adjusted_effect"
  } else {
    "subject_delta_sign_agrees_with_estimated_change"
  }
  list(
    direction_status = counted$status,
    direction_reason = reason,
    direction_numerator = counted$numerator,
    direction_denominator = counted$denominator,
    direction_zero_count = n_zero,
    direction_unavailable_count = n_unavailable,
    direction_groups = NULL
  )
}

triomics_b03_wilcoxon <- function(block, weights) {
  orientation <- 1
  if ("intercept" %in% names(weights) && is.finite(weights[["intercept"]]) && weights[["intercept"]] < 0) {
    orientation <- -1
  }
  features <- vector("list", length(block$feature_keys))
  for (i in seq_along(block$feature_keys)) {
    values <- as.numeric(block$assay[i, ])
    values <- values[is.finite(values)]
    oriented <- orientation * values
    n_zero <- as.integer(sum(abs(oriented) <= 1e-8))
    nonzero_abs <- abs(oriented[abs(oriented) > 1e-8])
    n_ties <- if (length(nonzero_abs) == 0L) 0L else as.integer(sum(duplicated(nonzero_abs)))
    p_value <- NULL
    if (length(oriented) > 0L && any(abs(oriented) > 1e-8)) {
      tested <- tryCatch(
        suppressWarnings(stats::wilcox.test(
          oriented, mu = 0, exact = FALSE, correct = TRUE, alternative = "two.sided"
        )),
        error = function(error) NULL
      )
      if (!is.null(tested) && length(tested$p.value) == 1L && is.finite(tested$p.value)) {
        p_value <- unname(as.numeric(tested$p.value))
      }
    }
    features[[i]] <- list(
      feature_key = block$feature_keys[[i]],
      p = p_value,
      n = as.integer(length(oriented)),
      n_zero_differences = n_zero,
      n_tied_nonzero_absolute = n_ties
    )
  }
  list(
    status = "complete",
    method = "wilcoxon_signed_rank",
    exact = FALSE,
    continuity_correction = TRUE,
    substitutes_for_primary = FALSE,
    equivalent_to_moderated_test = FALSE,
    features = features
  )
}

triomics_b03_sensitivity <- function(statistics_spec, plan, block, weights, kind) {
  if (!isTRUE(statistics_spec$rank_sensitivity)) return(list(status = "not_requested"))
  adjusted <- length(plan$covariates) > 0L
  grouped <- identical(plan$estimand, "difference_in_change")
  if (adjusted || grouped || !identical(kind, "within_person_change")) {
    return(list(
      status = "unavailable",
      method = NULL,
      reason = "rank_sensitivity_is_unavailable_for_adjusted_or_difference_in_change",
      substitutes_for_primary = FALSE,
      equivalent_to_moderated_test = FALSE,
      features = NULL
    ))
  }
  triomics_b03_wilcoxon(block, weights)
}

triomics_b03_fit <- function(resolved_analysis, statistics_spec) {
  if (identical(statistics_spec$method, "deqms")) {
    version <- if (requireNamespace("limma", quietly = TRUE)) as.character(packageVersion("limma")) else NULL
    return(list(
      status = "unavailable",
      fallback_applied = FALSE,
      blocks = list(),
      effects = NULL,
      limma_version = version
    ))
  }
  if (!identical(statistics_spec$method, "limma")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane fits limma or reports DEqMS as unavailable.",
                     key = "statistics.method")
  }
  if (!identical(statistics_spec$trend, FALSE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma trend stays false in this lane.", key = "statistics.trend")
  }
  if (!is.logical(statistics_spec$robust) || length(statistics_spec$robust) != 1L ||
      is.na(statistics_spec$robust)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma robust must be true or false.", key = "statistics.robust")
  }
  if (!is.logical(statistics_spec$rank_sensitivity) || length(statistics_spec$rank_sensitivity) != 1L ||
      is.na(statistics_spec$rank_sensitivity)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Rank sensitivity must be true or false.",
                     key = "statistics.rank_sensitivity")
  }
  if (!statistics_spec$selection_validity %in% c("qualified", "descriptive_only", "unverified")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Selection validity is not a known family status.",
                     key = "statistics.selection_validity")
  }
  if (!requireNamespace("limma", quietly = TRUE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma is not installed.", key = "statistics.method",
                     remediation = "Install limma from Bioconductor before fitting.")
  }
  plan <- resolved_analysis$model_plan
  scales <- unlist(lapply(resolved_analysis$blocks, function(block) block$scales), use.names = FALSE)
  required <- unique(vapply(scales, triomics_b03_required_unit, character(1)))
  if (length(required) != 1L || !nzchar(required[[1L]]) || !identical(required[[1L]], statistics_spec$effect_unit)) {
    triomics_b_abort(
      "B_DESIGN_EFFECT_UNIT",
      "The effect unit does not match the declared measurement scale.",
      key = "statistics.effect_unit"
    )
  }
  effect_unit <- required[[1L]]
  column_ids <- plan$column_ids
  contrasts <- statistics_spec$contrasts
  if (length(contrasts) == 0L) {
    triomics_b_abort("B_DESIGN_CONTRAST", "At least one named contrast is required.", key = "statistics.contrasts")
  }
  for (contrast in contrasts) triomics_b03_reject_projected(contrast, plan)
  contrast_ids <- vapply(contrasts, function(contrast) {
    if (!triomics_b_string(contrast$contrast_id)) {
      triomics_b_abort("B_DESIGN_CONTRAST", "Each contrast needs a contrast id.", key = "statistics.contrasts")
    }
    contrast$contrast_id
  }, character(1))
  if (anyDuplicated(contrast_ids)) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Contrast ids must be unique.", key = "statistics.contrasts")
  }
  weight_list <- lapply(contrasts, triomics_b02_weight_map, column_ids = column_ids)
  names(weight_list) <- contrast_ids
  publish_q <- identical(statistics_spec$selection_validity, "qualified")
  adjusted <- length(plan$covariates) > 0L
  blocks <- vector("list", length(resolved_analysis$blocks))
  names(blocks) <- names(resolved_analysis$blocks)
  all_estimated <- TRUE
  for (block in resolved_analysis$blocks) {
    design <- block$design
    if (is.null(design) || !identical(colnames(design), column_ids) ||
        !identical(rownames(design), colnames(block$assay))) {
      triomics_b_abort(
        "B_DESIGN_CONTRAST",
        "The design columns do not match the declared model-column ids.",
        key = "statistics.contrasts"
      )
    }
    n_features <- length(block$feature_keys)
    supports <- suppressWarnings(lapply(seq_len(n_features), function(i) {
      triomics_b02_feature_support(as.numeric(block$assay[i, ]), design, weight_list)
    }))
    model_hash <- triomics_b_hash(list(
      formula = plan$formula,
      column_ids = column_ids,
      representation = "delta",
      estimand = plan$estimand
    ))
    contrast_results <- vector("list", length(contrast_ids))
    names(contrast_results) <- contrast_ids
    for (j in seq_along(contrast_ids)) {
      weights <- weight_list[[contrast_ids[[j]]]]
      kind <- triomics_b03_contrast_kind(weights)
      moderated <- suppressWarnings(triomics_b02_limma_scalar(
        block$assay, design, supports, weight_list, j, statistics_spec$robust
      ))
      estimated <- vapply(seq_len(n_features), function(i) !is.null(moderated[[i]]), logical(1))
      p_values <- rep(NA_real_, n_features)
      if (any(estimated)) {
        p_values[estimated] <- vapply(which(estimated), function(i) moderated[[i]]$p, numeric(1))
      }
      if (!all(estimated)) all_estimated <- FALSE
      family <- triomics_b_family(
        resolved_analysis$study_id, resolved_analysis$analysis_id, block$block_id,
        contrast_ids[[j]], block$feature_keys, estimated, p_values, publish_q
      )
      rows <- vector("list", n_features)
      contrast_hash <- triomics_b_hash(list(contrast_id = contrast_ids[[j]], weights = as.list(weights)))
      for (i in seq_len(n_features)) {
        support <- supports[[i]]
        drawn <- moderated[[i]]
        estimable <- isTRUE(support$estimable[[j]])
        reason <- if (estimated[[i]]) {
          NULL
        } else if (!estimable) {
          "contrast_not_estimable"
        } else if (support$df < 1) {
          "zero_residual_support"
        } else {
          "contrast_not_estimable"
        }
        effect <- if (estimable && (estimated[[i]] || identical(reason, "zero_residual_support"))) {
          as.numeric(support$effects[[j]])
        } else {
          NULL
        }
        y <- as.numeric(block$assay[i, ])
        direction <- triomics_b03_direction(y, block, plan, kind, effect, estimated[[i]], adjusted)
        n_finite <- as.integer(support$n_used)
        rows[[i]] <- list(
          feature_key = block$feature_keys[[i]],
          effect = effect,
          effect_unit = effect_unit,
          ordinary_se = if (estimated[[i]]) as.numeric(support$ordinary_se[[j]]) else NULL,
          moderated_se = if (estimated[[i]]) drawn$moderated_se else NULL,
          statistic = if (estimated[[i]]) drawn$statistic else NULL,
          statistic_name = if (estimated[[i]]) "moderated_t" else NULL,
          ordinary_df = if (estimable) support$df else NULL,
          effective_test_df = if (estimated[[i]]) drawn$effective_df else NULL,
          df_prior = if (estimated[[i]]) drawn$df_prior else NULL,
          p = if (estimated[[i]]) drawn$p else NULL,
          q = if (estimated[[i]] && publish_q) family$internal_q[[i]] else NULL,
          inferential_status = triomics_b_inferential_status(estimated[[i]], statistics_spec$selection_validity),
          estimation_status = if (estimated[[i]]) "estimated" else "failed",
          estimation_reason = reason,
          coefficient_interpretation = kind,
          n_samples = as.integer(2L * n_finite),
          n_subjects = n_finite,
          n_rows_used = n_finite,
          n_pairs = n_finite,
          n_analysis_rows = block$n_analysis_rows,
          count_prior_ref = NULL,
          source_hash = block$source_hash,
          model_hash = model_hash,
          representation_hash = triomics_b_hash(list(
            representation = "delta",
            scale = block$scales[[i]],
            feature_key = block$feature_keys[[i]]
          )),
          contrast_hash = contrast_hash,
          representation_id = "delta",
          claim_type = "scalar_contrast",
          direction_status = direction$direction_status,
          direction_reason = direction$direction_reason,
          direction_numerator = direction$direction_numerator,
          direction_denominator = direction$direction_denominator,
          direction_zero_count = direction$direction_zero_count,
          direction_unavailable_count = direction$direction_unavailable_count,
          direction_groups = direction$direction_groups
        )
      }
      contrast_results[[contrast_ids[[j]]]] <- list(
        contrast_id = contrast_ids[[j]],
        estimand = kind,
        coefficient_interpretation = kind,
        rows = rows,
        family = family,
        count_prior_status = "not_used_no_automatic_deqms",
        sensitivity = triomics_b03_sensitivity(statistics_spec, plan, block, weights, kind),
        attrition = block$attrition,
        effect_unit = effect_unit
      )
    }
    blocks[[block$block_id]] <- list(
      block_id = block$block_id,
      representation_id = "delta",
      signed_delta_map = block$signed_delta_map,
      centered = FALSE,
      decomposition = "target_minus_reference_once",
      decomposition_count = 1L,
      contrasts = contrast_results
    )
  }
  list(
    status = if (all_estimated) "complete" else "partial",
    limma_version = as.character(packageVersion("limma")),
    blocks = blocks,
    effects = NULL
  )
}

triomics_resolve_analysis_before_b03 <- triomics_resolve_analysis

triomics_resolve_analysis <- function(prepared_study, analysis_spec) {
  if (isTRUE(triomics_b03_owns(prepared_study, analysis_spec))) {
    triomics_b03_resolve(prepared_study, analysis_spec)
  } else {
    triomics_resolve_analysis_before_b03(prepared_study, analysis_spec)
  }
}

resolve_analysis <- triomics_resolve_analysis

triomics_b03_dispatch_fit <- function(original, parent) {
  force(original)
  env <- new.env(parent = parent)
  env$b_t03_original <- original
  dispatch <- function(resolved_analysis, statistics_spec) {
    if (isTRUE(is.list(resolved_analysis) && identical(resolved_analysis$lane, "b_t03"))) {
      triomics_b03_fit(resolved_analysis, statistics_spec)
    } else {
      b_t03_original(resolved_analysis, statistics_spec)
    }
  }
  environment(dispatch) <- env
  attr(dispatch, "b_t03_dispatch") <- TRUE
  dispatch
}

triomics_b03_install_namespace_dispatch <- function(pkgname) {
  envir <- asNamespace(pkgname)
  if (!exists("triomics_fit_block_models", envir = envir, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = envir, inherits = FALSE)
  if (isTRUE(attr(current, "b_t03_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b03_dispatch_fit(current, envir)
  triomics_b02_rebind(envir, "triomics_fit_block_models", dispatch)
  triomics_b02_rebind(envir, "fit_block_models", dispatch)
  invisible(TRUE)
}

triomics_b03_install_global_dispatch <- function() {
  if (!exists("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)
  if (isTRUE(attr(current, "b_t03_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b03_dispatch_fit(current, .GlobalEnv)
  assign("triomics_fit_block_models", dispatch, envir = .GlobalEnv)
  assign("fit_block_models", dispatch, envir = .GlobalEnv)
  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  triomics_b02_install_namespace_dispatch(pkgname)
  triomics_b03_install_namespace_dispatch(pkgname)
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_main_before_b03 <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  assign("triomics_main_before_b03", triomics_main_before_b03, envir = .GlobalEnv)
  assign("triomics_main", function(args) {
    triomics_b03_install_global_dispatch()
    triomics_main_before_b03(args)
  }, envir = .GlobalEnv)
}
