triomics_b02_token <- function(value, key) {
  if (!triomics_b_string(value) || !grepl("^[A-Za-z0-9_.]+$", value)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Model terms must be declared names, not R expressions.", key = key)
  }
  value
}

triomics_b02_subjects_repeat <- function(prepared_study, design) {
  subject_name <- design$subject_id
  metadata <- prepared_study$sample_metadata
  if (!triomics_b_string(subject_name) || is.null(metadata) || !subject_name %in% names(metadata)) return(FALSE)
  subjects <- as.character(metadata[[subject_name]])
  subjects <- subjects[!is.na(subjects) & nzchar(subjects)]
  anyDuplicated(subjects) > 0L
}

triomics_b02_owns <- function(prepared_study, analysis_spec) {
  design <- if (is.list(analysis_spec)) analysis_spec$design else NULL
  if (is.null(design) || !is.list(design)) return(FALSE)
  if (!identical(design$kind, "independent") || !identical(design$repeated, FALSE)) return(FALSE)
  if (identical(design$representation, "mean")) return(TRUE)
  if (!identical(design$representation, "levels")) return(FALSE)
  if (identical(design$condition_type, "continuous")) return(TRUE)
  if (!identical(design$condition_type, "factor")) return(FALSE)
  declared <- NULL
  if (is.list(prepared_study) && !is.null(prepared_study$declared_factor_levels) &&
      triomics_b_string(design$condition)) {
    declared <- prepared_study$declared_factor_levels[[design$condition]]
  }
  n_levels <- if (is.null(declared)) 0L else length(triomics_b_characters(declared, "factor_levels"))
  if (n_levels >= 3L) return(TRUE)
  if (n_levels >= 2L && triomics_b_string(design$visit_column) &&
      triomics_b02_subjects_repeat(prepared_study, design)) {
    return(TRUE)
  }
  FALSE
}

triomics_b02_align <- function(metadata, sample_ids, column, key) {
  if (is.null(metadata) || !is.data.frame(metadata) || !"sample_id" %in% names(metadata)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Sample metadata is required before design resolution.",
                     key = "sample_metadata")
  }
  ids <- as.character(metadata$sample_id)
  if (anyDuplicated(ids)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "Sample identifiers must be unique before design resolution.",
                     key = "sample_id")
  }
  if (!triomics_b_string(column) || !column %in% names(metadata)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The declared column is not in the sample metadata.", key = key)
  }
  if (any(!sample_ids %in% ids)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "An assay column is outside the sample metadata.", key = "sample_id")
  }
  values <- metadata[[column]]
  names(values) <- ids
  values[sample_ids]
}

triomics_b02_covariate_names <- function(design) {
  covariates <- design$covariates
  if (triomics_b_empty(covariates)) return(character())
  if (!is.list(covariates)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Covariates must be declared records.", key = "design.covariates")
  }
  vapply(covariates, function(item) {
    if (!is.list(item) || !triomics_b_string(item$variable) || !triomics_b_string(item$form)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "Each covariate needs a variable and a form.",
                       key = "design.covariates")
    }
    if (!identical(item$form, "level")) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED",
                       "Only level covariates are entered. Delta, within, and between forms are not relabelled.",
                       key = "design.covariates")
    }
    triomics_b02_token(item$variable, "design.covariates")
  }, character(1))
}

triomics_b02_numeric_column <- function(metadata, sample_ids, name, key) {
  aligned <- triomics_b02_align(metadata, sample_ids, name, key)
  if (is.character(aligned) || is.factor(aligned)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A factor or label is not coerced into a continuous term.", key = key)
  }
  if (!is.numeric(aligned) || any(!is.finite(aligned))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Missing covariates are an error and are not imputed.", key = key)
  }
  stats::setNames(as.numeric(aligned), sample_ids)
}

triomics_b02_factor_column <- function(metadata, sample_ids, name, levels) {
  aligned <- triomics_b02_align(metadata, sample_ids, name, "design.condition")
  if (is.numeric(aligned)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A continuous column is not coerced into factor levels.",
                     key = "design.condition")
  }
  values <- as.character(aligned)
  if (anyNA(values) || any(!nzchar(values)) || any(!values %in% levels)) {
    triomics_b_abort("B_DESIGN_LEVELS", "A sample condition is outside the declared factor levels.",
                     key = "design.condition")
  }
  stats::setNames(values, sample_ids)
}

triomics_b02_mean_weights <- function(design, visit_levels) {
  raw <- design$mean_weights
  if (triomics_b_empty(raw) || is.character(raw) || is.language(raw) || is.null(names(raw))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED",
                     "A mean-retaining analysis needs explicit finite visit weights, not an expression.",
                     key = "design.mean_weights")
  }
  if (is.list(raw)) {
    for (value in raw) {
      if (is.language(value) || is.character(value) || length(value) != 1L || !is.numeric(value) || !is.finite(value)) {
        triomics_b_abort("B_DESIGN_UNSUPPORTED", "Visit weights must be finite numbers, not expressions.",
                         key = "design.mean_weights")
      }
    }
    weights <- stats::setNames(as.numeric(unlist(raw, use.names = TRUE)), names(raw))
  } else if (is.numeric(raw)) {
    weights <- as.numeric(raw)
  } else {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Visit weights must be finite numbers, not expressions.",
                     key = "design.mean_weights")
  }
  if (any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0 ||
      !setequal(names(weights), visit_levels)) {
    triomics_b_abort("B_DESIGN_LEVELS",
                     "Every declared visit needs an explicit non-negative weight, and at least one weight must be positive.",
                     key = "design.mean_weights")
  }
  stored <- stats::setNames(vector("list", length(visit_levels)), visit_levels)
  for (visit in visit_levels) stored[[visit]] <- unname(weights[[visit]])
  list(numeric = stats::setNames(weights[visit_levels], visit_levels), stored = stored)
}

triomics_b02_check_map <- function(prepared_study, representation_id) {
  unit_map <- prepared_study$analysis_unit_map
  if (is.null(unit_map)) return(invisible(TRUE))
  if (!is.data.frame(unit_map) || !"representation_id" %in% names(unit_map)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "An analysis unit map needs representation_id.",
                     key = "analysis_unit_map")
  }
  if (any(as.character(unit_map$representation_id) != representation_id)) {
    triomics_b_abort("B_DESIGN_UNIT_MAP",
                     "The supplied unit map representation contradicts this analysis and is not relabelled.",
                     key = "analysis_unit_map")
  }
  invisible(TRUE)
}

triomics_b02_constant <- function(values, subjects, key, message) {
  for (subject in unique(subjects)) {
    chosen <- values[subjects == subject]
    if (length(unique(chosen)) != 1L) {
      triomics_b_abort("B_DESIGN_LEVELS", message, key = key)
    }
  }
  stats::setNames(values[match(unique(subjects), subjects)], unique(subjects))
}

triomics_b02_design_matrix <- function(unit_ids, column_ids, columns) {
  design <- matrix(0, length(unit_ids), length(column_ids), dimnames = list(unit_ids, column_ids))
  for (column in column_ids) design[, column] <- as.numeric(columns[[column]])
  design
}

triomics_b02_levels <- function(prepared_study, condition) {
  declared <- prepared_study$declared_factor_levels[[condition]]
  if (is.null(declared)) {
    triomics_b_abort("B_DESIGN_LEVELS", "The condition has no declared factor levels.", key = "factor_levels")
  }
  levels <- triomics_b_characters(declared, "factor_levels")
  if (length(levels) < 2L || anyDuplicated(levels) || any(!nzchar(levels))) {
    triomics_b_abort("B_DESIGN_LEVELS", "Declared factor levels must be unique and non-empty.", key = "factor_levels")
  }
  vapply(levels, triomics_b02_token, character(1), key = "factor_levels")
}

triomics_b02_validate_assay <- function(assay) {
  if (!is.matrix(assay) || is.null(colnames(assay)) || is.null(rownames(assay)) ||
      anyDuplicated(colnames(assay)) || anyDuplicated(rownames(assay))) {
    triomics_b_abort("B_DESIGN_UNIT_MAP", "Each assay needs unique sample and feature identifiers.", key = "assays")
  }
  assay
}

triomics_b02_hash_check <- function(block_id, assay, hashes) {
  actual <- triomics_b_assay_hash(block_id, assay)
  declared <- if (is.null(hashes)) NULL else hashes[[block_id]]
  if (!is.null(declared) && !identical(as.character(declared), actual)) {
    triomics_b_abort("B_DESIGN_HASH", "The declared assay hash does not match the measurement matrix.",
                     key = "assay_hashes")
  }
  actual
}

triomics_b02_resolve <- function(prepared_study, analysis_spec) {
  design <- analysis_spec$design
  if (!triomics_b_empty(design$interactions)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Interactions are not expanded from names or evaluated as R code.",
                     key = "design.interactions")
  }
  if (!is.null(design$missing_visits) && !identical(design$missing_visits, "error")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Missing visits stay an error in this lane and are not imputed.",
                     key = "design.missing_visits")
  }
  if (!identical(design$representation, "mean") && !triomics_b_empty(design$mean_weights)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Visit mean weights belong only to a mean representation.",
                     key = "design.mean_weights")
  }
  condition <- triomics_b02_token(design$condition, "design.condition")
  covariate_names <- triomics_b02_covariate_names(design)
  if (anyDuplicated(covariate_names)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Covariate names must be unique.", key = "design.covariates")
  }
  representation_id <- design$representation
  triomics_b02_check_map(prepared_study, representation_id)
  block_ids <- triomics_b_characters(analysis_spec$block_ids, "block_ids")
  assays <- prepared_study$assays
  if (length(block_ids) == 0L || is.null(assays) || any(!block_ids %in% names(assays))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Every analysis block must already be in the prepared study.",
                     key = "block_ids")
  }
  if (!triomics_b_string(analysis_spec$analysis_id)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The analysis id is required.", key = "analysis_id")
  }
  metadata <- prepared_study$sample_metadata
  mean_stored <- NULL
  visit_levels <- NULL
  levels <- NULL
  coefficient_kind <- if (identical(design$condition_type, "continuous")) "continuous" else "group_contrast"
  if (identical(representation_id, "mean")) {
    if (!triomics_b_string(design$subject_id) || !triomics_b_string(design$visit_column)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "A subject-mean analysis needs a subject column and a visit column.",
                       key = "design.subject_id")
    }
    if (!identical(design$condition_type, "factor")) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED", "This mean-retaining lane uses a declared between-person factor.",
                       key = "design.condition_type")
    }
    visit_levels <- triomics_b02_levels(prepared_study, design$visit_column)
    parsed_weights <- triomics_b02_mean_weights(design, visit_levels)
    mean_stored <- parsed_weights$stored
    levels <- triomics_b02_levels(prepared_study, condition)
  } else if (identical(coefficient_kind, "group_contrast")) {
    levels <- triomics_b02_levels(prepared_study, condition)
    if (triomics_b02_subjects_repeat(prepared_study, design) && !triomics_b_string(design$visit_column)) {
      triomics_b_abort("B_DESIGN_UNSUPPORTED",
                       "Duplicate subjects need an explicit visit-level or subject-mean analysis.",
                       key = "design.subject_id")
    }
  } else if (!identical(design$condition_type, "continuous")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The condition type is not a declared factor or continuous term.",
                     key = "design.condition_type")
  }
  if (!is.null(levels) && triomics_b_string(design$reference_level) && !design$reference_level %in% levels) {
    triomics_b_abort("B_DESIGN_LEVELS", "The reference level is not one of the declared factor levels.",
                     key = "design.reference_level")
  }
  if (!is.null(levels) && triomics_b_string(design$target_level) && !design$target_level %in% levels) {
    triomics_b_abort("B_DESIGN_LEVELS", "The target level is not one of the declared factor levels.",
                     key = "design.target_level")
  }
  covariate_ids <- if (length(covariate_names) == 0L) {
    character()
  } else {
    paste0("covariate.", covariate_names)
  }
  column_ids <- if (identical(coefficient_kind, "continuous")) {
    c("intercept", paste0("continuous.", condition), covariate_ids)
  } else {
    c(paste0("condition.", levels), covariate_ids)
  }
  formula <- if (identical(coefficient_kind, "continuous")) {
    paste0("~1 + ", paste(column_ids[-1L], collapse = " + "))
  } else {
    paste0("~0 + ", paste(column_ids, collapse = " + "))
  }
  registry <- prepared_study$feature_registry
  blocks <- vector("list", length(block_ids))
  names(blocks) <- block_ids
  unit_map <- list()
  for (block_id in block_ids) {
    original <- triomics_b02_validate_assay(assays[[block_id]])
    triomics_b02_hash_check(block_id, original, prepared_study$assay_hashes)
    present <- colnames(original)
    if (identical(representation_id, "mean")) {
      subjects <- as.character(triomics_b02_align(metadata, present, design$subject_id, "design.subject_id"))
      visits <- as.character(triomics_b02_align(metadata, present, design$visit_column, "design.visit_column"))
      names(subjects) <- present
      names(visits) <- present
      if (anyNA(subjects) || any(!nzchar(subjects)) || anyNA(visits) || any(!visits %in% visit_levels)) {
        triomics_b_abort("B_DESIGN_LEVELS", "Every sample needs a subject and a declared visit.",
                         key = "design.visit_column")
      }
      groups <- triomics_b02_factor_column(metadata, present, condition, levels)
      triomics_b02_constant(groups, subjects, "design.condition",
                            "A subject-mean condition must be constant within subject.")
      covariate_values <- list()
      for (name in covariate_names) {
        covariate_values[[name]] <- triomics_b02_numeric_column(metadata, present, name, "design.covariates")
        triomics_b02_constant(covariate_values[[name]], subjects, "design.covariates",
                              "A subject-mean covariate must be constant within subject and is not averaged silently.")
      }
      unit_ids <- unique(subjects)
      weights <- parsed_weights$numeric
      collapsed <- matrix(NA_real_, nrow(original), length(unit_ids),
                          dimnames = list(rownames(original), unit_ids))
      map <- vector("list", length(unit_ids))
      group_by_unit <- character(length(unit_ids))
      names(group_by_unit) <- unit_ids
      covariate_by_unit <- lapply(covariate_names, function(name) stats::setNames(rep(NA_real_, length(unit_ids)), unit_ids))
      names(covariate_by_unit) <- covariate_names
      for (index in seq_along(unit_ids)) {
        subject <- unit_ids[[index]]
        chosen <- present[subjects == subject]
        group_by_unit[[subject]] <- unique(groups[chosen])
        for (name in covariate_names) covariate_by_unit[[name]][[subject]] <- unique(covariate_values[[name]][chosen])
        source_ids <- character()
        sample_for_visit <- stats::setNames(rep(NA_character_, length(visit_levels)), visit_levels)
        for (visit in visit_levels) {
          hits <- chosen[visits[chosen] == visit]
          if (weights[[visit]] > 0 && length(hits) != 1L) {
            triomics_b_abort("B_DESIGN_LEVELS",
                             "A positive visit weight is missing or duplicated. The subject is not dropped or imputed.",
                             key = "design.missing_visits")
          }
          if (weights[[visit]] == 0 && length(hits) > 1L) {
            triomics_b_abort("B_DESIGN_LEVELS", "A visit is duplicated within a subject.", key = "design.visit_column")
          }
          if (length(hits) == 1L) {
            sample_for_visit[[visit]] <- hits
            source_ids <- c(source_ids, hits)
          }
        }
        for (feature in seq_len(nrow(original))) {
          observed <- original[feature, sample_for_visit]
          visit_weights <- weights[names(sample_for_visit)]
          if (any(!is.finite(observed))) {
            collapsed[feature, subject] <- NA_real_
          } else {
            collapsed[feature, subject] <- sum(visit_weights * as.numeric(observed)) / sum(visit_weights)
          }
        }
        map[[index]] <- list(
          unit_id = subject,
          unit_type = "subject_mean",
          source_sample_ids = source_ids,
          subject_id = subject,
          representation_id = "mean",
          visit_weights = mean_stored
        )
      }
      columns <- list()
      for (level in levels) columns[[paste0("condition.", level)]] <- as.numeric(group_by_unit == level)
      for (name in covariate_names) columns[[paste0("covariate.", name)]] <- as.numeric(covariate_by_unit[[name]])
      analysis <- collapsed
      analysis_groups <- group_by_unit
      analysis_subjects <- stats::setNames(unit_ids, unit_ids)
      n_source <- length(present)
      unit_map <- map
    } else {
      if (identical(coefficient_kind, "group_contrast")) {
        groups <- triomics_b02_factor_column(metadata, present, condition, levels)
        if (triomics_b_string(design$subject_id)) {
          subjects <- as.character(triomics_b02_align(metadata, present, design$subject_id, "design.subject_id"))
          names(subjects) <- present
          if (triomics_b_string(design$visit_column)) {
            triomics_b02_constant(groups, subjects, "design.condition",
                                  "A visit-level between-person condition must be constant within subject.")
          }
        } else {
          subjects <- stats::setNames(present, present)
        }
      } else {
        groups <- NULL
        predictor <- triomics_b02_numeric_column(metadata, present, condition, "design.condition")
        subjects <- if (triomics_b_string(design$subject_id) && design$subject_id %in% names(metadata)) {
          aligned <- as.character(triomics_b02_align(metadata, present, design$subject_id, "design.subject_id"))
          stats::setNames(aligned, present)
        } else {
          stats::setNames(present, present)
        }
      }
      covariate_values <- list()
      for (name in covariate_names) {
        covariate_values[[name]] <- triomics_b02_numeric_column(metadata, present, name, "design.covariates")
      }
      columns <- list()
      if (identical(coefficient_kind, "continuous")) {
        columns$intercept <- rep(1, length(present))
        columns[[paste0("continuous.", condition)]] <- as.numeric(predictor)
      } else {
        for (level in levels) columns[[paste0("condition.", level)]] <- as.numeric(groups == level)
      }
      for (name in covariate_names) columns[[paste0("covariate.", name)]] <- as.numeric(covariate_values[[name]])
      analysis <- original
      unit_ids <- present
      analysis_groups <- if (is.null(groups)) stats::setNames(rep(NA_character_, length(present)), present) else groups
      analysis_subjects <- subjects
      n_source <- length(present)
      unit_type <- if (triomics_b_string(design$visit_column)) "visit_level" else "sample"
      map <- vector("list", length(unit_ids))
      for (index in seq_along(unit_ids)) {
        map[[index]] <- list(
          unit_id = unit_ids[[index]],
          unit_type = unit_type,
          source_sample_ids = unit_ids[[index]],
          subject_id = unname(analysis_subjects[[unit_ids[[index]]]]),
          representation_id = "levels"
        )
      }
      if (length(unit_map) == 0L) unit_map <- map
    }
    feature_keys <- rownames(analysis)
    scales <- vapply(feature_keys, triomics_b_scale, character(1), registry = registry, block_id = block_id)
    blocks[[block_id]] <- list(
      block_id = block_id,
      assay = analysis,
      sample_ids = colnames(analysis),
      unit_ids = colnames(analysis),
      groups = as.character(analysis_groups),
      subjects = analysis_subjects,
      feature_keys = feature_keys,
      scales = unname(scales),
      source_hash = triomics_b_assay_hash(block_id, analysis),
      design = triomics_b02_design_matrix(colnames(analysis), column_ids, columns),
      n_source_samples = as.integer(n_source),
      unit_map = if (identical(representation_id, "mean")) map else map
    )
  }
  if (identical(representation_id, "mean")) {
    unit_map <- blocks[[block_ids[[1L]]]]$unit_map
  }
  list(
    lane = "b_t02",
    study_id = prepared_study$study_id,
    analysis_id = analysis_spec$analysis_id,
    preparation_context_id = analysis_spec$preparation_context_id,
    representation = representation_id,
    representation_id = representation_id,
    model_plan = list(
      formula = formula,
      column_ids = column_ids,
      levels = levels,
      reference_level = design$reference_level,
      target_level = design$target_level,
      condition = condition,
      covariates = as.list(covariate_names),
      coefficient_kind = coefficient_kind,
      mean_weights = mean_stored,
      weight_reduction = if (identical(representation_id, "mean")) "weighted_mean" else NULL,
      residual_assumption = if (identical(representation_id, "mean")) {
        "subject_mean_independent_rows"
      } else if (triomics_b_string(design$visit_column)) {
        "ordinary_independent_rows_visit_level_between_person_signal_retained"
      } else {
        "ordinary_independent_rows"
      }
    ),
    blocks = blocks,
    subjects = blocks[[block_ids[[1L]]]]$subjects,
    unit_map = if (identical(representation_id, "mean")) blocks[[block_ids[[1L]]]]$unit_map else blocks[[block_ids[[1L]]]]$unit_map
  )
}

triomics_resolve_analysis_b_t01 <- triomics_resolve_analysis

triomics_resolve_analysis <- function(prepared_study, analysis_spec) {
  if (isTRUE(triomics_b02_owns(prepared_study, analysis_spec))) {
    triomics_b02_resolve(prepared_study, analysis_spec)
  } else {
    triomics_resolve_analysis_b_t01(prepared_study, analysis_spec)
  }
}

resolve_analysis <- triomics_resolve_analysis

triomics_b02_rebind <- function(envir, name, value) {
  if (!exists(name, envir = envir, inherits = FALSE)) return(invisible(FALSE))
  if (bindingIsLocked(name, envir)) unlockBinding(name, envir)
  assign(name, value, envir = envir)
  try(lockBinding(name, envir), silent = TRUE)
  invisible(TRUE)
}

triomics_b02_dispatch_fit <- function(original, parent) {
  force(original)
  env <- new.env(parent = parent)
  env$b_t02_original <- original
  dispatch <- function(resolved_analysis, statistics_spec) {
    if (isTRUE(is.list(resolved_analysis) && identical(resolved_analysis$lane, "b_t02"))) {
      triomics_b02_fit(resolved_analysis, statistics_spec)
    } else {
      b_t02_original(resolved_analysis, statistics_spec)
    }
  }
  environment(dispatch) <- env
  attr(dispatch, "b_t02_dispatch") <- TRUE
  dispatch
}

triomics_b02_install_namespace_dispatch <- function(pkgname) {
  envir <- asNamespace(pkgname)
  if (!exists("triomics_fit_block_models", envir = envir, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = envir, inherits = FALSE)
  if (isTRUE(attr(current, "b_t02_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b02_dispatch_fit(current, envir)
  triomics_b02_rebind(envir, "triomics_fit_block_models", dispatch)
  triomics_b02_rebind(envir, "fit_block_models", dispatch)
  invisible(TRUE)
}

triomics_b02_install_global_dispatch <- function() {
  if (!exists("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)
  if (isTRUE(attr(current, "b_t02_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b02_dispatch_fit(current, .GlobalEnv)
  assign("triomics_fit_block_models", dispatch, envir = .GlobalEnv)
  assign("fit_block_models", dispatch, envir = .GlobalEnv)
  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  triomics_b02_install_namespace_dispatch(pkgname)
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_main_b_t01 <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  assign("triomics_main_b_t01", triomics_main_b_t01, envir = .GlobalEnv)
  assign("triomics_main", function(args) {
    triomics_b02_install_global_dispatch()
    triomics_main_b_t01(args)
  }, envir = .GlobalEnv)
}
