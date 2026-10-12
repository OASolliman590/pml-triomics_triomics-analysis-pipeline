# Fold-local preparation. Learned state is fit on training units only.
# Apply reads that state and does not refit or reselect features.

triomics_fold_scalar <- function(value) {
  if (is.null(value) || length(value) == 0L) return(NA_character_)
  while (is.list(value)) value <- value[[1L]]
  as.character(value[[1L]])
}

triomics_fold_named_pairs <- function(value, key_name, value_name) {
  if (is.null(value) || length(value) == 0L) return(stats::setNames(numeric(), character()))
  if (is.data.frame(value)) {
    keys <- as.character(value[[key_name]])
    vals <- as.numeric(value[[value_name]])
    return(stats::setNames(vals, keys))
  }
  if (is.list(value) && !is.null(value[[key_name]]) && !is.list(value[[key_name]])) {
    keys <- as.character(unlist(value[[key_name]], use.names = FALSE))
    vals <- as.numeric(unlist(value[[value_name]], use.names = FALSE))
    return(stats::setNames(vals, keys))
  }
  keys <- vapply(value, function(row) as.character(row[[key_name]][[1L]]), character(1))
  vals <- vapply(value, function(row) {
    cell <- row[[value_name]]
    if (is.list(cell)) cell <- cell[[1L]]
    as.numeric(cell)
  }, numeric(1))
  stats::setNames(vals, keys)
}

triomics_fold_parameter_snapshot <- function(state) {
  learned <- state$learned_parameters
  if (is.null(learned) || !is.list(learned)) learned <- list()
  pqn <- learned$pqn
  role <- learned$imputation_role
  ids <- state$training_unit_ids
  keys <- state$feature_keys
  if (is.list(ids)) ids <- unlist(ids, use.names = FALSE)
  if (is.list(keys)) keys <- unlist(keys, use.names = FALSE)
  list(
    feature_keys = unname(as.character(keys)),
    training_unit_ids = unname(as.character(ids)),
    training_unit_hash = triomics_fold_scalar(state$training_unit_hash),
    input_scale = triomics_fold_scalar(state$input_scale),
    output_scale = triomics_fold_scalar(state$output_scale),
    reference_scale = triomics_fold_scalar(state$reference_scale),
    pqn_reference = if (is.null(pqn)) stats::setNames(numeric(), character()) else {
      triomics_fold_named_pairs(pqn$reference_vector, "feature_key", "value")
    },
    pqn_reference_scale = if (is.null(pqn)) NA_character_ else triomics_fold_scalar(pqn$reference_scale),
    medians = triomics_fold_named_pairs(learned$medians, "feature_key", "median"),
    imputation_method = if (is.list(role) && !is.null(role$method)) triomics_fold_scalar(role$method) else NA_character_,
    state_hash = triomics_fold_scalar(state$state_hash)
  )
}

triomics_fold_abort_split <- function(message) {
  triomics_abort(
    "A_STATE_SPLIT", message, key = "split_plan", module = "resampling_preparation", stage = "A",
    remediation = "Keep subject-disjoint outer and inner folds. Do not drop a fold or a class to force a split."
  )
}

triomics_split_ids <- function(value, label, allow_empty = FALSE) {
  if (is.factor(value)) value <- as.character(value)
  if (is.list(value) && !is.data.frame(value)) value <- unlist(value, use.names = FALSE)
  if (is.null(value) || (!allow_empty && !length(value))) triomics_fold_abort_split(paste0(label, " is empty."))
  if (!length(value)) return(character())
  if (!is.character(value) || anyNA(value) || any(!nzchar(value)) || anyDuplicated(value)) {
    triomics_fold_abort_split(paste0(label, " must be unique non-blank strings."))
  }
  unname(value)
}

triomics_split_times <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.numeric(value) && !is.null(names(value))) {
    times <- as.numeric(value)
    names(times) <- names(value)
  } else if (is.list(value)) {
    ids <- vapply(value, function(row) as.character(row$unit_id[[1L]]), character(1))
    times <- vapply(value, function(row) as.numeric(row$time[[1L]]), numeric(1))
    names(times) <- ids
  } else {
    triomics_fold_abort_split("Temporal order must name a time for every unit.")
  }
  if (anyDuplicated(names(times)) || any(!is.finite(times))) {
    triomics_fold_abort_split("Temporal order must be finite and unique per unit.")
  }
  times
}

triomics_split_check_partition <- function(fold, map, times) {
  train_units <- triomics_split_ids(fold$training_unit_ids, "Training units")
  test_units <- triomics_split_ids(fold$test_unit_ids, "Test units")
  train_subjects <- triomics_split_ids(fold$training_subject_ids, "Training subjects")
  test_subjects <- triomics_split_ids(fold$test_subject_ids, "Test subjects")
  if (length(intersect(train_subjects, test_subjects)) || length(intersect(train_units, test_units))) {
    triomics_fold_abort_split("A train/test partition shares a subject.")
  }
  if (!all(train_units %in% map$unit_id) || !all(test_units %in% map$unit_id)) {
    triomics_fold_abort_split("A split unit is missing from the unit map.")
  }
  expected_train <- map$unit_id[map$subject_id %in% train_subjects]
  expected_test <- map$unit_id[map$subject_id %in% test_subjects]
  if (!setequal(expected_train, train_units) || !setequal(expected_test, test_units)) {
    triomics_fold_abort_split("A train/test partition shares a subject.")
  }
  if (!is.null(times)) {
    needed <- c(train_units, test_units)
    if (any(!needed %in% names(times)) || min(times[test_units]) <= max(times[train_units])) {
      triomics_fold_abort_split("A test unit is not strictly later than every training unit.")
    }
  }
  invisible(TRUE)
}

triomics_split_check_fold <- function(fold, map, times, parent_id) {
  if (!is.list(fold) || !is.character(fold$fold_id) || length(fold$fold_id) != 1L || !nzchar(fold$fold_id)) {
    triomics_fold_abort_split("Each fold needs a fold id.")
  }
  if (!is.character(fold$level) || length(fold$level) != 1L || !fold$level %in% c("outer", "inner")) {
    triomics_fold_abort_split("Each fold level must be outer or inner.")
  }
  if (!is.null(parent_id) && !identical(fold$parent_fold_id, parent_id)) {
    triomics_fold_abort_split("An inner fold names the wrong parent.")
  }
  triomics_split_check_partition(fold, map, times)
  if (identical(fold$level, "inner")) return(invisible(TRUE))
  if (!is.list(fold$inner)) triomics_fold_abort_split("An outer fold needs an inner list.")
  for (inner in fold$inner) {
    triomics_split_check_fold(inner, map, times, fold$fold_id)
    if (!all(inner$test_subject_ids %in% fold$training_subject_ids) ||
        !all(inner$training_subject_ids %in% fold$training_subject_ids) ||
        length(intersect(inner$test_subject_ids, fold$test_subject_ids))) {
      triomics_fold_abort_split("An inner fold escaped its outer training subjects.")
    }
  }
  invisible(TRUE)
}

triomics_validate_split_plan <- function(plan, require_hash = TRUE) {
  if (!is.list(plan) || !is.data.frame(plan$unit_subject) ||
      !all(c("unit_id", "subject_id") %in% names(plan$unit_subject)) || !is.list(plan$folds) ||
      !length(plan$folds)) {
    triomics_fold_abort_split("A split plan needs folds and a unit-to-subject map.")
  }
  map <- plan$unit_subject
  map$unit_id <- as.character(map$unit_id)
  map$subject_id <- as.character(map$subject_id)
  if (anyNA(map$unit_id) || anyNA(map$subject_id) || any(!nzchar(map$unit_id)) ||
      any(!nzchar(map$subject_id)) || anyDuplicated(map$unit_id)) {
    triomics_fold_abort_split("Unit ids must be unique and every unit needs a subject.")
  }
  times <- triomics_split_times(plan$temporal_order)
  if (!is.null(times) && any(!map$unit_id %in% names(times))) {
    triomics_fold_abort_split("Temporal order must name a time for every unit.")
  }
  seen <- character()
  for (fold in plan$folds) {
    triomics_split_check_fold(fold, map, times, NULL)
    if (fold$fold_id %in% seen) triomics_fold_abort_split("Fold ids must be unique.")
    seen <- c(seen, fold$fold_id)
  }
  if (isTRUE(require_hash)) {
    body <- plan
    body$creation_hash <- NULL
    hash <- triomics_hash_text(triomics_canonical_json(body))
    if (!is.character(plan$creation_hash) || !identical(plan$creation_hash, hash)) {
      triomics_abort(
        "A_STATE_STALE", "Split plan creation hash does not match the fold contents.",
        key = "creation_hash", module = "resampling_preparation", stage = "A",
        remediation = "Rebuild the split plan. Do not edit fold membership under an old hash."
      )
    }
  }
  invisible(plan)
}

triomics_finalize_split_plan <- function(plan) {
  triomics_validate_split_plan(plan, FALSE)
  plan$creation_hash <- NULL
  plan$creation_hash <- triomics_hash_text(triomics_canonical_json(plan))
  plan
}

triomics_split_order_subjects <- function(subjects, seed, parent) {
  keys <- vapply(subjects, function(id) {
    triomics_hash_text(paste(as.integer(seed), if (is.null(parent)) "", parent, id, sep = "|"))
  }, character(1))
  unname(subjects[order(keys, subjects)])
}

triomics_split_deal <- function(subjects, n_folds, seed, parent, stratum_of) {
  if (length(subjects) < n_folds) {
    triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
  }
  folds <- rep(list(character()), n_folds)
  groups <- if (is.null(stratum_of)) list(subjects) else split(subjects, stratum_of[subjects])
  for (members in groups) {
    members <- unname(members)
    if (length(members) < n_folds) {
      triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
    }
    ordered <- triomics_split_order_subjects(members, seed, parent)
    for (i in seq_along(ordered)) {
      slot <- ((i - 1L) %% n_folds) + 1L
      folds[[slot]] <- c(folds[[slot]], ordered[[i]])
    }
  }
  if (any(vapply(folds, length, integer(1L)) == 0L)) {
    triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
  }
  folds
}

triomics_split_units_for <- function(subjects, unit_ids, subject_ids) {
  units <- character()
  for (subject in subjects) units <- c(units, unit_ids[subject_ids == subject])
  unname(units)
}

triomics_split_child <- function(subjects, n_folds, seed, parent, stratum_of, unit_ids, subject_ids,
                                level_subjects, temporal) {
  records <- list()
  if (isTRUE(temporal)) {
    ordered <- level_subjects
    if (length(ordered) < n_folds + 1L) {
      triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
    }
    for (k in seq_len(n_folds)) {
      test_index <- length(ordered) - n_folds + k
      test_subjects <- ordered[[test_index]]
      train_subjects <- ordered[seq_len(test_index - 1L)]
      records[[k]] <- list(test = test_subjects, train = train_subjects)
    }
    return(records)
  }
  dealt <- triomics_split_deal(subjects, n_folds, seed, parent, stratum_of)
  for (k in seq_along(dealt)) {
    records[[k]] <- list(test = dealt[[k]], train = subjects[!subjects %in% dealt[[k]]])
  }
  records
}

triomics_nested_split_plan <- function(unit_ids, subject_ids = NULL, n_outer, n_inner, seed,
                                       stratification = NULL, temporal_order = NULL,
                                       target = "unspecified") {
  unit_ids <- triomics_split_ids(unit_ids, "Units")
  if (is.null(subject_ids)) subject_ids <- unit_ids
  subject_ids <- as.character(subject_ids)
  if (length(subject_ids) != length(unit_ids) || anyNA(subject_ids) || any(!nzchar(subject_ids))) {
    triomics_fold_abort_split("Subject ids must align with every unit.")
  }
  if (!is.numeric(n_outer) || !is.numeric(n_inner) || !is.numeric(seed) ||
      n_outer != trunc(n_outer) || n_inner != trunc(n_inner) || seed != trunc(seed) ||
      n_outer < 2 || n_inner < 2) {
    triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
  }
  if (!is.null(stratification) && !is.null(temporal_order)) {
    triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
  }
  n_outer <- as.integer(n_outer)
  n_inner <- as.integer(n_inner)
  seed <- as.integer(seed)
  subjects <- unique(subject_ids)
  stratum_of <- NULL
  if (!is.null(stratification)) {
    if (length(stratification) != length(unit_ids)) {
      triomics_fold_abort_split("Stratification must align with every unit.")
    }
    stratum_of <- stats::setNames(character(length(subjects)), subjects)
    for (subject in subjects) {
      vals <- unique(as.character(stratification[subject_ids == subject]))
      if (length(vals) != 1L || is.na(vals) || !nzchar(vals)) {
        triomics_fold_abort_split("A subject cannot carry two stratification labels.")
      }
      stratum_of[[subject]] <- vals
    }
  }
  temporal <- !is.null(temporal_order)
  ordered_subjects <- subjects
  stored_times <- NULL
  if (temporal) {
    if (!is.numeric(temporal_order) || is.null(names(temporal_order)) ||
        !setequal(names(temporal_order), unit_ids)) {
      triomics_fold_abort_split("Temporal order must name a time for every unit.")
    }
    subject_time <- stats::setNames(numeric(length(subjects)), subjects)
    for (subject in subjects) {
      vals <- unique(as.numeric(temporal_order[unit_ids[subject_ids == subject]]))
      if (length(vals) != 1L || !is.finite(vals)) {
        triomics_fold_abort_split("Samples of one subject must share one finite time.")
      }
      subject_time[[subject]] <- vals
    }
    ordered_subjects <- unname(subjects[order(subject_time[subjects], subjects)])
    stored_times <- lapply(sort(unit_ids), function(id) {
      list(unit_id = id, time = unname(as.numeric(temporal_order[[id]])))
    })
  }
  outer_parts <- triomics_split_child(
    if (temporal) ordered_subjects else subjects,
    n_outer, seed, NULL, stratum_of, unit_ids, subject_ids, ordered_subjects, temporal
  )
  if (temporal && (length(ordered_subjects) - n_outer) < (n_inner + 1L)) {
    triomics_fold_abort_split("Requested outer/inner folds are infeasible and were not reduced.")
  }
  folds <- list()
  for (k in seq_along(outer_parts)) {
    outer_id <- sprintf("outer_%02d", k)
    train_subjects <- outer_parts[[k]]$train
    test_subjects <- outer_parts[[k]]$test
    if (temporal) {
      train_ordered <- ordered_subjects[ordered_subjects %in% train_subjects]
    } else {
      train_ordered <- train_subjects
    }
    inner_parts <- triomics_split_child(
      train_subjects, n_inner, seed, outer_id,
      if (is.null(stratum_of)) NULL else stratum_of[train_subjects],
      unit_ids, subject_ids, train_ordered, temporal
    )
    inner <- list()
    for (j in seq_along(inner_parts)) {
      inner[[j]] <- list(
        fold_id = paste0(outer_id, "__inner_", sprintf("%02d", j)),
        level = "inner",
        parent_fold_id = outer_id,
        training_unit_ids = triomics_split_units_for(inner_parts[[j]]$train, unit_ids, subject_ids),
        test_unit_ids = triomics_split_units_for(inner_parts[[j]]$test, unit_ids, subject_ids),
        training_subject_ids = unname(inner_parts[[j]]$train),
        test_subject_ids = unname(inner_parts[[j]]$test),
        inner = list()
      )
    }
    folds[[k]] <- list(
      fold_id = outer_id,
      level = "outer",
      parent_fold_id = NULL,
      training_unit_ids = triomics_split_units_for(train_subjects, unit_ids, subject_ids),
      test_unit_ids = triomics_split_units_for(test_subjects, unit_ids, subject_ids),
      training_subject_ids = unname(train_subjects),
      test_subject_ids = unname(test_subjects),
      inner = inner
    )
  }
  triomics_finalize_split_plan(list(
    schema = "SplitPlan",
    seed = seed,
    grouping_unit = "subject",
    target = as.character(target[[1L]]),
    stratification = if (is.null(stratification)) "none" else "stratified",
    temporal_order = stored_times,
    n_outer = n_outer,
    n_inner = n_inner,
    unit_subject = data.frame(unit_id = unit_ids, subject_id = subject_ids, stringsAsFactors = FALSE),
    folds = folds
  ))
}

triomics_fold_copy_se <- function(se) {
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  copied <- se
  SummarizedExperiment::assay(copied, "analysis") <- matrix(
    as.numeric(analysis), nrow = nrow(analysis), dimnames = dimnames(analysis)
  )
  SummarizedExperiment::assay(copied, "original_mask") <- matrix(
    as.character(mask), nrow = nrow(mask), dimnames = dimnames(mask)
  )
  meta <- S4Vectors::metadata(copied)
  if (!is.null(meta$triomics)) {
    meta$triomics$state_payload <- NULL
    meta$triomics$state_ref <- NULL
    meta$triomics$completed <- list()
    meta$triomics$feature_flow <- NULL
    meta$triomics$declared_flow <- NULL
  }
  S4Vectors::metadata(copied) <- meta
  copied
}

triomics_fold_operation_supported <- function(operation) {
  if (!is.list(operation)) return(FALSE)
  if (identical(operation$apply, "unavailable")) return(FALSE)
  qualification <- operation$qualification
  if (is.list(qualification) && identical(qualification$inductive, FALSE)) return(FALSE)
  if (is.list(qualification) && identical(qualification$fit_apply, FALSE)) return(FALSE)
  TRUE
}

triomics_fold_reject_noninductive <- function(spec) {
  for (block_spec in spec$blocks) {
    for (operation in block_spec$operations) {
      if (triomics_fold_operation_supported(operation)) next
      method <- if (is.list(operation) && is.character(operation$method)) operation$method else "unknown"
      triomics_abort(
        "A_STATE_PREDICT",
        paste0("Non-inductive method '", method, "' rejects unsupported prediction."),
        key = method, module = "resampling_preparation", stage = "A",
        remediation = "Do not fit a transductive method on the cohort or advertise it as deployable."
      )
    }
  }
  invisible(TRUE)
}

triomics_fold_impute_method <- function(operations) {
  for (operation in operations) {
    if (is.list(operation) && identical(operation$stage, "impute")) return(operation$method)
  }
  "none"
}

triomics_fold_presence <- function(study, block_id, sample_ids) {
  availability <- study$availability
  if (!is.data.frame(availability) || !all(c("block_id", "sample_id", "presence", "cell_imputation_eligible") %in% names(availability))) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Availability is required before fold preparation.",
      key = "availability", module = "resampling_preparation", stage = "A",
      remediation = "Record structural absence separately from feature-level missingness."
    )
  }
  present <- character()
  rows <- list()
  for (sample_id in sample_ids) {
    hit <- which(availability$block_id == block_id & availability$sample_id == sample_id)
    if (length(hit) != 1L) {
      triomics_abort(
        "A_SOURCE_CONTRACT", "Availability does not name this block and sample once.",
        key = "availability", module = "resampling_preparation", stage = "A",
        remediation = "Keep one availability row per block and sample."
      )
    }
    presence <- as.character(availability$presence[[hit]])
    eligible <- isTRUE(availability$cell_imputation_eligible[[hit]])
    in_assay <- sample_id %in% colnames(study$experiments[[block_id]])
    if (identical(presence, "structural_absence")) {
      if (in_assay || eligible) {
        triomics_abort(
          "A_SOURCE_ABSENCE", "A structurally absent view cannot be imputed as cells.",
          key = "availability", module = "resampling_preparation", stage = "A",
          remediation = "Omit the absent view. Do not fill it with NA and impute."
        )
      }
      rows[[length(rows) + 1L]] <- data.frame(
        block_id = block_id, sample_id = sample_id, presence = "structural_absence",
        imputed = FALSE, stringsAsFactors = FALSE
      )
    } else if (!identical(presence, "present") || !in_assay) {
      triomics_abort(
        "A_SOURCE_ABSENCE", "A present sample is missing from the block assay.",
        key = "availability", module = "resampling_preparation", stage = "A",
        remediation = "Do not treat an omitted column as a cell-missing view unless absence is declared."
      )
    } else {
      present <- c(present, sample_id)
      rows[[length(rows) + 1L]] <- data.frame(
        block_id = block_id, sample_id = sample_id, presence = "present",
        imputed = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  list(present = present, rows = rows)
}

triomics_fold_fit_block <- function(study, spec, block_spec, training_ids, context_id) {
  block_id <- block_spec$block_id
  presence <- triomics_fold_presence(study, block_id, training_ids)
  if (!length(presence$present)) {
    triomics_abort(
      "A_SOURCE_ABSENCE", "A fold has no present samples to fit for a block.",
      key = "training_unit_ids", module = "resampling_preparation", stage = "A",
      remediation = "Do not invent an assay for a wholly absent training view."
    )
  }
  se <- triomics_fold_copy_se(study$experiments[[block_id]])
  se <- se[, presence$present, drop = FALSE]
  sub <- study
  sub$experiments <- stats::setNames(list(se), block_id)
  sub$mae <- triomics_mae_from_experiments(sub)
  local_spec <- spec
  local_spec$preparation_context_id <- context_id
  local_spec$training_unit_ids <- presence$present
  local_spec$blocks <- list(block_spec)
  fitted <- prepare_fit(sub, local_spec)
  ref <- fitted$state_refs[[block_id]]
  state <- triomics_read_state(ref, spec$state_dir)
  snapshot <- triomics_fold_parameter_snapshot(state)
  snapshot$state_ref <- ref
  snapshot
}

triomics_fold_context_payload <- function(block_id, se, state_hash, unit_subject) {
  values <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  values <- matrix(as.numeric(values), nrow = nrow(values), dimnames = dimnames(values))
  mask <- matrix(as.character(mask), nrow = nrow(mask), dimnames = dimnames(mask))
  sample_ids <- colnames(values)
  sample_map <- data.frame(
    block_id = block_id, sample_id = sample_ids, assay_column = sample_ids, stringsAsFactors = FALSE
  )
  subjects <- as.character(unit_subject$subject_id[match(sample_ids, unit_subject$unit_id)])
  unit_map <- data.frame(
    block_id = block_id, unit_id = sample_ids, subject_id = subjects, stringsAsFactors = FALSE
  )
  map_rows <- lapply(seq_len(nrow(sample_map)), function(i) as.list(sample_map[i, , drop = FALSE]))
  list(
    feature_keys = unname(rownames(values)),
    sample_ids = unname(sample_ids),
    values = values,
    mask = mask,
    sample_map = sample_map,
    unit_map = unit_map,
    values_hash = triomics_hash_text(triomics_canonical_json(triomics_matrix_payload(values))),
    map_hash = triomics_hash_text(triomics_canonical_json(map_rows)),
    state_hash = state_hash
  )
}

triomics_fold_prepare_apply <- function(study, preparation_spec, fold_record) {
  triomics_fold_reject_noninductive(preparation_spec)
  if (is.null(study$experiments)) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Apply needs an assembled study.",
      key = "experiments", module = "resampling_preparation", stage = "A",
      remediation = "Assemble the study before applying a fold state."
    )
  }
  result <- study
  state_hash <- NULL
  blocks <- list()
  for (block_spec in preparation_spec$blocks) {
    block_id <- block_spec$block_id
    recorded <- fold_record$blocks[[block_id]]
    if (is.null(recorded)) {
      triomics_abort(
        "A_STATE_PARTIAL", paste0("No fold state is recorded for block '", block_id, "'."),
        key = "state_ref", module = "resampling_preparation", stage = "A",
        remediation = "Apply the fold that was fit. Do not refit."
      )
    }
    se <- study$experiments[[block_id]]
    keys <- unname(as.character(recorded$feature_keys))
    got <- unname(as.character(rownames(se)))
    if (!setequal(got, keys)) {
      triomics_abort(
        "A_STATE_FEATURES", "Apply feature set does not match the frozen fit state; refusing silent reselection.",
        key = "feature_key", module = "resampling_preparation", stage = "A",
        remediation = "Supply the frozen feature keys. Do not drop or add features on apply."
      )
    }
    if (!identical(got, keys)) {
      triomics_abort(
        "A_STATE_RESELECT", "Apply feature order does not match the frozen fit state.",
        key = "feature_key", module = "resampling_preparation", stage = "A",
        remediation = "Keep the fit feature order. Do not reorder features on apply."
      )
    }
    path <- file.path(preparation_spec$state_dir, recorded$state_ref$path)
    before <- triomics_sha256(path)
    local_spec <- preparation_spec
    local_spec$preparation_context_id <- fold_record$preparation_context_id
    local_spec$training_unit_ids <- recorded$training_unit_ids
    local_spec$blocks <- list(block_spec)
    sub <- study
    sub$experiments <- stats::setNames(list(triomics_fold_copy_se(se)), block_id)
    sub$mae <- triomics_mae_from_experiments(sub)
    applied <- prepare_apply(sub, local_spec, stats::setNames(list(recorded$state_ref), block_id))
    if (identical(applied$predictive_status[[block_id]], "unavailable")) {
      triomics_abort(
        "A_STATE_PREDICT",
        paste0("Non-inductive method '", block_id, "' rejects unsupported prediction."),
        key = block_id, module = "resampling_preparation", stage = "A",
        remediation = "Do not skip a non-inductive apply and do not refit it on the full cohort."
      )
    }
    after <- triomics_sha256(path)
    if (!identical(before, after)) {
      triomics_abort(
        "A_STATE_PREDICT", "Apply rewrote the fit state.",
        key = "state_hash", module = "resampling_preparation", stage = "A",
        remediation = "Apply must read the frozen state and must not refit."
      )
    }
    result$experiments[[block_id]] <- applied$experiments[[block_id]]
    state_hash <- recorded$state_hash
    blocks[[block_id]] <- recorded
  }
  result$mae <- triomics_mae_from_experiments(result)
  list(study = result, state_hash = state_hash, blocks = blocks, unchanged = TRUE)
}

triomics_fold_make <- function(study, spec, fold, plan) {
  context_id <- paste(spec$preparation_context_id, fold$fold_id, sep = "__")
  blocks <- list()
  disposition <- list()
  cohort <- c(fold$training_unit_ids, fold$test_unit_ids)
  for (block_spec in spec$blocks) {
    block_id <- block_spec$block_id
    blocks[[block_id]] <- triomics_fold_fit_block(study, spec, block_spec, fold$training_unit_ids, context_id)
    presence <- triomics_fold_presence(study, block_id, cohort)
    disposition <- c(disposition, presence$rows)
  }
  aligned <- study
  aligned$experiments <- list()
  for (block_spec in spec$blocks) {
    block_id <- block_spec$block_id
    present <- triomics_fold_presence(study, block_id, cohort)$present
    se <- triomics_fold_copy_se(study$experiments[[block_id]])
    aligned$experiments[[block_id]] <- se[blocks[[block_id]]$feature_keys, present, drop = FALSE]
  }
  aligned$mae <- triomics_mae_from_experiments(aligned)
  apply_spec <- spec
  apply_spec$preparation_context_id <- context_id
  apply_spec$training_unit_ids <- fold$training_unit_ids
  provisional <- list(
    fold_id = fold$fold_id,
    preparation_context_id = context_id,
    training_unit_ids = fold$training_unit_ids,
    blocks = blocks
  )
  applied <- triomics_fold_prepare_apply(aligned, apply_spec, provisional)
  block_ids <- vapply(spec$blocks, function(block_spec) block_spec$block_id, character(1))
  shared <- lapply(block_ids, function(block_id) {
    triomics_fold_context_payload(
      block_id, applied$study$experiments[[block_id]], blocks[[block_id]]$state_hash, plan$unit_subject
    )
  })
  names(shared) <- block_ids
  disp <- do.call(rbind, disposition)
  for (i in seq_len(nrow(disp))) {
    if (!identical(as.character(disp$presence[[i]]), "present")) next
    mask <- shared[[disp$block_id[[i]]]]$mask
    sample_id <- disp$sample_id[[i]]
    if (sample_id %in% colnames(mask)) disp$imputed[[i]] <- any(mask[, sample_id] == "known_imputed")
  }
  inner <- list()
  if (length(fold$inner)) inner <- lapply(fold$inner, function(child) triomics_fold_make(study, spec, child, plan))
  list(
    fold_id = fold$fold_id,
    level = fold$level,
    parent_fold_id = fold$parent_fold_id,
    preparation_context_id = context_id,
    training_unit_ids = unname(as.character(fold$training_unit_ids)),
    test_unit_ids = unname(as.character(fold$test_unit_ids)),
    blocks = blocks,
    view_disposition = disp,
    contexts = list(
      B = list(preparation_context_id = context_id, role = "fold_prediction", blocks = shared),
      C = list(preparation_context_id = context_id, role = "fold_prediction", blocks = shared)
    ),
    inner = inner
  )
}

triomics_fold_prepare <- function(study, predictive_spec, split_plan, descriptive_spec) {
  if (is.null(study$experiments) || is.null(study$sample_registry)) {
    triomics_abort(
      "A_SOURCE_CONTRACT", "Fold preparation needs an assembled study.",
      key = "experiments", module = "resampling_preparation", stage = "A",
      remediation = "Assemble blocks before fitting a fold."
    )
  }
  triomics_validate_split_plan(split_plan, TRUE)
  triomics_fold_reject_noninductive(predictive_spec)
  if (!is.list(descriptive_spec) || !is.character(descriptive_spec$preparation_context_id) ||
      !is.character(predictive_spec$preparation_context_id) ||
      identical(descriptive_spec$preparation_context_id, predictive_spec$preparation_context_id)) {
    triomics_abort(
      "A_STATE_CONTEXT", "A primary descriptive context cannot share its id with fold prediction.",
      key = "preparation_context_id", module = "resampling_preparation", stage = "A",
      remediation = "Give the primary analysis and each fold prediction different preparation context ids."
    )
  }
  for (block_spec in descriptive_spec$blocks) {
    if (!identical(triomics_fold_impute_method(block_spec$operations), "none")) {
      triomics_abort(
        "A_STATE_CONTEXT", "A primary descriptive/inferential analysis cannot use an imputed context.",
        key = "impute", module = "resampling_preparation", stage = "A",
        remediation = "Keep imputation on the fold prediction context. Do not force it onto the primary analysis."
      )
    }
  }
  known <- as.character(study$sample_registry$sample_id)
  needed <- unique(c(split_plan$unit_subject$unit_id, descriptive_spec$training_unit_ids))
  if (any(!needed %in% known)) {
    triomics_abort(
      "A_ID_UNKNOWN_SAMPLE", "A split or descriptive unit is not in the sample registry.",
      key = "sample_id", module = "resampling_preparation", stage = "A",
      remediation = "Use registry sample ids. Do not invent held-out rows."
    )
  }
  primary_blocks <- list()
  for (block_spec in descriptive_spec$blocks) {
    block_id <- block_spec$block_id
    primary_blocks[[block_id]] <- triomics_fold_fit_block(
      study, descriptive_spec, block_spec, descriptive_spec$training_unit_ids,
      descriptive_spec$preparation_context_id
    )
  }
  primary_values <- list()
  for (block_spec in descriptive_spec$blocks) {
    block_id <- block_spec$block_id
    presence <- triomics_fold_presence(study, block_id, descriptive_spec$training_unit_ids)
    se <- triomics_fold_copy_se(study$experiments[[block_id]])
    se <- se[, presence$present, drop = FALSE]
    sub <- study
    sub$experiments <- stats::setNames(list(se), block_id)
    sub$mae <- triomics_mae_from_experiments(sub)
    local_spec <- descriptive_spec
    local_spec$training_unit_ids <- presence$present
    local_spec$blocks <- list(block_spec)
    fitted <- prepare_fit(sub, local_spec)
    primary_values[[block_id]] <- triomics_fold_context_payload(
      block_id, fitted$experiments[[block_id]], primary_blocks[[block_id]]$state_hash, split_plan$unit_subject
    )
  }
  folds <- lapply(split_plan$folds, function(fold) triomics_fold_make(study, predictive_spec, fold, split_plan))
  list(
    split_plan = split_plan,
    primary = list(
      role = "descriptive_inferential",
      preparation_context_id = descriptive_spec$preparation_context_id,
      imputed = FALSE,
      blocks = primary_values
    ),
    folds = folds
  )
}
