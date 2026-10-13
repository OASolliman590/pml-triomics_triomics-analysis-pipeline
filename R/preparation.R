triomics_meta <- function(se) {
  meta <- S4Vectors::metadata(se)
  if (is.null(meta$triomics)) meta$triomics <- list()
  meta$triomics
}

triomics_set_triomics <- function(se, triomics) {
  meta <- S4Vectors::metadata(se)
  meta$triomics <- triomics
  S4Vectors::metadata(se) <- meta
  se
}

triomics_runtime_versions <- function() {
  versions <- list(
    r = paste(R.version$major, R.version$minor, sep = "."),
    triomics = "0.1.0"
  )
  for (package in c("SummarizedExperiment", "MultiAssayExperiment")) {
    versions[[package]] <- as.character(utils::packageVersion(package))
  }
  versions
}

triomics_matrix_payload <- function(values) {
  list(
    rows = rownames(values),
    cols = colnames(values),
    data = lapply(seq_len(nrow(values)), function(i) {
      lapply(seq_len(ncol(values)), function(j) {
        cell <- values[i, j]
        if (is.character(values)) return(as.character(cell))
        if (is.na(cell)) NULL else as.numeric(cell)
      })
    })
  )
}

triomics_record_operation <- function(se, operation, input_scale, output_scale) {
  triomics <- triomics_meta(se)
  completed <- triomics$completed
  if (is.null(completed)) completed <- list()
  nonidempotent <- !operation$method %in% c("none", "identity")
  if (nonidempotent) {
    for (prior in completed) {
      if (identical(prior$stage, operation$stage) && identical(prior$method, operation$method)) {
        code <- if (identical(operation$method, "log2")) "A_SCALE_DOUBLE_LOG" else "A_STATE_REPEAT"
        triomics_abort(code, paste0("Operation '", operation$stage, "/", operation$method, "' already ran."),
                       key = operation$stage, module = "preparation", stage = "A",
                       remediation = "Restore an earlier checkpoint under a new preparation context before repeating it.")
      }
    }
  }
  if (identical(operation$method, "log2") && identical(triomics$scale, "log2")) {
    triomics_abort("A_SCALE_DOUBLE_LOG", "log2 cannot be applied to data that is already on the log2 scale.",
                   key = "transform", module = "preparation", stage = "A",
                   remediation = "Keep the declared input scale. Do not log transformed values again.")
  }
  recorded <- operation
  recorded$input_scale <- input_scale
  recorded$output_scale <- output_scale
  completed[[length(completed) + 1L]] <- recorded
  triomics$completed <- completed
  triomics$scale <- output_scale
  se <- triomics_set_triomics(se, triomics)
  row_data <- SummarizedExperiment::rowData(se)
  if ("scale" %in% names(row_data)) {
    row_data$scale <- rep(output_scale, nrow(row_data))
    SummarizedExperiment::rowData(se) <- row_data
  }
  se
}

triomics_merge_learned <- function(se, partial) {
  triomics <- triomics_meta(se)
  if (is.null(triomics$state_payload)) triomics$state_payload <- list(learned_parameters = list())
  if (is.null(triomics$state_payload$learned_parameters)) triomics$state_payload$learned_parameters <- list()
  for (name in names(partial)) triomics$state_payload$learned_parameters[[name]] <- partial[[name]]
  triomics_set_triomics(se, triomics)
}

triomics_shared_validate <- function(se, params) {
  triomics_validate_se_dimensions(se)
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_count_mask <- function(codes, values) {
  list(
    n_observed = sum(codes == "known_observed" & is.finite(values)),
    n_missing = sum(codes == "known_missing"),
    n_imputed = sum(codes == "known_imputed"),
    n_unknown = sum(codes == "unknown")
  )
}

triomics_shared_filter_prevalence <- function(se, params) {
  if (identical(params$mode, "apply")) {
    keys <- as.character(params$fitted_state$feature_keys)
    if (any(!keys %in% rownames(se))) {
      triomics_abort("A_STATE_FEATURES", "Apply data is missing a feature from the frozen fit universe.",
                     key = "feature_key", module = "preparation", stage = "A",
                     remediation = "Do not reselect features on apply. Supply the fit feature keys.")
    }
    se <- se[keys, , drop = FALSE]
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  training <- params$training_unit_ids
  if (!is.character(training) || !length(training) || any(!training %in% colnames(se))) {
    triomics_abort("A_STATE_PARTIAL", "Prevalence filtering requires the declared training samples.",
                   key = "training_unit_ids", module = "preparation", stage = "A",
                   remediation = "Pass training ids. Do not count held-out or imputed cells.")
  }
  minimum <- params$operation$min_observed
  prevalence <- params$operation$prevalence
  if (is.null(minimum) && is.null(prevalence)) {
    triomics_abort("A_CONFIG_OPERATION", "Prevalence filtering requires a declared minimum.",
                   key = "filter", module = "preparation", stage = "A",
                   remediation = "Set min_observed or prevalence. There is no hidden default.")
  }
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  keep <- logical(nrow(se))
  flow <- vector("list", nrow(se))
  for (i in seq_len(nrow(se))) {
    counts <- triomics_count_mask(mask[i, training], analysis[i, training])
    ok <- TRUE
    if (!is.null(minimum)) ok <- ok && counts$n_observed >= minimum
    if (!is.null(prevalence)) ok <- ok && (counts$n_observed / length(training)) >= prevalence
    keep[[i]] <- ok
    flow[[i]] <- data.frame(
      feature_key = rownames(se)[[i]],
      block_id = params$block_id,
      stage = "filter",
      retained = ok,
      reason = if (ok) "retained" else "below_min_observed",
      n_observed = counts$n_observed,
      n_missing = counts$n_missing,
      n_imputed = counts$n_imputed,
      n_unknown = counts$n_unknown,
      stringsAsFactors = FALSE
    )
  }
  se <- se[keep, , drop = FALSE]
  triomics <- triomics_meta(se)
  triomics$feature_flow <- do.call(rbind, flow)
  se <- triomics_set_triomics(se, triomics)
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_shared_filter <- function(se, params) {
  method <- params$operation$method
  if (identical(method, "none")) {
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  if (identical(method, "prevalence")) return(triomics_shared_filter_prevalence(se, params))
  triomics_abort("A_CONFIG_METHOD", paste0("Shared profiles have no filter method '", method, "'."),
                 key = "filter", module = "preparation", stage = "A",
                 remediation = "Register a profile function for methods that are not shared.")
}

triomics_shared_transform_log2 <- function(se, params) {
  scale <- triomics_meta(se)$scale
  if (identical(scale, "log2")) {
    triomics_abort("A_SCALE_DOUBLE_LOG", "log2 cannot be applied to data that is already on the log2 scale.",
                   key = "transform", module = "preparation", stage = "A",
                   remediation = "Do not log a log2 assay again.")
  }
  if (!identical(scale, "linear")) {
    triomics_abort("A_SCALE_INPUT", "log2 requires a positive linear input scale.",
                   key = "transform", module = "preparation", stage = "A",
                   remediation = "Declare the input scale. Refusing an unknown scale is not a log.")
  }
  zero_policy <- params$operation$zero_policy
  if (is.null(zero_policy) || !nzchar(zero_policy)) zero_policy <- "error"
  zero_value <- params$operation$zero_value
  matrix <- SummarizedExperiment::assay(se, "analysis")
  finite <- is.finite(matrix)
  if (any(matrix[finite] < 0)) {
    triomics_abort("A_SCALE_NONPOSITIVE", "log2 cannot transform negative values.",
                   key = "transform", module = "preparation", stage = "A",
                   remediation = "Remove or expressly recode negative measurements before log2.")
  }
  zeros <- finite & matrix == 0
  if (any(zeros)) {
    if (!identical(zero_policy, "declared_replacement") || !is.numeric(zero_value) ||
        length(zero_value) != 1L || !is.finite(zero_value) || zero_value <= 0) {
      triomics_abort("A_SCALE_ZERO", "log2 found a zero without a declared positive replacement.",
                     key = "zero_policy", module = "preparation", stage = "A",
                     remediation = "Use zero_policy error or a declared positive zero_value. Do not invent a pseudocount.")
    }
    matrix[zeros] <- zero_value
  }
  matrix[is.finite(matrix)] <- log2(matrix[is.finite(matrix)])
  SummarizedExperiment::assay(se, "analysis") <- matrix
  learned <- list(zero_policy = zero_policy)
  if (identical(zero_policy, "declared_replacement")) learned$zero_value <- as.numeric(zero_value)
  se <- triomics_merge_learned(se, learned)
  triomics_record_operation(se, params$operation, "linear", "log2")
}

triomics_shared_transform <- function(se, params) {
  method <- params$operation$method
  if (identical(method, "none")) {
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  if (identical(method, "log2")) return(triomics_shared_transform_log2(se, params))
  triomics_abort("A_CONFIG_METHOD", paste0("Shared profiles have no transform method '", method, "'."),
                 key = "transform", module = "preparation", stage = "A",
                 remediation = "Register a profile function for methods that are not shared.")
}

triomics_shared_stage_none <- function(se, params, key) {
  if (!identical(params$operation$method, "none")) {
    triomics_abort("A_CONFIG_METHOD", paste0("Shared profiles have no ", key, " method '", params$operation$method, "'."),
                   key = key, module = "preparation", stage = "A",
                   remediation = "Register a profile function for methods that are not shared.")
  }
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_shared_derive <- function(se, params) triomics_shared_stage_none(se, params, "derive")

triomics_shared_normalise <- function(se, params) triomics_shared_stage_none(se, params, "normalise")

triomics_median_value <- function(state, feature_key) {
  medians <- state$learned_parameters$medians
  if (is.null(medians)) {
    triomics_abort("A_STATE_PARTIAL", "Training-median state has no stored medians.",
                   key = "learned_parameters", module = "preparation", stage = "A",
                   remediation = "Apply the fit state instead of recomputing medians.")
  }
  for (item in medians) {
    if (identical(as.character(item$feature_key), feature_key)) return(as.numeric(item$median))
  }
  triomics_abort("A_STATE_PARTIAL", paste0("No training median is stored for feature '", feature_key, "'."),
                 key = "feature_key", module = "preparation", stage = "A",
                 remediation = "Do not reestimate a median for a feature outside the fit state.")
}

triomics_shared_impute_training_median <- function(se, params) {
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  if (identical(params$mode, "apply")) {
    for (i in seq_len(nrow(se))) {
      median_value <- triomics_median_value(params$fitted_state, rownames(se)[[i]])
      for (j in seq_len(ncol(se))) {
        if (identical(mask[i, j], "known_missing")) {
          analysis[i, j] <- median_value
          mask[i, j] <- "known_imputed"
        }
      }
    }
    SummarizedExperiment::assay(se, "analysis") <- analysis
    SummarizedExperiment::assay(se, "original_mask") <- mask
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  training <- params$training_unit_ids
  if (!is.character(training) || any(!training %in% colnames(se))) {
    triomics_abort("A_STATE_PARTIAL", "Training-median imputation requires training sample ids.",
                   key = "training_unit_ids", module = "preparation", stage = "A",
                   remediation = "Fit medians on the declared training ids only.")
  }
  medians <- vector("list", nrow(se))
  for (i in seq_len(nrow(se))) {
    observed <- mask[i, training] == "known_observed" & is.finite(analysis[i, training])
    if (!any(observed)) {
      triomics_abort("A_SCALE_ALL_MISSING",
                     paste0("Feature '", rownames(se)[[i]], "' has no training observation to median."),
                     key = "impute", module = "preparation", stage = "A",
                     remediation = "Do not invent a median for an all-missing training feature.")
    }
    medians[[i]] <- list(feature_key = rownames(se)[[i]], median = stats::median(analysis[i, training][observed]))
  }
  for (i in seq_len(nrow(se))) {
    for (j in seq_len(ncol(se))) {
      if (identical(mask[i, j], "known_missing")) {
        analysis[i, j] <- medians[[i]]$median
        mask[i, j] <- "known_imputed"
      }
    }
  }
  SummarizedExperiment::assay(se, "analysis") <- analysis
  SummarizedExperiment::assay(se, "original_mask") <- mask
  se <- triomics_merge_learned(se, list(medians = medians))
  scale <- triomics_meta(se)$scale
  triomics_record_operation(se, params$operation, scale, scale)
}

triomics_shared_impute <- function(se, params) {
  method <- params$operation$method
  if (identical(method, "none")) {
    scale <- triomics_meta(se)$scale
    return(triomics_record_operation(se, params$operation, scale, scale))
  }
  if (identical(method, "training_median")) return(triomics_shared_impute_training_median(se, params))
  triomics_abort("A_CONFIG_METHOD", paste0("Shared profiles have no imputation method '", method, "'."),
                 key = "impute", module = "preparation", stage = "A",
                 remediation = "Register a profile function for methods that are not shared.")
}

triomics_baseline_profile <- function() {
  list(
    validate = triomics_shared_validate,
    filter = triomics_shared_filter,
    transform = triomics_shared_transform,
    normalise = triomics_shared_normalise,
    impute = triomics_shared_impute,
    derive = triomics_shared_derive
  )
}

triomics_freeze_operations <- function(block_spec) {
  operations <- block_spec$operations
  if (!is.list(operations) || !length(operations)) {
    triomics_abort("A_CONFIG_OPERATION", "Preparation operations must be an explicit ordered list.",
                   key = "operations", module = "preparation", stage = "A",
                   remediation = "Freeze validate, filter, derive, transform, normalise, and impute before execution.")
  }
  stages <- vapply(operations, function(operation) {
    if (!is.list(operation) || !is.character(operation$stage) || !is.character(operation$method)) {
      triomics_abort("A_CONFIG_OPERATION", "Each operation needs a stage and a method id.",
                     key = "operations", module = "preparation", stage = "A",
                     remediation = "Do not evaluate method names from free text.")
    }
    operation$stage
  }, character(1))
  expected <- c("validate", "filter", "derive", "transform", "normalise", "impute")
  if (length(stages) != length(expected) || anyDuplicated(stages) || !setequal(stages, expected) ||
      match("validate", stages) > match("filter", stages) ||
      match("filter", stages) > match("derive", stages) ||
      match("derive", stages) > min(match(c("transform", "normalise"), stages)) ||
      match("impute", stages) != length(stages)) {
    triomics_abort("A_CONFIG_OPERATION", "Operations must validate, filter, and derive before transform and normalise, with impute last.",
                   key = "operations", module = "preparation", stage = "A",
                   remediation = "Derive quantitative endpoints before log or closure.")
  }
  transform <- operations[[match("transform", stages)]]
  normalise <- operations[[match("normalise", stages)]]
  if (identical(normalise$method, "vsn") && !identical(transform$method, "none")) {
    triomics_abort("A_SCALE_DOUBLE_LOG", "VSN owns its combined transform and cannot receive a second log.",
                   key = "normalise", module = "preparation", stage = "A",
                   remediation = "Set the transform method to none when normalisation is vsn.")
  }
  operations
}

triomics_check_preparation_spec <- function(spec, require_training) {
  if (!is.list(spec) || !is.character(spec$preparation_context_id) || length(spec$preparation_context_id) != 1L ||
      is.na(spec$preparation_context_id) || !nzchar(spec$preparation_context_id)) {
    triomics_abort("A_CONFIG_OPERATION", "A preparation context id is required.",
                   key = "preparation_context_id", module = "preparation", stage = "A",
                   remediation = "Name the preparation context. Do not reuse a context for a repeated transform.")
  }
  if (!is.character(spec$state_dir) || length(spec$state_dir) != 1L || !nzchar(spec$state_dir)) {
    triomics_abort("A_CONFIG_OPERATION", "A state directory is required.",
                   key = "state_dir", module = "preparation", stage = "A",
                   remediation = "Write preparation state under an explicit directory.")
  }
  if (!is.numeric(spec$seed) || length(spec$seed) != 1L || !is.finite(spec$seed) || spec$seed != trunc(spec$seed)) {
    triomics_abort("A_CONFIG_OPERATION", "Preparation seed must be one finite integer.",
                   key = "seed", module = "preparation", stage = "A",
                   remediation = "Record the execution seed even when the shared methods are deterministic.")
  }
  if (require_training) {
    ids <- spec$training_unit_ids
    if (!is.character(ids) || !length(ids) || anyNA(ids) || anyDuplicated(ids)) {
      triomics_abort("A_STATE_PARTIAL", "Fit requires ordered training unit ids.",
                     key = "training_unit_ids", module = "preparation", stage = "A",
                     remediation = "Declare the training ids. Do not infer them from an outcome column.")
    }
  }
  if (!is.list(spec$blocks) || !length(spec$blocks)) {
    triomics_abort("A_CONFIG_OPERATION", "Preparation needs at least one block spec.",
                   key = "blocks", module = "preparation", stage = "A",
                   remediation = "Name each block id and profile id.")
  }
  spec$seed <- as.integer(spec$seed)
  spec
}

triomics_mask_lineage <- function(mask, feature_keys) {
  mask <- mask[feature_keys, , drop = FALSE]
  lapply(seq_len(nrow(mask)), function(i) {
    list(
      feature_key = unname(rownames(mask)[[i]]),
      samples = unname(colnames(mask)),
      codes = unname(as.character(mask[i, ]))
    )
  })
}

triomics_build_fit_payload <- function(se, spec, block_spec, initial_mask, initial_assay) {
  triomics <- triomics_meta(se)
  learned <- triomics$state_payload$learned_parameters
  if (is.null(learned)) learned <- list()
  operation_order <- triomics$completed
  if (is.null(operation_order) || !length(operation_order)) {
    triomics_abort("A_STATE_PARTIAL", "Fit finished without an operation order.",
                   key = "operation_order", module = "preparation", stage = "A",
                   remediation = "Record every stage, including no-op stages.")
  }
  lineage <- triomics_mask_lineage(initial_mask, rownames(se))
  list(
    mode = "fit",
    profile_id = block_spec$profile_id,
    block_id = block_spec$block_id,
    training_unit_ids = spec$training_unit_ids,
    training_unit_hash = triomics_hash_text(triomics_canonical_json(spec$training_unit_ids)),
    feature_keys = unname(rownames(se)),
    reference_values = if (is.null(learned$medians)) list() else learned$medians,
    reference_scale = triomics$scale,
    learned_parameters = learned,
    input_quantity = triomics$quantity,
    output_quantity = triomics$quantity,
    input_scale = operation_order[[1L]]$input_scale,
    output_scale = triomics$scale,
    mask_lineage = lineage,
    mask_hash = triomics_hash_text(triomics_canonical_json(lineage)),
    operation_order = operation_order,
    operation_hash = triomics_hash_text(triomics_canonical_json(operation_order)),
    rng_seed = spec$seed,
    rng_kind = RNGkind(),
    package_versions = triomics_runtime_versions(),
    fit_input_hashes = list(
      assay = triomics_hash_text(triomics_canonical_json(triomics_matrix_payload(initial_assay))),
      mask = triomics_hash_text(triomics_canonical_json(triomics_matrix_payload(initial_mask)))
    )
  )
}

triomics_snapshot <- function(se) {
  analysis <- SummarizedExperiment::assay(se, "analysis")
  mask <- SummarizedExperiment::assay(se, "original_mask")
  list(
    values = matrix(as.numeric(analysis), nrow = nrow(analysis), dimnames = dimnames(analysis)),
    mask = matrix(as.character(mask), nrow = nrow(mask), dimnames = dimnames(mask))
  )
}

triomics_run_profile <- function(se, profile, operations, params_base, record_shape) {
  checkpoints <- list()
  for (operation in operations) {
    triomics_validate_se_dimensions(se)
    before <- triomics_snapshot(se)
    before_count <- length(triomics_meta(se)$completed)
    if (is.null(before_count) || length(triomics_meta(se)$completed) == 0L) before_count <- 0L
    params <- params_base
    params$operation <- operation
    se <- profile[[operation$stage]](se, params)
    if (!methods::is(se, "SummarizedExperiment")) {
      triomics_abort("A_SOURCE_CONTRACT", "A profile method must return a SummarizedExperiment.",
                     key = operation$stage, module = "preparation", stage = "A",
                     remediation = "Keep the (se, params) -> SE signature.")
    }
    if (length(triomics_meta(se)$completed) == before_count) {
      scale <- triomics_meta(se)$scale
      se <- triomics_record_operation(se, operation, scale, scale)
    }
    triomics_validate_se_dimensions(se)
    after <- SummarizedExperiment::assay(se, "analysis")
    if (!identical(dim(before$values), dim(after))) {
      checkpoints[[length(checkpoints) + 1L]] <- list(
        block_id = params$block_id,
        stage = operation$stage,
        feature_keys = rownames(before$values),
        sample_ids = colnames(before$values),
        values = before$values,
        mask = before$mask
      )
    }
  }
  list(se = se, checkpoints = checkpoints)
}

prepare_fit <- function(study, preparation_spec) {
  if (is.null(study$experiments)) {
    triomics_abort("A_SOURCE_CONTRACT", "prepare_fit requires an assembled study.",
                   key = "experiments", module = "preparation", stage = "A",
                   remediation = "Assemble SE and MAE objects before fitting preparation.")
  }
  spec <- triomics_check_preparation_spec(preparation_spec, TRUE)
  study$checkpoints <- list()
  flows <- list()
  state_refs <- list()
  ledger <- list()
  for (block_spec in spec$blocks) {
    block_id <- block_spec$block_id
    if (!is.character(block_id) || length(block_id) != 1L || !block_id %in% names(study$experiments)) {
      triomics_abort("A_ID_REFERENCE", "Preparation names a block that is not in the study.",
                     key = "block_id", module = "preparation", stage = "A",
                     remediation = "Use the block id. A display name is not a join key.")
    }
    if (!is.character(block_spec$profile_id) || length(block_spec$profile_id) != 1L) {
      triomics_abort("A_CONFIG_PROFILE", "Each block preparation needs a profile id.",
                     key = "profile_id", module = "preparation", stage = "A",
                     remediation = "Dispatch on the registered profile id.")
    }
    profile <- triomics_registered_profile(block_spec$profile_id)
    operations <- triomics_freeze_operations(block_spec)
    se <- study$experiments[[block_id]]
    initial_mask <- SummarizedExperiment::assay(se, "original_mask")
    initial_assay <- SummarizedExperiment::assay(se, "analysis")
    ran <- triomics_run_profile(
      se, profile, operations,
      list(
        mode = "fit",
        training_unit_ids = spec$training_unit_ids,
        fitted_state = NULL,
        block_id = block_id,
        profile_id = block_spec$profile_id,
        seed = spec$seed
      )
    )
    se <- ran$se
    study$checkpoints <- c(study$checkpoints, ran$checkpoints)
    flow <- triomics_meta(se)$feature_flow
    if (is.data.frame(flow) && nrow(flow)) flows[[length(flows) + 1L]] <- flow
    payload <- triomics_build_fit_payload(se, spec, block_spec, initial_mask, initial_assay)
    ref <- triomics_externalize_state(payload, spec$state_dir)
    triomics <- triomics_meta(se)
    ledger[[block_id]] <- triomics$completed
    triomics$state_payload <- NULL
    triomics$feature_flow <- NULL
    triomics$state_ref <- ref
    se <- triomics_set_triomics(se, triomics)
    study$experiments[[block_id]] <- se
    state_refs[[block_id]] <- ref
  }
  study$state_refs <- state_refs
  study$operation_ledger <- ledger
  study$feature_flow <- if (length(flows)) do.call(rbind, flows) else data.frame()
  study$mae <- triomics_mae_from_experiments(study)
  study$predictive_status <- stats::setNames(rep("fit", length(state_refs)), names(state_refs))
  study
}

prepare_apply <- function(study, preparation_spec, state_refs) {
  if (is.null(study$experiments)) {
    triomics_abort("A_SOURCE_CONTRACT", "prepare_apply requires an assembled study.",
                   key = "experiments", module = "preparation", stage = "A",
                   remediation = "Assemble the study and pass immutable fit state refs.")
  }
  spec <- triomics_check_preparation_spec(preparation_spec, FALSE)
  if (!is.list(state_refs)) {
    triomics_abort("A_STATE_PARTIAL", "Apply requires state references.",
                   key = "state_refs", module = "preparation", stage = "A",
                   remediation = "Pass the fit state refs. Do not refit on the apply cohort.")
  }
  if (is.null(study$state_refs)) study$state_refs <- list()
  if (is.null(study$predictive_status)) study$predictive_status <- list()
  for (block_spec in spec$blocks) {
    block_id <- block_spec$block_id
    ref <- state_refs[[block_id]]
    if (is.null(ref)) {
      triomics_abort("A_STATE_PARTIAL", paste0("No state reference for block '", block_id, "'."),
                     key = "state_refs", module = "preparation", stage = "A",
                     remediation = "Apply cannot invent a fit state.")
    }
    state <- triomics_read_state(ref, spec$state_dir)
    operations <- triomics_freeze_operations(block_spec)
    frozen_apply_unavailable <- any(vapply(
      state$operation_order,
      function(operation) identical(operation$apply, "unavailable"),
      logical(1L)
    ))
    fit_operations <- lapply(state$operation_order, function(operation) {
      operation$input_scale <- NULL
      operation$output_scale <- NULL
      operation$apply <- NULL
      operation
    })
    requested_operations <- lapply(operations, function(operation) {
      operation$apply <- NULL
      operation
    })
    operations_match <- length(fit_operations) == length(requested_operations) && all(vapply(
      seq_along(requested_operations), function(i) {
        requested <- requested_operations[[i]]
        frozen <- fit_operations[[i]]
        requested_names <- names(requested)[!vapply(requested, is.null, logical(1L))]
        all(requested_names %in% names(frozen)) && all(vapply(requested_names, function(name) {
          identical(triomics_canonical_json(requested[[name]]), triomics_canonical_json(frozen[[name]]))
        }, logical(1L)))
      }, logical(1L)
    ))
    if (!operations_match) {
      triomics_abort("A_STATE_STALE", "Apply operations do not match the frozen fit parameters and order.",
                     key = "operations", module = "preparation", stage = "A",
                     remediation = "Use the same methods, parameters, and order as the fit state.")
    }
    if (frozen_apply_unavailable ||
        any(vapply(operations, function(operation) identical(operation$apply, "unavailable"), logical(1L)))) {
      study$predictive_status[[block_id]] <- "unavailable"
      study$state_refs[[block_id]] <- ref
      next
    }
    operations <- fit_operations
    profile <- triomics_registered_profile(block_spec$profile_id)
    se <- study$experiments[[block_id]]
    for (operation in operations) {
      triomics_validate_se_dimensions(se)
      se <- profile[[operation$stage]](se, list(
        mode = "apply",
        training_unit_ids = as.character(state$training_unit_ids),
        fitted_state = state,
        operation = operation,
        block_id = block_id,
        profile_id = block_spec$profile_id,
        seed = state$rng_seed
      ))
      if (!methods::is(se, "SummarizedExperiment")) {
        triomics_abort("A_SOURCE_CONTRACT", "A profile method must return a SummarizedExperiment.",
                       key = operation$stage, module = "preparation", stage = "A",
                       remediation = "Keep the (se, params) -> SE signature.")
      }
      triomics_validate_se_dimensions(se)
      if (!identical(operation$stage, "validate") &&
          !identical(rownames(se), as.character(state$feature_keys))) {
        triomics_abort("A_STATE_RESELECT", "Apply changed the frozen feature universe.",
                       key = "feature_key", module = "preparation", stage = "A",
                       remediation = "Do not drop or reorder features except to the fit feature keys.")
      }
    }
    triomics <- triomics_meta(se)
    triomics$state_payload <- NULL
    triomics$state_ref <- ref
    study$experiments[[block_id]] <- triomics_set_triomics(se, triomics)
    study$state_refs[[block_id]] <- ref
    study$predictive_status[[block_id]] <- "apply"
  }
  study$mae <- triomics_mae_from_experiments(study)
  study
}
