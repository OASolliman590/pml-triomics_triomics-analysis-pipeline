triomics_usage <- function() {
  paste(
    "Usage: triomics --config FILE --stage A|B|C|D|report|all --output DIR [--validate-only]",
    "       triomics --help",
    "",
    "Validation-only checks the requested implemented stage and writes a safe validation summary.",
    sep = "\n"
  )
}

triomics_parse_cli <- function(args) {
  options <- list(config = NULL, stage = NULL, output = NULL,
                  validate_only = FALSE, help = FALSE)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      options$help <- TRUE
      i <- i + 1L
      next
    }
    if (identical(arg, "--validate-only")) {
      if (isTRUE(options$validate_only)) {
        triomics_abort("A_CLI_ARGUMENT", "Option --validate-only was repeated.", key = arg,
                       module = "cli", remediation = "Provide each CLI option once.")
      }
      options$validate_only <- TRUE
      i <- i + 1L
      next
    }
    if (!arg %in% c("--config", "--stage", "--output")) {
      triomics_abort("A_CLI_ARGUMENT", paste0("Unknown command-line option '", arg, "'."),
                     key = arg, module = "cli", remediation = "Run triomics --help for supported options.")
    }
    if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
      triomics_abort("A_CLI_ARGUMENT", paste0("Option '", arg, "' requires a value."),
                     key = arg, module = "cli", remediation = "Provide a value for each CLI option.")
    }
    name <- switch(arg, `--config` = "config", `--stage` = "stage", `--output` = "output")
    if (!is.null(options[[name]])) {
      triomics_abort("A_CLI_ARGUMENT", paste0("Option '", arg, "' was repeated."),
                     key = arg, module = "cli", remediation = "Provide each CLI option once.")
    }
    options[[name]] <- args[[i + 1L]]
    i <- i + 2L
  }
  options
}

triomics_validate_only <- function(config, config_path, stage, output) {
  triomics_validate_config_paths(config, config_path)
  if (file.exists(output) && !dir.exists(output)) {
    triomics_abort("A_OUTPUT_INVALID", "Output path exists and is not a directory.", key = "output",
                   module = "cli", stage = stage,
                   remediation = "Choose a new local output directory.")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  summary <- list(
    schema_version = "2.0",
    study_id = config$study_id,
    stage = stage,
    status = "complete",
    validation_only = TRUE,
    config_sha256 = attr(config, "config_sha256"),
    implemented_stage = "A"
  )
  triomics_write_json_atomic(summary, file.path(output, "validation-summary.json"))
  invisible(summary)
}

triomics_cli_exit <- function(error) {
  if (identical(error$code, "A_STAGE_UNAVAILABLE")) {
    3L
  } else if (identical(error$code, "A_ARTIFACT_FINALIZE")) {
    4L
  } else {
    2L
  }
}

triomics_cli_prepare_registered_b09 <- function(options, args) {
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) ||
      is.null(options$config)) {
    return(FALSE)
  }
  config <- triomics_read_config(options$config)
  if (!isTRUE(config$clinical$enabled)) return(FALSE)
  if (identical(config$study_id, "triomics_b_t09_fixture")) {
    if (isNamespace(topenv(environment()))) {
      return(isTRUE(triomics_b09_prepare_cli(args)))
    }
    return(FALSE)
  }
  objectives <- config$clinical$objectives
  modes <- if (is.list(objectives) && length(objectives)) {
    vapply(objectives, function(objective) {
      if (is.null(objective$mode)) "" else as.character(objective$mode)
    }, character(1))
  } else character()
  if (any(modes %in% c("within_person", "baseline_to_later", "effect_modification"))) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Longitudinal objective execution requires a registered eligible endpoint bundle and clinical view.",
      key = "clinical.objectives.mode",
      remediation = paste(
        "Keep the declared objective unchanged and provide the accepted endpoint bundle and clinical view;",
        "the CLI must not substitute the public synthetic fixture."
      )
    )
  }
  FALSE
}

triomics_cli_prepare_registered_b08 <- function(options, args) {
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) ||
      is.null(options$config)) {
    return(FALSE)
  }
  config <- triomics_read_config(options$config)
  if (!isTRUE(config$clinical$enabled)) return(FALSE)
  if (identical(config$study_id, "triomics_b_t08_fixture")) {
    return(isTRUE(triomics_b08_prepare_cli(args)))
  }
  if (identical(config$study_id, "triomics_b_t09_fixture")) return(FALSE)
  objectives <- config$clinical$objectives
  if (is.list(objectives) && length(objectives)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Clinical objective execution requires a registered eligible endpoint bundle and clinical view.",
      key = "clinical.objectives",
      remediation = paste(
        "Keep the declared objective unchanged and provide the accepted endpoint bundle and clinical view;",
        "the CLI must not substitute the public B-T08 synthetic fixture."
      )
    )
  }
  FALSE
}

triomics_c01_matrix_payload <- function(value, row_key, column_key) {
  list(
    rows = stats::setNames(list(rownames(value)), row_key),
    columns = stats::setNames(list(colnames(value)), column_key),
    values = unname(lapply(seq_len(nrow(value)), function(i) {
      unname(as.list(as.numeric(value[i, , drop = TRUE])))
    }))
  )
}

triomics_c01_result_payload <- function(result) {
  result <- triomics_validate_integration_result(result)
  loadings <- lapply(names(result$loadings), function(block_id) {
    c(
      list(block_id = block_id),
      triomics_c01_matrix_payload(
        result$loadings[[block_id]], "feature_keys", "component_ids"
      )
    )
  })
  variance <- c(
    triomics_c01_matrix_payload(result$variance, "block_ids", "component_ids"),
    list(
      metric = attr(result$variance, "metric"),
      assay_space = attr(result$variance, "assay_space"),
      denominator = attr(result$variance, "denominator"),
      semantics = attr(result$variance, "semantics")
    )
  )
  list(
    scores = triomics_c01_matrix_payload(result$scores, "unit_ids", "component_ids"),
    loadings = loadings,
    variance = variance,
    selected = result$selected,
    backend = result$backend,
    params = result$params
  )
}

triomics_c02_block_operations <- function(block) {
  preprocessing <- block$preprocessing
  filter_method <- if (!is.null(preprocessing$filter$prevalence) ||
                       !is.null(preprocessing$filter$min_observed)) "prevalence" else "none"
  derive <- if (length(preprocessing$derive)) preprocessing$derive else "none"
  profile <- block$profile
  if (identical(profile, "proteomics")) {
    return(triomics_proteomics_operations(
      filter_method = filter_method,
      transform = preprocessing$transform$method,
      normalise = preprocessing$normalisation$method,
      impute = preprocessing$imputation$method,
      min_observed = preprocessing$filter$min_observed,
      prevalence = preprocessing$filter$prevalence,
      zero_policy = preprocessing$transform$zero_policy,
      zero_value = preprocessing$transform$zero_value,
      reference = preprocessing$normalisation$reference,
      scope = preprocessing$filter$scope,
      stratum = preprocessing$filter$stratum,
      selection_justification = preprocessing$filter$selection_justification,
      derive = derive,
      tail_probability = preprocessing$imputation$tail_probability,
      scale_multiplier = preprocessing$imputation$scale_multiplier
    ))
  }
  if (identical(profile, "metabolomics")) {
    return(triomics_metabolomics_operations(
      filter_method = filter_method,
      transform = preprocessing$transform$method,
      normalise = preprocessing$normalisation$method,
      impute = preprocessing$imputation$method,
      min_observed = preprocessing$filter$min_observed,
      prevalence = preprocessing$filter$prevalence,
      zero_policy = preprocessing$transform$zero_policy,
      zero_value = preprocessing$transform$zero_value,
      reference = preprocessing$normalisation$reference,
      reference_file = preprocessing$normalisation$reference_file,
      scope = preprocessing$filter$scope,
      stratum = preprocessing$filter$stratum,
      selection_justification = preprocessing$filter$selection_justification,
      k = preprocessing$imputation$k,
      trees = preprocessing$imputation$trees,
      derive = derive
    ))
  }
  if (identical(profile, "lipidomics")) {
    return(triomics_lipidomics_operations(
      filter_method = filter_method,
      transform = preprocessing$transform$method,
      normalise = preprocessing$normalisation$method,
      impute = preprocessing$imputation$method,
      min_observed = preprocessing$filter$min_observed,
      prevalence = preprocessing$filter$prevalence,
      zero_policy = preprocessing$transform$zero_policy,
      zero_value = preprocessing$transform$zero_value,
      reference_set = preprocessing$transform$reference_set,
      log_base = preprocessing$transform$log_base,
      scope = preprocessing$filter$scope,
      metadata_column = preprocessing$normalisation$metadata_column,
      standards_map = preprocessing$normalisation$standards_map,
      derive = derive
    ))
  }
  triomics_integration_abort(
    "C_EVAL_PREPARATION_INVALID",
    paste0("Block profile '", profile, "' has no registered fold-preparation route."),
    key = block$block_id
  )
}

triomics_c02_json_safe <- function(value) {
  if (inherits(value, "table")) {
    matrix_value <- unclass(value)
    return(list(
      rows = rownames(matrix_value),
      columns = colnames(matrix_value),
      values = unname(lapply(seq_len(nrow(matrix_value)), function(i) {
        unname(as.list(as.integer(matrix_value[i, , drop = TRUE])))
      }))
    ))
  }
  if (is.list(value) && !is.data.frame(value)) {
    return(lapply(value, triomics_c02_json_safe))
  }
  value
}

triomics_c02_evaluation_payload <- function(result) {
  fold_summaries <- lapply(result$outer_folds, function(fold) {
    fold[setdiff(names(fold), c("backend_fit", "common_result", "training_transforms"))]
  })
  payload <- list(
    mode = result$mode,
    method = result$method,
    native_version = result$native_version,
    split_plan_hash = result$split_plan_hash,
    outer_folds = fold_summaries,
    predictions = result$predictions,
    metrics = triomics_c02_json_safe(result$metrics),
    baseline_comparison = triomics_c02_json_safe(result$baseline_comparison),
    uncertainty = result$uncertainty,
    failures = result$failures,
    requested_folds = result$requested_folds,
    n_eligible = result$n_eligible,
    n_predicted = result$n_predicted,
    failed_prediction_count = result$failed_prediction_count,
    n_ineligible = result$n_ineligible,
    total_denominator = result$total_denominator,
    inspected_fit_count = result$inspected_fit_count,
    max_fits = result$max_fits
  )
  triomics_c02_json_safe(payload)
}

triomics_c02_evaluate_request <- function(config, ingested, analysis, request,
                                           block_ids, unit_ids, output) {
  block_config <- stats::setNames(config$blocks, vapply(config$blocks, `[[`, character(1), "block_id"))
  block_specs <- lapply(block_ids, function(block_id) {
    block <- block_config[[block_id]]
    triomics_export_ensure_profile(block$profile)
    list(
      block_id = block_id,
      profile_id = block$profile,
      operations = triomics_c02_block_operations(block)
    )
  })
  descriptive_blocks <- lapply(block_specs, function(block) {
    block$operations <- lapply(block$operations, function(operation) {
      if (identical(operation$stage, "impute")) {
        operation$method <- "none"
        operation$apply <- NULL
      }
      operation
    })
    block
  })
  display_names <- stats::setNames(vapply(block_ids, function(block_id) {
    as.character(block_config[[block_id]]$name)
  }, character(1)), block_ids)
  outcome <- request$outcome
  assembled_input <- ingested
  assembled_input$sample_registry[[outcome]] <- ingested$sample_metadata[[outcome]]
  assembled <- triomics_assemble_study(
    assembled_input, alignment = "strict", display_names = display_names
  )
  evaluation <- request$evaluation
  subject_column <- analysis$design$subject_id
  subject_ids <- if (identical(evaluation$generalization_unit, "subject") &&
                     is.character(subject_column) && length(subject_column) == 1L &&
                     nzchar(subject_column)) {
    as.character(ingested$sample_metadata[[subject_column]])
  } else unit_ids
  stratification <- if (is.character(evaluation$stratification) &&
                        length(evaluation$stratification) == 1L &&
                        nzchar(evaluation$stratification)) {
    as.character(ingested$sample_metadata[[evaluation$stratification]])
  } else NULL
  split_plan <- triomics_nested_split_plan(
    unit_ids,
    subject_ids = subject_ids,
    n_outer = evaluation$outer_folds,
    n_inner = evaluation$inner_folds,
    seed = config$execution$seed,
    stratification = stratification,
    target = outcome
  )
  levels <- ingested$declared_factor_levels[[outcome]]
  outcome_spec <- if (!is.null(levels)) {
    list(column = outcome, type = "categorical", levels = as.character(levels))
  } else list(column = outcome, type = "continuous")
  state_root <- file.path(output, "C", "state", request$integration_id)
  analysis_spec <- list(
    analysis_id = analysis$analysis_id,
    outcome = outcome_spec,
    predictive_spec = list(
      preparation_context_id = paste0(analysis$preparation_context_id, "__predictive"),
      seed = config$execution$seed,
      state_dir = file.path(state_root, "predictive"),
      blocks = block_specs
    ),
    descriptive_spec = list(
      preparation_context_id = paste0(analysis$preparation_context_id, "__descriptive"),
      training_unit_ids = unit_ids,
      seed = config$execution$seed,
      state_dir = file.path(state_root, "descriptive"),
      blocks = descriptive_blocks
    )
  )
  n_components <- as.integer(request$n_components)
  integration_spec <- request
  integration_spec$block_ids <- block_ids
  integration_spec$candidates <- lapply(seq_len(n_components), function(component) {
    list(id = paste0("ncomp-", component), ncomp = component)
  })
  integration_spec$max_fits <- evaluation$max_fits
  integration_spec$ncomp <- n_components
  integration_spec$task_mode <- outcome_spec$type
  integration_spec$required_views <- block_ids
  triomics_c02_evaluation_payload(
    evaluate_pipeline(assembled, analysis_spec, integration_spec, split_plan)
  )
}

triomics_execute_stage_c <- function(config_path, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_abort(
      "A_OUTPUT_INVALID", "Stage C output must be a directory path.",
      key = "output", module = "integration", stage = "C",
      remediation = "Choose a new local output directory."
    )
  }
  if (file.exists(output) && !dir.exists(output)) {
    triomics_abort(
      "A_OUTPUT_INVALID", "Stage C output exists and is not a directory.",
      key = "output", module = "integration", stage = "C",
      remediation = "Choose a new local output directory."
    )
  }

  config <- triomics_read_config(config_path)
  if (length(config$analyses) != 1L) {
    triomics_integration_abort(
      "C_ELIGIBILITY_INPUT_INVALID",
      "The registered C-01 CLI lane requires exactly one declared analysis.",
      key = "analyses"
    )
  }
  analysis <- config$analyses[[1L]]
  requests <- analysis$integration
  if (!is.list(requests) || !length(requests)) {
    triomics_integration_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "Stage C needs at least one explicit integration request.",
      key = "analyses.integration"
    )
  }
  requested_methods <- vapply(requests, function(request) {
    method <- request$method
    if (!is.character(method) || length(method) != 1L || is.na(method)) "" else method
  }, character(1))
  unsupported <- which(!requested_methods %in% triomics_integration_backend_names())
  if (length(unsupported)) {
    triomics_integration_backend(requested_methods[[unsupported[[1L]]]])
  }

  ingested <- triomics_ingest_configured_study(config, config_path)
  unit_ids <- as.character(ingested$sample_metadata$sample_id)
  representation <- analysis$design$representation
  block_ids <- as.character(unlist(analysis$block_ids, use.names = FALSE))
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = data.frame(
      unit_id = unit_ids,
      representation_id = rep(representation, length(unit_ids)),
      stringsAsFactors = FALSE
    )
  )
  resolved <- triomics_resolve_analysis(study, analysis)

  integration_results <- lapply(requests, function(request) {
    if (identical(request$evaluation$mode, "nested_cv")) {
      evaluation <- triomics_c02_evaluate_request(
        config, ingested, analysis, request, block_ids, unit_ids, output
      )
      return(list(
        integration_id = request$integration_id,
        method = request$method,
        required = isTRUE(request$required),
        status = "complete",
        evaluation = evaluation
      ))
    }
    spec <- request
    spec$block_ids <- block_ids
    spec$unit_ids <- unit_ids
    spec$training_unit_ids <- unit_ids
    spec$target_unit_ids <- unit_ids
    spec$required_views <- block_ids
    spec$task_mode <- request$evaluation$mode
    result <- integrate_blocks(resolved, spec)
    list(
      integration_id = request$integration_id,
      method = request$method,
      required = isTRUE(request$required),
      status = "complete",
      result = triomics_c01_result_payload(result)
    )
  })

  payload <- list(
    schema_version = "2.0",
    study_id = ingested$study_id,
    analysis_id = analysis$analysis_id,
    preparation_context_id = analysis$preparation_context_id,
    representation_id = representation,
    status = "complete",
    integrations = integration_results
  )
  payload_path <- file.path(output, "C", "integration-results.json")
  triomics_write_json_atomic(payload, payload_path)
  inputs <- unname(lapply(ingested$source_manifests, function(source) {
    list(artifact_id = source$source_id, sha256 = source$sha256)
  }))
  envelope <- triomics_artifact_envelope(
    artifact_type = "integration_results",
    artifact_id = "stage-c-integration-results",
    study_id = ingested$study_id,
    run_id = "c-01",
    status = "complete",
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = character(),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  envelope$analysis_id <- analysis$analysis_id
  envelope$preparation_context_id <- analysis$preparation_context_id
  envelope$representation_id <- representation
  triomics_write_envelope(envelope, file.path(output, "C", "integration-results.envelope.json"))
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "c-01",
    study_id = ingested$study_id,
    stage = "C",
    status = "complete"
  ), file.path(output, "registries", "run.json"))
  invisible(list(status = "complete", payload_path = payload_path, results = integration_results))
}

triomics_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch({
    options <- triomics_parse_cli(args)
    if (options$help) {
      cat(triomics_usage(), "\n")
      return(0L)
    }
    if (is.null(options$stage)) {
      triomics_abort("A_CLI_ARGUMENT", "Option --stage is required.", key = "stage",
                     module = "cli", remediation = "Run triomics --help for the stage grammar.")
    }
    triomics_require_stage(options$stage, options$validate_only)
    if (is.null(options$config) || is.null(options$output)) {
      triomics_abort("A_CLI_ARGUMENT", "Options --config and --output are required.",
                     key = if (is.null(options$config)) "config" else "output",
                     module = "cli", stage = options$stage,
                     remediation = "Provide --config FILE and --output DIR.")
    }
    if (isTRUE(options$validate_only)) {
      config <- triomics_read_config(options$config)
      triomics_validate_only(config, options$config, options$stage, options$output)
    } else if (identical(options$stage, "A")) {
      triomics_execute_stage_a(options$config, options$output)
    } else if (identical(options$stage, "B")) {
      registered_b09 <- triomics_cli_prepare_registered_b09(options, args)
      registered_b08 <- triomics_cli_prepare_registered_b08(options, args)
      stage_b_result <- triomics_execute_stage_b(options$config, options$output)
      if (isTRUE(registered_b09)) triomics_b09_cli_after_stage_b(args)
      if (isTRUE(registered_b08)) triomics_b08_cli_after_stage_b(args)
      triomics_b07_cli_after_stage_b(args, stage_b_result)
    } else if (identical(options$stage, "C")) {
      triomics_execute_stage_c(options$config, options$output)
    } else {
      triomics_abort("A_STAGE_UNAVAILABLE",
                     paste0("Execution is not implemented for stage '", options$stage, "'."),
                     key = "stage", module = "cli", stage = options$stage,
                     remediation = "Use --validate-only for stage A, or run an accepted stage.")
    }
    0L
  }, triomics_error = function(error) {
    cat(paste0("triomics ", error$code, ": ", conditionMessage(error), "\n"), file = stderr())
    triomics_cli_exit(error)
  }, error = function(error) {
    cat(paste0("triomics A_INTERNAL: ", conditionMessage(error), "\n"), file = stderr())
    1L
  })
}
