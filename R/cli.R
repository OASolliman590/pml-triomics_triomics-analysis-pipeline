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
      triomics_execute_stage_b(options$config, options$output)
      if (isTRUE(registered_b09)) triomics_b09_cli_after_stage_b(args)
      if (isTRUE(registered_b08)) triomics_b08_cli_after_stage_b(args)
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
