triomics_stage_registry <- function() {
  data.frame(
    stage = c("A", "B", "C", "D", "report", "all"),
    validation_only = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    execution = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

triomics_require_stage <- function(stage, validate_only = FALSE) {
  registry <- triomics_stage_registry()
  match <- match(stage, registry$stage)
  if (is.na(match)) {
    triomics_abort("A_STAGE_UNKNOWN", paste0("Unsupported stage '", stage, "'."),
                   key = "stage", stage = stage,
                   remediation = "Use A, B, C, D, report, or all.")
  }
  if (validate_only && !registry$validation_only[[match]]) {
    triomics_abort("A_STAGE_UNAVAILABLE", paste0("Validation is not implemented for stage '", stage, "'."),
                   key = "stage", stage = stage,
                   remediation = "Request validation-only for stage A until its consumer packet is accepted.")
  }
  if (!validate_only && !registry$execution[[match]]) {
    triomics_abort("A_STAGE_UNAVAILABLE", paste0("Execution is not implemented for stage '", stage, "'."),
                   key = "stage", stage = stage,
                   remediation = "Use --validate-only for the implemented stage A request.")
  }
  invisible(registry[match, , drop = FALSE])
}

triomics_install_stage_b_namespace_dispatch <- function(pkgname) {
  envir <- asNamespace(pkgname)
  install <- function(name, ...) {
    if (!exists(name, envir = envir, inherits = FALSE)) return(invisible(FALSE))
    get(name, envir = envir, inherits = FALSE)(...)
  }

  install("triomics_b02_install_namespace_dispatch", pkgname)
  install("triomics_b03_install_namespace_dispatch", pkgname)
  install("triomics_b04_install_namespace_dispatch", pkgname)
  install("triomics_b04_install_namespace_stage_hook", pkgname)
  install("triomics_b05_install_namespace_dispatch", pkgname)
  install("triomics_b06_install_namespace_dispatch", pkgname)
  if (exists("triomics_b06_install_source_context", envir = envir, inherits = FALSE)) {
    get("triomics_b06_install_source_context", envir = envir, inherits = FALSE)(
      envir, namespace = TRUE
    )
  }
  if (exists("triomics_fit_block_models", envir = envir, inherits = FALSE)) {
    composed <- get("triomics_fit_block_models", envir = envir, inherits = FALSE)
    for (marker in paste0("b_t0", 2:6, "_dispatch")) attr(composed, marker) <- TRUE
    get("triomics_b02_rebind", envir = envir, inherits = FALSE)(
      envir, "triomics_fit_block_models", composed
    )
    get("triomics_b02_rebind", envir = envir, inherits = FALSE)(
      envir, "fit_block_models", composed
    )
  }
  invisible(TRUE)
}

triomics_install_stage_b_global_dispatch <- function() {
  install <- function(name, ...) {
    if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
    get(name, envir = .GlobalEnv, inherits = FALSE)(...)
  }

  install("triomics_b02_install_global_dispatch")
  install("triomics_b03_install_global_dispatch")
  install("triomics_b04_install_global_dispatch")
  install("triomics_b04_install_stage_hook")
  install("triomics_b05_install_global_dispatch")
  install("triomics_b06_install_global_dispatch")
  if (exists("triomics_b06_install_source_context", envir = .GlobalEnv, inherits = FALSE)) {
    get("triomics_b06_install_source_context", envir = .GlobalEnv, inherits = FALSE)(
      .GlobalEnv, namespace = FALSE
    )
  }
  if (exists("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)) {
    composed <- get("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)
    for (marker in paste0("b_t0", 2:6, "_dispatch")) attr(composed, marker) <- TRUE
    assign("triomics_fit_block_models", composed, envir = .GlobalEnv)
    assign("fit_block_models", composed, envir = .GlobalEnv)
  }
  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  triomics_install_stage_b_namespace_dispatch(pkgname)
}


# This records producer artifacts, not later calibration or handoff acceptance.
triomics_b_write_run_registry <- function(record, output, config_sha256) {
  abort <- function(message, key = "registries") {
    triomics_abort("B_RUN_REGISTRY_INVALID", message, key = key,
                   module = "stage_registry", stage = "B",
                   remediation = "Use a fresh output root or preserve the same-run native A artifacts unchanged.")
  }
  path <- function(value, directory = FALSE) {
    tryCatch(triomics_d_secure_path(
      value, "B_RUN_REGISTRY_INVALID", "The run-registry path is unsafe.",
      "registries", directory = directory
    ), triomics_error = function(error) {
      error$module <- "stage_registry"
      error$stage <- "B"
      stop(error)
    })
  }
  read <- function(file) {
    file <- path(file)
    if (!file.exists(file) || dir.exists(file)) abort("A required run-registry artifact is missing.")
    value <- tryCatch({
      raw <- readChar(file, file.info(file)$size, useBytes = TRUE)
      triomics_provenance_json_scan(raw)
      jsonlite::fromJSON(raw, simplifyVector = FALSE)
    }, error = function(error) NULL)
    if (!is.list(value) || is.null(names(value))) abort("A run-registry artifact is malformed.")
    value
  }
  fields <- c("schema_version", "run_id", "study_id", "stage", "status")
  valid_record <- function(value) {
    is.list(value) && identical(sort(names(value)), sort(fields)) &&
      all(vapply(value, triomics_nonempty_string, logical(1))) &&
      identical(value$schema_version, "2.0")
  }
  if (!valid_record(record) || !identical(record$stage, "B") ||
      !record$run_id %in% c("b-t01", "b-t04") ||
      !record$status %in% triomics_envelope_statuses() ||
      !triomics_sha256_text(config_sha256)) {
    abort("The B producer run-record identity is invalid.")
  }
  root <- path(output, directory = TRUE)
  if (!dir.exists(root)) abort("The B output root does not exist.")
  registry_dir <- path(file.path(root, "registries"), directory = TRUE)
  canonical <- path(file.path(registry_dir, "run-B.json"))
  legacy <- path(file.path(registry_dir, "run.json"))
  if (file.exists(canonical) || dir.exists(canonical)) {
    abort("A canonical B run record already exists.", "registries.run-B")
  }
  for (target in c(canonical, legacy)) {
    temporary <- path(paste0(target, ".tmp"))
    if (file.exists(temporary) || dir.exists(temporary)) {
      abort("An unfinished registry write already exists.")
    }
  }
  legacy_present <- file.exists(legacy) || dir.exists(legacy)
  if (legacy_present) {
    previous <- read(legacy)
    if (!valid_record(previous) || !identical(previous$stage, "A") ||
        !identical(previous$status, "complete") || !identical(previous$run_id, "a-t02") ||
        !identical(previous$study_id, record$study_id)) {
      abort("The compatibility record is not the complete same-study A producer record.")
    }
    payload_path <- path(file.path(root, "A", "assays.json"))
    envelope_path <- path(file.path(root, "A", "assays.envelope.json"))
    payload <- read(payload_path)
    envelope <- read(envelope_path)
    if (!isTRUE(triomics_validate_envelope(envelope_path)) ||
        !identical(envelope$schema_version, "2.0") ||
        !identical(envelope$artifact_type, "assays") ||
        !identical(envelope$artifact_id, "stage-a-assays") ||
        !identical(envelope$payload_file, "assays.json") ||
        !identical(envelope$payload_sha256, triomics_sha256(payload_path)) ||
        !identical(envelope$run_id, previous$run_id) ||
        !identical(envelope$study_id, record$study_id) ||
        !identical(envelope$status, "complete") ||
        !identical(envelope$config_sha256, config_sha256) ||
        !identical(payload$schema_version, "2.0") ||
        !identical(payload$study_id, record$study_id) || !is.list(payload$blocks)) {
      abort("The A registry does not match its native payload, envelope, and current configuration.")
    }
  }
  if (!dir.exists(registry_dir)) {
    registry_dir <- tryCatch(triomics_d_create_directory_safely(
      registry_dir, "B_RUN_REGISTRY_INVALID", "The registry directory could not be created safely.",
      "registries"
    ), triomics_error = function(error) {
      error$module <- "stage_registry"
      error$stage <- "B"
      stop(error)
    })
  }
  # Match write_json_atomic's serialized bytes before claiming any new file.
  bytes <- paste0(jsonlite::toJSON(record, auto_unbox = TRUE, null = "null",
                                  pretty = TRUE, digits = NA), "\n")
  expected_sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  attempted <- character()
  published <- FALSE
  on.exit({
    if (!published) for (target in attempted) {
      if (!triomics_is_symlink(target) && file.exists(target) && !dir.exists(target) &&
          identical(triomics_sha256(target), expected_sha256)) unlink(target)
    }
  }, add = TRUE)
  targets <- if (legacy_present) canonical else c(legacy, canonical)
  for (target in targets) {
    target <- path(target)
    if (file.exists(target) || dir.exists(target)) abort("A registry target appeared before publication.")
    attempted <- c(attempted, target)
    triomics_write_json_atomic(record, target)
    target <- path(target)
    if (!file.exists(target) || dir.exists(target) ||
        !identical(triomics_sha256(target), expected_sha256)) {
      abort("The newly written run record failed byte verification.")
    }
  }
  published <- TRUE
  invisible(canonical)
}
