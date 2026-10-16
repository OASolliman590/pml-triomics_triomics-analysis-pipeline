triomics_stage_registry <- function() {
  data.frame(
    stage = c("A", "B", "C", "D", "report", "all"),
    validation_only = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    execution = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
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
