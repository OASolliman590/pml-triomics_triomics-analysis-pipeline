# Packet C-05: MOFA2 runtime availability and the explicit unavailable state.
# Core MOFA is exploratory structure discovery. Nothing here fits a model: the
# probes only report whether the native runtimes load, and the unavailable state
# carries no factors, weights, variance or reload agreement.

triomics_mofa_required_packages <- function() c("MOFA2", "mofapy2")

triomics_mofa_r_package_status <- function(package, library = NULL) {
  loaded <- suppressWarnings(requireNamespace(package, lib.loc = library, quietly = TRUE))
  list(
    package = package,
    loaded = isTRUE(loaded),
    version = if (isTRUE(loaded)) as.character(utils::packageVersion(package, lib.loc = library)) else NULL,
    reason = if (isTRUE(loaded)) NULL else "r_package_not_loadable"
  )
}

triomics_mofa_python_status <- function(python = Sys.getenv("TRIOMICS_MOFA_PYTHON", unset = "")) {
  not_loaded <- function(reason) list(package = "mofapy2", loaded = FALSE, version = NULL, reason = reason)
  if (!is.character(python) || length(python) != 1L || is.na(python) || !nzchar(trimws(python))) {
    return(not_loaded("python_not_configured"))
  }
  output <- tryCatch(
    suppressWarnings(system2(
      python, c("-c", shQuote("import mofapy2; print(mofapy2.__version__)")),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(error) structure(character(), status = 127L)
  )
  exit_status <- attr(output, "status")
  if (is.null(exit_status)) exit_status <- 0L
  if (!identical(as.integer(exit_status), 0L) || length(output) == 0L) {
    return(not_loaded("mofapy2_import_failed"))
  }
  list(package = "mofapy2", loaded = TRUE, version = trimws(output[[length(output)]]), reason = NULL)
}

triomics_mofa_default_probe <- function(package) {
  if (identical(package, "mofapy2")) triomics_mofa_python_status() else triomics_mofa_r_package_status(package)
}

triomics_mofa_backend_status <- function(probe = triomics_mofa_default_probe) {
  packages <- stats::setNames(lapply(triomics_mofa_required_packages(), probe),
                              triomics_mofa_required_packages())
  loaded <- vapply(packages, function(status) isTRUE(status$loaded), logical(1L))
  list(
    available = all(loaded),
    missing = names(packages)[!loaded],
    packages = packages
  )
}

# The explicit unavailable state is returned instead of a fit. It has no factors,
# weights or variance fields, and its reload check is marked not evaluated.
triomics_mofa_unavailable_state <- function(status) {
  list(
    status = "unavailable",
    method = "mofa2",
    trained = FALSE,
    reason = "mofa_runtime_not_loaded",
    missing_packages = status$missing,
    packages = status$packages,
    reload = list(status = "not_evaluated")
  )
}
