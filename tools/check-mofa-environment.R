script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "tools/check-mofa-environment.R"
# Rscript rewrites spaces in --file= to ~+~ before the script can read them.
script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
source(file.path(dirname(script_path), "..", "R", "mofa_state.R"), local = TRUE)

usage <- "Usage: check-mofa-environment.R --library LIBRARY_DIR [--python PYTHON_EXECUTABLE]"

parse_args <- function(args) {
  config <- list(library = NULL, python = Sys.getenv("TRIOMICS_MOFA_PYTHON", unset = ""))
  i <- 1L
  while (i <= length(args)) {
    option <- args[[i]]
    if (!option %in% c("--library", "--python") || i == length(args) || startsWith(args[[i + 1L]], "--")) {
      stop(usage, call. = FALSE)
    }
    config[[sub("^--", "", option)]] <- args[[i + 1L]]
    i <- i + 2L
  }
  if (is.null(config$library)) stop(usage, call. = FALSE)
  if (!dir.exists(config$library)) {
    stop("Library directory does not exist: ", config$library, call. = FALSE)
  }
  config
}

# Reports whether MOFA2 and mofapy2 load; it never fits a model.
check_mofa_environment <- function(args = commandArgs(trailingOnly = TRUE)) {
  config <- parse_args(args)
  library <- normalizePath(config$library, winslash = "/", mustWork = TRUE)
  packages <- list(
    MOFA2 = triomics_mofa_r_package_status("MOFA2", library = library),
    mofapy2 = triomics_mofa_python_status(config$python)
  )
  for (status in packages) {
    if (isTRUE(status$loaded)) {
      cat(status$package, ": loaded, version ", status$version, "\n", sep = "")
    } else {
      cat(status$package, ": not loaded (", status$reason, ")\n", sep = "")
    }
  }
  available <- all(vapply(packages, function(status) isTRUE(status$loaded), logical(1L)))
  if (available) {
    cat("Status: available. This check fitted no model; training evidence still requires the C-05 suite.\n")
  } else {
    cat("Status: unavailable. No model was fitted and none is claimed.\n")
  }
  invisible(available)
}

tryCatch(
  quit(save = "no", status = if (check_mofa_environment()) 0L else 1L),
  error = function(error) {
    cat("Environment check failed: ", conditionMessage(error), "\n", sep = "", file = stderr())
    quit(save = "no", status = 2L)
  }
)
