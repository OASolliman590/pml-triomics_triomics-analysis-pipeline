script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "tools/check-package.R"
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

parse_args <- function(args, default_root) {
  options <- list(root = default_root, output = NULL)
  seen <- character()
  i <- 1L
  while (i <= length(args)) {
    option <- args[[i]]
    if (!option %in% c("--root", "--output") || i == length(args) || startsWith(args[[i + 1L]], "--")) {
      stop("Usage: check-package.R [--root PACKAGE_DIR] [--output LOG_DIR]", call. = FALSE)
    }
    name <- if (identical(option, "--root")) "root" else "output"
    if (name %in% seen) stop(paste("Repeated option", option), call. = FALSE)
    seen <- c(seen, name)
    options[[name]] <- args[[i + 1L]]
    i <- i + 2L
  }
  if (is.null(options$output)) options$output <- file.path(options$root, "artifacts", "package-check")
  options$root <- normalizePath(options$root, winslash = "/", mustWork = TRUE)
  options$output <- normalizePath(options$output, winslash = "/", mustWork = FALSE)
  options
}

check_environment <- function() {
  keep <- c("PATH", "HOME", "TMPDIR", "LANG", "LC_ALL", "R_HOME")
  env <- Sys.getenv(keep, unset = NA_character_)
  env <- env[!is.na(env) & !grepl("[\r\n]", env)]
  mofa_library <- Sys.getenv("TRIOMICS_MOFA_LIBRARY", unset = "")
  if (!nzchar(mofa_library) && dir.exists("/tmp/triomics-mofa-lib")) {
    mofa_library <- "/tmp/triomics-mofa-lib"
  }
  python <- Sys.getenv("TRIOMICS_MOFA_PYTHON", unset = "")
  if (!nzchar(python)) {
    candidates <- c(
      "/tmp/triomics-mofa-py-m7-006/bin/python",
      "/tmp/triomics-mofa-py/bin/python"
    )
    python <- candidates[file.exists(candidates)][1L]
    if (is.na(python)) python <- ""
  }
  c(
    paste0(names(env), "=", env),
    paste0("TRIOMICS_MOFA_LIBRARY=", mofa_library),
    paste0("TRIOMICS_MOFA_PYTHON=", python)
  )
}

capture_native <- function(command, args, log_path) {
  warnings <- character()
  output <- withCallingHandlers(
    system2(command, args, stdout = TRUE, stderr = TRUE, env = check_environment()),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  writeLines(as.character(output), log_path, useBytes = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), warnings = warnings, output = as.character(output))
}

stage_package_source <- function(root, work, package) {
  source_dir <- file.path(work, paste0(package, "-source"))
  dir.create(source_dir)
  entries <- list.files(root, all.files = TRUE, no.. = TRUE, full.names = FALSE)
  prefixed <- file.path(package, entries)
  excluded <- tools:::inRbuildignore(prefixed, root)
  entries <- entries[!excluded]
  for (entry in entries) {
    copied <- file.copy(
      file.path(root, entry), source_dir,
      recursive = dir.exists(file.path(root, entry)),
      copy.date = TRUE
    )
    if (!isTRUE(copied)) stop("Could not stage package source entry: ", entry, call. = FALSE)
  }
  source_dir
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- parse_args(args, default_root)
  description_path <- file.path(options$root, "DESCRIPTION")
  if (!file.exists(description_path)) stop("Package root has no DESCRIPTION file.", call. = FALSE)
  metadata <- read.dcf(description_path, fields = c("Package", "Version"))
  package <- metadata[1L, "Package"]
  version <- metadata[1L, "Version"]
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", package) || !grepl("^[A-Za-z0-9.]+$", version)) {
    stop("DESCRIPTION has an invalid package name or version.", call. = FALSE)
  }
  dir.create(options$output, recursive = TRUE, showWarnings = FALSE)
  options$output <- normalizePath(options$output, winslash = "/", mustWork = TRUE)
  tarball_name <- paste0(package, "_", version, ".tar.gz")
  tarball <- file.path(options$output, tarball_name)
  if (file.exists(tarball) || file.exists(file.path(options$output, "build.log")) ||
      file.exists(file.path(options$output, "check.log"))) {
    stop("Output directory already contains a prior package-check run.", call. = FALSE)
  }
  appledouble <- list.files(options$root, pattern = "^\\._", all.files = TRUE,
                            recursive = TRUE, full.names = TRUE)
  if (length(appledouble) > 0L) unlink(appledouble)
  rcmd <- if (.Platform$OS.type == "windows") file.path(R.home("bin"), "Rcmd.exe") else file.path(R.home("bin"), "R")
  # Build and check on a local volume. External volumes inject AppleDouble sidecars
  # that R CMD check reports as package warnings.
  work <- tempfile("triomics-package-check-")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE)
  }, add = TRUE)
  staged_root <- stage_package_source(options$root, work, package)
  setwd(work)
  command_prefix <- if (.Platform$OS.type == "windows") character() else "CMD"
  build <- capture_native(
    rcmd,
    c(command_prefix, "build", "--no-build-vignettes", shQuote(staged_root)),
    file.path(options$output, "build.log")
  )
  built <- file.path(work, tarball_name)
  if (build$status != 0L || length(build$warnings) > 0L || !file.exists(built) ||
      !file.copy(built, tarball, overwrite = FALSE)) {
    cat("R CMD build failed; see build.log.\n", file = stderr())
    return(1L)
  }
  check <- capture_native(
    rcmd,
    c(command_prefix, "check", "--no-manual", "--no-build-vignettes", shQuote(tarball_name)),
    file.path(options$output, "check.log")
  )
  check_dir <- file.path(work, paste0(package, ".Rcheck"))
  check_log <- file.path(check_dir, "00check.log")
  # The work directory is removed on exit. Keep the test transcript where the
  # status line is not enough to name a warning.
  if (dir.exists(check_dir)) {
    rout <- list.files(check_dir, pattern = "testthat\\.Rout", recursive = TRUE, full.names = TRUE)
    for (path in rout) {
      file.copy(path, file.path(options$output, basename(path)), overwrite = TRUE)
    }
    if (file.exists(check_log)) {
      file.copy(check_log, file.path(options$output, "00check.log"), overwrite = TRUE)
    }
    warning_inventory <- list.files(
      check_dir, pattern = "^triomics-warning-inventory\\.json$",
      recursive = TRUE, full.names = TRUE
    )
    if (length(warning_inventory) != 1L || !file.copy(
      warning_inventory[[1L]], file.path(options$output, "warning-inventory.json"), overwrite = TRUE
    )) {
      cat("testthat warning inventory was not captured; see 00check.log.\n", file = stderr())
      return(1L)
    }
  }
  summary <- if (file.exists(check_log)) readLines(check_log, warn = FALSE) else character()
  status_line <- grep("^Status:", summary, value = TRUE)
  check_warning <- length(status_line) > 0L && !identical(trimws(status_line[[length(status_line)]]), "Status: OK")
  if (check$status != 0L || length(check$warnings) > 0L || check_warning || !file.exists(check_log)) {
    cat("R CMD check failed or reported an undisposed warning; see check.log and the .Rcheck directory.\n",
        file = stderr())
    return(1L)
  }
  cat("R CMD build and check passed for ", tarball_name, ". Logs: build.log, check.log\n", sep = "")
  0L
}

status <- tryCatch(main(), error = function(error) {
  cat("Package check wrapper: ", conditionMessage(error), "\n", sep = "", file = stderr())
  1L
})
quit(save = "no", status = as.integer(status))
