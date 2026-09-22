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

capture_native <- function(command, args, log_path) {
  warnings <- character()
  output <- withCallingHandlers(
    system2(command, args, stdout = TRUE, stderr = TRUE),
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
  tarball_name <- paste0(package, "_", version, ".tar.gz")
  tarball <- file.path(options$output, tarball_name)
  if (file.exists(tarball) || file.exists(file.path(options$output, "build.log")) ||
      file.exists(file.path(options$output, "check.log"))) {
    stop("Output directory already contains a prior package-check run.", call. = FALSE)
  }
  rcmd <- if (.Platform$OS.type == "windows") file.path(R.home("bin"), "Rcmd.exe") else file.path(R.home("bin"), "R")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(options$output)
  command_prefix <- if (.Platform$OS.type == "windows") character() else "CMD"
  build <- capture_native(
    rcmd,
    c(command_prefix, "build", "--no-build-vignettes", shQuote(options$root)),
    file.path(options$output, "build.log")
  )
  if (build$status != 0L || length(build$warnings) > 0L || !file.exists(tarball)) {
    cat("R CMD build failed; see build.log.\n", file = stderr())
    return(1L)
  }
  check <- capture_native(
    rcmd,
    c(command_prefix, "check", "--no-manual", "--no-build-vignettes", shQuote(tarball_name)),
    file.path(options$output, "check.log")
  )
  check_dir <- file.path(options$output, paste0(package, ".Rcheck"))
  check_log <- file.path(check_dir, "00check.log")
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
