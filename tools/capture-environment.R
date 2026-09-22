script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "tools/capture-environment.R"
default_project <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

parse_args <- function(args, default_project) {
  options <- list(project = default_project, output = NULL)
  seen <- character()
  i <- 1L
  while (i <= length(args)) {
    option <- args[[i]]
    if (!option %in% c("--project", "--output") || i == length(args) || startsWith(args[[i + 1L]], "--")) {
      stop("Usage: capture-environment.R [--project PROJECT_DIR] [--output OUTPUT_DIR]", call. = FALSE)
    }
    name <- if (identical(option, "--project")) "project" else "output"
    if (name %in% seen) stop(paste("Repeated option", option), call. = FALSE)
    seen <- c(seen, name)
    options[[name]] <- args[[i + 1L]]
    i <- i + 2L
  }
  if (is.null(options$output)) options$output <- file.path(options$project, "artifacts", "acceptance", "A-T01")
  options$project <- normalizePath(options$project, winslash = "/", mustWork = TRUE)
  options$output <- normalizePath(options$output, winslash = "/", mustWork = FALSE)
  options
}

capture_environment <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- parse_args(args, default_project)
  if (!requireNamespace("renv", quietly = TRUE)) stop("renv must be installed to capture the lock.", call. = FALSE)
  if (!file.exists(file.path(options$project, "DESCRIPTION"))) stop("Project has no DESCRIPTION file.", call. = FALSE)
  dir.create(options$output, recursive = TRUE, showWarnings = FALSE)

  project_library <- file.path(options$project, "renv", "library")
  library_candidates <- if (dir.exists(project_library)) {
    list.dirs(project_library, recursive = FALSE, full.names = TRUE)
  } else character()
  local_library <- library_candidates[startsWith(basename(library_candidates),
                                                 paste0(R.version$platform, "-"))]
  if (length(local_library) == 0L) {
    local_library <- .libPaths()[grepl(normalizePath(project_library, winslash = "/", mustWork = FALSE),
                                       normalizePath(.libPaths(), winslash = "/", mustWork = FALSE), fixed = TRUE)]
  }
  if (length(local_library) == 0L) local_library <- .libPaths()[[1L]]
  installed <- utils::installed.packages(lib.loc = unique(c(local_library, .libPaths())), noCache = TRUE)
  if (nrow(installed) == 0L) stop("No installed R packages were found for the environment lock.", call. = FALSE)

  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    renv::settings$bioconductor.enabled(TRUE, project = options$project)
    renv::settings$bioconductor.version("3.23", project = options$project)
  }
  renv::snapshot(project = options$project, library = unique(c(local_library, .Library)),
                 type = "implicit", prompt = FALSE, update = FALSE,
                 lockfile = file.path(options$project, "renv.lock"), force = TRUE)

  packages <- lapply(seq_len(nrow(installed)), function(i) {
    row <- installed[i, , drop = TRUE]
    fields <- intersect(c("Package", "Version", "LibPath", "Priority", "Repository"), names(row))
    as.list(row[fields])
  })
  names(packages) <- installed[, "Package"]
  bioc_version <- if ("BiocVersion" %in% installed[, "Package"]) {
    as.character(utils::packageVersion("BiocVersion", lib.loc = local_library))
  } else {
    renv::settings$bioconductor.version(project = options$project)
  }
  manifest <- list(
    captured_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    project = basename(options$project),
    R = list(version = as.character(getRversion()), platform = R.version$platform,
             architecture = R.version$arch, library_paths = normalizePath(unique(c(local_library, .libPaths())),
                                                                          winslash = "/", mustWork = FALSE)),
    Bioconductor = list(configured_release = bioc_version,
                        release_package_installed = "BiocVersion" %in% installed[, "Package"],
                        manager_version = if (requireNamespace("BiocManager", quietly = TRUE)) {
                          as.character(utils::packageVersion("BiocManager", lib.loc = local_library))
                        } else NULL,
                        release_validation = if ("BiocVersion" %in% installed[, "Package"]) "installed" else "configured_from_frozen_runtime_contract"),
    packages = packages
  )
  jsonlite::write_json(manifest, file.path(options$output, "installation-manifest.json"),
                       auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
  writeLines(utils::capture.output(utils::sessionInfo()),
             file.path(options$output, "session-manifest.txt"), useBytes = TRUE)
  cat("Captured renv.lock and local installation/session manifests.\n")
  invisible(manifest)
}

tryCatch(capture_environment(), error = function(error) {
  cat("Environment capture failed: ", conditionMessage(error), "\n", sep = "", file = stderr())
  quit(save = "no", status = 1L)
})
quit(save = "no", status = 0L)
