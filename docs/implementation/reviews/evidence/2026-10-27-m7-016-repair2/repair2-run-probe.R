repo <- "/Users/megastore/.codex/worktrees/m7-016-d-publication/PML MultiOmics Analysis Pipeline"
test_path <- file.path(repo, "tests/testthat/test-D-T01.R")
parsed <- parse(test_path)
test_call <- Filter(function(expr) {
  is.call(expr) && is.call(expr[[1L]]) &&
    identical(paste(deparse(expr[[1L]]), collapse = ""), "testthat::test_that") &&
    grepl("publishes a complete generation", as.character(expr[[2L]]), fixed = TRUE)
}, as.list(parsed))[[1L]]
body <- as.list(test_call[[3L]])
write_call <- Filter(function(expr) {
  is.call(expr) && identical(as.character(expr[[1L]])[1L], "writeLines") &&
    length(expr) >= 3L && is.name(expr[[3L]]) && identical(as.character(expr[[3L]]), "recovery_probe_script")
}, body)[[1L]]
if (is.null(write_call)) stop("Could not extract recovery probe writer from D-T01 test.")

source_config <- normalizePath(file.path(repo, "inst/examples/tickets/D-T01.yaml"),
                               winslash = "/", mustWork = TRUE)
retained_root <- "/private/tmp/triomics-m7-016-luna/native-debug"
install_library <- "/private/tmp/triomics-m7-016-luna/repair2-installed-lib"
installed_package <- file.path(install_library, "triomics")
stopifnot(dir.exists(retained_root), dir.exists(installed_package))

run_probe <- function(kind) {
  root <- tempfile(paste0("repair2-", kind, "-root-"), tmpdir = "/private/tmp")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  contents <- list.files(retained_root, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  copied <- file.copy(contents, root, recursive = TRUE)
  stopifnot(all(copied))
  script <- tempfile(paste0("repair2-", kind, "-probe-"), tmpdir = "/private/tmp")
  eval_env <- new.env(parent = globalenv())
  eval_env$recovery_probe_script <- script
  eval(write_call, envir = eval_env)
  package_root <- if (identical(kind, "source")) repo else installed_package
  config_path <- if (identical(kind, "source")) source_config else {
    system.file("examples", "tickets", "D-T01.yaml", package = "triomics",
                lib.loc = install_library)
  }
  stopifnot(nzchar(config_path), file.exists(config_path))
  Sys.setenv(R_LIBS = install_library)
  output <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(script), shQuote(config_path),
                      shQuote(root), shQuote(package_root)),
                    stdout = TRUE, stderr = TRUE)
  exit_code <- attr(output, "status")
  if (is.null(exit_code)) exit_code <- 0L
  pointer <- file.path(root, "registries", "current-D.json")
  pointer_sha256 <- digest::digest(file = pointer, algo = "sha256", serialize = FALSE)
  expected <- c(
    paste0("probe_bootstrap=", kind),
    "ambiguous_readback=D_PUBLICATION_STATUS_UNKNOWN",
    "planned_generation_leaf=D_OUTPUT_PATH_UNSAFE",
    "temporary_generation_ancestry=D_OUTPUT_PATH_UNSAFE",
    "temporary_pointer_ancestry=D_OUTPUT_PATH_UNSAFE",
    "failed_rename_class=D_PUBLICATION_FAILED",
    "old_current_preserved=TRUE"
  )
  log <- c(paste0("mode=", kind), paste0("config=", config_path),
           paste0("package_root=", package_root), paste0("output_root=", root),
           paste0("exit_code=", exit_code), paste0("pointer_sha256=", pointer_sha256),
           output)
  log_path <- file.path("/tmp/triomics-m7-016-luna", paste0("repair2-", kind, "-probe.log"))
  writeLines(log, log_path)
  if (exit_code != 0L || !all(vapply(expected, function(value) any(grepl(value, output, fixed = TRUE)), logical(1)))) {
    stop(paste("Probe failed; see", log_path, "\n", paste(output, collapse = "\n")))
  }
  cat(paste(log, collapse = "\n"), "\n", sep = "")
}
run_probe("source")
run_probe("installed")
