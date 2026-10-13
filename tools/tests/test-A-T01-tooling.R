repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
while (!file.exists(file.path(repo_root, "DESCRIPTION")) ||
       !file.exists(file.path(repo_root, "tools", "check-package.R"))) {
  parent <- dirname(repo_root)
  if (identical(parent, repo_root)) {
    stop("Run this test from the repository or one of its subdirectories.", call. = FALSE)
  }
  repo_root <- parent
}

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

run_case <- function(name, body) {
  tryCatch({
    body()
    cat("PASS: ", name, "\n", sep = "")
  }, error = function(error) {
    stop("FAIL: ", name, ": ", conditionMessage(error), call. = FALSE)
  })
}

status_code <- function(result) {
  status <- attr(result, "status")
  if (is.null(status)) 0L else as.integer(status)
}

new_check_fixture <- function(warn_on_load = FALSE) {
  root <- tempfile("triomics-check-fixture-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c(
    "Package: fixturecheck",
    "Title: Minimal Package Check Fixture",
    "Version: 0.1.0",
    "Authors@R: person(\"Fixture\", \"Author\", email = \"fixture@example.test\", role = c(\"aut\", \"cre\"))",
    "Description: A tiny synthetic package used to exercise the native check wrapper.",
    "License: GPL-3",
    "Encoding: UTF-8"
  ), file.path(root, "DESCRIPTION"))
  writeLines(character(), file.path(root, "NAMESPACE"))
  writeLines("fixture_value <- function() 1L", file.path(root, "R", "fixture.R"))
  if (warn_on_load) {
    writeLines(
      '.onLoad <- function(libname, pkgname) warning("intentional check warning fixture")',
      file.path(root, "R", "zzz.R")
    )
  }
  root
}

run_package_check <- function(root, output) {
  script <- file.path(repo_root, "tools", "check-package.R")
  old_wd <- setwd(dirname(script))
  on.exit(setwd(old_wd), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  suppressWarnings(system2(
    rscript,
    c("--vanilla", shQuote(basename(script)), "--root", shQuote(root), "--output", shQuote(output)),
    stdout = TRUE, stderr = TRUE, timeout = 120
  ))
}

run_case("native package checking derives names and preserves logs", function() {
  root <- new_check_fixture()
  output <- tempfile("triomics-check-clean-")
  on.exit(unlink(c(root, output), recursive = TRUE), add = TRUE)
  result <- run_package_check(root, output)
  build_log <- if (file.exists(file.path(output, "build.log"))) {
    readLines(file.path(output, "build.log"), warn = FALSE)
  } else character()
  check_log <- if (file.exists(file.path(output, "check.log"))) {
    readLines(file.path(output, "check.log"), warn = FALSE)
  } else character()
  assert_true(status_code(result) == 0L,
              paste(c(result, build_log, check_log), collapse = "\n"))
  assert_true(file.exists(file.path(output, "fixturecheck_0.1.0.tar.gz")),
              "The tarball name was not derived from fixture DESCRIPTION.")
  assert_true(file.exists(file.path(output, "build.log")), "build.log was not preserved.")
  assert_true(file.exists(file.path(output, "check.log")), "check.log was not preserved.")
})

run_case("native package checking never archives repository history", function() {
  root <- new_check_fixture()
  output <- tempfile("triomics-check-history-")
  dir.create(file.path(root, ".git", "objects"), recursive = TRUE)
  writeLines("must not ship", file.path(root, ".git", "objects", "sentinel"))
  on.exit(unlink(c(root, output), recursive = TRUE), add = TRUE)
  result <- run_package_check(root, output)
  tarball <- file.path(output, "fixturecheck_0.1.0.tar.gz")
  assert_true(status_code(result) == 0L, paste(result, collapse = "\n"))
  assert_true(file.exists(tarball), "The package-check tarball was not created.")
  members <- utils::untar(tarball, list = TRUE)
  assert_true(!any(grepl("(^|/)\\.git($|/)", members)),
              "The package archive contains repository history.")
})

run_case("native package checking fails on an undisposed package warning", function() {
  root <- new_check_fixture(warn_on_load = TRUE)
  output <- tempfile("triomics-check-warning-")
  on.exit(unlink(c(root, output), recursive = TRUE), add = TRUE)
  result <- run_package_check(root, output)
  assert_true(status_code(result) != 0L, "The package check accepted an undisposed warning.")
  assert_true(file.exists(file.path(output, "build.log")), "build.log was not preserved.")
  check_log_path <- file.path(output, "check.log")
  assert_true(file.exists(check_log_path), "check.log was not preserved.")
  check_log <- paste(readLines(check_log_path, warn = FALSE), collapse = "\n")
  assert_true(grepl("intentional check warning fixture|WARNING", check_log),
              "The check log did not retain the warning evidence.")
})

run_case("environment capture records actual runtime without changing the repository lock", function() {
  project <- tempfile("triomics-environment-project-")
  output <- tempfile("triomics-environment-output-")
  dir.create(file.path(project, "renv"), recursive = TRUE)
  dir.create(file.path(project, "R"), recursive = TRUE)
  on.exit(unlink(c(project, output), recursive = TRUE), add = TRUE)

  for (path in c("DESCRIPTION", "renv.lock", "renv/settings.json")) {
    source <- file.path(repo_root, path)
    target <- file.path(project, path)
    if (grepl("/", path, fixed = TRUE)) dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    assert_true(file.copy(source, target, overwrite = TRUE), paste("Could not copy", path))
  }
  r_files <- list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE)
  assert_true(all(file.copy(r_files, file.path(project, "R"), overwrite = TRUE)),
              "Could not copy the package R sources for the lock check.")

  script <- file.path(repo_root, "tools", "capture-environment.R")
  old_wd <- setwd(dirname(script))
  on.exit(setwd(old_wd), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  result <- suppressWarnings(system2(
    rscript,
    c("--vanilla", shQuote(basename(script)), "--project", shQuote(project), "--output", shQuote(output)),
    stdout = TRUE, stderr = TRUE, timeout = 60,
    env = "RENV_CONFIG_SNAPSHOT_VALIDATE=FALSE"
  ))
  assert_true(status_code(result) == 0L, paste(result, collapse = "\n"))

  lock <- jsonlite::read_json(file.path(project, "renv.lock"), simplifyVector = TRUE)
  assert_true(identical(lock$R$Version, as.character(getRversion())),
              "The captured lock R version does not match the running R.")
  assert_true(identical(lock$Bioconductor$Version, "3.23"),
              "The captured lock Bioconductor version changed.")
  installed <- utils::installed.packages(lib.loc = .libPaths()[1L])
  manifest <- jsonlite::read_json(file.path(output, "installation-manifest.json"), simplifyVector = TRUE)
  assert_true(identical(manifest$packages$BiocManager$Version,
                        unname(installed["BiocManager", "Version"])),
              "The installation manifest does not report the installed BiocManager version.")
  session <- readLines(file.path(output, "session-manifest.txt"), warn = FALSE)
  assert_true(any(grepl(R.version.string, session, fixed = TRUE)),
              "The session manifest does not report the running R version.")
})

run_case("A-T03 shared preparation sources avoid profile-specific code", function() {
  sources <- file.path(repo_root, c("R/study.R", "R/profile_registry.R", "R/preparation.R", "R/state_store.R"))
  source_text <- paste(unlist(lapply(sources, readLines, warn = FALSE)), collapse = "\n")
  assert_true(!grepl("median_shift|\\bpqn\\b|minprob|qrilc|proteomics|metabolomics|lipidomics", source_text),
              "Shared preparation source contains a profile-specific operation.")
  assert_true(!grepl("block\\$name|display_name\\s*==", source_text),
              "Shared preparation source branches on a display name.")
})

run_case("A-T08 profile extensions remain registry-driven", function() {
  source_text <- paste(readLines(file.path(repo_root, "R/export_a.R"), warn = FALSE), collapse = "\n")
  assert_true(!grepl("ion_mobility|glycomics", source_text),
              "A-T08 source hard-codes the fourth or fifth profile name.")
})

cat("Repository-only tooling checks passed: 6 cases.\n")
