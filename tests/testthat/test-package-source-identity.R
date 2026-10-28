testthat::test_that("installed source identity fails closed on absent and unexpanded stamps", {
  package_root <- tempfile("triomics-installed-identity-")
  dir.create(package_root)
  on.exit(unlink(package_root, recursive = TRUE), add = TRUE)
  writeLines(c("Package: triomics", "Version: 0.1.0"), file.path(package_root, "DESCRIPTION"))

  original_wd <- getwd()
  unrelated_wd <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  setwd(unrelated_wd)
  on.exit(setwd(original_wd), add = TRUE)

  stamp <- file.path(package_root, "triomics-source-revision")
  testthat::expect_identical(
    triomics_source_revision_from_root(package_root, installed = TRUE), "unavailable"
  )
  writeLines("$Format:%H$", stamp, useBytes = TRUE)
  testthat::expect_identical(
    triomics_source_revision_from_root(package_root, installed = TRUE), "unavailable"
  )

  writeLines("not-a-full-source-revision", stamp, useBytes = TRUE)
  testthat::expect_identical(
    triomics_source_revision_from_root(package_root, installed = TRUE), "unavailable"
  )

  actual_revision <- triomics_source_revision()
  testthat::expect_match(actual_revision, "^(?:[0-9a-f]{40}|[0-9a-f]{64})$")
  writeLines(actual_revision, stamp, useBytes = TRUE)
  testthat::expect_identical(
    triomics_source_revision_from_root(package_root, installed = TRUE), actual_revision
  )
})

testthat::test_that("source identity follows the executing namespace despite unrelated global symbols", {
  namespace_root <- normalizePath(
    getNamespaceInfo(asNamespace("triomics"), "path"), winslash = "/", mustWork = TRUE
  )
  actual_revision <- triomics_source_revision()
  testthat::expect_match(actual_revision, "^(?:[0-9a-f]{40}|[0-9a-f]{64})$")
  had_global_main <- exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  if (had_global_main) previous_global_main <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  assign("triomics_main", function(...) "unrelated global entrypoint", envir = .GlobalEnv)
  original_wd <- getwd()
  unrelated_wd <- tempfile("triomics-unrelated-working-directory-")
  dir.create(unrelated_wd)
  setwd(unrelated_wd)
  on.exit({
    setwd(original_wd)
    unlink(unrelated_wd, recursive = TRUE)
    if (had_global_main) {
      assign("triomics_main", previous_global_main, envir = .GlobalEnv)
    } else if (exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
      rm("triomics_main", envir = .GlobalEnv)
    }
  }, add = TRUE)

  context <- triomics_source_identity_context()
  testthat::expect_identical(context$root, namespace_root)
  testthat::expect_identical(context$installed, !triomics_source_root_valid(namespace_root))
  testthat::expect_identical(triomics_source_revision(), actual_revision)
})
