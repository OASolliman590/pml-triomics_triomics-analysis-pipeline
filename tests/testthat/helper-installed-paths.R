triomics_test_resource <- function(...) {
  components <- as.character(c(...))
  if (length(components) == 0L || anyNA(components) || any(!nzchar(components))) {
    stop("Package resource paths require non-empty components.", call. = FALSE)
  }

  relative_path <- do.call(file.path, as.list(components))
  source_path <- testthat::test_path("..", "..", "inst", relative_path)
  if (file.exists(source_path)) {
    return(normalizePath(source_path, winslash = "/", mustWork = TRUE))
  }

  installed_path <- do.call(
    system.file,
    c(as.list(components), list(package = "triomics"))
  )
  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(normalizePath(installed_path, winslash = "/", mustWork = TRUE))
  }

  stop(
    "Package resource not found: ", paste(components, collapse = "/"),
    call. = FALSE
  )
}
