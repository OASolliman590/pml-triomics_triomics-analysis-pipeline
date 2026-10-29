.libPaths(c("/tmp/triomics-mofa-lib-m7-006", .libPaths()))
pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-installed-paths.R", local = .GlobalEnv)
path <- "tests/testthat/test-C-06-shared.R"
expressions <- as.list(parse(path))
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.call(expression[[3L]]) && identical(expression[[3L]][[1L]], as.name("function"))) {
    eval(expression, envir = .GlobalEnv)
  }
}
testthat::set_max_fails(Inf)
testthat::test_that("M7-019 Sol retained-root native B/C/D missing-time proof", {
  fixture <- c06_shared_trajectory_missing_time_fixture()
  output_root <- tempfile("m7-019-sol-native-output-", tmpdir = "/private/tmp")
  cat("CANDIDATE_HEAD:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
  cat("FIXTURE_ROOT:", fixture$root, "\n")
  cat("OUTPUT_ROOT:", output_root, "\n")
  cat("EXCLUDED_SOURCE:", fixture$excluded_sample, "\n")

  b_run <- c06_shared_cli_run(output_root, stage = "B", config_path = fixture$config,
                              prepare_b = FALSE)
  cat("B_STATUS:", b_run$status, "\n", paste(b_run$output, collapse = "\n"), "\n")
  testthat::expect_identical(b_run$status, 0L, info = paste(b_run$output, collapse = "\n"))
  if (b_run$status != 0L) return(invisible(NULL))

  b_profile_paths <- list.files(file.path(output_root, "B", "handoffs"),
    pattern = "^stage-handoff-provenance[.]json$", recursive = TRUE, full.names = TRUE)
  testthat::expect_length(b_profile_paths, 1L)
  b_profile <- jsonlite::read_json(b_profile_paths[[1L]], simplifyVector = FALSE)
  b_map_entities <- Filter(function(entity) {
    identical(entity$scientific_role, "analysis_unit_map")
  }, b_profile$identity$entities)
  testthat::expect_length(b_map_entities, 1L)
  cat("B_ANALYSIS_UNIT_MAP_DIGEST:", b_map_entities[[1L]]$digest, "\n")
  cat("B_ANALYSIS_UNIT_MAP_ROW_IDENTITY_SHA256:",
      b_map_entities[[1L]]$row_identity_sha256, "\n")

  c_run <- c06_shared_cli_run(output_root, stage = "C", config_path = fixture$config,
                              prepare_b = FALSE)
  cat("C_STATUS:", c_run$status, "\n", paste(c_run$output, collapse = "\n"), "\n")
  testthat::expect_identical(c_run$status, 0L, info = paste(c_run$output, collapse = "\n"))
  if (c_run$status != 0L) return(invisible(NULL))

  endpoint_path <- file.path(output_root, "C", "score-endpoints", "integration-01.json")
  endpoint_text <- paste(readLines(endpoint_path, warn = FALSE), collapse = "\n")
  testthat::expect_true(grepl('"time"\\s*:\\s*null', endpoint_text, perl = TRUE),
                        info = "The persisted endpoint must contain an explicit null time.")
  endpoint_bundle <- triomics_c08_cli_validate_endpoint_readback(endpoint_path)
  testthat::expect_no_error(triomics_c08_validate_endpoint_bundle(endpoint_bundle))
  endpoint <- endpoint_bundle$endpoints[[1L]]
  unit_map <- endpoint$unit_map
  unit_ids <- vapply(unit_map, `[[`, character(1L), "unit_id")
  excluded <- unit_map[[match(fixture$excluded_sample, unit_ids)]]
  testthat::expect_identical(excluded$eligibility, "excluded")
  testthat::expect_identical(excluded$exclusion_reason, "absent_from_all_analysis_blocks")
  testthat::expect_identical(unlist(excluded$source_sample_ids, use.names = FALSE),
                             fixture$excluded_sample)
  testthat::expect_length(excluded$represented_blocks, 0L)
  testthat::expect_null(excluded$time)
  testthat::expect_false(fixture$excluded_sample %in% unlist(endpoint$training_unit_ids,
                                                              use.names = FALSE))
  testthat::expect_false(any(vapply(endpoint$samples, function(sample) {
    identical(sample$unit_id, fixture$excluded_sample)
  }, logical(1L))))

  d_run <- c06_shared_cli_run(output_root, stage = "D", config_path = fixture$config,
                              prepare_b = FALSE)
  cat("D_STATUS:", d_run$status, "\n", paste(d_run$output, collapse = "\n"), "\n")
  testthat::expect_identical(d_run$status, 0L, info = paste(d_run$output, collapse = "\n"))
  if (d_run$status != 0L) return(invisible(NULL))
  d_evidence <- jsonlite::read_json(file.path(output_root, "D", "integration-evidence.json"),
                                    simplifyVector = FALSE)$integrations[[1L]]
  testthat::expect_false(fixture$excluded_sample %in% unlist(
    d_evidence$eligibility$training_unit_ids, use.names = FALSE
  ))

  pointer_path <- file.path(output_root, "registries", "current-D.json")
  pointer <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  manifest_path <- file.path(output_root, pointer$generation_manifest$path)
  manifest_hash <- triomics_sha256(manifest_path)
  testthat::expect_identical(manifest_hash, pointer$generation_id)
  testthat::expect_identical(manifest_hash, pointer$generation_manifest$digest)
  b_bundle_path <- list.files(file.path(output_root, "B", "handoffs"),
    pattern = "^bundle[.]json$", recursive = TRUE, full.names = TRUE)
  testthat::expect_length(b_bundle_path, 1L)
  b_bundle_hash <- triomics_sha256(b_bundle_path[[1L]])
  d_profile_path <- file.path(output_root, "D", "stage-handoff-provenance.json")
  d_profile <- jsonlite::read_json(d_profile_path, simplifyVector = FALSE)
  predecessor_hashes <- vapply(d_profile$identity$entities, function(entity) {
    as.character(entity$digest %||% "")
  }, character(1L))
  testthat::expect_true(b_bundle_hash %in% predecessor_hashes)

  sha256 <- function(file) digest::digest(file, algo = "sha256", serialize = FALSE, file = TRUE)
  entries <- character()
  roots <- c(fixture = fixture$root, output = output_root)
  for (root_label in names(roots)) {
    root <- unname(roots[[root_label]])
    entries <- c(entries, paste("ROOT", root_label, root))
    for (file in sort(list.files(root, recursive = TRUE, all.files = TRUE,
                                 full.names = TRUE, no.. = TRUE))) {
      if (!file.info(file)$isdir) {
        entries <- c(entries, paste("FILE_SHA256", root_label,
                                   substring(file, nchar(root) + 2L), sha256(file)))
      }
    }
  }
  manifest_output <- "/tmp/triomics-m7-019/native-sha256-manifest.txt"
  writeLines(entries, manifest_output, useBytes = TRUE)
  evidence <- list(
    candidate_head = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    fixture_root = fixture$root,
    output_root = output_root,
    excluded_source = fixture$excluded_sample,
    b_analysis_unit_map_digest = b_map_entities[[1L]]$digest,
    b_analysis_unit_map_row_identity_sha256 = b_map_entities[[1L]]$row_identity_sha256,
    b_bundle_sha256 = b_bundle_hash,
    endpoint_bundle_sha256 = endpoint_bundle$payload_sha256,
    endpoint_sha256 = endpoint$endpoint_sha256,
    endpoint_file_sha256 = triomics_sha256(endpoint_path),
    d_pointer_sha256 = triomics_sha256(pointer_path),
    d_generation_id = pointer$generation_id,
    d_manifest_sha256 = manifest_hash,
    d_provenance_sha256 = triomics_sha256(d_profile_path),
    manifest_file = manifest_output,
    manifest_sha256 = sha256(manifest_output),
    files_hashed = sum(grepl("^FILE_SHA256 ", entries))
  )
  jsonlite::write_json(evidence, "/tmp/triomics-m7-019/native-evidence.json",
                       auto_unbox = TRUE, pretty = TRUE)
  cat("NATIVE_EVIDENCE:",
      jsonlite::toJSON(evidence, auto_unbox = TRUE), "\n")
  cat("NATIVE_COMPLETE\n")
})
