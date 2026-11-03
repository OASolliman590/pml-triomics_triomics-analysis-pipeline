d_status_cli_run <- function(config_path, stage, output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  stdout_path <- tempfile("d-status-command-stdout-", tmpdir = temp_root)
  stderr_path <- tempfile("d-status-command-stderr-", tmpdir = temp_root)
  on.exit(unlink(c(stdout_path, stderr_path), force = TRUE), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", shQuote(stage), "--output", shQuote(output_root)),
    stdout = stdout_path, stderr = stderr_path
  )
  output <- c(readLines(stdout_path, warn = FALSE), readLines(stderr_path, warn = FALSE))
  list(status = as.integer(status), output = output)
}

d_status_temp_root <- function() normalizePath(tempdir(), winslash = "/", mustWork = TRUE)

d_status_native_fixture <- function(all_disabled = FALSE) {
  ticket <- if (isTRUE(all_disabled)) "D-T01.yaml" else "D-T05.yaml"
  source_config <- triomics_test_resource("examples", "tickets", ticket)
  fixture_root <- tempfile("d-status-native-fixture-", tmpdir = d_status_temp_root())
  dir.create(file.path(fixture_root, "tickets"), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source_config, file.path(fixture_root, "tickets"))) {
    stop("Could not copy the public synthetic config.", call. = FALSE)
  }
  source_data <- normalizePath(file.path(dirname(source_config), "..", "data"),
                               winslash = "/", mustWork = TRUE)
  if (!file.copy(source_data, fixture_root, recursive = TRUE)) {
    stop("Could not copy the public synthetic data fixtures.", call. = FALSE)
  }
  config_path <- file.path(fixture_root, "tickets", basename(source_config))
  list(root = fixture_root, config = config_path)
}

d_status_predecessor_hashes <- function(output_root) {
  paths <- unlist(lapply(c("B", "C"), function(stage) {
    list.files(file.path(output_root, stage), recursive = TRUE, full.names = TRUE,
               all.files = TRUE)
  }), use.names = FALSE)
  paths <- paths[file.exists(paths) & !file.info(paths)$isdir]
  names <- substring(paths, nchar(output_root) + 2L)
  stats::setNames(vapply(paths, triomics_sha256, character(1)), names)
}

d_status_pointer_digests <- function(output_root) {
  pointer_path <- file.path(output_root, "registries", "current-D.json")
  pointer <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  payload_path <- file.path(output_root, "D", "generations", pointer$generation_id,
                            "evidence-routing.json")
  list(pointer = triomics_sha256(pointer_path), payload = triomics_sha256(payload_path),
       generation_id = pointer$generation_id)
}

d_status_published <- function(output_root) {
  pointer_path <- file.path(output_root, "registries", "current-D.json")
  pointer <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  generation <- file.path(output_root, "D", "generations", pointer$generation_id)
  list(
    pointer = pointer,
    generation = generation,
    manifest = jsonlite::read_json(file.path(generation, "generation-manifest.json"),
                                  simplifyVector = FALSE),
    routing = jsonlite::read_json(file.path(generation, "evidence-routing.json"),
                                 simplifyVector = FALSE)
  )
}

testthat::test_that("all-disabled native D publishes complete resource status with B/C lineage", {
  fixture <- d_status_native_fixture(all_disabled = TRUE)
  output_root <- tempfile("d-status-disabled-output-", tmpdir = d_status_temp_root())
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(fixture$root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  b <- d_status_cli_run(fixture$config, "B", output_root)
  testthat::expect_identical(b$status, 0L, info = paste(b$output, collapse = "\n"))
  if (b$status != 0L) return(invisible(NULL))
  c <- d_status_cli_run(fixture$config, "C", output_root)
  testthat::expect_identical(c$status, 0L, info = paste(c$output, collapse = "\n"))
  if (c$status != 0L) return(invisible(NULL))
  d <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(d$status, 0L, info = paste(d$output, collapse = "\n"))
  if (d$status != 0L) return(invisible(NULL))

  published <- d_status_published(output_root)
  route_status <- published$routing$route_status
  testthat::expect_identical(published$manifest$status, "complete")
  testthat::expect_identical(route_status$run_completeness, "complete")
  testthat::expect_identical(route_status$exit_code, 0L)
  testthat::expect_false(route_status$biological_absence)
  testthat::expect_length(route_status$unavailable_routes, 0L)
  testthat::expect_identical(route_status$predecessor$b_sha256,
                             published$manifest$provenance$b$bundle$payload_sha256)
  testthat::expect_match(route_status$predecessor$c_sha256, "^[0-9a-f]{64}$")
})

testthat::test_that("native optional resource failures retain reasons while required failures serialize incomplete status", {
  fixture <- d_status_native_fixture()
  output_root <- tempfile("d-status-resources-output-", tmpdir = d_status_temp_root())
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(fixture$root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  b <- d_status_cli_run(fixture$config, "B", output_root)
  testthat::expect_identical(b$status, 0L, info = paste(b$output, collapse = "\n"))
  if (b$status != 0L) return(invisible(NULL))
  c <- d_status_cli_run(fixture$config, "C", output_root)
  testthat::expect_identical(c$status, 0L, info = paste(c$output, collapse = "\n"))
  if (c$status != 0L) return(invisible(NULL))
  predecessor_hashes <- d_status_predecessor_hashes(output_root)
  gene_source <- file.path(fixture$root, "data", "d-status", "gene-map.tsv")
  gene_backup <- file.path(fixture$root, "data", "d-status", "gene-map-verified-copy.tsv")
  testthat::expect_true(file.copy(gene_source, gene_backup))

  d <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(d$status, 0L, info = paste(d$output, collapse = "\n"))
  if (d$status != 0L) return(invisible(NULL))
  initial <- d_status_published(output_root)
  initial_status <- initial$routing$route_status
  gene_mapping <- initial$routing$routes$gene_reaction$mappings$`gene-map`
  testthat::expect_identical(initial$manifest$status, "complete")
  testthat::expect_identical(initial_status$run_completeness, "complete")
  testthat::expect_identical(initial_status$exit_code, 0L)
  testthat::expect_identical(initial_status$unavailable_routes$ora$reason_code, "D_RESOURCE_RIGHTS")
  testthat::expect_identical(initial_status$predecessor$b_sha256,
                             initial$manifest$provenance$b$bundle$payload_sha256)
  testthat::expect_match(initial_status$predecessor$c_sha256, "^[0-9a-f]{64}$")
  testthat::expect_identical(initial$routing$routes$gene_reaction$status, "complete")
  testthat::expect_identical(initial$routing$routes$lipid_ontology$status, "complete")
  testthat::expect_setequal(names(initial$routing$routes), triomics_d_route_names())
  testthat::expect_identical(initial$routing$routes$ora$status, "unavailable")
  testthat::expect_identical(initial$routing$routes$mofa_native$status, "not_requested")
  testthat::expect_gt(length(gene_mapping$observations), 0L)
  testthat::expect_gt(length(gene_mapping$edges), 0L)
  testthat::expect_false(initial_status$biological_absence)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  config_lines <- readLines(fixture$config, warn = FALSE)
  gene_resource_line <- match("    - resource_id: gene-map", config_lines)
  testthat::expect_true(!is.na(gene_resource_line))
  rights_lines <- which(seq_along(config_lines) > gene_resource_line &
                          grepl("^      rights_status: verified$", config_lines))
  testthat::expect_gt(length(rights_lines), 0L)
  changed_rights_config <- file.path(dirname(fixture$config), "D-T05-rights-revoked.yaml")
  config_lines[[rights_lines[[1L]]]] <- "      rights_status: unverified"
  writeLines(config_lines, changed_rights_config, useBytes = TRUE)
  pointer_before_rights_change <- d_status_pointer_digests(output_root)
  changed_rights <- d_status_cli_run(changed_rights_config, "D", output_root)
  testthat::expect_false(identical(changed_rights$status, 0L),
                         info = paste(changed_rights$output, collapse = "\n"))
  testthat::expect_true(any(grepl("D_LINEAGE_MISMATCH", changed_rights$output, fixed = TRUE)),
                        info = paste(changed_rights$output, collapse = "\n"))
  testthat::expect_identical(d_status_pointer_digests(output_root), pointer_before_rights_change)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  config <- yaml::read_yaml(fixture$config)
  lipid_resource <- config$annotation$resources[[2L]]
  lipid_cache <- initial$routing$routes$lipid_ontology$mappings$`lipid-map`$resource$path
  lipid_source <- file.path(fixture$root, "data", "d-status", "lipid-map.tsv")
  testthat::expect_true(file.exists(lipid_cache))
  writeLines("tampered cached resource", lipid_cache)
  stale_cache <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(stale_cache$status, 0L, info = paste(stale_cache$output, collapse = "\n"))
  stale <- d_status_published(output_root)
  testthat::expect_identical(stale$manifest$status, "complete")
  testthat::expect_identical(stale$routing$route_status$run_completeness, "complete")
  testthat::expect_identical(stale$routing$route_status$unavailable_routes$lipid_ontology$reason_code,
                             "D_RESOURCE_HASH_MISMATCH")
  testthat::expect_identical(stale$routing$routes$gene_reaction$mappings$`gene-map`$observations,
                             gene_mapping$observations)
  testthat::expect_identical(stale$routing$routes$gene_reaction$mappings$`gene-map`$edges,
                             gene_mapping$edges)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  testthat::expect_true(unlink(lipid_cache) == 0L)
  testthat::expect_true(dir.create(lipid_cache))
  pointer_before_cache_directory <- d_status_pointer_digests(output_root)
  cache_directory <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_false(identical(cache_directory$status, 0L),
                         info = paste(cache_directory$output, collapse = "\n"))
  testthat::expect_true(any(grepl("D_RESOURCE_CACHE_INVALID", cache_directory$output, fixed = TRUE)),
                        info = paste(cache_directory$output, collapse = "\n"))
  testthat::expect_identical(d_status_pointer_digests(output_root), pointer_before_cache_directory)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  testthat::expect_true(unlink(lipid_cache, recursive = TRUE) == 0L)
  testthat::expect_true(unlink(lipid_source) == 0L)
  missing_optional <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(missing_optional$status, 0L,
                             info = paste(missing_optional$output, collapse = "\n"))
  missing <- d_status_published(output_root)
  testthat::expect_identical(missing$routing$route_status$run_completeness, "complete")
  testthat::expect_identical(missing$routing$route_status$unavailable_routes$lipid_ontology$reason_code,
                             "D_RESOURCE_MISSING")
  testthat::expect_identical(missing$routing$routes$gene_reaction$mappings$`gene-map`$observations,
                             gene_mapping$observations)
  testthat::expect_identical(missing$routing$routes$gene_reaction$mappings$`gene-map`$edges,
                             gene_mapping$edges)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  writeLines("wrong local resource bytes", lipid_source)
  bad_optional <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(bad_optional$status, 0L, info = paste(bad_optional$output, collapse = "\n"))
  bad_lipid <- d_status_published(output_root)
  testthat::expect_identical(bad_lipid$routing$route_status$run_completeness, "complete")
  testthat::expect_identical(bad_lipid$routing$route_status$unavailable_routes$lipid_ontology$reason_code,
                             "D_RESOURCE_HASH_MISMATCH")
  testthat::expect_identical(bad_lipid$routing$routes$gene_reaction$mappings$`gene-map`$observations,
                             gene_mapping$observations)
  testthat::expect_identical(bad_lipid$routing$routes$gene_reaction$mappings$`gene-map`$edges,
                             gene_mapping$edges)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  gene_cache <- initial$routing$routes$gene_reaction$mappings$`gene-map`$resource$path
  testthat::expect_true(file.exists(gene_cache))
  testthat::expect_true(unlink(gene_cache) == 0L)
  writeLines("wrong required mapping bytes", gene_source)
  bad_required <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(bad_required$status, 2L,
                             info = paste(bad_required$output, collapse = "\n"))
  incomplete_hash <- d_status_published(output_root)
  testthat::expect_identical(incomplete_hash$manifest$status, "complete")
  testthat::expect_identical(incomplete_hash$routing$route_status$run_completeness, "incomplete")
  testthat::expect_identical(incomplete_hash$routing$route_status$exit_code, 2L)
  testthat::expect_identical(incomplete_hash$routing$route_status$unavailable_routes$gene_reaction$reason_code,
                             "D_RESOURCE_HASH_MISMATCH")
  testthat::expect_false(incomplete_hash$routing$route_status$biological_absence)
  testthat::expect_identical(incomplete_hash$routing$route_status$predecessor,
                             initial_status$predecessor)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  testthat::expect_true(unlink(gene_source) == 0L)
  missing_required <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(missing_required$status, 2L,
                             info = paste(missing_required$output, collapse = "\n"))
  incomplete_missing <- d_status_published(output_root)
  testthat::expect_identical(incomplete_missing$routing$route_status$run_completeness, "incomplete")
  testthat::expect_identical(incomplete_missing$routing$route_status$exit_code, 2L)
  testthat::expect_identical(incomplete_missing$routing$route_status$unavailable_routes$gene_reaction$reason_code,
                             "D_RESOURCE_MISSING")
  testthat::expect_identical(incomplete_missing$routing$route_status$predecessor,
                             initial_status$predecessor)
  testthat::expect_identical(lipid_resource$rights_status, "verified")
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)

  testthat::expect_true(file.symlink(gene_backup, gene_source))
  pointer_before_unsafe_source <- d_status_pointer_digests(output_root)
  unsafe_source <- d_status_cli_run(fixture$config, "D", output_root)
  testthat::expect_false(identical(unsafe_source$status, 0L),
                         info = paste(unsafe_source$output, collapse = "\n"))
  testthat::expect_true(any(grepl("D_RESOURCE_HASH_MISMATCH", unsafe_source$output, fixed = TRUE)),
                        info = paste(unsafe_source$output, collapse = "\n"))
  testthat::expect_identical(d_status_pointer_digests(output_root), pointer_before_unsafe_source)
  testthat::expect_identical(d_status_predecessor_hashes(output_root), predecessor_hashes)
})
