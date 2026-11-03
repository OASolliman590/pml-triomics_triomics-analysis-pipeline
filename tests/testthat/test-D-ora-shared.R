d_ora_cli_run <- function(config_path, stage, output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  stdout_path <- tempfile("d-ora-command-stdout-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
  stderr_path <- tempfile("d-ora-command-stderr-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
  on.exit(unlink(c(stdout_path, stderr_path), force = TRUE), add = TRUE)
  exit_code <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", shQuote(stage), "--output", shQuote(output_root)),
    stdout = stdout_path, stderr = stderr_path
  )
  output <- c(readLines(stdout_path, warn = FALSE), readLines(stderr_path, warn = FALSE))
  list(exit_code = as.integer(exit_code), output = output)
}

d_ora_copy_fixture <- function() {
  root <- tempfile("d-ora-fixture-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
  ticket_dir <- file.path(root, "tickets")
  data_root <- file.path(root, "data")
  dir.create(ticket_dir, recursive = TRUE, showWarnings = FALSE)
  for (directory in c("c04", "independent", "d-ora")) {
    dir.create(file.path(data_root, directory), recursive = TRUE, showWarnings = FALSE)
  }
  source_config <- triomics_test_resource("examples", "tickets", "D-T04.yaml")
  config <- file.path(ticket_dir, "D-T04.yaml")
  testthat::expect_true(file.copy(source_config, config, overwrite = TRUE))
  files <- list(
    c("c04", "metadata.tsv"),
    c("independent", "proteomics.tsv"),
    c("independent", "metabolomics.tsv"),
    c("d-ora", "gene-feature-metadata.tsv"),
    c("d-ora", "gene-mapping.tsv"),
    c("d-ora", "ora-protocol.json")
  )
  for (parts in files) {
    source <- do.call(triomics_test_resource, as.list(c("examples", "data", parts)))
    destination <- do.call(file.path, as.list(c(data_root, parts)))
    testthat::expect_true(file.copy(source, destination, overwrite = TRUE))
  }
  list(root = root, config = config, protocol = file.path(data_root, "d-ora", "ora-protocol.json"),
       mapping = file.path(data_root, "d-ora", "gene-mapping.tsv"))
}

d_ora_read_native_bundle <- function(config_path, output_root) {
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  analysis <- config$analyses[[1L]]
  current_metadata <- list(
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels
  )
  triomics_d_read_evidence_bundle(
    output_root, as.character(config$study_id), as.character(analysis$analysis_id),
    ingested$source_manifests, ingested$feature_registry,
    imported_statistics = list(),
    routing_metadata = triomics_d_routing_metadata(config, config_path, ingested$feature_registry),
    current_config_sha256 = as.character(attr(config, "config_sha256")),
    current_metadata = current_metadata,
    current_count_metadata = analysis$statistics$count_metadata %||% NULL
  )
}

d_ora_hash <- function(value) digest::digest(value, algo = "sha256", serialize = FALSE)

d_ora_code <- function(expr) tryCatch({ force(expr); NULL }, error = function(error) error$code)

d_ora_test_bundle <- function(branch = "primary_statistics") {
  keys <- c("feature_a", "feature_b", "feature_c")
  registry <- stats::setNames(lapply(seq_along(keys), function(index) {
    list(
      feature_key = keys[[index]], block_id = "block_1", original_id = paste0("ID-", index),
      id_namespace = "synthetic_gene", source_id = "source_1", source_row = as.character(index),
      measured_or_derived = "measured", structural_resolution = "gene", taxon_id = "9606",
      taxon_status = "declared", capability_status = "declared", resolution_status = "declared",
      capabilities = list("gene_reaction")
    )
  }), keys)
  rows <- lapply(seq_along(keys), function(index) {
    list(result_id = paste0("result_", index), feature_key = keys[[index]],
         estimation_status = "estimated", p = 0.01, q = 0.02, effect = 1.5,
         effect_unit = "beta")
  })
  family <- list(family_id = "family_1", member_ids = as.list(keys), n_declared = 3L,
                 member_hash = d_ora_hash(paste(keys, collapse = "|")))
  nomination <- if (identical(branch, "primary_statistics")) {
    list(
      nomination_id = d_ora_hash("primary-feature-b"), feature_key = "feature_b",
      source_branch = "primary_statistics",
      result_ref = list(analysis_id = "analysis_1", block_id = "block_1",
                        claim_id = "claim_1", family_id = "family_1"),
      selection_rule = "valid_q_at_or_below_threshold_and_absolute_effect_at_or_above_reporting_threshold",
      selection_value = list(p = 0.01, q = 0.02, effect = 1.5, effect_unit = "beta"),
      q_threshold = 0.05, effect_threshold = 1
    )
  } else {
    list(
      nomination_id = d_ora_hash("integration-feature-b"), feature_key = "feature_b",
      source_branch = "integration_selection",
      result_ref = list(integration_id = "integration_1", backend = "concat_pca",
                        block_id = "block_1", component_ids = list("PC1")),
      selection_rule = "largest_absolute_component_loading",
      selection_value = list(block_id = "block_1", component_loadings = list(PC1 = 0.8)),
      q_threshold = NULL, effect_threshold = NULL
    )
  }
  integration <- list(
    integration_id = "integration_1", backend = "concat_pca", status = "complete",
    eligibility = list(feature_map = list(block_1 = as.list(keys))),
    common = list(scores = list(columns = list("PC1"))),
    selection = list(
      rule = "largest_absolute_component_loading", selected = list(block_1 = list("feature_b")),
      selected_values = list(block_1 = list(feature_b = list(component_loadings = list(PC1 = 0.8))))
    )
  )
  source <- list(source_id = "source_1", study_id = "study_1", role = "measurement_matrix",
                 sha256 = d_ora_hash("source"), selection_history = "full_unselected",
                 selection_evidence = "Synthetic complete feature inventory.")
  list(
    study_id = "study_1", analysis_id = "analysis_1",
    preparation_context_id = "prep_1", representation_id = "levels",
    feature_registry = registry, source_by_id = list(source_1 = source),
    b = list(
      block_results = list(blocks = list(block_1 = list(contrasts = list(
        claim_1 = list(family = family, rows = rows)
      )))),
      family_ledger = list(families = list(list(block_id = "block_1", claim_id = "claim_1",
                                                family = family))),
      candidate_nominations = list(nominations = if (identical(branch, "primary_statistics")) list(nomination) else list())
    ),
    c = list(
      candidate_nominations = list(branches = list(
        primary_statistics = list(nominations = if (identical(branch, "primary_statistics")) list(nomination) else list()),
        integration_selection = list(nominations = if (identical(branch, "integration_selection")) list(nomination) else list())
      )),
      integration_evidence = list(integrations = list(integration))
    )
  )
}

d_ora_mapper_fixture <- function() {
  directory <- tempfile("d-ora-mapper-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, "mapping.tsv")
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  table <- data.frame(
    source_namespace = rep("synthetic_gene", 5L),
    source_id = c("SRC-A", "SRC-B", "SRC-C", "SRC-C", "SRC-E"),
    taxon_id = rep("9606", 5L), target_namespace = rep("Reactome", 5L),
    target_id = c("ORA-1", "ORA-1", "ORA-3", "ORA-4", "ORA-2"),
    relation = rep("maps_to", 5L), resolution = rep("gene", 5L),
    evidence_ref = paste0("synthetic://mapping/", seq_len(5L)),
    resource_id = rep("d-ora-unit-map", 5L), stringsAsFactors = FALSE
  )
  utils::write.table(table, path, sep = "\t", quote = FALSE, row.names = FALSE)
  rows <- lapply(seq_along(c("SRC-A", "SRC-B", "SRC-C", "SRC-D", "SRC-E")), function(index) {
    list(feature_key = paste0("feature_", letters[[index]]), original_id = paste0("ID-", index),
         source_id = c("SRC-A", "SRC-B", "SRC-C", "SRC-D", "SRC-E")[[index]],
         source_namespace = "synthetic_gene", taxon_id = "9606", resolution = "gene",
         nominations = list())
  })
  resource <- list(resource_id = "d-ora-unit-map", source_url = "local://d-ora/mapping.tsv",
                   version = "synthetic-1", sha256 = triomics_sha256(path), local_file = path,
                   terms_url = "https://www.gnu.org/licenses/gpl-3.0.html", rights_status = "verified",
                   permitted_operation = list("gene_reaction"), redistribution = "allowed")
  mapping <- triomics_map_reactions(rows, resource, "gene_reaction", resource$version,
                                    resource$sha256, file.path(directory, "cache"))
  list(rows = rows, mapping = mapping, directory = directory)
}

testthat::test_that("D-T04 executes declared-universe ORA after native B and C", {
  fixture <- d_ora_copy_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  output_root <- tempfile("d-ora-native-output-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_root, recursive = TRUE, force = TRUE), add = TRUE)

  b_run <- d_ora_cli_run(fixture$config, "B", output_root)
  testthat::expect_identical(b_run$exit_code, 0L, info = paste(b_run$output, collapse = "\n"))
  if (b_run$exit_code != 0L) return(invisible(NULL))
  c_run <- d_ora_cli_run(fixture$config, "C", output_root)
  testthat::expect_identical(c_run$exit_code, 0L, info = paste(c_run$output, collapse = "\n"))
  if (c_run$exit_code != 0L) return(invisible(NULL))
  native_bundle <- d_ora_read_native_bundle(fixture$config, output_root)
  native_b_claim <- triomics_ora_resolve_claim(
    native_bundle,
    list(analysis_id = "d_t04_synthetic_exploratory", block_id = "block_1",
         source_branch = "primary_statistics", claim_id = "score_slope")
  )
  testthat::expect_length(native_b_claim$feature_keys, 10L)
  testthat::expect_setequal(native_b_claim$feature_keys,
                            vapply(native_b_claim$producer_rows, `[[`, character(1), "feature_key"))
  native_b_nominations <- native_bundle$c$candidate_nominations$branches$primary_statistics$nominations
  testthat::expect_true(any(vapply(native_b_nominations, function(nomination) {
    identical(as.character(nomination$result_ref$block_id), "block_1") &&
      identical(as.character(nomination$result_ref$claim_id), "score_slope")
  }, logical(1))))
  d_run <- d_ora_cli_run(fixture$config, "D", output_root)

  testthat::expect_identical(d_run$exit_code, 0L, info = paste(d_run$output, collapse = "\n"))
  if (d_run$exit_code != 0L) return(invisible(NULL))

  pointer_path <- file.path(output_root, "registries", "current-D.json")
  pointer <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  payload_path <- file.path(output_root, "D", "generations", pointer$generation_id, "evidence-routing.json")
  payload <- jsonlite::read_json(payload_path, simplifyVector = FALSE)
  claim <- payload$routes$ora$claims[[1L]]
  family <- claim$set_family$result
  first_set <- family$sets[[1L]]
  testthat::expect_identical(payload$routes$ora$status, "complete")
  testthat::expect_identical(claim$identity$source_branch, "integration_selection")
  testthat::expect_identical(claim$identity$claim_id, "d_t04_concat_pca")
  testthat::expect_identical(claim$inventory$status, "full_unselected")
  testthat::expect_equal(unlist(first_set[c("N", "K", "n", "k")]), c(N = 10, K = 3, n = 1, k = 1))
  testthat::expect_equal(first_set$p, 0.3)
  testthat::expect_equal(first_set$q, 0.3)
  testthat::expect_equal(family$full_family_size, 3L)
  testthat::expect_equal(family$estimable_count, 1L)
  testthat::expect_equal(family$empty_set_count, 1L)
  testthat::expect_equal(family$ineligible_set_count, 1L)
  testthat::expect_true(length(claim$measured_universe$included) > 0L)
  testthat::expect_true(is.list(claim$measured_universe$excluded))
  for (binding in unlist(claim$bindings, use.names = FALSE)) {
    testthat::expect_match(binding, "^[0-9a-f]{64}$")
  }
  testthat::expect_identical(claim$bindings$protocol_sha256, triomics_sha256(fixture$protocol))
  testthat::expect_identical(claim$bindings$mapping_sha256, triomics_sha256(fixture$mapping))
  universe <- claim$measured_universe
  universe_sha <- universe$sha256
  universe$sha256 <- NULL
  testthat::expect_identical(triomics_hash_text(triomics_canonical_json(universe)), universe_sha)
  testthat::expect_identical(payload$interpretation, "no_biological_absence_inferred")
  testthat::expect_match(payload$immutable_inputs$b_bundle_sha256, "^[0-9a-f]{64}$")
  testthat::expect_match(payload$immutable_inputs$c_manifest_sha256, "^[0-9a-f]{64}$")

  generation_id <- pointer$generation_id
  payload_sha256 <- triomics_sha256(payload_path)
  write(" ", file = fixture$protocol, append = TRUE)
  failed_d <- d_ora_cli_run(fixture$config, "D", output_root)
  testthat::expect_identical(failed_d$exit_code, 2L)
  current <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  testthat::expect_identical(current$generation_id, generation_id)
  testthat::expect_identical(triomics_sha256(payload_path), payload_sha256)
})

testthat::test_that("ORA counts unique resolved entities and preserves ambiguous and unmapped source rows", {
  fixture <- d_ora_mapper_fixture()
  resolved <- triomics_ora_unique_entities(
    fixture$rows, fixture$mapping, "Reactome", "9606", c("feature_b", "feature_c")
  )
  testthat::expect_setequal(resolved$universe$ids, c("ORA-1", "ORA-2"))
  testthat::expect_setequal(resolved$selected_ids, "ORA-1")
  testthat::expect_setequal(vapply(resolved$included, `[[`, character(1), "feature_key"),
                            c("feature_a", "feature_b", "feature_e"))
  excluded <- stats::setNames(resolved$excluded, vapply(resolved$excluded, `[[`, character(1), "feature_key"))
  testthat::expect_identical(excluded$feature_c$reason, "ambiguous_target_entity")
  testthat::expect_identical(excluded$feature_d$reason, "unmapped")

  family <- triomics_ora_family(
    resolved$universe,
    list(list(set_id = "set_one", members = "ORA-1", eligible = TRUE)),
    resolved$selected_ids, list(status = "full_unselected"),
    list(rule = "unique_resolved_entity", status = "compatible")
  )
  testthat::expect_equal(unlist(family$sets[[1L]][c("N", "K", "n", "k")]),
                         c(N = 2, K = 1, n = 1, k = 1))
  testthat::expect_equal(family$sets[[1L]]$p, 0.5)
})

testthat::test_that("ORA inventory qualification comes from the measured source manifests", {
  sources <- list(
    full = list(source_id = "full", selection_history = "full_unselected"),
    panel = list(source_id = "panel", selection_history = "prespecified_panel"),
    unknown = list(source_id = "unknown", selection_history = "unknown"),
    selected = list(source_id = "selected", selection_history = "outcome_selected")
  )
  testthat::expect_identical(triomics_ora_inventory_status("full", sources)$status, "full_unselected")
  testthat::expect_identical(triomics_ora_inventory_status(c("full", "panel"), sources)$status,
                             "prespecified_panel")
  testthat::expect_identical(triomics_ora_inventory_status("unknown", sources)$status, "unknown")
  testthat::expect_identical(triomics_ora_inventory_status("selected", sources)$status,
                             "outcome_selected")
})

testthat::test_that("ORA claim backgrounds come from the exact complete B claim or C block feature map", {
  primary <- triomics_ora_resolve_claim(
    d_ora_test_bundle("primary_statistics"),
    list(analysis_id = "analysis_1", block_id = "block_1",
         source_branch = "primary_statistics", claim_id = "claim_1")
  )
  testthat::expect_setequal(primary$feature_keys, c("feature_a", "feature_b", "feature_c"))
  testthat::expect_identical(primary$nomination_feature_keys, "feature_b")
  testthat::expect_identical(primary$family_id, "family_1")

  integration <- triomics_ora_resolve_claim(
    d_ora_test_bundle("integration_selection"),
    list(analysis_id = "analysis_1", block_id = "block_1",
         source_branch = "integration_selection", claim_id = "integration_1",
         integration_id = "integration_1")
  )
  testthat::expect_setequal(integration$feature_keys, c("feature_a", "feature_b", "feature_c"))
  testthat::expect_identical(integration$nomination_feature_keys, "feature_b")

  testthat::expect_identical(d_ora_code(triomics_ora_resolve_claim(
    d_ora_test_bundle("primary_statistics"),
    list(analysis_id = "analysis_1", block_id = "block_1",
         source_branch = "primary_statistics", claim_id = "wrong_claim")
  )), "D_ORA_LINEAGE_INVALID")
})

testthat::test_that("ORA mapping protocol refuses namespace, taxon, resolution, and target mismatches", {
  contract <- list(namespaces = "synthetic_gene", organisms = "9606", resolutions = "gene",
                   target_namespaces = "Reactome")
  claim <- list(source_namespace = "synthetic_gene", taxon_id = "9606",
                source_resolution = "gene", target_namespace = "Reactome")
  testthat::expect_identical(triomics_ora_mapping_compatibility(claim, contract)$status, "compatible")
  for (field in names(claim)) {
    wrong <- claim
    wrong[[field]] <- paste0("wrong_", wrong[[field]])
    testthat::expect_identical(triomics_ora_mapping_compatibility(wrong, contract)$status,
                               "incompatible", info = field)
  }
})
