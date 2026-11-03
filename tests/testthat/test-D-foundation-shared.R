d_foundation_ticket <- function(ticket) {
  path <- triomics_test_resource("examples", "tickets", ticket)
  list(path = path, config = yaml::read_yaml(path, eval.expr = FALSE))
}

d_foundation_cli_run <- function(config_path, stage, output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", shQuote(stage), "--output", shQuote(output_root)),
    stdout = TRUE, stderr = TRUE
  ))
  exit_code <- attr(output, "status")
  if (is.null(exit_code)) exit_code <- 0L
  list(exit_code = as.integer(exit_code), output = output)
}

d_foundation_payload <- function(output_root) {
  pointer_path <- file.path(output_root, "registries", "current-D.json")
  pointer <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  payload_path <- file.path(output_root, "D", "generations", pointer$generation_id,
                            "evidence-routing.json")
  list(pointer = pointer, payload = jsonlite::read_json(payload_path, simplifyVector = FALSE),
       payload_path = payload_path)
}

testthat::test_that("D-T02 and D-T03 tickets pin local mapping resources by content and rights", {
  cases <- list(
    list(ticket = "D-T02.yaml", route = "lipid_ontology", block_id = "block_2",
         table = "lipid-mapping.tsv", feature_metadata = "lipid-feature-metadata.tsv",
         resource_id = "d-foundation-lipid"),
    list(ticket = "D-T03.yaml", route = "gene_reaction", block_id = "block_1",
         table = "gene-reaction-mapping.tsv", feature_metadata = "gene-feature-metadata.tsv",
         resource_id = "d-foundation-gene")
  )

  for (case in cases) {
    fixture <- d_foundation_ticket(case$ticket)
    requests <- fixture$config$annotation$requests
    request <- Filter(function(item) identical(item$route, case$route), requests)
    testthat::expect_length(request, 1L)
    if (length(request) != 1L) next
    testthat::expect_identical(request[[1L]]$resource_ids, case$resource_id)
    testthat::expect_true(isTRUE(request[[1L]]$required), info = case$ticket)

    block <- Filter(function(item) identical(item$block_id, case$block_id), fixture$config$blocks)
    testthat::expect_length(block, 1L)
    block <- block[[1L]]
    testthat::expect_identical(block$annotation_route, case$route)
    metadata_path <- normalizePath(file.path(dirname(fixture$path), block$feature_metadata),
                                   winslash = "/", mustWork = TRUE)
    metadata <- utils::read.delim(metadata_path, colClasses = "character", check.names = FALSE)
    testthat::expect_true(all(c("feature_id", "taxon_id", "resolution", "capabilities",
                                "provenance_ref") %in% names(metadata)))
    testthat::expect_setequal(metadata$capabilities, case$route)

    resources <- fixture$config$annotation$resources
    resource <- Filter(function(item) identical(item$resource_id, request[[1L]]$resource_ids[[1L]]), resources)
    testthat::expect_length(resource, 1L)
    if (length(resource) != 1L) next
    resource <- resource[[1L]]
    testthat::expect_identical(resource$rights_status, "verified", info = case$ticket)
    testthat::expect_true(case$route %in% unlist(resource$permitted_operation, use.names = FALSE),
                          info = case$ticket)
    testthat::expect_identical(resource$resource_id, case$resource_id)

    resource_path <- normalizePath(file.path(dirname(fixture$path), resource$local_file),
                                   winslash = "/", mustWork = TRUE)
    testthat::expect_identical(triomics_sha256(resource_path), resource$sha256,
                               info = case$ticket)
    table <- utils::read.delim(resource_path, colClasses = "character", check.names = FALSE)
    required <- c("source_namespace", "source_id", "taxon_id", "target_namespace",
                  "target_id", "relation", "resolution", "evidence_ref", "resource_id")
    testthat::expect_true(all(required %in% names(table)), info = case$ticket)
    testthat::expect_gt(nrow(table), 0L)
    testthat::expect_true(all(table$resource_id == resource$resource_id), info = case$ticket)
    testthat::expect_true(all(metadata$feature_id %in% c("PC(34:1)", "PE(36:2)", "TG(50:1)",
                                                        "SYN-GENE-001", "SYN-GENE-002", "SYN-GENE-003")),
                          info = case$ticket)
  }
})

testthat::test_that("D-T02 and D-T03 map actual same-root native B and C nominations", {
  cases <- list(
    list(ticket = "D-T02.yaml", route = "lipid_ontology", resource_id = "d-foundation-lipid"),
    list(ticket = "D-T03.yaml", route = "gene_reaction", resource_id = "d-foundation-gene")
  )
  for (case in cases) {
    fixture <- d_foundation_ticket(case$ticket)
    output_root <- tempfile(paste0("d-foundation-", case$route, "-"),
                            tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
    dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(output_root, recursive = TRUE, force = TRUE), add = TRUE)

    b_run <- d_foundation_cli_run(fixture$path, "B", output_root)
    testthat::expect_identical(b_run$exit_code, 0L, info = paste(b_run$output, collapse = "\n"))
    if (b_run$exit_code != 0L) next
    c_run <- d_foundation_cli_run(fixture$path, "C", output_root)
    testthat::expect_identical(c_run$exit_code, 0L, info = paste(c_run$output, collapse = "\n"))
    if (c_run$exit_code != 0L) next
    d_run <- d_foundation_cli_run(fixture$path, "D", output_root)
    testthat::expect_identical(d_run$exit_code, 0L, info = paste(d_run$output, collapse = "\n"))
    if (d_run$exit_code != 0L) next

    native <- d_foundation_payload(output_root)
    manifest_path <- file.path(output_root, native$pointer$generation_manifest$path)
    manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
    payload_member <- Filter(function(member) identical(member$path, "evidence-routing.json"),
                             manifest$members)
    testthat::expect_length(payload_member, 1L)
    testthat::expect_identical(triomics_sha256(manifest_path), native$pointer$generation_id)
    testthat::expect_identical(triomics_sha256(native$payload_path), payload_member[[1L]]$digest)
    route <- native$payload$routes[[case$route]]
    mapping <- route$mappings[[case$resource_id]]
    testthat::expect_identical(mapping$status, "available")
    testthat::expect_identical(mapping$resource$network_policy, "offline")
    testthat::expect_false(mapping$resource$network_attempted)
    testthat::expect_false(mapping$resource$study_upload)
    testthat::expect_identical(mapping$resource$content_sha256,
                               fixture$config$annotation$resources[[1L]]$sha256)
    testthat::expect_gt(length(mapping$observations), 0L)

    nominated_features <- unique(vapply(route$nominations, `[[`, character(1), "feature_key"))
    testthat::expect_length(mapping$observations, length(nominated_features))
    if (identical(case$route, "lipid_ontology")) {
      pc <- Filter(function(row) identical(row$source_id, "PC(34:1)"), mapping$observations)
      testthat::expect_length(pc, 1L)
      testthat::expect_identical(pc[[1L]]$resolution, "sum_composition")
      testthat::expect_identical(pc[[1L]]$class, "PC")
      testthat::expect_identical(pc[[1L]]$carbons, 34L)
      testthat::expect_identical(pc[[1L]]$unsaturations, 1L)
      testthat::expect_null(pc[[1L]]$chains)
      testthat::expect_null(pc[[1L]]$sn_positions)
      testthat::expect_null(pc[[1L]]$isomer)
      pc_edges <- Filter(function(edge) identical(edge$feature_key, pc[[1L]]$feature_key), mapping$edges)
      testthat::expect_setequal(vapply(pc_edges, `[[`, character(1), "target_entity_id"),
                                c("LIPID:PC34:A", "LIPID:PC34:B"))
      testthat::expect_identical(pc_edges[[1L]]$ambiguity_group, pc_edges[[2L]]$ambiguity_group)
      testthat::expect_identical(mapping$observations[[which(vapply(mapping$observations,
        function(row) identical(row$source_id, "TG(50:1)"), logical(1)))[[1L]]]]$mapping_status, "unmapped")
    } else {
      mapped <- Filter(function(row) identical(row$mapping_status, "mapped"), mapping$observations)
      testthat::expect_length(mapped, 1L)
      testthat::expect_identical(mapped[[1L]]$source_id, "SYN-GENE-001")
      testthat::expect_identical(mapped[[1L]]$source_namespace, "synthetic_gene")
      testthat::expect_identical(mapped[[1L]]$taxon_id, "9606")
      testthat::expect_setequal(vapply(mapped[[1L]]$nominations, `[[`, character(1), "source_branch"),
                                c("primary_statistics", "integration_selection"))
      testthat::expect_length(unique(vapply(mapped[[1L]]$nominations, `[[`, character(1), "nomination_id")), 2L)
      testthat::expect_false(mapped[[1L]]$independent_corroboration)
      mapped_edges <- Filter(function(edge) identical(edge$feature_key, mapped[[1L]]$feature_key), mapping$edges)
      testthat::expect_setequal(vapply(mapped_edges, `[[`, character(1), "edge_type"),
                                c("reaction", "go_function"))
      reaction_edges <- Filter(function(edge) identical(edge$edge_type, "reaction"), mapped_edges)
      testthat::expect_length(reaction_edges, 2L)
      testthat::expect_identical(reaction_edges[[1L]]$ambiguity_group,
                                 reaction_edges[[2L]]$ambiguity_group)
      unmapped_ids <- vapply(Filter(function(row) identical(row$mapping_status, "unmapped"),
                                    mapping$observations), `[[`, character(1), "source_id")
      testthat::expect_setequal(unmapped_ids, c("SYN-GENE-002", "SYN-GENE-003"))
    }
  }
})
