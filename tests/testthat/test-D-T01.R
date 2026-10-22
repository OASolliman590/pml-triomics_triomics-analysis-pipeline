d_t01_hash <- function(character) {
  digest::digest(character, algo = "sha256", serialize = FALSE)
}

d_t01_invoke <- function(name, arguments) {
  implementation <- get0(name, mode = "function", inherits = TRUE)
  if (is.null(implementation)) return(NULL)
  do.call(implementation, arguments)
}

d_t01_nomination <- function(id, feature_key, source_branch, result_ref,
                             selection_rule, selection_value, q_threshold = NULL,
                             effect_threshold = NULL) {
  list(
    nomination_id = d_t01_hash(id),
    feature_key = feature_key,
    source_branch = source_branch,
    result_ref = result_ref,
    selection_rule = selection_rule,
    selection_value = selection_value,
    q_threshold = q_threshold,
    effect_threshold = effect_threshold
  )
}

d_t01_input <- function() {
  study_id <- "synthetic-d-t01"
  analysis_id <- "analysis-d-t01"
  gene_nomination <- d_t01_nomination(
    "gene-nomination", "gene_a", "primary_statistics",
    list(analysis_id = analysis_id, block_id = "proteome", claim_id = "contrast_1",
         family_id = "family_1"),
    "qualified_q_and_effect_thresholds", list(p = 0.01, q = 0.02, effect = 1.5),
    q_threshold = 0.05, effect_threshold = 1
  )
  lipid_nomination <- d_t01_nomination(
    "lipid-nomination", "lipid_a", "integration_selection",
    list(integration_id = "integration_1", backend = "mixomics_diablo",
         block_id = "lipidome", component_ids = list("comp_1")),
    "largest_absolute_component_loading", list(block_id = "lipidome", component_loadings = list(comp_1 = 0.8))
  )
  gene_rows <- list(
    list(result_id = "result_gene_a", feature_key = "gene_a", effect = 1.5, p = 0.01,
         q = 0.02, estimation_status = "estimated"),
    list(result_id = "result_gene_b", feature_key = "gene_b", effect = 0.1, p = 0.8,
         q = 0.9, estimation_status = "estimated")
  )
  lipid_rows <- list(
    list(result_id = "result_lipid_a", feature_key = "lipid_a", effect = -0.4, p = 0.2,
         q = 0.4, estimation_status = "estimated")
  )
  gene_family <- list(
    family_id = "family_1", member_ids = list("gene_a", "gene_b"), n_declared = 2L,
    dispositions = list(estimated = 2L), member_hash = d_t01_hash("gene_a|gene_b")
  )
  c_candidates <- list(
    schema_version = "2.0", artifact_type = "candidate_nominations",
    study_id = study_id, analysis_id = analysis_id,
    preparation_context_id = "prep_d_t01", representation_id = "levels",
    status = "complete", n_nominations = 2L,
    branches = list(
      primary_statistics = list(status = "complete", nominations = list(gene_nomination),
                                source_artifact_ref = list(artifact_id = "b-t07-candidate-nominations",
                                                           sha256 = d_t01_hash("b-nominations"))),
      integration_selection = list(status = "complete", nominations = list(lipid_nomination),
                                   source_artifact_ids = list("integration_1"))
    ),
    nominations = list(gene_nomination, lipid_nomination)
  )
  integration <- list(
    integration_id = "integration_1", backend = "mixomics_diablo", status = "complete",
    eligibility = list(source_roles = list(
      proteome = list(status = "supplied", value = list("measurement_matrix")),
      lipidome = list(status = "supplied", value = list("measurement_matrix"))
    )),
    selection = list(
      rule = "largest_absolute_component_loading",
      selected = list(lipidome = list("lipid_a", "lipid_b")),
      selected_values = list(lipidome = list(
        lipid_a = list(component_loadings = list(comp_1 = 0.8)),
        lipid_b = list(component_loadings = list(comp_1 = 0.2))
      ))
    )
  )
  list(
    study_id = study_id,
    analysis_id = analysis_id,
    source_manifests = list(
      list(source_id = "protein_source", study_id = study_id, role = "measurement_matrix",
           sha256 = d_t01_hash("protein-source"), selection_history = "full_unselected",
           selection_evidence = "Public synthetic fixture."),
      list(source_id = "lipid_source", study_id = study_id, role = "measurement_matrix",
           sha256 = d_t01_hash("lipid-source"), selection_history = "prespecified_panel",
           selection_evidence = "Public synthetic fixture panel.")
    ),
    feature_registry = list(
      gene_a = list(feature_key = "gene_a", block_id = "proteome", source_id = "protein_source",
                    source_row = "protein-row-a", id_namespace = "ensembl_gene", taxon_id = "9606",
                    structural_resolution = "gene", capabilities = list("gene_reaction"), measured_or_derived = "measured"),
      gene_b = list(feature_key = "gene_b", block_id = "proteome", source_id = "protein_source",
                    source_row = "protein-row-b", id_namespace = "ensembl_gene", taxon_id = "9606",
                    structural_resolution = "gene", capabilities = list("gene_reaction"), measured_or_derived = "measured"),
      lipid_a = list(feature_key = "lipid_a", block_id = "lipidome", source_id = "lipid_source",
                     source_row = "lipid-row-a", id_namespace = "hmdb_lipid", taxon_id = "9606",
                     structural_resolution = "sum_composition", capabilities = list("lipid_ontology"), measured_or_derived = "measured"),
      lipid_b = list(feature_key = "lipid_b", block_id = "lipidome", source_id = "lipid_source",
                     source_row = "lipid-row-b", id_namespace = "hmdb_lipid", taxon_id = "9606",
                     structural_resolution = "sum_composition", capabilities = list("lipid_ontology"), measured_or_derived = "measured")
    ),
    b = list(
      block_results = list(
        schema_version = "2.0", artifact_type = "block_results", study_id = study_id,
        analysis_id = analysis_id, status = "complete",
        blocks = list(
          proteome = list(contrasts = list(contrast_1 = list(family = gene_family, rows = gene_rows))),
          lipidome = list(contrasts = list(contrast_1 = list(
            family = list(family_id = "family_2", member_ids = list("lipid_a"), n_declared = 1L,
                         dispositions = list(estimated = 1L), member_hash = d_t01_hash("lipid_a")),
            rows = lipid_rows
          )))
        )
      ),
      family_ledger = list(
        schema_version = "2.0", artifact_type = "family_ledger", study_id = study_id,
        analysis_id = analysis_id, status = "complete",
        families = list(list(block_id = "proteome", claim_id = "contrast_1", family = gene_family),
                        list(block_id = "lipidome", claim_id = "contrast_1", family = list(
                          family_id = "family_2", member_ids = list("lipid_a"), n_declared = 1L,
                          dispositions = list(estimated = 1L), member_hash = d_t01_hash("lipid_a")
                        )))
      ),
      candidate_nominations = list(
        schema_version = "2.0", artifact_type = "candidate_nominations", study_id = study_id,
        analysis_id = analysis_id, status = "complete", source_artifact_id = "stage-b-block-results",
        nominations = list(gene_nomination)
      )
    ),
    c = list(
      candidate_nominations = c_candidates,
      integration_evidence = list(
        schema_version = "2.0", artifact_type = "integration_evidence_bundle",
        study_id = study_id, analysis_id = analysis_id,
        preparation_context_id = "prep_d_t01", representation_id = "levels",
        status = "partial", integrations = list(integration)
      ),
      evaluation_evidence = list(
        artifact_type = "integration_evaluation_evidence", study_id = study_id,
        analysis_id = analysis_id, status = "partial",
        controls = lapply(c("null", "weak_signal", "strong_signal", "adversarial"), function(control) {
          list(control = control, status = "not_run",
               candidate = list(status = "not_run", method = "not_selected", metrics = NULL),
               baseline = list(status = "not_run", method = "mixomics_diablo", metrics = NULL))
        })
      ),
      stage_status = list(
        artifact_type = "stage_c_status", study_id = study_id, analysis_id = analysis_id,
        status = "partial", clinical_prediction = list(status = "not_started",
          completion_policy = "conditional", packet_id = "C-09")
      )
    ),
    imported_statistics = list(list(
      evidence_id = "external-result-1", study_id = study_id, source_role = "imported_result",
      p = 0.001, q = 0.002, effect = 99, source_ref = "local-synthetic-reference"
    ))
  )
}

d_t01_routes <- function(enabled) {
  list(
    gene_reaction = list(enabled = enabled, namespaces = list("ensembl_gene"),
                         organisms = list("9606"), resolutions = list("gene"),
                         capabilities = list("gene_reaction")),
    lipid_ontology = list(enabled = enabled, namespaces = list("hmdb_lipid"),
                          organisms = list("9606"), resolutions = list("sum_composition"),
                          capabilities = list("lipid_ontology"))
  )
}

d_t01_write_c06_handoff <- function(directory, input = d_t01_input()) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  config_hash <- d_t01_hash("d-t01-config")
  records <- list(
    list(name = "candidate-nominations.json", type = "candidate_nominations",
         value = input$c$candidate_nominations),
    list(name = "integration-evidence.json", type = "integration_evidence_bundle",
         value = input$c$integration_evidence),
    list(name = "integration-comparisons.json", type = "integration_comparisons",
         value = list(schema_version = "2.0", artifact_type = "integration_comparisons",
                      study_id = input$study_id, analysis_id = input$analysis_id,
                      preparation_context_id = "prep_d_t01", representation_id = "levels",
                      status = "unavailable", comparisons = list())),
    list(name = "evaluation-evidence.json", type = "integration_evaluation_evidence",
         value = input$c$evaluation_evidence),
    list(name = "stage-status.json", type = "stage_c_status",
         value = input$c$stage_status)
  )
  members <- lapply(records, function(record) {
    record$value$preparation_context_id <- "prep_d_t01"
    record$value$representation_id <- "levels"
    payload_path <- file.path(directory, record$name)
    triomics_write_json_atomic(record$value, payload_path)
    envelope_path <- sub("[.]json$", ".envelope.json", payload_path)
    envelope <- triomics_artifact_envelope(
      artifact_type = record$type,
      artifact_id = paste0("c06-", sub("[.]json$", "", record$name)),
      study_id = input$study_id, run_id = "c-06", status = record$value$status,
      config_sha256 = config_hash, payload = payload_path, producer_commit = "synthetic-test",
      inputs = list(), reason_codes = character(), privacy_class = "synthetic"
    )
    envelope$analysis_id <- input$analysis_id
    envelope$preparation_context_id <- "prep_d_t01"
    envelope$representation_id <- "levels"
    triomics_write_envelope(envelope, envelope_path)
    list(artifact_type = record$type, payload_file = basename(payload_path),
         payload_sha256 = triomics_sha256(payload_path), envelope_file = basename(envelope_path),
         envelope_sha256 = triomics_sha256(envelope_path))
  })
  manifest_path <- file.path(directory, "handoff-manifest.json")
  manifest <- list(
    schema_version = "2.0", artifact_type = "stage_c_handoff_manifest",
    study_id = input$study_id, analysis_id = input$analysis_id,
    preparation_context_id = "prep_d_t01", representation_id = "levels",
    status = "partial", artifacts = members
  )
  triomics_write_json_atomic(manifest, manifest_path)
  manifest_envelope <- triomics_artifact_envelope(
    artifact_type = "stage_c_handoff_manifest", artifact_id = "c06-handoff-manifest",
    study_id = input$study_id, run_id = "c-06", status = "partial",
    config_sha256 = config_hash, payload = manifest_path, producer_commit = "synthetic-test",
    inputs = lapply(members, function(member) {
      list(artifact_id = paste0("c06-", sub("[.]json$", "", member$payload_file)),
           sha256 = member$payload_sha256)
    }), reason_codes = character(), privacy_class = "synthetic"
  )
  manifest_envelope$analysis_id <- input$analysis_id
  manifest_envelope$preparation_context_id <- "prep_d_t01"
  manifest_envelope$representation_id <- "levels"
  triomics_write_envelope(manifest_envelope, sub("[.]json$", ".envelope.json", manifest_path))
  directory
}

d_t01_write_b_handoff <- function(output_root, input) {
  b_dir <- file.path(output_root, "B")
  handoff_dir <- file.path(b_dir, "handoffs", "synthetic-d-t01-cache")
  dir.create(handoff_dir, recursive = TRUE, showWarnings = FALSE)
  config_hash <- d_t01_hash("d-t01-config")
  write_artifact <- function(value, path, type, id) {
    triomics_write_json_atomic(value, path)
    envelope_path <- sub("[.]json$", ".envelope.json", path)
    envelope <- triomics_artifact_envelope(
      artifact_type = type, artifact_id = id, study_id = input$study_id,
      run_id = "b-t07", status = value$status, config_sha256 = config_hash,
      payload = path, producer_commit = "synthetic-test", inputs = list(),
      privacy_class = "synthetic"
    )
    triomics_write_envelope(envelope, envelope_path)
    list(path = path, envelope_path = envelope_path, sha256 = triomics_sha256(path),
         envelope_sha256 = triomics_sha256(envelope_path))
  }
  block <- write_artifact(input$b$block_results, file.path(b_dir, "block-results.json"),
                          "block_results", "stage-b-block-results")
  family <- write_artifact(input$b$family_ledger, file.path(handoff_dir, "family-ledger.json"),
                           "family_ledger", "b-t07-family-ledger")
  nominations <- write_artifact(input$b$candidate_nominations,
                                file.path(handoff_dir, "candidate-nominations.json"),
                                "candidate_nominations", "b-t07-candidate-nominations")
  output_relative <- function(path) substring(normalizePath(path),
                                               nchar(normalizePath(output_root)) + 2L)
  members <- list(
    list(artifact_id = "block_results", status = "complete", path = output_relative(block$path),
         sha256 = block$sha256, envelope_path = output_relative(block$envelope_path),
         envelope_sha256 = block$envelope_sha256),
    list(artifact_id = "family_ledger", status = "complete", path = output_relative(family$path),
         sha256 = family$sha256, envelope_path = output_relative(family$envelope_path),
         envelope_sha256 = family$envelope_sha256),
    list(artifact_id = "candidate_nominations", status = "complete", path = output_relative(nominations$path),
         sha256 = nominations$sha256, envelope_path = output_relative(nominations$envelope_path),
         envelope_sha256 = nominations$envelope_sha256)
  )
  bundle_path <- file.path(handoff_dir, "bundle.json")
  bundle <- list(
    schema_version = "2.0", artifact_type = "stage_b_public_bundle",
    study_id = input$study_id, analysis_id = input$analysis_id,
    status = "complete", cache_key = d_t01_hash("synthetic-d-t01-cache"), members = members
  )
  triomics_write_json_atomic(bundle, bundle_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "stage_b_public_bundle", artifact_id = "b-t07-bundle",
    study_id = input$study_id, run_id = "b-t07", status = "complete",
    config_sha256 = config_hash, payload = bundle_path, producer_commit = "synthetic-test",
    inputs = list(), reason_codes = character(), privacy_class = "synthetic"
  )
  triomics_write_envelope(envelope, sub("[.]json$", ".envelope.json", bundle_path))
  list(
    bundle_path = bundle_path,
    candidate_ref = list(artifact_id = "b-t07-candidate-nominations", sha256 = nominations$sha256)
  )
}

testthat::test_that("D-T01 routes B and C nominations by molecular identity and keeps their full evidence", {
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(d_t01_input()))
  testthat::expect_true(is.list(bundle))
  if (!is.list(bundle)) return(invisible(NULL))

  routed <- d_t01_invoke("triomics_d_route_evidence", list(bundle, d_t01_routes(TRUE)))
  testthat::expect_true(is.list(routed))
  if (!is.list(routed)) return(invisible(NULL))
  testthat::expect_setequal(vapply(routed$routes$gene_reaction$nominations, `[[`, character(1), "feature_key"), "gene_a")
  testthat::expect_setequal(vapply(routed$routes$lipid_ontology$nominations, `[[`, character(1), "feature_key"), "lipid_a")
  testthat::expect_identical(routed$routes$gene_reaction$nominations[[1]]$source_branch, "primary_statistics")
  testthat::expect_identical(routed$routes$lipid_ontology$nominations[[1]]$source_branch, "integration_selection")
  testthat::expect_length(routed$routes$gene_reaction$nominations[[1]]$evidence$tested_rows, 2L)
  testthat::expect_identical(routed$routes$gene_reaction$nominations[[1]]$evidence$family$member_ids,
                             list("gene_a", "gene_b"))
  testthat::expect_true("selection_value" %in% names(routed$routes$gene_reaction$nominations[[1]]$evidence$nomination))
  testthat::expect_identical(routed$routes$lipid_ontology$nominations[[1]]$evidence$selection$selected,
                             list(lipidome = list("lipid_a", "lipid_b")))
  testthat::expect_identical(routed$study_id, "synthetic-d-t01")
})

testthat::test_that("D-T01 all-disabled routes retain accounting without making absence or enrichment claims", {
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(d_t01_input()))
  testthat::expect_true(is.list(bundle))
  if (!is.list(bundle)) return(invisible(NULL))
  routed <- d_t01_invoke("triomics_d_route_evidence", list(bundle, d_t01_routes(FALSE)))
  testthat::expect_true(is.list(routed))
  if (!is.list(routed)) return(invisible(NULL))

  testthat::expect_identical(routed$status, "complete")
  testthat::expect_identical(routed$interpretation, "no_biological_absence_inferred")
  testthat::expect_identical(routed$accounting$nomination_count, 2L)
  testthat::expect_identical(routed$accounting$measured_row_count, 3L)
  testthat::expect_identical(routed$accounting$routes_disabled, 2L)
  testthat::expect_identical(routed$enrichment$status, "not_requested")
  testthat::expect_length(routed$enrichment$results, 0L)
  testthat::expect_identical(routed$enrichment$test_count, 0L)
  testthat::expect_identical(routed$inherited_evidence$evaluation_evidence$controls[[1]]$status, "not_run")
  testthat::expect_identical(routed$inherited_evidence$stage_status$clinical_prediction$status, "not_started")

  output_root <- tempfile("d-t01-accounting-output-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  written <- d_t01_invoke("triomics_d_write_evidence_result", list(
    routed, output_root, d_t01_hash("d-t01-config"), "synthetic-test", "synthetic"
  ))
  testthat::expect_true(is.list(written))
  if (!is.list(written)) return(invisible(NULL))
  payload <- jsonlite::read_json(written$payload_path, simplifyVector = FALSE)
  testthat::expect_identical(payload$artifact_type, "annotation_routing_result")
  testthat::expect_equal(as.integer(payload$accounting$measured_row_count), 3L)
  testthat::expect_identical(payload$inherited_evidence$evaluation_evidence$controls[[1]]$status, "not_run")
  envelope <- jsonlite::read_json(written$envelope_path, simplifyVector = FALSE)
  testthat::expect_identical(envelope$study_id, "synthetic-d-t01")
  testthat::expect_identical(envelope$payload_sha256, triomics_sha256(written$payload_path))
  testthat::expect_true(triomics_validate_envelope(written$envelope_path))
})

testthat::test_that("D-T01 public annotation seam consumes the typed bundle and route request", {
  input <- d_t01_input()
  unverified <- d_t01_invoke("triomics_d_build_evidence_bundle", list(input))
  testthat::expect_error(d_t01_invoke("annotate_evidence", list(
    unverified, list(routes = d_t01_routes(FALSE))
  )), class = "triomics_error")

  output_root <- tempfile("d-t01-public-annotation-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  input$c$candidate_nominations$branches$primary_statistics$source_artifact_ref <-
    d_t01_write_b_handoff(output_root, input)$candidate_ref
  d_t01_write_c06_handoff(file.path(output_root, "D"), input)
  bundle <- d_t01_invoke("triomics_d_read_evidence_bundle", list(
    output_root, input$study_id, input$analysis_id, input$source_manifests,
    input$feature_registry, input$imported_statistics
  ))
  testthat::expect_true(is.list(bundle))
  if (!is.list(bundle)) return(invisible(NULL))
  result <- d_t01_invoke("annotate_evidence", list(bundle, list(routes = d_t01_routes(FALSE))))
  testthat::expect_true(is.list(result))
  if (!is.list(result)) return(invisible(NULL))
  testthat::expect_identical(result$artifact_type, "annotation_routing_result")
  testthat::expect_identical(result$accounting$routes_disabled, 2L)
})

testthat::test_that("D-T01 reconciles identical nominations but rejects mixed identities and unknown source roles", {
  input <- d_t01_input()
  input$c$candidate_nominations$nominations <- c(input$c$candidate_nominations$nominations,
                                                  list(input$c$candidate_nominations$nominations[[1]]))
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(input))
  testthat::expect_true(is.list(bundle))
  if (is.list(bundle)) {
    testthat::expect_identical(bundle$accounting$duplicate_nomination_count, 1L)
    testthat::expect_identical(length(bundle$nominations), 2L)
    testthat::expect_identical(length(bundle$measured_rows), 3L)
  }

  mixed <- d_t01_input()
  mixed$c$integration_evidence$study_id <- "different-study"
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(mixed)),
                         class = "triomics_error")
  unknown_role <- d_t01_input()
  unknown_role$source_manifests[[1]]$role <- "unreviewed_numeric_source"
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(unknown_role)),
                         class = "triomics_error")
  conflicting <- d_t01_input()
  conflicting$c$candidate_nominations$nominations[[2]]$nomination_id <-
    conflicting$c$candidate_nominations$nominations[[1]]$nomination_id
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(conflicting)),
                         class = "triomics_error")
})

testthat::test_that("D-T01 keeps imported statistics external and out of correction and enrichment inputs", {
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(d_t01_input()))
  testthat::expect_true(is.list(bundle))
  if (!is.list(bundle)) return(invisible(NULL))
  routed <- d_t01_invoke("triomics_d_route_evidence", list(bundle, d_t01_routes(TRUE)))
  testthat::expect_true(is.list(routed))
  if (!is.list(routed)) return(invisible(NULL))

  testthat::expect_identical(bundle$imported_evidence[[1]]$use, "external_display_concordance_only")
  testthat::expect_length(bundle$nominations, 2L)
  testthat::expect_identical(routed$accounting$imported_correction_inputs, 0L)
  testthat::expect_identical(routed$accounting$imported_enrichment_inputs, 0L)
  testthat::expect_identical(routed$imported_evidence[[1]]$effect, 99)
  testthat::expect_identical(routed$imported_evidence[[1]]$status, "display_only")
})

testthat::test_that("D-T01 resource access pins release, content, rights, cache and offline behavior", {
  root <- tempfile("d-t01-resource-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  resource_file <- file.path(root, "genes.tsv")
  writeLines(c("source_id\ttarget_id", "gene_a\tR1"), resource_file)
  content_hash <- triomics_sha256(resource_file)
  resource <- list(
    resource_id = "fixture-gene-map", version = "2026.1", sha256 = content_hash,
    source_url = "local://synthetic-gene-map", local_file = resource_file,
    terms_url = "local://synthetic-fixture",
    rights_status = "verified", permitted_operation = list("gene_reaction"),
    redistribution = "prohibited"
  )
  first <- d_t01_invoke("triomics_d_resource_access", list(
    resource, "gene_reaction", "2026.1", content_hash, file.path(root, "cache"), "offline"
  ))
  testthat::expect_true(is.list(first))
  if (!is.list(first)) return(invisible(NULL))
  testthat::expect_identical(first$status, "available")
  testthat::expect_identical(first$release, "2026.1")
  testthat::expect_identical(first$sha256, content_hash)
  testthat::expect_identical(first$cache_status, "written")
  testthat::expect_true(is.character(first$retrieval_timestamp) && nzchar(first$retrieval_timestamp))
  second <- d_t01_invoke("triomics_d_resource_access", list(
    resource, "gene_reaction", "2026.1", content_hash, file.path(root, "cache"), "offline"
  ))
  testthat::expect_identical(second$cache_status, "hit")
  testthat::expect_identical(second$retrieval_timestamp, first$retrieval_timestamp)
  testthat::expect_error(d_t01_invoke("triomics_d_resource_access", list(
    resource, "ora", "2026.1", content_hash, file.path(root, "cache"), "offline"
  )), class = "triomics_error")
  testthat::expect_error(d_t01_invoke("triomics_d_resource_access", list(
    resource, "gene_reaction", "2025.1", content_hash, file.path(root, "cache"), "offline"
  )), class = "triomics_error")
  mismatched_content <- resource
  mismatched_content$sha256 <- d_t01_hash("wrong-content")
  testthat::expect_error(d_t01_invoke("triomics_d_resource_access", list(
    mismatched_content, "gene_reaction", "2026.1", mismatched_content$sha256,
    file.path(root, "cache"), "offline"
  )), class = "triomics_error")

  missing <- resource
  missing$local_file <- file.path(root, "absent.tsv")
  missing$sha256 <- d_t01_hash("not-present")
  unavailable <- d_t01_invoke("triomics_d_resource_access", list(
    missing, "gene_reaction", "2026.1", missing$sha256, file.path(root, "empty-cache"), "offline"
  ))
  testthat::expect_identical(unavailable$status, "unavailable")
  testthat::expect_identical(unavailable$reason_code, "D_RESOURCE_MISSING")
  testthat::expect_null(unavailable$path)
  testthat::expect_false(dir.exists(file.path(root, "empty-cache")))
  fetch_unavailable <- d_t01_invoke("triomics_d_resource_access", list(
    missing, "gene_reaction", "2026.1", missing$sha256,
    file.path(root, "fetch-cache"), "approved_reference_fetch_only"
  ))
  testthat::expect_identical(fetch_unavailable$status, "unavailable")
  testthat::expect_identical(fetch_unavailable$reason_code, "D_RESOURCE_FETCH_UNAVAILABLE")
  testthat::expect_false(fetch_unavailable$network_attempted)
  testthat::expect_false(dir.exists(file.path(root, "fetch-cache")))
})

testthat::test_that("D-T01 reads only hash-verified C-06 handoffs and preserves explicit unrun work", {
  root <- tempfile("d-t01-c06-handoff-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  handoff_dir <- d_t01_write_c06_handoff(root)
  handoff <- d_t01_invoke("triomics_d_read_c06_handoff", list(
    handoff_dir, "synthetic-d-t01", "analysis-d-t01"
  ))
  testthat::expect_true(is.list(handoff))
  if (!is.list(handoff)) return(invisible(NULL))
  testthat::expect_identical(handoff$manifest$status, "partial")
  testthat::expect_identical(handoff$evaluation_evidence$controls[[1]]$status, "not_run")
  testthat::expect_identical(handoff$stage_status$clinical_prediction$status, "not_started")
  testthat::expect_identical(handoff$stage_status$clinical_prediction$completion_policy, "conditional")
  testthat::expect_identical(handoff$manifest_sha256, triomics_sha256(file.path(handoff_dir, "handoff-manifest.json")))

  writeLines("tampered payload", file.path(handoff_dir, "candidate-nominations.json"))
  testthat::expect_error(d_t01_invoke("triomics_d_read_c06_handoff", list(
    handoff_dir, "synthetic-d-t01", "analysis-d-t01"
  )), class = "triomics_error")
})

testthat::test_that("D-T01 resolves the exact B artifact named by the C-06 immutable nomination reference", {
  root <- tempfile("d-t01-b-c-handoffs-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- d_t01_input()
  b_ref <- d_t01_write_b_handoff(root, input)$candidate_ref
  input$c$candidate_nominations$branches$primary_statistics$source_artifact_ref <- b_ref
  d_t01_write_c06_handoff(file.path(root, "D"), input)
  c_handoff <- d_t01_invoke("triomics_d_read_c06_handoff", list(
    file.path(root, "D"), input$study_id, input$analysis_id
  ))
  testthat::expect_true(is.list(c_handoff))
  if (!is.list(c_handoff)) return(invisible(NULL))
  b_handoff <- d_t01_invoke("triomics_d_read_b_handoff", list(
    root, c_handoff$candidate_nominations$branches$primary_statistics$source_artifact_ref,
    input$study_id, input$analysis_id
  ))
  testthat::expect_true(is.list(b_handoff))
  if (!is.list(b_handoff)) return(invisible(NULL))
  testthat::expect_identical(b_handoff$block_results$study_id, input$study_id)
  testthat::expect_identical(b_handoff$family_ledger$families[[1]]$family$member_ids,
                             list("gene_a", "gene_b"))
  testthat::expect_identical(b_handoff$candidate_nominations$nominations[[1]]$nomination_id,
                             input$b$candidate_nominations$nominations[[1]]$nomination_id)

  evidence_bundle <- d_t01_invoke("triomics_d_read_evidence_bundle", list(
    root, input$study_id, input$analysis_id,
    input$source_manifests, input$feature_registry, input$imported_statistics
  ))
  testthat::expect_true(is.list(evidence_bundle))
  if (is.list(evidence_bundle)) {
    testthat::expect_true(is.list(evidence_bundle$immutable_inputs))
    testthat::expect_identical(evidence_bundle$accounting$measured_row_count, 3L)
    testthat::expect_identical(evidence_bundle$c$evaluation_evidence$controls[[1]]$status, "not_run")
  }

  wrong_ref <- c_handoff$candidate_nominations$branches$primary_statistics$source_artifact_ref
  wrong_ref$sha256 <- d_t01_hash("another-candidate-artifact")
  testthat::expect_error(d_t01_invoke("triomics_d_read_b_handoff", list(
    root, wrong_ref, input$study_id, input$analysis_id
  )), class = "triomics_error")
})
