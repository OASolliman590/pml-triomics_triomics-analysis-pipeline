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

d_t01_as_shared_input <- function(input) {
  original_registry <- input$feature_registry
  old_keys <- names(original_registry)
  new_keys <- stats::setNames(vapply(old_keys, function(old_key) {
    feature <- original_registry[[old_key]]
    triomics_feature_key(input$study_id, feature$block_id, feature$source_row)
  }, character(1)), old_keys)
  remap_key <- function(value) {
    value <- as.character(value)
    if (!value %in% names(new_keys)) stop("Unknown synthetic feature key: ", value)
    unname(new_keys[[value]])
  }
  remap_family <- function(family) {
    family$member_ids <- lapply(family$member_ids, remap_key)
    family$member_hash <- d_t01_hash(paste(unlist(family$member_ids, use.names = FALSE), collapse = "|"))
    family
  }
  remap_nominations <- function(items) {
    lapply(items, function(item) {
      item$feature_key <- remap_key(item$feature_key)
      item
    })
  }

  input$b$block_results$preparation_context_id <- "prep_d_t01"
  input$b$block_results$representation_id <- "levels"
  for (block_id in names(input$b$block_results$blocks)) {
    block <- input$b$block_results$blocks[[block_id]]
    for (group_name in c("contrasts", "joint_claims")) {
      claims <- block[[group_name]]
      if (!is.list(claims)) next
      for (claim_id in names(claims)) {
        claim <- claims[[claim_id]]
        claim$family <- remap_family(claim$family)
        claim$rows <- lapply(claim$rows, function(row) {
          row$feature_key <- remap_key(row$feature_key)
          row
        })
        claims[[claim_id]] <- claim
      }
      block[[group_name]] <- claims
    }
    input$b$block_results$blocks[[block_id]] <- block
  }
  input$b$family_ledger$families <- lapply(input$b$family_ledger$families, function(record) {
    record$family <- remap_family(record$family)
    record
  })
  input$b$candidate_nominations$nominations <- remap_nominations(input$b$candidate_nominations$nominations)
  input$c$candidate_nominations$branches$primary_statistics$nominations <- remap_nominations(
    input$c$candidate_nominations$branches$primary_statistics$nominations
  )
  input$c$candidate_nominations$branches$integration_selection$nominations <- remap_nominations(
    input$c$candidate_nominations$branches$integration_selection$nominations
  )
  input$c$candidate_nominations$nominations <- remap_nominations(input$c$candidate_nominations$nominations)
  integration <- input$c$integration_evidence$integrations[[1]]
  integration$selection$selected <- lapply(integration$selection$selected, function(ids) {
    lapply(ids, remap_key)
  })
  integration$selection$selected_values <- lapply(integration$selection$selected_values, function(values) {
    names(values) <- vapply(names(values), remap_key, character(1))
    values
  })
  input$c$integration_evidence$integrations[[1]] <- integration

  registry_rows <- lapply(old_keys, function(old_key) {
    feature <- original_registry[[old_key]]
    data.frame(
      study_id = input$study_id,
      block_id = feature$block_id,
      feature_key = unname(new_keys[[old_key]]),
      original_id = old_key,
      id_namespace = feature$id_namespace,
      source_id = feature$source_id,
      source_row = feature$source_row,
      measured_or_derived = feature$measured_or_derived,
      quantity = "intensity",
      scale = "linear",
      units = "synthetic_arbitrary_units",
      structural_resolution = "unknown",
      alias_refs = "",
      parent_refs = "",
      annotation_route = if (grepl("^gene_", old_key)) "gene_reaction" else "lipid_ontology",
      stringsAsFactors = FALSE
    )
  })
  input$feature_registry <- do.call(rbind, registry_rows)
  rownames(input$feature_registry) <- NULL
  input$routing_metadata <- stats::setNames(lapply(old_keys, function(old_key) {
    feature <- original_registry[[old_key]]
    list(
      taxon = list(status = "declared", value = feature$taxon_id,
                   provenance_ref = "synthetic://d-t01/taxon"),
      capabilities = list(status = "declared", value = unname(unlist(feature$capabilities)),
                           provenance_ref = "synthetic://d-t01/capabilities"),
      resolution = list(status = "declared", value = feature$structural_resolution,
                        provenance_ref = "synthetic://d-t01/resolution")
    )
  }), unname(new_keys))
  input
}

d_t01_key <- function(label) {
  source_rows <- c(gene_a = "protein-row-a", gene_b = "protein-row-b",
                   lipid_a = "lipid-row-a", lipid_b = "lipid-row-b")
  block_ids <- c(gene_a = "proteome", gene_b = "proteome",
                 lipid_a = "lipidome", lipid_b = "lipidome")
  triomics_feature_key("synthetic-d-t01", block_ids[[label]], source_rows[[label]])
}

d_t01_input <- function() {
  study_id <- "synthetic-d-t01"
  analysis_id <- "analysis-d-t01"
  gene_nomination <- d_t01_nomination(
    "gene-nomination", "gene_a", "primary_statistics",
    list(analysis_id = analysis_id, block_id = "proteome", claim_id = "contrast_1",
         family_id = "family_1"),
    "valid_q_at_or_below_threshold_and_absolute_effect_at_or_above_reporting_threshold",
    list(p = 0.01, q = 0.02, effect = 1.5, effect_unit = "group_difference"),
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
         q = 0.02, effect_unit = "group_difference", estimation_status = "estimated"),
    list(result_id = "result_gene_b", feature_key = "gene_b", effect = 0.1, p = 0.8,
         q = 0.9, effect_unit = "group_difference", estimation_status = "estimated")
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
    common = list(scores = list(columns = list("comp_1"))),
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
  input <- list(
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
        analysis_id = analysis_id, preparation_context_id = "prep_d_t01",
        representation_id = "levels", status = "complete",
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
        analysis_id = analysis_id, preparation_context_id = "prep_d_t01",
        representation_id = "levels", status = "partial",
        controls = lapply(c("null", "weak_signal", "strong_signal", "adversarial"), function(control) {
          list(control = control, status = "not_run",
               candidate = list(status = "not_run", method = "not_selected", metrics = NULL),
               baseline = list(status = "not_run", method = "mixomics_diablo", metrics = NULL))
        })
      ),
      stage_status = list(
        artifact_type = "stage_c_status", study_id = study_id, analysis_id = analysis_id,
        preparation_context_id = "prep_d_t01", representation_id = "levels",
        status = "partial", clinical_prediction = list(status = "not_started",
          completion_policy = "conditional", packet_id = "C-09")
      )
    ),
    imported_statistics = list(list(
      evidence_id = "external-result-1", study_id = study_id, source_role = "imported_result",
      p = 0.001, q = 0.002, effect = 99, source_ref = "local-synthetic-reference"
    ))
  )
  d_t01_as_shared_input(input)
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
  write_artifact <- function(value, path, type, id, inputs = list()) {
    triomics_write_json_atomic(value, path)
    envelope_path <- sub("[.]json$", ".envelope.json", path)
    envelope <- triomics_artifact_envelope(
      artifact_type = type, artifact_id = id, study_id = input$study_id,
      run_id = "b-t07", status = value$status, config_sha256 = config_hash,
      payload = path, producer_commit = "synthetic-test", inputs = inputs,
      privacy_class = "synthetic"
    )
    triomics_write_envelope(envelope, envelope_path)
    list(path = path, envelope_path = envelope_path, sha256 = triomics_sha256(path),
         envelope_sha256 = triomics_sha256(envelope_path))
  }
  block <- write_artifact(input$b$block_results, file.path(b_dir, "block-results.json"),
                          "block_results", "stage-b-block-results")
  family <- write_artifact(input$b$family_ledger, file.path(handoff_dir, "family-ledger.json"),
                           "family_ledger", "b-t07-family-ledger",
                           list(list(artifact_id = "stage-b-block-results", sha256 = block$sha256)))
  nominations <- write_artifact(input$b$candidate_nominations,
                                file.path(handoff_dir, "candidate-nominations.json"),
                                "candidate_nominations", "b-t07-candidate-nominations",
                                list(list(artifact_id = "stage-b-block-results", sha256 = block$sha256)))
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
    artifact_id = "b-t07-bundle",
    study_id = input$study_id, analysis_id = input$analysis_id,
    preparation_context_id = input$b$block_results$preparation_context_id,
    representation_id = input$b$block_results$representation_id,
    status = "complete", cache_key = d_t01_hash("synthetic-d-t01-cache"), members = members
  )
  triomics_write_json_atomic(bundle, bundle_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "stage_b_public_bundle", artifact_id = bundle$artifact_id,
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

d_t01_public_bundle <- function(root, input, routing_metadata = NULL) {
  b_handoff <- d_t01_write_b_handoff(root, input)
  input$c$candidate_nominations$branches$primary_statistics$source_artifact_ref <- b_handoff$candidate_ref
  d_t01_write_c06_handoff(file.path(root, "D"), input)
  arguments <- list(root, input$study_id, input$analysis_id, input$source_manifests,
                    input$feature_registry, input$imported_statistics)
  routing_metadata <- routing_metadata %||% input$routing_metadata
  if (!is.null(routing_metadata)) arguments[[length(arguments) + 1L]] <- routing_metadata
  d_t01_invoke("triomics_d_read_evidence_bundle", arguments)
}

testthat::test_that("D-T01 routes B and C nominations by molecular identity and keeps their full evidence", {
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(d_t01_input()))
  testthat::expect_true(is.list(bundle))
  if (!is.list(bundle)) return(invisible(NULL))

  routed <- d_t01_invoke("triomics_d_route_evidence", list(bundle, d_t01_routes(TRUE)))
  testthat::expect_true(is.list(routed))
  if (!is.list(routed)) return(invisible(NULL))
  testthat::expect_setequal(vapply(routed$routes$gene_reaction$nominations, `[[`, character(1), "feature_key"), d_t01_key("gene_a"))
  testthat::expect_setequal(vapply(routed$routes$lipid_ontology$nominations, `[[`, character(1), "feature_key"), d_t01_key("lipid_a"))
  testthat::expect_identical(routed$routes$gene_reaction$nominations[[1]]$source_branch, "primary_statistics")
  testthat::expect_identical(routed$routes$lipid_ontology$nominations[[1]]$source_branch, "integration_selection")
  testthat::expect_length(routed$routes$gene_reaction$nominations[[1]]$evidence$tested_rows, 2L)
  testthat::expect_identical(routed$routes$gene_reaction$nominations[[1]]$evidence$family$member_ids,
                             list(d_t01_key("gene_a"), d_t01_key("gene_b")))
  testthat::expect_true("selection_value" %in% names(routed$routes$gene_reaction$nominations[[1]]$evidence$nomination))
  testthat::expect_identical(routed$routes$lipid_ontology$nominations[[1]]$evidence$selection$selected,
                             list(lipidome = list(d_t01_key("lipid_a"), d_t01_key("lipid_b"))))
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
})

testthat::test_that("D-T01 public annotation seam consumes the typed bundle and route request", {
  input <- d_t01_input()
  unverified <- d_t01_invoke("triomics_d_build_evidence_bundle", list(input))
  missing_profile <- tryCatch(d_t01_invoke("annotate_evidence", list(
    unverified, list(routes = d_t01_routes(FALSE))
  )), error = identity)
  testthat::expect_identical(missing_profile$code, "D_PROVENANCE_REQUIRED")

  output_root <- tempfile("d-t01-public-annotation-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  missing_reader_profile <- tryCatch(d_t01_public_bundle(output_root, input), error = identity)
  testthat::expect_identical(missing_reader_profile$code, "D_PROVENANCE_REQUIRED")
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
                             list(d_t01_key("gene_a"), d_t01_key("gene_b")))
  testthat::expect_identical(b_handoff$candidate_nominations$nominations[[1]]$nomination_id,
                             input$b$candidate_nominations$nominations[[1]]$nomination_id)

  evidence_bundle <- tryCatch(d_t01_invoke("triomics_d_read_evidence_bundle", list(
    root, input$study_id, input$analysis_id,
    input$source_manifests, input$feature_registry, input$imported_statistics
  )), error = identity)
  testthat::expect_identical(evidence_bundle$code, "D_PROVENANCE_REQUIRED")

  wrong_ref <- c_handoff$candidate_nominations$branches$primary_statistics$source_artifact_ref
  wrong_ref$sha256 <- d_t01_hash("another-candidate-artifact")
  testthat::expect_error(d_t01_invoke("triomics_d_read_b_handoff", list(
    root, wrong_ref, input$study_id, input$analysis_id
  )), class = "triomics_error")
})

testthat::test_that("D-T01 refuses public routing without exact producer provenance", {
  root <- tempfile("d-t01-public-lineage-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- d_t01_input()
  missing_profile <- tryCatch(d_t01_public_bundle(root, input), error = identity)
  testthat::expect_identical(missing_profile$code, "D_PROVENANCE_REQUIRED")
})

testthat::test_that("D-T01 disabled output retains failed and nonnominated B evidence", {
  root <- tempfile("d-t01-full-b-evidence-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- d_t01_input()
  failed_family <- list(
    family_id = "family_failed_nonnominated", member_ids = list(d_t01_key("gene_b")), n_declared = 1L,
    dispositions = list(failed = 1L), member_hash = d_t01_hash(d_t01_key("gene_b"))
  )
  failed_claim <- list(
    family = failed_family,
    rows = list(list(result_id = "result_gene_b_failed", feature_key = d_t01_key("gene_b"),
                     estimation_status = "failed", reason = "synthetic_not_estimable"))
  )
  input$b$block_results$blocks$proteome$contrasts$contrast_failed <- failed_claim
  input$b$family_ledger$families[[3]] <- list(
    block_id = "proteome", claim_id = "contrast_failed", family = failed_family
  )
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(input))
  routed <- d_t01_invoke("triomics_d_route_evidence", list(bundle, d_t01_routes(FALSE)))
  testthat::expect_true(is.list(routed$inherited_evidence$b))
  if (!is.list(routed$inherited_evidence$b)) return(invisible(NULL))
  testthat::expect_true("contrast_failed" %in% names(
    routed$inherited_evidence$b$block_results$blocks$proteome$contrasts
  ))
  failed <- routed$inherited_evidence$b$block_results$blocks$proteome$contrasts$contrast_failed
  testthat::expect_identical(failed$family$family_id, "family_failed_nonnominated")
  testthat::expect_identical(failed$rows[[1]]$result_id, "result_gene_b_failed")
  testthat::expect_identical(failed$rows[[1]]$estimation_status, "failed")
})

testthat::test_that("D-T01 reconciles canonical nomination bodies and producer-selected values", {
  flattened_feature <- d_t01_input()
  flattened_feature$c$candidate_nominations$nominations[[1]]$feature_key <- d_t01_key("gene_b")
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(flattened_feature)),
                         class = "triomics_error")

  flattened_stat <- d_t01_input()
  flattened_stat$c$candidate_nominations$nominations[[1]]$selection_value$effect <- 99
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(flattened_stat)),
                         class = "triomics_error")

  branch_mismatch <- d_t01_input()
  branch_mismatch$c$candidate_nominations$branches$primary_statistics$nominations[[1]]$feature_key <- d_t01_key("gene_b")
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(branch_mismatch)),
                         class = "triomics_error")

  primary_consistent_tamper <- d_t01_input()
  forged_primary <- primary_consistent_tamper$c$candidate_nominations$branches$primary_statistics$nominations[[1]]
  forged_primary$feature_key <- d_t01_key("gene_b")
  forged_primary$selection_value$effect <- 99
  primary_consistent_tamper$c$candidate_nominations$branches$primary_statistics$nominations[[1]] <- forged_primary
  primary_consistent_tamper$c$candidate_nominations$nominations[[1]] <- forged_primary
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(primary_consistent_tamper)),
                         class = "triomics_error")

  integration_body <- d_t01_input()
  integration_body$c$candidate_nominations$branches$integration_selection$nominations[[1]]$selection_value$component_loadings$comp_1 <- 99
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(integration_body)),
                         class = "triomics_error")

  integration_consistent_tamper <- d_t01_input()
  forged_integration <- integration_consistent_tamper$c$candidate_nominations$branches$integration_selection$nominations[[1]]
  forged_integration$selection_value$component_loadings$comp_1 <- 99
  integration_consistent_tamper$c$candidate_nominations$branches$integration_selection$nominations[[1]] <- forged_integration
  integration_consistent_tamper$c$candidate_nominations$nominations[[2]] <- forged_integration
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(integration_consistent_tamper)),
                         class = "triomics_error")

  integration_value <- d_t01_input()
  integration_value$c$integration_evidence$integrations[[1]]$selection$selected_values$lipidome[[d_t01_key("lipid_a")]]$component_loadings$comp_1 <- 0.9
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(integration_value)),
                         class = "triomics_error")
})

testthat::test_that("D-T01 preserves structured native integration selection rules", {
  input <- d_t01_input()
  selection_rule <- list(rule = "largest_absolute_component_loading",
                         criterion = "maximum_absolute_loading", value = 1L)
  input$c$integration_evidence$integrations[[1]]$selection$rule <- selection_rule
  input$c$candidate_nominations$branches$integration_selection$nominations[[1]]$selection_rule <- selection_rule
  input$c$candidate_nominations$nominations[[2]]$selection_rule <- selection_rule

  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(input))
  testthat::expect_identical(bundle$nominations[[2]]$selection_rule, selection_rule)
})

testthat::test_that("D-T01 refuses B evidence with missing or conflicting preparation lineage", {
  mismatch <- d_t01_input()
  mismatch$b$block_results$preparation_context_id <- "different-b-preparation"
  mismatch$b$block_results$representation_id <- "delta"
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(mismatch)),
                         class = "triomics_error")

  missing <- d_t01_input()
  missing$b$block_results$preparation_context_id <- NULL
  missing$b$block_results$representation_id <- NULL
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(missing)),
                         class = "triomics_error")
})

testthat::test_that("D-T01 rejects cache and output paths traversing symlink ancestors", {
  root <- tempfile("d-t01-path-boundary-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  allowed <- file.path(root, "allowed")
  outside <- file.path(root, "outside")
  dir.create(allowed)
  dir.create(outside)
  resource_file <- file.path(root, "reference.tsv")
  writeLines(c("source_id\ttarget_id", "gene_a\tR1"), resource_file)
  content_hash <- triomics_sha256(resource_file)
  resource <- list(
    resource_id = "fixture-path-map", version = "2026.1", sha256 = content_hash,
    source_url = "local://synthetic-path-map", local_file = resource_file,
    terms_url = "local://synthetic-fixture", rights_status = "verified",
    permitted_operation = list("gene_reaction"), redistribution = "prohibited"
  )
  primed <- d_t01_invoke("triomics_d_resource_access", list(
    resource, "gene_reaction", "2026.1", content_hash, file.path(outside, "cache"), "offline"
  ))
  testthat::expect_identical(primed$status, "available")
  link <- file.path(allowed, "escape")
  testthat::expect_true(file.symlink(outside, link))
  testthat::expect_error(d_t01_invoke("triomics_d_resource_access", list(
    resource, "gene_reaction", "2026.1", content_hash, file.path(link, "cache"), "offline"
  )), class = "triomics_error")
  miss_resource <- resource
  miss_resource$resource_id <- "fixture-path-map-second-operation"
  testthat::expect_error(d_t01_invoke("triomics_d_resource_access", list(
    miss_resource, "gene_reaction", "2026.1", content_hash, file.path(link, "cache"), "offline"
  )), class = "triomics_error")
  testthat::expect_error(d_t01_invoke("triomics_d_resource_access", list(
    resource, "gene_reaction", "2026.1", content_hash,
    file.path(allowed, "..", "outside", "traversal-cache"), "offline"
  )), class = "triomics_error")

  handoff_root <- file.path(root, "handoff")
  testthat::expect_error(d_t01_invoke("triomics_d_secure_path", list(
    file.path(link, "written"), "D_OUTPUT_PATH_UNSAFE", "unsafe path", "output_root", TRUE
  )), class = "triomics_error")
  testthat::expect_false(file.exists(file.path(outside, "written", "D", "evidence-routing.json")))
  testthat::expect_error(d_t01_invoke("triomics_d_secure_path", list(
    file.path(allowed, "..", "outside", "traversal-output"),
    "D_OUTPUT_PATH_UNSAFE", "unsafe path", "output_root", TRUE
  )), class = "triomics_error")
})

testthat::test_that("D-T01 separates tested results from unique measured features across contrasts", {
  input <- d_t01_input()
  repeated_family <- list(
    family_id = "family_repeat", member_ids = list(d_t01_key("gene_a")), n_declared = 1L,
    dispositions = list(estimated = 1L), member_hash = d_t01_hash(d_t01_key("gene_a"))
  )
  repeated_row <- input$b$block_results$blocks$proteome$contrasts$contrast_1$rows[[1]]
  repeated_row$result_id <- "result_gene_a_contrast_2"
  input$b$block_results$blocks$proteome$contrasts$contrast_2 <- list(
    family = repeated_family, rows = list(repeated_row)
  )
  input$b$family_ledger$families[[3]] <- list(
    block_id = "proteome", claim_id = "contrast_2", family = repeated_family
  )
  bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(input))
  testthat::expect_identical(bundle$accounting$tested_result_count, 4L)
  testthat::expect_identical(bundle$accounting$measured_row_count, 3L)
  testthat::expect_length(bundle$tested_claims, 3L)
})

testthat::test_that("D-T01 adapts the shared A feature registry without inventing route metadata", {
  input <- d_t01_input()
  bundle <- tryCatch(d_t01_invoke("triomics_d_build_evidence_bundle", list(input)),
                     error = function(error) error)
  testthat::expect_false(inherits(bundle, "error"))
  if (inherits(bundle, "error")) return(invisible(NULL))
  feature_key <- triomics_feature_key(input$study_id, "proteome", "protein-row-a")
  feature <- bundle$feature_registry[[feature_key]]
  testthat::expect_identical(feature$study_id, input$study_id)
  testthat::expect_identical(feature$block_id, "proteome")
  testthat::expect_identical(feature$source_row, "protein-row-a")
  testthat::expect_identical(feature$taxon_status, "declared")
  testthat::expect_true("feature_registry_row" %in% names(feature$provenance))

  unknown <- input
  unknown$routing_metadata <- NULL
  unknown_bundle <- d_t01_invoke("triomics_d_build_evidence_bundle", list(unknown))
  unknown_feature <- unknown_bundle$feature_registry[[feature_key]]
  testthat::expect_identical(unknown_feature$taxon_status, "unknown")
  testthat::expect_identical(unknown_feature$capability_status, "unknown")
  testthat::expect_identical(unknown_feature$resolution_status, "unknown")
  routed_unknown <- d_t01_invoke("triomics_d_route_evidence", list(unknown_bundle, d_t01_routes(TRUE)))
  testthat::expect_identical(routed_unknown$routes$gene_reaction$candidate_count, 0L)
  testthat::expect_true(any(vapply(routed_unknown$incompatible_nominations, function(item) {
    identical(item$reason_code, "taxon_unknown")
  }, logical(1))))

  mixed <- input
  mixed$feature_registry$study_id[[1]] <- "another-study"
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(mixed)),
                         class = "triomics_error")
  invalid_key <- input
  invalid_key$feature_registry$feature_key[[1]] <- "gene_a"
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(invalid_key)),
                         class = "triomics_error")

  malformed_metadata <- input
  malformed_metadata$routing_metadata[[feature_key]]$taxon$status <- c("declared", "unknown")
  testthat::expect_error(d_t01_invoke("triomics_d_build_evidence_bundle", list(malformed_metadata)),
                         class = "triomics_error")
})

d_t01_cli_run <- function(config_path, stage, output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", shQuote(stage), "--output", shQuote(output_root)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = output)
}

testthat::test_that("D-T01 refuses stale paired signs and preserves the old pointer", {
  roots_path <- "/tmp/triomics-m7-017-luna/native-roots.txt"
  testthat::expect_true(file.exists(roots_path))
  if (!file.exists(roots_path)) return(invisible(NULL))
  roots <- utils::read.delim(roots_path, header = FALSE, sep = "\t",
                             stringsAsFactors = FALSE, quote = "")
  paired <- roots[roots[[1L]] == "paired", , drop = FALSE]
  testthat::expect_gt(nrow(paired), 0L)
  if (!nrow(paired)) return(invisible(NULL))
  fixture_root <- paired[[2L]][[nrow(paired)]]
  source_root <- paired[[3L]][[nrow(paired)]]
  source_pointer <- file.path(source_root, "registries", "current-D.json")
  testthat::expect_true(file.exists(source_pointer))
  if (!file.exists(source_pointer)) return(invisible(NULL))

  clone_root <- tempfile("d-t01-paired-sign-stale-", tmpdir = "/private/tmp")
  dir.create(clone_root, recursive = TRUE, showWarnings = FALSE)
  testthat::expect_true(all(file.copy(
    list.files(source_root, all.files = TRUE, no.. = TRUE, full.names = TRUE),
    clone_root, recursive = TRUE
  )))
  on.exit(unlink(clone_root, recursive = TRUE, force = TRUE), add = TRUE)
  pointer_path <- file.path(clone_root, "registries", "current-D.json")
  pointer_hash <- triomics_sha256(pointer_path)

  config_path <- file.path(fixture_root, "tickets", "D-T01.yaml")
  config_text <- paste(readLines(config_path, warn = FALSE), collapse = "\n")
  reversed <- sub('"reference_level": "baseline"',
                  '"reference_level": "followup"', config_text, fixed = TRUE)
  reversed <- sub('"target_level": "followup"',
                  '"target_level": "baseline"', reversed, fixed = TRUE)
  reversed <- sub('"reference_visit": "baseline"',
                  '"reference_visit": "followup"', reversed, fixed = TRUE)
  reversed <- sub('"target_visit": "followup"',
                  '"target_visit": "baseline"', reversed, fixed = TRUE)
  stale_config <- tempfile("d-t01-paired-sign-stale-", tmpdir = dirname(config_path),
                           fileext = ".yaml")
  on.exit(unlink(stale_config, force = TRUE), add = TRUE)
  writeLines(reversed, stale_config, useBytes = TRUE)

  stale_run <- d_t01_cli_run(stale_config, "D", clone_root)
  testthat::expect_false(identical(stale_run$status, 0L),
                         info = paste(stale_run$output, collapse = "\n"))
  testthat::expect_true(any(grepl("D_LINEAGE_MISMATCH", stale_run$output, fixed = TRUE)),
                        info = paste(stale_run$output, collapse = "\n"))
  testthat::expect_identical(triomics_sha256(pointer_path), pointer_hash)
})

testthat::test_that("D-T01 publishes a complete generation from actual native B and C CLI handoffs", {
  config <- triomics_test_resource("examples", "tickets", "D-T01.yaml")
  output_root <- tempfile("d-t01-native-cli-", tmpdir = "/private/tmp")
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
  clone_roots <- character()
  on.exit(unlink(c(output_root, clone_roots), recursive = TRUE, force = TRUE), add = TRUE)

  clone_native_root <- function() {
    destination <- tempfile("d-t01-native-tamper-", tmpdir = "/private/tmp")
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    contents <- list.files(output_root, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    copied <- file.copy(contents, destination, recursive = TRUE)
    if (!all(copied)) stop("Could not copy the synthetic native handoff root for an adversarial probe.")
    clone_roots <<- c(clone_roots, destination)
    normalizePath(destination, winslash = "/", mustWork = TRUE)
  }
  write_raw <- function(path, value) {
    connection <- file(path, open = "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(charToRaw(enc2utf8(value)), connection)
    invisible(path)
  }
  expect_d_code <- function(thunk, code) {
    outcome <- tryCatch(thunk(), error = identity)
    testthat::expect_true(inherits(outcome, "triomics_error"),
                          info = paste("Expected D error", code))
    if (inherits(outcome, "triomics_error")) {
      testthat::expect_identical(outcome$code, code)
    }
    invisible(outcome)
  }

  b <- d_t01_cli_run(config, "B", output_root)
  testthat::expect_identical(b$status, 0L, info = paste(b$output, collapse = "\n"))
  if (b$status != 0L) return(invisible(NULL))
  c <- d_t01_cli_run(config, "C", output_root)
  testthat::expect_identical(c$status, 0L, info = paste(c$output, collapse = "\n"))
  if (c$status != 0L) return(invisible(NULL))

  config_value <- triomics_read_config(config)
  analysis_id <- as.character(config_value$analyses[[1L]]$analysis_id)
  ingested <- triomics_ingest_configured_study(config_value, config)
  config_sha256 <- as.character(attr(config_value, "config_sha256"))
  current_metadata <- list(
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels
  )
  read_native <- function(root = output_root,
                          source_manifests = ingested$source_manifests,
                          feature_registry = ingested$feature_registry,
                          metadata = current_metadata,
                          config_digest = config_sha256) {
    triomics_d_read_evidence_bundle(
      root, as.character(config_value$study_id), analysis_id,
      source_manifests, feature_registry, imported_statistics = list(),
      routing_metadata = NULL, current_config_sha256 = config_digest,
      current_metadata = metadata
    )
  }

  wrong_sources <- ingested$source_manifests
  wrong_sources[[1L]]$sha256 <- d_t01_hash("altered-current-source-bytes")
  expect_d_code(function() read_native(source_manifests = wrong_sources), "D_LINEAGE_MISMATCH")

  wrong_registry <- ingested$feature_registry
  wrong_registry$quantity[[1L]] <- paste0(wrong_registry$quantity[[1L]], "_tampered")
  expect_d_code(function() read_native(feature_registry = wrong_registry), "D_LINEAGE_MISMATCH")

  wrong_metadata <- current_metadata
  wrong_metadata$sample_metadata <- list(tampered = TRUE)
  expect_d_code(function() read_native(metadata = wrong_metadata), "D_LINEAGE_MISMATCH")
  for (field in c("sample_id", "subject_id", "visit_id", "sample_role")) {
    changed_map <- current_metadata
    changed_map$sample_registry[[field]][[1L]] <- paste0("changed-", field)
    expect_d_code(function() read_native(metadata = changed_map), "D_LINEAGE_MISMATCH")
  }
  expect_d_code(function() read_native(config_digest = d_t01_hash("different-current-config")),
                 "D_LINEAGE_MISMATCH")

  missing_profile_root <- clone_native_root()
  b_profile_path <- list.files(file.path(missing_profile_root, "B", "handoffs"),
                               pattern = "^stage-handoff-provenance[.]json$",
                               recursive = TRUE, full.names = TRUE)[[1L]]
  unlink(b_profile_path)
  expect_d_code(function() read_native(root = missing_profile_root), "D_PROVENANCE_REQUIRED")

  duplicate_profile_root <- clone_native_root()
  duplicate_profile_path <- list.files(file.path(duplicate_profile_root, "B", "handoffs"),
                                      pattern = "^stage-handoff-provenance[.]json$",
                                      recursive = TRUE, full.names = TRUE)[[1L]]
  duplicate_profile_envelope <- sub("[.]json$", ".envelope.json", duplicate_profile_path)
  duplicate_profile_raw <- readChar(duplicate_profile_path,
                                    nchars = file.info(duplicate_profile_path)$size,
                                    useBytes = TRUE)
  duplicate_needle <- '"profile_version": "1.0"'
  testthat::expect_true(grepl(duplicate_needle, duplicate_profile_raw, fixed = TRUE))
  duplicate_profile_raw <- sub(duplicate_needle,
                               paste(duplicate_needle, duplicate_needle, sep = ",\n  "),
                               duplicate_profile_raw, fixed = TRUE)
  write_raw(duplicate_profile_path, duplicate_profile_raw)
  duplicate_envelope <- jsonlite::read_json(duplicate_profile_envelope, simplifyVector = FALSE)
  duplicate_envelope$payload_sha256 <- triomics_sha256(duplicate_profile_path)
  write_raw(duplicate_profile_envelope, triomics_b07_prov_canonical_value(duplicate_envelope))
  testthat::expect_true(triomics_validate_envelope(duplicate_profile_envelope))
  expect_d_code(function() read_native(root = duplicate_profile_root), "D_PROVENANCE_INVALID")

  reordered_envelope_root <- clone_native_root()
  predecessor_envelope_path <- list.files(file.path(reordered_envelope_root, "B", "handoffs"),
                                          pattern = "^bundle[.]envelope[.]json$",
                                          recursive = TRUE, full.names = TRUE)[[1L]]
  predecessor_original <- readChar(predecessor_envelope_path,
                                   nchars = file.info(predecessor_envelope_path)$size,
                                   useBytes = TRUE)
  predecessor_value <- jsonlite::fromJSON(predecessor_original, simplifyVector = FALSE)
  predecessor_reordered <- as.character(jsonlite::toJSON(
    predecessor_value[rev(names(predecessor_value))], auto_unbox = TRUE,
    null = "null", pretty = TRUE, digits = NA
  ))
  write_raw(predecessor_envelope_path, predecessor_reordered)
  testthat::expect_false(identical(
    digest::digest(predecessor_original, algo = "sha256", serialize = FALSE),
    digest::digest(predecessor_reordered, algo = "sha256", serialize = FALSE)
  ))
  testthat::expect_true(triomics_validate_envelope(predecessor_envelope_path))
  expect_d_code(function() read_native(root = reordered_envelope_root), "D_PROVENANCE_INVALID")

  native_ancestor_root <- clone_native_root()
  native_directory <- file.path(native_ancestor_root, "C", "native-states")
  native_external <- tempfile("d-t01-native-external-", tmpdir = "/private/tmp")
  on.exit(unlink(native_external, recursive = TRUE, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(native_directory, native_external))
  testthat::expect_true(file.symlink(native_external, native_directory))
  expect_d_code(function() read_native(root = native_ancestor_root), "D_OUTPUT_PATH_UNSAFE")
  expect_d_code(function() triomics_execute_stage_d(config, native_ancestor_root),
                 "D_OUTPUT_PATH_UNSAFE")

  native_parent_root <- clone_native_root()
  c_directory <- file.path(native_parent_root, "C")
  c_external <- tempfile("d-t01-c-parent-", tmpdir = "/private/tmp")
  on.exit(unlink(c_external, recursive = TRUE, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(c_directory, c_external))
  testthat::expect_true(file.symlink(c_external, c_directory))
  expect_d_code(function() read_native(root = native_parent_root), "D_OUTPUT_PATH_UNSAFE")

  native_envelope_root <- clone_native_root()
  native_envelope <- list.files(file.path(native_envelope_root, "C", "native-states"),
                                pattern = "[.]envelope[.]json$", full.names = TRUE)[[1L]]
  envelope_external <- tempfile("d-t01-native-envelope-", tmpdir = "/private/tmp")
  on.exit(unlink(envelope_external, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(native_envelope, envelope_external))
  testthat::expect_true(file.symlink(envelope_external, native_envelope))
  expect_d_code(function() read_native(root = native_envelope_root), "D_OUTPUT_PATH_UNSAFE")

  native_payload_root <- clone_native_root()
  native_payload <- list.files(file.path(native_payload_root, "C", "native-states"),
                               pattern = "[.]json$", full.names = TRUE)
  native_payload <- native_payload[!grepl("[.]envelope[.]json$", native_payload)]
  native_payload <- native_payload[[1L]]
  payload_external <- tempfile("d-t01-native-payload-", tmpdir = "/private/tmp")
  on.exit(unlink(payload_external, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(native_payload, payload_external))
  testthat::expect_true(file.symlink(payload_external, native_payload))
  expect_d_code(function() read_native(root = native_payload_root), "D_OUTPUT_PATH_UNSAFE")

  dangling_native_payload_root <- clone_native_root()
  dangling_native_payload <- list.files(
    file.path(dangling_native_payload_root, "C", "native-states"),
    pattern = "[.]json$", full.names = TRUE
  )
  dangling_native_payload <- dangling_native_payload[
    !grepl("[.]envelope[.]json$", dangling_native_payload)
  ][[1L]]
  dangling_payload_target <- tempfile("d-t01-dangling-native-payload-", tmpdir = "/private/tmp")
  testthat::expect_true(unlink(dangling_native_payload) == 0L)
  testthat::expect_true(file.symlink(dangling_payload_target, dangling_native_payload))
  expect_d_code(function() read_native(root = dangling_native_payload_root), "D_OUTPUT_PATH_UNSAFE")

  dangling_native_envelope_root <- clone_native_root()
  dangling_native_envelope <- list.files(
    file.path(dangling_native_envelope_root, "C", "native-states"),
    pattern = "[.]envelope[.]json$", full.names = TRUE
  )[[1L]]
  dangling_envelope_target <- tempfile("d-t01-dangling-native-envelope-", tmpdir = "/private/tmp")
  testthat::expect_true(unlink(dangling_native_envelope) == 0L)
  testthat::expect_true(file.symlink(dangling_envelope_target, dangling_native_envelope))
  expect_d_code(function() read_native(root = dangling_native_envelope_root), "D_OUTPUT_PATH_UNSAFE")

  dangling_native_root <- clone_native_root()
  dangling_native_dir <- file.path(dangling_native_root, "C", "native-states")
  dangling_native_target <- tempfile("d-t01-missing-native-target-", tmpdir = "/private/tmp")
  testthat::expect_true(unlink(dangling_native_dir, recursive = TRUE) == 0L)
  testthat::expect_true(file.symlink(dangling_native_target, dangling_native_dir))
  expect_d_code(function() read_native(root = dangling_native_root), "D_OUTPUT_PATH_UNSAFE")

  matrix_mutation_root <- clone_native_root()
  matrix_values_path <- list.files(file.path(matrix_mutation_root, "B", "handoffs"),
                                   pattern = "^stage-c-matrix-values[.]rds$",
                                   recursive = TRUE, full.names = TRUE)[[1L]]
  persisted_matrices <- readRDS(matrix_values_path)
  first_matrix <- names(persisted_matrices)[[1L]]
  persisted_matrices[[first_matrix]][[1L, 1L]] <- persisted_matrices[[first_matrix]][[1L, 1L]] + 0.25
  saveRDS(persisted_matrices, matrix_values_path, version = 3)
  expect_d_code(function() read_native(root = matrix_mutation_root), "D_PROVENANCE_INVALID")

  preparation_state_root <- clone_native_root()
  preparation_state <- list.files(file.path(preparation_state_root, "C", "preparation-states"),
                                  pattern = "[.]json$", full.names = TRUE)[[1L]]
  preparation_state_value <- jsonlite::read_json(preparation_state, simplifyVector = FALSE)
  preparation_state_value$training_unit_ids[[1L]] <- "changed-unit"
  write_raw(preparation_state, triomics_b07_prov_canonical_value(preparation_state_value))
  expect_d_code(function() read_native(root = preparation_state_root), "D_PROVENANCE_INVALID")

  unsafe_root <- tempfile("d-t01-native-symlink-", tmpdir = "/private/tmp")
  on.exit(unlink(unsafe_root), add = TRUE)
  testthat::expect_true(file.symlink(output_root, unsafe_root))
  expect_d_code(function() triomics_execute_stage_d(config, unsafe_root), "D_OUTPUT_PATH_UNSAFE")

  source_paths <- c(
    list.files(file.path(output_root, "B"), recursive = TRUE, full.names = TRUE, all.files = TRUE),
    list.files(file.path(output_root, "C"), recursive = TRUE, full.names = TRUE, all.files = TRUE),
    list.files(file.path(output_root, "D"), recursive = TRUE, full.names = TRUE, all.files = TRUE)
  )
  source_paths <- source_paths[!file.info(source_paths)$isdir &
                               !grepl("/D/generations/", source_paths, fixed = TRUE)]
  source_names <- substring(source_paths, nchar(output_root) + 2L)
  source_hashes_before <- stats::setNames(vapply(source_paths, triomics_sha256, character(1)), source_names)
  run_registry_paths <- file.path(output_root, "registries", c("run.json", "run-C.json"))
  run_registry_hashes_before <- vapply(run_registry_paths, triomics_sha256, character(1))

  d <- d_t01_cli_run(config, "D", output_root)
  testthat::expect_identical(d$status, 0L, info = paste(d$output, collapse = "\n"))
  if (d$status != 0L) return(invisible(NULL))

  pointer_path <- file.path(output_root, "registries", "current-D.json")
  testthat::expect_true(file.exists(pointer_path))
  if (!file.exists(pointer_path)) return(invisible(NULL))
  pointer <- jsonlite::read_json(pointer_path, simplifyVector = FALSE)
  testthat::expect_setequal(names(pointer),
                            c("schema_version", "study_id", "run_id", "generation_id",
                              "generation_manifest"))
  testthat::expect_setequal(names(pointer$generation_manifest),
                            c("path", "size_bytes", "digest_algorithm", "digest"))
  generation <- file.path(output_root, "D", "generations", pointer$generation_id)
  manifest_path <- file.path(generation, "generation-manifest.json")
  testthat::expect_match(pointer$generation_id, "^[0-9a-f]{64}$")
  testthat::expect_identical(triomics_sha256(manifest_path), pointer$generation_id)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  testthat::expect_setequal(names(manifest),
                            c("profile", "profile_version", "study_id", "run_id", "provenance",
                              "producer", "status", "limitations", "members"))
  testthat::expect_identical(manifest$profile, "triomics.output-generation-manifest")
  testthat::expect_identical(manifest$profile_version, "1.0")
  testthat::expect_identical(manifest$status, "complete")
  testthat::expect_identical(manifest$run_id, "d-t01")
  testthat::expect_length(manifest$members, 2L)
  generation_inventory <- triomics_d_generation_inventory(generation)
  testthat::expect_identical(generation_inventory$files,
                             sort(c("evidence-routing.envelope.json", "evidence-routing.json",
                                    "generation-manifest.json"), method = "radix"))
  testthat::expect_length(generation_inventory$directories, 0L)
  for (member in manifest$members) {
    fields <- c("path", "scientific_role", "artifact_type", "media_type", "status",
                "size_bytes", "digest_algorithm", "digest")
    if ("envelope_for_artifact_id" %in% names(member)) {
      fields <- c(fields, "envelope_for_artifact_id")
    }
    testthat::expect_setequal(names(member), fields)
    member_path <- file.path(generation, member$path)
    testthat::expect_true(file.exists(member_path))
    if (!file.exists(member_path)) next
    testthat::expect_identical(as.numeric(file.info(member_path)$size), as.numeric(member$size_bytes))
    testthat::expect_identical(triomics_sha256(member_path), member$digest)
  }
  testthat::expect_identical(pointer$generation_manifest$path,
                             file.path("D", "generations", pointer$generation_id,
                                       "generation-manifest.json"))
  testthat::expect_identical(pointer$generation_manifest$digest_algorithm, "sha256")
  testthat::expect_identical(pointer$generation_manifest$digest, pointer$generation_id)
  testthat::expect_identical(as.numeric(pointer$generation_manifest$size_bytes),
                             as.numeric(file.info(manifest_path)$size))
  testthat::expect_true(all(c("C-06 remains unaccepted.", "C-07 controls remain not_run.",
                              "C-09 remains unqualified and conditional.") %in%
                            unlist(manifest$limitations, use.names = FALSE)))
  routing_path <- file.path(generation, "evidence-routing.json")
  routing <- jsonlite::read_json(routing_path, simplifyVector = FALSE)
  testthat::expect_setequal(names(routing$routes), c("gene_reaction", "lipid_ontology", "ora", "mofa_native", "biopan"))
  testthat::expect_true(all(vapply(routing$routes, function(route) identical(route$status, "not_requested"), logical(1))))
  testthat::expect_true(all(vapply(routing$inherited_evidence$evaluation_evidence$controls,
                                   function(control) identical(control$status, "not_run"), logical(1))))
  testthat::expect_identical(routing$inherited_evidence$stage_status$clinical_prediction$completion_policy,
                             "conditional")
  testthat::expect_true(is.list(routing$inherited_evidence$integration_evidence))
  testthat::expect_true(is.list(routing$inherited_evidence$b$block_results))

  native_evidence <- read_native()
  profileless_bundle <- list(
    artifact_type = "evidence_bundle", study_id = native_evidence$study_id,
    analysis_id = native_evidence$analysis_id,
    immutable_inputs = native_evidence$immutable_inputs
  )
  expect_d_code(function() triomics_annotate_evidence(profileless_bundle),
                 "D_PROVENANCE_REQUIRED")
  changed_in_memory <- native_evidence
  changed_in_memory$source_manifests[[1L]]$sha256 <- d_t01_hash("changed-after-native-read")
  expect_d_code(function() triomics_annotate_evidence(changed_in_memory),
                 "D_PROVENANCE_INVALID")
  expected_inputs <- triomics_d_output_input_pairs(native_evidence$immutable_inputs)
  route_envelope <- jsonlite::read_json(file.path(generation, "evidence-routing.envelope.json"),
                                       simplifyVector = FALSE)
  testthat::expect_identical(route_envelope$inputs, expected_inputs)
  testthat::expect_identical(manifest$provenance$b$bundle$artifact_id,
                             native_evidence$immutable_inputs$b_bundle_artifact_id)
  testthat::expect_identical(manifest$provenance$b$bundle$payload_sha256,
                             native_evidence$immutable_inputs$b_bundle_sha256)
  testthat::expect_identical(manifest$provenance$b$bundle$envelope_sha256,
                             native_evidence$immutable_inputs$b_bundle_envelope_sha256)
  testthat::expect_setequal(names(manifest$provenance$b$members),
                            c("block_results", "family_ledger", "candidate_nominations"))
  testthat::expect_setequal(names(manifest$provenance$c$members),
                            c("candidate_nominations", "integration_evidence", "integration_comparisons",
                              "evaluation_evidence", "stage_status"))

  semantic_manifest_root <- clone_native_root()
  semantic_pointer <- triomics_d_validate_current_pointer(
    semantic_manifest_root, file.path(semantic_manifest_root, "registries", "current-D.json")
  )
  semantic_generation <- semantic_pointer$generation$paths$generation
  semantic_manifest_path <- file.path(semantic_generation, "generation-manifest.json")
  semantic_manifest <- jsonlite::read_json(semantic_manifest_path, simplifyVector = FALSE)
  payload_descriptor <- which(vapply(semantic_manifest$members, function(member) {
    identical(member$path, "evidence-routing.json")
  }, logical(1)))[[1L]]
  semantic_manifest$members[[payload_descriptor]]$artifact_type <- "wrong_type"
  semantic_manifest$members[[payload_descriptor]]$scientific_role <- "wrong_role"
  semantic_manifest$provenance$b$profile$payload_sha256 <- paste(rep("0", 64L), collapse = "")
  write_raw(semantic_manifest_path, triomics_b07_prov_canonical_value(semantic_manifest))
  semantic_generation_id <- triomics_sha256(semantic_manifest_path)
  testthat::expect_true(file.rename(
    semantic_generation,
    file.path(dirname(semantic_generation), semantic_generation_id)
  ))
  expect_d_code(function() triomics_d_validate_generation(semantic_manifest_root,
                                                           semantic_generation_id),
                 "D_PROVENANCE_INVALID")

  semantic_envelope_root <- clone_native_root()
  semantic_envelope_pointer <- triomics_d_validate_current_pointer(
    semantic_envelope_root, file.path(semantic_envelope_root, "registries", "current-D.json")
  )
  semantic_envelope_generation <- semantic_envelope_pointer$generation$paths$generation
  semantic_envelope_path <- file.path(semantic_envelope_generation,
                                      "evidence-routing.envelope.json")
  semantic_envelope <- jsonlite::read_json(semantic_envelope_path, simplifyVector = FALSE)
  semantic_envelope$inputs[[1L]]$sha256 <- paste(rep("0", 64L), collapse = "")
  write_raw(semantic_envelope_path, triomics_b07_prov_canonical_value(semantic_envelope))
  semantic_envelope_manifest_path <- file.path(semantic_envelope_generation,
                                               "generation-manifest.json")
  semantic_envelope_manifest <- jsonlite::read_json(semantic_envelope_manifest_path,
                                                    simplifyVector = FALSE)
  envelope_descriptor <- which(vapply(semantic_envelope_manifest$members, function(member) {
    identical(member$path, "evidence-routing.envelope.json")
  }, logical(1)))[[1L]]
  semantic_envelope_manifest$members[[envelope_descriptor]]$digest <- triomics_sha256(semantic_envelope_path)
  semantic_envelope_manifest$members[[envelope_descriptor]]$size_bytes <-
    as.numeric(file.info(semantic_envelope_path)$size)
  write_raw(semantic_envelope_manifest_path,
            triomics_b07_prov_canonical_value(semantic_envelope_manifest))
  semantic_envelope_generation_id <- triomics_sha256(semantic_envelope_manifest_path)
  testthat::expect_true(file.rename(
    semantic_envelope_generation,
    file.path(dirname(semantic_envelope_generation), semantic_envelope_generation_id)
  ))
  expect_d_code(function() triomics_d_validate_generation(semantic_envelope_root,
                                                           semantic_envelope_generation_id),
                 "D_PROVENANCE_INVALID")

  old_pointer_hash <- triomics_sha256(pointer_path)
  old_generation_files <- list.files(generation, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  old_generation_files <- old_generation_files[!file.info(old_generation_files)$isdir]
  old_generation_hashes <- stats::setNames(vapply(old_generation_files, triomics_sha256, character(1)),
                                           substring(old_generation_files, nchar(generation) + 2L))
  publisher_result <- triomics_annotate_evidence(native_evidence, config_value$annotation %||% list())
  publish_args <- list(
    result = publisher_result, output_root = output_root, config_sha256 = config_sha256,
    producer_commit = triomics_b07_git_ref(getwd()),
    privacy_class = if (identical(config_value$data_class, "synthetic")) "synthetic" else "sensitive",
    run_id = "d-t01"
  )
  expect_unchanged <- function() {
    testthat::expect_identical(triomics_sha256(pointer_path), old_pointer_hash)
    current_files <- list.files(generation, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    current_files <- current_files[!file.info(current_files)$isdir]
    current_hashes <- stats::setNames(vapply(current_files, triomics_sha256, character(1)),
                                      substring(current_files, nchar(generation) + 2L))
    testthat::expect_identical(current_hashes, old_generation_hashes)
  }
  old_failpoint <- getOption("triomics.d.publication.failpoint")
  options(triomics.d.publication.failpoint = "provenance_preflight_complete")
  preflight_failure <- tryCatch(triomics_execute_stage_d(config, output_root), error = identity)
  options(triomics.d.publication.failpoint = old_failpoint)
  testthat::expect_identical(preflight_failure$code, "D_PUBLICATION_FAILED")
  expect_unchanged()

  for (failpoint in c("temporary_generation_created", "payload_written", "envelope_written",
                      "manifest_sealed", "generation_renamed", "pointer_prepared",
                      "before_pointer_rename")) {
    options(triomics.d.publication.failpoint = failpoint)
    failure <- tryCatch(do.call(triomics_d_publish_evidence_generation, publish_args),
                        error = identity)
    options(triomics.d.publication.failpoint = old_failpoint)
    testthat::expect_identical(failure$code, "D_PUBLICATION_FAILED", info = failpoint)
    expect_unchanged()
  }
  options(triomics.d.publication.failpoint = "after_pointer_rename")
  recovered <- tryCatch(do.call(triomics_d_publish_evidence_generation, publish_args),
                        error = identity)
  options(triomics.d.publication.failpoint = old_failpoint)
  testthat::expect_false(inherits(recovered, "error"),
                         info = if (inherits(recovered, "error")) conditionMessage(recovered) else NULL)
  if (!inherits(recovered, "error")) {
    testthat::expect_identical(recovered$status, "complete")
    testthat::expect_true(isTRUE(recovered$recovered))
    testthat::expect_true(is.list(triomics_d_validate_current_pointer(output_root, pointer_path)))
  }

  generation_parent_root <- clone_native_root()
  generation_parent <- file.path(generation_parent_root, "D", "generations")
  generation_parent_external <- tempfile("d-t01-generations-external-", tmpdir = "/private/tmp")
  on.exit(unlink(generation_parent_external, recursive = TRUE, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(generation_parent, generation_parent_external))
  testthat::expect_true(file.symlink(generation_parent_external, generation_parent))
  expect_d_code(function() triomics_execute_stage_d(config, generation_parent_root),
                 "D_OUTPUT_PATH_UNSAFE")

  registry_parent_root <- clone_native_root()
  registry_parent <- file.path(registry_parent_root, "registries")
  registry_parent_external <- tempfile("d-t01-registry-external-", tmpdir = "/private/tmp")
  on.exit(unlink(registry_parent_external, recursive = TRUE, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(registry_parent, registry_parent_external))
  testthat::expect_true(file.symlink(registry_parent_external, registry_parent))
  expect_d_code(function() triomics_execute_stage_d(config, registry_parent_root),
                 "D_OUTPUT_PATH_UNSAFE")

  pointer_leaf_root <- clone_native_root()
  pointer_leaf <- file.path(pointer_leaf_root, "registries", "current-D.json")
  pointer_leaf_external <- tempfile("d-t01-pointer-external-", tmpdir = "/private/tmp")
  on.exit(unlink(pointer_leaf_external, force = TRUE), add = TRUE)
  testthat::expect_true(file.rename(pointer_leaf, pointer_leaf_external))
  testthat::expect_true(file.symlink(pointer_leaf_external, pointer_leaf))
  expect_d_code(function() triomics_execute_stage_d(config, pointer_leaf_root),
                 "D_OUTPUT_PATH_UNSAFE")

  dangling_pointer_root <- clone_native_root()
  dangling_pointer <- file.path(dangling_pointer_root, "registries", "current-D.json")
  dangling_pointer_target <- tempfile("d-t01-dangling-pointer-", tmpdir = "/private/tmp")
  testthat::expect_true(unlink(dangling_pointer) == 0L)
  testthat::expect_true(file.symlink(dangling_pointer_target, dangling_pointer))
  expect_d_code(function() triomics_execute_stage_d(config, dangling_pointer_root),
                 "D_OUTPUT_PATH_UNSAFE")

  recovery_probe_root <- clone_native_root()
  recovery_probe_script <- tempfile("d-t01-recovery-probe-", tmpdir = "/private/tmp")
  on.exit(unlink(recovery_probe_script, force = TRUE), add = TRUE)
  exec_resource <- triomics_test_resource("exec", "triomics")
  source_root <- normalizePath(file.path(dirname(exec_resource), "..", ".."),
                              winslash = "/", mustWork = TRUE)
  source_tree_available <- length(list.files(
    file.path(source_root, "R"), pattern = "[.]R$", full.names = TRUE
  )) > 0L
  package_root <- if (source_tree_available) {
    source_root
  } else {
    normalizePath(file.path(dirname(exec_resource), ".."),
                  winslash = "/", mustWork = TRUE)
  }
  writeLines(c(
    "probe_args <- commandArgs(trailingOnly=TRUE); config_path <- probe_args[[1L]]; root <- probe_args[[2L]]; package_root <- probe_args[[3L]]",
    "source_files <- sort(list.files(file.path(package_root,'R'), pattern='[.]R$', full.names=TRUE))",
    "if (length(source_files)) {",
    "  setwd(package_root); for (f in source_files) sys.source(f, .GlobalEnv)",
    "  publisher_env <- .GlobalEnv; bootstrap_kind <- 'source'",
    "} else {",
    "  package_namespace <- loadNamespace('triomics', lib.loc=dirname(package_root))",
    "  probe_bindings <- c('triomics_read_config','triomics_ingest_configured_study','triomics_d_read_evidence_bundle','triomics_annotate_evidence','triomics_sha256','triomics_d_validate_current_pointer','triomics_d_generation_paths','triomics_d_publish_evidence_generation','%||%')",
    "  for (name in probe_bindings) assign(name, get(name, envir=package_namespace, inherits=FALSE), envir=.GlobalEnv)",
    "  publisher_env <- new.env(parent=package_namespace)",
    "  publisher <- get('triomics_d_publish_evidence_generation', envir=package_namespace, inherits=FALSE)",
    "  environment(publisher) <- publisher_env",
    "  assign('triomics_d_publish_evidence_generation', publisher, envir=.GlobalEnv)",
    "  bootstrap_kind <- 'installed'",
    "}",
    "cat('probe_bootstrap=',bootstrap_kind,'\\n',sep='')",
    "config <- triomics_read_config(config_path); analysis <- config$analyses[[1L]]",
    "ingested <- triomics_ingest_configured_study(config, config_path)",
    "metadata <- list(sample_metadata=ingested$sample_metadata, sample_registry=ingested$sample_registry, declared_factor_levels=ingested$declared_factor_levels)",
    "evidence <- triomics_d_read_evidence_bundle(root, as.character(config$study_id), as.character(analysis$analysis_id), ingested$source_manifests, ingested$feature_registry, current_config_sha256=as.character(attr(config,'config_sha256')), current_metadata=metadata)",
    "routed <- triomics_annotate_evidence(evidence, config$annotation %||% list())",
    "args <- list(result=routed, output_root=root, config_sha256=as.character(attr(config,'config_sha256')), producer_commit='repair1-readback-probe', privacy_class='synthetic', run_id='d-t01')",
    "original_validate <- get('triomics_d_validate_current_pointer', envir=publisher_env); calls <- 0L",
    "replacement_validate <- function(root, pointer_path) { calls <<- calls + 1L; if (calls == 2L) return(NULL); original_validate(root, pointer_path) }",
    "assign('triomics_d_validate_current_pointer', replacement_validate, envir=publisher_env)",
    "options(triomics.d.publication.failpoint='after_pointer_rename')",
    "ambiguous <- tryCatch(do.call(triomics_d_publish_evidence_generation, args), error=identity)",
    "options(triomics.d.publication.failpoint=NULL); assign('triomics_d_validate_current_pointer', original_validate, envir=publisher_env)",
    "cat('ambiguous_readback=', if (inherits(ambiguous,'error')) ambiguous$code else ambiguous$status, '\\n', sep='')",
    "current <- original_validate(root, file.path(root,'registries','current-D.json'))",
    "cat('current_after_ambiguous=', current$pointer$generation_id, '\\n', sep='')",
    "original_paths <- get('triomics_d_generation_paths', envir=publisher_env); current_id <- as.character(current$pointer$generation_id)",
    "replacement_paths <- function(root, generation_id) { p <- original_paths(root,generation_id); if (!identical(as.character(generation_id),current_id) && !file.exists(p$generation) && !dir.exists(p$generation)) file.symlink('/private/tmp',p$generation); p }",
    "assign('triomics_d_generation_paths', replacement_paths, envir=publisher_env)",
    "args$producer_commit <- 'repair1-planned-leaf-probe'",
    "planned <- tryCatch(do.call(triomics_d_publish_evidence_generation, args), error=identity)",
    "assign('triomics_d_generation_paths', original_paths, envir=publisher_env)",
    "cat('planned_generation_leaf=', if (inherits(planned,'error')) planned$code else planned$status, '\\n', sep='')",
    "external_temp_root <- tempfile('repair1-external-temp-', tmpdir='/private/tmp'); dir.create(external_temp_root)",
    "temp_generation_parent <- file.path(root,'D','generations','.temp-parent-link'); file.symlink(external_temp_root,temp_generation_parent)",
    "base_tempfile <- base::tempfile; tempfile_calls <- 0L",
    "replacement_tempfile <- function(pattern='',tmpdir=tempdir(),fileext='') { tempfile_calls <<- tempfile_calls + 1L; if (tempfile_calls == 1L) file.path(temp_generation_parent,'planned-generation') else base_tempfile(pattern,tmpdir,fileext) }",
    "assign('tempfile', replacement_tempfile, envir=publisher_env)",
    "args$producer_commit <- 'repair1-temp-generation-parent-probe'",
    "temp_generation_ancestry <- tryCatch(do.call(triomics_d_publish_evidence_generation, args), error=identity)",
    "assign('tempfile', base_tempfile, envir=publisher_env)",
    "cat('temporary_generation_ancestry=', if (inherits(temp_generation_ancestry,'error')) temp_generation_ancestry$code else temp_generation_ancestry$status, '\\n', sep='')",
    "temp_pointer_parent <- file.path(root,'registries','.temp-parent-link'); file.symlink(external_temp_root,temp_pointer_parent)",
    "tempfile_calls <- 0L",
    "replacement_tempfile <- function(pattern='',tmpdir=tempdir(),fileext='') { tempfile_calls <<- tempfile_calls + 1L; if (tempfile_calls == 2L) file.path(temp_pointer_parent,'planned-pointer.tmp') else base_tempfile(pattern,tmpdir,fileext) }",
    "assign('tempfile', replacement_tempfile, envir=publisher_env)",
    "args$producer_commit <- 'repair1-temp-pointer-parent-probe'",
    "temp_pointer_ancestry <- tryCatch(do.call(triomics_d_publish_evidence_generation, args), error=identity)",
    "assign('tempfile', base_tempfile, envir=publisher_env)",
    "cat('temporary_pointer_ancestry=', if (inherits(temp_pointer_ancestry,'error')) temp_pointer_ancestry$code else temp_pointer_ancestry$status, '\\n', sep='')",
    "old_current <- original_validate(root,file.path(root,'registries','current-D.json')); old_pointer_hash <- triomics_sha256(file.path(root,'registries','current-D.json')); old_id <- as.character(old_current$pointer$generation_id)",
    "original_rename <- get('file.rename', envir=publisher_env)",
    "replacement_rename <- function(from,to) { if (identical(basename(to),'current-D.json')) FALSE else original_rename(from,to) }",
    "assign('file.rename', replacement_rename, envir=publisher_env)",
    "args$producer_commit <- 'repair1-old-current-readback-probe'",
    "old_current_failure <- tryCatch(do.call(triomics_d_publish_evidence_generation, args), error=identity)",
    "assign('file.rename', original_rename, envir=publisher_env)",
    "current_after_failed_rename <- original_validate(root,file.path(root,'registries','current-D.json'))",
    "cat('failed_rename_class=', if (inherits(old_current_failure,'error')) old_current_failure$code else old_current_failure$status, '\\n', sep='')",
    "cat('old_current_preserved=', identical(as.character(current_after_failed_rename$pointer$generation_id),old_id) && identical(triomics_sha256(file.path(root,'registries','current-D.json')),old_pointer_hash), '\\n', sep='')",
    "unlink(c(temp_generation_parent,temp_pointer_parent,external_temp_root), recursive=TRUE, force=TRUE)"
  ), recovery_probe_script, useBytes = TRUE)
  recovery_probe <- system2(file.path(R.home("bin"), "Rscript"),
                            c("--vanilla", shQuote(recovery_probe_script),
                              shQuote(config), shQuote(recovery_probe_root),
                              shQuote(package_root)),
                            stdout = TRUE, stderr = TRUE)
  recovery_probe_text <- paste(recovery_probe, collapse = "\n")
  expected_bootstrap <- if (source_tree_available) "source" else "installed"
  testthat::expect_true(any(grepl(paste0("probe_bootstrap=", expected_bootstrap),
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("ambiguous_readback=D_PUBLICATION_STATUS_UNKNOWN",
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("current_after_ambiguous=", recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("planned_generation_leaf=D_OUTPUT_PATH_UNSAFE",
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("temporary_generation_ancestry=D_OUTPUT_PATH_UNSAFE",
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("temporary_pointer_ancestry=D_OUTPUT_PATH_UNSAFE",
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("failed_rename_class=D_PUBLICATION_FAILED",
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)
  testthat::expect_true(any(grepl("old_current_preserved=TRUE",
                                  recovery_probe, fixed = TRUE)),
                        info = recovery_probe_text)

  inventory_root <- clone_native_root()
  inventory_pointer <- triomics_d_validate_current_pointer(
    inventory_root, file.path(inventory_root, "registries", "current-D.json")
  )
  inventory_generation <- inventory_pointer$generation$paths$generation
  writeLines("undisclosed synthetic member", file.path(inventory_generation, "undisclosed.txt"))
  expect_d_code(function() triomics_d_validate_current_pointer(
    inventory_root, file.path(inventory_root, "registries", "current-D.json")
  ), "D_PROVENANCE_INVALID")

  final_source_hashes <- stats::setNames(vapply(source_paths, triomics_sha256, character(1)), source_names)
  testthat::expect_identical(final_source_hashes, source_hashes_before)
  testthat::expect_identical(vapply(run_registry_paths, triomics_sha256, character(1)),
                             run_registry_hashes_before)
})

testthat::test_that("D-T01 revalidates native DEqMS group sign, time maps, counts and source bytes", {
  source_config <- triomics_test_resource("examples", "tickets", "D-T01.yaml")
  fixture_root <- tempfile("d-t01-source-mutation-fixture-", tmpdir = "/private/tmp")
  output_root <- tempfile("d-t01-source-mutation-output-", tmpdir = "/private/tmp")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fixture_root, "tickets"), recursive = TRUE, showWarnings = FALSE)
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  fixture_root <- normalizePath(fixture_root, winslash = "/", mustWork = TRUE)
  output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
  on.exit(unlink(c(fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)
  testthat::expect_true(file.copy(source_config, file.path(fixture_root, "tickets")))
  data_source <- normalizePath(file.path(dirname(source_config), "..", "data"),
                               winslash = "/", mustWork = TRUE)
  testthat::expect_true(file.copy(data_source, fixture_root, recursive = TRUE))
  config <- file.path(fixture_root, "tickets", basename(source_config))

  metadata_path <- file.path(fixture_root, "data", "c04", "metadata.tsv")
  sample_metadata <- utils::read.delim(metadata_path, check.names = FALSE,
                                       stringsAsFactors = FALSE)
  sample_metadata$group <- ifelse(
    as.integer(sub("^SYN_", "", sample_metadata$sample_id)) <= 6L,
    "control", "treatment"
  )
  sample_metadata$subject <- rep(paste0("pair-", seq_len(6L)), each = 2L)
  sample_metadata$visit <- rep(c("baseline", "followup"), times = 6L)
  sample_metadata$time <- rep(c(0, 1), times = 6L)
  sample_metadata$time_origin <- rep(0, nrow(sample_metadata))
  utils::write.table(sample_metadata, metadata_path, sep = "\t", row.names = FALSE,
                     quote = FALSE)
  config_text <- paste(readLines(config, warn = FALSE), collapse = "\n")
  config_text <- sub(
    "    score: numeric\n  factor_levels: {}",
    "    score: numeric\n    group: factor\n    subject: string\n    visit: factor\n    time: numeric\n    time_origin: numeric\n  factor_levels:\n    group:\n      - control\n      - treatment\n    visit:\n      - baseline\n      - followup",
    config_text, fixed = TRUE
  )
  config_text <- sub("  subject_id: null", "  subject_id: subject", config_text, fixed = TRUE)
  config_text <- sub("      visit_column: null", "      visit_column: visit", config_text, fixed = TRUE)
  config_text <- sub("      condition: score\n      condition_type: continuous",
                     "      condition: group\n      condition_type: factor",
                     config_text, fixed = TRUE)
  config_text <- sub("      reference_level: null\n      target_level: null",
                     "      reference_level: control\n      target_level: treatment",
                     config_text, fixed = TRUE)
  config_text <- sub("weights: {intercept: 0, continuous.score: 1}",
                     "weights: {condition.control: -1, condition.treatment: 1}",
                     config_text, fixed = TRUE)
  config_text <- sub("effect_unit: per_unit_beta", "effect_unit: group_difference",
                     config_text, fixed = TRUE)
  config_text <- sub("method: limma", "method: deqms", config_text, fixed = TRUE)
  writeLines(config_text, config, useBytes = TRUE)

  config_value <- triomics_read_config(config)
  ingested <- triomics_ingest_configured_study(config_value, config)
  count_frames <- lapply(sort(unique(as.character(ingested$feature_registry$block_id)),
                            method = "radix"), function(block_id) {
    features <- as.character(ingested$feature_registry$feature_key[
      as.character(ingested$feature_registry$block_id) == block_id
    ])
    samples <- as.character(ingested$sample_metadata$sample_id)
    data.frame(
      block_id = block_id,
      feature_key = rep(features, each = length(samples)),
      sample_id = rep(samples, times = length(features)),
      stringsAsFactors = FALSE
    )
  })
  count_table <- do.call(rbind, count_frames)
  rownames(count_table) <- NULL
  count_table$psm_count <- seq_len(nrow(count_table)) + 2L
  count_table$acquisition <- "synthetic_D_T01_qualified_acquisition"
  count_table$provenance <- "synthetic public D-T01 qualified count fixture"
  count_path <- file.path(fixture_root, "qualified-counts.tsv")
  utils::write.table(count_table, count_path, sep = "\t", row.names = FALSE,
                     quote = FALSE)
  config_text <- paste(readLines(config, warn = FALSE), collapse = "\n")
  config_text <- sub(
    "count_metadata: null",
    paste0("count_metadata: ", as.character(jsonlite::toJSON(count_path, auto_unbox = TRUE))),
    config_text, fixed = TRUE
  )
  writeLines(config_text, config, useBytes = TRUE)
  config_value <- triomics_read_config(config)
  ingested <- triomics_ingest_configured_study(config_value, config)

  b <- d_t01_cli_run(config, "B", output_root)
  testthat::expect_identical(b$status, 0L, info = paste(b$output, collapse = "\n"))
  if (b$status != 0L) return(invisible(NULL))
  c <- d_t01_cli_run(config, "C", output_root)
  testthat::expect_identical(c$status, 0L, info = paste(c$output, collapse = "\n"))
  if (c$status != 0L) return(invisible(NULL))
  d <- d_t01_cli_run(config, "D", output_root)
  testthat::expect_identical(d$status, 0L, info = paste(d$output, collapse = "\n"))
  if (d$status != 0L) return(invisible(NULL))

  block_results_path <- file.path(output_root, "B", "block-results.json")
  block_results <- jsonlite::read_json(block_results_path, simplifyVector = FALSE)
  testthat::expect_identical(block_results$method, "DEqMS")
  testthat::expect_identical(block_results$deqms_version, "1.30.0")
  testthat::expect_identical(block_results$status, "complete")
  first_block <- names(block_results$blocks)[[1L]]
  first_claim <- names(block_results$blocks[[first_block]]$contrasts)[[1L]]
  first_count_prior <- block_results$blocks[[first_block]]$contrasts[[first_claim]]$rows[[1L]]$count_prior_ref
  testthat::expect_identical(first_count_prior$method, "DEqMS")
  testthat::expect_identical(first_count_prior$count_type, "psm_count")
  for (field in c("count", "metadata_sha256")) {
    changed_prior <- block_results
    prior_rows <- changed_prior$blocks[[first_block]]$contrasts[[first_claim]]$rows
    if (identical(field, "count")) {
      prior_rows[[1L]]$count_prior_ref$count <- prior_rows[[1L]]$count_prior_ref$count + 1
    } else {
      prior_rows[[1L]]$count_prior_ref$metadata_sha256 <- d_t01_hash("different-count-table")
    }
    changed_prior$blocks[[first_block]]$contrasts[[first_claim]]$rows <- prior_rows
    prior_error <- tryCatch(
      triomics_d_validate_current_deqms_count_metadata(
        changed_prior, config_value$analyses[[1L]]$statistics$count_metadata
      ),
      error = identity
    )
    testthat::expect_true(inherits(prior_error, "triomics_error"))
    if (inherits(prior_error, "triomics_error")) {
      testthat::expect_identical(prior_error$code, "D_LINEAGE_MISMATCH")
    }
  }

  current_metadata <- list(
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels
  )
  read_native <- function(root = output_root) {
    triomics_d_read_evidence_bundle(
      root, as.character(config_value$study_id),
      as.character(config_value$analyses[[1L]]$analysis_id),
      ingested$source_manifests, ingested$feature_registry,
      current_config_sha256 = as.character(attr(config_value, "config_sha256")),
      current_metadata = current_metadata,
      current_count_metadata = config_value$analyses[[1L]]$statistics$count_metadata
    )
  }
  native <- tryCatch(read_native(), error = identity)
  testthat::expect_false(inherits(native, "error"),
                         info = if (inherits(native, "error")) {
                           paste(native$code, conditionMessage(native))
                         } else NULL)
  if (inherits(native, "error")) return(invisible(NULL))

  for (field in c("subject_id", "visit_id")) {
    changed_map <- current_metadata
    changed_map$sample_registry[[field]][[1L]] <- paste0("changed-", field)
    map_error <- tryCatch(triomics_d_read_evidence_bundle(
      output_root, as.character(config_value$study_id),
      as.character(config_value$analyses[[1L]]$analysis_id),
      ingested$source_manifests, ingested$feature_registry,
      current_config_sha256 = as.character(attr(config_value, "config_sha256")),
      current_metadata = changed_map,
      current_count_metadata = config_value$analyses[[1L]]$statistics$count_metadata
    ), error = identity)
    testthat::expect_true(inherits(map_error, "triomics_error"))
    if (inherits(map_error, "triomics_error")) {
      testthat::expect_identical(map_error$code, "D_LINEAGE_MISMATCH")
    }
  }
  for (field in c("time", "time_origin")) {
    changed_clock <- current_metadata
    changed_clock$sample_metadata[[field]][[1L]] <-
      changed_clock$sample_metadata[[field]][[1L]] + 1
    clock_error <- tryCatch(triomics_d_read_evidence_bundle(
      output_root, as.character(config_value$study_id),
      as.character(config_value$analyses[[1L]]$analysis_id),
      ingested$source_manifests, ingested$feature_registry,
      current_config_sha256 = as.character(attr(config_value, "config_sha256")),
      current_metadata = changed_clock,
      current_count_metadata = config_value$analyses[[1L]]$statistics$count_metadata
    ), error = identity)
    testthat::expect_true(inherits(clock_error, "triomics_error"))
    if (inherits(clock_error, "triomics_error")) {
      testthat::expect_identical(clock_error$code, "D_LINEAGE_MISMATCH")
    }
  }

  count_tamper_root <- tempfile("d-t01-deqms-count-prior-tamper-", tmpdir = "/private/tmp")
  dir.create(count_tamper_root, recursive = TRUE, showWarnings = FALSE)
  testthat::expect_true(all(file.copy(
    list.files(output_root, all.files = TRUE, no.. = TRUE, full.names = TRUE),
    count_tamper_root, recursive = TRUE
  )))
  on.exit(unlink(count_tamper_root, recursive = TRUE, force = TRUE), add = TRUE)
  count_tamper_root <- normalizePath(count_tamper_root, winslash = "/", mustWork = TRUE)
  count_tamper_path <- file.path(count_tamper_root, "B", "block-results.json")
  changed_block_results <- jsonlite::read_json(count_tamper_path, simplifyVector = FALSE)
  changed_rows <- changed_block_results$blocks[[first_block]]$contrasts[[first_claim]]$rows
  changed_rows[[1L]]$count_prior_ref$count <- changed_rows[[1L]]$count_prior_ref$count + 1
  changed_block_results$blocks[[first_block]]$contrasts[[first_claim]]$rows <- changed_rows
  writeLines(triomics_b07_prov_canonical_value(changed_block_results), count_tamper_path,
             useBytes = TRUE)
  expect_count_prior_error <- tryCatch(read_native(root = count_tamper_root), error = identity)
  testthat::expect_true(inherits(expect_count_prior_error, "triomics_error"))
  if (inherits(expect_count_prior_error, "triomics_error")) {
    testthat::expect_identical(expect_count_prior_error$code, "D_PROVENANCE_INVALID")
  }

  pointer_path <- file.path(output_root, "registries", "current-D.json")
  pointer_hash_before <- triomics_sha256(pointer_path)
  config_text <- paste(readLines(config, warn = FALSE), collapse = "\n")
  reversed_config_text <- sub(
    "weights: {condition.control: -1, condition.treatment: 1}",
    "weights: {condition.control: 1, condition.treatment: -1}",
    config_text, fixed = TRUE
  )
  reversed_config <- tempfile("d-t01-reversed-sign-", tmpdir = dirname(config),
                              fileext = ".yaml")
  on.exit(unlink(reversed_config, force = TRUE), add = TRUE)
  writeLines(reversed_config_text, reversed_config, useBytes = TRUE)
  reversed_sign <- tryCatch(triomics_execute_stage_d(reversed_config, output_root),
                            error = identity)
  testthat::expect_true(inherits(reversed_sign, "triomics_error"))
  if (inherits(reversed_sign, "triomics_error")) {
    testthat::expect_identical(reversed_sign$code, "D_LINEAGE_MISMATCH")
  }

  measurement_path <- file.path(fixture_root, "data", "independent", "proteomics.tsv")
  measurement_lines <- readLines(measurement_path, warn = FALSE)
  original_measurement_lines <- measurement_lines
  measurement_fields <- strsplit(measurement_lines[[2L]], "\t", fixed = TRUE)[[1L]]
  measurement_fields[[2L]] <- as.character(as.numeric(measurement_fields[[2L]]) + 0.25)
  measurement_lines[[2L]] <- paste(measurement_fields, collapse = "\t")
  writeLines(measurement_lines, measurement_path, useBytes = TRUE)
  changed_measurement <- tryCatch(triomics_execute_stage_d(config, output_root), error = identity)
  testthat::expect_true(inherits(changed_measurement, "triomics_error"))
  if (inherits(changed_measurement, "triomics_error")) {
    testthat::expect_identical(changed_measurement$code, "D_LINEAGE_MISMATCH")
  }
  testthat::expect_identical(triomics_sha256(pointer_path), pointer_hash_before)
  writeLines(original_measurement_lines, measurement_path, useBytes = TRUE)

  count_lines <- readLines(count_path, warn = FALSE)
  count_fields <- strsplit(count_lines[[2L]], "\t", fixed = TRUE)[[1L]]
  count_fields[[4L]] <- as.character(as.integer(count_fields[[4L]]) + 1L)
  count_lines[[2L]] <- paste(count_fields, collapse = "\t")
  writeLines(count_lines, count_path, useBytes = TRUE)
  changed_counts <- tryCatch(triomics_execute_stage_d(config, output_root), error = identity)
  testthat::expect_true(inherits(changed_counts, "triomics_error"))
  if (inherits(changed_counts, "triomics_error")) {
    testthat::expect_identical(changed_counts$code, "D_LINEAGE_MISMATCH")
  }
  testthat::expect_identical(triomics_sha256(pointer_path), pointer_hash_before)
})
