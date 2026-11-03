td_target_request <- function() {
  triomics_target_request(
    list(namespace = "synthetic_protein", id = "SYNTHETIC_TARGET_A", taxon_id = 9606L,
         entity_type = "protein", constructs = list("construct_a")),
    "assess", list(list(selection_id = "declared_target_a", hypothesis_status = "exploratory",
                        selected_with_observations = TRUE)),
    context = list(context_id = "synthetic_context"),
    drug = list(namespace = "synthetic_compound", drug_id = "COMPOUND_A", label = "Synthetic compound A")
  )
}

td_target_link <- function(feature_id = "FEATURE_A") {
  list(link_id = "link_a", relation = "regulatory", sign = "positive",
       context_id = "synthetic_context", source_id = "SYNTHETIC_SOURCE_ONLY",
       feature_namespace = "synthetic_metabolite", feature_id = feature_id,
       expected_direction = "positive")
}

td_target_evidence <- function(duplicate_feature = FALSE) {
  feature <- list(feature_key = "feature_key_a", study_id = "study_a", block_id = "block_metabolome",
                  original_id = "FEATURE_A", id_namespace = "synthetic_metabolite", taxon_id = 9606L,
                  source_id = "source_a", structural_resolution = "feature")
  features <- list(feature_key_a = feature)
  if (duplicate_feature) {
    second <- feature
    second$feature_key <- "feature_key_b"
    features$feature_key_b <- second
  }
  source <- list(source_id = "source_a", sha256 = paste(rep("a", 64L), collapse = ""),
                 selection_history = "full_unselected", selection_evidence = "Synthetic fixture only.")
  b_row <- list(result_id = "result_b_a", feature_key = "feature_key_a", estimation_status = "estimated",
                effect = 1.75, effect_unit = "synthetic_beta", contrast_id = "score_slope")
  b_tested <- list(result_b_a = list(block_id = "block_metabolome", claim_id = "score_slope", row = b_row))
  c_selection <- list(
    integration_id = "integration_a", backend = "synthetic_fixture", status = "complete",
    common = list(scores = list(columns = list("component_a"))),
    selection = list(
      rule = "synthetic_fixture_selection",
      selected = list(block_metabolome = list("feature_key_a")),
      selected_values = list(block_metabolome = list(feature_key_a =
        list(component_loadings = list(component_a = 0.25))))
    )
  )
  c_nomination <- list(
    nomination_id = "nomination_c_a", source_branch = "integration_selection", feature_key = "feature_key_a",
    result_ref = list(integration_id = "integration_a", backend = "synthetic_fixture",
                      block_id = "block_metabolome", component_ids = list("component_a")),
    selection_rule = "synthetic_fixture_selection",
    selection_value = list(block_id = "block_metabolome", component_loadings = list(component_a = 0.25))
  )
  if (duplicate_feature) {
    second_row <- b_row
    second_row$result_id <- "result_b_b"
    second_row$feature_key <- "feature_key_b"
    b_tested$result_b_b <- list(block_id = "block_metabolome", claim_id = "score_slope", row = second_row)
  }
  list(
    artifact_type = "evidence_bundle", status = "complete", study_id = "study_a", analysis_id = "analysis_a",
    immutable_inputs = list(b_bundle_sha256 = paste(rep("b", 64L), collapse = ""),
                            c_manifest_sha256 = paste(rep("c", 64L), collapse = "")),
    feature_registry = features, source_by_id = list(source_a = source), tested_results = b_tested,
    nominations = list(c_nomination),
    c = list(integration_evidence = list(integrations = list(c_selection)))
  )
}

testthat::test_that("same-run B and C observations retain producer identity and share dependency", {
  request <- td_target_request()
  observed <- tryCatch(triomics_target_resolve_observations(request, list(td_target_link()),
                                                            td_target_evidence()),
                       error = function(error) NULL)

  testthat::expect_true(is.list(observed))
  testthat::expect_setequal(vapply(observed, `[[`, character(1), "source_branch"),
                            c("primary_statistics", "integration_selection"))
  branches <- vapply(observed, `[[`, character(1), "source_branch")
  b <- observed[[match("primary_statistics", branches)]]
  c <- observed[[match("integration_selection", branches)]]
  testthat::expect_identical(b$result_id, "result_b_a")
  testthat::expect_identical(b$feature_key, "feature_key_a")
  testthat::expect_identical(b$source_id, "source_a")
  testthat::expect_identical(b$effect, 1.75)
  testthat::expect_identical(c$nomination_id, "nomination_c_a")
  testthat::expect_identical(c$component_id, "component_a")
  testthat::expect_identical(b$dependency_group, c$dependency_group)
})

testthat::test_that("duplicate molecular identities remain ambiguous and unmeasured", {
  observed <- tryCatch(triomics_target_resolve_observations(
    td_target_request(), list(td_target_link()), td_target_evidence(duplicate_feature = TRUE)),
    error = function(error) NULL)

  testthat::expect_true(is.list(observed))
  testthat::expect_true(any(vapply(observed, function(x) identical(x$status, "ambiguous"), logical(1))))
  testthat::expect_false(any(vapply(observed, function(x) !is.null(x$result_id), logical(1))))
})

testthat::test_that("unknown mechanism class produces explicit unassessable canonical axes", {
  request <- td_target_request()
  request$mechanism_class <- "unknown"
  profile <- tryCatch(triomics_target_profile(request, list(), list()),
                      error = function(error) NULL)

  testthat::expect_true(is.list(profile))
  testthat::expect_identical(profile$mechanism_status, "unassessable")
  testthat::expect_identical(names(profile$evidence_axes), c(
    "metabolic_multiomics_compatibility", "functional_perturbation",
    "cellular_tissue_engagement", "direct_binding"
  ))
})

testthat::test_that("external assay axes stay separate and a sourced Kd remains external", {
  request <- td_target_request()
  record <- list(
    evidence_id = "external_kd", axis = "binding", endpoint = "Kd", units = "nM", relation = "=",
    source_id = "synthetic_assay_source", publication_group = "synthetic_publication",
    measured_by_pipeline = FALSE, value = 4.2, target_namespace = "synthetic_protein",
    target_id = "SYNTHETIC_TARGET_A", target_entity_type = "protein", taxon_id = 9606L,
    context_id = "synthetic_context", construct = "construct_a",
    compound_namespace = "synthetic_compound", compound_id = "COMPOUND_A",
    assay_limitations = "Synthetic test record only; no biological claim."
  )
  external <- triomics_target_external_evidence(request, list(record))
  profile <- tryCatch(triomics_target_profile(request, list(), list(), external_evidence = external),
                      error = function(error) NULL)

  testthat::expect_true(is.list(profile))
  testthat::expect_identical(profile$evidence_axes$direct_binding$evidence[[1L]]$endpoint, "Kd")
  testthat::expect_identical(profile$evidence_axes$direct_binding$evidence[[1L]]$value, 4.2)
  testthat::expect_identical(profile$evidence_axes$direct_binding$evidence[[1L]]$measured_by_pipeline, FALSE)
  testthat::expect_identical(profile$external_associations, list())
  testthat::expect_null(profile$selected_target)
  testthat::expect_false("binding_probability" %in% names(profile))
})

testthat::test_that("typed target JSON rejects repeated object fields before parsing", {
  config_path <- tempfile("m7-024-config-", tmpdir = "/private/tmp", fileext = ".yaml")
  input_path <- tempfile("m7-024-target-", tmpdir = "/private/tmp", fileext = ".json")
  writeLines("synthetic fixture", config_path)
  writeLines('{"records": [], "records": []}', input_path)

  error <- tryCatch(triomics_d_target_read_json_input(input_path, config_path,
                                                      "target_investigation.external_evidence_file"),
                    triomics_error = identity,
                    error = identity)
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_TARGET_INPUT_INVALID")
  unlink(c(config_path, input_path), force = TRUE)
})

testthat::test_that("mechanism JSON rejects string taxon IDs before identity coercion", {
  config_path <- tempfile("m7-024-config-", tmpdir = "/private/tmp", fileext = ".yaml")
  mechanism_path <- tempfile("m7-024-mechanism-", tmpdir = "/private/tmp", fileext = ".json")
  writeLines("synthetic fixture", config_path)
  writeLines(paste0(
    '{"format":"triomics.target-mechanism","format_version":"1.0",',
    '"protocol_ref":"protocol_a","context_ref":"context_a","drug_ref":null,',
    '"drug_identity":null,"targets":[{"identity":{"namespace":"synthetic_protein",',
    '"id":"SYNTHETIC_TARGET_A","taxon_id":"9606","entity_type":"protein",',
    '"constructs":[]},"mechanism_class":"unknown","links":[],"selection_history":[]}]}'
  ), mechanism_path)
  investigation <- list(
    enabled = TRUE, mode = "assess",
    targets = list(list(namespace = "synthetic_protein", id = "SYNTHETIC_TARGET_A",
                        taxon_id = 9606L, entity_type = "protein")),
    drug_ref = NULL, context_ref = "context_a", protocol_ref = "protocol_a",
    mechanism_file = mechanism_path, external_evidence_file = NULL, quantitative_method_ref = NULL
  )
  error <- tryCatch(triomics_d_target_slot(investigation, config_path), triomics_error = identity)
  testthat::expect_identical(error$code, "D_TARGET_IDENTITY_REQUIRED")
  unlink(c(config_path, mechanism_path), force = TRUE)
})

testthat::test_that("target results bind actual local file bytes and retain declared constructs", {
  config_path <- tempfile("m7-024-config-", tmpdir = "/private/tmp", fileext = ".yaml")
  mechanism_path <- tempfile("m7-024-mechanism-", tmpdir = "/private/tmp", fileext = ".json")
  assay_path <- tempfile("m7-024-assays-", tmpdir = "/private/tmp", fileext = ".json")
  writeLines("synthetic fixture", config_path)
  writeLines(paste0(
    '{"format":"triomics.target-mechanism","format_version":"1.0",',
    '"protocol_ref":"protocol_a","context_ref":"context_a","drug_ref":null,',
    '"drug_identity":null,"targets":[{"identity":{"namespace":"synthetic_protein",',
    '"id":"SYNTHETIC_TARGET_A","taxon_id":9606,"entity_type":"protein",',
    '"constructs":["construct_a"]},"mechanism_class":"unknown","links":[],"selection_history":[]}]}'
  ), mechanism_path)
  writeLines('{"records":[]}', assay_path)
  investigation <- list(
    enabled = TRUE, mode = "assess",
    targets = list(list(namespace = "synthetic_protein", id = "SYNTHETIC_TARGET_A",
                        taxon_id = 9606L, entity_type = "protein")),
    drug_ref = NULL, context_ref = "context_a", protocol_ref = "protocol_a",
    mechanism_file = mechanism_path, external_evidence_file = assay_path, quantitative_method_ref = NULL
  )
  slot <- triomics_d_target_slot(investigation, config_path)
  record <- slot$results[[1L]]

  testthat::expect_identical(slot$input_provenance$mechanism_file$sha256, triomics_sha256(mechanism_path))
  testthat::expect_identical(slot$input_provenance$external_evidence_file$sha256, triomics_sha256(assay_path))
  testthat::expect_identical(record$request$constructs, list("construct_a"))
  testthat::expect_identical(record$request$identity_status, "unique")
  testthat::expect_length(record$links, 0L)
  profile <- triomics_target_compose_assessment(slot, investigation)$profiles[[1L]]
  testthat::expect_identical(profile$input_provenance, slot$input_provenance)
  unlink(c(config_path, mechanism_path, assay_path), force = TRUE)
})

testthat::test_that("target request identity refuses string taxon coercion", {
  identity <- list(namespace = "synthetic_protein", id = "SYNTHETIC_TARGET_A", taxon_id = "9606",
                   entity_type = "protein", constructs = list("construct_a"))
  testthat::expect_identical(tryCatch(triomics_target_request(identity, "assess", list()),
                                      triomics_error = function(error) error$code),
                             "D_TARGET_IDENTITY_REQUIRED")
})
