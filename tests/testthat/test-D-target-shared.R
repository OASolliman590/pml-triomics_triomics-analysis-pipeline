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

td_target_evidence <- function(duplicate_feature = FALSE, taxon_status = "declared",
                               taxon_value = "9606", routing_metadata_present = TRUE) {
  taxon <- list(status = taxon_status, value = taxon_value,
                provenance_ref = if (identical(taxon_status, "declared")) {
                  "synthetic://target-routing-metadata/feature_key_a"
                } else NULL)
  feature <- list(feature_key = "feature_key_a", study_id = "study_a", block_id = "block_metabolome",
                  original_id = "FEATURE_A", id_namespace = "synthetic_metabolite",
                  taxon_id = if (identical(taxon_status, "declared")) taxon_value else NULL,
                  taxon_status = taxon_status, source_id = "source_a", structural_resolution = "feature",
                  provenance = list(feature_registry_row = list(),
                                    routing_metadata = if (routing_metadata_present) list(taxon = taxon) else NULL))
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

testthat::test_that("B effects and C loadings require a declared matching feature taxon", {
  request <- td_target_request()
  request$mechanism_class <- "non_enzymatic"
  link <- td_target_link()
  matched <- triomics_target_resolve_observations(request, list(link), td_target_evidence())
  branches <- vapply(matched, `[[`, character(1), "source_branch")
  testthat::expect_setequal(branches, c("primary_statistics", "integration_selection"))
  testthat::expect_true(all(vapply(matched, function(observation) {
    identical(observation$taxon_applicability, "matched") &&
      identical(observation$request_taxon_id, 9606L) &&
      identical(observation$feature_taxon_id, "9606") &&
      identical(observation$feature_taxon_provenance_ref,
                "synthetic://target-routing-metadata/feature_key_a")
  }, logical(1))))

  cases <- list(
    list(status = "unknown", value = NULL, routing = FALSE, expected_id = NULL,
         applicability = "unassessable", reason = "feature_taxon_unknown"),
    list(status = "unknown", value = "9606", routing = TRUE, expected_id = NULL,
         applicability = "unassessable", reason = "feature_taxon_unknown"),
    list(status = "declared", value = "unknown", routing = TRUE, expected_id = "unknown",
         applicability = "unassessable", reason = "feature_taxon_unknown"),
    list(status = "declared", value = "9606x", routing = TRUE, expected_id = "9606x",
         applicability = "unassessable", reason = "feature_taxon_malformed"),
    list(status = "declared", value = "10090", routing = TRUE, expected_id = "10090",
         applicability = "inapplicable", reason = "feature_taxon_mismatch")
  )
  comparable <- triomics_target_profile(request, list(link), matched)
  for (case in cases) {
    evidence <- td_target_evidence(taxon_status = case$status, taxon_value = case$value,
                                   routing_metadata_present = case$routing)
    observations <- triomics_target_resolve_observations(request, list(link), evidence)
    testthat::expect_length(observations, 2L)
    testthat::expect_true(all(vapply(observations, function(observation) {
      identical(observation$taxon_applicability, case$applicability) &&
        identical(observation$taxon_reason_code, case$reason) &&
        identical(observation$request_taxon_id, 9606L) &&
        identical(observation$feature_taxon_id, case$expected_id) &&
        identical(observation$status, "uninformative") &&
        identical(observation$source_id, "source_a") &&
        identical(observation$source_sha256, paste(rep("a", 64L), collapse = "")) &&
        identical(observation$dependency_group, "molecular_feature:feature_key_a")
    }, logical(1))))
    branches <- vapply(observations, `[[`, character(1), "source_branch")
    b <- observations[[match("primary_statistics", branches)]]
    c <- observations[[match("integration_selection", branches)]]
    testthat::expect_identical(b$effect, 1.75)
    testthat::expect_identical(b$value, 1.75)
    testthat::expect_identical(c$value, 0.25)

    profile <- triomics_target_profile(request, list(link), observations)
    testthat::expect_identical(profile$evidence_axes$metabolic_multiomics_compatibility$status,
                               "unassessable")
    testthat::expect_identical(profile$evidence_axes$metabolic_multiomics_compatibility$coverage$measured_features,
                               0L)
    testthat::expect_identical(triomics_target_alternatives(list(profile, comparable))$comparability,
                               "noncomparable")

    encoded <- as.character(jsonlite::toJSON(observations, auto_unbox = TRUE,
                                             null = "null", na = "null", digits = NA))
    decoded <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)
    testthat::expect_true("feature_taxon_id" %in% names(decoded[[1L]]))
    if (is.null(case$expected_id)) testthat::expect_null(decoded[[1L]]$feature_taxon_id)
    else testthat::expect_identical(decoded[[1L]]$feature_taxon_id, case$expected_id)
    testthat::expect_true("feature_taxon_declaration" %in% names(decoded[[1L]]))
    if (case$routing) {
      testthat::expect_identical(decoded[[1L]]$feature_taxon_declaration$status, case$status)
      testthat::expect_identical(decoded[[1L]]$feature_taxon_declaration$value, case$value)
    } else {
      testthat::expect_null(decoded[[1L]]$feature_taxon_declaration)
    }
  }
})

testthat::test_that("legacy and inapplicable molecular observations cannot retain support or conflict", {
  request <- td_target_request()
  request$mechanism_class <- "non_enzymatic"
  link <- td_target_link()
  fields <- c("request_taxon_id", "feature_taxon_id", "feature_taxon_status",
              "feature_taxon_declaration", "feature_taxon_provenance_ref",
              "taxon_applicability", "taxon_reason_code")
  legacy <- triomics_target_resolve_observations(
    request, list(link),
    td_target_evidence(taxon_status = "unknown", taxon_value = NULL, routing_metadata_present = FALSE)
  )
  branches <- vapply(legacy, `[[`, character(1), "source_branch")
  legacy[[match("primary_statistics", branches)]]$status <- "support"
  legacy[[match("integration_selection", branches)]]$status <- "conflict"
  legacy <- lapply(legacy, function(observation) {
    observation[fields] <- NULL
    observation
  })
  expected_identity <- lapply(legacy, function(observation) {
    observation[c("observation_id", "value", "effect", "source_id", "dependency_group")]
  })
  legacy_profile <- triomics_target_profile(request, list(link), legacy)

  testthat::expect_true(all(vapply(legacy_profile$observations, function(observation) {
    identical(observation$status, "uninformative") &&
      identical(observation$taxon_applicability, "unassessable") &&
      identical(observation$coverage, "unassessable") &&
      all(c("feature_taxon_id", "feature_taxon_declaration", "feature_taxon_provenance_ref") %in%
            names(observation)) &&
      is.null(observation$feature_taxon_id) &&
      is.null(observation$feature_taxon_declaration) &&
      is.null(observation$feature_taxon_provenance_ref)
  }, logical(1))))
  testthat::expect_identical(lapply(legacy_profile$observations, function(observation) {
    observation[c("observation_id", "value", "effect", "source_id", "dependency_group")]
  }), expected_identity)
  testthat::expect_identical(
    legacy_profile$evidence_axes$metabolic_multiomics_compatibility$status,
    "unassessable"
  )

  mismatched <- triomics_target_resolve_observations(
    request, list(link), td_target_evidence(taxon_status = "declared", taxon_value = "10090")
  )
  branches <- vapply(mismatched, `[[`, character(1), "source_branch")
  mismatched[[match("primary_statistics", branches)]]$status <- "support"
  mismatched[[match("integration_selection", branches)]]$status <- "conflict"
  mismatched_profile <- triomics_target_profile(request, list(link), mismatched)
  testthat::expect_true(all(vapply(mismatched_profile$observations, function(observation) {
    identical(observation$status, "uninformative") &&
      identical(observation$taxon_applicability, "inapplicable") &&
      identical(observation$coverage, "inapplicable")
  }, logical(1))))
  testthat::expect_true(all(vapply(mismatched_profile$observations, function(observation) {
    !observation$status %in% c("support", "conflict")
  }, logical(1))))
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
    assay = "synthetic Kd assay fixture",
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
