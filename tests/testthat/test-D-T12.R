d12_code <- function(expr) tryCatch({ force(expr); NA_character_ }, triomics_error = function(e) e$code)

d12_sha <- function(text) paste(rep(text, 64L), collapse = "")

d12_write_json <- function(path, value) {
  encoded <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", pretty = FALSE, digits = NA))
  writeBin(charToRaw(enc2utf8(encoded)), path)
  invisible(path)
}

d12_identity <- function(id, construct = paste0("construct-", id)) {
  list(namespace = "synthetic_protein", id = id, taxon_id = 9606,
       entity_type = "protein", constructs = list(construct))
}

d12_link <- function(link_id = "path-a", feature_id = "SYN_FEATURE_001") {
  list(link_id = link_id, relation = "regulatory", sign = "positive",
       context_id = "synthetic_context", source_id = paste0("curated-path:", link_id),
       feature_namespace = "synthetic", feature_id = feature_id,
       expected_direction = "positive")
}

d12_member <- function(id, disposition = "included", reason = paste("declared", disposition),
                       feature_id = "SYN_FEATURE_001", history = list(), reference_sources = list()) {
  list(member_id = paste0("member-", id), disposition = disposition, reason = reason,
       identity = d12_identity(id), mechanism_class = "non_enzymatic",
       links = if (identical(disposition, "included")) list(d12_link("shared-path", feature_id)) else list(),
       selection_history = history, reference_sources = reference_sources)
}

d12_evidence <- function() {
  source_sha <- d12_sha("c")
  list(
    artifact_type = "evidence_bundle", study_id = "synthetic-study", analysis_id = "synthetic-analysis",
    immutable_inputs = list(b_bundle_sha256 = d12_sha("a"), c_manifest_sha256 = d12_sha("b")),
    feature_registry = list(list(
      feature_key = "feature-key-1", id_namespace = "synthetic", original_id = "SYN_FEATURE_001",
      taxon_id = "9606", taxon_status = "declared", source_id = "block-1",
      provenance = list(feature_registry_row = list(), routing_metadata = list(taxon = list(
        status = "declared", value = "9606", provenance_ref = "synthetic://target-routing-metadata/feature-key-1"
      )))
    )),
    source_by_id = list(`block-1` = list(source_id = "block-1", sha256 = source_sha,
                                        selection_history = "all_unselected",
                                        selection_evidence = "Public synthetic fixture.")),
    tested_results = list(list(block_id = "block-1", claim_id = "claim-1", row = list(
      result_id = "B-result-1", feature_key = "feature-key-1", estimation_status = "estimated",
      effect = 1.25, effect_unit = "synthetic_log_effect", contrast_id = "contrast-1", family_id = "family-1"))),
    nominations = list(), c = list(integration_evidence = list())
  )
}

d12_external_record <- function(target_id = "TARGET_A") {
  list(evidence_id = "assay-a", axis = "binding", endpoint = "IC50", units = "nM",
       relation = "=", source_id = "synthetic-assay-source", publication_group = "synthetic-publication-a",
       measured_by_pipeline = FALSE, value = 12.5, target_namespace = "synthetic_protein",
       target_id = target_id, target_entity_type = "protein", taxon_id = 9606,
       context_id = "synthetic_context", construct = paste0("construct-", target_id),
       compound_namespace = "synthetic_compound", compound_id = "SYNTHETIC_COMPOUND_A",
       assay = "synthetic assay fixture",
       assay_limitations = "Synthetic software fixture only; no biological claim.")
}

d12_case <- function(members = list(
  d12_member("TARGET_A", feature_id = "SYN_FEATURE_001"),
  d12_member("TARGET_B", feature_id = "SYN_FEATURE_001"),
  d12_member("TARGET_C", "excluded", "Outside the declared eligibility rule.")),
  records = NULL) {
  root <- tempfile("d12-discovery-")
  dir.create(root)
  universe_path <- file.path(root, "universe.json")
  protocol_path <- file.path(root, "protocol.json")
  config_path <- file.path(root, "study.yaml")
  writeLines("synthetic test configuration", config_path, useBytes = TRUE)
  universe <- list(format = "triomics.target-discovery-universe", format_version = "1.0",
                   universe_id = "synthetic-universe", version = "1.0",
                   enumeration_rule_id = "all-declared-members-v1", members = members)
  d12_write_json(universe_path, universe)
  protocol <- list(format = "triomics.target-discovery-protocol", format_version = "1.0",
                   protocol_id = "synthetic-discovery-protocol", version = "1.0",
                   context_ref = "synthetic_context", drug_ref = "synthetic_drug_request",
                   drug_identity = list(namespace = "synthetic_compound", id = "SYNTHETIC_COMPOUND_A",
                                        label = "Synthetic test compound A"),
                   universe_sha256 = triomics_sha256(universe_path),
                   enumeration_rule = list(rule_id = "all-declared-members-v1",
                                            method = "declared-disposition", ordering = "input-order",
                                            ranking = "none"))
  d12_write_json(protocol_path, protocol)
  assay_path <- NULL
  if (!is.null(records)) {
    assay_path <- file.path(root, "assays.json")
    d12_write_json(assay_path, list(format = "triomics.external-target-evidence",
                                    format_version = "1.0", records = records))
  }
  list(root = root, universe_path = universe_path, protocol_path = protocol_path,
       config_path = config_path, assay_path = assay_path,
       investigation = list(enabled = TRUE, mode = "discover", targets = list(),
                            drug_ref = "synthetic_drug_request", context_ref = "synthetic_context",
                            universe_ref = "universe.json",
                            external_evidence_file = if (is.null(assay_path)) NULL else "assays.json",
                            mechanism_file = NULL, protocol_ref = "protocol.json",
                            quantitative_method_ref = NULL, required = FALSE))
}

d12_slot <- function(case, evidence = d12_evidence()) {
  triomics_d_target_discovery_slot(case$investigation, case$config_path, evidence)
}

test_that("D-T12 enumerates and hashes every member while keeping equal profiles unranked", {
  case <- d12_case()
  result <- d12_slot(case)

  expect_identical(result$mode, "discover")
  expect_identical(result$discovery$format, "triomics.target-discovery")
  expect_identical(result$discovery$enumeration, "unranked")
  expect_identical(result$discovery$universe$sha256, triomics_sha256(case$universe_path))
  expect_identical(result$discovery$protocol$sha256, triomics_sha256(case$protocol_path))
  expect_identical(result$discovery$universe$member_count, 3L)
  expect_identical(result$discovery$universe$included_count, 2L)
  expect_identical(result$discovery$universe$excluded_count, 1L)
  expect_identical(vapply(result$discovery$members, `[[`, character(1), "inclusion_status"),
                   c("included", "included", "excluded"))
  expect_true(all(vapply(result$discovery$members, function(member) triomics_sha256_text(member$member_sha256), logical(1))))
  expect_identical(result$discovery$members[[3L]]$reason, "Outside the declared eligibility rule.")
  expect_identical(length(result$profiles), 2L)
  expect_identical(result$profiles[[1L]]$evidence_axes, result$profiles[[2L]]$evidence_axes)
  expect_null(result$selected_target)
  expect_null(result$alternatives$selected_target)
  expect_false(any(c("rank", "winner", "probability", "composite_score", "support_score") %in%
                     unlist(lapply(result$profiles, names), use.names = FALSE)))
})

test_that("Stage D routes an enabled discover request through the bounded universe", {
  case <- d12_case()
  result <- triomics_d_target_slot(case$investigation, case$config_path, d12_evidence())

  expect_identical(result$mode, "discover")
  expect_identical(result$discovery$enumeration, "unranked")
  expect_identical(length(result$discovery$members), 3L)
})

test_that("D-T12 rejects duplicate and incomplete namespaced member identities", {
  duplicate <- d12_member("TARGET_A")
  duplicate$member_id <- "member-target-b"
  expect_identical(d12_code(d12_slot(d12_case(list(d12_member("TARGET_A"), duplicate)))),
                   "D_TARGET_UNIVERSE_INVALID")

  unknown <- d12_member("TARGET_UNKNOWN")
  unknown$identity$taxon_id <- NULL
  expect_identical(d12_code(d12_slot(d12_case(list(unknown)))), "D_TARGET_IDENTITY_REQUIRED")
})

test_that("D-T12 rejects universe bytes that differ from the protocol-pinned hash", {
  case <- d12_case()
  writeBin(c(readBin(case$universe_path, "raw", n = file.info(case$universe_path)$size), as.raw(0x20)),
           case$universe_path)
  expect_identical(d12_code(d12_slot(case)), "D_TARGET_UNIVERSE_HASH_MISMATCH")
})

test_that("D-T12 preserves prior selection history and actual same-run B evidence lineage", {
  history <- list(list(selection_id = "prior-selection-a", hypothesis_status = "confirmed",
                       selected_with_observations = FALSE, selection_source = "prior-data",
                       selection_evidence_ref = "prior-study-record"))
  case <- d12_case(list(d12_member("TARGET_A", history = history)))
  result <- d12_slot(case)
  profile <- result$profiles[[1L]]
  observation <- profile$observations[[1L]]

  expect_identical(profile$discovery$prior_selection_history, history)
  expect_identical(profile$request$selection[[1L]]$recorded_hypothesis_status, "confirmed")
  expect_identical(profile$request$selection[[1L]]$hypothesis_status, "exploratory")
  expect_identical(observation$source_branch, "primary_statistics")
  expect_identical(observation$result_id, "B-result-1")
  expect_identical(observation$source_id, "block-1")
  expect_identical(observation$source_sha256, d12_sha("c"))
  expect_identical(observation$effect, 1.25)
  expect_identical(observation$effect_unit, "synthetic_log_effect")
  expect_identical(observation$feature_taxon_id, "9606")
  expect_identical(observation$feature_taxon_provenance_ref,
                   "synthetic://target-routing-metadata/feature-key-1")
  expect_identical(observation$dependency_group, "molecular_feature:feature-key-1")
})

test_that("D-T12 rejects reference signatures with exact current evaluation source identity", {
  source <- list(source_id = "block-1", sha256 = d12_sha("c"))
  case <- d12_case(list(d12_member("TARGET_A", reference_sources = list(source))))
  expect_identical(d12_code(d12_slot(case)), "D_TARGET_REFERENCE_LEAKAGE")
})

test_that("D-T12 treats renamed byte-identical sources as evaluation-derived and rejects independence flags", {
  renamed <- list(source_id = "renamed-reference", sha256 = d12_sha("c"))
  case <- d12_case(list(d12_member("TARGET_A", reference_sources = list(renamed))))
  expect_identical(d12_code(d12_slot(case)), "D_TARGET_REFERENCE_LEAKAGE")

  unsupported <- list(source_id = "reference-source", sha256 = d12_sha("d"), independent = TRUE)
  malformed <- d12_case(list(d12_member("TARGET_A", reference_sources = list(unsupported))))
  expect_identical(d12_code(d12_slot(malformed)), "D_TARGET_UNIVERSE_INVALID")
})

test_that("D-T12 marks only the overlap-unknown axis unassessable and retains valid external assay evidence", {
  unknown_source <- list(source_id = "block-1", sha256 = d12_sha("d"))
  members <- list(d12_member("TARGET_A", reference_sources = list(unknown_source)),
                  d12_member("TARGET_B"),
                  d12_member("TARGET_C", "excluded", "Outside the declared eligibility rule."))
  case <- d12_case(members, list(d12_external_record("TARGET_A")))
  result <- d12_slot(case)
  first <- result$profiles[[1L]]

  expect_identical(first$discovery$reference_overlap$status, "unknown")
  expect_identical(first$evidence_axes$metabolic_multiomics_compatibility$status, "unassessable")
  expect_identical(first$evidence_axes$direct_binding$status, "measured")
  expect_identical(first$external_evidence[[1L]]$attachment, "request")
  expect_identical(result$alternatives$comparability, "noncomparable")
  expect_null(result$selected_target)
})

test_that("D-T12 keeps an external assay without limitations unresolved", {
  record <- d12_external_record("TARGET_A")
  record$assay_limitations <- NULL
  result <- d12_slot(d12_case(records = list(record)))
  profile <- result$profiles[[1L]]

  expect_identical(profile$external_evidence[[1L]]$assay_qualification, "unresolved")
  expect_identical(profile$external_evidence[[1L]]$assay_qualification_reason,
                   "assay_limitations_missing")
  expect_identical(profile$external_evidence[[1L]]$value, 12.5)
  expect_identical(profile$evidence_axes$direct_binding$status, "unresolved")
  expect_null(result$selected_target)
})
