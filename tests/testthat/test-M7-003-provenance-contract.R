m7_003_test_digest <- paste(rep("a", 64L), collapse = "")

m7_003_test_types <- c(
  source_manifest = "triomics.source_manifest",
  normalized_metadata = "triomics.normalized_metadata",
  feature_registry = "triomics.feature_registry",
  analysis_unit_map = "triomics.analysis_unit_map",
  resolved_analysis = "triomics.resolved_analysis",
  represented_matrix = "triomics.represented_matrix",
  preprocessing_state = "triomics.preprocessing_state",
  method_state = "triomics.method_state",
  predecessor_envelope = "triomics.predecessor_envelope",
  predecessor_payload = "triomics.predecessor_payload",
  block_results = "triomics.block_results",
  family_ledger = "triomics.family_ledger",
  candidate_nominations = "triomics.candidate_nominations",
  integration_result = "triomics.integration_result",
  integration_state = "triomics.integration_state",
  evaluation_evidence = "triomics.evaluation_evidence",
  clinical_evidence = "triomics.clinical_evidence",
  native_sidecar = "triomics.native_sidecar",
  handoff_payload = "triomics.handoff_payload"
)

m7_003_test_scope <- function(role, route_id = NULL, method_id = "limma") {
  scope <- list(
    study_id = "synthetic-study",
    analysis_id = NULL,
    block_id = NULL,
    method_id = NULL,
    preparation_context_id = NULL,
    representation_id = NULL,
    route_id = NULL,
    row_identity_sha256 = NULL,
    column_identity_sha256 = NULL
  )
  if (role %in% c("normalized_metadata", "analysis_unit_map", "resolved_analysis",
                  "represented_matrix", "preprocessing_state", "method_state",
                  "predecessor_envelope", "predecessor_payload", "block_results",
                  "family_ledger", "candidate_nominations", "integration_result",
                  "integration_state", "evaluation_evidence", "clinical_evidence",
                  "native_sidecar", "handoff_payload")) {
    scope$analysis_id <- "analysis-1"
  }
  if (role %in% c("feature_registry", "represented_matrix", "preprocessing_state")) {
    scope$block_id <- "block-1"
  }
  if (role %in% c("analysis_unit_map", "resolved_analysis", "represented_matrix",
                  "preprocessing_state", "method_state", "predecessor_envelope",
                  "predecessor_payload", "block_results", "family_ledger",
                  "candidate_nominations", "integration_result", "integration_state",
                  "evaluation_evidence", "clinical_evidence", "native_sidecar",
                  "handoff_payload")) {
    scope$preparation_context_id <- "prep-1"
    scope$representation_id <- "representation-1"
  }
  if (role %in% c("resolved_analysis", "represented_matrix")) {
    scope$row_identity_sha256 <- paste(rep("b", 64L), collapse = "")
    scope$column_identity_sha256 <- paste(rep("c", 64L), collapse = "")
  } else if (role %in% c("normalized_metadata", "analysis_unit_map", "feature_registry")) {
    scope$row_identity_sha256 <- paste(rep("b", 64L), collapse = "")
    if (identical(role, "normalized_metadata")) {
      scope$column_identity_sha256 <- paste(rep("c", 64L), collapse = "")
    }
  }
  if (role %in% c("method_state", "integration_result", "integration_state", "native_sidecar")) {
    scope$method_id <- method_id
  }
  if (role %in% c("clinical_evidence", "native_sidecar")) scope$route_id <- route_id
  scope
}

m7_003_test_entity <- function(role, id, stage, generated = TRUE, method_id = "limma",
                               route_id = NULL, member_of = NULL, digest = m7_003_test_digest,
                               artifact_id = NULL, artifact_payload_sha256 = NULL,
                               derived_from = character()) {
  storage <- if (role %in% c("predecessor_envelope", "handoff_payload")) {
    "enveloped_artifact"
  } else {
    "file"
  }
  if (role == "predecessor_envelope") {
    artifact_id <- "accepted-predecessor"
    artifact_payload_sha256 <- paste(rep("d", 64L), collapse = "")
  }
  if (role == "handoff_payload") {
    artifact_id <- paste0("handoff-", tolower(stage))
    artifact_payload_sha256 <- paste(rep("e", 64L), collapse = "")
  }
  c(list(
    entity_id = id,
    entity_type = unname(m7_003_test_types[[role]]),
    scientific_role = role,
    schema_version = "1.0",
    storage_form = storage,
    media_type = if (storage == "enveloped_artifact") "application/json" else "application/octet-stream",
    digest_algorithm = "sha256",
    digest_kind = "bytes",
    digest = digest,
    size_bytes = 12L,
    canonical_schema_uri = NULL,
    artifact_id = artifact_id,
    artifact_payload_sha256 = artifact_payload_sha256,
    generated_by_activity_id = if (generated) "export-activity" else NULL,
    derived_from_entity_ids = as.list(derived_from),
    member_of_entity_id = member_of
  ), m7_003_test_scope(role, route_id = route_id, method_id = method_id))
}

m7_003_test_profile <- function(stage = "B", routes = NULL, extra_preprocessing = FALSE,
                                extra_integrations = FALSE) {
  if (is.null(routes)) routes <- list()
  if (stage == "B") {
    roles <- c("source_manifest", "normalized_metadata", "feature_registry",
               "analysis_unit_map", "resolved_analysis", "represented_matrix",
               "preprocessing_state", "method_state", "block_results", "family_ledger",
               "candidate_nominations")
  } else {
    roles <- c("source_manifest", "normalized_metadata", "feature_registry",
               "analysis_unit_map", "resolved_analysis", "represented_matrix",
               "preprocessing_state", "method_state", "candidate_nominations",
               "evaluation_evidence", "integration_result", "integration_state")
  }
  entities <- lapply(roles, function(role) {
    generated <- !identical(role, "source_manifest")
    m7_003_test_entity(role, paste0(role, "-1"), stage, generated = generated,
                       derived_from = if (generated) "source_manifest-1" else character())
  })
  if (stage == "C") {
    state_index <- which(vapply(entities, function(item) identical(item$scientific_role, "integration_state"), logical(1)))
    result_index <- which(vapply(entities, function(item) identical(item$scientific_role, "integration_result"), logical(1)))
    entities[[state_index]]$member_of_entity_id <- entities[[result_index]]$entity_id
  }
  entities[[length(entities) + 1L]] <- m7_003_test_entity(
    "predecessor_envelope", "predecessor-envelope-1", stage, generated = FALSE
  )
  entities[[length(entities) + 1L]] <- m7_003_test_entity(
    "predecessor_payload", "predecessor-payload-1", stage, generated = FALSE,
    member_of = "predecessor-envelope-1", digest = paste(rep("d", 64L), collapse = "")
  )
  if (stage == "B") {
    entities[[length(entities) + 1L]] <- m7_003_test_entity(
      "handoff_payload", "handoff-payload-1", stage,
      derived_from = "source_manifest-1"
    )
  } else {
    entities[[length(entities) + 1L]] <- m7_003_test_entity(
      "handoff_payload", "handoff-payload-1", stage,
      derived_from = "source_manifest-1"
    )
  }
  if (isTRUE(extra_preprocessing)) {
    entities[[length(entities) + 1L]] <- m7_003_test_entity(
      "preprocessing_state", "preprocessing_state-2", stage,
      derived_from = "source_manifest-1"
    )
  }
  if (stage == "C" && isTRUE(extra_integrations)) {
    entities[[length(entities) + 1L]] <- m7_003_test_entity(
      "method_state", "method_state-2", stage, method_id = "block.spls",
      derived_from = "source_manifest-1"
    )
    result <- m7_003_test_entity(
      "integration_result", "integration_result-2", stage, method_id = "block.spls",
      derived_from = "source_manifest-1"
    )
    state <- m7_003_test_entity(
      "integration_state", "integration_state-2", stage, method_id = "block.spls",
      member_of = "integration_result-2", derived_from = "source_manifest-1"
    )
    entities[[length(entities) + 1L]] <- result
    entities[[length(entities) + 1L]] <- state
    state_index <- which(vapply(entities, function(item) identical(item$scientific_role, "integration_state") &&
                                  identical(item$method_id, "block.spls"), logical(1)))
    result_index <- which(vapply(entities, function(item) identical(item$scientific_role, "integration_result") &&
                                   identical(item$method_id, "block.spls"), logical(1)))
    entities[[state_index]]$member_of_entity_id <- entities[[result_index]]$entity_id
  }
  for (route in routes) {
    role <- if (identical(route$route_kind, "clinical")) "clinical_evidence" else "native_sidecar"
    method <- route$method_id
    if (is.null(method)) method <- "limma"
    entities[[length(entities) + 1L]] <- m7_003_test_entity(
      role, paste0(role, "-", route$route_id), stage, method_id = method,
      route_id = route$route_id, derived_from = "source_manifest-1"
    )
  }
  all_ids <- vapply(entities, `[[`, character(1), "entity_id")
  external_ids <- vapply(entities, function(item) {
    if (is.null(item$generated_by_activity_id)) item$entity_id else NA_character_
  }, character(1))
  external_ids <- sort(external_ids[!is.na(external_ids)], method = "radix")
  generated_ids <- sort(all_ids[!all_ids %in% external_ids], method = "radix")
  entities <- entities[order(
    vapply(entities, `[[`, character(1), "scientific_role"),
    vapply(entities, `[[`, character(1), "entity_type"),
    vapply(entities, `[[`, character(1), "entity_id"), method = "radix"
  )]
  activity <- list(
    activity_id = "export-activity",
    stage = stage,
    activity_type = "handoff_export",
    method_id = "limma",
    used_entity_ids = as.list(external_ids),
    generated_entity_ids = as.list(generated_ids)
  )
  route_order <- if (length(routes) == 0L) integer() else order(
    vapply(routes, `[[`, character(1), "route_kind"),
    vapply(routes, `[[`, character(1), "route_id"), method = "radix"
  )
  routes <- routes[route_order]
  profile <- list(
    profile = "triomics.stage-handoff-provenance",
    profile_version = "1.0",
    producer_stage = stage,
    study_id = "synthetic-study",
    run_id = "synthetic-run",
    handoff_artifact_id = paste0("handoff-", tolower(stage)),
    producer_activity_id = "export-activity",
    identity = list(
      schema_uri = "urn:triomics:stage-handoff-provenance:identity",
      schema_version = "1.0",
      handoff_role = paste0("stage-", tolower(stage), "-handoff"),
      activities = I(list(activity)),
      entities = I(entities),
      declared_routes = I(routes)
    ),
    identity_sha256 = m7_003_test_digest,
    producer = list(
      package_version = "0.1.0",
      repository_commit = paste(rep("1", 40L), collapse = ""),
      module_id = if (stage == "B") "B-T07" else "C-06",
      method_ids = I(list("limma")),
      effective_configuration_sha256 = m7_003_test_digest,
      runtime_backend_versions = I(list(list(component = "R", version = "4.6.0")))
    ),
    limitations = I(list("public synthetic contract fixture; no scientific validation")),
    migration = NULL
  )
  if (stage == "C" && isTRUE(extra_integrations)) {
    profile$producer$method_ids <- I(list("block.spls", "limma"))
    activity$method_id <- "limma"
    profile$identity$activities <- I(list(activity))
  }
  profile
}

m7_003_test_refresh_identity <- function(profile) {
  profile$identity_sha256 <- triomics_provenance_identity_sha256(profile$identity)
  profile
}

m7_003_test_validate <- function(profile, payload_id = "provenance-payload-current",
                                 envelope_id = "provenance-envelope-current") {
  profile <- m7_003_test_refresh_identity(profile)
  triomics_validate_stage_handoff_provenance(profile, payload_id, envelope_id)
}

m7_003_test_schema_valid <- function(profile) {
  json <- as.character(jsonlite::toJSON(profile, auto_unbox = TRUE, null = "null", digits = NA))
  schema <- triomics:::triomics_provenance_schema_path()
  jsonvalidate::json_validate(json, schema, engine = "ajv")
}

m7_003_test_expect_invalid <- function(profile, payload_id = "provenance-payload-current",
                                       envelope_id = "provenance-envelope-current") {
  result <- tryCatch(
    triomics_validate_stage_handoff_provenance(
      m7_003_test_refresh_identity(profile), payload_id, envelope_id
    ),
    error = identity
  )
  testthat::expect_s3_class(result, "triomics_error")
  testthat::expect_identical(result$code, "D_PROVENANCE_INVALID")
  invisible(result)
}

testthat::test_that("JCS matches the RFC 8785 canonical sample and selected Appendix B boundaries", {
  sample <- '{"numbers":[333333333.33333329,1E30,4.50,2e-3,0.000000000000000000000000001],"string":"€$\\u000f\\nA\'B\\\"\\\\\\\"/","literals":[null,true,false]}'
  expected <- '{"literals":[null,true,false],"numbers":[333333333.3333333,1e+30,4.5,0.002,1e-27],"string":"€$\\u000f\\nA\'B\\\"\\\\\\\"/"}'
  testthat::expect_identical(triomics_jcs_canonicalize_json(sample), expected)

  vectors <- c(
    "5e-324" = "5e-324",
    "1e-6" = "0.000001",
    "9.999999999999997e-7" = "9.999999999999997e-7",
    "1e-7" = "1e-7",
    "1e20" = "100000000000000000000",
    "1e21" = "1e+21",
    "9.999999999999997e22" = "9.999999999999997e+22",
    "1e23" = "1e+23",
    "1.7976931348623157e308" = "1.7976931348623157e+308"
  )
  for (input in names(vectors)) {
    testthat::expect_identical(triomics_jcs_canonicalize_json(input), unname(vectors[[input]]))
  }
  testthat::expect_identical(triomics_jcs_canonicalize_json("-0"), "0")
})

testthat::test_that("JCS sorts object keys by UTF-16, preserves arrays, Unicode, and escapes", {
  astral <- intToUtf8(0x10000)
  bmp <- intToUtf8(0xE000)
  input <- paste0('{"', bmp, '":1,"', astral, '":2,"z":"line\\nquote\\\" slash/"}')
  expected <- paste0('{"z":"line\\nquote\\\" slash/","', astral, '":2,"', bmp, '":1}')
  testthat::expect_identical(triomics_jcs_canonicalize_json(input), expected)
  testthat::expect_identical(
    triomics_jcs_canonicalize_json('{"array":["second","first"]}'),
    '{"array":["second","first"]}'
  )
  testthat::expect_identical(
    triomics_jcs_canonicalize_json('{"x":"\\b\\t\\n\\f\\r\\\"\\\\"}'),
    '{"x":"\\b\\t\\n\\f\\r\\\"\\\\"}'
  )
})

testthat::test_that("raw JSON duplicate keys, malformed Unicode, and invalid JSON fail closed", {
  invalid <- c(
    '{"x":1,"x":2}',
    '{"outer":{"x":1,"x":2}}',
    '{"x":1,"\\u0078":2}',
    '"\\uD800"',
    '"\\uDC00"',
    '{"x":01}',
    '{"x":NaN}',
    '{"x":Infinity}'
  )
  for (json in invalid) {
    testthat::expect_error(triomics_jcs_canonicalize_json(json), class = "triomics_error")
  }
})

testthat::test_that("identity hashing is key-order invariant and rejects non-I-JSON identity numbers", {
  first <- list(schema_uri = "urn:test", schema_version = "1", handoff_role = "B",
                activities = I(list()), entities = I(list()), declared_routes = I(list()))
  second <- list(declared_routes = I(list()), entities = I(list()), activities = I(list()),
                 handoff_role = "B", schema_version = "1", schema_uri = "urn:test")
  testthat::expect_identical(
    triomics_provenance_identity_sha256(first),
    triomics_provenance_identity_sha256(second)
  )
  first$activities <- I(list(list(activity_id = "a"), list(activity_id = "b")))
  second$activities <- I(list(list(activity_id = "b"), list(activity_id = "a")))
  testthat::expect_false(identical(
    triomics_provenance_identity_sha256(first), triomics_provenance_identity_sha256(second)
  ))

  identity <- list(schema_uri = "urn:test", schema_version = "1", handoff_role = "B",
                   activities = I(list()), entities = I(list()), declared_routes = I(list()))
  for (raw in c(
    '{"schema_uri":"urn:test","schema_version":"1","handoff_role":"B","activities":[],"entities":[],"declared_routes":[],"number":1.5}',
    '{"schema_uri":"urn:test","schema_version":"1","handoff_role":"B","activities":[],"entities":[],"declared_routes":[],"number":-0}',
    '{"schema_uri":"urn:test","schema_version":"1","handoff_role":"B","activities":[],"entities":[],"declared_routes":[],"number":9007199254740992}',
    '{"schema_uri":"urn:test","schema_version":"1","handoff_role":"B","activities":[],"entities":[],"declared_routes":[],"number":1e0}'
  )) {
    testthat::expect_error(triomics_provenance_identity_sha256(raw), class = "triomics_error")
  }
  identity$entities <- I(list(list(size_bytes = 1.25)))
  testthat::expect_error(triomics_provenance_identity_sha256(identity), class = "triomics_error")
  identity$entities <- I(list(list(size_bytes = -0.0)))
  testthat::expect_error(triomics_provenance_identity_sha256(identity), class = "triomics_error")
  identity$entities <- I(list(list(size_bytes = 9007199254740992)))
  testthat::expect_error(triomics_provenance_identity_sha256(identity), class = "triomics_error")
  identity$entities <- I(list())
  identity$entities <- I(list(
    list(entity_id = "x", entity_type = "triomics.source_manifest", scientific_role = "source_manifest"),
    list(entity_id = "y", entity_type = "triomics.source_manifest", scientific_role = "source_manifest")
  ))
  reversed <- identity
  reversed$entities <- I(rev(reversed$entities))
  testthat::expect_identical(
    triomics_provenance_identity_sha256(identity),
    triomics_provenance_identity_sha256(reversed)
  )
})

testthat::test_that("complete public synthetic B and C profiles pass schema and contract validation", {
  b <- m7_003_test_profile("B")
  c <- m7_003_test_profile("C")
  testthat::expect_true(isTRUE(m7_003_test_schema_valid(b)))
  testthat::expect_true(isTRUE(m7_003_test_schema_valid(c)))
  testthat::expect_true(m7_003_test_validate(b))
  testthat::expect_true(m7_003_test_validate(c))
})

testthat::test_that("root and closed nested objects reject undeclared members", {
  profile <- m7_003_test_profile("C", routes = list(list(
    route_id = "native-1", route_kind = "native", analysis_id = "analysis-1",
    preparation_context_id = "prep-1", representation_id = "representation-1",
    method_id = "limma"
  )))
  mutations <- list(
    function(x) { x$unexpected <- TRUE; x },
    function(x) { x$identity$unexpected <- TRUE; x },
    function(x) { x$identity$activities[[1L]]$unexpected <- TRUE; x },
    function(x) { x$identity$entities[[1L]]$unexpected <- TRUE; x },
    function(x) { x$identity$declared_routes[[1L]]$unexpected <- TRUE; x },
    function(x) { x$producer$unexpected <- TRUE; x },
    function(x) { x$limitations <- I(list(list(text = "unexpected"))); x },
    function(x) {
      x$migration <- list(
        mode = "deterministic_legacy_packaging",
        source_artifacts = I(list(list(artifact_id = "old-artifact", sha256 = m7_003_test_digest))),
        source_acceptance_commit = paste(rep("1", 40L), collapse = ""),
        source_acceptance_receipt_refs = I(list("receipt")),
        migration_software_commit = paste(rep("2", 40L), collapse = ""),
        migration_software_version = "1",
        proof_status_by_role = list(
          source_manifest = "proven", normalized_metadata = "proven", feature_registry = "proven",
          analysis_unit_map = "proven", resolved_analysis = "proven", represented_matrix = "proven",
          preprocessing_state = "proven", method_state = "proven", predecessor_envelope = "proven",
          predecessor_payload = "proven", candidate_nominations = "proven", evaluation_evidence = "proven",
          integration_result = "proven", integration_state = "proven", handoff_payload = "proven",
          native_sidecar = "proven"
        ),
        numerical_payload_bytes_unchanged = TRUE,
        unexpected = TRUE
      ); x
    }
  )
  for (mutate in mutations) m7_003_test_expect_invalid(mutate(profile))
})

testthat::test_that("every role accepts only its registered entity type", {
  profile <- m7_003_test_profile("C", routes = list(list(
    route_id = "clinical-1", route_kind = "clinical", analysis_id = "analysis-1",
    preparation_context_id = "prep-1", representation_id = "representation-1",
    method_id = NULL
  ), list(
    route_id = "native-1", route_kind = "native", analysis_id = "analysis-1",
    preparation_context_id = "prep-1", representation_id = "representation-1",
    method_id = "limma"
  )))
  for (index in seq_along(profile$identity$entities)) {
    wrong <- profile
    role <- wrong$identity$entities[[index]]$scientific_role
    other_types <- setdiff(unname(m7_003_test_types), m7_003_test_types[[role]])
    wrong$identity$entities[[index]]$entity_type <- other_types[[1L]]
    m7_003_test_expect_invalid(wrong)
    unknown <- profile
    unknown$identity$entities[[index]]$scientific_role <- "unknown_role"
    m7_003_test_expect_invalid(unknown)
    unknown_type <- profile
    unknown_type$identity$entities[[index]]$entity_type <- "unknown.type"
    m7_003_test_expect_invalid(unknown_type)
  }
})

testthat::test_that("scope requirements, exact cardinalities, and global entity IDs are enforced", {
  profile <- m7_003_test_profile("B")
  for (case in list(
    function(x) { x$identity$entities[[which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "feature_registry")]]$block_id <- NULL; x },
    function(x) { x$identity$entities[[which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "normalized_metadata")]]$method_id <- "unexpected"; x },
    function(x) { x$identity$entities <- I(Filter(function(e) e$scientific_role != "family_ledger", x$identity$entities)); x },
    function(x) { x$identity$entities[[2L]]$entity_id <- x$identity$entities[[1L]]$entity_id; x }
  )) m7_003_test_expect_invalid(case(profile))

  multiple_states <- m7_003_test_profile("B", extra_preprocessing = TRUE)
  testthat::expect_true(m7_003_test_validate(multiple_states))
  duplicated_registry <- profile
  duplicate <- duplicated_registry$identity$entities[[which(vapply(
    duplicated_registry$identity$entities, `[[`, character(1), "scientific_role"
  ) == "feature_registry")]]
  duplicate$entity_id <- "feature_registry-2"
  duplicated_registry$identity$entities <- I(c(duplicated_registry$identity$entities, list(duplicate)))
  m7_003_test_expect_invalid(duplicated_registry)
})

testthat::test_that("route declarations are closed, sorted, countable, and method-bound", {
  routes <- list(
    list(route_id = "clinical-a", route_kind = "clinical", analysis_id = "analysis-1",
         preparation_context_id = "prep-1", representation_id = "representation-1", method_id = NULL),
    list(route_id = "clinical-b", route_kind = "clinical", analysis_id = "analysis-1",
         preparation_context_id = "prep-1", representation_id = "representation-1", method_id = NULL),
    list(route_id = "native-a", route_kind = "native", analysis_id = "analysis-1",
         preparation_context_id = "prep-1", representation_id = "representation-1", method_id = "limma"),
    list(route_id = "native-b", route_kind = "native", analysis_id = "analysis-1",
         preparation_context_id = "prep-1", representation_id = "representation-1", method_id = "limma")
  )
  profile <- m7_003_test_profile("C", routes = routes)
  testthat::expect_true(m7_003_test_validate(profile))
  cases <- list(
    function(x) { x$identity$declared_routes[[1L]]$route_id <- ""; x },
    function(x) { x$identity$declared_routes[[2L]]$route_id <- x$identity$declared_routes[[1L]]$route_id; x },
    function(x) { x$identity$declared_routes[[1L]]$route_kind <- "native"; x },
    function(x) { x$identity$declared_routes[[1L]]$method_id <- "limma"; x },
    function(x) { x$identity$declared_routes[[3L]]$method_id <- NULL; x },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "native_sidecar")[[1L]]
      x$identity$entities[[i]]$method_id <- "block.spls"
      x
    },
    function(x) {
      x$identity$entities <- I(Filter(function(e) e$route_id != "native-b", x$identity$entities)); x
    },
    function(x) {
      x$identity$entities[[which(vapply(x$identity$entities, function(e) identical(e$route_id, "native-a"), logical(1)))]]$route_id <- "undeclared"; x
    },
    function(x) {
      x$identity$declared_routes <- I(x$identity$declared_routes[-1L]); x
    }
  )
  for (mutate in cases) m7_003_test_expect_invalid(mutate(profile))
})

testthat::test_that("producer stage, predecessor pairs, integration pairs, and handoff mapping are enforced", {
  profile <- m7_003_test_profile("C", extra_integrations = TRUE)
  testthat::expect_true(m7_003_test_validate(profile))
  cases <- list(
    function(x) { x$producer_stage <- "B"; x },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "predecessor_payload")
      x$identity$entities[[i]]$member_of_entity_id <- "missing-envelope"; x
    },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "predecessor_payload")
      x$identity$entities[[i]]$digest <- paste(rep("e", 64L), collapse = ""); x
    },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "integration_state")[[1L]]
      x$identity$entities[[i]]$member_of_entity_id <- "integration_result-2"; x
    },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "handoff_payload")
      x$identity$entities[[i]]$artifact_id <- "different-handoff"; x
    },
    function(x) { x$handoff_artifact_id <- "provenance-payload-current"; x }
  )
  for (mutate in cases) m7_003_test_expect_invalid(mutate(profile))
})

testthat::test_that("activity graph, sorting, acyclicity, producer activity, and frame exclusion are enforced", {
  profile <- m7_003_test_profile("B")
  testthat::expect_true(m7_003_test_validate(profile))
  cases <- list(
    function(x) { x$identity$activities[[1L]]$used_entity_ids <- I(list("absent")); x },
    function(x) { x$identity$activities[[1L]]$generated_entity_ids <- I(list("source_manifest-1")); x },
    function(x) { x$producer_activity_id <- "absent"; x },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "resolved_analysis")
      x$identity$entities[[i]]$derived_from_entity_ids <- I(list("resolved_analysis-1")); x
    },
    function(x) { x$identity$activities[[1L]]$used_entity_ids <- I(list("source_manifest-1", "predecessor-envelope-1")); x },
    function(x) {
      x$identity$activities[[1L]]$generated_entity_ids <- I(list("provenance-payload-current")); x
    },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "handoff_payload")
      x$identity$entities[[i]]$entity_id <- "provenance-payload-current"; x
    },
    function(x) {
      i <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == "handoff_payload")
      x$identity$entities[[i]]$entity_id <- "provenance-envelope-current"; x
    },
    function(x) {
      x$identity$activities[[1L]]$generated_entity_ids <- I(rev(x$identity$activities[[1L]]$generated_entity_ids)); x
    }
  )
  for (mutate in cases) m7_003_test_expect_invalid(mutate(profile))

  later <- m7_003_test_profile("C")
  predecessor_index <- which(vapply(later$identity$entities, `[[`, character(1), "scientific_role") == "predecessor_envelope")
  previous_envelope_id <- later$identity$entities[[predecessor_index]]$entity_id
  previous_payload_id <- later$identity$entities[[predecessor_index + 1L]]$entity_id
  later$identity$entities[[predecessor_index]]$entity_id <- "prior-provenance-envelope"
  later$identity$entities[[predecessor_index]]$artifact_id <- "prior-provenance-envelope"
  later$identity$entities[[predecessor_index + 1L]]$entity_id <- "prior-provenance-payload"
  later$identity$entities[[predecessor_index + 1L]]$member_of_entity_id <-
    "prior-provenance-envelope"
  used_entity_ids <- unlist(later$identity$activities[[1L]]$used_entity_ids, use.names = FALSE)
  used_entity_ids <- c(
    setdiff(used_entity_ids, c(previous_envelope_id, previous_payload_id)),
    "prior-provenance-envelope", "prior-provenance-payload"
  )
  later$identity$activities[[1L]]$used_entity_ids <- as.list(sort(used_entity_ids, method = "radix"))
  testthat::expect_true(m7_003_test_validate(later))
})

testthat::test_that("storage form, size, canonical schema, and digest fields are conditional and exact", {
  profile <- m7_003_test_profile("B")
  mutate_entity <- function(x, role, mutation) {
    index <- which(vapply(x$identity$entities, `[[`, character(1), "scientific_role") == role)[[1L]]
    x$identity$entities[[index]] <- mutation(x$identity$entities[[index]])
    x
  }
  cases <- list(
    mutate_entity(profile, "source_manifest", function(e) { e$digest <- toupper(e$digest); e }),
    mutate_entity(profile, "source_manifest", function(e) { e$size_bytes <- -1L; e }),
    mutate_entity(profile, "source_manifest", function(e) { e$digest_kind <- "rfc8785"; e }),
    mutate_entity(profile, "source_manifest", function(e) { e$digest_algorithm <- "sha512"; e }),
    mutate_entity(profile, "source_manifest", function(e) { e$canonical_schema_uri <- "urn:test"; e }),
    mutate_entity(profile, "predecessor_envelope", function(e) { e$artifact_id <- NULL; e }),
    mutate_entity(profile, "predecessor_envelope", function(e) { e$artifact_payload_sha256 <- NULL; e }),
    mutate_entity(profile, "handoff_payload", function(e) { e$storage_form <- "embedded_json"; e })
  )
  for (case in cases) m7_003_test_expect_invalid(case)

  embedded <- profile
  index <- which(vapply(embedded$identity$entities, `[[`, character(1), "scientific_role") == "method_state")[[1L]]
  embedded$identity$entities[[index]]$storage_form <- "embedded_json"
  embedded$identity$entities[[index]]$digest_kind <- "rfc8785"
  embedded$identity$entities[[index]]["size_bytes"] <- list(NULL)
  embedded$identity$entities[[index]]$canonical_schema_uri <- "urn:triomics:method-state:1"
  testthat::expect_true(m7_003_test_validate(embedded))
})

testthat::test_that("profile identity digest is verified and malformed raw identity numbers fail", {
  profile <- m7_003_test_profile("B")
  profile$identity_sha256 <- paste(rep("f", 64L), collapse = "")
  result <- tryCatch(triomics_validate_stage_handoff_provenance(
    profile, "provenance-payload-current", "provenance-envelope-current"
  ), error = identity)
  testthat::expect_s3_class(result, "triomics_error")
  testthat::expect_identical(result$code, "D_PROVENANCE_INVALID")

  raw_profile <- as.character(jsonlite::toJSON(
    m7_003_test_refresh_identity(profile), auto_unbox = TRUE, null = "null", digits = NA
  ))
  raw_profile <- sub('"size_bytes":12', '"size_bytes":1.5', raw_profile, fixed = TRUE)
  result <- tryCatch(triomics_validate_stage_handoff_provenance(
    raw_profile, "provenance-payload-current", "provenance-envelope-current"
  ), error = identity)
  testthat::expect_s3_class(result, "triomics_error")
  testthat::expect_identical(result$code, "D_PROVENANCE_INVALID")
})

testthat::test_that("multiple represented blocks accept matching feature registries", {
  profile <- m7_003_test_profile("B")
  extra_entities <- list(
    m7_003_test_entity("feature_registry", "feature_registry-2", "B",
                       derived_from = "source_manifest-1"),
    m7_003_test_entity("represented_matrix", "represented_matrix-2", "B",
                       derived_from = "source_manifest-1"),
    m7_003_test_entity("preprocessing_state", "preprocessing_state-2", "B",
                       derived_from = "source_manifest-1")
  )
  for (index in seq_along(extra_entities)) extra_entities[[index]]$block_id <- "block-2"
  entities <- c(profile$identity$entities, extra_entities)
  entities <- entities[order(
    vapply(entities, `[[`, character(1), "scientific_role"),
    vapply(entities, `[[`, character(1), "entity_type"),
    vapply(entities, `[[`, character(1), "entity_id"), method = "radix"
  )]
  profile$identity$entities <- I(entities)
  generated_ids <- c(
    unlist(profile$identity$activities[[1L]]$generated_entity_ids, use.names = FALSE),
    vapply(extra_entities, `[[`, character(1), "entity_id")
  )
  profile$identity$activities[[1L]]$generated_entity_ids <- as.list(
    sort(generated_ids, method = "radix")
  )
  testthat::expect_true(m7_003_test_validate(profile))
})

testthat::test_that("every represented block has its required feature registry", {
  profile <- m7_003_test_profile("B")
  testthat::expect_true(m7_003_test_validate(profile))
  registry_index <- which(vapply(
    profile$identity$entities,
    function(entity) identical(entity$scientific_role, "feature_registry"),
    logical(1)
  ))[[1L]]
  profile$identity$entities[[registry_index]]$block_id <- "unrelated-block"
  m7_003_test_expect_invalid(profile)
})

testthat::test_that("every integration method group has its required method state", {
  profile <- m7_003_test_profile("C", extra_integrations = TRUE)
  testthat::expect_true(m7_003_test_validate(profile))
  method_state_index <- which(vapply(
    profile$identity$entities,
    function(entity) identical(entity$scientific_role, "method_state") &&
      identical(entity$method_id, "block.spls"),
    logical(1)
  ))[[1L]]
  missing_state_id <- profile$identity$entities[[method_state_index]]$entity_id
  profile$identity$entities <- I(Filter(
    function(entity) !identical(entity$entity_id, missing_state_id),
    profile$identity$entities
  ))
  generated_ids <- unlist(profile$identity$activities[[1L]]$generated_entity_ids, use.names = FALSE)
  profile$identity$activities[[1L]]$generated_entity_ids <- as.list(
    sort(setdiff(generated_ids, missing_state_id), method = "radix")
  )
  m7_003_test_expect_invalid(profile)
})

testthat::test_that("route evidence must match the declared route kind and complete scope", {
  route <- list(
    route_id = "native-only",
    route_kind = "native",
    analysis_id = "analysis-1",
    preparation_context_id = "prep-1",
    representation_id = "representation-1",
    method_id = "limma"
  )
  profile <- m7_003_test_profile("C", routes = list(route))
  testthat::expect_true(m7_003_test_validate(profile))
  extra_clinical <- m7_003_test_entity(
    "clinical_evidence", "clinical-evidence-surplus", "C", method_id = NULL,
    route_id = "native-only", derived_from = "source_manifest-1"
  )
  entities <- c(profile$identity$entities, list(extra_clinical))
  entities <- entities[order(
    vapply(entities, `[[`, character(1), "scientific_role"),
    vapply(entities, `[[`, character(1), "entity_type"),
    vapply(entities, `[[`, character(1), "entity_id"), method = "radix"
  )]
  profile$identity$entities <- I(entities)
  generated_ids <- unlist(profile$identity$activities[[1L]]$generated_entity_ids, use.names = FALSE)
  profile$identity$activities[[1L]]$generated_entity_ids <- as.list(
    sort(c(generated_ids, extra_clinical$entity_id), method = "radix")
  )
  m7_003_test_expect_invalid(profile)
})

testthat::test_that("method_state method IDs must cover the producer's declared methods", {
  profile <- m7_003_test_profile("B")
  testthat::expect_true(m7_003_test_validate(profile))
  method_state_index <- which(vapply(
    profile$identity$entities,
    function(entity) identical(entity$scientific_role, "method_state"),
    logical(1)
  ))[[1L]]
  profile$identity$entities[[method_state_index]]$method_id <- "unrelated-method"
  profile <- m7_003_test_refresh_identity(profile)
  m7_003_test_expect_invalid(profile)
})

testthat::test_that("normalized_metadata covers every profile analysis scope", {
  profile <- m7_003_test_profile("B")
  testthat::expect_true(m7_003_test_validate(profile))
  metadata_index <- which(vapply(
    profile$identity$entities,
    function(entity) identical(entity$scientific_role, "normalized_metadata"),
    logical(1)
  ))[[1L]]
  profile$identity$entities[[metadata_index]]$analysis_id <- "unrelated-analysis"
  profile <- m7_003_test_refresh_identity(profile)
  m7_003_test_expect_invalid(profile)
})

testthat::test_that("represented and preprocessing contexts require matching analysis evidence", {
  baseline <- m7_003_test_profile("B")
  testthat::expect_true(m7_003_test_validate(baseline))

  mismatched <- baseline
  for (role in c("represented_matrix", "preprocessing_state")) {
    index <- which(vapply(
      mismatched$identity$entities,
      function(entity) identical(entity$scientific_role, role),
      logical(1)
    ))[[1L]]
    mismatched$identity$entities[[index]]$preparation_context_id <- "prep-2"
  }
  mismatched <- m7_003_test_refresh_identity(mismatched)
  result <- tryCatch(m7_003_test_validate(mismatched), error = identity)
  testthat::expect_s3_class(result, "triomics_error")
  if (inherits(result, "triomics_error")) {
    testthat::expect_identical(result$code, "D_PROVENANCE_INVALID")
  }
})

testthat::test_that("multiple represented preparation contexts remain valid when fully covered", {
  profile <- m7_003_test_profile("B")
  context_roles <- c(
    "analysis_unit_map", "resolved_analysis", "represented_matrix",
    "preprocessing_state", "method_state"
  )
  extra_entities <- lapply(context_roles, function(role) {
    index <- which(vapply(
      profile$identity$entities,
      function(entity) identical(entity$scientific_role, role),
      logical(1)
    ))[[1L]]
    entity <- profile$identity$entities[[index]]
    entity$entity_id <- paste0(entity$entity_id, "-prep-2")
    entity$preparation_context_id <- "prep-2"
    entity
  })
  entities <- c(profile$identity$entities, extra_entities)
  entities <- entities[order(
    vapply(entities, `[[`, character(1), "scientific_role"),
    vapply(entities, `[[`, character(1), "entity_type"),
    vapply(entities, `[[`, character(1), "entity_id"), method = "radix"
  )]
  profile$identity$entities <- I(entities)
  generated_ids <- c(
    unlist(profile$identity$activities[[1L]]$generated_entity_ids, use.names = FALSE),
    vapply(extra_entities, `[[`, character(1), "entity_id")
  )
  profile$identity$activities[[1L]]$generated_entity_ids <- as.list(
    sort(generated_ids, method = "radix")
  )
  testthat::expect_true(m7_003_test_validate(profile))
})

testthat::test_that("migration proofs require the handoff_payload role", {
  profile <- m7_003_test_profile("B")
  proof_roles <- c(
    "source_manifest", "normalized_metadata", "feature_registry",
    "analysis_unit_map", "resolved_analysis", "represented_matrix",
    "preprocessing_state", "method_state", "predecessor_envelope",
    "predecessor_payload", "candidate_nominations", "block_results",
    "family_ledger", "handoff_payload"
  )
  profile$migration <- list(
    mode = "deterministic_legacy_packaging",
    source_artifacts = I(list(list(artifact_id = "legacy-b-handoff", sha256 = m7_003_test_digest))),
    source_acceptance_commit = paste(rep("1", 40L), collapse = ""),
    source_acceptance_receipt_refs = I(list("receipt")),
    migration_software_commit = paste(rep("2", 40L), collapse = ""),
    migration_software_version = "1",
    proof_status_by_role = as.list(setNames(rep("proven", length(proof_roles)), proof_roles)),
    numerical_payload_bytes_unchanged = TRUE
  )

  complete_result <- tryCatch(m7_003_test_validate(profile), error = identity)
  testthat::expect_true(isTRUE(complete_result))

  missing_handoff_proof <- profile
  missing_handoff_proof$migration$proof_status_by_role <-
    missing_handoff_proof$migration$proof_status_by_role[
      setdiff(names(missing_handoff_proof$migration$proof_status_by_role), "handoff_payload")
    ]
  malformed_result <- tryCatch(m7_003_test_validate(missing_handoff_proof), error = identity)
  testthat::expect_s3_class(malformed_result, "triomics_error")
  if (inherits(malformed_result, "triomics_error")) {
    testthat::expect_identical(malformed_result$code, "D_PROVENANCE_INVALID")
  }
})
