t07_test_root <- function(label) {
  project_root <- normalizePath(file.path(getwd(), "../.."), winslash = "/", mustWork = TRUE)
  base <- file.path(project_root, "artifacts", "acceptance", "B-T07", "testthat")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  path <- tempfile(pattern = paste0(label, "-"), tmpdir = base)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

t07_ticket <- function(name = "B-T06.yaml") {
  triomics_test_resource("examples", "tickets", name)
}

t07_write_artifact <- function(payload, path, artifact_type, artifact_id,
                               config_sha256, status = "partial", inputs = NULL) {
  triomics_write_json_atomic(payload, path)
  envelope <- triomics_artifact_envelope(
    artifact_type = artifact_type,
    artifact_id = artifact_id,
    study_id = payload$study_id,
    run_id = "b-t07-public-synthetic-fixture",
    status = status,
    config_sha256 = config_sha256,
    payload = path,
    producer_commit = "synthetic-fixture",
    inputs = if (is.null(inputs)) list() else inputs,
    reason_codes = if (identical(status, "partial")) "synthetic_member_unavailable" else character(),
    privacy_class = "synthetic"
  )
  triomics_write_envelope(envelope, sub("\\.json$", ".envelope.json", path))
  envelope
}

t07_public_bundle_fixture <- function(root, include_clinical = TRUE, ticket = "B-T08.yaml") {
  config_path <- t07_ticket(ticket)
  config <- triomics_read_config(config_path)
  config_sha256 <- attr(config, "config_sha256")
  source_sha256 <- triomics_hash_text("public synthetic B-T07 source")
  inputs <- list(list(artifact_id = "synthetic-measurement-source", sha256 = source_sha256))
  dir.create(file.path(root, "B"), recursive = TRUE, showWarnings = FALSE)

  family <- list(
    family_id = "family-synthetic-1",
    claim_scope = "scalar_contrast",
    member_ids = c("SYN-FEATURE-NEG", "SYN-FEATURE-UNAVAILABLE"),
    member_hash = triomics_b_hash(c("SYN-FEATURE-NEG", "SYN-FEATURE-UNAVAILABLE")),
    n_declared = 2L,
    n_estimated = 1L,
    n_failed = 1L,
    n_eligible = 1L,
    n_evaluated = 2L,
    dispositions = c(estimated = 1L, failed = 1L),
    internal_p = c(0.01, 1),
    internal_q = c(0.02, 1),
    correction_method = "BH",
    q_published = TRUE
  )
  common <- list(
    block_id = "block_1",
    analysis_id = "synthetic_primary",
    contrast_id = "treatment_minus_control",
    family_id = family$family_id,
    effect_unit = "group_difference",
    model_hash = triomics_b_hash(list(model = "synthetic independent model")),
    representation_id = "levels"
  )
  estimated <- c(common, list(
    result_id = "result-negative",
    feature_key = "SYN-FEATURE-NEG",
    effect = -2,
    ordinary_se = 0.5,
    moderated_se = 0.6,
    statistic = -3.3,
    statistic_name = "moderated_t",
    ordinary_df = 4,
    effective_test_df = 4,
    p = 0.01,
    q = 0.02,
    inferential_status = "unverified",
    estimation_status = "estimated",
    estimation_reason = NULL,
    n_samples = 6L,
    n_subjects = 6L,
    n_rows_used = 6L,
    representation_hash = triomics_b_hash(list(feature = "SYN-FEATURE-NEG"))
  ))
  unavailable <- c(common, list(
    result_id = "result-unavailable",
    feature_key = "SYN-FEATURE-UNAVAILABLE",
    effect = NULL,
    ordinary_se = NULL,
    moderated_se = NULL,
    statistic = NULL,
    statistic_name = NULL,
    ordinary_df = NULL,
    effective_test_df = NULL,
    p = NULL,
    q = NULL,
    inferential_status = "not_estimable",
    estimation_status = "unavailable",
    estimation_reason = "requested_method_unavailable",
    n_samples = 6L,
    n_subjects = 6L,
    n_rows_used = 0L,
    representation_hash = triomics_b_hash(list(feature = "SYN-FEATURE-UNAVAILABLE"))
  ))
  nomination <- list(
    nomination_id = triomics_b_hash(list(feature_key = "SYN-FEATURE-NEG", family_id = family$family_id)),
    feature_key = "SYN-FEATURE-NEG",
    source_branch = "primary_statistics",
    result_ref = list(
      analysis_id = "synthetic_primary", block_id = "block_1",
      claim_id = "treatment_minus_control", family_id = family$family_id
    ),
    selection_rule = "valid_q_and_reporting_effect_threshold",
    selection_value = list(p = 0.01, q = 0.02, effect = -2, effect_unit = "group_difference"),
    q_threshold = 0.05,
    effect_threshold = 0.5,
    filter_role = "reporting_only"
  )
  block_results <- list(
    schema_version = "2.0",
    study_id = config$study_id,
    analysis_id = "synthetic_primary",
    preparation_context_id = "b_t02_fixture",
    representation_id = "levels",
    status = "partial",
    method_status = "partial",
    requested_method = "deqms",
    method = "limma",
    unavailable_reason = "count_prior_not_qualified",
    deqms_status = list(status = "unavailable", reason = "aligned_count_prior_missing"),
    config_sha256 = config_sha256,
    runtime = list(r_version = paste(R.version$major, R.version$minor, sep = "."), limma_version = "synthetic"),
    blocks = list(block_1 = list(
      block_id = "block_1",
      representation_id = "levels",
      contrasts = list(treatment_minus_control = list(
        contrast_id = "treatment_minus_control",
        rows = list(estimated, unavailable),
        family = family,
        nominations = list(nomination),
        count_prior_status = "unavailable",
        sensitivity = list(status = "not_requested", reason = "fixture_only")
      ))
    ))
  )
  block_path <- file.path(root, "B", "block-results.json")
  t07_write_artifact(block_results, block_path, "block_results", "stage-b-block-results",
                     config_sha256, "partial", inputs)

  clinical_paths <- character()
  if (include_clinical) {
    association <- list(
      schema_version = "2.0", study_id = config$study_id,
      analysis_id = "synthetic_primary", status = "partial",
      family = list(family_id = "clinical-family", member_ids = c("SYN-ENDPOINT", "SYN-ABSENT"),
                    n_declared = 2L, n_failed = 1L, dispositions = c(estimated = 1L, failed = 1L)),
      results = list(list(endpoint_id = "SYN-ENDPOINT", effect = -1, effect_unit = "score_unit per unit",
                          estimation_status = "estimated"),
                     list(endpoint_id = "SYN-ABSENT", effect = NULL, q = NULL,
                          estimation_status = "unavailable", estimation_reason = "endpoint_absent"))
    )
    longitudinal <- list(
      schema_version = "2.0", study_id = config$study_id,
      analysis_id = "synthetic_primary", status = "unavailable",
      reason = "longitudinal_objective_not_requested",
      results = list(list(objective_id = "SYN-LONGITUDINAL", effect = NULL,
                          estimation_status = "unavailable", estimation_reason = "not_requested"))
    )
    clinical_paths <- c(
      file.path(root, "B", "clinical-associations.json"),
      file.path(root, "B", "clinical-longitudinal-associations.json")
    )
    t07_write_artifact(association, clinical_paths[[1L]], "clinical_association_results",
                       "b-t08-clinical-associations", config_sha256, "partial", inputs)
    t07_write_artifact(longitudinal, clinical_paths[[2L]], "clinical_association_results",
                       "b-t09-longitudinal-clinical-associations", config_sha256,
                       "unavailable", inputs)
  }
  list(config_path = config_path, config_sha256 = config_sha256,
       block_results_path = block_path, clinical_paths = clinical_paths)
}

t07_attempt_export <- function(...) {
  tryCatch(triomics_b07_export_handoff(...), error = function(error) error)
}

t07_attempt_resume <- function(...) {
  tryCatch(triomics_b07_resume_decision(...), error = function(error) error)
}

t07_resolve_matrices <- function(config_path) {
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  analysis <- config$analyses[[1L]]
  block_ids <- as.character(unlist(analysis$block_ids, use.names = FALSE))
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  lapply(resolved$blocks, function(block) block$assay)
}

t07_cli_smoke <- function(config_path, output) {
  cli <- triomics_test_resource("exec", "triomics")
  old_wd <- setwd(dirname(cli))
  on.exit(setwd(old_wd), add = TRUE)
  lines <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", "B", "--output", shQuote(output)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(lines, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = lines)
}

testthat::test_that("B-T07 AC1 exports full public bundles without dropping failures or lineage", {
  root <- t07_test_root("ac1")
  fixture <- t07_public_bundle_fixture(root, include_clinical = TRUE)
  exported <- t07_attempt_export(
    output = root, config_path = fixture$config_path,
    command = "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T07.yaml --stage B --output artifacts/acceptance/B-T07"
  )
  testthat::expect_type(exported, "list")
  if (inherits(exported, "error")) return(invisible(NULL))

  bundle <- jsonlite::read_json(exported$bundle_path, simplifyVector = FALSE)
  family <- jsonlite::read_json(exported$family_ledger_path, simplifyVector = FALSE)
  nominations <- jsonlite::read_json(exported$nominations_path, simplifyVector = FALSE)
  members <- vapply(bundle$members, function(member) member$artifact_id, character(1))
  testthat::expect_true(all(c("block_results", "family_ledger", "candidate_nominations",
                              "clinical_associations", "clinical_longitudinal_associations",
                              "stage_c_matrices") %in% members))
  testthat::expect_identical(unlist(family$families[[1L]]$member_ids, use.names = FALSE),
                             c("SYN-FEATURE-NEG", "SYN-FEATURE-UNAVAILABLE"))
  testthat::expect_identical(sum(unlist(family$families[[1L]]$dispositions)), 2L)
  testthat::expect_identical(nominations$nominations[[1L]]$selection_rule,
                             "valid_q_and_reporting_effect_threshold")
  testthat::expect_identical(bundle$clinical_associations$status, "partial")
  testthat::expect_identical(bundle$clinical_longitudinal_associations$status, "unavailable")
  testthat::expect_identical(bundle$method_dispositions[[1L]]$status, "unavailable")
})

testthat::test_that("B-T07 AC2 independent and paired CLI smokes retain signed effects and unavailable methods", {
  independent_config <- t07_ticket("B-T06.yaml")
  paired_config <- t07_ticket("B-T03.yaml")
  unavailable_config <- t07_ticket("B-T05.yaml")
  independent_root <- t07_test_root("ac2-independent")
  paired_root <- t07_test_root("ac2-paired")
  unavailable_root <- t07_test_root("ac2-unavailable")
  independent_run <- t07_cli_smoke(independent_config, independent_root)
  paired_run <- t07_cli_smoke(paired_config, paired_root)
  unavailable_run <- t07_cli_smoke(unavailable_config, unavailable_root)
  testthat::expect_identical(independent_run$status, 0L)
  testthat::expect_identical(paired_run$status, 0L)
  testthat::expect_identical(unavailable_run$status, 0L)
  if (any(c(independent_run$status, paired_run$status, unavailable_run$status) != 0L)) {
    return(invisible(NULL))
  }
  independent_matrices <- t07_resolve_matrices(independent_config)
  exported <- t07_attempt_export(
    output = independent_root, config_path = independent_config,
    matrices = independent_matrices,
    smoke_runs = list(independent = independent_root, paired = paired_root,
                      unavailable_method = unavailable_root),
    command = "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T07.yaml --stage B --output artifacts/acceptance/B-T07"
  )
  testthat::expect_type(exported, "list")
  if (inherits(exported, "error")) return(invisible(NULL))
  bundle <- jsonlite::read_json(exported$bundle_path, simplifyVector = FALSE)
  independent <- jsonlite::read_json(file.path(independent_root, "B", "block-results.json"), simplifyVector = FALSE)
  paired <- jsonlite::read_json(file.path(paired_root, "B", "block-results.json"), simplifyVector = FALSE)
  unavailable <- jsonlite::read_json(file.path(unavailable_root, "B", "block-results.json"), simplifyVector = FALSE)
  independent_row <- independent$blocks$block_1$contrasts[[1L]]$rows[[1L]]
  paired_row <- paired$blocks$block_1$contrasts[[1L]]$rows[[1L]]
  testthat::expect_identical(bundle$acceptance$AC2$verdict, "pass")
  testthat::expect_true(is.finite(independent_row$effect) && independent_row$effect != 0)
  testthat::expect_true(is.finite(paired_row$effect) && paired_row$effect != 0)
  testthat::expect_identical(paired$representation_id, "delta")
  testthat::expect_true(any(vapply(unavailable$blocks$block_1$contrasts[[1L]]$rows,
                                   function(row) identical(row$estimation_status, "unavailable"), logical(1))))
  receipt <- jsonlite::read_json(exported$acceptance_record_path, simplifyVector = FALSE)
  unavailable_methods <- receipt$smoke_runs$unavailable_method$method_dispositions
  testthat::expect_true(any(vapply(unavailable_methods,
                                   function(item) identical(item$status, "unavailable"), logical(1))))
})

testthat::test_that("B-T07 AC3 binds cache identity to the actual matrix and every dependency hash", {
  root <- t07_test_root("ac3")
  config_path <- t07_ticket("B-T06.yaml")
  run <- t07_cli_smoke(config_path, root)
  testthat::expect_identical(run$status, 0L)
  if (run$status != 0L) return(invisible(NULL))
  matrices <- t07_resolve_matrices(config_path)
  config <- triomics_read_config(config_path)
  exported <- tryCatch(
    triomics_b07_cli_after_stage_b(
      args = c("--config", config_path, "--stage", "B", "--output", root),
      stage_b_result = list(resolved_matrices = matrices)
    ),
    error = function(error) error
  )
  testthat::expect_type(exported, "list")
  if (inherits(exported, "error")) return(invisible(NULL))
  metadata <- jsonlite::read_json(exported$matrix_manifest_path, simplifyVector = FALSE)
  testthat::expect_identical(metadata$blocks$block_1$matrix_hash,
                             triomics_b07_matrix_hash("block_1", matrices$block_1))
  candidate <- triomics_b07_resume_candidate(exported$bundle_path)
  same <- triomics_b07_resume_decision(candidate, exported$cache_key)
  receipt <- jsonlite::read_json(exported$acceptance_record_path, simplifyVector = FALSE)
  changed_matrix <- matrices
  changed_matrix$block_1[1L, 1L] <- changed_matrix$block_1[1L, 1L] + 1
  original_matrix_key <- triomics_b07_cache_key(
    config_sha256 = attr(config, "config_sha256"),
    dependency_hashes = list(source = triomics_hash_text("public B source")),
    matrices = matrices
  )
  changed_matrix_key <- triomics_b07_cache_key(
    config_sha256 = attr(config, "config_sha256"),
    dependency_hashes = list(source = triomics_hash_text("public B source")),
    matrices = changed_matrix
  )
  changed_dependency <- triomics_b07_resume_decision(
    candidate,
    triomics_b07_cache_key(config_sha256 = attr(config, "config_sha256"),
                           dependency_hashes = c(fixture_source = triomics_hash_text("changed source")),
                           matrices = matrices,
                           model_hashes = list(block_1 = triomics_b_hash(list(model = "synthetic independent model"))),
                           representation_hashes = list(block_1 = triomics_b_hash(list(representation = "levels"))))
  )
  testthat::expect_true(isTRUE(same$reuse))
  testthat::expect_identical(receipt$acceptance[[3L]]$verdict, "pass")
  testthat::expect_false(isTRUE(changed_dependency$reuse))
  testthat::expect_identical(changed_dependency$reason, "invalidated")
  testthat::expect_false(identical(original_matrix_key, changed_matrix_key))
  testthat::expect_false(identical(triomics_b07_matrix_hash("block_1", matrices$block_1),
                                   triomics_b07_matrix_hash("block_1", changed_matrix$block_1)))
})

testthat::test_that("B-T07 AC3 rejects partial or hash-mismatched restart candidates", {
  expected <- triomics_hash_text("complete cache key")
  partial <- list(payload_present = TRUE, envelope_present = FALSE,
                  status = "complete", cache_key = expected,
                  payload_sha256 = expected, manifest_payload_sha256 = expected)
  mismatch <- list(payload_present = TRUE, envelope_present = TRUE,
                   status = "complete", cache_key = expected,
                   payload_sha256 = triomics_hash_text("payload"),
                   manifest_payload_sha256 = triomics_hash_text("manifest"))
  partial_decision <- t07_attempt_resume(partial, expected)
  mismatch_decision <- t07_attempt_resume(mismatch, expected)
  testthat::expect_false(inherits(partial_decision, "error"))
  testthat::expect_false(inherits(mismatch_decision, "error"))
  if (!inherits(partial_decision, "error")) {
    testthat::expect_identical(partial_decision, list(reuse = FALSE, reason = "partial"))
  }
  if (!inherits(mismatch_decision, "error")) {
    testthat::expect_identical(mismatch_decision, list(reuse = FALSE, reason = "hash_mismatch"))
  }
})

testthat::test_that("B-T07 AC4 records reproduction evidence and explicit limits", {
  root <- t07_test_root("ac4")
  fixture <- t07_public_bundle_fixture(root, include_clinical = TRUE)
  command <- "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T07.yaml --stage B --output artifacts/acceptance/B-T07"
  exported <- t07_attempt_export(output = root, config_path = fixture$config_path,
                                 command = command,
                                 working_ref = "b204b536962f8fab65df34fa49e88bd88aae7298")
  testthat::expect_type(exported, "list")
  if (inherits(exported, "error")) return(invisible(NULL))
  record <- paste(readLines(exported$reproduction_path, warn = FALSE), collapse = "\n")
  testthat::expect_true(grepl(command, record, fixed = TRUE))
  testthat::expect_true(grepl("environment SHA256", record, fixed = TRUE))
  testthat::expect_true(grepl("payload SHA256", record, fixed = TRUE))
  testthat::expect_true(grepl("unrun qualifications", record, ignore.case = TRUE))
  testthat::expect_true(grepl("not final HTML", record, ignore.case = TRUE))
  testthat::expect_true(grepl("not biological validation", record, ignore.case = TRUE))
  receipt <- jsonlite::read_json(exported$acceptance_record_path, simplifyVector = FALSE)
  testthat::expect_true(all(c("AC1", "AC2", "AC3", "AC4") %in%
                              vapply(receipt$acceptance, function(item) item$id, character(1))))
  testthat::expect_identical(receipt$acceptance[[4L]]$verdict, "pass")
  testthat::expect_true(triomics_validate_envelope(exported$acceptance_record_envelope_path))
  testthat::expect_true(triomics_validate_envelope(sub("\\.md$", ".envelope.json", exported$reproduction_path)))
  testthat::expect_identical(receipt$status, "partial")
})

testthat::test_that("B-T07 rejects orphaned clinical payloads instead of silently omitting them", {
  root <- t07_test_root("adversarial-orphan")
  fixture <- t07_public_bundle_fixture(root, include_clinical = FALSE)
  orphan <- list(schema_version = "2.0", study_id = "triomics_b_t06_fixture", status = "complete")
  triomics_write_json_atomic(orphan, file.path(root, "B", "clinical-associations.json"))
  attempted <- t07_attempt_export(output = root, config_path = fixture$config_path)
  testthat::expect_s3_class(attempted, "triomics_error")
  if (inherits(attempted, "triomics_error")) {
    testthat::expect_identical(attempted$code, "B_EXPORT_INCOMPLETE")
  }
})

testthat::test_that("B-T07 post-B hook refuses to invent an absent resolved matrix handoff", {
  root <- t07_test_root("adversarial-missing-runner-matrices")
  config_path <- t07_ticket("B-T06.yaml")
  attempted <- tryCatch(
    triomics_b07_cli_after_stage_b(
      args = c("--config", config_path, "--stage", "B", "--output", root),
      stage_b_result = list(status = "complete", resolved_matrices = NULL)
    ),
    error = function(error) error
  )
  testthat::expect_s3_class(attempted, "triomics_error")
  if (inherits(attempted, "triomics_error")) {
    testthat::expect_identical(attempted$code, "B_EXPORT_MATRIX")
  }
  testthat::expect_false(file.exists(file.path(root, "B", "handoffs")))
})

testthat::test_that("B-T07 handoff paths reject absolute and traversing members with typed errors", {
  root <- t07_test_root("adversarial-path")
  absolute <- tryCatch(triomics_b07_path_parts("/tmp/private-payload"), error = function(error) error)
  traversal <- tryCatch(
    triomics_b07_child_path(root, "B/../../private-payload"),
    error = function(error) error
  )
  testthat::expect_null(absolute)
  testthat::expect_s3_class(traversal, "triomics_error")
  if (inherits(traversal, "triomics_error")) {
    testthat::expect_identical(traversal$code, "B_EXPORT_PATH")
  }
})
