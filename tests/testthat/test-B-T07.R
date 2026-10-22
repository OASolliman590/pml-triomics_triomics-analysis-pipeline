t07_test_root <- function(label) {
  project_root <- normalizePath(file.path(getwd(), "../.."), winslash = "/", mustWork = TRUE)
  base <- file.path(project_root, "artifacts", "acceptance", "B-T07", "testthat")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  path <- tempfile(pattern = paste0(label, "-"), tmpdir = base)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

t07_package_context <- function() {
  source_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  namespace_root <- tryCatch(
    normalizePath(getNamespaceInfo(asNamespace("triomics"), "path"),
                  winslash = "/", mustWork = TRUE),
    error = function(error) ""
  )
  list(
    source_root = source_root,
    source_checkout = identical(namespace_root, source_root) &&
      dir.exists(file.path(source_root, "R")) &&
      file.exists(file.path(source_root, "inst", "exec", "triomics"))
  )
}

t07_package_resource <- function(...) {
  components <- as.character(c(...))
  context <- t07_package_context()
  if (context$source_checkout) return(triomics_test_resource(...))
  installed_path <- do.call(
    system.file,
    c(as.list(components), list(package = "triomics"))
  )
  if (!nzchar(installed_path) || !file.exists(installed_path)) {
    stop("Installed triomics resource not found: ", paste(components, collapse = "/"),
         call. = FALSE)
  }
  normalizePath(installed_path, winslash = "/", mustWork = TRUE)
}

t07_ticket <- function(name = "B-T06.yaml") {
  paired_ticket <- name %in% c("B-T03.yaml", "B-T05.yaml")
  data_group <- if (paired_ticket) "paired" else "independent"
  source_prefix <- if (paired_ticket) "../data/paired/" else {
    "../../../discovery/spec-v2/examples/independent/"
  }
  fixture_root <- tempfile(pattern = "b-t07-ticket-")
  ticket_dir <- file.path(fixture_root, "tickets")
  input_dir <- file.path(fixture_root, "inputs")
  dir.create(ticket_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

  source_ticket <- t07_package_resource("examples", "tickets", name)
  ticket_path <- file.path(ticket_dir, name)
  if (!file.copy(source_ticket, ticket_path, overwrite = TRUE)) {
    stop("Could not copy packaged B-T07 ticket fixture: ", name, call. = FALSE)
  }
  lines <- readLines(ticket_path, warn = FALSE)
  for (data_name in c("metadata.tsv", "proteomics.tsv")) {
    source_data <- t07_package_resource("examples", "data", data_group, data_name)
    if (!file.copy(source_data, file.path(input_dir, data_name), overwrite = TRUE)) {
      stop("Could not copy packaged B-T07 assay fixture: ", data_name, call. = FALSE)
    }
    source_ref <- paste0(source_prefix, data_name)
    if (!any(grepl(source_ref, lines, fixed = TRUE))) {
      stop("Packaged ticket fixture does not contain expected input path: ", source_ref,
           call. = FALSE)
    }
    lines <- gsub(source_ref, paste0("../inputs/", data_name), lines, fixed = TRUE)
  }
  writeLines(lines, ticket_path, useBytes = TRUE)
  normalizePath(ticket_path, winslash = "/", mustWork = TRUE)
}

t07_write_artifact <- function(payload, path, artifact_type, artifact_id,
                               config_sha256, status = "partial", inputs = NULL,
                               privacy_class = "synthetic") {
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
    privacy_class = privacy_class
  )
  triomics_write_envelope(envelope, sub("\\.json$", ".envelope.json", path))
  envelope
}

t07_public_bundle_fixture <- function(root, include_clinical = TRUE, ticket = "B-T08.yaml",
                                      privacy_class = "synthetic") {
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
                     config_sha256, "partial", inputs, privacy_class)

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
                       "b-t08-clinical-associations", config_sha256, "partial", inputs,
                       privacy_class)
    t07_write_artifact(longitudinal, clinical_paths[[2L]], "clinical_association_results",
                       "b-t09-longitudinal-clinical-associations", config_sha256,
                       "unavailable", inputs, privacy_class)
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

t07_expect_export_success <- function(value, label) {
  is_triomics_error <- inherits(value, "triomics_error")
  detail <- if (is_triomics_error) {
    paste(label, value$code, conditionMessage(value), sep = ": ")
  } else {
    paste(label, "export returned without a typed error")
  }
  testthat::expect_false(is_triomics_error, info = detail)
  if (is_triomics_error) list() else value
}

t07_read_json_safe <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) return(list())
  tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) list())
}

t07_item <- function(value, index = 1L) {
  if (is.list(value) && length(value) >= index) value[[index]] else list()
}

t07_acceptance_verdict <- function(record, acceptance_id) {
  acceptance <- record$acceptance
  item <- Filter(function(value) identical(as.character(value$id), acceptance_id), acceptance)
  if (!length(item)) NA_character_ else as.character(item[[1L]]$verdict)
}

t07_cli_smoke <- function(config_path, output) {
  context <- t07_package_context()
  rscript <- file.path(R.home("bin"), "Rscript")
  cli_args <- c("--config", shQuote(config_path), "--stage", "B", "--output", shQuote(output))

  if (context$source_checkout) {
    cli <- file.path(context$source_root, "inst", "exec", "triomics")
    original_wd <- getwd()
    setwd(dirname(cli))
    on.exit(setwd(original_wd), add = TRUE)
    lines <- system2(
      rscript,
      c("--vanilla", shQuote(basename(cli)), cli_args),
      stdout = TRUE, stderr = TRUE
    )
  } else {
    runner <- tempfile(pattern = "b-t07-installed-cli-", fileext = ".R")
    on.exit(unlink(runner), add = TRUE)
    writeLines(c(
      "status <- triomics:::triomics_main(commandArgs(trailingOnly = TRUE))",
      "quit(save = \"no\", status = as.integer(status))"
    ), runner, useBytes = TRUE)
    lines <- system2(
      rscript,
      c("--vanilla", shQuote(runner), cli_args),
      env = paste0("R_LIBS=", shQuote(paste(.libPaths(), collapse = .Platform$path.sep))),
      stdout = TRUE, stderr = TRUE
    )
  }
  status <- attr(lines, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = lines)
}

testthat::test_that("B-T07 AC1 exports full public bundles without dropping failures or lineage", {
  root <- t07_test_root("ac1")
  fixture <- t07_public_bundle_fixture(root, include_clinical = TRUE)
  exported <- t07_expect_export_success(t07_attempt_export(
    output = root, config_path = fixture$config_path,
    command = "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T07.yaml --stage B --output artifacts/acceptance/B-T07"
  ), "AC1")

  bundle <- t07_read_json_safe(exported$bundle_path)
  family <- t07_read_json_safe(exported$family_ledger_path)
  nominations <- t07_read_json_safe(exported$nominations_path)
  receipt <- t07_read_json_safe(exported$acceptance_record_path)
  members <- vapply(bundle$members, function(member) member$artifact_id, character(1))
  family_payload <- t07_item(family$families)$family
  dispositions <- unlist(family_payload$dispositions, use.names = TRUE)
  testthat::expect_true(all(c("block_results", "family_ledger", "candidate_nominations",
                              "clinical_associations", "clinical_longitudinal_associations",
                              "stage_c_matrices") %in% members))
  testthat::expect_identical(unlist(family_payload$member_ids, use.names = FALSE),
                             c("SYN-FEATURE-NEG", "SYN-FEATURE-UNAVAILABLE"))
  testthat::expect_identical(sum(as.integer(dispositions)), 2L)
  testthat::expect_identical(t07_item(nominations$nominations)$selection_rule,
                             "valid_q_and_reporting_effect_threshold")
  testthat::expect_identical(bundle$clinical_associations$status, "partial")
  testthat::expect_identical(bundle$clinical_longitudinal_associations$status, "unavailable")
  testthat::expect_identical(t07_item(bundle$method_dispositions)$status, "unavailable")
  testthat::expect_identical(t07_acceptance_verdict(receipt, "AC1"), "pass")
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
  testthat::expect_false(dir.exists(file.path(unavailable_root, "B", "handoffs")))

  unavailable_disposition_root <- t07_test_root("ac2-unavailable-disposition")
  unavailable_stage_result <- triomics_execute_stage_b(unavailable_config, unavailable_disposition_root)
  testthat::expect_identical(unavailable_stage_result$status, "unavailable")
  testthat::expect_identical(unavailable_stage_result$method_status, "unavailable")
  unavailable_disposition <- tryCatch(
    triomics_b07_cli_after_stage_b(
      args = c("--config", unavailable_config, "--stage", "B", "--output", unavailable_disposition_root),
      stage_b_result = unavailable_stage_result
    ),
    error = function(error) error
  )
  testthat::expect_false(inherits(unavailable_disposition, "triomics_error"),
                         info = if (inherits(unavailable_disposition, "triomics_error")) {
                           paste(unavailable_disposition$code, conditionMessage(unavailable_disposition))
                         } else "typed unavailable Stage B result has no exportable claims")
  disposition <- if (inherits(unavailable_disposition, "triomics_error")) list() else unavailable_disposition
  testthat::expect_identical(disposition$status, "unavailable")
  testthat::expect_identical(disposition$reason, "no_exportable_public_claims")
  testthat::expect_true(file.exists(unavailable_stage_result$payload_path))
  testthat::expect_false(dir.exists(file.path(unavailable_disposition_root, "B", "handoffs")))

  consolidated_root <- t07_test_root("ac2-consolidated")
  same_run_stage_result <- triomics_execute_stage_b(independent_config, consolidated_root)
  exported <- t07_expect_export_success(t07_attempt_export(
    output = consolidated_root, config_path = independent_config,
    matrices = same_run_stage_result$resolved_matrices,
    matrix_source = "stage_b_resolved_blocks",
    smoke_runs = list(independent = independent_root, paired = paired_root,
                      unavailable_method = unavailable_root),
    command = "Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T07.yaml --stage B --output artifacts/acceptance/B-T07"
  ), "AC2")
  independent <- t07_read_json_safe(file.path(independent_root, "B", "block-results.json"))
  paired <- t07_read_json_safe(file.path(paired_root, "B", "block-results.json"))
  unavailable <- t07_read_json_safe(file.path(unavailable_root, "B", "block-results.json"))
  independent_claim <- t07_item(independent$blocks$block_1$contrasts)
  paired_claim <- t07_item(paired$blocks$block_1$contrasts)
  independent_row <- t07_item(independent_claim$rows)
  paired_row <- t07_item(paired_claim$rows)
  receipt <- t07_read_json_safe(exported$acceptance_record_path)
  testthat::expect_identical(t07_acceptance_verdict(receipt, "AC2"), "pass")
  testthat::expect_identical(t07_acceptance_verdict(receipt, "AC3"), "pass")
  testthat::expect_true(is.finite(independent_row$effect) && independent_row$effect != 0)
  testthat::expect_true(is.finite(paired_row$effect) && paired_row$effect != 0)
  testthat::expect_identical(paired$representation_id, "delta")
  testthat::expect_identical(unavailable$deqms_status$status, "unavailable")
  testthat::expect_identical(unavailable$deqms_status$reason, "count_metadata_missing")
  unavailable_methods <- receipt$smoke_runs$unavailable_method$method_dispositions
  testthat::expect_true(any(vapply(unavailable_methods,
                                   function(item) identical(item$status, "unavailable"), logical(1))))
})

testthat::test_that("B-T07 AC3 validates the matrix hash and blocks unverified provenance", {
  root <- t07_test_root("ac3-unverified")
  config_path <- t07_ticket("B-T06.yaml")
  config <- triomics_read_config(config_path)
  # Use only the public source assay to exercise the hash boundary; this is not
  # the runner's same-run resolved object and must not prove AC3 integration.
  ingested <- tryCatch(triomics_ingest_configured_study(config, config_path), error = function(error) NULL)
  matrices <- if (is.null(ingested)) NULL else ingested$assays
  stage_b_result <- triomics_execute_stage_b(config_path, root)
  testthat::expect_identical(stage_b_result$status, "complete")
  testthat::expect_false(dir.exists(file.path(root, "B", "handoffs")))
  exported <- t07_expect_export_success(
    t07_attempt_export(output = root, config_path = config_path, matrices = matrices),
    "AC3 public matrix hash binding"
  )
  metadata <- t07_read_json_safe(exported$matrix_manifest_path)
  testthat::expect_identical(metadata$blocks$block_1$matrix_hash,
                             if (is.null(matrices)) NA_character_ else
                               triomics_b07_matrix_hash("block_1", matrices$block_1))
  candidate <- if (length(exported)) triomics_b07_resume_candidate(exported$bundle_path) else list()
  same <- t07_attempt_resume(candidate, exported$cache_key)
  receipt <- t07_read_json_safe(exported$acceptance_record_path)
  block_results <- t07_read_json_safe(file.path(root, "B", "block-results.json"))
  result_rows <- unlist(lapply(block_results$blocks$block_1$contrasts, `[[`, "rows"), recursive = FALSE)
  source_hashes <- vapply(result_rows, function(row) {
    if (is.null(row$source_hash)) NA_character_ else as.character(row$source_hash)
  }, character(1))
  changed_matrix <- matrices
  if (!is.null(changed_matrix)) changed_matrix$block_1[1L, 1L] <- changed_matrix$block_1[1L, 1L] + 1000
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
  changed_dependency_key <- triomics_b07_cache_key(
    config_sha256 = attr(config, "config_sha256"),
    dependency_hashes = list(source = triomics_hash_text("changed public B source")),
    matrices = matrices
  )
  testthat::expect_false(isTRUE(same$reuse))
  testthat::expect_identical(same$reason, "incomplete")
  testthat::expect_identical(t07_acceptance_verdict(receipt, "AC3"), "blocked")
  testthat::expect_identical(metadata$matrix_source, "caller_matrix_unverified")
  testthat::expect_true(length(source_hashes) > 0L && !is.null(matrices) &&
                          all(source_hashes == triomics_b07_matrix_hash("block_1", matrices$block_1)))
  testthat::expect_false(identical(original_matrix_key, changed_matrix_key))
  testthat::expect_false(identical(original_matrix_key, changed_dependency_key))
  testthat::expect_false(is.null(matrices) || identical(
    triomics_b07_matrix_hash("block_1", matrices$block_1),
    triomics_b07_matrix_hash("block_1", changed_matrix$block_1)
  ))
  changed_export <- t07_attempt_export(
    output = root, config_path = config_path, matrices = changed_matrix
  )
  testthat::expect_s3_class(changed_export, "triomics_error")
  testthat::expect_identical(changed_export$code, "B_EXPORT_MATRIX")
})

testthat::test_that("B-T07 integrated CLI writes a verified same-run matrix handoff", {
  root <- t07_test_root("integrated-cli")
  run <- t07_cli_smoke(t07_ticket("B-T07.yaml"), root)
  testthat::expect_identical(run$status, 0L, info = paste(run$output, collapse = "\n"))

  handoff_root <- file.path(root, "B", "handoffs")
  handoff_dirs <- if (dir.exists(handoff_root)) {
    list.dirs(handoff_root, recursive = FALSE, full.names = TRUE)
  } else {
    character()
  }
  testthat::expect_length(handoff_dirs, 1L)
  handoff_dir <- if (length(handoff_dirs) == 1L) handoff_dirs[[1L]] else file.path(handoff_root, "missing")
  bundle_path <- file.path(handoff_dir, "bundle.json")
  bundle_envelope_path <- file.path(handoff_dir, "bundle.envelope.json")
  matrix_manifest_path <- file.path(handoff_dir, "stage-c-matrices.json")
  matrix_manifest_envelope_path <- file.path(handoff_dir, "stage-c-matrices.envelope.json")
  matrix_values_path <- file.path(handoff_dir, "stage-c-matrix-values.rds")
  matrix_values_envelope_path <- file.path(handoff_dir, "stage-c-matrix-values.envelope.json")
  acceptance_record_path <- file.path(handoff_dir, "acceptance-record.json")
  acceptance_envelope_path <- file.path(handoff_dir, "acceptance-record.envelope.json")
  bundle <- t07_read_json_safe(bundle_path)
  matrix_manifest <- t07_read_json_safe(matrix_manifest_path)
  acceptance_record <- t07_read_json_safe(acceptance_record_path)

  testthat::expect_true(all(file.exists(c(
    bundle_path, matrix_manifest_path, matrix_values_path, acceptance_record_path
  ))))
  testthat::expect_identical(bundle$matrix_handoff$status, "complete")
  testthat::expect_identical(bundle$matrix_handoff$source, "stage_b_resolved_blocks")
  testthat::expect_identical(matrix_manifest$matrix_source, "stage_b_resolved_blocks")
  testthat::expect_identical(t07_acceptance_verdict(acceptance_record, "AC3"), "pass")
  testthat::expect_true(isTRUE(triomics_validate_envelope(bundle_envelope_path)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(matrix_manifest_envelope_path)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(matrix_values_envelope_path)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(acceptance_envelope_path)))
})

testthat::test_that("B-T07 Stage B returns the same-run keyed matrices without serializing them", {
  root <- tempfile("b-t07-same-run-matrices-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  config_path <- t07_ticket("B-T07.yaml")
  config <- triomics_read_config(config_path)
  block_ids <- as.character(config$analyses[[1L]]$block_ids)
  stage_b_result <- triomics_execute_stage_b(config_path, root)
  payload_path <- file.path(root, "B", "block-results.json")
  envelope_path <- file.path(root, "B", "block-results.envelope.json")
  payload <- t07_read_json_safe(payload_path)
  envelope <- t07_read_json_safe(envelope_path)

  testthat::expect_identical(names(payload), c(
    "schema_version", "study_id", "analysis_id", "preparation_context_id",
    "representation_id", "status", "limma_version", "config_sha256",
    "preparation_refit", "matrix_origin", "filter_applied", "runtime",
    "blocks", "method_status"
  ))
  result_ids <- character()
  for (block_id in names(payload$blocks)) {
    block <- payload$blocks[[block_id]]
    for (group_name in c("contrasts", "joint_claims")) {
      claims <- block[[group_name]]
      if (!is.list(claims)) next
      for (claim_id in names(claims)) {
        claim <- claims[[claim_id]]
        claim_type <- if (identical(group_name, "joint_claims")) "joint_contrast" else "scalar_contrast"
        contrast_id <- if (identical(claim_type, "joint_contrast")) {
          paste0("joint:", claim_id)
        } else {
          claim_id
        }
        for (row in claim$rows) {
          result_id <- if (is.null(row$result_id)) "" else as.character(row$result_id)
          testthat::expect_match(result_id, "^[a-f0-9]{64}$")
          testthat::expect_identical(as.character(row$study_id), as.character(payload$study_id))
          testthat::expect_identical(as.character(row$analysis_id), as.character(payload$analysis_id))
          testthat::expect_identical(as.character(row$block_id), block_id)
          testthat::expect_identical(as.character(row$claim_id), claim_id)
          testthat::expect_identical(as.character(row$claim_type), claim_type)
          testthat::expect_identical(as.character(row$family_id), as.character(claim$family$family_id))
          testthat::expect_identical(as.character(row$contrast_id), contrast_id)
          testthat::expect_identical(as.character(row$representation_id), as.character(payload$representation_id))
          result_ids <- c(result_ids, result_id)
        }
      }
    }
  }
  testthat::expect_gt(length(result_ids), 0L)
  testthat::expect_length(unique(result_ids), length(result_ids))
  testthat::expect_identical(names(envelope), c(
    "schema_version", "artifact_type", "artifact_id", "study_id", "run_id",
    "status", "created_utc", "config_sha256", "inputs", "payload_file",
    "payload_sha256", "producer", "reason_codes", "privacy_class"
  ))
  forbidden_fields <- c(
    "resolved_matrices", "assay", "fit", "native_model", "model_object",
    "lmFit", "MArrayLM"
  )
  has_forbidden_field <- function(value) {
    if (!is.list(value)) return(FALSE)
    any(names(value) %in% forbidden_fields) ||
      any(vapply(value, has_forbidden_field, logical(1)))
  }
  testthat::expect_false(has_forbidden_field(payload))
  testthat::expect_false(has_forbidden_field(envelope))

  resolved_matrices <- stage_b_result$resolved_matrices
  testthat::expect_identical(names(resolved_matrices), block_ids)
  if (is.list(resolved_matrices) && identical(names(resolved_matrices), block_ids)) {
    for (block_id in block_ids) {
      matrix <- resolved_matrices[[block_id]]
      testthat::expect_true(is.matrix(matrix) && is.numeric(matrix))
      testthat::expect_true(!is.null(rownames(matrix)) && !is.null(colnames(matrix)))
      testthat::expect_true(all(nzchar(rownames(matrix))) && all(nzchar(colnames(matrix))))
      testthat::expect_identical(anyDuplicated(rownames(matrix)), 0L)
      testthat::expect_identical(anyDuplicated(colnames(matrix)), 0L)

      block_result <- payload$blocks[[block_id]]
      rows <- unlist(lapply(block_result$contrasts, `[[`, "rows"), recursive = FALSE)
      source_hashes <- vapply(rows, function(row) as.character(row$source_hash), character(1))
      matrix_hash <- triomics_b_assay_hash(block_id, matrix)
      testthat::expect_true(length(source_hashes) > 0L)
      testthat::expect_true(all(source_hashes == matrix_hash))

      changed_matrices <- resolved_matrices
      changed_matrices[[block_id]][1L, 1L] <- changed_matrices[[block_id]][1L, 1L] + 1000
      changed_hash <- triomics_b_assay_hash(block_id, changed_matrices[[block_id]])
      testthat::expect_false(identical(matrix_hash, changed_hash))
      changed_binding <- tryCatch(
        triomics_b07_matrix_bindings(payload, changed_matrices),
        error = function(error) error
      )
      testthat::expect_s3_class(changed_binding, "triomics_error")
      if (inherits(changed_binding, "triomics_error")) {
        testthat::expect_identical(changed_binding$code, "B_EXPORT_MATRIX")
      }
    }
  }
})

testthat::test_that("B-T07 AC3 rejects partial or hash-mismatched restart candidates", {
  expected <- triomics_hash_text("complete cache key")
  complete <- list(payload_present = TRUE, envelope_present = TRUE,
                   status = "complete", cache_key = expected,
                   payload_sha256 = expected, manifest_payload_sha256 = expected)
  partial <- list(payload_present = TRUE, envelope_present = FALSE,
                  status = "complete", cache_key = expected,
                  payload_sha256 = expected, manifest_payload_sha256 = expected)
  mismatch <- list(payload_present = TRUE, envelope_present = TRUE,
                   status = "complete", cache_key = expected,
                   payload_sha256 = triomics_hash_text("payload"),
                   manifest_payload_sha256 = triomics_hash_text("manifest"))
  partial_decision <- t07_attempt_resume(partial, expected)
  mismatch_decision <- t07_attempt_resume(mismatch, expected)
  same_decision <- t07_attempt_resume(complete, expected)
  changed_dependency_decision <- t07_attempt_resume(complete, triomics_hash_text("changed dependency key"))
  testthat::expect_false(inherits(partial_decision, "error"))
  testthat::expect_false(inherits(mismatch_decision, "error"))
  testthat::expect_true(isTRUE(same_decision$reuse))
  testthat::expect_identical(changed_dependency_decision$reason, "invalidated")
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
  exported <- t07_expect_export_success(
    t07_attempt_export(output = root, config_path = fixture$config_path,
                       command = command,
                       working_ref = "b204b536962f8fab65df34fa49e88bd88aae7298"),
    "AC4"
  )
  reproduction_path <- exported$reproduction_path
  record <- if (is.character(reproduction_path) && length(reproduction_path) == 1L &&
                file.exists(reproduction_path)) {
    paste(readLines(reproduction_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  testthat::expect_true(grepl(command, record, fixed = TRUE))
  testthat::expect_true(grepl("environment SHA256", record, ignore.case = TRUE))
  testthat::expect_true(grepl("payload SHA256", record, fixed = TRUE))
  testthat::expect_true(grepl("unrun qualifications", record, ignore.case = TRUE))
  testthat::expect_true(grepl("not final HTML", record, ignore.case = TRUE))
  testthat::expect_true(grepl("not biological validation", record, ignore.case = TRUE))
  receipt <- t07_read_json_safe(exported$acceptance_record_path)
  testthat::expect_true(all(c("AC1", "AC2", "AC3", "AC4") %in%
                              vapply(receipt$acceptance, function(item) item$id, character(1))))
  testthat::expect_identical(t07_acceptance_verdict(receipt, "AC4"), "pass")
  testthat::expect_true(isTRUE(triomics_validate_envelope(exported$acceptance_record_envelope_path)))
  reproduction_envelope_path <- if (is.character(reproduction_path) && length(reproduction_path) == 1L) {
    sub("\\.md$", ".envelope.json", reproduction_path)
  } else {
    ""
  }
  testthat::expect_true(isTRUE(triomics_validate_envelope(reproduction_envelope_path)))
  testthat::expect_identical(receipt$status, "partial")
})

testthat::test_that("B-T07 AC4 refuses reuse if any reproduction or acceptance artifact is absent", {
  artifact_names <- c("reproduction.md", "reproduction.envelope.json",
                      "acceptance-record.json", "acceptance-record.envelope.json")
  for (artifact_name in artifact_names) {
    root <- t07_test_root(paste0("ac4-missing-", gsub("[^a-z]", "", artifact_name)))
    fixture <- t07_public_bundle_fixture(root, include_clinical = TRUE)
    exported <- t07_expect_export_success(
      t07_attempt_export(output = root, config_path = fixture$config_path),
      paste("AC4 artifact", artifact_name)
    )
    late_path <- file.path(dirname(exported$bundle_path), artifact_name)
    testthat::expect_true(file.exists(late_path))
    if (file.exists(late_path)) file.remove(late_path)
    candidate <- triomics_b07_resume_candidate(exported$bundle_path)
    decision <- t07_attempt_resume(candidate, exported$cache_key)
    testthat::expect_false(inherits(decision, "error"))
    testthat::expect_false(isTRUE(decision$reuse))
    attempted <- t07_attempt_export(output = root, config_path = fixture$config_path, resume = TRUE)
    testthat::expect_s3_class(attempted, "triomics_error")
    testthat::expect_identical(attempted$code, "B_EXPORT_INCOMPLETE")
  }
})

testthat::test_that("B-T07 reproduction wording follows the verified privacy classification", {
  root <- t07_test_root("ac4-privacy-context")
  fixture <- t07_public_bundle_fixture(root, include_clinical = TRUE,
                                       privacy_class = "private_study")
  exported <- t07_expect_export_success(
    t07_attempt_export(output = root, config_path = fixture$config_path),
    "AC4 privacy context"
  )
  reproduction_path <- exported$reproduction_path
  record <- if (is.character(reproduction_path) && length(reproduction_path) == 1L &&
                file.exists(reproduction_path)) {
    paste(readLines(reproduction_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  testthat::expect_true(grepl("private_study", record, fixed = TRUE))
  testthat::expect_false(grepl("public synthetic inputs", record, fixed = TRUE))
  testthat::expect_false(grepl("No real study inputs", record, fixed = TRUE))
  testthat::expect_false(grepl("No real study inputs, clinical identifiers, or private results were used",
                               record, fixed = TRUE))
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
