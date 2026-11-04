cnh_test_between_person_config <- function() {
  source_path <- triomics_test_resource("examples", "tickets", "C-08.yaml")
  fixture_root <- tempfile("triomics-cnh-single-visit-fixture-")
  dir.create(file.path(fixture_root, "tickets"), recursive = TRUE)
  source_data <- normalizePath(file.path(dirname(source_path), "..", "data"),
                               winslash = "/", mustWork = TRUE)
  stopifnot(file.copy(source_path, file.path(fixture_root, "tickets"), recursive = TRUE))
  stopifnot(file.copy(source_data, fixture_root, recursive = TRUE))
  config_path <- file.path(fixture_root, "tickets", basename(source_path))
  metadata_path <- file.path(fixture_root, "data", "c08", "metadata.tsv")
  links_path <- file.path(fixture_root, "data", "c08", "links.tsv")
  observations_path <- file.path(fixture_root, "data", "c08", "observations.tsv")
  metadata <- utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE)
  metadata$subject_id <- paste0("SINGLE_", metadata$sample_id)
  utils::write.table(metadata, metadata_path, sep = "\t", row.names = FALSE, quote = FALSE)
  links <- utils::read.delim(links_path, check.names = FALSE, stringsAsFactors = FALSE)
  links$subject_id <- paste0("SINGLE_", links$sample_id)
  links$link_id <- paste0("SINGLE_LINK_", links$sample_id)
  links$visit_id <- "V1"
  links$reference_time <- "2020-01-31"
  utils::write.table(links, links_path, sep = "\t", row.names = FALSE, quote = FALSE)
  observations <- utils::read.delim(observations_path, check.names = FALSE,
                                    stringsAsFactors = FALSE)
  observations$subject_id <- paste0("SINGLE_", observations$sample_id)
  observations$visit_id <- "V1"
  observations$time <- "2020-01-31"
  utils::write.table(observations, observations_path, sep = "\t",
                     row.names = FALSE, quote = FALSE)
  config <- yaml::read_yaml(config_path)
  for (index in seq_along(config$blocks)) {
    config$blocks[[index]]$source_refs <- as.list(config$blocks[[index]]$source_refs)
  }
  endpoint_id <- config$clinical$objectives[[1L]]$endpoint_ref
  objective <- config$clinical$objectives[[1L]]
  objective$mode <- "between_person"
  objective$time_window_ref <- "V1"
  objective$baseline_visit_ref <- NULL
  objective$later_visit_ref <- NULL
  objective$horizon_days <- NULL
  objective$baseline_adjustment <- NULL
  objective$outcome_estimand <- NULL
  config$clinical$objectives[[1L]] <- objective
  yaml::write_yaml(config, config_path)
  list(config_path = config_path, endpoint_id = endpoint_id, fixture_root = fixture_root)
}


cnh_test_multivisit_between_person_config <- function() {
  source_path <- triomics_test_resource("examples", "tickets", "C-08.yaml")
  config <- yaml::read_yaml(source_path)
  config_root <- dirname(source_path)
  resolve_fixture_path <- function(path) {
    if (is.null(path)) return(NULL)
    normalizePath(file.path(config_root, path), winslash = "/", mustWork = TRUE)
  }
  for (index in seq_along(config$sources)) {
    config$sources[[index]]$file <- resolve_fixture_path(config$sources[[index]]$file)
  }
  config$metadata$file <- resolve_fixture_path(config$metadata$file)
  for (index in seq_along(config$blocks)) {
    config$blocks[[index]]$file <- resolve_fixture_path(config$blocks[[index]]$file)
    config$blocks[[index]]$source_refs <- as.list(config$blocks[[index]]$source_refs)
  }
  for (field in c("dictionary_file", "observations_file", "links_file")) {
    config$clinical[[field]] <- resolve_fixture_path(config$clinical[[field]])
  }
  objective <- config$clinical$objectives[[1L]]
  objective$mode <- "between_person"
  objective$time_window_ref <- "V1"
  objective$baseline_visit_ref <- NULL
  objective$later_visit_ref <- NULL
  objective$horizon_days <- NULL
  objective$baseline_adjustment <- NULL
  objective$outcome_estimand <- NULL
  config$clinical$objectives[[1L]] <- objective
  path <- tempfile("triomics-cnh-multivisit-between-person-", fileext = ".yaml")
  yaml::write_yaml(config, path)
  path
}

testthat::test_that("native C publication JSON restores typed matrices and metadata", {
  metadata <- data.frame(
    sample_id = c("s1", "s2"),
    group = factor(c("case", "control"), levels = c("control", "case")),
    visit_day = as.Date(c("2020-01-01", "2020-01-08")),
    score = c(1.25, NA_real_),
    count = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  identity <- list(
    normalized_metadata = list(sample_metadata = metadata),
    analysis_unit_map = data.frame(
      unit_id = c("u1", "u2"), sample_ids = I(list("s1", "s2")),
      stringsAsFactors = FALSE
    ),
    blocks = list(block_a = list(matrix = matrix(
      c(1, 2, 3, 4), nrow = 2L,
      dimnames = list(c("f1", "f2"), c("u1", "u2"))
    )))
  )

  restored <- triomics_cnh_decode_r_value(triomics_cnh_encode_r_value(identity))

  testthat::expect_identical(restored, identity)
  testthat::expect_identical(class(restored$normalized_metadata$sample_metadata$group), "factor")
  testthat::expect_identical(class(restored$normalized_metadata$sample_metadata$visit_day), "Date")
  testthat::expect_type(restored$blocks$block_a$matrix, "double")
  testthat::expect_identical(restored$analysis_unit_map$sample_ids, I(list("s1", "s2")))
})

testthat::test_that("saved C replay distinguishes fresh, partial, and longitudinal routes", {
  between <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-fresh-c-route-")
  longitudinal <- triomics_test_resource("examples", "tickets", "C-08.yaml")
  on.exit(unlink(c(between$fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(output_root, recursive = TRUE)
  testthat::expect_null(triomics_cnh_publish_saved_c(between$config_path, output_root))
  dir.create(file.path(output_root, "C"), recursive = TRUE)
  file.create(file.path(output_root, "C", "integration-results.json"))
  partial_error <- testthat::capture_error(
    triomics_cnh_publish_saved_c(between$config_path, output_root)
  )
  testthat::expect_identical(partial_error$code, "C_PUBLICATION_CONTEXT_MISSING")
  testthat::expect_null(triomics_cnh_native_c_route(triomics_read_config(longitudinal)))
})

testthat::test_that("saved native C context replays the exact fit without refitting", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-saved-c-replay-")
  on.exit(unlink(c(fixture$fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  initial <- triomics_execute_stage_c(fixture$config_path, output_root)
  context_path <- file.path(output_root, "C", "native-publication-context.json")
  context_envelope_path <- file.path(output_root, "C", "native-publication-context.envelope.json")
  context <- jsonlite::read_json(context_path, simplifyVector = FALSE)
  c_identity <- triomics_cnh_decode_r_value(context$c_input_identity)
  testthat::expect_identical(initial$status, "endpoint_request_scheduled")
  testthat::expect_true(triomics_validate_envelope(context_envelope_path))
  testthat::expect_identical(
    triomics_b07_prov_value_sha256(c_identity), context$c_input_identity_sha256
  )
  testthat::expect_identical(c_identity$study_id, "triomics_c_08_fixture")
  testthat::expect_identical(nrow(c_identity$normalized_metadata$sample_metadata), 12L)
  testthat::expect_true(all(vapply(c_identity$blocks, function(block) {
    is.matrix(block$matrix) && identical(typeof(block$matrix), "double") &&
      identical(rownames(block$matrix), block$feature_ids) &&
      identical(colnames(block$matrix), block$unit_ids)
  }, logical(1L))))
  input_paths <- vapply(context$input_files, function(reference) as.character(reference$path), character(1L))
  testthat::expect_true(all(c(
    "C/integration-results.json", "C/integration-results.envelope.json",
    "C/native-states/integration-01.json", "C/native-states/integration-01.envelope.json",
    "C/score-endpoints/integration-01.json", "C/score-endpoints/integration-01.envelope.json",
    "B/clinical-score-association-requests/request-01.json",
    "B/clinical-score-association-requests/request-01.envelope.json"
  ) %in% input_paths))
  testthat::expect_false(file.exists(file.path(output_root, "D", "handoff-manifest.json")))

  b_exit <- triomics_main(c(
    "--config", fixture$config_path, "--stage", "B", "--output", output_root
  ))
  testthat::expect_identical(b_exit, 0L)
  testthat::expect_true(file.exists(file.path(output_root, "B", "clinical-associations.json")))

  fit_calls <- 0L
  fit_spy <- function(...) {
    fit_calls <<- fit_calls + 1L
    stop("The saved C replay attempted an integration fit.", call. = FALSE)
  }
  prep_spy <- function(...) {
    fit_calls <<- fit_calls + 1L
    stop("The saved C replay attempted preprocessing.", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    triomics_integration_fit_once = fit_spy,
    prepare_fit = prep_spy,
    .package = "triomics"
  )
  replay <- triomics_execute_stage_c(fixture$config_path, output_root)

  provenance_path <- file.path(output_root, "D", "stage-handoff-provenance.json")
  provenance_envelope_path <- file.path(output_root, "D", "stage-handoff-provenance.envelope.json")
  testthat::expect_identical(fit_calls, 0L)
  testthat::expect_identical(replay$run_status, "complete")
  testthat::expect_true(file.exists(file.path(output_root, "D", "handoff-manifest.json")))
  testthat::expect_true(triomics_validate_envelope(provenance_envelope_path))
  testthat::expect_true(triomics_validate_stage_handoff_provenance(
    jsonlite::read_json(provenance_path, simplifyVector = FALSE),
    "c06-stage-handoff-provenance", "c06-stage-handoff-provenance-envelope"
  ))
})

testthat::test_that("native B preflight authenticates the scheduled C08 V1 request", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-preflight-")
  on.exit(unlink(c(fixture$fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  scheduled <- triomics_execute_stage_c(fixture$config_path, output_root)
  native <- triomics_cnh_prepare_stage_b(fixture$config_path, output_root)

  testthat::expect_s3_class(native, "ClinicalNativeHandoffContext")
  testthat::expect_identical(native$request$status, "scheduled")
  testthat::expect_identical(native$request$objective_spec$objectives[[1L]]$mode, "between_person")
  testthat::expect_identical(native$request$objective_spec$objectives[[1L]]$time_window_ref, "V1")
  testthat::expect_identical(native$request$objective_spec$objectives[[1L]]$endpoint_ref,
                             fixture$endpoint_id)
  testthat::expect_identical(scheduled$status, "endpoint_request_scheduled")
  testthat::expect_false(file.exists(file.path(output_root, "B", "clinical-associations.json")))
})


testthat::test_that("the original multivisit C08 score remains ineligible for a single-visit request", {
  config_path <- cnh_test_multivisit_between_person_config()
  output_root <- tempfile("triomics-cnh-multivisit-refusal-")
  on.exit(unlink(c(config_path, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  error <- testthat::capture_error(triomics_execute_stage_c(config_path, output_root))

  testthat::expect_identical(error$code, "C_SCORE_TIME_MISMATCH")
})

testthat::test_that("saved requests and publication failures fail closed without partial C provenance", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-tamper-")
  on.exit(unlink(c(fixture$fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  triomics_execute_stage_c(fixture$config_path, output_root)
  native <- triomics_cnh_prepare_stage_b(fixture$config_path, output_root)
  request_path <- file.path(output_root, "B", "clinical-score-association-requests", "request-01.json")
  request_envelope_path <- sub("[.]json$", ".envelope.json", request_path)
  original_request <- readBin(request_path, what = "raw", n = file.info(request_path)$size)
  original_request_envelope <- readBin(
    request_envelope_path, what = "raw", n = file.info(request_envelope_path)$size
  )
  restore_request <- function() {
    if (file.exists(request_path) || triomics_is_symlink(request_path)) unlink(request_path)
    writeBin(original_request, request_path)
    writeBin(original_request_envelope, request_envelope_path)
  }

  publication_error <- testthat::capture_error(
    triomics_cnh_publish_stage_b(native, fixture$config_path, output_root)
  )
  testthat::expect_identical(publication_error$code, "B_SCORE_REQUEST_INVALID")
  testthat::expect_false(file.exists(file.path(output_root, "B", "clinical-associations.json")))
  testthat::expect_false(file.exists(file.path(output_root, "D", "stage-handoff-provenance.json")))

  request <- jsonlite::read_json(request_path, simplifyVector = FALSE)
  reordered <- request[rev(names(request))]
  jsonlite::write_json(reordered, request_path, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  reordered_error <- testthat::capture_error(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root)
  )
  testthat::expect_identical(reordered_error$code, "B_SCORE_REQUEST_INVALID")
  restore_request()

  request <- jsonlite::read_json(request_path, simplifyVector = FALSE)
  request$request_sha256 <- paste(rep("0", 64L), collapse = "")
  jsonlite::write_json(request, request_path, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  envelope <- jsonlite::read_json(request_envelope_path, simplifyVector = FALSE)
  envelope$payload_sha256 <- triomics_sha256(request_path)
  jsonlite::write_json(envelope, request_envelope_path, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  rehashed_error <- testthat::capture_error(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root)
  )
  testthat::expect_identical(rehashed_error$code, "B_SCORE_REQUEST_INVALID")
  restore_request()

  observations_path <- file.path(fixture$fixture_root, "data", "c08", "observations.tsv")
  original_observations <- readBin(observations_path, what = "raw", n = file.info(observations_path)$size)
  observations <- utils::read.delim(observations_path, stringsAsFactors = FALSE)
  observations$value[[1L]] <- as.numeric(observations$value[[1L]]) + 0.1
  utils::write.table(observations, observations_path, sep = "\t", row.names = FALSE,
                     quote = FALSE)
  source_error <- testthat::capture_error(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root)
  )
  testthat::expect_identical(source_error$code, "B_SCORE_LINEAGE_INVALID")
  writeBin(original_observations, observations_path)

  external_request <- tempfile("triomics-cnh-external-request-")
  writeBin(original_request, external_request)
  unlink(request_path)
  symlink_created <- file.symlink(external_request, request_path)
  testthat::expect_true(symlink_created)
  symlink_error <- testthat::capture_error(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root)
  )
  testthat::expect_identical(symlink_error$code, "B_SCORE_REQUEST_INVALID")
  unlink(request_path)
  writeBin(original_request, request_path)
  unlink(external_request)
  testthat::expect_s3_class(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root),
    "ClinicalNativeHandoffContext"
  )

  context_path <- file.path(output_root, "C", "native-publication-context.json")
  jsonlite::write_json(list(schema_version = "1.0"), context_path, auto_unbox = TRUE,
                       null = "null", pretty = TRUE, digits = NA)
  incomplete_context_error <- testthat::capture_error(
    triomics_cnh_publish_saved_c(fixture$config_path, output_root)
  )
  testthat::expect_identical(incomplete_context_error$code, "C_PUBLICATION_CONTEXT_INVALID")
  config <- triomics_read_config(fixture$config_path)
  collision_error <- testthat::capture_error(triomics_cnh_save_initial_c(
    integration_runs = list(list(status = "complete")), evaluation_controls = list(),
    c_input_identity = list(), config = config, output = output_root,
    source_inputs = list(), run_status = "complete"
  ))
  testthat::expect_identical(collision_error$code, "C_PUBLICATION_CONTEXT_CONFLICT")
  testthat::expect_false(file.exists(file.path(output_root, "D", "stage-handoff-provenance.json")))
})


testthat::test_that("native B publishes unchanged association results and bound identifier-free coverage", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-publication-")
  on.exit(unlink(c(fixture$fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)

  triomics_execute_stage_c(fixture$config_path, output_root)
  native <- triomics_cnh_prepare_stage_b(fixture$config_path, output_root)
  triomics_execute_stage_b(fixture$config_path, output_root)
  expected_fit <- triomics_associate_endpoints(
    native$request$endpoint_bundle, native$clinical_view, native$objective_spec
  )
  expected <- triomics_clinical_association_export(expected_fit)

  publication <- triomics_cnh_publish_stage_b(native, fixture$config_path, output_root)
  payload_path <- file.path(output_root, "B", "clinical-associations.json")
  envelope_path <- file.path(output_root, "B", "clinical-associations.envelope.json")
  payload <- jsonlite::read_json(payload_path, simplifyVector = FALSE)
  envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)

  testthat::expect_identical(publication$status, "complete")
  testthat::expect_identical(publication$artifact_ref$artifact_id, "b-t08-clinical-associations")
  testthat::expect_true(triomics_validate_envelope(envelope_path))
  testthat::expect_true(triomics_cnh_same_json(payload$results, expected$results))
  testthat::expect_true(triomics_cnh_same_json(payload$family, expected$family))
  testthat::expect_length(payload$clinical_dictionary_coverage$variables, 1L)
  variable <- payload$clinical_dictionary_coverage$variables[[1L]]
  testthat::expect_setequal(names(variable), c(
    "variable_id", "declared_type", "unit", "units_verification", "role",
    "time_semantics", "availability", "verification", "modelable", "missingness_rules"
  ))
  coverage <- payload$clinical_dictionary_coverage$coverage[[1L]]
  testthat::expect_identical(as.integer(coverage$n_subjects), 12L)
  testthat::expect_identical(as.integer(coverage$n_observed), 12L)
  testthat::expect_false(any(c("label", "evidence", "codebook", "numeric_value", "sample_id", "subject_id") %in%
                               names(variable)))
  raw_payload <- paste(readLines(payload_path, warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl("SINGLE_", raw_payload, fixed = TRUE))
  testthat::expect_false(grepl("SINGLE_LINK_", raw_payload, fixed = TRUE))
  input_ids <- vapply(envelope$inputs, function(input) as.character(input$artifact_id), character(1L))
  testthat::expect_setequal(input_ids, c(
    native$endpoint_reference$artifact_id,
    paste0(native$endpoint_reference$artifact_id, ":envelope"),
    native$request_reference$artifact_id,
    paste0(native$request_reference$artifact_id, ":envelope"),
    "clinical-dictionary-source", "clinical-observations-source", "clinical-links-source"
  ))
  testthat::expect_identical(envelope$privacy_class, "synthetic")
})
