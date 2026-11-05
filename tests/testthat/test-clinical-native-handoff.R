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
  testthat::expect_identical(initial$status, "endpoint_request_scheduled")
  context_present <- file.exists(context_path) && file.exists(context_envelope_path)
  testthat::expect_true(
    context_present,
    info = "Sol's C CLI wiring must persist the typed initial context before the run-C registry."
  )
  if (context_present) {
    context <- jsonlite::read_json(context_path, simplifyVector = FALSE)
    c_identity <- triomics_cnh_decode_r_value(context$c_input_identity)
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
  }
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

testthat::test_that("B refuses metadata changed after C without fitting or changing prior artifacts", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-metadata-guard-")
  metadata_path <- file.path(fixture$fixture_root, "data", "c08", "metadata.tsv")
  original_metadata <- readBin(metadata_path, what = "raw", n = file.info(metadata_path)$size)
  on.exit({
    writeBin(original_metadata, metadata_path)
    unlink(c(fixture$fixture_root, output_root), recursive = TRUE, force = TRUE)
  }, add = TRUE)

  initial <- triomics_execute_stage_c(fixture$config_path, output_root)
  testthat::expect_identical(initial$status, "endpoint_request_scheduled")
  request_paths <- file.path(output_root, "B", "clinical-score-association-requests",
                             c("request-01.json", "request-01.envelope.json"))
  request_hashes <- stats::setNames(vapply(request_paths, triomics_sha256, character(1L)),
                                    basename(request_paths))
  c_paths <- sort(list.files(file.path(output_root, "C"), recursive = TRUE,
                             full.names = TRUE, all.files = TRUE), method = "radix")
  c_hashes <- stats::setNames(vapply(c_paths, triomics_sha256, character(1L)),
                              substring(c_paths, nchar(output_root) + 2L))

  metadata <- utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE)
  control <- which(metadata$group == "control")[[1L]]
  treatment <- which(metadata$group == "treatment")[[1L]]
  metadata$group[c(control, treatment)] <- metadata$group[c(treatment, control)]
  utils::write.table(metadata, metadata_path, sep = "\t", row.names = FALSE, quote = FALSE)

  fit_calls <- 0L
  fit_spy <- function(...) {
    fit_calls <<- fit_calls + 1L
    stop("A Stage B fit ran after the declared metadata changed.", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    triomics_fit_block_models = fit_spy,
    triomics_associate_endpoints = fit_spy,
    .package = "triomics"
  )
  b_exit <- triomics_main(c(
    "--config", fixture$config_path, "--stage", "B", "--output", output_root
  ))

  testthat::expect_identical(b_exit, 2L)
  testthat::expect_identical(fit_calls, 0L)
  testthat::expect_identical(
    stats::setNames(vapply(request_paths, triomics_sha256, character(1L)), basename(request_paths)),
    request_hashes
  )
  c_paths_after <- sort(list.files(file.path(output_root, "C"), recursive = TRUE,
                                   full.names = TRUE, all.files = TRUE), method = "radix")
  testthat::expect_identical(
    stats::setNames(vapply(c_paths_after, triomics_sha256, character(1L)),
                    substring(c_paths_after, nchar(output_root) + 2L)),
    c_hashes
  )
  testthat::expect_false(file.exists(file.path(output_root, "B", "block-results.json")))
  testthat::expect_false(file.exists(file.path(output_root, "B", "clinical-associations.json")))

  writeBin(original_metadata, metadata_path)
  testthat::expect_s3_class(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root),
    "ClinicalNativeHandoffContext"
  )
})

testthat::test_that("B refuses an external registries symlink before fits and preserves same-root state", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-registry-symlink-")
  registry_path <- file.path(output_root, "registries")
  external_registry <- tempfile("triomics-cnh-external-registries-",
                                tmpdir = dirname(output_root))
  on.exit({
    if (triomics_is_symlink(registry_path)) unlink(registry_path)
    if (dir.exists(external_registry) && !dir.exists(registry_path)) {
      file.rename(external_registry, registry_path)
    }
    unlink(c(fixture$fixture_root, output_root, external_registry), recursive = TRUE, force = TRUE)
  }, add = TRUE)

  initial <- triomics_execute_stage_c(fixture$config_path, output_root)
  testthat::expect_identical(initial$status, "endpoint_request_scheduled")
  run_c_path <- file.path(registry_path, "run-C.json")
  run_c_hash <- triomics_sha256(run_c_path)
  request_paths <- file.path(output_root, "B", "clinical-score-association-requests",
                             c("request-01.json", "request-01.envelope.json"))
  request_hashes <- stats::setNames(vapply(request_paths, triomics_sha256, character(1L)),
                                    basename(request_paths))
  c_paths <- sort(list.files(file.path(output_root, "C"), recursive = TRUE,
                             full.names = TRUE, all.files = TRUE), method = "radix")
  c_hashes <- stats::setNames(vapply(c_paths, triomics_sha256, character(1L)),
                              substring(c_paths, nchar(output_root) + 2L))

  testthat::expect_true(file.rename(registry_path, external_registry))
  testthat::expect_true(file.symlink(external_registry, registry_path))
  fit_calls <- 0L
  fit_spy <- function(...) {
    fit_calls <<- fit_calls + 1L
    stop("A Stage B fit ran after registry ancestry escaped the output root.", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    triomics_fit_block_models = fit_spy,
    triomics_associate_endpoints = fit_spy,
    .package = "triomics"
  )
  b_exit <- triomics_main(c(
    "--config", fixture$config_path, "--stage", "B", "--output", output_root
  ))

  testthat::expect_identical(b_exit, 2L)
  testthat::expect_identical(fit_calls, 0L)
  testthat::expect_identical(triomics_sha256(file.path(external_registry, "run-C.json")), run_c_hash)
  testthat::expect_identical(
    stats::setNames(vapply(request_paths, triomics_sha256, character(1L)), basename(request_paths)),
    request_hashes
  )
  c_paths_after <- sort(list.files(file.path(output_root, "C"), recursive = TRUE,
                                   full.names = TRUE, all.files = TRUE), method = "radix")
  testthat::expect_identical(
    stats::setNames(vapply(c_paths_after, triomics_sha256, character(1L)),
                    substring(c_paths_after, nchar(output_root) + 2L)),
    c_hashes
  )
  testthat::expect_false(file.exists(file.path(output_root, "B", "block-results.json")))
  testthat::expect_false(file.exists(file.path(output_root, "B", "clinical-associations.json")))

  unlink(registry_path)
  testthat::expect_true(file.rename(external_registry, registry_path))
  testthat::expect_s3_class(
    triomics_cnh_prepare_stage_b(fixture$config_path, output_root),
    "ClinicalNativeHandoffContext"
  )
  testthat::expect_identical(triomics_sha256(run_c_path), run_c_hash)
})


testthat::test_that("saved C publication rejects rehashed identity and clinical evidence substitutions", {
  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-saved-context-authentication-")
  case_roots <- character()
  on.exit(unlink(c(fixture$fixture_root, output_root, case_roots),
                 recursive = TRUE, force = TRUE), add = TRUE)

  initial <- triomics_execute_stage_c(fixture$config_path, output_root)
  testthat::expect_identical(initial$status, "endpoint_request_scheduled")
  b_exit <- triomics_main(c(
    "--config", fixture$config_path, "--stage", "B", "--output", output_root
  ))
  testthat::expect_identical(b_exit, 0L)

  base_context_path <- file.path(output_root, "C", "native-publication-context.json")
  base_envelope_path <- file.path(output_root, "C", "native-publication-context.envelope.json")
  base_payload <- jsonlite::read_json(base_context_path, simplifyVector = FALSE)
  initial_handoff <- triomics_cnh_decode_r_value(base_payload$initial_handoff)
  testthat::expect_identical(initial_handoff$clinical_prediction$status, "not_started")
  testthat::expect_identical(initial_handoff$clinical_prediction$completion_policy, "conditional")
  testthat::expect_identical(initial_handoff$clinical_prediction$packet_id, "C-09")
  testthat::expect_setequal(
    vapply(initial_handoff$evaluation_evidence$controls,
           function(control) as.character(control$control), character(1L)),
    c("null", "weak_signal", "strong_signal", "adversarial")
  )
  testthat::expect_true(all(vapply(initial_handoff$evaluation_evidence$controls, function(control) {
    identical(control$candidate$status, "not_run") && identical(control$baseline$status, "not_run")
  }, logical(1L))))

  clone_root <- function(destination) {
    dir.create(destination, recursive = TRUE)
    entries <- list.files(output_root, recursive = TRUE, full.names = TRUE,
                          all.files = TRUE, no.. = TRUE)
    relative <- substring(entries, nchar(output_root) + 2L)
    directory <- file.info(entries)$isdir
    for (path in file.path(destination, relative[directory])) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    source_files <- entries[!directory]
    if (length(source_files)) {
      copied <- file.copy(source_files, file.path(destination, relative[!directory]),
                          overwrite = FALSE)
      stopifnot(all(copied))
    }
  }

  mutate_matrix <- function(payload) {
    identity <- triomics_cnh_decode_r_value(payload$c_input_identity)
    identity$blocks[[1L]]$matrix[[1L, 1L]] <- identity$blocks[[1L]]$matrix[[1L, 1L]] + 0.125
    payload$c_input_identity <- triomics_cnh_encode_r_value(identity)
    payload$c_input_identity_sha256 <- triomics_b07_prov_value_sha256(identity)
    payload
  }
  mutate_c09 <- function(payload) {
    handoff <- triomics_cnh_decode_r_value(payload$initial_handoff)
    handoff$clinical_prediction$status <- "complete"
    handoff$clinical_prediction$benefit_guaranteed <- TRUE
    payload$initial_handoff <- triomics_cnh_encode_r_value(handoff)
    payload
  }
  mutate_evaluation_status <- function(payload) {
    handoff <- triomics_cnh_decode_r_value(payload$initial_handoff)
    handoff$evaluation_evidence$status <- "complete"
    payload$initial_handoff <- triomics_cnh_encode_r_value(handoff)
    payload
  }
  mutate_control <- function(payload) {
    handoff <- triomics_cnh_decode_r_value(payload$initial_handoff)
    handoff$evaluation_evidence$controls[[1L]]$candidate$status <- "complete"
    handoff$evaluation_evidence$controls[[1L]]$candidate$artifact_ref <- "c07-candidate"
    handoff$evaluation_evidence$controls[[1L]]$candidate$artifact_sha256 <- paste(rep("a", 64L), collapse = "")
    handoff$evaluation_evidence$source_artifacts <- list(list(
      artifact_id = "c07-candidate", sha256 = paste(rep("a", 64L), collapse = "")
    ))
    payload$initial_handoff <- triomics_cnh_encode_r_value(handoff)
    payload
  }
  mutations <- list(
    identity_matrix = mutate_matrix,
    c09_completion_claim = mutate_c09,
    evaluation_completion_claim = mutate_evaluation_status,
    c07_control_completion_claim = mutate_control
  )
  for (case_name in names(mutations)) {
    case_root <- tempfile(paste0("triomics-cnh-context-case-", case_name, "-"))
    case_roots <- c(case_roots, case_root)
    clone_root(case_root)
    context_path <- file.path(case_root, "C", "native-publication-context.json")
    envelope_path <- file.path(case_root, "C", "native-publication-context.envelope.json")
    request_paths <- file.path(case_root, "B", "clinical-score-association-requests",
                               c("request-01.json", "request-01.envelope.json"))
    request_hashes <- stats::setNames(vapply(request_paths, triomics_sha256, character(1L)),
                                      basename(request_paths))
    payload <- jsonlite::read_json(context_path, simplifyVector = FALSE)
    payload <- mutations[[case_name]](payload)
    jsonlite::write_json(payload, context_path, auto_unbox = TRUE, null = "null",
                         pretty = TRUE, digits = NA)
    envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)
    envelope$payload_sha256 <- triomics_sha256(context_path)
    jsonlite::write_json(envelope, envelope_path, auto_unbox = TRUE, null = "null",
                         pretty = TRUE, digits = NA)
    c_paths_before <- sort(list.files(file.path(case_root, "C"), recursive = TRUE,
                                      full.names = TRUE, all.files = TRUE), method = "radix")
    c_hashes_before <- stats::setNames(vapply(c_paths_before, triomics_sha256, character(1L)),
                                       substring(c_paths_before, nchar(case_root) + 2L))

    error <- testthat::capture_error(
      triomics_cnh_publish_saved_c(fixture$config_path, case_root)
    )
    testthat::expect_identical(error$code, "C_PUBLICATION_CONTEXT_INVALID", info = case_name)
    testthat::expect_false(file.exists(file.path(case_root, "D", "stage-handoff-provenance.json")),
                           info = case_name)
    testthat::expect_identical(
      stats::setNames(vapply(request_paths, triomics_sha256, character(1L)), basename(request_paths)),
      request_hashes, info = case_name
    )
    c_paths_after <- sort(list.files(file.path(case_root, "C"), recursive = TRUE,
                                     full.names = TRUE, all.files = TRUE), method = "radix")
    testthat::expect_identical(
      stats::setNames(vapply(c_paths_after, triomics_sha256, character(1L)),
                      substring(c_paths_after, nchar(case_root) + 2L)),
      c_hashes_before, info = case_name
    )
  }
})

testthat::test_that("failed C handoff cleans an after-write envelope and preserves previous D state", {
  d_config <- triomics_test_resource("examples", "tickets", "D-T08.yaml")
  previous_d_root <- tempfile("triomics-cnh-previous-d-generation-")
  on.exit(unlink(previous_d_root, recursive = TRUE, force = TRUE), add = TRUE)
  for (stage in c("B", "C", "D")) {
    exit <- triomics_main(c(
      "--config", d_config, "--stage", stage, "--output", previous_d_root
    ))
    testthat::expect_identical(exit, 0L, info = paste("public D-T08", stage))
    if (!identical(exit, 0L)) return(invisible(NULL))
  }
  previous_pointer <- file.path(previous_d_root, "registries", "current-D.json")
  testthat::expect_true(file.exists(previous_pointer))
  pointer <- jsonlite::read_json(previous_pointer, simplifyVector = FALSE)
  previous_generation <- dirname(file.path(previous_d_root, pointer$generation_manifest$path))
  testthat::expect_true(dir.exists(previous_generation))
  previous_manifest <- file.path(previous_generation, "generation-manifest.json")
  testthat::expect_true(file.exists(previous_manifest))
  testthat::expect_identical(triomics_sha256(previous_manifest), as.character(pointer$generation_id))
  testthat::expect_true(is.list(triomics_d_validate_generation(
    previous_d_root, as.character(pointer$generation_id)
  )))

  fixture <- cnh_test_between_person_config()
  output_root <- tempfile("triomics-cnh-after-write-rollback-")
  on.exit(unlink(c(fixture$fixture_root, output_root), recursive = TRUE, force = TRUE), add = TRUE)
  initial <- triomics_execute_stage_c(fixture$config_path, output_root)
  testthat::expect_identical(initial$status, "endpoint_request_scheduled")
  b_exit <- triomics_main(c(
    "--config", fixture$config_path, "--stage", "B", "--output", output_root
  ))
  testthat::expect_identical(b_exit, 0L)

  copied_generation_parent <- file.path(output_root, "D", "generations")
  dir.create(copied_generation_parent, recursive = TRUE, showWarnings = FALSE)
  testthat::expect_true(file.copy(previous_generation, copied_generation_parent, recursive = TRUE))
  copied_pointer <- file.path(output_root, "registries", "current-D.json")
  testthat::expect_true(file.copy(previous_pointer, copied_pointer))
  previous_pointer_hash <- triomics_sha256(copied_pointer)
  copied_generation <- file.path(output_root, pointer$generation_manifest$path)
  generation_dir <- dirname(copied_generation)
  generation_files <- sort(list.files(generation_dir, recursive = TRUE, full.names = TRUE,
                                      all.files = TRUE), method = "radix")
  generation_files <- generation_files[!file.info(generation_files)$isdir]
  generation_hashes <- stats::setNames(vapply(generation_files, triomics_sha256, character(1L)),
                                       substring(generation_files, nchar(generation_dir) + 2L))
  c_paths <- sort(list.files(file.path(output_root, "C"), recursive = TRUE,
                             full.names = TRUE, all.files = TRUE), method = "radix")
  c_hashes <- stats::setNames(vapply(c_paths, triomics_sha256, character(1L)),
                              substring(c_paths, nchar(output_root) + 2L))
  request_paths <- file.path(output_root, "B", "clinical-score-association-requests",
                             c("request-01.json", "request-01.envelope.json"))
  request_hashes <- stats::setNames(vapply(request_paths, triomics_sha256, character(1L)),
                                    basename(request_paths))

  reached <- FALSE
  original_write_envelope <- triomics_write_envelope
  after_profile_envelope <- function(envelope, path) {
    result <- original_write_envelope(envelope, path)
    if (identical(basename(path), "stage-handoff-provenance.envelope.json")) {
      reached <<- TRUE
      stop("Injected after the C provenance envelope write.", call. = FALSE)
    }
    result
  }
  testthat::local_mocked_bindings(
    triomics_write_envelope = after_profile_envelope,
    .package = "triomics"
  )
  error <- testthat::capture_error(
    triomics_cnh_publish_saved_c(fixture$config_path, output_root)
  )

  testthat::expect_true(reached)
  testthat::expect_match(conditionMessage(error), "Injected after the C provenance envelope write")
  testthat::expect_identical(triomics_sha256(copied_pointer), previous_pointer_hash)
  files_after <- sort(list.files(generation_dir, recursive = TRUE, full.names = TRUE,
                                 all.files = TRUE), method = "radix")
  files_after <- files_after[!file.info(files_after)$isdir]
  testthat::expect_identical(
    stats::setNames(vapply(files_after, triomics_sha256, character(1L)),
                    substring(files_after, nchar(generation_dir) + 2L)),
    generation_hashes
  )
  testthat::expect_identical(
    stats::setNames(vapply(request_paths, triomics_sha256, character(1L)), basename(request_paths)),
    request_hashes
  )
  c_paths_after <- sort(list.files(file.path(output_root, "C"), recursive = TRUE,
                                   full.names = TRUE, all.files = TRUE), method = "radix")
  testthat::expect_identical(
    stats::setNames(vapply(c_paths_after, triomics_sha256, character(1L)),
                    substring(c_paths_after, nchar(output_root) + 2L)),
    c_hashes
  )
  c06_outputs <- file.path(output_root, "D", c(
    "candidate-nominations.json", "candidate-nominations.envelope.json",
    "integration-evidence.json", "integration-evidence.envelope.json",
    "integration-comparisons.json", "integration-comparisons.envelope.json",
    "evaluation-evidence.json", "evaluation-evidence.envelope.json",
    "stage-status.json", "stage-status.envelope.json",
    "handoff-manifest.json", "handoff-manifest.envelope.json",
    "stage-handoff-provenance.json", "stage-handoff-provenance.envelope.json"
  ))
  testthat::expect_false(any(file.exists(c06_outputs) | dir.exists(c06_outputs)))
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

  dictionary_path <- file.path(fixture$fixture_root, "data", "c08", "dictionary.tsv")
  dictionary <- utils::read.delim(dictionary_path, check.names = FALSE,
                                  stringsAsFactors = FALSE, colClasses = "character")
  dictionary$missing_map[dictionary$variable_id == "glucose"] <- ".=placeholder;CUSTOM=custom_reason"
  null_neighbor <- dictionary[1L, , drop = FALSE]
  null_neighbor[,] <- ""
  null_neighbor$variable_id <- "empty_missingness_neighbor"
  null_neighbor$label <- "Synthetic empty missingness neighbor"
  null_neighbor$type <- "continuous"
  null_neighbor$units <- "unitless"
  null_neighbor$units_verification <- "verified"
  null_neighbor$role <- "administrative"
  null_neighbor$allowed_model_roles <- "excluded"
  null_neighbor$time_semantics <- "visit"
  null_neighbor$privacy <- "ordinary"
  null_neighbor$verification <- "verified"
  null_neighbor$evidence <- "Synthetic test-only dictionary entry."
  null_neighbor$censoring <- "forbidden"
  null_neighbor$time_format <- "none"
  dictionary <- rbind(dictionary, null_neighbor)
  utils::write.table(dictionary, dictionary_path, sep = "\t", row.names = FALSE,
                     quote = FALSE)

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
  testthat::expect_length(payload$clinical_dictionary_coverage$variables, 2L)
  variable <- payload$clinical_dictionary_coverage$variables[[1L]]
  testthat::expect_setequal(names(variable), c(
    "variable_id", "declared_type", "unit", "units_verification", "role",
    "time_semantics", "availability", "verification", "modelable", "missingness_rules"
  ))
  coverage <- payload$clinical_dictionary_coverage$coverage[[1L]]
  testthat::expect_identical(as.integer(coverage$n_subjects), 12L)
  testthat::expect_identical(as.integer(coverage$n_observed), 12L)
  testthat::expect_identical(variable$missingness_rules$mapped_dispositions, list("placeholder"))
  testthat::expect_identical(as.integer(variable$missingness_rules$unsupported_disposition_count), 1L)
  testthat::expect_identical(
    payload$clinical_dictionary_coverage$variables[[2L]]$variable_id,
    "empty_missingness_neighbor"
  )
  testthat::expect_null(payload$clinical_dictionary_coverage$variables[[2L]]$missingness_rules)
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
