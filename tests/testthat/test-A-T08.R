expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

new_dir <- function(prefix) {
  path <- tempfile(prefix)
  dir.create(path)
  path
}

dict_row <- function(variable_id, label, type, units, units_verification, role,
                     allowed_model_roles, time_semantics, privacy, verification,
                     codebook = "", missing_map = "", censoring = "forbidden",
                     value_separator = "", time_format = "none") {
  c(
    variable_id = variable_id, label = label, type = type, units = units,
    units_verification = units_verification, role = role,
    allowed_model_roles = allowed_model_roles, time_semantics = time_semantics,
    privacy = privacy, verification = verification,
    evidence = "Synthetic fixture; no participant data.",
    codebook = codebook, missing_map = missing_map, censoring = censoring,
    value_separator = value_separator, time_format = time_format
  )
}

a_t08_dictionary <- function() {
  list(
    dict_row("glucose", "Glucose", "continuous", "mmol/L", "verified", "measured",
             "predictor", "visit", "ordinary", "verified",
             missing_map = ".=placeholder", time_format = "iso8601_date"),
    dict_row("mrn", "Record number", "text", "inapplicable", "inapplicable",
             "administrative", "excluded", "subject_static", "identifier", "verified")
  )
}

clinical_row <- function(subject_id, visit_id, sample_id, time, glucose, mrn) {
  c(
    subject_id = subject_id, visit_id = visit_id, sample_id = sample_id,
    time = time, time_unit = "day", glucose = glucose, mrn = mrn
  )
}

link_row <- function(link_id, subject_id, visit_id, sample_id) {
  c(
    link_id = link_id, subject_id = subject_id, visit_id = visit_id, sample_id = sample_id,
    cardinality = "one_to_one", match_rule = "exact_sample", reference_time = "",
    window_days = "", tie_rule = ""
  )
}

objective_of <- function(objective_id, predictor_ref) {
  list(
    objective_id = objective_id, analysis_id = "synthetic_primary",
    mode = "between_person", response_ref = "synthetic_endpoint",
    predictor_ref = predictor_ref, endpoint_ref = "synthetic_endpoint",
    adjustments = list(), time_window_ref = NULL,
    missingness_policy = "complete_case", family_ref = "clinical_family",
    hypothesis_status = "exploratory", protocol_ref = "synthetic-a-t08"
  )
}

clinical_bundle <- function(design) {
  if (identical(design, "independent")) {
    observations <- list(
      clinical_row("C1", "V1", "sim0001", "2020-01-01", "5.5", "SECRET-MRN-100"),
      clinical_row("C2", "V1", "sim0002", "2020-01-03", "4.25", "SECRET-MRN-200")
    )
    links <- list(
      link_row("L1", "C1", "V1", "sim0001"),
      link_row("L2", "C2", "V1", "sim0002")
    )
    tokens <- c("4.25", "5.5")
    samples <- c("sim0001", "sim0002")
  } else {
    observations <- list(
      clinical_row("sub0001", "a", "sim0001_a", "2020-01-01", "5.5", "SECRET-MRN-100"),
      clinical_row("sub0001", "b", "sim0001_b", "2020-06-01", "6.5", "SECRET-MRN-100"),
      clinical_row("sub0002", "a", "sim0002_a", "2020-01-03", "4.25", "SECRET-MRN-200"),
      clinical_row("sub0002", "b", "sim0002_b", "2020-06-03", "3.25", "SECRET-MRN-200")
    )
    links <- list(
      link_row("L1", "sub0001", "a", "sim0001_a"),
      link_row("L2", "sub0001", "b", "sim0001_b"),
      link_row("L3", "sub0002", "a", "sim0002_a"),
      link_row("L4", "sub0002", "b", "sim0002_b")
    )
    tokens <- c("3.25", "4.25", "5.5", "6.5")
    samples <- c("sim0001_a", "sim0001_b", "sim0002_a", "sim0002_b")
  }
  list(
    dictionary = a_t08_dictionary(),
    observations = observations,
    links = links,
    objectives = list(objective_of("glucose_between", "glucose"), objective_of("mrn_between", "mrn")),
    glucose_tokens = tokens,
    sample_ids = samples
  )
}

base_request <- function(design, study_id, clinical = TRUE) {
  features <- c(prot = 2L, lip = 2L)
  profiles <- c(prot = "proteomics", lip = "lipidomics")
  list(
    study_id = study_id,
    design = design,
    data_class = "synthetic",
    seed = 20261012,
    n_subjects = 2,
    n_features = features,
    profiles = profiles,
    display_names = c(prot = "proteomics", lip = "not-the-profile"),
    capabilities = c("ingest", "prepare", "clinical", "export"),
    preparation_context_id = "a-t08",
    version = "0.1.0",
    config_token = "fixture",
    clinical = if (isTRUE(clinical)) clinical_bundle(design) else NULL,
    planted = list(
      prot = list(list(feature = 1L, sample = 1L, token = "3.5")),
      lip = list(list(feature = 1L, sample = 1L, token = "7.25"))
    )
  )
}

without_clinical <- function(request) {
  request$capabilities <- c("ingest", "prepare", "export")
  request$clinical <- NULL
  request
}

artifact_where <- function(result, role, block_id = NULL) {
  hits <- Filter(function(item) {
    identical(item$role, role) && identical(item$block_id, block_id)
  }, result$artifacts)
  testthat::expect_length(hits, 1L)
  hits[[1L]]
}

read_export_json <- function(path) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}

cell_token <- function(path, row, column) {
  table <- triomics_read_delimited(path, "tsv")
  table$rows[[row]][[column]]
}

decision_candidate <- function(status, key, payload_sha, manifest_sha = payload_sha,
                               payload_present = TRUE, envelope_present = TRUE) {
  list(
    status = status,
    cache_key = key,
    payload_sha256 = payload_sha,
    manifest_payload_sha256 = manifest_sha,
    payload_present = payload_present,
    envelope_present = envelope_present
  )
}

extension_methods <- function() {
  methods <- triomics_baseline_profile()
  methods$transform <- function(se, params) {
    if (!identical(params$operation$method, "shift_one")) {
      triomics_abort(
        "A_CONFIG_METHOD", "The extension profile was not given its declared method.",
        key = "transform", module = "export_a", stage = "A",
        remediation = "Dispatch the registered profile. Do not substitute a built-in method."
      )
    }
    if (!is.null(params$display_name) && identical(params$display_name, params$profile_id)) {
      triomics_abort(
        "A_CONFIG_PROFILE", "The extension profile branched on a display name.",
        key = "display_name", module = "export_a", stage = "A",
        remediation = "Keep display names out of method dispatch."
      )
    }
    mat <- SummarizedExperiment::assay(se, "analysis")
    finite <- is.finite(mat)
    mat[finite] <- mat[finite] + 1
    SummarizedExperiment::assay(se, "analysis") <- mat
    triomics_record_operation(se, params$operation, triomics_meta(se)$scale, triomics_meta(se)$scale)
  }
  methods
}

shift_operations <- function() {
  ops <- list(
    list(stage = "validate", method = "identity"),
    list(stage = "filter", method = "none", min_observed = NULL, prevalence = NULL),
    list(stage = "derive", method = "none"),
    list(stage = "transform", method = "shift_one", zero_policy = "error", zero_value = NULL),
    list(stage = "normalise", method = "none"),
    list(stage = "impute", method = "none")
  )
  ops
}

expect_lineage <- function(result, design) {
  clinical <- clinical_bundle(design)
  simulator <- if (identical(design, "independent")) "two_group" else "paired"
  triomics_use_execution_context(result$seed, result$study_id)
  simulated <- simulate_study(
    2,
    n_features = c(prot = 2L, lip = 2L),
    design = simulator,
    effect_size = 0,
    prop_affected = 0,
    missingness = 0,
    shared_factor_strength = 0
  )
  testthat::expect_identical(result$status, "complete")
  testthat::expect_identical(result$design, design)
  testthat::expect_identical(result$simulator_design, simulator)
  testthat::expect_identical(result$capabilities$ingest, "complete")
  testthat::expect_identical(result$capabilities$prepare, "complete")
  testthat::expect_identical(result$capabilities$clinical, "complete")
  testthat::expect_identical(result$capabilities$export, "complete")
  testthat::expect_identical(colnames(result$assays$prot), simulated$study$metadata$sample_id)
  testthat::expect_identical(colnames(result$assays$lip), simulated$study$metadata$sample_id)
  if (identical(design, "independent")) {
    testthat::expect_length(simulated$study$metadata$sample_id, 2L)
    subjects <- vapply(result$samples, function(sample) {
      if (is.null(sample$subject_id)) NA_character_ else as.character(sample$subject_id)
    }, character(1))
    testthat::expect_true(all(is.na(subjects)))
    groups <- vapply(result$samples, function(sample) as.character(sample$group), character(1))
    testthat::expect_identical(sort(groups), c("control", "treatment"))
  } else {
    testthat::expect_length(simulated$study$metadata$sample_id, 4L)
    subjects <- vapply(result$samples, function(sample) as.character(sample$subject_id), character(1))
    testthat::expect_identical(sort(subjects), c("sub0001", "sub0001", "sub0002", "sub0002"))
    visits <- vapply(result$samples, function(sample) as.character(sample$visit_id), character(1))
    testthat::expect_identical(sort(visits), c("a", "a", "b", "b"))
  }
  source_token <- cell_token(result$files$source$prot, 2L, 3L)
  expected_token <- formatC(simulated$study$blocks$prot$assay[2, 2], format = "fg", digits = 15)
  testthat::expect_identical(source_token, expected_token)
  testthat::expect_identical(cell_token(result$files$source$prot, 1L, 2L), "3.5")
  testthat::expect_equal(result$assays$prot[1, 1], 3.5, tolerance = 1e-12)
  testthat::expect_equal(result$assays$prot[2, 2], as.numeric(expected_token), tolerance = 1e-12)
  testthat::expect_equal(result$assays$lip[1, 1], 7.25, tolerance = 1e-12)
  expected_keys <- c(
    triomics_feature_key(result$study_id, "prot", "1"),
    triomics_feature_key(result$study_id, "prot", "2")
  )
  testthat::expect_identical(result$features$prot$feature_keys, expected_keys)
  testthat::expect_identical(rownames(result$assays$prot), expected_keys)
  source <- artifact_where(result, "source", "prot")
  feature <- artifact_where(result, "feature", "prot")
  state <- artifact_where(result, "state", "prot")
  testthat::expect_identical(source$file_sha256, triomics_sha256(result$files$source$prot))
  feature_envelope <- read_export_json(feature$envelope_path)
  testthat::expect_identical(feature_envelope$inputs[[1L]]$artifact_id, source$artifact_id)
  testthat::expect_identical(feature_envelope$inputs[[1L]]$sha256, source$payload_sha256)
  testthat::expect_identical(feature_envelope$payload_sha256, feature$payload_sha256)
  state_envelope <- read_export_json(state$envelope_path)
  testthat::expect_identical(state_envelope$inputs[[1L]]$artifact_id, feature$artifact_id)
  testthat::expect_identical(state_envelope$inputs[[1L]]$sha256, feature$payload_sha256)
  loaded <- triomics_read_state(result$state_refs$prot, result$state_dir)
  testthat::expect_identical(loaded$state_hash, state$state_hash)
  testthat::expect_identical(as.character(loaded$feature_keys), expected_keys)
  testthat::expect_identical(as.character(loaded$training_unit_ids), colnames(result$assays$prot))
  testthat::expect_identical(as.character(loaded$profile_id), "proteomics")
  stages <- vapply(loaded$operation_order, function(operation) as.character(operation$stage), character(1))
  testthat::expect_identical(stages, c("validate", "filter", "derive", "transform", "normalise", "impute"))
  lip_state <- triomics_read_state(result$state_refs$lip, result$state_dir)
  testthat::expect_identical(as.character(lip_state$profile_id), "lipidomics")
  testthat::expect_false(identical(as.character(lip_state$profile_id), "not-the-profile"))
  testthat::expect_identical(result$clinical$glucose_tokens, clinical$glucose_tokens)
  testthat::expect_identical(result$clinical$predictor_variable_ids, "glucose")
  testthat::expect_identical(sort(result$clinical$approved_sample_ids), sort(clinical$sample_ids))
  objectives <- result$clinical$objectives
  glucose <- Filter(function(item) identical(item$predictor_ref, "glucose"), objectives)[[1L]]
  mrn <- Filter(function(item) identical(item$predictor_ref, "mrn"), objectives)[[1L]]
  testthat::expect_identical(glucose$status, "declared_not_fitted")
  testthat::expect_false(isTRUE(glucose$fitted))
  testthat::expect_null(glucose$p)
  testthat::expect_null(glucose$effect)
  testthat::expect_identical(mrn$status, "unavailable")
  testthat::expect_identical(mrn$reason, "privacy_excluded")
  testthat::expect_false(isTRUE(mrn$fitted))
  observations <- paste(readLines(result$clinical$observations_path, warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl("SECRET-MRN", observations, fixed = TRUE))
  testthat::expect_true(grepl("5.5", observations, fixed = TRUE))
  export_files <- list.files(file.path(result$output, "export"), recursive = TRUE, full.names = TRUE)
  export_text <- paste(unlist(lapply(export_files, readLines, warn = FALSE)), collapse = "\n")
  testthat::expect_false(grepl("SECRET-MRN", export_text, fixed = TRUE))
  clinical_artifact <- artifact_where(result, "clinical", NULL)
  clinical_envelope <- read_export_json(clinical_artifact$envelope_path)
  observation_input <- Filter(function(item) identical(item$artifact_id, "clinical-observations-source"),
                              clinical_envelope$inputs)[[1L]]
  testthat::expect_identical(observation_input$sha256, result$clinical$observations_sha256)
  testthat::expect_identical(observation_input$sha256, triomics_sha256(result$files$clinical_observations))
  handoff <- artifact_where(result, "handoff", NULL)
  handoff_envelope <- read_export_json(handoff$envelope_path)
  child_ids <- vapply(handoff_envelope$inputs, function(item) item$artifact_id, character(1))
  testthat::expect_true(feature$artifact_id %in% child_ids)
  testthat::expect_true(state$artifact_id %in% child_ids)
  testthat::expect_true(clinical_artifact$artifact_id %in% child_ids)
  on_disk <- read_export_json(result$handoff_path)
  testthat::expect_identical(on_disk$limitations$scientific_claim, "not_established")
  testthat::expect_true(isTRUE(on_disk$limitations$evidential_levels$source_validation$established))
  testthat::expect_false(isTRUE(on_disk$limitations$evidential_levels$real_data_scientific_validation$established))
  testthat::expect_false(isTRUE(on_disk$limitations$evidential_levels$optional_method_qualification$established))
  invisible(result)
}

testthat::test_that("A-T08-AC4 handoff separates source validation, scientific validation, and qualification", {
  handoff <- triomics_a_handoff_limitations(list())
  levels <- handoff$evidential_levels
  testthat::expect_identical(levels$source_validation$name, "source_validation")
  testthat::expect_identical(levels$real_data_scientific_validation$name, "real_data_scientific_validation")
  testthat::expect_identical(levels$optional_method_qualification$name, "optional_method_qualification")
  testthat::expect_true(isTRUE(levels$source_validation$established))
  testthat::expect_false(isTRUE(levels$real_data_scientific_validation$established))
  testthat::expect_false(isTRUE(levels$optional_method_qualification$established))
  testthat::expect_identical(levels$source_validation$status, "exercised_on_synthetic_fixtures")
  testthat::expect_identical(levels$real_data_scientific_validation$status, "not_established")
  testthat::expect_identical(levels$optional_method_qualification$status, "not_qualified")
  testthat::expect_false(identical(levels$source_validation$status, levels$real_data_scientific_validation$status))
  testthat::expect_false(identical(levels$real_data_scientific_validation$status,
                                   levels$optional_method_qualification$status))
  testthat::expect_match(levels$source_validation$meaning, "synthetic")
  testthat::expect_match(levels$real_data_scientific_validation$meaning, "do not establish")
  testthat::expect_match(levels$optional_method_qualification$meaning, "does not invent")
  testthat::expect_identical(handoff$scientific_claim, "not_established")
  testthat::expect_null(handoff$validated)
  methods <- levels$optional_method_qualification$methods
  method_ids <- vapply(methods, function(item) item$method_id, character(1))
  statuses <- vapply(methods, function(item) item$status, character(1))
  testthat::expect_identical(sort(method_ids), c("internal_standard", "knn", "minprob", "qrilc", "rf", "vsn"))
  testthat::expect_true(all(statuses == "not_qualified"))
  testthat::expect_false("log2" %in% method_ids)
  testthat::expect_false("none" %in% method_ids)
  expect_code(
    triomics_a_handoff_limitations(list(claim_scientific_validation = TRUE)),
    "A_EXPORT_CLAIM"
  )
  expect_code(
    triomics_a_handoff_limitations(list(
      qualifications = list(list(method_id = "minprob", status = "qualified"))
    )),
    "A_EXPORT_CLAIM"
  )
  request <- base_request("independent", "a_t08_claim")
  request$claim_method_qualification <- TRUE
  claim_dir <- new_dir("a-t08-claim-")
  expect_code(triomics_export_a(request, claim_dir), "A_EXPORT_CLAIM")
  claim_files <- list.files(claim_dir, recursive = TRUE)
  testthat::expect_false(any(grepl("handoff-.*\\.envelope\\.json$", claim_files)))
})

testthat::test_that("A-T08-AC3 resume accepts only a complete matching cache key", {
  key <- triomics_export_cache_key(list(role = "source", version = "0.1.0", source_sha256 = paste(rep("ab", 32), collapse = "")))
  again <- triomics_export_cache_key(list(source_sha256 = paste(rep("ab", 32), collapse = ""), version = "0.1.0", role = "source"))
  changed <- triomics_export_cache_key(list(role = "source", version = "0.1.1", source_sha256 = paste(rep("ab", 32), collapse = "")))
  testthat::expect_identical(key, again)
  testthat::expect_false(identical(key, changed))
  testthat::expect_match(key, "^[0-9a-f]{64}$")
  payload_sha <- paste(rep("cd", 32), collapse = "")
  matched <- triomics_export_resume_decision(decision_candidate("complete", key, payload_sha), key)
  testthat::expect_true(matched$reuse)
  testthat::expect_identical(matched$reason, "complete_match")
  partial <- triomics_export_resume_decision(
    decision_candidate("complete", key, payload_sha, envelope_present = FALSE),
    key
  )
  testthat::expect_false(partial$reuse)
  testthat::expect_identical(partial$reason, "partial")
  incomplete <- triomics_export_resume_decision(decision_candidate("partial", key, payload_sha), key)
  testthat::expect_false(incomplete$reuse)
  testthat::expect_identical(incomplete$reason, "incomplete")
  not_requested <- triomics_export_resume_decision(decision_candidate("not_requested", key, payload_sha), key)
  testthat::expect_false(not_requested$reuse)
  testthat::expect_identical(not_requested$reason, "incomplete")
  invalidated <- triomics_export_resume_decision(decision_candidate("complete", key, payload_sha), changed)
  testthat::expect_false(invalidated$reuse)
  testthat::expect_identical(invalidated$reason, "invalidated")
  mismatched <- triomics_export_resume_decision(
    decision_candidate("complete", key, payload_sha, manifest_sha = paste(rep("ef", 32), collapse = "")),
    key
  )
  testthat::expect_false(mismatched$reuse)
  testthat::expect_identical(mismatched$reason, "hash_mismatch")
  absent <- triomics_export_resume_decision(NULL, key)
  testthat::expect_false(absent$reuse)
  testthat::expect_identical(absent$reason, "absent")
})

testthat::test_that("A-T08-AC1 independent and paired exports keep source, feature, state, and clinical lineage", {
  independent <- triomics_export_a(base_request("independent", "a_t08_independent"), new_dir("a-t08-ind-"))
  paired <- triomics_export_a(base_request("paired", "a_t08_paired"), new_dir("a-t08-paired-"))
  expect_lineage(independent, "independent")
  expect_lineage(paired, "paired")
  quiet <- triomics_export_a(without_clinical(base_request("independent", "a_t08_noclinical")), new_dir("a-t08-noclin-"))
  testthat::expect_identical(quiet$capabilities$clinical, "not_requested")
  testthat::expect_identical(quiet$clinical$clinical_status, "not_requested")
  testthat::expect_length(quiet$clinical$predictor_variable_ids, 0L)
  testthat::expect_length(quiet$clinical$glucose_tokens, 0L)
  testthat::expect_false(identical(quiet$capabilities$clinical, "complete"))
  omitted <- paste(readLines(quiet$handoff_path, warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl("SECRET-MRN", omitted, fixed = TRUE))
  testthat::expect_false(grepl("5\\.5", omitted))
  bad <- base_request("independent", "a_t08_limma")
  bad$capabilities <- c("ingest", "limma")
  bad_dir <- new_dir("a-t08-limma-")
  expect_code(triomics_export_a(bad, bad_dir), "A_EXPORT_CAPABILITY")
  testthat::expect_false(any(grepl("index-.*\\.envelope\\.json$", list.files(bad_dir, recursive = TRUE))))
  private_dir <- new_dir("a-t08-private-")
  private <- base_request("independent", "a_t08_private")
  private$data_class <- "participant"
  expect_code(triomics_export_a(private, private_dir), "A_EXPORT_PRIVACY")
  expect_code(
    triomics_export_a(base_request("case_control", "a_t08_design"), new_dir("a-t08-design-")),
    "A_EXPORT_DESIGN"
  )
})

testthat::test_that("A-T08-AC2 fourth and fifth profiles use the registry without a new orchestrator", {
  before <- triomics_stage_registry()
  triomics_reset_profiles()
  triomics_register_profile("ion_mobility", extension_methods())
  triomics_register_profile("glycomics", extension_methods())
  request <- base_request("independent", "a_t08_five")
  request$n_features <- c(prot = 1L, lip = 1L, met = 1L, ion = 1L, gly = 1L)
  request$profiles <- c(
    prot = "proteomics", lip = "lipidomics", met = "metabolomics",
    ion = "ion_mobility", gly = "glycomics"
  )
  request$display_names <- c(
    prot = "proteomics", lip = "lipidomics", met = "metabolomics",
    ion = "not-a-profile", gly = "proteomics"
  )
  request$planted <- list(
    prot = list(list(feature = 1L, sample = 1L, token = "3.5")),
    lip = list(list(feature = 1L, sample = 1L, token = "3.5")),
    met = list(list(feature = 1L, sample = 1L, token = "3.5")),
    ion = list(list(feature = 1L, sample = 1L, token = "3.5")),
    gly = list(list(feature = 1L, sample = 1L, token = "3.5"))
  )
  request$operations <- list(ion = shift_operations(), gly = shift_operations())
  request$clinical <- NULL
  request$capabilities <- c("ingest", "prepare", "export")
  exported <- triomics_export_a(request, new_dir("a-t08-five-"))
  after <- triomics_stage_registry()
  testthat::expect_identical(before$stage, after$stage)
  testthat::expect_identical(before$execution, after$execution)
  testthat::expect_identical(before$validation_only, after$validation_only)
  testthat::expect_identical(names(exported$assays), c("prot", "lip", "met", "ion", "gly"))
  testthat::expect_equal(exported$assays$prot[1, 1], 3.5, tolerance = 1e-12)
  testthat::expect_equal(exported$assays$lip[1, 1], 3.5, tolerance = 1e-12)
  testthat::expect_equal(exported$assays$met[1, 1], 3.5, tolerance = 1e-12)
  testthat::expect_equal(exported$assays$ion[1, 1], 4.5, tolerance = 1e-12)
  testthat::expect_equal(exported$assays$gly[1, 1], 4.5, tolerance = 1e-12)
  ion_state <- triomics_read_state(exported$state_refs$ion, exported$state_dir)
  gly_state <- triomics_read_state(exported$state_refs$gly, exported$state_dir)
  prot_state <- triomics_read_state(exported$state_refs$prot, exported$state_dir)
  testthat::expect_identical(as.character(ion_state$profile_id), "ion_mobility")
  testthat::expect_identical(as.character(gly_state$profile_id), "glycomics")
  testthat::expect_false(identical(as.character(gly_state$profile_id), "proteomics"))
  testthat::expect_identical(as.character(prot_state$profile_id), "proteomics")
  gly_methods <- vapply(gly_state$operation_order, function(operation) as.character(operation$method), character(1))
  prot_methods <- vapply(prot_state$operation_order, function(operation) as.character(operation$method), character(1))
  testthat::expect_identical(gly_methods[[4L]], "shift_one")
  testthat::expect_false(identical(prot_methods[[4L]], "shift_one"))
  missing <- request
  missing$profiles[["gly"]] <- "not_registered_panel"
  expect_code(triomics_export_a(missing, new_dir("a-t08-missing-profile-")), "A_CONFIG_PROFILE")
  partial_methods <- extension_methods()
  partial_methods$derive <- NULL
  expect_code(triomics_register_profile("partial_panel", partial_methods), "A_CONFIG_PROFILE")
})

testthat::test_that("A-T08-AC3 changed source or version invalidates only affected descendants", {
  request <- without_clinical(base_request("independent", "a_t08_resume"))
  output <- new_dir("a-t08-resume-")
  first <- triomics_export_a(request, output, resume = FALSE)
  testthat::expect_true(all(vapply(first$artifacts, function(item) identical(item$reused, FALSE), logical(1))))
  second <- triomics_export_a(request, output, resume = TRUE)
  testthat::expect_true(all(vapply(second$artifacts, function(item) isTRUE(item$reused), logical(1))))
  testthat::expect_identical(second$cache_key, first$cache_key)
  testthat::expect_identical(second$features$prot$feature_keys, first$features$prot$feature_keys)
  testthat::expect_equal(second$assays$prot, first$assays$prot, tolerance = 1e-12)
  testthat::expect_identical(artifact_where(second, "state", "prot")$state_hash,
                             artifact_where(first, "state", "prot")$state_hash)

  interrupted <- new_dir("a-t08-interrupt-")
  expect_code(
    triomics_export_a(request, interrupted, interrupt = "feature:prot"),
    "A_EXPORT_INTERRUPTED"
  )
  payloads <- list.files(interrupted, recursive = TRUE, full.names = TRUE)
  payloads <- payloads[grepl("/feature-prot-[0-9a-f]+\\.json$", payloads)]
  testthat::expect_length(payloads, 1L)
  testthat::expect_false(file.exists(sub("\\.json$", ".envelope.json", payloads)))
  writeLines('{"feature_keys":["CORRUPT_FEATURE_KEY"],"cache_key":"not-a-result"}', payloads)
  resumed <- triomics_export_a(request, interrupted, resume = TRUE)
  testthat::expect_false(isTRUE(artifact_where(resumed, "feature", "prot")$reused))
  testthat::expect_false("CORRUPT_FEATURE_KEY" %in% resumed$features$prot$feature_keys)
  testthat::expect_true(all(startsWith(resumed$features$prot$feature_keys, "fk1|")))
  corrupt_text <- paste(unlist(lapply(list.files(file.path(interrupted, "export"), recursive = TRUE, full.names = TRUE),
                                      readLines, warn = FALSE)), collapse = "\n")
  testthat::expect_false(grepl("CORRUPT_FEATURE_KEY", corrupt_text, fixed = TRUE))

  shifted <- request
  shifted$planted$lip[[1L]]$token <- "8"
  changed <- triomics_export_a(shifted, output, resume = TRUE)
  testthat::expect_true(isTRUE(artifact_where(changed, "source", "prot")$reused))
  testthat::expect_true(isTRUE(artifact_where(changed, "feature", "prot")$reused))
  testthat::expect_true(isTRUE(artifact_where(changed, "state", "prot")$reused))
  testthat::expect_false(isTRUE(artifact_where(changed, "source", "lip")$reused))
  testthat::expect_false(isTRUE(artifact_where(changed, "feature", "lip")$reused))
  testthat::expect_false(isTRUE(artifact_where(changed, "state", "lip")$reused))
  testthat::expect_false(isTRUE(artifact_where(changed, "handoff", NULL)$reused))
  testthat::expect_equal(changed$assays$prot[1, 1], 3.5, tolerance = 1e-12)
  testthat::expect_equal(changed$assays$lip[1, 1], 8, tolerance = 1e-12)
  testthat::expect_false(identical(artifact_where(changed, "state", "lip")$state_hash,
                                   artifact_where(first, "state", "lip")$state_hash))
  testthat::expect_identical(artifact_where(changed, "state", "prot")$state_hash,
                             artifact_where(first, "state", "prot")$state_hash)

  versioned <- request
  versioned$version <- "0.1.1-test"
  version_dir <- new_dir("a-t08-version-")
  original <- triomics_export_a(request, version_dir)
  updated <- triomics_export_a(versioned, version_dir, resume = TRUE)
  testthat::expect_true(all(vapply(updated$artifacts, function(item) identical(item$reused, FALSE), logical(1))))
  testthat::expect_false(identical(updated$cache_key, original$cache_key))
  testthat::expect_false(identical(artifact_where(updated, "feature", "prot")$artifact_id,
                                   artifact_where(original, "feature", "prot")$artifact_id))
  testthat::expect_false(identical(artifact_where(updated, "state", "lip")$cache_key,
                                   artifact_where(original, "state", "lip")$cache_key))
  testthat::expect_equal(updated$assays$lip[1, 1], 7.25, tolerance = 1e-12)

  retoken <- request
  retoken$config_token <- "changed-config"
  reconfig <- triomics_export_a(retoken, version_dir, resume = TRUE)
  testthat::expect_false(isTRUE(artifact_where(reconfig, "feature", "prot")$reused))
  testthat::expect_false(isTRUE(artifact_where(reconfig, "state", "prot")$reused))
  testthat::expect_false(isTRUE(artifact_where(reconfig, "handoff", NULL)$reused))
  testthat::expect_false(identical(reconfig$cache_key, original$cache_key))
})
