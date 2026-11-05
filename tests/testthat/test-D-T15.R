testthat::test_that("the clinical report exposes the saved dictionary and per-variable coverage", {
  payload <- list(
    schema_version = "2.0",
    status = "complete",
    clinical_dictionary_coverage = list(
      schema_version = "1.0",
      variables = list(list(
        variable_id = "glucose",
        declared_type = "continuous",
        unit = "mmol/L",
        units_verification = "verified",
        role = "measured",
        time_semantics = "visit",
        availability = "available",
        verification = "verified",
        modelable = TRUE,
        missingness_rules = list(
          mapped_dispositions = list("placeholder"),
          unsupported_disposition_count = 0L,
          censoring = "forbidden",
          imputation = "none"
        )
      )),
      coverage = list(list(
        variable_id = "glucose", visit_id = "V1", scope = "visit",
        availability = "available", n_subjects = 12L, n_observed = 12L,
        n_missing = 0L, n_placeholder = 0L, n_censored = 0L, n_multiselect = 0L
      )),
      excluded = list()
    ),
    objectives = list(),
    results = list()
  )
  loaded <- list(payloads = list(clinical = payload))
  config <- list(clinical_enabled = TRUE, clinical_required = TRUE,
                 audience = "synthetic_demo", privacy_class = "synthetic")

  section <- triomics_report_clinical_section(loaded, config)

  testthat::expect_identical(section$status, "complete")
  testthat::expect_true(all(c("dictionary_coverage", "variable_coverage") %in%
                              names(section$tables)))
  dictionary <- section$tables$dictionary_coverage
  testthat::expect_identical(dictionary$variable_id[[1L]], "glucose")
  testthat::expect_identical(dictionary$unit[[1L]], "mmol/L")
  coverage <- section$tables$variable_coverage
  testthat::expect_identical(coverage$n_subjects[[1L]], 12L)
  testthat::expect_identical(coverage$n_observed[[1L]], 12L)
  testthat::expect_identical(coverage$n_missing[[1L]], 0L)
})
dt15_sha <- function(char = "a") paste(rep(char, 64L), collapse = "")

dt15_materialize_native_fixture <- function(root) {
  if (file.exists(root) || dir.exists(root)) {
    stop("The D-T15 fixture root must not already exist.", call. = FALSE)
  }
  if (!dir.create(root, recursive = TRUE)) {
    stop("The D-T15 fixture root could not be created.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  package_root <- normalizePath(testthat::test_path("..", ".."),
                                winslash = "/", mustWork = TRUE)
  source_root <- file.path(package_root, "inst", "examples", "data")
  c08_root <- file.path(source_root, "c08")
  data_root <- file.path(root, "data")
  clinical_root <- file.path(data_root, "d15-single-visit")
  assay_root <- file.path(data_root, "independent")
  ticket_root <- file.path(root, "tickets")
  dir.create(clinical_root, recursive = TRUE)
  dir.create(assay_root, recursive = TRUE)
  dir.create(ticket_root, recursive = TRUE)
  write_tsv <- function(value, path) {
    utils::write.table(value, path, sep = "\t", quote = FALSE, row.names = FALSE,
                       col.names = TRUE, na = "NA", fileEncoding = "UTF-8")
  }
  read_tsv <- function(path) {
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8")
  }

  metadata <- read_tsv(file.path(c08_root, "metadata.tsv"))
  if (nrow(metadata) != 12L || anyDuplicated(metadata$sample_id)) {
    stop("The public C-08 source template does not contain twelve unique samples.", call. = FALSE)
  }
  subject_map <- stats::setNames(paste0("SINGLE_", metadata$sample_id), metadata$sample_id)
  metadata$subject_id <- unname(subject_map[metadata$sample_id])
  write_tsv(metadata, file.path(clinical_root, "metadata.tsv"))

  links <- read_tsv(file.path(c08_root, "links.tsv"))
  links$subject_id <- unname(subject_map[links$sample_id])
  links$visit_id <- "V1"
  links$reference_time <- "2020-01-31"
  links$link_id <- paste0("SINGLE_LINK_", links$sample_id)
  write_tsv(links, file.path(clinical_root, "links.tsv"))

  observations <- read_tsv(file.path(c08_root, "observations.tsv"))
  observations$subject_id <- unname(subject_map[observations$sample_id])
  observations$visit_id <- "V1"
  observations$time <- "2020-01-31"
  write_tsv(observations, file.path(clinical_root, "observations.tsv"))

  file.copy(file.path(c08_root, "dictionary.tsv"),
            file.path(clinical_root, "dictionary.tsv"), overwrite = FALSE)
  assay_sources <- c("proteomics.tsv", "metabolomics.tsv")
  copied <- file.copy(file.path(source_root, "independent", assay_sources),
                      file.path(assay_root, assay_sources), overwrite = FALSE)
  if (!all(copied)) stop("The public assay fixtures could not be copied.", call. = FALSE)
  config_path <- file.path(ticket_root, "D-T15.yaml")
  ticket_template <- file.path(package_root, "inst", "examples", "tickets", "D-T15.yaml")
  if (!file.copy(ticket_template, config_path,
                 overwrite = FALSE)) {
    stop("The D-T15 template could not be copied into the fixture root.", call. = FALSE)
  }
  config <- yaml::read_yaml(config_path)
  relative_files <- c(
    vapply(config$sources, `[[`, character(1L), "file"),
    config$metadata$file,
    vapply(config$blocks, `[[`, character(1L), "file"),
    config$clinical$dictionary_file, config$clinical$observations_file,
    config$clinical$links_file
  )
  resolved_files <- file.path(dirname(config_path), relative_files)
  if (any(!file.exists(resolved_files))) {
    stop("The D-T15 template references an input absent from the materialized fixture.",
         call. = FALSE)
  }
  generated_files <- c(
    file.path(clinical_root, c("metadata.tsv", "links.tsv", "observations.tsv", "dictionary.tsv")),
    file.path(assay_root, assay_sources)
  )
  manifest <- list(
    fixture = "public_synthetic_single_visit",
    n_samples = as.integer(nrow(metadata)),
    n_independent_subjects = as.integer(length(unique(metadata$subject_id))),
    config_sha256 = triomics_sha256(config_path),
    source_template_sha256 = triomics_sha256(ticket_template),
    inputs = stats::setNames(lapply(generated_files, triomics_sha256),
                             file.path("data", c(
                               "d15-single-visit/metadata.tsv", "d15-single-visit/links.tsv",
                               "d15-single-visit/observations.tsv", "d15-single-visit/dictionary.tsv",
                               "independent/proteomics.tsv", "independent/metabolomics.tsv"
                             )))
  )
  manifest_path <- file.path(root, "fixture-manifest.json")
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)
  list(root = root, config_path = config_path, manifest_path = manifest_path,
       config_sha256 = manifest$config_sha256)
}

dt15_native_projection_fixture <- function(endpoint_id = "score_demo_pc1") {
  root <- tempfile("dt15-projection-", tmpdir = Sys.getenv("TMPDIR"))
  endpoint_dir <- file.path(root, "C", "score-endpoints")
  dir.create(endpoint_dir, recursive = TRUE)
  study_id <- "study-public-synthetic"
  analysis_id <- "analysis-pca"
  config_sha256 <- dt15_sha("d")
  endpoint <- list(
    artifact_type = "integration_score_endpoint",
    endpoint_id = endpoint_id,
    study_id = study_id,
    analysis_id = analysis_id,
    integration_id = "integration-pca",
    component_id = "PC1",
    response_type = "continuous",
    unit = "score_unit",
    units = "score_unit",
    scale = "native_pca_score",
    orientation = "native_component_sign",
    producing_fit = list(method = "concat_pca", model_role = "unsupervised_predictor_structure"),
    training_unit_ids = c("SYN_UNIT_01", "SYN_UNIT_02", "SYN_UNIT_03", "SYN_UNIT_04"),
    training_ids = c("SUBJECT_SECRET_01", "SUBJECT_SECRET_02",
                     "SUBJECT_SECRET_03", "SUBJECT_SECRET_04"),
    training_y = list(variable_id = NULL, used_by_fit = FALSE,
                      training_unit_ids = c("SYN_UNIT_01", "SYN_UNIT_02",
                                            "SYN_UNIT_03", "SYN_UNIT_04"),
                      training_subjects = c("SUBJECT_SECRET_01", "SUBJECT_SECRET_02",
                                            "SUBJECT_SECRET_03", "SUBJECT_SECRET_04")),
    training_outcome = NULL,
    selection = list(ancestry = "prespecified", validity = "qualified",
                     inspected_outcome = NULL),
    inference_status = "qualified",
    available_at_time = "after_integration_fit",
    dependency_group = "study-public-synthetic::integration-pca",
    preparation_context_id = "context-pca",
    representation_id = "levels",
    unit_map = c("LINK_SECRET_01", "LINK_SECRET_02", "LINK_SECRET_03", "LINK_SECRET_04"),
    source_hash = dt15_sha("e"),
    endpoint_sha256 = dt15_sha("f")
  )
  endpoint_bundle <- list(
    schema_version = "2.0",
    artifact_type = "integration_score_endpoint_bundle",
    study_id = study_id,
    analysis_id = analysis_id,
    integration_id = "integration-pca",
    unit_map_hash = dt15_sha("1"),
    source_hash = dt15_sha("2"),
    endpoints = list(PC1 = endpoint)
  )
  payload_path <- file.path(endpoint_dir, "integration-01.json")
  envelope_path <- file.path(endpoint_dir, "integration-01.envelope.json")
  triomics_write_json_atomic(endpoint_bundle, payload_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "integration_score_endpoint_bundle",
    artifact_id = "c08-score-endpoint-bundle-1",
    study_id = study_id,
    run_id = "c08-synthetic-fixture",
    status = "complete",
    config_sha256 = config_sha256,
    payload = payload_path,
    producer_commit = "synthetic-test-fixture",
    inputs = list(list(artifact_id = "C::integration-pca", sha256 = dt15_sha("3"))),
    privacy_class = "synthetic"
  )
  triomics_write_envelope(envelope, envelope_path)
  endpoint_ref <- list(
    artifact_id = "c08-score-endpoint-bundle-1",
    payload_file = file.path("C", "score-endpoints", "integration-01.json"),
    envelope_file = file.path("C", "score-endpoints", "integration-01.envelope.json"),
    payload_sha256 = triomics_sha256(payload_path),
    envelope_sha256 = triomics_sha256(envelope_path)
  )
  clinical <- list(
    schema_version = "2.0",
    status = "complete",
    study_id = study_id,
    analysis_id = analysis_id,
    objectives = list(list(
      objective_id = "objective-age-pc1", objective_version = "1",
      question = "CLINICAL_SENTINEL_QUESTION /private/study/notes.txt",
      mode = "between_person", response_ref = endpoint_id, endpoint_ref = endpoint_id,
      predictor_ref = "age_years", population = list(visit_id = "V1"),
      adjustments = list(list(variable = "sex_code", role = "confounder", form = "level",
                              rationale = "CLINICAL_SENTINEL_RATIONALE")),
      missingness_policy = "complete_case", hypothesis_status = "exploratory",
      family_id = "family-age-pc1", protocol_ref = "protocol-age-pc1", status = "fitted"
    )),
    family = list(
      family_id = "family-age-pc1", claim_scope = "declared_clinical_score_associations",
      member_ids = "score_demo_pc1", member_hash = dt15_sha("8"),
      n_declared = 1L, n_estimated = 1L, n_failed = 0L,
      correction_method = "BH", q_published = TRUE
    ),
    results = list(list(
      objective_id = "objective-age-pc1", objective_version = "1",
      analysis_id = analysis_id, endpoint_id = endpoint_id, predictor_ref = "age_years",
      response_role = "endpoint", predictor_role = "clinical", mode = "between_person",
      estimand = "adjusted_between_person_coefficient", response_type = "continuous",
      predictor_type = "continuous", effect = 0.4, effect_unit = "score_unit per year",
      effect_scale = "per_predictor_unit", predictor_units = "years", response_units = "score_unit",
      ordinary_se = 0.2, moderated_se = 0.2, interval_low = 0.1, interval_high = 0.7,
      interval_level = 0.95, interval_method = "moderated_t", statistic = 2,
      statistic_name = "moderated_t", ordinary_df = 8, effective_test_df = 8,
      p = 0.08, q = 0.08, inferential_status = "exploratory",
      estimation_status = "estimated", independent_validation_released = FALSE,
      hypothesis_status = "exploratory",
      adjustment = list(list(variable = "sex_code", role = "confounder", form = "level",
                             rationale = "CLINICAL_SENTINEL_RATIONALE")),
      n_subjects = 4L, n_samples = 4L, n_rows_used = 4L,
      n_excluded_clinical = 0L, n_excluded_endpoint = 0L,
      missingness_policy = "complete_case",
      formula = paste(endpoint_id, "~ intercept + age_years + sex_code"),
      family_id = "family-age-pc1", source_hash = dt15_sha("b"),
      model_hash = dt15_sha("c"), result_id = "result-age-pc1",
      selection = list(ancestry = "prespecified", validity = "qualified",
                       inspected_outcome = NULL, adapter_selection = list(method = "none"))
    )),
    clinical_dictionary_coverage = list(
      schema_version = "1.0",
      variables = list(
        list(
          variable_id = "age_years", label = "CLINICAL_SENTINEL_LABEL",
          declared_type = "continuous", unit = "years", units_verification = "verified",
          role = "measured", time_semantics = "visit", availability = "available",
          verification = "verified", modelable = TRUE,
          missingness_rules = list(mapped_dispositions = list("placeholder"),
                                   unsupported_disposition_count = 0L,
                                   censoring = "forbidden", imputation = "none")
        ),
        list(
          variable_id = "sex_code", label = "Synthetic sex code",
          declared_type = "categorical", unit = "category", units_verification = "not_applicable",
          role = "measured", time_semantics = "visit", availability = "available",
          verification = "verified", modelable = TRUE,
          missingness_rules = list(mapped_dispositions = list("placeholder"),
                                   unsupported_disposition_count = 0L,
                                   censoring = "forbidden", imputation = "none")
        )
      ),
      coverage = list(
        list(
          variable_id = "age_years", visit_id = "V1", scope = "visit",
          availability = "available", n_subjects = 4L, n_observed = 3L,
          n_missing = 1L, n_placeholder = 1L, n_censored = 0L, n_multiselect = 0L
        ),
        list(
          variable_id = "sex_code", visit_id = "V1", scope = "visit",
          availability = "available", n_subjects = 4L, n_observed = 4L,
          n_missing = 0L, n_placeholder = 0L, n_censored = 0L, n_multiselect = 0L
        )
      ),
      excluded = list()
    )
  )
  loaded <- list(
    root = root,
    config = list(study_id = study_id, analysis_id = analysis_id,
                  config_sha256 = config_sha256),
    payloads = list(
      clinical = clinical,
      c = list(integrations = list(list(
        integration_id = "integration-pca", method = "concat_pca", status = "complete",
        score_endpoint_bundle = endpoint_ref,
        clinical_score_request = list(status = "scheduled", owner_stage = "B",
                                      model_execution = "not_started")
      )))
    ),
    d = list(inherited_evidence = list(
      evaluation_evidence = list(controls = list()),
      stage_status = list(clinical_prediction = list(
        status = "not_started", completion_policy = "conditional",
        packet_id = "C-09", model_execution = "not_started"
      ))
    ))
  )
  config <- list(clinical_enabled = TRUE, clinical_required = TRUE,
                 audience = "synthetic_demo", privacy_class = "synthetic",
                 study_id = study_id, analysis_id = analysis_id,
                 config_sha256 = config_sha256)
  list(root = root, loaded = loaded, config = config,
       endpoint_payload_path = payload_path, endpoint_ref = endpoint_ref)
}

testthat::test_that("clinical objectives and associations preserve model, effects and saved C lineage", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  section <- triomics_report_clinical_section(fixture$loaded, fixture$config)

  testthat::expect_identical(section$status, "complete")
  testthat::expect_true(all(c("objectives", "associations", "endpoint_lineage",
                              "held_out_comparisons") %in% names(section$tables)))
  objective <- section$tables$objectives
  testthat::expect_identical(objective$objective_id[[1L]], "objective-age-pc1")
  testthat::expect_identical(objective$mode[[1L]], "between_person")
  testthat::expect_identical(objective$model_formula[[1L]],
                             "score_demo_pc1 ~ intercept + age_years + sex_code")
  testthat::expect_identical(objective$registered_adjustments[[1L]], "sex_code:confounder:level")
  association <- section$tables$associations
  testthat::expect_equal(association$effect[[1L]], 0.4)
  testthat::expect_equal(association$interval_lower[[1L]], 0.1)
  testthat::expect_equal(association$interval_upper[[1L]], 0.7)
  testthat::expect_identical(association$effect_unit[[1L]], "score_unit per year")
  testthat::expect_identical(association$family_id[[1L]], "family-age-pc1")
  testthat::expect_identical(association$evidence_class[[1L]], "descriptive_score_association")
  lineage <- section$tables$endpoint_lineage
  testthat::expect_identical(lineage$endpoint_id[[1L]], "score_demo_pc1")
  testthat::expect_identical(lineage$integration_id[[1L]], "integration-pca")
  testthat::expect_identical(lineage$training_unit_count[[1L]], 4L)
  testthat::expect_identical(lineage$training_subject_count[[1L]], 4L)
  testthat::expect_false(lineage$training_outcome_used_by_fit[[1L]])
  testthat::expect_identical(section$tables$held_out_comparisons$status[[1L]], "not_started")
  testthat::expect_identical(section$tables$held_out_comparisons$reason_code[[1L]],
                             "C09_COMPARISON_ARTIFACT_ABSENT")
  exported <- paste(unlist(lapply(section$tables, function(frame) {
    lapply(frame, as.character)
  }), use.names = FALSE), collapse = " ")
  testthat::expect_false(grepl(
    "CLINICAL_SENTINEL|SUBJECT_SECRET|SYN_UNIT_|LINK_SECRET|/private/study",
    exported
  ))
  testthat::expect_true(grepl(dt15_sha("b"), exported, fixed = TRUE))
})

testthat::test_that("a held_score name and a B release flag do not imply held-out comparison", {
  fixture <- dt15_native_projection_fixture(endpoint_id = "held_score")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture$loaded$payloads$clinical$results[[1L]]$independent_validation_released <- TRUE
  fixture$loaded$payloads$clinical$objectives[[1L]]$hypothesis_status <- "independent_validation"

  section <- triomics_report_clinical_section(fixture$loaded, fixture$config)

  testthat::expect_identical(section$tables$associations$evidence_class[[1L]],
                             "descriptive_score_association")
  testthat::expect_false(any(section$tables$held_out_comparisons$evidence_class ==
                               "held_out_performance_comparison"))
  testthat::expect_identical(section$tables$held_out_comparisons$status[[1L]], "not_started")
})
testthat::test_that("dictionary variables with unavailable missingness rules are retained explicitly", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  unavailable <- list(
    variable_id = "unsupported_marker",
    declared_type = "unsupported",
    unit = NULL,
    units_verification = "not_verified",
    role = "measured",
    time_semantics = "visit",
    availability = "unavailable",
    verification = "unsupported",
    modelable = FALSE,
    missingness_rules = NULL
  )
  fixture$loaded$payloads$clinical$clinical_dictionary_coverage$variables[[3L]] <- unavailable

  section <- triomics_report_clinical_section(fixture$loaded, fixture$config)

  row <- section$tables$dictionary_coverage[
    section$tables$dictionary_coverage$variable_id == "unsupported_marker", , drop = FALSE
  ]
  testthat::expect_identical(nrow(row), 1L)
  testthat::expect_identical(row$availability[[1L]], "unavailable")
  testthat::expect_identical(row$missingness_rule_status[[1L]], "not_reported")
  testthat::expect_true(is.na(row$unsupported_disposition_count[[1L]]))
  testthat::expect_false(row$modelable[[1L]])
})

testthat::test_that("registered unavailable objectives stay visible beside estimated results", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture$loaded$payloads$clinical$objectives[[2L]] <- list(
    objective_id = "objective-unavailable", objective_version = "1",
    question = "CLINICAL_SENTINEL_UNAVAILABLE_1",
    mode = "between_person", response_ref = "score_not_released",
    endpoint_ref = "score_not_released", predictor_ref = "age_years",
    adjustments = list(), missingness_policy = "complete_case",
    hypothesis_status = "exploratory", family_id = "family-age-pc1",
    protocol_ref = "protocol-unavailable", time_window_ref = "V1"
  )

  section <- triomics_report_clinical_section(fixture$loaded, fixture$config)

  unavailable <- section$tables$objectives[
    section$tables$objectives$objective_id == "objective-unavailable", , drop = FALSE
  ]
  testthat::expect_identical(nrow(unavailable), 1L)
  testthat::expect_identical(unavailable$result_status[[1L]], "unavailable")
  testthat::expect_identical(unavailable$result_reason[[1L]], "D_REPORT_CLINICAL_RESULT_ABSENT")
  testthat::expect_false(grepl("CLINICAL_SENTINEL_UNAVAILABLE_1",
                               paste(unlist(lapply(section$tables$objectives, as.character)),
                                     collapse = " ")))
})

testthat::test_that("a complete C-09 stage status remains status-only synthetic evidence", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture$loaded$d$inherited_evidence$stage_status$clinical_prediction <- list(
    status = "complete",
    completion_policy = "conditional",
    packet_id = "C-09",
    model_execution = "complete",
    status_scope = "synthetic_fixture_only",
    qualification_status = "unqualified",
    independent_validation = "not_performed",
    benefit_guaranteed = FALSE
  )

  section <- triomics_report_clinical_section(fixture$loaded, fixture$config)
  row <- section$tables$held_out_comparisons

  testthat::expect_identical(row$status[[1L]], "complete")
  testthat::expect_identical(row$evidence_class[[1L]], "held_out_comparison_status_only")
  testthat::expect_identical(row$evidence_scope[[1L]], "synthetic_fixture_only")
  testthat::expect_identical(row$qualification_status[[1L]], "unqualified")
  testthat::expect_identical(row$independent_validation[[1L]], "not_performed")
  testthat::expect_false(row$benefit_guaranteed[[1L]])
})

testthat::test_that("bound C-02 nested evaluation is displayed separately from B association and C-09", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  evaluation <- list(
    mode = "continuous", method = "concat_pls", native_version = "0.0.0-fixture",
    split_plan_hash = dt15_sha("9"),
    outer_folds = list(
      outer_1 = list(fold_id = "outer_1",
                     training_unit_ids = c("UNIT_SECRET_A", "UNIT_SECRET_B"),
                     test_unit_ids = c("UNIT_SECRET_C", "UNIT_SECRET_D"),
                     training_subject_ids = c("HELDOUT_SECRET_A", "HELDOUT_SECRET_B"),
                     test_subject_ids = c("HELDOUT_SECRET_C", "HELDOUT_SECRET_D")),
      outer_2 = list(fold_id = "outer_2",
                     training_unit_ids = c("UNIT_SECRET_C", "UNIT_SECRET_D"),
                     test_unit_ids = c("UNIT_SECRET_A", "UNIT_SECRET_B"),
                     training_subject_ids = c("HELDOUT_SECRET_C", "HELDOUT_SECRET_D"),
                     test_subject_ids = c("HELDOUT_SECRET_A", "HELDOUT_SECRET_B"))
    ),
    predictions = data.frame(
      fold_id = c("outer_1", "outer_1", "outer_2", "outer_2"),
      unit_id = c("UNIT_SECRET_C", "UNIT_SECRET_D", "UNIT_SECRET_A", "UNIT_SECRET_B"),
      truth = c(3, 4, 5, 6), prediction = c(3.2, 3.8, 5.1, 5.9),
      status = rep("predicted", 4L)
    ),
    metrics = list(
      model = list(rmse = 1.25, mae = 1, r_squared = 0.6,
                   r_squared_baseline = "training_intercept"),
      baseline = list(rmse = 2, mae = 1.5, r_squared = 0)
    ),
    baseline_comparison = list(
      coverage = "all_eligible_units", disposition = "complete_eligible_set",
      n_matched = 4L, failed_unit_ids = character(), baseline_failed_unit_ids = character()
    ),
    uncertainty = list(method = "not_estimated", disposition = "not_estimated",
                       resampling_unit = "subject"),
    failures = data.frame(), requested_folds = list(), inspected_fit_count = 12L, max_fits = 24L,
    n_eligible = 4L, n_predicted = 4L, failed_prediction_count = 0L,
    n_ineligible = 0L, total_denominator = 4L
  )
  fixture$loaded$payloads$c$integrations[[2L]] <- list(
    integration_id = "integration-pls", method = "concat_pls", status = "complete",
    evaluation = evaluation
  )
  fixture$config$config <- list(analyses = list(list(
    analysis_id = "analysis-pca",
    integration = list(
      list(integration_id = "integration-pca", method = "concat_pca"),
      list(integration_id = "integration-pls", method = "concat_pls", outcome = "age_years",
           evaluation = list(generalization_unit = "subject", outer_folds = 2L))
    )
  )))

  section <- triomics_report_clinical_section(fixture$loaded, fixture$config)
  held_out <- section$tables$held_out_comparisons
  c02 <- held_out[held_out$comparison_type == "C-02 nested evaluation" &
                    held_out$metric == "rmse", , drop = FALSE]
  unavailable <- held_out[held_out$comparison_type == "C-02 nested evaluation" &
                            held_out$integration_id == "integration-pca", , drop = FALSE]
  c09 <- held_out[held_out$comparison_type == "C-09 matched clinical prediction", , drop = FALSE]

  testthat::expect_identical(nrow(c02), 1L)
  testthat::expect_identical(c02$status[[1L]], "complete")
  testthat::expect_identical(c02$evidence_class[[1L]], "held_out_model_evaluation")
  testthat::expect_identical(c02$clinical_variable_id[[1L]], "age_years")
  testthat::expect_identical(c02$endpoint_link_status[[1L]], "clinical_variable_bound")
  testthat::expect_identical(c02$link_scope[[1L]], "integration_level_only")
  testthat::expect_equal(c02$model_value[[1L]], 1.25)
  testthat::expect_equal(c02$baseline_value[[1L]], 2)
  testthat::expect_identical(c02$n_predicted[[1L]], 4L)
  testthat::expect_identical(c02$outer_fold_count[[1L]], 2L)
  testthat::expect_identical(c02$resampling_unit[[1L]], "subject")
  testthat::expect_identical(c02$independent_validation[[1L]], "not_claimed")
  testthat::expect_match(c02$dependence_limitations[[1L]], "folds are not independent")
  testthat::expect_identical(unavailable$status[[1L]], "unavailable")
  testthat::expect_identical(unavailable$reason_code[[1L]], "C02_EVALUATION_ARTIFACT_ABSENT")
  testthat::expect_identical(c09$status[[1L]], "not_started")
  exported <- paste(unlist(lapply(held_out, as.character), use.names = FALSE), collapse = " ")
  testthat::expect_false(grepl("HELDOUT_SECRET|UNIT_SECRET|3.2", exported))
})

testthat::test_that("clinical disabled remains not requested and aggregate release remains gated", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  disabled <- fixture$config
  disabled$clinical_enabled <- FALSE

  section <- triomics_report_clinical_section(fixture$loaded, disabled)
  testthat::expect_identical(section$status, "not_requested")
  testthat::expect_length(section$tables, 0L)

  aggregate <- fixture$config
  aggregate$audience <- "approved_aggregate"
  testthat::expect_error(
    triomics_report_clinical_section(fixture$loaded, aggregate),
    "minimum-cell and complementary-suppression policy"
  )
})

testthat::test_that("a correctly enveloped C endpoint with a mismatched integration identity is refused", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  bundle <- jsonlite::read_json(fixture$endpoint_payload_path, simplifyVector = FALSE)
  bundle$endpoints$PC1$integration_id <- "integration-other"
  unlink(fixture$endpoint_payload_path)
  triomics_write_json_atomic(bundle, fixture$endpoint_payload_path)
  envelope_path <- file.path(fixture$root, "C", "score-endpoints", "integration-01.envelope.json")
  envelope <- triomics_artifact_envelope(
    artifact_type = "integration_score_endpoint_bundle",
    artifact_id = "c08-score-endpoint-bundle-1",
    study_id = "study-public-synthetic",
    run_id = "c08-synthetic-fixture",
    status = "complete",
    config_sha256 = dt15_sha("d"),
    payload = fixture$endpoint_payload_path,
    producer_commit = "synthetic-test-fixture",
    inputs = list(list(artifact_id = "C::integration-pca", sha256 = dt15_sha("3"))),
    privacy_class = "synthetic"
  )
  unlink(envelope_path)
  triomics_write_envelope(envelope, envelope_path)
  reference <- fixture$endpoint_ref
  reference$payload_sha256 <- triomics_sha256(fixture$endpoint_payload_path)
  reference$envelope_sha256 <- triomics_sha256(envelope_path)
  fixture$loaded$payloads$c$integrations[[1L]]$score_endpoint_bundle <- reference

  testthat::expect_error(
    triomics_report_clinical_section(fixture$loaded, fixture$config),
    "saved C score endpoint bundle identity"
  )
})

testthat::test_that("adjustment references absent from the safe dictionary are refused", {
  fixture <- dt15_native_projection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture$loaded$payloads$clinical$results[[1L]]$adjustment[[1L]]$variable <-
    "unregistered_adjustment_secret_result"

  testthat::expect_error(
    triomics_report_clinical_section(fixture$loaded, fixture$config),
    "adjustment variable is absent from the safe dictionary"
  )

  fixture$loaded$payloads$clinical$results[[1L]]$adjustment[[1L]]$variable <- "sex_code"
  fixture$loaded$payloads$clinical$objectives[[1L]]$adjustments[[1L]]$variable <-
    "unregistered_adjustment_secret_objective"
  testthat::expect_error(
    triomics_report_clinical_section(fixture$loaded, fixture$config),
    "adjustment variable is absent from the safe dictionary"
  )
})

testthat::test_that("the D-T15 template materializes 12 independent single-visit subjects portably", {
  configured_root <- Sys.getenv("TRIOMICS_D15_NATIVE_FIXTURE_ROOT")
  root <- if (nzchar(configured_root)) configured_root else {
    tempfile("dt15-native-fixture-", tmpdir = Sys.getenv("TMPDIR"))
  }
  fixture <- dt15_materialize_native_fixture(root)
  if (!nzchar(configured_root)) on.exit(unlink(root, recursive = TRUE), add = TRUE)
  metadata <- utils::read.delim(file.path(root, "data", "d15-single-visit", "metadata.tsv"),
                                check.names = FALSE, stringsAsFactors = FALSE)
  observations <- utils::read.delim(file.path(root, "data", "d15-single-visit", "observations.tsv"),
                                    check.names = FALSE, stringsAsFactors = FALSE)
  links <- utils::read.delim(file.path(root, "data", "d15-single-visit", "links.tsv"),
                             check.names = FALSE, stringsAsFactors = FALSE)
  config_text <- paste(readLines(fixture$config_path, warn = FALSE), collapse = "\n")
  config <- yaml::read_yaml(fixture$config_path)
  referenced_files <- c(
    vapply(config$sources, `[[`, character(1L), "file"),
    config$metadata$file,
    vapply(config$blocks, `[[`, character(1L), "file"),
    config$clinical$dictionary_file, config$clinical$observations_file,
    config$clinical$links_file
  )

  testthat::expect_identical(nrow(metadata), 12L)
  testthat::expect_identical(length(unique(metadata$subject_id)), 12L)
  testthat::expect_identical(unique(observations$visit_id), "V1")
  testthat::expect_identical(unique(links$visit_id), "V1")
  testthat::expect_true(isTRUE(config$clinical$enabled))
  testthat::expect_true(all(file.exists(file.path(dirname(fixture$config_path), referenced_files))))
  testthat::expect_false(grepl("/Users/|/Volumes/|/private/", config_text))
  if (nzchar(configured_root)) {
    message("D-T15 native fixture config: ", fixture$config_path)
    message("D-T15 native fixture config SHA256: ", fixture$config_sha256)
  }
})
