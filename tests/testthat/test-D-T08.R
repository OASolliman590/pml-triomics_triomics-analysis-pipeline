testthat::test_that("the report renderer rejects a bundle without an immutable source manifest", {
  renderer <- get0("render_report", envir = asNamespace("triomics"), inherits = FALSE)
  testthat::expect_true(is.function(renderer))
  if (!is.function(renderer)) return(invisible(NULL))

  error <- tryCatch(renderer(list(), list()), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_BUNDLE_INVALID")
})

testthat::test_that("report bundle construction refuses a missing producer root", {
  build_bundle <- get0("triomics_report_bundle_from_root", envir = asNamespace("triomics"),
                       inherits = FALSE)
  testthat::expect_true(is.function(build_bundle))
  if (!is.function(build_bundle)) return(invisible(NULL))

  missing_root <- file.path(tempdir(), "d-t08-missing-producer-root")
  error <- tryCatch(build_bundle(missing_root, list()), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_REQUIRED_MISSING")
})

testthat::test_that("the report resolves the source-checkout Quarto template", {
  template_path <- get0("triomics_report_template_path", envir = asNamespace("triomics"),
                        inherits = FALSE)
  testthat::expect_true(is.function(template_path))
  if (!is.function(template_path)) return(invisible(NULL))

  resolved <- normalizePath(template_path(), winslash = "/", mustWork = TRUE)
  expected <- normalizePath(triomics_test_resource("report", "triomics.qmd"),
                            winslash = "/", mustWork = TRUE)

  testthat::expect_identical(resolved, expected)
})

testthat::test_that("a valid artifact envelope must bind the exact fixed payload bytes", {
  root <- tempfile("d-t08-envelope-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  payload_path <- file.path(root, "assays.json")
  alternate_path <- file.path(root, "alternate.json")
  envelope_path <- file.path(root, "assays.envelope.json")
  writeLines('{"schema_version":"2.0","study_id":"study","status":"complete"}',
             payload_path, useBytes = TRUE)
  writeLines('{"schema_version":"2.0","study_id":"other","status":"complete"}',
             alternate_path, useBytes = TRUE)
  envelope <- triomics_artifact_envelope(
    artifact_type = "assays", artifact_id = "stage-a-assays", study_id = "study",
    run_id = "run", status = "complete", config_sha256 = paste(rep("a", 64), collapse = ""),
    payload = alternate_path, producer_commit = "test-commit"
  )
  triomics_write_envelope(envelope, envelope_path)

  error <- tryCatch(
    triomics_report_artifact(root, "assays.json", "assays", "stage-a-assays", "study",
                             paste(rep("a", 64), collapse = ""), parse_payload = FALSE),
    triomics_error = identity
  )

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_SOURCE_INVALID")
})

testthat::test_that("offline resource scanning rejects remote assets and executable fetches", {
  unsafe <- c(
    '<img src="//cdn.example/image.svg">',
    '<source srcset="https://cdn.example/a.png 1x, /private/b.png 2x">',
    '<object data="https://cdn.example/embed.bin"></object>',
    '<embed src="//cdn.example/embed.bin">',
    '<img src=assets/image.png>',
    '<style>.x { background-image: url(https://cdn.example/bg.png) }</style>',
    '<script>fetch("https://cdn.example/data.json")</script>'
  )
  testthat::expect_true(all(vapply(unsafe, triomics_report_external_reference, logical(1))))
  testthat::expect_false(triomics_report_external_reference('<a href="https://example.org/citation">Source</a>'))
  testthat::expect_false(triomics_report_external_reference('<img src="data:image/png;base64,AAAA">'))
})

testthat::test_that("report text sanitization removes local paths and credentials before HTML and TSV export", {
  raw <- c("project /Users/megastore/private/study.tsv", "Authorization: Bearer secret-token-value",
           "api_key=abcdef123456")
  cleaned <- triomics_report_clean_text(raw)
  html <- triomics_report_html_escape("<script>CLINICAL_SENTINEL_23</script>")
  tsv <- triomics_report_tsv_text(c("=SUM(1,1)", raw))

  testthat::expect_false(any(grepl("/Users/megastore|secret-token-value|abcdef123456", cleaned)))
  testthat::expect_false(grepl("<script>|</script>", html))
  testthat::expect_true(startsWith(tsv[[1L]], "'="))
  testthat::expect_false(any(grepl("/Users/megastore|secret-token-value|abcdef123456", tsv)))
})

testthat::test_that("B projection keeps every row's scientific values while withholding unsafe free text", {
  block <- list(
    block_id = "block_1", name = "/Users/private/CLINICAL_SENTINEL_810", profile = "proteomics",
    quantity = "intensity", scale = "linear", units = "log2 units",
    preparation_context_id = "c06", transform = "none", normalization = "none", imputation = "none"
  )
  row <- function(feature, effect) list(
    feature_key = feature, analysis_id = "analysis", representation_id = "levels",
    effect = effect, effect_unit = "log2 abundance per unit", ordinary_se = 0.25,
    moderated_se = 0.2, interval_lower = effect - 0.5, interval_upper = effect + 0.5,
    interval_method = "source_reported", statistic = effect / 0.2, statistic_name = "t",
    ordinary_df = 8, effective_test_df = 8, p = 0.02, q = 0.04,
    inferential_status = "estimated", estimation_status = "estimated",
    estimation_reason = "CLINICAL_SENTINEL_810 /Users/private/notes.txt",
    n_samples = 10, n_subjects = 10, n_rows_used = 10, n_pairs = 0,
    direction_numerator = 7, direction_denominator = 10, direction_zero_count = 0,
    direction_status = "complete", direction_reason = "/Volumes/private/sensitive.tsv",
    count_prior_ref = list(count = 4, count_type = "psm_count",
                           metadata_sha256 = paste(rep("a", 64), collapse = ""),
                           sample_ids = list("PRIVATE_SAMPLE_1"),
                           provenance = "CLINICAL_SENTINEL_810"),
    source_hash = paste(rep("b", 64), collapse = ""), model_hash = paste(rep("c", 64), collapse = ""),
    representation_hash = paste(rep("d", 64), collapse = ""), contrast_hash = paste(rep("e", 64), collapse = ""),
    result_id = paste0("result-", feature)
  )
  claim <- list(
    claim_type = "contrast", effect_unit = "log2 abundance per unit",
    family = list(family_id = "family-1", member_hash = paste(rep("f", 64), collapse = ""),
                  claim_scope = "all_tested_features", correction_method = "BH",
                  n_declared = 2, n_eligible = 2, n_evaluated = 2, n_failed = 0),
    rows = list(row("F1", 1.5), row("F2", -0.25))
  )
  payload <- list(
    analysis_id = "analysis", preparation_context_id = "c06", representation_id = "levels",
    blocks = list(block_1 = list(block_id = "block_1", contrasts = list(score_slope = claim)))
  )

  result <- triomics_report_rows(payload, list(block))
  text <- paste(unlist(lapply(result, as.character)), collapse = " ")

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(result$effect, c(1.5, -0.25))
  testthat::expect_identical(result$effect_unit, rep("log2 abundance per unit", 2L))
  testthat::expect_equal(result$ordinary_se, rep(0.25, 2L))
  testthat::expect_equal(result$interval_lower, c(1.0, -0.75))
  testthat::expect_equal(result$n_samples, rep(10, 2L))
  testthat::expect_equal(result$family_n_evaluated, rep(2, 2L))
  testthat::expect_equal(result$count_prior_count, rep(4, 2L))
  testthat::expect_identical(result$count_prior_type, rep("psm_count", 2L))
  testthat::expect_identical(result$block_name, rep("Block 1", 2L))
  testthat::expect_false(grepl("CLINICAL_SENTINEL_810|/Users/private|/Volumes/private", text))
})

testthat::test_that("A dimensions and missingness come from the saved assay export", {
  payload <- list(
    schema_version = "2.0", study_id = "study",
    blocks = list(list(
      block_id = "block_1", feature_keys = list("F1", "F2"), sample_ids = list("S1", "S2"),
      values = list(list(1, NULL), list(2, 3))
    ))
  )
  config <- list(study_id = "study", blocks = list(list(block_id = "block_1", name = "Block 1")))

  flow <- triomics_report_assay_summary(payload, config)

  testthat::expect_equal(flow$source_feature_count, 2L)
  testthat::expect_equal(flow$source_sample_count, 2L)
  testthat::expect_equal(flow$observed_cell_count, 3L)
  testthat::expect_equal(flow$missing_cell_count, 1L)
  testthat::expect_false(any(c("S1", "S2", "F1", "F2") %in% unlist(flow, use.names = FALSE)))
})

testthat::test_that("C projection retains saved variance and D retains coverage counts", {
  config <- list(integration_requirements = c(fit1 = TRUE), route_requirements = c(gene_reaction = FALSE))
  c_payload <- list(integrations = list(list(
    integration_id = "fit1", method = "concat_pca", status = "complete",
    result = list(
      variance = list(rows = list(block_ids = list("block_1", "block_2")),
                      columns = list(component_ids = list("PC1", "PC2")),
                      values = list(list(0.625, 0.125), list(0.375, 0.25)),
                      metric = "projection_fraction", assay_space = "training",
                      denominator = "source_ss", semantics = "incremental"),
      selected = list(block_1 = list("F1"), block_2 = list("F2")),
      params = list(retained_n_components = 1, rank_lineage = list(
        training_unit_count = 8, selected_feature_count = 2, numerical_rank = 1,
        requested_components = 2, retained_components = 1,
        disposition = "truncated_to_training_numeric_rank"),
        feature_flow = list(block_1 = list(input = list("F1", "F2", "F3"),
                                          selected = list("F1", "F2"),
                                          retained = list("F1", "F2"),
                                          zero_variance = list("F3"),
                                          excluded_by_selection = list())),
        effective = list(selection = list(rule = "none")))
    )
  )))
  d <- list(
    routes = list(gene_reaction = list(status = "complete", reason_code = "D_ROUTE_COMPLETE",
                                      nominations = list(list(feature_key = "F1")), candidate_count = 3L)),
    enrichment = list(status = "partial", reason_code = "D_ORA_INVENTORY_UNQUALIFIED",
                      test_count = 1L, results = list(list(
                        identity = list(study_id = "study-1", analysis_id = "analysis-1",
                                        preparation_context_id = "prep-1", representation_id = "levels",
                                        block_id = "block_1", source_branch = "primary_statistics",
                                        claim_id = "score_slope", integration_id = "not_applicable",
                                        family_id = "family-1", source_ids = list(), nomination_ids = list()),
                        status = "unavailable", family_size = 2L, tests_in_bh = 0L,
                        estimable_count = 0L, interpretation = "not_pathway_activation",
                        sets = list(list(set_id = "set-1", status = "unavailable", N = 10L,
                                         K = 2L, n = 1L, k = 1L, p = NULL, q = NULL))
                      )))
  )

  integrations <- triomics_report_integration_rows(c_payload, config)
  eligibility <- triomics_report_integration_eligibility_rows(c_payload)
  variance <- triomics_report_integration_variance_rows(c_payload)
  routes <- triomics_report_route_rows(d, config)
  enrichment <- triomics_report_enrichment_rows(d)

  testthat::expect_equal(variance$variance_value, c(0.625, 0.125, 0.375, 0.25))
  testthat::expect_identical(variance$variance_metric, rep("projection_fraction", 4L))
  testthat::expect_equal(integrations$training_unit_count, 8L)
  testthat::expect_equal(integrations$selected_feature_count, 2L)
  testthat::expect_equal(eligibility$input_feature_count, 3L)
  testthat::expect_equal(eligibility$selected_feature_count, 2L)
  testthat::expect_equal(eligibility$zero_variance_feature_count, 1L)
  testthat::expect_equal(routes$candidate_count[routes$route == "gene_reaction"], 3L)
  testthat::expect_equal(routes$nominated_count[routes$route == "gene_reaction"], 1L)
  testthat::expect_equal(enrichment$population_N, 10L)
  testthat::expect_equal(enrichment$set_members_K, 2L)
  testthat::expect_identical(enrichment$status, "unavailable")
})

testthat::test_that("C-09 status stays separate from qualification, scope, execution and benefit", {
  routes <- data.frame(route = c("mofa_native", "biopan"), status = c("unavailable", "not_requested"),
                       stringsAsFactors = FALSE)
  c09 <- list(status = "not_started", completion_policy = "conditional",
              model_execution = "not_started", status_scope = "synthetic_fixture_only",
              benefit_guaranteed = FALSE)

  capabilities <- triomics_report_conditional_capabilities(routes, "not_run", c09)
  row <- capabilities[capabilities$capability == "C-09 clinical prediction", , drop = FALSE]

  testthat::expect_identical(row$status, "not_started")
  testthat::expect_identical(row$completion_policy, "conditional")
  testthat::expect_identical(row$qualification_status, "unqualified")
  testthat::expect_identical(row$scope, "synthetic_fixture_only")
  testthat::expect_identical(row$model_execution, "not_started")
  testthat::expect_identical(row$benefit_guaranteed, FALSE)
})

testthat::test_that("future fixed sections default unavailable but accept later packet providers", {
  sections <- triomics_report_optional_sections(
    list(), list(target_enabled = TRUE, clinical_enabled = TRUE)
  )
  provider_env <- environment(triomics_report_optional_sections)
  providers <- c(targets = "triomics_report_targets_section",
                 clinical = "triomics_report_clinical_section")

  testthat::expect_identical(names(sections), c("targets", "clinical"))
  for (section in names(providers)) {
    provider <- get0(providers[[section]], envir = provider_env, mode = "function", inherits = TRUE)
    testthat::expect_true(sections[[section]]$status %in%
                            c("complete", "partial", "unavailable", "not_requested"))
    if (!is.function(provider)) {
      expected_reason <- if (identical(section, "targets")) {
        "D_REPORT_TARGET_SECTION_NOT_IMPLEMENTED"
      } else "D_REPORT_CLINICAL_SECTION_NOT_IMPLEMENTED"
      testthat::expect_identical(sections[[section]]$status, "unavailable")
      testthat::expect_identical(sections[[section]]$reason_code, expected_reason)
      testthat::expect_length(sections[[section]]$tables, 0L)
    }
  }
})

testthat::test_that("all-off D routes remain not requested without making a complete run look implemented", {
  config <- list(
    clinical_enabled = FALSE, clinical_required = FALSE,
    target_enabled = FALSE, target_required = FALSE,
    route_requirements = list(), integration_requirements = list(fit1 = TRUE),
    analysis = list(contrasts = list("score_slope")),
    blocks = list(list(block_id = "block_1"))
  )
  loaded <- list(
    d = list(
      status = "complete", route_status = list(run_completeness = "complete"),
      target_investigation = list(status = "not_requested"),
      inherited_evidence = list(
        evaluation_evidence = list(controls = list(list(status = "not_run"))),
        stage_status = list(clinical_prediction = list(
          status = "not_started", completion_policy = "conditional",
          model_execution = "not_started", status_scope = "synthetic_fixture_only",
          benefit_guaranteed = FALSE
        ))
      )
    ),
    payloads = list(b = list(status = "complete"), c = list(status = "complete"), clinical = NULL),
    envelopes = list(a = list(status = "complete"))
  )
  results <- data.frame(block_id = "block_1", feature_key = "F1", n_samples = 10,
                        stringsAsFactors = FALSE)
  clinical <- data.frame()
  integrations <- data.frame(integration_id = "fit1", status = "complete", required = TRUE,
                             stringsAsFactors = FALSE)
  routes <- triomics_report_route_rows(loaded$d, config)
  flow <- data.frame(source_sample_count = 10L, source_feature_count = 2L)
  extended <- triomics_report_optional_sections(loaded, config)

  projected <- triomics_report_sections(config, loaded, results, clinical, integrations,
                                        routes, flow, extended)
  statuses <- triomics_report_section_frame(projected$sections)

  testthat::expect_false(any(statuses$required & statuses$status != "complete"))
  testthat::expect_true(all(routes$status == "not_requested"))
  testthat::expect_false(any(routes$required))
  testthat::expect_identical(projected$conditional_statuses$C07, "not_run")
  testthat::expect_identical(projected$conditional_statuses$C09, "not_started")
  testthat::expect_identical(extended$targets$status, "not_requested")
  testthat::expect_identical(extended$clinical$status, "not_requested")
})

testthat::test_that("required unavailable annotation evidence yields partial required sections", {
  config <- list(
    clinical_enabled = FALSE, clinical_required = FALSE,
    target_enabled = FALSE, target_required = FALSE,
    route_requirements = list(gene_reaction = TRUE), integration_requirements = list(fit1 = TRUE),
    analysis = list(contrasts = list("score_slope")),
    blocks = list(list(block_id = "block_1"))
  )
  loaded <- list(
    d = list(
      status = "partial", route_status = list(run_completeness = "incomplete"),
      routes = list(gene_reaction = list(status = "unavailable",
                                        reason_code = "D_ROUTE_CAPABILITY_NOT_IMPLEMENTED",
                                        nominations = list(), candidate_count = 0L)),
      target_investigation = list(status = "not_requested"),
      inherited_evidence = list(
        evaluation_evidence = list(controls = list(list(status = "not_run"))),
        stage_status = list(clinical_prediction = list(status = "not_started",
          completion_policy = "conditional", model_execution = "not_started",
          status_scope = "synthetic_fixture_only", benefit_guaranteed = FALSE))
      )
    ),
    payloads = list(b = list(status = "complete"), c = list(status = "complete"), clinical = NULL),
    envelopes = list(a = list(status = "complete"))
  )
  results <- data.frame(block_id = "block_1", feature_key = "F1", n_samples = 10,
                        stringsAsFactors = FALSE)
  clinical <- data.frame()
  integrations <- data.frame(integration_id = "fit1", status = "complete", required = TRUE,
                             stringsAsFactors = FALSE)
  routes <- triomics_report_route_rows(loaded$d, config)
  flow <- data.frame(source_sample_count = 10L, source_feature_count = 2L)
  extended <- triomics_report_optional_sections(loaded, config)

  projected <- triomics_report_sections(config, loaded, results, clinical, integrations,
                                        routes, flow, extended)
  statuses <- triomics_report_section_frame(projected$sections)
  mapping <- statuses[statuses$section_id == "mapping_enrichment_coverage", , drop = FALSE]
  d_status <- statuses[statuses$section_id == "D", , drop = FALSE]

  testthat::expect_identical(mapping$status, "partial")
  testthat::expect_true(mapping$required)
  testthat::expect_identical(d_status$status, "partial")
  testthat::expect_true(any(statuses$required & statuses$status != "complete"))
})

testthat::test_that("clinical free-text identifiers are withheld from report rows", {
  payload <- list(results = list(list(
    objective_id = "objective-bp-age", endpoint_id = "endpoint-systolic-bp",
    predictor_ref = "age_years",
    clinical_variable = "Participant name: CLINICAL_SENTINEL_810", effect = 0.4,
    effect_unit = "standardized_beta", status = "estimated", n_subjects = 12,
    n_rows_used = 12, source_hash = paste(rep("b", 64), collapse = "")
  )))

  rows <- triomics_report_clinical_rows(payload)
  exported <- paste(unlist(lapply(rows, as.character)), collapse = " ")

  testthat::expect_identical(rows$objective_id, "objective-bp-age")
  testthat::expect_identical(rows$endpoint_id, "endpoint-systolic-bp")
  testthat::expect_identical(rows$clinical_variable, "age_years")
  testthat::expect_false(grepl("CLINICAL_SENTINEL_810|/Users/private|Participant name", exported))
  testthat::expect_equal(rows$effect, 0.4)
  testthat::expect_equal(rows$n_subjects, 12)
})

testthat::test_that("native B-T08 clinical records retain typed adjustments and saved interval bounds", {
  objective <- list(
    objective_id = "objective-bp-age", objective_version = "1",
    analysis_id = "analysis-clinical", predictor_ref = "age_years",
    hypothesis_status = "exploratory", missingness_policy = "complete_case",
    adjustments = list(list(variable = "sex_code", role = "confounder", form = "level",
                            rationale = "Private note /srv/studies/CLINICAL_SENTINEL_42/rationale.txt"))
  )
  estimated <- triomics_b08_blank_result(objective, "endpoint-systolic-bp", "estimated", NULL,
                                          "continuous")
  estimated$clinical_variable <- "Participant name: CLINICAL_SENTINEL_42"
  estimated$effect <- 0.5
  estimated$effect_unit <- "mmHg per year"
  estimated$interval_low <- 0.1
  estimated$interval_high <- 0.5
  estimated$interval_method <- "moderated_t"
  estimated$moderated_se <- 0.1
  estimated$statistic <- 5
  estimated$statistic_name <- "moderated_t"
  estimated$effective_test_df <- 8
  estimated$p <- 0.001
  estimated$adjustment <- objective$adjustments
  estimated$family_id <- "clinical-family-1"
  estimated$source_hash <- paste(rep("a", 64), collapse = "")
  estimated$model_hash <- paste(rep("b", 64), collapse = "")
  estimated$result_id <- "clinical-result-1"
  missing <- triomics_b08_blank_result(
    list(objective_id = "objective-bp-age", predictor_ref = "age_years", adjustments = list()),
    "endpoint-absent", "unavailable", "endpoint_absent", "continuous"
  )

  rows <- triomics_report_clinical_rows(list(results = list(estimated, missing)))

  testthat::expect_equal(rows$interval_lower[[1L]], 0.1)
  testthat::expect_equal(rows$interval_upper[[1L]], 0.5)
  testthat::expect_identical(rows$interval_status, c("source_reported", "not_exported"))
  testthat::expect_identical(rows$objective_id[[1L]], "objective-bp-age")
  testthat::expect_identical(rows$endpoint_id[[1L]], "endpoint-systolic-bp")
  testthat::expect_identical(rows$clinical_variable[[1L]], "age_years")
  testthat::expect_identical(rows$effect_unit[[1L]], "mmHg per year")
  testthat::expect_identical(rows$adjustment_count[[1L]], 1L)
  testthat::expect_identical(rows$adjustment_variables[[1L]], "sex_code")
  testthat::expect_identical(rows$adjustment_roles[[1L]], "confounder")
  testthat::expect_identical(rows$adjustment_forms[[1L]], "level")
  testthat::expect_identical(rows$adjustment_status, c("source_reported", "none_declared"))
  testthat::expect_identical(rows$status, c("estimated", "unavailable"))
  testthat::expect_false(grepl("CLINICAL_SENTINEL_42|/srv/studies|Private note|Participant name",
                               paste(unlist(lapply(rows, as.character)), collapse = " ")))
})

testthat::test_that("arbitrary absolute paths and credentials are withheld without corrupting typed units", {
  unsafe <- c(
    "Source /srv/studies/CLINICAL_SENTINEL_42/results.tsv",
    "Input D:/data/patient.tsv",
    "UNC \\\\clinical-host\\share\\patient.tsv",
    "UNC //clinical-host/share/patient.tsv",
    "file:///srv/studies/CLINICAL_SENTINEL_42/results.tsv",
    "api_key=PRIVATE_KEY_42",
    "Authorization: Bearer PRIVATE_BEARER_42"
  )
  cleaned <- triomics_report_clean_text(unsafe)
  unsafe_identifiers <- vapply(seq_along(unsafe), function(index) {
    error <- tryCatch(triomics_report_identifier_value(unsafe[[index]], "unsafe_identifier"),
                      triomics_error = identity)
    inherits(error, "triomics_error") && identical(error$code, "D_REPORT_SOURCE_INVALID")
  }, logical(1))
  unit <- triomics_report_unit("µmol/L")
  ratio <- triomics_report_unit("mg/dL per kg")

  testthat::expect_true(all(unsafe_identifiers))
  testthat::expect_false(any(grepl("CLINICAL_SENTINEL_42|patient[.]tsv|PRIVATE_KEY_42|PRIVATE_BEARER_42",
                                  cleaned)))
  testthat::expect_identical(unit, "µmol/L")
  testthat::expect_identical(ratio, "mg/dL per kg")
})

testthat::test_that("unsafe producer metadata cannot bypass report manifest sanitation", {
  reference <- list(
    path = "A/assays.json", artifact_type = "assays", artifact_id = "stage-a-assays",
    payload_sha256 = paste(rep("a", 64), collapse = ""),
    envelope_path = "A/assays.envelope.json",
    envelope_sha256 = paste(rep("b", 64), collapse = ""),
    producer = list(package = "triomics", version = "0.1.0",
                    commit = "D:/data/CLINICAL_SENTINEL_42/patient.tsv"),
    status = "complete"
  )
  bundle <- list(artifacts = list(a = reference, b = NULL, c = NULL, d = NULL, clinical = NULL))

  error <- tryCatch(triomics_report_manifest_sources(bundle), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_SOURCE_INVALID")
})

testthat::test_that("D source references bind the displayed B bytes and C native evidence", {
  root <- tempfile("d-t08-lineage-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- "study-lineage"
  analysis <- "analysis-lineage"
  config_sha <- paste(rep("a", 64), collapse = "")
  native_sha <- paste(rep("c", 64), collapse = "")
  native_id <- "c01-native-state-fit1"
  b_payload_path <- file.path(root, "block-results.json")
  b_envelope_path <- file.path(root, "block-results.envelope.json")
  writeLines('{"schema_version":"2.0","study_id":"study-lineage","analysis_id":"analysis-lineage","status":"complete","value":1}',
             b_payload_path, useBytes = TRUE)
  b_envelope <- triomics_artifact_envelope(
    artifact_type = "block_results", artifact_id = "stage-b-block-results", study_id = study,
    run_id = "b-run", status = "complete", config_sha256 = config_sha,
    payload = b_payload_path, producer_commit = "test-commit"
  )
  triomics_write_envelope(b_envelope, b_envelope_path)
  old_b <- list(
    artifact_type = "block_results", artifact_id = "stage-b-block-results",
    path = "B/block-results.json", envelope_path = "B/block-results.envelope.json",
    sha256 = triomics_sha256(b_payload_path), envelope_sha256 = triomics_sha256(b_envelope_path)
  )
  c_evidence <- list(
    schema_version = "2.0", artifact_type = "integration_evidence_bundle",
    study_id = study, analysis_id = analysis, status = "complete",
    integrations = list(list(integration_id = "fit1", status = "complete", backend = "concat_pca",
                             native_artifact_ref = list(artifact_id = native_id, sha256 = native_sha)))
  )
  c_evidence_path <- file.path(root, "integration-evidence.json")
  c_evidence_envelope_path <- file.path(root, "integration-evidence.envelope.json")
  triomics_write_json_atomic(c_evidence, c_evidence_path)
  c_evidence_envelope <- triomics_artifact_envelope(
    artifact_type = "integration_evidence_bundle", artifact_id = "c06-integration-evidence",
    study_id = study, run_id = "c-run", status = "complete", config_sha256 = config_sha,
    payload = c_evidence_path, producer_commit = "test-commit"
  )
  triomics_write_envelope(c_evidence_envelope, c_evidence_envelope_path)
  d_payload <- list(
    immutable_inputs = list(
      b_artifacts = list(block_results = old_b),
      c_artifacts = list(integration_evidence = list(
        artifact_type = "integration_evidence_bundle", artifact_id = "c06-integration-evidence",
        payload_file = "integration-evidence.json", payload_sha256 = triomics_sha256(c_evidence_path),
        envelope_file = "integration-evidence.envelope.json",
        envelope_sha256 = triomics_sha256(c_evidence_envelope_path)
      ))
    ),
    inherited_evidence = list(integration_evidence = c_evidence)
  )
  b_reference <- list(
    path = old_b$path, envelope_path = old_b$envelope_path,
    artifact_type = old_b$artifact_type, artifact_id = old_b$artifact_id,
    payload_sha256 = old_b$sha256, envelope_sha256 = old_b$envelope_sha256
  )
  c_envelope <- list(inputs = list(list(artifact_id = native_id, sha256 = native_sha)))
  c_payload <- list(integrations = list(list(integration_id = "fit1", method = "concat_pca",
                                             status = "complete")))

  testthat::expect_no_error(triomics_report_verify_d_source_lineage(
    b_reference, c_payload, c_envelope, d_payload
  ))
  writeLines('{"schema_version":"2.0","study_id":"study-lineage","analysis_id":"analysis-lineage","status":"complete","value":2}',
             b_payload_path, useBytes = TRUE)
  replacement_envelope <- triomics_artifact_envelope(
    artifact_type = "block_results", artifact_id = "stage-b-block-results", study_id = study,
    run_id = "b-run", status = "complete", config_sha256 = config_sha,
    payload = b_payload_path, producer_commit = "test-commit"
  )
  triomics_write_envelope(replacement_envelope, file.path(root, "replacement-envelope.json"))
  replacement_ref <- b_reference
  replacement_ref$payload_sha256 <- triomics_sha256(b_payload_path)
  replacement_ref$envelope_sha256 <- triomics_sha256(file.path(root, "replacement-envelope.json"))
  error <- tryCatch(triomics_report_verify_d_source_lineage(
    replacement_ref, c_payload, c_envelope, d_payload
  ),
                    triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_SOURCE_INVALID")
})

testthat::test_that("C reportable values must match its hash-bound D integration evidence", {
  root <- tempfile("d-t08-c-lineage-")
  dir.create(root)
  dir.create(file.path(root, "C"))
  dir.create(file.path(root, "D"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- "study"
  analysis <- "analysis"
  config_sha <- paste(rep("a", 64), collapse = "")
  preparation_context <- "prep-1"
  representation <- "levels"
  native_ref <- list(artifact_id = "c01-native-state-fit1",
                     sha256 = paste(rep("c", 64), collapse = ""))
  feature_flow <- list(block_1 = list(input = list("feature-a"), selected = list("feature-a"),
                                     retained = list("feature-a"), zero_variance = list(),
                                     excluded_by_selection = list()))
  rank_lineage <- list(training_unit_count = 8L, selected_feature_count = 1L,
                       maximum_centered_rank = 4L, numerical_rank = 1L,
                       requested_components = 1L, retained_components = 1L,
                       disposition = "complete")
  common <- list(
    selected = list(block_1 = list("feature-a")),
    variance = triomics_c06_matrix_payload(
      matrix(0.25, nrow = 1L, dimnames = list("block_1", "PC1")), "test_common_variance"
    ),
    parameters = list(retained_n_components = 1L, rank_lineage = rank_lineage,
                      feature_flow = feature_flow,
                      effective = list(selection = list(rule = "none")))
  )
  variance_definition <- list(metric = "projection_fraction", assay_space = "training",
                              denominator = "source_ss", semantics = "incremental")
  common_variance <- triomics_c06_matrix_payload(
    matrix(0.25, nrow = 1L, dimnames = list("block_1", "PC1")), "test_common_variance"
  )
  d_evidence <- list(
    integration_id = "fit1", status = "complete", backend = "concat_pca",
    native_artifact_ref = native_ref,
    context = list(preparation_context_id = preparation_context, representation_id = representation),
    common = common,
    variance = list(common_values = common_variance, common_definition = variance_definition),
    selection = list(rule = list(rule = "none", criterion = "retain_all_training_nonzero_variance_features",
                                  value = NULL))
  )
  public_c_evidence <- list(
    schema_version = "2.0", artifact_type = "integration_evidence_bundle",
    study_id = study, analysis_id = analysis,
    preparation_context_id = preparation_context, representation_id = representation,
    status = "complete", integrations = list(d_evidence)
  )
  public_c_path <- file.path(root, "D", "integration-evidence.json")
  public_c_envelope_path <- file.path(root, "D", "integration-evidence.envelope.json")
  triomics_write_json_atomic(public_c_evidence, public_c_path)
  public_c_envelope <- triomics_artifact_envelope(
    artifact_type = "integration_evidence_bundle", artifact_id = "c06-integration-evidence",
    study_id = study, run_id = "c-run", status = "complete", config_sha256 = config_sha,
    payload = public_c_path, producer_commit = "test-commit"
  )
  triomics_write_envelope(public_c_envelope, public_c_envelope_path)
  d_payload <- list(
    immutable_inputs = list(c_artifacts = list(integration_evidence = list(
      artifact_type = "integration_evidence_bundle", artifact_id = "c06-integration-evidence",
      payload_file = "integration-evidence.json", payload_sha256 = triomics_sha256(public_c_path),
      envelope_file = "integration-evidence.envelope.json",
      envelope_sha256 = triomics_sha256(public_c_envelope_path)
    ))),
    inherited_evidence = list(integration_evidence = public_c_evidence)
  )
  config <- list(study_id = study, analysis_id = analysis, config_sha256 = config_sha)
  testthat::expect_no_error(triomics_report_verify_d_public_c_evidence(root, d_payload, config))

  c_values <- list(
    schema_version = "2.0", study_id = study, analysis_id = analysis,
    preparation_context_id = preparation_context, representation_id = representation,
    integrations = list(list(
      integration_id = "fit1", method = "concat_pca", status = "complete",
      result = list(
        variance = list(rows = list(block_ids = list("block_1")),
                        columns = list(component_ids = list("PC1")), values = list(list(0.25)),
                        metric = variance_definition$metric, assay_space = variance_definition$assay_space,
                        denominator = variance_definition$denominator,
                        semantics = variance_definition$semantics),
        selected = common$selected,
        params = common$parameters
      )
    )))
  c_path <- file.path(root, "C", "integration-results.json")
  c_envelope_path <- file.path(root, "C", "integration-results.envelope.json")
  write_c_result <- function() {
    unlink(c(c_path, c_envelope_path), force = TRUE)
    triomics_write_json_atomic(c_values, c_path)
    envelope <- triomics_artifact_envelope(
      artifact_type = "integration_results", artifact_id = "stage-c-integration-results",
      study_id = study, run_id = "c-run", status = "complete", config_sha256 = config_sha,
      payload = c_path, producer_commit = "test-commit", inputs = list(native_ref)
    )
    envelope$analysis_id <- analysis
    triomics_write_envelope(envelope, c_envelope_path)
  }
  write_c_result()
  c_artifact <- triomics_report_artifact(
    root, file.path("C", "integration-results.json"), "integration_results",
    "stage-c-integration-results", study, config_sha, analysis
  )

  testthat::expect_no_error(triomics_report_verify_c_projection(
    c_artifact$payload, c_artifact$envelope, d_payload
  ))
  # A newly rehashed C result keeps its native references but changes a displayed statistic.
  c_values$integrations[[1L]]$result$variance$values[[1L]][[1L]] <- 0.5
  write_c_result()
  changed_c_artifact <- triomics_report_artifact(
    root, file.path("C", "integration-results.json"), "integration_results",
    "stage-c-integration-results", study, config_sha, analysis
  )
  error <- tryCatch(triomics_report_verify_c_projection(
    changed_c_artifact$payload, changed_c_artifact$envelope, d_payload
  ), triomics_error = identity)

  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "D_REPORT_SOURCE_INVALID")
})

testthat::test_that("enrichment rows retain producer claim identity and resource bindings", {
  family <- triomics_ora_family(
    universe = list(source = "measured", ids = c("entity-a", "entity-b", "entity-c", "entity-d")),
    sets = list(list(set_id = "set-metabolic", members = c("entity-a", "entity-b"), eligible = TRUE)),
    nominated = "entity-a", inventory = list(status = "full_unselected"),
    mapping = list(rule = "exact_identifier", status = "compatible")
  )
  identity <- list(
    study_id = "study-ora", analysis_id = "analysis-ora", preparation_context_id = "prep-1",
    representation_id = "levels", block_id = "block-metabolome", source_branch = "primary_statistics",
    claim_id = "contrast-lipid", integration_id = "", family_id = "family-ora",
    source_ids = list("source-metabolome"), nomination_ids = list("nomination-1")
  )
  detail <- list(
    status = "complete", identity = identity,
    protocol = list(resource_id = "ora-protocol-v1", sha256 = paste(rep("a", 64), collapse = ""),
                    mapping_rule = "exact_identifier", aggregation_rule = "unique_entities",
                    ambiguity_rule = "exclude_ambiguous"),
    mapping = list(status = "compatible",
                   resource = list(resource_id = "mapping-v1", version = "2026.1")),
    measured_universe = list(source = "measured", sha256 = paste(rep("b", 64), collapse = "")),
    set_family = list(sha256 = paste(rep("c", 64), collapse = ""), declared = "all_sets",
                      declared_count = family$full_family_size, result = family),
    bindings = list(protocol_sha256 = paste(rep("a", 64), collapse = ""),
                    mapping_sha256 = paste(rep("d", 64), collapse = ""),
                    set_family_sha256 = paste(rep("c", 64), collapse = ""),
                    measured_universe_sha256 = paste(rep("b", 64), collapse = "")),
    interpretation = "over_representation_not_pathway_activation_or_patient_score"
  )
  summary <- list(
    identity = identity, status = detail$status, family_size = family$full_family_size,
    tests_in_bh = family$tests_in_bh, estimable_count = family$estimable_count,
    sets = family$sets, interpretation = "not_pathway_activation"
  )

  rows <- triomics_report_enrichment_rows(list(
    enrichment = list(results = list(summary)), routes = list(ora = list(claims = list(detail)))
  ))

  testthat::expect_identical(rows$block_id, "block-metabolome")
  testthat::expect_identical(rows$claim_id, "contrast-lipid")
  testthat::expect_identical(rows$source_branch, "primary_statistics")
  testthat::expect_identical(rows$family_id, "family-ora")
  testthat::expect_identical(rows$protocol_resource_id, "ora-protocol-v1")
  testthat::expect_identical(rows$mapping_resource_id, "mapping-v1")
  testthat::expect_identical(rows$mapping_resource_version, "2026.1")
  testthat::expect_identical(rows$interpretation,
                            "over_representation_not_pathway_activation_or_patient_score")
  testthat::expect_identical(rows$measured_universe_sha256, paste(rep("b", 64), collapse = ""))
  testthat::expect_identical(rows$mapping_sha256, paste(rep("d", 64), collapse = ""))
  testthat::expect_identical(rows$set_id, "set-metabolic")
  testthat::expect_false(grepl("Enrichment claim 1", paste(unlist(lapply(rows, as.character)), collapse = " ")))
})

testthat::test_that("fixed optional-section provider status is execution status, not project acceptance", {
  collector <- triomics_report_optional_sections
  provider_env <- new.env(parent = environment(collector))
  assign("triomics_report_targets_section", function(loaded, config) {
    list(status = "complete", reason_code = "", tables = list(coverage = data.frame(rows = 1L)))
  }, envir = provider_env)
  assign("triomics_report_clinical_section", function(loaded, config) {
    list(status = "partial", reason_code = "D_REPORT_CLINICAL_EXPORT_PARTIAL", tables = list())
  }, envir = provider_env)
  environment(collector) <- provider_env
  sections <- collector(list(), list(target_enabled = TRUE, clinical_enabled = TRUE))
  routes <- data.frame(route = c("mofa_native", "biopan"), status = c("not_requested", "not_requested"),
                       stringsAsFactors = FALSE)
  capabilities <- triomics_report_conditional_capabilities(routes, "not_run", list(), sections)
  html <- paste(
    triomics_report_optional_section_html(sections$targets),
    triomics_report_optional_section_html(sections$clinical),
    triomics_report_html_table(capabilities), collapse = "\n"
  )
  json <- triomics_canonical_json(triomics_report_sanitize_value(list(
    extended_sections = sections,
    conditional_capabilities = triomics_report_data_frame_records(capabilities)
  )))

  testthat::expect_identical(sections$targets$status, "complete")
  testthat::expect_identical(sections$clinical$status, "partial")
  testthat::expect_identical(capabilities$status[capabilities$capability == "D-T13 target dossier"],
                             "complete")
  testthat::expect_identical(capabilities$status[capabilities$capability == "D-T15 clinical report"],
                             "partial")
  testthat::expect_identical(capabilities$project_acceptance_status[
    capabilities$capability %in% c("D-T13 target dossier", "D-T15 clinical report")
  ], rep("not_assessed", 2L))
  testthat::expect_true(grepl("complete|partial|D_REPORT_CLINICAL_EXPORT_PARTIAL", html))
  testthat::expect_true(grepl("complete|partial|D_REPORT_CLINICAL_EXPORT_PARTIAL|not_assessed", json))
  testthat::expect_false(grepl("not implemented", paste(html, json)))
})
