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
                        status = "unavailable", family_size = 2L, tests_in_bh = 0L,
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

testthat::test_that("future target and clinical sections have fixed unavailable-by-default slots", {
  sections <- triomics_report_optional_sections(
    list(), list(target_enabled = TRUE, clinical_enabled = TRUE)
  )

  testthat::expect_identical(names(sections), c("targets", "clinical"))
  testthat::expect_identical(sections$targets$status, "unavailable")
  testthat::expect_identical(sections$targets$reason_code, "D_REPORT_TARGET_SECTION_NOT_IMPLEMENTED")
  testthat::expect_identical(sections$clinical$status, "unavailable")
  testthat::expect_identical(sections$clinical$reason_code, "D_REPORT_CLINICAL_SECTION_NOT_IMPLEMENTED")
  testthat::expect_length(sections$targets$tables, 0L)
  testthat::expect_length(sections$clinical$tables, 0L)
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
    objective_id = "CLINICAL_SENTINEL_810", endpoint_id = "/Users/private/patient.tsv",
    clinical_variable = "Participant name: CLINICAL_SENTINEL_810", effect = 0.4,
    effect_unit = "standardized_beta", status = "estimated", n_subjects = 12,
    n_rows_used = 12, source_hash = paste(rep("b", 64), collapse = "")
  )))

  rows <- triomics_report_clinical_rows(payload)
  exported <- paste(unlist(lapply(rows, as.character)), collapse = " ")

  testthat::expect_identical(rows$objective_id, "Objective 1")
  testthat::expect_identical(rows$endpoint_id, "Endpoint 1")
  testthat::expect_identical(rows$clinical_variable, "Clinical measure 1")
  testthat::expect_false(grepl("CLINICAL_SENTINEL_810|/Users/private|Participant name", exported))
  testthat::expect_equal(rows$effect, 0.4)
  testthat::expect_equal(rows$n_subjects, 12)
})
