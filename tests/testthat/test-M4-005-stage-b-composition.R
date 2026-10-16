m4_005b_read_json <- function(path) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}

m4_005b_expected_b04_matrix <- function() {
  subjects <- sprintf("sub%04d", 1:4)
  out <- rbind(
    F1 = c(5.2, 6.1, 8.2, 7.2, 8.1, 10.2, 9.2, 10.1, 12.2, 11.2, 12.1, 14.2),
    F2 = c(5.2, 4.35, 4.7, 7.2, 6.35, 6.7, 9.2, 8.35, 8.7, 11.2, 10.35, 10.7)
  )
  colnames(out) <- unlist(lapply(subjects, function(subject) {
    paste0(subject, "_t", 0:2)
  }), use.names = FALSE)
  out
}

m4_005b_json_names <- function(value) {
  if (!is.list(value)) return(character())
  value_names <- names(value)
  if (is.null(value_names)) value_names <- character()
  c(value_names, unlist(lapply(value, m4_005b_json_names), use.names = FALSE))
}

m4_005b_run_b04 <- function(execute_stage_b) {
  config <- m4_005b_rewrite_b04_config()
  on.exit(unlink(config), add = TRUE)
  output <- tempfile("m4-005b-b-t04-")
  value <- tryCatch(
    execute_stage_b(config, output),
    error = identity
  )
  list(
    error = if (inherits(value, "error")) value else NULL,
    result = if (inherits(value, "error")) NULL else value,
    output = output
  )
}

m4_005b_assert_b04_result <- function(run) {
  if (!is.null(run$error)) {
    testthat::expect_s3_class(run$error, "triomics_error")
    testthat::expect_identical(run$error$code, "A_ID_REFERENCE")
  }
  testthat::expect_null(run$error, info = if (!is.null(run$error)) {
    paste(class(run$error), conditionMessage(run$error), sep = ": ")
  })
  if (!is.null(run$error)) return(invisible(NULL))

  testthat::expect_identical(run$result$status, "complete")
  testthat::expect_true(file.exists(run$result$payload_path))
  payload <- m4_005b_read_json(run$result$payload_path)
  row <- payload$blocks$block_1$contrasts$per_synthetic_step$rows[[1L]]
  resolved_matrices <- run$result$resolved_matrices
  expected_matrix <- m4_005b_expected_b04_matrix()
  assay_hash <- triomics_b_assay_hash("block_1", resolved_matrices$block_1)
  testthat::expect_identical(payload$representation_id, "subject_centered")
  testthat::expect_equal(row$effect, 1.5, tolerance = 1e-8)
  testthat::expect_identical(row$ordinary_df, 7L)
  testthat::expect_identical(row$design_rank, 5L)
  testthat::expect_identical(row$duplicate_correlation_applied, FALSE)
  testthat::expect_identical(names(resolved_matrices), "block_1")
  testthat::expect_identical(resolved_matrices$block_1, expected_matrix)
  testthat::expect_identical(row$source_hash, assay_hash)
  testthat::expect_false(any(c(
    "resolved_matrices", "assay", "assays", "fit", "native_model", "model_object",
    "lmFit", "MArrayLM"
  ) %in% m4_005b_json_names(payload)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(
    file.path(run$output, "B", "block-results.envelope.json")
  )))

  calibration <- m4_005b_read_json(file.path(run$output, "calibration.json"))
  testthat::expect_identical(calibration$status, "pass")
  testthat::expect_true(isTRUE(calibration$executed))
  testthat::expect_false(isTRUE(calibration$skipped))

  acceptance <- m4_005b_read_json(file.path(run$output, "acceptance-record.json"))
  verdicts <- vapply(acceptance$acceptance, function(item) item$verdict, character(1))
  testthat::expect_setequal(
    vapply(acceptance$acceptance, function(item) item$id, character(1)),
    paste0("B-T04-AC", 1:4)
  )
  testthat::expect_true(all(verdicts == "pass"))
}

m4_005b_rewrite_b04_config <- function() {
  fixture <- triomics_test_resource("examples", "tickets", "B-T04.yaml")
  metadata <- triomics_test_resource("examples", "data", "paired", "metadata.tsv")
  proteomics <- triomics_test_resource("examples", "data", "paired", "proteomics.tsv")
  lines <- readLines(fixture, warn = FALSE)
  lines <- gsub(
    "../../../discovery/spec-v2/examples/paired/metadata.tsv",
    paste0("\"", metadata, "\""), lines, fixed = TRUE
  )
  lines <- gsub(
    "../../../discovery/spec-v2/examples/paired/proteomics.tsv",
    paste0("\"", proteomics, "\""), lines, fixed = TRUE
  )
  config <- tempfile("m4-005b-b-t04-config-", fileext = ".yaml")
  writeLines(lines, config, useBytes = TRUE)
  config
}

m4_005b_source_cli_b04 <- function() {
  cli <- triomics_test_resource("exec", "triomics")
  config <- m4_005b_rewrite_b04_config()
  output <- tempfile("m4-005b-source-b-t04-")
  old_wd <- setwd(dirname(cli))
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(config), add = TRUE)

  lines <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla", shQuote(basename(cli)),
      "--config", shQuote(config),
      "--stage", "B",
      "--output", shQuote(output)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(lines, "status")
  if (is.null(status)) status <- 0L
  list(status = status, lines = lines, output = output)
}

m4_005b_b05_config <- function(fallback) {
  fixture <- triomics_test_resource("examples", "tickets", "B-T05.yaml")
  metadata <- triomics_test_resource("examples", "data", "paired", "metadata.tsv")
  proteomics <- triomics_test_resource("examples", "data", "paired", "proteomics.tsv")
  source <- paste(readLines(fixture, warn = FALSE), collapse = "\n")
  source <- gsub(
    "file: ../data/paired/metadata.tsv",
    paste0("file: \"", metadata, "\""), source, fixed = TRUE
  )
  source <- gsub(
    "file: ../data/paired/proteomics.tsv",
    paste0("file: \"", proteomics, "\""), source, fixed = TRUE
  )
  source <- sub(
    "fallback: none", paste0("fallback: ", fallback), source, fixed = TRUE
  )
  path <- tempfile("m4-005b-b-t05-config-", fileext = ".yaml")
  writeLines(source, path, useBytes = TRUE)
  path
}

m4_005b_run_b05 <- function(fallback) {
  config <- m4_005b_b05_config(fallback)
  output <- tempfile("m4-005b-b-t05-")
  value <- tryCatch(
    triomics_execute_stage_b(config, output),
    error = identity
  )
  payload_path <- file.path(output, "B", "block-results.json")
  list(
    error = if (inherits(value, "error")) value else NULL,
    result = if (inherits(value, "error")) NULL else value,
    payload = if (file.exists(payload_path)) m4_005b_read_json(payload_path) else NULL,
    output = output,
    cleanup = c(config, output)
  )
}

m4_005b_b06_fixture <- function() {
  samples <- c("c1", "c2", "c3", "t1", "t2", "t3")
  assay <- rbind(
    strong = c(1, 1, 1, 8, 8, 8),
    weak = c(1, 2, 1, 1, 2, 1),
    failed = rep(NA_real_, length(samples))
  )
  colnames(assay) <- samples
  study <- list(
    study_id = "synthetic_b_t06",
    preparation_context_id = "b_t06",
    sample_metadata = data.frame(
      sample_id = samples,
      group = rep(c("control", "treatment"), each = 3L),
      stringsAsFactors = FALSE
    ),
    sample_registry = data.frame(
      sample_id = samples,
      subject_id = NA_character_,
      visit_id = NA_character_,
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(group = c("control", "treatment")),
    feature_registry = data.frame(
      feature_key = rownames(assay),
      block_id = "block_1",
      scale = "linear",
      stringsAsFactors = FALSE
    ),
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = NULL,
    imported_pq = data.frame(feature_key = "strong", p = 0, q = 0),
    selection = data.frame(
      block_id = "block_1",
      source_id = "block_1_source",
      selection_history = "full_unselected",
      inference_gate = "declared_unselected",
      stringsAsFactors = FALSE
    )
  )
  analysis <- list(
    analysis_id = "b_t06_primary",
    block_ids = "block_1",
    preparation_context_id = "b_t06",
    design = list(
      kind = "independent",
      condition = "group",
      condition_type = "factor",
      subject_id = NULL,
      repeated = FALSE,
      representation = "levels",
      reference_level = "control",
      target_level = "treatment",
      between_group = NULL,
      visit_column = NULL,
      reference_visit = NULL,
      target_visit = NULL,
      covariates = list(),
      missing_visits = "error",
      time_column = NULL,
      time_unit = NULL,
      time_origin = NULL,
      spline_knots = list(),
      spline_boundary = list(),
      residual_contract_ref = NULL,
      mean_weights = NULL,
      interactions = list()
    )
  )
  statistics <- list(
    method = "limma",
    contrasts = list(list(
      contrast_id = "treatment_minus_control",
      weights = c("condition.control" = -1, "condition.treatment" = 1),
      interpretation = "Synthetic treatment minus control."
    )),
    trend = FALSE,
    robust = FALSE,
    q_threshold = 0.05,
    effect_threshold = 0.5,
    effect_unit = "group_difference",
    family_scope = "one_scalar_contrast_all_retained_features",
    selection_validity = "qualified",
    selection_justification = "Synthetic fixture; inference is not validated.",
    dependence_justification = "Synthetic fixture; no scientific dependence claim.",
    rank_sensitivity = TRUE,
    count_metadata = NULL,
    fallback = "none"
  )
  list(study = study, analysis = analysis, statistics = statistics)
}

m4_005b_fit_b06_fixture <- function(weights) {
  fixture <- m4_005b_b06_fixture()
  fixture$statistics$contrasts[[1L]]$weights <- weights
  resolved <- triomics_resolve_analysis(fixture$study, fixture$analysis)
  list(
    resolved = resolved,
    fit = triomics_fit_block_models(resolved, fixture$statistics)
  )
}

testthat::test_that("M4-005B installed composition preserves B-T04 trajectory fitting", {
  namespace <- asNamespace("triomics")
  execute_stage_b <- get("triomics_execute_stage_b", envir = namespace, inherits = FALSE)
  run <- m4_005b_run_b04(execute_stage_b)
  on.exit(unlink(run$output, recursive = TRUE), add = TRUE)

  m4_005b_assert_b04_result(run)
})

testthat::test_that("M4-005B source composition preserves B-T04 stage execution", {
  run <- m4_005b_source_cli_b04()
  if (!identical(run$status, 0L)) {
    testthat::expect_match(paste(run$lines, collapse = "\n"), "triomics A_ID_REFERENCE:")
  }
  testthat::expect_identical(run$status, 0L, info = paste(run$lines, collapse = "\n"))
  if (!identical(run$status, 0L)) return(invisible(NULL))

  payload <- m4_005b_read_json(file.path(run$output, "B", "block-results.json"))
  row <- payload$blocks$block_1$contrasts$per_synthetic_step$rows[[1L]]
  expected_matrix <- m4_005b_expected_b04_matrix()
  assay_hash <- triomics_b_assay_hash("block_1", expected_matrix)
  testthat::expect_identical(payload$status, "complete")
  testthat::expect_identical(payload$representation_id, "subject_centered")
  testthat::expect_equal(row$effect, 1.5, tolerance = 1e-8)
  testthat::expect_identical(row$ordinary_df, 7L)
  testthat::expect_identical(row$design_rank, 5L)
  testthat::expect_identical(row$source_hash, assay_hash)
  testthat::expect_false(any(c(
    "resolved_matrices", "assay", "assays", "fit", "native_model", "model_object",
    "lmFit", "MArrayLM"
  ) %in% m4_005b_json_names(payload)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(
    file.path(run$output, "B", "block-results.envelope.json")
  )))

  handoff_root <- file.path(run$output, "B", "handoffs")
  handoff_dirs <- if (dir.exists(handoff_root)) {
    list.dirs(handoff_root, recursive = FALSE, full.names = TRUE)
  } else {
    character()
  }
  testthat::expect_length(handoff_dirs, 1L)
  if (length(handoff_dirs) != 1L) return(invisible(NULL))

  handoff_dir <- handoff_dirs[[1L]]
  bundle_path <- file.path(handoff_dir, "bundle.json")
  bundle_envelope_path <- file.path(handoff_dir, "bundle.envelope.json")
  matrix_manifest_path <- file.path(handoff_dir, "stage-c-matrices.json")
  matrix_manifest_envelope_path <- file.path(handoff_dir, "stage-c-matrices.envelope.json")
  matrix_values_path <- file.path(handoff_dir, "stage-c-matrix-values.rds")
  matrix_values_envelope_path <- file.path(handoff_dir, "stage-c-matrix-values.envelope.json")
  receipt_path <- file.path(handoff_dir, "acceptance-record.json")
  receipt_envelope_path <- file.path(handoff_dir, "acceptance-record.envelope.json")
  testthat::expect_true(all(file.exists(c(
    bundle_path, matrix_manifest_path, matrix_values_path, receipt_path
  ))))
  if (!all(file.exists(c(bundle_path, matrix_manifest_path, matrix_values_path, receipt_path)))) {
    return(invisible(NULL))
  }

  bundle <- m4_005b_read_json(bundle_path)
  matrix_manifest <- m4_005b_read_json(matrix_manifest_path)
  handoff_matrices <- readRDS(matrix_values_path)
  receipt <- m4_005b_read_json(receipt_path)
  testthat::expect_identical(bundle$matrix_handoff$status, "complete")
  testthat::expect_identical(bundle$matrix_handoff$source, "stage_b_resolved_blocks")
  testthat::expect_identical(matrix_manifest$matrix_source, "stage_b_resolved_blocks")
  testthat::expect_identical(names(handoff_matrices), "block_1")
  testthat::expect_identical(handoff_matrices$block_1, expected_matrix)
  testthat::expect_identical(
    triomics_b_assay_hash("block_1", handoff_matrices$block_1), row$source_hash
  )
  ac3 <- Filter(function(item) identical(as.character(item$id), "AC3"), receipt$acceptance)
  testthat::expect_length(ac3, 1L)
  testthat::expect_identical(ac3[[1L]]$verdict, "pass")
  testthat::expect_true(isTRUE(triomics_validate_envelope(bundle_envelope_path)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(matrix_manifest_envelope_path)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(matrix_values_envelope_path)))
  testthat::expect_true(isTRUE(triomics_validate_envelope(receipt_envelope_path)))

  calibration <- m4_005b_read_json(file.path(run$output, "calibration.json"))
  acceptance <- m4_005b_read_json(file.path(run$output, "acceptance-record.json"))
  verdicts <- vapply(acceptance$acceptance, function(item) item$verdict, character(1))
  testthat::expect_identical(calibration$status, "pass")
  testthat::expect_true(isTRUE(calibration$executed))
  testthat::expect_false(isTRUE(calibration$skipped))
  testthat::expect_true(all(verdicts == "pass"))
})

testthat::test_that("M4-005B composition preserves B-T05 unavailable and fallback routing", {
  unavailable <- m4_005b_run_b05("none")
  on.exit(unlink(unavailable$cleanup, recursive = TRUE), add = TRUE)
  testthat::expect_null(unavailable$error, info = if (!is.null(unavailable$error)) {
    paste(class(unavailable$error), conditionMessage(unavailable$error), sep = ": ")
  })
  if (!is.null(unavailable$error)) return(invisible(NULL))
  testthat::expect_identical(unavailable$result$status, "unavailable")
  testthat::expect_identical(unavailable$result$method_status, "unavailable")
  testthat::expect_identical(unavailable$payload$status, "unavailable")
  testthat::expect_identical(unavailable$payload$method_status, "unavailable")
  testthat::expect_identical(unavailable$payload$requested_method, "DEqMS")
  testthat::expect_identical(unavailable$payload$method, "DEqMS")
  testthat::expect_identical(unavailable$payload$unavailable_reason, "count_metadata_missing")
  testthat::expect_false(isTRUE(unavailable$payload$fallback_applied))

  fallback <- m4_005b_run_b05("explicit_limma")
  on.exit(unlink(fallback$cleanup, recursive = TRUE), add = TRUE)
  testthat::expect_null(fallback$error, info = if (!is.null(fallback$error)) {
    paste(class(fallback$error), conditionMessage(fallback$error), sep = ": ")
  })
  if (!is.null(fallback$error)) return(invisible(NULL))
  testthat::expect_identical(fallback$result$status, "complete")
  testthat::expect_identical(fallback$result$method_status, "fallback_complete")
  testthat::expect_identical(fallback$payload$status, "complete")
  testthat::expect_identical(fallback$payload$method_status, "fallback_complete")
  testthat::expect_identical(fallback$payload$requested_method, "DEqMS")
  testthat::expect_identical(fallback$payload$method, "limma_explicit_fallback")
  testthat::expect_true(isTRUE(fallback$payload$fallback_applied))
  testthat::expect_identical(fallback$payload$fallback_provenance$status, "applied")
  testthat::expect_identical(
    fallback$payload$fallback_provenance$trigger_reason,
    "count_metadata_missing"
  )
})

testthat::test_that("M4-005B composition preserves B-T06 family and rank context", {
  canonical <- m4_005b_fit_b06_fixture(
    c("condition.control" = -1, "condition.treatment" = 1)
  )
  result <- canonical$fit$blocks$block_1$contrasts$treatment_minus_control
  family <- result$family
  testthat::expect_identical(family$n_declared, 3L)
  testthat::expect_identical(family$n_failed, 1L)
  testthat::expect_identical(family$n_evaluated, 3L)
  testthat::expect_identical(sum(family$dispositions), family$n_declared)
  testthat::expect_identical(family$internal_p[[match("failed", family$member_ids)]], 1)
  testthat::expect_identical(result$sensitivity$status, "complete")

  rank_block <- list(
    feature_keys = "feature",
    assay = matrix(c(1, 1, 1, 8, 8, 8), nrow = 1L,
                   dimnames = list("feature", NULL)),
    groups = c("control", "control", "control", "treatment", "treatment", "treatment")
  )
  rank_plan <- list(
    reference_level = "control",
    target_level = "treatment",
    levels = c("control", "treatment")
  )
  two_group_rank <- get(
    "triomics_b06_two_group_rank", envir = asNamespace("triomics"), inherits = FALSE
  )
  rank_for <- function(weights) {
    two_group_rank(rank_block, rank_plan, list(weights = weights))
  }
  canonical_rank <- rank_for(c("condition.control" = -1, "condition.treatment" = 1))
  incompatible_rank <- rank_for(c("condition.control" = 0, "condition.treatment" = 1e-9))
  tiny_rank <- rank_for(c("condition.control" = -1e-9, "condition.treatment" = 1e-9))
  extreme_rank <- rank_for(c("condition.control" = -1e308, "condition.treatment" = 1e308))
  direct <- stats::wilcox.test(
    c(8, 8, 8), c(1, 1, 1), exact = FALSE, correct = TRUE, alternative = "two.sided"
  )

  testthat::expect_identical(canonical_rank$status, "complete")
  testthat::expect_identical(canonical_rank$direction_sign, "positive_target_minus_reference")
  testthat::expect_equal(canonical_rank$features[[1L]]$p, direct$p.value)
  testthat::expect_null(incompatible_rank)
  testthat::expect_identical(tiny_rank$status, "complete")
  testthat::expect_identical(tiny_rank$direction_sign, "positive_target_minus_reference")
  testthat::expect_equal(tiny_rank$features[[1L]]$p, canonical_rank$features[[1L]]$p)
  testthat::expect_identical(extreme_rank$status, "complete")
  testthat::expect_identical(extreme_rank$direction_sign, "positive_target_minus_reference")
  testthat::expect_equal(extreme_rank$features[[1L]]$p, canonical_rank$features[[1L]]$p)
})

testthat::test_that("M4-005B installers are idempotent", {
  namespace <- asNamespace("triomics")
  namespace_installer <- get0(
    "triomics_install_stage_b_namespace_dispatch",
    envir = namespace,
    inherits = FALSE
  )
  testthat::expect_true(is.function(namespace_installer))
  if (is.function(namespace_installer)) {
    namespace_installer("triomics")
    namespace_fit <- get("triomics_fit_block_models", envir = namespace, inherits = FALSE)
    namespace_stage <- get("triomics_execute_stage_b", envir = namespace, inherits = FALSE)
    namespace_installer("triomics")
    testthat::expect_identical(
      get("triomics_fit_block_models", envir = namespace, inherits = FALSE), namespace_fit
    )
    testthat::expect_identical(
      get("triomics_execute_stage_b", envir = namespace, inherits = FALSE), namespace_stage
    )
  }

  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  script <- tempfile("m4-005b-global-idempotence-", fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  script_lines <- c(
    paste0("source_root <- ", encodeString(root, quote = "\"")),
    "if (dir.exists(file.path(source_root, 'R'))) {",
    "  options(triomics.source_root = source_root)",
    "  sources <- sort(list.files(file.path(source_root, 'R'), pattern = '\\\\.R$', full.names = TRUE))",
    "  for (file in sources) sys.source(file, envir = .GlobalEnv)",
    "} else {",
    "  library(triomics)",
    "  namespace <- asNamespace('triomics')",
    "  namespace_names <- ls(namespace, all.names = TRUE)",
    "  for (name in namespace_names) {",
    "    value <- get(name, envir = namespace, inherits = FALSE)",
    "    if (is.function(value)) assign(name, value, envir = .GlobalEnv)",
    "  }",
    paste0(
      "  required <- c(",
      "'triomics_b02_install_global_dispatch', ",
      "'triomics_b03_install_global_dispatch', ",
      "'triomics_b04_install_global_dispatch', ",
      "'triomics_b04_install_stage_hook', ",
      "'triomics_b05_install_global_dispatch', ",
      "'triomics_b06_install_global_dispatch', ",
      "'triomics_b06_install_source_context')"
    ),
    paste0(
      "  stopifnot(all(vapply(required, function(name) ",
      "is.function(get0(name, envir = .GlobalEnv, inherits = FALSE)), logical(1))))"
    ),
    "}",
    paste0(
      "if (!is.function(get0('triomics_install_stage_b_global_dispatch', ",
      "envir = .GlobalEnv, inherits = FALSE))) stop('missing triomics_install_stage_b_global_dispatch')"
    ),
    "triomics_install_stage_b_global_dispatch()",
    paste0(
      "stopifnot(",
      "identical(attr(triomics_fit_block_models, 'b_t02_dispatch'), TRUE), ",
      "identical(attr(triomics_fit_block_models, 'b_t03_dispatch'), TRUE), ",
      "identical(attr(triomics_fit_block_models, 'b_t04_dispatch'), TRUE), ",
      "identical(attr(triomics_fit_block_models, 'b_t05_dispatch'), TRUE), ",
      "identical(attr(triomics_fit_block_models, 'b_t06_dispatch'), TRUE), ",
      "identical(attr(triomics_execute_stage_b, 'b_t04_stage'), TRUE))"
    ),
    "fit_after_first <- triomics_fit_block_models",
    "stage_after_first <- triomics_execute_stage_b",
    "triomics_install_stage_b_global_dispatch()",
    "stopifnot(identical(triomics_fit_block_models, fit_after_first))",
    "stopifnot(identical(triomics_execute_stage_b, stage_after_first))"
  )
  writeLines(script_lines, script, useBytes = TRUE)
  lines <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(lines, "status")
  if (is.null(status)) status <- 0L
  testthat::expect_identical(status, 0L, info = paste(lines, collapse = "\n"))
})
