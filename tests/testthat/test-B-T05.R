b05_values <- function(n_features = 12L) {
  feature_index <- seq_len(n_features)
  sample_index <- seq_len(6L)
  groups <- rep(c("control", "treatment"), each = 3L)
  matrix <- outer(feature_index, sample_index, function(feature, sample) {
    10 + feature / 10 + (groups[sample] == "treatment") * (0.2 + feature / 20) +
      sin(feature * sample) * (0.02 + feature / 100)
  })
  rownames(matrix) <- paste0("F", feature_index)
  colnames(matrix) <- paste0("S", sample_index)
  matrix
}

b05_levels_study <- function(values = b05_values()) {
  sample_ids <- colnames(values)
  list(
    study_id = "synthetic_b_t05_levels",
    preparation_context_id = "b_t05_levels",
    sample_metadata = data.frame(
      sample_id = sample_ids,
      group = rep(c("control", "treatment"), each = 3L),
      stringsAsFactors = FALSE
    ),
    sample_registry = data.frame(
      sample_id = sample_ids,
      subject_id = rep(NA_character_, length(sample_ids)),
      visit_id = rep(NA_character_, length(sample_ids)),
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(group = c("control", "treatment")),
    feature_registry = data.frame(
      feature_key = rownames(values), block_id = "block_1", scale = "linear",
      quantity = "intensity", units = "synthetic_units", stringsAsFactors = FALSE
    ),
    assays = list(block_1 = values),
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
}

b05_levels_analysis <- function() {
  list(
    analysis_id = "b_t05_levels",
    block_ids = "block_1",
    preparation_context_id = "b_t05_levels",
    design = list(
      kind = "independent", condition = "group", condition_type = "factor",
      subject_id = NULL, repeated = FALSE, representation = "levels",
      reference_level = "control", target_level = "treatment", covariates = list(),
      missing_visits = "error"
    )
  )
}

b05_statistics <- function(count_metadata, fallback = "none") {
  list(
    method = "deqms",
    contrasts = list(list(
      contrast_id = "target_minus_reference",
      weights = list(condition.control = -1, condition.treatment = 1),
      interpretation = "Synthetic target minus reference."
    )),
    trend = FALSE,
    robust = FALSE,
    q_threshold = 0.05,
    effect_threshold = 0.5,
    effect_unit = "group_difference",
    family_scope = "one_scalar_contrast_all_retained_features",
    selection_validity = "qualified",
    selection_justification = "Synthetic fixture only.",
    dependence_justification = "Synthetic fixture only.",
    rank_sensitivity = FALSE,
    count_metadata = count_metadata,
    fallback = fallback
  )
}

b05_count_table <- function(values = b05_values(), count_type = "psm_count") {
  table <- data.frame(
    feature_key = rownames(values),
    acquisition = "synthetic_acquisition",
    provenance = "synthetic public count fixture",
    stringsAsFactors = FALSE
  )
  table[[count_type]] <- seq_len(nrow(values)) + 2L
  table[nrow(table):1L, , drop = FALSE]
}

b05_run_levels <- function(statistics, values = b05_values()) {
  resolved <- triomics_resolve_analysis(b05_levels_study(values), b05_levels_analysis())
  triomics_fit_block_models(resolved, statistics)
}

b05_delta_study <- function() {
  features <- paste0("F", seq_len(12L))
  sample_ids <- c("A_b", "A_f", "B_b", "B_f", "C_b", "C_f", "D_b")
  assay <- matrix(NA_real_, nrow = length(features), ncol = length(sample_ids),
                  dimnames = list(features, sample_ids))
  for (i in seq_len(4L)) {
    baseline <- paste0(LETTERS[[i]], "_b")
    followup <- paste0(LETTERS[[i]], "_f")
    feature_index <- seq_len(12L)
    assay[, baseline] <- feature_index / 10 + i + sin(feature_index * i) / 10
    if (followup %in% sample_ids) {
      assay[, followup] <- assay[, baseline] + 0.4 +
        sin(feature_index * (i + 1L)) * (0.05 + feature_index / 100)
    }
  }
  metadata <- data.frame(
    sample_id = sample_ids,
    subject = substr(sample_ids, 1L, 1L),
    visit = ifelse(grepl("_b$", sample_ids), "baseline", "followup"),
    stringsAsFactors = FALSE
  )
  list(
    study_id = "synthetic_b_t05_delta",
    preparation_context_id = "b_t05_delta",
    sample_metadata = metadata,
    sample_registry = data.frame(
      sample_id = sample_ids, subject_id = metadata$subject, visit_id = metadata$visit,
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(visit = c("baseline", "followup")),
    feature_registry = data.frame(
      feature_key = features, block_id = "block_1", scale = "linear",
      quantity = "intensity", units = "synthetic_units", stringsAsFactors = FALSE
    ),
    assays = list(block_1 = assay),
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
}

b05_delta_analysis <- function() {
  list(
    analysis_id = "b_t05_delta",
    block_ids = "block_1",
    preparation_context_id = "b_t05_delta",
    design = list(
      kind = "paired", condition = "visit", condition_type = "factor",
      subject_id = "subject", repeated = TRUE, representation = "delta",
      reference_level = "baseline", target_level = "followup", between_group = NULL,
      visit_column = "visit", reference_visit = "baseline", target_visit = "followup",
      covariates = list(), missing_visits = "complete_pairs_per_contrast",
      time_column = NULL, time_unit = NULL, time_origin = NULL,
      spline_knots = list(), spline_boundary = list(), residual_contract_ref = NULL,
      mean_weights = NULL, interactions = list()
    )
  )
}

b05_delta_statistics <- function(count_metadata) {
  stats <- b05_statistics(count_metadata)
  stats$effect_unit <- "delta_difference"
  stats$contrasts <- list(list(
    contrast_id = "followup_minus_baseline",
    weights = list(intercept = 1),
    interpretation = "Synthetic follow-up minus baseline delta."
  ))
  stats
}

b05_delta_counts <- function(study) {
  assay <- study$assays$block_1
  records <- expand.grid(
    feature_key = rownames(assay), sample_id = colnames(assay),
    stringsAsFactors = FALSE
  )
  feature_index <- match(records$feature_key, rownames(assay))
  sample_index <- match(records$sample_id, colnames(assay))
  records$psm_count <- feature_index + sample_index + 1L
  records$psm_count[records$sample_id == "D_b"] <- 1L
  records$acquisition <- "synthetic_paired_acquisition"
  records$provenance <- "synthetic public paired count fixture"
  records
}

b05_cli <- function(config, output) {
  cli <- triomics_test_resource("exec", "triomics")
  old_wd <- setwd(dirname(cli))
  on.exit(setwd(old_wd), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  args <- c(
    "--vanilla", shQuote(basename(cli)), "--config", shQuote(config),
    "--stage", "B", "--output", shQuote(output)
  )
  suppressWarnings(system2(rscript, args, stdout = TRUE, stderr = TRUE, timeout = 60))
}

b05_cli_config <- function(count_metadata, fallback = "none", root) {
  fixture <- triomics_test_resource("examples", "tickets", "B-T05.yaml")
  dir.create(file.path(root, "tickets"), recursive = TRUE, showWarnings = FALSE)
  data_root <- file.path(root, "data", "paired")
  dir.create(data_root, recursive = TRUE, showWarnings = FALSE)
  public_resources <- c(
    triomics_test_resource("examples", "data", "paired", "metadata.tsv"),
    triomics_test_resource("examples", "data", "paired", "proteomics.tsv")
  )
  copied <- file.copy(public_resources, data_root, overwrite = TRUE)
  stopifnot(all(copied))

  source <- paste(readLines(fixture, warn = FALSE), collapse = "\n")
  value <- if (is.null(count_metadata)) "null" else {
    as.character(jsonlite::toJSON(count_metadata, auto_unbox = TRUE))
  }
  source <- sub("      count_metadata: null", paste0("      count_metadata: ", value),
                source, fixed = TRUE)
  source <- sub("      fallback: none", paste0("      fallback: ", fallback),
                source, fixed = TRUE)
  path <- file.path(root, "tickets", basename(fixture))
  writeLines(source, path, useBytes = TRUE)
  path
}

b05_expect_native_provenance <- function(output, expected_revision) {
  handoff_root <- file.path(output, "B", "handoffs")
  handoff_dirs <- if (dir.exists(handoff_root)) {
    list.dirs(handoff_root, recursive = FALSE, full.names = TRUE)
  } else {
    character()
  }
  testthat::expect_length(handoff_dirs, 1L)
  if (length(handoff_dirs) != 1L) return(invisible(NULL))

  payload_path <- file.path(handoff_dirs[[1L]], "stage-handoff-provenance.json")
  envelope_path <- file.path(handoff_dirs[[1L]], "stage-handoff-provenance.envelope.json")
  testthat::expect_true(file.exists(payload_path),
                        info = "The B-T05 CLI emitted no native provenance payload.")
  testthat::expect_true(file.exists(envelope_path),
                        info = "The B-T05 CLI emitted no native provenance envelope.")
  if (!all(file.exists(c(payload_path, envelope_path)))) return(invisible(NULL))

  payload <- jsonlite::read_json(payload_path, simplifyVector = FALSE)
  envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)
  testthat::expect_true(isTRUE(triomics_validate_stage_handoff_provenance(
    payload, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
  )))
  testthat::expect_true(triomics_validate_envelope(envelope_path))
  testthat::expect_identical(envelope$payload_sha256, triomics_sha256(payload_path))
  testthat::expect_match(expected_revision, "^(?:[0-9a-f]{40}|[0-9a-f]{64})$")
  testthat::expect_identical(payload$producer$repository_commit, expected_revision)
  testthat::expect_identical(envelope$producer$commit, expected_revision)
  invisible(list(payload = payload, envelope = envelope))
}

testthat::test_that("B-T05 aligns count rows by feature identity and matches DEqMS 1.30.0 outputs", {
  testthat::expect_true(requireNamespace("DEqMS", quietly = TRUE))
  testthat::expect_identical(as.character(utils::packageVersion("DEqMS")), "1.30.0")
  values <- b05_values()
  counts <- b05_count_table(values)
  fit <- b05_run_levels(b05_statistics(counts), values)

  testthat::expect_identical(fit$status, "complete")
  testthat::expect_identical(fit$method, "DEqMS")
  testthat::expect_identical(fit$deqms_version, "1.30.0")
  rows <- fit$blocks$block_1$contrasts$target_minus_reference$rows
  testthat::expect_identical(vapply(rows, `[[`, character(1), "feature_key"), rownames(values))
  testthat::expect_identical(rows[[1L]]$count_prior_ref$count, 3)
  testthat::expect_identical(rows[[1L]]$count_prior_ref$count_type, "psm_count")
  testthat::expect_identical(rows[[1L]]$count_prior_ref$acquisition, "synthetic_acquisition")
  testthat::expect_identical(rows[[1L]]$count_prior_ref$provenance, "synthetic public count fixture")
  testthat::expect_identical(rows[[1L]]$statistic_name, "DEqMS_sca.t")
  testthat::expect_identical(rows[[1L]]$method_p_field, "sca.P.Value")
  testthat::expect_identical(rows[[1L]]$effect_unit, "group_difference")

  groups <- rep(c("control", "treatment"), each = 3L)
  design <- matrix(0, nrow = length(groups), ncol = 2L,
                   dimnames = list(NULL, c("condition.control", "condition.treatment")))
  design[groups == "control", 1L] <- 1
  design[groups == "treatment", 2L] <- 1
  contrast <- matrix(c(-1, 1), ncol = 1L,
                     dimnames = list(colnames(design), "target_minus_reference"))
  reference <- limma::eBayes(
    limma::contrasts.fit(limma::lmFit(values, design), contrast), trend = FALSE
  )
  reference$count <- stats::setNames(counts$psm_count, counts$feature_key)[rownames(values)]
  reference <- DEqMS::spectraCounteBayes(reference)
  reference_rows <- DEqMS::outputResult(reference, coef_col = 1L)
  first <- match("F1", reference_rows$gene)
  testthat::expect_equal(rows[[1L]]$deqms_sca_t, reference_rows$sca.t[[first]], tolerance = 1e-10)
  testthat::expect_equal(rows[[1L]]$p, reference_rows$sca.P.Value[[first]], tolerance = 1e-10)
  testthat::expect_equal(
    rows[[1L]]$moderated_se,
    sqrt(reference$sca.postvar[[1L]]) * reference$stdev.unscaled["F1", 1L],
    tolerance = 1e-10
  )
})

testthat::test_that("invalid, missing, or positionally misaligned counts keep DEqMS unavailable", {
  values <- b05_values()
  bad_key <- b05_count_table(values)
  bad_key$feature_key[[1L]] <- "not_a_fitted_feature"
  mismatch <- b05_run_levels(b05_statistics(bad_key), values)
  testthat::expect_identical(mismatch$status, "unavailable")
  testthat::expect_identical(mismatch$unavailable_reason, "count_feature_identity_mismatch")
  testthat::expect_length(mismatch$blocks, 0L)

  zero <- b05_count_table(values)
  zero$psm_count[[1L]] <- 0L
  invalid <- b05_run_levels(b05_statistics(zero), values)
  testthat::expect_identical(invalid$status, "unavailable")
  testthat::expect_identical(invalid$unavailable_reason, "count_values_not_positive_whole_numbers")

  extra_block <- b05_count_table(values)
  extra_block$block_id <- "block_1"
  unrelated <- extra_block[1L, , drop = FALSE]
  unrelated$block_id <- "not_in_analysis"
  extra_block <- rbind(extra_block, unrelated)
  extra <- b05_run_levels(b05_statistics(extra_block), values)
  testthat::expect_identical(extra$status, "unavailable")
  testthat::expect_identical(extra$unavailable_reason, "count_block_identity_mismatch")

  missing <- b05_run_levels(b05_statistics(NULL), values)
  testthat::expect_identical(missing$status, "unavailable")
  testthat::expect_identical(missing$unavailable_reason, "count_metadata_missing")
  testthat::expect_false(isTRUE(missing$fallback_applied))
})

testthat::test_that("B-T05 rejects effect units incompatible with the fitted representation", {
  values <- b05_values()
  linear_levels <- b05_statistics(b05_count_table(values))
  linear_levels$effect_unit <- "log2_fold_change"
  levels_fit <- b05_run_levels(linear_levels, values)
  testthat::expect_identical(levels_fit$status, "unavailable")
  testthat::expect_identical(levels_fit$unavailable_reason, "effect_unit_mismatch")

  study <- b05_delta_study()
  delta_stats <- b05_delta_statistics(b05_delta_counts(study))
  delta_stats$effect_unit <- "group_difference"
  resolved <- triomics_resolve_analysis(study, b05_delta_analysis())
  delta_fit <- triomics_fit_block_models(resolved, delta_stats)
  testthat::expect_identical(delta_fit$status, "unavailable")
  testthat::expect_identical(delta_fit$unavailable_reason, "effect_unit_mismatch")
})

testthat::test_that("B-T05 leaves DEqMS requests in other lanes to their existing dispatcher", {
  original <- function(resolved_analysis, statistics_spec) {
    list(method = statistics_spec$method, fallback = statistics_spec$fallback)
  }
  dispatch <- triomics_b05_dispatch_fit(original, .GlobalEnv)
  fit <- dispatch(list(lane = "b_t02"), b05_statistics(NULL, fallback = "explicit_limma"))

  testthat::expect_identical(fit$method, "deqms")
  testthat::expect_identical(fit$fallback, "explicit_limma")

  non_deqms <- dispatch(list(lane = "b_t03"), list(method = "limma", fallback = "none"))
  testthat::expect_identical(non_deqms$method, "limma")
  testthat::expect_identical(non_deqms$fallback, "none")
})

testthat::test_that("B-T01 level DEqMS reports missing counts with or without explicit limma fallback", {
  values <- b05_values()
  missing_path <- tempfile("b-t05-nonexistent-counts-")
  testthat::expect_false(file.exists(missing_path))
  cases <- list(
    list(id = "null without fallback", count_metadata = NULL, fallback = "none",
         status = "unavailable", method = "DEqMS", fallback_applied = FALSE),
    list(id = "missing path without fallback", count_metadata = missing_path, fallback = "none",
         status = "unavailable", method = "DEqMS", fallback_applied = FALSE),
    list(id = "null with explicit fallback", count_metadata = NULL, fallback = "explicit_limma",
         status = "fallback_complete", method = "limma_explicit_fallback", fallback_applied = TRUE),
    list(id = "missing path with explicit fallback", count_metadata = missing_path, fallback = "explicit_limma",
         status = "fallback_complete", method = "limma_explicit_fallback", fallback_applied = TRUE)
  )

  for (case in cases) {
    fit <- b05_run_levels(b05_statistics(case$count_metadata, fallback = case$fallback), values)
    testthat::expect_identical(fit$status, case$status, info = case$id)
    testthat::expect_identical(fit$requested_method, "DEqMS", info = case$id)
    testthat::expect_identical(fit$method, case$method, info = case$id)
    testthat::expect_identical(fit$fallback_applied, case$fallback_applied, info = case$id)
    testthat::expect_identical(fit$deqms_status$status, "unavailable", info = case$id)
    testthat::expect_identical(fit$deqms_status$reason, "count_metadata_missing", info = case$id)
    if (identical(case$fallback, "none")) {
      testthat::expect_identical(fit$unavailable_reason, "count_metadata_missing", info = case$id)
      testthat::expect_null(fit$fallback_provenance, info = case$id)
    } else {
      testthat::expect_identical(fit$fallback_provenance$source_method, "limma", info = case$id)
      testthat::expect_identical(fit$fallback_provenance$status, "applied", info = case$id)
      testthat::expect_identical(fit$fallback_provenance$trigger_reason,
                                 "count_metadata_missing", info = case$id)
      testthat::expect_true(isTRUE(fit$fallback_provenance$user_declared), info = case$id)
    }
  }
})

testthat::test_that("a declared limma fallback is separately identified and never labeled DEqMS", {
  values <- b05_values()
  bad_key <- b05_count_table(values)
  bad_key$feature_key[[1L]] <- "not_a_fitted_feature"
  fallback <- b05_run_levels(b05_statistics(bad_key, fallback = "explicit_limma"), values)

  testthat::expect_identical(fallback$status, "fallback_complete")
  testthat::expect_true(isTRUE(fallback$fallback_applied))
  testthat::expect_identical(fallback$requested_method, "DEqMS")
  testthat::expect_identical(fallback$method, "limma_explicit_fallback")
  testthat::expect_identical(fallback$fallback_provenance$source_method, "limma")
  testthat::expect_identical(fallback$deqms_status$status, "unavailable")
  row <- fallback$blocks$block_1$contrasts$target_minus_reference$rows[[1L]]
  testthat::expect_identical(row$method_id, "limma_explicit_fallback")
  testthat::expect_null(row$count_prior_ref)
  testthat::expect_null(row$deqms_sca_p)
})

testthat::test_that("delta count summaries use only sample counts from retained complete pairs", {
  study <- b05_delta_study()
  study$assays$block_1["F1", "A_f"] <- NA_real_
  counts <- b05_delta_counts(study)
  resolved <- triomics_resolve_analysis(study, b05_delta_analysis())
  fit <- triomics_fit_block_models(resolved, b05_delta_statistics(counts))

  testthat::expect_identical(fit$status, "complete")
  summary <- fit$blocks$block_1$count_summary
  testthat::expect_identical(summary$method,
                             "minimum_over_feature_specific_complete_pair_observations")
  testthat::expect_null(summary$sample_ids)
  testthat::expect_identical(summary$sample_scope, "per_feature_model_support")
  row <- fit$blocks$block_1$contrasts$followup_minus_baseline$rows[[1L]]
  testthat::expect_identical(row$effect_unit, "delta_difference")
  testthat::expect_identical(row$count_prior_ref$count, 5)
  testthat::expect_identical(row$count_prior_ref$sample_ids, c("B_b", "B_f", "C_b", "C_f"))
  row_f2 <- fit$blocks$block_1$contrasts$followup_minus_baseline$rows[[2L]]
  testthat::expect_identical(row_f2$count_prior_ref$count, 4)
  testthat::expect_identical(row_f2$count_prior_ref$sample_ids,
                             c("A_b", "A_f", "B_b", "B_f", "C_b", "C_f"))
  testthat::expect_true(
    length(summary$justification) == 1L &&
      grepl("complete pairs", summary$justification, ignore.case = TRUE)
  )
})

testthat::test_that("delta DEqMS refuses a feature-only count with no paired sample provenance", {
  study <- b05_delta_study()
  feature_counts <- b05_count_table(study$assays$block_1)
  resolved <- triomics_resolve_analysis(study, b05_delta_analysis())
  fit <- triomics_fit_block_models(resolved, b05_delta_statistics(feature_counts))

  testthat::expect_identical(fit$status, "unavailable")
  testthat::expect_identical(fit$unavailable_reason, "delta_sample_count_identity_required")
  testthat::expect_length(fit$blocks, 0L)
})

testthat::test_that("CLI startup routes qualified absolute-path DEqMS and explicit fallback", {
  fixture <- triomics_test_resource("examples", "tickets", "B-T05.yaml")
  config <- triomics_read_config(fixture)
  ingested <- triomics_ingest_configured_study(config, fixture)
  feature_keys <- as.character(ingested$feature_registry$feature_key)
  sample_ids <- as.character(ingested$sample_metadata$sample_id)
  count_table <- expand.grid(feature_key = feature_keys, sample_id = sample_ids,
                             stringsAsFactors = FALSE)
  count_table$psm_count <- match(count_table$feature_key, feature_keys) +
    match(count_table$sample_id, sample_ids) + 1L
  count_table$acquisition <- "synthetic_cli_acquisition"
  count_table$provenance <- "synthetic CLI test count fixture"
  count_path <- tempfile("b-t05-counts-", tmpdir = "/private/tmp")
  qualified_fixture_root <- tempfile("b-t05-qualified-fixture-", tmpdir = "/private/tmp")
  fallback_fixture_root <- tempfile("b-t05-fallback-fixture-", tmpdir = "/private/tmp")
  outputs <- c(tempfile("b-t05-qualified-out-", tmpdir = "/private/tmp"),
               tempfile("b-t05-fallback-out-", tmpdir = "/private/tmp"))
  on.exit(unlink(c(count_path, qualified_fixture_root, fallback_fixture_root, outputs),
                 recursive = TRUE), add = TRUE)

  utils::write.table(count_table, count_path, sep = "\t", row.names = FALSE, quote = TRUE)
  qualified_config <- b05_cli_config(count_path, root = qualified_fixture_root)
  fallback_config <- b05_cli_config(NULL, fallback = "explicit_limma", root = fallback_fixture_root)
  expected_revision <- triomics_source_revision()

  qualified_log <- b05_cli(qualified_config, outputs[[1L]])
  qualified_status <- attr(qualified_log, "status")
  testthat::expect_identical(if (is.null(qualified_status)) 0L else as.integer(qualified_status), 0L,
                             info = paste(qualified_log, collapse = "\n"))
  qualified <- jsonlite::read_json(file.path(outputs[[1L]], "B", "block-results.json"),
                                   simplifyVector = FALSE)
  qualified_envelope_path <- file.path(outputs[[1L]], "B", "block-results.envelope.json")
  qualified_envelope <- jsonlite::read_json(qualified_envelope_path, simplifyVector = FALSE)
  testthat::expect_identical(qualified$status, "complete")
  testthat::expect_true(triomics_validate_envelope(qualified_envelope_path))
  testthat::expect_identical(qualified_envelope$status, "complete")
  b05_expect_native_provenance(outputs[[1L]], expected_revision)

  fallback_log <- b05_cli(fallback_config, outputs[[2L]])
  fallback_status <- attr(fallback_log, "status")
  testthat::expect_identical(if (is.null(fallback_status)) 0L else as.integer(fallback_status), 0L,
                             info = paste(fallback_log, collapse = "\n"))
  fallback <- jsonlite::read_json(file.path(outputs[[2L]], "B", "block-results.json"),
                                  simplifyVector = FALSE)
  fallback_envelope_path <- file.path(outputs[[2L]], "B", "block-results.envelope.json")
  fallback_envelope <- jsonlite::read_json(fallback_envelope_path, simplifyVector = FALSE)
  fallback_run <- jsonlite::read_json(file.path(outputs[[2L]], "registries", "run.json"),
                                      simplifyVector = FALSE)
  testthat::expect_true(triomics_validate_envelope(fallback_envelope_path))
  testthat::expect_identical(fallback$status, "complete")
  testthat::expect_identical(fallback$method_status, "fallback_complete")
  testthat::expect_identical(fallback_envelope$status, "complete")
  testthat::expect_identical(fallback_run$status, "complete")
  testthat::expect_identical(fallback$requested_method, "DEqMS")
  testthat::expect_identical(fallback$method, "limma_explicit_fallback")
  testthat::expect_true(isTRUE(fallback$fallback_applied))
  testthat::expect_identical(fallback$deqms_status$status, "unavailable")
  testthat::expect_identical(fallback$deqms_status$reason, "count_metadata_missing")
  testthat::expect_identical(fallback_envelope$reason_codes, "count_metadata_missing")
  testthat::expect_identical(fallback$fallback_provenance$source_method, "limma")
  testthat::expect_identical(fallback$fallback_provenance$status, "applied")
  testthat::expect_identical(fallback$fallback_provenance$trigger_reason,
                             "count_metadata_missing")
  testthat::expect_true(isTRUE(fallback$fallback_provenance$user_declared))
  b05_expect_native_provenance(outputs[[2L]], expected_revision)
})
