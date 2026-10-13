expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

write_tsv <- function(path, header, rows) {
  lines <- c(
    paste(header, collapse = "\t"),
    vapply(rows, function(row) paste(row, collapse = "\t"), character(1))
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

read_tsv_lines <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  rows <- strsplit(lines, "\t", fixed = TRUE)
  list(header = rows[[1L]], rows = rows[-1L])
}

sample_registry <- function(ids) {
  data.frame(
    sample_id = ids,
    subject_id = rep(NA_character_, length(ids)),
    visit_id = rep(NA_character_, length(ids)),
    sample_role = rep("undeclared", length(ids)),
    stringsAsFactors = FALSE
  )
}

source_for <- function(path, study_id = "study", source_id = "matrix",
                       selection = "full_unselected", conversion = NULL, sha = NULL) {
  triomics_source_record(
    source_id = source_id,
    study_id = study_id,
    path = path,
    format = "tsv",
    role = "measurement_matrix",
    selection_history = selection,
    selection_evidence = "Synthetic fixture; no participant data.",
    sha256 = sha,
    conversion_manifest = conversion
  )
}

block_for <- function(source_id = "matrix", block_id = "block_a", mask = "known",
                      upstream = character(), sample_map = NULL, cell_mask = NULL) {
  list(
    block_id = block_id,
    source_id = source_id,
    feature_id_column = "feature_id",
    id_namespace = "synthetic",
    missing_tokens = c("NA", ""),
    sample_map = sample_map,
    cell_mask = cell_mask,
    annotation_route = "none",
    measurement = list(
      quantity = "intensity",
      scale = "linear",
      units = "synthetic_arbitrary_units",
      original_mask_status = mask,
      upstream_steps = upstream
    )
  )
}

ingest_one <- function(path, ids, study_id = "study", selection = "full_unselected",
                       mask = "known", upstream = character(), conversion = NULL,
                       sha = NULL, sample_map = NULL, cell_mask = NULL,
                       block_id = "block_a") {
  triomics_ingest_study_tables(
    study_id = study_id,
    sample_registry = sample_registry(ids),
    blocks = list(block_for(block_id = block_id, mask = mask, upstream = upstream,
                            sample_map = sample_map, cell_mask = cell_mask)),
    sources = list(source_for(path, study_id = study_id, selection = selection,
                              conversion = conversion, sha = sha))
  )
}

write_roles <- function(path, roles, canonical, mode = "identity", original = NULL,
                        sheet = NULL, region = NULL, formula = FALSE, cache = NULL,
                        tolerance = NULL) {
  payload <- list(
    schema_version = "2.0",
    mode = mode,
    original_ref = if (identical(mode, "identity")) NULL else "synthetic-workbook.xlsx",
    original_sha256 = if (identical(mode, "identity")) NULL else original,
    canonical_sha256 = canonical,
    converter = if (identical(mode, "identity")) NULL else "fixture-converter",
    converter_version = if (identical(mode, "identity")) NULL else "0",
    options = if (identical(mode, "identity")) NULL else list(extract = "cached-values"),
    sheet = sheet,
    region = region,
    header_role = "single_header",
    column_roles = as.list(roles),
    formula_derived = formula,
    cache_status = cache,
    tolerance = tolerance
  )
  jsonlite::write_json(payload, path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  invisible(path)
}

write_study_yaml <- function(dir, sample_rows, matrix_rows, selection = "full_unselected",
                             factor_levels = c("control", "treatment"),
                             subject_field = "null", extra_metadata = NULL,
                             block_ids = "block_1") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  meta_header <- c("sample_id", "group")
  if (!is.null(extra_metadata)) meta_header <- c(meta_header, extra_metadata$name)
  meta_rows <- lapply(seq_along(sample_rows$sample_id), function(i) {
    row <- c(sample_rows$sample_id[[i]], sample_rows$group[[i]])
    if (!is.null(extra_metadata)) row <- c(row, extra_metadata$values[[i]])
    row
  })
  write_tsv(file.path(dir, "metadata.tsv"), meta_header, meta_rows)
  write_tsv(file.path(dir, "block.tsv"), matrix_rows$header, matrix_rows$rows)
  levels <- paste(sprintf("      - %s", factor_levels), collapse = "\n")
  extra_type <- ""
  yaml <- sprintf(paste(c(
    "schema_version: '2.0'",
    "study_id: pattern_study",
    "data_class: synthetic",
    "sources:",
    "  - source_id: sample_metadata",
    "    file: metadata.tsv",
    "    format: tsv",
    "    role: sample_metadata",
    "    selection_history: %s",
    "    selection_evidence: Synthetic fixture; no participant data.",
    "  - source_id: block_1",
    "    file: block.tsv",
    "    format: tsv",
    "    role: measurement_matrix",
    "    selection_history: %s",
    "    selection_evidence: Synthetic fixture; no outcome-based selection.",
    "metadata:",
    "  file: metadata.tsv",
    "  sample_id: sample_id",
    "  subject_id: %s",
    "  column_types:",
    "    sample_id: string",
    "    group: factor%s",
    "  factor_levels:",
    "    group:",
    "%s",
    "blocks:",
    "  - block_id: block_1",
    "    name: Synthetic block",
    "    profile: proteomics",
    "    profile_spec_ref: null",
    "    file: block.tsv",
    "    feature_id_column: feature_id",
    "    id_namespace: synthetic",
    "    feature_metadata: null",
    "    source_refs:",
    "      - block_1",
    "    sample_map: null",
    "    missing_tokens:",
    "      - NA",
    "      - ''",
    "    measurement:",
    "      quantity: intensity",
    "      scale: linear",
    "      units: synthetic_arbitrary_units",
    "      acquisition: Explicit synthetic software fixture.",
    "      missingness_assumption: unknown",
    "      missingness_evidence: No mechanism is claimed.",
    "      upstream_steps: []",
    "      original_mask_status: known",
    "      denominator_ref: null",
    "    preprocessing:",
    "      context_id: a_t02_pattern",
    "      filter:",
    "        prevalence: 0.7",
    "        min_observed: 1",
    "        scope: global",
    "        stratum: null",
    "        selection_justification: Synthetic fixture choice only.",
    "      transform:",
    "        method: none",
    "        zero_policy: error",
    "        zero_value: null",
    "        reference_set: null",
    "        log_base: null",
    "      normalisation:",
    "        method: none",
    "        reference: none",
    "        reference_file: null",
    "        metadata_column: null",
    "        standards_map: null",
    "        qualification_ref: null",
    "      imputation:",
    "        method: none",
    "        tail_probability: null",
    "        scale_multiplier: null",
    "        k: null",
    "        trees: null",
    "        qualification_ref: null",
    "      derive: []",
    "    annotation_route: none",
    "analyses:",
    "  - analysis_id: synthetic_primary",
    "    block_ids:",
    "      - %s",
    "    preparation_context_id: a_t02_pattern",
    "    design:",
    "      kind: independent",
    "      condition: group",
    "      condition_type: factor",
    "      subject_id: null",
    "      repeated: false",
    "      representation: levels",
    "      reference_level: control",
    "      target_level: treatment",
    "      between_group: null",
    "      visit_column: null",
    "      reference_visit: null",
    "      target_visit: null",
    "      covariates: []",
    "      missing_visits: error",
    "      time_column: null",
    "      time_unit: null",
    "      time_origin: null",
    "      spline_knots: []",
    "      spline_boundary: []",
    "      residual_contract_ref: null",
    "    statistics:",
    "      method: limma",
    "      contrasts:",
    "        - contrast_id: target_minus_reference",
    "          weights:",
    "            condition.control: -1",
    "            condition.treatment: 1",
    "          interpretation: Synthetic software fixture only.",
    "      trend: false",
    "      robust: false",
    "      q_threshold: 0.05",
    "      effect_threshold: 0.5",
    "      effect_unit: log2_fold_change",
    "      family_scope: one_scalar_contrast_all_retained_features",
    "      selection_validity: unverified",
    "      selection_justification: Synthetic fixture; inference is not validated.",
    "      dependence_justification: Synthetic fixture; no scientific claim is made.",
    "      rank_sensitivity: false",
    "      count_metadata: null",
    "      fallback: none",
    "    integration: []",
    "    completeness: strict",
    "clinical:",
    "  enabled: false",
    "  dictionary_file: null",
    "  observations_file: null",
    "  links_file: null",
    "  objectives: []",
    "annotation:",
    "  resources: []",
    "  requests: []",
    "target_investigation:",
    "  enabled: false",
    "  mode: assess",
    "  targets: []",
    "  drug_ref: null",
    "  context_ref: null",
    "  universe_ref: null",
    "  external_evidence_file: null",
    "  mechanism_file: null",
    "  protocol_ref: null",
    "  quantitative_method_ref: null",
    "  required: false",
    "execution:",
    "  seed: 20260920",
    "  network_policy: offline",
    "  workers: 1",
    "  cpu_limit: 1",
    "  memory_gib: 2",
    "  max_fits: 10",
    "  audience: synthetic_demo",
    "  disclosure_policy_ref: null",
    ""
  ), collapse = "\n"), selection, selection, subject_field, extra_type, levels, block_ids[[1L]])
  path <- file.path(dir, "study.yaml")
  writeLines(yaml, path, useBytes = TRUE)
  path
}

testthat::test_that("A-T02-AC1 protected simulator signature labels truth and seed", {
  formals_out <- formals(simulate_study)
  testthat::expect_identical(
    names(formals_out),
    c("n_subjects", "n_features", "design", "effect_size", "prop_affected",
      "missingness", "shared_factor_strength")
  )
  testthat::expect_identical(eval(formals_out$n_features), c(prot = 800, lip = 400, met = 600))
  testthat::expect_identical(
    eval(formals_out$design),
    c("paired", "two_group", "continuous", "timeseries")
  )
  testthat::expect_false("seed" %in% names(formals_out))

  options(triomics.execution_context = NULL)
  expect_code(
    simulate_study(4, n_features = c(a = 1, b = 1), effect_size = 0, prop_affected = 0,
                   missingness = 0, shared_factor_strength = 0),
    "A_SIMULATOR_SEED"
  )
  triomics_use_execution_context(7, "sim")
  expect_code(
    simulate_study(4, n_features = c(a = 1, b = 1), design = "independent",
                   effect_size = 0, prop_affected = 0, missingness = 0,
                   shared_factor_strength = 0),
    "A_SIMULATOR_DESIGN"
  )
  expect_code(triomics_use_execution_context(1.5, "sim"), "A_SIMULATOR_SEED")

  labels <- c(two_group = "independent", paired = "paired", continuous = "continuous",
              timeseries = "time-series")
  previous_kind <- RNGkind()
  for (n_blocks in c(2L, 5L)) {
    block_names <- paste0("block", seq_len(n_blocks))
    features <- stats::setNames(rep(2, n_blocks), block_names)
    for (design in names(labels)) {
      seed <- 1000L + n_blocks * 10L + match(design, names(labels))
      triomics_use_execution_context(seed, paste0("sim-", n_blocks, "-", design))
      out <- simulate_study(
        4, n_features = features, design = design, effect_size = 1.25,
        prop_affected = 1, missingness = 0, shared_factor_strength = 0.3
      )
      testthat::expect_identical(out$truth$seed, seed)
      testthat::expect_identical(out$truth$design, design)
      testthat::expect_identical(out$truth$design_label, unname(labels[[design]]))
      testthat::expect_identical(out$truth$formula_version, "a-t02-simulator-1")
      testthat::expect_identical(names(out$study$blocks), block_names)
      testthat::expect_false("prot" %in% names(out$study$blocks))
      testthat::expect_false("seed" %in% names(out$study))
      testthat::expect_false("loadings" %in% names(out$study$blocks[[1L]]))
      meta <- out$study$metadata
      block <- out$truth$blocks[[1L]]
      recon <- 0.3 * outer(block$loadings, out$truth$factor_scores) + block$effect + block$noise
      if (!is.null(out$truth$subject_intercepts)) {
        recon <- sweep(recon, 2L, out$truth$intercept_by_sample, "+")
      }
      testthat::expect_equal(unname(block$complete), recon, tolerance = 1e-8)
      testthat::expect_equal(unname(out$study$blocks[[1L]]$assay), unname(block$complete),
                             tolerance = 1e-8)
      effect <- block$effect[1L, ]
      if (design == "two_group") {
        testthat::expect_true(all(is.na(meta$subject_id)))
        testthat::expect_equal(unname(effect[meta$group == "treatment"]),
                               rep(1.25, sum(meta$group == "treatment")), tolerance = 1e-8)
        testthat::expect_equal(unname(effect[meta$group == "control"]),
                               rep(0, sum(meta$group == "control")), tolerance = 1e-8)
        testthat::expect_equal(sum(meta$group == "control"), 2)
      } else if (design == "paired") {
        testthat::expect_false(anyNA(meta$subject_id))
        testthat::expect_equal(unname(effect[meta$visit == "b"]), rep(1.25, 4), tolerance = 1e-8)
        testthat::expect_equal(unname(effect[meta$visit == "a"]), rep(0, 4), tolerance = 1e-8)
      } else if (design == "continuous") {
        testthat::expect_true(all(is.na(meta$subject_id)))
        testthat::expect_equal(unname(effect), 1.25 * meta$predictor, tolerance = 1e-8)
      } else {
        testthat::expect_false(anyNA(meta$subject_id))
        testthat::expect_identical(out$truth$schedule$times, c(0, 1, 2))
        testthat::expect_equal(unname(effect), 1.25 * meta$time, tolerance = 1e-8)
      }
    }
  }
  testthat::expect_identical(RNGkind(), previous_kind)

  triomics_use_execution_context(42L, "replay")
  features <- c(alpha = 4, beta = 2)
  left <- simulate_study(4, n_features = features, design = "two_group", effect_size = 1.5,
                         prop_affected = 0.5, missingness = 0, shared_factor_strength = 0)
  triomics_use_execution_context(42L, "replay")
  right <- simulate_study(4, n_features = features, design = "two_group", effect_size = 1.5,
                          prop_affected = 0.5, missingness = 0, shared_factor_strength = 0)
  testthat::expect_identical(left$study$blocks$alpha$assay, right$study$blocks$alpha$assay)
  testthat::expect_identical(left$truth$blocks$alpha$affected_feature_ids,
                             left$study$blocks$alpha$feature_ids[1:2])
  testthat::expect_equal(unname(left$truth$blocks$alpha$effect[3L, ]), rep(0, 4), tolerance = 1e-8)
  testthat::expect_equal(
    unname(left$truth$blocks$alpha$effect[1L, left$study$metadata$group == "treatment"]),
    rep(1.5, 2), tolerance = 1e-8
  )
  triomics_use_execution_context(43L, "other-seed")
  other <- simulate_study(4, n_features = features, design = "two_group", effect_size = 1.5,
                          prop_affected = 0.5, missingness = 0, shared_factor_strength = 0)
  testthat::expect_false(identical(left$study$blocks$alpha$assay, other$study$blocks$alpha$assay))

  triomics_use_execution_context(44L, "missingness")
  hidden <- simulate_study(3, n_features = c(only = 2), design = "continuous", effect_size = 2,
                           prop_affected = 1, missingness = 1, shared_factor_strength = 0)
  testthat::expect_true(all(is.na(hidden$study$blocks$only$assay)))
  testthat::expect_true(all(is.finite(hidden$truth$blocks$only$complete)))
  testthat::expect_true(all(hidden$truth$blocks$only$missing))
  testthat::expect_false(identical(hidden$study$blocks$only$assay, hidden$truth$blocks$only$complete))

  triomics_use_execution_context(45L, "default-design")
  paired_default <- simulate_study(2, n_features = c(a = 1, b = 1), effect_size = 0,
                                   prop_affected = 0, missingness = 0, shared_factor_strength = 0)
  testthat::expect_identical(paired_default$truth$design, "paired")
  testthat::expect_identical(nrow(paired_default$study$metadata), 4L)
})

testthat::test_that("A-T02-AC2 reordered columns, leading zeros, and duplicate labels keep source-row keys", {
  testthat::expect_false(identical(
    triomics_feature_key("s|b", "1", "1"),
    triomics_feature_key("s", "b|1", "1")
  ))
  expect_code(triomics_preserve_identifier(1), "A_ID_COERCED")
  testthat::expect_identical(triomics_preserve_identifier("001"), "001")

  ids <- c("s01a", "s01b", "s02a", "s02b")
  scrambled <- tempfile(fileext = ".tsv")
  canonical <- tempfile(fileext = ".tsv")
  write_tsv(scrambled, c("feature_id", "s02b", "s01a", "s02a", "s01b"),
            list(c("P", "32", "8", "16", "16")))
  write_tsv(canonical, c("feature_id", ids), list(c("P", "8", "16", "16", "32")))
  left <- ingest_one(scrambled, ids)
  right <- ingest_one(canonical, ids)
  testthat::expect_identical(left$assays$block_a, right$assays$block_a)
  testthat::expect_equal(as.numeric(left$assays$block_a[1L, ]), c(8, 16, 16, 32), tolerance = 1e-8)
  testthat::expect_identical(colnames(left$assays$block_a), ids)

  zero_ids <- c("001", "010", "10")
  zero_path <- tempfile(fileext = ".tsv")
  write_tsv(zero_path, c("feature_id", "10", "001", "010"),
            list(c("001", "7", "5", "6"), c("1", "1", "2", "3")))
  zeros <- ingest_one(zero_path, zero_ids)
  testthat::expect_identical(colnames(zeros$assays$block_a), zero_ids)
  testthat::expect_equal(as.numeric(zeros$assays$block_a[1L, ]), c(5, 6, 7), tolerance = 1e-8)
  testthat::expect_identical(zeros$feature_registry$original_id, c("001", "1"))
  testthat::expect_identical(zeros$feature_registry$original_id[[1L]], "001")

  dup_path <- tempfile(fileext = ".tsv")
  write_tsv(dup_path, c("feature_id", "alias", "001"),
            list(c("PC(32:0)", "alias-a", "10"), c("PC(32:0)", "alias-a", "30")))
  roles <- tempfile(fileext = ".json")
  write_roles(roles, c(feature_id = "feature_id", alias = "alias", `001` = "measurement"),
              canonical = triomics_sha256(dup_path))
  dup <- ingest_one(dup_path, "001", conversion = roles)
  testthat::expect_identical(dup$feature_registry$original_id, c("PC(32:0)", "PC(32:0)"))
  testthat::expect_identical(
    dup$feature_registry$feature_key,
    c(triomics_feature_key("study", "block_a", "1"), triomics_feature_key("study", "block_a", "2"))
  )
  testthat::expect_equal(sort(as.numeric(dup$assays$block_a[, "001"])), c(10, 30), tolerance = 1e-8)
  testthat::expect_identical(dup$feature_registry$structural_resolution, c("unknown", "unknown"))
  testthat::expect_identical(nrow(dup$assays$block_a), 2L)
  testthat::expect_false(any(grepl("PC(32:0)", dup$feature_registry$feature_key, fixed = TRUE)))
  testthat::expect_false("alias" %in% colnames(dup$assays$block_a))

  duplicate_cols <- tempfile(fileext = ".tsv")
  write_tsv(duplicate_cols, c("feature_id", "001", "001"), list(c("A", "1", "2")))
  expect_code(ingest_one(duplicate_cols, "001"), "A_ID_DUPLICATE_SAMPLE")

  raw <- read_tsv_lines(triomics_test_resource("examples", "data", "independent", "proteomics.tsv"))
  reversed <- tempfile(fileext = ".tsv")
  order_rev <- c(1L, rev(seq_along(raw$header)[-1L]))
  write_tsv(reversed, raw$header[order_rev], lapply(raw$rows, function(row) row[order_rev]))
  meta <- read_tsv_lines(triomics_test_resource("examples", "data", "independent", "metadata.tsv"))
  sample_ids <- vapply(meta$rows, `[[`, character(1), 1L)
  original <- ingest_one(triomics_test_resource("examples", "data", "independent", "proteomics.tsv"),
                         sample_ids, block_id = "block_1")
  permutated <- ingest_one(reversed, sample_ids, block_id = "block_1")
  testthat::expect_identical(original$assays$block_1, permutated$assays$block_1)
  expected <- as.numeric(raw$rows[[1L]][[match("SYN_12", raw$header)]])
  key <- triomics_feature_key("study", "block_1", "1")
  testthat::expect_equal(original$assays$block_1[key, "SYN_12"], expected, tolerance = 1e-8)
  testthat::expect_identical(original$feature_registry$original_id[[1L]], raw$rows[[1L]][[1L]])
})

testthat::test_that("A-T02-AC3 summary columns and stale or conflicting conversions are rejected", {
  bare <- tempfile(fileext = ".tsv")
  write_tsv(bare, c("feature_id", "001", "mean", "pvalue", "fc"),
            list(c("A", "5", "999", "0.01", "2")))
  expect_code(ingest_one(bare, "001"), "A_SOURCE_ROLE")

  smuggled <- tempfile(fileext = ".tsv")
  write_tsv(smuggled, c("feature_id", "001", "mean"), list(c("A", "5", "999")))
  expect_code(ingest_one(smuggled, c("001", "mean")), "A_SOURCE_SUMMARY_AS_MEASUREMENT")

  declared <- tempfile(fileext = ".tsv")
  write_tsv(declared, c("feature_id", "001", "mean", "pvalue", "fc"),
            list(c("A", "5", "999", "0.01", "2")))
  role_path <- tempfile(fileext = ".json")
  write_roles(
    role_path,
    c(feature_id = "feature_id", `001` = "measurement", mean = "summary",
      pvalue = "imported_statistic", fc = "summary"),
    canonical = triomics_sha256(declared)
  )
  kept <- ingest_one(declared, "001", conversion = role_path)
  testthat::expect_identical(colnames(kept$assays$block_a), "001")
  testthat::expect_equal(kept$assays$block_a[[1L]], 5, tolerance = 1e-8)
  testthat::expect_false(any(kept$assays$block_a == 999, na.rm = TRUE))
  testthat::expect_false(any(kept$column_accounting$included_in_assay &
                               kept$column_accounting$role %in% c("summary", "imported_statistic")))

  claimed <- tempfile(fileext = ".json")
  write_roles(
    claimed,
    c(feature_id = "feature_id", `001` = "measurement", mean = "measurement",
      pvalue = "imported_statistic", fc = "summary"),
    canonical = triomics_sha256(declared)
  )
  expect_code(ingest_one(declared, c("001", "mean"), conversion = claimed),
              "A_SOURCE_SUMMARY_AS_MEASUREMENT")

  wrong_hash <- paste(rep("ab", 32), collapse = "")
  expect_code(ingest_one(declared, "001", sha = wrong_hash), "A_SOURCE_HASH_MISMATCH")

  stale_path <- tempfile(fileext = ".json")
  original_hash <- digest::digest("synthetic-workbook", algo = "sha256", serialize = FALSE)
  write_roles(
    stale_path,
    c(feature_id = "feature_id", `001` = "measurement", mean = "summary",
      pvalue = "imported_statistic", fc = "summary"),
    canonical = triomics_sha256(declared), mode = "external", original = original_hash,
    sheet = "Assay", region = list(row_start = 1L, row_end = 1L, col_start = 1L, col_end = 5L),
    formula = TRUE, cache = "stale"
  )
  expect_code(ingest_one(declared, "001", conversion = stale_path), "A_SOURCE_STALE_CONVERSION")

  fictional <- tempfile(fileext = ".json")
  write_roles(
    fictional,
    c(feature_id = "feature_id", `001` = "measurement"),
    canonical = triomics_sha256(bare), sheet = "Sheet1"
  )
  expect_code(ingest_one(bare, "001", conversion = fictional), "A_SOURCE_CONVERSION")

  make_pair <- function(left_value, right_value, tolerance) {
    left <- tempfile(fileext = ".tsv")
    right <- tempfile(fileext = ".tsv")
    write_tsv(left, c("feature_id", "s1"), list(c("f1", left_value)))
    write_tsv(right, c("feature_id", "s1"), list(c("f1", right_value)))
    region <- list(row_start = 1L, row_end = 1L, col_start = 1L, col_end = 2L)
    left_manifest <- tempfile(fileext = ".json")
    right_manifest <- tempfile(fileext = ".json")
    roles <- c(feature_id = "feature_id", s1 = "measurement")
    write_roles(left_manifest, roles, canonical = triomics_sha256(left), mode = "external",
                original = original_hash, sheet = "Assay", region = region, tolerance = tolerance)
    write_roles(right_manifest, roles, canonical = triomics_sha256(right), mode = "external",
                original = original_hash, sheet = "Assay", region = region, tolerance = tolerance)
    list(left = left, right = right, left_manifest = left_manifest, right_manifest = right_manifest)
  }
  conflict <- make_pair("2", "5", NULL)
  err <- expect_code(
    triomics_ingest_study_tables(
      study_id = "study",
      sample_registry = sample_registry("s1"),
      blocks = list(block_for("left", "block_left"), block_for("right", "block_right")),
      sources = list(
        source_for(conflict$left, source_id = "left", conversion = conflict$left_manifest),
        source_for(conflict$right, source_id = "right", conversion = conflict$right_manifest)
      )
    ),
    "A_SOURCE_OVERLAP"
  )
  testthat::expect_match(conditionMessage(err), "averaged", ignore.case = TRUE)

  tolerant <- make_pair("1", "1.1", 0.2)
  agreed <- triomics_ingest_study_tables(
    study_id = "study",
    sample_registry = sample_registry("s1"),
    blocks = list(block_for("left", "block_left"), block_for("right", "block_right")),
    sources = list(
      source_for(tolerant$left, source_id = "left", conversion = tolerant$left_manifest),
      source_for(tolerant$right, source_id = "right", conversion = tolerant$right_manifest)
    )
  )
  testthat::expect_equal(agreed$assays$block_left[[1L]], 1, tolerance = 1e-8)
  testthat::expect_equal(agreed$assays$block_right[[1L]], 1.1, tolerance = 1e-8)
  testthat::expect_false(any(abs(unlist(agreed$assays) - 1.05) < 1e-8))
})

testthat::test_that("A-T02-AC4 masks, selection, and structural absence stay distinct", {
  known_path <- tempfile(fileext = ".tsv")
  write_tsv(known_path, c("feature_id", "001", "10"), list(c("A", "4", "NA")))
  known <- ingest_one(known_path, c("001", "010", "10"), mask = "known",
                      upstream = "vendor_normalization")
  testthat::expect_identical(known$original_masks$block_a[1L, "001"], "known_observed")
  testthat::expect_identical(known$original_masks$block_a[1L, "10"], "known_missing")
  testthat::expect_false("010" %in% colnames(known$assays$block_a))
  absent <- known$availability[known$availability$sample_id == "010", , drop = FALSE]
  testthat::expect_identical(absent$presence, "structural_absence")
  testthat::expect_false(absent$cell_imputation_eligible)
  testthat::expect_identical(known$upstream_steps$block_a, "vendor_normalization")
  testthat::expect_false(any(known$original_masks$block_a == "structural_absence"))

  unknown_path <- tempfile(fileext = ".tsv")
  write_tsv(unknown_path, c("feature_id", "001", "10"), list(c("A", "4", "NA")))
  unknown <- ingest_one(unknown_path, c("001", "10"), mask = "unknown",
                        upstream = "vendor_imputation")
  testthat::expect_identical(as.vector(unknown$original_masks$block_a), c("unknown", "unknown"))
  testthat::expect_false(any(unknown$original_masks$block_a %in% c("known_observed", "known_missing",
                                                                  "known_imputed")))

  expect_code(ingest_one(known_path, c("001", "10"), mask = "partly_known"), "A_SOURCE_MASK")
  mask <- list(
    header = c("feature_id", "001", "10"),
    columns = list(c("A"), c("observed"), c("imputed"))
  )
  write_tsv(known_path, c("feature_id", "001", "10"), list(c("A", "4", "6")))
  mixed <- ingest_one(known_path, c("001", "10"), mask = "partly_known", cell_mask = mask)
  testthat::expect_identical(mixed$original_masks$block_a[1L, "001"], "known_observed")
  testthat::expect_identical(mixed$original_masks$block_a[1L, "10"], "known_imputed")

  upgraded <- list(header = c("feature_id", "001"), columns = list(c("A"), c("observed")))
  write_tsv(unknown_path, c("feature_id", "001"), list(c("A", "4")))
  expect_code(ingest_one(unknown_path, "001", mask = "unknown", cell_mask = upgraded), "A_SOURCE_MASK")

  selected <- tempfile(fileext = ".tsv")
  write_tsv(selected, c("feature_id", "001"), list(c("only-row", "5")))
  unknown_sel <- ingest_one(selected, "001", selection = "unknown")
  full_sel <- ingest_one(selected, "001", selection = "full_unselected")
  testthat::expect_identical(unknown_sel$assays$block_a, full_sel$assays$block_a)
  testthat::expect_identical(nrow(unknown_sel$feature_registry), 1L)
  testthat::expect_identical(unknown_sel$selection$selection_history[[1L]], "unknown")
  testthat::expect_false(isTRUE(unknown_sel$selection$verified_universe_complete[[1L]]))
  testthat::expect_identical(unknown_sel$selection$inference_gate[[1L]], "withheld")
  testthat::expect_identical(full_sel$selection$inference_gate[[1L]], "declared_unselected")
  testthat::expect_false(any(unknown_sel$original_masks$block_a %in%
                               c("unknown", "full_unselected", "outcome_selected")))

  fixture <- tempfile("a-t02-pattern-")
  config <- write_study_yaml(
    fixture,
    sample_rows = list(sample_id = c("SUBJ001_V2", "SUBJ001_V3"), group = c("control", "control")),
    matrix_rows = list(header = c("feature_id", "SUBJ001_V2", "SUBJ001_V3"),
                       rows = list(c("A", "1", "0"))),
    factor_levels = c("control", "treatment")
  )
  parsed <- triomics_read_config(config)
  ingested <- triomics_ingest_configured_study(parsed, config)
  testthat::expect_true(all(is.na(ingested$sample_registry$subject_id)))
  testthat::expect_true(all(is.na(ingested$sample_registry$visit_id)))
  testthat::expect_identical(ingested$clinical_observations, list())
  testthat::expect_identical(ingested$sample_registry$sample_id, c("SUBJ001_V2", "SUBJ001_V3"))
  testthat::expect_true("treatment" %in% ingested$declared_factor_levels$group)
  testthat::expect_identical(unique(ingested$sample_metadata$group), "control")
  testthat::expect_equal(ingested$assays$block_1[1L, "SUBJ001_V3"], 0, tolerance = 1e-8)

  extra <- write_study_yaml(
    tempfile("a-t02-clinical-"),
    sample_rows = list(sample_id = "SUBJ001_V2", group = "control"),
    matrix_rows = list(header = c("feature_id", "SUBJ001_V2"), rows = list(c("A", "1"))),
    extra_metadata = list(name = "diagnosis", values = list("invented"))
  )
  expect_code(triomics_ingest_configured_study(triomics_read_config(extra), extra), "A_SOURCE_ROLE")

  dup <- write_study_yaml(
    tempfile("a-t02-dup-"),
    sample_rows = list(sample_id = c("001", "001"), group = c("control", "treatment")),
    matrix_rows = list(header = c("feature_id", "001"), rows = list(c("A", "1")))
  )
  expect_code(triomics_ingest_configured_study(triomics_read_config(dup), dup), "A_ID_DUPLICATE_SAMPLE")

  bad_level <- write_study_yaml(
    tempfile("a-t02-level-"),
    sample_rows = list(sample_id = "001", group = "other"),
    matrix_rows = list(header = c("feature_id", "001"), rows = list(c("A", "1"))),
    factor_levels = c("control", "treatment")
  )
  expect_code(triomics_ingest_configured_study(triomics_read_config(bad_level), bad_level),
              "A_ID_REFERENCE")

  bad <- known
  bad$original_masks$block_a[1L, "001"] <- "structural_absence"
  expect_code(triomics_validate_ingest_contract(bad), "A_SOURCE_MASK")
  bad <- known
  bad$clinical_observations <- list(list(observation_id = "fabricated"))
  expect_code(triomics_validate_ingest_contract(bad), "A_CLINICAL_FABRICATED")
})

testthat::test_that("A-T02 stage A ingest uses predecessor fixtures without a checkpoint", {
  config_path <- triomics_test_resource("examples", "tickets", "A-T02.yaml")
  config <- triomics_read_config(config_path)
  ingested <- triomics_ingest_configured_study(config, config_path)
  raw <- read_tsv_lines(triomics_test_resource("examples", "data", "independent", "proteomics.tsv"))
  expected <- as.numeric(raw$rows[[1L]][[match("SYN_12", raw$header)]])
  key <- triomics_feature_key(config$study_id, "block_1", "1")
  testthat::expect_equal(ingested$assays$block_1[key, "SYN_12"], expected, tolerance = 1e-8)
  testthat::expect_true("block_2" %in% names(ingested$assays))
  testthat::expect_identical(ingested$clinical_observations, list())
  testthat::expect_false(any(ingested$availability$presence == "structural_absence" &
                               ingested$availability$cell_imputation_eligible %in% TRUE))

  output <- tempfile("a-t02-exec-")
  triomics_execute_stage_a(config_path, output)
  envelope <- file.path(output, "A", "assays.envelope.json")
  testthat::expect_true(triomics_validate_envelope(envelope))
  testthat::expect_false(any(grepl("checkpoint", list.files(output, recursive = TRUE),
                                  ignore.case = TRUE)))
  testthat::expect_false(file.exists(file.path(output, "clinical", "observations.json")))
  payload <- jsonlite::read_json(file.path(output, "A", "assays.json"), simplifyVector = FALSE)
  block <- payload$blocks[[1L]]
  row_index <- match(key, unlist(block$feature_keys))
  column_index <- match("SYN_12", unlist(block$sample_ids))
  testthat::expect_equal(block$values[[row_index]][[column_index]], expected, tolerance = 1e-8)
})
