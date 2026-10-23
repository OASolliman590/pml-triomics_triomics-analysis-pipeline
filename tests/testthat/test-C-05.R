c05_error <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

# Injected probe: the missing-package path is tested without depending on what
# this machine has installed.
c05_probe <- function(loaded) {
  function(package) {
    is_loaded <- isTRUE(loaded[[package]])
    list(
      package = package, loaded = is_loaded,
      version = if (is_loaded) "0.0.0-test" else NULL,
      reason = if (is_loaded) NULL else "r_package_not_loadable"
    )
  }
}

c05_runtime_fixture <- function(cell = "scattered_na", repeats = 3L) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(505)
  units <- sprintf("sample_%02d", seq_len(10L))
  make_assay <- function(prefix) {
    matrix(
      stats::rnorm(50L), nrow = 5L,
      dimnames = list(paste0(prefix, "_feature_", seq_len(5L)), units)
    )
  }
  first <- make_assay("view_1")
  second <- make_assay("view_2")
  if (identical(cell, "scattered_na")) first[2L, 3L] <- NA_real_
  if (identical(cell, "missing_sample_view")) second[, 10L] <- NA_real_
  if (identical(cell, "all_missing_feature")) second[3L, ] <- NA_real_
  if (identical(cell, "all_view_missing_sample")) {
    first[, 1L] <- NA_real_
    second[, 1L] <- NA_real_
  }
  root <- tempfile("c05-runtime-")
  dir.create(root)
  list(
    root = root,
    resolved = list(
      analysis_id = "c05-synthetic-runtime",
      study_id = "c05-synthetic-runtime",
      unit_ids = units,
      blocks = list(
        view_1 = list(assay = first, feature_keys = rownames(first), sample_ids = colnames(first)),
        view_2 = list(assay = second, feature_keys = rownames(second), sample_ids = colnames(second))
      )
    ),
    params = list(
      method = "mofa2", block_ids = c("view_1", "view_2"), n_components = 2L,
      scale_features = TRUE, seed = 20260920L, mofa_seed_repeats = repeats,
      mofa_control_regime = "weak_null", mofa_convergence = "medium",
      model_path = file.path(root, "mofa-primary.hdf5")
    )
  )
}

c05_mapping_edge <- function(target_entity_id) {
  list(
    block_id = "view_1", feature_key = "view_1_feature_1",
    target_namespace = "synthetic_namespace", target_entity_id = target_entity_id,
    relation = "maps_to", resolution = "exact", taxon = "synthetic_taxon",
    context = "synthetic_context", resource_release = "fixture-v1",
    evidence = "synthetic_fixture_evidence", ambiguity_group = "synthetic-group-1"
  )
}

c05_sidecar_state <- function() {
  model_path <- tempfile("c05-sidecar-model-")
  writeBin(as.raw(c(0x4d, 0x4f, 0x46, 0x41)), model_path)
  list(
    model_path = model_path,
    native_model_sha256 = triomics_mofa_hash_file(model_path),
    training_input_hashes = list(
      transformed_data_sha256 = paste(rep("a", 64L), collapse = ""),
      missingness_mask_sha256 = paste(rep("b", 64L), collapse = ""),
      transformed_data_and_masks_sha256 = paste(rep("c", 64L), collapse = "")
    ),
    native_version = "1.22.1", python_backend_version = "0.7.5",
    training_state_hash = paste(rep("d", 64L), collapse = ""),
    components = "Factor1", block_ids = c("view_1", "view_2"),
    factors = matrix(0, nrow = 2L, ncol = 1L,
                     dimnames = list(c("sample_1", "sample_2"), "Factor1")),
    weights = list(
      view_1 = matrix(0, nrow = 2L, ncol = 1L,
                      dimnames = list(c("view_1_feature_1", "view_1_feature_2"), "Factor1")),
      view_2 = matrix(0, nrow = 2L, ncol = 1L,
                      dimnames = list(c("view_2_feature_1", "view_2_feature_2"), "Factor1"))
    ),
    variance = matrix(0, nrow = 1L, ncol = 2L),
    variance_semantics = list()
  )
}

c05_require_runtime <- function() {
  status <- triomics_mofa_backend_status()
  testthat::expect_true(status$available)
  testthat::expect_identical(status$packages$MOFA2$version, "1.22.1")
  testthat::expect_identical(status$packages$mofapy2$version, "0.7.5")
  status
}

testthat::test_that("C-05 missing-package path names each absent runtime and is available only when both load", {
  both_missing <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = FALSE, mofapy2 = FALSE)))
  testthat::expect_false(both_missing$available)
  testthat::expect_identical(both_missing$missing, c("MOFA2", "mofapy2"))

  mofapy_missing <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = FALSE)))
  testthat::expect_false(mofapy_missing$available)
  testthat::expect_identical(mofapy_missing$missing, "mofapy2")

  mofa_missing <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = FALSE, mofapy2 = TRUE)))
  testthat::expect_false(mofa_missing$available)
  testthat::expect_identical(mofa_missing$missing, "MOFA2")

  both_loaded <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = TRUE)))
  testthat::expect_true(both_loaded$available)
  testthat::expect_identical(both_loaded$missing, character())
})

testthat::test_that("C-05 missing-package path returns an unavailable state with no factors, weights, variance or reload agreement", {
  status <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = FALSE, mofapy2 = FALSE)))
  state <- triomics_mofa_fit(list(), list(method = "mofa2"), status = status)

  testthat::expect_identical(state$status, "unavailable")
  testthat::expect_false(state$trained)
  testthat::expect_identical(state$missing_packages, c("MOFA2", "mofapy2"))
  testthat::expect_false(any(c("factors", "weights", "variance", "scores", "loadings") %in% names(state)))
  testthat::expect_identical(state$reload$status, "not_evaluated")
  testthat::expect_false("agreement" %in% names(state$reload))
})

testthat::test_that("C-05 missing-package path refuses when only one runtime is absent", {
  status <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = FALSE)))
  state <- triomics_mofa_fit(list(), list(method = "mofa2"), status = status)

  testthat::expect_identical(state$status, "unavailable")
  testthat::expect_false(state$trained)
  testthat::expect_identical(state$missing_packages, "mofapy2")
  testthat::expect_false("factors" %in% names(state))
})

testthat::test_that("C-05 does not report training when both runtimes load but the training path is absent", {
  status <- triomics_mofa_backend_status(probe = c05_probe(c(MOFA2 = TRUE, mofapy2 = TRUE)))
  real <- triomics_mofa_real_runtime_status()
  if (!isTRUE(real$MOFA2$loaded) || !isTRUE(real$mofapy2$loaded)) {
    err <- c05_error(
      triomics_mofa_fit(list(), list(method = "mofa2"), status = status),
      "C_SCORE_BACKEND_UNAVAILABLE"
    )
    testthat::expect_match(conditionMessage(err), "no factors, weights or variance were produced")
  } else {
    err <- c05_error(
      triomics_mofa_fit(list(), list(method = "mofa2"), status = status),
      "C_ELIGIBILITY_BLOCK_MAP"
    )
    testthat::expect_false(isTRUE(err$trained))
  }
})

testthat::test_that("C-05 python status refuses an unconfigured or unlaunchable mofapy2 runtime", {
  unconfigured <- triomics_mofa_python_status("")
  testthat::expect_false(unconfigured$loaded)
  testthat::expect_identical(unconfigured$reason, "python_not_configured")

  missing_binary <- triomics_mofa_python_status(file.path(tempdir(), "c05-no-such-python"))
  testthat::expect_false(missing_binary$loaded)
  testthat::expect_identical(missing_binary$reason, "mofapy2_import_failed")
})

testthat::test_that("C-05 live runtime status agrees with the R namespace and the default probe", {
  live <- triomics_mofa_backend_status()

  testthat::expect_identical(live$packages$MOFA2$loaded, requireNamespace("MOFA2", quietly = TRUE))
  testthat::expect_identical(
    live$available,
    all(vapply(live$packages, function(status) isTRUE(status$loaded), logical(1L)))
  )
})

testthat::test_that("C-05 environment check reports both runtimes as not loaded from an empty library and exits non-zero", {
  script <- testthat::test_path("..", "..", "tools", "check-mofa-environment.R")
  source_checkout <- file.exists(testthat::test_path(
    "..", "..", "discovery", "spec-v2", "schemas", "config.schema.json"
  ))
  if (!source_checkout) {
    # tools/ is excluded from the built package. The runtime probe remains the installed check.
    live <- triomics_mofa_backend_status()
    if (isTRUE(live$available)) {
      err <- c05_error(
        triomics_mofa_fit(list(), list(method = "mofa2"), status = live),
        "C_ELIGIBILITY_BLOCK_MAP"
      )
      testthat::expect_false(isTRUE(err$trained))
    } else {
      state <- triomics_mofa_fit(list(), list(method = "mofa2"), status = live)
      testthat::expect_identical(state$status, "unavailable")
      testthat::expect_false(state$trained)
    }
    return()
  }
  testthat::expect_true(file.exists(script))
  library <- tempfile("c05-empty-library-")
  dir.create(library)
  # An ambient TRIOMICS_MOFA_PYTHON must not make the empty-library probe look loaded.
  # Pass only the variables the child needs. A full Sys.getenv() dump can
  # contain newlines that make system2 reject the env vector.
  keep <- c("PATH", "HOME", "TMPDIR", "LANG", "LC_ALL", "R_HOME")
  env <- Sys.getenv(keep, unset = NA_character_)
  env <- env[!is.na(env) & !grepl("[\r\n]", env)]
  output <- suppressWarnings(system2(
    "Rscript", c("--vanilla", script, "--library", library),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0(names(env), "=", env), "TRIOMICS_MOFA_PYTHON=")
  ))

  testthat::expect_identical(attr(output, "status"), 1L)
  testthat::expect_true(any(grepl("^MOFA2: not loaded", output)))
  testthat::expect_true(any(grepl("^mofapy2: not loaded", output)))
  testthat::expect_true(any(grepl("No model was fitted", output, fixed = TRUE)))
})

testthat::test_that("C-05 ticket validates under the existing config schema and requests mofa2 without an outcome", {
  previous <- options(triomics.source_root = normalizePath(
    testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE
  ))
  on.exit(options(previous), add = TRUE)
  config <- triomics_read_config(triomics_test_resource("examples", "tickets", "C-05.yaml"))
  request <- config$analyses[[1L]]$integration[[1L]]

  testthat::expect_identical(request$method, "mofa2")
  testthat::expect_null(request$outcome)
  testthat::expect_identical(request$mofa_convergence, "medium")
  testthat::expect_identical(request$evaluation$mode, "exploratory")
})

testthat::test_that("C-05 public registry exposes only the exploratory MOFA2 adapter", {
  adapter <- triomics_integration_backend("mofa2")
  testthat::expect_s3_class(adapter, "BackendAdapter")
  testthat::expect_identical(adapter$evaluation_modes, "exploratory")
  c05_error(
    adapter$predict(NULL, NULL, list(method = "mofa2")),
    "C_EVAL_PREDICTION_UNSUPPORTED"
  )
  c05_error(triomics_integration_backend("mofa2_unregistered"), "C_SCORE_BACKEND_UNAVAILABLE")
})

testthat::test_that("C-05 seed sensitivity is JSON-safe at the shared CLI boundary", {
  sensitivity <- structure(
    list(status = "assessed", seeds = c(1L, 2L), comparisons = list()),
    class = "MofaSeedSensitivity"
  )
  result <- triomics_new_integration_result(
    scores = matrix(0, nrow = 2L, ncol = 1L,
                    dimnames = list(c("sample_1", "sample_2"), "Factor1")),
    loadings = list(view_1 = matrix(
      0, nrow = 1L, ncol = 1L, dimnames = list("feature_1", "Factor1")
    )),
    variance = matrix(0, nrow = 1L, ncol = 1L,
                      dimnames = list("view_1", "Factor1")),
    selected = list(view_1 = "feature_1"), backend = "mofa2",
    params = list(seed_sensitivity = sensitivity)
  )

  payload <- triomics_c01_result_payload(result)
  testthat::expect_null(attr(payload$params$seed_sensitivity, "class", exact = TRUE))
  testthat::expect_no_error(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
})

testthat::test_that("C-05 registration records the accepted synthetic-software-only route", {
  registration_path <- testthat::test_path(
    "..", "..", "docs", "implementation", "registrations", "C-05.json"
  )
  source_checkout <- file.exists(testthat::test_path(
    "..", "..", "discovery", "spec-v2", "schemas", "config.schema.json"
  ))
  if (!source_checkout) {
    # docs/ is excluded from the built package. The public adapter must still be registered.
    testthat::expect_s3_class(triomics_integration_backend("mofa2"), "BackendAdapter")
    return()
  }
  testthat::expect_true(file.exists(registration_path))
  registration <- jsonlite::read_json(registration_path, simplifyVector = TRUE)

  testthat::expect_identical(registration$packet_id, "C-05")
  testthat::expect_true(registration$accepted)
  testthat::expect_true(registration$applied_to_shared_files)
  testthat::expect_true(registration$cli_registration$enabled)
  testthat::expect_true(registration$cli_registration$requested_enabled)
  testthat::expect_identical(registration$cli_registration$gate, "passed")
  testthat::expect_identical(registration$integration_backends$method, "mofa2")
  testthat::expect_identical(registration$dependencies$package, c("MOFA2", "mofapy2"))
  testthat::expect_match(registration$implementation_status, "accepted on synthetic software evidence")
  testthat::expect_match(registration$implementation_status, "No real-study or biological validation is claimed")
  testthat::expect_match(registration$integration_backends$status, "implemented_registered")
  testthat::expect_true(any(grepl("synthetic software evidence only", registration$known_gaps, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "private-study validation and biological factor interpretation remain unperformed",
    registration$known_gaps, fixed = TRUE
  )))
  testthat::expect_true(any(grepl("enrichment execution was not performed", registration$known_gaps, fixed = TRUE)))
  testthat::expect_false(any(grepl("not implemented", registration$known_gaps, fixed = TRUE)))
})

testthat::test_that("C-05 native percent variance converts a sub-one-percent value to a proportion", {
  native <- matrix(
    c(0.5, 40), nrow = 2L,
    dimnames = list(c("Factor1", "Factor2"), "view_1")
  )
  converted <- triomics_mofa_variance_proportions(native, c("Factor1", "Factor2"), "view_1")

  testthat::expect_equal(
    unname(converted$variance), matrix(c(0.005, 0.4), nrow = 1L), tolerance = 1e-15
  )
  testthat::expect_identical(converted$variance_units, "proportion_from_native_percentage")
  testthat::expect_identical(converted$semantics$native_unit, "percentage")
  testthat::expect_identical(converted$semantics$conversion, "divide_by_100_unconditionally")
})

testthat::test_that("C-05 seed comparison aligns sign and permutation and compares factor subspaces", {
  reference <- cbind(
    Factor1 = c(-2, -1, 0, 1, 2),
    Factor2 = c(2, -1, -2, -1, 2)
  )
  rownames(reference) <- paste0("sample_", seq_len(nrow(reference)))
  candidate <- cbind(FactorA = -reference[, "Factor2"], FactorB = reference[, "Factor1"])
  rownames(candidate) <- rownames(reference)

  aligned <- triomics_mofa_align_seed_factors(reference, candidate)
  angle <- pi / 5
  rotated <- reference %*% matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2L)
  testthat::expect_identical(unname(aligned$permutation), c(2L, 1L))
  testthat::expect_identical(unname(aligned$signs), c(1, -1))
  testthat::expect_equal(unname(aligned$scores), unname(reference), tolerance = 1e-12)
  testthat::expect_equal(unname(aligned$aligned_correlations), c(1, 1), tolerance = 1e-12)
  testthat::expect_equal(
    triomics_mofa_subspace_similarity(reference, rotated)$similarity, 1, tolerance = 1e-12
  )

  classified <- triomics_mofa_seed_sensitivity(list(
    list(seed = 1L, status = "complete", outputs = list(factors = reference)),
    list(seed = 2L, status = "software_failure", error_code = "C_EVAL_NATIVE_FIT")
  ), control_regime = "weak_null")
  testthat::expect_identical(classified$status, "software_failure")
  testthat::expect_null(classified$recovery_threshold)
  testthat::expect_identical(classified$factor_naming, "native_generic_ids_only")
})

testthat::test_that("C-05 enrichment handoff accepts typed MappingEdges and hashes their canonical meaning", {
  state <- c05_sidecar_state()
  on.exit(unlink(state$model_path), add = TRUE)
  edges <- list(c05_mapping_edge("synthetic:target-b"), c05_mapping_edge("synthetic:target-a"))
  as_table <- as.data.frame(do.call(rbind, lapply(edges, as.data.frame)), stringsAsFactors = FALSE)
  as_table <- as_table[nrow(as_table):1L, , drop = FALSE]

  handoff <- triomics_mofa_enrichment_input_sidecar(
    state, context = list(annotation_mappings = edges)
  )
  table_handoff <- triomics_mofa_enrichment_input_sidecar(
    state, context = list(annotation_mappings = as_table)
  )
  expected <- data.frame(
    block_id = c("view_1", "view_1"),
    feature_key = c("view_1_feature_1", "view_1_feature_1"),
    target_namespace = c("synthetic_namespace", "synthetic_namespace"),
    target_entity_id = c("synthetic:target-a", "synthetic:target-b"),
    relation = c("maps_to", "maps_to"), resolution = c("exact", "exact"),
    taxon = c("synthetic_taxon", "synthetic_taxon"),
    context = c("synthetic_context", "synthetic_context"),
    resource_release = c("fixture-v1", "fixture-v1"),
    evidence = c("synthetic_fixture_evidence", "synthetic_fixture_evidence"),
    ambiguity_group = c("synthetic-group-1", "synthetic-group-1"),
    stringsAsFactors = FALSE
  )

  testthat::expect_identical(handoff$annotation_mappings, expected)
  testthat::expect_identical(table_handoff$annotation_mappings, expected)
  testthat::expect_identical(handoff$hashes$annotation_mappings_sha256,
                             table_handoff$hashes$annotation_mappings_sha256)
  testthat::expect_identical(handoff$annotation_mapping_status, "supplied")
  testthat::expect_identical(handoff$status, "ready_for_downstream_handoff")

  changed <- triomics_mofa_enrichment_input_sidecar(
    state, context = list(annotation_mappings = list(c05_mapping_edge("synthetic:target-c")))
  )
  testthat::expect_false(identical(
    handoff$hashes$annotation_mappings_sha256,
    changed$hashes$annotation_mappings_sha256
  ))
})

testthat::test_that("C-05 enrichment handoff rejects malformed MappingEdge collections with a typed error", {
  state <- c05_sidecar_state()
  on.exit(unlink(state$model_path), add = TRUE)

  for (malformed in list("synthetic scalar", list("not an edge object"))) {
    err <- c05_error(
      triomics_mofa_enrichment_input_sidecar(
        state, context = list(annotation_mappings = malformed)
      ),
      "D_MAPPING_INVALID"
    )
    testthat::expect_identical(err$config_key, "annotation_mappings")
  }
})

testthat::test_that("C-05 enrichment handoff rejects missing MappingEdge fields", {
  state <- c05_sidecar_state()
  on.exit(unlink(state$model_path), add = TRUE)
  incomplete <- c05_mapping_edge("synthetic:target-a")
  incomplete$ambiguity_group <- NULL

  err <- c05_error(
    triomics_mofa_enrichment_input_sidecar(
      state, context = list(annotation_mappings = list(incomplete))
    ),
    "D_MAPPING_INVALID"
  )
  testthat::expect_identical(err$config_key, "annotation_mappings")
})

testthat::test_that("C-05 enrichment handoff rejects MappingEdges outside the fitted feature map", {
  state <- c05_sidecar_state()
  on.exit(unlink(state$model_path), add = TRUE)
  absent <- c05_mapping_edge("synthetic:target-a")
  absent$feature_key <- "not_a_fitted_feature"

  err <- c05_error(
    triomics_mofa_enrichment_input_sidecar(
      state, context = list(annotation_mappings = list(absent))
    ),
    "D_MAPPING_INVALID"
  )
  testthat::expect_identical(err$config_key, "feature_key")
})

testthat::test_that("C-05 enrichment handoff marks empty mappings as not supplied", {
  state <- c05_sidecar_state()
  on.exit(unlink(state$model_path), add = TRUE)
  handoff <- triomics_mofa_enrichment_input_sidecar(state, context = list())

  testthat::expect_identical(handoff$status, "annotation_mapping_not_supplied")
  testthat::expect_identical(handoff$annotation_mapping_status, "not_supplied")
  testthat::expect_identical(nrow(handoff$annotation_mappings), 0L)
})

testthat::test_that("C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes", {
  status <- c05_require_runtime()
  ticket <- triomics_read_config(triomics_test_resource("examples", "tickets", "C-05.yaml"))
  request <- ticket$analyses[[1L]]$integration[[1L]]
  fixture <- c05_runtime_fixture("scattered_na", repeats = request$mofa_seed_repeats)
  fixture$params$seed <- ticket$execution$seed
  fixture$params$mofa_convergence <- request$mofa_convergence
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  eligibility <- triomics_mofa_validate(fixture$resolved, fixture$params)

  fit <- triomics_mofa_fit(fixture$resolved, fixture$params, status = status)
  extraction <- triomics_mofa_extract(
    fit,
    context = list(annotation_mappings = list(c05_mapping_edge("synthetic:target-a"))),
    params = fixture$params
  )

  testthat::expect_true(fit$trained)
  testthat::expect_null(names(fit$selected$view_1))
  testthat::expect_identical(fit$selected$view_1, rownames(fit$native_state$weights$view_1))
  testthat::expect_true(eligibility$missingness_mask$view_1["view_1_feature_2", "sample_03"])
  testthat::expect_true(fit$native_state$missingness_mask$view_1["view_1_feature_2", "sample_03"])
  testthat::expect_identical(fit$result_params$variance$assay_space, "adapter_transformed_training_views")
  testthat::expect_identical(
    fit$result_params$variance$denominator,
    "sum_of_squared_observed_transformed_values_about_zero_per_view"
  )
  testthat::expect_identical(
    fit$result_params$variance$accumulation,
    "single_factor_per_view_against_zero_baseline_not_cumulative"
  )

  sensitivity <- fit$result_params$seed_sensitivity
  testthat::expect_identical(sensitivity$seeds, c(20260920L, 20260921L, 20260922L))
  testthat::expect_identical(sensitivity$requested_repeats, 3L)
  testthat::expect_identical(vapply(sensitivity$runs, `[[`, character(1L), "status"), rep("complete", 3L))
  testthat::expect_length(sensitivity$comparisons, 2L)
  for (comparison in sensitivity$comparisons) {
    info <- paste("seed", comparison$seed, "subspace", comparison$subspace_similarity,
                  "aligned factors", length(comparison$aligned_correlations))
    testthat::expect_true(is.finite(comparison$subspace_similarity), info = info)
    testthat::expect_true(comparison$subspace_similarity >= 0, info = info)
    testthat::expect_true(comparison$subspace_similarity <= 1, info = info)
    testthat::expect_true(length(comparison$aligned_correlations) > 0L, info = info)
    testthat::expect_identical(
      length(comparison$aligned_correlations),
      length(comparison$alignment$matched_reference_factors), info = info
    )
  }
  testthat::expect_null(sensitivity$recovery_threshold)
  testthat::expect_identical(sensitivity$factor_naming, "native_generic_ids_only")
  testthat::expect_identical(sensitivity$interpretation, "descriptive_stability_uncertainty_no_forced_recovery")

  handoff <- extraction$sidecars$enrichment_inputs
  testthat::expect_s3_class(handoff, "MofaEnrichmentInputSidecar")
  testthat::expect_identical(handoff$schema_version, "2.0")
  testthat::expect_identical(handoff$native_version, "1.22.1")
  testthat::expect_identical(handoff$python_backend_version, "0.7.5")
  testthat::expect_identical(
    handoff$hashes$native_model_sha256,
    digest::digest(file = fit$native_state$model_path, algo = "sha256", serialize = FALSE)
  )
  testthat::expect_identical(
    handoff$hashes$transformed_data_and_masks_sha256,
    digest::digest(
      list(
        transformed_data = eligibility$data,
        missingness_mask = eligibility$missingness_mask
      ),
      algo = "sha256", serialize = TRUE
    )
  )
  testthat::expect_identical(handoff$annotation_mapping_status, "supplied")
  testthat::expect_identical(nrow(handoff$annotation_mappings), 1L)
  testthat::expect_identical(handoff$annotation_mappings$feature_key, "view_1_feature_1")
  testthat::expect_identical(handoff$enrichment_execution, "not_performed")
})

testthat::test_that("C-05 real MOFA2 fits and extracts a missing sample-view capability cell", {
  status <- c05_require_runtime()
  fixture <- c05_runtime_fixture("missing_sample_view", repeats = 1L)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  fit <- triomics_mofa_fit(fixture$resolved, fixture$params, status = status)
  extraction <- triomics_mofa_extract(fit, context = list(), params = fixture$params)

  testthat::expect_true(fit$trained)
  testthat::expect_true(all(fit$native_state$missingness_mask$view_2[, "sample_10"]))
  testthat::expect_s3_class(extraction$result, "IntegrationResult")
})

testthat::test_that("C-05 all-missing features and all-view-missing samples fail with typed missingness errors", {
  status <- c05_require_runtime()

  feature <- c05_runtime_fixture("all_missing_feature", repeats = 1L)
  on.exit(unlink(feature$root, recursive = TRUE), add = TRUE)
  feature_error <- c05_error(
    triomics_mofa_fit(feature$resolved, feature$params, status = status), "C_NA_CAPABILITY"
  )
  testthat::expect_identical(feature_error$config_key, "view_2_feature_3")

  sample <- c05_runtime_fixture("all_view_missing_sample", repeats = 1L)
  on.exit(unlink(sample$root, recursive = TRUE), add = TRUE)
  c05_error(
    triomics_mofa_fit(sample$resolved, sample$params, status = status), "C_NA_CAPABILITY"
  )
})
