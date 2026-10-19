c01_error_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

c01_matrix <- function(features, units, values) {
  matrix(values, nrow = length(features), ncol = length(units), byrow = TRUE,
         dimnames = list(features, units))
}

c01_resolved <- function(blocks, units = NULL, study_id = "study-synthetic",
                         context = "ctx-levels", representation = "levels") {
  if (is.null(units)) {
    units <- sort(unique(unlist(lapply(blocks, function(block) colnames(block)), use.names = FALSE)))
  }
  normalized <- lapply(names(blocks), function(block_id) {
    assay <- blocks[[block_id]]
    list(
      block_id = block_id,
      study_id = study_id,
      preparation_context_id = context,
      representation_id = representation,
      assay = assay,
      sample_ids = colnames(assay),
      feature_keys = rownames(assay)
    )
  })
  names(normalized) <- names(blocks)
  list(
    study_id = study_id,
    analysis_id = "analysis-synthetic",
    preparation_context_id = context,
    representation = representation,
    unit_ids = units,
    blocks = normalized
  )
}

c01_spec <- function(block_ids, units, training = units, targets = units,
                     n_components = 2L, scale_features = FALSE,
                     block_scaling = "none", selection = list(rule = "none")) {
  list(
    block_ids = block_ids,
    unit_ids = units,
    training_unit_ids = training,
    target_unit_ids = targets,
    n_components = n_components,
    scale_features = scale_features,
    block_scaling = block_scaling,
    selection = selection
  )
}

c01_fixture <- function() {
  units <- paste0("U", 1:6)
  block_a <- c01_matrix(
    c("A1", "A2", "A3", "A0"), units,
    c(1, 2, 4, 3, 5, 8,
      8, 7, 6, 5, 3, 2,
      2, 4, 1, 5, 3, 6,
      rep(9, 6))
  )
  block_b <- c01_matrix(
    c("B1", "B2", "B3"), units,
    c(3, 1, 4, 2, 8, 5,
      4, 6, 3, 7, 1, 5,
      5, 2, 7, 3, 9, 4)
  )
  blocks <- list(plasma_renamed = block_a, metabolite_set = block_b)
  list(
    resolved = c01_resolved(blocks, units),
    spec = c01_spec(names(blocks), units, n_components = 2L),
    blocks = blocks,
    units = units
  )
}

testthat::test_that("C-01 emits a finite keyed six-field result and rejects invalid source identity", {
  fixture <- c01_fixture()
  fit <- triomics_integration_pca_fit(fixture$resolved, fixture$spec)

  testthat::expect_s3_class(fit, "IntegrationResult")
  testthat::expect_identical(
    names(fit), c("scores", "loadings", "variance", "selected", "backend", "params")
  )
  testthat::expect_identical(rownames(fit$scores), sort(fixture$units))
  testthat::expect_identical(names(fit$loadings), fixture$spec$block_ids)
  testthat::expect_identical(names(fit$selected), fixture$spec$block_ids)
  testthat::expect_identical(rownames(fit$loadings$plasma_renamed), fit$selected$plasma_renamed)
  testthat::expect_true(all(is.finite(fit$scores)))
  testthat::expect_true(all(vapply(fit$loadings, function(x) all(is.finite(x)), logical(1))))
  testthat::expect_true(all(is.finite(fit$variance)))
  testthat::expect_identical(triomics_validate_integration_result(fit), fit)

  mixed <- fixture$resolved
  mixed$blocks$metabolite_set$study_id <- "another-study"
  c01_error_code(
    triomics_integration_eligibility(mixed, fixture$spec),
    "C_ELIGIBILITY_STUDY_MISMATCH"
  )

  summary_only <- fixture$resolved
  summary_only$blocks$plasma_renamed$assay <- NULL
  summary_only$blocks$plasma_renamed$summary <- list(feature_count = 3L)
  c01_error_code(
    triomics_integration_eligibility(summary_only, fixture$spec),
    "C_ELIGIBILITY_SUMMARY_ONLY"
  )
})

testthat::test_that("C-01 concatenation PCA matches prcomp and applies by feature identity", {
  fixture <- c01_fixture()
  fit <- triomics_integration_pca_fit(fixture$resolved, fixture$spec)
  training_x <- t(rbind(fixture$blocks$plasma_renamed[c("A1", "A2", "A3"), , drop = FALSE],
                        fixture$blocks$metabolite_set[, , drop = FALSE]))
  reference <- stats::prcomp(training_x, center = TRUE, scale. = FALSE, rank. = 2L)
  actual <- fit$scores[, colnames(reference$x), drop = FALSE]
  aligned_reference <- reference$x
  for (component in seq_len(ncol(actual))) {
    sign <- sign(sum(actual[, component] * aligned_reference[, component]))
    if (sign == 0) sign <- 1
    aligned_reference[, component] <- aligned_reference[, component] * sign
  }
  testthat::expect_equal(actual, aligned_reference, tolerance = 1e-8)

  reordered <- fixture$resolved
  reordered$blocks$plasma_renamed$assay <- reordered$blocks$plasma_renamed$assay[
    c("A0", "A3", "A1", "A2"), fixture$units[6:1], drop = FALSE
  ]
  reordered$blocks$plasma_renamed$feature_keys <- rownames(reordered$blocks$plasma_renamed$assay)
  reordered$blocks$plasma_renamed$sample_ids <- colnames(reordered$blocks$plasma_renamed$assay)
  reordered$blocks$metabolite_set$assay <- reordered$blocks$metabolite_set$assay[
    c("B3", "B1", "B2"), fixture$units[6:1], drop = FALSE
  ]
  reordered$blocks$metabolite_set$feature_keys <- rownames(reordered$blocks$metabolite_set$assay)
  reordered$blocks$metabolite_set$sample_ids <- colnames(reordered$blocks$metabolite_set$assay)
  applied <- triomics_integration_pca_apply(fit, reordered, target_unit_ids = fixture$units)
  testthat::expect_equal(applied$scores[fixture$units, , drop = FALSE], fit$scores[fixture$units, , drop = FALSE],
                         tolerance = 1e-8)

  missing_feature <- reordered
  missing_feature$blocks$plasma_renamed$assay <-
    missing_feature$blocks$plasma_renamed$assay[c("A0", "A3", "A1"), , drop = FALSE]
  missing_feature$blocks$plasma_renamed$feature_keys <- rownames(missing_feature$blocks$plasma_renamed$assay)
  c01_error_code(
    triomics_integration_pca_apply(fit, missing_feature, target_unit_ids = fixture$units),
    "C_ELIGIBILITY_FEATURE_MAP"
  )

  extra_feature <- reordered
  extra_feature$blocks$metabolite_set$assay <- rbind(
    extra_feature$blocks$metabolite_set$assay,
    B_EXTRA = rep(1, length(fixture$units))
  )
  extra_feature$blocks$metabolite_set$feature_keys <- rownames(extra_feature$blocks$metabolite_set$assay)
  c01_error_code(
    triomics_integration_pca_apply(fit, extra_feature, target_unit_ids = fixture$units),
    "C_ELIGIBILITY_FEATURE_MAP"
  )
})

testthat::test_that("C-01 block projection fractions equal each component's direct SSE reduction", {
  fixture <- c01_fixture()
  fit <- triomics_integration_pca_fit(fixture$resolved, fixture$spec)
  testthat::expect_identical(attr(fit$variance, "metric"), "orthogonal_pca_block_projection")
  training_x <- t(rbind(fixture$blocks$plasma_renamed[c("A1", "A2", "A3"), , drop = FALSE],
                        fixture$blocks$metabolite_set[, , drop = FALSE]))
  for (block_id in fixture$spec$block_ids) {
    keys <- fit$selected[[block_id]]
    block_x <- t(fixture$blocks[[block_id]][keys, , drop = FALSE])
    block_x <- scale(block_x, center = colMeans(block_x), scale = FALSE)
    previous <- matrix(0, nrow = nrow(block_x), ncol = ncol(block_x))
    for (component in seq_len(ncol(fit$scores))) {
      current <- previous + tcrossprod(
        fit$scores[, component], fit$loadings[[block_id]][, component]
      )
      sse_drop <- sum((block_x - previous)^2) - sum((block_x - current)^2)
      fraction <- sse_drop / sum(block_x^2)
      testthat::expect_equal(unname(fit$variance[block_id, component]), fraction,
                             tolerance = 1e-8)
      previous <- current
    }
  }
  testthat::expect_equal(nrow(training_x), length(fixture$units))
})

testthat::test_that("C-01 records scaling, dense top-k selection, zero-variance exclusion, and rank truncation", {
  fixture <- c01_fixture()
  spec <- c01_spec(
    fixture$spec$block_ids, fixture$units, n_components = 99L,
    scale_features = TRUE, block_scaling = "inverse_sqrt_p",
    selection = list(rule = "top_k", top_k = 2L)
  )
  fit <- triomics_integration_pca_fit(fixture$resolved, spec)
  testthat::expect_identical(fit$params$effective$scale_features, TRUE)
  testthat::expect_identical(fit$params$effective$block_scaling, "inverse_sqrt_p")
  testthat::expect_length(fit$selected$plasma_renamed, 2L)
  testthat::expect_false("A0" %in% fit$selected$plasma_renamed)
  testthat::expect_true("A0" %in% fit$params$feature_flow$plasma_renamed$zero_variance)
  testthat::expect_true(fit$params$rank_truncated)
  testthat::expect_identical(fit$params$requested_n_components, 99L)
  testthat::expect_identical(ncol(fit$scores), fit$params$retained_n_components)
  testthat::expect_true(all(fit$params$state$feature_scales > 0))
  testthat::expect_true(all(fit$params$state$block_weights > 0))

  rank_zero <- fixture$resolved
  rank_zero$blocks$plasma_renamed$assay[,] <- 3
  rank_zero$blocks$metabolite_set$assay[,] <- -2
  c01_error_code(
    triomics_integration_pca_fit(rank_zero, fixture$spec),
    "C_SCORE_RANK_ZERO"
  )
})

testthat::test_that("C-01 eligibility preserves two- and five-block availability with explicit dispositions", {
  units <- paste0("unit-", 1:6)
  two <- list(
    beta_view = c01_matrix(c("B2", "B1"), units[1:5], seq_len(10)),
    omega_view = c01_matrix(c("O1", "O2"), units[c(1:4, 6)], seq_len(10) + 1)
  )
  eligibility_two <- triomics_integration_eligibility(
    c01_resolved(two, units),
    c01_spec(names(two), units, training = units[1:4], targets = units)
  )
  testthat::expect_true(all(eligibility_two$unit_status$status[1:4] == "eligible"))
  testthat::expect_identical(eligibility_two$unit_status$status[5], "projected_out")
  testthat::expect_identical(eligibility_two$unit_status$reason_code[5], "required_view_missing")
  testthat::expect_identical(eligibility_two$unit_status$status[6], "projected_out")
  testthat::expect_true(all(names(eligibility_two$block_availability) == names(two)))
  testthat::expect_false(eligibility_two$block_availability$beta_view[["unit-6"]])
  testthat::expect_true(eligibility_two$block_availability$omega_view[["unit-6"]])

  five_names <- c("view-q", "view-2", "lipid-renamed", "omics_x", "block-z")
  five <- stats::setNames(lapply(seq_along(five_names), function(i) {
    available <- if (i == 2L) units[1:4] else units[1:5]
    c01_matrix(paste0("F", i, c("b", "a")), available,
               seq_len(2L * length(available)) + i)
  }), five_names)
  five_resolved <- c01_resolved(five, units)
  five_resolved$blocks[["view-2"]]$assay <- five_resolved$blocks[["view-2"]]$assay[c("F2a", "F2b"), , drop = FALSE]
  five_resolved$blocks[["view-2"]]$feature_keys <- rownames(five_resolved$blocks[["view-2"]]$assay)
  five_spec <- c01_spec(five_names, units, training = units[1:4], targets = units)
  eligibility_five <- triomics_integration_eligibility(five_resolved, five_spec)
  testthat::expect_identical(eligibility_five$block_ids, five_names)
  testthat::expect_identical(eligibility_five$unit_status$status[6], "refused")
  testthat::expect_identical(eligibility_five$unit_status$reason_code[6], "all_views_missing")
  testthat::expect_true(all(vapply(eligibility_five$block_availability, function(x) !x[["unit-6"]], logical(1))))
  testthat::expect_identical(eligibility_five$unit_status$status[5], "projected_out")
})

testthat::test_that("C-01 rejects unknown intersections and incompatible contexts without dropping rows", {
  fixture <- c01_fixture()
  unknown <- fixture$resolved
  unknown$blocks$metabolite_set$assay <- unknown$blocks$metabolite_set$assay[, fixture$units[1:5], drop = FALSE]
  unknown$blocks$metabolite_set$sample_ids <- fixture$units[1:5]
  spec <- fixture$spec
  spec$unit_ids <- NULL
  spec$target_unit_ids <- NULL
  c01_error_code(triomics_integration_eligibility(unknown, spec), "C_ELIGIBILITY_UNKNOWN_INTERSECTION")

  context <- fixture$resolved
  context$blocks$metabolite_set$preparation_context_id <- "other-context"
  c01_error_code(
    triomics_integration_eligibility(context, fixture$spec),
    "C_ELIGIBILITY_CONTEXT_MISMATCH"
  )

  requested_unknown <- fixture$spec
  requested_unknown$target_unit_ids <- c(fixture$units, "not-a-unit")
  eligibility <- triomics_integration_eligibility(fixture$resolved, requested_unknown)
  testthat::expect_identical(tail(eligibility$unit_status$status, 1L), "refused")
  testthat::expect_identical(tail(eligibility$unit_status$reason_code, 1L), "unknown_target_unit")
})

testthat::test_that("C-01 consensus freezes anchor signs and training-only standardization", {
  training_units <- paste0("T", 1:4)
  alpha <- matrix(c(1, 2, 3, 4), ncol = 1L,
                  dimnames = list(training_units, "PC1"))
  zeta <- matrix(c(-3, -6, -9, -12), ncol = 1L,
                 dimnames = list(training_units, "PC1"))
  loadings <- list(
    alpha = matrix(c(-0.9, 0.2), ncol = 1L,
                    dimnames = list(c("a-feature", "b-feature"), "PC1")),
    zeta = matrix(c(0.8, -0.1), ncol = 1L,
                   dimnames = list(c("z-feature", "y-feature"), "PC1"))
  )
  state <- make_consensus_state(
    list(zeta = zeta, alpha = alpha), loadings, training_units,
    list(required_views = c("alpha", "zeta"), correlation_tolerance = 1e-8,
         sd_tolerance = 1e-8)
  )
  consensus <- apply_consensus_state(list(alpha = alpha, zeta = zeta), state)
  expected <- as.numeric(scale(-c(1, 2, 3, 4)))
  testthat::expect_equal(unname(consensus[, "PC1"]), expected, tolerance = 1e-8)
  testthat::expect_identical(state$anchor_view, "alpha")
  testthat::expect_identical(unname(state$signs["alpha", "PC1"]), -1)
  testthat::expect_identical(unname(state$signs["zeta", "PC1"]), 1)

  held_out <- list(
    alpha = matrix(100, nrow = 1L, dimnames = list("H1", "PC1")),
    zeta = matrix(-300, nrow = 1L, dimnames = list("H1", "PC1"))
  )
  before <- state
  projected <- apply_consensus_state(held_out, state)
  expected_held_out <- mean(c(
    (-100 - mean(-c(1, 2, 3, 4))) / stats::sd(-c(1, 2, 3, 4)),
    (-300 - mean(-c(3, 6, 9, 12))) / stats::sd(-c(3, 6, 9, 12))
  ))
  testthat::expect_equal(unname(projected["H1", "PC1"]), expected_held_out, tolerance = 1e-8)
  testthat::expect_identical(state, before)

  permuted <- make_consensus_state(
    list(alpha = alpha, zeta = zeta),
    list(zeta = loadings$zeta[c("y-feature", "z-feature"), , drop = FALSE],
         alpha = loadings$alpha[c("b-feature", "a-feature"), , drop = FALSE]),
    training_units,
    list(required_views = c("zeta", "alpha"), correlation_tolerance = 1e-8,
         sd_tolerance = 1e-8)
  )
  testthat::expect_equal(
    apply_consensus_state(list(alpha = alpha, zeta = zeta), permuted),
    consensus,
    tolerance = 1e-8
  )
})

testthat::test_that("C-01 consensus refuses absent views, undefined alignment, and rank-zero native inputs", {
  training_units <- paste0("T", 1:4)
  one <- matrix(c(1, 2, 3, 4), ncol = 1L, dimnames = list(training_units, "PC1"))
  loadings <- list(
    alpha = matrix(c(0.8, 0.2), ncol = 1L, dimnames = list(c("f1", "f2"), "PC1")),
    zeta = matrix(c(0.7, 0.3), ncol = 1L, dimnames = list(c("g1", "g2"), "PC1"))
  )
  c01_error_code(
    make_consensus_state(list(alpha = one), loadings["alpha"], training_units,
                         list(required_views = c("alpha", "zeta"))),
    "C_SCORE_REQUIRED_VIEW_MISSING"
  )
  c01_error_code(
    make_consensus_state(list(
                           alpha = one,
                           zeta = matrix(c(1, -1, -1, 1), ncol = 1L,
                                         dimnames = list(training_units, "PC1"))
                         ),
                         loadings, training_units,
                         list(required_views = c("alpha", "zeta"), correlation_tolerance = 1e-8,
                              sd_tolerance = 1e-8)),
    "C_SCORE_ALIGNMENT_UNDEFINED"
  )
  constant <- matrix(2, nrow = length(training_units), ncol = 1L,
                     dimnames = list(training_units, "PC1"))
  c01_error_code(
    make_consensus_state(list(alpha = constant, zeta = one), loadings, training_units,
                         list(required_views = c("alpha", "zeta"), sd_tolerance = 1e-8)),
    "C_SCORE_RANK_ZERO"
  )
  state <- make_consensus_state(
    list(alpha = one, zeta = one * 2), loadings, training_units,
    list(required_views = c("alpha", "zeta"), correlation_tolerance = 1e-8,
         sd_tolerance = 1e-8)
  )
  c01_error_code(apply_consensus_state(list(alpha = one), state), "C_SCORE_REQUIRED_VIEW_MISSING")
})

testthat::test_that("the adapter registry exposes only accepted integration routes", {
  adapter <- triomics_integration_backend("concat_pca")
  testthat::expect_true(all(c("validate", "fit", "predict", "extract") %in% names(adapter)))
  testthat::expect_setequal(
    triomics_integration_backend_names(), c("concat_pca", "concat_plsda", "concat_pls")
  )
  c01_error_code(triomics_integration_backend("mixomics_diablo"), "C_SCORE_BACKEND_UNAVAILABLE")
})

testthat::test_that("C-01 rejects mismatched sample maps and non-finite assay values", {
  fixture <- c01_fixture()
  mismatched <- fixture$resolved
  mismatched$blocks$metabolite_set$sample_ids[1] <- "not-the-assay-column"
  c01_error_code(
    triomics_integration_eligibility(mismatched, fixture$spec),
    "C_ELIGIBILITY_SAMPLE_MAP"
  )

  non_finite <- fixture$resolved
  non_finite$blocks$plasma_renamed$assay[1, 1] <- Inf
  c01_error_code(
    triomics_integration_eligibility(non_finite, fixture$spec),
    "C_ELIGIBILITY_NONFINITE"
  )
})

testthat::test_that("C-01 result validation rejects extra fields and non-finite retained scores", {
  valid_shape <- structure(
    list(
      scores = matrix(1, 1, 1, dimnames = list("unit-1", "PC1")),
      loadings = list(view = matrix(1, 1, 1, dimnames = list("feature-1", "PC1"))),
      variance = matrix(1, 1, 1, dimnames = list("view", "PC1")),
      selected = list(view = "feature-1"),
      backend = "concat_pca",
      params = list()
    ),
    class = "IntegrationResult"
  )
  extra <- valid_shape
  extra$status <- "complete"
  c01_error_code(triomics_validate_integration_result(extra), "C_SCORE_RESULT_INVALID")

  non_finite <- valid_shape
  non_finite$scores[1, 1] <- Inf
  c01_error_code(triomics_validate_integration_result(non_finite), "C_SCORE_NONFINITE")
})

testthat::test_that("C-01 PCA fitting and scaling use training units only", {
  fixture <- c01_fixture()
  training_units <- fixture$units[1:4]
  target_units <- fixture$units[5:6]
  spec <- c01_spec(
    fixture$spec$block_ids, fixture$units, training = training_units,
    targets = target_units, scale_features = TRUE
  )
  fit <- triomics_integration_pca_fit(fixture$resolved, spec)

  changed_targets <- fixture$resolved
  for (block_id in names(changed_targets$blocks)) {
    changed_targets$blocks[[block_id]]$assay[, target_units] <-
      changed_targets$blocks[[block_id]]$assay[, target_units] + 10000
  }
  changed_fit <- triomics_integration_pca_fit(changed_targets, spec)
  testthat::expect_identical(
    changed_fit$params$state$feature_means,
    fit$params$state$feature_means
  )
  testthat::expect_identical(
    changed_fit$params$state$feature_scales,
    fit$params$state$feature_scales
  )
  testthat::expect_equal(changed_fit$scores, fit$scores, tolerance = 1e-8)
  testthat::expect_identical(rownames(fit$scores), training_units)

  projected <- triomics_integration_pca_apply(
    fit, fixture$resolved, target_unit_ids = target_units
  )
  testthat::expect_identical(rownames(projected$scores), target_units)
  testthat::expect_true(all(is.finite(projected$scores)))
})

testthat::test_that("C-01 public integration seam dispatches only the registered PCA route", {
  fixture <- c01_fixture()
  spec <- fixture$spec
  spec$method <- "concat_pca"
  result <- integrate_blocks(fixture$resolved, spec)
  testthat::expect_s3_class(result, "IntegrationResult")
  testthat::expect_identical(result$backend, "concat_pca")

  spec$method <- "mixomics_diablo"
  c01_error_code(
    integrate_blocks(fixture$resolved, spec),
    "C_SCORE_BACKEND_UNAVAILABLE"
  )
})

c01_b_resolved_fixture <- function(fixture) {
  units <- fixture$units
  prepared_study <- list(
    study_id = fixture$resolved$study_id,
    sample_metadata = data.frame(
      sample_id = units,
      group = c(rep("control", length(units) / 2L), rep("treatment", length(units) / 2L)),
      stringsAsFactors = FALSE
    ),
    declared_factor_levels = list(group = c("control", "treatment")),
    assays = fixture$blocks,
    feature_registry = NULL,
    assay_hashes = NULL,
    analysis_unit_map = data.frame(
      unit_id = units,
      representation_id = rep("levels", length(units)),
      stringsAsFactors = FALSE
    ),
    sample_registry = NULL
  )
  analysis_spec <- list(
    analysis_id = fixture$resolved$analysis_id,
    block_ids = names(fixture$blocks),
    preparation_context_id = fixture$resolved$preparation_context_id,
    design = list(
      kind = "independent",
      condition = "group",
      condition_type = "factor",
      repeated = FALSE,
      representation = "levels",
      reference_level = "control",
      target_level = "treatment",
      subject_id = NULL,
      covariates = list()
    )
  )
  triomics_resolve_analysis(prepared_study, analysis_spec)
}

testthat::test_that("C-01 consumes the actual B resolver result through the public seam", {
  fixture <- c01_fixture()
  resolved <- c01_b_resolved_fixture(fixture)
  testthat::expect_identical(resolved$study_id, fixture$resolved$study_id)
  testthat::expect_identical(resolved$preparation_context_id, fixture$resolved$preparation_context_id)
  testthat::expect_identical(resolved$representation, "levels")
  testthat::expect_false(any(c("study_id", "preparation_context_id", "representation_id") %in%
                             names(resolved$blocks$plasma_renamed)))

  spec <- fixture$spec
  spec$method <- "concat_pca"
  result <- integrate_blocks(resolved, spec)
  testthat::expect_s3_class(result, "IntegrationResult")
  testthat::expect_identical(result$params$provenance$study_id, resolved$study_id)
  testthat::expect_identical(result$params$provenance$preparation_context_id,
                             resolved$preparation_context_id)
  testthat::expect_identical(result$params$provenance$representation_id, resolved$representation)
})

testthat::test_that("C-01 refuses fewer than two requested predictor blocks", {
  fixture <- c01_fixture()
  spec <- fixture$spec
  spec$method <- "concat_pca"
  spec$block_ids <- fixture$spec$block_ids[1L]
  c01_error_code(
    integrate_blocks(fixture$resolved, spec),
    "C_ELIGIBILITY_BLOCK_COUNT"
  )
})

testthat::test_that("C-01 rejects missing training coverage in every requested PCA block", {
  fixture <- c01_fixture()
  resolved <- fixture$resolved
  missing_training_unit <- fixture$units[[4L]]
  resolved$blocks$metabolite_set$assay <- resolved$blocks$metabolite_set$assay[
    , setdiff(colnames(resolved$blocks$metabolite_set$assay), missing_training_unit), drop = FALSE
  ]
  resolved$blocks$metabolite_set$sample_ids <- colnames(resolved$blocks$metabolite_set$assay)
  spec <- fixture$spec
  spec$method <- "concat_pca"
  spec$training_unit_ids <- fixture$units[1:4]
  spec$target_unit_ids <- fixture$units
  spec$required_views <- "plasma_renamed"
  c01_error_code(
    integrate_blocks(resolved, spec),
    "C_ELIGIBILITY_TRAINING_MISSING"
  )
})

testthat::test_that("C-01 public integration seam types malformed arguments", {
  fixture <- c01_fixture()
  spec <- fixture$spec
  spec$method <- "concat_pca"
  c01_error_code(
    integrate_blocks(fixture$resolved, 1),
    "C_ELIGIBILITY_INPUT_INVALID"
  )
  c01_error_code(
    integrate_blocks(1, spec),
    "C_ELIGIBILITY_INPUT_INVALID"
  )
})
