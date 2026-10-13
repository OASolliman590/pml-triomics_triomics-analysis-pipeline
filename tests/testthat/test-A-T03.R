expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

sample_registry <- function(ids, subject = NULL, visit = NULL, group = NULL) {
  n <- length(ids)
  frame <- data.frame(sample_id = ids, stringsAsFactors = FALSE)
  if (!is.null(subject)) frame$subject_id <- subject
  if (!is.null(visit)) frame$visit_id <- visit
  if (!is.null(group)) frame$group <- group
  frame$sample_role <- rep("undeclared", n)
  frame
}

feature_rows <- function(study_id, block_id, keys, original_ids, scale = "linear") {
  n <- length(keys)
  data.frame(
    study_id = rep(study_id, n),
    block_id = rep(block_id, n),
    feature_key = keys,
    original_id = original_ids,
    id_namespace = rep("synthetic", n),
    source_id = rep(block_id, n),
    source_row = as.character(seq_len(n)),
    measured_or_derived = rep("measured", n),
    quantity = rep("intensity", n),
    scale = rep(scale, n),
    units = rep("synthetic_arbitrary_units", n),
    structural_resolution = rep("unknown", n),
    alias_refs = rep("", n),
    parent_refs = rep("", n),
    annotation_route = rep("none", n),
    stringsAsFactors = FALSE
  )
}

named_map <- function(block_id, assay_columns, sample_ids, source_columns = assay_columns) {
  data.frame(
    block_id = rep(block_id, length(sample_ids)),
    assay_column = assay_columns,
    sample_id = sample_ids,
    source_column = source_columns,
    stringsAsFactors = FALSE
  )
}

manual_study <- function(study_id, registry, assays, masks, maps, features) {
  ids <- names(assays)
  names(maps) <- ids
  list(
    study_id = study_id,
    sample_registry = registry,
    feature_registry = do.call(rbind, features),
    sample_maps = maps,
    assays = assays,
    original_masks = masks,
    availability = do.call(rbind, lapply(ids, function(id) {
      data.frame(
        study_id = study_id,
        block_id = id,
        sample_id = registry$sample_id,
        presence = ifelse(registry$sample_id %in% colnames(assays[[id]]) |
                            registry$sample_id %in% maps[[id]]$sample_id,
                          "present", "structural_absence"),
        cell_imputation_eligible = FALSE,
        stringsAsFactors = FALSE
      )
    })),
    source_manifests = list(),
    selection = data.frame(),
    column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = stats::setNames(replicate(length(ids), character(), FALSE), ids)
  )
}

observed_mask <- function(assay, codes = NULL) {
  mask <- matrix("known_observed", nrow(assay), ncol(assay), dimnames = dimnames(assay))
  if (!is.null(codes)) {
    mask[] <- codes
    dimnames(mask) <- dimnames(assay)
  }
  mask
}

analysis_of <- function(study, block_id) {
  SummarizedExperiment::assay(study$experiments[[block_id]], "analysis")
}

mask_of <- function(study, block_id) {
  SummarizedExperiment::assay(study$experiments[[block_id]], "original_mask")
}

baseline_ops <- function(filter_method = "none", transform = "none", impute = "none",
                         min_observed = NULL, zero_policy = "error", zero_value = NULL,
                         derive = "none", apply = NULL) {
  ops <- list(
    list(stage = "validate", method = "identity"),
    list(stage = "filter", method = filter_method, min_observed = min_observed, prevalence = NULL),
    list(stage = "derive", method = derive),
    list(stage = "transform", method = transform, zero_policy = zero_policy, zero_value = zero_value),
    list(stage = "normalise", method = "none"),
    list(stage = "impute", method = impute)
  )
  if (!is.null(apply)) {
    ops <- lapply(ops, function(op) {
      op$apply <- apply
      op
    })
  }
  ops
}

fit_spec <- function(block_id, profile_id, dir, training, operations, context = "ctx") {
  list(
    preparation_context_id = context,
    training_unit_ids = training,
    seed = 7L,
    state_dir = dir,
    blocks = list(list(
      block_id = block_id,
      profile_id = profile_id,
      operations = operations
    ))
  )
}

register_baseline <- function() {
  triomics_reset_profiles()
  triomics_register_profile("baseline", triomics_baseline_profile())
}

testthat::test_that("A-T03-AC1 sample maps keep identity across order, shape, and alignment", {
  values_a <- matrix(c(1, 4, 2, 5), nrow = 2, dimnames = list(c("fa", "fb"), c("s1", "s2")))
  values_b <- matrix(c(NA_real_, 50, 10, NA_real_), nrow = 2, dimnames = list(c("ga", "gb"), c("s2", "s1")))
  mask_b <- matrix(
    c("known_missing", "known_observed", "known_observed", "known_missing"),
    nrow = 2, dimnames = dimnames(values_b)
  )
  registry <- sample_registry(
    c("s1", "s2"),
    subject = c(NA_character_, NA_character_),
    group = c("control", "treatment")
  )
  raw <- manual_study(
    "study",
    registry,
    list(alpha = values_a, beta = values_b),
    list(alpha = observed_mask(values_a), beta = mask_b),
    list(
      alpha = named_map("alpha", c("s1", "s2"), c("s1", "s2"), c("raw-s1", "raw-s2")),
      beta = named_map("beta", c("s2", "s1"), c("s2", "s1"), c("raw-s2", "raw-s1"))
    ),
    list(
      feature_rows("study", "alpha", c("fa", "fb"), c("SAME", "other")),
      feature_rows("study", "beta", c("gb", "ga"), c("SAME", "beta-only"))
    )
  )
  # Registry rows are deliberately not in assay-row order for beta.
  raw$feature_registry <- raw$feature_registry[c(1, 2, 4, 3), , drop = FALSE]
  assembled <- triomics_assemble_study(raw, display_names = c(alpha = "Proteomics view", beta = "Other view"))
  alpha <- analysis_of(assembled, "alpha")
  beta <- analysis_of(assembled, "beta")
  testthat::expect_identical(colnames(alpha), c("s1", "s2"))
  testthat::expect_identical(colnames(beta), c("s1", "s2"))
  testthat::expect_equal(as.numeric(beta["ga", "s1"]), 10, tolerance = 1e-12)
  testthat::expect_true(is.na(beta["ga", "s2"]))
  testthat::expect_equal(as.numeric(alpha["fa", c("s1", "s2")]), c(1, 2), tolerance = 1e-12)
  testthat::expect_identical(mask_of(assembled, "beta")["ga", "s1"], "known_observed")
  testthat::expect_identical(mask_of(assembled, "beta")["ga", "s2"], "known_missing")
  cold <- as.data.frame(SummarizedExperiment::colData(assembled$experiments$beta))
  testthat::expect_identical(cold["s2", "group"], "treatment")
  testthat::expect_identical(cold["s1", "source_column"], "raw-s1")
  row_beta <- as.data.frame(SummarizedExperiment::rowData(assembled$experiments$beta))
  testthat::expect_identical(row_beta["ga", "original_id"], "beta-only")
  testthat::expect_identical(row_beta["ga", "feature_key"], "ga")
  testthat::expect_false(identical(rownames(assembled$experiments$alpha), rownames(assembled$experiments$beta)))
  testthat::expect_identical(names(assembled$experiments), c("alpha", "beta"))
  testthat::expect_identical(
    names(MultiAssayExperiment::experiments(assembled$mae)),
    c("alpha", "beta")
  )
  map <- as.data.frame(MultiAssayExperiment::sampleMap(assembled$mae))
  map$assay <- as.character(map$assay)
  testthat::expect_identical(nrow(map), 4L)
  testthat::expect_identical(
    map$primary[map$assay == "beta" & map$colname == "s1"],
    "s1"
  )
  testthat::expect_equal(
    as.numeric(SummarizedExperiment::assay(
      MultiAssayExperiment::experiments(assembled$mae)[["beta"]], "analysis"
    )["ga", "s1"]),
    10,
    tolerance = 1e-12
  )
  triomics_validate_se_dimensions(assembled$experiments$alpha)
  triomics_validate_se_dimensions(assembled$experiments$beta)
  testthat::expect_identical(dim(SummarizedExperiment::rowData(assembled$experiments$alpha)), c(2L, ncol(SummarizedExperiment::rowData(assembled$experiments$alpha))))
  testthat::expect_identical(dim(alpha), dim(mask_of(assembled, "alpha")))

  renamed <- triomics_assemble_study(raw, display_names = c(alpha = "Not a profile", beta = "Also not"))
  testthat::expect_identical(analysis_of(renamed, "beta"), beta)
  testthat::expect_identical(names(renamed$experiments), c("alpha", "beta"))

  partial <- manual_study(
    "study",
    sample_registry(c("s1", "s2", "s3")),
    list(
      alpha = matrix(c(1, 2, 3), nrow = 1, dimnames = list("fa", c("s1", "s2", "s3"))),
      beta = matrix(c(10, 20), nrow = 1, dimnames = list("ga", c("s2", "s1")))
    ),
    list(
      alpha = observed_mask(matrix(c(1, 2, 3), nrow = 1, dimnames = list("fa", c("s1", "s2", "s3")))),
      beta = observed_mask(matrix(c(10, 20), nrow = 1, dimnames = list("ga", c("s2", "s1"))))
    ),
    list(
      alpha = named_map("alpha", c("s1", "s2", "s3"), c("s1", "s2", "s3")),
      beta = named_map("beta", c("s2", "s1"), c("s2", "s1"))
    ),
    list(
      feature_rows("study", "alpha", "fa", "A"),
      feature_rows("study", "beta", "ga", "B")
    )
  )
  err <- expect_code(triomics_assemble_study(partial), "A_SAMPLE_ALIGNMENT")
  testthat::expect_match(conditionMessage(err), "s3", fixed = TRUE)
  crossed <- triomics_assemble_study(partial, alignment = "intersection")
  testthat::expect_identical(colnames(analysis_of(crossed, "alpha")), c("s1", "s2"))
  testthat::expect_false("s3" %in% colnames(analysis_of(crossed, "alpha")))
  testthat::expect_equal(as.numeric(analysis_of(crossed, "alpha")["fa", c("s1", "s2")]), c(1, 2), tolerance = 1e-12)
  testthat::expect_true(any(crossed$alignment$records$sample_id == "s3" &
                               crossed$alignment$records$status == "removed"))
  testthat::expect_identical(crossed$alignment$policy, "intersection")
  united <- triomics_assemble_study(partial, alignment = "union")
  testthat::expect_identical(colnames(analysis_of(united, "alpha")), c("s1", "s2", "s3"))
  testthat::expect_identical(colnames(analysis_of(united, "beta")), c("s1", "s2"))
  testthat::expect_false("s3" %in% colnames(analysis_of(united, "beta")))
  testthat::expect_false(anyNA(analysis_of(united, "beta")))
  testthat::expect_equal(as.numeric(analysis_of(united, "alpha")["fa", "s3"]), 3, tolerance = 1e-12)
  testthat::expect_true(any(united$alignment$records$sample_id == "s3" &
                               united$alignment$records$block_id == "beta" &
                               united$alignment$records$status == "absent"))

  raw_cols <- c("raw2", "raw1")
  labeled <- matrix(c(7, 4), nrow = 1, dimnames = list("fa", raw_cols))
  labeled_study <- manual_study(
    "study",
    sample_registry(c("s1", "s2")),
    list(alpha = labeled),
    list(alpha = observed_mask(labeled)),
    list(alpha = named_map("alpha", c("raw1", "raw2"), c("s1", "s2"), c("raw1", "raw2"))),
    list(feature_rows("study", "alpha", "fa", "A"))
  )
  by_name <- triomics_assemble_study(labeled_study)
  testthat::expect_equal(as.numeric(analysis_of(by_name, "alpha")["fa", c("s1", "s2")]), c(4, 7), tolerance = 1e-12)

  visits <- manual_study(
    "study",
    sample_registry(c("visit-a", "visit-b"), subject = c("P", "P"), visit = c("a", "b")),
    list(alpha = matrix(c(1, 9), nrow = 1, dimnames = list("fa", c("visit-b", "visit-a")))),
    list(alpha = observed_mask(matrix(c(1, 9), nrow = 1, dimnames = list("fa", c("visit-b", "visit-a"))))),
    list(alpha = named_map("alpha", c("visit-b", "visit-a"), c("visit-b", "visit-a"))),
    list(feature_rows("study", "alpha", "fa", "A"))
  )
  visited <- triomics_assemble_study(visits)
  testthat::expect_identical(colnames(analysis_of(visited, "alpha")), c("visit-a", "visit-b"))
  testthat::expect_equal(as.numeric(analysis_of(visited, "alpha")["fa", c("visit-a", "visit-b")]), c(9, 1), tolerance = 1e-12)
  testthat::expect_identical(nrow(SummarizedExperiment::colData(visited$experiments$alpha)), 2L)
  testthat::expect_false(any(colnames(analysis_of(visited, "alpha")) == "P"))

  blocks <- list()
  masks <- list()
  maps <- list()
  features <- list()
  ids <- c("zeta", "alpha", "mu", "delta", "epsilon")
  registry5 <- sample_registry(c("s1", "s2"))
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    columns <- if (i %% 2L == 0L) c("s2", "s1") else c("s1", "s2")
    vals <- c(i * 100 + 1, i * 100 + 2)
    names(vals) <- c("s1", "s2")
    mat <- matrix(unname(vals[columns]), nrow = 1, dimnames = list("f", columns))
    blocks[[id]] <- mat
    masks[[id]] <- observed_mask(mat)
    maps[[id]] <- named_map(id, columns, columns)
    features[[id]] <- feature_rows("wide", id, "f", paste0("orig-", id))
  }
  wide <- triomics_assemble_study(manual_study("wide", registry5, blocks, masks, maps, features))
  testthat::expect_identical(names(wide$experiments), ids)
  for (i in seq_along(ids)) {
    got <- analysis_of(wide, ids[[i]])
    testthat::expect_equal(as.numeric(got["f", c("s1", "s2")]), c(i * 100 + 1, i * 100 + 2), tolerance = 1e-12)
  }
  one <- triomics_assemble_study(manual_study(
    "one", registry5,
    blocks["zeta"], masks["zeta"], maps["zeta"], features["zeta"]
  ))
  testthat::expect_identical(names(one$experiments), "zeta")
  testthat::expect_equal(as.numeric(analysis_of(one, "zeta")["f", "s2"]), 102, tolerance = 1e-12)

  se <- assembled$experiments$alpha
  extra <- SummarizedExperiment::colData(se)
  bad_col <- rbind(as.data.frame(extra), s9 = as.data.frame(extra)[1L, , drop = FALSE])
  rownames(bad_col)[nrow(bad_col)] <- "s9"
  map_only <- as.data.frame(MultiAssayExperiment::sampleMap(assembled$mae))
  map_only <- map_only[map_only$assay == "alpha", c("assay", "primary", "colname")]
  expect_code(
    triomics_bind_mae(list(alpha = se), bad_col, map_only),
    "A_SAMPLE_MAP_DROP"
  )
  short_map <- map_only[map_only$colname == "s1", , drop = FALSE]
  expect_code(triomics_bind_mae(list(alpha = se), as.data.frame(extra), short_map), "A_SAMPLE_MAP")

  dir <- tempfile("a-t03-shape-")
  dir.create(dir)
  register_baseline()
  shape_values <- matrix(c(1, 1, 8, 2, NA_real_, 9), nrow = 3,
                         dimnames = list(c("keep", "drop", "also"), c("s1", "s2")))
  shape_mask <- observed_mask(shape_values)
  shape_mask["drop", "s2"] <- "known_missing"
  shape_mask["also", "s2"] <- "known_imputed"
  shaped <- manual_study(
    "study",
    sample_registry(c("s1", "s2")),
    list(alpha = shape_values),
    list(alpha = shape_mask),
    list(alpha = named_map("alpha", c("s1", "s2"), c("s1", "s2"))),
    list(feature_rows("study", "alpha", c("keep", "drop", "also"), c("k", "d", "a")))
  )
  before <- triomics_assemble_study(shaped)
  triomics_validate_se_dimensions(before$experiments$alpha)
  testthat::expect_identical(nrow(analysis_of(before, "alpha")), 3L)
  fitted <- prepare_fit(before, fit_spec(
    "alpha", "baseline", dir, c("s1", "s2"),
    baseline_ops(filter_method = "prevalence", min_observed = 2L, transform = "none")
  ))
  after <- analysis_of(fitted, "alpha")
  testthat::expect_identical(rownames(after), "keep")
  testthat::expect_identical(dim(after), c(1L, 2L))
  testthat::expect_identical(dim(mask_of(fitted, "alpha")), dim(after))
  testthat::expect_identical(
    nrow(SummarizedExperiment::rowData(fitted$experiments$alpha)),
    1L
  )
  triomics_validate_se_dimensions(fitted$experiments$alpha)
  testthat::expect_equal(as.numeric(after["keep", c("s1", "s2")]), c(1, 2), tolerance = 1e-12)
  testthat::expect_true(any(fitted$feature_flow$feature_key == "drop" & fitted$feature_flow$retained %in% FALSE))
  testthat::expect_true(any(fitted$feature_flow$feature_key == "also" & fitted$feature_flow$n_observed == 1))
  testthat::expect_false("drop" %in% rownames(after))
  checkpoints <- fitted$checkpoints
  testthat::expect_true(length(checkpoints) >= 1L)
  testthat::expect_identical(nrow(checkpoints[[1L]]$values), 3L)
  testthat::expect_false("values" %in% SummarizedExperiment::assayNames(fitted$experiments$alpha))
  assay_dims <- vapply(SummarizedExperiment::assays(fitted$experiments$alpha), dim, integer(2))
  testthat::expect_true(all(assay_dims[1L, ] == 1L))
})

testthat::test_that("A-T03-AC2 profile dispatch ignores display names and records no-op provenance", {
  expect_code(triomics_register_profile("short", list(validate = identity)), "A_CONFIG_PROFILE")
  register_baseline()
  testthat::expect_identical(
    names(triomics_registered_profile("baseline")),
    c("validate", "filter", "transform", "normalise", "impute", "derive")
  )
  decoy <- triomics_baseline_profile()
  decoy$transform <- function(se, params) {
    if (!is.null(params$display_name) || !is.null(params$block_name)) {
      triomics_abort("A_CONFIG_PROFILE", "Display name was supplied to a profile.", key = "name")
    }
    mat <- SummarizedExperiment::assay(se, "analysis")
    mat[] <- mat * 100
    SummarizedExperiment::assay(se, "analysis") <- mat
    se
  }
  triomics_register_profile("proteomics", decoy)
  registry <- sample_registry(c("s1", "s2"))
  mat <- matrix(c(1, 2, 8, 4), nrow = 2, dimnames = list(c("fa", "fb"), c("s2", "s1")))
  raw <- manual_study(
    "study", registry,
    list(alpha = mat),
    list(alpha = observed_mask(mat)),
    list(alpha = named_map("alpha", c("s2", "s1"), c("s2", "s1"))),
    list(feature_rows("study", "alpha", c("fa", "fb"), c("o1", "o2")))
  )
  left <- triomics_assemble_study(raw, display_names = c(alpha = "Proteomics"))
  right <- triomics_assemble_study(raw, display_names = c(alpha = "Renamed block"))
  dir_left <- tempfile("a-t03-left-")
  dir_right <- tempfile("a-t03-right-")
  dir.create(dir_left)
  dir.create(dir_right)
  ops <- baseline_ops(transform = "log2")
  fit_left <- prepare_fit(left, fit_spec("alpha", "baseline", dir_left, c("s1", "s2"), ops, "left"))
  fit_right <- prepare_fit(right, fit_spec("alpha", "baseline", dir_right, c("s1", "s2"), ops, "right"))
  testthat::expect_equal(
    as.numeric(analysis_of(fit_left, "alpha")["fa", c("s1", "s2")]),
    c(log2(8), log2(1)),
    tolerance = 1e-12
  )
  testthat::expect_identical(analysis_of(fit_left, "alpha"), analysis_of(fit_right, "alpha"))
  testthat::expect_identical(
    fit_left$state_refs$alpha$state_hash,
    fit_right$state_refs$alpha$state_hash
  )
  testthat::expect_false(grepl("Proteomics", fit_left$state_refs$alpha$state_hash))
  stages <- vapply(fit_left$operation_ledger$alpha, function(op) op$stage, character(1))
  methods <- vapply(fit_left$operation_ledger$alpha, function(op) op$method, character(1))
  testthat::expect_identical(stages, c("validate", "filter", "derive", "transform", "normalise", "impute"))
  testthat::expect_identical(methods[[4L]], "log2")
  none_dir <- tempfile("a-t03-none-")
  dir.create(none_dir)
  none_fit <- prepare_fit(left, fit_spec("alpha", "baseline", none_dir, c("s1", "s2"), baseline_ops(), "none"))
  testthat::expect_equal(
    as.numeric(analysis_of(none_fit, "alpha")["fa", c("s1", "s2")]),
    c(8, 1),
    tolerance = 1e-12
  )
  none_methods <- vapply(none_fit$operation_ledger$alpha, function(op) op$method, character(1))
  testthat::expect_identical(none_methods[[4L]], "none")
  testthat::expect_true(length(none_fit$state_refs$alpha$state_hash) == 1L)

  zeros <- matrix(c(0, 2), nrow = 1, dimnames = list("fa", c("s1", "s2")))
  zero_study <- triomics_assemble_study(manual_study(
    "study", registry, list(alpha = zeros), list(alpha = observed_mask(zeros)),
    list(alpha = named_map("alpha", c("s1", "s2"), c("s1", "s2"))),
    list(feature_rows("study", "alpha", "fa", "z"))
  ))
  expect_code(
    prepare_fit(zero_study, fit_spec("alpha", "baseline", tempfile("zdir"), c("s1", "s2"), baseline_ops(transform = "log2"), "z")),
    "A_SCALE_ZERO"
  )
  replaced <- prepare_fit(zero_study, fit_spec(
    "alpha", "baseline", { d <- tempfile("zrep-"); dir.create(d); d }, c("s1", "s2"),
    baseline_ops(transform = "log2", zero_policy = "declared_replacement", zero_value = 0.5),
    "zrep"
  ))
  testthat::expect_equal(as.numeric(analysis_of(replaced, "alpha")["fa", "s1"]), log2(0.5), tolerance = 1e-12)
  testthat::expect_equal(as.numeric(analysis_of(replaced, "alpha")["fa", "s2"]), 1, tolerance = 1e-12)

  again <- expect_code(
    prepare_fit(fit_left, fit_spec("alpha", "baseline", tempfile("again"), c("s1", "s2"), ops, "again")),
    "A_SCALE_DOUBLE_LOG"
  )
  testthat::expect_match(conditionMessage(again), "log2", fixed = TRUE)

  vsn_ops <- baseline_ops(transform = "log2")
  vsn_ops[[5L]]$method <- "vsn"
  expect_code(
    triomics_freeze_operations(list(block_id = "alpha", profile_id = "baseline", operations = vsn_ops)),
    "A_SCALE_DOUBLE_LOG"
  )
})

testthat::test_that("A-T03-AC3 state refs keep hashes, masks, training ids, and reject stale apply", {
  register_baseline()
  registry <- sample_registry(c("s1", "s2", "s3"), group = c("control", "control", "treatment"))
  values <- matrix(
    c(1, 5, 3, NA, 100, 9),
    nrow = 2,
    dimnames = list(c("fa", "fb"), c("s1", "s2", "s3"))
  )
  mask <- observed_mask(values)
  mask["fa", "s3"] <- "known_missing"
  mask["fb", "s2"] <- "known_missing"
  values["fa", "s3"] <- NA_real_
  values["fb", "s2"] <- NA_real_
  raw <- manual_study(
    "study", registry,
    list(alpha = values[, c("s3", "s1", "s2"), drop = FALSE]),
    list(alpha = mask[, c("s3", "s1", "s2"), drop = FALSE]),
    list(alpha = named_map("alpha", c("s3", "s1", "s2"), c("s3", "s1", "s2"))),
    list(feature_rows("study", "alpha", c("fa", "fb"), c("A", "B")))
  )
  # Permute columns after dimnames were copied in the opposite order above.
  raw$assays$alpha <- values[, c("s3", "s1", "s2"), drop = FALSE]
  raw$original_masks$alpha <- mask[, c("s3", "s1", "s2"), drop = FALSE]
  assembled <- triomics_assemble_study(raw)
  dir <- tempfile("a-t03-state-")
  dir.create(dir)
  ops <- baseline_ops(filter_method = "prevalence", min_observed = 1L, impute = "training_median")
  fitted <- prepare_fit(assembled, fit_spec("alpha", "baseline", dir, c("s1", "s2"), ops))
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  testthat::expect_identical(state$training_unit_ids, c("s1", "s2"))
  testthat::expect_identical(state$mode, "fit")
  testthat::expect_identical(
    vapply(state$operation_order, function(op) op$stage, character(1)),
    c("validate", "filter", "derive", "transform", "normalise", "impute")
  )
  fa <- state$mask_lineage[[which(vapply(state$mask_lineage, function(row) row$feature_key, character(1)) == "fa")]]
  testthat::expect_identical(fa$codes[match(c("s1", "s2"), fa$samples)], c("known_observed", "known_observed"))
  medians <- state$learned_parameters$medians
  fa_median <- medians[[which(vapply(medians, function(row) row$feature_key, character(1)) == "fa")]]$median
  fb_median <- medians[[which(vapply(medians, function(row) row$feature_key, character(1)) == "fb")]]$median
  testthat::expect_equal(fa_median, 2, tolerance = 1e-12)
  testthat::expect_equal(fb_median, 5, tolerance = 1e-12)
  testthat::expect_equal(as.numeric(analysis_of(fitted, "alpha")["fa", "s3"]), 2, tolerance = 1e-12)
  testthat::expect_identical(mask_of(fitted, "alpha")["fa", "s3"], "known_imputed")
  testthat::expect_identical(mask_of(fitted, "alpha")["fa", "s1"], "known_observed")
  testthat::expect_equal(as.numeric(analysis_of(fitted, "alpha")["fb", "s1"]), 5, tolerance = 1e-12)
  testthat::expect_false("outcome" %in% names(state))

  repeat_dir <- tempfile("a-t03-repeat-")
  dir.create(repeat_dir)
  refit <- prepare_fit(assembled, fit_spec("alpha", "baseline", repeat_dir, c("s1", "s2"), ops, "repeat"))
  testthat::expect_identical(refit$state_refs$alpha$state_hash, fitted$state_refs$alpha$state_hash)

  held <- assembled
  held$experiments$alpha <- assembled$experiments$alpha
  SummarizedExperiment::assay(held$experiments$alpha, "analysis")["fa", "s3"] <- 100
  SummarizedExperiment::assay(held$experiments$alpha, "original_mask")["fa", "s3"] <- "known_observed"
  # Rebuild MAE is unnecessary for prepare_apply, which reads experiments.
  apply_dir_values <- analysis_of(held, "alpha")
  applied <- prepare_apply(held, fit_spec("alpha", "baseline", dir, NULL, ops, "apply"), fitted$state_refs)
  testthat::expect_identical(applied$state_refs$alpha$state_hash, fitted$state_refs$alpha$state_hash)
  testthat::expect_equal(as.numeric(analysis_of(applied, "alpha")["fa", "s3"]), 100, tolerance = 1e-12)
  reread <- triomics_read_state(applied$state_refs$alpha, dir)
  fa_median_2 <- reread$learned_parameters$medians[[which(vapply(reread$learned_parameters$medians, function(row) row$feature_key, character(1)) == "fa")]]$median
  testthat::expect_equal(fa_median_2, 2, tolerance = 1e-12)
  testthat::expect_identical(reread$training_unit_ids, c("s1", "s2"))

  missing_apply <- assembled
  SummarizedExperiment::assay(missing_apply$experiments$alpha, "analysis")["fb", ] <- NA_real_
  SummarizedExperiment::assay(missing_apply$experiments$alpha, "original_mask")["fb", ] <- "known_missing"
  # Reverse feature order so a positional subset would attach fa's values to fb.
  reversed <- missing_apply$experiments$alpha[c("fb", "fa"), ]
  missing_apply$experiments$alpha <- reversed
  applied_missing <- prepare_apply(
    missing_apply,
    fit_spec("alpha", "baseline", dir, NULL, ops, "apply-missing"),
    fitted$state_refs
  )
  testthat::expect_identical(rownames(analysis_of(applied_missing, "alpha")), state$feature_keys)
  testthat::expect_equal(
    as.numeric(analysis_of(applied_missing, "alpha")["fb", "s1"]),
    reread$learned_parameters$medians[[which(vapply(reread$learned_parameters$medians, function(row) row$feature_key, character(1)) == "fb")]]$median,
    tolerance = 1e-12
  )
  testthat::expect_true("fb" %in% rownames(analysis_of(applied_missing, "alpha")))

  path <- file.path(dir, fitted$state_refs$alpha$path)
  bytes <- readLines(path, warn = FALSE)
  flipped <- sub("known_observed", "known_missing", paste(bytes, collapse = "\n"), fixed = TRUE)
  writeLines(flipped, path)
  expect_code(triomics_read_state(fitted$state_refs$alpha, dir), "A_STATE_STALE")
  writeLines(bytes, path)

  partial <- list(mode = "fit", state_hash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                  state_id = "st_aaaaaaaaaaaaaaaaaaaa", path = "partial.json")
  writeLines("{\"mode\":\"fit\"}", file.path(dir, "partial.json"))
  expect_code(triomics_read_state(partial, dir), "A_STATE_PARTIAL")
  tmp <- paste0(fitted$state_refs$alpha$path, ".tmp")
  writeLines(bytes, file.path(dir, tmp))
  tmp_ref <- fitted$state_refs$alpha
  tmp_ref$path <- tmp
  expect_code(triomics_read_state(tmp_ref, dir), "A_STATE_PARTIAL")

  malicious <- triomics_baseline_profile()
  malicious$filter <- function(se, params) {
    if (identical(params$mode, "apply")) return(se[-1L, , drop = FALSE])
    triomics_shared_filter(se, params)
  }
  triomics_register_profile("malicious", malicious)
  expect_code(
    prepare_apply(assembled, fit_spec("alpha", "malicious", dir, NULL, ops, "mal"), fitted$state_refs),
    "A_STATE_RESELECT"
  )

  unavailable <- lapply(ops, function(op) {
    op$apply <- "unavailable"
    op
  })
  marker <- triomics_baseline_profile()
  marker$transform <- function(se, params) {
    mat <- SummarizedExperiment::assay(se, "analysis")
    mat[] <- mat + 1000
    SummarizedExperiment::assay(se, "analysis") <- mat
    se
  }
  triomics_register_profile("blocked", marker)
  blocked <- prepare_apply(
    assembled,
    fit_spec("alpha", "blocked", dir, NULL, unavailable, "blocked"),
    fitted$state_refs
  )
  testthat::expect_identical(blocked$predictive_status$alpha, "unavailable")
  testthat::expect_equal(analysis_of(blocked, "alpha"), analysis_of(assembled, "alpha"))
  testthat::expect_false(any(abs(analysis_of(blocked, "alpha") - analysis_of(assembled, "alpha")) > 0, na.rm = TRUE))
})

testthat::test_that("A-T03-AC4 shared preparation runs a caller-supplied profile without proteomic code", {
  triomics_reset_profiles()
  custom <- triomics_baseline_profile()
  custom$transform <- function(se, params) {
    if (!identical(params$operation$method, "plus_one")) {
      triomics_abort("A_CONFIG_METHOD", "Unexpected method.", key = "transform")
    }
    if (!is.null(params$display_name)) {
      triomics_abort("A_CONFIG_PROFILE", "Branched on a display name.", key = "name")
    }
    mat <- SummarizedExperiment::assay(se, "analysis")
    finite <- is.finite(mat)
    mat[finite] <- mat[finite] + 1
    SummarizedExperiment::assay(se, "analysis") <- mat
    meta <- S4Vectors::metadata(se)
    meta$triomics$scale <- "linear"
    S4Vectors::metadata(se) <- meta
    se
  }
  triomics_register_profile("custom_panel", custom)
  registry <- sample_registry(c("s1", "s2"))
  mat <- matrix(c(3, 4), nrow = 1, dimnames = list("fa", c("s2", "s1")))
  assembled <- triomics_assemble_study(manual_study(
    "study", registry, list(panel = mat), list(panel = observed_mask(mat)),
    list(panel = named_map("panel", c("s2", "s1"), c("s2", "s1"))),
    list(feature_rows("study", "panel", "fa", "A"))
  ), display_names = c(panel = "proteomics"))
  ops <- baseline_ops()
  ops[[4L]]$method <- "plus_one"
  dir <- tempfile("a-t03-custom-")
  dir.create(dir)
  fitted <- prepare_fit(assembled, fit_spec("panel", "custom_panel", dir, c("s1", "s2"), ops))
  testthat::expect_equal(as.numeric(analysis_of(fitted, "panel")["fa", c("s1", "s2")]), c(5, 4), tolerance = 1e-12)
  testthat::expect_identical(names(fitted$experiments), "panel")
  loaded <- triomics_read_state(fitted$state_refs$panel, dir)
  testthat::expect_identical(loaded$training_unit_ids, c("s1", "s2"))
  testthat::expect_false(identical(loaded$profile_id, "proteomics"))
  testthat::expect_identical(loaded$profile_id, "custom_panel")
})
