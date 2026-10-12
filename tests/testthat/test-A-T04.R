expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

sample_registry <- function(ids, group = NULL) {
  frame <- data.frame(sample_id = ids, stringsAsFactors = FALSE)
  if (!is.null(group)) frame$group <- group
  frame$sample_role <- rep("undeclared", length(ids))
  frame
}

feature_rows <- function(study_id, block_id, keys, scale = "linear") {
  n <- length(keys)
  data.frame(
    study_id = rep(study_id, n),
    block_id = rep(block_id, n),
    feature_key = keys,
    original_id = keys,
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

named_map <- function(block_id, sample_ids) {
  data.frame(
    block_id = rep(block_id, length(sample_ids)),
    assay_column = sample_ids,
    sample_id = sample_ids,
    source_column = sample_ids,
    stringsAsFactors = FALSE
  )
}

manual_study <- function(registry, values, mask) {
  list(
    study_id = "study",
    sample_registry = registry,
    feature_registry = feature_rows("study", "alpha", rownames(values), scale = "linear"),
    sample_maps = list(alpha = named_map("alpha", colnames(values))),
    assays = list(alpha = values),
    original_masks = list(alpha = mask),
    availability = data.frame(
      study_id = "study",
      block_id = "alpha",
      sample_id = registry$sample_id,
      presence = "present",
      cell_imputation_eligible = FALSE,
      stringsAsFactors = FALSE
    ),
    source_manifests = list(),
    selection = data.frame(),
    column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = list(alpha = character())
  )
}

observed_mask <- function(assay) {
  matrix("known_observed", nrow(assay), ncol(assay), dimnames = dimnames(assay))
}

analysis_of <- function(study) {
  SummarizedExperiment::assay(study$experiments$alpha, "analysis")
}

mask_of <- function(study) {
  SummarizedExperiment::assay(study$experiments$alpha, "original_mask")
}

new_dir <- function(prefix) {
  path <- tempfile(prefix)
  dir.create(path)
  path
}

fit_spec <- function(dir, training, operations, context = "ctx") {
  list(
    preparation_context_id = context,
    training_unit_ids = training,
    seed = 7L,
    state_dir = dir,
    blocks = list(list(
      block_id = "alpha",
      profile_id = "proteomics",
      operations = operations
    ))
  )
}

assemble_block <- function(values, mask = NULL, scale = "linear", groups = NULL,
                           registry_ids = colnames(values), display = "metabolomics") {
  if (is.null(mask)) mask <- observed_mask(values)
  registry <- sample_registry(registry_ids, group = groups)
  raw <- manual_study(registry, values, mask)
  raw$feature_registry <- feature_rows("study", "alpha", rownames(values), scale = scale)
  triomics_assemble_study(raw, display_names = c(alpha = display))
}

register_proteomics <- function() {
  triomics_reset_profiles()
  triomics_register_proteomics_profile()
}

vector_at <- function(study, feature, samples) {
  as.numeric(analysis_of(study)[feature, samples])
}

snapshot <- function(study) {
  mat <- analysis_of(study)
  matrix(as.numeric(mat), nrow = nrow(mat), dimnames = dimnames(mat))
}

flow_row <- function(study, key) {
  flow <- study$feature_flow
  flow[as.character(flow$feature_key) == key, , drop = FALSE]
}

count_table <- function() {
  table <- data.frame(
    feature_key = c("f3", "f1", "f2"),
    peptide_count = c(8, 10, 3),
    psm_count = c(16, 21, 5),
    acquisition = c("acq-f3", "acq-f1", "acq-f2"),
    provenance = c("prov-f3", "prov-f1", "prov-f2"),
    stringsAsFactors = FALSE
  )
  rownames(table) <- c("f1", "f2", "f3")
  table
}

count_rows <- function(state) {
  rows <- state$learned_parameters$count_metadata
  lapply(rows, function(row) {
    norm <- function(value) if (is.null(value) || !length(value)) NA_real_ else as.numeric(value)
    list(
      feature_key = as.character(row$feature_key),
      peptide_count = norm(row$peptide_count),
      psm_count = norm(row$psm_count),
      acquisition = as.character(row$acquisition),
      provenance = as.character(row$provenance)
    )
  })
}

vsn_pin <- function() {
  list(
    status = "pinned",
    method_id = "vsn",
    package = "vsn",
    version = "3.82.0",
    native_fit = "vsn2",
    native_apply = "predict",
    fit_apply = TRUE,
    contexts = "proteomics"
  )
}

minprob_pin <- function(fit_apply = FALSE, native_apply = NULL) {
  list(
    status = "pinned",
    method_id = "minprob",
    package = "imputeLCMD",
    version = "2.1",
    native_fit = "impute.MinProb",
    native_apply = native_apply,
    fit_apply = fit_apply,
    contexts = "proteomics"
  )
}

testthat::test_that("A-T04-AC1 log2 and none match hand calculations and refuse a second log", {
  register_proteomics()
  values <- matrix(c(1, 2, 4, 8), nrow = 1, dimnames = list("f1", c("s1", "s2", "s3", "s4")))
  study <- assemble_block(values)
  none <- prepare_fit(study, fit_spec(
    new_dir("a-t04-none-"), c("s1", "s2", "s3", "s4"),
    triomics_proteomics_operations(transform = "none"), "none"
  ))
  testthat::expect_equal(vector_at(none, "f1", c("s1", "s2", "s3", "s4")), c(1, 2, 4, 8), tolerance = 1e-8)

  logged <- prepare_fit(study, fit_spec(
    new_dir("a-t04-log-"), c("s1", "s2", "s3", "s4"),
    triomics_proteomics_operations(transform = "log2"), "log"
  ))
  testthat::expect_equal(vector_at(logged, "f1", c("s1", "s2", "s3", "s4")), c(0, 1, 2, 3), tolerance = 1e-8)
  methods <- vapply(logged$operation_ledger$alpha, function(op) op$method, character(1))
  testthat::expect_identical(methods[[4L]], "log2")

  zeros <- matrix(c(0, 4), nrow = 1, dimnames = list("f1", c("s1", "s2")))
  zero_study <- assemble_block(zeros)
  before_zero <- snapshot(zero_study)
  expect_code(
    prepare_fit(zero_study, fit_spec(
      new_dir("a-t04-zero-"), c("s1", "s2"),
      triomics_proteomics_operations(transform = "log2"), "zero"
    )),
    "A_SCALE_ZERO"
  )
  testthat::expect_equal(snapshot(zero_study), before_zero, tolerance = 1e-8)
  replaced <- prepare_fit(zero_study, fit_spec(
    new_dir("a-t04-zrep-"), c("s1", "s2"),
    triomics_proteomics_operations(
      transform = "log2", zero_policy = "declared_replacement", zero_value = 0.5
    ),
    "zrep"
  ))
  testthat::expect_equal(vector_at(replaced, "f1", c("s1", "s2")), c(-1, 2), tolerance = 1e-8)

  negative <- assemble_block(matrix(c(-2, 4), nrow = 1, dimnames = list("f1", c("s1", "s2"))))
  before_negative <- snapshot(negative)
  expect_code(
    prepare_fit(negative, fit_spec(
      new_dir("a-t04-neg-"), c("s1", "s2"),
      triomics_proteomics_operations(transform = "log2"), "neg"
    )),
    "A_SCALE_NONPOSITIVE"
  )
  testthat::expect_equal(snapshot(negative), before_negative, tolerance = 1e-8)

  already <- assemble_block(
    matrix(c(3, 1), nrow = 1, dimnames = list("f1", c("s1", "s2"))),
    scale = "log2"
  )
  before_already <- snapshot(already)
  err <- expect_code(
    prepare_fit(already, fit_spec(
      new_dir("a-t04-double-"), c("s1", "s2"),
      triomics_proteomics_operations(transform = "log2"), "double"
    )),
    "A_SCALE_DOUBLE_LOG"
  )
  testthat::expect_match(conditionMessage(err), "log2", fixed = TRUE)
  testthat::expect_equal(snapshot(already), before_already, tolerance = 1e-8)
  kept <- prepare_fit(assemble_block(
    matrix(c(3, 1), nrow = 1, dimnames = list("f1", c("s1", "s2"))),
    scale = "log2"
  ), fit_spec(
    new_dir("a-t04-keep-"), c("s1", "s2"),
    triomics_proteomics_operations(transform = "none"), "keep"
  ))
  testthat::expect_equal(vector_at(kept, "f1", c("s1", "s2")), c(3, 1), tolerance = 1e-8)

  again <- prepare_fit(study, fit_spec(
    new_dir("a-t04-log2-"), c("s1", "s2", "s3", "s4"),
    triomics_proteomics_operations(transform = "log2"), "log2"
  ))
  expect_code(
    prepare_fit(again, fit_spec(
      new_dir("a-t04-again-"), c("s1", "s2", "s3", "s4"),
      triomics_proteomics_operations(transform = "log2"), "again"
    )),
    "A_SCALE_DOUBLE_LOG"
  )
  testthat::expect_equal(vector_at(again, "f1", "s4"), 3, tolerance = 1e-8)
})

testthat::test_that("A-T04-AC1 median-shift matches the frozen training-median hand calculation", {
  register_proteomics()
  # Sample medians are 3, 6 and 5. Training median of s1 and s2 is 4.5.
  values <- matrix(
    c(1, 3, 5, 4, 6, 8, 10, 0, 5),
    nrow = 3,
    dimnames = list(c("f1", "f2", "f3"), c("s1", "s2", "s3"))
  )
  study <- assemble_block(
    values, scale = "log2", registry_ids = c("s3", "s1", "s2"),
    groups = c("treatment", "control", "control"), display = "metabolomics"
  )
  ops <- triomics_proteomics_operations(
    transform = "none", normalise = "median_shift", reference = "training_median"
  )
  testthat::expect_identical(triomics_proteomics_predictive_eligibility(ops[[5L]]), "fit_apply")
  testthat::expect_false(any(vapply(ops, function(op) identical(op$apply, "unavailable"), logical(1L))))
  dir <- new_dir("a-t04-med-")
  fitted <- prepare_fit(study, fit_spec(dir, c("s1", "s2"), ops, "med"))
  testthat::expect_equal(vector_at(fitted, "f1", c("s1", "s2", "s3")), c(2.5, 2.5, 9.5), tolerance = 1e-8)
  testthat::expect_equal(vector_at(fitted, "f2", c("s1", "s2", "s3")), c(4.5, 4.5, -0.5), tolerance = 1e-8)
  testthat::expect_equal(vector_at(fitted, "f3", c("s1", "s2", "s3")), c(6.5, 6.5, 4.5), tolerance = 1e-8)
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  testthat::expect_identical(state$profile_id, "proteomics")
  testthat::expect_equal(as.numeric(state$learned_parameters$median_shift$training_median), 4.5, tolerance = 1e-8)
  testthat::expect_false(grepl(
    "treatment|control|outcome",
    paste(names(unlist(state$learned_parameters)), collapse = " ")
  ))

  flip_dir <- new_dir("a-t04-flip-")
  flipped <- assemble_block(
    values, scale = "log2", registry_ids = c("s3", "s1", "s2"),
    groups = c("control", "treatment", "treatment")
  )
  flipped_fit <- prepare_fit(flipped, fit_spec(flip_dir, c("s1", "s2"), ops, "flip"))
  testthat::expect_equal(vector_at(flipped_fit, "f1", "s1"), 2.5, tolerance = 1e-8)
  flip_state <- triomics_read_state(flipped_fit$state_refs$alpha, flip_dir)
  testthat::expect_equal(as.numeric(flip_state$learned_parameters$median_shift$training_median), 4.5, tolerance = 1e-8)

  mutated_values <- values
  mutated_values[, "s1"] <- 10
  mutated <- assemble_block(mutated_values, scale = "log2", registry_ids = c("s3", "s1", "s2"))
  applied <- prepare_apply(mutated, fit_spec(dir, NULL, ops, "apply"), fitted$state_refs)
  testthat::expect_identical(applied$predictive_status[["alpha"]], "apply")
  testthat::expect_identical(applied$state_refs$alpha$state_hash, fitted$state_refs$alpha$state_hash)
  testthat::expect_equal(vector_at(applied, "f1", c("s1", "s2", "s3")), c(4.5, 2.5, 9.5), tolerance = 1e-8)
  testthat::expect_equal(vector_at(applied, "f2", c("s1", "s2", "s3")), c(4.5, 4.5, -0.5), tolerance = 1e-8)
  testthat::expect_equal(vector_at(applied, "f3", c("s1", "s2", "s3")), c(4.5, 6.5, 4.5), tolerance = 1e-8)
  reread <- triomics_read_state(applied$state_refs$alpha, dir)
  testthat::expect_equal(as.numeric(reread$learned_parameters$median_shift$training_median), 4.5, tolerance = 1e-8)

  linear <- assemble_block(values, scale = "linear", registry_ids = c("s3", "s1", "s2"))
  before_linear <- snapshot(linear)
  expect_code(
    prepare_fit(linear, fit_spec(new_dir("a-t04-lin-"), c("s1", "s2"), ops, "linear")),
    "A_SCALE_INPUT"
  )
  testthat::expect_equal(snapshot(linear), before_linear, tolerance = 1e-8)

  missing_ref <- triomics_proteomics_operations(transform = "none", normalise = "median_shift")
  expect_code(
    prepare_fit(study, fit_spec(new_dir("a-t04-noref-"), c("s1", "s2"), missing_ref, "noref")),
    "A_CONFIG_METHOD"
  )
  supplied <- triomics_proteomics_operations(
    transform = "none", normalise = "median_shift", reference = "supplied"
  )
  expect_code(
    prepare_fit(study, fit_spec(new_dir("a-t04-supplied-"), c("s1", "s2"), supplied, "supplied")),
    "A_CONFIG_METHOD"
  )

  partial <- matrix(
    c(1, NA, 5, 3, 3, 3),
    nrow = 3,
    dimnames = list(c("f1", "f2", "f3"), c("s1", "s2"))
  )
  partial_mask <- observed_mask(partial)
  partial_mask["f2", "s1"] <- "known_missing"
  partial_study <- assemble_block(partial, mask = partial_mask, scale = "log2")
  partial_fit <- prepare_fit(partial_study, fit_spec(
    new_dir("a-t04-partial-"), c("s1", "s2"), ops, "partial"
  ))
  testthat::expect_equal(vector_at(partial_fit, "f1", "s1"), 1, tolerance = 1e-8)
  testthat::expect_true(is.na(analysis_of(partial_fit)["f2", "s1"]))
  testthat::expect_equal(vector_at(partial_fit, "f3", "s1"), 5, tolerance = 1e-8)

  empty <- matrix(c(NA_real_, NA_real_, 1, 2), nrow = 2, dimnames = list(c("f1", "f2"), c("s1", "s2")))
  empty_mask <- observed_mask(empty)
  empty_mask[, "s1"] <- "known_missing"
  empty_study <- assemble_block(empty, mask = empty_mask, scale = "log2")
  expect_code(
    prepare_fit(empty_study, fit_spec(new_dir("a-t04-empty-"), c("s1", "s2"), ops, "empty")),
    "A_SCALE_ALL_MISSING"
  )

  linear_combo <- matrix(
    c(4, 16, 4, 16, 16, 64),
    nrow = 3,
    dimnames = list(c("f1", "f2", "f3"), c("s1", "s2"))
  )
  combo_ops <- triomics_proteomics_operations(
    transform = "log2", normalise = "median_shift", reference = "training_median"
  )
  combo <- prepare_fit(assemble_block(linear_combo), fit_spec(
    new_dir("a-t04-combo-"), c("s1", "s2"), combo_ops, "combo"
  ))
  testthat::expect_equal(vector_at(combo, "f1", c("s1", "s2")), c(3, 3), tolerance = 1e-8)
  testthat::expect_equal(vector_at(combo, "f2", c("s1", "s2")), c(5, 3), tolerance = 1e-8)
  testthat::expect_equal(vector_at(combo, "f3", c("s1", "s2")), c(3, 5), tolerance = 1e-8)
  stages <- vapply(combo$operation_ledger$alpha, function(op) op$stage, character(1))
  testthat::expect_true(which(stages == "transform") < which(stages == "normalise"))
})

testthat::test_that("A-T04-AC2 filtering uses original observations and runs before imputation", {
  register_proteomics()
  values <- matrix(
    c(2, NA, 8, 100, NA, NA, 1, 50, 50),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("keep", "sparse", "bait"), c("s1", "s2", "s3"))
  )
  mask <- observed_mask(values)
  mask["keep", "s2"] <- "known_missing"
  mask["sparse", c("s2", "s3")] <- "known_missing"
  mask["bait", c("s2", "s3")] <- "known_imputed"
  study <- assemble_block(values, mask = mask)
  # A filled known-missing cell must not count as an original observation.
  SummarizedExperiment::assay(study$experiments$alpha, "analysis")["sparse", "s2"] <- 99
  ops <- triomics_proteomics_operations(
    filter_method = "prevalence", min_observed = 2L, impute = "training_median",
    missingness_assumption = "unknown"
  )
  fitted <- prepare_fit(study, fit_spec(new_dir("a-t04-filt-"), c("s1", "s2", "s3"), ops, "filt"))
  testthat::expect_identical(rownames(analysis_of(fitted)), "keep")
  testthat::expect_equal(vector_at(fitted, "keep", c("s1", "s2", "s3")), c(2, 5, 8), tolerance = 1e-8)
  testthat::expect_identical(mask_of(fitted)["keep", "s2"], "known_imputed")
  testthat::expect_identical(mask_of(fitted)["keep", "s1"], "known_observed")
  testthat::expect_equal(flow_row(fitted, "sparse")$n_observed[[1L]], 1)
  testthat::expect_false(isTRUE(flow_row(fitted, "sparse")$retained[[1L]]))
  testthat::expect_equal(flow_row(fitted, "bait")$n_observed[[1L]], 1)
  testthat::expect_false(isTRUE(flow_row(fitted, "bait")$retained[[1L]]))
  testthat::expect_equal(flow_row(fitted, "keep")$n_observed[[1L]], 2)
  stages <- vapply(fitted$operation_ledger$alpha, function(op) op$stage, character(1))
  testthat::expect_true(which(stages == "filter") < which(stages == "impute"))
  testthat::expect_identical(
    vapply(fitted$operation_ledger$alpha, function(op) op$method, character(1))[[6L]],
    "training_median"
  )

  ghost_values <- matrix(
    c(1, 3, NA, 4, 6, 8),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("ghost", "other"), c("s1", "s2", "s3"))
  )
  ghost_mask <- observed_mask(ghost_values)
  ghost_mask["ghost", "s3"] <- "unknown"
  ghost <- assemble_block(ghost_values, mask = ghost_mask)
  ghost_ops <- triomics_proteomics_operations(
    impute = "training_median", missingness_assumption = "unknown"
  )
  ghost_fit <- prepare_fit(ghost, fit_spec(new_dir("a-t04-ghost-"), c("s1", "s2", "s3"), ghost_ops, "ghost"))
  testthat::expect_true(is.na(analysis_of(ghost_fit)["ghost", "s3"]))
  testthat::expect_identical(mask_of(ghost_fit)["ghost", "s3"], "unknown")
  testthat::expect_equal(vector_at(ghost_fit, "other", c("s1", "s2", "s3")), c(4, 6, 8), tolerance = 1e-8)
  testthat::expect_identical(
    triomics_proteomics_predictive_eligibility(ghost_ops[[6L]]),
    "fit_apply"
  )

  censor <- assemble_block(ghost_values, mask = ghost_mask)
  before <- snapshot(censor)
  censor_ops <- triomics_proteomics_operations(
    impute = "minprob", missingness_assumption = "unknown",
    qualification = minprob_pin()
  )
  err <- expect_code(
    prepare_fit(censor, fit_spec(new_dir("a-t04-censor-"), c("s1", "s2", "s3"), censor_ops, "censor")),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(err), "Unknown missingness does not select a censoring model.", fixed = TRUE)
  testthat::expect_match(conditionMessage(err), "missingness_does_not_select_censoring", fixed = TRUE)
  testthat::expect_equal(snapshot(censor), before, tolerance = 1e-8)
  testthat::expect_identical(triomics_proteomics_lane_decision(censor_ops[[6L]])$status, "unavailable")
  testthat::expect_false(triomics_proteomics_predictive_eligibility(censor_ops[[6L]]) %in% c("apply", "eligible", "fit_apply"))

  mcar_ops <- triomics_proteomics_operations(
    impute = "minprob", missingness_assumption = "mcar", missingness_evidence = "not left censoring"
  )
  mcar_err <- expect_code(
    prepare_fit(assemble_block(ghost_values, mask = ghost_mask), fit_spec(
      new_dir("a-t04-mcar-"), c("s1", "s2", "s3"), mcar_ops, "mcar"
    )),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(mcar_err), "does not select a censoring model", fixed = TRUE)
})

testthat::test_that("A-T04-AC2 group filters stay on training labels and declared contaminants", {
  register_proteomics()
  ids <- c("s1", "s2", "s3", "s4", "s5")
  groups <- c("control", "control", "treatment", "treatment", "treatment")
  features <- c("leak", "any_only", "both", "held_only")
  values <- matrix(NA_real_, length(features), length(ids), dimnames = list(features, ids))
  values["leak", c("s1", "s5")] <- c(2, 9)
  values["any_only", "s1"] <- 4
  values["both", c("s1", "s3")] <- c(1, 3)
  values["held_only", "s5"] <- 7
  mask <- observed_mask(values)
  mask[is.na(values)] <- "known_missing"
  study <- assemble_block(values, mask = mask, groups = groups)
  each <- triomics_proteomics_operations(
    filter_method = "prevalence", min_observed = 1L, scope = "stratum", stratum = "group",
    selection_policy = "training_min_in_each_group",
    selection_justification = "Declared training groups only."
  )
  each_fit <- prepare_fit(study, fit_spec(new_dir("a-t04-each-"), c("s1", "s2", "s3", "s4"), each, "each"))
  testthat::expect_identical(rownames(analysis_of(each_fit)), "both")

  any <- triomics_proteomics_operations(
    filter_method = "prevalence", min_observed = 1L, scope = "stratum", stratum = "group",
    selection_policy = "training_min_in_any_group",
    selection_justification = "Declared training groups only."
  )
  any_fit <- prepare_fit(study, fit_spec(new_dir("a-t04-any-"), c("s1", "s2", "s3", "s4"), any, "any"))
  testthat::expect_identical(rownames(analysis_of(any_fit)), c("leak", "any_only", "both"))

  flipped <- assemble_block(values, mask = mask, groups = c("control", "control", "treatment", "treatment", "control"))
  flipped_fit <- prepare_fit(flipped, fit_spec(new_dir("a-t04-lab-"), c("s1", "s2", "s3", "s4"), each, "lab"))
  testthat::expect_identical(rownames(analysis_of(flipped_fit)), "both")

  missing_policy <- triomics_proteomics_operations(
    filter_method = "prevalence", min_observed = 1L, scope = "stratum", stratum = "group"
  )
  expect_code(
    prepare_fit(study, fit_spec(new_dir("a-t04-nopol-"), c("s1", "s2", "s3", "s4"), missing_policy, "nopol")),
    "A_CONFIG_OPERATION"
  )

  rows <- c("cont", "qc", "CON_trypsin", "good", "sparse")
  qc_values <- matrix(5, length(rows), 3, dimnames = list(rows, c("s1", "s2", "s3")))
  qc_values["sparse", c("s2", "s3")] <- NA_real_
  qc_mask <- observed_mask(qc_values)
  qc_mask["sparse", c("s2", "s3")] <- "known_missing"
  qc_study <- assemble_block(qc_values, mask = qc_mask)
  row_data <- SummarizedExperiment::rowData(qc_study$experiments$alpha)
  row_data$declared_exclusion <- NA_character_
  row_data["cont", "declared_exclusion"] <- "contaminant"
  row_data["qc", "declared_exclusion"] <- "qc_failure"
  SummarizedExperiment::rowData(qc_study$experiments$alpha) <- row_data
  qc_ops <- triomics_proteomics_operations(filter_method = "prevalence", min_observed = 2L)
  qc_fit <- prepare_fit(qc_study, fit_spec(new_dir("a-t04-qc-"), c("s1", "s2", "s3"), qc_ops, "qc"))
  testthat::expect_identical(rownames(analysis_of(qc_fit)), c("CON_trypsin", "good"))
  testthat::expect_identical(as.character(flow_row(qc_fit, "cont")$reason[[1L]]), "declared_contaminant")
  testthat::expect_identical(as.character(flow_row(qc_fit, "qc")$reason[[1L]]), "declared_qc_failure")
  testthat::expect_equal(flow_row(qc_fit, "cont")$n_observed[[1L]], 3)
  testthat::expect_false(isTRUE(flow_row(qc_fit, "sparse")$retained[[1L]]))

  bad <- assemble_block(qc_values, mask = qc_mask)
  bad_row <- SummarizedExperiment::rowData(bad$experiments$alpha)
  bad_row$declared_exclusion <- NA_character_
  bad_row["good", "declared_exclusion"] <- "maybe"
  SummarizedExperiment::rowData(bad$experiments$alpha) <- bad_row
  err <- expect_code(
    prepare_fit(bad, fit_spec(new_dir("a-t04-maybe-"), c("s1", "s2", "s3"), qc_ops, "maybe")),
    "A_CONFIG_OPERATION"
  )
  testthat::expect_match(conditionMessage(err), "declared_exclusion", fixed = TRUE)
})

testthat::test_that("A-T04-AC3 qualified lanes stay unavailable and do not fall back", {
  register_proteomics()
  values <- matrix(c(2, NA, 8, 4), nrow = 1, dimnames = list("f1", c("s1", "s2", "s3", "s4")))
  mask <- observed_mask(values)
  mask["f1", "s2"] <- "known_missing"
  values["f1", "s2"] <- NA_real_
  study <- assemble_block(values, mask = mask)
  before <- snapshot(study)
  training_median <- 4
  profile <- triomics_proteomics_profile()

  honest <- triomics_proteomics_operations(
    impute = "minprob",
    missingness_assumption = "mnar_left_censored",
    missingness_evidence = "Synthetic left-censoring fixture.",
    qualification = minprob_pin()
  )
  decision <- triomics_proteomics_lane_decision(honest[[6L]])
  testthat::expect_identical(decision$status, "unavailable")
  testthat::expect_identical(decision$reason, "inductive_fit_apply_not_supported")
  testthat::expect_identical(triomics_proteomics_predictive_eligibility(honest[[6L]]), "unavailable")
  testthat::expect_true(all(vapply(honest, function(op) identical(op$apply, "unavailable"), logical(1L))))
  err <- expect_code(
    profile$impute(study$experiments$alpha, list(
      mode = "fit", training_unit_ids = c("s1", "s3", "s4"), block_id = "alpha", operation = honest[[6L]]
    )),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(err), "unavailable", fixed = TRUE)
  testthat::expect_match(conditionMessage(err), "inductive_fit_apply_not_supported", fixed = TRUE)
  testthat::expect_true(is.na(SummarizedExperiment::assay(study$experiments$alpha, "analysis")["f1", "s2"]))
  testthat::expect_false(isTRUE(all.equal(
    SummarizedExperiment::assay(study$experiments$alpha, "analysis")["f1", "s2"],
    training_median
  )))

  missing_q <- triomics_proteomics_operations(
    impute = "minprob",
    missingness_assumption = "mnar_left_censored",
    missingness_evidence = "Synthetic left-censoring fixture."
  )
  testthat::expect_identical(triomics_proteomics_lane_decision(missing_q[[6L]])$reason, "qualification_missing")

  false_pin <- triomics_proteomics_operations(
    impute = "minprob",
    missingness_assumption = "mnar_left_censored",
    missingness_evidence = "Synthetic left-censoring fixture.",
    qualification = minprob_pin(fit_apply = TRUE, native_apply = "impute.MinProb")
  )
  testthat::expect_identical(triomics_proteomics_lane_decision(false_pin[[6L]])$reason, "qualification_not_pinned")

  qrilc <- triomics_proteomics_operations(
    impute = "qrilc",
    missingness_assumption = "mnar_left_censored",
    missingness_evidence = "Synthetic left-censoring fixture.",
    qualification = list(
      status = "pinned", method_id = "qrilc", package = "imputeLCMD", version = "2.1",
      native_fit = "impute.QRILC", native_apply = NULL, fit_apply = FALSE, contexts = "proteomics"
    )
  )
  testthat::expect_identical(triomics_proteomics_lane_decision(qrilc[[6L]])$reason, "inductive_fit_apply_not_supported")
  expect_code(
    prepare_fit(study, fit_spec(new_dir("a-t04-qrilc-"), c("s1", "s3", "s4"), qrilc, "qrilc")),
    "A_CONFIG_METHOD"
  )
  testthat::expect_equal(snapshot(study), before, tolerance = 1e-8)

  vsn_ops <- triomics_proteomics_operations(
    transform = "none", normalise = "vsn", qualification = vsn_pin()
  )
  vsn_decision <- triomics_proteomics_lane_decision(vsn_ops[[5L]])
  testthat::expect_identical(vsn_decision$status, "unavailable")
  testthat::expect_identical(vsn_decision$reason, "native_fit_apply_not_runtime_qualified")
  vsn_err <- expect_code(
    profile$normalise(study$experiments$alpha, list(
      mode = "fit", training_unit_ids = c("s1", "s3", "s4"), block_id = "alpha", operation = vsn_ops[[5L]]
    )),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(vsn_err), "unavailable", fixed = TRUE)
  testthat::expect_match(conditionMessage(vsn_err), "native_fit_apply_not_runtime_qualified", fixed = TRUE)
  testthat::expect_equal(snapshot(study), before, tolerance = 1e-8)
  testthat::expect_identical(
    triomics_proteomics_lane_decision(triomics_proteomics_operations(transform = "none", normalise = "vsn")[[5L]])$reason,
    "qualification_missing"
  )

  double <- triomics_proteomics_operations(transform = "log2", normalise = "vsn", qualification = vsn_pin())
  expect_code(
    triomics_freeze_operations(list(block_id = "alpha", profile_id = "proteomics", operations = double)),
    "A_SCALE_DOUBLE_LOG"
  )

  for (method in c("knn", "rf")) {
    ops <- triomics_proteomics_operations(impute = method)
    testthat::expect_identical(triomics_proteomics_lane_decision(ops[[6L]])$status, "unavailable")
    testthat::expect_identical(triomics_proteomics_predictive_eligibility(ops[[6L]]), "unavailable")
  }
  pqn <- triomics_proteomics_operations(normalise = "pqn", reference = "supplied")
  pqn_err <- expect_code(
    profile$normalise(study$experiments$alpha, list(
      mode = "fit", training_unit_ids = c("s1", "s3", "s4"), block_id = "alpha", operation = pqn[[5L]]
    )),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(pqn_err), "not_proteomic_policy", fixed = TRUE)
  clr <- triomics_proteomics_operations(transform = "clr")
  expect_code(
    profile$transform(study$experiments$alpha, list(
      mode = "fit", block_id = "alpha", operation = clr[[4L]]
    )),
    "A_CONFIG_METHOD"
  )
  expect_code(
    profile$derive(study$experiments$alpha, list(
      mode = "fit", block_id = "alpha",
      operation = list(stage = "derive", method = "class_sum")
    )),
    "A_CONFIG_METHOD"
  )
  testthat::expect_equal(snapshot(study), before, tolerance = 1e-8)
})

testthat::test_that("A-T04-AC4 peptide and PSM counts stay keyed and non-inductive prep is not predictive", {
  register_proteomics()
  values <- matrix(
    c(2, 4, 6, 8, 10, 12, 14, 16, 18),
    nrow = 3,
    dimnames = list(c("f1", "f2", "f3"), c("s1", "s2", "s3"))
  )
  counts <- count_table()
  ops <- triomics_proteomics_operations(count_metadata = counts)
  dir <- new_dir("a-t04-count-")
  fitted <- prepare_fit(assemble_block(values), fit_spec(dir, c("s1", "s2"), ops, "count"))
  rows <- count_rows(triomics_read_state(fitted$state_refs$alpha, dir))
  testthat::expect_identical(vapply(rows, function(row) row$feature_key, character(1)), c("f1", "f2", "f3"))
  testthat::expect_equal(rows[[1L]]$peptide_count, 10)
  testthat::expect_equal(rows[[1L]]$psm_count, 21)
  testthat::expect_identical(rows[[1L]]$acquisition, "acq-f1")
  testthat::expect_identical(rows[[2L]]$provenance, "prov-f2")
  testthat::expect_identical(rows[[3L]]$feature_key, "f3")
  row_data <- as.data.frame(SummarizedExperiment::rowData(fitted$experiments$alpha))
  testthat::expect_equal(row_data["f1", "peptide_count"], 10)
  testthat::expect_equal(row_data["f3", "peptide_count"], 8)
  testthat::expect_false(any(row_data$peptide_count == rowSums(values)))

  reordered <- counts[c(3, 1, 2), , drop = FALSE]
  dir2 <- new_dir("a-t04-count2-")
  fitted2 <- prepare_fit(
    assemble_block(values),
    fit_spec(dir2, c("s1", "s2"), triomics_proteomics_operations(count_metadata = reordered), "count2")
  )
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  state2 <- triomics_read_state(fitted2$state_refs$alpha, dir2)
  testthat::expect_identical(state$learned_parameters$count_hash, state2$learned_parameters$count_hash)

  bare_dir <- new_dir("a-t04-bare-")
  bare_fit <- prepare_fit(
    assemble_block(values),
    fit_spec(bare_dir, c("s1", "s2"), triomics_proteomics_operations(), "bare")
  )
  bare_state <- triomics_read_state(bare_fit$state_refs$alpha, bare_dir)
  testthat::expect_null(bare_state$learned_parameters$count_metadata)
  testthat::expect_false("peptide_count" %in% names(SummarizedExperiment::rowData(bare_fit$experiments$alpha)))

  dropped_values <- values
  dropped_values["f2", ] <- NA_real_
  dropped_mask <- observed_mask(dropped_values)
  dropped_mask["f2", ] <- "known_missing"
  drop_dir <- new_dir("a-t04-dropcount-")
  dropped <- prepare_fit(
    assemble_block(dropped_values, mask = dropped_mask),
    fit_spec(drop_dir, c("s1", "s2"), triomics_proteomics_operations(
      filter_method = "prevalence", min_observed = 1L, count_metadata = counts
    ), "dropcount")
  )
  dropped_rows <- count_rows(triomics_read_state(dropped$state_refs$alpha, drop_dir))
  testthat::expect_identical(vapply(dropped_rows, function(row) row$feature_key, character(1)), c("f1", "f3"))
  testthat::expect_equal(dropped_rows[[1L]]$peptide_count, 10)
  testthat::expect_equal(dropped_rows[[2L]]$peptide_count, 8)
  dropped_row_data <- as.data.frame(SummarizedExperiment::rowData(dropped$experiments$alpha))
  testthat::expect_equal(dropped_row_data["f1", "peptide_count"], 10)
  testthat::expect_equal(dropped_row_data["f3", "psm_count"], 16)

  apply_values <- matrix(
    c(14, 16, 18, 2, 4, 6, 8, 10, 12),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("f3", "f1", "f2"), c("s1", "s2", "s3"))
  )
  apply_study <- assemble_block(apply_values)
  applied <- prepare_apply(apply_study, fit_spec(dir, NULL, triomics_proteomics_operations(), "count-apply"), fitted$state_refs)
  testthat::expect_identical(rownames(analysis_of(applied)), c("f1", "f2", "f3"))
  applied_rows <- as.data.frame(SummarizedExperiment::rowData(applied$experiments$alpha))
  testthat::expect_equal(applied_rows["f1", "peptide_count"], 10)
  testthat::expect_equal(applied_rows["f2", "peptide_count"], 3)
  testthat::expect_equal(applied_rows["f3", "psm_count"], 16)
  testthat::expect_equal(as.numeric(analysis_of(applied)["f1", "s1"]), 2)

  tampered <- assemble_block(values)
  tampered_row <- SummarizedExperiment::rowData(tampered$experiments$alpha)
  tampered_row$peptide_count <- c(99, 3, 8)
  tampered_row$psm_count <- c(21, 5, 16)
  tampered_row$count_acquisition <- c("acq-f1", "acq-f2", "acq-f3")
  tampered_row$count_provenance <- c("prov-f1", "prov-f2", "prov-f3")
  SummarizedExperiment::rowData(tampered$experiments$alpha) <- tampered_row
  expect_code(
    prepare_apply(tampered, fit_spec(dir, NULL, triomics_proteomics_operations(), "tamper"), fitted$state_refs),
    "A_SOURCE_CONTRACT"
  )

  positional <- data.frame(peptide_count = c(10, 3, 8), stringsAsFactors = FALSE)
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(
      new_dir("a-t04-pos-"), c("s1", "s2"),
      triomics_proteomics_operations(count_metadata = positional), "pos"
    )),
    "A_SOURCE_CONTRACT"
  )
  zero_counts <- counts
  zero_counts$peptide_count[zero_counts$feature_key == "f1"] <- 0
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(
      new_dir("a-t04-zeroc-"), c("s1", "s2"),
      triomics_proteomics_operations(count_metadata = zero_counts), "zeroc"
    )),
    "A_SOURCE_CONTRACT"
  )
  blank <- counts
  blank$provenance[blank$feature_key == "f2"] <- ""
  err <- expect_code(
    prepare_fit(assemble_block(values), fit_spec(
      new_dir("a-t04-blank-"), c("s1", "s2"),
      triomics_proteomics_operations(count_metadata = blank), "blank"
    )),
    "A_SOURCE_CONTRACT"
  )
  testthat::expect_match(conditionMessage(err), "provenance", fixed = TRUE)
  extra <- counts
  extra <- rbind(extra, data.frame(
    feature_key = "f9", peptide_count = 4, psm_count = 4,
    acquisition = "acq-f9", provenance = "prov-f9", stringsAsFactors = FALSE
  ))
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(
      new_dir("a-t04-extra-"), c("s1", "s2"),
      triomics_proteomics_operations(count_metadata = extra), "extra"
    )),
    "A_SOURCE_CONTRACT"
  )
  fraction <- counts
  fraction$peptide_count[fraction$feature_key == "f1"] <- 2.5
  frac_err <- expect_code(
    prepare_fit(assemble_block(values), fit_spec(
      new_dir("a-t04-frac-"), c("s1", "s2"),
      triomics_proteomics_operations(count_metadata = fraction), "frac"
    )),
    "A_SOURCE_CONTRACT"
  )
  testthat::expect_match(conditionMessage(frac_err), "whole", fixed = TRUE)

  testthat::expect_identical(
    triomics_proteomics_predictive_eligibility(list(stage = "impute", method = "minprob")),
    "unavailable"
  )
  testthat::expect_identical(
    triomics_proteomics_predictive_eligibility(list(stage = "impute", method = "rf")),
    "unavailable"
  )
  testthat::expect_false(identical(
    triomics_proteomics_predictive_eligibility(list(stage = "normalise", method = "vsn")),
    "fit_apply"
  ))
})
