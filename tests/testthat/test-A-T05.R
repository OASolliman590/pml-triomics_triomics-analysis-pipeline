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
      profile_id = "metabolomics",
      operations = operations
    ))
  )
}

assemble_block <- function(values, mask = NULL, scale = "linear", groups = NULL,
                           registry_ids = colnames(values), display = "custom panel") {
  if (is.null(mask)) mask <- observed_mask(values)
  registry <- sample_registry(registry_ids, group = groups)
  raw <- manual_study(registry, values, mask)
  raw$feature_registry <- feature_rows("study", "alpha", rownames(values), scale = scale)
  triomics_assemble_study(raw, display_names = c(alpha = display))
}

register_metabolomics <- function() {
  triomics_reset_profiles()
  triomics_register_metabolomics_profile()
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

write_reference <- function(named_values, path) {
  frame <- data.frame(
    feature_key = names(named_values),
    reference = as.numeric(named_values),
    stringsAsFactors = FALSE
  )
  utils::write.table(frame, path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  path
}

reference_named <- function(state) {
  records <- state$learned_parameters$pqn$reference_vector
  if (is.data.frame(records)) {
    keys <- as.character(records$feature_key)
    vals <- as.numeric(records$value)
  } else if (is.list(records) && !is.null(records$feature_key) && !is.list(records$feature_key)) {
    keys <- as.character(records$feature_key)
    vals <- as.numeric(records$value)
  } else {
    keys <- vapply(records, function(row) as.character(row$feature_key), character(1))
    vals <- vapply(records, function(row) as.numeric(row$value), numeric(1))
  }
  stats::setNames(vals, keys)
}

knn_pin <- function() {
  list(
    status = "pinned",
    method_id = "knn",
    package = "triomics",
    version = "0.1.0",
    native_fit = "triomics_metabolomics_knn_fit",
    native_apply = "triomics_metabolomics_knn_apply",
    fit_apply = TRUE,
    inductive = TRUE,
    distance = "rms_shared_observed",
    weights = "inverse_distance",
    donors = "training_samples_only",
    contexts = "metabolomics"
  )
}

rf_pin <- function() {
  list(
    status = "pinned",
    method_id = "rf",
    package = "triomics",
    version = "0.1.0",
    native_fit = "triomics_metabolomics_rf_fit",
    native_apply = "triomics_metabolomics_rf_apply",
    fit_apply = TRUE,
    inductive = TRUE,
    donors = "training_rows_observed_target",
    forest = "bootstrap_stumps",
    contexts = "metabolomics"
  )
}

pqn_then_log <- function(values, training) {
  reference <- apply(values[, training, drop = FALSE], 1, stats::median)
  out <- values
  for (j in seq_len(ncol(values))) out[, j] <- values[, j] / stats::median(values[, j] / reference)
  log2(out)
}

testthat::test_that("A-T05-AC1 PQN divides by the median positive linear training ratio before log2", {
  register_metabolomics()
  values <- matrix(
    c(20, 40, 5, 10, 40, 80),
    nrow = 2,
    dimnames = list(c("f1", "f2"), c("s1", "s2", "s3"))
  )
  study <- assemble_block(values, display = "metabolomics")
  ops <- triomics_metabolomics_operations(
    transform = "log2", normalise = "pqn", reference = "training_median"
  )
  testthat::expect_identical(triomics_metabolomics_predictive_eligibility(ops[[5L]]), "fit_apply")
  testthat::expect_false(any(vapply(ops, function(op) identical(op$apply, "unavailable"), logical(1L))))
  dir <- new_dir("a-t05-pqn-")
  fitted <- prepare_fit(study, fit_spec(dir, c("s1", "s2"), ops, "pqn"))
  expected <- log2(c(12.5, 25))
  testthat::expect_equal(vector_at(fitted, "f1", c("s1", "s2", "s3")), rep(expected[[1L]], 3), tolerance = 1e-8)
  testthat::expect_equal(vector_at(fitted, "f2", c("s1", "s2", "s3")), rep(expected[[2L]], 3), tolerance = 1e-8)
  wrong <- pqn_then_log(log2(values), c("s1", "s2"))
  got <- snapshot(fitted)
  testthat::expect_gt(max(abs(got - wrong)), 1e-4)
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  testthat::expect_identical(state$profile_id, "metabolomics")
  testthat::expect_equal(reference_named(state)[c("f1", "f2")], c(f1 = 12.5, f2 = 25), tolerance = 1e-8)
  testthat::expect_identical(state$learned_parameters$pqn$reference_scale, "linear")
  testthat::expect_identical(as.character(state$learned_parameters$pqn$numeric_order), c("pqn", "log2"))
  logged <- Filter(function(op) identical(op$method, "log2"), fitted$operation_ledger$alpha)[[1L]]
  testthat::expect_identical(logged$applied_after, "pqn")
  testthat::expect_identical(logged$output_scale, "log2")
  stages <- vapply(fitted$operation_ledger$alpha, function(op) op$stage, character(1))
  testthat::expect_true(which(stages == "transform") < which(stages == "normalise"))

  mutated_values <- values
  mutated_values[, "s3"] <- c(1, 1000)
  mutated <- assemble_block(mutated_values, display = "not a profile id")
  mutated_dir <- new_dir("a-t05-held-")
  mutated_fit <- prepare_fit(mutated, fit_spec(mutated_dir, c("s1", "s2"), ops, "held"))
  mutated_state <- triomics_read_state(mutated_fit$state_refs$alpha, mutated_dir)
  testthat::expect_identical(mutated_state$learned_parameters$pqn$reference_hash, state$learned_parameters$pqn$reference_hash)
  testthat::expect_equal(vector_at(mutated_fit, "f1", c("s1", "s2")), rep(expected[[1L]], 2), tolerance = 1e-8)
  testthat::expect_equal(reference_named(mutated_state)[c("f1", "f2")], c(f1 = 12.5, f2 = 25), tolerance = 1e-8)

  apply_values <- values
  apply_values[, "s1"] <- c(20, 80)
  applied <- prepare_apply(
    assemble_block(apply_values),
    fit_spec(dir, NULL, ops, "apply-ref"),
    fitted$state_refs
  )
  testthat::expect_identical(applied$state_refs$alpha$state_hash, fitted$state_refs$alpha$state_hash)
  testthat::expect_equal(vector_at(applied, "f1", "s1"), log2(25 / 3), tolerance = 1e-8)
  testthat::expect_equal(vector_at(applied, "f2", "s1"), log2(100 / 3), tolerance = 1e-8)
  testthat::expect_equal(vector_at(applied, "f2", "s2"), log2(25), tolerance = 1e-8)
  reread <- triomics_read_state(applied$state_refs$alpha, dir)
  testthat::expect_equal(reference_named(reread)[c("f1", "f2")], c(f1 = 12.5, f2 = 25), tolerance = 1e-8)

  logged_input <- assemble_block(log2(values), scale = "log2")
  before_log <- snapshot(logged_input)
  expect_code(
    prepare_fit(logged_input, fit_spec(
      new_dir("a-t05-logscale-"), c("s1", "s2"),
      triomics_metabolomics_operations(transform = "none", normalise = "pqn", reference = "training_median"),
      "logscale"
    )),
    "A_SCALE_INPUT"
  )
  testthat::expect_equal(snapshot(logged_input), before_log, tolerance = 1e-8)

  manual <- triomics_metabolomics_operations(
    transform = "log2", normalise = "pqn", reference = "training_median"
  )
  manual[[4L]]$defer_until_pqn <- FALSE
  manual[[5L]]$then_log2 <- FALSE
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-order-"), c("s1", "s2"), manual, "order")),
    "A_SCALE_INPUT"
  )

  only_log <- prepare_fit(assemble_block(values), fit_spec(
    new_dir("a-t05-onlylog-"), c("s1", "s2", "s3"),
    triomics_metabolomics_operations(transform = "log2"), "onlylog"
  ))
  testthat::expect_equal(vector_at(only_log, "f1", "s1"), log2(20), tolerance = 1e-8)
  expect_code(
    prepare_fit(only_log, fit_spec(
      new_dir("a-t05-double-"), c("s1", "s2", "s3"),
      triomics_metabolomics_operations(transform = "log2"), "double"
    )),
    "A_SCALE_DOUBLE_LOG"
  )
  shift <- triomics_metabolomics_operations(normalise = "median_shift", reference = "training_median")
  shift_err <- expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-shift-"), c("s1", "s2"), shift, "shift")),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(shift_err), "not_metabolomic_policy", fixed = TRUE)
  named <- fit_spec(new_dir("a-t05-name-"), c("s1", "s2"), ops, "name")
  named$blocks[[1L]]$profile_id <- "metabolomics display"
  name_err <- expect_code(
    prepare_fit(assemble_block(values, display = "metabolomics"), named),
    "A_CONFIG_PROFILE"
  )
  testthat::expect_match(conditionMessage(name_err), "metabolomics display", fixed = TRUE)
})

testthat::test_that("A-T05-AC1 supplied PQN reference is frozen and not reread on apply", {
  register_metabolomics()
  values <- matrix(
    c(20, 40, 5, 10, 8, 40),
    nrow = 2,
    dimnames = list(c("f1", "f2"), c("s1", "s2", "s3"))
  )
  study <- assemble_block(values)
  ref_path <- write_reference(c(f2 = 20, f1 = 10), file.path(new_dir("a-t05-ref-"), "reference.tsv"))
  ops <- triomics_metabolomics_operations(
    transform = "log2", normalise = "pqn", reference = "supplied", reference_file = ref_path
  )
  dir <- new_dir("a-t05-supplied-")
  fitted <- prepare_fit(study, fit_spec(dir, c("s1", "s2"), ops, "supplied"))
  testthat::expect_equal(vector_at(fitted, "f1", c("s1", "s2")), rep(log2(10), 2), tolerance = 1e-8)
  testthat::expect_equal(vector_at(fitted, "f2", c("s1", "s2")), rep(log2(20), 2), tolerance = 1e-8)
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  testthat::expect_equal(reference_named(state)[c("f1", "f2")], c(f1 = 10, f2 = 20), tolerance = 1e-8)
  factors <- state$learned_parameters$pqn$fit_dilution_factors
  factor_s3 <- if (is.data.frame(factors)) {
    as.numeric(factors$factor[as.character(factors$sample_id) == "s3"])
  } else if (!is.null(factors$sample_id) && !is.list(factors$sample_id)) {
    as.numeric(factors$factor[as.character(factors$sample_id) == "s3"])
  } else {
    as.numeric(Filter(function(row) identical(as.character(row$sample_id), "s3"), factors)[[1L]]$factor)
  }
  testthat::expect_equal(factor_s3, stats::median(c(8 / 10, 40 / 20)), tolerance = 1e-8)

  write_reference(c(f1 = 2, f2 = 4), ref_path)
  applied <- prepare_apply(assemble_block(values), fit_spec(dir, NULL, ops, "stale-file"), fitted$state_refs)
  testthat::expect_equal(vector_at(applied, "f1", "s1"), log2(10), tolerance = 1e-8)
  testthat::expect_equal(reference_named(triomics_read_state(applied$state_refs$alpha, dir))[["f1"]], 10, tolerance = 1e-8)

  permuted <- write_reference(c(f2 = 20, f1 = 10), file.path(new_dir("a-t05-perm-"), "reference.tsv"))
  perm_ops <- triomics_metabolomics_operations(
    transform = "none", normalise = "pqn", reference = "supplied", reference_file = permuted
  )
  perm <- prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-permfit-"), c("s1", "s2"), perm_ops, "perm"))
  testthat::expect_equal(vector_at(perm, "f1", "s2"), 10, tolerance = 1e-8)
  testthat::expect_equal(vector_at(perm, "f2", "s3"), 40 / stats::median(c(8 / 10, 40 / 20)), tolerance = 1e-8)

  missing_file <- triomics_metabolomics_operations(
    normalise = "pqn", reference = "supplied", reference_file = file.path(new_dir("a-t05-miss-"), "absent.tsv")
  )
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-nofile-"), c("s1", "s2"), missing_file, "nofile")),
    "A_SCALE_REFERENCE"
  )
  training_with_file <- triomics_metabolomics_operations(
    normalise = "pqn", reference = "training_median", reference_file = ref_path
  )
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-both-"), c("s1", "s2"), training_with_file, "both")),
    "A_CONFIG_METHOD"
  )
})

testthat::test_that("A-T05-AC2 unknown missingness is not rewritten as MAR and training median stays an explicit baseline", {
  register_metabolomics()
  values <- matrix(
    c(2, 4, NA, 6, 8, 10),
    nrow = 2,
    dimnames = list(c("f1", "f2"), c("s1", "s2", "s3"))
  )
  mask <- observed_mask(values)
  mask["f1", "s2"] <- "known_missing"
  study <- assemble_block(values, mask = mask)
  default_ops <- triomics_metabolomics_operations()
  testthat::expect_identical(default_ops[[6L]]$method, "none")
  role <- triomics_metabolomics_imputation_description("training_median")
  testthat::expect_identical(role$role, "explicit_baseline")
  testthat::expect_false(isTRUE(role$biological_default))
  testthat::expect_false(identical(role$mechanism, "mar"))
  none_dir <- new_dir("a-t05-none-")
  kept <- prepare_fit(study, fit_spec(none_dir, c("s1", "s2"), default_ops, "none"))
  testthat::expect_true(is.na(analysis_of(kept)["f1", "s2"]))
  testthat::expect_identical(mask_of(kept)["f1", "s2"], "known_missing")
  none_state <- triomics_read_state(kept$state_refs$alpha, none_dir)
  testthat::expect_identical(as.character(none_state$learned_parameters$missingness_assumption), "unknown")
  testthat::expect_false(identical(as.character(none_state$learned_parameters$missingness_assumption), "mar"))

  bare <- list(stage = "impute", method = "knn", k = 1L, qualification = knn_pin())
  testthat::expect_identical(
    triomics_metabolomics_lane_decision(bare)$reason,
    "unknown_missingness_is_not_mar"
  )
  knn_unknown <- triomics_metabolomics_operations(
    impute = "knn", k = 1L, qualification = knn_pin(),
    missingness_assumption = "unknown",
    missingness_evidence = "This text must not turn unknown into MAR."
  )
  testthat::expect_identical(triomics_metabolomics_lane_decision(knn_unknown[[6L]])$reason, "unknown_missingness_is_not_mar")
  testthat::expect_true(all(vapply(knn_unknown, function(op) identical(op$apply, "unavailable"), logical(1L))))
  before <- snapshot(study)
  unknown_err <- expect_code(
    prepare_fit(study, fit_spec(new_dir("a-t05-unk-"), c("s1", "s2"), knn_unknown, "unk")),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(unknown_err), "Unknown missingness", fixed = TRUE)
  testthat::expect_match(conditionMessage(unknown_err), "MAR", fixed = TRUE)
  testthat::expect_equal(snapshot(study), before, tolerance = 1e-8)

  mcar <- triomics_metabolomics_operations(
    impute = "rf", trees = 2L, qualification = rf_pin(),
    missingness_assumption = "mcar", missingness_evidence = "MCAR is not MAR."
  )
  mcar_err <- expect_code(
    prepare_fit(assemble_block(values, mask = mask), fit_spec(new_dir("a-t05-mcar-"), c("s1", "s2"), mcar, "mcar")),
    "A_CONFIG_METHOD"
  )
  testthat::expect_match(conditionMessage(mcar_err), "missingness_does_not_select_mar", fixed = TRUE)

  median_ops <- triomics_metabolomics_operations(
    impute = "training_median", missingness_assumption = "unknown",
    missingness_evidence = "Explicit baseline requested. Not a biological mechanism."
  )
  median_dir <- new_dir("a-t05-med-")
  median_fit <- prepare_fit(assemble_block(values, mask = mask), fit_spec(median_dir, c("s1", "s2"), median_ops, "median"))
  testthat::expect_equal(vector_at(median_fit, "f1", "s2"), 2, tolerance = 1e-8)
  testthat::expect_identical(mask_of(median_fit)["f1", "s2"], "known_imputed")
  median_state <- triomics_read_state(median_fit$state_refs$alpha, median_dir)
  testthat::expect_identical(as.character(median_state$learned_parameters$missingness_assumption), "unknown")
  testthat::expect_identical(as.character(median_state$learned_parameters$imputation_role$role), "explicit_baseline")
  testthat::expect_false(isTRUE(median_state$learned_parameters$imputation_role$biological_default))
  testthat::expect_false(identical(as.character(median_state$learned_parameters$imputation_role$mechanism), "mar"))

  unknown_mask <- mask
  unknown_mask["f2", "s3"] <- "unknown"
  unknown_values <- values
  unknown_values["f2", "s3"] <- NA_real_
  unknown_study <- assemble_block(unknown_values, mask = unknown_mask)
  before_unknown <- snapshot(unknown_study)
  expect_code(
    prepare_fit(unknown_study, fit_spec(new_dir("a-t05-umask-"), c("s1", "s2"), median_ops, "umask")),
    "A_SOURCE_MASK"
  )
  testthat::expect_equal(snapshot(unknown_study), before_unknown, tolerance = 1e-8)
})

testthat::test_that("A-T05-AC3 kNN fits training donors only and held-out rows do not change them", {
  register_metabolomics()
  values <- matrix(
    c(1, 10, 100, 100, 1, 2, 100, 100),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("a", "b"), c("s1", "s2", "s3", "s4"))
  )
  values["b", "s4"] <- NA_real_
  mask <- observed_mask(values)
  mask["b", "s4"] <- "known_missing"
  study <- assemble_block(values, mask = mask)
  ops <- triomics_metabolomics_operations(
    impute = "knn", k = 1L, qualification = knn_pin(),
    missingness_assumption = "mar",
    missingness_evidence = "Synthetic MAR fixture. Not an assay-wide default."
  )
  testthat::expect_identical(triomics_metabolomics_lane_decision(ops[[6L]])$status, "fit_apply")
  dir <- new_dir("a-t05-knn-")
  fitted <- prepare_fit(study, fit_spec(dir, c("s1", "s2"), ops, "knn"))
  testthat::expect_equal(vector_at(fitted, "b", "s4"), 2, tolerance = 1e-8)
  testthat::expect_identical(mask_of(fitted)["b", "s4"], "known_imputed")
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  donors <- as.character(state$learned_parameters$knn$donor_sample_ids)
  testthat::expect_identical(donors, c("s1", "s2"))
  testthat::expect_false("s3" %in% donors || "s4" %in% donors)

  changed <- values
  changed["a", "s3"] <- 0
  changed["b", "s3"] <- 0
  changed_dir <- new_dir("a-t05-knn2-")
  changed_fit <- prepare_fit(
    assemble_block(changed, mask = mask),
    fit_spec(changed_dir, c("s1", "s2"), ops, "knn2")
  )
  testthat::expect_equal(vector_at(changed_fit, "b", "s4"), 2, tolerance = 1e-8)
  changed_state <- triomics_read_state(changed_fit$state_refs$alpha, changed_dir)
  testthat::expect_identical(
    changed_state$learned_parameters$knn$donor_hash,
    state$learned_parameters$knn$donor_hash
  )

  weighted_values <- matrix(
    c(1, 2, 10, 1, 2, NA),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("a", "b"), c("s1", "s2", "s3"))
  )
  weighted_mask <- observed_mask(weighted_values)
  weighted_mask["b", "s3"] <- "known_missing"
  weighted_ops <- triomics_metabolomics_operations(
    impute = "knn", k = 2L, qualification = knn_pin(),
    missingness_assumption = "mar", missingness_evidence = "Synthetic MAR fixture."
  )
  weighted <- prepare_fit(
    assemble_block(weighted_values, mask = weighted_mask),
    fit_spec(new_dir("a-t05-k2-"), c("s1", "s2"), weighted_ops, "k2")
  )
  testthat::expect_equal(vector_at(weighted, "b", "s3"), 26 / 17, tolerance = 1e-8)

  decoy <- matrix(
    c(1, 2, 3, NA, 2, 3),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("a", "b"), c("s1", "s2", "s3"))
  )
  decoy_mask <- observed_mask(decoy)
  decoy_mask["b", "s1"] <- "known_missing"
  decoy_ops <- triomics_metabolomics_operations(
    impute = "knn", k = 1L, qualification = knn_pin(),
    missingness_assumption = "mar", missingness_evidence = "Synthetic MAR fixture."
  )
  decoy_fit <- prepare_fit(
    assemble_block(decoy, mask = decoy_mask),
    fit_spec(new_dir("a-t05-decoy-"), c("s1", "s2", "s3"), decoy_ops, "decoy")
  )
  testthat::expect_equal(vector_at(decoy_fit, "b", "s1"), 2, tolerance = 1e-8)

  fresh <- assemble_block(values, mask = mask)
  applied <- prepare_apply(fresh, fit_spec(dir, NULL, ops, "knn-apply"), fitted$state_refs)
  testthat::expect_equal(vector_at(applied, "b", "s4"), 2, tolerance = 1e-8)
  testthat::expect_identical(applied$state_refs$alpha$state_hash, fitted$state_refs$alpha$state_hash)
  testthat::expect_identical(
    triomics_read_state(applied$state_refs$alpha, dir)$learned_parameters$knn$donor_hash,
    state$learned_parameters$knn$donor_hash
  )

  short <- matrix(c(1, NA_real_), nrow = 1, dimnames = list("a", c("s1", "s2")))
  short_mask <- observed_mask(short)
  short_mask["a", "s2"] <- "known_missing"
  short_ops <- triomics_metabolomics_operations(
    impute = "knn", k = 2L, qualification = knn_pin(),
    missingness_assumption = "mar", missingness_evidence = "Synthetic MAR fixture."
  )
  expect_code(
    prepare_fit(assemble_block(short, mask = short_mask), fit_spec(new_dir("a-t05-kshort-"), "s1", short_ops, "kshort")),
    "A_SCALE_NEIGHBORS"
  )
  missing_k <- triomics_metabolomics_operations(
    impute = "knn", qualification = knn_pin(),
    missingness_assumption = "mar", missingness_evidence = "Synthetic MAR fixture."
  )
  expect_code(
    prepare_fit(study, fit_spec(new_dir("a-t05-nok-"), c("s1", "s2"), missing_k, "nok")),
    "A_CONFIG_METHOD"
  )
})

testthat::test_that("A-T05-AC3 RF requires a pinned inductive fit and ignores held-out rows", {
  register_metabolomics()
  values <- matrix(
    c(1, 2, 3, 9, 2, 4, 6, NA),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("x", "y"), c("s1", "s2", "s3", "s4"))
  )
  mask <- observed_mask(values)
  mask["y", "s4"] <- "known_missing"
  ops <- triomics_metabolomics_operations(
    impute = "rf", trees = 4L, qualification = rf_pin(),
    missingness_assumption = "mar",
    missingness_evidence = "Synthetic MAR fixture for an inductive forest."
  )
  dir <- new_dir("a-t05-rf-")
  fitted <- prepare_fit(assemble_block(values, mask = mask), fit_spec(dir, c("s1", "s2", "s3"), ops, "rf"))
  observed <- c(2, 4, 6)
  imputed <- vector_at(fitted, "y", "s4")
  testthat::expect_gte(imputed, min(observed) - 1e-8)
  testthat::expect_lte(imputed, max(observed) + 1e-8)
  testthat::expect_identical(mask_of(fitted)["y", "s4"], "known_imputed")
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  testthat::expect_identical(as.character(state$learned_parameters$rf$donor_sample_ids), c("s1", "s2", "s3"))
  testthat::expect_false("s4" %in% as.character(state$learned_parameters$rf$donor_sample_ids))

  changed <- values
  changed["x", "s4"] <- 1000
  changed_dir <- new_dir("a-t05-rf2-")
  changed_fit <- prepare_fit(assemble_block(changed, mask = mask), fit_spec(changed_dir, c("s1", "s2", "s3"), ops, "rf2"))
  changed_state <- triomics_read_state(changed_fit$state_refs$alpha, changed_dir)
  testthat::expect_identical(changed_state$learned_parameters$rf$forest_hash, state$learned_parameters$rf$forest_hash)

  applied <- prepare_apply(
    assemble_block(values, mask = mask),
    fit_spec(dir, NULL, ops, "rf-apply"),
    fitted$state_refs
  )
  testthat::expect_equal(vector_at(applied, "y", "s4"), imputed, tolerance = 1e-8)
  testthat::expect_identical(applied$state_refs$alpha$state_hash, fitted$state_refs$alpha$state_hash)

  transductive <- triomics_metabolomics_operations(
    impute = "rf", trees = 2L,
    missingness_assumption = "mar", missingness_evidence = "Synthetic MAR fixture.",
    qualification = list(
      status = "pinned", method_id = "rf", package = "missForest", version = "1.5",
      native_fit = "missForest", native_apply = NULL, fit_apply = FALSE,
      contexts = "metabolomics"
    )
  )
  testthat::expect_identical(
    triomics_metabolomics_lane_decision(transductive[[6L]])$reason,
    "inductive_fit_apply_not_supported"
  )
  before <- snapshot(assemble_block(values, mask = mask))
  trans_study <- assemble_block(values, mask = mask)
  expect_code(
    prepare_fit(trans_study, fit_spec(new_dir("a-t05-trans-"), c("s1", "s2", "s3"), transductive, "trans")),
    "A_CONFIG_METHOD"
  )
  testthat::expect_equal(snapshot(trans_study), before, tolerance = 1e-8)

  vague <- triomics_metabolomics_operations(
    impute = "knn", k = 1L,
    missingness_assumption = "mar", missingness_evidence = "Synthetic MAR fixture.",
    qualification = list(
      status = "pinned", method_id = "knn", package = "impute", version = "1.76.0",
      native_fit = "impute.knn", native_apply = "impute.knn", fit_apply = TRUE,
      distance = "rms_shared_observed", weights = "inverse_distance",
      donors = "training_samples_only", contexts = "metabolomics"
    )
  )
  testthat::expect_identical(triomics_metabolomics_lane_decision(vague[[6L]])$reason, "qualification_not_pinned")
  no_pin <- triomics_metabolomics_operations(
    impute = "rf", trees = 2L, missingness_assumption = "mar",
    missingness_evidence = "Synthetic MAR fixture."
  )
  testthat::expect_identical(triomics_metabolomics_lane_decision(no_pin[[6L]])$reason, "qualification_missing")

  all_missing <- matrix(c(NA_real_, 1, NA_real_, 2), nrow = 2, dimnames = list(c("gone", "keep"), c("s1", "s2")))
  all_mask <- observed_mask(all_missing)
  all_mask["gone", ] <- "known_missing"
  expect_code(
    prepare_fit(
      assemble_block(all_missing, mask = all_mask),
      fit_spec(new_dir("a-t05-allrf-"), c("s1", "s2"), ops, "allrf")
    ),
    "A_SCALE_ALL_MISSING"
  )
})

testthat::test_that("A-T05-AC4 zeros, reference mismatch, all-missing features and losses carry named lineage", {
  register_metabolomics()
  values <- matrix(
    c(20, 40, 5, 10),
    nrow = 2,
    dimnames = list(c("f1", "f2"), c("s1", "s2"))
  )
  ops <- triomics_metabolomics_operations(
    transform = "log2", normalise = "pqn", reference = "training_median"
  )
  zero_values <- values
  zero_values["f1", "s2"] <- 0
  zero_study <- assemble_block(zero_values)
  before_zero <- snapshot(zero_study)
  zero_err <- expect_code(
    prepare_fit(zero_study, fit_spec(new_dir("a-t05-zero-"), c("s1", "s2"), ops, "zero")),
    "A_SCALE_ZERO"
  )
  testthat::expect_match(conditionMessage(zero_err), "zero", fixed = TRUE)
  testthat::expect_equal(snapshot(zero_study), before_zero, tolerance = 1e-8)

  negative <- values
  negative["f2", "s1"] <- -4
  neg_err <- expect_code(
    prepare_fit(assemble_block(negative), fit_spec(new_dir("a-t05-neg-"), c("s1", "s2"), ops, "neg")),
    "A_SCALE_NONPOSITIVE"
  )
  testthat::expect_match(conditionMessage(neg_err), "negative", fixed = TRUE)

  ref_path <- write_reference(c(f1 = 0, f2 = 20), file.path(new_dir("a-t05-zref-"), "reference.tsv"))
  zero_ref <- triomics_metabolomics_operations(
    normalise = "pqn", reference = "supplied", reference_file = ref_path
  )
  den_err <- expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-den-"), c("s1", "s2"), zero_ref, "den")),
    "A_SCALE_ZERO"
  )
  testthat::expect_match(conditionMessage(den_err), "denominator", fixed = TRUE)

  bad_path <- write_reference(c(f1 = 10, other = 20), file.path(new_dir("a-t05-badref-"), "reference.tsv"))
  bad_ops <- triomics_metabolomics_operations(
    normalise = "pqn", reference = "supplied", reference_file = bad_path
  )
  mismatch <- expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-mis-"), c("s1", "s2"), bad_ops, "mis")),
    "A_SCALE_REFERENCE"
  )
  testthat::expect_match(conditionMessage(mismatch), "reference mismatch", fixed = TRUE)

  neg_path <- write_reference(c(f1 = 10, f2 = -5), file.path(new_dir("a-t05-nref-"), "reference.tsv"))
  neg_ops <- triomics_metabolomics_operations(
    normalise = "pqn", reference = "supplied", reference_file = neg_path
  )
  expect_code(
    prepare_fit(assemble_block(values), fit_spec(new_dir("a-t05-nrefit-"), c("s1", "s2"), neg_ops, "nref")),
    "A_SCALE_NONPOSITIVE"
  )

  empty <- matrix(c(NA_real_, 4, NA_real_, 8), nrow = 2, dimnames = list(c("gone", "keep"), c("s1", "s2")))
  empty_mask <- observed_mask(empty)
  empty_mask["gone", ] <- "known_missing"
  expect_code(
    prepare_fit(
      assemble_block(empty, mask = empty_mask),
      fit_spec(new_dir("a-t05-empty-"), c("s1", "s2"), ops, "empty")
    ),
    "A_SCALE_ALL_MISSING"
  )

  filtered <- prepare_fit(
    assemble_block(empty, mask = empty_mask),
    fit_spec(
      new_dir("a-t05-drop-"), c("s1", "s2"),
      triomics_metabolomics_operations(
        filter_method = "prevalence", min_observed = 1L,
        transform = "log2", normalise = "pqn", reference = "training_median"
      ),
      "drop"
    )
  )
  testthat::expect_identical(rownames(analysis_of(filtered)), "keep")
  testthat::expect_false(isTRUE(flow_row(filtered, "gone")$retained[[1L]]))
  testthat::expect_identical(as.character(flow_row(filtered, "gone")$reason[[1L]]), "below_min_observed")
  testthat::expect_equal(vector_at(filtered, "keep", "s1"), log2(6), tolerance = 1e-8)

  ids <- c("s1", "s2", "s3", "s4", "s5")
  groups <- c("control", "control", "treatment", "treatment", "treatment")
  features <- c("leak", "any_only", "both", "held_only")
  group_values <- matrix(NA_real_, length(features), length(ids), dimnames = list(features, ids))
  group_values["leak", c("s1", "s5")] <- c(2, 9)
  group_values["any_only", "s1"] <- 4
  group_values["both", c("s1", "s3")] <- c(1, 3)
  group_values["held_only", "s5"] <- 7
  group_mask <- observed_mask(group_values)
  group_mask[is.na(group_values)] <- "known_missing"
  group_study <- assemble_block(group_values, mask = group_mask, groups = groups)
  each <- triomics_metabolomics_operations(
    filter_method = "prevalence", min_observed = 1L, scope = "stratum", stratum = "group",
    selection_policy = "training_min_in_each_group",
    selection_justification = "Declared training groups only."
  )
  each_fit <- prepare_fit(group_study, fit_spec(new_dir("a-t05-each-"), c("s1", "s2", "s3", "s4"), each, "each"))
  testthat::expect_identical(rownames(analysis_of(each_fit)), "both")
  testthat::expect_false(isTRUE(flow_row(each_fit, "held_only")$retained[[1L]]))

  rows <- c("cont", "good", "sparse")
  qc_values <- matrix(c(5, 5, 5, 6, 6, 6, 1, NA, NA), nrow = 3, byrow = TRUE, dimnames = list(rows, c("s1", "s2", "s3")))
  qc_mask <- observed_mask(qc_values)
  qc_mask["sparse", c("s2", "s3")] <- "known_missing"
  qc_study <- assemble_block(qc_values, mask = qc_mask)
  row_data <- SummarizedExperiment::rowData(qc_study$experiments$alpha)
  row_data$declared_exclusion <- NA_character_
  row_data["cont", "declared_exclusion"] <- "contaminant"
  SummarizedExperiment::rowData(qc_study$experiments$alpha) <- row_data
  qc_ops <- triomics_metabolomics_operations(
    filter_method = "prevalence", min_observed = 2L, impute = "training_median",
    missingness_assumption = "unknown"
  )
  qc_dir <- new_dir("a-t05-qc-")
  qc_fit <- prepare_fit(qc_study, fit_spec(qc_dir, c("s1", "s2", "s3"), qc_ops, "qc"))
  testthat::expect_identical(rownames(analysis_of(qc_fit)), "good")
  testthat::expect_identical(as.character(flow_row(qc_fit, "cont")$reason[[1L]]), "declared_contaminant")
  testthat::expect_false(isTRUE(flow_row(qc_fit, "sparse")$retained[[1L]]))
  testthat::expect_equal(flow_row(qc_fit, "cont")$n_observed[[1L]], 3)

  lineage_values <- matrix(c(2, NA, 8, 4, 6, 10), nrow = 2, byrow = TRUE, dimnames = list(c("f1", "f2"), c("s1", "s2", "s3")))
  lineage_mask <- observed_mask(lineage_values)
  lineage_mask["f1", "s2"] <- "known_missing"
  lineage_ops <- triomics_metabolomics_operations(
    impute = "training_median", missingness_assumption = "unknown"
  )
  lineage_dir <- new_dir("a-t05-line-")
  lineage_fit <- prepare_fit(
    assemble_block(lineage_values, mask = lineage_mask),
    fit_spec(lineage_dir, c("s1", "s2"), lineage_ops, "line")
  )
  lineage_state <- triomics_read_state(lineage_fit$state_refs$alpha, lineage_dir)
  lineage <- lineage_state$learned_parameters$imputation_lineage
  if (is.data.frame(lineage)) {
    keys <- as.character(lineage$feature_key)
    samples <- as.character(lineage$sample_id)
    methods <- as.character(lineage$method)
    roles <- as.character(lineage$role)
    vals <- as.numeric(lineage$value)
  } else if (is.list(lineage) && !is.null(lineage$feature_key) && !is.list(lineage$feature_key)) {
    keys <- as.character(lineage$feature_key)
    samples <- as.character(lineage$sample_id)
    methods <- as.character(lineage$method)
    roles <- as.character(lineage$role)
    vals <- as.numeric(lineage$value)
  } else {
    keys <- vapply(lineage, function(row) as.character(row$feature_key), character(1))
    samples <- vapply(lineage, function(row) as.character(row$sample_id), character(1))
    methods <- vapply(lineage, function(row) as.character(row$method), character(1))
    roles <- vapply(lineage, function(row) as.character(row$role), character(1))
    vals <- vapply(lineage, function(row) as.numeric(row$value), numeric(1))
  }
  testthat::expect_identical(keys, "f1")
  testthat::expect_identical(samples, "s2")
  testthat::expect_identical(methods, "training_median")
  testthat::expect_identical(roles, "explicit_baseline")
  testthat::expect_equal(vals, 2, tolerance = 1e-8)
  testthat::expect_identical(mask_of(lineage_fit)["f1", "s2"], "known_imputed")
  testthat::expect_false(any(mask_of(lineage_fit) == "known_imputed" & !(rownames(lineage_fit$experiments$alpha)[1L] == "f1")))
})
