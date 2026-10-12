expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

new_dir <- function(prefix) {
  path <- tempfile(prefix)
  dir.create(path)
  path
}

sample_registry <- function(ids, outcome, subject = ids) {
  data.frame(
    sample_id = ids,
    subject_id = subject,
    outcome = outcome,
    sample_role = rep("undeclared", length(ids)),
    stringsAsFactors = FALSE
  )
}

feature_rows <- function(study_id, block_id, keys) {
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
    scale = rep("linear", n),
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

observed_mask <- function(assay) {
  mask <- matrix("known_observed", nrow(assay), ncol(assay), dimnames = dimnames(assay))
  mask[is.na(assay)] <- "known_missing"
  mask
}

availability_rows <- function(block_ids, registry_ids, absent) {
  rows <- list()
  for (block_id in block_ids) {
    for (sample_id in registry_ids) {
      gone <- !is.null(absent[[block_id]]) && sample_id %in% absent[[block_id]]
      rows[[length(rows) + 1L]] <- data.frame(
        study_id = "study",
        block_id = block_id,
        sample_id = sample_id,
        presence = if (gone) "structural_absence" else "present",
        cell_imputation_eligible = !gone,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

assemble_blocks <- function(registry, assays, masks, alignment = "strict", absent = NULL) {
  ids <- names(assays)
  raw <- list(
    study_id = "study",
    sample_registry = registry,
    feature_registry = do.call(rbind, lapply(ids, function(id) feature_rows("study", id, rownames(assays[[id]])))),
    sample_maps = stats::setNames(lapply(ids, function(id) named_map(id, colnames(assays[[id]]))), ids),
    assays = assays,
    original_masks = masks,
    availability = availability_rows(ids, registry$sample_id, absent),
    source_manifests = list(),
    selection = data.frame(),
    column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = stats::setNames(replicate(length(ids), character(), FALSE), ids)
  )
  triomics_assemble_study(raw, alignment = alignment)
}

register_profiles <- function() {
  triomics_reset_profiles()
  triomics_register_metabolomics_profile()
  triomics_register_proteomics_profile()
  triomics_register_profile("baseline", triomics_baseline_profile())
}

predictive_ops <- function() {
  triomics_metabolomics_operations(
    filter_method = "prevalence",
    min_observed = 1,
    scope = "stratum",
    stratum = "outcome",
    selection_policy = "training_min_in_any_group",
    selection_justification = "Synthetic training-label filter.",
    transform = "log2",
    normalise = "pqn",
    reference = "training_median",
    impute = "training_median"
  )
}

descriptive_ops <- function(ops) {
  lapply(ops, function(op) {
    if (identical(op$stage, "impute")) op$method <- "none"
    op
  })
}

spec_for <- function(dir, ops, context, training = NULL, blocks = "alpha") {
  list(
    preparation_context_id = context,
    training_unit_ids = training,
    seed = 11L,
    state_dir = dir,
    blocks = lapply(blocks, function(block_id) {
      list(block_id = block_id, profile_id = "metabolomics", operations = ops)
    })
  )
}

manual_plan <- function() {
  list(
    schema = "SplitPlan",
    seed = 11L,
    grouping_unit = "subject",
    target = "synthetic_outcome",
    stratification = "none",
    temporal_order = NULL,
    n_outer = 2L,
    n_inner = 2L,
    unit_subject = data.frame(
      unit_id = paste0("s", 1:8),
      subject_id = paste0("s", 1:8),
      stringsAsFactors = FALSE
    ),
    folds = list(
      list(
        fold_id = "outer_01",
        level = "outer",
        parent_fold_id = NULL,
        training_unit_ids = c("s1", "s2", "s3", "s4"),
        test_unit_ids = c("s5", "s6", "s7", "s8"),
        training_subject_ids = c("s1", "s2", "s3", "s4"),
        test_subject_ids = c("s5", "s6", "s7", "s8"),
        inner = list(list(
          fold_id = "outer_01__inner_01",
          level = "inner",
          parent_fold_id = "outer_01",
          training_unit_ids = c("s1", "s2"),
          test_unit_ids = c("s3", "s4"),
          training_subject_ids = c("s1", "s2"),
          test_subject_ids = c("s3", "s4"),
          inner = list()
        ))
      ),
      list(
        fold_id = "outer_02",
        level = "outer",
        parent_fold_id = NULL,
        training_unit_ids = c("s5", "s6", "s7", "s8"),
        test_unit_ids = c("s1", "s2", "s3", "s4"),
        training_subject_ids = c("s5", "s6", "s7", "s8"),
        test_subject_ids = c("s1", "s2", "s3", "s4"),
        inner = list(list(
          fold_id = "outer_02__inner_01",
          level = "inner",
          parent_fold_id = "outer_02",
          training_unit_ids = c("s5", "s6"),
          test_unit_ids = c("s7", "s8"),
          training_subject_ids = c("s5", "s6"),
          test_subject_ids = c("s7", "s8"),
          inner = list()
        ))
      )
    )
  )
}

ac1_matrices <- function(test_ids, test_value, test_outcome) {
  ids <- paste0("s", 1:8)
  outcome <- c("control", "treatment", "control", "treatment", "treatment", "control", "treatment", "control")
  names(outcome) <- ids
  outcome[test_ids] <- test_outcome
  stable <- c(s2 = 30, s1 = 10, s4 = 40, s3 = 20, s8 = 1000, s6 = 1000, s5 = 1000, s7 = 1000)
  gap <- c(s2 = NA, s1 = 8, s4 = 16, s3 = 12, s8 = 500, s6 = 500, s5 = 500, s7 = 500)
  leaky <- c(s2 = NA, s1 = NA, s4 = NA, s3 = NA, s8 = NA, s6 = NA, s5 = 7, s7 = NA)
  order <- c("s2", "s1", "s4", "s3", "s8", "s6", "s5", "s7")
  values <- rbind(stable = stable[order], gap = gap[order], leaky = leaky[order])
  if (length(test_ids)) values[, test_ids] <- test_value
  values[, "s5"] <- ifelse(rownames(values) == "leaky", test_value, values[, "s5"])
  if (!length(test_ids)) values["leaky", "s5"] <- 7
  list(order = order, outcome = outcome[order], values = values)
}

find_fold <- function(prepared, fold_id) {
  for (outer in prepared$folds) {
    if (identical(outer$fold_id, fold_id)) return(outer)
    for (inner in outer$inner) if (identical(inner$fold_id, fold_id)) return(inner)
  }
  NULL
}

oracle_hash <- function(study, ids, ops, dir) {
  se <- study$experiments$alpha[, ids, drop = FALSE]
  sub <- study
  sub$experiments <- list(alpha = se)
  sub$mae <- triomics_mae_from_experiments(sub)
  fitted <- prepare_fit(sub, spec_for(dir, ops, "oracle", ids, "alpha"))
  triomics_read_state(fitted$state_refs$alpha, dir)$state_hash
}

testthat::test_that("A-T07-AC1 outer and inner fits ignore held-out values and outcomes", {
  register_profiles()
  ops <- predictive_ops()
  built <- ac1_matrices(character(), 7, "treatment")
  registry <- sample_registry(built$order, unname(built$outcome))
  study <- assemble_blocks(registry, list(alpha = built$values), list(alpha = observed_mask(built$values)))
  plan <- triomics_finalize_split_plan(manual_plan())
  dir <- new_dir("a-t07-ac1-")
  prepared <- triomics_fold_prepare(
    study,
    spec_for(dir, ops, "predictive", NULL, "alpha"),
    plan,
    spec_for(dir, descriptive_ops(ops), "descriptive", built$order, "alpha")
  )
  outer <- find_fold(prepared, "outer_01")
  inner <- find_fold(prepared, "outer_01__inner_01")
  other <- find_fold(prepared, "outer_02")
  testthat::expect_false(is.null(outer))
  testthat::expect_false(is.null(inner))
  testthat::expect_false("leaky" %in% outer$blocks$alpha$feature_keys)
  testthat::expect_false("leaky" %in% inner$blocks$alpha$feature_keys)
  testthat::expect_true("leaky" %in% other$blocks$alpha$feature_keys)
  testthat::expect_equal(outer$blocks$alpha$pqn_reference[["stable"]], 25, tolerance = 1e-8)
  testthat::expect_equal(outer$blocks$alpha$pqn_reference[["gap"]], 12, tolerance = 1e-8)
  testthat::expect_identical(outer$blocks$alpha$output_scale, "log2")
  testthat::expect_identical(outer$blocks$alpha$pqn_reference_scale, "linear")
  testthat::expect_identical(outer$blocks$alpha$training_unit_ids, c("s1", "s2", "s3", "s4"))
  testthat::expect_identical(
    outer$blocks$alpha$state_hash,
    oracle_hash(study, c("s1", "s2", "s3", "s4"), ops, dir)
  )
  testthat::expect_identical(
    inner$blocks$alpha$state_hash,
    oracle_hash(study, c("s1", "s2"), ops, dir)
  )
  state_path <- file.path(dir, outer$blocks$alpha$state_ref$path)
  frozen_hash <- triomics_sha256(state_path)
  frozen_keys <- outer$blocks$alpha$feature_keys
  outer$blocks$alpha$feature_keys <- "tampered"
  reread <- triomics_read_state(prepared$folds[[1L]]$blocks$alpha$state_ref, dir)
  testthat::expect_identical(as.character(reread$feature_keys), frozen_keys)
  testthat::expect_false("tampered" %in% as.character(reread$feature_keys))

  mutated_built <- ac1_matrices(c("s5", "s6", "s7", "s8"), 1e9, "mutated_outcome")
  mutated <- assemble_blocks(
    sample_registry(mutated_built$order, unname(mutated_built$outcome)),
    list(alpha = mutated_built$values),
    list(alpha = observed_mask(mutated_built$values))
  )
  original_assay <- SummarizedExperiment::assay(study$experiments$alpha, "analysis")
  again <- triomics_fold_prepare(
    mutated,
    spec_for(dir, ops, "predictive", NULL, "alpha"),
    plan,
    spec_for(dir, descriptive_ops(ops), "descriptive", mutated_built$order, "alpha")
  )
  again_outer <- find_fold(again, "outer_01")
  again_inner <- find_fold(again, "outer_01__inner_01")
  testthat::expect_identical(again_outer$blocks$alpha$state_hash, outer$blocks$alpha$state_hash)
  testthat::expect_identical(again_inner$blocks$alpha$state_hash, inner$blocks$alpha$state_hash)
  testthat::expect_identical(again_outer$blocks$alpha$feature_keys, frozen_keys)
  testthat::expect_equal(again_outer$blocks$alpha$pqn_reference[["stable"]], 25, tolerance = 1e-8)
  testthat::expect_equal(again_outer$blocks$alpha$medians[["gap"]], outer$blocks$alpha$medians[["gap"]], tolerance = 1e-8)
  testthat::expect_identical(again_outer$blocks$alpha$output_scale, "log2")
  testthat::expect_identical(again_outer$blocks$alpha$pqn_reference_scale, outer$blocks$alpha$pqn_reference_scale)
  testthat::expect_identical(triomics_sha256(state_path), frozen_hash)
  testthat::expect_identical(
    SummarizedExperiment::assay(study$experiments$alpha, "analysis"),
    original_assay
  )
  testthat::expect_false("s5" %in% again_outer$blocks$alpha$training_unit_ids)

  leaked <- manual_plan()
  leaked$folds[[1L]]$test_subject_ids <- c(leaked$folds[[1L]]$test_subject_ids, "s1")
  leaked$folds[[1L]]$test_unit_ids <- c(leaked$folds[[1L]]$test_unit_ids, "s1")
  overlap <- expect_code(triomics_finalize_split_plan(leaked), "A_STATE_SPLIT")
  testthat::expect_match(conditionMessage(overlap), "shares a subject", fixed = TRUE)
})

testthat::test_that("A-T07-AC1 infeasible, grouped, and temporal splits are not quietly reduced", {
  ids <- paste0("u", 1:8)
  subjects <- rep(paste0("p", 1:4), each = 2)
  plan <- triomics_nested_split_plan(ids, subjects, n_outer = 2L, n_inner = 2L, seed = 19L, target = "group")
  testthat::expect_identical(plan$grouping_unit, "subject")
  testthat::expect_true(nzchar(plan$creation_hash))
  for (outer in plan$folds) {
    testthat::expect_length(intersect(outer$training_subject_ids, outer$test_subject_ids), 0L)
    testthat::expect_length(intersect(outer$training_unit_ids, outer$test_unit_ids), 0L)
    for (subject in unique(subjects)) {
      units <- ids[subjects == subject]
      in_train <- any(units %in% outer$training_unit_ids)
      in_test <- any(units %in% outer$test_unit_ids)
      testthat::expect_false(in_train && in_test)
      if (in_train) testthat::expect_true(all(units %in% outer$training_unit_ids))
      if (in_test) testthat::expect_true(all(units %in% outer$test_unit_ids))
    }
    for (inner in outer$inner) {
      testthat::expect_length(intersect(inner$training_subject_ids, inner$test_subject_ids), 0L)
      testthat::expect_true(all(inner$test_subject_ids %in% outer$training_subject_ids))
      testthat::expect_length(intersect(inner$test_subject_ids, outer$test_subject_ids), 0L)
    }
  }
  tampered <- plan
  tampered$creation_hash <- paste0(rep("0", 64), collapse = "")
  stale <- expect_code(triomics_validate_split_plan(tampered), "A_STATE_STALE")
  testthat::expect_match(conditionMessage(stale), "creation hash", fixed = TRUE)

  shrink <- expect_code(
    triomics_nested_split_plan(paste0("u", 1:4), n_outer = 5L, n_inner = 2L, seed = 3L),
    "A_STATE_SPLIT"
  )
  testthat::expect_match(conditionMessage(shrink), "were not reduced", fixed = TRUE)
  strata <- c("control", "treatment", "treatment", "treatment")
  dropped <- expect_code(
    triomics_nested_split_plan(paste0("u", 1:4), n_outer = 2L, n_inner = 2L, seed = 3L, stratification = strata),
    "A_STATE_SPLIT"
  )
  testthat::expect_match(conditionMessage(dropped), "were not reduced", fixed = TRUE)

  temporal <- triomics_nested_split_plan(
    paste0("t", 1:6), n_outer = 2L, n_inner = 2L, seed = 5L,
    temporal_order = stats::setNames(1:6, paste0("t", 1:6))
  )
  first <- temporal$folds[[1L]]
  testthat::expect_identical(first$training_unit_ids, paste0("t", 1:4))
  testthat::expect_identical(first$test_unit_ids, "t5")
  testthat::expect_identical(temporal$folds[[2L]]$test_unit_ids, "t6")
  testthat::expect_true(all(first$inner[[1L]]$test_unit_ids %in% paste0("t", 3:4)))
  backwards <- manual_plan()
  backwards$temporal_order <- lapply(paste0("s", 1:8), function(id) list(unit_id = id, time = if (id == "s5") 0 else 2))
  backwards$folds <- list(backwards$folds[[1L]])
  backwards$folds[[1L]]$inner <- list()
  backwards$n_outer <- 1L
  early <- expect_code(triomics_finalize_split_plan(backwards), "A_STATE_SPLIT")
  testthat::expect_match(conditionMessage(early), "strictly later", fixed = TRUE)
})

testthat::test_that("A-T07-AC2 apply does not refit and rejects feature reordering", {
  register_profiles()
  ids <- c("s1", "s2", "s3", "s4")
  values <- matrix(
    c(2, NA, 4, 100, 10, 10, 10, 10),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("f1", "f2"), ids)
  )
  mask <- observed_mask(values)
  registry <- sample_registry(ids, c("control", "treatment", "control", "treatment"))
  study <- assemble_blocks(registry, list(alpha = values), list(alpha = mask))
  ops <- triomics_metabolomics_operations(impute = "training_median")
  plan <- triomics_finalize_split_plan(list(
    schema = "SplitPlan", seed = 11L, grouping_unit = "subject", target = "synthetic",
    stratification = "none", temporal_order = NULL, n_outer = 2L, n_inner = 2L,
    unit_subject = data.frame(unit_id = ids, subject_id = ids, stringsAsFactors = FALSE),
    folds = list(
      list(
        fold_id = "outer_01", level = "outer", parent_fold_id = NULL,
        training_unit_ids = c("s1", "s2", "s3"), test_unit_ids = "s4",
        training_subject_ids = c("s1", "s2", "s3"), test_subject_ids = "s4",
        inner = list(list(
          fold_id = "outer_01__inner_01", level = "inner", parent_fold_id = "outer_01",
          training_unit_ids = c("s1", "s2"), test_unit_ids = "s3",
          training_subject_ids = c("s1", "s2"), test_subject_ids = "s3", inner = list()
        ))
      ),
      list(
        fold_id = "outer_02", level = "outer", parent_fold_id = NULL,
        training_unit_ids = c("s1", "s3", "s4"), test_unit_ids = "s2",
        training_subject_ids = c("s1", "s3", "s4"), test_subject_ids = "s2",
        inner = list(list(
          fold_id = "outer_02__inner_01", level = "inner", parent_fold_id = "outer_02",
          training_unit_ids = c("s1", "s3"), test_unit_ids = "s4",
          training_subject_ids = c("s1", "s3"), test_subject_ids = "s4", inner = list()
        ))
      )
    )
  ))
  dir <- new_dir("a-t07-ac2-")
  prepared <- triomics_fold_prepare(
    study,
    spec_for(dir, ops, "predictive", NULL, "alpha"),
    plan,
    spec_for(dir, descriptive_ops(ops), "descriptive", ids, "alpha")
  )
  fold <- find_fold(prepared, "outer_01")
  state_path <- file.path(dir, fold$blocks$alpha$state_ref$path)
  before <- triomics_sha256(state_path)
  apply_values <- values
  apply_values["f1", "s4"] <- NA_real_
  apply_mask <- observed_mask(apply_values)
  apply_study <- assemble_blocks(registry, list(alpha = apply_values), list(alpha = apply_mask))
  applied <- triomics_fold_prepare_apply(apply_study, spec_for(dir, ops, "predictive", NULL, "alpha"), fold)
  got <- SummarizedExperiment::assay(applied$study$experiments$alpha, "analysis")
  testthat::expect_equal(got["f1", "s4"], 3, tolerance = 1e-8)
  testthat::expect_equal(got["f1", "s2"], 3, tolerance = 1e-8)
  testthat::expect_false(isTRUE(all.equal(got["f1", "s4"], stats::median(c(2, 4, 100)))))
  reread <- triomics_read_state(fold$blocks$alpha$state_ref, dir)
  testthat::expect_identical(as.character(reread$training_unit_ids), c("s1", "s2", "s3"))
  testthat::expect_false("s4" %in% as.character(reread$training_unit_ids))
  testthat::expect_identical(triomics_sha256(state_path), before)
  testthat::expect_identical(applied$state_hash, fold$blocks$alpha$state_hash)

  permuted <- apply_study
  permuted$experiments$alpha <- permuted$experiments$alpha[c("f2", "f1"), , drop = FALSE]
  names_before <- rownames(permuted$experiments$alpha)
  reorder <- expect_code(
    triomics_fold_prepare_apply(permuted, spec_for(dir, ops, "predictive", NULL, "alpha"), fold),
    "A_STATE_RESELECT"
  )
  testthat::expect_match(conditionMessage(reorder), "feature order", fixed = TRUE)
  testthat::expect_identical(rownames(permuted$experiments$alpha), names_before)
  testthat::expect_identical(triomics_sha256(state_path), before)
  reread_keys <- as.character(triomics_read_state(fold$blocks$alpha$state_ref, dir)$feature_keys)
  testthat::expect_identical(reread_keys, c("f1", "f2"))

  dropped <- apply_study
  dropped$experiments$alpha <- dropped$experiments$alpha["f1", , drop = FALSE]
  missing <- expect_code(
    triomics_fold_prepare_apply(dropped, spec_for(dir, ops, "predictive", NULL, "alpha"), fold),
    "A_STATE_FEATURES"
  )
  testthat::expect_match(conditionMessage(missing), "reselect", fixed = TRUE)
  testthat::expect_identical(triomics_sha256(state_path), before)
})

testthat::test_that("A-T07-AC3 missing cells and absent views use profiles and reject non-inductive prediction", {
  register_profiles()
  ids <- c("s1", "s2", "s3", "s4")
  alpha <- matrix(
    c(2, NA, 1e9, 4, 10, 10, 10, 10),
    nrow = 2, byrow = TRUE, dimnames = list(c("f1", "f2"), ids)
  )
  beta <- matrix(c(5, 7, 9), nrow = 1, dimnames = list("g1", c("s1", "s2", "s4")))
  registry <- sample_registry(ids, c("control", "treatment", "control", "treatment"))
  study <- assemble_blocks(
    registry,
    list(alpha = alpha, beta = beta),
    list(alpha = observed_mask(alpha), beta = observed_mask(beta)),
    alignment = "union",
    absent = list(beta = "s3")
  )
  ops <- triomics_metabolomics_operations(impute = "training_median")
  plan <- triomics_finalize_split_plan(list(
    schema = "SplitPlan", seed = 11L, grouping_unit = "subject", target = "synthetic",
    stratification = "none", temporal_order = NULL, n_outer = 2L, n_inner = 2L,
    unit_subject = data.frame(unit_id = ids, subject_id = ids, stringsAsFactors = FALSE),
    folds = list(
      list(
        fold_id = "outer_01", level = "outer", parent_fold_id = NULL,
        training_unit_ids = c("s1", "s2", "s4"), test_unit_ids = "s3",
        training_subject_ids = c("s1", "s2", "s4"), test_subject_ids = "s3",
        inner = list(list(
          fold_id = "outer_01__inner_01", level = "inner", parent_fold_id = "outer_01",
          training_unit_ids = c("s1", "s4"), test_unit_ids = "s2",
          training_subject_ids = c("s1", "s4"), test_subject_ids = "s2", inner = list()
        ))
      ),
      list(
        fold_id = "outer_02", level = "outer", parent_fold_id = NULL,
        training_unit_ids = c("s1", "s3", "s4"), test_unit_ids = "s2",
        training_subject_ids = c("s1", "s3", "s4"), test_subject_ids = "s2",
        inner = list(list(
          fold_id = "outer_02__inner_01", level = "inner", parent_fold_id = "outer_02",
          training_unit_ids = c("s1", "s4"), test_unit_ids = "s3",
          training_subject_ids = c("s1", "s4"), test_subject_ids = "s3", inner = list()
        ))
      )
    )
  ))
  dir <- new_dir("a-t07-ac3-")
  before <- SummarizedExperiment::assay(study$experiments$alpha, "analysis")
  prepared <- triomics_fold_prepare(
    study,
    spec_for(dir, ops, "predictive", NULL, c("alpha", "beta")),
    plan,
    spec_for(dir, descriptive_ops(ops), "descriptive", ids, c("alpha", "beta"))
  )
  fold <- find_fold(prepared, "outer_01")
  testthat::expect_identical(fold$blocks$alpha$imputation_method, "training_median")
  testthat::expect_equal(fold$blocks$alpha$medians[["f1"]], 3, tolerance = 1e-8)
  beta_context <- fold$contexts$B$blocks$beta
  alpha_context <- fold$contexts$B$blocks$alpha
  testthat::expect_false("s3" %in% colnames(beta_context$values))
  testthat::expect_false("s3" %in% fold$blocks$beta$training_unit_ids)
  disposition <- fold$view_disposition
  absent <- disposition[disposition$block_id == "beta" & disposition$sample_id == "s3", , drop = FALSE]
  testthat::expect_identical(as.character(absent$presence), "structural_absence")
  testthat::expect_false(isTRUE(absent$imputed[[1L]]))
  testthat::expect_equal(alpha_context$values["f1", "s2"], 3, tolerance = 1e-8)
  testthat::expect_identical(alpha_context$mask["f1", "s2"], "known_imputed")
  testthat::expect_identical(fold$contexts$C$blocks$alpha$mask["f1", "s2"], "known_imputed")
  testthat::expect_equal(
    SummarizedExperiment::assay(study$experiments$alpha, "analysis")["f1", "s3"],
    before["f1", "s3"]
  )
  testthat::expect_true(is.na(before["f1", "s2"]))

  minprob <- triomics_proteomics_operations(
    impute = "minprob",
    missingness_assumption = "mnar_left_censored",
    missingness_evidence = "Synthetic left-censoring fixture."
  )
  testthat::expect_true(all(vapply(minprob, function(op) identical(op$apply, "unavailable"), logical(1L))))
  proteomics <- study
  proteomics$experiments <- list(alpha = study$experiments$alpha)
  proteomics$mae <- triomics_mae_from_experiments(proteomics)
  rejected <- spec_for(new_dir("a-t07-minprob-"), minprob, "predictive", NULL, "alpha")
  rejected$blocks[[1L]]$profile_id <- "proteomics"
  descriptive <- spec_for(rejected$state_dir, descriptive_ops(minprob), "descriptive", ids, "alpha")
  descriptive$blocks[[1L]]$profile_id <- "proteomics"
  err <- expect_code(triomics_fold_prepare(proteomics, rejected, plan, descriptive), "A_STATE_PREDICT")
  testthat::expect_match(conditionMessage(err), "unsupported prediction", fixed = TRUE)
  testthat::expect_true(is.na(SummarizedExperiment::assay(proteomics$experiments$alpha, "analysis")["f1", "s2"]))
  testthat::expect_false(isTRUE(all.equal(
    SummarizedExperiment::assay(proteomics$experiments$alpha, "analysis")["f1", "s2"], 3
  )))
  testthat::expect_length(list.files(rejected$state_dir, pattern = "\\.json$"), 0L)

  stripped <- lapply(minprob, function(op) {
    op$apply <- NULL
    if (identical(op$stage, "impute")) {
      op$qualification <- list(inductive = FALSE, fit_apply = FALSE, method_id = "minprob")
    }
    op
  })
  stripped_dir <- new_dir("a-t07-stripped-")
  stripped_spec <- spec_for(stripped_dir, stripped, "predictive-stripped", NULL, "alpha")
  stripped_spec$blocks[[1L]]$profile_id <- "proteomics"
  stripped_desc <- spec_for(stripped_dir, descriptive_ops(stripped), "descriptive-stripped", ids, "alpha")
  stripped_desc$blocks[[1L]]$profile_id <- "proteomics"
  stripped_err <- expect_code(
    triomics_fold_prepare(proteomics, stripped_spec, plan, stripped_desc),
    "A_STATE_PREDICT"
  )
  testthat::expect_match(conditionMessage(stripped_err), "unsupported prediction", fixed = TRUE)
  testthat::expect_true(is.na(SummarizedExperiment::assay(proteomics$experiments$alpha, "analysis")["f1", "s2"]))
})

testthat::test_that("A-T07-AC4 fold B and C share values while primary stays unimputed", {
  register_profiles()
  ids <- c("s1", "s2", "s3", "s4")
  values <- matrix(
    c(1, NA, 3, 5, 4, 4, 4, 4),
    nrow = 2, byrow = TRUE, dimnames = list(c("f1", "f2"), ids)
  )
  registry <- sample_registry(ids, c("control", "treatment", "control", "treatment"))
  study <- assemble_blocks(registry, list(alpha = values), list(alpha = observed_mask(values)))
  ops <- triomics_metabolomics_operations(impute = "training_median")
  plan <- triomics_finalize_split_plan(list(
    schema = "SplitPlan", seed = 11L, grouping_unit = "subject", target = "synthetic",
    stratification = "none", temporal_order = NULL, n_outer = 2L, n_inner = 2L,
    unit_subject = data.frame(unit_id = ids, subject_id = ids, stringsAsFactors = FALSE),
    folds = list(
      list(
        fold_id = "outer_01", level = "outer", parent_fold_id = NULL,
        training_unit_ids = c("s1", "s2", "s3"), test_unit_ids = "s4",
        training_subject_ids = c("s1", "s2", "s3"), test_subject_ids = "s4",
        inner = list(list(
          fold_id = "outer_01__inner_01", level = "inner", parent_fold_id = "outer_01",
          training_unit_ids = c("s1", "s3"), test_unit_ids = "s2",
          training_subject_ids = c("s1", "s3"), test_subject_ids = "s2", inner = list()
        ))
      ),
      list(
        fold_id = "outer_02", level = "outer", parent_fold_id = NULL,
        training_unit_ids = c("s1", "s3", "s4"), test_unit_ids = "s2",
        training_subject_ids = c("s1", "s3", "s4"), test_subject_ids = "s2",
        inner = list(list(
          fold_id = "outer_02__inner_01", level = "inner", parent_fold_id = "outer_02",
          training_unit_ids = c("s1", "s4"), test_unit_ids = "s3",
          training_subject_ids = c("s1", "s4"), test_subject_ids = "s3", inner = list()
        ))
      )
    )
  ))
  dir <- new_dir("a-t07-ac4-")
  same <- expect_code(
    triomics_fold_prepare(
      study,
      spec_for(dir, ops, "shared", NULL, "alpha"),
      plan,
      spec_for(dir, descriptive_ops(ops), "shared", ids, "alpha")
    ),
    "A_STATE_CONTEXT"
  )
  testthat::expect_match(conditionMessage(same), "primary descriptive", fixed = TRUE)
  imputed_primary <- expect_code(
    triomics_fold_prepare(
      study,
      spec_for(dir, ops, "predictive", NULL, "alpha"),
      plan,
      spec_for(dir, ops, "also-imputed", ids, "alpha")
    ),
    "A_STATE_CONTEXT"
  )
  testthat::expect_match(conditionMessage(imputed_primary), "imputed", fixed = TRUE)

  prepared <- triomics_fold_prepare(
    study,
    spec_for(dir, ops, "predictive", NULL, "alpha"),
    plan,
    spec_for(dir, descriptive_ops(ops), "descriptive", ids, "alpha")
  )
  fold <- find_fold(prepared, "outer_01")
  b_block <- fold$contexts$B$blocks$alpha
  c_block <- fold$contexts$C$blocks$alpha
  primary <- prepared$primary$blocks$alpha
  testthat::expect_false(prepared$primary$imputed)
  testthat::expect_identical(prepared$primary$role, "descriptive_inferential")
  testthat::expect_identical(fold$contexts$B$role, "fold_prediction")
  testthat::expect_identical(fold$contexts$B$preparation_context_id, fold$contexts$C$preparation_context_id)
  testthat::expect_false(identical(prepared$primary$preparation_context_id, fold$contexts$B$preparation_context_id))
  testthat::expect_identical(b_block$values, c_block$values)
  testthat::expect_identical(b_block$sample_map, c_block$sample_map)
  testthat::expect_identical(b_block$unit_map, c_block$unit_map)
  testthat::expect_identical(b_block$values_hash, c_block$values_hash)
  testthat::expect_identical(b_block$map_hash, c_block$map_hash)
  testthat::expect_identical(b_block$state_hash, fold$blocks$alpha$state_hash)
  testthat::expect_equal(b_block$values["f1", "s2"], 2, tolerance = 1e-8)
  testthat::expect_equal(b_block$values["f1", "s2"], fold$blocks$alpha$medians[["f1"]], tolerance = 1e-8)
  testthat::expect_true(is.na(primary$values["f1", "s2"]))
  testthat::expect_identical(primary$mask["f1", "s2"], "known_missing")
  testthat::expect_identical(b_block$mask["f1", "s2"], "known_imputed")
  testthat::expect_false(identical(primary$state_hash, fold$blocks$alpha$state_hash))
  testthat::expect_false(identical(primary$values_hash, b_block$values_hash))
})
