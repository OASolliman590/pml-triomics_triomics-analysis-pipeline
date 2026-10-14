expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

sample_registry <- function(ids) {
  data.frame(
    sample_id = ids,
    sample_role = rep("undeclared", length(ids)),
    stringsAsFactors = FALSE
  )
}

feature_rows <- function(block_id, keys, quantity = "intensity", scale = "linear",
                         units = "synthetic_arbitrary_units") {
  n <- length(keys)
  data.frame(
    study_id = rep("study", n),
    block_id = rep(block_id, n),
    feature_key = keys,
    original_id = rep("PC 34:1", n),
    id_namespace = rep("synthetic", n),
    source_id = rep(block_id, n),
    source_row = as.character(seq_len(n)),
    measured_or_derived = rep("measured", n),
    quantity = rep(quantity, n),
    scale = rep(scale, n),
    units = rep(units, n),
    structural_resolution = rep("sum_composition", n),
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
  matrix("known_observed", nrow(assay), ncol(assay), dimnames = dimnames(assay))
}

assemble_lipid <- function(values, mask = NULL, block_id = "alpha", display = "lipid panel",
                           scale = "linear", quantity = "intensity",
                           units = "synthetic_arbitrary_units") {
  if (is.null(mask)) mask <- observed_mask(values)
  registry <- sample_registry(colnames(values))
  raw <- list(
    study_id = "study",
    sample_registry = registry,
    feature_registry = feature_rows(block_id, rownames(values), quantity, scale, units),
    sample_maps = stats::setNames(list(named_map(block_id, colnames(values))), block_id),
    assays = stats::setNames(list(values), block_id),
    original_masks = stats::setNames(list(mask), block_id),
    availability = data.frame(
      study_id = "study", block_id = block_id, sample_id = registry$sample_id,
      presence = "present", cell_imputation_eligible = FALSE, stringsAsFactors = FALSE
    ),
    source_manifests = list(),
    selection = data.frame(),
    column_accounting = data.frame(),
    clinical_observations = list(),
    upstream_steps = stats::setNames(list(character()), block_id)
  )
  triomics_assemble_study(raw, display_names = stats::setNames(display, block_id))
}

new_dir <- function(prefix) {
  path <- tempfile(prefix)
  dir.create(path)
  path
}

fit_spec <- function(dir, training, operations, context = "ctx", block_id = "alpha") {
  list(
    preparation_context_id = context,
    training_unit_ids = training,
    seed = 7L,
    state_dir = dir,
    blocks = list(list(
      block_id = block_id,
      profile_id = "lipidomics",
      operations = operations
    ))
  )
}

register_lipidomics <- function() {
  triomics_reset_profiles()
  triomics_register_lipidomics_profile()
}

pc_parents <- function(chain = c("16:0/18:1", "18:0_should_not_leak"),
                       resolution = c("sum_composition", "sum_composition"),
                       quantity = c("intensity", "intensity"),
                       calibration = c("not_required", "not_required"),
                       units = c("synthetic_arbitrary_units", "synthetic_arbitrary_units"),
                       carbons = c(16, 18), double_bonds = c(0, 1)) {
  data.frame(
    feature_key = c("p1", "p2"),
    class_id = c("PC", "PC"),
    carbons = carbons,
    double_bonds = double_bonds,
    structural_resolution = resolution,
    quantity = quantity,
    units = units,
    scale = c("linear", "linear"),
    calibration_status = calibration,
    compatible = c(TRUE, TRUE),
    chain_label = chain,
    stringsAsFactors = FALSE
  )
}

derive_request <- function(method, policy, rule, id = "pc_total", parents = pc_parents(), extra = list()) {
  request <- list(
    endpoint_id = id,
    method = method,
    parent_set_ref = "universe:pc",
    definition_ref = paste0("def:", method),
    quantity_rule = rule,
    missing_parent_policy = policy,
    parents = parents
  )
  for (name in names(extra)) request[[name]] <- extra[[name]]
  request
}

analysis_of <- function(study, block_id = "alpha") {
  SummarizedExperiment::assay(study$experiments[[block_id]], "analysis")
}

snapshot_block <- function(study, block_id = "alpha") {
  mat <- analysis_of(study, block_id)
  matrix(as.numeric(mat), nrow = nrow(mat), dimnames = dimnames(mat))
}

sample_record <- function(study, feature_key, sample_id, block_id = "alpha") {
  triomics_lipid_endpoint_sample(study$experiments[[block_id]], feature_key, sample_id)
}

endpoint_field <- function(study, feature_key, field, block_id = "alpha") {
  found <- sample_record(study, feature_key, colnames(analysis_of(study, block_id))[[1L]], block_id)
  testthat::expect_false(is.null(found))
  found$endpoint[[field]]
}

two_by_two <- function(values = c(2, 3, 2, 3)) {
  matrix(values, nrow = 2, dimnames = list(c("p1", "p2"), c("s1", "s2")))
}

testthat::test_that("A-T06-AC1 class totals sum linear parents over a fixed universe", {
  register_lipidomics()
  values <- two_by_two()
  study <- assemble_lipid(values, display = "not a method name")
  ops <- triomics_lipidomics_operations(
    transform = "log2", zero_policy = "error",
    derive = derive_request("class_sum", "unavailable", "linear_sum")
  )
  dir <- new_dir("a-t06-sum-")
  fitted <- prepare_fit(study, fit_spec(dir, c("s1", "s2"), ops, "linear-sum"))
  logged_sum <- log2(2) + log2(3)
  for (sample_id in c("s1", "s2")) {
    record <- sample_record(fitted, "pc_total", sample_id)
    testthat::expect_equal(record$sample$value, 5, tolerance = 1e-8)
    testthat::expect_gt(abs(record$sample$value - logged_sum), 1e-4)
    testthat::expect_identical(record$sample$coverage, "complete")
    testthat::expect_identical(record$sample$n_expected, 2)
    testthat::expect_identical(record$endpoint$scale, "linear")
    testthat::expect_identical(record$endpoint$absolute_concentration, FALSE)
    testthat::expect_identical(record$endpoint$quantity, "intensity")
  }
  assay <- snapshot_block(fitted)
  testthat::expect_equal(assay["p1", "s1"], log2(2), tolerance = 1e-8)
  testthat::expect_equal(assay["p2", "s1"], log2(3), tolerance = 1e-8)
  state <- triomics_read_state(fitted$state_refs$alpha, dir)
  testthat::expect_identical(state$learned_parameters$lipid_derivation$method, "class_sum")
  testthat::expect_identical(state$learned_parameters$lipid_derivation$absolute_concentration, FALSE)
  stages <- vapply(fitted$operation_ledger$alpha, function(op) op$stage, character(1))
  testthat::expect_true(which(stages == "derive") < which(stages == "transform"))

  extra <- rbind(values, rogue = c(100, 100))
  rownames(extra) <- c("p1", "p2", "rogue")
  rogue <- prepare_fit(
    assemble_lipid(extra),
    fit_spec(new_dir("a-t06-rogue-"), c("s1", "s2"), ops, "rogue")
  )
  testthat::expect_equal(sample_record(rogue, "pc_total", "s1")$sample$value, 5, tolerance = 1e-8)
  below <- matrix(c(0.2, 0.3, 0.2, 0.3), nrow = 2, dimnames = dimnames(values))
  small <- prepare_fit(
    assemble_lipid(below),
    fit_spec(new_dir("a-t06-small-"), c("s1", "s2"), ops, "small")
  )
  testthat::expect_equal(sample_record(small, "pc_total", "s1")$sample$value, 0.5, tolerance = 1e-8)
  testthat::expect_gt(sample_record(small, "pc_total", "s1")$sample$value, 0.4)

  duplicate <- values
  rownames(duplicate) <- c("adduct_a", "adduct_b")
  dup_parents <- pc_parents()
  dup_parents$feature_key <- c("adduct_a", "adduct_b")
  dup_ops <- triomics_lipidomics_operations(
    derive = derive_request("class_sum", "unavailable", "linear_sum", parents = dup_parents)
  )
  duplicated_fit <- prepare_fit(
    assemble_lipid(duplicate),
    fit_spec(new_dir("a-t06-adduct-"), c("s1", "s2"), dup_ops, "adduct")
  )
  testthat::expect_equal(sample_record(duplicated_fit, "pc_total", "s1")$sample$value, 5, tolerance = 1e-8)

  log_values <- matrix(c(log2(2), log2(3), log2(2), log2(3)), nrow = 2, dimnames = dimnames(values))
  log_input <- assemble_lipid(log_values, scale = "log2")
  before_log <- snapshot_block(log_input)
  log_err <- expect_code(
    prepare_fit(log_input, fit_spec(
      new_dir("a-t06-log-"), c("s1", "s2"),
      triomics_lipidomics_operations(derive = derive_request("class_sum", "unavailable", "linear_sum")),
      "log-input"
    )),
    "A_SCALE_INPUT"
  )
  testthat::expect_match(conditionMessage(log_err), "log", ignore.case = TRUE)
  testthat::expect_equal(snapshot_block(log_input), before_log, tolerance = 1e-8)
  rule_err <- expect_code(
    prepare_fit(assemble_lipid(values), fit_spec(
      new_dir("a-t06-rule-"), c("s1", "s2"),
      triomics_lipidomics_operations(derive = derive_request("class_sum", "unavailable", "sum_of_logs")),
      "sum-of-logs"
    )),
    "A_SCALE_INPUT"
  )
  testthat::expect_match(conditionMessage(rule_err), "log", ignore.case = TRUE)

  partial_values <- matrix(c(2, NA, 4, NA), nrow = 2, dimnames = dimnames(values))
  partial_mask <- observed_mask(partial_values)
  partial_mask["p2", ] <- "known_missing"
  partial_ops <- triomics_lipidomics_operations(
    filter_method = "prevalence", min_observed = 1L,
    derive = derive_request("class_sum", "partial", "linear_sum")
  )
  partial_fit <- prepare_fit(
    assemble_lipid(partial_values, partial_mask),
    fit_spec(new_dir("a-t06-partial-"), c("s1", "s2"), partial_ops, "partial")
  )
  partial_record <- sample_record(partial_fit, "pc_total", "s1")
  testthat::expect_equal(partial_record$sample$value, 2, tolerance = 1e-8)
  testthat::expect_identical(partial_record$sample$coverage, "partial")
  testthat::expect_identical(partial_record$sample$n_expected, 2)
  testthat::expect_identical(partial_record$sample$n_observed, 1)
  testthat::expect_false(identical(partial_record$sample$coverage, "complete"))
  testthat::expect_false("p2" %in% rownames(analysis_of(partial_fit)))

  unavailable_ops <- triomics_lipidomics_operations(
    filter_method = "prevalence", min_observed = 1L,
    derive = derive_request("class_sum", "unavailable", "linear_sum")
  )
  unavailable_fit <- prepare_fit(
    assemble_lipid(partial_values, partial_mask),
    fit_spec(new_dir("a-t06-unavail-"), c("s1", "s2"), unavailable_ops, "unavailable")
  )
  unavailable_record <- sample_record(unavailable_fit, "pc_total", "s1")
  testthat::expect_true(is.na(unavailable_record$sample$value))
  testthat::expect_false(is.nan(unavailable_record$sample$value))
  testthat::expect_identical(unavailable_record$sample$coverage, "unavailable")
  testthat::expect_identical(unavailable_record$sample$n_expected, 2)

  estimated_values <- partial_values
  estimated_values["p2", ] <- c(3, 3)
  estimated_mask <- observed_mask(estimated_values)
  estimated_mask["p2", ] <- "known_imputed"
  estimated_ops <- triomics_lipidomics_operations(
    derive = derive_request("class_sum", "qualified_estimate", "linear_sum")
  )
  estimated_fit <- prepare_fit(
    assemble_lipid(estimated_values, estimated_mask),
    fit_spec(new_dir("a-t06-est-"), c("s1", "s2"), estimated_ops, "estimated")
  )
  estimated_record <- sample_record(estimated_fit, "pc_total", "s1")
  testthat::expect_equal(estimated_record$sample$value, 5, tolerance = 1e-8)
  testthat::expect_identical(estimated_record$sample$coverage, "estimated")
  testthat::expect_equal(estimated_record$sample$observed_contribution, 2, tolerance = 1e-8)
  testthat::expect_equal(estimated_record$sample$imputed_contribution, 3, tolerance = 1e-8)
  testthat::expect_false(identical(estimated_record$sample$coverage, "complete"))

  partial_imputed_ops <- triomics_lipidomics_operations(
    derive = derive_request("class_sum", "partial", "linear_sum")
  )
  partial_imputed <- prepare_fit(
    assemble_lipid(estimated_values, estimated_mask),
    fit_spec(new_dir("a-t06-pimp-"), c("s1", "s2"), partial_imputed_ops, "partial-imputed")
  )
  partial_imputed_record <- sample_record(partial_imputed, "pc_total", "s1")
  testthat::expect_equal(partial_imputed_record$sample$value, 2, tolerance = 1e-8)
  testthat::expect_identical(partial_imputed_record$sample$coverage, "partial")
  testthat::expect_equal(partial_imputed_record$sample$imputed_contribution, 0, tolerance = 1e-8)

  unknown_mask <- observed_mask(values)
  unknown_mask["p2", "s1"] <- "unknown"
  unknown_fit <- prepare_fit(
    assemble_lipid(values, unknown_mask),
    fit_spec(
      new_dir("a-t06-unknown-"), c("s1", "s2"),
      triomics_lipidomics_operations(derive = derive_request("class_sum", "partial", "linear_sum")),
      "unknown-mask"
    )
  )
  unknown_record <- sample_record(unknown_fit, "pc_total", "s1")
  testthat::expect_true(is.na(unknown_record$sample$value))
  testthat::expect_identical(unknown_record$sample$coverage, "unavailable")
  testthat::expect_identical(unknown_record$sample$reason, "unknown_mask")

  negative <- values
  negative["p2", "s1"] <- -3
  negative_fit <- prepare_fit(
    assemble_lipid(negative),
    fit_spec(
      new_dir("a-t06-neg-"), c("s1", "s2"),
      triomics_lipidomics_operations(derive = derive_request("class_sum", "partial", "linear_sum")),
      "negative"
    )
  )
  negative_record <- sample_record(negative_fit, "pc_total", "s1")
  testthat::expect_true(is.na(negative_record$sample$value))
  testthat::expect_identical(negative_record$sample$coverage, "unavailable")
  testthat::expect_false(isTRUE(all.equal(negative_record$sample$value, -1)))

  held_values <- values
  held_values[, "s2"] <- c(20, 30)
  held <- prepare_fit(
    assemble_lipid(held_values),
    fit_spec(new_dir("a-t06-held-"), "s1", ops, "held")
  )
  mutated_values <- held_values
  mutated_values[, "s2"] <- c(100, 100)
  mutated <- prepare_fit(
    assemble_lipid(mutated_values, display = "renamed lipids"),
    fit_spec(new_dir("a-t06-held2-"), "s1", ops, "held-2")
  )
  testthat::expect_equal(sample_record(held, "pc_total", "s1")$sample$value, 5, tolerance = 1e-8)
  testthat::expect_equal(sample_record(mutated, "pc_total", "s1")$sample$value, 5, tolerance = 1e-8)
  testthat::expect_identical(
    sample_record(held, "pc_total", "s1")$endpoint$universe_hash,
    sample_record(mutated, "pc_total", "s1")$endpoint$universe_hash
  )
  testthat::expect_equal(sample_record(mutated, "pc_total", "s2")$sample$value, 200, tolerance = 1e-8)
})

testthat::test_that("A-T06-AC2 fractions and CLR keep an explicit denominator and the class total", {
  register_lipidomics()
  values <- two_by_two()
  fraction_request <- derive_request(
    "within_class_fraction", "unavailable", "within_class_fraction", id = "pc_fraction",
    extra = list(
      denominator = "linear_sum_of_parent_set:universe:pc",
      units = "fraction",
      zero_policy = "unavailable"
    )
  )
  ops <- triomics_lipidomics_operations(derive = fraction_request)
  fitted <- prepare_fit(
    assemble_lipid(values),
    fit_spec(new_dir("a-t06-frac-"), c("s1", "s2"), ops, "fraction")
  )
  testthat::expect_equal(snapshot_block(fitted), values, tolerance = 1e-8)
  p1 <- sample_record(fitted, "pc_fraction::p1", "s1")
  p2 <- sample_record(fitted, "pc_fraction::p2", "s1")
  total <- sample_record(fitted, "pc_fraction::class_total", "s1")
  testthat::expect_equal(p1$sample$value, 0.4, tolerance = 1e-8)
  testthat::expect_equal(p2$sample$value, 0.6, tolerance = 1e-8)
  testthat::expect_equal(total$sample$value, 5, tolerance = 1e-8)
  testthat::expect_identical(p1$endpoint$denominator, "linear_sum_of_parent_set:universe:pc")
  testthat::expect_identical(p1$endpoint$units, "fraction")
  testthat::expect_identical(p1$endpoint$zero_policy, "unavailable")
  testthat::expect_identical(p1$endpoint$class_total_retained, TRUE)
  testthat::expect_identical(p1$endpoint$absolute_concentration, FALSE)
  testthat::expect_false(is.null(p1$endpoint$disclosure))
  testthat::expect_match(p1$endpoint$disclosure, "class total", ignore.case = TRUE)
  testthat::expect_false(identical(p1$endpoint$estimand, "abundance"))

  missing_policy <- fraction_request
  missing_policy$zero_policy <- NULL
  expect_code(
    prepare_fit(
      assemble_lipid(values),
      fit_spec(
        new_dir("a-t06-frac-zero-"), c("s1", "s2"),
        triomics_lipidomics_operations(derive = missing_policy),
        "fraction-zero-policy"
      )
    ),
    "A_CONFIG_OPERATION"
  )
  replacement <- fraction_request
  replacement$zero_policy <- "declared_replacement"
  replacement$zero_value <- 1e-6
  replace_err <- expect_code(
    prepare_fit(
      assemble_lipid(values),
      fit_spec(
        new_dir("a-t06-pseudo-"), c("s1", "s2"),
        triomics_lipidomics_operations(derive = replacement),
        "pseudocount"
      )
    ),
    "A_CONFIG_OPERATION"
  )
  testthat::expect_match(conditionMessage(replace_err), "pseudocount|zero", ignore.case = TRUE)
  erase <- fraction_request
  erase$erase_class_total <- TRUE
  expect_code(
    prepare_fit(
      assemble_lipid(values),
      fit_spec(
        new_dir("a-t06-erase-"), c("s1", "s2"),
        triomics_lipidomics_operations(derive = erase),
        "erase"
      )
    ),
    "A_CONFIG_OPERATION"
  )

  zeros <- matrix(c(0, 0, 2, 3), nrow = 2, dimnames = dimnames(values))
  zero_fit <- prepare_fit(
    assemble_lipid(zeros),
    fit_spec(new_dir("a-t06-zero-den-"), c("s1", "s2"), ops, "zero-denominator")
  )
  zero_fraction <- sample_record(zero_fit, "pc_fraction::p1", "s1")
  zero_total <- sample_record(zero_fit, "pc_fraction::class_total", "s1")
  testthat::expect_true(is.na(zero_fraction$sample$value))
  testthat::expect_false(is.nan(zero_fraction$sample$value))
  testthat::expect_identical(zero_fraction$sample$coverage, "unavailable")
  testthat::expect_identical(zero_fraction$sample$reason, "zero_denominator")
  testthat::expect_equal(zero_total$sample$value, 0, tolerance = 1e-8)
  testthat::expect_identical(zero_total$endpoint$class_total_retained, TRUE)

  partial_values <- matrix(c(2, NA, 2, NA), nrow = 2, dimnames = dimnames(values))
  partial_mask <- observed_mask(partial_values)
  partial_mask["p2", ] <- "known_missing"
  partial_request <- fraction_request
  partial_request$missing_parent_policy <- "partial"
  partial_fit <- prepare_fit(
    assemble_lipid(partial_values, partial_mask),
    fit_spec(
      new_dir("a-t06-frac-partial-"), c("s1", "s2"),
      triomics_lipidomics_operations(derive = partial_request),
      "fraction-partial"
    )
  )
  partial_p1 <- sample_record(partial_fit, "pc_fraction::p1", "s1")
  testthat::expect_equal(partial_p1$sample$value, 1, tolerance = 1e-8)
  testthat::expect_identical(partial_p1$sample$coverage, "partial")
  testthat::expect_false(identical(partial_p1$sample$coverage, "complete"))
  testthat::expect_match(partial_p1$endpoint$disclosure, "partial", ignore.case = TRUE)

  clr_ops <- triomics_lipidomics_operations(
    transform = "clr", zero_policy = "error", log_base = 2,
    reference_set = "parts:pc", reference_features = c("p1", "p2")
  )
  clr_values <- matrix(c(4, 1, 4, 1), nrow = 2, dimnames = dimnames(values))
  clr_fit <- prepare_fit(
    assemble_lipid(clr_values),
    fit_spec(new_dir("a-t06-clr-"), c("s1", "s2"), clr_ops, "clr")
  )
  clr_assay <- snapshot_block(clr_fit)
  testthat::expect_equal(clr_assay["p1", "s1"], 1, tolerance = 1e-8)
  testthat::expect_equal(clr_assay["p2", "s1"], -1, tolerance = 1e-8)
  natural <- log(4 / sqrt(4)) 
  testthat::expect_gt(abs(clr_assay["p1", "s1"] - natural), 1e-4)
  record <- triomics_lipid_clr_record(clr_fit$experiments$alpha)
  testthat::expect_identical(record$estimand, "clr_log_ratio")
  testthat::expect_identical(record$abundance_estimand, FALSE)
  testthat::expect_identical(record$class_total_erased_from_assay, TRUE)
  testthat::expect_identical(record$abundance_retained_in_state, TRUE)
  testthat::expect_equal(record$abundance_before[["p1"]][["s1"]], 4, tolerance = 1e-8)
  testthat::expect_equal(record$class_total[["s1"]], 5, tolerance = 1e-8)
  testthat::expect_false(is.null(record$disclosure))
  testthat::expect_identical(record$log_base, 2)
  testthat::expect_identical(record$zero_policy, "error")
  testthat::expect_false(identical(
    as.character(SummarizedExperiment::rowData(clr_fit$experiments$alpha)$quantity[[1L]]),
    "concentration"
  ))

  zero_clr <- clr_values
  zero_clr["p1", "s1"] <- 0
  zero_study <- assemble_lipid(zero_clr)
  before_zero <- snapshot_block(zero_study)
  expect_code(
    prepare_fit(zero_study, fit_spec(new_dir("a-t06-clr-zero-"), c("s1", "s2"), clr_ops, "clr-zero")),
    "A_SCALE_ZERO"
  )
  testthat::expect_equal(snapshot_block(zero_study), before_zero, tolerance = 1e-8)
  silent <- clr_ops
  silent[[4L]]$zero_policy <- NULL
  expect_code(
    prepare_fit(
      assemble_lipid(clr_values),
      fit_spec(new_dir("a-t06-clr-policy-"), c("s1", "s2"), silent, "clr-policy")
    ),
    "A_CONFIG_OPERATION"
  )
  replaced <- clr_ops
  replaced[[4L]]$zero_policy <- "declared_replacement"
  replaced[[4L]]$zero_value <- NULL
  expect_code(
    prepare_fit(
      assemble_lipid(zero_clr),
      fit_spec(new_dir("a-t06-clr-missing-value-"), c("s1", "s2"), replaced, "clr-missing-value")
    ),
    "A_SCALE_ZERO"
  )
  ln_ops <- triomics_lipidomics_operations(
    transform = "clr", zero_policy = "error", log_base = exp(1),
    reference_set = "parts:pc", reference_features = c("p1", "p2")
  )
  ln_fit <- prepare_fit(
    assemble_lipid(clr_values),
    fit_spec(new_dir("a-t06-clr-ln-"), c("s1", "s2"), ln_ops, "clr-ln")
  )
  testthat::expect_equal(snapshot_block(ln_fit)["p1", "s1"], log(2), tolerance = 1e-8)
  testthat::expect_false(isTRUE(all.equal(snapshot_block(ln_fit)["p1", "s1"], 1)))
  testthat::expect_identical(triomics_lipid_clr_record(ln_fit$experiments$alpha)$output_scale, "clr_ln")
  testthat::expect_identical(triomics_lipid_clr_record(clr_fit$experiments$alpha)$output_scale, "clr_log2")
})

testthat::test_that("A-T06-AC3 weighted carbon and double-bond endpoints match the arithmetic oracle", {
  register_lipidomics()
  values <- two_by_two()
  carbon_ops <- triomics_lipidomics_operations(
    derive = derive_request("weighted_carbons", "unavailable", "abundance_weighted_mean", id = "pc_carbons")
  )
  carbon_fit <- prepare_fit(
    assemble_lipid(values),
    fit_spec(new_dir("a-t06-carbon-"), c("s1", "s2"), carbon_ops, "carbons")
  )
  carbon <- sample_record(carbon_fit, "pc_carbons", "s1")
  testthat::expect_equal(carbon$sample$value, 17.2, tolerance = 1e-8)
  testthat::expect_equal(carbon$sample$value, (2 * 16 + 3 * 18) / 5, tolerance = 1e-8)
  testthat::expect_gt(abs(carbon$sample$value - 17), 1e-6)
  testthat::expect_identical(carbon$sample$coverage, "complete")
  testthat::expect_identical(carbon$endpoint$quantity, "weighted_total_carbon_index")
  testthat::expect_identical(carbon$endpoint$units, "carbon_atoms")
  testthat::expect_identical(carbon$endpoint$sn_positions, "not_implied")
  testthat::expect_identical(carbon$endpoint$chain_identities, "not_implied")
  testthat::expect_identical(carbon$endpoint$structural_resolution, "sum_composition")
  leaked <- paste(unlist(carbon$endpoint), collapse = "\n")
  testthat::expect_false(grepl("16:0/18:1", leaked, fixed = TRUE))
  testthat::expect_false(grepl("18:0_should_not_leak", leaked, fixed = TRUE))

  bond_ops <- triomics_lipidomics_operations(
    derive = derive_request("weighted_double_bonds", "unavailable", "abundance_weighted_mean", id = "pc_db")
  )
  bond_fit <- prepare_fit(
    assemble_lipid(values),
    fit_spec(new_dir("a-t06-db-"), c("s1", "s2"), bond_ops, "double-bonds")
  )
  bond <- sample_record(bond_fit, "pc_db", "s1")
  testthat::expect_equal(bond$sample$value, 0.6, tolerance = 1e-8)
  testthat::expect_identical(bond$endpoint$quantity, "weighted_total_double_bond_index")
  testthat::expect_identical(bond$endpoint$units, "double_bonds")
  testthat::expect_identical(bond$endpoint$sn_positions, "not_implied")
  testthat::expect_identical(bond$endpoint$chain_identities, "not_implied")

  unresolved <- pc_parents()
  unresolved$carbons[[2L]] <- NA_real_
  unresolved_fit <- prepare_fit(
    assemble_lipid(values),
    fit_spec(
      new_dir("a-t06-unresolved-carbon-"), c("s1", "s2"),
      triomics_lipidomics_operations(
        derive = derive_request(
          "weighted_carbons", "partial", "abundance_weighted_mean",
          id = "pc_carbons", parents = unresolved
        )
      ),
      "unresolved-carbon"
    )
  )
  unresolved_record <- sample_record(unresolved_fit, "pc_carbons", "s1")
  testthat::expect_true(is.na(unresolved_record$sample$value))
  testthat::expect_false(is.nan(unresolved_record$sample$value))
  testthat::expect_identical(unresolved_record$sample$coverage, "unavailable")
  testthat::expect_identical(unresolved_record$sample$reason, "unresolved_property")
  testthat::expect_false(isTRUE(all.equal(unresolved_record$sample$value, 16)))

  zeros <- matrix(c(0, 0, 2, 3), nrow = 2, dimnames = dimnames(values))
  zero_fit <- prepare_fit(
    assemble_lipid(zeros),
    fit_spec(new_dir("a-t06-zero-weight-"), c("s1", "s2"), carbon_ops, "zero-weight")
  )
  zero_record <- sample_record(zero_fit, "pc_carbons", "s1")
  testthat::expect_true(is.na(zero_record$sample$value))
  testthat::expect_false(is.nan(zero_record$sample$value))
  testthat::expect_identical(zero_record$sample$coverage, "unavailable")
  testthat::expect_identical(zero_record$sample$reason, "zero_denominator")

  ratio_ops <- triomics_lipidomics_operations(
    derive = derive_request(
      "saturated_unsaturated_ratio", "unavailable", "saturated_over_unsaturated", id = "pc_ratio"
    )
  )
  ratio_fit <- prepare_fit(
    assemble_lipid(values),
    fit_spec(new_dir("a-t06-ratio-"), c("s1", "s2"), ratio_ops, "ratio")
  )
  ratio <- sample_record(ratio_fit, "pc_ratio", "s1")
  testthat::expect_equal(ratio$sample$value, 2 / 3, tolerance = 1e-8)
  testthat::expect_identical(ratio$endpoint$quantity, "saturated_to_unsaturated_ratio")
  testthat::expect_identical(ratio$endpoint$units, "dimensionless")
  unsat_zero <- matrix(c(2, 0, 2, 0), nrow = 2, dimnames = dimnames(values))
  unsat_fit <- prepare_fit(
    assemble_lipid(unsat_zero),
    fit_spec(new_dir("a-t06-ratio-zero-"), c("s1", "s2"), ratio_ops, "ratio-zero")
  )
  unsat_record <- sample_record(unsat_fit, "pc_ratio", "s1")
  testthat::expect_true(is.na(unsat_record$sample$value))
  testthat::expect_identical(unsat_record$sample$coverage, "unavailable")
  testthat::expect_identical(unsat_record$sample$reason, "zero_denominator")
  testthat::expect_false(is.infinite(unsat_record$sample$value))
})

testthat::test_that("A-T06-AC4 block rename is not a policy and unresolved calibration is not molar", {
  register_lipidomics()
  left <- triomics_lipidomics_policy_identity("alpha", "PC lipids")
  right <- triomics_lipidomics_policy_identity("renamed_block", "proteome totals")
  testthat::expect_identical(left, right)
  testthat::expect_identical(left$profile_id, "lipidomics")
  testthat::expect_false(identical(left$profile_id, "proteome totals"))

  values <- two_by_two()
  ops <- triomics_lipidomics_operations(
    derive = derive_request("class_sum", "unavailable", "linear_sum")
  )
  first <- prepare_fit(
    assemble_lipid(values, block_id = "alpha", display = "PC lipids"),
    fit_spec(new_dir("a-t06-name-a-"), c("s1", "s2"), ops, "name-a", "alpha")
  )
  second <- prepare_fit(
    assemble_lipid(values, block_id = "zzz", display = "make this molar"),
    fit_spec(new_dir("a-t06-name-b-"), c("s1", "s2"), ops, "name-b", "zzz")
  )
  testthat::expect_equal(
    sample_record(first, "pc_total", "s1")$sample$value,
    sample_record(second, "pc_total", "s1", "zzz")$sample$value,
    tolerance = 1e-8
  )
  testthat::expect_identical(
    sample_record(first, "pc_total", "s1")$endpoint$universe_hash,
    sample_record(second, "pc_total", "s1", "zzz")$endpoint$universe_hash
  )
  testthat::expect_identical(
    sample_record(second, "pc_total", "s1", "zzz")$endpoint$absolute_concentration,
    FALSE
  )
  named <- fit_spec(new_dir("a-t06-display-"), c("s1", "s2"), ops, "display")
  named$blocks[[1L]]$profile_id <- "make this molar"
  expect_code(prepare_fit(assemble_lipid(values, display = "make this molar"), named), "A_CONFIG_PROFILE")

  standards <- function(status, factor = 4) {
    list(list(
      feature_key = "analyte",
      standard_key = "istd",
      calibration_factor = factor,
      calibration_status = status,
      calibrated_quantity = "concentration",
      calibrated_units = "umol/L"
    ))
  }
  pair <- matrix(c(10, 2, 10, 2), nrow = 2, dimnames = list(c("analyte", "istd"), c("s1", "s2")))
  unresolved_ops <- triomics_lipidomics_operations(
    normalise = "internal_standard", standards_map = standards("unresolved", 4)
  )
  unresolved_study <- assemble_lipid(pair, display = "make this molar")
  before <- snapshot_block(unresolved_study)
  unresolved_err <- expect_code(
    prepare_fit(
      unresolved_study,
      fit_spec(new_dir("a-t06-istd-"), c("s1", "s2"), unresolved_ops, "unresolved-standard")
    ),
    "A_SCALE_INPUT"
  )
  testthat::expect_match(conditionMessage(unresolved_err), "molar|calibration", ignore.case = TRUE)
  testthat::expect_equal(snapshot_block(unresolved_study), before, tolerance = 1e-8)
  testthat::expect_false(isTRUE(all.equal(snapshot_block(unresolved_study)["analyte", "s1"], 20)))

  molar_parents <- pc_parents(quantity = c("intensity", "intensity"), calibration = c("unresolved", "unresolved"))
  molar_err <- expect_code(
    prepare_fit(
      assemble_lipid(values),
      fit_spec(
        new_dir("a-t06-molfrac-"), c("s1", "s2"),
        triomics_lipidomics_operations(derive = derive_request(
          "molar_fraction", "unavailable", "molar_fraction", id = "pc_mol",
          parents = molar_parents,
          extra = list(
            denominator = "linear_sum_of_parent_set:universe:pc",
            units = "mol_percent",
            zero_policy = "unavailable"
          )
        )),
        "molar-fraction"
      )
    ),
    "A_SCALE_INPUT"
  )
  testthat::expect_match(conditionMessage(molar_err), "molar|calibration", ignore.case = TRUE)

  amount_values <- matrix(c(10, 30, 20, 40), nrow = 2, dimnames = dimnames(values))
  amount_study <- assemble_lipid(amount_values)
  se <- amount_study$experiments$alpha
  coldata <- as.data.frame(SummarizedExperiment::colData(se))
  coldata$sample_amount <- c(s1 = 2, s2 = 4)
  rownames(coldata) <- colnames(se)
  SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(coldata)
  amount_study$experiments$alpha <- se
  amount_ops <- triomics_lipidomics_operations(
    normalise = "sample_amount", metadata_column = "sample_amount", amount_units = "mg"
  )
  amount_fit <- prepare_fit(
    amount_study,
    fit_spec(new_dir("a-t06-amount-"), c("s1", "s2"), amount_ops, "amount")
  )
  testthat::expect_equal(snapshot_block(amount_fit)["p1", "s1"], 5, tolerance = 1e-8)
  testthat::expect_equal(snapshot_block(amount_fit)["p2", "s2"], 10, tolerance = 1e-8)
  amount_record <- triomics_lipid_amount_record(amount_fit$experiments$alpha)
  testthat::expect_identical(amount_record$absolute_concentration, FALSE)
  testthat::expect_identical(amount_record$output_quantity, "intensity")
  testthat::expect_identical(amount_record$output_units, "synthetic_arbitrary_units/mg")
  molar_amount <- amount_ops
  molar_amount[[5L]]$output_quantity <- "concentration"
  molar_amount_study <- assemble_lipid(amount_values, display = "molar by name")
  se <- molar_amount_study$experiments$alpha
  coldata <- as.data.frame(SummarizedExperiment::colData(se))
  coldata$sample_amount <- c(2, 4)
  rownames(coldata) <- colnames(se)
  SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(coldata)
  molar_amount_study$experiments$alpha <- se
  before_amount <- snapshot_block(molar_amount_study)
  expect_code(
    prepare_fit(
      molar_amount_study,
      fit_spec(new_dir("a-t06-amount-molar-"), c("s1", "s2"), molar_amount, "amount-molar")
    ),
    "A_SCALE_INPUT"
  )
  testthat::expect_equal(snapshot_block(molar_amount_study), before_amount, tolerance = 1e-8)

  calibrated_ops <- triomics_lipidomics_operations(
    normalise = "internal_standard", standards_map = standards("calibrated", 4)
  )
  calibrated <- prepare_fit(
    assemble_lipid(pair, block_id = "zzz", display = "renamed calibration"),
    fit_spec(new_dir("a-t06-calibrated-"), c("s1", "s2"), calibrated_ops, "calibrated", "zzz")
  )
  testthat::expect_equal(snapshot_block(calibrated, "zzz")["analyte", "s1"], 20, tolerance = 1e-8)
  calibrated_row <- as.data.frame(SummarizedExperiment::rowData(calibrated$experiments$zzz))
  testthat::expect_identical(calibrated_row["analyte", "quantity"], "concentration")
  testthat::expect_identical(calibrated_row["analyte", "units"], "umol/L")
  testthat::expect_identical(calibrated_row["istd", "quantity"], "intensity")
})
