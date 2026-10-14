expect_close <- function(actual, expected, tolerance = 1e-7) {
  testthat::expect_true(is.finite(actual))
  testthat::expect_true(abs(actual - expected) <= tolerance,
                        info = paste(actual, expected, sep = " != "))
}

b09_expect_code <- function(expr, code) {
  error <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, code)
  invisible(error)
}

b09_variable <- function(id, type = "continuous", units = "score_unit",
                         semantics = "visit", codebook = list()) {
  list(
    variable_id = id,
    declared_type = type,
    type = type,
    units = units,
    units_verification = if (type %in% c("continuous", "count")) "verified" else "inapplicable",
    privacy = "ordinary",
    verification = "verified",
    modelable = TRUE,
    time_semantics = semantics,
    codebook = codebook
  )
}

b09_observation <- function(variable_id, subject, visit, value, time = NULL) {
  missing <- length(value) != 1L || is.na(value) || !is.finite(value)
  list(
    variable_id = variable_id,
    subject_key = paste0("sub_", subject),
    visit_id = visit,
    time = time,
    value_kind = if (missing) "missing" else "numeric",
    numeric_value = if (missing) NULL else as.numeric(value),
    code_value = NULL
  )
}

b09_fixture <- function(x0, x1, y0, y1, x1_offset = 0,
                        x_missing = integer(), y_missing = integer(),
                        selection = NULL) {
  n <- length(x0)
  subjects <- LETTERS[seq_len(n)]
  observations <- list()
  samples <- list()
  for (i in seq_len(n)) {
    x0_i <- if (i %in% x_missing) NA_real_ else x0[[i]]
    observations[[length(observations) + 1L]] <- b09_observation("x", subjects[[i]], "V0", x0_i,
                                                                  "2020-01-01")
    observations[[length(observations) + 1L]] <- b09_observation("x", subjects[[i]], "V1",
                                                                  x1[[i]] + x1_offset,
                                                                  "2020-01-31")
    samples[[length(samples) + 1L]] <- list(
      sample_id = paste0(subjects[[i]], "_V0"), subject_key = paste0("sub_", subjects[[i]]),
      visit_id = "V0", time = "2020-01-01",
      value = if (i %in% y_missing) NA_real_ else y0[[i]]
    )
    samples[[length(samples) + 1L]] <- list(
      sample_id = paste0(subjects[[i]], "_V1"), subject_key = paste0("sub_", subjects[[i]]),
      visit_id = "V1", time = "2020-01-31",
      value = if (i %in% y_missing) NA_real_ else y1[[i]]
    )
  }
  view <- list(
    study_id = "synthetic_b_t09",
    dictionary = list(x = b09_variable("x", units = "mmol/L")),
    observations = observations,
    links = list()
  )
  endpoint <- list(
    endpoint_id = "score", endpoint_class = "score", response_type = "continuous",
    units = "score_unit", samples = samples,
    selection = if (is.null(selection)) {
      list(ancestry = "prespecified", validity = "qualified", inspected_outcome = NULL)
    } else selection,
    training_outcome = NULL
  )
  list(view = view, bundle = list(
    study_id = view$study_id, analysis_id = "synthetic_primary", endpoints = list(endpoint)
  ))
}

b09_spec <- function(mode, fields = list(), hypothesis_status = "exploratory") {
  objective <- list(
    objective_id = paste0("objective_", mode), objective_version = "1",
    analysis_id = "synthetic_primary", mode = mode,
    response_ref = "score", predictor_ref = "x", endpoint_ref = "score",
    adjustments = list(), time_window_ref = "V0", missingness_policy = "complete_case",
    family_ref = "clinical_family", hypothesis_status = hypothesis_status,
    protocol_ref = "synthetic-b-t09"
  )
  for (name in names(fields)) objective[[name]] <- fields[[name]]
  list(
    objectives = list(objective),
    family = list(family_id = "clinical_family", member_ids = objective$endpoint_ref,
                  claim_scope = "synthetic_clinical_associations"),
    trend = FALSE, robust = FALSE, fallback = "none", imputation = "none"
  )
}

b09_result <- function(fit) {
  testthat::expect_length(fit$results, 1L)
  fit$results[[1L]]
}

testthat::test_that("B-T09-AC1 between-person and within-person effects use distinct patient units", {
  x0 <- 0:7
  delta_x <- c(1, 2, 3, 4, 4, 3, 2, 1)
  baseline_noise <- c(1, -2, 1, 0, 0, 1, -2, 1)
  change_noise <- c(-0.2, -0.1, -0.2, -0.1, 0.1, 0.2, 0.1, 0.2)
  y0 <- 2 * x0 + baseline_noise
  y1 <- y0 + 3 * delta_x + change_noise
  fixture <- b09_fixture(x0, x0 + delta_x, y0, y1)

  between <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("between_person")
  ))
  within <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view,
    b09_spec("within_person", list(baseline_visit_ref = "V0", later_visit_ref = "V1"))
  ))

  expect_close(between$effect, 2)
  expect_close(within$effect, 3)
  testthat::expect_identical(between$n_subjects, 8L)
  testthat::expect_identical(between$n_samples, 8L)
  testthat::expect_identical(within$n_subjects, 8L)
  testthat::expect_identical(within$n_samples, 16L)
  testthat::expect_identical(within$estimand, "within_person_change_association")
  testthat::expect_false(within$objective_audit$unit_map$repeated_records_as_independent)
})

testthat::test_that("B-T09-AC2 baseline-to-later uses declared horizon and baseline-only predictors", {
  x0 <- 0:7
  delta_x <- c(1, 2, 3, 4, 4, 3, 2, 1)
  y0 <- c(2, 4, 1, 7, 5, 9, 3, 8)
  y1 <- 8 + 2 * x0 + 0.5 * y0 + c(-0.2, 0.1, 0.1, -0.2, 0.2, -0.1, -0.1, 0.2)
  fixture <- b09_fixture(x0, x0 + delta_x, y0, y1)
  fields <- list(
    baseline_visit_ref = "V0", later_visit_ref = "V1", horizon_days = 30,
    baseline_adjustment = TRUE, outcome_estimand = "later_level"
  )
  fit <- associate_longitudinal_endpoints(fixture$bundle, fixture$view,
                                          b09_spec("baseline_to_later", fields))
  row <- b09_result(fit)
  future_changed <- fixture$view
  for (i in seq_along(future_changed$observations)) {
    if (identical(future_changed$observations[[i]]$variable_id, "x") &&
        identical(future_changed$observations[[i]]$visit_id, "V1")) {
      future_changed$observations[[i]]$numeric_value <- future_changed$observations[[i]]$numeric_value + 1000
    }
  }
  future_row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, future_changed, b09_spec("baseline_to_later", fields)
  ))

  testthat::expect_identical(row$estimation_status, "estimated")
  testthat::expect_identical(row$objective_audit$time_window$observed_horizon_days, 30L)
  testthat::expect_identical(row$objective_audit$time_window$predictor_visit, "V0")
  testthat::expect_true(grepl("baseline_endpoint", row$formula, fixed = TRUE))
  expect_close(future_row$effect, row$effect)

  missing_policy <- fields
  missing_policy$horizon_days <- NULL
  blocked <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("baseline_to_later", missing_policy)
  ))
  testthat::expect_identical(blocked$estimation_status, "unavailable")
  testthat::expect_identical(blocked$estimation_reason, "baseline_horizon_undeclared")

  no_adjustment_choice <- fields
  no_adjustment_choice$baseline_adjustment <- NULL
  choice_blocked <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("baseline_to_later", no_adjustment_choice)
  ))
  testthat::expect_identical(choice_blocked$estimation_reason, "baseline_adjustment_choice_undeclared")

  wrong_horizon <- fixture$bundle
  for (i in seq_along(wrong_horizon$endpoints[[1L]]$samples)) {
    if (identical(wrong_horizon$endpoints[[1L]]$samples[[i]]$visit_id, "V1")) {
      wrong_horizon$endpoints[[1L]]$samples[[i]]$time <- "2020-02-01"
    }
  }
  horizon_blocked <- b09_result(associate_longitudinal_endpoints(
    wrong_horizon, fixture$view, b09_spec("baseline_to_later", fields)
  ))
  testthat::expect_identical(horizon_blocked$estimation_status, "unavailable")
  testthat::expect_identical(horizon_blocked$estimation_reason, "horizon_mismatch")
  testthat::expect_identical(horizon_blocked$objective_audit$missingness$reasons$horizon_mismatch, 1L)
})

testthat::test_that("B-T09-AC2 predictor timestamps cannot move the baseline information cutoff", {
  x0 <- 0:7
  y0 <- c(2, 4, 1, 7, 5, 9, 3, 8)
  y1 <- 8 + 2 * x0 + 0.5 * y0 + c(-0.2, 0.1, 0.1, -0.2, 0.2, -0.1, -0.1, 0.2)
  fixture <- b09_fixture(x0, x0 + 1, y0, y1)
  for (i in seq_along(fixture$view$observations)) {
    observation <- fixture$view$observations[[i]]
    if (identical(observation$variable_id, "x") && identical(observation$visit_id, "V0")) {
      fixture$view$observations[[i]]$time <- "2020-01-02"
    }
  }
  fields <- list(
    baseline_visit_ref = "V0", later_visit_ref = "V1", horizon_days = 30,
    baseline_adjustment = TRUE, outcome_estimand = "later_level"
  )
  row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("baseline_to_later", fields)
  ))

  testthat::expect_identical(row$estimation_status, "unavailable")
  testthat::expect_identical(row$estimation_reason, "predictor_after_baseline")
  testthat::expect_identical(row$objective_audit$time_window$observed_horizon_days, 30L)
  testthat::expect_equal(row$objective_audit$time_window$predictor_to_later_horizon_days, 29)
})

testthat::test_that("B-T09-AC2 any observed horizon outside the declaration blocks the objective", {
  x0 <- 0:7
  y0 <- c(2, 4, 1, 7, 5, 9, 3, 8)
  y1 <- 8 + 2 * x0 + 0.5 * y0 + c(-0.2, 0.1, 0.1, -0.2, 0.2, -0.1, -0.1, 0.2)
  fixture <- b09_fixture(x0, x0 + 1, y0, y1)
  fixture$bundle$endpoints[[1L]]$samples[[2L]]$time <- "2020-02-01"
  fields <- list(
    baseline_visit_ref = "V0", later_visit_ref = "V1", horizon_days = 30,
    baseline_adjustment = TRUE, outcome_estimand = "later_level"
  )
  row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("baseline_to_later", fields)
  ))

  testthat::expect_identical(row$estimation_status, "unavailable")
  testthat::expect_identical(row$estimation_reason, "horizon_mismatch")
  testthat::expect_identical(row$objective_audit$time_window$observed_horizon_days, 31L)
  testthat::expect_identical(row$objective_audit$missingness$reasons$horizon_mismatch, 1L)
})

testthat::test_that("B-T09-AC2 later-only static adjustments cannot enter a baseline model", {
  x0 <- 0:7
  y0 <- c(2, 4, 1, 7, 5, 9, 3, 8)
  y1 <- 8 + 2 * x0 + 0.5 * y0 + c(-0.2, 0.1, 0.1, -0.2, 0.2, -0.1, -0.1, 0.2)
  fixture <- b09_fixture(x0, x0 + 1, y0, y1)
  fixture$view$dictionary$late_static <- b09_variable(
    "late_static", units = "units", semantics = "subject_static"
  )
  static_values <- c(2, 4, 1, 6, 3, 8, 5, 7)
  fixture$view$observations <- c(fixture$view$observations, lapply(seq_along(static_values), function(i) {
    b09_observation("late_static", LETTERS[[i]], "V1", static_values[[i]], "2020-01-31")
  }))
  fields <- list(
    baseline_visit_ref = "V0", later_visit_ref = "V1", horizon_days = 30,
    baseline_adjustment = TRUE, outcome_estimand = "later_level",
    adjustments = list(list(variable = "late_static", form = "level",
                            rationale = "Synthetic adversarial later-only measurement."))
  )
  row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("baseline_to_later", fields)
  ))

  testthat::expect_identical(row$estimation_status, "unavailable")
  testthat::expect_identical(row$estimation_reason, "adjustment_after_baseline")
})

testthat::test_that("B-T09 refuses a schema-valid within-person CLI objective instead of relabeling it", {
  config <- list(
    study_id = "triomics_b_t09_fixture",
    clinical = list(objectives = list(list(
      objective_id = "longitudinal_change", analysis_id = "synthetic_primary",
      mode = "within_person", response_ref = "synthetic_longitudinal_score",
      predictor_ref = "glucose", endpoint_ref = "synthetic_longitudinal_score",
      adjustments = list(), time_window_ref = "V1", missingness_policy = "complete_case",
      family_ref = "clinical_longitudinal_family", hypothesis_status = "exploratory",
      protocol_ref = "synthetic-b-t09"
    )))
  )

  error <- b09_expect_code(triomics_b09_cli_spec(config), "B_DESIGN_UNSUPPORTED")
  testthat::expect_match(conditionMessage(error), "declared objective mode")
})

testthat::test_that("B-T09 missing outcome estimand returns typed unavailability", {
  x0 <- 0:7
  y0 <- c(2, 4, 1, 7, 5, 9, 3, 8)
  y1 <- 8 + 2 * x0 + 0.5 * y0 + c(-0.2, 0.1, 0.1, -0.2, 0.2, -0.1, -0.1, 0.2)
  fixture <- b09_fixture(x0, x0 + 1, y0, y1)
  fields <- list(baseline_visit_ref = "V0", later_visit_ref = "V1", horizon_days = 30,
                 baseline_adjustment = TRUE, outcome_estimand = NULL)

  row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("baseline_to_later", fields)
  ))
  testthat::expect_identical(row$estimation_status, "unavailable")
  testthat::expect_identical(row$estimation_reason, "outcome_estimand_undeclared")
})

testthat::test_that("B-T09-AC3 effect modification tests the fitted interaction coefficient", {
  n_per_group <- 8L
  x <- rep(1:8, times = 2L)
  group <- rep(c("low", "high"), each = n_per_group)
  residual <- rep(c(-0.1, 0.1, 0.1, -0.1, -0.1, 0.1, 0.1, -0.1), times = 2L)
  y <- 2 + x + 3 * as.integer(group == "high") +
    4 * x * as.integer(group == "high") + residual
  subjects <- paste0("I", seq_along(x))
  observations <- unlist(lapply(seq_along(x), function(i) list(
    b09_observation("x", subjects[[i]], "V0", x[[i]], "2020-01-01"),
    list(variable_id = "group", subject_key = paste0("sub_", subjects[[i]]),
         visit_id = "V0", time = "2020-01-01", value_kind = "code",
         numeric_value = NULL, code_value = group[[i]])
  )), recursive = FALSE)
  view <- list(
    study_id = "synthetic_b_t09",
    dictionary = list(
      x = b09_variable("x", units = "mmol/L"),
      group = b09_variable("group", type = "binary", units = "inapplicable",
                           codebook = list(low = "Low", high = "High"))
    ), observations = observations, links = list()
  )
  samples <- lapply(seq_along(x), function(i) list(
    sample_id = paste0(subjects[[i]], "_V0"), subject_key = paste0("sub_", subjects[[i]]),
    visit_id = "V0", time = "2020-01-01", value = y[[i]]
  ))
  bundle <- list(study_id = view$study_id, analysis_id = "synthetic_primary", endpoints = list(list(
    endpoint_id = "score", endpoint_class = "score", response_type = "continuous", units = "score_unit",
    samples = samples, selection = list(ancestry = "prespecified", validity = "qualified",
                                        inspected_outcome = NULL)
  )))
  spec <- b09_spec("effect_modification", list(modifier_ref = "group"))
  row <- b09_result(associate_longitudinal_endpoints(bundle, view, spec))

  expect_close(row$effect, 4)
  testthat::expect_identical(row$tested_term, "x:group")
  testthat::expect_true(is.finite(row$p))
  testthat::expect_identical(row$n_subjects, 16L)
})

testthat::test_that("B-T09-AC4 outcome-selected scores cannot release independent confirmation", {
  fixture <- b09_fixture(0:7, 1:8, 0:7, 1:8,
                         selection = list(ancestry = "selected_after_clinical_outcome",
                                          validity = "qualified", inspected_outcome = "x"))
  fit <- associate_longitudinal_endpoints(
    fixture$bundle, fixture$view,
    b09_spec("between_person", hypothesis_status = "independent_validation")
  )
  row <- b09_result(fit)

  testthat::expect_true(is.finite(row$effect))
  testthat::expect_identical(row$inferential_status, "descriptive_only")
  testthat::expect_false(row$independent_validation_released)
  testthat::expect_null(row$q)
  testthat::expect_identical(row$objective_audit$unit_map$analysis_unit, "subject")
  testthat::expect_identical(row$objective_audit$time_window$visit_ids, "V0")
  testthat::expect_identical(row$objective_audit$missingness$policy, "complete_case")
  testthat::expect_identical(row$objective_audit$missingness$n_included, 8L)
})

testthat::test_that("B-T09 audits clinical and endpoint missingness separately", {
  fixture <- b09_fixture(0:7, 1:8, 0:7, 1:8, x_missing = 1L, y_missing = 2L)
  row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("between_person")
  ))

  testthat::expect_identical(row$n_subjects, 6L)
  testthat::expect_identical(row$objective_audit$missingness$n_excluded_clinical, 1L)
  testthat::expect_identical(row$objective_audit$missingness$n_excluded_endpoint, 1L)
  testthat::expect_identical(row$objective_audit$unit_map$input_record_unit, "sample_visit")
})

testthat::test_that("B-T09 counts subjects missing the declared between-person visit", {
  fixture <- b09_fixture(0:7, 1:8, 0:7, 1:8)
  fixture$bundle$endpoints[[1L]]$samples <- Filter(function(sample) {
    !identical(sample$sample_id, "A_V0")
  }, fixture$bundle$endpoints[[1L]]$samples)
  row <- b09_result(associate_longitudinal_endpoints(
    fixture$bundle, fixture$view, b09_spec("between_person")
  ))

  testthat::expect_identical(row$n_subjects, 7L)
  testthat::expect_identical(row$objective_audit$missingness$n_candidate, 8L)
  testthat::expect_identical(row$objective_audit$missingness$n_excluded_endpoint, 1L)
  testthat::expect_identical(row$objective_audit$missingness$reasons$endpoint_visit_missing, 1L)
})

testthat::test_that("B-T09 canonical schema admits only complete longitudinal objectives", {
  base <- readLines(triomics_test_resource("examples", "tickets", "B-T09.yaml"), warn = FALSE)
  write_config <- function(mode, fields = character()) {
    replacement <- paste(c(paste0("      mode: ", mode), paste0("      ", fields)), collapse = "\n")
    config <- sub("      mode: between_person", replacement, base, fixed = TRUE)
    path <- tempfile(fileext = ".yaml")
    writeLines(config, path)
    path
  }

  parsed <- triomics_read_config(write_config(
    "within_person", c("baseline_visit_ref: V0", "later_visit_ref: V1")
  ))
  testthat::expect_identical(parsed$clinical$objectives[[1L]]$mode, "within_person")

  b09_expect_code(
    triomics_read_config(write_config("within_person", "baseline_visit_ref: V0")),
    "A_CONFIG_SCHEMA"
  )

  baseline_fields <- c(
    "baseline_visit_ref: V0", "later_visit_ref: V1", "horizon_days: 30",
    "baseline_adjustment: true", "outcome_estimand: later_level"
  )
  baseline <- triomics_read_config(write_config("baseline_to_later", baseline_fields))
  testthat::expect_identical(baseline$clinical$objectives[[1L]]$horizon_days, 30L)

  b09_expect_code(
    triomics_read_config(write_config("baseline_to_later", baseline_fields[-5L])),
    "A_CONFIG_SCHEMA"
  )

  modifier <- triomics_read_config(write_config(
    "effect_modification", "modifier_ref: treatment_group"
  ))
  testthat::expect_identical(
    modifier$clinical$objectives[[1L]]$modifier_ref,
    "treatment_group"
  )
})
