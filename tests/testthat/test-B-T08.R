expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

expect_close <- function(actual, expected) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  testthat::expect_true(is.finite(actual) && is.finite(expected))
  testthat::expect_true(abs(actual - expected) <= 1e-8 + 1e-6 * abs(expected),
                        info = paste(actual, expected, sep = " != "))
}

b08_variable <- function(variable_id, type = "continuous", units = "mmol/L",
                         semantics = "visit", privacy = "ordinary",
                         roles = "predictor", codebook = list(),
                         censoring = "forbidden") {
  list(
    variable_id = variable_id,
    declared_type = type,
    analysis_type = type,
    type = type,
    units = units,
    units_verification = if (type %in% c("continuous", "count")) "verified" else "inapplicable",
    role = "measured",
    allowed_model_roles = roles,
    time_semantics = semantics,
    privacy = privacy,
    verification = "verified",
    codebook = codebook,
    missing_map = list("." = "placeholder"),
    censoring = censoring,
    availability = "available",
    modelable = TRUE
  )
}

b08_cohort <- function(glucose = c(1, 2, 3, 4, 5, 6, 7, 8),
                       age = c(30, 35, 33, 40, 38, 50, 45, 55),
                       score = NULL,
                       glucose_kind = NULL,
                       glucose_code = NULL,
                       score_na = integer(),
                       extra_visit = FALSE,
                       predictor_type = "continuous",
                       response_type = "continuous",
                       endpoint_class = "score",
                       training_outcome = NULL,
                       ancestry = "prespecified",
                       inspected = NULL,
                       members = "synthetic_score",
                       include_endpoint = TRUE,
                       hash = NULL,
                       mode = "between_person",
                       policy = "complete_case",
                       fallback = "none",
                       imputation = NULL,
                       hypothesis = "exploratory",
                       adjustments = NULL,
                       family = TRUE,
                       sex = NULL) {
  n <- length(glucose)
  sample_ids <- sprintf("SYN_%02d", seq_len(n))
  subject_ids <- sprintf("SECRET-SUBJ-%02d", seq_len(n))
  subject_keys <- sprintf("sub_%02d", seq_len(n))
  if (is.null(score)) score <- 1.5 * glucose + 0.2 * age + c(-0.4, 0.3, -0.2, 0.5, -0.1, 0.2, -0.3, 0.4)[seq_len(n)]
  if (length(score_na)) score[score_na] <- NA_real_
  if (is.null(glucose_kind)) glucose_kind <- rep("numeric", n)
  dictionary <- list(
    glucose = b08_variable("glucose", type = predictor_type, units = if (predictor_type == "binary") "inapplicable" else "mmol/L",
                           codebook = if (predictor_type == "binary") list(low = "Low", high = "High") else list(),
                           censoring = if (any(glucose_kind == "censored")) "allowed" else "forbidden"),
    age = b08_variable("age", units = "year"),
    mrn = b08_variable("mrn", type = "text", units = "inapplicable", semantics = "subject_static",
                       privacy = "identifier", roles = "excluded")
  )
  if (!is.null(sex)) {
    dictionary$sex <- b08_variable("sex", type = "binary", units = "inapplicable", semantics = "subject_static",
                                   codebook = list(low = "Low", high = "High"))
  }
  observations <- list()
  links <- list()
  add_obs <- function(variable_id, index, kind, numeric_value = NULL, code_value = NULL,
                      comparator = NULL, limit = NULL, visit = "V1", sample = sample_ids[[index]]) {
    observations[[length(observations) + 1L]] <<- list(
      observation_id = paste(variable_id, index, visit, sep = "-"),
      subject_id = subject_ids[[index]],
      subject_key = subject_keys[[index]],
      variable_id = variable_id,
      value_kind = kind,
      numeric_value = numeric_value,
      code_value = code_value,
      comparator = comparator,
      limit = limit,
      scope = dictionary[[variable_id]]$time_semantics,
      visit_id = if (identical(dictionary[[variable_id]]$time_semantics, "subject_static")) NULL else visit,
      sample_id = sample,
      units = dictionary[[variable_id]]$units,
      privacy = dictionary[[variable_id]]$privacy,
      type = dictionary[[variable_id]]$type,
      time_semantics = dictionary[[variable_id]]$time_semantics,
      text_value = if (identical(variable_id, "mrn")) sprintf("SECRET-MRN-%02d", index) else NULL
    )
  }
  for (i in seq_len(n)) {
    kind <- glucose_kind[[i]]
    if (identical(kind, "numeric")) {
      add_obs("glucose", i, "numeric", numeric_value = glucose[[i]])
    } else if (identical(kind, "missing")) {
      add_obs("glucose", i, "missing")
    } else if (identical(kind, "censored")) {
      add_obs("glucose", i, "censored", comparator = "<", limit = 1)
    } else if (identical(kind, "code")) {
      add_obs("glucose", i, "code", code_value = glucose_code[[i]])
    }
    add_obs("age", i, "numeric", numeric_value = age[[i]])
    add_obs("mrn", i, "excluded")
    links[[length(links) + 1L]] <- list(
      observation_id = paste("glucose", i, "V1", sep = "-"),
      variable_id = "glucose",
      subject_id = subject_ids[[i]],
      subject_key = subject_keys[[i]],
      visit_id = "V1",
      sample_id = sample_ids[[i]],
      status = "approved"
    )
  }
  if (!is.null(sex)) {
    for (i in seq_len(n)) add_obs("sex", i, "code", code_value = sex[[i]], visit = NULL, sample = NULL)
  }
  if (isTRUE(extra_visit)) {
    add_obs("glucose", 1L, "numeric", numeric_value = glucose[[1L]] + 3, visit = "V2", sample = "SYN_01B")
    links[[length(links) + 1L]] <- list(
      observation_id = "glucose-1-V2", variable_id = "glucose", subject_id = subject_ids[[1L]],
      subject_key = subject_keys[[1L]], visit_id = "V2", sample_id = "SYN_01B", status = "approved"
    )
  }
  samples <- lapply(seq_len(n), function(i) {
    list(sample_id = sample_ids[[i]], subject_key = subject_keys[[i]], visit_id = "V1",
         value = score[[i]])
  })
  if (isTRUE(extra_visit)) {
    samples[[length(samples) + 1L]] <- list(
      sample_id = "SYN_01B", subject_key = subject_keys[[1L]], visit_id = "V2", value = score[[1L]]
    )
  }
  endpoint <- list(
    endpoint_id = "synthetic_score",
    endpoint_class = endpoint_class,
    response_type = response_type,
    units = "score_unit",
    scale = "linear",
    samples = samples,
    producing_fit = "synthetic-b-t08",
    training_ids = if (is.null(training_outcome)) NULL else subject_keys,
    training_outcome = training_outcome,
    orientation = "native",
    selection = list(ancestry = ancestry, validity = "qualified", inspected_outcome = inspected),
    available_at_time = "V1",
    dependency_group = "synthetic",
    matrix_hash = hash,
    source_hash = "synthetic-source"
  )
  bundle <- list(
    study_id = "triomics_b_t08_fixture",
    analysis_id = "synthetic_primary",
    endpoints = if (isTRUE(include_endpoint)) list(endpoint) else list()
  )
  if (is.null(adjustments)) {
    adjustments <- list(list(
      variable = "age", role = "confounder", form = "level",
      rationale = "Synthetic age balance, not a causal claim."
    ))
  }
  objective <- list(
    objective_id = "glucose_between",
    objective_version = "1",
    analysis_id = "synthetic_primary",
    question = "Between-person association of a synthetic score with glucose.",
    mode = mode,
    response_ref = "synthetic_score",
    predictor_ref = if (is.null(sex)) "glucose" else "sex",
    endpoint_ref = "synthetic_score",
    adjustments = adjustments,
    time_window_ref = "V1",
    missingness_policy = policy,
    family_ref = "clinical_between_family",
    hypothesis_status = hypothesis,
    protocol_ref = "synthetic-b-t08"
  )
  if (!is.null(sex)) objective$predictor_ref <- "sex"
  spec <- list(
    objectives = list(objective),
    trend = FALSE,
    robust = FALSE,
    fallback = fallback,
    imputation = imputation
  )
  if (isTRUE(family)) {
    spec$family <- list(
      family_id = "clinical_between_family",
      member_ids = members,
      claim_scope = "between_person_synthetic_scores"
    )
  }
  list(
    bundle = bundle, view = list(
      study_id = "triomics_b_t08_fixture",
      dictionary = dictionary,
      observations = observations,
      links = links
    ),
    spec = spec, glucose = glucose, age = age, score = score,
    sample_ids = sample_ids, subject_keys = subject_keys, subject_ids = subject_ids
  )
}

b08_one <- function(fit, endpoint_id = "synthetic_score") {
  hits <- Filter(function(row) identical(row$endpoint_id, endpoint_id), fit$results)
  testthat::expect_length(hits, 1L)
  hits[[1L]]
}

testthat::test_that("B-T08-AC1 adjusted continuous association reports units, effect, uncertainty and patients", {
  cohort <- b08_cohort()
  fit <- associate_endpoints(cohort$bundle, cohort$view, cohort$spec)
  row <- b08_one(fit)
  used <- is.finite(cohort$score)
  oracle <- stats::lm(cohort$score[used] ~ cohort$glucose[used] + cohort$age[used])
  expect_close(row$effect, unname(stats::coef(oracle)[["cohort$glucose[used]"]]))
  expect_close(row$ordinary_se, unname(summary(oracle)$coefficients["cohort$glucose[used]", "Std. Error"]))
  design <- cbind(intercept = 1, glucose = cohort$glucose[used], age = cohort$age[used])
  contrast <- matrix(c(0, 1, 0), ncol = 1L)
  moderated <- limma::eBayes(limma::contrasts.fit(limma::lmFit(matrix(cohort$score[used], nrow = 1L), design), contrast),
                             trend = FALSE, robust = FALSE)
  expect_close(row$moderated_se, unname(sqrt(moderated$s2.post) * moderated$stdev.unscaled))
  expect_close(row$statistic, unname(moderated$t))
  crit <- stats::qt(0.975, unname(moderated$df.total))
  expect_close(row$interval_low, row$effect - crit * row$moderated_se)
  expect_close(row$interval_high, row$effect + crit * row$moderated_se)
  testthat::expect_identical(row$interval_method, "moderated_t")
  testthat::expect_identical(row$effect_unit, "score_unit per mmol/L")
  testthat::expect_identical(row$effect_scale, "per_predictor_unit")
  testthat::expect_identical(row$predictor_units, "mmol/L")
  testthat::expect_identical(row$response_units, "score_unit")
  testthat::expect_identical(row$n_subjects, 8L)
  testthat::expect_identical(row$n_rows_used, 8L)
  testthat::expect_identical(row$estimation_status, "estimated")
  testthat::expect_identical(row$imputation, "none")
  unadjusted <- stats::coef(stats::lm(cohort$score ~ cohort$glucose))[[2L]]
  testthat::expect_true(abs(row$effect - unadjusted) > 1e-6)
  testthat::expect_false(isTRUE(fit$fallback_applied))
  testthat::expect_identical(fit$method, "limma")
})

testthat::test_that("B-T08-AC1 a binary clinical predictor is a factor contrast, not a numeric code", {
  sex <- c("low", "high", "low", "high", "low", "high", "low", "high")
  age <- c(30, 35, 33, 40, 38, 50, 45, 55)
  score <- ifelse(sex == "high", 4, 0) + 0.1 * age + c(-0.2, 0.4, 0.1, -0.3, 0.2, -0.1, 0.3, -0.4)
  cohort <- b08_cohort(glucose = rep(1, 8), age = age, score = score, sex = sex,
                        adjustments = list(list(variable = "age", role = "confounder", form = "level",
                                                rationale = "Synthetic age balance, not a causal claim.")))
  cohort$spec$objectives[[1L]]$predictor_ref <- "sex"
  fit <- associate_endpoints(cohort$bundle, cohort$view, cohort$spec)
  row <- b08_one(fit)
  indicator <- as.numeric(sex == "high")
  oracle <- stats::lm(score ~ indicator + age)
  expect_close(row$effect, unname(stats::coef(oracle)[["indicator"]]))
  testthat::expect_identical(row$effect_scale, "level_difference")
  testthat::expect_identical(row$predictor_type, "binary")
  testthat::expect_false(identical(row$effect_scale, "per_predictor_unit"))
})

testthat::test_that("B-T08-AC2 unsupported responses, unknown codes, censoring and constants do not fall back to Pearson", {
  binary <- b08_cohort(response_type = "binary", fallback = "pearson")
  binary_fit <- associate_endpoints(binary$bundle, binary$view, binary$spec)
  binary_row <- b08_one(binary_fit)
  testthat::expect_null(binary_row$effect)
  testthat::expect_null(binary_row$p)
  testthat::expect_identical(binary_row$response_type, "binary")
  testthat::expect_identical(binary_row$estimation_status, "unavailable")
  testthat::expect_identical(binary_row$estimation_reason, "unsupported_response_type")
  testthat::expect_false(isTRUE(binary_fit$fallback_applied))
  testthat::expect_false(identical(binary_fit$method, "pearson"))
  testthat::expect_null(binary_row$correlation)

  survival <- b08_cohort(response_type = "survival", fallback = "pearson")
  survival_row <- b08_one(associate_endpoints(survival$bundle, survival$view, survival$spec))
  testthat::expect_identical(survival_row$estimation_reason, "unsupported_response_type")
  testthat::expect_null(survival_row$effect)

  ordinal <- b08_cohort(predictor_type = "ordinal", glucose_kind = rep("code", 8),
                        glucose_code = c("1", "2", "3", "1", "2", "3", "1", "2"),
                        fallback = "pearson")
  ordinal$view$dictionary$glucose$codebook <- list("1" = "Low", "2" = "Mid", "3" = "High")
  ordinal$view$dictionary$glucose$units <- "inapplicable"
  ordinal$view$dictionary$glucose$units_verification <- "inapplicable"
  ordinal_row <- b08_one(associate_endpoints(ordinal$bundle, ordinal$view, ordinal$spec))
  testthat::expect_identical(ordinal_row$estimation_reason, "ordinal_not_numeric_distance")
  testthat::expect_null(ordinal_row$effect)
  testthat::expect_null(ordinal_row$correlation)

  unknown <- b08_cohort(predictor_type = "binary", glucose_kind = rep("code", 8),
                        glucose_code = c("low", "high", "low", "maybe", "low", "high", "low", "high"))
  unknown_row <- b08_one(associate_endpoints(unknown$bundle, unknown$view, unknown$spec))
  testthat::expect_identical(unknown_row$estimation_reason, "unknown_coding")
  testthat::expect_null(unknown_row$effect)

  kinds <- rep("numeric", 8)
  kinds[[2L]] <- "censored"
  censored <- b08_cohort(glucose_kind = kinds)
  censored_row <- b08_one(associate_endpoints(censored$bundle, censored$view, censored$spec))
  substituted <- censored$glucose
  substituted[[2L]] <- 1
  leaked <- unname(stats::coef(stats::lm(censored$score ~ substituted + censored$age))[[2L]])
  testthat::expect_identical(censored_row$estimation_reason, "censored_values_retained")
  testthat::expect_null(censored_row$effect)
  testthat::expect_false(isTRUE(is.finite(censored_row$effect) && abs(censored_row$effect - leaked) < 1e-6))

  constant <- b08_cohort(glucose = rep(5, 8), fallback = "pearson")
  constant_row <- b08_one(associate_endpoints(constant$bundle, constant$view, constant$spec))
  testthat::expect_identical(constant_row$estimation_status, "failed")
  testthat::expect_identical(constant_row$estimation_reason, "constant_predictor")
  testthat::expect_null(constant_row$effect)
  testthat::expect_null(constant_row$p)
  testthat::expect_null(constant_row$correlation)
  testthat::expect_false(isTRUE(associate_endpoints(constant$bundle, constant$view, constant$spec)$fallback_applied))
})

testthat::test_that("B-T08-AC2 a constant continuous response fails closed without family q-values", {
  constant <- b08_cohort(score = rep(9, 8), fallback = "pearson")
  fit <- associate_endpoints(constant$bundle, constant$view, constant$spec)
  row <- b08_one(fit)

  testthat::expect_identical(row$estimation_status, "failed")
  testthat::expect_identical(row$estimation_reason, "constant_response")
  testthat::expect_null(row$effect)
  testthat::expect_null(row$ordinary_se)
  testthat::expect_null(row$moderated_se)
  testthat::expect_null(row$p)
  testthat::expect_null(row$q)
  testthat::expect_false(isTRUE(fit$family$q_published))
  testthat::expect_identical(unname(fit$family$internal_p), 1)
  testthat::expect_false(isTRUE(fit$fallback_applied))
  testthat::expect_false(identical(fit$method, "pearson"))
})

testthat::test_that("B-T08-AC3 duplicate fixed-family members abort before fitting", {
  cohort <- b08_cohort()
  cohort$spec$family$member_ids <- c("synthetic_score", "synthetic_score")
  fit_called <- FALSE
  testthat::local_mocked_bindings(
    triomics_b08_fit_endpoint = function(...) {
      fit_called <<- TRUE
      stop("endpoint fit reached before duplicate-family validation")
    },
    .package = "triomics"
  )

  expect_code(associate_endpoints(cohort$bundle, cohort$view, cohort$spec), "B_FAMILY_UNDECLARED")
  testthat::expect_false(fit_called)
})

testthat::test_that("B-T08-AC3 fixed endpoint families and training ancestry are enforced on synthetic scores", {
  e1 <- b08_cohort()
  e2 <- e1
  e2$bundle$endpoints[[1L]]$endpoint_id <- "held_score"
  e2$bundle$endpoints[[1L]]$samples <- lapply(e2$bundle$endpoints[[1L]]$samples, function(sample) {
    sample$value <- sample$value + 1
    sample
  })
  bundle <- e1$bundle
  bundle$endpoints[[2L]] <- e2$bundle$endpoints[[1L]]
  spec <- e1$spec
  spec$family$member_ids <- c("synthetic_score", "held_score", "unsupplied_score")
  spec$objectives[[2L]] <- spec$objectives[[1L]]
  spec$objectives[[2L]]$objective_id <- "held_between"
  spec$objectives[[2L]]$response_ref <- "held_score"
  spec$objectives[[2L]]$endpoint_ref <- "held_score"
  fit <- associate_endpoints(bundle, e1$view, spec)
  testthat::expect_identical(fit$family$n_declared, 3L)
  testthat::expect_identical(fit$family$n_failed, 1L)
  testthat::expect_identical(fit$family$member_ids, c("synthetic_score", "held_score", "unsupplied_score"))
  rows <- lapply(fit$family$member_ids, function(id) b08_one(fit, id))
  testthat::expect_identical(rows[[3L]]$estimation_reason, "endpoint_absent")
  testthat::expect_null(rows[[3L]]$p)
  testthat::expect_null(rows[[3L]]$q)
  internal <- c(rows[[1L]]$p, rows[[2L]]$p, 1)
  expected_q <- unname(stats::p.adjust(internal, method = "BH"))
  shrunk_q <- unname(stats::p.adjust(internal[1:2], method = "BH"))
  expect_close(rows[[1L]]$q, expected_q[[1L]])
  expect_close(rows[[2L]]$q, expected_q[[2L]])
  testthat::expect_true(abs(rows[[1L]]$q - shrunk_q[[1L]]) > 1e-8)

  trained <- b08_cohort(training_outcome = "glucose", hypothesis = "independent_validation",
                        ancestry = "trained_on_outcome")
  trained_fit <- associate_endpoints(trained$bundle, trained$view, trained$spec)
  trained_row <- b08_one(trained_fit)
  testthat::expect_identical(trained_row$inferential_status, "descriptive_only")
  testthat::expect_false(isTRUE(trained_row$independent_validation_released))
  testthat::expect_null(trained_row$q)
  testthat::expect_true(is.finite(trained_row$effect))
  testthat::expect_false(isTRUE(trained_fit$family$q_published))

  selected <- b08_cohort(endpoint_class = "unsupervised_factor", ancestry = "selected_after_clinical_outcome",
                         inspected = "glucose")
  selected_row <- b08_one(associate_endpoints(selected$bundle, selected$view, selected$spec))
  testthat::expect_null(selected_row$q)
  testthat::expect_identical(selected_row$estimation_reason, "selection_after_outcome")
  testthat::expect_true(is.finite(selected_row$effect))

  undeclared <- b08_cohort(endpoint_class = "unsupervised_factor", family = FALSE)
  undeclared_row <- b08_one(associate_endpoints(undeclared$bundle, undeclared$view, undeclared$spec))
  testthat::expect_null(undeclared_row$effect)
  testthat::expect_identical(undeclared_row$estimation_reason, "family_undeclared")

  pathway <- b08_cohort(endpoint_class = "pathway_enrichment")
  pathway_row <- b08_one(associate_endpoints(pathway$bundle, pathway$view, pathway$spec))
  testthat::expect_null(pathway_row$effect)
  testthat::expect_identical(pathway_row$estimation_reason, "pathway_enrichment_not_patient_score")

  within <- b08_cohort(mode = "within_person")
  within_row <- b08_one(associate_endpoints(within$bundle, within$view, within$spec))
  testthat::expect_identical(within_row$mode, "within_person")
  testthat::expect_null(within_row$effect)
  testthat::expect_identical(within_row$estimation_reason, "mode_not_executable")
})

testthat::test_that("B-T08-AC3 failed constant members stay in mixed-family adjustment without publishing their q", {
  first <- b08_cohort()
  second <- b08_cohort(score = first$score + c(0.2, -0.1, 0.4, -0.3, 0.1, -0.2, 0.3, -0.4))
  constant <- b08_cohort(score = rep(12, 8))

  endpoint_ids <- c("synthetic_score", "held_score", "constant_score")
  second$bundle$endpoints[[1L]]$endpoint_id <- endpoint_ids[[2L]]
  constant$bundle$endpoints[[1L]]$endpoint_id <- endpoint_ids[[3L]]
  bundle <- first$bundle
  bundle$endpoints <- c(first$bundle$endpoints,
                        second$bundle$endpoints,
                        constant$bundle$endpoints)

  spec <- first$spec
  spec$family$member_ids <- endpoint_ids
  second_objective <- spec$objectives[[1L]]
  second_objective$objective_id <- "held_between"
  second_objective$response_ref <- endpoint_ids[[2L]]
  second_objective$endpoint_ref <- endpoint_ids[[2L]]
  constant_objective <- spec$objectives[[1L]]
  constant_objective$objective_id <- "constant_between"
  constant_objective$response_ref <- endpoint_ids[[3L]]
  constant_objective$endpoint_ref <- endpoint_ids[[3L]]
  spec$objectives <- c(spec$objectives, list(second_objective, constant_objective))

  fit <- associate_endpoints(bundle, first$view, spec)
  rows <- lapply(endpoint_ids, function(id) b08_one(fit, id))
  internal_p <- c(rows[[1L]]$p, rows[[2L]]$p, 1)
  expected_q <- unname(stats::p.adjust(internal_p, method = "BH"))

  testthat::expect_identical(fit$family$n_declared, 3L)
  testthat::expect_identical(fit$family$n_estimated, 2L)
  testthat::expect_identical(fit$family$n_failed, 1L)
  testthat::expect_identical(unname(fit$family$internal_p), internal_p)
  testthat::expect_true(isTRUE(fit$family$q_published))
  expect_close(rows[[1L]]$q, expected_q[[1L]])
  expect_close(rows[[2L]]$q, expected_q[[2L]])
  testthat::expect_identical(rows[[3L]]$estimation_status, "failed")
  testthat::expect_identical(rows[[3L]]$estimation_reason, "constant_response")
  testthat::expect_null(rows[[3L]]$effect)
  testthat::expect_null(rows[[3L]]$ordinary_se)
  testthat::expect_null(rows[[3L]]$moderated_se)
  testthat::expect_null(rows[[3L]]$p)
  testthat::expect_null(rows[[3L]]$q)
  testthat::expect_false(isTRUE(fit$fallback_applied))
})

testthat::test_that("B-T08-AC4 ordinary exports omit private identities and separate clinical exclusions from endpoint missingness", {
  kinds <- rep("numeric", 8)
  kinds[[8L]] <- "missing"
  cohort <- b08_cohort(glucose_kind = kinds, score_na = 7L)
  fit <- associate_endpoints(cohort$bundle, cohort$view, cohort$spec)
  row <- b08_one(fit)
  testthat::expect_identical(row$imputation, "none")
  testthat::expect_false(isTRUE(row$assay_imputation_reused))
  testthat::expect_identical(row$missingness_policy, "complete_case")
  reasons <- vapply(row$exclusions, function(item) item$reason, character(1))
  sources <- vapply(row$exclusions, function(item) item$source, character(1))
  testthat::expect_true("clinical_complete_case" %in% reasons)
  testthat::expect_true("endpoint_unobserved" %in% reasons)
  testthat::expect_false(any(reasons[sources == "endpoint"] == "clinical_complete_case"))
  testthat::expect_false(any(reasons[sources == "clinical"] == "endpoint_unobserved"))
  keys <- vapply(row$exclusions, function(item) item$subject_key, character(1))
  testthat::expect_true(all(startsWith(keys, "sub_")))
  exported <- triomics_clinical_association_export(fit)
  text <- as.character(jsonlite::toJSON(exported, auto_unbox = TRUE, null = "null"))
  testthat::expect_false(grepl("SECRET-SUBJ-", text, fixed = TRUE))
  testthat::expect_false(grepl("SECRET-MRN-", text, fixed = TRUE))
  testthat::expect_false(grepl("\"subject_id\"", text, fixed = TRUE))
  testthat::expect_false(grepl("text_value", text, fixed = TRUE))
  testthat::expect_identical(row$n_excluded_clinical, 1L)
  testthat::expect_identical(row$n_excluded_endpoint, 1L)
  testthat::expect_identical(row$n_subjects, 6L)

  refused <- b08_cohort(imputation = "qrilc")
  refused_row <- b08_one(associate_endpoints(refused$bundle, refused$view, refused$spec))
  testthat::expect_null(refused_row$effect)
  testthat::expect_identical(refused_row$estimation_reason, "clinical_imputation_refused")
  testthat::expect_identical(refused_row$imputation, "none")
  testthat::expect_false(isTRUE(refused_row$assay_imputation_reused))
})

testthat::test_that("B-T08 adversarial repeated visits, rank failure and stale endpoint hashes stay explicit", {
  repeated <- b08_cohort(extra_visit = TRUE)
  repeated$spec$objectives[[1L]]$time_window_ref <- NULL
  repeated_row <- b08_one(associate_endpoints(repeated$bundle, repeated$view, repeated$spec))
  testthat::expect_null(repeated_row$effect)
  testthat::expect_identical(repeated_row$estimation_reason, "repeated_visits_not_independent")

  windowed <- b08_cohort(extra_visit = TRUE)
  window_fit <- associate_endpoints(windowed$bundle, windowed$view, windowed$spec)
  window_row <- b08_one(window_fit)
  testthat::expect_identical(window_row$n_subjects, 8L)
  testthat::expect_true(is.finite(window_row$effect))

  collinear <- b08_cohort(adjustments = list(list(
    variable = "age", role = "confounder", form = "level",
    rationale = "Synthetic duplicate of the predictor."
  )))
  for (obs_i in seq_along(collinear$view$observations)) {
    obs <- collinear$view$observations[[obs_i]]
    if (!identical(obs$variable_id, "age")) next
    index <- match(obs$subject_key, collinear$subject_keys)
    collinear$view$observations[[obs_i]]$numeric_value <- collinear$glucose[[index]]
  }
  collinear_row <- b08_one(associate_endpoints(collinear$bundle, collinear$view, collinear$spec))
  testthat::expect_null(collinear_row$effect)
  testthat::expect_identical(collinear_row$estimation_reason, "rank_deficient")

  stale <- b08_cohort(hash = paste(rep("a", 64), collapse = ""))
  expect_code(associate_endpoints(stale$bundle, stale$view, stale$spec), "B_DESIGN_HASH")
  fresh <- b08_cohort()
  fresh$bundle$endpoints[[1L]]$matrix_hash <- triomics_b08_endpoint_hash(fresh$bundle$endpoints[[1L]])
  fresh_row <- b08_one(associate_endpoints(fresh$bundle, fresh$view, fresh$spec))
  testthat::expect_true(is.finite(fresh_row$effect))
  changed <- fresh
  changed$bundle$endpoints[[1L]]$samples[[1L]]$value <- changed$bundle$endpoints[[1L]]$samples[[1L]]$value + 10
  expect_code(associate_endpoints(changed$bundle, changed$view, changed$spec), "B_DESIGN_HASH")
})

testthat::test_that("B-T08 consumes an A-T09 clinical bundle without a second schema", {
  n <- 6L
  registry <- data.frame(sample_id = sprintf("SYN_%02d", seq_len(n)),
                         subject_id = rep(NA_character_, n), stringsAsFactors = FALSE)
  dir <- tempfile("b-t08-")
  dir.create(dir)
  dictionary <- list(
    c(variable_id = "glucose", label = "Glucose", type = "continuous", units = "mmol/L",
      units_verification = "verified", role = "measured", allowed_model_roles = "predictor",
      time_semantics = "visit", privacy = "ordinary", verification = "verified",
      evidence = "Synthetic fixture; no participant data.", codebook = "",
      missing_map = ".=placeholder", censoring = "forbidden", value_separator = "",
      time_format = "iso8601_date"),
    c(variable_id = "age", label = "Age", type = "continuous", units = "year",
      units_verification = "verified", role = "measured", allowed_model_roles = "predictor",
      time_semantics = "visit", privacy = "ordinary", verification = "verified",
      evidence = "Synthetic fixture; no participant data.", codebook = "",
      missing_map = ".=placeholder", censoring = "forbidden", value_separator = "",
      time_format = "iso8601_date")
  )
  glucose <- c(2, 4, 6, 8, 10, 12)
  age <- c(40, 42, 41, 55, 50, 48)
  score <- 2 * glucose + 0.1 * age
  rows <- lapply(seq_len(n), function(i) {
    c(subject_id = sprintf("S%02d", i), visit_id = "V1", sample_id = sprintf("SYN_%02d", i),
      time = sprintf("2020-01-%02d", i), time_unit = "day",
      glucose = format(glucose[[i]], trim = TRUE, scientific = FALSE),
      age = format(age[[i]], trim = TRUE, scientific = FALSE))
  })
  write_table <- function(path, records) {
    header <- names(records[[1L]])
    lines <- c(paste(header, collapse = "\t"),
               vapply(records, function(row) paste(unname(row[header]), collapse = "\t"), character(1)))
    writeLines(lines, path, useBytes = TRUE)
  }
  write_table(file.path(dir, "dictionary.tsv"), dictionary)
  write_table(file.path(dir, "observations.tsv"), rows)
  links <- lapply(seq_len(n), function(i) {
    c(link_id = sprintf("L%02d", i), subject_id = sprintf("S%02d", i), visit_id = "V1",
      sample_id = sprintf("SYN_%02d", i), cardinality = "one_to_one", match_rule = "exact_sample",
      reference_time = "", window_days = "", tie_rule = "")
  })
  write_table(file.path(dir, "links.tsv"), links)
  config_path <- file.path(dir, "case.txt")
  writeLines("synthetic-b-t08", config_path)
  config <- list(
    study_id = "triomics_b_t08_fixture",
    clinical = list(
      enabled = TRUE,
      dictionary_file = file.path(dir, "dictionary.tsv"),
      observations_file = file.path(dir, "observations.tsv"),
      links_file = file.path(dir, "links.tsv"),
      objectives = list(list(
        objective_id = "glucose_between", analysis_id = "synthetic_primary", mode = "between_person",
        response_ref = "synthetic_score", predictor_ref = "glucose", endpoint_ref = "synthetic_score",
        adjustments = list(), time_window_ref = "V1", missingness_policy = "complete_case",
        family_ref = "clinical_between_family", hypothesis_status = "exploratory",
        protocol_ref = "synthetic-b-t08"
      ))
    )
  )
  attr(config, "config_sha256") <- triomics_sha256(config_path)
  bundle <- triomics_ingest_clinical(config, config_path, registry)
  samples <- lapply(seq_len(n), function(i) {
    list(sample_id = sprintf("SYN_%02d", i),
         subject_key = bundle$observations[[which(vapply(bundle$observations, function(obs) {
           identical(obs$variable_id, "glucose") && identical(obs$sample_id, sprintf("SYN_%02d", i))
         }, logical(1)))[[1L]]]]$subject_key,
         visit_id = "V1", value = score[[i]])
  })
  endpoint_bundle <- list(
    study_id = "triomics_b_t08_fixture",
    analysis_id = "synthetic_primary",
    endpoints = list(list(
      endpoint_id = "synthetic_score", endpoint_class = "score", response_type = "continuous",
      units = "score_unit", scale = "linear", samples = samples, producing_fit = "synthetic-b-t08",
      training_outcome = NULL, orientation = "native",
      selection = list(ancestry = "prespecified", validity = "qualified", inspected_outcome = NULL),
      available_at_time = "V1", dependency_group = "synthetic", matrix_hash = NULL,
      source_hash = bundle$observations_sha256
    ))
  )
  spec <- list(
    objectives = config$clinical$objectives,
    family = list(family_id = "clinical_between_family", member_ids = "synthetic_score",
                  claim_scope = "between_person_synthetic_scores"),
    trend = FALSE, robust = FALSE, fallback = "none"
  )
  spec$objectives[[1L]]$adjustments <- list(list(
    variable = "age", role = "confounder", form = "level",
    rationale = "Synthetic age balance, not a causal claim."
  ))
  fit <- associate_endpoints(endpoint_bundle, bundle, spec)
  row <- b08_one(fit)
  expect_close(row$effect, 2)
  expect_close(stats::coef(stats::lm(score ~ glucose + age))[["glucose"]], 2)
  testthat::expect_identical(row$n_subjects, 6L)
  testthat::expect_identical(row$predictor_units, "mmol/L")
  exported <- as.character(jsonlite::toJSON(triomics_clinical_association_export(fit), auto_unbox = TRUE))
  testthat::expect_false(grepl("S01", exported, fixed = TRUE))
})
