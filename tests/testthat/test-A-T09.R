expect_code <- function(expr, code) {
  err <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(err, "triomics_error")
  testthat::expect_identical(err$code, code)
  err
}

write_named_tsv <- function(path, rows) {
  header <- names(rows[[1L]])
  lines <- c(
    paste(header, collapse = "\t"),
    vapply(rows, function(row) paste(unname(row[header]), collapse = "\t"), character(1))
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

dict_row <- function(variable_id, label, type, units, units_verification, role,
                     allowed_model_roles, time_semantics, privacy, verification,
                     codebook = "", missing_map = "", censoring = "forbidden",
                     value_separator = "", time_format = "none") {
  c(
    variable_id = variable_id, label = label, type = type, units = units,
    units_verification = units_verification, role = role,
    allowed_model_roles = allowed_model_roles, time_semantics = time_semantics,
    privacy = privacy, verification = verification,
    evidence = "Synthetic fixture; no participant data.",
    codebook = codebook, missing_map = missing_map, censoring = censoring,
    value_separator = value_separator, time_format = time_format
  )
}

base_dictionary <- function() {
  list(
    dict_row("glucose", "Glucose", "continuous", "mmol/L", "verified", "measured",
             "predictor", "visit", "ordinary", "verified",
             missing_map = ".=placeholder", time_format = "iso8601_date"),
    dict_row("sex", "Sex", "nominal", "inapplicable", "inapplicable", "measured",
             "predictor", "subject_static", "ordinary", "verified",
             codebook = "female=Female;male=Male"),
    dict_row("ldl", "LDL", "continuous", "mmol/L", "verified", "measured",
             "predictor", "visit", "ordinary", "verified",
             missing_map = ".=placeholder", censoring = "allowed",
             time_format = "iso8601_date"),
    dict_row("symptoms", "Symptoms", "multi_response", "inapplicable", "inapplicable",
             "measured", "excluded", "visit", "ordinary", "verified",
             codebook = "headache=Headache;nausea=Nausea;fatigue=Fatigue",
             value_separator = "|", time_format = "iso8601_date"),
    dict_row("bp", "Blood pressure", "continuous", "mmHg", "verified", "measured",
             "predictor", "visit", "ordinary", "verified",
             missing_map = ".=placeholder;N/A=placeholder", time_format = "iso8601_date"),
    dict_row("mrn", "Record number", "text", "inapplicable", "inapplicable",
             "administrative", "excluded", "subject_static", "identifier", "verified"),
    dict_row("note", "Note", "text", "inapplicable", "inapplicable", "measured",
             "excluded", "visit", "free_text", "verified",
             missing_map = ".=placeholder", time_format = "iso8601_date")
  )
}

base_wide_rows <- function() {
  list(
    c(subject_id = "S1", visit_id = "V1", sample_id = "SYN_01", time = "2020-01-01",
      time_unit = "day", glucose = "5.5", sex = "female", ldl = "<0.2",
      symptoms = "nausea|headache|nausea", bp = ".", mrn = "SECRET-MRN-100",
      note = "feels tired today"),
    c(subject_id = "S1", visit_id = "V2", sample_id = "SYN_02", time = "2020-06-01",
      time_unit = "day", glucose = "6.1", sex = "female", ldl = ">=3",
      symptoms = "headache", bp = ".", mrn = "SECRET-MRN-100", note = "no change"),
    c(subject_id = "S2", visit_id = "V1", sample_id = "SYN_03", time = "2020-01-03",
      time_unit = "day", glucose = "4.8", sex = "male", ldl = "1.4",
      symptoms = "headache", bp = ".", mrn = "SECRET-MRN-200", note = "ok"),
    c(subject_id = "S2", visit_id = "V2", sample_id = "SYN_04", time = "2020-06-03",
      time_unit = "day", glucose = ".", sex = "male", ldl = ".",
      symptoms = "", bp = "N/A", mrn = "SECRET-MRN-200", note = ".")
  )
}

wide_to_long <- function(rows) {
  identity <- c("subject_id", "visit_id", "sample_id", "time", "time_unit")
  variables <- setdiff(names(rows[[1L]]), identity)
  out <- list()
  for (row in rows) {
    for (variable in variables) {
      out[[length(out) + 1L]] <- c(
        subject_id = unname(row[["subject_id"]]),
        visit_id = unname(row[["visit_id"]]),
        sample_id = unname(row[["sample_id"]]),
        time = unname(row[["time"]]),
        time_unit = unname(row[["time_unit"]]),
        variable_id = variable,
        value = unname(row[[variable]])
      )
    }
  }
  out
}

base_links <- function() {
  list(
    c(link_id = "L1", subject_id = "S1", visit_id = "V1", sample_id = "SYN_01",
      cardinality = "one_to_one", match_rule = "exact_sample", reference_time = "",
      window_days = "", tie_rule = ""),
    c(link_id = "L2", subject_id = "S1", visit_id = "V2", sample_id = "SYN_02",
      cardinality = "one_to_one", match_rule = "exact_sample", reference_time = "",
      window_days = "", tie_rule = ""),
    c(link_id = "L3", subject_id = "S2", visit_id = "V1", sample_id = "SYN_03",
      cardinality = "one_to_one", match_rule = "exact_sample", reference_time = "",
      window_days = "", tie_rule = ""),
    c(link_id = "L4", subject_id = "S2", visit_id = "V2", sample_id = "SYN_04",
      cardinality = "one_to_one", match_rule = "exact_sample", reference_time = "",
      window_days = "", tie_rule = "")
  )
}

objective_of <- function(objective_id, predictor_ref) {
  list(
    objective_id = objective_id, analysis_id = "synthetic_primary",
    mode = "between_person", response_ref = "synthetic_endpoint",
    predictor_ref = predictor_ref, endpoint_ref = "synthetic_endpoint",
    adjustments = list(), time_window_ref = NULL,
    missingness_policy = "complete_case", family_ref = "clinical_family",
    hypothesis_status = "exploratory", protocol_ref = "synthetic-a-t09"
  )
}

predecessor_study <- function() {
  config_path <- triomics_test_resource("examples", "tickets", "A-T02.yaml")
  config <- triomics_read_config(config_path)
  triomics_ingest_configured_study(config, config_path)
}

ingest_case <- function(dictionary, observations, links, registry, layout = "wide",
                        objectives = NULL) {
  dir <- tempfile("a-t09-")
  dir.create(dir)
  write_named_tsv(file.path(dir, "dictionary.tsv"), dictionary)
  if (identical(layout, "long")) observations <- wide_to_long(observations)
  write_named_tsv(file.path(dir, "observations.tsv"), observations)
  write_named_tsv(file.path(dir, "links.tsv"), links)
  config_path <- file.path(dir, "case.txt")
  writeLines("synthetic-a-t09", config_path)
  if (is.null(objectives)) {
    objectives <- list(
      objective_of("glucose_between", "glucose"),
      objective_of("note_between", "note")
    )
  }
  config <- list(
    study_id = "triomics_a_t09_fixture",
    data_class = "synthetic",
    clinical = list(
      enabled = TRUE,
      dictionary_file = file.path(dir, "dictionary.tsv"),
      observations_file = file.path(dir, "observations.tsv"),
      links_file = file.path(dir, "links.tsv"),
      objectives = objectives
    )
  )
  attr(config, "config_sha256") <- triomics_sha256(config_path)
  bundle <- triomics_ingest_clinical(config, config_path, registry)
  bundle$.case_dir <- dir
  bundle
}

obs_where <- function(bundle, variable_id, predicate) {
  hits <- Filter(function(obs) identical(obs$variable_id, variable_id) && predicate(obs),
                 bundle$observations)
  hits
}

one_obs <- function(bundle, variable_id, predicate) {
  hits <- obs_where(bundle, variable_id, predicate)
  testthat::expect_length(hits, 1L)
  hits[[1L]]
}

coverage_row <- function(bundle, variable_id, visit_id) {
  hits <- Filter(function(row) {
    identical(row$variable_id, variable_id) && identical(row$visit_id, visit_id)
  }, bundle$public$coverage)
  testthat::expect_length(hits, 1L)
  hits[[1L]]
}

testthat::test_that("A-T09-AC1 wide and long inputs keep typed values, units, source and time", {
  study <- predecessor_study()
  testthat::expect_identical(study$clinical_observations, list())
  wide <- ingest_case(base_dictionary(), base_wide_rows(), base_links(), study$sample_registry)
  long <- ingest_case(base_dictionary(), base_wide_rows(), base_links(), study$sample_registry,
                      layout = "long")
  testthat::expect_identical(study$clinical_observations, list())
  testthat::expect_true(all(is.na(study$sample_registry$subject_id)))

  glucose <- function(bundle) {
    sort(vapply(obs_where(bundle, "glucose", function(obs) identical(obs$value_kind, "numeric")),
                function(obs) obs$numeric_token, character(1)))
  }
  testthat::expect_identical(glucose(wide), c("4.8", "5.5", "6.1"))
  testthat::expect_identical(glucose(long), glucose(wide))
  point <- one_obs(wide, "glucose", function(obs) identical(obs$numeric_token, "5.5"))
  testthat::expect_equal(point$numeric_value, 5.5, tolerance = 1e-8)
  testthat::expect_identical(point$units, "mmol/L")
  testthat::expect_identical(point$units_verification, "verified")
  testthat::expect_identical(point$type, "continuous")
  testthat::expect_identical(point$time_semantics, "visit")
  testthat::expect_identical(point$time, "2020-01-01")
  testthat::expect_identical(point$time_unit, "day")
  testthat::expect_identical(point$source_row, 1L)
  testthat::expect_identical(point$source_sha256, triomics_sha256(file.path(wide$.case_dir, "observations.tsv")))
  testthat::expect_identical(point$sample_id, "SYN_01")
  testthat::expect_false(identical(wide$observations_sha256, long$observations_sha256))
  testthat::expect_identical(long$observations[[1L]]$source_sha256, long$observations_sha256)

  sex <- one_obs(wide, "sex", function(obs) identical(obs$code_value, "female"))
  testthat::expect_null(sex$numeric_value)
  testthat::expect_identical(sex$time_semantics, "subject_static")
  testthat::expect_length(obs_where(wide, "sex", function(obs) TRUE), 2L)

  glucose_links <- Filter(function(link) identical(link$variable_id, "glucose") && identical(link$status, "approved"),
                          wide$links)
  paired <- vapply(glucose_links, function(link) paste(link$visit_id, link$sample_id, sep = "="), character(1))
  testthat::expect_identical(sort(paired), c("V1=SYN_01", "V1=SYN_03", "V2=SYN_02", "V2=SYN_04"))

  objective <- Filter(function(item) identical(item$objective_id, "glucose_between"), wide$objectives)[[1L]]
  testthat::expect_identical(objective$status, "declared_not_fitted")
  testthat::expect_false(objective$fitted)
  testthat::expect_null(objective$effect)
  testthat::expect_null(objective$p)
  testthat::expect_identical(objective$owner, "B")

  again <- ingest_case(base_dictionary(), base_wide_rows(), base_links(), study$sample_registry)
  testthat::expect_identical(
    vapply(wide$observations, function(obs) obs$observation_id, character(1)),
    vapply(again$observations, function(obs) obs$observation_id, character(1))
  )

  output <- tempfile("a-t09-artifacts-")
  triomics_write_clinical_artifacts(wide, output)
  envelope <- file.path(output, "clinical", "coverage.envelope.json")
  testthat::expect_true(triomics_validate_envelope(envelope))
  payload <- jsonlite::read_json(file.path(output, "clinical", "coverage.json"), simplifyVector = FALSE)
  testthat::expect_identical(triomics_sha256(file.path(output, "clinical", "coverage.json")),
                             jsonlite::read_json(envelope, simplifyVector = FALSE)$payload_sha256)
  testthat::expect_gte(length(payload), 1L)
})

testthat::test_that("A-T09-AC1 unverified units are not inferred from the variable name", {
  study <- predecessor_study()
  dictionary <- base_dictionary()
  dictionary[[1L]][["units"]] <- ""
  dictionary[[1L]][["units_verification"]] <- "unverified"
  dictionary[[1L]][["variable_id"]] <- "glucose_mg_dl"
  rows <- base_wide_rows()
  rows <- lapply(rows, function(row) {
    names(row)[names(row) == "glucose"] <- "glucose_mg_dl"
    row
  })
  err <- expect_code(
    ingest_case(dictionary, rows, base_links(), study$sample_registry),
    "A_CLINICAL_UNIT_UNVERIFIED"
  )
  testthat::expect_false(grepl("mg/dL", conditionMessage(err), fixed = TRUE))
})

testthat::test_that("A-T09-AC2 static conflicts and visit time conflicts are refused", {
  study <- predecessor_study()
  rows <- base_wide_rows()
  rows[[2L]][["sex"]] <- "male"
  err <- expect_code(ingest_case(base_dictionary(), rows, base_links(), study$sample_registry),
                     "A_CLINICAL_STATIC_CONFLICT")
  testthat::expect_false(grepl("chosen", conditionMessage(err), ignore.case = TRUE))
  testthat::expect_false(grepl("first", conditionMessage(err), ignore.case = TRUE))

  rows <- base_wide_rows()
  rows[[2L]][["time"]] <- "2020-06-02"
  rows[[2L]][["visit_id"]] <- "V1"
  rows[[2L]][["sample_id"]] <- "SYN_01"
  links <- base_links()
  links[[2L]][["visit_id"]] <- "V1"
  links[[2L]][["sample_id"]] <- "SYN_01"
  expect_code(ingest_case(base_dictionary(), rows, links, study$sample_registry),
              "A_CLINICAL_VISIT_CONFLICT")
})

testthat::test_that("A-T09-AC2 missing codebooks and unmapped codes are not guessed", {
  study <- predecessor_study()
  dictionary <- base_dictionary()
  dictionary[[2L]][["codebook"]] <- ""
  expect_code(ingest_case(dictionary, base_wide_rows(), base_links(), study$sample_registry),
              "A_CLINICAL_DEFINITION_UNRESOLVED")

  rows <- base_wide_rows()
  rows[[1L]][["sex"]] <- "Female"
  expect_code(ingest_case(base_dictionary(), rows, base_links(), study$sample_registry),
              "A_CLINICAL_DEFINITION_UNRESOLVED")

  rows <- base_wide_rows()
  rows[[1L]][["symptoms"]] <- "headache|not_a_code"
  expect_code(ingest_case(base_dictionary(), rows, base_links(), study$sample_registry),
              "A_CLINICAL_DEFINITION_UNRESOLVED")
})

testthat::test_that("A-T09-AC2 ambiguous nearest joins and duplicate matches are refused", {
  registry <- data.frame(
    sample_id = c("P1", "P2", "P3"),
    subject_id = c("S9", "S9", "S8"),
    visit_id = c("A", "B", "C"),
    collection_time = c("2020-01-01", "2020-01-11", "2020-01-06"),
    stringsAsFactors = FALSE
  )
  dictionary <- list(dict_row(
    "glucose", "Glucose", "continuous", "mmol/L", "verified", "measured",
    "predictor", "visit", "ordinary", "verified", time_format = "iso8601_date"
  ))
  rows <- list(c(
    subject_id = "S9", visit_id = "V1", sample_id = "", time = "2020-01-06",
    time_unit = "day", glucose = "5"
  ))
  nearest <- function(tie_rule, window = "10", sample_times = registry$collection_time) {
    local <- registry
    local$collection_time <- sample_times
    links <- list(c(
      link_id = "N1", subject_id = "S9", visit_id = "V1", sample_id = "",
      cardinality = "one_to_one", match_rule = "nearest_visit",
      reference_time = "2020-01-06", window_days = window, tie_rule = tie_rule
    ))
    ingest_case(dictionary, rows, links, local, objectives = list(objective_of("glucose_between", "glucose")))
  }
  expect_code(nearest("refuse_ties"), "A_CLINICAL_JOIN_AMBIGUOUS")
  expect_code(nearest(""), "A_CLINICAL_JOIN_AMBIGUOUS")
  expect_code(nearest("first", sample_times = c("2020-01-05", "2020-01-09", "2020-01-06")),
              "A_CLINICAL_JOIN_AMBIGUOUS")
  expect_code(nearest("unique_minimum", sample_times = c("", "", "")), "A_CLINICAL_JOIN_AMBIGUOUS")

  resolved <- nearest("unique_minimum", sample_times = c("2020-01-05", "2020-01-20", "2020-01-06"))
  link <- Filter(function(item) identical(item$variable_id, "glucose"), resolved$links)[[1L]]
  testthat::expect_identical(link$status, "approved")
  testthat::expect_identical(link$sample_id, "P1")
  testthat::expect_identical(link$distance_days, 1L)
  testthat::expect_false(identical(link$sample_id, "P3"))

  study <- predecessor_study()
  links <- base_links()
  links[[5L]] <- c(link_id = "L5", subject_id = "S1", visit_id = "V1", sample_id = "SYN_02",
                   cardinality = "one_to_one", match_rule = "exact_sample",
                   reference_time = "", window_days = "", tie_rule = "")
  expect_code(ingest_case(base_dictionary(), base_wide_rows(), links, study$sample_registry),
              "A_CLINICAL_JOIN_AMBIGUOUS")

  missing <- base_links()
  missing[[1L]][["sample_id"]] <- "SYN_99"
  unmatched <- ingest_case(base_dictionary(), base_wide_rows(), missing, study$sample_registry)
  lost <- Filter(function(item) identical(item$visit_id, "V1") && identical(item$subject_key, unmatched$public$subjects[[1L]]$subject_key),
                 unmatched$links)
  lost <- Filter(function(item) identical(item$variable_id, "glucose") && identical(item$requested_sample_id, "SYN_99"),
                 unmatched$links)
  testthat::expect_length(lost, 1L)
  testthat::expect_identical(lost[[1L]]$status, "unmatched")
  testthat::expect_null(lost[[1L]]$sample_id)
})

testthat::test_that("A-T09-AC2 explicit observation and registry sample identities must agree", {
  study <- predecessor_study()
  registry <- data.frame(
    sample_id = c("SYN_01", "SYN_ALT"), subject_id = c("S1", "S1"),
    visit_id = c("V1", "V1"), collection_time = c("2020-01-01", "2020-01-01"),
    stringsAsFactors = FALSE
  )
  rows <- base_wide_rows()
  rows[[1L]][["sample_id"]] <- "SYN_ALT"
  expect_code(
    ingest_case(base_dictionary(), rows, base_links(), registry),
    "A_CLINICAL_JOIN_AMBIGUOUS"
  )

  links <- base_links()
  links[[1L]][["sample_id"]] <- "SYN_02"
  expect_code(
    ingest_case(base_dictionary(), base_wide_rows(), links, study$sample_registry),
    "A_CLINICAL_JOIN_AMBIGUOUS"
  )
})

testthat::test_that("A-T09-AC2 exact-visit links refuse a contradictory requested sample", {
  registry <- data.frame(
    sample_id = c("P1", "P2"), subject_id = c("S9", "S9"),
    visit_id = c("V1", "V2"), collection_time = c("2020-01-01", "2020-01-11"),
    stringsAsFactors = FALSE
  )
  dictionary <- list(dict_row(
    "glucose", "Glucose", "continuous", "mmol/L", "verified", "measured",
    "predictor", "visit", "ordinary", "verified", time_format = "iso8601_date"
  ))
  rows <- list(c(
    subject_id = "S9", visit_id = "V1", sample_id = "", time = "2020-01-01",
    time_unit = "day", glucose = "5"
  ))
  links <- list(c(
    link_id = "E1", subject_id = "S9", visit_id = "V1", sample_id = "P2",
    cardinality = "one_to_one", match_rule = "exact_visit", reference_time = "",
    window_days = "", tie_rule = ""
  ))
  expect_code(
    ingest_case(dictionary, rows, links, registry,
                objectives = list(objective_of("glucose_between", "glucose"))),
    "A_CLINICAL_JOIN_AMBIGUOUS"
  )
})

testthat::test_that("A-T09-AC2 nearest-visit links refuse a contradictory requested sample", {
  registry <- data.frame(
    sample_id = c("P1", "P2"), subject_id = c("S9", "S9"),
    visit_id = c("V1", "V2"), collection_time = c("2020-01-05", "2020-01-20"),
    stringsAsFactors = FALSE
  )
  dictionary <- list(dict_row(
    "glucose", "Glucose", "continuous", "mmol/L", "verified", "measured",
    "predictor", "visit", "ordinary", "verified", time_format = "iso8601_date"
  ))
  rows <- list(c(
    subject_id = "S9", visit_id = "V1", sample_id = "", time = "2020-01-06",
    time_unit = "day", glucose = "5"
  ))
  links <- list(c(
    link_id = "N1", subject_id = "S9", visit_id = "V1", sample_id = "P2",
    cardinality = "one_to_one", match_rule = "nearest_visit",
    reference_time = "2020-01-06", window_days = "10", tie_rule = "unique_minimum"
  ))
  expect_code(
    ingest_case(dictionary, rows, links, registry,
                objectives = list(objective_of("glucose_between", "glucose"))),
    "A_CLINICAL_JOIN_AMBIGUOUS"
  )
})

testthat::test_that("A-T09-AC1 sample-scoped exact links allow an unspecified visit", {
  registry <- data.frame(
    sample_id = "P1", subject_id = "S9", visit_id = "V1",
    collection_time = "2020-01-05", stringsAsFactors = FALSE
  )
  dictionary <- list(dict_row(
    "glucose", "Glucose", "continuous", "mmol/L", "verified", "measured",
    "predictor", "sample", "ordinary", "verified"
  ))
  rows <- list(c(
    subject_id = "S9", visit_id = "", sample_id = "P1", time = "",
    time_unit = "", glucose = "5"
  ))
  links <- list(c(
    link_id = "S1", subject_id = "S9", visit_id = "", sample_id = "P1",
    cardinality = "one_to_one", match_rule = "exact_sample", reference_time = "",
    window_days = "", tie_rule = ""
  ))
  bundle <- ingest_case(
    dictionary, rows, links, registry,
    objectives = list(objective_of("glucose_between", "glucose"))
  )
  link <- Filter(function(item) identical(item$variable_id, "glucose"), bundle$links)[[1L]]
  testthat::expect_identical(link$status, "approved")
  testthat::expect_identical(link$sample_id, "P1")
})

testthat::test_that("A-T09-AC3 censored limits and multi-select sets survive ingest", {
  study <- predecessor_study()
  bundle <- ingest_case(base_dictionary(), base_wide_rows(), base_links(), study$sample_registry)
  low <- one_obs(bundle, "ldl", function(obs) identical(obs$comparator, "<"))
  testthat::expect_identical(low$value_kind, "censored")
  testthat::expect_identical(low$limit_token, "0.2")
  testthat::expect_equal(low$limit, 0.2, tolerance = 1e-8)
  testthat::expect_identical(low$direction, "left")
  testthat::expect_null(low$numeric_value)
  high <- one_obs(bundle, "ldl", function(obs) identical(obs$comparator, ">="))
  testthat::expect_identical(high$limit_token, "3")
  testthat::expect_identical(high$direction, "right")
  testthat::expect_null(high$numeric_value)
  point <- one_obs(bundle, "ldl", function(obs) identical(obs$value_kind, "numeric"))
  testthat::expect_identical(point$numeric_token, "1.4")
  symptoms <- one_obs(bundle, "symptoms", function(obs) identical(obs$sample_id, "SYN_01"))
  testthat::expect_identical(symptoms$value_kind, "set")
  testthat::expect_identical(symptoms$set_values, c("headache", "nausea"))
  testthat::expect_identical(symptoms$set_size, 2L)
  testthat::expect_null(symptoms$numeric_value)
})

testthat::test_that("A-T09-AC3 all-placeholder fields stay unavailable and censored text is not coerced", {
  study <- predecessor_study()
  bundle <- ingest_case(base_dictionary(), base_wide_rows(), base_links(), study$sample_registry)
  bp <- bundle$dictionary[["bp"]]
  testthat::expect_identical(bp$declared_type, "continuous")
  testthat::expect_identical(bp$analysis_type, "unavailable")
  testthat::expect_identical(bp$availability, "unavailable")
  testthat::expect_identical(bp$reason, "all_placeholder")
  testthat::expect_false(bp$numeric_values_present)
  values <- lapply(obs_where(bundle, "bp", function(obs) TRUE), function(obs) obs$numeric_value)
  testthat::expect_true(all(vapply(values, is.null, logical(1))))
  testthat::expect_true(all(vapply(obs_where(bundle, "bp", function(obs) TRUE), function(obs) {
    identical(obs$value_kind, "unavailable") && identical(obs$missing_reason, "placeholder")
  }, logical(1))))
  testthat::expect_false("bp" %in% bundle$public$predictor_variable_ids)

  rows <- base_wide_rows()
  rows[[1L]][["glucose"]] <- "<5"
  expect_code(ingest_case(base_dictionary(), rows, base_links(), study$sample_registry),
              "A_CLINICAL_VALUE_CENSORED_UNRESOLVED")
})

testthat::test_that("A-T09-AC4 private identifiers and free text stay out of predictors and public outputs", {
  study <- predecessor_study()
  bundle <- ingest_case(base_dictionary(), base_wide_rows(), base_links(), study$sample_registry)
  testthat::expect_identical(sort(bundle$public$predictor_variable_ids), c("glucose", "sex"))
  note <- Filter(function(item) identical(item$objective_id, "note_between"), bundle$objectives)[[1L]]
  testthat::expect_identical(note$status, "unavailable")
  testthat::expect_identical(note$reason, "privacy_excluded")
  testthat::expect_false(note$fitted)
  testthat::expect_null(note$effect)

  public_text <- triomics_canonical_json(bundle$public)
  testthat::expect_false(grepl("SECRET-MRN-100", public_text, fixed = TRUE))
  testthat::expect_false(grepl("SECRET-MRN-200", public_text, fixed = TRUE))
  testthat::expect_false(grepl("feels tired today", public_text, fixed = TRUE))
  testthat::expect_false(grepl("no change", public_text, fixed = TRUE))
  testthat::expect_false(any(vapply(bundle$observations, function(obs) {
    identical(obs$variable_id, "mrn") && !is.null(obs$text_value)
  }, logical(1))))
  testthat::expect_false(any(vapply(bundle$observations, function(obs) {
    identical(obs$variable_id, "note") && !is.null(obs$text_value)
  }, logical(1))))

  glucose_v1 <- coverage_row(bundle, "glucose", "V1")
  glucose_v2 <- coverage_row(bundle, "glucose", "V2")
  testthat::expect_identical(glucose_v1$n_subjects, 2L)
  testthat::expect_identical(glucose_v1$n_observed, 2L)
  testthat::expect_identical(glucose_v1$n_placeholder, 0L)
  testthat::expect_identical(glucose_v2$n_subjects, 2L)
  testthat::expect_identical(glucose_v2$n_observed, 1L)
  testthat::expect_identical(glucose_v2$n_placeholder, 1L)
  bp <- coverage_row(bundle, "bp", "V1")
  testthat::expect_identical(bp$availability, "unavailable")
  testthat::expect_identical(bp$n_placeholder, 2L)
  testthat::expect_identical(bp$n_observed, 0L)
  sex <- coverage_row(bundle, "sex", NULL)
  testthat::expect_identical(sex$scope, "subject_static")
  testthat::expect_identical(sex$n_subjects, 2L)
  testthat::expect_identical(sex$n_observed, 2L)

  output <- tempfile("a-t09-public-")
  triomics_write_clinical_artifacts(bundle, output)
  exported <- paste(vapply(list.files(file.path(output, "clinical"), full.names = TRUE), function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  testthat::expect_false(grepl("SECRET-MRN-100", exported, fixed = TRUE))
  testthat::expect_false(grepl("feels tired today", exported, fixed = TRUE))
  testthat::expect_false(grepl("\"S1\"", exported, fixed = TRUE))
})

testthat::test_that("A-T09-AC4 disabled clinical does not ingest or fit supplied objectives", {
  dir <- tempfile("a-t09-disabled-")
  dir.create(dir)
  secret <- file.path(dir, "observations.tsv")
  writeLines("subject_id\tnote\nS1\tSECRET-MRN-100 feels tired today", secret)
  config_path <- file.path(dir, "case.txt")
  writeLines("disabled", config_path)
  config <- list(
    study_id = "triomics_a_t09_fixture",
    data_class = "synthetic",
    clinical = list(
      enabled = FALSE,
      dictionary_file = secret,
      observations_file = secret,
      links_file = secret,
      objectives = list()
    )
  )
  bundle <- triomics_ingest_clinical(config, config_path, data.frame(sample_id = "SYN_01", stringsAsFactors = FALSE))
  testthat::expect_identical(bundle$status, "not_requested")
  testthat::expect_identical(bundle$observations, list())
  testthat::expect_false(grepl("SECRET-MRN-100", triomics_canonical_json(bundle$public), fixed = TRUE))
  config$clinical$objectives <- list(objective_of("glucose_between", "glucose"))
  expect_code(triomics_ingest_clinical(config, config_path, data.frame(sample_id = "SYN_01", stringsAsFactors = FALSE)),
              "A_CLINICAL_DEFINITION_UNRESOLVED")
})

testthat::test_that("A-T09 Stage A writes clinical artifacts for an enabled clinical ticket", {
  config <- triomics_test_resource("examples", "tickets", "A-T09.yaml")
  output <- tempfile("a-t09-stage-a-")
  triomics_execute_stage_a(config, output)
  clinical_dir <- file.path(output, "clinical")
  payload_names <- c("dictionary", "observations", "links", "coverage", "public_view", "subjects", "visits")
  payloads <- file.path(clinical_dir, paste0(payload_names, ".json"))
  envelopes <- file.path(clinical_dir, paste0(payload_names, ".envelope.json"))
  testthat::expect_true(all(file.exists(c(payloads, envelopes))))

  observations <- jsonlite::read_json(payloads[[2L]], simplifyVector = FALSE)
  glucose <- Filter(function(row) identical(row$variable_id, "glucose"), observations)[[1L]]
  testthat::expect_equal(glucose$numeric_value, 5.4)
  testthat::expect_identical(glucose$units, "mmol/L")
  testthat::expect_identical(glucose$units_verification, "verified")
  testthat::expect_identical(glucose$time_semantics, "visit")
  testthat::expect_identical(glucose$time, "2026-09-20")
  testthat::expect_identical(glucose$time_unit, "day")
  testthat::expect_identical(glucose$source_row, 1L)
  testthat::expect_match(glucose$source_sha256, "^[0-9a-f]{64}$")

  links <- jsonlite::read_json(payloads[[3L]], simplifyVector = FALSE)
  exact_link <- Filter(function(row) identical(row$match_rule, "exact_sample") &&
                         identical(row$sample_id, "SYN_01") && identical(row$status, "approved"), links)
  testthat::expect_length(exact_link, 1L)
  subjects <- jsonlite::read_json(payloads[[6L]], simplifyVector = FALSE)
  testthat::expect_match(subjects[[1L]]$subject_key, "^sub_[0-9a-f]{16}$")
  exported <- paste(vapply(c(payloads, envelopes), function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  testthat::expect_false(grepl("SYN_SUBJECT_01", exported, fixed = TRUE))
  privacy <- vapply(envelopes, function(path) {
    jsonlite::read_json(path, simplifyVector = TRUE)$privacy_class
  }, character(1))
  testthat::expect_true(all(privacy == "synthetic"))
})

stage_a_ticket_variant <- function(variant) {
  path <- triomics_test_resource("examples", "tickets", "A-T09.yaml")
  lines <- readLines(path, warn = FALSE)
  if (identical(variant, "disabled")) {
    lines <- sub("^  enabled: true$", "  enabled: false", lines)
  } else if (identical(variant, "disabled_empty_missing")) {
    lines <- sub("^  enabled: true$", "  enabled: false", lines)
    lines <- sub("^  dictionary_file: .*", "  dictionary_file: ../data/clinical/missing-dictionary.tsv", lines)
    lines <- sub("^  observations_file: .*", "  observations_file: ../data/clinical/missing-observations.tsv", lines)
    lines <- sub("^  links_file: .*", "  links_file: ../data/clinical/missing-links.tsv", lines)
    objective_start <- match("  objectives:", lines)
    annotation_start <- match("annotation:", lines)
    lines <- c(lines[seq_len(objective_start - 1L)], "  objectives: []", lines[annotation_start:length(lines)])
  } else if (identical(variant, "private")) {
    lines <- sub("^data_class: synthetic$", "data_class: real", lines)
    lines <- sub("^  audience: synthetic_demo$", "  audience: private_research", lines)
  } else if (identical(variant, "aggregate")) {
    lines <- sub("^data_class: synthetic$", "data_class: real", lines)
    lines <- sub("^  audience: synthetic_demo$", "  audience: approved_aggregate", lines)
  } else {
    stop("Unknown A-T09 config variant.", call. = FALSE)
  }
  config_path <- tempfile(".A-T09-stage-config-", tmpdir = dirname(path), fileext = ".yaml")
  writeLines(lines, config_path, useBytes = TRUE)
  config_path
}

testthat::test_that("A-T09 Stage A rejects disabled clinical objectives", {
  config <- stage_a_ticket_variant("disabled")
  on.exit(unlink(config), add = TRUE)
  output <- tempfile("a-t09-disabled-stage-a-")
  expect_code(triomics_execute_stage_a(config, output), "A_CLINICAL_DEFINITION_UNRESOLVED")
  testthat::expect_false(file.exists(file.path(output, "registries", "run.json")))
})

testthat::test_that("A-T09 Stage A skips clinical sources when disabled without objectives", {
  config <- stage_a_ticket_variant("disabled_empty_missing")
  on.exit(unlink(config), add = TRUE)
  output <- tempfile("a-t09-disabled-empty-stage-a-")
  triomics_execute_stage_a(config, output)
  testthat::expect_true(file.exists(file.path(output, "A", "assays.json")))
  testthat::expect_true(file.exists(file.path(output, "registries", "run.json")))
  testthat::expect_false(dir.exists(file.path(output, "clinical")))
})

testthat::test_that("A-T09 Stage A labels real private clinical artifacts sensitive", {
  config <- stage_a_ticket_variant("private")
  on.exit(unlink(config), add = TRUE)
  output <- tempfile("a-t09-private-stage-a-")
  triomics_execute_stage_a(config, output)
  envelopes <- list.files(file.path(output, "clinical"), pattern = "\\.envelope\\.json$", full.names = TRUE)
  privacy <- vapply(envelopes, function(path) {
    jsonlite::read_json(path, simplifyVector = TRUE)$privacy_class
  }, character(1))
  testthat::expect_gt(length(envelopes), 0L)
  testthat::expect_true(all(privacy == "sensitive"))
})

testthat::test_that("A-T09 Stage A fails closed for aggregate audience without disclosure policy", {
  config <- stage_a_ticket_variant("aggregate")
  on.exit(unlink(config), add = TRUE)
  output <- tempfile("a-t09-aggregate-stage-a-")
  expect_code(triomics_execute_stage_a(config, output), "A_CLINICAL_DISCLOSURE_UNSUPPORTED")
  testthat::expect_length(list.files(file.path(output, "clinical")), 0L)
})
