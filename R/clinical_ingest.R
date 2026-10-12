triomics_clinical_blank <- function(status) {
  empty <- list(
    study_id = NULL,
    status = status,
    dictionary = list(),
    observations = list(),
    links = list(),
    coverage = list(),
    subjects = list(),
    visits = list(),
    objectives = list(),
    predictor_variable_ids = character(),
    reason_codes = if (identical(status, "not_requested")) "clinical_not_requested" else character(),
    observations_sha256 = NULL,
    dictionary_sha256 = NULL,
    links_sha256 = NULL,
    config_sha256 = NULL
  )
  empty$public <- list(
    study_id = NULL,
    status = status,
    dictionary = list(),
    observations = list(),
    links = list(),
    coverage = list(),
    subjects = list(),
    visits = list(),
    objectives = list(),
    predictor_variable_ids = character(),
    reason_codes = empty$reason_codes
  )
  empty
}

triomics_clinical_identity_columns <- function() {
  c("subject_id", "sample_id", "visit_id", "specimen", "time", "time_unit", "scope", "source_row")
}

triomics_clinical_observation_shell <- function(study_id, subject_id, variable, source_sha, source_row,
                                               visit_id, sample_id, specimen, time, time_unit, parsed) {
  subject_key <- triomics_clinical_subject_key(study_id, subject_id)
  list(
    observation_id = paste0("obs_", substr(triomics_hash_text(paste(
      study_id, source_sha, source_row, variable$variable_id, subject_id, sep = "\n"
    )), 1L, 24L)),
    study_id = study_id,
    subject_id = subject_id,
    subject_key = subject_key,
    scope = variable$time_semantics,
    visit_id = if (nzchar(visit_id)) visit_id else NULL,
    sample_id = if (nzchar(sample_id)) sample_id else NULL,
    variable_id = variable$variable_id,
    type = variable$declared_type,
    units = variable$units,
    units_verification = variable$units_verification,
    time_semantics = variable$time_semantics,
    role = variable$role,
    privacy = variable$privacy,
    value_kind = parsed$value_kind,
    numeric_value = parsed$numeric_value,
    numeric_token = parsed$numeric_token,
    code_value = parsed$code_value,
    set_values = parsed$set_values,
    set_size = parsed$set_size,
    comparator = parsed$comparator,
    limit = parsed$limit,
    limit_token = parsed$limit_token,
    direction = parsed$direction,
    missing_reason = parsed$missing_reason,
    text_value = NULL,
    exclusion_reason = parsed$exclusion_reason,
    fingerprint = parsed$fingerprint,
    specimen = if (nzchar(specimen)) specimen else NULL,
    time = if (nzchar(time)) time else NULL,
    time_unit = if (nzchar(time_unit)) time_unit else NULL,
    source_ref = "observations",
    source_sha256 = source_sha,
    source_row = as.integer(source_row)
  )
}

triomics_clinical_check_time <- function(variable, time, time_unit, seen_time, seen_unit, subject_id, visit_id) {
  if (nzchar(time) && identical(variable$time_format, "iso8601_date")) {
    triomics_clinical_iso_date(time, variable$variable_id)
  } else if (nzchar(time) && identical(variable$time_semantics, "visit") && identical(variable$time_format, "none")) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      paste0("Variable '", variable$variable_id, "' has a time without a declared format."),
      variable$variable_id, "clinical_ingest", "Declare an ISO date format. Do not parse an arbitrary time."
    )
  }
  if (!nzchar(visit_id)) return(list(time = seen_time, unit = seen_unit))
  key <- paste(subject_id, visit_id, sep = "\r")
  if (nzchar(time)) {
    previous <- seen_time[[key]]
    if (!is.null(previous) && !identical(previous, time)) {
      triomics_clinical_fail(
        "A_CLINICAL_VISIT_CONFLICT",
        "Visit time labels conflict for one subject.",
        "visit_id", "clinical_ingest",
        "Emit the exception and require a resolved mapping. Do not overwrite either time."
      )
    }
    seen_time[[key]] <- time
  }
  if (nzchar(time_unit)) {
    previous <- seen_unit[[key]]
    if (!is.null(previous) && !identical(previous, time_unit)) {
      triomics_clinical_fail(
        "A_CLINICAL_UNIT_UNVERIFIED",
        "Visit time units conflict for one subject.",
        "time_unit", "clinical_ingest", "Do not choose one of the conflicting time units."
      )
    }
    seen_unit[[key]] <- time_unit
  }
  list(time = seen_time, unit = seen_unit)
}

triomics_clinical_row_context <- function(row, header, variable) {
  subject_id <- triomics_clinical_identifier(triomics_clinical_cell(row, header, "subject_id"), "subject_id")
  visit_id <- triomics_clinical_cell(row, header, "visit_id")
  sample_id <- triomics_clinical_cell(row, header, "sample_id")
  if (nzchar(sample_id)) sample_id <- triomics_clinical_identifier(sample_id, "sample_id")
  if (nzchar(visit_id)) visit_id <- triomics_clinical_identifier(visit_id, "visit_id")
  scope <- triomics_clinical_cell(row, header, "scope")
  if (nzchar(scope) && !identical(scope, variable$time_semantics)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      paste0("Variable '", variable$variable_id, "' disagrees with the row scope."),
      "scope", "clinical_ingest", "Do not override the dictionary time semantics."
    )
  }
  if (identical(variable$time_semantics, "visit") && !nzchar(visit_id)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      paste0("Variable '", variable$variable_id, "' is visit-specific but has no visit."),
      "visit_id", "clinical_ingest", "Supply the visit. Do not infer it from an identifier pattern."
    )
  }
  if (identical(variable$time_semantics, "sample") && !nzchar(sample_id)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      paste0("Variable '", variable$variable_id, "' is sample-specific but has no sample."),
      "sample_id", "clinical_ingest", "Supply the sample. Do not infer it from a visit."
    )
  }
  list(
    subject_id = subject_id,
    visit_id = visit_id,
    sample_id = sample_id,
    specimen = triomics_clinical_cell(row, header, "specimen"),
    time = triomics_clinical_cell(row, header, "time"),
    time_unit = triomics_clinical_cell(row, header, "time_unit")
  )
}

triomics_clinical_expand <- function(table, dictionary, study_id, source_sha) {
  header <- table$header
  long <- all(c("variable_id", "value") %in% header)
  identity <- triomics_clinical_identity_columns()
  observations <- list()
  seen_time <- list()
  seen_unit <- list()
  if (long) {
    allowed <- c(identity, "variable_id", "value")
    if (any(!header %in% allowed)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        "The long clinical table mixes wide value columns into the long contract.",
        "observations_file", "clinical_ingest", "Choose one layout. Do not guess which cells are authoritative."
      )
    }
    for (i in seq_along(table$rows)) {
      row <- table$rows[[i]]
      variable_id <- triomics_clinical_cell(row, header, "variable_id")
      variable <- dictionary[[variable_id]]
      if (is.null(variable)) {
        triomics_clinical_fail(
          "A_CLINICAL_DEFINITION_UNRESOLVED",
          "An observation names a variable that is not in the dictionary.",
          "variable_id", "clinical_ingest", "Do not create a dictionary entry from the data."
        )
      }
      context <- triomics_clinical_row_context(row, header, variable)
      checked <- triomics_clinical_check_time(variable, context$time, context$time_unit, seen_time, seen_unit,
                                              context$subject_id, context$visit_id)
      seen_time <- checked$time
      seen_unit <- checked$unit
      parsed <- triomics_parse_clinical_token(triomics_clinical_cell(row, header, "value"), variable)
      observations[[length(observations) + 1L]] <- triomics_clinical_observation_shell(
        study_id, context$subject_id, variable, source_sha, i, context$visit_id, context$sample_id,
        context$specimen, context$time, context$time_unit, parsed
      )
    }
    return(observations)
  }
  if (!"subject_id" %in% header) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED", "A wide clinical table has no subject identifier.",
      "subject_id", "clinical_ingest", "Supply subject_id. Do not infer subjects from sample identifiers."
    )
  }
  for (column in header) {
    if (column %in% identity) next
    if (is.null(dictionary[[column]])) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Wide column '", column, "' is not in the dictionary."),
        column, "clinical_ingest", "Do not treat an unknown column as a clinical variable."
      )
    }
  }
  for (i in seq_along(table$rows)) {
    row <- table$rows[[i]]
    for (column in header) {
      if (column %in% identity) next
      variable <- dictionary[[column]]
      context <- triomics_clinical_row_context(row, header, variable)
      checked <- triomics_clinical_check_time(variable, context$time, context$time_unit, seen_time, seen_unit,
                                              context$subject_id, context$visit_id)
      seen_time <- checked$time
      seen_unit <- checked$unit
      parsed <- triomics_parse_clinical_token(triomics_clinical_cell(row, header, column), variable)
      observations[[length(observations) + 1L]] <- triomics_clinical_observation_shell(
        study_id, context$subject_id, variable, source_sha, i, context$visit_id, context$sample_id,
        context$specimen, context$time, context$time_unit, parsed
      )
    }
  }
  observations
}

triomics_clinical_static_values <- function(observations) {
  if (length(observations) < 1L) return(observations)
  keys <- vapply(observations, function(obs) {
    if (!identical(obs$time_semantics, "subject_static")) return("")
    paste(obs$subject_id, obs$variable_id, sep = "\r")
  }, character(1))
  real_kinds <- c("numeric", "code", "set", "censored", "excluded", "date")
  drop <- rep(FALSE, length(observations))
  for (key in unique(keys[nzchar(keys)])) {
    indexes <- which(keys == key)
    real <- indexes[vapply(observations[indexes], function(obs) obs$value_kind %in% real_kinds, logical(1))]
    fingerprints <- unique(vapply(observations[real], function(obs) {
      if (is.null(obs$fingerprint)) "" else obs$fingerprint
    }, character(1)))
    fingerprints <- fingerprints[nzchar(fingerprints)]
    if (length(fingerprints) > 1L) {
      triomics_clinical_fail(
        "A_CLINICAL_STATIC_CONFLICT",
        "Static values conflict for one subject and variable.",
        "time_semantics", "clinical_ingest",
        "Resolve the source records. Do not select a recorded value."
      )
    }
    keeper <- if (length(real) >= 1L) min(real) else min(indexes)
    drop[setdiff(indexes, keeper)] <- TRUE
    observations[[keeper]]$visit_id <- NULL
    observations[[keeper]]$sample_id <- NULL
    observations[[keeper]]$time <- NULL
    observations[[keeper]]$time_unit <- NULL
  }
  observations[!drop]
}

triomics_clinical_refuse_duplicate_visits <- function(observations) {
  if (length(observations) < 1L) return(invisible(TRUE))
  keys <- vapply(observations, function(obs) {
    if (identical(obs$time_semantics, "subject_static")) return("")
    paste(obs$subject_id, obs$scope, if (is.null(obs$visit_id)) "" else obs$visit_id,
          if (is.null(obs$sample_id)) "" else obs$sample_id, obs$variable_id, sep = "\r")
  }, character(1))
  keys <- keys[nzchar(keys)]
  if (any(duplicated(keys))) {
    triomics_clinical_fail(
      "A_CLINICAL_JOIN_AMBIGUOUS",
      "The same visit variable was recorded more than once.",
      "observation_id", "clinical_ingest",
      "Refuse the duplicate record. Do not keep one copy or average the values."
    )
  }
  invisible(TRUE)
}

triomics_clinical_apply_availability <- function(observations, dictionary) {
  for (variable_id in names(dictionary)) {
    indexes <- which(vapply(observations, function(obs) identical(obs$variable_id, variable_id), logical(1)))
    if (length(indexes) < 1L) {
      dictionary[[variable_id]]$availability <- "absent"
      dictionary[[variable_id]]$analysis_type <- "unavailable"
      dictionary[[variable_id]]$reason <- "absent"
      dictionary[[variable_id]]$modelable <- FALSE
      next
    }
    kinds <- vapply(observations[indexes], function(obs) obs$value_kind, character(1))
    if (all(kinds == "placeholder")) {
      for (index in indexes) {
        observations[[index]]$value_kind <- "unavailable"
        observations[[index]]$missing_reason <- "placeholder"
        observations[[index]]$numeric_value <- NULL
        observations[[index]]$numeric_token <- NULL
      }
      dictionary[[variable_id]]$availability <- "unavailable"
      dictionary[[variable_id]]$analysis_type <- "unavailable"
      dictionary[[variable_id]]$reason <- "all_placeholder"
      dictionary[[variable_id]]$numeric_values_present <- FALSE
    } else if (any(kinds == "censored")) {
      dictionary[[variable_id]]$availability <- "censored"
      dictionary[[variable_id]]$analysis_type <- "censored"
      dictionary[[variable_id]]$reason <- "censored_values_retained"
      dictionary[[variable_id]]$numeric_values_present <- any(kinds == "numeric")
    } else {
      dictionary[[variable_id]]$numeric_values_present <- any(kinds == "numeric")
    }
    variable <- dictionary[[variable_id]]
    dictionary[[variable_id]]$modelable <- identical(variable$verification, "verified") &&
      identical(variable$availability, "available") &&
      "predictor" %in% variable$allowed_model_roles &&
      !variable$privacy %in% c("identifier", "free_text", "sensitive") &&
      !variable$role %in% c("administrative", "reference") &&
      variable$declared_type %in% c("continuous", "count", "binary", "nominal", "ordinal")
  }
  list(observations = observations, dictionary = dictionary)
}

triomics_clinical_subjects_visits <- function(observations, study_id) {
  subjects <- list()
  visits <- list()
  seen_subjects <- character()
  seen_visits <- character()
  for (obs in observations) {
    if (!obs$subject_key %in% seen_subjects) {
      seen_subjects <- c(seen_subjects, obs$subject_key)
      subjects[[length(subjects) + 1L]] <- list(
        study_id = study_id,
        subject_key = obs$subject_key,
        direct_identifier_exported = FALSE
      )
    }
    if (identical(obs$scope, "visit") && !is.null(obs$visit_id)) {
      key <- paste(obs$subject_key, obs$visit_id, sep = "\r")
      if (!key %in% seen_visits) {
        seen_visits <- c(seen_visits, key)
        visits[[length(visits) + 1L]] <- list(
          study_id = study_id,
          subject_key = obs$subject_key,
          visit_id = obs$visit_id,
          time = obs$time,
          time_unit = obs$time_unit,
          source_sha256 = obs$source_sha256
        )
      }
    }
  }
  for (i in seq_along(subjects)) {
    key <- subjects[[i]]$subject_key
    subjects[[i]]$n_visits <- as.integer(sum(vapply(visits, function(visit) identical(visit$subject_key, key), logical(1))))
  }
  list(subjects = subjects, visits = visits)
}

triomics_clinical_objectives <- function(config, dictionary) {
  objectives <- config$clinical$objectives
  if (is.null(objectives)) objectives <- list()
  if (!is.list(objectives) || is.data.frame(objectives)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED", "Clinical objectives must be a list.",
      "objectives", "clinical_ingest", "Declare objectives explicitly. Do not fit an implied endpoint."
    )
  }
  lapply(objectives, function(objective) {
    predictor <- objective$predictor_ref
    variable <- dictionary[[predictor]]
    if (is.null(variable)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED", "An objective names a predictor that is not in the dictionary.",
        "predictor_ref", "clinical_ingest", "Do not create a predictor that was not declared."
      )
    }
    if (variable$privacy %in% c("identifier", "free_text", "sensitive")) {
      status <- "unavailable"
      reason <- "privacy_excluded"
    } else if (!isTRUE(variable$modelable)) {
      status <- "unavailable"
      reason <- if (!is.null(variable$reason)) variable$reason else "not_modelable"
    } else {
      status <- "declared_not_fitted"
      reason <- NULL
    }
    list(
      objective_id = objective$objective_id,
      predictor_ref = predictor,
      response_ref = objective$response_ref,
      endpoint_ref = objective$endpoint_ref,
      mode = objective$mode,
      status = status,
      reason = reason,
      owner = "B",
      fitted = FALSE,
      effect = NULL,
      p = NULL
    )
  })
}

triomics_ingest_clinical <- function(config, config_path, sample_registry) {
  if (!is.list(config) || !is.list(config$clinical)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED", "The configuration has no clinical block.",
      "clinical", "clinical_ingest", "Declare clinical.enabled and its files."
    )
  }
  if (!isTRUE(config$clinical$enabled)) {
    objectives <- config$clinical$objectives
    if (!is.null(objectives) && length(objectives) > 0L) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        "Disabled clinical input cannot carry objectives.",
        "objectives", "clinical_ingest",
        "Remove the objectives or enable clinical ingest. Do not fit them silently."
      )
    }
    bundle <- triomics_clinical_blank("not_requested")
    bundle$study_id <- config$study_id
    bundle$public$study_id <- config$study_id
    return(bundle)
  }
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  dictionary_path <- triomics_resolve_declared_input(config$clinical$dictionary_file, "clinical.dictionary_file", root)
  observations_path <- triomics_resolve_declared_input(config$clinical$observations_file, "clinical.observations_file", root)
  links_path <- triomics_resolve_declared_input(config$clinical$links_file, "clinical.links_file", root)
  dictionary <- triomics_load_clinical_dictionary(dictionary_path, config$study_id)
  source_sha <- triomics_sha256(observations_path)
  observations <- triomics_clinical_expand(
    triomics_clinical_read_table(observations_path, "observations_file"),
    dictionary, config$study_id, source_sha
  )
  observations <- triomics_clinical_static_values(observations)
  triomics_clinical_refuse_duplicate_visits(observations)
  applied <- triomics_clinical_apply_availability(observations, dictionary)
  observations <- applied$observations
  dictionary <- applied$dictionary
  links <- triomics_clinical_resolve_links(observations, links_path, sample_registry, config$study_id)
  coverage <- triomics_clinical_coverage(observations, dictionary)
  people <- triomics_clinical_subjects_visits(observations, config$study_id)
  objectives <- triomics_clinical_objectives(config, dictionary)
  predictors <- names(dictionary)[vapply(dictionary, function(variable) isTRUE(variable$modelable), logical(1))]
  predictors <- sort(predictors)
  reason_codes <- unique(c(
    if (any(vapply(dictionary, function(variable) identical(variable$reason, "all_placeholder"), logical(1)))) "all_placeholder",
    if (any(vapply(dictionary, function(variable) identical(variable$availability, "censored"), logical(1)))) "censored_values_retained",
    if (any(vapply(dictionary, function(variable) variable$privacy %in% c("identifier", "free_text", "sensitive"), logical(1)))) "privacy_excluded"
  ))
  config_sha <- attr(config, "config_sha256", exact = TRUE)
  if (!triomics_sha256_text(config_sha)) config_sha <- triomics_sha256(config_path)
  bundle <- list(
    study_id = config$study_id,
    status = "complete",
    dictionary = dictionary,
    observations = observations,
    links = links,
    coverage = coverage,
    subjects = people$subjects,
    visits = people$visits,
    objectives = objectives,
    predictor_variable_ids = predictors,
    reason_codes = reason_codes,
    observations_sha256 = source_sha,
    dictionary_sha256 = triomics_sha256(dictionary_path),
    links_sha256 = triomics_sha256(links_path),
    config_sha256 = config_sha
  )
  bundle$public <- triomics_clinical_public_bundle(bundle)
  bundle$public$predictor_variable_ids <- predictors
  bundle$public$coverage <- coverage
  bundle
}

triomics_write_clinical_artifacts <- function(bundle, output) {
  if (!triomics_sha256_text(bundle$config_sha256)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED", "Clinical artifacts require the configuration hash.",
      "config_sha256", "clinical_ingest", "Hash the configuration before writing artifacts."
    )
  }
  public <- bundle$public
  inputs <- list(
    list(artifact_id = "clinical-dictionary-source", sha256 = bundle$dictionary_sha256),
    list(artifact_id = "clinical-observations-source", sha256 = bundle$observations_sha256),
    list(artifact_id = "clinical-links-source", sha256 = bundle$links_sha256)
  )
  pieces <- list(
    dictionary = public$dictionary,
    observations = public$observations,
    links = public$links,
    coverage = public$coverage,
    public_view = list(
      study_id = bundle$study_id,
      predictor_variable_ids = public$predictor_variable_ids,
      observations = Filter(function(obs) obs$variable_id %in% public$predictor_variable_ids, public$observations),
      objectives = public$objectives
    ),
    subjects = public$subjects,
    visits = public$visits
  )
  for (name in names(pieces)) {
    payload <- file.path(output, "clinical", paste0(name, ".json"))
    triomics_write_json_atomic(pieces[[name]], payload)
    envelope <- triomics_artifact_envelope(
      artifact_type = paste0("clinical_", name),
      artifact_id = paste0("clinical-", name),
      study_id = bundle$study_id,
      run_id = "a-t09",
      status = bundle$status,
      config_sha256 = bundle$config_sha256,
      payload = payload,
      producer_commit = "not-recorded",
      inputs = inputs,
      reason_codes = bundle$reason_codes,
      privacy_class = "synthetic"
    )
    triomics_write_envelope(envelope, file.path(output, "clinical", paste0(name, ".envelope.json")))
  }
  invisible(output)
}
