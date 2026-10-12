triomics_clinical_subject_key <- function(study_id, subject_id) {
  paste0("sub_", substr(triomics_hash_text(paste(study_id, subject_id, sep = "\n")), 1L, 16L))
}

triomics_clinical_identifier <- function(value, key) {
  if (is.factor(value) || is.numeric(value)) {
    triomics_clinical_fail(
      "A_ID_COERCED", "An identifier was coerced away from character.",
      key, "clinical_linkage", "Keep identifiers as character so leading zeros survive."
    )
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    triomics_clinical_fail(
      "A_ID_INVALID", "An identifier is missing or blank.",
      key, "clinical_linkage", "Supply one non-blank character identifier."
    )
  }
  value
}

triomics_clinical_iso_date <- function(token, key) {
  if (!is.character(token) || length(token) != 1L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", token)) {
    triomics_clinical_fail(
      "A_CLINICAL_JOIN_AMBIGUOUS", "A clinical time is not an ISO date.",
      key, "clinical_linkage", "Supply YYYY-MM-DD. Do not guess a timezone or nearest visit."
    )
  }
  parsed <- as.Date(token, format = "%Y-%m-%d")
  if (is.na(parsed) || format(parsed, "%Y-%m-%d") != token) {
    triomics_clinical_fail(
      "A_CLINICAL_JOIN_AMBIGUOUS", "A clinical time is not a real ISO date.",
      key, "clinical_linkage", "Reject impossible dates instead of rolling them forward."
    )
  }
  parsed
}

triomics_clinical_day_distance <- function(left, right, key) {
  as.integer(as.numeric(difftime(triomics_clinical_iso_date(left, key),
                                 triomics_clinical_iso_date(right, key), units = "days")))
}

triomics_clinical_registry <- function(registry) {
  if (!is.data.frame(registry) || !"sample_id" %in% names(registry)) {
    triomics_clinical_fail(
      "A_SOURCE_CONTRACT", "Clinical linkage requires the predecessor sample registry.",
      "sample_registry", "clinical_linkage",
      "Pass the sample registry from study ingest. Do not invent sample identifiers."
    )
  }
  pull <- function(name) {
    if (!name %in% names(registry)) return(rep("", nrow(registry)))
    values <- registry[[name]]
    if (is.factor(values) || is.numeric(values)) {
      triomics_clinical_fail(
        "A_ID_COERCED", "Sample registry identifiers must stay character.",
        name, "clinical_linkage", "Read registry identifiers as character."
      )
    }
    out <- as.character(values)
    out[is.na(out)] <- ""
    out
  }
  frame <- data.frame(
    sample_id = pull("sample_id"),
    subject_id = pull("subject_id"),
    visit_id = pull("visit_id"),
    collection_time = pull("collection_time"),
    stringsAsFactors = FALSE
  )
  if (any(!nzchar(frame$sample_id)) || anyDuplicated(frame$sample_id)) {
    triomics_clinical_fail(
      "A_ID_DUPLICATE_SAMPLE", "Sample registry identifiers are blank or duplicated.",
      "sample_id", "clinical_linkage", "Keep one registry row per sample."
    )
  }
  frame
}

triomics_clinical_link_rows <- function(path) {
  table <- triomics_clinical_read_table(path, "links_file")
  required <- c("link_id", "subject_id", "cardinality", "match_rule")
  optional <- c("visit_id", "sample_id", "observation_id", "reference_time", "window_days", "tie_rule")
  if (any(!required %in% table$header) || any(!table$header %in% c(required, optional))) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED", "The link table columns do not match the contract.",
      "links_file", "clinical_linkage", "Do not add an unreviewed join column."
    )
  }
  lapply(seq_along(table$rows), function(i) {
    row <- table$rows[[i]]
    cell <- function(name) triomics_clinical_cell(row, table$header, name)
    list(
      link_id = cell("link_id"),
      subject_id = cell("subject_id"),
      visit_id = cell("visit_id"),
      sample_id = cell("sample_id"),
      cardinality = cell("cardinality"),
      match_rule = cell("match_rule"),
      reference_time = cell("reference_time"),
      window_days = cell("window_days"),
      tie_rule = cell("tie_rule"),
      source_row = as.integer(i)
    )
  })
}

triomics_clinical_resolve_links <- function(observations, links_path, registry, study_id) {
  rows <- triomics_clinical_link_rows(links_path)
  links_sha <- triomics_sha256(links_path)
  frame <- triomics_clinical_registry(registry)
  if (any(vapply(rows, function(row) !nzchar(row$link_id) || !nzchar(row$subject_id), logical(1)))) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED", "A link row is missing its identifier or subject.",
      "links_file", "clinical_linkage", "Do not infer the subject from a sample identifier."
    )
  }
  link_ids <- vapply(rows, function(row) row$link_id, character(1))
  if (anyDuplicated(link_ids)) {
    triomics_clinical_fail(
      "A_CLINICAL_JOIN_AMBIGUOUS", "A link identifier is duplicated.",
      "link_id", "clinical_linkage", "Do not keep one of the duplicated link rows."
    )
  }
  keys <- vapply(rows, function(row) paste(row$subject_id, row$visit_id, sep = "\r"), character(1))
  if (any(duplicated(keys))) {
    triomics_clinical_fail(
      "A_CLINICAL_JOIN_AMBIGUOUS", "More than one link matches the same subject and visit.",
      "links_file", "clinical_linkage", "Refuse the duplicate match. Do not choose a sample."
    )
  }
  resolved <- list()
  for (row in rows) {
    if (!identical(row$cardinality, "one_to_one")) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED", "Only an explicit one-to-one cardinality is resolved.",
        "cardinality", "clinical_linkage", "Do not expand an unreviewed many-match."
      )
    }
    key <- paste(row$subject_id, row$visit_id, sep = "\r")
    result <- list(
      link_id = row$link_id,
      status = "unmatched",
      sample_id = NULL,
      requested_sample_id = if (nzchar(row$sample_id)) row$sample_id else NULL,
      distance_days = NULL,
      cardinality = row$cardinality,
      match_rule = row$match_rule,
      tie_rule = if (nzchar(row$tie_rule)) row$tie_rule else NULL,
      window_days = NULL,
      reference_time = if (nzchar(row$reference_time)) row$reference_time else NULL,
      source_sha256 = links_sha
    )
    if (identical(row$match_rule, "exact_sample")) {
      if (!nzchar(row$sample_id)) {
        triomics_clinical_fail(
          "A_CLINICAL_DEFINITION_UNRESOLVED", "An exact sample link does not name a sample.",
          "sample_id", "clinical_linkage", "Name the sample. Do not search for a nearest one."
        )
      }
      hit <- match(row$sample_id, frame$sample_id)
      if (!is.na(hit)) {
        registry_subject <- frame$subject_id[[hit]]
        if (nzchar(registry_subject) && !identical(registry_subject, row$subject_id)) {
          triomics_clinical_fail(
            "A_CLINICAL_JOIN_AMBIGUOUS", "The sample registry subject disagrees with the link.",
            "sample_id", "clinical_linkage", "Do not relabel the sample to the other subject."
          )
        }
        result$status <- "approved"
        result$sample_id <- frame$sample_id[[hit]]
      }
    } else if (identical(row$match_rule, "exact_visit")) {
      hits <- which(frame$subject_id == row$subject_id & frame$visit_id == row$visit_id & nzchar(frame$subject_id))
      if (length(hits) > 1L) {
        triomics_clinical_fail(
          "A_CLINICAL_JOIN_AMBIGUOUS", "More than one sample shares that subject and visit.",
          "visit_id", "clinical_linkage", "Refuse the duplicate visit match. Do not choose a sample."
        )
      }
      if (length(hits) == 1L) {
        result$status <- "approved"
        result$sample_id <- frame$sample_id[[hits]]
      }
    } else if (identical(row$match_rule, "nearest_visit")) {
      if (!row$tie_rule %in% c("unique_minimum", "refuse_ties")) {
        triomics_clinical_fail(
          "A_CLINICAL_JOIN_AMBIGUOUS", "Nearest-visit matching has no authorized tie rule.",
          "tie_rule", "clinical_linkage",
          "Do not use row order or the nearer sample unless the tie rule is explicit."
        )
      }
      if (!grepl("^(?:0|[1-9][0-9]*)$", row$window_days)) {
        triomics_clinical_fail(
          "A_CLINICAL_JOIN_AMBIGUOUS", "Nearest-visit matching has no finite window.",
          "window_days", "clinical_linkage", "Declare a non-negative day window. Do not use an open window."
        )
      }
      window <- as.integer(row$window_days)
      reference <- row$reference_time
      if (!nzchar(reference)) {
        triomics_clinical_fail(
          "A_CLINICAL_JOIN_AMBIGUOUS", "Nearest-visit matching has no reference time.",
          "reference_time", "clinical_linkage", "A missing date does not authorize nearest-visit matching."
        )
      }
      candidates <- which(frame$subject_id == row$subject_id & nzchar(frame$collection_time))
      if (length(candidates) < 1L) {
        triomics_clinical_fail(
          "A_CLINICAL_JOIN_AMBIGUOUS", "Nearest-visit matching has no dated sample for that subject.",
          "collection_time", "clinical_linkage",
          "A missing date does not authorize nearest-visit matching."
        )
      }
      distances <- vapply(candidates, function(index) {
        abs(triomics_clinical_day_distance(frame$collection_time[[index]], reference, "collection_time"))
      }, integer(1))
      inside <- which(distances <= window)
      if (length(inside) > 0L) {
        best <- min(distances[inside])
        winners <- inside[distances[inside] == best]
        if (length(winners) != 1L) {
          triomics_clinical_fail(
            "A_CLINICAL_JOIN_AMBIGUOUS", "Nearest-visit candidates are tied inside the window.",
            "tie_rule", "clinical_linkage", "Refuse the tie. Do not choose a sample."
          )
        }
        result$status <- "approved"
        result$sample_id <- frame$sample_id[[candidates[[winners]]]]
        result$distance_days <- distances[[winners]]
      }
      result$window_days <- window
      result$reference_time <- reference
    } else {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED", "The link match rule is not a declared rule.",
        "match_rule", "clinical_linkage", "Use exact_sample, exact_visit, or an explicit nearest_visit rule."
      )
    }
    resolved[[key]] <- result
  }
  links <- list()
  for (obs in observations) {
    if (!identical(obs$scope, "visit") && !identical(obs$scope, "sample")) next
    key <- paste(obs$subject_id, obs$visit_id, sep = "\r")
    result <- resolved[[key]]
    if (is.null(result)) {
      result <- list(link_id = NULL, status = "unmatched", sample_id = NULL, requested_sample_id = NULL,
                     distance_days = NULL, cardinality = NULL, match_rule = NULL, tie_rule = NULL,
                     window_days = NULL, reference_time = NULL, source_sha256 = links_sha)
    }
    links[[length(links) + 1L]] <- list(
      link_id = if (is.null(result$link_id)) paste0("unmatched|", obs$observation_id) else paste(result$link_id, obs$observation_id, sep = "|"),
      observation_id = obs$observation_id,
      variable_id = obs$variable_id,
      subject_id = obs$subject_id,
      subject_key = obs$subject_key,
      visit_id = obs$visit_id,
      sample_id = result$sample_id,
      requested_sample_id = result$requested_sample_id,
      unit_id = result$sample_id,
      cardinality = result$cardinality,
      match_rule = result$match_rule,
      tie_rule = result$tie_rule,
      window_days = result$window_days,
      reference_time = result$reference_time,
      distance_days = result$distance_days,
      status = result$status,
      source_sha256 = links_sha,
      ambiguous = FALSE
    )
  }
  links
}

triomics_clinical_coverage <- function(observations, dictionary) {
  if (length(observations) < 1L) return(list())
  keys <- vapply(observations, function(obs) {
    paste(obs$variable_id, obs$scope, if (is.null(obs$visit_id)) "" else obs$visit_id, sep = "\r")
  }, character(1))
  rows <- list()
  for (key in unique(keys)) {
    group <- observations[keys == key]
    variable <- dictionary[[group[[1L]]$variable_id]]
    subjects <- unique(vapply(group, function(obs) obs$subject_key, character(1)))
    kind <- vapply(group, function(obs) obs$value_kind, character(1))
    reason <- vapply(group, function(obs) if (is.null(obs$missing_reason)) "" else obs$missing_reason, character(1))
    rows[[length(rows) + 1L]] <- list(
      variable_id = group[[1L]]$variable_id,
      scope = group[[1L]]$scope,
      visit_id = group[[1L]]$visit_id,
      n_subjects = as.integer(length(subjects)),
      n_observed = as.integer(sum(kind %in% c("numeric", "code", "set", "censored", "date"))),
      n_missing = as.integer(sum(kind == "missing")),
      n_placeholder = as.integer(sum(reason == "placeholder")),
      n_censored = as.integer(sum(kind == "censored")),
      n_multiselect = as.integer(sum(kind == "set")),
      availability = variable$availability
    )
  }
  rows
}

triomics_clinical_public_observation <- function(obs) {
  list(
    observation_id = obs$observation_id,
    subject_key = obs$subject_key,
    scope = obs$scope,
    visit_id = obs$visit_id,
    sample_id = obs$sample_id,
    variable_id = obs$variable_id,
    type = obs$type,
    units = obs$units,
    units_verification = obs$units_verification,
    time_semantics = obs$time_semantics,
    value_kind = obs$value_kind,
    numeric_value = obs$numeric_value,
    numeric_token = obs$numeric_token,
    code_value = obs$code_value,
    set_values = obs$set_values,
    set_size = obs$set_size,
    comparator = obs$comparator,
    limit = obs$limit,
    limit_token = obs$limit_token,
    direction = obs$direction,
    missing_reason = obs$missing_reason,
    time = obs$time,
    time_unit = obs$time_unit,
    source_sha256 = obs$source_sha256,
    source_row = obs$source_row,
    exclusion_reason = obs$exclusion_reason
  )
}

triomics_clinical_public_bundle <- function(bundle) {
  dictionary <- unname(lapply(bundle$dictionary, function(variable) {
    variable$study_id <- bundle$study_id
    variable
  }))
  observations <- lapply(bundle$observations, triomics_clinical_public_observation)
  links <- lapply(bundle$links, function(link) {
    link$subject_id <- NULL
    link
  })
  list(
    study_id = bundle$study_id,
    status = bundle$status,
    dictionary = dictionary,
    observations = observations,
    links = links,
    coverage = bundle$coverage,
    subjects = bundle$subjects,
    visits = bundle$visits,
    objectives = bundle$objectives,
    predictor_variable_ids = bundle$predictor_variable_ids,
    reason_codes = bundle$reason_codes
  )
}
