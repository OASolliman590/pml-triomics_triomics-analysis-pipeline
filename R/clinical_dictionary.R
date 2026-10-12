triomics_clinical_fail <- function(code, message, key, module, remediation) {
  triomics_abort(code, message, key = key, module = module, stage = "A", remediation = remediation)
}

triomics_clinical_pairs <- function(text, key) {
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) return(list())
  parts <- strsplit(text, ";", fixed = TRUE)[[1L]]
  out <- list()
  for (part in parts) {
    if (!nzchar(part)) next
    eq <- regexpr("=", part, fixed = TRUE)[[1L]]
    if (eq < 2L) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        "A code or missing token has no mapped meaning.",
        key, "clinical_dictionary",
        "Declare token=meaning pairs separated by semicolons. Do not guess a mapping."
      )
    }
    token <- substring(part, 1L, eq - 1L)
    meaning <- substring(part, eq + 1L)
    if (!nzchar(token) || !nzchar(meaning) || token %in% names(out)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        "A code or missing-token mapping is blank or repeated.",
        key, "clinical_dictionary",
        "Give each token one non-blank meaning. Do not guess a mapping."
      )
    }
    out[[token]] <- meaning
  }
  out
}

triomics_clinical_read_table <- function(path, key) {
  format <- if (grepl("[.]csv$", path, ignore.case = TRUE)) "csv" else "tsv"
  table <- triomics_read_delimited(path, format)
  if (anyDuplicated(table$header)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      "A clinical table repeats a column name.",
      key, "clinical_dictionary",
      "Use one column per field. Do not merge repeated columns."
    )
  }
  if (length(table$rows) < 1L) {
    triomics_clinical_fail(
      "A_SOURCE_CONTRACT",
      "A clinical table has a header but no rows.",
      key, "clinical_dictionary",
      "Provide at least one data row."
    )
  }
  table
}

triomics_clinical_cell <- function(row, header, name) {
  index <- match(name, header)
  if (is.na(index)) return("")
  value <- row[[index]]
  if (length(value) != 1L || is.na(value)) "" else as.character(value)
}

triomics_clinical_roles <- function(text, key) {
  if (!nzchar(text)) return("excluded")
  parts <- strsplit(text, "|", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  allowed <- c("predictor", "response", "covariate", "excluded")
  if (length(parts) < 1L || any(!parts %in% allowed) || anyDuplicated(parts)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      "A variable declares an unknown model role.",
      key, "clinical_dictionary",
      "Use predictor, response, covariate, or excluded. Do not invent a role."
    )
  }
  parts
}

triomics_load_clinical_dictionary <- function(path, study_id) {
  table <- triomics_clinical_read_table(path, "dictionary_file")
  required <- c(
    "variable_id", "label", "type", "units", "units_verification", "role",
    "allowed_model_roles", "time_semantics", "privacy", "verification", "evidence",
    "codebook", "missing_map", "censoring", "value_separator", "time_format"
  )
  if (any(!required %in% table$header) || any(!table$header %in% required)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      "The clinical dictionary columns do not match the contract.",
      "dictionary_file", "clinical_dictionary",
      "Supply the declared dictionary columns only. Do not add an unreviewed mapping column."
    )
  }
  types <- c("continuous", "binary", "nominal", "ordinal", "count", "date", "multi_response", "text")
  roles <- c("measured", "derived", "device_estimated", "reference", "administrative")
  privacy_values <- c("ordinary", "identifier", "free_text", "sensitive")
  semantics <- c("subject_static", "visit", "sample")
  variables <- list()
  for (row in table$rows) {
    cell <- function(name) triomics_clinical_cell(row, table$header, name)
    variable_id <- cell("variable_id")
    if (!nzchar(variable_id) || variable_id %in% names(variables)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        "A dictionary variable identifier is blank or repeated.",
        "variable_id", "clinical_dictionary",
        "Keep one dictionary row per variable. Do not merge conflicting definitions."
      )
    }
    type <- cell("type")
    role <- cell("role")
    privacy <- cell("privacy")
    units <- cell("units")
    units_verification <- cell("units_verification")
    verification <- cell("verification")
    time_semantics <- cell("time_semantics")
    censoring <- cell("censoring")
    time_format <- cell("time_format")
    if (!type %in% types || !role %in% roles || !privacy %in% privacy_values ||
        !time_semantics %in% semantics || !verification %in% c("verified", "unverified") ||
        !censoring %in% c("allowed", "forbidden") || !time_format %in% c("iso8601_date", "none") ||
        !nzchar(cell("label")) || !nzchar(cell("evidence"))) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", variable_id, "' has an unresolved dictionary field."),
        variable_id, "clinical_dictionary",
        "Use a declared type, role, privacy class, and verification. Do not infer them."
      )
    }
    if (identical(role, "derived")) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Derived variable '", variable_id, "' has no formula provenance."),
        variable_id, "clinical_dictionary",
        "Record the formula and inputs. Do not recompute an undocumented index."
      )
    }
    model_roles <- triomics_clinical_roles(cell("allowed_model_roles"), variable_id)
    if (role %in% c("administrative", "reference") && any(model_roles %in% c("predictor", "response"))) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", variable_id, "' cannot be an administrative or reference predictor."),
        variable_id, "clinical_dictionary",
        "Remove predictor and response roles from administrative and reference variables."
      )
    }
    coded <- type %in% c("binary", "nominal", "ordinal", "multi_response")
    codebook <- triomics_clinical_pairs(cell("codebook"), variable_id)
    if (coded && length(codebook) < 1L) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", variable_id, "' is missing its codebook."),
        variable_id, "clinical_dictionary",
        "Supply the codebook before ingest. Do not guess a mapping."
      )
    }
    if (!coded && length(codebook) > 0L) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", variable_id, "' has a codebook its type does not use."),
        variable_id, "clinical_dictionary",
        "Remove the unused codebook or declare a coded type."
      )
    }
    if (type %in% c("continuous", "count")) {
      if (!identical(units_verification, "verified") || !nzchar(units) || identical(units, "inapplicable")) {
        triomics_clinical_fail(
          "A_CLINICAL_UNIT_UNVERIFIED",
          paste0("Units for variable '", variable_id, "' are unverified."),
          variable_id, "clinical_dictionary",
          "Declare verified units. Do not infer units from the name or magnitude."
        )
      }
    } else if (!identical(units, "inapplicable") || !identical(units_verification, "inapplicable")) {
      triomics_clinical_fail(
        "A_CLINICAL_UNIT_UNVERIFIED",
        paste0("Units for variable '", variable_id, "' do not match its non-numeric type."),
        variable_id, "clinical_dictionary",
        "Use inapplicable units unless the type is continuous or count."
      )
    }
    if (identical(censoring, "allowed") && !identical(type, "continuous")) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", variable_id, "' cannot declare censoring for this type."),
        variable_id, "clinical_dictionary",
        "Allow censoring only for continuous measurements."
      )
    }
    separator <- cell("value_separator")
    if (identical(type, "multi_response")) {
      if (nchar(separator) != 1L) {
        triomics_clinical_fail(
          "A_CLINICAL_DEFINITION_UNRESOLVED",
          paste0("Variable '", variable_id, "' has no single multi-response separator."),
          variable_id, "clinical_dictionary",
          "Declare one separator. Do not split on an inferred delimiter."
        )
      }
    } else if (nzchar(separator)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", variable_id, "' declares a separator it does not use."),
        variable_id, "clinical_dictionary",
        "Leave the separator blank unless the type is multi-response."
      )
    }
    variables[[variable_id]] <- list(
      variable_id = variable_id,
      study_id = study_id,
      label = cell("label"),
      declared_type = type,
      analysis_type = if (identical(verification, "verified")) type else "unverified",
      type = type,
      units = units,
      units_verification = units_verification,
      role = role,
      allowed_model_roles = model_roles,
      time_semantics = time_semantics,
      privacy = privacy,
      verification = verification,
      evidence = cell("evidence"),
      codebook = codebook,
      missing_map = triomics_clinical_pairs(cell("missing_map"), variable_id),
      censoring = censoring,
      value_separator = separator,
      time_format = time_format,
      availability = if (identical(verification, "verified")) "available" else "unverified",
      reason = if (identical(verification, "verified")) NULL else "unverified",
      numeric_values_present = FALSE,
      modelable = FALSE
    )
  }
  variables
}

triomics_clinical_number <- function(token, key) {
  if (!grepl("^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?$", token)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      "A clinical token is not a strict finite number.",
      key, "clinical_dictionary",
      "Reject stray text, Inf, and NaN. Do not coerce a partial number."
    )
  }
  value <- as.numeric(token)
  if (!is.finite(value)) {
    triomics_clinical_fail(
      "A_CLINICAL_DEFINITION_UNRESOLVED",
      "A clinical token is not a finite number.",
      key, "clinical_dictionary",
      "Reject non-finite values."
    )
  }
  value
}

triomics_clinical_censored_token <- function(token) {
  if (!grepl("^(<=|>=|<|>)\\s*(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?$", token, perl = TRUE)) return(NULL)
  parts <- regmatches(token, regexec("^(<=|>=|<|>)\\s*((?:0|[1-9][0-9]*)(?:\\.[0-9]+)?)$", token, perl = TRUE))[[1L]]
  comparator <- parts[[2L]]
  limit_token <- parts[[3L]]
  list(
    comparator = comparator,
    limit_token = limit_token,
    limit = as.numeric(limit_token),
    direction = if (startsWith(comparator, "<")) "left" else "right"
  )
}

triomics_parse_clinical_token <- function(token, variable) {
  key <- variable$variable_id
  if (!is.character(token) || length(token) != 1L || is.na(token)) token <- ""
  fingerprint <- if (nzchar(token)) triomics_hash_text(token) else NULL
  if (token %in% names(variable$missing_map)) {
    reason <- variable$missing_map[[token]]
    return(list(value_kind = if (identical(reason, "placeholder")) "placeholder" else "missing",
                missing_reason = reason, fingerprint = NULL))
  }
  if (!nzchar(token)) {
    return(list(value_kind = "missing", missing_reason = "empty", fingerprint = NULL))
  }
  if (identical(variable$declared_type, "text") ||
      variable$privacy %in% c("identifier", "free_text", "sensitive")) {
    return(list(value_kind = "excluded", exclusion_reason = variable$privacy, fingerprint = fingerprint))
  }
  if (identical(variable$declared_type, "multi_response")) {
    parts <- strsplit(token, variable$value_separator, fixed = TRUE)[[1L]]
    parts <- trimws(parts)
    if (any(!nzchar(parts)) || any(!parts %in% names(variable$codebook))) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", key, "' has a multi-response code outside its codebook."),
        key, "clinical_dictionary",
        "Keep the full set unresolved. Do not drop or truncate an unknown code."
      )
    }
    # Duplicate members are one set element, not a count and not a truncated value.
    values <- sort(unique(parts))
    return(list(value_kind = "set", set_values = values, set_size = as.integer(length(values)),
                fingerprint = triomics_hash_text(paste(values, collapse = "|"))))
  }
  if (variable$declared_type %in% c("binary", "nominal", "ordinal")) {
    if (!token %in% names(variable$codebook)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", key, "' has a code outside its codebook."),
        key, "clinical_dictionary",
        "Do not map a label or unknown code onto another level."
      )
    }
    return(list(value_kind = "code", code_value = token, fingerprint = fingerprint))
  }
  if (variable$declared_type %in% c("continuous", "count")) {
    censored <- triomics_clinical_censored_token(token)
    if (!is.null(censored)) {
      if (!identical(variable$censoring, "allowed")) {
        triomics_clinical_fail(
          "A_CLINICAL_VALUE_CENSORED_UNRESOLVED",
          paste0("Variable '", key, "' has a comparator that its dictionary does not allow."),
          key, "clinical_dictionary",
          "Keep the comparator unresolved. Do not coerce it to a point number."
        )
      }
      censored$value_kind <- "censored"
      censored$fingerprint <- fingerprint
      return(censored)
    }
    if (grepl("^(<=|>=|<|>)", token, perl = TRUE)) {
      triomics_clinical_fail(
        "A_CLINICAL_VALUE_CENSORED_UNRESOLVED",
        paste0("Variable '", key, "' has an unreadable comparator."),
        key, "clinical_dictionary",
        "Do not coerce an unreadable comparator into a number."
      )
    }
    value <- triomics_clinical_number(token, key)
    if (identical(variable$declared_type, "count") && grepl("\\.", token, perl = TRUE)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", key, "' is a count but the token is not an integer."),
        key, "clinical_dictionary",
        "Do not round a count."
      )
    }
    return(list(value_kind = "numeric", numeric_value = value, numeric_token = token, fingerprint = fingerprint))
  }
  if (identical(variable$declared_type, "date")) {
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", token)) {
      triomics_clinical_fail(
        "A_CLINICAL_DEFINITION_UNRESOLVED",
        paste0("Variable '", key, "' is not an ISO date."),
        key, "clinical_dictionary",
        "Do not parse an ambiguous date."
      )
    }
    return(list(value_kind = "date", code_value = token, fingerprint = fingerprint))
  }
  triomics_clinical_fail(
    "A_CLINICAL_DEFINITION_UNRESOLVED",
    paste0("Variable '", key, "' could not be typed."),
    key, "clinical_dictionary",
    "Leave the value unresolved."
  )
}
