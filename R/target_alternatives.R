triomics_target_alternatives <- function(profiles) {
  claim_names <- c('binding', 'flux', 'activity', 'causal_mediation', 'kd', 'binding_probability')
  canonical_axes <- c('metabolic_multiomics_compatibility', 'functional_perturbation',
                      'cellular_tissue_engagement', 'direct_binding')
  claim_text <- function(x) {
    if (!is.atomic(x)) return(FALSE)
    rendered <- if (is.character(x)) x else tryCatch(as.character(x), error = function(e) NA_character_)
    if (!is.character(rendered)) return(FALSE)
    present <- rendered[!is.na(rendered)]
    length(present) >= 1L && any(tolower(present) %in% claim_names)
  }
  label_state <- function(x) {
    if (is.character(x)) return(if (claim_text(x)) 'claim' else 'ok')
    rendered <- tryCatch(as.character(x), error = function(e) NA_character_)
    if (claim_text(rendered)) return('claim')
    'invalid'
  }
  scan_attributes <- function(x) {
    attrs <- attributes(x)
    if (is.null(attrs)) return(NA_character_)
    fields <- names(attrs)
    if (is.null(fields)) fields <- rep(NA_character_, length(attrs))
    present <- fields[!is.na(fields)]
    if (any(tolower(present) %in% claim_names)) return('D_TARGET_CLAIM_COLLAPSED')
    invalid <- FALSE
    for (i in seq_along(attrs)) {
      field <- fields[[i]]
      if (!is.na(field) && identical(field, 'names')) next
      code <- scan(attrs[[i]])
      if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
      if (identical(code, 'D_TARGET_INPUT_INVALID')) invalid <- TRUE
    }
    if (invalid) return('D_TARGET_INPUT_INVALID')
    NA_character_
  }
  scan <- function(x) {
    if (is.environment(x) || isS4(x)) return(NA_character_)
    attribute_code <- scan_attributes(x)
    if (identical(attribute_code, 'D_TARGET_CLAIM_COLLAPSED')) return(attribute_code)
    invalid <- identical(attribute_code, 'D_TARGET_INPUT_INVALID')
    if (is.atomic(x)) {
      if (claim_text(x)) return('D_TARGET_CLAIM_COLLAPSED')
      fields <- names(x)
      if (!is.null(fields)) {
        for (i in seq_along(x)) {
          field <- fields[[i]]
          if (is.na(field)) next
          if (tolower(field) %in% claim_names) return('D_TARGET_CLAIM_COLLAPSED')
          if (!field %in% c('endpoint', 'axes')) next
          state <- label_state(x[[i]])
          if (state == 'claim') return('D_TARGET_CLAIM_COLLAPSED')
          if (state == 'invalid') invalid <- TRUE
        }
      }
      dimensions <- dimnames(x)
      if (!is.null(dimensions)) {
        code <- scan(dimensions)
        if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
        if (identical(code, 'D_TARGET_INPUT_INVALID')) invalid <- TRUE
      }
      if (invalid) return('D_TARGET_INPUT_INVALID')
      return(NA_character_)
    }
    if (is.function(x)) {
      if (is.primitive(x)) return(if (invalid) 'D_TARGET_INPUT_INVALID' else NA_character_)
      code <- scan(as.list(formals(x)))
      if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
      invalid <- invalid || identical(code, 'D_TARGET_INPUT_INVALID')
      code <- scan(body(x))
      if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
      if (identical(code, 'D_TARGET_INPUT_INVALID') || invalid) return('D_TARGET_INPUT_INVALID')
      return(NA_character_)
    }
    if (is.call(x) || is.expression(x)) {
      code <- scan(as.list(x))
      if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
      if (identical(code, 'D_TARGET_INPUT_INVALID') || invalid) return('D_TARGET_INPUT_INVALID')
      return(NA_character_)
    }
    if (!is.list(x)) {
      rendered <- tryCatch(as.character(x), error = function(e) NA_character_)
      if (claim_text(rendered)) return('D_TARGET_CLAIM_COLLAPSED')
      if (invalid) return('D_TARGET_INPUT_INVALID')
      return(NA_character_)
    }
    fields <- names(x)
    if (is.null(fields)) fields <- rep(NA_character_, length(x))
    for (i in seq_along(x)) {
      field <- fields[[i]]
      if (is.na(field)) next
      if (tolower(field) %in% claim_names) return('D_TARGET_CLAIM_COLLAPSED')
      if (!field %in% c('endpoint', 'axes')) next
      state <- label_state(x[[i]])
      if (state == 'claim') return('D_TARGET_CLAIM_COLLAPSED')
      if (state == 'invalid') invalid <- TRUE
    }
    dimensions <- dimnames(x)
    if (!is.null(dimensions)) {
      code <- scan(dimensions)
      if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
      if (identical(code, 'D_TARGET_INPUT_INVALID')) invalid <- TRUE
    }
    for (i in seq_along(x)) {
      code <- scan(x[[i]])
      if (identical(code, 'D_TARGET_CLAIM_COLLAPSED')) return(code)
      if (identical(code, 'D_TARGET_INPUT_INVALID')) invalid <- TRUE
    }
    if (invalid) return('D_TARGET_INPUT_INVALID')
    NA_character_
  }

  prepare_external_guard_profile <- function(profile) {
    unchanged <- list(profile = profile, valid = FALSE)
    if (!is.list(profile) || is.data.frame(profile)) return(unchanged)
    request <- profile[['request', exact = TRUE]]
    records <- profile[['external_evidence', exact = TRUE]]
    if (!is.list(request) || is.data.frame(request) ||
        !is.list(records) || is.data.frame(records) || !is.null(names(records)) ||
        !identical(profile[['request_id', exact = TRUE]], request$request_id) ||
        !identical(profile[['target_id', exact = TRUE]], request$id)) return(unchanged)

    validated <- tryCatch(triomics_target_external_evidence(request, records), error = function(e) NULL)
    if (is.null(validated) || !identical(records, validated$records)) return(unchanged)
    rebuilt <- tryCatch(triomics_target_profile(
      request, profile[['links', exact = TRUE]], profile[['observations', exact = TRUE]],
      external_evidence = list(request_id = request$request_id, records = records)
    ), error = function(e) NULL)
    if (is.null(rebuilt) ||
        !identical(profile[['axes', exact = TRUE]], rebuilt$axes) ||
        !identical(profile[['evidence_axes', exact = TRUE]], rebuilt$evidence_axes) ||
        !identical(profile[['external_evidence', exact = TRUE]], rebuilt$external_evidence) ||
        !identical(profile[['external_associations', exact = TRUE]], rebuilt$external_associations)) {
      return(unchanged)
    }

    sanitize_kd_record <- function(record) {
      if (is.list(record) && identical(as.character(record$axis), 'binding') &&
          triomics_target_string(record$endpoint) && identical(tolower(record$endpoint), 'kd')) {
        record$axis <- 'sourced_external_assay'
        record$endpoint <- 'dissociation_constant'
      }
      record
    }
    guard_profile <- profile
    guard_profile$external_evidence <- lapply(records, sanitize_kd_record)
    guard_profile$evidence_axes$direct_binding$evidence <- lapply(
      profile$evidence_axes$direct_binding$evidence, sanitize_kd_record
    )
    list(profile = guard_profile, valid = TRUE)
  }

  prepared <- lapply(profiles, prepare_external_guard_profile)
  guard_profiles <- lapply(prepared, `[[`, 'profile')
  external_validation_ok <- vapply(prepared, `[[`, logical(1), 'valid')
  code <- scan(guard_profiles)
  if (!is.na(code)) {
    message <- if (identical(code, 'D_TARGET_INPUT_INVALID')) {
      'Endpoint and axis labels must be character.'
    } else {
      'Mechanistic claim axes are not permitted.'
    }
    triomics_d_abort(code, message, 'profiles')
  }
  valid <- is.list(profiles) && !is.data.frame(profiles) && length(profiles) >= 2L &&
    all(vapply(profiles, function(p) {
      is.list(p) && triomics_target_string(p[['target_id', exact = TRUE]]) &&
        is.character(p[['axes', exact = TRUE]]) && !anyNA(p[['axes', exact = TRUE]]) &&
        all(nzchar(trimws(p[['axes', exact = TRUE]]))) &&
        triomics_target_string(p[['coverage', exact = TRUE]]) &&
        p[['coverage', exact = TRUE]] %in% c('measured', 'unmeasured')
    }, logical(1)))
  if (!valid) triomics_d_abort('D_TARGET_INPUT_INVALID', 'At least two valid target profiles are required.', 'profiles')

  same_axes <- all(vapply(profiles, function(profile) {
    setequal(profile[['axes', exact = TRUE]], profiles[[1]][['axes', exact = TRUE]])
  }, logical(1)))

  canonical_intent <- any(vapply(profiles, function(profile) {
    if (!is.list(profile) || is.data.frame(profile)) return(FALSE)
    axes <- profile[['axes', exact = TRUE]]
    !is.null(profile[['evidence_axes', exact = TRUE]]) ||
      (is.character(axes) && any(axes %in% canonical_axes))
  }, logical(1)))
  count_value <- function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) &&
      value >= 0 && value == floor(value)
  }
  valid_axis_coverage <- function(name, axis) {
    if (!is.list(axis) || !triomics_target_string(axis$status) || !is.list(axis$coverage)) return(FALSE)
    expected <- if (identical(name, 'metabolic_multiomics_compatibility')) {
      c('expected_links', 'measured_features', 'unmeasured_features')
    } else {
      c('expected_records', 'measured_records', 'unmeasured_records', 'unresolved_records',
        'independent_publication_groups')
    }
    coverage <- axis$coverage
    if (!setequal(names(coverage), expected) || anyDuplicated(names(coverage)) ||
        !all(vapply(coverage[expected], count_value, logical(1)))) return(FALSE)
    allowed_statuses <- if (identical(name, 'metabolic_multiomics_compatibility')) {
      c('measured', 'unmeasured', 'unassessable')
    } else c('measured', 'unmeasured', 'unresolved')
    if (!axis$status %in% allowed_statuses) return(FALSE)
    if (!identical(name, 'metabolic_multiomics_compatibility')) {
      coverage$measured_records + coverage$unmeasured_records + coverage$unresolved_records ==
        coverage$expected_records && coverage$independent_publication_groups <= coverage$expected_records
    } else TRUE
  }
  canonical_profiles_valid <- all(vapply(profiles, function(profile) {
    if (!is.list(profile) || is.data.frame(profile) ||
        !identical(profile[['axes', exact = TRUE]], canonical_axes)) return(FALSE)
    evidence_axes <- profile[['evidence_axes', exact = TRUE]]
    is.list(evidence_axes) && identical(names(evidence_axes), canonical_axes) &&
      all(vapply(canonical_axes, function(name) valid_axis_coverage(name, evidence_axes[[name]]), logical(1)))
  }, logical(1)))

  if (canonical_intent) {
    canonical_ready <- canonical_profiles_valid && all(external_validation_ok)
    if (canonical_ready) {
    coverage_signatures <- lapply(profiles, function(profile) {
      lapply(profile[['evidence_axes', exact = TRUE]], function(axis) {
        list(status = axis$status, coverage = axis$coverage)
      })
    })
    same_coverage <- all(vapply(coverage_signatures[-1L], function(signature) {
      identical(triomics_canonical_json(signature), triomics_canonical_json(coverage_signatures[[1L]]))
    }, logical(1)))
    each_has_comparable_evidence <- all(vapply(profiles, function(profile) {
      axes <- profile[['evidence_axes', exact = TRUE]]
      statuses <- vapply(axes, `[[`, character(1), 'status')
      no_unresolved_records <- all(vapply(axes[c('functional_perturbation',
        'cellular_tissue_engagement', 'direct_binding')], function(axis) {
          axis$coverage$unresolved_records == 0L
        }, logical(1)))
      any(statuses == 'measured') && !any(statuses %in% c('unresolved', 'unassessable')) &&
        no_unresolved_records
    }, logical(1)))
      same_coverage <- same_coverage && each_has_comparable_evidence
    } else {
      same_coverage <- FALSE
    }
  } else {
    measured <- all(vapply(profiles, function(profile) profile[['coverage', exact = TRUE]] == 'measured', logical(1)))
    same_coverage <- measured
  }
  list(profiles = profiles, comparison_axes = if (canonical_intent) canonical_axes else NULL,
       comparability = if (same_axes && same_coverage) 'comparable' else 'noncomparable',
       selected_target = NULL, tie_status = 'unresolved')
}
