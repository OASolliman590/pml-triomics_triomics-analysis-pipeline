triomics_target_alternatives <- function(profiles) {
  claim_names <- c('binding', 'flux', 'activity', 'causal_mediation', 'kd', 'binding_probability')
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

  code <- scan(profiles)
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

  same_axes <- all(vapply(profiles, function(p) {
    setequal(p[['axes', exact = TRUE]], profiles[[1]][['axes', exact = TRUE]])
  }, logical(1)))
  measured <- all(vapply(profiles, function(p) p[['coverage', exact = TRUE]] == 'measured', logical(1)))
  list(profiles = profiles, comparability = if (same_axes && measured) 'comparable' else 'noncomparable',
       selected_target = NULL, tie_status = 'unresolved')
}
