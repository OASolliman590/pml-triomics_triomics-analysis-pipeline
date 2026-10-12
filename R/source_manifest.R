triomics_split_delimited_line <- function(line, separator) {
  matches <- gregexpr(separator, line, fixed = TRUE)[[1L]]
  n_sep <- if (identical(matches[[1L]], -1L)) 0L else length(matches)
  parts <- strsplit(line, separator, fixed = TRUE)[[1L]]
  expected <- n_sep + 1L
  if (length(parts) < expected) parts <- c(parts, rep.int("", expected - length(parts)))
  parts
}

# read.delim would rename duplicate headers and coerce leading-zero identifiers.
triomics_read_delimited <- function(path, format) {
  separator <- if (identical(format, "tsv")) {
    "\t"
  } else if (identical(format, "csv")) {
    ","
  } else {
    triomics_abort("A_SOURCE_CONTRACT", "Source format must be tsv or csv.",
                   key = "format", module = "source", stage = "A",
                   remediation = "Declare format tsv or csv.")
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 1L) {
    triomics_abort("A_SOURCE_CONTRACT", "Source file has no header.",
                   key = "file", module = "source", stage = "A",
                   remediation = "Provide a UTF-8 table with one header row.")
  }
  rows <- lapply(lines, triomics_split_delimited_line, separator = separator)
  header <- rows[[1L]]
  body <- rows[-1L]
  if (length(body) > 0L && any(vapply(body, length, integer(1L)) != length(header))) {
    triomics_abort("A_SOURCE_CONTRACT", "A source row does not match the header width.",
                   key = "file", module = "source", stage = "A",
                   remediation = "Keep every row the same width as the header.")
  }
  list(header = header, rows = body)
}

triomics_summary_column_names <- function() {
  c(
    "mean", "means", "group_mean", "group_means", "average", "avg", "median",
    "sd", "se", "sem", "p", "pvalue", "p_value", "p.value", "pval",
    "q", "qvalue", "q_value", "q.value", "qval", "padj", "fdr", "adj_p", "p_adj",
    "adj.p", "fc", "foldchange", "fold_change", "log2fc", "logfc", "log2_fc",
    "log2foldchange", "statistic", "stat", "t", "tstat", "t_statistic", "t_stat",
    "summary", "selected", "selection", "significant", "hit", "hits"
  )
}

triomics_is_summary_column <- function(name) {
  is.character(name) & tolower(name) %in% triomics_summary_column_names()
}

triomics_measurement_pattern <- function() {
  "^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?$"
}

triomics_parse_measurements <- function(tokens, missing_tokens) {
  if (!is.character(tokens)) {
    triomics_abort("A_SOURCE_NUMERIC", "Measurement cells must stay character tokens until parsed.",
                   key = "measurement", module = "source", stage = "A",
                   remediation = "Read the source as text and do not coerce identifiers.")
  }
  if (!is.character(missing_tokens)) {
    triomics_abort("A_SOURCE_CONTRACT", "Missing tokens must be character strings.",
                   key = "missing_tokens", module = "source", stage = "A",
                   remediation = "Declare missing tokens as strings such as NA.")
  }
  pattern <- triomics_measurement_pattern()
  out <- rep(NA_real_, length(tokens))
  for (i in seq_along(tokens)) {
    token <- tokens[[i]]
    if (token %in% missing_tokens) next
    if (!grepl(pattern, token, perl = TRUE) || !is.finite(as.numeric(token))) {
      triomics_abort("A_SOURCE_NUMERIC",
                     "A measurement cell is not a finite number or a declared missing token.",
                     key = "measurement", module = "source", stage = "A",
                     remediation = "Remove stray text, Inf, and NaN, or declare the missing token.")
    }
    out[[i]] <- as.numeric(token)
  }
  out
}

triomics_manifest_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

triomics_column_roles_character <- function(roles) {
  if (is.character(roles) && !is.null(names(roles))) return(roles)
  if (!is.list(roles) || is.null(names(roles)) || any(!nzchar(names(roles)))) {
    triomics_abort("A_SOURCE_ROLE", "Column roles must name every converted column.",
                   key = "column_roles", module = "source", stage = "A",
                   remediation = "Map each header to one declared role.")
  }
  vapply(roles, function(role) {
    if (!triomics_manifest_string(role)) {
      triomics_abort("A_SOURCE_ROLE", "Each column role must be one string.",
                     key = "column_roles", module = "source", stage = "A",
                     remediation = "Use the source-role vocabulary.")
    }
    role
  }, character(1))
}

triomics_check_role_vocabulary <- function(roles) {
  allowed <- c(
    "feature_id", "source_row_id", "alias", "feature_metadata", "measurement",
    "qc", "blank", "standard", "summary", "imported_statistic", "exclusion"
  )
  if (any(!roles %in% allowed)) {
    triomics_abort("A_SOURCE_ROLE", "A column role is outside the source-role vocabulary.",
                   key = "column_roles", module = "source", stage = "A",
                   remediation = "Use feature, measurement, QC, summary, statistic, or exclusion roles.")
  }
  invisible(roles)
}

triomics_json_bounds <- function(region) {
  if (!is.list(region) || is.null(names(region))) {
    triomics_abort("A_SOURCE_CONVERSION", "Workbook region must declare row and column bounds.",
                   key = "region", module = "source", stage = "A",
                   remediation = "Provide row_start, row_end, col_start, and col_end.")
  }
  needed <- c("row_start", "row_end", "col_start", "col_end")
  if (!all(needed %in% names(region))) {
    triomics_abort("A_SOURCE_CONVERSION", "Workbook region is missing a bound.",
                   key = "region", module = "source", stage = "A",
                   remediation = "Provide row_start, row_end, col_start, and col_end.")
  }
  bounds <- lapply(needed, function(key) {
    value <- region[[key]]
    if (is.list(value) && length(value) == 1L) value <- value[[1L]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) ||
        value < 1 || value != trunc(value)) {
      triomics_abort("A_SOURCE_CONVERSION", "Workbook region bounds must be positive integers.",
                     key = "region", module = "source", stage = "A",
                     remediation = "Number the data rectangle, excluding the header row.")
    }
    as.integer(value)
  })
  names(bounds) <- needed
  if (bounds$row_end < bounds$row_start || bounds$col_end < bounds$col_start) {
    triomics_abort("A_SOURCE_CONVERSION", "Workbook region bounds are reversed.",
                   key = "region", module = "source", stage = "A",
                   remediation = "Set each end bound at or after its start.")
  }
  bounds
}

triomics_blank_source_record <- function(source_id, study_id, role, local_ref, format, actual,
                                        selection_history, selection_evidence) {
  list(
    source_id = source_id,
    study_id = study_id,
    role = role,
    local_ref = local_ref,
    format = format,
    sha256 = actual,
    original_ref = NULL,
    original_hash = NULL,
    sheet = NULL,
    region = NULL,
    header_role = NULL,
    column_roles = NULL,
    conversion_evidence = list(mode = "identity"),
    upstream_steps = character(),
    selection_history = selection_history,
    selection_evidence_ref = selection_evidence,
    formula_derived = FALSE,
    cache_status = NULL,
    tolerance = NULL,
    mode = "identity"
  )
}

triomics_source_record <- function(source_id, study_id, path, format, role,
                                   selection_history, selection_evidence,
                                   sha256 = NULL, conversion_manifest = NULL) {
  if (!triomics_manifest_string(source_id) || !triomics_manifest_string(study_id)) {
    triomics_abort("A_ID_INVALID", "Source and study identifiers must be non-blank strings.",
                   key = "source_id", module = "source", stage = "A",
                   remediation = "Supply character source and study ids.")
  }
  if (!triomics_manifest_string(selection_history) || !triomics_manifest_string(selection_evidence)) {
    triomics_abort("A_SOURCE_SELECTION", "Selection history and evidence must be recorded.",
                   key = "selection_history", module = "source", stage = "A",
                   remediation = "Declare selection history and the evidence string.")
  }
  local_ref <- normalizePath(path, winslash = "/", mustWork = TRUE)
  table <- triomics_read_delimited(local_ref, format)
  actual <- triomics_sha256(local_ref)
  if (!is.null(sha256) && !identical(sha256, actual)) {
    triomics_abort("A_SOURCE_HASH_MISMATCH", "Declared SHA-256 does not match the source bytes.",
                   key = "sha256", module = "source", stage = "A",
                   remediation = "Recompute the source hash. Do not reuse a stale digest.")
  }
  record <- triomics_blank_source_record(source_id, study_id, role, local_ref, format, actual,
                                        selection_history, selection_evidence)
  if (is.null(conversion_manifest)) return(record)

  manifest <- jsonlite::read_json(conversion_manifest, simplifyVector = FALSE)
  mode <- manifest$mode
  if (!identical(mode, "identity") && !identical(mode, "external")) {
    triomics_abort("A_SOURCE_CONVERSION", "Conversion mode must be identity or external.",
                   key = "mode", module = "source", stage = "A",
                   remediation = "Use identity for a native canonical file.")
  }
  if (identical(mode, "identity")) {
    fictional <- c("original_ref", "original_sha256", "converter", "converter_version",
                   "options", "sheet", "region", "cache_status")
    for (field in fictional) {
      if (!is.null(manifest[[field]])) {
        triomics_abort("A_SOURCE_CONVERSION",
                       "Identity conversion cannot carry a fictional workbook field.",
                       key = field, module = "source", stage = "A",
                       remediation = "Leave workbook sheet, region, converter, and cache empty for a native file.")
      }
    }
    if (isTRUE(manifest$formula_derived)) {
      triomics_abort("A_SOURCE_CONVERSION", "Identity conversion cannot be formula-derived.",
                     key = "formula_derived", module = "source", stage = "A",
                     remediation = "Use an external conversion manifest for workbook formulas.")
    }
    if (!is.null(manifest$header_role) && !identical(manifest$header_role, "single_header")) {
      triomics_abort("A_SOURCE_CONVERSION", "Header role must be a single header.",
                     key = "header_role", module = "source", stage = "A",
                     remediation = "Declare header_role single_header.")
    }
  } else if (!triomics_manifest_string(manifest$original_ref) ||
             !triomics_sha256_text(manifest$original_sha256) ||
             !triomics_manifest_string(manifest$converter) ||
             !triomics_manifest_string(manifest$converter_version) ||
             !triomics_sha256_text(manifest$canonical_sha256) ||
             !triomics_manifest_string(manifest$sheet) ||
             !identical(manifest$header_role, "single_header")) {
    triomics_abort("A_SOURCE_CONVERSION", "External conversion is missing workbook provenance.",
                   key = "conversion_manifest", module = "source", stage = "A",
                   remediation = "Record the original hash, converter, sheet, region, and canonical hash.")
  }
  if (!is.null(manifest$canonical_sha256) && !identical(manifest$canonical_sha256, actual)) {
    triomics_abort("A_SOURCE_HASH_MISMATCH",
                   "Canonical SHA-256 does not match the converted source bytes.",
                   key = "canonical_sha256", module = "source", stage = "A",
                   remediation = "Rebuild the canonical file and its manifest together.")
  }
  formula <- isTRUE(manifest$formula_derived)
  cache <- manifest$cache_status
  if (formula && !identical(cache, "checked_cached") && !identical(cache, "recalculated")) {
    triomics_abort("A_SOURCE_STALE_CONVERSION",
                   "Formula-derived workbook cache is stale or unverified.",
                   key = "cache_status", module = "source", stage = "A",
                   remediation = "Use checked cached values or recalculate the workbook.")
  }
  roles <- NULL
  if (!is.null(manifest$column_roles)) {
    roles <- triomics_check_role_vocabulary(triomics_column_roles_character(manifest$column_roles))
  } else if (identical(mode, "external")) {
    triomics_abort("A_SOURCE_CONVERSION", "External conversion must declare column roles.",
                   key = "column_roles", module = "source", stage = "A",
                   remediation = "Map each converted header to a source role.")
  }
  region <- NULL
  if (identical(mode, "external")) {
    region <- triomics_json_bounds(manifest$region)
    n_rows <- length(table$rows)
    n_cols <- length(table$header)
    if (!identical(region$row_end - region$row_start + 1L, n_rows) ||
        !identical(region$col_end - region$col_start + 1L, n_cols)) {
      triomics_abort("A_SOURCE_CONVERSION",
                     "Workbook region does not match the canonical data rectangle.",
                     key = "region", module = "source", stage = "A",
                     remediation = "Cover every data row and column, including the feature-id column, and exclude the header.")
    }
  }
  tolerance <- manifest$tolerance
  if (is.list(tolerance) && length(tolerance) == 1L && is.null(names(tolerance))) {
    tolerance <- tolerance[[1L]]
  }
  if (!is.null(tolerance) && (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) ||
                              !is.finite(tolerance) || tolerance < 0)) {
    triomics_abort("A_SOURCE_CONVERSION", "Overlap tolerance must be a non-negative number.",
                   key = "tolerance", module = "source", stage = "A",
                   remediation = "Declare one finite tolerance, or null when cells must match exactly.")
  }
  record$mode <- mode
  record$original_ref <- manifest$original_ref
  record$original_hash <- manifest$original_sha256
  record$sheet <- manifest$sheet
  record$region <- region
  record$header_role <- manifest$header_role
  record$column_roles <- roles
  record$formula_derived <- formula
  record$cache_status <- cache
  record$tolerance <- tolerance
  record$conversion_evidence <- list(
    mode = mode,
    converter = manifest$converter,
    converter_version = manifest$converter_version,
    options = manifest$options,
    formula_derived = formula,
    cache_status = cache
  )
  record
}
