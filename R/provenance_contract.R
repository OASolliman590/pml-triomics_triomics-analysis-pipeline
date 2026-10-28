.triomics_provenance_runtime <- new.env(parent = emptyenv())

triomics_source_root_valid <- function(root) {
  if (!is.character(root) || length(root) != 1L || is.na(root) || !dir.exists(root)) return(FALSE)
  description <- file.path(root, "DESCRIPTION")
  source_files <- file.path(root, "R", "export_c.R")
  executable <- file.path(root, "inst", "exec", "triomics")
  if (!file.exists(description) || !file.exists(source_files) || !file.exists(executable)) return(FALSE)
  metadata <- tryCatch(read.dcf(description, fields = "Package"), error = function(error) NULL)
  !is.null(metadata) && nrow(metadata) == 1L && identical(unname(metadata[1L, "Package"]), "triomics")
}

triomics_installed_root_valid <- function(root) {
  if (!is.character(root) || length(root) != 1L || is.na(root) || !dir.exists(root)) return(FALSE)
  description <- file.path(root, "DESCRIPTION")
  if (!file.exists(description)) return(FALSE)
  metadata <- tryCatch(read.dcf(description, fields = "Package"), error = function(error) NULL)
  !is.null(metadata) && nrow(metadata) == 1L && identical(unname(metadata[1L, "Package"]), "triomics")
}

triomics_source_git_revision <- function(root) {
  git_metadata <- file.path(root, ".git")
  if (!file.exists(git_metadata) && !dir.exists(git_metadata)) return("unavailable")
  if (!nzchar(Sys.which("git"))) return("unavailable")
  run_git <- function(arguments) {
    tryCatch(
      system2("git", c("-C", shQuote(root), arguments), stdout = TRUE, stderr = TRUE),
      error = function(error) character()
    )
  }
  repository_root <- run_git(c("rev-parse", "--show-toplevel"))
  if (length(repository_root) != 1L || (!is.null(attr(repository_root, "status")) &&
      !identical(as.integer(attr(repository_root, "status")), 0L))) return("unavailable")
  repository_root <- tryCatch(normalizePath(repository_root, winslash = "/", mustWork = TRUE),
                              error = function(error) "")
  if (!identical(repository_root, root)) return("unavailable")
  status <- run_git(c("status", "--porcelain", "--untracked-files=all"))
  if (length(status) != 0L || (!is.null(attr(status, "status")) &&
      !identical(as.integer(attr(status, "status")), 0L))) return("unavailable")
  revision <- run_git(c("rev-parse", "--verify", "HEAD^{commit}"))
  if (length(revision) == 1L && grepl("^(?:[0-9a-f]{40}|[0-9a-f]{64})$", revision) &&
      (is.null(attr(revision, "status")) || identical(as.integer(attr(revision, "status")), 0L))) {
    revision
  } else {
    "unavailable"
  }
}

triomics_source_revision_from_root <- function(root, installed = NULL) {
  root <- tryCatch(normalizePath(root, winslash = "/", mustWork = TRUE),
                   error = function(error) "")
  source_tree <- nzchar(root) && triomics_source_root_valid(root)
  if (is.null(installed)) installed <- !source_tree
  if (!is.logical(installed) || length(installed) != 1L || is.na(installed) ||
      (isTRUE(installed) && !triomics_installed_root_valid(root)) ||
      (!isTRUE(installed) && !source_tree)) return("unavailable")

  stamp_path <- if (isTRUE(installed)) {
    file.path(root, "triomics-source-revision")
  } else {
    file.path(root, "inst", "triomics-source-revision")
  }
  if (!file.exists(stamp_path) || dir.exists(stamp_path) || triomics_is_symlink(stamp_path)) {
    return(if (isTRUE(installed)) "unavailable" else triomics_source_git_revision(root))
  }
  stamp <- tryCatch(readLines(stamp_path, warn = FALSE), error = function(error) character())
  if (length(stamp) != 1L) return("unavailable")
  if (grepl("^(?:[0-9a-f]{40}|[0-9a-f]{64})$", stamp)) return(stamp)
  if (!isTRUE(installed) && identical(stamp, "$Format:%H$")) {
    return(triomics_source_git_revision(root))
  }
  "unavailable"
}

triomics_source_identity_context <- function() {
  resolver_environment <- environment(triomics_source_identity_context)
  if (!identical(resolver_environment, .GlobalEnv)) {
    namespace_name <- tryCatch(unname(getNamespaceName(resolver_environment)), error = function(error) "")
    if (!identical(namespace_name, "triomics")) return(NULL)
    root <- tryCatch(
      normalizePath(getNamespaceInfo(resolver_environment, "path"), winslash = "/", mustWork = TRUE),
      error = function(error) ""
    )
    if (!nzchar(root)) return(NULL)
    if (triomics_source_root_valid(root)) return(list(root = root, installed = FALSE))
    if (triomics_installed_root_valid(root)) return(list(root = root, installed = TRUE))
    return(NULL)
  }

  source_entrypoint <- exists("triomics_main", envir = .GlobalEnv, inherits = FALSE) &&
    is.function(get("triomics_main", envir = .GlobalEnv, inherits = FALSE)) &&
    identical(environment(get("triomics_main", envir = .GlobalEnv, inherits = FALSE)), .GlobalEnv)
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (source_entrypoint && length(script_arg) == 1L) {
    script_path <- tryCatch(
      normalizePath(sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE),
      error = function(error) ""
    )
    if (nzchar(script_path)) {
      root <- tryCatch(normalizePath(file.path(dirname(script_path), "..", ".."),
                                     winslash = "/", mustWork = TRUE),
                       error = function(error) "")
      expected_script <- if (nzchar(root)) normalizePath(file.path(root, "inst", "exec", "triomics"),
                                                         winslash = "/", mustWork = FALSE) else ""
      if (nzchar(root) && identical(script_path, expected_script) && triomics_source_root_valid(root)) {
        return(list(root = root, installed = FALSE))
      }
    }
  }

  NULL
}

triomics_source_revision <- function() {
  context <- triomics_source_identity_context()
  if (is.null(context)) return("unavailable")
  # Git archive expansion records the build source; it is unsigned metadata, not a
  # signature and not proof against a malicious repackager.
  triomics_source_revision_from_root(context$root, installed = context$installed)
}

triomics_provenance_abort <- function(reason) {
  reason <- as.character(reason)
  if (length(reason) != 1L || is.na(reason) || !nzchar(reason)) {
    reason <- "invalid stage handoff provenance"
  }
  triomics_abort(
    "D_PROVENANCE_INVALID",
    paste0("Invalid Stage Handoff Provenance: ", reason),
    key = "provenance",
    module = "D",
    stage = "D-T01",
    remediation = "Regenerate the producer-owned provenance profile from accepted immutable inputs."
  )
}

triomics_provenance_schema_path <- function() {
  source_root <- getOption("triomics.source_root")
  candidates <- character()
  if (is.character(source_root) && length(source_root) == 1L && !is.na(source_root)) {
    candidates <- c(candidates, file.path(source_root, "inst", "schema", "stage-handoff-provenance.schema.json"))
  }
  installed <- system.file("schema", "stage-handoff-provenance.schema.json", package = "triomics")
  if (nzchar(installed)) candidates <- c(candidates, installed)
  candidates <- c(candidates, file.path(getwd(), "inst", "schema", "stage-handoff-provenance.schema.json"))
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) triomics_provenance_abort("the local JSON Schema is unavailable")
  normalizePath(found[[1L]], winslash = "/", mustWork = TRUE)
}

triomics_provenance_json_scan <- function(json) {
  if (!is.character(json) || length(json) != 1L || is.na(json)) {
    triomics_provenance_abort("JSON input must be one non-missing character string")
  }
  utf8 <- tryCatch(enc2utf8(json), error = function(error) NA_character_)
  if (is.na(utf8) || is.na(iconv(utf8, from = "UTF-8", to = "UTF-8", sub = NA_character_))) {
    triomics_provenance_abort("JSON input contains invalid UTF-8")
  }
  characters <- strsplit(utf8, "", fixed = TRUE)[[1L]]
  length_input <- length(characters)
  position <- 1L
  numbers <- list()
  fail <- function(reason) triomics_provenance_abort(reason)
  current <- function() {
    if (position > length_input) "" else characters[[position]]
  }
  skip_space <- function() {
    while (position <= length_input && current() %in% c(" ", "\t", "\n", "\r")) {
      position <<- position + 1L
    }
  }
  parse_string <- function() {
    if (!identical(current(), "\"")) fail("expected a JSON string")
    start <- position
    position <<- position + 1L
    repeat {
      if (position > length_input) fail("unterminated JSON string")
      character <- current()
      if (identical(character, "\"")) {
        token <- paste(characters[start:position], collapse = "")
        position <<- position + 1L
        value <- tryCatch(jsonlite::fromJSON(token, simplifyVector = FALSE), error = function(error) NULL)
        if (!is.character(value) || length(value) != 1L || is.na(value)) {
          fail("JSON string contains invalid Unicode or escapes")
        }
        return(enc2utf8(value))
      }
      codepoint <- utf8ToInt(character)
      if (length(codepoint) != 1L || codepoint < 0x20L) fail("JSON strings cannot contain raw control characters")
      if (identical(character, "\\")) {
        escape_index <- position + 1L
        if (escape_index > length_input) fail("unterminated JSON escape")
        escape <- characters[[escape_index]]
        if (escape %in% c("\"", "\\", "/", "b", "f", "n", "r", "t")) {
          position <<- escape_index + 1L
        } else if (identical(escape, "u")) {
          digits_start <- escape_index + 1L
          digits_end <- digits_start + 3L
          if (digits_end > length_input) fail("short Unicode escape")
          digits <- paste(characters[digits_start:digits_end], collapse = "")
          if (!grepl("^[0-9A-Fa-f]{4}$", digits)) fail("malformed Unicode escape")
          scalar <- strtoi(digits, base = 16L)
          after_escape <- digits_end + 1L
          if (scalar >= 0xD800L && scalar <= 0xDBFFL) {
            if (after_escape + 5L > length_input ||
                !identical(characters[[after_escape]], "\\") ||
                !identical(characters[[after_escape + 1L]], "u")) {
              fail("lone high surrogate in JSON string")
            }
            low_digits <- paste(characters[(after_escape + 2L):(after_escape + 5L)], collapse = "")
            if (!grepl("^[0-9A-Fa-f]{4}$", low_digits)) fail("malformed low-surrogate escape")
            low <- strtoi(low_digits, base = 16L)
            if (low < 0xDC00L || low > 0xDFFFL) fail("invalid low surrogate in JSON string")
            position <<- after_escape + 6L
          } else if (scalar >= 0xDC00L && scalar <= 0xDFFFL) {
            fail("lone low surrogate in JSON string")
          } else {
            position <<- after_escape
          }
        } else {
          fail("malformed JSON escape")
        }
      } else {
        position <<- position + 1L
      }
    }
  }
  parse_value <- NULL
  parse_array <- function(path) {
    position <<- position + 1L
    skip_space()
    if (identical(current(), "]")) {
      position <<- position + 1L
      return(invisible(NULL))
    }
    index <- 1L
    repeat {
      parse_value(c(path, as.character(index)))
      skip_space()
      if (identical(current(), "]")) {
        position <<- position + 1L
        return(invisible(NULL))
      }
      if (!identical(current(), ",")) fail("expected a comma or closing array bracket")
      position <<- position + 1L
      skip_space()
      index <- index + 1L
    }
  }
  parse_object <- function(path) {
    position <<- position + 1L
    skip_space()
    if (identical(current(), "}")) {
      position <<- position + 1L
      return(invisible(NULL))
    }
    seen <- character()
    repeat {
      key <- parse_string()
      if (key %in% seen) fail(paste0("duplicate JSON object member '", key, "'"))
      seen <- c(seen, key)
      skip_space()
      if (!identical(current(), ":")) fail("expected a colon after an object member name")
      position <<- position + 1L
      skip_space()
      parse_value(c(path, key))
      skip_space()
      if (identical(current(), "}")) {
        position <<- position + 1L
        return(invisible(NULL))
      }
      if (!identical(current(), ",")) fail("expected a comma or closing object brace")
      position <<- position + 1L
      skip_space()
    }
  }
  parse_value <- function(path) {
    skip_space()
    character <- current()
    if (identical(character, "{")) return(parse_object(path))
    if (identical(character, "[")) return(parse_array(path))
    if (identical(character, "\"")) {
      parse_string()
      return(invisible(NULL))
    }
    if (character %in% c("t", "f", "n")) {
      literal <- switch(character, t = "true", f = "false", n = "null")
      end <- position + nchar(literal, type = "chars") - 1L
      if (end > length_input || !identical(paste(characters[position:end], collapse = ""), literal)) {
        fail("malformed JSON literal")
      }
      position <<- end + 1L
      return(invisible(NULL))
    }
    rest <- if (position <= length_input) paste(characters[position:length_input], collapse = "") else ""
    match <- regexpr("^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?", rest, perl = TRUE)
    if (match[[1L]] != 1L) fail("malformed JSON value")
    token <- regmatches(rest, match)
    numbers[[length(numbers) + 1L]] <<- list(path = path, token = token)
    position <<- position + nchar(token, type = "chars")
    invisible(NULL)
  }
  skip_space()
  parse_value(character())
  skip_space()
  if (position <= length_input) fail("unexpected content after the JSON value")
  numbers
}

triomics_provenance_validate_identity_number_tokens <- function(number_tokens) {
  for (entry in number_tokens) {
    token <- entry$token
    if (!grepl("^-?(?:0|[1-9][0-9]*)$", token)) {
      triomics_provenance_abort("triomics identity numbers must be exact integer JSON tokens")
    }
    if (identical(token, "-0")) triomics_provenance_abort("negative zero is forbidden in triomics identity")
    value <- suppressWarnings(as.numeric(token))
    if (!is.finite(value) || abs(value) > 9007199254740991) {
      triomics_provenance_abort("triomics identity integer exceeds the exact IEEE-754 safe range")
    }
  }
  invisible(TRUE)
}

triomics_provenance_validate_identity_numbers <- function(value, path = "identity") {
  if (is.numeric(value)) {
    if (length(value) > 0L && (anyNA(value) || any(!is.finite(value)))) {
      triomics_provenance_abort(paste0(path, " contains NaN or infinity"))
    }
    if (length(value) > 0L && any(value == 0 & is.infinite(1 / value) & (1 / value < 0))) {
      triomics_provenance_abort(paste0(path, " contains negative zero"))
    }
    if (length(value) > 0L && any(value != trunc(value))) {
      triomics_provenance_abort(paste0(path, " contains a floating-point value"))
    }
    if (length(value) > 0L && any(abs(as.double(value)) > 9007199254740991)) {
      triomics_provenance_abort(paste0(path, " contains an integer outside the exact IEEE-754 safe range"))
    }
    return(invisible(TRUE))
  }
  if (is.list(value)) {
    for (index in seq_along(value)) {
      child <- if (!is.null(names(value)) && nzchar(names(value)[[index]])) {
        paste0(path, ".", names(value)[[index]])
      } else {
        paste0(path, "[", index, "]")
      }
      triomics_provenance_validate_identity_numbers(value[[index]], child)
    }
  }
  invisible(TRUE)
}

triomics_provenance_jcs_context <- function() {
  if (!requireNamespace("V8", quietly = TRUE)) {
    triomics_provenance_abort("the V8 runtime required for RFC 8785 serialization is unavailable")
  }
  if (is.null(.triomics_provenance_runtime$jcs_context)) {
    context <- V8::v8()
    context$eval(paste(
      "function triomicsCanonicalize(input, normalizeIdentity) {",
      "  var root = JSON.parse(input);",
      "  if (normalizeIdentity && root && typeof root === 'object' && !Array.isArray(root)) {",
      "    if (Array.isArray(root.entities)) root.entities.sort(function(a, b) {",
      "      return triomicsCompareTuple(a, b, ['scientific_role', 'entity_type', 'entity_id']);",
      "    });",
      "    if (Array.isArray(root.declared_routes)) root.declared_routes.sort(function(a, b) {",
      "      return triomicsCompareTuple(a, b, ['route_kind', 'route_id']);",
      "    });",
      "  }",
      "  function triomicsCompareTuple(a, b, fields) {",
      "    for (var i = 0; i < fields.length; i++) {",
      "      var left = a[fields[i]], right = b[fields[i]];",
      "      if (left < right) return -1; if (left > right) return 1;",
      "    } return 0;",
      "  }",
      "  function serialize(value) {",
      "    if (value === null) return 'null';",
      "    if (typeof value === 'string' || typeof value === 'boolean') return JSON.stringify(value);",
      "    if (typeof value === 'number') {",
      "      if (!Number.isFinite(value)) throw new Error('non-finite number');",
      "      return JSON.stringify(value);",
      "    }",
      "    if (Array.isArray(value)) return '[' + value.map(serialize).join(',') + ']';",
      "    if (typeof value === 'object') {",
      "      var keys = Object.keys(value).sort();",
      "      return '{' + keys.map(function(key) { return JSON.stringify(key) + ':' + serialize(value[key]); }).join(',') + '}';",
      "    }",
      "    throw new Error('unsupported JSON value');",
      "  }",
      "  return serialize(root);",
      "}",
      collapse = "\n"
    ))
    .triomics_provenance_runtime$jcs_context <- context
  }
  .triomics_provenance_runtime$jcs_context
}

triomics_jcs_canonicalize_json <- function(json) {
  triomics_provenance_json_scan(json)
  context <- triomics_provenance_jcs_context()
  result <- tryCatch(context$call("triomicsCanonicalize", json, FALSE), error = function(error) error)
  if (inherits(result, "error") || !is.character(result) || length(result) != 1L || is.na(result)) {
    reason <- if (inherits(result, "error")) conditionMessage(result) else "canonicalizer returned no JSON text"
    triomics_provenance_abort(paste0("RFC 8785 canonicalization failed: ", reason))
  }
  enc2utf8(result)
}

triomics_provenance_identity_sha256 <- function(identity) {
  if (is.character(identity) && length(identity) == 1L && !is.na(identity)) {
    number_tokens <- triomics_provenance_json_scan(identity)
    triomics_provenance_validate_identity_number_tokens(number_tokens)
    parsed <- tryCatch(jsonlite::fromJSON(identity, simplifyVector = FALSE), error = function(error) error)
    if (inherits(parsed, "error") || !is.list(parsed)) {
      triomics_provenance_abort("identity must be a JSON object")
    }
    json <- identity
  } else {
    if (!is.list(identity) || is.null(names(identity))) {
      triomics_provenance_abort("identity must be an object")
    }
    triomics_provenance_validate_identity_numbers(identity)
    json <- tryCatch(as.character(jsonlite::toJSON(
      identity, auto_unbox = TRUE, null = "null", digits = NA, force = TRUE
    )), error = function(error) error)
    if (inherits(json, "error") || length(json) != 1L || is.na(json)) {
      triomics_provenance_abort("identity cannot be represented as JSON")
    }
  }
  normalized <- triomics_provenance_jcs_context()$call("triomicsCanonicalize", json, TRUE)
  digest::digest(charToRaw(enc2utf8(normalized)), algo = "sha256", serialize = FALSE)
}

triomics_provenance_profile_json <- function(profile) {
  if (is.character(profile) && length(profile) == 1L && !is.na(profile)) return(profile)
  if (!is.list(profile) || is.null(names(profile))) {
    triomics_provenance_abort("profile must be an object or raw JSON text")
  }
  triomics_provenance_validate_identity_numbers(profile$identity)
  json <- tryCatch(as.character(jsonlite::toJSON(
    profile, auto_unbox = TRUE, null = "null", digits = NA, force = TRUE
  )), error = function(error) error)
  if (inherits(json, "error") || length(json) != 1L || is.na(json)) {
    triomics_provenance_abort("profile cannot be represented as JSON")
  }
  json
}

triomics_provenance_validate_schema <- function(json) {
  schema_path <- triomics_provenance_schema_path()
  validation <- tryCatch(jsonvalidate::json_validate(
    json, schema_path, engine = "ajv", verbose = TRUE
  ), error = function(error) error)
  if (inherits(validation, "error")) {
    triomics_provenance_abort(paste0("JSON Schema validation failed: ", conditionMessage(validation)))
  }
  valid <- if (is.list(validation) && !is.null(validation$valid)) isTRUE(validation$valid) else isTRUE(validation)
  if (!valid) triomics_provenance_abort("profile does not match the closed Stage Handoff Provenance schema")
  invisible(TRUE)
}

triomics_provenance_role_contract <- function() {
  list(
    source_manifest = list(type = "triomics.source_manifest", scope = character(), row = FALSE, column = FALSE),
    normalized_metadata = list(type = "triomics.normalized_metadata", scope = "analysis_id", row = TRUE, column = TRUE),
    feature_registry = list(type = "triomics.feature_registry", scope = "block_id", row = TRUE, column = FALSE),
    analysis_unit_map = list(type = "triomics.analysis_unit_map", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = TRUE, column = FALSE),
    resolved_analysis = list(type = "triomics.resolved_analysis", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = TRUE, column = TRUE),
    represented_matrix = list(type = "triomics.represented_matrix", scope = c("analysis_id", "block_id", "preparation_context_id", "representation_id"), row = TRUE, column = TRUE),
    preprocessing_state = list(type = "triomics.preprocessing_state", scope = c("analysis_id", "block_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    method_state = list(type = "triomics.method_state", scope = c("analysis_id", "method_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    predecessor_envelope = list(type = "triomics.predecessor_envelope", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    predecessor_payload = list(type = "triomics.predecessor_payload", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    block_results = list(type = "triomics.block_results", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    family_ledger = list(type = "triomics.family_ledger", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    candidate_nominations = list(type = "triomics.candidate_nominations", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    integration_result = list(type = "triomics.integration_result", scope = c("analysis_id", "method_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    integration_state = list(type = "triomics.integration_state", scope = c("analysis_id", "method_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    evaluation_evidence = list(type = "triomics.evaluation_evidence", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE),
    clinical_evidence = list(type = "triomics.clinical_evidence", scope = c("analysis_id", "preparation_context_id", "representation_id", "route_id"), row = FALSE, column = FALSE),
    native_sidecar = list(type = "triomics.native_sidecar", scope = c("analysis_id", "method_id", "preparation_context_id", "representation_id", "route_id"), row = FALSE, column = FALSE),
    handoff_payload = list(type = "triomics.handoff_payload", scope = c("analysis_id", "preparation_context_id", "representation_id"), row = FALSE, column = FALSE)
  )
}

triomics_provenance_list <- function(value, key) {
  result <- value[[key]]
  if (!is.list(result)) triomics_provenance_abort(paste0("identity.", key, " must be an array"))
  result
}

triomics_provenance_assert_sorted_unique <- function(values, label, allow_empty = TRUE) {
  values <- unlist(values, use.names = FALSE)
  if (is.null(values)) values <- character()
  if (!allow_empty && length(values) == 0L) triomics_provenance_abort(paste0(label, " must not be empty"))
  if (anyNA(values) || any(!nzchar(values))) triomics_provenance_abort(paste0(label, " contains an empty value"))
  if (!identical(values, sort(unique(values), method = "radix"))) {
    triomics_provenance_abort(paste0(label, " must be sorted and unique"))
  }
  invisible(TRUE)
}

triomics_provenance_group_key <- function(entity, fields) {
  if (length(fields) == 0L) return("profile")
  values <- enc2utf8(vapply(fields, function(field) as.character(entity[[field]]), character(1)))
  encoded_values <- paste0(nchar(values, type = "bytes"), ":", values)
  paste0(length(values), ":", paste0(encoded_values, collapse = ""))
}

triomics_provenance_exactly_one_per <- function(entities, role, fields, required = TRUE) {
  matching <- Filter(function(entity) identical(entity$scientific_role, role), entities)
  if (length(matching) == 0L) {
    if (required) triomics_provenance_abort(paste0("required role is missing: ", role))
    return(character())
  }
  keys <- vapply(matching, triomics_provenance_group_key, character(1), fields = fields)
  if (anyDuplicated(keys)) triomics_provenance_abort(paste0("role has duplicate cardinality group: ", role))
  keys
}

triomics_provenance_assert_acyclic <- function(entity_ids, edges, label) {
  states <- new.env(hash = TRUE, parent = emptyenv())
  visit <- function(entity_id) {
    state <- if (exists(entity_id, envir = states, inherits = FALSE)) get(entity_id, envir = states) else 0L
    if (state == 1L) triomics_provenance_abort(paste0(label, " graph contains a cycle"))
    if (state == 2L) return(invisible(NULL))
    assign(entity_id, 1L, envir = states)
    for (parent in edges[[entity_id]]) visit(parent)
    assign(entity_id, 2L, envir = states)
    invisible(NULL)
  }
  for (entity_id in entity_ids) visit(entity_id)
  invisible(TRUE)
}

triomics_provenance_validate_profile <- function(profile, frame_payload_id, frame_envelope_id) {
  roles <- triomics_provenance_role_contract()
  role_names <- names(roles)
  if (!profile$producer_stage %in% c("B", "C")) triomics_provenance_abort("producer_stage must be B or C")
  identity <- profile$identity
  entities <- triomics_provenance_list(identity, "entities")
  activities <- triomics_provenance_list(identity, "activities")
  routes <- triomics_provenance_list(identity, "declared_routes")
  entity_ids <- vapply(entities, function(entity) entity$entity_id, character(1))
  activity_ids <- vapply(activities, function(activity) activity$activity_id, character(1))
  if (anyDuplicated(entity_ids)) triomics_provenance_abort("entity_id values must be globally unique")
  if (anyDuplicated(activity_ids)) triomics_provenance_abort("activity_id values must be unique")
  entity_index <- stats::setNames(seq_along(entities), entity_ids)
  activity_index <- stats::setNames(seq_along(activities), activity_ids)
  identity_key <- paste(profile$producer_stage, profile$study_id, profile$run_id,
                        profile$handoff_artifact_id, sep = "\034")
  if (identical(frame_payload_id, frame_envelope_id)) {
    triomics_provenance_abort("current provenance payload and envelope identifiers must differ")
  }

  for (entity in entities) {
    role <- entity$scientific_role
    if (!role %in% role_names) triomics_provenance_abort(paste0("unknown scientific_role: ", role))
    contract <- roles[[role]]
    if (!identical(entity$entity_type, contract$type)) {
      triomics_provenance_abort(paste0("entity_type disagrees with scientific_role: ", role))
    }
    if (!identical(entity$study_id, profile$study_id)) triomics_provenance_abort("entity study_id differs from profile")
    if (identical(entity$digest_algorithm, "sha256") != TRUE ||
        !grepl("^[0-9a-f]{64}$", entity$digest)) {
      triomics_provenance_abort(paste0("malformed SHA-256 digest for entity ", entity$entity_id))
    }
    scope_fields <- c("analysis_id", "block_id", "method_id", "preparation_context_id",
                      "representation_id", "route_id")
    required_scope <- contract$scope
    for (field in scope_fields) {
      value <- entity[[field]]
      if (field %in% required_scope) {
        if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
          triomics_provenance_abort(paste0("required scope is missing: ", role, ".", field))
        }
      } else if (!is.null(value)) {
        triomics_provenance_abort(paste0("inapplicable scope must be null: ", role, ".", field))
      }
    }
    for (field in c("row_identity_sha256", "column_identity_sha256")) {
      value <- entity[[field]]
      needed <- if (identical(field, "row_identity_sha256")) contract$row else contract$column
      if (needed) {
        if (!is.character(value) || length(value) != 1L || is.na(value) || !grepl("^[0-9a-f]{64}$", value)) {
          triomics_provenance_abort(paste0("required ordered identity digest is missing: ", role, ".", field))
        }
      } else if (!is.null(value)) {
        triomics_provenance_abort(paste0("inapplicable ordered identity digest must be null: ", role, ".", field))
      }
    }
    triomics_provenance_assert_sorted_unique(entity$derived_from_entity_ids,
                                             paste0(role, ".derived_from_entity_ids"))
    if (!is.null(entity$member_of_entity_id) &&
        !role %in% c("predecessor_payload", "integration_state")) {
      triomics_provenance_abort(paste0("member_of_entity_id is not permitted for role ", role))
    }
    if (!is.null(entity$generated_by_activity_id) && !entity$generated_by_activity_id %in% activity_ids) {
      triomics_provenance_abort(paste0("generated_by_activity_id does not resolve for ", entity$entity_id))
    }
  }

  b_only <- c("block_results", "family_ledger")
  c_only <- c("integration_result", "integration_state", "evaluation_evidence")
  present_roles <- vapply(entities, function(entity) entity$scientific_role, character(1))
  if (profile$producer_stage == "B" && any(present_roles %in% c_only)) {
    triomics_provenance_abort("C-only role appears in a B profile")
  }
  if (profile$producer_stage == "C" && any(present_roles %in% b_only)) {
    triomics_provenance_abort("B-only role appears in a C profile")
  }

  for (activity in activities) {
    triomics_provenance_assert_sorted_unique(activity$used_entity_ids,
                                             paste0(activity$activity_id, ".used_entity_ids"))
    triomics_provenance_assert_sorted_unique(activity$generated_entity_ids,
                                             paste0(activity$activity_id, ".generated_entity_ids"))
    if (!activity$stage %in% c("B", "C")) triomics_provenance_abort("activity stage must be B or C")
    for (entity_id in c(unlist(activity$used_entity_ids, use.names = FALSE),
                        unlist(activity$generated_entity_ids, use.names = FALSE))) {
      if (!entity_id %in% entity_ids) triomics_provenance_abort(paste0("activity entity reference does not resolve: ", entity_id))
      if (entity_id %in% c(frame_payload_id, frame_envelope_id)) {
        triomics_provenance_abort("current provenance frame is self-inventoried by an activity")
      }
    }
  }
  for (entity in entities) {
    for (reference in c(unlist(entity$derived_from_entity_ids, use.names = FALSE),
                        entity$member_of_entity_id)) {
      if (!is.null(reference) && !reference %in% entity_ids) {
        triomics_provenance_abort(paste0("entity graph reference does not resolve: ", reference))
      }
    }
  }
  if (any(entity_ids %in% c(frame_payload_id, frame_envelope_id)) ||
      profile$handoff_artifact_id %in% c(frame_payload_id, frame_envelope_id)) {
    triomics_provenance_abort("current provenance payload or envelope is self-inventoried")
  }
  for (entity in entities) {
    if (!is.null(entity$artifact_id) &&
        entity$artifact_id %in% c(frame_payload_id, frame_envelope_id)) {
      triomics_provenance_abort("current provenance frame is named by an artifact descriptor")
    }
  }

  derived_edges <- stats::setNames(vector("list", length(entity_ids)), entity_ids)
  member_edges <- stats::setNames(vector("list", length(entity_ids)), entity_ids)
  for (entity in entities) {
    derived_edges[[entity$entity_id]] <- unlist(entity$derived_from_entity_ids, use.names = FALSE)
    member_edges[[entity$entity_id]] <- if (is.null(entity$member_of_entity_id)) character() else entity$member_of_entity_id
  }
  triomics_provenance_assert_acyclic(entity_ids, derived_edges, "derived_from")
  triomics_provenance_assert_acyclic(entity_ids, member_edges, "member_of")

  for (activity in activities) {
    for (entity_id in unlist(activity$generated_entity_ids, use.names = FALSE)) {
      entity <- entities[[entity_index[[entity_id]]]]
      if (!identical(entity$generated_by_activity_id, activity$activity_id)) {
        triomics_provenance_abort(paste0("generated entity does not name its producing activity: ", entity_id))
      }
    }
    for (entity_id in unlist(activity$used_entity_ids, use.names = FALSE)) {
      if (!entity_id %in% entity_ids) triomics_provenance_abort(paste0("used entity does not resolve: ", entity_id))
    }
  }
  for (entity in entities) {
    generated_by <- entity$generated_by_activity_id
    if (is.null(generated_by)) {
      used_count <- sum(vapply(activities, function(activity) entity$entity_id %in% unlist(activity$used_entity_ids, use.names = FALSE), logical(1)))
      if (used_count < 1L) triomics_provenance_abort(paste0("external entity is not used by an activity: ", entity$entity_id))
    } else {
      count <- sum(vapply(activities, function(activity) entity$entity_id %in% unlist(activity$generated_entity_ids, use.names = FALSE), logical(1)))
      if (count != 1L) triomics_provenance_abort(paste0("generated entity must have exactly one generating activity: ", entity$entity_id))
    }
  }
  if (!profile$producer_activity_id %in% activity_ids) triomics_provenance_abort("producer_activity_id does not resolve")
  handoffs <- Filter(function(entity) identical(entity$scientific_role, "handoff_payload"), entities)
  if (length(handoffs) != 1L) triomics_provenance_abort("exactly one handoff_payload is required per profile")
  if (!identical(handoffs[[1L]]$artifact_id, profile$handoff_artifact_id)) {
    triomics_provenance_abort("handoff_payload artifact_id differs from handoff_artifact_id")
  }
  producer_activity <- activities[[activity_index[[profile$producer_activity_id]]]]
  if (!handoffs[[1L]]$entity_id %in% unlist(producer_activity$generated_entity_ids, use.names = FALSE)) {
    triomics_provenance_abort("producer_activity_id does not generate the handoff payload")
  }

  required_by_role <- c("source_manifest", "normalized_metadata", "feature_registry",
                        "analysis_unit_map", "resolved_analysis", "represented_matrix",
                        "preprocessing_state", "method_state", "predecessor_envelope",
                        "predecessor_payload", "candidate_nominations", "handoff_payload")
  if (profile$producer_stage == "B") required_by_role <- c(required_by_role, "block_results", "family_ledger")
  if (profile$producer_stage == "C") required_by_role <- c(required_by_role, "integration_result", "integration_state", "evaluation_evidence")
  for (role in required_by_role) {
    if (!any(present_roles == role)) triomics_provenance_abort(paste0("required role is missing: ", role))
  }
  if (sum(present_roles == "source_manifest") < 1L) triomics_provenance_abort("one or more source_manifest entities are required")
  cardinalities <- list(
    normalized_metadata = "analysis_id",
    feature_registry = "block_id",
    analysis_unit_map = c("analysis_id", "preparation_context_id", "representation_id"),
    resolved_analysis = c("analysis_id", "preparation_context_id", "representation_id"),
    represented_matrix = c("analysis_id", "block_id", "preparation_context_id", "representation_id"),
    method_state = c("analysis_id", "method_id", "preparation_context_id", "representation_id"),
    integration_result = c("analysis_id", "method_id", "preparation_context_id", "representation_id"),
    clinical_evidence = c("analysis_id", "preparation_context_id", "representation_id", "route_id"),
    native_sidecar = c("analysis_id", "method_id", "preparation_context_id", "representation_id", "route_id")
  )
  for (role in names(cardinalities)) {
    if (role %in% required_by_role) {
      triomics_provenance_exactly_one_per(entities, role, cardinalities[[role]])
    }
  }
  method_state_groups <- triomics_provenance_exactly_one_per(
    entities, "method_state", cardinalities$method_state
  )
  analysis_unit_maps <- Filter(function(entity) identical(entity$scientific_role, "analysis_unit_map"), entities)
  declared_method_ids <- unlist(profile$producer$method_ids, use.names = FALSE)
  expected_method_state_groups <- unlist(lapply(analysis_unit_maps, function(unit) {
    vapply(declared_method_ids, function(method_id) {
      triomics_provenance_group_key(list(
        analysis_id = unit$analysis_id,
        method_id = method_id,
        preparation_context_id = unit$preparation_context_id,
        representation_id = unit$representation_id
      ), cardinalities$method_state)
    }, character(1))
  }), use.names = FALSE)
  if (length(method_state_groups) != length(expected_method_state_groups) ||
      !setequal(method_state_groups, expected_method_state_groups)) {
    triomics_provenance_abort("method_state groups must cover every analysis context and producer method")
  }
  normalized_metadata <- Filter(function(entity) identical(entity$scientific_role, "normalized_metadata"), entities)
  metadata_analysis_ids <- vapply(normalized_metadata, `[[`, character(1), "analysis_id")
  other_analysis_entities <- Filter(function(entity) {
    !identical(entity$scientific_role, "normalized_metadata") && !is.null(entity$analysis_id)
  }, entities)
  profile_analysis_ids <- unique(vapply(other_analysis_entities, `[[`, character(1), "analysis_id"))
  if (!setequal(metadata_analysis_ids, profile_analysis_ids)) {
    triomics_provenance_abort("normalized_metadata must cover every profile analysis scope exactly")
  }
  preprocessing <- Filter(function(entity) identical(entity$scientific_role, "preprocessing_state"), entities)
  preprocessing_keys <- vapply(preprocessing, triomics_provenance_group_key, character(1),
                               fields = cardinalities$represented_matrix)
  if (length(preprocessing_keys) == 0L) triomics_provenance_abort("one or more preprocessing_state entities are required")
  represented <- Filter(function(entity) identical(entity$scientific_role, "represented_matrix"), entities)
  represented_keys <- vapply(represented, triomics_provenance_group_key, character(1),
                             fields = cardinalities$represented_matrix)
  feature_registry <- Filter(function(entity) identical(entity$scientific_role, "feature_registry"), entities)
  feature_registry_blocks <- vapply(feature_registry, `[[`, character(1), "block_id")
  represented_blocks <- unique(vapply(represented, `[[`, character(1), "block_id"))
  if (!setequal(feature_registry_blocks, represented_blocks)) {
    triomics_provenance_abort("feature_registry block coverage must match represented_matrix block coverage")
  }
  if (any(!represented_keys %in% preprocessing_keys)) {
    triomics_provenance_abort("every represented_matrix group requires preprocessing_state")
  }
  unit_groups <- triomics_provenance_exactly_one_per(
    entities, "analysis_unit_map", cardinalities$analysis_unit_map
  )
  resolved_groups <- triomics_provenance_exactly_one_per(
    entities, "resolved_analysis", cardinalities$resolved_analysis
  )
  if (!setequal(unit_groups, resolved_groups)) {
    triomics_provenance_abort("analysis_unit_map and resolved_analysis groups must match")
  }
  analysis_context_fields <- c("analysis_id", "preparation_context_id", "representation_id")
  method_state_entities <- Filter(function(entity) identical(entity$scientific_role, "method_state"), entities)
  method_state_contexts <- unique(vapply(
    method_state_entities, triomics_provenance_group_key, character(1),
    fields = analysis_context_fields
  ))
  for (role in c("represented_matrix", "preprocessing_state")) {
    role_entities <- if (identical(role, "represented_matrix")) represented else preprocessing
    contexts <- unique(vapply(
      role_entities, triomics_provenance_group_key, character(1),
      fields = analysis_context_fields
    ))
    if (any(!contexts %in% unit_groups) ||
        any(!contexts %in% resolved_groups) ||
        any(!contexts %in% method_state_contexts)) {
      triomics_provenance_abort(paste0(
        role, " contexts must match analysis_unit_map, resolved_analysis, and method_state"
      ))
    }
  }
  profile_singletons <- c("handoff_payload", "candidate_nominations")
  if (profile$producer_stage == "B") profile_singletons <- c(profile_singletons, "block_results", "family_ledger")
  if (profile$producer_stage == "C") profile_singletons <- c(profile_singletons, "evaluation_evidence")
  for (role in profile_singletons) triomics_provenance_exactly_one_per(entities, role, character())

  predecessor_envelopes <- Filter(function(entity) identical(entity$scientific_role, "predecessor_envelope"), entities)
  predecessor_payloads <- Filter(function(entity) identical(entity$scientific_role, "predecessor_payload"), entities)
  if (length(predecessor_envelopes) == 0L || length(predecessor_envelopes) != length(predecessor_payloads)) {
    triomics_provenance_abort("one or more one-to-one predecessor envelope/payload pairs are required")
  }
  for (envelope in predecessor_envelopes) {
    members <- Filter(function(payload) identical(payload$member_of_entity_id, envelope$entity_id), predecessor_payloads)
    if (length(members) != 1L) triomics_provenance_abort("each predecessor envelope must have exactly one payload")
    payload <- members[[1L]]
    if (!identical(payload$digest, envelope$artifact_payload_sha256) ||
        !identical(vapply(c("study_id", "analysis_id", "preparation_context_id", "representation_id"),
                          function(field) as.character(payload[[field]]), character(1)),
                   vapply(c("study_id", "analysis_id", "preparation_context_id", "representation_id"),
                          function(field) as.character(envelope[[field]]), character(1)))) {
      triomics_provenance_abort("predecessor envelope/payload digest or scope mismatch")
    }
  }

  if (profile$producer_stage == "C") {
    integration_result_groups <- triomics_provenance_exactly_one_per(
      entities, "integration_result", cardinalities$integration_result
    )
    if (!setequal(method_state_groups, integration_result_groups)) {
      triomics_provenance_abort("C method_state groups must match integration_result groups")
    }
    integration_results <- Filter(function(entity) identical(entity$scientific_role, "integration_result"), entities)
    integration_states <- Filter(function(entity) identical(entity$scientific_role, "integration_state"), entities)
    if (length(integration_results) == 0L || length(integration_results) != length(integration_states)) {
      triomics_provenance_abort("C profiles require one integration_state for every integration_result")
    }
    for (result in integration_results) {
      states <- Filter(function(state) identical(state$member_of_entity_id, result$entity_id), integration_states)
      if (length(states) != 1L) triomics_provenance_abort("each integration_result must have exactly one state")
      state <- states[[1L]]
      scope_fields <- c("study_id", "analysis_id", "method_id", "preparation_context_id", "representation_id")
      if (!identical(vapply(scope_fields, function(field) as.character(state[[field]]), character(1)),
                     vapply(scope_fields, function(field) as.character(result[[field]]), character(1)))) {
        triomics_provenance_abort("integration result/state scope mismatch")
      }
    }
  }

  route_keys <- character()
  route_ids <- character()
  for (route in routes) {
    key <- paste(route$route_kind, route$route_id, sep = "\034")
    route_keys <- c(route_keys, key)
    route_ids <- c(route_ids, route$route_id)
    role <- if (identical(route$route_kind, "clinical")) "clinical_evidence" else "native_sidecar"
    matching <- Filter(function(entity) identical(entity$scientific_role, role) &&
                         identical(entity$route_id, route$route_id), entities)
    if (length(matching) != 1L) triomics_provenance_abort("every declared route requires exactly one matching evidence descriptor")
    descriptor <- matching[[1L]]
    fields <- c("analysis_id", "preparation_context_id", "representation_id", "method_id")
    if (!identical(lapply(fields, function(field) descriptor[[field]]),
                   lapply(fields, function(field) route[[field]]))) {
      triomics_provenance_abort("route descriptor scope or method_id differs from its declaration")
    }
  }
  if (length(route_ids) > 1L && anyDuplicated(route_ids)) triomics_provenance_abort("route_id values must be unique within the profile")
  if (length(route_keys) > 1L && (anyDuplicated(route_keys) ||
      !identical(route_keys, sort(route_keys, method = "radix")))) {
    triomics_provenance_abort("declared_routes must be sorted uniquely by route_kind and route_id")
  }
  route_descriptors <- Filter(function(entity) entity$scientific_role %in% c("clinical_evidence", "native_sidecar"), entities)
  for (descriptor in route_descriptors) {
    route_kind <- if (identical(descriptor$scientific_role, "clinical_evidence")) "clinical" else "native"
    matching <- Filter(function(route) identical(route$route_kind, route_kind) &&
                         identical(route$route_id, descriptor$route_id), routes)
    if (length(matching) != 1L) {
      triomics_provenance_abort("route evidence descriptor must match exactly one declaration by kind and route_id")
    }
    route <- matching[[1L]]
    fields <- c("analysis_id", "preparation_context_id", "representation_id", "method_id")
    if (!identical(lapply(fields, function(field) descriptor[[field]]),
                   lapply(fields, function(field) route[[field]]))) {
      triomics_provenance_abort("route evidence descriptor scope or method_id differs from its declaration")
    }
  }

  if (!is.null(profile$migration)) {
    proofs <- profile$migration$proof_status_by_role
    if (!is.list(proofs) || is.null(names(proofs))) triomics_provenance_abort("migration proof_status_by_role must be an object")
    route_roles <- unique(vapply(routes, function(route) {
      if (identical(route$route_kind, "clinical")) "clinical_evidence" else "native_sidecar"
    }, character(1)))
    expected_proofs <- unique(c(required_by_role, route_roles))
    if (!setequal(names(proofs), expected_proofs) || any(vapply(proofs, function(value) !identical(value, "proven"), logical(1)))) {
      triomics_provenance_abort("migration must prove every required scientific role and no unknown role")
    }
  }
  invisible(TRUE)
}

triomics_validate_stage_handoff_provenance <- function(profile, provenance_payload_id,
                                                       provenance_envelope_id) {
  if (!is.character(provenance_payload_id) || length(provenance_payload_id) != 1L ||
      is.na(provenance_payload_id) || !nzchar(provenance_payload_id) ||
      !is.character(provenance_envelope_id) || length(provenance_envelope_id) != 1L ||
      is.na(provenance_envelope_id) || !nzchar(provenance_envelope_id)) {
    triomics_provenance_abort("current provenance payload and envelope identifiers are required")
  }
  json <- triomics_provenance_profile_json(profile)
  number_tokens <- triomics_provenance_json_scan(json)
  if (is.character(profile) && length(profile) == 1L) {
    identity_tokens <- Filter(function(entry) length(entry$path) > 0L && identical(entry$path[[1L]], "identity"), number_tokens)
    triomics_provenance_validate_identity_number_tokens(identity_tokens)
    profile <- tryCatch(jsonlite::fromJSON(profile, simplifyVector = FALSE), error = function(error) error)
    if (inherits(profile, "error")) triomics_provenance_abort("profile JSON could not be parsed")
  }
  if (!is.list(profile) || is.null(names(profile)) || !is.list(profile$identity)) {
    triomics_provenance_abort("profile and identity must be JSON objects")
  }
  triomics_provenance_validate_identity_numbers(profile$identity)
  triomics_provenance_validate_schema(json)
  expected_digest <- triomics_provenance_identity_sha256(profile$identity)
  if (!identical(profile$identity_sha256, expected_digest)) {
    triomics_provenance_abort("identity_sha256 does not match RFC 8785 canonical identity bytes")
  }
  triomics_provenance_validate_profile(profile, provenance_payload_id, provenance_envelope_id)
  invisible(TRUE)
}
