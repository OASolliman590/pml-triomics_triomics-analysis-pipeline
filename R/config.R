triomics_config_schema <- function() {
  installed <- system.file("schema", "config.schema.json", package = "triomics")
  source_root <- getOption("triomics.source_root")
  if (is.character(source_root) && length(source_root) == 1L) {
    source_schema <- file.path(source_root, "inst", "schema", "config.schema.json")
    if (file.exists(source_schema)) return(source_schema)
  }
  source_tree <- file.path(getwd(), "inst", "schema", "config.schema.json")
  if (nzchar(installed) && file.exists(installed)) return(installed)
  if (file.exists(source_tree)) return(source_tree)
  triomics_abort("A_CONFIG_SCHEMA_MISSING", "The local configuration schema is unavailable.",
                 module = "A", remediation = "Restore inst/schema/config.schema.json from the package source.")
}

triomics_resolve_schema_ref <- function(schema, ref) {
  if (is.null(ref) || !startsWith(ref, "#/")) return(NULL)
  parts <- strsplit(sub("^#/", "", ref), "/", fixed = TRUE)[[1L]]
  Reduce(function(node, key) {
    key <- gsub("~1", "/", gsub("~0", "~", key, fixed = TRUE), fixed = TRUE)
    node[[key]]
  }, parts, init = schema)
}

triomics_check_config_keys <- function(value, node, schema, path = "") {
  if (!is.null(node$`$ref`)) {
    node <- triomics_resolve_schema_ref(schema, node$`$ref`)
  }
  if (is.null(node)) return(invisible(NULL))
  if (is.list(value) && !is.null(names(value)) && length(names(value)) > 0L) {
    properties <- node$properties
    if (!is.null(properties)) {
      unknown <- setdiff(names(value), names(properties))
      if (identical(node$additionalProperties, FALSE) && length(unknown) > 0L) {
        key <- if (nzchar(path)) paste(path, unknown[[1L]], sep = ".") else unknown[[1L]]
        triomics_abort("A_CONFIG_UNKNOWN_KEY", paste0("Unknown configuration key '", key, "'."),
                       key = key, remediation = "Remove the key or use a field defined by the local schema.")
      }
      for (name in intersect(names(value), names(properties))) {
        child_path <- if (nzchar(path)) paste(path, name, sep = ".") else name
        triomics_check_config_keys(value[[name]], properties[[name]], schema, child_path)
      }
    } else if (is.list(node$additionalProperties)) {
      for (name in names(value)) {
        triomics_check_config_keys(value[[name]], node$additionalProperties, schema,
                                   if (nzchar(path)) paste(path, name, sep = ".") else name)
      }
    }
  } else if (is.list(value) && !is.null(node$items)) {
    for (i in seq_along(value)) {
      triomics_check_config_keys(value[[i]], node$items, schema,
                                 paste0(path, "[", i, "]"))
    }
  }
  invisible(NULL)
}

triomics_config_json_value <- function(value, node, schema) {
  if (!is.null(node$`$ref`)) node <- triomics_resolve_schema_ref(schema, node$`$ref`)
  if (is.null(node)) return(value)
  if (identical(node$type, "array")) {
    elements <- if (is.list(value)) value else as.list(value)
    if (!is.null(node$items)) {
      elements <- lapply(elements, triomics_config_json_value,
                         node = node$items, schema = schema)
    }
    return(I(elements))
  }
  if (identical(node$type, "object") && is.list(value) && !is.null(names(value))) {
    for (name in names(value)) {
      child <- node$properties[[name]]
      if (is.null(child) && is.list(node$additionalProperties)) child <- node$additionalProperties
      if (!is.null(child)) value[name] <- list(triomics_config_json_value(value[[name]], child, schema))
    }
  }
  value
}

triomics_validation_failure <- function(result) {
  errors <- if (is.list(result)) result$errors else attr(result, "errors")
  if (is.data.frame(errors) && nrow(errors) > 0L) {
    candidates <- if ("instancePath" %in% names(errors)) which(nzchar(errors$instancePath)) else integer()
    if (length(candidates) > 0L && "keyword" %in% names(errors)) {
      preferred <- candidates[errors$keyword[candidates] %in% c("type", "const", "enum", "pattern")]
      if (length(preferred) > 0L) candidates <- preferred
    }
    if (length(candidates) > 0L) {
      index <- candidates[[1L]]
      key <- gsub("/", ".", sub("^/", "", errors$instancePath[[index]]), fixed = TRUE)
      return(list(key = key, keyword = errors$keyword[[index]], message = errors$message[[index]]))
    }
    if ("params" %in% names(errors) && "missingProperty" %in% names(errors$params)) {
      key <- errors$params$missingProperty[[1L]]
      return(list(key = key, keyword = "required", message = errors$message[[1L]]))
    }
    return(list(key = "config", keyword = "schema", message = errors$message[[1L]]))
  }
  list(key = "config", keyword = "schema", message = paste(unlist(errors, use.names = FALSE), collapse = "; "))
}

triomics_read_config <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    triomics_abort("A_CONFIG_MISSING", "Configuration file does not exist.", key = "config",
                   remediation = "Provide a readable configuration file path.")
  }
  handlers <- list(map = function(values) {
    keys <- names(values)
    duplicate <- unique(keys[duplicated(keys)])
    if (length(duplicate) > 0L) {
      triomics_abort("A_CONFIG_DUPLICATE_KEY",
                     paste0("Duplicate configuration key '", duplicate[[1L]], "'."),
                     key = duplicate[[1L]],
                     remediation = "Remove the repeated YAML mapping before validation.")
    }
    values
  })
  parsed <- tryCatch(yaml::yaml.load_file(path, handlers = handlers, eval.expr = FALSE),
                     error = function(e) e)
  if (inherits(parsed, "error")) {
    if (inherits(parsed, "triomics_error")) stop(parsed)
    if (grepl("Duplicate map key:", conditionMessage(parsed), fixed = TRUE)) {
      match <- regmatches(conditionMessage(parsed), regexec("Duplicate map key: ['\\\"]([^'\\\"]+)['\\\"]", conditionMessage(parsed)))[[1L]]
      key <- if (length(match) >= 2L) match[[2L]] else "<unknown>"
      triomics_abort("A_CONFIG_DUPLICATE_KEY", paste0("Duplicate configuration key '", key, "'."),
                     key = key, remediation = "Remove the repeated YAML mapping before validation.")
    }
    triomics_abort("A_CONFIG_PARSE", paste0("Configuration YAML is malformed: ", conditionMessage(parsed)),
                   key = "config", remediation = "Correct the YAML syntax and retry.")
  }
  if (!is.list(parsed) || is.null(names(parsed))) {
    triomics_abort("A_CONFIG_TYPE", "Configuration must be a YAML mapping.", key = "<root>",
                   remediation = "Provide a YAML object matching the local configuration schema.")
  }
  schema_path <- triomics_config_schema()
  schema <- jsonlite::read_json(schema_path, simplifyVector = FALSE)
  triomics_check_config_keys(parsed, schema, schema)
  json_value <- triomics_config_json_value(parsed, schema, schema)
  json <- jsonlite::toJSON(json_value, auto_unbox = TRUE, null = "null", digits = NA)
  validation <- tryCatch(
    jsonvalidate::json_validate(json, schema_path, engine = "ajv", verbose = TRUE),
    error = function(e) e
  )
  if (inherits(validation, "error")) {
    triomics_abort("A_CONFIG_SCHEMA", paste0("Configuration schema validation failed: ", conditionMessage(validation)),
                   key = "config", remediation = "Correct the configuration to match schema version 2.0.")
  }
  valid <- if (is.list(validation) && !is.null(validation$valid)) isTRUE(validation$valid) else isTRUE(validation)
  if (!valid) {
    failure <- triomics_validation_failure(validation)
    key <- failure$key
    if (!nzchar(key)) key <- "config"
    code <- if (failure$keyword %in% c("type", "const", "enum")) "A_CONFIG_TYPE" else "A_CONFIG_SCHEMA"
    triomics_abort(code, paste0("Invalid configuration at '", key, "': ", failure$message), key = key,
                   remediation = "Correct the named field to match the local configuration schema.")
  }
  attr(parsed, "config_sha256") <- triomics_sha256(path)
  attr(parsed, "config_path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  parsed
}

triomics_validate_config_paths <- function(config, config_path) {
  root <- dirname(normalizePath(config_path, winslash = "/", mustWork = TRUE))
  paths <- character()
  if (length(config$sources) > 0L) {
    paths <- c(paths, vapply(config$sources, function(source) source$file, character(1L)))
  }
  if (!is.null(config$metadata$file)) paths <- c(paths, config$metadata$file)
  resolved <- vapply(paths, function(path) {
    candidate <- if (grepl("^(?:[A-Za-z]:[/\\\\]|/)", path)) path else file.path(root, path)
    normalizePath(candidate, winslash = "/", mustWork = FALSE)
  }, character(1L))
  missing <- resolved[!file.exists(resolved)]
  if (length(missing) > 0L) {
    triomics_abort("A_SOURCE_MISSING", paste0("Declared input file does not exist: ", basename(missing[[1L]]), "."),
                   key = "sources.file", remediation = "Check source paths relative to the configuration file.")
  }
  invisible(resolved)
}
