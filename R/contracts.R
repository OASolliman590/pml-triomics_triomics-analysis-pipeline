triomics_abort <- function(code, message, key = NULL, module = "A", stage = NULL,
                           remediation = "Review the named configuration field.") {
  condition <- structure(
    list(
      message = as.character(message),
      call = NULL,
      code = as.character(code),
      module = as.character(module),
      stage = if (is.null(stage)) NULL else as.character(stage),
      config_key = if (is.null(key)) NULL else as.character(key),
      remediation = as.character(remediation)
    ),
    class = c("triomics_error", "error", "condition")
  )
  stop(condition)
}

triomics_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

triomics_artifact_envelope <- function(artifact_type, artifact_id, study_id, run_id,
                                       status, config_sha256, payload,
                                       producer_commit, inputs = list(),
                                       reason_codes = character(),
                                       privacy_class = "synthetic") {
  if (!file.exists(payload)) {
    triomics_abort("A_ARTIFACT_PAYLOAD_MISSING", "Artifact payload does not exist.",
                   module = "contracts", remediation = "Create the payload before finalizing its manifest.")
  }
  list(
    schema_version = "2.0",
    artifact_type = artifact_type,
    artifact_id = artifact_id,
    study_id = study_id,
    run_id = run_id,
    status = status,
    created_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ"),
    config_sha256 = config_sha256,
    inputs = inputs,
    payload_file = basename(payload),
    payload_sha256 = triomics_sha256(payload),
    producer = list(
      package = "triomics",
      version = "0.1.0",
      commit = producer_commit
    ),
    reason_codes = reason_codes,
    privacy_class = privacy_class
  )
}

triomics_sha256_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && grepl("^[0-9a-f]{64}$", value)
}

triomics_nonempty_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

triomics_envelope_statuses <- function() {
  c("not_requested", "not_started", "running", "complete", "partial", "unavailable", "failed")
}

triomics_is_symlink <- function(path) {
  link <- Sys.readlink(path)
  is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)
}

triomics_resolve_payload <- function(manifest_path, payload_file) {
  if (!triomics_nonempty_string(payload_file)) return(NULL)
  if (grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\)", payload_file)) return(NULL)
  parts <- strsplit(payload_file, "[/\\\\]+")[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L || any(parts == "..")) return(NULL)
  root <- normalizePath(dirname(manifest_path), winslash = "/", mustWork = TRUE)
  current <- root
  for (part in parts) {
    if (identical(part, ".")) next
    current <- file.path(current, part)
    if (triomics_is_symlink(current)) return(NULL)
    if (!file.exists(current)) return(NULL)
  }
  resolved <- normalizePath(current, winslash = "/", mustWork = TRUE)
  root_prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  if (!(identical(resolved, root) || startsWith(resolved, root_prefix))) return(NULL)
  if (dir.exists(resolved)) return(NULL)
  resolved
}

triomics_validate_envelope <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) return(FALSE)
  envelope <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(envelope) || is.null(names(envelope))) return(FALSE)
  allowed <- c(
    "schema_version", "artifact_type", "artifact_id", "study_id", "run_id",
    "analysis_id", "preparation_context_id", "representation_id",
    "status", "created_utc", "config_sha256", "inputs", "payload_file",
    "payload_sha256", "producer", "reason_codes", "privacy_class"
  )
  required <- c(
    "schema_version", "artifact_type", "artifact_id", "study_id", "run_id",
    "status", "created_utc", "config_sha256", "inputs", "payload_file",
    "payload_sha256", "producer", "reason_codes", "privacy_class"
  )
  if (!all(names(envelope) %in% allowed) || !all(required %in% names(envelope))) return(FALSE)
  if (!identical(envelope$schema_version, "2.0")) return(FALSE)
  if (!triomics_nonempty_string(envelope$status) || !envelope$status %in% triomics_envelope_statuses()) {
    return(FALSE)
  }
  if (!triomics_sha256_text(envelope$config_sha256) || !triomics_sha256_text(envelope$payload_sha256)) {
    return(FALSE)
  }
  producer <- envelope$producer
  producer_fields <- c("package", "version", "commit")
  if (!is.list(producer) || is.null(names(producer)) ||
      !all(producer_fields %in% names(producer)) || !all(names(producer) %in% producer_fields)) {
    return(FALSE)
  }
  if (!all(vapply(producer_fields, function(field) triomics_nonempty_string(producer[[field]]), logical(1L)))) {
    return(FALSE)
  }
  inputs <- envelope$inputs
  if (!is.list(inputs) || (!is.null(names(inputs)) && any(nzchar(names(inputs))))) return(FALSE)
  for (input in inputs) {
    if (!is.list(input) || is.null(names(input))) return(FALSE)
    if (!all(c("artifact_id", "sha256") %in% names(input)) ||
        !all(names(input) %in% c("artifact_id", "sha256"))) return(FALSE)
    if (!triomics_nonempty_string(input$artifact_id) || !triomics_sha256_text(input$sha256)) return(FALSE)
  }
  payload <- triomics_resolve_payload(path, envelope$payload_file)
  if (is.null(payload)) return(FALSE)
  identical(triomics_sha256(payload), envelope$payload_sha256)
}

triomics_write_envelope <- function(envelope, path) {
  if (file.exists(path)) {
    triomics_abort("A_ARTIFACT_IMMUTABLE", "An artifact manifest already exists at the requested path.",
                   module = "contracts", remediation = "Use a new artifact path for each finalized artifact.")
  }
  payload <- file.path(dirname(path), envelope$payload_file)
  if (!file.exists(payload) || !identical(triomics_sha256(payload), envelope$payload_sha256)) {
    triomics_abort("A_ARTIFACT_HASH_MISMATCH", "Artifact payload is missing or its SHA256 does not match.",
                   module = "contracts", remediation = "Recreate the payload and its manifest together.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(envelope, temporary, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  if (!triomics_validate_envelope(temporary)) {
    triomics_abort("A_ARTIFACT_INVALID", "The finalized artifact envelope failed validation.",
                   module = "contracts", remediation = "Review the payload, hash, and envelope fields.")
  }
  if (!file.rename(temporary, path)) {
    triomics_abort("A_ARTIFACT_FINALIZE", "The artifact manifest could not be atomically finalized.",
                   module = "contracts", remediation = "Use an output directory on a local filesystem with rename support.")
  }
  invisible(path)
}

triomics_write_json_atomic <- function(value, path) {
  if (file.exists(path)) {
    triomics_abort("A_ARTIFACT_FINALIZE", "The output file could not be atomically finalized.",
                   module = "contracts",
                   remediation = "Use a new output path on a local filesystem.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  if (file.exists(temporary)) unlink(temporary)
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  if (file.exists(path) || !file.rename(temporary, path)) {
    triomics_abort("A_ARTIFACT_FINALIZE", "The output file could not be atomically finalized.",
                   module = "contracts",
                   remediation = "Use a new output path on a local filesystem.")
  }
  invisible(path)
}
