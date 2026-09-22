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

triomics_validate_envelope <- function(path) {
  if (!file.exists(path)) return(FALSE)
  envelope <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
  required <- c("schema_version", "artifact_type", "artifact_id", "study_id", "run_id",
                "status", "created_utc", "config_sha256", "inputs", "payload_file",
                "payload_sha256", "producer", "reason_codes", "privacy_class")
  if (!is.list(envelope) || !all(required %in% names(envelope))) return(FALSE)
  if (!identical(envelope$schema_version, "2.0") ||
      !grepl("^[0-9a-f]{64}$", envelope$config_sha256) ||
      !grepl("^[0-9a-f]{64}$", envelope$payload_sha256)) return(FALSE)
  payload <- file.path(dirname(path), envelope$payload_file)
  if (!file.exists(payload)) return(FALSE)
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
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  if (file.exists(path) || !file.rename(temporary, path)) {
    triomics_abort("A_ARTIFACT_FINALIZE", "The output file could not be atomically finalized.",
                   module = "contracts", remediation = "Use a new output path on a local filesystem.")
  }
  invisible(path)
}
