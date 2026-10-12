triomics_canonical_json <- function(value) {
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE))
}

triomics_hash_text <- function(text) {
  digest::digest(as.character(text), algo = "sha256", serialize = FALSE)
}

triomics_state_fields <- function(include_id = TRUE) {
  fields <- c(
    "mode", "profile_id", "block_id", "training_unit_ids", "training_unit_hash",
    "feature_keys", "reference_values", "reference_scale", "learned_parameters",
    "input_quantity", "output_quantity", "input_scale", "output_scale",
    "mask_lineage", "mask_hash", "operation_order", "operation_hash",
    "rng_seed", "rng_kind", "package_versions", "fit_input_hashes"
  )
  if (include_id) c(fields, "state_hash", "state_id") else fields
}

triomics_state_complete <- function(payload, include_id = TRUE) {
  if (!is.list(payload) || is.null(names(payload))) return(FALSE)
  fields <- triomics_state_fields(include_id)
  if (!all(fields %in% names(payload))) return(FALSE)
  if (!is.character(payload$mode) || !identical(payload$mode, "fit")) return(FALSE)
  if (!is.character(payload$training_unit_ids) || !length(payload$training_unit_ids)) return(FALSE)
  if (!is.character(payload$feature_keys) || !length(payload$feature_keys)) return(FALSE)
  if (!is.list(payload$mask_lineage) || !is.list(payload$operation_order) || !length(payload$operation_order)) return(FALSE)
  if (is.null(payload$learned_parameters) || !is.list(payload$learned_parameters)) return(FALSE)
  TRUE
}

triomics_state_path <- function(state_dir, path_name) {
  if (!is.character(state_dir) || length(state_dir) != 1L || is.na(state_dir) || !nzchar(state_dir)) {
    triomics_abort("A_STATE_PARTIAL", "State storage directory is missing.",
                   key = "state_dir", module = "state_store", stage = "A",
                   remediation = "Choose a local directory for the preparation state.")
  }
  if (!is.character(path_name) || length(path_name) != 1L || is.na(path_name) ||
      !nzchar(path_name) || grepl("\\.tmp$|[\\\\/]|\\.\\.", path_name) || !endsWith(path_name, ".json")) {
    triomics_abort("A_STATE_PARTIAL", "Interrupted or non-local state paths are not resume candidates.",
                   key = "state_ref", module = "state_store", stage = "A",
                   remediation = "Read only a finalized state file inside the state directory.")
  }
  file.path(state_dir, path_name)
}

triomics_check_state_hashes <- function(parsed, state_ref) {
  body <- parsed
  body$state_hash <- NULL
  body$state_id <- NULL
  hash <- triomics_hash_text(triomics_canonical_json(body))
  if (!identical(hash, parsed$state_hash) || !identical(hash, state_ref$state_hash) ||
      !identical(parsed$state_id, state_ref$state_id)) {
    triomics_abort("A_STATE_STALE", "Preparation state hash does not match its contents.",
                   key = "state_hash", module = "state_store", stage = "A",
                   remediation = "Discard the state and fit again. Do not repair a hash in place.")
  }
  if (!identical(parsed$training_unit_hash, triomics_hash_text(triomics_canonical_json(parsed$training_unit_ids))) ||
      !identical(parsed$mask_hash, triomics_hash_text(triomics_canonical_json(parsed$mask_lineage))) ||
      !identical(parsed$operation_hash, triomics_hash_text(triomics_canonical_json(parsed$operation_order)))) {
    triomics_abort("A_STATE_STALE", "Training ids, masks, or operation order no longer match their hashes.",
                   key = "state_hash", module = "state_store", stage = "A",
                   remediation = "Do not apply a state whose lineage fields were edited.")
  }
  invisible(hash)
}

triomics_read_state <- function(state_ref, state_dir) {
  if (!is.list(state_ref) || is.null(state_ref$path) || is.null(state_ref$state_hash) || is.null(state_ref$state_id)) {
    triomics_abort("A_STATE_PARTIAL", "A state reference is missing its id, hash, or path.",
                   key = "state_ref", module = "state_store", stage = "A",
                   remediation = "Pass the reference returned when the fit state was finalized.")
  }
  path <- triomics_state_path(state_dir, state_ref$path)
  if (!file.exists(path) || dir.exists(path)) {
    triomics_abort("A_STATE_PARTIAL", "The finalized state file is missing.",
                   key = "state_ref", module = "state_store", stage = "A",
                   remediation = "Do not resume from a temporary state file.")
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(paste(readLines(path, warn = FALSE), collapse = "\n"),
                       simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    error = function(error) NULL
  )
  if (is.null(parsed) || !triomics_state_complete(parsed, TRUE)) {
    triomics_abort("A_STATE_PARTIAL", "Preparation state is incomplete or unreadable.",
                   key = "state_ref", module = "state_store", stage = "A",
                   remediation = "Reject partial state instead of filling missing fields.")
  }
  if (!is.character(parsed$training_unit_ids)) parsed$training_unit_ids <- as.character(parsed$training_unit_ids)
  if (!is.character(parsed$feature_keys)) parsed$feature_keys <- as.character(parsed$feature_keys)
  triomics_check_state_hashes(parsed, state_ref)
  parsed
}

triomics_externalize_state <- function(payload, state_dir) {
  if (!triomics_state_complete(payload, FALSE)) {
    triomics_abort("A_STATE_PARTIAL", "Fit state is missing hashes, masks, training ids, or operation order.",
                   key = "state", module = "state_store", stage = "A",
                   remediation = "Finish the fit payload before writing it.")
  }
  if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  hash <- triomics_hash_text(triomics_canonical_json(payload))
  state_id <- paste0("st_", substr(hash, 1L, 20L))
  path_name <- paste0(state_id, ".json")
  final <- file.path(state_dir, path_name)
  ref <- list(state_id = state_id, state_hash = hash, path = path_name)
  if (file.exists(final)) {
    triomics_read_state(ref, state_dir)
    return(ref)
  }
  payload$state_hash <- hash
  payload$state_id <- state_id
  temporary <- paste0(final, ".tmp")
  if (file.exists(temporary)) unlink(temporary)
  jsonlite::write_json(payload, temporary, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
  verified <- tryCatch(
    jsonlite::fromJSON(paste(readLines(temporary, warn = FALSE), collapse = "\n"),
                       simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    error = function(error) NULL
  )
  body <- verified
  if (!is.null(body)) {
    body$state_hash <- NULL
    body$state_id <- NULL
  }
  if (is.null(verified) || !triomics_state_complete(verified, TRUE) ||
      !identical(triomics_hash_text(triomics_canonical_json(body)), hash)) {
    if (file.exists(temporary)) unlink(temporary)
    triomics_abort("A_STATE_PARTIAL", "State verification failed before the file was finalized.",
                   key = "state", module = "state_store", stage = "A",
                   remediation = "Do not leave a partial state as a resume candidate.")
  }
  if (file.exists(final) || !file.rename(temporary, final)) {
    if (file.exists(temporary)) unlink(temporary)
    triomics_abort("A_STATE_PARTIAL", "The state file could not be atomically finalized.",
                   key = "state", module = "state_store", stage = "A",
                   remediation = "Use a local directory that supports rename.")
  }
  ref
}
