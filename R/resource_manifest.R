triomics_d_resource_abort <- function(code, message, key = NULL) {
  triomics_d_abort(
    code, message, key = key,
    remediation = "Verify the exact local release, content hash, terms, permitted operation, and network policy."
  )
}

triomics_d_resource_cache_path <- function(cache_dir, cache_key) {
  cache_parts <- if (triomics_d_nonempty_string(cache_dir)) {
    strsplit(cache_dir, "[/\\\\]+")[[1L]]
  } else {
    character()
  }
  if (!triomics_d_nonempty_string(cache_dir) || any(cache_parts == "..")) {
    triomics_d_resource_abort("D_RESOURCE_CACHE_INVALID", "The local resource cache path is invalid.", "cache_dir")
  }
  if (dir.exists(cache_dir) && triomics_is_symlink(cache_dir)) {
    triomics_d_resource_abort("D_RESOURCE_CACHE_INVALID", "The local resource cache cannot be a symlink.", "cache_dir")
  }
  file.path(cache_dir, paste0(cache_key, ".resource"))
}

triomics_d_resource_manifest <- function(resource, operation, status, cache_status,
                                         cache_path, network_policy, reason_code = NULL) {
  retrieval_timestamp <- resource$retrieval_timestamp %||% if (!is.null(cache_path) && file.exists(cache_path)) {
    format(file.info(cache_path)$mtime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  } else {
    NULL
  }
  list(
    schema_version = "2.0",
    artifact_type = "resource_manifest",
    resource_id = as.character(resource$resource_id),
    canonical_source = as.character(resource$source_url),
    release = as.character(resource$version),
    content_sha256 = as.character(resource$sha256),
    retrieval_timestamp = retrieval_timestamp,
    terms_evidence = as.character(resource$terms_url),
    rights_status = as.character(resource$rights_status),
    permitted_operation = as.list(as.character(unlist(resource$permitted_operation, use.names = FALSE))),
    redistribution_status = as.character(resource$redistribution),
    operation = as.character(operation),
    local_cache = cache_path,
    cache_status = as.character(cache_status),
    network_policy = as.character(network_policy),
    network_attempted = FALSE,
    study_upload = FALSE,
    status = as.character(status),
    reason_code = reason_code
  )
}

triomics_d_resource_access <- function(resource, operation, expected_version,
                                       expected_sha256, cache_dir,
                                       network_policy = "offline") {
  if (!is.list(resource) || !triomics_d_nonempty_string(resource$resource_id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", as.character(resource$resource_id)) ||
      !triomics_d_nonempty_string(resource$source_url) ||
      !triomics_d_nonempty_string(resource$version) ||
      !triomics_sha256_text(as.character(resource$sha256)) ||
      !triomics_d_nonempty_string(resource$local_file) ||
      !triomics_d_nonempty_string(resource$terms_url) ||
      !triomics_d_nonempty_string(operation) ||
      !is.character(expected_version) || length(expected_version) != 1L || is.na(expected_version) ||
      !triomics_sha256_text(as.character(expected_sha256)) ||
      !network_policy %in% c("offline", "approved_reference_fetch_only")) {
    triomics_d_resource_abort("D_RESOURCE_REQUEST_INVALID", "Resource access needs exact release/hash, local source, terms, operation, and policy.",
                              "resource")
  }
  if (!identical(as.character(resource$version), expected_version) ||
      !identical(as.character(resource$sha256), as.character(expected_sha256))) {
    triomics_d_resource_abort("D_RESOURCE_RELEASE_MISMATCH", "The resource declaration does not match the requested exact release and content hash.",
                              as.character(resource$resource_id))
  }
  if (!identical(as.character(resource$rights_status), "verified") ||
      !operation %in% as.character(unlist(resource$permitted_operation, use.names = FALSE)) ||
      !as.character(resource$redistribution) %in% c("allowed", "prohibited", "unverified")) {
    triomics_d_resource_abort("D_RESOURCE_RIGHTS", "The resource has no verified right for this operation.",
                              as.character(resource$resource_id))
  }
  cache_key <- triomics_hash_text(triomics_canonical_json(list(
    resource_id = as.character(resource$resource_id), release = expected_version,
    sha256 = as.character(expected_sha256), operation = operation
  )))
  cache_path <- triomics_d_resource_cache_path(cache_dir, cache_key)
  if (file.exists(cache_path) || dir.exists(cache_path)) {
    if (dir.exists(cache_path) || triomics_is_symlink(cache_path) ||
        !identical(triomics_sha256(cache_path), as.character(expected_sha256))) {
      triomics_d_resource_abort("D_RESOURCE_HASH_MISMATCH", "A cached resource does not match its pinned SHA256.",
                                as.character(resource$resource_id))
    }
    manifest <- triomics_d_resource_manifest(resource, operation, "available", "hit",
                                             normalizePath(cache_path, winslash = "/", mustWork = TRUE), network_policy)
    manifest$path <- manifest$local_cache
    manifest$sha256 <- manifest$content_sha256
    return(manifest)
  }

  source_path <- as.character(resource$local_file)
  if (grepl("^(?:https?|ftp)://", source_path, ignore.case = TRUE)) {
    triomics_d_resource_abort("D_RESOURCE_PATH_INVALID", "D resource access accepts local resource paths only.",
                              as.character(resource$resource_id))
  }
  if (!file.exists(source_path) || dir.exists(source_path)) {
    reason_code <- if (identical(network_policy, "offline")) {
      "D_RESOURCE_MISSING"
    } else {
      "D_RESOURCE_FETCH_UNAVAILABLE"
    }
    manifest <- triomics_d_resource_manifest(resource, operation, "unavailable", "miss", NULL,
                                             network_policy, reason_code)
    manifest$path <- NULL
    manifest$sha256 <- manifest$content_sha256
    manifest$reason <- if (identical(network_policy, "offline")) {
      "The exact local resource is not present in cache or at its declared path."
    } else {
      "No authorized local copy is present; this service did not initiate a network request."
    }
    return(manifest)
  }
  if (triomics_is_symlink(source_path) ||
      !identical(triomics_sha256(source_path), as.character(expected_sha256))) {
    triomics_d_resource_abort("D_RESOURCE_HASH_MISMATCH", "The local resource bytes do not match the pinned SHA256.",
                              as.character(resource$resource_id))
  }
  cache_parent <- dirname(cache_path)
  if (dir.exists(cache_parent) && triomics_is_symlink(cache_parent)) {
    triomics_d_resource_abort("D_RESOURCE_CACHE_INVALID", "The resource cache directory cannot be a symlink.", "cache_dir")
  }
  if (!dir.exists(cache_parent)) dir.create(cache_parent, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(cache_parent) || triomics_is_symlink(cache_parent)) {
    triomics_d_resource_abort("D_RESOURCE_CACHE_INVALID", "The local resource cache directory could not be created safely.", "cache_dir")
  }
  temporary <- tempfile(pattern = ".d-resource-", tmpdir = cache_parent, fileext = ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  if (!file.copy(source_path, temporary, overwrite = FALSE) ||
      !identical(triomics_sha256(temporary), as.character(expected_sha256))) {
    triomics_d_resource_abort("D_RESOURCE_CACHE_WRITE", "The verified local resource could not be copied into cache.",
                              as.character(resource$resource_id))
  }
  if (file.exists(cache_path) || !file.rename(temporary, cache_path)) {
    if (!file.exists(cache_path) || dir.exists(cache_path) || triomics_is_symlink(cache_path) ||
        !identical(triomics_sha256(cache_path), as.character(expected_sha256))) {
      triomics_d_resource_abort("D_RESOURCE_CACHE_WRITE", "The local resource cache could not be finalized safely.",
                                as.character(resource$resource_id))
    }
  }
  manifest <- triomics_d_resource_manifest(
    resource, operation, "available", "written",
    normalizePath(cache_path, winslash = "/", mustWork = TRUE), network_policy
  )
  manifest$path <- manifest$local_cache
  manifest$sha256 <- manifest$content_sha256
  manifest
}
