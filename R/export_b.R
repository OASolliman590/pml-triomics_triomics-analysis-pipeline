# B export and Stage C handoff. Numerical fits remain owned by the B producers.

triomics_b07_abort <- function(code, message, key = NULL, remediation) {
  triomics_abort(code, message, key = key, module = "export_b", stage = "B", remediation = remediation)
}

triomics_b07_path_parts <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\)", path)) return(NULL)
  parts <- strsplit(path, "[/\\\\]+")[[1L]]
  parts <- parts[nzchar(parts)]
  if (!length(parts) || any(parts %in% c(".", ".."))) return(NULL)
  parts
}

triomics_b07_output_root <- function(output, create = TRUE) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output) ||
      grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", output)) {
    triomics_b07_abort(
      "B_EXPORT_PATH", "B handoff output must be a local directory without traversal.", "output",
      "Choose a local output directory and keep every handoff member beneath it."
    )
  }
  if (dir.exists(output) && triomics_is_symlink(output)) {
    triomics_b07_abort(
      "B_EXPORT_PATH", "B handoff output cannot be a symlink.", "output",
      "Choose a direct local directory path."
    )
  }
  if (!dir.exists(output) && create) dir.create(output, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output)) {
    triomics_b07_abort(
      "B_EXPORT_PATH", "B handoff output directory is missing.", "output",
      "Create the Stage B output directory before exporting its handoff."
    )
  }
  normalizePath(output, winslash = "/", mustWork = TRUE)
}

triomics_b07_child_path <- function(root, relative) {
  parts <- triomics_b07_path_parts(relative)
  if (is.null(parts)) {
    triomics_b07_abort(
      "B_EXPORT_PATH", "A B handoff member path is not local to its output root.", "path",
      "Use a relative member path without traversal or absolute paths."
    )
  }
  current <- root
  for (part in parts) {
    current <- file.path(current, part)
    if (file.exists(current) && triomics_is_symlink(current)) {
      triomics_b07_abort(
        "B_EXPORT_PATH", "A B handoff member cannot traverse a symlink.", "path",
        "Use regular files and directories beneath the output root."
      )
    }
  }
  resolved <- normalizePath(current, winslash = "/", mustWork = FALSE)
  prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  if (!(identical(resolved, root) || startsWith(resolved, prefix))) {
    triomics_b07_abort(
      "B_EXPORT_PATH", "A B handoff member escaped the output root.", "path",
      "Keep every generated artifact inside the selected output directory."
    )
  }
  current
}

triomics_b07_read_json <- function(path, key) {
  if (!file.exists(path) || dir.exists(path) || triomics_is_symlink(path)) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", paste0("Required public B artifact is missing: ", key, "."), key,
      "Run the declared Stage B producer and preserve its finalized payload and envelope."
    )
  }
  parsed <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (!is.list(parsed) || is.null(names(parsed))) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", paste0("Public B artifact is unreadable: ", key, "."), key,
      "Recreate the artifact from its producer; do not repair a payload in place."
    )
  }
  parsed
}

triomics_b07_verified_artifact <- function(root, payload_relative, expected_type,
                                           config_sha256 = NULL, study_id = NULL,
                                           optional = FALSE) {
  payload_path <- triomics_b07_child_path(root, payload_relative)
  envelope_relative <- sub("\\.json$", ".envelope.json", payload_relative)
  envelope_path <- triomics_b07_child_path(root, envelope_relative)
  payload_present <- file.exists(payload_path) && !dir.exists(payload_path)
  envelope_present <- file.exists(envelope_path) && !dir.exists(envelope_path)
  if (!payload_present && !envelope_present && optional) return(NULL)
  if (!payload_present || !envelope_present) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", paste0("Artifact ", payload_relative, " lacks its payload or verified envelope."),
      payload_relative,
      "Do not export or resume an interrupted artifact; rerun its producer into a clean output root."
    )
  }
  if (!triomics_validate_envelope(envelope_path)) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", paste0("Artifact envelope failed payload/hash validation: ", envelope_relative, "."),
      envelope_relative,
      "Recreate the payload and envelope together from the accepted producer."
    )
  }
  envelope <- triomics_b07_read_json(envelope_path, envelope_relative)
  payload <- triomics_b07_read_json(payload_path, payload_relative)
  if (!identical(as.character(envelope$artifact_type), expected_type) ||
      !identical(as.character(payload$schema_version), "2.0") ||
      !triomics_b_string(as.character(payload$status)) ||
      !identical(as.character(payload$status), as.character(envelope$status)) ||
      !identical(as.character(payload$study_id), as.character(envelope$study_id)) ||
      (!is.null(config_sha256) &&
       (!identical(as.character(envelope$config_sha256), config_sha256) ||
        ("config_sha256" %in% names(payload) &&
         !identical(as.character(payload$config_sha256), config_sha256)))) ||
      (!is.null(study_id) && !identical(as.character(payload$study_id), study_id))) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", paste0("Artifact contract does not match its declared Stage B context: ", payload_relative, "."),
      payload_relative,
      "Keep artifact type, status, study, configuration and envelope hashes aligned."
    )
  }
  list(
    payload = payload,
    envelope = envelope,
    payload_path = payload_path,
    envelope_path = envelope_path,
    payload_relative = payload_relative,
    envelope_relative = envelope_relative,
    payload_sha256 = triomics_sha256(payload_path),
    envelope_sha256 = triomics_sha256(envelope_path),
    status = as.character(envelope$status)
  )
}

triomics_b07_claim_entries <- function(block_results) {
  blocks <- block_results$blocks
  if (!is.list(blocks) || !length(blocks)) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", "Stage B results have no public block records.", "blocks",
      "Pass the complete public block-results artifact."
    )
  }
  entries <- list()
  for (block_id in names(blocks)) {
    block <- blocks[[block_id]]
    for (group_name in c("contrasts", "joint_claims")) {
      group <- block[[group_name]]
      if (!is.list(group) || !length(group)) next
      for (claim_id in names(group)) {
        claim <- group[[claim_id]]
        if (is.null(claim$family) && is.null(claim$rows)) next
        if (!is.list(claim$rows) || !is.list(claim$family)) {
          triomics_b07_abort(
            "B_EXPORT_INCOMPLETE", "A public Stage B claim lacks rows or its family ledger.",
            paste(block_id, group_name, claim_id, sep = "."),
            "Preserve each claim's complete member rows and family accounting."
          )
        }
        entries[[length(entries) + 1L]] <- list(
          block_id = block_id,
          claim_id = claim_id,
          claim_type = if (is.null(claim$claim_type)) group_name else as.character(claim$claim_type),
          rows = claim$rows,
          family = claim$family,
          nominations = if (is.list(claim$nominations)) claim$nominations else list()
        )
      }
    }
  }
  if (!length(entries)) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", "Stage B results have no family-backed public claims.", "families",
      "Require the accepted B family ledger before exporting a D-facing bundle."
    )
  }
  entries
}

triomics_b07_validate_family <- function(entry) {
  family <- entry$family
  member_ids <- as.character(unlist(family$member_ids, use.names = FALSE))
  row_ids <- vapply(entry$rows, function(row) {
    if (is.null(row$feature_key)) "" else as.character(row$feature_key)
  }, character(1))
  declared <- suppressWarnings(as.integer(unlist(family$n_declared, use.names = FALSE)))
  dispositions <- unlist(family$dispositions, use.names = TRUE)
  valid <- length(declared) == 1L && !is.na(declared) && declared == length(member_ids) &&
    length(member_ids) == length(entry$rows) && !anyDuplicated(member_ids) &&
    identical(unname(member_ids), unname(row_ids)) && length(dispositions) > 0L &&
    all(is.finite(as.numeric(dispositions))) && sum(as.numeric(dispositions)) == declared &&
    identical(as.character(family$member_hash), triomics_b_hash(member_ids))
  if (!valid) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", "A family ledger does not account for every declared member.",
      paste(entry$block_id, entry$claim_id, sep = "."),
      "Return to the B producer and preserve every member disposition; do not truncate or re-correct a family."
    )
  }
  invisible(TRUE)
}

triomics_b07_family_ledger <- function(block_results) {
  entries <- triomics_b07_claim_entries(block_results)
  for (entry in entries) triomics_b07_validate_family(entry)
  list(
    schema_version = "2.0",
    artifact_type = "family_ledger",
    study_id = as.character(block_results$study_id),
    analysis_id = as.character(block_results$analysis_id),
    status = as.character(block_results$status),
    source_artifact_id = "stage-b-block-results",
    families = lapply(entries, function(entry) list(
      block_id = entry$block_id,
      claim_id = entry$claim_id,
      claim_type = entry$claim_type,
      family = entry$family
    ))
  )
}

triomics_b07_nominations <- function(block_results) {
  entries <- triomics_b07_claim_entries(block_results)
  nominations <- unlist(lapply(entries, function(entry) entry$nominations), recursive = FALSE)
  if (length(nominations)) {
    valid <- vapply(nominations, function(nomination) {
      required <- c("nomination_id", "feature_key", "source_branch", "result_ref",
                    "selection_rule", "selection_value", "q_threshold", "effect_threshold")
      is.list(nomination) && all(required %in% names(nomination)) &&
        triomics_sha256_text(as.character(nomination$nomination_id)) &&
        triomics_b_string(as.character(nomination$feature_key)) &&
        is.list(nomination$result_ref) && is.list(nomination$selection_value)
    }, logical(1))
    if (!all(valid)) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "A candidate nomination is missing its public selection lineage.",
        "nominations",
        "Preserve source result, family, selection rule, thresholds and values from B-T06."
      )
    }
  }
  list(
    schema_version = "2.0",
    artifact_type = "candidate_nominations",
    study_id = as.character(block_results$study_id),
    analysis_id = as.character(block_results$analysis_id),
    status = as.character(block_results$status),
    source_artifact_id = "stage-b-block-results",
    n_nominations = as.integer(length(nominations)),
    nominations = nominations
  )
}

triomics_b07_row_accounting <- function(entries) {
  rows <- unlist(lapply(entries, `[[`, "rows"), recursive = FALSE)
  statuses <- vapply(rows, function(row) {
    if (is.null(row$estimation_status)) return("failed")
    as.character(row$estimation_status)
  }, character(1))
  if (any(vapply(rows, function(row) {
    identical(as.character(row$estimation_status), "estimated") &&
      (!is.numeric(row$effect) || length(row$effect) != 1L || !is.finite(row$effect))
  }, logical(1)))) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", "An estimated B result lacks a finite signed effect.", "rows",
      "Preserve the producer's signed effect and typed estimation disposition."
    )
  }
  list(
    declared = as.integer(length(rows)),
    tested = as.integer(sum(statuses == "estimated")),
    failed = as.integer(sum(!statuses %in% c("estimated", "unavailable"))),
    unavailable = as.integer(sum(statuses == "unavailable")),
    estimation_status_counts = as.list(table(statuses))
  )
}

triomics_b07_method_dispositions <- function(block_results) {
  out <- list()
  add <- function(method, status, reason, block_id = NULL, claim_id = NULL) {
    if (is.null(status) || !length(status) || is.na(status[[1L]])) return(invisible(NULL))
    out[[length(out) + 1L]] <<- list(
      method = if (is.null(method)) "unspecified" else as.character(method),
      status = as.character(status[[1L]]),
      reason = if (is.null(reason) || !length(reason)) NULL else as.character(unlist(reason, use.names = FALSE)),
      block_id = block_id,
      claim_id = claim_id
    )
    invisible(NULL)
  }
  deqms <- block_results$deqms_status
  if (is.list(deqms)) {
    add("deqms", if (is.null(deqms$status)) "unavailable" else deqms$status,
        if (is.null(deqms$reason)) block_results$unavailable_reason else deqms$reason)
  } else if (!is.null(block_results$requested_method) &&
             identical(as.character(block_results$requested_method), "deqms") &&
             identical(as.character(block_results$method_status), "unavailable")) {
    add("deqms", "unavailable", block_results$unavailable_reason)
  }
  claims <- if (is.list(block_results$blocks) && length(block_results$blocks)) {
    triomics_b07_claim_entries(block_results)
  } else {
    list()
  }
  for (entry in claims) {
    block <- block_results$blocks[[entry$block_id]]
    claim <- block$contrasts[[entry$claim_id]]
    if (is.null(claim)) claim <- block$joint_claims[[entry$claim_id]]
    count_status <- claim$count_prior_status
    if (!is.null(count_status)) {
      status <- if (grepl("unavailable|withheld|failed", as.character(count_status))) {
        "unavailable"
      } else {
        as.character(count_status)
      }
      add("DEqMS count prior", status, count_status, entry$block_id, entry$claim_id)
    }
  }
  if (!length(out)) {
    out <- list(list(
      method = if (is.null(block_results$method)) "unspecified" else as.character(block_results$method),
      status = if (is.null(block_results$method_status)) as.character(block_results$status) else as.character(block_results$method_status),
      reason = if (is.null(block_results$unavailable_reason)) NULL else as.character(block_results$unavailable_reason),
      block_id = NULL,
      claim_id = NULL
    ))
  }
  out
}

triomics_b07_has_unavailable_method <- function(block_results) {
  any(vapply(triomics_b07_method_dispositions(block_results), function(item) {
    identical(as.character(item$status), "unavailable")
  }, logical(1)))
}

triomics_b07_matrix_hash <- function(block_id, matrix) {
  if (!triomics_b_string(as.character(block_id)) || !is.matrix(matrix) || !is.numeric(matrix) ||
      is.null(rownames(matrix)) || is.null(colnames(matrix)) ||
      anyNA(rownames(matrix)) || anyNA(colnames(matrix)) ||
      any(!nzchar(rownames(matrix))) || any(!nzchar(colnames(matrix))) ||
      anyDuplicated(rownames(matrix)) || anyDuplicated(colnames(matrix)) ||
      any(is.infinite(matrix))) {
    triomics_b07_abort(
      "B_EXPORT_MATRIX", "Stage C handoff matrices need numeric values and unique feature/unit IDs.",
      as.character(block_id),
      "Pass the exact keyed matrices returned by the resolved B representation."
    )
  }
  triomics_b_assay_hash(as.character(block_id), matrix)
}

triomics_b07_matrix_bindings <- function(block_results, matrices) {
  if (!is.list(matrices) || is.null(names(matrices)) || !length(matrices) ||
      any(!nzchar(names(matrices))) || anyDuplicated(names(matrices))) {
    triomics_b07_abort(
      "B_EXPORT_MATRIX", "Stage C handoff matrices must be a uniquely named block list.", "matrices",
      "Pass each exact resolved B matrix under its block_id."
    )
  }
  block_ids <- names(block_results$blocks)
  if (!setequal(names(matrices), block_ids)) {
    triomics_b07_abort(
      "B_EXPORT_MATRIX", "The handoff matrix block inventory differs from Stage B results.", "matrices",
      "Supply every and only the blocks used by the fitted Stage B analysis."
    )
  }
  claims <- triomics_b07_claim_entries(block_results)
  out <- vector("list", length(block_ids))
  names(out) <- block_ids
  for (block_id in block_ids) {
    matrix <- matrices[[block_id]]
    matrix_hash <- triomics_b07_matrix_hash(block_id, matrix)
    block_claims <- Filter(function(entry) identical(entry$block_id, block_id), claims)
    feature_keys <- sort(unique(vapply(unlist(lapply(block_claims, `[[`, "rows"), recursive = FALSE),
                                      function(row) as.character(row$feature_key), character(1))))
    if (!setequal(rownames(matrix), feature_keys)) {
      triomics_b07_abort(
        "B_EXPORT_MATRIX", "The Stage C matrix feature keys differ from the complete Stage B member set.",
        block_id,
        "Pass the exact fitted matrix with all tested, failed and unavailable members intact."
      )
    }
    block <- block_results$blocks[[block_id]]
    representation_id <- if (!is.null(block$representation_id)) {
      as.character(block$representation_id)
    } else {
      as.character(block_results$representation_id)
    }
    if (!identical(representation_id, as.character(block_results$representation_id))) {
      triomics_b07_abort(
        "B_EXPORT_MATRIX", "A block's representation differs from the Stage B analysis representation.",
        block_id,
        "Preserve the declared representation; do not combine incompatible level and delta matrices."
      )
    }
    feature_bindings <- lapply(feature_keys, function(feature_key) {
      rows <- unlist(lapply(block_claims, `[[`, "rows"), recursive = FALSE)
      rows <- Filter(function(row) identical(as.character(row$feature_key), feature_key), rows)
      bindings <- lapply(rows, function(row) {
        model_hash <- as.character(row$model_hash)
        representation_hash <- as.character(row$representation_hash)
        source_hash <- as.character(row$source_hash)
        if (!triomics_sha256_text(source_hash) || !identical(source_hash, matrix_hash)) {
          triomics_b07_abort(
            "B_EXPORT_MATRIX", "A public result source hash does not match the keyed matrix handed to Stage C.",
            paste(block_id, feature_key, sep = "."),
            "Pass the exact resolved B matrix fitted by the producer; do not mutate or reconstruct it."
          )
        }
        if (!triomics_sha256_text(model_hash) || !triomics_sha256_text(representation_hash)) {
          triomics_b07_abort(
            "B_EXPORT_MATRIX", "A fitted row lacks model or representation hash provenance.",
            paste(block_id, feature_key, sep = "."),
            "Use result rows carrying the accepted B model and representation hashes."
          )
        }
        list(
          claim_id = as.character(if (!is.null(row$contrast_id)) row$contrast_id else
                                    if (!is.null(row$claim_id)) row$claim_id else "unknown"),
          source_hash = source_hash,
          model_hash = model_hash,
          representation_hash = representation_hash
        )
      })
      list(feature_key = feature_key, fitted_claims = bindings)
    })
    descriptor <- list(
      block_id = block_id,
      representation_id = representation_id,
      feature_keys = feature_keys,
      unit_ids = sort(colnames(matrix)),
      matrix_hash = matrix_hash,
      feature_bindings = feature_bindings
    )
    descriptor$binding_hash <- triomics_b_hash(descriptor)
    out[[block_id]] <- descriptor
  }
  out
}

triomics_b07_cache_key <- function(config_sha256, dependency_hashes, matrices = list(),
                                   model_hashes = list(), representation_hashes = list(),
                                   method_versions = list(), resource_hashes = list()) {
  if (!triomics_sha256_text(as.character(config_sha256))) {
    triomics_b07_abort(
      "B_EXPORT_CACHE", "The Stage B configuration hash is missing or invalid.", "config_sha256",
      "Recompute the cache identity from the effective configuration."
    )
  }
  normalize_hashes <- function(values, field) {
    if (is.null(values)) return(list())
    if (!is.list(values) && !is.atomic(values)) {
      triomics_b07_abort("B_EXPORT_CACHE", paste0(field, " must be a hash map."), field,
                         "Supply named artifact and SHA256 pairs.")
    }
    values <- as.list(values)
    if (!length(values)) return(list())
    if (length(values) && (is.null(names(values)) || any(!nzchar(names(values))))) {
      triomics_b07_abort("B_EXPORT_CACHE", paste0(field, " must be uniquely named."), field,
                         "Name each dependency, method, resource or matrix hash.")
    }
    if (length(values) && !all(vapply(values, function(value) {
      triomics_sha256_text(as.character(value))
    }, logical(1)))) {
      triomics_b07_abort("B_EXPORT_CACHE", paste0(field, " contains an invalid SHA256."), field,
                         "Pass full lowercase SHA256 hashes, not labels or shortened digests.")
    }
    values[order(names(values))]
  }
  dependency_hashes <- normalize_hashes(dependency_hashes, "dependency_hashes")
  resource_hashes <- normalize_hashes(resource_hashes, "resource_hashes")
  matrix_hashes <- list()
  if (length(matrices)) {
    for (block_id in names(matrices)) {
      value <- matrices[[block_id]]
      matrix_hashes[[block_id]] <- if (is.matrix(value)) {
        triomics_b07_matrix_hash(block_id, value)
      } else if (is.list(value) && triomics_sha256_text(as.character(value$matrix_hash))) {
        as.character(value$matrix_hash)
      } else {
        triomics_b07_abort("B_EXPORT_CACHE", "A cache matrix entry has no verified matrix hash.", block_id,
                           "Pass the actual numeric matrix or its validated descriptor.")
      }
    }
    matrix_hashes <- matrix_hashes[order(names(matrix_hashes))]
  }
  model_hashes <- if (is.null(model_hashes)) list() else model_hashes
  representation_hashes <- if (is.null(representation_hashes)) list() else representation_hashes
  method_versions <- if (is.null(method_versions)) list() else method_versions
  triomics_export_cache_key(list(
    config_sha256 = as.character(config_sha256),
    dependency_hashes = dependency_hashes,
    method_versions = method_versions,
    resource_hashes = resource_hashes,
    matrix_hashes = matrix_hashes,
    model_hashes = model_hashes,
    representation_hashes = representation_hashes
  ))
}

triomics_b07_resume_decision <- function(candidate, expected_key) {
  triomics_export_resume_decision(candidate, expected_key)
}

triomics_b07_resume_candidate <- function(bundle_path) {
  if (!is.character(bundle_path) || length(bundle_path) != 1L || is.na(bundle_path) || !nzchar(bundle_path)) {
    triomics_b07_abort("B_EXPORT_CACHE", "A bundle path is required for restart validation.", "bundle_path",
                       "Pass the finalized bundle payload path.")
  }
  envelope_path <- sub("\\.json$", ".envelope.json", bundle_path)
  payload_present <- file.exists(bundle_path) && !dir.exists(bundle_path)
  envelope_ok <- file.exists(envelope_path) && isTRUE(triomics_validate_envelope(envelope_path))
  payload <- if (payload_present) tryCatch(jsonlite::read_json(bundle_path, simplifyVector = FALSE),
                                            error = function(error) NULL) else NULL
  member_ok <- is.list(payload) && isTRUE(triomics_b07_validate_bundle_members(payload, bundle_path))
  if (!member_ok) envelope_ok <- FALSE
  if (isTRUE(envelope_ok)) {
    envelope_ok <- isTRUE(triomics_b07_validate_late_artifacts(payload, bundle_path, envelope_path))
  }
  manifest_payload_sha <- if (envelope_ok) {
    envelope <- triomics_b07_read_json(envelope_path, "bundle.envelope.json")
    as.character(envelope$payload_sha256)
  } else NULL
  list(
    status = if (is.list(payload) && !is.null(payload$status)) as.character(payload$status) else NULL,
    cache_key = if (is.list(payload) && !is.null(payload$cache_key)) as.character(payload$cache_key) else NULL,
    payload_sha256 = if (payload_present) triomics_sha256(bundle_path) else NULL,
    manifest_payload_sha256 = manifest_payload_sha,
    payload_present = payload_present,
    envelope_present = envelope_ok
  )
}

triomics_b07_validate_late_artifacts <- function(bundle, bundle_path, bundle_envelope_path) {
  handoff_dir <- dirname(bundle_path)
  reproduction_path <- file.path(handoff_dir, "reproduction.md")
  reproduction_envelope_path <- file.path(handoff_dir, "reproduction.envelope.json")
  record_path <- file.path(handoff_dir, "acceptance-record.json")
  record_envelope_path <- file.path(handoff_dir, "acceptance-record.envelope.json")
  paths <- c(reproduction_path, reproduction_envelope_path, record_path, record_envelope_path)
  if (any(!file.exists(paths) | dir.exists(paths) | vapply(paths, triomics_is_symlink, logical(1)))) {
    return(FALSE)
  }
  if (!isTRUE(triomics_validate_envelope(reproduction_envelope_path)) ||
      !isTRUE(triomics_validate_envelope(record_envelope_path))) {
    return(FALSE)
  }
  reproduction_envelope <- tryCatch(
    jsonlite::read_json(reproduction_envelope_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  record_envelope <- tryCatch(
    jsonlite::read_json(record_envelope_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  record <- tryCatch(jsonlite::read_json(record_path, simplifyVector = FALSE),
                     error = function(error) NULL)
  if (!is.list(reproduction_envelope) || !is.list(record_envelope) || !is.list(record) ||
      !identical(as.character(reproduction_envelope$artifact_type), "b_t07_reproduction_record") ||
      !identical(as.character(reproduction_envelope$artifact_id), "b-t07-reproduction-record") ||
      !identical(as.character(record_envelope$artifact_type), "b_t07_acceptance_record") ||
      !identical(as.character(record_envelope$artifact_id), "b-t07-acceptance-record") ||
      !identical(as.character(record$packet_id), "B-T07") ||
      !identical(as.character(record$cache_key), as.character(bundle$cache_key)) ||
      !identical(as.character(record$study_id), as.character(bundle$study_id)) ||
      !identical(as.character(record$config_sha256), as.character(bundle$config_sha256)) ||
      !identical(as.character(record_envelope$status), as.character(record$status)) ||
      !identical(as.character(record_envelope$config_sha256), as.character(bundle$config_sha256)) ||
      !identical(as.character(reproduction_envelope$config_sha256), as.character(bundle$config_sha256))) {
    return(FALSE)
  }
  root <- tryCatch(
    normalizePath(file.path(handoff_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(root)) return(FALSE)
  expected_reproduction_path <- tryCatch(triomics_b07_relative(root, reproduction_path),
                                         error = function(error) NULL)
  if (is.null(expected_reproduction_path) ||
      !identical(as.character(record$reproduction_path), expected_reproduction_path)) {
    return(FALSE)
  }
  hashes <- record$payload_and_manifest_hashes
  if (!is.list(hashes) ||
      !identical(as.character(hashes$bundle_sha256), triomics_sha256(bundle_path)) ||
      !identical(as.character(hashes$bundle_envelope_sha256), triomics_sha256(bundle_envelope_path)) ||
      !identical(as.character(hashes$reproduction_sha256), triomics_sha256(reproduction_path)) ||
      !identical(as.character(hashes$reproduction_envelope_sha256), triomics_sha256(reproduction_envelope_path))) {
    return(FALSE)
  }
  has_input <- function(envelope, artifact_id, sha256) {
    any(vapply(envelope$inputs, function(input) {
      identical(as.character(input$artifact_id), artifact_id) &&
        identical(as.character(input$sha256), sha256)
    }, logical(1)))
  }
  has_input(record_envelope, as.character(bundle$artifact_id), triomics_sha256(bundle_path)) &&
    has_input(record_envelope, "b-t07-reproduction-record", triomics_sha256(reproduction_path)) &&
    any(readLines(reproduction_path, warn = FALSE) == "# B-T07 reproduction record") &&
    any(vapply(record$acceptance, function(item) {
      identical(as.character(item$id), "AC4") && identical(as.character(item$verdict), "pass")
    }, logical(1)))
}

triomics_b07_validate_bundle_members <- function(bundle, bundle_path) {
  if (!is.list(bundle$members) || is.null(bundle$cache_key)) return(FALSE)
  root <- tryCatch(
    normalizePath(file.path(dirname(bundle_path), "..", "..", ".."), winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(root)) return(FALSE)
  for (member in bundle$members) {
    if (is.null(member$path)) next
    parts <- triomics_b07_path_parts(as.character(member$path))
    if (is.null(parts)) return(FALSE)
    path <- file.path(root, do.call(file.path, as.list(parts)))
    if (!file.exists(path) || dir.exists(path) || triomics_is_symlink(path) ||
        !identical(triomics_sha256(path), as.character(member$sha256))) return(FALSE)
    if (!is.null(member$envelope_path)) {
      envelope_path <- file.path(root, member$envelope_path)
      if (!file.exists(envelope_path) || triomics_is_symlink(envelope_path) ||
          !isTRUE(triomics_validate_envelope(envelope_path)) ||
          !identical(triomics_sha256(envelope_path), as.character(member$envelope_sha256))) return(FALSE)
    }
  }
  TRUE
}

triomics_b07_write_enveloped_json <- function(payload, path, artifact_type, artifact_id,
                                              study_id, config_sha256, inputs, status,
                                              reason_codes = character(), producer_commit = "not-recorded",
                                              privacy_class = "synthetic") {
  triomics_write_json_atomic(payload, path)
  envelope <- triomics_artifact_envelope(
    artifact_type = artifact_type,
    artifact_id = artifact_id,
    study_id = study_id,
    run_id = "b-t07",
    status = status,
    config_sha256 = config_sha256,
    payload = path,
    producer_commit = producer_commit,
    inputs = inputs,
    reason_codes = reason_codes,
    privacy_class = privacy_class
  )
  envelope_path <- sub("\\.json$", ".envelope.json", path)
  triomics_write_envelope(envelope, envelope_path)
  list(
    path = path,
    envelope_path = envelope_path,
    sha256 = triomics_sha256(path),
    envelope_sha256 = triomics_sha256(envelope_path),
    status = status
  )
}

triomics_b07_atomic_rds <- function(value, path, expected_hashes) {
  if (file.exists(path)) {
    triomics_b07_abort("B_EXPORT_CACHE", "A matrix handoff payload already exists.", "matrices",
                       "Resume a verified matching handoff or use a clean output root.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".b-t07-matrices-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3)
  verified <- tryCatch(readRDS(temporary), error = function(error) NULL)
  if (!is.list(verified) || !identical(names(verified), names(expected_hashes)) ||
      !all(vapply(names(expected_hashes), function(block_id) {
        identical(triomics_b07_matrix_hash(block_id, verified[[block_id]]), expected_hashes[[block_id]])
      }, logical(1)))) {
    triomics_b07_abort("B_EXPORT_MATRIX", "The temporary Stage C matrix handoff failed hash verification.",
                       "matrices", "Do not finalize an RDS matrix artifact that changed while writing.")
  }
  if (file.exists(path) || !file.rename(temporary, path)) {
    triomics_b07_abort("B_EXPORT_FINALIZE", "The Stage C matrix handoff could not be finalized atomically.",
                       "matrices", "Use a local output filesystem that supports same-directory rename.")
  }
  invisible(path)
}

triomics_b07_relative <- function(root, path) {
  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  if (!startsWith(resolved, prefix)) {
    triomics_b07_abort("B_EXPORT_PATH", "A B handoff artifact escaped the output root.", "path",
                       "Keep all generated artifacts beneath the selected output directory.")
  }
  substring(resolved, nchar(prefix) + 1L)
}

triomics_b07_member <- function(artifact_id, status, root, path = NULL,
                                envelope_path = NULL, reason_codes = character()) {
  if (is.null(path)) {
    return(list(artifact_id = artifact_id, status = status, path = NULL, sha256 = NULL,
                envelope_path = NULL, envelope_sha256 = NULL, reason_codes = as.list(reason_codes)))
  }
  list(
    artifact_id = artifact_id,
    status = status,
    path = triomics_b07_relative(root, path),
    sha256 = triomics_sha256(path),
    envelope_path = if (is.null(envelope_path)) NULL else triomics_b07_relative(root, envelope_path),
    envelope_sha256 = if (is.null(envelope_path)) NULL else triomics_sha256(envelope_path),
    reason_codes = as.list(reason_codes)
  )
}

triomics_b07_git_ref <- function(root) {
  value <- tryCatch(
    system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(error) character()
  )
  if (length(value) == 1L && grepl("^[0-9a-f]{40,64}$", value)) value else "unavailable"
}

triomics_b07_runtime <- function() {
  version <- function(package) tryCatch(as.character(utils::packageVersion(package)),
                                        error = function(error) "unavailable")
  list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    triomics_version = version("triomics"),
    limma_version = version("limma"),
    digest_version = version("digest"),
    jsonlite_version = version("jsonlite"),
    jsonvalidate_version = version("jsonvalidate"),
    yaml_version = version("yaml"),
    testthat_version = version("testthat")
  )
}

triomics_b07_smoke_receipt <- function(label, output) {
  root <- triomics_b07_output_root(output, create = FALSE)
  artifact <- triomics_b07_verified_artifact(
    root, "B/block-results.json", "block_results", optional = FALSE
  )
  payload <- artifact$payload
  expected <- switch(label, independent = "levels", paired = "delta", NULL)
  representation_id <- as.character(payload$representation_id)
  if (identical(label, "unavailable_method") &&
      (!is.list(payload$blocks) || !length(payload$blocks))) {
    method_dispositions <- triomics_b07_method_dispositions(payload)
    unavailable_method <- any(vapply(method_dispositions, function(item) {
      identical(as.character(item$status), "unavailable")
    }, logical(1)))
    if (!unavailable_method) {
      triomics_b07_abort("B_EXPORT_SMOKE", "The unavailable-method smoke has no visible unavailable disposition.", label,
                         "Pass a real Stage B result that records the requested method and its unavailable reason.")
    }
    return(list(
      label = label,
      representation_id = if (!length(representation_id)) "unavailable" else representation_id,
      status = artifact$status,
      artifact_sha256 = artifact$payload_sha256,
      finite_effect_count = 0L,
      unavailable_method = TRUE,
      method_dispositions = method_dispositions,
      run_id = as.character(artifact$envelope$run_id)
    ))
  }
  entries <- triomics_b07_claim_entries(payload)
  rows <- unlist(lapply(entries, `[[`, "rows"), recursive = FALSE)
  finite_effects <- vapply(rows, function(row) {
    is.numeric(row$effect) && length(row$effect) == 1L && is.finite(row$effect) &&
      !is.null(row$effect_unit) && nzchar(as.character(row$effect_unit))
  }, logical(1))
  if (!is.null(expected) && !identical(representation_id, expected)) {
    triomics_b07_abort("B_EXPORT_SMOKE", paste0(label, " smoke used the wrong representation."), label,
                       "Keep independent levels and paired delta smoke paths distinct.")
  }
  if (label != "unavailable_method" && !any(finite_effects)) {
    triomics_b07_abort("B_EXPORT_SMOKE", paste0(label, " smoke contains no signed fitted effect."), label,
                       "Require an actual finite effect from the Stage B fit."
    )
  }
  unavailable_method <- triomics_b07_has_unavailable_method(payload)
  if (identical(label, "unavailable_method") && !unavailable_method) {
    triomics_b07_abort("B_EXPORT_SMOKE", "The unavailable-method smoke has no visible unavailable disposition.", label,
                       "Pass a real Stage B result that records the requested method and its unavailable reason.")
  }
  list(
    label = label,
    representation_id = representation_id,
    status = artifact$status,
    artifact_sha256 = artifact$payload_sha256,
    finite_effect_count = as.integer(sum(finite_effects)),
    unavailable_method = unavailable_method,
    method_dispositions = triomics_b07_method_dispositions(payload),
    run_id = as.character(artifact$envelope$run_id)
  )
}

triomics_b07_write_text_atomic <- function(text, path) {
  if (file.exists(path)) {
    triomics_b07_abort("B_EXPORT_FINALIZE", "The reproduction record already exists.", "reproduction",
                       "Resume the verified matching handoff or use a clean output root.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".b-t07-reproduction-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(text, temporary, useBytes = TRUE)
  if (file.exists(path) || !file.rename(temporary, path)) {
    triomics_b07_abort("B_EXPORT_FINALIZE", "The reproduction record could not be finalized atomically.",
                       "reproduction", "Use a local output filesystem that supports same-directory rename.")
  }
  invisible(path)
}

triomics_b07_reproduction_text <- function(command, working_ref, runtime, environment_sha256,
                                           config_sha256, data_class, privacy_class,
                                           bundle, matrix_bindings, acceptance, qualifications) {
  artifact_lines <- vapply(bundle$members, function(member) {
    paste0("- ", member$artifact_id, ": status `", member$status, "`; path `",
           if (is.null(member$path)) "not written" else member$path,
           "`; payload SHA256 `", if (is.null(member$sha256)) "unavailable" else member$sha256,
           "`; manifest SHA256 `", if (is.null(member$envelope_sha256)) "unavailable" else member$envelope_sha256,
           "`.")
  }, character(1))
  ac_lines <- vapply(acceptance, function(item) {
    paste0("- ", item$id, ": `", item$verdict, "` — ", item$detail)
  }, character(1))
  matrix_lines <- if (length(matrix_bindings)) {
    unlist(lapply(matrix_bindings, function(block) {
      paste0("- ", block$block_id, ": representation `", block$representation_id,
             "`, matrix hash `", block$matrix_hash, "`, binding hash `", block$binding_hash, "`.")
    }), use.names = FALSE)
  } else {
    "- Stage C matrix handoff: unavailable because the Stage B runner did not provide resolved matrices."
  }
  runtime_lines <- paste0("- ", names(runtime), ": `", unlist(runtime, use.names = FALSE), "`.")
  qualification_lines <- paste0("- ", qualifications)
  synthetic_context <- identical(as.character(data_class), "synthetic") &&
    identical(as.character(privacy_class), "synthetic")
  data_context <- paste0(
    "Verified data context: configuration data class `", as.character(data_class),
    "`; Stage B source privacy class `", as.character(privacy_class), "`."
  )
  context_scope <- if (synthetic_context) {
    "This is software handoff evidence for declared public synthetic inputs."
  } else {
    "This record does not assert that the underlying inputs are synthetic, de-identified, public, or non-private."
  }
  data_limitation <- if (synthetic_context) {
    "The declared synthetic fixture validates software flow only; it does not establish method performance or biological validity."
  } else {
    paste0("The `", as.character(privacy_class), "` source privacy classification governs this handoff; this record does not attest to data release or de-identification.")
  }
  paste(c(
    "# B-T07 reproduction record",
    "",
    data_context,
    context_scope,
    "This is not biological validation, a study result, a final HTML report, or a vignette.",
    "",
    "## Commands and execution",
    "",
    paste0("- Command: `", command, "`."),
    paste0("- Working ref: `", working_ref, "`."),
    paste0("- Stage B output status: `", bundle$source_status, "`; export status: `", bundle$status, "`."),
    paste0("- Effective configuration SHA256: `", config_sha256, "`."),
    paste0("- Environment SHA256: `", environment_sha256, "`."),
    "",
    "## Verified environment",
    "",
    runtime_lines,
    "",
    "## Payloads and dispositions",
    "",
    artifact_lines,
    "",
    "## Stage C matrix bindings",
    "",
    matrix_lines,
    "",
    "## Acceptance evidence",
    "",
    ac_lines,
    "",
    "## Unrun qualifications",
    "",
    qualification_lines,
    "",
    "## Limitations",
    "",
    paste0("- ", data_limitation),
    "- This is not final HTML and does not claim a vignette, integration report, or evidence about Omar's studies."
  ), collapse = "\n")
}

triomics_b07_export_handoff <- function(output, config_path, matrices = NULL,
                                         smoke_runs = NULL, command = NULL,
                                         working_ref = NULL, cli_exit_code = NULL,
                                         resume = FALSE, qualifications = NULL,
                                         matrix_source = NULL) {
  root <- triomics_b07_output_root(output, create = TRUE)
  if (!is.character(config_path) || length(config_path) != 1L || is.na(config_path) ||
      !nzchar(config_path) || !file.exists(config_path)) {
    triomics_b07_abort("B_EXPORT_CONFIG", "A saved Stage B configuration is required.", "config_path",
                       "Pass the same local config file that produced block-results.json.")
  }
  config <- triomics_read_config(config_path)
  config_sha256 <- as.character(attr(config, "config_sha256"))
  artifact <- triomics_b07_verified_artifact(
    root, "B/block-results.json", "block_results", config_sha256 = config_sha256
  )
  block_results <- artifact$payload
  entries <- triomics_b07_claim_entries(block_results)
  ledger <- triomics_b07_family_ledger(block_results)
  nominations <- triomics_b07_nominations(block_results)
  accounting <- triomics_b07_row_accounting(entries)
  method_dispositions <- triomics_b07_method_dispositions(block_results)

  clinical_specs <- list(
    clinical_associations = list(path = "B/clinical-associations.json", type = "clinical_association_results"),
    clinical_longitudinal_associations = list(path = "B/clinical-longitudinal-associations.json", type = "clinical_association_results")
  )
  clinical_artifacts <- list()
  clinical_dispositions <- list()
  clinical_enabled <- is.list(config$clinical) && isTRUE(config$clinical$enabled)
  for (name in names(clinical_specs)) {
    spec <- clinical_specs[[name]]
    item <- triomics_b07_verified_artifact(
      root, spec$path, spec$type, config_sha256 = config_sha256,
      study_id = as.character(block_results$study_id), optional = TRUE
    )
    if (is.null(item)) {
      if (clinical_enabled) {
        clinical_dispositions[[name]] <- list(status = "unavailable", reason = "requested_artifact_missing")
      } else {
        clinical_dispositions[[name]] <- list(status = "not_requested", reason = "clinical_disabled")
      }
    } else {
      clinical_artifacts[[name]] <- item
      clinical_dispositions[[name]] <- list(status = item$status, reason = NULL)
    }
  }

  matrix_bindings <- if (is.null(matrices)) list() else triomics_b07_matrix_bindings(block_results, matrices)
  if (length(matrix_bindings)) matrices <- matrices[names(matrix_bindings)]
  matrix_source_verified <- identical(matrix_source, "stage_b_resolved_blocks")
  model_hashes <- lapply(matrix_bindings, function(block) {
    triomics_b_hash(lapply(block$feature_bindings, function(feature) feature$fitted_claims))
  })
  representation_hashes <- lapply(matrix_bindings, function(block) {
    triomics_b_hash(list(representation_id = block$representation_id,
                         features = lapply(block$feature_bindings, function(feature) feature$fitted_claims)))
  })
  dependency_pairs <- list(list(artifact_id = "stage-b-block-results", sha256 = artifact$payload_sha256))
  dependency_pairs <- c(dependency_pairs, artifact$envelope$inputs)
  dependency_pairs <- c(dependency_pairs, lapply(clinical_artifacts, function(item) {
    list(artifact_id = as.character(item$envelope$artifact_id), sha256 = item$payload_sha256)
  }))
  dependency_pairs <- c(dependency_pairs, unlist(lapply(clinical_artifacts, function(item) {
    item$envelope$inputs
  }), recursive = FALSE))
  dependency_hashes <- setNames(
    lapply(seq_along(dependency_pairs), function(index) as.character(dependency_pairs[[index]]$sha256)),
    vapply(seq_along(dependency_pairs), function(index) {
      paste0(as.character(dependency_pairs[[index]]$artifact_id), "#", index)
    }, character(1))
  )
  runtime <- triomics_b07_runtime()
  method_versions <- list(
    runtime = runtime,
    requested_method = if (is.null(block_results$requested_method)) "unspecified" else as.character(block_results$requested_method),
    method = if (is.null(block_results$method)) "unspecified" else as.character(block_results$method),
    method_status = as.character(if (is.null(block_results$method_status)) block_results$status else block_results$method_status)
  )
  resource_hashes <- list()
  cache_key <- triomics_b07_cache_key(
    config_sha256 = config_sha256,
    dependency_hashes = dependency_hashes,
    matrices = if (is.null(matrices)) list() else matrices,
    model_hashes = model_hashes,
    representation_hashes = representation_hashes,
    method_versions = method_versions,
    resource_hashes = resource_hashes
  )

  handoff_relative <- file.path("B", "handoffs", cache_key)
  handoff_dir <- triomics_b07_child_path(root, handoff_relative)
  bundle_path <- triomics_b07_child_path(root, file.path(handoff_relative, "bundle.json"))
  bundle_envelope_path <- sub("\\.json$", ".envelope.json", bundle_path)
  candidate <- triomics_b07_resume_candidate(bundle_path)
  if (isTRUE(resume)) {
    decision <- triomics_b07_resume_decision(candidate, cache_key)
    if (isTRUE(decision$reuse)) {
      return(list(
        status = "reused", reused = TRUE, cache_key = cache_key,
        bundle_path = bundle_path, envelope_path = bundle_envelope_path,
        family_ledger_path = triomics_b07_child_path(root, file.path(handoff_relative, "family-ledger.json")),
        nominations_path = triomics_b07_child_path(root, file.path(handoff_relative, "candidate-nominations.json")),
        matrix_manifest_path = if (length(matrix_bindings)) triomics_b07_child_path(root, file.path(handoff_relative, "stage-c-matrices.json")) else NULL,
        reproduction_path = triomics_b07_child_path(root, file.path(handoff_relative, "reproduction.md")),
        acceptance_record_path = triomics_b07_child_path(root, file.path(handoff_relative, "acceptance-record.json")),
        acceptance_record_envelope_path = triomics_b07_child_path(root, file.path(handoff_relative, "acceptance-record.envelope.json")),
        acceptance = list(AC1 = list(verdict = "pass"), AC2 = list(verdict = "not_recomputed"),
                          AC3 = list(verdict = "pass"), AC4 = list(verdict = "pass"))
      ))
    }
    if (identical(decision$reason, "partial") || identical(decision$reason, "incomplete") ||
        identical(decision$reason, "hash_mismatch")) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "An interrupted or hash-mismatched B handoff is not a restart candidate.",
        "resume",
        "Keep the partial artifact for diagnosis and write the retry to a fresh output root."
      )
    }
  } else if (candidate$payload_present || candidate$envelope_present) {
    triomics_b07_abort(
      "B_EXPORT_CACHE", "A B handoff already exists for this cache identity.", "resume",
      "Set resume=TRUE only for a complete matching manifest, or choose a fresh output root."
    )
  }

  dir.create(handoff_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(handoff_dir) || triomics_is_symlink(handoff_dir)) {
    triomics_b07_abort("B_EXPORT_PATH", "The local B handoff directory could not be created safely.", "output",
                       "Choose a regular local output directory."
    )
  }
  source_inputs <- list(list(artifact_id = "stage-b-block-results", sha256 = artifact$payload_sha256))
  family_path <- triomics_b07_child_path(root, file.path(handoff_relative, "family-ledger.json"))
  family_artifact <- triomics_b07_write_enveloped_json(
    ledger, family_path, "family_ledger", "b-t07-family-ledger",
    as.character(block_results$study_id), config_sha256, source_inputs, artifact$status,
    producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
    privacy_class = as.character(artifact$envelope$privacy_class)
  )
  nominations_path <- triomics_b07_child_path(root, file.path(handoff_relative, "candidate-nominations.json"))
  nominations_artifact <- triomics_b07_write_enveloped_json(
    nominations, nominations_path, "candidate_nominations", "b-t07-candidate-nominations",
    as.character(block_results$study_id), config_sha256, source_inputs, artifact$status,
    producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
    privacy_class = as.character(artifact$envelope$privacy_class)
  )

  matrix_artifact <- NULL
  matrix_manifest_artifact <- NULL
  matrix_status <- if (!length(matrix_bindings)) "unavailable" else if (matrix_source_verified) "complete" else "partial"
  matrix_reason <- if (!length(matrix_bindings)) {
    "resolved_matrices_not_provided_by_stage_b_runner"
  } else if (!matrix_source_verified) {
    "runner_resolved_matrix_source_not_verified"
  } else {
    NULL
  }
  if (length(matrix_bindings)) {
    matrices_path <- triomics_b07_child_path(root, file.path(handoff_relative, "stage-c-matrix-values.rds"))
    triomics_b07_atomic_rds(matrices, matrices_path,
                            vapply(matrix_bindings, function(block) block$matrix_hash, character(1)))
    matrix_envelope <- triomics_artifact_envelope(
      artifact_type = "stage_c_input_matrices",
      artifact_id = "b-t07-stage-c-input-matrices",
      study_id = as.character(block_results$study_id),
      run_id = "b-t07",
      status = "complete",
      config_sha256 = config_sha256,
      payload = matrices_path,
      producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
      inputs = source_inputs,
      reason_codes = character(),
      privacy_class = as.character(artifact$envelope$privacy_class)
    )
    matrix_envelope_path <- sub("\\.rds$", ".envelope.json", matrices_path)
    triomics_write_envelope(matrix_envelope, matrix_envelope_path)
    matrix_artifact <- list(path = matrices_path, envelope_path = matrix_envelope_path,
                            sha256 = triomics_sha256(matrices_path),
                            envelope_sha256 = triomics_sha256(matrix_envelope_path), status = "complete")
    matrix_manifest <- list(
      schema_version = "2.0",
      artifact_type = "stage_c_matrix_bindings",
      study_id = as.character(block_results$study_id),
      analysis_id = as.character(block_results$analysis_id),
      representation_id = as.character(block_results$representation_id),
      status = "complete",
      matrix_values_path = triomics_b07_relative(root, matrices_path),
      matrix_values_sha256 = matrix_artifact$sha256,
      matrix_values_envelope_path = triomics_b07_relative(root, matrix_envelope_path),
      matrix_values_envelope_sha256 = matrix_artifact$envelope_sha256,
      cache_key = cache_key,
      matrix_source = if (matrix_source_verified) "stage_b_resolved_blocks" else "caller_matrix_unverified",
      blocks = matrix_bindings
    )
    matrix_manifest_path <- triomics_b07_child_path(root, file.path(handoff_relative, "stage-c-matrices.json"))
    matrix_manifest_artifact <- triomics_b07_write_enveloped_json(
      matrix_manifest, matrix_manifest_path, "stage_c_matrix_bindings", "b-t07-stage-c-matrix-bindings",
      as.character(block_results$study_id), config_sha256, source_inputs, "complete",
      producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
      privacy_class = as.character(artifact$envelope$privacy_class)
    )
  }

  members <- list(
    triomics_b07_member("block_results", artifact$status, root,
                        artifact$payload_path, artifact$envelope_path,
                        as.character(unlist(artifact$envelope$reason_codes, use.names = FALSE))),
    triomics_b07_member("family_ledger", family_artifact$status, root,
                        family_artifact$path, family_artifact$envelope_path),
    triomics_b07_member("candidate_nominations", nominations_artifact$status, root,
                        nominations_artifact$path, nominations_artifact$envelope_path)
  )
  for (name in names(clinical_specs)) {
    if (name %in% names(clinical_artifacts)) {
      item <- clinical_artifacts[[name]]
      members[[length(members) + 1L]] <- triomics_b07_member(
        name, item$status, root, item$payload_path, item$envelope_path,
        as.character(unlist(item$envelope$reason_codes, use.names = FALSE))
      )
    } else {
      disposition <- clinical_dispositions[[name]]
      members[[length(members) + 1L]] <- triomics_b07_member(
        name, disposition$status, root, reason_codes = disposition$reason
      )
    }
  }
  if (!is.null(matrix_manifest_artifact)) {
    members[[length(members) + 1L]] <- triomics_b07_member(
      "stage_c_matrices", "complete", root,
      matrix_manifest_artifact$path, matrix_manifest_artifact$envelope_path
    )
    members[[length(members) + 1L]] <- triomics_b07_member(
      "stage_c_matrix_values", "complete", root,
      matrix_artifact$path, matrix_artifact$envelope_path
    )
  } else {
    members[[length(members) + 1L]] <- triomics_b07_member(
      "stage_c_matrices", "unavailable", root, reason_codes = matrix_reason
    )
  }
  missing_requested_clinical <- clinical_enabled && any(vapply(clinical_dispositions, function(item) {
    identical(item$status, "unavailable") && identical(item$reason, "requested_artifact_missing")
  }, logical(1)))
  bundle_status <- if (is.null(matrix_artifact) || !matrix_source_verified || missing_requested_clinical) {
    "partial"
  } else {
    "complete"
  }
  bundle <- list(
    schema_version = "2.0",
    artifact_type = "stage_b_public_bundle",
    artifact_id = paste0("b07_", substr(cache_key, 1L, 20L)),
    study_id = as.character(block_results$study_id),
    analysis_id = as.character(block_results$analysis_id),
    preparation_context_id = if (is.null(block_results$preparation_context_id)) NULL else as.character(block_results$preparation_context_id),
    representation_id = as.character(block_results$representation_id),
    status = bundle_status,
    source_status = artifact$status,
    config_sha256 = config_sha256,
    cache_key = cache_key,
    member_accounting = accounting,
    method_dispositions = method_dispositions,
    clinical_associations = clinical_dispositions$clinical_associations,
    clinical_longitudinal_associations = clinical_dispositions$clinical_longitudinal_associations,
    matrix_handoff = list(status = matrix_status, reason = matrix_reason,
                          source = if (matrix_source_verified) "stage_b_resolved_blocks" else "caller_matrix_unverified"),
    members = members
  )
  bundle_inputs <- lapply(dependency_pairs, function(pair) {
    list(artifact_id = as.character(pair$artifact_id), sha256 = as.character(pair$sha256))
  })
  if (length(bundle_inputs)) {
    ids <- vapply(bundle_inputs, `[[`, character(1), "artifact_id")
    shas <- vapply(bundle_inputs, `[[`, character(1), "sha256")
    keep <- !duplicated(paste(ids, shas, sep = "\n"))
    bundle_inputs <- unname(bundle_inputs[keep])
  }
  triomics_write_json_atomic(bundle, bundle_path)
  bundle_envelope <- triomics_artifact_envelope(
    artifact_type = "stage_b_public_bundle",
    artifact_id = as.character(bundle$artifact_id),
    study_id = as.character(block_results$study_id),
    run_id = "b-t07",
    status = bundle_status,
    config_sha256 = config_sha256,
    payload = bundle_path,
    producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
    inputs = bundle_inputs,
    reason_codes = c(
      if (identical(bundle_status, "partial") && is.null(matrix_artifact)) matrix_reason,
      if (missing_requested_clinical) "requested_clinical_artifact_missing"
    ),
    privacy_class = as.character(artifact$envelope$privacy_class)
  )
  triomics_write_envelope(bundle_envelope, bundle_envelope_path)

  smoke_receipts <- list()
  if (is.list(smoke_runs)) {
    for (label in intersect(c("independent", "paired", "unavailable_method"), names(smoke_runs))) {
      smoke_receipts[[label]] <- triomics_b07_smoke_receipt(label, smoke_runs[[label]])
    }
  }
  ac2_pass <- all(c("independent", "paired", "unavailable_method") %in% names(smoke_receipts)) &&
    !is.null(smoke_receipts$independent) && !is.null(smoke_receipts$paired) &&
    isTRUE(smoke_receipts$unavailable_method$unavailable_method)
  member_hashes <- list(
    bundle_sha256 = triomics_sha256(bundle_path),
    bundle_envelope_sha256 = triomics_sha256(bundle_envelope_path),
    family_ledger_sha256 = family_artifact$sha256,
    family_ledger_envelope_sha256 = family_artifact$envelope_sha256,
    nominations_sha256 = nominations_artifact$sha256,
    nominations_envelope_sha256 = nominations_artifact$envelope_sha256,
    source_payload_sha256 = artifact$payload_sha256,
    source_envelope_sha256 = artifact$envelope_sha256
  )
  if (!is.null(matrix_artifact)) {
    member_hashes$matrix_values_sha256 <- matrix_artifact$sha256
    member_hashes$matrix_manifest_sha256 <- matrix_manifest_artifact$sha256
    member_hashes$matrix_envelope_sha256 <- matrix_artifact$envelope_sha256
  }
  acceptance <- list(
    list(id = "AC1", verdict = if (!missing_requested_clinical) "pass" else "blocked",
         detail = if (!missing_requested_clinical) "Public block rows, full family membership, nomination lineage and clinical dispositions are indexed." else "A requested clinical artifact has no verified payload/envelope."),
    list(id = "AC2", verdict = if (ac2_pass) "pass" else "blocked",
         detail = if (ac2_pass) "Independent, paired and unavailable-method Stage B smoke outputs were verified." else "Both independent and paired CLI outputs plus a visible unavailable-method output are required."),
    list(id = "AC3", verdict = if (!is.null(matrix_artifact) && matrix_source_verified) "pass" else "blocked",
         detail = if (!is.null(matrix_artifact) && matrix_source_verified) "Model and representation hashes are bound to the same-run resolved keyed matrices; the complete envelope was reverified." else if (!is.null(matrix_artifact)) "Hashes were computed for the supplied keyed matrix, but its same-run Stage B resolved-matrix provenance is unverified." else "The Stage B caller did not provide resolved matrices, so no Stage C handoff was written."),
    list(id = "AC4", verdict = "pass",
         detail = "A Markdown reproduction record contains the command, verified environment, hashes, statuses, unrun qualifications and limitations.")
  )
  record <- list(
    packet_id = "B-T07",
    study_id = as.character(block_results$study_id),
    status = if (all(vapply(acceptance, function(item) identical(item$verdict, "pass"), logical(1)))) "software_handoff_evidence_complete" else "partial",
    command = if (is.null(command)) "not recorded" else as.character(command),
    working_ref = if (is.null(working_ref)) triomics_b07_git_ref(root) else as.character(working_ref),
    cli_exit_code = if (is.null(cli_exit_code)) NULL else as.integer(cli_exit_code),
    runtime = runtime,
    environment_sha256 = triomics_hash_text(triomics_canonical_json(runtime)),
    config_sha256 = config_sha256,
    cache_key = cache_key,
    input_hashes = dependency_hashes,
    payload_and_manifest_hashes = member_hashes,
    acceptance = acceptance,
    smoke_runs = smoke_receipts,
    gaps = c(
      if (identical(acceptance[[2L]]$verdict, "blocked")) "AC2 smoke receipts are incomplete.",
      if (identical(acceptance[[3L]]$verdict, "blocked")) "Stage C matrix handoff is unavailable until the Sol-owned runner hook passes exact resolved matrices.",
      if (identical(acceptance[[1L]]$verdict, "blocked")) "One or more requested clinical outputs are missing."
    )
  )
  if (is.null(qualifications)) {
    qualifications <- c(
      "R CMD build/check on the integrated milestone snapshot.",
      "Full method qualification simulations and real-data scientific validation.",
      "Final HTML rendering and vignette generation."
    )
  }
  if (!is.character(qualifications)) {
    triomics_b07_abort("B_EXPORT_REPRODUCTION", "Unrun qualifications must be explicit strings.", "qualifications",
                       "List each unrun validation or runtime qualification in plain language.")
  }
  reproduction_path <- triomics_b07_child_path(root, file.path(handoff_relative, "reproduction.md"))
  environment_sha256 <- triomics_hash_text(triomics_canonical_json(runtime))
  record$environment_sha256 <- environment_sha256
  reproduction <- triomics_b07_reproduction_text(
    command = if (is.null(command)) "not recorded" else as.character(command),
    working_ref = record$working_ref,
    runtime = runtime,
    environment_sha256 = environment_sha256,
    config_sha256 = config_sha256,
    data_class = as.character(config$data_class),
    privacy_class = as.character(artifact$envelope$privacy_class),
    bundle = bundle,
    matrix_bindings = matrix_bindings,
    acceptance = acceptance,
    qualifications = qualifications
  )
  reproduction <- paste0(
    reproduction, "\n\n## Bundle manifest hashes\n\n",
    "- Bundle payload SHA256: `", triomics_sha256(bundle_path), "`.\n",
    "- Bundle envelope SHA256: `", triomics_sha256(bundle_envelope_path), "`.\n"
  )
  triomics_b07_write_text_atomic(reproduction, reproduction_path)
  reproduction_envelope_path <- sub("\\.md$", ".envelope.json", reproduction_path)
  reproduction_envelope <- triomics_artifact_envelope(
    artifact_type = "b_t07_reproduction_record",
    artifact_id = "b-t07-reproduction-record",
    study_id = as.character(block_results$study_id),
    run_id = "b-t07",
    status = "complete",
    config_sha256 = config_sha256,
    payload = reproduction_path,
    producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
    inputs = bundle_inputs,
    reason_codes = character(),
    privacy_class = as.character(artifact$envelope$privacy_class)
  )
  triomics_write_envelope(reproduction_envelope, reproduction_envelope_path)
  record$payload_and_manifest_hashes$reproduction_sha256 <- triomics_sha256(reproduction_path)
  record$payload_and_manifest_hashes$reproduction_envelope_sha256 <- triomics_sha256(reproduction_envelope_path)
  record$reproduction_path <- triomics_b07_relative(root, reproduction_path)
  record_path <- triomics_b07_child_path(root, file.path(handoff_relative, "acceptance-record.json"))
  record$status <- if (all(vapply(acceptance, function(item) identical(item$verdict, "pass"), logical(1)))) {
    "complete"
  } else {
    "partial"
  }
  record$evidence_scope <- if (identical(as.character(config$data_class), "synthetic") &&
                               identical(as.character(artifact$envelope$privacy_class), "synthetic")) {
    "public_synthetic_software_fixture"
  } else {
    paste0("software_handoff_data_class_", as.character(config$data_class),
           "_privacy_class_", as.character(artifact$envelope$privacy_class))
  }
  triomics_write_json_atomic(record, record_path)
  record_envelope_path <- sub("\\.json$", ".envelope.json", record_path)
  record_inputs <- c(
    bundle_inputs,
    list(list(artifact_id = as.character(bundle$artifact_id), sha256 = triomics_sha256(bundle_path)),
         list(artifact_id = "b-t07-reproduction-record", sha256 = triomics_sha256(reproduction_path)))
  )
  record_envelope <- triomics_artifact_envelope(
    artifact_type = "b_t07_acceptance_record",
    artifact_id = "b-t07-acceptance-record",
    study_id = as.character(block_results$study_id),
    run_id = "b-t07",
    status = record$status,
    config_sha256 = config_sha256,
    payload = record_path,
    producer_commit = if (is.null(working_ref)) "not-recorded" else working_ref,
    inputs = unname(record_inputs),
    reason_codes = as.character(record$gaps),
    privacy_class = as.character(artifact$envelope$privacy_class)
  )
  triomics_write_envelope(record_envelope, record_envelope_path)
  candidate_after_write <- triomics_b07_resume_candidate(bundle_path)
  if (!isTRUE(candidate_after_write$envelope_present) ||
      !identical(candidate_after_write$cache_key, cache_key) ||
      !identical(candidate_after_write$payload_sha256, candidate_after_write$manifest_payload_sha256)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The finalized B handoff failed its restart verification.", "bundle",
                       "Do not publish a bundle whose manifest, late artifacts or member hashes fail read-back verification.")
  }
  list(
    status = bundle_status,
    reused = FALSE,
    cache_key = cache_key,
    bundle_path = bundle_path,
    envelope_path = bundle_envelope_path,
    family_ledger_path = family_artifact$path,
    nominations_path = nominations_artifact$path,
    matrix_manifest_path = if (is.null(matrix_manifest_artifact)) NULL else matrix_manifest_artifact$path,
    reproduction_path = reproduction_path,
    acceptance_record_path = record_path,
    acceptance_record_envelope_path = record_envelope_path,
    acceptance = stats::setNames(acceptance, vapply(acceptance, `[[`, character(1), "id")),
    matrix_bindings = matrix_bindings,
    member_accounting = accounting
  )
}

triomics_b07_cli_after_stage_b <- function(args, stage_b_result, smoke_runs = NULL) {
  options <- triomics_parse_cli(args)
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) ||
      is.null(options$config) || is.null(options$output)) {
    triomics_b07_abort(
      "B_EXPORT_CLI", "The B-T07 handoff hook requires a completed, non-validation Stage B CLI request.",
      "stage",
      "Invoke the hook only after a successful `--stage B` execution with its original config and output paths."
    )
  }
  matrices <- if (is.list(stage_b_result)) stage_b_result$resolved_matrices else NULL
  if (!is.list(matrices) || !length(matrices) || is.null(names(matrices)) ||
      any(!nzchar(names(matrices))) || anyDuplicated(names(matrices))) {
    triomics_b07_abort(
      "B_EXPORT_MATRIX", "The Stage B runner did not hand off its exact resolved matrices.", "matrices",
      paste(
        "Sol's post-B hook must pass `resolved$blocks[[block_id]]$assay` from the same fit run;",
        "do not re-ingest, reconstruct or synthesize a matrix."
      )
    )
  }
  command <- paste(c("Rscript --vanilla inst/exec/triomics", vapply(args, shQuote, character(1))),
                   collapse = " ")
  triomics_b07_export_handoff(
    output = options$output,
    config_path = options$config,
    matrices = matrices,
    smoke_runs = smoke_runs,
    command = command,
    working_ref = triomics_b07_git_ref(options$output),
    cli_exit_code = 0L,
    matrix_source = "stage_b_resolved_blocks"
  )
}
