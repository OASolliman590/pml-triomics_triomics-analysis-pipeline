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
  absent <- triomics_export_resume_decision(NULL, expected_key)
  if (is.null(candidate) || !isTRUE(candidate$payload_present)) return(absent)
  if (!isTRUE(candidate$envelope_present)) {
    return(list(reuse = FALSE, reason = "partial"))
  }
  if (!isTRUE(candidate$payload_readable) || !isTRUE(candidate$base_envelope_valid) ||
      !identical(candidate$status, "complete")) {
    return(list(reuse = FALSE, reason = "incomplete"))
  }

  decision <- triomics_export_resume_decision(candidate, expected_key)
  if (!identical(decision$reason, "complete_match")) return(decision)
  if (!isTRUE(candidate$payload_hash_matches)) {
    return(list(reuse = FALSE, reason = "hash_mismatch"))
  }
  if (!isTRUE(candidate$required_evidence_complete)) {
    return(list(reuse = FALSE, reason = "incomplete"))
  }
  decision
}

triomics_b07_prov_canonical_value <- function(value) {
  json <- as.character(jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE, force = TRUE
  ))
  triomics_jcs_canonicalize_json(json)
}

triomics_b07_prov_value_sha256 <- function(value) {
  digest::digest(charToRaw(enc2utf8(triomics_b07_prov_canonical_value(value))),
                 algo = "sha256", serialize = FALSE)
}

triomics_b07_prov_ordered_ids_sha256 <- function(values) {
  values <- as.character(values)
  if (anyNA(values) || any(!nzchar(values))) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "Provenance ordered identities contain empty values.",
                       "provenance", "Regenerate the profile from complete keyed same-run objects.")
  }
  triomics_b07_prov_value_sha256(unname(as.list(values)))
}

triomics_b07_clinical_artifact_specs <- function() {
  list(
    clinical_associations = list(path = "B/clinical-associations.json", type = "clinical_association_results"),
    clinical_longitudinal_associations = list(path = "B/clinical-longitudinal-associations.json", type = "clinical_association_results")
  )
}

triomics_b07_clinical_string <- function(value) {
  if (is.null(value)) return(NA_character_)
  values <- as.character(unlist(value, use.names = FALSE))
  if (length(values) != 1L || is.na(values[[1L]]) || !nzchar(values[[1L]])) return(NA_character_)
  values[[1L]]
}

triomics_b07_clinical_integer <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || !is.null(dim(value)) ||
      is.na(value) || !is.finite(value) || value < 0 ||
      value > .Machine$integer.max || value != trunc(value)) return(NA_integer_)
  as.integer(value)
}

triomics_b07_clinical_correction_vector <- function(value, expected_length) {
  if (is.list(value)) {
    if (length(value) != expected_length || any(!vapply(value, function(item) {
      is.numeric(item) && length(item) == 1L && is.null(dim(item)) &&
        !is.na(item) && is.finite(item)
    }, logical(1L)))) return(NULL)
    values <- vapply(value, as.numeric, numeric(1L))
  } else if (is.numeric(value) && is.null(dim(value)) &&
             length(value) == expected_length && all(is.finite(value))) {
    values <- as.numeric(value)
  } else {
    return(NULL)
  }
  if (anyNA(values) || any(!is.finite(values)) || any(values < 0 | values > 1)) return(NULL)
  unname(values)
}

triomics_b07_clinical_values_equal <- function(left, right) {
  if (is.null(left)) left <- list()
  if (is.null(right)) right <- list()
  identical(triomics_b07_prov_canonical_value(left), triomics_b07_prov_canonical_value(right))
}

triomics_b07_clinical_family_consistent <- function(payload, objective) {
  family <- if (is.list(payload$family)) payload$family else list()
  q_published <- family[["q_published", exact = TRUE]]
  q_published_valid <- is.logical(q_published) && length(q_published) == 1L &&
    is.null(dim(q_published)) && !is.na(q_published)
  members <- as.character(unlist(family$member_ids, use.names = FALSE))
  results <- if (is.list(payload$results)) payload$results else list()
  endpoints <- vapply(results, function(result) triomics_b07_clinical_string(result$endpoint_id), character(1L))
  statuses <- vapply(results, function(result) triomics_b07_clinical_string(result$estimation_status), character(1L))
  estimated <- sum(statuses == "estimated")
  expected_internal_p <- rep(1, length(results))
  correction_rows_valid <- TRUE
  for (index in seq_along(results)) {
    result <- results[[index]]
    result_p <- result[["p", exact = TRUE]]
    result_q <- result[["q", exact = TRUE]]
    if (identical(statuses[[index]], "estimated")) {
      if (is.numeric(result_p) && length(result_p) == 1L && is.null(dim(result_p)) &&
          is.finite(result_p) && result_p >= 0 && result_p <= 1) {
        expected_internal_p[[index]] <- as.numeric(result_p)
      } else {
        correction_rows_valid <- FALSE
      }
    } else if (!is.null(result_p) || !is.null(result_q)) {
      correction_rows_valid <- FALSE
    }
  }
  internal_p <- triomics_b07_clinical_correction_vector(family$internal_p, length(members))
  internal_q <- triomics_b07_clinical_correction_vector(family$internal_q, length(members))
  expected_internal_q <- stats::p.adjust(expected_internal_p, method = "BH")
  correction_vectors_valid <- q_published_valid && correction_rows_valid && !is.null(internal_p) &&
    !is.null(internal_q) && identical(triomics_b07_clinical_string(family$correction_method), "BH") &&
    isTRUE(all.equal(internal_p, unname(expected_internal_p), tolerance = 1e-12,
                     check.attributes = FALSE)) &&
    isTRUE(all.equal(internal_q, unname(expected_internal_q), tolerance = 1e-12,
                     check.attributes = FALSE))
  if (q_published_valid) {
    correction_vectors_valid <- correction_vectors_valid && all(vapply(seq_along(results), function(index) {
      result <- results[[index]]
      result_q <- result[["q", exact = TRUE]]
      if (identical(statuses[[index]], "estimated") && isTRUE(q_published)) {
        is.numeric(result_q) && length(result_q) == 1L && is.null(dim(result_q)) &&
          is.finite(result_q) && result_q >= 0 && result_q <= 1 &&
          isTRUE(all.equal(as.numeric(result_q), unname(expected_internal_q[[index]]),
                          tolerance = 1e-12, check.attributes = FALSE))
      } else {
        is.null(result_q)
      }
    }, logical(1L)))
  }
  typed_dispositions <- vapply(seq_along(results), function(index) {
    result <- results[[index]]
    status <- statuses[[index]]
    if (is.na(status)) return(FALSE)
    if (identical(status, "estimated")) {
      is.numeric(result$effect) && length(result$effect) == 1L && is.finite(result$effect) &&
        is.numeric(result$p) && length(result$p) == 1L && is.finite(result$p) &&
        result$p >= 0 && result$p <= 1 &&
        !is.na(triomics_b07_clinical_string(result$effect_unit))
    } else {
      !is.na(triomics_b07_clinical_string(result$estimation_reason))
    }
  }, logical(1L))
  current_endpoint <- triomics_b07_clinical_string(objective$endpoint_ref)
  if (!is.list(family) || !length(members) || anyNA(members) || any(!nzchar(members)) ||
      anyDuplicated(members) || length(results) != length(members) ||
      anyNA(endpoints) || anyDuplicated(endpoints) ||
      !identical(members, endpoints) ||
      !current_endpoint %in% members || anyNA(statuses) || any(!typed_dispositions) ||
      !correction_vectors_valid ||
      any(!statuses %in% c("estimated", "failed", "unavailable")) ||
      !identical(triomics_b07_clinical_string(family$family_id),
                 triomics_b07_clinical_string(objective$family_ref)) ||
      !identical(triomics_b07_clinical_integer(family$n_declared), as.integer(length(members))) ||
      !identical(triomics_b07_clinical_integer(family$n_estimated), as.integer(estimated)) ||
      !identical(triomics_b07_clinical_integer(family$n_failed), as.integer(length(members) - estimated)) ||
      !identical(triomics_b07_clinical_string(family$member_hash), triomics_b_hash(members))) {
    return(FALSE)
  }
  TRUE
}

triomics_b07_clinical_b08_registry_matches <- function(registry, objective) {
  population <- registry$population
  same_string <- function(actual, expected) {
    identical(triomics_b07_clinical_string(actual), triomics_b07_clinical_string(expected))
  }
  same_string(registry$objective_id, objective$objective_id) &&
    same_string(registry$analysis_id, objective$analysis_id) &&
    same_string(registry$mode, objective$mode) &&
    same_string(registry$response_ref, objective$response_ref) &&
    same_string(registry$predictor_ref, objective$predictor_ref) &&
    same_string(registry$endpoint_ref, objective$endpoint_ref) &&
    same_string(registry$time_window_ref, objective$time_window_ref) &&
    same_string(population$visit_id, objective$time_window_ref) &&
    same_string(registry$missingness_policy, objective$missingness_policy) &&
    same_string(population$missingness_policy, objective$missingness_policy) &&
    same_string(registry$family_ref, objective$family_ref) &&
    same_string(registry$family_id, objective$family_ref) &&
    same_string(registry$hypothesis_status, objective$hypothesis_status) &&
    same_string(registry$protocol_ref, objective$protocol_ref) &&
    triomics_b07_clinical_values_equal(registry$adjustments, objective$adjustments)
}

triomics_b07_clinical_b09_registry_matches <- function(registry, result, objective) {
  audit <- result$objective_audit
  if (!is.list(audit) || !triomics_b07_clinical_values_equal(registry, audit)) return(FALSE)
  same_string <- function(actual, expected) {
    identical(triomics_b07_clinical_string(actual), triomics_b07_clinical_string(expected))
  }
  time_window <- audit$time_window
  mode <- triomics_b07_clinical_string(objective$mode)
  expected_predictor_visit <- if (identical(mode, "baseline_to_later")) {
    objective$baseline_visit_ref
  } else {
    NULL
  }
  expected_visits <- if (mode %in% c("within_person", "baseline_to_later")) {
    as.character(c(objective$baseline_visit_ref, objective$later_visit_ref))
  } else if (is.null(objective$time_window_ref)) {
    character()
  } else {
    as.character(objective$time_window_ref)
  }
  actual_visits <- as.character(unlist(time_window$visit_ids, use.names = FALSE))
  identical(audit$objective_id, objective$objective_id) &&
    same_string(audit$mode, objective$mode) &&
    identical(actual_visits, expected_visits) &&
    triomics_b07_clinical_values_equal(time_window$predictor_visit, expected_predictor_visit) &&
    triomics_b07_clinical_values_equal(time_window$baseline_visit, objective$baseline_visit_ref) &&
    triomics_b07_clinical_values_equal(time_window$later_visit, objective$later_visit_ref) &&
    triomics_b07_clinical_values_equal(time_window$declared_horizon_days, objective$horizon_days) &&
    triomics_b07_clinical_values_equal(time_window$baseline_adjustment, objective$baseline_adjustment) &&
    same_string(audit$missingness$policy, objective$missingness_policy)
}

triomics_b07_clinical_result_matches <- function(result, objective, lane) {
  same_string <- function(actual, expected) {
    identical(triomics_b07_clinical_string(actual), triomics_b07_clinical_string(expected))
  }
  if (!same_string(result$objective_id, objective$objective_id) ||
      !same_string(result$analysis_id, objective$analysis_id) ||
      !same_string(result$endpoint_id, objective$endpoint_ref) ||
      !same_string(result$predictor_ref, objective$predictor_ref) ||
      !same_string(result$mode, objective$mode) ||
      !same_string(result$hypothesis_status, objective$hypothesis_status) ||
      !triomics_b07_clinical_values_equal(result$adjustment, objective$adjustments) ||
      !triomics_b07_clinical_string(result$estimation_status) %in% c("estimated", "failed", "unavailable") ||
      !identical(triomics_b07_clinical_string(objective$response_ref),
                 triomics_b07_clinical_string(objective$endpoint_ref))) return(FALSE)
  if (identical(lane, "clinical_associations")) {
    return(same_string(result$missingness_policy, objective$missingness_policy))
  }
  audit <- result$objective_audit
  is.list(audit) &&
    same_string(audit$missingness$policy, objective$missingness_policy) &&
    triomics_b07_clinical_b09_registry_matches(audit, result, objective)
}

triomics_b07_clinical_output_accounting <- function(config, analysis_id, clinical_artifacts) {
  clinical <- if (is.list(config$clinical)) config$clinical else list()
  enabled <- isTRUE(clinical$enabled)
  declared <- if (is.list(clinical$objectives)) clinical$objectives else list()
  current <- Filter(function(objective) {
    identical(triomics_b07_clinical_string(objective$analysis_id),
              triomics_b07_clinical_string(analysis_id))
  }, declared)
  objective_ids <- vapply(current, function(objective) {
    triomics_b07_clinical_string(objective$objective_id)
  }, character(1L))
  if (anyNA(objective_ids) || anyDuplicated(objective_ids)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "Current-analysis clinical objective ids are missing or duplicated.",
                       "clinical.objectives", "Keep one stable objective id per declared route in the current analysis.")
  }
  by_objective <- stats::setNames(vector("list", length(current)), objective_ids)
  claims_by_objective <- stats::setNames(vector("list", length(current)), objective_ids)
  for (objective in current) {
    route_id <- triomics_b07_clinical_string(objective$objective_id)
    claims <- list()
    for (lane in names(triomics_b07_clinical_artifact_specs())) {
      item <- clinical_artifacts[[lane]]
      if (is.null(item)) next
      payload <- item$payload
      registry <- if (is.list(payload$objectives)) payload$objectives else list()
      results <- if (is.list(payload$results)) payload$results else list()
      registry_hits <- Filter(function(entry) {
        identical(triomics_b07_clinical_string(entry$objective_id), route_id)
      }, registry)
      result_hits <- Filter(function(result) {
        result_id <- result$objective_id
        if (is.null(result_id) && is.list(result$objective_audit)) result_id <- result$objective_audit$objective_id
        identical(triomics_b07_clinical_string(result_id), route_id)
      }, results)
      if (!length(registry_hits) && !length(result_hits)) next
      evidence <- list(
        artifact_id = as.character(item$envelope$artifact_id),
        payload_sha256 = as.character(item$payload_sha256),
        envelope_sha256 = as.character(item$envelope_sha256),
        status = as.character(item$status),
        result_sha256 = if (length(result_hits)) triomics_b07_prov_value_sha256(result_hits) else NULL
      )
      claims[[lane]] <- list(
        registry = registry_hits, results = result_hits, item = item, evidence = evidence
      )
    }
    claims_by_objective[[route_id]] <- claims
    accounted <- FALSE
    matched_status <- NULL
    if (enabled && length(claims) == 1L) {
      lane <- names(claims)[[1L]]
      claim <- claims[[1L]]
      payload <- claim$item$payload
      result <- if (length(claim$results) == 1L) claim$results[[1L]] else NULL
      registry <- if (length(claim$registry) == 1L) claim$registry[[1L]] else NULL
      family_ok <- triomics_b07_clinical_family_consistent(payload, objective)
      metadata_ok <- identical(triomics_b07_clinical_string(payload$analysis_id),
                               triomics_b07_clinical_string(objective$analysis_id)) &&
        is.list(result) && triomics_b07_clinical_result_matches(result, objective, lane) &&
        family_ok
      if (identical(lane, "clinical_associations")) {
        metadata_ok <- metadata_ok && is.list(registry) &&
          triomics_b07_clinical_b08_registry_matches(registry, objective)
        if (metadata_ok) {
          fitted <- identical(triomics_b07_clinical_string(result$estimation_status), "estimated")
          expected_registry_status <- if (fitted) "fitted" else "unavailable"
          metadata_ok <- identical(isTRUE(registry$fitted), fitted) &&
            identical(triomics_b07_clinical_string(registry$status), expected_registry_status) &&
            triomics_b07_clinical_values_equal(registry$reason, result$estimation_reason)
        }
      } else {
        metadata_ok <- metadata_ok && is.list(registry) &&
          triomics_b07_clinical_b09_registry_matches(registry, result, objective)
      }
      accounted <- isTRUE(metadata_ok)
      if (accounted) matched_status <- as.character(claim$item$status)
    }
    by_objective[[route_id]] <- list(accounted = accounted, status = matched_status)
  }
  all_accounted <- !enabled || !length(current) || all(vapply(by_objective, function(item) {
    isTRUE(item$accounted)
  }, logical(1L)))
  dispositions <- list()
  for (lane in names(triomics_b07_clinical_artifact_specs())) {
    if (lane %in% names(clinical_artifacts)) {
      item <- clinical_artifacts[[lane]]
      dispositions[[lane]] <- list(status = item$status, reason = NULL)
    } else if (!enabled) {
      dispositions[[lane]] <- list(status = "not_requested", reason = "clinical_disabled")
    } else if (!length(current)) {
      dispositions[[lane]] <- list(status = "not_requested", reason = "no_declared_objectives_for_analysis")
    } else if (all_accounted) {
      dispositions[[lane]] <- list(status = "not_requested", reason = "objectives_accounted_for_by_present_artifacts")
    } else {
      dispositions[[lane]] <- list(status = "unavailable", reason = "requested_artifact_missing")
    }
  }
  list(enabled = enabled, objectives = current, by_objective = by_objective,
       claims_by_objective = claims_by_objective, all_accounted = all_accounted,
       dispositions = dispositions)
}

triomics_b07_clinical_route_analysis <- function(config, objective, current_analysis) {
  route_analysis_id <- triomics_b07_clinical_string(objective$analysis_id)
  if (identical(route_analysis_id, current_analysis$analysis_id)) return(current_analysis)
  analysis_specs <- if (is.list(config$analyses)) config$analyses else list()
  hits <- Filter(function(analysis) {
    identical(triomics_b07_clinical_string(analysis$analysis_id), route_analysis_id)
  }, analysis_specs)
  if (length(hits) != 1L) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "A clinical route for another analysis has no unique declared analysis contract.",
                       route_analysis_id, "Keep each clinical route bound to one validated analysis declaration.")
  }
  hits[[1L]]
}

triomics_b07_prov_entity_id <- function(role, scope_key) {
  paste0(role, "-", substr(triomics_b07_prov_value_sha256(scope_key), 1L, 20L))
}

triomics_b07_prov_scope <- function(study_id, analysis_id = NULL, block_id = NULL,
                                    method_id = NULL, preparation_context_id = NULL,
                                    representation_id = NULL, route_id = NULL) {
  list(
    study_id = as.character(study_id),
    analysis_id = if (is.null(analysis_id)) NULL else as.character(analysis_id),
    block_id = if (is.null(block_id)) NULL else as.character(block_id),
    method_id = if (is.null(method_id)) NULL else as.character(method_id),
    preparation_context_id = if (is.null(preparation_context_id)) NULL else as.character(preparation_context_id),
    representation_id = if (is.null(representation_id)) NULL else as.character(representation_id),
    route_id = if (is.null(route_id)) NULL else as.character(route_id)
  )
}

triomics_b07_prov_entity <- function(role, entity_id, study_id, digest,
                                    scope = list(), storage_form = "embedded_json",
                                    media_type = "application/json", size_bytes = NULL,
                                    canonical_schema_uri = "urn:triomics:b-t07:embedded-identity:1.0",
                                    artifact_id = NULL, artifact_payload_sha256 = NULL,
                                    generated_by_activity_id = NULL,
                                    derived_from_entity_ids = character(), member_of_entity_id = NULL,
                                    row_identity_sha256 = NULL, column_identity_sha256 = NULL) {
  types <- c(
    source_manifest = "triomics.source_manifest",
    normalized_metadata = "triomics.normalized_metadata",
    feature_registry = "triomics.feature_registry",
    analysis_unit_map = "triomics.analysis_unit_map",
    resolved_analysis = "triomics.resolved_analysis",
    represented_matrix = "triomics.represented_matrix",
    preprocessing_state = "triomics.preprocessing_state",
    method_state = "triomics.method_state",
    predecessor_envelope = "triomics.predecessor_envelope",
    predecessor_payload = "triomics.predecessor_payload",
    block_results = "triomics.block_results",
    family_ledger = "triomics.family_ledger",
    candidate_nominations = "triomics.candidate_nominations",
    clinical_evidence = "triomics.clinical_evidence",
    handoff_payload = "triomics.handoff_payload"
  )
  if (!role %in% names(types)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The B provenance producer received an unsupported entity role.",
                       "provenance", "Keep the B-T07 identity within the accepted M7-003 role contract.")
  }
  c(list(
    entity_id = as.character(entity_id),
    entity_type = unname(types[[role]]),
    scientific_role = role,
    schema_version = "1.0",
    storage_form = storage_form,
    media_type = media_type,
    digest_algorithm = "sha256",
    digest_kind = if (identical(storage_form, "embedded_json")) "rfc8785" else "bytes",
    digest = as.character(digest),
    size_bytes = if (identical(storage_form, "embedded_json")) NULL else as.numeric(size_bytes),
    canonical_schema_uri = if (identical(storage_form, "embedded_json")) canonical_schema_uri else NULL,
    artifact_id = if (is.null(artifact_id)) NULL else as.character(artifact_id),
    artifact_payload_sha256 = if (is.null(artifact_payload_sha256)) NULL else as.character(artifact_payload_sha256),
    generated_by_activity_id = if (is.null(generated_by_activity_id)) NULL else as.character(generated_by_activity_id),
    derived_from_entity_ids = as.list(sort(unique(as.character(derived_from_entity_ids)), method = "radix")),
    member_of_entity_id = if (is.null(member_of_entity_id)) NULL else as.character(member_of_entity_id)
  ), scope, list(
    row_identity_sha256 = if (is.null(row_identity_sha256)) NULL else as.character(row_identity_sha256),
    column_identity_sha256 = if (is.null(column_identity_sha256)) NULL else as.character(column_identity_sha256)
  ))
}

triomics_b07_prov_embedded_entity <- function(role, entity_id, value, study_id, scope = list(),
                                              generated_by_activity_id, derived_from_entity_ids = character(),
                                              row_identity_sha256 = NULL, column_identity_sha256 = NULL,
                                              schema_uri = "urn:triomics:b-t07:embedded-identity:1.0") {
  triomics_b07_prov_entity(
    role = role, entity_id = entity_id, study_id = study_id,
    digest = triomics_b07_prov_value_sha256(value), scope = scope,
    generated_by_activity_id = generated_by_activity_id,
    derived_from_entity_ids = derived_from_entity_ids,
    row_identity_sha256 = row_identity_sha256,
    column_identity_sha256 = column_identity_sha256,
    canonical_schema_uri = schema_uri
  )
}

triomics_b07_prov_artifact_entity <- function(role, entity_id, payload_path, envelope_path,
                                              study_id, scope = list(), generated_by_activity_id,
                                              derived_from_entity_ids = character(),
                                              storage_form = "enveloped_artifact", member_of_entity_id = NULL,
                                              row_identity_sha256 = NULL, column_identity_sha256 = NULL) {
  if (!file.exists(payload_path) || !file.exists(envelope_path) ||
      !isTRUE(triomics_validate_envelope(envelope_path))) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "A required B provenance artifact lacks verified persisted bytes.",
                       role, "Recreate the artifact and ordinary envelope from their accepted producer.")
  }
  envelope <- triomics_b07_read_json(envelope_path, paste0(role, ".envelope"))
  payload_sha256 <- triomics_sha256(payload_path)
  if (!identical(as.character(envelope$payload_sha256), payload_sha256)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "A persisted B provenance artifact differs from its envelope.",
                       role, "Recreate the payload and envelope together from the accepted producer.")
  }
  is_envelope <- identical(storage_form, "enveloped_artifact")
  path <- if (is_envelope) envelope_path else payload_path
  if (identical(storage_form, "file")) {
    return(triomics_b07_prov_entity(
      role = role, entity_id = entity_id, study_id = study_id,
      digest = payload_sha256, scope = scope,
      storage_form = "file", media_type = "application/json",
      size_bytes = file.info(payload_path)$size,
      generated_by_activity_id = generated_by_activity_id,
      derived_from_entity_ids = derived_from_entity_ids,
      member_of_entity_id = member_of_entity_id,
      row_identity_sha256 = row_identity_sha256,
      column_identity_sha256 = column_identity_sha256
    ))
  }
  triomics_b07_prov_entity(
    role = role, entity_id = entity_id, study_id = study_id,
    digest = triomics_sha256(path), scope = scope,
    storage_form = storage_form,
    media_type = if (grepl("\\.rds$", payload_path)) "application/x-r-serialized" else "application/json",
    size_bytes = file.info(path)$size,
    artifact_id = as.character(envelope$artifact_id),
    artifact_payload_sha256 = payload_sha256,
    generated_by_activity_id = generated_by_activity_id,
    derived_from_entity_ids = derived_from_entity_ids,
    member_of_entity_id = member_of_entity_id,
    row_identity_sha256 = row_identity_sha256,
    column_identity_sha256 = column_identity_sha256
  )
}

triomics_b07_validate_provenance_context <- function(context, config_sha256, artifact,
                                                     matrices = NULL) {
  required <- c("config", "analysis_spec", "source_manifests", "source_files",
                "sample_metadata", "sample_registry", "declared_factor_levels",
                "feature_registry", "analysis_unit_map", "resolved_analysis",
                "statistics_spec", "method_state", "block_results",
                "block_results_path", "block_results_envelope_path",
                "block_results_sha256", "block_results_envelope_sha256", "run_id",
                "producer_method_ids")
  if (!is.list(context) || !all(required %in% names(context)) ||
      !is.list(context$analysis_spec) || !is.list(context$resolved_analysis) ||
      !length(context$source_files) || !length(context$producer_method_ids)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The Stage B runner did not retain its complete same-run provenance context.",
                       "provenance_context", "Pass only the context returned by the successful Stage B fit.")
  }
  if (identical(as.character(context$block_results$study_id), "triomics_b_t04_fixture")) {
    triomics_b07_validate_b04_synthetic_context(context, config_sha256, artifact)
  }
  if (!identical(as.character(attr(context$config, "config_sha256")), as.character(config_sha256)) ||
      !identical(as.character(context$block_results_sha256), as.character(artifact$payload_sha256)) ||
      !identical(as.character(context$block_results_envelope_sha256), as.character(artifact$envelope_sha256)) ||
      !identical(as.character(context$run_id), as.character(artifact$envelope$run_id)) ||
      !identical(normalizePath(context$block_results_path, winslash = "/", mustWork = TRUE), artifact$payload_path) ||
      !identical(normalizePath(context$block_results_envelope_path, winslash = "/", mustWork = TRUE), artifact$envelope_path)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The retained Stage B context conflicts with persisted configuration or predecessor bytes.",
                       "provenance_context", "Use the exact Stage B payload and envelope from the same fit run.")
  }
  for (source in context$source_files) {
    if (!is.list(source) || !triomics_b_string(as.character(source$local_ref)) ||
        !file.exists(source$local_ref) || dir.exists(source$local_ref) ||
        triomics_is_symlink(source$local_ref) ||
        !identical(triomics_sha256(source$local_ref), as.character(source$sha256))) {
      triomics_b07_abort("B_EXPORT_SOURCE_CHANGED", "A declared Stage B source changed after the fit and before handoff export.",
                         "source_manifest", "Restore the exact same-run source bytes or rerun Stage B before exporting.")
    }
  }
  if (!is.null(matrices)) {
    expected <- stats::setNames(lapply(names(context$resolved_analysis$blocks), function(block_id) {
      context$resolved_analysis$blocks[[block_id]]$assay
    }), names(context$resolved_analysis$blocks))
    if (!identical(matrices, expected)) {
      triomics_b07_abort("B_EXPORT_MATRIX", "The matrix handoff differs from the exact resolved B fit object.",
                         "matrices", "Pass the unchanged resolved$blocks[[block_id]]$assay values from this fit run.")
    }
  }
  if (!identical(triomics_b07_prov_value_sha256(context$block_results),
                 triomics_b07_prov_value_sha256(artifact$payload))) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The in-memory Stage B result context differs from its persisted predecessor payload.",
                       "block_results", "Regenerate the handoff using the same unmodified Stage B return object.")
  }
  invisible(TRUE)
}

triomics_b07_prov_safe_manifest <- function(source_file, manifest = NULL) {
  value <- list(
    source_id = as.character(source_file$source_id),
    study_id = if (is.null(manifest$study_id)) NULL else as.character(manifest$study_id),
    role = as.character(source_file$role),
    format = as.character(source_file$format),
    source_bytes_sha256 = as.character(source_file$sha256),
    selection_history = as.character(source_file$selection_history),
    selection_evidence_sha256 = as.character(source_file$selection_evidence_sha256),
    conversion_manifest_sha256 = NULL
  )
  if (!is.null(manifest)) {
    conversion <- manifest$conversion_evidence
    value$manifest = list(
      source_id = as.character(manifest$source_id),
      study_id = as.character(manifest$study_id),
      role = as.character(manifest$role),
      format = as.character(manifest$format),
      source_sha256 = as.character(manifest$sha256),
      original_sha256 = if (is.null(manifest$original_sha256)) NULL else as.character(manifest$original_sha256),
      original_ref_sha256 = if (is.null(manifest$original_ref)) NULL else triomics_hash_text(as.character(manifest$original_ref)),
      sheet = if (is.null(manifest$sheet)) NULL else as.character(manifest$sheet),
      region = if (is.null(manifest$region)) NULL else manifest$region,
      header_role = if (is.null(manifest$header_role)) NULL else as.character(manifest$header_role),
      column_roles_sha256 = if (is.null(manifest$column_roles)) NULL else triomics_b07_prov_value_sha256(manifest$column_roles),
      conversion_evidence = list(
        mode = if (is.null(conversion$mode)) NULL else as.character(conversion$mode),
        original_sha256 = if (is.null(conversion$original_sha256)) NULL else as.character(conversion$original_sha256),
        canonical_sha256 = if (is.null(conversion$canonical_sha256)) NULL else as.character(conversion$canonical_sha256),
        converter = if (is.null(conversion$converter)) NULL else as.character(conversion$converter),
        converter_version = if (is.null(conversion$converter_version)) NULL else as.character(conversion$converter_version),
        sheet = if (is.null(conversion$sheet)) NULL else as.character(conversion$sheet),
        region = if (is.null(conversion$region)) NULL else conversion$region,
        formula_derived = if (is.null(conversion$formula_derived)) FALSE else isTRUE(conversion$formula_derived),
        selection_evidence_sha256 = if (is.null(manifest$selection_evidence_ref)) NULL else
          triomics_hash_text(as.character(manifest$selection_evidence_ref))
      )
    )
  }
  value
}

triomics_b07_prov_result_rows <- function(block_results) {
  entries <- triomics_b07_claim_entries(block_results)
  rows <- list()
  for (entry in entries) {
    claim <- block_results$blocks[[entry$block_id]]$contrasts[[entry$claim_id]]
    if (is.null(claim)) claim <- block_results$blocks[[entry$block_id]]$joint_claims[[entry$claim_id]]
    for (row in entry$rows) {
      rows[[length(rows) + 1L]] <- list(
        block_id = as.character(entry$block_id),
        claim_id = as.character(entry$claim_id),
        claim_type = as.character(entry$claim_type),
        contrast_id = if (is.null(row$contrast_id)) NULL else as.character(row$contrast_id),
        feature_key = if (is.null(row$feature_key)) NULL else as.character(row$feature_key),
        result_id = if (is.null(row$result_id)) NULL else as.character(row$result_id),
        model_hash = if (is.null(row$model_hash)) NULL else as.character(row$model_hash),
        representation_hash = if (is.null(row$representation_hash)) NULL else as.character(row$representation_hash),
        source_hash = if (is.null(row$source_hash)) NULL else as.character(row$source_hash),
        count_prior_status = if (!is.null(row$count_prior_status)) as.character(row$count_prior_status) else
          if (is.null(claim$count_prior_status)) NULL else as.character(claim$count_prior_status),
        count_prior_ref = if (is.null(row$count_prior_ref)) NULL else triomics_b07_prov_value_sha256(row$count_prior_ref)
      )
    }
  }
  rows[order(vapply(rows, function(row) paste(row$block_id, row$claim_id, row$feature_key, sep = "\034"), character(1L)),
             method = "radix")]
}

triomics_b07_prov_runtime_versions <- function() {
  components <- c("DEqMS", "R", "V8", "digest", "jsonlite", "limma", "triomics")
  lapply(components, function(component) {
    version <- if (identical(component, "R")) {
      paste(R.version$major, R.version$minor, sep = ".")
    } else {
      tryCatch(as.character(utils::packageVersion(component)), error = function(error) "not-installed")
    }
    list(component = component, version = version)
  })
}

triomics_b07_validate_b04_synthetic_context <- function(context, config_sha256, artifact) {
  incomplete <- function(key, message = "The same-run B-T04 synthetic lineage is incomplete or inconsistent.") {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", message, key,
      "Rerun the native B-T04 producer and preserve its protocol, generated records, fit identities, and predecessor bytes."
    )
  }
  synthetic <- context$synthetic_producer
  required <- c(
    "protocol", "raw_response", "centered_response", "ordered_design",
    "subject_time_identity", "centering_rule", "method_state", "calibration",
    "lineage_identity_sha256"
  )
  if (!is.list(synthetic) || !all(required %in% names(synthetic))) {
    incomplete("synthetic_producer", "The B-T04 producer omitted a required synthetic lineage identity.")
  }

  expected_protocol <- triomics_b04_synthetic_protocol(config_sha256)
  expected_protocol_bytes <- triomics_b07_prov_canonical_value(expected_protocol)
  protocol <- synthetic$protocol
  expected_protocol_path <- file.path(dirname(artifact$payload_path), "b-t04-synthetic-protocol.json")
  if (!is.list(protocol) || !all(c("path", "bytes", "sha256", "value") %in% names(protocol)) ||
      !file.exists(expected_protocol_path) || dir.exists(expected_protocol_path) ||
      triomics_is_symlink(expected_protocol_path) ||
      !identical(as.character(protocol$path), normalizePath(expected_protocol_path, winslash = "/", mustWork = TRUE)) ||
      !identical(as.character(protocol$bytes), expected_protocol_bytes) ||
      !identical(triomics_b07_prov_canonical_value(protocol$value), expected_protocol_bytes)) {
    incomplete("synthetic_protocol", "The canonical B-T04 protocol bytes or identity do not match the approved synthetic contract.")
  }
  actual_protocol_bytes <- readBin(expected_protocol_path, what = "raw", n = file.info(expected_protocol_path)$size)
  expected_protocol_raw <- charToRaw(enc2utf8(expected_protocol_bytes))
  protocol_sha256 <- triomics_sha256(expected_protocol_path)
  if (!identical(actual_protocol_bytes, expected_protocol_raw) ||
      !identical(as.character(protocol$sha256), protocol_sha256) ||
      !identical(protocol_sha256, digest::digest(expected_protocol_raw, algo = "sha256", serialize = FALSE))) {
    incomplete("synthetic_protocol", "The persisted B-T04 protocol is not the retained immutable canonical byte sequence.")
  }
  protocol_id <- as.character(expected_protocol$protocol_id)
  expected_source_file <- list(
    source_id = protocol_id,
    role = "synthetic_protocol",
    format = "application/json",
    local_ref = normalizePath(expected_protocol_path, winslash = "/", mustWork = TRUE),
    sha256 = protocol_sha256,
    selection_history = "full_unselected",
    selection_evidence_sha256 = triomics_hash_text(
      "B-T04 synthetic protocol is the sole response-generating source."
    )
  )
  expected_source_manifest <- list(
    source_id = protocol_id,
    study_id = as.character(expected_protocol$study_id),
    role = "synthetic_protocol",
    format = "application/json",
    sha256 = protocol_sha256,
    original_sha256 = NULL,
    original_ref = NULL,
    sheet = NULL,
    region = NULL,
    header_role = NULL,
    column_roles = NULL,
    conversion_evidence = NULL
  )
  if (length(context$source_files) != 1L || length(context$source_manifests) != 1L ||
      !identical(context$source_files[[1L]], expected_source_file) ||
      !identical(context$source_manifests[[1L]], expected_source_manifest) ||
      length(artifact$envelope$inputs) != 1L ||
      !identical(as.character(artifact$envelope$inputs[[1L]]$artifact_id), protocol_id) ||
      !identical(as.character(artifact$envelope$inputs[[1L]]$sha256), protocol_sha256)) {
    incomplete("source_manifest", "B-T04 response ancestry must have the protocol as its only source root.")
  }
  normalized_or_null <- function(path) {
    tryCatch(normalizePath(path, winslash = "/", mustWork = TRUE), error = function(error) NULL)
  }
  if (!identical(normalized_or_null(context$block_results_path), artifact$payload_path) ||
      !identical(normalized_or_null(context$block_results_envelope_path), artifact$envelope_path) ||
      !identical(as.character(context$block_results_sha256), as.character(artifact$payload_sha256)) ||
      !identical(as.character(context$block_results_envelope_sha256), as.character(artifact$envelope_sha256)) ||
      !identical(as.character(context$run_id), as.character(artifact$envelope$run_id))) {
    incomplete("block_results", "The B-T04 same-run predecessor payload or envelope identity changed.")
  }

  subjects <- unlist(expected_protocol$ordered_subjects, use.names = FALSE)
  times <- as.numeric(unlist(expected_protocol$ordered_times$values, use.names = FALSE))
  visits_by_time <- unlist(expected_protocol$ordered_times$visit_labels, use.names = FALSE)
  features <- unlist(expected_protocol$feature_ids, use.names = FALSE)
  sample_ids <- unlist(lapply(subjects, function(subject) paste0(subject, "_", visits_by_time)), use.names = FALSE)
  sample_subjects <- rep(subjects, each = length(times))
  sample_times <- rep(times, times = length(subjects))
  sample_visits <- rep(visits_by_time, times = length(subjects))
  if (!is.data.frame(context$sample_metadata) ||
      !identical(names(context$sample_metadata), c("sample_id", "subject", "visit", "time", "time_unit", "time_origin")) ||
      nrow(context$sample_metadata) != 12L ||
      !identical(as.character(context$sample_metadata$sample_id), sample_ids) ||
      !identical(as.character(context$sample_metadata$subject), sample_subjects) ||
      !identical(as.character(context$sample_metadata$visit), sample_visits) ||
      !identical(as.numeric(context$sample_metadata$time), sample_times) ||
      !identical(as.character(context$sample_metadata$time_unit), rep("synthetic_step", 12L)) ||
      !identical(as.numeric(context$sample_metadata$time_origin), rep(0, 12L))) {
    incomplete("normalized_metadata", "The ordered generated metadata do not match the B-T04 protocol.")
  }
  sample_registry <- context$sample_registry
  sample_registry_columns <- c(
    "study_id", "sample_id", "subject_id", "visit_id", "time", "time_unit", "time_origin",
    "source_id", "source_row", "sample_role"
  )
  if (!is.data.frame(sample_registry) || !identical(names(sample_registry), sample_registry_columns) ||
      nrow(sample_registry) != 12L ||
      !identical(as.character(sample_registry$study_id), rep(as.character(expected_protocol$study_id), 12L)) ||
      !identical(as.character(sample_registry$sample_id), sample_ids) ||
      !identical(as.character(sample_registry$subject_id), sample_subjects) ||
      !identical(as.character(sample_registry$visit_id), sample_visits) ||
      !identical(as.numeric(sample_registry$time), sample_times) ||
      !identical(as.character(sample_registry$time_unit), rep("synthetic_step", 12L)) ||
      !identical(as.numeric(sample_registry$time_origin), rep(0, 12L)) ||
      !identical(as.character(sample_registry$source_id), rep(protocol_id, 12L)) ||
      !identical(as.character(sample_registry$source_row), as.character(seq_len(12L))) ||
      !identical(as.character(sample_registry$sample_role), rep("synthetic_generated", 12L))) {
    incomplete("sample_registry", "The generated SampleRegistry does not cover the exact twelve protocol units.")
  }
  feature_registry <- context$feature_registry
  feature_registry_columns <- c(
    "study_id", "block_id", "feature_key", "canonical_id", "original_id", "id_namespace",
    "source_id", "source_row", "measured_or_derived", "quantity", "scale", "units",
    "structural_resolution", "alias_refs", "parent_refs", "annotation_route"
  )
  if (!is.data.frame(feature_registry) || !identical(names(feature_registry), feature_registry_columns) ||
      nrow(feature_registry) != 2L ||
      !identical(as.character(feature_registry$study_id), rep(as.character(expected_protocol$study_id), 2L)) ||
      !identical(as.character(feature_registry$block_id), rep("block_1", 2L)) ||
      !identical(as.character(feature_registry$feature_key), features) ||
      !identical(as.character(feature_registry$canonical_id), features) ||
      !identical(as.character(feature_registry$original_id), features) ||
      !identical(as.character(feature_registry$id_namespace), rep("synthetic", 2L)) ||
      !identical(as.character(feature_registry$source_id), rep(protocol_id, 2L)) ||
      !identical(as.character(feature_registry$source_row), as.character(seq_len(2L))) ||
      !identical(as.character(feature_registry$measured_or_derived), rep("derived", 2L)) ||
      !identical(as.character(feature_registry$quantity), rep("intensity", 2L)) ||
      !identical(as.character(feature_registry$scale), rep("linear", 2L)) ||
      !identical(as.character(feature_registry$units), rep("synthetic_arbitrary_units", 2L)) ||
      !identical(as.character(feature_registry$structural_resolution), rep("unknown", 2L)) ||
      !identical(as.character(feature_registry$alias_refs), rep("", 2L)) ||
      !identical(as.character(feature_registry$parent_refs), rep("", 2L)) ||
      !identical(as.character(feature_registry$annotation_route), rep("none", 2L))) {
    incomplete("feature_registry", "The generated FeatureRegistry is incomplete or differs from the protocol features.")
  }

  unit_map <- context$analysis_unit_map
  unit_map_columns <- c(
    "study_id", "analysis_id", "preparation_context_id", "unit_id", "unit_type",
    "representation_id", "source_sample_ids", "subject_id", "visit_id", "condition",
    "time", "time_unit", "time_origin", "eligible", "exclusion_reason", "represented_blocks"
  )
  unit_lists_valid <- is.data.frame(unit_map) &&
    is.list(unit_map$source_sample_ids) && length(unit_map$source_sample_ids) == 12L &&
    is.list(unit_map$represented_blocks) && length(unit_map$represented_blocks) == 12L
  if (!unit_lists_valid || !identical(names(unit_map), unit_map_columns) || nrow(unit_map) != 12L ||
      !identical(as.character(unit_map$study_id), rep(as.character(expected_protocol$study_id), 12L)) ||
      !identical(as.character(unit_map$analysis_id), rep(as.character(context$analysis_spec$analysis_id), 12L)) ||
      !identical(as.character(unit_map$preparation_context_id), rep(as.character(context$analysis_spec$preparation_context_id), 12L)) ||
      !identical(as.character(unit_map$unit_id), sample_ids) ||
      !identical(as.character(unit_map$unit_type), rep("sample", 12L)) ||
      !identical(as.character(unit_map$representation_id), rep("subject_centered", 12L)) ||
      !identical(as.character(unit_map$subject_id), sample_subjects) ||
      !identical(as.character(unit_map$visit_id), sample_visits) ||
      !identical(as.character(unit_map$condition), as.character(sample_times)) ||
      !identical(as.numeric(unit_map$time), sample_times) ||
      !identical(as.character(unit_map$time_unit), rep("synthetic_step", 12L)) ||
      !identical(as.numeric(unit_map$time_origin), rep(0, 12L)) ||
      !all(unit_map$eligible) || !all(is.na(unit_map$exclusion_reason)) ||
      !all(vapply(seq_len(12L), function(index) {
        identical(as.character(unit_map$source_sample_ids[[index]]), sample_ids[[index]]) &&
          identical(as.character(unit_map$represented_blocks[[index]]), "block_1")
      }, logical(1L)))) {
    incomplete("analysis_unit_map", "The generated AnalysisUnitMap does not bind all ordered subject-centered units.")
  }

  intercepts <- as.numeric(unlist(expected_protocol$subject_intercepts[subjects], use.names = FALSE))
  slopes <- as.numeric(unlist(expected_protocol$feature_slopes[features], use.names = FALSE))
  names(slopes) <- features
  residual <- rep(as.numeric(unlist(expected_protocol$residual_pattern$values, use.names = FALSE)),
                  times = length(subjects))
  raw_expected <- do.call(rbind, lapply(features, function(feature) {
    rep(intercepts, each = length(times)) + slopes[[feature]] * sample_times + residual
  }))
  rownames(raw_expected) <- features
  colnames(raw_expected) <- sample_ids
  design_expected <- cbind(
    `subject.sub0001` = as.numeric(sample_subjects == "sub0001"),
    `subject.sub0002` = as.numeric(sample_subjects == "sub0002"),
    `subject.sub0003` = as.numeric(sample_subjects == "sub0003"),
    `subject.sub0004` = as.numeric(sample_subjects == "sub0004"),
    `time.linear` = sample_times
  )
  rownames(design_expected) <- sample_ids
  analysis <- context$analysis_spec
  resolved <- context$resolved_analysis
  if (!identical(analysis, context$config$analyses[[1L]]) ||
      !identical(context$statistics_spec, analysis$statistics) ||
      !identical(as.character(resolved$study_id), as.character(expected_protocol$study_id)) ||
      !identical(as.character(resolved$analysis_id), as.character(analysis$analysis_id)) ||
      !identical(as.character(resolved$preparation_context_id), as.character(analysis$preparation_context_id)) ||
      !identical(as.character(resolved$representation_id), "subject_centered") ||
      !identical(names(resolved$blocks), "block_1") ||
      !identical(resolved$blocks$block_1$assay, raw_expected) ||
      !identical(as.character(resolved$blocks$block_1$sample_ids), sample_ids) ||
      !identical(as.character(resolved$blocks$block_1$subject_ids), sample_subjects) ||
      !identical(as.character(resolved$blocks$block_1$feature_keys), features) ||
      !identical(as.character(resolved$blocks$block_1$scales), rep("linear", 2L)) ||
      !identical(resolved$blocks$block_1$design, design_expected) ||
      !identical(resolved$unit_map, unit_map) ||
      !identical(as.character(resolved$model_plan$column_ids),
                 c("subject.sub0001", "subject.sub0002", "subject.sub0003", "subject.sub0004", "time.linear")) ||
      !identical(as.character(resolved$model_plan$formula), "~0 + subject + time.linear") ||
      !isTRUE(resolved$model_plan$subject_terms_retained) ||
      !identical(resolved$model_plan$duplicate_correlation_applied, FALSE) ||
      !identical(as.character(resolved$model_plan$time_unit), "synthetic_step") ||
      !identical(as.character(resolved$model_plan$time_origin), "0")) {
    incomplete("resolved_analysis", "The resolved B-T04 design or matrix does not match the ordered protocol context.")
  }
  centered_expected <- raw_expected
  for (feature_index in seq_len(nrow(raw_expected))) {
    centered_expected[feature_index, ] <- triomics_b04_center_vector(
      as.numeric(raw_expected[feature_index, ]), sample_subjects
    )
  }
  raw_response <- synthetic$raw_response
  centered_response <- synthetic$centered_response
  raw_identity <- list(
    block_id = "block_1", feature_ids = as.list(features), sample_ids = as.list(sample_ids),
    values = raw_expected
  )
  centered_identity <- list(
    block_id = "block_1", feature_ids = as.list(features), sample_ids = as.list(sample_ids),
    values = centered_expected
  )
  if (!is.list(raw_response) || !identical(raw_response$matrix, raw_expected) ||
      !identical(as.character(raw_response$assay_sha256), triomics_b_assay_hash("block_1", raw_expected)) ||
      !identical(as.character(raw_response$identity_sha256), triomics_b07_prov_value_sha256(raw_identity)) ||
      !identical(as.character(raw_response$row_identity_sha256), triomics_b07_prov_ordered_ids_sha256(features)) ||
      !identical(as.character(raw_response$column_identity_sha256), triomics_b07_prov_ordered_ids_sha256(sample_ids)) ||
      !is.list(centered_response) || !identical(centered_response$matrix, centered_expected) ||
      !identical(as.character(centered_response$assay_sha256), triomics_b_assay_hash("block_1", centered_expected)) ||
      !identical(as.character(centered_response$identity_sha256), triomics_b07_prov_value_sha256(centered_identity)) ||
      !identical(as.character(centered_response$row_identity_sha256), triomics_b07_prov_ordered_ids_sha256(features)) ||
      !identical(as.character(centered_response$column_identity_sha256), triomics_b07_prov_ordered_ids_sha256(sample_ids))) {
    incomplete("synthetic_response", "The uncentered handoff matrix or centered fitted-response identity changed.")
  }
  design_identity <- synthetic$ordered_design
  expected_design_sha <- triomics_b07_prov_value_sha256(list(
    row_ids = as.list(sample_ids),
    column_ids = as.list(colnames(design_expected)),
    matrix = design_expected
  ))
  if (!is.list(design_identity) || !identical(design_identity$row_ids, as.list(sample_ids)) ||
      !identical(design_identity$column_ids, as.list(colnames(design_expected))) ||
      !identical(design_identity$matrix, design_expected) ||
      !identical(as.character(design_identity$sha256), expected_design_sha)) {
    incomplete("ordered_design", "The exact twelve-by-five ordered design identity is missing or changed.")
  }
  subject_time <- synthetic$subject_time_identity
  expected_subject_time <- list(
    sample_ids = as.list(sample_ids), subject_ids = as.list(sample_subjects),
    visit_ids = as.list(sample_visits), time_values = as.list(sample_times),
    time_unit = "synthetic_step", time_origin = 0
  )
  expected_subject_time$sha256 <- triomics_b07_prov_value_sha256(expected_subject_time)
  if (!identical(subject_time, expected_subject_time)) {
    incomplete("subject_time_identity", "The ordered subject, visit, time, unit, or origin identity changed.")
  }
  expected_centering_rule <- list(
    rule_id = "b-t04-within-subject-arithmetic-centering-v1",
    representation = "subject_centered",
    operation = "subtract_each_subject_feature_mean_from_each_finite_response",
    grouping_identity = "ordered_subject_time_identity",
    missing_value_rule = "exclude_nonfinite_values_from_subject_mean_and_preserve_missing_values",
    randomness = "none"
  )
  if (!identical(synthetic$centering_rule, expected_centering_rule)) {
    incomplete("centering_rule", "The fitted response lacks the approved deterministic subject-centering rule.")
  }

  calibration <- synthetic$calibration
  expected_calibration_protocol <- triomics_b04_protocol()
  expected_calibration_path <- file.path(dirname(dirname(artifact$payload_path)), "calibration.json")
  calibration_payload <- triomics_b07_read_json(expected_calibration_path, "calibration.json")
  expected_calibration_path <- normalizePath(expected_calibration_path, winslash = "/", mustWork = TRUE)
  if (!is.list(calibration) || !identical(as.character(calibration$status), "pass") ||
      !isTRUE(calibration$executed) || isTRUE(calibration$skipped) ||
      !identical(calibration$protocol, expected_calibration_protocol) ||
      !identical(as.character(calibration$protocol_sha256),
                 triomics_b07_prov_value_sha256(expected_calibration_protocol)) ||
      !identical(as.character(calibration$result_sha256),
                 triomics_b07_prov_value_sha256(calibration$result)) ||
      !identical(as.character(calibration$result$protocol$seeds), as.character(1001:1400)) ||
      !identical(as.character(calibration$payload_path), normalizePath(expected_calibration_path, winslash = "/", mustWork = TRUE)) ||
      !file.exists(expected_calibration_path) ||
      !identical(as.character(calibration$payload_sha256), triomics_sha256(expected_calibration_path)) ||
      !identical(triomics_b07_prov_value_sha256(calibration_payload),
                 as.character(calibration$result_sha256))) {
    incomplete("calibration", "The B-T04 calibration protocol, unchanged result, or persisted bytes are not bound.")
  }
  method_state <- synthetic$method_state
  contrast <- analysis$statistics$contrasts[[1L]]
  expected_method_version <- as.character(utils::packageVersion("limma"))
  if (!is.list(method_state) ||
      !identical(as.character(method_state$requested_method), "limma") ||
      !identical(as.character(method_state$method), "limma") ||
      !identical(as.character(method_state$method_status), as.character(context$block_results$status)) ||
      !identical(as.character(method_state$limma$version), expected_method_version) ||
      !identical(as.character(method_state$limma$fit), "limma::lmFit") ||
      !identical(as.character(method_state$limma$contrast), "limma::contrasts.fit") ||
      !identical(as.character(method_state$limma$moderation), "limma::eBayes") ||
      !identical(method_state$limma$trend, FALSE) || !identical(method_state$limma$robust, FALSE) ||
      !identical(method_state$limma$duplicate_correlation_applied, FALSE) ||
      !identical(method_state$retained_subject_columns,
                 as.list(c("subject.sub0001", "subject.sub0002", "subject.sub0003", "subject.sub0004"))) ||
      !identical(method_state$design_rank, 5L) ||
      !identical(method_state$residual_df_rule,
                 "n_observed_minus_rank_of_full_retained_subject_and_time_design") ||
      !identical(method_state$residual_df, 7L) ||
      !identical(as.character(method_state$contrast$contrast_id), "per_synthetic_step") ||
      !identical(method_state$contrast$weights, as.list(contrast$weights)) ||
      !identical(method_state$moderation_settings, list(
        trend = FALSE, robust = FALSE, multiplicity = "BH",
        p_value_source = "moderated$p.value", effective_df_source = "moderated$df.total"
      )) ||
      !identical(as.character(method_state$calibration_protocol_sha256),
                 as.character(calibration$protocol_sha256)) ||
      !identical(as.character(method_state$calibration_result_sha256),
                 as.character(calibration$result_sha256))) {
    incomplete("method_state", "The limma settings, retained columns, contrast, rank, residual rule, or calibration identity changed.")
  }
  result_rows <- context$block_results$blocks$block_1$contrasts$per_synthetic_step$rows
  if (!is.list(result_rows) || length(result_rows) != 2L ||
      !identical(as.character(vapply(result_rows, `[[`, character(1L), "feature_key")), features) ||
      !triomics_b_close(as.numeric(result_rows[[1L]]$effect), 1.5) ||
      !triomics_b_close(as.numeric(result_rows[[2L]]$effect), -0.25) ||
      !all(vapply(result_rows, function(row) identical(as.integer(row$ordinary_df), 7L), logical(1L))) ||
      !all(vapply(result_rows, function(row) identical(as.integer(row$design_rank), 5L), logical(1L))) ||
      !all(vapply(result_rows, function(row) identical(as.character(row$source_hash), raw_response$assay_sha256), logical(1L)))) {
    incomplete("block_results", "The B-T04 public effect, rank, df, or raw matrix identity changed.")
  }
  forbidden_public_names <- c(
    "provenance_context", "synthetic_producer", "resolved_matrices", "assay", "assays",
    "fit", "native_model", "model_object", "lmFit", "MArrayLM"
  )
  public_names <- function(value) {
    if (!is.list(value)) return(character())
    c(names(value), unlist(lapply(value, public_names), use.names = FALSE))
  }
  if (any(forbidden_public_names %in% public_names(artifact$payload))) {
    incomplete("block_results", "Private B-T04 fit context or matrix/model objects were serialized in block results.")
  }
  expected_context_sha256 <- triomics_b04_synthetic_context_sha256(context)
  if (!identical(as.character(synthetic$lineage_identity_sha256), expected_context_sha256)) {
    incomplete("synthetic_producer", "The retained B-T04 same-run context identity digest does not verify.")
  }
  invisible(TRUE)
}

triomics_b07_build_provenance_profile <- function(context, config_sha256, bundle, bundle_path,
                                                   bundle_envelope_path, block_artifact,
                                                   family_artifact, nominations_artifact,
                                                   matrix_artifact, matrix_bindings,
                                                   clinical_artifacts, clinical_dispositions,
                                                   repository_commit) {
  analysis <- context$analysis_spec
  resolved <- context$resolved_analysis
  study_id <- as.character(context$block_results$study_id)
  analysis_id <- as.character(analysis$analysis_id)
  preparation_context_id <- as.character(analysis$preparation_context_id)
  representation_id <- as.character(analysis$design$representation)
  if (is.null(matrix_artifact) || !triomics_b_string(as.character(repository_commit)) ||
      !grepl("^[0-9a-f]{40,64}$", as.character(repository_commit))) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "Native B provenance requires the exact persisted matrix and repository revision.",
                       "provenance", "Run the B-T07 producer from a Git revision and preserve its complete matrix handoff.")
  }
  source_ids <- character()
  entities <- list()
  add_entity <- function(entity) {
    entities[[length(entities) + 1L]] <<- entity
    invisible(entity$entity_id)
  }
  source_files <- context$source_files
  for (source_file in source_files) {
    source_id <- as.character(source_file$source_id)
    source_ids <- c(source_ids, source_id)
    hit <- which(vapply(context$source_manifests, function(manifest) {
      identical(as.character(manifest$source_id), source_id)
    }, logical(1L)))
    manifest <- if (length(hit) == 1L) context$source_manifests[[hit[[1L]]]] else NULL
    if (!is.null(manifest) && !identical(as.character(manifest$sha256), as.character(source_file$sha256))) {
      triomics_b07_abort("B_EXPORT_SOURCE_CHANGED", "The retained source manifest does not match the Stage B source bytes.",
                         source_id, "Rerun Stage B with unchanged source bytes before provenance export.")
    }
    manifest_value <- triomics_b07_prov_safe_manifest(source_file, manifest)
    entity_id <- triomics_b07_prov_entity_id("source_manifest", list(source_id = source_id, sha256 = source_file$sha256))
    add_entity(triomics_b07_prov_embedded_entity(
      "source_manifest", entity_id, manifest_value, study_id,
      triomics_b07_prov_scope(study_id),
      generated_by_activity_id = NULL, schema_uri = "urn:triomics:source-manifest:identity:1.0"
    ))
  }
  source_ids <- sort(unique(source_ids), method = "radix")
  analysis_activity <- "b-t07-stage-b-fit"
  export_activity <- "b-t07-handoff-export"
  source_entity_ids <- vapply(Filter(function(entity) identical(entity$scientific_role, "source_manifest"), entities),
                              `[[`, character(1L), "entity_id")
  provenance_scope <- triomics_b07_prov_scope(study_id, analysis_id = analysis_id,
                                              preparation_context_id = preparation_context_id,
                                              representation_id = representation_id)
  metadata_value <- list(
    sample_metadata = context$sample_metadata,
    sample_registry = context$sample_registry,
    declared_factor_levels = context$declared_factor_levels
  )
  metadata_entity_id <- triomics_b07_prov_entity_id("normalized_metadata", analysis_id)
  add_entity(triomics_b07_prov_embedded_entity(
    "normalized_metadata", metadata_entity_id, metadata_value, study_id,
    triomics_b07_prov_scope(study_id, analysis_id = analysis_id),
    generated_by_activity_id = analysis_activity,
    derived_from_entity_ids = source_entity_ids,
    row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(context$sample_metadata$sample_id),
    column_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(names(context$sample_metadata)),
    schema_uri = "urn:triomics:normalized-metadata:identity:1.0"
  ))
  unit_map <- context$analysis_unit_map
  if (!is.data.frame(unit_map) || !"unit_id" %in% names(unit_map)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The Stage B runner omitted the exact same-run analysis unit map.",
                       "analysis_unit_map", "Preserve the resolved unit map in the Stage B fit context.")
  }
  unit_entity_id <- triomics_b07_prov_entity_id("analysis_unit_map", list(
    analysis_id = analysis_id, preparation_context_id = preparation_context_id,
    representation_id = representation_id
  ))
  add_entity(triomics_b07_prov_embedded_entity(
    "analysis_unit_map", unit_entity_id, unit_map, study_id, provenance_scope,
    generated_by_activity_id = analysis_activity,
    derived_from_entity_ids = c(source_entity_ids, metadata_entity_id),
    row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(unit_map$unit_id),
    schema_uri = "urn:triomics:analysis-unit-map:identity:1.0"
  ))
  block_ids <- names(resolved$blocks)
  if (!length(block_ids) || !identical(sort(block_ids, method = "radix"),
                                        sort(names(matrix_bindings), method = "radix"))) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "Resolved Stage B blocks and persisted matrix bindings disagree.",
                       "represented_matrix", "Preserve every and only same-run resolved matrix block.")
  }
  feature_entities <- list()
  preprocessing_entities <- list()
  matrix_entities <- list()
  config_blocks <- config_blocks_by_id <- context$config$blocks
  config_block_ids <- vapply(config_blocks_by_id, function(block) as.character(block$block_id), character(1L))
  names(config_blocks_by_id) <- config_block_ids
  for (block_id in sort(block_ids, method = "radix")) {
    block <- resolved$blocks[[block_id]]
    assay <- block$assay
    if (!is.matrix(assay) || is.null(rownames(assay)) || is.null(colnames(assay)) ||
        !identical(unname(as.character(rownames(assay))),
                   unname(as.character(block$feature_keys))) ||
        !identical(unname(as.character(colnames(assay))),
                   unname(as.character(block$sample_ids)))) {
      triomics_b07_abort("B_EXPORT_INCOMPLETE", "A resolved matrix has no complete keyed same-run identity.",
                         block_id, "Preserve the exact resolved feature and unit keys from the fit.")
    }
    registry <- context$feature_registry[as.character(context$feature_registry$block_id) == block_id, , drop = FALSE]
    if (!nrow(registry) || !setequal(as.character(registry$feature_key), rownames(assay))) {
      triomics_b07_abort("B_EXPORT_INCOMPLETE", "The FeatureRegistry does not cover the resolved matrix rows.",
                         block_id, "Preserve the exact same-run FeatureRegistry rows for this block.")
    }
    feature_id <- triomics_b07_prov_entity_id("feature_registry", block_id)
    feature_entities[[block_id]] <- feature_id
    add_entity(triomics_b07_prov_embedded_entity(
      "feature_registry", feature_id, registry, study_id,
      triomics_b07_prov_scope(study_id, block_id = block_id),
      generated_by_activity_id = analysis_activity,
      derived_from_entity_ids = c(source_entity_ids, metadata_entity_id),
      row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(registry$feature_key),
      schema_uri = "urn:triomics:feature-registry:identity:1.0"
    ))
    cfg_block <- config_blocks_by_id[[block_id]]
    if (is.null(cfg_block) || is.null(cfg_block$preprocessing) || is.null(cfg_block$measurement)) {
      triomics_b07_abort("B_EXPORT_INCOMPLETE", "The declared B preprocessing or measurement state is missing.",
                         block_id, "Preserve the validated configuration state used by the same-run Stage B fit.")
    }
    preprocessing_value <- list(
      block_id = block_id,
      preparation_context_id = preparation_context_id,
      measurement = cfg_block$measurement,
      preprocessing = cfg_block$preprocessing,
      resolved_feature_scales = stats::setNames(as.list(as.character(block$scales)), as.character(block$feature_keys)),
      matrix_hash = triomics_b07_matrix_hash(block_id, assay)
    )
    if (!is.null(context$synthetic_producer)) {
      synthetic <- context$synthetic_producer
      preprocessing_value$synthetic_response_identity <- list(
        exported_representation = "uncentered_generated_response",
        raw_response_assay_sha256 = synthetic$raw_response$assay_sha256,
        raw_response_identity_sha256 = synthetic$raw_response$identity_sha256,
        centered_response_assay_sha256 = synthetic$centered_response$assay_sha256,
        centered_response_identity_sha256 = synthetic$centered_response$identity_sha256,
        centering_rule = synthetic$centering_rule,
        subject_time_identity_sha256 = synthetic$subject_time_identity$sha256
      )
    }
    preprocessing_id <- triomics_b07_prov_entity_id("preprocessing_state", list(
      analysis_id = analysis_id, block_id = block_id,
      preparation_context_id = preparation_context_id, representation_id = representation_id
    ))
    preprocessing_entities[[block_id]] <- preprocessing_id
    add_entity(triomics_b07_prov_embedded_entity(
      "preprocessing_state", preprocessing_id, preprocessing_value, study_id,
      triomics_b07_prov_scope(study_id, analysis_id, block_id,
                              preparation_context_id = preparation_context_id,
                              representation_id = representation_id),
      generated_by_activity_id = analysis_activity,
      derived_from_entity_ids = c(source_entity_ids, metadata_entity_id, feature_id),
      schema_uri = "urn:triomics:preprocessing-state:identity:1.0"
    ))
    matrix_envelope_path <- matrix_artifact$envelope_path
    matrix_id <- triomics_b07_prov_entity_id("represented_matrix", list(
      analysis_id = analysis_id, block_id = block_id,
      preparation_context_id = preparation_context_id, representation_id = representation_id
    ))
    matrix_entities[[block_id]] <- matrix_id
    add_entity(triomics_b07_prov_artifact_entity(
      "represented_matrix", matrix_id, matrix_artifact$path, matrix_envelope_path,
      study_id, triomics_b07_prov_scope(study_id, analysis_id, block_id,
                                        preparation_context_id = preparation_context_id,
                                        representation_id = representation_id),
      generated_by_activity_id = export_activity,
      derived_from_entity_ids = c(feature_id, unit_entity_id, preprocessing_id),
      row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(rownames(assay)),
      column_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(colnames(assay))
    ))
  }
  resolved_row_ids <- unlist(lapply(sort(block_ids, method = "radix"), function(block_id) {
    paste0(block_id, "::", as.character(resolved$blocks[[block_id]]$feature_keys))
  }), use.names = FALSE)
  resolved_column_ids <- sort(unique(unlist(lapply(resolved$blocks, function(block) {
    as.character(block$sample_ids)
  }), use.names = FALSE)), method = "radix")
  resolved_value <- list(
    study_id = resolved$study_id,
    analysis_id = resolved$analysis_id,
    preparation_context_id = resolved$preparation_context_id,
    representation = resolved$representation,
    model_plan = resolved$model_plan,
    blocks = lapply(sort(block_ids, method = "radix"), function(block_id) {
      block <- resolved$blocks[[block_id]]
      list(block_id = block_id, feature_keys = as.list(as.character(block$feature_keys)),
           sample_ids = as.list(as.character(block$sample_ids)),
           groups = as.list(as.character(block$groups)), scales = as.list(as.character(block$scales)),
           source_hash = as.character(block$source_hash))
    })
  )
  if (!is.null(context$synthetic_producer)) {
    synthetic <- context$synthetic_producer
    resolved_value$synthetic_producer_identity <- list(
      protocol_artifact_id = as.character(synthetic$protocol$value$protocol_id),
      protocol_sha256 = as.character(synthetic$protocol$sha256),
      effective_configuration_sha256 = as.character(config_sha256),
      ordered_design = list(
        row_ids = synthetic$ordered_design$row_ids,
        column_ids = synthetic$ordered_design$column_ids,
        matrix = synthetic$ordered_design$matrix,
        sha256 = synthetic$ordered_design$sha256
      ),
      subject_time_identity = synthetic$subject_time_identity,
      raw_response_identity = synthetic$raw_response[c(
        "assay_sha256", "identity_sha256", "row_identity_sha256", "column_identity_sha256"
      )],
      centered_response_identity = synthetic$centered_response[c(
        "assay_sha256", "identity_sha256", "row_identity_sha256", "column_identity_sha256"
      )],
      centering_rule = synthetic$centering_rule,
      lineage_identity_sha256 = as.character(synthetic$lineage_identity_sha256)
    )
  }
  resolved_entity_id <- triomics_b07_prov_entity_id("resolved_analysis", list(
    analysis_id = analysis_id, preparation_context_id = preparation_context_id,
    representation_id = representation_id
  ))
  add_entity(triomics_b07_prov_embedded_entity(
    "resolved_analysis", resolved_entity_id, resolved_value, study_id, provenance_scope,
    generated_by_activity_id = analysis_activity,
    derived_from_entity_ids = c(source_entity_ids, metadata_entity_id, unit_entity_id,
                                unlist(feature_entities, use.names = FALSE)),
    row_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(resolved_row_ids),
    column_identity_sha256 = triomics_b07_prov_ordered_ids_sha256(resolved_column_ids),
    schema_uri = "urn:triomics:resolved-analysis:identity:1.0"
  ))
  method_ids <- sort(unique(as.character(context$producer_method_ids)), method = "radix")
  row_identity <- triomics_b07_prov_result_rows(context$block_results)
  method_entities <- list()
  for (method_id in method_ids) {
    method_value <- list(
      method_id = method_id,
      method_state = context$method_state,
      statistics_spec = context$statistics_spec,
      deqms_status = context$block_results$deqms_status,
      result_row_identities = row_identity,
      represented_matrix_bindings = lapply(sort(names(matrix_bindings), method = "radix"), function(block_id) {
        binding <- matrix_bindings[[block_id]]
        list(block_id = block_id, representation_id = binding$representation_id,
             matrix_hash = binding$matrix_hash, binding_hash = binding$binding_hash,
             feature_bindings = lapply(binding$feature_bindings, function(feature) {
               list(feature_key = feature$feature_key, fitted_claims = feature$fitted_claims)
             }))
      }),
      count_metadata_identity = if (is.null(context$statistics_spec$count_metadata)) NULL else
        triomics_b07_prov_value_sha256(context$statistics_spec$count_metadata)
    )
    if (!is.null(context$synthetic_producer)) {
      synthetic <- context$synthetic_producer
      method_value$synthetic_producer_state <- list(
        method_state = synthetic$method_state,
        centering_rule = synthetic$centering_rule,
        raw_response_identity = synthetic$raw_response[c(
          "assay_sha256", "identity_sha256", "row_identity_sha256", "column_identity_sha256"
        )],
        centered_response_identity = synthetic$centered_response[c(
          "assay_sha256", "identity_sha256", "row_identity_sha256", "column_identity_sha256"
        )],
        subject_time_identity = synthetic$subject_time_identity,
        calibration_identity = synthetic$calibration[c(
          "status", "executed", "skipped", "protocol_sha256", "result_sha256", "payload_sha256"
        )],
        lineage_identity_sha256 = as.character(synthetic$lineage_identity_sha256)
      )
    }
    method_id_entity <- triomics_b07_prov_entity_id("method_state", list(
      analysis_id = analysis_id, method_id = method_id,
      preparation_context_id = preparation_context_id, representation_id = representation_id
    ))
    method_entities[[method_id]] <- method_id_entity
    add_entity(triomics_b07_prov_embedded_entity(
      "method_state", method_id_entity, method_value, study_id,
      triomics_b07_prov_scope(study_id, analysis_id = analysis_id, method_id = method_id,
                              preparation_context_id = preparation_context_id,
                              representation_id = representation_id),
      generated_by_activity_id = analysis_activity,
      derived_from_entity_ids = c(source_entity_ids, metadata_entity_id, unit_entity_id,
                                  resolved_entity_id),
      schema_uri = "urn:triomics:method-state:identity:1.0"
    ))
  }
  block_envelope_entity_id <- triomics_b07_prov_entity_id("predecessor_envelope", "stage-b-block-results")
  predecessor_payload_entity_id <- triomics_b07_prov_entity_id("predecessor_payload", "stage-b-block-results")
  block_results_entity_id <- triomics_b07_prov_entity_id("block_results", "stage-b-block-results")
  block_scope <- provenance_scope
  add_entity(triomics_b07_prov_artifact_entity(
    "predecessor_envelope", block_envelope_entity_id, block_artifact$payload_path,
    block_artifact$envelope_path, study_id, block_scope,
    generated_by_activity_id = analysis_activity,
    derived_from_entity_ids = c(source_entity_ids, metadata_entity_id, resolved_entity_id,
                                unlist(method_entities, use.names = FALSE))
  ))
  add_entity(triomics_b07_prov_artifact_entity(
    "predecessor_payload", predecessor_payload_entity_id, block_artifact$payload_path,
    block_artifact$envelope_path, study_id, block_scope,
    generated_by_activity_id = analysis_activity,
    derived_from_entity_ids = c(source_entity_ids, resolved_entity_id),
    storage_form = "file", member_of_entity_id = block_envelope_entity_id
  ))
  add_entity(triomics_b07_prov_artifact_entity(
    "block_results", block_results_entity_id, block_artifact$payload_path,
    block_artifact$envelope_path, study_id, block_scope,
    generated_by_activity_id = analysis_activity,
    derived_from_entity_ids = c(source_entity_ids, resolved_entity_id,
                                unlist(method_entities, use.names = FALSE))
  ))
  family_entity_id <- triomics_b07_prov_entity_id("family_ledger", "b-t07-family-ledger")
  nominations_entity_id <- triomics_b07_prov_entity_id("candidate_nominations", "b-t07-candidate-nominations")
  add_entity(triomics_b07_prov_artifact_entity(
    "family_ledger", family_entity_id, family_artifact$path, family_artifact$envelope_path,
    study_id, block_scope, generated_by_activity_id = export_activity,
    derived_from_entity_ids = c(block_results_entity_id, predecessor_payload_entity_id)
  ))
  add_entity(triomics_b07_prov_artifact_entity(
    "candidate_nominations", nominations_entity_id, nominations_artifact$path,
    nominations_artifact$envelope_path, study_id, block_scope,
    generated_by_activity_id = export_activity,
    derived_from_entity_ids = c(block_results_entity_id, predecessor_payload_entity_id)
  ))
  clinical_accounting <- triomics_b07_clinical_output_accounting(
    context$config, analysis_id, clinical_artifacts
  )
  declared_routes <- list()
  route_entity_ids <- character()
  objectives <- if (is.list(context$config$clinical)) context$config$clinical$objectives else list()
  if (is.null(objectives)) objectives <- list()
  declared_objective_ids <- vapply(objectives, function(objective) {
    triomics_b07_clinical_string(objective$objective_id)
  }, character(1L))
  if (anyNA(declared_objective_ids) || anyDuplicated(declared_objective_ids)) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "Declared clinical route ids are missing or duplicated.",
                       "clinical.objectives", "Keep one stable objective id per declared route.")
  }
  for (objective in objectives) {
    route_id <- triomics_b07_clinical_string(objective$objective_id)
    route_analysis <- triomics_b07_clinical_route_analysis(context$config, objective,
                                                           context$analysis_spec)
    route_analysis_id <- as.character(route_analysis$analysis_id)
    route_preparation_context_id <- as.character(route_analysis$preparation_context_id)
    route_representation_id <- if (identical(route_analysis_id, analysis_id)) {
      representation_id
    } else {
      as.character(route_analysis$design$representation)
    }
    route_requested <- isTRUE(context$config$clinical$enabled) &&
      identical(route_analysis_id, analysis_id)
    route_accounting <- clinical_accounting$by_objective[[route_id]]
    route_claims <- clinical_accounting$claims_by_objective[[route_id]]
    matched_artifacts <- list()
    if (route_requested && is.list(route_claims) && length(route_claims)) {
      matched_artifacts <- lapply(route_claims, `[[`, "evidence")
    }
    route_status <- if (!route_requested) {
      "not_requested"
    } else if (is.list(route_accounting) && isTRUE(route_accounting$accounted)) {
      as.character(route_accounting$status)
    } else {
      "unavailable"
    }
    disposition_value <- list(
      route_id = route_id,
      route_kind = "clinical",
      analysis_id = route_analysis_id,
      preparation_context_id = route_preparation_context_id,
      representation_id = route_representation_id,
      objective_declaration = objective,
      requested = route_requested,
      status = route_status,
      evidence = matched_artifacts,
      declared_artifact_dispositions = clinical_accounting$dispositions
    )
    declared_routes[[length(declared_routes) + 1L]] <- list(
      route_id = route_id, route_kind = "clinical", analysis_id = route_analysis_id,
      preparation_context_id = route_preparation_context_id,
      representation_id = route_representation_id, method_id = NULL
    )
    route_entity_id <- triomics_b07_prov_entity_id("clinical_evidence", route_id)
    route_entity_ids <- c(route_entity_ids, route_entity_id)
    add_entity(triomics_b07_prov_embedded_entity(
      "clinical_evidence", route_entity_id, disposition_value, study_id,
      triomics_b07_prov_scope(study_id, analysis_id = route_analysis_id,
                              preparation_context_id = route_preparation_context_id,
                              representation_id = route_representation_id, route_id = route_id),
      generated_by_activity_id = export_activity,
      derived_from_entity_ids = if (route_requested) {
        c(source_entity_ids, block_results_entity_id)
      } else {
        source_entity_ids
      },
      schema_uri = "urn:triomics:clinical-route-evidence:1.0"
    ))
  }
  declared_routes <- declared_routes[order(
    vapply(declared_routes, `[[`, character(1L), "route_kind"),
    vapply(declared_routes, `[[`, character(1L), "route_id"), method = "radix"
  )]
  bundle_entity_id <- triomics_b07_prov_entity_id("handoff_payload", as.character(bundle$artifact_id))
  handoff_derived <- c(block_results_entity_id, predecessor_payload_entity_id, family_entity_id,
                       nominations_entity_id, unlist(feature_entities, use.names = FALSE),
                       unlist(preprocessing_entities, use.names = FALSE),
                       unlist(matrix_entities, use.names = FALSE),
                       unlist(method_entities, use.names = FALSE), route_entity_ids)
  add_entity(triomics_b07_prov_artifact_entity(
    "handoff_payload", bundle_entity_id, bundle_path, bundle_envelope_path,
    study_id, block_scope, generated_by_activity_id = export_activity,
    derived_from_entity_ids = handoff_derived
  ))
  fit_generated <- vapply(Filter(function(entity) identical(entity$generated_by_activity_id, analysis_activity), entities),
                          `[[`, character(1L), "entity_id")
  export_generated <- vapply(Filter(function(entity) identical(entity$generated_by_activity_id, export_activity), entities),
                             `[[`, character(1L), "entity_id")
  analysis_used <- source_entity_ids
  export_used <- setdiff(vapply(entities, `[[`, character(1L), "entity_id"),
                         c(source_entity_ids, export_generated))
  activities <- list(
    list(activity_id = analysis_activity, stage = "B", activity_type = "analysis",
         method_id = method_ids[[1L]], used_entity_ids = as.list(sort(analysis_used, method = "radix")),
         generated_entity_ids = as.list(sort(fit_generated, method = "radix"))),
    list(activity_id = export_activity, stage = "B", activity_type = "handoff_export",
         method_id = method_ids[[1L]], used_entity_ids = as.list(sort(unique(export_used), method = "radix")),
         generated_entity_ids = as.list(sort(export_generated, method = "radix")))
  )
  entities <- entities[order(
    vapply(entities, `[[`, character(1L), "scientific_role"),
    vapply(entities, `[[`, character(1L), "entity_type"),
    vapply(entities, `[[`, character(1L), "entity_id"), method = "radix"
  )]
  profile <- list(
    profile = "triomics.stage-handoff-provenance",
    profile_version = "1.0",
    producer_stage = "B",
    study_id = study_id,
    run_id = as.character(context$run_id),
    handoff_artifact_id = as.character(bundle$artifact_id),
    producer_activity_id = export_activity,
    identity = list(
      schema_uri = "urn:triomics:stage-handoff-provenance:identity",
      schema_version = "1.0",
      handoff_role = "stage-b-handoff",
      activities = activities,
      entities = entities,
      declared_routes = declared_routes
    ),
    producer = list(
      package_version = as.character(utils::packageVersion("triomics")),
      repository_commit = as.character(repository_commit),
      module_id = "B-T07",
      method_ids = as.list(method_ids),
      effective_configuration_sha256 = as.character(config_sha256),
      runtime_backend_versions = triomics_b07_prov_runtime_versions()
    ),
    limitations = list(
      "Native same-run B producer provenance; synthetic software evidence is not scientific validation.",
      "The profile inventories the handoff and predecessor bytes; C/D verification and publication remain out of scope."
    ),
    migration = NULL
  )
  profile$identity_sha256 <- triomics_provenance_identity_sha256(profile$identity)
  profile
}

triomics_b07_write_provenance <- function(context, config_sha256, bundle, bundle_path,
                                          bundle_envelope_path, block_artifact,
                                          family_artifact, nominations_artifact,
                                          matrix_artifact, matrix_bindings,
                                          clinical_artifacts, clinical_dispositions,
                                          repository_commit, privacy_class) {
  handoff_dir <- dirname(bundle_path)
  profile_path <- file.path(handoff_dir, "stage-handoff-provenance.json")
  profile_envelope_path <- file.path(handoff_dir, "stage-handoff-provenance.envelope.json")
  if (file.exists(profile_path) || file.exists(profile_envelope_path)) {
    triomics_b07_abort("B_EXPORT_CACHE", "A native provenance frame already exists for this handoff.",
                       "provenance", "Reuse only a fully revalidated frame or write a new handoff identity.")
  }
  finalized <- FALSE
  on.exit(if (!finalized) unlink(c(profile_path, profile_envelope_path), force = TRUE), add = TRUE)
  profile <- triomics_b07_build_provenance_profile(
    context, config_sha256, bundle, bundle_path, bundle_envelope_path, block_artifact,
    family_artifact, nominations_artifact, matrix_artifact, matrix_bindings,
    clinical_artifacts, clinical_dispositions, repository_commit
  )
  valid <- tryCatch(isTRUE(triomics_validate_stage_handoff_provenance(
    profile, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
  )), error = function(error) FALSE)
  if (!valid) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The native B provenance profile failed the accepted public validator.",
                       "provenance", "Do not accept a partial or schema-invalid provenance frame.")
  }
  triomics_write_json_atomic(profile, profile_path)
  profile_envelope <- triomics_artifact_envelope(
    artifact_type = "stage_handoff_provenance",
    artifact_id = "b-t07-stage-handoff-provenance",
    study_id = as.character(profile$study_id),
    run_id = as.character(profile$run_id),
    status = "complete",
    config_sha256 = as.character(config_sha256),
    payload = profile_path,
    producer_commit = as.character(repository_commit),
    inputs = list(list(artifact_id = as.character(bundle$artifact_id),
                       sha256 = triomics_sha256(bundle_path))),
    reason_codes = character(),
    privacy_class = as.character(privacy_class)
  )
  triomics_write_envelope(profile_envelope, profile_envelope_path)
  readback <- tryCatch(jsonlite::read_json(profile_path, simplifyVector = FALSE), error = function(error) NULL)
  envelope_ok <- isTRUE(triomics_validate_envelope(profile_envelope_path))
  profile_ok <- is.list(readback) && tryCatch(isTRUE(triomics_validate_stage_handoff_provenance(
    readback, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
  )), error = function(error) FALSE)
  if (!envelope_ok || !profile_ok ||
      !identical(triomics_sha256(profile_path), as.character(profile_envelope$payload_sha256))) {
    triomics_b07_abort("B_EXPORT_INCOMPLETE", "The native B provenance payload/envelope failed exact-byte read-back validation.",
                       "provenance", "Do not accept a frame without both successful validator and envelope read-back checks.")
  }
  finalized <- TRUE
  list(path = profile_path, envelope_path = profile_envelope_path,
       payload_sha256 = triomics_sha256(profile_path),
       envelope_sha256 = triomics_sha256(profile_envelope_path), profile = profile)
}

triomics_b07_provenance_entities <- function(profile, role) {
  Filter(function(entity) identical(as.character(entity$scientific_role), role), profile$identity$entities)
}

triomics_b07_profile_artifact_matches <- function(entity, payload_path, envelope_path, artifact_id) {
  if (is.null(entity) || !file.exists(payload_path) || !file.exists(envelope_path) ||
      !isTRUE(triomics_validate_envelope(envelope_path))) return(FALSE)
  envelope <- tryCatch(jsonlite::read_json(envelope_path, simplifyVector = FALSE), error = function(error) NULL)
  is.list(envelope) &&
    identical(as.character(entity$artifact_id), as.character(artifact_id)) &&
    identical(as.character(envelope$artifact_id), as.character(artifact_id)) &&
    identical(as.character(entity$digest), triomics_sha256(envelope_path)) &&
    identical(as.character(entity$artifact_payload_sha256), triomics_sha256(payload_path)) &&
    identical(as.character(envelope$payload_sha256), triomics_sha256(payload_path))
}

triomics_b07_validate_b04_persisted_evidence <- function(bundle, bundle_path,
                                                         bundle_envelope_path, profile) {
  if (!identical(as.character(bundle$study_id), "triomics_b_t04_fixture")) return(TRUE)
  fail <- function() FALSE
  root <- tryCatch(
    normalizePath(file.path(dirname(bundle_path), "..", "..", ".."),
                  winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(root)) return(fail())
  protocol_path <- file.path(root, "B", "b-t04-synthetic-protocol.json")
  block_envelope_path <- file.path(root, "B", "block-results.envelope.json")
  calibration_path <- file.path(root, "calibration.json")
  paths <- c(protocol_path, block_envelope_path, calibration_path, bundle_envelope_path)
  if (any(!file.exists(paths) | dir.exists(paths)) ||
      any(vapply(paths, triomics_is_symlink, logical(1L)))) return(fail())

  expected_protocol <- tryCatch(
    triomics_b04_synthetic_protocol(as.character(bundle$config_sha256)),
    error = function(error) NULL
  )
  if (!is.list(expected_protocol)) return(fail())
  expected_protocol_bytes <- tryCatch(
    charToRaw(enc2utf8(triomics_b07_prov_canonical_value(expected_protocol))),
    error = function(error) NULL
  )
  protocol_bytes <- tryCatch(
    readBin(protocol_path, what = "raw", n = file.info(protocol_path)$size),
    error = function(error) NULL
  )
  if (is.null(expected_protocol_bytes) || is.null(protocol_bytes) ||
      !identical(protocol_bytes, expected_protocol_bytes)) return(fail())
  protocol_sha256 <- tryCatch(triomics_sha256(protocol_path), error = function(error) NULL)
  calibration_sha256 <- tryCatch(triomics_sha256(calibration_path), error = function(error) NULL)
  if (!triomics_sha256_text(protocol_sha256) || !triomics_sha256_text(calibration_sha256)) return(fail())

  calibration <- tryCatch(
    jsonlite::read_json(calibration_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  expected_calibration_protocol <- triomics_b04_protocol()
  if (!is.list(calibration) ||
      !identical(as.character(calibration$status), "pass") ||
      !isTRUE(calibration$executed) || isTRUE(calibration$skipped) ||
      !is.list(calibration$protocol) ||
      !identical(triomics_b07_prov_value_sha256(calibration$protocol),
                 triomics_b07_prov_value_sha256(expected_calibration_protocol)) ||
      !identical(as.character(calibration$protocol$seeds), as.character(1001:1400))) {
    return(fail())
  }

  block_envelope <- tryCatch(
    jsonlite::read_json(block_envelope_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  bundle_envelope <- tryCatch(
    jsonlite::read_json(bundle_envelope_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  has_input <- function(envelope, artifact_id, sha256) {
    is.list(envelope) && is.list(envelope$inputs) && any(vapply(envelope$inputs, function(input) {
      is.list(input) && identical(as.character(input$artifact_id), artifact_id) &&
        identical(as.character(input$sha256), sha256)
    }, logical(1L)))
  }
  protocol_id <- as.character(expected_protocol$protocol_id)
  if (!has_input(block_envelope, protocol_id, protocol_sha256) ||
      !has_input(bundle_envelope, protocol_id, protocol_sha256) ||
      !has_input(bundle_envelope, "b-t04-calibration", calibration_sha256)) return(fail())

  source_entities <- triomics_b07_provenance_entities(profile, "source_manifest")
  resolved_entities <- triomics_b07_provenance_entities(profile, "resolved_analysis")
  method_entities <- triomics_b07_provenance_entities(profile, "method_state")
  expected_source_entity_id <- triomics_b07_prov_entity_id("source_manifest", list(
    source_id = protocol_id, sha256 = protocol_sha256
  ))
  expected_resolved_entity_id <- triomics_b07_prov_entity_id("resolved_analysis", list(
    analysis_id = as.character(bundle$analysis_id),
    preparation_context_id = as.character(bundle$preparation_context_id),
    representation_id = as.character(bundle$representation_id)
  ))
  expected_method_entity_id <- triomics_b07_prov_entity_id("method_state", list(
    analysis_id = as.character(bundle$analysis_id), method_id = "limma",
    preparation_context_id = as.character(bundle$preparation_context_id),
    representation_id = as.character(bundle$representation_id)
  ))
  if (length(source_entities) != 1L || length(resolved_entities) != 1L ||
      length(method_entities) != 1L ||
      !identical(as.character(source_entities[[1L]]$entity_id), expected_source_entity_id) ||
      !identical(as.character(resolved_entities[[1L]]$entity_id), expected_resolved_entity_id) ||
      !identical(as.character(method_entities[[1L]]$entity_id), expected_method_entity_id) ||
      !triomics_sha256_text(as.character(source_entities[[1L]]$digest)) ||
      !triomics_sha256_text(as.character(resolved_entities[[1L]]$digest)) ||
      !triomics_sha256_text(as.character(method_entities[[1L]]$digest))) return(fail())
  TRUE
}

triomics_b07_validate_provenance_artifacts <- function(bundle, bundle_path, bundle_envelope_path) {
  handoff_dir <- dirname(bundle_path)
  profile_path <- file.path(handoff_dir, "stage-handoff-provenance.json")
  profile_envelope_path <- file.path(handoff_dir, "stage-handoff-provenance.envelope.json")
  if (any(!file.exists(c(profile_path, profile_envelope_path))) ||
      any(dir.exists(c(profile_path, profile_envelope_path))) ||
      any(vapply(c(profile_path, profile_envelope_path), triomics_is_symlink, logical(1L))) ||
      !isTRUE(triomics_validate_envelope(profile_envelope_path))) return(FALSE)
  profile <- tryCatch(jsonlite::read_json(profile_path, simplifyVector = FALSE), error = function(error) NULL)
  envelope <- tryCatch(jsonlite::read_json(profile_envelope_path, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(profile) || !is.list(envelope) ||
      !identical(as.character(envelope$artifact_type), "stage_handoff_provenance") ||
      !identical(as.character(envelope$artifact_id), "b-t07-stage-handoff-provenance") ||
      !identical(as.character(profile$producer_stage), "B") ||
      !identical(as.character(profile$handoff_artifact_id), as.character(bundle$artifact_id)) ||
      !identical(as.character(profile$study_id), as.character(bundle$study_id)) ||
      !identical(as.character(profile$producer$effective_configuration_sha256), as.character(bundle$config_sha256)) ||
      !identical(as.character(envelope$config_sha256), as.character(bundle$config_sha256)) ||
      !identical(as.character(envelope$run_id), as.character(profile$run_id)) ||
      !tryCatch(isTRUE(triomics_validate_stage_handoff_provenance(
        profile, "b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope"
      )), error = function(error) FALSE)) return(FALSE)
  if (!isTRUE(triomics_b07_validate_b04_persisted_evidence(
    bundle, bundle_path, bundle_envelope_path, profile
  ))) return(FALSE)
  handoff <- triomics_b07_provenance_entities(profile, "handoff_payload")
  if (length(handoff) != 1L ||
      !triomics_b07_profile_artifact_matches(handoff[[1L]], bundle_path, bundle_envelope_path,
                                             as.character(bundle$artifact_id))) return(FALSE)
  root <- tryCatch(normalizePath(file.path(handoff_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE),
                   error = function(error) NULL)
  if (is.null(root)) return(FALSE)
  block_path <- file.path(root, "B", "block-results.json")
  block_envelope_path <- file.path(root, "B", "block-results.envelope.json")
  block_entity <- triomics_b07_provenance_entities(profile, "block_results")
  predecessor_envelope <- triomics_b07_provenance_entities(profile, "predecessor_envelope")
  predecessor_payload <- triomics_b07_provenance_entities(profile, "predecessor_payload")
  if (length(block_entity) != 1L || length(predecessor_envelope) != 1L || length(predecessor_payload) != 1L ||
      !triomics_b07_profile_artifact_matches(block_entity[[1L]], block_path, block_envelope_path,
                                             "stage-b-block-results") ||
      !triomics_b07_profile_artifact_matches(predecessor_envelope[[1L]], block_path, block_envelope_path,
                                             "stage-b-block-results") ||
      !identical(as.character(predecessor_payload[[1L]]$digest), triomics_sha256(block_path)) ||
      !identical(as.character(predecessor_payload[[1L]]$member_of_entity_id),
                 as.character(predecessor_envelope[[1L]]$entity_id))) return(FALSE)
  member_path <- function(artifact_id) {
    hits <- Filter(function(member) identical(as.character(member$artifact_id), artifact_id), bundle$members)
    if (length(hits) != 1L || is.null(hits[[1L]]$path) || is.null(hits[[1L]]$envelope_path)) return(NULL)
    list(payload = file.path(root, as.character(hits[[1L]]$path)),
         envelope = file.path(root, as.character(hits[[1L]]$envelope_path)),
         artifact_id = if (identical(artifact_id, "family_ledger")) "b-t07-family-ledger" else
           if (identical(artifact_id, "candidate_nominations")) "b-t07-candidate-nominations" else NULL)
  }
  for (role in c("family_ledger", "candidate_nominations")) {
    entities <- triomics_b07_provenance_entities(profile, role)
    paths <- member_path(role)
    if (length(entities) != 1L || is.null(paths) ||
        !triomics_b07_profile_artifact_matches(entities[[1L]], paths$payload, paths$envelope, paths$artifact_id)) return(FALSE)
  }
  matrix_member <- member_path("stage_c_matrix_values")
  matrix_entities <- triomics_b07_provenance_entities(profile, "represented_matrix")
  if (is.null(matrix_member) || !length(matrix_entities)) return(FALSE)
  matrix_envelope <- tryCatch(jsonlite::read_json(matrix_member$envelope, simplifyVector = FALSE),
                              error = function(error) NULL)
  matrices <- tryCatch(readRDS(matrix_member$payload), error = function(error) NULL)
  if (!is.list(matrix_envelope) || !is.list(matrices) ||
      !identical(as.character(matrix_envelope$artifact_id), "b-t07-stage-c-input-matrices") ||
      any(!vapply(matrix_entities, function(entity) {
        block_id <- as.character(entity$block_id)
        is.matrix(matrices[[block_id]]) &&
          identical(as.character(entity$artifact_id), as.character(matrix_envelope$artifact_id)) &&
          identical(as.character(entity$digest), triomics_sha256(matrix_member$envelope)) &&
          identical(as.character(entity$artifact_payload_sha256), triomics_sha256(matrix_member$payload)) &&
          identical(as.character(entity$row_identity_sha256),
                    triomics_b07_prov_ordered_ids_sha256(rownames(matrices[[block_id]]))) &&
          identical(as.character(entity$column_identity_sha256),
                    triomics_b07_prov_ordered_ids_sha256(colnames(matrices[[block_id]])))
      }, logical(1L)))) return(FALSE)
  matrix_manifest_member <- member_path("stage_c_matrices")
  matrix_manifest <- if (is.null(matrix_manifest_member)) NULL else
    tryCatch(jsonlite::read_json(matrix_manifest_member$payload, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(matrix_manifest) || !identical(as.character(matrix_manifest$status), "complete") ||
      any(!vapply(names(matrices), function(block_id) {
        is.list(matrix_manifest$blocks[[block_id]]) &&
          identical(as.character(matrix_manifest$blocks[[block_id]]$matrix_hash),
                    triomics_b07_matrix_hash(block_id, matrices[[block_id]]))
      }, logical(1L)))) return(FALSE)
  TRUE
}

triomics_b07_resume_candidate <- function(bundle_path) {
  if (!is.character(bundle_path) || length(bundle_path) != 1L || is.na(bundle_path) || !nzchar(bundle_path)) {
    triomics_b07_abort("B_EXPORT_CACHE", "A bundle path is required for restart validation.", "bundle_path",
                       "Pass the finalized bundle payload path.")
  }
  envelope_path <- sub("\\.json$", ".envelope.json", bundle_path)
  payload_present <- file.exists(bundle_path) && !dir.exists(bundle_path)
  envelope_present <- file.exists(envelope_path) && !dir.exists(envelope_path)
  payload_sha256 <- if (payload_present) {
    tryCatch(triomics_sha256(bundle_path), error = function(error) NULL)
  } else {
    NULL
  }
  payload <- if (payload_present && !triomics_is_symlink(bundle_path)) {
    tryCatch(jsonlite::read_json(bundle_path, simplifyVector = FALSE), error = function(error) NULL)
  } else {
    NULL
  }
  payload_readable <- is.list(payload) && !is.null(names(payload))
  envelope <- if (envelope_present && !triomics_is_symlink(envelope_path)) {
    tryCatch(jsonlite::read_json(envelope_path, simplifyVector = FALSE), error = function(error) NULL)
  } else {
    NULL
  }
  manifest_payload_sha <- if (is.list(envelope) && !is.null(envelope$payload_sha256)) {
    as.character(envelope$payload_sha256)
  } else {
    NULL
  }
  payload_hash_matches <- length(payload_sha256) == 1L && length(manifest_payload_sha) == 1L &&
    !is.na(payload_sha256) && !is.na(manifest_payload_sha) &&
    identical(payload_sha256, manifest_payload_sha)

  base_envelope_valid <- FALSE
  if (envelope_present && is.list(envelope) && !is.null(names(envelope)) &&
      payload_present && !triomics_is_symlink(bundle_path) &&
      length(payload_sha256) == 1L && !is.na(payload_sha256) &&
      length(manifest_payload_sha) == 1L && !is.na(manifest_payload_sha) &&
      isTRUE(triomics_sha256_text(manifest_payload_sha))) {
    if (isTRUE(payload_hash_matches)) {
      base_envelope_valid <- isTRUE(triomics_validate_envelope(envelope_path))
    } else {
      schema_probe_dir <- tempfile(pattern = ".b-t07-envelope-schema-", tmpdir = tempdir())
      if (dir.create(schema_probe_dir, recursive = FALSE, showWarnings = FALSE)) {
        on.exit(unlink(schema_probe_dir, recursive = TRUE), add = TRUE)
        schema_probe_path <- file.path(schema_probe_dir, basename(envelope_path))
        schema_probe_payload_path <- file.path(schema_probe_dir, basename(bundle_path))
        schema_probe <- envelope
        schema_probe$payload_sha256 <- payload_sha256
        base_envelope_valid <- tryCatch({
          if (!isTRUE(file.copy(bundle_path, schema_probe_payload_path))) {
            FALSE
          } else {
            jsonlite::write_json(schema_probe, schema_probe_path, auto_unbox = TRUE,
                                 null = "null", pretty = TRUE, digits = NA)
            isTRUE(triomics_validate_envelope(schema_probe_path))
          }
        }, error = function(error) FALSE)
      }
    }
  }

  member_evidence_valid <- is.list(payload) &&
    isTRUE(triomics_b07_validate_bundle_members(payload, bundle_path))
  late_evidence_valid <- is.list(payload) && envelope_present &&
    isTRUE(triomics_b07_validate_late_artifacts(payload, bundle_path, envelope_path))
  list(
    status = if (is.list(payload) && !is.null(payload$status)) as.character(payload$status) else NULL,
    cache_key = if (is.list(payload) && !is.null(payload$cache_key)) as.character(payload$cache_key) else NULL,
    payload_sha256 = payload_sha256,
    manifest_payload_sha256 = manifest_payload_sha,
    payload_present = payload_present,
    envelope_present = envelope_present,
    payload_readable = payload_readable,
    base_envelope_valid = base_envelope_valid,
    payload_hash_matches = payload_hash_matches,
    required_evidence_complete = isTRUE(member_evidence_valid) && isTRUE(late_evidence_valid)
  )
}

triomics_b07_validate_late_artifacts <- function(bundle, bundle_path, bundle_envelope_path) {
  handoff_dir <- dirname(bundle_path)
  reproduction_path <- file.path(handoff_dir, "reproduction.md")
  reproduction_envelope_path <- file.path(handoff_dir, "reproduction.envelope.json")
  record_path <- file.path(handoff_dir, "acceptance-record.json")
  record_envelope_path <- file.path(handoff_dir, "acceptance-record.envelope.json")
  provenance_path <- file.path(handoff_dir, "stage-handoff-provenance.json")
  provenance_envelope_path <- file.path(handoff_dir, "stage-handoff-provenance.envelope.json")
  paths <- c(reproduction_path, reproduction_envelope_path, record_path, record_envelope_path,
             provenance_path, provenance_envelope_path)
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
    }, logical(1))) &&
    isTRUE(triomics_b07_validate_provenance_artifacts(bundle, bundle_path, bundle_envelope_path))
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
  triomics_source_revision()
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
    paste0("- ", item$id, ": `", item$verdict, "` \u2014 ", item$detail)
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
                                         matrix_source = NULL, provenance_context = NULL) {
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
  if (!is.null(provenance_context)) {
    triomics_b07_validate_provenance_context(
      provenance_context, config_sha256, artifact, matrices = matrices
    )
    if (!identical(matrix_source, "stage_b_resolved_blocks")) {
      triomics_b07_abort("B_EXPORT_MATRIX", "Native B provenance requires the exact same-run matrix source designation.",
                         "matrix_source", "Use only matrices returned by this Stage B fit run.")
    }
    native_source_revision <- triomics_source_revision()
    if (!is.character(native_source_revision) || length(native_source_revision) != 1L ||
        is.na(native_source_revision) ||
        !grepl("^(?:[0-9a-f]{40}|[0-9a-f]{64})$", native_source_revision) ||
        (!is.null(working_ref) &&
         (!is.character(working_ref) || length(working_ref) != 1L || is.na(working_ref) ||
          !identical(working_ref, native_source_revision)))) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE",
        "Native B provenance requires the executing package source revision; a supplied working_ref must match it.",
        "working_ref",
        "Use the immutable source identity resolved from the executing package, not a caller-supplied revision claim."
      )
    }
    working_ref <- native_source_revision
  }
  block_results <- artifact$payload
  entries <- triomics_b07_claim_entries(block_results)
  ledger <- triomics_b07_family_ledger(block_results)
  nominations <- triomics_b07_nominations(block_results)
  accounting <- triomics_b07_row_accounting(entries)
  method_dispositions <- triomics_b07_method_dispositions(block_results)

  clinical_specs <- triomics_b07_clinical_artifact_specs()
  clinical_artifacts <- list()
  clinical_enabled <- is.list(config$clinical) && isTRUE(config$clinical$enabled)
  for (name in names(clinical_specs)) {
    spec <- clinical_specs[[name]]
    item <- triomics_b07_verified_artifact(
      root, spec$path, spec$type, config_sha256 = config_sha256,
      study_id = as.character(block_results$study_id), optional = TRUE
    )
    if (is.null(item)) {
      next
    } else {
      clinical_artifacts[[name]] <- item
    }
  }
  clinical_accounting <- triomics_b07_clinical_output_accounting(
    config, as.character(block_results$analysis_id), clinical_artifacts
  )
  clinical_dispositions <- clinical_accounting$dispositions

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
  dependency_hashes <- stats::setNames(
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
  missing_requested_clinical <- clinical_enabled && !isTRUE(clinical_accounting$all_accounted)
  missing_requested_clinical_artifact <- any(vapply(clinical_dispositions, function(item) {
    identical(item$status, "unavailable") && identical(item$reason, "requested_artifact_missing")
  }, logical(1L)))
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
  if (identical(as.character(block_results$study_id), "triomics_b_t04_fixture") &&
      is.list(provenance_context$synthetic_producer)) {
    calibration_path <- triomics_b07_child_path(root, "calibration.json")
    calibration_sha256 <- triomics_sha256(calibration_path)
    if (!identical(as.character(provenance_context$synthetic_producer$calibration$payload_sha256),
                   calibration_sha256)) {
      triomics_b07_abort("B_EXPORT_INCOMPLETE", "The retained B-T04 calibration bytes changed before bundle finalization.",
                         "calibration", "Rerun the native B-T04 producer and preserve the exact calibration payload bytes.")
    }
    bundle_inputs <- c(bundle_inputs, list(list(
      artifact_id = "b-t04-calibration",
      sha256 = calibration_sha256
    )))
  }
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
      if (missing_requested_clinical_artifact) "requested_clinical_artifact_missing",
      if (missing_requested_clinical && !missing_requested_clinical_artifact) "clinical_objective_evidence_unproven"
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
         detail = if (!missing_requested_clinical) {
           "Public block rows, full family membership, nomination lineage and clinical dispositions are indexed."
         } else if (missing_requested_clinical_artifact) {
           "A requested clinical artifact has no verified payload/envelope."
         } else {
           "A declared clinical objective lacks unique, consistent registry, result and family evidence."
         }),
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
    working_ref = if (is.null(working_ref)) triomics_b07_git_ref(getwd()) else as.character(working_ref),
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
  provenance_artifact <- NULL
  if (!is.null(provenance_context)) {
    provenance_artifact <- triomics_b07_write_provenance(
      provenance_context, config_sha256, bundle, bundle_path, bundle_envelope_path,
      artifact, family_artifact, nominations_artifact, matrix_artifact, matrix_bindings,
      clinical_artifacts, clinical_dispositions,
      repository_commit = native_source_revision,
      privacy_class = as.character(artifact$envelope$privacy_class)
    )
  }
  candidate_after_write <- triomics_b07_resume_candidate(bundle_path)
  candidate_valid <- isTRUE(candidate_after_write$payload_present) &&
    isTRUE(candidate_after_write$payload_readable) &&
    isTRUE(candidate_after_write$envelope_present) &&
    isTRUE(candidate_after_write$base_envelope_valid) &&
    isTRUE(candidate_after_write$payload_hash_matches) &&
    identical(candidate_after_write$status, bundle_status) &&
    (!identical(bundle_status, "complete") ||
       isTRUE(candidate_after_write$required_evidence_complete))
  if (!candidate_valid || !identical(candidate_after_write$cache_key, cache_key)) {
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
    provenance_path = if (is.null(provenance_artifact)) NULL else provenance_artifact$path,
    provenance_envelope_path = if (is.null(provenance_artifact)) NULL else provenance_artifact$envelope_path,
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
  if (is.list(stage_b_result) &&
      identical(as.character(stage_b_result$status), "unavailable") &&
      identical(as.character(stage_b_result$method_status), "unavailable")) {
    root <- triomics_b07_output_root(options$output, create = FALSE)
    config <- triomics_read_config(options$config)
    artifact <- triomics_b07_verified_artifact(
      root, "B/block-results.json", "block_results",
      config_sha256 = as.character(attr(config, "config_sha256"))
    )
    result_payload <- artifact$payload
    stage_result_path_matches <- is.character(stage_b_result$payload_path) &&
      length(stage_b_result$payload_path) == 1L && !is.na(stage_b_result$payload_path) &&
      nzchar(stage_b_result$payload_path) && tryCatch(
        identical(
          normalizePath(stage_b_result$payload_path, winslash = "/", mustWork = TRUE),
          artifact$payload_path
        ),
        error = function(error) FALSE
      )
    method_dispositions <- triomics_b07_method_dispositions(result_payload)
    has_unavailable_disposition <- any(vapply(method_dispositions, function(item) {
      identical(as.character(item$status), "unavailable")
    }, logical(1)))
    if (isTRUE(stage_result_path_matches) &&
        identical(as.character(result_payload$status), "unavailable") &&
        identical(as.character(result_payload$method_status), "unavailable") &&
        is.list(result_payload$blocks) && !length(result_payload$blocks) &&
        has_unavailable_disposition) {
      return(list(
        status = "unavailable",
        reason = "no_exportable_public_claims",
        method_status = "unavailable",
        method_dispositions = method_dispositions,
        block_results_path = artifact$payload_path,
        handoff_path = NULL
      ))
    }
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
  if (!is.list(stage_b_result$provenance_context)) {
    triomics_b07_abort(
      "B_EXPORT_INCOMPLETE", "The Stage B runner did not supply its same-run native provenance context.",
      "provenance_context",
      "The B-T07 post-stage hook must receive the in-memory provenance context from triomics_execute_stage_b()."
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
    working_ref = triomics_b07_git_ref(getwd()),
    cli_exit_code = 0L,
    matrix_source = "stage_b_resolved_blocks",
    provenance_context = stage_b_result$provenance_context
  )
}
