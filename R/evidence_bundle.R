triomics_d_abort <- function(code, message, key = NULL, remediation = NULL) {
  if (is.null(remediation)) {
    remediation <- "Recreate the D input from its verified producer and keep study identities and provenance aligned."
  }
  triomics_abort(code, message, key = key, module = "annotation", stage = "D",
                 remediation = remediation)
}

triomics_d_nonempty_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

triomics_d_same_identity <- function(payload, field, expected, label) {
  value <- if (is.list(payload)) payload[[field]] else NULL
  if (!triomics_d_nonempty_string(value) || !triomics_d_nonempty_string(expected) ||
      !identical(value, expected)) {
    triomics_d_abort(
      "D_LINEAGE_MISMATCH", paste0(label, " has an absent or mismatched ", field, "."),
      key = paste0(label, ".", field)
    )
  }
  invisible(TRUE)
}

triomics_d_read_json <- function(path, key) {
  if (triomics_d_nonempty_string(path) && triomics_is_symlink(path)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", paste0("The D handoff path is a symbolic link: ", key, "."), key)
  }
  if (!triomics_d_nonempty_string(path) || !file.exists(path) || dir.exists(path)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", paste0("The verified C-06 handoff is missing ", key, "."), key)
  }
  value <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(value) || is.null(names(value))) {
    triomics_d_abort("D_PROVENANCE_INVALID", paste0("The C-06 handoff member is unreadable: ", key, "."), key)
  }
  value
}

triomics_d_child_path <- function(root, relative, key) {
  if (!triomics_d_nonempty_string(relative) ||
      grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\)", relative)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A producer descriptor names an unsafe path.", key)
  }
  parts <- strsplit(relative, "[/\\\\]+")[[1L]]
  parts <- parts[nzchar(parts)]
  if (!length(parts) || any(parts %in% c(".", ".."))) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A producer descriptor names an unsafe path.", key)
  }
  current <- root
  for (part in parts) {
    current <- file.path(current, part)
    if (triomics_is_symlink(current)) {
      triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A producer handoff path crosses a symbolic link.", key)
    }
  }
  if (!file.exists(current) || dir.exists(current)) return(NULL)
  resolved <- normalizePath(current, winslash = "/", mustWork = TRUE)
  prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  if (!startsWith(resolved, prefix)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A producer handoff path escapes its output root.", key)
  }
  resolved
}

triomics_d_read_c06_handoff <- function(handoff_dir, expected_study_id, expected_analysis_id) {
  if (!triomics_d_nonempty_string(handoff_dir) || !dir.exists(handoff_dir) ||
      triomics_is_symlink(handoff_dir)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "The C-06 D handoff directory is unavailable.", "handoff_dir")
  }
  root <- normalizePath(handoff_dir, winslash = "/", mustWork = TRUE)
  manifest_path <- file.path(root, "handoff-manifest.json")
  envelope_path <- file.path(root, "handoff-manifest.envelope.json")
  manifest_json <- if (file.exists(manifest_path) && !dir.exists(manifest_path)) {
    tryCatch(readChar(manifest_path, nchars = file.info(manifest_path)$size, useBytes = TRUE),
             error = function(error) NULL)
  } else NULL
  if (triomics_is_symlink(manifest_path) || triomics_is_symlink(envelope_path)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "The C-06 handoff manifest path crosses a symbolic link.",
                     "handoff_manifest")
  }
  if (!file.exists(manifest_path) || dir.exists(manifest_path) ||
      !file.exists(envelope_path) || dir.exists(envelope_path)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "The C-06 handoff manifest or envelope is missing.",
                     "handoff_manifest")
  }
  if (!is.character(manifest_json) ||
      !isTRUE(tryCatch({
        triomics_provenance_json_scan(manifest_json)
        TRUE
      }, error = function(error) FALSE)) ||
      !isTRUE(triomics_validate_envelope(envelope_path))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The C-06 handoff manifest or its envelope is invalid.",
                     "handoff_manifest")
  }
  manifest <- triomics_d_read_json(manifest_path, "handoff-manifest.json")
  envelope <- triomics_d_read_json(envelope_path, "handoff-manifest.envelope.json")
  required_types <- c(
    candidate_nominations = "candidate_nominations",
    integration_evidence = "integration_evidence_bundle",
    integration_comparisons = "integration_comparisons",
    evaluation_evidence = "integration_evaluation_evidence",
    stage_status = "stage_c_status"
  )
  if (!identical(manifest$schema_version, "2.0") ||
      !identical(manifest$artifact_type, "stage_c_handoff_manifest") ||
      !identical(envelope$artifact_type, "stage_c_handoff_manifest") ||
      !identical(envelope$artifact_id, "c06-handoff-manifest") ||
      !identical(envelope$study_id, manifest$study_id) ||
      !identical(envelope$analysis_id, manifest$analysis_id) ||
      !identical(envelope$preparation_context_id, manifest$preparation_context_id) ||
      !identical(envelope$representation_id, manifest$representation_id) ||
      !identical(as.character(envelope$status), as.character(manifest$status))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The C-06 manifest identity, type, or status is inconsistent.",
                     "handoff_manifest")
  }
  triomics_d_same_identity(manifest, "study_id", expected_study_id, "C-06 manifest")
  triomics_d_same_identity(manifest, "analysis_id", expected_analysis_id, "C-06 manifest")
  if (!is.list(manifest$artifacts) || length(manifest$artifacts) != length(required_types)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The C-06 manifest does not account for every required handoff member.",
                     "handoff_manifest.artifacts")
  }
  seen_types <- vapply(manifest$artifacts, function(item) {
    if (!is.list(item) || is.null(item$artifact_type)) "" else as.character(item$artifact_type)
  }, character(1))
  if (anyDuplicated(seen_types) || !setequal(seen_types, unname(required_types))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The C-06 manifest member set is incomplete or duplicated.",
                     "handoff_manifest.artifacts")
  }
  triomics_d_same_identity(manifest, "preparation_context_id", manifest$preparation_context_id, "C-06 manifest")
  triomics_d_same_identity(manifest, "representation_id", manifest$representation_id, "C-06 manifest")

  payloads <- vector("list", length(required_types))
  names(payloads) <- names(required_types)
  artifact_refs <- list()
  for (artifact in manifest$artifacts) {
    artifact_type <- as.character(artifact$artifact_type)
    output_name <- names(required_types)[match(artifact_type, unname(required_types))]
    payload_path <- triomics_d_child_path(root, as.character(artifact$payload_file),
                                          paste0(output_name, ".payload_file"))
    member_envelope_path <- triomics_d_child_path(root, as.character(artifact$envelope_file),
                                                  paste0(output_name, ".envelope_file"))
    if (is.null(payload_path) || is.null(member_envelope_path) ||
        !triomics_d_nonempty_string(artifact$payload_sha256) ||
        !triomics_d_nonempty_string(artifact$envelope_sha256) ||
        !identical(triomics_sha256(payload_path), as.character(artifact$payload_sha256)) ||
        !identical(triomics_sha256(member_envelope_path), as.character(artifact$envelope_sha256)) ||
        !isTRUE(triomics_validate_envelope(member_envelope_path))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C-06 handoff payload or envelope failed hash verification.",
                       paste0("handoff_manifest.", output_name))
    }
    member <- triomics_d_read_json(payload_path, basename(payload_path))
    member_envelope <- triomics_d_read_json(member_envelope_path, basename(member_envelope_path))
    if (!identical(as.character(member$artifact_type), artifact_type) ||
        !identical(as.character(member_envelope$artifact_type), artifact_type) ||
        !identical(as.character(member_envelope$payload_sha256), as.character(artifact$payload_sha256)) ||
        !identical(as.character(member_envelope$study_id), as.character(manifest$study_id)) ||
        !identical(as.character(member_envelope$analysis_id), as.character(manifest$analysis_id)) ||
        !identical(as.character(member_envelope$preparation_context_id), as.character(manifest$preparation_context_id)) ||
        !identical(as.character(member_envelope$representation_id), as.character(manifest$representation_id))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C-06 member type or envelope identity is inconsistent.",
                       paste0("handoff_manifest.", output_name))
    }
    triomics_d_same_identity(member, "study_id", expected_study_id, output_name)
    triomics_d_same_identity(member, "analysis_id", expected_analysis_id, output_name)
    if (!identical(as.character(member$preparation_context_id), as.character(manifest$preparation_context_id)) ||
        !identical(as.character(member$representation_id), as.character(manifest$representation_id))) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "A C-06 member has different preparation or representation identity.",
                       paste0("handoff_manifest.", output_name))
    }
    payloads[[output_name]] <- member
    artifact_refs[[output_name]] <- list(
      artifact_type = artifact_type,
      artifact_id = as.character(member_envelope$artifact_id),
      payload_file = as.character(artifact$payload_file),
      payload_sha256 = as.character(artifact$payload_sha256),
      envelope_file = as.character(artifact$envelope_file),
      envelope_sha256 = as.character(artifact$envelope_sha256)
    )
  }
  for (artifact in manifest$artifacts) {
    artifact_id <- paste0("c06-", sub("[.]json$", "", artifact$payload_file))
    if (!any(vapply(envelope$inputs, function(input) {
      identical(as.character(input$artifact_id), artifact_id) &&
        identical(as.character(input$sha256), as.character(artifact$payload_sha256))
    }, logical(1)))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "The C-06 manifest envelope does not bind every declared member payload.",
                       paste0("handoff_manifest.", artifact_id))
    }
  }
  controls <- payloads$evaluation_evidence$controls
  expected_controls <- c("null", "weak_signal", "strong_signal", "adversarial")
  control_names <- if (is.list(controls)) {
    vapply(controls, function(control) as.character(control$control %||% ""), character(1))
  } else {
    character()
  }
  if (!identical(control_names, expected_controls)) {
    triomics_d_abort("D_EVALUATION_EVIDENCE_INVALID", "C-07 control records are missing or reordered.",
                     "evaluation_evidence.controls")
  }
  clinical <- payloads$stage_status$clinical_prediction
  if (!is.list(clinical) || !identical(as.character(clinical$completion_policy), "conditional")) {
    triomics_d_abort("D_CLINICAL_EVIDENCE_INVALID", "C-09 clinical prediction must remain conditionally scoped.",
                     "stage_status.clinical_prediction")
  }
  list(
    study_id = as.character(manifest$study_id),
    analysis_id = as.character(manifest$analysis_id),
    preparation_context_id = as.character(manifest$preparation_context_id),
    representation_id = as.character(manifest$representation_id),
    manifest = manifest,
    manifest_artifact_id = as.character(envelope$artifact_id),
    manifest_sha256 = triomics_sha256(manifest_path),
    manifest_envelope_sha256 = triomics_sha256(envelope_path),
    artifact_refs = artifact_refs,
    candidate_nominations = payloads$candidate_nominations,
    integration_evidence = payloads$integration_evidence,
    integration_comparisons = payloads$integration_comparisons,
    evaluation_evidence = payloads$evaluation_evidence,
    stage_status = payloads$stage_status
  )
}

triomics_d_read_b_handoff <- function(output_root, candidate_ref,
                                     expected_study_id, expected_analysis_id,
                                     expected_preparation_context_id = NULL,
                                     expected_representation_id = NULL) {
  if (!triomics_d_nonempty_string(output_root) || !dir.exists(output_root) ||
      triomics_is_symlink(output_root) || !is.list(candidate_ref) ||
      !triomics_d_nonempty_string(candidate_ref$artifact_id) ||
      !triomics_sha256_text(as.character(candidate_ref$sha256))) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "D needs a local B output root and the exact C-06 nomination artifact reference.",
                     "b_handoff")
  }
  expected_candidate_ref <- candidate_ref
  root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
  handoff_root <- file.path(root, "B", "handoffs")
  if (!dir.exists(handoff_root) || triomics_is_symlink(handoff_root)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "The B-T07 public handoff directory is unavailable.", "B/handoffs")
  }
  bundle_files <- list.files(handoff_root, pattern = "^bundle[.]json$", recursive = TRUE,
                             full.names = TRUE, all.files = TRUE)
  matches <- list()
  for (listed_path in bundle_files) {
    relative_bundle <- substring(normalizePath(listed_path, winslash = "/", mustWork = FALSE),
                                 nchar(root) + 2L)
    bundle_path <- triomics_d_child_path(root, relative_bundle, "B bundle")
    if (is.null(bundle_path)) next
    bundle_envelope_path <- sub("[.]json$", ".envelope.json", bundle_path)
    if (!isTRUE(triomics_validate_envelope(bundle_envelope_path))) next
    bundle <- tryCatch(jsonlite::read_json(bundle_path, simplifyVector = FALSE), error = function(error) NULL)
    bundle_envelope <- tryCatch(jsonlite::read_json(bundle_envelope_path, simplifyVector = FALSE),
                                error = function(error) NULL)
    if (!is.list(bundle) || !is.list(bundle_envelope) ||
        !identical(bundle$artifact_type, "stage_b_public_bundle") ||
        !identical(bundle_envelope$artifact_type, "stage_b_public_bundle") ||
        !identical(as.character(bundle$artifact_id), as.character(bundle_envelope$artifact_id)) ||
        !identical(as.character(bundle$study_id), expected_study_id) ||
        !identical(as.character(bundle$analysis_id), expected_analysis_id) ||
        !identical(as.character(bundle_envelope$study_id), expected_study_id) ||
        !identical(as.character(bundle_envelope$status), as.character(bundle$status)) ||
        !identical(as.character(bundle_envelope$payload_sha256), triomics_sha256(bundle_path))) {
      next
    }
    candidate_members <- Filter(function(member) {
      is.list(member) && identical(as.character(member$artifact_id), "candidate_nominations") &&
        identical(as.character(member$sha256), as.character(candidate_ref$sha256))
    }, bundle$members)
    if (length(candidate_members)) {
      matches[[length(matches) + 1L]] <- list(
        bundle = bundle, bundle_path = bundle_path,
        bundle_envelope = bundle_envelope, bundle_envelope_path = bundle_envelope_path,
        candidate_member = candidate_members[[1L]]
      )
    }
  }
  if (length(matches) != 1L) {
    triomics_d_abort(
      if (length(matches)) "D_LINEAGE_MISMATCH" else "D_PROVENANCE_REQUIRED",
      "The C-06 nomination reference does not resolve to exactly one verified B-T07 public bundle.",
      "candidate_nominations.source_artifact_ref"
    )
  }
  match <- matches[[1L]]
  bundle <- match$bundle
  member_types <- c(block_results = "block_results", family_ledger = "family_ledger",
                    candidate_nominations = "candidate_nominations")
  payloads <- list()
  artifact_refs <- list()
  for (member_name in names(member_types)) {
    member_id <- unname(member_types[[member_name]])
    found <- Filter(function(member) {
      is.list(member) && identical(as.character(member$artifact_id), member_id)
    }, bundle$members)
    if (length(found) != 1L) {
      triomics_d_abort("D_PROVENANCE_INVALID", "The B-T07 public bundle has a missing or duplicated required member.",
                       paste0("B.bundle.", member_name))
    }
    member <- found[[1L]]
    payload_path <- triomics_d_child_path(root, as.character(member$path), paste0(member_name, ".path"))
    envelope_path <- triomics_d_child_path(root, as.character(member$envelope_path),
                                           paste0(member_name, ".envelope_path"))
    if (is.null(payload_path) || is.null(envelope_path) ||
        !triomics_sha256_text(as.character(member$sha256)) ||
        !triomics_sha256_text(as.character(member$envelope_sha256)) ||
        !identical(triomics_sha256(payload_path), as.character(member$sha256)) ||
        !identical(triomics_sha256(envelope_path), as.character(member$envelope_sha256)) ||
        !isTRUE(triomics_validate_envelope(envelope_path))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A B-T07 bundle member failed payload or envelope verification.",
                       paste0("B.bundle.", member_name))
    }
    payload <- triomics_d_read_json(payload_path, paste0("B/", member_name))
    member_envelope <- triomics_d_read_json(envelope_path, paste0("B/", member_name, ".envelope"))
    payload_type_valid <- if (identical(member_name, "block_results")) {
      is.null(payload$artifact_type) || identical(as.character(payload$artifact_type), member_id)
    } else {
      identical(as.character(payload$artifact_type), member_id)
    }
    if (!isTRUE(payload_type_valid) ||
        !identical(as.character(member$artifact_id), member_id) ||
        !identical(as.character(member_envelope$artifact_type), member_id) ||
        !identical(as.character(member_envelope$study_id), expected_study_id) ||
        !identical(as.character(member_envelope$payload_sha256), as.character(member$sha256)) ||
        !identical(as.character(payload$status), as.character(member$status))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A B-T07 member type, status, or envelope identity is inconsistent.",
                       paste0("B.bundle.", member_name))
    }
    triomics_d_same_identity(payload, "study_id", expected_study_id, paste0("B ", member_name))
    triomics_d_same_identity(payload, "analysis_id", expected_analysis_id, paste0("B ", member_name))
    payloads[[member_name]] <- payload
    artifact_refs[[member_name]] <- list(
      artifact_type = member_id,
      artifact_id = as.character(member_envelope$artifact_id),
      sha256 = as.character(member$sha256),
      envelope_sha256 = as.character(member$envelope_sha256),
      path = as.character(member$path),
      envelope_path = as.character(member$envelope_path),
      envelope_inputs = member_envelope$inputs
    )
  }
  if (!identical(as.character(artifact_refs$candidate_nominations$artifact_id),
                 as.character(expected_candidate_ref$artifact_id)) ||
      !identical(as.character(artifact_refs$candidate_nominations$sha256),
                 as.character(expected_candidate_ref$sha256)) ||
      !identical(as.character(match$candidate_member$sha256), as.character(expected_candidate_ref$sha256))) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "The B candidate nomination reference does not match the C-06 immutable input.",
                     "candidate_nominations.source_artifact_ref")
  }
  preparation_context_id <- bundle$preparation_context_id
  representation_id <- bundle$representation_id
  block_results <- payloads$block_results
  if (!triomics_d_nonempty_string(preparation_context_id) ||
      !triomics_d_nonempty_string(representation_id) ||
      !identical(block_results$preparation_context_id, preparation_context_id) ||
      !identical(block_results$representation_id, representation_id) ||
      (triomics_d_nonempty_string(expected_preparation_context_id) &&
       !identical(preparation_context_id, expected_preparation_context_id)) ||
      (triomics_d_nonempty_string(expected_representation_id) &&
       !identical(representation_id, expected_representation_id))) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "B-T07 and its source block results do not match the C preparation context and representation.",
                     "B.bundle.preparation_context_id")
  }
  expected_block_ref <- list(artifact_id = "stage-b-block-results",
                             sha256 = artifact_refs$block_results$sha256)
  for (derived_name in c("family_ledger", "candidate_nominations")) {
    has_source_edge <- any(vapply(artifact_refs[[derived_name]]$envelope_inputs, function(input) {
      identical(as.character(input$artifact_id), expected_block_ref$artifact_id) &&
        identical(as.character(input$sha256), expected_block_ref$sha256)
    }, logical(1)))
    if (!has_source_edge) {
    triomics_d_abort("D_PROVENANCE_INVALID", "A derived B-T07 artifact is not hash-bound to its source block results.",
                       paste0("B.bundle.", derived_name, ".source"))
    }
  }
  list(
    study_id = expected_study_id, analysis_id = expected_analysis_id,
    bundle = bundle, bundle_path = match$bundle_path,
    preparation_context_id = preparation_context_id,
    representation_id = representation_id,
    bundle_sha256 = triomics_sha256(match$bundle_path),
    bundle_artifact_id = as.character(match$bundle_envelope$artifact_id),
    bundle_envelope_sha256 = triomics_sha256(match$bundle_envelope_path),
    block_results = payloads$block_results,
    family_ledger = payloads$family_ledger,
    candidate_nominations = payloads$candidate_nominations,
    artifact_refs = artifact_refs
  )
}

triomics_d_profile_entities <- function(profile, role) {
  entities <- profile$identity$entities
  if (!is.list(entities)) return(list())
  Filter(function(entity) identical(as.character(entity$scientific_role), role), entities)
}

triomics_d_profile_file <- function(path, key) {
  if (triomics_d_nonempty_string(path) && triomics_is_symlink(path)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "A producer provenance path is a symbolic link.", key)
  }
  if (!triomics_d_nonempty_string(path) || !file.exists(path) || dir.exists(path)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "A producer provenance payload or envelope is missing.", key)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

triomics_d_read_producer_profile <- function(payload_path, envelope_path, stage,
                                             payload_id, envelope_id, study_id,
                                             analysis_id) {
  payload_path <- triomics_d_profile_file(payload_path, paste0(stage, ".provenance"))
  envelope_path <- triomics_d_profile_file(envelope_path, paste0(stage, ".provenance_envelope"))
  if (!isTRUE(triomics_validate_envelope(envelope_path))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "A producer provenance envelope failed validation.",
                     paste0(stage, ".provenance_envelope"))
  }
  profile_json <- tryCatch(readChar(payload_path, nchars = file.info(payload_path)$size,
                                   useBytes = TRUE),
                           error = function(error) NULL)
  profile <- if (is.character(profile_json)) {
    tryCatch(jsonlite::fromJSON(profile_json, simplifyVector = FALSE),
             error = function(error) NULL)
  } else NULL
  envelope <- tryCatch(jsonlite::read_json(envelope_path, simplifyVector = FALSE),
                       error = function(error) NULL)
  validator_ids <- if (identical(stage, "B")) {
    c("b-t07-stage-handoff-provenance", "b-t07-stage-handoff-provenance-envelope")
  } else {
    c("c06-stage-handoff-provenance", "c06-stage-handoff-provenance-envelope")
  }
  if (!is.list(profile) || !is.list(envelope) ||
      !identical(as.character(envelope$artifact_type), "stage_handoff_provenance") ||
      !identical(as.character(envelope$artifact_id), envelope_id) ||
      !identical(as.character(envelope$study_id), study_id) ||
      !identical(as.character(envelope$run_id), as.character(profile$run_id)) ||
      !identical(as.character(envelope$status), "complete") ||
      !identical(as.character(envelope$payload_sha256), triomics_sha256(payload_path)) ||
      !identical(as.character(profile$producer_stage), stage) ||
      !identical(as.character(profile$study_id), study_id) ||
      !identical(as.character(profile$handoff_artifact_id), payload_id) ||
      !isTRUE(tryCatch(triomics_validate_stage_handoff_provenance(
        profile_json, validator_ids[[1L]], validator_ids[[2L]]
      ), error = function(error) FALSE))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "A producer profile is not a valid, byte-bound B/C handoff identity.",
                     paste0(stage, ".provenance"))
  }
  if (!is.null(envelope$analysis_id) &&
      !identical(as.character(envelope$analysis_id), analysis_id)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "A producer provenance profile has mismatched analysis identity.",
                     paste0(stage, ".provenance.analysis_id"))
  }
  list(
    profile = profile, envelope = envelope,
    path = payload_path, envelope_path = envelope_path,
    payload_sha256 = triomics_sha256(payload_path),
    envelope_sha256 = triomics_sha256(envelope_path)
  )
}

triomics_d_validate_profile_artifact <- function(entity, payload_path, envelope_path,
                                                 artifact_id, key) {
  payload_path <- triomics_d_profile_file(payload_path, paste0(key, ".payload"))
  envelope_path <- triomics_d_profile_file(envelope_path, paste0(key, ".envelope"))
  if (!isTRUE(triomics_b07_profile_artifact_matches(
    entity, payload_path, envelope_path, artifact_id
  ))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "A persisted producer artifact differs from its provenance descriptor.",
                     key)
  }
  invisible(TRUE)
}

triomics_d_validate_predecessor_pair <- function(profile, payload_path,
                                                 envelope_path, artifact_id,
                                                 key) {
  envelopes <- triomics_d_profile_entities(profile, "predecessor_envelope")
  payloads <- triomics_d_profile_entities(profile, "predecessor_payload")
  envelope_hits <- Filter(function(entity) {
    identical(as.character(entity$artifact_id), artifact_id)
  }, envelopes)
  if (length(envelope_hits) != 1L) {
    triomics_d_abort("D_PROVENANCE_INVALID", "C did not persist one exact B predecessor envelope.",
                     key)
  }
  payload_hits <- Filter(function(entity) {
    identical(as.character(entity$member_of_entity_id),
              as.character(envelope_hits[[1L]]$entity_id)) &&
      identical(as.character(entity$digest), triomics_sha256(payload_path))
  }, payloads)
  if (length(payload_hits) != 1L ||
      !identical(as.character(envelope_hits[[1L]]$digest), triomics_sha256(envelope_path)) ||
      !identical(as.character(envelope_hits[[1L]]$artifact_payload_sha256), triomics_sha256(payload_path))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "C did not bind the exact B predecessor payload/envelope bytes.",
                     key)
  }
  invisible(TRUE)
}

triomics_d_compare_profile_identity <- function(b_profile, c_profile, role) {
  b_entities <- triomics_d_profile_entities(b_profile, role)
  c_entities <- triomics_d_profile_entities(c_profile, role)
  b_ids <- vapply(b_entities, function(entity) as.character(entity$entity_id), character(1))
  c_ids <- vapply(c_entities, function(entity) as.character(entity$entity_id), character(1))
  if (!length(b_ids) || anyDuplicated(b_ids) || !all(b_ids %in% c_ids) ||
      any(!vapply(b_ids, function(id) {
        left <- b_entities[[match(id, b_ids)]]
        right <- c_entities[[match(id, c_ids)]]
        identical(as.character(left$digest), as.character(right$digest)) &&
          identical(as.character(left$artifact_payload_sha256 %||% ""),
                    as.character(right$artifact_payload_sha256 %||% "")) &&
          identical(as.character(left$artifact_id %||% ""),
                    as.character(right$artifact_id %||% "")) &&
          identical(as.character(left$storage_form), as.character(right$storage_form))
      }, logical(1)))) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "C does not preserve the exact inherited B identity descriptors.",
                     paste0("C.provenance.", role))
  }
  invisible(TRUE)
}

triomics_d_validate_c_profile_values <- function(root, b_handoff, c_handoff,
                                                 c_profile_ref) {
  profile <- c_profile_ref$profile
  for (spec in list(
    list(role = "candidate_nominations", type = "candidate_nominations",
         artifact_id = "c06-candidate-nominations"),
    list(role = "evaluation_evidence", type = "integration_evaluation_evidence",
         artifact_id = "c06-evaluation-evidence")
  )) {
    entities <- triomics_d_profile_entities(profile, spec$role)
    artifacts <- Filter(function(item) identical(as.character(item$artifact_type), spec$type),
                        c_handoff$manifest$artifacts)
    if (length(entities) != 1L || length(artifacts) != 1L) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A required C handoff member has no unique provenance descriptor.",
                       paste0("C.provenance.", spec$role))
    }
    payload_path <- triomics_d_child_path(
      file.path(root, "D"), as.character(artifacts[[1L]]$payload_file),
      paste0("C.", spec$role, ".payload")
    )
    envelope_path <- triomics_d_child_path(
      file.path(root, "D"), as.character(artifacts[[1L]]$envelope_file),
      paste0("C.", spec$role, ".envelope")
    )
    if (is.null(payload_path) || is.null(envelope_path)) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C provenance member path is unsafe.",
                       paste0("C.", spec$role))
    }
    triomics_d_validate_profile_artifact(
      entities[[1L]], payload_path, envelope_path,
      spec$artifact_id, paste0("C.", spec$role)
    )
  }

  matrix_members <- Filter(function(member) {
    identical(as.character(member$artifact_id), "stage_c_matrix_values")
  }, b_handoff$bundle$members)
  matrices <- if (length(matrix_members) == 1L) {
    matrix_path <- triomics_d_child_path(root, as.character(matrix_members[[1L]]$path),
                                         "B.matrix_values")
    if (is.null(matrix_path)) NULL else tryCatch(readRDS(matrix_path), error = function(error) NULL)
  } else NULL
  if (!is.list(matrices)) {
    triomics_d_abort("D_PROVENANCE_INVALID", "B represented matrices needed for C lineage are unavailable.",
                     "B.matrix_values")
  }

  integrations <- c_handoff$integration_evidence$integrations
  result_entities <- triomics_d_profile_entities(profile, "integration_result")
  state_entities <- triomics_d_profile_entities(profile, "integration_state")
  method_entities <- triomics_d_profile_entities(profile, "method_state")
  native_entities <- triomics_d_profile_entities(profile, "native_sidecar")
  preparation_entities <- triomics_d_profile_entities(profile, "preprocessing_state")
  expected_results <- expected_states <- expected_methods <- expected_native <- character()
  expected_preparation <- list()

  for (evidence in integrations) {
    integration_id <- as.character(evidence$integration_id)
    method_id <- as.character(evidence$backend %||% evidence$method %||% "")
    if (!triomics_d_nonempty_string(integration_id) || !triomics_d_nonempty_string(method_id)) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C integration lacks its method or route identity.",
                       "C.integration_evidence")
    }
    scope <- list(
      analysis_id = c_handoff$analysis_id,
      method_id = method_id,
      preparation_context_id = c_handoff$preparation_context_id,
      representation_id = c_handoff$representation_id
    )
    result_id <- triomics_b07_prov_entity_id("integration_result", scope)
    result_digest <- triomics_b07_prov_value_sha256(evidence)
    result <- Filter(function(entity) identical(as.character(entity$entity_id), result_id) &&
      identical(as.character(entity$digest), result_digest), result_entities)
    state_id <- triomics_b07_prov_entity_id("integration_state", scope)
    state_value <- list(
      integration_id = integration_id,
      method_id = method_id,
      status = as.character(evidence$status),
      reason_code = evidence$reason_code %||% NULL,
      reason = evidence$reason %||% NULL,
      native_artifact_ref = evidence$native_artifact_ref %||% NULL
    )
    method_id_entity <- triomics_b07_prov_entity_id("method_state", scope)
    method_value <- list(
      integration_id = integration_id,
      method_id = method_id,
      status = as.character(evidence$status),
      backend = as.character(evidence$backend),
      native_state_ref = evidence$native_artifact_ref %||% NULL
    )
    method_state <- Filter(function(entity) {
      identical(as.character(entity$entity_id), method_id_entity) &&
        identical(as.character(entity$digest), triomics_b07_prov_value_sha256(method_value))
    }, method_entities)
    if (length(result) != 1L || length(method_state) != 1L) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "C integration result/state/method values differ from profile bytes.",
                       paste0("C.integration.", integration_id))
    }
    expected_results <- c(expected_results, result_id)
    expected_methods <- c(expected_methods, method_id_entity)

    native_ref <- evidence$native_artifact_ref
    native_payload <- NULL
    if (identical(as.character(evidence$status), "complete") && is.list(native_ref)) {
    native_dir <- triomics_d_secure_path(
      file.path(root, "C", "native-states"), "D_OUTPUT_PATH_UNSAFE",
      "The C native-state directory is unsafe or crosses a symbolic link.",
      "C.native_sidecar.directory", directory = TRUE
    )
    root_prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
    if (!identical(native_dir, root) && !startsWith(native_dir, root_prefix)) {
      triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "The C native-state directory escapes its output root.",
                       "C.native_sidecar.directory")
    }
    native_files <- if (dir.exists(native_dir)) {
      list.files(native_dir, pattern = "^integration-[0-9]+[.]json$",
                 full.names = TRUE, all.files = FALSE)
    } else character()
    native_hits <- native_files[vapply(native_files, function(path) {
      relative_path <- file.path("C", "native-states", basename(path))
      safe_path <- triomics_d_child_path(root, relative_path,
                                         paste0("C.native_sidecar.", integration_id))
      if (is.null(safe_path)) return(FALSE)
      value <- tryCatch(jsonlite::read_json(safe_path, simplifyVector = FALSE),
                        error = function(error) NULL)
      is.list(value) &&
        identical(as.character(value$artifact_id), as.character(native_ref$artifact_id)) &&
        identical(triomics_sha256(safe_path), as.character(native_ref$sha256))
    }, logical(1))]
    if (length(native_hits) != 1L) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C native sidecar does not resolve to one exact persisted file.",
                       paste0("C.native_sidecar.", integration_id))
    }
    native_relative_path <- file.path("C", "native-states", basename(native_hits[[1L]]))
    native_path <- triomics_d_child_path(root, native_relative_path,
                                         paste0("C.native_sidecar.", integration_id))
    native_envelope_relative_path <- sub("[.]json$", ".envelope.json", native_relative_path)
    native_envelope_path <- triomics_d_child_path(
      root, native_envelope_relative_path,
      paste0("C.native_sidecar.", integration_id, ".envelope")
    )
    if (is.null(native_path) || is.null(native_envelope_path)) {
      triomics_d_abort("D_PROVENANCE_REQUIRED", "A C native sidecar payload or envelope is missing.",
                       paste0("C.native_sidecar.", integration_id))
    }
    native_payload <- triomics_d_read_json(native_path, paste0("C.native_sidecar.", integration_id))
    native_envelope <- triomics_d_read_json(
      native_envelope_path, paste0("C.native_sidecar.", integration_id, ".envelope")
    )
    if (!isTRUE(triomics_validate_envelope(native_envelope_path)) ||
        !identical(as.character(native_payload$integration_id), integration_id) ||
        !identical(as.character(native_payload$backend), method_id) ||
        !identical(as.character(native_envelope$payload_sha256), triomics_sha256(native_path)) ||
        !identical(as.character(native_envelope$config_sha256),
                   as.character(profile$producer$effective_configuration_sha256))) {
      triomics_d_abort("D_PROVENANCE_INVALID", "A C native sidecar has a mismatched payload or envelope identity.",
                       paste0("C.native_sidecar.", integration_id))
    }
    native_id <- triomics_b07_prov_entity_id("native_sidecar", integration_id)
    native_entity <- Filter(function(entity) identical(as.character(entity$entity_id), native_id) &&
      identical(as.character(entity$route_id), integration_id), native_entities)
    if (length(native_entity) != 1L) {
      triomics_d_abort("D_PROVENANCE_INVALID", "C profile does not bind one native sidecar per completed integration.",
                       paste0("C.provenance.native_sidecar.", integration_id))
    }
    triomics_d_validate_profile_artifact(
      native_entity[[1L]], native_path, native_envelope_path,
      as.character(native_ref$artifact_id), paste0("C.native_sidecar.", integration_id)
    )
    expected_native <- c(expected_native, native_id)

    prep_states <- tryCatch(
      triomics_c06_validate_native_preparation_inputs(
        evidence, native_payload, native_envelope, root
      ),
      error = function(error) NULL
    )
    if (is.null(prep_states)) {
      triomics_d_abort("D_PROVENANCE_INVALID", "C native preparation matrix or persisted state failed read-back.",
                       paste0("C.preparation_state.", integration_id))
    }
    for (block_id in names(prep_states)) {
      item <- prep_states[[block_id]]
      matrix_hashes <- evidence$identity$matrix_sha256
      if (is.null(matrices[[block_id]]) || is.null(matrix_hashes[[block_id]])) {
        triomics_d_abort("D_PROVENANCE_INVALID", "C preparation has no matching B matrix or fitted matrix hash.",
                         paste0("C.preparation_state.", block_id))
      }
      preparation_value <- list(
        producer_stage = "C",
        integration_id = integration_id,
        block_id = block_id,
        preparation_context_id = c_handoff$preparation_context_id,
        representation_id = c_handoff$representation_id,
        state_id = as.character(item$reference$state_id),
        state_hash = as.character(item$reference$state_hash),
        state_payload_sha256 = as.character(item$reference$payload_sha256),
        state_path = as.character(item$reference$path),
        input_matrix_sha256 = triomics_b07_matrix_hash(block_id, matrices[[block_id]]),
        prepared_matrix_sha256 = as.character(matrix_hashes[[block_id]]),
        profile_id = as.character(item$state$profile_id),
        operation_order = item$state$operation_order,
        operation_hash = as.character(item$state$operation_hash),
        training_unit_hash = as.character(item$state$training_unit_hash),
        feature_keys_sha256 = triomics_b07_prov_ordered_ids_sha256(item$state$feature_keys)
      )
      preparation_id <- triomics_b07_prov_entity_id("preprocessing_state", list(
        producer_stage = "C",
        analysis_id = c_handoff$analysis_id,
        block_id = block_id,
        preparation_context_id = c_handoff$preparation_context_id,
        representation_id = c_handoff$representation_id,
        state_hash = as.character(item$reference$state_hash)
      ))
      expected_preparation[[length(expected_preparation) + 1L]] <- list(
        entity_id = preparation_id,
        digest = triomics_b07_prov_value_sha256(preparation_value)
      )
    }
    }
    if (!is.null(native_payload)) state_value$native_state <- native_payload
    state_digest <- triomics_b07_prov_value_sha256(state_value)
    state <- Filter(function(entity) identical(as.character(entity$entity_id), state_id) &&
      identical(as.character(entity$digest), state_digest),
      state_entities)
    if (length(state) != 1L) {
      triomics_d_abort("D_LINEAGE_MISMATCH",
                       paste0("C integration state differs from its handoff/native sidecar (expected ",
                              state_id, "/", state_digest, "; profile ",
                              paste(vapply(state_entities, function(entity) {
                                paste(as.character(entity$entity_id), as.character(entity$digest), sep = "/")
                              }, character(1)), collapse = ", "), ")."),
                       paste0("C.integration.", integration_id))
    }
    expected_states <- c(expected_states, state_id)
  }

  entity_ids <- function(entities) vapply(entities, function(entity) {
    as.character(entity$entity_id)
  }, character(1))
  if (!identical(sort(expected_results), sort(entity_ids(result_entities))) ||
      !identical(sort(expected_states), sort(entity_ids(state_entities))) ||
      !identical(sort(expected_methods), sort(entity_ids(method_entities))) ||
      !identical(sort(expected_native), sort(entity_ids(native_entities)))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "C profile has an integration or native artifact outside the exact handoff evidence.",
                     "C.provenance.integration")
  }
  for (expected in expected_preparation) {
    matches <- Filter(function(entity) {
      identical(as.character(entity$entity_id), expected$entity_id) &&
        identical(as.character(entity$digest), expected$digest)
    }, preparation_entities)
    if (length(matches) != 1L) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "C profile prepared matrix/state differs from the exact native sidecar state.",
                       paste0("C.provenance.", expected$entity_id))
    }
  }
  invisible(TRUE)
}

triomics_d_validate_current_deqms_count_metadata <- function(block_results, current_count_metadata) {
  status <- if (is.list(block_results)) as.character(block_results$status %||% "") else ""
  if (!is.list(block_results) || !identical(as.character(block_results$method), "DEqMS") ||
      !status %in% c("complete", "partial")) {
    return(invisible(TRUE))
  }
  if (is.null(current_count_metadata)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED",
                     "A successful native DEqMS handoff needs its current count metadata for D readback.",
                     "statistics.count_metadata")
  }
  count_input <- tryCatch(triomics_b05_read_count_metadata(current_count_metadata),
                          error = function(error) NULL)
  if (!is.list(count_input) || !isTRUE(count_input$ok) || !is.data.frame(count_input$table)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "Current DEqMS count metadata are missing or unreadable.",
                     "statistics.count_metadata")
  }
  table <- count_input$table
  count_columns <- intersect(c("peptide_count", "psm_count"), names(table))
  if (length(count_columns) == 0L || length(count_columns) > 2L) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "Current DEqMS count metadata do not match the producer table schema.",
                     "statistics.count_metadata")
  }
  if (!"block_id" %in% names(table)) {
    if (length(block_results$blocks) != 1L) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "Current DEqMS count metadata omit required block identities.",
                       "statistics.count_metadata.block_id")
    }
    table$block_id <- names(block_results$blocks)[[1L]]
  }
  raw_counts <- lapply(count_columns, function(column) {
    suppressWarnings(as.numeric(as.character(table[[column]])))
  })
  names(raw_counts) <- count_columns
  present <- lapply(raw_counts, function(values) !is.na(values))
  if (length(present) > 1L && any(present[[1L]] & present[[2L]])) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "Current DEqMS count metadata ambiguously declare count types.",
                     "statistics.count_metadata")
  }
  any_count <- Reduce(`|`, present)
  if (any(!any_count)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "Current DEqMS count metadata contain missing count values.",
                     "statistics.count_metadata")
  }
  chosen <- if (length(present) == 1L) rep(count_columns[[1L]], nrow(table)) else {
    ifelse(present[[1L]], count_columns[[1L]], count_columns[[2L]])
  }
  if (length(unique(chosen)) != 1L) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "Current DEqMS count metadata mix peptide and PSM counts.",
                     "statistics.count_metadata")
  }
  table$count_value <- raw_counts[[unique(chosen)]]
  table$count_type <- unique(chosen)
  table$feature_key <- as.character(table$feature_key)
  table$block_id <- as.character(table$block_id)
  table$acquisition <- as.character(table$acquisition)
  table$provenance <- as.character(table$provenance)
  if ("sample_id" %in% names(table)) table$sample_id <- as.character(table$sample_id)
  current_hash <- triomics_b05_table_hash(table)
  has_sample <- "sample_id" %in% names(table)
  if (has_sample) table$sample_id <- as.character(table$sample_id)
  persisted_hashes <- character()
  for (block_id in names(block_results$blocks)) {
    block <- block_results$blocks[[block_id]]
    for (group in c("contrasts", "joint_claims")) {
      claims <- block[[group]]
      if (!is.list(claims)) next
      for (claim in claims) {
        rows <- claim$rows
        if (!is.list(rows)) next
        for (row in rows) {
          reference <- row$count_prior_ref
          if (!is.list(reference) ||
              !triomics_sha256_text(as.character(reference$metadata_sha256))) {
            triomics_d_abort("D_PROVENANCE_INVALID",
                             "A complete native DEqMS handoff omits its count-table identity.",
                             "B.block_results.count_prior_ref")
          }
          persisted_hashes <- c(persisted_hashes, as.character(reference$metadata_sha256))
          feature_key <- as.character(row$feature_key %||% "")
          if (!identical(as.character(reference$method), "DEqMS") ||
              !identical(as.character(reference$feature_key), feature_key) ||
              !identical(as.character(reference$count_type), unique(chosen))) {
            triomics_d_abort("D_LINEAGE_MISMATCH",
                             "A persisted DEqMS count-prior row conflicts with its current feature or count type.",
                             "B.block_results.count_prior_ref")
          }
          feature_rows <- table[table$block_id == block_id &
                                table$feature_key == feature_key, , drop = FALSE]
          if (has_sample) {
            source_sample_ids <- as.character(unlist(reference$sample_ids, use.names = FALSE))
            if (!length(source_sample_ids) && is.null(reference$count)) {
              if (!as.character(reference$summary_method) %in% c(
                "minimum_over_feature_specific_samples_used_by_fit",
                "minimum_over_feature_specific_complete_pair_observations"
              ) || length(unlist(reference$acquisition, use.names = FALSE)) ||
                  length(unlist(reference$provenance, use.names = FALSE))) {
                triomics_d_abort("D_LINEAGE_MISMATCH",
                                 "An unavailable DEqMS count prior has inconsistent empty source metadata.",
                                 "B.block_results.count_prior_ref")
              }
              next
            }
            if (!length(source_sample_ids) || anyNA(source_sample_ids) ||
                any(!nzchar(source_sample_ids)) || anyDuplicated(source_sample_ids)) {
              triomics_d_abort("D_LINEAGE_MISMATCH",
                               "A persisted DEqMS count-prior row has invalid source-sample identities.",
                               "B.block_results.count_prior_ref.sample_ids")
            }
            feature_rows <- feature_rows[
              feature_rows$sample_id %in% source_sample_ids, , drop = FALSE
            ]
            if (nrow(feature_rows) != length(source_sample_ids) ||
                !setequal(as.character(feature_rows$sample_id), source_sample_ids)) {
              triomics_d_abort("D_LINEAGE_MISMATCH",
                               "Current DEqMS count metadata do not cover the persisted feature-specific source samples.",
                               "statistics.count_metadata.sample_id")
            }
            expected_summary <- if (identical(as.character(reference$summary_method),
                                               "minimum_over_feature_specific_complete_pair_observations")) {
              "minimum_over_feature_specific_complete_pair_observations"
            } else {
              "minimum_over_feature_specific_samples_used_by_fit"
            }
            expected_count <- min(as.numeric(feature_rows$count_value))
          } else {
            if (!is.null(reference$sample_ids) || nrow(feature_rows) != 1L) {
              triomics_d_abort("D_LINEAGE_MISMATCH",
                               "Current source-reported DEqMS counts do not match the persisted feature identity.",
                               "statistics.count_metadata.feature_key")
            }
            expected_summary <- "source_reported_feature_count"
            expected_count <- as.numeric(feature_rows$count_value[[1L]])
          }
          expected_acquisition <- unique(as.character(feature_rows$acquisition))
          expected_provenance <- unique(as.character(feature_rows$provenance))
          if (is.null(reference$count) || length(reference$count) != 1L ||
              !is.finite(as.numeric(reference$count)) ||
              !identical(as.numeric(reference$count), expected_count) ||
              !identical(as.character(reference$summary_method), expected_summary) ||
              !identical(as.character(unlist(reference$acquisition, use.names = FALSE)),
                         expected_acquisition) ||
              !identical(as.character(unlist(reference$provenance, use.names = FALSE)),
                         expected_provenance)) {
            triomics_d_abort("D_LINEAGE_MISMATCH",
                             "Persisted DEqMS count priors differ from the exact current table values or metadata.",
                             "B.block_results.count_prior_ref")
          }
        }
      }
    }
  }
  if (!length(persisted_hashes) || any(!vapply(persisted_hashes, identical, logical(1), current_hash))) {
    triomics_d_abort("D_LINEAGE_MISMATCH",
                     "Current DEqMS count metadata differ from the exact B-T05 count-prior identity.",
                     "statistics.count_metadata")
  }
  invisible(TRUE)
}

triomics_d_validate_producer_provenance <- function(root, b_handoff, c_handoff,
                                                    source_manifests, feature_registry,
                                                    current_config_sha256 = NULL,
                                                    current_metadata = NULL,
                                                    current_count_metadata = NULL) {
  b_profile_path <- file.path(dirname(b_handoff$bundle_path), "stage-handoff-provenance.json")
  b_profile_envelope_path <- sub("[.]json$", ".envelope.json", b_profile_path)
  b_profile_ref <- triomics_d_read_producer_profile(
    b_profile_path, b_profile_envelope_path, "B",
    b_handoff$bundle_artifact_id, "b-t07-stage-handoff-provenance",
    b_handoff$study_id, b_handoff$analysis_id
  )
  b_profile <- b_profile_ref$profile
  if (!isTRUE(tryCatch(triomics_b07_validate_provenance_artifacts(
    b_handoff$bundle, b_handoff$bundle_path,
    sub("[.]json$", ".envelope.json", b_handoff$bundle_path)
  ), error = function(error) FALSE))) {
    triomics_d_abort("D_PROVENANCE_INVALID", "B provenance does not bind every persisted B handoff artifact.",
                     "B.provenance")
  }
  c_profile_path <- file.path(root, "D", "stage-handoff-provenance.json")
  c_profile_envelope_path <- sub("[.]json$", ".envelope.json", c_profile_path)
  c_profile_ref <- triomics_d_read_producer_profile(
    c_profile_path, c_profile_envelope_path, "C",
    "c06-handoff-manifest", "c06-stage-handoff-provenance",
    c_handoff$study_id, c_handoff$analysis_id
  )
  c_profile <- c_profile_ref$profile
  c_manifest_path <- file.path(root, "D", "handoff-manifest.json")
  c_manifest_envelope_path <- sub("[.]json$", ".envelope.json", c_manifest_path)
  c_handoff_entities <- triomics_d_profile_entities(c_profile, "handoff_payload")
  if (length(c_handoff_entities) != 1L) {
    triomics_d_abort("D_PROVENANCE_INVALID", "C provenance must bind exactly one handoff manifest.",
                     "C.provenance.handoff_payload")
  }
  triomics_d_validate_profile_artifact(
    c_handoff_entities[[1L]], c_manifest_path, c_manifest_envelope_path,
    c_handoff$manifest_artifact_id, "C.handoff_manifest"
  )
  b_bundle_envelope_path <- sub("[.]json$", ".envelope.json", b_handoff$bundle_path)
  triomics_d_validate_predecessor_pair(
    c_profile, b_handoff$bundle_path, b_bundle_envelope_path,
    b_handoff$bundle_artifact_id, "C.provenance.predecessor_b_bundle"
  )
  triomics_d_validate_predecessor_pair(
    c_profile, b_profile_ref$path, b_profile_ref$envelope_path,
    "b-t07-stage-handoff-provenance", "C.provenance.predecessor_b_profile"
  )
  triomics_d_validate_c_profile_values(root, b_handoff, c_handoff, c_profile_ref)
  inherited_roles <- c("source_manifest", "normalized_metadata", "feature_registry",
                       "analysis_unit_map", "resolved_analysis", "represented_matrix")
  for (role in inherited_roles) triomics_d_compare_profile_identity(b_profile, c_profile, role)
  inherited_preprocessing <- triomics_d_profile_entities(b_profile, "preprocessing_state")
  c_preprocessing <- triomics_d_profile_entities(c_profile, "preprocessing_state")
  if (!length(inherited_preprocessing) || any(!vapply(inherited_preprocessing, function(entity) {
    matches <- Filter(function(candidate) identical(as.character(candidate$entity_id),
                                                    as.character(entity$entity_id)), c_preprocessing)
    length(matches) == 1L &&
      identical(as.character(matches[[1L]]$digest), as.character(entity$digest)) &&
      identical(as.character(matches[[1L]]$canonical_schema_uri),
                as.character(entity$canonical_schema_uri))
  }, logical(1)))) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "C preprocessing lineage differs from persisted B descriptors.",
                     "C.provenance.preprocessing_state")
  }
  if (!is.null(current_config_sha256)) {
    config_hashes <- c(
      as.character(b_handoff$bundle$config_sha256),
      as.character(b_profile$producer$effective_configuration_sha256),
      as.character(c_profile$producer$effective_configuration_sha256),
      as.character(c_profile_ref$envelope$config_sha256)
    )
    if (!triomics_sha256_text(as.character(current_config_sha256)) ||
        any(!vapply(config_hashes, identical, logical(1), as.character(current_config_sha256)))) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "The current D configuration differs from the B/C producer request.",
                       "config_sha256")
    }
  }
  source_ids <- vapply(source_manifests, function(source) {
    triomics_b07_prov_entity_id("source_manifest", list(
      source_id = as.character(source$source_id), sha256 = as.character(source$sha256)
    ))
  }, character(1))
  persisted_source_ids <- vapply(triomics_d_profile_entities(c_profile, "source_manifest"),
                                 function(entity) as.character(entity$entity_id), character(1))
  source_hashes <- vapply(source_manifests, function(source) as.character(source$sha256), character(1))
  if (!length(source_ids) || any(!vapply(source_hashes, triomics_sha256_text, logical(1))) ||
      any(!source_ids %in% persisted_source_ids)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "Current ingested sources differ from exact B/C source identities.",
                     "source_manifests")
  }
  if (!is.null(current_metadata)) {
    metadata_entities <- triomics_d_profile_entities(c_profile, "normalized_metadata")
    expected_digest <- triomics_b07_prov_value_sha256(current_metadata)
    if (length(metadata_entities) != 1L ||
        !identical(as.character(metadata_entities[[1L]]$digest), expected_digest)) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "Current normalized metadata differ from B/C producer evidence.",
                       "normalized_metadata")
    }
  }
  triomics_d_validate_current_deqms_count_metadata(
    b_handoff$block_results, current_count_metadata
  )
  feature_entities <- triomics_d_profile_entities(c_profile, "feature_registry")
  block_ids <- sort(unique(as.character(feature_registry$block_id)), method = "radix")
  if (!length(block_ids) || length(feature_entities) != length(block_ids) ||
      any(!vapply(block_ids, function(block_id) {
        entity_id <- triomics_b07_prov_entity_id("feature_registry", block_id)
        entities <- Filter(function(entity) identical(as.character(entity$entity_id), entity_id),
                           feature_entities)
        rows <- feature_registry[as.character(feature_registry$block_id) == block_id, , drop = FALSE]
        length(entities) == 1L &&
          identical(as.character(entities[[1L]]$digest), triomics_b07_prov_value_sha256(rows))
      }, logical(1)))) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "The current FeatureRegistry differs from exact B/C provenance.",
                     "feature_registry")
  }
  list(
    root = root,
    current_config_sha256 = if (is.null(current_config_sha256)) NULL else
      as.character(current_config_sha256),
    b_bundle_path = b_handoff$bundle_path,
    b_bundle_envelope_path = b_bundle_envelope_path,
    b_bundle_artifact_id = b_handoff$bundle_artifact_id,
    b_bundle_sha256 = b_handoff$bundle_sha256,
    b_bundle_envelope_sha256 = b_handoff$bundle_envelope_sha256,
    b_artifacts = b_handoff$artifact_refs,
    b_profile = b_profile_ref,
    c_profile = c_profile_ref,
    c_manifest_path = c_manifest_path,
    c_manifest_envelope_path = c_manifest_envelope_path,
    c_artifacts = c_handoff$artifact_refs
  )
}

triomics_d_read_evidence_bundle <- function(output_root, expected_study_id, expected_analysis_id,
                                            source_manifests, feature_registry,
                                            imported_statistics = list(), routing_metadata = NULL,
                                            current_config_sha256 = NULL,
                                            current_metadata = NULL,
                                            current_count_metadata = NULL) {
  if (!triomics_d_nonempty_string(output_root) || !dir.exists(output_root) ||
      triomics_is_symlink(output_root)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "The Stage-B/Stage-C local output root is unavailable.", "output_root")
  }
  root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
  c_handoff <- triomics_d_read_c06_handoff(
    file.path(root, "D"), expected_study_id, expected_analysis_id
  )
  nomination_ref <- c_handoff$candidate_nominations$branches$primary_statistics$source_artifact_ref
  b_handoff <- triomics_d_read_b_handoff(
    root, nomination_ref, expected_study_id, expected_analysis_id,
    c_handoff$preparation_context_id, c_handoff$representation_id
  )
  producer_provenance <- triomics_d_validate_producer_provenance(
    root, b_handoff, c_handoff, source_manifests, feature_registry,
    current_config_sha256, current_metadata, current_count_metadata
  )
  producer_provenance$verified_request <- list(
    source_manifests = source_manifests,
    feature_registry = feature_registry,
    current_config_sha256 = current_config_sha256,
    current_metadata = current_metadata,
    current_count_metadata = current_count_metadata,
    imported_statistics = imported_statistics,
    routing_metadata = routing_metadata
  )
  result <- triomics_d_build_evidence_bundle(list(
    study_id = expected_study_id,
    analysis_id = expected_analysis_id,
    source_manifests = source_manifests,
    feature_registry = feature_registry,
    routing_metadata = routing_metadata,
    b = b_handoff,
    c = list(
      candidate_nominations = c_handoff$candidate_nominations,
      integration_evidence = c_handoff$integration_evidence,
      integration_comparisons = c_handoff$integration_comparisons,
      evaluation_evidence = c_handoff$evaluation_evidence,
      stage_status = c_handoff$stage_status
    ),
    imported_statistics = imported_statistics
  ))
  result$immutable_inputs <- list(
    b_bundle_sha256 = b_handoff$bundle_sha256,
    b_bundle_artifact_id = b_handoff$bundle_artifact_id,
    b_bundle_envelope_sha256 = b_handoff$bundle_envelope_sha256,
    b_artifacts = b_handoff$artifact_refs,
    c_manifest_artifact_id = c_handoff$manifest_artifact_id,
    c_manifest_sha256 = c_handoff$manifest_sha256,
    c_manifest_envelope_sha256 = c_handoff$manifest_envelope_sha256,
    c_artifacts = c_handoff$artifact_refs,
    b_profile_artifact_id = "b-t07-stage-handoff-provenance",
    b_profile_sha256 = producer_provenance$b_profile$payload_sha256,
    b_profile_envelope_sha256 = producer_provenance$b_profile$envelope_sha256,
    c_profile_artifact_id = "c06-stage-handoff-provenance",
    c_profile_sha256 = producer_provenance$c_profile$payload_sha256,
    c_profile_envelope_sha256 = producer_provenance$c_profile$envelope_sha256
  )
  result$publication_context <- producer_provenance
  result
}

triomics_d_validate_sources <- function(input, study_id) {
  valid_roles <- c("measurement_matrix", "clinical_metadata", "sample_metadata",
                   "feature_metadata", "imported_result", "reference_evidence")
  sources <- input$source_manifests
  if (!is.list(sources) || !length(sources)) {
    triomics_d_abort("D_SOURCE_ROLE_INVALID", "D requires the source manifest roles from its immutable inputs.",
                     "source_manifests")
  }
  source_ids <- vapply(sources, function(source) as.character(source$source_id %||% ""), character(1))
  if (any(!nzchar(source_ids)) || anyDuplicated(source_ids)) {
    triomics_d_abort("D_SOURCE_ROLE_INVALID", "D source manifests need unique source IDs.", "source_manifests")
  }
  for (index in seq_along(sources)) {
    source <- sources[[index]]
    if (!identical(as.character(source$study_id), study_id)) {
      triomics_d_abort("D_LINEAGE_MISMATCH", "A D source manifest belongs to a different study.",
                       paste0("source_manifests.", source_ids[[index]], ".study_id"))
    }
    if (!is.character(source$role) || length(source$role) != 1L || is.na(source$role) ||
        !source$role %in% valid_roles || !triomics_sha256_text(as.character(source$sha256))) {
      triomics_d_abort("D_SOURCE_ROLE_INVALID", "A D source manifest has an unknown role or invalid content hash.",
                       paste0("source_manifests.", source_ids[[index]]))
    }
  }
  stats::setNames(sources, source_ids)
}

triomics_d_validate_source_role_evidence <- function(integration_evidence, sources, feature_registry) {
  integrations <- integration_evidence$integrations
  if (!is.list(integrations)) return(invisible(TRUE))
  valid_roles <- c("measurement_matrix", "clinical_metadata", "sample_metadata",
                   "feature_metadata", "imported_result", "reference_evidence")
  for (integration in integrations) {
    role_blocks <- integration$eligibility$source_roles
    if (!is.list(role_blocks)) next
    for (block_id in names(role_blocks)) {
      role_record <- role_blocks[[block_id]]
      if (!identical(as.character(role_record$status), "supplied")) next
      role_values <- as.character(unlist(role_record$value, use.names = FALSE))
      if (!length(role_values) || anyNA(role_values) || any(!role_values %in% valid_roles)) {
        triomics_d_abort("D_SOURCE_ROLE_INVALID", "C integration evidence contains an unknown source role.",
                         paste0("integration.", integration$integration_id, ".source_roles.", block_id))
      }
      feature_ids <- names(feature_registry)[vapply(feature_registry, function(feature) {
        identical(as.character(feature$block_id), block_id)
      }, logical(1))]
      source_ids <- unique(vapply(feature_registry[feature_ids], function(feature) {
        as.character(feature$source_id %||% "")
      }, character(1)))
      source_ids <- source_ids[nzchar(source_ids)]
      declared_roles <- unique(vapply(sources[source_ids], function(source) as.character(source$role), character(1)))
      if (length(declared_roles) && !setequal(role_values, declared_roles)) {
        triomics_d_abort("D_SOURCE_ROLE_MISMATCH", "C source-role lineage disagrees with the registered source manifests.",
                         paste0("integration.", integration$integration_id, ".source_roles.", block_id))
      }
    }
  }
  invisible(TRUE)
}

triomics_d_adapt_feature_registry <- function(feature_registry, routing_metadata, study_id, sources) {
  required <- c("study_id", "block_id", "feature_key", "original_id", "id_namespace", "source_id",
                "source_row", "measured_or_derived", "quantity", "scale", "units",
                "structural_resolution", "alias_refs", "parent_refs", "annotation_route")
  if (!is.data.frame(feature_registry) || !all(required %in% names(feature_registry)) ||
      nrow(feature_registry) < 1L) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "D requires the shared A FeatureRegistry data-frame contract.",
                     "feature_registry")
  }
  identity_fields <- c("study_id", "block_id", "feature_key", "original_id", "id_namespace", "source_id",
                       "source_row", "measured_or_derived", "structural_resolution")
  if (any(!vapply(feature_registry[identity_fields], is.character, logical(1)))) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A FeatureRegistry identity and routing fields must remain character.",
                     "feature_registry")
  }
  if (!is.null(routing_metadata) && (!is.list(routing_metadata) ||
      (length(routing_metadata) && (is.null(names(routing_metadata)) ||
       any(!nzchar(names(routing_metadata))) || anyDuplicated(names(routing_metadata)))))) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "D routing metadata must be a uniquely keyed packet-owned list.",
                     "routing_metadata")
  }
  feature_keys <- feature_registry$feature_key
  if (!length(feature_keys) || anyNA(feature_keys) || any(!nzchar(feature_keys)) || anyDuplicated(feature_keys)) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A FeatureRegistry needs unique, non-empty feature keys.",
                     "feature_registry.feature_key")
  }
  if (!is.null(routing_metadata) && any(!names(routing_metadata) %in% feature_keys)) {
    triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "D routing metadata refers to a feature outside the A registry.",
                     "routing_metadata")
  }
  adapted <- vector("list", length(feature_keys))
  names(adapted) <- feature_keys
  for (index in seq_along(feature_keys)) {
    row <- lapply(feature_registry, `[`, index)
    feature_key <- feature_keys[[index]]
    block_id <- feature_registry$block_id[[index]]
    source_row <- feature_registry$source_row[[index]]
    source_id <- feature_registry$source_id[[index]]
    identities_valid <- all(vapply(identity_fields, function(field) {
      triomics_d_nonempty_string(feature_registry[[field]][[index]])
    }, logical(1))) &&
      identical(feature_registry$study_id[[index]], study_id) &&
      identical(feature_key, triomics_feature_key(study_id, block_id, source_row)) &&
      source_id %in% names(sources) &&
      feature_registry$measured_or_derived[[index]] %in% c("measured", "derived")
    if (!identities_valid) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A FeatureRegistry row has mixed study, invalid study/block/source-row identity, or unknown source.",
                       paste0("feature_registry.", feature_key))
    }
    metadata <- if (is.null(routing_metadata)) NULL else routing_metadata[[feature_key]]
    if (!is.null(metadata) && !is.list(metadata)) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A D routing metadata record must be a typed list.",
                       paste0("routing_metadata.", feature_key))
    }
    taxon <- metadata$taxon %||% list(status = "unknown", value = NULL, provenance_ref = NULL)
    capabilities <- metadata$capabilities %||% list(status = "unknown", value = list(), provenance_ref = NULL)
    resolution <- metadata$resolution %||% if (identical(feature_registry$structural_resolution[[index]], "unknown")) {
      list(status = "unknown", value = NULL, provenance_ref = NULL)
    } else {
      list(status = "declared", value = feature_registry$structural_resolution[[index]],
           provenance_ref = "A.FeatureRegistry.structural_resolution")
    }
    valid_state <- function(record) {
      triomics_d_nonempty_string(record$status) && record$status %in% c("declared", "unknown")
    }
    if (!is.list(taxon) || !is.list(capabilities) || !is.list(resolution) ||
        !valid_state(taxon) || !valid_state(capabilities) || !valid_state(resolution)) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "Taxon, capability, and resolution states must be declared or explicitly unknown.",
                       paste0("routing_metadata.", feature_key))
    }
    for (record in list(taxon, capabilities, resolution)) {
      if (identical(record$status, "declared") && !triomics_d_nonempty_string(record$provenance_ref)) {
        triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "Declared D routing metadata needs a provenance reference.",
                         paste0("routing_metadata.", feature_key))
      }
    }
    if (identical(taxon$status, "declared") && !triomics_d_nonempty_string(taxon$value)) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A declared taxon needs one non-empty taxon ID.",
                       paste0("routing_metadata.", feature_key, ".taxon"))
    }
    if (identical(capabilities$status, "declared") &&
        (!is.character(capabilities$value) || !length(capabilities$value) || anyNA(capabilities$value) ||
         any(!nzchar(capabilities$value)))) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "Declared feature capabilities must be explicit character values.",
                       paste0("routing_metadata.", feature_key, ".capabilities"))
    }
    if (identical(resolution$status, "declared") && !triomics_d_nonempty_string(resolution$value)) {
      triomics_d_abort("D_FEATURE_REGISTRY_INVALID", "A declared molecular resolution needs one non-empty value.",
                       paste0("routing_metadata.", feature_key, ".resolution"))
    }
    feature <- row
    feature$taxon_id <- if (identical(taxon$status, "declared")) taxon$value else NULL
    feature$taxon_status <- taxon$status
    feature$capabilities <- if (identical(capabilities$status, "declared")) as.list(capabilities$value) else list()
    feature$capability_status <- capabilities$status
    feature$structural_resolution <- if (identical(resolution$status, "declared")) resolution$value else "unknown"
    feature$resolution_status <- resolution$status
    feature$provenance <- list(feature_registry_row = row, routing_metadata = metadata)
    adapted[[feature_key]] <- feature
  }
  adapted
}

triomics_d_claim_records <- function(block_results) {
  blocks <- block_results$blocks
  if (!is.list(blocks) || !length(blocks) || is.null(names(blocks))) {
    triomics_d_abort("D_B_EVIDENCE_INVALID", "The B block-results artifact has no keyed block records.", "b.block_results.blocks")
  }
  claims <- list()
  for (block_id in names(blocks)) {
    block <- blocks[[block_id]]
    for (group_name in c("contrasts", "joint_claims")) {
      group <- block[[group_name]]
      if (!is.list(group) || !length(group)) next
      for (claim_id in names(group)) {
        claim <- group[[claim_id]]
        if (!is.list(claim$rows) || !is.list(claim$family)) next
        claims[[length(claims) + 1L]] <- list(
          block_id = block_id, claim_id = claim_id,
          claim_type = as.character(claim$claim_type %||% group_name),
          family = claim$family, rows = claim$rows
        )
      }
    }
  }
  if (!length(claims)) {
    triomics_d_abort("D_B_EVIDENCE_INVALID", "The B handoff has no complete family-backed tested claims.", "b.block_results.blocks")
  }
  claims
}

triomics_d_tested_rows <- function(claims, feature_registry) {
  indexed <- list()
  for (claim in claims) {
    for (row in claim$rows) {
      result_id <- as.character(row$result_id %||% "")
      feature_key <- as.character(row$feature_key %||% "")
      if (!nzchar(result_id) || !feature_key %in% names(feature_registry)) {
        triomics_d_abort("D_B_EVIDENCE_INVALID", "A B tested row lacks a result ID or registered feature key.",
                         paste0("b.results.", claim$block_id, ".", claim$claim_id))
      }
      record <- list(block_id = claim$block_id, claim_id = claim$claim_id, row = row)
      previous <- indexed[[result_id]]
      if (!is.null(previous)) {
        if (!identical(triomics_canonical_json(previous), triomics_canonical_json(record))) {
          triomics_d_abort("D_DUPLICATE_RESULT_CONFLICT", "A B result ID names conflicting tested rows.", result_id)
        }
      } else {
        indexed[[result_id]] <- record
      }
    }
  }
  unname(indexed)
}

triomics_d_unique_measured_features <- function(tested_results, feature_registry) {
  feature_keys <- unique(vapply(tested_results, function(result) {
    as.character(result$row$feature_key)
  }, character(1)))
  lapply(feature_keys, function(feature_key) feature_registry[[feature_key]])
}

triomics_d_index_nominations <- function(nominations, feature_registry, label) {
  if (!is.list(nominations)) {
    triomics_d_abort("D_NOMINATION_INVALID", "A nomination branch must be a list.", label)
  }
  indexed <- list()
  for (nomination in nominations) {
    triomics_d_validate_nomination(nomination, feature_registry)
    nomination_id <- as.character(nomination$nomination_id)
    previous <- indexed[[nomination_id]]
    if (!is.null(previous) &&
        !identical(triomics_canonical_json(previous), triomics_canonical_json(nomination))) {
      triomics_d_abort("D_DUPLICATE_NOMINATION_CONFLICT", "A nomination branch repeats a nomination ID.",
                       paste0(label, ".", nomination_id))
    }
    if (is.null(previous)) indexed[[nomination_id]] <- nomination
  }
  indexed
}

triomics_d_reconcile_nominations <- function(expected, actual, feature_registry, label) {
  expected_by_id <- triomics_d_index_nominations(expected, feature_registry, paste0(label, ".expected"))
  actual_by_id <- triomics_d_index_nominations(actual, feature_registry, paste0(label, ".actual"))
  if (!setequal(names(expected_by_id), names(actual_by_id))) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A C-06 nomination branch differs from its immutable producer nominations.",
                     label)
  }
  for (nomination_id in names(expected_by_id)) {
    if (!identical(triomics_canonical_json(expected_by_id[[nomination_id]]),
                   triomics_canonical_json(actual_by_id[[nomination_id]]))) {
      triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A nomination ID carries different feature or selection evidence across branches.",
                       paste0(label, ".", nomination_id))
    }
  }
  invisible(TRUE)
}

triomics_d_resolve_primary_nomination <- function(block_results, family_ledger,
                                                  nomination, analysis_id) {
  reference <- nomination$result_ref
  expected_ref <- list(
    analysis_id = analysis_id,
    block_id = as.character(reference$block_id %||% ""),
    claim_id = as.character(reference$claim_id %||% ""),
    family_id = as.character(reference$family_id %||% "")
  )
  if (!identical(triomics_canonical_json(reference), triomics_canonical_json(expected_ref))) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination reference has extra or mismatched producer identity.",
                     as.character(nomination$nomination_id))
  }
  block_id <- expected_ref$block_id
  claim_id <- expected_ref$claim_id
  claim <- block_results$blocks[[block_id]]$contrasts[[claim_id]] %||%
    block_results$blocks[[block_id]]$joint_claims[[claim_id]]
  if (!is.list(claim) || !is.list(claim$family) || !is.list(claim$rows) ||
      !identical(as.character(claim$family$family_id), expected_ref$family_id)) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination does not resolve to its complete B family-backed claim.",
                     as.character(nomination$nomination_id))
  }
  family_records <- Filter(function(record) {
    identical(as.character(record$block_id), block_id) &&
      identical(as.character(record$claim_id), claim_id)
  }, family_ledger$families)
  if (length(family_records) != 1L ||
      !identical(triomics_canonical_json(family_records[[1L]]$family),
                 triomics_canonical_json(claim$family))) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination's B family disagrees with the family ledger.",
                     as.character(nomination$nomination_id))
  }
  feature_key <- as.character(nomination$feature_key)
  if (!feature_key %in% triomics_d_route_character_values(claim$family$member_ids)) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination feature is not a member of its B family.",
                     as.character(nomination$nomination_id))
  }
  matching_rows <- Filter(function(row) identical(as.character(row$feature_key), feature_key), claim$rows)
  if (length(matching_rows) != 1L) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination does not resolve to exactly one tested feature row.",
                     as.character(nomination$nomination_id))
  }
  row <- matching_rows[[1L]]
  q_threshold <- nomination$q_threshold
  effect_threshold <- nomination$effect_threshold
  if (!identical(as.character(row$estimation_status), "estimated") ||
      !is.numeric(q_threshold) || length(q_threshold) != 1L || is.na(q_threshold) ||
      !is.finite(q_threshold) || q_threshold < 0 || q_threshold > 1 ||
      !is.numeric(effect_threshold) || length(effect_threshold) != 1L || is.na(effect_threshold) ||
      !is.finite(effect_threshold) || effect_threshold < 0 ||
      !is.numeric(row$p) || length(row$p) != 1L || is.na(row$p) || !is.finite(row$p) ||
      row$p < 0 || row$p > 1 ||
      !is.numeric(row$q) || length(row$q) != 1L || is.na(row$q) || !is.finite(row$q) ||
      !is.numeric(row$effect) || length(row$effect) != 1L || is.na(row$effect) || !is.finite(row$effect) ||
      row$q > q_threshold || abs(row$effect) < effect_threshold ||
      !identical(as.character(nomination$selection_rule),
                 "valid_q_at_or_below_threshold_and_absolute_effect_at_or_above_reporting_threshold")) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination does not satisfy its producer selection rule and thresholds.",
                     as.character(nomination$nomination_id))
  }
  expected_value <- list(p = row$p, q = row$q, effect = row$effect, effect_unit = row$effect_unit)
  if (!identical(triomics_canonical_json(nomination$selection_value),
                 triomics_canonical_json(expected_value))) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A primary nomination's selected statistics differ from its immutable B result row.",
                     as.character(nomination$nomination_id))
  }
  list(claim = claim, family_record = family_records[[1L]], row = row)
}

triomics_d_resolve_integration_nomination <- function(integration_evidence, nomination) {
  reference <- nomination$result_ref
  integration_id <- as.character(reference$integration_id %||% "")
  integrations <- integration_evidence$integrations
  matches <- Filter(function(item) identical(as.character(item$integration_id), integration_id), integrations)
  if (length(matches) != 1L || !identical(as.character(matches[[1L]]$status), "complete") ||
      !is.list(matches[[1L]]$selection)) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "An integration nomination does not resolve to one complete native selection.",
                     as.character(nomination$nomination_id))
  }
  integration <- matches[[1L]]
  selection <- integration$selection
  block_id <- as.character(reference$block_id %||% "")
  feature_key <- as.character(nomination$feature_key)
  selected_features <- triomics_d_route_character_values(selection$selected[[block_id]])
  selected_values <- selection$selected_values[[block_id]]
  component_ids <- triomics_d_route_character_values(integration$common$scores$columns)
  if (!feature_key %in% selected_features || !is.list(selected_values) ||
      !feature_key %in% names(selected_values) || !is.list(integration$common$scores) ||
      !length(component_ids)) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "An integration nomination is absent from the producer's selected values.",
                     as.character(nomination$nomination_id))
  }
  selected_value <- selected_values[[feature_key]]
  expected_ref <- list(
    integration_id = as.character(integration$integration_id),
    backend = as.character(integration$backend),
    block_id = block_id,
    component_ids = as.list(component_ids)
  )
  expected_value <- list(block_id = block_id,
                         component_loadings = selected_value$component_loadings)
  if (!identical(triomics_canonical_json(reference), triomics_canonical_json(expected_ref)) ||
      !identical(triomics_canonical_json(nomination$selection_rule),
                 triomics_canonical_json(selection$rule)) ||
      !identical(triomics_canonical_json(nomination$selection_value),
                 triomics_canonical_json(expected_value))) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "An integration nomination's selected value or producer reference was changed.",
                     as.character(nomination$nomination_id))
  }
  list(integration = integration, selection = selection, selected_value = selected_value)
}

triomics_d_validate_nomination <- function(nomination, feature_registry) {
  selection_rule_valid <- function(rule, source_branch) {
    if (identical(as.character(source_branch), "primary_statistics")) {
      return(triomics_d_nonempty_string(rule))
    }
    if (triomics_d_nonempty_string(rule)) return(TRUE)
    if (!is.list(rule) || !length(rule) || is.null(names(rule)) ||
        any(!nzchar(names(rule))) || anyDuplicated(names(rule))) return(FALSE)
    tryCatch({
      triomics_canonical_json(rule)
      TRUE
    }, error = function(error) FALSE)
  }
  if (!is.list(nomination) || !triomics_sha256_text(as.character(nomination$nomination_id)) ||
      !triomics_d_nonempty_string(as.character(nomination$feature_key)) ||
      !as.character(nomination$feature_key) %in% names(feature_registry) ||
      !as.character(nomination$source_branch) %in% c("primary_statistics", "integration_selection") ||
      !is.list(nomination$result_ref) ||
      !selection_rule_valid(nomination$selection_rule, as.character(nomination$source_branch)) ||
      !is.list(nomination$selection_value)) {
    triomics_d_abort("D_NOMINATION_INVALID", "A D nomination lacks a registered feature or full selection lineage.",
                     "candidate_nominations")
  }
  feature <- feature_registry[[as.character(nomination$feature_key)]]
  block_id <- as.character(nomination$result_ref$block_id %||% "")
  if (!triomics_d_nonempty_string(block_id) ||
      !identical(as.character(feature$block_id), block_id)) {
    triomics_d_abort("D_NOMINATION_LINEAGE_MISMATCH", "A nomination feature key does not belong to its producer block.",
                     as.character(nomination$nomination_id))
  }
  invisible(TRUE)
}

triomics_d_unique_nominations <- function(nominations, feature_registry) {
  if (!is.list(nominations)) {
    triomics_d_abort("D_NOMINATION_INVALID", "C-06 candidate nominations must be a list.", "candidate_nominations.nominations")
  }
  indexed <- list()
  duplicate_count <- 0L
  for (nomination in nominations) {
    triomics_d_validate_nomination(nomination, feature_registry)
    nomination_id <- as.character(nomination$nomination_id)
    previous <- indexed[[nomination_id]]
    if (is.null(previous)) {
      indexed[[nomination_id]] <- nomination
    } else if (!identical(triomics_canonical_json(previous), triomics_canonical_json(nomination))) {
      triomics_d_abort("D_DUPLICATE_NOMINATION_CONFLICT", "One nomination ID carries conflicting feature or selection evidence.",
                       nomination_id)
    } else {
      duplicate_count <- duplicate_count + 1L
    }
  }
  list(nominations = unname(indexed), duplicate_count = duplicate_count)
}

triomics_d_build_evidence_bundle <- function(input) {
  if (!is.list(input) || !triomics_d_nonempty_string(input$study_id) ||
      !triomics_d_nonempty_string(input$analysis_id) || !is.list(input$b) || !is.list(input$c)) {
    triomics_d_abort("D_EVIDENCE_INPUT_INVALID", "D evidence needs explicit study, analysis, B, and C identities.", "evidence_bundle")
  }
  study_id <- as.character(input$study_id)
  analysis_id <- as.character(input$analysis_id)
  sources <- triomics_d_validate_sources(input, study_id)
  feature_registry <- triomics_d_adapt_feature_registry(
    input$feature_registry, input$routing_metadata, study_id, sources
  )
  b <- input$b
  c <- input$c
  b_artifacts <- list(block_results = b$block_results, family_ledger = b$family_ledger,
                      candidate_nominations = b$candidate_nominations)
  for (name in names(b_artifacts)) {
    artifact <- b_artifacts[[name]]
    triomics_d_same_identity(artifact, "study_id", study_id, paste0("B ", name))
    triomics_d_same_identity(artifact, "analysis_id", analysis_id, paste0("B ", name))
  }
  c_artifacts <- list(candidate_nominations = c$candidate_nominations,
                      integration_evidence = c$integration_evidence,
                      evaluation_evidence = c$evaluation_evidence,
                      stage_status = c$stage_status)
  for (name in names(c_artifacts)) {
    artifact <- c_artifacts[[name]]
    triomics_d_same_identity(artifact, "study_id", study_id, paste0("C ", name))
    triomics_d_same_identity(artifact, "analysis_id", analysis_id, paste0("C ", name))
  }
  context_id <- c$candidate_nominations$preparation_context_id
  representation_id <- c$candidate_nominations$representation_id
  if (!triomics_d_nonempty_string(context_id) || !triomics_d_nonempty_string(representation_id)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "C-06 nominations require preparation and representation identities.",
                     "candidate_nominations.context")
  }
  for (name in names(c_artifacts)) {
    artifact <- c_artifacts[[name]]
    triomics_d_same_identity(artifact, "preparation_context_id", context_id, paste0("C ", name))
    triomics_d_same_identity(artifact, "representation_id", representation_id, paste0("C ", name))
  }
  triomics_d_same_identity(b$block_results, "preparation_context_id", context_id, "B block_results")
  triomics_d_same_identity(b$block_results, "representation_id", representation_id, "B block_results")
  if (!is.null(b$preparation_context_id) && !identical(b$preparation_context_id, context_id)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "B-T07 and C-06 preparation contexts differ.",
                     "B.bundle.preparation_context_id")
  }
  if (!is.null(b$representation_id) && !identical(b$representation_id, representation_id)) {
    triomics_d_abort("D_LINEAGE_MISMATCH", "B-T07 and C-06 representations differ.",
                     "B.bundle.representation_id")
  }
  candidate_payload <- c$candidate_nominations
  primary_branch <- candidate_payload$branches$primary_statistics
  if (!is.list(candidate_payload$branches) || !is.list(primary_branch) ||
      !is.list(candidate_payload$branches$integration_selection)) {
    triomics_d_abort("D_NOMINATION_INVALID", "The C-06 handoff must preserve both nomination branches.",
                     "candidate_nominations.branches")
  }
  b_nominations <- b$candidate_nominations$nominations
  primary_nominations <- primary_branch$nominations
  integration_nominations <- candidate_payload$branches$integration_selection$nominations
  triomics_d_reconcile_nominations(
    b_nominations, primary_nominations, feature_registry,
    "candidate_nominations.branches.primary_statistics"
  )
  triomics_d_reconcile_nominations(
    c(primary_nominations, integration_nominations), candidate_payload$nominations,
    feature_registry, "candidate_nominations.flattened"
  )
  for (nomination in primary_nominations) {
    if (!identical(as.character(nomination$source_branch), "primary_statistics")) {
      triomics_d_abort("D_NOMINATION_INVALID", "A B nomination was assigned to the wrong C-06 branch.",
                       "candidate_nominations.branches.primary_statistics")
    }
    triomics_d_resolve_primary_nomination(
      b$block_results, b$family_ledger, nomination, analysis_id
    )
  }
  for (nomination in integration_nominations) {
    if (!identical(as.character(nomination$source_branch), "integration_selection")) {
      triomics_d_abort("D_NOMINATION_INVALID", "A C nomination was assigned to the wrong C-06 branch.",
                       "candidate_nominations.branches.integration_selection")
    }
    triomics_d_resolve_integration_nomination(c$integration_evidence, nomination)
  }
  triomics_d_validate_source_role_evidence(c$integration_evidence, sources, feature_registry)
  claims <- triomics_d_claim_records(b$block_results)
  tested_results <- triomics_d_tested_rows(claims, feature_registry)
  measured_rows <- triomics_d_unique_measured_features(tested_results, feature_registry)
  nominations <- triomics_d_unique_nominations(candidate_payload$nominations, feature_registry)

  imported <- input$imported_statistics %||% list()
  if (!is.list(imported)) {
    triomics_d_abort("D_IMPORTED_EVIDENCE_INVALID", "Imported statistics must remain a list of sourced external evidence.",
                     "imported_statistics")
  }
  imported <- lapply(imported, function(record) {
    if (!is.list(record) || !identical(as.character(record$study_id), study_id) ||
        !identical(as.character(record$source_role), "imported_result")) {
      triomics_d_abort("D_IMPORTED_EVIDENCE_INVALID", "Imported statistics need matching study identity and imported_result source role.",
                       "imported_statistics")
    }
    record$use <- "external_display_concordance_only"
    record$status <- "display_only"
    record
  })
  family_records <- b$family_ledger$families
  if (!is.list(family_records) || !length(family_records)) {
    triomics_d_abort("D_B_EVIDENCE_INVALID", "The B family ledger does not account for tested claims.", "b.family_ledger.families")
  }
  list(
    schema_version = "2.0", artifact_type = "evidence_bundle", status = "complete",
    study_id = study_id, analysis_id = analysis_id,
    preparation_context_id = context_id, representation_id = representation_id,
    source_manifests = unname(sources), source_by_id = sources,
    feature_registry = feature_registry,
    b = list(block_results = b$block_results, family_ledger = b$family_ledger,
             candidate_nominations = b$candidate_nominations),
    c = list(candidate_nominations = c$candidate_nominations,
             integration_evidence = c$integration_evidence,
             integration_comparisons = c$integration_comparisons %||% NULL,
             evaluation_evidence = c$evaluation_evidence, stage_status = c$stage_status),
    nominations = nominations$nominations,
    tested_claims = claims,
    tested_results = tested_results,
    measured_rows = measured_rows,
    imported_evidence = imported,
    accounting = list(
      nomination_count = as.integer(length(nominations$nominations)),
      duplicate_nomination_count = as.integer(nominations$duplicate_count),
      measured_row_count = as.integer(length(measured_rows)),
      tested_result_count = as.integer(length(tested_results)),
      tested_claim_count = as.integer(length(claims)),
      family_count = as.integer(length(family_records))
    )
  )
}
