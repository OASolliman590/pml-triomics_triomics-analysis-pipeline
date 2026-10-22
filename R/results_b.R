triomics_b_required_effect_unit <- function(scale) {
  if (identical(scale, "log2")) return("log2_fold_change")
  if (scale %in% c("clr_ln", "clr_log2")) return("clr_contrast")
  if (scale %in% c("linear", "ln", "variance_stabilized")) return("group_difference")
  ""
}

triomics_b_check_effect_unit <- function(scales, effect_unit) {
  required <- unique(vapply(scales, triomics_b_required_effect_unit, character(1)))
  if (length(required) != 1L || !nzchar(required[[1L]]) || !identical(required[[1L]], effect_unit)) {
    triomics_b_abort(
      "B_DESIGN_EFFECT_UNIT",
      "The effect unit does not match the declared measurement scale.",
      key = "statistics.effect_unit"
    )
  }
  required[[1L]]
}

triomics_b_inferential_status <- function(estimated, selection_validity) {
  if (!estimated) return("not_estimable")
  if (identical(selection_validity, "descriptive_only")) return("descriptive_only")
  "unverified"
}

triomics_b_family <- function(study_id, analysis_id, block_id, contrast_id, member_ids,
                              estimated, p_values, publish_q) {
  n <- length(member_ids)
  internal_p <- rep(1, n)
  if (any(estimated)) internal_p[estimated] <- as.numeric(p_values[estimated])
  internal_p <- unname(as.numeric(internal_p))
  internal_q <- unname(as.numeric(stats::p.adjust(internal_p, method = "BH")))
  n_estimated <- as.integer(sum(estimated))
  n_failed <- as.integer(n - n_estimated)
  list(
    family_id = triomics_b_hash(list(
      study_id = study_id,
      analysis_id = analysis_id,
      block_id = block_id,
      contrast_id = contrast_id,
      grain = "study_analysis_block_scalar_contrast"
    )),
    member_ids = as.character(member_ids),
    member_hash = triomics_b_hash(as.character(member_ids)),
    n_declared = as.integer(n),
    n_estimated = n_estimated,
    n_failed = n_failed,
    n_eligible = n_estimated,
    n_evaluated = as.integer(n),
    dispositions = c(estimated = n_estimated, failed = n_failed),
    internal_p = internal_p,
    internal_q = internal_q,
    correction_method = "BH",
    q_published = isTRUE(publish_q)
  )
}

triomics_b_direction_fields <- function() {
  list(
    direction_status = "not_applicable",
    direction_reason = "independent_samples_have_no_within_person_delta",
    direction_numerator = NULL,
    direction_denominator = NULL,
    direction_zero_count = NULL,
    n_pairs = NULL
  )
}

triomics_b_optional_number <- function(value) {
  if (length(value) != 1L || !is.finite(value)) NULL else as.numeric(value)
}

triomics_b_n_subjects <- function(subjects, sample_ids, used) {
  n_used <- as.integer(sum(used))
  if (is.null(subjects)) return(n_used)
  chosen <- subjects[sample_ids[used]]
  chosen <- chosen[!is.na(chosen) & nzchar(chosen)]
  if (length(chosen) == 0L) n_used else as.integer(length(unique(chosen)))
}

triomics_b_artifact_status <- function(method_status) {
  if (!is.character(method_status) || length(method_status) != 1L ||
      is.na(method_status) || !nzchar(method_status)) {
    triomics_b_abort(
      "B_RESULT_CONTRACT",
      "Stage B method status is outside the artifact result contract.",
      key = "status"
    )
  }
  if (identical(method_status, "fallback_complete")) return("complete")
  if (identical(method_status, "fallback_partial")) return("partial")
  if (method_status %in% triomics_envelope_statuses()) return(method_status)
  triomics_b_abort(
    "B_RESULT_CONTRACT",
    "Stage B method status is outside the artifact result contract.",
    key = "status"
  )
}

triomics_b_method_metadata <- function(fit) {
  fields <- c(
    "requested_method", "method", "unavailable_reason", "deqms_status",
    "deqms_version", "fallback_applied", "fallback_provenance"
  )
  metadata <- list(method_status = fit$status)
  for (field in fields) {
    if (field %in% names(fit)) metadata[[field]] <- fit[[field]]
  }
  metadata
}

triomics_b_reason_codes <- function(fit) {
  deqms_reason <- if (is.list(fit$deqms_status)) fit$deqms_status$reason else NULL
  reasons <- unlist(list(fit$unavailable_reason, deqms_reason), use.names = FALSE)
  reasons <- as.character(reasons)
  unique(reasons[!is.na(reasons) & nzchar(reasons)])
}

triomics_b_identity_preparation <- function(config) {
  analyses <- config$analyses
  if (length(analyses) == 0L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Stage B needs an analysis.", key = "analyses")
  }
  for (analysis in analyses) {
    block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
    context <- analysis$preparation_context_id
    for (block in config$blocks) {
      if (!block$block_id %in% block_ids) next
      prep <- block$preprocessing
      identity <- !is.null(prep) &&
        identical(prep$context_id, context) &&
        identical(prep$transform$method, "none") &&
        identical(prep$normalisation$method, "none") &&
        identical(prep$imputation$method, "none") &&
        triomics_b_empty(prep$derive)
      if (!identity) {
        triomics_b_abort(
          "B_DESIGN_PREPARATION",
          "Stage B reads the declared measurement matrix and does not refit preparation.",
          key = "preprocessing"
        )
      }
    }
  }
  invisible(TRUE)
}

triomics_b_result_identity_abort <- function(key) {
  triomics_b_abort(
    "B_RESULT_CONTRACT",
    "A Stage B result identity conflicts with its enclosing claim context.",
    key = key
  )
}

triomics_b_result_identity_string <- function(value, key) {
  if (!triomics_b_string(value)) triomics_b_result_identity_abort(key)
  value
}

triomics_b_result_identity_assert <- function(row, field, expected, key) {
  supplied <- row[[field]]
  if (!is.null(supplied) &&
      (!triomics_b_string(supplied) || !identical(supplied, expected))) {
    triomics_b_result_identity_abort(key)
  }
  invisible(TRUE)
}

triomics_b_finalize_public_result_identity <- function(payload) {
  blocks <- payload$blocks
  if (!is.list(blocks) || length(blocks) == 0L) return(payload)
  if (is.null(names(blocks)) || any(!nzchar(names(blocks))) || anyDuplicated(names(blocks))) {
    triomics_b_result_identity_abort("blocks")
  }

  study_id <- triomics_b_result_identity_string(payload$study_id, "study_id")
  analysis_id <- triomics_b_result_identity_string(payload$analysis_id, "analysis_id")
  representation_id <- triomics_b_result_identity_string(payload$representation_id, "representation_id")

  for (block_id in names(blocks)) {
    block <- blocks[[block_id]]
    block_key <- paste0("blocks.", block_id)
    if (!is.list(block)) triomics_b_result_identity_abort(block_key)
    block_id <- triomics_b_result_identity_string(block_id, block_key)
    if (!is.null(block$block_id) &&
        (!triomics_b_string(block$block_id) || !identical(block$block_id, block_id))) {
      triomics_b_result_identity_abort(paste0(block_key, ".block_id"))
    }
    if (!is.null(block$representation_id) &&
        (!triomics_b_string(block$representation_id) ||
         !identical(block$representation_id, representation_id))) {
      triomics_b_result_identity_abort(paste0(block_key, ".representation_id"))
    }

    for (group_name in c("contrasts", "joint_claims")) {
      claims <- block[[group_name]]
      if (!is.list(claims) || length(claims) == 0L) next
      if (is.null(names(claims)) || any(!nzchar(names(claims))) || anyDuplicated(names(claims))) {
        triomics_b_result_identity_abort(paste0(block_key, ".", group_name))
      }
      claim_type <- if (identical(group_name, "joint_claims")) "joint_contrast" else "scalar_contrast"

      for (claim_id in names(claims)) {
        claim_key <- paste(block_key, group_name, claim_id, sep = ".")
        claim <- claims[[claim_id]]
        if (!is.list(claim)) triomics_b_result_identity_abort(claim_key)
        if (is.null(claim$rows) && is.null(claim$family)) next
        if (!is.list(claim$rows) || !is.list(claim$family)) {
          triomics_b_result_identity_abort(claim_key)
        }
        claim_id <- triomics_b_result_identity_string(claim_id, paste0(claim_key, ".claim_id"))
        family_id <- triomics_b_result_identity_string(
          claim$family$family_id, paste0(claim_key, ".family.family_id")
        )
        if (!is.null(claim$claim_type) &&
            (!triomics_b_string(claim$claim_type) || !identical(claim$claim_type, claim_type))) {
          triomics_b_result_identity_abort(paste0(claim_key, ".claim_type"))
        }
        if (!is.null(claim$claim_id) &&
            (!triomics_b_string(claim$claim_id) || !identical(claim$claim_id, claim_id))) {
          triomics_b_result_identity_abort(paste0(claim_key, ".claim_id"))
        }

        contrast_id <- if (identical(claim_type, "joint_contrast")) {
          paste0("joint:", claim_id)
        } else {
          claim_id
        }
        supplied_contrast_id <- claim[["contrast_id"]]
        if (!is.null(supplied_contrast_id) &&
            (!triomics_b_string(supplied_contrast_id) ||
             !identical(supplied_contrast_id, contrast_id))) {
          triomics_b_result_identity_abort(paste0(claim_key, ".contrast_id"))
        }
        contrast_ids <- NULL
        if (identical(claim_type, "joint_contrast")) {
          contrast_ids <- claim$contrast_ids
          if (!is.character(contrast_ids) || length(contrast_ids) < 2L ||
              anyNA(contrast_ids) || any(!nzchar(contrast_ids)) || anyDuplicated(contrast_ids)) {
            triomics_b_result_identity_abort(paste0(claim_key, ".contrast_ids"))
          }
        }

        for (index in seq_along(claim$rows)) {
          row <- claim$rows[[index]]
          row_key <- paste0(claim_key, ".rows.", index)
          if (!is.list(row) || is.null(names(row))) triomics_b_result_identity_abort(row_key)
          feature_key <- triomics_b_result_identity_string(row$feature_key, paste0(row_key, ".feature_key"))
          expected_context <- list(
            study_id = study_id,
            analysis_id = analysis_id,
            block_id = block_id,
            claim_type = claim_type,
            claim_id = claim_id,
            family_id = family_id,
            representation_id = representation_id,
            contrast_id = contrast_id
          )
          for (field in names(expected_context)) {
            triomics_b_result_identity_assert(
              row, field, expected_context[[field]], paste0(row_key, ".", field)
            )
          }
          if (identical(claim_type, "joint_contrast") && !is.null(row$contrast_ids)) {
            supplied_members <- row$contrast_ids
            if (is.list(supplied_members)) supplied_members <- unlist(supplied_members, use.names = FALSE)
            if (!is.character(supplied_members) || !identical(unname(supplied_members), unname(contrast_ids))) {
              triomics_b_result_identity_abort(paste0(row_key, ".contrast_ids"))
            }
          }

          identity <- c(expected_context[c(
            "study_id", "analysis_id", "block_id", "claim_type", "claim_id", "family_id"
          )], list(feature_key = feature_key, representation_id = representation_id,
                   contrast_id = contrast_id))
          if (identical(claim_type, "joint_contrast")) identity$contrast_ids <- as.list(contrast_ids)
          result_id <- triomics_b_hash(identity)
          if (!is.null(row$result_id) &&
              (!triomics_b_string(row$result_id) || !identical(row$result_id, result_id))) {
            triomics_b_result_identity_abort(paste0(row_key, ".result_id"))
          }

          row$study_id <- study_id
          row$analysis_id <- analysis_id
          row$block_id <- block_id
          row$claim_type <- claim_type
          row$claim_id <- claim_id
          row$family_id <- family_id
          row$representation_id <- representation_id
          row$contrast_id <- contrast_id
          if (identical(claim_type, "joint_contrast")) row$contrast_ids <- contrast_ids
          row$result_id <- result_id
          claim$rows[[index]] <- row
        }
        claims[[claim_id]] <- claim
      }
      block[[group_name]] <- claims
    }
    blocks[[block_id]] <- block
  }
  payload$blocks <- blocks
  payload
}

triomics_execute_stage_b <- function(config_path, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Stage B output must be a directory path.", key = "output")
  }
  config <- triomics_read_config(config_path)
  triomics_b_identity_preparation(config)
  ingested <- triomics_ingest_configured_study(config, config_path)
  n_fits <- 0L
  for (analysis in config$analyses) {
    block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
    n_features <- sum(vapply(block_ids, function(block_id) {
      nrow(ingested$assays[[block_id]])
    }, integer(1)))
    n_fits <- n_fits + as.integer(n_features) * as.integer(length(analysis$statistics$contrasts))
  }
  max_fits <- config$execution$max_fits
  if (!is.numeric(max_fits) || length(max_fits) != 1L || is.na(max_fits) || n_fits > max_fits) {
    triomics_b_abort("B_DESIGN_BUDGET", "The declared feature-contrast fits exceed max_fits.",
                     key = "execution.max_fits")
  }
  if (length(config$analyses) != 1L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane writes one analysis envelope.", key = "analyses")
  }
  analysis <- config$analyses[[1L]]
  block_ids <- triomics_b_characters(analysis$block_ids, "block_ids")
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = NULL
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  fit <- triomics_fit_block_models(resolved, analysis$statistics)
  artifact_status <- triomics_b_artifact_status(fit$status)
  method_metadata <- triomics_b_method_metadata(fit)
  payload <- list(
    schema_version = "2.0",
    study_id = ingested$study_id,
    analysis_id = analysis$analysis_id,
    preparation_context_id = analysis$preparation_context_id,
    representation_id = analysis$design$representation,
    status = artifact_status,
    limma_version = fit$limma_version,
    config_sha256 = attr(config, "config_sha256"),
    preparation_refit = FALSE,
    matrix_origin = "declared_measurement_matrix",
    filter_applied = FALSE,
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      limma_version = fit$limma_version
    ),
    blocks = fit$blocks
  )
  payload <- c(payload, method_metadata)
  payload <- triomics_b_finalize_public_result_identity(payload)
  payload_path <- file.path(output, "B", "block-results.json")
  triomics_write_json_atomic(payload, payload_path)
  inputs <- unname(lapply(ingested$source_manifests, function(source) {
    list(artifact_id = source$source_id, sha256 = source$sha256)
  }))
  envelope <- triomics_artifact_envelope(
    artifact_type = "block_results",
    artifact_id = "stage-b-block-results",
    study_id = ingested$study_id,
    run_id = "b-t01",
    status = artifact_status,
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = triomics_b_reason_codes(fit),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  triomics_write_envelope(envelope, file.path(output, "B", "block-results.envelope.json"))
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "b-t01",
    study_id = ingested$study_id,
    stage = "B",
    status = artifact_status
  ), file.path(output, "registries", "run.json"))
  list(status = artifact_status, method_status = fit$status,
       payload_path = payload_path, limma_version = fit$limma_version,
       resolved_matrices = stats::setNames(lapply(block_ids, function(block_id) {
         resolved$blocks[[block_id]]$assay
       }), block_ids))
}
