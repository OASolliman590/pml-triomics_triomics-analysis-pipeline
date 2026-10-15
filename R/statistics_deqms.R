# Packet B-T05: count-dependent precision for the B-T01 levels and B-T03 paired
# lanes. The DEqMS contract is version-qualified below; unsupported requests fail
# closed and an explicit limma fallback remains a separately labeled result.

triomics_b05_required_deqms_version <- "1.30.0"

triomics_b05_unavailable <- function(reason, limma_version = NULL) {
  list(
    status = "unavailable",
    method = "DEqMS",
    requested_method = "DEqMS",
    fallback_applied = FALSE,
    unavailable_reason = reason,
    deqms_status = list(status = "unavailable", reason = reason),
    limma_version = limma_version,
    deqms_version = NULL,
    blocks = list(),
    effects = NULL
  )
}

triomics_b05_read_count_metadata <- function(value) {
  if (is.data.frame(value)) return(list(ok = TRUE, table = value))
  if (is.list(value) && length(value) > 0L) {
    table <- tryCatch(as.data.frame(value, stringsAsFactors = FALSE), error = function(error) NULL)
    if (!is.null(table)) return(list(ok = TRUE, table = table))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(list(ok = FALSE, reason = "count_metadata_missing"))
  }
  candidates <- value
  source_root <- getOption("triomics.source_root")
  if (is.character(source_root) && length(source_root) == 1L && nzchar(source_root) &&
      !grepl("^(?:[A-Za-z]:[/\\\\]|/)", value)) {
    candidates <- unique(c(candidates, file.path(source_root, value)))
  }
  path <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (!length(path)) return(list(ok = FALSE, reason = "count_metadata_missing"))
  table <- tryCatch(
    utils::read.delim(path[[1L]], check.names = FALSE, stringsAsFactors = FALSE,
                      na.strings = c("", "NA"), quote = "\""),
    error = function(error) NULL
  )
  if (is.null(table)) return(list(ok = FALSE, reason = "count_metadata_unreadable"))
  list(ok = TRUE, table = table)
}

triomics_b05_table_hash <- function(table) {
  columns <- sort(names(table))
  keys <- intersect(c("block_id", "feature_key", "sample_id"), columns)
  order_rows <- if (length(keys) == 0L) seq_len(nrow(table)) else {
    do.call(order, c(unname(lapply(keys, function(key) as.character(table[[key]]))), list(na.last = TRUE)))
  }
  records <- lapply(order_rows, function(index) {
    stats::setNames(lapply(columns, function(column) {
      value <- table[[column]][[index]]
      if (length(value) == 0L || is.na(value)) NULL else as.character(value)
    }), columns)
  })
  triomics_b_hash(records)
}

triomics_b05_delta_source_samples <- function(block) {
  unit_ids <- colnames(block$assay)
  unit_map <- block$unit_map
  if (is.null(unit_map) || !is.list(unit_map)) return(NULL)
  samples <- character()
  for (unit_id in unit_ids) {
    item <- NULL
    for (candidate in unit_map) {
      if (identical(as.character(candidate$unit_id), as.character(unit_id))) {
        item <- candidate
        break
      }
    }
    if (is.null(item) || !identical(item$eligibility, "retained") ||
        !is.character(item$source_sample_ids) || length(item$source_sample_ids) == 0L) {
      return(NULL)
    }
    samples <- c(samples, as.character(item$source_sample_ids))
  }
  if (anyNA(samples) || any(!nzchar(samples)) || anyDuplicated(samples)) return(NULL)
  samples
}

triomics_b05_count_identity <- function(resolved_analysis, table) {
  required <- c("feature_key", "acquisition", "provenance")
  count_columns <- intersect(c("peptide_count", "psm_count"), names(table))
  if (!all(required %in% names(table)) || length(count_columns) == 0L) {
    return(list(ok = FALSE, reason = "count_metadata_schema_invalid"))
  }
  if (nrow(table) == 0L) return(list(ok = FALSE, reason = "count_metadata_empty"))
  if (length(resolved_analysis$blocks) > 1L && !"block_id" %in% names(table)) {
    return(list(ok = FALSE, reason = "count_block_identity_required"))
  }
  if (!"block_id" %in% names(table)) table$block_id <- names(resolved_analysis$blocks)[[1L]]
  block_ids <- as.character(table$block_id)
  feature_keys <- as.character(table$feature_key)
  if (anyNA(block_ids) || any(!nzchar(block_ids)) || anyNA(feature_keys) || any(!nzchar(feature_keys))) {
    return(list(ok = FALSE, reason = "count_feature_identity_invalid"))
  }
  if (any(!block_ids %in% names(resolved_analysis$blocks))) {
    return(list(ok = FALSE, reason = "count_block_identity_mismatch"))
  }
  acquisition <- as.character(table$acquisition)
  provenance <- as.character(table$provenance)
  if (anyNA(acquisition) || any(!nzchar(trimws(acquisition))) ||
      anyNA(provenance) || any(!nzchar(trimws(provenance)))) {
    return(list(ok = FALSE, reason = "count_provenance_missing"))
  }
  has_sample <- "sample_id" %in% names(table)
  sample_ids <- if (has_sample) as.character(table$sample_id) else NULL
  if (has_sample && (anyNA(sample_ids) || any(!nzchar(sample_ids)))) {
    return(list(ok = FALSE, reason = "count_sample_identity_invalid"))
  }
  raw_counts <- lapply(count_columns, function(column) {
    suppressWarnings(as.numeric(as.character(table[[column]])))
  })
  names(raw_counts) <- count_columns
  present <- lapply(raw_counts, function(values) !is.na(values))
  if (length(present) > 1L && any(present[[1L]] & present[[2L]])) {
    return(list(ok = FALSE, reason = "count_type_ambiguous"))
  }
  any_count <- Reduce(`|`, present)
  if (any(!any_count)) return(list(ok = FALSE, reason = "count_values_missing"))
  chosen <- if (length(present) == 1L) rep(count_columns[[1L]], nrow(table)) else {
    ifelse(present[[1L]], count_columns[[1L]], count_columns[[2L]])
  }
  if (length(unique(chosen)) != 1L) return(list(ok = FALSE, reason = "count_type_mixed"))
  count_values <- raw_counts[[unique(chosen)]]
  if (any(!is.finite(count_values)) || any(count_values <= 0) ||
      any(abs(count_values - round(count_values)) > 1e-8)) {
    return(list(ok = FALSE, reason = "count_values_not_positive_whole_numbers"))
  }
  table$count_value <- count_values
  table$count_type <- unique(chosen)
  table$feature_key <- feature_keys
  table$block_id <- block_ids
  table$acquisition <- acquisition
  table$provenance <- provenance
  if (has_sample) table$sample_id <- sample_ids
  key_columns <- c("block_id", "feature_key", if (has_sample) "sample_id")
  row_key <- do.call(paste, c(unname(lapply(key_columns, function(name) table[[name]])), sep = "\r"))
  if (anyDuplicated(row_key)) return(list(ok = FALSE, reason = "count_identity_duplicate"))

  table_hash <- triomics_b05_table_hash(table)
  counts <- vector("list", length(resolved_analysis$blocks))
  names(counts) <- names(resolved_analysis$blocks)
  for (block_id in names(resolved_analysis$blocks)) {
    block <- resolved_analysis$blocks[[block_id]]
    rows <- table[table$block_id == block_id, , drop = FALSE]
    if (nrow(rows) == 0L) return(list(ok = FALSE, reason = "count_block_missing"))
    expected_features <- as.character(block$feature_keys)
    if (anyDuplicated(expected_features) || !setequal(rows$feature_key, expected_features)) {
      return(list(ok = FALSE, reason = "count_feature_identity_mismatch"))
    }
    if (!has_sample) {
      if (identical(resolved_analysis$representation, "delta") ||
          identical(resolved_analysis$representation_id, "delta")) {
        return(list(ok = FALSE, reason = "delta_sample_count_identity_required"))
      }
      order_rows <- match(expected_features, rows$feature_key)
      block_rows <- rows[order_rows, , drop = FALSE]
      sample_order <- NULL
      matrix_counts <- matrix_acquisition <- matrix_provenance <- NULL
      summary <- list(
        method = "source_reported_feature_count",
        sample_ids = NULL,
        aggregation = "none",
        sample_scope = "source_reported_feature_count",
        justification = paste(
          "The source supplied one positive peptide/PSM count per fitted feature with",
          "acquisition and provenance; no value is inferred from abundance or pooled here."
        )
      )
    } else {
      if (identical(resolved_analysis$representation, "delta") ||
          identical(resolved_analysis$representation_id, "delta")) {
        sample_order <- triomics_b05_delta_source_samples(block)
        if (is.null(sample_order) || length(sample_order) == 0L) {
          return(list(ok = FALSE, reason = "delta_source_sample_identity_missing"))
        }
        known_samples <- unique(unlist(lapply(block$unit_map, function(item) item$source_sample_ids),
                                       use.names = FALSE))
        if (any(!rows$sample_id %in% known_samples)) {
          return(list(ok = FALSE, reason = "count_sample_identity_mismatch"))
        }
        summary_method <- "minimum_over_feature_specific_complete_pair_observations"
        justification <- paste(
          "DEqMS needs one count per fitted feature. The official LFQ workflow uses",
          "the minimum count across quantitative samples; this delta applies that",
          "conservative minimum only to source observations contributing to each",
          "feature's fitted complete pairs. Per-feature source sample IDs are recorded",
          "in that feature's count_prior_ref."
        )
      } else {
        sample_order <- as.character(block$sample_ids)
        if (anyNA(sample_order) || any(!nzchar(sample_order)) || any(!rows$sample_id %in% sample_order)) {
          return(list(ok = FALSE, reason = "count_sample_identity_mismatch"))
        }
        summary_method <- "minimum_over_feature_specific_samples_used_by_fit"
        justification <- paste(
          "DEqMS needs one count per fitted feature. The official LFQ workflow uses",
          "the minimum count across quantitative samples; only source samples actually",
          "used for each feature's fit contribute to that feature's minimum. Per-feature",
          "source sample IDs are recorded in that feature's count_prior_ref."
        )
      }
      matrix_counts <- matrix(NA_real_, nrow = length(expected_features), ncol = length(sample_order),
                              dimnames = list(expected_features, sample_order))
      matrix_acquisition <- matrix(NA_character_, nrow = nrow(matrix_counts), ncol = ncol(matrix_counts),
                                   dimnames = dimnames(matrix_counts))
      matrix_provenance <- matrix(NA_character_, nrow = nrow(matrix_counts), ncol = ncol(matrix_counts),
                                  dimnames = dimnames(matrix_counts))
      for (row in seq_len(nrow(rows))) {
        sample_id <- rows$sample_id[[row]]
        if (!sample_id %in% sample_order) next
        matrix_counts[rows$feature_key[[row]], sample_id] <- rows$count_value[[row]]
        matrix_acquisition[rows$feature_key[[row]], sample_id] <- rows$acquisition[[row]]
        matrix_provenance[rows$feature_key[[row]], sample_id] <- rows$provenance[[row]]
      }
      if (anyNA(matrix_counts) || anyNA(matrix_acquisition) || anyNA(matrix_provenance)) {
        return(list(ok = FALSE, reason = "count_sample_identity_incomplete"))
      }
      summary <- list(
        method = summary_method,
        sample_ids = NULL,
        aggregation = "feature_specific_minimum",
        sample_scope = "per_feature_model_support",
        justification = justification
      )
    }
    if (!has_sample) {
      block_rows$acquisition <- as.character(block_rows$acquisition)
      block_rows$provenance <- as.character(block_rows$provenance)
    }
    values <- if (has_sample) NULL else stats::setNames(as.numeric(block_rows$count_value), expected_features)
    acquisition_values <- if (has_sample) NULL else stats::setNames(
      lapply(block_rows$acquisition, as.character), expected_features
    )
    provenance_values <- if (has_sample) NULL else stats::setNames(
      lapply(block_rows$provenance, as.character), expected_features
    )
    counts[[block_id]] <- list(
      feature_keys = expected_features,
      values = values,
      count_type = unique(as.character(rows$count_type))[[1L]],
      acquisition = acquisition_values,
      provenance = provenance_values,
      table_hash = table_hash,
      summary = summary,
      sample_ids = sample_order,
      count_matrix = matrix_counts,
      acquisition_matrix = matrix_acquisition,
      provenance_matrix = matrix_provenance
    )
  }
  list(ok = TRUE, counts = counts)
}

triomics_b05_design <- function(resolved_analysis, block) {
  design <- block$design
  if (is.null(design)) {
    design <- triomics_b_indicator(
      block$groups,
      resolved_analysis$model_plan$levels,
      resolved_analysis$model_plan$column_ids
    )
  }
  if (is.null(colnames(design)) || !identical(colnames(design), resolved_analysis$model_plan$column_ids)) {
    return(NULL)
  }
  if (is.null(rownames(design))) rownames(design) <- colnames(block$assay)
  if (!identical(rownames(design), colnames(block$assay))) return(NULL)
  design
}

triomics_b05_weight_list <- function(statistics_spec, column_ids) {
  contrasts <- statistics_spec$contrasts
  if (!is.list(contrasts) || length(contrasts) == 0L) return(NULL)
  ids <- vapply(contrasts, function(contrast) {
    if (!triomics_b_string(contrast$contrast_id)) return(NA_character_)
    as.character(contrast$contrast_id)
  }, character(1L))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) return(NULL)
  weights <- tryCatch(
    lapply(contrasts, triomics_b_contrast_weights, column_ids = column_ids),
    error = function(error) NULL
  )
  if (is.null(weights)) return(NULL)
  names(weights) <- ids
  weights
}

triomics_b05_feature_count_records <- function(resolved_analysis, block, counts, supports) {
  feature_keys <- as.character(block$feature_keys)
  values <- stats::setNames(rep(NA_real_, length(feature_keys)), feature_keys)
  per_feature <- stats::setNames(vector("list", length(feature_keys)), feature_keys)
  is_delta <- identical(resolved_analysis$representation_id, "delta")
  unit_map <- block$unit_map
  for (index in seq_along(feature_keys)) {
    feature_key <- feature_keys[[index]]
    if (is.null(counts$count_matrix)) {
      count <- as.numeric(counts$values[[feature_key]])
      acquisition <- as.character(counts$acquisition[[feature_key]])
      provenance <- as.character(counts$provenance[[feature_key]])
      sample_ids <- NULL
    } else {
      used_units <- colnames(block$assay)[as.logical(supports[[index]]$used)]
      source_sample_ids <- if (is_delta) {
        unlist(lapply(used_units, function(unit_id) {
          item <- NULL
          for (candidate in unit_map) {
            if (identical(as.character(candidate$unit_id), as.character(unit_id))) {
              item <- candidate
              break
            }
          }
          if (is.null(item)) character() else as.character(item$source_sample_ids)
        }), use.names = FALSE)
      } else {
        as.character(used_units)
      }
      sample_ids <- counts$sample_ids[counts$sample_ids %in% source_sample_ids]
      if (length(sample_ids) == 0L) {
        count <- NA_real_
        acquisition <- character()
        provenance <- character()
      } else {
        count <- min(counts$count_matrix[feature_key, sample_ids])
        acquisition <- unique(as.character(counts$acquisition_matrix[feature_key, sample_ids]))
        provenance <- unique(as.character(counts$provenance_matrix[feature_key, sample_ids]))
      }
    }
    values[[feature_key]] <- count
    per_feature[[feature_key]] <- list(
      count = count,
      acquisition = acquisition,
      provenance = provenance,
      sample_ids = sample_ids
    )
  }
  list(values = values, per_feature = per_feature)
}

triomics_b05_row_count_reference <- function(counts, feature_key, feature_counts) {
  record <- feature_counts$per_feature[[feature_key]]
  list(
    method = "DEqMS",
    metadata_sha256 = counts$table_hash,
    feature_key = feature_key,
    count_type = counts$count_type,
    count = if (is.finite(record$count)) as.numeric(record$count) else NULL,
    acquisition = record$acquisition,
    provenance = record$provenance,
    summary_method = counts$summary$method,
    sample_ids = record$sample_ids
  )
}

triomics_b05_deqms_block <- function(resolved_analysis, statistics_spec, block, counts) {
  design <- triomics_b05_design(resolved_analysis, block)
  if (is.null(design)) return(list(ok = FALSE, reason = "model_design_identity_mismatch"))
  weight_list <- triomics_b05_weight_list(statistics_spec, colnames(design))
  if (is.null(weight_list)) return(list(ok = FALSE, reason = "contrast_spec_invalid"))
  contrast_ids <- names(weight_list)
  contrast_matrix <- matrix(0, nrow = ncol(design), ncol = length(weight_list),
                            dimnames = list(colnames(design), contrast_ids))
  for (index in seq_along(weight_list)) contrast_matrix[, index] <- weight_list[[index]]
  feature_keys <- as.character(block$feature_keys)
  supports <- lapply(seq_along(block$feature_keys), function(index) {
    triomics_b_feature_support(as.numeric(block$assay[index, ]), design, weight_list)
  })
  eligible <- vapply(supports, function(support) isTRUE(support$eligible), logical(1L))
  if (!any(eligible)) return(list(ok = FALSE, reason = "no_estimable_features"))
  feature_counts <- triomics_b05_feature_count_records(resolved_analysis, block, counts, supports)
  if (any(!is.finite(feature_counts$values[eligible]))) {
    return(list(ok = FALSE, reason = "count_support_unavailable"))
  }
  model_hash <- triomics_b_hash(list(
    formula = resolved_analysis$model_plan$formula,
    column_ids = colnames(design),
    sample_ids = rownames(design),
    design = unname(design),
    representation = resolved_analysis$representation_id
  ))
  fit <- tryCatch({
    limma_fit <- limma::lmFit(block$assay[eligible, , drop = FALSE], design)
    moderated <- limma::eBayes(
      limma::contrasts.fit(limma_fit, contrast_matrix),
      trend = FALSE,
      robust = isTRUE(statistics_spec$robust)
    )
    moderated$count <- stats::setNames(
      as.numeric(feature_counts$values[rownames(moderated$coefficients)]),
      rownames(moderated$coefficients)
    )
    deqms_fit <- DEqMS::spectraCounteBayes(moderated)
    outputs <- lapply(seq_along(contrast_ids), function(index) {
      DEqMS::outputResult(deqms_fit, coef_col = index)
    })
    names(outputs) <- contrast_ids
    list(fit = deqms_fit, outputs = outputs)
  }, error = function(error) NULL)
  if (is.null(fit) || !all(vapply(fit$outputs, function(output) {
    is.data.frame(output) && all(c("gene", "sca.t", "sca.P.Value") %in% names(output))
  }, logical(1L)))) {
    return(list(ok = FALSE, reason = "deqms_fit_or_output_invalid"))
  }
  if (!all(vapply(fit$outputs, function(output) {
    all(is.finite(as.numeric(output$sca.t))) &&
      all(is.finite(as.numeric(output$sca.P.Value))) &&
      all(as.numeric(output$sca.P.Value) >= 0 & as.numeric(output$sca.P.Value) <= 1)
  }, logical(1L)))) {
    return(list(ok = FALSE, reason = "deqms_method_outputs_invalid"))
  }
  local_features <- rownames(fit$fit$coefficients)
  if (is.null(local_features) || anyDuplicated(local_features) ||
      !setequal(local_features, feature_keys[eligible])) {
    return(list(ok = FALSE, reason = "deqms_feature_identity_mismatch"))
  }
  list(
    ok = TRUE,
    fit = fit$fit,
    outputs = fit$outputs,
    supports = supports,
    eligible = eligible,
    model_hash = model_hash,
    design = design,
    weights = weight_list,
    contrast_ids = contrast_ids,
    feature_counts = feature_counts
  )
}

triomics_b05_rows <- function(resolved_analysis, statistics_spec, block, counts, fitted) {
  feature_keys <- as.character(block$feature_keys)
  n_features <- length(feature_keys)
  local_features <- rownames(fitted$fit$coefficients)
  local_of <- stats::setNames(seq_along(local_features), local_features)
  block_results <- vector("list", length(fitted$contrast_ids))
  names(block_results) <- fitted$contrast_ids
  publish_q <- identical(statistics_spec$selection_validity, "qualified")
  for (contrast_index in seq_along(fitted$contrast_ids)) {
    contrast_id <- fitted$contrast_ids[[contrast_index]]
    output <- fitted$outputs[[contrast_id]]
    output_index <- match(feature_keys[fitted$eligible], as.character(output$gene))
    if (anyNA(output_index)) {
      return(list(ok = FALSE, reason = "deqms_output_feature_identity_mismatch"))
    }
    p_values <- rep(NA_real_, n_features)
    estimated <- fitted$eligible
    p_values[estimated] <- as.numeric(output$sca.P.Value[output_index])
    family <- triomics_b_family(
      resolved_analysis$study_id, resolved_analysis$analysis_id, block$block_id,
      contrast_id, feature_keys, estimated, p_values, publish_q
    )
    rows <- vector("list", n_features)
    weight <- fitted$weights[[contrast_id]]
    contrast_hash <- triomics_b_hash(list(contrast_id = contrast_id, weights = as.list(weight)))
    for (index in seq_len(n_features)) {
      support <- fitted$supports[[index]]
      feature_key <- feature_keys[[index]]
      estimated_one <- isTRUE(estimated[[index]])
      method_index <- if (estimated_one) match(feature_key, as.character(output$gene)) else NA_integer_
      local <- if (estimated_one) unname(local_of[[feature_key]]) else NA_integer_
      effect <- if (estimated_one) as.numeric(fitted$fit$coefficients[feature_key, contrast_index]) else NULL
      sca_t <- if (estimated_one) as.numeric(output$sca.t[[method_index]]) else NULL
      sca_p <- if (estimated_one) as.numeric(output$sca.P.Value[[method_index]]) else NULL
      posterior_variance <- if (estimated_one) triomics_b_gene_value(fitted$fit$sca.postvar, local) else NULL
      unscaled <- if (estimated_one) triomics_b_at(
        fitted$fit$stdev.unscaled, local, contrast_index, sum(estimated), length(fitted$contrast_ids)
      ) else NULL
      sca_df_prior <- if (estimated_one) triomics_b_gene_value(fitted$fit$sca.dfprior, local) else NULL
      moderated_se <- if (estimated_one) sqrt(posterior_variance) * unscaled else NULL
      reason <- if (estimated_one) NULL else if (!isTRUE(support$estimable[[contrast_index]])) {
        "contrast_not_estimable"
      } else if (support$df < 1L) {
        "zero_residual_support"
      } else {
        "deqms_fit_failed"
      }
      q_value <- if (estimated_one && publish_q) family$internal_q[[index]] else NULL
      used_samples <- as.integer(sum(support$used))
      if (identical(resolved_analysis$representation, "delta") ||
          identical(resolved_analysis$representation_id, "delta")) {
        n_samples <- as.integer(2L * used_samples)
        n_subjects <- used_samples
      } else {
        n_samples <- as.integer(length(block$sample_ids))
        n_subjects <- triomics_b_n_subjects(resolved_analysis$subjects, block$sample_ids, support$used)
      }
      row <- list(
        feature_key = feature_key,
        effect = if (estimated_one) effect else NULL,
        effect_unit = statistics_spec$effect_unit,
        ordinary_se = if (estimated_one) as.numeric(support$ordinary_se[[contrast_index]]) else NULL,
        moderated_se = if (estimated_one) as.numeric(moderated_se) else NULL,
        statistic = sca_t,
        statistic_name = if (estimated_one) "DEqMS_sca.t" else NULL,
        ordinary_df = if (isTRUE(support$estimable[[contrast_index]])) as.integer(support$df) else NULL,
        effective_test_df = if (estimated_one) as.numeric(sca_df_prior + fitted$fit$df.residual[[local]]) else NULL,
        df_prior = if (estimated_one) as.numeric(sca_df_prior) else NULL,
        p = sca_p,
        q = q_value,
        inferential_status = triomics_b_inferential_status(estimated_one, statistics_spec$selection_validity),
        estimation_status = if (estimated_one) "estimated" else "failed",
        estimation_reason = reason,
        n_samples = n_samples,
        n_subjects = as.integer(n_subjects),
        n_rows_used = as.integer(support$n_used),
        count_prior_ref = triomics_b05_row_count_reference(counts, feature_key, fitted$feature_counts),
        source_hash = block$source_hash,
        model_hash = fitted$model_hash,
        representation_hash = triomics_b_hash(list(
          representation = resolved_analysis$representation_id,
          scale = block$scales[[index]], feature_key = feature_key
        )),
        contrast_hash = contrast_hash,
        method_id = "DEqMS",
        method_statistic_field = "sca.t",
        method_p_field = "sca.P.Value",
        deqms_sca_t = sca_t,
        deqms_sca_p = sca_p
      )
      if (identical(resolved_analysis$representation, "delta") ||
          identical(resolved_analysis$representation_id, "delta")) {
        kind <- triomics_b03_contrast_kind(weight)
        direction <- triomics_b03_direction(
          as.numeric(block$assay[index, ]), block, resolved_analysis$model_plan, kind,
          effect, estimated_one, length(resolved_analysis$model_plan$covariates) > 0L
        )
        row$n_pairs <- used_samples
        row$n_analysis_rows <- block$n_analysis_rows
        row$representation_id <- "delta"
        row$claim_type <- "scalar_contrast"
        row$coefficient_interpretation <- kind
        row$direction_status <- direction$direction_status
        row$direction_reason <- direction$direction_reason
        row$direction_numerator <- direction$direction_numerator
        row$direction_denominator <- direction$direction_denominator
        row$direction_zero_count <- direction$direction_zero_count
        row$direction_unavailable_count <- direction$direction_unavailable_count
        row$direction_groups <- direction$direction_groups
      } else {
        row$representation_id <- "levels"
        row$claim_type <- "scalar_contrast"
        row <- c(row, triomics_b_direction_fields())
      }
      rows[[index]] <- row
    }
    block_results[[contrast_id]] <- list(
      contrast_id = contrast_id,
      rows = rows,
      family = family,
      count_prior_status = "deqms_count_prior_qualified",
      count_summary = counts$summary,
      sensitivity = if (identical(resolved_analysis$representation, "delta") ||
                        identical(resolved_analysis$representation_id, "delta")) {
        triomics_b03_sensitivity(
          statistics_spec, resolved_analysis$model_plan, block, weight,
          triomics_b03_contrast_kind(weight)
        )
      } else if (isTRUE(statistics_spec$rank_sensitivity)) {
        triomics_b_wilcoxon(block, resolved_analysis$model_plan, weight)
      } else {
        list(status = "not_requested")
      }
    )
  }
  list(ok = TRUE, contrasts = block_results)
}

triomics_b05_fallback <- function(resolved_analysis, statistics_spec, original, reason) {
  limma_spec <- statistics_spec
  limma_spec$method <- "limma"
  limma_spec$count_metadata <- NULL
  limma_spec$fallback <- "none"
  fit <- tryCatch(original(resolved_analysis, limma_spec), error = function(error) NULL)
  if (is.null(fit)) {
    unavailable <- triomics_b05_unavailable(reason,
      if (requireNamespace("limma", quietly = TRUE)) as.character(utils::packageVersion("limma")) else NULL
    )
    return(unavailable)
  }
  for (block_id in names(fit$blocks)) {
    block_result <- fit$blocks[[block_id]]
    for (contrast_id in names(block_result$contrasts)) {
      result <- block_result$contrasts[[contrast_id]]
      result$method_id <- "limma_explicit_fallback"
      result$requested_method <- "DEqMS"
      result$fallback_applied <- TRUE
      result$count_prior_status <- "not_used_limma_fallback"
      if (!is.null(result$rows)) {
        result$rows <- lapply(result$rows, function(row) {
          row$method_id <- "limma_explicit_fallback"
          row$count_prior_ref <- NULL
          row$deqms_sca_t <- NULL
          row$deqms_sca_p <- NULL
          row$method_statistic_field <- NULL
          row$method_p_field <- NULL
          row
        })
      }
      block_result$contrasts[[contrast_id]] <- result
    }
    fit$blocks[[block_id]] <- block_result
  }
  fit$status <- if (identical(fit$status, "complete")) "fallback_complete" else "fallback_partial"
  fit$method <- "limma_explicit_fallback"
  fit$requested_method <- "DEqMS"
  fit$fallback_applied <- TRUE
  fit$deqms_status <- list(status = "unavailable", reason = reason)
  fit$fallback_provenance <- list(
    source_method = "limma",
    status = "applied",
    trigger_reason = reason,
    user_declared = TRUE,
    interpretation = "These are limma results from an explicit fallback, not DEqMS results."
  )
  fit
}

triomics_b05_fit <- function(resolved_analysis, statistics_spec, original) {
  limma_version <- if (requireNamespace("limma", quietly = TRUE)) {
    as.character(utils::packageVersion("limma"))
  } else {
    NULL
  }
  unavailable <- NULL
  if (!requireNamespace("DEqMS", quietly = TRUE)) {
    unavailable <- triomics_b05_unavailable("deqms_not_installed", limma_version)
  } else if (!identical(as.character(utils::packageVersion("DEqMS")), triomics_b05_required_deqms_version)) {
    unavailable <- triomics_b05_unavailable("deqms_version_unqualified", limma_version)
  } else if (!requireNamespace("limma", quietly = TRUE)) {
    unavailable <- triomics_b05_unavailable("limma_not_installed", NULL)
  } else if (!identical(statistics_spec$trend, FALSE) ||
             !is.logical(statistics_spec$robust) || length(statistics_spec$robust) != 1L ||
             is.na(statistics_spec$robust)) {
    unavailable <- triomics_b05_unavailable("statistics_spec_unqualified", limma_version)
  } else if (!statistics_spec$selection_validity %in% c("qualified", "descriptive_only", "unverified")) {
    unavailable <- triomics_b05_unavailable("selection_validity_unqualified", limma_version)
  } else if (is.null(resolved_analysis$lane) || identical(resolved_analysis$lane, "b_t03")) {
    scales <- unlist(lapply(resolved_analysis$blocks, function(block) block$scales), use.names = FALSE)
    delta <- identical(resolved_analysis$representation, "delta") ||
      identical(resolved_analysis$representation_id, "delta")
    required <- tryCatch({
      if (delta) {
        unique(vapply(scales, triomics_b03_required_unit, character(1L)))
      } else {
        unique(vapply(scales, triomics_b_required_effect_unit, character(1L)))
      }
    }, error = function(error) character())
    if (length(required) != 1L || !nzchar(required[[1L]]) ||
        !identical(statistics_spec$effect_unit, required[[1L]])) {
      unavailable <- triomics_b05_unavailable("effect_unit_mismatch", limma_version)
    } else {
      statistics_spec$effect_unit <- required[[1L]]
      read <- triomics_b05_read_count_metadata(statistics_spec$count_metadata)
      if (!isTRUE(read$ok)) {
        unavailable <- triomics_b05_unavailable(read$reason, limma_version)
      } else {
        aligned <- triomics_b05_count_identity(resolved_analysis, read$table)
        if (!isTRUE(aligned$ok)) unavailable <- triomics_b05_unavailable(aligned$reason, limma_version)
      }
    }
  } else {
    unavailable <- triomics_b05_unavailable("design_lane_not_supported", limma_version)
  }
  if (!is.null(unavailable)) {
    if (identical(statistics_spec$fallback, "explicit_limma")) {
      return(triomics_b05_fallback(
        resolved_analysis, statistics_spec, original, unavailable$unavailable_reason
      ))
    }
    return(unavailable)
  }
  counts <- aligned$counts
  blocks <- vector("list", length(resolved_analysis$blocks))
  names(blocks) <- names(resolved_analysis$blocks)
  all_estimated <- TRUE
  for (block_id in names(resolved_analysis$blocks)) {
    block <- resolved_analysis$blocks[[block_id]]
    fitted <- triomics_b05_deqms_block(
      resolved_analysis, statistics_spec, block, counts[[block_id]]
    )
    if (!isTRUE(fitted$ok)) {
      unavailable <- triomics_b05_unavailable(fitted$reason, limma_version)
      if (identical(statistics_spec$fallback, "explicit_limma")) {
        return(triomics_b05_fallback(
          resolved_analysis, statistics_spec, original, unavailable$unavailable_reason
        ))
      }
      return(unavailable)
    }
    result <- triomics_b05_rows(resolved_analysis, statistics_spec, block, counts[[block_id]], fitted)
    if (!isTRUE(result$ok)) {
      unavailable <- triomics_b05_unavailable(result$reason, limma_version)
      if (identical(statistics_spec$fallback, "explicit_limma")) {
        return(triomics_b05_fallback(
          resolved_analysis, statistics_spec, original, unavailable$unavailable_reason
        ))
      }
      return(unavailable)
    }
    if (any(vapply(result$contrasts, function(contrast) {
      any(vapply(contrast$rows, function(row) !identical(row$estimation_status, "estimated"), logical(1L)))
    }, logical(1L)))) all_estimated <- FALSE
    blocks[[block_id]] <- list(
      block_id = block_id,
      representation_id = resolved_analysis$representation_id,
      count_summary = counts[[block_id]]$summary,
      contrasts = result$contrasts
    )
  }
  list(
    status = if (all_estimated) "complete" else "partial",
    method = "DEqMS",
    requested_method = "DEqMS",
    fallback_applied = FALSE,
    limma_version = limma_version,
    deqms_version = as.character(utils::packageVersion("DEqMS")),
    deqms_api = list(
      fit = "limma::lmFit + limma::contrasts.fit + limma::eBayes(trend=FALSE)",
      count_prior = "DEqMS::spectraCounteBayes",
      result_extraction = "DEqMS::outputResult",
      result_statistic_field = "sca.t",
      result_p_field = "sca.P.Value"
    ),
    blocks = blocks,
    effects = NULL
  )
}

triomics_b05_dispatch_fit <- function(original, parent) {
  force(original)
  env <- new.env(parent = parent)
  env$b_t05_original <- original
  dispatch <- function(resolved_analysis, statistics_spec) {
    supported_lane <- is.list(resolved_analysis) && (
      identical(resolved_analysis$lane, "b_t03") ||
        is.null(resolved_analysis$lane)
    )
    if (isTRUE(is.list(statistics_spec) && identical(statistics_spec$method, "deqms") && supported_lane)) {
      triomics_b05_fit(
        resolved_analysis, statistics_spec,
        get("b_t05_original", envir = environment(), inherits = TRUE)
      )
    } else {
      get("b_t05_original", envir = environment(), inherits = TRUE)(
        resolved_analysis, statistics_spec
      )
    }
  }
  environment(dispatch) <- env
  attr(dispatch, "b_t05_dispatch") <- TRUE
  # The packet dispatcher wraps the existing lane dispatchers and delegates
  # their behavior for requests outside B-T05. Mark those lanes as composed so
  # startup hooks do not wrap B-T05 again after it has been installed.
  attr(dispatch, "b_t02_dispatch") <- TRUE
  attr(dispatch, "b_t03_dispatch") <- TRUE
  attr(dispatch, "b_t04_dispatch") <- TRUE
  dispatch
}

triomics_b05_rebind <- function(envir, name, value) {
  if (!exists(name, envir = envir, inherits = FALSE)) return(invisible(FALSE))
  if (base::bindingIsLocked(name, envir)) base::unlockBinding(name, envir)
  assign(name, value, envir = envir)
  try(base::lockBinding(name, envir), silent = TRUE)
  invisible(TRUE)
}

triomics_b05_install_namespace_dispatch <- function(pkgname) {
  envir <- asNamespace(pkgname)
  if (exists("triomics_b02_install_namespace_dispatch", envir = envir, inherits = TRUE)) {
    get("triomics_b02_install_namespace_dispatch", envir = envir)(pkgname)
  }
  if (exists("triomics_b03_install_namespace_dispatch", envir = envir, inherits = TRUE)) {
    get("triomics_b03_install_namespace_dispatch", envir = envir)(pkgname)
  }
  if (!exists("triomics_fit_block_models", envir = envir, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = envir, inherits = FALSE)
  if (isTRUE(attr(current, "b_t05_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b05_dispatch_fit(current, envir)
  triomics_b05_rebind(envir, "triomics_fit_block_models", dispatch)
  triomics_b05_rebind(envir, "fit_block_models", dispatch)
  invisible(TRUE)
}

triomics_b05_install_global_dispatch <- function() {
  if (exists("triomics_b02_install_global_dispatch", envir = .GlobalEnv, inherits = FALSE)) {
    triomics_b02_install_global_dispatch()
  }
  if (exists("triomics_b03_install_global_dispatch", envir = .GlobalEnv, inherits = FALSE)) {
    triomics_b03_install_global_dispatch()
  }
  if (exists("triomics_b04_install_global_dispatch", envir = .GlobalEnv, inherits = FALSE)) {
    triomics_b04_install_global_dispatch()
  }
  if (!exists("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)
  if (isTRUE(attr(current, "b_t05_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b05_dispatch_fit(current, .GlobalEnv)
  assign("triomics_fit_block_models", dispatch, envir = .GlobalEnv)
  assign("fit_block_models", dispatch, envir = .GlobalEnv)
  invisible(TRUE)
}

if (exists(".onLoad", inherits = FALSE)) {
  triomics_b05_on_load_previous <- get(".onLoad", inherits = FALSE)
  .onLoad <- function(libname, pkgname) {
    triomics_b05_on_load_previous(libname, pkgname)
    triomics_b05_install_namespace_dispatch(pkgname)
  }
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_main_before_b05 <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  assign("triomics_main_before_b05", triomics_main_before_b05, envir = .GlobalEnv)
  assign("triomics_main", function(args) {
    triomics_b05_install_global_dispatch()
    triomics_main_before_b05(args)
  }, envir = .GlobalEnv)
}
