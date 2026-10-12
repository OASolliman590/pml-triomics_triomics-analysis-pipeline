triomics_b_contrast_weights <- function(contrast, column_ids) {
  weights <- contrast$weights
  names_ok <- !is.null(weights) && !is.null(names(weights)) &&
    length(weights) == length(column_ids) && !anyDuplicated(names(weights)) &&
    setequal(names(weights), column_ids)
  if (!names_ok) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Contrast weights must match the model column ids by name.",
                     key = "statistics.contrasts")
  }
  numeric_weights <- suppressWarnings(as.numeric(unlist(weights[column_ids])))
  if (length(numeric_weights) != length(column_ids) || any(!is.finite(numeric_weights))) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Contrast weights must be finite numbers.",
                     key = "statistics.contrasts")
  }
  stats::setNames(numeric_weights, column_ids)
}

triomics_b_indicator <- function(groups, levels, column_ids) {
  design <- matrix(0, length(groups), length(levels), dimnames = list(NULL, column_ids))
  for (index in seq_along(levels)) design[groups == levels[[index]], index] <- 1
  design
}

triomics_b_estimable <- function(design, weights) {
  if (nrow(design) == 0L) return(FALSE)
  fitted <- qr.fitted(qr(t(design)), weights)
  all(abs(as.numeric(fitted) - as.numeric(weights)) <= 1e-8)
}

triomics_b_feature_support <- function(y, design, weight_list) {
  ok <- is.finite(y)
  used <- design[ok, , drop = FALSE]
  used_y <- as.numeric(y[ok])
  n_used <- length(used_y)
  n_coef <- ncol(design)
  rank <- if (n_used == 0L) 0L else qr(used)$rank
  df <- n_used - rank
  coefficients <- rep(NA_real_, n_coef)
  if (n_used > 0L) coefficients <- suppressWarnings(qr.coef(qr(used), used_y))
  n_contrast <- length(weight_list)
  estimable <- logical(n_contrast)
  effects <- rep(NA_real_, n_contrast)
  ordinary_se <- rep(NA_real_, n_contrast)
  for (j in seq_len(n_contrast)) {
    weights <- weight_list[[j]]
    estimable[[j]] <- triomics_b_estimable(used, weights)
    if (!estimable[[j]]) next
    coef <- coefficients
    coef[is.na(coef)] <- 0
    effects[[j]] <- sum(weights * coef)
    if (df >= 1 && rank == n_coef) {
      residual <- used_y - as.numeric(qr.fitted(qr(used), used_y))
      sigma2 <- sum(residual^2) / df
      variance <- sum(weights * solve(crossprod(used), weights))
      ordinary_se[[j]] <- sqrt(sigma2 * variance)
    }
  }
  list(
    n_used = n_used,
    used = ok,
    rank = rank,
    df = df,
    estimable = estimable,
    effects = effects,
    ordinary_se = ordinary_se,
    eligible = isTRUE(rank == n_coef && df >= 1 && all(estimable))
  )
}

triomics_b_close <- function(actual, expected) {
  is.finite(actual) && is.finite(expected) &&
    abs(actual - expected) <= 1e-8 + 1e-6 * abs(expected)
}

triomics_b_at <- function(value, gene, contrast, n_gene, n_contrast) {
  if (is.null(dim(value))) {
    flat <- as.numeric(value)
    if (length(flat) == 1L) return(flat[[1L]])
    if (n_gene == 1L) return(flat[[contrast]])
    if (n_contrast == 1L) return(flat[[gene]])
    return(flat[[(contrast - 1L) * n_gene + gene]])
  }
  as.numeric(value[gene, contrast])
}

triomics_b_gene_value <- function(value, gene) {
  flat <- as.numeric(value)
  if (length(flat) == 1L) flat[[1L]] else flat[[gene]]
}

triomics_b_wilcoxon <- function(block, plan, weights) {
  reference <- plan$reference_level
  target <- plan$target_level
  positive_target <- weights[[paste0("condition.", target)]] > weights[[paste0("condition.", reference)]]
  features <- vector("list", length(block$feature_keys))
  for (i in seq_along(block$feature_keys)) {
    values <- as.numeric(block$assay[i, ])
    reference_values <- values[block$groups == reference]
    target_values <- values[block$groups == target]
    reference_values <- reference_values[is.finite(reference_values)]
    target_values <- target_values[is.finite(target_values)]
    p_value <- NULL
    if (length(reference_values) > 0L && length(target_values) > 0L) {
      tested <- suppressWarnings(if (isTRUE(positive_target)) {
        stats::wilcox.test(target_values, reference_values, exact = FALSE, correct = TRUE,
                           alternative = "two.sided")
      } else {
        stats::wilcox.test(reference_values, target_values, exact = FALSE, correct = TRUE,
                           alternative = "two.sided")
      })
      p_value <- unname(as.numeric(tested$p.value))
    }
    features[[i]] <- list(
      feature_key = block$feature_keys[[i]],
      p = p_value,
      n_reference = as.integer(length(reference_values)),
      n_target = as.integer(length(target_values))
    )
  }
  list(
    status = "complete",
    method = "wilcoxon_rank_sum",
    exact = FALSE,
    continuity_correction = TRUE,
    substitutes_for_primary = FALSE,
    equivalent_to_moderated_test = FALSE,
    features = features
  )
}

triomics_b_result_row <- function(feature_key, scale, support, contrast_index, estimated, reason,
                                  moderated_se, statistic, effective_df, df_prior, p_value, q_value,
                                  effect_unit, selection_validity, sample_ids, subjects, source_hash,
                                  model_hash, contrast_hash) {
  estimable <- isTRUE(support$estimable[[contrast_index]])
  effect <- if (estimable && (estimated || identical(reason, "zero_residual_support"))) {
    as.numeric(support$effects[[contrast_index]])
  } else {
    NULL
  }
  c(
    list(
      feature_key = feature_key,
      effect = effect,
      effect_unit = effect_unit,
      ordinary_se = if (estimated) as.numeric(support$ordinary_se[[contrast_index]]) else NULL,
      moderated_se = moderated_se,
      statistic = statistic,
      statistic_name = if (estimated) "moderated_t" else NULL,
      ordinary_df = if (estimable) support$df else NULL,
      effective_test_df = effective_df,
      df_prior = df_prior,
      p = p_value,
      q = q_value,
      inferential_status = triomics_b_inferential_status(estimated, selection_validity),
      estimation_status = if (estimated) "estimated" else "failed",
      estimation_reason = reason,
      n_samples = as.integer(length(sample_ids)),
      n_subjects = triomics_b_n_subjects(subjects, sample_ids, support$used),
      n_rows_used = as.integer(support$n_used),
      count_prior_ref = NULL,
      source_hash = source_hash,
      model_hash = model_hash,
      representation_hash = triomics_b_hash(list(
        representation = "levels",
        scale = scale,
        feature_key = feature_key
      )),
      contrast_hash = contrast_hash
    ),
    triomics_b_direction_fields()
  )
}

triomics_fit_block_models <- function(resolved_analysis, statistics_spec) {
  # DEqMS is a later packet. Explicit fallback is recorded and not applied here.
  if (identical(statistics_spec$method, "deqms")) {
    version <- if (requireNamespace("limma", quietly = TRUE)) as.character(packageVersion("limma")) else NULL
    return(list(
      status = "unavailable",
      fallback_applied = FALSE,
      blocks = list(),
      effects = NULL,
      limma_version = version
    ))
  }
  if (!identical(statistics_spec$method, "limma")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "This lane fits limma or reports DEqMS as unavailable.",
                     key = "statistics.method")
  }
  if (!identical(statistics_spec$trend, FALSE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma trend stays false in this lane.",
                     key = "statistics.trend")
  }
  if (!is.logical(statistics_spec$robust) || length(statistics_spec$robust) != 1L ||
      is.na(statistics_spec$robust)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma robust must be true or false.", key = "statistics.robust")
  }
  if (!is.logical(statistics_spec$rank_sensitivity) || length(statistics_spec$rank_sensitivity) != 1L ||
      is.na(statistics_spec$rank_sensitivity)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Rank sensitivity must be true or false.",
                     key = "statistics.rank_sensitivity")
  }
  if (!statistics_spec$selection_validity %in% c("qualified", "descriptive_only", "unverified")) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Selection validity is not a known family status.",
                     key = "statistics.selection_validity")
  }
  if (!requireNamespace("limma", quietly = TRUE)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "limma is not installed.", key = "statistics.method",
                     remediation = "Install limma from Bioconductor before fitting.")
  }
  scales <- unlist(lapply(resolved_analysis$blocks, function(block) block$scales), use.names = FALSE)
  effect_unit <- triomics_b_check_effect_unit(scales, statistics_spec$effect_unit)
  plan <- resolved_analysis$model_plan
  column_ids <- plan$column_ids
  contrasts <- statistics_spec$contrasts
  if (length(contrasts) == 0L) {
    triomics_b_abort("B_DESIGN_CONTRAST", "At least one named contrast is required.",
                     key = "statistics.contrasts")
  }
  contrast_ids <- vapply(contrasts, function(contrast) {
    if (!triomics_b_string(contrast$contrast_id)) {
      triomics_b_abort("B_DESIGN_CONTRAST", "Each contrast needs a contrast id.", key = "statistics.contrasts")
    }
    contrast$contrast_id
  }, character(1))
  if (anyDuplicated(contrast_ids)) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Contrast ids must be unique.", key = "statistics.contrasts")
  }
  weight_list <- lapply(contrasts, triomics_b_contrast_weights, column_ids = column_ids)
  contrast_matrix <- matrix(0, length(column_ids), length(contrast_ids),
                            dimnames = list(column_ids, contrast_ids))
  for (j in seq_along(weight_list)) contrast_matrix[, j] <- weight_list[[j]]
  publish_q <- identical(statistics_spec$selection_validity, "qualified")
  blocks <- vector("list", length(resolved_analysis$blocks))
  names(blocks) <- names(resolved_analysis$blocks)
  all_estimated <- TRUE
  for (block in resolved_analysis$blocks) {
    block_design <- triomics_b_indicator(block$groups, plan$levels, column_ids)
    n_features <- length(block$feature_keys)
    supports <- lapply(seq_len(n_features), function(i) {
      triomics_b_feature_support(as.numeric(block$assay[i, ]), block_design, weight_list)
    })
    eligible <- vapply(supports, function(support) isTRUE(support$eligible), logical(1))
    moderated <- NULL
    n_eligible <- sum(eligible)
    if (n_eligible > 0L) {
      # Failed and zero-df features stay out of eBayes so they cannot borrow a p.
      moderated <- suppressWarnings(limma::eBayes(
        limma::contrasts.fit(
          limma::lmFit(block$assay[eligible, , drop = FALSE], block_design),
          contrast_matrix
        ),
        trend = FALSE,
        robust = statistics_spec$robust
      ))
      local <- 0L
      for (feature in which(eligible)) {
        local <- local + 1L
        sigma <- triomics_b_gene_value(moderated$sigma, local)
        for (j in seq_along(weight_list)) {
          unscaled <- triomics_b_at(moderated$stdev.unscaled, local, j, n_eligible, length(weight_list))
          limma_effect <- triomics_b_at(moderated$coefficients, local, j, n_eligible, length(weight_list))
          if (!triomics_b_close(supports[[feature]]$ordinary_se[[j]], sigma * unscaled) ||
              !triomics_b_close(supports[[feature]]$effects[[j]], limma_effect)) {
            triomics_b_abort("B_FIT_DIVERGENCE",
                             "Ordinary contrast results diverged from limma.",
                             key = "statistics.contrasts")
          }
        }
      }
    }
    local_of <- rep(NA_integer_, n_features)
    if (n_eligible > 0L) local_of[which(eligible)] <- seq_len(n_eligible)
    model_hash <- triomics_b_hash(list(
      formula = plan$formula,
      column_ids = column_ids,
      samples = lapply(order(block$sample_ids), function(index) {
        list(sample_id = block$sample_ids[[index]], condition = block$groups[[index]])
      })
    ))
    contrast_results <- vector("list", length(contrast_ids))
    names(contrast_results) <- contrast_ids
    for (j in seq_along(contrast_ids)) {
      weights <- weight_list[[j]]
      contrast_hash <- triomics_b_hash(list(contrast_id = contrast_ids[[j]], weights = as.list(weights)))
      p_values <- rep(NA_real_, n_features)
      estimated <- logical(n_features)
      rows <- vector("list", n_features)
      moderated_values <- vector("list", n_features)
      for (i in seq_len(n_features)) {
        support <- supports[[i]]
        local <- local_of[[i]]
        if (!is.na(local)) {
          estimated[[i]] <- TRUE
          unscaled <- triomics_b_at(moderated$stdev.unscaled, local, j, n_eligible, length(weight_list))
          s2_post <- triomics_b_gene_value(moderated$s2.post, local)
          p_values[[i]] <- triomics_b_at(moderated$p.value, local, j, n_eligible, length(weight_list))
          moderated_values[[i]] <- list(
            moderated_se = sqrt(s2_post) * unscaled,
            statistic = triomics_b_at(moderated$t, local, j, n_eligible, length(weight_list)),
            effective_df = triomics_b_gene_value(moderated$df.total, local),
            df_prior = triomics_b_gene_value(moderated$df.prior, local),
            p = p_values[[i]]
          )
        } else if (!isTRUE(support$estimable[[j]])) {
          moderated_values[[i]] <- list(reason = "contrast_not_estimable")
        } else if (support$df < 1) {
          moderated_values[[i]] <- list(reason = "zero_residual_support")
        } else {
          moderated_values[[i]] <- list(reason = "contrast_not_estimable")
        }
      }
      if (!all(estimated)) all_estimated <- FALSE
      family <- triomics_b_family(
        resolved_analysis$study_id, resolved_analysis$analysis_id, block$block_id,
        contrast_ids[[j]], block$feature_keys, estimated, p_values, publish_q
      )
      for (i in seq_len(n_features)) {
        drawn <- moderated_values[[i]]
        q_value <- if (estimated[[i]] && publish_q) family$internal_q[[i]] else NULL
        rows[[i]] <- triomics_b_result_row(
          feature_key = block$feature_keys[[i]],
          scale = block$scales[[i]],
          support = supports[[i]],
          contrast_index = j,
          estimated = estimated[[i]],
          reason = if (estimated[[i]]) NULL else drawn$reason,
          moderated_se = if (estimated[[i]]) drawn$moderated_se else NULL,
          statistic = if (estimated[[i]]) drawn$statistic else NULL,
          effective_df = if (estimated[[i]]) drawn$effective_df else NULL,
          df_prior = if (estimated[[i]]) drawn$df_prior else NULL,
          p_value = if (estimated[[i]]) drawn$p else NULL,
          q_value = q_value,
          effect_unit = effect_unit,
          selection_validity = statistics_spec$selection_validity,
          sample_ids = block$sample_ids,
          subjects = resolved_analysis$subjects,
          source_hash = block$source_hash,
          model_hash = model_hash,
          contrast_hash = contrast_hash
        )
      }
      sensitivity <- if (isTRUE(statistics_spec$rank_sensitivity)) {
        triomics_b_wilcoxon(block, plan, weights)
      } else {
        list(status = "not_requested")
      }
      contrast_results[[contrast_ids[[j]]]] <- list(
        contrast_id = contrast_ids[[j]],
        rows = rows,
        family = family,
        count_prior_status = "not_used_no_automatic_deqms",
        sensitivity = sensitivity
      )
    }
    blocks[[block$block_id]] <- list(block_id = block$block_id, contrasts = contrast_results)
  }
  list(
    status = if (all_estimated) "complete" else "partial",
    limma_version = as.character(packageVersion("limma")),
    blocks = blocks,
    effects = NULL
  )
}

fit_block_models <- triomics_fit_block_models
