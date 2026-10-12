# Limma's generalized inverse can return a coefficient for a contrast that is not
# estimable when a covariate aliases a column. Estimability is decided first.

triomics_b02_weight_map <- function(contrast, column_ids) {
  weights <- contrast$weights
  if (is.language(weights) || is.character(weights) || is.factor(weights)) {
    triomics_b_abort("B_DESIGN_CONTRAST",
                     "Contrast weights must be finite numbers, not expressions.",
                     key = "statistics.contrasts")
  }
  if (is.list(weights)) {
    for (value in weights) {
      if (is.language(value) || is.character(value) || is.factor(value) ||
          length(value) != 1L || !is.numeric(value) || !is.finite(value)) {
        triomics_b_abort("B_DESIGN_CONTRAST",
                         "Contrast weights must be finite numbers, not expressions.",
                         key = "statistics.contrasts")
      }
    }
  }
  triomics_b_contrast_weights(contrast, column_ids)
}

triomics_b02_estimable_weights <- function(design, weights) {
  if (nrow(design) == 0L || ncol(design) == 0L || length(weights) != ncol(design)) return(FALSE)
  fitted <- tryCatch(qr.fitted(qr(t(design)), weights), error = function(error) NULL)
  if (is.null(fitted)) return(FALSE)
  all(abs(as.numeric(fitted) - as.numeric(weights)) <= 1e-8)
}

triomics_b02_quadratic <- function(design, weights) {
  decomposition <- tryCatch(svd(design), error = function(error) NULL)
  if (is.null(decomposition) || length(decomposition$d) == 0L) return(NA_real_)
  singular <- decomposition$d
  tolerance <- max(dim(design)) * max(singular) * .Machine$double.eps
  keep <- singular > tolerance
  if (!any(keep)) return(NA_real_)
  rotated <- crossprod(decomposition$v[, keep, drop = FALSE], weights)
  sum(as.numeric(rotated)^2 / singular[keep]^2)
}

triomics_b02_feature_support <- function(y, design, weight_list) {
  used <- is.finite(y)
  y_used <- as.numeric(y[used])
  x_used <- design[used, , drop = FALSE]
  n_used <- length(y_used)
  rank <- if (n_used == 0L || ncol(x_used) == 0L) 0L else qr(x_used)$rank
  df <- n_used - rank
  n_contrast <- length(weight_list)
  estimable <- logical(n_contrast)
  effects <- rep(NA_real_, n_contrast)
  ordinary_se <- rep(NA_real_, n_contrast)
  if (n_used > 0L && ncol(x_used) > 0L) {
    coefficients <- qr.coef(qr(x_used), y_used)
    residual <- y_used - as.numeric(qr.fitted(qr(x_used), y_used))
    sigma2 <- if (df >= 1) sum(residual^2) / df else NA_real_
    for (j in seq_len(n_contrast)) {
      weights <- weight_list[[j]]
      if (!triomics_b02_estimable_weights(x_used, weights)) next
      if (any(abs(weights) > 1e-8 & !is.finite(coefficients))) next
      estimable[[j]] <- TRUE
      coef <- coefficients
      coef[!is.finite(coef)] <- 0
      effects[[j]] <- sum(weights * coef)
      if (df >= 1 && is.finite(sigma2)) {
        quad <- triomics_b02_quadratic(x_used, weights)
        if (is.finite(quad)) ordinary_se[[j]] <- sqrt(sigma2 * quad)
      }
    }
  }
  if (!is.null(names(weight_list))) {
    names(estimable) <- names(weight_list)
    names(effects) <- names(weight_list)
    names(ordinary_se) <- names(weight_list)
  }
  list(
    n_used = n_used, used = used, rank = rank, df = df,
    estimable = estimable, effects = effects, ordinary_se = ordinary_se
  )
}

triomics_b02_required_unit <- function(kind, scales) {
  if (identical(kind, "continuous")) return("per_unit_beta")
  required <- unique(vapply(scales, triomics_b_required_effect_unit, character(1)))
  if (length(required) != 1L || !nzchar(required[[1L]])) {
    triomics_b_abort("B_DESIGN_EFFECT_UNIT",
                     "The effect unit does not match the declared measurement scale.",
                     key = "statistics.effect_unit")
  }
  required[[1L]]
}

triomics_b02_direction <- function(kind, representation_id, claim_type) {
  reason <- if (identical(claim_type, "joint_contrast")) {
    "joint_contrast_has_no_signed_reference_direction"
  } else if (identical(kind, "continuous")) {
    "continuous_design_has_no_within_person_delta"
  } else if (identical(representation_id, "mean")) {
    "subject_mean_analysis_has_no_within_person_delta"
  } else {
    "independent_samples_have_no_within_person_delta"
  }
  list(
    direction_status = "not_applicable",
    direction_reason = reason,
    direction_numerator = NULL,
    direction_denominator = NULL,
    direction_zero_count = NULL,
    n_pairs = NULL
  )
}

triomics_b02_result_row <- function(feature_key, effect, effect_unit, ordinary_se, moderated_se,
                                    statistic, statistic_name, ordinary_df, effective_df, df_prior,
                                    p_value, q_value, estimated, reason, selection_validity,
                                    n_samples, n_subjects, n_rows_used, source_hash, model_hash,
                                    representation_hash, contrast_hash, representation_id,
                                    reference_direction_status, claim_type, kind) {
  c(
    list(
      feature_key = feature_key,
      effect = effect,
      effect_unit = effect_unit,
      ordinary_se = ordinary_se,
      moderated_se = moderated_se,
      statistic = statistic,
      statistic_name = statistic_name,
      ordinary_df = ordinary_df,
      effective_test_df = effective_df,
      df_prior = df_prior,
      p = p_value,
      q = q_value,
      inferential_status = triomics_b_inferential_status(estimated, selection_validity),
      estimation_status = if (estimated) "estimated" else "failed",
      estimation_reason = reason,
      n_samples = n_samples,
      n_subjects = n_subjects,
      n_rows_used = n_rows_used,
      count_prior_ref = NULL,
      source_hash = source_hash,
      model_hash = model_hash,
      representation_hash = representation_hash,
      contrast_hash = contrast_hash,
      representation_id = representation_id,
      reference_direction_status = reference_direction_status,
      claim_type = claim_type
    ),
    triomics_b02_direction(kind, representation_id, claim_type)
  )
}

triomics_b02_joint_family <- function(study_id, analysis_id, block_id, claim_id, member_ids,
                                      estimated, p_values, publish_q) {
  n <- length(member_ids)
  internal_p <- rep(1, n)
  if (any(estimated)) internal_p[estimated] <- as.numeric(p_values[estimated])
  list(
    family_id = triomics_b_hash(list(
      study_id = study_id,
      analysis_id = analysis_id,
      block_id = block_id,
      claim_id = claim_id,
      grain = "study_analysis_block_joint_contrast"
    )),
    grain = "study_analysis_block_joint_contrast",
    claim_scope = "joint_contrast",
    member_ids = as.character(member_ids),
    member_hash = triomics_b_hash(as.character(member_ids)),
    n_declared = as.integer(n),
    n_estimated = as.integer(sum(estimated)),
    n_failed = as.integer(n - sum(estimated)),
    n_eligible = as.integer(sum(estimated)),
    n_evaluated = as.integer(n),
    dispositions = c(estimated = as.integer(sum(estimated)), failed = as.integer(n - sum(estimated))),
    internal_p = unname(as.numeric(internal_p)),
    internal_q = unname(as.numeric(stats::p.adjust(internal_p, method = "BH"))),
    correction_method = "BH",
    q_published = isTRUE(publish_q)
  )
}

triomics_b02_check_statistics <- function(statistics_spec, kind, scales) {
  if (identical(statistics_spec$method, "deqms")) return(invisible(NULL))
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
  required <- triomics_b02_required_unit(kind, scales)
  if (!identical(statistics_spec$effect_unit, required)) {
    triomics_b_abort("B_DESIGN_EFFECT_UNIT",
                     "The effect unit does not match the declared measurement scale and coefficient.",
                     key = "statistics.effect_unit")
  }
  required
}

triomics_b02_claims <- function(statistics_spec, contrast_ids, effect_unit) {
  claims <- statistics_spec$claims
  if (triomics_b_empty(claims)) return(list())
  if (!is.list(claims)) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Joint claims must be declared records, not expressions.",
                     key = "statistics.claims")
  }
  ids <- character(length(claims))
  for (i in seq_along(claims)) {
    claim <- claims[[i]]
    if (!triomics_b_string(claim$claim_id) || !triomics_b_string(claim$claim_type)) {
      triomics_b_abort("B_DESIGN_CONTRAST", "Each joint claim needs a claim id and claim type.",
                       key = "statistics.claims")
    }
    if (!identical(claim$claim_type, "joint_contrast")) {
      triomics_b_abort("B_DESIGN_CONTRAST",
                       "A scalar family is a separate declaration from a joint-contrast claim.",
                       key = "statistics.claims")
    }
    members <- triomics_b_characters(claim$contrast_ids, "statistics.claims")
    if (length(members) < 2L || any(!members %in% contrast_ids)) {
      triomics_b_abort("B_DESIGN_CONTRAST",
                       "A joint claim names two or more declared scalar contrasts and does not replace them.",
                       key = "statistics.claims")
    }
    if (!identical(claim$effect_unit, effect_unit)) {
      triomics_b_abort("B_DESIGN_EFFECT_UNIT",
                       "The joint claim effect unit must match the scalar coefficient unit.",
                       key = "statistics.claims")
    }
    claims[[i]]$contrast_ids <- members
    ids[[i]] <- claim$claim_id
  }
  if (anyDuplicated(ids) || any(ids %in% contrast_ids)) {
    triomics_b_abort("B_DESIGN_CONTRAST", "Joint claim ids must be unique and distinct from scalar contrast ids.",
                     key = "statistics.claims")
  }
  names(claims) <- ids
  claims
}

triomics_b02_sensitivity <- function(rank_sensitivity) {
  if (!isTRUE(rank_sensitivity)) return(list(status = "not_requested"))
  list(
    status = "unavailable",
    method = NULL,
    reason = "rank_sensitivity_is_unavailable_for_multigroup_continuous_mean_or_adjusted_estimands",
    substitutes_for_primary = FALSE,
    equivalent_to_moderated_test = FALSE,
    features = NULL
  )
}

triomics_b02_limma_scalar <- function(assay, design, supports, weight_list, contrast_index, robust) {
  n_features <- nrow(assay)
  moderated <- vector("list", n_features)
  signatures <- vapply(supports, function(support) paste(which(support$used), collapse = ","), character(1))
  groups <- split(seq_len(n_features), signatures)
  for (indexes in groups) {
    indexes <- as.integer(indexes)
    pattern <- supports[[indexes[[1L]]]]$estimable
    if (!isTRUE(pattern[[contrast_index]]) || supports[[indexes[[1L]]]]$df < 1) next
    used <- supports[[indexes[[1L]]]]$used
    sub_y <- assay[indexes, used, drop = FALSE]
    sub_x <- design[used, , drop = FALSE]
    contrast <- matrix(weight_list[[contrast_index]], ncol = 1L,
                       dimnames = list(colnames(sub_x), "contrast"))
    eb <- tryCatch(
      suppressMessages(suppressWarnings(limma::eBayes(
        limma::contrasts.fit(limma::lmFit(sub_y, sub_x), contrast),
        trend = FALSE, robust = robust
      ))),
      error = function(error) NULL
    )
    if (is.null(eb)) {
      triomics_b_abort("B_FIT_DIVERGENCE", "Ordinary contrast results diverged from limma.",
                       key = "statistics.contrasts")
    }
    for (local in seq_along(indexes)) {
      feature <- indexes[[local]]
      limma_effect <- triomics_b_at(eb$coefficients, local, 1L, length(indexes), 1L)
      unscaled <- triomics_b_at(eb$stdev.unscaled, local, 1L, length(indexes), 1L)
      sigma <- triomics_b_gene_value(eb$sigma, local)
      if (!triomics_b_close(supports[[feature]]$effects[[contrast_index]], limma_effect) ||
          !triomics_b_close(supports[[feature]]$ordinary_se[[contrast_index]], sigma * unscaled)) {
        triomics_b_abort("B_FIT_DIVERGENCE", "Ordinary contrast results diverged from limma.",
                         key = "statistics.contrasts")
      }
      s2_post <- triomics_b_gene_value(eb$s2.post, local)
      moderated[[feature]] <- list(
        moderated_se = sqrt(s2_post) * unscaled,
        statistic = triomics_b_at(eb$t, local, 1L, length(indexes), 1L),
        effective_df = triomics_b_gene_value(eb$df.total, local),
        df_prior = triomics_b_gene_value(eb$df.prior, local),
        p = triomics_b_at(eb$p.value, local, 1L, length(indexes), 1L)
      )
    }
  }
  moderated
}

triomics_b02_limma_joint <- function(assay, design, supports, weight_list, members, robust) {
  n_features <- nrow(assay)
  out <- vector("list", n_features)
  signatures <- vapply(supports, function(support) paste(which(support$used), collapse = ","), character(1))
  groups <- split(seq_len(n_features), signatures)
  for (indexes in groups) {
    indexes <- as.integer(indexes)
    support <- supports[[indexes[[1L]]]]
    if (support$df < 1 || !all(vapply(members, function(id) isTRUE(support$estimable[[id]]), logical(1)))) next
    used <- support$used
    sub_y <- assay[indexes, used, drop = FALSE]
    sub_x <- design[used, , drop = FALSE]
    contrast <- do.call(cbind, lapply(members, function(id) weight_list[[id]]))
    colnames(contrast) <- members
    rownames(contrast) <- colnames(sub_x)
    fitted <- tryCatch({
      eb <- suppressMessages(suppressWarnings(limma::eBayes(
        limma::contrasts.fit(limma::lmFit(sub_y, sub_x), contrast),
        trend = FALSE, robust = robust
      )))
      table <- suppressMessages(suppressWarnings(limma::topTable(
        eb, coef = seq_along(members), number = Inf, sort.by = "none"
      )))
      list(eb = eb, table = table)
    }, error = function(error) NULL)
    if (is.null(fitted) || !all(c("F", "P.Value") %in% names(fitted$table))) {
      triomics_b_abort("B_FIT_DIVERGENCE", "The joint contrast statistic diverged from limma.",
                       key = "statistics.claims")
    }
    table <- fitted$table
    eb <- fitted$eb
    if (!is.null(rownames(table)) && all(rownames(sub_y) %in% rownames(table))) {
      table <- table[rownames(sub_y), , drop = FALSE]
    }
    for (local in seq_along(indexes)) {
      out[[indexes[[local]]]] <- list(
        statistic = as.numeric(table$F[[local]]),
        p = as.numeric(table$P.Value[[local]]),
        effective_df = triomics_b_gene_value(eb$df.total, local),
        df_prior = triomics_b_gene_value(eb$df.prior, local)
      )
    }
  }
  out
}

triomics_b02_fit <- function(resolved_analysis, statistics_spec) {
  if (identical(statistics_spec$method, "deqms")) {
    version <- if (requireNamespace("limma", quietly = TRUE)) as.character(packageVersion("limma")) else NULL
    return(list(status = "unavailable", fallback_applied = FALSE, blocks = list(),
                effects = NULL, limma_version = version))
  }
  plan <- resolved_analysis$model_plan
  scales <- unlist(lapply(resolved_analysis$blocks, function(block) block$scales), use.names = FALSE)
  effect_unit <- triomics_b02_check_statistics(statistics_spec, plan$coefficient_kind, scales)
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
  weight_list <- lapply(contrasts, triomics_b02_weight_map, column_ids = column_ids)
  names(weight_list) <- contrast_ids
  claims <- triomics_b02_claims(statistics_spec, contrast_ids, effect_unit)
  publish_q <- identical(statistics_spec$selection_validity, "qualified")
  sensitivity <- triomics_b02_sensitivity(statistics_spec$rank_sensitivity)
  blocks <- vector("list", length(resolved_analysis$blocks))
  names(blocks) <- names(resolved_analysis$blocks)
  all_estimated <- TRUE
  for (block in resolved_analysis$blocks) {
    design <- block$design
    if (is.null(design) || !identical(colnames(design), column_ids) || !identical(rownames(design), colnames(block$assay))) {
      triomics_b_abort("B_DESIGN_CONTRAST", "The design columns do not match the declared model-column ids.",
                       key = "statistics.contrasts")
    }
    n_features <- length(block$feature_keys)
    supports <- lapply(seq_len(n_features), function(i) {
      triomics_b02_feature_support(as.numeric(block$assay[i, ]), design, weight_list)
    })
    model_hash <- triomics_b_hash(list(
      formula = plan$formula,
      column_ids = column_ids,
      representation = resolved_analysis$representation_id,
      coefficient_kind = plan$coefficient_kind,
      units = lapply(sort(rownames(design)), function(unit_id) {
        list(unit_id = unit_id, columns = as.list(stats::setNames(as.numeric(design[unit_id, ]), column_ids)))
      })
    ))
    contrast_results <- vector("list", length(contrast_ids))
    names(contrast_results) <- contrast_ids
    for (j in seq_along(contrast_ids)) {
      weights <- weight_list[[contrast_ids[[j]]]]
      moderated <- triomics_b02_limma_scalar(
        block$assay, design, supports, weight_list, j, statistics_spec$robust
      )
      estimated <- vapply(seq_len(n_features), function(i) !is.null(moderated[[i]]), logical(1))
      p_values <- rep(NA_real_, n_features)
      p_values[estimated] <- vapply(which(estimated), function(i) moderated[[i]]$p, numeric(1))
      if (!all(estimated)) all_estimated <- FALSE
      family <- triomics_b_family(
        resolved_analysis$study_id, resolved_analysis$analysis_id, block$block_id,
        contrast_ids[[j]], block$feature_keys, estimated, p_values, publish_q
      )
      family$grain <- "study_analysis_block_scalar_contrast"
      rows <- vector("list", n_features)
      contrast_hash <- triomics_b_hash(list(contrast_id = contrast_ids[[j]], weights = as.list(weights)))
      reference_direction <- list(
        status = "declared_weight_direction",
        reference_level = plan$reference_level,
        target_level = plan$target_level,
        weights = as.list(weights),
        positive_means = "higher_weight_minus_lower_weight_on_named_model_columns"
      )
      for (i in seq_len(n_features)) {
        support <- supports[[i]]
        drawn <- moderated[[i]]
        estimable <- isTRUE(support$estimable[[j]])
        reason <- if (estimated[[i]]) {
          NULL
        } else if (!estimable) {
          "contrast_not_estimable"
        } else if (support$df < 1) {
          "zero_residual_support"
        } else {
          "contrast_not_estimable"
        }
        effect <- if (estimable && (estimated[[i]] || identical(reason, "zero_residual_support"))) {
          as.numeric(support$effects[[j]])
        } else {
          NULL
        }
        rows[[i]] <- triomics_b02_result_row(
          feature_key = block$feature_keys[[i]],
          effect = effect,
          effect_unit = effect_unit,
          ordinary_se = if (estimated[[i]]) as.numeric(support$ordinary_se[[j]]) else NULL,
          moderated_se = if (estimated[[i]]) drawn$moderated_se else NULL,
          statistic = if (estimated[[i]]) drawn$statistic else NULL,
          statistic_name = if (estimated[[i]]) "moderated_t" else NULL,
          ordinary_df = if (estimable) support$df else NULL,
          effective_df = if (estimated[[i]]) drawn$effective_df else NULL,
          df_prior = if (estimated[[i]]) drawn$df_prior else NULL,
          p_value = if (estimated[[i]]) drawn$p else NULL,
          q_value = if (estimated[[i]] && publish_q) family$internal_q[[i]] else NULL,
          estimated = estimated[[i]],
          reason = reason,
          selection_validity = statistics_spec$selection_validity,
          n_samples = as.integer(block$n_source_samples),
          n_subjects = triomics_b_n_subjects(block$subjects, block$sample_ids, support$used),
          n_rows_used = as.integer(support$n_used),
          source_hash = block$source_hash,
          model_hash = model_hash,
          representation_hash = triomics_b_hash(list(
            representation = resolved_analysis$representation_id,
            scale = block$scales[[i]],
            feature_key = block$feature_keys[[i]],
            mean_weights = plan$mean_weights
          )),
          contrast_hash = contrast_hash,
          representation_id = resolved_analysis$representation_id,
          reference_direction_status = "declared_weight_direction",
          claim_type = "scalar_contrast",
          kind = plan$coefficient_kind
        )
      }
      contrast_results[[contrast_ids[[j]]]] <- list(
        contrast_id = contrast_ids[[j]],
        claim_scope = "scalar_contrast",
        claim_type = "scalar_contrast",
        rows = rows,
        family = family,
        count_prior_status = "not_used_no_automatic_deqms",
        sensitivity = sensitivity,
        effect_unit = effect_unit,
        reference_direction = reference_direction
      )
    }
    joint_claims <- list()
    for (claim in claims) {
      members <- claim$contrast_ids
      joint_fit <- triomics_b02_limma_joint(
        block$assay, design, supports, weight_list, members, statistics_spec$robust
      )
      estimated <- vapply(joint_fit, function(item) !is.null(item), logical(1))
      p_values <- rep(NA_real_, n_features)
      if (any(estimated)) p_values[estimated] <- vapply(which(estimated), function(i) joint_fit[[i]]$p, numeric(1))
      if (!all(estimated)) all_estimated <- FALSE
      family <- triomics_b02_joint_family(
        resolved_analysis$study_id, resolved_analysis$analysis_id, block$block_id,
        claim$claim_id, block$feature_keys, estimated, p_values, publish_q
      )
      contrast_matrix <- do.call(cbind, lapply(members, function(id) weight_list[[id]]))
      colnames(contrast_matrix) <- members
      rows <- vector("list", n_features)
      for (i in seq_len(n_features)) {
        drawn <- joint_fit[[i]]
        rows[[i]] <- triomics_b02_result_row(
          feature_key = block$feature_keys[[i]],
          effect = NULL,
          effect_unit = effect_unit,
          ordinary_se = NULL,
          moderated_se = NULL,
          statistic = if (estimated[[i]]) drawn$statistic else NULL,
          statistic_name = if (estimated[[i]]) "moderated_F" else NULL,
          ordinary_df = if (estimated[[i]]) supports[[i]]$df else NULL,
          effective_df = if (estimated[[i]]) drawn$effective_df else NULL,
          df_prior = if (estimated[[i]]) drawn$df_prior else NULL,
          p_value = if (estimated[[i]]) drawn$p else NULL,
          q_value = if (estimated[[i]] && publish_q) family$internal_q[[i]] else NULL,
          estimated = estimated[[i]],
          reason = if (estimated[[i]]) NULL else "contrast_not_estimable",
          selection_validity = statistics_spec$selection_validity,
          n_samples = as.integer(block$n_source_samples),
          n_subjects = triomics_b_n_subjects(block$subjects, block$sample_ids, supports[[i]]$used),
          n_rows_used = as.integer(supports[[i]]$n_used),
          source_hash = block$source_hash,
          model_hash = model_hash,
          representation_hash = triomics_b_hash(list(
            representation = resolved_analysis$representation_id,
            scale = block$scales[[i]],
            feature_key = block$feature_keys[[i]],
            claim_id = claim$claim_id
          )),
          contrast_hash = triomics_b_hash(list(claim_id = claim$claim_id, contrast_ids = as.list(members))),
          representation_id = resolved_analysis$representation_id,
          reference_direction_status = "not_a_scalar_direction",
          claim_type = "joint_contrast",
          kind = plan$coefficient_kind
        )
      }
      joint_claims[[claim$claim_id]] <- list(
        claim_id = claim$claim_id,
        claim_scope = "joint_contrast",
        claim_type = "joint_contrast",
        contrast_ids = members,
        numerator_df = as.integer(qr(contrast_matrix)$rank),
        rows = rows,
        family = family,
        effect_unit = effect_unit,
        reference_direction = list(
          status = "not_a_scalar_direction",
          reason = "joint_contrast_has_no_single_reference_direction",
          contrast_ids = members,
          effect_unit = effect_unit,
          weights = lapply(members, function(id) as.list(weight_list[[id]]))
        )
      )
    }
    block_result <- list(
      block_id = block$block_id,
      representation_id = resolved_analysis$representation_id,
      contrasts = contrast_results
    )
    if (length(joint_claims) > 0L) block_result$joint_claims <- joint_claims
    blocks[[block$block_id]] <- block_result
  }
  list(
    status = if (all_estimated) "complete" else "partial",
    limma_version = as.character(packageVersion("limma")),
    blocks = blocks,
    effects = NULL
  )
}
