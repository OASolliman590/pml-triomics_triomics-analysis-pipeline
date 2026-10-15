triomics_b06_nominate <- function(rows, family, statistics_spec, analysis_id, block_id, claim_id) {
  if (!isTRUE(family$q_published) || !isTRUE(family$validation$eligible)) return(list())
  q_threshold <- statistics_spec$q_threshold
  effect_threshold <- statistics_spec$effect_threshold
  if (!is.numeric(q_threshold) || length(q_threshold) != 1L || is.na(q_threshold) ||
      !is.finite(q_threshold) || q_threshold < 0 || q_threshold > 1 ||
      !is.numeric(effect_threshold) || length(effect_threshold) != 1L ||
      is.na(effect_threshold) || !is.finite(effect_threshold) || effect_threshold < 0) {
    return(list())
  }
  keep <- vapply(rows, function(row) {
    q <- row$q
    effect <- row$effect
    is.numeric(q) && length(q) == 1L && !is.na(q) && is.finite(q) && q <= q_threshold &&
      is.numeric(effect) && length(effect) == 1L && !is.na(effect) && is.finite(effect) &&
      abs(effect) >= effect_threshold
  }, logical(1))
  indexes <- which(keep)
  lapply(indexes, function(index) {
    row <- rows[[index]]
    nomination_context <- list(
      family_id = family$family_id,
      analysis_id = analysis_id,
      block_id = block_id,
      claim_id = claim_id,
      feature_key = row$feature_key,
      q_threshold = as.numeric(q_threshold),
      effect_threshold = as.numeric(effect_threshold),
      effect_unit = row$effect_unit
    )
    list(
      nomination_id = triomics_b_hash(nomination_context),
      feature_key = row$feature_key,
      source_branch = "primary_statistics",
      result_ref = list(
        analysis_id = analysis_id,
        block_id = block_id,
        claim_id = claim_id,
        family_id = family$family_id
      ),
      selection_rule = "valid_q_at_or_below_threshold_and_absolute_effect_at_or_above_reporting_threshold",
      selection_value = list(
        p = row$p,
        q = row$q,
        effect = row$effect,
        effect_unit = row$effect_unit
      ),
      q_threshold = as.numeric(q_threshold),
      effect_threshold = as.numeric(effect_threshold),
      filter_role = "reporting_only"
    )
  })
}
