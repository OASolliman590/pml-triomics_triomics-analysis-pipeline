# Linear lipid endpoints. Logs are never summed. Sum composition does not
# assign sn positions or individual chains.

triomics_lipid_abort <- function(code, message, key = "derive") {
  triomics_abort(
    code, message, key = key, module = "lipid_derivation", stage = "A",
    remediation = "Keep a fixed linear parent universe and an explicit coverage policy. Do not sum logs, invent a pseudocount, or label intensity as a molar concentration."
  )
}

triomics_lipid_json_number <- function(value) {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) return(NULL)
  number <- suppressWarnings(as.numeric(value[[1L]]))
  if (!is.finite(number)) return(NULL)
  unname(number)
}

triomics_lipid_text <- function(value, default = NULL) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) return(default)
  text <- as.character(value[[1L]])
  if (!nzchar(text)) return(default)
  text
}

triomics_lipid_optional_number <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) return(NA_real_)
  number <- suppressWarnings(as.numeric(value[[1L]]))
  if (length(number) != 1L) return(NA_real_)
  number
}

triomics_lipid_require_text <- function(value, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      paste0("Lipid derivation requires one non-blank ", field, "."),
      field
    )
  }
  value
}

triomics_lipid_count <- function(value) unname(as.numeric(value))

triomics_lipid_rows <- function(parents) {
  if (is.data.frame(parents)) {
    if (!nrow(parents)) return(list())
    return(lapply(seq_len(nrow(parents)), function(i) as.list(parents[i, , drop = FALSE])))
  }
  if (is.list(parents) && length(parents) && all(vapply(parents, is.list, logical(1L)))) return(parents)
  triomics_lipid_abort(
    "A_CONFIG_OPERATION",
    "Lipid derivation requires a fixed parent universe.",
    "parents"
  )
}

triomics_lipid_normalize_parents <- function(parents) {
  rows <- triomics_lipid_rows(parents)
  if (!length(rows)) {
    triomics_lipid_abort("A_CONFIG_OPERATION", "The lipid parent universe is empty.", "parents")
  }
  n <- length(rows)
  frame <- data.frame(
    feature_key = character(n),
    class_id = character(n),
    carbons = rep(NA_real_, n),
    double_bonds = rep(NA_real_, n),
    structural_resolution = character(n),
    quantity = character(n),
    units = character(n),
    scale = character(n),
    calibration_status = character(n),
    compatible = logical(n),
    stringsAsFactors = FALSE
  )
  allowed_resolution <- c("sum_composition", "molecular_species", "sn_resolved", "unresolved")
  for (i in seq_len(n)) {
    row <- rows[[i]]
    frame$feature_key[[i]] <- triomics_lipid_text(row$feature_key, "")
    frame$class_id[[i]] <- triomics_lipid_text(row$class_id, "declared")
    frame$carbons[[i]] <- triomics_lipid_optional_number(row$carbons)
    frame$double_bonds[[i]] <- triomics_lipid_optional_number(row$double_bonds)
    resolution <- triomics_lipid_text(row$structural_resolution, "unresolved")
    if (!resolution %in% allowed_resolution) resolution <- "unresolved"
    frame$structural_resolution[[i]] <- resolution
    frame$quantity[[i]] <- triomics_lipid_text(row$quantity, "")
    frame$units[[i]] <- triomics_lipid_text(row$units, "")
    frame$scale[[i]] <- triomics_lipid_text(row$scale, "linear")
    frame$calibration_status[[i]] <- triomics_lipid_text(row$calibration_status, "unresolved")
    frame$compatible[[i]] <- if (is.null(row$compatible)) TRUE else isTRUE(as.logical(row$compatible[[1L]]))
  }
  if (any(!nzchar(frame$feature_key)) || anyDuplicated(frame$feature_key)) {
    triomics_lipid_abort(
      "A_ID_DUPLICATE_FEATURE",
      "Each lipid parent needs a unique feature key. Duplicate adducts stay separate rows.",
      "feature_key"
    )
  }
  if (any(!nzchar(frame$quantity)) || any(!nzchar(frame$units))) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      "Each lipid parent needs an explicit quantity and units.",
      "quantity"
    )
  }
  frame
}

triomics_lipid_structural_claim <- function(resolutions) {
  resolutions <- as.character(resolutions)
  if (any(resolutions == "sum_composition")) {
    resolution <- "sum_composition"
    claim <- "total_carbons_unsaturation_only"
  } else if (any(resolutions == "unresolved") || any(!resolutions %in% c("sn_resolved", "molecular_species"))) {
    resolution <- "unresolved"
    claim <- "total_carbons_unsaturation_only"
  } else if (all(resolutions == "sn_resolved")) {
    resolution <- "sn_resolved"
    claim <- "aggregate_does_not_assign_sn"
  } else {
    resolution <- "molecular_species"
    claim <- "aggregate_does_not_assign_chains"
  }
  list(
    structural_resolution = resolution,
    structural_claim = claim,
    sn_positions = "not_implied",
    chain_identities = "not_implied"
  )
}

triomics_lipid_universe_hash <- function(frame, request) {
  payload <- list(
    parent_set_ref = request$parent_set_ref,
    definition_ref = request$definition_ref,
    method = request$method,
    quantity_rule = request$quantity_rule,
    missing_parent_policy = request$missing_parent_policy,
    parents = lapply(seq_len(nrow(frame)), function(i) {
      list(
        feature_key = frame$feature_key[[i]],
        class_id = frame$class_id[[i]],
        carbons = triomics_lipid_json_number(frame$carbons[[i]]),
        double_bonds = triomics_lipid_json_number(frame$double_bonds[[i]]),
        structural_resolution = frame$structural_resolution[[i]],
        quantity = frame$quantity[[i]],
        units = frame$units[[i]],
        scale = frame$scale[[i]],
        calibration_status = frame$calibration_status[[i]],
        compatible = isTRUE(frame$compatible[[i]])
      )
    })
  )
  triomics_hash_text(triomics_canonical_json(payload))
}

triomics_lipid_rule <- function(method) {
  switch(method,
    class_sum = "linear_sum",
    weighted_carbons = "abundance_weighted_mean",
    weighted_double_bonds = "abundance_weighted_mean",
    saturated_unsaturated_ratio = "saturated_over_unsaturated",
    within_class_fraction = "within_class_fraction",
    molar_fraction = "molar_fraction",
    NULL
  )
}

triomics_lipid_check_request <- function(request, block_scale) {
  method <- triomics_lipid_require_text(request$method, "method")
  rule <- triomics_lipid_require_text(request$quantity_rule, "quantity_rule")
  policy <- triomics_lipid_require_text(request$missing_parent_policy, "missing_parent_policy")
  triomics_lipid_require_text(request$endpoint_id, "endpoint_id")
  triomics_lipid_require_text(request$parent_set_ref, "parent_set_ref")
  triomics_lipid_require_text(request$definition_ref, "definition_ref")
  if (isTRUE(request$erase_class_total) || isTRUE(request$replace_parents)) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      "Within-class closure cannot erase the class total. Retain the linear total beside any fraction.",
      "erase_class_total"
    )
  }
  if (!identical(block_scale, "linear") || identical(rule, "sum_of_logs")) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "Class totals and weighted lipid endpoints are derived in linear space and cannot sum logarithms.",
      "scale"
    )
  }
  expected <- triomics_lipid_rule(method)
  if (is.null(expected)) {
    triomics_lipid_abort(
      "A_CONFIG_METHOD",
      paste0("Lipid derivation has no endpoint method '", method, "'."),
      "method"
    )
  }
  if (!identical(rule, expected)) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      paste0("quantity_rule '", rule, "' does not match lipid method '", method, "'."),
      "quantity_rule"
    )
  }
  if (!policy %in% c("unavailable", "partial", "qualified_estimate")) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      "missing_parent_policy must be unavailable, partial, or qualified_estimate.",
      "missing_parent_policy"
    )
  }
  if (method %in% c("within_class_fraction", "molar_fraction")) {
    triomics_lipid_require_text(request$denominator, "denominator")
    units <- triomics_lipid_require_text(request$units, "units")
    zero_policy <- request$zero_policy
    if (identical(zero_policy, "declared_replacement")) {
      triomics_lipid_abort(
        "A_CONFIG_OPERATION",
        "A fraction zero policy cannot invent a pseudocount for a zero class total.",
        "zero_policy"
      )
    }
    if (!is.character(zero_policy) || length(zero_policy) != 1L || is.na(zero_policy) ||
        !zero_policy %in% c("unavailable", "error")) {
      triomics_lipid_abort(
        "A_CONFIG_OPERATION",
        "Fractions require an explicit zero policy of unavailable or error.",
        "zero_policy"
      )
    }
    if (identical(method, "within_class_fraction") && grepl("mol", units, ignore.case = TRUE)) {
      triomics_lipid_abort(
        "A_SCALE_INPUT",
        "Unresolved standards/calibration cannot produce molar concentrations from intensity.",
        "units"
      )
    }
  }
  invisible(TRUE)
}

triomics_lipid_cell_role <- function(value, code) {
  code <- as.character(code)[[1L]]
  if (identical(code, "unknown")) return("unknown")
  if (!is.finite(value)) return("missing")
  if (value < 0) return("incompatible")
  if (identical(code, "known_imputed")) return("imputed")
  if (identical(code, "known_observed")) return("observed")
  if (identical(code, "known_missing")) return("missing")
  "incompatible"
}

triomics_lipid_sample_record <- function(sample_id, value, coverage, reason, n_expected,
                                         n_observed, n_missing, n_imputed, n_unknown,
                                         n_incompatible, observed_contribution,
                                         imputed_contribution) {
  list(
    sample_id = as.character(sample_id),
    value = if (is.finite(value)) unname(as.numeric(value)) else NA_real_,
    coverage = as.character(coverage),
    reason = as.character(reason),
    n_expected = triomics_lipid_count(n_expected),
    n_observed = triomics_lipid_count(n_observed),
    n_missing = triomics_lipid_count(n_missing),
    n_imputed = triomics_lipid_count(n_imputed),
    n_unknown = triomics_lipid_count(n_unknown),
    n_incompatible = triomics_lipid_count(n_incompatible),
    observed_contribution = if (is.finite(observed_contribution)) unname(as.numeric(observed_contribution)) else NA_real_,
    imputed_contribution = if (is.finite(imputed_contribution)) unname(as.numeric(imputed_contribution)) else NA_real_
  )
}

triomics_lipid_unavailable_sample <- function(sample_id, reason, n_expected, n_incompatible = 0) {
  triomics_lipid_sample_record(
    sample_id, NA_real_, "unavailable", reason, n_expected,
    0, 0, 0, 0, n_incompatible, NA_real_, NA_real_
  )
}

triomics_lipid_parent_roles <- function(frame, assay, mask, sample_id) {
  n <- nrow(frame)
  roles <- character(n)
  observed <- rep(NA_real_, n)
  imputed <- rep(NA_real_, n)
  units <- unique(frame$units)
  quantities <- unique(frame$quantity)
  mixed <- length(units) != 1L || length(quantities) != 1L
  for (i in seq_len(n)) {
    key <- frame$feature_key[[i]]
    bad <- mixed || !isTRUE(frame$compatible[[i]]) || !identical(frame$scale[[i]], "linear")
    if (!key %in% rownames(assay)) {
      roles[[i]] <- if (bad) "incompatible" else "missing"
      next
    }
    value <- suppressWarnings(as.numeric(assay[key, sample_id]))
    code <- mask[key, sample_id]
    role <- if (bad) "incompatible" else triomics_lipid_cell_role(value, code)
    roles[[i]] <- role
    if (identical(role, "observed")) observed[[i]] <- value
    if (identical(role, "imputed")) imputed[[i]] <- value
  }
  list(roles = roles, observed = observed, imputed = imputed)
}

triomics_lipid_aggregate_sample <- function(sample_id, roles, observed, imputed, property, policy, mode) {
  n_expected <- length(roles)
  n_observed <- sum(roles == "observed")
  n_missing <- sum(roles == "missing")
  n_imputed <- sum(roles == "imputed")
  n_unknown <- sum(roles == "unknown")
  n_incompatible <- sum(roles == "incompatible")
  unavailable <- function(reason) {
    triomics_lipid_sample_record(
      sample_id, NA_real_, "unavailable", reason, n_expected,
      n_observed, n_missing, n_imputed, n_unknown, n_incompatible, NA_real_, NA_real_
    )
  }
  if (n_incompatible > 0) return(unavailable("incompatible_parent"))
  if (n_unknown > 0) return(unavailable("unknown_mask"))
  if (!is.null(property) && any(!is.finite(property) | property < 0)) {
    return(unavailable("unresolved_property"))
  }
  if (identical(policy, "unavailable") && (n_missing + n_imputed) > 0) {
    return(unavailable(if (n_missing > 0) "missing_parent" else "imputed_parent"))
  }
  if (identical(policy, "partial") && n_observed == 0) return(unavailable("no_observed_parent"))
  if (identical(policy, "qualified_estimate") && n_missing > 0) return(unavailable("estimate_not_supplied"))
  if (identical(policy, "qualified_estimate") && (n_observed + n_imputed) == 0) {
    return(unavailable("no_observed_parent"))
  }
  use_imputed <- identical(policy, "qualified_estimate")
  obs_index <- roles == "observed"
  imp_index <- roles == "imputed" & use_imputed
  obs_values <- observed[obs_index]
  imp_values <- imputed[imp_index]
  obs_property <- if (is.null(property)) NULL else property[obs_index]
  imp_property <- if (is.null(property)) NULL else property[imp_index]
  parts <- c(obs_values, if (use_imputed) imp_values else numeric())
  props <- if (is.null(property)) NULL else c(obs_property, if (use_imputed) imp_property else numeric())
  obs_contrib <- if (length(obs_values)) sum(obs_values) else 0
  imp_contrib <- if (use_imputed && length(imp_values)) sum(imp_values) else 0
  if (identical(mode, "sum")) {
    value <- obs_contrib + imp_contrib
  } else if (identical(mode, "weighted")) {
    denominator <- sum(parts)
    if (!is.finite(denominator) || denominator == 0) return(unavailable("zero_denominator"))
    numerator <- sum(parts * props)
    obs_contrib <- if (length(obs_values)) sum(obs_values * obs_property) else 0
    imp_contrib <- if (use_imputed && length(imp_values)) sum(imp_values * imp_property) else 0
    value <- numerator / denominator
  } else {
    sat <- parts[props == 0]
    unsat <- parts[props > 0]
    denominator <- if (length(unsat)) sum(unsat) else 0
    if (!length(unsat) || !is.finite(denominator) || denominator == 0) {
      return(unavailable("zero_denominator"))
    }
    value <- if (length(sat)) sum(sat) / denominator else 0
    obs_contrib <- if (length(sat)) sum(sat) else 0
    imp_contrib <- denominator
  }
  if (n_missing + n_imputed == 0) {
    coverage <- "complete"
    reason <- "complete"
  } else if (identical(policy, "qualified_estimate")) {
    coverage <- "estimated"
    reason <- "qualified_estimate"
  } else {
    coverage <- "partial"
    reason <- "partial_observed"
  }
  triomics_lipid_sample_record(
    sample_id, value, coverage, reason, n_expected,
    n_observed, n_missing, n_imputed, n_unknown, n_incompatible,
    obs_contrib, imp_contrib
  )
}

triomics_lipid_endpoint_shell <- function(feature_key, endpoint_id, method, class_id, frame, request,
                                          hash, quantity, units, estimand, samples,
                                          denominator = NULL, zero_policy = NULL,
                                          disclosure = NULL, class_total_feature = NULL,
                                          class_total_retained = FALSE) {
  claim <- triomics_lipid_structural_claim(frame$structural_resolution)
  list(
    endpoint_id = endpoint_id,
    feature_key = feature_key,
    method = method,
    class_id = class_id,
    quantity = quantity,
    units = units,
    scale = "linear",
    absolute_concentration = FALSE,
    concentration_claim = "not_absolute",
    structural_resolution = claim$structural_resolution,
    structural_claim = claim$structural_claim,
    sn_positions = claim$sn_positions,
    chain_identities = claim$chain_identities,
    parent_set_ref = request$parent_set_ref,
    definition_ref = request$definition_ref,
    universe_hash = hash,
    parent_feature_keys = as.character(frame$feature_key),
    quantity_rule = request$quantity_rule,
    missing_parent_policy = request$missing_parent_policy,
    estimand = estimand,
    denominator = denominator,
    zero_policy = zero_policy,
    class_total_retained = isTRUE(class_total_retained),
    class_total_feature = class_total_feature,
    disclosure = disclosure,
    samples = samples
  )
}

triomics_lipid_fraction_disclosure <- function(samples) {
  base <- paste(
    "Within-class fractions remove class-total information from the fraction rows.",
    "The linear class total is retained beside them.",
    "Fractions are not abundances and are not a mandatory lipid correction."
  )
  if (any(vapply(samples, function(sample) identical(sample$coverage, "partial"), logical(1L)))) {
    return(paste(base, "The denominator is the partial observed class sum, not a complete universe total."))
  }
  base
}

triomics_lipid_fraction_samples <- function(frame, assay, mask, totals, policy, zero_policy) {
  sample_ids <- colnames(assay)
  per_parent <- lapply(seq_len(nrow(frame)), function(i) vector("list", length(sample_ids)))
  for (j in seq_along(sample_ids)) {
    sample_id <- sample_ids[[j]]
    total <- totals[[j]]
    roles <- triomics_lipid_parent_roles(frame, assay, mask, sample_id)
    total_value <- total$value
    zero_denominator <- is.finite(total_value) && total_value == 0
    if (zero_denominator && identical(zero_policy, "error")) {
      triomics_lipid_abort(
        "A_SCALE_ZERO",
        "The within-class fraction denominator is zero.",
        "zero_policy"
      )
    }
    for (i in seq_len(nrow(frame))) {
      role <- roles$roles[[i]]
      contributed <- if (identical(role, "observed")) {
        roles$observed[[i]]
      } else if (identical(role, "imputed") && identical(policy, "qualified_estimate")) {
        roles$imputed[[i]]
      } else {
        NA_real_
      }
      if (identical(total$coverage, "unavailable") || zero_denominator || !is.finite(total_value)) {
        reason <- if (zero_denominator) "zero_denominator" else total$reason
        per_parent[[i]][[j]] <- triomics_lipid_sample_record(
          sample_id, NA_real_, "unavailable", reason, total$n_expected,
          total$n_observed, total$n_missing, total$n_imputed, total$n_unknown,
          total$n_incompatible, NA_real_, NA_real_
        )
      } else if (!is.finite(contributed)) {
        per_parent[[i]][[j]] <- triomics_lipid_sample_record(
          sample_id, NA_real_, total$coverage, total$reason, total$n_expected,
          total$n_observed, total$n_missing, total$n_imputed, total$n_unknown,
          total$n_incompatible, NA_real_, NA_real_
        )
      } else {
        per_parent[[i]][[j]] <- triomics_lipid_sample_record(
          sample_id, contributed / total_value, total$coverage, total$reason, total$n_expected,
          total$n_observed, total$n_missing, total$n_imputed, total$n_unknown,
          total$n_incompatible, total$observed_contribution, total$imputed_contribution
        )
      }
    }
  }
  per_parent
}

triomics_lipid_class_endpoints <- function(frame, assay, mask, request, hash) {
  method <- request$method
  policy <- request$missing_parent_policy
  class_id <- frame$class_id[[1L]]
  n_classes <- request$n_classes
  mode <- if (method %in% c("weighted_carbons", "weighted_double_bonds")) {
    "weighted"
  } else if (identical(method, "saturated_unsaturated_ratio")) {
    "ratio"
  } else {
    "sum"
  }
  property <- if (identical(method, "weighted_carbons")) {
    frame$carbons
  } else if (method %in% c("weighted_double_bonds", "saturated_unsaturated_ratio")) {
    frame$double_bonds
  } else {
    NULL
  }
  sample_ids <- colnames(assay)
  totals <- lapply(sample_ids, function(sample_id) {
    roles <- triomics_lipid_parent_roles(frame, assay, mask, sample_id)
    triomics_lipid_aggregate_sample(
      sample_id, roles$roles, roles$observed, roles$imputed, property, policy, mode
    )
  })
  quantity <- unique(as.character(frame$quantity))[[1L]]
  units <- unique(as.character(frame$units))[[1L]]
  if (method %in% c("within_class_fraction", "molar_fraction")) {
    if (any(vapply(totals, function(sample) {
      is.finite(sample$value) && sample$value == 0 && identical(request$zero_policy, "error")
    }, logical(1L)))) {
      triomics_lipid_abort("A_SCALE_ZERO", "The within-class fraction denominator is zero.", "zero_policy")
    }
    fraction_samples <- triomics_lipid_fraction_samples(frame, assay, mask, totals, policy, request$zero_policy)
    disclosure <- triomics_lipid_fraction_disclosure(unlist(fraction_samples, recursive = FALSE))
    total_key <- if (identical(n_classes, 1L)) {
      paste0(request$endpoint_id, "::class_total")
    } else {
      paste0(request$endpoint_id, "::", class_id, "::class_total")
    }
    endpoints <- lapply(seq_len(nrow(frame)), function(i) {
      key <- frame$feature_key[[i]]
      feature_key <- if (identical(n_classes, 1L)) {
        paste0(request$endpoint_id, "::", key)
      } else {
        paste0(request$endpoint_id, "::", class_id, "::", key)
      }
      triomics_lipid_endpoint_shell(
        feature_key, request$endpoint_id, method, class_id, frame[i, , drop = FALSE], request, hash,
        if (identical(method, "molar_fraction")) "molar_fraction" else "fraction",
        request$units, method, fraction_samples[[i]],
        denominator = request$denominator, zero_policy = request$zero_policy,
        disclosure = disclosure, class_total_feature = total_key, class_total_retained = TRUE
      )
    })
    total_endpoint <- triomics_lipid_endpoint_shell(
      total_key, request$endpoint_id, "class_sum", class_id, frame, request, hash,
      quantity, units, "linear_class_total", totals,
      denominator = request$denominator, zero_policy = request$zero_policy,
      disclosure = disclosure, class_total_feature = total_key, class_total_retained = TRUE
    )
    return(c(endpoints, list(total_endpoint)))
  }
  feature_key <- if (identical(n_classes, 1L)) request$endpoint_id else paste0(request$endpoint_id, "::", class_id)
  estimand <- if (identical(mode, "sum")) "linear_class_total" else if (identical(mode, "weighted")) "abundance_weighted_mean" else "saturated_over_unsaturated"
  list(triomics_lipid_endpoint_shell(
    feature_key, request$endpoint_id, method, class_id, frame, request, hash,
    quantity, units, estimand, totals, class_total_retained = identical(mode, "sum")
  ))
}

triomics_lipid_state_endpoint <- function(endpoint) {
  list(
    endpoint_id = endpoint$endpoint_id,
    feature_key = endpoint$feature_key,
    method = endpoint$method,
    quantity = endpoint$quantity,
    units = endpoint$units,
    scale = endpoint$scale,
    absolute_concentration = FALSE,
    sn_positions = endpoint$sn_positions,
    chain_identities = endpoint$chain_identities,
    structural_resolution = endpoint$structural_resolution,
    universe_hash = endpoint$universe_hash,
    parent_set_ref = endpoint$parent_set_ref,
    denominator = endpoint$denominator,
    zero_policy = endpoint$zero_policy,
    class_total_retained = isTRUE(endpoint$class_total_retained),
    disclosure = endpoint$disclosure,
    samples = lapply(endpoint$samples, function(sample) {
      list(
        sample_id = sample$sample_id,
        value = triomics_lipid_json_number(sample$value),
        coverage = sample$coverage,
        reason = sample$reason,
        n_expected = triomics_lipid_count(sample$n_expected),
        n_observed = triomics_lipid_count(sample$n_observed),
        n_missing = triomics_lipid_count(sample$n_missing),
        n_imputed = triomics_lipid_count(sample$n_imputed),
        n_unknown = triomics_lipid_count(sample$n_unknown),
        n_incompatible = triomics_lipid_count(sample$n_incompatible),
        observed_contribution = triomics_lipid_json_number(sample$observed_contribution),
        imputed_contribution = triomics_lipid_json_number(sample$imputed_contribution)
      )
    })
  )
}

triomics_lipid_derivation_result <- function(assay, mask, request, block_scale) {
  triomics_lipid_check_request(request, block_scale)
  frame <- triomics_lipid_normalize_parents(request$parents)
  if (identical(request$method, "molar_fraction")) {
    calibrated <- all(frame$calibration_status == "calibrated") &&
      all(frame$quantity %in% c("concentration", "molar_fraction"))
    if (!calibrated) {
      triomics_lipid_abort(
        "A_SCALE_INPUT",
        "Unresolved standards/calibration cannot produce molar concentrations from intensity.",
        "calibration_status"
      )
    }
  }
  hash <- triomics_lipid_universe_hash(frame, request)
  classes <- unique(frame$class_id)
  request$n_classes <- length(classes)
  endpoints <- list()
  for (class_id in classes) {
    subset <- frame[frame$class_id == class_id, , drop = FALSE]
    endpoints <- c(endpoints, triomics_lipid_class_endpoints(subset, assay, mask, request, hash))
  }
  learned <- list(
    universe_hash = hash,
    method = request$method,
    parent_set_ref = request$parent_set_ref,
    definition_ref = request$definition_ref,
    quantity_rule = request$quantity_rule,
    missing_parent_policy = request$missing_parent_policy,
    absolute_concentration = FALSE,
    endpoints = lapply(endpoints, triomics_lipid_state_endpoint)
  )
  list(endpoints = endpoints, learned = learned)
}

triomics_lipid_clr_scale <- function(log_base) {
  if (!is.numeric(log_base) || length(log_base) != 1L || !is.finite(log_base) || log_base <= 1) {
    triomics_lipid_abort("A_CONFIG_OPERATION", "CLR requires an explicit log base greater than 1.", "log_base")
  }
  if (abs(log_base - 2) < 1e-8) return("clr_log2")
  if (abs(log_base - exp(1)) < 1e-8) return("clr_ln")
  triomics_lipid_abort(
    "A_CONFIG_OPERATION",
    "CLR output scale must be clr_log2 or clr_ln. A natural-log definition is not base 2.",
    "log_base"
  )
}

triomics_lipid_reference_hash <- function(keys, log_base, zero_policy, zero_value) {
  triomics_hash_text(triomics_canonical_json(list(
    keys = lapply(keys, function(key) list(feature_key = key)),
    log_base = unname(as.numeric(log_base)),
    zero_policy = zero_policy,
    zero_value = triomics_lipid_json_number(zero_value)
  )))
}

triomics_lipid_clr_values <- function(assay, reference_features, log_base, zero_policy, zero_value) {
  if (!is.character(reference_features) || !length(reference_features) || anyNA(reference_features) ||
      anyDuplicated(reference_features)) {
    triomics_lipid_abort("A_CONFIG_OPERATION", "CLR requires a fixed reference feature set.", "reference_features")
  }
  if (!setequal(reference_features, rownames(assay))) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      "The CLR reference set must be the frozen part set for this block.",
      "reference_set"
    )
  }
  output_scale <- triomics_lipid_clr_scale(log_base)
  if (!is.character(zero_policy) || length(zero_policy) != 1L || is.na(zero_policy) || !nzchar(zero_policy)) {
    triomics_lipid_abort(
      "A_CONFIG_OPERATION",
      "CLR requires an explicit zero policy. No automatic pseudocount is applied.",
      "zero_policy"
    )
  }
  if (!zero_policy %in% c("error", "declared_replacement")) {
    triomics_lipid_abort("A_CONFIG_OPERATION", "CLR zero policy must be error or declared_replacement.", "zero_policy")
  }
  replacement <- triomics_lipid_json_number(zero_value)
  result <- matrix(as.numeric(assay), nrow = nrow(assay), dimnames = dimnames(assay))
  for (sample_id in colnames(assay)) {
    parts <- as.numeric(assay[reference_features, sample_id])
    if (any(!is.finite(parts))) {
      triomics_lipid_abort(
        "A_SCALE_INPUT",
        "A CLR part is missing. Missingness is not zero and is not a pseudocount.",
        "reference_set"
      )
    }
    if (any(parts < 0)) {
      triomics_lipid_abort("A_SCALE_NONPOSITIVE", "CLR cannot transform a negative part.", "transform")
    }
    if (any(parts == 0)) {
      if (!identical(zero_policy, "declared_replacement") || is.null(replacement) || replacement <= 0) {
        triomics_lipid_abort(
          "A_SCALE_ZERO",
          "CLR found a zero without a declared positive replacement. No pseudocount is invented.",
          "zero_policy"
        )
      }
      parts[parts == 0] <- replacement
    }
    logged <- log(parts) / log(as.numeric(log_base))
    result[reference_features, sample_id] <- logged - mean(logged)
  }
  abundance_before <- lapply(rownames(assay), function(key) {
    values <- lapply(colnames(assay), function(sample_id) unname(as.numeric(assay[key, sample_id])))
    names(values) <- colnames(assay)
    values
  })
  names(abundance_before) <- rownames(assay)
  class_total <- lapply(colnames(assay), function(sample_id) unname(sum(as.numeric(assay[, sample_id]))))
  names(class_total) <- colnames(assay)
  list(
    values = result,
    output_scale = output_scale,
    abundance_before = abundance_before,
    class_total = class_total,
    reference_hash = triomics_lipid_reference_hash(reference_features, log_base, zero_policy, zero_value)
  )
}

triomics_lipid_standard_items <- function(standards_map) {
  if (is.null(standards_map)) {
    triomics_lipid_abort("A_CONFIG_OPERATION", "Internal-standard normalisation needs a standards map.", "standards_map")
  }
  if (is.data.frame(standards_map)) {
    return(lapply(seq_len(nrow(standards_map)), function(i) as.list(standards_map[i, , drop = FALSE])))
  }
  if (is.list(standards_map) && length(standards_map)) {
    if (is.list(standards_map[[1L]]) && !is.null(standards_map[[1L]]$feature_key)) return(standards_map)
  }
  triomics_lipid_abort("A_CONFIG_OPERATION", "The standards map must name each analyte and standard.", "standards_map")
}

triomics_lipid_resolve_standard <- function(item) {
  status <- triomics_lipid_text(item$calibration_status, "unresolved")
  factor <- triomics_lipid_optional_number(item$calibration_factor)
  quantity <- triomics_lipid_text(item$calibrated_quantity, "")
  units <- triomics_lipid_text(item$calibrated_units, "")
  factor_ok <- is.finite(factor) && factor > 0
  resolved <- identical(status, "calibrated") && factor_ok && nzchar(quantity) && nzchar(units)
  if (!resolved) {
    triomics_lipid_abort(
      "A_SCALE_INPUT",
      "Unresolved standards/calibration cannot produce molar concentrations from intensity.",
      "standards_map"
    )
  }
  list(
    feature_key = triomics_lipid_require_text(item$feature_key, "feature_key"),
    standard_key = triomics_lipid_require_text(item$standard_key, "standard_key"),
    calibration_factor = unname(as.numeric(factor)),
    calibration_status = status,
    calibrated_quantity = quantity,
    calibrated_units = units,
    absolute_concentration = grepl("mol", units, ignore.case = TRUE) ||
      identical(quantity, "concentration") || identical(quantity, "molar_fraction")
  )
}
