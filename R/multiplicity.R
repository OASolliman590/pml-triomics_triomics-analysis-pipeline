triomics_b06_context <- new.env(parent = emptyenv())
triomics_b06_context$pending_selection <- NULL

triomics_b06_bad_qualification <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    return(TRUE)
  }
  token <- tolower(trimws(value))
  grepl("\\b(unknown|unqualified|invalid|not assessed|not qualified)\\b", token, perl = TRUE)
}

triomics_b06_selection_gate <- function(resolved_analysis, block_id) {
  selection <- resolved_analysis$selection
  if (is.null(selection)) return(list(valid = FALSE, reason = "selection_inventory_missing"))
  if (!is.data.frame(selection) || !all(c("block_id", "inference_gate") %in% names(selection))) {
    return(list(valid = FALSE, reason = "selection_inventory_invalid"))
  }
  block_id <- as.character(block_id)
  if (length(block_id) != 1L || is.na(block_id) || !nzchar(block_id) ||
      !block_id %in% names(resolved_analysis$blocks)) {
    return(list(valid = FALSE, reason = "selection_inventory_block_invalid"))
  }
  if (!block_id %in% as.character(selection$block_id)) {
    return(list(valid = FALSE, reason = "selection_inventory_incomplete"))
  }
  selected <- selection[as.character(selection$block_id) == block_id, , drop = FALSE]
  gates <- as.character(selected$inference_gate)
  if (!length(gates) || anyNA(gates) || any(!nzchar(gates))) {
    return(list(valid = FALSE, reason = "selection_inventory_unknown"))
  }
  if (any(gates == "withheld")) {
    return(list(valid = FALSE, reason = "selection_inventory_withheld"))
  }
  if (any(gates == "panel_only")) {
    return(list(valid = FALSE, reason = "source_panel_scope_unqualified"))
  }
  if (any(gates != "declared_unselected")) {
    return(list(valid = FALSE, reason = "selection_inventory_unknown"))
  }
  list(valid = TRUE, reason = NULL)
}

triomics_b06_family_validation <- function(resolved_analysis, statistics_spec, family, rows, block_id) {
  reasons <- character()
  if (!identical(statistics_spec$method, "limma")) reasons <- c(reasons, "model_method_unqualified")
  if (!identical(statistics_spec$family_scope, "one_scalar_contrast_all_retained_features")) {
    reasons <- c(reasons, "family_scope_not_fixed")
  }
  if (!identical(statistics_spec$selection_validity, "qualified")) {
    reasons <- c(reasons, "selection_not_qualified")
  }
  if (triomics_b06_bad_qualification(statistics_spec$selection_justification)) {
    reasons <- c(reasons, "selection_justification_unqualified")
  }
  if (triomics_b06_bad_qualification(statistics_spec$dependence_justification)) {
    reasons <- c(reasons, "dependence_not_qualified")
  }
  model_plan <- resolved_analysis$model_plan
  if (!is.list(model_plan) || !is.character(model_plan$formula) ||
      length(model_plan$formula) != 1L || !nzchar(model_plan$formula) ||
      !is.character(model_plan$column_ids) || !length(model_plan$column_ids) ||
      anyNA(model_plan$column_ids) || any(!nzchar(model_plan$column_ids)) ||
      anyDuplicated(model_plan$column_ids)) {
    reasons <- c(reasons, "model_plan_unqualified")
  }
  inventory <- triomics_b06_selection_gate(resolved_analysis, block_id)
  if (!isTRUE(inventory$valid)) reasons <- c(reasons, inventory$reason)

  member_ids <- if (is.list(family)) as.character(unlist(family$member_ids, use.names = FALSE)) else character()
  row_ids <- vapply(rows, function(row) {
    if (is.null(row$feature_key)) "" else as.character(row$feature_key)
  }, character(1))
  if (length(member_ids) != length(rows) || anyDuplicated(member_ids) ||
      anyDuplicated(row_ids) || !identical(unname(member_ids), unname(row_ids))) {
    reasons <- c(reasons, "family_membership_mismatch")
  }
  if (length(rows) == 0L) reasons <- c(reasons, "family_empty")

  expected_internal <- rep(1, length(rows))
  names(expected_internal) <- row_ids
  estimated <- logical(length(rows))
  model_hashes <- character(length(rows))
  invalid_p <- FALSE
  for (index in seq_along(rows)) {
    row <- rows[[index]]
    estimated[[index]] <- identical(row$estimation_status, "estimated")
    model_hashes[[index]] <- if (is.character(row$model_hash) && length(row$model_hash) == 1L) {
      row$model_hash
    } else {
      ""
    }
    p <- row$p
    if (estimated[[index]]) {
      if (is.numeric(p) && length(p) == 1L && !is.na(p) && is.finite(p) && p >= 0 && p <= 1) {
        expected_internal[[index]] <- as.numeric(p)
      } else {
        invalid_p <- TRUE
      }
    }
  }
  if (invalid_p) reasons <- c(reasons, "estimated_p_unavailable_or_invalid")
  if (any(!nzchar(model_hashes)) || length(unique(model_hashes)) != 1L) {
    reasons <- c(reasons, "model_hash_unqualified")
  }

  if (length(member_ids) != length(rows) ||
      !is.numeric(family$internal_p) || length(family$internal_p) != length(rows) ||
      any(!is.finite(family$internal_p)) || any(family$internal_p < 0 | family$internal_p > 1) ||
      !isTRUE(all.equal(unname(as.numeric(family$internal_p)), unname(expected_internal)))) {
    reasons <- c(reasons, "internal_family_p_invalid")
  }
  reasons <- unique(reasons)
  list(
    eligible = length(reasons) == 0L,
    reasons = reasons,
    selection_inventory_status = if (isTRUE(inventory$valid)) "complete" else "withheld",
    n_declared = as.integer(length(rows)),
    n_estimated = as.integer(sum(estimated)),
    n_failed = as.integer(length(rows) - sum(estimated)),
    internal_p = unname(as.numeric(expected_internal)),
    internal_q = unname(as.numeric(stats::p.adjust(expected_internal, method = "BH")))
  )
}

triomics_b06_process_family <- function(resolved_analysis, statistics_spec, result,
                                         block_id, claim_id, claim_scope = "scalar_contrast") {
  if (!is.list(result) || !is.list(result$rows) || !is.list(result$family)) return(result)
  rows <- result$rows
  family <- result$family
  validation <- triomics_b06_family_validation(
    resolved_analysis, statistics_spec, family, rows, block_id
  )
  family$member_ids <- vapply(rows, function(row) as.character(row$feature_key), character(1))
  family$member_hash <- triomics_b_hash(as.character(family$member_ids))
  family$n_declared <- validation$n_declared
  family$n_estimated <- validation$n_estimated
  family$n_failed <- validation$n_failed
  family$n_eligible <- validation$n_estimated
  family$n_evaluated <- validation$n_declared
  family$dispositions <- c(estimated = validation$n_estimated, failed = validation$n_failed)
  family$internal_p <- validation$internal_p
  family$internal_q <- validation$internal_q
  family$correction_method <- "BH"
  family$claim_scope <- claim_scope
  family$q_published <- isTRUE(family$q_published) && isTRUE(validation$eligible)
  family$validation <- list(
    eligible = isTRUE(validation$eligible),
    status = if (isTRUE(validation$eligible)) "eligible" else "withheld",
    reasons = validation$reasons,
    selection_inventory_status = validation$selection_inventory_status
  )
  for (index in seq_along(rows)) {
    failed <- !identical(rows[[index]]$estimation_status, "estimated")
    rows[[index]]$family_id <- family$family_id
    rows[[index]]["p"] <- list(if (failed) NULL else rows[[index]]$p)
    rows[[index]]["q"] <- list(
      if (!failed && isTRUE(family$q_published)) family$internal_q[[index]] else NULL
    )
    if (!isTRUE(validation$eligible)) rows[[index]]$inferential_status <- "withheld"
  }
  result$rows <- rows
  result$family <- family
  result$validation <- family$validation
  result$nominations <- triomics_b06_nominate(
    rows = rows,
    family = family,
    statistics_spec = statistics_spec,
    analysis_id = resolved_analysis$analysis_id,
    block_id = block_id,
    claim_id = claim_id
  )
  result$sensitivity <- triomics_b06_rank_sensitivity(
    resolved_analysis = resolved_analysis,
    statistics_spec = statistics_spec,
    claim_id = claim_id,
    block_id = block_id,
    current = result$sensitivity
  )
  result
}

triomics_b06_finalize_fit <- function(resolved_analysis, statistics_spec, fit) {
  if (!is.list(fit) || !is.list(fit$blocks)) return(fit)
  for (block_id in names(fit$blocks)) {
    block <- fit$blocks[[block_id]]
    if (is.list(block$contrasts)) {
      for (contrast_id in names(block$contrasts)) {
        block$contrasts[[contrast_id]] <- triomics_b06_process_family(
          resolved_analysis, statistics_spec, block$contrasts[[contrast_id]],
          block_id, contrast_id, "scalar_contrast"
        )
      }
    }
    if (is.list(block$joint_claims)) {
      for (claim_id in names(block$joint_claims)) {
        block$joint_claims[[claim_id]] <- triomics_b06_process_family(
          resolved_analysis, statistics_spec, block$joint_claims[[claim_id]],
          block_id, claim_id, "joint_contrast"
        )
      }
    }
    fit$blocks[[block_id]] <- block
  }
  fit
}

triomics_b06_dispatch_fit <- function(original, parent) {
  force(original)
  env <- new.env(parent = parent)
  env$b_t06_original <- original
  dispatch <- function(resolved_analysis, statistics_spec) {
    fitted <- get("b_t06_original", envir = environment(), inherits = TRUE)(
      resolved_analysis, statistics_spec
    )
    triomics_b06_finalize_fit(resolved_analysis, statistics_spec, fitted)
  }
  environment(dispatch) <- env
  attr(dispatch, "b_t06_dispatch") <- TRUE
  dispatch
}

triomics_b06_install_namespace_dispatch <- function(pkgname) {
  envir <- asNamespace(pkgname)
  if (!exists("triomics_fit_block_models", envir = envir, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = envir, inherits = FALSE)
  if (isTRUE(attr(current, "b_t06_dispatch"))) return(invisible(TRUE))
  dispatch <- triomics_b06_dispatch_fit(current, envir)
  triomics_b02_rebind(envir, "triomics_fit_block_models", dispatch)
  triomics_b02_rebind(envir, "fit_block_models", dispatch)
  invisible(TRUE)
}

triomics_b06_install_global_dispatch <- function() {
  if (!exists("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  current <- get("triomics_fit_block_models", envir = .GlobalEnv, inherits = FALSE)
  if (!isTRUE(attr(current, "b_t06_dispatch"))) {
    dispatch <- triomics_b06_dispatch_fit(current, .GlobalEnv)
    assign("triomics_fit_block_models", dispatch, envir = .GlobalEnv)
    assign("fit_block_models", dispatch, envir = .GlobalEnv)
  }
  invisible(TRUE)
}

triomics_b06_wrap_ingest <- function(original, context) {
  force(original)
  force(context)
  wrapper <- function(config, config_path) {
    context$pending_selection <- NULL
    ingested <- original(config, config_path)
    context$pending_selection <- ingested$selection
    ingested
  }
  attr(wrapper, "b_t06_ingest_dispatch") <- TRUE
  wrapper
}

triomics_b06_wrap_resolve <- function(original, context) {
  force(original)
  force(context)
  wrapper <- function(prepared_study, analysis_spec) {
    on.exit(context$pending_selection <- NULL, add = TRUE)
    resolved <- original(prepared_study, analysis_spec)
    selection <- context$pending_selection
    if (is.null(selection) && is.list(prepared_study)) selection <- prepared_study$selection
    if (!is.null(selection) && is.list(resolved)) resolved$selection <- selection
    resolved
  }
  attr(wrapper, "b_t06_resolve_dispatch") <- TRUE
  wrapper
}

triomics_b06_install_source_context <- function(envir, namespace = FALSE) {
  set_binding <- function(name, value) {
    if (namespace) triomics_b02_rebind(envir, name, value) else assign(name, value, envir = envir)
  }
  if (exists("triomics_ingest_configured_study", envir = envir, inherits = FALSE)) {
    current <- get("triomics_ingest_configured_study", envir = envir, inherits = FALSE)
    if (!isTRUE(attr(current, "b_t06_ingest_dispatch"))) {
      set_binding("triomics_ingest_configured_study", triomics_b06_wrap_ingest(current, triomics_b06_context))
    }
  }
  for (name in c("triomics_resolve_analysis", "resolve_analysis")) {
    if (exists(name, envir = envir, inherits = FALSE)) {
      current <- get(name, envir = envir, inherits = FALSE)
      if (!isTRUE(attr(current, "b_t06_resolve_dispatch"))) {
        set_binding(name, triomics_b06_wrap_resolve(current, triomics_b06_context))
      }
    }
  }
  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  envir <- asNamespace(pkgname)
  if (exists("triomics_install_stage_b_namespace_dispatch", envir = envir, inherits = FALSE)) {
    get("triomics_install_stage_b_namespace_dispatch", envir = envir, inherits = FALSE)(pkgname)
  }
}

if (!isNamespace(topenv()) && exists("triomics_main", envir = .GlobalEnv, inherits = FALSE)) {
  triomics_main_before_b06 <- get("triomics_main", envir = .GlobalEnv, inherits = FALSE)
  assign("triomics_main_before_b06", triomics_main_before_b06, envir = .GlobalEnv)
  assign("triomics_main", function(args) {
    if (exists("triomics_install_stage_b_global_dispatch", envir = .GlobalEnv, inherits = FALSE)) {
      triomics_install_stage_b_global_dispatch()
    }
    triomics_main_before_b06(args)
  }, envir = .GlobalEnv)
}
