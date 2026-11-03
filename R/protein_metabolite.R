triomics_protein_metabolite_report <- function(investigation, documented_links = list(), measured_changes = list()) {
  empty <- list(proteins = list(), selected_target = NULL)
  if (!isTRUE(investigation$enabled)) return(empty)

  targets <- investigation$targets %||% list()
  if (!triomics_d_sequence(targets) || !triomics_d_sequence(documented_links) ||
      !triomics_d_sequence(measured_changes)) {
    triomics_d_abort("D_TARGET_INPUT_INVALID", "Targets, links and changes must be lists of records.")
  }
  claim_tokens <- c("binding", "flux", "activity", "causal_mediation", "kd", "binding_probability")
  for (record in c(documented_links, measured_changes)) {
    if (is.list(record) && any(tolower(names(record)) %in% claim_tokens)) {
      triomics_d_abort("D_TARGET_CLAIM_COLLAPSED", "Mechanistic claim axes are not permitted.")
    }
  }
  for (change in measured_changes) {
    if (is.list(change) && triomics_target_string(change$endpoint) &&
        tolower(change$endpoint) %in% claim_tokens) {
      triomics_d_abort("D_TARGET_CLAIM_COLLAPSED", "Mechanistic claim axes are not permitted.", "measured_changes")
    }
  }
  links <- Filter(function(link) {
    if (!is.list(link)) triomics_d_abort("D_TARGET_INPUT_INVALID", "Each link must be a record.", "documented_links")
    if (!triomics_target_string(link$evidence_ref)) return(FALSE)
    fields <- c("link_id", "protein_namespace", "protein_id", "metabolite_namespace", "metabolite_id", "relation", "sign")
    if (!all(vapply(fields, function(field) triomics_target_string(link[[field]]), logical(1))) ||
        !identical(link$relation, "documented_association") ||
        !link$sign %in% c("positive", "negative", "unsigned", "unknown")) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Invalid documented link.", "documented_links")
    }
    TRUE
  }, documented_links)
  link_fields <- c("link_id", "protein_namespace", "protein_id", "metabolite_namespace", "metabolite_id", "relation", "evidence_ref", "sign")
  links <- lapply(links, function(link) link[link_fields])
  for (change in measured_changes) {
    fields <- c("change_id", "metabolite_namespace", "metabolite_id", "link_id", "endpoint", "status")
    if (!is.list(change) || !all(c(fields, "value") %in% names(change)) ||
        !all(vapply(fields, function(field) triomics_target_string(change[[field]]), logical(1))) ||
        !change$status %in% c("support", "conflict", "ambiguous", "uninformative") ||
        !(is.null(change$value) || (is.numeric(change$value) && length(change$value) == 1L && is.finite(change$value)))) {
      triomics_d_abort("D_TARGET_INPUT_INVALID", "Invalid measured change.", "measured_changes")
    }
  }
  change_fields <- c("change_id", "metabolite_namespace", "metabolite_id", "link_id", "endpoint", "value", "status")
  measured_changes <- lapply(measured_changes, function(change) change[change_fields])
  proteins <- lapply(targets, function(target) {
    if (!is.list(target)) triomics_d_abort("D_TARGET_IDENTITY_REQUIRED", "A target identity is required.", "target_investigation.targets")
    namespace <- triomics_d_one_string(target$namespace, "D_TARGET_IDENTITY_REQUIRED", "A target needs a namespace.", "target_investigation.targets")
    id <- triomics_d_one_string(target$id, "D_TARGET_IDENTITY_REQUIRED", "A target needs an identifier.", "target_investigation.targets")
    entity_type <- triomics_d_one_string(target$entity_type, "D_TARGET_IDENTITY_REQUIRED", "A target needs an entity type.", "target_investigation.targets")
    taxon_id <- target$taxon_id
    if (!triomics_target_string(namespace) || !triomics_target_string(id) ||
        !entity_type %in% c("protein", "complex", "pathway", "other_declared") ||
        !is.numeric(taxon_id) || length(taxon_id) != 1L || !is.finite(taxon_id) ||
        taxon_id <= 0 || taxon_id != floor(taxon_id)) {
      triomics_d_abort("D_TARGET_IDENTITY_REQUIRED", "A namespaced target, positive taxon and entity type are required.", "target_investigation.targets")
    }
    kept_links <- if (identical(entity_type, "protein")) Filter(function(link) {
      identical(link$protein_namespace, namespace) && identical(link$protein_id, id)
    }, links) else list()
    kept_changes <- Filter(function(change) {
      any(vapply(kept_links, function(link) {
        identical(change$link_id, link$link_id) &&
          identical(change$metabolite_namespace, link$metabolite_namespace) &&
          identical(change$metabolite_id, link$metabolite_id)
      }, logical(1)))
    }, measured_changes)
    list(namespace = namespace, id = id, taxon_id = taxon_id, entity_type = entity_type,
         links = kept_links, changes = kept_changes,
         link_status = if (!identical(entity_type, "protein")) "not_applicable" else if (length(kept_links)) "documented" else "not_documented",
         change_status = if (length(kept_changes)) "retained" else "not_attributed",
         coverage = if (length(kept_changes) && all(vapply(kept_changes, function(change) !is.null(change$value), logical(1)))) "measured" else "unmeasured",
         binding_status = "unavailable", binding_reason = "D_TARGET_EVIDENCE_ABSENT")
  })
  list(proteins = proteins, selected_target = NULL)
}
