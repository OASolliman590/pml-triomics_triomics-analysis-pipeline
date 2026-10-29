triomics_target_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

triomics_target_request <- function(identity, mode, selection, context = NULL, drug = NULL, universe = NULL) {
  invalid_identity <- !is.list(identity) || !triomics_target_string(identity$namespace) ||
    !triomics_target_string(identity$id) || !is.numeric(identity$taxon_id) ||
    length(identity$taxon_id) != 1L || !is.finite(identity$taxon_id) ||
    identity$taxon_id <= 0 || identity$taxon_id != floor(identity$taxon_id) ||
    !triomics_target_string(identity$entity_type) ||
    !identity$entity_type %in% c('protein', 'complex', 'pathway', 'other_declared')
  if (invalid_identity) triomics_d_abort('D_TARGET_IDENTITY_REQUIRED', 'A namespaced target, positive taxon and entity type are required.', 'identity')
  if (!triomics_target_string(mode) || !mode %in% c('assess', 'compare', 'discover'))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'Target mode is invalid.', 'mode')
  if (!is.null(context) && !is.list(context))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'Target context must be a list.', 'context')
  if (!is.null(drug) && (!is.list(drug) || (length(drug) != 3L || !setequal(names(drug), c('namespace', 'drug_id', 'label'))) ||
    !triomics_target_string(drug$namespace) || !triomics_target_string(drug$drug_id) || !triomics_target_string(drug$label)))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'Drug must declare only a namespace, ID and label.', 'drug')
  if (mode == 'discover' && (!is.list(universe) || !triomics_target_string(universe$universe_id) ||
    !triomics_target_string(universe$version) || !identical(universe$enumeration, 'unranked')))
    triomics_d_abort('D_TARGET_UNIVERSE_REQUIRED', 'Discovery requires a versioned unranked universe.', 'universe')
  if (!is.list(selection) || is.data.frame(selection))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'Selection must be a list of records.', 'selection')
  valid_selection <- vapply(selection, function(s) is.list(s) && triomics_target_string(s$selection_id) &&
    triomics_target_string(s$hypothesis_status) && s$hypothesis_status %in% c('exploratory', 'confirmed') &&
    is.logical(s$selected_with_observations) && length(s$selected_with_observations) == 1L && !is.na(s$selected_with_observations), logical(1))
  if (!all(valid_selection) || anyDuplicated(vapply(selection, `[[`, character(1), 'selection_id')))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'Selection records must be valid and uniquely identified.', 'selection')
  selection <- lapply(selection, function(s) {
    if (s$hypothesis_status == 'confirmed' && (s$selected_with_observations ||
      !triomics_target_string(context$confirmation_protocol))) s$hypothesis_status <- 'exploratory'
    s
  })
  list(request_id = paste0('target:', identity$namespace, ':', identity$id, ':taxon:', identity$taxon_id),
       namespace = identity$namespace, id = identity$id, taxon_id = identity$taxon_id,
       entity_type = identity$entity_type, constructs = identity$constructs,
       identity_status = if (length(identity$constructs) > 1L) 'ambiguous' else 'unique',
       binding_target = identity$entity_type != 'pathway', mode = mode, selection = selection,
       context = context, drug = drug, universe = universe)
}
