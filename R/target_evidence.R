triomics_target_external_evidence <- function(request, records) {
  if (!is.list(request) || !triomics_target_string(request$request_id) ||
      !is.list(records) || is.data.frame(records))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'A target request and list of assay records are required.', 'records')
  collapsed <- function(fields) {
    fields <- tolower(fields)
    any(fields %in% c('composite_score', 'support_score', 'potency_score', 'binding_probability') |
      grepl('^kd($|_)', fields))
  }
  if (collapsed(names(records)) || any(vapply(records, function(r) collapsed(names(r)), logical(1))))
    triomics_d_abort('D_TARGET_AXIS_COLLAPSED', 'Collapsed assay fields are not permitted.', 'records')
  required <- c('evidence_id', 'axis', 'endpoint', 'units', 'relation', 'source_id', 'publication_group', 'measured_by_pipeline', 'value')
  valid <- vapply(records, function(r) {
    if (!is.list(r) || !all(required %in% names(r))) return(FALSE)
    strings <- c('evidence_id', 'axis', 'endpoint', 'units', 'relation', 'source_id', 'publication_group')
    all(vapply(r[strings], triomics_target_string, logical(1))) &&
      r$axis %in% c('binding', 'function', 'engagement', 'association') &&
      is.logical(r$measured_by_pipeline) && length(r$measured_by_pipeline) == 1L && !is.na(r$measured_by_pipeline) &&
      (is.null(r$value) || (is.numeric(r$value) && length(r$value) == 1L && is.finite(r$value))) &&
      (is.null(r$taxon_id) || (is.numeric(r$taxon_id) && length(r$taxon_id) == 1L && is.finite(r$taxon_id) && r$taxon_id > 0 && r$taxon_id == floor(r$taxon_id))) &&
      (is.null(r$context_id) || triomics_target_string(r$context_id)) &&
      (is.null(r$construct) || triomics_target_string(r$construct)) &&
      (is.null(r$compound_id) || triomics_target_string(r$compound_id)) &&
      (is.null(r$compound_namespace) || triomics_target_string(r$compound_namespace))
  }, logical(1))
  if (!all(valid) || anyDuplicated(vapply(records, `[[`, character(1), 'evidence_id')))
    triomics_d_abort('D_TARGET_INPUT_INVALID', 'Assay records must be valid and uniquely identified.', 'records')
  if (any(vapply(records, `[[`, logical(1), 'measured_by_pipeline')))
    triomics_d_abort('D_TARGET_EXTERNAL_ONLY', 'Only external assay records are accepted.', 'measured_by_pipeline')
  rows <- lapply(records, function(r) {
    mismatch <- (!is.null(r$taxon_id) && r$taxon_id != request$taxon_id) ||
      (!is.null(r$context_id) && !identical(r$context_id, request$context$context_id))
    complete_match <- !is.null(r$taxon_id) && !is.null(r$context_id) &&
      triomics_target_string(request$context$context_id) && !is.null(r$construct) &&
      r$construct %in% unlist(request$constructs, use.names = FALSE) &&
      !is.null(r$compound_namespace) && triomics_target_string(request$drug$namespace) &&
      identical(r$compound_namespace, request$drug$namespace) &&
      !is.null(r$compound_id) && triomics_target_string(request$drug$drug_id) &&
      identical(r$compound_id, request$drug$drug_id)
    r$attachment <- if (mismatch) 'context_mismatch' else if (r$axis == 'binding' && !isTRUE(request$binding_target)) 'not_binding_target' else if (identical(request$identity_status, 'ambiguous') || !complete_match) 'unresolved' else 'request'
    r$dependency_group <- paste0('publication:', r$publication_group)
    r$independent_replication <- FALSE
    r$availability <- if (is.null(r$value)) 'unavailable' else 'available'
    r
  })
  list(request_id = request$request_id, records = rows,
       independent_group_count = length(unique(vapply(rows, `[[`, character(1), 'publication_group'))))
}
