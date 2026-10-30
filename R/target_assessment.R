triomics_target_profile <- function(request, links, observations) {
  invalid <- function(message, field) triomics_d_abort('D_TARGET_INPUT_INVALID', message, field)
  if (!is.list(request) || !triomics_target_string(request$request_id) ||
      !triomics_target_string(request$namespace) || !triomics_target_string(request$id) ||
      !is.numeric(request$taxon_id) || length(request$taxon_id) != 1L ||
      !is.finite(request$taxon_id) || request$taxon_id <= 0 || request$taxon_id != floor(request$taxon_id))
    invalid('A valid target request is required.', 'request')
  if (!triomics_target_string(request$mechanism_class) ||
      !request$mechanism_class %in% c('non_enzymatic', 'enzymatic') ||
      (identical(request$id, 'SYNTH_FCN3_LIKE') && request$mechanism_class != 'non_enzymatic'))
    invalid('A valid mechanism class is required for the target.', 'mechanism_class')
  if (!is.list(links) || is.data.frame(links) || !is.list(observations) || is.data.frame(observations))
    invalid('Links and observations must be lists of records.', 'links')
  collapsed <- function(x) {
    if (!is.list(x)) return(FALSE)
    any(tolower(names(x)) %in% c('kd', 'binding_probability', 'flux', 'causal_mediation', 'binding', 'activity')) ||
      any(vapply(x, collapsed, logical(1)))
  }
  if (collapsed(request) || collapsed(links) || collapsed(observations))
    triomics_d_abort('D_TARGET_CLAIM_COLLAPSED', 'Inferred binding, flux or causal claims are not permitted.', 'observations')
  for (link in links) {
    if (!is.list(link)) invalid('Each link must be a record.', 'links')
    if (!triomics_target_string(link$sign) || !triomics_target_string(link$context_id) || !triomics_target_string(link$source_id))
      triomics_d_abort('D_TARGET_PATH_UNDOCUMENTED', 'Each path requires a sign, context and source.', 'links')
    if (!triomics_target_string(link$link_id) || !triomics_target_string(link$relation) ||
        !link$relation %in% c('regulatory', 'perturbational', 'enzymatic_reaction') ||
        !link$sign %in% c('positive', 'negative', 'unsigned', 'unknown'))
      invalid('Invalid mechanistic link.', 'links')
    if (identical(request$mechanism_class, 'non_enzymatic') && link$relation == 'enzymatic_reaction')
      triomics_d_abort('D_TARGET_REACTION_INVENTED', 'A non-enzymatic target cannot have an invented reaction.', 'links')
  }
  link_ids <- vapply(links, `[[`, character(1), 'link_id')
  if (anyDuplicated(link_ids)) invalid('Link identifiers must be unique.', 'links')
  rows <- lapply(observations, function(obs) {
    if (!is.list(obs) || !triomics_target_string(obs$link_id) || !obs$link_id %in% link_ids)
      triomics_d_abort('D_TARGET_PATH_UNDOCUMENTED', 'Each observation requires a matching documented path.', 'observations')
    link <- links[[match(obs$link_id, link_ids)]]
    if (!is.list(obs) || !all(c('observation_id', 'endpoint', 'value', 'dependency_group', 'status') %in% names(obs)) ||
        !triomics_target_string(obs$observation_id) || !triomics_target_string(obs$endpoint) ||
        !triomics_target_string(obs$dependency_group) || !triomics_target_string(obs$status) ||
        !obs$status %in% c('support', 'conflict', 'ambiguous', 'uninformative') ||
        !(is.null(obs$value) || (is.numeric(obs$value) && length(obs$value) == 1L && is.finite(obs$value))))
      invalid('Invalid observation record.', 'observations')
    if (tolower(obs$endpoint) %in% c('binding', 'flux', 'activity', 'causal_mediation', 'kd', 'binding_probability'))
      triomics_d_abort('D_TARGET_CLAIM_COLLAPSED', 'Observation endpoints cannot be converted to mechanistic claims.', 'observations')
    obs$path_sign <- link$sign
    obs$path_context <- link$context_id
    obs$coverage <- if (is.null(obs$value)) 'unmeasured' else 'measured'
    obs
  })
  list(request_id = request$request_id, target_id = request$id, request = request,
       links = links, observations = rows,
       axes = unique(vapply(rows, `[[`, character(1), 'endpoint')),
       coverage = if (length(rows) == 0L || any(vapply(rows, function(x) x$coverage == 'unmeasured', logical(1)))) 'unmeasured' else 'measured',
       independent_count = length(unique(vapply(rows, `[[`, character(1), 'dependency_group'))))
}
