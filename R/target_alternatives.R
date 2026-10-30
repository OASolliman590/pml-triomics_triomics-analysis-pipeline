triomics_target_alternatives <- function(profiles) {
  valid <- is.list(profiles) && !is.data.frame(profiles) && length(profiles) >= 2L &&
    all(vapply(profiles, function(p) {
      is.list(p) && triomics_target_string(p$target_id) && is.character(p$axes) &&
        !anyNA(p$axes) && all(nzchar(trimws(p$axes))) &&
        triomics_target_string(p$coverage) && p$coverage %in% c('measured', 'unmeasured')
    }, logical(1)))
  if (!valid) triomics_d_abort('D_TARGET_INPUT_INVALID', 'At least two valid target profiles are required.', 'profiles')
  claim_names <- c('binding', 'flux', 'activity', 'causal_mediation', 'kd', 'binding_probability')
  collapsed <- function(x) {
    if (!is.list(x)) return(FALSE)
    any(tolower(names(x)) %in% claim_names) || any(vapply(x, collapsed, logical(1)))
  }
  if (collapsed(profiles) || any(vapply(profiles, function(p) any(tolower(p$axes) %in% claim_names), logical(1))))
    triomics_d_abort('D_TARGET_CLAIM_COLLAPSED', 'Mechanistic claim axes are not permitted.', 'profiles')
  same_axes <- all(vapply(profiles, function(p) setequal(p$axes, profiles[[1]]$axes), logical(1)))
  measured <- all(vapply(profiles, function(p) p$coverage == 'measured', logical(1)))
  list(profiles = profiles, comparability = if (same_axes && measured) 'comparable' else 'noncomparable',
       selected_target = NULL, tie_status = 'unresolved')
}
