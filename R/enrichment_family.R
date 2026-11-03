# Inputs are already resolved to unique analysis-entity identifiers by the caller.
triomics_ora_family <- function(universe, sets, nominated, inventory, mapping = NULL) {
  if (!is.list(universe) || is.null(universe$ids) || !identical(universe$source, 'measured')) {
    triomics_d_abort('D_ORA_UNIVERSE_UNDECLARED', 'Declare the measured eligible entity list.', 'universe')
  }
  valid_ids <- function(x) is.character(x) && !anyNA(x) && all(nzchar(x))
  if (!valid_ids(universe$ids) || !valid_ids(nominated) || !is.list(sets) ||
      !is.list(inventory) || !is.character(inventory$status) || length(inventory$status) != 1L || is.na(inventory$status)) {
    triomics_d_abort('D_ORA_INPUT_INVALID', 'Provide entity identifiers, a set family and inventory status.', 'input')
  }
  if (!inventory$status %in% c('full_unselected', 'prespecified_panel', 'unknown', 'outcome_selected')) {
    triomics_d_abort('D_ORA_INPUT_INVALID', 'Inventory status must use the declared vocabulary.', 'inventory.status')
  }
  valid_scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  if (!is.list(mapping) || !valid_scalar(mapping$rule) || !valid_scalar(mapping$status) ||
      !mapping$status %in% c('compatible', 'incompatible')) {
    triomics_d_abort('D_ORA_MAPPING_INVALID', 'Declare a mapping rule and compatible or incompatible status.', 'mapping')
  }
  ids <- unique(universe$ids)
  nominations <- unique(nominated)
  selected <- intersect(nominations, ids)
  if (length(ids) > 0L && length(selected) == length(ids)) {
    triomics_d_abort('D_ORA_BACKGROUND_INVALID', 'The selected list cannot define its own background.', 'universe')
  }
  valid_set <- function(s) is.list(s) && is.character(s$set_id) && length(s$set_id) == 1L &&
    !is.na(s$set_id) && nzchar(s$set_id) && valid_ids(s$members) &&
    is.logical(s$eligible) && length(s$eligible) == 1L && !is.na(s$eligible)
  if (!all(vapply(sets, valid_set, logical(1))) || anyDuplicated(vapply(sets, `[[`, character(1), 'set_id'))) {
    triomics_d_abort('D_ORA_INPUT_INVALID', 'Each set needs a unique ID, members and eligibility.', 'sets')
  }
  qualified <- inventory$status %in% c('full_unselected', 'prespecified_panel') && identical(mapping$status, 'compatible')
  rows <- lapply(sets, function(s) {
    members <- intersect(unique(s$members), ids)
    counts <- list(N = length(ids), K = length(members), n = length(selected), k = length(intersect(members, selected)),
                   members_outside_universe = length(setdiff(unique(s$members), ids)))
    status <- if (!qualified) 'unavailable' else if (!length(ids) || !length(members) || !s$eligible) 'non_estimable' else 'estimable'
    p <- if (identical(status, 'estimable')) triomics_ora_upper_tail(counts$N, counts$K, counts$n, counts$k) else NULL
    c(list(set_id = s$set_id, status = status), counts, list(p = p, q = NULL, claim = 'not_pathway_activation'))
  })
  estimable <- which(vapply(rows, function(row) identical(row$status, 'estimable'), logical(1)))
  if (length(estimable)) {
    adjusted <- stats::p.adjust(vapply(rows[estimable], `[[`, numeric(1), 'p'), method = 'BH')
    for (i in seq_along(estimable)) rows[[estimable[[i]]]]['q'] <- list(adjusted[[i]])
  }
  statuses <- vapply(rows, `[[`, character(1), "status")
  empty_sets <- sum(vapply(sets, function(set) length(unique(set$members)) == 0L, logical(1)))
  ineligible_sets <- sum(!vapply(sets, `[[`, logical(1), "eligible"))
  list(family_size = length(sets), full_family_size = as.integer(length(sets)),
       estimable_count = as.integer(length(estimable)),
       non_estimable_count = as.integer(sum(statuses == "non_estimable")),
       unavailable_count = as.integer(sum(statuses == "unavailable")),
       empty_set_count = as.integer(empty_sets),
       ineligible_set_count = as.integer(ineligible_sets),
       tests_in_bh = length(estimable),
       nominated_outside_universe = length(setdiff(nominations, ids)),
       mapping_rule = mapping$rule, mapping_status = mapping$status, sets = rows)
}
