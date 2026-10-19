triomics_c02_split_abort <- function(message, key = "split_plan") {
  triomics_abort(
    "C02_SPLIT_PROVENANCE", paste0("C02_SPLIT_PROVENANCE: ", message), key = key,
    module = "splits", stage = "C",
    remediation = "Pass a validated SplitPlan and a fold id that names one outer or inner fold with nonempty, subject-disjoint sides."
  )
}

triomics_c02_find_fold <- function(split_plan, fold_id) {
  for (outer in split_plan$folds) {
    if (!is.list(outer)) next
    if (identical(outer$fold_id, fold_id)) return(outer)
    for (inner in outer$inner) {
      if (is.list(inner) && identical(inner$fold_id, fold_id)) return(inner)
    }
  }
  NULL
}

triomics_c02_subjects_from_split <- function(split_plan, fold_id) {
  if (!is.character(fold_id) || length(fold_id) != 1L || is.na(fold_id) || !nzchar(fold_id)) {
    triomics_c02_split_abort("A fold id must be one nonempty string.", key = "fold_id")
  }
  if (!is.list(split_plan) || !is.list(split_plan$folds)) {
    triomics_c02_split_abort("A SplitPlan needs a folds list.")
  }
  fold <- triomics_c02_find_fold(split_plan, fold_id)
  if (is.null(fold)) {
    triomics_c02_split_abort(paste0("Fold ", fold_id, " is missing from the SplitPlan."), key = "fold_id")
  }
  if (!length(fold$training_unit_ids) || !length(fold$test_unit_ids)) {
    triomics_c02_split_abort(paste0("Fold ", fold_id, " has an empty training or test side."), key = "fold_id")
  }
  triomics_validate_split_plan(split_plan, TRUE)
  map <- split_plan$unit_subject
  subjects_of <- function(units) {
    unique(as.character(map$subject_id[match(as.character(units), as.character(map$unit_id))]))
  }
  sides <- list(train = subjects_of(fold$training_unit_ids), test = subjects_of(fold$test_unit_ids))
  if (!length(sides$train) || !length(sides$test)) {
    triomics_c02_split_abort(paste0("Fold ", fold_id, " has no subjects on one side."), key = "fold_id")
  }
  if (length(intersect(sides$train, sides$test))) {
    triomics_c02_split_abort(paste0("Fold ", fold_id, " shares a subject between training and test."), key = "fold_id")
  }
  sides
}
