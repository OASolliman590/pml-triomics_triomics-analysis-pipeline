# Natural-spline and linear time basis for the fixed-subject trajectory.
# Knots and boundaries are on the recorded time clock; the origin is subtracted
# before splines::ns. Subject columns are added by the repeated design.

triomics_b04_as_numbers <- function(value, key) {
  if (triomics_b_empty(value)) return(numeric())
  numbers <- suppressWarnings(as.numeric(unlist(value)))
  if (length(numbers) == 0L || any(!is.finite(numbers))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Spline knots and boundaries must be finite numbers.", key = key)
  }
  numbers
}

triomics_b04_linear_pattern <- function(subjects, times) {
  sets <- lapply(split(times, subjects), function(value) sort(unique(as.numeric(value))))
  same <- length(sets) > 0L && all(vapply(sets, function(value) identical(value, sets[[1L]]), logical(1)))
  if (same && length(sets[[1L]]) >= 3L) "balanced_linear_complete" else "irregular_linear_complete"
}

triomics_b04_time_basis <- function(design, metadata, sample_ids) {
  if (!identical(design$condition_type, "continuous") || !identical(design$condition, design$time_column)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A trajectory condition is the declared time column.", key = "design.condition")
  }
  if (!triomics_b_string(design$time_unit)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A trajectory needs an explicit time unit.", key = "design.time_unit")
  }
  if (!triomics_b_string(design$time_origin)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A trajectory needs an explicit numeric time origin.", key = "design.time_origin")
  }
  origin <- suppressWarnings(as.numeric(design$time_origin))
  if (length(origin) != 1L || !is.finite(origin)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "The time origin must be one finite number on the time clock.", key = "design.time_origin")
  }
  if (triomics_b_string(design$reference_level) || triomics_b_string(design$target_level)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A level contrast is not a trajectory slope or spline contrast.", key = "design.reference_level")
  }
  knots <- triomics_b04_as_numbers(design$spline_knots, "design.spline_knots")
  boundary <- triomics_b04_as_numbers(design$spline_boundary, "design.spline_boundary")
  if (length(knots) == 0L && length(boundary) == 0L) {
    basis_name <- "linear"
  } else if (length(knots) >= 1L && length(boundary) == 2L) {
    basis_name <- "natural_spline"
  } else {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "A natural spline needs interior knots and both boundaries; neither is imputed.",
      key = "design.spline_knots"
    )
  }
  if (identical(basis_name, "natural_spline")) {
    if (boundary[[1L]] >= boundary[[2L]] || any(knots <= boundary[[1L]] | knots >= boundary[[2L]]) ||
        anyDuplicated(knots) || is.unsorted(knots, strictly = TRUE)) {
      triomics_b_abort(
        "B_DESIGN_UNSUPPORTED",
        "Spline knots must be unique, increasing, and strictly inside the two boundaries.",
        key = "design.spline_knots"
      )
    }
  }
  observed <- triomics_b02_align(metadata, sample_ids, design$time_column, "design.time_column")
  if (is.factor(observed) || is.character(observed) || !is.numeric(observed)) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Time is not coerced from labels.", key = "design.time_column")
  }
  observed <- as.numeric(observed)
  if (any(!is.finite(observed))) {
    triomics_b_abort("B_REPEATED_MODEL_UNSUPPORTED", "A missing time is not imputed.", key = "design.time_column")
  }
  subjects <- as.character(triomics_b02_align(metadata, sample_ids, design$subject_id, "design.subject_id"))
  if (any(!nzchar(subjects) | is.na(subjects))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Every repeated row needs a subject id.", key = "design.subject_id")
  }
  if (anyDuplicated(paste(subjects, observed, sep = "\r"))) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "A duplicate subject time is not averaged.", key = "design.time_column")
  }
  counts <- tapply(observed, subjects, length)
  if (any(counts < 2L)) {
    triomics_b_abort(
      "B_REPEATED_MODEL_UNSUPPORTED",
      "A subject with fewer than two times has no within-person trajectory.",
      key = "design.time_column"
    )
  }
  if (all(counts == 2L) && length(unique(observed)) == 2L) {
    triomics_b_abort("B_DESIGN_UNSUPPORTED", "Exactly two common times stay on the paired delta lane.", key = "design.time_column")
  }
  model_time <- observed - origin
  if (identical(basis_name, "linear")) {
    extra_ids <- "time.linear"
    extra <- list(time.linear = model_time)
    pattern <- triomics_b04_linear_pattern(subjects, observed)
    formula <- "~0 + subject + time.linear"
  } else {
    model_knots <- knots - origin
    model_boundary <- boundary - origin
    if (any(model_time < model_boundary[[1L]] - 1e-8 | model_time > model_boundary[[2L]] + 1e-8)) {
      triomics_b_abort(
        "B_REPEATED_MODEL_UNSUPPORTED",
        "An observed time is outside the declared spline boundaries.",
        key = "design.spline_boundary"
      )
    }
    basis <- splines::ns(model_time, knots = model_knots, Boundary.knots = model_boundary)
    extra_ids <- paste0("time.ns", seq_len(ncol(basis)))
    extra <- lapply(seq_len(ncol(basis)), function(index) as.numeric(basis[, index]))
    names(extra) <- extra_ids
    pattern <- "natural_spline_complete"
    formula <- "~0 + subject + time.ns"
  }
  list(
    extra_ids = extra_ids,
    extra = extra,
    pattern = pattern,
    basis = basis_name,
    subjects = subjects,
    time_unit = design$time_unit,
    time_origin = design$time_origin,
    time_origin_value = origin,
    spline_knots = knots,
    spline_boundary = boundary,
    formula = formula,
    basis_hash = triomics_b_hash(list(
      basis = basis_name,
      origin = origin,
      unit = design$time_unit,
      knots = knots,
      boundary = boundary,
      columns = extra
    ))
  )
}
