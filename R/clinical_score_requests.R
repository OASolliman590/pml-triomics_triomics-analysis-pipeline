triomics_c08_validate_endpoint_bundle <- function(endpoint_bundle) {
  if (!inherits(endpoint_bundle, "IntegrationScoreEndpointBundle") ||
      !identical(endpoint_bundle$artifact_type, "integration_score_endpoint_bundle") ||
      !is.list(endpoint_bundle$endpoints) || !length(endpoint_bundle$endpoints) ||
      !triomics_c08_valid_sha256(endpoint_bundle$payload_sha256) ||
      !identical(endpoint_bundle$payload_sha256, triomics_c08_bundle_hash(endpoint_bundle))) {
    triomics_c08_abort("C_SCORE_ENDPOINT_HASH", "The score endpoint bundle is not a valid immutable C artifact.", "endpoint_bundle")
  }
  endpoint_ids <- vapply(endpoint_bundle$endpoints, function(endpoint) endpoint$endpoint_id, character(1))
  if (anyNA(endpoint_ids) || any(!nzchar(endpoint_ids)) || anyDuplicated(endpoint_ids)) {
    triomics_c08_abort("C_SCORE_REQUEST_INVALID", "Score endpoint ids must be unique and complete.", "endpoint_id")
  }
  for (endpoint in endpoint_bundle$endpoints) {
    if (!identical(endpoint$artifact_type, "integration_score_endpoint") ||
        !triomics_c08_valid_sha256(endpoint$endpoint_sha256) ||
        !identical(endpoint$endpoint_sha256, triomics_c08_endpoint_hash(endpoint))) {
      triomics_c08_abort("C_SCORE_ENDPOINT_HASH", "A score endpoint has changed since C-08 construction.", "endpoint_sha256")
    }
    if (!triomics_c08_valid_sha256(endpoint$matrix_hash) ||
        !identical(endpoint$matrix_hash, triomics_c08_matrix_hash(endpoint))) {
      triomics_c08_abort("C_SCORE_ENDPOINT_HASH", "A score endpoint matrix has changed since C-08 construction.", "matrix_hash")
    }
  }
  invisible(endpoint_ids)
}

triomics_c08_endpoint_index <- function(endpoint_bundle) {
  stats::setNames(endpoint_bundle$endpoints,
                  vapply(endpoint_bundle$endpoints, function(endpoint) endpoint$endpoint_id, character(1)))
}

triomics_c08_validate_clinical_links <- function(endpoint_bundle, clinical_view) {
  if (!is.list(clinical_view) || !identical(endpoint_bundle$study_id, clinical_view$study_id) ||
      !is.list(clinical_view$links)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Clinical view and score bundle must identify one study and approved links.", "clinical_view")
  }
  endpoint_samples <- endpoint_bundle$endpoints[[1L]]$samples
  sample_ids <- vapply(endpoint_samples, function(sample) sample$sample_id, character(1))
  if (anyDuplicated(sample_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Score endpoint sample coverage is not one-to-one.", "sample_id")
  }
  links <- clinical_view$links
  link_ids <- vapply(links, function(link) {
    if (is.null(link$link_id)) "" else as.character(link$link_id)
  }, character(1))
  link_samples <- vapply(links, function(link) {
    if (is.null(link$sample_id)) "" else as.character(link$sample_id)
  }, character(1))
  if (any(!nzchar(link_ids)) || anyDuplicated(link_ids) || any(!nzchar(link_samples)) || anyDuplicated(link_samples) ||
      !setequal(link_samples, sample_ids) || length(link_samples) != length(sample_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Approved clinical links must cover every score sample exactly once.", "clinical_links")
  }
  index <- stats::setNames(links, link_samples)
  linked <- lapply(endpoint_samples, function(sample) {
    link <- index[[sample$sample_id]]
    if (is.null(link) || !identical(link$status, "approved") ||
        !identical(link$cardinality, "one_to_one") ||
        !identical(link$subject_key, sample$subject_key)) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "A score sample lacks one approved clinical subject link.", "clinical_links")
    }
    if (!identical(link$visit_id, sample$visit_id) ||
        is.null(link$reference_time) || !identical(as.character(link$reference_time), sample$time)) {
      triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Clinical link visit or reference time differs from the score unit map.", "clinical_links")
    }
    list(
      link_id = if (is.null(link$link_id)) "" else as.character(link$link_id),
      sample_id = sample$sample_id,
      subject_key = sample$subject_key,
      visit_id = sample$visit_id,
      reference_time = sample$time,
      status = link$status,
      cardinality = link$cardinality
    )
  })
  list(
    links = linked,
    links_sha256 = triomics_c08_sha256(linked)
  )
}

triomics_c08_parse_date <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) return(NULL)
  parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), value)) NULL else parsed
}

triomics_c08_validate_objective_time <- function(objective, endpoint) {
  mode <- objective$mode
  valid_modes <- c("between_person", "within_person", "baseline_to_later", "effect_modification", "prediction")
  if (!is.character(mode) || length(mode) != 1L || !mode %in% valid_modes) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Clinical score objective has an unsupported time mode.", "mode")
  }
  samples <- endpoint$samples
  visits <- vapply(samples, function(sample) sample$visit_id, character(1))
  subjects <- vapply(samples, function(sample) sample$subject_key, character(1))
  times <- vapply(samples, function(sample) sample$time, character(1))
  parsed_times <- lapply(times, triomics_c08_parse_date)
  if (any(vapply(parsed_times, is.null, logical(1)))) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Every score sample needs a valid ISO collection date.", "time")
  }
  if (identical(mode, "between_person") || identical(mode, "effect_modification") || identical(mode, "prediction")) {
    visit <- objective$time_window_ref
    if (!is.character(visit) || length(visit) != 1L || is.na(visit) || !nzchar(visit) ||
        any(visits != visit)) {
      triomics_c08_abort("C_SCORE_TIME_MISMATCH", "All score units must match the objective's declared visit window.", "time_window_ref")
    }
    return(invisible(TRUE))
  }
  baseline <- objective$baseline_visit_ref
  later <- objective$later_visit_ref
  horizon <- objective$horizon_days
  if (!is.character(baseline) || length(baseline) != 1L || is.na(baseline) || !nzchar(baseline) ||
      !is.character(later) || length(later) != 1L || is.na(later) || !nzchar(later) ||
      identical(baseline, later) || !is.numeric(horizon) || length(horizon) != 1L ||
      is.na(horizon) || !is.finite(horizon) || horizon <= 0 || horizon != as.integer(horizon) ||
      any(!visits %in% c(baseline, later))) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Repeated-visit objectives need explicit baseline/later visits and a positive integer horizon.", "time_window_ref")
  }
  pairs <- split(seq_along(samples), subjects)
  for (indices in pairs) {
    baseline_index <- indices[visits[indices] == baseline]
    later_index <- indices[visits[indices] == later]
    if (length(baseline_index) != 1L || length(later_index) != 1L) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Each repeated-measure subject needs one baseline and one later score unit.", "unit_map")
    }
    observed <- as.integer(parsed_times[[later_index]] - parsed_times[[baseline_index]])
    if (observed <= 0L || observed != as.integer(horizon)) {
      triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Observed score-unit horizon differs from the declared objective horizon.", "horizon_days")
    }
  }
  invisible(TRUE)
}

triomics_schedule_clinical_score_request <- function(endpoint_bundle, objective_spec, clinical_view) {
  if (!inherits(endpoint_bundle, "IntegrationScoreEndpointBundle") ||
      !identical(endpoint_bundle$artifact_type, "integration_score_endpoint_bundle")) {
    triomics_c08_abort("C_SCORE_REQUEST_INVALID", "Feature-association artifacts are not clinical score-association requests.", "endpoint_bundle")
  }
  triomics_c08_validate_endpoint_bundle(endpoint_bundle)
  if (!is.list(objective_spec) || !is.list(objective_spec$objectives) ||
      !length(objective_spec$objectives)) {
    triomics_c08_abort("C_SCORE_REQUEST_INVALID", "A score association request needs explicit clinical objectives.", "objective_spec")
  }
  endpoints <- triomics_c08_endpoint_index(endpoint_bundle)
  linkage <- triomics_c08_validate_clinical_links(endpoint_bundle, clinical_view)
  for (objective in objective_spec$objectives) {
    endpoint_id <- objective$endpoint_ref
    if (is.null(endpoint_id) || !is.character(endpoint_id) || length(endpoint_id) != 1L ||
        !endpoint_id %in% names(endpoints) ||
        !identical(objective$response_ref, endpoint_id) ||
        !identical(objective$analysis_id, endpoint_bundle$analysis_id) ||
        !is.character(objective$objective_id) || length(objective$objective_id) != 1L ||
        is.na(objective$objective_id) || !nzchar(objective$objective_id)) {
      triomics_c08_abort("C_SCORE_REQUEST_INVALID", "Score-association objectives must target a C-08 score endpoint, not a feature artifact.", "endpoint_ref")
    }
    triomics_c08_validate_objective_time(objective, endpoints[[endpoint_id]])
  }
  request <- list(
    artifact_type = "clinical_score_association_request",
    request_type = "clinical_score_association",
    owner_stage = "B",
    status = "scheduled",
    model_execution = "not_started",
    recurses_to_stage_c = FALSE,
    endpoint_bundle_sha256 = endpoint_bundle$payload_sha256,
    endpoint_bundle = endpoint_bundle,
    objective_spec = objective_spec,
    clinical_linkage = list(
      study_id = clinical_view$study_id,
      approved_links = linkage$links,
      links_sha256 = linkage$links_sha256
    ),
    results = list()
  )
  class(request) <- c("ClinicalScoreAssociationRequest", "list")
  body <- request
  body$request_sha256 <- NULL
  request$request_sha256 <- triomics_c08_sha256(body)
  request
}
