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
  endpoint_unit_signature <- function(endpoint) {
    lapply(endpoint$samples, function(sample) {
      if (!identical(sample$sample_id, sample$unit_id)) {
        triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "B-compatible score sample ids must remain the declared analysis unit ids.", "unit_id")
      }
      sample[c(
        "sample_id", "unit_id", "unit_type", "source_sample_ids", "subject_id",
        "representation_id", "visit_weights", "eligibility", "study_id",
        "analysis_id", "preparation_context_id"
      )]
    })
  }
  reference_unit_signature <- NULL
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
    signature <- endpoint_unit_signature(endpoint)
    if (is.null(reference_unit_signature)) reference_unit_signature <- signature else if (
        !identical(reference_unit_signature, signature)) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Every component endpoint must retain the same AnalysisUnitMap lineage.", "unit_map")
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
  source_ids <- unlist(lapply(endpoint_samples, function(sample) sample$source_sample_ids), use.names = FALSE)
  if (!length(source_ids) || anyNA(source_ids) || any(!nzchar(source_ids)) || anyDuplicated(source_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Analysis units must map to unique source samples before clinical linking.", "source_sample_ids")
  }
  links <- clinical_view$links
  link_ids <- vapply(links, function(link) {
    if (is.null(link$link_id)) "" else as.character(link$link_id)
  }, character(1))
  link_samples <- vapply(links, function(link) {
    if (is.null(link$sample_id)) "" else as.character(link$sample_id)
  }, character(1))
  if (any(!nzchar(link_ids)) || anyDuplicated(link_ids) || any(!nzchar(link_samples)) || anyDuplicated(link_samples) ||
      !setequal(link_samples, source_ids) || length(link_samples) != length(source_ids)) {
    triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Approved clinical links must cover every AnalysisUnitMap source sample exactly once.", "clinical_links")
  }
  index <- stats::setNames(links, link_samples)
  linked <- lapply(source_ids, function(sample_id) {
    link <- index[[sample_id]]
    subject <- if (is.null(link$subject_key)) link$subject_id else link$subject_key
    if (is.null(link) || !identical(link$status, "approved") ||
        !identical(link$cardinality, "one_to_one") ||
        !is.character(subject) || length(subject) != 1L || is.na(subject) || !nzchar(subject)) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "An AnalysisUnitMap source sample lacks one approved clinical subject link.", "clinical_links")
    }
    list(
      link_id = as.character(link$link_id),
      sample_id = sample_id,
      subject_id = as.character(subject),
      visit_id = if (is.null(link$visit_id)) NULL else as.character(link$visit_id),
      reference_time = if (is.null(link$reference_time)) NULL else as.character(link$reference_time),
      status = link$status,
      cardinality = link$cardinality
    )
  })
  names(linked) <- source_ids
  by_unit <- lapply(endpoint_samples, function(sample) {
    unit_links <- linked[sample$source_sample_ids]
    subjects <- vapply(unit_links, `[[`, character(1), "subject_id")
    if (length(unique(subjects)) != 1L) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Derived unit source samples do not belong to one clinical subject.", "clinical_links")
    }
    if (!is.null(sample$subject_id) && any(subjects != sample$subject_id)) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Clinical subject links disagree with the AnalysisUnitMap subject_id.", "subject_id")
    }
    unit_links
  })
  names(by_unit) <- vapply(endpoint_samples, `[[`, character(1), "unit_id")
  for (sample in endpoint_samples) {
    if (!sample$unit_type %in% c("subject_mean", "subject_delta")) next
    unit_links <- by_unit[[sample$unit_id]]
    visits <- unname(vapply(unit_links, function(link) {
      if (is.null(link$visit_id)) "" else as.character(link$visit_id)
    }, character(1)))
    weights <- sample$visit_weights
    if (any(!nzchar(visits)) || anyDuplicated(visits) ||
        any(!visits %in% names(weights)) ||
        !identical(visits, names(weights)[names(weights) %in% visits])) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Derived AnalysisUnitMap visit weights do not match their linked source-sample visit lineage.", "visit_weights")
    }
    if (identical(sample$unit_type, "subject_mean") &&
        any(!names(weights)[weights > 0] %in% visits)) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Every positive subject-mean visit weight needs a linked source sample.", "visit_weights")
    }
    if (identical(sample$unit_type, "subject_delta") &&
        (!identical(visits, names(weights)) ||
         !identical(as.numeric(weights), c(-1, 1)))) {
      triomics_c08_abort("C_SCORE_COVERAGE_MISMATCH", "Subject-delta links must preserve the declared reference-to-target ordering and -1/+1 weights.", "visit_weights")
    }
  }
  list(
    links = linked,
    by_unit = by_unit,
    links_sha256 = triomics_c08_sha256(linked)
  )
}

triomics_c08_parse_date <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) return(NULL)
  parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), value)) NULL else parsed
}

triomics_c08_validate_objective_time <- function(objective, endpoint, links_by_unit) {
  mode <- objective$mode
  valid_modes <- c("between_person", "within_person", "baseline_to_later", "effect_modification", "prediction")
  if (!is.character(mode) || length(mode) != 1L || !mode %in% valid_modes) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Clinical score objective has an unsupported time mode.", "mode")
  }
  samples <- endpoint$samples
  unit_types <- vapply(samples, `[[`, character(1), "unit_type")
  if (any(unit_types == "subject_mean")) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "A multi-visit subject mean cannot be assigned to a single clinical objective time window.", "representation_id")
  }
  if (any(unit_types == "subject_delta")) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "A subject-delta value is one derived contrast, not two independent visit-level scores accepted by the B longitudinal consumer.", "unit_type")
  }
  get_link_fields <- function(sample) {
    links <- links_by_unit[[sample$unit_id]]
    if (is.null(links) || !length(links) || any(vapply(links, function(link) {
      is.null(link$visit_id) || length(link$visit_id) != 1L || is.na(link$visit_id) ||
        !nzchar(link$visit_id) || is.null(link$reference_time) ||
        length(link$reference_time) != 1L || is.na(link$reference_time) ||
        !nzchar(link$reference_time)
    }, logical(1)))) {
      triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Every linked source sample needs an explicit visit and collection time.", "clinical_links")
    }
    list(
      visits = vapply(links, `[[`, character(1), "visit_id"),
      times = vapply(links, `[[`, character(1), "reference_time"),
      subjects = vapply(links, `[[`, character(1), "subject_id")
    )
  }
  if (identical(mode, "between_person") || identical(mode, "effect_modification") || identical(mode, "prediction")) {
    visit <- objective$time_window_ref
    if (!is.character(visit) || length(visit) != 1L || is.na(visit) || !nzchar(visit) ||
        any(vapply(samples, function(sample) {
          fields <- get_link_fields(sample)
          length(fields$visits) != 1L || !identical(fields$visits[[1L]], visit) ||
            is.null(triomics_c08_parse_date(fields$times[[1L]]))
        }, logical(1)))) {
      triomics_c08_abort("C_SCORE_TIME_MISMATCH", "All score units must match the objective's declared visit window and have a valid ISO collection date.", "time_window_ref")
    }
    return(invisible(TRUE))
  }
  baseline <- objective$baseline_visit_ref
  later <- objective$later_visit_ref
  horizon <- objective$horizon_days
  if (!is.character(baseline) || length(baseline) != 1L || is.na(baseline) || !nzchar(baseline) ||
      !is.character(later) || length(later) != 1L || is.na(later) || !nzchar(later) ||
      identical(baseline, later) || !is.numeric(horizon) || length(horizon) != 1L ||
      is.na(horizon) || !is.finite(horizon) || horizon <= 0 || horizon != as.integer(horizon)) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Repeated-visit objectives need explicit baseline/later visits and a positive integer horizon.", "time_window_ref")
  }
  unit_fields <- lapply(samples, get_link_fields)
  visits <- vapply(unit_fields, function(fields) fields$visits[[1L]], character(1))
  subjects <- vapply(unit_fields, function(fields) fields$subjects[[1L]], character(1))
  times <- vapply(unit_fields, function(fields) fields$times[[1L]], character(1))
  parsed_times <- lapply(times, triomics_c08_parse_date)
  if (any(vapply(parsed_times, is.null, logical(1))) || any(!visits %in% c(baseline, later))) {
    triomics_c08_abort("C_SCORE_TIME_MISMATCH", "Repeated-visit score units need valid linked baseline/later visits and dates.", "time")
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

triomics_c08_b_compatible_endpoint_bundle <- function(endpoint_bundle, linkage) {
  bundle <- endpoint_bundle
  bundle$source_payload_sha256 <- endpoint_bundle$payload_sha256
  bundle$clinical_linkage_sha256 <- linkage$links_sha256
  bundle$endpoints <- lapply(endpoint_bundle$endpoints, function(endpoint) {
    b_endpoint <- endpoint
    b_endpoint$source_endpoint_sha256 <- endpoint$endpoint_sha256
    b_endpoint$clinical_linkage_sha256 <- linkage$links_sha256
    b_endpoint$samples <- lapply(endpoint$samples, function(sample) {
      links <- linkage$by_unit[[sample$unit_id]]
      if (length(links) != 1L || !identical(sample$unit_type, "sample") &&
          !identical(sample$unit_type, "visit_level")) {
        triomics_c08_abort("C_SCORE_TIME_MISMATCH", "B-compatible clinical endpoints require one independently observed visit-level score per analysis unit.", "unit_type")
      }
      link <- links[[1L]]
      if (is.null(link$visit_id) || !nzchar(link$visit_id) ||
          is.null(triomics_c08_parse_date(link$reference_time))) {
        triomics_c08_abort("C_SCORE_TIME_MISMATCH", "B-compatible score samples need a verified visit id and ISO collection date from the approved link.", "clinical_links")
      }
      sample$subject_key <- link$subject_id
      sample$visit_id <- link$visit_id
      sample$time <- link$reference_time
      sample$source_link_id <- link$link_id
      sample$source_link_sha256 <- triomics_c08_sha256(link)
      sample
    })
    b_endpoint$endpoint_sha256 <- NULL
    b_endpoint$endpoint_sha256 <- triomics_c08_endpoint_hash(b_endpoint)
    b_endpoint
  })
  bundle$payload_sha256 <- NULL
  bundle$payload_sha256 <- triomics_c08_bundle_hash(bundle)
  class(bundle) <- class(endpoint_bundle)
  bundle
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
    triomics_c08_validate_objective_time(objective, endpoints[[endpoint_id]], linkage$by_unit)
  }
  b_endpoint_bundle <- triomics_c08_b_compatible_endpoint_bundle(endpoint_bundle, linkage)
  request <- list(
    artifact_type = "clinical_score_association_request",
    request_type = "clinical_score_association",
    owner_stage = "B",
    status = "scheduled",
    model_execution = "not_started",
    recurses_to_stage_c = FALSE,
    source_endpoint_bundle_sha256 = endpoint_bundle$payload_sha256,
    endpoint_bundle_sha256 = b_endpoint_bundle$payload_sha256,
    endpoint_bundle = b_endpoint_bundle,
    objective_spec = objective_spec,
    clinical_linkage = list(
      study_id = clinical_view$study_id,
      approved_links = linkage$links,
      unit_links = linkage$by_unit,
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
