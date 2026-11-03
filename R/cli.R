triomics_usage <- function() {
  paste(
    "Usage: triomics --config FILE --stage A|B|C|D|report|all --output DIR [--validate-only]",
    "       triomics --help",
    "",
    "Validation-only checks the requested implemented stage and writes a safe validation summary.",
    sep = "\n"
  )
}

triomics_parse_cli <- function(args) {
  options <- list(config = NULL, stage = NULL, output = NULL,
                  validate_only = FALSE, help = FALSE)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      options$help <- TRUE
      i <- i + 1L
      next
    }
    if (identical(arg, "--validate-only")) {
      if (isTRUE(options$validate_only)) {
        triomics_abort("A_CLI_ARGUMENT", "Option --validate-only was repeated.", key = arg,
                       module = "cli", remediation = "Provide each CLI option once.")
      }
      options$validate_only <- TRUE
      i <- i + 1L
      next
    }
    if (!arg %in% c("--config", "--stage", "--output")) {
      triomics_abort("A_CLI_ARGUMENT", paste0("Unknown command-line option '", arg, "'."),
                     key = arg, module = "cli", remediation = "Run triomics --help for supported options.")
    }
    if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
      triomics_abort("A_CLI_ARGUMENT", paste0("Option '", arg, "' requires a value."),
                     key = arg, module = "cli", remediation = "Provide a value for each CLI option.")
    }
    name <- switch(arg, `--config` = "config", `--stage` = "stage", `--output` = "output")
    if (!is.null(options[[name]])) {
      triomics_abort("A_CLI_ARGUMENT", paste0("Option '", arg, "' was repeated."),
                     key = arg, module = "cli", remediation = "Provide each CLI option once.")
    }
    options[[name]] <- args[[i + 1L]]
    i <- i + 2L
  }
  options
}

triomics_validate_only <- function(config, config_path, stage, output) {
  triomics_validate_config_paths(config, config_path)
  if (file.exists(output) && !dir.exists(output)) {
    triomics_abort("A_OUTPUT_INVALID", "Output path exists and is not a directory.", key = "output",
                   module = "cli", stage = stage,
                   remediation = "Choose a new local output directory.")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  summary <- list(
    schema_version = "2.0",
    study_id = config$study_id,
    stage = stage,
    status = "complete",
    validation_only = TRUE,
    config_sha256 = attr(config, "config_sha256"),
    implemented_stage = "A"
  )
  triomics_write_json_atomic(summary, file.path(output, "validation-summary.json"))
  invisible(summary)
}

triomics_cli_exit <- function(error) {
  if (identical(error$code, "A_STAGE_UNAVAILABLE")) {
    3L
  } else if (identical(error$code, "A_ARTIFACT_FINALIZE")) {
    4L
  } else {
    2L
  }
}

triomics_cli_prepare_registered_b09 <- function(options, args) {
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) ||
      is.null(options$config)) {
    return(FALSE)
  }
  config <- triomics_read_config(options$config)
  if (!isTRUE(config$clinical$enabled)) return(FALSE)
  if (identical(config$study_id, "triomics_b_t09_fixture")) {
    if (isNamespace(topenv(environment()))) {
      return(isTRUE(triomics_b09_prepare_cli(args)))
    }
    return(FALSE)
  }
  objectives <- config$clinical$objectives
  modes <- if (is.list(objectives) && length(objectives)) {
    vapply(objectives, function(objective) {
      if (is.null(objective$mode)) "" else as.character(objective$mode)
    }, character(1))
  } else character()
  if (any(modes %in% c("within_person", "baseline_to_later", "effect_modification"))) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Longitudinal objective execution requires a registered eligible endpoint bundle and clinical view.",
      key = "clinical.objectives.mode",
      remediation = paste(
        "Keep the declared objective unchanged and provide the accepted endpoint bundle and clinical view;",
        "the CLI must not substitute the public synthetic fixture."
      )
    )
  }
  FALSE
}

triomics_cli_prepare_registered_b08 <- function(options, args) {
  if (!identical(options$stage, "B") || isTRUE(options$validate_only) ||
      is.null(options$config)) {
    return(FALSE)
  }
  config <- triomics_read_config(options$config)
  if (!isTRUE(config$clinical$enabled)) return(FALSE)
  if (identical(config$study_id, "triomics_b_t08_fixture")) {
    return(isTRUE(triomics_b08_prepare_cli(args)))
  }
  if (identical(config$study_id, "triomics_b_t09_fixture")) return(FALSE)
  objectives <- config$clinical$objectives
  if (is.list(objectives) && length(objectives)) {
    triomics_b_abort(
      "B_DESIGN_UNSUPPORTED",
      "Clinical objective execution requires a registered eligible endpoint bundle and clinical view.",
      key = "clinical.objectives",
      remediation = paste(
        "Keep the declared objective unchanged and provide the accepted endpoint bundle and clinical view;",
        "the CLI must not substitute the public B-T08 synthetic fixture."
      )
    )
  }
  FALSE
}

triomics_c01_matrix_payload <- function(value, row_key, column_key) {
  list(
    rows = stats::setNames(list(rownames(value)), row_key),
    columns = stats::setNames(list(colnames(value)), column_key),
    values = unname(lapply(seq_len(nrow(value)), function(i) {
      unname(as.list(as.numeric(value[i, , drop = TRUE])))
    }))
  )
}

triomics_c01_result_payload <- function(result) {
  result <- triomics_validate_integration_result(result)
  loadings <- lapply(names(result$loadings), function(block_id) {
    c(
      list(block_id = block_id),
      triomics_c01_matrix_payload(
        result$loadings[[block_id]], "feature_keys", "component_ids"
      )
    )
  })
  variance <- c(
    triomics_c01_matrix_payload(result$variance, "block_ids", "component_ids"),
    list(
      metric = attr(result$variance, "metric"),
      assay_space = attr(result$variance, "assay_space"),
      denominator = attr(result$variance, "denominator"),
      semantics = attr(result$variance, "semantics")
    )
  )
  list(
    scores = triomics_c01_matrix_payload(result$scores, "unit_ids", "component_ids"),
    loadings = loadings,
    variance = variance,
    selected = result$selected,
    backend = result$backend,
    params = triomics_c02_json_safe(result$params)
  )
}

triomics_c08_cli_links <- function(clinical, source_sample_ids) {
  public <- clinical$public
  source_sample_ids <- as.character(source_sample_ids)
  if (!is.list(public) || !is.list(public$links)) {
    triomics_c08_abort(
      "C_SCORE_COVERAGE_MISMATCH",
      "C-08 clinical scheduling requires the public, linked clinical input view.",
      "clinical_view"
    )
  }
  links <- lapply(source_sample_ids, function(sample_id) {
    matches <- Filter(function(link) identical(link$sample_id, sample_id), public$links)
    if (!length(matches)) {
      triomics_c08_abort(
        "C_SCORE_COVERAGE_MISMATCH",
        "An endpoint source sample has no declared clinical link.",
        "clinical.links_file"
      )
    }
    required <- c("link_id", "subject_key", "visit_id", "reference_time", "status", "cardinality")
    signatures <- lapply(matches, function(link) {
      lapply(required, function(field) {
        value <- link[[field]]
        if (is.null(value)) "" else as.character(value)
      }) |> stats::setNames(required)
    })
    reference <- signatures[[1L]]
    agrees <- vapply(signatures, function(signature) {
      all(vapply(required[-1L], function(field) {
        identical(signature[[field]], reference[[field]])
      }, logical(1L)))
    }, logical(1L))
    if (!all(agrees) || !identical(reference$status, "approved") ||
        !identical(reference$cardinality, "one_to_one") ||
        !nzchar(reference$subject_key) || !nzchar(reference$visit_id) ||
        !nzchar(reference$reference_time) || !nzchar(reference$link_id)) {
      triomics_c08_abort(
        "C_SCORE_COVERAGE_MISMATCH",
        "Per-sample clinical links must agree on one approved subject, visit, time, and cardinality.",
        "clinical.links_file"
      )
    }
    # Preserve the actual link id from the first agreeing source row; do not mint a replacement.
    list(
      link_id = matches[[1L]]$link_id,
      sample_id = sample_id,
      subject_id = reference$subject_key,
      subject_key = reference$subject_key,
      visit_id = reference$visit_id,
      reference_time = reference$reference_time,
      status = reference$status,
      cardinality = reference$cardinality,
      source_sha256 = matches[[1L]]$source_sha256
    )
  })
  names(links) <- source_sample_ids
  links
}

triomics_c08_cli_clinical_view <- function(clinical, source_sample_ids) {
  public <- clinical$public
  links <- triomics_c08_cli_links(clinical, source_sample_ids)
  list(
    study_id = public$study_id,
    dictionary = public$dictionary,
    observations = public$observations,
    links = links
  )
}

triomics_c08_cli_unit_map <- function(resolved, sample_registry, analysis_unit_map,
                                      clinical_view = NULL) {
  declared_map <- if (!is.null(resolved$unit_map)) resolved$unit_map else analysis_unit_map
  representation_id <- resolved$representation_id
  if (is.null(representation_id)) representation_id <- resolved$representation
  records <- triomics_c08_unit_records(declared_map)
  if (is.null(records) || !length(records)) {
    triomics_c08_abort(
      "C_SCORE_LINEAGE_INVALID",
      "C-08 requires the declared AnalysisUnitMap from analysis resolution.",
      "analysis_unit_map"
    )
  }
  registry <- sample_registry
  if (!is.data.frame(registry) || !"sample_id" %in% names(registry)) {
    triomics_c08_abort(
      "C_SCORE_LINEAGE_INVALID", "C-08 requires the predecessor sample registry.", "sample_registry"
    )
  }
  clinical_links <- if (is.null(clinical_view)) list() else clinical_view$links
  lapply(records, function(record) {
    source_ids <- record$source_sample_ids
    if (!is.character(source_ids) || !length(source_ids) || anyNA(source_ids) ||
        any(!nzchar(source_ids))) {
      triomics_c08_abort(
        "C_SCORE_LINEAGE_INVALID", "AnalysisUnitMap source sample ids are incomplete.", "source_sample_ids"
      )
    }
    registry_index <- match(source_ids, as.character(registry$sample_id))
    if (anyNA(registry_index)) {
      triomics_c08_abort(
        "C_SCORE_COVERAGE_MISMATCH", "An AnalysisUnitMap source sample is absent from the predecessor registry.",
        "sample_registry.sample_id"
      )
    }
    raw_subject_ids <- character()
    if ("subject_id" %in% names(registry)) {
      raw_subject_ids <- as.character(registry$subject_id[registry_index])
      raw_subject_ids[is.na(raw_subject_ids)] <- ""
    }
    subject_ids <- raw_subject_ids
    if (length(clinical_links)) {
      mapped <- clinical_links[source_ids]
      if (length(mapped) != length(source_ids) || any(vapply(mapped, is.null, logical(1L)))) {
        triomics_c08_abort(
          "C_SCORE_COVERAGE_MISMATCH", "A declared AnalysisUnitMap source sample is not linked.",
          "clinical.links_file"
        )
      }
      raw_subjects <- unique(raw_subject_ids[nzchar(raw_subject_ids)])
      if (length(raw_subjects) != 1L) {
        if (length(raw_subjects) > 1L) {
          triomics_c08_abort(
            "C_SCORE_COVERAGE_MISMATCH",
            "AnalysisUnitMap source samples disagree on the raw molecular subject identity.",
            "analysis_unit_map.subject_id"
          )
        }
      }
      declared_raw_subject <- triomics_c06_cli_unit_map_scalar(record, "subject_id")
      if (!is.null(declared_raw_subject) && !nzchar(as.character(declared_raw_subject))) {
        declared_raw_subject <- NULL
      }
      if (length(raw_subjects) && !is.null(declared_raw_subject) &&
          !identical(as.character(declared_raw_subject), raw_subjects[[1L]])) {
        triomics_c08_abort(
          "C_SCORE_COVERAGE_MISMATCH",
          "The AnalysisUnitMap raw subject identity disagrees with predecessor source samples.",
          "analysis_unit_map.subject_id"
        )
      }
      clinical_subject_keys <- vapply(mapped, function(link) {
        as.character(link$subject_key %||% "")
      }, character(1L))
      if (any(!nzchar(clinical_subject_keys)) || length(clinical_subject_keys) != length(source_ids)) {
        triomics_c08_abort(
          "C_SCORE_COVERAGE_MISMATCH", "A source sample has an incomplete linked clinical subject key.",
          "clinical.links_file"
        )
      }
      clinical_study_id <- as.character(clinical_view$study_id %||% "")
      if (length(clinical_study_id) != 1L || is.na(clinical_study_id) || !nzchar(clinical_study_id)) {
        triomics_c08_abort(
          "C_SCORE_COVERAGE_MISMATCH", "Clinical subject links lack their declared study identity.",
          "clinical_view.study_id"
        )
      }
      raw_subject <- if (length(raw_subjects)) raw_subjects[[1L]] else declared_raw_subject
      repeated_representation <- representation_id %in% c("delta", "subject_centered")
      if (is.null(raw_subject) && repeated_representation) {
        triomics_c08_abort(
          "C_SCORE_COVERAGE_MISMATCH",
          "Repeated C-08 units require the declared raw molecular subject identity.",
          "analysis_unit_map.subject_id"
        )
      }
      if (!is.null(raw_subject)) {
        expected_subject_key <- triomics_clinical_subject_key(clinical_study_id, raw_subject)
        if (any(clinical_subject_keys != expected_subject_key)) {
          triomics_c08_abort(
            "C_SCORE_COVERAGE_MISMATCH",
            "Clinical subject keys do not match the declared raw molecular subject link.",
            "clinical.links_file.subject_key"
          )
        }
        subject_ids <- expected_subject_key
      } else {
        linked_subject_keys <- unique(clinical_subject_keys)
        if (length(linked_subject_keys) != 1L) {
          triomics_c08_abort(
            "C_SCORE_COVERAGE_MISMATCH",
            "Linked source samples disagree on their approved clinical subject key.",
            "clinical.links_file.subject_key"
          )
        }
        subject_ids <- linked_subject_keys[[1L]]
      }
    }
    subject_ids <- unique(subject_ids[nzchar(subject_ids)])
    if (length(subject_ids) > 1L) {
      triomics_c08_abort(
        "C_SCORE_COVERAGE_MISMATCH",
        "AnalysisUnitMap source samples disagree on the declared subject identity.",
        "analysis_unit_map.subject_id"
      )
    }
    if (!length(clinical_links) && !is.null(record$subject_id) && length(subject_ids) &&
        !identical(as.character(record$subject_id), subject_ids[[1L]])) {
      triomics_c08_abort(
        "C_SCORE_COVERAGE_MISMATCH",
        "The declared AnalysisUnitMap subject identity disagrees with its source samples.",
        "analysis_unit_map.subject_id"
      )
    }
    if (length(clinical_links) && length(subject_ids)) {
      record$subject_id <- subject_ids[[1L]]
    } else if (is.null(record$subject_id) && length(subject_ids)) {
      record$subject_id <- subject_ids[[1L]]
    }
    record$representation_id <- representation_id
    record
  })
}

triomics_c06_cli_unit_map_records <- function(unit_map) {
  if (is.data.frame(unit_map)) {
    return(lapply(seq_len(nrow(unit_map)), function(index) {
      record <- lapply(names(unit_map), function(field) {
        value <- unit_map[[field]]
        if (is.list(value)) value[[index]] else value[[index]]
      })
      names(record) <- names(unit_map)
      record
    }))
  }
  triomics_c08_unit_records(unit_map)
}

triomics_c06_cli_unit_map_scalar <- function(record, field) {
  value <- record[[field]]
  if (is.null(value) || !length(value)) return(NULL)
  if (length(value) != 1L || is.na(value[[1L]])) return(NULL)
  value[[1L]]
}

triomics_c06_cli_compose_unit_map <- function(resolved, ingested, analysis,
                                               block_ids, source_unit_map) {
  representation <- as.character(analysis$design$representation)
  metadata <- ingested$sample_metadata
  sample_ids <- as.character(metadata$sample_id)
  block_samples <- stats::setNames(lapply(block_ids, function(block_id) {
    block <- resolved$blocks[[block_id]]
    ids <- as.character(block$sample_ids)
    if (!is.matrix(block$assay) || !identical(ids, as.character(colnames(block$assay))) ||
        anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_SAMPLE_MAP",
        "A repeated-measure block needs a unique named source-sample map.",
        key = paste0("blocks.", block_id)
      )
    }
    ids
  }), block_ids)
  all_samples <- sort(unique(unlist(block_samples, use.names = FALSE)), method = "radix")
  if (identical(representation, "subject_centered")) {
    if (any(!all_samples %in% sample_ids)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_UNKNOWN_UNIT",
        "Resolved trajectory blocks contain samples absent from the B source map.",
        key = "analysis_unit_map.source_sample_ids"
      )
    }
    records <- triomics_c06_cli_unit_map_records(source_unit_map)
    if (is.null(records) || !length(records)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Trajectory composition requires the exact B AnalysisUnitMap.",
        key = "analysis_unit_map"
      )
    }
    source_ids <- vapply(records, function(record) {
      as.character(triomics_c06_cli_unit_map_scalar(record, "unit_id") %||% "")
    }, character(1L))
    if (any(!nzchar(source_ids)) || anyDuplicated(source_ids) ||
        any(!all_samples %in% source_ids) || !setequal(source_ids, sample_ids)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Trajectory B unit identities must preserve every named source sample, including exclusions.",
        key = "analysis_unit_map.unit_id"
      )
    }
    source_representations <- vapply(records, function(record) {
      as.character(triomics_c06_cli_unit_map_scalar(record, "representation_id") %||% "")
    }, character(1L))
    if (any(source_representations != "subject_centered")) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Trajectory C composition requires the declared subject-centered B representation.",
        key = "analysis_unit_map.representation_id"
      )
    }
    source_by_id <- stats::setNames(records, source_ids)
    declared_blocks <- stats::setNames(vector("list", length(source_ids)), source_ids)
    source_eligibility <- stats::setNames(logical(length(source_ids)), source_ids)
    source_exclusion <- stats::setNames(character(length(source_ids)), source_ids)
    for (unit_id in source_ids) {
      source_record <- source_by_id[[unit_id]]
      source_members <- as.character(unlist(source_record$source_sample_ids, use.names = FALSE))
      if (!identical(source_members, unit_id) ||
          !"represented_blocks" %in% names(source_record)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B units must retain one named source sample and its represented-block membership.",
          key = paste0("analysis_unit_map.", unit_id, ".source_sample_ids")
        )
      }
      mapped_blocks <- as.character(unlist(source_record$represented_blocks, use.names = FALSE))
      if (anyNA(mapped_blocks) || any(!nzchar(mapped_blocks)) || anyDuplicated(mapped_blocks) ||
          any(!mapped_blocks %in% block_ids)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B represented-block membership contains invalid block identities.",
          key = paste0("analysis_unit_map.", unit_id, ".represented_blocks")
        )
      }
      actual_blocks <- names(block_samples)[vapply(block_samples, function(ids) unit_id %in% ids, logical(1L))]
      if (!setequal(mapped_blocks, actual_blocks)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B represented-block membership disagrees with the named resolved matrices.",
          key = paste0("analysis_unit_map.", unit_id, ".represented_blocks")
        )
      }
      eligible <- triomics_c06_cli_unit_map_scalar(source_record, "eligible")
      disposition <- triomics_c06_cli_unit_map_scalar(source_record, "eligibility")
      if (is.null(eligible) && is.null(disposition)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B units need an explicit eligibility disposition.",
          key = paste0("analysis_unit_map.", unit_id, ".eligible")
        )
      }
      if (!is.null(eligible) && !is.logical(eligible)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B eligibility must be a single logical value.",
          key = paste0("analysis_unit_map.", unit_id, ".eligible")
        )
      }
      if (!is.null(disposition) && !as.character(disposition) %in% c("retained", "excluded")) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B eligibility disposition must be retained or excluded.",
          key = paste0("analysis_unit_map.", unit_id, ".eligibility")
        )
      }
      is_eligible <- if (!is.null(eligible)) eligible else identical(as.character(disposition), "retained")
      if (!is.null(disposition) && !identical(as.character(disposition),
          if (is_eligible) "retained" else "excluded")) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B eligibility fields disagree.",
          key = paste0("analysis_unit_map.", unit_id, ".eligibility")
        )
      }
      exclusion_reason <- as.character(triomics_c06_cli_unit_map_scalar(source_record, "exclusion_reason") %||% "")
      if (!length(mapped_blocks) &&
          (!identical(is_eligible, FALSE) || !identical(exclusion_reason, "absent_from_all_analysis_blocks"))) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "A B unit absent from every resolved block must retain its explicit absent-from-all exclusion.",
          key = paste0("analysis_unit_map.", unit_id, ".exclusion_reason")
        )
      }
      if (length(mapped_blocks) && !is_eligible && !nzchar(exclusion_reason)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "An excluded trajectory B unit must retain its declared exclusion reason.",
          key = paste0("analysis_unit_map.", unit_id, ".exclusion_reason")
        )
      }
      declared_blocks[[unit_id]] <- mapped_blocks
      source_eligibility[[unit_id]] <- is_eligible
      source_exclusion[[unit_id]] <- exclusion_reason
    }
    design <- analysis$design
    subject_column <- as.character(design$subject_id %||% "")
    time_column <- as.character(design$time_column %||% "")
    visit_column <- as.character(design$visit_column %||% "")
    time_unit <- design$time_unit
    time_origin <- design$time_origin
    if (length(subject_column) != 1L || !nzchar(subject_column) ||
        !subject_column %in% names(metadata) || length(time_column) != 1L ||
        !nzchar(time_column) || !time_column %in% names(metadata) ||
        !is.character(time_unit) || length(time_unit) != 1L || is.na(time_unit) ||
        !nzchar(trimws(time_unit)) || length(time_origin) != 1L || is.na(time_origin) ||
        (is.character(time_origin) && !nzchar(trimws(time_origin))) ||
        (is.numeric(time_origin) && !is.finite(time_origin))) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Trajectory composition needs declared subject, finite time, time unit, and time origin identities.",
        key = "analyses.design"
      )
    }
    subject_values <- as.character(metadata[[subject_column]])
    time_values <- suppressWarnings(as.numeric(as.character(metadata[[time_column]])))
    unit_ids <- sort(source_ids, method = "radix")
    used_rows <- match(unit_ids, sample_ids)
    required_time <- lengths(declared_blocks[unit_ids]) > 0L
    source_times <- unname(time_values[used_rows])
    unavailable_absent_all_time <- !required_time & is.na(source_times) & !is.nan(source_times)
    valid_source_time <- is.finite(source_times) | unavailable_absent_all_time
    if (anyNA(subject_values[used_rows]) || any(!nzchar(trimws(subject_values[used_rows]))) ||
        any(!valid_source_time)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Trajectory source metadata needs subject identities and finite time for every represented unit.",
        key = "analysis_unit_map.subject_id"
      )
    }
    visit_declared <- length(visit_column) == 1L && nzchar(visit_column)
    if (visit_declared && !visit_column %in% names(metadata)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "The declared trajectory visit column is absent from source metadata.",
        key = "analyses.design.visit_column"
      )
    }
    source_rows <- match(unit_ids, sample_ids)
    output <- lapply(seq_along(unit_ids), function(index) {
      sample_id <- unit_ids[[index]]
      source_record <- source_by_id[[sample_id]]
      source_members <- as.character(unlist(source_record$source_sample_ids, use.names = FALSE))
      if (!identical(source_members, sample_id)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B unit membership must map each sample unit to its exact source sample.",
          key = "analysis_unit_map.source_sample_ids"
        )
      }
      subject <- as.character(subject_values[[source_rows[[index]]]])
      mapped_subject <- triomics_c06_cli_unit_map_scalar(source_record, "subject_id")
      if (is.null(mapped_subject) || !identical(as.character(mapped_subject), subject)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B subject identity disagrees with the declared source metadata.",
          key = "analysis_unit_map.subject_id"
        )
      }
      time <- unname(time_values[[source_rows[[index]]]])
      represented <- declared_blocks[[sample_id]]
      absent_all_missing_time <- !length(represented) && !source_eligibility[[sample_id]] &&
        identical(source_exclusion[[sample_id]], "absent_from_all_analysis_blocks") &&
        is.na(time) && !is.nan(time)
      mapped_time <- triomics_c06_cli_unit_map_scalar(source_record, "time")
      if (is.null(mapped_time)) mapped_time <- triomics_c06_cli_unit_map_scalar(source_record, "condition")
      if (!is.null(mapped_time)) {
        mapped_numeric <- suppressWarnings(as.numeric(as.character(mapped_time)))
        mapped_missing_time <- length(mapped_numeric) == 1L && is.na(mapped_numeric) &&
          !is.nan(mapped_numeric)
        mapped_time_matches <- absent_all_missing_time && mapped_missing_time
        if (!mapped_time_matches &&
            (length(mapped_numeric) != 1L || is.na(mapped_numeric) ||
             !is.finite(mapped_numeric) || !identical(as.numeric(mapped_numeric), time))) {
          triomics_integration_abort(
            "C_SCORE_LINEAGE_INVALID",
            "Trajectory B time identity disagrees with the declared source metadata.",
            key = "analysis_unit_map.time"
          )
        }
      }
      if (is.character(design$condition) && length(design$condition) == 1L &&
          design$condition %in% names(metadata)) {
        source_condition <- triomics_c06_cli_unit_map_scalar(source_record, "condition")
        declared_condition <- as.character(metadata[[design$condition]][[source_rows[[index]]]])
        if (!is.null(source_condition) && !identical(as.character(source_condition), declared_condition)) {
          triomics_integration_abort(
            "C_SCORE_LINEAGE_INVALID",
            "Trajectory B condition identity disagrees with declared source metadata.",
            key = paste0("analysis_unit_map.", sample_id, ".condition")
          )
        }
      }
      mapped_time_unit <- triomics_c06_cli_unit_map_scalar(source_record, "time_unit")
      mapped_time_origin <- triomics_c06_cli_unit_map_scalar(source_record, "time_origin")
      if ((!is.null(mapped_time_unit) && !identical(as.character(mapped_time_unit), as.character(time_unit))) ||
          (!is.null(mapped_time_origin) &&
           !identical(as.character(mapped_time_origin), as.character(time_origin)))) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Trajectory B time unit or origin disagrees with the declared analysis identity.",
          key = "analysis_unit_map.time_origin"
        )
      }
      visit_id <- triomics_c06_cli_unit_map_scalar(source_record, "visit_id")
      if (visit_declared) {
        declared_visit <- as.character(metadata[[visit_column]][[source_rows[[index]]]])
        if (is.na(declared_visit) || !nzchar(trimws(declared_visit)) ||
            (!is.null(visit_id) && !identical(as.character(visit_id), declared_visit))) {
          triomics_integration_abort(
            "C_SCORE_LINEAGE_INVALID",
            "Trajectory B visit identity disagrees with the declared visit column.",
            key = "analysis_unit_map.visit_id"
          )
        }
        visit_id <- declared_visit
      }
      eligible <- all(block_ids %in% represented) && source_eligibility[[sample_id]]
      missing_blocks <- setdiff(block_ids, represented)
      exclusion_reason <- if (eligible) NULL else if (!length(represented)) {
        source_exclusion[[sample_id]]
      } else if (!source_eligibility[[sample_id]]) {
        source_exclusion[[sample_id]]
      } else if (length(missing_blocks)) {
        paste0("absent_from_required_blocks:", paste(missing_blocks, collapse = ","))
      } else {
        source_exclusion[[sample_id]]
      }
      record <- list(
        unit_id = sample_id,
        unit_type = if (visit_declared) "visit_level" else "sample",
        representation_id = "subject_centered",
        source_sample_ids = source_members,
        subject_id = subject,
        visit_id = visit_id,
        condition = if (is.character(design$condition) && length(design$condition) == 1L &&
                        design$condition %in% names(metadata)) {
          as.character(metadata[[design$condition]][[source_rows[[index]]]])
        } else {
          as.character(triomics_c06_cli_unit_map_scalar(source_record, "condition") %||% "")
        },
        time = if (absent_all_missing_time) NULL else time,
        time_unit = as.character(time_unit),
        time_origin = time_origin,
        eligibility = if (eligible) "retained" else "excluded",
        exclusion_reason = exclusion_reason,
        represented_blocks = represented,
        study_id = as.character(ingested$study_id),
        analysis_id = as.character(analysis$analysis_id),
        preparation_context_id = as.character(analysis$preparation_context_id)
      )
      record
    })
    names(output) <- unit_ids
    retained <- vapply(output, function(record) identical(record$eligibility, "retained"), logical(1L))
    return(list(
      unit_map = output,
      unit_ids = unit_ids,
      training_unit_ids = unit_ids[retained],
      target_unit_ids = unit_ids,
      block_ids = block_ids
    ))
  }

  if (!identical(representation, "delta")) return(NULL)
  top_map_records <- triomics_c06_cli_unit_map_records(source_unit_map)
  if (is.null(top_map_records) || !length(top_map_records)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "Paired C composition requires the persisted B AnalysisUnitMap, including excluded subjects.",
      key = "analysis_unit_map"
    )
  }
  top_map_ids <- vapply(top_map_records, function(record) {
    as.character(triomics_c06_cli_unit_map_scalar(record, "unit_id") %||% "")
  }, character(1L))
  if (any(!nzchar(top_map_ids)) || anyDuplicated(top_map_ids)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "The persisted paired B AnalysisUnitMap has invalid subject identities.",
      key = "analysis_unit_map.unit_id"
    )
  }
  expected_paired_identity <- list(
    unit_type = "subject_delta",
    representation_id = "delta",
    study_id = as.character(ingested$study_id),
    analysis_id = as.character(analysis$analysis_id),
    preparation_context_id = as.character(analysis$preparation_context_id)
  )
  validate_paired_identity <- function(records, map_label) {
    for (record_index in seq_along(records)) {
      record <- records[[record_index]]
      unit_id <- as.character(triomics_c06_cli_unit_map_scalar(record, "unit_id") %||% "")
      for (field in names(expected_paired_identity)) {
        observed <- triomics_c06_cli_unit_map_scalar(record, field)
        expected <- expected_paired_identity[[field]]
        if (is.null(observed) || length(expected) != 1L || is.na(expected) || !nzchar(expected) ||
            !identical(as.character(observed), expected)) {
          triomics_integration_abort(
            "C_SCORE_LINEAGE_INVALID",
            paste0("Paired ", map_label, " unit ", unit_id, " has an absent or foreign ", field, "."),
            key = paste0(map_label, ".", unit_id, ".", field)
          )
        }
      }
    }
    invisible(records)
  }
  validate_paired_identity(top_map_records, "analysis_unit_map")
  block_records <- stats::setNames(lapply(block_ids, function(block_id) {
    block <- resolved$blocks[[block_id]]
    if (!identical(block$decomposition, "target_minus_reference_once") ||
        !identical(as.integer(block$decomposition_count), 1L) ||
        !identical(block$centered, FALSE) ||
        !identical(block$signed_delta_map, block$assay)) {
      triomics_integration_abort(
        "C_ELIGIBILITY_REPRESENTATION_MISMATCH",
        "Paired C composition requires the exact once-computed target-minus-reference B matrix.",
        key = paste0("blocks.", block_id)
      )
    }
    records <- triomics_c06_cli_unit_map_records(block$unit_map)
    if (is.null(records) || !length(records)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Every paired block needs its own B unit map.",
        key = paste0("blocks.", block_id, ".unit_map")
      )
    }
    ids <- vapply(records, function(record) {
      as.character(triomics_c06_cli_unit_map_scalar(record, "unit_id") %||% "")
    }, character(1L))
    if (any(!nzchar(ids)) || anyDuplicated(ids) ||
        !setequal(ids, top_map_ids) || any(!as.character(block$sample_ids) %in% ids)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Each paired block map must account for every fitted subject by name, including exclusions.",
        key = paste0("blocks.", block_id, ".unit_map")
      )
    }
    validate_paired_identity(records, paste0("blocks.", block_id, ".unit_map"))
    stats::setNames(records, ids)
  }), block_ids)
  canonical_record <- function(record) {
    source_ids <- sort(as.character(unlist(record$source_sample_ids, use.names = FALSE)), method = "radix")
    weights <- record$visit_weights
    if (is.list(weights)) weights <- unlist(weights, use.names = TRUE)
    if (is.numeric(weights) && !is.null(names(weights))) {
      weights <- weights[sort(names(weights), method = "radix")]
    }
    list(
      unit_id = triomics_c06_cli_unit_map_scalar(record, "unit_id"),
      unit_type = triomics_c06_cli_unit_map_scalar(record, "unit_type"),
      representation_id = triomics_c06_cli_unit_map_scalar(record, "representation_id"),
      eligibility = triomics_c06_cli_unit_map_scalar(record, "eligibility"),
      exclusion_reason = triomics_c06_cli_unit_map_scalar(record, "exclusion_reason"),
      source_sample_ids = source_ids,
      subject_id = triomics_c06_cli_unit_map_scalar(record, "subject_id"),
      visit_weights = weights,
      study_id = triomics_c06_cli_unit_map_scalar(record, "study_id"),
      analysis_id = triomics_c06_cli_unit_map_scalar(record, "analysis_id"),
      preparation_context_id = triomics_c06_cli_unit_map_scalar(record, "preparation_context_id")
    )
  }
  first_block_map <- block_records[[1L]]
  top_by_id <- stats::setNames(top_map_records, top_map_ids)
  if (!all(vapply(top_map_ids, function(unit_id) {
    identical(canonical_record(top_by_id[[unit_id]]), canonical_record(first_block_map[[unit_id]]))
  }, logical(1L)))) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "The persisted paired B unit map differs from the first block's exact named map.",
      key = "analysis_unit_map"
    )
  }
  unit_ids <- sort(top_map_ids, method = "radix")
  design <- analysis$design
  subject_column <- as.character(design$subject_id %||% "")
  visit_column <- as.character(design$visit_column %||% "")
  if (length(subject_column) != 1L || !nzchar(subject_column) ||
      !subject_column %in% names(metadata) || length(visit_column) != 1L ||
      !nzchar(visit_column) || !visit_column %in% names(metadata)) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "Paired C composition requires declared subject and visit columns.",
      key = "analyses.design"
    )
  }
  metadata_subject <- as.character(metadata[[subject_column]])
  metadata_visit <- as.character(metadata[[visit_column]])
  sample_row <- stats::setNames(seq_along(sample_ids), sample_ids)
  reference_visit <- as.character(resolved$model_plan$reference_visit)
  target_visit <- as.character(resolved$model_plan$target_visit)
  if (length(reference_visit) != 1L || length(target_visit) != 1L ||
      !nzchar(reference_visit) || !nzchar(target_visit) || reference_visit == target_visit) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Paired B resolution lacks distinct named reference and target visits.",
      key = "model_plan.visit_levels"
    )
  }
  output <- lapply(unit_ids, function(unit_id) {
    per_block <- lapply(block_records, `[[`, unit_id)
    if (any(vapply(per_block, is.null, logical(1L)))) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Paired block maps are not consistent by named subject identity.",
        key = paste0("analysis_unit_map.", unit_id)
      )
    }
    subjects <- vapply(per_block, function(record) {
      as.character(triomics_c06_cli_unit_map_scalar(record, "subject_id") %||% "")
    }, character(1L))
    if (any(!nzchar(subjects)) || any(subjects != unit_id)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Paired unit and subject identities must agree across blocks by name.",
        key = paste0("analysis_unit_map.", unit_id, ".subject_id")
      )
    }
    weights <- lapply(per_block, function(record) record$visit_weights)
    valid_weights <- vapply(weights, function(value) {
      is.numeric(value) && length(value) == 2L && !is.null(names(value)) &&
        !anyDuplicated(names(value)) && setequal(names(value), c(reference_visit, target_visit)) &&
        identical(unname(value[reference_visit]), -1) && identical(unname(value[target_visit]), 1)
    }, logical(1L))
    if (!all(valid_weights)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "Paired blocks disagree on named reference and target signs.",
        key = paste0("analysis_unit_map.", unit_id, ".visit_weights")
      )
    }
    eligibility <- vapply(per_block, function(record) {
      value <- triomics_c06_cli_unit_map_scalar(record, "eligibility")
      if (is.null(value)) "retained" else as.character(value)
    }, character(1L))
    if (any(!eligibility %in% c("retained", "excluded"))) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Paired block eligibility must be retained or excluded.",
        key = paste0("analysis_unit_map.", unit_id, ".eligibility")
      )
    }
    source_by_block <- lapply(seq_along(per_block), function(block_index) {
      record <- per_block[[block_index]]
      ids <- as.character(unlist(record$source_sample_ids, use.names = FALSE))
      if (!length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
          any(!ids %in% sample_ids)) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID", "Paired block source memberships must use known named samples.",
          key = paste0("analysis_unit_map.", unit_id, ".source_sample_ids")
        )
      }
      rows <- unname(sample_row[ids])
      visits <- metadata_visit[rows]
      if (any(metadata_subject[rows] != unit_id) ||
          any(!visits %in% c(reference_visit, target_visit)) || anyDuplicated(visits) ||
          (eligibility[[block_index]] == "retained" &&
           !setequal(visits, c(reference_visit, target_visit)))) {
        triomics_integration_abort(
          "C_SCORE_LINEAGE_INVALID",
          "Paired source samples must resolve to their declared subject and reference/target visits.",
          key = paste0("analysis_unit_map.", unit_id, ".source_sample_ids")
        )
      }
      stats::setNames(ids, visits)
    })
    for (left in seq_along(source_by_block)) {
      for (right in seq_len(left - 1L)) {
        shared_visits <- intersect(names(source_by_block[[left]]), names(source_by_block[[right]]))
        if (length(shared_visits) && any(source_by_block[[left]][shared_visits] !=
                                         source_by_block[[right]][shared_visits])) {
          triomics_integration_abort(
            "C_SCORE_LINEAGE_INVALID",
            "Paired blocks disagree on the named source sample for a subject visit.",
            key = paste0("analysis_unit_map.", unit_id, ".source_sample_ids")
          )
        }
      }
    }
    retained <- all(eligibility == "retained")
    excluded_blocks <- names(eligibility)[eligibility != "retained"]
    exclusion_reasons <- vapply(seq_along(per_block), function(index) {
      value <- triomics_c06_cli_unit_map_scalar(per_block[[index]], "exclusion_reason")
      as.character(value %||% "")
    }, character(1L))
    list(
      unit_id = unit_id,
      unit_type = "subject_delta",
      representation_id = "delta",
      eligibility = if (retained) "retained" else "excluded",
      exclusion_reason = if (retained) NULL else paste0(
        "ineligible_in_blocks:", paste(excluded_blocks, collapse = ","),
        if (any(nzchar(exclusion_reasons))) paste0("; ", paste(unique(exclusion_reasons[nzchar(exclusion_reasons)]), collapse = "; ")) else ""
      ),
      source_sample_ids = unique(unname(unlist(source_by_block, use.names = FALSE))),
      subject_id = unit_id,
      visit_weights = stats::setNames(c(-1, 1), c(reference_visit, target_visit)),
      block_eligibility = stats::setNames(as.list(eligibility), names(block_records)),
      block_source_sample_ids = stats::setNames(source_by_block, names(block_records)),
      study_id = as.character(ingested$study_id),
      analysis_id = as.character(analysis$analysis_id),
      preparation_context_id = as.character(analysis$preparation_context_id)
    )
  })
  names(output) <- unit_ids
  retained <- vapply(output, function(record) identical(record$eligibility, "retained"), logical(1L))
  list(
    unit_map = output,
    unit_ids = unit_ids,
    training_unit_ids = unit_ids[retained],
    target_unit_ids = unit_ids,
    block_ids = block_ids
  )
}

triomics_c08_cli_selection <- function(request, ingested, block_ids) {
  block_ids <- as.character(unlist(block_ids, use.names = FALSE))
  if (!length(block_ids) || anyNA(block_ids) || any(!nzchar(block_ids))) {
    triomics_c08_abort(
      "C_SCORE_LINEAGE_INVALID", "C-08 requires the fitted analysis block ids for source selection.",
      "block_ids"
    )
  }
  selection <- ingested$selection
  source_selection <- lapply(block_ids, function(block_id) {
    if (!is.data.frame(selection) || !all(c("block_id", "selection_history", "inference_gate") %in% names(selection))) {
      triomics_c08_abort(
        "C_SCORE_LINEAGE_INVALID", "C-08 requires source selection ancestry from ingest.", "selection"
      )
    }
    rows <- selection[selection$block_id == block_id, , drop = FALSE]
    if (nrow(rows) != 1L) {
      triomics_c08_abort(
        "C_SCORE_LINEAGE_INVALID", "Source selection ancestry must resolve once per block.", "selection"
      )
    }
    as.list(rows[1L, , drop = FALSE])
  })
  names(source_selection) <- block_ids
  histories <- vapply(source_selection, function(source) as.character(source$selection_history), character(1L))
  gates <- vapply(source_selection, function(source) as.character(source$inference_gate), character(1L))
  if (any(histories == "unknown")) {
    triomics_c08_abort(
      "C_SCORE_LINEAGE_INVALID",
      "Unknown source selection ancestry cannot produce a qualified C-08 handoff.",
      "selection"
    )
  }
  ancestry <- if (any(histories == "outcome_selected")) {
    "selected_after_clinical_outcome"
  } else {
    "prespecified"
  }
  list(
    ancestry = ancestry,
    inspected_outcome = NULL,
    validity = if (any(gates == "withheld")) {
      "withheld"
    } else if (any(gates == "panel_only")) {
      "panel_only"
    } else {
      "qualified"
    },
    source_selection = source_selection,
    adapter_selection = request$selection
  )
}

triomics_c08_cli_objective_spec <- function(config, analysis) {
  objectives <- config$clinical$objectives
  if (!is.list(objectives) || !length(objectives)) {
    triomics_c08_abort(
      "C_SCORE_REQUEST_INVALID", "Clinical score scheduling needs explicitly declared objectives.",
      "clinical.objectives"
    )
  }
  family_refs <- unique(vapply(objectives, function(objective) {
    if (is.null(objective$family_ref)) "" else as.character(objective$family_ref)
  }, character(1L)))
  if (length(family_refs) != 1L || !nzchar(family_refs[[1L]])) {
    triomics_c08_abort(
      "C_SCORE_REQUEST_INVALID", "One scheduled request must carry one declared objective family.",
      "clinical.objectives.family_ref"
    )
  }
  list(
    objectives = objectives,
    family = list(
      family_id = family_refs[[1L]],
      member_ids = unique(vapply(objectives, function(objective) as.character(objective$endpoint_ref), character(1L))),
      claim_scope = "declared_clinical_score_associations"
    ),
    trend = isTRUE(analysis$statistics$trend),
    robust = isTRUE(analysis$statistics$robust),
    fallback = if (is.null(analysis$statistics$fallback)) "none" else analysis$statistics$fallback,
    imputation = "none"
  )
}

triomics_c08_cli_write_artifact <- function(payload, path, envelope_path, artifact_type,
                                            artifact_id, config, analysis, inputs,
                                            status, privacy_class) {
  triomics_write_json_atomic(payload, path)
  envelope <- triomics_artifact_envelope(
    artifact_type = artifact_type,
    artifact_id = artifact_id,
    study_id = config$study_id,
    run_id = "c-08",
    status = status,
    config_sha256 = attr(config, "config_sha256"),
    payload = path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = character(),
    privacy_class = privacy_class
  )
  envelope$analysis_id <- analysis$analysis_id
  envelope$preparation_context_id <- analysis$preparation_context_id
  envelope$representation_id <- analysis$design$representation
  triomics_write_envelope(envelope, envelope_path)
  list(
    path = path,
    envelope_path = envelope_path,
    payload_sha256 = triomics_sha256(path),
    envelope_sha256 = triomics_sha256(envelope_path)
  )
}

triomics_c08_cli_endpoint_payload <- function(endpoint_bundle) {
  triomics_c08_validate_endpoint_bundle(endpoint_bundle)
  payload <- unclass(endpoint_bundle)
  payload$schema_version <- "2.0"
  payload$endpoints <- lapply(payload$endpoints, function(endpoint) {
    endpoint$samples <- lapply(endpoint$samples, function(sample) {
      weights <- sample$visit_weights
      if (is.numeric(weights) && !is.null(names(weights))) {
        sample$visit_weights <- stats::setNames(as.list(as.numeric(weights)), names(weights))
      }
      sample
    })
    if (is.data.frame(endpoint$unit_map) && "visit_weights" %in% names(endpoint$unit_map)) {
      endpoint$unit_map$visit_weights <- I(lapply(endpoint$unit_map$visit_weights, function(weights) {
        if (is.numeric(weights) && !is.null(names(weights))) {
          return(stats::setNames(as.list(as.numeric(weights)), names(weights)))
        }
        weights
      }))
    }
    endpoint$endpoint_sha256 <- NULL
    endpoint$endpoint_sha256 <- triomics_c08_endpoint_hash(endpoint)
    endpoint
  })
  if (length(payload$endpoints)) {
    payload$unit_map_hash <- triomics_c08_sha256(payload$endpoints[[1L]]$unit_map)
  }
  payload$payload_sha256 <- NULL
  payload$payload_sha256 <- triomics_c08_bundle_hash(payload)
  class(payload) <- class(endpoint_bundle)
  triomics_c08_validate_endpoint_bundle(payload)
  payload
}

triomics_c08_cli_validate_endpoint_readback <- function(path) {
  endpoint_bundle <- jsonlite::read_json(path, simplifyVector = FALSE)
  class(endpoint_bundle) <- c("IntegrationScoreEndpointBundle", "list")
  triomics_c08_validate_endpoint_bundle(endpoint_bundle)
  endpoint_bundle
}

triomics_c02_block_operations <- function(block) {
  preprocessing <- block$preprocessing
  filter_method <- if (!is.null(preprocessing$filter$prevalence) ||
                       !is.null(preprocessing$filter$min_observed)) "prevalence" else "none"
  derive <- if (length(preprocessing$derive)) preprocessing$derive else "none"
  profile <- block$profile
  if (identical(profile, "proteomics")) {
    return(triomics_proteomics_operations(
      filter_method = filter_method,
      transform = preprocessing$transform$method,
      normalise = preprocessing$normalisation$method,
      impute = preprocessing$imputation$method,
      min_observed = preprocessing$filter$min_observed,
      prevalence = preprocessing$filter$prevalence,
      zero_policy = preprocessing$transform$zero_policy,
      zero_value = preprocessing$transform$zero_value,
      reference = preprocessing$normalisation$reference,
      scope = preprocessing$filter$scope,
      stratum = preprocessing$filter$stratum,
      selection_justification = preprocessing$filter$selection_justification,
      derive = derive,
      tail_probability = preprocessing$imputation$tail_probability,
      scale_multiplier = preprocessing$imputation$scale_multiplier
    ))
  }
  if (identical(profile, "metabolomics")) {
    return(triomics_metabolomics_operations(
      filter_method = filter_method,
      transform = preprocessing$transform$method,
      normalise = preprocessing$normalisation$method,
      impute = preprocessing$imputation$method,
      min_observed = preprocessing$filter$min_observed,
      prevalence = preprocessing$filter$prevalence,
      zero_policy = preprocessing$transform$zero_policy,
      zero_value = preprocessing$transform$zero_value,
      reference = preprocessing$normalisation$reference,
      reference_file = preprocessing$normalisation$reference_file,
      scope = preprocessing$filter$scope,
      stratum = preprocessing$filter$stratum,
      selection_justification = preprocessing$filter$selection_justification,
      k = preprocessing$imputation$k,
      trees = preprocessing$imputation$trees,
      derive = derive
    ))
  }
  if (identical(profile, "lipidomics")) {
    return(triomics_lipidomics_operations(
      filter_method = filter_method,
      transform = preprocessing$transform$method,
      normalise = preprocessing$normalisation$method,
      impute = preprocessing$imputation$method,
      min_observed = preprocessing$filter$min_observed,
      prevalence = preprocessing$filter$prevalence,
      zero_policy = preprocessing$transform$zero_policy,
      zero_value = preprocessing$transform$zero_value,
      reference_set = preprocessing$transform$reference_set,
      log_base = preprocessing$transform$log_base,
      scope = preprocessing$filter$scope,
      metadata_column = preprocessing$normalisation$metadata_column,
      standards_map = preprocessing$normalisation$standards_map,
      derive = derive
    ))
  }
  triomics_integration_abort(
    "C_EVAL_PREPARATION_INVALID",
    paste0("Block profile '", profile, "' has no registered fold-preparation route."),
    key = block$block_id
  )
}

triomics_c02_json_safe <- function(value) {
  if (inherits(value, "table")) {
    matrix_value <- unclass(value)
    return(list(
      rows = rownames(matrix_value),
      columns = colnames(matrix_value),
      values = unname(lapply(seq_len(nrow(matrix_value)), function(i) {
        unname(as.list(as.integer(matrix_value[i, , drop = TRUE])))
      }))
    ))
  }
  if (is.list(value) && !is.data.frame(value)) {
    return(lapply(value, triomics_c02_json_safe))
  }
  value
}

triomics_c02_evaluation_payload <- function(result) {
  fold_summaries <- lapply(result$outer_folds, function(fold) {
    fold[setdiff(names(fold), c("backend_fit", "common_result", "training_transforms"))]
  })
  payload <- list(
    mode = result$mode,
    method = result$method,
    native_version = result$native_version,
    split_plan_hash = result$split_plan_hash,
    outer_folds = fold_summaries,
    predictions = result$predictions,
    metrics = triomics_c02_json_safe(result$metrics),
    baseline_comparison = triomics_c02_json_safe(result$baseline_comparison),
    uncertainty = result$uncertainty,
    failures = result$failures,
    requested_folds = result$requested_folds,
    n_eligible = result$n_eligible,
    n_predicted = result$n_predicted,
    failed_prediction_count = result$failed_prediction_count,
    n_ineligible = result$n_ineligible,
    total_denominator = result$total_denominator,
    inspected_fit_count = result$inspected_fit_count,
    max_fits = result$max_fits
  )
  triomics_c02_json_safe(payload)
}

triomics_c02_evaluate_request <- function(config, ingested, analysis, request,
                                           block_ids, unit_ids, output) {
  block_config <- stats::setNames(config$blocks, vapply(config$blocks, `[[`, character(1), "block_id"))
  block_specs <- lapply(block_ids, function(block_id) {
    block <- block_config[[block_id]]
    triomics_export_ensure_profile(block$profile)
    list(
      block_id = block_id,
      profile_id = block$profile,
      operations = triomics_c02_block_operations(block)
    )
  })
  descriptive_blocks <- lapply(block_specs, function(block) {
    block$operations <- lapply(block$operations, function(operation) {
      if (identical(operation$stage, "impute")) {
        operation$method <- "none"
        operation$apply <- NULL
      }
      operation
    })
    block
  })
  display_names <- stats::setNames(vapply(block_ids, function(block_id) {
    as.character(block_config[[block_id]]$name)
  }, character(1)), block_ids)
  outcome <- request$outcome
  assembled_input <- ingested
  assembled_input$sample_registry[[outcome]] <- ingested$sample_metadata[[outcome]]
  assembled <- triomics_assemble_study(
    assembled_input, alignment = "strict", display_names = display_names
  )
  evaluation <- request$evaluation
  subject_column <- analysis$design$subject_id
  subject_ids <- if (identical(evaluation$generalization_unit, "subject") &&
                     is.character(subject_column) && length(subject_column) == 1L &&
                     nzchar(subject_column)) {
    as.character(ingested$sample_metadata[[subject_column]])
  } else unit_ids
  stratification <- if (is.character(evaluation$stratification) &&
                        length(evaluation$stratification) == 1L &&
                        nzchar(evaluation$stratification)) {
    as.character(ingested$sample_metadata[[evaluation$stratification]])
  } else NULL
  split_plan <- triomics_nested_split_plan(
    unit_ids,
    subject_ids = subject_ids,
    n_outer = evaluation$outer_folds,
    n_inner = evaluation$inner_folds,
    seed = config$execution$seed,
    stratification = stratification,
    target = outcome
  )
  levels <- ingested$declared_factor_levels[[outcome]]
  outcome_spec <- if (!is.null(levels)) {
    list(column = outcome, type = "categorical", levels = as.character(levels))
  } else list(column = outcome, type = "continuous")
  state_root <- file.path(output, "C", "state", request$integration_id)
  analysis_spec <- list(
    analysis_id = analysis$analysis_id,
    outcome = outcome_spec,
    predictive_spec = list(
      preparation_context_id = paste0(analysis$preparation_context_id, "__predictive"),
      seed = config$execution$seed,
      state_dir = file.path(state_root, "predictive"),
      blocks = block_specs
    ),
    descriptive_spec = list(
      preparation_context_id = paste0(analysis$preparation_context_id, "__descriptive"),
      training_unit_ids = unit_ids,
      seed = config$execution$seed,
      state_dir = file.path(state_root, "descriptive"),
      blocks = descriptive_blocks
    )
  )
  n_components <- as.integer(request$n_components)
  integration_spec <- request
  integration_spec$block_ids <- block_ids
  integration_spec$candidates <- lapply(seq_len(n_components), function(component) {
    candidate <- list(id = paste0("ncomp-", component), ncomp = component)
    if (identical(request$method, "mixomics_diablo")) candidate$n_components <- component
    keep_x <- request$keepX
    if (is.list(keep_x) && length(keep_x) && !is.null(names(keep_x))) {
      sliced <- lapply(keep_x, function(values) {
        values <- suppressWarnings(as.numeric(unlist(values, use.names = FALSE)))
        if (!length(values) || any(!is.finite(values)) || length(values) < component) {
          return(NULL)
        }
        as.integer(values[seq_len(component)])
      })
      if (!any(vapply(sliced, is.null, logical(1L)))) candidate$keepX <- sliced
    }
    candidate
  })
  integration_spec$max_fits <- evaluation$max_fits
  integration_spec$ncomp <- n_components
  integration_spec$task_mode <- outcome_spec$type
  integration_spec$required_views <- block_ids
  if (identical(request$method, "mixomics_diablo")) {
    # The frozen config schema has no NA-capability field. The registered
    # DIABLO route is pinned to complete predictor blocks and still fails
    # closed when a block contains missing values.
    integration_spec$na_capability <- "complete_predictor_blocks"
  }
  triomics_c02_evaluation_payload(
    evaluate_pipeline(assembled, analysis_spec, integration_spec, split_plan)
  )
}

triomics_c06_cli_requested_evaluation_status <- function(evaluation) {
  counts <- c("n_eligible", "n_predicted", "failed_prediction_count",
              "n_ineligible", "total_denominator")
  values <- lapply(counts, function(name) evaluation[[name]])
  names(values) <- counts
  if (any(!vapply(values, function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value)
  }, logical(1L)))) {
    return("failed")
  }
  if (values$n_predicted > 0L &&
      values$n_predicted == values$total_denominator &&
      values$n_eligible == values$total_denominator &&
      values$failed_prediction_count == 0L &&
      values$n_ineligible == 0L) {
    return("complete")
  }
  if (values$n_predicted > 0L) "partial" else "failed"
}

triomics_c06_cli_integration_run <- function(request, fitted) {
  sidecars <- fitted$sidecars
  if (!is.list(sidecars)) sidecars <- list()
  sidecars$native_state <- fitted$backend_fit$native_state
  sidecars$metadata <- fitted$backend_fit$metadata
  list(
    integration_id = request$integration_id,
    method = request$method,
    required = isTRUE(request$required),
    status = "complete",
    result = fitted$result,
    eligibility = fitted$eligibility,
    sidecars = sidecars
  )
}

triomics_c06_cli_ingest_lineage <- function(resolved, ingested, config, block_ids) {
  sources <- stats::setNames(
    ingested$source_manifests,
    vapply(ingested$source_manifests, `[[`, character(1L), "source_id")
  )
  block_config <- stats::setNames(config$blocks, vapply(config$blocks, `[[`, character(1L), "block_id"))
  for (block_id in block_ids) {
    source_ids <- as.character(unlist(block_config[[block_id]]$source_refs, use.names = FALSE))
    if (!length(source_ids) || any(!source_ids %in% names(sources))) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Integration blocks need configured source-role ancestry.",
        key = paste0("blocks.", block_id, ".source_refs")
      )
    }
    selection <- ingested$selection
    if (!is.data.frame(selection) || !all(c("block_id", "selection_history", "inference_gate") %in% names(selection))) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Integration blocks need source-selection ancestry from ingest.",
        key = "selection"
      )
    }
    rows <- selection[selection$block_id == block_id, , drop = FALSE]
    if (nrow(rows) != 1L) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID", "Source-selection ancestry must resolve once per integration block.",
        key = paste0("selection.", block_id)
      )
    }
    resolved$blocks[[block_id]]$source_roles <- stats::setNames(
      lapply(source_ids, function(source_id) sources[[source_id]]$role), source_ids
    )
    resolved$blocks[[block_id]]$source_selection <- as.list(rows[1L, , drop = FALSE])
  }
  resolved
}

triomics_c06_cli_prepare_resolved <- function(resolved, ingested, config, analysis,
                                              block_ids, output) {
  block_config <- stats::setNames(config$blocks, vapply(config$blocks, `[[`, character(1L), "block_id"))
  display_names <- stats::setNames(vapply(block_ids, function(block_id) {
    as.character(block_config[[block_id]]$name)
  }, character(1L)), block_ids)
  assembled <- triomics_assemble_study(
    ingested, alignment = "strict", display_names = display_names
  )
  preparation <- prepare_fit(assembled, list(
    preparation_context_id = analysis$preparation_context_id,
    training_unit_ids = as.character(ingested$sample_metadata$sample_id),
    seed = config$execution$seed,
    state_dir = file.path(output, "C", "preparation-states"),
    blocks = lapply(block_ids, function(block_id) {
      block <- block_config[[block_id]]
      triomics_export_ensure_profile(block$profile)
      list(
        block_id = block_id,
        profile_id = block$profile,
        operations = triomics_c02_block_operations(block)
      )
    })
  ))
  state_refs <- preparation$state_refs
  state_hashes <- stats::setNames(lapply(block_ids, function(block_id) {
    state_refs[[block_id]]$state_hash
  }), block_ids)
  for (block_id in block_ids) {
    assay <- SummarizedExperiment::assay(preparation$experiments[[block_id]], "analysis")
    resolved$blocks[[block_id]]$assay <- matrix(
      as.numeric(assay), nrow = nrow(assay), dimnames = dimnames(assay)
    )
    resolved$blocks[[block_id]]$feature_keys <- rownames(assay)
    resolved$blocks[[block_id]]$sample_ids <- colnames(assay)
  }
  resolved$preparation_state_refs <- state_refs
  resolved$preparation_state_hashes <- state_hashes
  resolved
}

triomics_c06_cli_bind_preparation_lineage <- function(fitted, resolved) {
  hashes <- resolved$preparation_state_hashes
  if (!is.list(hashes) || !identical(sort(names(hashes)), sort(names(resolved$blocks))) ||
      any(!vapply(hashes, triomics_sha256_text, logical(1L)))) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID", "Direct integration fit requires finalized preparation-state hashes.",
      key = "preparation_state_hashes"
    )
  }
  fitted$eligibility$preparation_state_hashes <- hashes
  fitted$result$params$provenance$preparation_state_hashes <- hashes
  fitted$result$params$provenance$preparation_state_refs <- resolved$preparation_state_refs
  fitted$result <- triomics_validate_integration_result(fitted$result)
  fitted
}

triomics_c06_cli_primary_nominations <- function(output, config, analysis) {
  root <- normalizePath(output, winslash = "/", mustWork = FALSE)
  handoff_dir <- triomics_b07_child_path(root, file.path("B", "handoffs"))
  if (!dir.exists(handoff_dir)) return(NULL)
  nomination_paths <- list.files(
    handoff_dir, pattern = "^candidate-nominations[.]json$",
    recursive = TRUE, full.names = TRUE, all.files = FALSE
  )
  if (!length(nomination_paths)) return(NULL)
  applicable <- list()
  for (path in nomination_paths) {
    relative <- substring(normalizePath(path, winslash = "/", mustWork = TRUE), nchar(root) + 2L)
    artifact <- triomics_b07_verified_artifact(
      root, relative, "candidate_nominations",
      config_sha256 = attr(config, "config_sha256"), study_id = config$study_id
    )
    payload <- artifact$payload
    if (is.null(payload$analysis_id) || !triomics_b_string(as.character(payload$analysis_id))) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "A candidate nomination artifact lacks its analysis identity.",
        relative, "Preserve the analysis id from the producing Stage-B handoff."
      )
    }
    if (!identical(as.character(payload$analysis_id), as.character(analysis$analysis_id))) next
    if (!is.list(payload$nominations) || is.null(payload$n_nominations) ||
        !is.numeric(payload$n_nominations) || length(payload$n_nominations) != 1L ||
        is.na(payload$n_nominations) || as.integer(payload$n_nominations) != length(payload$nominations)) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "Candidate nomination count does not match the immutable payload.",
        relative, "Re-run the accepted Stage-B producer; do not repair nomination rows in place."
      )
    }
    block_artifact <- triomics_b07_verified_artifact(
      root, file.path("B", "block-results.json"), "block_results",
      config_sha256 = attr(config, "config_sha256"), study_id = config$study_id,
      optional = TRUE
    )
    if (is.null(block_artifact) ||
        !identical(payload$source_artifact_id, block_artifact$envelope$artifact_id) ||
        !identical(as.character(block_artifact$payload$analysis_id), as.character(analysis$analysis_id))) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "Candidate nominations are not bound to the applicable Stage-B block results.",
        relative, "Preserve the verified block-results source and its artifact reference."
      )
    }
    block_context <- as.character(block_artifact$payload$preparation_context_id %||%
                                    block_artifact$envelope$preparation_context_id)
    block_representation <- as.character(block_artifact$payload$representation_id %||%
                                           block_artifact$envelope$representation_id)
    if (!identical(block_context, as.character(analysis$preparation_context_id)) ||
        !identical(block_representation, as.character(analysis$design$representation))) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "Stage-B nomination source context differs from the configured analysis.",
        relative, "Use immutable Stage-B artifacts produced for the same preparation and representation."
      )
    }
    bound_input <- any(vapply(artifact$envelope$inputs, function(input) {
      identical(input$artifact_id, block_artifact$envelope$artifact_id) &&
        identical(input$sha256, block_artifact$payload_sha256)
    }, logical(1L)))
    if (!bound_input) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "Candidate nominations do not hash-bind their Stage-B block-results source.",
        relative, "Preserve the producer's immutable input references."
      )
    }
    if ((!is.null(payload$preparation_context_id) &&
         !identical(as.character(payload$preparation_context_id), block_context)) ||
        (!is.null(artifact$envelope$preparation_context_id) &&
         !identical(as.character(artifact$envelope$preparation_context_id), block_context)) ||
        (!is.null(payload$representation_id) &&
         !identical(as.character(payload$representation_id), block_representation)) ||
        (!is.null(artifact$envelope$representation_id) &&
         !identical(as.character(artifact$envelope$representation_id), block_representation))) {
      triomics_b07_abort(
        "B_EXPORT_INCOMPLETE", "Candidate nomination context conflicts with its Stage-B source.",
        relative, "Keep preparation and representation identity aligned across Stage-B artifacts."
      )
    }
    applicable[[length(applicable) + 1L]] <- list(
      artifact = artifact,
      context_id = block_context,
      representation_id = block_representation
    )
  }
  if (!length(applicable)) return(NULL)
  if (length(applicable) != 1L) {
    triomics_integration_abort(
      "C_EXPORT_NOMINATION_INVALID", "More than one applicable immutable Stage-B nomination artifact exists.",
      key = "primary_nominations"
    )
  }
  artifact <- applicable[[1L]]$artifact
  list(
    artifact_type = "candidate_nominations",
    study_id = artifact$payload$study_id,
    analysis_id = artifact$payload$analysis_id,
    status = artifact$status,
    source_artifact_id = artifact$envelope$artifact_id,
    source_artifact_ref = list(
      artifact_id = artifact$envelope$artifact_id,
      sha256 = artifact$payload_sha256
    ),
    preparation_context_id = applicable[[1L]]$context_id,
    representation_id = applicable[[1L]]$representation_id,
    nominations = artifact$payload$nominations
  )
}

triomics_c06_cli_write_native_state <- function(integration_run, config, analysis,
                                                output, request_index,
                                                source_inputs, privacy_class) {
  result <- triomics_validate_integration_result(integration_run$result)
  provenance <- result$params$provenance
  variance_definition <- triomics_c06_variance_definition(result)
  native <- triomics_c06_native_payload(integration_run, result, variance_definition)
  native$variance_definition <- triomics_c06_native_variance_definition(result)
  preparation_hashes <- integration_run$eligibility$preparation_state_hashes %||% list()
  source_state_refs <- provenance$preparation_state_refs %||% list()
  if (length(preparation_hashes) &&
      (!identical(sort(names(preparation_hashes)), sort(names(source_state_refs))) ||
       any(!vapply(preparation_hashes, triomics_sha256_text, logical(1L))))) {
    triomics_integration_abort(
      "C_SCORE_LINEAGE_INVALID",
      "Native sidecar preparation hashes and immutable state references must match by block.",
      key = "preparation_state_refs"
    )
  }
  state_directory <- file.path(output, "C", "preparation-states")
  preparation_state_refs <- stats::setNames(lapply(names(preparation_hashes), function(block_id) {
    state_ref <- source_state_refs[[block_id]]
    if (!is.list(state_ref) || !triomics_c06_nonempty_string(state_ref$path) ||
        !identical(state_ref$state_hash, preparation_hashes[[block_id]])) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "A native sidecar cannot bind an absent or mismatched preparation state.",
        key = paste0("preparation_state_refs.", block_id)
      )
    }
    state <- triomics_read_state(state_ref, state_directory)
    if (!identical(state$block_id, block_id)) {
      triomics_integration_abort(
        "C_SCORE_LINEAGE_INVALID",
        "A persisted preparation state does not match its declared block.",
        key = paste0("preparation_state_refs.", block_id)
      )
    }
    state_path <- file.path(state_directory, state_ref$path)
    list(
      state_id = state_ref$state_id,
      path = file.path("C", "preparation-states", basename(state_path)),
      state_hash = state_ref$state_hash,
      payload_sha256 = triomics_sha256(state_path)
    )
  }), names(preparation_hashes))
  artifact_id <- paste0("c01-native-state-", integration_run$integration_id)
  payload <- list(
    schema_version = "2.0",
    artifact_type = "integration_native_state",
    artifact_id = artifact_id,
    study_id = provenance$study_id,
    analysis_id = provenance$analysis_id,
    integration_id = integration_run$integration_id,
    backend = result$backend,
    preparation_context_id = provenance$preparation_context_id,
    representation_id = provenance$representation_id,
    preparation_state_hashes = preparation_hashes,
    preparation_state_refs = preparation_state_refs,
    native = native
  )
  stub <- sprintf("integration-%02d", request_index)
  directory <- file.path(output, "C", "native-states")
  payload_path <- file.path(directory, paste0(stub, ".json"))
  envelope_path <- file.path(directory, paste0(stub, ".envelope.json"))
  triomics_write_json_atomic(payload, payload_path)
  matrix_inputs <- lapply(names(provenance$matrix_sha256), function(block_id) {
    list(
      artifact_id = paste(integration_run$integration_id, block_id, sep = "::"),
      sha256 = provenance$matrix_sha256[[block_id]]
    )
  })
  preparation_inputs <- lapply(names(preparation_hashes), function(block_id) {
    list(
      artifact_id = paste("preparation-state", block_id, sep = "::"),
      sha256 = preparation_state_refs[[block_id]]$payload_sha256
    )
  })
  envelope <- triomics_artifact_envelope(
    artifact_type = "integration_native_state",
    artifact_id = artifact_id,
    study_id = provenance$study_id,
    run_id = "c-01",
    status = integration_run$status,
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = c(source_inputs, matrix_inputs, preparation_inputs),
    reason_codes = character(),
    privacy_class = privacy_class
  )
  envelope$analysis_id <- analysis$analysis_id
  envelope$preparation_context_id <- analysis$preparation_context_id
  envelope$representation_id <- analysis$design$representation
  triomics_write_envelope(envelope, envelope_path)
  list(
    artifact_ref = list(artifact_id = artifact_id, sha256 = triomics_sha256(payload_path)),
    payload_file = file.path("C", "native-states", basename(payload_path)),
    envelope_file = file.path("C", "native-states", basename(envelope_path)),
    payload_sha256 = triomics_sha256(payload_path),
    envelope_sha256 = triomics_sha256(envelope_path)
  )
}

triomics_c06_cli_unrun_controls <- function(integration_runs, resolved, analysis,
                                           integration_results) {
  completed <- Filter(function(run) {
    identical(run$status, "complete") && inherits(run$result, "IntegrationResult")
  }, integration_runs)
  nested <- Filter(function(item) is.list(item$evaluation), integration_results)
  split_plan_sha256 <- NULL
  split_plan_status <- "not_run"
  if (length(completed)) {
    provenance <- completed[[1L]]$result$params$provenance
    sample_ids <- provenance$unit_map %||% completed[[1L]]$result$params$training_unit_ids
    feature_keys <- provenance$feature_map %||% completed[[1L]]$eligibility$block_feature_keys
    preparation_context_id <- provenance$preparation_context_id
    representation_id <- provenance$representation_id
    candidate_method <- "not_selected"
  } else if (length(nested)) {
    evaluation <- nested[[1L]]$evaluation
    predictions <- evaluation$predictions
    sample_ids <- if (is.data.frame(predictions) && "unit_id" %in% names(predictions)) {
      unique(as.character(predictions$unit_id))
    } else if (is.list(predictions) && !is.null(predictions$unit_id)) {
      unique(as.character(unlist(predictions$unit_id, use.names = FALSE)))
    } else {
      character()
    }
    resolved_units <- resolved$unit_ids
    if (is.null(resolved_units) && length(resolved$blocks)) {
      first_block <- resolved$blocks[[1L]]
      resolved_units <- first_block$sample_ids %||% colnames(first_block$assay)
    }
    resolved_units <- unique(as.character(unlist(resolved_units, use.names = FALSE)))
    block_units <- lapply(resolved$blocks, function(block) {
      unique(as.character(unlist(block$sample_ids %||% colnames(block$assay), use.names = FALSE)))
    })
    if (!length(sample_ids) || !length(resolved_units) || anyNA(resolved_units) ||
        any(!nzchar(resolved_units)) || anyDuplicated(resolved_units) ||
        !setequal(sample_ids, resolved_units) || length(sample_ids) != length(resolved_units) ||
        !length(block_units) || any(!vapply(block_units, function(units) {
          length(units) == length(resolved_units) && setequal(units, resolved_units)
        }, logical(1L)))) {
      triomics_integration_abort(
        "C_EVAL_EVIDENCE_INVALID",
        "Nested evaluation units must exactly match the resolved block/unit context.",
        key = "evaluation_evidence.controls.match_context.sample_ids"
      )
    }
    feature_keys <- stats::setNames(lapply(names(resolved$blocks), function(block_id) {
      block <- resolved$blocks[[block_id]]
      if (!is.null(block$feature_keys)) block$feature_keys else rownames(block$assay)
    }), names(resolved$blocks))
    preparation_context_id <- resolved$preparation_context_id %||%
      analysis$preparation_context_id
    representation_id <- resolved$representation_id %||% resolved$representation %||%
      analysis$design$representation
    split_plan_sha256 <- as.character(evaluation$split_plan_hash)
    split_plan_status <- "executed"
    candidate_method <- "not_selected"
  } else {
    sample_ids <- resolved$unit_ids
    feature_keys <- stats::setNames(lapply(names(resolved$blocks), function(block_id) {
      block <- resolved$blocks[[block_id]]
      if (!is.null(block$feature_keys)) block$feature_keys else rownames(block$assay)
    }), names(resolved$blocks))
    preparation_context_id <- resolved$preparation_context_id %||%
      analysis$preparation_context_id
    representation_id <- resolved$representation_id %||% resolved$representation %||%
      analysis$design$representation
    candidate_method <- "not_selected"
  }
  if (is.list(sample_ids)) sample_ids <- unlist(sample_ids, use.names = FALSE)
  sample_ids <- unique(as.character(sample_ids))
  if (!length(sample_ids) || anyNA(sample_ids) || any(!nzchar(sample_ids)) || anyDuplicated(sample_ids)) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID",
      "Unrun C-07 controls need units resolved from integration or evaluation evidence.",
      key = "evaluation_evidence.controls.match_context.sample_ids"
    )
  }
  if (!is.list(feature_keys) || !length(feature_keys) || is.null(names(feature_keys)) ||
      any(!vapply(feature_keys, function(keys) is.character(keys) && length(keys) &&
        !anyNA(keys) && all(nzchar(keys)), logical(1L)))) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID",
      "Unrun C-07 controls need verified feature maps from resolved blocks or integration evidence.",
      key = "evaluation_evidence.controls.match_context.feature_keys"
    )
  }
  context <- list(
    sample_ids = sample_ids,
    feature_keys = feature_keys,
    preparation_context_id = as.character(preparation_context_id),
    representation_id = as.character(representation_id),
    split_plan_status = split_plan_status,
    split_plan_sha256 = split_plan_sha256
  )
  if (identical(split_plan_status, "executed") && !triomics_sha256_text(split_plan_sha256)) {
    triomics_integration_abort(
      "C_EVAL_EVIDENCE_INVALID", "Nested Stage-C evidence must retain its actual immutable split plan hash.",
      key = "evaluation_evidence.controls.match_context.split_plan_sha256"
    )
  }
  reason <- if (length(nested)) {
    paste(
      "C-07 candidate was not selected and matched-baseline controls were not run;",
      "the split-plan hash refers only to the executed nested-CV plan, not to a C-07 run."
    )
  } else {
    paste(
      "C-07 candidate was not selected and matched-baseline controls were not run;",
      "no C-07 split plan was executed or created."
    )
  }
  lapply(triomics_c06_control_names(), function(control_name) {
    unrun <- function(method) list(
      method = method,
      status = "not_run",
      artifact_ref = NULL,
      metrics = NULL,
      reason = paste(control_name, reason),
      match_context = context
    )
    list(
      control = control_name,
      candidate = unrun(candidate_method),
      baseline = unrun("concat_pca")
    )
  })
}

triomics_c06_cli_aggregate_status <- function(handoff) {
  if (identical(handoff$status, "complete") &&
      !identical(handoff$evaluation_evidence$status, "complete")) {
    handoff$status <- "partial"
  }
  handoff
}

triomics_c09_cli_protocol_signature <- function(protocol) {
  canonicalize_mappings <- function(value) {
    if (!is.list(value)) return(value)
    value <- lapply(value, canonicalize_mappings)
    value_names <- names(value)
    if (!is.null(value_names)) {
      value <- value[order(value_names, method = "radix")]
    }
    value
  }

  as.character(jsonlite::toJSON(
    canonicalize_mappings(protocol), auto_unbox = TRUE, null = "null", digits = NA
  ))
}

triomics_c09_cli_validate_predecessors <- function(declared, inputs) {
  split_plan <- inputs$split_plan
  split_contract <- declared$predecessor_contracts$split_plan
  tryCatch(
    triomics_validate_split_plan(split_plan, TRUE),
    error = function(error) triomics_c09_abort(
      "C_C09_SPLIT_INVALID", paste0("The generated C-02 SplitPlan failed validation: ", conditionMessage(error)),
      "split_plan"
    )
  )
  split_matches <- identical(split_plan$schema, split_contract$artifact_type) &&
    identical(split_plan$grouping_unit, split_contract$grouping_unit) &&
    identical(as.integer(split_plan$n_outer), as.integer(split_contract$outer_folds)) &&
    identical(as.integer(split_plan$n_inner), as.integer(split_contract$inner_folds)) &&
    identical(as.integer(split_plan$seed), as.integer(split_contract$seed)) &&
    identical(split_plan$target, split_contract$response_target) &&
    length(split_plan$folds) == split_contract$outer_folds &&
    all(vapply(split_plan$folds, function(fold) {
      length(fold$inner) == split_contract$inner_folds
    }, logical(1L)))
  if (!isTRUE(split_matches)) {
    triomics_c09_abort(
      "C_C09_SPLIT_INVALID",
      "The generated C-02 SplitPlan does not match the declared C-09 predecessor contract.",
      "predecessor_contracts.split_plan"
    )
  }

  endpoint_bundle <- inputs$endpoint_bundle
  request <- inputs$clinical_score_request
  endpoint_contract <- declared$predecessor_contracts$score_endpoint
  tryCatch(
    triomics_c09_validate_c08(endpoint_bundle, request, split_plan),
    error = function(error) triomics_c09_abort(
      "C_C09_ENDPOINT_INVALID", paste0("The generated C-08 handoff failed validation: ", conditionMessage(error)),
      "score_endpoint"
    )
  )
  endpoints <- endpoint_bundle$endpoints
  endpoint <- endpoints[[1L]]
  endpoint_matches <- inherits(endpoint_bundle, endpoint_contract$artifact_type) &&
    identical(endpoint_bundle$artifact_type, "integration_score_endpoint_bundle") &&
    identical(length(endpoints), 1L) &&
    identical(endpoint$endpoint_id, endpoint_contract$fixture_endpoint_id) &&
    identical(endpoint$available_at_time, endpoint_contract$information_time) &&
    identical(request$status, endpoint_contract$clinical_score_request_status) &&
    identical(request$owner_stage, endpoint_contract$clinical_score_request_owner) &&
    identical(request$model_execution, endpoint_contract$clinical_score_request_execution) &&
    identical(request$recurses_to_stage_c,
              endpoint_contract$clinical_score_request_recurses_to_stage_c)
  if (!isTRUE(endpoint_matches)) {
    triomics_c09_abort(
      "C_C09_ENDPOINT_INVALID",
      "The generated C-08 endpoint bundle and scheduled B request do not match the declared predecessor contract.",
      "predecessor_contracts.score_endpoint"
    )
  }

  generated_protocol <- inputs$protocol
  generated_protocol$information_set$c08_endpoint_bundle_sha256 <- NULL
  if (!identical(
    triomics_c09_cli_protocol_signature(generated_protocol),
    triomics_c09_cli_protocol_signature(declared$protocol)
  )) {
    triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID",
      "The generated synthetic protocol does not match the declared C-09 protocol.",
      "protocol"
    )
  }
  tryCatch(
    triomics_c09_validate_protocol(inputs$protocol, split_plan),
    error = function(error) triomics_c09_abort(
      "C_C09_PROTOCOL_INVALID", paste0("The generated C-09 protocol failed validation: ", conditionMessage(error)),
      "protocol"
    )
  )
  invisible(TRUE)
}

triomics_execute_c09_synthetic_stage <- function(config, output) {
  if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
    triomics_c09_abort(
      "A_ARTIFACT_IMMUTABLE",
      "The C-09 synthetic output directory is nonempty; choose a new output directory.",
      "output"
    )
  }

  declared <- config$clinical_prediction
  fixture <- declared$synthetic_fixture
  if (!identical(fixture$generator, "triomics::triomics_c09_synthetic_inputs")) {
    triomics_c09_abort(
      "C_C09_SYNTHETIC_INPUT_INVALID",
      "Stage C accepts only the declared public C-09 synthetic input generator.",
      "synthetic_fixture.generator"
    )
  }
  arguments <- fixture$generator_arguments
  inputs <- triomics_c09_synthetic_inputs(
    seed = arguments$seed,
    n_subjects = arguments$n_subjects,
    endpoint_method = arguments$endpoint_method,
    missing_outcome = arguments$missing_outcome
  )
  if (!inherits(inputs, "C09SyntheticInputs") ||
      !identical(names(inputs$candidates), c("clinical_only", "molecular_only", "combined"))) {
    triomics_c09_abort(
      "C_C09_SYNTHETIC_INPUT_INVALID",
      "The public C-09 generator did not return the declared matched synthetic candidates.",
      "synthetic_fixture"
    )
  }
  triomics_c09_cli_validate_predecessors(declared, inputs)

  result <- run_clinical_prediction_comparison(
    inputs$protocol, inputs$endpoint_bundle, inputs$clinical_score_request,
    inputs$split_plan, inputs$candidates
  )
  payload <- unclass(result)
  endpoint <- inputs$endpoint_bundle$endpoints[[1L]]
  payload$study_id <- config$study_id
  payload$predecessor_identity <- list(
    split_plan = list(
      artifact_type = inputs$split_plan$schema,
      creation_hash = inputs$split_plan$creation_hash,
      target = inputs$split_plan$target,
      grouping_unit = inputs$split_plan$grouping_unit,
      seed = inputs$split_plan$seed,
      outer_folds = inputs$split_plan$n_outer,
      inner_folds = inputs$split_plan$n_inner
    ),
    c08_endpoint_bundle = list(
      artifact_type = inputs$endpoint_bundle$artifact_type,
      study_id = inputs$endpoint_bundle$study_id,
      analysis_id = inputs$endpoint_bundle$analysis_id,
      integration_id = inputs$endpoint_bundle$integration_id,
      payload_sha256 = inputs$endpoint_bundle$payload_sha256
    ),
    c08_endpoint = list(
      artifact_type = endpoint$artifact_type,
      endpoint_id = endpoint$endpoint_id,
      endpoint_sha256 = endpoint$endpoint_sha256,
      information_time = endpoint$available_at_time
    ),
    clinical_score_request = list(
      artifact_type = inputs$clinical_score_request$artifact_type,
      request_type = inputs$clinical_score_request$request_type,
      request_sha256 = inputs$clinical_score_request$request_sha256,
      owner_stage = inputs$clinical_score_request$owner_stage,
      status = inputs$clinical_score_request$status,
      model_execution = inputs$clinical_score_request$model_execution,
      recurses_to_stage_c = inputs$clinical_score_request$recurses_to_stage_c
    )
  )
  payload$synthetic_fixture <- list(
    generator = fixture$generator,
    evidence_scope = fixture$evidence_scope,
    generator_arguments = arguments
  )

  payload_path <- file.path(output, "C", "clinical-prediction-comparison.json")
  envelope_path <- file.path(output, "C", "clinical-prediction-comparison.envelope.json")
  triomics_write_json_atomic(payload, payload_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "clinical_prediction_comparison",
    artifact_id = "stage-c-clinical-prediction-comparison",
    study_id = config$study_id,
    run_id = "c-09-synthetic",
    status = "complete",
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = list(
      list(artifact_id = "C-02::SplitPlan", sha256 = inputs$split_plan$creation_hash),
      list(artifact_id = "C-08::IntegrationScoreEndpointBundle",
           sha256 = inputs$endpoint_bundle$payload_sha256),
      list(artifact_id = "B::ClinicalScoreAssociationRequest",
           sha256 = inputs$clinical_score_request$request_sha256)
    ),
    reason_codes = "SYNTHETIC_FIXTURE_ONLY",
    privacy_class = "synthetic"
  )
  triomics_write_envelope(envelope, envelope_path)

  registry_path <- file.path(output, "registries", "run.json")
  payload_sha256 <- triomics_sha256(payload_path)
  envelope_sha256 <- triomics_sha256(envelope_path)
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "c-09-synthetic",
    study_id = config$study_id,
    stage = "C",
    status = "complete",
    status_scope = "synthetic_fixture_only",
    completion_policy = "conditional",
    clinical_prediction_status = "synthetic_software_complete",
    config_sha256 = attr(config, "config_sha256"),
    artifact_ref = list(
      artifact_id = envelope$artifact_id,
      payload_file = file.path("C", basename(payload_path)),
      payload_sha256 = payload_sha256,
      envelope_file = file.path("C", basename(envelope_path)),
      envelope_sha256 = envelope_sha256
    )
  ), registry_path)

  invisible(list(
    status = "complete",
    run_status = "complete",
    payload_path = payload_path,
    envelope_path = envelope_path,
    required_complete = TRUE
  ))
}

triomics_c06_cli_native_b_predecessors <- function(output) {
  abort <- function(code, message, key = "B/handoffs") {
    triomics_integration_abort(code, message, key = key)
  }
  root <- tryCatch(
    triomics_b07_output_root(output, create = FALSE),
    error = function(error) NULL
  )
  if (is.null(root)) {
    abort(
      "C_EXPORT_PREDECESSOR_MISSING",
      "C-06 publication requires a native B handoff produced in this output root."
    )
  }
  handoff_dir <- tryCatch(
    triomics_b07_child_path(root, file.path("B", "handoffs")),
    error = function(error) NULL
  )
  if (is.null(handoff_dir) || !dir.exists(handoff_dir)) {
    abort(
      "C_EXPORT_PREDECESSOR_MISSING",
      "C-06 publication requires a native B handoff produced in this output root."
    )
  }
  bundle_paths <- list.files(
    handoff_dir, pattern = "^bundle[.]json$", recursive = TRUE,
    full.names = TRUE, all.files = FALSE
  )
  if (!length(bundle_paths)) {
    abort(
      "C_EXPORT_PREDECESSOR_MISSING",
      "C-06 publication requires one complete native B handoff in this output root."
    )
  }
  if (length(bundle_paths) != 1L) {
    abort(
      "C_EXPORT_PREDECESSOR_AMBIGUOUS",
      "C-06 publication found more than one B handoff in this output root."
    )
  }
  bundle_relative <- substring(
    normalizePath(bundle_paths[[1L]], winslash = "/", mustWork = TRUE),
    nchar(root) + 2L
  )
  bundle_path <- tryCatch(
    triomics_b07_child_path(root, bundle_relative),
    error = function(error) NULL
  )
  if (is.null(bundle_path)) {
    abort(
      "C_EXPORT_PREDECESSOR_PATH",
      "The native B handoff path must resolve beneath this output root."
    )
  }
  provenance_path <- file.path(dirname(bundle_path), "stage-handoff-provenance.json")
  list(
    bundle_path = bundle_path,
    bundle_envelope_path = sub("[.]json$", ".envelope.json", bundle_path),
    provenance_path = provenance_path,
    provenance_envelope_path = sub("[.]json$", ".envelope.json", provenance_path)
  )
}

triomics_c06_cli_input_identity <- function(resolved, ingested, analysis,
                                             representation, block_ids,
                                             source_inputs, config,
                                             source_resolved = resolved) {
  block_identity <- function(block_set, hash_matrix) {
    stats::setNames(lapply(block_ids, function(block_id) {
      block <- block_set$blocks[[block_id]]
      matrix <- block$assay
      if (!is.matrix(matrix) || is.null(rownames(matrix)) || is.null(colnames(matrix))) {
        triomics_integration_abort(
          "C_EXPORT_IDENTITY_MISMATCH",
          "C-06 provenance needs the exact keyed matrices resolved by Stage C.",
          key = paste0("blocks.", block_id)
        )
      }
      list(
        matrix = matrix,
        matrix_sha256 = hash_matrix(block_id, matrix),
        feature_ids = as.character(rownames(matrix)),
        unit_ids = as.character(colnames(matrix))
      )
    }), block_ids)
  }
  blocks <- block_identity(resolved, function(block_id, matrix) {
    digest::digest(matrix, algo = "sha256", serialize = TRUE)
  })
  source_blocks <- block_identity(source_resolved, triomics_b07_matrix_hash)
  source_specs <- config$sources %||% list()
  source_ids <- vapply(source_specs, function(source) as.character(source$source_id), character(1L))
  if (length(source_ids) && anyDuplicated(source_ids)) {
    triomics_integration_abort(
      "C_EXPORT_IDENTITY_MISMATCH", "Configured C source identities must be unique.",
      key = "sources"
    )
  }
  conversion_count <- sum(vapply(source_specs, function(source) {
    !is.null(source$conversion_manifest)
  }, logical(1L)))
  metadata_is_declared_source <- any(vapply(source_specs, function(source) {
    identical(as.character(source$file), as.character(config$metadata$file))
  }, logical(1L)))
  b_analysis_unit_map <- triomics_b_provenance_unit_map(ingested, analysis, source_resolved)
  list(
    study_id = as.character(ingested$study_id),
    analysis_id = as.character(analysis$analysis_id),
    preparation_context_id = as.character(analysis$preparation_context_id),
    representation_id = as.character(representation),
    source_inputs = unname(source_inputs),
    source_blocks = source_blocks,
    expected_source_entity_count = length(source_specs) + conversion_count +
      as.integer(!metadata_is_declared_source),
    normalized_metadata = list(
      sample_metadata = ingested$sample_metadata,
      sample_registry = ingested$sample_registry,
      declared_factor_levels = ingested$declared_factor_levels
    ),
    analysis_unit_map = b_analysis_unit_map,
    analysis_unit_map_sha256 = triomics_b07_prov_value_sha256(b_analysis_unit_map),
    blocks = blocks
  )
}

triomics_c08_cli_refuse_existing_d <- function(output) {
  d_root <- file.path(output, "D")
  if (triomics_is_symlink(d_root) || (file.exists(d_root) && !dir.exists(d_root)) ||
      (dir.exists(d_root) && length(list.files(d_root, all.files = TRUE, no.. = TRUE)))) {
    triomics_c08_abort(
      "C_EXPORT_ENDPOINT_CONFLICT",
      "The endpoint-only C route cannot schedule B requests into a root with an existing D publication.",
      "D"
    )
  }
  invisible(TRUE)
}

triomics_execute_stage_c <- function(config_path, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_abort(
      "A_OUTPUT_INVALID", "Stage C output must be a directory path.",
      key = "output", module = "integration", stage = "C",
      remediation = "Choose a new local output directory."
    )
  }
  if (file.exists(output) && !dir.exists(output)) {
    triomics_abort(
      "A_OUTPUT_INVALID", "Stage C output exists and is not a directory.",
      key = "output", module = "integration", stage = "C",
      remediation = "Choose a new local output directory."
    )
  }

  config <- triomics_read_config(config_path)
  if (!is.null(config$clinical_prediction)) {
    return(triomics_execute_c09_synthetic_stage(config, output))
  }
  has_clinical_objectives <- isTRUE(config$clinical$enabled) &&
    is.list(config$clinical$objectives) && length(config$clinical$objectives) > 0L
  if (has_clinical_objectives) triomics_c08_cli_refuse_existing_d(output)
  if (length(config$analyses) != 1L) {
    triomics_integration_abort(
      "C_ELIGIBILITY_INPUT_INVALID",
      "The registered C-01 CLI lane requires exactly one declared analysis.",
      key = "analyses"
    )
  }
  analysis <- config$analyses[[1L]]
  requests <- analysis$integration
  if (!is.list(requests) || !length(requests)) {
    triomics_integration_abort(
      "C_SCORE_BACKEND_UNAVAILABLE",
      "Stage C needs at least one explicit integration request.",
      key = "analyses.integration"
    )
  }
  requested_methods <- vapply(requests, function(request) {
    method <- request$method
    if (!is.character(method) || length(method) != 1L || is.na(method)) "" else method
  }, character(1))
  unsupported <- which(!requested_methods %in% triomics_integration_backend_names())
  if (length(unsupported)) {
    triomics_integration_backend(requested_methods[[unsupported[[1L]]]])
  }

  ingested <- triomics_ingest_configured_study(config, config_path)
  clinical <- if (isTRUE(config$clinical$enabled)) {
    triomics_ingest_clinical(config, config_path, ingested$sample_registry)
  } else {
    NULL
  }
  unit_ids <- as.character(ingested$sample_metadata$sample_id)
  representation <- analysis$design$representation
  block_ids <- as.character(unlist(analysis$block_ids, use.names = FALSE))
  analysis_unit_map <- NULL
  if (identical(representation, "levels")) {
    unit_type <- if (is.character(analysis$design$visit_column) &&
                     length(analysis$design$visit_column) == 1L &&
                     nzchar(analysis$design$visit_column)) "visit_level" else "sample"
    analysis_unit_map <- data.frame(
      unit_id = unit_ids,
      unit_type = rep(unit_type, length(unit_ids)),
      representation_id = rep(representation, length(unit_ids)),
      stringsAsFactors = FALSE
    )
    analysis_unit_map$source_sample_ids <- I(lapply(unit_ids, identity))
    if ("subject_id" %in% names(ingested$sample_registry)) {
      registry_subjects <- as.character(ingested$sample_registry$subject_id)
      if (length(registry_subjects) == length(unit_ids) && all(!is.na(registry_subjects)) &&
          all(nzchar(registry_subjects))) {
        analysis_unit_map$subject_id <- registry_subjects
      }
    }
  }
  study <- list(
    study_id = ingested$study_id,
    preparation_context_id = analysis$preparation_context_id,
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels,
    feature_registry = ingested$feature_registry,
    assays = ingested$assays[block_ids],
    assay_hashes = NULL,
    analysis_unit_map = analysis_unit_map
  )
  resolved <- triomics_resolve_analysis(study, analysis)
  resolved <- triomics_c06_cli_ingest_lineage(resolved, ingested, config, block_ids)
  source_resolved <- resolved
  if (any(!vapply(requests, function(request) {
    identical(request$evaluation$mode, "nested_cv")
  }, logical(1L))) && identical(representation, "levels")) {
    resolved <- triomics_c06_cli_prepare_resolved(
      resolved, ingested, config, analysis, block_ids, output
    )
  }
  source_unit_map <- triomics_b_provenance_unit_map(ingested, analysis, source_resolved)
  repeated_composition <- triomics_c06_cli_compose_unit_map(
    resolved, ingested, analysis, block_ids, source_unit_map
  )
  integration_resolved <- resolved
  if (!is.null(repeated_composition)) {
    integration_resolved$unit_ids <- repeated_composition$unit_ids
  }
  c08_methods <- c("concat_pca", "mixomics_diablo", "mixomics_block_spls", "mofa2")
  resolved_unit_records <- triomics_c08_unit_records(
    if (!is.null(repeated_composition)) repeated_composition$unit_map else
      if (!is.null(resolved$unit_map)) resolved$unit_map else analysis_unit_map
  )
  clinical_source_ids <- unique(unlist(lapply(resolved_unit_records, function(record) {
    record$source_sample_ids
  }), use.names = FALSE))
  clinical_view <- if (has_clinical_objectives && !is.null(clinical) && length(clinical_source_ids)) {
    triomics_c08_cli_clinical_view(clinical, clinical_source_ids)
  } else {
    NULL
  }

  c06_source_inputs <- unname(lapply(ingested$source_manifests, function(source) {
    list(artifact_id = source$source_id, sha256 = source$sha256)
  }))
  privacy_class <- if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  c06_integration_runs <- vector("list", length(requests))
  integration_results <- lapply(seq_along(requests), function(request_index) {
    request <- requests[[request_index]]
    c08_method <- request$method %in% c08_methods
    if (has_clinical_objectives && !c08_method) {
      triomics_c08_abort(
        "C_SCORE_BACKEND_UNAVAILABLE",
        "Declared clinical objectives need a registered C-08 score endpoint method.",
        "analyses.integration.method"
      )
    }
    if (identical(request$evaluation$mode, "nested_cv")) {
      if (has_clinical_objectives) {
        triomics_c08_abort(
          "C_SCORE_REQUEST_INVALID",
          "Nested evaluation does not expose a single same-fit score endpoint for B scheduling.",
          "analyses.integration.evaluation.mode"
        )
      }
      evaluation <- triomics_c02_evaluate_request(
        config, ingested, analysis, request, block_ids, unit_ids, output
      )
      c06_integration_runs[[request_index]] <<- list(
        integration_id = request$integration_id,
        method = request$method,
        required = isTRUE(request$required),
        status = "unavailable",
        requested_evaluation_status = triomics_c06_cli_requested_evaluation_status(evaluation),
        reason_code = "C06_INTEGRATION_RESULT_UNAVAILABLE",
        reason = paste(
          "Nested-CV evaluation did not produce a single IntegrationResult",
          "and native sidecar for the D handoff."
        )
      )
      return(list(
        integration_id = request$integration_id,
        method = request$method,
        required = isTRUE(request$required),
        status = "complete",
        evaluation = evaluation
      ))
    }
    spec <- request
    spec$block_ids <- block_ids
    spec$unit_ids <- if (is.null(repeated_composition)) unit_ids else repeated_composition$unit_ids
    spec$training_unit_ids <- if (is.null(repeated_composition)) {
      unit_ids
    } else {
      repeated_composition$training_unit_ids
    }
    spec$target_unit_ids <- if (is.null(repeated_composition)) {
      unit_ids
    } else {
      repeated_composition$target_unit_ids
    }
    spec$required_views <- block_ids
    spec$task_mode <- request$evaluation$mode
    result <- if (c08_method) {
      unit_map <- if (is.null(repeated_composition)) {
        triomics_c08_cli_unit_map(
          resolved, ingested$sample_registry, analysis_unit_map, clinical_view
        )
      } else {
        repeated_resolved <- resolved
        repeated_resolved$unit_map <- repeated_composition$unit_map
        triomics_c08_cli_unit_map(
          repeated_resolved, ingested$sample_registry, analysis_unit_map, clinical_view
        )
      }
      selection <- triomics_c08_cli_selection(request, ingested, block_ids)
      fit_context <- list(
        integration_id = request$integration_id,
        unit_map = unit_map,
        selection = selection,
        available_at_time = "after_integration_fit",
        dependency_group = paste(ingested$study_id, request$integration_id, sep = "::")
      )
      fitted <- triomics_integration_fit_once(integration_resolved, spec, c08_context = fit_context)
      if (!is.null(resolved$preparation_state_hashes)) {
        fitted <- triomics_c06_cli_bind_preparation_lineage(fitted, resolved)
      }
      c06_run <- triomics_c06_cli_integration_run(request, fitted)
      native_artifact <- triomics_c06_cli_write_native_state(
        c06_run, config, analysis, output, request_index, c06_source_inputs, privacy_class
      )
      c06_run$native_artifact_ref <- native_artifact$artifact_ref
      c06_integration_runs[[request_index]] <<- c06_run
      endpoint_bundle <- triomics_integration_score_endpoints(
        fitted$result, fitted$backend_fit, fitted$context
      )
      endpoint_ids <- vapply(endpoint_bundle$endpoints, function(endpoint) {
        endpoint$endpoint_id
      }, character(1L))
      endpoint_directory <- file.path(output, "C", "score-endpoints")
      endpoint_stub <- sprintf("integration-%02d", request_index)
      endpoint_path <- file.path(endpoint_directory, paste0(endpoint_stub, ".json"))
      endpoint_envelope_path <- file.path(endpoint_directory, paste0(endpoint_stub, ".envelope.json"))
      endpoint_payload <- triomics_c08_cli_endpoint_payload(endpoint_bundle)
      source_inputs <- unname(lapply(ingested$source_manifests, function(source) {
        list(artifact_id = source$source_id, sha256 = source$sha256)
      }))
      endpoint_artifact <- triomics_c08_cli_write_artifact(
        endpoint_payload, endpoint_path, endpoint_envelope_path,
        artifact_type = "integration_score_endpoint_bundle",
        artifact_id = paste0("c08-score-endpoint-bundle-", request_index),
        config = config, analysis = analysis, inputs = source_inputs,
        status = "complete",
        privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
      )
      endpoint_bundle <- triomics_c08_cli_validate_endpoint_readback(endpoint_path)
      endpoint_reference <- list(
        artifact_id = paste0("c08-score-endpoint-bundle-", request_index),
        payload_file = file.path("C", "score-endpoints", basename(endpoint_path)),
        envelope_file = file.path("C", "score-endpoints", basename(endpoint_envelope_path)),
        payload_sha256 = endpoint_artifact$payload_sha256,
        envelope_sha256 = endpoint_artifact$envelope_sha256,
        endpoint_ids = as.list(endpoint_ids)
      )
      request_reference <- NULL
      if (has_clinical_objectives) {
        objective_spec <- triomics_c08_cli_objective_spec(config, analysis)
        scheduled <- triomics_schedule_clinical_score_request(
          endpoint_bundle, objective_spec, clinical_view
        )
        request_directory <- file.path(output, "B", "clinical-score-association-requests")
        request_stub <- sprintf("request-%02d", request_index)
        request_path <- file.path(request_directory, paste0(request_stub, ".json"))
        request_envelope_path <- file.path(request_directory, paste0(request_stub, ".envelope.json"))
        clinical_inputs <- list(
          list(artifact_id = endpoint_reference$artifact_id, sha256 = endpoint_artifact$payload_sha256),
          list(artifact_id = "clinical-dictionary-source", sha256 = clinical$dictionary_sha256),
          list(artifact_id = "clinical-observations-source", sha256 = clinical$observations_sha256),
          list(artifact_id = "clinical-links-source", sha256 = clinical$links_sha256)
        )
        request_artifact <- triomics_c08_cli_write_artifact(
          unclass(scheduled), request_path, request_envelope_path,
          artifact_type = "clinical_score_association_request",
          artifact_id = paste0("c08-clinical-score-request-", request_index),
          config = config, analysis = analysis, inputs = clinical_inputs,
          status = "not_started",
          privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
        )
        request_reference <- list(
          artifact_id = paste0("c08-clinical-score-request-", request_index),
          payload_file = file.path("B", "clinical-score-association-requests", basename(request_path)),
          envelope_file = file.path("B", "clinical-score-association-requests", basename(request_envelope_path)),
          payload_sha256 = request_artifact$payload_sha256,
          envelope_sha256 = request_artifact$envelope_sha256,
          request_sha256 = scheduled$request_sha256,
          status = scheduled$status,
          owner_stage = scheduled$owner_stage,
          model_execution = scheduled$model_execution
        )
      }
      list(
        result = fitted$result,
        endpoint_bundle = endpoint_bundle,
        endpoint_reference = endpoint_reference,
        request_reference = request_reference
      )
    } else {
      fitted <- triomics_integration_fit_once(resolved, spec)
      if (!is.null(resolved$preparation_state_hashes)) {
        fitted <- triomics_c06_cli_bind_preparation_lineage(fitted, resolved)
      }
      c06_run <- triomics_c06_cli_integration_run(request, fitted)
      native_artifact <- triomics_c06_cli_write_native_state(
        c06_run, config, analysis, output, request_index, c06_source_inputs, privacy_class
      )
      c06_run$native_artifact_ref <- native_artifact$artifact_ref
      c06_integration_runs[[request_index]] <<- c06_run
      list(result = fitted$result)
    }
    item <- list(
      integration_id = request$integration_id,
      method = request$method,
      required = isTRUE(request$required),
      status = "complete",
      result = triomics_c01_result_payload(result$result)
    )
    if (!is.null(result$endpoint_reference)) item$score_endpoint_bundle <- result$endpoint_reference
    if (!is.null(result$request_reference)) item$clinical_score_request <- result$request_reference
    item
  })

  payload <- list(
    schema_version = "2.0",
    study_id = ingested$study_id,
    analysis_id = analysis$analysis_id,
    preparation_context_id = analysis$preparation_context_id,
    representation_id = representation,
    status = "complete",
    integrations = integration_results
  )
  payload_path <- file.path(output, "C", "integration-results.json")
  triomics_write_json_atomic(payload, payload_path)
  native_inputs <- lapply(Filter(function(run) {
    identical(run$status, "complete") && is.list(run$native_artifact_ref)
  }, c06_integration_runs), `[[`, "native_artifact_ref")
  inputs <- c(c06_source_inputs, native_inputs)
  envelope <- triomics_artifact_envelope(
    artifact_type = "integration_results",
    artifact_id = "stage-c-integration-results",
    study_id = ingested$study_id,
    run_id = "c-01",
    status = "complete",
    config_sha256 = attr(config, "config_sha256"),
    payload = payload_path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = character(),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive"
  )
  envelope$analysis_id <- analysis$analysis_id
  envelope$preparation_context_id <- analysis$preparation_context_id
  envelope$representation_id <- representation
  triomics_write_envelope(envelope, file.path(output, "C", "integration-results.envelope.json"))
  endpoint_requests <- vapply(integration_results, function(item) {
    request <- item$clinical_score_request
    is.list(request) && identical(request$status, "scheduled") &&
      identical(request$owner_stage, "B") && identical(request$model_execution, "not_started")
  }, logical(1L))
  if (has_clinical_objectives && !all(endpoint_requests)) {
    triomics_c08_abort(
      "C_SCORE_REQUEST_INVALID",
      "A configured clinical-objective route must produce every declared B request before Stage C returns.",
      "clinical.objectives"
    )
  }
  required_runs <- Filter(function(run) isTRUE(run$required), c06_integration_runs)
  required_complete <- all(vapply(required_runs, function(run) {
    requested_status <- run$requested_evaluation_status %||% run$status
    identical(requested_status, "complete")
  }, logical(1L)))
  run_status <- if (required_complete) "complete" else "partial"
  if (has_clinical_objectives) {
    triomics_write_json_atomic(list(
      schema_version = "2.0",
      run_id = "c-01",
      study_id = ingested$study_id,
      stage = "C",
      status = run_status
    ), file.path(output, "registries", "run-C.json"))
    return(invisible(list(
      status = "endpoint_request_scheduled",
      run_status = run_status,
      payload_path = payload_path,
      results = integration_results,
      required_complete = required_complete
    )))
  }
  evaluation_controls <- triomics_c06_cli_unrun_controls(
    c06_integration_runs, resolved, analysis, integration_results
  )
  handoff <- triomics_c06_build_handoff(
    primary_nominations = triomics_c06_cli_primary_nominations(output, config, analysis),
    integration_runs = c06_integration_runs,
    evaluation_evidence = list(controls = evaluation_controls),
    clinical_prediction = list(
      status = "not_started",
      completion_policy = "conditional",
      packet_id = "C-09",
      model_execution = "not_started",
      reason = "Conditional matched clinical prediction has not been executed."
    )
  )
  if (!triomics_c06_nonempty_string(ingested$study_id) ||
      !triomics_c06_nonempty_string(analysis$analysis_id)) {
    triomics_integration_abort(
      "C_EXPORT_CONTEXT_MISSING",
      "The Stage-C handoff needs verified study and analysis identities from its resolved request.",
      key = "handoff.context"
    )
  }
  handoff$study_id <- ingested$study_id
  handoff$analysis_id <- analysis$analysis_id
  handoff <- triomics_c06_cli_aggregate_status(handoff)
  triomics_c06_write_handoff(
    handoff,
    output,
    config_sha256 = attr(config, "config_sha256"),
    producer_commit = triomics_b07_git_ref(getwd()),
    privacy_class = privacy_class,
    b_predecessor_paths = triomics_c06_cli_native_b_predecessors(output),
    c_input_identity = triomics_c06_cli_input_identity(
      resolved, ingested, analysis, representation, block_ids, c06_source_inputs,
      config, source_resolved
    )
  )
  run_status <- if (required_complete) "complete" else handoff$status
  triomics_write_json_atomic(list(
    schema_version = "2.0",
    run_id = "c-01",
    study_id = ingested$study_id,
    stage = "C",
    status = run_status
  ), file.path(output, "registries", "run-C.json"))
  invisible(list(
      status = handoff$status,
      run_status = run_status,
      payload_path = payload_path,
      results = integration_results,
      required_complete = required_complete
  ))
}

triomics_d_cli_route_status <- function(evidence) {
  provenance <- evidence$immutable_inputs
  predecessor <- list(
    b_sha256 = as.character(provenance$b_bundle_sha256),
    c_sha256 = as.character(provenance$c_manifest_sha256)
  )
  routes <- stats::setNames(lapply(triomics_d_route_names(), function(name) {
    list(enabled = FALSE, required = FALSE, status = "not_run")
  }), triomics_d_route_names())
  triomics_d_route_status(routes, list(), predecessor, list())
}

triomics_d_attach_target_profiles <- function(existing, investigation) {
  investigation <- investigation %||% list()
  if (!is.list(existing)) existing <- list()
  if (!isTRUE(investigation$enabled)) return(existing)
  # A supplied evidence or mechanism file is not turned into links or observations.
  if (triomics_d_nonempty_string(investigation$external_evidence_file) ||
      triomics_d_nonempty_string(investigation$mechanism_file)) {
    return(existing)
  }
  targets <- investigation$targets %||% list()
  if (!triomics_d_sequence(targets) || !length(targets)) return(existing)
  profiles <- list()
  for (target in targets) {
    target_id <- triomics_d_one_string(target$id, "D_TARGET_IDENTITY_REQUIRED",
                                       "A target needs an identifier.",
                                       "target_investigation.targets")
    if (!identical(target_id, "SYNTH_FCN3_LIKE")) {
      existing$skipped <- c(existing$skipped, list(list(
        id = target_id, reason = "mechanism_class_not_declared"
      )))
      next
    }
    namespace <- triomics_d_one_string(target$namespace, "D_TARGET_IDENTITY_REQUIRED",
                                       "A target needs a namespace.",
                                       "target_investigation.targets")
    request <- triomics_target_request(
      identity = list(
        namespace = namespace,
        id = target_id,
        taxon_id = as.numeric(target$taxon_id),
        entity_type = triomics_d_one_string(target$entity_type, "D_TARGET_IDENTITY_REQUIRED",
                                            "A target needs an entity type.",
                                            "target_investigation.targets"),
        constructs = list()
      ),
      mode = triomics_d_one_string(investigation$mode, "D_TARGET_INPUT_INVALID",
                                  "Target mode must be one value.",
                                  "target_investigation.mode"),
      selection = list(list(
        selection_id = paste0("declared:", namespace, ":", target_id),
        hypothesis_status = "exploratory",
        selected_with_observations = FALSE
      )),
      context = list(context_id = as.character(investigation$context_ref),
                     confirmation_protocol = as.character(investigation$protocol_ref)),
      drug = NULL
    )
    request$mechanism_class <- "non_enzymatic"
    profiles[[length(profiles) + 1L]] <- triomics_target_profile(request, list(), list())
  }
  alternatives <- if (length(profiles) >= 2L) triomics_target_alternatives(profiles) else NULL
  existing$drug_ref <- investigation$drug_ref %||% NULL
  existing$mechanism_status <- "not_supplied"
  existing$profiles <- profiles
  existing$alternatives <- alternatives
  existing$selected_target <- NULL
  existing$tie_status <- if (is.null(alternatives)) NULL else alternatives$tie_status
  existing
}

triomics_execute_stage_d <- function(config_path, output) {
  root <- triomics_d_secure_path(
    output, "D_OUTPUT_PATH_UNSAFE",
    "Stage D requires the existing same-root B/C handoff directory and a safe local output root.",
    "output", directory = TRUE
  )
  if (!dir.exists(root)) {
    triomics_d_abort("D_PROVENANCE_REQUIRED", "Stage D requires a completed same-root B/C handoff.",
                     "output")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  registry <- file.path(root, "registries")
  pointer_path <- file.path(registry, "current-D.json")
  lock_path <- file.path(registry, ".D-publication.lock")
  for (path in c(file.path(root, "D"), file.path(root, "D", "generations"),
                 registry, pointer_path, lock_path)) {
    triomics_d_secure_path(
      path, "D_OUTPUT_PATH_UNSAFE",
      "A D publication path is unsafe or crosses a symlink.",
      "publication.paths",
      directory = path %in% c(file.path(root, "D"),
                              file.path(root, "D", "generations"),
                              registry, lock_path)
    )
  }
  if (!dir.exists(file.path(root, "D")) || !dir.exists(registry) ||
      (file.exists(pointer_path) && dir.exists(pointer_path)) ||
      file.exists(lock_path) || dir.exists(lock_path)) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "D publication requires safe B/C directories and an available exclusive lock.",
                     "publication.paths")
  }
  if (!dir.create(lock_path, recursive = FALSE, showWarnings = FALSE)) {
    triomics_d_abort("D_PUBLICATION_FAILED", "Another D publisher holds the exclusive publication lock.",
                     "publication.lock")
  }
  on.exit(unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
  for (path in c(file.path(root, "D"), file.path(root, "D", "generations"),
                 registry, pointer_path, lock_path)) {
    triomics_d_secure_path(
      path, "D_OUTPUT_PATH_UNSAFE",
      "A D publication path changed or became unsafe while acquiring the run lock.",
      "publication.paths",
      directory = path %in% c(file.path(root, "D"),
                              file.path(root, "D", "generations"),
                              registry, lock_path)
    )
  }
  if (!dir.exists(file.path(root, "D")) || !dir.exists(registry) ||
      (file.exists(pointer_path) && dir.exists(pointer_path))) {
    triomics_d_abort("D_OUTPUT_PATH_UNSAFE", "D publication directories changed before locked preflight.",
                     "publication.paths")
  }
  triomics_d_validate_current_pointer(root, pointer_path)
  config <- triomics_read_config(config_path)
  triomics_validate_config_paths(config, config_path)
  if (length(config$analyses) != 1L) {
    triomics_d_abort("D_PROVENANCE_INVALID", "The registered D-T01 lane requires one declared analysis.",
                     "analyses")
  }
  analysis <- config$analyses[[1L]]
  ingested <- triomics_ingest_configured_study(config, config_path)
  current_metadata <- list(
    sample_metadata = ingested$sample_metadata,
    sample_registry = ingested$sample_registry,
    declared_factor_levels = ingested$declared_factor_levels
  )
  annotation <- config$annotation %||% list()
  requests <- if (is.list(annotation)) annotation$requests %||% list() else list()
  local_annotation <- triomics_d_sequence(requests) && length(requests) > 0L
  if (length(requests) > 0L && !local_annotation) {
    triomics_d_abort("D_ROUTE_INVALID", "Annotation requests must be a list.", "annotation.requests")
  }
  routing_metadata <- if (local_annotation) {
    triomics_d_routing_metadata(config, config_path, ingested$feature_registry)
  } else {
    NULL
  }
  evidence <- triomics_d_read_evidence_bundle(
    root, as.character(config$study_id), as.character(analysis$analysis_id),
    ingested$source_manifests, ingested$feature_registry,
    imported_statistics = list(),
    routing_metadata = routing_metadata,
    current_config_sha256 = as.character(attr(config, "config_sha256")),
    current_metadata = current_metadata,
    current_count_metadata = analysis$statistics$count_metadata %||% NULL
  )
  if (local_annotation) {
    routed <- triomics_d_annotate_local_resources(
      evidence, annotation, file.path(root, "D", "resource-cache"),
      config$target_investigation %||% list(), config_path
    )
    route_status <- routed$route_status
    routed$route_status <- NULL
  } else {
    routed <- triomics_annotate_evidence(evidence, annotation)
    route_status <- triomics_d_cli_route_status(evidence)
  }
  investigation <- config$target_investigation %||% list()
  if (isTRUE(investigation$enabled)) {
    if (is.null(routed$target_investigation)) {
      routed$target_investigation <- triomics_d_target_slot(investigation, config_path)
      if (isTRUE(investigation$required) &&
          identical(routed$target_investigation$status, "unavailable")) {
        route_status$exit_code <- 2L
      }
    }
    routed$target_investigation <- triomics_d_attach_target_profiles(
      routed$target_investigation, investigation
    )
    routed$target_investigation$protein_metabolites <- triomics_protein_metabolite_report(investigation)
  }
  triomics_d_publication_failpoint("provenance_preflight_complete")
  triomics_d_publish_evidence_generation(
    routed, root, config_sha256 = as.character(attr(config, "config_sha256")),
    producer_commit = triomics_b07_git_ref(getwd()),
    privacy_class = if (identical(config$data_class, "synthetic")) "synthetic" else "sensitive",
    run_id = "d-t01",
    lock_held = TRUE
  )
  invisible(list(status = "complete", root = root, exit_code = route_status$exit_code,
                 route_status = route_status))
}

triomics_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch({
    options <- triomics_parse_cli(args)
    if (options$help) {
      cat(triomics_usage(), "\n")
      return(0L)
    }
    if (is.null(options$stage)) {
      triomics_abort("A_CLI_ARGUMENT", "Option --stage is required.", key = "stage",
                     module = "cli", remediation = "Run triomics --help for the stage grammar.")
    }
    triomics_require_stage(options$stage, options$validate_only)
    if (is.null(options$config) || is.null(options$output)) {
      triomics_abort("A_CLI_ARGUMENT", "Options --config and --output are required.",
                     key = if (is.null(options$config)) "config" else "output",
                     module = "cli", stage = options$stage,
                     remediation = "Provide --config FILE and --output DIR.")
    }
    if (isTRUE(options$validate_only)) {
      config <- triomics_read_config(options$config)
      triomics_validate_only(config, options$config, options$stage, options$output)
    } else if (identical(options$stage, "A")) {
      triomics_execute_stage_a(options$config, options$output)
    } else if (identical(options$stage, "B")) {
      registered_b09 <- triomics_cli_prepare_registered_b09(options, args)
      registered_b08 <- triomics_cli_prepare_registered_b08(options, args)
      stage_b_result <- triomics_execute_stage_b(options$config, options$output)
      if (isTRUE(registered_b09)) triomics_b09_cli_after_stage_b(args)
      if (isTRUE(registered_b08)) triomics_b08_cli_after_stage_b(args)
      triomics_b07_cli_after_stage_b(args, stage_b_result)
    } else if (identical(options$stage, "C")) {
      stage_c_result <- triomics_execute_stage_c(options$config, options$output)
      if (!isTRUE(stage_c_result$required_complete)) {
        return(4L)
      }
    } else if (identical(options$stage, "D")) {
      stage_d_result <- triomics_execute_stage_d(options$config, options$output)
      return(as.integer(stage_d_result$exit_code))
    } else {
      triomics_abort("A_STAGE_UNAVAILABLE",
                     paste0("Execution is not implemented for stage '", options$stage, "'."),
                     key = "stage", module = "cli", stage = options$stage,
                     remediation = "Use --validate-only for stage A, or run an accepted stage.")
    }
    0L
  }, triomics_error = function(error) {
    cat(paste0("triomics ", error$code, ": ", conditionMessage(error), "\n"), file = stderr())
    triomics_cli_exit(error)
  }, error = function(error) {
    cat(paste0("triomics A_INTERNAL: ", conditionMessage(error), "\n"), file = stderr())
    1L
  })
}
