# A export, resume, and profile-extension handoff. Orchestration dispatches on
# registered profile ids; it does not grow a branch per profile name.

triomics_export_abort <- function(code, message, key = NULL, remediation) {
  triomics_abort(code, message, key = key, module = "export_a", stage = "A", remediation = remediation)
}

triomics_export_cache_key <- function(components) {
  if (!is.list(components) || is.null(names(components)) || anyDuplicated(names(components)) ||
      any(!nzchar(names(components)))) {
    triomics_export_abort(
      "A_EXPORT_CACHE", "A cache key needs uniquely named components.", "cache_key",
      "Name each cache component. Do not hash an unnamed or repeated list."
    )
  }
  components <- components[order(names(components))]
  triomics_hash_text(triomics_canonical_json(components))
}

triomics_export_resume_decision <- function(candidate, expected_key) {
  if (!is.character(expected_key) || length(expected_key) != 1L || is.na(expected_key) ||
      !grepl("^[0-9a-f]{64}$", expected_key)) {
    triomics_export_abort(
      "A_EXPORT_CACHE", "The expected cache key is not a SHA256 digest.", "cache_key",
      "Recompute the cache key from the source, configuration, and version."
    )
  }
  refuse <- function(reason) list(reuse = FALSE, reason = reason)
  if (is.null(candidate) || !isTRUE(candidate$payload_present)) return(refuse("absent"))
  if (!isTRUE(candidate$envelope_present)) return(refuse("partial"))
  if (!identical(candidate$status, "complete")) return(refuse("incomplete"))
  if (!identical(candidate$cache_key, expected_key)) return(refuse("invalidated"))
  if (!identical(candidate$payload_sha256, candidate$manifest_payload_sha256)) return(refuse("hash_mismatch"))
  list(reuse = TRUE, reason = "complete_match")
}

triomics_a_handoff_limitations <- function(request = list()) {
  if (is.null(request)) request <- list()
  if (!is.list(request)) {
    triomics_export_abort(
      "A_EXPORT_CLAIM", "Handoff limitations need a request list.", "request",
      "Pass the export request. Do not collapse the evidential levels into one flag."
    )
  }
  qualifications <- request$qualifications
  claimed <- isTRUE(request$claim_scientific_validation) || isTRUE(request$claim_method_qualification) ||
    (is.list(qualifications) && length(qualifications) > 0L)
  if (claimed) {
    triomics_export_abort(
      "A_EXPORT_CLAIM",
      "A handoff cannot claim real-data scientific validation or optional method qualification.",
      "scientific_claim",
      "Keep source validation separate. Do not mark an optional method qualified without a version-bound qualification."
    )
  }
  optional <- list(
    list(method_id = "internal_standard", mode_support = "fit_apply_qualified", status = "not_qualified",
         reason = "No version-bound MethodQualification artifact was supplied."),
    list(method_id = "vsn", mode_support = "fit_apply_qualified", status = "not_qualified",
         reason = "No version-bound MethodQualification artifact was supplied."),
    list(method_id = "minprob", mode_support = "qualified", status = "not_qualified",
         reason = "No version-bound MethodQualification artifact was supplied."),
    list(method_id = "qrilc", mode_support = "qualified", status = "not_qualified",
         reason = "No version-bound MethodQualification artifact was supplied."),
    list(method_id = "knn", mode_support = "qualified", status = "not_qualified",
         reason = "No version-bound MethodQualification artifact was supplied."),
    list(method_id = "rf", mode_support = "qualified", status = "not_qualified",
         reason = "No version-bound MethodQualification artifact was supplied.")
  )
  list(
    schema_version = "2.0",
    scientific_claim = "not_established",
    evidential_levels = list(
      source_validation = list(
        name = "source_validation",
        level = "implementation_acceptance",
        established = TRUE,
        status = "exercised_on_synthetic_fixtures",
        meaning = "This export checks synthetic source bytes, feature keys, preparation state, and declared clinical linkage. It is source validation of software behavior, not a measurement of real samples."
      ),
      real_data_scientific_validation = list(
        name = "real_data_scientific_validation",
        level = "scientific_user_data",
        established = FALSE,
        status = "not_established",
        meaning = "Representative measurements and a prespecified null, uncertainty, coverage, and robustness protocol are required. Synthetic fixtures and this handoff do not establish real-data scientific validation."
      ),
      optional_method_qualification = list(
        name = "optional_method_qualification",
        level = "method_qualification",
        established = FALSE,
        status = "not_qualified",
        meaning = "Optional methods stay unavailable until a version-bound MethodQualification artifact, exact native API, and supported-context tests exist. A-T08 does not invent that qualification.",
        methods = optional
      )
    )
  )
}

triomics_export_identifier <- function(value, key) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !grepl("^[A-Za-z][A-Za-z0-9_-]*$", value)) {
    triomics_export_abort(
      "A_EXPORT_CAPABILITY", paste0("'", key, "' must be one identifier."), key,
      "Use a stable identifier. Do not dispatch on a display sentence."
    )
  }
  value
}

triomics_export_default_operations <- function() {
  list(
    list(stage = "validate", method = "identity"),
    list(stage = "filter", method = "none", min_observed = NULL, prevalence = NULL),
    list(stage = "derive", method = "none"),
    list(stage = "transform", method = "none", zero_policy = "error", zero_value = NULL),
    list(stage = "normalise", method = "none"),
    list(stage = "impute", method = "none")
  )
}

triomics_export_ensure_profile <- function(profile_id) {
  if (identical(profile_id, "proteomics")) {
    triomics_register_proteomics_profile()
  } else if (identical(profile_id, "metabolomics")) {
    triomics_register_metabolomics_profile()
  } else if (identical(profile_id, "lipidomics")) {
    triomics_register_lipidomics_profile()
  }
  triomics_registered_profile(profile_id)
  invisible(profile_id)
}

triomics_export_format_number <- function(value) {
  if (is.na(value)) "NA" else formatC(value, format = "fg", digits = 15)
}

triomics_export_write_tsv <- function(path, header, rows) {
  lines <- c(
    paste(header, collapse = "\t"),
    vapply(rows, function(row) paste(row, collapse = "\t"), character(1))
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

triomics_export_write_records <- function(path, records) {
  if (!is.list(records) || length(records) < 1L || is.null(names(records[[1L]]))) {
    triomics_export_abort(
      "A_EXPORT_CAPABILITY", "Clinical tables need at least one named row.", "clinical",
      "Supply dictionary, observation, and link rows. Do not invent a clinical table."
    )
  }
  header <- names(records[[1L]])
  rows <- lapply(records, function(record) {
    if (!identical(names(record), header)) {
      triomics_export_abort(
        "A_EXPORT_CAPABILITY", "Clinical rows do not share one header.", "clinical",
        "Give every row the same columns."
      )
    }
    unname(as.character(record[header]))
  })
  triomics_export_write_tsv(path, header, rows)
}

triomics_export_as_chr <- function(value) {
  if (is.null(value)) return(character())
  if (is.character(value) && is.null(names(value))) return(unname(value))
  if (is.character(value)) return(as.character(value))
  out <- unlist(value, use.names = FALSE)
  if (is.null(out)) character() else as.character(out)
}

triomics_export_candidate <- function(payload_path) {
  envelope_path <- sub("\\.json$", ".envelope.json", payload_path)
  payload_present <- file.exists(payload_path) && !dir.exists(payload_path)
  envelope_ok <- file.exists(envelope_path) && isTRUE(triomics_validate_envelope(envelope_path))
  cache_key <- NULL
  payload_sha <- NULL
  status <- NULL
  manifest_sha <- NULL
  if (payload_present) {
    payload_sha <- triomics_sha256(payload_path)
    parsed <- tryCatch(jsonlite::read_json(payload_path, simplifyVector = FALSE), error = function(error) NULL)
    if (is.list(parsed) && is.character(parsed$cache_key) && length(parsed$cache_key) == 1L) {
      cache_key <- parsed$cache_key
    }
  }
  if (envelope_ok) {
    envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)
    status <- if (is.character(envelope$status) && length(envelope$status) == 1L) envelope$status else NULL
    manifest_sha <- envelope$payload_sha256
  }
  list(
    status = status,
    cache_key = cache_key,
    payload_sha256 = payload_sha,
    manifest_payload_sha256 = manifest_sha,
    payload_present = payload_present,
    envelope_present = envelope_ok
  )
}

triomics_export_inputs <- function(ids, shas) {
  if (length(ids) < 1L) return(list())
  lapply(seq_along(ids), function(index) list(artifact_id = ids[[index]], sha256 = shas[[index]]))
}

triomics_export_finalize <- function(payload, path, artifact_type, artifact_id, study_id, config_sha256,
                                    inputs, reason_codes, interrupt, interrupt_token) {
  envelope_path <- sub("\\.json$", ".envelope.json", path)
  if (file.exists(envelope_path)) {
    triomics_export_abort(
      "A_EXPORT_CACHE", "A complete manifest already exists for this cache key.", artifact_id,
      "Resume the matching artifact instead of rewriting it."
    )
  }
  if (file.exists(path)) unlink(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  triomics_write_json_atomic(payload, path)
  if (!is.null(interrupt) && identical(interrupt, interrupt_token)) {
    triomics_export_abort(
      "A_EXPORT_INTERRUPTED",
      "Export stopped between the payload and its manifest.",
      interrupt_token,
      "Do not accept a payload without a manifest. Resume only a complete matching artifact."
    )
  }
  envelope <- triomics_artifact_envelope(
    artifact_type = artifact_type,
    artifact_id = artifact_id,
    study_id = study_id,
    run_id = "a-t08",
    status = "complete",
    config_sha256 = config_sha256,
    payload = path,
    producer_commit = "not-recorded",
    inputs = inputs,
    reason_codes = reason_codes,
    privacy_class = "synthetic"
  )
  triomics_write_envelope(envelope, envelope_path)
  list(
    payload_path = path,
    envelope_path = envelope_path,
    payload_sha256 = envelope$payload_sha256
  )
}

triomics_export_matrix <- function(payload) {
  feature_keys <- triomics_export_as_chr(payload$feature_keys)
  sample_ids <- triomics_export_as_chr(payload$sample_ids)
  values <- payload$values
  assay <- matrix(NA_real_, length(feature_keys), length(sample_ids), dimnames = list(feature_keys, sample_ids))
  if (is.null(values)) return(assay)
  for (i in seq_along(feature_keys)) {
    row <- values[[i]]
    for (j in seq_along(sample_ids)) {
      value <- row[[j]]
      if (!is.null(value)) assay[i, j] <- as.numeric(value)
    }
  }
  assay
}

triomics_export_a <- function(request, output, resume = FALSE, interrupt = NULL) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    triomics_export_abort("A_OUTPUT_INVALID", "Export output must be a directory path.", "output",
                          "Choose a local output directory.")
  }
  if (!is.list(request)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "The export request must be a list.", "request",
                          "Pass the synthetic study request. Do not infer a study.")
  }
  triomics_a_handoff_limitations(request)
  design <- request$design
  if (!is.character(design) || length(design) != 1L || !design %in% c("independent", "paired")) {
    triomics_export_abort(
      "A_EXPORT_DESIGN", "Export design must be independent or paired.", "design",
      "Use independent for the two-group simulator design and paired for the paired design."
    )
  }
  data_class <- request$data_class
  if (is.null(data_class)) data_class <- "synthetic"
  if (!identical(data_class, "synthetic")) {
    triomics_export_abort(
      "A_EXPORT_PRIVACY", "A export refuses a non-synthetic data class.", "data_class",
      "Do not export participant values through the synthetic handoff."
    )
  }
  capabilities <- request$capabilities
  known <- c("ingest", "prepare", "clinical", "export")
  if (!is.character(capabilities) || !length(capabilities) || anyDuplicated(capabilities) || any(!capabilities %in% known) ||
      !all(c("ingest", "prepare", "export") %in% capabilities)) {
    triomics_export_abort(
      "A_EXPORT_CAPABILITY", "The requested A capabilities are not a supported export set.", "capabilities",
      "Request ingest, prepare, and export. Clinical is optional. Do not request another stage."
    )
  }
  study_id <- triomics_export_identifier(request$study_id, "study_id")
  version <- request$version
  if (!is.character(version) || length(version) != 1L || is.na(version) || !nzchar(version)) {
    triomics_export_abort("A_EXPORT_CACHE", "Export version must be one non-blank string.", "version",
                          "Record the export version. A missing version is not reusable.")
  }
  context <- request$preparation_context_id
  if (!is.character(context) || length(context) != 1L || is.na(context) || !nzchar(context)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "A preparation context id is required.", "preparation_context_id",
                          "Name the preparation context.")
  }
  config_token <- request$config_token
  if (is.null(config_token)) config_token <- ""
  if (!is.character(config_token) || length(config_token) != 1L || is.na(config_token)) {
    triomics_export_abort("A_EXPORT_CACHE", "The configuration token must be one string.", "config_token",
                          "Use a stable token so configuration changes invalidate descendants.")
  }
  seed <- request$seed
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != trunc(seed)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "Export seed must be one finite integer.", "seed",
                          "Record the simulator seed. There is no hidden seed.")
  }
  seed <- as.integer(seed)
  n_subjects <- request$n_subjects
  if (!is.numeric(n_subjects) || length(n_subjects) != 1L || !is.finite(n_subjects) ||
      n_subjects != trunc(n_subjects) || n_subjects < 1L) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "n_subjects must be a positive integer.", "n_subjects",
                          "Name the synthetic cohort size.")
  }
  n_subjects <- as.integer(n_subjects)
  n_features <- request$n_features
  if (!is.numeric(n_features) || is.null(names(n_features)) || any(!nzchar(names(n_features))) ||
      anyDuplicated(names(n_features)) || any(!is.finite(n_features)) || any(n_features != trunc(n_features)) ||
      any(n_features < 1L)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "n_features must name positive block sizes.", "n_features",
                          "Name each block. A block name is not a profile id.")
  }
  block_ids <- names(n_features)
  n_features <- stats::setNames(as.integer(n_features), block_ids)
  profiles <- request$profiles
  if (!is.character(profiles) || is.null(names(profiles)) || !setequal(names(profiles), block_ids)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "Each block needs a registered profile id.", "profiles",
                          "Map block ids to profile ids. Do not dispatch on the display name.")
  }
  profiles <- profiles[block_ids]
  for (profile_id in profiles) triomics_export_ensure_profile(profile_id)
  display_names <- request$display_names
  if (is.null(display_names)) {
    display_names <- stats::setNames(block_ids, block_ids)
  } else if (!is.character(display_names) || is.null(names(display_names)) || !setequal(names(display_names), block_ids)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "Display names must cover every block id.", "display_names",
                          "Display names are labels. They do not select a profile.")
  } else {
    display_names <- display_names[block_ids]
  }
  supplied_operations <- request$operations
  if (is.null(supplied_operations)) supplied_operations <- list()
  if (!is.list(supplied_operations)) {
    triomics_export_abort("A_EXPORT_CAPABILITY", "Per-block operations must be a list.", "operations",
                          "Supply operations by block id or omit them.")
  }
  ops_map <- lapply(block_ids, function(block_id) {
    custom <- supplied_operations[[block_id]]
    if (is.null(custom)) triomics_export_default_operations() else custom
  })
  names(ops_map) <- block_ids
  wants_clinical <- "clinical" %in% capabilities
  clinical <- request$clinical
  if (wants_clinical && (is.null(clinical) || !is.list(clinical))) {
    triomics_export_abort(
      "A_EXPORT_CAPABILITY", "Clinical export was requested without clinical tables.", "clinical",
      "Supply dictionary, observations, and links, or omit the clinical capability."
    )
  }
  if (!wants_clinical && !is.null(clinical)) {
    triomics_export_abort(
      "A_EXPORT_CAPABILITY", "Clinical tables were supplied for a request that does not run clinical.", "clinical",
      "Add the clinical capability or remove the tables. Do not drop them silently."
    )
  }
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume)) {
    triomics_export_abort("A_EXPORT_CACHE", "Resume must be TRUE or FALSE.", "resume",
                          "Request resume explicitly. A missing flag does not reuse partial output.")
  }
  if (!is.null(interrupt) && (!is.character(interrupt) || length(interrupt) != 1L || !nzchar(interrupt))) {
    triomics_export_abort("A_EXPORT_INTERRUPTED", "The interrupt token must be one string.", "interrupt",
                          "Name the artifact to stop after, or leave the token empty.")
  }

  if (file.exists(output) && !dir.exists(output)) {
    triomics_export_abort("A_OUTPUT_INVALID", "Output path exists and is not a directory.", "output",
                          "Choose a new local output directory.")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output, winslash = "/", mustWork = TRUE)
  table_dir <- file.path(output, "inputs", "tables")
  clinical_dir <- file.path(output, "inputs", "clinical")
  state_dir <- file.path(output, "state")
  export_dir <- file.path(output, "export")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  config_body <- list(
    study_id = study_id,
    design = design,
    seed = seed,
    n_subjects = n_subjects,
    block_ids = block_ids,
    n_features = as.list(n_features),
    profiles = as.list(profiles),
    capabilities = as.list(capabilities),
    preparation_context_id = context,
    operations = ops_map,
    config_token = config_token,
    display_names = as.list(display_names)
  )
  config_sha <- triomics_hash_text(triomics_canonical_json(config_body))
  simulator_design <- if (identical(design, "independent")) "two_group" else "paired"
  triomics_use_execution_context(seed, study_id)
  simulated <- simulate_study(
    n_subjects,
    n_features = n_features,
    design = simulator_design,
    effect_size = 0,
    prop_affected = 0,
    missingness = 0,
    shared_factor_strength = 0
  )
  metadata <- simulated$study$metadata
  sample_ids <- as.character(metadata$sample_id)
  subject <- if ("subject_id" %in% names(metadata)) as.character(metadata$subject_id) else rep(NA_character_, length(sample_ids))
  visit <- if ("visit_id" %in% names(metadata)) as.character(metadata$visit_id) else rep(NA_character_, length(sample_ids))
  group <- if ("group" %in% names(metadata)) as.character(metadata$group) else rep(NA_character_, length(sample_ids))
  blank <- function(values) {
    out <- as.character(values)
    out[is.na(out)] <- ""
    out
  }
  metadata_path <- file.path(table_dir, "metadata.tsv")
  triomics_export_write_tsv(
    metadata_path,
    c("sample_id", "subject_id", "visit_id", "group"),
    lapply(seq_along(sample_ids), function(index) c(sample_ids[[index]], blank(subject[[index]]), blank(visit[[index]]), blank(group[[index]])))
  )
  planted <- request$planted
  if (is.null(planted)) planted <- list()
  source_paths <- stats::setNames(vector("list", length(block_ids)), block_ids)
  for (block_id in block_ids) {
    block <- simulated$study$blocks[[block_id]]
    assay <- block$assay
    plants <- planted[[block_id]]
    if (is.null(plants)) plants <- list()
    rows <- lapply(seq_len(nrow(assay)), function(feature_index) {
      tokens <- vapply(seq_along(sample_ids), function(sample_index) {
        token <- NULL
        for (plant in plants) {
          if (identical(as.integer(plant$feature), feature_index) && identical(as.integer(plant$sample), sample_index)) {
            token <- as.character(plant$token)
          }
        }
        if (is.null(token)) triomics_export_format_number(assay[feature_index, sample_index]) else token
      }, character(1))
      c(block$feature_ids[[feature_index]], tokens)
    })
    source_paths[[block_id]] <- file.path(table_dir, paste0(block_id, ".tsv"))
    triomics_export_write_tsv(source_paths[[block_id]], c("feature_id", sample_ids), rows)
  }
  metadata_record <- triomics_source_record(
    source_id = "metadata", study_id = study_id, path = metadata_path, format = "tsv",
    role = "sample_metadata", selection_history = "full_unselected",
    selection_evidence = "Synthetic A-T08 fixture; no participant data."
  )
  measurement_records <- lapply(block_ids, function(block_id) {
    triomics_source_record(
      source_id = block_id, study_id = study_id, path = source_paths[[block_id]], format = "tsv",
      role = "measurement_matrix", selection_history = "full_unselected",
      selection_evidence = "Synthetic A-T08 fixture; no participant data."
    )
  })
  names(measurement_records) <- block_ids
  clinical_files <- list(dictionary = NULL, observations = NULL, links = NULL)
  if (wants_clinical) {
    dir.create(clinical_dir, recursive = TRUE, showWarnings = FALSE)
    clinical_files$dictionary <- file.path(clinical_dir, "dictionary.tsv")
    clinical_files$observations <- file.path(clinical_dir, "observations.tsv")
    clinical_files$links <- file.path(clinical_dir, "links.tsv")
    triomics_export_write_records(clinical_files$dictionary, clinical$dictionary)
    triomics_export_write_records(clinical_files$observations, clinical$observations)
    triomics_export_write_records(clinical_files$links, clinical$links)
    clinical_files <- lapply(clinical_files, function(path) normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  artifacts <- list()
  add_artifact <- function(entry) {
    artifacts[[length(artifacts) + 1L]] <<- entry
  }
  resolve_new <- function(role, label, cache_key, block_id, artifact_type, payload, inputs, reason_codes, extra) {
    artifact_id <- paste0(role, "-", label, "-", substr(cache_key, 1L, 20L))
    path <- file.path(export_dir, paste0(artifact_id, ".json"))
    interrupt_token <- if (is.null(block_id)) role else paste0(role, ":", block_id)
    reused <- FALSE
    if (isTRUE(resume) && file.exists(path)) {
      decision <- triomics_export_resume_decision(triomics_export_candidate(path), cache_key)
      reused <- isTRUE(decision$reuse)
    }
    if (!reused) {
      payload$cache_key <- cache_key
      payload$role <- role
      written <- triomics_export_finalize(
        payload, path, artifact_type, artifact_id, study_id, config_sha, inputs, reason_codes, interrupt, interrupt_token
      )
      payload_sha <- written$payload_sha256
      envelope_path <- written$envelope_path
    } else {
      envelope_path <- sub("\\.json$", ".envelope.json", path)
      payload_sha <- triomics_sha256(path)
      payload <- jsonlite::read_json(path, simplifyVector = FALSE)
    }
    entry <- list(
      role = role,
      block_id = block_id,
      artifact_id = artifact_id,
      cache_key = cache_key,
      payload_sha256 = payload_sha,
      envelope_path = envelope_path,
      payload_path = path,
      reused = reused,
      file_sha256 = extra$file_sha256,
      state_hash = extra$state_hash,
      state_id = extra$state_id
    )
    if (reused) {
      if (!is.null(payload$file_sha256)) entry$file_sha256 <- payload$file_sha256
      if (!is.null(payload$state_hash)) entry$state_hash <- payload$state_hash
      if (!is.null(payload$state_id)) entry$state_id <- payload$state_id
    }
    add_artifact(entry)
    list(entry = entry, payload = payload, reused = reused)
  }

  meta_key <- triomics_export_cache_key(list(
    role = "source", source_id = "metadata", version = version, config_sha256 = config_sha,
    file_sha256 = metadata_record$sha256
  ))
  resolve_new(
    "source", "metadata", meta_key, NULL, "source_manifest",
    list(source_id = "metadata", block_id = NULL, file_sha256 = metadata_record$sha256, format = "tsv",
         selection_history = "full_unselected"),
    list(), character(),
    list(file_sha256 = metadata_record$sha256, state_hash = NULL, state_id = NULL)
  )

  source_results <- list()
  for (block_id in block_ids) {
    file_sha <- measurement_records[[block_id]]$sha256
    cache_key <- triomics_export_cache_key(list(
      role = "source", source_id = block_id, version = version, config_sha256 = config_sha, file_sha256 = file_sha
    ))
    source_results[[block_id]] <- resolve_new(
      "source", block_id, cache_key, block_id, "source_manifest",
      list(source_id = block_id, block_id = block_id, file_sha256 = file_sha, format = "tsv",
           selection_history = "full_unselected", path = measurement_records[[block_id]]$local_ref),
      list(), character(),
      list(file_sha256 = file_sha, state_hash = NULL, state_id = NULL)
    )
  }

  feature_plans <- list()
  state_plans <- list()
  for (block_id in block_ids) {
    file_sha <- source_results[[block_id]]$entry$file_sha256
    feature_key <- triomics_export_cache_key(list(
      role = "feature", block_id = block_id, profile_id = profiles[[block_id]], version = version,
      config_sha256 = config_sha, preparation_context_id = context, source_file_sha256 = file_sha
    ))
    feature_id <- paste0("feature-", block_id, "-", substr(feature_key, 1L, 20L))
    feature_path <- file.path(export_dir, paste0(feature_id, ".json"))
    feature_reused <- FALSE
    feature_payload <- NULL
    feature_sha <- NULL
    if (isTRUE(resume) && file.exists(feature_path)) {
      decision <- triomics_export_resume_decision(triomics_export_candidate(feature_path), feature_key)
      feature_reused <- isTRUE(decision$reuse)
    }
    if (feature_reused) {
      feature_payload <- jsonlite::read_json(feature_path, simplifyVector = FALSE)
      feature_sha <- triomics_sha256(feature_path)
    }
    state_reused <- FALSE
    state_key <- NULL
    state_payload <- NULL
    if (feature_reused) {
      state_key <- triomics_export_cache_key(list(
        role = "state", block_id = block_id, profile_id = profiles[[block_id]], version = version,
        config_sha256 = config_sha, preparation_context_id = context, feature_payload_sha256 = feature_sha
      ))
      state_id <- paste0("state-", block_id, "-", substr(state_key, 1L, 20L))
      state_path <- file.path(export_dir, paste0(state_id, ".json"))
      if (file.exists(state_path)) {
        decision <- triomics_export_resume_decision(triomics_export_candidate(state_path), state_key)
        state_reused <- isTRUE(decision$reuse)
        if (state_reused) state_payload <- jsonlite::read_json(state_path, simplifyVector = FALSE)
      }
    }
    feature_plans[[block_id]] <- list(
      cache_key = feature_key, artifact_id = feature_id, path = feature_path, reused = feature_reused,
      payload = feature_payload, payload_sha256 = feature_sha
    )
    state_plans[[block_id]] <- list(
      cache_key = state_key, reused = state_reused, payload = state_payload
    )
  }

  prepared <- NULL
  dirty <- block_ids[!vapply(block_ids, function(block_id) {
    isTRUE(feature_plans[[block_id]]$reused) && isTRUE(state_plans[[block_id]]$reused)
  }, logical(1))]
  if (length(dirty) > 0L) {
    registry <- data.frame(
      sample_id = sample_ids,
      subject_id = subject,
      visit_id = visit,
      sample_role = rep("undeclared", length(sample_ids)),
      stringsAsFactors = FALSE
    )
    blocks <- lapply(block_ids, function(block_id) {
      list(
        block_id = block_id,
        source_id = block_id,
        feature_id_column = "feature_id",
        id_namespace = "synthetic",
        missing_tokens = c("NA", ""),
        sample_map = NULL,
        cell_mask = NULL,
        annotation_route = "none",
        technical_replicate_spec_ref = NULL,
        measurement = list(
          quantity = "intensity",
          scale = "linear",
          units = "synthetic_arbitrary_units",
          original_mask_status = "known",
          upstream_steps = character()
        )
      )
    })
    ingested <- triomics_ingest_study_tables(study_id, registry, blocks, unname(measurement_records))
    prepared <- triomics_assemble_study(ingested, alignment = "strict", display_names = display_names)
    fit_blocks <- Filter(function(block_id) !isTRUE(state_plans[[block_id]]$reused) || !isTRUE(feature_plans[[block_id]]$reused), block_ids)
    prepared <- prepare_fit(prepared, list(
      preparation_context_id = context,
      training_unit_ids = sample_ids,
      seed = seed,
      state_dir = state_dir,
      blocks = lapply(fit_blocks, function(block_id) list(
        block_id = block_id,
        profile_id = profiles[[block_id]],
        operations = ops_map[[block_id]]
      ))
    ))
  }

  assays <- stats::setNames(vector("list", length(block_ids)), block_ids)
  feature_store <- stats::setNames(vector("list", length(block_ids)), block_ids)
  state_refs <- stats::setNames(vector("list", length(block_ids)), block_ids)
  for (block_id in block_ids) {
    plan <- feature_plans[[block_id]]
    source_entry <- source_results[[block_id]]$entry
    if (!isTRUE(plan$reused)) {
      assay <- SummarizedExperiment::assay(prepared$experiments[[block_id]], "analysis")
      assay <- matrix(as.numeric(assay), nrow = nrow(assay), dimnames = dimnames(assay))
      feature_keys <- rownames(assay)
      tsv_path <- file.path(export_dir, paste0("assay-", block_id, "-", substr(plan$cache_key, 1L, 20L), ".tsv"))
      triomics_export_write_tsv(
        tsv_path,
        c("feature_key", colnames(assay)),
        lapply(seq_along(feature_keys), function(index) {
          c(feature_keys[[index]], vapply(seq_len(ncol(assay)), function(column) {
            triomics_export_format_number(assay[index, column])
          }, character(1)))
        })
      )
      values <- lapply(seq_len(nrow(assay)), function(index) {
        lapply(seq_len(ncol(assay)), function(column) {
          value <- assay[index, column]
          if (is.na(value)) NULL else as.numeric(value)
        })
      })
      payload <- list(
        block_id = block_id,
        profile_id = profiles[[block_id]],
        source_artifact_id = source_entry$artifact_id,
        source_file_sha256 = source_entry$file_sha256,
        feature_keys = feature_keys,
        sample_ids = colnames(assay),
        values = values,
        assay_tsv = tsv_path,
        assay_tsv_sha256 = triomics_sha256(tsv_path)
      )
      written <- resolve_new(
        "feature", block_id, plan$cache_key, block_id, "feature_lineage", payload,
        triomics_export_inputs(source_entry$artifact_id, source_entry$payload_sha256),
        character(),
        list(file_sha256 = NULL, state_hash = NULL, state_id = NULL)
      )
      feature_plans[[block_id]]$payload <- written$payload
      feature_plans[[block_id]]$payload_sha256 <- written$entry$payload_sha256
      feature_plans[[block_id]]$artifact_id <- written$entry$artifact_id
      assays[[block_id]] <- assay
      feature_store[[block_id]] <- list(feature_keys = feature_keys, sample_ids = colnames(assay))
    } else {
      payload <- plan$payload
      assay <- triomics_export_matrix(payload)
      assays[[block_id]] <- assay
      feature_store[[block_id]] <- list(
        feature_keys = rownames(assay),
        sample_ids = colnames(assay)
      )
      add_artifact(list(
        role = "feature",
        block_id = block_id,
        artifact_id = plan$artifact_id,
        cache_key = plan$cache_key,
        payload_sha256 = plan$payload_sha256,
        envelope_path = sub("\\.json$", ".envelope.json", plan$path),
        payload_path = plan$path,
        reused = TRUE,
        file_sha256 = NULL,
        state_hash = NULL,
        state_id = NULL
      ))
    }
    feature_entry <- artifacts[[length(artifacts)]]
    state_plan <- state_plans[[block_id]]
    if (!isTRUE(state_plan$reused)) {
      ref <- prepared$state_refs[[block_id]]
      state_key <- triomics_export_cache_key(list(
        role = "state", block_id = block_id, profile_id = profiles[[block_id]], version = version,
        config_sha256 = config_sha, preparation_context_id = context,
        feature_payload_sha256 = feature_entry$payload_sha256
      ))
      payload <- list(
        block_id = block_id,
        profile_id = profiles[[block_id]],
        state_id = ref$state_id,
        state_hash = ref$state_hash,
        state_path = ref$path,
        training_unit_ids = sample_ids,
        feature_keys = feature_store[[block_id]]$feature_keys,
        feature_artifact_id = feature_entry$artifact_id,
        feature_payload_sha256 = feature_entry$payload_sha256
      )
      written <- resolve_new(
        "state", block_id, state_key, block_id, "preparation_state", payload,
        triomics_export_inputs(feature_entry$artifact_id, feature_entry$payload_sha256),
        character(),
        list(file_sha256 = NULL, state_hash = ref$state_hash, state_id = ref$state_id)
      )
      state_refs[[block_id]] <- list(state_id = ref$state_id, state_hash = ref$state_hash, path = ref$path)
    } else {
      payload <- state_plan$payload
      add_artifact(list(
        role = "state",
        block_id = block_id,
        artifact_id = paste0("state-", block_id, "-", substr(state_plan$cache_key, 1L, 20L)),
        cache_key = state_plan$cache_key,
        payload_sha256 = triomics_sha256(file.path(export_dir, paste0("state-", block_id, "-", substr(state_plan$cache_key, 1L, 20L), ".json"))),
        envelope_path = file.path(export_dir, paste0("state-", block_id, "-", substr(state_plan$cache_key, 1L, 20L), ".envelope.json")),
        payload_path = file.path(export_dir, paste0("state-", block_id, "-", substr(state_plan$cache_key, 1L, 20L), ".json")),
        reused = TRUE,
        file_sha256 = NULL,
        state_hash = payload$state_hash,
        state_id = payload$state_id
      ))
      state_refs[[block_id]] <- list(
        state_id = payload$state_id,
        state_hash = payload$state_hash,
        path = payload$state_path
      )
    }
  }

  clinical_result <- list(
    clinical_status = if (wants_clinical) "complete" else "not_requested",
    glucose_tokens = character(),
    predictor_variable_ids = character(),
    approved_sample_ids = character(),
    objectives = list(),
    observations_sha256 = NULL,
    dictionary_sha256 = NULL,
    links_sha256 = NULL,
    observations_path = NULL
  )
  if (wants_clinical) {
    dictionary_sha <- triomics_sha256(clinical_files$dictionary)
    observations_sha <- triomics_sha256(clinical_files$observations)
    links_sha <- triomics_sha256(clinical_files$links)
    clinical_key <- triomics_export_cache_key(list(
      role = "clinical", clinical_status = "complete", version = version, config_sha256 = config_sha,
      dictionary_sha256 = dictionary_sha, observations_sha256 = observations_sha, links_sha256 = links_sha
    ))
    clinical_id <- paste0("clinical-", substr(clinical_key, 1L, 20L))
    clinical_path <- file.path(export_dir, paste0(clinical_id, ".json"))
    clinical_reused <- FALSE
    if (isTRUE(resume) && file.exists(clinical_path)) {
      decision <- triomics_export_resume_decision(triomics_export_candidate(clinical_path), clinical_key)
      clinical_reused <- isTRUE(decision$reuse)
    }
    if (clinical_reused) {
      payload <- jsonlite::read_json(clinical_path, simplifyVector = FALSE)
      add_artifact(list(
        role = "clinical", block_id = NULL, artifact_id = clinical_id, cache_key = clinical_key,
        payload_sha256 = triomics_sha256(clinical_path),
        envelope_path = sub("\\.json$", ".envelope.json", clinical_path), payload_path = clinical_path,
        reused = TRUE, file_sha256 = NULL, state_hash = NULL, state_id = NULL
      ))
      clinical_result$glucose_tokens <- triomics_export_as_chr(payload$glucose_tokens)
      clinical_result$predictor_variable_ids <- triomics_export_as_chr(payload$predictor_variable_ids)
      clinical_result$approved_sample_ids <- triomics_export_as_chr(payload$approved_sample_ids)
      clinical_result$objectives <- payload$objectives
      clinical_result$observations_sha256 <- payload$observations_sha256
      clinical_result$dictionary_sha256 <- payload$dictionary_sha256
      clinical_result$links_sha256 <- payload$links_sha256
      clinical_result$observations_path <- payload$observations_path
    } else {
      pointer <- file.path(clinical_dir, "source.txt")
      writeLines("synthetic-a-t08", pointer, useBytes = TRUE)
      config <- list(
        study_id = study_id,
        data_class = "synthetic",
        clinical = list(
          enabled = TRUE,
          dictionary_file = clinical_files$dictionary,
          observations_file = clinical_files$observations,
          links_file = clinical_files$links,
          objectives = clinical$objectives
        )
      )
      attr(config, "config_sha256") <- config_sha
      registry <- data.frame(
        sample_id = sample_ids, subject_id = subject, visit_id = visit,
        sample_role = rep("undeclared", length(sample_ids)), stringsAsFactors = FALSE
      )
      bundle <- triomics_ingest_clinical(config, pointer, registry)
      service_dir <- file.path(export_dir, "clinical-service", substr(clinical_key, 1L, 20L))
      triomics_write_clinical_artifacts(bundle, service_dir)
      observations_path <- normalizePath(file.path(service_dir, "clinical", "observations.json"), winslash = "/", mustWork = TRUE)
      tokens <- character()
      for (obs in bundle$public$observations) {
        if (identical(obs$variable_id, "glucose") && identical(obs$value_kind, "numeric")) {
          tokens <- c(tokens, obs$numeric_token)
        }
      }
      approved <- character()
      for (link in bundle$public$links) {
        if (identical(link$status, "approved") && !is.null(link$sample_id)) approved <- c(approved, link$sample_id)
      }
      clinical_result$glucose_tokens <- sort(unique(tokens))
      clinical_result$predictor_variable_ids <- as.character(bundle$predictor_variable_ids)
      clinical_result$approved_sample_ids <- sort(unique(approved))
      clinical_result$objectives <- bundle$objectives
      clinical_result$observations_sha256 <- bundle$observations_sha256
      clinical_result$dictionary_sha256 <- bundle$dictionary_sha256
      clinical_result$links_sha256 <- bundle$links_sha256
      clinical_result$observations_path <- observations_path
      payload <- list(
        clinical_status = "complete",
        glucose_tokens = clinical_result$glucose_tokens,
        predictor_variable_ids = clinical_result$predictor_variable_ids,
        approved_sample_ids = clinical_result$approved_sample_ids,
        objectives = clinical_result$objectives,
        observations_sha256 = clinical_result$observations_sha256,
        dictionary_sha256 = clinical_result$dictionary_sha256,
        links_sha256 = clinical_result$links_sha256,
        observations_path = observations_path,
        service_dir = service_dir
      )
      resolve_new(
        "clinical", substr(clinical_key, 1L, 20L), clinical_key, NULL, "clinical_lineage", payload,
        triomics_export_inputs(
          c("clinical-dictionary-source", "clinical-observations-source", "clinical-links-source"),
          c(clinical_result$dictionary_sha256, clinical_result$observations_sha256, clinical_result$links_sha256)
        ),
        character(),
        list(file_sha256 = NULL, state_hash = NULL, state_id = NULL)
      )
    }
  } else {
    clinical_key <- triomics_export_cache_key(list(
      role = "clinical", clinical_status = "not_requested", version = version, config_sha256 = config_sha
    ))
    resolve_new(
      "clinical", substr(clinical_key, 1L, 20L), clinical_key, NULL, "clinical_lineage",
      list(
        clinical_status = "not_requested",
        glucose_tokens = character(),
        predictor_variable_ids = character(),
        approved_sample_ids = character(),
        objectives = list(),
        observations_sha256 = NULL,
        observations_path = NULL
      ),
      list(), "clinical_not_requested",
      list(file_sha256 = NULL, state_hash = NULL, state_id = NULL)
    )
  }

  limitations <- triomics_a_handoff_limitations(request)
  child_ids <- vapply(artifacts, function(item) item$artifact_id, character(1))
  child_keys <- vapply(artifacts, function(item) item$cache_key, character(1))
  child_shas <- vapply(artifacts, function(item) item$payload_sha256, character(1))
  child_order <- order(child_ids)
  handoff_key <- triomics_export_cache_key(list(
    role = "handoff", version = version, config_sha256 = config_sha,
    child_ids = child_ids[child_order], child_keys = child_keys[child_order]
  ))
  handoff_written <- resolve_new(
    "handoff", substr(handoff_key, 1L, 20L), handoff_key, NULL, "a_handoff",
    list(limitations = limitations, child_artifact_ids = child_ids[child_order]),
    triomics_export_inputs(child_ids[child_order], child_shas[child_order]),
    c("source_validation_only", "scientific_validation_not_established", "optional_methods_not_qualified"),
    list(file_sha256 = NULL, state_hash = NULL, state_id = NULL)
  )

  index_ids <- vapply(artifacts, function(item) item$artifact_id, character(1))
  index_keys <- vapply(artifacts, function(item) item$cache_key, character(1))
  index_order <- order(index_ids)
  index_key <- triomics_export_cache_key(list(
    role = "index", version = version, config_sha256 = config_sha,
    child_ids = index_ids[index_order], child_keys = index_keys[index_order]
  ))
  samples <- lapply(seq_along(sample_ids), function(index) {
    present <- function(value) if (is.na(value) || !nzchar(value)) NULL else value
    list(
      sample_id = sample_ids[[index]],
      subject_id = present(subject[[index]]),
      visit_id = present(visit[[index]]),
      group = present(group[[index]])
    )
  })
  resolve_new(
    "index", substr(index_key, 1L, 20L), index_key, NULL, "a_export_index",
    list(
      study_id = study_id,
      design = design,
      simulator_design = simulator_design,
      status = "complete",
      capabilities = list(
        ingest = "complete",
        prepare = "complete",
        clinical = clinical_result$clinical_status,
        export = "complete"
      ),
      samples = samples,
      limitations = limitations
    ),
    triomics_export_inputs(handoff_written$entry$artifact_id, handoff_written$entry$payload_sha256),
    character(),
    list(file_sha256 = NULL, state_hash = NULL, state_id = NULL)
  )

  invisible(list(
    status = "complete",
    study_id = study_id,
    design = design,
    simulator_design = simulator_design,
    seed = seed,
    output = output,
    cache_key = index_key,
    version = version,
    state_dir = state_dir,
    capabilities = list(
      ingest = "complete",
      prepare = "complete",
      clinical = clinical_result$clinical_status,
      export = "complete"
    ),
    samples = samples,
    assays = assays,
    features = feature_store,
    state_refs = state_refs,
    clinical = clinical_result,
    files = list(source = source_paths, clinical_observations = clinical_files$observations),
    artifacts = artifacts,
    handoff_path = handoff_written$entry$payload_path
  ))
}
