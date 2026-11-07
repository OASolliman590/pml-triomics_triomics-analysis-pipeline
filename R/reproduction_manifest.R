triomics_reproduction_abort <- function(code, message, key) {
  triomics_abort(
    code, message, key = key, module = "reproduction", stage = "report",
    remediation = "Keep execution evidence separate from scientific and packet acceptance; repair the cited local evidence and retry."
  )
}

triomics_reproduction_authority_file <- function(root, relative_path) {
  path <- file.path(root, relative_path)
  if (!file.exists(path) || dir.exists(path) || triomics_is_symlink(path)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme record is missing or is not a regular file.",
      relative_path
    )
  }
  size <- file.info(path)$size
  if (is.na(size) || size < 0 || size > .Machine$integer.max) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme record has an invalid byte length.", relative_path
    )
  }
  bytes <- tryCatch(readBin(path, what = "raw", n = as.integer(size)), error = function(error) NULL)
  before_hash <- tryCatch(triomics_sha256(path), error = function(error) NULL)
  after_hash <- tryCatch(triomics_sha256(path), error = function(error) NULL)
  bytes_hash <- if (is.raw(bytes)) {
    digest::digest(bytes, algo = "sha256", serialize = FALSE)
  } else {
    NULL
  }
  if (is.null(bytes) || length(bytes) != size || !is.character(before_hash) ||
      length(before_hash) != 1L || !identical(before_hash, after_hash) ||
      !identical(before_hash, bytes_hash)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme record changed during byte read-back.", relative_path
    )
  }
  value <- tryCatch(
    jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (!is.list(value) || is.null(names(value))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme record is not a JSON object.", relative_path
    )
  }
  list(
    value = value,
    reference = list(path = relative_path, sha256 = before_hash, size_bytes = as.integer(size))
  )
}

triomics_reproduction_record_ids <- function(records, field, source_key) {
  if (!is.list(records) || !length(records)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme inventory is empty or malformed.", source_key
    )
  }
  ids <- vapply(records, function(record) {
    value <- if (is.list(record)) record[[field]] else NULL
    if (is.null(value) || length(value) != 1L || is.na(value)) return(NA_character_)
    as.character(value)
  }, character(1L))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme inventory has missing, blank, or duplicate identities.",
      source_key
    )
  }
  ids
}

triomics_reproduction_reference_ids <- function(value, key) {
  if (is.null(value)) return(character())
  ids <- unlist(value, recursive = TRUE, use.names = FALSE)
  ids <- as.character(ids)
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "An authoritative programme record has missing or duplicate references.", key
    )
  }
  ids
}

triomics_reproduction_authority_snapshot <- function(project_root) {
  if (!is.character(project_root) || length(project_root) != 1L || is.na(project_root) ||
      !nzchar(project_root) || !dir.exists(project_root) || triomics_is_symlink(project_root)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "The programme evidence root is unavailable or unsafe.", "project_root"
    )
  }
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  packet_file <- triomics_reproduction_authority_file(
    root, file.path("discovery", "spec-v2", "packets.json")
  )
  requirement_file <- triomics_reproduction_authority_file(
    root, file.path("discovery", "spec-v2", "requirements.json")
  )
  ledger_file <- triomics_reproduction_authority_file(
    root, file.path("docs", "implementation", "execution-ledger.json")
  )
  packet_spec <- packet_file$value
  requirement_spec <- requirement_file$value
  ledger <- ledger_file$value
  packet_ids <- triomics_reproduction_record_ids(
    packet_spec$packets, "id", "discovery/spec-v2/packets.json"
  )
  requirement_ids <- triomics_reproduction_record_ids(
    requirement_spec$requirements, "id", "discovery/spec-v2/requirements.json"
  )
  if (!is.numeric(packet_spec$packet_count) || length(packet_spec$packet_count) != 1L ||
      is.na(packet_spec$packet_count) || packet_spec$packet_count != length(packet_ids) ||
      !is.list(ledger$packets) || is.null(names(ledger$packets)) ||
      any(!nzchar(names(ledger$packets))) || anyDuplicated(names(ledger$packets)) ||
      length(ledger$packets) != length(packet_ids) || !setequal(names(ledger$packets), packet_ids)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_AUTHORITY_INVALID",
      "The frozen packet inventory and saved execution ledger have missing or unknown identities.",
      "packets"
    )
  }

  packet_requirements <- stats::setNames(vector("list", length(packet_ids)), packet_ids)
  for (index in seq_along(packet_ids)) {
    packet <- packet_spec$packets[[index]]
    refs <- triomics_reproduction_reference_ids(
      packet$requirements, paste0("packets.", packet_ids[[index]], ".requirements")
    )
    if (any(!refs %in% requirement_ids)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_AUTHORITY_INVALID",
        "A packet references a requirement absent from the frozen requirement inventory.",
        paste0("packets.", packet_ids[[index]], ".requirements")
      )
    }
    packet_requirements[[packet_ids[[index]]]] <- sort(refs, method = "radix")
  }
  for (index in seq_along(requirement_ids)) {
    requirement <- requirement_spec$requirements[[index]]
    refs <- triomics_reproduction_reference_ids(
      requirement$packets, paste0("requirements.", requirement_ids[[index]], ".packets")
    )
    if (any(!refs %in% packet_ids)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_AUTHORITY_INVALID",
        "A requirement references a packet absent from the frozen packet inventory.",
        paste0("requirements.", requirement_ids[[index]], ".packets")
      )
    }
    for (packet_id in refs) {
      if (!requirement_ids[[index]] %in% packet_requirements[[packet_id]]) {
        triomics_reproduction_abort(
          "D_REPRODUCTION_AUTHORITY_INVALID",
          "Packet and requirement records disagree about their identity mapping.",
          paste0("requirements.", requirement_ids[[index]], ".packets")
        )
      }
    }
  }
  for (packet_id in packet_ids) {
    for (requirement_id in packet_requirements[[packet_id]]) {
      declared <- triomics_reproduction_reference_ids(
        requirement_spec$requirements[[match(requirement_id, requirement_ids)]]$packets,
        paste0("requirements.", requirement_id, ".packets")
      )
      if (!packet_id %in% declared) {
        triomics_reproduction_abort(
          "D_REPRODUCTION_AUTHORITY_INVALID",
          "Packet and requirement records disagree about their identity mapping.",
          paste0("packets.", packet_id, ".requirements")
        )
      }
    }
  }

  conditional_ids <- vapply(packet_spec$packets, function(packet) {
    gate <- packet$conditional_gate
    isTRUE(identical(as.character(packet$scope_type %||% ""), "conditional")) ||
      (!is.null(gate) && length(gate) == 1L && !is.na(gate) && nzchar(as.character(gate)))
  }, logical(1L))
  packet_id_hash <- triomics_hash_text(paste(sort(packet_ids, method = "radix"), collapse = "\n"))
  requirement_id_hash <- triomics_hash_text(paste(sort(requirement_ids, method = "radix"), collapse = "\n"))
  list(
    schema_version = "2.0",
    status = "pending_sol_reconciliation",
    packet_acceptance = "pending_sol_acceptance",
    publication_status = "pending_sol_reconciliation",
    real_data_validation_status = "not_included_in_synthetic_walkthrough",
    packet_count = as.integer(length(packet_ids)),
    requirement_count = as.integer(length(requirement_ids)),
    ledger_packet_count = as.integer(length(ledger$packets)),
    conditional_scope_count = as.integer(sum(conditional_ids)),
    packet_ids_sha256 = packet_id_hash,
    requirement_ids_sha256 = requirement_id_hash,
    authorities = list(
      packet_registry = packet_file$reference,
      requirement_registry = requirement_file$reference,
      execution_ledger = ledger_file$reference
    )
  )
}

triomics_reproduction_installed_resource <- function(...) {
  resource <- system.file(..., package = "triomics")
  if (!nzchar(resource) || !file.exists(resource) || dir.exists(resource)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "A required public synthetic package resource is unavailable.",
      paste(c(...), collapse = "/")
    )
  }
  normalizePath(resource, winslash = "/", mustWork = TRUE)
}

triomics_reproduction_set_null <- function(value, name) {
  value[name] <- list(NULL)
  value
}

triomics_reproduction_write_tsv <- function(value, path) {
  tryCatch(
    utils::write.table(
      value, path, sep = "\t", quote = FALSE, row.names = FALSE,
      col.names = TRUE, na = "NA", fileEncoding = "UTF-8"
    ),
    error = function(error) triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "A public synthetic fixture table could not be written.", basename(path)
    )
  )
  invisible(path)
}

triomics_reproduction_materialize_fixture <- function(design, root) {
  if (!is.character(design) || length(design) != 1L || is.na(design) ||
      !design %in% c("paired", "independent") ||
      !is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root) ||
      file.exists(root) || dir.exists(root)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "Fixture materialization needs a supported design and a new local root.",
      "fixture"
    )
  }
  if (!dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "The public synthetic fixture root could not be created.", "fixture.root"
    )
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  tickets_root <- file.path(root, "tickets")
  data_root <- file.path(root, "data", design)
  if (!dir.create(tickets_root, recursive = TRUE, showWarnings = FALSE) ||
      !dir.create(data_root, recursive = TRUE, showWarnings = FALSE)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "The fixture ticket or data directory could not be created.", "fixture.root"
    )
  }
  template_path <- triomics_reproduction_installed_resource("examples", "tickets", "D-T09.yaml")
  resource_root <- triomics_reproduction_installed_resource("examples", "data", design, "metadata.tsv")
  resource_root <- dirname(resource_root)
  assay_names <- if (identical(design, "paired")) {
    c("proteomics.tsv")
  } else {
    c("proteomics.tsv", "metabolomics.tsv")
  }
  copied <- file.copy(
    file.path(resource_root, c("metadata.tsv", assay_names)),
    file.path(data_root, c("metadata.tsv", assay_names)),
    overwrite = FALSE
  )
  if (!all(copied)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "The public synthetic assay fixture could not be copied.", "fixture.inputs"
    )
  }
  metadata_path <- file.path(data_root, "metadata.tsv")
  metadata <- tryCatch(
    utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8"),
    error = function(error) NULL
  )
  if (!is.data.frame(metadata) || !"sample_id" %in% names(metadata) ||
      anyNA(metadata$sample_id) || anyDuplicated(metadata$sample_id)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "The source fixture has an invalid sample identity map.", "fixture.metadata"
    )
  }
  if (identical(design, "paired")) {
    if (!all(c("subject", "visit") %in% names(metadata)) ||
        nrow(metadata) != 12L || length(unique(metadata$subject)) != 6L) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_FIXTURE_INVALID",
        "The paired source fixture does not contain six complete subject pairs.",
        "fixture.metadata"
      )
    }
    subject_index <- match(metadata$subject, unique(metadata$subject))
    metadata$batch <- ifelse(subject_index %% 2L == 1L, "batch_a", "batch_b")
    protein_path <- file.path(data_root, "proteomics.tsv")
    protein <- utils::read.delim(protein_path, check.names = FALSE,
                                 stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    if (!"feature_id" %in% names(protein) ||
        !setequal(names(protein)[-1L], metadata$sample_id)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_FIXTURE_INVALID",
        "The paired assay columns do not match the declared sample identities.",
        "fixture.assay"
      )
    }
    values <- as.matrix(protein[, -1L, drop = FALSE])
    storage.mode(values) <- "double"
    derived <- 25 + 1.25 * values + matrix(rep(seq_len(nrow(values)) / 100,
                                               ncol(values)), nrow(values))
    companion <- data.frame(
      feature_id = paste0("SYN_MET_", as.character(protein$feature_id)),
      as.data.frame(derived, check.names = FALSE), check.names = FALSE
    )
    names(companion) <- c("feature_id", names(protein)[-1L])
    triomics_reproduction_write_tsv(companion, file.path(data_root, "metabolomics.tsv"))
  } else {
    if (!"group" %in% names(metadata) || nrow(metadata) != 12L ||
        anyDuplicated(metadata$sample_id)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_FIXTURE_INVALID",
        "The independent source fixture does not contain twelve unique samples.",
        "fixture.metadata"
      )
    }
    metadata$batch <- rep(c("batch_a", "batch_b"), length.out = nrow(metadata))
    for (name in assay_names) {
      assay <- utils::read.delim(file.path(data_root, name), check.names = FALSE,
                                 stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      if (!"feature_id" %in% names(assay) ||
          !setequal(names(assay)[-1L], metadata$sample_id)) {
        triomics_reproduction_abort(
          "D_REPRODUCTION_FIXTURE_INVALID",
          "The independent assay columns do not match the declared sample identities.",
          "fixture.assay"
        )
      }
    }
  }
  triomics_reproduction_write_tsv(metadata, metadata_path)

  config <- tryCatch(
    yaml::yaml.load_file(
      template_path,
      handlers = list(seq = function(value) as.list(value)),
      eval.expr = FALSE
    ),
    error = function(error) NULL
  )
  if (!is.list(config) || is.null(config$analyses) || length(config$analyses) != 1L ||
      length(config$blocks) != 2L || length(config$sources) != 3L) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "The standalone D-T09 YAML fixture has an unexpected structure.", "fixture.config"
    )
  }
  study_id <- paste0("triomics_d_t09_", design, "_fixture")
  config$study_id <- study_id
  relative_dir <- paste0("../data/", design, "/")
  config$sources[[1L]]$file <- paste0(relative_dir, "metadata.tsv")
  config$metadata$file <- paste0(relative_dir, "metadata.tsv")
  config$sources[[2L]]$file <- paste0(relative_dir, "proteomics.tsv")
  config$blocks[[1L]]$file <- paste0(relative_dir, "proteomics.tsv")
  config$sources[[3L]]$file <- paste0(relative_dir, "metabolomics.tsv")
  config$blocks[[2L]]$file <- paste0(relative_dir, "metabolomics.tsv")
  config$metadata$column_types$batch <- "factor"
  config$metadata$factor_levels$batch <- c("batch_a", "batch_b")
  analysis <- config$analyses[[1L]]
  if (identical(design, "independent")) {
    config$metadata <- triomics_reproduction_set_null(config$metadata, "subject_id")
    config$metadata$column_types <- list(
      sample_id = "string", group = "factor", batch = "factor"
    )
    config$metadata$factor_levels <- list(
      group = c("control", "treatment"), batch = c("batch_a", "batch_b")
    )
    analysis$design$kind <- "independent"
    analysis$design$condition <- "group"
    analysis$design$condition_type <- "factor"
    analysis$design <- triomics_reproduction_set_null(analysis$design, "subject_id")
    analysis$design$repeated <- FALSE
    analysis$design$representation <- "levels"
    analysis$design$reference_level <- "control"
    analysis$design$target_level <- "treatment"
    for (field in c("visit_column", "reference_visit", "target_visit")) {
      analysis$design <- triomics_reproduction_set_null(analysis$design, field)
    }
    analysis$statistics$contrasts[[1L]]$contrast_id <- "target_minus_reference"
    analysis$statistics$contrasts[[1L]]$weights <- list(
      `condition.control` = -1, `condition.treatment` = 1
    )
    analysis$statistics$contrasts[[1L]]$interpretation <-
      "Synthetic independent target-minus-reference software example only."
    analysis$statistics$effect_unit <- "group_difference"
    analysis$integration[[1L]]$evaluation$generalization_unit <- "sample"
  } else {
    config$metadata$subject_id <- "subject"
    config$metadata$column_types <- list(
      sample_id = "string", subject = "string", visit = "factor", batch = "factor"
    )
    config$metadata$factor_levels <- list(
      visit = c("baseline", "followup"), batch = c("batch_a", "batch_b")
    )
    analysis$design$kind <- "paired"
    analysis$design$condition <- "visit"
    analysis$design$condition_type <- "factor"
    analysis$design$subject_id <- "subject"
    analysis$design$repeated <- TRUE
    analysis$design$representation <- "delta"
    analysis$design$reference_level <- "baseline"
    analysis$design$target_level <- "followup"
    analysis$design$visit_column <- "visit"
    analysis$design$reference_visit <- "baseline"
    analysis$design$target_visit <- "followup"
    analysis$statistics$contrasts[[1L]]$weights <- list(intercept = 1)
    analysis$statistics$contrasts[[1L]]$interpretation <-
      "Synthetic paired target-minus-reference software example only."
    analysis$statistics$effect_unit <- "delta_difference"
    analysis$integration[[1L]]$evaluation$generalization_unit <- "subject"
  }
  analysis$analysis_id <- paste0("d_t09_", design, "_synthetic")
  config$analyses[[1L]] <- analysis
  config_path <- file.path(tickets_root, "D-T09.yaml")
  tryCatch(
    yaml::write_yaml(config, config_path),
    error = function(error) triomics_reproduction_abort(
      "D_REPRODUCTION_FIXTURE_INVALID",
      "The frozen D-T09 configuration could not be written.", "fixture.config"
    )
  )
  parsed <- triomics_read_config(config_path)
  triomics_validate_config_paths(parsed, config_path)
  input_paths <- c(
    file.path(data_root, "metadata.tsv"),
    file.path(data_root, assay_names),
    if (identical(design, "paired")) file.path(data_root, "metabolomics.tsv")
  )
  input_paths <- unique(input_paths)
  inputs <- lapply(input_paths, function(path) {
    list(
      path = file.path("data", design, basename(path)),
      sha256 = triomics_sha256(path),
      size_bytes = as.integer(file.info(path)$size)
    )
  })
  names(inputs) <- vapply(input_paths, basename, character(1L))
  list(
    design = design,
    study_id = study_id,
    root = root,
    config_path = normalizePath(config_path, winslash = "/", mustWork = TRUE),
    config_sha256 = as.character(attr(parsed, "config_sha256")),
    inputs = inputs
  )
}

triomics_reproduction_evidence_root <- function() {
  root <- Sys.getenv("TRIOMICS_PACKET_EVIDENCE_ROOT", unset = "")
  if (!nzchar(root)) root <- tempdir()
  if (file.exists(root) && !dir.exists(root)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The packet evidence destination is not a directory.", "evidence_root"
    )
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(root) || triomics_is_symlink(root)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The packet evidence destination is unavailable or unsafe.", "evidence_root"
    )
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

triomics_reproduction_safe_file <- function(root, relative_path, must_exist = TRUE) {
  if (!is.character(root) || length(root) != 1L || is.na(root) || !dir.exists(root) ||
      !is.character(relative_path) || length(relative_path) != 1L ||
      is.na(relative_path) || !nzchar(relative_path) ||
      grepl("^(?:/|[A-Za-z]:[/\\\\])", relative_path) || grepl("\\\\", relative_path)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_PATH_UNSAFE",
      "A reproduction evidence path is missing or unsafe.", "path"
    )
  }
  parts <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
  if (!length(parts) || any(!nzchar(parts)) || any(parts %in% c(".", ".."))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_PATH_UNSAFE",
      "A reproduction evidence path escapes its local root.", "path"
    )
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  current <- root
  for (index in seq_along(parts)) {
    current <- file.path(current, parts[[index]])
    if (triomics_is_symlink(current)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_PATH_UNSAFE",
        "A reproduction evidence path crosses a symbolic link.", "path"
      )
    }
    if (index < length(parts) && !dir.exists(current)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_SOURCE_INVALID",
        "A required reproduction evidence directory is missing.", "path"
      )
    }
  }
  if (isTRUE(must_exist) && (!file.exists(current) || dir.exists(current))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "A required reproduction evidence file is missing.", relative_path
    )
  }
  if (file.exists(current)) {
    resolved <- normalizePath(current, winslash = "/", mustWork = TRUE)
    prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
    if (!startsWith(resolved, prefix)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_PATH_UNSAFE",
        "A reproduction evidence path escapes its local root.", "path"
      )
    }
    return(resolved)
  }
  current
}

triomics_reproduction_file_reference <- function(root, relative_path, expected_sha256 = NULL,
                                                  expected_size = NULL) {
  path <- triomics_reproduction_safe_file(root, relative_path)
  size <- file.info(path)$size
  if (is.na(size) || size < 0 || size > .Machine$integer.max ||
      (!is.null(expected_size) && !identical(as.integer(size), as.integer(expected_size)))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "A reproduction evidence file has an invalid or changed byte length.", relative_path
    )
  }
  bytes <- tryCatch(readBin(path, what = "raw", n = as.integer(size)), error = function(error) NULL)
  before_hash <- tryCatch(triomics_sha256(path), error = function(error) NULL)
  after_hash <- tryCatch(triomics_sha256(path), error = function(error) NULL)
  bytes_hash <- if (is.raw(bytes)) digest::digest(bytes, algo = "sha256", serialize = FALSE) else NULL
  if (is.null(bytes) || length(bytes) != size || !is.character(before_hash) ||
      length(before_hash) != 1L || !identical(before_hash, after_hash) ||
      !identical(before_hash, bytes_hash) ||
      (!is.null(expected_sha256) && !identical(before_hash, expected_sha256))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "A reproduction evidence file changed or failed byte read-back.", relative_path
    )
  }
  list(path = relative_path, sha256 = before_hash, size_bytes = as.integer(size))
}

triomics_reproduction_validate_fixture <- function(fixture) {
  if (!is.list(fixture) || !is.character(fixture$root) || length(fixture$root) != 1L ||
      !dir.exists(fixture$root) || !is.character(fixture$config_path) ||
      length(fixture$config_path) != 1L || !is.list(fixture$inputs) ||
      !length(fixture$inputs) || !triomics_sha256_text(fixture$config_sha256)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The materialized fixture identity is incomplete.", "fixture"
    )
  }
  root <- normalizePath(fixture$root, winslash = "/", mustWork = TRUE)
  config_relative <- if (startsWith(normalizePath(fixture$config_path, winslash = "/", mustWork = TRUE),
                                    paste0(root, "/"))) {
    substring(normalizePath(fixture$config_path, winslash = "/", mustWork = TRUE), nchar(root) + 2L)
  } else {
    ""
  }
  if (!nzchar(config_relative)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The materialized configuration is outside its fixture root.", "fixture.config"
    )
  }
  config_reference <- triomics_reproduction_file_reference(
    root, config_relative, fixture$config_sha256
  )
  config <- tryCatch(triomics_read_config(fixture$config_path), error = function(error) NULL)
  if (!is.list(config) || !identical(as.character(attr(config, "config_sha256")),
                                     fixture$config_sha256)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The materialized configuration changed or no longer validates.", "fixture.config"
    )
  }
  for (input in fixture$inputs) {
    if (!is.list(input) || !triomics_sha256_text(input$sha256) ||
        !is.character(input$path) || length(input$path) != 1L ||
        !is.numeric(input$size_bytes) || length(input$size_bytes) != 1L) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_SOURCE_INVALID",
        "A materialized input identity is incomplete.", "fixture.inputs"
      )
    }
    triomics_reproduction_file_reference(
      root, input$path, input$sha256, input$size_bytes
    )
  }
  list(config = config, config_reference = config_reference)
}

triomics_reproduction_find_project_root <- function(start = getwd()) {
  if (!is.character(start) || length(start) != 1L || is.na(start) || !nzchar(start)) {
    start <- getwd()
  }
  candidate <- normalizePath(start, winslash = "/", mustWork = TRUE)
  if (!dir.exists(candidate)) candidate <- dirname(candidate)
  for (attempt in seq_len(12L)) {
    if (file.exists(file.path(candidate, "discovery", "spec-v2", "packets.json")) &&
        file.exists(file.path(candidate, "docs", "implementation", "execution-ledger.json"))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  triomics_reproduction_abort(
    "D_REPRODUCTION_AUTHORITY_INVALID",
    "The project authority root could not be found from the walkthrough source.",
    "project_root"
  )
}

triomics_reproduction_installed_identity <- function() {
  evidence_root <- triomics_reproduction_evidence_root()
  script_path <- tempfile("triomics-d09-installed-identity-", tmpdir = evidence_root,
                          fileext = ".R")
  writeLines(c(
    "stamp <- system.file('triomics-source-revision', package = 'triomics')",
    "launcher <- system.file('exec', 'triomics', package = 'triomics')",
    "if (!nzchar(stamp) || !file.exists(stamp) || !nzchar(launcher) || !file.exists(launcher)) quit(status = 2L)",
    "revision <- paste(readLines(stamp, warn = FALSE), collapse = '')",
    "identity <- list(package = 'triomics', version = as.character(utils::packageVersion('triomics')), source_revision = revision, launcher = 'exec/triomics')",
    "cat('D09_INSTALLED_IDENTITY:', as.character(jsonlite::toJSON(identity, auto_unbox = TRUE)), '\\n', sep = '')"
  ), script_path, useBytes = TRUE)
  command <- file.path(R.home("bin"), "Rscript")
  output <- tryCatch(
    system2(command, c("--vanilla", shQuote(script_path)), stdout = TRUE, stderr = TRUE),
    error = function(error) structure(conditionMessage(error), status = 127L)
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  line <- grep("^D09_INSTALLED_IDENTITY:", output, value = TRUE)
  if (!identical(as.integer(status), 0L) || length(line) != 1L) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_INSTALLED_UNQUALIFIED",
      "The installed package identity could not be read back in a clean R process.",
      "installed.package"
    )
  }
  identity <- tryCatch(
    jsonlite::fromJSON(sub("^D09_INSTALLED_IDENTITY:", "", line[[1L]])),
    error = function(error) NULL
  )
  if (!is.list(identity) || !identical(as.character(identity$package), "triomics") ||
      !grepl("^[0-9a-f]{40}$", as.character(identity$source_revision)) ||
      !identical(as.character(identity$launcher), "exec/triomics")) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_INSTALLED_UNQUALIFIED",
      "The installed package has no resolved archive source revision or installed launcher.",
      "installed.package"
    )
  }
  list(
    package = "triomics",
    version = as.character(identity$version),
    source_revision = as.character(identity$source_revision),
    launcher = "exec/triomics"
  )
}

triomics_reproduction_runtime <- function(installed) {
  package_names <- c("testthat", "MOFA2", "DEqMS", "knitr", "rmarkdown")
  package_versions <- stats::setNames(lapply(package_names, function(name) {
    version <- tryCatch(as.character(utils::packageVersion(name)), error = function(error) NA_character_)
    if (is.na(version)) "unavailable" else version
  }), package_names)
  quarto_command <- Sys.getenv("QUARTO_PATH", unset = "quarto")
  quarto_output <- tryCatch(
    system2(quarto_command, "--version", stdout = TRUE, stderr = TRUE),
    error = function(error) character()
  )
  quarto_status <- attr(quarto_output, "status")
  quarto_version <- if (!is.null(quarto_status) && !identical(as.integer(quarto_status), 0L)) {
    "unavailable"
  } else {
    trimws(paste(quarto_output, collapse = " "))
  }
  list(
    R = as.character(getRversion()),
    package = installed,
    package_versions = package_versions,
    Quarto = if (nzchar(quarto_version)) quarto_version else "unavailable"
  )
}

triomics_reproduction_run_installed <- function(fixture, output_root, project_root = NULL) {
  fixture_state <- triomics_reproduction_validate_fixture(fixture)
  if (!is.character(output_root) || length(output_root) != 1L || is.na(output_root) ||
      !nzchar(output_root) || file.exists(output_root) || dir.exists(output_root)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The installed walkthrough needs a new output root.", "output_root"
    )
  }
  evidence_root <- triomics_reproduction_evidence_root()
  output_parent <- normalizePath(dirname(output_root), winslash = "/", mustWork = TRUE)
  evidence_prefix <- if (endsWith(evidence_root, "/")) evidence_root else paste0(evidence_root, "/")
  if (!startsWith(paste0(output_parent, "/"), evidence_prefix)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "Pipeline output must stay inside the packet's local evidence root.", "output_root"
    )
  }
  project_root <- if (is.null(project_root)) {
    triomics_reproduction_find_project_root()
  } else {
    triomics_reproduction_find_project_root(project_root)
  }
  launcher <- system.file("exec", "triomics", package = "triomics")
  if (!nzchar(launcher) || !file.exists(launcher) || dir.exists(launcher) ||
      triomics_is_symlink(launcher)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_INSTALLED_UNQUALIFIED",
      "The archive-installed package launcher is missing or unsafe.",
      "installed.launcher"
    )
  }
  launcher <- normalizePath(launcher, winslash = "/", mustWork = TRUE)
  launcher_root <- normalizePath(dirname(dirname(launcher)), winslash = "/", mustWork = TRUE)
  installed <- triomics_reproduction_installed_identity()
  if (!dir.create(output_root, recursive = TRUE, showWarnings = FALSE)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The installed walkthrough output root could not be created.", "output_root"
    )
  }
  output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
  previous_wd <- getwd()
  on.exit(setwd(previous_wd), add = TRUE)
  setwd(launcher_root)
  rscript <- file.path(R.home("bin"), "Rscript")
  stages <- list()
  stage_names <- c("A", "B", "C", "D", "report")
  for (stage in stage_names) {
    arguments <- c(
      "--vanilla", file.path("exec", "triomics"), "--config", shQuote(fixture$config_path),
      "--stage", stage, "--output", shQuote(output_root)
    )
    output <- tryCatch(
      system2(rscript, arguments, stdout = TRUE, stderr = TRUE),
      error = function(error) structure(conditionMessage(error), status = 127L)
    )
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    status <- as.integer(status)
    log_root <- file.path(evidence_root, "installed-cli-logs")
    dir.create(log_root, recursive = TRUE, showWarnings = FALSE)
    log_path <- file.path(log_root, paste0(basename(output_root), "-", stage, ".txt"))
    writeLines(output, log_path, useBytes = TRUE)
    warnings <- grep("Warning", output, value = TRUE, ignore.case = TRUE)
    stages[[stage]] <- list(
      stage = stage,
      status = if (identical(status, 0L)) "complete" else "failed",
      exit_code = status,
      command = paste(
        "Rscript --vanilla exec/triomics --config <fixture>/tickets/D-T09.yaml",
        "--stage", stage, "--output <packet-evidence-root>"
      ),
      warning_count = as.integer(length(warnings)),
      log_path = file.path("installed-cli-logs", basename(log_path))
    )
    if (!identical(status, 0L)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_EXECUTION_FAILED",
        paste0("Installed stage ", stage, " returned exit code ", status, "."),
        paste0("stage.", stage)
      )
    }
    triomics_reproduction_validate_fixture(fixture)
  }
  authority <- triomics_reproduction_authority_snapshot(project_root)
  run <- list(
    fixture = fixture,
    output_root = output_root,
    project_root = project_root,
    installed = installed,
    stages = stages,
    authority = authority
  )
  published <- triomics_reproduction_manifest(run)
  run$manifest <- published$manifest
  run$manifest_path <- published$path
  run$manifest_byte_sha256 <- published$byte_sha256
  run
}

triomics_reproduction_read_json <- function(root, relative_path) {
  reference <- triomics_reproduction_file_reference(root, relative_path)
  path <- triomics_reproduction_safe_file(root, relative_path)
  bytes <- tryCatch(readBin(path, what = "raw", n = reference$size_bytes),
                    error = function(error) NULL)
  value <- if (is.raw(bytes)) {
    tryCatch(jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE),
             error = function(error) NULL)
  } else NULL
  triomics_reproduction_file_reference(root, relative_path, reference$sha256,
                                       reference$size_bytes)
  if (!is.list(value) || is.null(names(value))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "A saved reproduction output is not a JSON object.", relative_path
    )
  }
  list(value = value, reference = reference)
}

triomics_reproduction_remote_reference <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) return(FALSE)
  value <- trimws(value)
  value <- gsub("[\\r\\n\\t]", "", value, perl = TRUE)
  value <- gsub("&#0*58;?", ":", value, ignore.case = TRUE, perl = TRUE)
  value <- gsub("&#x0*3a;?", ":", value, ignore.case = TRUE, perl = TRUE)
  value <- gsub("&#0*47;?", "/", value, ignore.case = TRUE, perl = TRUE)
  value <- gsub("&#x0*2f;?", "/", value, ignore.case = TRUE, perl = TRUE)
  grepl("^(?:https?:)?//", value, ignore.case = TRUE, perl = TRUE)
}

triomics_reproduction_html_attributes <- function(value) {
  matches <- gregexpr(
    "[[:alpha:]_:][[:alnum:]_.:-]*\\s*=\\s*(?:\"[^\"]*\"|'[^']*'|[^\\s>]+)",
    value, ignore.case = TRUE, perl = TRUE
  )
  attributes <- regmatches(value, matches)[[1L]]
  if (!length(attributes)) return(list())
  output <- list()
  for (attribute in attributes) {
    equals <- regexpr("=", attribute, fixed = TRUE)[[1L]]
    if (equals < 1L) next
    name <- tolower(trimws(substr(attribute, 1L, equals - 1L)))
    content <- trimws(substr(attribute, equals + 1L, nchar(attribute)))
    if (nchar(content) >= 2L && substr(content, 1L, 1L) %in% c("'", "\"") &&
        identical(substr(content, nchar(content), nchar(content)), substr(content, 1L, 1L))) {
      content <- substr(content, 2L, nchar(content) - 1L)
    } else {
      content <- sub("/+$", "", content)
    }
    output[[length(output) + 1L]] <- list(name = name, value = content)
  }
  output
}

triomics_reproduction_css_remote <- function(css) {
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)
  url_pattern <- "(?is)url\\(\\s*(?:\"[^\"]*\"|'[^']*'|[^)]*)\\s*\\)"
  url_matches <- gregexpr(url_pattern, css, perl = TRUE)
  urls <- regmatches(css, url_matches)[[1L]]
  if (length(urls)) {
    values <- vapply(urls, function(url) {
      value <- sub("(?is)^url\\(\\s*", "", url, perl = TRUE)
      value <- sub("\\s*\\)$", "", value, perl = TRUE)
      if (nchar(value) >= 2L && substr(value, 1L, 1L) %in% c("'", "\"") &&
          identical(substr(value, nchar(value), nchar(value)), substr(value, 1L, 1L))) {
        value <- substr(value, 2L, nchar(value) - 1L)
      }
      value
    }, character(1L))
    if (any(vapply(values, triomics_reproduction_remote_reference, logical(1L)))) return(TRUE)
  }
  import_pattern <- "(?is)@import\\s+(?:layer\\([^)]*\\)\\s*)?(?:supports\\([^)]*\\)\\s*)?(['\"])(.*?)\\1"
  imports <- gregexpr(import_pattern, css, perl = TRUE)
  import_matches <- regmatches(css, imports)[[1L]]
  if (!length(import_matches)) return(FALSE)
  import_values <- vapply(import_matches, function(item) {
    value <- sub("(?is)^@import\\s+", "", item, perl = TRUE)
    value <- sub("(?is)^(?:layer\\([^)]*\\)\\s*)?(?:supports\\([^)]*\\)\\s*)?", "", value, perl = TRUE)
    if (nchar(value) >= 2L) substr(value, 2L, nchar(value) - 1L) else value
  }, character(1L))
  any(vapply(import_values, triomics_reproduction_remote_reference, logical(1L)))
}

triomics_reproduction_offline_html <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size < 0 || size > .Machine$integer.max) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_RENDER_INVALID",
      "The rendered report has an invalid byte length.", "report.html"
    )
  }
  bytes <- readBin(path, what = "raw", n = as.integer(size))
  html <- gsub("(?is)<!--.*?-->", "", rawToChar(bytes), perl = TRUE)
  style_matches <- gregexpr("(?is)<style\\b[^>]*>.*?</style\\s*>", html, perl = TRUE)
  style_blocks <- regmatches(html, style_matches)[[1L]]
  style_bodies <- vapply(style_blocks, function(block) {
    body <- sub("(?is)^<style\\b[^>]*>", "", block, perl = TRUE)
    sub("(?is)</style\\s*>$", "", body, perl = TRUE)
  }, character(1L))
  if (length(style_bodies) && any(vapply(style_bodies, triomics_reproduction_css_remote, logical(1L)))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_RENDER_INVALID",
      "The rendered report depends on an active remote resource.", "report.html"
    )
  }
  html_without_styles <- gsub("(?is)<style\\b[^>]*>.*?</style\\s*>", "", html, perl = TRUE)
  tag_matches <- gregexpr("(?is)<[[:alpha:]][^<>]*>", html_without_styles, perl = TRUE)
  tags <- regmatches(html_without_styles, tag_matches)[[1L]]
  active_tags <- c("script", "iframe", "img", "source", "audio", "video", "track",
                   "embed", "object", "image", "use")
  active_link_relations <- c("stylesheet", "preload", "modulepreload", "import", "prefetch",
                             "preconnect", "dns-prefetch", "manifest", "icon", "apple-touch-icon",
                             "mask-icon", "pingback", "serviceworker")
  active_remote <- FALSE
  for (tag in tags) {
    tag_match <- regmatches(tag, regexec("(?is)^<([[:alpha:]][[:alnum:]:_-]*)\\b([^<>]*)>$",
                                         tag, perl = TRUE))[[1L]]
    if (length(tag_match) < 3L) next
    tag_name <- tolower(tag_match[[2L]])
    attributes <- triomics_reproduction_html_attributes(tag_match[[3L]])
    names_by_attr <- vapply(attributes, `[[`, character(1L), "name")
    values_by_attr <- vapply(attributes, `[[`, character(1L), "value")
    values <- split(values_by_attr, names_by_attr)
    references <- character()
    if (tag_name %in% active_tags) {
      active_names <- if (tag_name %in% c("object")) c("data", "codebase") else
        if (tag_name %in% c("image", "use")) c("href", "xlink:href") else
          c("src", "srcset", "poster")
      references <- unlist(values[intersect(active_names, names(values))], use.names = FALSE)
    }
    if (identical(tag_name, "link") && "rel" %in% names(values) && "href" %in% names(values)) {
      relation_tokens <- unlist(strsplit(paste(values$rel, collapse = " "), "[[:space:]]+"),
                                use.names = FALSE)
      if (any(tolower(relation_tokens) %in% active_link_relations)) {
        references <- c(references, values$href)
      }
    }
    if ("style" %in% names(values) &&
        triomics_reproduction_css_remote(paste(values$style, collapse = " "))) {
      active_remote <- TRUE
      break
    }
    if (length(references)) {
      reference_urls <- unlist(strsplit(references, ",", fixed = TRUE), use.names = FALSE)
      if (any(vapply(reference_urls, triomics_reproduction_remote_reference, logical(1L)))) {
        active_remote <- TRUE
        break
      }
    }
  }
  if (isTRUE(active_remote)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_RENDER_INVALID",
      "The rendered report depends on an active remote resource.", "report.html"
    )
  }
  list(status = "offline_assets_verified", active_remote_resources = 0L)
}

triomics_reproduction_validate_report <- function(output_root, fixture) {
  report_bundle <- tryCatch(
    triomics_report_bundle_from_root(output_root, fixture$config_path),
    error = function(error) error
  )
  if (inherits(report_bundle, "error")) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The saved A/B/C/D handoffs failed the existing report consumer validation.",
      "producer_artifacts"
    )
  }
  report_manifest_record <- triomics_reproduction_read_json(
    output_root, file.path("report", "report_manifest.json")
  )
  report_manifest <- report_manifest_record$value
  config_sha256 <- as.character(fixture$config_sha256)
  if (!identical(as.character(report_manifest$artifact_type), "triomics.report-manifest") ||
      !identical(as.character(report_manifest$status), "complete") ||
      !identical(as.character(report_manifest$config_sha256), config_sha256) ||
      !is.numeric(report_manifest$figure_count) || length(report_manifest$figure_count) != 1L ||
      is.na(report_manifest$figure_count) || report_manifest$figure_count < 1L ||
      !is.list(report_manifest$outputs) || !length(report_manifest$outputs)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The saved report manifest is partial or identifies a different run.",
      "report_manifest"
    )
  }
  output_names <- vapply(report_manifest$outputs, function(item) {
    if (!is.list(item) || is.null(item$path)) "" else as.character(item$path)
  }, character(1L))
  if (any(!nzchar(output_names)) || anyDuplicated(output_names) ||
      !all(c("report.html", "report_data.json", "results.tsv") %in% output_names)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The report manifest omits or duplicates a required output identity.",
      "report_manifest.outputs"
    )
  }
  output_refs <- lapply(report_manifest$outputs, function(item) {
    relative <- file.path("report", as.character(item$path))
    triomics_reproduction_file_reference(
      output_root, relative, as.character(item$digest), item$size_bytes
    )
  })
  names(output_refs) <- output_names
  for (relative in c("report/report_data.envelope.json", "report/report_manifest.envelope.json")) {
    if (!isTRUE(triomics_validate_envelope(file.path(output_root, relative))) ||
        triomics_is_symlink(file.path(output_root, relative))) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_SOURCE_INVALID",
        "A rendered report envelope is incomplete or changed.", relative
      )
    }
  }
  report_html <- triomics_reproduction_safe_file(output_root, "report/report.html")
  offline <- triomics_reproduction_offline_html(report_html)
  results_path <- triomics_reproduction_safe_file(output_root, "report/results.tsv")
  results_ref <- triomics_reproduction_file_reference(output_root, "report/results.tsv")
  results <- tryCatch(
    utils::read.delim(results_path, check.names = FALSE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8"),
    error = function(error) NULL
  )
  triomics_reproduction_file_reference(output_root, "report/results.tsv",
                                       results_ref$sha256, results_ref$size_bytes)
  required_columns <- c("block_id", "feature_key", "analysis_id", "representation_id",
                        "contrast_id", "effect", "effect_unit", "n_samples",
                        "n_subjects", "n_pairs")
  if (!is.data.frame(results) || !all(required_columns %in% names(results))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The report results table lacks the required saved identities or units.",
      "report/results.tsv"
    )
  }
  report_config <- triomics_report_config(fixture$config_path)
  report_sources <- tryCatch(
    triomics_report_verify_bundle(report_bundle, report_config),
    error = function(error) error
  )
  if (inherits(report_sources, "error")) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The report source bundle could not reproduce the saved result projection.",
      "report/results.tsv"
    )
  }
  projection_path <- tempfile(
    "d09-report-results-",
    tmpdir = triomics_reproduction_evidence_root()
  )
  on.exit(unlink(projection_path, force = TRUE), add = TRUE)
  tryCatch(
    triomics_report_write_tsv(
      triomics_report_rows(report_sources$payloads$b, report_config$blocks),
      projection_path
    ),
    error = function(error) triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The report result projection could not be reproduced from the verified B handoff.",
      "report/results.tsv"
    )
  )
  projected_size <- file.info(projection_path)$size
  projected_sha256 <- triomics_sha256(projection_path)
  if (is.na(projected_size) || !identical(as.numeric(projected_size),
                                          as.numeric(results_ref$size_bytes)) ||
      !identical(projected_sha256, results_ref$sha256)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The complete saved result table differs from the verified B-backed report projection.",
      "report/results.tsv"
    )
  }
  config <- report_config$config
  analysis <- config$analyses[[1L]]
  design <- analysis$design
  selected <- results[
    results$block_id == "block_1" & results$contrast_id == "target_minus_reference",
    , drop = FALSE
  ]
  if (!nrow(selected) || anyNA(selected$feature_key) ||
      any(!grepl("^fk1\\|", selected$feature_key)) ||
      any(selected$analysis_id != analysis$analysis_id) || any(!is.finite(selected$effect))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "Saved result rows do not retain their feature, analysis, contrast, and effect identities.",
      "report/results.tsv"
    )
  }
  selected <- selected[order(selected$feature_key, method = "radix"), , drop = FALSE]
  first <- selected[1L, , drop = FALSE]
  if (identical(design$kind, "paired")) {
    valid <- identical(as.character(first$representation_id[[1L]]), "delta") &&
      identical(as.character(first$effect_unit[[1L]]), "delta_difference") &&
      identical(as.integer(first$n_subjects[[1L]]), 6L) &&
      identical(as.integer(first$n_pairs[[1L]]), 6L) &&
      is.finite(first$effect[[1L]]) && first$effect[[1L]] > 0
    verification <- list(
      design = "paired",
      representation = "delta",
      subject_count = 6L,
      pair_count = 6L,
      contrast = "target_minus_reference",
      effect_unit = "delta_difference",
      first_saved_effect = as.numeric(first$effect[[1L]]),
      first_saved_feature_key = as.character(first$feature_key[[1L]])
    )
  } else {
    pairs <- first$n_pairs[[1L]]
    subjects <- first$n_subjects[[1L]]
    valid <- identical(as.character(first$representation_id[[1L]]), "levels") &&
      identical(as.character(first$effect_unit[[1L]]), "group_difference") &&
      identical(as.integer(first$n_samples[[1L]]), 12L) &&
      (is.na(pairs) || pairs == 0) &&
      (is.na(subjects) || subjects != 6L) &&
      is.finite(first$effect[[1L]]) && first$effect[[1L]] > 0
    verification <- list(
      design = "independent",
      representation = "levels",
      sample_count = 12L,
      paired_count = if (is.na(pairs)) NULL else as.integer(pairs),
      contrast = "target_minus_reference",
      effect_unit = "group_difference",
      first_saved_effect = as.numeric(first$effect[[1L]]),
      first_saved_feature_key = as.character(first$feature_key[[1L]])
    )
  }
  if (!isTRUE(valid)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "Saved result counts or effect direction disagree with the declared synthetic design.",
      "report/results.tsv"
    )
  }
  a_assays <- triomics_reproduction_read_json(output_root, file.path("A", "assays.json"))
  a_envelope_path <- triomics_reproduction_safe_file(output_root, "A/assays.envelope.json")
  a_block_ids <- if (is.list(a_assays$value$blocks)) {
    vapply(a_assays$value$blocks, function(block) {
      id <- if (is.list(block)) block$block_id else NULL
      if (is.null(id) || length(id) != 1L || is.na(id)) NA_character_ else as.character(id)
    }, character(1L))
  } else character()
  config_block_ids <- as.character(vapply(config$blocks, `[[`, character(1L), "block_id"))
  if (!isTRUE(triomics_validate_envelope(a_envelope_path)) ||
      triomics_is_symlink(a_envelope_path) ||
      !identical(as.character(a_assays$value$study_id), as.character(config$study_id)) ||
      !length(a_block_ids) || anyNA(a_block_ids) || anyDuplicated(a_block_ids) ||
      !setequal(a_block_ids, config_block_ids) || as.integer(report_manifest$figure_count) < 1L) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The installed A assay artifact does not bind this configuration.",
      "A/assays.json"
    )
  }
  list(
    producer_artifacts = report_bundle$artifacts,
    report_manifest = report_manifest_record$reference,
    report_outputs = output_refs,
    report_status = as.character(report_manifest$status),
    report_figures = as.integer(report_manifest$figure_count),
    offline_assets = offline,
    analysis_verification = verification,
    a_assays = a_assays$reference
  )
}

triomics_reproduction_manifest <- function(run) {
  required_stages <- c("A", "B", "C", "D", "report")
  if (!is.list(run) || !is.list(run$stages) ||
      !identical(names(run$stages), required_stages) ||
      !is.list(run$installed) || !is.list(run$authority) ||
      !is.character(run$output_root) || length(run$output_root) != 1L ||
      !dir.exists(run$output_root) || !is.character(run$project_root) ||
      !dir.exists(run$project_root)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_MANIFEST_INVALID",
      "The reproduction receipt is incomplete or has an unsupported stage inventory.",
      "run"
    )
  }
  if (!all(vapply(run$stages, function(stage) {
    is.list(stage) && identical(stage$status, "complete") &&
      identical(as.integer(stage$exit_code), 0L)
  }, logical(1L)))) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_EXECUTION_FAILED",
      "The reproduction manifest requires completed installed stage receipts.",
      "stages"
    )
  }
  fixture_state <- triomics_reproduction_validate_fixture(run$fixture)
  authority <- triomics_reproduction_authority_snapshot(run$project_root)
  if (!identical(authority, run$authority)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The authoritative programme records changed after the installed walkthrough.",
      "programme_authorities"
    )
  }
  output_root <- normalizePath(run$output_root, winslash = "/", mustWork = TRUE)
  report <- triomics_reproduction_validate_report(output_root, run$fixture)
  stage_receipts <- lapply(run$stages, function(stage) {
    list(
      stage = as.character(stage$stage),
      status = as.character(stage$status),
      exit_code = as.integer(stage$exit_code),
      command = as.character(stage$command),
      warning_count = as.integer(stage$warning_count %||% 0L)
    )
  })
  input_records <- run$fixture$inputs
  metadata_path <- file.path(run$fixture$root, "data", run$fixture$design, "metadata.tsv")
  metadata <- utils::read.delim(metadata_path, check.names = FALSE, stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8")
  if (!"batch" %in% names(metadata) || anyNA(metadata$batch)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The walkthrough metadata fixture has invalid batch factor values.",
      "fixture.metadata"
    )
  }
  config_relative <- substring(
    normalizePath(run$fixture$config_path, winslash = "/", mustWork = TRUE),
    nchar(normalizePath(run$fixture$root, winslash = "/", mustWork = TRUE)) + 2L
  )
  payload <- list(
    schema_version = "2.0",
    artifact_type = "triomics.d-t09-reproduction-wrapper",
    execution_status = "complete",
    scientific_validation_status = "synthetic_software_fixture_only",
    real_data_validation_status = authority$real_data_validation_status,
    packet_acceptance_status = "pending_sol_acceptance",
    registration_accepted = FALSE,
    self_acceptance = FALSE,
    publication_status = authority$publication_status,
    installed_identity = run$installed,
    environment = triomics_reproduction_runtime(run$installed),
    stage_receipts = stage_receipts,
    fixture = list(
      design = as.character(run$fixture$design),
      study_id = as.character(fixture_state$config$study_id),
      analysis_id = as.character(fixture_state$config$analyses[[1L]]$analysis_id),
      config_path = config_relative,
      config_sha256 = as.character(run$fixture$config_sha256),
      inputs = input_records,
      missingness = list(
        status = "not_exercised",
        role = "no_missing_measurement_or_clinical_values_in_walkthrough_fixture"
      )
    ),
    analysis_verification = report$analysis_verification,
    producer_artifacts = report$producer_artifacts,
    report = list(
      status = report$report_status,
      manifest = report$report_manifest,
      outputs = report$report_outputs,
      figure_count = report$report_figures,
      offline_assets = report$offline_assets
    ),
    programme_accounting = list(
      status = "pending_sol_reconciliation",
      packet_acceptance = authority$packet_acceptance,
      packet_count = authority$packet_count,
      requirement_count = authority$requirement_count,
      ledger_packet_count = authority$ledger_packet_count,
      conditional_scope_count = authority$conditional_scope_count,
      packet_ids_sha256 = authority$packet_ids_sha256,
      requirement_ids_sha256 = authority$requirement_ids_sha256,
      authorities = authority$authorities
    )
  )
  content_id <- triomics_hash_text(triomics_canonical_json(payload))
  payload$manifest_id <- content_id
  reproduction_dir <- file.path(output_root, "reproduction")
  if (file.exists(reproduction_dir) && !dir.exists(reproduction_dir)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The immutable reproduction directory is occupied by a regular file.",
      "reproduction"
    )
  }
  dir.create(reproduction_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(reproduction_dir) || triomics_is_symlink(reproduction_dir)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The immutable reproduction directory is unsafe.", "reproduction"
    )
  }
  relative_path <- file.path("reproduction", paste0("manifest-", content_id, ".json"))
  manifest_path <- file.path(output_root, relative_path)
  existing_manifests <- sort(
    list.files(reproduction_dir, pattern = "^manifest-.*[.]json$", full.names = FALSE),
    method = "radix"
  )
  if (length(existing_manifests) > 1L) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_SOURCE_INVALID",
      "The completed walkthrough already has multiple reproduction wrappers; existing evidence was preserved.",
      "reproduction.manifest"
    )
  }
  if (length(existing_manifests) == 1L) {
    existing_relative <- file.path("reproduction", existing_manifests[[1L]])
    existing_record <- tryCatch(
      triomics_reproduction_read_json(output_root, existing_relative),
      error = function(error) error
    )
    if (inherits(existing_record, "error")) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_SOURCE_INVALID",
        "The existing completed reproduction wrapper is invalid; it was preserved.",
        "reproduction.manifest"
      )
    }
    existing <- existing_record$value
    existing_id <- as.character(existing$manifest_id %||% "")
    existing_body <- existing
    existing_body$manifest_id <- NULL
    if (!identical(existing_id, sub("^manifest-(.*)[.]json$", "\\1", existing_manifests[[1L]])) ||
        !identical(triomics_hash_text(triomics_canonical_json(existing_body)), existing_id)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_SOURCE_INVALID",
        "The existing completed reproduction wrapper failed its identity check; it was preserved.",
        "reproduction.manifest"
      )
    }
    if (!identical(triomics_canonical_json(existing), triomics_canonical_json(payload))) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_SOURCE_INVALID",
        "Completed walkthrough evidence changed after its original wrapper; existing bytes were preserved.",
        "reproduction.manifest"
      )
    }
  }
  if (file.exists(manifest_path) || dir.exists(manifest_path)) {
    existing <- triomics_reproduction_read_json(output_root, relative_path)$value
    existing_id <- as.character(existing$manifest_id %||% "")
    existing_body <- existing
    existing_body$manifest_id <- NULL
    if (!identical(existing_id, content_id) ||
        !identical(triomics_hash_text(triomics_canonical_json(existing_body)), content_id)) {
      triomics_reproduction_abort(
        "D_REPRODUCTION_OUTPUT_INVALID",
        "An existing immutable reproduction manifest is corrupt; it was preserved.",
        "reproduction.manifest"
      )
    }
  } else {
    tryCatch(
      triomics_write_json_atomic(payload, manifest_path),
      triomics_error = function(error) stop(error),
      error = function(error) triomics_reproduction_abort(
        "D_REPRODUCTION_OUTPUT_INVALID",
        "The immutable reproduction manifest could not be finalized.",
        "reproduction.manifest"
      )
    )
  }
  saved <- triomics_reproduction_read_json(output_root, relative_path)
  saved_body <- saved$value
  saved_id <- as.character(saved_body$manifest_id %||% "")
  saved_body$manifest_id <- NULL
  if (!identical(saved_id, content_id) ||
      !identical(triomics_hash_text(triomics_canonical_json(saved_body)), content_id)) {
    triomics_reproduction_abort(
      "D_REPRODUCTION_OUTPUT_INVALID",
      "The reproduction manifest failed identity read-back.", "reproduction.manifest"
    )
  }
  list(
    manifest = payload,
    path = manifest_path,
    relative_path = relative_path,
    byte_sha256 = saved$reference$sha256
  )
}
