registry_test_root <- function(label = "case") {
  base <- Sys.getenv("TRIOMICS_M8_D08_S01_OUTPUT_ROOT", unset = tempdir())
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  root <- tempfile(paste0("stage-run-registry-", label, "-"), tmpdir = base)
  dir.create(root, recursive = TRUE)
  root
}

registry_test_sha <- paste(rep("a", 64L), collapse = "")

registry_test_record <- function(run_id = "b-t01", study_id = "registry_fixture",
                                 stage = "B", status = "complete",
                                 schema_version = "2.0") {
  list(schema_version = schema_version, run_id = run_id, study_id = study_id,
       stage = stage, status = status)
}

registry_test_write_json <- function(value, path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null",
                       pretty = TRUE, digits = NA)
  invisible(path)
}

registry_test_bytes <- function(path) {
  readBin(path, what = "raw", n = file.info(path)$size)
}

registry_test_native_a <- function(output, study_id = "registry_fixture",
                                   config_sha256 = registry_test_sha) {
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output, "A"), showWarnings = FALSE)
  dir.create(file.path(output, "registries"), showWarnings = FALSE)
  payload_path <- file.path(output, "A", "assays.json")
  payload <- list(schema_version = "2.0", study_id = study_id,
                  blocks = list(block_1 = list(features = list())))
  registry_test_write_json(payload, payload_path)
  envelope <- triomics_artifact_envelope(
    artifact_type = "assays", artifact_id = "stage-a-assays",
    study_id = study_id, run_id = "a-t02", status = "complete",
    config_sha256 = config_sha256, payload = payload_path,
    producer_commit = "registry-test", inputs = list(),
    reason_codes = character(), privacy_class = "synthetic"
  )
  envelope_path <- file.path(output, "A", "assays.envelope.json")
  triomics_write_envelope(envelope, envelope_path)
  record <- list(schema_version = "2.0", run_id = "a-t02",
                 study_id = study_id, stage = "A", status = "complete")
  legacy_path <- file.path(output, "registries", "run.json")
  triomics_write_json_atomic(record, legacy_path)
  list(record = record, payload = payload, envelope = envelope,
       legacy_path = legacy_path, payload_path = payload_path,
       envelope_path = envelope_path)
}

registry_test_expect_code <- function(expr, expected_code) {
  error <- testthat::capture_error(force(expr))
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, expected_code)
  invisible(error)
}

registry_test_reject <- function(output, record = registry_test_record(),
                                 config_sha256 = registry_test_sha) {
  registry_test_expect_code(
    triomics_b_write_run_registry(record, output, config_sha256),
    "B_RUN_REGISTRY_INVALID"
  )
}

testthat::test_that("standalone B publication creates byte-identical legacy and canonical records", {
  output <- registry_test_root("standalone")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  record <- registry_test_record()
  triomics_b_write_run_registry(record, output, registry_test_sha)

  legacy <- file.path(output, "registries", "run.json")
  canonical <- file.path(output, "registries", "run-B.json")
  testthat::expect_identical(registry_test_bytes(legacy), registry_test_bytes(canonical))
  testthat::expect_identical(
    jsonlite::read_json(canonical, simplifyVector = FALSE), record
  )
})

testthat::test_that("native A registry, payload, and envelope stay byte-identical during B publication", {
  output <- registry_test_root("native-a")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  paths <- registry_test_native_a(output)
  before <- lapply(paths[c("legacy_path", "payload_path", "envelope_path")],
                   registry_test_bytes)

  triomics_b_write_run_registry(registry_test_record(), output, registry_test_sha)

  after <- lapply(paths[c("legacy_path", "payload_path", "envelope_path")],
                  registry_test_bytes)
  testthat::expect_identical(after, before)
  testthat::expect_true(file.exists(file.path(output, "registries", "run-B.json")))
})

testthat::test_that("B publication rejects malformed, incomplete, extra, and unsupported identities", {
  output <- registry_test_root("bad-records")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  base <- registry_test_record()
  invalid <- list(
    malformed = "not a registry record",
    missing_identity = base[setdiff(names(base), "study_id")],
    extra_identity = c(base, list(extra = "unexpected")),
    wrong_schema = within(base, schema_version <- "1.0"),
    unsupported_stage = within(base, stage <- "C"),
    unsupported_run = within(base, run_id <- "b-t99"),
    unsupported_status = within(base, status <- "not-a-status")
  )
  for (record in invalid) registry_test_reject(output, record)
})

testthat::test_that("B publication rejects malformed, B, and unsupported legacy records", {
  output <- registry_test_root("legacy-admission")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  legacy <- file.path(output, "registries", "run.json")
  records <- list(
    b_orphan = registry_test_record(),
    unsupported_c = registry_test_record(run_id = "c-t01", stage = "C")
  )
  for (record in records) {
    registry_test_write_json(record, legacy)
    registry_test_reject(output)
    unlink(legacy)
  }
  writeBin(charToRaw("{malformed"), legacy)
  registry_test_reject(output)
})

testthat::test_that("identical existing canonical B records and directory targets are rejected unchanged", {
  for (target in c("run-B.json", "run.json")) {
    output <- registry_test_root(paste0("directory-", target))
    on.exit(unlink(output, recursive = TRUE), add = TRUE)
    dir.create(file.path(output, "registries"))
    dir.create(file.path(output, "registries", target))
    registry_test_reject(output)
    testthat::expect_true(dir.exists(file.path(output, "registries", target)))
    unlink(output, recursive = TRUE)
  }

  output <- registry_test_root("identical-canonical")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  canonical <- file.path(output, "registries", "run-B.json")
  record <- registry_test_record()
  triomics_write_json_atomic(record, canonical)
  before <- registry_test_bytes(canonical)
  registry_test_reject(output, record)
  testthat::expect_identical(registry_test_bytes(canonical), before)
  testthat::expect_false(file.exists(file.path(output, "registries", "run.json")))
})

testthat::test_that("preexisting registry temporary files are rejected", {
  for (target in c("run.json.tmp", "run-B.json.tmp")) {
    output <- registry_test_root(paste0("temporary-", target))
    on.exit(unlink(output, recursive = TRUE), add = TRUE)
    dir.create(file.path(output, "registries"))
    temporary <- file.path(output, "registries", target)
    writeBin(charToRaw("unfinished"), temporary)
    before <- registry_test_bytes(temporary)
    registry_test_reject(output)
    testthat::expect_identical(registry_test_bytes(temporary), before)
  }
})

testthat::test_that("native A registry identity and schema mismatches fail closed", {
  changes <- list(
    wrong_registry_schema = function(paths, native) {
      native$record$schema_version <- "1.0"
      registry_test_write_json(native$record, paths$legacy_path)
    },
    wrong_registry_run = function(paths, native) {
      native$record$run_id <- "a-other"
      registry_test_write_json(native$record, paths$legacy_path)
    },
    wrong_registry_study = function(paths, native) {
      native$record$study_id <- "another-study"
      registry_test_write_json(native$record, paths$legacy_path)
    },
    wrong_registry_status = function(paths, native) {
      native$record$status <- "partial"
      registry_test_write_json(native$record, paths$legacy_path)
    },
    wrong_envelope_schema = function(paths, native) {
      native$envelope$schema_version <- "1.0"
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_envelope_study = function(paths, native) {
      native$envelope$study_id <- "another-study"
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_envelope_run = function(paths, native) {
      native$envelope$run_id <- "a-other"
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_current_config = function(paths, native) {
      native$envelope$config_sha256 <- paste(rep("b", 64L), collapse = "")
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_artifact_type = function(paths, native) {
      native$envelope$artifact_type <- "block_results"
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_artifact_id = function(paths, native) {
      native$envelope$artifact_id <- "another-artifact"
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_payload_name = function(paths, native) {
      native$envelope$payload_file <- "other-assays.json"
      registry_test_write_json(native$envelope, paths$envelope_path)
    },
    wrong_payload_hash = function(paths, native) {
      native$envelope$payload_sha256 <- paste(rep("0", 64L), collapse = "")
      registry_test_write_json(native$envelope, paths$envelope_path)
    }
  )
  for (name in names(changes)) {
    output <- registry_test_root(name)
    on.exit(unlink(output, recursive = TRUE), add = TRUE)
    paths <- registry_test_native_a(output)
    native <- paths
    changes[[name]](paths, native)
    registry_test_reject(output)
  }
})

testthat::test_that("native A payload schema, study, and block identity mismatches fail closed", {
  changes <- list(
    wrong_payload_schema = function(payload) {
      payload$schema_version <- "1.0"
      payload
    },
    wrong_payload_study = function(payload) {
      payload$study_id <- "another-study"
      payload
    },
    missing_blocks = function(payload) {
      payload$blocks <- NULL
      payload
    },
    malformed_blocks = function(payload) {
      payload$blocks <- "not-a-block-map"
      payload
    }
  )
  for (name in names(changes)) {
    output <- registry_test_root(name)
    on.exit(unlink(output, recursive = TRUE), add = TRUE)
    paths <- registry_test_native_a(output)
    payload <- jsonlite::read_json(paths$payload_path, simplifyVector = FALSE)
    payload <- changes[[name]](payload)
    registry_test_write_json(payload, paths$payload_path)
    envelope <- jsonlite::read_json(paths$envelope_path, simplifyVector = FALSE)
    envelope$payload_sha256 <- triomics_sha256(paths$payload_path)
    registry_test_write_json(envelope, paths$envelope_path)
    registry_test_reject(output)
  }

  output <- registry_test_root("current-config")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  registry_test_native_a(output, config_sha256 = paste(rep("b", 64L), collapse = ""))
  registry_test_reject(output, config_sha256 = registry_test_sha)
})

testthat::test_that("resolving, dangling, and ancestor symlinks are rejected", {
  output <- registry_test_root("resolving-link")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  target <- file.path(output, "target.json")
  writeBin(charToRaw("target"), target)
  link <- file.path(output, "registries", "run-B.json")
  if (!file.symlink(target, link)) testthat::skip("filesystem does not support symlink fixtures")
  registry_test_reject(output)
  testthat::expect_identical(registry_test_bytes(target), charToRaw("target"))

  output <- registry_test_root("dangling-link")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  link <- file.path(output, "registries", "run.json")
  if (!file.symlink("missing-target", link)) testthat::skip("filesystem does not support symlink fixtures")
  registry_test_reject(output)
  testthat::expect_true(nzchar(Sys.readlink(link)))

  output <- registry_test_root("ancestor-link")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registry-real"))
  if (!file.symlink(file.path(output, "registry-real"),
                    file.path(output, "registries"))) {
    testthat::skip("filesystem does not support symlink fixtures")
  }
  registry_test_reject(output)
})

testthat::test_that("publication boundary exceptions clean only matching attempt bytes and never complete", {
  original_writer <- getFromNamespace("triomics_write_json_atomic", "triomics")
  fail_at <- ""
  registry_writes <- 0L
  testthat::local_mocked_bindings(
    triomics_write_json_atomic = function(value, path) {
      name <- basename(path)
      if (name %in% c("run.json", "run-B.json")) {
        registry_writes <<- registry_writes + 1L
        role <- if (identical(name, "run.json")) "legacy" else "canonical"
        if (identical(fail_at, paste0("before:", role))) {
          stop("injected boundary failure", call. = FALSE)
        }
        original_writer(value, path)
        if (identical(fail_at, paste0("after:", role))) {
          stop("injected boundary failure", call. = FALSE)
        }
        return(invisible(path))
      }
      original_writer(value, path)
    },
    .package = "triomics"
  )

  for (point in c("before:legacy", "after:legacy",
                  "before:canonical", "after:canonical")) {
    fail_at <- point
    registry_writes <- 0L
    output <- registry_test_root(gsub(":", "-", point))
    on.exit(unlink(output, recursive = TRUE), add = TRUE)
    dir.create(file.path(output, "registries"))
    returned <- FALSE
    testthat::expect_error({
      triomics_b_write_run_registry(registry_test_record(), output, registry_test_sha)
      returned <- TRUE
    }, "injected boundary failure")
    testthat::expect_false(returned)
    testthat::expect_false(file.exists(file.path(output, "registries", "run.json")))
    testthat::expect_false(file.exists(file.path(output, "registries", "run-B.json")))
    testthat::expect_false(file.exists(file.path(output, "registries", "run.json.tmp")))
    testthat::expect_false(file.exists(file.path(output, "registries", "run-B.json.tmp")))
    testthat::expect_identical(registry_writes, if (grepl("canonical", point)) 2L else 1L)
  }
})

testthat::test_that("cleanup preserves bytes changed by another writer after this attempt starts", {
  original_writer <- getFromNamespace("triomics_write_json_atomic", "triomics")
  testthat::local_mocked_bindings(
    triomics_write_json_atomic = function(value, path) {
      original_writer(value, path)
      if (identical(basename(path), "run.json")) {
        writeBin(charToRaw("foreign replacement"), path)
        stop("injected post-write failure", call. = FALSE)
      }
      invisible(path)
    },
    .package = "triomics"
  )
  output <- registry_test_root("foreign-bytes")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  dir.create(file.path(output, "registries"))
  legacy <- file.path(output, "registries", "run.json")
  testthat::expect_error(
    triomics_b_write_run_registry(registry_test_record(), output, registry_test_sha),
    "injected post-write failure"
  )
  testthat::expect_identical(registry_test_bytes(legacy), charToRaw("foreign replacement"))
  testthat::expect_false(file.exists(file.path(output, "registries", "run-B.json")))
})

testthat::test_that("canonical-write exceptions leave a valid native A triplet untouched", {
  original_writer <- getFromNamespace("triomics_write_json_atomic", "triomics")
  fail_when <- ""
  testthat::local_mocked_bindings(
    triomics_write_json_atomic = function(value, path) {
      if (identical(basename(path), "run-B.json")) {
        if (identical(fail_when, "before")) stop("injected canonical failure", call. = FALSE)
        original_writer(value, path)
        if (identical(fail_when, "after")) stop("injected canonical failure", call. = FALSE)
        return(invisible(path))
      }
      original_writer(value, path)
    },
    .package = "triomics"
  )
  for (phase in c("before", "after")) {
    fail_when <- phase
    output <- registry_test_root(paste0("native-a-", phase))
    on.exit(unlink(output, recursive = TRUE), add = TRUE)
    paths <- registry_test_native_a(output)
    before <- lapply(paths[c("legacy_path", "payload_path", "envelope_path")],
                     registry_test_bytes)
    testthat::expect_error(
      triomics_b_write_run_registry(registry_test_record(), output, registry_test_sha),
      "injected canonical failure"
    )
    after <- lapply(paths[c("legacy_path", "payload_path", "envelope_path")],
                    registry_test_bytes)
    testthat::expect_identical(after, before)
    testthat::expect_false(file.exists(file.path(output, "registries", "run-B.json")))
  }
})

testthat::test_that("ordinary Stage B publishes through the frozen registry helper", {
  output <- registry_test_root("ordinary-call-site")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  config_path <- triomics_test_resource("examples", "tickets", "B-T01.yaml")
  expected_config <- getFromNamespace("triomics_read_config", "triomics")(config_path)
  helper_calls <- list()
  original_helper <- getFromNamespace("triomics_b_write_run_registry", "triomics")
  testthat::local_mocked_bindings(
    triomics_b_write_run_registry = function(record, output, config_sha256) {
      helper_calls[[length(helper_calls) + 1L]] <<- list(
        record = record, output = output, config_sha256 = config_sha256
      )
      original_helper(record, output, config_sha256)
    },
    .package = "triomics"
  )

  triomics_execute_stage_b(config_path, output)

  testthat::expect_length(helper_calls, 1L)
  if (length(helper_calls) == 1L) {
    testthat::expect_identical(names(helper_calls[[1L]]$record),
                              c("schema_version", "run_id", "study_id", "stage", "status"))
    testthat::expect_identical(helper_calls[[1L]]$record$run_id, "b-t01")
    testthat::expect_identical(helper_calls[[1L]]$record$stage, "B")
    testthat::expect_identical(helper_calls[[1L]]$output, output)
    testthat::expect_identical(helper_calls[[1L]]$config_sha256,
                              attr(expected_config, "config_sha256"))
  }
  testthat::expect_true(file.exists(file.path(output, "registries", "run-B.json")))
  if (file.exists(file.path(output, "registries", "run.json")) &&
      file.exists(file.path(output, "registries", "run-B.json"))) {
    testthat::expect_identical(
      registry_test_bytes(file.path(output, "registries", "run.json")),
      registry_test_bytes(file.path(output, "registries", "run-B.json"))
    )
  }
})

testthat::test_that("repeated Stage B publishes before later acceptance failure without recalibration", {
  output <- registry_test_root("repeated-call-site")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  config_path <- triomics_test_resource("examples", "tickets", "B-T04.yaml")
  expected_config <- getFromNamespace("triomics_read_config", "triomics")(config_path)
  helper_calls <- list()
  calibration_calls <- 0L
  original_helper <- getFromNamespace("triomics_b_write_run_registry", "triomics")
  calibration <- list(
    status = "pass", executed = TRUE, skipped = FALSE,
    patterns = list(
      irregular_linear_complete = list(null = list(status = "pass")),
      natural_spline_complete = list(null = list(status = "pass"))
    )
  )
  mocked_fit <- list(
    status = "partial", limma_version = "not-run",
    blocks = list(block_1 = list(contrasts = list(
      per_synthetic_step = list(rows = list(list(
        effect = 0, ordinary_df = 7L, design_rank = 5L,
        duplicate_correlation_applied = FALSE
      )))
    )))
  )
  testthat::local_mocked_bindings(
    triomics_b_write_run_registry = function(record, output, config_sha256) {
      helper_calls[[length(helper_calls) + 1L]] <<- list(
        record = record, output = output, config_sha256 = config_sha256
      )
      original_helper(record, output, config_sha256)
    },
    triomics_b04_calibrate = function() {
      calibration_calls <<- calibration_calls + 1L
      calibration
    },
    triomics_fit_block_models = function(...) mocked_fit,
    .package = "triomics"
  )

  registry_test_expect_code(
    triomics_b04_execute_fixture(config_path, output),
    "B_REPEATED_MODEL_UNSUPPORTED"
  )

  testthat::expect_length(helper_calls, 1L)
  if (length(helper_calls) == 1L) {
    testthat::expect_identical(names(helper_calls[[1L]]$record),
                              c("schema_version", "run_id", "study_id", "stage", "status"))
    testthat::expect_identical(helper_calls[[1L]]$record$run_id, "b-t04")
    testthat::expect_identical(helper_calls[[1L]]$record$stage, "B")
    testthat::expect_identical(helper_calls[[1L]]$record$status, "partial")
    testthat::expect_identical(helper_calls[[1L]]$output, output)
    testthat::expect_identical(helper_calls[[1L]]$config_sha256,
                              attr(expected_config, "config_sha256"))
  }
  testthat::expect_identical(calibration_calls, 2L)
  testthat::expect_true(file.exists(file.path(output, "registries", "run-B.json")))
  if (file.exists(file.path(output, "registries", "run.json")) &&
      file.exists(file.path(output, "registries", "run-B.json"))) {
    testthat::expect_identical(
      registry_test_bytes(file.path(output, "registries", "run.json")),
      registry_test_bytes(file.path(output, "registries", "run-B.json"))
    )
  }
  acceptance <- jsonlite::read_json(file.path(output, "acceptance-record.json"),
                                    simplifyVector = FALSE)
  testthat::expect_identical(acceptance$acceptance[[1L]]$verdict, "fail")
})
