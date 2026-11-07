d09_project_root <- function() {
  candidate <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (attempt in seq_len(8L)) {
    if (file.exists(file.path(candidate, "discovery", "spec-v2", "packets.json"))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  stop("Could not locate the public triomics project records.", call. = FALSE)
}

d09_test_root <- function(name) {
  base <- Sys.getenv("TRIOMICS_PACKET_EVIDENCE_ROOT", unset = tempdir())
  root <- file.path(base, name)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

d09_new_root <- function(name) {
  base <- Sys.getenv("TRIOMICS_PACKET_EVIDENCE_ROOT", unset = tempdir())
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  tempfile(paste0(name, "-"), tmpdir = base)
}

d09_append_newline <- function(path) {
  size <- file.info(path)$size
  original <- readBin(path, what = "raw", n = as.integer(size))
  writeBin(c(original, as.raw(10L)), path)
  original
}

testthat::test_that("the programme evidence snapshot binds complete identities and stays pending", {
  root <- d09_project_root()
  snapshot_fn <- get0("triomics_reproduction_authority_snapshot", mode = "function",
                      inherits = TRUE)
  testthat::expect_true(is.function(snapshot_fn),
                        info = "The D-T09 authority snapshot interface is missing.")

  if (is.function(snapshot_fn)) {
    snapshot <- snapshot_fn(root)
    testthat::expect_identical(snapshot$status, "pending_sol_reconciliation")
    testthat::expect_identical(snapshot$packet_count, 41L)
    testthat::expect_identical(snapshot$requirement_count, 29L)
    testthat::expect_identical(snapshot$ledger_packet_count, 41L)
    testthat::expect_identical(snapshot$packet_acceptance, "pending_sol_acceptance")
    testthat::expect_identical(
      snapshot$authorities$execution_ledger$sha256,
      triomics_sha256(file.path(root, "docs", "implementation", "execution-ledger.json"))
    )

    packet_path <- file.path(root, "discovery", "spec-v2", "packets.json")
    original <- jsonlite::read_json(packet_path, simplifyVector = FALSE)
    cases <- list(
      missing = function(value) {
        value$packets <- value$packets[-1L]
        value
      },
      unknown = function(value) {
        value$packets[[1L]]$id <- "UNKNOWN-PACKET"
        value
      },
      duplicate = function(value) {
        value$packets[[2L]]$id <- value$packets[[1L]]$id
        value
      }
    )

    for (case_name in names(cases)) {
      case_root <- d09_test_root(paste0("authority-", case_name))
      spec_dir <- file.path(case_root, "discovery", "spec-v2")
      ledger_dir <- file.path(case_root, "docs", "implementation")
      dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(ledger_dir, recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(root, "discovery", "spec-v2", "requirements.json"),
                spec_dir, overwrite = TRUE)
      file.copy(file.path(root, "docs", "implementation", "execution-ledger.json"),
                ledger_dir, overwrite = TRUE)
      bad <- cases[[case_name]](original)
      jsonlite::write_json(bad, file.path(spec_dir, "packets.json"), auto_unbox = TRUE,
                           null = "null", pretty = TRUE, digits = NA)

      error <- testthat::expect_error(
        snapshot_fn(case_root),
        class = "triomics_error"
      )
      testthat::expect_identical(error$code, "D_REPRODUCTION_AUTHORITY_INVALID")
    }
  }
})

testthat::test_that("paired and independent fixtures materialize with their declared units", {
  materialize_fn <- get0("triomics_reproduction_materialize_fixture", mode = "function",
                         inherits = TRUE)
  template_resource_fn <- get0("triomics_reproduction_installed_resource",
                              mode = "function", inherits = TRUE)
  testthat::expect_true(is.function(materialize_fn),
                        info = "The D-T09 fixture materializer is missing.")
  testthat::expect_true(is.function(template_resource_fn),
                        info = "The installed D-T09 template reader is missing.")

  if (is.function(materialize_fn) && is.function(template_resource_fn)) {
    template_path <- template_resource_fn("examples", "tickets", "D-T09.yaml")
    template <- triomics_read_config(template_path)
    triomics_validate_config_paths(template, template_path)
    paired <- materialize_fn("paired", d09_new_root("paired-fixture"))
    independent <- materialize_fn("independent", d09_new_root("independent-fixture"))
    paired_config <- triomics_read_config(paired$config_path)
    independent_config <- triomics_read_config(independent$config_path)
    paired_design <- paired_config$analyses[[1L]]$design
    independent_design <- independent_config$analyses[[1L]]$design
    paired_metadata <- utils::read.delim(
      file.path(dirname(paired$config_path), paired_config$metadata$file),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    independent_metadata <- utils::read.delim(
      file.path(dirname(independent$config_path), independent_config$metadata$file),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    paired_matrix <- utils::read.delim(
      file.path(dirname(paired$config_path), paired_config$blocks[[1L]]$file),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    testthat::expect_identical(template$analyses[[1L]]$design$kind, "independent")
    testthat::expect_identical(template$analyses[[1L]]$design$condition, "group")
    testthat::expect_identical(length(template$blocks), 2L)
    testthat::expect_identical(paired_design$kind, "paired")
    testthat::expect_identical(paired_design$subject_id, "subject")
    testthat::expect_identical(paired_design$reference_visit, "baseline")
    testthat::expect_identical(paired_design$target_visit, "followup")
    testthat::expect_identical(nrow(paired_metadata), 12L)
    testthat::expect_identical(length(unique(paired_metadata$subject)), 6L)
    testthat::expect_true(all(vapply(split(paired_metadata$visit, paired_metadata$subject),
                                     function(visits) identical(sort(visits),
                                                               c("baseline", "followup")),
                                     logical(1L))))
    testthat::expect_equal(
      paired_matrix$SYN_01_followup[[1L]] - paired_matrix$SYN_01_baseline[[1L]],
      4.9919007, tolerance = 1e-12
    )
    testthat::expect_identical(independent_design$kind, "independent")
    testthat::expect_null(independent_design$subject_id)
    testthat::expect_identical(nrow(independent_metadata), 12L)
    testthat::expect_identical(length(unique(independent_metadata$sample_id)), 12L)
    group_counts <- sort(table(independent_metadata$group))
    testthat::expect_identical(as.integer(group_counts), c(6L, 6L))
    testthat::expect_identical(names(group_counts), c("control", "treatment"))
    testthat::expect_identical(sum(is.na(paired_metadata$batch)), 0L)
    testthat::expect_identical(sum(is.na(independent_metadata$batch)), 0L)
    testthat::expect_identical(paired$config_sha256, triomics_sha256(paired$config_path))
    testthat::expect_identical(independent$config_sha256,
                               triomics_sha256(independent$config_path))
  }
})

testthat::test_that("the installed CLI walks both designs and binds execution separately from acceptance", {
  run_fn <- get0("triomics_reproduction_run_installed", mode = "function", inherits = TRUE)
  manifest_fn <- get0("triomics_reproduction_manifest", mode = "function", inherits = TRUE)
  testthat::expect_true(is.function(run_fn),
                        info = "The installed D-T09 walkthrough runner is missing.")
  testthat::expect_true(is.function(manifest_fn),
                        info = "The D-T09 immutable reproduction manifest is missing.")

  if (is.function(run_fn)) {
    runs <- list()
    for (design in c("paired", "independent")) {
      fixture <- triomics_reproduction_materialize_fixture(
        design, d09_new_root(paste0(design, "-run-fixture"))
      )
      output_root <- d09_new_root(paste0(design, "-run-output"))
      runs[[design]] <- run_fn(fixture, output_root, d09_project_root())
      run <- runs[[design]]
      testthat::expect_identical(
        vapply(run$stages, `[[`, character(1L), "stage"),
        c("A", "B", "C", "D", "report")
      )
      testthat::expect_identical(
        vapply(run$stages, `[[`, integer(1L), "exit_code"),
        rep(0L, 5L)
      )
      testthat::expect_identical(run$manifest$execution_status, "complete")
      testthat::expect_identical(run$manifest$scientific_validation_status,
                                 "synthetic_software_fixture_only")
      testthat::expect_identical(run$manifest$packet_acceptance_status,
                                 "pending_sol_acceptance")
      testthat::expect_false(run$manifest$self_acceptance)
      testthat::expect_gte(run$manifest$report$figure_count, 1L)
      testthat::expect_match(run$installed$source_revision, "^[0-9a-f]{40}$")
      testthat::expect_true(file.exists(run$manifest_path))
      testthat::expect_true(file.exists(file.path(output_root, "A", "assays.json")))
      testthat::expect_true(file.exists(file.path(output_root, "A", "assays.envelope.json")))
      testthat::expect_identical(run$manifest$a_assays$sha256,
                                 triomics_sha256(file.path(output_root, "A", "assays.json")))
      testthat::expect_identical(run$manifest_byte_sha256,
                                 triomics_sha256(run$manifest_path))

      saved <- utils::read.delim(file.path(output_root, "report", "results.tsv"),
                                 check.names = FALSE, stringsAsFactors = FALSE)
      selected <- saved[saved$block_id == "block_1" &
                        saved$contrast_id == "target_minus_reference", , drop = FALSE]
      testthat::expect_true(nrow(selected) > 0L)
      testthat::expect_true(all(startsWith(selected$feature_key, "fk1|")))
      if (identical(design, "paired")) {
        testthat::expect_identical(selected$representation_id[[1L]], "delta")
        testthat::expect_identical(as.integer(selected$n_subjects[[1L]]), 6L)
        testthat::expect_identical(as.integer(selected$n_pairs[[1L]]), 6L)
        testthat::expect_identical(selected$effect_unit[[1L]], "delta_difference")
        testthat::expect_gt(selected$effect[[1L]], 0)
      } else {
        testthat::expect_identical(selected$representation_id[[1L]], "levels")
        testthat::expect_identical(as.integer(selected$n_samples[[1L]]), 12L)
        testthat::expect_true(is.na(selected$n_subjects[[1L]]) ||
                              selected$n_subjects[[1L]] != 6L)
        testthat::expect_true(is.na(selected$n_pairs[[1L]]))
        testthat::expect_identical(selected$effect_unit[[1L]], "group_difference")
        testthat::expect_gt(selected$effect[[1L]], 0)
      }
    }

    if (is.function(manifest_fn)) {
      run <- runs$paired
      manifest_path <- run$manifest_path
      original_manifest_hash <- triomics_sha256(manifest_path)
      metadata_record <- run$fixture$inputs[["metadata.tsv"]]
      input_path <- file.path(run$fixture$root, metadata_record$path)

      original_input <- d09_append_newline(input_path)
      error <- testthat::expect_error(manifest_fn(run), class = "triomics_error")
      testthat::expect_identical(error$code, "D_REPRODUCTION_SOURCE_INVALID")
      testthat::expect_identical(triomics_sha256(manifest_path), original_manifest_hash)
      writeBin(original_input, input_path)

      original_config <- d09_append_newline(run$fixture$config_path)
      error <- testthat::expect_error(manifest_fn(run), class = "triomics_error")
      testthat::expect_identical(error$code, "D_REPRODUCTION_SOURCE_INVALID")
      testthat::expect_identical(triomics_sha256(manifest_path), original_manifest_hash)
      writeBin(original_config, run$fixture$config_path)

      block_results_path <- file.path(run$output_root, "B", "block-results.json")
      d09_append_newline(block_results_path)
      error <- testthat::expect_error(manifest_fn(run), class = "triomics_error")
      testthat::expect_identical(error$code, "D_REPRODUCTION_SOURCE_INVALID")
      testthat::expect_identical(triomics_sha256(manifest_path), original_manifest_hash)
    }
  }
})

testthat::test_that("the installed A exporter recovers a partial checkpoint and invalidates stale inputs", {
  expected_library <- Sys.getenv("TRIOMICS_CANDIDATE_LIBRARY", unset = "")
  installed_stamp <- if (nzchar(expected_library)) {
    file.path(expected_library, "triomics", "triomics-source-revision")
  } else ""
  testthat::expect_true(nzchar(expected_library),
                        info = "The focused run must name its isolated archive-installed library.")
  testthat::expect_true(nzchar(installed_stamp) && file.exists(installed_stamp),
                        info = "The isolated installed candidate has no source revision stamp.")

  if (nzchar(expected_library) && nzchar(installed_stamp) && file.exists(installed_stamp)) {
    candidate_root <- normalizePath(expected_library, winslash = "/", mustWork = TRUE)
    package_root <- normalizePath(dirname(installed_stamp), winslash = "/", mustWork = TRUE)
    testthat::expect_true(startsWith(paste0(package_root, "/"), paste0(candidate_root, "/")))

    evidence_root <- d09_test_root("installed-restart")
    output_root <- d09_new_root("a-restart-output")
    script_path <- file.path(evidence_root, "installed-restart.R")
    result_path <- file.path(evidence_root, "installed-restart-result.json")
    script <- c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "output <- args[[1L]]",
      "result_path <- args[[2L]]",
      "expected_library <- normalizePath(args[[3L]], winslash = '/', mustWork = TRUE)",
      "package_root <- normalizePath(find.package('triomics'), winslash = '/', mustWork = TRUE)",
      "if (!startsWith(paste0(package_root, '/'), paste0(expected_library, '/'))) stop('child process loaded triomics outside the isolated candidate library')",
      "stamp <- system.file('triomics-source-revision', package = 'triomics')",
      "revision <- paste(readLines(stamp, warn = FALSE), collapse = '')",
      "request <- list(",
      "  study_id = 'd_t09_restart_fixture', design = 'independent', data_class = 'synthetic',",
      "  seed = 20261107L, n_subjects = 2L, n_features = c(prot = 2L, lip = 2L),",
      "  profiles = c(prot = 'proteomics', lip = 'lipidomics'),",
      "  capabilities = c('ingest', 'prepare', 'export'),",
      "  preparation_context_id = 'd-t09-restart', version = '0.1.0', config_token = 'original',",
      "  planted = list(prot = list(list(feature = 1L, sample = 1L, token = '3.5')),",
      "                 lip = list(list(feature = 1L, sample = 1L, token = '7.25')))",
      ")",
      "interrupted <- tryCatch(",
      "  triomics:::triomics_export_a(request, output, resume = FALSE, interrupt = 'feature:prot'),",
      "  error = function(error) error",
      ")",
      "if (!inherits(interrupted, 'triomics_error') || !identical(interrupted$code, 'A_EXPORT_INTERRUPTED')) stop('expected the producer interruption')",
      "payloads <- list.files(output, recursive = TRUE, full.names = TRUE)",
      "payloads <- payloads[grepl('/feature-prot-[0-9a-f]+\\\\.json$', payloads)]",
      "if (length(payloads) != 1L) stop('expected one interrupted feature payload')",
      "partial_path <- payloads[[1L]]",
      "partial_envelope <- sub('\\\\.json$', '.envelope.json', partial_path)",
      "if (file.exists(partial_envelope)) stop('interrupted payload unexpectedly has a complete envelope')",
      "partial_without_envelope <- !file.exists(partial_envelope)",
      "writeLines('{\"feature_keys\":[\"CORRUPT_D09_KEY\"],\"cache_key\":\"not-a-result\"}', partial_path)",
      "resumed <- triomics:::triomics_export_a(request, output, resume = TRUE)",
      "feature <- Filter(function(item) identical(item$role, 'feature') && identical(item$block_id, 'prot'), resumed$artifacts)",
      "if (length(feature) != 1L || isTRUE(feature[[1L]]$reused)) stop('partial checkpoint was reused')",
      "if ('CORRUPT_D09_KEY' %in% resumed$features$prot$feature_keys) stop('corrupt partial payload survived recovery')",
      "if (!all(startsWith(resumed$features$prot$feature_keys, 'fk1|'))) stop('recovered feature identities are invalid')",
      "replayed <- triomics:::triomics_export_a(request, output, resume = TRUE)",
      "if (!all(vapply(replayed$artifacts, function(item) isTRUE(item$reused), logical(1)))) stop('complete checkpoint was not fully reused')",
      "source_changed <- request; source_changed$planted$prot[[1L]]$token <- '8'",
      "source_update <- triomics:::triomics_export_a(source_changed, output, resume = TRUE)",
      "changed_source <- Filter(function(item) identical(item$role, 'source') && identical(item$block_id, 'prot'), source_update$artifacts)",
      "changed_feature <- Filter(function(item) identical(item$role, 'feature') && identical(item$block_id, 'prot'), source_update$artifacts)",
      "unchanged_feature <- Filter(function(item) identical(item$role, 'feature') && identical(item$block_id, 'lip'), source_update$artifacts)",
      "if (length(changed_source) != 1L || isTRUE(changed_source[[1L]]$reused) || length(changed_feature) != 1L || isTRUE(changed_feature[[1L]]$reused)) stop('source change failed to invalidate affected artifacts')",
      "if (length(unchanged_feature) != 1L || !isTRUE(unchanged_feature[[1L]]$reused)) stop('source change invalidated an unrelated block')",
      "changed <- request; changed$config_token <- 'changed-config'",
      "reconfigured <- triomics:::triomics_export_a(changed, output, resume = TRUE)",
      "config_feature <- Filter(function(item) identical(item$role, 'feature') && identical(item$block_id, 'prot'), reconfigured$artifacts)",
      "if (length(config_feature) != 1L || isTRUE(config_feature[[1L]]$reused)) stop('configuration change failed to invalidate the feature')",
      "versioned <- changed; versioned$version <- '0.1.1-test'",
      "version_update <- triomics:::triomics_export_a(versioned, output, resume = TRUE)",
      "version_feature <- Filter(function(item) identical(item$role, 'feature') && identical(item$block_id, 'prot'), version_update$artifacts)",
      "if (length(version_feature) != 1L || isTRUE(version_feature[[1L]]$reused)) stop('version change failed to invalidate the feature')",
      "seeded <- versioned; seeded$seed <- request$seed + 1L",
      "seed_update <- triomics:::triomics_export_a(seeded, output, resume = TRUE)",
      "seed_feature <- Filter(function(item) identical(item$role, 'feature') && identical(item$block_id, 'prot'), seed_update$artifacts)",
      "if (length(seed_feature) != 1L || isTRUE(seed_feature[[1L]]$reused)) stop('seed change failed to invalidate the feature')",
      "result <- list(source_revision = revision, partial_envelope_absent = partial_without_envelope,",
      "  partial_recovered = !isTRUE(feature[[1L]]$reused), complete_replay_reused = all(vapply(replayed$artifacts, function(item) isTRUE(item$reused), logical(1))),",
      "  source_invalidated = !isTRUE(changed_source[[1L]]$reused) && !isTRUE(changed_feature[[1L]]$reused),",
      "  unrelated_block_reused = isTRUE(unchanged_feature[[1L]]$reused), configuration_invalidated = !isTRUE(config_feature[[1L]]$reused),",
      "  version_invalidated = !isTRUE(version_feature[[1L]]$reused), seed_invalidated = !isTRUE(seed_feature[[1L]]$reused),",
      "  recovered_feature_count = length(resumed$features$prot$feature_keys), corrupt_key_absent = !('CORRUPT_D09_KEY' %in% resumed$features$prot$feature_keys))",
      "jsonlite::write_json(result, result_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)",
      "cat('D09_RESTART_RESULT:', as.character(jsonlite::toJSON(result, auto_unbox = TRUE)), '\\n', sep = '')"
    )
    script_parse <- tryCatch(parse(text = script), error = function(error) error)
    testthat::expect_false(inherits(script_parse, "error"),
                           info = "The installed restart subprocess script must parse before execution.")
    writeLines(script, script_path, useBytes = TRUE)
    output <- tryCatch(
      system2(file.path(R.home("bin"), "Rscript"),
              c("--vanilla", shQuote(script_path), shQuote(output_root), shQuote(result_path),
                shQuote(expected_library)),
              stdout = TRUE, stderr = TRUE),
      error = function(error) structure(conditionMessage(error), status = 127L)
    )
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    writeLines(output, file.path(evidence_root, "installed-restart.log"), useBytes = TRUE)
    testthat::expect_identical(as.integer(status), 0L, info = paste(output, collapse = "\n"))
    testthat::expect_true(file.exists(result_path))
    if (file.exists(result_path)) {
      result <- jsonlite::read_json(result_path, simplifyVector = FALSE)
      testthat::expect_identical(as.character(result$source_revision),
                                 paste(readLines(installed_stamp, warn = FALSE), collapse = ""))
      testthat::expect_true(isTRUE(result$partial_envelope_absent))
      testthat::expect_true(isTRUE(result$partial_recovered))
      testthat::expect_true(isTRUE(result$complete_replay_reused))
      testthat::expect_true(isTRUE(result$source_invalidated))
      testthat::expect_true(isTRUE(result$unrelated_block_reused))
      testthat::expect_true(isTRUE(result$configuration_invalidated))
      testthat::expect_true(isTRUE(result$version_invalidated))
      testthat::expect_true(isTRUE(result$seed_invalidated))
      testthat::expect_identical(as.integer(result$recovered_feature_count), 2L)
      testthat::expect_true(isTRUE(result$corrupt_key_absent))
    }
  }
})
