c09_shared_fixture_path <- function() {
  triomics_test_resource("examples", "tickets", "C-09.yaml")
}

c09_shared_variant_path <- function(change) {
  config <- yaml::read_yaml(c09_shared_fixture_path())
  config <- change(config)
  path <- tempfile("triomics-c09-config-", fileext = ".yaml")
  yaml::write_yaml(config, path)
  path
}

c09_shared_config_error <- function(path) {
  tryCatch(triomics_read_config(path), error = identity)
}

c09_shared_cli_run <- function(output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  config <- c09_shared_fixture_path()
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config),
      "--stage", "C", "--output", shQuote(output_root)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = output)
}

testthat::test_that("C-09 accepts only the closed declared synthetic fixture config", {
  config <- triomics_read_config(c09_shared_fixture_path())
  testthat::expect_named(config, c("schema_version", "study_id", "data_class", "clinical_prediction"))
  testthat::expect_identical(config$data_class, "synthetic")
  testthat::expect_identical(
    config$clinical_prediction$synthetic_fixture$generator,
    "triomics::triomics_c09_synthetic_inputs"
  )
})

testthat::test_that("C-09 accepts protocol mappings reordered at top and nested levels", {
  path <- c09_shared_variant_path(function(config) {
    protocol <- config$clinical_prediction$protocol
    config$clinical_prediction$protocol <- protocol[rev(names(protocol))]
    config$clinical_prediction$protocol$response_model <-
      config$clinical_prediction$protocol$response_model[
        rev(names(config$clinical_prediction$protocol$response_model))
      ]
    config
  })
  on.exit(unlink(path), add = TRUE)

  config <- triomics_read_config(path)
  inputs <- triomics::triomics_c09_synthetic_inputs(
    seed = 90210L, n_subjects = 32L,
    endpoint_method = "stats::prcomp", missing_outcome = FALSE
  )
  error <- tryCatch({
    triomics_c09_cli_validate_predecessors(config$clinical_prediction, inputs)
    NULL
  }, error = identity)
  if (!is.null(error)) {
    testthat::expect_identical(error$code, "C_C09_PROTOCOL_INVALID")
  }
  testthat::expect_null(
    error,
    info = paste(
      "Equivalent protocol mappings were rejected with",
      if (inherits(error, "triomics_error")) error$code else "no typed code"
    )
  )
})

testthat::test_that("C-09 protocol comparison rejects changed scalars and sequence values or order", {
  config <- triomics_read_config(c09_shared_fixture_path())
  inputs <- triomics::triomics_c09_synthetic_inputs(
    seed = 90210L, n_subjects = 32L,
    endpoint_method = "stats::prcomp", missing_outcome = FALSE
  )
  variants <- list(
    scalar = function(protocol) {
      protocol$endpoint_id <- "altered_response_30d"
      protocol
    },
    sequence_order = function(protocol) {
      protocol$response_model$metric_names <- rev(protocol$response_model$metric_names)
      protocol
    },
    sequence_content = function(protocol) {
      protocol$response_model$metric_names[[3L]] <- "adjusted_r_squared"
      protocol
    }
  )

  for (variant in names(variants)) {
    declared <- config$clinical_prediction
    declared$protocol <- variants[[variant]](declared$protocol)
    error <- tryCatch(
      triomics_c09_cli_validate_predecessors(declared, inputs),
      error = identity
    )
    testthat::expect_s3_class(error, "triomics_error")
    testthat::expect_identical(error$code, "C_C09_PROTOCOL_INVALID", info = variant)
  }
})

testthat::test_that("C-09 rejects an unknown nested protocol key at its declared path", {
  path <- c09_shared_variant_path(function(config) {
    config$clinical_prediction$protocol$response_model$surprise <- TRUE
    config
  })
  on.exit(unlink(path), add = TRUE)

  error <- c09_shared_config_error(path)
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "A_CONFIG_UNKNOWN_KEY")
  testthat::expect_identical(
    error$config_key,
    "clinical_prediction.protocol.response_model.surprise"
  )
})

testthat::test_that("C-09 rejects mixed ordinary-ingest and synthetic-only config forms", {
  path <- c09_shared_variant_path(function(config) {
    config$sources <- list()
    config
  })
  on.exit(unlink(path), add = TRUE)

  error <- c09_shared_config_error(path)
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_identical(error$code, "A_CONFIG_SCHEMA")
})

testthat::test_that("C-09 rejects missing, wrong-type, non-synthetic, and unqualified-real declarations", {
  variants <- list(
    missing_protocol_field = function(config) {
      config$clinical_prediction$predecessor_contracts$score_endpoint$clinical_score_request_execution <- NULL
      config
    },
    wrong_type = function(config) {
      config$clinical_prediction$enabled <- "true"
      config
    },
    non_synthetic = function(config) {
      config$data_class <- "real"
      config
    },
    unqualified_real = function(config) {
      config$clinical_prediction$protocol$deployment$real_deployment_status <- "qualified"
      config
    }
  )

  for (variant in names(variants)) {
    path <- c09_shared_variant_path(variants[[variant]])
    error <- c09_shared_config_error(path)
    unlink(path)
    testthat::expect_s3_class(error, "triomics_error")
    testthat::expect_true(startsWith(error$code, "A_CONFIG_"), info = variant)
  }
})

testthat::test_that("C-09 rejects an altered synthetic generator declaration", {
  path <- c09_shared_variant_path(function(config) {
    config$clinical_prediction$synthetic_fixture$generator <- "base::system"
    config
  })
  on.exit(unlink(path), add = TRUE)

  error <- c09_shared_config_error(path)
  testthat::expect_s3_class(error, "triomics_error")
  testthat::expect_true(startsWith(error$code, "A_CONFIG_"))
  testthat::expect_identical(error$config_key, "clinical_prediction.synthetic_fixture.generator")
})

testthat::test_that("the public C-09 generator returns validated matched predecessor inputs", {
  inputs <- triomics::triomics_c09_synthetic_inputs(
    seed = 90210L, n_subjects = 32L,
    endpoint_method = "stats::prcomp", missing_outcome = FALSE
  )

  testthat::expect_s3_class(inputs, "C09SyntheticInputs")
  testthat::expect_named(inputs$candidates, c("clinical_only", "molecular_only", "combined"))
  testthat::expect_identical(inputs$split_plan$schema, "SplitPlan")
  testthat::expect_identical(inputs$split_plan$grouping_unit, "subject")
  testthat::expect_equal(inputs$split_plan$n_outer, 2L)
  testthat::expect_equal(inputs$split_plan$n_inner, 2L)
  testthat::expect_identical(inputs$split_plan$target, "synthetic_response_30d")
  testthat::expect_invisible(triomics_validate_split_plan(inputs$split_plan, TRUE))
  testthat::expect_invisible(triomics_c09_validate_protocol(inputs$protocol, inputs$split_plan))
  testthat::expect_invisible(triomics_c09_validate_c08(
    inputs$endpoint_bundle, inputs$clinical_score_request, inputs$split_plan
  ))
})

testthat::test_that("the C-09 Stage-C CLI persists hashed matched-comparison evidence", {
  output_root <- tempfile("triomics-c09-shared-cli-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  cli <- c09_shared_cli_run(output_root)
  testthat::expect_identical(cli$status, 0L, info = paste(cli$output, collapse = "\n"))

  if (identical(cli$status, 0L)) {
    payload_path <- file.path(output_root, "C", "clinical-prediction-comparison.json")
    envelope_path <- file.path(output_root, "C", "clinical-prediction-comparison.envelope.json")
    registry_path <- file.path(output_root, "registries", "run.json")
    testthat::expect_true(file.exists(payload_path))
    testthat::expect_true(file.exists(envelope_path))
    testthat::expect_true(file.exists(registry_path))

    payload <- jsonlite::read_json(payload_path, simplifyVector = FALSE)
    envelope <- jsonlite::read_json(envelope_path, simplifyVector = FALSE)
    registry <- jsonlite::read_json(registry_path, simplifyVector = FALSE)
    config <- triomics_read_config(c09_shared_fixture_path())
    inputs <- triomics::triomics_c09_synthetic_inputs(
      seed = 90210L, n_subjects = 32L,
      endpoint_method = "stats::prcomp", missing_outcome = FALSE
    )

    testthat::expect_true(triomics_validate_envelope(envelope_path))
    testthat::expect_identical(envelope$artifact_type, "clinical_prediction_comparison")
    testthat::expect_identical(envelope$privacy_class, "synthetic")
    testthat::expect_identical(envelope$config_sha256, attr(config, "config_sha256"))
    testthat::expect_identical(envelope$payload_sha256, triomics_sha256(payload_path))
    testthat::expect_true(triomics_sha256_text(triomics_sha256(envelope_path)))
    testthat::expect_identical(payload$study_id, config$study_id)
    testthat::expect_named(payload$candidates, c("clinical_only", "molecular_only", "combined"))
    testthat::expect_identical(payload$split_plan_hash, inputs$split_plan$creation_hash)
    testthat::expect_identical(payload$predecessor_identity$split_plan$creation_hash,
                               inputs$split_plan$creation_hash)
    testthat::expect_identical(payload$predecessor_identity$c08_endpoint$endpoint_id,
                               "score_c09_synthetic_pc1")
    testthat::expect_identical(payload$predecessor_identity$c08_endpoint_bundle$payload_sha256,
                               inputs$endpoint_bundle$payload_sha256)
    testthat::expect_identical(payload$predecessor_identity$clinical_score_request$request_sha256,
                               inputs$clinical_score_request$request_sha256)
    testthat::expect_true(all(vapply(payload$candidates, function(candidate) {
      denominator <- candidate$denominator
      denominator$total == 32 &&
        denominator$predicted + denominator$failed + denominator$ineligible == denominator$total
    }, logical(1L))))

    delta <- payload$incremental$combined_vs_clinical_only$rmse
    testthat::expect_identical(delta$uncertainty_method,
                               "paired_subject_cluster_bootstrap_percentile_type7")
    testthat::expect_length(unlist(delta$interval, use.names = FALSE), 2L)
    testthat::expect_true(is.finite(delta$estimate))
    testthat::expect_false(payload$benefit_guaranteed)
    testthat::expect_identical(payload$qualification$evidence_scope, "synthetic_software_only")
    testthat::expect_identical(payload$qualification$real_endpoint_protocol, "not_supplied")
    testthat::expect_identical(payload$qualification$real_deployment, "not_qualified")
    testthat::expect_identical(payload$qualification$independent_validation, "not_performed")

    testthat::expect_identical(registry$study_id, config$study_id)
    testthat::expect_identical(registry$status, "complete")
    testthat::expect_identical(registry$status_scope, "synthetic_fixture_only")
    testthat::expect_identical(registry$artifact_ref$payload_sha256, triomics_sha256(payload_path))
    testthat::expect_identical(registry$artifact_ref$envelope_sha256, triomics_sha256(envelope_path))
  }
})

testthat::test_that("re-running C-09 into its nonempty output directory refuses without mutation", {
  output_root <- tempfile("triomics-c09-shared-collision-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  first <- c09_shared_cli_run(output_root)
  testthat::expect_identical(first$status, 0L, info = paste(first$output, collapse = "\n"))

  if (identical(first$status, 0L)) {
    payload_path <- file.path(output_root, "C", "clinical-prediction-comparison.json")
    envelope_path <- file.path(output_root, "C", "clinical-prediction-comparison.envelope.json")
    registry_path <- file.path(output_root, "registries", "run.json")
    before <- vapply(c(payload_path, envelope_path, registry_path), triomics_sha256, character(1L))
    second <- c09_shared_cli_run(output_root)

    testthat::expect_false(identical(second$status, 0L), info = paste(second$output, collapse = "\n"))
    testthat::expect_true(any(grepl("A_ARTIFACT_IMMUTABLE|A_ARTIFACT_FINALIZE", second$output)))
    after <- vapply(c(payload_path, envelope_path, registry_path), triomics_sha256, character(1L))
    testthat::expect_identical(after, before)
  }
})
