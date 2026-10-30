test_that("a stage with no declared annotation routes exits complete", {
  evidence <- list(immutable_inputs = list(
    b_bundle_sha256 = paste(rep("a", 64), collapse = ""),
    c_manifest_sha256 = paste(rep("b", 64), collapse = "")
  ))
  result <- triomics_d_cli_route_status(evidence)
  expect_identical(result$run_completeness, "complete")
  expect_identical(result$exit_code, 0L)
  expect_false(result$biological_absence)
  expect_identical(result$predecessor, list(
    b_sha256 = evidence$immutable_inputs$b_bundle_sha256,
    c_sha256 = evidence$immutable_inputs$c_manifest_sha256
  ))
  expect_identical(result$capability_status, list(mofa_native = "incomplete", biopan = "incomplete"))
  expect_identical(length(result$unavailable_routes), 0L)
})
