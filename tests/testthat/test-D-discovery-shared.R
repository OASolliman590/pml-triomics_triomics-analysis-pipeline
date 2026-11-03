d12_shared_code <- function(expr) tryCatch({ force(expr); NA_character_ }, triomics_error = function(e) e$code)

test_that("the Stage D discovery marker preserves overlap qualifications during attachment", {
  prepared <- list(
    status = "recorded", mode = "discover", selected_target = NULL,
    discovery = list(
      format = "triomics.target-discovery", format_version = "1.0", mode = "discover",
      enumeration = "unranked",
      universe = list(universe_id = "synthetic-universe", version = "1.0",
                      sha256 = paste(rep("a", 64L), collapse = ""), member_count = 1L,
                      included_count = 1L, excluded_count = 0L),
      protocol = list(protocol_id = "synthetic-protocol", version = "1.0",
                      sha256 = paste(rep("b", 64L), collapse = "")),
      members = list(list(member_id = "member-a", member_sha256 = paste(rep("c", 64L), collapse = ""),
                          inclusion_status = "included", reason = "declared", identity = list(
                            namespace = "synthetic", id = "TARGET_A", taxon_id = 9606,
                            entity_type = "protein")))
    ),
    profiles = list(list(evidence_axes = list(
      metabolic_multiomics_compatibility = list(status = "unassessable")
    ))),
    alternatives = NULL, results = list()
  )

  attached <- triomics_d_attach_target_profiles(prepared, list(enabled = TRUE, mode = "discover"))
  expect_identical(attached, prepared)
  expect_identical(attached$profiles[[1L]]$evidence_axes$metabolic_multiomics_compatibility$status,
                   "unassessable")
})

test_that("the Stage D discovery attachment rejects an untyped prepared result", {
  malformed <- list(status = "recorded", profiles = list(), results = list())
  expect_identical(d12_shared_code(
    triomics_d_attach_target_profiles(malformed, list(enabled = TRUE, mode = "discover"))
  ), "D_TARGET_DISCOVERY_INVALID")
})
