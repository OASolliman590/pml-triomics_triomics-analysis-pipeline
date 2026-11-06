d13_target_profile <- function(target_id = "TARGET_A") {
  observation <- list(
    observation_id = "B:result-1",
    study_id = "fixture-study",
    analysis_id = "fixture-analysis",
    source_branch = "primary_statistics",
    source_id = "block_public",
    source_sha256 = paste(rep("a", 64L), collapse = ""),
    source_selection_history = "full_unselected",
    feature_key = "feature-1",
    feature_namespace = "synthetic",
    feature_id = "FEATURE_1",
    endpoint = "statistical_effect",
    value = -0.25,
    effect_unit = "per_unit_beta",
    status = "conflict",
    dependency_group = "molecular_feature:feature-1",
    link_id = "path-1",
    path_context = "synthetic_context",
    path_sign = "unknown",
    coverage = "measured",
    interpretation = "descriptive_compatibility_only"
  )
  assay <- list(
    evidence_id = "assay-1",
    axis = "binding",
    endpoint = "Kd",
    units = "nM",
    relation = "=",
    value = NULL,
    source_id = "public_assay_source",
    publication_group = "publication-1",
    dependency_group = "publication:publication-1",
    measured_by_pipeline = FALSE,
    target_namespace = "synthetic_protein",
    target_id = target_id,
    taxon_id = 9606,
    context_id = "synthetic_context",
    construct = "construct-1",
    compound_namespace = "synthetic_compound",
    compound_id = "compound-1",
    assay = "surface assay",
    assay_limitations = "Synthetic assay record; endpoint unavailable.",
    attachment = "request",
    target_identity_status = "matched",
    availability = "unavailable",
    assay_qualification = "qualified",
    independent_replication = FALSE
  )
  compatibility <- list(
    status = "measured",
    reason_code = NULL,
    coverage = list(expected_links = 1L, measured_features = 1L,
                    unmeasured_features = 0L),
    links = list(list(
      link_id = "path-1", relation = "regulatory", sign = "unknown",
      expected_direction = "positive", context_id = "synthetic_context",
      source_id = "mechanism-source", feature_namespace = "synthetic",
      feature_id = "FEATURE_1"
    )),
    observations = list(observation),
    independent_group_count = 1L
  )
  external_axis <- list(
    status = "unresolved",
    reason_code = "D_TARGET_IDENTITY_UNRESOLVED",
    coverage = list(expected_records = 1L, measured_records = 0L,
                    unmeasured_records = 1L, unresolved_records = 0L,
                    independent_publication_groups = 1L),
    evidence = list(assay)
  )
  list(
    target_id = target_id,
    request_id = paste0("target:", target_id),
    request = list(
      namespace = "synthetic_protein", id = target_id, taxon_id = 9606,
      entity_type = "protein", mode = "assess", binding_target = TRUE,
      mechanism_class = "non_enzymatic"
    ),
    mechanism_class = "non_enzymatic",
    mechanism_status = "assessable",
    mechanism_declaration_status = "supplied",
    mechanism_declarations = list(),
    coverage = "measured",
    evidence_axes = list(
      metabolic_multiomics_compatibility = compatibility,
      functional_perturbation = list(
        status = "unmeasured", reason_code = "D_TARGET_EVIDENCE_ABSENT",
        coverage = list(expected_records = 0L, measured_records = 0L,
                        unmeasured_records = 0L, unresolved_records = 0L,
                        independent_publication_groups = 0L), evidence = list()
      ),
      cellular_tissue_engagement = list(
        status = "unmeasured", reason_code = "D_TARGET_EVIDENCE_ABSENT",
        coverage = list(expected_records = 0L, measured_records = 0L,
                        unmeasured_records = 0L, unresolved_records = 0L,
                        independent_publication_groups = 0L), evidence = list()
      ),
      direct_binding = external_axis
    ),
    links = compatibility$links,
    observations = list(observation),
    external_evidence = list(assay),
    external_associations = list(),
    independent_count = 1L,
    input_provenance = list(
      external_evidence_file = list(
        status = "available", reference = "/private/study/assays.json",
        sha256 = paste(rep("b", 64L), collapse = "")
      )
    )
  )
}

d13_recorded_target <- function(profiles = list(d13_target_profile()), mode = "assess",
                                alternatives = NULL, discovery = NULL) {
  list(
    status = "recorded", reason_code = NULL, mode = mode,
    results = lapply(profiles, function(profile) list(request = profile$request)),
    profiles = profiles,
    alternatives = alternatives,
    discovery = discovery,
    selected_target = NULL,
    tie_status = NULL,
    input_provenance = list()
  )
}

testthat::test_that("target report composition preserves four axes and unavailable assay values", {
  loaded <- list(d = list(target_investigation = d13_recorded_target()))
  sections <- triomics_report_optional_sections(
    loaded, list(target_enabled = TRUE, clinical_enabled = FALSE)
  )

  testthat::expect_identical(sections$targets$status, "complete")
  axes <- sections$targets$tables$evidence_axes
  testthat::expect_setequal(axes$axis, c(
    "metabolic_multiomics_compatibility", "functional_perturbation",
    "cellular_tissue_engagement", "direct_binding"
  ))
  binding <- sections$targets$tables$external_evidence
  testthat::expect_identical(binding$endpoint, "Kd")
  testthat::expect_identical(binding$units, "nM")
  testthat::expect_true(is.na(binding$value))
  testthat::expect_identical(binding$value_status, "unavailable")
  path <- sections$targets$tables$mechanistic_context
  testthat::expect_identical(path$relation, "regulatory")
  testthat::expect_identical(path$sign, "unknown")
  testthat::expect_identical(path$expected_direction, "positive")
  testthat::expect_identical(sections$targets$tables$observations$status, "conflict")
  testthat::expect_identical(sections$targets$tables$observations$path_sign, "unknown")
  testthat::expect_identical(sections$targets$tables$dossier$quantitative_method_status,
                             "conditional_unapproved")
  testthat::expect_identical(sections$targets$tables$dossier$d14_status, "not_completed")
  testthat::expect_identical(sections$targets$tables$dossier$project_acceptance_status,
                             "not_assessed")
})

testthat::test_that("discovery report retains exploratory selection history and overlap status", {
  profile <- d13_target_profile()
  profile$request$mode <- "discover"
  profile$discovery <- list(
    status = "exploratory_unranked", universe_id = "universe-1", universe_version = "1",
    enumeration = "unranked", member_id = "member-a",
    reference_overlap = list(status = "unknown", reason_code = "D_TARGET_REFERENCE_OVERLAP_UNKNOWN")
  )
  history <- list(list(
    selection_id = "prior-selection-a", hypothesis_status = "confirmed",
    selected_with_observations = FALSE, selection_source = "prior-external-study",
    selection_evidence_ref = "prior-study-evidence"
  ))
  member <- list(
    member_id = "member-a", member_sha256 = paste(rep("c", 64L), collapse = ""),
    inclusion_status = "included", reason = "Included by the declared finite universe.",
    identity = list(id = "TARGET_A"), prior_selection_history = history,
    reference_overlap = list(status = "unknown", reason_code = "D_TARGET_REFERENCE_OVERLAP_UNKNOWN",
                             evidence = list())
  )
  discovery <- list(
    enumeration = "unranked",
    universe = list(universe_id = "universe-1", version = "1",
                    sha256 = paste(rep("d", 64L), collapse = "")),
    protocol = list(protocol_id = "protocol-1", sha256 = paste(rep("e", 64L), collapse = "")),
    members = list(member)
  )
  target <- d13_recorded_target(list(profile), mode = "discover", discovery = discovery)
  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = target)),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets

  testthat::expect_identical(section$status, "complete")
  testthat::expect_identical(section$tables$discovery$discovery_status, "exploratory_unranked")
  testthat::expect_identical(section$tables$discovery$reference_overlap_status, "unknown")
  history_rows <- section$tables$selection_history
  testthat::expect_identical(history_rows$selection_id, "prior-selection-a")
  testthat::expect_identical(history_rows$hypothesis_status, "confirmed")
  testthat::expect_identical(history_rows$selected_with_observations, FALSE)
  testthat::expect_identical(section$tables$profiles$subject_overlap_status,
                             "unknown_not_recorded")
})

testthat::test_that("compare mode reports noncomparability and unresolved ties without ranking", {
  first <- d13_target_profile("TARGET_A")
  second <- d13_target_profile("TARGET_B")
  first$request$mode <- "compare"
  second$request$mode <- "compare"
  axes <- as.list(names(first$evidence_axes))
  alternatives <- list(
    comparability = "noncomparable", comparison_axes = axes, tie_status = "unresolved",
    selected_target = NULL, profiles = list(first, second)
  )
  target <- d13_recorded_target(list(first, second), mode = "compare", alternatives = alternatives)
  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = target)),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets

  testthat::expect_identical(section$status, "complete")
  testthat::expect_setequal(section$tables$alternatives$target_id, c("TARGET_A", "TARGET_B"))
  testthat::expect_true(all(section$tables$alternatives$comparability == "noncomparable"))
  testthat::expect_true(all(section$tables$alternatives$tie_status == "unresolved"))
  testthat::expect_true(all(section$tables$alternatives$selected_target == "not_selected"))
  testthat::expect_false("rank" %in% names(section$tables$alternatives))

  alternatives$comparability <- "comparable"
  comparable <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(
      list(first, second), mode = "compare", alternatives = alternatives
    ))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets$tables$alternatives
  testthat::expect_true(all(comparable$comparability == "comparable"))
  testthat::expect_true(all(comparable$comparison_axes == paste(axes, collapse = "; ")))
})

testthat::test_that("discovery ledger retains excluded members outside the profile set", {
  profile <- d13_target_profile()
  profile$request$mode <- "discover"
  profile$discovery <- list(
    status = "exploratory_unranked", universe_id = "universe-1", universe_version = "1",
    enumeration = "unranked", member_id = "member-a",
    reference_overlap = list(status = "not_applicable", evidence = list())
  )
  included <- list(
    member_id = "member-a", member_sha256 = paste(rep("c", 64L), collapse = ""),
    inclusion_status = "included", reason = "Included by the declared finite universe.",
    identity = list(id = "TARGET_A"), prior_selection_history = list(),
    reference_overlap = list(status = "not_applicable", evidence = list())
  )
  excluded <- list(
    member_id = "member-c", member_sha256 = paste(rep("f", 64L), collapse = ""),
    inclusion_status = "excluded", reason = "Excluded by the declared eligibility rule.",
    identity = list(id = "TARGET_C"),
    prior_selection_history = list(list(
      selection_id = "excluded-prior", hypothesis_status = "exploratory",
      selected_with_observations = TRUE, selection_source = "prior-enumeration"
    )),
    reference_overlap = list(status = "unknown", reason_code = "D_TARGET_REFERENCE_OVERLAP_UNKNOWN",
                             evidence = list())
  )
  discovery <- list(
    enumeration = "unranked",
    universe = list(universe_id = "universe-1", version = "1",
                    sha256 = paste(rep("d", 64L), collapse = "")),
    protocol = list(protocol_id = "protocol-1", sha256 = paste(rep("e", 64L), collapse = "")),
    members = list(included, excluded)
  )
  target <- d13_recorded_target(list(profile), mode = "discover", discovery = discovery)
  tables <- triomics_report_optional_sections(
    list(d = list(target_investigation = target)),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets$tables

  testthat::expect_setequal(tables$discovery$member_id, c("member-a", "member-c"))
  excluded_row <- tables$discovery[tables$discovery$member_id == "member-c", , drop = FALSE]
  testthat::expect_identical(excluded_row$inclusion_status, "excluded")
  testthat::expect_identical(excluded_row$reason, "Excluded by the declared eligibility rule.")
  testthat::expect_identical(tables$selection_history$selection_id, "excluded-prior")
  testthat::expect_identical(tables$selection_history$selected_with_observations, TRUE)

  all_excluded <- d13_recorded_target(list(), mode = "discover", discovery = list(
    enumeration = "unranked",
    universe = discovery$universe,
    protocol = discovery$protocol,
    members = list(excluded)
  ))
  all_excluded_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = all_excluded)),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  testthat::expect_equal(all_excluded_section$tables$dossier$profile_count, 0)
  testthat::expect_identical(all_excluded_section$tables$discovery$member_id, "member-c")
  testthat::expect_identical(all_excluded_section$tables$discovery$inclusion_status, "excluded")
})

testthat::test_that("same-study B and C observations keep shared source and dependency lineage", {
  profile <- d13_target_profile()
  second <- profile$observations[[1L]]
  second$observation_id <- "C:component-1"
  second$source_branch <- "integration_selection"
  second$analysis_id <- "fixture-integration"
  profile$observations <- c(profile$observations, list(second))
  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets

  testthat::expect_equal(nrow(section$tables$source_lineage), 2L)
  testthat::expect_identical(section$tables$profiles$study_ids, "fixture-study")
  testthat::expect_equal(section$tables$profiles$dependency_group_count, 1L)
  testthat::expect_true(all(section$tables$source_lineage$source_sha256 == paste(rep("a", 64L), collapse = "")))
  testthat::expect_true(all(section$tables$source_lineage$dependency_group ==
                              "molecular_feature:feature-1"))
  testthat::expect_identical(section$tables$profiles$subject_overlap_status,
                             "unknown_not_recorded")
  testthat::expect_true(grepl("not independent replication",
                              paste(section$tables$limitations$limit, collapse = " "),
                              fixed = TRUE))
  testthat::expect_false("independent_count" %in% names(section$tables$profiles))
})

testthat::test_that("association-only producer profiles retain safe study and construction lineage", {
  request <- triomics_target_request(
    list(namespace = "synthetic_protein", id = "TARGET_ASSOCIATION", taxon_id = 9606,
         entity_type = "protein"),
    "assess",
    list(list(selection_id = "association-selection", hypothesis_status = "exploratory",
              selected_with_observations = FALSE)),
    context = list(context_id = "association-context")
  )
  association <- list(
    evidence_id = "association-evidence-1", axis = "association",
    endpoint = "association_beta", units = "unitless", relation = "=",
    source_id = "association-source", publication_group = "association-publication",
    measured_by_pipeline = FALSE, value = NULL,
    study_id = "synthetic_association_study", analysis_id = "synthetic_association_analysis",
    subject_overlap_status = "shared_subjects",
    construction_lineage = "clinical_association_source",
    source_selection_history = "full_unselected",
    raw_subject_ids = list("PRIVATE_SUBJECT_13"),
    local_path = "/private/association/source.tsv",
    unapproved_note = "<script>PRIVATE_ASSOCIATION_NOTE</script>"
  )
  external <- triomics_target_external_evidence(request, list(association))
  profile <- triomics_target_profile(request, list(), list(), external_evidence = external)
  testthat::expect_identical(profile$external_associations[[1L]]$study_id,
                             "synthetic_association_study")
  testthat::expect_identical(profile$external_associations[[1L]]$analysis_id,
                             "synthetic_association_analysis")
  testthat::expect_identical(profile$external_associations[[1L]]$subject_overlap_status,
                             "shared_subjects")
  testthat::expect_identical(profile$external_associations[[1L]]$construction_lineage,
                             "clinical_association_source")
  testthat::expect_identical(profile$external_associations[[1L]]$source_selection_history,
                             "full_unselected")

  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  testthat::expect_identical(section$tables$profiles$study_ids,
                             "synthetic_association_study")
  testthat::expect_equal(section$tables$profiles$study_count, 1L)
  testthat::expect_identical(section$tables$profiles$subject_overlap_status,
                             "shared_subjects")
  external_rows <- section$tables$external_evidence
  testthat::expect_identical(external_rows$study_id, "synthetic_association_study")
  testthat::expect_identical(external_rows$analysis_id, "synthetic_association_analysis")
  testthat::expect_identical(external_rows$subject_overlap_status, "shared_subjects")
  testthat::expect_identical(external_rows$construction_lineage,
                             "clinical_association_source")
  testthat::expect_identical(external_rows$source_selection_history, "full_unselected")
  testthat::expect_identical(external_rows$independent_replication, FALSE)
  testthat::expect_false(any(c("raw_subject_ids", "local_path", "unapproved_note") %in%
                             names(external_rows)))
  testthat::expect_false(grepl(
    "PRIVATE_SUBJECT_13|PRIVATE_ASSOCIATION_NOTE|/private/association/source.tsv|<script",
    paste(unlist(external_rows, use.names = FALSE), collapse = " ")
  ))

  missing_lineage <- association[setdiff(names(association), c(
    "subject_overlap_status", "construction_lineage", "source_selection_history"
  ))]
  unknown_request <- triomics_target_request(
    list(namespace = "synthetic_protein", id = "TARGET_ASSOCIATION_UNKNOWN", taxon_id = 9606,
         entity_type = "protein"),
    "assess",
    list(list(selection_id = "association-selection-unknown", hypothesis_status = "exploratory",
              selected_with_observations = FALSE)),
    context = list(context_id = "association-context")
  )
  unknown_profile <- triomics_target_profile(
    unknown_request, list(), list(),
    external_evidence = triomics_target_external_evidence(unknown_request, list(missing_lineage))
  )
  unknown_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(unknown_profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  testthat::expect_identical(unknown_section$tables$profiles$subject_overlap_status,
                             "unknown_not_recorded")
  testthat::expect_identical(unknown_section$tables$external_evidence$construction_lineage, "")
  testthat::expect_identical(unknown_section$tables$external_evidence$source_selection_history, "")

  prose_association <- association
  prose_association$construction_lineage <-
    "Clinical score constructed from the same molecular features"
  prose_association$source_selection_history <-
    "All eligible features; no outcome selection"
  prose_profile <- triomics_target_profile(
    request, list(), list(),
    external_evidence = triomics_target_external_evidence(request, list(prose_association))
  )
  testthat::expect_identical(prose_profile$external_associations[[1L]]$construction_lineage,
                             "Clinical score constructed from the same molecular features")
  testthat::expect_identical(prose_profile$external_associations[[1L]]$source_selection_history,
                             "All eligible features; no outcome selection")
  prose_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(prose_profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  testthat::expect_identical(
    prose_section$tables$external_evidence$construction_lineage,
    "Clinical score constructed from the same molecular features"
  )
  testthat::expect_identical(
    prose_section$tables$external_evidence$source_selection_history,
    "All eligible features; no outcome selection"
  )

  numeric_association <- association
  numeric_association$construction_lineage <-
    "Clinical score constructed from 3 molecular features"
  numeric_association$source_selection_history <-
    "Top 10 features selected in the same study"
  numeric_profile <- triomics_target_profile(
    request, list(), list(),
    external_evidence = triomics_target_external_evidence(request, list(numeric_association))
  )
  numeric_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(numeric_profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  testthat::expect_identical(
    numeric_profile$external_associations[[1L]]$construction_lineage,
    numeric_association$construction_lineage
  )
  testthat::expect_identical(
    numeric_profile$external_associations[[1L]]$source_selection_history,
    numeric_association$source_selection_history
  )
  testthat::expect_identical(
    numeric_section$tables$external_evidence$construction_lineage,
    numeric_association$construction_lineage
  )
  testthat::expect_identical(
    numeric_section$tables$external_evidence$source_selection_history,
    numeric_association$source_selection_history
  )

  numeric_identifier_association <- numeric_association
  numeric_identifier_association$construction_lineage <- "Patient ID 13 clinical score"
  numeric_identifier_association$source_selection_history <- "Subject 7 features selected"
  numeric_identifier_profile <- triomics_target_profile(
    request, list(), list(),
    external_evidence = triomics_target_external_evidence(
      request, list(numeric_identifier_association)
    )
  )
  numeric_identifier_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(numeric_identifier_profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  testthat::expect_identical(
    numeric_identifier_section$tables$external_evidence$construction_lineage,
    ""
  )
  testthat::expect_identical(
    numeric_identifier_section$tables$external_evidence$source_selection_history,
    ""
  )

  unsafe_association <- association
  unsafe_association$construction_lineage <-
    "Clinical score constructed from /private/association/source.tsv"
  unsafe_association$source_selection_history <-
    "<script>PRIVATE_SELECTION_NOTE</script>"
  unsafe_profile <- triomics_target_profile(
    request, list(), list(),
    external_evidence = triomics_target_external_evidence(request, list(unsafe_association))
  )
  unsafe_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(unsafe_profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  unsafe_rows <- unsafe_section$tables$external_evidence
  testthat::expect_identical(unsafe_rows$construction_lineage, "")
  testthat::expect_identical(unsafe_rows$source_selection_history, "")
  testthat::expect_false(grepl(
    "PRIVATE_SELECTION_NOTE|/private/association/source.tsv|<script",
    paste(unlist(unsafe_rows, use.names = FALSE), collapse = " ")
  ))

  nested_association <- association
  nested_association$construction_lineage <- list(
    subject_ids = list("PRIVATE_SUBJECT_13")
  )
  nested_association$source_selection_history <- "PRIVATE_SUBJECT_13"
  nested_profile <- triomics_target_profile(
    request, list(), list(),
    external_evidence = triomics_target_external_evidence(request, list(nested_association))
  )
  nested_section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(nested_profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  nested_rows <- nested_section$tables$external_evidence
  testthat::expect_identical(nested_rows$construction_lineage, "")
  testthat::expect_identical(nested_rows$source_selection_history, "")
  testthat::expect_false(grepl(
    "PRIVATE_SUBJECT_13",
    paste(unlist(nested_rows, use.names = FALSE), collapse = " ")
  ))
})

testthat::test_that("D-T13 association lineage preserves descriptions and rejects identifiers end to end", {
  request <- triomics_target_request(
    list(namespace = "synthetic_protein", id = "TARGET_ASSOCIATION_PRIVACY", taxon_id = 9606,
         entity_type = "protein"),
    "assess",
    list(list(selection_id = "association-selection-privacy", hypothesis_status = "exploratory",
              selected_with_observations = FALSE)),
    context = list(context_id = "association-context-privacy")
  )
  association <- list(
    evidence_id = "association-evidence-privacy", axis = "association",
    endpoint = "association_beta", units = "unitless", relation = "=",
    source_id = "association-source", publication_group = "association-publication",
    measured_by_pipeline = FALSE, value = NULL,
    study_id = "synthetic_association_study", analysis_id = "synthetic_association_analysis",
    subject_overlap_status = "shared_subjects",
    construction_lineage = NULL,
    source_selection_history = NULL
  )
  safe <- c(
    "Clinical score constructed from 3 molecular features",
    "Top 10 features selected in the same study",
    "Score constructed from 3 features across 32 subjects",
    "PCA v2.1 with 10 features",
    "Sample count 32; 10 features selected",
    "Subject count 32; 10 features selected",
    "Sample-wise normalization of 32 measurements",
    "Sample-level score constructed from 3 features",
    "Sample-level splitting with 10 features",
    "Sample-wise median normalization of 32 measurements",
    "Sample-level feature validation with 10 features",
    "Sample-based score constructed from 3 features",
    "Sample-level PCA v2.1 with 10 features"
  )
  safe <- unique(c(
    safe,
    toupper(safe),
    "Sample-Level score constructed from 3 features",
    "Sample-Wise normalization of 32 measurements",
    "Sample: Median normalization of 32 measurements",
    "Sample-Level Median normalization of 32 measurements",
    "Sample-Wise PCA v2.1 with 10 features",
    "Sample: MEAN normalization of 32 measurements",
    "Sample: Quantile normalization of 32 measurements"
  ))
  identifiers <- c(
    "Patient: 13 clinical score",
    "Subject: 7 features selected",
    "Patient ID A13 clinical score",
    "Patient number 13 clinical score",
    "MRN 123456 clinical score",
    "participant number A13 used for selection",
    "patient id: A13 used for selection",
    "Subject - A13 used for selection",
    "Subject identifier A13 used for selection",
    "MRN: A13 used for selection",
    "Sample ID A13 used in construction",
    "Patient ID (A13) clinical score",
    "Subject 'S7' used in selection",
    "MRN (AB-123) used in construction",
    "participant No.: A13 used in selection",
    "pAtIeNt ID: A13 clinical score",
    "SUBJECT_IDENTIFIER = A-13 used for selection",
    "Participant No. S-7 used for selection",
    "SampleID:A13 used in construction",
    "medical-record-number 123456 clinical score",
    "MRN A13 used for selection",
    "Medical record No. AB-123 used in selection",
    "PatientAlpha clinical score",
    "SubjectAlpha used in selection",
    "ParticipantAlpha used in construction",
    "cohortPatientAlpha clinical score",
    "ClinicalPatient_ID_A13",
    "Patient, A13 clinical score",
    "MRN; 123456 used in construction",
    "cohortSubjectBeta used in selection",
    "Sample-A13 used in construction",
    "Sample: A13 used in construction",
    "Sample: ALPHA used in construction",
    "Sample-Alpha used in selection",
    "Sample_ID_a13 used in construction",
    "Sample-Level score constructed from 3 features; Patient ID A13",
    "Sample-Wise normalization of 32 measurements; Sample ID A13",
    "Sample: Median normalization of 32 measurements; MRN 123456",
    "Patient ID A13; Sample-Level score constructed from 3 features",
    "Sample: Median normalization of 32 measurements; cohortPatientAlpha"
  )
  lineage_descriptions <- c(
    "Sample: Robust normalization of 32 measurements",
    "Sample: Log normalization of 32 measurements",
    "Sample: Total intensity normalization of 32 measurements",
    "Sample: Median scaling of 32 measurements",
    "Sample: Pareto scaling of 32 measurements",
    "Sample: Unit variance scaling of 32 measurements",
    "Sample: Batch correction of 32 measurements",
    "Sample: Probabilistic quotient normalization of 32 measurements",
    "Sample: Log transformation of 32 measurements",
    "Sample: Robust feature filtering of 32 measurements",
    "Sample: Pooled quality control based signal correction of 32 measurements",
    "Sample: Score constructed from 3 features",
    "Sample: Top 10 features selected in the same study",
    "sample: centering and scaling of 32 measurements",
    "Sample: Centering and scaling of 32 measurements",
    "SAMPLE: CENTERING AND SCALING OF 32 MEASUREMENTS",
    "sample: robust normalization using 32 measurements",
    "Sample: Robust normalization using 32 measurements",
    "SAMPLE: ROBUST NORMALIZATION USING 32 MEASUREMENTS",
    "subjects: 32; features selected",
    "Subjects: 32; features selected",
    "SUBJECTS: 32; FEATURES SELECTED",
    "samples: normalization of 32 measurements",
    "Samples: normalization of 32 measurements",
    "SAMPLES: NORMALIZATION OF 32 MEASUREMENTS",
    "Sample: Centering and Scaling of 32 Measurements",
    "SAMPLE: CENTERING AND SCALING USING 32 MEASUREMENTS",
    "Sample: Robust Normalization Using 32 Measurements",
    "Sample: centering and scaling of 16 features",
    "Samples: normalization using 32 measurements",
    "subjects: 32; 10 features selected",
    "Subjects: 32; 10 features selected",
    "SUBJECTS: 32; 10 FEATURES SELECTED",
    "subjects: 32; features retained",
    "Subjects: 32; features retained",
    "SUBJECTS: 32; FEATURES RETAINED"
  )
  title_case <- function(value) {
    tokens <- strsplit(value, " ", fixed = TRUE)[[1L]]
    paste(vapply(tokens, function(token) {
      if (!nzchar(token)) return(token)
      paste0(toupper(substr(token, 1L, 1L)), substring(token, 2L))
    }, character(1)), collapse = " ")
  }
  lineage_variants <- unique(unlist(lapply(lineage_descriptions, function(value) {
    c(value, tolower(value), toupper(value), title_case(value))
  }), use.names = FALSE))
  safe <- unique(c(
    safe, lineage_variants,
    "SUBJECTS count 32; features selected"
  ))
  safe <- unique(c(
    safe, tolower(safe), toupper(safe),
    vapply(safe, function(value) {
      if (!grepl(":", value, fixed = TRUE)) return(tolower(value))
      prefix <- sub("^([^:]+:[[:space:]]*).*$", "\\1", value, perl = TRUE)
      description <- sub("^[^:]+:[[:space:]]*", "", value, perl = TRUE)
      paste0(prefix, tolower(description))
    }, character(1))
  ))
  additional_count_lineage <- c(
    "subjects:32; features selected",
    "Subjects:32; features selected",
    "SUBJECTS:32; FEATURES SELECTED",
    "subjects: 32. Features selected",
    "Subjects: 32. Features selected",
    "SUBJECTS: 32. FEATURES SELECTED",
    "subjects : 32; 10 features selected",
    "Subjects : 32; 10 features selected",
    "SUBJECTS : 32; 10 FEATURES SELECTED",
    "subjects  :  32. Features retained",
    "Subjects  :  32. Features retained",
    "SUBJECTS  :  32. FEATURES RETAINED",
    "subjects:32, features selected",
    "Subjects:32, features selected",
    "SUBJECTS:32, FEATURES SELECTED",
    "subjects:32.",
    "Subjects:32.",
    "SUBJECTS:32.",
    "subjects : 32",
    "Subjects : 32",
    "SUBJECTS : 32",
    "Subjects:7. Features selected",
    "SUBJECTS : 128; features retained"
  )
  safe <- unique(c(safe, additional_count_lineage))
  attached_plural_identifiers <- c(
    "SubjectS-7 used in selection",
    "PatientS-A13 used in construction",
    "cohortPatientS-A13 clinical score",
    "cohortSubjectS-7 used in selection",
    "SampleS-7 used in construction",
    "ParticipantS-A13 used in selection",
    "SubjectS.7 used in selection",
    "PatientS:A13 used in construction",
    "SubjectS_7 used in selection",
    "PatientS.13 used in construction",
    "cohortSubjectS:A7 used in selection",
    "cohortPatientS_13 clinical score",
    "SampleS.7 used in construction",
    "SampleS:A13 used in construction",
    "ParticipantS.7 used in selection",
    "cohortParticipantS-13 used in selection",
    "cohortPatientS",
    "cohortPatientS count 32",
    "ClinicalSubjectS",
    "ClinicalSubjectS count 32"
  )
  mixed_lineage_descriptions <- c(
    "Sample: Centering and Scaling of 32 Measurements",
    "SAMPLE: CENTERING AND SCALING USING 32 MEASUREMENTS",
    "Sample: Robust Normalization Using 32 Measurements",
    "SUBJECTS: 32; FEATURES SELECTED",
    "SAMPLES: NORMALIZATION OF 32 MEASUREMENTS"
  )
  mixed_identifier_forms <- c(
    "SubjectS-7", "cohortPatientS-A13", "Patient ID 13 clinical score"
  )
  plural_value_identifier_forms <- c(
    "SubjectS: A13 used in selection", "SUBJECTS: A13",
    "Samples: A13 used in construction", "SAMPLES: A13 used in construction"
  )
  count_token_identifiers <- c(
    "SUBJECTS: 32:A13 used in selection",
    "SubjectS: 32:A13 used in selection"
  )
  count_token_mixed_identifiers <- unique(unlist(lapply(
    c(
      "Sample: Robust Normalization Using 32 Measurements",
      "SAMPLES: NORMALIZATION OF 32 MEASUREMENTS"
    ),
    function(description) c(
      paste0(description, "; ", count_token_identifiers),
      paste0(count_token_identifiers, "; ", description)
    )
  ), use.names = FALSE))
  count_spacing_identifier_forms <- c(
    "SubjectS-7", "Patient ID 13 clinical score"
  )
  count_spacing_mixed_lineage <- c(
    "subjects:32; features selected",
    "subjects: 32. Features selected",
    "subjects : 32; 10 features selected",
    "subjects  :  32. Features retained",
    "subjects:32, features selected",
    "subjects:32.",
    "subjects : 32"
  )
  count_spacing_mixed_identifiers <- unique(unlist(lapply(
    count_spacing_mixed_lineage,
    function(description) c(
      paste0(description, "; ", count_spacing_identifier_forms),
      paste0(count_spacing_identifier_forms, "; ", description)
    )
  ), use.names = FALSE))
  count_spacing_composite_identifiers <- c(
    "SubjectS: 32-A13 used in selection",
    "SUBJECTS:32:A13 used in selection",
    "SUBJECTS:32.A13 used in selection",
    "SUBJECTS:32_A13 used in selection",
    "SUBJECTS:32-A13 used in selection",
    "SUBJECTS:32A13 used in selection",
    "SUBJECTS:32.7 used in selection",
    "SUBJECTS: 32.A13 used in selection",
    "SUBJECTS: 32_A13 used in selection",
    "SUBJECTS: 32-A13 used in selection",
    "SUBJECTS: 32A13 used in selection",
    "SUBJECTS: 32.7 used in selection"
  )
  mixed_lineage_identifiers <- unique(c(
    unlist(lapply(mixed_lineage_descriptions, function(description) {
      paste0(description, "; ", mixed_identifier_forms)
    }), use.names = FALSE),
    unlist(lapply(mixed_lineage_descriptions, function(description) {
      paste0(mixed_identifier_forms, "; ", description)
    }), use.names = FALSE),
    unlist(lapply(
      c("SUBJECTS: 32; FEATURES SELECTED", "SAMPLES: NORMALIZATION OF 32 MEASUREMENTS"),
      function(description) paste0(description, "; ", plural_value_identifier_forms)
    ), use.names = FALSE)
  ))
  subject_count_mixed_identifiers <- unique(c(
    unlist(lapply(
      c("SUBJECTS: 32; 10 FEATURES SELECTED", "SUBJECTS: 32; FEATURES RETAINED"),
      function(description) paste0(description, "; ", mixed_identifier_forms)
    ), use.names = FALSE),
    unlist(lapply(
      c("SUBJECTS: 32; 10 FEATURES SELECTED", "SUBJECTS: 32; FEATURES RETAINED"),
      function(description) paste0(mixed_identifier_forms, "; ", description)
    ), use.names = FALSE)
  ))
  identifiers <- c(
    identifiers, attached_plural_identifiers,
    "ClinicalSubjectS used in selection",
    "ParticipantAlpha", "PatientAlpha", "SubjectAlpha",
    "cohortPatientS clinical score", "cohortParticipantS used in construction",
    "Patient ID 13 clinical score",
    "SubjectS-7", "cohortPatientS-A13", "SubjectS.A13", "SubjectS:A13",
    "SubjectS/A13", "cohortParticipantS7", "SampleS_A13 used in construction",
    plural_value_identifier_forms, count_token_identifiers, count_token_mixed_identifiers,
    "Sample: Robust normalization of 32 measurements; Sample ID A13",
    mixed_lineage_identifiers, subject_count_mixed_identifiers,
    count_spacing_composite_identifiers, count_spacing_mixed_identifiers,
    "Sample: ALPHA used in construction of 32 measurements",
    paste0("SubjectS-7; ", lineage_descriptions),
    paste0(lineage_descriptions, "; cohortPatientS-A13")
  )
  safe <- unique(safe)
  identifiers <- unique(identifiers)
  testthat::expect_length(intersect(safe, identifiers), 0L)
  cases <- unique(c(safe, identifiers))

  for (value in cases) {
    is_safe_description <- value %in% safe
    expected <- if (is_safe_description) value else ""
    case_group <- if (is_safe_description) "safe scientific lineage" else "identifier-bearing lineage"
    case <- association
    case$construction_lineage <- value
    case$source_selection_history <- value
    profile <- triomics_target_profile(
      request, list(), list(),
      external_evidence = triomics_target_external_evidence(request, list(case))
    )
    testthat::expect_identical(
      profile$external_associations[[1L]]$construction_lineage, value,
      info = paste(case_group, "producer construction lineage:", value)
    )
    testthat::expect_identical(
      profile$external_associations[[1L]]$source_selection_history, value,
      info = paste(case_group, "producer selection history:", value)
    )

    section <- triomics_report_optional_sections(
      list(d = list(target_investigation = d13_recorded_target(list(profile)))),
      list(target_enabled = TRUE, clinical_enabled = FALSE)
    )$targets
    rows <- section$tables$external_evidence
    testthat::expect_identical(
      rows$construction_lineage, expected,
      info = paste(case_group, "report construction lineage:", value)
    )
    testthat::expect_identical(
      rows$source_selection_history, expected,
      info = paste(case_group, "report selection history:", value)
    )
    testthat::expect_identical(rows$subject_overlap_status, "shared_subjects")
    testthat::expect_identical(rows$independent_replication, FALSE)

    html <- triomics_report_optional_section_html(section)
    visible <- grepl(value, html, fixed = TRUE)
    testthat::expect_identical(
      visible, value %in% safe,
      info = paste(case_group, "optional section HTML visibility:", value)
    )
  }
})

testthat::test_that("mismatched null assays retain source applicability and safe serialized evidence", {
  profile <- d13_target_profile()
  assay <- profile$external_evidence[[1L]]
  assay$target_id <- "OTHER_TARGET"
  assay$attachment <- "target_mismatch"
  assay$target_identity_status <- "mismatched"
  assay$assay_limitations <- paste(
    "<script>alert('x')</script>", "/private/study/assays.tsv", "password=PRIVATE_SECRET_77"
  )
  profile$external_evidence <- list(assay)
  profile$evidence_axes$direct_binding$evidence <- list(assay)
  profile$input_provenance$external_evidence_file$reference <- "/private/study/assays.tsv"
  profile$observations[[1L]]$source_selection_evidence <- "Patient note: CLINICAL_SENTINEL_79"
  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  assay_row <- section$tables$external_evidence[1L, , drop = FALSE]
  html <- triomics_report_optional_section_html(section)
  json <- as.character(jsonlite::toJSON(
    triomics_report_sanitize_value(list(tables = section$tables)),
    auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA
  ))

  testthat::expect_identical(assay_row$target_identity, "OTHER_TARGET")
  testthat::expect_identical(assay_row$attachment, "target_mismatch")
  testthat::expect_identical(assay_row$target_identity_status, "mismatched")
  testthat::expect_true(is.na(assay_row$value))
  testthat::expect_identical(assay_row$value_status, "unavailable")
  testthat::expect_true(grepl("Not available (source value is null)", html, fixed = TRUE))
  testthat::expect_false(grepl("<script>|PRIVATE_SECRET_77|/private/study|CLINICAL_SENTINEL_79", html))
  testthat::expect_true(grepl("&lt;script&gt;", html, fixed = TRUE))
  testthat::expect_false(grepl("PRIVATE_SECRET_77|/private/study|CLINICAL_SENTINEL_79", json))
  testthat::expect_true(grepl("source_value_null|unavailable|public_assay_source", json))
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  json_assay <- parsed$tables$external_evidence[[1L]]
  testthat::expect_false("value" %in% names(json_assay))
  testthat::expect_identical(json_assay$value_status, "unavailable")
  testthat::expect_identical(json_assay$value_reason, "source_value_null")
})

testthat::test_that("disabled and missing target routes keep distinct report statuses", {
  disabled <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target())),
    list(target_enabled = FALSE, clinical_enabled = FALSE)
  )$targets
  missing <- triomics_report_optional_sections(
    list(d = list()),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  partial_target <- d13_recorded_target()
  partial_target$status <- "partial"
  partial_target$reason_code <- "D_TARGET_PROFILE_PARTIAL"
  partial <- triomics_report_optional_sections(
    list(d = list(target_investigation = partial_target)),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets

  testthat::expect_identical(disabled$status, "not_requested")
  testthat::expect_identical(missing$status, "unavailable")
  testthat::expect_identical(missing$reason_code, "D_REPORT_TARGET_DOSSIER_MISSING")
  testthat::expect_identical(partial$status, "partial")
  testthat::expect_identical(partial$reason_code, "D_TARGET_PROFILE_PARTIAL")
})

testthat::test_that("an absent evidence axis stays unavailable with unknown coverage", {
  profile <- d13_target_profile()
  profile$evidence_axes$cellular_tissue_engagement <- NULL
  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  row <- section$tables$evidence_axes[
    section$tables$evidence_axes$axis == "cellular_tissue_engagement", , drop = FALSE
  ]

  testthat::expect_identical(row$status, "unavailable")
  testthat::expect_identical(row$reason_code, "D_REPORT_TARGET_AXIS_MISSING")
  testthat::expect_true(is.na(row$expected_records))
  testthat::expect_true(grepl("not evidence", row$assessment_note, fixed = TRUE))
})

testthat::test_that("assessment report keeps declared drug, constructs and request selection history", {
  profile <- d13_target_profile()
  profile$request$drug <- list(
    namespace = "synthetic_compound", drug_id = "compound-1", label = "Synthetic compound"
  )
  profile$request$constructs <- list("construct-1", "construct-2")
  profile$request$context <- list(context_id = "synthetic_context", confirmation_protocol = NULL)
  profile$request$selection <- list(list(
    selection_id = "request-selection-1", hypothesis_status = "exploratory",
    selected_with_observations = TRUE, selection_source = "declared_selection",
    selection_evidence_ref = "/private/selection/record.json"
  ))
  section <- triomics_report_optional_sections(
    list(d = list(target_investigation = d13_recorded_target(list(profile)))),
    list(target_enabled = TRUE, clinical_enabled = FALSE)
  )$targets
  target_row <- section$tables$profiles[1L, , drop = FALSE]
  selection_row <- section$tables$selection_history[1L, , drop = FALSE]

  testthat::expect_identical(target_row$drug_namespace, "synthetic_compound")
  testthat::expect_identical(target_row$drug_id, "compound-1")
  testthat::expect_identical(target_row$drug_label, "Synthetic compound")
  testthat::expect_identical(target_row$declared_constructs, "construct-1; construct-2")
  testthat::expect_identical(target_row$request_context_id, "synthetic_context")
  testthat::expect_identical(selection_row$selection_id, "request-selection-1")
  testthat::expect_identical(selection_row$hypothesis_status, "exploratory")
  testthat::expect_identical(selection_row$selected_with_observations, TRUE)
  testthat::expect_false(grepl("/private/selection", selection_row$selection_evidence_ref))
})

testthat::test_that("a required target section keeps the shared renderer's recorded status semantics", {
  target <- d13_recorded_target()
  loaded <- list(
    d = list(
      status = "complete", route_status = list(run_completeness = "complete"),
      target_investigation = target,
      inherited_evidence = list(
        evaluation_evidence = list(controls = lapply(seq_len(4L), function(index) {
          list(status = "not_run")
        })),
        stage_status = list(clinical_prediction = list(
          status = "not_started", completion_policy = "conditional",
          status_scope = "synthetic_fixture_only", qualification_status = "unqualified",
          model_execution = "not_started", benefit_guaranteed = FALSE
        ))
      )
    ),
    envelopes = list(a = list(status = "complete")),
    payloads = list(b = list(status = "complete"), c = list(status = "complete"), clinical = NULL)
  )
  config <- list(
    target_enabled = TRUE, target_required = TRUE,
    clinical_enabled = FALSE, clinical_required = FALSE,
    blocks = list(list(block_id = "block_1")),
    analysis = list(contrasts = list("score_slope"))
  )
  results <- data.frame(block_id = "block_1", feature_key = "feature-1", n_samples = 4L)
  clinical <- data.frame()
  integrations <- data.frame(integration_id = character(), status = character(), required = logical())
  routes <- data.frame(route = character(), status = character(), required = logical())
  flow <- data.frame(source_sample_count = 4L, source_feature_count = 1L)
  extended <- triomics_report_optional_sections(
    loaded, list(target_enabled = TRUE, clinical_enabled = FALSE)
  )
  projection <- triomics_report_sections(config, loaded, results, clinical, integrations,
                                         routes, flow, extended)
  statuses <- triomics_report_section_frame(projection$sections)
  target_status <- statuses[statuses$section_id == "target_evidence_alternatives", , drop = FALSE]
  dossier_status <- statuses[statuses$section_id == "target_dossier_details", , drop = FALSE]

  testthat::expect_identical(target_status$status, "recorded")
  testthat::expect_true(target_status$required)
  testthat::expect_identical(dossier_status$status, "complete")
  testthat::expect_true(dossier_status$required)
})
