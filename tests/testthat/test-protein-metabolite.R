pm_code <- function(expr) tryCatch({ force(expr); NA_character_ }, triomics_error = function(e) e$code)

pm_target <- function(id = 'OTHER_TARGET', entity_type = 'protein') {
  list(namespace = 'synthetic', id = id, taxon_id = 9606, entity_type = entity_type)
}

pm_investigation <- function(targets = list(pm_target()), enabled = TRUE) {
  list(enabled = enabled, mode = 'assess', targets = targets, drug_ref = NULL,
       context_ref = 'synthetic_context', external_evidence_file = NULL,
       mechanism_file = NULL, protocol_ref = 'synthetic_target_protocol', required = FALSE)
}

pm_link <- function() {
  list(link_id = 'link-1', protein_namespace = 'synthetic', protein_id = 'OTHER_TARGET',
       metabolite_namespace = 'synthetic', metabolite_id = 'METABOLITE_1',
       relation = 'documented_association', evidence_ref = 'evidence-1', sign = 'unknown')
}

pm_change <- function(link_id = 'link-1', endpoint = 'metabolite_concentration') {
  list(change_id = 'change-1', metabolite_namespace = 'synthetic',
       metabolite_id = 'METABOLITE_1', link_id = link_id, endpoint = endpoint,
       value = 1.5, status = 'support')
}

pm_synthetic_prepared_target_slot <- function(id, mechanism_class = NULL) {
  identity <- list(namespace = 'synthetic', id = id, taxon_id = 9606L,
                   entity_type = 'protein', constructs = list(paste0('fixture_construct_', id)))
  request <- triomics_target_request(
    identity, 'assess', list(),
    context = list(context_id = 'protein_metabolite_fixture_context',
                    confirmation_protocol = 'protein_metabolite_fixture_protocol')
  )
  if (!is.null(mechanism_class)) request$mechanism_class <- mechanism_class

  declaration <- if (is.null(mechanism_class)) list() else list(list(
    identity = identity, mechanism_class = mechanism_class, links = list(), selection_history = list()
  ))
  mechanism_status <- if (is.null(mechanism_class)) 'not_declared' else if (
    mechanism_class %in% c('enzymatic', 'non_enzymatic')
  ) 'assessable' else 'unassessable'
  evidence <- triomics_target_external_evidence(request, list())

  list(status = 'prepared_synthetic_fixture', reason_code = NULL, results = list(list(
    request = request, mechanism_status = mechanism_status, mechanism_declarations = declaration,
    links = list(), observations = list(), external_evidence = evidence, input_provenance = list()
  )))
}

pm_expect_no_inferred_target_claims <- function(attached, profile) {
  expect_null(attached$selected_target)
  expect_null(profile$selected_target)
  expect_null(attached$alternatives)
  expect_identical(profile$links, list())
  expect_identical(profile$observations, list())
  expect_identical(profile$external_evidence, list())
  expect_identical(profile$evidence_axes$direct_binding$evidence, list())
  expect_false(any(c('binding', 'binding_probability', 'flux', 'causal_mediation', 'kd') %in%
                     names(profile)))
}

test_that('empty and disabled declared-protein investigations return no proteins', {
  empty <- triomics_protein_metabolite_report(pm_investigation(targets = list()))
  expect_length(empty$proteins, 0L)
  expect_null(empty$selected_target)

  disabled <- triomics_protein_metabolite_report(pm_investigation(enabled = FALSE))
  expect_length(disabled$proteins, 0L)
})

test_that('a declared protein without evidence remains unattributed and unavailable', {
  report <- triomics_protein_metabolite_report(pm_investigation())
  expect_length(report$proteins, 1L)
  expect_identical(report$proteins[[1]]$links, list())
  expect_identical(report$proteins[[1]]$changes, list())
  expect_identical(report$proteins[[1]]$link_status, 'not_documented')
  expect_identical(report$proteins[[1]]$change_status, 'not_attributed')
  expect_identical(report$proteins[[1]]$coverage, 'unmeasured')
  expect_identical(report$proteins[[1]]$binding_status, 'unavailable')
  expect_identical(report$proteins[[1]]$binding_reason, 'D_TARGET_EVIDENCE_ABSENT')
  expect_null(report$selected_target)
})

test_that('a documented link retains its cited measured metabolite change', {
  link <- pm_link()
  change <- pm_change()
  report <- triomics_protein_metabolite_report(
    pm_investigation(), documented_links = list(link), measured_changes = list(change)
  )
  protein <- report$proteins[[1]]
  expect_identical(protein$links, list(link))
  expect_identical(protein$changes[[1]]$endpoint, 'metabolite_concentration')
  expect_identical(protein$changes[[1]]$value, 1.5)
  expect_identical(protein$link_status, 'documented')
  expect_identical(protein$change_status, 'retained')
  expect_identical(protein$coverage, 'measured')
})

test_that('a change without a matching link is omitted while its documented link remains', {
  link <- pm_link()
  change <- pm_change(link_id = 'missing-link')
  report <- triomics_protein_metabolite_report(
    pm_investigation(), documented_links = list(link), measured_changes = list(change)
  )
  protein <- report$proteins[[1]]
  expect_identical(protein$links, list(link))
  expect_length(protein$changes, 0L)
  expect_identical(protein$link_status, 'documented')
})

test_that('a link without an evidence reference is omitted', {
  link <- pm_link()
  link$evidence_ref <- ''
  report <- triomics_protein_metabolite_report(pm_investigation(), documented_links = list(link))
  expect_length(report$proteins[[1]]$links, 0L)
  expect_identical(report$proteins[[1]]$link_status, 'not_documented')
})

test_that('flux changes abort instead of collapsing into metabolite evidence', {
  expect_identical(
    pm_code(triomics_protein_metabolite_report(
      pm_investigation(), documented_links = list(pm_link()),
      measured_changes = list(pm_change(endpoint = 'flux'))
    )),
    'D_TARGET_CLAIM_COLLAPSED'
  )
})

test_that('multiple declared proteins do not select a target', {
  report <- triomics_protein_metabolite_report(pm_investigation(
    targets = list(pm_target('OTHER_TARGET'), pm_target('ANOTHER_TARGET'))
  ))
  expect_length(report$proteins, 2L)
  expect_null(report$selected_target)
})

test_that('non-protein entities have no metabolite links', {
  report <- triomics_protein_metabolite_report(pm_investigation(
    targets = list(pm_target('PATHWAY_1', entity_type = 'pathway'))
  ))
  expect_identical(report$proteins[[1]]$link_status, 'not_applicable')
  expect_length(report$proteins[[1]]$links, 0L)
})

test_that('only a declared ordinary target receives its typed nonenzymatic profile', {
  ordinary <- triomics_d_attach_target_profiles(
    pm_synthetic_prepared_target_slot('ORDINARY_TARGET', mechanism_class = 'non_enzymatic'),
    pm_investigation(targets = list(pm_target('ORDINARY_TARGET')))
  )
  expect_length(ordinary$profiles, 1L)
  profile <- ordinary$profiles[[1L]]
  expect_identical(profile$request$namespace, 'synthetic')
  expect_identical(profile$request$id, 'ORDINARY_TARGET')
  expect_identical(profile$request$taxon_id, 9606L)
  expect_identical(profile$request$entity_type, 'protein')
  expect_identical(profile$request$constructs, list('fixture_construct_ORDINARY_TARGET'))
  expect_identical(profile$mechanism_class, 'non_enzymatic')
  expect_identical(profile$mechanism_status, 'assessable')
  expect_identical(profile$mechanism_declaration_status, 'assessable')
  expect_identical(profile$mechanism_declarations[[1L]]$identity, list(
    namespace = 'synthetic', id = 'ORDINARY_TARGET', taxon_id = 9606L,
    entity_type = 'protein', constructs = list('fixture_construct_ORDINARY_TARGET')
  ))
  expect_identical(profile$mechanism_declarations[[1L]]$mechanism_class, 'non_enzymatic')
  expect_identical(profile$coverage, 'unmeasured')
  expect_identical(profile$independent_count, 0L)
  pm_expect_no_inferred_target_claims(ordinary, profile)
})

test_that('a special target name alone cannot create a mechanism profile', {
  special <- triomics_d_attach_target_profiles(
    list(), pm_investigation(targets = list(pm_target('SYNTH_FCN3_LIKE')))
  )
  expect_length(special$profiles, 0L)
  expect_null(special$selected_target)
  expect_null(special$alternatives)
})

test_that('missing and unknown mechanism declarations remain unassessable without evidence', {
  missing <- triomics_d_attach_target_profiles(
    pm_synthetic_prepared_target_slot('SYNTH_FCN3_LIKE'),
    pm_investigation(targets = list(pm_target('SYNTH_FCN3_LIKE')))
  )
  expect_length(missing$profiles, 1L)
  missing_profile <- missing$profiles[[1L]]
  expect_null(missing_profile$mechanism_class)
  expect_identical(missing_profile$mechanism_status, 'unassessable')
  expect_identical(missing_profile$mechanism_declaration_status, 'not_declared')
  expect_identical(missing_profile$mechanism_declarations, list())
  pm_expect_no_inferred_target_claims(missing, missing_profile)

  unknown <- triomics_d_attach_target_profiles(
    pm_synthetic_prepared_target_slot('ORDINARY_TARGET', mechanism_class = 'unknown'),
    pm_investigation(targets = list(pm_target('ORDINARY_TARGET')))
  )
  expect_length(unknown$profiles, 1L)
  unknown_profile <- unknown$profiles[[1L]]
  expect_null(unknown_profile$mechanism_class)
  expect_identical(unknown_profile$request$mechanism_class, 'unknown')
  expect_identical(unknown_profile$mechanism_status, 'unassessable')
  expect_identical(unknown_profile$mechanism_declaration_status, 'unassessable')
  expect_identical(unknown_profile$mechanism_declarations[[1L]]$mechanism_class, 'unknown')
  pm_expect_no_inferred_target_claims(unknown, unknown_profile)
})

test_that('measured changes cannot carry a Kd claim', {
  change <- pm_change()
  change$kd <- 5
  expect_identical(
    pm_code(triomics_protein_metabolite_report(
      pm_investigation(), documented_links = list(pm_link()), measured_changes = list(change)
    )),
    'D_TARGET_CLAIM_COLLAPSED'
  )
})

test_that('a whitespace-only evidence reference does not document a link', {
  link <- pm_link()
  link$evidence_ref <- ' '
  report <- triomics_protein_metabolite_report(pm_investigation(), documented_links = list(link))
  expect_length(report$proteins[[1]]$links, 0L)
  expect_identical(report$proteins[[1]]$link_status, 'not_documented')
})

test_that('an empty sibling evidence file leaves the target slot unavailable', {
  evidence_dir <- tempfile()
  dir.create(evidence_dir)
  on.exit(unlink(evidence_dir, recursive = TRUE), add = TRUE)
  evidence_path <- file.path(evidence_dir, 'evidence.json')
  config_path <- file.path(evidence_dir, 'config.yml')
  writeLines('[]', evidence_path)
  writeLines('{}', config_path)

  investigation <- pm_investigation(targets = list())
  investigation$external_evidence_file <- basename(evidence_path)
  slot <- triomics_d_target_slot(investigation, config_path)
  expect_identical(slot$status, 'unavailable')
  expect_identical(slot$reason_code, 'D_TARGET_EVIDENCE_ABSENT')
  expect_length(slot$results, 0L)
})
