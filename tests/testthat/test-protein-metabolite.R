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

test_that('the synthetic target keeps its profile and other targets are skipped', {
  synthetic <- triomics_d_attach_target_profiles(
    list(), pm_investigation(targets = list(pm_target('SYNTH_FCN3_LIKE')))
  )
  expect_length(synthetic$profiles, 1L)
  expect_identical(synthetic$profiles[[1]]$request$mechanism_class, 'non_enzymatic')

  other <- triomics_d_attach_target_profiles(list(), pm_investigation())
  expect_length(other$profiles, 0L)
  expect_identical(other$skipped[[1]]$id, 'OTHER_TARGET')
  expect_identical(other$skipped[[1]]$reason, 'mechanism_class_not_declared')
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
