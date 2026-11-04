d10_code <- function(expr) tryCatch({ force(expr); NULL }, error = function(e) e$code)
d10_identity <- function() list(namespace='synthetic', id='SYNTH_FCN3_LIKE', taxon_id=9606L, entity_type='protein', constructs=list('a'))
d10_selection <- function() list(list(selection_id='s1', hypothesis_status='exploratory', selected_with_observations=TRUE), list(selection_id='s2', hypothesis_status='confirmed', selected_with_observations=TRUE))
d10_request <- function(identity=d10_identity()) triomics_target_request(identity, 'assess', d10_selection(), context=list(context_id='ctx', confirmation_protocol='protocol'))
d10_row <- function(id, axis, endpoint, units, value=1) list(evidence_id=id, axis=axis, endpoint=endpoint, units=units, relation='=', source_id=paste0('source-',id), publication_group='paper', measured_by_pipeline=FALSE, value=value, target_namespace='synthetic', target_id='SYNTH_FCN3_LIKE', target_entity_type='protein', assay='synthetic assay fixture', assay_limitations='Synthetic software fixture only; no biological claim.')

testthat::test_that('AC1 records target context and qualified selection history', {
  x <- d10_request()
  testthat::expect_identical(x$request_id, 'target:synthetic:SYNTH_FCN3_LIKE:taxon:9606')
  testthat::expect_identical(x$namespace, 'synthetic'); testthat::expect_identical(x$taxon_id, 9606L)
  testthat::expect_identical(x$entity_type, 'protein'); testthat::expect_identical(x$constructs, list('a'))
  testthat::expect_identical(x$context$context_id, 'ctx')
  testthat::expect_equal(vapply(x$selection, `[[`, '', 'hypothesis_status'), c('exploratory','exploratory'))
  s <- list(list(selection_id='s', hypothesis_status='confirmed', selected_with_observations=FALSE))
  testthat::expect_identical(triomics_target_request(d10_identity(), 'assess', s, list(confirmation_protocol='p'))$selection[[1]]$hypothesis_status, 'confirmed')
  testthat::expect_identical(triomics_target_request(d10_identity(), 'assess', s)$selection[[1]]$hypothesis_status, 'exploratory')
  testthat::expect_identical(d10_code(triomics_target_request(d10_identity(), 'rank', s)), 'D_TARGET_INPUT_INVALID')
  testthat::expect_identical(d10_code(triomics_target_request(list(), 'assess', s)), 'D_TARGET_IDENTITY_REQUIRED')
  testthat::expect_identical(d10_code(triomics_target_request(d10_identity(), 'discover', s)), 'D_TARGET_UNIVERSE_REQUIRED')
  testthat::expect_identical(d10_code(triomics_target_request(d10_identity(), 'assess', c(s,s))), 'D_TARGET_INPUT_INVALID')
  u <- list(universe_id='u', version='1', enumeration='unranked')
  testthat::expect_identical(triomics_target_request(d10_identity(), 'discover', s, universe=u)$universe, u)
})

testthat::test_that('AC2 distinct assays retain their endpoints and units without collapse', {
  rows <- list(d10_row('b','binding','IC50','nM'), d10_row('f','function','activity','percent'), d10_row('e','engagement','occupancy','fraction'), d10_row('a','association','beta','mg/L'))
  x <- triomics_target_external_evidence(d10_request(), rows)
  testthat::expect_equal(vapply(x$records, `[[`, '', 'endpoint'), c('IC50','activity','occupancy','beta'))
  testthat::expect_equal(vapply(x$records, `[[`, '', 'units'), c('nM','percent','fraction','mg/L'))
  testthat::expect_equal(vapply(x$records, `[[`, '', 'axis'), c('binding','function','engagement','association'))
  testthat::expect_false(any(c('kd','potency_score','binding_probability') %in% c(names(x), unlist(lapply(x$records,names)))))
  rows[[1]]$potency_score <- 1
  testthat::expect_identical(d10_code(triomics_target_external_evidence(d10_request(), rows)), 'D_TARGET_AXIS_COLLAPSED')
  pathway <- d10_identity(); pathway$entity_type <- 'pathway'
  testthat::expect_false(d10_request(pathway)$binding_target)
  testthat::expect_false(identical(triomics_target_external_evidence(d10_request(pathway), list(d10_row('b','binding','IC50','nM')))$records[[1]]$attachment, 'request'))
})

testthat::test_that('AC3 external sources and unavailable affinity remain explicit', {
  row <- d10_row('b','binding','affinity','nM',NULL)
  x <- triomics_target_external_evidence(d10_request(), list(row))$records[[1]]
  testthat::expect_null(x$value); testthat::expect_identical(x$availability, 'unavailable')
  testthat::expect_identical(x$source_id, 'source-b'); testthat::expect_false(x$measured_by_pipeline)
  row$measured_by_pipeline <- TRUE
  testthat::expect_identical(d10_code(triomics_target_external_evidence(d10_request(), list(row))), 'D_TARGET_EXTERNAL_ONLY')
})

testthat::test_that('AC4 ambiguity, context and publication dependency survive', {
  identity <- d10_identity(); identity$constructs <- list('a','b')
  x <- d10_request(identity); testthat::expect_identical(x$identity_status, 'ambiguous')
  a <- d10_row('a','binding','IC50','nM'); b <- d10_row('b','function','activity','percent')
  y <- triomics_target_external_evidence(x, list(a,b))
  testthat::expect_equal(vapply(y$records, `[[`, '', 'attachment'), c('unresolved','unresolved'))
  testthat::expect_equal(vapply(y$records, `[[`, '', 'dependency_group'), c('publication:paper','publication:paper'))
  testthat::expect_true(all(!vapply(y$records, `[[`, logical(1), 'independent_replication')))
  testthat::expect_identical(y$independent_group_count, 1L)
  a$taxon_id <- 10090L; b$context_id <- 'other'
  y <- triomics_target_external_evidence(d10_request(), list(a,b))
  testthat::expect_equal(vapply(y$records, `[[`, '', 'attachment'), c('taxon_mismatch','context_mismatch'))
  b$evidence_id <- 'a'
  testthat::expect_identical(d10_code(triomics_target_external_evidence(d10_request(), list(a,b))), 'D_TARGET_INPUT_INVALID')
})

testthat::test_that('V19 synthetic non-enzymatic target gets no invented reaction or Kd', {
  x <- triomics_target_external_evidence(d10_request(), list(d10_row('b','binding','IC50','nM',NULL)))
  testthat::expect_null(x$records[[1]]$value)
  testthat::expect_false(any(c('kd','reaction','reaction_edges') %in% c(names(x), unlist(lapply(x$records,names)))))
})

testthat::test_that('repair 1 rejects collapsed fields without rejecting an IC50 endpoint', {
  for (field in c('composite_score', 'support_score', 'Kd', 'KD', 'kd_nM', 'binding_probability', 'potency_score')) {
    row <- d10_row('b','binding','IC50','nM'); row[[field]] <- 1
    testthat::expect_identical(d10_code(triomics_target_external_evidence(d10_request(), list(row))), 'D_TARGET_AXIS_COLLAPSED')
  }
  testthat::expect_identical(triomics_target_external_evidence(d10_request(), list(d10_row('b','binding','IC50','nM')))$records[[1]]$endpoint, 'IC50')
})

testthat::test_that('repair 1 preserves a namespaced drug and requires its namespace', {
  drug <- list(namespace='synthetic', drug_id='drug1', label='Drug one')
  x <- triomics_target_request(d10_identity(), 'assess', d10_selection(), drug=drug)
  testthat::expect_identical(x$drug, drug)
  testthat::expect_identical(d10_code(triomics_target_request(d10_identity(), 'assess', d10_selection(), drug=drug[c('drug_id','label')])), 'D_TARGET_INPUT_INVALID')
  drug$kd <- 1
  testthat::expect_identical(d10_code(triomics_target_request(d10_identity(), 'assess', d10_selection(), drug=drug)), 'D_TARGET_INPUT_INVALID')
})

testthat::test_that('repair 1 attaches only completely matching assay identity and context', {
  drug <- list(namespace='synthetic', drug_id='drug1', label='Drug one')
  request <- triomics_target_request(d10_identity(), 'assess', d10_selection(), list(context_id='ctx'), drug)
  row <- d10_row('b','binding','IC50','nM')
  row$taxon_id <- 9606L; row$context_id <- 'ctx'; row$construct <- 'a'; row$compound_id <- 'drug1'; row$compound_namespace <- 'synthetic'
  attach <- function(r, req=request) triomics_target_external_evidence(req, list(r))$records[[1]]$attachment
  testthat::expect_identical(attach(row), 'request')
  missing_namespace <- row; missing_namespace$compound_namespace <- NULL
  testthat::expect_identical(attach(missing_namespace), 'unresolved')
  other_namespace <- row; other_namespace$compound_namespace <- 'other'
  testthat::expect_identical(attach(other_namespace), 'compound_mismatch')
  for (bad in list('', NA_character_, c('a', 'b'), 1)) {
    invalid <- row; invalid$compound_namespace <- bad
    testthat::expect_identical(d10_code(triomics_target_external_evidence(request, list(invalid))), 'D_TARGET_INPUT_INVALID')
  }
  missing <- row; missing$taxon_id <- NULL
  testthat::expect_identical(attach(missing), 'unresolved')
  other <- row; other$compound_id <- 'other'
  testthat::expect_identical(attach(other), 'compound_mismatch')
  other <- row; other$construct <- 'other'
  testthat::expect_identical(attach(other), 'construct_mismatch')
  mismatch <- row; mismatch$taxon_id <- 10090L; mismatch$compound_id <- NULL
  testthat::expect_identical(attach(mismatch), 'taxon_mismatch')
  mismatch <- row; mismatch$context_id <- 'other'; mismatch$construct <- NULL
  testthat::expect_identical(attach(mismatch), 'context_mismatch')
  pathway <- d10_identity(); pathway$entity_type <- 'pathway'
  pathway_request <- triomics_target_request(pathway, 'assess', d10_selection(), list(context_id='ctx'), drug)
  testthat::expect_identical(attach(row, pathway_request), 'not_binding_target')
})

testthat::test_that('assay attachment requires the exact declared target identity', {
  drug <- list(namespace='synthetic_drug', drug_id='drug1', label='Drug one')
  request <- triomics_target_request(d10_identity(), 'assess', d10_selection(),
                                    list(context_id='ctx'), drug)
  row <- d10_row('b','binding','Kd','nM')
  row$taxon_id <- 9606L; row$context_id <- 'ctx'; row$construct <- 'a'
  row$compound_id <- 'drug1'; row$compound_namespace <- 'synthetic_drug'
  attach <- function(value) triomics_target_external_evidence(request, list(value))$records[[1]]$attachment

  testthat::expect_identical(attach(row), 'request')
  wrong_target <- row; wrong_target$target_id <- 'DIFFERENT_TARGET_B'
  testthat::expect_identical(attach(wrong_target), 'target_mismatch')
  wrong_namespace <- row; wrong_namespace$target_namespace <- 'other_namespace'
  testthat::expect_identical(attach(wrong_namespace), 'target_mismatch')
  wrong_entity <- row; wrong_entity$target_entity_type <- 'complex'
  testthat::expect_identical(attach(wrong_entity), 'target_mismatch')
  missing_identity <- row; missing_identity$target_id <- NULL
  testthat::expect_identical(attach(missing_identity), 'unresolved')
})

testthat::test_that('external binding_probability labels cannot masquerade as measured endpoints', {
  drug <- list(namespace='synthetic_drug', drug_id='drug1', label='Drug one')
  request <- triomics_target_request(d10_identity(), 'assess', d10_selection(),
                                    list(context_id='ctx'), drug)
  row <- d10_row('b','binding','binding_probability','fraction')
  row$taxon_id <- 9606L; row$context_id <- 'ctx'
  row$construct <- 'a'; row$compound_id <- 'drug1'; row$compound_namespace <- 'synthetic_drug'
  testthat::expect_identical(d10_code(triomics_target_external_evidence(request, list(row))),
                             'D_TARGET_AXIS_COLLAPSED')
})

testthat::test_that('assay identity remains matched while missing assay metadata is unresolved', {
  drug <- list(namespace='synthetic_drug', drug_id='drug1', label='Synthetic drug')
  request <- triomics_target_request(d10_identity(), 'assess', d10_selection(),
                                     list(context_id='ctx'), drug)
  base <- d10_row('qualified-kd', 'binding', 'Kd', 'nM', 4.2)
  base$taxon_id <- 9606L; base$context_id <- 'ctx'; base$construct <- 'a'
  base$compound_namespace <- 'synthetic_drug'; base$compound_id <- 'drug1'

  for (missing_field in c('assay', 'assay_limitations')) {
    incomplete <- base
    incomplete[[missing_field]] <- NULL
    ledger <- triomics_target_external_evidence(request, list(incomplete))
    record <- ledger$records[[1L]]
    profile <- triomics_target_profile(request, list(), list(), external_evidence=ledger)

    testthat::expect_identical(record$attachment, 'request')
    testthat::expect_identical(record$assay_qualification, 'unresolved')
    testthat::expect_identical(record$assay_qualification_reason,
      if (identical(missing_field, 'assay')) 'assay_description_missing' else 'assay_limitations_missing')
    testthat::expect_identical(record$endpoint, 'Kd')
    testthat::expect_identical(record$value, 4.2)
    testthat::expect_identical(record$source_id, 'source-qualified-kd')
    testthat::expect_identical(profile$evidence_axes$direct_binding$status, 'unresolved')
    testthat::expect_identical(profile$evidence_axes$direct_binding$coverage$measured_records, 0L)
    testthat::expect_identical(profile$evidence_axes$direct_binding$coverage$unresolved_records, 1L)
  }
})
