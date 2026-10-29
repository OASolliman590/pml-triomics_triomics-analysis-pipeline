d_t02_fixture <- function() {
  root <- tempfile('d-t02-'); dir.create(root)
  path <- file.path(root, 'lipids.tsv')
  writeLines(c(
    'source_namespace\tsource_id\ttaxon_id\ttarget_namespace\ttarget_id\trelation\tresolution\tevidence_ref\tresource_id',
    'lipid\tPC(34:1)\t9606\tref\tPC34A\texact\tsum_composition\tsynthetic:1\tlipid-map',
    'lipid\tPC(34:1)\t9606\tref\tPC34B\texact\tsum_composition\tsynthetic:2\tlipid-map',
    'lipid\tPE(36:2)\t9606\tref\tPE36\texact\tsum_composition\tsynthetic:3\tlipid-map',
    'lipid\tPC alias\t9606\tref\tPC34A\texact\tsum_composition\tsynthetic:1\tlipid-map'
  ), path)
  hash <- triomics_sha256(path)
  resource <- list(resource_id='lipid-map', source_url='local://synthetic', version='1', sha256=hash,
                   local_file=path, terms_url='local://terms', rights_status='verified',
                   permitted_operation=list('lipid_ontology'), redistribution='allowed')
  rows <- list(
    list(feature_key='row1', source_id='PC(34:1)', source_namespace='lipid', taxon_id='9606', resolution='sum_composition', aliases=list(list(source_id='PC alias', source_namespace='lipid', provenance_ref='synthetic:alias'))),
    list(feature_key='row2', source_id='PE(36:2)', source_namespace='lipid', taxon_id='9606', resolution='sum_composition'),
    list(feature_key='row3', source_id='TG(50:1)', source_namespace='lipid', taxon_id='9606', resolution='sum_composition')
  )
  list(root=root, path=path, hash=hash, resource=resource, rows=rows)
}
d_t02_map <- function(f, resource=f$resource, version='1', hash=f$hash, cache=file.path(f$root, 'cache')) {
  triomics_map_lipids(f$rows, resource, 'lipid_ontology', version, hash, cache)
}
testthat::test_that('D-T02 AC1 AC2 AC4 parse local mappings without multiplying observations', {
  f <- d_t02_fixture(); x <- d_t02_map(f)
  testthat::expect_identical(x$status, 'available')
  testthat::expect_length(x$observations, 3L)
  testthat::expect_identical(x$observations[[1]]$class, 'PC')
  testthat::expect_identical(x$observations[[1]]$carbons, 34L)
  testthat::expect_identical(x$observations[[1]]$unsaturations, 1L)
  testthat::expect_identical(x$observations[[1]]$resolution, 'sum_composition')
  testthat::expect_null(x$observations[[1]]$chains)
  testthat::expect_null(x$observations[[1]]$sn_positions)
  testthat::expect_null(x$observations[[1]]$isomer)
  testthat::expect_length(x$observations[[1]]$aliases, 1L)
  testthat::expect_length(x$edges, 3L)
  testthat::expect_setequal(vapply(x$edges, `[[`, '', 'target_entity_id'), c('PC34A','PC34B','PE36'))
  testthat::expect_length(x$ambiguity_groups, 1L)
  testthat::expect_identical(x$ambiguity_groups[[1]]$feature_key, 'row1')
  testthat::expect_identical(x$observations[[3]]$mapping_status, 'unmapped')
  testthat::expect_false(any(vapply(x$edges, function(e) e$feature_key == 'row3', logical(1))))
})
testthat::test_that('D-T02 AC1 rejects position assertions for sum composition', {
  f <- d_t02_fixture()
  cat('lipid\tPC(34:1)\t9606\tref\tsn1=16:0\texact\tsum_composition\tsynthetic:bad\tlipid-map\n', file=f$path, append=TRUE)
  f$hash <- triomics_sha256(f$path); f$resource$sha256 <- f$hash
  testthat::expect_identical(tryCatch(d_t02_map(f), error=identity)$code, 'D_LIPID_RESOLUTION_CONFLICT')
})
testthat::test_that('D-T02 AC3 resource service failures remain fail closed', {
  f <- d_t02_fixture(); x <- d_t02_map(f)
  testthat::expect_identical(x$resource$network_policy, 'offline')
  testthat::expect_false(x$resource$network_attempted)
  testthat::expect_false(x$resource$study_upload)
  missing <- f$resource; missing$local_file <- file.path(f$root, 'absent')
  x <- d_t02_map(f, missing, cache=file.path(f$root, 'missing-cache'))
  testthat::expect_identical(x$status, 'unavailable')
  testthat::expect_identical(x$reason_code, 'D_RESOURCE_MISSING')
  testthat::expect_false(x$network_attempted)
  testthat::expect_false(x$study_upload)
  testthat::expect_identical(tryCatch(d_t02_map(f, version='wrong'), error=identity)$code, 'D_RESOURCE_RELEASE_MISMATCH')
  bad <- f$resource; bad$sha256 <- paste(rep('0',64), collapse='')
  testthat::expect_identical(tryCatch(d_t02_map(f, bad, hash=bad$sha256, cache=file.path(f$root,'bad-cache')), error=identity)$code, 'D_RESOURCE_HASH_MISMATCH')
  bad <- f$resource; bad$rights_status <- 'unverified'
  testthat::expect_identical(tryCatch(d_t02_map(f, bad), error=identity)$code, 'D_RESOURCE_RIGHTS')
})


d_t02_table <- function(f, transform) {
  tab <- read.delim(f$path, colClasses='character', check.names=FALSE, quote='')
  tab <- transform(tab)
  write.table(tab, f$path, sep='\t', row.names=FALSE, quote=FALSE)
  f$hash <- triomics_sha256(f$path); f$resource$sha256 <- f$hash
  f
}
d_t02_code <- function(expr) tryCatch({ force(expr); NULL }, error=function(e) e$code)

testthat::test_that('D-T02 repair rejects structural claims in every candidate cell', {
  claims <- c('PC(18:1(9Z)/16:0)', 'PC(18:1n9/16:0)', 'PC(34:1(9Z))',
              'sn-1 16:0', 'sn1-16:0', '16:0 at sn-1')
  for (claim in claims) {
    f <- d_t02_fixture(); f <- d_t02_table(f, function(t) { t$target_id[1] <- claim; t })
    testthat::expect_identical(d_t02_code(d_t02_map(f)), 'D_LIPID_RESOLUTION_CONFLICT', info=claim)
  }
  for (field in c('details', 'structure', 'relation', 'evidence_ref')) {
    claim <- if (field == 'structure') 'PC 16:0/18:1' else 'sn1=16:0; sn2=18:1'
    f <- d_t02_fixture(); f <- d_t02_table(f, function(t) { t[[field]] <- rep('unresolved', nrow(t)); t[[field]][1] <- claim; t })
    testthat::expect_identical(d_t02_code(d_t02_map(f)), 'D_LIPID_RESOLUTION_CONFLICT', info=field)
  }
  f <- d_t02_fixture(); f$rows[[1]]$source_id <- 'PC'; f$rows[[1]]$resolution <- 'class'; f$rows[[1]]$aliases <- list()
  f <- d_t02_table(f, function(t) { t$source_id[1] <- 'PC'; t$target_id[1] <- 'PC(34:1)'; t$resolution[1] <- 'class'; t })
  testthat::expect_identical(d_t02_code(d_t02_map(f)), 'D_LIPID_RESOLUTION_CONFLICT')
  for (token in c('unresolved', 'unknown', 'NA', 'none', 'not_applicable')) {
    f <- d_t02_fixture(); f <- d_t02_table(f, function(t) { t$sn_position <- token; t })
    testthat::expect_identical(d_t02_map(f)$status, 'available', info=token)
  }
})
testthat::test_that('D-T02 repair parses shorthand and rejects finer declarations', {
  for (label in c('PC(34:1)', 'PC 34:1')) {
    f <- d_t02_fixture(); f$rows[[1]]$source_id <- label
    x <- d_t02_map(f)$observations[[1]]
    testthat::expect_identical(c(x$class, x$carbons, x$unsaturations, x$resolution), c('PC','34','1','sum_composition'))
    testthat::expect_null(x$chains); testthat::expect_null(x$sn_positions); testthat::expect_null(x$isomer)
    for (resolution in c('molecular_species', 'sn_resolved')) {
      f$rows[[1]]$resolution <- resolution
      testthat::expect_identical(d_t02_code(d_t02_map(f)), 'D_LIPID_RESOLUTION_CONFLICT')
    }
  }
})
testthat::test_that('D-T02 repair emits normalized edges and detects disagreement', {
  f <- d_t02_fixture(); x <- d_t02_map(f)
  e <- Filter(function(e) e$feature_key == 'row1', x$edges)
  testthat::expect_length(e, 2L)
  testthat::expect_setequal(names(e[[1]]), c('feature_key','target_namespace','target_entity_id','relation','resolution','taxon','context','resource_release','evidence','ambiguity_group'))
  testthat::expect_identical(e[[1]]$ambiguity_group, e[[2]]$ambiguity_group)
  testthat::expect_true(is.character(e[[1]]$ambiguity_group) && nzchar(e[[1]]$ambiguity_group))
  testthat::expect_identical(e[[1]]$resource_release, '1')
  testthat::expect_identical(e[[1]]$taxon, '9606')
  testthat::expect_null(e[[1]]$context)
  testthat::expect_null(Filter(function(e) e$feature_key == 'row2', x$edges)[[1]]$ambiguity_group)
  f <- d_t02_table(f, function(t) { t$evidence_ref[4] <- t$evidence_ref[1]; t })
  testthat::expect_length(d_t02_map(f)$edges, 3L)
  f <- d_t02_table(f, function(t) { t$evidence_ref[4] <- 'different'; t })
  testthat::expect_identical(d_t02_code(d_t02_map(f)), 'D_LIPID_TABLE_INVALID')
})
testthat::test_that('D-T02 repair maps an alias-only identity without adding observations', {
  f <- d_t02_fixture(); f$rows <- list(f$rows[[1]])
  f <- d_t02_table(f, function(t) t[t$source_id == 'PC alias', , drop=FALSE])
  x <- d_t02_map(f)
  testthat::expect_length(x$observations, 1L); testthat::expect_length(x$edges, 1L)
  testthat::expect_identical(x$edges[[1]]$target_entity_id, 'PC34A')
  f$rows[[1]]$aliases <- list()
  testthat::expect_length(d_t02_map(f)$edges, 0L)
})
testthat::test_that('D-T02 repair rejects non-row and bundle inputs', {
  f <- d_t02_fixture()
  testthat::expect_identical(d_t02_code(triomics_map_lipids('bad', f$resource, 'lipid_ontology', '1', f$hash, file.path(f$root,'cache'))), 'D_LIPID_INPUT_INVALID')
  testthat::expect_identical(d_t02_code(triomics_map_lipids(list(artifact_type='evidence_bundle'), f$resource, 'lipid_ontology', '1', f$hash, file.path(f$root,'cache'))), 'D_LIPID_INPUT_INVALID')
})
