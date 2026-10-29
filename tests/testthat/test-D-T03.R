d_t03_fixture <- function() {
  root <- tempfile('d-t03-'); dir.create(root)
  path <- file.path(root, 'mapping.tsv')
  writeLines(c('source_namespace\tsource_id\ttaxon_id\ttarget_namespace\ttarget_id\trelation\tresolution\tedge_type\tevidence_ref\tresource_id',
    'protein_group\tP1;P2\t9606\tUniProt\tP1-1\tmaps_to\tdeclared\treaction\te1\tmap',
    'protein_group\tP1;P2\t9606\tUniProt\tP2-2\tmaps_to\tdeclared\treaction\te2\tmap',
    'compound\tC\t9606\tref\tC1\tmaps_to\tdeclared\treaction\te3\tmap',
    'compound\tC\t9606\tref\tC2\tmaps_to\tdeclared\treaction\te4\tmap',
    'gene\tG\t9606\tGO\tGO:1\tfunction\tdeclared\tgo_function\te5\tmap',
    'gene\tG\t9606\tKEGG\tR1\tparticipates\tdeclared\treaction\te6\tmap',
    'GO\tunknown\t9606\tGO\tGO:coerced\tfunction\tdeclared\tgo_function\te7\tmap'), path)
  hash <- triomics_sha256(path)
  resource <- list(resource_id='map', source_url='local://synthetic', version='1', sha256=hash, local_file=path, terms_url='local://terms', rights_status='verified', permitted_operation=list('gene_reaction'), redistribution='allowed')
  nominations <- list(list(nomination_id='B', source_branch='primary_statistics'), list(nomination_id='C', source_branch='integration'))
  row <- function(key, id, ns) list(feature_key=key, source_id=id, source_namespace=ns, taxon_id='9606', nominations=nominations)
  list(root=root, path=path, hash=hash, resource=resource, rows=list(row('protein','P1;P2','protein_group'), row('compound','C','compound'), row('gene','G','gene'), row('unknown','unknown','gene')))
}
d_t03_map <- function(f, rows=f$rows, resource=f$resource, version='1', hash=f$hash, cache=file.path(f$root,'cache')) triomics_map_reactions(rows, resource, 'gene_reaction', version, hash, cache)
d_t03_code <- function(expr) tryCatch({force(expr); NULL}, error=function(e) e$code)
testthat::test_that('D-T03 AC1 AC2 AC4 retain distinct mappings and shared lineage', {
  f <- d_t03_fixture(); x <- d_t03_map(f)
  testthat::expect_length(x$observations, 4L)
  testthat::expect_length(x$edges, 6L)
  for (key in c('protein','compound')) {
    e <- Filter(function(z) z$feature_key == key, x$edges)
    testthat::expect_length(e, 2L)
    testthat::expect_true(nzchar(e[[1]]$ambiguity_group))
    testthat::expect_identical(e[[1]]$ambiguity_group, e[[2]]$ambiguity_group)
    testthat::expect_identical(e[[1]]$evidence_lineage, e[[2]]$evidence_lineage)
  }
  gene <- Filter(function(z) z$feature_key == 'gene', x$edges)
  testthat::expect_setequal(vapply(gene, `[[`, '', 'edge_type'), c('go_function','reaction'))
  testthat::expect_identical(gene[[1]]$target_namespace, 'GO')
  testthat::expect_identical(gene[[2]]$target_namespace, 'KEGG')
  testthat::expect_identical(x$observations[[4]]$mapping_status, 'unmapped')
  testthat::expect_false(any(vapply(x$edges, function(e) e$feature_key == 'unknown', logical(1))))
  testthat::expect_length(x$observations[[1]]$nominations, 2L)
  testthat::expect_false(x$observations[[1]]$independent_corroboration)
  testthat::expect_identical(x$observations[[1]]$evidence_lineage, x$edges[[1]]$evidence_lineage)
})
testthat::test_that('D-T03 rejects missing identity, invalid inputs and edge types', {
  f <- d_t03_fixture()
  for (field in c('source_namespace','taxon_id')) { rows <- f$rows; rows[[1]][[field]] <- NULL; testthat::expect_identical(d_t03_code(d_t03_map(f, rows)), 'D_REACTION_IDENTITY_REQUIRED') }
  resource <- f$resource; resource$resource_id <- NULL
  testthat::expect_identical(d_t03_code(d_t03_map(f, resource=resource)), 'D_REACTION_IDENTITY_REQUIRED')
  for (bad in list('bad', list(artifact_type='evidence_bundle'))) testthat::expect_identical(d_t03_code(d_t03_map(f, rows=bad)), 'D_REACTION_INPUT_INVALID')
  rows <- f$rows; rows[[1]]$nominations[[1]]$source_branch <- 'other'
  testthat::expect_identical(d_t03_code(d_t03_map(f, rows)), 'D_REACTION_INPUT_INVALID')
  tab <- read.delim(f$path, colClasses='character'); tab$edge_type[1] <- 'unknown'; write.table(tab, f$path, sep='\t', row.names=FALSE, quote=FALSE)
  f$hash <- triomics_sha256(f$path); f$resource$sha256 <- f$hash
  testthat::expect_identical(d_t03_code(d_t03_map(f, cache=file.path(f$root,'new'))), 'D_REACTION_TABLE_INVALID')
})
testthat::test_that('D-T03 AC3 enforces the resource service', {
  f <- d_t03_fixture(); x <- d_t03_map(f)
  testthat::expect_identical(x$resource$network_policy, 'offline')
  testthat::expect_false(x$resource$network_attempted); testthat::expect_false(x$resource$study_upload)
  missing <- f$resource; missing$local_file <- file.path(f$root,'absent')
  x <- d_t03_map(f, resource=missing, cache=file.path(f$root,'missing'))
  testthat::expect_identical(x$reason_code, 'D_RESOURCE_MISSING'); testthat::expect_identical(x$status, 'unavailable')
  testthat::expect_false(x$network_attempted); testthat::expect_false(x$study_upload)
  testthat::expect_identical(d_t03_code(d_t03_map(f, version='wrong')), 'D_RESOURCE_RELEASE_MISMATCH')
  bad <- f$resource; bad$sha256 <- paste(rep('0',64), collapse='')
  testthat::expect_identical(d_t03_code(d_t03_map(f, resource=bad, hash=bad$sha256, cache=file.path(f$root,'bad'))), 'D_RESOURCE_HASH_MISMATCH')
  bad <- f$resource; bad$rights_status <- 'unverified'
  testthat::expect_identical(d_t03_code(d_t03_map(f, resource=bad)), 'D_RESOURCE_RIGHTS')
})

d_t03_table <- function(f, change) {
  tab <- utils::read.delim(f$path, colClasses='character', check.names=FALSE)
  tab <- change(tab)
  utils::write.table(tab, f$path, sep='\t', row.names=FALSE, quote=FALSE)
  f$hash <- triomics_sha256(f$path); f$resource$sha256 <- f$hash
  f
}
testthat::test_that('repair reads the spec table without edge_type and preserves resolution', {
  f <- d_t03_fixture()
  f <- d_t03_table(f, function(t) { t$resolution <- c('isoform','protein_group', rep('compound',2), rep('gene',3)); t$edge_type <- NULL; t })
  x <- d_t03_map(f)
  testthat::expect_identical(vapply(x$edges[1:2], `[[`, '', 'resolution'), c('isoform','protein_group'))
  testthat::expect_setequal(vapply(x$edges, `[[`, '', 'edge_type'), c('reaction','go_function'))
  f <- d_t03_table(f, function(t) { t$resolution <- NULL; t })
  testthat::expect_identical(d_t03_code(d_t03_map(f, cache=file.path(f$root,'missing-resolution'))), 'D_REACTION_TABLE_INVALID')
})
testthat::test_that('repair rejects edge types that contradict target namespace', {
  for (row in c(1,5)) {
    f <- d_t03_fixture()
    f <- d_t03_table(f, function(t) { t$resolution <- 'declared'; t$edge_type[row] <- if (row == 1) 'go_function' else 'reaction'; t })
    testthat::expect_identical(d_t03_code(d_t03_map(f)), 'D_REACTION_TABLE_INVALID')
  }
})
testthat::test_that('repair groups ambiguity separately for each edge type', {
  f <- d_t03_fixture()
  f <- d_t03_table(f, function(t) { t$resolution <- 'declared'; extra <- t[5,]; extra$target_id <- 'GO:2'; rbind(t,extra) })
  x <- d_t03_map(f); gene <- Filter(function(e) e$feature_key == 'gene', x$edges)
  go <- Filter(function(e) e$edge_type == 'go_function', gene)
  reaction <- Filter(function(e) e$edge_type == 'reaction', gene)
  testthat::expect_length(go, 2L); testthat::expect_identical(go[[1]]$ambiguity_group, go[[2]]$ambiguity_group)
  testthat::expect_match(go[[1]]$ambiguity_group, 'gene'); testthat::expect_match(go[[1]]$ambiguity_group, 'go_function')
  testthat::expect_null(reaction[[1]]$ambiguity_group)
  group <- Filter(function(g) g$feature_key == 'gene', x$ambiguity_groups)
  testthat::expect_length(group, 1L); testthat::expect_identical(group[[1]]$edge_type, 'go_function')
})
testthat::test_that('repair binds both nominations and rejects duplicate ids or branches and other operations', {
  f <- d_t03_fixture(); f <- d_t03_table(f, function(t) { t$resolution <- 'declared'; t })
  x <- d_t03_map(f); lineage <- x$observations[[1]]$evidence_lineage
  testthat::expect_match(lineage, 'B'); testthat::expect_match(lineage, 'C')
  testthat::expect_false(x$observations[[1]]$independent_corroboration)
  for (field in c('nomination_id','source_branch')) {
    rows <- f$rows; rows[[1]]$nominations[[2]][[field]] <- rows[[1]]$nominations[[1]][[field]]
    testthat::expect_identical(d_t03_code(d_t03_map(f, rows)), 'D_REACTION_INPUT_INVALID')
  }
  testthat::expect_identical(d_t03_code(triomics_map_reactions(f$rows, f$resource, 'other', '1', f$hash, file.path(f$root,'other'))), 'D_REACTION_INPUT_INVALID')
})
