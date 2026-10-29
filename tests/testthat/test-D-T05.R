d_t05_predecessor <- list(b_sha256 = paste(rep('a', 64), collapse = ''), c_sha256 = paste(rep('B', 64), collapse = ''))
d_t05_route <- function(enabled = TRUE, required = FALSE, status = 'unavailable', reason = 'source unavailable') {
  list(enabled = enabled, required = required, status = status, reason = reason)
}
d_t05_fixture <- function() {
  root <- tempfile('d-t05-'); dir.create(root)
  path <- file.path(root, 'resource.tsv'); writeLines('synthetic resource', path)
  hash <- triomics_sha256(path)
  resource <- list(resource_id = 'test', version = '1', sha256 = hash,
                   source_url = 'local://test', local_file = path, terms_url = 'local://terms',
                   rights_status = 'verified', permitted_operation = list('gene_reaction'), redistribution = 'prohibited')
  list(root = root, resource = resource, request = list(resource = resource, operation = 'gene_reaction',
       expected_version = '1', expected_sha256 = hash, cache_dir = file.path(root, 'resource-cache')),
       cache = list(content_sha256 = hash))
}

testthat::test_that('optional and required unavailable routes have distinct exit behavior', {
  route <- d_t05_route()
  optional <- triomics_d_route_status(list(gene_reaction = route), list(), d_t05_predecessor, list())
  testthat::expect_identical(optional$run_completeness, 'complete')
  testthat::expect_identical(optional$exit_code, 0L)
  testthat::expect_identical(optional$unavailable_routes$gene_reaction$reason, 'source unavailable')
  route$required <- TRUE
  required <- triomics_d_route_status(list(gene_reaction = route), list(), d_t05_predecessor, list())
  testthat::expect_identical(required$run_completeness, 'incomplete')
  testthat::expect_identical(required$exit_code, 2L)
  testthat::expect_false(required$biological_absence)
})

testthat::test_that('offline resources enforce rights, hashes and cached output identity', {
  f <- d_t05_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  routes <- list(gene_reaction = d_t05_route(status = 'complete'))
  run <- function(request, cache = f$cache) triomics_d_route_status(routes, list(gene_reaction = request), d_t05_predecessor, list(gene_reaction = cache))
  good <- run(f$request)
  testthat::expect_true(good$resources$gene_reaction$cache_valid)
  changed <- run(f$request, list(content_sha256 = paste(rep('0', 64), collapse = '')))
  testthat::expect_false(changed$resources$gene_reaction$cache_valid)
  testthat::expect_identical(changed$exit_code, 0L)
  testthat::expect_identical(changed$unavailable_routes$gene_reaction$reason_code, 'D_STATUS_CACHE_INVALID')
  routes$gene_reaction$required <- TRUE
  required <- run(f$request, list(content_sha256 = paste(rep('0', 64), collapse = '')))
  testthat::expect_identical(required$run_completeness, 'incomplete')
  testthat::expect_identical(required$exit_code, 2L)
  testthat::expect_false(required$biological_absence)
  testthat::expect_identical(required$unavailable_routes$gene_reaction$reason_code, 'D_STATUS_CACHE_INVALID')
  testthat::expect_false(required$resources$gene_reaction$cache_valid)
  routes$gene_reaction$enabled <- FALSE
  disabled <- run(f$request, list(content_sha256 = paste(rep('0', 64), collapse = '')))
  testthat::expect_identical(disabled$run_completeness, 'complete')
  testthat::expect_length(disabled$unavailable_routes, 0L)
  routes$gene_reaction$enabled <- TRUE
  bad <- f$request; bad$resource$rights_status <- 'unverified'
  error <- tryCatch(run(bad), error = identity)
  testthat::expect_identical(error$code, 'D_RESOURCE_RIGHTS')
  testthat::expect_false(error$cache_valid)
  bad <- f$request; bad$resource$sha256 <- paste(rep('0', 64), collapse = ''); bad$expected_sha256 <- bad$resource$sha256
  error <- tryCatch(run(bad), error = identity)
  testthat::expect_identical(error$code, 'D_RESOURCE_HASH_MISMATCH')
  testthat::expect_false(error$cache_valid)
  missing <- f$request; missing$resource$local_file <- file.path(f$root, 'missing'); missing$cache_dir <- file.path(f$root, 'empty')
  result <- run(missing)
  testthat::expect_identical(result$unavailable_routes$gene_reaction$reason_code, 'D_RESOURCE_MISSING')
  testthat::expect_identical(result$run_completeness, 'incomplete')
  testthat::expect_identical(result$exit_code, 2L)
  testthat::expect_false(result$biological_absence)
  testthat::expect_false(result$resources$gene_reaction$cache_valid)
  writeLines('corrupted cached resource', good$resources$gene_reaction$path)
  error <- tryCatch(run(f$request), error = identity)
  testthat::expect_identical(error$code, 'D_RESOURCE_HASH_MISMATCH')
  testthat::expect_false(error$cache_valid)
})

testthat::test_that('disabled and partial routes preserve predecessor evidence and bytes', {
  root <- tempfile('d-t05-predecessor-'); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  writeBin(as.raw(0:255), file.path(root, 'evidence.bin'))
  before <- readBin(file.path(root, 'evidence.bin'), 'raw', n = 256)
  disabled <- setNames(lapply(triomics_d_route_names(), function(x) d_t05_route(FALSE, FALSE, 'not_run')), triomics_d_route_names())
  result <- triomics_d_route_status(disabled, list(), d_t05_predecessor, list())
  testthat::expect_identical(result$run_completeness, 'complete')
  testthat::expect_identical(result$exit_code, 0L)
  testthat::expect_false(result$biological_absence)
  testthat::expect_identical(result$predecessor, d_t05_predecessor)
  disabled$ora <- d_t05_route(TRUE, FALSE, 'unavailable')
  partial <- triomics_d_route_status(disabled, list(), d_t05_predecessor, list())
  testthat::expect_identical(partial$predecessor, d_t05_predecessor)
  testthat::expect_identical(readBin(file.path(root, 'evidence.bin'), 'raw', n = 256), before)
  testthat::expect_identical(partial$capability_status, list(mofa_native = 'incomplete', biopan = 'incomplete'))
})

testthat::test_that('unknown routes and completed conditional adapters are rejected', {
  error <- tryCatch(triomics_d_route_status(list(unknown = d_t05_route()), list(), d_t05_predecessor, list()), error = identity)
  testthat::expect_identical(error$code, 'D_STATUS_ROUTE_INVALID')
  disabled <- list(mofa_native = d_t05_route(FALSE, FALSE, 'complete'))
  error <- tryCatch(triomics_d_route_status(disabled, list(), d_t05_predecessor, list()), error = identity)
  testthat::expect_identical(error$code, 'D_STATUS_CONDITIONAL_INCOMPLETE')
  for (name in c('mofa_native', 'biopan')) {
    routes <- setNames(list(d_t05_route(status = 'complete')), name)
    error <- tryCatch(triomics_d_route_status(routes, list(), d_t05_predecessor, list()), error = identity)
    testthat::expect_identical(error$code, 'D_STATUS_CONDITIONAL_INCOMPLETE')
  }
})
