t11_code <- function(expr) tryCatch({ force(expr); NA_character_ }, triomics_error = function(e) e$code)
t11_request <- function() {
  r <- triomics_target_request(list(namespace = 'synthetic', id = 'SYNTH_FCN3_LIKE', taxon_id = 9606, entity_type = 'protein'), 'assess', list())
  r$mechanism_class <- 'non_enzymatic'
  r
}
t11_link <- function() list(link_id = 'l1', relation = 'regulatory', sign = 'unsigned', context_id = 'ctx', source_id = 'src')
t11_obs <- function(id, status, value = 1, endpoint = 'metabolite_concentration', group = 'shared') {
  list(observation_id = id, endpoint = endpoint, value = value, dependency_group = group, status = status, link_id = 'l1')
}

test_that('documented paths preserve sign and context and nonenzymes reject reactions', {
  p <- triomics_target_profile(t11_request(), list(t11_link()), list())
  expect_identical(p$links[[1]]$sign, 'unsigned')
  expect_identical(p$links[[1]]$context_id, 'ctx')
  link <- t11_link(); link$relation <- 'enzymatic_reaction'
  expect_identical(t11_code(triomics_target_profile(t11_request(), list(link), list())), 'D_TARGET_REACTION_INVENTED')
  link <- t11_link(); link$sign <- NULL
  expect_identical(t11_code(triomics_target_profile(t11_request(), list(link), list())), 'D_TARGET_PATH_UNDOCUMENTED')
})

test_that('profiles retain evidence and alternatives expose noncomparable coverage', {
  obs <- list(t11_obs('a', 'support'), t11_obs('b', 'conflict'), t11_obs('c', 'ambiguous'), t11_obs('d', 'uninformative', NULL))
  p <- triomics_target_profile(t11_request(), list(t11_link()), obs)
  expect_identical(vapply(p$observations, `[[`, character(1), 'status'), c('support', 'conflict', 'ambiguous', 'uninformative'))
  expect_null(p$observations[[4]]$value)
  expect_identical(p$observations[[4]]$coverage, 'unmeasured')
  a <- triomics_target_alternatives(list(list(target_id = 'a', axes = 'function', coverage = 'measured'), list(target_id = 'b', axes = 'clinical_change', coverage = 'unmeasured')))
  expect_identical(a$comparability, 'noncomparable')
  expect_null(a$selected_target)
  expect_identical(a$tie_status, 'unresolved')
  b <- triomics_target_alternatives(list(list(target_id = 'a', axes = 'function', coverage = 'measured'), list(target_id = 'b', axes = 'function', coverage = 'measured')))
  expect_null(b$selected_target)
  expect_identical(b$tie_status, 'unresolved')
})

test_that('shared dependency groups count once', {
  p <- triomics_target_profile(t11_request(), list(t11_link()), list(t11_obs('a', 'support'), t11_obs('b', 'support')))
  expect_identical(p$independent_count, 1L)
  expect_identical(vapply(p$observations, `[[`, character(1), 'path_sign'), c('unsigned', 'unsigned'))
  expect_identical(vapply(p$observations, `[[`, character(1), 'path_context'), c('ctx', 'ctx'))
})

test_that('binding probability is not a measured endpoint', {
  for (endpoint in c('binding_probability', 'Binding_Probability')) {
    obs <- t11_obs('a', 'support', endpoint = endpoint)
    expect_identical(
      t11_code(triomics_target_profile(t11_request(), list(t11_link()), list(obs))),
      'D_TARGET_CLAIM_COLLAPSED'
    )
  }
})

test_that('clinical change remains its endpoint and affinity claims are rejected', {
  obs <- t11_obs('c', 'support', endpoint = 'clinical_change')
  p <- triomics_target_profile(t11_request(), list(t11_link()), list(obs))
  expect_identical(p$observations[[1]]$endpoint, 'clinical_change')
  expect_false('kd' %in% names(p))
  expect_false('kd' %in% names(p$observations[[1]]))
  obs$kd <- 1
  expect_identical(t11_code(triomics_target_profile(t11_request(), list(t11_link()), list(obs))), 'D_TARGET_CLAIM_COLLAPSED')
})

test_that('observations require a matching documented path', {
  obs <- t11_obs('a', 'support'); obs$link_id <- NULL
  expect_identical(t11_code(triomics_target_profile(t11_request(), list(t11_link()), list(obs))), 'D_TARGET_PATH_UNDOCUMENTED')
  obs$link_id <- 'absent'
  expect_identical(t11_code(triomics_target_profile(t11_request(), list(t11_link()), list(obs))), 'D_TARGET_PATH_UNDOCUMENTED')
  obs$link_id <- 'l1'
  expect_identical(t11_code(triomics_target_profile(t11_request(), list(), list(obs))), 'D_TARGET_PATH_UNDOCUMENTED')
})

test_that('alternatives reject mechanistic claim axes', {
  for (axis in c('binding', 'flux', 'activity', 'causal_mediation', 'kd')) {
    profiles <- list(list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'), list(target_id = 'b', axes = axis, coverage = 'measured'))
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }
})

test_that('claim field names are rejected exactly and case insensitively', {
  for (field in c('binding', 'activity', 'BiNdInG', 'ACTIVITY')) {
    obs <- t11_obs('a', 'support'); obs[[field]] <- 1
    expect_identical(t11_code(triomics_target_profile(t11_request(), list(t11_link()), list(obs))), 'D_TARGET_CLAIM_COLLAPSED')
  }
  obs <- t11_obs('a', 'support'); obs$enzyme_activity <- 1
  expect_identical(triomics_target_profile(t11_request(), list(t11_link()), list(obs))$observations[[1]]$enzyme_activity, 1)
})

test_that('alternatives reject claim fields and binding probability axis', {
  for (field in c('kd', 'binding', 'KD', 'Binding')) {
    profiles <- list(list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'), list(target_id = 'b', axes = 'clinical_change', coverage = 'measured'))
    profiles[[2]][[field]] <- 1
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }
  profiles[[2]]$Binding <- NULL; profiles[[2]]$axes <- 'binding_probability'
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
})

test_that('mechanism class is required and the synthetic target is nonenzymatic', {
  request <- t11_request(); request$mechanism_class <- NULL
  expect_identical(t11_code(triomics_target_profile(request, list(), list())), 'D_TARGET_INPUT_INVALID')
  request$mechanism_class <- 'enzymatic'
  expect_identical(t11_code(triomics_target_profile(request, list(), list())), 'D_TARGET_INPUT_INVALID')
  request$mechanism_class <- 'unknown'
  expect_identical(t11_code(triomics_target_profile(request, list(), list())), 'D_TARGET_INPUT_INVALID')
})

test_that('observations inherit the specific cited path', {
  link <- t11_link(); link$link_id <- 'l2'; link$sign <- 'negative'; link$context_id <- 'other_ctx'
  obs <- t11_obs('a', 'support'); obs$link_id <- 'l2'
  p <- triomics_target_profile(t11_request(), list(t11_link(), link), list(obs))
  expect_identical(p$observations[[1]]$path_sign, 'negative')
  expect_identical(p$observations[[1]]$path_context, 'other_ctx')
})
