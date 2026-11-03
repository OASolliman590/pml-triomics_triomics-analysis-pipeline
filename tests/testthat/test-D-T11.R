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

test_that('alternatives reject nested mechanistic observation endpoints', {
  for (endpoint in c('binding_probability', 'Binding_Probability', 'kd', 'flux', 'causal_mediation', 'binding', 'activity')) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
           observations = list(list(endpoint = endpoint, value = 1, coverage = 'measured')))
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         nested = list(axes = 'Binding_Probability'))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured',
         observations = list(list(endpoint = 'clinical_change', value = 1, coverage = 'measured'))),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = list(list(endpoint = 'metabolite_concentration', value = 1, coverage = 'measured')))
  )
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(kept$profiles[[2]]$observations[[1]]$endpoint, 'metabolite_concentration')
})

test_that('alternatives reject non-character mechanistic endpoints', {
  for (endpoint in c('binding_probability', 'Binding_Probability', 'kd', 'flux', 'causal_mediation', 'binding', 'activity')) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
           observations = list(list(endpoint = factor(endpoint, levels = endpoint), value = 1, coverage = 'measured')))
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         nested = list(axes = factor('binding_probability', levels = 'binding_probability')))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = list(list(endpoint = as.symbol('binding_probability'), value = 1, coverage = 'measured')))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = list(list(endpoint = factor('clinical_change', levels = 'clinical_change'), value = 1, coverage = 'measured')))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_INPUT_INVALID')
})

test_that('alternatives reject a claim hidden by a missing endpoint cell', {
  for (endpoint in c('binding_probability', 'Binding_Probability', 'kd', 'flux', 'causal_mediation', 'binding', 'activity')) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
           observations = data.frame(endpoint = c(endpoint, NA_character_), value = c(1, NA_real_),
                                     coverage = c('measured', 'unmeasured'), stringsAsFactors = FALSE))
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = data.frame(endpoint = factor(c('binding_probability', NA_character_)),
                                   value = c(1, NA_real_), coverage = c('measured', 'unmeasured')))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         nested = list(axes = c('Binding_Probability', NA_character_)))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured',
         observations = data.frame(endpoint = c('clinical_change', NA_character_), value = c(1, NA_real_),
                                   coverage = c('measured', 'unmeasured'), stringsAsFactors = FALSE)),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = data.frame(endpoint = c('metabolite_concentration', NA_character_), value = c(1, NA_real_),
                                   coverage = c('measured', 'unmeasured'), stringsAsFactors = FALSE))
  )
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(as.character(kept$profiles[[1]]$observations$endpoint), c('clinical_change', NA_character_))
  expect_identical(as.character(kept$profiles[[2]]$observations$endpoint), c('metabolite_concentration', NA_character_))
  expect_identical(kept$tie_status, 'unresolved')
})

test_that('alternatives inspect every duplicate endpoint and axes element', {
  claim_texts <- c('binding', 'flux', 'activity', 'causal_mediation', 'kd', 'binding_probability')
  claims <- c(claim_texts, toupper(claim_texts), 'Binding_Probability')
  for (claim in claims) {
    for (cells in list(claim, c(claim, NA_character_))) {
      observations <- c(
        list(endpoint = 'clinical_change', value = 1, coverage = 'measured'),
        list(endpoint = cells)
      )
      profiles <- list(
        list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
        list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observations)
      )
      expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')

      duplicate_axes <- c(list(axes = 'clinical_change'), list(axes = cells))
      profiles <- list(
        list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
        list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', nested = duplicate_axes)
      )
      expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
    }
  }

  duplicate_endpoints <- data.frame(
    endpoint = c('clinical_change', NA_character_),
    endpoint = c('binding_probability', NA_character_),
    value = c(1, NA_real_),
    coverage = c('measured', 'unmeasured'),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = duplicate_endpoints)
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
})

test_that('alternatives reject claims in named atomic vectors', {
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = list(c(endpoint = 'binding_probability', value = '1', coverage = 'measured')))
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')

  profiles[[2]]$observations <- list(c(endpoint = 'clinical_change', binding_probability = '1', coverage = 'measured'))
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
})

test_that('alternatives reject claims in partial-match axis fields', {
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes_x = 'binding_probability', coverage = 'measured')
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')

  profiles[[2]]$axes <- 'clinical_change'
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
})

test_that('alternatives reject claim tokens in matrix dimnames', {
  for (column in c('binding_probability', 'kd')) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
           observations = matrix(0.9, nrow = 1, dimnames = list(NULL, column)))
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }

  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = matrix(0.9, nrow = 1, dimnames = list(NULL, 'metabolite_concentration')))
  )
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(dimnames(kept$profiles[[2]]$observations)[[2]], 'metabolite_concentration')
})

test_that('alternatives scan list matrix dimnames and preserve metabolite labels', {
  for (column in c('binding_probability', 'kd')) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
           observations = matrix(list(0.9), nrow = 1, dimnames = list(NULL, column)))
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }

  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = matrix(list(0.9), nrow = 1, dimnames = list(NULL, 'metabolite_concentration')))
  )
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(dimnames(kept$profiles[[2]]$observations)[[2]], 'metabolite_concentration')
})

test_that('alternatives scan data frame row names and preserve metabolite labels', {
  for (row_name in c('binding_probability', 'kd')) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
           observations = data.frame(value = 0.9, row.names = row_name))
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }

  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = data.frame(value = 0.9, row.names = 'metabolite_concentration'))
  )
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(rownames(kept$profiles[[2]]$observations), 'metabolite_concentration')
})

test_that('alternatives reject claim attributes and preserve nonclaim attributes', {
  claimed <- list(
    structure(0.9, endpoint = 'binding_probability'),
    structure(0.9, kd = 1),
    structure(0.9, label = 'flux'),
    structure(list(0.9), class = 'causal_mediation'),
    factor('clinical_change', levels = c('clinical_change', 'binding_probability'))
  )
  for (observation in claimed) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }

  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured',
         observations = structure(0.9, endpoint = 'clinical_change', comment = 'metabolite_concentration'))
  )
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(kept$profiles[[2]]$observations, profiles[[2]]$observations)

  profiles[[2]]$observations <- factor('clinical_change', levels = 'clinical_change')
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(as.character(kept$profiles[[2]]$observations), 'clinical_change')

  profiles[[2]]$observations <- data.frame(value = 0.9)
  kept <- triomics_target_alternatives(profiles)
  expect_identical(class(kept$profiles[[2]]$observations), 'data.frame')
  expect_identical(rownames(kept$profiles[[2]]$observations), '1')
})

test_that('alternatives reject claim symbols outside endpoint and axes fields', {
  profiles <- list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes_x = as.symbol('binding_probability'), coverage = 'measured')
  )
  expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')

  profiles[[2]]$axes <- 'clinical_change'
  profiles[[2]]$axes_x <- as.symbol('metabolite_concentration')
  kept <- triomics_target_alternatives(profiles)
  expect_null(kept$selected_target)
  expect_identical(kept$profiles[[2]]$axes_x, as.symbol('metabolite_concentration'))
})

test_that('alternatives walk calls and expressions for exact claim tokens', {
  claimed <- list(
    quote(list(binding_probability = 0.9)),
    quote(f(kd = 0.9)),
    quote(f(g(binding_probability))),
    expression(binding_probability = 0.9)
  )
  for (observation in claimed) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }

  kept_values <- list(
    quote(f(metabolite_concentration = 0.9)),
    quote(f('g(binding_probability)')),
    expression(metabolite_concentration = 0.9)
  )
  for (observation in kept_values) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
    )
    kept <- triomics_target_alternatives(profiles)
    expect_null(kept$selected_target)
    expect_identical(kept$profiles[[2]]$observations, observation)
  }
})

test_that('alternatives recursively inspect nested attribute values', {
  claimed <- list(
    structure(0.9, label = list('binding_probability')),
    structure(0.9, label = structure('clinical_change', note = 'binding_probability'))
  )
  for (observation in claimed) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
    )
    expect_identical(t11_code(triomics_target_alternatives(profiles)), 'D_TARGET_CLAIM_COLLAPSED')
  }

  kept_values <- list(
    structure(0.9, label = list('metabolite_concentration')),
    structure(0.9, label = structure('clinical_change', note = 'metabolite_concentration'))
  )
  for (observation in kept_values) {
    profiles <- list(
      list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
      list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
    )
    kept <- triomics_target_alternatives(profiles)
    expect_null(kept$selected_target)
    expect_identical(kept$profiles[[2]]$observations, observation)
  }
})

test_that('alternatives inspect closure formals and bodies without invoking them', {
  profile <- function(observation) list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
  )
  claimed <- list(
    function() list(binding_probability = 0.9),
    function(binding_probability = 0.9) NULL,
    function() list(kd = 0.9)
  )
  for (observation in claimed) {
    expect_identical(t11_code(triomics_target_alternatives(profile(observation))), 'D_TARGET_CLAIM_COLLAPSED')
  }

  kept <- list(
    function() list(metabolite_concentration = 0.9),
    function(note = 0.9) NULL,
    sum
  )
  captured <- new.env(parent = baseenv())
  captured$binding_probability <- 0.9
  kept[[length(kept) + 1L]] <- eval(quote(function() NULL), envir = captured)
  for (observation in kept) {
    result <- triomics_target_alternatives(profile(observation))
    expect_null(result$selected_target)
    expect_identical(result$profiles[[2]]$observations, observation)
  }
})

test_that('alternatives inspect formula expressions for exact claim tokens', {
  profile <- function(observation) list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
  )
  expect_identical(
    t11_code(triomics_target_alternatives(profile(as.formula('~ binding_probability')))),
    'D_TARGET_CLAIM_COLLAPSED'
  )
  observation <- as.formula('~ metabolite_concentration')
  result <- triomics_target_alternatives(profile(observation))
  expect_null(result$selected_target)
  expect_identical(result$profiles[[2]]$observations, observation)
})

test_that('claim attributes take precedence over earlier invalid endpoint labels', {
  profile <- function(observation) list(
    list(target_id = 'a', axes = 'clinical_change', coverage = 'measured'),
    list(target_id = 'b', axes = 'clinical_change', coverage = 'measured', observations = observation)
  )
  expect_claim_abort <- function(observation) {
    error <- tryCatch(triomics_target_alternatives(profile(observation)), triomics_error = identity)
    expect_identical(error$code, 'D_TARGET_CLAIM_COLLAPSED')
    expect_identical(conditionMessage(error), 'Mechanistic claim axes are not permitted.')
  }

  expect_claim_abort(structure(
    0.9,
    metadata = list(endpoint = 1),
    note = 'binding_probability'
  ))
  expect_claim_abort(structure(
    function() NULL,
    metadata = list(endpoint = 1),
    note = 'binding_probability'
  ))

  observation <- structure(
    0.9,
    metadata = list(endpoint = 'clinical_change'),
    note = 'metabolite_concentration'
  )
  result <- triomics_target_alternatives(profile(observation))
  expect_null(result$selected_target)
  expect_identical(result$profiles[[2]]$observations, observation)
})

test_that('mechanism class is required and the synthetic target is nonenzymatic', {
  request <- t11_request(); request$mechanism_class <- NULL
  expect_identical(t11_code(triomics_target_profile(request, list(), list())), 'D_TARGET_INPUT_INVALID')
  request$mechanism_class <- 'enzymatic'
  expect_identical(t11_code(triomics_target_profile(request, list(), list())), 'D_TARGET_INPUT_INVALID')
  request$mechanism_class <- 'unknown'
  expect_identical(t11_code(triomics_target_profile(request, list(), list())), 'D_TARGET_INPUT_INVALID')
})

t11_investigation <- function(targets = list(list(namespace = 'synthetic', id = 'SYNTH_FCN3_LIKE', taxon_id = 9606, entity_type = 'protein'))) {
  list(enabled = TRUE, mode = 'assess', targets = targets, drug_ref = NULL,
       context_ref = 'synthetic_context', external_evidence_file = NULL,
       mechanism_file = NULL, protocol_ref = 'synthetic_target_protocol', required = FALSE)
}

test_that('stage D records one unmeasured profile and does not choose an alternative', {
  slot <- list(status = 'unavailable', reason_code = 'D_TARGET_EVIDENCE_ABSENT', results = list())
  attached <- triomics_d_attach_target_profiles(slot, t11_investigation())
  expect_identical(attached$status, 'unavailable')
  expect_identical(attached$reason_code, 'D_TARGET_EVIDENCE_ABSENT')
  expect_identical(length(attached$profiles), 1L)
  expect_identical(attached$profiles[[1]]$request$mechanism_class, 'non_enzymatic')
  expect_null(attached$profiles[[1]]$request$drug)
  expect_identical(attached$profiles[[1]]$coverage, 'unmeasured')
  expect_identical(attached$profiles[[1]]$independent_count, 0L)
  expect_length(attached$profiles[[1]]$links, 0L)
  expect_null(attached$alternatives)
  expect_null(attached$selected_target)
  expect_null(attached$tie_status)
  expect_identical(attached$mechanism_status, 'not_supplied')
})

test_that('stage D compares profiles only when two declared targets have a known class', {
  targets <- list(
    list(namespace = 'synthetic', id = 'SYNTH_FCN3_LIKE', taxon_id = 9606, entity_type = 'protein'),
    list(namespace = 'synthetic', id = 'SYNTH_FCN3_LIKE', taxon_id = 9606, entity_type = 'protein')
  )
  attached <- triomics_d_attach_target_profiles(list(status = 'unavailable', reason_code = 'D_TARGET_EVIDENCE_ABSENT', results = list()), t11_investigation(targets))
  expect_identical(length(attached$profiles), 2L)
  expect_identical(attached$alternatives$comparability, 'noncomparable')
  expect_null(attached$alternatives$selected_target)
  expect_identical(attached$alternatives$tie_status, 'unresolved')
  expect_null(attached$selected_target)
  other <- t11_investigation(list(list(namespace = 'synthetic', id = 'OTHER_TARGET', taxon_id = 9606, entity_type = 'protein')))
  expect_identical(t11_code(triomics_d_attach_target_profiles(list(), other)), 'D_TARGET_INPUT_INVALID')
})

test_that('stage D does not invent profiles from an evidence or mechanism file', {
  slot <- list(status = 'recorded', reason_code = NULL, results = list(list(kept = TRUE)))
  evidence <- t11_investigation()
  evidence$external_evidence_file <- 'evidence.json'
  kept <- triomics_d_attach_target_profiles(slot, evidence)
  expect_null(kept$profiles)
  expect_identical(kept$results[[1]]$kept, TRUE)
  mechanism <- t11_investigation()
  mechanism$mechanism_file <- 'mechanism.json'
  kept <- triomics_d_attach_target_profiles(slot, mechanism)
  expect_null(kept$profiles)
  disabled <- t11_investigation()
  disabled$enabled <- FALSE
  kept <- triomics_d_attach_target_profiles(slot, disabled)
  expect_null(kept$profiles)
})

test_that('stage D execution calls the target profile attachment', {
  expect_true(any(grepl('triomics_d_attach_target_profiles', deparse(body(triomics_execute_stage_d)), fixed = TRUE)))
  expect_true(any(grepl('triomics_target_profile', deparse(body(triomics_d_attach_target_profiles)), fixed = TRUE)))
  expect_true(any(grepl('triomics_target_alternatives', deparse(body(triomics_d_attach_target_profiles)), fixed = TRUE)))
})

test_that('observations inherit the specific cited path', {
  link <- t11_link(); link$link_id <- 'l2'; link$sign <- 'negative'; link$context_id <- 'other_ctx'
  obs <- t11_obs('a', 'support'); obs$link_id <- 'l2'
  p <- triomics_target_profile(t11_request(), list(t11_link(), link), list(obs))
  expect_identical(p$observations[[1]]$path_sign, 'negative')
  expect_identical(p$observations[[1]]$path_context, 'other_ctx')
})
