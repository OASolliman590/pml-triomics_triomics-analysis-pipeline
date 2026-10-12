triomics_use_execution_context <- function(seed, study_id) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed) || seed != trunc(seed)) {
    triomics_abort("A_SIMULATOR_SEED", "Execution seed must be one finite integer.",
                   key = "execution.seed", module = "simulator", stage = "A",
                   remediation = "Record an integer seed in the execution context. There is no hidden seed.")
  }
  if (!is.character(study_id) || length(study_id) != 1L || is.na(study_id) || !nzchar(study_id)) {
    triomics_abort("A_SIMULATOR_SEED", "Execution context requires a study id.",
                   key = "study_id", module = "simulator", stage = "A",
                   remediation = "Pass the synthetic study id with the seed.")
  }
  options(triomics.execution_context = list(
    seed = as.integer(seed),
    study_id = study_id,
    rng_kind = "Mersenne-Twister",
    normal_kind = "Inversion",
    sample_kind = "Rejection"
  ))
  invisible(getOption("triomics.execution_context"))
}

triomics_simulator_context <- function() {
  ctx <- getOption("triomics.execution_context")
  required <- c("seed", "study_id", "rng_kind", "normal_kind", "sample_kind")
  if (!is.list(ctx) || !all(required %in% names(ctx)) || !is.numeric(ctx$seed) ||
      length(ctx$seed) != 1L || is.na(ctx$seed)) {
    triomics_abort("A_SIMULATOR_SEED", "Simulator seed must be supplied through the execution context.",
                   key = "execution.seed", module = "simulator", stage = "A",
                   remediation = "Call triomics_use_execution_context(seed, study_id) before simulate_study().")
  }
  ctx
}

triomics_simulator_number <- function(value, key, minimum = NULL, maximum = NULL) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value)) {
    triomics_abort("A_SIMULATOR_ARGUMENT", paste0("'", key, "' must be one finite number."),
                   key = key, module = "simulator", stage = "A",
                   remediation = "Pass the named generator argument explicitly.")
  }
  if (!is.null(minimum) && value < minimum || !is.null(maximum) && value > maximum) {
    triomics_abort("A_SIMULATOR_ARGUMENT", paste0("'", key, "' is outside its generator range."),
                   key = key, module = "simulator", stage = "A",
                   remediation = "Use a finite value inside the documented generator range.")
  }
  value
}

# Formula a-t02-simulator-1. After set.seed(context seed): sample scores, then
# subject intercepts for paired/time-series only, then each named block's
# loadings, column-major noise, and row-major missingness uniforms.
simulate_study <- function(n_subjects,
                           n_features = c(prot = 800, lip = 400, met = 600),
                           design = c("paired", "two_group", "continuous", "timeseries"),
                           effect_size,
                           prop_affected,
                           missingness,
                           shared_factor_strength) {
  if (missing(n_features)) n_features <- eval(formals(simulate_study)$n_features)
  choices <- eval(formals(simulate_study)$design)
  if (missing(design)) {
    design <- choices[[1L]]
  } else if (!is.character(design) || length(design) != 1L || is.na(design) || !design %in% choices) {
    triomics_abort("A_SIMULATOR_DESIGN",
                   "design must be paired, two_group, continuous, or timeseries.",
                   key = "design", module = "simulator", stage = "A",
                   remediation = "Use two_group for an independent two-group design. The public enum is not widened.")
  }
  n_subjects <- triomics_simulator_number(n_subjects, "n_subjects", minimum = 1)
  if (n_subjects != trunc(n_subjects)) {
    triomics_abort("A_SIMULATOR_ARGUMENT", "n_subjects must be a positive integer.",
                   key = "n_subjects", module = "simulator", stage = "A",
                   remediation = "Pass a whole number of subjects or independent samples.")
  }
  n_subjects <- as.integer(n_subjects)
  if (identical(design, "two_group") && n_subjects < 2L) {
    triomics_abort("A_SIMULATOR_ARGUMENT", "two_group needs at least two independent samples.",
                   key = "n_subjects", module = "simulator", stage = "A",
                   remediation = "Use at least two subjects so both groups exist.")
  }
  if (!is.numeric(n_features) || length(n_features) < 1L || is.null(names(n_features)) ||
      any(!nzchar(names(n_features))) || anyDuplicated(names(n_features)) ||
      any(!is.finite(n_features)) || any(n_features != trunc(n_features)) || any(n_features < 1)) {
    triomics_abort("A_SIMULATOR_ARGUMENT",
                   "n_features must be a named vector of positive block sizes.",
                   key = "n_features", module = "simulator", stage = "A",
                   remediation = "Name each block explicitly. Names are labels, not profile dispatch.")
  }
  block_names <- names(n_features)
  n_features <- stats::setNames(as.integer(n_features), block_names)
  effect_size <- triomics_simulator_number(effect_size, "effect_size")
  prop_affected <- triomics_simulator_number(prop_affected, "prop_affected", 0, 1)
  missingness <- triomics_simulator_number(missingness, "missingness", 0, 1)
  shared_factor_strength <- triomics_simulator_number(shared_factor_strength, "shared_factor_strength")
  ctx <- triomics_simulator_context()

  if (identical(design, "two_group")) {
    sample_ids <- sprintf("sim%04d", seq_len(n_subjects))
    n_control <- as.integer(ceiling(n_subjects / 2))
    group <- c(rep("control", n_control), rep("treatment", n_subjects - n_control))
    metadata <- data.frame(sample_id = sample_ids, group = group,
                           subject_id = rep(NA_character_, n_subjects),
                           visit_id = rep(NA_character_, n_subjects),
                           stringsAsFactors = FALSE)
    subject_ids <- NULL
    schedule <- list(group_rule = "first_ceiling_half_control", levels = c("control", "treatment"))
    id_scheme <- "sim%04d"
    design_label <- "independent"
  } else if (identical(design, "paired")) {
    subject_ids <- sprintf("sub%04d", seq_len(n_subjects))
    visit <- rep(c("a", "b"), times = n_subjects)
    subject <- rep(subject_ids, each = 2L)
    sample_ids <- sprintf("sim%04d_%s", rep(seq_len(n_subjects), each = 2L), visit)
    metadata <- data.frame(sample_id = sample_ids, subject_id = subject, visit = visit,
                           visit_id = visit, stringsAsFactors = FALSE)
    schedule <- list(visits = c("a", "b"))
    id_scheme <- "sim%04d_%s"
    design_label <- "paired"
  } else if (identical(design, "continuous")) {
    sample_ids <- sprintf("sim%04d", seq_len(n_subjects))
    metadata <- data.frame(sample_id = sample_ids,
                           predictor = seq.int(0, n_subjects - 1L),
                           subject_id = rep(NA_character_, n_subjects),
                           visit_id = rep(NA_character_, n_subjects),
                           stringsAsFactors = FALSE)
    subject_ids <- NULL
    schedule <- list(predictor_origin = 0)
    id_scheme <- "sim%04d"
    design_label <- "continuous"
  } else {
    subject_ids <- sprintf("sub%04d", seq_len(n_subjects))
    time <- rep(c(0, 1, 2), times = n_subjects)
    subject <- rep(subject_ids, each = 3L)
    sample_ids <- sprintf("sim%04d_t%d", rep(seq_len(n_subjects), each = 3L), as.integer(time))
    metadata <- data.frame(sample_id = sample_ids, subject_id = subject, time = time,
                           visit_id = rep(NA_character_, length(sample_ids)),
                           stringsAsFactors = FALSE)
    schedule <- list(times = c(0, 1, 2), time_unit = "synthetic_step")
    id_scheme <- "sim%04d_t%d"
    design_label <- "time-series"
  }

  previous_kind <- RNGkind(ctx$rng_kind, ctx$normal_kind, ctx$sample_kind)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    RNGkind(previous_kind[[1L]], previous_kind[[2L]], previous_kind[[3L]])
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(ctx$seed)

  n_samples <- nrow(metadata)
  scores <- unname(stats::rnorm(n_samples))
  subject_intercepts <- NULL
  intercept_by_sample <- NULL
  if (!is.null(subject_ids)) {
    subject_intercepts <- stats::setNames(stats::rnorm(n_subjects), subject_ids)
    intercept_by_sample <- unname(subject_intercepts[metadata$subject_id])
  }

  study_blocks <- vector("list", length(block_names))
  truth_blocks <- study_blocks
  names(study_blocks) <- block_names
  names(truth_blocks) <- block_names
  for (name in block_names) {
    n_feat <- n_features[[name]]
    feature_ids <- sprintf("%s_f%04d", name, seq_len(n_feat))
    loadings <- unname(stats::rnorm(n_feat))
    noise <- matrix(stats::rnorm(n_feat * n_samples), nrow = n_feat, ncol = n_samples)
    affected_count <- as.integer(floor(prop_affected * n_feat + 1e-10))
    affected <- seq_len(affected_count)
    effect <- matrix(0, n_feat, n_samples)
    if (length(affected) > 0L) {
      if (identical(design, "two_group")) {
        effect[affected, metadata$group == "treatment"] <- effect_size
      } else if (identical(design, "paired")) {
        effect[affected, metadata$visit == "b"] <- effect_size
      } else if (identical(design, "continuous")) {
        effect[affected, ] <- outer(rep(effect_size, length(affected)), metadata$predictor)
      } else {
        effect[affected, ] <- outer(rep(effect_size, length(affected)), metadata$time)
      }
    }
    complete <- shared_factor_strength * outer(loadings, scores) + effect + noise
    if (!is.null(intercept_by_sample)) complete <- sweep(complete, 2L, intercept_by_sample, "+")
    missing <- matrix(stats::runif(n_feat * n_samples) < missingness,
                      nrow = n_feat, ncol = n_samples, byrow = TRUE)
    assay <- complete
    assay[missing] <- NA_real_
    rownames(assay) <- feature_ids
    colnames(assay) <- metadata$sample_id
    study_blocks[[name]] <- list(block_id = name, feature_ids = feature_ids, assay = assay)
    truth_blocks[[name]] <- list(
      loadings = loadings, noise = noise, effect = effect, complete = complete,
      missing = missing, affected_feature_ids = feature_ids[affected]
    )
  }

  list(
    study = list(study_id = ctx$study_id, design = design, metadata = metadata, blocks = study_blocks),
    truth = list(
      formula_version = "a-t02-simulator-1",
      formula = "complete = subject_intercept + shared_factor_strength * loading * score + effect + noise",
      design = design,
      design_label = design_label,
      seed = ctx$seed,
      rng = list(kind = ctx$rng_kind, normal_kind = ctx$normal_kind, sample_kind = ctx$sample_kind),
      n_subjects = n_subjects,
      n_features = n_features,
      effect_size = effect_size,
      prop_affected = prop_affected,
      missingness = missingness,
      shared_factor_strength = shared_factor_strength,
      affected_rule = "first_floor_prop_times_n",
      schedule = schedule,
      id_scheme = id_scheme,
      subject_intercepts = subject_intercepts,
      intercept_by_sample = intercept_by_sample,
      factor_scores = scores,
      blocks = truth_blocks
    )
  )
}
