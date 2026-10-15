suppressPackageStartupMessages(library(limma))

protocol_seeds <- 1001:1400
n_features <- 100L

subject_design <- function(subjects, extra) {
  subject_ids <- sort(unique(subjects))
  subject_columns <- lapply(subject_ids, function(subject) as.numeric(subjects == subject))
  names(subject_columns) <- paste0("subject.", subject_ids)
  columns <- c(subject_columns, extra)
  design <- do.call(cbind, columns)
  colnames(design) <- names(columns)
  design
}

make_layout <- function(pattern) {
  if (pattern %in% c("balanced_complete_visits", "balanced_visits_one_cell_removed")) {
    subjects <- rep(paste0("S", 1:5), each = 3)
    visits <- rep(c("v1", "v2", "v3"), times = 5)
    design <- subject_design(subjects, list(
      visit.v2 = as.numeric(visits == "v2"),
      visit.v3 = as.numeric(visits == "v3")
    ))
    target <- "visit.v3"
    if (pattern == "balanced_visits_one_cell_removed") {
      design <- design[-3L, , drop = FALSE]
      subjects <- subjects[-3L]
    }
  } else if (pattern == "balanced_linear_complete") {
    subjects <- rep(c("A", "B", "C", "D"), each = 3)
    times <- rep(c(0, 1, 2), times = 4)
    design <- subject_design(subjects, list(time.linear = times))
    target <- "time.linear"
  } else if (pattern == "irregular_linear_complete") {
    subjects <- rep(c("A", "B", "C", "D"), each = 3)
    times <- c(0, 1, 4, 0, 2, 5, 1, 2, 6, 0, 3, 6)
    design <- subject_design(subjects, list(time.linear = times))
    target <- "time.linear"
  } else if (pattern == "natural_spline_complete") {
    subjects <- rep(c("A", "B", "C", "D"), each = 5)
    times <- rep(0:4, times = 4)
    basis <- splines::ns(times, knots = 2, Boundary.knots = c(0, 4))
    design <- subject_design(subjects, list(
      time.ns1 = as.numeric(basis[, 1L]),
      time.ns2 = as.numeric(basis[, 2L])
    ))
    target <- "time.ns1"
  } else {
    stop("Unknown pattern: ", pattern)
  }
  weights <- rep(0, ncol(design))
  names(weights) <- colnames(design)
  weights[[target]] <- 1
  list(pattern = pattern, subjects = subjects, design = design, weights = weights)
}

center_features <- function(generated, subjects) {
  centered <- generated
  for (subject in unique(subjects)) {
    index <- which(subjects == subject)
    centered[index, ] <- sweep(
      centered[index, , drop = FALSE], 2L,
      colMeans(centered[index, , drop = FALSE]), "-"
    )
  }
  centered
}

upper_bound <- function(x, n = length(protocol_seeds)) {
  stats::qbeta(0.95, x + 1, n - x)
}

lower_bound <- function(x, n) {
  stats::qbeta(0.05, x, n - x + 1)
}

probe_layout <- function(layout) {
  design <- layout$design
  weights <- layout$weights
  decomposition <- qr(design)
  df <- nrow(design) - decomposition$rank
  variance <- sum(weights * solve(crossprod(design), weights))
  ordinary_rejections <- 0L
  moderated_rejections <- 0L
  ordinary_covers <- 0L
  moderated_covers <- 0L
  moderated_study_coverage <- numeric(length(protocol_seeds))
  for (seed_index in seq_along(protocol_seeds)) {
    seed <- protocol_seeds[[seed_index]]
    set.seed(seed)
    generated <- matrix(stats::rnorm(nrow(design) * n_features), nrow = nrow(design))
    centered <- center_features(generated, layout$subjects)

    coefficients <- qr.coef(decomposition, centered)
    residual <- centered - qr.fitted(decomposition, centered)
    sigma2 <- colSums(residual^2) / df
    effects <- as.numeric(crossprod(weights, coefficients))
    ses <- sqrt(sigma2 * variance)
    pvals <- 2 * stats::pt(-abs(effects / ses), df)
    ordinary_rejections <- ordinary_rejections +
      as.integer(any(stats::p.adjust(pvals, "BH") < 0.05))
    ordinary_covers <- ordinary_covers +
      sum(abs(effects) <= stats::qt(0.975, df) * ses)

    fit <- limma::lmFit(t(centered), design)
    contrast <- matrix(weights, ncol = 1L, dimnames = list(names(weights), "target"))
    fit <- limma::eBayes(
      limma::contrasts.fit(fit, contrast),
      trend = FALSE,
      robust = FALSE
    )
    moderated_p <- as.numeric(fit$p.value[, 1L])
    moderated_effect <- as.numeric(fit$coefficients[, 1L])
    moderated_se <- sqrt(as.numeric(fit$s2.post)) *
      as.numeric(fit$stdev.unscaled[, 1L])
    moderated_df <- as.numeric(fit$df.total)
    moderated_rejections <- moderated_rejections +
      as.integer(any(stats::p.adjust(moderated_p, "BH") < 0.05))
    covered <- abs(moderated_effect) <= stats::qt(0.975, moderated_df) * moderated_se
    moderated_covers <- moderated_covers + sum(covered)
    moderated_study_coverage[[seed_index]] <- mean(covered)
  }
  n_intervals <- length(protocol_seeds) * n_features
  moderated_study_lower <- mean(moderated_study_coverage) -
    stats::qt(0.95, length(moderated_study_coverage) - 1L) *
    stats::sd(moderated_study_coverage) / sqrt(length(moderated_study_coverage))
  data.frame(
    pattern = layout$pattern,
    n = nrow(design),
    rank = decomposition$rank,
    df = df,
    ordinary_rejections = ordinary_rejections,
    ordinary_upper = upper_bound(ordinary_rejections),
    ordinary_coverage_lower = lower_bound(ordinary_covers, n_intervals),
    moderated_rejections = moderated_rejections,
    moderated_upper = upper_bound(moderated_rejections),
    moderated_coverage_lower_descriptive = lower_bound(moderated_covers, n_intervals),
    moderated_coverage_lower_by_study = moderated_study_lower
  )
}

patterns <- c(
  "balanced_complete_visits",
  "balanced_visits_one_cell_removed",
  "balanced_linear_complete",
  "irregular_linear_complete",
  "natural_spline_complete"
)

results <- do.call(rbind, lapply(patterns, function(pattern) {
  probe_layout(make_layout(pattern))
}))

print(results, row.names = FALSE, digits = 10)
cat("\nGate diagnostics under a study-level null rejection probability of 0.05:\n")
cat("P(X >= 23), X~Bin(400,.05):", stats::pbinom(22, 400, 0.05, lower.tail = FALSE), "\n")
cat("P(pass gate), X <= 22:", stats::pbinom(22, 400, 0.05), "\n")
cat("upper bound at 22/400:", upper_bound(22), "\n")
cat("upper bound at 23/400:", upper_bound(23), "\n")
cat(
  "R:", paste(R.version$major, R.version$minor, sep = "."),
  "limma:", as.character(utils::packageVersion("limma")), "\n"
)
