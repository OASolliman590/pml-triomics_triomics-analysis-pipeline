triomics_ora_upper_tail <- function(N, K, n, k) {
  counts <- list(N, K, n, k)
  valid <- all(vapply(counts, function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 0 && x == floor(x), logical(1)))
  if (!valid || K > N || n > N || k > K || k > n || k < max(0, n - (N - K))) {
    triomics_d_abort('D_ORA_COUNT_INVALID', 'Hypergeometric counts must describe a possible draw.', 'counts')
  }
  stats::phyper(k - 1, K, N - K, n, lower.tail = FALSE)
}
