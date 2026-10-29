triomics_d_route_status <- function(routes, resources, predecessor, cache) {
  named_list <- function(x) {
    is.list(x) && (!length(x) || (!is.null(names(x)) &&
      !anyNA(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))))
  }
  if (!named_list(routes) || any(!names(routes) %in% triomics_d_route_names())) {
    triomics_d_abort("D_STATUS_ROUTE_INVALID", "Routes must be uniquely named registered routes.", "routes")
  }
  for (name in names(routes)) {
    route <- routes[[name]]
    if (!is.list(route) || !is.logical(route$enabled) || length(route$enabled) != 1L ||
        is.na(route$enabled) || !is.logical(route$required) || length(route$required) != 1L ||
        is.na(route$required) || !is.character(route$status) || length(route$status) != 1L ||
        is.na(route$status) || !route$status %in% c("complete", "unavailable", "not_run")) {
      triomics_d_abort("D_STATUS_ROUTE_INVALID", "Each route needs enabled, required, and a valid status.", name)
    }
    if (name %in% c("mofa_native", "biopan") && identical(route$status, "complete")) {
      triomics_d_abort("D_STATUS_CONDITIONAL_INCOMPLETE", "Conditional adapters cannot be completed by this handler.", name)
    }
  }
  if (!is.list(predecessor) || !triomics_sha256_text(tolower(predecessor$b_sha256)) ||
      !triomics_sha256_text(tolower(predecessor$c_sha256))) {
    triomics_d_abort("D_STATUS_PREDECESSOR_INVALID", "B and C predecessor digests are required.", "predecessor")
  }
  if (!named_list(resources) || !named_list(cache) || any(!names(resources) %in% names(routes))) {
    triomics_d_abort("D_STATUS_RESOURCE_INVALID", "Resources and cache must be named lists associated with routes.", "resources")
  }
  checked <- list()
  for (name in names(resources)) {
    request <- resources[[name]]
    result <- tryCatch(
      triomics_d_resource_access(request$resource, request$operation, request$expected_version,
                                 request$expected_sha256, request$cache_dir, network_policy = "offline"),
      error = function(error) {
        error$cache_valid <- FALSE
        stop(error)
      }
    )
    result$cache_valid <- identical(result$status, "available") &&
      identical(cache[[name]]$content_sha256, result$content_sha256)
    checked[[name]] <- result
    if (identical(result$status, "unavailable")) {
      routes[[name]]$status <- "unavailable"
      routes[[name]]$reason_code <- result$reason_code
      routes[[name]]$reason <- result$reason
    } else if (isTRUE(routes[[name]]$enabled) && !result$cache_valid) {
      routes[[name]]$status <- "unavailable"
      routes[[name]]$reason_code <- "D_STATUS_CACHE_INVALID"
      routes[[name]]$reason <- "The cached output does not match the verified resource hash."
    }
  }
  unavailable <- routes[vapply(routes, function(route) isTRUE(route$enabled) &&
                                  identical(route$status, "unavailable"), logical(1))]
  incomplete <- any(vapply(routes, function(route) isTRUE(route$enabled) &&
                             isTRUE(route$required) && !identical(route$status, "complete"), logical(1)))
  list(run_completeness = if (incomplete) "incomplete" else "complete",
       exit_code = if (incomplete) 2L else 0L, biological_absence = FALSE,
       unavailable_routes = unavailable, resources = checked, predecessor = predecessor,
       capability_status = list(mofa_native = "incomplete", biopan = "incomplete"))
}
