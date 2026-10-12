triomics_stage_registry <- function() {
  data.frame(
    stage = c("A", "B", "C", "D", "report", "all"),
    validation_only = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    execution = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

triomics_require_stage <- function(stage, validate_only = FALSE) {
  registry <- triomics_stage_registry()
  match <- match(stage, registry$stage)
  if (is.na(match)) {
    triomics_abort("A_STAGE_UNKNOWN", paste0("Unsupported stage '", stage, "'."),
                   key = "stage", stage = stage,
                   remediation = "Use A, B, C, D, report, or all.")
  }
  if (validate_only && !registry$validation_only[[match]]) {
    triomics_abort("A_STAGE_UNAVAILABLE", paste0("Validation is not implemented for stage '", stage, "'."),
                   key = "stage", stage = stage,
                   remediation = "Request validation-only for stage A until its consumer packet is accepted.")
  }
  if (!validate_only && !registry$execution[[match]]) {
    triomics_abort("A_STAGE_UNAVAILABLE", paste0("Execution is not implemented for stage '", stage, "'."),
                   key = "stage", stage = stage,
                   remediation = "Use --validate-only for the implemented stage A request.")
  }
  invisible(registry[match, , drop = FALSE])
}
