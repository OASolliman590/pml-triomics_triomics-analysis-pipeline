triomics_report_abort <- function(code, message, key = NULL,
                                   remediation = "Use verified local producer exports and the packaged report template.") {
  triomics_abort(code, message, key = key, module = "report", stage = "report",
                 remediation = remediation)
}
