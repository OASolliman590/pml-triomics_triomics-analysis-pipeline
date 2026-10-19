# Packet C-05: MOFA2 adapter gate. The public registry still refuses "mofa2"
# (see triomics_integration_backend_names()); this gate is reachable only by the
# packet tests until Sol applies the shared registration.

triomics_mofa_abort <- function(code, message, key = NULL,
                                remediation = "Install MOFA2 and mofapy2 in an isolated environment, then rerun the C-05 checks.") {
  triomics_abort(
    code, message, key = key, module = "evaluation", stage = "C",
    remediation = remediation
  )
}

# Returns the explicit unavailable state when a native runtime is missing. Aborts
# when both runtimes load, because the training, save and reload path is not
# implemented in this revision and must not be reported as trained.
triomics_mofa_fit <- function(resolved_analysis, params, status = triomics_mofa_backend_status()) {
  if (!isTRUE(status$available)) {
    return(triomics_mofa_unavailable_state(status))
  }
  triomics_mofa_abort(
    "C_SCORE_BACKEND_UNAVAILABLE",
    "MOFA2 training, save and reload are not implemented in this revision; no factors, weights or variance were produced.",
    key = "integration.method"
  )
}
