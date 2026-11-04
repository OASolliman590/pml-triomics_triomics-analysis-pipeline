render_report <- function(report_bundle, report_spec) {
  required <- c("schema_version", "root", "config_sha256", "study_id",
                "analysis_id", "privacy_class", "audience", "artifacts")
  if (!is.list(report_bundle) || is.null(names(report_bundle)) ||
      any(!required %in% names(report_bundle))) {
    triomics_report_abort(
      "D_REPORT_BUNDLE_INVALID",
      "The report needs an immutable manifest of verified producer artifacts.",
      "report_bundle"
    )
  }
  invisible(NULL)
}
