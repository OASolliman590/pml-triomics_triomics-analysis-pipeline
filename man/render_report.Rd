\name{render_report}
\alias{render_report}
\title{Render an offline report from verified saved exports}
\description{
Renders a self-contained HTML report and machine-readable tables from saved
A, B, C, and D exports. It verifies source bytes and their declared lineage
before rendering. It does not fit models or launch predecessor stages.
}
\usage{
render_report(report_bundle, report_spec)
}
\arguments{
  \item{report_bundle}{A versioned manifest with \code{schema_version},
  \code{root}, \code{config_path}, \code{config_sha256}, \code{study_id},
  \code{analysis_id}, \code{privacy_class}, \code{audience}, and
  \code{artifacts}. Artifact references identify exact saved payloads,
  envelopes, and the current validated D generation. The report CLI constructs
  this manifest from the existing output root and verifies it again before use.}
  \item{report_spec}{A named list containing only
  \code{output_relative_path = "report"}. The destination must not already exist.}
}
\details{
The local Quarto executable and packaged templates are required. Use the
\code{report} CLI stage after the native A, B, C, and D commands have written
their artifacts under the same output root. Rendering writes a new
\code{report} directory containing HTML, TSV tables, JSON data and manifests,
and envelopes for the primary JSON artifacts. Optional unavailable routes
remain visible; incomplete required sections produce a partial report.

Only the supported audience and disclosure policy may be used. Participant
identifiers and raw clinical records are not report content. A rendered report
is not biological validation, clinical qualification, target binding evidence,
or acceptance of an unavailable method.
}
\value{
A list containing \code{status}, \code{exit_code}, \code{output_relative_path},
\code{manifest_sha256}, \code{html_sha256}, \code{results_rows},
\code{clinical_rows}, \code{stage_statuses}, and \code{section_statuses}.
The returned exit code is zero for a complete report or four when required
sections are incomplete. Invalid inputs and missing dependencies raise typed
errors handled by the CLI.
}
