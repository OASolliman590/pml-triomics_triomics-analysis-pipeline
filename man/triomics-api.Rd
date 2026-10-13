\name{triomics-api}
\alias{simulate_study}
\alias{triomics_export_a}
\alias{triomics_export_cache_key}
\alias{triomics_export_resume_decision}
\alias{triomics_a_handoff_limitations}
\alias{triomics_resolve_analysis}
\alias{triomics_fit_block_models}
\alias{triomics_execute_stage_b}
\alias{resolve_analysis}
\alias{fit_block_models}
\title{Triomics simulation, export, and stage B analysis entry points}
\description{
Public entry points for generating synthetic studies, exporting a validated
stage A handoff, resolving a declared stage B analysis, and fitting or
executing the corresponding block models.
}
\usage{
simulate_study(
  n_subjects,
  n_features = c(prot = 800, lip = 400, met = 600),
  design = c("paired", "two_group", "continuous", "timeseries"),
  effect_size,
  prop_affected,
  missingness,
  shared_factor_strength
)

triomics_export_a(request, output, resume = FALSE, interrupt = NULL)

triomics_export_cache_key(components)

triomics_export_resume_decision(candidate, expected_key)

triomics_a_handoff_limitations(request = list())

triomics_resolve_analysis(prepared_study, analysis_spec)

resolve_analysis(prepared_study, analysis_spec)

triomics_fit_block_models(resolved_analysis, statistics_spec)

fit_block_models(resolved_analysis, statistics_spec)

triomics_execute_stage_b(config_path, output)
}
\arguments{
  \item{n_subjects}{Positive integer number of synthetic subjects.}
  \item{n_features}{Named integer vector giving the number of features per block.}
  \item{design}{Synthetic study design.}
  \item{effect_size}{Finite planted effect size.}
  \item{prop_affected}{Proportion of features receiving the planted effect.}
  \item{missingness}{Proportion of synthetic measurements made missing.}
  \item{shared_factor_strength}{Finite strength of the shared latent factor.}
  \item{request}{A validated synthetic stage A export request.}
  \item{output}{Local output directory.}
  \item{resume}{Whether a complete matching export may be reused.}
  \item{interrupt}{Optional test hook for an interrupted export.}
  \item{components}{Uniquely named list used to derive a cache key.}
  \item{candidate}{Candidate cached-export metadata.}
  \item{expected_key}{Expected SHA-256 cache key.}
  \item{prepared_study}{Prepared study object satisfying the stage B contract.}
  \item{analysis_spec}{Declared analysis specification.}
  \item{resolved_analysis}{Resolved stage B design and block data.}
  \item{statistics_spec}{Declared statistical fitting specification.}
  \item{config_path}{Path to a strict triomics configuration file.}
}
\value{
The return value depends on the entry point: a synthetic study, export or
resume record, handoff limitations, resolved analysis, fitted model result, or
stage B execution summary.
}
\details{
\code{resolve_analysis} and \code{fit_block_models} are aliases for their
\code{triomics_} counterparts. These interfaces enforce the frozen analysis
contracts and do not infer undeclared scientific choices.
}
\seealso{\code{system.file("exec", "triomics", package = "triomics")}}
