\name{triomics-api}
\alias{simulate_study}
\alias{triomics_export_a}
\alias{triomics_export_cache_key}
\alias{triomics_export_resume_decision}
\alias{triomics_a_handoff_limitations}
\alias{triomics_resolve_analysis}
\alias{triomics_fit_block_models}
\alias{triomics_execute_stage_b}
\alias{triomics_b07_export_handoff}
\alias{triomics_b07_cli_after_stage_b}
\alias{resolve_analysis}
\alias{fit_block_models}
\alias{associate_endpoints}
\alias{triomics_associate_endpoints}
\alias{triomics_resolve_clinical_objectives}
\alias{triomics_clinical_association_export}
\alias{triomics_execute_clinical_associations}
\alias{triomics_b08_endpoint_hash}
\alias{integrate_blocks}
\alias{evaluate_pipeline}
\alias{make_consensus_state}
\alias{apply_consensus_state}
\alias{associate_longitudinal_endpoints}
\alias{triomics_associate_longitudinal_endpoints}
\alias{triomics_execute_longitudinal_associations}
\title{Triomics simulation, analysis, and integration entry points}
\description{
Public entry points for generating synthetic studies, exporting a validated
stage A handoff, resolving a declared stage B analysis, and fitting or
executing the corresponding block models.
The B-T07 export entry points create a verified Stage B handoff for Stage C and
bind it to the exact keyed matrices retained from the same Stage B execution.
The clinical-association entry points resolve and fit declared between-person
endpoint objectives, export privacy-scrubbed results, and expose reproducible
endpoint hashes.
The longitudinal entry points fit declared between-person, within-person,
baseline-to-later, or effect-modification clinical objectives when their
mode-specific requirements and eligible inputs are available.
The integration entry points fit registered concatenation adapters, apply the
frozen multiblock consensus-score convention, and run leakage-safe nested
supervised evaluation.
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

triomics_b07_export_handoff(
  output,
  config_path,
  matrices = NULL,
  smoke_runs = NULL,
  command = NULL,
  working_ref = NULL,
  cli_exit_code = NULL,
  resume = FALSE,
  qualifications = NULL,
  matrix_source = NULL
)

triomics_b07_cli_after_stage_b(args, stage_b_result, smoke_runs = NULL)

triomics_associate_endpoints(endpoint_bundle, clinical_view, objective_spec)

associate_endpoints(endpoint_bundle, clinical_view, objective_spec)

triomics_resolve_clinical_objectives(clinical_view, objective_spec)

triomics_clinical_association_export(result)

triomics_execute_clinical_associations(config_path, output)

triomics_b08_endpoint_hash(endpoint)

integrate_blocks(resolved_analysis, integration_spec)

evaluate_pipeline(study, analysis_spec, integration_spec, split_plan)

make_consensus_state(native_scores, native_loadings, training_units, params)

apply_consensus_state(native_scores, state)

triomics_associate_longitudinal_endpoints(
  endpoint_bundle,
  clinical_view,
  objective_spec
)

associate_longitudinal_endpoints(
  endpoint_bundle,
  clinical_view,
  objective_spec
)

triomics_execute_longitudinal_associations(config_path, output)
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
  \item{resume}{Whether a complete matching export or B-T07 handoff may be reused.}
  \item{interrupt}{Optional test hook for an interrupted export.}
  \item{components}{Uniquely named list used to derive a cache key.}
  \item{candidate}{Candidate cached-export metadata.}
  \item{expected_key}{Expected SHA-256 cache key.}
  \item{prepared_study}{Prepared study object satisfying the stage B contract.}
  \item{analysis_spec}{Declared analysis specification.}
  \item{resolved_analysis}{Resolved stage B design and block data.}
  \item{statistics_spec}{Declared statistical fitting specification.}
  \item{integration_spec}{Declared integration method, block, unit, scaling,
  selection, and evaluation settings.}
  \item{study}{Assembled study containing the registered sample and assay maps.}
  \item{split_plan}{Validated subject- or sample-disjoint nested split plan.}
  \item{native_scores}{Named list of block-native score matrices with stable
  unit and component identities.}
  \item{native_loadings}{Named list of block-native loading matrices used to
  freeze deterministic component orientation.}
  \item{training_units}{Declared unit identifiers used to fit consensus
  orientation and standardization.}
  \item{params}{Declared consensus required views and numerical tolerance.}
  \item{state}{Frozen consensus orientation, training transforms, weights,
  units, and required views returned by \code{make_consensus_state()}.}
  \item{config_path}{Path to a strict triomics configuration file.}
  \item{matrices}{Named block matrices retained from the same Stage B fit.}
  \item{smoke_runs}{Optional named synthetic qualification runs used by the
  B-T07 acceptance record.}
  \item{command}{Exact command recorded for reproduction.}
  \item{working_ref}{Recorded source-control reference for reproduction.}
  \item{cli_exit_code}{Recorded exit code for the completed Stage B command.}
  \item{qualifications}{Explicit unrun scientific qualifications recorded in
  the acceptance and reproduction evidence.}
  \item{matrix_source}{Declared provenance label for the handed-off matrices.}
  \item{args}{Original Stage B CLI argument vector.}
  \item{stage_b_result}{In-memory result returned by the same
  \code{triomics_execute_stage_b()} execution.}
  \item{endpoint_bundle}{Eligible endpoint values with subject, visit, time,
  unit, selection, and training-lineage metadata.}
  \item{clinical_view}{Validated clinical dictionary, observations, and sample
  links for the same study.}
  \item{objective_spec}{Declared between-person or longitudinal clinical
  objectives, fixed testing family, and model options.}
  \item{result}{Clinical-association result to export after removing private
  identifier fields.}
  \item{endpoint}{One endpoint record whose declared values and sample keys are
  hashed for lineage verification.}
}
\value{
The return value depends on the entry point: a synthetic study, export or
resume record, handoff limitations, resolved analysis, fitted model result, or
stage B execution summary, verified Stage B handoff, resolved clinical
objectives, privacy-scrubbed
clinical results, or an endpoint hash. Clinical and longitudinal association
functions return typed objective, family, audit, and result records.
\code{integrate_blocks()} returns the protected six-field
\code{IntegrationResult}; \code{evaluate_pipeline()} returns fold-level
predictions, metrics, failure dispositions, and fit-budget evidence; the
consensus functions return a frozen state or a
unit-by-component score matrix. Unsupported
inputs return a named unavailable status or a typed CLI error rather than
substituting an analysis.
}
\details{
\code{resolve_analysis} and \code{fit_block_models} are aliases for their
\code{triomics_} counterparts. These interfaces enforce the frozen analysis
contracts and do not infer undeclared scientific choices.

The simulator deliberately has no seed argument. Stage A orchestration must
set \code{options("triomics.execution_context")} to a list containing one
integer \code{seed}, one non-empty \code{study_id}, and the recorded RNG kinds
\code{Mersenne-Twister}, \code{Inversion}, and \code{Rejection}. The returned
truth record includes the supplied seed, while the study record includes the
study identifier, so the synthetic study can be replayed.
}
\seealso{\code{system.file("exec", "triomics", package = "triomics")}}
\examples{
old_context <- getOption("triomics.execution_context")
options(triomics.execution_context = list(
  seed = 7L,
  study_id = "documented-synthetic-example",
  rng_kind = "Mersenne-Twister",
  normal_kind = "Inversion",
  sample_kind = "Rejection"
))
simulated <- simulate_study(
  n_subjects = 4L,
  n_features = c(proteome = 2L, metabolome = 2L),
  design = "two_group",
  effect_size = 1,
  prop_affected = 0.5,
  missingness = 0,
  shared_factor_strength = 0.2
)
simulated$truth$seed
options(triomics.execution_context = old_context)
}
