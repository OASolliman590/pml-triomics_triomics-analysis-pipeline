# Cross-component draft seam review

Reviewed 2026-09-05. Scope: identity, schemas, preprocessing/validation boundaries, implementation order, approval/status claims and whole-pipeline report coverage across the four provisional drafts. This reviewer authored A: all observations about A are **self-consistency checks**, not an independent A review. B/C/D observations are independent of their authors. This is neither an independent full review of every package nor implementation validation. Only this review file was written; no contract or draft was edited.

## Reviewed snapshots

| Artifact | SHA-256 |
|---|---|
| `discovery/spec-drafts/A-ingest-profiles.md` | `267F687EB4EEBE5A00BAAF6F97CEA40B7456514B66B2E55114C296B0550C6EAA` |
| `discovery/spec-drafts/B-design-statistics.md` | `47A1AFF5D47AAF5C2009F93BAA73BBC4B6A567EE36DF419F0E796E65E8921564` |
| `discovery/spec-drafts/C-integration.md` | `1CE5C5C7FEB2E2E0A9EFF7ADAD71AB2EA5FE19F0D21B2D9D0CFE88DBD55A938F` |
| `discovery/spec-drafts/D-annotation-report.md` | `211FF8A0E76835FB8E6BF70427D6406B01BB194F9C75DD8C85190B388F546AE3` |

The drafts were read, with targeted rereads of B's result/model contract and D's configuration section where the initial combined output was truncated. Local SHA-256 commands ran. No R, statistical tests, renderer, database adapter or end-to-end pipeline ran. Findings below are draft integration risks, not observed software defects. Existing explicit ADR blockers are not approvals and are not treated as concealed implementation changes.

## Findings

### X-R01 — P1: one Feature identity cannot currently round-trip through all four contracts

**Exact headings:** A §3.3 “Sample and Feature identity” and §3.4 “Proposed SE/MAE output schema”; B §2 “Dependency-by-question coverage and version/namespace manifest,” Identifier handling, and §3 “Resolved analysis and result proposal”; C §3 “Protected IntegrationResult — retained fields”; D §3 “Proposed logical input tables.”

**Concrete mismatch:** A permits distinct rows sharing a molecular ID when an explicit source-row discriminator is supplied. B defines measured identity as `(block_id,id_namespace,feature_id)` and its result key has no source-row discriminator or required A internal key. C exports within-block selected Feature strings, and D requires `feature_key` plus `block_key` and `original_id`. The drafts do not state that B/C `feature_id` is A's opaque unique row key, nor define a lossless crosswalk. Using the molecular ID in B/C collapses A's allowed rows; using opaque keys without a registry mapping prevents D from receiving the correct identifier to annotate.

**Failure example:** two explicitly distinct protein measurement rows have the same UniProt accession and different source-row IDs. A admits them, B either rejects them as duplicate keys or overwrites one, and C's selected accession cannot identify which row was nominated. Derived endpoints introduce the same risk if an internal endpoint label coincides with a measured molecular ID.

**Required correction:** approve one canonical immutable `feature_key`/`block_key` registry, define whether each table/matrix dimname is an opaque key or an original molecular ID, and specify an explicit field-name crosswalk. Preserve original IDs and row discriminators in the registry. If the intended policy is to prohibit those A duplicates instead, amend A explicitly; do not resolve this by accidental downstream rejection.

**Acceptance evidence needed:** a synthetic distinct-row/same-accession example, same bare string in two namespaces, derived endpoint collision and arbitrary Block display rename must round-trip A → B/C → D without loss or ambiguous nomination. This is an A self-consistency finding combined with an independent B/C/D contract finding.

### X-R02 — P1: C cannot guarantee training-only preprocessing from its current input envelope

**Exact headings:** A §4.3 “Proposed high-level fit/apply test seam (A-D08)”; B §3 “Inputs” / “Resolved analysis and result proposal”; C §3 “Input envelope — proposed clarification, not an accepted new interface” and §7 “Environment, workflow, artifacts and reproduction.”

**Concrete gap:** A proposes fitting feature eligibility, normalization references and imputation state within training folds. B consumes A's processed data and hands C decomposed Blocks. C's envelope supplies processed/decomposed matrices, masks and provenance, while its workflow promises training-only learned transformations inside resampling. It does not require an accessible pre-filter source checkpoint or an approved preparation service with typed fit/apply state. Masks and hashes of already filtered data do not reconstruct removed Features or reverse global fitted normalization/imputation.

**Failure example:** a Feature fails an all-Sample prevalence rule but would pass using a particular training fold. If C begins with B's already filtered matrix, it cannot discover the correct training eligibility. Conversely, a Feature retained because of held-out observations can remain in training despite violating the training-only rule. Refitting scale parameters on that filtered input cannot fix either case.

**Required correction:** freeze a resampling input/service contract that reaches the necessary immutable source quantities and invokes the approved A preparation and B Design transformation in each training/apply partition. Specify which stages are fixed mathematical transformations versus learned operations and how common B/C primary analysis provenance differs from validation replicas. Do not imply that the existing processed-matrix envelope itself provides nested validation. The protected same-matrix rule must be reconciled explicitly rather than silently bypassed.

**Acceptance evidence needed:** mutate held-out values so global filtering/reference fitting would change; prove training input/state/output and training eligibility are unchanged under the approved fold-local path. The A-D08 issue is already acknowledged; this finding identifies the missing C-side input needed to close it.

### X-R03 — P1: D's final report contract does not yet cover the entire pipeline

**Exact headings:** A §8 “Software architecture, environment and reproduction”; B §10 “Reproduction guide, expected artifacts and readiness checklist”; C §1 “Problem, scope and architecture mapping,” §6 “Validation and test strategy,” and §7 “Environment, workflow, artifacts and reproduction”; D §3 “Proposed logical input tables” / “Proposed outputs,” §9 “Bounded vertical implementation tickets,” D-T08.

**Concrete gap:** A promises QC/processing summaries to D. B requires D report sections for all keyed inference, diagnostics and sensitivity. C requires baseline comparison, held-out performance, tuning/selection stability, convergence and per-seed diagnostics. D's required input tables chiefly cover candidates, source-result references, annotation evidence and resources; D-T08's final report acceptance is dominated by annotation/status/integrity/offline safety. It does not define an obligatory complete A/B/C report view or even which source artifacts D must read through approved public schemas. A pointer and digest alone do not specify a renderable complete-results contract.

**Failure example:** B produces no candidates but many valid nonsignificant estimates and imputation-sensitive findings; C finds no improvement over baseline. A candidate-centric D implementation could satisfy its current accounting/enrichment tests while omitting both complete B results and C's negative comparison. The user would receive an annotation report without the requested interpretable analysis report.

**Required correction:** add a whole-pipeline report content/coverage contract, including A sample/Feature attrition, missingness and scale decisions; B all tested/failed/excluded rows, contrasts, q-family/SE semantics and sensitivity/direction statuses; C scores/loadings/variance definitions, requested/backend eligibility, baseline comparisons, validation/tuning/seed/convergence summaries; D mappings/enrichment/resources; and run-wide provenance/limits. Define public table/figure schemas or artifact adapters rather than allowing D to inspect upstream internals.

**Acceptance evidence needed:** an end-to-end synthetic report with zero B candidates, unavailable annotation and no C improvement still displays complete A/B/C outcomes and limitations. Required content must also be checked for a positive example and both requested study Designs. Root's separate D review may overlap this issue; deduplicate resolution by one report-contract decision.

### X-R04 — P2: shared analysis-unit identity and counts remain under-specified even after representation choice

**Exact headings:** B §3 “Resolved analysis and result proposal” and §4 “Proposed ADR-B-01: representation and statistically valid repeated fit”; C §3 “Input envelope” and protected `scores` row; D §3 “Proposed logical input tables,” RunContext/SourceResultIndex.

**Concrete gap:** B says `analysis_units` maps output row/column identifiers to Samples/Subjects/contrast and defines `n_used` as “independent/resolved analysis units.” C retains `scores: samples × components` while awaiting a contrast-unit amendment. D carries `analysis_unit` in RunContext but no normative unit-map schema or per-analysis association to score rows. Choosing delta versus sample-level within matrices resolves one scientific issue, but does not alone determine the key, key scope, multiple-contrast mapping or `n_used` counting rule.

**Failure example:** sample-level within data have 2n rows from n Subjects; calling `n_used` the number of independent units suggests n, while calling it the number of resolved rows suggests 2n. Two paired contrasts from the same Subject also cannot both use a bare Subject ID as a globally unique output key without analysis scope. A valid C result could be mislabeled in D even though numeric row order matches.

**Required correction:** define `analysis_id`, `analysis_unit_id`, `unit_kind`, source Sample list, Subject if relevant and contrast/projection identifier; require every matrix/result to reference that same map. Define `n_used` exactly per approved strategy or retain separate unambiguous counts with a documented primary count. Make the protected score-row amendment explicit if selected.

**Acceptance evidence needed:** one independent, one paired sample-level and one paired contrast-unit fixture, plus multiple contrasts sharing a Subject, prove lossless and correctly labeled joins. This is a retained readiness blocker, not evidence that a proposed representation has already been adopted.

### X-R05 — P2: global implementation gates are not expressed uniformly in ticket dependencies

**Exact headings:** A §9 “Proposed bounded implementation tickets,” Global blocker G-A; B §8 “Proposed implementation tickets and blocking edges,” Global gate G; C §8 “Bounded implementation tickets — proposed, blocked”; D §9 “Bounded vertical implementation tickets.”

**Concrete mismatch:** A/B/C explicitly gate production work on all four reviewed packages; B also names a validated predecessor A milestone and C states production begins after B's validated milestone. D's global ticket status names D specification approval and technology/dependency decisions, while D-T01 names approved A/B/C identity/result contracts. Approved contracts do not imply validated predecessor implementation. Read literally as a dispatch table, D could enter production after its own draft/contracts are approved while the global all-four review or sequential predecessor gate remains unmet.

**Required correction:** reference one global gate and predecessor-milestone policy from every ticket, distinguishing evidence-only API probes, synthetic interface fixtures and production work. Carry D's conditional route reopening separately so that documenting an unavailable route does not become either permission to skip required work or a blocker on unrelated design drafting.

**Acceptance evidence needed:** review the unified ticket dependency graph for the requested A → B → C → D implementation order, with no cycles caused by requiring implemented downstream report code to validate upstream interface fixtures. Cross-component fixture contracts may be approved before their consumers are implemented; the graph must name that distinction. No actual dependency cycle is claimed from the current wording alone.

### X-R06 — P2: CLI, output-path and completion policies need one shared schema and precedence rule

**Exact headings:** A §3.2 “Proposed configuration schema,” §3.5 “Failure contract,” §8 reproduction; B §3 configuration/results status; C §3 “Proposed configuration keys,” §7 planned commands; D §5 “Proposed configuration, errors and unavailable behaviour.”

**Concrete gap:** A proposes `execution.output_dir` and a `Rscript ... --stage A --output ...` invocation; C illustrates `triomics run --config ... --output ...`; D proposes `report.output_dir` and defers CLI syntax. C has `integration.completeness_policy`, D has `annotation.required_routes`/failure policy, and B/A have their own stage error/status distinctions. Each is labeled a proposal, which avoids a false implementation claim, but there is no single cross-stage state aggregation or output-root precedence rule.

**Failure example:** C permits an unavailable requested backend to be recorded, D successfully renders a report and an optional route fails. Without an aggregate policy, a renderer-success flag could be exposed as a successful requested analysis. Conflicting `execution.output_dir`, `report.output_dir` and CLI `--output` could also split protected artifacts from their integrity manifest.

**Required correction:** approve one CLI/schema with explicit command/config precedence, subordinate report output paths and a run-level state machine separating validated input, completed requested analyses, partial/unavailable analyses, render success and whole-goal status. Keep per-feature failures and optional-route no-hits distinct. Harmonize aliases (`block_id`/`block_key`, `feature_level`/`feature_kind`/`measured_or_derived`) through a versioned crosswalk under X-R01 rather than informal adapters.

**Acceptance evidence needed:** intentionally conflicting output keys, requested-backend unavailability, required versus optional annotation outage, and a render failure after valid statistics all produce the declared deterministic aggregate status and protected output location. These are draft schema integration checks; no current CLI is alleged to fail.

## Consistent decisions and non-findings

- All four snapshots explicitly state provisional/not-ready status and distinguish proposed schemas from existing APIs. No draft claims R confirmation, dependency installation, published issues, passing R checks or completed production implementation.
- The primary same-decomposed-matrix and no-block-name-dispatch constraints are explicitly retained; scientific challenges are presented as pending decisions. The review does not approve their reversal.
- B/C explicitly distinguish Sample from Subject and contrast units at the conceptual level. X-R04 concerns the concrete key/count schema needed to make that distinction executable.
- D correctly keeps native MOFA enrichment unresolved rather than reading a native object secretly from IntegrationResult. D also keeps BioPAN quantitative/Design requirements conditional. Those are intended blockers, not new undisclosed implementation errors.
- C correctly distinguishes implementation order from a user's explicitly requested runtime backend subset. No requirement to run every backend on every dataset follows from build order.
- No whole-goal completion is justified by any draft. A safe report with unavailable resources remains useful, but does not accept a user-goal deferral.

## Review disposition and next action

**Changes required before cross-component implementation readiness:** close X-R01–03 through reviewed contract decisions, then freeze X-R04–06 in one versioned identity/config/status/report crosswalk and ticket graph. Preserve each author's explicit unknowns and the root review findings; do not resolve disagreement by silently copying one draft's field names.

Recommended review artifacts are a miniature canonical registry/unit-map example shared by all four specs, one fold-local preparation flow with explicit source reachability, one complete report view-model schema, and one global milestone/status graph. These are proposed documentation outputs, not newly authorized dependencies or production code. Independent A review remains another author's responsibility.
