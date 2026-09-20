# Independent full D draft review

Reviewer: root orchestrator, not the D draft author. Date: 2026-09-05. Target `discovery/spec-drafts/D-annotation-report.md`, SHA-256 `211FF8A0E76835FB8E6BF70427D6406B01BB194F9C75DD8C85190B388F546AE3`. Read the full document, separately retrieving the middle section when tool output was truncated. Comparison: both preserved user inputs, D evidence and its independent review, root CONTEXT and decision frontier. This is document review, not a Git diff/CLI code-review invocation; no repository baseline exists. No executable validation was performed.

## Requirement and contract coverage

The draft addresses all ten requested categories and correctly remains not adopted/not implementation-ready. Namespace routing, both candidate origins, ambiguity, conditional route reopening and no invented validation are explicit. Its ten-category structural coverage must not be conflated with full specification completion: it lacks approved inputs, locked environment, executable verification and resolved methodological gates.

### D-SR01 — P1: report coverage is narrower than the requested pipeline report

Target: sections 1, 3 (RunContext/SourceResultIndex/ReportBundle) and D-T08. The draft defines a detailed annotation/accounting report, but it does not explicitly require A QC/filtering/normalization/imputation provenance; B full design/model/contrast, effect/SE/p/q/n/directional-consistency tables and sensitivity disagreements; and C baseline comparison, variance/score meaning, tuning/folds, selection/seed stability and convergence outputs. SourceResultIndex alone is not a contract for the whole report.

Correction: add a full-pipeline report content contract referencing immutable A/B/C stage artifacts, with missing-section status, explicit parameter/default provenance and all relevant uncertainty. Do not require report code to reach into upstream internal native objects. A report without annotation must still explain the upstream analyses and their limitations.

### D-SR02 — P1: proposed ticket order builds report code before the authorized report milestone

Target: D-T01 ownership of report_model.R/annotation.qmd and minimal local report deliverable. The supplied default build order is annotation router (7), then Quarto report (8), with no building ahead. Generic vertical-slice skill advice does not supersede that order.

Correction: make D-T01 an annotation CLI/status-table slice with a runnable CLI and machine-readable artifacts; reserve Quarto/report rendering code for milestone 8/D-T08. The draft can specify the report early; production implementation cannot start it early without an explicitly approved order change. Make milestone mapping explicit for every D ticket, including conditional specialized subroutes.

### D-SR03 — P2: self-contained HTML versus full sidecar results needs an explicit deliverable definition

Target: report.self_contained, report.max_table_rows and section 6. A locally linked sidecar is not embedded in a single HTML file. The draft promises readable offline HTML and full sidecars but does not distinguish whether opening the HTML alone provides every core report section.

Correction: specify a standalone HTML with embedded narrative, diagnostics, core tables and all essential interpretation, and separately named machine-readable full-result exports if display limits are used. State display limits and export dependencies; verify the HTML in isolation as well as the complete bundle. No remote JavaScript/fonts/data may be required for core content.

## Scientific and evidence findings

### D-SR04 — P2: summary composition is not a structure-candidate identification claim

Target: worked example D-E1 describing a sum composition with two possible structural children. The fixture can illustrate one-to-many mapping, but a database's listed children are not a complete set of plausible isomers or evidence those specific children were measured.

Correction: label them as artificial resource-supported candidate mappings, explicitly nonexhaustive and not resolved identities. Separate exact-identity mapping from class/ontology associations. The main schema already supports structural resolution and evidence; the example must use those semantics precisely.

Confirmed strengths: native MOFA enrichment remains unavailable under the current common object; BioPAN is not treated as an arbitrary candidate lookup or paired-valid analysis; GO is kept distinct from reactions; background/rank and multiplicity decisions remain open; restricted-resource conditions are not bypassed. The licensed route table relies on bounded documentary evidence and correctly does not assert institutional entitlement or observed API coverage. Root additionally opened the current Reactome licence and KEGG API page. LION FAQ retrieval failed in this root pass; its earlier researcher inspection was not independently reconfirmed here. Reactome's returned page also displays a development-site banner despite the canonical URL, so canonical production applicability needs one further check before terms are relied on operationally.

## Disposition

Request revisions D-SR01–04 to the draft. Even after revision, D remains conditional and not implementation-ready until contract decisions, approvals, package/resource checks, environment and publication gates are satisfied. Neither this review nor an author response certifies runtime behavior or completes the overall goal.
