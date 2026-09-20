# Shared contract resolution proposal

## Settlement update — 2026-09-06

[The adopted contract settlement](contract-settlement.md) and [Astra arbitration](../reviews/astra-design-arbitration.md) resolve the shared design/statistical alternatives below. DEC-02–12 and CL-ADR01–04 have bounded design dispositions; older pending labels are historical. Language confirmation, real-study/user parameters, rights, empirical tests and provisioning remain separate gates.

Status: **proposed, not adopted**. This is the root response to cross-component X-R01–06. It supplies a concrete review surface; it does not select the still unresolved scientific representation or add production interfaces without approval. Read all four component drafts with this proposal and the original protected contracts. Field conflicts must be resolved once in a shared schema before implementation.

## Three-study source extension — 2026-09-06

The [source contract](source-contract-proposal.md) extends X-R01 and the A–D dated amendments with one Study/source/selection/alias schema. It owns the new field names/enums; do not create component variants. `study_id` scopes every artifact and local identity. Named Analyses belong to one Study; batch directories imply no synthesis. Source validation/audited conversion is part of A-T02 at milestone 2. All protected decisions/milestones below remain. The [confirmation proposal](../spec-drafts/three-study-revision-proposal.md) and [Opus response](../reviews/opus-review-response.md) distinguish recommendations from adoption.

## Clinical association extension — proposed

The [clinical contract](../spec-drafts/clinical-molecular-association-extension.md) extends the same Study/subject/sample/analysis-unit identities with typed clinical dictionaries/observations, visit-time links, objective/endpoint/family artifacts and one B-owned association service. A creates canonical metadata views; C supplies score lineage and evaluation context; D renders immutable clinical association artifacts and requests B-owned fits for new approved score endpoints without refitting prior results. Clinical metadata is not an omics Block by default. This proposal has its own review and does not adopt model/representation, protected signature or new dependency changes. Per-Block FDR remains distinct from score/clinical-endpoint family claims.

## Identity crosswalk proposed for approval

| Canonical concept | Proposed representation | Component mapping / unresolved behavior |
| --- | --- | --- |
| Study | study_id | Scopes local identities and every artifact envelope; equal code strings never create cross-study joins |
| Run | study_id + run_id + schema_version | Every artifact references this context; IDs do not imply validation |
| Analysis | analysis_id, design_id, contrast/projection or supervised target, representation_id | Multiple contrasts sharing a subject remain different analyses |
| Block | block_key + display_name | A's container label, B block_id and D block_key need one explicit map; numeric processing is invariant to display-name changes. Cross-run stable keys require registry continuity, not a hash of mutable labels. |
| Feature | feature_key → block_key, original_id, id_namespace, optional source_row_id, feature_kind, units/scale, parents | A SE row names, B result joins, C loading/selected names and D nominations use feature_key; mandatory user-visible original feature ID/namespace remain separate columns. This interprets “feature names” as unique measurement identity and needs owner approval. |
| Sample | sample_id, original assay columns and availability map | Original observation identity; never replaced by bare subject ID |
| Analysis unit | (analysis_id, analysis_unit_id), unit_kind, source_sample_ids, optional subject_id, contrast/projection_id | Maps every B fit column and C score row. Original protected score-row semantics still require representation decision. |
| Nomination | nomination_id → feature_key + source_analysis_id + branch/result/selection_rule | Two branches nominating one feature create two nomination rows, not two observed features |

No delimiter concatenation substitutes for a typed key map. Repeated original IDs are either preserved via explicit row discrimination or rejected by an approved ingest policy; downstream modules must not decide independently. Derived class endpoints keep parent-universe and missingness/aggregation policy, so they cannot masquerade as exact molecular identifiers. “Observed count”, fitted row count and independent unit count are separate quantities; B's revised proposal defines n_used as independent units with the other counts explicitly retained.

Acceptance fixtures: same accession on two source rows, same text across namespaces, a derived label colliding with a measured ID, renamed display labels, repeated subjects with several contrasts, and independently permuted block columns. Every join must be reversible through registry/unit-map artifacts.

## Preparation contexts

Exploratory/full-study analysis consumes the authoritative processed/decomposed artifact under its declared estimand. Predictive evaluation requires immutable pre-filter source values and a reviewed A/B public preparation service. C generates partitions, A fits learned quantities only on permitted training observations and applies them to held-out inputs, B creates a fold-valid common representation, and C tunes/scores. D receives only public result/diagnostic artifacts. No stage reads another's private native object to restore missing context.

The deployment information set is explicit: new independent sample, new subject with a complete pair, or future-visit prediction are distinct tasks. A service that uses both visits cannot silently score a task where one visit is unavailable. All data-dependent feature selection, including B-based selection used for prediction, belongs inside the evaluation partitions. Source fingerprints alone cannot establish absence of leakage.

## Run state and output-root proposal

Record three separate status fields: input validity, requested-analysis completion, and rendering outcome. “HTML rendered” cannot upgrade an incomplete analysis to complete. Feature-level non-estimability and scientifically empty results retain their own counts/reasons and follow an explicit completeness policy; they are not automatically technical failures.

Proposed analysis states: not_started, running, complete, partial, failed. Proposed rendering states: not_requested, not_started, running, complete, failed. A run may have complete analyses and failed rendering, or partial analyses and a successfully rendered explanatory report. Whole-goal completion is an external project assessment requiring all approved component milestones and synchronized publication, never a pipeline exit-code synonym.

The single run root is resolved by A's eventual CLI schema. Proposed precedence: an explicit CLI output override replaces execution.output_dir once and is printed in resolved configuration; otherwise execution.output_dir is required. D's report location is subordinate to that root; an independently escaping report.output_dir is rejected unless a separately approved output policy exists. The alternative CLI examples in A and C are unaccepted syntax illustrations, not two supported entry points. Freeze one actual command and exit-code table before A-T01 acceptance.

Acceptance fixtures: conflicting output roots; requested C backend unavailable; optional versus required annotation outage; no candidates/no enrichment; rendering failure after valid B outputs; interrupted artifact finalization. Assert both stage and aggregate statuses without collapsing statistical nulls into failures.

## Global implementation gate and dependency graph

G0 applies to every production ticket: all four complete specification packages independently reviewed and findings resolved; ready packages committed/pushed to the verified private repository; exact technology/dependencies/contracts and project fleet approved as required. Conditional resource decisions retain explicit reopening evidence and are not falsely declared ready. API/source research and small scientific verification probes are distinct from production milestones; probes still obey the user's technology-before-code boundary.

| Ordered milestone | Component | Required predecessor / exit |
| --- | --- | --- |
| 1 package/environment/CLI/config | A | G0; runnable CLI and actual approved checks |
| 2 simulation and ingest | A | 1 validated; truth separated from inputs |
| 3 three profiles and preparation contracts | A | 2 validated; all promised profile gates or explicit accepted disposition |
| 4 resolver and statistics | B | A validated; cross-component interface fixtures may be synthetic without implemented C/D |
| 5 baseline/common result | C | B validated |
| 6 mixOmics then MOFA | C | Baseline validated; each backend milestone reviewed before next |
| 7 annotation router | D | C validated; output CLI tables/status, no early production report code |
| 8 Quarto report | D | Annotation milestone's approved disposition; complete A/B/C/D report contract |
| 9 paired and independent two-group vignettes | Cross-cutting | Validated report; no early production vignette dependency; continuous-outcome examples do not substitute for the required two-group workflow |

Each exit also requires orchestrator-reviewed diff, actual tests/CLI smoke, independent review and resolved findings, verified commit/push/PR record and targeted Notion update. No force push or unrelated history deletion. One component's production work at a time; shared-file tickets are sequential. Downstream interface fixtures are not a requirement to implement downstream components prematurely. A report displaying an unavailable route does not accept a permanent deferral on the user's behalf.

## Whole-pipeline report

The D revision must render public artifacts covering all A QC/attrition/missingness/scale/provenance, all B tested and non-estimable results and sensitivity/SE/family/count semantics, all C requested fit statuses/score-loadings-variance meanings/tuning/stability/baseline comparisons, and D mappings/enrichment/resource conditions. Zero candidates plus no improvement plus unavailable annotation is a mandatory report fixture. Core interpretation must remain readable from the HTML alone; full machine-readable exports are separately labelled.

## What remains undecided

This proposal does not choose delta versus sample-level within rows, the repeated covariance/model, imputation/normalization defaults, the derived namespace/storage design, common DIABLO score/variance definitions, enrichment native-model evidence, institutional resource entitlement, or exact CLI/runtime. Those remain in the decision frontier. Crosswalk approval can close field-name inconsistencies only after the scientific decisions they depend on are settled.
