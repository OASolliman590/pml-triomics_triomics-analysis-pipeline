# Independent full A draft review

Reviewer: root orchestrator, not A's author. Date: 2026-09-05. Target: `discovery/spec-drafts/A-ingest-profiles.md`. Full text read in two sequential chunks, with a separate retrieval of the SE schema row affected by output truncation. Compare against both preserved user inputs, CONTEXT, A evidence, independent A evidence review, and C/D input needs. This is an artifact-based document review; no Git baseline or runtime check exists.

## Requirement and contract coverage

Original reviewed snapshot SHA-256: `267F687EB4EEBE5A00BAAF6F97CEA40B7456514B66B2E55114C296B0550C6EAA`. Subsequent author revisions and status updates have separate records; this review remains tied to its original snapshot.

All ten kit categories are explicitly addressed. The draft retains all profiles and the six-function contract, separates measurement quantities from namespaces, includes two distinct illustrative designs and provides bounded ticket proposals. Readiness is honestly blocked. Core scientific qualifications from A-REV01–04 are incorporated. Concrete final dependency selection, environment lock, API signatures/behavior, approved profile defaults and historical reconciliation are still missing; the kit is not complete or ready merely because these gaps have named sections.

### A-SR01 — P1 existing cross-component blocker: unique feature identity is not yet a shared contract

Target: §3.3 optional source-row discriminator and §3.4 rowData; B/C/D join boundaries. A permits multiple measured features sharing a molecular identifier if an explicit row key exists. B's draft triple and C's selected name lists do not yet explicitly guarantee that discriminator survives. An internal unique feature key must map to original molecular ID/namespace and stay the common join key; repeated molecular IDs cannot be deduplicated implicitly.

Required clarification: freeze the feature registry and key semantics jointly, distinguishing original_id from internal unique feature identity in SE rows, B tables, C selected/loadings and D nominations. Keep this as an explicit cross-component approval gate; do not repair it with concatenated strings or silently drop supported repeated identifiers.

### A-SR02 — P2: A-T08 starts a vignette before the default vignette milestone

Target: §9 A-T08 ownership of `vignettes/ingest-profiles.qmd`. The supplied default order reserves vignettes for milestone 9 after annotation/report. A can have executable smoke fixtures and reproduction documentation before then; it should not quietly move the production vignette milestone earlier.

Correction: A-T08 owns an A reproduction Markdown artifact and synthetic CLI fixture. Schedule the Quarto vignette in milestone 9, consuming the validated A artifacts. If earlier vignette implementation is desired, record it as an order-change proposal rather than an adopted dependency.

## Scientific/evidence findings

### A-SR03 — P2: class-level derived values need observation-completeness semantics

Target: §3.4 derived representation and §4.2 class concentration sums. Parent lineage, units, standards and ordering are described, but a class total from partly observed species needs an explicit policy: observed partial sum, sum after approved imputation, or missing/unestimable total. Treating NA as zero would create abundance changes from missingness; using varying parent sets changes the estimand.

Correction: require a fixed eligible parent universe per endpoint/analysis, the missing-parent aggregation policy, completeness/coverage and observed/imputed contribution metadata. Distinguish a measured partial sum from an estimate of the defined full eligible sum. Carry this policy into derived-endpoint tests and B effect labels. No universal policy/value is selected by this review.

### A-SR04 — P2: restore the synthetic generator's complete design-coverage dependency

Target: §6.1 and A-T02. The supplied generator interface examples name paired/two_group/continuous/timeseries, but B must validate independent >2 groups and repeated >2 levels as well. The draft preserves the generator parameters and mentions visit schedules, yet its ticket acceptance only explicitly exercises paired and continuous examples.

Correction: document which additional designs the generator will support directly and which will use explicitly constructed synthetic fixtures, with a fixture per resolver row. This can remain a proposed extended generator schema pending approval; the coverage cannot disappear because the default enum is narrower than B's design table.

## Review limits and disposition

Root had already independently inspected the strongest A normalization/source concerns, while an independent A evidence reviewer checked five primary-source groups. This full package review did not repeat every cited paper or install any package. Source versions and executable details are still pending. No empirical scientific validity or successful test is inferred.

Request A-SR02–04 wording/ticket revisions; retain A-SR01 as a shared-contract decision with concrete downstream alignment. A remains not implementation-ready after revisions. Frozen reviewed input hashes and re-review disposition are to be recorded in the review index.
