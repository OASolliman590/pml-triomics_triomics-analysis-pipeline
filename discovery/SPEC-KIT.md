# triomics Full Specification Kit

**Status:** prepared for repository publication; not frozen and not pushed  
**Date:** 2026-09-20  
**Package:** `triomics` — PML MultiOmics Analysis Pipeline

This file is the GitHub entry point for the full specification kit. The
source documents remain separate so that evidence, decisions, tickets, and
reviews retain their provenance. No production implementation is implied by
this kit.

## 1. Project scope and vocabulary

- [Project context and domain vocabulary](../CONTEXT.md)
- [Discovery status and retained scope](README.md)
- The execution and handoff record is retained locally and excluded from the
  public repository because it contains private infrastructure and delegation
  details.

The pipeline covers quantified feature-by-sample molecular assay matrices and
metadata linked by explicit identities and experimental design. It preserves
arbitrary block counts and supported study designs. It is not a raw-data
processor and does not introduce a new statistical method.

## 2. Component specifications

| Component | Responsibility | Specification |
|---|---|---|
| A | Configuration, ingest, identity/linkage, block profiles, synthetic studies, immutable preprocessing | [A-ingest-profiles.md](spec-drafts/A-ingest-profiles.md) |
| B | Design resolution, per-block inference, clinical associations | [B-design-statistics.md](spec-drafts/B-design-statistics.md) |
| C | Concatenation baseline, mixOmics/DIABLO, MOFA2, common results, evaluation | [C-integration.md](spec-drafts/C-integration.md) |
| D | Identifier-aware annotation, target investigation, evidence, reporting | [D-annotation-report.md](spec-drafts/D-annotation-report.md) |

Extensions retained in scope:

- [Clinical molecular association](spec-drafts/clinical-molecular-association-extension.md)
- [General target investigation](spec-drafts/D-target-investigation-extension.md)
- [Three-study revision proposal](spec-drafts/three-study-revision-proposal.md)

## 3. Accepted shared contract and decisions

- [Adopted shared contract and statistical settlement](operations/contract-settlement.md)
- [ADR 0001 — analysis context and shared contracts](../docs/adr/0001-analysis-context-and-shared-contracts.md)
- [Implementation command contract](operations/implementation-command-contract.md)
- [Language and execution approval](operations/language-and-execution-approval.md)

These documents define analysis-unit identity, immutable preprocessing state,
shared schemas, multiplicity, DIABLO semantics, variance/selection/NA rules,
profiles and derived quantities, clinical/target/resource interpretation, and
CLI/milestone boundaries.

## 4. Work packets and dependency graph

The complete packet set is preserved as the 41 existing ticket identities:

- [Ticket index and predecessor graph](tickets/README.md)
- [A tickets](tickets/A-T01.md)
- [B tickets](tickets/B-T01.md)
- [C tickets](tickets/C-01.md)
- [D tickets](tickets/D-T01.md)

The ticket index is authoritative for the full list. Tickets are executed only
after the Sol kit is frozen, their predecessor gates are satisfied, and a
separate Luna task is dispatched for each packet.

## 5. Evidence, reviews, and validation gates

- [Decision frontier](operations/decision-frontier.md)
- [Cross-component review](reviews/cross-component-review.md)
- [A–D specification reviews and re-reviews](reviews/)
- [AIU/runtime readiness](operations/aiu-hpc-readiness.md)
- [Task authorization](operations/task-authorization.md)

Historical reviews and proposals are evidence, not automatic approval. The
latest accepted settlement and explicit approval records take precedence.

## 6. Publication checklist

- [ ] Sol reconciliation completed and reviewed.
- [ ] Astra spec audit completed.
- [ ] Kit version/date and frozen packet table recorded.
- [ ] Dependency graph, parallel groups, risky pile, and blockers recorded.
- [ ] No credentials, patient data, source workbook contents, or private
      connection details included.
- [ ] Exact GitHub repository and visibility confirmed.
- [ ] Git repository initialized or existing checkout selected.
- [ ] `AGENTS.md`, `CONTEXT.md`, `docs/`, and `discovery/` included.
- [ ] GitHub push performed and commit/tag recorded.

## 7. Current publication blocker

This workspace is not currently a Git checkout, and the exact private GitHub
repository is not recorded. Publication must wait for the repository URL or an
explicit instruction to create a new private repository. Do not guess a
destination.
