# Preserve estimands through shared analysis contexts

Status: accepted design direction, 2026-09-06, under the user's explicit delegation to Opus and Sol with Astra confirmation. Language confirmation and executable validation remain separate prerequisites.

Use a single R-package design with versioned shared artifact schemas and one molecular representation per Study/Analysis/preparation context. Two-occasion change uses subject deltas; levels use a separate mean-retaining Analysis; longer repeated designs use a guarded centered/fixed-subject route with an explicit residual model. This rejects a universal sample-row or universal delta representation because those answer different questions. Analysis-unit mappings explicitly qualify the original IntegrationResult sample-row semantics.

Preserve the six BlockProfile signatures and IntegrationResult top-level fields. Fit-mode profile state emerges through reserved SE metadata and is externalized by A; apply receives immutable state through params. B owns association fitting, C owns score construction/evaluation and D owns annotation/reporting; the orchestrator schedules artifacts without B invoking C/D. The single package owns shared schemas; no separate contract package is introduced.

The [authoritative settlement](../../discovery/operations/contract-settlement.md) defines multiplicity, score/variance/selection and command consequences. [Astra arbitration](../../discovery/reviews/astra-design-arbitration.md) resolves the actual [Opus](../../discovery/reviews/opus-design-rebuttal.md) and [Sol](../../discovery/reviews/sol-design-rebuttal.md) exchange. These are design decisions, not successful scientific or environment tests.
