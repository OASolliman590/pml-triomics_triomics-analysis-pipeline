# Select the Stage D provenance domain model

Label: `wayfinder:grilling`  
Parent: [Scientific provenance boundary for Stage D](MAP.md)  
Status: closed
Assignee: `/root`

## Question

Which canonical objects and terms will triomics adopt, which actor owns each
object, and which exact identities must cross the B-to-C-to-D boundary? The
decision must close FeatureRegistry drift, paired/repeated metadata drift,
DEqMS identity disagreement, untyped recursive hash discovery, and lifecycle
mutation before output validation.

## Research available

- [Stage-handoff scientific provenance research](../2026-10-23-stage-handoff-scientific-provenance-research.md)
- [Canonical digests and fail-safe artifact publication](../2026-10-23-canonical-digests-safe-publication-research.md)

This is the current Wayfinder frontier. It requires an explicit human decision
on the threat model, durability promise, identity granularity, canonical JSON
implementation, and registry layout before the legacy and spec-amendment
tickets can proceed.

## Resolution

Omar adopted the domain model on 2026-10-23:

- `Stage Handoff Provenance` is a closed, versioned triomics JSON profile owned
  by the B or C producer. D validates it and never reconstructs producer-specific
  scientific identity.
- Its `Scientific Entity Descriptor` members bind every consumed source,
  normalized metadata, complete FeatureRegistry, AnalysisUnitMap, exact
  ResolvedAnalysis/design, represented molecular matrix, method-specific state,
  producer software/configuration, predecessor artifact, and produced handoff.
- A `Lineage-Complete Handoff` is the only handoff eligible for D. Typed schema
  paths—not recursive key searches—select provenance fields, so user values such
  as a contrast ID named `model_hash` remain data.
- Stage D prepares one complete immutable `Output Generation`. A small
  `Current Generation Pointer` selects it only after full validation and is the
  final atomic replacement.
- One `Single Pipeline Writer` owns publication. The promise is
  `Process-Failure Atomicity`, including failure before pointer replacement and
  static or accidental symlink hazards. Hostile concurrent mutation, operating-
  system/power failure, and cross-filesystem durability are not promised.
- RFC 8785 defines canonical JSON; full SHA-256 identifies canonical records and
  exact artifact bytes. The amendment must require conformance vectors but may
  select the audited implementation during bounded implementation planning.

The canonical language is recorded in [GLOSSARY.md](../../../../GLOSSARY.md),
and the hard-to-reverse boundary decision is recorded in
[ADR 0003](../../../adr/0003-stage-handoff-provenance-and-generation-publication.md).
This resolution is a planning decision, not a frozen-spec amendment or product
implementation authorization.
