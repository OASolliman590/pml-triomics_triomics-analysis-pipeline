# Scientific provenance identity for a stage handoff

Label: `wayfinder:research`  
Parent: [Scientific provenance boundary for Stage D](MAP.md)  
Status: closed
Assignee: `/root/provenance_research`

## Question

Which primary scientific-workflow provenance standards support a producer-owned,
immutable identity that binds every consumed source, metadata table,
FeatureRegistry, resolved design matrix, representation, software method, and
predecessor artifact? Decide which standard concepts can be adopted without
changing triomics statistical methods, and state the minimum scientific
lineage invariants that Stage B/C must publish and Stage D must verify.

## Resolution

Resolved by
[Stage-handoff scientific provenance research](../2026-10-23-stage-handoff-scientific-provenance-research.md).
Adopt a triomics-specific, producer-owned provenance profile informed by W3C
PROV, Workflow Run RO-Crate, BagIt, RFC 8785, and RFC 6920. The profile must
bind the exact scientific entities used by B/C, and D must verify rather than
reconstruct their identity.
