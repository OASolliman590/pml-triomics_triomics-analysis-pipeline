# ADR 0003 — producer-owned handoff provenance and generation publication

Date: 2026-10-23. Status: accepted by Omar for the forthcoming reviewed
spec-v2 amendment.

Triomics will use its own closed JSON Stage Handoff Provenance profile rather
than claim conformance to a generic provenance or packaging standard. B and C
own the identity of the exact scientific entities they consumed and produced;
D verifies that identity and never reconstructs producer-specific model
identity. Stage-D output is prepared as a complete immutable Output Generation,
then a small Current Generation Pointer is atomically replaced as the final
publication operation under a Single Pipeline Writer.

The guarantee is Process-Failure Atomicity. It covers ordinary validation and
write failures in the sole triomics writer, including static or accidental
symlink hazards. It does not promise safety against a hostile concurrent local
process, operating-system failure, power loss, or unsupported filesystem
semantics. Those stronger guarantees would require a separate native and
filesystem-specific decision.

Legacy B/C evidence is not made eligible for D by a consumer-recomputed hash or
a previously passing result. It is eligible only when immutable producer
artifacts prove the complete required lineage; otherwise D fails closed until a
separately reviewed regeneration or deterministic migration route establishes
that provenance.
