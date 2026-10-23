# D-T01 blocker Astra audit

Date: 2026-10-23  
Candidate: `71c0a1b628c314c03112a4c2a27a760e9ab0d0a1`  
Verdict: `changes_required`

The fresh read-only Astra-low audit independently confirmed all three Important
and both Minor findings from the shared Sol review. It did not edit files,
authorize a repair, accept D-T01, or make a scientific-validation claim.

- Mutable source ingestion beside stale B/C artifacts is an Important lineage
  violation. Source-change invalidation remains mandatory even when all
  annotation routes are disabled.
- Registry ancestor-symlink escape is an Important local-root violation. Leaf
  path checks are insufficient before registry rename or write.
- Rehashed envelopes do not make conflicting semantic identities valid. Exact
  applicable artifact, preparation, and representation identities must agree.
- Key-order-sensitive structured selection comparison is a valid Minor
  interoperability defect.
- Registry mutation before config validation is a valid Minor lifecycle defect.

Astra recommends keeping D-T01 blocked because its initial attempt and two
repairs are exhausted. It also recommends one explicit, narrowly bounded
repair-cap exception because the defects are localized consumer/provenance
boundaries rather than a scientific redesign. The audit itself does not grant
that exception.

The smallest proposed product allowlist is `R/evidence_bundle.R`, Sol-owned
`R/cli.R`, `tests/testthat/test-D-T01-shared.R`, and only if unit coverage
requires it, `tests/testthat/test-D-T01.R`. No frozen specification, global
serializer, producer/B/C scientific logic, namespace, lockfile, or annotation
adapter may change. Required evidence is recorded in change proposal M7-001.
No private study data were read.
