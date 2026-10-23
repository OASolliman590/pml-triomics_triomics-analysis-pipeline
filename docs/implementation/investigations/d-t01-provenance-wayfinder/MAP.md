# Scientific provenance boundary for Stage D

Label: `wayfinder:map`  
Status: m7_003_audit_pending

## Destination

Reach a reviewed decision for a citable, producer-owned provenance contract and
fail-safe publication lifecycle that closes all five M7-001 findings. The final
map must be sufficient to draft one explicit spec-v2 change record for user and
Astra approval before any product implementation starts.

## Notes

- Domain: scientific workflow provenance for local multi-omics analysis.
- Use `research` for external facts and primary sources only.
- Use `domain-modeling` when the canonical provenance objects and terms are
  selected; create one root `GLOSSARY.md` only after terms are resolved.
- This map is planning-only. It does not reopen M7-001, modify the frozen
  specification, change product code, or create new implementation packet IDs.
- Preserve all statistical methods, models, families, thresholds, seeds, and
  scientific claims. Preserve the B-T04 0.08 bound and seeds 1001:1400.
- Use only public synthetic fixtures. Do not read or publish private study data.
- The repository tracker remains the frozen packet cards plus the execution
  ledger. These local decision tickets are an investigation layer, not a
  competing implementation tracker.

## Decisions so far

- [Scientific provenance identity for a stage handoff](scientific-provenance-identity.md)
  is resolved by the primary-source research note: B/C producers must publish a
  closed, versioned handoff provenance profile and D must verify it without
  reconstructing producer-specific scientific identity.
- [Canonical digests and fail-safe artifact publication](canonical-digests-and-publication.md)
  is resolved by the primary-source research note: use a closed typed schema,
  RFC 8785 canonical JSON plus full SHA-256, immutable content-addressed
  generations, complete manifests, path preflight, and an atomic registry
  replacement as the final publication operation.
- W3C PROV, Workflow Run RO-Crate, BagIt, OCI descriptors, and SLSA are sources
  of selected concepts. The future contract is a triomics application profile,
  not a claim of conformance to those standards.
- This changes provenance validation and publication safety only. It does not
  change any estimator, analysis population, contrast, multiple-testing family,
  threshold, B-T04 bound, seed, integration method, or biological claim.
- [Select the Stage D provenance domain model](select-provenance-domain-model.md)
  is resolved: triomics owns a producer-issued JSON Stage Handoff Provenance;
  D verifies lineage-complete handoffs and publishes immutable generations under
  a single writer by replacing a small current-generation pointer last.
- [Decide legacy handoff migration and compatibility](decide-legacy-migration.md)
  is resolved: only immutable producer-complete lineage may migrate; missing
  proof fails closed and requires producer regeneration under unchanged science.
- [Define the spec amendment and acceptance route](authorize-spec-amendment-route.md)
  is resolved by proposed change record M7-002. Its initial audit consumed one
  documentation repair, and the focused rereview still found one Important
  closed-contract defect. M7-002 is blocked and implementation is prohibited.
- Omar authorized successor change record M7-003. It defines the exact closed
  descriptor scope, total role/type/cardinality table, and provenance-frame
  self-exclusion. It is a proposed specification delta awaiting a fresh
  Astra-low audit; product implementation remains prohibited.

## Current gate

- Fresh Astra-low audit of the immutable M7-003 candidate. A pass permits Sol to
  accept the combined amendment and authorize only its contract-foundation
  implementation phase. Until then, D-T01 and every Stage-D successor remain
  blocked.

## Out of scope

- Changing a statistical estimator, model, multiple-testing family, threshold,
  seed, biological interpretation, or clinical claim.
- Implementing D-T02 or any later Stage-D packet before D-T01 is accepted.
- Reading, fitting, copying, or uploading Omar's private study data.
- Implementing the destination during this Wayfinder planning map.
