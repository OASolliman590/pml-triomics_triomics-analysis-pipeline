# Scientific provenance boundary for Stage D

Label: `wayfinder:map`  
Status: open

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

None.

## Not yet specified

- The exact versioned schema and digest boundary for the selected provenance
  object.
- How legacy accepted B/C artifacts can migrate without accepting ambiguous
  lineage or refitting scientific models.
- The complete validation and adversarial test matrix for the amended contract.
- The exact spec-v2 files, acceptance records, and affected gates that a future
  change record must reopen.

## Out of scope

- Changing a statistical estimator, model, multiple-testing family, threshold,
  seed, biological interpretation, or clinical claim.
- Implementing D-T02 or any later Stage-D packet before D-T01 is accepted.
- Reading, fitting, copying, or uploading Omar's private study data.
- Implementing the destination during this Wayfinder planning map.
