# D-T01 packet implementation review

Date: 2026-10-22  
Packet: D-T01  
Reviewed base: `57be42a9078d533885c01f3015ac54464f9a2d9f`  
Initial candidate: `98bf4c54a52cb8bab7fd3824fabb88deda579a23`  
Repair 1: `c857d646eae7419a41cf31d6fa8b7e98c6f7abb3`  
Integrated commits: `d2db62d`, `88f1591`

## Initial review

The fresh read-only Sol-high review reproduced seven Important findings despite
the initial focused suite passing 86 expectations:

1. Routing dropped immutable B/C inputs before the output envelope was written.
2. Disabled/incompatible routes discarded complete B evidence.
3. A flattened nomination could change feature/statistics while retaining an
   immutable nomination ID.
4. Hash-valid B and C artifacts with different preparation or representation
   identities could be combined.
5. Existing ancestor symlinks could escape cache and output roots.
6. Repeated features across contrasts inflated measured-feature counts.
7. The D registry boundary did not consume the shared A FeatureRegistry data
   frame and incompletely guarded study identity.

Verdict: Critical 0, Important 7, Minor 0; not ready for integration.

## Repair and focused rereview

Repair 1 changed only `R/evidence_bundle.R`, `R/annotation_router.R`,
`R/resource_manifest.R`, and `tests/testthat/test-D-T01.R`. Its focused suite
passed FAIL 0, WARN 0, SKIP 0, PASS 131. The same reviewer independently ran
the repaired suite plus adversarial and actual-producer probes.

All seven findings were addressed. No new Critical, Important, or Minor finding
was reported. The focused spec and code-quality verdicts both passed, and the
candidate was declared ready for serial integration.

This review does not accept D-T01. Shared NAMESPACE, manual, stage-registry and
CLI registration plus integrated artifact read-back remain required. Evidence
is public synthetic software evidence only; no private study data or external
annotation service was used.
