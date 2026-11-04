# M7-029 — truthful assay and species applicability

Status: proposed bounded repair; implementation requires a fresh read-only
Astra-low scope audit. User authorization: new bounded cycles to complete Stage D
while M7/G7 remain unreleased. This is not another repair on a capped old cycle.

Baseline product P `126a30fc68fd593826a3ca82cd399fff35ee90ef` passed source7353,
installed7326, all24 native commands and exact package `Status: OK`, but its initial
final Astra audit failed two P1 boundaries. Preserve that product, recordsQ
`f33b1fd764589f4645691847b9ee610b42d718cf`, all logs and the failed audit receipt.
The existing D contract (D_ANNOTATION_TARGET_REPORT.md sections4–5) already
requires exact assay, construct/species/context applicability and truthful
uncertainty. No scientific method or schema change is proposed.

## Required behavior

1. Close M7-FINAL-ASTRA-01. A supplied external record needs an explicit nonempty
   assay description/identifier and explicit nonempty assay limitations before
   its numeric endpoint can qualify as measured applicable assay evidence.
   Missing fields must remain explicit unresolved/unassessable evidence, retained
   with a reason and the supplied endpoint/value/source; never invent an assay or
   its limitations. Existing malformed supplied-value guards remain. Preserve
   exact compound/target/construct/species/context matching and mismatch reasons;
   distinguish missing assay qualification from biological identity uncertainty.
   Do not infer quality from prose or invent a new assay ranking. Fully specified
   matching Kd, Ki, functional and engagement records retain their original axes,
   endpoints, units, relation, limitations and external status. Incomplete records
   cannot be laundered through canonical comparison or discovery into measured
   binding or comparable qualified evidence. No absent value becomes measured.
2. Close M7-FINAL-ASTRA-02. Molecular target observation composition must account
   for feature taxon from the verified evidence bundle's adapted FeatureRegistry as
   well as namespace and identifier. Only an explicitly declared matching taxon may supply applicable
   molecular support/conflict or qualified C loading evidence for that request.
   Preserve the existing producer's legitimate taxon representation (canonical
   decimal identifiers may be strings); do not use lossy coercion to turn unknown,
   malformed or conflicting identities into matches. Missing/unknown taxon stays
   unassessable; mismatched taxon is explicitly inapplicable. Retain feature/source
   candidates and the declared target/feature taxa, with reasons, without treating
   those observations as support, conflict, absence or independent confirmation.
   No cross-species translation, orthology or default-from-request taxon is added.
   A qualified observation's taxon and applicability must survive the published
   JSON. Existing duplicate-identity, context, source hash and B/C dependency
   guards remain; unknown applicability must propagate to descriptive profile
   coverage/comparability rather than being silently discarded.
3. Keep all original measurement bytes and numerical expectations. Public
   synthetic positive fixtures may explicitly state their intended assay and
   taxon; retain negatives with the information genuinely absent or mismatched.
   No fabricated private assay, real-study identity or biological claim.

## Factual provenance clarification (focused scope rereview required)

`R/evidence_bundle.R:1255–1299` adapts the same-run registry with existing D
routing metadata. The taxon value/status may originate in that declared metadata,
with its `provenance_ref`, rather than in a raw B/C registry row. “FeatureRegistry”
above means this existing adapted evidence object. Preserve its actual taxon
status and recorded origin (`feature$provenance$routing_metadata$taxon` when
present); never relabel it as a taxon measured or produced by B/C. An explicit
unknown status cannot become declared merely because a plausible value is also
present. Synthetic unit fixtures may explicitly declare their intended metadata
origin; negative fixtures retain the missing/unknown state. No new source-origin
proof is invented. Existing provenance/registry validators remain unchanged;
`R/evidence_bundle.R` and `R/annotation_apply.R` stay outside the allowlist.
This clarifies the existing data path, not a new scientific method or ownership
expansion. Implementation waits for the one allowed focused scope rereview.

## Ownership and bounds

One fresh isolated Luna gpt-6-luna/max worker; no child agents. Sol alone owns
ledger, review records, integration and acceptance. Luna may make local commits
of only the allowlisted changes so native provenance uses a clean executing tree.
No amendment of P or earlier candidates. Initial attempt plus at most two targeted
repairs, including internal corrections after a post-implementation test failure.
Report each failure before repair; pre-implementation behavioral RED is separate.

Luna allowlist:
- R/target_request.R (external-record applicability only)
- R/target_assessment.R (molecular taxon and qualification propagation only)
- R/target_alternatives.R only if needed to preserve the existing canonical
  no-unresolved-comparison rule for these qualifications; no weakened claim guard
- tests/testthat/test-D-T10.R, test-D-T11.R, test-D-T12.R,
  test-D-target-shared.R and test-D-discovery-shared.R
- inst/examples/data/d-target/external-assays.json only if an existing positive
  synthetic fixture is incomplete; do not alter numeric assay values

Any other needed file stops the worker with evidence. R/cli.R, B/C producers,
provenance schema, NAMESPACE/manuals, frozen spec/hash/packets, statistical methods,
B-T04 bound0.08 and seeds1001:1400 are forbidden. No private data or push. D06/07/14
remain blocked without credit; C07 not_run and C09 unqualified/synthetic-only.

## Proof and review

Behavioral RED must reproduce both reported paths and exercise missing assay,
missing limitations, absent/unknown/malformed/mismatched/matched feature taxon,
B and C branches, and preservation of exact source identity and values. GREEN
must retain matched fully specified assay positives, existing genuine Kd/Ki and
functional endpoints, canonical coverage/attribute claim guards, unequal/equal
coverage, duplicate evidence, all unranked discovery exclusions and no winner.
Run focused D10/D11/D12 and shared target/discovery tests, then D01 regression.
Run fresh native B/C/D for D10/D11/D12; Sol reads back actual effect/loading/taxon,
external assay metadata/qualification and all predecessor/generation hashes.
Add persisted-output negatives for incomplete assays and species mismatch/unknown
using public fixtures without changing original producer measurement values.

Sol reviews the diff and verifies proofs, integrates only verified code, then
runs one uninterrupted full source suite and clean git-archive package check of
that new immutable product. Exact `Status: OK` and every warning named are
required. Refresh DOC-M7 from the new receipts. Use the one remaining focused
final Astra rereview for the two findings and the new immutable snapshot; no
acceptance unless it passes. If another bounded cycle is needed, preserve this
cycle's attempts and review history; do not silently reset a cap.
