# Decide legacy handoff migration and compatibility

Label: `wayfinder:grilling`  
Parent: [Scientific provenance boundary for Stage D](MAP.md)  
Status: closed
Assignee: `/root`

## Question

Can the accepted B/C artifacts be upgraded deterministically from retained
producer evidence, or must D fail closed until B/C are rerun under the new
provenance contract? Decide the compatibility boundary without inventing
missing historical identities or refitting scientific models silently.

## Adopted constraint

Omar decided that legacy evidence is rejected unless its complete required
lineage can be proved from immutable producer artifacts. This ticket now needs
to define the allowed proof and deterministic migration/regeneration route; it
must not infer missing provenance from a consumer-recomputed hash or a passing
historical result.

## Resolution

Legacy evidence has no grandfathering path. It is eligible only when the B or C
producer can create a complete native profile or a deterministic migration record
from immutable retained producer artifacts covering every required scientific
entity. Migration may validate, inventory, canonicalize, and hash existing
objects; it cannot refit, recompute, fill gaps from current data, or change any
numerical payload byte.

If one required proof class is absent or conflicting, migration fails closed and
D rejects the handoff. Recovery is producer regeneration from accepted upstream
inputs under the unchanged method, configuration, family, threshold, and seed,
followed by focused B-T07 or C-06 handoff requalification. Exact rules are in
[M7-002 — Stage Handoff Provenance and atomic generation publication](../../change-records/M7-002-stage-handoff-provenance-amendment.md).
