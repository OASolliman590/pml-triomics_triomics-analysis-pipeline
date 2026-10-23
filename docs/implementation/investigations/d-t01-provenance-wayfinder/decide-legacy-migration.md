# Decide legacy handoff migration and compatibility

Label: `wayfinder:grilling`  
Parent: [Scientific provenance boundary for Stage D](MAP.md)  
Status: open

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
