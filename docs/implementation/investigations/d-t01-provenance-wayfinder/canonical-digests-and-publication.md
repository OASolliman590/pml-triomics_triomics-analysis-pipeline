# Canonical digests and fail-safe artifact publication

Label: `wayfinder:research`  
Parent: [Scientific provenance boundary for Stage D](MAP.md)  
Status: closed
Assignee: `/root/publication_research`

## Question

Which primary standards specify canonical structured-data hashing,
content-addressed manifests, typed digest fields, safe path handling, and
all-or-nothing artifact publication? Decide which concepts prevent key-name
collisions, dangling-symlink writes, registry rollover before failure, and
consumer reconstruction of producer-specific identities in a local R pipeline.

## Resolution

Resolved by
[Canonical digests and fail-safe artifact publication](../2026-10-23-canonical-digests-safe-publication-research.md).
Use typed fixed schema paths, RFC 8785 canonical JSON, algorithm-tagged SHA-256
descriptors, a complete immutable generation manifest, and prepare-then-commit
publication in which the current registry is atomically replaced last. The
future domain decision must select the threat and durability promises.
