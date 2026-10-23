# triomics scientific handoff language

This glossary names the scientific provenance objects that cross the triomics
stage boundaries and the publication state that selects a complete Stage-D run.

## Language

**Stage Handoff Provenance**:
The producer-owned, immutable account of the exact scientific entities and
producer activity underlying one B or C handoff.
_Avoid_: Consumer-reconstructed model hash, inferred provenance

**Scientific Entity Descriptor**:
A typed identity for one scientifically material entity, including its role,
content identity, and relationship to the producer activity.
_Avoid_: Untyped hash, recursively discovered hash

**Descriptor Scope**:
The explicit study, analysis, block, method, preparation, representation, row,
and column identity slots that place one Scientific Entity Descriptor.
_Avoid_: Implicit scope, consumer-inferred scope

**Provenance Frame**:
The Stage Handoff Provenance payload and its binding envelope, kept outside that
profile's own scientific-entity inventory to prevent circular identity.
_Avoid_: Self-describing provenance entity, self-inventory

**Lineage-Complete Handoff**:
A stage handoff whose Stage Handoff Provenance accounts for every scientifically
material input, method state, predecessor, and produced artifact.
_Avoid_: Best-effort handoff, assumed-compatible handoff

**Output Generation**:
One complete, immutable set of Stage-D artifacts that is validated as a unit and
identified by its generation manifest.
_Avoid_: Partially written run, mutable output folder

**Current Generation Pointer**:
The small registry record that selects exactly one verified Output Generation as
current without duplicating its manifest.
_Avoid_: Mutable full run registry, current output directory

**Single Pipeline Writer**:
The sole triomics process permitted to prepare and publish an Output Generation
for a run at one time.
_Avoid_: Cooperative writers, concurrent publishers

**Process-Failure Atomicity**:
The promise that an ordinary triomics process failure cannot make a partial
Output Generation current or damage the previously current generation.
_Avoid_: Crash durability, power-loss durability
