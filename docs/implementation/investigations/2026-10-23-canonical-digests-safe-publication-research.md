# Canonical digests and fail-safe artifact publication

Date: 2026-10-23
Question: Which standards-based concepts should triomics use to bind Stage D to
accepted B/C evidence and publish D artifacts without damaging the current run?

## Decision in brief

Use four controls together:

1. Define one closed, versioned **Provenance Identity Record**. The producer
   creates it from the exact entities used by an analysis. Serialize this record
   with RFC 8785 JSON Canonicalization Scheme (JCS), then calculate its SHA-256
   digest. Identifiers such as a contrast named `model_hash` stay in values at
   schema-defined paths. They are never interpreted by a recursive key search.
2. Make the B/C producer authoritative for method-specific scientific identity.
   The D consumer verifies producer-issued content descriptors and identity
   digests. It does not reconstruct a B model identity from a different view of
   the design. A changed FeatureRegistry, AnalysisUnitMap, visit assignment,
   time value, source, or representation is a different input entity and must
   fail the lineage check.
3. Use a complete manifest of typed content descriptors. Each descriptor has a
   fixed role, media type or artifact type, digest algorithm, digest value, and
   byte size. The manifest digest identifies an immutable run generation.
4. Publish one complete generation, then replace the current registry reference
   as the final commit operation. Do not move or archive the current registry
   before all candidate files, schemas, paths, and hashes pass. Reject symbolic
   links in the protected output tree. Use directory-relative, no-follow system
   operations if hostile concurrent path changes are in scope.

This is a standards-informed triomics contract. It does **not** make triomics
BagIt-, OCI-, SLSA-, PROV-, or RO-Crate-compliant. The project should borrow the
specific invariants below and keep its current local R scope.

## Why the controls are separate

A checksum answers whether specified bytes changed. It does not define what the
bytes mean, who had authority to construct them, or when a set of files becomes
the current accepted run. RFC 6920 makes this limit explicit: a hash name binds
an algorithm and digest to an object, but another specification must define the
hash input. A hash provides an integrity binding, not authenticity by itself
([RFC 6920, sections 2 and 10](https://www.rfc-editor.org/rfc/rfc6920.html#section-2)).

The five M7-001 findings therefore cover three different failures:

- FeatureRegistry, paired/repeated metadata, and DEqMS are provenance-boundary
  failures. They need producer-declared input entities and producer-owned
  scientific identity.
- A contrast ID named `model_hash` is a schema-navigation failure. It needs
  fixed typed paths, not a stronger hash function.
- Registry rollover before a later write failure is a transaction-order and
  filesystem failure. It needs prepare-then-commit publication.

## 1. Canonical structured-data digest

### Adopt RFC 8785 for the identity record

JCS defines invariant JSON serialization by restricting input to I-JSON,
serializing JSON primitives in a specified form, and sorting object properties
recursively. It prohibits duplicate property names. It preserves array order
and preserves Unicode strings without normalization
([RFC 8785, sections 3.1-3.2](https://www.rfc-editor.org/rfc/rfc8785.html#section-3.1)).
This gives triomics a citable rule for producing the same digest when only JSON
object key order changes.

JCS does not define scientific meaning. Its security section requires the
application to validate data against ecosystem conventions before it relies on
the canonicalized data
([RFC 8785, section 5](https://www.rfc-editor.org/rfc/rfc8785.html#section-5)).
Therefore triomics also needs a closed JSON Schema with required fields,
enumerated roles, and rejected unknown fields. JSON Schema 2020-12 supplies the
structural validation vocabulary and object applicators for this purpose
([JSON Schema Core](https://json-schema.org/draft/2020-12/json-schema-core),
[JSON Schema Validation](https://json-schema.org/draft/2020-12/json-schema-validation)).

The identity schema should have these rules:

- A required schema/type URI and schema version provide domain separation.
- Field names are reserved by the schema. User-controlled IDs occur only as
  string values in typed records.
- A consumer reads exact schema paths. It must not recursively search for a
  property such as `model_hash`, `sha256`, or `artifact_id`.
- An input is an array of records with fixed fields such as `role`,
  `artifact_type`, `artifact_id`, `algorithm`, `digest`, and `size`. A custom
  invariant requires the role/ID tuple to be unique.
- Every array states whether order is meaningful. If an array represents a set,
  the producer sorts it by a schema-defined tuple before JCS. JCS deliberately
  does not reorder arrays, so JCS alone cannot canonicalize set semantics.
- IDs and digests remain strings. JCS numbers use IEEE 754 double-precision
  serialization, so identifiers and exact decimal concepts must not be routed
  through a floating-point JSON number
  ([RFC 8785, section 3.1](https://www.rfc-editor.org/rfc/rfc8785.html#section-3.1)).

A minimal shape is:

```json
{
  "schema": "https://triomics.example/schema/provenance-identity/v1",
  "identity_type": "resolved-analysis",
  "producer_stage": "B",
  "study_id": "study-1",
  "analysis_id": "analysis-1",
  "method_id": "deqms-two-group",
  "inputs": [
    {
      "role": "feature_registry",
      "artifact_type": "FeatureRegistry",
      "artifact_id": "feature-registry-1",
      "media_type": "application/vnd.triomics.feature-registry.v1+json",
      "algorithm": "sha256",
      "digest": "<64 lowercase hex characters>",
      "size": 1234
    }
  ]
}
```

The literal URI above is illustrative. Its final URI and field set require a
spec decision. The important rule is that the schema/type is inside the
canonical record that is hashed.

### Use a typed content descriptor

The OCI descriptor specification is a useful first-party pattern. An OCI
descriptor combines a required media type, a digest that acts as a content
identifier, and the raw byte size; a consumer verifies retrieved content
against both digest and size
([OCI Image Specification 1.1.1, Content Descriptors](https://github.com/opencontainers/image-spec/blob/v1.1.1/descriptor.md)).
RFC 6920 likewise requires the digest algorithm to be part of a hash-based name
([RFC 6920, section 3](https://www.rfc-editor.org/rfc/rfc6920.html#section-3)).

Triomics should use fixed `algorithm` and `digest` fields, not use an arbitrary
map key as the algorithm or artifact role. It should verify artifact role,
artifact/media type, digest, and size together. This is important because the
in-toto Statement specification warns that its subjects are matched purely by
digest regardless of content type; type-sensitive consumers need an additional
policy
([in-toto Statement v1](https://github.com/in-toto/attestation/blob/main/spec/v1/statement.md)).

This design prevents the `model_hash` name collision. A contrast ID equal to
`model_hash` is only the value of a `contrast_id` field. It cannot shadow the
producer identity field at its exact schema path.

## 2. Producer-owned scientific identity

W3C PROV defines an entity as something with fixed aspects. It defines
generation as completion of a new entity and derivation as producing a new
entity from a used entity. A changed entity is not silently the old entity
([W3C PROV-DM, sections 2.1.1-2.1.2](https://www.w3.org/TR/2013/REC-prov-dm-20130430/#section-entity-activity)).
This model fits the scientific requirement: changed visit labels, time values,
feature namespaces, or analysis-unit mappings create different entities. Old B
or C evidence cannot be attributed to them.

SLSA provenance gives a useful producer/consumer pattern even though triomics is
not a software build service. The producing process records resolved dependencies
and output subjects. A verifier checks the subject digest and expected producer,
process type, and external parameters
([SLSA Provenance 1.2](https://slsa.dev/spec/v1.2/build-provenance),
[SLSA verification](https://slsa.dev/spec/v1.2/verifying-artifacts)).
The important lesson is that the producer states what it used; the consumer
checks that statement against policy. The consumer does not invent a new
producer-specific identity algorithm.

For triomics, B and C must issue descriptors for every entity that can change
the meaning of their evidence. At minimum, the accepted handoff must bind:

- each SourceManifest and its byte digest;
- SampleRegistry and FeatureRegistry semantics;
- AnalysisUnitMap, including subject, visit, time, weights, and delta signs;
- the ResolvedAnalysis or an equivalent typed projection containing the outcome,
  covariate view, ModelPlan, contrasts, representation, and method identity;
- BlockResult, FamilyLedger, CandidateNominations, IntegrationResult, and any
  native sidecars that D consumes;
- configuration, preparation context, representation, producer version, and
  other inputs already required by the frozen envelope contract.

The producer creates the canonical identity once from the exact objects it
used. D verifies the producer envelope and the referenced object bytes. If D
also checks mutable source files at invocation time, it compares their current
byte descriptors with the producer-issued SourceManifest descriptors. It must
not calculate a paired, repeated, or DEqMS model hash from a newly reconstructed
design.

This closes three reproduced defects:

- FeatureRegistry `id_namespace` drift changes the FeatureRegistry entity digest.
- Paired or repeated visit/time drift changes the AnalysisUnitMap or
  ResolvedAnalysis digest.
- DEqMS identity remains the value issued by its B producer. D does not need to
  know whether that producer synthesized a design internally.

The pipeline must fail closed if an accepted artifact lacks a required input
descriptor. “Unknown” is not treated as “unchanged.” This follows the current
frozen rule that stale or unknown schemas and hashes fail before numerics, but
it makes the required identity boundary explicit.

RO-Crate is useful for naming research data entities and their relationships.
Its 1.2 specification requires typed entities with unique `@id` values and can
identify file format and profile
([RO-Crate 1.2 data entities](https://www.researchobject.org/ro-crate/specification/1.2/data-entities),
[RO-Crate 1.2 structure](https://www.researchobject.org/ro-crate/specification/1.2/structure)).
It does not require every file to be described, and it does not specify a
canonical digest or atomic local publication. Use its entity/profile concepts
only where useful; it is not the integrity mechanism.

## 3. Complete, content-addressed manifests

BagIt requires every payload file to occur exactly once in a payload manifest,
maps each relative path to a checksum, forbids a manifest from referring outside
the payload directory, and defines a valid bag as a complete bag whose checksums
have been verified. A tag manifest can also checksum the metadata manifests
([RFC 8493, sections 2.1.3, 2.2.1, and 3](https://www.rfc-editor.org/rfc/rfc8493.html#section-2.1.3)).
These are useful completeness invariants for a triomics generation.

BagIt is not a publication transaction and is not an active-attack security
format. Its own specification says that its integrity assurance targets data
corruption, not active attacks
([RFC 8493, sections 1.1 and 5.4](https://www.rfc-editor.org/rfc/rfc8493.html#section-5.4)).
It also does not define scientific artifact types or canonical structured-data
identity. Therefore triomics should borrow the complete-manifest and
manifest-of-manifest concepts, not adopt BagIt as the Stage D contract.

Recommended structure:

- The generation manifest lists every output as one typed content descriptor.
- Each descriptor path is relative, normalized, and constrained to the
  generation directory. User-supplied IDs are not filesystem path components.
- The manifest lists required and optional members explicitly. A member with an
  unavailable scientific status remains a typed member; it is not silently
  omitted.
- The manifest is canonicalized and hashed. Its digest is the generation ID.
- The current registry references the generation manifest descriptor. It does
  not restate or reconstruct every member identity.
- A consumer first verifies the generation manifest, then each member it uses.

## 4. Safe path handling

An ordinary existence check is not a symbolic-link check. R documents that
`file.exists()` follows a symbolic link and returns the state of its target; a
broken symbolic link can therefore appear not to exist
([R `files` documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/files.html)).
POSIX `lstat()` instead returns information about the link itself
([POSIX `lstat`](https://pubs.opengroup.org/onlinepubs/007904875/functions/lstat.html)).
This distinction directly explains the dangling-leaf defect.

The cross-platform minimum for the current local pipeline is:

1. Before changing any accepted/current state, reject absolute member paths,
   `..`, empty components, and path separators in generated names.
2. Inspect the output root, protected subdirectories, and every existing path
   component without following the final symbolic link. Reject both valid and
   dangling symbolic links anywhere in the protected publication tree.
3. Create a unique staging directory inside the final generation parent, with
   exclusive creation. Do not write through legacy destination leaves.
4. Hold one exclusive pipeline writer lock for preflight, preparation, and
   commit. Recheck protected components immediately before commit.

This minimum detects static and accidental symlink hazards. It is not a complete
defence against a hostile process that changes a directory between checks and
use. POSIX introduced directory-relative `openat()` and `renameat()` operations
to avoid pathname races by anchoring operations to open directory descriptors
([POSIX `open`/`openat` rationale](https://pubs.opengroup.org/onlinepubs/9799919799/functions/open.html),
[POSIX `rename`/`renameat` rationale](https://pubs.opengroup.org/onlinepubs/9799919799/functions/rename.html)).
A strong POSIX implementation should traverse from a trusted directory file
descriptor, reject links for each component, and perform final writes and
renames relative to trusted descriptors.

Linux `openat2()` can enforce `RESOLVE_BENEATH` and
`RESOLVE_NO_SYMLINKS` across all path components. `O_NOFOLLOW` alone affects
only the final component. `openat2()` is Linux-specific and cannot be the only
implementation for a pipeline that also supports macOS or Windows
([Linux `openat2(2)`](https://man7.org/linux/man-pages/man2/openat2.2.html)).
If hostile concurrent local modification is in scope on all supported systems,
triomics needs a small native, platform-specific secure-path backend. A pure R
`lstat`-then-write sequence can support only the explicit single-writer threat
model.

## 5. Fail-safe publication and registry commit

POSIX requires directory operations such as rename to be atomic. When replacing
an existing target, observers see either the old or the new directory entry,
not a missing intermediate entry. A failed rename normally leaves the target
unaffected
([POSIX directory operations](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap04.html#tag_04_04),
[POSIX `rename`](https://pubs.opengroup.org/onlinepubs/9799919799/functions/rename.html)).
R exposes the operating system rename but warns that behavior is platform
specific, that cross-filesystem moves often fail, that code must check the
return value, and that renaming within one directory is the portable use
([R `file.rename`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/files.html)).

Use this prepare/commit protocol:

1. **Preflight:** validate configuration, accepted predecessor manifests,
   current inputs, schemas, all planned target paths, writer lock, and output
   ancestry. This phase must not move, replace, or archive the current registry.
2. **Prepare:** create `runs/.tmp-<random>` inside the same directory that will
   hold immutable generations. Write all D payloads and envelopes there. Validate
   every schema and checksum. Write and verify the complete generation manifest.
3. **Publish generation:** rename the temporary directory to a name derived from
   the verified generation-manifest digest. The source and target are in the
   same parent directory. If that immutable target already exists, verify it is
   identical or fail; never overwrite it with different content.
4. **Prepare registry:** write the new registry or current-generation pointer to
   a unique temporary file in the registry directory. It references only the
   already-published, verified generation.
5. **Commit:** atomically rename the temporary registry file over
   `registries/run.json`. This is the only operation that changes which
   generation is current, and it is last.
6. **After commit:** keep prior generations immutable. Remove or quarantine an
   orphan staging directory after a failure. Do not make cleanup part of the
   scientific success condition.

This sequence prevents premature rollover. A failure before step 5 leaves the
old registry current. A failure after step 3 can leave an unreferenced immutable
generation, but it cannot make a partial generation current. A dangling legacy
D output leaf cannot redirect a write because candidate outputs are created in
a new generation directory, and protected ancestors are checked before commit.

Atomic visibility is not the same as crash durability. POSIX notes that an
application needing post-crash durability must synchronize the new file and the
containing directory before and after rename
([POSIX rationale on directory durability](https://pubs.opengroup.org/onlinepubs/9799919799/xrat/V4_xbd_chap01.html)).
Base R does not expose a complete portable directory-`fsync` transaction. The
spec must say whether it promises protection from ordinary process errors only,
or also from operating-system/power loss. It must not claim crash durability
without a native synchronization implementation and filesystem-specific tests.

## Finding-to-control traceability

| Reproduced finding | Required control | Why it closes the finding |
|---|---|---|
| FeatureRegistry namespace can drift | Producer-issued typed descriptor for FeatureRegistry; exact digest verified by D | Changed registry semantics create a different entity and digest. |
| Paired/repeated visit or time values are omitted | Producer binds AnalysisUnitMap and ResolvedAnalysis, including consumed visit/time values | D verifies the exact used entity instead of a partial reconstruction. |
| DEqMS producer and D construct different design hashes | Producer owns method-specific identity; D verifies it and never rebuilds it | There is one identity algorithm, executed at production. |
| Contrast ID `model_hash` hides real identity | Closed schema, user IDs only in values, exact path lookup, JCS duplicate-key prohibition | Data values cannot shadow reserved identity fields. |
| Registry moves before a later D write fails | Complete immutable generation first; atomic current-registry replacement last | No later artifact write exists after the commit point. |
| Dangling or ancestor symlink redirects output | Reject links in protected ancestry; create a new generation; use descriptor-relative no-follow operations for strong mode | Writes cannot traverse an existing output leaf or escape the trusted root. |

## Acceptance implications for a later specification amendment

The amended contract should require behavioral tests, not only helper-unit
tests:

- Reordering JSON object keys does not change the identity digest.
- Reordering an explicitly unordered collection does not change the digest
  after schema normalization; reordering an ordered collection does change it.
- Duplicate keys, unknown fields, duplicate role/ID tuples, and missing required
  descriptors fail closed.
- `model_hash` is accepted as a contrast ID and cannot satisfy the reserved
  producer-identity field.
- Mutating FeatureRegistry namespace, visit, time, subject mapping, delta sign,
  or any consumed source changes the producer-bound entity and blocks D before
  publication.
- Unchanged supported DEqMS evidence passes without D rebuilding its design.
- A symlink or dangling symlink at each protected ancestor and destination class
  fails before the current registry changes.
- A forced failure at every prepare step leaves the old registry byte-identical.
- A forced failure after generation publication but before registry commit leaves
  an orphan generation only; the old generation remains current and valid.
- A successful commit makes a complete generation current in one registry
  replacement. Every referenced member exists and matches type, size, and digest.
- Tests run on each supported output filesystem. Crash-durability tests are
  required only if that stronger promise is adopted.

## Unresolved decisions

These are policy or design choices. The primary sources do not decide them:

1. **Threat model:** Is the required guarantee single-writer protection against
   accidental/static symlinks, or protection against a hostile concurrent local
   process? The latter requires a native secure-path backend on each supported
   operating system.
2. **Durability level:** Is atomic process-failure behavior sufficient, or must
   a committed run survive sudden power or operating-system loss? The latter
   requires file and directory synchronization plus filesystem-specific tests.
3. **Identity granularity:** Should the accepted handoff bind one full
   ResolvedAnalysis artifact, smaller typed projections, or both? The result
   must cover every scientifically influential field without requiring D to
   reconstruct producer logic.
4. **JCS implementation:** Which audited R or native JCS implementation will be
   supported, and how will conformance be tested against RFC 8785 vectors?
5. **Legacy transition:** Will existing B/C envelopes be migrated, remain
   readable but ineligible for Stage D, or require regeneration? A missing
   producer descriptor must never be silently treated as unchanged.
6. **Registry layout:** Whether `registries/run.json` becomes a small current
   pointer or remains a full registry document. In either case, it must be
   prepared separately and replaced last, and historical generations must stay
   immutable.

## Primary sources

- [RFC 8785: JSON Canonicalization Scheme](https://www.rfc-editor.org/rfc/rfc8785.html)
- [RFC 6920: Naming Things with Hashes](https://www.rfc-editor.org/rfc/rfc6920.html)
- [RFC 8493: BagIt File Packaging Format](https://www.rfc-editor.org/rfc/rfc8493.html)
- [OCI Image Specification 1.1.1: Content Descriptors](https://github.com/opencontainers/image-spec/blob/v1.1.1/descriptor.md)
- [W3C PROV Data Model](https://www.w3.org/TR/2013/REC-prov-dm-20130430/)
- [RO-Crate 1.2 specification](https://www.researchobject.org/ro-crate/specification/1.2/)
- [SLSA 1.2 Build Provenance](https://slsa.dev/spec/v1.2/build-provenance)
- [in-toto Statement v1](https://github.com/in-toto/attestation/blob/main/spec/v1/statement.md)
- [JSON Schema Draft 2020-12](https://json-schema.org/draft/2020-12)
- [POSIX.1-2024 `rename`/`renameat`](https://pubs.opengroup.org/onlinepubs/9799919799/functions/rename.html)
- [POSIX.1-2024 `open`/`openat`](https://pubs.opengroup.org/onlinepubs/9799919799/functions/open.html)
- [R file manipulation documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/files.html)
- [Linux `openat2(2)` manual](https://man7.org/linux/man-pages/man2/openat2.2.html)
