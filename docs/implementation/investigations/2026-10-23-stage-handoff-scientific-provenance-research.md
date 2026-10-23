# Stage-handoff scientific provenance research

Date: 2026-10-23
Decision ticket: [Scientific provenance identity for a stage handoff](d-t01-provenance-wayfinder/scientific-provenance-identity.md)
Scope: Stage B and C producer outputs consumed by Stage D. This note does not
change the frozen specification or implement product code.

## Short answer

No single reviewed standard supplies the full triomics handoff contract. The
citable solution is a small, versioned **triomics stage-handoff provenance
profile** that combines four established concepts:

1. Model every immutable input and output as a W3C PROV **Entity**, each
   producer operation as an **Activity**, the running software as a software
   **Agent**, and the versioned plan and configuration as Entities. PROV
   defines an Entity as a thing with fixed
   aspects, separates use from generation, and represents derivation from a
   used entity to a generated entity. It also supports named provenance
   bundles and provenance for collections and their members.
   ([W3C PROV-DM](https://www.w3.org/TR/2013/REC-prov-dm-20130430/))
2. Represent the concrete run like Workflow Run RO-Crate: the run action names
   every consumed `object`, every generated `result`, and the versioned
   software `instrument`. Use the more detailed Provenance Run Crate pattern
   where individual B and C producer steps and intermediate outputs must be
   visible. The current profiles are Workflow Run RO-Crate 0.6 over RO-Crate
   1.3.
   ([Process Run Crate 0.6](https://w3id.org/ro/wfrun/process/0.6),
   [Provenance Run Crate 0.6](https://w3id.org/ro/wfrun/provenance/0.6),
   [RO-Crate 1.3](https://w3id.org/ro/crate/1.3))
3. Apply the BagIt **complete and valid manifest** rule to the handoff: list
   every member exactly once, bind each file to a cryptographic checksum, and
   verify completeness and checksums before use. BagIt is useful for byte
   inventory and transfer integrity, but it treats payload bytes as opaque and
   does not describe their scientific meaning.
   ([RFC 8493, sections 2.1.3 and 3](https://www.rfc-editor.org/rfc/rfc8493.html#section-2.1.3))
4. Compute the handoff-manifest digest over a specified canonical JSON form.
   RFC 8785 exists because hashing needs an invariant representation; it uses
   I-JSON, deterministic primitive serialization, deterministic object-property
   sorting, and UTF-8. JCS does not decide scientific row order, array order,
   types, or which fields are material, so the triomics profile must decide
   those first.
   ([RFC 8785](https://www.rfc-editor.org/rfc/rfc8785.html))

This is an application profile of standard concepts, not a claim that triomics
is a conforming RO-Crate or BagIt implementation. Conformance can be claimed
only if a future implementation satisfies the selected profile's required
fields and validation rules.

## What each source contributes

| Source | Supported concept | Limit for this decision |
|---|---|---|
| W3C PROV-DM | Immutable-version entities; use, generation and derivation; accountable agents; provenance bundles | Domain-neutral. It does not select SHA-256, prescribe JSON canonicalization, or say which multi-omics objects are scientifically material. A PROV collection can have known members without asserting that no other members exist, so PROV membership alone is not a closed handoff inventory. ([PROV entities and derivation](https://www.w3.org/TR/2013/REC-prov-dm-20130430/#section-entity-activity), [PROV collections](https://www.w3.org/TR/2013/REC-prov-dm-20130430/#component6)) |
| Workflow Run RO-Crate 0.6 | Retrospective run record linking consumed objects, generated results, software instrument and version; detailed step executions and intermediates | Its mandatory metadata is intentionally small and flexible. The peer-reviewed profile paper says this is a usability choice, so triomics still needs a stricter domain profile. ([Process Run requirements](https://w3id.org/ro/wfrun/process/0.6), [Leo et al. 2024](https://doi.org/10.1371/journal.pone.0309210)) |
| BagIt 1.0 | Exhaustive payload-file inventory, file checksums, completeness validation and path-containment warning | It understands bytes, not scientific semantics. Its integrity check targets corruption and is not protection against an active attacker. ([RFC 8493](https://www.rfc-editor.org/rfc/rfc8493.html)) |
| JCS | Repeatable bytes for hashing one JSON value | It canonicalizes syntax only. A schema must still distinguish maps from arrays, define array order, define number/string meaning, and reject omitted or unknown required members. ([RFC 8785](https://www.rfc-editor.org/rfc/rfc8785.html)) |
| Named Information hashes | Standard precedent for binding a name to object bytes with an identified hash algorithm; full SHA-256 is a registered choice | A hash proves only the byte binding. Its trust is only as good as the trusted reference, and a hash does not protect confidentiality. ([RFC 6920](https://www.rfc-editor.org/rfc/rfc6920.html)) |

CWLProv is useful precedent, but it is not the recommended outer contract.
CWLProv combines PROV traces, Research Objects and BagIt and records nested step
executions and intermediate entities. Its own repository calls it an informal
profile, and the Workflow Run RO-Crate paper reports that CWLProv's multiple
metadata files and serializations can complicate generation and consumption.
Triomics is not a CWL workflow, so adopting CWLProv would add a language-shaped
package without closing the domain-specific identity gaps.
([CWLProv authoritative profile](https://github.com/common-workflow-language/cwlprov),
[Leo et al. 2024, background and comparison](https://doi.org/10.1371/journal.pone.0309210))

## Recommended producer-owned object

The following is an inference from the standards above and the existing
[shared data contract](../../../discovery/spec-v2/DATA_CONTRACTS.md). Call the
object a **StageHandoffProvenance** until the domain-modeling ticket selects
the final ubiquitous term.

The Stage B or C producer MUST create the object when it creates the accepted
handoff. Stage D MUST verify it. Stage D MUST NOT recreate a producer identity
from current inputs or from a heuristic walk through result fields.

### Root identity

The root must contain:

- a profile name and profile version;
- `study_id`, `run_id`, producing stage and producer activity ID;
- a stable handoff `artifact_id` that is separate from its digest;
- the digest algorithm, fixed as full SHA-256 for this profile;
- the SHA-256 digest of the RFC 8785 canonical form of the root manifest with
  its self-digest field excluded by an explicit rule;
- an exhaustive, typed member array; and
- the producer software identity: triomics package version, repository commit,
  method/adapter identity, effective parameters, dependent package/backend
  versions, and runtime identity where it can affect the result.

RFC 6920 supports explicit algorithm-plus-digest object names and requires the
algorithm, length and value all to match for identity. The existing triomics
contract already selects 64 lowercase hexadecimal SHA-256 over actual bytes;
the profile should preserve that representation rather than introduce a
second public identifier syntax.
([RFC 6920 sections 2, 3 and 10](https://www.rfc-editor.org/rfc/rfc6920.html),
[triomics envelope contract](../../../discovery/spec-v2/DATA_CONTRACTS.md#1-envelope-and-atomic-provenance))

### Closed member record

Each member must be a typed record, not an arbitrary key discovered by name.
Each record must contain:

- `entity_id`, `entity_type`, `scientific_role` and schema version;
- storage form and media type;
- byte SHA-256 for a file or a canonical-value SHA-256 for an embedded JSON
  value, never an unlabeled mixture of the two;
- study, analysis, preparation-context and representation identities when
  applicable;
- an exact pointer to the producer activity that used or generated it; and
- predecessor `artifact_id` plus digest for a referenced B or C artifact.

The member array is exhaustive and has defined ordering. Maps use JCS object
ordering. Arrays retain domain order unless the schema explicitly defines
them as sets and specifies a unique sort key. Unknown duplicate identities,
unknown roles, omitted required roles and extra unlisted payload files are
validation failures. This applies the closed-inventory rule of BagIt while
retaining the scientific types that BagIt deliberately does not interpret.
([RFC 8493 sections 2.1.3 and 3](https://www.rfc-editor.org/rfc/rfc8493.html#section-2.1.3),
[RFC 8785](https://www.rfc-editor.org/rfc/rfc8785.html))

### Minimum scientific lineage closure

One accepted B/C handoff must bind all of the following as direct or
transitively closed entities. “Bind” means that the manifest identifies the
entity, its scientific role, its exact digest, and the producer activity that
used or generated it.

| Required entity | Minimum identity-bearing content |
|---|---|
| Every consumed source | Complete `SourceManifest`; source role; exact source-byte digest; original/conversion relationship; relevant sheet/region/column selection; upstream step and selection history. |
| Normalized metadata | Typed normalized metadata table used for the analysis, including unit/subject/sample/visit keys, condition and covariate values, time values with units/origin, missingness, row order and column order. A digest of the source workbook alone is not sufficient. |
| FeatureRegistry | Complete ordered rows and every identity-bearing field, including `feature_key`, `original_id`, `id_namespace`, source row, quantity/scale/units, structural resolution, aliases/parents and annotation route. |
| Analysis-unit identity | Complete `AnalysisUnitMap`, including source sample IDs, subject/visit identity, weights or signs, eligibility/exclusion and representation identity. |
| Resolved design | The exact numeric design matrix with row and column IDs; outcome/covariate view; model-column map; contrast vectors; family declarations; residual assumptions; and model-plan identity. For paired and repeated analyses this includes the consumed visit and time values, units/origin, knots/boundaries where applicable, and the exact rows used. |
| Representation | Complete representation specification and exact represented molecular matrix identity; ordered unit and feature keys; preparation context; preprocessing-state references; quantity, scale and units. |
| Method state | Statistical method and effective settings; count-prior/count provenance for DEqMS; random seed and RNG kind where used; package/backend/runtime versions; method-specific state and diagnostics required to interpret the output. |
| Predecessor artifacts | Every accepted B/C envelope and payload consumed by D, including BlockResult, FamilyLedger, CandidateNominations, IntegrationResult and typed sidecars when applicable, each by artifact ID, schema, payload digest and envelope digest. |
| Produced handoff | Complete payload inventory and digests, generation activity, completion status, and root-manifest digest. |

This closure is stricter than a generic workflow-run record because the
Workflow Run RO-Crate profiles deliberately keep mandatory metadata small.
The domain profile is justified by the triomics scientific seam: the existing
contract says a `ResolvedAnalysis` includes the AnalysisUnitMap, FeatureRegistry,
representation, outcome/covariate view, ModelPlan and input hashes, while D is
an evidence consumer and must not refit upstream work.
([triomics resolved-analysis contract](../../../discovery/spec-v2/DATA_CONTRACTS.md#3-preparation-and-resolved-analysis),
[D evidence-in contract](../../../discovery/spec-v2/D_ANNOTATION_TARGET_REPORT.md#1-evidence-in-never-upstream-refitting))

## Required Stage D verification

Before any registry rollover or D payload write, Stage D must perform one
read-only preflight:

1. Validate the profile and every member schema.
2. Recompute the canonical root-manifest digest.
3. Verify the exhaustive inventory and every referenced byte or canonical
   digest.
4. Verify that every required scientific role exists exactly once where the
   role is singular and that typed identities agree across the B/C provenance
   graph.
5. Verify that each predecessor envelope and payload is the exact accepted
   entity named by the producer.
6. Verify the complete producer-declared lineage closure. Do not reconstruct
   model identity from the current configuration or from result-field names.
7. Validate all destination paths, all existing path ancestors and all final
   leaves before the first mutation. Then publish payloads, envelopes and the
   run registry as one fail-safe state transition.

Items 1–6 follow the standard use/generation graph and closed manifest
concepts. Item 7 is necessary for M7-001 but is not fully specified by a
scientific provenance standard. BagIt requires implementations to prevent
manifest paths from accessing outside the bag and recommends adversarial path
tests, but it does not define triomics registry rollover or a filesystem
transaction.
([RFC 8493 section 5.1](https://www.rfc-editor.org/rfc/rfc8493.html#section-5.1))
The separate publication-lifecycle decision must specify that state machine.

## How this closes the five M7-001 findings

The findings are recorded in the
[focused Sol rereview](../reviews/2026-10-23-m7-001-focused-sol-rereview.md).

| Finding | Contract effect |
|---|---|
| FeatureRegistry namespace drift passed | The producer binds the complete FeatureRegistry as a typed Entity. Stage D verifies its digest and identity before use. Any `id_namespace` change creates a different Entity and invalidates the handoff. |
| Paired/repeated visit and time values were omitted | The normalized metadata, AnalysisUnitMap and exact resolved design matrix are required Entities. Their digests cover visit/time values and row identities, so a change cannot keep the same accepted lineage. |
| D recreated the DEqMS identity differently from B | Only the B producer publishes the resolved design and method-state identity, including count-prior lineage. D verifies the published identity; it does not reproduce B's hash algorithm from a generic `block$design`. |
| A contrast named `model_hash` confused traversal | Members are selected by schema-defined `entity_type` and `scientific_role`, never by recursively matching a field name. A user-domain contrast ID cannot become a provenance instruction. |
| A dangling D destination symlink failed after registry rollover | The read-only preflight validates the complete destination graph before mutation. BagIt supplies path-containment precedent; the future publication-lifecycle decision must define atomic commit/rollback behavior. |

## Scientific effect

This recommendation changes provenance and publication validation only. It
does not change an estimand, analysis population, representation, design
matrix, contrast, estimator, moderation method, multiple-testing family,
threshold, B-T04 bound, seed, integration method or biological interpretation.
It makes the producer's already-resolved scientific objects explicit and
prevents a consumer from combining them with a different input state.

The Workflow Run RO-Crate paper describes prospective provenance as the plan
and retrospective provenance as what actually ran, including concrete inputs,
outputs, tools and parameter values. The proposed handoff is retrospective
provenance with a stricter multi-omics domain profile; it does not rerun or
alter the scientific Activity.
([Leo et al. 2024](https://doi.org/10.1371/journal.pone.0309210))

## Privacy and trust limits

A digest is not anonymization. RFC 6920 warns that the content of a
low-variation object can be guessed and checked against a disclosed digest.
Therefore, private normalized metadata, source references and their digests
must remain local under the repository's privacy rule; public evidence may
contain only approved non-sensitive identifiers and summaries.
([RFC 6920 section 10](https://www.rfc-editor.org/rfc/rfc6920.html#section-10))

Checksums also do not establish who is trustworthy. BagIt states that its
manifests protect against corruption, not active attacks, and RFC 6920 states
that a hash binding is only as trustworthy as the reference from which it is
obtained. Triomics therefore still needs its accepted-snapshot and Sol/Astra
authority controls; the digest prevents undetected substitution within that
trust boundary.
([RFC 8493 sections 1.1 and 5.4](https://www.rfc-editor.org/rfc/rfc8493.html),
[RFC 6920 section 10](https://www.rfc-editor.org/rfc/rfc6920.html#section-10))

## Decisions still unresolved

This research supports the lineage model, but the Wayfinder map still needs
separate decisions for:

- the exact JSON schema, profile name, version, canonical field set and
  treatment of large typed matrices;
- whether the implementation exports an actual RO-Crate/BagIt package or only
  a triomics JSON profile aligned with their concepts;
- the fail-safe publication state machine, including destination preflight,
  atomic finalize and registry rollover;
- migration of accepted legacy B/C artifacts that did not publish this full
  closure; and
- the adversarial validation matrix and the exact specification and milestone
  gates that must reopen.

The legacy decision must not infer missing lineage from a passing result or a
consumer-recomputed hash. If an invariant cannot be proven from an immutable
producer artifact, its status is unknown until a separately approved migration
or regeneration route establishes it.

## Primary references

- Moreau L, Missier P, editors. **PROV-DM: The PROV Data Model.** W3C
  Recommendation, 2013. <https://www.w3.org/TR/2013/REC-prov-dm-20130430/>
- Leo S, Crusoe MR, Rodríguez-Navas L, et al. **Recording provenance of
  workflow runs with RO-Crate.** *PLOS ONE*. 2024;19(9):e0309210.
  <https://doi.org/10.1371/journal.pone.0309210>
- Workflow Run RO-Crate Working Group. **Process Run Crate 0.6; Workflow Run
  Crate 0.6; Provenance Run Crate 0.6.** 2026.
  <https://www.researchobject.org/workflow-run-crate/profiles/>
- RO-Crate Community. **RO-Crate Metadata Specification 1.3.** Recommendation,
  2026. <https://doi.org/10.5281/zenodo.20720080>
- Kunze J, Littman J, Madden E, Scancella J, Adams C. **The BagIt File
  Packaging Format (V1.0).** RFC 8493, 2018.
  <https://www.rfc-editor.org/rfc/rfc8493.html>
- Rundgren A, Jordan B, Erdtman S. **JSON Canonicalization Scheme (JCS).** RFC
  8785, 2020. <https://www.rfc-editor.org/rfc/rfc8785.html>
- Farrell S, Kutscher D, Dannewitz C, et al. **Naming Things with Hashes.** RFC
  6920, 2013. <https://www.rfc-editor.org/rfc/rfc6920.html>
