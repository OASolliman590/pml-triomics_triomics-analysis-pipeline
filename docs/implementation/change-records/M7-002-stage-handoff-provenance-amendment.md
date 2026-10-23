# M7-002 — Stage Handoff Provenance and atomic generation publication

- Date: 2026-10-23
- Status: **blocked; focused Astra-low rereview returned `changes_required`**
- User authority: Omar adopted the domain model and instructed Sol to define the
  legacy proof/regeneration rules, draft the amendment, and dispatch the audit.
- Frozen base specification hash:
  `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`
- Trigger: the five Important findings in
  `docs/implementation/reviews/2026-10-23-m7-001-focused-sol-rereview.md`.
- Research:
  `docs/implementation/investigations/2026-10-23-stage-handoff-scientific-provenance-research.md`
  and
  `docs/implementation/investigations/2026-10-23-canonical-digests-safe-publication-research.md`.
- Domain decision: `docs/adr/0003-stage-handoff-provenance-and-generation-publication.md`.

## Scope and authority

If independently accepted, this record is a normative amendment to
`discovery/spec-v2/DATA_CONTRACTS.md` sections 1, 3, 4, 5, and 6;
`D_ANNOTATION_TARGET_REPORT.md` section 1; `VALIDATION.md` V21; and the public
handoff/acceptance seams of B-T07, C-06, and D-T01. The 41 packet identities,
their dependency graph, all statistical methods, estimands, thresholds, seeds,
families, and scientific claims remain unchanged.

The amendment adds a producer-owned provenance boundary and a publication state
machine. It does not make triomics conformant with W3C PROV, RO-Crate, BagIt,
OCI, SLSA, or another external profile. Those sources justify selected concepts;
the contract below is triomics-specific.

Normative terms are defined in the root `GLOSSARY.md`. `MUST`, `MUST NOT`,
`SHOULD`, and `MAY` have their ordinary specification meanings in this record.

## 1. Stage Handoff Provenance profile

B-T07 and C-06 MUST each emit one immutable `StageHandoffProvenance` JSON
artifact for every handoff offered to a downstream stage. The producing B or C
module owns the record. D MUST validate it and MUST NOT derive, repair, or
reconstruct producer-specific model identity from current input files, result
field names, a recursive key walk, or private model state.

The JSON document is a closed object with these root members and no unknown
members:

- `profile`: literal `triomics.stage-handoff-provenance`;
- `profile_version`: literal `1.0`;
- `producer_stage`: `B` or `C`;
- `study_id`, `run_id`, `handoff_artifact_id`, and `producer_activity_id`;
- `identity`: the closed identity object described below;
- `identity_sha256`: full lowercase SHA-256 of the RFC 8785 canonical UTF-8
  serialization of `identity`;
- `producer`: package version, repository commit, module/method identifiers,
  effective configuration digest, and runtime/backend versions that can affect
  interpretation;
- `limitations`: explicit qualification strings; and
- `migration`: null for a native profile or a closed migration record defined in
  section 4.

The containing ordinary triomics artifact envelope continues to bind the exact
payload-file bytes through `payload_sha256`. `identity_sha256` binds structured
scientific identity. A semantic identity digest and a file-byte digest MUST have
different field names and MUST NOT be substituted for one another.

### 1.1 Canonical identity

`identity` is a closed object containing `schema_uri`, `schema_version`,
`handoff_role`, exhaustive `activities` and `entities` arrays, and nothing else.
It uses I-JSON values.
Floating-point values, NaN, infinity, negative zero, and integers outside the
exact IEEE-754 range MUST NOT appear in the canonical identity. Exact scientific
numbers belong in typed immutable artifacts and are bound by descriptors; exact
integer-like identifiers and quantities MAY be encoded as decimal strings.

RFC 8785 defines serialization. Objects are key-order invariant. Arrays retain
order unless the schema declares them set-like; the `entities` set MUST be
sorted uniquely by `(scientific_role, entity_type, entity_id)` before
canonicalization. Duplicate keys, duplicate sort tuples, missing required roles,
unknown roles, and unknown fields fail closed. The implementation MUST pass the
RFC 8785 Appendix B and object-sorting vectors used by the project, plus explicit
Unicode, escape, duplicate-key, array-order, and invalid-number probes. A
home-grown approximation or ordinary JSON serialization is not evidence of
conformance.

### 1.2 Scientific Entity Descriptor

Every `entities` member is a closed `ScientificEntityDescriptor` containing:

- `entity_id`, `entity_type`, `scientific_role`, and entity `schema_version`;
- `storage_form`, exactly `file`, `enveloped_artifact`, or `embedded_json`, and
  `media_type`;
- `digest_algorithm`, fixed to `sha256`;
- `digest_kind`, exactly `bytes` or `rfc8785`;
- `digest`, full 64-character lowercase hexadecimal SHA-256;
- `size_bytes` as an exact non-negative integer when `digest_kind=bytes`;
- `canonical_schema_uri` when `digest_kind=rfc8785`;
- `generated_by_activity_id`, null only for an external source entity;
- sorted unique `derived_from_entity_ids` and optional `member_of_entity_id`;
- `artifact_id` and `artifact_payload_sha256` when the entity is an enveloped
  artifact; and
- applicable `study_id`, `analysis_id`, `preparation_context_id`,
  `representation_id`, and ordered row/column identity references.

User-controlled identifiers occur only as values at schema-defined paths. A
contrast or entity ID equal to `model_hash`, `sha256`, or another reserved field
name remains data and cannot satisfy a provenance requirement.

Conditional fields are closed. `file` and `enveloped_artifact` require
`digest_kind=bytes` and `size_bytes`; `embedded_json` requires
`digest_kind=rfc8785` and `canonical_schema_uri` and forbids `size_bytes`.
`enveloped_artifact` requires `artifact_id` and `artifact_payload_sha256`; the
other storage forms forbid them. An applicable analysis, preparation,
representation, block, row, or column identity cannot be null merely because a
consumer does not need it.

### 1.3 Activities, graph resolution, and role registry

Every `activities` member is closed and contains `activity_id`, `stage`,
`activity_type`, `method_id`, sorted unique `used_entity_ids`, and sorted unique
`generated_entity_ids`. Allowed activity types are `analysis`, `integration`,
`handoff_export`, and `deterministic_legacy_packaging`. Every entity reference
MUST resolve to exactly one entity in the same identity object. Every generated
entity has exactly one matching `generated_by_activity_id`; external source
entities have null `generated_by_activity_id` and MUST be used by at least one
activity. Every `derived_from_entity_ids` and `member_of_entity_id` reference
resolves in the same graph. Derivation and membership graphs are acyclic. The
root `producer_activity_id` is an `activities.activity_id` and generates the
handoff artifacts.

`scientific_role` is one of this closed registry:

- `source_manifest`, `normalized_metadata`, `feature_registry`,
  `analysis_unit_map`, `resolved_analysis`, `represented_matrix`,
  `preprocessing_state`, `method_state`, `predecessor_envelope`,
  `predecessor_payload`, `block_results`, `family_ledger`,
  `candidate_nominations`, `integration_result`, `integration_state`,
  `evaluation_evidence`, `clinical_evidence`, `native_sidecar`, or
  `handoff_payload`.

Cardinality is evaluated after grouping by the descriptor's applicable scope
IDs. Both producer profiles require one or more `source_manifest` overall;
exactly one `normalized_metadata`, `analysis_unit_map`, and `resolved_analysis`
per `analysis_id`; exactly one `feature_registry` per `block_id`; exactly one
`represented_matrix` and one or more `preprocessing_state` per
`(analysis_id, block_id, representation_id)`; exactly one `method_state` per
`(analysis_id, method_id)`; and one or more matching predecessor envelope/payload
pairs and handoff payloads.

The B profile additionally requires exactly one `block_results`, one
`family_ledger`, and one `candidate_nominations` per exported B handoff.
Applicable clinical and native sidecars are required as descriptors; when a
declared route is unavailable, its typed unavailable payload is the descriptor
target rather than omission.

The C profile additionally requires one or more `integration_result` and
matching `integration_state` descriptors, exactly one `candidate_nominations`,
and exactly one `evaluation_evidence` descriptor per exported C handoff.
Applicable clinical and native sidecars follow the same typed-unavailable rule.
Extra roles, missing roles, role/type disagreement, or two descriptors where the
registry says exactly one are `D_PROVENANCE_INVALID`.

## 2. Required lineage closure

A `LineageCompleteHandoff` MUST account directly or transitively for every
scientifically material entity used to produce the handoff. At minimum:

1. every consumed `SourceManifest`, including exact selected source bytes and
   conversion/selection lineage;
2. the exact typed normalized metadata used by the analysis, with unit, sample,
   subject, visit, condition, covariate, time, missingness, row, and column
   identity;
3. the complete `FeatureRegistry`, including `id_namespace`, units/scale,
   structural resolution, aliases/parents, source row, and annotation route;
4. the complete `AnalysisUnitMap`, including eligibility/exclusion, source
   samples, subject/visit identity, weights or delta signs, and representation;
5. the exact `ResolvedAnalysis` and numeric design identity: ordered rows and
   columns, outcome/covariate view, model-column map, contrasts, family,
   residual assumptions, and all consumed visit/time values, units, origin,
   knots, or boundaries where applicable;
6. the exact represented molecular matrix and representation specification,
   including ordered unit and feature keys and preparation state;
7. method-specific state and settings, including DEqMS count/prior provenance,
   RNG state where used, and package/backend versions;
8. every accepted predecessor payload and envelope consumed downstream,
   including BlockResult, FamilyLedger, CandidateNominations,
   IntegrationResult, evaluation/clinical sidecars, and native typed sidecars;
9. the produced handoff payloads, envelopes, and complete member inventory.

Changing any required entity creates a different identity. An unknown or absent
descriptor is not equivalent to an unchanged entity. D MUST reject a handoff
before route computation or output mutation when closure cannot be proven.

## 3. D verification boundary

D performs one read-only preflight before it creates, moves, replaces, archives,
or deletes any current D artifact or registry entry. The preflight MUST:

1. validate the profile and all closed schemas;
2. verify RFC 8785 canonical identity bytes and `identity_sha256`;
3. verify every exact artifact byte digest, canonical-value digest, size, type,
   role, and required-role cardinality;
4. verify that the B and C provenance graphs agree on study, run, analysis,
   preparation, representation, unit, feature, source, and predecessor identity;
5. verify the exact accepted predecessor envelopes and payloads;
6. reject current mutable source, metadata, registry, design, or representation
   state that conflicts with producer-declared lineage; and
7. complete the destination/path and single-writer preflight in section 5.

D may compare current bytes with producer descriptors when current files are
part of the request. It MUST NOT use current files to manufacture a missing
producer descriptor. D does not refit or recompute B/C scientific results.

Required stable failure classes are:

- `D_PROVENANCE_REQUIRED` for a missing profile;
- `D_PROVENANCE_INVALID` for schema, canonicalization, digest, type, inventory,
  or cardinality failure;
- `D_LINEAGE_MISMATCH` for conflicting declared entities;
- `D_LEGACY_INELIGIBLE` for an unprovable legacy handoff;
- `D_OUTPUT_PATH_UNSAFE` for an unsafe publication path; and
- `D_PUBLICATION_FAILED` for a failed prepare/publish operation that leaves the
  prior generation current; and
- `D_PUBLICATION_STATUS_UNKNOWN` when pointer replacement may have committed but
  acknowledgement/read-back cannot establish which complete generation is
  current.

## 4. Legacy proof, migration, and regeneration

There is no grandfathering. A prior M4/M6 acceptance, passing numerical test,
matching consumer-recomputed hash, or matching title/ID does not establish the
new provenance contract.

A legacy B or C handoff is eligible for deterministic migration only when the
producer module can prove every section-2 entity from immutable retained
producer artifacts. The migration input set MUST include exact accepted payload
and envelope bytes, their acceptance commit/receipt, all required registries and
unit/design/representation artifacts, and all method-specific state. Every input
must have a verifiable byte or canonical digest. Mutable current sources and
consumer-created summaries are not proof.

Migration is a new producer-side provenance activity. It MAY validate, sort,
canonicalize, inventory, and hash retained entities. It MUST NOT refit a model,
recompute an effect/p value/q value/score/loading, change a matrix, fill an
absent field from current data, or reinterpret an old result. The migrated
record contains `migration` with:

- literal `mode = deterministic_legacy_packaging`;
- source artifact IDs and byte digests;
- source acceptance commit and receipt references;
- migration software commit/version;
- proof status for every required scientific role; and
- a statement that numerical payload bytes are unchanged.

If any required entity or proof is absent, conflicting, mutable-only, or cannot
be tied to the original producer activity, migration MUST fail with
`D_LEGACY_INELIGIBLE`; no partial profile is emitted.

The allowed recovery is producer regeneration: run the affected B-T07 or C-06
handoff/export seam from the retained accepted upstream inputs under the same
scientific configuration, method, threshold, family, seed, and package/backend
qualification, now emitting native Stage Handoff Provenance. Regeneration is
not migration and creates new artifact identities. Its numerical outputs MUST
match the accepted outputs exactly where deterministic or within the already
accepted method-specific tolerance. Any unexplained difference reopens the
affected scientific acceptance and blocks D.

## 5. Output Generation publication

Only one `SinglePipelineWriter` may publish a D run at a time. The writer holds
one exclusive run lock from path preflight through pointer commit. This contract
targets ordinary process failures and static/accidental path hazards. It does
not claim protection from a hostile concurrent process, operating-system or
power failure, or unsupported filesystem semantics.

The writer MUST use this state machine:

1. **Preflight:** validate request, dependency provenance, lock, all planned
   relative paths, output-root ancestry, protected directories, and every
   existing final component without following symbolic links. Reject absolute
   paths, empty/`.`/`..` components, generated separators, symlinks, and dangling
   symlinks before mutating current state.
2. **Prepare:** create an exclusive temporary directory inside the final
   generation parent. Write all payloads/envelopes there. Validate every schema,
   size, digest, cross-reference, and required status.
3. **Seal:** write a closed complete generation manifest. Its fields are
   `profile` (literal `triomics.output-generation-manifest`), `profile_version`
   (`1.0`), `study_id`, `run_id`, the validated B/C provenance artifact
   descriptors, `producer` identity, `status`, `limitations`, and `members`.
   Each member is a closed record containing normalized relative `path`,
   `scientific_role`, `artifact_type`, `media_type`, `status`, `size_bytes`,
   `digest_algorithm=sha256`, `digest`, and optional paired
   `envelope_for_artifact_id`. Members are sorted uniquely by path. The manifest
   lists every generation file except the manifest itself and excludes the
   current pointer, lock, and temporary files. The Stage Handoff Provenance
   documents are inputs named at the manifest root, not undisclosed members.
   Canonicalize and hash the complete manifest file; its full byte SHA-256 is the
   immutable `generation_id`. Neither `generation_id` nor a manifest self-
   descriptor appears inside the manifest, so no circular digest exists.
4. **Publish generation:** rename the temporary directory to the generation ID
   within the same parent. An existing generation ID is accepted only after
   complete byte-for-byte verification; conflicting content fails.
5. **Prepare pointer:** write a small closed `CurrentGenerationPointer` in the
   registry directory with `schema_version`, `study_id`, `run_id`,
   `generation_id`, and the generation-manifest descriptor.
6. **Commit pointer last:** atomically rename the temporary pointer over the
   current pointer within the same directory. Successful rename is the single
   linearization point: the new complete generation is current from that point.
   All validations occur before it. A pre-rename failure leaves the old pointer
   current. If the rename result or subsequent acknowledgement/read-back is
   ambiguous, recovery reads and validates the current pointer and the complete
   generation it names. Recovery reports old-current or new-current when proven;
   otherwise it returns `D_PUBLICATION_STATUS_UNKNOWN` and MUST NOT claim
   rollback or delete either complete generation.
7. **After commit:** prior generations remain immutable. An orphan unreferenced
   generation or temporary directory may be reported for later cleanup; cleanup
   is not part of scientific success and cannot invalidate the prior pointer.

No current registry or prior generation is moved or archived before step 6.
Every failure before the step-6 rename leaves the prior pointer and generation
bytes unchanged. A definite failed rename reports `D_PUBLICATION_FAILED` and
leaves the prior pointer current. A successful rename is committed even if later
acknowledgement fails; recovery recognizes the pointer it can prove rather than
pretending old bytes survived. Base-R path checks alone are sufficient only
for the declared single-writer/static-hazard model; the implementation must not
claim hostile-race or crash durability.

## 6. Behavioral acceptance matrix

The implementation is not accepted by schema existence or helper-unit tests.
Public synthetic behavioral tests MUST demonstrate:

1. JSON object key reordering preserves `identity_sha256`; ordered-array
   reordering changes it; declared set-like entity ordering normalizes before
   hashing.
2. RFC 8785 reference vectors pass. Duplicate keys, unknown fields, invalid
   numbers, duplicate role/ID tuples, missing required roles, and malformed
   digest/type/size combinations fail closed.
3. A valid contrast ID `model_hash` remains data and cannot mask or satisfy the
   producer identity path.
4. Mutating FeatureRegistry `id_namespace`, normalized visit/time, subject/unit
   mapping, paired sign, repeated-time origin, represented matrix, source bytes,
   or preparation state blocks D before publication.
5. Unchanged DEqMS evidence passes through the B producer identity without D
   recreating its design hash. Count/prior mutation blocks it.
6. Complete B and C native provenance records validate together; a missing or
   conflicting predecessor envelope/payload fails before routing.
7. A complete legacy fixture migrates without changing any numerical payload
   byte. Removing each required proof class in turn yields
   `D_LEGACY_INELIGIBLE`; D does not infer it.
8. Regeneration from public accepted fixtures preserves the already accepted
   statistical outputs and produces new native provenance records.
9. Symlink and dangling-symlink probes at the output root, generation parent,
   registry directory, current pointer, temporary generation, and planned leaf
   fail before the current pointer changes.
10. Forced failure at every step before the pointer rename leaves the old pointer
    byte-identical. A failure after generation publication but before pointer
    commit may leave only an unreferenced complete generation. Fault injection
    at rename acknowledgement/read-back must recover and validate whichever old
    or new complete generation the pointer names, or return
    `D_PUBLICATION_STATUS_UNKNOWN`; it must not assert that the old pointer was
    preserved after a successful rename.
11. A successful commit exposes one complete generation; every referenced
    member exists and matches type, size, and digest. A rerun cannot mutate it.
12. Fresh public Stage B -> C -> D CLIs succeed with actual-byte read-back and
    disabled-route accounting. B/C source trees and prior D generation bytes
    remain unchanged.

Required checks after integration are the affected B-T07, C-06, and D-T01
source suites and exact public CLI fixtures, the full local test suite, and a
clean Git-archive package check. No required skip is a pass. No private study
data are read or delegated.

## 7. Affected acceptance and bounded implementation route

This amendment does not revoke M4 or M6 numerical acceptance. It reopens only
the B-T07 and C-06 handoff-provenance seams for focused compatibility evidence.
D-T01 remains unaccepted. M7-001 remains exhausted and is not retroactively
repaired.

If Astra passes this amendment, Sol may authorize one new cross-cutting
M7-002 implementation cycle with these dependency-ordered phases:

1. **Contract foundation:** one fresh Luna-max worker adds the closed profile,
   JCS-conformance layer, schemas, and contract tests in new exclusively owned
   files. No statistical or producer logic changes.
2. **Producer seams:** after foundation review, fresh file-disjoint Luna-max
   workers may update only the B-T07 and C-06 public export/handoff code and
   their focused tests to emit native or deterministic-migration provenance.
3. **D verification/publication:** after producer integration, one fresh
   Luna-max worker may update only the D-T01 evidence consumer, D publication
   lifecycle, D-owned fixtures/registration request, and focused tests under one
   exclusive allowlist. The worker does not edit shared CLI, stage registration,
   namespace, manuals, or package registration.

Sol alone applies mechanical shared CLI/stage/package registration serially from
the reviewed packet-owned handoff, as required by AGENTS hard rule 5. No shared
implementation exception is granted here. If the handoff requires substantive
shared behavior rather than mechanical registration, Sol stops and records a
separate scoped change proposal before editing.

Each phase has one initial attempt plus at most two targeted repairs, independent
fresh Sol-high review, serial Sol integration, and exact affected tests. This is
a new material-contract implementation cycle, not a reset of D-T01 or M7-001
attempt counters. Any needed file outside the approved phase allowlist stops the
phase for a Sol disposition; workers do not expand scope.

After all phases, Sol runs the section-6 integrated gates on one immutable
snapshot, records focused B-T07/C-06 seam dispositions, and dispatches fresh
Astra-low M7 review. Only Sol can accept D-T01 and M7 or release the next gate.
D-T02 and every Stage-D successor remain closed meanwhile.

## 8. Independent amendment gate

Before implementation, a fresh read-only Astra-low reviewer MUST audit one
immutable commit containing this record, the research notes, glossary, ADR, map,
and orchestration state. The reviewer checks scientific identity completeness,
legacy non-inference, all five M7-001 findings, publication failure semantics,
scope/claim limits, affected acceptance, testability, and implementation
ownership. Astra does not edit, accept the amendment, release M7, or start work.

The initial audit of commit `81fdf776f5bdfb9bccc0628c61b30fc7cb58643f`
returned `changes_required` with three Important findings: incomplete closed
role/graph/manifest definitions, an impossible post-rename rollback expectation,
and shared-registration ownership drift. Documentation repair 1 changed
sections 1, 3, 5, 6, and 7. The focused rereview confirmed that it closed the
rename and ownership findings but only partially addressed the closed-contract
finding. No other repair is allowed.

The single focused rereview of repair commit
`366f284ba81bc595faa42f2017f00aa754ff8c6c` returned `changes_required`:
Critical 0, Important 1, Minor 0. The pointer-linearization and Sol-ownership
findings are closed, but the closed provenance contract still lacks descriptor
`block_id`/`method_id` fields required by its cardinality rules, a closed
`entity_type` and role/type mapping, and an explicit exclusion preventing the
Stage Handoff Provenance payload/envelope from inventorying itself. The durable
receipt is
`docs/implementation/reviews/2026-10-23-m7-002-focused-astra-rereview.md`.
The permitted repair and rereview are exhausted.

## Current disposition

Blocked and not accepted. No implementation phase is authorized. D-T01, M7,
and all Stage-D successors remain blocked. A future attempt requires Omar's
explicit authorization of a new bounded change record; it is not another
M7-002 repair. No product code, frozen spec file, packet JSON, threshold, seed,
method, lockfile, or private study input is changed by this record.
