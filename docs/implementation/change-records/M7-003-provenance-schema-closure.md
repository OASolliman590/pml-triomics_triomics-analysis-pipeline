# M7-003 — provenance schema closure

- Date: 2026-10-23
- Status: **accepted by Sol after focused Astra-low pass; contract foundation released**
- User authority: Omar explicitly authorized M7-003 to close the three remaining
  provenance-schema defects, followed by a fresh Astra-low audit, with no
  implementation before that audit passes.
- Frozen base specification hash:
  `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`
- Predecessor records:
  `M7-002-stage-handoff-provenance-amendment.md` and
  `2026-10-23-m7-002-focused-astra-rereview.md`.

## 1. Authority and narrow scope

M7-002 remains historically blocked and is not silently repaired. If this
record passes its independent audit and Sol accepts it, the normative candidate
is the M7-002 contract with only the replacements and clarifications in this
record. All other M7-002 provisions remain unchanged, including immutable
producer proof, deterministic legacy packaging, producer regeneration,
canonical hashing, pointer-rename publication semantics, test gates, scientific
limits, and Sol-only shared registration.

This record replaces M7-002 sections 1.1, 1.2, and 1.3 and clarifies sections 1,
2.9, 3, 6, 7, and 8 only where needed to close:

1. the missing descriptor `block_id` and `method_id` scope fields;
2. the missing closed `entity_type` registry and role/type mapping; and
3. the missing exclusion that prevents a provenance payload or envelope from
   inventorying itself.

It changes no estimator, model, analysis population, family, threshold, seed,
scientific claim, packet identity, dependency, or private-data policy. The
B-T04 bound remains 0.08 and seeds remain 1001:1400.

## 2. Closed identity, route registry, and Scientific Entity Descriptor

The closed `identity` object contains exactly `schema_uri`, `schema_version`,
`handoff_role`, `activities`, `entities`, and `declared_routes`. This replaces
the M7-002 section-1.1 root-member sentence. `declared_routes` is always present
and is a sorted array, empty when the producer declares no clinical or native
route. Each member is a closed record containing exactly:

- `route_id`, a non-empty identifier unique within the profile;
- `route_kind`, exactly `clinical` or `native`;
- `analysis_id`, `preparation_context_id`, and `representation_id`, all
  non-empty; and
- `method_id`, null for `clinical` and non-empty for `native`.

The array is sorted uniquely by `(route_kind, route_id)`. Each record is derived
from the validated producer configuration bound by the profile's effective-
configuration digest. A route cannot be inferred from current consumer state.
The matching rules in section 3 require one evidence descriptor for every
declared route, including a typed unavailable descriptor when the declared
route did not run.

Every `identity.entities` member is a closed `ScientificEntityDescriptor`. The
following keys are always present; no other key is permitted:

- identity and classification: `entity_id`, `entity_type`, `scientific_role`,
  `schema_version`;
- storage and content: `storage_form`, `media_type`, `digest_algorithm`,
  `digest_kind`, `digest`, `size_bytes`, `canonical_schema_uri`, `artifact_id`,
  `artifact_payload_sha256`;
- graph: `generated_by_activity_id`, `derived_from_entity_ids`,
  `member_of_entity_id`; and
- scope: `study_id`, `analysis_id`, `block_id`, `method_id`,
  `preparation_context_id`, `representation_id`, `route_id`,
  `row_identity_sha256`, `column_identity_sha256`.

Every key remains present even when its value is inapplicable. Inapplicable
scalar keys have JSON null. Applicable identifiers are non-empty strings.
`derived_from_entity_ids` is always a sorted unique array, including when empty.
The two ordered-identity fields, when non-null, are full lowercase SHA-256
digests of the canonical ordered row-key or column-key array used by that
entity. They are content identities, not graph references.

Storage conditions are exhaustive:

- `storage_form=file` requires `digest_kind=bytes`, non-negative integer
  `size_bytes`, and null `canonical_schema_uri`, `artifact_id`, and
  `artifact_payload_sha256`;
- `storage_form=enveloped_artifact` requires `digest_kind=bytes`, non-negative
  integer `size_bytes`, non-null `artifact_id` and
  `artifact_payload_sha256`, and null `canonical_schema_uri`; and
- `storage_form=embedded_json` requires `digest_kind=rfc8785`, non-null
  `canonical_schema_uri`, and null `size_bytes`, `artifact_id`, and
  `artifact_payload_sha256`.

`digest_algorithm` is always `sha256`; `digest` and every non-null SHA-256 field
are 64-character lowercase hexadecimal values. Graph resolution, acyclicity,
and generated/external-entity rules remain as specified by M7-002.
`member_of_entity_id` is non-null only for a `predecessor_payload`, where it
identifies that pair's `predecessor_envelope`, or an `integration_state`, where
it identifies the matching `integration_result`; it is null for every other
role.

## 3. Closed role, type, scope, and cardinality registry

`entity_type` is the provenance category below, not a replacement for an
artifact payload's own `artifact_type` or schema. Payload type and schema remain
bound by the envelope or `canonical_schema_uri`. Every scientific role has
exactly one allowed provenance entity type:

| `scientific_role` | required `entity_type` | required non-null scope beyond `study_id` | cardinality |
|---|---|---|---|
| `source_manifest` | `triomics.source_manifest` | none | one or more per profile |
| `normalized_metadata` | `triomics.normalized_metadata` | `analysis_id`, row and column identity | exactly one per `analysis_id` |
| `feature_registry` | `triomics.feature_registry` | `block_id`, row identity | exactly one per `block_id` |
| `analysis_unit_map` | `triomics.analysis_unit_map` | `analysis_id`, `preparation_context_id`, `representation_id`, row identity | exactly one per `(analysis_id, preparation_context_id, representation_id)` |
| `resolved_analysis` | `triomics.resolved_analysis` | `analysis_id`, `preparation_context_id`, `representation_id`, row and column identity | exactly one per `(analysis_id, preparation_context_id, representation_id)` |
| `represented_matrix` | `triomics.represented_matrix` | `analysis_id`, `block_id`, `preparation_context_id`, `representation_id`, row and column identity | exactly one per `(analysis_id, block_id, preparation_context_id, representation_id)` |
| `preprocessing_state` | `triomics.preprocessing_state` | `analysis_id`, `block_id`, `preparation_context_id`, `representation_id` | one or more per `(analysis_id, block_id, preparation_context_id, representation_id)` |
| `method_state` | `triomics.method_state` | `analysis_id`, `method_id`, `preparation_context_id`, `representation_id` | exactly one per `(analysis_id, method_id, preparation_context_id, representation_id)` |
| `predecessor_envelope` | `triomics.predecessor_envelope` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per accepted predecessor pair |
| `predecessor_payload` | `triomics.predecessor_payload` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per accepted predecessor pair |
| `block_results` | `triomics.block_results` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per exported B handoff |
| `family_ledger` | `triomics.family_ledger` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per exported B handoff |
| `candidate_nominations` | `triomics.candidate_nominations` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per exported B or C handoff |
| `integration_result` | `triomics.integration_result` | `analysis_id`, `method_id`, `preparation_context_id`, `representation_id` | one or more per exported C handoff; exactly one per `(analysis_id, method_id, preparation_context_id, representation_id)` |
| `integration_state` | `triomics.integration_state` | `analysis_id`, `method_id`, `preparation_context_id`, `representation_id` | exactly one per matching `integration_result` |
| `evaluation_evidence` | `triomics.evaluation_evidence` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per exported C handoff |
| `clinical_evidence` | `triomics.clinical_evidence` | `analysis_id`, `preparation_context_id`, `representation_id`, `route_id` | exactly one per matching declared `clinical` route |
| `native_sidecar` | `triomics.native_sidecar` | `analysis_id`, `method_id`, `preparation_context_id`, `representation_id`, `route_id` | exactly one per matching declared `native` route |
| `handoff_payload` | `triomics.handoff_payload` | `analysis_id`, `preparation_context_id`, `representation_id` | exactly one per profile key |

`study_id` is non-null for every descriptor. A scope key named in the table is
non-null; every scope key not named for that row is null. "Row identity" and
"column identity" mean non-null `row_identity_sha256` and
`column_identity_sha256`. Cardinality grouping uses only the tuple explicitly
named in that row's cardinality cell. Other required non-null scope fields are
consistency constraints, not hidden grouping keys; a verifier must not invent
an implicit grouping key.

The **profile key** is the root
`(producer_stage, study_id, run_id, handoff_artifact_id)` tuple. "Per profile"
and "per exported handoff" in the table both mean this exact key. B profiles
require the B-only singleton roles; C profiles require the C-only singleton
roles. A role assigned to the wrong producer stage is invalid. The one-or-more
rows (`source_manifest`, `preprocessing_state`, and C `integration_result`
across distinct result tuples) explicitly permit multiple descriptors in their
group. Every `entity_id` remains globally unique, but sharing a permitted
one-or-more group key is not duplication. For an exactly-one row, count zero is
missing and count greater than one is duplicate.

For a matching predecessor pair, the envelope descriptor has
`storage_form=enveloped_artifact`; the payload descriptor has
`storage_form=file`; the payload's `member_of_entity_id` equals the envelope's
`entity_id`; their scope tuples are identical; and the payload `digest` equals
the envelope's `artifact_payload_sha256`. Each predecessor envelope has exactly
one member payload, and each predecessor payload names exactly one envelope.

For a matching integration pair, `integration_state.member_of_entity_id` equals
the result's `entity_id`, their scope tuples are identical, each result has
exactly one state, and each state names exactly one result. The C profile has at
least one integration result; its table cardinality tuple is unique among them.

For routes, every `declared_routes` record has exactly one descriptor of the
matching role: `clinical` maps to `clinical_evidence`, and `native` maps to
`native_sidecar`. The descriptor's `route_id`, analysis, method nullability,
preparation, and representation equal the declaration. Every such descriptor
matches exactly one declaration; an undeclared route descriptor is invalid.
Two route records may share all other scope values when their `route_id` values
differ. The `handoff_payload` descriptor has
`storage_form=enveloped_artifact` and its `artifact_id` equals the root
`handoff_artifact_id`.

Unknown roles or types, a non-table role/type pair, missing or surplus scope,
null required scope, duplicate entity ID, wrong-stage role, broken pair or route
mapping, or unmet row-specific cardinality is `D_PROVENANCE_INVALID`.

## 4. Provenance-frame self-exclusion

The `StageHandoffProvenance` JSON payload and the ordinary artifact envelope
whose `payload_sha256` binds that JSON are the provenance frame. They are not
scientific entities described by their own `identity` object.

For a profile P:

- P's payload and envelope MUST NOT appear in P's `identity.entities`;
- their IDs MUST NOT appear in P's activities' `used_entity_ids` or
  `generated_entity_ids`;
- they MUST NOT appear among P's `handoff_payload` descriptors or produced
  handoff member inventory;
- `handoff_artifact_id` identifies the scientific handoff payload, never P or
  P's envelope; and
- M7-002 lineage-closure item 9 means the scientific handoff payloads,
  envelopes, and members only; it excludes P and P's envelope.

This exclusion removes circular identity. P's `identity_sha256` is computed
before the provenance payload is serialized; the enclosing envelope then binds
the final provenance payload bytes. A later producer profile MAY describe P and
P's envelope as accepted predecessor artifacts using exact byte digests. That
later reference does not place them inside P's own identity graph.

A descriptor or activity reference to the current profile's provenance payload
or envelope is `D_PROVENANCE_INVALID`, even if its digest is otherwise correct.

## 5. Required behavioral evidence

Before the combined amendment can be implemented or accepted, its future
contract tests must demonstrate at least:

1. every descriptor contains the exact closed key set, including `block_id` and
   `method_id`; unknown, missing, or surplus keys fail;
2. every table row accepts its one role/type pair and rejects another role's
   type;
3. every required scope key rejects null, and every inapplicable scope key
   rejects non-null;
4. cardinality grouping changes when a required block or method changes and
   never uses an undeclared field;
5. two distinct preprocessing states in one valid group pass, while a missing
   group and an exactly-one group with two descriptors fail;
6. two declared clinical routes and two declared native routes sharing all
   non-route scope pass; missing, duplicate, wrong-kind, mismatched-scope, and
   undeclared-route descriptors fail;
7. every predecessor envelope/payload and integration result/state pair is
   one-to-one and scope-consistent;
8. the current provenance payload and envelope are rejected from their own
   entities, activities, and inventories before D mutates output;
9. a later producer may bind an earlier profile as an immutable predecessor
   without creating self-reference; and
10. all M7-002 behavioral tests and the previously closed pointer-rename and
   Sol-ownership rules remain unchanged.

These are specification requirements only in this record. No test or product
code is authorized until the independent audit passes.

## 6. Acceptance and implementation guard

One fresh read-only Astra-low reviewer audits an immutable commit containing
M7-002, M7-003, both M7-002 audit receipts, the glossary, ADR 0003, the
Wayfinder map, handover, and ledger. The reviewer decides whether:

- the three remaining schema defects are completely closed and mechanically
  testable;
- the new table is internally total and consistent with the unchanged lineage,
  legacy, publication, and ownership contract;
- no circular digest or hidden self-inventory remains;
- no statistical or clinical claim changed; and
- M7-001 and M7-002 history remains truthful.

A pass permits Sol to accept the combined amendment and begin only the contract-
foundation phase inherited from M7-002 section 7, now recorded as the M7-003
implementation cycle. `changes_required` permits at most one documentation
repair and one focused rereview. Astra does not edit, accept the amendment,
release M7, dispatch workers, or implement.

The initial audit of commit
`0e128c3a0bb926f569e6d35c95ef100e3059df9a` returned
`changes_required`: Critical 0, Important 1, Minor 0. It confirmed the closed
descriptor, role/type mapping, and provenance-frame exclusion, but found the
cardinality rules internally inconsistent for permitted multi-state groups and
non-executable for multiple clinical/native routes. Documentation repair 1 adds
the closed route registry and route key, executable pair/route/profile mappings,
and separate entity-uniqueness versus row-cardinality rules. The receipt is
`docs/implementation/reviews/2026-10-23-m7-003-astra-audit.md`. No second repair
is allowed after the focused rereview.

The focused rereview of immutable repair
`2ae1b6e1c1e0b9a47efcdf054da6f2e6f5791bff` returned `pass`: Critical 0,
Important 0, Minor 1. The Minor requires an explicit future negative fixture for
a native descriptor whose non-null `method_id` differs from its declared route;
it is an evidence clarification under the existing mismatched-scope rule, not a
contract repair. Sol's acceptance is recorded in
`docs/implementation/reviews/2026-10-23-m7-003-sol-acceptance.md`.

Sol acceptance releases only contract foundation. D-T01, M7, all Stage-D
successors, producer-seam changes, D verification/publication, migration, and
shared registration remain blocked. No private study-data access is authorized.

## Current disposition

Accepted specification delta. Only the contract-foundation implementation phase
is released. Producer seams, D verification/publication, shared registration,
migration, D-T01 acceptance, M7 release, and every Stage-D successor remain
closed. No product file, frozen `discovery/spec-v2` file, packet JSON, threshold,
seed, method, lockfile, or private study input was changed by this record.
