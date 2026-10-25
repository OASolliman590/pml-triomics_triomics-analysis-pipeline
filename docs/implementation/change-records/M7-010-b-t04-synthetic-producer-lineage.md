# M7-010 — truthful B-T04 synthetic-producer lineage

- Date: 2026-10-25
- Status: **authorized for definition and audit; implementation prohibited until
  a fresh read-only Astra-low audit passes**
- Product base: `5c95359567fd`
- Trigger:
  `docs/implementation/reviews/2026-10-25-m7-009-integration-gate.md`
- Governing provenance contract: accepted M7-002 plus M7-003, M7-006 and
  M7-007 repairs
- User authority: Omar selected the truthful native synthetic-producer route,
  explicitly excluded the unused two-visit matrix from response ancestry, and
  authorized implementation and verification only after the amendment audit.

## Purpose and selected approach

M7-009 integrated the native B-T07 producer seam. Its focused gates passed, but
the full source suite exposed one compatibility boundary: the custom B-T04
executor returns its exact resolved matrix without the same-run provenance
context required by B-T07. The post-stage hook correctly fails closed with
`B_EXPORT_INCOMPLETE`.

Two lawful approaches were considered:

1. publish a complete native profile for the deterministic B-T04 synthetic
   producer; or
2. preserve the statistical result but declare it ineligible for a downstream
   handoff.

Omar selected approach 1. It preserves the existing public synthetic
composition route while making its actual derivation explicit. The alternative
would be simpler but would intentionally remove the accepted B-T04-to-B-T07
composition behavior.

This amendment clarifies how M7-002 sections 1.3 and 2 apply to a deterministic
synthetic producer. It introduces no new provenance role, schema version,
statistical method, scientific claim or real-study qualification.

## Normative synthetic-source rule

A source is ancestral only when its bytes or canonical value materially
influence the generated metadata, matrix, design, fit or published result.
Configuration references used only to exercise schema parsing are not sources
of the synthetic response.

For B-T04:

- the two configured public paired-example TSVs are schema fixtures only;
- neither TSV is read to construct the B-T04 study, response, design or fit;
- their payload bytes, digests and source IDs MUST NOT appear as response
  `source_manifest` roots, block-results envelope inputs, matrix derivation
  parents or result derivation parents; and
- retaining their paths in the validated ticket does not make them ancestors.

The producer MUST instead materialize one immutable, versioned B-T04 synthetic
protocol before generation and fitting. That protocol is the `source_manifest`
root used by the analysis activity. Its exact canonical bytes and digest MUST
be retained and it MUST define, without relying on the paired-example TSVs:

- protocol and generator identifiers;
- the ordered subjects `sub0001` through `sub0004`;
- ordered time values `0`, `1`, `2`, visit labels and time unit
  `synthetic_step` with origin `0`;
- feature identifiers `F1` and `F2`;
- subject intercepts `5`, `7`, `9`, `11`;
- feature slopes `1.5` and `-0.25`;
- residual pattern `0.2`, `-0.4`, `0.2`;
- linear scale and `synthetic_arbitrary_units`;
- deterministic generation with no RNG; and
- the effective configuration digest, generator protocol version, and an
  explicit statement that the configured two-visit TSVs are non-ancestral.

The profile's producer record continues to bind the exact repository commit and
runtime/backend versions. The protocol, effective configuration digest and
repository commit are complementary identities and MUST NOT be substituted for
one another.

## Required generated lineage

The B-T04 same-run context MUST retain all of the following from the actual
producer invocation:

1. generated normalized metadata for all twelve ordered samples, including
   sample, subject, visit, exact time, time unit and origin identity;
2. a generated SampleRegistry and complete FeatureRegistry with study/block,
   original and canonical feature IDs, synthetic namespace, source row,
   quantity, units, scale, structural/alias/parent dispositions and annotation
   route;
3. an AnalysisUnitMap with study, analysis and preparation context, ordered
   units, self source-sample mappings, subject/visit/time identity,
   eligibility/exclusion, represented block and `subject_centered`
   representation;
4. the ResolvedAnalysis with ordered rows and columns, exact numeric design
   identity, ordered subjects/times, formula, contrast weights, residual
   contract, family and representation rules;
5. the exact uncentered two-by-twelve generated response matrix that is retained
   by `resolved$blocks$block_1$assay` and handed to Stage C;
6. the deterministic subject-centering rule and the exact fitted centered
   response identity derived from item 5 and the ordered subject map;
7. method state binding limma settings/version, retained subject columns,
   rank/residual-df rule, contrast, moderation settings, and the unchanged
   B-T04 calibration protocol/result identity;
8. exact block-results payload/envelope paths and hashes, run ID and producer
   method IDs; and
9. the existing family ledger, nominations, matrix handoff, bundle and their
   native B-T07 provenance frame.

The exported Stage-C matrix remains the exact uncentered generated response.
The profile MUST NOT call those values already centered. The fitted centered
response is a deterministic method view and must be bound separately by its
transformation rule, ordered subject identity and exact digest. No private or
native model object is serialized.

The private `provenance_context` remains an in-memory return value only. It MUST
NOT be added to block-results, envelopes, registries, reports or the public
bundle.

## Public behavior and fail-closed rules

A successful B-T04 CLI run MUST emit the existing B-T04 artifacts and one
complete B-T07 handoff with a native `StageHandoffProvenance` payload/envelope
that pass the public validator and exact-byte resume validation.

The producer MUST fail closed before publishing a complete/reusable handoff if
any of the following are missing, changed or inconsistent:

- retained protocol bytes or digest;
- generated metadata, registry or unit identity;
- ordered design, subject/time or centering identity;
- generated uncentered matrix or fitted centered-response digest;
- method/calibration identity;
- block-results payload or envelope bytes; or
- any required B-T07 late artifact or provenance member.

A digest of an unused paired-example TSV cannot satisfy any of these checks.
An unavailable or malformed synthetic-producer context is
`B_EXPORT_INCOMPLETE`; it is not eligible for partial legacy reconstruction.

## TDD and verification contract

The implementation MUST use public vertical RED/GREEN slices:

1. Preserve evidence that the exact B-T04 CLI and source-composition test fail
   at the current base with `B_EXPORT_INCOMPLETE` because the custom executor
   omits `provenance_context`.
2. Make both routes complete with a publicly validated provenance
   payload/envelope while retaining effects `1.5` and `-0.25`, ordinary df `7`,
   design rank `5`, the exact existing two-by-twelve matrix, and the unchanged
   calibration and B-T04 acceptance results.
3. Assert the protocol is the ancestry root and the paired-example TSV payload
   bytes/digests are absent from block-envelope inputs, `source_manifest`
   entities and derivation edges.
4. Assert ordered generated metadata, registry, unit map, full design,
   uncentered exported matrix, centering rule and centered-response digest.
5. Mutate or omit each material protocol/context class and prove export fails
   closed without a complete provenance frame or reusable bundle.
6. Prove no context, assay, fit or native model object is serialized in the
   public block result.
7. Run exact B-T04, Stage-B composition and B-T07 non-regression suites, then a
   fresh two-axis review, serial integration and one uninterrupted full source
   suite with warnings unsuppressed.

No existing assertion may be weakened merely to obtain GREEN.

## Implementation boundary after audit pass

One fresh isolated Luna-max implementer may edit only:

- `R/design_repeated.R`;
- `R/export_b.R`;
- `tests/testthat/test-B-T04.R`;
- `tests/testthat/test-M4-005-stage-b-composition.R`; and
- `docs/implementation/registrations/B-T07.json`.

`tests/testthat/test-B-T07.R` is a required non-regression gate but is not in the
edit allowlist. If the accepted exporter/schema cannot represent this truthful
lineage within the allowed files, the implementer stops with evidence rather
than broadening scope.

The cycle permits one implementation plus at most two focused repair rounds.
Each repair receives a fresh scoped review. Both Standards and Spec axes must
pass before serial integration.

## Closed scope

Do not change `discovery/spec-v2`, the frozen hash, provenance schema,
`R/provenance_contract.R`, package registration, C/D code, statistical methods,
effect estimates, calibration algorithms, the B-T04 `0.08` bound or seeds
`1001:1400`. Do not treat the ordinary 23/400 historical diagnostic as passing.
Do not read private study data, suppress warnings, implement legacy migration,
accept B-T07/M7, release G7, start downstream Stage D or push to GitHub.

## Audit question

Does this amendment create a complete and scientifically truthful native
B-T04 lineage using only artifacts that actually generate its synthetic result,
while distinguishing the exported uncentered matrix from the fitted centered
response and excluding the unused two-visit TSVs from ancestry?
