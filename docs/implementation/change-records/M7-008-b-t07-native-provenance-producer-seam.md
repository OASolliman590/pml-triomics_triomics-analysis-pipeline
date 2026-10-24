# M7-008 — B-T07 native provenance producer seam

- Date: 2026-10-24
- Status: **authorized by Omar**
- Accepted contract-foundation snapshot:
  `b29c3aa5693b29ac232f26344987877e4e79511d`
- Sol acceptance:
  `docs/implementation/reviews/2026-10-24-m7-007-sol-acceptance.md`
- Governing amendment: accepted M7-002 plus M7-003

## Selection decision

The next dependency-legal phase is the B-T07 native Stage Handoff Provenance
producer seam. It precedes C-06 because the C producer must be able to bind the
accepted B handoff and provenance frame as immutable predecessors.

Deterministic legacy migration is not selected. A fresh public B-T07 probe at
the accepted snapshot produced the historical B payloads and envelopes but no
complete immutable proof for normalized metadata, unit/design identity,
preprocessing state, or method state. Under M7-002 section 4, those omissions
make that legacy output ineligible for consumer reconstruction or partial
migration. The lawful route is native producer regeneration from the exact
same-run public synthetic inputs. No private study data were inspected.

## Public seam and behavioral contract

The pre-agreed public TDD seam is the ordinary Stage-B CLI together with the
existing exported post-stage entrypoint `triomics_b07_cli_after_stage_b()`.
After a successful B-T07 run, the handoff directory must contain one native
`StageHandoffProvenance` payload and its ordinary triomics envelope. The profile
must pass `triomics_validate_stage_handoff_provenance()` and bind the exact B
handoff artifact, its complete member inventory, and all required B producer
roles from the same run.

The producer must not re-ingest, refit, reconstruct, or infer a missing entity
inside the export hook. `triomics_execute_stage_b()` may return an additional
in-memory, non-public provenance context containing exact same-run inputs and
states needed by B-T07. That context must not be serialized into block-results,
an envelope, a registry, or a report. If required same-run evidence is absent or
conflicts with persisted bytes, export fails closed and emits no partial
provenance profile.

Required public behavioral evidence:

1. RED first: the exact B-T07 CLI currently emits no native provenance profile.
2. GREEN: the CLI emits a profile/envelope that validate through the public
   provenance validator and bind the exact handoff payload and B result members.
3. Source manifests, normalized metadata, feature registries, analysis-unit and
   resolved-analysis identity, represented matrices, preprocessing state,
   method state, predecessor envelope/payload, block results, family ledger,
   candidate nominations, declared clinical routes, and handoff payload are
   complete under the accepted closed role/type/scope/cardinality contract.
4. Mutating a retained source/metadata/unit/design/preparation/method identity,
   matrix binding, DEqMS count/prior identity, or persisted predecessor byte
   blocks validation or export. A contrast identifier named `model_hash`
   remains ordinary data.
5. Changing a public source file after the Stage-B fit but before the post-stage
   hook cannot be silently rebound as same-run evidence: the original retained
   evidence is used or export fails closed.
6. The existing block-results payload shape and scientific/numerical values are
   unchanged. No private/native model object or raw study value is added to a
   public payload.
7. A complete existing handoff is reusable only when its provenance payload and
   envelope also exist and revalidate byte-for-byte.

## Exclusive allowlist

One fresh isolated Luna-max worker may edit only:

- `R/results_b.R` — expose the exact same-run in-memory provenance context;
- `R/export_b.R` — construct, validate, write, and revalidate native B
  provenance;
- `tests/testthat/test-B-T07.R` — public-seam RED/GREEN and regression fixtures;
- `docs/implementation/registrations/B-T07.json` — candidate evidence request.

The worker must not edit `R/provenance_contract.R`, the provenance schema,
shared CLI or stage registration, NAMESPACE, DESCRIPTION, manuals, lock files,
frozen `discovery/spec-v2`, C/D code, the ledger, handover, or private inputs.
If the accepted contract helper or schema is insufficient, the worker stops
with evidence instead of expanding scope.

## Attempts, review, and gates

The cycle allows one initial implementation plus at most two targeted repairs.
The worker uses vertical public-seam RED/GREEN slices and runs the exact B-T07
suite plus only directly affected B source tests. It returns one clean commit
and a concise receipt; it does not run the full suite or accept a gate.

Sol then performs the two-axis Standards/Spec review against this authorization
and fixed point `3c11c9c`. Both fresh read-only Sol-high axes must pass before
serial integration. Sol reruns the focused B-T07 and affected B regression
suites, then one uninterrupted full source suite. This phase receives a focused
producer-seam disposition only. The milestone Astra audit remains deferred until
the later C-06 and D verification/publication phases complete.

## Closed scope

This cycle does not implement deterministic legacy migration, C-06 provenance,
D verification/publication, shared CLI/package registration, D-T01 acceptance,
M7 acceptance, or any Stage-D successor. It does not loosen B-T04's 0.08 bound,
reroll seeds 1001:1400, suppress warnings, read private study data, or push.
