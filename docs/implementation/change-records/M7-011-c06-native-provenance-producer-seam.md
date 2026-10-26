# M7-011 — C-06 native provenance producer seam

- Date: 2026-10-26
- Status: **defined, awaiting Astra-low audit**
- Base snapshot: `a93a754`
- Governing contract: accepted M7-002 plus M7-003, as implemented by the accepted contract foundation and the integrated B producer through M7-010
- Predecessor evidence: `docs/implementation/reviews/2026-10-26-m7-010-resumed-source-gate.md`

## Selection decision

M7-010 closed the B-T04 synthetic-producer hole. The uninterrupted source suite passed at `2a8e5d5` with FAIL 0, WARN 14, SKIP 0, ERROR 0, PASS 5893. Those 14 warnings are the named C-05 small-fixture set. B-T07 remains `candidate/review_pending`. M7, G7, and Stage D remain closed.

The next dependency-legal phase is the C-06 native Stage Handoff Provenance producer. M7-002 section 7 orders it after the B producer and before D verification. The C profile must bind the same-run B handoff and the B provenance frame as immutable predecessors. It must not reconstruct missing B lineage.

Deterministic legacy migration is not selected. A C handoff whose B predecessor lacks a complete revalidated provenance frame fails closed. No private study data are inspected.

This record does not revoke M4 or M6 numerical acceptance. It does not accept M7, release G7, or accept D-T01.

## Public seam

The public seam is the ordinary Stage-C CLI together with `triomics_c06_write_handoff()`. After a successful C-06 run, the D handoff directory must contain one native `stage-handoff-provenance.json` payload and `stage-handoff-provenance.envelope.json`.

The profile uses `profile = "triomics.stage-handoff-provenance"`, `profile_version = "1.0"`, and `producer_stage = "C"`. It must pass `triomics_validate_stage_handoff_provenance()` and then revalidate from the bytes just written. `handoff_artifact_id` is the scientific C/D handoff payload, never the provenance payload or its envelope.

The producer must not re-ingest, refit, or infer a missing predecessor inside the export hook. Required same-run evidence comes from the handoff object already passed to `triomics_c06_write_handoff()` plus the exact B payload, B envelope, B provenance payload, and B provenance envelope from that run. If any required byte is absent or disagrees with its recorded digest, export fails closed and writes no partial provenance frame.

## Required behavior

1. RED first: the exact C-06 CLI currently writes the D handoff and does not write a native C provenance frame.
2. GREEN: the CLI writes a profile and envelope that validate and bind the exact D handoff members from the same run.
3. The C profile includes the C-only singleton roles `integration_result`, `integration_state`, and `evaluation_evidence`. It does not include B-only roles `block_results` or `family_ledger`.
4. Each integration result has exactly one integration state. Their scope tuples match, and `integration_state.member_of_entity_id` names that result.
5. The same-run B handoff payload, B envelope, B provenance payload, and B provenance envelope are predecessor entities. Payload `digest` equals the envelope `artifact_payload_sha256`. Pair scope tuples match. Digests are full SHA-256 of the bytes on disk.
6. The four C-07 controls stay explicitly `not_run`. `evaluation_evidence` binds the existing evaluation artifact. The producer does not execute C-07 or turn an unrun control into a pass.
7. C-09 stays conditional. The profile does not declare a clinical route and does not emit `clinical_evidence` for an unqualified endpoint. `benefit_guaranteed` stays false. No real deployment or uncertainty protocol is invented.
8. A native route is emitted only when the same-run handoff already has that native sidecar and its method declaration. A descriptor `method_id` that differs from its declaration fails validation.
9. Comparison status, variance units, nomination bytes, and privacy class stay as the accepted C-06 handoff already records them. A partial handoff remains partial.
10. Mutating a bound predecessor byte, integration payload, evaluation payload, preparation identity, or representation identity blocks export. The current provenance payload and envelope are absent from their own entity list, activities, and inventory.
11. A second write into an existing provenance frame fails closed. Reuse requires byte-for-byte revalidation of the existing frame.

## Exclusive allowlist after audit pass

One fresh isolated Luna-max worker may edit only:

- `R/export_c.R`
- `tests/testthat/test-C-06.R`
- `docs/implementation/registrations/C-06.json`

The worker must not edit `R/provenance_contract.R`, the provenance schema, `R/cli.R`, stage registration, NAMESPACE, DESCRIPTION, manuals, lock files, frozen `discovery/spec-v2`, B code, D code, the ledger, the handover, or private inputs. If the accepted validator or the existing handoff object cannot represent this frame, the worker stops with evidence.

The cycle permits one implementation plus at most two focused repairs. Each repair receives a fresh scoped review. Standards and Spec must both pass before serial integration. Sol then reruns the exact C-06 suite and the B-T07 non-regression suite. No assertion is weakened to obtain a green result.

## Closed scope

Do not implement D verification or publication, legacy migration, shared registration, D-T01 acceptance, M7 acceptance, or G7 release. Do not loosen the B-T04 0.08 bound or reroll seeds 1001:1400. Do not suppress warnings, read private study data, or push to GitHub.

## Audit question

Does this record authorize one C producer that binds the same-run B handoff and B provenance frame by exact bytes, keeps C-07 unrun and C-09 unqualified, and leaves the provenance schema and D consumer unchanged?
