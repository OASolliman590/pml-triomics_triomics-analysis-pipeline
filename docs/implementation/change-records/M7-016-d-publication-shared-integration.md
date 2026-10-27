# M7-016 — D publication with authorized shared integration

2026-10-27; base123d2cb. Omar explicitly authorizes the shared D executor and
registration alongside D-owned implementation, audit first, then one Luna worker
and actual B-to-C-to-D verification. M7/G7 stay closed until fresh Astra review
passes. This resolves M7-015's scope blocker; no old worker is restarted.

## Binding scope

The scientific/provenance/publication behavior and acceptance matrix in M7-015
and accepted M7-002 (as amended by M7-003 and integrated foundation repairs)
remain binding. No schema or statistical changes. D-T01 is the only D packet.
Do not execute downstream annotation methods, targets, reports or D-T02.
C-06 stays unaccepted in this run, four C-07 controls stay not_run and C-09
unqualified. No private study data or push. Prior blocked cycles remain history.

One fresh Luna-max worker, no child agents, isolated worktree, no commits, may
edit exactly these existing files:

- R/evidence_bundle.R
- R/annotation_router.R
- R/resource_manifest.R
- tests/testthat/test-D-T01.R
- inst/examples/tickets/D-T01.yaml
- docs/implementation/registrations/D-T01.json
- R/cli.R — D executor/dispatch only; preserve all A/B/C behavior
- R/stage_registry.R — enable D execution only, no unrelated dispatch changes
- NAMESPACE — D public API exports only
- man/triomics-api.Rd — matching D public aliases, usage and documentation only

The last four files are an explicit single-worker exclusive exception authorized
by Omar's latest instruction. Sol retains sole integration/acceptance/ledger
ownership. No roxygen. No DESCRIPTION/dependency/lock-file change. Stop with
specific evidence for a needed out-of-scope file; never bypass a shared guard
through source-time rebinding. Fixture effect and named contrast labels may be
corrected within D-T01.yaml without changing the estimand or numerical policy.

## Task 1: Verify native predecessors and publish D atomically

Read M7-015's consumer and publication requirements, M7-002 sections3/5/6, the
actual integrated provenance validator and B/C producer interfaces. Write
behavioral failing tests first. Extend D reader to require same-root native B/C
profiles and bind all exact payload/envelope bytes; validate complete typed
lineage and current-request identity before any routing/output mutation. Reject
changed measurements, consumed metadata and FeatureRegistry, mismatched prepared
matrices/state, unbound/rehashed envelopes, missing proofs and unsafe paths.
Use producer identities; never recreate a B design hash. Object-key ordering is
canonical-value invariant; exact bound-file byte changes are not equivalent.
Preserve model_hash as a valid contrast ID and DEqMS producer compatibility.

Implement the D-owned generation publisher with exclusive single-writer lock,
read-only path/request/provenance preflight, exclusive temporary generation,
complete typed member manifest, immutable generation named by full manifest
SHA-256 and small current-generation pointer atomically replaced LAST.
Every precommit failure preserves the old pointer and all old generation bytes.
Postrename acknowledgement ambiguity must validate old/new complete state or
return D_PUBLICATION_STATUS_UNKNOWN, never falsely claim rollback. Validate
symlink/dangling-symlink ancestry and leaf hazards before mutation, including
registry, temporary, generation and pointer paths. Do not overwrite C's D
handoff files or B/C run registries. No hostile concurrent mutation, power-failure
or OS-crash durability claim.

The CLI D executor performs config validation and nonmutating ingestion/current
identity comparison, then invokes the verified consumer/publisher. It must not
call a generic registry preparation routine which archives or rolls over prior
state before D validation. D never launches B/C. Use the current pointer as the
publication commit; no second mutable run registry becomes a conflicting commit
point. Return success only for a validated complete publication generation;
retain partial/unrun scientific status inside the evidence and route accounting.
Account for every route, including disabled routes, without implementing later
annotation consumers. Shared wiring is thin and exclusive to this worker.

## Task 2: Sol verification and fresh snapshot review

Sol inspects diff/scope and runs B-T07, C-06 and D-T01 without suppressed warnings
or required skips. Run public B then C then D with the corrected D-T01 ticket in
one fresh output root. Read back profiles, every exact predecessor digest, D
manifest/member type/size/hash, pointer and enabled/disabled routes. Prove B/C
trees unchanged and prior D generation immutable across successful rerun and
injected precommit failures. Include all accepted M7-002 section6 behavioral
probes; reuse existing foundation tests where they already prove an invariant
rather than claim unrelated old suite counts as new evidence.

Integrate only after focused proofs and actual-byte B-to-C-to-D read-back. Then
run the required full source suite and clean git-archive package check on an
immutable integrated candidate; Status: OK is required, shell exit0 is not.
Record named warnings and all counts. Preserve the isolated MOFA runtime if
available. Freeze exact candidate and concise evidence for a fresh read-only
Astra-low acceptance audit. This audit is separate from the phase-record audit.
M7/G7 remain closed until all gates and that audit pass. No downstream D work.

## Attempts and dispositions

One initial implementation and at most two targeted repairs for reproduced
failures; same Luna handles repairs. One initial phase-record audit plus at most
one focused documentation rereview if needed. Fresh final Astra audit plus at
most one focused rereview after bounded repair. Sol records spec compliance and
code-quality review; implementation approval is not scientific validation.
No attempt counters from M7-001/011/012/013 are reset. M7-015 used no worker.
If a required gate fails at cap, preserve evidence, mark this cycle blocked and
stop honestly without acceptance. All new work is public synthetic software.

## Preflight interface rulings

| Tasks/interface | Finding and ruling |
|---|---|
| Native B/C -> D consumer | Integrated producers exist; D binds persisted profiles, never constructs producer lineage. |
| D consumer -> publisher | Verification completes before publication mutation; pointer-last is the single commit point. |
| Publisher -> CLI | Previously forbidden shared seam is now explicitly authorized; no generic run rollover. |
| Task1 allowlist/tests | Existing D-owned suite covers shared D wiring; no separate shared test file required. |
| Task2 -> acceptance | Focused CLI proof, full source and clean archive check precede fresh Astra; M7/G7 closed meanwhile. |
