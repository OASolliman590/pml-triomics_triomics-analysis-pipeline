# M7-013 — Stage C publication split

Date: 2026-10-26. Base: `55389dd`. Status: candidate for immutable Astra-low audit;
implementation closed pending `audit_pass: yes`.
Authority: Omar's explicit M7-013 instruction. M7-011 and M7-012 remain blocked
and incomplete; their workers and repair cycles are not resumed.

## Decision and boundary

Separate successful C-08 endpoint publication from C-06 publication of a
B-backed D handoff. This resolves the shared-caller contradiction recorded in
`reviews/2026-10-26-m7-012-preflight-blocker.md`; it does not repair B production.
The old C-06 290 and C-08 52 passes are baselines, not provenance GREEN.

The CLI chooses publication intent from the existing resolved request, not
from whether B files happen to exist. The existing C-08 route with configured
clinical objectives and a produced clinical score request is endpoint-only:
it retains its endpoint/request artifacts and schedules B without executing B.
It does not invoke the C-06 D handoff publisher, write a D handoff manifest or
C provenance pair, or declare the D handoff complete. A successful Stage C
endpoint run is not a complete D handoff. Both existing fresh-root C-08 tests
retain every success criterion and gain only additive assertions proving this.
A C-06 request (including the public C-06 ticket and direct handoff writer)
requires B-backed publication; absence of B never silently changes its intent.
No new config-schema field or ticket-name/path special case selects this split.
If existing resolved intent cannot distinguish these routes, stop with evidence.

Endpoint-only execution must not leave an existing D frame appearing current
for its changed C outputs: refuse conflicting/pre-existing D publication rather
than delete it or claim reuse. This run adds no mixed-request auto-orchestration.
A request requiring both publications must satisfy the C-06 prerequisites;
ambiguity must fail closed rather than silently downgrade to endpoint-only.

## Same-root native predecessor

Before C-06 publication, the exact output root must already contain a complete
native B frame written by the existing Stage B exporter /
`triomics_b07_write_provenance()`. Stage B is an explicit preceding execution;
Stage C never runs it, fabricates its bytes, or reconstructs legacy evidence.
The shared C-06 fixture must call the native B producer, replacing fabricated
B results/nominations. The public ticket may use the existing supported effect
unit vocabulary without changing its scientific meaning or source matrices.

Bind four real files by full SHA-256 of their persisted bytes: B handoff payload,
B handoff envelope, B provenance payload, B provenance envelope. Resolve all
four within that same root; reject traversal, symlink escape, foreign, ambiguous,
missing, inconsistent or tampered frames. Validate payload/envelope pairings
and B provenance with existing validators, not presence checks. Verify config,
study, analysis, preparation, representation, source/member and material C-input
identity against the native evidence; co-location alone is not same-run proof.
Do not rewrite producer run labels to force equality or modify any B bytes.
If the existing evidence cannot establish the link, stop instead of claiming it.

## C-06 publication contract

Retain M7-011's C profile semantics: producer_stage C, the scientific C/D
handoff as handoff_artifact_id, correctly scoped integration_result and
integration_state membership, evaluation_evidence and native declarations;
no B-only result/family roles or self-reference to the current provenance frame.
Bind the four B predecessors and exact C/D members without inventing ancestry.

Publish `D/stage-handoff-provenance.json` and
`D/stage-handoff-provenance.envelope.json` alongside the real D handoff.
Revalidate persisted bytes with `triomics_validate_stage_handoff_provenance()`
and the envelope validator, checking every recorded member digest. Missing B
on a C-06 request is a structured failure, never endpoint-only success. Validate
preconditions before D publication. No partial C provenance pair or misleading
complete D handoff may remain after failure. Preserve pre-existing immutable
artifacts; refuse overwrite, and do not claim reusable output without complete
byte validation. Process-failure atomicity under one pipeline writer is required;
no crash/power-loss durability or concurrent-writer guarantee is added.

C-06 accepted stays false. C-07 controls stay not_run; C-09 remains unqualified,
with no clinical benefit, endpoint, deployment or uncertainty claim. Preserve
partial scientific status, comparison/variance/nomination/privacy semantics.
Provenance schema, D consumer, frozen spec/hash, B-T04 bound 0.08 and seeds
1001:1400 are unchanged. No private study data or GitHub push.

## Audit, implementation and verification

Freeze this record by commit and SHA-256; one fresh read-only `gpt-6-astra`,
effort low, audits it. One documentation repair and one focused rereview maximum.
No implementation while audit is open or before audit_pass yes.
Then one fresh `gpt-6-luna`, effort max, in a new isolated worktree, no commits,
no child agents, may edit only:

- `R/export_c.R`
- `R/cli.R`, only choosing endpoint-only versus B-backed provenance publication
- `tests/testthat/test-C-06.R`
- `tests/testthat/test-C-06-shared.R`
- `tests/testthat/test-C-08-shared.R`, additive assertions only
- `inst/examples/tickets/C-06.yaml`
- `docs/implementation/registrations/C-06.json`, accepted=false

Any other required file stops the worker with evidence. No schema, B code,
D consumer, namespace, package metadata or manual changes are authorized.
Write failing behavioral tests before implementation. Preserve existing
assertions except replacing fabricated C-06 predecessor setup with native B.
Test endpoint-only fresh-root success/no B execution/no D completion/no C frame;
C-06 missing-B rejection/no partial frame; actual native B-to-C success and four
matching digests; tampering, identity conflict, path escape, immutable artifacts
and failure cleanup. Existing C-08 assertions cannot be weakened or removed.

Sol inspects the actual diff and independently runs:

```sh
Rscript --vanilla -e "testthat::test_local(filter='^C-08-shared$', stop_on_failure=TRUE)"
Rscript --vanilla -e "testthat::test_local(filter='C-06', stop_on_failure=TRUE)"
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage B --output <fresh-root>
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage C --output <fresh-root>
```

Capture four native B hashes before C, verify unchanged afterward and equal to
the C frame's predecessor digests. Independently read/validate the resulting C
payload/envelope and record paths and hashes. Shell exit zero is insufficient.
Required failures/skips or missing read-back prevent integration. Record counts
and named warnings without suppression. Integrate only after these checks;
no full source/package or scientific-validation claim follows from this scope.

Stop with record/integration commit, Astra verdict, files changed, both test
counts and provenance paths/full hashes (or explicitly none written). Keep M7,
G7 and Stage D closed. Save the ledger and literal handover with no active workers.
