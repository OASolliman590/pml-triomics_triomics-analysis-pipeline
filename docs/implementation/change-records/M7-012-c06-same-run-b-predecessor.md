# M7-012 — C-06 same-run B predecessor supply

- Date: 2026-10-26
- Status: frozen candidate for fresh Astra-low audit; implementation closed
- Base: `71d74ff` (M7-011 blocker receipt)
- Authority: Omar's bounded M7-012 continuation; this does not complete M7-011
- Prior definition: M7-011; governing profile: accepted M7-002/M7-003 and repairs
- Evidence: `reviews/2026-10-26-m7-011-predecessor-blocker.md`
- Documentation repair 1: initial audit `db96c5e` returned changes_required
  (Important 1) for the shared C-08 caller compatibility boundary below.

## Defect and scope

M7-011's old C-06 suite passed 290 expectations, while its exact Stage C command
wrote the D handoff without any B directory or provenance frame. The shared
fixture manually wrote B block-results and nominations. Those bytes do not
constitute the required native B predecessor. Preserve that RED evidence.

This continuation supplies the real predecessor through the already integrated
Stage B producer, then implements the C profile defined by M7-011. It changes
neither the provenance schema nor the D consumer. It is public synthetic
software work, not a new statistical method or a real-study qualification.

## Producer ownership and ordering

1. Allocate a fresh output root for this run. Execute the existing Stage B
   producer using the same C-06 ticket, study, analysis, preparation and
   representation that Stage C will use. Its normal B export must call
   `triomics_b07_write_provenance()` and persist the real B handoff payload,
   handoff envelope, provenance payload and provenance envelope under this root.
2. Execute the ordinary Stage C command against that **same** root. Here
   "fresh output directory" means a new B-to-C run directory, not a directory
   emptied between stages. B preparation is an explicit preceding command;
   `--stage C` does not secretly refit B or synthesize a B handoff.
3. `R/cli.R` may only pass the existing same-run B predecessor paths into
   `triomics_c06_write_handoff()`. Any path resolution added there is limited
   to selecting the unique applicable existing B frame within this output
   root. It must not invoke a new producer or change stage orchestration.
4. The C writer validates and reads those files, then binds their actual bytes
   by full SHA-256. It must never invent, reconstruct, copy in a foreign frame,
   repair, or overwrite B evidence. No private fit object is serialized.

The ticket may be corrected only as needed to exercise the existing B producer
on the same public fixture with the declared scientific meaning preserved
(for example, use the already supported continuous effect-unit vocabulary).
No source data file, B method, numerical threshold or generator is changed.

## Same-run proof, not location alone

All four predecessor files must resolve inside the exact output root, without
path traversal or symlink escape. There must be one applicable complete native
B frame; ambiguity, absence or partial/inconsistent evidence fails closed.
Co-location alone is insufficient: verify the effective configuration digest,
study, analysis, preparation, representation and producer identities across
the B bundle, its envelope, profile and profile envelope. Validate both B
envelopes and the B profile using the existing public validators, including
exact payload hashes and the profile's binding to the actual B handoff.

The B-to-C link must also agree with the material C inputs actually consumed:
ordered matrix/unit/feature identities and the source/preparation evidence
available in the existing C handoff and native sidecars. A rehashed but
context-conflicting frame, or changed B source/member bytes, is not same-run
proof. Use existing native B read-back/reuse validation where applicable;
do not replace it with four pathname checks. If the allowed seam lacks the
evidence necessary to prove this, stop rather than assert compatibility.
Stage-specific activity/run labels need not be artificially rewritten to match;
record their real producer identities and the explicit predecessor relationship.

## C profile and publication

Retain M7-011's profile obligations: producer_stage C; the current scientific
C/D handoff as `handoff_artifact_id`; matching integration_result/integration_state
scopes and membership; evaluation_evidence; exact B predecessor payload/envelope
pairs including the B provenance frame; no B-only result/family roles in the C
inventory; and truthful native-route declarations. Do not include the current
C provenance payload or envelope in its own identity inventory or activities.

Write `D/stage-handoff-provenance.json` and
`D/stage-handoff-provenance.envelope.json` beside the existing D handoff. Verify
the in-memory profile and revalidate the bytes read from disk using
`triomics_validate_stage_handoff_provenance()` with the current frame IDs.
Validate its envelope and all recorded SHA-256 bindings. A shell exit of zero
alone does not qualify this output.

Missing or invalid predecessors must fail closed before publishing a C frame;
no misleading complete/reusable frame or half pair may remain. Existing
provenance files are immutable: refuse overwrite; any allowed reuse requires
complete byte revalidation. Record structured errors, not silent fallback.
Preserve B bytes throughout C execution and failure paths.

The four C-07 controls stay explicitly `not_run`; preserve partial status and
existing numerical/variance/nomination/privacy semantics. C-09 remains
unqualified, with no invented endpoint, deployment or uncertainty protocol and
no claim of benefit. A successful native profile does not accept a milestone.

## Fixture and verification requirements

### Known compatibility blocker and mandatory stop

The shared writer is called by ordinary Stage C at `R/cli.R:1708`. Existing
accepted C-08 shared tests at `test-C-08-shared.R:486–490` and `:539–544` expect
successful fresh-root C execution without a preceding B run. Mandatory native
B validation at that writer can break those callers. This is a known unresolved
dependency, not presumed compatibility and not permission to retrofit them.

In addition to C-06, the complete C-08 shared test file and B-T07 suite are
mandatory non-regression gates. Those tests may be run but not edited. If
preserving those callers requires changing their fixtures, stage orchestration,
any other file or any assertion outside the six-file scope, the packet stops
blocked without integration. A C-06-only green result is insufficient. There
is no authorized missing-B bypass, route-specific silent fallback or weakening
of the required C frame. The worker must assess this boundary before product
edits; if it cannot be satisfied, return bounded evidence rather than spend an
implementation repair. A passed proposal audit approves this stop disposition,
not a finding that implementation feasibility has been established.

The shared C-06 fixture must invoke the native B producer, not manually seed B
results, nominations, envelopes or provenance. Assertions must inspect the
resulting real artifacts. Existing behavioral requirements are retained; do
not weaken an assertion to make the predecessor check green.

Required RED/GREEN tests cover successful native B-to-C binding; missing B
files; changed payload/envelope/provenance bytes; conflicting config/context;
foreign or ambiguous frames; C-input identity mismatch; path escape; immutable
B bytes; missing/changed C members; and frame overwrite/partial-publication
refusal. Exercise the C writer and public command, not only a new helper.
Do not claim unrelated Stage C routes are newly qualified. If required
compatibility work needs files outside this allowlist, stop with evidence.

Sol independently reviews the dirty diff and reruns:

```sh
Rscript --vanilla -e "testthat::test_local(filter='C-06', stop_on_failure=TRUE)"
Rscript --vanilla -e "testthat::test_local(filter='^C-08-shared$', stop_on_failure=TRUE)"
Rscript --vanilla -e "testthat::test_local(filter='^B-T07$', stop_on_failure=TRUE)"
# Allocate a new output root; substitute that same path for <fresh-root> below.
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage B --output <fresh-root>
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage C --output <fresh-root>
```

Capture B file SHA-256 values before C; after C, require unchanged values and
matching predecessor entity digests in the persisted validated C profile.
Record exact testthat counts, named warnings/skips, artifact paths, all four B
hashes and both C provenance hashes. Failed or skipped required checks prevent
integration. No full-suite or clean-package pass is inferred from this filter.

## Exclusive implementation allowlist and attempt limit

Only after `audit_pass: yes`, dispatch one fresh `gpt-6-luna`, effort `max`,
worker in a new worktree. The worker does not commit and may edit only:

- `R/export_c.R`
- `R/cli.R`, limited to passing existing same-run B predecessor paths as above
- `tests/testthat/test-C-06.R`
- `tests/testthat/test-C-06-shared.R`
- `inst/examples/tickets/C-06.yaml`
- `docs/implementation/registrations/C-06.json`, keeping `accepted=false`

No other file is authorized. In particular B code, schemas,
`R/provenance_contract.R`, D code, registration/namespace/package metadata,
lock files, frozen `discovery/spec-v2`, and the frozen hash remain unchanged.
Luna stops if a real B frame cannot be supplied/bound within this scope.
No second implementation repair is authorized. Sol alone may integrate after
diff review and the exact rerun/read-back evidence above passes.

One fresh read-only `gpt-6-astra`, effort `low`, audits this immutable record.
If changes_required, allow one documentation repair and one focused rereview.
No implementation while either audit is open or before explicit audit_pass yes.
No M7 acceptance, G7 release, Stage D implementation, private-data access,
warning suppression, B-T04 bound/seed change, or GitHub push.
