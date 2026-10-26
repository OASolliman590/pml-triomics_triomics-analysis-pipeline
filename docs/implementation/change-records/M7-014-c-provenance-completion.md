# M7-014 — authorized C provenance seam completion

Date: 2026-10-26. Base receipt `749cd79`. This run is a fresh implementation
continuation, not completion or reopening of M7-011/M7-012/M7-013.
User explicitly authorizes finishing the previously audited publication split.
Reuse Astra PASS at `4e5ad08`; do not repeat that audit.

The governing split is M7-013: standalone C-08 endpoints/request scheduling
without B execution or C provenance, versus C-06 D publication requiring native
same-root B evidence. Bind the B payload, envelope, provenance payload and
provenance envelope by full SHA-256. Missing B fails closed without a partial
C frame. Preserve scientific semantics, exact-byte validation and identity rules.

Additional explicit scope resolves the diagnosed fixture blockers:
- C-02.yaml analysis preparation_context_id c01_fixture -> c02_fixture, matching
  both existing no-op block preprocessing contexts. B validation remains intact.
- C-06.yaml effect_unit slope_per_score_unit -> per_unit_beta, matching the
  existing continuous coefficient vocabulary, with no numerical method change.

One fresh Luna-max worker, no commits, new worktree. Initial allowlist:
R/export_c.R; R/cli.R (Stage C publication caller); tests/testthat/test-C-06.R;
tests/testthat/test-C-06-shared.R; tests/testthat/test-C-08-shared.R (additive
assertions); inst/examples/tickets/C-02.yaml; inst/examples/tickets/C-06.yaml;
docs/implementation/registrations/C-06.json (accepted=false).
Sol may extend to another existing fixture/ticket/test only when a reproduced
failure demonstrates the same stale preparation identity or old effect label.
Sol records the exact extra file and reason in the ledger before its edit.
Fixture labels, ticket labels, C writer and Stage C caller decisions do not
require another user decision. Do not treat the prior RED patch as finished work.

Sol reviews the actual diff and may return at most two reproduced-failure
repairs to this same new worker. No schema, Stage D, statistical-method,
numerical-expectation, B-T04 0.08 bound or seeds 1001:1400 changes. Such a required
change stops the run. C-07 not_run; C-09 unqualified; M7/G7/Stage D closed.

Independent integration proofs: native C-02 Stage B exit0 with validated B
provenance pair; passing C-08-shared and fresh endpoint-only root with no C
provenance; passing C-06 and actual same-root B-to-C byte read-back with four
matching predecessor digests. Quote counts, paths and full SHA-256. Exit0 alone
is insufficient. Integrate only after these proofs; update ledger/handover and
stop with one receipt. No private data or push. No full-suite/package or
scientific-validation claim from these focused proofs.
