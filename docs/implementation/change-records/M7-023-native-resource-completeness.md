# M7-023 — native optional/required resource status verification

Frozen design scope only; no D05 implementation until D04 verifies and this
scope passes Astra. A read-only scope audit may run earlier, without granting
dependency acceptance. New bounded cycle under 3 November authority, no historical
attempt counter reset.
M7/G7 closed. Existing D05 function tests do not qualify the native publication.

Verify public commands distinguish optional unavailable resources (complete run,
explicit unavailable reason, exit0) from required unavailable resources (incomplete,
exit2). Requests disabled/optional/required, route execution and interpretation
remain separate. Completed B/C evidence and their hashes survive every case.
Conditional D06/D07/D14 remain blocked capabilities without completion credit.

Resource errors must retain exact reason and qualification: missing local bytes,
wrong content hash, changed/absent rights, stale cache and unsupported native model
are not negative biological findings. Do not consume unverified bytes or reuse
an output whose resource content/rights context changed. Known resource access
failures may be translated at the D annotation boundary to typed unavailable
route results with the request's optionality; never swallow source/provenance,
identity, publication-integrity errors or unexpected programming errors. Invalid
predecessors always fail closed and preserve the previous published generation.

Fresh Luna allowlist: R/annotation_status.R, R/annotation_apply.R (resource/status
boundary only), R/resource_manifest.R only if a reproduced rights/cache defect
requires it; tests/testthat/test-D-T05.R and new test-D-status-shared.R;
inst/examples/tickets/D-T05.yaml; public synthetic inst/examples/data/d-status/;
docs/implementation/registrations/D-T05.json (accepted false).
Sol shared proposals only R/cli.R existing status/exit/publication seam, matching
namespace/manual. No schema/method/statistical/source-identity change. No other
file edits, private data, dependency/lock changes, conditional adapters or push.

Add failing native tests before repair. Fresh native B/C/D same-root matrix:
all-disabled, optional missing, required missing, optional/required bad hash,
rights changed/denied, partial successful mapping plus unavailable route. Required
unavailability must expose the existing route_status.run_completeness=incomplete
in serialized output as well as exit2;
optional failure preserves successful outputs and explains missing work. Source
clean on each native command. Verify no untrusted cached result becomes complete,
full route accounting, hashes and old-pointer preservation on true rejection. Preserve D01's structural accounting guard: result.status and
generation completeness describe fully written/accounted artifacts, while
route_status.run_completeness describes whether required requested work is
available. Do not loosen the D publisher or conflate a valid incomplete-run
status publication with an invalid/partially written generation. R/cli.R currently
removes route_status before publishing; Sol must retain truthful completeness
evidence through that existing payload seam.
Run D05+shared and D01/02/03/04 regressions, name warnings and require no skipped
required checks. Sol reads back exact published payloads and hashes. Initial plus
two repairs, scope audit plus one focused rereview. M7 acceptance still requires
all remaining packets and final same-snapshot source/archive/Astra gates.
