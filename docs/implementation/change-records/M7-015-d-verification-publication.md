# M7-015 — D verification and generation publication

Date: 2026-10-27. Base: `4d40f3c`. Authority: Omar's current phase-3 instruction;
accepted M7-002 as amended by M7-003 and integrated contract repairs.
Status: frozen for one fresh Astra-low preimplementation audit; no implementation.

## Outcome and limits

D-T01 consumes the same-run native B and C provenance frames and publishes one
complete immutable D generation. Every route is accounted for as enabled or
disabled; completeness of publication does not assert scientific completeness.
The four C-07 controls stay not_run; C-09 stays unqualified; C-06 accepted=false.
M7 and G7 stay closed until integration, passing B-T07/C-06/D-T01 suites and a
fresh immutable-snapshot Astra acceptance review. No D-T02, annotation execution,
target investigation, report, private study data or GitHub push is authorized.
M7-014 is preserved; M7-011/012/013 are not restarted or completed.

## Required consumer behavior

Use the accepted M7-002 sections 3, 5 and 6 with its accepted schema amendments.
Validate closed profiles, canonical identities, every typed entity and inventory,
B/C scope and same-root predecessor relationships before route computation or
publication. Bind exact payload AND envelope bytes; recomputing an envelope's
hash does not establish it as an accepted producer predecessor. Reject changed
measurement/current consumed metadata/registry/design identities, missing lineage,
unsafe ancestry, escaping or dangling symlinks, and envelope identity conflicts.
Never reconstruct missing producer evidence or refit B/C science.

Object key order is semantically invariant under the accepted RFC 8785 contract;
ordered arrays remain order-sensitive. Exact artifact byte references remain
byte-sensitive: reserializing a bound predecessor must fail unless the actual
producer regenerates consistent lineage. Thus a key-order discrepancy cannot
bypass byte verification, while equivalent structured object values must not
cause a false semantic mismatch. This applies the accepted rule rather than
introducing a new serialization or statistical policy.

Hold the single-writer lock from path preflight through pointer commit. Prepare
and validate all generation members in an exclusive temporary directory, seal
the complete closed inventory, publish by manifest SHA-256, then atomically
replace the small current-generation pointer last. All precommit failures leave
the old pointer and generation byte-identical. Ambiguous postrename outcomes
must read back and validate the current complete generation or report
D_PUBLICATION_STATUS_UNKNOWN; never claim rollback without proof. Preserve prior
generations. No hostile-concurrency or OS/power-failure durability is claimed.

## Exclusive worker boundary

After audit_pass yes and a viable in-scope entry point, one fresh gpt-6-luna at
max effort may edit only:

- R/evidence_bundle.R
- R/annotation_router.R
- R/resource_manifest.R
- tests/testthat/test-D-T01.R
- inst/examples/tickets/D-T01.yaml
- docs/implementation/registrations/D-T01.json

The worker does not commit, spawn agents, accept tickets, edit the ledger or
change shared registration. At most two targeted repairs follow reproduced
failures. D's fixture may receive the already diagnosed per_unit_beta and
complete named contrast-vector corrections within its own allowlist.
R/cli.R, R/stage_registry.R, NAMESPACE, manuals, schema, B/C product code,
statistical methods, numerical expectations, B-T04's 0.08 bound and seeds
1001:1400 are outside scope. If publication needs shared files, stop and record
that seam; no monkey-patching, rebinding a shared function from a D file, or
source-time enabling of Stage D is permitted.

## Preflight finding requiring audit disposition

At base 4d40f3c, R/stage_registry.R declares D execution=false.
R/cli.R:1919 calls triomics_require_stage before dispatch; its execution branches
at lines 1929–1941 implement only A, B and C. No Stage D executor is wired.
D-T01.json already records A_STAGE_UNAVAILABLE, disabled CLI registration and
Sol-owned registration requirements. The existing D-owned functions provide a
reader, router and payload writer, but cannot make the actual stage-D request
reach them without a shared-file change. The older rejected shared candidates
are not current implementation authority and must not be imported.

The current instruction explicitly says to stop and record a shared-file seam
when needed to finish publication. Proposed disposition: blocked before Luna
implementation, rather than claim a direct internal-function smoke is completion
of the public Stage D phase. Astra must assess whether this is a real required
boundary or whether an existing supported in-scope public publication path
satisfies the requested end-to-end phase. Audit approval of this stop disposition
is not implementation acceptance. Do not spend a worker attempt on a known
unreachable CLI or pretend a disabled stage is implemented.

## Verification if an in-scope path exists

Sol reviews the returned six-file diff. Execute native B then C on a fresh
public synthetic root, then the real D publication entry point. Read back B/C
profiles, exact predecessor hashes, the D manifest and every generation member,
and the current pointer. Check all enabled/disabled route dispositions and
preserved C-07/C-09 limits. Test changed measurements, escaping and dangling
symlinks, rehashed envelope conflicts, semantic key-order equivalence and exact
bound-byte tampering. Inject failures throughout prepare/seal/publish/pointer
steps and prove old pointer and generation hashes unchanged before commit;
exercise ambiguous acknowledgement recovery after commit.

Run B-T07, C-06 and D-T01 without suppressing warnings or skipping required tests.
Record exact commands, counts, paths and SHA-256 values. Integrate only after
successful real B-to-C-to-D read-back. Freeze the resulting snapshot for a fresh
Astra-low acceptance review; the preimplementation audit cannot substitute for
that review. Sol alone records any acceptance and release; downstream work stays
closed in this run. If the shared seam blocks execution, record no new GREEN
counts and no fabricated D generation or hashes.
