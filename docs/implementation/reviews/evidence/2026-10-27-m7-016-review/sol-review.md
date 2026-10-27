# M7-016 frozen-candidate task review

Candidate: `ac98db3191bf8a5854144f5e017329038e9170e4`; base: `92351d6`.
Read-only product review, 2026-10-27. No repository edit, commit, agent dispatch,
private-data access, integration, or acceptance decision. Reproducer scripts,
logs, and synthetic copies were written only under `/tmp` / `/private/tmp`.

**Spec verdict: changes required. Critical 0, Important 4, Minor 0.**
**Code-quality verdict: changes required. Critical 0, Important 3, Minor 0.**
The first three findings are also correctness/quality findings; the fourth is
an acceptance-evidence finding. These are separate counts by axis, not seven
distinct defects. This review grants no acceptance authority.

## Review basis and scope

Read `discovery/spec-v2/README.md` first; then M7-016, inherited M7-015,
M7-002 sections 3/5/6, relevant M7-003 schema amendments, the actual diff and
current source, producer validation seams, focused D tests, and the worker
report. Applied the code-review skill's separate spec and standards axes
directly because this worker is prohibited from spawning child agents.
`AGENTS.md` supplies repository standards; no separate coding-standards or
contributing file appeared in the bounded standards-file inventory.

The pinned change consists of exactly nine files, all within the ten-file
M7-016 allowlist. `R/resource_manifest.R` is untouched. Shared changes are
limited to the authorized D executor/dispatch, D execution enablement, exports,
and corresponding manual entries. Existing B/C product files, schemas,
statistical methods, and numerical expectations are untouched. Registration
continues to defer acceptance/integration gates to Sol. The CLI avoids generic
registry rollover, and the normal publisher commits through a final current
pointer rename while retaining prior generations. C-06/C-07/C-09 limitations
are represented explicitly.

No affected or full suite was rerun here: the coordinator owns those runs. The
worker's 208 passing expectations are reported evidence, not an independent
test result from this review. Two small bounded reproducer scripts tested the
suspect boundaries against the candidate's real functions and the retained
public synthetic native B/C/D root.

## Spec findings

### 1. Important — C native-sidecar ancestry can escape the same-root boundary

Location: `R/evidence_bundle.R:623-641`, especially leaf-only check at 629 and
`normalizePath()` at 640; subsequent reads/validation at 642-664.

M7-016 task 1 requires same-root native proofs and rejects unsafe ancestry
before routing/publication. M7-015 explicitly includes escaping symlinks;
M7-002 section 3 requires complete path preflight. This code enumerates
`root/C/native-states`, checks `triomics_is_symlink(path)` only on each JSON
leaf, and then canonicalizes the accepted path. A symlink on the directory
ancestor is followed during enumeration and erased by canonicalization. The
later profile-artifact check verifies bytes but does not recover the original
unsafe ancestry or constrain the resolved native file to the same root.

Reproduction: copy the already generated public synthetic `native-debug` root;
verify baseline native consumption; move `C/native-states` to a sibling under
`/private/tmp`; replace it with a symlink to that sibling; call both the native
D reader and the actual D executor. With all predecessor bytes unchanged,
the reader returns `evidence_bundle` and the executor returns `complete`.
This is a static filesystem hazard, without concurrency or hostile mutation.

Evidence: `/tmp/triomics-m7-016/sol-probe-native-ancestry.R` and
`/tmp/triomics-m7-016/sol-probe-native-ancestry.log`; command exited 0.

Necessary fix: validate the original `native-states` ancestry and each original
payload/envelope path with the existing D secure-path/same-root boundary before
enumeration, normalization, or reading. Preserve `D_OUTPUT_PATH_UNSAFE` for
linked ancestry and add native-directory, intermediate-ancestor, envelope-leaf,
and dangling-link behavioral negatives before publication.

### 2. Important — Newly exported routing API bypasses required producer proof

Location: `R/annotation_router.R:1115-1135`; export introduced in
`NAMESPACE:28-29`; matching claim in `man/triomics-api.Rd:229-233`.

M7-002 section 3 and M7-016 require producer provenance verification before
route computation. The public API documentation now says these exports require
persisted producer provenance binding exact B/C payload and envelope bytes.
The public guard still checks only old bundle/member hash-shaped strings. It
does not require either profile reference, a verified publication context, or
any persisted file. It then invokes the router directly.

Reproduction: construct a minimal `evidence_bundle` using the real published
payload's study/analysis and immutable references; remove all six B/C
profile-reference fields and omit `publication_context`. Calling
`triomics_annotate_evidence(minimal)` returns `status=complete`. There is no
filesystem proof or native-provenance readback at this entry point. The
publisher later requiring a context does not satisfy the earlier prohibition
on routing unproven evidence.

Evidence: `/tmp/triomics-m7-016/sol-probe-readback.R` and `.log`, output
`profileless_exported_api=complete` (exit 0).

Necessary fix: make the exported entry validate the native same-root proof
boundary before routing, using a required verified context and persisted-byte
readback (or route the public entry through the verified consumer). Keep pure
in-memory routing helpers internal. Add a direct exported-API negative for
profile-less/context-less input, not merely a negative on the reader helper.

### 3. Important — Generation readback accepts semantic manifest conflicts

Location: `R/annotation_router.R:443-470`, `496-523`, `547-569`.

M7-002 section 5 requires every manifest member's type/status/digest and all
cross-references to validate, and ambiguity recovery must establish a complete
generation. The current validator checks that member `artifact_type` and
`scientific_role` are nonempty, but never compares the payload member's values
to the actual payload. The manifest's B/C provenance hashes are checked only
for shape; they are not reconciled with the payload's immutable inputs or the
envelope's input descriptors. Thus a canonical, internally byte-sealed manifest
can advertise the wrong scientific inventory/provenance while readback calls
it complete.

Reproduction: copy a real published generation, change the payload member type
and role to `wrong_type`/`wrong_role`, replace the manifest B profile payload
digest with 64 zeros, canonicalize it, and name the directory with its new full
manifest digest. Keep the actual routing payload/envelope unchanged. Calling
`triomics_d_validate_generation(root, new_digest)` returns manifest status
`complete`. This is not a hash mismatch: it exposes the missing semantic
cross-checks that the contract requires in addition to byte verification.

Evidence: `/tmp/triomics-m7-016/sol-probe-readback.R` and `.log`, output
`wrong_member_type_and_conflicting_provenance_readback=complete` (exit 0).

Necessary fix: enforce the exact payload member type/role and allowed pairing
fields, reconcile every manifest provenance descriptor against the routing
payload and envelope inputs, and verify required payload identities/status
cross-references before treating current-generation recovery as complete.
Add negatives where all outer manifest hashes are recomputed consistently.

### 4. Important — Required D-boundary behavioral acceptance evidence is incomplete

Location: `tests/testthat/test-D-T01.R:945-957`, `1006-1009`, `1125-1151`;
the sole native consumer/publication block is 883-1168.

M7-016 task 2 expressly requires all accepted M7-002 section 6 probes and permits
reuse of foundation tests only where they already prove the invariant. Existing
foundation tests cover JCS/raw JSON/schema/graph mechanics, and B-T07 has legal
`model_hash` contrast and unavailable-DEqMS producer tests. Those do not execute
the native D consumer for the required scientific/path/recovery cases.

The new native block replaces a source hash in an already constructed manifest,
changes registry quantity and replaces an entire metadata object. It does not
change actual measurement bytes and invoke D ingestion, nor exercise consumed
visit/time, subject/unit mapping, paired sign, repeated-time origin, persisted
represented/prepared matrices/state, or successful native DEqMS followed by
count/prior mutation at D. The only native publication path probe is a symlink
output root. Generation parent, registry, current pointer, temporary generation,
planned leaf, intermediate ancestry and dangling symlinks are absent at the
publication boundary. Failure-point loops cover ordinary raised errors and one
successful postrename recovery, but not failed/ambiguous readback returning
`D_PUBLICATION_STATUS_UNKNOWN` or a proven old-current recovery. Finding 1 is a
concrete example of a defect the omitted ancestry coverage would catch.

Necessary fix: supply named, executable D-boundary behavioral probes for the
missing requirements, with exact expected D classes and old-pointer/generation
byte preservation. Existing foundation evidence can be linked for serialization
and schema invariants, but should not be counted as native D scientific-lineage
or publication-path evidence. For any legacy/regeneration requirement whose
evidence belongs to an earlier accepted phase, name that exact retained receipt
and prove it applies rather than counting unrelated suite totals.

## Code-quality findings

1. **Important — path validation after normalization**: finding 1. The native
   sidecar branch uses a separate leaf-only path-validation pattern instead of
   the existing ancestry-aware D boundary. This permits a real executor success
   on unsafe source ancestry. Validate original paths before resolving them.
2. **Important — inconsistent public validation boundary**: finding 2. The
   exported API accepts proof-less objects while its reader and publisher use
   different stronger guards. Make the public entry enforce the required native
   boundary and retain pure route-model helpers internally.
3. **Important — byte integrity mistaken for semantic manifest validity**:
   finding 3. The readback validator verifies nonempty metadata and digests but
   does not establish type/provenance agreement. Centralize the exact expected
   member and provenance mapping and use it during publication and recovery.

No additional style-only or speculative smell finding is raised. Dead temporary
variables or formatting choices are not acceptance blockers here.

## Evidence limits and next gate

The two probe commands exited successfully because their observed result was
the erroneous acceptance; they are reproducible counterexamples, not passing
acceptance tests. Synthetic copies and original retained native data remain
local. Source/installed full-suite and archive checks remain the coordinator's
gates. Passing those suites cannot close the counterexamples without targeted
repairs/tests. A repaired immutable candidate needs the authorized focused
review before integration, followed by the separately required fresh Astra
milestone audit; this receipt itself does not accept D-T01, C-06, M7, or G7.
