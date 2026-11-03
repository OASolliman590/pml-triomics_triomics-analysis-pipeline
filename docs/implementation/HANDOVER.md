# Implementation handover

## Active 3 November 2026 — M7 completion from b03cdef

User authorizes new bounded cycles for remaining required Stage D/M7 work.
Coordinator branch codex/m7-021-stage-d-closure begins at exact b03cdef.
Original draft/stage-d-wire and spec/harden-v2-20260920 are preserved.
M7/G7 stay closed. Earlier caps/receipts remain historical, never reset.

M7-021 reconciles records and verifies C06/B07/D01, then completes native local
mapping packet evidence; D05 follows verified D04. Subsequent separately scoped cycles cover ORA,
general target profiles and D12 discovery; each needs Astra scope approval and
one fresh Luna-max worker, initial plus at most two repairs. Sol owns shared CLI,
namespace/manual integration via reviewed proposals. All consumers await their
verified dependencies. Final same-snapshot receipts, full source, clean archive
Status: OK, Terra DOC-M7 and fresh Astra pass precede Sol acceptance.

Current MOFA2/mofapy2 temporary runtime is incomplete and needs restoration before
full gates. No private data, AIU run, fasting checks or GitHub push in this scope.
Resume: audit M7-021 record, verify predecessor suites from clean committed source,
then dispatch Luna. Do not act on the stale historical next-action text below.

## Current checkpoint — 29 October 2026, D-T11 stopped at the repair limit

D-T11 is not landed. Terra used the initial attempt and both repairs. The focused
suite on the uncommitted candidate passed 42. Opus 5.5 rereview of repair 2
returned audit_pass yes. Astra high returned audit_pass no. An observation
endpoint `binding_probability` is still accepted and becomes a measured axis.
The candidate remains uncommitted in worktree
`/Users/megastore/.codex/worktrees/d-t11-profiles` on branch `draft/d-t11`.
Do not start repair 3 without a new authorization. Do not start D-T12. M7 and
G7 stay closed.

Commit `9e1499d` records a target request and external assay evidence. The focused
suite passed 59, with no failures, warnings, or skips. Astra high and Opus 5.5
rereviews of repair 2 both returned audit_pass yes. A record attaches only when
taxon, context, construct, compound namespace, and compound ID all match. Binding,
function, engagement, and association stay separate. The Stage D CLI call remains
a Sol integration blocker, so `accepted` stays false. M7 and G7 stay closed.
D-T11 is the next required packet.

Commit `62421b1` accounts for optional and required resource failure. The focused
suite passed 39, with no failures, warnings, or skips. Astra high and Opus 5.5
rereviews both returned audit_pass yes. A required route with a stale cache
exits 2 as `D_STATUS_CACHE_INVALID`. An optional stale cache stays complete and
keeps that reason. A missing file stays `D_RESOURCE_MISSING`. The Stage D CLI
call remains a Sol integration blocker, so `accepted` stays false. M7 and G7
stay closed. D-T10 is in repair.

Commit `4b8eb12` adds declared-universe over-representation. The focused suite
passed 38, with no failures, warnings, or skips. Astra high and Opus 5.5
rereviews both returned audit_pass yes. The tail is P(X >= k): N=10, K=3, n=2
gives 1/15 at k=2 and 8/15 at k=1. The Stage D CLI call remains a Sol
integration blocker, so `accepted` stays false. M7 and G7 stay closed. D-T05
is the next required packet.

Commit `1d420e6` adds local gene-product and reaction mapping. The focused
suite passed 53, with no failures, warnings, or skips. Astra high and Opus 5.5
rereviews both returned audit_pass yes. The Stage D CLI call and the pinned
mapping table remain a Sol integration blocker, so `accepted` stays false.

Commit `fcc05b6` adds local lipid mapping. The focused suite passed 71, with
no failures, warnings, or skips. Astra high rereview audit_pass yes. Opus 5.5
found no Critical defect in the mapper. The Stage D CLI call and the pinned
mapping table remain a Sol integration blocker, so `accepted` stays false.

Commit `7ff1d53` binds numeric visit-weight names into the endpoint hash and
keeps explicit `visit_weights: null`. C-08 on that change passed 250, with no
failures, warnings, or skips. A clean worktree of `7ff1d53` then passed D-T01
with FAIL 0, WARN 1, SKIP 0, PASS 325. The one warning is the existing stale
paired-sign rejection.

The repair loop had a mechanical cause. `triomics_source_git_revision()` returns
`unavailable` whenever `git status --porcelain` is non-empty. The main checkout
carries untracked AppleDouble files and dirty `.Rbuildignore`, `.gitignore`, and
`renv/activate.R`. Native Stage B then exits 2 with `B_EXPORT_INCOMPLETE`, and
Stage C and Stage D fail because that B frame was never written. Prove native
B, C, and D only from a clean worktree or archive. Do not start another repair
cycle for that symptom.

## Current M7-019 — blocked close; cap exhausted

Integrated product snapshot98c71256e61c4613833f81bf049de06427c9a094; failed final
candidatee41048693bebdec93d73f4aeb0ec207a81cd9299 preserved, NOT integrated.
Scope Astra passed e6f9b29. Initial plus two repairs used; no repair3.
Repair1 focused2023pass0fail0error1warn0skip; native B/C/D0/0/0 with21assertions,
SYN_12 time:null and verified same-root B/C/D hashes. Full source98c7125:
6287pass0fail0error15warn0skip. Independent coordinated weight-name tamper probe
still fails integrity. Final candidate bindsnames but deletes explicit null keys;
focused1627pass12fail5error7warn0skip. These are separate snapshots.
Clean archive98c7125: Status: 1 ERROR; FAIL24 WARN5 SKIP0 PASS5877. All5 warnings
named in same-run inventory. B-T01 YAML arrays scalarized; C06 sensitive-fixture
path fails; installed C05 runtime unavailable despite supplied isolated paths.
Fresh final Astra audit_pass no, fourP1; source runtime cannot qualify installed.
All workers and package91770 completed. No desktop worker chats created.
M7/G7closed,C06false,C07not_run,C09unqualified; no laterD/private data/push.
Receipt: reviews/2026-10-28-m7-019-blocked-close-receipt.md (closed29October).
Preserve unrelated dirty .Rbuildignore/.gitignore/renv/activate.R and original
6456055/cfe9160 candidates. Detailed immutable evidence retained in reviews/evidence.

Literal resume: authorize a NEW bounded cycle for weight-name plus explicit-null
hashing, typed installed YAML/fixture paths and installed MOFA runtime visibility.
Keep all assertions/validators. Audit scope, fresh bounded worker, independent
focused/native/source/archive gates and fresh Astra. Do not reopen M7-019 repair3.

## Current M7-018 — integrated null fix; gates failed; closed

Repair774bbcc is integrated at d660879. Terra found no scoped findings.
Native B/C/D0/0/0,21 assertions, explicit SYN_12 time:null. Focused1176pass,
0fail,1named expected warning,0skip. Original6456055/cfe9160 unchanged.
One full-source run:6280pass1fail15warn0error0skip. Paired visit_weights list
versus established named numeric API is unresolved. Clean archive d660879:
Status: 1 ERROR, 1 WARNING, 1 NOTE; installed5266pass45fail42warn0skip.
Archive native producer Git-revision/fixture portability failures, Rd usage and
setNames diagnostics remain; installed warning names are not fully enumerated.
Fresh Astra-low audit_pass no: P1 1, P2 2, P3 1. No additional product repair.
All run-owned workers/checks completed; no desktop chats created.
Receipt: reviews/2026-10-28-m7-018-close-receipt.md.
M7/G7closed,C06false,C07not_run,C09unqualified; no laterD/private data/push.
Literal resume: preserve d660879; scope a new bounded cycle for numeric endpoint
API and installed provenance/fixture portability, complete installed warning
inventory and package diagnostics. Do not reopen M7-017 or waive lineage.
Preserve unrelated dirty .Rbuildignore, .gitignore and renv/activate.R.

## Current M7-017 checkpoint — blocked and closed at two-repair cap

Scope audit passed; isolated initialc8b309e, repair17ef25ef and final6456055
are preserved. Original D candidatecfe9160 remains clean/unchanged. No product
integration. Final native B/C/D commands exit0 and publish matching hashes, but
semantic proof FAIL2/PASS19: excluded SYN_12 time is JSON string"NA", not null.
Its source/exclusion and no-score status are correct. Parent independently
reproduced; Sol review one Important unresolved. Full close receipt:
reviews/2026-10-27-m7-017-blocked-close-receipt.md.

Last comprehensive focused gate843pass0fail0error1expectedwarning0skip/53blocks
belongs to repair1 only. Final narrow C08 regression4pass, reviewer8negative
checks; no final comprehensive/source/archive/finalAstra pass inferred.
M7/G7closed,C06false,C07not_run,C09unqualified; downstreamDclosed. No private
inputs/push. No active worker/test process; no run-owned desktop chats created.
Historical AGENTS/CLAUDE frozen hash discrepancy preserved, other90match.

Literal resume: obtain a newly authorized bounded C explicit-null serialization
cycle from6456055; preservecfe9160, retain failing native tests, keep global writer
and statistical/schema guards unchanged. Then independent focused/native gates,
integration only after pass, full source/archive Status: OK and fresh scoped
Astra. Do not open a third M7-017 repair or repeat older producer cycles.

## Current M7-016 checkpoint — blocked, isolated repair preserved

2026-10-27: scope Astra PASS at92351d6. Initial candidateac98db3, repaired
f23b5a7, final test-only repaircfe9160 remain isolated in
`/Users/megastore/.codex/worktrees/m7-016-d-publication/PML MultiOmics Analysis Pipeline`.
No product integration. Sol independently reproduced the three initial defects
and verified their rejection after repair1. Focused reviewer marks those fixed;
final test-bootstrap repair passes narrow rereview. Two repairs used.

Worker D suite:317 success markers, no failure/warning/skip blocks; structured
counts unavailable. Source+installed recovery probes pass seven checks each.
These are not full acceptance gates. Required native paired/trajectory evidence
is blocked before D: fresh B0/C2 C_ELIGIBILITY_UNKNOWN_UNIT (paired), B0/C2
C_SCORE_LINEAGE_INVALID missing AnalysisUnitMap (trajectory). Fixing the C
caller is outside the D-only shared exception. No requirement waived.

Receipt: reviews/2026-10-27-m7-016-blocked-repair-receipt.md and its two durable
evidence directories. No worker/proof process remains active. Final Astra,
full source, archive package gate and final independent B/C/D readback did not
run to completion. No M7/G7 acceptance or release. C06unaccepted, C07not_run,
C09unqualified; all downstream D closed. No private data or push.

Literal resume: preservecfe9160; do not redispatch M7-016 Luna or open a third
repair. Scope/audit the C paired/trajectory unit-map composition seam, then
complete outstanding independent gates and fresh Astra before M7/G7 release.
Do not restart earlier producer cycles or treat old counts as new GREEN.

## Current M7-015 checkpoint — shared integration seam blocked

2026-10-27: Omar authorized D verification/publication, phase3, within six D-owned
files and explicitly required stopping for shared-file needs. Record frozen at
`6fe5f1d`; one fresh Astra-low audit approves the blocker disposition only.
The public D stage is disabled in R/stage_registry.R and has no R/cli.R executor.
Sol's D command exits3 A_STAGE_UNAVAILABLE and creates no output root. The
accepted M7-002 gate requires public B/C/D CLI read-back; an internal-function
smoke cannot substitute. No Luna dispatched, product edits, suite reruns or D
artifacts. Receipt: reviews/2026-10-27-m7-015-d-publication-preflight.md.

Next requires a separately scoped shared executor/registration integration
record before the six-file D implementation can meet its full gate. Do not
reimport rejected shared candidates or bypass the stage guard. M7-014 remains
verified/integrated; M7-011/012/013 incomplete. C06acceptedfalse, C07not_run,
C09unqualified; M7/G7 closed and D downstream closed. No active worker/process.
Literal resume: read the M7-015 receipt, resolve its shared integration scope,
then resume the legal D phase without repeating producer work or this audit.

## Current M7-014 checkpoint — verified and integrated; milestone closed

The C publication seam is integrated at `359a1d0`, with all eight candidate
file blobs matching independently verified snapshot `18b6e90`. Existing Astra
PASS at `4e5ad08` was reused as instructed; no repeat audit. Fresh Luna worker
finished and Sol used no post-return repair slots. No worker or proof process
remains active. Full receipt: `reviews/2026-10-26-m7-014-sol-integration-receipt.md`.

Sol independently verified: native C02 Stage B exit0 with valid provenance;
C08-shared FAIL0/WARN0/SKIP0/ERROR0/PASS64 and a fresh endpoint-only run with
only scheduled B requests, no B results/handoff or C provenance; C06
FAIL0/WARN0/SKIP0/ERROR0/PASS347 and native B-to-C exits0/0 with validated C
profile/envelope and four exact unchanged B predecessor hashes. Missing B exits2
and leaves no C provenance pair or D manifest. Artifact paths/full hashes and
raw logs are in the receipt/evidence directory. No full-source/package pass inferred.

The authorized C02 preparation label, C06 effect/contrast column labels and
C-specific run-C.json resolve the earlier blockers without changing methods,
numerical expectations, schema, D consumer, B-T04 bound or seeds. C metadata is
compared from ingested values to validated B normalized metadata, not claimed
as an independent raw metadata-file rehash. Source/prepared matrices are checked
using their existing distinct hash conventions; C preparation state is bound.

C-06 accepted=false. C-07 remains not_run, C-09 unqualified, D aggregate partial.
M7, G7 and Stage D remain closed. M7-011/M7-012/M7-013 stay incomplete.
No private study data, GitHub push or Notion sync. Retain the proof worktree:
`/Users/megastore/.codex/worktrees/m7-014-c-provenance-completion/PML MultiOmics Analysis Pipeline`.
Earlier checkpoints below are history and do not reopen completed work.

## Current M7-013 checkpoint — blocked, not integrated

M7-013 record `4e5ad08` passed fresh Astra-low audit with zero findings; no
repair/rereview used. Fresh Luna-max `/root/m7_013_luna` finished at `425455b`
with only three dirty test files, no product edits or commits. Worktree retained:
`/Users/megastore/.codex/worktrees/m7-013-c-publication-split/PML MultiOmics Analysis Pipeline`.
No run-owned worker remains active. Sol inspected the patch and independently
reran: C-08-shared FAIL 2 / WARN 0 / SKIP 0 / PASS 60; C-06 FAIL 10 / WARN 0 /
SKIP 0 / ERROR 0 / PASS 106. These are RED diagnostics, not provenance GREEN.

The nested C-02 caller in the C-06 suite requires D artifacts, but native B on
its unchanged ticket fails B_DESIGN_PREPARATION: block context c02_fixture,
analysis context c01_fixture. C-02.yaml/B code are outside the allowlist. No
bypass, relabeling, excluded-file repair or second attempt was performed.
No B/C provenance frame or hash receipt exists. Only documentation/evidence
is integrated. Receipt: `reviews/2026-10-26-m7-013-publication-split-receipt.md`.
C-06 accepted=false; C-07 not_run; C-09 unqualified. M7/G7/Stage D closed.
The completed audit is not proof of feasibility. A new bounded disposition for
the nested caller/fixture identity is needed; preserve M7-011/M7-012 as blocked.

## Current checkpoint — 26 October 2026, M7-011 and M7-012 blocked

M7-012 stopped before implementation. Fresh Luna-max `/root/m7_012_luna`
finished at base `4bdda74` with only a 42-line C-06 registration receipt;
no worker commit or product/test changes. Its worktree is retained at
`/Users/megastore/.codex/worktrees/m7-012-c06-predecessor/PML MultiOmics Analysis Pipeline`.
No active M7-012 worker or audit remains. Sol inspected the returned diff.

The mandatory B-backed shared C handoff conflicts with two existing C-08
standalone Stage C callers. Their unchanged baseline passes FAIL 0 / WARN 0 /
SKIP 0 / PASS 52 (Luna-reported); preserving that contract requires a decision
outside this six-file scope. No new C-06, B-T07 or B-to-C acceptance gate ran,
and no new provenance paths or digests were produced. Receipt:
`reviews/2026-10-26-m7-012-preflight-blocker.md`.

M7-012's sole documentation repair is frozen at `e37f334` (SHA-256
`6cd3447e918272f9cde9dbaabceb6fa829fda4b3c41044779aea0bc72944c0e7`).
The focused Astra-low rereview passed with zero findings: approval of the
explicit stop disposition, not implementation feasibility or acceptance.
Do not redispatch either worker or open a second implementation repair.
M7-011/M7-012 remain incomplete; C-06 accepted=false; M7/G7 and Stage D closed.

M7-011's audit passed, but its Luna-max worker stopped before implementation:
the exact Stage C output lacks the required native B predecessor frame. Sol
inspected the worktree and preserved its sole registration change. The old
C-06 suite passed 290 expectations; that is not new provenance GREEN.
See `reviews/2026-10-26-m7-011-predecessor-blocker.md`. M7-011 is not complete.
Omar authorized M7-012: freeze and audit the bounded same-root B predecessor
supply record before a fresh Luna-max implementation. No active M7-011 worker
remains. C-06 accepted=false; M7, G7 and Stage D remain closed.

M7-010's resumed full source gate remains the prior result: **FAIL 0 / WARN 14 /
SKIP 0 / ERROR 0 / PASS 5893**, 352 test blocks, exit 0 after `DONE`, against
snapshot `2a8e5d5` with product files unchanged from `0afe2c5`. All fourteen
conditions match the previously dispositioned C-05 source warning set. A-T01
passed 61 expectations both alone and inside the full suite; the earlier
interrupted-run diagnostics did not recur and their cause remains unproven.

M7-010 final candidate `bf41f10` passed the scoped Astra-low rereview with
Critical 0, Important 0, Minor 0. Both initial review findings were repaired
and passed the Spec and Standards rereviews. The final structured-error
regression passed 3 expectations; Luna's completed B-T04 suite passed 430,
with zero failures, warnings, skips or errors.

The approved changes are integrated at `0afe2c5`; all five allowlisted file
blobs match `bf41f10` exactly. The integrated focused gate passed B-T04 430,
B-T07 315 and composition 94 (839 total), with no failures/warnings/skips/errors.
The prior interrupted run remains historical, not a passing result. Its receipt
is `reviews/2026-10-26-m7-010-integration-gate.md`. The decisive uninterrupted
source result and named warning inventory are in
`reviews/2026-10-26-m7-010-resumed-source-gate.md` and
`reviews/evidence/2026-10-26-m7-010-resumed/`. This is source-suite verification,
not a new clean package check or milestone acceptance. The separate spec
validator remains blocked by missing Python dependencies; no fresh spec
consistency pass is claimed. Do not repeat completed repairs, reviews or this
source gate merely to recover context.

B-T07 remains `candidate/review_pending`, `accepted=false`. B-T07/M7 acceptance,
G7 and Stage D remain closed. No private study data were read or fitted and no
GitHub push occurred. The historical sections below do not override this
checkpoint or the current execution ledger.

Run `triomics-20260920-sol-01` continues on branch
`spec/harden-v2-20260920`. The clean archive package gate remains the accepted
historical result at implementation snapshot `76fe5e2`: `Status: OK`, FAIL 0,
WARN 22, SKIP 0, PASS 4787. The single focused Astra-low rereview of immutable
commit `2150b4d` passed: `M6-ASTRA-01: ADDRESSED`, with no new blocker.

Sol accepted M6 and released G6 on 2026-10-22. C-06 is accepted on synthetic
software evidence. C-09 remains conditionally deferred without completion
credit: its public route is `synthetic_fixture_only`,
`benefit_guaranteed=false`, and no real endpoint, deployment, uncertainty
protocol, independent validation, biological validity, or clinical benefit is
qualified. C-07 controls remain `not_run`. No private study data were read or
fitted.

Omar explicitly resumed Stage D on 2026-10-22. D-T01 is the only initially legal
packet. Luna-max returned the bounded six-file candidate `98bf4c5` from base
`57be42a`; its exact test suite passed 86 expectations, but the fresh Sol-high
review reproduced seven Important contract defects: lost provenance, discarded
full B evidence, mutable nomination lineage, mismatched preparation identity,
an ancestor-symlink escape, duplicate measured-feature counts, and an
incompatible shared feature-registry boundary. Nothing was integrated.
Bounded Luna repair 1 returned `c857d64`, changing four allowlisted files; its
focused suite passed 131 expectations with no failures, warnings, or skips. The
single focused Sol-high rereview passed all seven findings with no new finding.
The packet commits are integrated as `d2db62d` and `88f1591`. The first shared
Luna worktree predated the current ledger and its host guard
refused Stage D. Both approved OpenCode free models then returned provider-quota
429 before editing. Replacement Luna-max worker `/root/d_t01_shared_luna2` is
paused from current-ledger snapshot `ae6762b` with its exclusive exports,
stage-registry, manual and CLI changes preserved. Its exact B-to-C-to-D gate
reached one real upstream failure: accepted public Stage-B BlockResult rows omit
the frozen contract's required `result_id`. Luna-max repaired that central
public finalization boundary in candidate `05fc87c`; B-T01, B-T02 and B-T07 pass
581, 208 and 216 expectations respectively, and the B-T02 public read-back has
20 unique 64-hex identities. Fresh read-only Sol-high review passed with no
Critical, Important, or Minor finding, and Sol integrated it as `9efdf19`.
Replacement worker `/root/d_t01_shared_luna2` completed the exact B-to-C-to-D
gate with its already-authorized `per_unit_beta` fixture correction. Candidate
`71c0a1b` is clean; its named suite passed 204 expectations and fresh Stage B,
C, and D CLI runs exited 0/0/0. The D rerun refused with
`D_OUTPUT_FINALIZED`, all 41 output hashes remained unchanged, and D left the B
and C trees unchanged. Fresh read-only Sol-high review rejected the candidate
with zero Critical, three Important, and two Minor findings. D can combine
changed current source data with stale B/C results, write run registries outside
the output root through a symlinked `registries` ancestor, and accept rehashed B
envelopes with conflicting artifact/preparation/representation identities. The
structured C rule comparison is also key-order-sensitive, and invalid config
failure can mutate the current registry. D-T01's initial implementation plus
two repairs are consumed, so no third repair is authorized. Fresh Astra-low
auditor `/root/d_t01_blocker_astra` independently confirmed all five findings
and recommended one narrow repair-cap exception. Omar explicitly authorized
M7-001 on 2026-10-23 with `I authorize M7-001.` Fresh isolated worktree
`m7-001-dt01-provenance` is clean at candidate `71c0a1b`; the exact D-T01
baseline exits zero. Luna-max returned evidence/test commit `0d82e9f`; Sol
applied the exclusive CLI lifecycle repair as `1b44a84`. The resulting clean
candidate changes exactly the four M7-001-authorized files. Former RED probes
for changed measurements and consumed metadata, registry symlink escape, B
envelope identity conflicts, structured-key order, and invalid-config
immutability are green. The exact uncapped D-T01 suite passed 268 expectations
across 27 test blocks with FAIL 0, WARN 0, SKIP 0. Fresh read-only Sol-high
reviewer `/root/m7_001_sol_review` rejected the immutable candidate with zero
Critical, five Important, and zero Minor findings. It reproduced unbound
FeatureRegistry drift, paired/repeated consumed-metadata changes that preserve
the guard hash, an unchanged two-group DEqMS producer/consumer hash mismatch, a
valid `model_hash` contrast-ID collision in recursive collection, and a dangling
D-output symlink rejected only after registry rollover. The candidate was not
integrated or accepted. M7-001's single exceptional repair is consumed. D-T01
and every direct or transitive Stage-D successor remain blocked, and no
run-owned agent is active. The rejected M7-001 worktree is archived as a
recoverable snapshot. No private study data were read or delegated.

Omar then explicitly requested a Wayfinder plus primary-source research cycle
to choose a scientifically citable repair before any spec amendment or new
implementation. The planning-only map is
`docs/implementation/investigations/d-t01-provenance-wayfinder/MAP.md`. Both
research tickets are now closed. Their notes recommend a producer-owned,
versioned triomics handoff-provenance profile informed by W3C PROV and Workflow
Run RO-Crate; a closed BagIt-style typed inventory; RFC 8785 canonical JSON and
full SHA-256; and immutable output generations whose current registry pointer
is atomically replaced only after complete validation. D verifies producer
identity and never reconstructs it. No external-standard compliance claim is
made. Omar accepted the domain model on 2026-10-23: a producer-owned triomics
JSON Stage Handoff Provenance, typed Scientific Entity Descriptors, immutable
Output Generations, a small Current Generation Pointer replaced last, and
Process-Failure Atomicity under a Single Pipeline Writer. This deliberately
does not promise protection from hostile concurrent mutation, operating-system
or power failure, or unsupported filesystem behavior. The terms are recorded in
the root glossary and ADR 0003. The open frontier is now `Decide legacy handoff
migration and compatibility`; Omar's binding constraint is that legacy evidence
must be rejected unless immutable producer artifacts prove complete lineage.
The legacy route is now resolved: only complete immutable producer evidence may
be packaged deterministically, with numerical payload bytes unchanged; a missing
proof class fails closed and requires producer regeneration under unchanged
science. Proposed normative change record M7-002 defines the exact JSON profile,
required lineage, legacy route, D preflight, immutable generations, atomic pointer
commit, behavioral matrix, and bounded three-phase implementation route. It does
not rewrite the historical freeze. Implementation is prohibited until a fresh
read-only Astra-low audit passes the immutable M7-002 snapshot. Stage D remains
blocked; M7-001 remains exhausted. No private study data were read.

Fresh read-only Astra-low reviewer `/root/m7_002_astra_audit` audited `81fdf77`
and returned `changes_required`: Critical 0, Important 3, Minor 0. Documentation
repair 1 was frozen as `366f284`. Its single focused rereview also returned
`changes_required`: Critical 0, Important 1, Minor 0. Pointer-linearization and
Sol-only shared-registration ownership are now sound, but the closed provenance
contract still lacks descriptor `block_id`/`method_id` fields required by its
cardinality rules, a closed entity-type/role mapping, and an explicit
self-inventory exclusion for the provenance payload/envelope. M7-002 is blocked
and not accepted; its repair and rereview allowance is exhausted. No product
implementation is authorized, and no run-owned agent is active.

Omar explicitly authorized successor specification record M7-003 on 2026-10-23.
M7-003 changes only the three remaining provenance-schema defects: it gives the
closed descriptor explicit block/method and ordered-identity scope fields,
defines a total role/type/scope/cardinality table, and keeps each provenance
payload and envelope outside its own inventory while allowing a later producer
to bind their bytes as predecessors. The proposed delta, glossary terms, map,
audit brief, and ledger were frozen as `0e128c3`. Fresh read-only Astra-low audit
returned `changes_required`: Critical 0, Important 1, Minor 0. It confirmed the
closed descriptor, role/type mapping, and provenance-frame exclusion but found
that permitted multiple preprocessing states conflicted with a blanket duplicate
rule, while multiple clinical/native routes had no executable route key.
Documentation repair 1 adds a closed declared-route registry and descriptor
`route_id`, exact pair/profile/route mappings, and separates entity uniqueness
from row cardinality. The focused Astra-low rereview of `2ae1b6e` passed:
Critical 0, Important 0, Minor 1. Sol accepted the combined M7-002 plus M7-003
amendment. The Minor is bound to contract-foundation evidence: a native route
whose descriptor `method_id` differs from its declaration must fail
`D_PROVENANCE_INVALID`. Only contract foundation is released. Producer seams,
D verification/publication, migration, shared registration, D-T01 acceptance,
M7 release, and every Stage-D successor remain closed. No private-data access
is authorized.

The released contract-foundation packet ran in the isolated worktree
`/Users/megastore/.codex/worktrees/m7-003-contract-foundation/PML MultiOmics Analysis Pipeline`
from exact base `221df65`. Luna-max worker `/root/m7_003_contract_luna` stopped
after the initial implementation and two targeted repairs because the focused
gate still had two failures and six errors. The remaining implementation defect
passes an empty `derived_from_entity_ids` through `sort(NULL, method="radix")`;
one synthetic embedded-JSON fixture also removes `size_bytes` instead of
retaining JSON null. No commit or registration request exists, the full suite
has no result, and no Sol review or integration occurred. The three uncommitted
allowlisted files are preserved in the worktree. The durable receipt is
`reviews/2026-10-23-m7-003-contract-foundation-blocker.md`. Further product
work requires Omar's explicit authorization of a new bounded repair record.

Omar supplied that authority by invoking the `implement` workflow and asking to
implement the accepted specification to a passing test gate. M7-004 is the new
bounded completion record. It reopens only contract foundation for one fresh
Luna-max implementation plus at most two targeted repairs, then one complete
suite and independent Standards/Spec review. The three failing M7-003 files may
be preserved only as an explicitly non-acceptance seed. No producer, D,
migration, shared-registration, D-T01, M7, or successor gate is released yet.

Fresh Luna-max worker `/root/m7_004_contract_luna` is now active in the fresh
isolated worktree
`/Users/megastore/.codex/worktrees/m7-004-contract-completion/PML MultiOmics Analysis Pipeline`
from failing seed `8bc460b`. It owns exactly the R contract helper, JSON Schema,
focused test file, and packet registration request. The review fixed point is
accepted base `221df65`. The legal next transition is an immutable passing
candidate followed by parallel Standards and Spec review; no integration occurs
before both review axes pass.

Luna returned immutable candidate `65662bc` with focused M7-003 GREEN and a
clean allowlisted diff. Sol's permission-correct full-suite run traversed the
new provenance tests successfully but exposed two pre-existing C-05 source-test
assertions that still expect the now-accepted registration to be unaccepted.
With the approved MOFA2/mofapy2 runtime, focused C-05 reduces to those two stale
assertions plus the fourteen already dispositioned small-fixture warnings. The
C-05 files do not differ between base `221df65` and candidate `65662bc`; the
baseline still blocks overall acceptance and is not waived. Fresh read-only
Standards and Spec reviewers are active on the M7-004 candidate. Integration
remains prohibited until both axes pass and the independent baseline gate is
resolved.

The M7-004 Standards axis passed with no Critical/Important finding and one
nonblocking duplicate-fixture branch. The Spec axis returned two Important
reproduced fail-open paths: missing required block/method groups can be masked by
other groups, and surplus evidence can reuse a route ID with the wrong route
kind. Repair 1 commit `6c437d1` reproduced all three malformed profiles as RED,
then completed the focused suite with zero failures, warnings, or skips. It adds
complete required block/method scope coverage, reverse route kind/ID/full-scope
matching, and positive multi-block controls. The single focused Spec rereview
confirmed the route and repaired group checks, but returned one Important
remaining scope gap: B method state could move to an unrelated method and
normalized metadata could move to an unrelated analysis. The same Luna-max
worker used final repair 2/2 to close those two paths.

Repair 2/2 returned final candidate `647192f`. Sol independently reran its
focused suite: exit 0, `DONE`, no failure, warning, or skip. The exact candidate
blobs were integrated as `8b8c3cd`, `fe34495`, and `1cb9daf`; Sol registered V8
in Imports as `5dcc25f`. The combined source suite then exited 0 with no failure,
error, or skip and the same fourteen named C-05 native MOFA small-fixture
warnings. The summary reporter emitted no numeric PASS total. The durable
receipt is `reviews/2026-10-23-m7-004-integration-verification.md`.

M7-004 is not yet accepted: its single focused Spec rereview was consumed before
the final repair. The run is paused with no active worker. A fresh immutable
acceptance audit needs explicit authorization; otherwise record M7-004 blocked.
B-T07/C-06 producer provenance, D verification/publication, migration, D-T01
acceptance, M7 release, and every Stage-D successor remain closed.

On 2026-10-24 Omar authorized exactly one fresh read-only Astra-low acceptance
audit of immutable snapshot `5dcc25f`. Reviewer
`/root/m7_004_astra_acceptance_audit` returned `changes_required`: Critical 0,
Important 2, Minor 0. It confirmed all five old probes are closed, but found that
represented-matrix/preprocessing contexts are not required to join the
unit/design/method context and that migration proof coverage omits mandatory
`handoff_payload`. The durable receipt is
`reviews/2026-10-24-m7-004-astra-acceptance-audit.md`. M7-004 is not accepted.
Its implementation and two repairs are consumed; no worker remains active.

Omar then authorized M7-006 as a new bounded repair cycle for exactly those two
findings. Its change record is
`change-records/M7-006-provenance-completeness-repair.md`. The legal route is one
fresh isolated Luna-max worker using public-validator TDD, focused GREEN,
two-axis code review, serial Sol integration, one full source suite, and exactly
one fresh read-only Astra-low acceptance audit. Downstream Stage D remains
closed throughout.

Fresh Luna-max worker `/root/m7_006_provenance_luna` ran from commit `b306f14`
in managed worktree
`/Users/megastore/.codex/worktrees/m7-006-provenance-completeness/PML MultiOmics Analysis Pipeline`.
Its exact three-file allowlist and RED/GREEN obligations are recorded in
M7-006. It returned focused-green candidate `5242609` after reproducing both
defects as RED. Fresh read-only Standards and Spec reviewers are active in
parallel under `reviews/2026-10-24-m7-006-two-axis-review-brief.md`. Both axes
passed with Critical 0, Important 0, Minor 0 and `ready_to_integrate: yes`. The
durable receipt is `reviews/2026-10-24-m7-006-two-axis-review.md`. Sol integrated
the exact three candidate blobs as immutable snapshot `e4aed59`; the integrated
focused suite exited 0 with no failure, warning, or skip. After rebuilding the
expired temporary MOFA runtime in fresh isolated prefixes, one uninterrupted
full source suite exited 0 with failures 0, errors 0, skips 0 and the same
fourteen named C-05 native small-fixture warnings. No new warning appeared and
none was suppressed. The durable receipt is
`reviews/2026-10-24-m7-006-integration-verification.md`. The single fresh
read-only Astra-low acceptance auditor `/root/m7_006_astra_acceptance_audit`
returned `changes_required`: Critical 0, Important 1, Minor 0,
`ready_for_sol_acceptance: no`. It confirmed the migration proof now requires
`handoff_payload`, but found that the raw U+001C delimiter used to join context
tuple fields can collide when that escaped character appears inside permitted
identifiers. Sol confirmed the cited immutable construction and schema. The
durable receipt is
`reviews/2026-10-24-m7-006-astra-acceptance-audit.md`. M7-004/M7-006 remain
unaccepted, no automatic rereview is authorized, no worker is active, and no
Stage-D gate has been released.

Omar then instructed Sol to continue, authorizing bounded successor M7-007 for
the single remaining `M7-006-ASTRA-01` collision. Its change record is
`change-records/M7-007-collision-free-provenance-context-keys.md`. The public
seam remains `triomics_validate_stage_handoff_provenance()`. One fresh isolated
Luna-max worker may change only the contract helper, its focused public tests,
and the contract-foundation registration request. It must reproduce the escaped
U+001C tuple collision as RED, implement collision-free component encoding
without banning legal identifier values, preserve a valid U+001C multi-context
positive, and return focused GREEN. Independent two-axis review, Sol integration
verification, one full source suite, and one fresh Astra-low audit remain
mandatory. Fresh Luna-max worker `/root/m7_007_context_key_luna` is active from
authorization commit `dfceea7` in managed worktree
`/Users/megastore/.codex/worktrees/m7-007-collision-free-context/PML MultiOmics Analysis Pipeline`.
It returned focused-green implementation commit `0b1fcbd`: RED was one failure
with no errors/warnings/skips because the public validator accepted the collision;
GREEN exited 0 with no failures/warnings/skips and preserved a legal escaped
identifier/multiple-context profile. Sol inspection requested one registration-
only repair for a stale next-gate sentence; `b34df9f` changed no product code or
test and left the worktree clean. Fresh read-only Standards and Spec reviewers
`/root/m7_007_standards_review` and `/root/m7_007_spec_review` both passed. The
Standards axis reported Critical 0, Important 0, Minor 1 for nonblocking
duplicated test setup; the Spec axis reported Critical 0, Important 0, Minor 0
and confirmed the encoding is injective. Both returned `ready_to_integrate:
yes`. The durable receipt is
`reviews/2026-10-24-m7-007-two-axis-review.md`. Sol integrated exact candidate
blobs as `ea1407b` and `b29c3aa`. The focused suite exited 0 with no failure,
warning, or skip. The uninterrupted full source suite also exited 0 with
failures 0, errors 0, skips 0 and exactly the same fourteen named C-05 native
small-fixture warnings. The durable receipt is
`reviews/2026-10-24-m7-007-integration-verification.md`. Exactly one fresh
read-only Astra-low acceptance auditor `/root/m7_007_astra_acceptance_audit`
passed immutable snapshot `b29c3aa`: Critical 0, Important 0, Minor 0,
`ready_for_sol_acceptance: yes`. It closed `M7-006-ASTRA-01` and found no
regression in the prior provenance controls. Sol accepted the combined
M7-004/M7-006/M7-007 public synthetic provenance contract foundation. The
durable audit and acceptance receipts are
`reviews/2026-10-24-m7-007-astra-acceptance-audit.md` and
`reviews/2026-10-24-m7-007-sol-acceptance.md`. The audited worker registration
remains unchanged as a candidate handoff; no post-audit product mutation was
made. D-T01, M7, producer wiring, migration execution, D verification/publication,
and every Stage-D successor remain closed in this run.

Omar then authorized a new bounded cycle to select and implement the next legal
producer-provenance or migration seam. Sol selected B-T07 native provenance as
M7-008. The historical public B handoff is not eligible for deterministic
migration because it lacks complete immutable proof for required metadata,
unit/design, preprocessing, and method-state roles; D may not reconstruct those
facts. The authorized route is native regeneration from exact same-run public
synthetic objects. The change record is
`change-records/M7-008-b-t07-native-provenance-producer-seam.md`. One fresh
isolated Luna-max worker may edit only `R/results_b.R`, `R/export_b.R`, the
B-T07 focused test file, and its registration request. C-06, migration, D
verification/publication, D-T01, M7, and every Stage-D successor remain closed.
Worker `/root/m7_008_bt07_provenance_luna` is active from base `0d96c57` in
managed worktree
`/Users/megastore/.codex/worktrees/m7-008-bt07-provenance/PML MultiOmics Analysis Pipeline`.
Its next legal transition is a clean focused-green candidate followed by Sol
inspection and the two-axis read-only review; it cannot accept or integrate.
Initial candidate `62f33c4` remained RED: the exact B-T07 suite reported FAIL 9,
WARN 5, SKIP 0, PASS 224 because strict matrix-key equality treated names as
content and list-form retained unit maps were not normalized. B-T03 passed; the
two B-T01 product failures were the same unit-map defect, while its third error
was the known managed-worktree inability to write a temporary fixture beneath
`inst/examples/tickets`. Targeted repair 1/2 produced clean candidate
`39b49f7`: it fixed both original identity-normalization defects, but complete
profile construction exposed the next narrow mismatch.
`triomics_b07_prov_artifact_entity()` does not accept
`row_identity_sha256` and `column_identity_sha256`, although the
`represented_matrix` caller supplies both and the underlying entity helper
supports them. The exact B-T07 suite now reports FAIL 9, WARN 5, SKIP 0,
PASS 231; both public B-T02 CLI identities pass the prior unit-map and matrix
checks before reaching that same constructor error. Targeted repair 2/2 is
active with the same worker and four-file allowlist. It may add and forward
those two digests and repair only directly exposed profile-construction or
validator defects. Final repair candidate `8745f22` successfully emits and
validates the native B provenance payload/envelope; both public B-T02 identity
CLIs exit 0 and B-T03 passes 286 expectations. The exact B-T07 suite reaches
FAIL 1, WARN 0, SKIP 0, PASS 261. Its only remaining failure is AC3: a direct
`caller_matrix_unverified` export without same-run `provenance_context` remains
fail-closed with reason `partial`, while the existing assertion expects
`incomplete`. The assertion was not relaxed. The initial attempt and both
targeted repairs are consumed, so M7-008 is blocked without independent review
or integration. See
`reviews/2026-10-24-m7-008-b-t07-producer-blocker.md`.
The managed M7-008 worktree was archived with its recoverable candidate
snapshot after the blocker receipt was committed.

On 2026-10-25 Omar authorized M7-009 to adjudicate the sole remaining B-T07
AC3 resume reason before any repair. The proposed change record is
`change-records/M7-009-resume-reason-vocabulary.md`. It preserves `partial` for
a physically interrupted payload/envelope pair and `incomplete` for a present
finalized candidate whose declared status or required provenance evidence is
not complete. The existing AC3 expectation remains `incomplete`; no product
code has been changed. The restored M7-008 candidate remains at `8745f22` and
must receive a fresh read-only audit of this decision before any repair.
The initial Astra-low audit of snapshot `b4d43c5` returned changes required:
failure precedence was ambiguous, physical envelope presence could weaken the
post-write validation guard, and the draft overstated uniform Stage-A reader
behavior. The M7-009 record now preserves `hash_mismatch` precedence for a
readable envelope binding different bytes, separates physical presence from
base-envelope and required-evidence validation, protects resume and post-write
finalization, and remains bounded to B-T07. One fresh focused re-audit is
pending; implementation is still prohibited.
That focused re-audit accepted those repairs but found one Important wording
conflict: AC3 intentionally publishes a self-consistent diagnostic bundle with
declared status `partial`, so the contract cannot prohibit all incomplete
artifacts from finalizing. The record now distinguishes diagnostic partial
publication, which may finalize but is never reusable and resumes as
`incomplete`, from a complete/reusable claim, which requires every evidence
check or fails with `B_EXPORT_INCOMPLETE`. A new fresh focused audit is pending;
product code remains unchanged.
Fresh final Astra-low audit of immutable snapshot `8d12c67` then passed with
Critical 0, Important 0, Minor 0. M7-009 product work is unlocked only on the
restored candidate `8745f22`, limited to `R/export_b.R`, the B-T07 focused test,
and its registration. Required evidence includes the existing AC3 RED, ordered
reason precedence, overlapping hash/evidence failure, complete-claim post-write
failure, exact B-T07, affected B-T02/B-T03, and Stage-A non-regression. No
integration or downstream work is authorized before focused GREEN and review.
Luna-max returned candidate `762a0a0`; its exact B-T07 suite passed 298
expectations and focused B-T02, B-T03 and A-T08 checks passed. The fresh Spec
axis passed with no finding. The Standards axis found one Important defect:
hash-mismatch inspection writes a temporary schema probe beside the finalized
artifact, so a readable non-writable artifact directory can be misclassified as
`incomplete`. Candidate `762a0a0` is not ready to integrate. One bounded repair
is active with the same Luna worker and the same three-file allowlist; it must
make inspection read-only, preserve `hash_mismatch`, add a public-seam
regression and receive focused rereview. See
`reviews/2026-10-25-m7-009-two-axis-review.md`.
Luna repair `2eb483a` moved the schema probe to a unique process-temporary
directory and added a public exported-resume regression over a read-only
handoff. Focused RED reproduced two failures and the denied artifact-tree write;
GREEN passed 17 expectations. Exact B-T07 passed 315, and focused B-T02, B-T03
and A-T08 passed 266, 286 and 284 expectations, with no failures, test warnings
or skips. Sol independently reran the focused regression at the immutable
repair commit: 17 passed. Fresh focused Standards and Spec rereviews both
passed with Critical 0, Important 0, Minor 0. The original Standards finding is
fully addressed and the seven-rule M7-009 contract is unchanged. Candidate
`2eb483a` is verified but remains unintegrated and unaccepted; registration is
still `candidate/review_pending`, `accepted: false`. See
`reviews/2026-10-25-m7-009-repair-rereview.md`. No run-owned agent or handoff is
active. A later serial integration requires explicit authority; all downstream
Stage-D work remains closed.
Omar then directed the serial integration. The reviewed final blobs were
integrated as `0fa320c`, `fced7eb`, `5ca5012`, and `0f139a4`; they equal
candidate `2eb483a` exactly. Fresh integrated focused gates passed B-T07 315,
B-T02 266, B-T03 286 and A-T08 284 expectations with zero failures, test
warnings or skips. The required full source suite then reached `DONE` but exited
1 with three failures and fifteen warnings. Fourteen warnings are the existing
C-05 native small-fixture diagnostics. The new warning and all failures come
from B-T04: its custom synthetic Stage-B executor returns exact matrices but no
same-run `provenance_context`, so the universal B-T07 post-stage hook refuses
with `B_EXPORT_INCOMPLETE`. Focused B-T04 and source-composition reproductions
confirm the same boundary. This is not a B-T04 numerical/calibration failure;
the 0.08 bound and seeds 1001:1400 are unchanged. The unused two-visit fixture
matrix cannot truthfully be relabelled as ancestry for B-T04's generated
closed-form response. See
`reviews/2026-10-25-m7-009-integration-gate.md`. B-T07 remains integrated but
unaccepted and `candidate/review_pending`. A new explicitly authorized bounded
record must select and audit truthful B-T04 producer lineage or an explicit
no-handoff disposition before repair. No run-owned agent is active and all
downstream Stage-D work remains closed.
M7-010 then selected the truthful native B-T04 synthetic-producer route. The
immutable amendment at `083df95` requires a versioned synthetic protocol as the
only analysis source root, excludes the two configured paired-example TSVs from
both provenance and block-envelope inputs, and binds the uncentered matrix handed
downstream separately from the subject-centered response fitted by limma. A
fresh read-only Astra-low audit passed with Critical 0, Important 0, Minor 0 and
`audit_pass: yes`. Bounded test-first implementation is unlocked under
`docs/superpowers/plans/2026-10-25-m7-010-b-t04-synthetic-lineage.md`; B-T07/M7,
G7 and downstream Stage D remain closed.

M7-005 opens that independent baseline gate as a one-file source-test repair.
Fresh Luna-max worker `/root/m7_005_c05_luna` may change only
`tests/testthat/test-C-05.R`: update the obsolete unaccepted-registration
expectations to the already tracked accepted synthetic-only C-05 state, preserve
all qualification assertions and the fourteen named native warnings, and return
a clean commit for fresh Sol review. This lane is file-disjoint from both M7-004
reviewers and does not release a product gate. Its fresh isolated worktree is
`/Users/megastore/.codex/worktrees/m7-005-c05-source-baseline/PML MultiOmics Analysis Pipeline`
at base `29e7a36`.

M7-005 candidate `ef62066` changes only the obsolete C-05 registration-state
test block. Focused RED had the two stale assertions; focused GREEN had zero
failures/errors/skips and preserved the fourteen named native warnings plus one
intermittent correlated-factor QC warning already covered by the C-05 acceptance
review. Fresh read-only Sol-high review passed with Critical 0, Important 0,
Minor 0 and independently passed all 16 registration expectations. Sol
integrated the one-file correction as `223546e`. The durable receipt is
`reviews/2026-10-23-m7-005-c05-source-baseline-review.md`. This repairs only the
source-suite baseline; M7-004, D-T01, M7, and every downstream gate remain
closed pending the contract repair, focused rereview, integration, and combined
source-suite gate.

Omar also approved `opencode/deepseek-v4.1-flash-free` and
`opencode/glm-5.3-flash-free` as additional public-synthetic implementation
lanes. They are not used for D-T01: only after D-T01 acceptance do the
file-disjoint D-T02 and D-T03 packets become legal. Preserve every C-05, C-07,
and C-09 scientific qualification and never send private study inputs to free
providers.

G0 through G5 are released. Stages A and B and C-01
through C-05 plus C-08 are accepted on synthetic software evidence. The
decisive C-08 clean archive check at `d69ef1e` is `Status: OK`, with testthat
FAIL 0, WARN 22, SKIP 0, PASS 4367. Eight warnings are the previously
dispositioned B-T07 missing-Git-metadata probes; fourteen are native MOFA
small-fixture/QC diagnostics. The run is not described as warning-free. C-09
packet code and C-06 packet/shared code are now integrated. C-09 shared
NAMESPACE/schema/CLI registration is also integrated and freshly verified.
C-06 and C-09 are not yet accepted at M6. The clean package gate at `c5e65f9`
ended `Status: 1 WARNING` because the newly exported C-09 synthetic-input
generator lacks a documentation entry. Astra did not run. The run is paused;
G6 and every D packet remain closed.
The historical sections below keep their original wording.

## 21 October M6 package-gate failure

Sol created a clean `git archive` of exact commit `c5e65f9`; live-checkout
changes and AppleDouble files were not included. `tools/check-package.R` ran on
the extracted archive with the accepted isolated MOFA runtime. Its wrapper
exited 1 and `00check.log` ended `Status: 1 WARNING`. The sole R CMD check
warning is missing public documentation for `triomics_c09_synthetic_inputs`.

Installed tests themselves completed with FAIL 0, WARN 22, SKIP 0, PASS 4787.
The warnings match the previously named eight B-T07 missing-Git probes and
fourteen native C-05 MOFA small-fixture diagnostics; none was suppressed. Full
hashes and test names are in
`reviews/2026-10-21-m6-package-gate.md`. No repair was made and Astra was not
dispatched. M6 is unaccepted; G6 and Stage D remain closed.

## 21 October pause after C-09 shared cycle

C-09 shared candidates `c23a3c8`, `d5aaa1e`, `604a868`, and `b2aafd6` were
reviewed and integrated as `91107d3`, `164eda5`, `c9613de`, and `40bc026`.
The initial fresh Sol-6.1-high review found one Important mapping-order defect.
Luna repair 1 fixed it, and the single focused rereview passed with no Critical,
Important, or Minor findings. Fresh integrated tests passed C-09-shared 71,
C-09 124, C-06 290, and C-02 165 expectations, all with zero failures,
warnings, or skips.

The exact synthetic C-09 Stage-C CLI exited 0. Its payload, envelope and run
registry cross-hashes validated. The route is explicitly limited to
`synthetic_fixture_only`, retains `benefit_guaranteed=false`, and does not
qualify a real endpoint, clinical deployment, uncertainty protocol or
independent validation. No private study data were read or fitted.

This cycle intentionally stops before M6. No package gate or immutable Astra
audit was run after C-09 integration. All run-owned workers are reconciled;
there is no active handoff or background polling.

## 21 October pause after C-06 shared cycle

C-09 initial plus two bounded repairs were integrated as `98f73c3`, `6713603`
and `996087f`; its exact packet suite passed 53 expectations. Its exclusive
shared NAMESPACE/schema/CLI registration has not started, and its real
endpoint, deployment and uncertainty protocol remains unqualified.

C-06 packet commits were integrated as `7d29da4` and `634c29f`. Its reviewed
shared Stage-C/D-handoff series was integrated as `32ab4d1`, `a658398` and
`3753be0`. Fresh integrated C-06 tests passed 290 expectations with zero
failures, warnings or skips. The exact synthetic CLI exited 0 and wrote a
truthful partial D handoff: all five payload/envelope pairs plus the manifest
envelope matched recorded hashes; the four C-07 controls are explicitly
`not_run`; C-09 is conditional/not started. The accepted C-02 regression
fixture also exited 0 with 12/12 eligible predictions and correct root identity.
Its single-fit D export remains explicitly unavailable without downgrading the
complete nested evaluation.

This is synthetic software evidence only. No private study data were read or
fitted. No package gate or immutable M6 Astra audit ran after this integration,
so C-06 is verified but not M6-accepted. All run-owned workers are reconciled;
there is no active handoff or background polling.

## Current evidence

- B-T07 packet commits were integrated as `8cf2d38` and `4a33f01`.
- Same-run matrix retention and CLI/API registration were integrated as
  `f1ed853` and `c5f868f`.
- B-T07 repair 2 was integrated as `1146b5a`; its focused suite passed 124
  expectations and exact CLI AC3 passed. One Sol P1 was transparently retained
  and overruled by Astra as a P2 traceability ambiguity because the frozen
  contract permits verified aggregate unavailable status without the stripped
  optional method fields.
- M4-013 cross-packet composition candidate `57474e7` passed fresh Sol-high
  review and was integrated as `bd7da66`. B-T04/B-T07/B-T08/B-T09 plus the
  composition suite passed 870 focused expectations; four exact CLIs exited 0
  and 34 envelopes validated. Numerical results, B-T04 calibration and clinical
  privacy behavior were unchanged.
- The complete local suite on `bd7da66` passed with zero failures, warnings or
  skips.
- The clean Git-archive package check then failed with one ERROR, two WARNINGs
  and one NOTE: 15 installed B-T07 fixture-path/runner failures, one non-ASCII
  em dash in `R/export_b.R`, one unqualified `setNames()`, and duplicated
  `resume` documentation. Evidence:
  `reviews/M4-013-package-check-gate.md`.

These are packaging/test-infrastructure defects; no scientific regression was
demonstrated. They still cannot be waived. No private study data under the
authorized external data folder was read, fitted, copied, uploaded or committed.
All evidence remains public synthetic software evidence only.

## M4-014 authorization recorded

B-T07 consumed its initial attempt and two repairs. Omar explicitly authorized
the bounded M4-014 exception on 2026-10-16 after Sol restated the exact scope.
The exact record is
`change-records/M4-014-b-t07-package-gate-exception-proposal.md`. It permits only
`tests/testthat/test-B-T07.R`, the two mechanical `R/export_b.R` corrections,
and the Sol-owned duplicate Rd consolidation. It does not reset counters,
authorize a scientific change, or waive any verification gate.

Luna candidate `4acbd71` changed exactly `tests/testthat/test-B-T07.R` and
`R/export_b.R`; source and installed focused runs each passed 124 expectations
with zero failures or skips. Sol-owned manual cleanup is commit `7194b23`.
Fresh Sol-6.1 high review passed with P0/P1/P2 zero, and the candidate was
serially integrated as `01c5e49`. Fresh source verification then exposed a
path-with-spaces defect in the newly changed source CLI helper: Rscript receives
the absolute script argument with spaces transformed to `~+~`, so four source
CLI smokes failed before dispatch. Luna repair `74335d1` changed only the test
helper and passed 124 expectations from a spaced checkout, the integration
snapshot and an installed package, with zero failures or skips. Fresh focused
Sol-6.1 high re-review marked the finding ADDRESSED with no new P0/P1, and the
repair was integrated as `cbe5400`. Fresh integrated evidence at `d2a2231` is:
source B-T07 124/124 with no warnings/skips; installed B-T07 124/124 with no
failures/skips; full suite 3,462/3,462 with no warnings/skips; clean archive
package check `Status: OK` with no ERROR/WARNING/NOTE. The immutable Astra review
returned `changes_required` only because B-T01/B-T02/B-T03 lacked explicit
acceptance records. Sol applied record-only M4-015 after a fresh 749-expectation
combined pass; all three are accepted now, not retroactively. Fresh read-only
reviewer `/root/m4_g4_focused_astra` is active on record-only correction
snapshot `0f4be24` for the single focused rereview of M4-G4-001. That rereview
passed, and Sol accepted B-T07/G4 and released Stage C. No worker is active at
that G4 acceptance boundary; the current C-01 worker is recorded below. Older
completed managed worktrees remain archived.
The user-owned `.gitignore` and AppleDouble files remain untouched. The
git-ignored `.superpowers/` contains review packages only and remains untracked.

## 18 October specification re-evaluation

The documentation-only review in
`reviews/2026-10-18-spec-kit-reevaluation.md` compared all 29 frozen
requirements with the accepted A and B implementation. It found 7 requirements
verified on synthetic software evidence, 15 partially implemented, and 7 with
no accepted implementation. It made no scientific-contract or product-code
change. The separate 17 October local workbook inventory is now recorded as
inventory only. It is not a pipeline fit or biological validation. The two
Notion sync receipts record successful read-back, while GitHub publication
remains unverified and the current CLI token is invalid. Initial Astra review
confirmed all 29 requirement classifications and found one P2 lifecycle-record
issue. The focused rereview passed after correction, with no new P0 or P1.

## Accepted C-01 handoff

Omar explicitly resumed implementation on 2026-10-18. Fresh Luna-max chat
`01a14fdb-c11f-7e13-836e-e87635a89f23` continued the same C-01 initial
attempt in the preserved isolated worktree
`/Users/megastore/.codex/worktrees/c-01-integration-foundation/PML MultiOmics
Analysis Pipeline` at base `945ef8c`. The attempt counter remains 1. Before
dispatch, the worktree contained only the preserved allowlisted file
`tests/testthat/test-C-01.R` (SHA-256
`6eb0aca4f9a6c974d68b1a46f6e17aa0e7509f3dde749d702b51ed63fef881da`). The
worker produced clean allowlisted candidate
`5e43391155643732289a2cfc3d2a27fdbcd3b416`. Fresh Sol-6.1-high review
returned changes required: P0 0, P1 2, P2 2. Luna repair 1 fixed actual
B-resolver identity compatibility, the two-block minimum, complete-view typed
refusal and malformed public-input typing. The single focused rereview passed
with P0/P1/P2 all zero. Sol integrated the candidate as `b5527ef`, repair as
`1cf1632`, and shared registration as `40caa9f`. The exact C-01 suite passed 109
expectations, the exact stage-C CLI exited 0 with a valid complete envelope, and
clean archive package check ended `Status: OK`. C-01-AC1 through AC4 are
accepted on synthetic software evidence. Private study data remain untouched.
Acceptance receipts are commit `38bc7dc`. The three run-owned chats are archived
and the C-01 worktree is archived with a recoverable snapshot. C-02 is released;
C-03/C-04/C-05 remain behind G5.

## Historical C-02 handoff

Fresh Luna-max chat `01a15056-fa00-79b3-867c-538f1a004894` completed C-02
candidate `809190e` in isolated worktree
`/Users/megastore/.codex/worktrees/c-02-supervised-evaluation/PML MultiOmics
Analysis Pipeline` at base `38bc7dc`. The worktree was clean and the C-01
baseline passed 109 expectations before dispatch. `mixOmics 6.36.0` is installed
and the C-02 suite passed 61 expectations with no failure, warning or skip. The
worker reported an invalid `ticket_id` CLI key, partial AC3/AC4 evidence, an
interrupted package check and pending shared registration, so acceptance was not
claimed. Fresh read-only Sol-6.1-high review chat
`01a1506e-9b59-7241-8c33-6a1a6da05423` returned `changes_required`: zero
Critical, eight Important and two Minor findings. The review is
`/private/tmp/triomics-c-02-sol-review-2026-10-18.md` and the review chat is
archived. Targeted repair 1 is active in the original Luna chat. It is confined
to matched metrics, fold counts/subject provenance, generic adapter dispatch,
native-output failure handling, a valid CLI fixture, preparation-failure ledger,
uncertainty disposition and the missing leakage/budget tests. No integration or
G5 release is authorized yet.

## 19 October C-02 registration

Commits `a3a1cfd`, `249e2d0` and `2f4e8e4` closed review findings F01–F08.
Luna high rechecked that tree, changed no files, and reported 17 cases and 131
assertions passing. Astra low had already returned a code-only pass on the same
range and did not rerun tests. Sol then applied the shared registration:
`mixOmics` in DESCRIPTION and `renv.lock`, `evaluate_pipeline` exported,
`concat_plsda` and `concat_pls` registered, and stage C dispatching nested
evaluation. The allowlisted splits helper is commit `028f7c8`.

The integrated C-02 suite passed 165 expectations with no failures, warnings
or skips. A-T05, including the empty-filter guard, passed 127. The command

`Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-02.yaml --stage C --output artifacts/acceptance/C-02`

was run against a fresh temporary directory and exited 0. The synthetic
`concat_plsda` result predicted 12/12 eligible units, with balanced error 0 and
8 inspected fits. One inner fold requested `ncomp=2` at training rank 1 and
failed closed as `C_EVAL_COMPONENT_RANK`. The fixture `min_observed` is 1
because an inner training side can contain two samples. That accuracy is a
property of the tiny synthetic fixture, not a biological result.

C-02 is accepted on synthetic software evidence. The M5 review thread
`01a15362-3c23-7432-8e22-d52aa222658f` returned pass with no Critical or
Important findings and did not rerun tests. G5 is released for C-03, C-04,
and C-05. C-08 and every D packet remain closed.

C-03 code is commit `9758b4a`. Sol reran `test_local(filter='C-03')`: 57
passed, 0 failed, 0 warnings, 0 skips, 7.8 seconds. The stage C fixture
exited 2 with `C_SCORE_BACKEND_UNAVAILABLE` before ingest. Shared registration
is not applied, and C-03 is not accepted. The rank guard currently measures
the first predictor block only; native component mismatch still fails closed.

## Literal resume instruction

Read the current M7-014 checkpoint, integration receipt and execution ledger.
The authorized seam is verified and integrated at359a1d0; no worker or proof
process needs resuming. Preserve candidate18b6e90 and its exact artifact/hash
receipt. Do not repeat the M7-013 audit, known label repairs or focused proofs
merely to recover context, and do not mark M7-011/M7-012/M7-013 complete.
C-06 accepted=false; M7/G7/Stage D remain closed. Any next acceptance/downstream
transition requires its own authority. No private data or push.
The older resume guidance below is historical and does not override this scope.

Read the current checkpoint, `execution-ledger.json`, M7-010's change record,
and `reviews/2026-10-26-m7-010-resumed-source-gate.md`. M7-010 is reviewed,
integrated and verified by the completed full source suite. No worker or gate
needs resuming. Preserve the completed repairs/reviews/source evidence. Any
next producer-provenance or migration seam needs its own bounded scope and
applicable audit before implementation; this run did not authorize it. Restore
the isolated spec-validator dependencies before claiming fresh specification
consistency validation. The three old A-T01 diagnostics did not recur; do not
invent their cause or claim a code fix.
M6/G6 and the accepted public synthetic contract foundation remain historical
accepted results. B-T07/M7 acceptance, G7 and Stage D remain closed in this
cycle. Do not loosen the B-T04 0.08 bound, reroll seeds 1001:1400, edit frozen
`discovery/spec-v2` content, read private study data, or push to GitHub.
