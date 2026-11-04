# M8-D08-S01 — Stage B run-registry collision (scope proposal, revision 1)

Status: proposed; initial Astra scope audit requested changes. This is the one documentation repair before a focused rereview. No implementation, packet acceptance or milestone release. The frozen scientific contracts and all41 packet identities remain unchanged.

## Evidence and scope authority

D-T08 requires saved A/B/C/D artifacts from one output root. Candidate23b5689 passes75 focused assertions; Sol's untouched native A exits0 then B exits4. R/ingest.R and R/results_b.R both write registries/run.json with a non-overwriting atomic writer. R/design_repeated.R has the same B registry publication. See reviews/2026-11-04-d-t08-initial-review.md and its preserved RED evidence. The worker registry-removal workaround is excluded from acceptance.

The user previously authorized bounded cycles where needed and assigns shared registration to Sol. Neither B writer belongs to the D-T08 worker's allowlist. This is a proposed exclusive predecessor repair, not a silent expansion of that worker's scope. Scope-audit approval does not itself grant foreign-file editing authority; Sol must reconcile standing task authorization before any implementation.

Ordinary C already uses run-C.json. The separate C-09 synthetic route in R/cli.R:2027–2047 still uses run.json. That route is explicitly excluded: do not execute, repair, qualify or reclassify it here. C-09 remains unqualified/synthetic_fixture_only/benefit_guaranteed=false.

## Meaning of the records

Preserve the existing B run-record fields and values. Their status describes the saved B producer artifacts at that call site; it is not proof of successful complete CLI execution, successful calibration, a valid handoff, scientific eligibility or milestone acceptance. This is particularly relevant to the existing repeated-measures call site, which precedes calibration/acceptance writes and a possible abort. Do not move that call or alter the calibration/control flow in this narrow repair.

The new canonical B record is registries/run-B.json. Retain registries/run.json as the compatibility record for the first stage that wrote it. Standalone B still creates that legacy record when absent. A pre-existing valid A record stays byte-identical. C/D behavior remains unchanged. Registry presence alone must never substitute for the command exit and verified B provenance frame. A failed calibration or later B handoff must retain its nonzero exit/unqualified outcome even if a producer-artifact record exists.

Do not create a new global current-run pointer, schema, automatic recovery protocol, scheduler or data-ancestry claim. In particular, the A registry/configuration identity check does not assert that A's matrix is the response ancestry of the B-T04 synthetic generator.

## Admission and publication sequence

A single pipeline writer is assumed, as in the accepted provenance route. Use fixed filenames beneath the validated output root. Before any new registry is published:

- Reject any pre-existing run-B.json, including an identical record; no resume or overwrite is added.
- Reject directory targets, dangling or resolving symlinks, and symlink/path escapes anywhere in the controlled registry path.
- The legacy filename may be absent or a valid complete A record only. Existing B records, unsupported stages, malformed JSON, missing/extra identity fields, wrong schema/study/run identity and conflicting records fail closed.
- A legacy A record must match its existing validated A/assays.json and exact envelope. Require the expected artifact type/ID, exact payload filename and SHA-256, study identity, current configuration SHA-256, and complete producer status. The legacy A record alone is insufficient because it omits configuration identity. Preserve all A bytes; do not fabricate predecessor artifacts or infer scientific ancestry from this administrative check.

When a valid A record exists, publish only run-B.json through the existing atomic non-overwrite writer. No A record is replaced, renamed or removed.

When the legacy filename is absent, write the legacy B record first and the canonical run-B.json last. Both carry the same existing record shape and bytes. The canonical publication is the registry commit marker; the helper returns success only after verifying both files and their matching hashes. The legacy alias by itself is not a successful new registry publication.

On an ordinary exception/failure during the second publication, remove only files newly created by this helper whose bytes still match this attempt, then propagate failure. Never remove or replace prior bytes. If the process is terminated between writes, a legacy-only orphan may remain; it is explicitly unpublished, cannot count as completed stage/handoff evidence and is rejected on a later attempt. This proposal does not promise power-loss durability, crash recovery or whole-Stage-B transactionality. Existing partially written B payload behavior is not silently relabelled as complete.

## Exact proposed implementation scope

- R/stage_registry.R: small Sol-owned shared registry-publication helper, using existing atomic and path-validation utilities.
- R/results_b.R: replace only its registry-publication call with the helper; preserve fitting, result identity, status values and provenance behavior.
- R/design_repeated.R: equivalent call replacement at its existing location only; preserve statistics, calibration, acceptance writes, failure control flow, bound0.08 and seeds1001:1400.
- tests/testthat/test-stage-run-registry.R: bounded shared behavioral regressions, including both B call sites. No concurrent edit to D-T08's active tests.
- Sol-owned implementation receipts, ledger and handover.

Any larger need stops with evidence and requires a separately reconciled scope. Do not edit R/contracts.R, R/ingest.R, C-09, schemas, canonical spec, packet identities, lock files or scientific methods.

## Required proof

1. Native A then B in a fresh untouched root: both exit0; A registry/payload/envelope hashes unchanged; canonical B record and native B provenance frame validate. No harness deletion or relocation.
2. Standalone B creates matching canonical and legacy records. Test both ordinary and repeated writer call paths without changing or rerunning numerical calibration merely to test registry filenames. Preserve the distinction between a producer record and later calibration/handoff success.
3. Inject failures at actual legacy/canonical publication boundaries. Verify ordinary failure cleanup, immutable prior-A bytes, nonzero/throwing outcomes, and rejection of a simulated terminated legacy-only orphan. Neither partial publication nor an orphan alias is GREEN stage evidence.
4. Reject all admission cases above, including identical canonical records, unsupported legacy stages, wrong A envelope/configuration identity, dangling symlinks and directory targets, without replacing prior files.
5. Focused existing regressions: test-A-T09.R; test-M4-005-B-T05-integration.R; test-C-06-shared.R; test-D-T01.R. Add the shared registry suite and the affected ordinary/repeated B focused tests as justified by the call-site diff. Do not reroll seeds or relax numerical expectations.
6. Finally the D-T08 same-root A/B/C/D/report command proof, exact byte lineage and actual Quarto read-back must pass on the combined immutable candidate. This proposal alone is not a report acceptance.

The scope audit is separate from the future M8 milestone audit. M7/G7 retain their historical acceptance, D-T08 remains incomplete, G8/G9 stay unreleased, and all conditional/unrun/pending-audit dispositions remain. No private study data or push.
