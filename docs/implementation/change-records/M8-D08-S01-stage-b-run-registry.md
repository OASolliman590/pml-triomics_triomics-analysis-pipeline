# M8-D08-S01 — Stage B run-registry collision (scope proposal)

Status: proposed, awaiting a fresh read-only scope audit. No implementation. This is an implementation compatibility repair for the existing native stage sequence, not a scientific-contract or schema amendment and not a new packet identity.

## Evidence and authority

The authorized D-T08 packet requires real saved A/B/C/D artifacts from one output root. Immutable candidate23b5689 passes75 focused assertions but native A exits0 and B exits4. Both R/ingest.R and R/results_b.R publish registries/run.json with a non-overwriting atomic writer. The repeated-measures B producer has the same registry filename in R/design_repeated.R. C already uses run-C.json. Sol's preserved RED receipt is reviews/2026-11-04-d-t08-initial-review.md and its evidence. A's record must not be deleted to make a verification pass.

The user previously authorized new bounded cycles where needed and assigns shared registration to Sol. D-T08's worker allowlist does not include either B writer. This proposal does not silently extend that allowlist: any implementation must be an exclusive predecessor repair with its own audited scope. No implementation is permitted merely because a reviewer approves the document; the coordinator must reconcile the standing task authorization and exact scope first.

## Proposed behavior

Stage B publishes its existing run-record shape to a fixed stage-owned registries/run-B.json. Preserve registries/run.json as a legacy first-stage record: create it for standalone B only when absent; never overwrite or delete an existing A record. Preserve A payload/envelope bytes. C and D registry/publication behavior stays unchanged. Do not introduce a new global mutable pointer, a new schema or a timestamp-based scientific identity.

The narrow shared helper must validate the controlled registry directory and reject symlink/path escapes, malformed/conflicting records and cross-study use. A pre-existing A record requires its existing valid A artifact/envelope and matching study/configuration identity; do not bless an arbitrary file merely because its stage label says A. Existing B records are not silently replaced. Conflicts fail closed. Document exactly which per-stage record is authoritative and the compatibility meaning of the legacy filename.

Preserve the existing atomic no-overwrite writer for payloads and envelopes. Do not patch it globally or temporarily hide a registry in the test harness. No recovery, concurrency, all-stage scheduler, scientific method, seed or numerical expectation is invented by this seam. Publication must not falsely mark a failed or incomplete stage complete.

## Proposed exclusive file scope

- R/stage_registry.R: Sol-owned small registry-publication helper. Any alternate location requires a revised explicit scope before implementation.
- R/results_b.R: replace only its run-registry publication call; no fitting, inference, result identity or provenance change.
- R/design_repeated.R: the equivalent registry-publication call only; no B-T04 method, calibration, 0.08 bound or seeds1001:1400 edit.
- tests/testthat/test-stage-run-registry.R: bounded shared regression tests; do not edit D-T08's active test file in this predecessor repair.
- Sol-owned implementation receipts, ledger and handover only.

Any larger implementation need stops with evidence. This document is a proposal, not permission to edit extra files opportunistically.

## Required proof before integration

1. Native A then B in one untouched fresh root both exit0; A registry and A payload/envelope hashes stay unchanged, B's stage record and native provenance frame exist and validate.
2. Standalone B retains its previous legacy registry behavior. Both ordinary and repeated B registry call paths use the fixed shared helper, with narrow tests that do not rerun or alter statistical calibration merely to test a filename.
3. Invalid identity, symlink and pre-existing conflicting records fail closed with no replacement of previous registry bytes; exercise publication interruption/failure at the actual boundary supported by the implementation.
4. Relevant existing A/B/C consumers still pass focused regressions. Then run the D-T08 same-root A/B/C/D/report proof without manual removal, copying fabricated producer evidence, or weakening assertions.

The scope audit is not the M8 milestone audit and does not accept D-T08, M8 or G8. M7/G7 stay historically accepted; D-T06/07/14, C-07, C-09 and the pending frozen-kit audit keep their existing dispositions. No private data or push.
