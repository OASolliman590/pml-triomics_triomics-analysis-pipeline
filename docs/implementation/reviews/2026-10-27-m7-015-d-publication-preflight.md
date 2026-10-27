# M7-015 — D publication preflight and Astra disposition

Date: 2026-10-27. Base 4d40f3c. Immutable phase record snapshot:
`6fe5f1d3641446e130b2cd49bc7b7f8a9b6c66ca`.

## Result

Blocked before implementation at the shared integration seam. One fresh read-only
Astra audit approved the stop disposition, not D implementation or acceptance.
No Luna dispatched, no implementation/repair attempt consumed, no product diff,
no integration, no D generation/pointer or new artifact hashes. Existing M7-014
integration and proof remain intact. No old worker restarted.

## Astra receipt

Agent `/root/m7_015_astra_preflight`; requested `gpt-6-astra`, effort `low`,
fresh context. Host accepted the exact spawn arguments; effective runtime
settings were not separately exposed. No recursive delegation, file edits,
tests or private-data access by reviewer.

- audit_pass: yes, for blocker disposition only; implementation feasibility: no.
- Important: R/stage_registry.R:3–5 disables D. R/cli.R:1919,1929–1947
  rejects D and has no D executor. Accepted M7-002 section 6.12 (lines363–365),
  retained by M7-003 section1, requires fresh public B-to-C-to-D CLIs and
  actual-byte read-back.
- Important: R/annotation_router.R:323–359 routes in-memory evidence. Its writer
  at244–320 separately writes two files, with no complete generation manifest
  or current pointer. Neither annotation entry is exported in NAMESPACE; the
  D-T01 registration explicitly requests shared exports and CLI registration.
  D-owned API development alone cannot complete the required integrated phase.
- No defect: semantic JSON object-key invariance is distinct from exact bound
  predecessor bytes; rehashing an altered envelope does not renew lineage.
- No defect: pointer-last publication, precommit preservation and ambiguous
  recovery follow the accepted contract. Scientific qualifications are retained.
- Disposition: stop at shared seam before Luna. Approval is not D-T01/M7 acceptance.

Sol agrees with the disposition under Omar's explicit shared-file stop rule.
These are existing feasibility blockers, not two attempted product repairs.

## Direct Sol evidence

From the integration checkout at4d40f3c (subsequent changes documentation only):

```sh
Rscript --vanilla inst/exec/triomics \
  --config inst/examples/tickets/D-T01.yaml --stage D \
  --output /tmp/triomics-m7-015/d-preflight
```

CLI exit3; exact message:
`triomics A_STAGE_UNAVAILABLE: Execution is not implemented for stage 'D'.`
The output root did not exist afterward. No B/C producer suite was rerun.
B-T07, C-06 and D-T01 testthat counts: NOT RUN in this cycle; no GREEN inferred.
Provenance/generation/pointer paths and SHA-256: none written in this cycle.
Evidence: `evidence/2026-10-27-m7-015/d-preflight.log`.

## Shared seam and resume

A subsequent separately scoped shared integration record must cover the D stage
executor/dispatch and stage registration, with package exports/manual changes
only as required by the reviewed public API. The executor must invoke the future
D-owned verifier/publisher before any mutable registry lifecycle. Do not import
rejected historical shared candidates or enable D through source-time rebinding.
The six-file D implementation and shared integration must have an authorized
sequence capable of passing the public CLI gate before work restarts.

Literal resume: Read this receipt and M7-015. Resolve the recorded shared-file
integration scope before dispatching a fresh Luna worker; do not repeat the
M7-014 producer repairs or this preflight audit merely to regain context. Keep
C-06 accepted=false, C-07 not_run and C-09 unqualified. M7/G7 remain closed;
D-T01 publication is blocked; D-T02, annotation execution, targets and report
remain closed. No private study data, GitHub push or Notion sync occurred.
No run-owned worker or proof process remains active.
