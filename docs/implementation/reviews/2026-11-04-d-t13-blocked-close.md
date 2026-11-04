# D-T13 Sol blocked receipt — final repair cap

D-T13 remains **blocked and unaccepted**, with no completion credit or product integration. Final product candidate `37663a12d285ee4cbc68146a40342caaed513c15` and worker evidence commit `ef48615b721ed9148f132d0548529802fcbeb9cd` are clean and preserved on the isolated worker branch. One initial implementation and both allowed repairs are consumed. No repair3 is opened.

## Standards

Fresh read-only `/root/d13_final_sol_review`: pass. Exact allowlist and immutable archive/diff hashes verified. Requested gpt-6.1-sol/high; effective settings not exposed. Archive SHA-256 `4b23932f60ecd3d63188866105ee1f43572cf6eeb7f4c52d9a3ee175e4fe4e60`.

## Spec

Changes required: one P2, no P0/P1. At candidate `R/report_targets.R:58`, the alphabetic-only prose filter drops legal numeric methodological descriptions. The actual external-evidence and profile producers retain “Clinical score constructed from 3 molecular features” and “Top 10 features selected in the same study”, but both report fields become empty. This still violates D13 AC2 and D contract161–162 construction-lineage preservation. Shared-subject status and independent_replication=false remain intact. Six focused privacy/control probes pass; no additional privacy or false-independence defect identified. This is information loss, not evidence of an independent replication.

Sol independently reproduced the same two lost fields on the immutable archive, using the pinned runtime and synthetic metadata only. The probe exited1 with preservation_pass=false. Source retained both sentences; report contained two empty strings, shared_subjects and independent_replication=false. The evidence and hashes are in [the receipt](../evidence/2026-11-04-d-t13-close/blocked-receipt.json).

The worker's uncapped focused suite on product37663a1 passed **260 assertions /36 blocks: D-T13 109; D-T08 151; FAIL0, ERROR0, WARN0, SKIP0**. There are no warning names. Sol read the structured receipt; these are worker-run tests, not a parent full gate. Passing tests did not cover the final counterexample. D08's151 versus prior154 follows its unchanged provider-absent fallback branch, not removed tests or skips.

No fresh native A/B/C/D/report gate, package check or Astra milestone audit was run after the failed final local review. Therefore no final-candidate provenance hashes or Status:OK are claimed. Initial94ede2d native output and all earlier RED/GREEN receipts remain historical; they do not qualify this candidate. Repair1 RED reached its10-failure cap; finalrepair2 RED was uncapped with two failures. Prepared parent native/readback runners remain unexecuted.

Only coordination/review evidence is integrated. M7/G7 and D08 stay accepted. M8/G8 and M9/G9 stay closed; D13 remains required. D06/D07/D14 retain zero credit; C07 controls not_run; C09 unqualified/synthetic_fixture_only/benefit_guaranteed=false; frozen-kit audit pending. No private study data, GitHub push, AIU run or Notion sync. No active run-owned worker/process remains.

Literal resume: **Continue the independent D-T15 clinical-objectives packet in one fresh Luna-max worker. Its frozen dependencies D-T08/C-08 and G7 are satisfied; D13 is not a dependency. Preserve D13's blocked receipt and candidates, do not reopen its exhausted repair cycle, and do not release G8 while D13 is unresolved. Stop after the D15 receipt.**
