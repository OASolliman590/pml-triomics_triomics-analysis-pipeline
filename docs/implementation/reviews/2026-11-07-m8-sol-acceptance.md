# Sol acceptance — M8 and G8

**Sol accepts M8 and releases G8 on immutable product `4c0c59d91b5c8f7117b1097a57e68b0211f0e45a`. G9 remains closed; D-T09/M9 has not started.** This receipt records the decision after the package gate and fresh Astra passed. The commit containing these records is a later documentation revision, not a newly tested product.

The required reporting packets D-T08, D-T13 and D-T15 remain accepted, with their commands, counts, hashes, attempts and historical failures preserved. Their native runs belong to the packet commits recorded in [DOC-M8](../milestones/M8.md) and the [machine acceptance receipt](../evidence/2026-11-07-m8-final-gates/receipt.json). No separate D13 repair, focused rerun or new standalone native M8 report run was performed.

## Acceptance evidence

- Terra DOC-M8 `509fe3dcf05622a802ef95077ed5d070945a4e2c` was reviewed by Sol and integrated before the archive freeze.
- The clean `git archive` of `4c0c59d91b5c8f7117b1097a57e68b0211f0e45a` passed the required `Rscript --vanilla tools/check-package.R --root <archive> --output <logs>` command with actual exit **0**, exactly **`Status: OK`**, **FAIL 0 / WARN 15 / SKIP 0 / PASS 11518**. [Gate receipt and named warnings](2026-11-07-m8-package-gate.md).
- The fifteen unsuppressed warnings match M7: `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes` (11), `C-05 real MOFA2 fits and extracts a missing sample-view capability cell` (3), and `D-T01 refuses stale paired signs and preserves the old pointer` (1 expected CLI status 2). C-05 message multiplicities match exactly; D-T01 has only generated-path differences. No new warning names.
- Archive SHA-256: `25314c061841f1fd77f74facf4295eed3f7d536efb1e888301ffa2fc5cfd6ce8`. Manifest SHA-256: `0f50d787bad7df6021e1204d4fd741137e0fb9c8b6448570d3fa5763808f2b81`. All **1,207 source files** were unchanged after the gate and audit.
- Fresh [Astra-low milestone audit](2026-11-07-m8-astra.md): **pass, audit_pass yes, no blocking findings**, same immutable product. Initial review 1 used; focused rereview 0. Astra did not accept or release anything; this is Sol's decision.

The first archive `ac66bd6` remains recorded as `Status: 1 WARNING, 1 NOTE`. M8-PKG-001 resolved those diagnostics using four equivalent Unicode source escapes and one explicit `stats::setNames` import. Independent byte comparison and parsed-expression identity prove the D13 source meaning is unchanged. Tests and every prior repair attempt remain intact.

## Qualifications retained

Astra M8-N01 records the inherited AGENTS.md/CLAUDE.md manifest discrepancy as nonblocking for bounded M8; both files match accepted M7. The validator still exited 1. The declared frozen-kit hash is historical, and its separate independent audit remains pending. No frozen hash or scientific contract was changed.

Astra M8-N02 records that clinical `approved_aggregate` release is unavailable and fails closed without an institution-approved minimum-cell and complementary-suppression implementation. It receives no implementation credit. Private local reports remain local; text guards do not establish universal anonymization.

C-07's four scientific controls remain not_run. C-09 stays unqualified, synthetic_fixture_only, benefit_guaranteed=false. D-T06/D-T07/D-T14 remain blocked with zero completion credit. Qualification is synthetic software evidence on the recorded macOS runtime with internal APFS output, not biological validity, clinical performance, target binding or unrestricted portability.

No private study data, AIU run, GitHub push or Notion sync occurred. All M8 workers and gate processes are terminal. The next programme step is D-T09/M9 reproduction and final system acceptance; no worker is dispatched for it in this run. **G8 released; G9 closed.**
