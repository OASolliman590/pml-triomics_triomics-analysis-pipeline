# Sol acceptance — required Stage D scope, M7 and G7

**Sol accepts the eight required M7 Stage D packets and M7, and releases G7 on the single immutable tested product `fabd6fa6bb7ae06f6eea1276517010829ea4cde1` (P2).** B-T07 and the current C-06 producer revision are reconciled and accepted on that same product before accepting their D-T01 consumer. This is the acceptance decision; a test log or worker exit alone was not acceptance. The Git commit containing this receipt records the decision and documentation; it is not claimed as a newly tested product revision.

## Packet decisions

Accepted: **D-T01, D-T02, D-T03, D-T04, D-T05, D-T10, D-T11 and D-T12**, plus B-T07/C-06 prerequisites. The [accepted packet receipts](../evidence/2026-11-04-m7-final-rereview/accepted-packet-receipts.json) store each packet's exact command, P2 commit, actual exit, test counts, native command exits, artifact paths and full SHA-256 values. The audited [original verification receipts](../evidence/2026-11-04-m7-final-rereview/packet-receipts.json) remain unchanged. Acceptance is public-synthetic software evidence only.

D-T06 remains blocked until a release-pinned native model/input/null/resource qualification passes; D-T07 remains blocked until a lawful local quantitative contract exists; D-T14 remains blocked until a quantitative target method is approved. All three receive **zero completion credit**, and their methods remain uninvented. This accepts the required M7 annotation/target scope, not every packet whose ID begins D: M8's D-T08/D-T13/D-T15 and M9's D-T09 remain unstarted.

## Same-product acceptance gates

- One uninterrupted P2 source suite: **7554 pass, 0 fail, 0 error, 15 warnings, 0 skips, 500 blocks**, actual exit0.
- Clean `git archive` of P2, 997 files, tar SHA-256 `f07cfdd02b086ed35369ca37b50aec9f3d1f40041314dc15077a9b76d8f41be6`; `Rscript --vanilla tools/check-package.R --root <archive> --output <logs>` ended with exact **`Status: OK`**. Installed tests: **7527 pass, 0 fail, 15 warnings, 0 skips**, actual exit0. The [gate receipt](2026-11-04-m7-final-rereview-gates.md) links expanded commands and logs. The 27-count source/installed difference is explicitly attributed to measured source counts versus static existing installed branches; it is not a second test run.
- All **24** B/C/D commands across eight required D packets exited0, with independent same-root lineage, persisted-measurement, resource, ORA, target and generation readbacks. Ten applicability cases added **30** native commands, all exit0, **2060** verifier assertions and **323** independent Sol checks. Native checks are separate from testthat totals.
- Fresh [focused final Astra rereview](2026-11-04-m7-final-focused-astra.md): **audit_pass yes**, both initial P1 findings resolved, no blocking findings. Reviewed product P2 and immutable documentation Q2 `4b8b6c8b46a7c2642ed435f350f20359a0ee7037`. Q2 is docs-only relative to P2. The initial final review and one focused rereview are consumed.
- Terra wrote [DOC-M7](../milestones/M7.md) from these receipts in worker commit `07be95bc370b4e6ea6d5dd13b000304fdf2d279f`; Sol inspected and integrated it before freezing Q2 and dispatching the audit.

Each full run retained the same **15 named warnings** without suppression: `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes` (11: small-view power3, factor/average-expression correlation4, empty-max4); `C-05 real MOFA2 fits and extracts a missing sample-view capability cell` (3: small-view power1, inactive-factor-removal-disabled2); `D-T01 refuses stale paired signs and preserves the old pointer` (1: deliberate CLI status2 rejection, old generation preserved). No new warning names. These are not the historical M6 WARN22 set.

## Limits preserved

Qualification is on the recorded macOS runtime with internal APFS output and the documented relative CLI entry invocation. External-SSD AppleDouble rejection and absolute entry-script space bootstrap failure are documented limits, not passing portability results. [Qualification scope](../evidence/2026-11-04-m7-final-rereview/qualification-scope.json) controls those claims. Synthetic tests do not establish biological/clinical validity, pathway activation, direct binding or causation.

C-07's four scientific checks remain not_run. C-09 remains unqualified, synthetic_fixture_only, benefit_guaranteed=false, without a real endpoint, deployment or uncertainty protocol. Frozen-kit independent_audit_status remains pending as a separate audit. Frozen spec, provenance schema, statistical methods, B-T04 bound0.08 and seeds1001:1400 were not changed by these repairs. Failed/capped cycles, including M7-024 and M7-029, remain historical incomplete attempts; they are not relabelled successful.

No private study inputs were read, fit, copied or published. GitHub push, AIU, fasting checks and M8/M9 implementation are outside this receipt. Next milestone is M8 safe offline report/dossier; no worker is started for it in this run.
