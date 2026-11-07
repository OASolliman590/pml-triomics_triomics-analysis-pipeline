# Sol acceptance — D-T09, M9 and G9

**Sol accepts D-T09 and M9 and releases G9 on immutable product `2f8c83727be9f693d0e60457024edc55474c006b`.** G7 and G8 remain released on the same ancestor line. The later commit containing these acceptance records is documentation only, not a newly tested product. No additional packet is started.

The existing Luna worker completed D09 within one initial attempt and two repairs. Both bounded reviews passed; worker and independent Sol focused suites each passed 218 with no failures/warnings/skips on `8a82a64`. Sol integrated identical reviewed product blobs serially. Terra's one draft and one finalization were reviewed and integrated before Q. No D13 or M8 repair was reopened; prior failures and attempt counters remain.

## Final evidence on Q

| Gate | Actual result |
| --- | --- |
| Uninterrupted full source | exit 0; PASS 11766, FAIL 0, ERROR 0, WARN 15, SKIP 0; 604 blocks |
| Clean Git archive package | exit 0; **Status: OK**; PASS 11739, FAIL 0, WARN 15, SKIP 0 |
| Installed paired and independent walkthroughs | Both actual outer Quarto renders exit 0; each A/B/C/D/report sequence exit 0; figures, tables, manifests and same-run provenance bytes read back |
| Unchanged standalone D-T09 ticket | A/B/C/D/report all exit 0; ticket and producer/report hashes verified |
| Installed clinical missingness smoke | A/C-schedule/B/C-publish/D/report all exit 0;12 subjects,11 included,1 clinical exclusion,0 endpoint exclusions; detailed JSON and HTML agree |
| Fresh Astra-low | **pass; audit_pass yes**; no blockers or missing required runtime evidence |

Archive SHA-256: `d00a44e00309f946780a07221a33c6d1152f8fb1895017c9f24f35629812c67d`. All 1,242 archive files were unchanged after tests and audit. Package `00check.log` SHA-256: `32470ee8fc5517dd68def602f8980081c2b80ba4c5776e5dee741e99e8bf359a`. [Machine receipt](../evidence/2026-11-07-m9-final-gates/receipt.json) stores commands, separate counts and hashes; [native read-back](../evidence/2026-11-07-m9-final-gates/native-readback.json) stores full provenance and rendered-output hashes. [DOC-M9](../milestones/M9.md) provides rerun commands and the qualified environment.

Each gate's 15 unsuppressed warnings have the same named dispositions: `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes` (11), `C-05 real MOFA2 fits and extracts a missing sample-view capability cell` (3), and `D-T01 refuses stale paired signs and preserves the old pointer` (1 intentional StageD exit 2). MOFA text/frequencies match after whitespace normalization; D01 differs only by generated paths. [Warning details](../evidence/2026-11-07-m9-final-gates/named-warnings.json).

The original clinical harness exit 1/KeyError remains preserved. It incorrectly requested D15 detailed exclusion columns from the compact legacy TSV. The separate read-only correction checked common fields in compact JSON/TSV and detailed JSON, plus exclusions in detailed JSON and actual HTML. It did not rerun any pipeline command, fit or render. Astra accepted this disposition. Source/package totals differ by 27 assertions; both actual results are reported separately, without claiming identical paths or inventing a complete explanation. See [Astra qualifications](2026-11-07-m9-astra.md).

## Final scope and limits

All 41 packet identities and 29 requirements are reconciled: **37 required packets accepted;4 conditional packets receive zero completion credit**. The [scope receipt](../evidence/2026-11-07-m9-final-gates/scope-reconciliation.json) preserves before/after states and historical references. B-T04's two ignored runtime receipts are available as local companions whose full SHA-256 matches the immutable tracked review; they are not falsely represented as archive members. M2/M5 historical reconstruction limits remain.

C-07's four controls stay not_run. C-09 stays unqualified, synthetic_fixture_only, benefit_guaranteed=false. D-T06/D-T07/D-T14 remain blocked without credit. Clinical approved_aggregate release remains unavailable and fails closed. Optional report QC export is visibly unavailable; this does not claim all optional report views. The pending frozen-kit independent audit and inherited AGENTS/CLAUDE manifest discrepancy are unchanged. The B-T04 bound 0.08 and seeds 1001:1400 are unchanged.

Acceptance covers synthetic software execution on the recorded qualified local runtime, not biological or clinical validity, patient-level deployment, target binding, fresh-machine restoration or unrestricted portability. No private study data, GitHub push, AIU run, Notion sync or fasting software checks occurred. Protected dirty files were preserved. All run-owned workers and gates are terminal; no polling, relaunch or new work is left active. **G9 released; run closed.**
