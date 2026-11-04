# M7 final same-snapshot software gates — Sol verification

Tested product commit: `126a30fc68fd593826a3ca82cd399fff35ee90ef`. Gates passed; Terra DOC-M7, fresh milestone Astra audit and Sol acceptance remain pending. M7/G7 are closed.

One uninterrupted full source run: **7353 pass, 0 fail, 0 error, 15 warnings, 0 skips**, 493 test blocks; exit 0. Started 2026-11-04 01:37:50 UTC; finished 2026-11-04 02:03:54 UTC.

Clean git archive: `1c1b0473df6285fe2a073dff2d3195bc0de0dd6e9be287213d8c64b1d6daa202`, 945 files, exported source stamp exactly P. Archive files were unchanged after checking. Exact package line: **`Status: OK`**. Installed testthat: **PASS 7326, FAIL 0, WARN 15, SKIP 0**.

The 27-expectation source/installed difference is accounted for by existing layout-specific checks; see [count attribution](../evidence/2026-11-04-m7-final/count-attribution.md). Installed per-test counts in that note are static branch counts, not a second measured test report.

The source and archive each contain the repaired M7-028 tests. Earlier failed 48ca0c4 evidence is preserved and is not credited here. No production code changed in M7-028. The original dirty SSD checkout was not tested or archived.

All 24 fresh native B/C/D commands exited 0 on P. C predecessor digests matched all four B files in the same root. Sol separately checked actual B effects/C loadings, source/selection/dependency groups, external assay endpoints, unranked discovery and full member accounting, exact ORA counts/probabilities, resource statuses, generation manifests and raw file hashes. Shell success alone was not used.

| Packet | Source pass / warnings | Same-P native B/C/D |
|---|---:|---|
| B-T07 | 320 / 0 | 0 / 0 / 0 (D-T01 predecessor proof) |
| C-06 | 442 / 0 | 0 / 0 / 0 (D-T01 predecessor proof) |
| D-T01 | 344 / 1 | 0 / 0 / 0 |
| D-T02 | 87 / 0 | 0 / 0 / 0 |
| D-T03 | 62 / 0 | 0 / 0 / 0 |
| D-T04 | 38 / 0 | 0 / 0 / 0 |
| D-T05 | 71 / 0 | 0 / 0 / 0 |
| D-T10 | 65 / 0 | 0 / 0 / 0 |
| D-T11 | 262 / 0 | 0 / 0 / 0 |
| D-T12 | 41 / 0 | 0 / 0 / 0 |

Counts are attributed by matching packet test files inside the uninterrupted full run; C-06 includes its shared tests. Shared D suites also ran and are included in the full total, not added again to the packet rows. Each packet receipt stores exact command, P, exit, counts, native root and full SHA-256 values.

## Named warnings

| Test | Source | Installed | Disposition |
|---|---:|---:|---|
| `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes` | 11 | 11 | Native MOFA small-fixture power, correlation and low-variance diagnostics; retained without suppression. |
| `C-05 real MOFA2 fits and extracts a missing sample-view capability cell` | 3 | 3 | Native MOFA small-fixture diagnostics; retained without suppression. |
| `D-T01 refuses stale paired signs and preserves the old pointer` | 1 | 1 | Expected native CLI exit 2 from intentional stale-sign rejection; prior generation preservation is asserted. |

The actual current warning messages and multiplicities are preserved in gate-summary.json and the raw inventory. These are not relabelled as the older M6 22-warning inventory. No warning was suppressed.

## Evidence and limitations

- [Gate summary](../evidence/2026-11-04-m7-final/gate-summary.json), [packet receipts](../evidence/2026-11-04-m7-final/packet-receipts.json), [native hashes](../evidence/2026-11-04-m7-final/native/native-receipt.json), [archive manifest](../evidence/2026-11-04-m7-final/archive-receipt.json), [package log](../evidence/2026-11-04-m7-final/package/00check.log).
- Semantic readbacks and exact verification scripts are adjacent. Full logs, RDS and public synthetic artifacts remain local at the external root recorded in the gate summary; log hashes are committed.
- Runtime qualification is this macOS host: R 4.6.1, MOFA2 1.22.1, mofapy2 0.7.5, DEqMS 1.30.0. No cross-platform claim.
- C-06 current revision is verified but unaccepted pending this milestone decision; historical M6 acceptance remains historical.
- D-T06, D-T07 and D-T14 remain blocked without completion credit. C-07 scientific checks remain not_run; C-09 remains unqualified, synthetic_fixture_only, benefit_guaranteed=false.
- Frozen kit independent_audit_status stays pending as a separate audit. No schema/statistical/B-T04 threshold/seed changes; no private study data, AIU run, fasting checks, M8/M9 work or GitHub push.
- Next transition: Terra DOC-M7, then one fresh read-only Astra-low milestone audit of P and these immutable records; Sol alone decides acceptance afterwards.
