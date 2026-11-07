# M8 clean-archive package gate

Immutable candidate **`4c0c59d91b5c8f7117b1097a57e68b0211f0e45a`**, source archive SHA-256 `25314c061841f1fd77f74facf4295eed3f7d536efb1e888301ffa2fc5cfd6ce8`, contains Terra DOC-M8 and the reviewed packaging-only correction. The required command ran once on this corrected archive:

```sh
Rscript --vanilla tools/check-package.R --root "$M8_EVIDENCE/package-correction-1/snapshot/source" --output "$M8_EVIDENCE/package-correction-1/package"
```

Actual exit **0**, exact final **`Status: OK`**. Installed full-suite testthat: **FAIL 0, WARN 15, SKIP 0, PASS 11518**. Started 2026-11-07 07:35:47 UTC; finished 08:35:51 UTC. Sol independently read the actual logs, warning messages/multiplicities and all 1,207 archive source hashes. No source changed. [Machine gate](../evidence/2026-11-07-m8-final-gates/package-gate.json), [check log](../evidence/2026-11-07-m8-final-gates/00check.log), [Sol readback](../evidence/2026-11-07-m8-final-gates/Sol-gate-readback.json).

## Unsuppressed named warnings

- `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes`: **11** (small-view power 3, factor/average-expression correlation 4, empty-max 4).
- `C-05 real MOFA2 fits and extracts a missing sample-view capability cell`: **3** (small-view power 1, inactive-factor-removal-disabled 2).
- `D-T01 refuses stale paired signs and preserves the old pointer`: **1** (intentional native CLI status 2; previous generation preservation asserted).

All names/counts match the accepted M7 baseline. C-05 message text matches exactly; D-T01 differs only in generated execution paths. No new warning names. The [inventory](../evidence/2026-11-07-m8-final-gates/warning-inventory.json) preserves every warning; this is the M7 set of fifteen, not historical M6's twenty-two.

Raw `00check.log` SHA-256: `b45b10e587a201e98bfcbce53a14256348fe4ef9b097243a3ab759532fa04ce6`; raw `testthat.Rout`: `9b0061847a33da3b3028ff85880a480d39f16ed271ec53fa92b2dea967e05b19`. Public copies substitute local path tokens; raw logs remain local. The archive manifest SHA-256 is `0f50d787bad7df6021e1204d4fd741137e0fb9c8b6448570d3fa5763808f2b81`.

The first archive `ac66bd6` remains failed and preserved. Four equivalent Unicode source escapes and one explicit `stats::setNames` import are the only product differences; parsed D13 expressions, tests and all prior attempts are unchanged. No separate D13 packet/native/source suite was rerun. Prior native report proofs remain attributed to their accepted packet commits. This full installed suite includes native command tests.

This gate does not itself accept M8. Fresh immutable Astra and Sol's decision remain required; G8/G9 stay closed at this record's creation. Frozen-kit independent audit remains separate/pending; the inherited AGENTS/CLAUDE checksum discrepancy is disclosed to Astra, not labelled a validator pass. Qualification remains public-synthetic software evidence on the recorded local R/Quarto/MOFA runtime and internal APFS; no private study analysis or clinical validity claim.
