# M7-017 repair 2 Sol focused review

Immutable candidate `6456055d394057baf029eba7321c45ad90a3ec21`; bounded diff from `7ef25eff64dbb334b5ac75f1051a3ca1dca37a58`. Reviewed remaining R1 and new breakage only. Read-only: no repository edits, children, fitting, commits or acceptance. Requested Sol/high; effective model/effort are not independently exposed. Clean worktree and HEAD verified; diff whitespace check passed.

**Scientific/spec verdict: changes required — Critical 0, Important 1, Minor 0.**
**Code-behavior verdict: no additional distinct finding — Critical 0, Important 0, Minor 0.**
The one remaining issue is counted once. Final comprehensive coordinator gates remain ahead.

## R1 remains Important: persisted unavailable time is the string "NA", not null

The new in-memory exception is appropriately narrow and resolves the producer rejection. Actual configured ingest and B resolution accept the missing-time source; composition and endpoint validation now accept its explicit absent-all exclusion, preserving singleton source `SYN_12`, empty represented blocks, reason and absence from training.

However, the composed `time` remains numeric NA (`R/cli.R:618,700`). `triomics_c08_validate_unit_map` preserves that value in its time list column. Existing `triomics_write_json_atomic` (`R/contracts.R:172–173`) uses `jsonlite::write_json(..., auto_unbox=TRUE, null="null")` without a numeric-NA policy. This serializes NA_real_ as the **string** `"NA"`. Thus repair 2 still fails the required truthful JSON-null handoff, and its own native test's `expect_null(excluded$time)` (`tests/testthat/test-C-06-shared.R:682`) cannot pass against those bytes. This finding is confined to the new value introduced by this repair, not a general audit of the preexisting shared writer.

Evidence:

- `/tmp/triomics-m7-017/sol-repair2-probe.R` invokes the real public synthetic configured ingest, native B resolver/map producer, C composer, endpoint-map validator and actual atomic writer. `/tmp/triomics-m7-017/sol-repair2-probe.log` records `C_COMPOSITION_ACCEPTED`, `EXCLUDED_IN_TRAINING FALSE`, then the written record `"time": "NA"`; `ROUNDTRIP_HAS_TIME_FIELD TRUE`, `ROUNDTRIP_TIME_NULL FALSE`. Revalidating the reparsed map gives `C_SCORE_LINEAGE_INVALID`.
- The worker's actual native endpoint independently has the same bytes at `/private/tmp/m7-017-repair2-native-output-182073a594a3a/C/score-endpoints/integration-01.json`: `SYN_12` is excluded with exact source, empty membership, `condition:null`, but **`time:"NA"`**. This is actual publication output, not only a helper simulation. At readback, C provenance existed and no D pointer was present; no final B/C/D success is claimed here. Parent independently confirmed the same string value in `/tmp/triomics-m7-017/parent-repair2-native-readback.json` (SHA-256 `543c9da2e07feadc7b03423ded62b9886b41de822c46a49b00270629c847549e`).
- A minimal independent jsonlite probe confirms both a list and an AsIs data-frame list column containing NA_real_ serialize to `time:"NA"` under the writer's current options.

A narrow representation at the allowlisted composition seam can preserve an explicit named NULL for this exceptional unavailable time; the endpoint validator already accepts that exact case. A global writer change is unnecessary for this scoped repair and is outside this review's implementation authority. Do not remove the time key, infer a finite clock value, relax represented-unit guards, or count CLI exit status as proof that the null field is correct. The coordinator confirms the implementation repair cycle is at its cap; no integration, full gates, final Astra or further repair is authorized in this cycle.

## Guards verified

- Explicit named NULL is accepted: the probe uses `record['time'] <- list(NULL)`, which preserves the key. This differs from `record$time <- NULL`, which deletes the key and correctly fails the explicit-key guard.
- Eight focused negative cases all refuse: wrong exclusion reason; omitted represented_blocks; nonempty represented_blocks; omitted time field; NaN; Inf; missing time on a retained unit; excluded unit inserted into score IDs. The last returns `C_SCORE_COVERAGE_MISMATCH`; all other cases return `C_SCORE_LINEAGE_INVALID`.
- The new C08 behavioral test passes four assertions independently. It verifies numeric NA in memory and retained/represented negative cases, but does not itself test JSON serialization; the added native test does test the missing null and must remain intact.
- The producer/composition and endpoint conditions retain strict represented/retained finite-time requirements, explicit absent-all eligibility/reason, empty represented membership, singleton source cardinality, declared subject/clock/origin and no excluded score. No separate newly introduced guard defect was found in this bounded diff.

No schema, method, threshold or frozen-manifest change is requested. This is not acceptance: C06 remains false, C07 not_run, C09 unqualified, M7/G7 closed. Parent source/package/Astra gates and the worker native result remain independent required evidence.
