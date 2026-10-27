# M7-017 repair 1 Sol focused review

Candidate: `7ef25eff64dbb334b5ac75f1051a3ca1dca37a58`; reviewed repair diff from `c8b309e5e0a09887e5939a35db641e3665d4f650`. Read-only, no children, edits, commits, fitting or acceptance. Requested Sol/high; effective settings are not independently exposed. Candidate HEAD and clean worktree verified.

**Scientific/spec verdict: changes required — Critical 0, Important 1, Minor 0.**
**Code-behavior verdict: Q1/Q2 addressed; no separate new finding — Critical 0, Important 0, Minor 0.**
One distinct blocker remains. Its observable refusal is counted once under scientific/spec preservation, not again under code behavior.

## Original findings

- **S1 partially addressed:** all B-map source identities, represented blocks and exclusions now survive composition when excluded metadata has a finite time. The new all-source finite-time check rejects a legitimate B-produced excluded row with unavailable time; see R1 below.
- **S2 addressed:** `R/cli.R:730–756,788` validate required type, representation and study/analysis/preparation identity for the top map and every block map before composing/relabeling. New tests exercise all five foreign secondary-map fields.
- **Q1 addressed for reviewed supported routes:** raw molecular identity is checked before conversion to the authorized clinical key; exact-sample non-repeated linkage without a raw subject remains accepted. Source membership for clinical view is now taken from the composed map, and repeated units use the adapter. Independent targeted test passes include valid raw→hashed translation, foreign-link rejection and exact-sample fallback. Retained public C08 native output schedules one B-owned request without B results, D provenance or run.json.
- **Q2 addressed:** run-owned manifest recording and cross-test root lookup are removed. D's stale-sign test creates its own public synthetic config, runs its own B/C/D sequence, mutates the actual YAML fields and cleans its files. No `triomics-m7-017-luna` or `native-roots` references remain in the three affected test files. Supplied standalone D log records seven passing assertions and one named expected `system2` warning for intended exit 2; it is not silent test-order evidence.

## R1 — Important: retain an unavailable time on a genuinely excluded B source row

**Lines:** `R/cli.R:575–582` (new full-universe check); the corresponding endpoint consumer `R/integration_endpoints.R:182–195` also requires a finite time unconditionally, including excluded rows.

Repair 1 broadens `used_rows` to all B-map samples and demands finite time even when a source sample is absent from every block and explicitly excluded by B. This adds an observation-time requirement to a source that never enters the repeated model. The failure is concrete and comes through the real configured ingest and B resolver, not a manually fabricated map:

1. Copy the public synthetic trajectory input locally.
2. Remove source `SYN_12` from both block matrices; leave its metadata row and subject identity.
3. Set only that unused source's time to numeric NA.
4. Real `triomics_ingest_configured_study` accepts it, and real `triomics_resolve_analysis` succeeds without fitting.
5. Real `triomics_b_provenance_unit_map` emits `eligible=FALSE`, `exclusion_reason=absent_from_all_analysis_blocks`, exact source membership `SYN_12`, and zero represented blocks.
6. Repair 1 composition refuses the entire eligible integration with `C_SCORE_LINEAGE_INVALID: Trajectory source metadata has incomplete subject or finite time identities`.

Reproducer: `/tmp/triomics-m7-017/sol-repair1-time-probe.R`; evidence: `/tmp/triomics-m7-017/sol-repair1-time-probe.log`. Retained fully public synthetic input: `/private/tmp/sol-repair1-missing-time-16c44402666a3`. Parent independently reproduced the same result. A separate non-fitting endpoint probe confirms both numeric NA and NULL time are refused for this excluded row: `/tmp/triomics-m7-017/sol-repair1-endpoint-time-probe.R` and `.log`.

**Minimum existing-scope repair:** distinguish B's explicit absent-from-all excluded rows from represented observations, preserving their exact source identity, empty represented membership, exclusion and unavailable time as NA/null. Keep finite time mandatory for retained/represented observations and preserve declared time unit/origin. At the already allowlisted endpoint consumer, narrowly allow unavailable time for this explicit no-score absent-all exclusion, while keeping existing strict retained-unit, source, representation and time guards. Do not infer a finite time or discard the row. The existing coverage check (`R/integration_endpoints.R:136–139`) already prevents excluded IDs from appearing among score rows; endpoint construction stores the full map (`607`) but emits samples from retained score-unit rows (`500,548–567`). No new schema, result field, statistical method or numerical policy is required. Verify null serialization/hash read-back and negative retained-time tests before native reruns.

## Evidence checked

- Independently reran only the three non-fitting new regression blocks via the bounded runner: 20 + 15 + 5 assertions passed, `/tmp/triomics-m7-017/sol-repair1-small.log`.
- Independently checked all ten hashes in `parent-attrition-readback.json`: zero mismatches. Actual paired endpoint preserves six unit records, four score/training units, and two different per-block exclusions with the correct one-source versus two-source maps. The trajectory root has twelve sample units with declared times/clock/origin and no invented visits. Neither retained native example contains an absent-all missing-time source, so neither exercises R1.
- Full focused fitting gate is still owned by the active worker. No suite pass, clean archive result, Astra result, acceptance, C06 completion or M7/G7 release is inferred here. C06 remains false, C07 not_run, C09 unqualified; downstream work stays closed.
