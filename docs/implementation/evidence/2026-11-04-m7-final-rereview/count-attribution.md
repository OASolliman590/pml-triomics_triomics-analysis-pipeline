# Source versus installed counts on P2

Product `fabd6fa6bb7ae06f6eea1276517010829ea4cde1`: observed source **7554**, observed installed **7527**, difference **27**. Both have zero failures/skips and 15 named warnings. No tests were rerun for this note.

Source per-test counts below were read from the completed testthat-results.rds; [extraction](source-installed-count-attribution.json). Installed per-test counts are **static counts of existing installed-layout branches**, not a measured per-test report. The actual installed aggregate comes from package/testthat.Rout, whose digest is recorded in gate-summary.json.

| Existing test | Observed source pass | Installed branch expectations (static) | Difference |
|---|---:|---:|---:|
| C-03 fixture is schema-valid and shared registration remains coordinator-owned | 11 | 6 | 5 |
| C-04 registration request records the applied shared contract change | 7 | 2 | 5 |
| C-05 environment check reports both runtimes as not loaded from an empty library and exits non-zero | 5 | 3 | 2 |
| C-05 registration records the accepted synthetic-software-only route | 16 | 1 | 15 |
| Total | 39 | 12 | 27 |

These four unchanged source/installed branches account exactly for the aggregate difference. Source-only schema/registration/tool availability checks have packaged-layout alternatives. Native model fits and the new target-only command regression executed in the actual installed suite; no required skip was credited.
