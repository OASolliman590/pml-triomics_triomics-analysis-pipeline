# D13 extra7 — Sol acceptance receipt

Sol accepts **D-T13 only** on immutable product commit `8b18c5ba7547efa06b5bc3e6790ad73b1c34be70`, after fresh Astra audit_pass yes with no findings. Worker `9180c24efd0de955d1f9d6068266c677f958f994` started from `397d7537585b42db3258e17b06aa0360eedf3b24`. Integration was a fast-forward to the exact tested snapshot; no product bytes changed after review. The later record commit adds acceptance metadata, not retested product code.

The bounded repair changes only `R/report_targets.R` and its D13 test file. Aggregate descriptions such as `subjects:32; features selected` and `subjects: 32. Features selected` retain their original text in construction and selection history. The tested attached identifiers remain excluded, and a valid count does not exempt the rest of a mixed string from privacy scanning. Initial attempt 1, original repairs 2 and all seven explicit extra repairs remain recorded (nine repairs total); no post-handoff repair occurred.

| Proof on `8b18c5b` | Result |
|---|---|
| Sol source, D13 + D08 + D15 | PASS 3525; FAIL/ERROR/WARN/SKIP 0; 60 blocks |
| Clean-archive installation and installed focused suite | Install exit 0; PASS 3525; FAIL/ERROR/WARN/SKIP 0 |
| D13 share of either focused run | 3245 assertions, 12 blocks |
| Consolidated independent oracle | 264/264: 134 retain and 130 reject cases |
| End-to-end named case coverage | 447 unique inputs; all 384 earlier inputs/outcomes preserved |
| Former Astra examples | 12/12, a subset of the 264 oracle; no additional execution credit |
| Native A, B, C, D, report | Exit codes 0, 0, 0, 0, 0 on the public D13 fixture |
| Saved artifact readback | 1729 base checks + 456 target checks; B-to-C-to-D bindings verified |
| Deliberate missing-producer/runtime/tamper probes | Expected exits 3, 3, 2; no partial publication |
| Local Spec / Standards / fresh Astra | pass / pass / audit_pass yes |

Warnings and skips: none. Source and installed counts are separate runs, not distinct scientific experiments. The numeric-lineage probe checks distinct text containing counts plus `shared_subjects` and `independent_replication=false`; it does not establish model performance or validate numeric-valued fields.

The offline HTML has 23 tables, zero scripts, and SHA-256 `446f43029ea6c22654851039d1a204b1acceb03998962c45f7a1a72a31bf9996`. The git archive SHA-256 is `08a471403f26c7b7c1506f97bf0cf1d7ae4c7ed9d454877b0f6eeef09c7bd5c7`. [The machine receipt](../evidence/2026-11-06-d13-extra7-accepted/receipt.json) retains commands, exact counts, every native artifact hash, snapshot digests and review evidence. Public command copies use `$SOURCE`, `$EVIDENCE`, `$RUN` and `$RUNTIME`; exact local commands remain in the coordinator's evidence directory. [The oracle script](../evidence/2026-11-06-d13-extra7-accepted/consolidated-lineage-probe.R) and its manifest are preserved for reproduction.

No full source suite or full package check ran in this bounded cycle. Archive installation is **not** `Status: OK`. M8 still requires Terra documentation, the required clean-archive package gate, a fresh milestone Astra audit and Sol acceptance. M9/D09 are not started. **G8/G9 remain closed.** Prior D08/D15 and M7/G7 acceptance, frozen scientific contracts and conditional routes are unchanged. No private study data, server run, GitHub push or Notion sync occurred.

All run-owned agents are terminal; no native process, polling loop or sidebar child remains active. Protected local dirt was preserved.

Literal resume: **D13 is accepted at `8b18c5ba7547efa06b5bc3e6790ad73b1c34be70`. Read this receipt; preserve every prior attempt. Continue the existing M8 documentation and immutable package/milestone-review gates from the acceptance-record HEAD. Do not redispatch D13 or redo its passed packet gates without new evidence. Keep G8/G9 closed until their respective gates and Sol acceptance pass; do not start D09 before G8 release.**
