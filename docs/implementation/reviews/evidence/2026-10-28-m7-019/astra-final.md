# Fresh Astra-low M7-019 close audit

audit_pass: **no**.

- Reviewed integrated `98c71256e61c4613833f81bf049de06427c9a094` and unintegrated repair `e41048693bebdec93d73f4aeb0ec207a81cd9299`; archive verification records 751 unchanged files.
- **P1, integrated:** named numeric visit weights omit names from endpoint hashes. Coordinated name changes survive validation and are serialized with fresh hashes (`R/integration_endpoints.R:484`, `R/cli.R:1084`; confirmed probe).
- **P1, installed gate:** B-T01 relocation scalarizes YAML arrays; C06 sensitive-lineage fixture fails `A_SOURCE_MISSING`. Required installed assertions cannot complete.
- **P1, runtime qualification:** installed C05 reports unavailable MOFA runtime and NULL package versions. Cause remains unproven; source runtime success cannot qualify this installed run.
- **P1, unintegrated repair:** assigning NULL removes explicit `visit_weights` fields during hash normalization, breaking JSON round trips; focused evidence: 1,627 pass, 12 failures, 5 errors, 7 warnings, zero skips. Do not integrate.
- Integrated source evidence: 6,287 pass, zero failures/errors/skips, 15 warnings. Installed gate: exit 1, exact `Status: 1 ERROR`; `FAIL 24 | WARN 5 | SKIP 0 | PASS 5877`.
- Installed warning inventory contains three B-T01 warnings, one C06 missing-envelope warning, and one expected D01 stale-sign warning. Source MOFA warnings are not installed evidence.
- Source-identity boundary and retained SYN_12 null/predecessor evidence support bounded improvements, not overall acceptance. Read-only audit; no tests rerun or scientific-validity claim.
- Attempt cap exhausted: blocked close; M7/G7 remain closed, C06 accepted=false, C07 not_run, C09 unqualified.

Coordinator: requested gpt-6-astra / low / fresh context accepted by dispatch;
effective runtime settings not independently exposed. No edits, tests or children.
