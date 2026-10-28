# M7-018 bounded close receipt

## Disposition

The explicit-null defect is fixed and integrated. Broader acceptance gates failed;
M7 and G7 remain closed. No later D packet was started. C-06 accepted remains false,
C-07 stays not_run, C-09 stays unqualified. No private data or GitHub push.

## Immutable candidates and roles

- Preserved base: 6456055d394057baf029eba7321c45ad90a3ec21; D candidate cfe916059c9e2e3b04f64cb96035d8777520b488 unchanged.
- Repair: 774bbcc5f7ad3152e6daf1d57d1972cf99519963. Luna changed only R/cli.R: absent-all missing time becomes named NULL. No helper or test edit, no additional repair.
- Integrated snapshot: d660879ab7d6dbf4e652aa8957452c02ec5e1cdb. Product/test blobs match the repaired candidate.
- Terra 6.1 high independently reproduced 19 passes / 2 failures on the base, then found no scoped findings in the frozen repair. The requested Luna-max and Terra-high settings were accepted by the tool; independently observable effective runtime settings were unavailable.
- Fresh Astra-low: audit_pass no (P0 0; P1 1; P2 2; P3 1); see evidence/2026-10-28-m7-018/astra-review.md. Audit does not accept M7.

The prior D and C candidate chain was integrated serially only after the native
and focused proofs below passed. A transient multi-pick/index issue required
quitting the incomplete sequence, restoring only the coordinator-owned conflicted
test file, and replaying the missing prerequisite and remaining candidates one at
a time. Final product blobs were verified identical to the repaired candidate.
No unrelated dirty user files were committed.

## Independent proofs

| Gate | PASS | FAIL | WARN | SKIP |
|---|---:|---:|---:|---:|
| Native synthetic B → C → D assertions | 21 | 0 | 0 | 0 |
| Focused B-T07, C-06, C-08-shared, D-T01 | 1176 | 0 | 1 | 0 |
| One uninterrupted full source suite | 6280 | 1 | 15 | 0 |
| Clean archive installed testthat | 5266 | 45 | 42 | 0 |

Native B, C and D command exit codes were 0, 0 and 0. The persisted SYN_12
endpoint explicitly contains **time:null**, remains excluded and has no score or
training membership. Both previously failing null assertions remain and pass.
Focused per-file counts: B-T07 315; C-06-shared 355; C-06 87;
C-08-shared 94; D-T01 325. Source failures/errors are distinct from warning counts;
the installed FAIL count includes errors and is not a count of independent defects.

## Provenance read-back (SHA-256)

Synthetic output root: /private/tmp/m7-018-sol-native-output-11fa824268892.
B frame directory: B/handoffs/b603fd22641a03da4301a6f770e7baedd138ccf8d748729e791fc50313aaafb0/.

| Artifact | SHA-256 |
|---|---|
| B stage-handoff-provenance.json | bfad080d3c7bea359f709ddf66bb3b6e91771391edc9db41f452a971014897be |
| B provenance envelope | 13d5e9f8cc4ed91b3f83dd3672eff9aeb2d1bfeb0c25d72f41ee64e9702a83f5 |
| B handoff bundle | 78beedcaf72003e65a306fb91b4b397c3a1b8c54199629a222d1b768acefb84c |
| B handoff envelope | 91cf8791716f0a0aec55056b80ffcbd2e914d98da10823940adeba9b4facc9b9 |
| C-produced D/stage-handoff-provenance.json | 1aa0e0189ecb2b200bd2d66b0ac548a90dcae9f00eedce1d66767261a2036ccd |
| C provenance envelope | e3d529a5c70c38524b1b78452ac29c3ddbc9868137ec8bed797d1b2310f1007f |
| D generation manifest | 942bf940f7a80608f8aa67919d3fd7065f6783a000760d45826283d6e2d44fa1 |
| D registries/current-D.json | 49bcb631409fd4b2d09a9509d8ffdba24af1317a926507bd2c7bfb40eb137b34 |
| D evidence-routing | f3e630cb8fe1f3596226fcefeb4eabad630d85fe6ff85c81f39d88eea324f8e6 |
| D evidence-routing envelope | aaa41ff631f1ac2059d285030557f6b0a02191ff3c4848d7e7e0c496568741bd |
| C/score-endpoints/integration-01.json | 6f63d224d7bcd7f29b74a62898c4fab48b3a366f50977f790eb3685d76546b86 |

Read-back verified all four B predecessor bindings, D manifest member sizes and
hashes, disabled-route accounting, four C-07 not_run controls and conditional C-09.
The 58-file manifest and independent read-back JSON are in the evidence directory.

## Failed acceptance gates and warning accounting

Full-source failure: test-C-08.R, `C-08 preserves mean and delta AnalysisUnitMap
lineage and rejects incompatible times`. The public visit_weights value is a list
but the retained assertion requires a named numeric vector. This derives from
prior c8b309e / integrated 4c421a4, not the one-line null repair. It is unresolved;
no assertion was weakened.

Focused warning: `D-T01 refuses stale paired signs and preserves the old pointer`
(one system2 status-2 warning for the intentional rejection). Source warnings:
that same D-T01 warning plus 11 in `C-05 real MOFA2 fit preserves scattered
missingness and reports seed sensitivity and enrichment hashes` and 3 in
`C-05 real MOFA2 fits and extracts a missing sample-view capability cell`.
Individual warning messages are retained in warning-inventory.md. No warning
suppression was added. Installed CheckReporter reports 42 warnings without their
test names; that set is uncertified, not assumed to match the source set.

Archive commit d660879; tar SHA-256
f5ad40dd46fd22d0495f88971179856ee610707e5328a3f93dbf77d797db03d3.
All 734 archived file hashes were checked unchanged before the Astra audit.
MOFA2 1.22.1 and mofapy2 0.7.5 isolated runtimes were used.

**Status: 1 ERROR, 1 WARNING, 1 NOTE**

Log: /tmp/triomics-m7-018/package/00check.log (durable copy in evidence).
ERROR: installed tests fail, including absent Git revision for native producer
lineage, installed fixture-path problems, and the signed-weight API mismatch.
WARNING: triomics_b07_export_handoff Rd usage omits provenance_context.
NOTE: unqualified setNames in triomics_provenance_validate_profile.
The archive is not accepted. Neither suite nor package check was repeated.
Historical AGENTS/CLAUDE operational-document frozen-hash drift remains separately
recorded; no frozen hashes, methods, schema, B-T04 bound or seeds were changed.

## Close and resume

All run-owned workers and checks have completed. No desktop chats were created.
Literal resume: preserve integrated d660879 and the M7-018 repair; authorize a new
bounded cycle for numeric visit-weight API compatibility and clean-installed
provenance/fixture portability, with named installed warnings and package
documentation diagnostics. Do not reopen M7-017 or loosen lineage checks.
No additional implementation is authorized by this close receipt.
Evidence: evidence/2026-10-28-m7-018/. Software checks on synthetic fixtures do
not establish biological or clinical validity. No milestone acceptance is implied.

Worktree cleanup: managed archive tool refused removal because M7-018 is the task primary checkout. It remains clean at774bbcc with no active worker. Original M7-017 and M7-016 worktrees were rechecked clean at their preserved commits.
