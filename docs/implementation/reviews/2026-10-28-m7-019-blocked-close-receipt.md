# M7-019 bounded close receipt — blocked close

M7-019 exhausted its initial implementation and two repairs. No third repair is
permitted in this cycle. M7 and G7 remain closed, C-06 remains unaccepted, C-07
not_run, C-09 unqualified, and no later D packet has started.

## Snapshot-specific evidence

| Snapshot | Gate | PASS | FAIL | ERROR | WARN | SKIP |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 0668d07 initial | focused | 1999 | 10 | 1 | 2 | 0 |
| 859fa9d repair 1 | focused | 2023 | 0 | 0 | 1 | 0 |
| 98c7125 integrated repair 1 | uninterrupted full source | 6287 | 0 | 0 | 15 | 0 |
| e410486 repair 2, not integrated | final focused | 1627 | 12 | 5 | 7 | 0 |

The 98c7125 source gate is not a pass of e410486. The latter is preserved as a
failed candidate in the isolated M7-019 worktree. There is no successful native
proof or full-source gate for that candidate.

Scope audit passed at e6f9b29 after one documentation correction. Luna-max
implemented the initial attempt and two repairs; Sol reviewed and ran the gates.
Requested model settings were accepted by the tools; effective runtime settings
were not independently exposed. No desktop worker chats were created.

## Native proof and provenance

Repair 1 (859fa9d, product/test blobs matched integrated 98c7125) passed 21 native
assertions. Synthetic Stage B, C and D commands exited 0, 0 and 0. Persisted
SYN_12 time is JSON null, excluded and without score/training membership. An
independent read-back matched all four B predecessor hashes in C and the D
manifest member bytes, sizes and profile hashes.

Root: `/private/tmp/m7-019-sol-native-output-4f5049d308ee`.
Exact relative paths and all hashes are recorded in
[evidence manifest](evidence/2026-10-28-m7-019/native-sha256-manifest.txt).

| Artifact | SHA-256 |
| --- | --- |
| B provenance payload | 8a1b92340912ca32c307f28a1a416212d9a4acacea6dbbfc8b628cd0b8656eb4 |
| B provenance envelope | 6a24c7c75f0c87dcf5b4e58fa2ca21779b66bf4460e42c596814b387bda6a1b1 |
| B handoff payload | d98652790cf6d2460a84d62d150995fe8a3f0aeb2e793569fe7a98bf13e88630 |
| B handoff envelope | f826c693736143f40a427359147122c9951d9e51d08d4ed30635772d5c648531 |
| C-produced D/stage-handoff-provenance.json | 33e8d5628e5cee8578b2eb245f7c7f835c4538948f39d740677a7cfc673e9edc |
| C provenance envelope | 0bfdb4d14cacd2c15a50ad59efb6beb96b30a2e0d02f702fe14f891d6d16f8a6 |
| D generation manifest | e2daafc749a8b3e01e3d0bee5b098dd6d280e73e0f729846d340f5d4671b9850 |

## Remaining defects

1. Integrated 98c7125 accepts coordinated numeric visit-weight name changes
   across every endpoint component and rehashes the resulting payload. The source
   suite passed without detecting this; the parent independent probe did detect it.
2. Final candidate e410486 binds these names and its 39 narrow assertions pass,
   but its hash-copy normalization deletes explicit `visit_weights:null` keys
   from row-list JSON read-back. Native C rejects valid endpoints with
   `C_SCORE_ENDPOINT_HASH`. The two representations must hash equally without
   weakening source-name, score, retained-time or null requirements.
3. Installed B-T01 fixture relocation uses read_yaml/write_yaml and scalarizes
   singleton sequence fields. A read-only installed probe fails at
   `blocks[1].source_refs`: expected an array. Executing installed source identity
   correctly resolves 98c71256e61c4613833f81bf049de06427c9a094. The validator must
   remain strict; relocation must preserve YAML field types.

## Immutable archive

Archive commit: 98c71256e61c4613833f81bf049de06427c9a094.
Tar SHA-256: 86a65418dd5c65c8148f06a04f709e50147fa58f647d3dc82693b39e7a3cdcdd.
All 751 archived source files were independently rehashed unchanged. The checker
uses that extraction, never the dirty live checkout. The existing isolated MOFA2
1.22.1 and mofapy2 0.7.5 paths were supplied. The installed test process nevertheless
reported both unavailable; successful runtime use is not established by this gate. Documentation, usage and R-code diagnostics
passed before tests; final package outcome is recorded below.

## Evidence limits and protected state

The 15 source warnings are 14 MOFA small-fixture diagnostics across the two named
C-05 tests, plus the named D-T01 stale-sign rejection warning. Exact messages and
names are in full-before-name-binding.json. The seven failed-candidate warnings
are separately named in focused-final.json and are not disposed as harmless.

Spec validation still reports historical frozen AGENTS.md hash drift. No frozen
hash was rebaselined. Statistical methods, provenance schema, B-T04 0.08 bound,
seeds 1001:1400, and original 6456055/cfe9160 candidates remain unchanged. The
synthetic checks are software evidence, not biological or clinical validation.
No private study data were read, no GitHub push or Notion sync occurred, and
unrelated dirty .Rbuildignore, .gitignore and renv/activate.R remain unstaged.

## Completed installed package gate

Exact result: **Status: 1 ERROR**. Shell exit 1. Testthat reports
**FAIL 24 | WARN 5 | SKIP 0 | PASS 5877** (FAIL includes errors).
Read [00check.log](evidence/2026-10-28-m7-019/package/00check.log),
[complete test output](evidence/2026-10-28-m7-019/package/testthat.Rout.fail), and
[named inventory](evidence/2026-10-28-m7-019/package/warning-inventory.json).

The installed failures include B-T01 YAML sequence scalarization, C-06 sensitive
fixture A_SOURCE_MISSING, and C-05 runtime unavailable (MOFA2/mofapy2 version
checks NULL), followed by failed extraction/missingness assertions. The exact
cause of runtime invisibility within R CMD check is not established. Do not
substitute source-runtime success for installed capability. No rerun or repair
was performed after this result.

All five warnings are named, with none suppressed:

| Test | Count | Disposition |
| --- | ---: | --- |
| Stage B CLI writes unique, deterministic BlockResult identities across contrasts | 3 | Two status-2 commands and missing BlockResult output; unresolved failure symptoms |
| native C-06 writer propagates sensitive privacy and binds D lineage | 1 | Missing envelope after A_SOURCE_MISSING; unresolved failure symptom |
| D-T01 refuses stale paired signs and preserves the old pointer | 1 | Expected negative-test status-2 warning; unchanged pointer asserted |

This is not the source warning set. Absence of the 14 native MOFA fixture warnings
reflects failed runtime availability here and is not evidence of improved fits.
The package archive's 751 files were rehashed unchanged after completion.

## Final audit and close

Fresh Astra-low returned **audit_pass: no**, with four P1 findings: integrated
weight-name integrity, installed fixture portability, installed runtime
qualification, and the unintegrated null-hash regression. The full verdict is
[recorded here](evidence/2026-10-28-m7-019/astra-final.md). Sol records the verdict;
neither milestone acceptance nor G7 release occurred.

All run-owned workers and the package process have completed. No third repair,
second package run or new downstream work was started. Close is blocked, not
completion of M7. The failed candidate is preserved at e410486.

Literal resume: Authorize a new bounded cycle from preserved 98c7125/e410486 to
bind numeric visit-weight names while preserving explicit null keys, preserve
YAML sequence types and installed fixture paths, and diagnose installed MOFA
runtime visibility. Retain all failing tests and strict validators. Audit scope,
use a fresh bounded worker, then focused/native/source/archive gates and a fresh
read-only Astra review. Do not reopen M7-019 as repair 3 or release M7/G7.

Managed-worktree cleanup was attempted. The app refused removal because the
M7-019 worktree is the task primary checkout. It remains clean at e410486 with
no active worker; no older worktree was archived or altered.
