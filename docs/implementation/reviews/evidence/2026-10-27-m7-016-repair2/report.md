# M7-016 D-T01 worker report

## Status

- Implemented the authorized D-T01 verifier, router, generation publisher, D-only CLI executor/registry enablement, namespace exports, matching API docs, corrected fixture, focused adversarial tests, and registration request. No acceptance or commit was performed.
- Dirty files are exactly the nine modified files reported by `git status --short`: `NAMESPACE`, `R/annotation_router.R`, `R/cli.R`, `R/evidence_bundle.R`, `R/stage_registry.R`, `docs/implementation/registrations/D-T01.json`, `inst/examples/tickets/D-T01.yaml`, `man/triomics-api.Rd`, and `tests/testthat/test-D-T01.R`. `R/resource_manifest.R` remains untouched. All are in the authorized ten-file allowlist.
- `git diff --check` passed. Current diff SHA-256: `0abfb0ef0a7428a7ce9d6c6dd36c8bf37e8bc2c02d6331db73cf88877b5f6d30`.

## RED/GREEN

- Behavioral RED: `/tmp/triomics-m7-016-luna/nomination-red.log`; 8 failed test outcomes (7 errors plus 1 assertion failure) and 1 warning. This exposed missing-profile acceptance expectations in old fabricated consumer fixtures, structured C integration selection-rule rejection, and the `/var/folders` output-root alias.
- Final GREEN command: `Rscript --vanilla -e 'testthat::test_local(filter="D-T01", reporter="summary", stop_on_failure=FALSE)'`.
- Final result: exit 0, 208 passed expectations, 0 failures, 0 warnings, 0 skips. Log: `/tmp/triomics-m7-016-luna/final-focused-green.log`.

## Native publication/readback evidence

- Focused test runs actual Stage B, Stage C, and Stage D CLI commands with `inst/examples/tickets/D-T01.yaml` under a fresh canonical `/private/tmp` root. It validates the exact current pointer, closed manifest/member shape, manifest and envelope hashes, complete three-file generation inventory, exact B/C provenance input descriptors, and unchanged B/C/run registry bytes.
- Independent retained synthetic root: `/private/tmp/triomics-m7-016-luna/native-debug`.
- Pointer SHA-256: `7d2f9476bad8461eaf2c4db2fb643529330c0bdf678bdb59bf19c264a5bd63b6`.
- Generation ID and manifest SHA-256: `6d9817bd148eaa44c261ed5d8ccc58fee9cd8ab91ccabdd818e219a2776b06f9`.
- Generation inventory: `generation-manifest.json`, `evidence-routing.json`, and `evidence-routing.envelope.json` only. Routing payload SHA-256: `b7f67369603f3e60e3eeb782ffde069e7e74bbe605544eebfebd54d60bdc73f9`; envelope SHA-256: `b422df0fc483e33d1131e611ae331cd633f68d01475d9a6ba58a8a084509169e`.
- Readback verified B profile/payload-envelope hashes (`9206df3f…e6c7c35d`, `4e5a5a6e…216fd93a`), B bundle (`1b181310…ec3562`), C profile/payload-envelope hashes (`9262e295…92bac7`, `9e075d8f…4a947a2`), and C manifest/payload-envelope hashes (`60ca4846…15abf81`, `32d55173…cb34b6b`). Full output: `/tmp/triomics-m7-016-luna/final-readback.log`; verifier script: `/tmp/triomics-m7-016-luna/final-readback.R`.
- Limitations read back unchanged: C-06 unaccepted; C-07 controls `not_run`; C-09 unqualified and conditional; disabled routes make no biological absence claim.

## Adversarial/recovery coverage

- Native D readback rejects changed current config, source hash, normalized metadata, or FeatureRegistry with `D_LINEAGE_MISMATCH`; missing producer profile with `D_PROVENANCE_REQUIRED`; duplicate-key provenance and semantically identical key-reordered predecessor envelope with `D_PROVENANCE_INVALID`.
- The native D executor rejects a symlink output root with `D_OUTPUT_PATH_UNSAFE`; generation validation rejects an undisclosed extra file.
- Injected `provenance_preflight_complete`, `temporary_generation_created`, `payload_written`, `envelope_written`, `manifest_sealed`, `generation_renamed`, `pointer_prepared`, and `before_pointer_rename` failures return `D_PUBLICATION_FAILED` while preserving old pointer/generation bytes. `after_pointer_rename` recovers a complete current generation.
- Existing profile-less synthetic consumer checks were reclassified as explicit `D_PROVENANCE_REQUIRED` rejection or pure in-memory routing/model tests. Direct B/C parser unit checks remain. The native B/C/D CLI test now owns consumer/publication/readback assertions, including inherited failed/non-nominated B evidence and C scientific limitations.

## Remaining gates

- Sol owns independent affected, full-suite, archive, review, integration, and acceptance gates. The registration request still has `accepted: false`. No dependency, schema, statistics, or numerical-contract change was made.

## Repair 1 — bounded review response

### Frozen state and scope

- Sol froze this repair snapshot at `f23b5a7c96cec1134b6d098850e90fbbfc420299` after the worker edits. The worker made no commit. Worktree status was clean at freeze.
- Repair edits were limited to the authorized files `R/evidence_bundle.R`, `R/annotation_router.R`, `R/cli.R`, and `tests/testthat/test-D-T01.R`; no B/C implementation, schema, dependencies, statistics, or numerical methods changed.
- The earlier section's 208-expectation GREEN belongs to the pre-review implementation and is not evidence for this repair.

### Repaired review findings

- Native C sidecar ancestry, payload, and envelope paths are validated before enumeration, normalization, or read. D rejects linked and dangling `C/native-states`, `C`, payload, and envelope paths as `D_OUTPUT_PATH_UNSAFE`.
- Exported `triomics_annotate_evidence()` now requires a verified same-root B/C persisted proof and re-reads it before routing. Contextless proof gets `D_PROVENANCE_REQUIRED`; changed in-memory evidence gets `D_PROVENANCE_INVALID`.
- Generation readback binds payload member type/role, envelope type/role/pairing, the complete canonical B/C descriptor projection, the envelope's B/C input pairs, and manifest/payload study identity. Re-sealed semantic conflicts are rejected as `D_PROVENANCE_INVALID`.
- Successful native DEqMS readback checks current count-table bytes and every persisted count-prior row's feature, count type, source sample IDs, count minimum, acquisition, provenance, summary method, and metadata digest. D does not refit the model or rebuild its design hash.

### Repair verification

- Parse and whitespace checks passed: `/tmp/triomics-m7-016-luna/repair1-parse-final.log`; `git diff --check` passed before Sol froze the snapshot.
- Final focused command: `Rscript --vanilla -e 'testthat::test_local(filter="D-T01", reporter="summary", stop_on_failure=FALSE)'`.
- Final focused log: `/tmp/triomics-m7-016-luna/repair1-focused-final.log`. The summary reporter emitted 317 success dots and `DONE`, with no failure, warning, or skip blocks. The runner did not save a structured test-results object, so exact testthat totals beyond the 317 success markers are unavailable; the suite was not rerun solely to obtain counts.
- The named native suite `D-T01 publishes a complete generation from actual native B and C CLI handoffs` exercises source/FeatureRegistry/normalized metadata drift, duplicate-key and byte-reordered predecessor proof, C ancestry/payload/envelope link hazards (including dangling links), prepared matrix/state changes, manifest/envelope semantic conflicts, closed inventory, publication path hazards, pointer ambiguity, failed rename with a verified old pointer, and precommit immutability.
- The named native suite `D-T01 revalidates native DEqMS group sign, time maps, counts and source bytes` executes qualified DEqMS 1.30.0 through native B/C/D, then checks actual measurement-byte drift, group-contrast reversal, nonempty subject/visit/time/time-origin metadata drift, current count-table value drift while config and B/C artifacts are unchanged, and direct count/count-digest prior tampering. Full native readback also rejects a tampered persisted B count-prior artifact.

### Remaining required acceptance evidence

- Native paired/trajectory lineage coverage remains unresolved. Sol's retained fresh composition diagnosis is `/tmp/triomics-m7-016/parent-c-boundary.log`: paired B exits 0, C exits 2 with `C_ELIGIBILITY_UNKNOWN_UNIT` for `block_1` because matrix sample IDs do not match the declared unit map; trajectory B exits 0, C exits 2 with `C_SCORE_LINEAGE_INVALID` because C-08 cannot obtain an AnalysisUnitMap from resolution. Fresh output roots are `/private/tmp/sol-paired-boundary-81e71c7a0cf7` and `/private/tmp/sol-trajectory-boundary-81e71e40580b`.
- D-T01's trajectory-derived public config requested exploratory `concat_pca`, with no clinical objective or separate endpoint request. The C CLI still routes `concat_pca` through C-08 unit-map handling and endpoint bundle writing; its only local fallback map is for `levels`, while trajectory is `subject_centered`. The paired failure occurs earlier, inside C integration eligibility. Avoiding either boundary would require changing B/C behavior or contract outside this worker's allowlist. The two-group DEqMS fixture's subject/visit/time metadata and reversed group contrast are not being claimed as native paired-sign or repeated-time-origin proof.
- These required paired/trajectory cases are not waived. Repair 1 therefore does not claim full acceptance readiness, integration, or completion of M7/G7. C-06 remains unaccepted, C-07 controls remain `not_run`, C-09 remains unqualified, and no downstream D route was executed.

## Repair 2 — installed recovery-probe bootstrap (test-only)

- Changed only `tests/testthat/test-D-T01.R`. The recovery subprocess now checks for actual `R/*.R` source files before sourcing. For installed-package mode it calls `loadNamespace("triomics", lib.loc=dirname(package_root))`, copies the required private bindings into the child environment, and rebinds the publisher closure to a private overlay whose parent is that exact namespace. The validator, generation-path, `tempfile`, and `file.rename` failpoint replacements are assigned/restored in the publisher environment, so installed-mode calls exercise the same real publisher and injections as source mode.
- Reproduced the pre-fix failure with the reviewer’s installed-bootstrap script: `/tmp/triomics-m7-016-luna/repair2-installed-bootstrap-red.log` (`R_source_count=0`, namespace not loaded, `triomics_read_config` undefined). Original reviewer artifact: `/tmp/triomics-m7-016/sol-repair1-installed-bootstrap.log`.
- Installed only the candidate package into isolated `/private/tmp/triomics-m7-016-luna/repair2-installed-lib` using `R CMD INSTALL --no-test-load --library=/private/tmp/triomics-m7-016-luna/repair2-installed-lib .`; no dependencies or shared library were modified. Install output: `/tmp/triomics-m7-016-luna/repair2-install.log` (`* DONE (triomics)`). The project’s renv bootstrap emitted an offline warning before R CMD INSTALL continued; no dependency installation was attempted.
- Recovery-only proof extracted the exact `writeLines()` recovery script expression from `test-D-T01.R` and ran it against cloned retained native B/C evidence, without rerunning B/C or the full test suite. Driver/command: `/tmp/triomics-m7-016-luna/repair2-run-probe.R` via `R_LIBS=/private/tmp/triomics-m7-016-luna/repair2-installed-lib Rscript --vanilla /tmp/triomics-m7-016-luna/repair2-run-probe.R`; combined output: `/tmp/triomics-m7-016-luna/repair2-source-installed-run.log`.
- Source subprocess passed in `/private/tmp/repair2-source-root-936b36f7db48`; it printed `probe_bootstrap=source` and all expected recovery/path/failure markers. Final pointer SHA-256: `5389f10bd634a14538a1c1bc296e77d6bf085071d241af42c52707c43f9eacf2`. Full log: `/tmp/triomics-m7-016-luna/repair2-source-probe.log`.
- Installed subprocess loaded the candidate from `/private/tmp/triomics-m7-016-luna/repair2-installed-lib/triomics` and passed in `/private/tmp/repair2-installed-root-936b3c6eb9e3`; it printed `probe_bootstrap=installed` and all expected recovery/path/failure markers. Final pointer SHA-256: `01441c0998f87bedc013b4cd0a7a172edec1bcc3d0e04044173ad9f08fe9816a`. Full log: `/tmp/triomics-m7-016-luna/repair2-installed-probe.log`.
- Both modes verified `D_PUBLICATION_STATUS_UNKNOWN` after ambiguous pointer readback, a complete current pointer after that outcome, `D_OUTPUT_PATH_UNSAFE` for planned-generation and temporary generation/pointer ancestry hazards, and `D_PUBLICATION_FAILED` plus byte-and-generation preservation for failed pointer rename.
- Final lightweight checks: R parse passed (`/tmp/triomics-m7-016-luna/repair2-parse-final.log`), `git diff --check` passed, only `tests/testthat/test-D-T01.R` is dirty. File SHA-256: `58fb3b4033bfdd19aee456dfb59dd08c36c23c9c42e08800048cba62bbc21cfe`. No full suite, B/C fits, or commit. The native paired/trajectory C-boundary evidence gap from repair 1 remains open and is not affected by this test-only repair.
