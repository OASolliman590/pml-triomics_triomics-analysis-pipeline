# M7-019 Luna handoff report

## Baseline red evidence

I did not rerun the old product tests before editing. The independent baseline is the M7-018 close receipt at `docs/implementation/reviews/2026-10-28-m7-018-close-receipt.md`: source suite 6,280 pass / 1 fail / 15 warn; clean archive testthat 5,266 pass / 45 fail / 42 warn. The recorded source failure was `C-08 preserves mean and delta AnalysisUnitMap lineage and rejects incompatible times`: delta visit weights were a named list while the established API assertion requires a named numeric vector. The archive failures also named missing native producer revision, installed fixture paths, and this signed-weight API mismatch. No acceptance claim is inferred from those old counts.

## Bounded warning reporter probe

A standalone reporter probe ran one test that emitted `warning inventory message` and one failing assertion. The unmodified `CheckReporter` retained its normal counts (FAIL 1 / WARN 1 / SKIP 1 / PASS 0); a `finally` callback wrote the warning record with test name and message even though the run failed. The temporary probe artifact was removed after read-back verification. The repository reporter now uses this same instance-plus-finally pattern.

## Edits frozen for parent candidate commit

- Numeric C-08 delta weights remain named numeric vectors in the endpoint API. The separate parent-owned CLI patch converts endpoint sample and embedded unit-map weights to named JSON objects before hashing, recomputes endpoint/unit-map/bundle hashes, and validates both before and after conversion.
- Added source identity from the executing package namespace or verified source CLI entrypoint. Installed packages require a strict expanded stamp and never consult Git or the working directory. Source Git fallback requires the exact executing source root and a clean worktree. No environment claim is read. The source comment records that archive metadata is unsigned and not proof against repackaging.
- Added `.gitattributes` export substitution and `inst/triomics-source-revision`; recorded helper/stamp paths before edits in `/tmp/triomics-m7-019/luna-paths.md`.
- Updated source/installed test-resource selection and B-T01 test CLI/config paths. Packaged independent fixture files were verified byte-for-byte against discovery fixtures (metadata SHA-256 `de9a7a6f45f296b07c9733de174521ea06e3b2cc7a74cb63547824f2a622fa2c`; proteomics SHA-256 `e2744731cf39baaa9767cb66f8e419cde8cc1090e5105a39afbb1a691944a843`).
- Added source-identity, missing/unexpanded/malformed-stamp, unrelated-working-directory, weight API/JSON round-trip, tamper rejection, and installed warning-inventory proofs. Parent-applied shared changes cover `R/cli.R`, `tools/check-package.R`, and `man/triomics-api.Rd`.

## Verification still pending

At the initial handoff, no focused C-08 or source-identity suite, native producer fit, package build, or package gate had run from this dirty worker checkout. Repair 1 subsequently ran only the filtered C-08 test and pure identity/path probes recorded below. Parent requested a clean candidate commit before tests that need a truthful source revision. No native fitting job was started.

## Repair 1 — focused gate 0668d07

The parent reproduction (`/tmp/triomics-m7-019/focused-initial.json` and `.log`) reported 1,999 pass / 10 fail / 1 error / 2 warn / 0 skip. The identity failures came from comparing a *named* value returned by `getNamespaceName()` to an unnamed scalar; the resolver now removes that incidental name while retaining the executing namespace branch. Persisted C-08 JSON parses integer tokens as integers, so only the two new persisted-object expectations now require the exact named integer list; the API still requires named numeric weights. The B-T01 refusal/budget configs and their unchanged relative data files are copied under a temporary `tickets/` plus `data/independent/` layout before mutation, leaving packaged fixtures read-only.

At the native B provenance seam, a non-null `provenance_context` now resolves the executing package revision, requires a full commit id, rejects a supplied `working_ref` that is absent-invalid or differs, and uses the resolved identity for the provenance profile. Legacy `working_ref` metadata remains available for non-native API fixtures. The B-T07 mismatch test reuses the single AC2 Stage B result and passes a verified historical commit SHA as a stale claim; it asserts rejection before a handoff is written.

Pure verification completed: namespace context resolves to this package source root after the `unname()` fix; dirty-source identity remains `unavailable`; installed missing/unexpanded/malformed stamp probes all fail closed; the filtered C-08 mean/delta test passed including signed JSON round-trip and tamper rejection; the temporary B-T01 fixture tree resolves its unchanged relative refs; R parsing passed for all edited product/test files. I did not run B-T07 native export or any fitting gate; those await parent commit and the next focused acceptance run.

## Repair 2 — C-08 visit-name hash binding

The parent-provided baseline probe (`/tmp/triomics-m7-019/weight-name-probe.R`, log `weight-name-full-probe.log`) confirmed that renaming only numeric sample `visit_weights` in every component was accepted and serialized under the wrong names. The bounded fix is in `R/integration_endpoints.R`: `triomics_c08_endpoint_hash()` now hashes a copy where only named numeric weights in endpoint samples and embedded unit-map rows/columns become named lists. The public endpoint API remains numeric; no shared writer, schema, method, or CLI file changed.

The existing C-08 mean/delta behavior test now mutates all components' sample weights, unit-map weights, and both together. Each case recomputes the enclosing payload hash, then asserts that CLI conversion rejects the stale endpoint hashes with `C_SCORE_ENDPOINT_HASH`, proving the check reaches the endpoint hash rather than merely the bundle hash. Existing exact numeric API, named JSON object, persisted integer JSON, null/exclusion, and score guards remain in the test.

Verification on candidate base `859fa9d`: `pkgload::load_all()` plus filtered `testthat::test_file("tests/testthat/test-C-08.R", desc="C-08 preserves mean and delta AnalysisUnitMap lineage and rejects incompatible times")` passed (39 expectations); `git diff --check` passed. No fit, full suite, package gate, commit, or scope change was performed. This repair is frozen for parent review.
