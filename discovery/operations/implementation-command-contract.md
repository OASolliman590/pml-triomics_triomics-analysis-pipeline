# Implementation ticket command contract

Date: 2026-09-06. Status: **concrete acceptance specification; implementation and execution pending**. This defines commands that tickets must make work. It does not claim the R package, CLI, fixtures or test files already exist. R core plus Python is now [approved](language-and-execution-approval.md); a verified relevant runtime and original production G0 remain prerequisites. The [Astra arbitration](../reviews/astra-design-arbitration.md) and [adopted settlement](contract-settlement.md) govern this contract.

## One CLI and artifact root

The package owns `inst/exec/triomics`. Use `Rscript --vanilla inst/exec/triomics --config <yaml> --stage <A|B|C|D|report|all> --output <directory> [--validate-only]`. Config validation is stage-aware: a milestone-1 A validation must not demand an implemented C backend or D renderer. `--validate-only` checks configuration/source references and planned eligibility without running analysis; omitting `--validate-only` performs only the requested implemented stage and verifies immutable predecessor artifacts. An unavailable stage must fail explicitly. `all` is enabled only after the approved stage set is implemented and validated; report generation remains milestone 8.

CLI output overrides the YAML execution output once and is recorded in resolved configuration. All output artifacts remain below that resolved root. Relative source/config paths resolve against the config file's directory. Each successful invocation writes `resolved-config.yaml` and `run-manifest.json`; later stage manifests reference predecessor hashes. No change of filename or output folder erases data reuse/selection history. Numeric results, masks, dictionaries and model artifacts follow the common contract rather than being inferred from filenames.

Exit status: 0 = requested operation complete under its declared required/optional policy; 2 = invalid configuration/input/unsupported requested operation; 3 = unmet dependency, execution or resource requirement; 4 = runtime/model failure or required requested work incomplete. A valid zero-candidate result is not an error. Optional annotation unavailability may coexist with exit 0 only if the configuration declared it optional before execution and the manifest reports it. Rendering and analysis statuses remain separate.

## Exact acceptance command pattern

Every local ticket lists concrete test files. For each listed file, the implementer must create tests that exercise the ticket's public behavior, with no default skip for missing core dependencies. Run its named suite after implementing the slice, then the package check and the ticket's synthetic CLI smoke. For example A-T01 must create `tests/testthat/test-config.R` and the tiny `inst/examples/tickets/A-T01.yaml` fixture, and must make these commands work from the package root:

```text
Rscript --vanilla -e "testthat::test_local(filter='^config$', stop_on_failure=TRUE)"
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/A-T01.yaml --stage A --output artifacts/acceptance/A-T01 --validate-only
Rscript --vanilla tools/check-package.R
```

A-T01 owns tools/check-package.R, a dependency-free wrapper around native R CMD build and R CMD check --no-manual. It derives the generated tarball name from DESCRIPTION, retains command/check logs, and returns nonzero on errors or undisposed warnings. Warning dispositions must be explicit and independently reviewed; NOTE entries require review. It must not demand Quarto or future vignettes before their original milestones. No extra rcmdcheck dependency is introduced. The wrapper is a future deliverable, not a file claimed to exist now.

For each other ticket, the local card supplies its exact test filter, unique synthetic YAML fixture, stage and validation flag. The fixture is a deliverable owned by that ticket, not a real-study configuration. Tests must assert expected values or reference-method agreement, file schemas and completeness, failure exit codes, provenance and restart behavior. Do not prewrite expected output checksums before output exists: capture checksums after execution and use invariants/tolerances for numerical comparisons.

At A-T01, the CLI can only validate; subsequent slices expose bounded runnable behavior through the same stage command. No ticket claims future-stage availability. D-T09 alone owns the final paired and independent two-group QMD walkthrough execution at milestone 9; earlier components produce Markdown reproduction instructions and synthetic machine-readable outputs.

## Per-ticket execution record

Record ticket ID, accepted spec/decision hash, exact code commit (when a verified repository exists), environment/lock hashes, actual command, start/end, exit status, counts of passed/failed/skipped tests, output artifacts/checksums, independent review and remaining blockers. A zero-test suite is a failure. A skipped conditional adapter remains unvalidated; it cannot make its own ticket complete. Cross-request hypothesis and evaluation ledgers are retained.

The complete ticket list is in `../tickets/README.md`. Tickets are blocked until their specific prerequisites are satisfied; they are executable specifications, not evidence of execution.
