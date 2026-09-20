# A-T01 implementation handoff

## Current delegation lanes

Use the [current user-approved fleet](../operations/active-delegate-fleet.json): Sol High manages; Grok 4.6 medium implements features; agy/Gemini handles bugfixes and docs; planning, debate, Spark and read-only reviews use their configured lanes. This supersedes older mandatory Gemini/Sonnet/Opus sequencing below. [Standing authorization](../operations/task-authorization.md) and this ticket's prerequisites still apply. Model allocation does not bypass a provider permission denial or expand an approved payload.


## Standing authorization

User-approved workflow: use the current configured lanes above, with Sol High management and minimal Astra acceptance. Read [task authorization](../operations/task-authorization.md) before dispatch. Do not re-request settled workflow approval. The approved A-T01 provider file list and Root/Sol AIU SSH/setup permissions are scoped there; they do not automatically authorize this ticket's unlisted files for external upload. Preserve ticket prerequisites and runtime approval enforcement.

Status: prepared; production implementation still subject to its recorded prerequisites. The user approved R core plus Python and the current Sol High → Gemini 3.8 Flash through Antigravity → Claude Sonnet → Sol/Opus → Astra workflow. Earlier Spark/ZCode routing is historical. Read [A-T01](A-T01.md), the [command contract](../operations/implementation-command-contract.md), [settlement](../operations/contract-settlement.md), and [Spark handoff disposition](../reviews/spark-a-t01-handoff-response.md). Those govern older draft alternatives.

## Deliverable

Build the smallest complete R package slice that validates a synthetic Study configuration through the CLI, reports exact key/file/reason failures, and writes resolved configuration plus a JSON run manifest for successful validation. This is configuration validation only. No molecular preprocessing, model fitting, inference, integration, association, annotation or rendering is implemented here.

## Ownership

Create only DESCRIPTION, NAMESPACE, .Rbuildignore, R/config.R, R/cli.R, R/schema.R, inst/exec/triomics, inst/schema/config.yaml, inst/examples/tickets/A-T01.yaml and its tiny synthetic referenced source files, tests/testthat.R, tests/testthat/test-config.R, and tools/check-package.R. Package API documentation belongs in man/ only when required for this slice's exported interface. Approved environment/lock deliverables follow the provisioning manifest. Use only owner-confirmed package identity/license metadata; do not invent maintainer identity or release licensing. Ask the orchestrator to resolve missing required package metadata before a package check can pass.

One shared schema module owns the minimal config and artifact envelope; subsequent A tickets extend its registries. Protected BlockProfile, simulator and IntegrationResult contracts are unchanged. No source study data, external resources, global settings, credentials or unrelated project files belong in this task. Do not initialize Git, commit, push, publish or install dependencies.

## Required behavior

CLI: Rscript --vanilla inst/exec/triomics --config <yaml> --stage <A|B|C|D|report|all> --output <directory> [--validate-only]. At this slice, only A with --validate-only is runnable. Other recognized stages and A processing fail explicitly; --help works without config and does not create run artifacts.

Reject unknown/duplicate keys, unknown schema versions, incompatible declared quantity/scale/transform combinations, malformed lists and missing references with structured key/file/reason diagnostics. Preserve declared user metadata and design shapes without silently choosing scientific defaults. Future-stage eligibility may be declared unavailable without requiring those packages for A validation.

Resolve relative sources against the YAML directory; ../ is allowed when the resolved source meets the source policy. Keep generated artifacts below the canonical CLI output root. Successful validation writes resolved-config.yaml and run-manifest.json with schema/status, resolved configuration and applicable source hashes. Do not echo source values, participant identifiers or secrets. Validation creates no processing checkpoint. Existing output reuse follows the specified provenance/restart policy; do not silently overwrite a different run.

Exits: 0 complete validation/help; 2 invalid input or unsupported request; 3 unmet runtime/dependency/resource requirement; 4 runtime failure or required work incomplete. Handle errors at the CLI boundary without converting failures to successful empty outputs.

tools/check-package.R uses native R CMD build/check --no-manual without an rcmdcheck dependency, derives the tarball from DESCRIPTION, retains logs and exit results, and fails on ERROR or undisposed WARNING. Record and review NOTEs. Do not require future Quarto vignettes. Exclude discovery documents, generated artifacts, private local records and check output from the source package.

## Acceptance commands

These commands are copied from the accepted command contract and remain future deliverables:

```text
Rscript --vanilla -e "testthat::test_local(filter='^config$', stop_on_failure=TRUE)"
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/A-T01.yaml --stage A --output artifacts/acceptance/A-T01 --validate-only
Rscript --vanilla tools/check-package.R
```

Test public behavior: valid YAML, misspelled/duplicate keys, invalid schema and quantity/scale combinations, missing/unreadable references, valid ../ references, malformed inputs, no silent defaults, deterministic manifests, help, unsupported stages, explicit exit codes, no validation checkpoint, and no successful output after failure. Fixtures are labelled synthetic. No zero-test success or skipped required assertions. Gemini runs the applicable local tests during implementation; Sonnet independently reviews the actual code and test evidence; Sol verifies outcomes and debates completeness with Opus before minimal Astra acceptance/integration. Record actual commands, exits, test counts, environment and output checksums; deferred checks stay NOT RUN.

## Dispatch versus acceptance

Before dispatch: verified relevant A-T01 runtime/dependency disposition, the original requirement to commit and push ready specifications to the verified private repository, and an available approved implementer. ZCode and Spark availability are not prerequisites for the current approved delegation sequence. Later MOFA and Quarto installations are not A-T01 runtime requirements. The exact Notion destination is required before writing to Notion; unavailable Notion synchronization stays queued and is not an additional prohibition on coding.

After implementation: execute the commands and negative CLI cases, independent review, root verification/integration, and authorized GitHub/Notion synchronization. A-T01's own test outputs are acceptance gates, not prerequisites to writing its code. No component is complete through scaffolding alone.

The implementer must report changed files, actual gate outcomes, decisions/deviations and uncompleted work. No commit or destination choice by the implementer.
