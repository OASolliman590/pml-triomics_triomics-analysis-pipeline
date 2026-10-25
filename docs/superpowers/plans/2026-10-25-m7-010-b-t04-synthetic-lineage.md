# M7-010 B-T04 Synthetic Lineage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the accepted B-T04 synthetic trajectory producer emit a complete,
truthful native B-T07 provenance handoff without treating its unused paired TSVs
as response ancestry.

**Architecture:** The custom B-T04 executor materializes one versioned canonical
synthetic protocol, generates all analysis-domain objects from that protocol,
and returns the same private in-memory provenance context shape consumed by
B-T07. The exporter retains the exact raw matrix as the downstream
`represented_matrix` while adding ordered design/subject/time identity and a
separate digest for the centered response actually fitted by limma.

**Tech Stack:** R, testthat, jsonlite, existing triomics artifact envelopes,
RFC-8785 provenance identity and B-T07 exporter/validator.

**Spec:** `docs/implementation/change-records/M7-010-b-t04-synthetic-producer-lineage.md`

## Global Constraints

- Edit only `R/design_repeated.R`, `R/export_b.R`,
  `tests/testthat/test-B-T04.R`,
  `tests/testthat/test-M4-005-stage-b-composition.R`, and
  `docs/implementation/registrations/B-T07.json`.
- Do not edit `discovery/spec-v2`, the frozen hash, provenance schema,
  `R/provenance_contract.R`, package registration, C/D code or B-T07 tests.
- Preserve effects `1.5` and `-0.25`, ordinary df `7`, design rank `5`, the
  exact existing two-by-twelve exported matrix and all B-T04 calibration
  results.
- Do not loosen the `0.08` bound, reroll seeds `1001:1400`, suppress warnings,
  access private study data, implement migration, accept a milestone, open
  Stage D or push.
- The configured paired-example TSV payloads, digests and source IDs are never
  response ancestors or block-results envelope inputs.
- The public block result and bundle never serialize `provenance_context`, an
  assay, fit or native model object.

## Review Focus

- A changed or missing retained protocol must fail before a complete/reusable
  handoff is published; Task 1 adds protocol mutation and omission probes.
- Raw and centered matrix identities must not be conflated; Task 1 independently
  recomputes both hashes and asserts their distinct roles.
- Exact row/column order, subjects and times must survive canonicalization;
  Task 1 asserts the ordered 12-by-5 design identity.
- Unused paired-example paths may remain in the ticket but their bytes/digests
  must not enter ancestry; Task 1 checks envelope inputs, source descriptors and
  derivation edges.
- Existing non-B-T04 B-T07 profiles must retain their current shape and pass;
  Task 1 runs the exact B-T07 suite without editing it.

---

### Task 1: Native B-T04 synthetic producer context

**Files:**

- Modify: `R/design_repeated.R`
- Modify: `R/export_b.R`
- Test: `tests/testthat/test-B-T04.R`
- Test: `tests/testthat/test-M4-005-stage-b-composition.R`
- Modify: `docs/implementation/registrations/B-T07.json`

**Interfaces:**

- Consumes: `triomics_b04_execute_fixture(config_path, output)`,
  `triomics_b_provenance_unit_map(ingested, analysis, resolved)`,
  `triomics_b07_validate_provenance_context(context, config_sha256, artifact,
  matrices)`, and `triomics_b07_build_provenance_profile(...)`.
- Produces: the existing Stage-B return list plus private
  `provenance_context`; one immutable canonical B-T04 protocol artifact; and a
  native profile whose `source_manifest`, resolved-analysis and method-state
  descriptors bind the approved synthetic lineage.

- [ ] **Step 1: Add RED assertions for the producer return and public CLI**

  Add named tests proving the base lacks B-T04 `provenance_context` and the CLI
  terminates with `B_EXPORT_INCOMPLETE`. Extend the desired behavior assertions
  to require a valid profile/envelope, unchanged numerical output, and no
  serialized private context.

- [ ] **Step 2: Run the exact RED slices**

  Run:

  ```bash
  Rscript --vanilla -e 'testthat::test_local(filter = "B-T04", reporter = "summary")'
  Rscript --vanilla -e 'testthat::test_local(filter = "M4-005-stage-b-composition", reporter = "summary")'
  ```

  Expected: failure caused by missing B-T04 same-run provenance, not a numerical
  or calibration difference. Record exact FAIL/WARN/SKIP/PASS counts.

- [ ] **Step 3: Materialize the canonical synthetic protocol and generated domain state**

  In `R/design_repeated.R`, add a B-T04 protocol builder/writer with a stable
  protocol ID and the exact ordered subjects, times, features, intercepts,
  slopes, residuals, units, deterministic/no-RNG statement, effective config
  digest and explicit non-ancestral source declaration. Build the current
  matrix from those retained protocol values. Construct generated metadata,
  SampleRegistry, complete FeatureRegistry and AnalysisUnitMap from the same
  values.

- [ ] **Step 4: Return an exact private same-run context**

  Extend `triomics_b04_execute_fixture()` to bind only the protocol artifact in
  the block-results envelope inputs and return the generic context fields plus
  B-T04-specific raw-response, centered-response, ordered design,
  subject/time, method and calibration identities. Preserve the current public
  block-results shape and exact matrix.

- [ ] **Step 5: Extend the allowed exporter path without changing the schema**

  In `R/export_b.R`, make source-manifest construction retain the exact
  protocol bytes/canonical identity, extend resolved-analysis identity with
  ordered design/subject/time fields when present, and include the approved
  centering/fitted-response evidence in preprocessing/method state. Validate
  every required synthetic-specific field and mismatch before writing a
  complete profile. Keep ordinary producers backward-compatible.

- [ ] **Step 6: Add ancestry, identity and fail-closed tests**

  Assert the profile validates; the protocol is the analysis source root; the
  paired TSV bytes/digests are absent from envelope inputs, source entities and
  derivation edges; raw and centered matrix hashes independently match; the
  12-by-5 design, ordered subjects/times, registry and unit map are complete;
  and changing or omitting protocol, metadata/registry/unit/design,
  centering/method/calibration, matrix or predecessor evidence refuses a
  complete handoff. Assert exact B-T04 effects, df, rank, matrix and calibration
  are unchanged.

- [ ] **Step 7: Run focused GREEN and non-regression gates**

  Run:

  ```bash
  Rscript --vanilla -e 'testthat::test_local(filter = "B-T04", reporter = "summary")'
  Rscript --vanilla -e 'testthat::test_local(filter = "M4-005-stage-b-composition", reporter = "summary")'
  Rscript --vanilla -e 'testthat::test_local(filter = "B-T07", reporter = "summary")'
  ```

  Expected: each command finishes with zero failures, warnings and skips. Record
  exact expectation counts in the B-T07 registration without marking it
  accepted.

- [ ] **Step 8: Self-review and commit the bounded candidate**

  Confirm the diff contains only the five allowlisted files, no paired TSV
  digest appears in provenance/envelope inputs, no numerical/calibration value
  changed, and no private context is serialized. Commit with a concise message
  and report the immutable commit, RED/GREEN counts and concerns.

## Coordinator gates after Task 1

The coordinator independently checks the allowlist and exact diff, dispatches
fresh Standards and Spec read-only reviewers, and permits at most two focused
repair rounds. Only after both axes pass may the coordinator integrate serially,
rerun the three focused suites, and run one uninterrupted full source suite
with warnings unsuppressed. B-T07/M7 acceptance, G7 and Stage D remain closed in
this cycle.
