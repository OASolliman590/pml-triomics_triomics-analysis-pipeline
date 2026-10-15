# M4-005B Stage B Dispatch Composition Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore the final B-T06 candidate and establish one idempotent Stage B startup composition that preserves accepted B-T04 and integrated B-T05 behavior in source and installed execution.

**Architecture:** A Luna-max worker supplies behavior-first cross-packet tests only. Sol restores the B-T06 candidate and owns the central registration implementation in `R/stage_registry.R` plus the registration-only reconciliation in `R/multiplicity.R`. Statistical functions, B-T04 calibration and the accepted B-T06 rank fix stay frozen.

**Tech Stack:** R 4.6, testthat 3, limma, DEqMS 1.30.0, source CLI and installed R package hooks.

**Spec:** `docs/implementation/change-records/M4-005-stage-b-shared-integration.md`

## Global Constraints

- Unit A must be integrated and verified before this plan starts.
- Restore all six B-T06 files from `086d604de8ba481e437d3c9109a1780839104810` before editing.
- Luna may create only `tests/testthat/test-M4-005-stage-b-composition.R`.
- Sol alone may edit `R/stage_registry.R` and registration-only portions of `R/multiplicity.R`.
- Do not modify B-T04 source/tests, `R/rank_sensitivity.R`, family/nomination algorithms, statistical thresholds, estimands or calibration controls.
- Private study inputs are excluded. Tests use repository synthetic fixtures only.
- A scientific/factual blocker receives a focused primary-literature review and fresh Astra-low adjudication. Reproducible engineering failures go through systematic debugging and the single authorized repair loop.

## Review Focus

- Function attributes can falsely claim a dispatcher is present; Task 2 asserts actual B-T04/B-T05/B-T06 returned behavior.
- Source-order wrappers can capture B-T01 and bypass B-T04; Task 2 executes source CLI trajectories.
- Installed `.onLoad` order can differ from source order; Task 3 and Task 4 exercise installed startup.
- Repeated installation can double-wrap or change lane choice; Task 2 invokes each composer twice and compares behavior.
- B-T06 context propagation can be lost while fit dispatch survives; Task 2 checks fixed-family selection state and rank output, not only fit status.

---

### Task 1: Restore the reviewed B-T06 candidate

**Files:**
- Restore: `R/multiplicity.R`
- Restore: `R/nominations.R`
- Restore: `R/rank_sensitivity.R`
- Restore: `tests/testthat/test-B-T06.R`
- Restore: `inst/examples/tickets/B-T06.yaml`
- Restore: `docs/implementation/registrations/B-T06.json`

**Interfaces:**
- Consumes: accepted Unit A integration commit and final B-T06 candidate tree.
- Produces: a preparatory commit with all six files byte-identical to `086d604`.

- [ ] **Step 1: Create a fresh isolated worktree**

Use the Unit A integration `HEAD`, a new `codex/m4-005b-stage-b-composition` branch, and the repository `.worktrees/` directory.

- [ ] **Step 2: Restore the six candidate files**

```bash
git restore --source=086d604de8ba481e437d3c9109a1780839104810 -- R/multiplicity.R R/nominations.R R/rank_sensitivity.R tests/testthat/test-B-T06.R inst/examples/tickets/B-T06.yaml docs/implementation/registrations/B-T06.json
```

- [ ] **Step 3: Verify exact restoration and commit**

Run `git diff --exit-code 086d604de8ba481e437d3c9109a1780839104810 --` against exactly those six paths. Expected: exit 0. Commit as `chore: restore final B-T06 candidate for M4-005B`.

### Task 2: Write RED cross-packet composition tests

**Files:**
- Create: `tests/testthat/test-M4-005-stage-b-composition.R`

**Interfaces:**
- Consumes: public Stage B dispatcher installers and synthetic B-T04/B-T05/B-T06 fixtures.
- Produces: behavioral tests for source and namespace composition; no product-code changes.

- [ ] **Step 1: Add the behavior matrix**

Create tests named:

- `M4-005B installed composition preserves B-T04 trajectory fitting`
- `M4-005B source composition preserves B-T04 stage execution`
- `M4-005B composition preserves B-T05 unavailable and fallback routing`
- `M4-005B composition preserves B-T06 family and rank context`
- `M4-005B installers are idempotent`

Assert returned lane-specific results, payload method/status fields, B-T04 acceptance/calibration fields, B-T06 family counts and scale-invariant rank output. Dispatcher attributes may be secondary assertions only.

- [ ] **Step 2: Run RED tests**

```bash
Rscript --vanilla -e "testthat::test_local(filter='M4-005-stage-b-composition', stop_on_failure=TRUE)"
```

Expected: the restored B-T06 startup chain reproduces the B-T04 bypass or missing centralized composer. No unrelated failure counts as RED evidence.

- [ ] **Step 3: Commit tests only**

```bash
git add tests/testthat/test-M4-005-stage-b-composition.R
git commit -m "test: pin Stage B cross-packet dispatch composition"
```

### Task 3: Implement Sol-owned centralized registration

**Files:**
- Modify: `R/stage_registry.R`
- Modify narrowly: `R/multiplicity.R`

**Interfaces:**
- Consumes: existing `triomics_b02_*`, `triomics_b03_*`, `triomics_b04_*`, `triomics_b05_*`, `triomics_b06_*` installers.
- Produces: `triomics_install_stage_b_namespace_dispatch(pkgname) -> invisible logical`; `triomics_install_stage_b_global_dispatch() -> invisible logical`; one canonical `.onLoad` composer that later idempotent wrappers may call; a source wrapper that calls the current pre-B-T06 CLI chain rather than `triomics_main_b_t01`.

- [ ] **Step 1: Add `triomics_install_stage_b_namespace_dispatch(pkgname)`**

Invoke available installers in the fixed order B-T02, B-T03, B-T04 fit, B-T04 stage hook, B-T05, B-T06 fit, then B-T06 source/context setup. Guard optional functions with exact existence checks and rely on their idempotence markers.

- [ ] **Step 2: Add `triomics_install_stage_b_global_dispatch()`**

Install the same ordered fit chain and B-T04 stage hook in `.GlobalEnv`, followed by B-T06 context setup. Return invisibly without changing statistical results.

- [ ] **Step 3: Make `stage_registry.R` the canonical package-load composer**

Define `.onLoad(libname, pkgname)` to call the namespace composer. Preserve B-T05's later wrapper as an idempotent call path; do not create a second statistical dispatcher.

- [ ] **Step 4: Reconcile only B-T06 registration in `R/multiplicity.R`**

Make its `.onLoad` delegate to the central composer when available. In source mode, capture the current `triomics_main` chain as `triomics_main_before_b06`, invoke the global composer at call time, and then call that captured chain. Remove the dependency on `triomics_main_b_t01`. Do not alter family, nomination, selection or rank code.

- [ ] **Step 5: Run GREEN composition and adjacent tests**

```bash
Rscript --vanilla -e "testthat::test_local(filter='M4-005-stage-b-composition|B-T04|B-T05|B-T06', stop_on_failure=TRUE)"
```

Expected: zero failures, warnings or required skips; actual B-T04, B-T05 and B-T06 behavior remains available after two installer calls.

- [ ] **Step 6: Run source CLI probes**

Run the B-T04, B-T05 and B-T06 ticket fixtures into fresh M4-005B output directories. Expected: exit 0 for supported fixtures, valid envelopes, B-T04 trajectory acceptance unchanged, B-T05 provenance preserved, and B-T06 family/rank evidence present.

- [ ] **Step 7: Commit the Sol-owned registration change**

```bash
git add R/stage_registry.R R/multiplicity.R
git commit -m "fix: centralize Stage B dispatch composition"
```

### Task 4: Review, integrated verification and pre-B-T07 gate

**Files:**
- Review: complete Unit A integration-to-Unit B candidate diff.
- Evidence: local `artifacts/acceptance/M4-005B-*` and fresh package-check output only.

**Interfaces:**
- Consumes: restored B-T06 candidate, RED tests and Sol registration commit.
- Produces: accepted Unit B integration or one bounded repair; then the immutable M4-005 Astra integration snapshot.

- [ ] **Step 1: Dispatch one fresh Sol 6.1 high review**

The reviewer inspects the entire candidate, reruns composition/B-T04/B-T05/B-T06 tests, verifies source and installed behavior, and confirms statistical code is unchanged. At most one focused repair and one focused rereview are available.

- [ ] **Step 2: Run full integrated verification after review acceptance**

Run the complete local test suite, supported source CLI probes, installed CLI probes and `Rscript --vanilla tools/check-package.R` to a fresh output directory. Record discovered/pass/fail/warn/skip counts and build/check status.

- [ ] **Step 3: Integrate serially and create immutable M4-005 snapshot**

Cherry-pick the accepted commits, update ledger/handover, verify candidate ancestry and clean tracked state, then remove the Unit B worktree while preserving its branch.

- [ ] **Step 4: Dispatch the bounded Astra-low M4-005 integration review**

Review result-boundary semantics, actual dispatch behavior, preserved B-T04 controls, source/installed parity, privacy and B-T07 dependency legality. One initial review and at most one focused rereview are allowed. B-T07 starts automatically only after Sol accepts this review and all five frozen dependencies are accepted.

