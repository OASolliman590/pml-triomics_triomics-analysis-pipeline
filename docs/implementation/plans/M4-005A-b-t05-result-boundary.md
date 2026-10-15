# M4-005A B-T05 Result Boundary Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore the final B-T05 candidate and serialize unavailable or explicit-limma-fallback outcomes into valid, scientifically interpretable Stage B artifacts.

**Architecture:** Keep DEqMS and fallback calculation unchanged in `R/statistics_deqms.R`. Add a boundary adapter in `R/results_b.R` that separates generic artifact completion from method-specific outcome, persists method provenance, and supplies envelope reason codes. The source and installed CLI must write the same payload, envelope and run-registry statuses.

**Tech Stack:** R 4.6, testthat 3, jsonlite, limma, DEqMS 1.30.0, triomics artifact envelopes.

**Spec:** `docs/implementation/change-records/M4-005-stage-b-shared-integration.md`

## Global Constraints

- Integration base is the current accepted branch commit when the worktree is created.
- Restore the four B-T05 files from `b6d397072db09b67b765e9e6cf0e2454798f2d12` before editing.
- Do not modify `R/statistics_deqms.R`, `R/contracts.R`, package/CLI registration, canonical spec, thresholds, estimands or B-T04 controls.
- The only restored-test edit is the test block named `CLI startup routes qualified absolute-path DEqMS and explicit fallback`.
- Generic artifact statuses remain `not_requested`, `not_started`, `running`, `complete`, `partial`, `unavailable`, and `failed`.
- Private study inputs are excluded. Tests use repository synthetic fixtures only.
- If a blocker is scientific or factual, Sol frames one focused question, reviews primary literature without private data, and requests a fresh Astra-low evidence adjudication. Engineering defects use systematic debugging and the authorized repair loop; literature is not a substitute for reproducing code failures.

## Review Focus

- Missing-count fallback `none` must persist `unavailable` consistently in payload, envelope and run registry; Task 2 tests it.
- Complete and partial limma fallbacks must map to generic artifact status while preserving distinct method status; Task 2 tests both.
- Fallback output must not claim DEqMS statistics or count priors; Task 2 inspects serialized rows and provenance.
- Ordinary limma and ordinary DEqMS output must not acquire false fallback fields; Task 2 tests both paths.
- Source and installed execution can diverge through startup composition; Task 3 runs both and compares the persisted contract.

---

### Task 1: Restore the reviewed B-T05 candidate

**Files:**
- Restore: `R/statistics_deqms.R`
- Restore: `tests/testthat/test-B-T05.R`
- Restore: `inst/examples/tickets/B-T05.yaml`
- Restore: `docs/implementation/registrations/B-T05.json`

**Interfaces:**
- Consumes: final candidate tree at `b6d397072db09b67b765e9e6cf0e2454798f2d12`.
- Produces: a preparatory commit whose four restored files are byte-identical to that candidate.

- [ ] **Step 1: Create a fresh isolated worktree**

Use the current integration `HEAD`, a new `codex/m4-005a-b-t05-result-boundary` branch, and the repository `.worktrees/` directory.

- [ ] **Step 2: Restore the four candidate files**

Run:

```bash
git restore --source=b6d397072db09b67b765e9e6cf0e2454798f2d12 -- R/statistics_deqms.R tests/testthat/test-B-T05.R inst/examples/tickets/B-T05.yaml docs/implementation/registrations/B-T05.json
```

- [ ] **Step 3: Verify exact restoration**

Run:

```bash
git diff --exit-code b6d397072db09b67b765e9e6cf0e2454798f2d12 -- R/statistics_deqms.R tests/testthat/test-B-T05.R inst/examples/tickets/B-T05.yaml docs/implementation/registrations/B-T05.json
```

Expected: exit 0 and no output.

- [ ] **Step 4: Commit the restoration**

```bash
git add R/statistics_deqms.R tests/testthat/test-B-T05.R inst/examples/tickets/B-T05.yaml docs/implementation/registrations/B-T05.json
git commit -m "chore: restore final B-T05 candidate for M4-005A"
```

### Task 2: Implement the result-boundary adapter with TDD

**Files:**
- Modify: `R/results_b.R`
- Modify narrowly: `tests/testthat/test-B-T05.R`
- Create: `tests/testthat/test-M4-005-B-T05-integration.R`

**Interfaces:**
- Consumes: `triomics_fit_block_models(resolved_analysis, statistics_spec)` and its `status`, method and provenance fields.
- Produces: `triomics_b_artifact_status(method_status) -> character(1)`; `triomics_b_method_metadata(fit) -> named list`; `triomics_b_reason_codes(fit) -> character()`; generic persisted status plus `method_status` in the Stage B payload and return value.

- [ ] **Step 1: Correct the stale restored CLI regression**

In only the named B-T05 CLI test, assert fallback CLI exit 0; a valid envelope; payload/envelope/run status `complete`; `method_status == "fallback_complete"`; `requested_method == "DEqMS"`; `method == "limma_explicit_fallback"`; `fallback_applied == TRUE`; DEqMS unavailability reason; and explicit fallback provenance. Remove the expectation for `A_ARTIFACT_INVALID`.

- [ ] **Step 2: Add failing integration tests for the full status matrix**

Create tests named:

- `M4-005A serializes unavailable DEqMS without fallback`
- `M4-005A serializes complete explicit limma fallback`
- `M4-005A serializes partial explicit limma fallback`
- `M4-005A leaves ordinary limma and DEqMS provenance clean`

For each persisted run, assert payload, envelope and `registries/run.json` status. For fallback, assert generic status and method-specific status separately, named trigger reason, actual method, and absence of DEqMS statistic/count-prior claims in fallback rows.

- [ ] **Step 3: Run RED tests**

Run:

```bash
Rscript --vanilla -e "testthat::test_local(filter='M4-005-B-T05-integration|B-T05', stop_on_failure=TRUE)"
```

Expected: failures show `fallback_complete`/`fallback_partial` reaching generic envelope status and missing serialized provenance; no unrelated failure is accepted as RED evidence.

- [ ] **Step 4: Implement `triomics_b_artifact_status(method_status)`**

Map `fallback_complete` to `complete` and `fallback_partial` to `partial`; pass through only the frozen generic statuses. Abort an unknown status with a Stage B result-contract error rather than silently coercing it.

- [ ] **Step 5: Implement `triomics_b_method_metadata(fit)` and `triomics_b_reason_codes(fit)`**

Copy only present method fields named by M4-005. Derive reason codes from `unavailable_reason` and `deqms_status$reason`, remove empty/duplicate values, and retain a fallback trigger without turning a successful fallback artifact into `unavailable`.

- [ ] **Step 6: Apply the adapter in `triomics_execute_stage_b()`**

Use generic status for payload `status`, envelope `status`, run-registry `status`, and returned `status`. Persist original `fit$status` as `method_status`; append method metadata to the payload; pass reason codes to `triomics_artifact_envelope()`; do not change block rows or fitting behavior.

- [ ] **Step 7: Run GREEN packet and integration tests**

Run:

```bash
Rscript --vanilla -e "testthat::test_local(filter='M4-005-B-T05-integration|B-T05', stop_on_failure=TRUE)"
Rscript --vanilla -e "testthat::test_local(filter='B-T01|B-T03|B-T04', stop_on_failure=TRUE)"
```

Expected: zero failures, warnings or skips in required tests.

- [ ] **Step 8: Run the source CLI fixture**

Run:

```bash
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/B-T05.yaml --stage B --output artifacts/acceptance/M4-005A-source
```

Expected: exit 0 and valid payload/envelope/run registry with consistent generic status.

- [ ] **Step 9: Commit the bounded implementation**

```bash
git add R/results_b.R tests/testthat/test-B-T05.R tests/testthat/test-M4-005-B-T05-integration.R
git commit -m "fix: serialize B-T05 method outcomes at Stage B boundary"
```

### Task 3: Review, installed verification and serial integration

**Files:**
- Review: complete integration-base-to-candidate diff.
- Evidence: local `artifacts/acceptance/M4-005A-*` only; do not commit generated outputs.

**Interfaces:**
- Consumes: Task 2 candidate commit and test receipt.
- Produces: accepted Unit A integration commit or one bounded repair request.

- [ ] **Step 1: Dispatch one fresh Sol 6.1 high review**

The reviewer reruns B-T05 plus M4-005A tests, checks the complete restored diff, validates source/installed parity, and inspects absence of false DEqMS claims. A finding may trigger at most one focused repair by the same Luna worker and one focused rereview.

- [ ] **Step 2: Run installed-package verification after review acceptance**

Run the package build/check wrapper to a fresh output directory, execute the installed B-T05 CLI fixture, and compare payload/envelope/run status fields with the source result.

- [ ] **Step 3: Cherry-pick serially and update canonical records**

Only after review and installed verification pass, integrate Unit A, record exact commits/tests/exits in the ledger and handover, and remove the Unit A worktree while preserving its branch.

