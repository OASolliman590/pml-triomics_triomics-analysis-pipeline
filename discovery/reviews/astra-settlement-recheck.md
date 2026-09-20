# Astra settlement reconciliation recheck

Date: 2026-09-06. **Design semantics pass; two acceptance/dependency corrections remain before declaring reconciliation complete.** This is a bounded document review, not scientific validation, package execution or implementation approval.

Reviewed the adopted settlement, ADR 0001, implementation command contract, all ticket-index entries and README, and A-T01, A-T07, B-T03, C-03 and D-T09 cards. Existing 41 identities and sequential milestones are retained. No production function, wrapper, fixture or test is represented as already implemented.

## Passing reconciliation

The internal service signatures establish usable module seams without changing the six BlockProfile signatures. `params$mode`, `params$fitted_state`, provisional `metadata(se)$triomics$state_payload` and finalized `state_ref` form a coherent fit/apply transport contract. B-owned association fitting and orchestrator-scheduled artifact requests avoid a B-to-C/D callback cycle.

Per-Analysis shared matrices, delta unit semantics, guarded longer repeated models, separate per-scalar-contrast/claim per-Block families and failed-row internal p=1/published unavailable p/q match the arbitration. The settlement preserves validity assumptions and makes no candidate-union FDR claim. Explicit DIABLO consensus configuration, training-only transformation, mandatory valid common scores, verified proportion semantics and unavailable common-result handling are consistent. Dense selection remains an executed explicit rule.

The canonical CLI has one grammar with `--validate-only`; `report` retains milestone 8 and QMD vignettes milestone 9. A-T01 owns the native build/check wrapper; its logs, warning dispositions and future-deliverable status are explicit. Early validation and package checks do not require future Quarto/vignette implementation. Sampled cards name test suites, fixtures, CLI calls and the wrapper, and distinguish synthetic acceptance from real-study validity.

## Corrections required

1. **Separate core resource-status/report prerequisites from optional adapter gates.** `index.json` and README make D-T05 depend unconditionally on D-T04, which depends on both D-T02 and D-T03. D-T08 then depends on D-T05. Thus the machine-readable graph can block unavailable/optional-resource handling and reporting on successful licensed mapping/enrichment, despite the accepted conditional design. Make D-T01 the core prerequisite for D-T05’s generic status/completeness contract; gate actual enabled-adapter tests on their respective tickets. D-T04 should require the mapping route(s) its specified method consumes, not automatically both unrelated routes. Represent conditional gates or reviewed scope dispositions consistently in index, cards and README. A disposition must remain explicit and cannot mark the unexecuted adapter complete or silently cancel requested scope. The graph is acyclic; this is an unintended resource gate, not a graph cycle.

2. **Make D-T09’s end-to-end commands explicit.** Its listed pipeline invocation runs only `--stage report`; two `quarto render` commands do not themselves specify fresh A-to-D execution for both required designs. Add owned, separate paired and independent-two-group synthetic YAML fixtures and exact `--stage all` invocations with distinct output roots before the rendering commands, or one explicitly owned runner whose acceptance contract executes and records both full workflows. Require clean-run and restart evidence. Keep the current report smoke if useful. The existing test requirement describes the intended behavior, but the command list does not yet make both full runs concrete.

## Current environment and release status

The later SERVER_NOTES-guided audit supersedes the initial PATH-only finding: `omics-r` supplies R 4.5.3/Bioconductor 3.22 and several verified namespace imports. DEqMS, lipidr, MultiAssayExperiment, renv, testthat, optparse, Quarto and mofapy2 remain missing. This is a documented schedulerless workstation; localomq help inspection is not queue execution or training. Do not repeat “no R” or “host role unknown” as current findings.

User R confirmation, a coherent approved release lock/provisioning route, methodological/API gates and original G0 remain open. Resolving the two document findings establishes reconciliation only; it does not pass those gates.

## Final disposition after corrections

**PASS — bounded document reconciliation complete, 2026-09-06.** Rechecked D-T05/D-T08 gate records, relevant cards and README: core status/report behavior no longer depends on successful optional licensed enrichment. D-T04 now has D-T01 as its core predecessor, with explicit method-specific D-T02/D-T03 mapping gates consistently recorded in index, card and README. Enabled-adapter requirements, unavailable reporting and whole-project scope disposition remain distinct.

D-T09 now owns separate paired and independent-two-group synthetic YAML fixtures and exact `--stage all` commands with distinct clean output roots before the two Quarto renders; report-only smoke is explicitly insufficient. Both findings above are closed. The earlier findings are retained as review history. This pass confirms specification/ticket consistency only: no package code, acceptance command, scientific gate or workflow was executed, and language confirmation, runtime/dependency readiness and G0 remain outstanding.
