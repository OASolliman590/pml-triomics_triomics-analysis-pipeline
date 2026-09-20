# A specification review response

Date: 2026-09-05. Author: A draft author, responding to the independent root review in `discovery/reviews/A-spec-review.md`. This is an author revision record, not an independent re-review or implementation approval. Only `discovery/spec-drafts/A-ingest-profiles.md` was revised for this response; this response file was added. No production code, test execution, dependency installation, R/API check, publication or contract adoption occurred.

## Artifact identity

Hash algorithm: SHA-256, computed from the actual file bytes using PowerShell `Get-FileHash` before and after editing.

| Target | Hash |
|---|---|
| A draft before revisions | `267F687EB4EEBE5A00BAAF6F97CEA40B7456514B66B2E55114C296B0550C6EAA` |
| A draft after revisions | `6E15C24F9BB88CA5A80BCE4F11BAB7FC61E303079C75668935E1ECC51699C59B` |

The cross-component review at `discovery/reviews/cross-component-review.md` records its own earlier snapshot. It is intentionally not rewritten to imply that these author revisions resolved the shared findings.

## A-SR01 — P1 shared Feature identity remains open

**Before:** §3.3 proposed an optional source-row discriminator without a shared A/B/C/D registry that guaranteed its survival.

**After:** §3.3 now explicitly requires coordinated approval of one registry separating unique measured/derived Feature identity from original molecular ID, with discriminator preservation through SE, B tables, C selections/loadings and D nominations. Keys, names, encoding and round-trip fixtures remain unresolved under A-D01/D04/D06.

**Disposition:** clarification supplied; **not resolved or approved**. A's tuple remains a proposal. No B/C/D files or public contracts were amended, and repeated molecular identifiers have not been silently disallowed.

## A-SR02 — P2 vignette timing

**Before:** §9 A-T08 owned `vignettes/ingest-profiles.qmd`, ahead of the supplied milestone 9.

**After:** A-T08 instead owns proposed reproduction Markdown at `docs/reproduction/A-ingest-profiles.md` and synthetic CLI fixtures at `inst/examples/synthetic-a-cli/`. The explicit **Milestone 9 handoff** reserves the Quarto vignette for after annotation/report, consuming validated A artifacts. It is neither an A-T08 deliverable nor a dependency.

**Disposition:** requested draft correction applied; independent root verification pending. The actual milestone 9 ticket is still part of the orchestrator's global schedule and no artifact named in that proposed ticket is claimed created.

## A-SR03 — P2 missing-parent derived endpoint semantics

**Before:** lineage/units/ordering existed but did not specify aggregation when some eligible parent abundances were missing.

**After:** §3.2's `blocks[].derive`, §3.4 **Derived observation-completeness contract**, §3.5 `A_DERIVE_UNRESOLVED`, §4.2 ordering, §6.4 **Missing-parent estimand drift**, A-D04 and A-T06 now require a fixed eligible parent universe, an explicit missing-parent/coverage policy, and completeness/observed/imputed/unknown provenance. The documented alternatives are labeled observed partial sums, estimates of the eligible sum after approved imputation, or missing/unestimable endpoints. None is selected as a default. NA-as-zero is not inferred; a finite derived quantity is not automatically an observed measurement. Additive contribution records and weighted numerator/denominator provenance are distinguished. B/D endpoint labels must retain the policy and universe.

The proposed hand oracle contrasts `(10,30)` and `(10,NA)` without treating the missing value as a true zero. Unknown upstream provenance and all-parent-missing cases are included. These are future checks, not executed tests.

**Disposition:** requested gap is now explicit and ticketed; independent root verification pending. Endpoint representation, exact shared fields, eligibility/coverage thresholds and the scientific policy remain unapproved under A-D04. No imputation or aggregation method has been adopted.

## A-SR04 — P2 complete resolver fixture coverage

**Before:** the generator parameters were retained, but A-T02 acceptance explicitly mentioned only paired and continuous metadata.

**After:** §6.1 **Complete resolver fixture coverage** maps every B resolver row: repeated two-level, repeated more-than-two-level, independent two-group, independent more-than-two-group, independent continuous, and time-series/trajectory. It proposes direct use of the four supplied generator modes and explicit versioned fixtures for the other two rows, with an optional generalized schema left to A-D10 approval. Every row has a malformed variant, map/type oracle and distinct truth record. A-T02 and §6.4 require the complete inventory; B owns model/eligibility validation. Unsupported-model fixtures do not imply fitting support.

**Disposition:** requested coverage dependency restored; independent root verification pending. No public enum was widened, no fixture or generator was implemented, and extended schema/API choices remain unresolved under A-D10.

## Verification limits and readiness

Performed document-only checks: read the independent review, inspected affected A sections and the actual B resolver table, applied the scoped text revisions, inspected the changed headings/ticket references, and calculated before/after file hashes. No R session, executable scientific check, test suite, renderer or full spec re-review ran in this response.

A remains provisional and **not implementation-ready**. Historical handover, technology/fleet confirmation, dependency/API/version verification, shared identity and resampling/source-access contracts, scientific decisions, independent re-review and global readiness gates remain outstanding. Root must verify this revision and record the final review disposition.
