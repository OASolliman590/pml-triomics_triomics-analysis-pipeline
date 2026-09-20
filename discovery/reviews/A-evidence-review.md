# Independent bounded evidence review: A preprocessing

Reviewed 2026-09-05 by the D evidence researcher, independently of A's author. Target: `discovery/research/A-preprocessing.md`, SHA-256 `5A168AB010BD6899C30D811844F034C0AF88BB72F001CC827FEF0EFAFCD7E18C`. No edits to that file, production files or project contracts were made. Scope is five primary-source groups: Burla; Wei; imputeLCMD documentation; lipidr source/documentation; compositions CLR documentation. This is not a complete specification, statistical-method or implementation review.

**Disposition:** No hard factual error established in the checked principal conclusions. Four qualifications should be incorporated before these notes become normative specification text. The material challenges to unconditional class-total normalization and assay-name-based missingness assignments are supported. The lipidr source/documentation discrepancy is independently confirmed, but remains unpinned and unexecuted.

## Findings

### A-REV01 — Preserve the distinction between concentration normalization and compositional analysis

Classification: **qualification**, not a hard error. Target: A-Q04 and headline item 2.

[Burla et al.](https://pmc.ncbi.nlm.nih.gov/articles/PMC6168311/), DOI 10.1194/jlr.S087163, “Quantification”, opening paragraphs and Step iii, supports the author's bounded statement. It discourages class-sum normalization for plasma/serum concentration reporting. It also calls molar percentages legitimate and informative, while preferring concentration reporting where possible. The Introduction explicitly restricts the paper to human blood plasma/serum measured by MS.

Suggested addition: “This guidance does not prohibit a separately identified compositional endpoint or all methods described as class-aware normalization. Class-specific standards, sample-volume correction, class-total closure and variance scaling are different operations.” The current note already largely preserves this distinction; carry it into the final decision record, especially when responding to the user's mandatory within-class requirement.

Status: supported with this scope. No new assay default follows from the citation.

### A-REV02 — Wei supports a mechanism-dependent alternative, not a universal RF/QRILC winner

Classification: **qualification**, not a hard error. Target: A-Q02 and A-Q09.
+[Wei et al.](https://www.nature.com/articles/s41598-017-19120-0), DOI 10.1038/s41598-017-19120-0, 2018; inspected “Missing types in metabolomics data”, Discussion, “Missing value generation” and “Missing value imputation methods”. The reported GC/MS and targeted LC/MS observations match A. The random deletion benchmark labeled MCAR/MAR does not implement all conditional MAR mechanisms. QRILC was applied after log transformation; random low-tail censoring supplied the MNAR benchmark. MinProb was not one of the eight compared methods.

Suggested addition: distinguish the observed study patterns from a diagnosis of the first consumer's missingness, and explicitly state that Wei does not compare MinProb against QRILC. The modified 80% rule is an example/default of that study's workflow, not a validation of either a universal 80% or triomics' 70%.

Status: central challenge supported; no default winner accepted. The A note already states most of these limitations.

### A-REV03 — Make the lipidr mutation conditions exact

Classification: **qualification with implementation consequences**. Target: headline item 4 and A-Q07.

Independent static inspection of [upstream normalize.R](https://raw.githubusercontent.com/ahmohamed/lipidr/master/R/normalize.R), functions `.prenormalize_check`, `.log_data`, `normalize_istd`, confirms the cited code. [normalize_istd documentation](https://www.lipidr.org/reference/normalize_istd.html), Description, instead promises all-standard fallback.

Suggested precise wording: “In inspected unpinned master, pre-normalization assigns `min(assay_, na.rm=TRUE)` to nonfinite entries. This is the entire assay's minimum, not a per-class or per-feature minimum, and is not explicitly restricted to finite values. Subunit clamping occurs only when `log=TRUE` and the assay is not flagged as already logged. A missing same-class standard gives divisor 1; absence of all standards raises an error.”

Consequently do not describe the normalization as necessarily producing a finite completed matrix, or as always clamping subunit values. Negative infinity/all-missing inputs need adversarial probes. Existing source/release caveats must remain. The discrepancy is a confirmed static concern, not a demonstrated failure of installed release 2.26.0.

Status: pending release pin and execution. No production workaround selected.

### A-REV04 — State CLR's log base explicitly at the executable boundary

Classification: **qualification**, not a hard error. Target: A-Q06.

The [compositions full reference](https://cran.r-project.org/web/packages/compositions/refman/compositions.html#clr), “Centered log ratio transform”, Details, confirms centering log coordinates, zero sum and singular covariance. The [individual help page](https://search.r-project.org/CRAN/refmans/compositions/html/clr.html) inspected here reports 2.0-8, while the target cites the distinct full reference as 2.0-9; these are not interchangeable version evidence. The displayed formula uses natural logarithms.

Suggested addition: “The base-2 expression in A-Q06 is an explicitly chosen rescaling; it must not be assumed to be the output of `compositions::clr`.” A common positive unit conversion cancels from a sample's CLR; feature-specific calibration factors can shift coordinates, and uncalibrated intensity proportions are not automatically molar proportions. These last consequences are algebraic interpretation, not assay validation. The reference parts must stay fixed for a contrast to have the stated interpretation.

Status: formula and estimand warning supported; version/base declaration still needed in a concrete adapter.

## Checks without an actionable error

**MinProb versus QRILC return shapes:** [imputeLCMD 2.1 MinProb help](https://search.r-project.org/CRAN/refmans/imputeLCMD/html/impute.MinProb.html), Usage/Arguments/Value, supports A's signature and completed-data return; `q` controls a sample-level low quantile and `tune.sigma` scales distribution SD. [QRILC help](https://search.r-project.org/CRAN/refmans/imputeLCMD/html/impute.QRILC.html), Usage/Value, supports the list return containing completed abundances and fitted distribution parameters. They are distinct procedures; MinProb's Gaussian draws are not interchangeable with QRILC's quantile-regression procedure. A does not wrongly claim identical return objects. Matrix orientation and numerical edge cases remain unverified.

**Class-total arithmetic:** Independently checked symbolically: multiplying every positive member by a common factor leaves `x_i/sum(x)` unchanged. A-Q05's `(10,30)` to `(20,60)` example is correct. It proves loss of that total-change signal after closure, not that compositional analysis is intrinsically invalid.

**Imputation bias expression:** The A-Q03 expression is an exact bookkeeping identity for completed-versus-full arithmetic group means on a common scale with fixed missing fractions. It is not a general formula for moderated model-coefficient bias after covariate adjustment, nonlinear normalization or stochastic fitting. Retain its “inference from algebra” label. The cited Chion article was outside this bounded primary-source recheck.

## Verification limits and next action

The named web articles, manuals and unpinned source were re-opened independently. Wei's PMC page returned a browser check; its publisher page provided the inspected text. No R test, package install, source checksum, benchmark, measurement-data validation or licence audit was performed. Target file hash was calculated locally before this review was written.

Incorporate A-REV01–04 as qualifications when reconciling evidence, then pin actual package source and perform the already queued synthetic adversarial checks. Keep the current evidence-only disposition. This review does not approve a reversal of user decisions, a new dependency, an A specification, or any production milestone.
