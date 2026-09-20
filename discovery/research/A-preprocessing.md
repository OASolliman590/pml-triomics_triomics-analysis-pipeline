# A: provisional preprocessing evidence

Research date: 2026-09-05. Scope: missingness assumptions, imputation bias, lipid normalization/composition, derived lipid features and lipidr support. This is an evidence note for orchestrator review, **not an approved specification or an implementation**. No production code, dependencies, study data, credentials, contracts or architecture were changed. No R/package behavior checks were run. Mathematical examples below are hand-derived illustrations, not measurement-data validation.

## Material requirements to challenge before implementation

1. Assay identity does not establish a missingness mechanism. The blanket proteomics-MNAR/metabolomics-MAR assignments need an approved qualification.
2. Mandatory within-class lipid scaling is ambiguous and, if it means dividing by sample class totals, can erase precisely the class abundance changes the pipeline must test. Plasma/serum guidance specifically discourages that normalization for concentration reporting.
3. Mol%-within-class is a compositional representation; it does not remove the compositional problem. CLR changes the species estimand to a ratio against a specified reference set.
4. The inspected lipidr normalization source contains implicit replacement of missing/nonfinite values and subunit values, plus a documentation/source discrepancy for missing class standards. Wrapping normalization without checking these behaviors could bypass the requested explicit imputation stage.

Decision authority remains with the orchestrator and maintainer. All proposed contract or settled-policy changes below require approval/ADR; none is silently adopted here.

## Evidence ledger

### A-Q01: Can proteomics missingness be assumed predominantly left-censored?

- **Proposed answer / classification:** Verified fact, bounded to the cited label-free studies: multiple mechanisms coexist, and performance depends on their proportions. The prompt's universal prescription is unsupported; the actual first-consumer mechanism is an **unknown**.
- **Evidence / inspected artifact:** Lazar et al. (2016), *Accounting for the Multiple Natures of Missing Values in Label-Free Quantitative Proteomics Data Sets to Compare Imputation Strategies*, DOI [10.1021/acs.jproteome.5b00981](https://pubs.acs.org/doi/10.1021/acs.jproteome.5b00981), Abstract and §5 Conclusions, items 2–4. The authors favor assessing the likely mechanism and discuss hybrid strategies; they caution against applying a left-censoring method to other missingness.
- **Contradictions / limits:** This paper does not quantify a universal MNAR fraction for all proteomics platforms. Label-free evidence cannot establish the correct default for DIA, DDA, isobaric data and upstream-imputed exports alike. Its peptide-level recommendation also cannot be enforced after receiving only protein matrices.
- **Spec effect / proposed design choice:** Keep separate profiles, but require acquisition/quantification and upstream-processing provenance, an explicit missingness assumption, method and sensitivity plan. Namespace alone cannot supply measurement provenance. Do not infer a mechanism from observed intensities as certainty.
- **Decision status:** Evidence-backed challenge; maintainer disposition pending.

### A-Q02: Is metabolomics missingness mostly technical MAR?

- **Proposed answer / classification:** Verified fact: the cited study found preprocessing-related MCAR/MAR in GC/MS profiling and left-censored MNAR in targeted LC/MS. The blanket claim does not hold across targeted and untargeted assays.
- **Evidence / inspected artifact:** Wei et al. (2018), *Missing Value Imputation Approach for Mass Spectrometry-based Metabolomics Data*, DOI [10.1038/s41598-017-19120-0](https://pmc.ncbi.nlm.nih.gov/articles/PMC5766532/), Discussion, Fig. 4 strategy; Methods, Missing value generation and Missing value imputation methods. Its recommendations condition RF versus QRILC on mechanism; the analysis used R 3.4.2.
- **Contradictions / limits:** Random deletion was used to represent MCAR/MAR in part of the benchmark; this does not span every conditional MAR process. Results do not establish one best method for all studies. A technical cause can still depend on the unobserved abundance and be MNAR.
- **Spec effect / proposed design choice:** Distinguish targeted/profiling acquisition, detection limits, structural absences, instrument failures and upstream processing where metadata support this. QRILC must remain possible for metabolomics; RF/kNN must not be justified solely by the word metabolomics. Raw-data recovery advice from this paper remains upstream/out of scope.
- **Decision status:** Evidence-backed challenge; study-specific mechanism unknown.

### A-Q03: In which direction does left-censored imputation bias effects?

- **Proposed answer / classification:** **Inference from algebra:** there is no universal direction. On the analysis scale, let missing fractions be `r_A,r_B`, means of the truly missing values `m_A,m_B`, and imputed means `c_A,c_B`. Relative to the complete-data mean difference B−A, the imputation-induced change is `r_B(c_B−m_B) − r_A(c_A−m_A)`. Its sign can be positive, negative or zero.
- **Worked illustration:** Two groups truly have mean 10. If half of B is technically missing and those values are filled by 5, completed B has mean 7.5, creating a −2.5 effect. Swapping which group is missing reverses the sign. This demonstrates a possible failure, not a benchmark of MinProb or QRILC. Under genuine censoring, a correctly specified conditional distribution could improve the estimate; this cannot be assumed.
- **Evidence / inspected artifact:** Chion et al. (2022), *Accounting for multiple imputation-induced variability for differential analysis in mass spectrometry-based label-free quantitative proteomics*, DOI [10.1371/journal.pcbi.1010420](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010420), Introduction (single-imputation limitations and Rubin's rules). It supports the uncertainty concern, **not** a universal direction of bias or automatic suitability of multiple imputation for every pipeline backend.
- **Contradictions / limits:** A lower imputed intensity is not equivalent to downward bias in a group contrast. Treating completed entries as observed also omits imputation uncertainty. Averaging multiple completed matrices is not Rubin pooling of model estimates.
- **Spec effect / proposed design choice:** Preserve observed/imputed masks, missing fractions by contrast, seeds, parameters and sensitivity results. Report when an effect is supported mainly by imputation. Assess effect bias, interval coverage and false discoveries separately from reconstruction error. Multiple-imputation support would require coordinated B/C design work; do not add it as an unreviewed patch to A.
- **Decision status:** Factual concern established; inferential policy pending.

### A-Q04: Does lipidomics require within-class scaling?

- **Proposed answer / classification:** **Verified guidance with restricted scope:** Burla et al. encourage concentrations where possible and discourage plasma/serum normalization by class sums. **Inference:** an unconditional requirement is not defensible without defining the operation and estimand.
- **Evidence / inspected artifact:** Burla et al. (2018), *MS-based lipidomics of human blood plasma: a community-initiated position paper to develop accepted guidelines*, DOI [10.1194/jlr.S087163](https://pmc.ncbi.nlm.nih.gov/articles/PMC6168311/), Quantification, especially step iii; Fig. 1. This primary consensus paper distinguishes concentration units, standards and sample-volume normalization, and warns that percentages depend on analytical coverage.
- **Independent official guidance:** [Lipidomics Standards Initiative, Lipid Species Quantification](https://lipidomicstandards.org/lipid-species-quantification/), Consideration of analytical response; Normalization of data; Type of quantification. Retrieved 2026-09-05; page revision date not provided. It separates standard/sample-amount normalization from relative percentages and identifies structural response dependencies beyond class.
- **Contradictions / limits:** Class differences may include analytical response and biology; this evidence does not support dismissing between-class abundance variation as exclusively chemical. Plasma guidance is not automatically a tissue normalization rule. Normalization by matching standards, division by class totals and variance standardization are different operations.
- **Spec effect / proposed design choice:** Replace ambiguous terminology with explicit operations and units after approval. Preserve class-total information when the estimand is concentration. Class-aware analytical correction can be supported without forcing equal class sums.
- **Decision status:** Material settled requirement challenge; ADR/maintainer approval needed.

### A-Q05: What would class closure erase?

- **Proposed answer / classification:** **Inference, exact arithmetic:** if `p_i = x_i / sum_class(x)`, then every complete class sums to 1. Multiplying every species of a class by two leaves its within-class composition unchanged. Therefore class-abundance changes cannot be tested from those closed values alone.
- **Worked illustration:** Class X has species `(10,30)` before and `(20,60)` after. Its total doubles from 40 to 80; both compositions equal `(0.25,0.75)`. Summing log abundances instead yields the log of a product, not log total concentration. Deriving class sums after subject-centering can likewise create a different quantity than summing linear abundance before transformation.
- **Supporting context / inspected artifact:** Aitchison (1982), *The Statistical Analysis of Compositional Data*, DOI [10.1111/j.2517-6161.1982.tb01195.x](https://rss.onlinelibrary.wiley.com/doi/pdfdirect/10.1111/j.2517-6161.1982.tb01195.x), publisher Summary establishes proportions-of-a-whole setting; detailed arithmetic here is our derivation, not a quote or a claim that the full PDF was inspected.
- **Spec effect / proposed design choice:** Specify the linear quantity and feature universe used for class sums, and derive them before information-destroying closure. Retain provenance between source abundances and transformed analysis matrices. This must be reconciled with the same-decomposed-matrix contract by the orchestrator; it is not permission for silent branch-specific preprocessing.
- **Decision status:** Mathematical conflict identified; orchestration decision pending.

### A-Q06: Are mol%-within-class and CLR interchangeable solutions?

- **Proposed answer / classification:** **Verified definitions:** closure gives shares; CLR gives log ratios. [compositions 2.0-9 manual](https://cran.r-project.org/web/packages/compositions/refman/compositions.html#clr), Centered log ratio transform, Usage/Details (retrieved lines 4040–4073), defines `clr_i(x)=log(x_i)−mean(log(x))`; coordinates sum to zero and covariance is singular. Package version dated 2025-08-20, publication 2025-08-21. This is inspected documentation, not a proposed dependency.
- **Inference:** A group difference in base-2 CLR for species i is its log2 change minus the average log2 change across the reference species. An unchanged absolute species can have a negative CLR change when the rest increase. Filtering changes the reference set, and within-class CLR differs from whole-block CLR. Positive scaling of an entire sample cancels out. Ordinary logarithms require positive values; zero handling affects results.
- **Contradictions / limits:** A within-class mol% matrix is still closed/compositional. Relative MS signal is not automatically mol%; response calibration is necessary to interpret intensity shares as molar shares. Unknown-scale intensity is not necessarily an exported fixed-sum composition. Ordinary per-species testing of shares may describe shares, but it does not estimate absolute concentration changes.
- **Spec effect / proposed design choice:** Require explicit denominator/reference set, quantification units, closure scope, log base, zero policy and estimand labels. Never silently CLR-transform. Preserve original-scale values when available. Validate compatibility with rank-deficient input downstream.
- **Decision status:** Definitions established; allowed representations/defaults pending.

### A-Q07: What does lipidr actually provide for normalization/imputation?

- **Proposed answer / classification:** **Verified documentation, behavior pending:** [`normalize_istd(data, measure="Area", exclude="blank", log=TRUE)`](https://www.lipidr.org/reference/normalize_istd.html) normalizes against standards from the matching class; its description says a missing matching standard falls back to all standards. [`normalize_pqn`](https://www.lipidr.org/reference/normalize_pqn.html) uses sample dilution factors, with matching signature. [`impute_na`](https://www.lipidr.org/reference/impute_na.html) advertises wrappers for knn, svd, mle, QRILC, minDet, minProb and zero through imputeLCMD. A class-aware imputation guarantee is not documented there.
- **Inspected source artifact:** [upstream `R/normalize.R`, master](https://raw.githubusercontent.com/ahmohamed/lipidr/master/R/normalize.R), accessed by browser 2026-09-05, unpinned. `.prenormalize_check` replaces nonfinite entries with `min(...,na.rm=TRUE)`. `.log_data` replaces values below 1 by 1 before log2. `normalize_istd` uses `f <- 1` for an empty same-class standards list, contradicting the documented fallback. It normalizes standards by their across-sample means. This is static source inspection, **not observed execution** and not yet established for release 2.26.0.
- **Contradictions / limits:** Website/source/release versions differ. Normalization may implicitly impute and clamp values; the wrapper cannot be presumed safe for negative already-logged values, subunit concentrations or explicit imputation provenance. A whole-matrix wrapper is not verified class-aware imputation.
- **Spec effect / proposed design choice:** Pin and inspect installed source, then execute adversarial probes before reuse. Reject unsupported implicit coercions or implement an approved adapter guard. Parameterize standard matching and unavailable-standard behavior; report it.
- **Decision status:** Dependency behavior verification blocker, no package added.

### A-Q08: What can lipidr support for derived lipid features?

- **Proposed answer / classification:** **Verified advertised capability:** [Mohamed et al. 2020](https://pubmed.ncbi.nlm.nih.gov/32168452/), DOI 10.1021/acs.jproteome.0c00082, Abstract, describes lipid-class/chain/unsaturation annotation and LSEA. [`annotate_lipids(molecules,no_match=c("warn","remove","ignore"))`](https://www.lipidr.org/reference/annotate_lipids.html), Description/Examples, returns structural annotations including `total_cl` and `total_cs`; partial sum-composition identifiers are supported. [`gen_lipidsets(molecules,min_size=2)`](https://www.lipidr.org/reference/gen_lipidsets.html), Value/Examples, returns sets, not per-sample derived abundance matrices. [`lsea`](https://www.lipidr.org/reference/lsea.html), Arguments/Value, consumes DE results and passes parameters to fgsea.
- **Contradictions / limits:** [`plot_chain_distribution`](https://www.lipidr.org/reference/plot_chain_distribution.html) aggregates plotted DE measures by chain annotations; it does not establish a tested per-sample mean-chain-length or double-bond-index implementation. Static carbon/double-bond counts are metadata, not varying sample-level measurements. Full support for arbitrary shorthand, ambiguous species and structural modifications requires executable parser fixtures.
- **Spec effect / proposed design choice:** Explicitly define each derived measure. A candidate class-weighted total-chain measure is `sum_i(a_i * total_cl_i)/sum_i(a_i)` and unsaturation measure `sum_i(a_i * total_cs_i)/sum_i(a_i)`, using documented nonnegative abundance weights. These are **proposed definitions**, not universally standardized DBI formulas. Distinguish carbon count per molecule from per fatty-acyl chain. Record inclusion sets, unknown-annotation handling, internal-standard exclusion, duplicate/adduct handling and measured coverage. A ratio with zero denominator must not become infinity silently.
- **Decision status:** Annotation/set capabilities documented; derived definitions and exact parser behavior pending.

### A-Q09: Is a 70% prevalence threshold established?

- **Proposed answer / classification:** **Design choice**, not established fact. The inspected Wei strategy discusses a modified 80% rule; it does not validate this pipeline's proposed 70% across every assay and design.
- **Contradictions / limits:** Global versus within-condition prevalence can retain different species. Group-exclusive species may be scientifically important yet have incomplete contrasts; small classes can make subsequent ratios unstable. Data-dependent denominator changes are a separate compositional issue.
- **Spec effect / proposed design choice:** Preserve configurability and justify 70% as a proposed operating point, with denominator, boundary rounding, minimum observed counts and contrast estimability explicit. Filtering must precede imputation. For predictive validation, all learned preprocessing quantities and feature eligibility must be determined within the training split; reference matrices and availability of held-out outcomes need explicit adapter design.
- **Decision status:** Threshold/scope decision pending; no calibrated universal threshold claimed.

## Version and artifact manifest

| Resource | What was verified on 2026-09-05 | Version/license limits |
|---|---|---|
| lipidr release | [Bioconductor release landing page](https://bioconductor.org/packages/release/bioc/html/lipidr.html), Details lists 2.26.0; Bioconductor 3.23; R installation guidance 4.6; imports include imputeLCMD | MIT + file LICENSE advertised; linked LICENSE fetch failed. Installed version, source tarball, checksum and full license notice are unverified. |
| lipidr website | Function pages above; `gen_lipidsets`/`plot_chain_distribution` header exposes 2.15.1 | Do not label these 2.26.0 behavior documentation. |
| lipidr source | `R/normalize.R` upstream master inspected statically | Commit SHA unresolved; no release-equivalence claim. `R/impute.R` retrieval failed. |
| lipidr release PDF | [Manual search artifact](https://bioconductor.org/packages/release/bioc/manuals/lipidr/man/lipidr.pdf) reports Bioconductor 3.19 / 2024 publication metadata | Browser/search cache is stale relative to release landing page; exclude it as proof of current semantics. |
| imputeLCMD | Official [MinProb](https://search.r-project.org/CRAN/refmans/imputeLCMD/html/impute.MinProb.html) and [QRILC](https://search.r-project.org/CRAN/refmans/imputeLCMD/html/impute.QRILC.html) manuals identify version 2.1 | MinProb signature `impute.MinProb(dataSet.mvs,q=0.01,tune.sigma=1)` returns completed data; QRILC signature `impute.QRILC(dataSet.mvs,tune.sigma=1)` returns a list of matrix and fitted parameters. Defaults are package defaults, not justified triomics defaults. License/source/hash not verified. |
| compositions | CRAN reference 2.0-9, GPL >=2; CLR definition inspected | Reference only. No installation or dependency proposal. |
| LSI quantification guidance | Official web headings recorded above | No revision/release date exposed; not a versioned software resource. |

The [Bioconductor 3.23 release announcement](https://www.bioconductor.org/news/bioc_3_23_release/) states R 4.6 compatibility. Consequently `R >= 4.3` alone does not pin a reproducible mutually compatible dependency set. Choice of a specific R/Bioconductor pair remains with the orchestrator/maintainer.

## Verification record and pending work

Completed: read the two provided prompt attachments and actual research SKILL.md; inspected the named primary articles, official manuals, official guidance and one upstream source file using web tools. Only this evidence file was written. PowerShell attempted public GitHub commit/source retrieval with `Invoke-RestMethod`/`Invoke-WebRequest`; network calls failed with connection refused. That is an access failure, not a package test. No private data or credentials were read for this subtask. No clone, install, commit, push or statistical check is claimed.

Pending executable checks after language/dependencies and contracts are approved:

1. Pin lipidr release tarball/source commit; inspect `R/normalize.R`, `R/impute.R`, annotation parser, DESCRIPTION and LICENSE. Reconcile website/source/release differences and record hashes.
2. With a labeled synthetic `LipidomicsExperiment`, probe NA, NaN, Inf, negative/logged inputs, values below 1, zero/all-missing rows, missing class standards and duplicate names. Compare input/output masks, values and normalization flags; no implicit imputation may escape provenance.
3. Verify imputeLCMD orientation, return shape, stochastic reproducibility and small-matrix/all-missing failure behavior; audit whether lipidr partitions by class. Do not advertise class-awareness until verified.
4. Verify class closure removes a planted uniform class increase while concentration totals retain it; test both representations' estimand labels. Test CLR reference-set changes and zero policy. These demonstrate invariants only.
5. Verify parser outcomes for sum composition, known chains, ethers/plasmalogens, sphingolipids, isotopic standards, ambiguous names and unmatched identifiers; never invent unresolved chain identities.
6. Use an approved measurement-data benchmark separately to assess missingness assumptions, imputation bias/uncertainty and normalization validity. Synthetic known-factor recovery cannot validate an actual assay mechanism.

Readiness: **provisional, not implementation-ready**. Evidence permits drafting alternatives and explicit blocked decisions; it does not authorize reversing the user's settled contracts.
