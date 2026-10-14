# Scientific validity of the triomics v2 imputation boundary

Date: 2026-10-14

Scope: Whether triomics v2 may scientifically retain `none` and a training-derived feature-median baseline while keeping kNN and random-forest imputation unavailable pending separate qualification.

Audit mode: parallel independent literature, GitHub implementation, and validation/leakage checks.

Capabilities used: primary-literature search, public GitHub inspection, and three fresh-context research agents. No private study data were uploaded or inspected.

## Contents

1. [Verdict](#verdict)
2. [What routine metabolomics does](#what-routine-metabolomics-does)
3. [Evidence from scientific studies](#evidence-from-scientific-studies)
4. [Evidence from maintained software](#evidence-from-maintained-software)
5. [Required scientific boundaries for triomics](#required-scientific-boundaries-for-triomics)
6. [Limitations](#limitations)
7. [Sources](#sources)

## Verdict

**Option 1 is scientifically defensible and conservative.** triomics v2 may keep kNN and random-forest imputation unavailable until their implementations and intended study contexts are separately qualified. It should not describe those methods as scientifically invalid: both are used routinely and can perform well under suitable missingness mechanisms.

The scientifically accurate v2 contract is:

- `none` remains available where the downstream analysis legitimately tolerates incomplete observations.
- Training-derived feature median remains an explicitly labelled baseline or sensitivity route, not a reconstruction of true biological concentrations.
- kNN and random forest remain optional but unavailable routes pending qualified, leakage-safe fit/apply behavior and study-representative validation.
- Reports preserve missingness summaries, feature filtering, effective sample counts, and the distinction between measured and imputed values.

## What routine metabolomics does

Routine metabolomics commonly filters features with excessive missingness and then imputes some remaining values. There is no single universally correct imputer. Missing values can reflect true absence, concentrations below detection, ion suppression, peak-picking or identification failure, batch/run-day effects, or approximately random technical loss. A method that is suitable for one mechanism can distort another.

QComics emphasizes separating missing measurements from true absence because merging the two can remove biological information ([González-Domínguez et al., 2024](https://pubmed.ncbi.nlm.nih.gov/38179935/)). The mQACC recommendations require explicit reporting of how missing values were handled rather than mandating one universal method ([Kirwan et al., 2022](https://pmc.ncbi.nlm.nih.gov/articles/PMC9420093/)).

```text
separately handling missing values and truly absent data
```

Source: [QComics abstract](https://pubmed.ncbi.nlm.nih.gov/38179935/).

## Evidence from scientific studies

### Assertion: no-imputation can be a valid primary route

Di Guida et al. compared 280 preprocessing combinations across four untargeted UHPLC-MS datasets. For univariate analysis they recommended normalization without missing-value imputation because imputation can alter within-class distributions. They found RF/KNN useful for particular multivariate purposes, which shows that the correct choice depends on the analysis rather than on a universal ranking ([Di Guida et al., 2016](https://doi.org/10.1007/s11306-016-1030-9)).

```text
PQN normalisation with no missing value imputation
```

Source: [open author manuscript](https://pure-oai.bham.ac.uk/ws/portalfiles/portal/27489547/Di_Guida_et_al_non_tergeted_UHPLC_MS_metabolomic_Published_Metabolomics_2016.pdf).

### Assertion: kNN and RF can be valid, but performance depends on missingness

Wei et al. compared eight methods across four metabolomics datasets. RF performed best for simulated MCAR/MAR missingness, whereas QRILC was favored for left-censored MNAR data ([Wei et al., 2018](https://pubmed.ncbi.nlm.nih.gov/29330539/)). Do et al. evaluated 31 approaches and found a KNN variant robust in their specific untargeted-MS setting, while also finding substantial run-day/detection-limit missingness ([Do et al., 2018](https://pubmed.ncbi.nlm.nih.gov/30830398/)). These results support keeping kNN/RF as future options, not enabling them without qualification.

```text
RF performed the best for MCAR/MAR
```

Source: [Wei et al. abstract](https://pubmed.ncbi.nlm.nih.gov/29330539/).

### Assertion: an advanced imputer can generate misleading biology

Krutkin et al. evaluated targeted and untargeted real-data settings. kNN and RF worked better at low MCAR rates but degraded for MNAR or greater missingness and could introduce apparent abundance differences not present in measured values ([Krutkin et al., 2025](https://pubmed.ncbi.nlm.nih.gov/40007142/)). Their public analyses are available on [GitHub](https://github.com/ddkrutkin/metabolomics_imputations).

```text
imputation should be used with extreme caution
```

Source: [Krutkin et al., conclusion](https://doi.org/10.1021/jasms.4c00434).

## Evidence from maintained software

### Assertion: established packages expose a menu, not one mandatory method

The RforMassSpectrometry core utility `MsCoreUtils` lists kNN, RF, left-censor methods, constants, mixed strategies, and `none` as separate valid choices. Its implementation intentionally returns the data unchanged for `none` ([source file](https://github.com/RforMassSpectrometry/MsCoreUtils/blob/main/R/imputation.R#L137-L139)).

```text
No imputation is performed and the missing values are left untouched.
```

Source: [`MsCoreUtils` imputation implementation](https://github.com/RforMassSpectrometry/MsCoreUtils/blob/main/R/imputation.R).

MetaboAnalyst presents KNN as an alternative to feature removal or minimum/mean/median replacement ([official vignette](https://new.metaboanalyst.ca/MetaboAnalyst/resources/vignettes/Introductions.html)). MetaboShiny exposes RF, KNN, mean, median, removal, and leaving values alone ([README](https://github.com/joannawolthuis/Metaboshiny#filtering-and-normalization)). Bioconductor's `pmp` and `statTarget` likewise expose multiple selectable methods rather than treating RF or KNN as compulsory ([pmp vignette](https://bioconductor.org/packages/release/bioc/vignettes/pmp/inst/doc/pmp_vignette_peak_matrix_processing_for_metabolomics_datasets.html#missing-value-imputation), [statTarget vignette](https://bioconductor.org/packages/release/bioc/vignettes/statTarget/inst/doc/statTarget.html)).

### Assertion: the current routine APIs do not by themselves prove safe inductive use

`MsCoreUtils` calls matrix-oriented kNN and RF implementations. A routine that accepts and completes one matrix does not automatically establish the frozen training-state/apply-to-new-samples behavior required for predictive validation. In contrast, tidymodels' recipe explicitly learns reference data during training and applies that frozen state to other datasets ([`recipes` source](https://github.com/tidymodels/recipes/blob/main/R/impute_knn.R#L19-L21)).

```text
The step uses the training set to impute any other data sets.
```

Source: [tidymodels `step_impute_knn`](https://github.com/tidymodels/recipes/blob/main/R/impute_knn.R).

Official scikit-learn guidance also identifies imputation as preprocessing that must be learned only from training data to avoid optimistic validation results ([common pitfalls](https://scikit-learn.org/stable/common_pitfalls.html#data-leakage)). Therefore, a code fingerprint or caller-created artifact can demonstrate code identity, but cannot demonstrate independent scientific approval, missingness suitability, or leakage-safe qualification.

## Required scientific boundaries for triomics

Option 1 remains valid only with these boundaries:

1. **Do not equate unavailable with invalid.** State that kNN/RF are withheld pending qualification.
2. **Keep missingness visible.** Record feature- and sample-level missingness, filtering decisions, effective analysis counts, and observed/imputed status.
3. **Constrain `none`.** A downstream method that requires a complete matrix must fail clearly rather than silently discard or invent values.
4. **Constrain median.** Estimate each feature's median from training data only, within every resampling fold; apply it unchanged to validation/test samples. Label it baseline/sensitivity analysis.
5. **Do not claim bias disappears.** Complete-case or available-case analysis can still be biased under informative missingness.
6. **Require real future qualification.** To enable kNN/RF later, use study-representative masking patterns, appropriate MCAR/MAR/MNAR scenarios, train-only fitting, held-out application, provenance/lineage, and comparison of downstream scientific conclusions—not imputation error alone.

Recommended wording:

> triomics v2 supports no imputation for compatible analyses and fold-local, training-derived feature-median imputation as an explicitly labelled baseline or sensitivity route. kNN and random-forest imputation are unavailable pending independently verified method and study-context qualification. No route is universally optimal.

## Limitations

- This review evaluates the pipeline contract, not the best imputer for Omar's three future datasets. That decision requires local inspection of assay platform, QC samples, detection limits, batch structure, missingness patterns, and the downstream estimand.
- Public GitHub code demonstrates implemented behavior and prevailing software conventions; it does not prove scientific validity for a particular dataset.
- No private clinical identifiers, assay matrices, or study outputs were uploaded or analyzed.
- The literature contains conflicting winners because datasets, masking mechanisms, and evaluation targets differ. That conflict supports qualification rather than automatic activation.

## Sources

- [Di Guida et al. (2016), *Metabolomics*](https://doi.org/10.1007/s11306-016-1030-9)
- [Wei et al. (2018), *Scientific Reports*](https://pubmed.ncbi.nlm.nih.gov/29330539/)
- [Do et al. (2018), *Metabolomics*](https://pubmed.ncbi.nlm.nih.gov/30830398/)
- [González-Domínguez et al. (2024), QComics](https://pubmed.ncbi.nlm.nih.gov/38179935/)
- [Kirwan et al. (2022), mQACC recommendations](https://pmc.ncbi.nlm.nih.gov/articles/PMC9420093/)
- [Krutkin et al. (2025), *JASMS*](https://pubmed.ncbi.nlm.nih.gov/40007142/)
- [RforMassSpectrometry/MsCoreUtils](https://github.com/RforMassSpectrometry/MsCoreUtils/blob/main/R/imputation.R)
- [MetaboAnalyst official vignette](https://new.metaboanalyst.ca/MetaboAnalyst/resources/vignettes/Introductions.html)
- [MetaboShiny README](https://github.com/joannawolthuis/Metaboshiny#filtering-and-normalization)
- [Bioconductor pmp vignette](https://bioconductor.org/packages/release/bioc/vignettes/pmp/inst/doc/pmp_vignette_peak_matrix_processing_for_metabolomics_datasets.html#missing-value-imputation)
- [Bioconductor statTarget vignette](https://bioconductor.org/packages/release/bioc/vignettes/statTarget/inst/doc/statTarget.html)
- [tidymodels recipes kNN imputation source](https://github.com/tidymodels/recipes/blob/main/R/impute_knn.R)
- [scikit-learn data-leakage guidance](https://scikit-learn.org/stable/common_pitfalls.html#data-leakage)
