# B-T04 calibration diagnostic spike

Date: 2026-10-15. Status: recommendation only; no implementation or acceptance.

## Question

Why does the `balanced_visits_one_cell_removed` null cell fail at 23/400
studies when the fixed-subject ordinary fit, coverage and four other declared
cells pass? The frozen seeds 1001:1400, 100-feature generator, q=0.05 rule,
one-sided 95% upper bound and 0.08 maximum remain unchanged
[triomics validation contract (2026/09), Numerical tolerances and calibration].

## Root cause

The held B-T04 calibration computes ordinary per-feature t-test p-values and
applies BH to those p-values. The held executable fitter instead publishes
`limma::eBayes()` moderated p-values after `lmFit()` and `contrasts.fit()`.
Therefore, the gate is testing a different inferential statistic from the one
the packet would publish [B-T04 held source (2026/10), `triomics_b04_null_cell`
and `triomics_b04_fit`].

This difference conflicts with the existing v2 contract rather than proposing a
new statistical method. The contract requires limma with `eBayes(trend=FALSE)`
for eligible continuous responses, preservation of ordinary and moderated
quantities, and explicit ordinary/moderated null evidence
[triomics B contract (2026/09), Sections 3 and 7]. Limma's current guide likewise
describes fixed block effects followed by `lmFit()` and `eBayes()` for moderated
paired inference, and recommends the moderated statistics over reconstructed
ordinary statistics [limma User's Guide (2026/10), Sections 8.4 and 13.2].

## Controlled probe

The accompanying synthetic-only script reconstructs the five frozen layouts.
For every route it uses the same seeds, random-normal generator, subject
centering, feature count, contrast, q threshold, BH family, coverage threshold
and binomial bound. The only comparison is ordinary p-values versus the
production-aligned moderated p-values [B-T04 spike (2026/10), script and output].

Command:

```text
Rscript --vanilla docs/implementation/investigations/B-T04-calibration-spike.R
```

| Frozen pattern | Ordinary discoveries | Ordinary upper | Moderated discoveries | Moderated upper | Moderated coverage lower by study |
|---|---:|---:|---:|---:|---:|
| balanced complete visits | 17/400 | 0.06307 | 17/400 | 0.06307 | 0.94927 |
| balanced visits, one cell removed | 23/400 | 0.08048 | 15/400 | 0.05716 | 0.94910 |
| balanced linear complete | 19/400 | 0.06892 | 12/400 | 0.04815 | 0.94617 |
| irregular linear complete | 15/400 | 0.05716 | 10/400 | 0.04204 | 0.94885 |
| natural spline complete | 18/400 | 0.06600 | 21/400 | 0.07472 | 0.94752 |

All five production-aligned moderated cells satisfy the unchanged 0.08 rejection
bound and 0.90 coverage bound. The previously failing cell becomes 15/400 with
upper bound 0.05716. The coverage column uses a one-sided t lower bound over 400
independent study-level coverage proportions. The interval-level beta lower bound
is also emitted for continuity, but is descriptive because empirical-Bayes
moderation couples the 100 feature intervals within each study. R 4.6.1 and
limma 3.68.5 were used
[B-T04 spike (2026/10), script and output].

The original 23/400 is still a genuine failure of the frozen gate. It is not,
however, strong evidence that the ordinary t-test is anticonservative: its raw
rate is 0.0575, per-feature ordinary p<0.05 frequency is 0.0481, and ordinary
coverage is 0.9519. Under an exact 0.05 study-level null rejection probability,
the chance of observing at least 23 rejections in 400 studies is 0.275. The gate
passes only at 22 or fewer, so even a perfectly 0.05-calibrated procedure passes
this finite Monte Carlo gate with probability about 0.725
[B-T04 spike (2026/10), gate diagnostics]. This explains the narrow miss but
does not authorize changing the threshold, seeds or generator.

## Scientific interpretation

Subject fixed effects remain appropriate for the supported within-subject
contrast: they remove subject-level baselines while keeping the declared visit
or time coefficient. Limma's official guide presents the same fixed-block design
for moderated paired inference [limma User's Guide (2026/10), Section 8.4].
Empirical-Bayes moderation stabilizes feature-wise residual variances by sharing
information across features, which is the inferential route already specified
and implemented in the held fitter [Smyth (2004), Methods; limma manual
(2026/10), `eBayes`].

The authorized study workbooks were not fitted and contributed no values,
identifiers, thresholds or design constants to this result. Real-study onboarding
must remain a later, separate evidence level [triomics validation contract
(2026/09), Three different evidential levels].

## Recommendation

Create an explicit change record authorizing one targeted B-T04 repair. The
repair should make AC3 qualify the actual published moderated route while
retaining the ordinary QR/reference identity and ordinary df/SE evidence.

The repair must:

- keep seeds 1001:1400, 400 studies, 100 features, the generator, q=0.05,
  one-sided 95% bound, maximum 0.08 and coverage minimum 0.90 unchanged;
- keep the full subject fixed-effect design and per-feature ordinary
  `n_observed - rank(X)` outputs;
- run `lmFit()`, the declared contrast and `eBayes(trend=FALSE, robust=<declared>)`
  in calibration exactly as in the executable fitter;
- qualify `robust=FALSE` only; any future `robust=TRUE` route requires its own
  prespecified calibration and change record;
- gate on the moderated p/q and matching moderated interval while recording
  ordinary reference values separately;
- treat the interval-level beta lower bound as descriptive and gate coverage on
  a stated one-sided lower bound over the 400 independent study-level coverage
  proportions;
- add a direct assertion that the calibration statistic and published statistic
  have the same method, contrast, p-value and df provenance;
- add no `duplicateCorrelation`, random slope, covariance substitution,
  reseeding, threshold relaxation or real-study tuning.

If independent review does not accept this alignment, B-T04 remains blocked.
Even if accepted, this spike is not packet acceptance: a Luna repair, fresh Sol
6.1 high review, integrated tests, package check and the normal G4 Astra review
remain required.

## Sources

[B-T04 held source, 2026/10] Preserved local packet candidate. "B-T04 repeated-model calibration and fitter." `hold/b-t04/design_repeated.R` in the isolated worktree; SHA-256 recorded with the review receipt.

[B-T04 spike, 2026/10] Sol. "Production-aligned B-T04 null calibration probe." `docs/implementation/investigations/B-T04-calibration-spike.R`.

[triomics B contract, 2026/09] triomics. "B — design resolution, inference and clinical associations." `discovery/spec-v2/B_STATISTICS_CLINICAL.md`.

[triomics validation contract, 2026/09] triomics. "Validation and acceptance protocol." `discovery/spec-v2/VALIDATION.md`.

[limma User's Guide, 2026/10] Smyth et al. "limma: Linear Models for Microarray and RNA-Seq Data User's Guide." Bioconductor. https://bioconductor.org/packages/release/bioc/vignettes/limma/inst/doc/usersguide.pdf

[limma manual, 2026/10] Smyth et al. "Package limma reference manual." Bioconductor. https://bioconductor.org/packages/release/bioc/manuals/limma/man/limma.pdf

[Smyth, 2004] Gordon K. Smyth. "Linear Models and Empirical Bayes Methods for Assessing Differential Expression in Microarray Experiments." Statistical Applications in Genetics and Molecular Biology. https://pubmed.ncbi.nlm.nih.gov/16646809/
