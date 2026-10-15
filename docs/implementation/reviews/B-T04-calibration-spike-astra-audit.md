# B-T04 calibration spike — Astra low audit

- Date: 2026-10-15
- Role: fresh-context Astra low, read-only
- Scope: synthetic diagnostic and proposed production-statistic alignment only
- Verdict: **PASS WITH REQUIRED CHANGES**

The audit reproduced ordinary rejection counts 17/23/19/15/18 and moderated
counts 17/15/12/10/21. All moderated rejection upper bounds passed the unchanged
0.08 maximum, and all reported coverage lower bounds passed 0.90.

The auditor confirmed that the held fitter publishes limma `eBayes` p-values and
`df.total`, while the held calibration gates ordinary t-test p-values and
ordinary intervals. The proposed moderated interval calculation uses matching
`sqrt(s2.post) * stdev.unscaled` and `df.total`.

Required conditions:

1. Preserve the original ordinary failure and ordinary QR/SE/df evidence.
2. Accept an explicit change record before changing AC3's operational gate.
3. Qualify only `robust=FALSE`; any other robustness setting needs its own
   prespecified calibration.
4. Treat the pooled interval-level binomial coverage bound as descriptive because
   empirical-Bayes priors couple feature intervals within a study. Use a stated
   independent-study-level coverage bound for the operational coverage gate.
5. Do not extend qualification to other covariance or missingness regimes.

B-T04 remains blocked. This audit does not accept a packet, authorize code, or
establish real-data scientific validity.

## Focused re-review

The one permitted focused re-review returned **PASS**. It confirmed that the
revised proposal preserves the 23/400 ordinary failure, restricts qualification
to `robust=FALSE`, requires matching moderated p/SE/df/contrast provenance,
labels pooled interval-level beta bounds descriptive, gates coverage across the
400 independent studies, and excludes additional covariance/missingness scope.
The rerun's minimum study-level lower coverage bound was 0.9461728.

## Subsequent disposition

Omar explicitly accepted M4-002 on 2026-10-15. The independent audit remains a
diagnostic verdict, not advance acceptance of the implementation candidate.
