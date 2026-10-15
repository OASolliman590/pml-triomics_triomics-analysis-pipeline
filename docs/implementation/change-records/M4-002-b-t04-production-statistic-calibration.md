# M4-002 — align B-T04 calibration with its published moderated statistic

- Date: 2026-10-15
- Trigger: B-T04's frozen ordinary null gate failed at 23/400 for the
  one-cell-removed pattern, while the held fitter would publish limma-moderated
  p-values.
- Status: **accepted by Omar on 2026-10-15**
- Evidence:
  `docs/implementation/investigations/B-T04-calibration-spike.md`
- Independent audit:
  `docs/implementation/reviews/B-T04-calibration-spike-astra-audit.md`

## Proposed decision

For B-T04 AC3, qualify the inferential statistic the executable route actually
publishes: fixed-subject `limma::lmFit()`, the declared contrast, and
`limma::eBayes(trend=FALSE, robust=FALSE)`. Preserve the ordinary QR fit,
per-feature rank, residual df, effect and SE as required reference evidence, but
do not use ordinary p-values to qualify a moderated p/q publication route.

The original ordinary outcome remains part of the record: 23/400 discoveries,
one-sided 95% upper bound 0.0804819, and therefore failure against 0.08. It is
not relabelled as a pass.

Coverage for the moderated route uses the matching posterior SE and limma
`df.total`. The interval-level beta lower bound is retained as a descriptive
compatibility statistic. Because features within one study share estimated
empirical-Bayes priors, the operational coverage gate uses a stated one-sided
lower bound over the 400 independent study-level coverage proportions.

## Frozen controls that do not change

- seeds 1001:1400;
- 400 studies and 100 features per supported design cell;
- Gaussian generator and all five declared layouts;
- q=0.05, BH family and any-rejection study endpoint;
- one-sided 95% rejection upper bound and maximum 0.08;
- coverage minimum 0.90;
- full subject fixed-effect design, actual per-feature rank and ordinary
  `n_observed - rank(X)` residual df;
- supported method/pattern scope and B-T04 packet identity;
- prohibition on `duplicateCorrelation`, random slopes, covariance substitution,
  reseeding, threshold relaxation and real-study tuning.

## Contract effect

This is an operational alignment with the existing v2 B contract, which already
requires limma empirical-Bayes inference and separate ordinary/moderated
evidence. It does not add a new model or broaden supported repeated-measures
scope. An explicit record is nevertheless required because it changes which
statistic AC3 uses as its qualification gate after the original attempt stopped.

`robust=TRUE` is unavailable in B-T04 v2 unless a later accepted record supplies
its own prespecified calibration. The five existing synthetic cells do not
qualify additional missingness, covariance, carryover or random-slope regimes.

## Authorized work only if Omar accepts

Acceptance authorizes one bounded, test-first B-T04 repair in a fresh isolated
Luna max worker using the existing packet allowlist. The repair must:

1. fail first on an assertion that calibration and publication share method,
   contrast, p-value, SE and df provenance;
2. calibrate `lmFit` + declared contrast +
   `eBayes(trend=FALSE, robust=FALSE)` on the unchanged protocol;
3. retain and emit the original ordinary reference/failure evidence separately;
4. use matching moderated SE and `df.total` for coverage, with the
   independent-study coverage bound stated above;
5. refuse `robust=TRUE` as unqualified;
6. pass all five unchanged cells, B-T04 behavioral tests and the synthetic CLI;
7. make no writes outside the B-T04 packet allowlist.

A fresh Sol 6.1 high reviewer must inspect the candidate and rerun the packet
tests. Sol may integrate only after that review accepts it and fresh package
verification passes. B-T04 would then be eligible for M4 reconciliation, but G4
would remain blocked until B-T05 and B-T06 also have accepted dispositions.

This record does not fit or validate any of Omar's studies and does not establish
biological validity.

## User decision

Omar explicitly accepted this record on 2026-10-15. That acceptance authorizes
only the bounded B-T04 repair and review sequence above; it does not accept the
result in advance or alter any frozen calibration control.
