# B-T04 acceptance review

- Date: 2026-10-15
- Packet: B-T04 — qualified multivisit and trajectory fixed-subject inference
- Change record: `M4-002-b-t04-production-statistic-calibration.md`
- Initial candidate: `bb0181bd07acd3d18bf3a91abb1c1e943879a2bd`
- Targeted repair 1: `9e5f08ea3e962119490381bb842523cd373ad2bd`
- Integration commits: `753ae47`, `af69a9e`
- Reviewer: fresh Sol 6.1 high; same reviewer for the one focused re-review
- Final verdict: **ACCEPT**

## Scientific and software disposition

Calibration and publication now use the same declared route: full fixed-subject
`limma::lmFit()`, the named contrast, and
`limma::eBayes(trend=FALSE, robust=FALSE)`. Moderated p-values, posterior
standard errors and `df.total` share that fit. Coverage is gated by the stated
one-sided lower bound over 400 independent study-level coverage proportions;
the pooled feature-interval beta bound is descriptive.

The ordinary QR evidence remains separate and visible. In particular, the
one-cell-removed pattern still fails the ordinary diagnostic at 23/400 with
upper bound 0.0804818997406527. It was not relabelled as passing. On the same
unchanged seeds, generator and thresholds, the corresponding production-aligned
moderated result is 15/400 with upper bound 0.0571572504439887 and study-level
coverage lower bound 0.949097781965663. All five declared synthetic cells pass
the moderated gate. `robust=TRUE`, additional covariance/missingness regimes,
random slopes and carryover remain unqualified.

The initial review found no Critical or Important issue. Integrated package
verification then exposed two software-only defects: a source-layout-only CLI
test and a missing installed-package stage hook. Targeted repair 1 made the test
resolve packaged resources, installed an idempotent namespace hook that
delegates non-B-T04 requests unchanged, and removed an undefined-global check
NOTE. The focused re-review accepted the exact two-file repair and confirmed
that model/calibration source was unchanged.

## Fresh integrated evidence

- B-T04: 12 cases, 302 expectations; 0 failures, warnings or skips.
- Synthetic CLI: exit 0; B-T04-AC1 through AC4 pass.
- Full suite: 2,706 expectations; 0 failures, warnings or skips.
- Package build/check: exit 0, `Status: OK`.
- Build log SHA-256:
  `9923a71b7a38fddfe501ed620738b8392647702de6ddde9e337e8bccf645a8a0`.
- Check log SHA-256:
  `6baea0de66a2c4012af2a5097797d0aab734a2d29adc9a2df1b9baa944dbf63f`.
- Deterministic calibration JSON SHA-256:
  `7dec4d66cd9fa3053de399e2e737c6e7b2ae4e6a90620794efb6bfbd6133729a`.
- Acceptance record SHA-256:
  `1bb75f6ecc414417185238be0e515ee1f56e4d0e5c64d97fb5569b5d04f35591`.

This accepts synthetic software and calibration evidence for the five declared
B-T04 cells only. It does not validate Omar's studies, establish biological
validity or release G4. B-T05 and B-T06 remain blocked; B-T07 therefore remains
dependency-blocked.
